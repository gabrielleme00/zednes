pub mod instruction;

use core::panic;

use super::super::emulator::bus::Bus;
use instruction::{AddressingMode as Mode, *};

/// Pending interrupt disable state for SEI/CLI instructions
enum InterruptDisablePending {
    /// No pending change
    None,
    /// Clear Interrupt Disable flag after delay
    Clear,
    /// Set Interrupt Disable flag after delay
    Set,
}

#[derive(PartialEq, Debug)]
enum InterruptType {
    Nmi,
    Irq,
    Brk,
}

/// NES CPU (6502 processor)
pub struct Cpu {
    // Registers
    pub a: u8,      // Accumulator
    pub x: u8,      // Index Register X
    pub y: u8,      // Index Register Y
    pub sp: u8,     // Stack Pointer
    pub pc: u16,    // Program Counter
    pub status: u8, // Status Register

    // Cycles counter
    pub cycles: u64,

    int_disable_pending: InterruptDisablePending, // Pending interrupt enable/disable (for SEI/CLI delay)
    nmi_pending: bool,                            // NMI pending flag
    nmi_prev: bool,                               // Previous NMI line state for edge detection
    irq_line: bool,                               // IRQ line state
    int_delay: bool,                              // Delay for SEI/CLI/PLP delay

    // Set when a BRK instruction is executed
    pub halted: bool,
}

// Status flags
const CARRY: u8 = 1 << 0;
const ZERO: u8 = 1 << 1;
const INTERRUPT_DISABLE: u8 = 1 << 2;
const DECIMAL: u8 = 1 << 3;
const BREAK: u8 = 1 << 4;
const UNUSED: u8 = 1 << 5;
const OVERFLOW: u8 = 1 << 6;
const NEGATIVE: u8 = 1 << 7;

const STACK_BASE: u16 = 0x0100;

impl Cpu {
    /// Create a new CPU instance with initial state
    pub fn new() -> Self {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xFD,
            pc: 0,
            status: UNUSED | INTERRUPT_DISABLE,
            cycles: 0,
            int_disable_pending: InterruptDisablePending::None,
            nmi_pending: false,
            nmi_prev: true,  // Start high
            irq_line: false,
            int_delay: false,
            halted: false,
        }
    }

    /// Reset the CPU to initial state
    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.status = UNUSED | INTERRUPT_DISABLE;
        self.cycles = 7;
        self.halted = false;
        // PC will be loaded from reset vector at 0xFFFC-0xFFFD
    }

    /// Set or clear a status flag
    pub fn set_flag(&mut self, flag: u8, value: bool) {
        if value {
            self.status |= flag;
        } else {
            self.status &= !flag;
        }
    }

    /// Get the value of a status flag
    pub fn get_flag(&self, flag: u8) -> bool {
        self.status & flag != 0
    }

    /// Execute one CPU instruction and return number of cycles taken.
    /// Returns 0 if the CPU is halted.
    pub fn step(&mut self, bus: &mut Bus) -> u8 {
        if self.halted {
            return 0;
        }

        // Clear interrupt delay from previous instruction
        let prev_delay = self.int_delay;
        self.int_delay = false;
        
        // Apply pending interrupt disable from previous instruction
        self.apply_pending_interrupt_disable();

        // Check for interrupts (unless delayed from CLI/SEI/PLP)
        if !prev_delay {
            if let Some(int_type) = self.poll_interrupts() {
                let cycles = self.handle_interrupt(bus, int_type);
                self.cycles = self.cycles.wrapping_add(cycles as u64);
                return cycles;
            }
        }

        // Fetch opcode
        // let pc = self.pc;
        let opcode = self.fetch(bus);

        // Decode instruction
        let instruction = get_instruction(opcode);
        // println!(
        //     "{:<45}{}",
        //     disassemble_at(pc, bus).0,
        //     self._disassemble_regs()
        // );

        // Execute instruction
        let cycles = self.execute_instruction(instruction, bus);
        self.cycles = self.cycles.wrapping_add(cycles as u64);

        cycles
    }

    /// Fetch a byte from memory at the current PC and increment PC
    fn fetch(&mut self, bus: &mut Bus) -> u8 {
        let data = bus.cpu_read(self.pc);
        self.pc = self.pc.wrapping_add(1);
        data
    }

    /// Poll interrupts and return the type if any
    fn poll_interrupts(&mut self) -> Option<InterruptType> {
        if self.nmi_pending {
            return Some(InterruptType::Nmi);
        }

        if self.irq_line && !self.get_flag(INTERRUPT_DISABLE) && !self.int_delay {
            return Some(InterruptType::Irq);
        }

        None
    }

    fn handle_interrupt(&mut self, bus: &mut Bus, int_type: InterruptType) -> u8 {
        let (vector, b_flag) = match int_type {
            InterruptType::Nmi => (0xFFFA, false),
            InterruptType::Irq => (0xFFFE, false),
            InterruptType::Brk => (0xFFFE, true),
        };

        // Cycles 1-2: Dummy reads (PC not incremented for NMI/IRQ)
        if int_type == InterruptType::Brk {
            self.pc = self.pc.wrapping_add(1);
        }

        // Cycles 3-4: Push PC
        self.push_stack_u16(bus, self.pc);

        // Cycle 5: Push status (with B flag clear for interrupts)
        let mut status = self.status;
        if b_flag {
            status |= BREAK; // Set B flag for BRK
        } else {
            status &= !BREAK; // Clear B flag for interrupts
        }
        status |= UNUSED; // U flag always set
        self.push_stack(bus, status);

        // Cycles 6-7: Fetch new PC from vector
        let lo = bus.cpu_read(vector) as u16;
        let hi = bus.cpu_read(vector + 1) as u16;
        self.pc = (hi << 8) | lo;

        // Set I flag
        self.set_flag(INTERRUPT_DISABLE, true);

        // Clear NMI pending flag
        if int_type == InterruptType::Nmi {
            self.nmi_pending = false;
        }

        7 // Total cycles
    }

    /// Set NMI line state (trigger on falling edge).
    /// 
    /// When NMI line goes from high to low, set nmi_pending flag to trigger NMI on next step.
    pub fn set_nmi(&mut self, high: bool) {
        if self.nmi_prev && !high {
            self.nmi_pending = true;
        }
        self.nmi_prev = high;
    }

    /// Set IRQ line
    /// 
    /// When IRQ line is set high, it will trigger an IRQ on the next step if interrupts are not disabled.
    // pub fn set_irq(&mut self, high: bool) {
    //     self.irq_line = !high; // Active low
    // }

    /// Apply any pending interrupt disable clear/set
    fn apply_pending_interrupt_disable(&mut self) {
        use InterruptDisablePending::*;

        // Apply pending interrupt disable from previous SEI
        match self.int_disable_pending {
            Clear => self.set_flag(INTERRUPT_DISABLE, false),
            Set => self.set_flag(INTERRUPT_DISABLE, true),
            None => {}
        }
        self.int_disable_pending = None;
    }

    /// Disassemble current CPU registers into a string
    fn _disassemble_regs(&self) -> String {
        // Format: A:XX X:XX Y:XX P:XX SP:XX
        // Example: A:00 X:00 Y:00 P:24 SP:FD
        format!(
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{:3}",
            self.a, self.x, self.y, self.status, self.sp, self.cycles
        )
    }

    /// Execute instruction based on opcode. Returns number of cycles taken.
    fn execute_instruction(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        match inst.mnemonic {
            // Access
            "LDA" => self.op_lda(inst, bus),
            "STA" => self.op_sta(inst, bus),
            "LDX" => self.op_ldx(inst, bus),
            "STX" => self.op_stx(inst, bus),
            "LDY" => self.op_ldy(inst, bus),
            "STY" => self.op_sty(inst, bus),
            // Transfer
            "TAX" => self.op_tax(),
            "TXA" => self.op_txa(),
            "TAY" => self.op_tay(),
            "TYA" => self.op_tya(),
            // Arithmetic
            "ADC" => self.op_adc(inst, bus),
            "SBC" => self.op_sbc(inst, bus),
            "INC" => self.op_inc(inst, bus),
            "DEC" => self.op_dec(inst, bus),
            "INX" => self.op_inx(),
            "DEX" => self.op_dex(),
            "INY" => self.op_iny(),
            "DEY" => self.op_dey(),
            // Shift
            "ASL" => self.op_asl(inst, bus),
            "LSR" => self.op_lsr(inst, bus),
            "ROL" => self.op_rol(inst, bus),
            "ROR" => self.op_ror(inst, bus),
            // Bitwise
            "AND" => self.op_and(inst, bus),
            "ORA" => self.op_ora(inst, bus),
            "EOR" => self.op_eor(inst, bus),
            "BIT" => self.op_bit(inst, bus),
            // Compare
            "CMP" => self.op_cmp(inst, bus),
            "CPX" => self.op_cpx(inst, bus),
            "CPY" => self.op_cpy(inst, bus),
            // Branch
            "BCC" => self.op_bcc(inst, bus),
            "BCS" => self.op_bcs(inst, bus),
            "BEQ" => self.op_beq(inst, bus),
            "BNE" => self.op_bne(inst, bus),
            "BPL" => self.op_bpl(inst, bus),
            "BMI" => self.op_bmi(inst, bus),
            "BVC" => self.op_bvc(inst, bus),
            "BVS" => self.op_bvs(inst, bus),
            // Jump
            "JMP" => self.op_jmp(inst, bus),
            "JSR" => self.op_jsr(inst, bus),
            "RTS" => self.op_rts(bus),
            "BRK" => self.op_brk(bus),
            "RTI" => self.op_rti(bus),
            // Stack
            "PHA" => self.op_pha(bus),
            "PLA" => self.op_pla(bus),
            "PHP" => self.op_php(bus),
            "PLP" => self.op_plp(bus),
            "TXS" => self.op_txs(),
            "TSX" => self.op_tsx(),
            // Flags
            "CLC" => self.op_clc(),
            "SEC" => self.op_sec(),
            "CLI" => self.op_cli(),
            "SEI" => self.op_sei(),
            "CLD" => self.op_cld(),
            "SED" => self.op_sed(),
            "CLV" => self.op_clv(),
            // Other
            "NOP" => self.op_nop(inst),
            // Unofficial
            "SLO" => self.op_slo(inst, bus),
            "RLA" => self.op_rla(inst, bus),
            "SRE" => self.op_sre(inst, bus),
            "RRA" => self.op_rra(inst, bus),
            "SAX" => self.op_sax(inst, bus),
            "LAX" => self.op_lax(inst, bus),
            "DCP" => self.op_dcp(inst, bus),
            "ISC" => self.op_isc(inst, bus),
            _ => todo!("unknown opcode: ${:02X} ({})", inst.opcode, inst.mnemonic),
        }
    }

    /// Helper function for branch instructions
    fn branch(&mut self, condition: bool, inst: Instruction, bus: &mut Bus) -> u8 {
        if condition {
            // Branch taken
            let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
            self.pc = addr;

            // 3 cycles if branch taken, +1 if page boundary crossed
            adjust_cycles(3, page_crossed)
        } else {
            // Branch not taken - still need to skip the operand byte
            self.pc = self.pc.wrapping_add(1);
            2
        }
    }

    /// Push a value onto the stack
    fn push_stack(&mut self, bus: &mut Bus, value: u8) {
        let addr = STACK_BASE | (self.sp as u16);
        bus.cpu_write(addr, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    /// Push a 16-bit value onto the stack (high byte first)
    fn push_stack_u16(&mut self, bus: &mut Bus, value: u16) {
        let hi = (value >> 8) as u8;
        let lo = (value & 0xFF) as u8;
        self.push_stack(bus, hi);
        self.push_stack(bus, lo);
    }

    /// Pop a value from the stack
    fn pop_stack(&mut self, bus: &mut Bus) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let addr = STACK_BASE | (self.sp as u16);
        bus.cpu_read(addr)
    }

    /// LDA - Load Accumulator
    fn op_lda(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        self.a = bus.cpu_read(addr);
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for LDA"),
        }
    }

    /// STA - Store Accumulator
    fn op_sta(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        bus.cpu_write(addr, self.a);

        match inst.mode {
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => 5,
            Mode::AbsoluteY => 5,
            Mode::IndirectX => 6,
            Mode::IndirectY => 6,
            _ => panic!("unsupported addressing mode for STA"),
        }
    }

    /// LDX - Load Index X
    fn op_ldx(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        self.x = bus.cpu_read(addr);
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageY => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            _ => panic!("unsupported addressing mode for LDX"),
        }
    }

    /// STX - Store Index X
    fn op_stx(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        bus.cpu_write(addr, self.x);

        match inst.mode {
            Mode::ZeroPage => 3,
            Mode::ZeroPageY => 4,
            Mode::Absolute => 4,
            _ => panic!("unsupported addressing mode for STX"),
        }
    }

    /// LDY - Load Index Y
    fn op_ldy(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        self.y = bus.cpu_read(addr);
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            _ => panic!("unsupported addressing mode for LDY"),
        }
    }

    /// STY - Store Index Y
    fn op_sty(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        bus.cpu_write(addr, self.y);

        match inst.mode {
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            _ => panic!("unsupported addressing mode for STX"),
        }
    }

    /// TAX - Transfer Accumulator to X
    fn op_tax(&mut self) -> u8 {
        self.x = self.a;
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
        2
    }

    /// TXA - Transfer X to Accumulator
    fn op_txa(&mut self) -> u8 {
        self.a = self.x;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        2
    }

    /// TAY - Transfer Accumulator to Y
    fn op_tay(&mut self) -> u8 {
        self.y = self.a;
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);
        2
    }

    /// TYA - Transfer Y to Accumulator
    fn op_tya(&mut self) -> u8 {
        self.a = self.y;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        2
    }

    /// ADC - Add with Carry
    fn op_adc(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);
        let carry_in = if self.get_flag(CARRY) { 1 } else { 0 };

        let old_a = self.a;
        let (sum1, carry1) = self.a.overflowing_add(value);
        let (sum2, carry2) = sum1.overflowing_add(carry_in);
        self.a = sum2;

        self.set_flag(CARRY, carry1 || carry2);
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.set_flag(OVERFLOW, (self.a ^ old_a) & (self.a ^ value) & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for ADC"),
        }
    }

    /// SBC - Subtract with Carry
    fn op_sbc(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr) ^ 0xFF; // Invert bits for subtraction
        let carry_in = if self.get_flag(CARRY) { 1 } else { 0 };

        let old_a = self.a;
        let (sum1, carry1) = self.a.overflowing_add(value);
        let (sum2, carry2) = sum1.overflowing_add(carry_in);
        self.a = sum2;

        self.set_flag(CARRY, carry1 || carry2);
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.set_flag(OVERFLOW, (self.a ^ old_a) & (self.a ^ value) & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for SBC"),
        }
    }

    /// INC - Increment Memory
    fn op_inc(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);
        value = value.wrapping_add(1);
        bus.cpu_write(addr, value);
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => 7,
            _ => panic!("unsupported addressing mode for INC"),
        }
    }

    /// DEC - Decrement Memory
    fn op_dec(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);
        value = value.wrapping_sub(1);
        bus.cpu_write(addr, value);
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => 7,
            _ => panic!("unsupported addressing mode for DEC"),
        }
    }

    /// INX - Increment Index X
    fn op_inx(&mut self) -> u8 {
        self.x = self.x.wrapping_add(1);
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
        2
    }

    /// DEX - Decrement Index X
    fn op_dex(&mut self) -> u8 {
        self.x = self.x.wrapping_sub(1);
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
        2
    }

    /// INY - Increment Index Y
    fn op_iny(&mut self) -> u8 {
        self.y = self.y.wrapping_add(1);
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);
        2
    }

    /// DEY - Decrement Y Register
    fn op_dey(&mut self) -> u8 {
        self.y = self.y.wrapping_sub(1);
        self.set_flag(ZERO, self.y == 0);
        self.set_flag(NEGATIVE, self.y & 0x80 != 0);
        2
    }

    /// ASL - Arithmetic Shift Left
    fn op_asl(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        match inst.mode {
            Mode::Accumulator => {
                let carry = (self.a & 0x80) >> 7;
                self.a <<= 1;
                self.set_flag(CARRY, carry != 0);
                self.set_flag(ZERO, self.a == 0);
                self.set_flag(NEGATIVE, self.a & 0x80 != 0);
                2
            }
            _ => {
                let (addr, _) = self.get_operand_address(inst.mode, bus);
                let mut value = bus.cpu_read(addr);
                let carry = (value & 0x80) >> 7;
                value <<= 1;
                bus.cpu_write(addr, value);
                self.set_flag(CARRY, carry != 0);
                self.set_flag(ZERO, value == 0);
                self.set_flag(NEGATIVE, value & 0x80 != 0);

                match inst.mode {
                    Mode::ZeroPage => 5,
                    Mode::ZeroPageX => 6,
                    Mode::Absolute => 6,
                    Mode::AbsoluteX => 7,
                    _ => panic!("unsupported addressing mode for ASL"),
                }
            }
        }
    }

    /// LSR - Logical Shift Right
    fn op_lsr(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        match inst.mode {
            Mode::Accumulator => {
                let carry = self.a & 0x01;
                self.a >>= 1;
                self.set_flag(CARRY, carry != 0);
                self.set_flag(ZERO, self.a == 0);
                self.set_flag(NEGATIVE, false);
                2
            }
            _ => {
                let (addr, _) = self.get_operand_address(inst.mode, bus);
                let mut value = bus.cpu_read(addr);
                let carry = value & 0x01;
                value >>= 1;
                bus.cpu_write(addr, value);
                self.set_flag(CARRY, carry != 0);
                self.set_flag(ZERO, value == 0);
                self.set_flag(NEGATIVE, false);

                match inst.mode {
                    Mode::ZeroPage => 5,
                    Mode::ZeroPageX => 6,
                    Mode::Absolute => 6,
                    Mode::AbsoluteX => 7,
                    _ => panic!("unsupported addressing mode for LSR"),
                }
            }
        }
    }

    /// ROL - Rotate Left
    fn op_rol(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        match inst.mode {
            Mode::Accumulator => {
                let old_carry = if self.get_flag(CARRY) { 1 } else { 0 };
                let new_carry = (self.a & 0x80) >> 7;
                self.a = (self.a << 1) | old_carry;
                self.set_flag(CARRY, new_carry != 0);
                self.set_flag(ZERO, self.a == 0);
                self.set_flag(NEGATIVE, self.a & 0x80 != 0);
                2
            }
            _ => {
                let (addr, _) = self.get_operand_address(inst.mode, bus);
                let mut value = bus.cpu_read(addr);
                let old_carry = if self.get_flag(CARRY) { 1 } else { 0 };
                let new_carry = (value & 0x80) >> 7;
                value = (value << 1) | old_carry;
                bus.cpu_write(addr, value);
                self.set_flag(CARRY, new_carry != 0);
                self.set_flag(ZERO, value == 0);
                self.set_flag(NEGATIVE, value & 0x80 != 0);

                match inst.mode {
                    Mode::ZeroPage => 5,
                    Mode::ZeroPageX => 6,
                    Mode::Absolute => 6,
                    Mode::AbsoluteX => 7,
                    _ => panic!("unsupported addressing mode for ROL"),
                }
            }
        }
    }

    /// ROR - Rotate Right
    fn op_ror(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        match inst.mode {
            Mode::Accumulator => {
                let old_carry = if self.get_flag(CARRY) { 1 } else { 0 };
                let new_carry = self.a & 0x01;
                self.a = (self.a >> 1) | (old_carry << 7);
                self.set_flag(CARRY, new_carry != 0);
                self.set_flag(ZERO, self.a == 0);
                self.set_flag(NEGATIVE, self.a & 0x80 != 0);
                2
            }
            _ => {
                let (addr, _) = self.get_operand_address(inst.mode, bus);
                let mut value = bus.cpu_read(addr);
                let old_carry = if self.get_flag(CARRY) { 1 } else { 0 };
                let new_carry = value & 0x01;
                value = (value >> 1) | (old_carry << 7);
                bus.cpu_write(addr, value);
                self.set_flag(CARRY, new_carry != 0);
                self.set_flag(ZERO, value == 0);
                self.set_flag(NEGATIVE, value & 0x80 != 0);

                match inst.mode {
                    Mode::ZeroPage => 5,
                    Mode::ZeroPageX => 6,
                    Mode::Absolute => 6,
                    Mode::AbsoluteX => 7,
                    _ => panic!("unsupported addressing mode for ROR"),
                }
            }
        }
    }

    fn op_and(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);

        self.a &= value;

        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for AND"),
        }
    }

    fn op_ora(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);

        self.a |= value;

        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for ORA"),
        }
    }

    fn op_eor(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);

        self.a ^= value;

        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for EOR"),
        }
    }

    fn op_bit(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);

        self.set_flag(ZERO, (self.a & value) == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);
        self.set_flag(OVERFLOW, value & 0x40 != 0);

        match inst.mode {
            Mode::ZeroPage => 3,
            Mode::Absolute => 4,
            _ => panic!("unsupported addressing mode for BIT"),
        }
    }

    /// CMP - Compare Accumulator
    fn op_cmp(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);
        let result = self.a.wrapping_sub(value);

        self.set_flag(CARRY, self.a >= value);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageX => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteX => adjust_cycles(4, page_crossed),
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for CMP"),
        }
    }

    /// CPX - Compare Index X
    fn op_cpx(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);
        let result = self.x.wrapping_sub(value);

        self.set_flag(CARRY, self.x >= value);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::Absolute => 4,
            _ => panic!("unsupported addressing mode for CPX"),
        }
    }

    /// CPY - Compare Index Y
    fn op_cpy(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);
        let result = self.y.wrapping_sub(value);

        self.set_flag(CARRY, self.y >= value);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::Absolute => 4,
            _ => panic!("unsupported addressing mode for CPY"),
        }
    }

    /// JMP - Jump to Address
    fn op_jmp(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        self.pc = addr;

        match inst.mode {
            Mode::Absolute => 3,
            Mode::Indirect => 5,
            _ => panic!("unsupported addressing mode for JMP"),
        }
    }

    /// JSR - Jump to Subroutine
    fn op_jsr(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        // Get target address (this advances PC by 2)
        let (addr, _) = self.get_operand_address(inst.mode, bus);

        // Push return address (PC now points to next instruction, subtract 1 as per JSR spec)
        let return_addr = self.pc.wrapping_sub(1);
        self.push_stack(bus, ((return_addr >> 8) & 0xFF) as u8);
        self.push_stack(bus, (return_addr & 0xFF) as u8);

        // Jump to target
        self.pc = addr;

        match inst.mode {
            Mode::Absolute => 6,
            _ => panic!("unsupported addressing mode for JSR"),
        }
    }

    /// RTS - Return from Subroutine
    fn op_rts(&mut self, bus: &mut Bus) -> u8 {
        let lo = self.pop_stack(bus) as u16;
        let hi = self.pop_stack(bus) as u16;
        self.pc = ((hi << 8) | lo).wrapping_add(1);
        6
    }

    /// BRK - Force Interrupt
    fn op_brk(&mut self, bus: &mut Bus) -> u8 {
        // BRK triggers an interrupt sequence
        // PC has already been incremented by fetch, so we need to increment once more
        self.pc = self.pc.wrapping_add(1);
        
        // Push PC
        self.push_stack_u16(bus, self.pc);
        
        // Push status with B flag set
        let status = self.status | BREAK | UNUSED;
        self.push_stack(bus, status);
        
        // Set I flag
        self.set_flag(INTERRUPT_DISABLE, true);
        
        // Load PC from IRQ/BRK vector
        let lo = bus.cpu_read(0xFFFE) as u16;
        let hi = bus.cpu_read(0xFFFF) as u16;
        self.pc = (hi << 8) | lo;

        // Halt execution - BRK has no legitimate use in NES games
        self.halted = true;
        
        7
    }

    /// RTI - Return from Interrupt
    fn op_rti(&mut self, bus: &mut Bus) -> u8 {
        // Pop status from stack (restore all flags including I flag)
        let status = self.pop_stack(bus);
        self.status = (status & !UNUSED) | UNUSED;  // Restore status, keep U flag set
        
        // Pop PC from stack
        let lo = self.pop_stack(bus) as u16;
        let hi = self.pop_stack(bus) as u16;
        self.pc = (hi << 8) | lo;
        
        // RTI does NOT delay interrupts - they can trigger immediately if I flag was cleared
        // (This is different from CLI/SEI/PLP)

        6
    }

    /// BCC - Branch if Carry Clear
    fn op_bcc(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(!self.get_flag(CARRY), inst, bus)
    }

    /// BCS - Branch if Carry Set
    fn op_bcs(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(self.get_flag(CARRY), inst, bus)
    }

    /// BEQ - Branch if Equal (branch if zero flag is set)
    fn op_beq(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(self.get_flag(ZERO), inst, bus)
    }

    /// BNE - Branch if Not Equal (branch if zero flag is clear)
    fn op_bne(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(!self.get_flag(ZERO), inst, bus)
    }

    /// BPL - Branch if Positive (branch if negative flag is clear)
    fn op_bpl(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(!self.get_flag(NEGATIVE), inst, bus)
    }

    /// BMI - Branch if Minus (branch if negative flag is set)
    fn op_bmi(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(self.get_flag(NEGATIVE), inst, bus)
    }

    /// BVC - Branch if Overflow Clear
    fn op_bvc(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(!self.get_flag(OVERFLOW), inst, bus)
    }

    /// BVS - Branch if Overflow Set
    fn op_bvs(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        self.branch(self.get_flag(OVERFLOW), inst, bus)
    }

    /// PHA - Push Accumulator
    fn op_pha(&mut self, bus: &mut Bus) -> u8 {
        self.push_stack(bus, self.a);
        3
    }

    /// PLA - Pull Accumulator
    fn op_pla(&mut self, bus: &mut Bus) -> u8 {
        self.a = self.pop_stack(bus);
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        4
    }

    /// PHP - Push Processor Status
    fn op_php(&mut self, bus: &mut Bus) -> u8 {
        let status = self.status | UNUSED | BREAK;
        self.push_stack(bus, status);
        3
    }

    /// PLP - Pull Processor Status
    fn op_plp(&mut self, bus: &mut Bus) -> u8 {
        let status = self.pop_stack(bus);
        self.set_flag(CARRY, status & CARRY != 0);
        self.set_flag(ZERO, status & ZERO != 0);
        self.int_disable_pending = if status & INTERRUPT_DISABLE != 0 {
            InterruptDisablePending::Set
        } else {
            InterruptDisablePending::Clear
        };
        self.set_flag(DECIMAL, status & DECIMAL != 0);
        self.set_flag(OVERFLOW, status & OVERFLOW != 0);
        self.set_flag(NEGATIVE, status & NEGATIVE != 0);
        self.int_delay = true;  // Delay interrupt by 1 instruction
        4
    }

    /// TXS - Transfer X to Stack Pointer
    fn op_txs(&mut self) -> u8 {
        self.sp = self.x;
        2
    }

    /// TSX - Transfer Stack Pointer to X
    fn op_tsx(&mut self) -> u8 {
        self.x = self.sp;
        self.set_flag(ZERO, self.x == 0);
        self.set_flag(NEGATIVE, self.x & 0x80 != 0);
        2
    }

    /// CLC - Clear Carry Flag
    fn op_clc(&mut self) -> u8 {
        self.set_flag(CARRY, false);
        2
    }

    /// SEC - Set Carry Flag
    fn op_sec(&mut self) -> u8 {
        self.set_flag(CARRY, true);
        2
    }

    /// CLI - Clear Interrupt Disable
    fn op_cli(&mut self) -> u8 {
        // CLI has a one-instruction delay before taking effect
        self.int_disable_pending = InterruptDisablePending::Clear;
        self.int_delay = true;  // Delay interrupt by 1 instruction
        2
    }

    /// SEI - Set Interrupt Disable
    fn op_sei(&mut self) -> u8 {
        // SEI has a one-instruction delay before taking effect
        self.int_disable_pending = InterruptDisablePending::Set;
        self.int_delay = true;  // Delay interrupt by 1 instruction
        2
    }

    /// CLD - Clear Decimal Mode
    fn op_cld(&mut self) -> u8 {
        self.set_flag(DECIMAL, false);
        2
    }

    fn op_sed(&mut self) -> u8 {
        self.set_flag(DECIMAL, true);
        2
    }

    fn op_clv(&mut self) -> u8 {
        self.set_flag(OVERFLOW, false);
        2
    }

    /// NOP - No Operation
    fn op_nop(&mut self, inst: Instruction) -> u8 {
        self.pc = self.pc.wrapping_add((inst.bytes - 1) as u16);
        2
    }

    /// SLO - Shift Left and OR with Accumulator (Unofficial Opcode)
    fn op_slo(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);

        // ASL part
        let carry = (value & 0x80) >> 7;
        value <<= 1;
        bus.cpu_write(addr, value);
        self.set_flag(CARRY, carry != 0);
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        // ORA part
        self.a |= value;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => adjust_cycles(7, page_crossed),
            Mode::AbsoluteY => adjust_cycles(7, page_crossed),
            Mode::IndirectX => 8,
            Mode::IndirectY => adjust_cycles(7, page_crossed),
            _ => panic!("unsupported addressing mode for SLO: {:?}", inst.mode),
        }
    }

    /// RLA - Rotate Left and AND with Accumulator (Unofficial Opcode)
    fn op_rla(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);

        // ROL part
        let old_carry = if self.get_flag(CARRY) { 1 } else { 0 };
        let new_carry = (value & 0x80) >> 7;
        value = (value << 1) | old_carry;
        bus.cpu_write(addr, value);
        self.set_flag(CARRY, new_carry != 0);
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        // AND part
        self.a &= value;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => adjust_cycles(7, page_crossed),
            Mode::AbsoluteY => adjust_cycles(7, page_crossed),
            Mode::IndirectX => 8,
            Mode::IndirectY => adjust_cycles(7, page_crossed),
            _ => panic!("unsupported addressing mode for RLA: {:?}", inst.mode),
        }
    }

    /// SAX - Store A AND X (Unofficial Opcode)
    fn op_sax(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let value = self.a & self.x;
        bus.cpu_write(addr, value);

        match inst.mode {
            Mode::ZeroPage => 3,
            Mode::ZeroPageY => 4,
            Mode::Absolute => 4,
            Mode::IndirectX => 6,
            _ => panic!("unsupported addressing mode for SAX"),
        }
    }

    /// SRE - Shift Right and EOR with Accumulator (Unofficial Opcode)
    fn op_sre(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, _) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);

        // LSR part
        let carry = value & 0x01;
        value >>= 1;
        bus.cpu_write(addr, value);
        self.set_flag(CARRY, carry != 0);

        // EOR part
        self.a ^= value;
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => 7,
            Mode::AbsoluteY => 7,
            Mode::IndirectX => 8,
            Mode::IndirectY => 8,
            _ => panic!("unsupported addressing mode for SRE: {:?}", inst.mode),
        }
    }

    /// RRA - Rotate Right and Add with Carry (Unofficial Opcode)
    fn op_rra(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);

        // ROR part
        let old_carry = if self.get_flag(CARRY) { 1 } else { 0 };
        let new_carry = value & 0x01;
        value = (value >> 1) | (old_carry << 7);
        bus.cpu_write(addr, value);
        self.set_flag(CARRY, new_carry != 0);
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        // ADC part
        let carry_in = if self.get_flag(CARRY) { 1 } else { 0 };

        let old_a = self.a;
        let (sum1, carry1) = self.a.overflowing_add(value);
        let (sum2, carry2) = sum1.overflowing_add(carry_in);
        self.a = sum2;

        self.set_flag(CARRY, carry1 || carry2);
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.set_flag(OVERFLOW, (self.a ^ old_a) & (self.a ^ value) & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => adjust_cycles(7, page_crossed),
            Mode::AbsoluteY => adjust_cycles(7, page_crossed),
            Mode::IndirectX => 8,
            Mode::IndirectY => adjust_cycles(7, page_crossed),
            _ => panic!("unsupported addressing mode for RRA: {:?}", inst.mode),
        }
    }

    /// LAX - Load Accumulator and Index X (Unofficial Opcode)
    fn op_lax(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let value = bus.cpu_read(addr);
        self.a = value;
        self.x = value;
        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        match inst.mode {
            Mode::Immediate => 2,
            Mode::ZeroPage => 3,
            Mode::ZeroPageY => 4,
            Mode::Absolute => 4,
            Mode::AbsoluteY => adjust_cycles(4, page_crossed),
            Mode::IndirectX => 6,
            Mode::IndirectY => adjust_cycles(5, page_crossed),
            _ => panic!("unsupported addressing mode for LAX"),
        }
    }

    /// DCP - Decrement Memory and Compare with Accumulator (Unofficial Opcode)
    fn op_dcp(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);
        value = value.wrapping_sub(1);
        bus.cpu_write(addr, value);

        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        let result = self.a.wrapping_sub(value);
        self.set_flag(CARRY, self.a >= value);
        self.set_flag(ZERO, result == 0);
        self.set_flag(NEGATIVE, result & 0x80 != 0);

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => adjust_cycles(7, page_crossed),
            Mode::AbsoluteY => adjust_cycles(7, page_crossed),
            Mode::IndirectX => 8,
            Mode::IndirectY => adjust_cycles(7, page_crossed),
            _ => panic!("unsupported addressing mode for DCP: {:?}", inst.mode),
        }
    }

    /// ISC - Increment Memory and Subtract with Carry (Unofficial Opcode)
    fn op_isc(&mut self, inst: Instruction, bus: &mut Bus) -> u8 {
        let (addr, page_crossed) = self.get_operand_address(inst.mode, bus);
        let mut value = bus.cpu_read(addr);
        value = value.wrapping_add(1);
        bus.cpu_write(addr, value);

        self.set_flag(ZERO, value == 0);
        self.set_flag(NEGATIVE, value & 0x80 != 0);

        let value_inverted = value ^ 0xFF;
        let carry_in = if self.get_flag(CARRY) { 1 } else { 0 };

        let old_a = self.a;
        let (sum1, carry1) = self.a.overflowing_add(value_inverted);
        let (sum2, carry2) = sum1.overflowing_add(carry_in);
        self.a = sum2;

        self.set_flag(CARRY, carry1 || carry2);
        self.set_flag(ZERO, self.a == 0);
        self.set_flag(NEGATIVE, self.a & 0x80 != 0);
        self.set_flag(
            OVERFLOW,
            (self.a ^ old_a) & (self.a ^ value_inverted) & 0x80 != 0,
        );

        match inst.mode {
            Mode::ZeroPage => 5,
            Mode::ZeroPageX => 6,
            Mode::Absolute => 6,
            Mode::AbsoluteX => adjust_cycles(7, page_crossed),
            Mode::AbsoluteY => adjust_cycles(7, page_crossed),
            Mode::IndirectX => 8,
            Mode::IndirectY => adjust_cycles(7, page_crossed),
            _ => panic!("unsupported addressing mode for ISC: {:?}", inst.mode),
        }
    }

    /// Get the effective address for the given addressing mode.
    /// Returns (address, page_crossed).
    fn get_operand_address(&mut self, mode: Mode, bus: &mut Bus) -> (u16, bool) {
        match mode {
            Mode::Immediate => {
                let addr = self.pc;
                self.pc = self.pc.wrapping_add(1);
                (addr, false)
            }
            Mode::ZeroPage => {
                let addr = bus.cpu_read(self.pc) as u16;
                self.pc = self.pc.wrapping_add(1);
                (addr, false)
            }
            Mode::ZeroPageX => {
                let base = bus.cpu_read(self.pc);
                self.pc = self.pc.wrapping_add(1);
                let addr = base.wrapping_add(self.x) as u16;
                (addr, false)
            }
            Mode::ZeroPageY => {
                let base = bus.cpu_read(self.pc);
                self.pc = self.pc.wrapping_add(1);
                let addr = base.wrapping_add(self.y) as u16;
                (addr, false)
            }
            Mode::Absolute => {
                let lo = bus.cpu_read(self.pc) as u16;
                let hi = bus.cpu_read(self.pc.wrapping_add(1)) as u16;
                self.pc = self.pc.wrapping_add(2);
                let addr = (hi << 8) | lo;
                (addr, false)
            }
            Mode::AbsoluteX => {
                let lo = bus.cpu_read(self.pc) as u16;
                let hi = bus.cpu_read(self.pc.wrapping_add(1)) as u16;
                self.pc = self.pc.wrapping_add(2);
                let base = (hi << 8) | lo;
                let addr = base.wrapping_add(self.x as u16);
                let page_crossed = (base & 0xFF00) != (addr & 0xFF00);
                (addr, page_crossed)
            }
            Mode::AbsoluteY => {
                let lo = bus.cpu_read(self.pc) as u16;
                let hi = bus.cpu_read(self.pc.wrapping_add(1)) as u16;
                self.pc = self.pc.wrapping_add(2);
                let base = (hi << 8) | lo;
                let addr = base.wrapping_add(self.y as u16);
                let page_crossed = (base & 0xFF00) != (addr & 0xFF00);
                (addr, page_crossed)
            }
            Mode::Indirect => {
                let lo = bus.cpu_read(self.pc) as u16;
                let hi = bus.cpu_read(self.pc.wrapping_add(1)) as u16;
                self.pc = self.pc.wrapping_add(2);
                let ptr = (hi << 8) | lo;

                // 6502 bug: if pointer is at page boundary (xxFF), high byte wraps within same page
                let addr = if ptr & 0x00FF == 0x00FF {
                    let lo = bus.cpu_read(ptr) as u16;
                    let hi = bus.cpu_read(ptr & 0xFF00) as u16;
                    (hi << 8) | lo
                } else {
                    let lo = bus.cpu_read(ptr) as u16;
                    let hi = bus.cpu_read(ptr.wrapping_add(1)) as u16;
                    (hi << 8) | lo
                };
                (addr, false)
            }
            Mode::IndirectX => {
                let base = bus.cpu_read(self.pc);
                self.pc = self.pc.wrapping_add(1);
                let ptr = base.wrapping_add(self.x);
                let lo = bus.cpu_read(ptr as u16) as u16;
                let hi = bus.cpu_read(ptr.wrapping_add(1) as u16) as u16;
                let addr = (hi << 8) | lo;
                (addr, false)
            }
            Mode::IndirectY => {
                let ptr = bus.cpu_read(self.pc);
                self.pc = self.pc.wrapping_add(1);
                let lo = bus.cpu_read(ptr as u16) as u16;
                let hi = bus.cpu_read(ptr.wrapping_add(1) as u16) as u16;
                let base = (hi << 8) | lo;
                let addr = base.wrapping_add(self.y as u16);
                let page_crossed = (base & 0xFF00) != (addr & 0xFF00);
                (addr, page_crossed)
            }
            Mode::Relative => {
                let offset = bus.cpu_read(self.pc) as i8;
                self.pc = self.pc.wrapping_add(1);
                let addr = self.pc.wrapping_add(offset as u16);
                let page_crossed = (self.pc.wrapping_sub(1) & 0xFF00) != (addr & 0xFF00);
                (addr, page_crossed)
            }
            Mode::Implied | Mode::Accumulator => {
                // These modes don't have an operand address
                (0, false)
            }
        }
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculate additional cycles if a page boundary was crossed
fn adjust_cycles(base_cycles: u8, page_crossed: bool) -> u8 {
    if page_crossed {
        base_cycles + 1
    } else {
        base_cycles
    }
}
