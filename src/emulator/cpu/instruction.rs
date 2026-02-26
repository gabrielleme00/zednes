#[allow(dead_code)]

use crate::emulator::bus::Bus;

/// Addressing mode for an instruction
#[derive(Debug, Clone, Copy)]
pub enum AddressingMode {
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
}

/// Instruction information for disassembly
pub struct Instruction {
    pub opcode: u8,
    pub mnemonic: &'static str,
    pub mode: AddressingMode,
    pub bytes: u8,
}

impl Instruction {
    pub fn new(mnemonic: &'static str, mode: AddressingMode, bytes: u8) -> Self {
        Instruction {
            opcode: 0, // Placeholder, will be set in get_instruction
            mnemonic,
            mode,
            bytes,
        }
    }
}

/// Get instruction information by opcode
pub fn get_instruction(opcode: u8) -> Instruction {
    use Instruction as Inst;
    use AddressingMode::*;

    let mut inst = match opcode {
        // Access
        0xA9 => Inst::new("LDA", Immediate, 2),
        0xA5 => Inst::new("LDA", ZeroPage, 2),
        0xB5 => Inst::new("LDA", ZeroPageX, 2),
        0xAD => Inst::new("LDA", Absolute, 3),
        0xBD => Inst::new("LDA", AbsoluteX, 3),
        0xB9 => Inst::new("LDA", AbsoluteY, 3),
        0xA1 => Inst::new("LDA", IndirectX, 2),
        0xB1 => Inst::new("LDA", IndirectY, 2),

        0x85 => Inst::new("STA", ZeroPage, 2),
        0x95 => Inst::new("STA", ZeroPageX, 2),
        0x8D => Inst::new("STA", Absolute, 3),
        0x9D => Inst::new("STA", AbsoluteX, 3),
        0x99 => Inst::new("STA", AbsoluteY, 3),
        0x81 => Inst::new("STA", IndirectX, 2),
        0x91 => Inst::new("STA", IndirectY, 2),

        0xA2 => Inst::new("LDX", Immediate, 2),
        0xA6 => Inst::new("LDX", ZeroPage, 2),
        0xB6 => Inst::new("LDX", ZeroPageY, 2),
        0xAE => Inst::new("LDX", Absolute, 3),
        0xBE => Inst::new("LDX", AbsoluteY, 3),

        0x86 => Inst::new("STX", ZeroPage, 2),
        0x96 => Inst::new("STX", ZeroPageY, 2),
        0x8E => Inst::new("STX", Absolute, 3),

        0xA0 => Inst::new("LDY", Immediate, 2),
        0xA4 => Inst::new("LDY", ZeroPage, 2),
        0xB4 => Inst::new("LDY", ZeroPageX, 2),
        0xAC => Inst::new("LDY", Absolute, 3),
        0xBC => Inst::new("LDY", AbsoluteX, 3),

        0x84 => Inst::new("STY", ZeroPage, 2),
        0x94 => Inst::new("STY", ZeroPageX, 2),
        0x8C => Inst::new("STY", Absolute, 3),

        // Transfer
        0xAA => Inst::new("TAX", Implied, 1),
        0xA8 => Inst::new("TAY", Implied, 1),
        0x8A => Inst::new("TXA", Implied, 1),
        0x98 => Inst::new("TYA", Implied, 1),

        // Arithmetic
        0x69 => Inst::new("ADC", Immediate, 2),
        0x65 => Inst::new("ADC", ZeroPage, 2),
        0x75 => Inst::new("ADC", ZeroPageX, 2),
        0x6D => Inst::new("ADC", Absolute, 3),
        0x7D => Inst::new("ADC", AbsoluteX, 3),
        0x79 => Inst::new("ADC", AbsoluteY, 3),
        0x61 => Inst::new("ADC", IndirectX, 2),
        0x71 => Inst::new("ADC", IndirectY, 2),

        0xE9 => Inst::new("SBC", Immediate, 2),
        0xE5 => Inst::new("SBC", ZeroPage, 2),
        0xF5 => Inst::new("SBC", ZeroPageX, 2),
        0xED => Inst::new("SBC", Absolute, 3),
        0xFD => Inst::new("SBC", AbsoluteX, 3),
        0xF9 => Inst::new("SBC", AbsoluteY, 3),
        0xE1 => Inst::new("SBC", IndirectX, 2),
        0xF1 => Inst::new("SBC", IndirectY, 2),

        0xE6 => Inst::new("INC", ZeroPage, 2),
        0xF6 => Inst::new("INC", ZeroPageX, 2),
        0xEE => Inst::new("INC", Absolute, 3),
        0xFE => Inst::new("INC", AbsoluteX, 3),

        0xC6 => Inst::new("DEC", ZeroPage, 2),
        0xD6 => Inst::new("DEC", ZeroPageX, 2),
        0xCE => Inst::new("DEC", Absolute, 3),
        0xDE => Inst::new("DEC", AbsoluteX, 3),

        0xE8 => Inst::new("INX", Implied, 1),
        0xCA => Inst::new("DEX", Implied, 1),
        0xC8 => Inst::new("INY", Implied, 1),
        0x88 => Inst::new("DEY", Implied, 1),

        // Shift
        0x0A => Inst::new("ASL", Accumulator, 1),
        0x06 => Inst::new("ASL", ZeroPage, 2),
        0x16 => Inst::new("ASL", ZeroPageX, 2),
        0x0E => Inst::new("ASL", Absolute, 3),
        0x1E => Inst::new("ASL", AbsoluteX, 3),

        0x4A => Inst::new("LSR", Accumulator, 1),
        0x46 => Inst::new("LSR", ZeroPage, 2),
        0x56 => Inst::new("LSR", ZeroPageX, 2),
        0x4E => Inst::new("LSR", Absolute, 3),
        0x5E => Inst::new("LSR", AbsoluteX, 3),

        0x2A => Inst::new("ROL", Accumulator, 1),
        0x26 => Inst::new("ROL", ZeroPage, 2),
        0x36 => Inst::new("ROL", ZeroPageX, 2),
        0x2E => Inst::new("ROL", Absolute, 3),
        0x3E => Inst::new("ROL", AbsoluteX, 3),

        0x6A => Inst::new("ROR", Accumulator, 1),
        0x66 => Inst::new("ROR", ZeroPage, 2),
        0x76 => Inst::new("ROR", ZeroPageX, 2),
        0x6E => Inst::new("ROR", Absolute, 3),
        0x7E => Inst::new("ROR", AbsoluteX, 3),

        // Bitwise
        0x29 => Inst::new("AND", Immediate, 2),
        0x25 => Inst::new("AND", ZeroPage, 2),
        0x35 => Inst::new("AND", ZeroPageX, 2),
        0x2D => Inst::new("AND", Absolute, 3),
        0x3D => Inst::new("AND", AbsoluteX, 3),
        0x39 => Inst::new("AND", AbsoluteY, 3),
        0x21 => Inst::new("AND", IndirectX, 2),
        0x31 => Inst::new("AND", IndirectY, 2),

        0x09 => Inst::new("ORA", Immediate, 2),
        0x05 => Inst::new("ORA", ZeroPage, 2),
        0x15 => Inst::new("ORA", ZeroPageX, 2),
        0x0D => Inst::new("ORA", Absolute, 3),
        0x1D => Inst::new("ORA", AbsoluteX, 3),
        0x19 => Inst::new("ORA", AbsoluteY, 3),
        0x01 => Inst::new("ORA", IndirectX, 2),
        0x11 => Inst::new("ORA", IndirectY, 2),

        0x49 => Inst::new("EOR", Immediate, 2),
        0x45 => Inst::new("EOR", ZeroPage, 2),
        0x55 => Inst::new("EOR", ZeroPageX, 2),
        0x4D => Inst::new("EOR", Absolute, 3),
        0x5D => Inst::new("EOR", AbsoluteX, 3),
        0x59 => Inst::new("EOR", AbsoluteY, 3),
        0x41 => Inst::new("EOR", IndirectX, 2),
        0x51 => Inst::new("EOR", IndirectY, 2),

        0x24 => Inst::new("BIT", ZeroPage, 2),
        0x2C => Inst::new("BIT", Absolute, 3),

        // Compare
        0xC9 => Inst::new("CMP", Immediate, 2),
        0xC5 => Inst::new("CMP", ZeroPage, 2),
        0xD5 => Inst::new("CMP", ZeroPageX, 2),
        0xCD => Inst::new("CMP", Absolute, 3),
        0xDD => Inst::new("CMP", AbsoluteX, 3),
        0xD9 => Inst::new("CMP", AbsoluteY, 3),
        0xC1 => Inst::new("CMP", IndirectX, 2),
        0xD1 => Inst::new("CMP", IndirectY, 2),

        0xE0 => Inst::new("CPX", Immediate, 2),
        0xE4 => Inst::new("CPX", ZeroPage, 2),
        0xEC => Inst::new("CPX", Absolute, 3),

        0xC0 => Inst::new("CPY", Immediate, 2),
        0xC4 => Inst::new("CPY", ZeroPage, 2),
        0xCC => Inst::new("CPY", Absolute, 3),

        // Branch
        0x90 => Inst::new("BCC", Relative, 2),
        0xB0 => Inst::new("BCS", Relative, 2),
        0xF0 => Inst::new("BEQ", Relative, 2),
        0xD0 => Inst::new("BNE", Relative, 2),
        0x10 => Inst::new("BPL", Relative, 2),
        0x30 => Inst::new("BMI", Relative, 2),
        0x50 => Inst::new("BVC", Relative, 2),
        0x70 => Inst::new("BVS", Relative, 2),

        // Jump
        0x4C => Inst::new("JMP", Absolute, 3),
        0x6C => Inst::new("JMP", Indirect, 3),
        0x20 => Inst::new("JSR", Absolute, 3),
        0x60 => Inst::new("RTS", Implied, 1),
        0x00 => Inst::new("BRK", Implied, 1),
        0x40 => Inst::new("RTI", Implied, 1),

        // Stack
        0x48 => Inst::new("PHA", Implied, 1),
        0x68 => Inst::new("PLA", Implied, 1),
        0x08 => Inst::new("PHP", Implied, 1),
        0x28 => Inst::new("PLP", Implied, 1),
        0x9A => Inst::new("TXS", Implied, 1),
        0xBA => Inst::new("TSX", Implied, 1),

        // Flags
        0x18 => Inst::new("CLC", Implied, 1),
        0x38 => Inst::new("SEC", Implied, 1),
        0x58 => Inst::new("CLI", Implied, 1),
        0x78 => Inst::new("SEI", Implied, 1),
        0xD8 => Inst::new("CLD", Implied, 1),
        0xF8 => Inst::new("SED", Implied, 1),
        0xB8 => Inst::new("CLV", Implied, 1),

        // Other
        0xEA => Inst::new("NOP", Implied, 1),

        // Unofficial
        0x03 => Inst::new("SLO", IndirectX, 2),
        0x13 => Inst::new("SLO", IndirectY, 2),
        0x07 => Inst::new("SLO", ZeroPage, 2),
        0x17 => Inst::new("SLO", ZeroPageX, 2),
        0x0F => Inst::new("SLO", Absolute, 3),
        0x1F => Inst::new("SLO", AbsoluteX, 3),
        0x1B => Inst::new("SLO", AbsoluteY, 3),

        0x23 => Inst::new("RLA", IndirectX, 2),
        0x27 => Inst::new("RLA", ZeroPage, 2),
        0x2F => Inst::new("RLA", Absolute, 3),
        0x33 => Inst::new("RLA", IndirectY, 2),
        0x37 => Inst::new("RLA", ZeroPageX, 2),
        0x3B => Inst::new("RLA", AbsoluteY, 3),
        0x3F => Inst::new("RLA", AbsoluteX, 3),

        0x43 => Inst::new("SRE", IndirectX, 2),
        0x47 => Inst::new("SRE", ZeroPage, 2),
        0x4F => Inst::new("SRE", Absolute, 3),
        0x53 => Inst::new("SRE", IndirectY, 2),
        0x57 => Inst::new("SRE", ZeroPageX, 2),
        0x5B => Inst::new("SRE", AbsoluteY, 3),
        0x5F => Inst::new("SRE", AbsoluteX, 3),

        0x63 => Inst::new("RRA", IndirectX, 2),
        0x67 => Inst::new("RRA", ZeroPage, 2),
        0x6F => Inst::new("RRA", Absolute, 3),
        0x73 => Inst::new("RRA", IndirectY, 2),
        0x77 => Inst::new("RRA", ZeroPageX, 2),
        0x7B => Inst::new("RRA", AbsoluteY, 3),
        0x7F => Inst::new("RRA", AbsoluteX, 3),

        0x83 => Inst::new("SAX", IndirectX, 2),
        0x87 => Inst::new("SAX", ZeroPage, 2),
        0x8F => Inst::new("SAX", Absolute, 3),
        0x97 => Inst::new("SAX", ZeroPageY, 2),

        0xA3 => Inst::new("LAX", IndirectX, 2),
        0xA7 => Inst::new("LAX", ZeroPage, 2),
        0xAB => Inst::new("LAX", Immediate, 2),
        0xAF => Inst::new("LAX", Absolute, 3),
        0xB3 => Inst::new("LAX", IndirectY, 2),
        0xB7 => Inst::new("LAX", ZeroPageY, 2),
        0xBF => Inst::new("LAX", AbsoluteY, 3),

        0xEB => Inst::new("SBC", Immediate, 2),

        0xC3 => Inst::new("DCP", IndirectX, 2),
        0xC7 => Inst::new("DCP", ZeroPage, 2),
        0xCF => Inst::new("DCP", Absolute, 3),
        0xD3 => Inst::new("DCP", IndirectY, 2),
        0xD7 => Inst::new("DCP", ZeroPageX, 2),
        0xDB => Inst::new("DCP", AbsoluteY, 3),
        0xDF => Inst::new("DCP", AbsoluteX, 3),

        0xE3 => Inst::new("ISC", IndirectX, 2),
        0xE7 => Inst::new("ISC", ZeroPage, 2),
        0xEF => Inst::new("ISC", Absolute, 3),
        0xF3 => Inst::new("ISC", IndirectY, 2),
        0xF7 => Inst::new("ISC", ZeroPageX, 2),
        0xFB => Inst::new("ISC", AbsoluteY, 3),
        0xFF => Inst::new("ISC", AbsoluteX, 3),

        0x04 => Inst::new("NOP", ZeroPage, 2),
        0x44 => Inst::new("NOP", ZeroPage, 2),
        0x64 => Inst::new("NOP", ZeroPage, 2),
        0x0C => Inst::new("NOP", Absolute, 3),
        0x14 => Inst::new("NOP", ZeroPageX, 2),
        0x34 => Inst::new("NOP", ZeroPageX, 2),
        0x54 => Inst::new("NOP", ZeroPageX, 2),
        0x74 => Inst::new("NOP", ZeroPageX, 2),
        0xD4 => Inst::new("NOP", ZeroPageX, 2),
        0xF4 => Inst::new("NOP", ZeroPageX, 2),
        0x1C => Inst::new("NOP", AbsoluteX, 3),
        0x3C => Inst::new("NOP", AbsoluteX, 3),
        0x5C => Inst::new("NOP", AbsoluteX, 3),
        0x7C => Inst::new("NOP", AbsoluteX, 3),
        0xDC => Inst::new("NOP", AbsoluteX, 3),
        0xFC => Inst::new("NOP", AbsoluteX, 3),
        0x1A => Inst::new("NOP", Implied, 1),
        0x3A => Inst::new("NOP", Implied, 1),
        0x5A => Inst::new("NOP", Implied, 1),
        0x7A => Inst::new("NOP", Implied, 1),
        0xDA => Inst::new("NOP", Implied, 1),
        0xFA => Inst::new("NOP", Implied, 1),
        0x80 => Inst::new("NOP", Immediate, 2),
        0x89 => Inst::new("NOP", Immediate, 2),

        _ => Inst::new("???", Implied, 1),
    };

    inst.opcode = opcode;

    inst
}

/// Disassemble instruction at given address. Returns formatted string and instruction length.
pub fn disassemble_at(addr: u16, bus: &Bus) -> (String, u8) {
    let opcode = bus.peek(addr);
    let instr = get_instruction(opcode);

    let operand = match instr.bytes {
        1 => String::new(),
        2 => {
            let byte1 = bus.peek(addr.wrapping_add(1));
            match instr.mode {
                AddressingMode::Immediate => format!("#${:02X}", byte1),
                AddressingMode::ZeroPage => format!("${:02X}", byte1),
                AddressingMode::ZeroPageX => format!("${:02X},X", byte1),
                AddressingMode::ZeroPageY => format!("${:02X},Y", byte1),
                AddressingMode::IndirectX => format!("(${:02X},X)", byte1),
                AddressingMode::IndirectY => format!("(${:02X}),Y", byte1),
                AddressingMode::Relative => {
                    let offset = byte1 as i8;
                    let target = addr.wrapping_add(2).wrapping_add(offset as u16);
                    format!("${:04X}", target)
                }
                _ => format!("${:02X}", byte1),
            }
        }
        3 => {
            let lo = bus.peek(addr.wrapping_add(1));
            let hi = bus.peek(addr.wrapping_add(2));
            let addr16 = ((hi as u16) << 8) | (lo as u16);
            match instr.mode {
                AddressingMode::Absolute => format!("${:04X}", addr16),
                AddressingMode::AbsoluteX => format!("${:04X},X", addr16),
                AddressingMode::AbsoluteY => format!("${:04X},Y", addr16),
                AddressingMode::Indirect => format!("(${:04X})", addr16),
                _ => format!("${:04X}", addr16),
            }
        }
        _ => String::new(),
    };

    let formatted = if operand.is_empty() {
        format!("{:04X}\t{:02X}      \t{}      ", addr, opcode, instr.mnemonic)
    } else {
        let bytes_str = match instr.bytes {
            2 => {
                let byte1 = bus.peek(addr.wrapping_add(1));
                format!("{:02X} {:02X}   ", opcode, byte1)
            }
            3 => {
                let byte1 = bus.peek(addr.wrapping_add(1));
                let byte2 = bus.peek(addr.wrapping_add(2));
                format!("{:02X} {:02X} {:02X}", opcode, byte1, byte2)
            }
            _ => format!("{:02X}\t\t", opcode),
        };
        format!(
            "{:04X}\t{}\t{} {}",
            addr, bytes_str, instr.mnemonic, operand
        )
    };

    (formatted, instr.bytes)
}
