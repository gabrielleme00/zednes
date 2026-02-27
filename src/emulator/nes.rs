use super::bus::Bus;
use super::cartridge::Cartridge;
use super::cpu::Cpu;
use super::ppu::TvSystem;

const DEFAULT_TV_SYSTEM: TvSystem = TvSystem::Ntsc;

/// Main NES emulator structure
pub struct Nes {
    pub cpu: Cpu,
    pub bus: Bus,

    // PPU/CPU cycle ratio numerator and denominator (derived from TV system at boot)
    ppu_mult: u32,
    cpu_mult: u32,

    // PPU cycle accumulator for PAL timing (3.2 PPU cycles per CPU cycle)
    ppu_cycle_debt: u32,
}

impl Nes {
    pub fn new() -> Self {
        Self::new_with_tv_system(DEFAULT_TV_SYSTEM)
    }

    pub fn new_with_tv_system(tv_system: TvSystem) -> Self {
        let (ppu_mult, cpu_mult) = tv_system.ppu_cycles_per_cpu();
        Nes {
            cpu: Cpu::new(),
            bus: Bus::new_with_tv_system(tv_system),
            ppu_mult,
            cpu_mult,
            ppu_cycle_debt: 0,
        }
    }

    /// Load a ROM into the NES
    pub fn load_rom(&mut self, rom_data: &[u8]) -> Result<(), String> {
        let cartridge = Cartridge::new(rom_data)?;
        self.bus.load_cartridge(cartridge);

        // Reset CPU and load PC from reset vector
        self.cpu.reset();
        self.load_reset_vector();

        Ok(())
    }

    /// Reset the NES system
    pub fn reset(&mut self) {
        self.cpu.reset();
        self.bus.ppu.reset();
        self.load_reset_vector();
    }

    /// Execute one CPU instruction and corresponding PPU cycles
    pub fn step(&mut self) {
        // Execute one CPU instruction
        let cpu_cycles = self.cpu.step(&mut self.bus);

        self.step_ppu_for_cpu_cycles(cpu_cycles);
    }

    /// Advance the PPU by the number of PPU cycles that correspond to the given CPU cycles,
    /// carrying over any fractional cycles (needed for PAL's 3.2 ratio).
    fn step_ppu_for_cpu_cycles(&mut self, cpu_cycles: u8) {
        // For NTSC: 3 PPU cycles per CPU cycle (3/1 = 3.0)
        // For PAL: 16 PPU cycles per 5 CPU cycles (16/5 = 3.2)
        let total_ppu_cycles = (cpu_cycles as u32 * self.ppu_mult) + self.ppu_cycle_debt;
        let ppu_cycles_to_run = total_ppu_cycles / self.cpu_mult;
        self.ppu_cycle_debt = total_ppu_cycles % self.cpu_mult;

        for _ in 0..ppu_cycles_to_run {
            self.step_ppu();
        }
    }

    /// Step the PPU independently
    fn step_ppu(&mut self) {
        // Run one PPU cycle and check for NMI
        let nmi = self.bus.ppu.step();

        // Trigger NMI if PPU signals it (edge-triggered)
        if nmi {
            self.cpu.set_nmi(true);
        } else {
            self.cpu.set_nmi(false);
        }
    }

    /// Run the NES for one complete frame (until PPU frame counter increments)
    pub fn step_frame(&mut self) {
        let target_frame = self.bus.ppu.frame + 1;

        // Run until we reach the next frame, stopping early if the CPU halts
        while self.bus.ppu.frame < target_frame {
            if self.cpu.halted {
                break;
            }
            self.step();
        }
    }

    /// Load PC from reset vector at 0xFFFC-0xFFFD
    fn load_reset_vector(&mut self) {
        let lo = self.bus.cpu_read(0xFFFC) as u16;
        let hi = self.bus.cpu_read(0xFFFD) as u16;
        self.cpu.pc = (hi << 8) | lo;
    }

    /// Set PC directly (for testing)
    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.pc = pc;
    }

    /// Read CPU memory at the given address (for disassembly)
    pub fn read_cpu_mem(&self, addr: u16) -> u8 {
        self.bus.peek(addr)
    }

    /// Get current PC
    pub fn get_pc(&self) -> u16 {
        self.cpu.pc
    }

    /// Get CPU registers for display
    pub fn get_cpu_state(&self) -> (u8, u8, u8, u8, u16, u8) {
        (self.cpu.a, self.cpu.x, self.cpu.y, self.cpu.sp, self.cpu.pc, self.cpu.status)
    }

    /// Get palette entry from PPU
    pub fn get_palette(&self, index: usize) -> u8 {
        self.bus.ppu.tbl_palette[index]
    }

    /// Get all 256 tiles from the specified pattern table (0 or 1)
    pub fn get_pattern_table(&self, index: u8) -> Vec<[u8; 64]> {
        self.bus.ppu.get_pattern_table(index)
    }

    /// Get the RGB color for a given palette index (0-7) and pixel value (0-3).
    /// Pixel 0 always returns the universal background color.
    pub fn get_color_from_palette_ram(&self, palette_index: u8, pixel: u8) -> (u8, u8, u8) {
        self.bus.ppu.get_color_from_palette_ram(palette_index, pixel)
    }
}

impl Default for Nes {
    fn default() -> Self {
        Self::new()
    }
}
