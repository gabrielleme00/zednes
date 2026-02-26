use std::cell::RefCell;
use std::rc::Rc;

use super::cartridge::Cartridge;
use super::ppu::{Ppu, TvSystem};

const RAM_SIZE: usize = 64 * 1024; // 64KB of fake RAM for testing purposes

/// Memory bus that connects CPU to RAM, PPU, and cartridge
pub struct Bus {
    // CPU RAM
    pub cpu_ram: [u8; RAM_SIZE],

    // Controllers (placeholders)
    pub controller1: u8,
    pub controller2: u8,

    // References to other components
    pub ppu: Ppu,
    pub cartridge: Option<Rc<RefCell<Cartridge>>>,
    
    // PPUDATA read buffer (for buffered reads)
    _ppu_data_buffer: u8,
}

impl Bus {
    pub fn new() -> Self {
        Self::new_with_tv_system(TvSystem::Ntsc)
    }

    pub fn new_with_tv_system(tv_system: TvSystem) -> Self {
        Bus {
            cpu_ram: [0; RAM_SIZE],
            controller1: 0,
            controller2: 0,
            ppu: Ppu::new(tv_system),
            cartridge: None,
            _ppu_data_buffer: 0,
        }
    }

    /// Read a byte from the bus without side effects
    pub fn peek(&self, addr: u16) -> u8 {
        match addr {
            // RAM (mirrored)
            0x0000..=0x1FFF => self.cpu_ram[(addr & 0x07FF) as usize],

            // PPU Registers (mirrored every 8 bytes from 0x2000 to 0x3FFF)
            0x2000..=0x3FFF => self.ppu.peek(addr & 0x0007),

            // APU and I/O registers
            0x4000..=0x4015 => {
                // APU registers (not implemented yet)
                0
            }
            0x4016 => self.controller1,
            0x4017 => self.controller2,

            // Cartridge space
            0x4020..=0xFFFF => {
                if let Some(ref cart) = self.cartridge {
                    cart.borrow().cpu_read(addr).0
                } else {
                    0
                }
            }

            _ => 0,
        }
    }

    /// Returns the mapper-intercepted read value if the mapper handled the address,
    /// or None to let the normal bus logic handle it.
    fn mapper_intercepts_read(&self, addr: u16) -> Option<u8> {
        self.cartridge.as_ref().and_then(|c| {
            let (val, handled) = c.borrow().cpu_read(addr);
            if handled { Some(val) } else { None }
        })
    }

    /// Returns true if the mapper handled the write (e.g. bank switching).
    fn mapper_intercepts_write(&self, addr: u16, data: u8) -> bool {
        self.cartridge
            .as_ref()
            .map_or(false, |c| c.borrow_mut().cpu_write(addr, data))
    }

    /// Read a byte from the bus with side effects (from the CPU)
    pub fn cpu_read(&mut self, addr: u16) -> u8 {
        if let Some(val) = self.mapper_intercepts_read(addr) {
            return val;
        }
        match addr {
            // RAM (mirrored)
            0x0000..=0x1FFF => self.cpu_ram[(addr & 0x07FF) as usize],

            // PPU Registers (mirrored every 8 bytes from 0x2000 to 0x3FFF)
            0x2000..=0x3FFF => self.ppu.cpu_read(addr & 0x0007),

            // APU and I/O registers
            0x4000..=0x4015 => {
                // APU registers (not implemented yet)
                0
            }
            0x4016 => self.controller1,
            0x4017 => self.controller2,

            // Cartridge space
            0x4020..=0xFFFF => {
                if let Some(ref cart) = self.cartridge {
                    cart.borrow().cpu_read(addr).0
                } else {
                    0
                }
            }

            _ => 0,
        }
    }

    /// Write a byte to the bus (from the CPU)
    pub fn cpu_write(&mut self, addr: u16, data: u8) {
        if self.mapper_intercepts_write(addr, data) {
            return;
        }
        match addr {
            // RAM (mirrored)
            0x0000..=0x1FFF => {
                self.cpu_ram[(addr & 0x07FF) as usize] = data;
            }

            // PPU Registers (mirrored)
            0x2000..=0x3FFF => {
                self.ppu.cpu_write(addr & 0x0007, data);
            }

            // OAM DMA
            0x4014 => {
                // DMA transfer from CPU memory to PPU OAM
                let page = (data as u16) << 8;
                for i in 0..256 {
                    let byte = self.cpu_read(page | i);
                    self.ppu.oam_data[i as usize] = byte;
                }
            }

            // APU and I/O registers
            0x4000..=0x4013 | 0x4015 | 0x4017 => {
                // APU registers (not implemented yet)
            }

            0x4016 => {
                // Controller strobe
                self.controller1 = data;
                self.controller2 = data;
            }

            0x4018..=0x401F => {} // APU and I/O functionality that is normally disabled

            // Cartridge space
            0x4020..=0xFFFF => {
                if let Some(ref cart) = self.cartridge {
                    cart.borrow_mut().cpu_write(addr, data);
                }
            }
        }
    }

    /// Loads a cartridge into the bus, making it available for CPU and PPU access
    pub fn load_cartridge(&mut self, cartridge: Cartridge) {
        let shared = Rc::new(RefCell::new(cartridge));
        self.ppu.load_cartridge(Rc::clone(&shared));
        self.cartridge = Some(shared);
    }
}

impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}
