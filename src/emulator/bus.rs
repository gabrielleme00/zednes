use std::cell::RefCell;
use std::rc::Rc;

use super::{Apu, Cartridge, Ppu, TvSystem, Controller};

const RAM_SIZE: usize = 64 * 1024; // 64KB of fake RAM for testing purposes

/// Memory bus that connects CPU to RAM, PPU, and cartridge
pub struct Bus {
    // CPU RAM
    pub cpu_ram: [u8; RAM_SIZE],

    // References to other components
    pub controllers: [Controller; 2],
    pub ppu: Ppu,
    pub cartridge: Option<Rc<RefCell<Cartridge>>>,
    pub apu: Apu,

    // For OAM DMA transfers
    dma_page: u8,
    dma_addr: u8,
    dma_data: u8,
    dma_transfer: bool,
    dma_dummy: bool,
    dma_sync: bool,

    // Tracks the CPU cycle count at the time of the last CPU step (for odd-cycle DMA detection)
    pub cpu_cycles: u64,
}

impl Bus {
    /// Create a new bus with the default TV system (NTSC).
    pub fn new() -> Self {
        Self::new_with_tv_system(TvSystem::Ntsc)
    }

    /// Create a new bus with the specified TV system (NTSC or PAL).
    pub fn new_with_tv_system(tv_system: TvSystem) -> Self {
        Bus {
            cpu_ram: [0; RAM_SIZE],
            controllers: [Controller::new(), Controller::new()],
            ppu: Ppu::new(tv_system),
            cartridge: None,
            apu: Apu::new_with_tv_system(tv_system),
            dma_page: 0,
            dma_addr: 0,
            dma_data: 0,
            dma_transfer: false,
            dma_dummy: true,
            dma_sync: true,
            cpu_cycles: 0,
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
            0x4000..=0x4015 => self.apu.cpu_read(addr),
            0x4016 => self.controllers[0].peek_bit(),
            0x4017 => self.controllers[1].peek_bit(),

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
            0x4000..=0x4015 => self.apu.cpu_read(addr),
            0x4016 => self.controllers[0].read_serial(),
            0x4017 => self.controllers[1].read_serial(),

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
                self.dma_page = data;
                self.dma_addr = 0;
                self.dma_transfer = true;
                self.dma_dummy = self.cpu_cycles % 2 != 0;
                self.dma_sync = true;
            }

            // APU and I/O registers
            0x4000..=0x4013 | 0x4015 | 0x4017 => {
                self.apu.cpu_write(addr, data);
            }

            0x4016 => {
                // Controller strobe: bit 0 controls both controllers
                let high = data & 0x01 != 0;
                self.controllers[0].set_strobe(high);
                self.controllers[1].set_strobe(high);
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

    pub fn dma_active(&self) -> bool {
        self.dma_transfer
    }

    pub fn tick_dma(&mut self) -> u8 {
        if self.dma_dummy {
            self.dma_dummy = false;
            return 1;
        }

        let addr = ((self.dma_page as u16) << 8) | (self.dma_addr as u16);

        if self.dma_sync {
            // Read cycle
            self.dma_data = self.cpu_read(addr);
            self.dma_sync = false;
        } else {
            // Write cycle
            self.ppu.cpu_write(0x0004, self.dma_data);
            self.dma_addr = self.dma_addr.wrapping_add(1);
            if self.dma_addr == 0 {
                self.dma_transfer = false;
            }
            self.dma_sync = true;
        }

        1
    }
}

impl Default for Bus {
    fn default() -> Self {
        Self::new()
    }
}
