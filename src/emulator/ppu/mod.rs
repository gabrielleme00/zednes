mod tv_system;

mod ppuctrl_flags {
    // pub const NAMETABLE_X: u8 = 0b0000_0001;
    // pub const NAMETABLE_Y: u8 = 0b0000_0010;
    pub const INCREMENT_MODE: u8 = 0b0000_0100;
    // pub const SPRITE_PATTERN: u8 = 0b0000_1000;
    // pub const BACKGROUND_PATTERN: u8 = 0b0001_0000;
    // pub const SPRITE_SIZE: u8 = 0b0010_0000;
    // pub const SLAVE_MODE: u8 = 0b0100_0000;
    pub const ENABLE_NMI: u8 = 0b1000_0000;
}

mod ppustatus_flags {
    pub const SPRITE_OVERFLOW: u8 = 0b0010_0000;
    pub const SPRITE_ZERO_HIT: u8 = 0b0100_0000;
    pub const VBLANK: u8 = 0b1000_0000;
}

pub use tv_system::TvSystem;

use std::cell::RefCell;
use std::rc::Rc;

use super::cartridge::Cartridge;
use ppuctrl_flags::*;
use ppustatus_flags::*;

// NES Color Palette (64 colors in RGB format)
pub const SYSTEM_PALETTE: [(u8, u8, u8); 64] = [
    (84, 84, 84),
    (0, 30, 116),
    (8, 16, 144),
    (48, 0, 136),
    (68, 0, 100),
    (92, 0, 48),
    (84, 4, 0),
    (60, 24, 0),
    (32, 42, 0),
    (8, 58, 0),
    (0, 64, 0),
    (0, 60, 0),
    (0, 50, 60),
    (0, 0, 0),
    (0, 0, 0),
    (0, 0, 0),
    (152, 150, 152),
    (8, 76, 196),
    (48, 50, 236),
    (92, 30, 228),
    (136, 20, 176),
    (160, 20, 100),
    (152, 34, 32),
    (120, 60, 0),
    (84, 90, 0),
    (40, 114, 0),
    (8, 124, 0),
    (0, 118, 40),
    (0, 102, 120),
    (0, 0, 0),
    (0, 0, 0),
    (0, 0, 0),
    (236, 238, 236),
    (76, 154, 236),
    (120, 124, 236),
    (176, 98, 236),
    (228, 84, 236),
    (236, 88, 180),
    (236, 106, 100),
    (212, 136, 32),
    (160, 170, 0),
    (116, 196, 0),
    (76, 208, 32),
    (56, 204, 108),
    (56, 180, 204),
    (60, 60, 60),
    (0, 0, 0),
    (0, 0, 0),
    (236, 238, 236),
    (168, 204, 236),
    (188, 188, 236),
    (212, 178, 236),
    (236, 174, 236),
    (236, 174, 212),
    (236, 180, 176),
    (228, 196, 144),
    (204, 210, 120),
    (180, 222, 120),
    (168, 226, 144),
    (152, 226, 180),
    (160, 214, 228),
    (160, 162, 160),
    (0, 0, 0),
    (0, 0, 0),
];

/// NES PPU (Picture Processing Unit)
pub struct Ppu {
    // PPU Registers
    pub ctrl: u8,     // PPUCTRL ($2000)
    pub mask: u8,     // PPUMASK ($2001)
    pub status: u8,   // PPUSTATUS ($2002)
    pub oam_addr: u8, // OAMADDR ($2003)

    // Internal registers
    pub v: u16,  // Current VRAM address
    pub t: u16,  // Temporary VRAM address
    pub x: u8,   // Fine X scroll
    pub w: bool, // Write toggle

    // Memory
    pub palette_table: [u8; 32], // Palette RAM indexes
    pub name_table: [u8; 2048],  // Name tables
    pub oam_data: [u8; 256],     // Object Attribute Memory (OAM)

    // Rendering
    pub scanline: i16, // Current scanline (-1 to 260)
    pub cycle: u16,    // Current cycle (0 to 340)
    pub frame: u64,    // Current frame count

    // Timing
    pub tv_system: TvSystem, // TV system (NTSC/PAL)
    nmi_occurred: bool,      // NMI flag (separate from status register)
    nmi_output: bool,        // Current NMI output state

    // PPU bus: shared cartridge for pattern table and nametable access
    cartridge: Option<Rc<RefCell<Cartridge>>>,

    // Frame buffer (256x240 pixels, indexed color)
    pub screen: [u8; 256 * 240],
}

impl Ppu {
    /// Create a new PPU instance with the specified TV system
    pub fn new(tv_system: TvSystem) -> Self {
        Ppu {
            ctrl: 0,
            mask: 0,
            status: 0,
            oam_addr: 0,
            v: 0,
            t: 0,
            x: 0,
            w: false,
            palette_table: [0; 32],
            name_table: [0; 2048],
            oam_data: [0; 256],
            scanline: -1,
            cycle: 0,
            frame: 0,
            tv_system,
            nmi_occurred: false,
            nmi_output: false,
            cartridge: None,
            screen: [0; 256 * 240],
        }
    }

    /// Attach a cartridge to the PPU bus
    pub fn load_cartridge(&mut self, cartridge: Rc<RefCell<Cartridge>>) {
        self.cartridge = Some(cartridge);
    }

    /// Reset the PPU to its initial state
    pub fn reset(&mut self) {
        self.ctrl = 0;
        self.mask = 0;
        self.status = 0;
        self.oam_addr = 0;
        self.v = 0;
        self.t = 0;
        self.x = 0;
        self.w = false;
        self.scanline = -1;
        self.cycle = 0;
        self.nmi_occurred = false;
        self.nmi_output = false;
        // tv_system is preserved
    }

    /// Execute one PPU cycle and return true if NMI should be triggered (edge-triggered)
    pub fn step(&mut self) -> bool {
        // Plot random pixel for testing (remove later)
        let x = (self.cycle as usize) % 256;
        let y = (self.scanline as usize) % 240;
        self.screen[y * 256 + x] = (self.cycle % 64) as u8;

        // Store previous NMI output state for edge detection
        let prev_nmi = self.nmi_output;

        // Advance cycle counter
        self.cycle += 1;

        // Each scanline is 341 cycles
        if self.cycle >= 341 {
            self.cycle = 0;
            self.scanline += 1;

            let scanlines_per_frame = self.tv_system.scanlines_per_frame() as i16;
            let vblank_scanline = self.tv_system.vblank_scanline() as i16;

            // VBlank start (scanline 241 for both NTSC and PAL)
            if self.scanline == vblank_scanline {
                // Enter VBlank on cycle 1 of scanline 241
                // For now, we set it at the start of the scanline
            }

            // Wrap to pre-render scanline
            if self.scanline >= scanlines_per_frame {
                self.scanline = -1;
                self.frame += 1;
            }
        }

        // Specific cycle actions
        match (self.scanline, self.cycle) {
            // Set VBlank flag at cycle 1 of scanline 241
            (241, 1) => {
                self.status |= VBLANK;
                self.nmi_occurred = true;
                self.update_nmi_output();
            }
            // Clear VBlank flag and NMI occurred at cycle 1 of pre-render scanline
            (-1, 1) => {
                self.status &= !VBLANK;
                self.status &= !SPRITE_ZERO_HIT;
                self.status &= !SPRITE_OVERFLOW;
                self.nmi_occurred = false;
                self.update_nmi_output();
            }
            _ => {}
        }

        // NMI is edge-triggered: detect rising edge
        let nmi_edge = !prev_nmi && self.nmi_output;
        nmi_edge
    }

    /// Update the NMI output based on NMI occurred flag and NMI enable
    fn update_nmi_output(&mut self) {
        self.nmi_output = self.nmi_occurred && (self.ctrl & ENABLE_NMI != 0);
    }

    /// Peek at a PPU register without side effects
    pub fn peek(&self, addr: u16) -> u8 {
        match addr {
            0x0 => {
                // PPUCTRL
                self.ctrl
            }
            0x1 => {
                // PPUMASK
                self.mask
            }
            0x2 => {
                // PPUSTATUS
                self.status
            }
            0x3 => {
                // OAMADDR
                self.oam_addr
            }
            0x4 => {
                // OAMDATA
                self.oam_data[self.oam_addr as usize]
            }
            0x5 => {
                // PPUSCROLL
                0
            }
            0x6 => {
                // PPUADDR
                0
            }
            0x7 => {
                // PPUDATA
                0
            }
            _ => 0,
        }
    }

    /// Read from a PPU register (from CPU/Main Bus)
    pub fn cpu_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0 => {
                // PPUCTRL
                0
            }
            0x2 => {
                // PPUSTATUS
                let data = self.status;
                self.status &= !VBLANK; // Clear VBLANK on read
                self.nmi_occurred = false; // Clear NMI occurred
                self.update_nmi_output();
                self.w = false; // Reset write toggle
                data
            }
            0x3 => {
                // OAMADDR
                0
            }
            0x4 => {
                // OAMDATA
                self.oam_data[self.oam_addr as usize]
            }
            0x5 => {
                // PPUSCROLL
                0
            }
            0x6 => {
                // PPUADDR
                0
            }
            0x7 => {
                // PPUDATA - read is buffered except for palette RAM
                let addr = self.v & 0x3FFF;

                if addr >= 0x3F00 {
                    // Palette RAM - not buffered
                    self.read_palette(addr)
                } else {
                    // Other reads would need bus access - return 0 for now
                    0
                }
            }
            _ => 0,
        }
    }

    /// Write to a PPU register (from CPU/Main Bus)
    pub fn cpu_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0 => {
                // PPUCTRL
                self.ctrl = data;
                self.update_nmi_output();
            }
            0x1 => {
                // PPUMASK
                self.mask = data;
            },
            0x3 => {
                // OAMADDR
                self.oam_addr = data;
            },
            0x4 => {
                // OAMDATA
                self.oam_data[self.oam_addr as usize] = data;
                self.oam_addr = self.oam_addr.wrapping_add(1);
            }
            0x5 => {
                // PPUSCROLL
                if !self.w {
                    self.x = data & 0x07;
                    self.t = (self.t & 0xFFE0) | ((data as u16) >> 3);
                } else {
                    self.t = (self.t & 0x0C1F) | (((data as u16) & 0x07) << 12);
                    self.t = (self.t & 0xFC1F) | (((data as u16) & 0xF8) << 2);
                }
                self.w = !self.w;
            }
            0x6 => {
                // PPUADDR
                if !self.w {
                    self.t = (self.t & 0x00FF) | (((data as u16) & 0x3F) << 8);
                } else {
                    self.t = (self.t & 0xFF00) | (data as u16);
                    self.v = self.t;
                }
                self.w = !self.w;
            }
            0x7 => {
                // PPUDATA
                let addr = self.v & 0x3FFF;

                // Check if writing to palette RAM ($3F00-$3FFF)
                if addr >= 0x3F00 {
                    self.write_palette(addr, data);
                }
                // Other writes (nametable, etc.) would need bus access - ignored for now

                // Increment address
                let increment = if self.ctrl & INCREMENT_MODE != 0 {
                    32
                } else {
                    1
                };
                self.v = self.v.wrapping_add(increment);
            }
            _ => {}
        }
    }

    /// Internal PPU read: checks the mapper first, then falls through to
    /// internal nametable RAM and palette RAM.
    fn ppu_read(&self, addr: u16) -> u8 {
        let addr = addr & 0x3FFF;

        // Let the mapper intercept first (CHR bank switching, custom nametables, etc.)
        if let Some(ref cart) = self.cartridge {
            let (val, handled) = cart.borrow().ppu_read(addr);
            if handled {
                return val;
            }
        }

        match addr {
            // Pattern tables: handled by mapper above; unreachable if mapper is present
            0x0000..=0x1FFF => 0,
            // Nametables (simple 4KB mirror; mirroring mode applied by mapper above)
            0x2000..=0x3EFF => {
                let mirrored_addr = addr & 0x0FFF;
                self.name_table[mirrored_addr as usize]
            }
            // Palette RAM
            0x3F00..=0x3FFF => self.read_palette(addr),
            _ => 0,
        }
    }

    /// Internal PPU write: checks the mapper first, then falls through to
    /// internal nametable RAM and palette RAM.
    fn ppu_write(&mut self, addr: u16, value: u8) {
        let addr = addr & 0x3FFF;

        // Let the mapper intercept first (CHR RAM writes, custom nametable routing, etc.)
        if let Some(ref cart) = self.cartridge {
            if cart.borrow_mut().ppu_write(addr, value) {
                return;
            }
        }

        match addr {
            // Pattern tables: handled by mapper above; unreachable if mapper is present
            0x0000..=0x1FFF => {}
            // Nametables
            0x2000..=0x3EFF => {
                let mirrored_addr = addr & 0x0FFF;
                self.name_table[mirrored_addr as usize] = value;
            }
            // Palette RAM
            0x3F00..=0x3FFF => self.write_palette(addr, value),
            _ => {}
        }
    }

    /// Decode a single 8x8 tile from CHR memory
    /// Returns a 64-element array (8x8) with palette indices (0-3)
    pub fn decode_tile(&self, tile_index: u16, pattern_table: u16) -> [u8; 64] {
        let mut pixels = [0u8; 64];
        let tile_addr = pattern_table + (tile_index * 16);

        for row in 0..8 {
            let byte1 = self.ppu_read(tile_addr + row);
            let byte2 = self.ppu_read(tile_addr + row + 8);

            for col in 0..8 {
                let bit1 = (byte1 >> (7 - col)) & 1;
                let bit2 = (byte2 >> (7 - col)) & 1;
                let pixel_value = (bit2 << 1) | bit1;

                pixels[(row * 8 + col) as usize] = pixel_value;
            }
        }

        pixels
    }

    /// Get palette entry (0-31)
    pub fn get_palette(&self, index: usize) -> u8 {
        if index < 32 {
            self.palette_table[index]
        } else {
            0
        }
    }

    /// Write to palette RAM with proper mirroring
    fn write_palette(&mut self, addr: u16, value: u8) {
        let addr = addr & 0x1F; // Wrap to 0-31 range

        // Handle mirroring: $3F10, $3F14, $3F18, $3F1C mirror to $3F00, $3F04, $3F08, $3F0C
        let mirrored_addr = if addr >= 0x10 && (addr & 0x03) == 0 {
            addr - 0x10
        } else {
            addr
        } as usize;

        self.palette_table[mirrored_addr] = value & 0x3F; // Only lower 6 bits are used
    }

    /// Read from palette RAM with proper mirroring
    fn read_palette(&self, addr: u16) -> u8 {
        let addr = addr & 0x1F; // Wrap to 0-31 range

        // Handle mirroring: $3F10, $3F14, $3F18, $3F1C mirror to $3F00, $3F04, $3F08, $3F0C
        let mirrored_addr = if addr >= 0x10 && (addr & 0x03) == 0 {
            addr - 0x10
        } else {
            addr
        } as usize;

        self.palette_table[mirrored_addr] & 0x3F // Only lower 6 bits are used
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self::new(TvSystem::Ntsc)
    }
}
