mod flags;
mod system_palette;
mod tv_system;

pub use system_palette::SYSTEM_PALETTE;
pub use tv_system::TvSystem;

use std::cell::RefCell;
use std::rc::Rc;

use super::cartridge::{Cartridge, Mirroring};
use flags::all::*;

type Tile = [u8; 64]; // 8x8 pixels, 4 bits per pixel (2 bitplanes)

const SCREEN_WIDTH: usize = 256;
const SCREEN_HEIGHT: usize = 240;
const SCREEN_SIZE: usize = SCREEN_WIDTH * SCREEN_HEIGHT;

/// NES PPU (2C02)
pub struct Ppu {
    // CPU-exposed registers
    pub control: u8,  // $2000
    pub mask: u8,     // $2001
    pub status: u8,   // $2002
    pub oam_addr: u8, // $2003

    // Scroll / address registers
    pub vram_addr: u16,      // active VRAM address
    pub tram_addr: u16,      // temporary address (updated during writes)
    pub fine_x: u8,          // fine X scroll (3 bits)
    pub address_latch: u8,   // two-write latch: 0 = first write, 1 = second
    pub ppu_data_buffer: u8, // read buffer for $2007

    // Internal RAM
    pub tbl_name: [u8; 2048],  // nametable RAM
    pub tbl_palette: [u8; 32], // palette RAM
    pub oam_data: [u8; 256],   // sprite attribute memory

    // Background tile fetch latches
    pub bg_next_tile_id: u8,
    pub bg_next_tile_attrib: u8,
    pub bg_next_tile_lsb: u8,
    pub bg_next_tile_msb: u8,

    // Background pixel shift registers
    pub bg_shifter_pattern_lo: u16,
    pub bg_shifter_pattern_hi: u16,
    pub bg_shifter_attrib_lo: u16,
    pub bg_shifter_attrib_hi: u16,

    // Timing
    pub scanline: i16,
    pub cycle: u16,
    pub frame: u64,
    pub frame_complete: bool,
    pub tv_system: TvSystem,

    // Set when the PPU requests a CPU NMI
    pub nmi: bool,

    // Output: 256×240 NES palette indices
    pub screen: [u8; SCREEN_SIZE],

    // Reference to the cartridge for memory-mapped access
    cartridge: Option<Rc<RefCell<Cartridge>>>,
}

impl Default for Ppu {
    fn default() -> Self {
        Self::new(TvSystem::Ntsc)
    }
}

impl Ppu {
    pub fn new(tv_system: TvSystem) -> Self {
        Ppu {
            control: 0,
            mask: 0,
            status: 0,
            oam_addr: 0,
            vram_addr: 0,
            tram_addr: 0,
            fine_x: 0,
            address_latch: 0,
            ppu_data_buffer: 0,
            tbl_name: [0; 2048],
            tbl_palette: [0; 32],
            oam_data: [0; 256],
            bg_next_tile_id: 0,
            bg_next_tile_attrib: 0,
            bg_next_tile_lsb: 0,
            bg_next_tile_msb: 0,
            bg_shifter_pattern_lo: 0,
            bg_shifter_pattern_hi: 0,
            bg_shifter_attrib_lo: 0,
            bg_shifter_attrib_hi: 0,
            scanline: -1,
            cycle: 0,
            frame: 0,
            frame_complete: false,
            tv_system,
            nmi: false,
            cartridge: None,
            screen: [0; SCREEN_SIZE],
        }
    }

    pub fn load_cartridge(&mut self, cartridge: Rc<RefCell<Cartridge>>) {
        self.cartridge = Some(cartridge);
    }

    /// Reset all registers and rendering state.
    pub fn reset(&mut self) {
        self.fine_x = 0x00;
        self.address_latch = 0x00;
        self.ppu_data_buffer = 0x00;
        self.scanline = -1;
        self.cycle = 0;
        self.bg_next_tile_id = 0x00;
        self.bg_next_tile_attrib = 0x00;
        self.bg_next_tile_lsb = 0x00;
        self.bg_next_tile_msb = 0x00;
        self.bg_shifter_pattern_lo = 0x0000;
        self.bg_shifter_pattern_hi = 0x0000;
        self.bg_shifter_attrib_lo = 0x0000;
        self.bg_shifter_attrib_hi = 0x0000;
        self.status = 0x00;
        self.mask = 0x00;
        self.control = 0x00;
        self.vram_addr = 0x0000;
        self.tram_addr = 0x0000;
        self.frame_complete = false;
        self.nmi = false;
    }

    /// Read a PPU register from the CPU bus. Most registers have side-effects.
    pub fn cpu_read(&mut self, addr: u16) -> u8 {
        let mut data: u8 = 0x00;

        match addr {
            0x0000 => {} // control - write-only
            0x0001 => {} // mask    - write-only

            // Reading status returns the top 3 bits plus low-5 bus noise,
            // clears the vertical-blank flag, and resets the address latch.
            0x0002 => {
                data = (self.status & 0xE0) | (self.ppu_data_buffer & 0x1F);
                self.status &= !VERTICAL_BLANK;
                self.address_latch = 0;
            }

            0x0003 => {} // OAM address - write-only

            0x0004 => {
                data = self.oam_data[self.oam_addr as usize];
            }

            0x0005 => {} // scroll  - write-only
            0x0006 => {} // address - write-only

            // Data reads are buffered by one cycle; palette reads bypass the buffer.
            // The address auto-increments after every read.
            0x0007 => {
                data = self.ppu_data_buffer;
                self.ppu_data_buffer = self.ppu_read(self.vram_addr);
                if self.vram_addr >= 0x3F00 {
                    data = self.ppu_data_buffer;
                }
                self.vram_addr =
                    self.vram_addr
                        .wrapping_add(if self.control & INCREMENT_MODE != 0 {
                            32
                        } else {
                            1
                        });
            }

            _ => {}
        }

        data
    }

    /// Write a PPU register from the CPU bus.
    pub fn cpu_write(&mut self, addr: u16, data: u8) {
        match addr {
            // Store control and mirror nametable select into tram.
            0x0000 => {
                self.control = data;
                self.tram_addr = (self.tram_addr & 0xF3FF) | (((data as u16) & 0x03) << 10);
            }

            0x0001 => {
                self.mask = data;
            }

            0x0002 => {} // status - read-only

            0x0003 => {
                self.oam_addr = data;
            }

            0x0004 => {
                self.oam_data[self.oam_addr as usize] = data;
            }

            // Two writes: first latches X scroll, second latches Y scroll.
            0x0005 => {
                if self.address_latch == 0 {
                    self.fine_x = data & 0x07;
                    self.tram_addr = (self.tram_addr & 0xFFE0) | ((data as u16) >> 3);
                    self.address_latch = 1;
                } else {
                    self.tram_addr = (self.tram_addr & 0x8FFF) | (((data as u16) & 0x07) << 12);
                    self.tram_addr = (self.tram_addr & 0xFC1F) | (((data as u16) & 0xF8) << 2);
                    self.address_latch = 0;
                }
            }

            // Two writes: first high byte, second low byte; then copy to vram_addr.
            0x0006 => {
                if self.address_latch == 0 {
                    self.tram_addr = (((data as u16) & 0x3F) << 8) | (self.tram_addr & 0x00FF);
                    self.address_latch = 1;
                } else {
                    self.tram_addr = (self.tram_addr & 0xFF00) | (data as u16);
                    self.vram_addr = self.tram_addr;
                    self.address_latch = 0;
                }
            }

            // Write to PPU bus, then auto-increment address.
            0x0007 => {
                self.ppu_write(self.vram_addr, data);
                self.vram_addr =
                    self.vram_addr
                        .wrapping_add(if self.control & INCREMENT_MODE != 0 {
                            32
                        } else {
                            1
                        });
            }

            _ => {}
        }
    }

    /// Read a register without any side-effects (for debugging).
    pub fn peek(&self, addr: u16) -> u8 {
        match addr {
            0x0000 => self.control,
            0x0001 => self.mask,
            0x0002 => self.status,
            0x0003 => self.oam_addr,
            0x0004 => self.oam_data[self.oam_addr as usize],
            _ => 0,
        }
    }

    /// Read from the PPU address bus. The mapper intercepts first;
    /// unhandled addresses fall through to internal nametable and palette RAM.
    fn ppu_read(&self, addr: u16) -> u8 {
        let addr = addr & 0x3FFF;

        if let Some(ref cart) = self.cartridge {
            let (val, handled) = cart.borrow().ppu_read(addr);
            if handled {
                return val;
            }
        }

        match addr {
            // CHR ROM/RAM is handled entirely by the mapper
            0x0000..=0x1FFF => 0,

            // Nametable RAM with mirroring
            0x2000..=0x3EFF => self.tbl_name[self.mirror_nametable_addr(addr)],

            // Palette RAM with sprite-background mirroring and optional grayscale mask
            0x3F00..=0x3FFF => {
                let mut a = addr & 0x001F;
                a = match a {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    _ => a,
                };
                let mask = if self.mask & GRAYSCALE != 0 {
                    0x30
                } else {
                    0x3F
                };
                self.tbl_palette[a as usize] & mask
            }

            _ => 0,
        }
    }

    /// Write to the PPU address bus. The mapper intercepts first;
    /// unhandled addresses fall through to internal nametable and palette RAM.
    fn ppu_write(&mut self, addr: u16, data: u8) {
        let addr = addr & 0x3FFF;

        if let Some(ref cart) = self.cartridge {
            if cart.borrow_mut().ppu_write(addr, data) {
                return;
            }
        }

        match addr {
            0x0000..=0x1FFF => {} // CHR ROM/RAM - mapper only

            // Nametable RAM with mirroring
            0x2000..=0x3EFF => {
                self.tbl_name[self.mirror_nametable_addr(addr)] = data;
            }

            // Palette RAM with sprite-background mirroring
            0x3F00..=0x3FFF => {
                let mut a = addr & 0x001F;
                a = match a {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    _ => a,
                };
                self.tbl_palette[a as usize] = data;
            }

            _ => {}
        }
    }

    /// Advance the PPU by one clock cycle. Returns `true` on a rising NMI edge.
    pub fn step(&mut self) -> bool {
        let prev_nmi = self.nmi;

        if self.scanline >= -1 && self.scanline < 240 {
            // Skip cycle 0 on scanline 0 (odd-frame timing quirk)
            if self.scanline == 0 && self.cycle == 0 {
                self.cycle = 1;
            }

            // Pre-render scanline: clear all status flags for the coming frame
            if self.scanline == -1 && self.cycle == 1 {
                self.status &= !VERTICAL_BLANK;
                self.status &= !SPRITE_ZERO_HIT;
                self.status &= !SPRITE_OVERFLOW;
                self.nmi = false;
            }

            // Active fetch window: shift registers advance every cycle;
            // every 8 cycles fetch the next tile id, attribute, and bit-planes.
            if (self.cycle >= 2 && self.cycle < 258) || (self.cycle >= 321 && self.cycle < 338) {
                self.update_shifters();

                match (self.cycle - 1) % 8 {
                    0 => {
                        self.load_background_shifters();
                        self.bg_next_tile_id = self.ppu_read(0x2000 | (self.vram_addr & 0x0FFF));
                    }
                    2 => {
                        let coarse_x = self.vram_addr & 0x001F;
                        let coarse_y = (self.vram_addr >> 5) & 0x001F;
                        let nametable_x = (self.vram_addr >> 10) & 0x0001;
                        let nametable_y = (self.vram_addr >> 11) & 0x0001;
                        self.bg_next_tile_attrib = self.ppu_read(
                            0x23C0
                                | (nametable_y << 11)
                                | (nametable_x << 10)
                                | ((coarse_y >> 2) << 3)
                                | (coarse_x >> 2),
                        );
                        if coarse_y & 0x02 != 0 {
                            self.bg_next_tile_attrib >>= 4;
                        }
                        if coarse_x & 0x02 != 0 {
                            self.bg_next_tile_attrib >>= 2;
                        }
                        self.bg_next_tile_attrib &= 0x03;
                    }
                    4 => {
                        let fine_y = (self.vram_addr >> 12) & 0x0007;
                        self.bg_next_tile_lsb = self.ppu_read(
                            (((self.control & PATTERN_BACKGROUND) as u16) << 8)
                                + ((self.bg_next_tile_id as u16) << 4)
                                + fine_y,
                        );
                    }
                    6 => {
                        let fine_y = (self.vram_addr >> 12) & 0x0007;
                        self.bg_next_tile_msb = self.ppu_read(
                            (((self.control & PATTERN_BACKGROUND) as u16) << 8)
                                + ((self.bg_next_tile_id as u16) << 4)
                                + fine_y
                                + 8,
                        );
                    }
                    7 => self.increment_scroll_x(),
                    _ => {}
                }
            }

            if self.cycle == 256 {
                self.increment_scroll_y();
            }

            if self.cycle == 257 {
                self.load_background_shifters();
                self.transfer_address_x();
            }

            // Superfluous nametable fetches at the end of the scanline
            if self.cycle == 338 || self.cycle == 340 {
                self.bg_next_tile_id = self.ppu_read(0x2000 | (self.vram_addr & 0x0FFF));
            }

            // Restore vertical scroll bits during the pre-render scanline
            if self.scanline == -1 && self.cycle >= 280 && self.cycle < 305 {
                self.transfer_address_y();
            }
        }

        // Enter vertical blank: set flag and optionally fire NMI
        if self.scanline == 241 && self.cycle == 1 {
            self.status |= VERTICAL_BLANK;
            if self.control & ENABLE_NMI != 0 {
                self.nmi = true;
            }
        }

        // Compose the background pixel for this cycle from the shift registers
        let mut bg_pixel: u8 = 0x00;
        let mut bg_palette: u8 = 0x00;

        if self.mask & RENDER_BACKGROUND != 0 {
            let bit_mux: u16 = 0x8000 >> self.fine_x;

            let p0_pixel = ((self.bg_shifter_pattern_lo & bit_mux) > 0) as u8;
            let p1_pixel = ((self.bg_shifter_pattern_hi & bit_mux) > 0) as u8;
            bg_pixel = (p1_pixel << 1) | p0_pixel;

            let bg_pal0 = ((self.bg_shifter_attrib_lo & bit_mux) > 0) as u8;
            let bg_pal1 = ((self.bg_shifter_attrib_hi & bit_mux) > 0) as u8;
            bg_palette = (bg_pal1 << 1) | bg_pal0;
        }

        if self.scanline >= 0 && self.scanline < 240 && self.cycle >= 1 && self.cycle <= 256 {
            let x = (self.cycle - 1) as usize;
            let y = self.scanline as usize;
            let color_index =
                self.ppu_read(0x3F00 + ((bg_palette as u16) << 2) + bg_pixel as u16) & 0x3F;
            self.screen[y * 256 + x] = color_index;
        }

        self.cycle += 1;
        if self.cycle >= 341 {
            self.cycle = 0;
            self.scanline += 1;
            let scanlines_per_frame = self.tv_system.scanlines_per_frame() as i16;
            if self.scanline >= scanlines_per_frame - 1 {
                self.scanline = -1;
                self.frame += 1;
                self.frame_complete = true;
            }
        }

        !prev_nmi && self.nmi
    }

    /// Advance the horizontal tile position, wrapping across nametable boundaries.
    fn increment_scroll_x(&mut self) {
        if self.mask & (RENDER_BACKGROUND | RENDER_SPRITES) != 0 {
            let coarse_x = self.vram_addr & 0x001F;
            if coarse_x == 31 {
                self.vram_addr &= !0x001F;
                self.vram_addr ^= 0x0400; // flip nametable_x
            } else {
                self.vram_addr += 1;
            }
        }
    }

    /// Advance the vertical position by one scanline, wrapping across nametable boundaries.
    fn increment_scroll_y(&mut self) {
        if self.mask & (RENDER_BACKGROUND | RENDER_SPRITES) != 0 {
            let fine_y = (self.vram_addr >> 12) & 0x07;
            if fine_y < 7 {
                self.vram_addr += 0x1000;
            } else {
                self.vram_addr &= !0x7000; // reset fine_y
                let coarse_y = (self.vram_addr >> 5) & 0x001F;
                match coarse_y {
                    29 => {
                        self.vram_addr &= !0x03E0;
                        self.vram_addr ^= 0x0800; // flip nametable_y
                    }
                    31 => {
                        self.vram_addr &= !0x03E0; // wrap within current nametable
                    }
                    _ => {
                        self.vram_addr += 0x0020;
                    }
                }
            }
        }
    }

    /// Copy horizontal scroll bits (coarse_x, nametable_x) from tram to vram.
    fn transfer_address_x(&mut self) {
        if self.mask & (RENDER_BACKGROUND | RENDER_SPRITES) != 0 {
            self.vram_addr = (self.vram_addr & !0x041F) | (self.tram_addr & 0x041F);
        }
    }

    /// Copy vertical scroll bits (fine_y, nametable_y, coarse_y) from tram to vram.
    fn transfer_address_y(&mut self) {
        if self.mask & (RENDER_BACKGROUND | RENDER_SPRITES) != 0 {
            self.vram_addr = (self.vram_addr & !0x7BE0) | (self.tram_addr & 0x7BE0);
        }
    }

    /// Load the next fetched tile data into the low bytes of the shift registers.
    fn load_background_shifters(&mut self) {
        self.bg_shifter_pattern_lo =
            (self.bg_shifter_pattern_lo & 0xFF00) | self.bg_next_tile_lsb as u16;
        self.bg_shifter_pattern_hi =
            (self.bg_shifter_pattern_hi & 0xFF00) | self.bg_next_tile_msb as u16;
        self.bg_shifter_attrib_lo = (self.bg_shifter_attrib_lo & 0xFF00)
            | (if self.bg_next_tile_attrib & 0b01 != 0 {
                0xFF
            } else {
                0x00
            });
        self.bg_shifter_attrib_hi = (self.bg_shifter_attrib_hi & 0xFF00)
            | (if self.bg_next_tile_attrib & 0b10 != 0 {
                0xFF
            } else {
                0x00
            });
    }

    /// Convert a PPU nametable address ($2000-$3EFF) to a physical index into
    /// `tbl_name` (0-2047) using the cartridge's mirroring mode.
    ///
    /// * **Vertical**   - NT0/NT2 share the low 1 KB; NT1/NT3 share the high 1 KB.
    /// * **Horizontal** - NT0/NT1 share the low 1 KB; NT2/NT3 share the high 1 KB.
    fn mirror_nametable_addr(&self, addr: u16) -> usize {
        // $3000-$3EFF mirrors $2000-$2EFF; strip the extra $1000.
        let addr = if addr >= 0x3000 { addr - 0x1000 } else { addr };

        let mirroring = self
            .cartridge
            .as_ref()
            .map(|c| c.borrow().mirroring)
            .unwrap_or(Mirroring::Vertical);

        match mirroring {
            // Vertical mirroring: bit 11 selects the physical KB.
            Mirroring::Vertical => (addr & 0x07FF) as usize,
            // Horizontal mirroring: bit 11 (halved) selects the physical KB.
            Mirroring::Horizontal => (((addr & 0x0800) >> 1) | (addr & 0x03FF)) as usize,
            // Four-screen would need 4 KB; clamp to 2 KB as a safe fallback.
            Mirroring::FourScreen => (addr & 0x07FF) as usize,
        }
    }

    /// Shift all background shift registers left by one bit.
    fn update_shifters(&mut self) {
        if self.mask & RENDER_BACKGROUND != 0 {
            self.bg_shifter_pattern_lo <<= 1;
            self.bg_shifter_pattern_hi <<= 1;
            self.bg_shifter_attrib_lo <<= 1;
            self.bg_shifter_attrib_hi <<= 1;
        }
    }

    /// Decode all 256 tiles from the given pattern table (0 or 1) into 8×8 pixel
    /// arrays of palette indices. Pixels are read from the two CHR bit-planes
    /// LSB-first, which produces the correct left-to-right column order.
    pub fn get_pattern_table(&self, index: u8) -> Vec<Tile> {
        let mut tiles: Vec<Tile> = Vec::with_capacity(256);

        for tile_y in 0u16..16 {
            for tile_x in 0u16..16 {
                let offset = tile_y * 256 + tile_x * 16;
                let mut tile: Tile = [0u8; 64];

                for row in 0u16..8 {
                    let mut tile_lsb = self.ppu_read((index as u16) * 0x1000 + offset + row);
                    let mut tile_msb =
                        self.ppu_read((index as u16) * 0x1000 + offset + row + 0x0008);

                    for col in 0u16..8 {
                        let pixel = (tile_lsb & 0x01) + (tile_msb & 0x01);
                        tile_lsb >>= 1;
                        tile_msb >>= 1;
                        tile[row as usize * 8 + (7 - col) as usize] = pixel;
                    }
                }

                tiles.push(tile);
            }
        }

        tiles
    }

    /// Look up the RGB colour for a palette index and 2-bit pixel value.
    pub fn get_color_from_palette_ram(&self, palette: u8, pixel: u8) -> (u8, u8, u8) {
        let color_index = self.ppu_read(0x3F00 + ((palette as u16) << 2) + pixel as u16) & 0x3F;
        SYSTEM_PALETTE[color_index as usize % SYSTEM_PALETTE.len()]
    }
}
