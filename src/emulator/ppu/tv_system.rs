/// TV system timing configuration
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TvSystem {
    Ntsc,
    Pal,
}

impl TvSystem {
    /// Get the number of scanlines per frame
    pub fn scanlines_per_frame(self) -> u16 {
        match self {
            TvSystem::Ntsc => 261,
            TvSystem::Pal => 312,
        }
    }

    /// Get the number of visible scanlines
    pub fn visible_scanlines(self) -> u16 {
        match self {
            TvSystem::Ntsc => 240,
            TvSystem::Pal => 240,
        }
    }

    /// Get the vblank start scanline
    pub fn vblank_scanline(self) -> u16 {
        match self {
            TvSystem::Ntsc => 241,
            TvSystem::Pal => 241,
        }
    }

    /// Get the pre-render scanline
    pub fn prerender_scanline(self) -> i16 {
        -1
    }

    /// Get the number of PPU cycles per CPU cycle
    /// NTSC: 3.0, PAL: 3.2
    pub fn ppu_cycles_per_cpu(self) -> (u32, u32) {
        match self {
            TvSystem::Ntsc => (3, 1),  // 3 PPU cycles per 1 CPU cycle
            TvSystem::Pal => (16, 5),  // 16 PPU cycles per 5 CPU cycles (3.2 ratio)
        }
    }
}