pub mod ppuctrl_flags {
    pub const INCREMENT_MODE: u8 = 0b0000_0100;
    pub const PATTERN_SPRITE: u8 = 0b0000_1000;
    pub const PATTERN_BACKGROUND: u8 = 0b0001_0000;
    pub const SPRITE_SIZE: u8 = 0b0010_0000;
    pub const ENABLE_NMI: u8 = 0b1000_0000;
}

pub mod ppumask_flags {
    pub const GRAYSCALE: u8 = 0b0000_0001;
    pub const SHOW_BACKGROUND_LEFT: u8 = 0b0000_0010;
    pub const SHOW_SPRITES_LEFT: u8 = 0b0000_0100;
    pub const RENDER_BACKGROUND: u8 = 0b0000_1000;
    pub const RENDER_SPRITES: u8 = 0b0001_0000;
}

pub mod ppustatus_flags {
    pub const SPRITE_OVERFLOW: u8 = 0b0010_0000;
    pub const SPRITE_ZERO_HIT: u8 = 0b0100_0000;
    pub const VERTICAL_BLANK: u8 = 0b1000_0000;
}

pub mod oam_flags {
    pub const PALETTE: u8 = 0b0000_0011;
    pub const PRIORITY: u8 = 0b0010_0000;
    pub const HORIZONTAL_FLIP: u8 = 0b0100_0000;
    pub const VERTICAL_FLIP: u8 = 0b1000_0000;
}

pub mod all {
    pub use super::ppuctrl_flags::*;
    pub use super::ppumask_flags::*;
    pub use super::ppustatus_flags::*;
}