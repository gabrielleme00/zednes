use super::flags::oam_flags::*;

/// Represents a single entry in the Object Attribute Memory (OAM) for sprites.
#[derive(Copy, Clone)]
pub struct ObjectAttributeEntry {
    pub y: u8,         // Y position of the top of the sprite
    pub id: u8,        // Tile index number (0-255)
    pub attribute: u8, // Bitfield: V-flip, H-flip, priority, palette
    pub x: u8,         // X position of the left of the sprite
}

impl ObjectAttributeEntry {
    /// Creates a new OAM entry with default values (all zeros).
    pub fn new() -> Self {
        Self {
            y: 0,
            id: 0,
            attribute: 0,
            x: 0,
        }
    }

    /// Reads a byte from the OAM entry based on the index (0-3).
    pub fn read_u8(&self, index: usize) -> u8 {
        match index {
            0 => self.y,
            1 => self.id,
            2 => self.attribute,
            3 => self.x,
            _ => panic!("Invalid OAM index: {}", index),
        }
    }

    /// Writes a byte to the OAM entry based on the index (0-3).
    pub fn write_u8(&mut self, index: usize, value: u8) {
        match index {
            0 => self.y = value,
            1 => self.id = value,
            2 => self.attribute = value,
            3 => self.x = value,
            _ => panic!("Invalid OAM index: {}", index),
        }
    }

    /// Checks the palette bits in the attribute byte and returns the palette index (0-3).
    pub fn get_palette(&self) -> u8 {
        self.attribute & PALETTE
    }

    /// Checks the priority bit in the attribute byte.
    /// Returns 0 if the sprite is in front of the background, or 1 if it's behind.
    pub fn get_priority(&self) -> u8 {
        (self.attribute & PRIORITY) >> 5
    }

    /// Checks if the horizontal flip bit is set in the attribute byte.
    pub fn is_hflip(&self) -> bool {
        self.attribute & HORIZONTAL_FLIP != 0
    }

    /// Checks if the vertical flip bit is set in the attribute byte.
    pub fn is_vflip(&self) -> bool {
        self.attribute & VERTICAL_FLIP != 0
    }
}
