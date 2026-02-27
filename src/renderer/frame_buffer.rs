use crate::emulator::ppu::SYSTEM_PALETTE;

/// Frame buffer for rendering NES display
/// NES native resolution: 256x240 pixels
pub struct FrameBuffer {
    /// RGB buffer (256 * 240 * 3 bytes)
    pub data: Vec<u8>,
    pub width: usize,
    pub height: usize,
}

impl FrameBuffer {
    pub const NES_WIDTH: usize = 256;
    pub const NES_HEIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            data: vec![0; Self::NES_WIDTH * Self::NES_HEIGHT * 3],
            width: Self::NES_WIDTH,
            height: Self::NES_HEIGHT,
        }
    }

    /// Update the buffer in-place from a PPU screen buffer (palette indices).
    pub fn update_from_ppu_screen(&mut self, screen: &[u8]) {
        for (i, &idx) in screen.iter().enumerate() {
            let (r, g, b) = SYSTEM_PALETTE[idx as usize % 64];
            let offset = i * 3;
            self.data[offset] = r;
            self.data[offset + 1] = g;
            self.data[offset + 2] = b;
        }
    }

    /// Get the raw buffer data
    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }
}
