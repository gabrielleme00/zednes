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

    /// Clear the frame buffer to black
    pub fn clear(&mut self) {
        self.data.fill(0);
    }

    /// Set a pixel at (x, y) with RGB color
    pub fn set_pixel(&mut self, x: usize, y: usize, r: u8, g: u8, b: u8) {
        if x < self.width && y < self.height {
            let offset = (y * self.width + x) * 3;
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

impl Default for FrameBuffer {
    fn default() -> Self {
        Self::new()
    }
}
