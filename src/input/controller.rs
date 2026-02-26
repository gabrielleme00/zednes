/// NES Controller button mapping
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Button {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
}

/// Controller state
pub struct Controller {
    // Bitmask for button states (1 = pressed, 0 = released)
    state: u8,
}

impl Controller {
    pub fn new() -> Self {
        Self { state: 0 }
    }

    /// Press a button
    pub fn press(&mut self, button: Button) {
        self.state |= button.to_mask();
    }

    /// Release a button
    pub fn release(&mut self, button: Button) {
        self.state &= !button.to_mask();
    }

    /// Check if a button is pressed
    pub fn is_pressed(&self, button: Button) -> bool {
        (self.state & button.to_mask()) != 0
    }

    /// Get the current state byte (for NES controller register)
    pub fn get_state(&self) -> u8 {
        self.state
    }

    /// Reset all buttons
    pub fn reset(&mut self) {
        self.state = 0;
    }
}

impl Button {
    fn to_mask(self) -> u8 {
        match self {
            Button::A => 0b0000_0001,
            Button::B => 0b0000_0010,
            Button::Select => 0b0000_0100,
            Button::Start => 0b0000_1000,
            Button::Up => 0b0001_0000,
            Button::Down => 0b0010_0000,
            Button::Left => 0b0100_0000,
            Button::Right => 0b1000_0000,
        }
    }
}

impl Default for Controller {
    fn default() -> Self {
        Self::new()
    }
}
