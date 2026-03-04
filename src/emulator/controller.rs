/// NES Controller button mapping.
/// Bit positions match the NES serial read order:
/// A=0, B=1, Select=2, Start=3, Up=4, Down=5, Left=6, Right=7
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

/// NES Standard Controller with proper shift-register emulation.
///
/// The NES reads controller state serially:
/// - Write 1 to $4016 (strobe high): shift register continuously reloads from button state.
/// - Write 0 to $4016 (strobe low): latches current button state into the shift register.
/// - Each read of $4016 returns bit 0 of the shift register and shifts right.
///   Read order: A, B, Select, Start, Up, Down, Left, Right, then all 1s.
pub struct Controller {
    /// Current live button states (bit mask).
    button_state: u8,
    /// Shift register: loaded on strobe falling-edge, shifted on each read.
    shift_register: u8,
    /// True while the strobe signal is held high.
    strobe: bool,
}

impl Controller {
    pub fn new() -> Self {
        Self {
            button_state: 0,
            shift_register: 0,
            strobe: false,
        }
    }

    /// Press a button.
    pub fn press(&mut self, button: Button) {
        self.button_state |= button.to_mask();
    }

    /// Release a button.
    pub fn release(&mut self, button: Button) {
        self.button_state &= !button.to_mask();
    }

    /// Check if a button is currently pressed.
    pub fn is_pressed(&self, button: Button) -> bool {
        (self.button_state & button.to_mask()) != 0
    }

    /// Set the strobe signal (CPU writes bit 0 of $4016).
    /// Rising edge: keep reloading. Falling edge: latch button state.
    pub fn set_strobe(&mut self, high: bool) {
        if high {
            self.strobe = true;
            // While strobe is held high the shift register mirrors live state.
            self.shift_register = self.button_state;
        } else {
            if self.strobe {
                // Falling edge: latch the current button state.
                self.shift_register = self.button_state;
            }
            self.strobe = false;
        }
    }

    /// Peek at the current serial output bit without advancing the shift register.
    /// Used for side-effect-free reads (e.g. debugger).
    pub fn peek_bit(&self) -> u8 {
        if self.strobe {
            return self.button_state & 0x01;
        }
        self.shift_register & 0x01
    }

    /// Read one bit from the serial shift register (CPU reads $4016/$4017).
    ///
    /// When strobe is high, always returns the state of button A (bit 0).
    /// When strobe is low, returns bits A→B→Select→Start→Up→Down→Left→Right
    /// in order, then returns 1 for all subsequent reads.
    pub fn read_serial(&mut self) -> u8 {
        if self.strobe {
            return self.button_state & 0x01;
        }
        let bit = self.shift_register & 0x01;
        // After the 8th bit the bus returns open-bus (1).
        self.shift_register = (self.shift_register >> 1) | 0x80;
        bit
    }

    /// Reset all buttons and clear the shift register.
    pub fn reset(&mut self) {
        self.button_state = 0;
        self.shift_register = 0;
        self.strobe = false;
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
