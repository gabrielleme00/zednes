use crate::emulator::Nes;

/// Manages the emulator state and lifecycle
pub struct EmulatorState {
    pub nes: Nes,
    pub running: bool,
    pub rom_loaded: bool,
    pub error_message: Option<String>,
}

impl EmulatorState {
    pub fn new() -> Self {
        Self {
            nes: Nes::new(),
            running: false,
            rom_loaded: false,
            error_message: None,
        }
    }

    /// Load a ROM from bytes
    pub fn load_rom(&mut self, rom_data: &[u8]) -> Result<(), String> {
        match self.nes.load_rom(rom_data) {
            Ok(_) => {
                self.rom_loaded = true;
                self.error_message = None;
                Ok(())
            }
            Err(e) => {
                self.error_message = Some(e.clone());
                Err(e)
            }
        }
    }

    /// Reset the emulator
    pub fn reset(&mut self) {
        if self.rom_loaded {
            self.nes.reset();
            self.running = false;
            self.error_message = None;
        }
    }

    /// Toggle pause/resume
    pub fn toggle_pause(&mut self) {
        if self.rom_loaded && !self.nes.cpu.halted {
            self.running = !self.running;
        }
    }

    /// Execute one frame if running
    pub fn step_frame(&mut self) {
        if self.running && self.rom_loaded {
            self.nes.step_frame();
            if self.nes.cpu.halted {
                self.running = false;
                self.error_message = Some(format!(
                    "CPU halted: BRK instruction at PC=${:04X}",
                    self.nes.cpu.pc
                ));
            }
        }
    }

    /// Advance exactly one frame.
    /// If already running, only pauses - the main loop already advanced a frame this tick.
    /// If paused, steps one frame then stays paused.
    pub fn step_frame_once(&mut self) {
        if !self.rom_loaded || self.nes.cpu.halted {
            return;
        }
        if self.running {
            // A frame was already executed by the main loop this update tick; just pause.
            self.running = false;
            return;
        }
        self.nes.step_frame();
        if self.nes.cpu.halted {
            self.error_message = Some(format!(
                "CPU halted: BRK instruction at PC=${:04X}",
                self.nes.cpu.pc
            ));
        }
    }

    /// Execute one instruction
    pub fn step_instruction(&mut self) {
        if self.rom_loaded && !self.nes.cpu.halted {
            self.nes.step();
        }
    }
}

impl Default for EmulatorState {
    fn default() -> Self {
        Self::new()
    }
}
