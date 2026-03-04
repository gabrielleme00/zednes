#[derive(Clone, Copy)]
pub struct FrameCounter {
    mode_five_step: bool,
    irq_inhibit: bool,
    pub frame_irq_flag: bool,
    sequence_cycle: u32,
}

impl Default for FrameCounter {
    fn default() -> Self {
        Self {
            mode_five_step: false,
            irq_inhibit: false,
            frame_irq_flag: false,
            sequence_cycle: 0,
        }
    }
}

#[derive(Clone, Copy)]
pub struct FrameStepEvents {
    pub quarter: bool,
    pub half: bool,
    pub frame_irq: bool,
}

impl Default for FrameStepEvents {
    fn default() -> Self {
        Self {
            quarter: false,
            half: false,
            frame_irq: false,
        }
    }
}

impl FrameCounter {
    pub fn write_4017(&mut self, value: u8) -> FrameStepEvents {
        self.mode_five_step = value & 0x80 != 0;
        self.irq_inhibit = value & 0x40 != 0;
        if self.irq_inhibit {
            self.frame_irq_flag = false;
        }
        self.sequence_cycle = 0;

        if self.mode_five_step {
            FrameStepEvents {
                quarter: true,
                half: true,
                frame_irq: false,
            }
        } else {
            FrameStepEvents::default()
        }
    }

    pub fn clock(&mut self) -> FrameStepEvents {
        self.sequence_cycle = self.sequence_cycle.wrapping_add(1);
        let mut events = FrameStepEvents::default();

        if self.mode_five_step {
            match self.sequence_cycle {
                3729 | 11186 => events.quarter = true,
                7457 | 18641 => {
                    events.quarter = true;
                    events.half = true;
                }
                _ => {}
            }

            if self.sequence_cycle >= 18641 {
                self.sequence_cycle = 0;
            }
        } else {
            match self.sequence_cycle {
                3729 | 11186 => events.quarter = true,
                7457 => {
                    events.quarter = true;
                    events.half = true;
                }
                14915 => {
                    events.quarter = true;
                    events.half = true;
                    if !self.irq_inhibit {
                        events.frame_irq = true;
                    }
                }
                _ => {}
            }

            if self.sequence_cycle >= 14915 {
                self.sequence_cycle = 0;
            }
        }

        if events.frame_irq {
            self.frame_irq_flag = true;
        }

        events
    }
}
