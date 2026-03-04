#[derive(Clone, Copy)]
pub struct DmcChannel {
    enabled: bool,
    irq_enabled: bool,
    loop_flag: bool,
    rate_index: u8,
    timer_counter: u16,

    sample_address_reg: u8,
    sample_length_reg: u8,

    current_address: u16,
    pub bytes_remaining: u16,

    shift_register: u8,
    bits_remaining: u8,
    silence: bool,

    pub output_level: u8,
    pub interrupt_flag: bool,
}

impl Default for DmcChannel {
    fn default() -> Self {
        Self {
            enabled: false,
            irq_enabled: false,
            loop_flag: false,
            rate_index: 0,
            timer_counter: 0,
            sample_address_reg: 0,
            sample_length_reg: 0,
            current_address: 0xC000,
            bytes_remaining: 0,
            shift_register: 0,
            bits_remaining: 8,
            silence: true,
            output_level: 0,
            interrupt_flag: false,
        }
    }
}

impl DmcChannel {
    pub fn write_4010(&mut self, value: u8) {
        self.irq_enabled = value & 0x80 != 0;
        self.loop_flag = value & 0x40 != 0;
        self.rate_index = value & 0x0F;
        if !self.irq_enabled {
            self.interrupt_flag = false;
        }
    }

    pub fn write_4011(&mut self, value: u8) {
        self.output_level = value & 0x7F;
    }

    pub fn write_4012(&mut self, value: u8) {
        self.sample_address_reg = value;
    }

    pub fn write_4013(&mut self, value: u8) {
        self.sample_length_reg = value;
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.bytes_remaining = 0;
        } else if self.bytes_remaining == 0 {
            self.restart_sample();
        }
    }

    fn restart_sample(&mut self) {
        self.current_address = 0xC000 | ((self.sample_address_reg as u16) << 6);
        self.bytes_remaining = ((self.sample_length_reg as u16) << 4) | 0x0001;
    }

    pub fn clock_timer(&mut self, rate_table: &[u16; 16]) {
        if !self.enabled {
            return;
        }

        if self.timer_counter == 0 {
            self.timer_counter = rate_table[self.rate_index as usize];
            self.clock_output_unit();
        } else {
            self.timer_counter -= 1;
        }
    }

    fn clock_output_unit(&mut self) {
        if !self.silence {
            if self.shift_register & 0x01 != 0 {
                if self.output_level <= 125 {
                    self.output_level += 2;
                }
            } else if self.output_level >= 2 {
                self.output_level -= 2;
            }
        }

        self.shift_register >>= 1;
        self.bits_remaining = self.bits_remaining.saturating_sub(1);

        if self.bits_remaining == 0 {
            self.bits_remaining = 8;
            self.reload_sample_buffer_stub();
        }
    }

    fn reload_sample_buffer_stub(&mut self) {
        if self.bytes_remaining > 0 {
            self.bytes_remaining -= 1;

            self.silence = true;
            self.shift_register = 0;

            self.current_address = if self.current_address == 0xFFFF {
                0x8000
            } else {
                self.current_address + 1
            };

            if self.bytes_remaining == 0 {
                if self.loop_flag {
                    self.restart_sample();
                } else if self.irq_enabled {
                    self.interrupt_flag = true;
                }
            }
        } else {
            self.silence = true;
        }
    }
}
