use super::tables::{DUTY_TABLE, LENGTH_TABLE, TRIANGLE_TABLE};

#[derive(Clone, Copy, Default)]
struct Envelope {
    loop_flag: bool,
    constant_volume: bool,
    period_or_volume: u8,
    start_flag: bool,
    divider: u8,
    decay_level: u8,
}

impl Envelope {
    fn write_control(&mut self, value: u8) {
        self.loop_flag = value & 0x20 != 0;
        self.constant_volume = value & 0x10 != 0;
        self.period_or_volume = value & 0x0F;
    }

    fn restart(&mut self) {
        self.start_flag = true;
    }

    fn clock(&mut self) {
        if self.start_flag {
            self.start_flag = false;
            self.decay_level = 15;
            self.divider = self.period_or_volume;
            return;
        }

        if self.divider == 0 {
            self.divider = self.period_or_volume;
            if self.decay_level == 0 {
                if self.loop_flag {
                    self.decay_level = 15;
                }
            } else {
                self.decay_level -= 1;
            }
        } else {
            self.divider -= 1;
        }
    }

    fn output(&self) -> u8 {
        if self.constant_volume {
            self.period_or_volume
        } else {
            self.decay_level
        }
    }
}

#[derive(Clone, Copy)]
pub struct PulseChannel {
    enabled: bool,
    duty: u8,
    sequence_step: u8,
    timer_period: u16,
    timer_counter: u16,
    length_counter: u8,
    length_halt: bool,
    envelope: Envelope,

    sweep_enabled: bool,
    sweep_period: u8,
    sweep_negate: bool,
    sweep_shift: u8,
    sweep_reload: bool,
    sweep_divider: u8,
    negate_uses_ones_complement: bool,
}

impl Default for PulseChannel {
    fn default() -> Self {
        Self {
            enabled: false,
            duty: 0,
            sequence_step: 0,
            timer_period: 0,
            timer_counter: 0,
            length_counter: 0,
            length_halt: false,
            envelope: Envelope::default(),
            sweep_enabled: false,
            sweep_period: 1,
            sweep_negate: false,
            sweep_shift: 0,
            sweep_reload: false,
            sweep_divider: 0,
            negate_uses_ones_complement: false,
        }
    }
}

impl PulseChannel {
    pub fn pulse1() -> Self {
        Self {
            negate_uses_ones_complement: true,
            ..Default::default()
        }
    }

    pub fn pulse2() -> Self {
        Self::default()
    }

    pub fn write_control(&mut self, value: u8) {
        self.duty = (value >> 6) & 0x03;
        self.length_halt = value & 0x20 != 0;
        self.envelope.write_control(value);
    }

    pub fn write_sweep(&mut self, value: u8) {
        self.sweep_enabled = value & 0x80 != 0;
        self.sweep_period = ((value >> 4) & 0x07) + 1;
        self.sweep_negate = value & 0x08 != 0;
        self.sweep_shift = value & 0x07;
        self.sweep_reload = true;
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0xFF00) | value as u16;
    }

    pub fn write_timer_high_and_length(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0x00FF) | (((value & 0x07) as u16) << 8);
        self.length_counter = LENGTH_TABLE[(value >> 3) as usize & 0x1F];
        self.sequence_step = 0;
        self.envelope.restart();
    }

    pub fn clock_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = self.timer_period;
            self.sequence_step = (self.sequence_step + 1) & 0x07;
        } else {
            self.timer_counter -= 1;
        }
    }

    pub fn clock_quarter_frame(&mut self) {
        self.envelope.clock();
    }

    pub fn clock_half_frame(&mut self) {
        self.clock_sweep();

        if !self.length_halt && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn clock_sweep(&mut self) {
        let target_period = self.sweep_target_period();
        let sweep_mute = self.timer_period < 8 || target_period > 0x07FF;

        if self.sweep_divider == 0 {
            if self.sweep_enabled && self.sweep_shift != 0 && !sweep_mute {
                self.timer_period = target_period;
            }
            self.sweep_divider = self.sweep_period;
        } else {
            self.sweep_divider -= 1;
        }

        if self.sweep_reload {
            self.sweep_reload = false;
            self.sweep_divider = self.sweep_period;
        }
    }

    fn sweep_target_period(&self) -> u16 {
        let change = self.timer_period >> self.sweep_shift;
        if self.sweep_negate {
            let extra = if self.negate_uses_ones_complement { 1 } else { 0 };
            self.timer_period.wrapping_sub(change).wrapping_sub(extra)
        } else {
            self.timer_period.wrapping_add(change)
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    pub fn length_counter_active(&self) -> bool {
        self.length_counter > 0
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 {
            return 0;
        }

        let target_period = self.sweep_target_period();
        let sweep_mute = self.timer_period < 8 || target_period > 0x07FF;
        if sweep_mute {
            return 0;
        }

        let duty_value = DUTY_TABLE[self.duty as usize][self.sequence_step as usize];
        if duty_value == 0 {
            0
        } else {
            self.envelope.output()
        }
    }
}

#[derive(Clone, Copy)]
pub struct TriangleChannel {
    enabled: bool,
    timer_period: u16,
    timer_counter: u16,
    sequence_step: u8,
    length_counter: u8,
    length_halt: bool,
    linear_counter_reload_value: u8,
    linear_counter: u8,
    linear_reload_flag: bool,
    linear_control_flag: bool,
}

impl Default for TriangleChannel {
    fn default() -> Self {
        Self {
            enabled: false,
            timer_period: 0,
            timer_counter: 0,
            sequence_step: 0,
            length_counter: 0,
            length_halt: false,
            linear_counter_reload_value: 0,
            linear_counter: 0,
            linear_reload_flag: false,
            linear_control_flag: false,
        }
    }
}

impl TriangleChannel {
    pub fn write_linear_counter(&mut self, value: u8) {
        self.length_halt = value & 0x80 != 0;
        self.linear_control_flag = value & 0x80 != 0;
        self.linear_counter_reload_value = value & 0x7F;
    }

    pub fn write_timer_low(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0xFF00) | value as u16;
    }

    pub fn write_timer_high_and_length(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0x00FF) | (((value & 0x07) as u16) << 8);
        self.length_counter = LENGTH_TABLE[(value >> 3) as usize & 0x1F];
        self.linear_reload_flag = true;
    }

    pub fn clock_timer(&mut self) {
        if self.timer_counter == 0 {
            self.timer_counter = self.timer_period;
            if self.length_counter > 0 && self.linear_counter > 0 {
                self.sequence_step = (self.sequence_step + 1) & 0x1F;
            }
        } else {
            self.timer_counter -= 1;
        }
    }

    pub fn clock_quarter_frame(&mut self) {
        if self.linear_reload_flag {
            self.linear_counter = self.linear_counter_reload_value;
        } else if self.linear_counter > 0 {
            self.linear_counter -= 1;
        }

        if !self.linear_control_flag {
            self.linear_reload_flag = false;
        }
    }

    pub fn clock_half_frame(&mut self) {
        if !self.length_halt && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    pub fn length_counter_active(&self) -> bool {
        self.length_counter > 0
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 || self.linear_counter == 0 {
            return 0;
        }
        if self.timer_period < 2 {
            return 0;
        }
        TRIANGLE_TABLE[self.sequence_step as usize]
    }
}

#[derive(Clone, Copy)]
pub struct NoiseChannel {
    enabled: bool,
    mode: bool,
    period_index: u8,
    timer_counter: u16,
    length_counter: u8,
    length_halt: bool,
    shift_register: u16,
    envelope: Envelope,
}

impl Default for NoiseChannel {
    fn default() -> Self {
        Self {
            enabled: false,
            mode: false,
            period_index: 0,
            timer_counter: 0,
            length_counter: 0,
            length_halt: false,
            shift_register: 1,
            envelope: Envelope::default(),
        }
    }
}

impl NoiseChannel {
    pub fn write_control(&mut self, value: u8) {
        self.length_halt = value & 0x20 != 0;
        self.envelope.write_control(value);
    }

    pub fn write_period(&mut self, value: u8) {
        self.mode = value & 0x80 != 0;
        self.period_index = value & 0x0F;
    }

    pub fn write_length_and_restart(&mut self, value: u8) {
        self.length_counter = LENGTH_TABLE[(value >> 3) as usize & 0x1F];
        self.envelope.restart();
    }

    pub fn clock_timer(&mut self, period_table: &[u16; 16]) {
        if self.timer_counter == 0 {
            self.timer_counter = period_table[self.period_index as usize];

            let bit0 = self.shift_register & 1;
            let tap_bit = if self.mode {
                (self.shift_register >> 6) & 1
            } else {
                (self.shift_register >> 1) & 1
            };
            let feedback = bit0 ^ tap_bit;

            self.shift_register >>= 1;
            self.shift_register |= feedback << 14;
        } else {
            self.timer_counter -= 1;
        }
    }

    pub fn clock_quarter_frame(&mut self) {
        self.envelope.clock();
    }

    pub fn clock_half_frame(&mut self) {
        if !self.length_halt && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.length_counter = 0;
        }
    }

    pub fn length_counter_active(&self) -> bool {
        self.length_counter > 0
    }

    pub fn output(&self) -> u8 {
        if !self.enabled || self.length_counter == 0 {
            return 0;
        }
        if self.shift_register & 1 != 0 {
            return 0;
        }
        self.envelope.output()
    }
}
