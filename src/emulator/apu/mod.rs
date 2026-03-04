mod channels;
mod dmc;
mod filters;
mod frame_counter;
mod tables;

use super::TvSystem;
use std::collections::VecDeque;

use channels::{NoiseChannel, PulseChannel, TriangleChannel};
use dmc::DmcChannel;
use filters::{OnePoleHighPass, OnePoleLowPass};
use frame_counter::{FrameCounter, FrameStepEvents};
use tables::{
    DMC_RATE_TABLE_NTSC, DMC_RATE_TABLE_PAL, NOISE_PERIOD_TABLE_NTSC, NOISE_PERIOD_TABLE_PAL,
    NTSC_CPU_HZ, PAL_CPU_HZ,
};

/// APU (Audio Processing Unit) module for the NES emulator.
pub struct Apu {
    registers: [u8; 0x18],
    sample_queue: VecDeque<f32>,
    output_sample_rate: f64,
    cpu_clock_hz: f64,
    cycles_until_next_sample: f64,
    cpu_cycle_parity: bool,

    pulse1: PulseChannel,
    pulse2: PulseChannel,
    triangle: TriangleChannel,
    noise: NoiseChannel,
    frame_counter: FrameCounter,
    dmc: DmcChannel,

    noise_period_table: [u16; 16],
    dmc_rate_table: [u16; 16],

    hp_90: OnePoleHighPass,
    hp_440: OnePoleHighPass,
    lp_14k: OnePoleLowPass,

    mute_pulse1: bool,
    mute_pulse2: bool,
    mute_triangle: bool,
    mute_noise: bool,
    mute_dmc: bool,
}

impl Apu {
    pub fn new_with_tv_system(tv_system: TvSystem) -> Self {
        let (cpu_clock_hz, noise_period_table, dmc_rate_table) = match tv_system {
            TvSystem::Ntsc => (NTSC_CPU_HZ, NOISE_PERIOD_TABLE_NTSC, DMC_RATE_TABLE_NTSC),
            TvSystem::Pal => (PAL_CPU_HZ, NOISE_PERIOD_TABLE_PAL, DMC_RATE_TABLE_PAL),
        };

        let output_sample_rate = 44_100.0;

        Apu {
            registers: [0; 0x18],
            sample_queue: VecDeque::with_capacity(8192),
            output_sample_rate,
            cpu_clock_hz,
            cycles_until_next_sample: 0.0,
            cpu_cycle_parity: false,

            pulse1: PulseChannel::pulse1(),
            pulse2: PulseChannel::pulse2(),
            triangle: TriangleChannel::default(),
            noise: NoiseChannel::default(),
            frame_counter: FrameCounter::default(),
            dmc: DmcChannel::default(),

            noise_period_table,
            dmc_rate_table,

            hp_90: OnePoleHighPass::new(90.0, output_sample_rate as f32),
            hp_440: OnePoleHighPass::new(440.0, output_sample_rate as f32),
            lp_14k: OnePoleLowPass::new(14_000.0, output_sample_rate as f32),

            mute_pulse1: false,
            mute_pulse2: false,
            mute_triangle: false,
            mute_noise: false,
            mute_dmc: false,
        }
    }

    pub fn reset(&mut self) {
        self.registers = [0; 0x18];
        self.sample_queue.clear();
        self.cycles_until_next_sample = 0.0;
        self.cpu_cycle_parity = false;

        self.pulse1 = PulseChannel::pulse1();
        self.pulse2 = PulseChannel::pulse2();
        self.triangle = TriangleChannel::default();
        self.noise = NoiseChannel::default();
        self.frame_counter = FrameCounter::default();
        self.dmc = DmcChannel::default();

        self.hp_90.reset_state();
        self.hp_440.reset_state();
        self.lp_14k.reset_state();
    }

    pub fn cpu_write(&mut self, addr: u16, value: u8) {
        if !(0x4000..=0x4017).contains(&addr) {
            return;
        }

        self.registers[(addr - 0x4000) as usize] = value;

        match addr {
            0x4000 => self.pulse1.write_control(value),
            0x4001 => self.pulse1.write_sweep(value),
            0x4002 => self.pulse1.write_timer_low(value),
            0x4003 => self.pulse1.write_timer_high_and_length(value),

            0x4004 => self.pulse2.write_control(value),
            0x4005 => self.pulse2.write_sweep(value),
            0x4006 => self.pulse2.write_timer_low(value),
            0x4007 => self.pulse2.write_timer_high_and_length(value),

            0x4008 => self.triangle.write_linear_counter(value),
            0x400A => self.triangle.write_timer_low(value),
            0x400B => self.triangle.write_timer_high_and_length(value),

            0x400C => self.noise.write_control(value),
            0x400E => self.noise.write_period(value),
            0x400F => self.noise.write_length_and_restart(value),

            0x4010 => self.dmc.write_4010(value),
            0x4011 => self.dmc.write_4011(value),
            0x4012 => self.dmc.write_4012(value),
            0x4013 => self.dmc.write_4013(value),

            0x4015 => {
                self.pulse1.set_enabled(value & 0x01 != 0);
                self.pulse2.set_enabled(value & 0x02 != 0);
                self.triangle.set_enabled(value & 0x04 != 0);
                self.noise.set_enabled(value & 0x08 != 0);
                self.dmc.set_enabled(value & 0x10 != 0);
                if value & 0x10 == 0 {
                    self.dmc.interrupt_flag = false;
                }
            }

            0x4017 => {
                let immediate_events = self.frame_counter.write_4017(value);
                self.apply_frame_events(immediate_events);
            }

            _ => {}
        }
    }

    pub fn cpu_read(&self, addr: u16) -> u8 {
        if addr == 0x4015 {
            let mut status = 0u8;
            if self.pulse1.length_counter_active() {
                status |= 0x01;
            }
            if self.pulse2.length_counter_active() {
                status |= 0x02;
            }
            if self.triangle.length_counter_active() {
                status |= 0x04;
            }
            if self.noise.length_counter_active() {
                status |= 0x08;
            }
            if self.dmc.bytes_remaining > 0 {
                status |= 0x10;
            }
            if self.frame_counter.frame_irq_flag {
                status |= 0x40;
            }
            if self.dmc.interrupt_flag {
                status |= 0x80;
            }
            return status;
        }

        if (0x4000..=0x4017).contains(&addr) {
            return self.registers[(addr - 0x4000) as usize];
        }

        0
    }

    pub fn set_output_sample_rate(&mut self, sample_rate: u32) {
        self.output_sample_rate = sample_rate.max(8_000) as f64;
        let sr = self.output_sample_rate as f32;
        self.hp_90.reconfigure(sr);
        self.hp_440.reconfigure(sr);
        self.lp_14k.reconfigure(sr);
    }

    pub fn step(&mut self, cpu_cycles: u32) {
        if cpu_cycles == 0 {
            return;
        }

        let cpu_cycles_per_sample = self.cpu_clock_hz / self.output_sample_rate;

        for _ in 0..cpu_cycles {
            self.step_cpu_cycle();
            self.cycles_until_next_sample += 1.0;

            while self.cycles_until_next_sample >= cpu_cycles_per_sample {
                self.cycles_until_next_sample -= cpu_cycles_per_sample;
                let sample = self.mix_output_sample();
                self.sample_queue.push_back(sample);
            }
        }
    }

    pub fn drain_samples(&mut self, out: &mut Vec<f32>) {
        out.extend(self.sample_queue.drain(..));
    }

    pub fn set_pulse1_muted(&mut self, muted: bool) {
        self.mute_pulse1 = muted;
    }

    pub fn is_pulse1_muted(&self) -> bool {
        self.mute_pulse1
    }

    pub fn set_pulse2_muted(&mut self, muted: bool) {
        self.mute_pulse2 = muted;
    }

    pub fn is_pulse2_muted(&self) -> bool {
        self.mute_pulse2
    }

    pub fn set_triangle_muted(&mut self, muted: bool) {
        self.mute_triangle = muted;
    }

    pub fn is_triangle_muted(&self) -> bool {
        self.mute_triangle
    }

    pub fn set_noise_muted(&mut self, muted: bool) {
        self.mute_noise = muted;
    }

    pub fn is_noise_muted(&self) -> bool {
        self.mute_noise
    }

    pub fn set_dmc_muted(&mut self, muted: bool) {
        self.mute_dmc = muted;
    }

    pub fn is_dmc_muted(&self) -> bool {
        self.mute_dmc
    }

    fn step_cpu_cycle(&mut self) {
        self.triangle.clock_timer();

        // APU frame counter and pulse/noise timers clock on APU cycles (CPU/2).
        self.cpu_cycle_parity = !self.cpu_cycle_parity;
        if self.cpu_cycle_parity {
            let events = self.frame_counter.clock();
            self.apply_frame_events(events);
            self.pulse1.clock_timer();
            self.pulse2.clock_timer();
            self.noise.clock_timer(&self.noise_period_table);
        }

        self.dmc.clock_timer(&self.dmc_rate_table);
    }

    fn apply_frame_events(&mut self, events: FrameStepEvents) {
        if events.quarter {
            self.pulse1.clock_quarter_frame();
            self.pulse2.clock_quarter_frame();
            self.triangle.clock_quarter_frame();
            self.noise.clock_quarter_frame();
        }

        if events.half {
            self.pulse1.clock_half_frame();
            self.pulse2.clock_half_frame();
            self.triangle.clock_half_frame();
            self.noise.clock_half_frame();
        }
    }

    fn mix_output_sample(&mut self) -> f32 {
        let p1 = if self.mute_pulse1 {
            0.0
        } else {
            self.pulse1.output() as f64
        };
        let p2 = if self.mute_pulse2 {
            0.0
        } else {
            self.pulse2.output() as f64
        };
        let t = if self.mute_triangle {
            0.0
        } else {
            self.triangle.output() as f64
        };
        let n = if self.mute_noise {
            0.0
        } else {
            self.noise.output() as f64
        };
        let d = if self.mute_dmc {
            0.0
        } else {
            self.dmc.output_level as f64
        };

        let pulse_sum = p1 + p2;
        let pulse_out = if pulse_sum > 0.0 {
            95.88 / ((8128.0 / pulse_sum) + 100.0)
        } else {
            0.0
        };

        let tnd_sum = (t / 8227.0) + (n / 12241.0) + (d / 22638.0);
        let tnd_out = if tnd_sum > 0.0 {
            159.79 / ((1.0 / tnd_sum) + 100.0)
        } else {
            0.0
        };

        let mut sample = (pulse_out + tnd_out) as f32;
        sample = self.hp_90.process(sample);
        sample = self.hp_440.process(sample);
        sample = self.lp_14k.process(sample);
        (sample * 0.8).clamp(-1.0, 1.0)
    }
}
