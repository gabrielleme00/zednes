use crate::audio::AudioBackend;
use crate::emulator::{Nes, TvSystem};
use std::time::Instant;

const NTSC_FPS: f64 = 60.0988;
const PAL_FPS: f64 = 50.0070;
const MAX_FRAMES_PER_UPDATE_TICK: usize = 8;
const AUDIO_TARGET_BUFFER_MS: usize = 50;
const AUDIO_LOW_WATERMARK_MS: usize = 30;

/// Manages the emulator state and lifecycle
pub struct EmulatorState {
    pub nes: Nes,
    pub running: bool,
    pub rom_loaded: bool,
    pub error_message: Option<String>,
    pub audio: Option<AudioBackend>,

    audio_samples_buffer: Vec<f32>,
    timing_anchor: Option<Instant>,
    frame_accumulator: f64,
}

impl EmulatorState {
    pub fn new() -> Self {
        let mut nes = Nes::new();

        let audio = match AudioBackend::new(40) {
            Ok(audio) => {
                nes.set_audio_sample_rate(audio.sample_rate());
                Some(audio)
            }
            Err(err) => {
                eprintln!("Audio disabled: {err}");
                None
            }
        };

        Self {
            nes,
            running: false,
            rom_loaded: false,
            error_message: None,
            audio,
            audio_samples_buffer: Vec::new(),
            timing_anchor: None,
            frame_accumulator: 0.0,
        }
    }

    /// Load a ROM from bytes
    pub fn load_rom(&mut self, rom_data: &[u8], start_paused: bool) -> Result<(), String> {
        match self.nes.load_rom(rom_data) {
            Ok(_) => {
                self.rom_loaded = true;
                self.running = !start_paused;
                self.error_message = None;
                self.reset_timing_state();
                self.prepare_audio_for_transition(true);
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
            self.reset_timing_state();
            self.prepare_audio_for_transition(false);
        }
    }

    /// Toggle pause/resume
    pub fn toggle_pause(&mut self) {
        if self.rom_loaded && !self.nes.cpu.halted {
            self.running = !self.running;
            if self.running {
                self.reset_timing_state();
            } else {
                self.prepare_audio_for_transition(false);
            }
        }
    }

    /// Execute emulation according to real elapsed wall-clock time and buffer feedback.
    pub fn step_frame(&mut self) {
        if self.running && self.rom_loaded {
            let now = Instant::now();
            if self.timing_anchor.is_none() {
                self.timing_anchor = Some(now);
            }

            let elapsed = now
                .saturating_duration_since(self.timing_anchor.unwrap())
                .as_secs_f64()
                .min(0.25);
            self.timing_anchor = Some(now);

            self.frame_accumulator += elapsed * self.target_fps();

            let base_frames = self.frame_accumulator.floor() as usize;
            self.frame_accumulator -= base_frames as f64;

            let extra_frames = self.audio_catchup_frames();
            let mut frames_to_step = base_frames
                .saturating_add(extra_frames)
                .clamp(1, MAX_FRAMES_PER_UPDATE_TICK);

            if !self.audio_needs_more_samples() && base_frames == 0 {
                frames_to_step = 1;
            }

            for _ in 0..frames_to_step {
                self.nes.step_frame();
                self.flush_apu_samples_to_audio_backend();

                if self.nes.cpu.halted {
                    self.running = false;
                    self.error_message = Some(format!(
                        "CPU halted: BRK instruction at PC=${:04X}",
                        self.nes.cpu.pc
                    ));
                    self.reset_timing_state();
                    break;
                }
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
            self.reset_timing_state();
            return;
        }
        self.nes.step_frame();
        self.flush_apu_samples_to_audio_backend();
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
            self.flush_apu_samples_to_audio_backend();
        }
    }

    pub fn set_apu_pulse1_muted(&mut self, muted: bool) {
        self.nes.set_apu_pulse1_muted(muted);
    }

    pub fn is_apu_pulse1_muted(&self) -> bool {
        self.nes.is_apu_pulse1_muted()
    }

    pub fn set_apu_pulse2_muted(&mut self, muted: bool) {
        self.nes.set_apu_pulse2_muted(muted);
    }

    pub fn is_apu_pulse2_muted(&self) -> bool {
        self.nes.is_apu_pulse2_muted()
    }

    pub fn set_apu_triangle_muted(&mut self, muted: bool) {
        self.nes.set_apu_triangle_muted(muted);
    }

    pub fn is_apu_triangle_muted(&self) -> bool {
        self.nes.is_apu_triangle_muted()
    }

    pub fn set_apu_noise_muted(&mut self, muted: bool) {
        self.nes.set_apu_noise_muted(muted);
    }

    pub fn is_apu_noise_muted(&self) -> bool {
        self.nes.is_apu_noise_muted()
    }

    pub fn set_apu_dmc_muted(&mut self, muted: bool) {
        self.nes.set_apu_dmc_muted(muted);
    }

    pub fn is_apu_dmc_muted(&self) -> bool {
        self.nes.is_apu_dmc_muted()
    }

    /// Drain generated APU samples from the emulator and push them to the audio backend.
    pub fn flush_apu_samples_to_audio_backend(&mut self) {
        let Some(audio) = &self.audio else {
            return;
        };

        self.audio_samples_buffer.clear();
        self.nes.drain_audio_samples(&mut self.audio_samples_buffer);
        if !self.audio_samples_buffer.is_empty() {
            audio.push_samples(&self.audio_samples_buffer);
        }
    }

    fn audio_needs_more_samples(&self) -> bool {
        let Some(audio) = &self.audio else {
            return false;
        };

        let low_watermark_samples =
            (audio.sample_rate() as usize * AUDIO_LOW_WATERMARK_MS / 1000).max(1024);
        audio.buffer_len() < low_watermark_samples
    }

    pub fn target_fps(&self) -> f64 {
        match self.nes.bus.ppu.tv_system {
            TvSystem::Ntsc => NTSC_FPS,
            TvSystem::Pal => PAL_FPS,
        }
    }

    fn audio_catchup_frames(&self) -> usize {
        let Some(audio) = &self.audio else {
            return 0;
        };

        let target_samples = (audio.sample_rate() as usize * AUDIO_TARGET_BUFFER_MS / 1000).max(1024);
        let current = audio.buffer_len();
        if current >= target_samples {
            return 0;
        }

        let samples_per_frame = (audio.sample_rate() as f64 / self.target_fps()).max(1.0);
        let deficit = target_samples - current;
        ((deficit as f64 / samples_per_frame).ceil() as usize).min(4)
    }

    fn reset_timing_state(&mut self) {
        self.timing_anchor = None;
        self.frame_accumulator = 0.0;
    }

    fn prepare_audio_for_transition(&self, reset_diagnostics: bool) {
        let Some(audio) = &self.audio else {
            return;
        };

        audio.clear_buffer();
        if reset_diagnostics {
            audio.reset_diagnostics();
        }
    }
}

impl Default for EmulatorState {
    fn default() -> Self {
        Self::new()
    }
}
