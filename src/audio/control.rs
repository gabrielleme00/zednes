use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};

#[derive(Debug, Clone, Copy)]
pub struct AudioControlsSnapshot {
    pub volume: f32,
    pub muted: bool,
    pub underflows: u64,
    pub overflows: u64,
}

/// Thread-safe audio controls that can be shared between the emulator and the audio output thread
#[derive(Default)]
pub struct AudioControls {
    volume_bits: AtomicU32,
    muted: AtomicBool,
    underflows: AtomicU64,
    overflows: AtomicU64,
}

impl AudioControls {
    /// Create a new AudioControls instance with default settings (volume=1.0, muted=false, no underflows/overflows)
    pub fn new() -> Self {
        Self {
            volume_bits: AtomicU32::new(1.0f32.to_bits()),
            muted: AtomicBool::new(false),
            underflows: AtomicU64::new(0),
            overflows: AtomicU64::new(0),
        }
    }

    /// Get the current audio output volume as a multiplier (1.0 = normal volume, 0.5 = half volume, etc.)
    pub fn volume(&self) -> f32 {
        f32::from_bits(self.volume_bits.load(Ordering::Relaxed))
    }

    /// Set the audio output volume.
    /// 
    /// The volume is a multiplier applied to the audio samples, where 1.0 is normal volume,
    /// 0.5 is half volume, and 2.0 is double volume.
    /// 
    /// Values outside the range [0.0, 2.0] will be clamped.
    pub fn set_volume(&self, volume: f32) {
        self.volume_bits
            .store(volume.clamp(0.0, 2.0).to_bits(), Ordering::Relaxed);
    }

    /// Get whether the audio output is currently muted.
    pub fn muted(&self) -> bool {
        self.muted.load(Ordering::Relaxed)
    }

    /// Set whether the audio output should be muted.
    /// 
    /// When muted, the audio output thread will output silence regardless of the volume setting.
    pub fn set_muted(&self, muted: bool) {
        self.muted.store(muted, Ordering::Relaxed);
    }

    /// Increment the underflow counter by a specified amount.
    /// 
    /// This can be used to track how many times the audio output thread ran out of samples to play, which can help with diagnostics and performance tuning.
    pub fn add_underflows(&self, count: u64) {
        self.underflows.fetch_add(count, Ordering::Relaxed);
    }

    /// Increment the overflow counter by a specified amount.
    /// 
    /// This can be used to track how many audio samples were dropped due to buffer overflows,
    /// which can help with diagnostics and performance tuning.
    pub fn add_overflows(&self, count: u64) {
        self.overflows.fetch_add(count, Ordering::Relaxed);
    }

    /// Reset underflow/overflow counters.
    pub fn reset_diagnostics(&self) {
        self.underflows.store(0, Ordering::Relaxed);
        self.overflows.store(0, Ordering::Relaxed);
    }

    /// Get a snapshot of the current audio control settings and statistics for diagnostics or display purposes
    pub fn snapshot(&self) -> AudioControlsSnapshot {
        AudioControlsSnapshot {
            volume: self.volume(),
            muted: self.muted(),
            underflows: self.underflows.load(Ordering::Relaxed),
            overflows: self.overflows.load(Ordering::Relaxed),
        }
    }
}

pub type SharedAudioControls = Arc<AudioControls>;
