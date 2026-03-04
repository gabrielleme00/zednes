use super::buffer::{SampleConsumer, SampleProducer, create_sample_queue};
use super::control::{AudioControls, SharedAudioControls};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use std::sync::Arc;

/// Audio backend module that manages the audio output stream, sample buffering, and shared controls
pub struct AudioBackend {
    _stream: cpal::Stream,
    producer: SampleProducer,
    controls: SharedAudioControls,
    sample_rate: u32,
    channels: u16,
    buffer_capacity: usize,
}

impl AudioBackend {
    /// Create a new AudioBackend instance with the specified target latency in milliseconds.
    /// 
    /// This will initialize the audio output stream and set up the sample queue and controls.
    pub fn new(target_latency_ms: u32) -> Result<Self, String> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .ok_or_else(|| "No default output audio device found".to_string())?;

        let supported = device
            .default_output_config()
            .map_err(|e| format!("Failed to query default output config: {e}"))?;

        let sample_format = supported.sample_format();
        let config: cpal::StreamConfig = supported.into();
        let sample_rate = config.sample_rate;
        let channels = config.channels;

        let frames_per_latency =
            ((sample_rate as usize * target_latency_ms.max(10) as usize) / 1000).max(1024);
        let buffer_capacity = frames_per_latency * 4;

        let (producer, consumer) = create_sample_queue(buffer_capacity);
        let controls: SharedAudioControls = Arc::new(AudioControls::new());

        let stream = match sample_format {
            cpal::SampleFormat::F32 => {
                Self::build_output_stream::<f32>(&device, &config, consumer, Arc::clone(&controls))?
            }
            cpal::SampleFormat::I16 => {
                Self::build_output_stream::<i16>(&device, &config, consumer, Arc::clone(&controls))?
            }
            cpal::SampleFormat::U16 => {
                Self::build_output_stream::<u16>(&device, &config, consumer, Arc::clone(&controls))?
            }
            other => {
                return Err(format!("Unsupported output sample format: {other:?}"));
            }
        };

        stream
            .play()
            .map_err(|e| format!("Failed to start output audio stream: {e}"))?;

        Ok(Self {
            _stream: stream,
            producer,
            controls,
            sample_rate,
            channels,
            buffer_capacity,
        })
    }
    
    pub fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    pub fn channels(&self) -> u16 {
        self.channels
    }

    pub fn buffer_capacity(&self) -> usize {
        self.buffer_capacity
    }

    pub fn buffer_len(&self) -> usize {
        self.producer.len()
    }

    pub fn controls(&self) -> SharedAudioControls {
        Arc::clone(&self.controls)
    }

    /// Push a slice of audio samples into the backend's sample queue.
    /// 
    /// If the queue is full, the oldest samples will be dropped to make room for the new ones.
    /// Any dropped samples will be counted as overflows in the audio controls.
    pub fn push_samples(&self, samples: &[f32]) {
        let dropped = self.producer.push_samples(samples);
        if dropped > 0 {
            self.controls.add_overflows(dropped);
        }
    }

    /// Clear pending samples from the producer queue.
    pub fn clear_buffer(&self) {
        let _ = self.producer.clear();
    }

    /// Reset diagnostic counters (underflows/overflows).
    pub fn reset_diagnostics(&self) {
        self.controls.reset_diagnostics();
    }

    /// Build the output audio stream for the specified sample type, using the provided consumer and controls.
    fn build_output_stream<T>(
        device: &cpal::Device,
        config: &cpal::StreamConfig,
        consumer: SampleConsumer,
        controls: SharedAudioControls,
    ) -> Result<cpal::Stream, String>
    where
        T: cpal::Sample + cpal::FromSample<f32> + cpal::SizedSample,
    {
        let channels = config.channels as usize;
        let err_fn = |err| eprintln!("Audio stream error: {err}");

        device
            .build_output_stream(
                config,
                move |data: &mut [T], _| {
                    Self::write_interleaved_output(data, channels, &consumer, &controls)
                },
                err_fn,
                None,
            )
            .map_err(|e| format!("Failed to build output audio stream: {e}"))
    }

    /// Write interleaved audio samples to the output buffer, applying volume and mute controls.
    /// 
    /// This function is called by the audio output stream callback and handles underflows by
    /// outputting silence when the sample queue is empty.
    fn write_interleaved_output<T>(
        output: &mut [T],
        channels: usize,
        consumer: &SampleConsumer,
        controls: &SharedAudioControls,
    ) where
        T: cpal::Sample + cpal::FromSample<f32>,
    {
        let muted = controls.muted();
        let volume = controls.volume();
        let mut underflows = 0_u64;

        for frame in output.chunks_exact_mut(channels) {
            let mut sample = match consumer.pop_sample() {
                Some(value) => value,
                None => {
                    underflows += 1;
                    0.0
                }
            };

            if muted {
                sample = 0.0;
            } else {
                sample *= volume;
            }

            let converted = T::from_sample(sample);
            for channel_out in frame {
                *channel_out = converted;
            }
        }

        if underflows > 0 {
            controls.add_underflows(underflows);
        }
    }
}
