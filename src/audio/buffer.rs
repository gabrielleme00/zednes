use crossbeam_queue::ArrayQueue;
use std::sync::Arc;

#[derive(Clone)]
pub struct SampleProducer {
    queue: Arc<ArrayQueue<f32>>,
}

#[derive(Clone)]
pub struct SampleConsumer {
    queue: Arc<ArrayQueue<f32>>,
}

/// Create a new sample queue with the specified capacity and return the producer and consumer handles
pub fn create_sample_queue(capacity: usize) -> (SampleProducer, SampleConsumer) {
    let capacity = capacity.max(1024);
    let queue = Arc::new(ArrayQueue::new(capacity));
    (
        SampleProducer {
            queue: Arc::clone(&queue),
        },
        SampleConsumer { queue },
    )
}

impl SampleProducer {
    /// Push a slice of audio samples into the queue.
    /// 
    /// If the queue is full, the oldest samples will be dropped to make room for the new ones.
    pub fn push_samples(&self, samples: &[f32]) -> u64 {
        let mut dropped = 0_u64;

        for &sample in samples {
            if self.queue.push(sample).is_err() {
                let _ = self.queue.pop();
                dropped += 1;
                let _ = self.queue.push(sample);
            }
        }

        dropped
    }

    /// Get the number of samples currently in the queue.
    pub fn len(&self) -> usize {
        self.queue.len()
    }

    /// Remove all pending samples from the queue and return how many were removed.
    pub fn clear(&self) -> usize {
        let mut removed = 0usize;
        while self.queue.pop().is_some() {
            removed += 1;
        }
        removed
    }
}

impl SampleConsumer {
    /// Pop a single audio sample from the queue. Returns `None` if the queue is empty.
    pub fn pop_sample(&self) -> Option<f32> {
        self.queue.pop()
    }
}
