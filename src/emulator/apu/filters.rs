#[derive(Clone, Copy)]
pub struct OnePoleHighPass {
    cutoff_hz: f32,
    alpha: f32,
    prev_input: f32,
    prev_output: f32,
}

impl OnePoleHighPass {
    pub fn new(cutoff_hz: f32, sample_rate: f32) -> Self {
        let mut out = Self {
            cutoff_hz,
            alpha: 0.0,
            prev_input: 0.0,
            prev_output: 0.0,
        };
        out.reconfigure(sample_rate);
        out
    }

    pub fn reconfigure(&mut self, sample_rate: f32) {
        let dt = 1.0 / sample_rate.max(1.0);
        let rc = 1.0 / (2.0 * std::f32::consts::PI * self.cutoff_hz.max(1.0));
        self.alpha = rc / (rc + dt);
    }

    pub fn reset_state(&mut self) {
        self.prev_input = 0.0;
        self.prev_output = 0.0;
    }

    pub fn process(&mut self, input: f32) -> f32 {
        let output = self.alpha * (self.prev_output + input - self.prev_input);
        self.prev_input = input;
        self.prev_output = output;
        output
    }
}

#[derive(Clone, Copy)]
pub struct OnePoleLowPass {
    cutoff_hz: f32,
    alpha: f32,
    prev_output: f32,
}

impl OnePoleLowPass {
    pub fn new(cutoff_hz: f32, sample_rate: f32) -> Self {
        let mut out = Self {
            cutoff_hz,
            alpha: 0.0,
            prev_output: 0.0,
        };
        out.reconfigure(sample_rate);
        out
    }

    pub fn reconfigure(&mut self, sample_rate: f32) {
        let dt = 1.0 / sample_rate.max(1.0);
        let rc = 1.0 / (2.0 * std::f32::consts::PI * self.cutoff_hz.max(1.0));
        self.alpha = dt / (rc + dt);
    }

    pub fn reset_state(&mut self) {
        self.prev_output = 0.0;
    }

    pub fn process(&mut self, input: f32) -> f32 {
        self.prev_output += self.alpha * (input - self.prev_output);
        self.prev_output
    }
}
