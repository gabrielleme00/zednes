use super::state::EmulatorState;
use crate::emulator::controller::Button;
use crate::renderer::{FrameBuffer, SYSTEM_PALETTE};
use eframe::egui;
use std::collections::HashMap;
use std::path::Path;

/// Commands that can be triggered from the UI or keyboard shortcuts
enum Command {
    ToggleRunPause,
    Reset,
    StepInstruction,
    StepFrame,
}

/// Mapping of keyboard keys to controller buttons for player 1
const CONTROLLER_1_KEY_MAP: &[(egui::Key, Button)] = &[
    (egui::Key::K, Button::A),
    (egui::Key::L, Button::B),
    (egui::Key::Q, Button::Select),
    (egui::Key::E, Button::Start),
    (egui::Key::W, Button::Up),
    (egui::Key::S, Button::Down),
    (egui::Key::A, Button::Left),
    (egui::Key::D, Button::Right),
];

/// Mapping of keyboard keys to commands for emulator control
const COMMAND_KEY_MAP: &[(egui::Key, Command)] = &[
    (egui::Key::Space, Command::ToggleRunPause),
    (egui::Key::R, Command::Reset),
    (egui::Key::T, Command::StepInstruction),
    (egui::Key::Y, Command::StepFrame),
];

/// Main eframe application for the ZedNES emulator
pub struct ZednesApp {
    state: EmulatorState,
    show_palette_debugger: bool,
    show_cpu_debugger: bool,
    show_mem_debugger: bool,
    show_oam_debugger: bool,
    show_audio_diagnostics: bool,
    chr_textures: HashMap<(u8, u8), egui::TextureHandle>,
    /// Selected palette index (0-7) for each pattern table (0 = $0000, 1 = $1000)
    chr_palette_select: [u8; 2],
    screen_texture: Option<egui::TextureHandle>,
    screen_image: egui::ColorImage,
    frame_buffer: FrameBuffer,
    /// Base address shown in Mem debugger
    mem_debugger_addr: u16,
    /// Text input for "Go to address"
    mem_debugger_goto: String,
    /// Number of rows (16 bytes each) to display
    mem_debugger_rows: usize,
    /// Used to send the maximize command on the first frame (more reliable on Linux)
    first_frame: bool,
}

impl ZednesApp {
    pub fn new(_cc: &eframe::CreationContext, startup_rom_path: Option<&str>) -> Self {
        let mut app = Self {
            state: EmulatorState::new(),
            show_palette_debugger: false,
            show_cpu_debugger: true,
            show_mem_debugger: false,
            show_oam_debugger: false,
            show_audio_diagnostics: true,
            chr_textures: HashMap::new(),
            chr_palette_select: [0; 2],
            screen_texture: None,
            screen_image: egui::ColorImage::new(
                [FrameBuffer::NES_WIDTH, FrameBuffer::NES_HEIGHT],
                vec![egui::Color32::BLACK; FrameBuffer::NES_WIDTH * FrameBuffer::NES_HEIGHT],
            ),
            frame_buffer: FrameBuffer::new(),
            mem_debugger_addr: 0x0000,
            mem_debugger_goto: String::new(),
            mem_debugger_rows: 16,
            first_frame: true,
        };

        if let Some(path) = startup_rom_path {
            app.load_rom_from_path(path);
        }

        app
    }

    fn load_rom_from_path<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref();
        match std::fs::read(path) {
            Ok(rom_data) => {
                if let Err(err) = self.state.load_rom(&rom_data) {
                    eprintln!("Failed to load ROM '{}': {}", path.display(), err);
                }
            }
            Err(err) => {
                eprintln!("Failed to read ROM '{}': {}", path.display(), err);
            }
        }
    }

    fn render_menu_bar(&mut self, ui: &mut egui::Ui) {
        egui::MenuBar::new().ui(ui, |ui| {
            ui.menu_button("File", |ui| {
                if ui.button("📂 Load ROM...").clicked() {
                    if let Some(path) = rfd::FileDialog::new()
                        .add_filter("NES ROM", &["nes"])
                        .set_title("Open NES ROM")
                        .pick_file()
                    {
                        self.load_rom_from_path(path);
                    }
                    ui.close();
                }
                ui.separator();
                if ui.button("Exit").clicked() {
                    std::process::exit(0);
                }
            });

            ui.menu_button("Audio", |ui| {
                if let Some(audio) = &self.state.audio {
                    let controls = audio.controls();
                    let mut muted = controls.muted();
                    if ui.checkbox(&mut muted, "Mute").changed() {
                        controls.set_muted(muted);
                    }

                    let mut volume = controls.volume();
                    if ui
                        .add(
                            egui::Slider::new(&mut volume, 0.0..=2.0)
                                .clamping(egui::SliderClamping::Always),
                        )
                        .changed()
                    {
                        controls.set_volume(volume);
                    }

                    ui.separator();
                    ui.label("APU Channel Mute");

                    let mut mute_pulse1 = self.state.is_apu_pulse1_muted();
                    if ui.checkbox(&mut mute_pulse1, "Pulse 1").changed() {
                        self.state.set_apu_pulse1_muted(mute_pulse1);
                    }

                    let mut mute_pulse2 = self.state.is_apu_pulse2_muted();
                    if ui.checkbox(&mut mute_pulse2, "Pulse 2").changed() {
                        self.state.set_apu_pulse2_muted(mute_pulse2);
                    }

                    let mut mute_triangle = self.state.is_apu_triangle_muted();
                    if ui.checkbox(&mut mute_triangle, "Triangle").changed() {
                        self.state.set_apu_triangle_muted(mute_triangle);
                    }

                    let mut mute_noise = self.state.is_apu_noise_muted();
                    if ui.checkbox(&mut mute_noise, "Noise").changed() {
                        self.state.set_apu_noise_muted(mute_noise);
                    }

                    let mut mute_dmc = self.state.is_apu_dmc_muted();
                    if ui.checkbox(&mut mute_dmc, "DMC").changed() {
                        self.state.set_apu_dmc_muted(mute_dmc);
                    }
                } else {
                    ui.label("Audio output unavailable");
                }
            });

            ui.menu_button("Debug", |ui| {
                if ui
                    .selectable_label(self.show_palette_debugger, "Palette Debugger")
                    .clicked()
                {
                    self.show_palette_debugger = !self.show_palette_debugger;
                    ui.close();
                }

                if ui
                    .selectable_label(self.show_cpu_debugger, "CPU Debugger")
                    .clicked()
                {
                    self.show_cpu_debugger = !self.show_cpu_debugger;
                    ui.close();
                }

                if ui
                    .selectable_label(self.show_mem_debugger, "Mem Debugger")
                    .clicked()
                {
                    self.show_mem_debugger = !self.show_mem_debugger;
                    ui.close();
                }

                if ui
                    .selectable_label(self.show_oam_debugger, "OAM Debugger")
                    .clicked()
                {
                    self.show_oam_debugger = !self.show_oam_debugger;
                    ui.close();
                }

                if ui
                    .selectable_label(self.show_audio_diagnostics, "Audio Diagnostics")
                    .clicked()
                {
                    self.show_audio_diagnostics = !self.show_audio_diagnostics;
                    ui.close();
                }
            });

            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                let (status_color, status_label) = if self.state.rom_loaded {
                    if self.state.nes.cpu.halted {
                        (egui::Color32::RED, "HALTED (BRK)")
                    } else if self.state.running {
                        (egui::Color32::GREEN, "RUNNING")
                    } else {
                        (egui::Color32::YELLOW, "PAUSED")
                    }
                } else {
                    (egui::Color32::RED, "NO ROM LOADED")
                };
                ui.colored_label(status_color, status_label);

                if self.show_audio_diagnostics {
                    if let Some(audio) = &self.state.audio {
                        let controls = audio.controls();
                        let stats = controls.snapshot();
                        let buffer_len = audio.buffer_len();
                        let buffer_cap = audio.buffer_capacity().max(1);
                        let fill_percent = (buffer_len as f32 / buffer_cap as f32) * 100.0;
                        ui.separator();
                        ui.monospace(format!(
                            "AUDIO buf:{}/{} ({:.0}%) vol:{:.0}% {} uf:{} of:{} fps:{:.2}",
                            buffer_len,
                            buffer_cap,
                            fill_percent,
                            stats.volume * 100.0,
                            if stats.muted { "MUTED" } else { "LIVE" },
                            stats.underflows,
                            stats.overflows,
                            self.state.target_fps()
                        ));
                    }
                }

                if let Some(ref msg) = self.state.error_message {
                    ui.separator();
                    ui.colored_label(egui::Color32::from_rgb(255, 120, 120), msg.as_str());
                }
            });
        });
    }

    fn render_display(&mut self, ctx: &egui::Context) {
        const NES_WIDTH: f32 = 256.0;
        const NES_HEIGHT: f32 = 240.0;

        // Update the FrameBuffer and ColorImage in-place
        self.frame_buffer
            .update_from_ppu_screen(&self.state.nes.bus.ppu.screen);
        for (pixel, c) in self
            .screen_image
            .pixels
            .iter_mut()
            .zip(self.frame_buffer.as_slice().chunks_exact(3))
        {
            *pixel = egui::Color32::from_rgb(c[0], c[1], c[2]);
        }

        // Update or create the texture for the main screen
        match &mut self.screen_texture {
            Some(tex) => tex.set(self.screen_image.clone(), egui::TextureOptions::NEAREST),
            None => {
                self.screen_texture = Some(ctx.load_texture(
                    "nes_screen",
                    self.screen_image.clone(),
                    egui::TextureOptions::NEAREST,
                ));
            }
        }

        // Center the display in the available space
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical_centered(|ui| {
                // Scale to fit available space (integer scale preferred)
                let available_size = ui.available_size();
                let scale_x = (available_size.x / NES_WIDTH).floor();
                let scale_y = (available_size.y / NES_HEIGHT).floor();
                let scale = scale_x.min(scale_y).max(1.0);

                // Calculate the display size based on the NES resolution and the chosen scale factor
                let display_size = egui::vec2(NES_WIDTH * scale, NES_HEIGHT * scale);
                let texture = self.screen_texture.as_ref().unwrap();
                ui.image((texture.id(), display_size));
            });
        });
    }

    fn render_palette_dbg_window(&mut self, ctx: &egui::Context) {
        let mut open = self.show_palette_debugger;
        egui::Window::new("PPU Debugger")
            .open(&mut open)
            .resizable(true)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    self.render_pattern_table(ui, ctx, 0);
                    ui.separator();
                    self.render_pattern_table(ui, ctx, 1);
                });
                self.render_palettes(ui);
            });
        self.show_palette_debugger = open;
    }

    fn render_oam_dbg_window(&self, ctx: &egui::Context, open: &mut bool) {
        egui::Window::new("OAM Debugger")
            .open(open)
            .resizable(true)
            .show(ctx, |ui| {
                let oam = self.state.nes.get_oam();

                egui::Grid::new("oam_header")
                    .num_columns(9)
                    .spacing([6.0, 2.0])
                    .show(ui, |ui| {
                        ui.strong("#");
                        ui.strong("X");
                        ui.strong("Y");
                        ui.strong("Tile");
                        ui.strong("Attr");
                        ui.strong("Pal");
                        ui.strong("Pri");
                        ui.strong("H-Flip");
                        ui.strong("V-Flip");
                        ui.end_row();
                    });

                ui.separator();

                egui::ScrollArea::vertical()
                    .id_salt("oam_scroll")
                    .show(ui, |ui| {
                        egui::Grid::new("oam_table")
                            .num_columns(9)
                            .spacing([6.0, 2.0])
                            .striped(true)
                            .show(ui, |ui| {
                                for (i, (y, tile, attr, x)) in oam.iter().enumerate() {
                                    let palette  = attr & 0x03;
                                    let priority = (attr >> 5) & 0x01;
                                    let flip_h   = (attr >> 6) & 0x01;
                                    let flip_v   = (attr >> 7) & 0x01;

                                    ui.label(format!("{:02}", i));
                                    ui.label(format!("${:02X}", x));
                                    ui.label(format!("${:02X}", y));
                                    ui.label(format!("${:02X}", tile));
                                    ui.label(format!("${:02X}", attr))
                                        .on_hover_text(format!("binary: {:08b}", attr));
                                    ui.label(format!("SP{}", palette + 4));
                                    ui.label(if priority == 0 { "Front" } else { "Behind" });
                                    ui.label(if flip_h != 0 { "Y" } else { "N" });
                                    ui.label(if flip_v != 0 { "Y" } else { "N" });
                                    ui.end_row();
                                }
                            });
                    });
            });
    }

    /// Draw a single filled colour swatch with a hover tooltip.
    fn render_color_swatch(
        ui: &mut egui::Ui,
        size: f32,
        rounding: f32,
        color: egui::Color32,
        hover_text: String,
    ) {
        let (rect, response) = ui.allocate_exact_size(egui::vec2(size, size), egui::Sense::hover());
        ui.painter().rect_filled(rect, rounding, color);
        response.on_hover_text(hover_text);
    }

    /// Draw one row of 4 palette swatches for the given palette index.
    fn render_palette_row(&self, ui: &mut egui::Ui, pal_idx: usize) {
        ui.horizontal(|ui| {
            ui.label(format!("${:02X}:", pal_idx * 4));
            for color_idx in 0..4 {
                let palette_addr = pal_idx * 4 + color_idx;
                let nes_color = self.state.nes.get_palette(palette_addr);
                let (r, g, b) = SYSTEM_PALETTE[nes_color as usize % 64];
                Self::render_color_swatch(
                    ui,
                    20.0,
                    2.0,
                    egui::Color32::from_rgb(r, g, b),
                    format!(
                        "Palette addr: ${:02X}  Color: ${:02X}",
                        palette_addr, nes_color
                    ),
                );
            }
        });
    }

    fn render_palettes(&self, ui: &mut egui::Ui) {
        ui.group(|ui| {
            ui.horizontal(|ui| {
                // Background palettes (0-3)
                ui.vertical(|ui| {
                    ui.label("Background:");
                    for pal_idx in 0..4 {
                        self.render_palette_row(ui, pal_idx);
                    }
                });

                ui.add_space(4.0);
                ui.separator();
                ui.add_space(4.0);

                // System palette: all 64 NES colors arranged as 4 rows of 16
                ui.vertical(|ui| {
                    ui.label("System Palette:");
                    ui.add_space(4.0);
                    for row in 0..4_usize {
                        ui.horizontal(|ui| {
                            ui.label(format!("${:02X}:", row * 16));
                            for col in 0..16_usize {
                                let idx = row * 16 + col;
                                let (r, g, b) = SYSTEM_PALETTE[idx];
                                Self::render_color_swatch(
                                    ui,
                                    18.0,
                                    1.0,
                                    egui::Color32::from_rgb(r, g, b),
                                    format!("${:02X} - rgb({}, {}, {})", idx, r, g, b),
                                );
                            }
                        });
                    }
                });

                ui.add_space(4.0);
                ui.separator();
                ui.add_space(4.0);

                // Sprite palettes (4-7)
                ui.vertical(|ui| {
                    ui.label("Sprite:");
                    for pal_idx in 4..8 {
                        self.render_palette_row(ui, pal_idx);
                    }
                });
            });
        });
    }

    fn render_pattern_table(&mut self, ui: &mut egui::Ui, ctx: &egui::Context, table_idx: u8) {
        let palette_sel = self.chr_palette_select[table_idx as usize];
        let cache_key = (table_idx, palette_sel);

        // Rebuild texture when ROM data may have changed or palette was switched
        if self.state.rom_loaded {
            let tex =
                Self::create_pattern_table_texture(ctx, &self.state.nes, table_idx, palette_sel);
            self.chr_textures.insert(cache_key, tex);
        } else {
            self.chr_textures.entry(cache_key).or_insert_with(|| {
                Self::create_pattern_table_texture(ctx, &self.state.nes, table_idx, palette_sel)
            });
        }

        let texture = &self.chr_textures[&cache_key];

        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                ui.label(if table_idx == 0 { "$0000" } else { "$1000" });
                ui.separator();
                for p in 0u8..8 {
                    let label = if p < 4 {
                        format!("BG{}", p)
                    } else {
                        format!("SP{}", p - 4)
                    };
                    if ui.selectable_label(palette_sel == p, label).clicked() {
                        self.chr_palette_select[table_idx as usize] = p;
                    }
                }
            });

            let scale = 3.0;
            let size = egui::vec2(128.0 * scale, 128.0 * scale);
            ui.image((texture.id(), size));
        });
    }

    /// Build a 128×128 texture from one of the two CHR pattern tables,
    /// coloured with the specified palette (0-3 background, 4-7 sprite).
    fn create_pattern_table_texture(
        ctx: &egui::Context,
        nes: &crate::emulator::Nes,
        table_idx: u8,
        palette: u8,
    ) -> egui::TextureHandle {
        const SIZE: usize = 128; // 16 tiles × 8 pixels
        let mut pixels = vec![egui::Color32::BLACK; SIZE * SIZE];
        let tiles = nes.get_pattern_table(table_idx);

        // Check whether the selected palette has any non-zero entries for colors 1-3.
        // When it does not (e.g. palette RAM not yet initialised by the game), fall back
        // to a four-shade greyscale ramp so tile shapes are always visible.
        let palette_base = palette as usize * 4;
        let has_real_colors =
            (1..4).any(|i| nes.bus.ppu.tbl_palette[(palette_base + i).min(31)] != 0);

        let debug_shades: [(u8, u8, u8); 4] =
            [(0, 0, 0), (85, 85, 85), (170, 170, 170), (255, 255, 255)];

        for tile_y in 0..16usize {
            for tile_x in 0..16usize {
                let tile = tiles[tile_y * 16 + tile_x];
                for row in 0..8usize {
                    for col in 0..8usize {
                        let pixel_value = tile[row * 8 + col];
                        let (r, g, b) = if has_real_colors {
                            nes.get_color_from_palette_ram(palette, pixel_value)
                        } else {
                            debug_shades[pixel_value as usize & 3]
                        };
                        pixels[(tile_y * 8 + row) * SIZE + (tile_x * 8 + col)] =
                            egui::Color32::from_rgb(r, g, b);
                    }
                }
            }
        }

        ctx.load_texture(
            format!("pattern_table_{}_{}", table_idx, palette),
            egui::ColorImage {
                size: [SIZE, SIZE],
                source_size: egui::Vec2::splat(SIZE as f32),
                pixels,
            },
            egui::TextureOptions::NEAREST,
        )
    }

    fn render_mem_dbg_window(&mut self, ctx: &egui::Context) {
        let rom_loaded = self.state.rom_loaded;
        let mut open = self.show_mem_debugger;

        egui::Window::new("Mem Debugger")
            .open(&mut open)
            .default_width(620.0)
            .default_height(450.0)
            .resizable(true)
            .show(ctx, |ui| {
                if !rom_loaded {
                    ui.label("No ROM loaded");
                    return;
                }

                // Navigation bar
                ui.horizontal(|ui| {
                    // Quick-jump buttons
                    if ui.button("Zero Page").clicked() {
                        self.mem_debugger_addr = 0x0000;
                    }
                    if ui.button("Stack").clicked() {
                        self.mem_debugger_addr = 0x0100;
                    }
                    if ui.button("RAM").clicked() {
                        self.mem_debugger_addr = 0x0200;
                    }
                    if ui.button("PPU Regs").clicked() {
                        self.mem_debugger_addr = 0x2000;
                    }
                    if ui.button("PRG ROM").clicked() {
                        self.mem_debugger_addr = 0x8000;
                    }

                    ui.separator();

                    // Previous / Next page (256 bytes)
                    if ui.button("◀ -256").clicked() {
                        self.mem_debugger_addr = self.mem_debugger_addr.saturating_sub(256);
                    }
                    if ui.button("+256 ▶").clicked() {
                        self.mem_debugger_addr = self.mem_debugger_addr.saturating_add(256);
                    }
                });

                ui.horizontal(|ui| {
                    ui.label("Go to ($):");
                    let resp = ui.add(
                        egui::TextEdit::singleline(&mut self.mem_debugger_goto)
                            .desired_width(60.0)
                            .font(egui::TextStyle::Monospace),
                    );
                    if resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
                        if let Ok(addr) =
                            u16::from_str_radix(self.mem_debugger_goto.trim_start_matches('$'), 16)
                        {
                            self.mem_debugger_addr = addr & 0xFFF0; // align to 16
                        }
                    }
                    if ui.button("Go").clicked() {
                        if let Ok(addr) =
                            u16::from_str_radix(self.mem_debugger_goto.trim_start_matches('$'), 16)
                        {
                            self.mem_debugger_addr = addr & 0xFFF0;
                        }
                    }

                    ui.separator();

                    ui.label("Rows:");
                    ui.add(
                        egui::Slider::new(&mut self.mem_debugger_rows, 4..=64)
                            .clamping(egui::SliderClamping::Always),
                    );
                });

                ui.separator();

                // Hex dump
                let base = self.mem_debugger_addr;
                let rows = self.mem_debugger_rows;

                // Column header
                ui.horizontal(|ui| {
                    ui.add_space(4.0);
                    ui.monospace(
                        egui::RichText::new(
                            "  ADDR    00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F   ASCII",
                        )
                        .color(egui::Color32::from_rgb(160, 160, 220)),
                    );
                });

                ui.separator();

                let pc = self.state.nes.get_pc();
                let sp_addr = 0x0100u16 | (self.state.nes.get_cpu_state().3 as u16);

                egui::ScrollArea::vertical()
                    .auto_shrink([false, true])
                    .show(ui, |ui| {
                        for row in 0..rows {
                            let row_addr = base.wrapping_add((row * 16) as u16);

                            ui.horizontal(|ui| {
                                ui.add_space(4.0);

                                // Address column
                                ui.monospace(
                                    egui::RichText::new(format!("  ${:04X}  ", row_addr))
                                        .color(egui::Color32::from_rgb(100, 180, 255)),
                                );

                                // Hex bytes - two groups of 8 separated by an extra space
                                for byte_idx in 0..16u16 {
                                    if byte_idx == 8 {
                                        ui.monospace(" ");
                                    }
                                    let addr = row_addr.wrapping_add(byte_idx);
                                    let byte = self.state.nes.read_cpu_mem(addr);

                                    // Colour-code special addresses
                                    let color = if addr == pc {
                                        egui::Color32::from_rgb(100, 255, 100) // PC - green
                                    } else if addr == sp_addr {
                                        egui::Color32::from_rgb(255, 200, 80) // SP - gold
                                    } else if byte == 0x00 {
                                        egui::Color32::from_rgb(80, 80, 80) // zero - dim
                                    } else {
                                        egui::Color32::WHITE
                                    };

                                    ui.monospace(
                                        egui::RichText::new(format!("{:02X}", byte)).color(color),
                                    );
                                }

                                ui.monospace(" ");

                                // ASCII column
                                let mut ascii = String::with_capacity(16);
                                for byte_idx in 0..16u16 {
                                    let b = self
                                        .state
                                        .nes
                                        .read_cpu_mem(row_addr.wrapping_add(byte_idx));
                                    ascii.push(if b >= 0x20 && b < 0x7F {
                                        b as char
                                    } else {
                                        '.'
                                    });
                                }
                                ui.monospace(
                                    egui::RichText::new(ascii)
                                        .color(egui::Color32::from_rgb(200, 200, 140)),
                                );
                            });
                        }
                    });

                // Legend
                ui.separator();
                ui.horizontal(|ui| {
                    ui.monospace(
                        egui::RichText::new("■").color(egui::Color32::from_rgb(100, 255, 100)),
                    );
                    ui.label("PC");
                    ui.add_space(8.0);
                    ui.monospace(
                        egui::RichText::new("■").color(egui::Color32::from_rgb(255, 200, 80)),
                    );
                    ui.label("SP");
                    ui.add_space(8.0);
                    ui.monospace(
                        egui::RichText::new("■").color(egui::Color32::from_rgb(80, 80, 80)),
                    );
                    ui.label("$00");
                });
            });

        self.show_mem_debugger = open;
    }

    fn render_cpu_dbg_window(&mut self, ctx: &egui::Context) {
        let rom_loaded = self.state.rom_loaded;
        let running = self.state.running;

        egui::SidePanel::right("cpu_debugger")
            .resizable(true)
            .default_width(300.0)
            .show(ctx, |ui| {
                ui.heading("CPU Debugger");
                ui.separator();
                if !rom_loaded {
                    ui.label("No ROM loaded");
                    return;
                }

                // CPU State
                ui.group(|ui| {
                    ui.set_min_width(ui.available_width());
                    let (a, x, y, sp, pc, status) = self.state.nes.get_cpu_state();

                    let flags = format!(
                        "{}{}{}{}{}{}{}",
                        if status & 0x80 != 0 { "N" } else { "n" },
                        if status & 0x40 != 0 { "V" } else { "v" },
                        if status & 0x10 != 0 { "B" } else { "b" },
                        if status & 0x08 != 0 { "D" } else { "d" },
                        if status & 0x04 != 0 { "I" } else { "i" },
                        if status & 0x02 != 0 { "Z" } else { "z" },
                        if status & 0x01 != 0 { "C" } else { "c" },
                    );

                    ui.horizontal(|ui| {
                        ui.monospace(format!("PC:{:04X}", pc));
                        ui.separator();
                        ui.monospace(format!("A:{:02X}", a));
                        ui.monospace(format!("X:{:02X}", x));
                        ui.monospace(format!("Y:{:02X}", y));
                        ui.monospace(format!("SP:{:02X}", sp));
                    });

                    ui.horizontal(|ui| {
                        ui.monospace(format!("ST:{:02X}  ", status));
                        ui.separator();
                        ui.monospace(flags);
                    })
                });

                ui.add_space(5.0);

                // Emulation Controls
                ui.group(|ui| {
                    ui.set_min_width(ui.available_width());
                    ui.horizontal(|ui| {
                        let play_pause_text = if running { "⏸ Pause" } else { "▶ Run" };
                        if ui.button(play_pause_text).clicked() {
                            self.state.toggle_pause();
                        }

                        if ui.button("🔄 Reset").clicked() {
                            self.state.reset();
                        }

                        if ui.button("➡ Instr.").clicked() {
                            self.state.step_instruction();
                        }

                        if ui.button("⏭ Frame").clicked() {
                            self.state.step_frame_once();
                        }
                    });
                });

                ui.add_space(5.0);

                // Disassembly view
                ui.group(|ui| {
                    ui.set_min_width(ui.available_width());
                    egui::ScrollArea::vertical()
                        .auto_shrink(false)
                        .show(ui, |ui| {
                            let pc = self.state.nes.get_pc();

                            // Show instructions around PC
                            let start_addr = pc.saturating_sub(0x30);
                            let mut addr = start_addr;

                            for _ in 0..50 {
                                let opcode = self.state.nes.read_cpu_mem(addr);
                                let instruction =
                                    crate::emulator::cpu::instruction::get_instruction(opcode);

                                // Format the instruction
                                let bytes = Self::get_instruction_bytes(
                                    &self.state.nes,
                                    addr,
                                    instruction.bytes,
                                );
                                let disasm = Self::format_instruction(addr, &instruction, &bytes);

                                // Highlight current PC
                                let text = if addr == pc {
                                    egui::RichText::new(disasm)
                                        .monospace()
                                        .background_color(egui::Color32::from_rgb(60, 60, 100))
                                        .color(egui::Color32::WHITE)
                                } else {
                                    egui::RichText::new(disasm).monospace()
                                };

                                ui.label(text);

                                addr = addr.wrapping_add(instruction.bytes as u16);
                            }
                        });
                });
            });
    }

    fn get_instruction_bytes(nes: &crate::emulator::Nes, addr: u16, count: u8) -> Vec<u8> {
        (0..count)
            .map(|i| nes.read_cpu_mem(addr.wrapping_add(i as u16)))
            .collect()
    }

    fn format_instruction(
        addr: u16,
        instruction: &crate::emulator::cpu::instruction::Instruction,
        bytes: &[u8],
    ) -> String {
        use crate::emulator::cpu::instruction::AddressingMode;

        let bytes_str = bytes
            .iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<_>>()
            .join(" ");

        let operand = if bytes.len() > 1 {
            use AddressingMode::*;
            match instruction.mode {
                Immediate => format!("#${:02X}", bytes[1]),
                ZeroPage => format!("${:02X}", bytes[1]),
                ZeroPageX => format!("${:02X},X", bytes[1]),
                ZeroPageY => format!("${:02X},Y", bytes[1]),
                Absolute if bytes.len() == 3 => format!("${:02X}{:02X}", bytes[2], bytes[1]),
                AbsoluteX if bytes.len() == 3 => format!("${:02X}{:02X},X", bytes[2], bytes[1]),
                AbsoluteY if bytes.len() == 3 => format!("${:02X}{:02X},Y", bytes[2], bytes[1]),
                Indirect if bytes.len() == 3 => format!("(${:02X}{:02X})", bytes[2], bytes[1]),
                IndirectX => format!("(${:02X},X)", bytes[1]),
                IndirectY => format!("(${:02X}),Y", bytes[1]),
                Relative => {
                    let offset = bytes[1] as i8;
                    let target = addr.wrapping_add(2).wrapping_add(offset as u16);
                    format!("${:04X}", target)
                }
                _ => String::new(),
            }
        } else {
            String::new()
        };

        format!(
            "{:04X}  {:<8}  {} {}",
            addr, bytes_str, instruction.mnemonic, operand
        )
    }

    /// Handle keyboard input for controller buttons and emulator commands.
    fn handle_input(&mut self, ctx: &egui::Context) {
        let (pressed_btns, released_btns, triggered_cmds) = ctx.input(|i| {
            let mut pressed = Vec::new();
            let mut released = Vec::new();
            for (key, button) in CONTROLLER_1_KEY_MAP {
                if i.key_pressed(*key) {
                    pressed.push(*button);
                }
                if i.key_released(*key) {
                    released.push(*button);
                }
            }
            let cmds = COMMAND_KEY_MAP
                .iter()
                .filter(|(key, _)| i.key_pressed(*key))
                .map(|(_, cmd)| cmd)
                .collect::<Vec<_>>();
            (pressed, released, cmds)
        });

        for btn in pressed_btns {
            self.state.nes.bus.controllers[0].press(btn);
        }
        for btn in released_btns {
            self.state.nes.bus.controllers[0].release(btn);
        }
        for cmd in triggered_cmds {
            match cmd {
                Command::ToggleRunPause => self.state.toggle_pause(),
                Command::Reset => self.state.reset(),
                Command::StepInstruction => self.state.step_instruction(),
                Command::StepFrame => self.state.step_frame_once(),
            }
        }
    }
}

impl eframe::App for ZednesApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // On the first frame, send a command to maximize the window.
        // This is more reliable than setting the initial window size in eframe,
        // which can be ignored by some platforms or window managers.
        if self.first_frame {
            ctx.send_viewport_cmd(egui::ViewportCommand::Maximized(true));
            self.first_frame = false;
        }

        // Handle input for controller buttons and emulator commands
        self.handle_input(ctx);

        // Step the emulator if it's running. We do this before rendering so that the display and debuggers
        // show the most up-to-date state. The `step_frame` method will internally decide how many CPU cycles to run
        // based on the current TV system and whether the user has requested a single-step or frame advance.
        if self.state.running {
            ctx.request_repaint();
            self.state.step_frame();
        }

        // Render the menu bar at the top
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            self.render_menu_bar(ui);
        });

        // Render the CPU debugger as a side panel if enabled
        if self.show_cpu_debugger {
            self.render_cpu_dbg_window(ctx);
        }

        // Render the main NES display in the central panel
        self.render_display(ctx);

        // Render the PPU debugger as a separate window if enabled
        if self.show_palette_debugger {
            self.render_palette_dbg_window(ctx);
        }

        // Render the memory debugger as a separate window if enabled
        if self.show_mem_debugger {
            self.render_mem_dbg_window(ctx);
        }

        // Render the OAM debugger as a separate window if enabled
        if self.show_oam_debugger {
            let mut open = self.show_oam_debugger;
            self.render_oam_dbg_window(ctx, &mut open);
            self.show_oam_debugger = open;
        }
    }
}
