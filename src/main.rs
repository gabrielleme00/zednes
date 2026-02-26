use zednes::ZednesApp;
use eframe::egui;

fn main() -> eframe::Result<()> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1200.0, 800.0])
            .with_maximized(true)
            .with_title("ZedNES - NES Emulator"),
        ..Default::default()
    };

    eframe::run_native(
        "ZedNES",
        options,
        Box::new(|cc| Ok(Box::new(ZednesApp::new(cc)))),
    )
}
