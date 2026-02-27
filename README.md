# ZedNES

A Nintendo Entertainment System (NES) emulator written in Rust, featuring a graphical interface with built-in debugging tools.

## Features

- **MOS 6502 CPU** emulation with full official instruction set support
- **PPU** (Picture Processing Unit) producing 256×240 output
- **NTSC / PAL** timing (configurable TV system)
- **iNES ROM format** parsing with mappers support
- **Controller input** (Player 1 & 2)
- **Built-in debuggers**
  - CPU state viewer (registers, flags, disassembly)
  - PPU pattern-table and palette viewer
  - Memory hex viewer

## Screenshots

![ZedNES Emulator](screenshot.png)

## Requirements

- [Rust](https://www.rust-lang.org/tools/install) (edition 2024 / stable toolchain)

## Building

```bash
git clone https://github.com/your-username/zednes.git
cd zednes
cargo build --release
```

The compiled binary will be at `target/release/zednes`.

## Running

```bash
cargo run --release
```

Use **File → Load ROM…** in the menu bar to load a `.nes` ROM file.

## Controls

> Default keyboard mapping (configurable in source):

| NES Button | Key |
|------------|-----|
| A          | Z   |
| B          | X   |
| Start      | Enter |
| Select     | Right Shift |
| D-Pad      | Arrow keys |

## Debuggers

Open any debugger from the **Debug** menu:

- **CPU Debugger** - shows registers (A, X, Y, SP, PC), status flags, and a disassembly view.
- **PPU Debugger** - renders both pattern tables and the system palette.
- **Mem Debugger** - hex dump of the full address space with a *Go to address* input.

## Project Structure

```
src/
├── main.rs              # Entry point
├── lib.rs               # Library root
├── app/
│   ├── state.rs         # Emulator lifecycle (load, reset, pause, step)
│   └── ui.rs            # egui application & all UI panels
├── emulator/
│   ├── nes.rs           # Top-level NES struct (ties CPU + Bus together)
│   ├── bus.rs           # Memory bus (CPU RAM, PPU registers, cartridge)
│   ├── cpu/             # MOS 6502 CPU implementation
│   ├── ppu/             # PPU & TV system timing
│   └── cartridge/       # iNES parser & mapper implementations
├── input/
│   └── controller.rs    # NES controller button state
└── renderer/
    └── frame_buffer.rs  # Raw pixel frame buffer
```

## ROM Compatibility

| Mapper | Name  | Status |
|--------|-------|--------|
| 0      | NROM  | ✅ Supported |
| Others | -     | 🚧 Not yet implemented |

## Dependencies

| Crate   | Purpose                  |
|---------|--------------------------|
| `eframe` | Native window & egui rendering |

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.
