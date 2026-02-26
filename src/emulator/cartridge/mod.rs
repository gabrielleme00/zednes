pub mod header;
pub mod mapper;
pub mod mapper0;

use mapper::Mapper;
use mapper0::Mapper0;

/// NES Cartridge with iNES format support
pub struct Cartridge {
    pub prg_rom: Vec<u8>,         // Program ROM
    pub chr_rom: Vec<u8>,         // Character ROM
    pub mapper_id: u8,            // Mapper number
    pub mirroring: Mirroring,     // Mirroring type
    mapper: Box<dyn Mapper>,      // Active mapper implementation
}

#[derive(Debug, Clone, Copy)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    FourScreen,
}

impl Cartridge {
    pub fn new(rom_data: &[u8]) -> Result<Self, String> {
        // Parse the iNES header
        let header = header::Header::from_bytes(rom_data)?;

        // Validate the header
        if &header.name != b"NES\x1A" {
            return Err("Invalid iNES header".to_string());
        }

        // Mapper number is a combination of bits from both mapper_1 and mapper_2
        let mapper = (header.mapper_2 & 0xF0) | (header.mapper_1 >> 4);

        // Determine mirroring type
        let mirroring = if header.mapper_1 & 0x08 != 0 {
            Mirroring::FourScreen
        } else if header.mapper_1 & 0x01 != 0 {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };

        // Skip trainer if present
        let has_trainer = header.mapper_1 & 0x04 != 0;
        let prg_start = 16 + if has_trainer { 512 } else { 0 };
        let chr_start = prg_start + header.prg_rom_size;

        // Validate ROM data length
        if rom_data.len() < chr_start + header.chr_rom_size {
            return Err("ROM data incomplete".to_string());
        }

        // Load PRG and CHR data
        let prg_rom = rom_data[prg_start..prg_start + header.prg_rom_size].to_vec();
        let chr_rom = if header.chr_rom_size > 0 {
            rom_data[chr_start..chr_start + header.chr_rom_size].to_vec()
        } else {
            // CHR RAM
            vec![0; 8192]
        };

        // Calculate number of PRG and CHR banks
        let prg_banks = header.prg_rom_size / (16 * 1024);
        let chr_banks = header.chr_rom_size / (8 * 1024);

        // Create the appropriate mapper implementation based on the mapper number
        let mapper_impl: Box<dyn Mapper> = match mapper {
            0 => Box::new(Mapper0::new(prg_banks, chr_banks)),
            _ => return Err(format!("Unsupported mapper: {}", mapper)),
        };

        Ok(Cartridge {
            prg_rom,
            chr_rom,
            mapper_id: mapper,
            mirroring,
            mapper: mapper_impl,
        })
    }

    /// CPU read from cartridge. Returns (data, is_mapped).
    pub fn cpu_read(&self, addr: u16) -> (u8, bool) {
        if let Some(mapped_addr) = self.mapper.cpu_map_read(addr) {
            let data = self.prg_rom[mapped_addr as usize];
            (data, true)
        } else {
            (0, false)
        }
    }

    /// CPU write to cartridge. Returns true if the write was handled by the mapper.
    pub fn cpu_write(&mut self, addr: u16, data: u8) -> bool {
        if let Some(mapped_addr) = self.mapper.cpu_map_write(addr, data) {
            self.prg_rom[mapped_addr as usize] = data;
            true
        } else {
            false
        }
    }

    /// PPU read from cartridge. Returns (data, is_mapped).
    pub fn ppu_read(&self, addr: u16) -> (u8, bool) {
        if let Some(mapped_addr) = self.mapper.ppu_map_read(addr) {
            let data = self.chr_rom[mapped_addr as usize];
            (data, true)
        } else {
            (0, false)
        }
    }

    /// PPU write to cartridge. Returns true if the write was handled by the mapper.
    pub fn ppu_write(&mut self, addr: u16, data: u8) -> bool {
        if let Some(mapped_addr) = self.mapper.ppu_map_write(addr, data) {
            self.chr_rom[mapped_addr as usize] = data;
            true
        } else {
            false
        }
    }
}
