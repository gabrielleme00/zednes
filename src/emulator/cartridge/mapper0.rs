use super::mapper::{Mapper, Address, Data};

/// Mapper 000 (NROM) - No bank switching.
///
/// CPU $8000-$BFFF: First 16 KB PRG ROM bank.
/// CPU $C000-$FFFF: Last 16 KB PRG ROM bank (or mirror of first if only 1 bank).
/// PPU $0000-$1FFF: 8 KB CHR ROM.
pub struct Mapper0 {
    prg_banks: usize,
    chr_banks: usize,
}

impl Mapper for Mapper0 {
    fn new(prg_banks: usize, chr_banks: usize) -> Self {
        Mapper0 { prg_banks, chr_banks }
    }

    fn cpu_map_read(&self, addr: Address) -> Option<Address> {
        if addr >= 0x8000 {
            // Mask mirrors the address if only one 16 KB bank is present.
            let mask = if self.prg_banks > 1 { 0x7FFF } else { 0x3FFF };
            Some((addr & mask) as Address)
        } else {
            None
        }
    }

    fn cpu_map_write(&mut self, addr: Address, _data: Data) -> Option<Address> {
        if addr >= 0x8000 {
            let mask = if self.prg_banks > 1 { 0x7FFF } else { 0x3FFF };
            Some((addr & mask) as Address)
        } else {
            None
        }
    }

    fn ppu_map_read(&self, addr: Address) -> Option<Address> {
        if addr < 0x2000 {
            Some(addr as Address)
        } else {
            None
        }
    }

    fn ppu_map_write(&mut self, addr: Address, _data: Data) -> Option<Address> {
        // Allow writes only if CHR RAM is present (chr_banks == 0 means CHR RAM).
        if addr < 0x2000 && self.chr_banks == 0 {
            Some(addr as Address)
        } else {
            None
        }
    }
}
