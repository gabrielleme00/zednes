use super::mapper::{Mapper, Address, Data};

/// Mapper 002 (UNROM) - Simple PRG ROM bank switching.
///
/// CPU $8000-$BFFF: Switchable 16 KB PRG ROM bank.
/// CPU $C000-$FFFF: Fixed 16 KB PRG ROM bank (last bank).
/// PPU $0000-$1FFF: 8 KB CHR ROM.
pub struct Mapper2 {
    prg_banks: usize,
    chr_banks: usize,
    selected_prg_bank: usize,
}

impl Mapper for Mapper2 {
    fn new(prg_banks: usize, chr_banks: usize) -> Self {
        Self {
            prg_banks,
            chr_banks,
            selected_prg_bank: 0,
        }
    }

    fn cpu_map_read(&self, addr: Address) -> Option<Address> {
        match addr {
            0x8000..=0xBFFF => {
                let bank = self.selected_prg_bank.min(self.prg_banks.saturating_sub(1));
                Some((bank * 0x4000 + (addr as usize & 0x3FFF)) as Address)
            }
            0xC000..=0xFFFF => {
                let last_bank = self.prg_banks.saturating_sub(1);
                Some((last_bank * 0x4000 + (addr as usize & 0x3FFF)) as Address)
            }
            _ => None,
        }
    }

    fn cpu_map_write(&mut self, addr: Address, data: Data) -> Option<Address> {
        if addr >= 0x8000 {
            if self.prg_banks > 0 {
                self.selected_prg_bank = (data as usize) % self.prg_banks;
            }
        }

        None
    }

    fn ppu_map_read(&self, addr: Address) -> Option<Address> {
        if addr < 0x2000 {
            Some(addr)
        } else {
            None
        }
    }

    fn ppu_map_write(&mut self, addr: Address, _data: Data) -> Option<Address> {
        if addr < 0x2000 && self.chr_banks == 0 {
            Some(addr)
        } else {
            None
        }
    }
}