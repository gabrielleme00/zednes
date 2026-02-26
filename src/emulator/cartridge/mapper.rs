pub type Address = u16;
pub type Data = u8;

pub trait Mapper {
    // Creates a new mapper with the given number of PRG and CHR banks.
    fn new(prg_banks: usize, chr_banks: usize) -> Self
    where
        Self: Sized;

    // These functions map an address to a cartridge address.
    // Returns `None` if the address is not mapped by the cartridge.
    fn cpu_map_read(&self, addr: Address) -> Option<Address>;
    fn cpu_map_write(&mut self, addr: Address, data: Data) -> Option<Address>;
    fn ppu_map_read(&self, addr: Address) -> Option<Address>;
    fn ppu_map_write(&mut self, addr: Address, data: Data) -> Option<Address>;
}
