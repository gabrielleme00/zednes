/// Internal representation of the iNES header
pub struct Header {
    pub name: [u8; 4],
    pub prg_rom_size: usize,
    pub chr_rom_size: usize,
    pub mapper_1: u8,
    pub mapper_2: u8,
    pub prg_ram_size: usize,
    pub tv_system_1: u8,
    pub tv_system_2: u8,
    pub unused: [u8; 5],
}

impl Header {
    /// Parses the iNES header from the given byte slice.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, String> {
        if bytes.len() < 16 {
            return Err("Header must be at least 16 bytes".to_string());
        }
        let mut name = [0; 4];
        name.copy_from_slice(&bytes[0..4]);
        Ok(Header {
            name,
            prg_rom_size: bytes[4] as usize * 1024 * 16,
            chr_rom_size: bytes[5] as usize * 1024 * 8,
            mapper_1: bytes[6],
            mapper_2: bytes[7],
            prg_ram_size: if bytes[8] == 0 { 8192 } else { bytes[8] as usize * 1024 * 8 },
            tv_system_1: bytes[9],
            tv_system_2: bytes[10],
            unused: [0; 5],
        })
    }
}