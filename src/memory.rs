use crate::ppu::{ControlFlags, MaskFlags, Ppu};

const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 0x4000;
const CHR_ROM_PAGE_SIZE: usize = 0x2000;

pub trait Memory {
    fn read_u8(&mut self, addr: u16) -> u8;
    fn write_u8(&mut self, addr: u16, value: u8);

    fn read_u16(&mut self, addr: u16) -> u16 {
        let lo = self.read_u8(addr);
        let hi = self.read_u8(addr + 1);
        u16::from_le_bytes([lo, hi])
    }

    fn write_u16(&mut self, addr: u16, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.write_u8(addr, lo);
        self.write_u8(addr + 1, hi);
    }
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mirroring: Mirroring,
    pub mapper: u8,
}

impl Rom {
    pub fn from_bytes(data: &[u8]) -> Result<Self, &'static str> {
        if data.len() > 0x8000 {
            return Err("ROM too large");
        }

        if &data[0..4] != NES_TAG {
            return Err("Invalid NES tag");
        }

        let mapper = (data[7] & 0b1111_0000) | (data[6] >> 4);

        let ines_ver = (data[7] >> 2) & 0b11;
        if ines_ver != 0 {
            return Err("iNES2.0 format not supported");
        }

        let four_screen = data[6] & 0b1000 != 0;
        let vertical_mirroring = data[6] & 0b1 != 0;
        let mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let prg_rom_size = data[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = data[5] as usize * CHR_ROM_PAGE_SIZE;

        let skip_trainer = data[6] & 0b100 != 0;

        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        Ok(Rom {
            prg_rom: data[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: data[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
            mirroring,
            mapper,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub struct MemoryBus {
    pub vram: [u8; 0x800],
    pub rom: Rom,
}

impl MemoryBus {
    pub fn default_with_rom(rom: Rom) -> Self {
        MemoryBus {
            vram: [0; 0x800],
            rom,
        }
    }

    pub fn with_empty_rom() -> Self {
        let rom = Rom::from_bytes(&{
            let header = vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ];

            let prg_rom = vec![1; 2 * PRG_ROM_PAGE_SIZE];
            let chr_rom = vec![2; 2 * CHR_ROM_PAGE_SIZE];

            header
                .into_iter()
                .chain(prg_rom)
                .chain(chr_rom)
                .collect::<Vec<_>>()
        })
        .unwrap();

        Self::default_with_rom(rom)
    }

    pub fn snapshot<'a>(&'a mut self, ppu: &'a mut Ppu) -> MemorySnapshot<'a> {
        MemorySnapshot { bus: self, ppu }
    }
}

pub struct MemorySnapshot<'a> {
    bus: &'a mut MemoryBus,
    ppu: &'a mut Ppu,
}

impl MemorySnapshot<'_> {
    pub fn page_cross(&self, addr1: u16, addr2: u16) -> bool {
        addr1 & 0xFF00 != addr2 & 0xFF00
    }
}

impl Memory for MemorySnapshot<'_> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => {
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.bus.vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address 0x{:04X}", addr);
            }
            0x2002 => self.ppu.read_and_clear_status_register().bits(),
            0x2004 => todo!("Read PPU OAM data"),
            0x2007 => self.ppu.read_from_data_segment(&self.bus.rom.chr_rom),
            0x2008..=0x3FFF => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.read_u8(mirror_down_addr)
            }
            0x4000..=0x4015 => {
                // TODO: Implement the APU
                0xFF
            }
            0x8000..=0xFFFF => {
                let mut addr = addr - 0x8000;
                if self.bus.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
                    addr = addr % 0x4000;
                }

                self.bus.rom.prg_rom[addr as usize]
            }
            _ => panic!("Invalid read: 0x{:04X}", addr),
        }
    }

    fn write_u8(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => {
                let mirror_down_addr = addr & 0b11111111111;
                self.bus.vram[mirror_down_addr as usize] = value;
            }
            0x2000 => self
                .ppu
                .write_to_control_register(ControlFlags::from_bits_truncate(value)),
            0x2001 => self
                .ppu
                .write_to_mask_register(MaskFlags::from_bits_truncate(value)),
            0x2002 => panic!("Attempt to write to read-only PPU status register"),
            0x2005 => {
                self.ppu.write_to_scroll_register(value);
            }
            0x2006 => {
                self.ppu.write_to_data_address_register(value);
            }
            0x2007 => {
                self.ppu.write_to_data_segment(value);
            }

            0x2008..=0x3FFF => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.write_u8(mirror_down_addr, value);
            }
            0x4000..=0x4013 | 0x4015 => {
                // TODO: Implement the APU
            }
            0x8000..=0xFFFF => panic!(
                "Attempted to write to cartridge ROM at address {:04X}",
                addr
            ),
            _ => panic!("Invalid write: {:04X}", addr),
        }
    }
}

// pub struct MemoryMapper {
//     vram: [u8; 0x800],
//     prg_rom: Vec<u8>,
//     ppu: Ppu,
// }

// impl MemoryMapper {
//     pub fn default_with_rom(rom: Rom) -> Self {
//         MemoryMapper {
//             vram: [0; 0x800],
//             prg_rom: rom.prg_rom,
//             ppu: Ppu::new(rom.chr_rom, rom.mirroring),
//         }
//     }

//     pub fn with_empty_rom() -> Self {
//         let rom = Rom::from_bytes(&{
//             let header = vec![
//                 0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
//             ];

//             let prg_rom = vec![1; 2 * PRG_ROM_PAGE_SIZE];
//             let chr_rom = vec![2; 2 * CHR_ROM_PAGE_SIZE];

//             header
//                 .into_iter()
//                 .chain(prg_rom)
//                 .chain(chr_rom)
//                 .collect::<Vec<_>>()
//         })
//         .unwrap();

//         self::MemoryMapper::default_with_rom(rom)
//     }

//     pub(crate) fn page_cross(&self, addr1: u16, addr2: u16) -> bool {
//         addr1 & 0xFF00 != addr2 & 0xFF00
//     }
// }

// impl Memory for MemoryMapper {
//     fn read_u8(&mut self, addr: u16) -> u8 {
//         match addr {
//             0x0000..=0x1FFF => {
//                 let mirror_down_addr = addr & 0b00000111_11111111;
//                 self.vram[mirror_down_addr as usize]
//             }
//             0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
//                 panic!("Attempt to read from write-only PPU address 0x{:04X}", addr);
//             }
//             0x2002 => self.ppu.read_and_clear_status_register().bits(),
//             0x2004 => todo!("Read PPU OAM data"),
//             0x2007 => self.ppu.read_from_data_segment(),
//             0x2008..=0x3FFF => {
//                 let mirror_down_addr = addr & 0b00100000_00000111;
//                 self.read_u8(mirror_down_addr)
//             }
//             0x4000..=0x4015 => {
//                 // TODO: Implement the APU
//                 0xFF
//             }
//             0x8000..=0xFFFF => {
//                 let mut addr = addr - 0x8000;
//                 if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
//                     addr = addr % 0x4000;
//                 }

//                 self.prg_rom[addr as usize]
//             }
//             _ => panic!("Invalid read: 0x{:04X}", addr),
//         }
//     }

//     fn write_u8(&mut self, addr: u16, value: u8) {
//         match addr {
//             0x0000..=0x1FFF => {
//                 let mirror_down_addr = addr & 0b11111111111;
//                 self.vram[mirror_down_addr as usize] = value;
//             }
//             0x2000 => self
//                 .ppu
//                 .write_to_control_register(ControlFlags::from_bits_truncate(value)),
//             0x2001 => self
//                 .ppu
//                 .write_to_mask_register(MaskFlags::from_bits_truncate(value)),
//             0x2002 => panic!("Attempt to write to read-only PPU status register"),
//             0x2005 => {
//                 self.ppu.write_to_scroll_register(value);
//             }
//             0x2006 => {
//                 self.ppu.write_to_data_address_register(value);
//             }
//             0x2007 => {
//                 self.ppu.write_to_data_segment(value);
//             }

//             0x2008..=0x3FFF => {
//                 let mirror_down_addr = addr & 0b00100000_00000111;
//                 self.write_u8(mirror_down_addr, value);
//             }
//             0x4000..=0x4013 | 0x4015 => {
//                 // TODO: Implement the APU
//             }
//             0x8000..=0xFFFF => panic!(
//                 "Attempted to write to cartridge ROM at address {:04X}",
//                 addr
//             ),
//             _ => panic!("Invalid write: {:04X}", addr),
//         }
//     }
// }
