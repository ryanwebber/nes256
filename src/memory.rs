use crate::{
    joypad::Joypad,
    ppu::{ControlFlags, MaskFlags, Ppu},
};

const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 0x4000;
const CHR_ROM_PAGE_SIZE: usize = 0x2000;

/// MMC1 Memory Management Controller
/// Supports bank switching for PRG ROM and CHR ROM
pub struct MMC1 {
    // Shift register for loading control values
    shift_register: u8,
    shift_count: u8,

    // Control register (0x8000-0x9FFF)
    control: u8,

    // CHR bank 0 (0xA000-0xBFFF)
    chr_bank0: u8,

    // CHR bank 1 (0xC000-0xDFFF)
    chr_bank1: u8,

    // PRG bank (0xE000-0xFFFF)
    prg_bank: u8,

    // Original ROM data
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    chr_ram: Vec<u8>, // CHR RAM for games that use it
    mirroring: Mirroring,
    has_chr_ram: bool,

    // Reusable CHR buffer to avoid allocations
    chr_buffer: Vec<u8>,
    buffer_dirty: bool, // Track if buffer needs updating
}

impl MMC1 {
    fn new(prg_rom: Vec<u8>, chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        // Check if this is a CHR RAM game (CHR ROM size is 0)
        let has_chr_ram = chr_rom.is_empty();
        let chr_ram = if has_chr_ram {
            vec![0; 0x2000] // 8KB CHR RAM
        } else {
            vec![]
        };

        let mut mmc1 = Self {
            shift_register: 0,
            shift_count: 0,
            control: 0x0C, // Default: 32KB PRG ROM, 8KB CHR ROM, horizontal mirroring
            chr_bank0: 0,
            chr_bank1: 0,
            prg_bank: 0,
            prg_rom,
            chr_rom,
            chr_ram,
            mirroring,
            has_chr_ram,
            chr_buffer: vec![0; 0x2000], // 8KB buffer
            buffer_dirty: true,          // Mark as dirty initially
        };

        // Initialize the buffer immediately with the default bank state
        mmc1.update_chr_buffer();

        mmc1
    }

    fn write_register(&mut self, addr: u16, value: u8) {
        // MMC1 uses a shift register mechanism
        if value & 0x80 != 0 {
            // Reset shift register
            self.shift_register = 0;
            self.shift_count = 0;
            self.control |= 0x0C; // Set default values
            return;
        }

        // Load bit into shift register
        self.shift_register = (self.shift_register >> 1) | ((value & 1) << 4);
        self.shift_count += 1;

        if self.shift_count == 5 {
            // Complete 5-bit value loaded, write to appropriate register
            match addr {
                0x8000..=0x9FFF => {
                    self.control = self.shift_register & 0x1F;
                    // Update mirroring when control register changes
                    self.mirroring = self.get_mirroring();
                    self.buffer_dirty = true; // CHR mode or bank configuration changed
                }
                0xA000..=0xBFFF => {
                    self.chr_bank0 = self.shift_register & 0x1F;
                    self.buffer_dirty = true; // CHR bank 0 changed
                }
                0xC000..=0xDFFF => {
                    self.chr_bank1 = self.shift_register & 0x1F;
                    self.buffer_dirty = true; // CHR bank 1 changed;
                }
                0xE000..=0xFFFF => {
                    self.prg_bank = self.shift_register & 0x0F;
                    // PRG bank changes don't affect CHR buffer
                }
                _ => {}
            }

            // Reset shift register
            self.shift_register = 0;
            self.shift_count = 0;
        }
    }

    fn read_prg(&self, addr: u16) -> u8 {
        let bank_mode = (self.control >> 2) & 0x3;
        let prg_addr = match bank_mode {
            0 | 1 => {
                // 32KB mode: ignore lowest bit of bank number
                let bank = ((self.prg_bank & 0x0E) >> 1) as usize;
                let offset = (addr - 0x8000) as usize;
                bank * PRG_ROM_PAGE_SIZE * 2 + offset
            }
            2 => {
                // Fix first bank at 0x8000, switch second bank
                if addr < 0xC000 {
                    // First bank (fixed)
                    (addr - 0x8000) as usize
                } else {
                    // Second bank (switchable)
                    let bank = (self.prg_bank & 0x0F) as usize;
                    let offset = (addr - 0xC000) as usize;
                    bank * PRG_ROM_PAGE_SIZE + offset
                }
            }
            3 => {
                // Switch first bank, fix last bank
                if addr < 0xC000 {
                    // First bank (switchable)
                    let bank = (self.prg_bank & 0x0F) as usize;
                    let offset = (addr - 0x8000) as usize;
                    bank * PRG_ROM_PAGE_SIZE + offset
                } else {
                    // Last bank (fixed)
                    let last_bank = (self.prg_rom.len() / PRG_ROM_PAGE_SIZE) - 1;
                    let offset = (addr - 0xC000) as usize;
                    last_bank * PRG_ROM_PAGE_SIZE + offset
                }
            }
            _ => unreachable!(),
        };

        if prg_addr < self.prg_rom.len() {
            self.prg_rom[prg_addr]
        } else {
            0xFF // Return 0xFF for out-of-bounds reads
        }
    }

    fn read_chr(&self, addr: u16) -> u8 {
        if self.has_chr_ram {
            // CHR RAM mode
            self.chr_ram[addr as usize]
        } else {
            // CHR ROM mode
            let chr_mode = (self.control >> 4) & 0x1;
            let chr_addr = match chr_mode {
                0 => {
                    // 8KB mode: single 8KB bank
                    let bank = ((self.chr_bank0 & 0x1E) >> 1) as usize;
                    bank * 0x2000 + addr as usize
                }
                1 => {
                    // 4KB mode: two 4KB banks
                    if addr < 0x1000 {
                        // First 4KB bank
                        let bank = (self.chr_bank0 & 0x1F) as usize;
                        bank * 0x1000 + addr as usize
                    } else {
                        // Second 4KB bank
                        let bank = (self.chr_bank1 & 0x1F) as usize;
                        let offset = (addr - 0x1000) as usize;
                        bank * 0x1000 + offset
                    }
                }
                _ => unreachable!(),
            };

            if chr_addr < self.chr_rom.len() {
                self.chr_rom[chr_addr]
            } else {
                0 // Return 0 for out-of-bounds reads
            }
        }
    }

    fn get_mirroring(&self) -> Mirroring {
        match self.control & 0x3 {
            0 => Mirroring::OneScreenLower,
            1 => Mirroring::OneScreenUpper,
            2 => Mirroring::Vertical,
            3 => Mirroring::Horizontal,
            _ => unreachable!(),
        }
    }

    // Update the CHR buffer with current bank state
    fn update_chr_buffer(&mut self) {
        if !self.buffer_dirty {
            return; // Buffer is already up to date
        }

        for i in 0..0x2000 {
            self.chr_buffer[i] = self.read_chr(i as u16);
        }

        self.buffer_dirty = false;
    }

    // Get the current CHR data (updating buffer if needed)
    pub fn get_chr_data(&mut self) -> &[u8] {
        self.update_chr_buffer();
        &self.chr_buffer
    }

    // Public method for testing buffer state
    #[cfg(test)]
    pub fn is_buffer_dirty(&self) -> bool {
        self.buffer_dirty
    }

    #[cfg(test)]
    pub fn get_buffer_value(&self, index: usize) -> u8 {
        self.chr_buffer[index]
    }
}

pub trait Memory {
    fn read_u8(&mut self, addr: u16) -> u8;
    fn write_u8(&mut self, addr: u16, value: u8);

    fn page_cross(&self, addr1: u16, addr2: u16) -> bool {
        addr1 & 0xFF00 != addr2 & 0xFF00
    }

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
        // Require at least the 16-byte header
        if data.len() < 16 {
            return Err("ROM too small");
        }

        if &data[0..4] != NES_TAG {
            return Err("Invalid NES tag");
        }

        let is_nes2 = (data[7] & 0x0C) == 0x08;

        // Mapper number (12-bit in NES 2.0, 8-bit in iNES 1.0)
        let mapper12: u16 = ((data[6] as u16) >> 4)
            | ((data[7] as u16) & 0xF0)
            | if is_nes2 {
                ((data[8] as u16) & 0x0F) << 8
            } else {
                0
            };

        let mapper: u8 = (mapper12 & 0xFF) as u8; // TODO: widen to u16 if needed elsewhere

        // Mirroring (unchanged between formats)
        let four_screen = data[6] & 0b1000 != 0;
        let vertical_mirroring = data[6] & 0b1 != 0;
        let mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        // PRG/CHR page counts (units: 16 KiB and 8 KiB)
        let (prg_pages, chr_pages): (usize, usize) = if is_nes2 {
            // NES 2.0 extends page counts with MSBs in header byte 9
            let prg_msb = (data[9] & 0x0F) as usize;
            let chr_msb = (data[9] >> 4) as usize;
            let prg = (prg_msb << 8) | data[4] as usize;
            let chr = (chr_msb << 8) | data[5] as usize;

            // Note: NES 2.0 also supports an exponent/multiplier encoding for very large sizes.
            // This implementation does not handle that rare case yet.
            (prg, chr)
        } else {
            (data[4] as usize, data[5] as usize)
        };

        let prg_rom_size = prg_pages.saturating_mul(PRG_ROM_PAGE_SIZE);
        let chr_rom_size = chr_pages.saturating_mul(CHR_ROM_PAGE_SIZE);

        let skip_trainer = data[6] & 0b100 != 0;
        let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        // Bounds checking against actual file size
        if prg_rom_start
            .checked_add(prg_rom_size)
            .filter(|&end| end <= data.len())
            .is_none()
        {
            return Err("PRG ROM size exceeds file length");
        }

        if chr_rom_start
            .checked_add(chr_rom_size)
            .filter(|&end| end <= data.len())
            .is_none()
        {
            return Err("CHR ROM size exceeds file length");
        }

        log::info!(
            "Loaded ROM: PRG {} KiB, CHR {} KiB, Mirroring: {:?}, Mapper: {}",
            prg_rom_size / 1024,
            chr_rom_size / 1024,
            mirroring,
            mapper
        );

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
    OneScreenLower,
    OneScreenUpper,
}

pub struct MemoryBus {
    pub ram: [u8; 0x800],
    pub rom: Rom,
    pub mmc1: Option<MMC1>,
}

impl MemoryBus {
    pub fn default_with_rom(rom: Rom) -> Self {
        let mmc1 = if rom.mapper == 1 {
            Some(MMC1::new(
                rom.prg_rom.clone(),
                rom.chr_rom.clone(),
                rom.mirroring,
            ))
        } else {
            None
        };

        MemoryBus {
            ram: [0; 0x800],
            rom,
            mmc1,
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

    pub fn snapshot<'a>(
        &'a mut self,
        ppu: &'a mut Ppu,
        joypad: &'a mut Joypad,
    ) -> MemorySnapshot<'a> {
        MemorySnapshot {
            bus: self,
            ppu,
            joypad,
        }
    }
}

pub struct MemorySnapshot<'a> {
    bus: &'a mut MemoryBus,
    ppu: &'a mut Ppu,
    joypad: &'a mut Joypad,
}

impl Memory for MemorySnapshot<'_> {
    fn read_u8(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => {
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.bus.ram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => {
                log::warn!(
                    "Attempt to read from write-only PPU register at 0x{:04X}",
                    addr
                );
                0
            }
            0x2002 => self.ppu.read_and_clear_status_register().bits(),
            0x2004 => todo!("Read PPU OAM data"),
            0x2007 => {
                if let Some(ref mut mmc1) = self.bus.mmc1 {
                    // Update PPU mirroring if it has changed
                    if mmc1.mirroring != self.bus.rom.mirroring {
                        self.ppu.update_mirroring(mmc1.mirroring);
                    }

                    // Use the reusable buffer (only updates when bank configuration changes)
                    let chr_data = mmc1.get_chr_data();
                    self.ppu.read_from_data_segment(chr_data)
                } else {
                    self.ppu.read_from_data_segment(&self.bus.rom.chr_rom)
                }
            }
            0x2008..=0x3FFF => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.read_u8(mirror_down_addr)
            }
            0x4000..0x4016 => {
                // TODO: Implement the APU
                0xFF
            }
            0x4016 => self.joypad.read(),
            0x4017 => {
                /* Ignore joypad2 */
                0
            }
            0x4018..=0x401F => {
                // TODO: Implement the APU
                0xFF
            }
            0x4020..=0x7FFF => {
                // Expansion ROM - return 0xFF (unused in most games)
                0xFF
            }
            0x8000..=0xFFFF => {
                if let Some(ref mmc1) = self.bus.mmc1 {
                    mmc1.read_prg(addr)
                } else {
                    // Fallback to original logic for non-MMC1 mappers
                    let mut addr = addr - 0x8000;
                    if self.bus.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
                        addr = addr % 0x4000;
                    }

                    self.bus.rom.prg_rom[addr as usize]
                }
            }
        }
    }

    fn write_u8(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => {
                let mirror_down_addr = addr & 0b11111111111;
                self.bus.ram[mirror_down_addr as usize] = value;
            }
            0x2000 => self
                .ppu
                .write_to_control_register(ControlFlags::from_bits_truncate(value)),
            0x2001 => self
                .ppu
                .write_to_mask_register(MaskFlags::from_bits_truncate(value)),
            0x2002 => panic!("Attempt to write to read-only PPU status register"),
            0x2003 => {
                self.ppu.write_to_oam_address_register(value);
            }
            0x2004 => {
                // Write to PPU OAM data
                self.ppu.write_oam_data(value);
            }
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
                let mirror_down_addr = addr & 0x2007;
                self.write_u8(mirror_down_addr, value);
            }
            0x4000..0x4014 => {
                // TODO: Other IO controllers?
            }
            0x4014 => {
                // OAM DMA - read 256 bytes from RAM and write to PPU OAM
                self.ppu.oam_dma_transfer(value, &self.bus.ram);
            }
            0x4015..0x4016 => {
                // TODO: Other IO controllers?
            }
            0x4016 => self.joypad.write(value),
            0x4017 => { /* Ignore joypad2 */ }
            0x4018..=0x401F => {
                // TODO: Implement the APU
            }
            0x4020..=0x7FFF => {
                // Expansion ROM - ignore writes (unused in most games)
            }
            0x8000..=0xFFFF => {
                match self.bus.rom.mapper {
                    0 => {
                        // NROM: ignore writes
                    }
                    1 => {
                        if let Some(ref mut mmc1) = self.bus.mmc1 {
                            mmc1.write_register(addr, value);
                        }
                    }
                    2 => {
                        todo!("Implement UxROM bank switch");
                    }
                    _ => {
                        unimplemented!("Mapper {} not implemented", self.bus.rom.mapper);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mmc1_creation() {
        let prg_rom = vec![0xAA; 0x8000]; // 32KB PRG ROM
        let chr_rom = vec![0xBB; 0x2000]; // 8KB CHR ROM
        let mirroring = Mirroring::Horizontal;

        let mmc1 = MMC1::new(prg_rom.clone(), chr_rom.clone(), mirroring);

        assert_eq!(mmc1.control, 0x0C); // Default control value
        assert_eq!(mmc1.chr_bank0, 0);
        assert_eq!(mmc1.chr_bank1, 0);
        assert_eq!(mmc1.prg_bank, 0);
        assert_eq!(mmc1.mirroring, Mirroring::Horizontal);
    }

    #[test]
    fn test_mmc1_shift_register() {
        let prg_rom = vec![0xAA; 0x8000];
        let chr_rom = vec![0xBB; 0x2000];
        let mut mmc1 = MMC1::new(prg_rom, chr_rom, Mirroring::Horizontal);

        // Write to control register (0x8000-0x9FFF)
        // Write 5 bits: 10101 (0x15)
        mmc1.write_register(0x8000, 1); // Bit 0
        mmc1.write_register(0x8000, 0); // Bit 1
        mmc1.write_register(0x8000, 1); // Bit 2
        mmc1.write_register(0x8000, 0); // Bit 3
        mmc1.write_register(0x8000, 1); // Bit 4

        assert_eq!(mmc1.control, 0x15);
        assert_eq!(mmc1.mirroring, Mirroring::OneScreenUpper); // 0x15 & 0x03 = 0x01
    }

    #[test]
    fn test_mmc1_prg_banking() {
        let mut prg_rom = vec![0; 0x8000]; // 32KB PRG ROM
                                           // Set different values for each 16KB bank
        for i in 0..0x4000 {
            prg_rom[i] = 0xAA; // First bank
            prg_rom[i + 0x4000] = 0xBB; // Second bank
        }

        let chr_rom = vec![0xCC; 0x2000];
        let mut mmc1 = MMC1::new(prg_rom, chr_rom, Mirroring::Horizontal);

        // Set control to mode 2 (fix first bank, switch second bank)
        mmc1.control = 0x08; // 0x08 >> 2 = 0x02

        // Test first bank (fixed at 0x8000-0xBFFF)
        assert_eq!(mmc1.read_prg(0x8000), 0xAA);
        assert_eq!(mmc1.read_prg(0xBFFF), 0xAA);

        // Test second bank (switchable at 0xC000-0xFFFF)
        mmc1.prg_bank = 0x01; // Switch to bank 1
        assert_eq!(mmc1.read_prg(0xC000), 0xBB);
        assert_eq!(mmc1.read_prg(0xFFFF), 0xBB);
    }

    #[test]
    fn test_mmc1_chr_banking() {
        let prg_rom = vec![0xDD; 0x8000];
        let mut chr_rom = vec![0; 0x4000]; // 16KB CHR ROM (2 banks of 8KB)

        // Set different values for each 4KB bank (not 8KB)
        for i in 0..0x1000 {
            chr_rom[i] = 0xAA; // First 4KB bank
            chr_rom[i + 0x1000] = 0xBB; // Second 4KB bank
            chr_rom[i + 0x2000] = 0xCC; // Third 4KB bank
            chr_rom[i + 0x3000] = 0xDD; // Fourth 4KB bank
        }

        let mut mmc1 = MMC1::new(prg_rom, chr_rom, Mirroring::Horizontal);

        // Set control to 4KB CHR mode
        mmc1.control = 0x10; // 0x10 >> 4 = 0x01

        // Test first 4KB bank
        mmc1.chr_bank0 = 0x00;
        assert_eq!(mmc1.read_chr(0x0000), 0xAA);
        assert_eq!(mmc1.read_chr(0x0FFF), 0xAA);

        // Test second 4KB bank
        mmc1.chr_bank1 = 0x01;
        assert_eq!(mmc1.read_chr(0x1000), 0xBB);
        assert_eq!(mmc1.read_chr(0x1FFF), 0xBB);

        // Test third 4KB bank
        mmc1.chr_bank0 = 0x02;
        assert_eq!(mmc1.read_chr(0x0000), 0xCC);
        assert_eq!(mmc1.read_chr(0x0FFF), 0xCC);
    }

    #[test]
    fn test_mmc1_mirroring_modes() {
        let prg_rom = vec![0xEE; 0x8000];
        let chr_rom = vec![0xFF; 0x2000];
        let mut mmc1 = MMC1::new(prg_rom, chr_rom, Mirroring::Horizontal);

        // Test different mirroring modes
        mmc1.control = 0x00; // OneScreenLower
        assert_eq!(mmc1.get_mirroring(), Mirroring::OneScreenLower);

        mmc1.control = 0x01; // OneScreenUpper
        assert_eq!(mmc1.get_mirroring(), Mirroring::OneScreenUpper);

        mmc1.control = 0x02; // Vertical
        assert_eq!(mmc1.get_mirroring(), Mirroring::Vertical);

        mmc1.control = 0x03; // Horizontal
        assert_eq!(mmc1.get_mirroring(), Mirroring::Horizontal);
    }

    #[test]
    fn test_mmc1_chr_ram_support() {
        let prg_rom = vec![0xDD; 0x8000];
        let chr_rom = vec![]; // Empty CHR ROM = CHR RAM mode
        let mmc1 = MMC1::new(prg_rom.clone(), chr_rom, Mirroring::Horizontal);

        // Test that CHR RAM is initialized
        assert!(mmc1.has_chr_ram);
        assert_eq!(mmc1.chr_ram.len(), 0x2000); // 8KB

        // Test reading from CHR RAM (should be initialized to 0)
        assert_eq!(mmc1.read_chr(0x0000), 0);
        assert_eq!(mmc1.read_chr(0x1000), 0);

        // Test CHR ROM mode
        let chr_rom = vec![0xFF; 0x2000];
        let mmc1_rom = MMC1::new(prg_rom.clone(), chr_rom, Mirroring::Horizontal);
        assert!(!mmc1_rom.has_chr_ram);
        assert_eq!(mmc1_rom.read_chr(0x0000), 0xFF);
    }

    #[test]
    fn test_mmc1_reusable_buffer() {
        let prg_rom = vec![0xEE; 0x8000];
        let mut chr_rom = vec![0; 0x4000]; // 16KB CHR ROM

        // Set up test data
        for i in 0..0x2000 {
            chr_rom[i] = 0xAA; // First 8KB bank
            chr_rom[i + 0x2000] = 0xBB; // Second 8KB bank
        }

        let mut mmc1 = MMC1::new(prg_rom, chr_rom, Mirroring::Horizontal);

        // Initial state - buffer should be clean (initialized in constructor)
        assert!(!mmc1.is_buffer_dirty());

        // First call should not update the buffer (already clean)
        mmc1.update_chr_buffer();
        assert!(!mmc1.is_buffer_dirty()); // Buffer should still be clean
        assert_eq!(mmc1.get_buffer_value(0), 0xAA); // Should read from first bank

        // Second call should reuse the buffer (no update needed)
        mmc1.update_chr_buffer();
        assert!(!mmc1.is_buffer_dirty()); // Buffer should still be clean
        assert_eq!(mmc1.get_buffer_value(0), 0xAA); // Should be the same data

        // Change CHR bank configuration using the proper method
        // Write 5 bits: 00010 (0x02) to CHR bank 0 register
        // In 8KB mode, this becomes bank 1 after shifting
        mmc1.write_register(0xA000, 0); // Bit 0
        mmc1.write_register(0xA000, 1); // Bit 1
        mmc1.write_register(0xA000, 0); // Bit 2
        mmc1.write_register(0xA000, 0); // Bit 3
        mmc1.write_register(0xA000, 0); // Bit 4
        assert!(mmc1.is_buffer_dirty()); // Buffer should be marked as dirty

        // Call should update the buffer again
        mmc1.update_chr_buffer();
        assert!(!mmc1.is_buffer_dirty()); // Buffer should be clean again
        assert_eq!(mmc1.get_buffer_value(0), 0xBB); // Should now read from second bank
    }

    #[test]
    fn test_mmc1_4kb_mode() {
        let prg_rom = vec![0xEE; 0x8000];
        let mut chr_rom = vec![0; 0x4000]; // 16KB CHR ROM

        // Set up test data for 4KB banks
        for i in 0..0x1000 {
            chr_rom[i] = 0xAA; // First 4KB bank
            chr_rom[i + 0x1000] = 0xBB; // Second 4KB bank
            chr_rom[i + 0x2000] = 0xCC; // Third 4KB bank
            chr_rom[i + 0x3000] = 0xDD; // Fourth 4KB bank
        }

        let mut mmc1 = MMC1::new(prg_rom, chr_rom, Mirroring::Horizontal);

        // Switch to 4KB mode (control = 0x10)
        // The shift register loads bits from LSB to MSB, so we need to write MSB first
        mmc1.write_register(0x8000, 0); // Bit 4: MSB (4KB mode)
        mmc1.write_register(0x8000, 0); // Bit 3
        mmc1.write_register(0x8000, 0); // Bit 2
        mmc1.write_register(0x8000, 0); // Bit 1
        mmc1.write_register(0x8000, 1); // Bit 0: LSB (4KB mode)

        // Set CHR bank 0 to bank 1 (0x01)
        mmc1.write_register(0xA000, 1); // Bit 0
        mmc1.write_register(0xA000, 0); // Bit 1
        mmc1.write_register(0xA000, 0); // Bit 2
        mmc1.write_register(0xA000, 0); // Bit 3
        mmc1.write_register(0xA000, 0); // Bit 4

        // Set CHR bank 1 to bank 2 (0x02)
        mmc1.write_register(0xC000, 0); // Bit 0
        mmc1.write_register(0xC000, 1); // Bit 1
        mmc1.write_register(0xC000, 0); // Bit 2
        mmc1.write_register(0xC000, 0); // Bit 3
        mmc1.write_register(0xC000, 0); // Bit 4

        // Update buffer and check results
        mmc1.update_chr_buffer();

        // First 4KB should be from bank 1 (0xBB)
        assert_eq!(mmc1.get_buffer_value(0), 0xBB);
        assert_eq!(mmc1.get_buffer_value(0x0FFF), 0xBB);

        // Second 4KB should be from bank 2 (0xCC)
        assert_eq!(mmc1.get_buffer_value(0x1000), 0xCC);
        assert_eq!(mmc1.get_buffer_value(0x1FFF), 0xCC);

        // Verify the mode is correct
        assert_eq!((mmc1.control >> 4) & 0x1, 1); // 4KB mode
    }
}
