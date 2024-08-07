use crate::{memory::Mirroring, Register};

pub struct Ppu {
    pub vram: [u8; 2048],
    pub palettes: [u8; 32],
    pub oam_data: [u8; 256],
    pub chr_rom: Vec<u8>,
    pub mirroring: Mirroring,
    pub registers: Registers,
    previous_read: u8,
}

impl Ppu {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        Ppu {
            vram: [0; 2048],
            palettes: [0; 32],
            oam_data: [0; 256],
            chr_rom,
            mirroring,
            registers: Registers {
                address: AddressLatch::new(),
                control: Register(Control::empty()),
            },
            previous_read: 0,
        }
    }

    pub fn read_from_data_segment(&mut self) -> u8 {
        let addr = self.registers.address.read();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.previous_read;
                self.previous_read = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2fff => {
                let result = self.previous_read;
                self.previous_read = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3eff => panic!(
                "Address space 0x3000..0x3eff is not expected to be used, requested = {addr}",
            ),
            0x3f00..=0x3fff => self.palettes[(addr - 0x3f00) as usize],
            _ => panic!("Unexpected access to mirrored space {}", addr),
        }
    }

    pub fn write_to_data_segment(&mut self, value: u8) {
        let addr = self.registers.address.read();
        match addr {
            0..=0x1fff => println!("Attempt to write to CHR ROM space at 0x{:04X}", addr),
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = value;
            }
            0x3000..=0x3eff => unimplemented!("Write to addr 0x{:04X} not supported", addr),
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                let add_mirror = addr - 0x10;
                self.palettes[(add_mirror - 0x3f00) as usize] = value;
            }
            0x3f00..=0x3fff => {
                self.palettes[(addr - 0x3f00) as usize] = value;
            }
            _ => panic!("Unexpected access to mirrored space: 0x{:04X}", addr),
        }

        self.increment_vram_addr();
    }

    fn increment_vram_addr(&mut self) {
        self.registers
            .address
            .increment(self.registers.control.vram_addr_increment());
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0b10111111111111;
        let vram_index = mirrored_vram - 0x2000;
        let name_table = vram_index / 0x400;
        match (&self.mirroring, name_table) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            (Mirroring::Horizontal, 2) => vram_index - 0x400,
            (Mirroring::Horizontal, 1) => vram_index - 0x400,
            (Mirroring::Horizontal, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }
}

pub struct Registers {
    pub address: AddressLatch,
    pub control: Register<Control>,
}

pub struct AddressLatch {
    pub low_byte: u8,
    pub high_byte: u8,
    pub is_high_byte: bool,
}

impl AddressLatch {
    fn new() -> Self {
        AddressLatch {
            low_byte: 0,
            high_byte: 0,
            is_high_byte: true,
        }
    }

    pub fn set(&mut self, value: u16) {
        let [low_byte, high_byte] = value.to_le_bytes();
        self.low_byte = low_byte;
        self.high_byte = high_byte;
    }

    pub fn write(&mut self, byte: u8) {
        if self.is_high_byte {
            self.high_byte = byte;
        } else {
            self.low_byte = byte;
        }

        if self.read() > 0x3FFF {
            self.set(self.read() & 0x3FFF);
        }

        self.is_high_byte = !self.is_high_byte;
    }

    pub fn increment(&mut self, value: u8) {
        let low_byte = self.low_byte;
        self.low_byte = low_byte.wrapping_add(value as u8);

        if low_byte > self.low_byte {
            self.high_byte = self.high_byte.wrapping_add(1);
        }

        if self.read() > 0x3FFF {
            self.set(self.read() & 0x3FFF);
        }
    }

    pub fn read(&self) -> u16 {
        u16::from_le_bytes([self.low_byte, self.high_byte])
    }

    pub fn reset(&mut self) {
        self.is_high_byte = true;
    }
}

bitflags::bitflags! {
   pub struct Control: u8 {
       const NAMETABLE1              = 0b00000001;
       const NAMETABLE2              = 0b00000010;
       const VRAM_ADD_INCREMENT      = 0b00000100;
       const SPRITE_PATTERN_ADDR     = 0b00001000;
       const BACKROUND_PATTERN_ADDR  = 0b00010000;
       const SPRITE_SIZE             = 0b00100000;
       const MASTER_SLAVE_SELECT     = 0b01000000;
       const GENERATE_NMI            = 0b10000000;
   }
}

impl Control {
    pub fn vram_addr_increment(&self) -> u8 {
        if !self.contains(Self::VRAM_ADD_INCREMENT) {
            1
        } else {
            32
        }
    }
}
