use crate::{common::Register, memory::Mirroring, system::Interrupt};

pub struct Ppu {
    pub vram: [u8; 2048],
    pub palettes: [u8; 32],
    pub oam_data: [u8; 256],
    pub mirroring: Mirroring,
    pub scanline: u16,
    pub cycles: usize,
    registers: Registers,
    previous_read: u8,
    buffered_nmi: bool,
}

impl Ppu {
    pub fn new(mirroring: Mirroring) -> Self {
        Ppu {
            vram: [0; 2048],
            palettes: [0; 32],
            oam_data: [0; 256],
            mirroring,
            scanline: 0,
            cycles: 0,
            previous_read: 0,
            buffered_nmi: false,
            registers: Registers {
                address: Register(AddressLatch::new()),
                scroll: Register(Scroll::new()),
                control: Register(ControlFlags::empty()),
                mask: Register(MaskFlags::empty()),
                status: Register(StatusFlags::empty()),
            },
        }
    }

    pub fn write_to_control_register(&mut self, value: ControlFlags) {
        self.registers.control.load(value);

        let before_nmi_status = self.registers.control.contains(ControlFlags::GENERATE_NMI);
        self.registers.control.load(value);
        if !before_nmi_status
            && self.registers.control.contains(ControlFlags::GENERATE_NMI)
            && self.registers.status.contains(StatusFlags::VBLANK_STARTED)
        {
            self.buffered_nmi = true;
        }
    }

    pub fn write_to_mask_register(&mut self, value: MaskFlags) {
        self.registers.mask.load(value);
    }

    pub fn write_to_scroll_register(&mut self, value: u8) {
        self.registers.scroll.write(value);
    }

    pub fn write_to_data_address_register(&mut self, value: u8) {
        self.registers.address.write(value);
    }

    pub fn write_to_data_segment(&mut self, value: u8) {
        let addr = self.registers.address.read();
        match addr {
            0..=0x1fff => println!("Attempt to write to CHR ROM space at 0x{:04X}", addr),
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = value;
            }
            0x3000..=0x3eff => unimplemented!("Invalid write to PPU at addr 0x{:04X}", addr),
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

    pub fn read_and_clear_status_register(&mut self) -> StatusFlags {
        let status = self.registers.status.value();
        self.registers.status.remove(StatusFlags::VBLANK_STARTED);
        self.registers.status.remove(StatusFlags::SPRITE_ZERO_HIT);
        self.registers.status.remove(StatusFlags::SPRITE_OVERFLOW);
        status
    }

    pub fn read_from_data_segment(&mut self, chr_rom: &[u8]) -> u8 {
        let addr = self.registers.address.read();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.previous_read;
                self.previous_read = chr_rom[addr as usize];
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

    fn increment_vram_addr(&mut self) {
        self.registers
            .address
            .increment(self.registers.control.vram_addr_increment());
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0x2FFF;
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

    pub fn step(&mut self, cycles: u8, interrupt: &mut Option<Interrupt>) {
        let buffered_nmi = std::mem::replace(&mut self.buffered_nmi, false);
        self.cycles += (cycles as usize) * 3;
        if self.cycles >= 341 {
            self.cycles = self.cycles - 341;
            self.scanline += 1;

            if self.scanline == 241 {
                if self.registers.control.contains(ControlFlags::GENERATE_NMI) {
                    self.registers.status.insert(StatusFlags::VBLANK_STARTED);
                    *interrupt = Some(Interrupt::Nmi);
                }
            } else if self.scanline >= 262 {
                self.scanline = 0;
                self.registers.status.remove(StatusFlags::VBLANK_STARTED);
            }
        }

        if buffered_nmi {
            *interrupt = Some(Interrupt::Nmi);
        }
    }

    pub fn status_flags(&self) -> StatusFlags {
        self.registers.status.value()
    }
}

pub struct Registers {
    pub address: Register<AddressLatch>,
    pub scroll: Register<Scroll>,
    pub control: Register<ControlFlags>,
    pub mask: Register<MaskFlags>,
    pub status: Register<StatusFlags>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
        let (low_byte, overflow) = self.low_byte.overflowing_add(value as u8);
        self.low_byte = low_byte;

        if overflow {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Scroll {
    pub scroll_x: u8,
    pub scroll_y: u8,
    pub latch: bool,
}

impl Scroll {
    fn new() -> Self {
        Scroll {
            scroll_x: 0,
            scroll_y: 0,
            latch: false,
        }
    }

    pub fn write(&mut self, value: u8) {
        if self.latch {
            self.scroll_y = value;
        } else {
            self.scroll_x = value;
        }

        self.latch = !self.latch;
    }

    pub fn reset(&mut self) {
        self.latch = false;
    }
}

bitflags::bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
   pub struct ControlFlags: u8 {
       const NAMETABLE1              = 0b00000001;
       const NAMETABLE2              = 0b00000010;
       const VRAM_ADD_INCREMENT      = 0b00000100;
       const SPRITE_PATTERN_ADDR     = 0b00001000;
       const BACKROUND_PATTERN_ADDR  = 0b00010000;
       const SPRITE_SIZE             = 0b00100000;
       const MASTER_SLAVE_SELECT     = 0b01000000;
       const GENERATE_NMI            = 0b10000000;
   }

   #[derive(Debug, PartialEq, Eq, Clone, Copy)]
   pub struct MaskFlags: u8 {
        const GREYSCALE                = 0b00000001;
        const LEFTMOST_8PXL_BACKGROUND = 0b00000010;
        const LEFTMOST_8PXL_SPRITE     = 0b00000100;
        const SHOW_BACKGROUND          = 0b00001000;
        const SHOW_SPRITES             = 0b00010000;
        const EMPHASISE_RED            = 0b00100000;
        const EMPHASISE_GREEN          = 0b01000000;
        const EMPHASISE_BLUE           = 0b10000000;
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub struct StatusFlags: u8 {
        const UNUSED           = 0b00000001;
        const UNUSED2          = 0b00000010;
        const UNUSED3          = 0b00000100;
        const UNUSED4          = 0b00001000;
        const UNUSED5          = 0b00010000;
        const SPRITE_OVERFLOW  = 0b00100000;
        const SPRITE_ZERO_HIT  = 0b01000000;
        const VBLANK_STARTED   = 0b10000000;
    }
}

impl ControlFlags {
    pub fn vram_addr_increment(&self) -> u8 {
        if !self.contains(Self::VRAM_ADD_INCREMENT) {
            1
        } else {
            32
        }
    }
}
