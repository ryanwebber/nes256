use crate::Register;

pub struct Cpu {
    pub cycles: u64,
    pub registers: Registers,
    pub halted: bool,
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            cycles: 0,
            registers: Registers {
                a: Register(0),
                x: Register(0),
                y: Register(0),
                p: Register(Flags::INTERRUPT_DISABLE | Flags::BREAK2),
                sp: Register(0xFD),
                pc: Register(0),
            },
            halted: false,
        }
    }

    pub fn update_flags(&mut self, mask: Flags, value: u8) {
        if mask.contains(Flags::ZERO) {
            self.registers.p.0.set(Flags::ZERO, value == 0);
        }

        if mask.contains(Flags::NEGATIVE) {
            self.registers.p.0.set(Flags::NEGATIVE, value & 0x80 != 0);
        }
    }

    pub fn status_flags(&self) -> Flags {
        self.registers.p.0
    }

    pub fn status_flags_mut(&mut self) -> &mut Flags {
        &mut self.registers.p.0
    }

    pub fn update_register_with_flags(
        &mut self,
        register: RegisterIndex,
        mask: Flags,
        f: impl FnOnce(&mut u8),
    ) {
        let register = match register {
            RegisterIndex::A => &mut self.registers.a,
            RegisterIndex::X => &mut self.registers.x,
            RegisterIndex::Y => &mut self.registers.y,
        };

        f(&mut *register);

        let value = **register;
        self.update_flags(mask, value);
    }

    pub fn set_register_with_flags(&mut self, register: RegisterIndex, mask: Flags, value: u8) {
        self.update_register_with_flags(register, mask, |r| *r = value);
    }

    pub fn halt(&mut self) {
        self.halted = true;
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Registers {
    pub a: Register,
    pub x: Register,
    pub y: Register,
    pub sp: Register,
    pub pc: Register<u16>,
    pub p: Register<Flags>,
}

pub enum RegisterIndex {
    A,
    X,
    Y,
}

bitflags::bitflags! {
    #[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Flags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL_MODE      = 0b00001000;
        const BREAK             = 0b00010000;
        const BREAK2            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIVE          = 0b10000000;

        const ZERO_AND_NEGATIVE = Self::ZERO.bits() | Self::NEGATIVE.bits();
    }
}
