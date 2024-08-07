pub mod memory;
pub mod opcode;

use std::ops::{Deref, DerefMut};

use memory::{Memory, MemoryMapper, Rom};
use opcode::AddressingMode;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    InvalidOpcode { pc: u16, op: u8 },
    InvalidMemoryAccess { addr: u16 },
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::InvalidOpcode { pc, op } => {
                write!(f, "Invalid op 0x{:02X} at PC=0x{:04X}", op, pc)
            }
            Error::InvalidMemoryAccess { addr } => {
                write!(f, "Invalid memory access at 0x{:04X}", addr)
            }
        }
    }
}

pub struct System {
    pub cpu: Cpu,
    pub memory_mapper: MemoryMapper,
}

impl System {
    pub fn new(memory_mapper: MemoryMapper) -> Self {
        let cpu = Cpu::new();
        System { cpu, memory_mapper }
    }

    pub fn with_rom(rom: Rom) -> System {
        let memory_mapper = MemoryMapper::default_with_rom(rom);
        let mut cpu = Cpu::new();
        *cpu.registers.pc = memory_mapper.read_u16(0xFFFC);

        System { cpu, memory_mapper }
    }

    pub fn stack(&mut self) -> Stack {
        Stack(&mut self.memory_mapper, &mut self.cpu.registers.sp)
    }

    pub fn resolve_addr(&self, operands: &[u8], addressing_mode: AddressingMode) -> (u16, bool) {
        match (operands, addressing_mode) {
            ([lo, hi], AddressingMode::Absolute) => (u16::from_le_bytes([*lo, *hi]), false),
            ([_], AddressingMode::Immediate) => (*self.cpu.registers.pc + 1, false),
            ([lo], AddressingMode::ZeroPage) => (*lo as u16, false),
            ([lo], AddressingMode::ZeroPageX) => {
                (lo.wrapping_add(*self.cpu.registers.x) as u16, false)
            }
            ([lo], AddressingMode::ZeroPageY) => {
                (lo.wrapping_add(*self.cpu.registers.y) as u16, false)
            }
            ([lo], AddressingMode::IndirectY) => (
                u16::from_le_bytes([
                    self.memory_mapper.read_u8(*lo as u16),
                    self.memory_mapper.read_u8(lo.wrapping_add(1) as u16),
                ])
                .wrapping_add(*self.cpu.registers.y as u16),
                false,
            ),
            ([lo], AddressingMode::IndirectX) => {
                let addr = lo.wrapping_add(*self.cpu.registers.x);
                let lo = self.memory_mapper.read_u8(addr as u16);
                let hi = self.memory_mapper.read_u8(addr.wrapping_add(1) as u16);
                (u16::from_le_bytes([lo, hi]), false)
            }
            ([lo, hi], AddressingMode::AbsoluteX) => {
                let base = u16::from_le_bytes([*lo, *hi]);
                let addr = base.wrapping_add(*self.cpu.registers.x as u16);
                (addr, self.memory_mapper.page_cross(base, addr))
            }
            ([lo, hi], AddressingMode::AbsoluteY) => {
                let base = u16::from_le_bytes([*lo, *hi]);
                let addr = base.wrapping_add(*self.cpu.registers.y as u16);
                (addr, self.memory_mapper.page_cross(base, addr))
            }
            _ => unimplemented!(
                "Addressing mode {:?} not implemented for operands {:?}",
                addressing_mode,
                operands
            ),
        }
    }

    pub fn interrupt(&mut self, _interrupt: Interrupt) {
        // TODO
    }

    pub fn step(&mut self) -> Result<(), Error> {
        let pc = *self.cpu.registers.pc;
        let op = self.memory_mapper.read_u8(*self.cpu.registers.pc);

        let (opcode, instruction) = opcode::lookup(op);
        let mut instruction_cycles = opcode.cycles;

        let mut operands = [0; 2];
        let operant_count = (opcode.size - 1) as usize;
        for (i, operand) in operands.iter_mut().enumerate().take(operant_count) {
            *operand = self.memory_mapper.read_u8(pc + 1 + i as u16);
        }

        instruction.execute(
            &operands[..operant_count],
            opcode,
            self,
            &mut instruction_cycles,
        );

        if pc == *self.cpu.registers.pc {
            *self.cpu.registers.pc += opcode.size as u16;
        }

        self.cpu.cycles += u64::from(instruction_cycles);

        Ok(())
    }
}

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Register<T = u8>(T);

impl<T> Register<T> {
    pub fn load(&mut self, value: T) {
        self.0 = value;
    }

    pub fn load_with(&mut self, f: impl FnOnce(&mut T)) {
        f(&mut self.0);
    }
}

impl<T> Deref for Register<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Register<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
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

pub enum Interrupt {
    Break,
}

pub struct Stack<'a>(&'a mut MemoryMapper, &'a mut Register<u8>);

impl<'a> Stack<'a> {
    const STACK_BASE: u16 = 0x100;

    fn push_u8(&mut self, value: u8) {
        let sp = **self.1;
        self.0.write_u8(Self::STACK_BASE + sp as u16, value);
        **self.1 -= 1;
    }

    fn pop_u8(&mut self) -> u8 {
        **self.1 += 1;
        let sp = **self.1;
        let value = self.0.read_u8(Self::STACK_BASE + sp as u16);
        value
    }

    fn push_u16(&mut self, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.push_u8(hi);
        self.push_u8(lo);
    }

    fn pop_u16(&mut self) -> u16 {
        let lo = self.pop_u8();
        let hi = self.pop_u8();
        u16::from_le_bytes([lo, hi])
    }
}

#[cfg(test)]
mod tests {
    // use rand::Rng;
    // use reference::Mem;

    // use super::*;

    // #[test]
    // fn test_sanity_check() {
    //     let game_code: Vec<u8> = vec![
    //         0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9,
    //         0x02, 0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85,
    //         0x12, 0xa9, 0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60,
    //         0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60,
    //         0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20,
    //         0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9,
    //         0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04,
    //         0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
    //         0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04,
    //         0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60,
    //         0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d,
    //         0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60,
    //         0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09,
    //         0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60, 0xa6,
    //         0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0,
    //         0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
    //         0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28,
    //         0x60, 0xe6, 0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69,
    //         0x20, 0x85, 0x10, 0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c,
    //         0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35,
    //         0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10,
    //         0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea, 0xea, 0xca, 0xd0, 0xfb,
    //         0x60,
    //     ];

    //     let mut s1 = System::new();
    //     s1.cpu.registers.pc.load(0x0600);
    //     s1.memory.write_all(0x0600, &game_code);
    //     s1.memory.write_u16(0xFFFC, 0x0600);

    //     let mut s2 = reference::CPU::new();
    //     s2.load(game_code);
    //     s2.reset();

    //     let mut rng = rand::thread_rng();

    //     for _ in 0..100000 {
    //         if s1.memory.read_u8(*s1.cpu.registers.pc) == 0x00 {
    //             break;
    //         }

    //         let r: u8 = rng.gen_range(1..16);
    //         s1.memory.write_u8(0xFE, r);
    //         s2.mem_write(0xFE, r);

    //         s2.step();
    //         if let Err(e) = s1.step() {
    //             panic!("{}", e);
    //         }

    //         assert_eq!(*s1.cpu.registers.a, s2.register_a, "A");
    //         assert_eq!(*s1.cpu.registers.x, s2.register_x, "X");
    //         assert_eq!(*s1.cpu.registers.y, s2.register_y, "Y");
    //         assert_eq!(s1.cpu.registers.p.bits(), s2.status.bits(), "P");
    //         assert_eq!(*s1.cpu.registers.sp, s2.stack_pointer, "SP");
    //         assert_eq!(*s1.cpu.registers.pc, s2.program_counter, "PC");
    //     }
    // }
}
