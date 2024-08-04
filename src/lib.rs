use std::ops::{Deref, DerefMut};

use opcode::AddressingMode;

mod opcode;

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
    pub memory: Memory,
    pub cycles: u64,
}

impl System {
    pub fn new() -> Self {
        let memory = Memory::zeros();
        let cpu = Cpu::new();
        System {
            cpu,
            memory,
            cycles: 0,
        }
    }

    pub fn with_program(program: &[u8]) -> System {
        let memory = {
            let prog_size = program.len();
            let mut memory = Memory::zeros();
            memory.addr_space[0x8000..0x8000 + prog_size].copy_from_slice(program);
            memory.write_u16(0xFFFC, 0x8000);
            memory
        };

        let mut cpu = Cpu::new();
        *cpu.registers.pc = memory.read_u16(0xFFFC);

        System {
            cpu,
            memory,
            cycles: 0,
        }
    }

    pub fn stack(&mut self) -> Stack {
        Stack(&mut self.memory, &mut self.cpu.registers.sp)
    }

    pub fn resolve_addr(
        &self,
        operands: [u8; 2],
        addressing_mode: Option<AddressingMode>,
    ) -> (u16, bool) {
        match addressing_mode {
            Some(AddressingMode::Absolute) => {
                (u16::from_le_bytes([operands[0], operands[1]]), false)
            }
            Some(AddressingMode::Immediate) => (*self.cpu.registers.pc + 1, false),
            Some(AddressingMode::ZeroPage) => {
                (self.memory.read_u8(operands[0] as u16) as u16, false)
            }
            _ => unimplemented!("Addressing mode not implemented: {:?}", addressing_mode),
        }
    }

    pub fn interrupt(&mut self, _interrupt: Interrupt) {
        // TODO
    }

    pub fn update_accumulator_with_flags(&mut self, mask: Flags, f: impl FnOnce(&mut u8)) {
        Registers::set_with_flags(
            &mut self.cpu.registers.a,
            &mut self.cpu.registers.p,
            mask,
            f,
        );
    }

    pub fn set_accumulator_with_flags(&mut self, mask: Flags, value: u8) {
        Registers::set_with_flags(
            &mut self.cpu.registers.a,
            &mut self.cpu.registers.p,
            mask,
            |a| *a = value,
        );
    }

    pub fn step(&mut self) -> Result<(), Error> {
        let pc = *self.cpu.registers.pc;
        let op = self.memory.read_u8(*self.cpu.registers.pc);

        let (opcode, instruction) =
            opcode::lookup(self.memory.read_u8(pc)).ok_or(Error::InvalidOpcode { pc, op })?;

        let operands = [self.memory.read_u8(pc + 1), self.memory.read_u8(pc + 2)];
        let mut instruction_cycles = opcode.cycles;

        {
            print!(
                "[{:06}] PC=0x{:04X} {} OP=0x{:02X}",
                self.cycles, pc, instruction.mnemonic, op
            );
            for operand in operands.iter().take(opcode.size as usize - 1) {
                print!(" 0x{:02X}", operand);
            }

            println!();
        }

        instruction.execute(operands, opcode, self, &mut instruction_cycles);

        // If PC was not modified by the instruction, increment it here
        // to the next instruction (op_code.size - 1 because we already incremented it once)
        if pc == *self.cpu.registers.pc {
            *self.cpu.registers.pc += opcode.size as u16;
        }

        self.cycles += u64::from(instruction_cycles);

        Ok(())
    }
}

pub struct Cpu {
    registers: Registers,
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            registers: Registers {
                a: Register(0),
                x: Register(0),
                y: Register(0),
                p: Register(Flags::empty()),
                sp: Register(0xFD),
                pc: Register(0),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Registers {
    pub a: Register,
    pub x: Register,
    pub y: Register,
    pub p: Register<Flags>,
    pub sp: Register<u16>,
    pub pc: Register<u16>,
}

impl Registers {
    pub fn set_with_flags(
        register: &mut Register<u8>,
        flags: &mut Register<Flags>,
        mask: Flags,
        f: impl FnOnce(&mut u8),
    ) {
        f(&mut *register);

        if mask.contains(Flags::ZERO) {
            flags.0.set(Flags::ZERO, register.0 == 0);
        }

        if mask.contains(Flags::NEGATIVE) {
            flags.0.set(Flags::NEGATIVE, register.0 & 0x80 != 0);
        }
    }
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

pub struct Memory {
    addr_space: [u8; 0xFFFF],
}

impl Memory {
    pub fn zeros() -> Memory {
        Memory {
            addr_space: [0; 0xFFFF],
        }
    }

    pub fn read_u8(&self, addr: u16) -> u8 {
        self.addr_space[addr as usize]
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read_u8(addr);
        let hi = self.read_u8(addr + 1);
        u16::from_le_bytes([lo, hi])
    }

    pub fn write_u8(&mut self, addr: u16, value: u8) {
        self.addr_space[addr as usize] = value;
    }

    pub fn write_u16(&mut self, addr: u16, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.write_u8(addr, lo);
        self.write_u8(addr + 1, hi);
    }

    pub fn write_all(&mut self, addr: u16, data: &[u8]) {
        self.addr_space[addr as usize..addr as usize + data.len()].copy_from_slice(data);
    }
}

pub struct Stack<'a>(&'a mut Memory, &'a mut Register<u16>);

impl<'a> Stack<'a> {
    const STACK_BASE: u16 = 0x100;

    fn push_u8(&mut self, value: u8) {
        **self.1 -= 1;
        let sp = **self.1;
        self.0.write_u8(Self::STACK_BASE + sp, value);
    }

    fn pop_u8(&mut self) -> u8 {
        let sp = **self.1;
        let value = self.0.read_u8(Self::STACK_BASE + sp);
        **self.1 += 1;
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

pub enum Interrupt {
    Break,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sanity_check() {
        let game_code: Vec<u8> = vec![
            0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9,
            0x02, 0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85,
            0x12, 0xa9, 0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60,
            0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60,
            0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20,
            0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9,
            0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04,
            0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
            0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04,
            0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60,
            0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d,
            0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60,
            0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09,
            0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60, 0xa6,
            0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0,
            0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
            0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28,
            0x60, 0xe6, 0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69,
            0x20, 0x85, 0x10, 0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c,
            0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35,
            0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10,
            0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea, 0xea, 0xca, 0xd0, 0xfb,
            0x60,
        ];

        let mut system = System::new();
        system.cpu.registers.pc.load(0x0600);
        system.memory.write_all(0x0600, &game_code);
        system.memory.write_u16(0xFFFC, 0x0600);

        for _ in 0..1000 {
            if let Err(e) = system.step() {
                panic!("{}", e);
            }
        }

        assert!(false)
    }
}
