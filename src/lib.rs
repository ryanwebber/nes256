pub mod cpu;
pub mod memory;
pub mod opcode;
pub mod ppu;

use std::ops::{Deref, DerefMut};

use memory::{Memory, Rom};
use opcode::AddressingMode;

use crate::{
    cpu::Cpu,
    memory::{MemoryBus, MemorySnapshot},
    ppu::Ppu,
};

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
    pub ppu: Ppu,
    pub bus: MemoryBus,
}

impl System {
    pub fn new(bus: MemoryBus) -> Self {
        System {
            cpu: Cpu::new(),
            ppu: Ppu::new(bus.rom.mirroring),
            bus,
        }
    }

    pub fn with_rom(rom: Rom) -> System {
        let mut bus = MemoryBus::default_with_rom(rom);
        let mut ppu = Ppu::new(bus.rom.mirroring);
        let mut cpu = Cpu::new();
        *cpu.registers.pc = bus.snapshot(&mut ppu).read_u16(0xFFFC);

        System { cpu, ppu, bus }
    }

    pub fn resolve_addr(
        &mut self,
        operands: &[u8],
        addressing_mode: AddressingMode,
    ) -> (u16, bool) {
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
                    self.memory().read_u8(*lo as u16),
                    self.memory().read_u8(lo.wrapping_add(1) as u16),
                ])
                .wrapping_add(*self.cpu.registers.y as u16),
                false,
            ),
            ([lo], AddressingMode::IndirectX) => {
                let addr = lo.wrapping_add(*self.cpu.registers.x);
                let lo = self.memory().read_u8(addr as u16);
                let hi = self.memory().read_u8(addr.wrapping_add(1) as u16);
                (u16::from_le_bytes([lo, hi]), false)
            }
            ([lo, hi], AddressingMode::AbsoluteX) => {
                let base = u16::from_le_bytes([*lo, *hi]);
                let addr = base.wrapping_add(*self.cpu.registers.x as u16);
                (addr, self.memory().page_cross(base, addr))
            }
            ([lo, hi], AddressingMode::AbsoluteY) => {
                let base = u16::from_le_bytes([*lo, *hi]);
                let addr = base.wrapping_add(*self.cpu.registers.y as u16);
                (addr, self.memory().page_cross(base, addr))
            }
            _ => unimplemented!(
                "Addressing mode {:?} not implemented for operands {:?}",
                addressing_mode,
                operands
            ),
        }
    }

    pub fn interrupt(&mut self, interrupt: Interrupt) {
        match interrupt {
            Interrupt::Break => {
                todo!("Handle break interrupt");
            }
            Interrupt::Nmi => {
                let pc = *self.cpu.registers.pc;
                self.stack().push_u16(pc);

                let mut flags = self.cpu.status_flags();
                flags.set(cpu::Flags::BREAK, false);
                flags.set(cpu::Flags::BREAK2, true);

                self.stack().push_u8(flags.bits());
                self.cpu
                    .status_flags_mut()
                    .insert(cpu::Flags::INTERRUPT_DISABLE);

                self.ppu.step(2, &mut None);
                *self.cpu.registers.pc = self.memory().read_u16(0xFFFA);
            }
        }
    }

    pub fn step(&mut self) -> Result<(), Error> {
        let pc = *self.cpu.registers.pc;
        let op = self.memory().read_u8(pc);

        let (opcode, instruction) = opcode::lookup(op);
        let mut instruction_cycles = opcode.cycles;

        let mut operands = [0; 2];
        let operant_count = (opcode.size - 1) as usize;
        for (i, operand) in operands.iter_mut().enumerate().take(operant_count) {
            *operand = self.memory().read_u8(pc + 1 + i as u16);
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

        // Catch the PPU up
        let mut interrupt: Option<Interrupt> = None;
        self.ppu.step(instruction_cycles as u8, &mut interrupt);

        if let Some(Interrupt::Nmi) = interrupt {
            self.interrupt(Interrupt::Nmi);
        }

        Ok(())
    }

    pub fn stack<'a>(&'a mut self) -> Stack<'a> {
        Stack(self.bus.snapshot(&mut self.ppu), &mut self.cpu.registers.sp)
    }

    pub fn memory<'a>(&'a mut self) -> MemorySnapshot<'a> {
        self.bus.snapshot(&mut self.ppu)
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

    pub fn value(&self) -> T
    where
        T: Copy,
    {
        self.0
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

pub enum Interrupt {
    Break,
    Nmi,
}

pub struct Stack<'a>(MemorySnapshot<'a>, &'a mut Register<u8>);

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
