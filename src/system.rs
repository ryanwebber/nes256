use crate::common::Register;
use crate::joypad::Joypad;
use crate::memory::{Memory, Rom};
use crate::opcode::AddressingMode;
use crate::trace::Trace;
use crate::{cpu, opcode};

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
    pub joypad1: Joypad,
    pub bus: MemoryBus,
}

impl System {
    pub fn new(bus: MemoryBus) -> Self {
        let mut system = System {
            cpu: Cpu::new(),
            ppu: Ppu::new(bus.rom.mirroring),
            joypad1: Joypad::new(),
            bus,
        };

        // Read the reset vector from the end of address space
        let lo = system.memory().read_u8(0xFFFC);
        let hi = system.memory().read_u8(0xFFFD);
        let pc = u16::from_le_bytes([lo, hi]);
        *system.cpu.registers.pc = pc;

        log::info!("Reset vector: 0x{:04X}", pc);

        // Account for the initial reset cycle count
        system.cpu.cycles = 7;
        system.ppu.cycles = 21;

        system
    }

    pub fn with_rom(rom: Rom) -> System {
        let bus: MemoryBus = MemoryBus::default_with_rom(rom);
        Self::new(bus)
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
            ([lo], AddressingMode::IndirectY) => {
                let zp_addr = *lo;
                let base_lo = self.memory().read_u8(zp_addr as u16);
                let base_hi = self.memory().read_u8(zp_addr.wrapping_add(1) as u16); // wrap around in zero page
                let base = u16::from_le_bytes([base_lo, base_hi]);

                let y = *self.cpu.registers.y;
                let final_addr = base.wrapping_add(y as u16);
                let page_cross = (base & 0xFF00) != (final_addr & 0xFF00);

                (final_addr, page_cross)
            }
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
                self.cpu.halt();
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

                self.ppu.step(2);
                *self.cpu.registers.pc = self.memory().read_u16(0xFFFA);
            }
        }
    }

    pub fn step(&mut self) -> Result<(), Error> {
        if self.cpu.halted {
            return Ok(());
        }

        if self.ppu.poll_buffered_nmi() {
            self.interrupt(Interrupt::Nmi);
        }

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
        self.ppu.step(instruction_cycles as u8);

        Ok(())
    }

    pub fn stack<'a>(&'a mut self) -> Stack<'a> {
        Stack(
            self.bus.snapshot(&mut self.ppu, &mut self.joypad1),
            &mut self.cpu.registers.sp,
        )
    }

    pub fn memory<'a>(&'a mut self) -> MemorySnapshot<'a> {
        self.bus.snapshot(&mut self.ppu, &mut self.joypad1)
    }

    pub fn trace<'a>(&'a mut self) -> Trace<'a> {
        Trace::new(self)
    }
}

pub enum Interrupt {
    Break,
    Nmi,
}

pub struct Stack<'a>(MemorySnapshot<'a>, &'a mut Register<u8>);

impl<'a> Stack<'a> {
    const STACK_BASE: u16 = 0x100;

    pub fn push_u8(&mut self, value: u8) {
        let sp = **self.1;
        self.0.write_u8(Self::STACK_BASE + sp as u16, value);
        **self.1 -= 1;
    }

    pub fn pop_u8(&mut self) -> u8 {
        **self.1 += 1;
        let sp = **self.1;
        let value = self.0.read_u8(Self::STACK_BASE + sp as u16);
        value
    }

    pub fn push_u16(&mut self, value: u16) {
        let [lo, hi] = value.to_le_bytes();
        self.push_u8(hi);
        self.push_u8(lo);
    }

    pub fn pop_u16(&mut self) -> u16 {
        let lo = self.pop_u8();
        let hi = self.pop_u8();
        u16::from_le_bytes([lo, hi])
    }
}
