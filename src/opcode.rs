use std::collections::HashMap;

use crate::{memory::Memory, Flags, Interrupt, RegisterIndex, System};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    Unsupported,
}

pub struct OpCode {
    pub size: u8,
    pub cycles: u8,
    pub addressing_mode: AddressingMode,
}

pub struct Instruction {
    pub mnemonic: &'static str,
    implementation: fn(&[u8], &OpCode, &mut System, &mut u8),
}

impl Instruction {
    pub fn execute(&self, operands: &[u8], opcode: &OpCode, system: &mut System, cycles: &mut u8) {
        (self.implementation)(operands, opcode, system, cycles);
    }
}

pub fn lookup(opcode: u8) -> Option<(&'static OpCode, &'static Instruction)> {
    OPCODE_TABLE.get(&opcode).copied()
}

lazy_static::lazy_static! {
    static ref OPCODE_TABLE: HashMap<u8, (&'static OpCode, &'static Instruction)> = {
        let mut table = HashMap::new();
        for (opcode, opcode_info, instruction) in INSTRUCTIONS {
            if table.insert(*opcode, (opcode_info, instruction)).is_some() {
                panic!("Duplicate opcode: 0x{:02x}", opcode);
            }
        }

        table
    };
}

const INSTRUCTIONS: &[(u8, OpCode, Instruction)] = &[
    (
        0x00,
        OpCode {
            size: 1,
            cycles: 7,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BRK",
            implementation: |_, _, system, _| {
                system.interrupt(Interrupt::Break);
            },
        },
    ),
    (
        0x01,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::ORA,
    ),
    (
        0x04,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x05,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::ORA,
    ),
    (
        0x06,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::ASL,
    ),
    (
        0x08,
        OpCode {
            size: 1,
            cycles: 3,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "PHP",
            implementation: |_, _, system, _| {
                let mut p = *system.cpu.registers.p;
                p.set(Flags::BREAK, true);
                p.set(Flags::BREAK2, true);
                system.stack().push_u8(p.bits());
            },
        },
    ),
    (
        0x09,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::ORA,
    ),
    (
        0x0A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        instructions::ASL,
    ),
    (
        0x0C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x0D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::ORA,
    ),
    (
        0x0E,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::ASL,
    ),
    (
        0x10,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BPL",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    !system.cpu.registers.p.contains(crate::Flags::NEGATIVE)
                });
            },
        },
    ),
    (
        0x11,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::ORA,
    ),
    (
        0x14,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x15,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::ORA,
    ),
    (
        0x16,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::ASL,
    ),
    (
        0x18,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "CLC",
            implementation: |_, _, system, _| {
                *system.cpu.registers.p &= !crate::Flags::CARRY;
            },
        },
    ),
    (
        0x19,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::ORA,
    ),
    (
        0x1A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x1C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x1D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::ORA,
    ),
    (
        0x1E,
        OpCode {
            size: 3,
            cycles: 7,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::ASL,
    ),
    (
        0x20,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "JSR",
            implementation: |operands, _, system, _| {
                let pc = *system.cpu.registers.pc;
                let (addr, ..) = system.resolve_addr(operands, AddressingMode::Absolute);
                system.stack().push_u16(pc + 2);
                system.cpu.registers.pc.load(addr);
            },
        },
    ),
    (
        0x21,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::AND,
    ),
    (
        0x24,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::BIT,
    ),
    (
        0x25,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::AND,
    ),
    (
        0x26,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::ROL,
    ),
    (
        0x28,
        OpCode {
            size: 1,
            cycles: 4,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "PLP",
            implementation: |_, _, system, _| {
                let value = system.stack().pop_u8();
                let mut flags = Flags::from_bits_truncate(value);

                // https://www.nesdev.org/wiki/Status_flags#The_B_flag
                flags.set(Flags::BREAK, false);
                flags.set(Flags::BREAK2, true);

                system.cpu.registers.p.load(flags);
            },
        },
    ),
    (
        0x29,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::AND,
    ),
    (
        0x2A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        instructions::ROL,
    ),
    (
        0x2C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::BIT,
    ),
    (
        0x2D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::AND,
    ),
    (
        0x2E,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::ROL,
    ),
    (
        0x30,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BMI",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    system.cpu.registers.p.contains(crate::Flags::NEGATIVE)
                });
            },
        },
    ),
    (
        0x31,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::AND,
    ),
    (
        0x34,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x35,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::AND,
    ),
    (
        0x36,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::ROL,
    ),
    (
        0x38,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "SEC",
            implementation: |_, _, system, _| {
                system.cpu.registers.p.set(Flags::CARRY, true);
            },
        },
    ),
    (
        0x39,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::AND,
    ),
    (
        0x3A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x3C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x3D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::AND,
    ),
    (
        0x3E,
        OpCode {
            size: 3,
            cycles: 7,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::ROL,
    ),
    (
        0x40,
        OpCode {
            size: 1,
            cycles: 6,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "RTI",
            implementation: |_, _, system, _| {
                let value = system.stack().pop_u8();
                let mut flags = Flags::from_bits_truncate(value);
                flags.set(Flags::BREAK, false);
                flags.set(Flags::BREAK2, true);
                system.cpu.registers.p.load(flags);

                let pc = system.stack().pop_u16();
                system.cpu.registers.pc.load(pc);
            },
        },
    ),
    (
        0x41,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::EOR,
    ),
    (
        0x44,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x45,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::EOR,
    ),
    (
        0x46,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::LSR,
    ),
    (
        0x48,
        OpCode {
            size: 1,
            cycles: 3,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "PHA",
            implementation: |_, _, system, _| {
                let a = *system.cpu.registers.a;
                system.stack().push_u8(a);
            },
        },
    ),
    (
        0x49,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::EOR,
    ),
    (
        0x4A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        instructions::LSR,
    ),
    (
        0x4C,
        OpCode {
            size: 3,
            cycles: 3,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "JMP",
            implementation: |operands, _, system, _| {
                let (addr, _) = system.resolve_addr(operands, AddressingMode::Absolute);
                system.cpu.registers.pc.load(addr);
            },
        },
    ),
    (
        0x4D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::EOR,
    ),
    (
        0x4E,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::LSR,
    ),
    (
        0x50,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BVC",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    !system.cpu.registers.p.contains(crate::Flags::OVERFLOW)
                });
            },
        },
    ),
    (
        0x51,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::EOR,
    ),
    (
        0x54,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x55,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::EOR,
    ),
    (
        0x56,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::LSR,
    ),
    (
        0x59,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::EOR,
    ),
    (
        0x5A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x5C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x5D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::EOR,
    ),
    (
        0x5E,
        OpCode {
            size: 3,
            cycles: 7,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::LSR,
    ),
    (
        0x60,
        OpCode {
            size: 1,
            cycles: 6,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "RTS",
            implementation: |_, _, system, _| {
                let pc = system.stack().pop_u16() + 1;
                system.cpu.registers.pc.load(pc);
            },
        },
    ),
    (
        0x61,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::ADC,
    ),
    (
        0x64,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x65,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::ADC,
    ),
    (
        0x66,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::ROR,
    ),
    (
        0x68,
        OpCode {
            size: 1,
            cycles: 4,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "PLA",
            implementation: |_, _, system, _| {
                let value = system.stack().pop_u8();
                system.cpu.set_register_with_flags(
                    RegisterIndex::A,
                    Flags::ZERO_AND_NEGATIVE,
                    value,
                );
            },
        },
    ),
    (
        0x69,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::ADC,
    ),
    (
        0x6A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        instructions::ROR,
    ),
    (
        0x6C,
        OpCode {
            size: 3,
            cycles: 5,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "JMP",
            implementation: |operands, _, system, _| {
                let (addr, _) = system.resolve_addr(operands, AddressingMode::Absolute);

                // I don't fully understand this, so it may be incorrect
                let jmp_addr = if addr & 0x00FF == 0x00FF {
                    let lo = system.memory_mapper.read_u8(addr);
                    let hi = system.memory_mapper.read_u8(addr & 0xFF00);
                    u16::from_le_bytes([lo, hi])
                } else {
                    system.memory_mapper.read_u16(addr)
                };

                system.cpu.registers.pc.load(jmp_addr);
            },
        },
    ),
    (
        0x6D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::ADC,
    ),
    (
        0x6E,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::ROR,
    ),
    (
        0x70,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BVS",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    system.cpu.registers.p.contains(crate::Flags::OVERFLOW)
                });
            },
        },
    ),
    (
        0x71,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::ADC,
    ),
    (
        0x74,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x75,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::ADC,
    ),
    (
        0x76,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::ROR,
    ),
    (
        0x78,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "SEI",
            implementation: |_, _, system, _| {
                system.cpu.registers.p.set(Flags::INTERRUPT_DISABLE, true);
            },
        },
    ),
    (
        0x79,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::ADC,
    ),
    (
        0x7A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x7C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x7D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::ADC,
    ),
    (
        0x7E,
        OpCode {
            size: 3,
            cycles: 7,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::ROR,
    ),
    (
        0x80,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0x81,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::STA,
    ),
    (
        0x84,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::STY,
    ),
    (
        0x85,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::STA,
    ),
    (
        0x86,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::STX,
    ),
    (
        0x88,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "DEY",
            implementation: |_, _, system, _| {
                system.cpu.update_register_with_flags(
                    RegisterIndex::Y,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |y| *y = y.wrapping_sub(1),
                );
            },
        },
    ),
    (
        0x8A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "TXA",
            implementation: |_, _, system, _| {
                let x = *system.cpu.registers.x;
                system.cpu.update_register_with_flags(
                    RegisterIndex::A,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |a| *a = x,
                );
            },
        },
    ),
    (
        0x8C,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::STY,
    ),
    (
        0x8D,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::STA,
    ),
    (
        0x8E,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::STX,
    ),
    (
        0x90,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BCC",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    !system.cpu.registers.p.contains(crate::Flags::CARRY)
                });
            },
        },
    ),
    (
        0x91,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::STA,
    ),
    (
        0x94,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::STY,
    ),
    (
        0x95,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::STA,
    ),
    (
        0x96,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageY,
        },
        instructions::STX,
    ),
    (
        0x98,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "TYA",
            implementation: |_, _, system, _| {
                let y = *system.cpu.registers.y;
                system.cpu.update_register_with_flags(
                    RegisterIndex::A,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |a| *a = y,
                );
            },
        },
    ),
    (
        0x99,
        OpCode {
            size: 3,
            cycles: 5,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::STA,
    ),
    (
        0x9A,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "TXS",
            implementation: |_, _, system, _| {
                let x = *system.cpu.registers.x;
                system.cpu.registers.sp.load(x);
            },
        },
    ),
    (
        0x9D,
        OpCode {
            size: 3,
            cycles: 5,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::STA,
    ),
    (
        0xA0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::LDY,
    ),
    (
        0xA1,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::LDA,
    ),
    (
        0xA2,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::LDX,
    ),
    (
        0xA4,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::LDY,
    ),
    (
        0xA5,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::LDA,
    ),
    (
        0xA6,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::LDX,
    ),
    (
        0xA8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "TAY",
            implementation: |_, _, system, _| {
                let a = *system.cpu.registers.a;
                system.cpu.update_register_with_flags(
                    RegisterIndex::Y,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |y| *y = a,
                );
            },
        },
    ),
    (
        0xA9,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::LDA,
    ),
    (
        0xAA,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "TAX",
            implementation: |_, _, system, _| {
                let a = *system.cpu.registers.a;
                system.cpu.update_register_with_flags(
                    RegisterIndex::X,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |x| *x = a,
                );
            },
        },
    ),
    (
        0xAC,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::LDY,
    ),
    (
        0xAD,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::LDA,
    ),
    (
        0xAE,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::LDX,
    ),
    (
        0xB0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BCS",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    system.cpu.registers.p.contains(crate::Flags::CARRY)
                });
            },
        },
    ),
    (
        0xB1,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::LDA,
    ),
    (
        0xB4,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::LDY,
    ),
    (
        0xB5,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::LDA,
    ),
    (
        0xB6,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageY,
        },
        instructions::LDX,
    ),
    (
        0xB8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "CLV",
            implementation: |_, _, system, _| {
                system.cpu.registers.p.set(Flags::OVERFLOW, false);
            },
        },
    ),
    (
        0xB9,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::LDA,
    ),
    (
        0xBA,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "TSX",
            implementation: |_, _, system, _| {
                let sp = *system.cpu.registers.sp;
                system.cpu.update_register_with_flags(
                    RegisterIndex::X,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |x| *x = sp,
                );
            },
        },
    ),
    (
        0xBC,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::LDY,
    ),
    (
        0xBD,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::LDA,
    ),
    (
        0xBE,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::LDX,
    ),
    (
        0xC0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::CPY,
    ),
    (
        0xC1,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::CMP,
    ),
    (
        0xC4,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::CPY,
    ),
    (
        0xC5,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::CMP,
    ),
    (
        0xC6,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::DEC,
    ),
    (
        0xC8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "INY",
            implementation: |_, _, system, _| {
                system.cpu.update_register_with_flags(
                    RegisterIndex::Y,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |y| *y = y.wrapping_add(1),
                );
            },
        },
    ),
    (
        0xC9,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::CMP,
    ),
    (
        0xCA,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "DEX",
            implementation: |_, _, system, _| {
                system.cpu.update_register_with_flags(
                    RegisterIndex::X,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |x| *x = x.wrapping_sub(1),
                );
            },
        },
    ),
    (
        0xCC,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::CPY,
    ),
    (
        0xCD,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::CMP,
    ),
    (
        0xCE,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::DEC,
    ),
    (
        0xD0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BNE",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    !system.cpu.registers.p.contains(crate::Flags::ZERO)
                });
            },
        },
    ),
    (
        0xD1,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::CMP,
    ),
    (
        0xD4,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xD5,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::CMP,
    ),
    (
        0xD6,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::DEC,
    ),
    (
        0xD8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "CLD",
            implementation: |_, _, system, _| {
                system.cpu.registers.p.set(Flags::DECIMAL_MODE, false);
            },
        },
    ),
    (
        0xD9,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::CMP,
    ),
    (
        0xDA,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xDC,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xDD,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::CMP,
    ),
    (
        0xDE,
        OpCode {
            size: 3,
            cycles: 7,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::DEC,
    ),
    (
        0xE0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::CPX,
    ),
    (
        0xE1,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectX,
        },
        instructions::SBC,
    ),
    (
        0xE4,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::CPX,
    ),
    (
        0xE5,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::SBC,
    ),
    (
        0xE6,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::INC,
    ),
    (
        0xE8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "INX",
            implementation: |_, _, system, _| {
                system.cpu.update_register_with_flags(
                    RegisterIndex::X,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    |x| *x = x.wrapping_add(1),
                );
            },
        },
    ),
    (
        0xE9,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::SBC,
    ),
    (
        0xEA,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xEC,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::CPX,
    ),
    (
        0xED,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::SBC,
    ),
    (
        0xEE,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        instructions::INC,
    ),
    (
        0xF0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BEQ",
            implementation: |operands, _, system, cycles| {
                instructions::branch_if(system, operands, cycles, |system| {
                    system.cpu.registers.p.contains(crate::Flags::ZERO)
                });
            },
        },
    ),
    (
        0xF1,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::SBC,
    ),
    (
        0xF4,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xF5,
        OpCode {
            size: 2,
            cycles: 4,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::SBC,
    ),
    (
        0xF6,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::ZeroPageX,
        },
        instructions::INC,
    ),
    (
        0xF8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "SED",
            implementation: |_, _, system, _| {
                system.cpu.registers.p.set(Flags::DECIMAL_MODE, true);
            },
        },
    ),
    (
        0xF9,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteY,
        },
        instructions::SBC,
    ),
    (
        0xFA,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xFC,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        Instruction {
            mnemonic: "*NOP",
            implementation: |_, _, _, _| {},
        },
    ),
    (
        0xFD,
        OpCode {
            size: 3,
            cycles: 4,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::SBC,
    ),
    (
        0xFE,
        OpCode {
            size: 3,
            cycles: 7,
            addressing_mode: AddressingMode::AbsoluteX,
        },
        instructions::INC,
    ),
];

mod instructions {
    use crate::{memory::Memory, Flags, RegisterIndex, System};

    use super::{AddressingMode, Instruction};

    fn add_to_accumulator(system: &mut System, value: u8) {
        let a = *system.cpu.registers.a as u16;
        let sum = a.wrapping_add(value as u16)
            + if system.cpu.registers.p.contains(Flags::CARRY) {
                1
            } else {
                0
            };

        let result = sum as u8;

        let carry = sum > 0xFF;
        let overflow = (*system.cpu.registers.a ^ result) & (value ^ result) & 0x80 != 0;

        system.cpu.registers.a.load(result);
        system.cpu.registers.p.set(Flags::CARRY, carry);
        system.cpu.registers.p.set(Flags::OVERFLOW, overflow);
        system
            .cpu
            .set_register_with_flags(RegisterIndex::A, Flags::ZERO_AND_NEGATIVE, result);
    }

    fn update_accumulator_or_addr(
        operands: &[u8],
        addressing_mode: AddressingMode,
        system: &mut System,
        f: impl Fn(u8, &mut System) -> u8,
    ) {
        match addressing_mode {
            AddressingMode::Unsupported => {
                let value = *system.cpu.registers.a;
                let result = f(value, system);
                system.cpu.registers.a.load(result);
            }
            _ => {
                let (addr, _) = system.resolve_addr(operands, addressing_mode);
                let value = system.memory_mapper.read_u8(addr);
                let result = f(value, system);
                system.memory_mapper.write_u8(addr, result);
            }
        }
    }

    pub fn branch_if(
        system: &mut System,
        operands: &[u8],
        cycles: &mut u8,
        condition: fn(&System) -> bool,
    ) {
        if condition(system) {
            let pc = *system.cpu.registers.pc;
            let jump_offset = operands[0] as i8;
            let jump_addr = pc.wrapping_add(jump_offset as u16).wrapping_add(2);
            if pc & 0xFF00 != jump_addr & 0xFF00 {
                *cycles += 1;
            }

            system.cpu.registers.pc.load(jump_addr);
        }
    }

    pub const ADC: Instruction = Instruction {
        mnemonic: "ADC",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);

            add_to_accumulator(system, value);

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const AND: Instruction = Instruction {
        mnemonic: "AND",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            system.cpu.update_register_with_flags(
                RegisterIndex::A,
                Flags::ZERO_AND_NEGATIVE,
                |a| {
                    *a &= value;
                },
            );

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const ASL: Instruction = Instruction {
        mnemonic: "ASL",
        implementation: |operands, opcode, system, _| {
            update_accumulator_or_addr(
                operands,
                opcode.addressing_mode,
                system,
                |value, system| {
                    let carry = value & 0x80 != 0;
                    let result = value << 1;

                    system.cpu.registers.p.set(Flags::CARRY, carry);
                    system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);

                    result
                },
            );
        },
    };

    pub const BIT: Instruction = Instruction {
        mnemonic: "BIT",
        implementation: |operands, opcode, system, _| {
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            let a = *system.cpu.registers.a;
            let result = a & value;

            system.cpu.update_flags(Flags::ZERO, result);

            system
                .cpu
                .registers
                .p
                .set(Flags::OVERFLOW, value & 0x40 != 0);

            system
                .cpu
                .registers
                .p
                .set(Flags::NEGATIVE, value & 0x80 != 0);
        },
    };

    pub const CMP: Instruction = Instruction {
        mnemonic: "CMP",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            let a = *system.cpu.registers.a;
            let result = a.wrapping_sub(value);

            system.cpu.registers.p.set(Flags::CARRY, a >= value);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const CPX: Instruction = Instruction {
        mnemonic: "CPX",
        implementation: |operands, opcode, system, _| {
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            let x = *system.cpu.registers.x;
            let result: u8 = x.wrapping_sub(value);

            system.cpu.registers.p.set(Flags::CARRY, x >= value);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);
        },
    };

    pub const CPY: Instruction = Instruction {
        mnemonic: "CPY",
        implementation: |operands, opcode, system, _| {
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            let y = *system.cpu.registers.y;
            let result: u8 = y.wrapping_sub(value);

            system.cpu.registers.p.set(Flags::CARRY, y >= value);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);
        },
    };

    pub const DEC: Instruction = Instruction {
        mnemonic: "DEC",
        implementation: |operands, opcode, system, _| {
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            let result = value.wrapping_sub(1);

            system.memory_mapper.write_u8(addr, result);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);
        },
    };

    pub const EOR: Instruction = Instruction {
        mnemonic: "EOR",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            system.cpu.update_register_with_flags(
                RegisterIndex::A,
                Flags::ZERO_AND_NEGATIVE,
                |a| {
                    *a ^= value;
                },
            );

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const INC: Instruction = Instruction {
        mnemonic: "INC",
        implementation: |operands, opcode, system, _| {
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            let result = value.wrapping_add(1);

            system.memory_mapper.write_u8(addr, result);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);
        },
    };

    pub const LDA: Instruction = Instruction {
        mnemonic: "LDA",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            system
                .cpu
                .set_register_with_flags(RegisterIndex::A, Flags::ZERO_AND_NEGATIVE, value);

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const LDX: Instruction = Instruction {
        mnemonic: "LDX",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            system
                .cpu
                .set_register_with_flags(RegisterIndex::X, Flags::ZERO_AND_NEGATIVE, value);

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const LDY: Instruction = Instruction {
        mnemonic: "LDY",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            system
                .cpu
                .set_register_with_flags(RegisterIndex::Y, Flags::ZERO_AND_NEGATIVE, value);

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const LSR: Instruction = Instruction {
        mnemonic: "LSR",
        implementation: |operands, opcode, system, _| {
            update_accumulator_or_addr(
                operands,
                opcode.addressing_mode,
                system,
                |value, system| {
                    let carry = value & 1 != 0;
                    let result = value >> 1;

                    system.cpu.registers.p.set(Flags::CARRY, carry);
                    system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);

                    result
                },
            );
        },
    };

    pub const ORA: Instruction = Instruction {
        mnemonic: "ORA",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr);
            system.cpu.update_register_with_flags(
                RegisterIndex::A,
                Flags::ZERO_AND_NEGATIVE,
                |a| {
                    *a |= value;
                },
            );

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const ROL: Instruction = Instruction {
        mnemonic: "ROL",
        implementation: |operands, opcode, system, _| {
            update_accumulator_or_addr(
                operands,
                opcode.addressing_mode,
                system,
                |value, system| {
                    let carry = value & 0x80 != 0;
                    let result = (value << 1)
                        | if system.cpu.registers.p.contains(Flags::CARRY) {
                            1
                        } else {
                            0
                        };

                    system.cpu.registers.p.set(Flags::CARRY, carry);
                    system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);

                    result
                },
            );
        },
    };

    pub const ROR: Instruction = Instruction {
        mnemonic: "ROR",
        implementation: |operands, opcode, system, _| {
            update_accumulator_or_addr(
                operands,
                opcode.addressing_mode,
                system,
                |value, system| {
                    let carry = value & 1 != 0;
                    let result = (value >> 1)
                        | (if system.cpu.registers.p.contains(Flags::CARRY) {
                            0x80
                        } else {
                            0
                        });

                    system.cpu.registers.p.set(Flags::CARRY, carry);
                    system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);

                    result
                },
            );
        },
    };

    pub const SBC: Instruction = Instruction {
        mnemonic: "SBC",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory_mapper.read_u8(addr) as i8;
            let value = value.wrapping_neg().wrapping_sub(1);
            add_to_accumulator(system, value as u8);

            if page_cross {
                *cycles += 1;
            }
        },
    };

    pub const STA: Instruction = Instruction {
        mnemonic: "STA",
        implementation: |operands, opcode, system, _| {
            let (addr, ..) = system.resolve_addr(operands, opcode.addressing_mode);
            system.memory_mapper.write_u8(addr, *system.cpu.registers.a);
        },
    };

    pub const STX: Instruction = Instruction {
        mnemonic: "STX",
        implementation: |operands, opcode, system, _| {
            let (addr, ..) = system.resolve_addr(operands, opcode.addressing_mode);
            system.memory_mapper.write_u8(addr, *system.cpu.registers.x);
        },
    };

    pub const STY: Instruction = Instruction {
        mnemonic: "STY",
        implementation: |operands, opcode, system, _| {
            let (addr, ..) = system.resolve_addr(operands, opcode.addressing_mode);
            system.memory_mapper.write_u8(addr, *system.cpu.registers.y);
        },
    };
}
