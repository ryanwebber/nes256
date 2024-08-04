use std::collections::HashMap;

use crate::{Interrupt, RegisterIndex, System};

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
            table.insert(*opcode, (opcode_info, instruction));
        }

        // TODO: Remove this print statement
        println!("Loaded {} opcodes...", table.len());

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
        0x06,
        OpCode {
            size: 2,
            cycles: 5,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::ASL,
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
        0x10,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BPL",
            implementation: |operands, _, system, cycles| {
                if !system.cpu.registers.p.contains(crate::Flags::NEGATIVE) {
                    let pc = *system.cpu.registers.pc;
                    let jump_offset = operands[0] as i8;
                    let jump_addr = pc.wrapping_add(jump_offset as u16).wrapping_add(2);
                    if pc & 0xFF00 != jump_addr & 0xFF00 {
                        *cycles += 1;
                    }

                    system.cpu.registers.pc.load(jump_addr);
                }
            },
        },
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
        0x20,
        OpCode {
            size: 3,
            cycles: 6,
            addressing_mode: AddressingMode::Absolute,
        },
        Instruction {
            mnemonic: "JSR",
            implementation: |operands, opcode, system, _| {
                let pc = *system.cpu.registers.pc;
                let (addr, ..) = system.resolve_addr(operands, opcode.addressing_mode);
                system.stack().push_u16(pc + 2);
                system.cpu.registers.pc.load(addr);
            },
        },
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
        0x29,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::AND,
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
        0x69,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::ADC,
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
        0x85,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::STA,
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
        0x91,
        OpCode {
            size: 2,
            cycles: 6,
            addressing_mode: AddressingMode::IndirectY,
        },
        instructions::STA,
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
        0xA0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::LDY,
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
        0xA9,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::LDA,
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
                if system.cpu.registers.p.contains(crate::Flags::CARRY) {
                    let pc = *system.cpu.registers.pc;
                    let jump_offset = operands[0] as i8;
                    let jump_addr = pc.wrapping_add(jump_offset as u16).wrapping_add(2);
                    if pc & 0xFF00 != jump_addr & 0xFF00 {
                        *cycles += 1;
                    }

                    system.cpu.registers.pc.load(jump_addr);
                }
            },
        },
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
        0xC5,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::CMP,
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
        0xD0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BNE",
            implementation: |operands, _, system, cycles| {
                if !system.cpu.registers.p.contains(crate::Flags::ZERO) {
                    let pc = *system.cpu.registers.pc;
                    let jump_offset = operands[0] as i8;
                    let jump_addr = pc.wrapping_add(jump_offset as u16).wrapping_add(2);
                    if pc & 0xFF00 != jump_addr & 0xFF00 {
                        *cycles += 1;
                    }

                    system.cpu.registers.pc.load(jump_addr);
                }
            },
        },
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
        0xF0,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "BEQ",
            implementation: |operands, _, system, cycles| {
                if system.cpu.registers.p.contains(crate::Flags::ZERO) {
                    let pc = *system.cpu.registers.pc;
                    let jump_offset = operands[0] as i8;
                    let jump_addr = pc.wrapping_add(jump_offset as u16).wrapping_add(2);
                    if pc & 0xFF00 != jump_addr & 0xFF00 {
                        *cycles += 1;
                    }

                    system.cpu.registers.pc.load(jump_addr);
                }
            },
        },
    ),
];

mod instructions {
    use crate::{Flags, RegisterIndex, System};

    use super::{AddressingMode, Instruction};

    fn add_to_accumulator(system: &mut System, value: u8) {
        let sum = system.cpu.registers.a.wrapping_add(value)
            + if system.cpu.registers.p.contains(Flags::CARRY) {
                1
            } else {
                0
            };

        let carry = (sum as u16) > 0xFF;
        let overflow = (*system.cpu.registers.a ^ sum) & (value ^ sum) & 0x80 != 0;

        system.cpu.registers.a.load(sum);
        system.cpu.registers.p.set(Flags::CARRY, carry);
        system.cpu.registers.p.set(Flags::OVERFLOW, overflow);
        system
            .cpu
            .set_register_with_flags(RegisterIndex::A, Flags::ZERO_AND_NEGATIVE, sum);
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
                let value = system.memory.read_u8(addr);
                let result = f(value, system);
                system.memory.write_u8(addr, result);
            }
        }
    }

    pub const ADC: Instruction = Instruction {
        mnemonic: "ADC",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory.read_u8(addr);

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
            let value = system.memory.read_u8(addr);
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
            let value = system.memory.read_u8(addr);
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
            let value = system.memory.read_u8(addr);
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
            let value = system.memory.read_u8(addr);
            let x = *system.cpu.registers.x;
            let result: u8 = x.wrapping_sub(value);

            system.cpu.registers.p.set(Flags::CARRY, x >= value);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);
        },
    };

    pub const INC: Instruction = Instruction {
        mnemonic: "INC",
        implementation: |operands, opcode, system, _| {
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory.read_u8(addr);
            let result = value.wrapping_add(1);

            system.memory.write_u8(addr, result);
            system.cpu.update_flags(Flags::ZERO_AND_NEGATIVE, result);
        },
    };

    pub const LDA: Instruction = Instruction {
        mnemonic: "LDA",
        implementation: |operands, opcode, system, cycles| {
            let (addr, page_cross) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory.read_u8(addr);
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
            let value = system.memory.read_u8(addr);
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
            let value = system.memory.read_u8(addr);
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
            let value = system.memory.read_u8(addr);
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

    pub const STA: Instruction = Instruction {
        mnemonic: "STA",
        implementation: |operands, opcode, system, _| {
            let (addr, ..) = system.resolve_addr(operands, opcode.addressing_mode);
            system.memory.write_u8(addr, *system.cpu.registers.a);
        },
    };
}
