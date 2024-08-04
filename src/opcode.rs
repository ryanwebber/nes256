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
    implementation: fn([u8; 2], &OpCode, &mut System, &mut u8),
}

impl Instruction {
    pub fn execute(
        &self,
        operands: [u8; 2],
        opcode: &OpCode,
        system: &mut System,
        cycles: &mut u8,
    ) {
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
        0x29,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::AND,
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
                let (addr_of_addr, _) = system.resolve_addr(operands, AddressingMode::Absolute);
                let addr = system.memory.read_u16(addr_of_addr);
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
        0x85,
        OpCode {
            size: 2,
            cycles: 3,
            addressing_mode: AddressingMode::ZeroPage,
        },
        instructions::STA,
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
        0xA9,
        OpCode {
            size: 2,
            cycles: 2,
            addressing_mode: AddressingMode::Immediate,
        },
        instructions::LDA,
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
        0xE8,
        OpCode {
            size: 1,
            cycles: 2,
            addressing_mode: AddressingMode::Unsupported,
        },
        Instruction {
            mnemonic: "INX",
            implementation: |_, _, system, _| {
                let x = system.cpu.registers.x.wrapping_add(1);
                system.cpu.registers.x.load(x);
                system.cpu.set_register_with_flags(
                    RegisterIndex::X,
                    crate::Flags::ZERO_AND_NEGATIVE,
                    x,
                );
            },
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

    use super::Instruction;

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
            let (addr, _) = system.resolve_addr(operands, opcode.addressing_mode);
            let value = system.memory.read_u8(addr);
            let (result, carry) = value.overflowing_shl(1);

            system.memory.write_u8(addr, result);
            system.cpu.registers.p.set(Flags::CARRY, carry);
            system
                .cpu
                .set_register_with_flags(RegisterIndex::A, Flags::ZERO_AND_NEGATIVE, result);
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
            println!(
                "STA: addr={:04X} op[0]={:02X} op[1]={:02X} addr_mode={:?}",
                addr, operands[0], operands[1], opcode.addressing_mode
            );
            system.memory.write_u8(addr, *system.cpu.registers.a);
        },
    };
}