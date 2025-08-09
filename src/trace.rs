use std::{cell::RefCell, fmt::Display};

use crate::{
    memory::Memory,
    opcode::{self, AddressingMode},
    system::System,
};

pub struct Trace<'a> {
    system: RefCell<&'a mut System>,
}

impl<'a> Trace<'a> {
    pub fn new(system: &'a mut System) -> Self {
        Trace {
            system: RefCell::new(system),
        }
    }
}

impl Display for Trace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut system = self.system.borrow_mut();
        let pc = *system.cpu.registers.pc;
        let op = system.memory().read_u8(pc);
        let (opcode, instruction) = opcode::lookup(op);

        let instruction_data: Vec<u8> = [op]
            .iter()
            .copied()
            .chain((1..opcode.size).map(|i| system.memory().read_u8(pc + i as u16)))
            .collect::<Vec<_>>();

        let (mem_addr, stored_value) = match opcode.addressing_mode {
            AddressingMode::Immediate | AddressingMode::Unsupported => (0u16, 0u8),
            _ => {
                let (addr, _) = system.resolve_addr(&instruction_data[1..], opcode.addressing_mode);
                (addr, system.memory().read_u8(addr))
            }
        };

        let memory_fragment = match &instruction_data[..] {
            [op] => match op {
                0x0a | 0x4a | 0x2a | 0x6a => format!("A "),
                _ => format!(""),
            },
            [op, addr] => match opcode.addressing_mode {
                AddressingMode::Immediate => format!("#${:02X}", addr),
                AddressingMode::ZeroPage => format!("${:02X} = {:02X}", mem_addr, stored_value),
                AddressingMode::ZeroPageX => {
                    format!("${:02X},X @ {:02X} = {:02X}", addr, mem_addr, stored_value)
                }
                AddressingMode::ZeroPageY => {
                    format!("${:02X},Y @ {:02X} = {:02X}", addr, mem_addr, stored_value)
                }
                AddressingMode::IndirectX => format!(
                    "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    addr,
                    (addr.wrapping_add(*system.cpu.registers.x)),
                    mem_addr,
                    stored_value
                ),
                AddressingMode::IndirectY => format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    addr,
                    (mem_addr.wrapping_sub(*system.cpu.registers.y as u16)),
                    mem_addr,
                    stored_value
                ),
                AddressingMode::Unsupported => {
                    let addr: usize = (pc as usize + 2).wrapping_add((*addr as i8) as usize);
                    format!("${:04X}", addr)
                }
                _ => panic!(
                    "Unexpected addressing mode {:?} for 2-byte opcode {:02X}",
                    opcode.addressing_mode, op
                ),
            },
            [op, addr_lo, addr_hi] => {
                let addr = u16::from_le_bytes([*addr_lo, *addr_hi]);
                match opcode.addressing_mode {
                    AddressingMode::Unsupported => {
                        if *op == 0x6c {
                            // Special case for jump indirect
                            let jmp_addr = if addr & 0x00FF == 0x00FF {
                                let lo = system.memory().read_u8(addr);
                                let hi = system.memory().read_u8(addr & 0xFF00);
                                u16::from_le_bytes([lo, hi])
                            } else {
                                system.memory().read_u16(addr)
                            };

                            format!("(${:04X}) = {:04X}", addr, jmp_addr)
                        } else {
                            format!("${:04X}", addr)
                        }
                    }
                    AddressingMode::Absolute => {
                        format!("${:04X} = {:02X}", mem_addr, stored_value)
                    }
                    AddressingMode::AbsoluteX => {
                        format!("${:04X},X @ {:04X} = {:02X}", addr, mem_addr, stored_value)
                    }
                    AddressingMode::AbsoluteY => {
                        format!("${:04X},Y @ {:04X} = {:02X}", addr, mem_addr, stored_value)
                    }
                    _ => panic!(
                        "Unexpected addressing mode {:?} for 3-byte opcode {:02X}",
                        opcode.addressing_mode, op
                    ),
                }
            }
            _ => panic!("Unexpected instruction data: {:?}", instruction_data),
        };

        let opcode_and_operands = instruction_data
            .iter()
            .map(|z| format!("{:02X}", z))
            .collect::<Vec<String>>()
            .join(" ");

        let asm_instruction = format!(
            "{:04X}  {:8} {: >4} {}",
            pc, opcode_and_operands, instruction.mnemonic, memory_fragment
        );

        write!(
            f,
            "{:47} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}",
            asm_instruction.trim(),
            *system.cpu.registers.a,
            *system.cpu.registers.x,
            *system.cpu.registers.y,
            system.cpu.registers.p.bits(),
            *system.cpu.registers.sp,
            system.ppu.scanline,
            system.ppu.cycles,
            system.cpu.cycles
        )?;

        Ok(())
    }
}
