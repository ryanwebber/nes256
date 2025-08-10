use std::fmt::Display;

use crate::{
    cpu,
    memory::Memory,
    opcode::{self, AddressingMode, Instruction, OpCode},
    system::System,
};

pub struct Trace {
    pub registers: cpu::Registers,
    pub op: u8,
    pub opcode: OpCode,
    pub instruction: Instruction,
    pub addressing_mode: AddressingMode,
    pub operand_mem_addr: u16,
    pub operand_stored_value: u8,
    pub operands: [u8; 2],
    pub ppu_scanline: u32,
    pub ppu_cycles: u32,
    pub cpu_cycles: u32,
    pub jump_addr: Option<u16>,
}

impl Trace {
    pub fn tracing(system: &mut System) -> Self {
        let pc = *system.cpu.registers.pc;

        // Read opcode and decode
        let op = system.memory().read_u8(pc);
        let (opcode, instruction) = opcode::lookup(op);
        let operand_len = (opcode.size - 1) as usize;
        let addressing_mode = opcode.addressing_mode;

        // Fetch up to two operand bytes (following Trace::fmt behavior)
        // Note: operands[1] is used for 2-byte instructions by the Display impl.
        let mut operands = [0u8; 2];
        for i in 0..operand_len {
            operands[i as usize] = system.memory().read_u8(pc.wrapping_add(1 + i as u16));
        }

        let (operand_mem_addr, operand_stored_value) = match addressing_mode {
            AddressingMode::Immediate | AddressingMode::Unsupported => (0u16, 0u8),
            _ => {
                let (addr, _) = system.resolve_addr(&operands[0..operand_len], addressing_mode);
                let val = system.memory().read_u8(addr);
                (addr, val)
            }
        };

        // Special case: JMP (indirect) with 6502 page-wrap bug
        let jump_addr = if op == 0x6C && operands.len() == 2 {
            let ptr = u16::from_le_bytes([operands[0], operands[1]]);
            let target = if (ptr & 0x00FF) == 0x00FF {
                let lo = system.memory().read_u8(ptr);
                let hi = system.memory().read_u8(ptr & 0xFF00);
                u16::from_le_bytes([lo, hi])
            } else {
                system.memory().read_u16(ptr)
            };
            Some(target)
        } else {
            None
        };

        Trace {
            registers: system.cpu.registers.clone(),
            op,
            opcode: opcode.clone(),
            instruction: instruction.clone(),
            addressing_mode,
            operand_mem_addr,
            operand_stored_value,
            operands,
            ppu_scanline: system.ppu.scanline as u32,
            ppu_cycles: system.ppu.cycles as u32,
            cpu_cycles: system.cpu.cycles as u32,
            jump_addr,
        }
    }
}

impl Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let pc = *self.registers.pc;
        let op = self.op;
        let opcode = self.opcode;
        let operand_len = (opcode.size - 1) as usize;
        let instruction = self.instruction;
        let mem_addr = self.operand_mem_addr;
        let stored_value = self.operand_stored_value;

        let instruction_data: Vec<u8> = [op]
            .iter()
            .copied()
            .chain((0..operand_len).map(|i| self.operands[i as usize]))
            .collect::<Vec<_>>();

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
                    (addr.wrapping_add(*self.registers.x)),
                    mem_addr,
                    stored_value
                ),
                AddressingMode::IndirectY => format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    addr,
                    (mem_addr.wrapping_sub(*self.registers.y as u16)),
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
                            let jmp_addr = self.jump_addr.unwrap_or(0xDEAD);
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
            *self.registers.a,
            *self.registers.x,
            *self.registers.y,
            self.registers.p.bits(),
            *self.registers.sp,
            self.ppu_scanline,
            self.ppu_cycles,
            self.cpu_cycles
        )?;

        Ok(())
    }
}
