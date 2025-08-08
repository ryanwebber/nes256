use nes256::{
    memory::{Memory, MemoryBus, Rom},
    opcode::{self, AddressingMode},
    system::System,
};

fn trace(system: &mut System) -> String {
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
            AddressingMode::Immediate => format!("#${:02x}", addr),
            AddressingMode::ZeroPage => format!("${:02x} = {:02x}", mem_addr, stored_value),
            AddressingMode::ZeroPageX => {
                format!("${:02x},X @ {:02x} = {:02x}", addr, mem_addr, stored_value)
            }
            AddressingMode::ZeroPageY => {
                format!("${:02x},Y @ {:02x} = {:02x}", addr, mem_addr, stored_value)
            }
            AddressingMode::IndirectX => format!(
                "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                addr,
                (addr.wrapping_add(*system.cpu.registers.x)),
                mem_addr,
                stored_value
            ),
            AddressingMode::IndirectY => format!(
                "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                addr,
                (mem_addr.wrapping_sub(*system.cpu.registers.y as u16)),
                mem_addr,
                stored_value
            ),
            AddressingMode::Unsupported => {
                let addr: usize = (pc as usize + 2).wrapping_add((*addr as i8) as usize);
                format!("${:04x}", addr)
            }
            _ => panic!(
                "Unexpected addressing mode {:?} for 2-byte opcode {:02x}",
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

                        format!("(${:04x}) = {:04x}", addr, jmp_addr)
                    } else {
                        format!("${:04x}", addr)
                    }
                }
                AddressingMode::Absolute => format!("${:04x} = {:02x}", mem_addr, stored_value),
                AddressingMode::AbsoluteX => {
                    format!("${:04x},X @ {:04x} = {:02x}", addr, mem_addr, stored_value)
                }
                AddressingMode::AbsoluteY => {
                    format!("${:04x},Y @ {:04x} = {:02x}", addr, mem_addr, stored_value)
                }
                _ => panic!(
                    "Unexpected addressing mode {:?} for 3-byte opcode {:02x}",
                    opcode.addressing_mode, op
                ),
            }
        }
        _ => panic!("Unexpected instruction data: {:?}", instruction_data),
    };

    let opcode_and_operands = instruction_data
        .iter()
        .map(|z| format!("{:02x}", z))
        .collect::<Vec<String>>()
        .join(" ");

    let asm_instruction = format!(
        "{:04x}  {:8} {: >4} {}",
        pc, opcode_and_operands, instruction.mnemonic, memory_fragment
    );

    let trace_line = format!(
        "{:47} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x} PPU:{:>3},{:>3} CYC:{}",
        asm_instruction.trim(),
        *system.cpu.registers.a,
        *system.cpu.registers.x,
        *system.cpu.registers.y,
        system.cpu.registers.p.bits(),
        *system.cpu.registers.sp,
        system.ppu.scanline,
        system.ppu.cycles,
        system.cpu.cycles
    );

    trace_line.to_ascii_uppercase()
}

#[test]
fn test_correctness() {
    const LOG: &'static str = include_str!("nestest.log");
    const ROM_DATA: &'static [u8] = include_bytes!("nestest.nes");

    let rom = Rom::from_bytes(ROM_DATA).expect("Failed to load ROM");
    let memory_bus = MemoryBus::default_with_rom(rom);

    let mut system = System::new(memory_bus);
    system.cpu.registers.pc.load(0xC000);

    // Account for the initial reset cycle count
    system.cpu.cycles = 7;
    system.ppu.cycles = 21;

    for (i, expected_log) in LOG.lines().enumerate() {
        let actual_log = trace(&mut system);

        println!("[{:>4}] {}", i + 1, actual_log);

        // TODO: Start comparing the full log string once the PPU is implemented
        assert_eq!(&actual_log, &expected_log[..], "Failed on line {}", i + 1);

        system.step().expect(&format!(
            "Failed to step on line {}: {}",
            i + 1,
            expected_log
        ));
    }
}
