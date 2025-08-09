use nes256::{
    memory::{MemoryBus, Rom},
    system::System,
};

#[test]
fn test_correctness() {
    const LOG: &'static str = include_str!("nestest.log");
    const ROM_DATA: &'static [u8] = include_bytes!("nestest.nes");

    let rom = Rom::from_bytes(ROM_DATA).expect("Failed to load ROM");
    let memory_bus = MemoryBus::default_with_rom(rom);

    let mut system = System::new(memory_bus);
    system.cpu.registers.pc.load(0xC000);

    for (i, expected_log) in LOG.lines().enumerate() {
        let actual_log = system.trace().to_string();

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
