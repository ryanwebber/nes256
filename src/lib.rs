use crate::{
    memory::{MemoryBus, Rom},
    renderer::{RawFramebuffer, Renderer},
    system::System,
};

pub mod common;
pub mod cpu;
pub mod memory;
pub mod opcode;
pub mod ppu;
pub mod renderer;
pub mod system;
pub mod trace;

pub struct Emulator {
    system: System,
    renderer: Renderer,
}

impl Emulator {
    pub fn load(rom: Rom) -> Self {
        let bus = MemoryBus::default_with_rom(rom);
        let system = System::new(bus);

        Emulator {
            system,
            renderer: Renderer::new(),
        }
    }

    pub fn run_for(
        &mut self,
        duration: std::time::Duration,
        cycle_tuning: &mut u64,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let requested_cycles: u64 = (duration.as_secs_f64() * self.cycles_per_second()) as u64;
        let target_cycles = requested_cycles - *cycle_tuning;

        log::debug!(
            "Emulating {:?}ms ({} cycles)",
            duration.as_millis(),
            target_cycles
        );

        self.run(target_cycles)?;

        // TODO: Fix cycle tuning
        *cycle_tuning = 0;

        Ok(())
    }

    pub fn run(&mut self, cycles: u64) -> Result<(), Box<dyn std::error::Error>> {
        let current_cycles = self.system.cpu.cycles;
        while self.system.cpu.cycles < current_cycles + cycles {
            if self.system.cpu.halted {
                break;
            }

            let was_in_vblank = self
                .system
                .ppu
                .status_flags()
                .contains(ppu::StatusFlags::VBLANK_STARTED);

            self.system.step()?;

            let now_in_vblank = self
                .system
                .ppu
                .status_flags()
                .contains(ppu::StatusFlags::VBLANK_STARTED);

            let rendering_disabled = self
                .system
                .ppu
                .mask()
                .intersection(ppu::MaskFlags::SHOW_BACKGROUND | ppu::MaskFlags::SHOW_SPRITES)
                .is_empty();

            if !was_in_vblank && now_in_vblank && !rendering_disabled {
                // VBlank has started, render the frame
                self.force_render();
            }
        }

        Ok(())
    }

    pub fn force_render(&mut self) {
        self.renderer
            .render_frame(&self.system.ppu, &self.system.bus);
    }

    pub fn pixel_data(&self) -> &RawFramebuffer {
        &self.renderer.framebuffer().0
    }

    fn cycles_per_second(&self) -> f64 {
        // Assume 1.7897725 MHz for NTSC NES
        1_789_772.5
    }
}
