use crate::{
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

pub struct Emulator {
    system: System,
    renderer: Renderer,
}

impl Emulator {
    pub fn run_for(
        &mut self,
        duration: std::time::Duration,
        cycle_tuning: &mut u64,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let requested_cycles: u64 = (duration.as_secs_f64() * self.cycles_per_second()) as u64;
        let target_cycles = requested_cycles - *cycle_tuning;
        let current_cycles = self.system.cpu.cycles;
        self.run(target_cycles)?;
        *cycle_tuning = self.system.cpu.cycles - current_cycles;
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

            if !was_in_vblank && now_in_vblank {
                // VBlank has started, render the frame
                self.renderer
                    .render_frame(&self.system.ppu, &self.system.bus);
            }
        }

        Ok(())
    }

    pub fn pixel_data(&self) -> &RawFramebuffer {
        &self.renderer.framebuffer().0
    }

    fn cycles_per_second(&self) -> f64 {
        // Assume 1.7897725 MHz for NTSC NES
        1_789_772.5
    }
}
