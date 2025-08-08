use crate::{memory::MemoryBus, ppu::Ppu};

pub const FRAMEBUFFER_SIZE: usize = 256 * 240 * 3;

pub type RawFramebuffer = [u8; FRAMEBUFFER_SIZE];

pub struct Renderer {
    pub framebuffer: Framebuffer,
}

impl Renderer {
    pub fn new() -> Self {
        Renderer {
            framebuffer: Framebuffer::default(),
        }
    }

    pub fn render_frame(&mut self, ppu: &Ppu, memory: &MemoryBus) {
        _ = memory; // Placeholder for memory usage, if needed
        _ = ppu; // Placeholder for PPU usage, if needed
    }

    pub fn framebuffer(&self) -> &Framebuffer {
        &self.framebuffer
    }
}

pub struct Framebuffer(pub RawFramebuffer);

impl Default for Framebuffer {
    fn default() -> Self {
        Framebuffer([0; FRAMEBUFFER_SIZE])
    }
}
