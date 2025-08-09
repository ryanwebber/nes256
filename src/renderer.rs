use crate::{memory::MemoryBus, ppu::Ppu};

pub const FRAMEBUFFER_SIZE: usize = 256 * 240 * 3;

// RGB8 framebuffer
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
        // Very basic background-only renderer:
        // - Uses nametable 0 (first 32x30 tiles) from PPU VRAM
        // - Uses CHR ROM pattern table at 0x0000
        // - Decodes 2bpp tiles to grayscale (ignores palettes/attributes/sprites/scroll)

        let fb = &mut self.framebuffer.0;
        let chr = &memory.rom.chr_rom[..];

        // Clear to black if CHR ROM missing
        if chr.is_empty() {
            fb.fill(0);
            return;
        }

        // NES frame is 256x240 pixels, tiles are 8x8 (32x30 tiles)
        const WIDTH: usize = 256;
        const HEIGHT: usize = 240;
        const TILES_X: usize = 32;
        const TILES_Y: usize = 30;
        const TILE_SIZE: usize = 8;
        const PATTERN_BASE: usize = 0x0000; // Assume background tiles from pattern table 0

        for tile_y in 0..TILES_Y {
            for tile_x in 0..TILES_X {
                let name_index = tile_y * 32 + tile_x;
                let tile_id = ppu.vram.get(name_index).copied().unwrap_or(0) as usize;

                // Each tile is 16 bytes in CHR: 8 bytes low plane + 8 bytes high plane
                let tile_addr = PATTERN_BASE + tile_id * 16;
                if tile_addr + 15 >= chr.len() {
                    // If out of bounds, skip drawing this tile
                    continue;
                }

                for row in 0..TILE_SIZE {
                    let low_plane = chr[tile_addr + row];
                    let high_plane = chr[tile_addr + 8 + row];

                    for col in 0..TILE_SIZE {
                        let x = tile_x * TILE_SIZE + col;
                        let y = tile_y * TILE_SIZE + row;
                        if x >= WIDTH || y >= HEIGHT {
                            continue;
                        }

                        // Bit position: leftmost pixel uses bit 7
                        let mask = 0x80u8 >> col;
                        let lo = ((low_plane & mask) != 0) as u8;
                        let hi = ((high_plane & mask) != 0) as u8;
                        let color2 = (hi << 1) | lo; // 0..3

                        // Map 2bpp value to simple grayscale (0, 85, 170, 255)
                        let intensity = match color2 {
                            0 => 0u8,
                            1 => 85u8,
                            2 => 170u8,
                            _ => 255u8,
                        };

                        let idx = (y * WIDTH + x) * 3;
                        fb[idx] = intensity; // R
                        fb[idx + 1] = intensity; // G
                        fb[idx + 2] = intensity; // B
                    }
                }
            }
        }
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
