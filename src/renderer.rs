use crate::{memory::MemoryBus, ppu::Ppu};

pub const FRAMEBUFFER_SIZE: usize = 256 * 240 * 3;

// RGB8 framebuffer
pub type RawFramebuffer = [u8; FRAMEBUFFER_SIZE];

// NES color palette (approximate RGB values)
const NES_COLORS: [[u8; 3]; 64] = [
    [84, 84, 84],
    [0, 30, 116],
    [8, 16, 144],
    [48, 0, 136],
    [68, 0, 100],
    [92, 0, 48],
    [84, 4, 0],
    [60, 24, 0],
    [32, 42, 0],
    [8, 58, 0],
    [0, 64, 0],
    [0, 60, 0],
    [0, 50, 60],
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
    [152, 150, 152],
    [8, 76, 196],
    [48, 50, 236],
    [92, 30, 228],
    [136, 20, 176],
    [160, 20, 100],
    [152, 34, 32],
    [120, 60, 0],
    [84, 90, 0],
    [40, 114, 0],
    [8, 124, 0],
    [0, 118, 40],
    [0, 102, 120],
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0],
    [236, 238, 236],
    [76, 154, 236],
    [120, 124, 236],
    [176, 98, 236],
    [228, 84, 236],
    [236, 88, 180],
    [236, 106, 100],
    [212, 136, 32],
    [160, 170, 0],
    [116, 196, 0],
    [76, 208, 32],
    [56, 204, 108],
    [56, 180, 204],
    [60, 60, 60],
    [0, 0, 0],
    [0, 0, 0],
    [236, 238, 236],
    [168, 204, 236],
    [188, 188, 236],
    [212, 178, 236],
    [236, 174, 236],
    [236, 174, 212],
    [236, 180, 176],
    [228, 196, 144],
    [204, 210, 120],
    [180, 222, 120],
    [168, 226, 144],
    [152, 226, 180],
    [160, 214, 228],
    [160, 162, 160],
    [0, 0, 0],
    [0, 0, 0],
];

// Sprite structure for OAM data
#[derive(Debug, Clone, Copy)]
struct Sprite {
    y: u8,
    tile_index: u8,
    attributes: u8,
    x: u8,
}

impl Sprite {
    fn from_oam(oam: &[u8], index: usize) -> Self {
        let base = index * 4;
        Sprite {
            y: oam[base],
            tile_index: oam[base + 1],
            attributes: oam[base + 2],
            x: oam[base + 3],
        }
    }

    fn flip_horizontal(&self) -> bool {
        self.attributes & 0x40 != 0
    }

    fn flip_vertical(&self) -> bool {
        self.attributes & 0x80 != 0
    }

    fn priority(&self) -> bool {
        self.attributes & 0x20 == 0 // 0 = behind background, 1 = in front
    }

    fn palette(&self) -> u8 {
        self.attributes & 0x03
    }
}

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
        let fb = &mut self.framebuffer.0;
        let chr = &memory.rom.chr_rom[..];

        // Clear to black if CHR ROM missing
        if chr.is_empty() {
            fb.fill(0);
            return;
        }

        // Get PPU state
        let control = ppu.control();
        let mask = ppu.mask();
        let scroll_x = ppu.scroll_x();
        let scroll_y = ppu.scroll_y();

        // Determine nametable and pattern table addresses
        let nametable_base = match control.bits() & 0x03 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            _ => 0x2C00,
        };
        let background_pattern_base =
            if control.contains(crate::ppu::ControlFlags::BACKROUND_PATTERN_ADDR) {
                0x1000
            } else {
                0x0000
            };
        let sprite_pattern_base = if control.contains(crate::ppu::ControlFlags::SPRITE_PATTERN_ADDR)
        {
            0x1000
        } else {
            0x0000
        };

        // Clear framebuffer
        fb.fill(0);

        // Render background if enabled
        if mask.contains(crate::ppu::MaskFlags::SHOW_BACKGROUND) {
            Self::render_background(
                fb,
                ppu,
                chr,
                nametable_base,
                background_pattern_base,
                scroll_x,
                scroll_y,
                mask,
            );
        }

        // Render sprites if enabled
        if mask.contains(crate::ppu::MaskFlags::SHOW_SPRITES) {
            Self::render_sprites(fb, ppu, chr, sprite_pattern_base, mask);
        }
    }

    fn render_background(
        fb: &mut [u8],
        ppu: &Ppu,
        chr: &[u8],
        nametable_base: u16,
        pattern_base: u16,
        scroll_x: u8,
        scroll_y: u8,
        mask: crate::ppu::MaskFlags,
    ) {
        const WIDTH: usize = 256;
        const HEIGHT: usize = 240;
        const TILES_X: usize = 32;
        const TILES_Y: usize = 30;
        const TILE_SIZE: usize = 8;

        // Calculate which nametable tiles to render based on scroll
        let start_tile_x = (scroll_x as usize) / TILE_SIZE;
        let start_tile_y = (scroll_y as usize) / TILE_SIZE;
        let pixel_offset_x = (scroll_x as usize) % TILE_SIZE;
        let pixel_offset_y = (scroll_y as usize) % TILE_SIZE;

        // Check if we should clip leftmost 8 pixels
        let clip_left = mask.contains(crate::ppu::MaskFlags::LEFTMOST_8PXL_BACKGROUND);
        let start_x = if clip_left { 8 } else { 0 };

        for screen_y in 0..HEIGHT {
            let tile_y = (start_tile_y + (screen_y + pixel_offset_y) / TILE_SIZE) % TILES_Y;
            let pixel_y = (screen_y + pixel_offset_y) % TILE_SIZE;

            for screen_x in start_x..WIDTH {
                let tile_x = (start_tile_x + (screen_x + pixel_offset_x) / TILE_SIZE) % TILES_X;
                let pixel_x = (screen_x + pixel_offset_x) % TILE_SIZE;

                // Get tile data - ensure we stay within VRAM bounds
                let name_index = tile_y * TILES_X + tile_x;
                let vram_addr = nametable_base + name_index as u16;
                let mirrored_addr = Self::mirror_vram_addr(vram_addr);

                // Ensure we don't go beyond VRAM bounds
                if mirrored_addr >= 0x2000 {
                    let vram_index = (mirrored_addr - 0x2000) as usize;
                    if vram_index >= ppu.vram.len() {
                        continue;
                    }

                    let tile_id = ppu.vram[vram_index] as usize;

                    // Get attribute data - ensure we stay within VRAM bounds
                    let attr_index = (tile_y / 4) * 8 + (tile_x / 4);
                    let attr_vram_addr = nametable_base + 0x3C0 + attr_index as u16;
                    let mirrored_attr_addr = Self::mirror_vram_addr(attr_vram_addr);

                    if mirrored_attr_addr >= 0x2000 {
                        let attr_vram_index = (mirrored_attr_addr - 0x2000) as usize;
                        if attr_vram_index >= ppu.vram.len() {
                            continue;
                        }

                        let attr_byte = ppu.vram[attr_vram_index];
                        let attr_shift = ((tile_y % 4) / 2) * 4 + ((tile_x % 4) / 2) * 2;
                        let palette_id = (attr_byte >> attr_shift) & 0x03;

                        // Get tile pattern
                        let tile_addr = pattern_base as usize + tile_id * 16;
                        if tile_addr + 15 >= chr.len() {
                            continue;
                        }

                        let low_plane = chr[tile_addr + pixel_y];
                        let high_plane = chr[tile_addr + 8 + pixel_y];

                        let mask = 0x80u8 >> pixel_x;
                        let lo = ((low_plane & mask) != 0) as u8;
                        let hi = ((high_plane & mask) != 0) as u8;
                        let color_index = (hi << 1) | lo;

                        if color_index != 0 {
                            // NES palette layout:
                            // 0x00: Universal background color
                            // 0x01-0x03: Background palette 0 colors 1-3
                            // 0x05-0x07: Background palette 1 colors 1-3
                            // 0x09-0x0B: Background palette 2 colors 1-3
                            // 0x0D-0x0F: Background palette 3 colors 1-3
                            let palette_addr = if palette_id == 0 {
                                0x01 + (color_index as usize - 1)
                            } else {
                                0x01 + (palette_id as usize - 1) * 4 + (color_index as usize)
                            };

                            if palette_addr < ppu.palettes.len() {
                                let color_id = ppu.palettes[palette_addr] as usize;
                                if color_id < NES_COLORS.len() {
                                    let idx = (screen_y * WIDTH + screen_x) * 3;
                                    fb[idx] = NES_COLORS[color_id][0]; // R
                                    fb[idx + 1] = NES_COLORS[color_id][1]; // G
                                    fb[idx + 2] = NES_COLORS[color_id][2]; // B
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn render_sprites(
        fb: &mut [u8],
        ppu: &Ppu,
        chr: &[u8],
        pattern_base: u16,
        mask: crate::ppu::MaskFlags,
    ) {
        const WIDTH: usize = 256;
        const HEIGHT: usize = 240;

        // Get sprite size from control register
        let sprite_size = if ppu
            .control()
            .contains(crate::ppu::ControlFlags::SPRITE_SIZE)
        {
            16 // 8x16 sprites
        } else {
            8 // 8x8 sprites
        };

        // Check if we should clip leftmost 8 pixels for sprites
        let clip_left = mask.contains(crate::ppu::MaskFlags::LEFTMOST_8PXL_SPRITE);
        let start_x = if clip_left { 8 } else { 0 };

        // Parse OAM data into sprites
        let mut sprites = Vec::new();
        for i in 0..64 {
            let sprite = Sprite::from_oam(&ppu.oam_data, i);
            if sprite.y < 0xEF {
                // Valid sprite Y position
                sprites.push(sprite);
            }
        }

        // Sort sprites by priority (lower index = higher priority)
        sprites.sort_by_key(|s| s.y);

        // Render sprites from back to front
        for sprite in sprites.iter().rev() {
            let sprite_y = sprite.y as usize;
            let sprite_x = sprite.x as usize;
            let tile_index = sprite.tile_index as usize;

            // Check if sprite is visible
            if sprite_y >= HEIGHT || sprite_x >= WIDTH {
                continue;
            }

            // For 8x16 sprites, the tile index is always even
            let base_tile = if sprite_size == 16 {
                tile_index & 0xFE
            } else {
                tile_index
            };

            // Render sprite (either 8x8 or 8x16)
            for y in 0..sprite_size {
                let screen_y = sprite_y + y;
                if screen_y >= HEIGHT {
                    continue;
                }

                let pattern_y = if sprite.flip_vertical() {
                    sprite_size - 1 - y
                } else {
                    y
                };
                let tile_y = pattern_y / 8;
                let pixel_y = pattern_y % 8;

                // Get tile pattern (for 8x16, we need two tiles)
                let tile_addr = pattern_base as usize + (base_tile + tile_y) * 16;
                if tile_addr + 15 >= chr.len() {
                    continue;
                }

                let low_plane = chr[tile_addr + pixel_y];
                let high_plane = chr[tile_addr + 8 + pixel_y];

                for x in 0..8 {
                    let screen_x = sprite_x + x;
                    if screen_x >= WIDTH || screen_x < start_x {
                        continue;
                    }

                    let pattern_x = if sprite.flip_horizontal() { 7 - x } else { x };
                    let mask = 0x80u8 >> pattern_x;
                    let lo = ((low_plane & mask) != 0) as u8;
                    let hi = ((high_plane & mask) != 0) as u8;
                    let color_index = (hi << 1) | lo;

                    if color_index != 0 {
                        let palette_id = sprite.palette();
                        // NES sprite palette layout:
                        // 0x11-0x13: Sprite palette 0 colors 1-3
                        // 0x15-0x17: Sprite palette 1 colors 1-3
                        // 0x19-0x1B: Sprite palette 2 colors 1-3
                        // 0x1D-0x1F: Sprite palette 3 colors 1-3
                        let palette_addr =
                            0x11 + (palette_id as usize) * 4 + (color_index as usize - 1);

                        if palette_addr < ppu.palettes.len() {
                            let color_id = ppu.palettes[palette_addr] as usize;

                            if color_id < NES_COLORS.len() {
                                let idx = (screen_y * WIDTH + screen_x) * 3;

                                // Check if we should draw this pixel (sprite priority)
                                if sprite.priority() || fb[idx] == 0 {
                                    fb[idx] = NES_COLORS[color_id][0]; // R
                                    fb[idx + 1] = NES_COLORS[color_id][1]; // G
                                    fb[idx + 2] = NES_COLORS[color_id][2]; // B
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn mirror_vram_addr(addr: u16) -> u16 {
        // Handle VRAM mirroring (0x3000-0x3EFF mirrors 0x2000-0x2EFF)
        match addr {
            0x3000..=0x3EFF => addr - 0x1000,
            _ => addr,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::Mirroring;
    use crate::ppu::{ControlFlags, MaskFlags, Ppu};

    fn create_test_ppu() -> Ppu {
        Ppu::new(Mirroring::Horizontal)
    }

    fn create_test_memory() -> MemoryBus {
        MemoryBus::with_empty_rom()
    }

    fn create_test_chr_rom() -> Vec<u8> {
        // Create a simple test pattern: 8x8 tile with a diagonal line
        let mut chr = vec![0u8; 0x2000]; // 8KB CHR ROM

        // Tile 0: diagonal line from top-left to bottom-right
        // Low plane: 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
        chr[0] = 0x80; // 10000000
        chr[1] = 0x40; // 01000000
        chr[2] = 0x20; // 00100000
        chr[3] = 0x10; // 00010000
        chr[4] = 0x08; // 00001000
        chr[5] = 0x04; // 00000100
        chr[6] = 0x02; // 00000010
        chr[7] = 0x01; // 00000001

        // High plane: 00000000 for all rows (making it a simple diagonal)
        // High plane bytes start at offset 8
        for i in 8..16 {
            chr[i] = 0x00;
        }

        chr
    }

    #[test]
    fn test_renderer_creation() {
        let renderer = Renderer::new();
        assert_eq!(renderer.framebuffer.0.len(), FRAMEBUFFER_SIZE);

        // Check that framebuffer is initialized to black
        for &pixel in &renderer.framebuffer.0 {
            assert_eq!(pixel, 0);
        }
    }

    #[test]
    fn test_empty_chr_rom_rendering() {
        let mut renderer = Renderer::new();
        let ppu = create_test_ppu();
        let memory = create_test_memory();

        renderer.render_frame(&ppu, &memory);

        // Should render all black
        for &pixel in &renderer.framebuffer.0 {
            assert_eq!(pixel, 0);
        }
    }

    #[test]
    fn test_basic_background_rendering() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up nametable: put tile 0 at position (0,0)
        // Use proper VRAM addressing within 2048-byte limit
        ppu.vram[0] = 0; // Tile ID 0 at nametable base

        // Set up attribute table: palette 0 for the first 2x2 tile group
        ppu.vram[0x3C0] = 0x00; // All palettes set to 0

        // Set up palette: make color 1 (the diagonal line) bright white
        // In NES, palette entry 1 should contain the color index
        ppu.palettes[0x01] = 0x30; // Color 48 in NES palette (bright white)

        // Enable background rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_BACKGROUND);

        renderer.render_frame(&ppu, &memory);

        // Check that the diagonal line is rendered
        // Pixel (0,0) should be color 1 (the diagonal line) since it's the leftmost pixel
        let idx = 0 * 3;
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[48][0]); // R - should be white
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[48][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[48][2]); // B

        // Pixel (7,7) should be color 1 (the diagonal line)
        // Since we're using palette 0, color 1 maps to palette entry 1
        // which should be bright white (color 48)
        let idx = (7 * 256 + 7) * 3;
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[48][0]); // R - should be white
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[48][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[48][2]); // B
    }

    #[test]
    fn test_sprite_rendering() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up OAM: sprite at position (10, 10) using tile 0
        ppu.oam_data[0] = 10; // Y position
        ppu.oam_data[1] = 0; // Tile index
        ppu.oam_data[2] = 0x00; // Attributes: palette 0, no flip, behind background
        ppu.oam_data[3] = 10; // X position

        // Set up sprite palette: make color 1 bright red
        ppu.palettes[0x11] = 0x16; // Color 22 in NES palette (reddish)

        // Enable sprite rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_SPRITES);

        renderer.render_frame(&ppu, &memory);

        // Check that sprite is rendered at the correct position
        let idx = (10 * 256 + 10) * 3;
        // The sprite should be visible at position (10, 10)
        // Since we're using a diagonal pattern, pixel (10, 10) should be color 1
        // which maps to palette entry 1 in sprite palette 0
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[22][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[22][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[22][2]); // B
    }

    #[test]
    fn test_sprite_flipping() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up OAM: sprite at position (10, 10) using tile 0, flipped horizontally
        ppu.oam_data[0] = 10; // Y position
        ppu.oam_data[1] = 0; // Tile index
        ppu.oam_data[2] = 0x40; // Attributes: palette 0, horizontal flip, behind background
        ppu.oam_data[3] = 10; // X position

        // Set up sprite palette: make color 1 light blue
        ppu.palettes[0x11] = 0x31; // Color 49 in NES palette (light blue)

        // Enable sprite rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_SPRITES);

        renderer.render_frame(&ppu, &memory);

        // Check that flipped sprite is rendered correctly
        // With horizontal flip, the diagonal should go from top-right to bottom-left
        let idx = (10 * 256 + 10) * 3;
        // Pixel (10, 10) should now be color 0 (transparent) due to flip
        assert_eq!(renderer.framebuffer.0[idx], 0); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], 0); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], 0); // B

        // Pixel (17, 10) should be color 1 (the diagonal line)
        let idx = (10 * 256 + 17) * 3;
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[49][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[49][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[49][2]); // B
    }

    #[test]
    fn test_sprite_priority() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up background: tile 0 at position (10, 10) with a visible color
        ppu.vram[0] = 0; // Tile ID 0
        ppu.vram[0x3C0] = 0x00; // Palette 0
        ppu.palettes[0x01] = 0x30; // Bright white

        // Set up OAM: sprite at same position (10, 10) using tile 0, behind background
        ppu.oam_data[0] = 10; // Y position
        ppu.oam_data[1] = 0; // Tile index
        ppu.oam_data[2] = 0x20; // Attributes: palette 0, behind background
        ppu.oam_data[3] = 10; // X position

        // Set up sprite palette: make color 1 bright red
        ppu.palettes[0x11] = 0x16; // Bright red

        // Enable both background and sprite rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_BACKGROUND | MaskFlags::SHOW_SPRITES);

        renderer.render_frame(&ppu, &memory);

        // Check that background takes priority over sprite
        let idx = (10 * 256 + 10) * 3;
        // Should show background color (white) not sprite color (red)
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[48][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[48][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[48][2]); // B
    }

    #[test]
    fn test_scroll_rendering() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up nametable: put tile 0 at position (0,0)
        ppu.vram[0] = 0; // Tile ID 0
        ppu.vram[0x3C0] = 0x00; // Palette 0
        ppu.palettes[0x01] = 0x30; // Bright white

        // Set scroll to (4, 4) - 4 pixels right and down
        ppu.write_to_scroll_register(4); // X scroll
        ppu.write_to_scroll_register(4); // Y scroll

        // Enable background rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_BACKGROUND);

        renderer.render_frame(&ppu, &memory);

        // With scroll (4, 4), the tile at (0,0) should be visible at screen position (4,4)
        // Check that the diagonal line is visible at the scrolled position
        let idx = (4 * 256 + 4) * 3;
        // This should be color 1 (the diagonal line)
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[48][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[48][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[48][2]); // B
    }

    #[test]
    fn test_nametable_selection() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up nametable 1 (0x2400) with tile 0 at position (0,0)
        ppu.vram[0x400] = 0; // Tile ID 0 in nametable 1
        ppu.vram[0x7C0] = 0x00; // Palette 0 in nametable 1
        ppu.palettes[0x01] = 0x30; // Bright white

        // Set control register to use nametable 1
        ppu.write_to_control_register(ControlFlags::NAMETABLE1);

        // Enable background rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_BACKGROUND);

        renderer.render_frame(&ppu, &memory);

        // Check that nametable 1 is used
        let idx = 0 * 3;
        // Should show the tile from nametable 1
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[48][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[48][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[48][2]); // B
    }

    #[test]
    fn test_pattern_table_selection() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM with different patterns in each table
        let mut chr = vec![0u8; 0x2000];

        // Pattern table 0: diagonal line
        chr[0] = 0x80; // 10000000
        chr[1] = 0x40; // 01000000
        chr[2] = 0x20; // 00100000
        chr[3] = 0x10; // 00010000
        chr[4] = 0x08; // 00001000
        chr[5] = 0x04; // 00000100
        chr[6] = 0x02; // 00000010
        chr[7] = 0x01; // 00000001

        // Pattern table 1: horizontal line
        chr[0x1000] = 0xFF; // 11111111
        chr[0x1001] = 0x00; // 00000000
        chr[0x1002] = 0x00; // 00000000
        chr[0x1003] = 0x00; // 00000000
        chr[0x1004] = 0x00; // 00000000
        chr[0x1005] = 0x00; // 00000000
        chr[0x1006] = 0x00; // 00000000
        chr[0x1007] = 0x00; // 00000000

        memory.rom.chr_rom = chr;

        // Set up nametable: put tile 0 at position (0,0)
        ppu.vram[0] = 0; // Tile ID 0
        ppu.vram[0x3C0] = 0x00; // Palette 0
        ppu.palettes[0x01] = 0x30; // Bright white

        // Set control register to use pattern table 1 for background
        ppu.write_to_control_register(ControlFlags::BACKROUND_PATTERN_ADDR);

        // Enable background rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_BACKGROUND);

        renderer.render_frame(&ppu, &memory);

        // Check that pattern table 1 is used
        let idx = 0 * 3;
        // Should show the horizontal line from pattern table 1
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[48][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[48][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[48][2]); // B
    }

    #[test]
    fn test_8x16_sprite_rendering() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM with 8x16 sprite pattern
        let mut chr = vec![0u8; 0x2000];

        // Top tile (tile 0): diagonal line
        chr[0] = 0x80; // 10000000
        chr[1] = 0x40; // 01000000
        chr[2] = 0x20; // 00100000
        chr[3] = 0x10; // 00010000
        chr[4] = 0x08; // 00001000
        chr[5] = 0x04; // 00000100
        chr[6] = 0x02; // 00000010
        chr[7] = 0x01; // 00000001

        // Bottom tile (tile 1): horizontal line
        chr[16] = 0xFF; // 11111111
        chr[17] = 0x00; // 00000000
        chr[18] = 0x00; // 00000000
        chr[19] = 0x00; // 00000000
        chr[20] = 0x00; // 00000000
        chr[21] = 0x00; // 00000000
        chr[22] = 0x00; // 00000000
        chr[23] = 0x00; // 00000000

        memory.rom.chr_rom = chr;

        // Set up OAM: 8x16 sprite at position (10, 10) using tile 0
        ppu.oam_data[0] = 10; // Y position
        ppu.oam_data[1] = 0; // Tile index (must be even for 8x16)
        ppu.oam_data[2] = 0x00; // Attributes: palette 0, no flip, behind background
        ppu.oam_data[3] = 10; // X position

        // Set control register to use 8x16 sprites
        ppu.write_to_control_register(ControlFlags::SPRITE_SIZE);

        // Set up sprite palette: make color 1 bright green
        ppu.palettes[0x11] = 0x3A; // Color 58 in NES palette (bright green)

        // Enable sprite rendering
        ppu.write_to_mask_register(MaskFlags::SHOW_SPRITES);

        renderer.render_frame(&ppu, &memory);

        // Check that 8x16 sprite is rendered correctly
        // Top part (diagonal line) at position (10, 10)
        let idx = (10 * 256 + 10) * 3;
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[58][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[58][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[58][2]); // B

        // Bottom part (horizontal line) at position (10, 18)
        let idx = (18 * 256 + 10) * 3;
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[58][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[58][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[58][2]); // B
    }

    #[test]
    fn test_clipping() {
        let mut renderer = Renderer::new();
        let mut ppu = create_test_ppu();
        let mut memory = create_test_memory();

        // Set up test CHR ROM
        memory.rom.chr_rom = create_test_chr_rom();

        // Set up nametable: put tile 0 at position (0,0)
        ppu.vram[0] = 0; // Tile ID 0
        ppu.vram[0x3C0] = 0x00; // Palette 0
        ppu.palettes[0x01] = 0x30; // Bright white

        // Set up OAM: sprite at position (2, 10) using tile 0
        ppu.oam_data[0] = 2; // Y position
        ppu.oam_data[1] = 0; // Tile index
        ppu.oam_data[2] = 0x00; // Attributes: palette 0, no flip, behind background
        ppu.oam_data[3] = 10; // X position

        // Set up sprite palette: make color 1 bright red
        ppu.palettes[0x11] = 0x16; // Bright red

        // Enable leftmost 8-pixel clipping for both background and sprites
        ppu.write_to_mask_register(
            MaskFlags::SHOW_BACKGROUND
                | MaskFlags::SHOW_SPRITES
                | MaskFlags::LEFTMOST_8PXL_BACKGROUND
                | MaskFlags::LEFTMOST_8PXL_SPRITE,
        );

        renderer.render_frame(&ppu, &memory);

        // Debug: check if sprite is rendered at all
        let mut sprite_found = false;
        for x in 0..256 {
            let idx = (2 * 256 + x) * 3;
            if renderer.framebuffer.0[idx] == NES_COLORS[22][0] {
                sprite_found = true;
                break;
            }
        }
        assert!(sprite_found, "Sprite not found in row 2");

        // Check that leftmost 8 pixels are clipped (black)
        for x in 0..8 {
            let idx = (2 * 256 + x) * 3;
            assert_eq!(renderer.framebuffer.0[idx], 0); // R
            assert_eq!(renderer.framebuffer.0[idx + 1], 0); // G
            assert_eq!(renderer.framebuffer.0[idx + 2], 0); // B
        }

        // Check that sprite is visible at its actual position (2, 10) - beyond the 8-pixel clip
        let idx = (2 * 256 + 10) * 3;
        // Should show sprite color (red) since this is the sprite's leftmost pixel
        assert_eq!(renderer.framebuffer.0[idx], NES_COLORS[22][0]); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], NES_COLORS[22][1]); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], NES_COLORS[22][2]); // B

        // Check that sprite is visible at position (2, 17) - within sprite bounds (last pixel of sprite)
        let idx = (2 * 256 + 17) * 3;
        // Should show transparent (0) since this is the last pixel of the sprite and the diagonal pattern ends there
        assert_eq!(renderer.framebuffer.0[idx], 0); // R
        assert_eq!(renderer.framebuffer.0[idx + 1], 0); // G
        assert_eq!(renderer.framebuffer.0[idx + 2], 0); // B
    }
}
