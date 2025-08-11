use std::sync::Arc;
use std::time::Instant;

use pixels::Pixels;
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::WindowEvent,
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

use nes256::{joypad::JoypadButton, memory::Rom, Emulator};

pub const NES_WIDTH: u32 = 256;
pub const NES_HEIGHT: u32 = 240;

struct App {
    state: Option<State>,
}

impl ApplicationHandler<()> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let state = self.state.get_or_insert_with(|| {
            log::info!("Initializing NES emulator application state...");
            let size = LogicalSize::new(NES_WIDTH * 2, NES_HEIGHT * 2);
            let attributes = Window::default_attributes()
                .with_title("NES256 Emulator")
                .with_inner_size(size);

            let window = Arc::new(
                event_loop
                    .create_window(attributes)
                    .expect("Failed to create window"),
            );

            let window_size = window.inner_size();

            let surface_texture =
                pixels::SurfaceTexture::new(window_size.width, window_size.height, window.clone());

            let pixels = Pixels::new(NES_WIDTH, NES_HEIGHT, surface_texture)
                .expect("Failed to create Pixels instance");

            let rom = if let Some(path) = std::env::args().nth(1) {
                log::info!("Loading ROM from: {}", path);
                let file_bytes = std::fs::read(path).expect("Failed to read ROM file");
                Rom::from_bytes(&file_bytes).expect("Failed to load ROM")
            } else {
                const ROM_DATA: &[u8] = include_bytes!("../tests/nestest.nes");
                Rom::from_bytes(ROM_DATA).expect("Failed to load ROM")
            };

            let emulator = Emulator::load(rom);

            State {
                window,
                pixels,
                emulator,
                cycle_tuning: 0,
                last_frame_time: None,
            }
        });

        // Request the first redraw to start the rendering loop
        state.window.request_redraw();
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        let Some(state) = &mut self.state else {
            return;
        };

        if state.window.id() != window_id {
            return;
        }

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                let Some(last_frame_time) = state.last_frame_time else {
                    // First frame, just record the time and request another redraw
                    // rather than having a big emulation chunk up front
                    state.last_frame_time = Some(Instant::now());
                    state.window.request_redraw();
                    return;
                };

                // Calculate the time since the last frame
                let current_time = Instant::now();
                let frame_duration = current_time.duration_since(last_frame_time);
                state.last_frame_time = Some(current_time);

                // Ensure we have a reasonable frame duration (minimum 1/120th of a second)
                let min_frame_duration = std::time::Duration::from_secs_f64(1.0 / 120.0);
                let adjusted_duration = if frame_duration < min_frame_duration {
                    min_frame_duration
                } else {
                    frame_duration
                };

                // Run the emulator for the actual time that has passed
                if let Err(err) = state
                    .emulator
                    .run_for(adjusted_duration, &mut state.cycle_tuning)
                {
                    eprintln!("Emulator error: {}", err);
                    event_loop.exit();
                    return;
                }

                // Get the pixel data from the emulator
                let pixel_data = state.emulator.pixel_data();

                // Convert the RGB data to RGBA for pixels crate
                let frame = state.pixels.frame_mut();
                for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
                    if i < pixel_data.len() / 3 {
                        let r = pixel_data[i * 3];
                        let g = pixel_data[i * 3 + 1];
                        let b = pixel_data[i * 3 + 2];
                        pixel[0] = r;
                        pixel[1] = g;
                        pixel[2] = b;
                        pixel[3] = 255; // Alpha
                    }
                }

                if let Err(err) = state.pixels.render() {
                    eprintln!("Failed to render frame: {}", err);
                    event_loop.exit();
                }

                // Request the next frame
                state.window.request_redraw();
            }
            WindowEvent::Resized(size) => {
                if let Err(err) = state.pixels.resize_surface(size.width, size.height) {
                    eprintln!("Failed to resize surface: {}", err);
                    event_loop.exit();
                }
                // Request a redraw after resize
                state.window.request_redraw();
            }
            WindowEvent::KeyboardInput { event, .. } => {
                let joypad_key = match event.physical_key {
                    PhysicalKey::Code(KeyCode::KeyA) => Some(JoypadButton::LEFT),
                    PhysicalKey::Code(KeyCode::KeyD) => Some(JoypadButton::RIGHT),
                    PhysicalKey::Code(KeyCode::KeyW) => Some(JoypadButton::UP),
                    PhysicalKey::Code(KeyCode::KeyS) => Some(JoypadButton::DOWN),
                    PhysicalKey::Code(KeyCode::KeyJ) => Some(JoypadButton::A),
                    PhysicalKey::Code(KeyCode::KeyK) => Some(JoypadButton::B),
                    PhysicalKey::Code(KeyCode::Enter) => Some(JoypadButton::START),
                    PhysicalKey::Code(KeyCode::Space) => Some(JoypadButton::SELECT),
                    _ => None,
                };

                if let Some(button) = joypad_key {
                    state
                        .emulator
                        .joypad1_mut()
                        .set_button(button, event.state == winit::event::ElementState::Pressed);
                }
            }
            WindowEvent::Focused(focused) => {
                if focused {
                    // Window gained focus, request a redraw
                    state.window.request_redraw();
                }
            }
            _ => (),
        }
    }
}

struct State {
    window: Arc<Window>,
    pixels: Pixels<'static>,
    emulator: Emulator,
    last_frame_time: Option<Instant>,
    cycle_tuning: u64,
}

fn main() {
    env_logger::init();

    let event_loop = EventLoop::<()>::with_user_event()
        .build()
        .expect("Failed to create event loop");

    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = App { state: None };
    if let Err(e) = event_loop.run_app(&mut app) {
        eprintln!("Error running application: {}", e);
    }
}
