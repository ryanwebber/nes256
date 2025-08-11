pub struct Joypad {
    strobe: bool,
    button_index: u8,
    button_status: JoypadButton,
}

impl Joypad {
    pub fn new() -> Self {
        Joypad {
            strobe: false,
            button_index: 0,
            button_status: JoypadButton::empty(),
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 == 1;
        if self.strobe {
            self.button_index = 0;
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 0;
        }

        let response = (self.button_status.bits() & (1 << self.button_index)) >> self.button_index;
        if !self.strobe && self.button_index < 7 {
            self.button_index += 1;
        }

        response
    }

    pub fn set_button(&mut self, button: JoypadButton, pressed: bool) {
        log::trace!("Setting button {:?} to {}", button, pressed);
        self.button_status.set(button, pressed);
    }
}

bitflags::bitflags! {
       // https://wiki.nesdev.com/w/index.php/Controller_reading_code
       #[derive(Debug)]
       pub struct JoypadButton: u8 {
           const RIGHT   = 0b10000000;
           const LEFT    = 0b01000000;
           const DOWN    = 0b00100000;
           const UP      = 0b00010000;
           const START   = 0b00001000;
           const SELECT  = 0b00000100;
           const B       = 0b00000010;
           const A       = 0b00000001;
       }
}
