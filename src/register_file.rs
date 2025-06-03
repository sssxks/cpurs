// src/register_file.rs

#[derive(Debug)]
pub struct RegisterFile {
    regs: [u32; 32],
}

impl RegisterFile {
    pub fn new() -> Self {
        Self { regs: [0; 32] }
    }

    pub fn read(&self, reg_num: u8) -> u32 {
        if reg_num >= 32 {
            panic!("Invalid register number: {}", reg_num);
        }
        self.regs[reg_num as usize]
    }

    pub fn write(&mut self, reg_num: u8, value: u32) {
        if reg_num >= 32 {
            panic!("Invalid register number: {}", reg_num);
        }
        if reg_num == 0 {
            // x0 is hardwired to zero, writes are ignored
            return;
        }
        self.regs[reg_num as usize] = value;
    }
}
