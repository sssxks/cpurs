// src/memory.rs

use std::collections::HashMap;

#[derive(Debug)]
pub struct DataMemory {
    ram: HashMap<u32, u8>,
}

impl DataMemory {
    pub fn new() -> Self {
        Self {
            ram: HashMap::new(),
        }
    }

    // Helper function to read a single byte, defaulting to 0 if not present.
    fn read_byte_internal(&self, address: u32) -> u8 {
        *self.ram.get(&address).unwrap_or(&0)
    }

    #[allow(dead_code)] // May be used later
    pub fn read_byte(&self, address: u32) -> u8 {
        self.read_byte_internal(address)
    }

    #[allow(dead_code)] // May be used later
    pub fn write_byte(&mut self, address: u32, value: u8) {
        self.ram.insert(address, value);
    }

    pub fn read_u32(&self, address: u32) -> u32 {
        let byte0 = self.read_byte_internal(address) as u32;
        let byte1 = self.read_byte_internal(address.wrapping_add(1)) as u32;
        let byte2 = self.read_byte_internal(address.wrapping_add(2)) as u32;
        let byte3 = self.read_byte_internal(address.wrapping_add(3)) as u32;

        (byte3 << 24) | (byte2 << 16) | (byte1 << 8) | byte0
    }

    pub fn write_u32(&mut self, address: u32, value: u32) {
        let byte0 = (value & 0xFF) as u8;
        let byte1 = ((value >> 8) & 0xFF) as u8;
        let byte2 = ((value >> 16) & 0xFF) as u8;
        let byte3 = ((value >> 24) & 0xFF) as u8;

        self.ram.insert(address, byte0);
        self.ram.insert(address.wrapping_add(1), byte1);
        self.ram.insert(address.wrapping_add(2), byte2);
        self.ram.insert(address.wrapping_add(3), byte3);
    }
}
