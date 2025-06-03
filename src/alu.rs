// src/alu.rs

use crate::decoder::{
    ALU_OP_ADD, ALU_OP_SUB, ALU_OP_AND, ALU_OP_OR, ALU_OP_XOR,
    ALU_OP_SLL, ALU_OP_SRL, ALU_OP_SLT, ALU_OP_SLTU, ALU_OP_SRA,
    ALU_OP_COPY_B,
};

#[allow(dead_code)] // Will be used in later subtasks
pub fn execute_alu(op: u8, val_a: u32, val_b: u32) -> u32 {
    match op {
        ALU_OP_ADD => val_a.wrapping_add(val_b),
        ALU_OP_SUB => val_a.wrapping_sub(val_b),
        ALU_OP_AND => val_a & val_b,
        ALU_OP_OR => val_a | val_b,
        ALU_OP_XOR => val_a ^ val_b,
        ALU_OP_SLL => val_a.wrapping_shl(val_b & 0x1F),
        ALU_OP_SRL => val_a.wrapping_shr(val_b & 0x1F),
        ALU_OP_SLT => {
            if (val_a as i32) < (val_b as i32) {
                1
            } else {
                0
            }
        }
        ALU_OP_SLTU => {
            if val_a < val_b {
                1
            } else {
                0
            }
        }
        ALU_OP_SRA => (val_a as i32).wrapping_shr(val_b & 0x1F) as u32,
        ALU_OP_COPY_B => val_b,
        _ => panic!("Unknown ALU operation: {}", op),
    }
}
