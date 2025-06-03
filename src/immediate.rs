// src/immediate.rs

// Extracts bits 31:20, sign-extends to 32 bits.
pub fn extract_imm_i(inst: u32) -> u32 {
    let imm = (inst >> 20) & 0xFFF;
    if (inst >> 31) & 1 == 1 { // Check sign bit (bit 11 of immediate, which is bit 31 of instruction)
        imm | 0xFFFFF000 // Sign-extend
    } else {
        imm
    }
}

// Extracts bits 31:25 and 11:7, combines them, sign-extends.
// imm[11:5] = inst[31:25]
// imm[4:0]  = inst[11:7]
pub fn extract_imm_s(inst: u32) -> u32 {
    let imm_11_5 = (inst >> 25) & 0x7F; // inst[31:25]
    let imm_4_0 = (inst >> 7) & 0x1F;   // inst[11:7]
    let imm = (imm_11_5 << 5) | imm_4_0;
    if (inst >> 31) & 1 == 1 { // Check sign bit (bit 11 of immediate, which is bit 31 of instruction)
        imm | 0xFFFFF000 // Sign-extend
    } else {
        imm
    }
}

// Extracts bits 31, 7, 30:25, and 11:8, combines them, sign-extends, and left shifts by 1.
// imm[12]   = inst[31]
// imm[11]   = inst[7]
// imm[10:5] = inst[30:25]
// imm[4:1]  = inst[11:8]
// imm[0]    = 0
pub fn extract_imm_b(inst: u32) -> u32 {
    let imm_12 = (inst >> 31) & 0x1;    // inst[31]
    let imm_11 = (inst >> 7) & 0x1;     // inst[7]
    let imm_10_5 = (inst >> 25) & 0x3F; // inst[30:25]
    let imm_4_1 = (inst >> 8) & 0xF;    // inst[11:8]

    let imm = (imm_12 << 12) | (imm_11 << 11) | (imm_10_5 << 5) | (imm_4_1 << 1);
    // Sign extension for B-type (13-bit effective immediate, including implicit 0)
    if imm_12 == 1 { // Check original sign bit (inst[31])
        imm | 0xFFFFE000
    } else {
        imm
    }
}

// Extracts bits 31:12, shifts them left by 12 bits.
// This is effectively inst[31:12] << 12. No sign extension needed as it forms the upper 20 bits.
pub fn extract_imm_u(inst: u32) -> u32 {
    inst & 0xFFFFF000
}

// Extracts bits 31, 19:12, 20, and 30:21, combines them, sign-extends, and left shifts by 1.
// imm[20]   = inst[31]
// imm[19:12]= inst[19:12]
// imm[11]   = inst[20]
// imm[10:1] = inst[30:21]
// imm[0]    = 0
pub fn extract_imm_j(inst: u32) -> u32 {
    let imm_20 = (inst >> 31) & 0x1;        // inst[31]
    let imm_19_12 = (inst >> 12) & 0xFF;    // inst[19:12]
    let imm_11 = (inst >> 20) & 0x1;        // inst[20]
    let imm_10_1 = (inst >> 21) & 0x3FF;    // inst[30:21]

    let imm = (imm_20 << 20) | (imm_19_12 << 12) | (imm_11 << 11) | (imm_10_1 << 1);
    // Sign extension for J-type (21-bit effective immediate, including implicit 0)
    if imm_20 == 1 { // Check original sign bit (inst[31])
        imm | 0xFFF00000
    } else {
        imm
    }
}
