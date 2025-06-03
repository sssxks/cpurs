#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct DecodedInstruction {
    // Dispatch signals
    pub alu_dispatch: bool,
    pub mul_dispatch: bool,
    pub div_dispatch: bool,
    pub load_dispatch: bool,
    pub store_dispatch: bool,
    pub branch_dispatch: bool,
    pub ujump_dispatch: bool,

    // Immediate selector
    pub imm_sel: u8, // 3 bits, e.g., I-type, S-type, etc.

    // ALU
    pub alu_op: u8,      // 4 bits, e.g., ADD, SUB, XOR, etc.
    pub alu_src_a: bool, // True if ALU source A is PC, false if it's RegA
    pub alu_src_b: bool, // True if ALU source B is Immediate, false if it's RegB

    // Jump
    pub jump_op: u8, // 4 bits, e.g., BEQ, JAL, etc.

    // Functional Unit register write enable
    pub fu_reg_write: bool, // True if the result should be written back to a register
}

// Constants for instruction fields and types, similar to Verilog `localparam`
// Opcodes
const OPCODE_REG_ARITH: u32 = 0b0110011; // R-type (ADD, SUB, XOR, etc.)
const OPCODE_IMM_ARITH: u32 = 0b0010011; // I-type (ADDI, SLTI, XORI, etc.)
const OPCODE_LOAD: u32 = 0b0000011; // I-type (LB, LH, LW, etc.)
const OPCODE_STORE: u32 = 0b0100011; // S-type (SB, SH, SW)
const OPCODE_BRANCH: u32 = 0b1100011; // B-type (BEQ, BNE, etc.)
const OPCODE_JAL: u32 = 0b1101111;
const OPCODE_JALR: u32 = 0b1100111;
const OPCODE_LUI: u32 = 0b0110111;
const OPCODE_AUIPC: u32 = 0b0010111;

// Funct7 values (for R-type and some I-type)
const FUNCT7_0: u32 = 0x00; // For ADD, SLL, SLT, SLTU, XOR, SRL, OR, AND, SLLI, SRLI, MUL, MULH, MULHSU, MULHU
const FUNCT7_1: u32 = 0x01; // For M-extension instructions (MUL, DIV, REM family)
const FUNCT7_32: u32 = 0x20; // (decimal 32) For SUB, SRA, SRAI

// Funct3 values
const FUNCT3_0: u32 = 0x0; // ADD, ADDI, SUB, MUL, BEQ, LB, SB, JALR
const FUNCT3_1: u32 = 0x1; // SLL, SLLI, MULH, BNE, LH, SH
const FUNCT3_2: u32 = 0x2; // SLT, SLTI, MULHSU, LW, SW
const FUNCT3_3: u32 = 0x3; // SLTU, SLTIU, MULHU
const FUNCT3_4: u32 = 0x4; // XOR, XORI, DIV, BLT, LBU
const FUNCT3_5: u32 = 0x5; // SRL, SRLI, SRA, SRAI, DIVU, BGE, LHU
const FUNCT3_6: u32 = 0x6; // OR, ORI, REM, BLTU
const FUNCT3_7: u32 = 0x7; // AND, ANDI, REMU, BGEU

// Immediate type selectors (ImmSel output)
const IMM_TYPE_I: u8 = 0b001;
const IMM_TYPE_S: u8 = 0b100; // Store
const IMM_TYPE_B: u8 = 0b010; // Branch
const IMM_TYPE_U: u8 = 0b101; // Upper immediate (LUI, AUIPC)
const IMM_TYPE_J: u8 = 0b011; // Jump (JAL)

// JUMP operation types (JUMP_op output)
const JUMP_OP_JAL: u8 = 0b0_000;
const JUMP_OP_JALR: u8 = 0b1_000;
const JUMP_OP_BEQ: u8 = 0b0_001;
const JUMP_OP_BNE: u8 = 0b0_010;
const JUMP_OP_BLT: u8 = 0b0_011;
const JUMP_OP_BGE: u8 = 0b0_100;
const JUMP_OP_BLTU: u8 = 0b0_101;
const JUMP_OP_BGEU: u8 = 0b0_110;

// ALU operation types (ALU_op output)
const ALU_OP_ADD: u8 = 0b0001;
const ALU_OP_SUB: u8 = 0b0010;
const ALU_OP_AND: u8 = 0b0011;
const ALU_OP_OR: u8 = 0b0100;
const ALU_OP_XOR: u8 = 0b0101;
const ALU_OP_SLL: u8 = 0b0110;
const ALU_OP_SRL: u8 = 0b0111;
const ALU_OP_SLT: u8 = 0b1000;
const ALU_OP_SLTU: u8 = 0b1001;
const ALU_OP_SRA: u8 = 0b1010;
const ALU_OP_COPY_B: u8 = 0b1100; // Pass B (immediate for LUI)

/// Decodes a 32-bit RISC-V instruction and returns control signals.
///
/// # Arguments
/// * `inst`: The 32-bit instruction word.
///
/// # Returns
/// A `DecodedInstruction` struct containing the control signals.
pub fn decode_dispatch(inst: u32) -> DecodedInstruction {
    // Extract instruction fields
    let opcode = inst & 0x7F; // inst[6:0]
    let rd = (inst >> 7) & 0x1F; // inst[11:7]
    let funct3 = (inst >> 12) & 0x07; // inst[14:12]
    // let rs1 = (inst >> 15) & 0x1F;    // inst[19:15] // Not directly used for dispatch signals but good for context
    // let rs2 = (inst >> 20) & 0x1F;    // inst[24:20] // Not directly used for dispatch signals but good for context
    let funct7 = (inst >> 25) & 0x7F; // inst[31:25]

    // Instruction type specification
    let r_op = opcode == OPCODE_REG_ARITH;
    let i_op_arith = opcode == OPCODE_IMM_ARITH; // ADDI, SLTI, XORI etc. (excludes loads, JALR)
    let i_op_load = opcode == OPCODE_LOAD; // LB, LH, LW etc.
    let i_op_jalr = opcode == OPCODE_JALR; // JALR

    let b_op = opcode == OPCODE_BRANCH;
    let s_op = opcode == OPCODE_STORE;
    let u_op_lui = opcode == OPCODE_LUI;
    let u_op_auipc = opcode == OPCODE_AUIPC;
    let j_op_jal = opcode == OPCODE_JAL;

    // Funct7 conditions (for R-type and some I-type like SLLI, SRLI, SRAI)
    let funct7_is_0 = funct7 == FUNCT7_0;
    let funct7_is_1 = funct7 == FUNCT7_1; // M-extension
    let funct7_is_32 = funct7 == FUNCT7_32; // For SUB, SRA

    // Funct3 conditions
    let funct3_is_0 = funct3 == FUNCT3_0;
    let funct3_is_1 = funct3 == FUNCT3_1;
    let funct3_is_2 = funct3 == FUNCT3_2;
    let funct3_is_3 = funct3 == FUNCT3_3;
    let funct3_is_4 = funct3 == FUNCT3_4;
    let funct3_is_5 = funct3 == FUNCT3_5;
    let funct3_is_6 = funct3 == FUNCT3_6;
    let funct3_is_7 = funct3 == FUNCT3_7;

    // --- ALL Instructions ---

    // R-type ALU instructions
    let add = r_op && funct3_is_0 && funct7_is_0;
    let sub = r_op && funct3_is_0 && funct7_is_32;
    let sll = r_op && funct3_is_1 && funct7_is_0;
    let slt = r_op && funct3_is_2 && funct7_is_0;
    let sltu = r_op && funct3_is_3 && funct7_is_0;
    let xor = r_op && funct3_is_4 && funct7_is_0;
    let srl = r_op && funct3_is_5 && funct7_is_0;
    let sra = r_op && funct3_is_5 && funct7_is_32;
    let or = r_op && funct3_is_6 && funct7_is_0;
    let and = r_op && funct3_is_7 && funct7_is_0;

    // R-type M-extension instructions
    let mul = r_op && funct3_is_0 && funct7_is_1;
    let mulh = r_op && funct3_is_1 && funct7_is_1;
    let mulhsu = r_op && funct3_is_2 && funct7_is_1;
    let mulhu = r_op && funct3_is_3 && funct7_is_1;
    let div = r_op && funct3_is_4 && funct7_is_1;
    let divu = r_op && funct3_is_5 && funct7_is_1;
    let rem = r_op && funct3_is_6 && funct7_is_1;
    let remu = r_op && funct3_is_7 && funct7_is_1;

    // I-type ALU instructions
    let addi = i_op_arith && funct3_is_0;
    let slti = i_op_arith && funct3_is_2;
    let sltiu = i_op_arith && funct3_is_3;
    let xori = i_op_arith && funct3_is_4;
    let ori = i_op_arith && funct3_is_6;
    let andi = i_op_arith && funct3_is_7;
    // SLLI, SRLI, SRAI are special I-types that use funct7 bits
    let slli = i_op_arith && funct3_is_1 && funct7_is_0; // funct7[6:1] must be 0 for RV32I
    let srli = i_op_arith && funct3_is_5 && funct7_is_0; // funct7[6:1] must be 0 for RV32I
    let srai = i_op_arith && funct3_is_5 && funct7_is_32; // funct7[6:1] must be 0x10 for RV32I (which is part of 0x20)

    // B-type instructions
    let beq = b_op && funct3_is_0;
    let bne = b_op && funct3_is_1;
    let blt = b_op && funct3_is_4;
    let bge = b_op && funct3_is_5;
    let bltu = b_op && funct3_is_6;
    let bgeu = b_op && funct3_is_7;

    // Load instructions (I-type)
    let lb = i_op_load && funct3_is_0;
    let lh = i_op_load && funct3_is_1;
    let lw = i_op_load && funct3_is_2;
    let lbu = i_op_load && funct3_is_4;
    let lhu = i_op_load && funct3_is_5;

    // Store instructions (S-type)
    let sb = s_op && funct3_is_0;
    let sh = s_op && funct3_is_1;
    let sw = s_op && funct3_is_2;

    // U-type instructions
    let lui = u_op_lui;
    let auipc = u_op_auipc;

    // jal & jalr
    let jal = j_op_jal;
    let jalr = i_op_jalr && funct3_is_0; // JALR is I-type

    // --- Control Signals ---

    let r_alu_valid = add || sub || sll || slt || sltu || xor || srl || sra || or || and;
    let r_mul_valid = mul || mulh || mulhsu || mulhu;
    let r_div_valid = div || divu || rem || remu;
    let i_alu_valid = addi || slti || sltiu || xori || ori || andi || slli || srli || srai;
    let b_valid = beq || bne || blt || bge || bltu || bgeu;
    let l_valid = lw || lh || lb || lhu || lbu;
    let s_valid = sw || sh || sb;
    let u_valid = lui || auipc;

    let rd_is_not_x0 = rd != 0;

    let alu_dispatch = (r_alu_valid || i_alu_valid || u_valid) && rd_is_not_x0;
    let mul_dispatch = r_mul_valid && rd_is_not_x0;
    let div_dispatch = r_div_valid && rd_is_not_x0;
    let load_dispatch = l_valid && rd_is_not_x0;
    let store_dispatch = s_valid;
    let branch_dispatch = b_valid;
    let ujump_dispatch = jal || jalr;

    // ImmSel calculation
    let mut imm_sel = 0;
    if jalr || l_valid || i_alu_valid {
        imm_sel = IMM_TYPE_I;
    } else if b_valid {
        imm_sel = IMM_TYPE_B;
    } else if jal {
        imm_sel = IMM_TYPE_J;
    } else if s_valid {
        imm_sel = IMM_TYPE_S;
    } else if lui || auipc {
        imm_sel = IMM_TYPE_U;
    }

    // JUMP_op calculation
    let mut jump_op = 0; // Default to no jump operation or a specific NOP-like jump
    if beq {
        jump_op = JUMP_OP_BEQ;
    } else if bne {
        jump_op = JUMP_OP_BNE;
    } else if blt {
        jump_op = JUMP_OP_BLT;
    } else if bge {
        jump_op = JUMP_OP_BGE;
    } else if bltu {
        jump_op = JUMP_OP_BLTU;
    } else if bgeu {
        jump_op = JUMP_OP_BGEU;
    } else if jal {
        jump_op = JUMP_OP_JAL;
    } else if jalr {
        jump_op = JUMP_OP_JALR;
    }

    // ALU_op calculation
    let mut alu_op = 0; // Default to a NOP or specific default operation
    if add || addi || auipc {
        alu_op = ALU_OP_ADD;
    } else if sub {
        alu_op = ALU_OP_SUB;
    } else if and || andi {
        alu_op = ALU_OP_AND;
    } else if or || ori {
        alu_op = ALU_OP_OR;
    } else if xor || xori {
        alu_op = ALU_OP_XOR;
    } else if sll || slli {
        alu_op = ALU_OP_SLL;
    } else if srl || srli {
        alu_op = ALU_OP_SRL;
    } else if slt || slti {
        alu_op = ALU_OP_SLT;
    } else if sltu || sltiu {
        alu_op = ALU_OP_SLTU;
    } else if sra || srai {
        alu_op = ALU_OP_SRA;
    } else if lui {
        alu_op = ALU_OP_COPY_B;
    }

    let alu_src_a = auipc; // ALU Source A is PC if AUIPC, else from register file (rs1)
    let alu_src_b = i_alu_valid || i_op_load || i_op_jalr || lui || auipc || s_valid; // ALU Source B is immediate if I-type, U-type, S-type, or Load/JALR.

    let writes_to_rd = r_alu_valid
        || i_alu_valid
        || r_mul_valid
        || r_div_valid
        || l_valid
        || lui
        || auipc
        || jal
        || jalr;
    let fu_reg_write = writes_to_rd && rd_is_not_x0;

    DecodedInstruction {
        alu_dispatch,
        mul_dispatch,
        div_dispatch,
        load_dispatch,
        store_dispatch,
        ujump_dispatch,
        branch_dispatch,
        imm_sel,
        alu_op,
        jump_op,
        alu_src_a,
        alu_src_b,
        fu_reg_write,
    }
}

// Example Usage and Tests
#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function to assert dispatch signals
    ///
    /// # Arguments
    /// * `decoded` - The decoded instruction
    /// * `expected_dispatches` - A tuple of expected dispatch signals in the order:
    ///   (alu, mul, div, load, store, ujump, branch)
    fn assert_dispatch_signals(
        decoded: &DecodedInstruction,
        expected_dispatches: (bool, bool, bool, bool, bool, bool, bool),
    ) {
        let (alu, mul, div, load, store, ujump, branch) = expected_dispatches;
        assert_eq!(decoded.alu_dispatch, alu, "alu_dispatch mismatch");
        assert_eq!(decoded.mul_dispatch, mul, "mul_dispatch mismatch");
        assert_eq!(decoded.div_dispatch, div, "div_dispatch mismatch");
        assert_eq!(decoded.load_dispatch, load, "load_dispatch mismatch");
        assert_eq!(decoded.store_dispatch, store, "store_dispatch mismatch");
        assert_eq!(decoded.ujump_dispatch, ujump, "ujump_dispatch mismatch");
        assert_eq!(decoded.branch_dispatch, branch, "branch_dispatch mismatch");
    }

    #[test]
    fn test_add() {
        // ADD x3, x1, x2 (0x002081B3) -> rd=x3, rs1=x1, rs2=x2
        // opcode=0110011 (R_ARITH), funct3=000 (ADD), funct7=0000000 (ADD)
        let inst = 0x002081B3; // ADD x3, x1, x2
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (true, false, false, false, false, false, false));
        assert_eq!(decoded.imm_sel, 0); // No immediate for R-type in this context of ImmSel
        assert_eq!(decoded.alu_op, ALU_OP_ADD);
        assert!(!decoded.alu_src_a); // rs1
        assert!(!decoded.alu_src_b); // rs2
        assert!(decoded.fu_reg_write);
    }

    #[test]
    fn test_addi() {
        // ADDI x1, x2, 100 (0x06410093) -> rd=x1, rs1=x2, imm=100
        // opcode=0010011 (I_ARITH), funct3=000 (ADDI)
        let inst = 0x06410093; // ADDI x1, x2, 100
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (true, false, false, false, false, false, false));
        assert_eq!(decoded.imm_sel, IMM_TYPE_I);
        assert_eq!(decoded.alu_op, ALU_OP_ADD);
        assert_eq!(decoded.jump_op, 0); // No jump

        assert!(!decoded.alu_src_a); // rs1
        assert!(decoded.alu_src_b); // immediate
        assert!(decoded.fu_reg_write); // Writes to x1
    }

    #[test]
    fn test_lw() {
        // LW a0, 0(sp) -> LW x10, 0(x2) (0x00012503)
        // opcode=0000011 (LOAD), rd=x10, funct3=010 (LW), rs1=x2, imm=0
        let inst = 0x00012503;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (false, false, false, true, false, false, false));
        assert_eq!(decoded.imm_sel, IMM_TYPE_I);
        assert!(decoded.fu_reg_write);
    }

    #[test]
    fn test_sw() {
        // SW a0, 4(sp) -> SW x10, 4(x2) (0x00a12223)
        // opcode=0100011 (STORE), rs2=x10, funct3=010 (SW), rs1=x2, imm_low=4, imm_high=0
        let inst = 0x00a12223;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (false, false, false, false, true, false, false));
        assert_eq!(decoded.imm_sel, IMM_TYPE_S);
        assert!(!decoded.fu_reg_write); // Stores do not write to rd
    }

    #[test]
    fn test_beq() {
        // BEQ x1, x2, label (e.g., 0x00208663 for BEQ x1, x2, +12)
        // opcode=1100011 (BRANCH), funct3=000 (BEQ)
        let inst = 0x00208663;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (false, false, false, false, false, false, true));
        assert_eq!(decoded.imm_sel, IMM_TYPE_B);
        assert_eq!(decoded.jump_op, JUMP_OP_BEQ);
        // ALU might be used for comparison (SUB), but alu_op might not reflect this if branch unit is separate
        // The Verilog doesn't set ALU_op for branches.
        assert_eq!(decoded.alu_op, 0); // Or specific ALU op for compare if designed that way
        assert!(!decoded.fu_reg_write);
    }

    #[test]
    fn test_jal() {
        // JAL x1, offset (e.g., 0x00C000EF for JAL x1, +12) rd=x1
        // opcode=1101111 (JAL)
        let inst = 0x00C000EF;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (false, false, false, false, false, true, false));
        assert_eq!(decoded.imm_sel, IMM_TYPE_J);
        assert_eq!(decoded.jump_op, JUMP_OP_JAL);
        assert!(decoded.fu_reg_write); // JAL writes PC+4 to rd
    }

    #[test]
    fn test_jalr() {
        // JALR x1, x2, offset (e.g., 0x000100E7 for JALR x1, x2, 0) rd=x1, rs1=x2
        // opcode=1100111 (JALR), funct3=000
        let inst = 0x000100E7;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (false, false, false, false, false, true, false));
        assert_eq!(decoded.imm_sel, IMM_TYPE_I);
        assert_eq!(decoded.jump_op, JUMP_OP_JALR);
        assert!(decoded.fu_reg_write); // JALR writes PC+4 to rd
    }

    #[test]
    fn test_lui() {
        // LUI a0, 0x12345 (0x12345537) -> rd=a0 (x10), imm=0x12345
        // opcode=0110111 (LUI)
        let inst = 0x12345537;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (true, false, false, false, false, false, false));
        assert_eq!(decoded.imm_sel, IMM_TYPE_U);
        assert_eq!(decoded.alu_op, ALU_OP_COPY_B); // Passes immediate
        assert!(!decoded.alu_src_a); // Source A usually zero or not used directly for value
        assert!(decoded.alu_src_b); // Immediate
        assert!(decoded.fu_reg_write);
    }

    #[test]
    fn test_mul() {
        // MUL x3, x1, x2 (0x022081B3) -> rd=x3, rs1=x1, rs2=x2
        // opcode=0110011 (R_ARITH), funct3=000 (MUL), funct7=0000001 (MULDIV)
        let inst = 0x022081B3;
        let decoded = decode_dispatch(inst);

        assert_dispatch_signals(&decoded, (false, true, false, false, false, false, false));
        assert_eq!(decoded.alu_op, 0); // MUL has its own dispatch, not standard ALU_op
        assert!(decoded.fu_reg_write);
    }

    #[test]
    fn test_auipc() {
        // AUIPC x3, 0x12345 (0x12345097) -> rd=x3, imm=0x12345
        let inst = 0x12345097;
        let decoded = decode_dispatch(inst);

        assert!(decoded.alu_dispatch);
        assert!(!decoded.mul_dispatch);
        assert!(!decoded.div_dispatch);
        assert!(!decoded.load_dispatch);
        assert!(!decoded.store_dispatch);
        assert!(!decoded.ujump_dispatch);
        assert!(!decoded.branch_dispatch);

        assert_eq!(decoded.imm_sel, IMM_TYPE_U);
        assert_eq!(decoded.alu_op, ALU_OP_ADD);
        assert_eq!(decoded.jump_op, 0);

        assert!(decoded.alu_src_a);
        assert!(decoded.alu_src_b);
        assert!(decoded.fu_reg_write);
    }

    #[test]
    fn test_div() {
        // DIV x3, x1, x2 (0x0220C1B3)
        let inst = 0x0220C1B3;
        let decoded = decode_dispatch(inst);

        assert!(!decoded.alu_dispatch);
        assert!(!decoded.mul_dispatch);
        assert!(decoded.div_dispatch);
        assert_eq!(decoded.alu_op, 0);
        assert!(decoded.fu_reg_write);
    }
}
