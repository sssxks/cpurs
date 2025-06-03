mod alu;
mod decoder;
mod logic;
mod rom;
mod register_file;
mod memory;
mod immediate;

use decoder::{decode_dispatch, IMM_TYPE_I, IMM_TYPE_S, IMM_TYPE_B, IMM_TYPE_U, IMM_TYPE_J};
use register_file::RegisterFile;
use memory::DataMemory;
use immediate::*;
use alu::execute_alu;
use logic::Procedual;
use rom::ROM;

struct Core {
    rom: ROM<1024>,
    reg_file: RegisterFile,
    data_mem: DataMemory,
    pub pc: u32,
}

impl Core {
    fn new() -> Self {
        Self {
            rom: ROM::new("program/build/test_big.mem".to_string()).unwrap(),
            reg_file: RegisterFile::new(),
            data_mem: DataMemory::new(),
            pc: 0,
        }
    }
}

impl Procedual for Core {
    fn posedge_clk(&mut self) {
        let inst_addr_idx = (self.pc / 4).try_into().unwrap();
        let inst = self.rom.read(inst_addr_idx);
        // PC increment is now handled by jump/branch logic or defaults to pc + 4 later
        // self.pc += 4; // Temporary removal, will be re-added/modified
        let decoded = decode_dispatch(inst);

        // Extract register numbers
        let rd_addr = ((inst >> 7) & 0x1F) as u8;
        let rs1_addr = ((inst >> 15) & 0x1F) as u8;
        let rs2_addr = ((inst >> 20) & 0x1F) as u8;

        // Fetch register operands
        let rs1_val = self.reg_file.read(rs1_addr);
        let rs2_val = self.reg_file.read(rs2_addr);

        // Determine Immediate Value
        let imm_val = match decoded.imm_sel {
            IMM_TYPE_I => extract_imm_i(inst),
            IMM_TYPE_S => extract_imm_s(inst),
            IMM_TYPE_B => extract_imm_b(inst),
            IMM_TYPE_U => extract_imm_u(inst),
            IMM_TYPE_J => extract_imm_j(inst),
            _ => 0, // Default for instructions without immediates or error
        };

        // Select ALU Operands
        // Note: PC used for alu_src_a is relative to current PC *before* increment for AUIPC.
        // The self.pc here is already pointing to the *next* instruction if we incremented early.
        // For now, assuming self.pc holds the address of the *current* instruction for AUIPC.
        // If self.pc was incremented early, AUIPC would need (self.pc - 4).
        // Let's assume PC is incremented *after* all other operations for this clock cycle.
        let alu_val_a = if decoded.alu_src_a { self.pc } else { rs1_val };
        let alu_val_b = if decoded.alu_src_b { imm_val } else { rs2_val };

        // ALU Execution
        let mut alu_result = 0;
        if decoded.alu_dispatch {
            alu_result = execute_alu(decoded.alu_op, alu_val_a, alu_val_b);
        }

        // Load/Store Execution
        let mut load_data = 0;
        // For loads (I-type) and stores (S-type), the address is rs1 + immediate.
        let mem_addr = rs1_val.wrapping_add(imm_val); 

        if decoded.load_dispatch {
            // TODO: Add support for different load sizes (LB, LH, LW, LBU, LHU)
            // For now, assuming all loads are LW (read_u32)
            load_data = self.data_mem.read_u32(mem_addr);
        }

        if decoded.store_dispatch {
            // TODO: Add support for different store sizes (SB, SH, SW)
            // For now, assuming all stores are SW (write_u32)
            self.data_mem.write_u32(mem_addr, rs2_val);
        }

        // ALU Execution - result stored in alu_result
        // MUL Execution
        let mut mul_result = 0;
        if decoded.mul_dispatch {
            mul_result = rs1_val.wrapping_mul(rs2_val);
            // println!("MUL result: {:#x} ({} * {})", mul_result, rs1_val, rs2_val);
        }

        // Write-Back Logic
        if decoded.fu_reg_write && rd_addr != 0 {
            let result_to_write = if decoded.load_dispatch {
                load_data
            } else if decoded.ujump_dispatch { // JAL and JALR write PC + 4 (link address)
                self.pc.wrapping_add(4) // PC of the current jump instruction + 4
            } else if decoded.mul_dispatch {
                mul_result
            } else { // Default to ALU result for other cases like R-type, I-type ALU
                alu_result
            };
            self.reg_file.write(rd_addr, result_to_write);
        }
        
        // Placeholder for current instruction printing
        // println!("PC: {:#010x}, INST: {:#010x}, Decoded: {:?}", self.pc, inst, decoded);
        // println!("rs1_addr: {}, rs1_val: {:#x}", rs1_addr, rs1_val);
        // println!("rs2_addr: {}, rs2_val: {:#x}", rs2_addr, rs2_val);
        // println!("rd_addr: {}", rd_addr);
        // println!("imm_val: {:#x}", imm_val);
        // if decoded.alu_dispatch { println!("ALU result: {:#x}", alu_result); }
        // if decoded.load_dispatch { println!("Loaded data: {:#x} from addr {:#x}", load_data, mem_addr); }
        // if decoded.store_dispatch { println!("Stored data: {:#x} to addr {:#x}", rs2_val, mem_addr); }


        // --- PC Update Logic ---
        let mut next_pc = self.pc.wrapping_add(4); // Default: next instruction

        if decoded.branch_dispatch {
            let condition_met = match decoded.jump_op {
                crate::decoder::JUMP_OP_BEQ => rs1_val == rs2_val,
                crate::decoder::JUMP_OP_BNE => rs1_val != rs2_val,
                crate::decoder::JUMP_OP_BLT => (rs1_val as i32) < (rs2_val as i32),
                crate::decoder::JUMP_OP_BGE => (rs1_val as i32) >= (rs2_val as i32),
                crate::decoder::JUMP_OP_BLTU => rs1_val < rs2_val,
                crate::decoder::JUMP_OP_BGEU => rs1_val >= rs2_val,
                _ => false, // Should not happen for valid branch opcodes
            };

            if condition_met {
                next_pc = self.pc.wrapping_add(imm_val); // imm_val is B-type immediate
                // println!("Branch taken: new PC {:#x}", next_pc);
            } else {
                // println!("Branch not taken.");
            }
        } else if decoded.ujump_dispatch {
            if decoded.jump_op == crate::decoder::JUMP_OP_JALR {
                // For JALR: target = (rs1 + imm) & ~1
                next_pc = (rs1_val.wrapping_add(imm_val)) & !1u32;
                // println!("JALR: target PC {:#x}", next_pc);
            } else { // JAL
                next_pc = self.pc.wrapping_add(imm_val); // imm_val is J-type immediate
                // println!("JAL: target PC {:#x}", next_pc);
            }
        }
        
        self.pc = next_pc;
    }
}

fn main() {
    let mut core = Core::new();

    println!("Starting simulation...");
    const FINAL_LOOP_PC: u32 = 0xB4; // Updated expected PC for the new new loop
    const BEQ_X0_X0_0: u32 = 0x00000063; // opcode for beq x0, x0, 0

    // Enough cycles to complete the new test.S (around 45 instructions before loop)
    for cycle in 0..55 { // Increased cycle count slightly
        println!("Cycle {}: PC: 0x{:08x}", cycle, core.pc);
        
        // Check for halt condition (specific loop)
        if core.pc == FINAL_LOOP_PC {
            let current_inst_idx = (core.pc / 4).try_into().unwrap();
            let current_instruction_at_pc = core.rom.read(current_inst_idx);
            if current_instruction_at_pc == BEQ_X0_X0_0 {
                 println!("Detected loop at PC: 0x{:08x} (instruction: 0x{:08x}). Halting simulation.", core.pc, current_instruction_at_pc);
                 core.posedge_clk(); // Execute the BEQ once to confirm it holds PC
                 println!("Cycle {}: PC: 0x{:08x} (after executing detected loop's beq)", cycle + 1, core.pc);
                 break; 
            }
        }
        core.posedge_clk();
    }

    println!("\nRegisters after simulation:");
    // Print registers that are expected to be used or modified by the new test.S
    let regs_to_print: Vec<u8> = vec![
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
    ];
    for i in regs_to_print {
        let val = core.reg_file.read(i);
        // Optionally only print if non-zero for cleaner output, but for verification, print all listed.
        // if val != 0 || i == 0 { 
            println!("x{:<2}: {:10} (0x{:08x})", i, val, val);
        // }
    }
    // No need for "Other non-zero registers:" if we print all relevant ones.

    println!("\nMemory after simulation:");
    println!("Memory @ 0x0: 0x{:08x}", core.data_mem.read_u32(0));
    // You can add more memory locations if your test program uses them
    // println!("Memory @ 0x4: 0x{:08x}", core.data_mem.read_u32(4));
}
