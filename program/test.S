.global _start
_start:
  # Initial setup
  addi x1, x0, 10      # x1 = 10
  addi x2, x0, 20      # x2 = 20
  add  x3, x1, x2      # x3 = x1 + x2 = 30
  sw   x3, 0(x0)       # Store x3 (30) to memory address 0
  addi x4, x0, 0       # Clear x4
  lw   x4, 0(x0)       # Load value from memory address 0 into x4 (x4=30)
  addi x5, x4, 5       # x5 = x4 + 5 = 35

  # Conditional Branch Tests
  addi x6, x0, 5
  addi x7, x0, 5
  beq  x6, x7, target_taken
  addi x20, x0, 999           # SKIPPED
  addi x21, x0, 888           # SKIPPED
target_taken:
  addi x9, x0, 50

  addi x6, x0, 10
  bne  x6, x7, target_bne_taken
  addi x22, x0, 777           # SKIPPED
target_bne_taken:
  addi x8, x0, 100

  # JAL Test
  jal x10, after_jal
  addi x23, x0, 1             # SKIPPED
after_jal:
  addi x11, x0, 200

  # LUI and AUIPC Tests
  lui x12, 0xABCDE
  auipc x13, 0x12345

  # More ALU Immediate Tests
  addi x14, x0, -1
  xori x16, x14, 0x0F0
  ori  x17, x14, 0x01F
  andi x18, x14, 0x01F
  slti x19, x14, 0
  sltiu x24, x14, 0
  # x2 is 20 (original) before shifts
  addi x2, x0, 20      # Restore x2 for sltiu tests against 20
  sltiu x25, x2, 20
  sltiu x26, x2, 21

  # More ALU Register-Register Tests
  addi x1, x0, 10      # Restore x1 = 10
  addi x2, x0, 20      # Restore x2 = 20
  xor  x27, x1, x2
  or   x28, x1, x2
  and  x29, x1, x2
  
  # Shift tests
  addi x7, x0, 2       # x7 = 2 (shift amount)
  # x1 = 10, x2 = 20, x14 = -1
  sll  x1, x1, x7      # x1 = 10 << 2 = 40
  srl  x2, x2, x7      # x2 = 20 >> 2 = 5
  sra  x14, x14, x7    # x14 = -1 >> 2 = -1

  # SLT / SLTU with register-register
  # After shifts: x1 = 40, x2 = 5
  slt  x30, x2, x1     # x30 = (5 < 40) = 1
  sltu x31, x2, x1     # x31 = (5 < 40) = 1

  # MUL Test
  # x9=50, x8=100
  mul x20, x9, x8      # x20 = 50 * 100 = 5000

  # JALR to loop
  # PC of `addi x13_jalr_base` is 0xAC (idx 43)
  # PC of `jalr` = 0xAC + 4 = 0xB0
  # `loop:` label will be at 0xB0 + 4 = 0xB4
  # Target for JALR is 0xB4.
  # If x13_jalr_base = 0, then imm = 0xB4 = 180.
  addi x13, x0, 0      # x13_jalr_base = 0. (PC = 0xAC)
  jalr x15, x13, 180   # Jump to (0 + 180) & ~1 = 180 (0xB4). x15_link = PC_jalr + 4 = 0xB0 + 4 = 0xB4. (PC = 0xB0)

# Simple infinite loop to halt
loop:                           # Expected PC = 0xB4
  beq x0, x0, loop            # Branch to self (offset 0)
