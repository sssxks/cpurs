yes another RISC-V CPU

pretend to program like verilog or some HLS.

some rules:
- don't use sum types(enums with fields)
- don't use dynamic memory allocation (vectors, hash maps, etc.)
- write unit test.

program can be found in `program/`. require riscv toolchain to build.

frontend:

fetches instructions from ROM, filling the backend as fast as possible

backend:

- alu
- load/store
- mul
- div

