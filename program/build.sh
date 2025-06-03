#!/bin/bash

# Create build directory if it doesn't exist
mkdir -p build

# If "clean" is passed as an argument, perform cleanup
if [ "$1" == "clean" ]; then
    rm -rf build
    exit 0
fi

# Define the list of source files
SRC_FILES=("test.S")

for SRC in "${SRC_FILES[@]}"; do
    BASENAME="${SRC%.S}"
    OBJ="build/${BASENAME}.o"
    ELF="build/${BASENAME}.elf"
    DUMP="build/${BASENAME}_dump.s"
    BIN="build/${BASENAME}.bin"
    HEX="build/${BASENAME}.mem"
    COE="build/${BASENAME}.coe"
    HEX_REV="build/${BASENAME}_big.mem" # 用于存储转为大端序的十六进制文件

    # Assemble the source file
    if ! riscv64-linux-gnu-as -march=rv32im -mabi=ilp32 -o "$OBJ" "$SRC"; then
        echo "Assembly failed for $SRC"
        exit 1
    fi

    # Link the object file
    if ! riscv64-linux-gnu-ld -m elf32lriscv -T linker.ld -o "$ELF" "$OBJ"; then
        echo "Linking failed for $OBJ"
        exit 1
    fi

    # Generate disassembly (optional)
    if ! riscv64-linux-gnu-objdump -S "$ELF" > "$DUMP"; then
        echo "Objdump failed for $ELF"
        exit 1
    fi

    # Generate binary file from ELF
    if ! riscv64-linux-gnu-objcopy -O binary "$ELF" "$BIN"; then
        echo "Binary extraction failed for $ELF"
        exit 1
    fi

    # Convert binary to hexadecimal format, 4 bytes per line
    if ! xxd -p -c 4 "$BIN" > "$HEX"; then
        echo "Hexadecimal conversion failed for $BIN"
        exit 1
    fi

    # Adjust the byte order (convert little-endian to big-endian)
    # Take each 32-bit line, split into bytes, reverse the order, and reassemble
    if ! awk '
    {
        line = $0;
        first = substr(line, 7, 2);  # Byte 4
        second = substr(line, 5, 2); # Byte 3
        third = substr(line, 3, 2);  # Byte 2
        fourth = substr(line, 1, 2); # Byte 1
        printf("%s%s%s%s\n", first, second, third, fourth);
    }' "$HEX" > "$HEX_REV"; then
        echo "Byte order adjustment failed for $HEX"
        exit 1
    fi

    # Format the hexadecimal data as Vivado COE file
    echo "memory_initialization_radix=16;" > "$COE"
    echo "memory_initialization_vector=" >> "$COE"
    sed 's/$/,/' "$HEX_REV" | sed '$ s/,$/;/' >> "$COE"

    echo "Generated $COE from $SRC"
done
