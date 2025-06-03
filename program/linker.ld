SECTIONS
{
    .text 0x00000000 : { *(.text) }
    .data 0x10000000 : { *(.data) }
    .bss  0x20000000 : { *(.bss) }
}
