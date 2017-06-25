%include "sysv.asm"

%macro START 0
    section .text
    align 16
    global _start
    _start:
%endmacro

SYS_EXIT:  equ 1
SYS_READ:  equ 3
SYS_WRITE: equ 4
SYS_OPEN:  equ 5
SYS_CLOSE: equ 6
