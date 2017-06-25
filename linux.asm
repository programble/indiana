%include "sysv.asm"

%macro START 0
    section .text
    align 16
    global _start
    _start:
%endmacro

SYS_READ:  equ 0
SYS_WRITE: equ 1
SYS_OPEN:  equ 2
SYS_CLOSE: equ 3
SYS_EXIT:  equ 60
