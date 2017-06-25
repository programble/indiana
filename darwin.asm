%include "sysv.asm"

%macro START 0
    section .text
    align 16
    global start
    start:
%endmacro

SYS_EXIT:  equ 0200_0001h
SYS_READ:  equ 0200_0003h
SYS_WRITE: equ 0200_0004h
SYS_OPEN:  equ 0200_0005h
SYS_CLOSE: equ 0200_0006h
