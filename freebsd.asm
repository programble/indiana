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

O_RDONLY:   equ 0000h
O_WRONLY:   equ 0001h
O_RDWR:     equ 0002h
O_NONBLOCK: equ 0004h
O_APPEND:   equ 0008h
O_CREAT:    equ 0200h
O_TRUNC:    equ 0400h
O_EXCL:     equ 0800h
