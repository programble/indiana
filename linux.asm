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

O_RDONLY:   equ 0000q
O_WRONLY:   equ 0001q
O_RDWR:     equ 0002q
O_CREAT:    equ 0100q
O_EXCL:     equ 0200q
O_TRUNC:    equ 1000q
O_APPEND:   equ 2000q
O_NONBLOCK: equ 4000q
