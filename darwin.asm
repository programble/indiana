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

O_RDONLY:   equ 0000h
O_WRONLY:   equ 0001h
O_RDWR:     equ 0002h
O_NONBLOCK: equ 0004h
O_APPEND:   equ 0008h
O_CREAT:    equ 0200h
O_TRUNC:    equ 0400h
O_EXCL:     equ 0800h
