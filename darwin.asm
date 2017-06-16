%macro START 0
    section .text
    align 16
    global start
    start:
%endmacro

%macro SAVE 0
    mov r15, rsi
    mov r14, r8
%endmacro

%macro RESTORE 0
    mov r8, r14
    mov rsi, r15
%endmacro

%macro SYSCALL 1
    mov rax, %1
    syscall
%endmacro

%macro SYSCALL 2
    mov rdi, %2
    mov rax, %1
    syscall
%endmacro

%macro SYSCALL 3
    mov rsi, %3
    SYSCALL %1, %2
%endmacro

%macro SYSCALL 4
    mov rdx, %4
    SYSCALL %1, %2, %3
%endmacro

SYS_EXIT:  equ 0200_0001h
SYS_READ:  equ 0200_0003h
SYS_WRITE: equ 0200_0004h
SYS_OPEN:  equ 0200_0005h
SYS_CLOSE: equ 0200_0006h
