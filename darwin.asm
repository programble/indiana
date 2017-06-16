%macro START 0
    section .text
    align 16
    global start
    start:
%endmacro

%macro SYS_EXIT 1
    mov rdi, %1
    mov rax, 0200_0001h
    syscall
%endmacro

%macro SYS_READ 3
    mov r15, rsi
    mov r14, r8

    mov rdi, %1
    mov rsi, %2
    mov rdx, %3
    mov rax, 0200_0003h
    syscall

    mov r8, r14
    mov rsi, r15
%endmacro

%macro SYS_WRITE 3
    mov r15, rsi
    mov r14, r8

    mov rdi, %1
    mov rsi, %2
    mov rdx, %3
    mov rax, 0200_0004h
    syscall

    mov r8, r14
    mov rsi, r15
%endmacro
