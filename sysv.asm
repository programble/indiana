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
