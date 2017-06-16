INDIANA_VERSION: equ 1

bits 64
%use altreg
%use smartalign
alignmode p6

;; Return stack manipulation.

%macro RBPUSH 1
    lea rbp, [rbp - 8]
    mov [rbp], %1
%endmacro

%macro RBPOP 1
    mov %1, [rbp]
    lea rbp, [rbp + 8]
%endmacro

;; Main.

%macro NEXT 0
    lodsq
    jmp [rax]
%endmacro

section .text
align 16
docol:
    RBPUSH rsi
    lea rsi, [rax + 8]
NEXT

section .bss
alignb 4096
rstack: resb 8192
    .$:

START
    mov [rel var_sz], rsp
    xor r8, r8
    mov rbp, rstack.$
    mov rsi, .quit
NEXT

section .rodata
.quit: dq quit

;; Dictionary.

F_IMMED:   equ 1000_0000b
F_HIDDEN:  equ 0100_0000b
F_LENMASK: equ 0011_1111b

%define LINK 0

;; DEFWORD "name", label, flags
%macro DEFWORD 2-3 0
    section .rodata
    align 8
    word_%2:
        dq LINK
        %define LINK word_%2
        %strlen WORD_LEN %1
        db WORD_LEN | %3
        db %1
    align 8
    $%2: dq docol
%endmacro

;; DEFCODE "name", label, flags
%macro DEFCODE 2-3 0
    section .rodata
    align 8
    word_%2:
        dq LINK
        %define LINK word_%2
        %strlen WORD_LEN %1
        db WORD_LEN | %3
        db %1
    align 8
    $%2: dq code_%2
    section .text
    align 16
    code_%2:
%endmacro

;; DEFCONST "name", label, value, flags
%macro DEFCONST 3-4 0
    DEFCODE %1, %2, %4
        push r8
        mov r8, %3
    NEXT
%endmacro

;; DEFVAR "name", label, value, flags
%macro DEFVAR 2-4 0, 0
    DEFCODE %1, %2, %4
        push r8
        mov r8, var_%2
    NEXT
    section .data
    align 8
    var_%2: dq %3
%endmacro

;; Essentials.

DEFCODE "EXIT", exit
    RBPOP rsi
NEXT

DEFCODE "LIT", lit
    push r8
    lodsq
    mov r8, rax
NEXT

DEFCODE "LITSTRING", litstring
    push r8
    lodsq
    push rsi
    mov r8, rax
    add rsi, rax
    add rsi, 7
    and rsi, ~7
NEXT

;; Constants.

DEFCONST "VERSION", version, INDIANA_VERSION
DEFCONST "R0", rz, rstack.$
DEFCONST "DOCOL", _docol, docol
DEFCONST "F_IMMED", f_immed, F_IMMED
DEFCONST "F_HIDDEN", f_hidden, F_HIDDEN
DEFCONST "F_LENMASK", f_lenmask, F_LENMASK

DEFCONST "SYS_EXIT", sys_exit, SYS_EXIT
DEFCONST "SYS_OPEN", sys_open, SYS_OPEN
DEFCONST "SYS_CLOSE", sys_close, SYS_CLOSE
DEFCONST "SYS_READ", sys_read, SYS_READ
DEFCONST "SYS_WRITE", sys_write, SYS_WRITE

;; Variables.

DEFVAR "STATE", state
DEFVAR "DP", dp, data
DEFVAR "LATEST", latest, word_last
DEFVAR "S0", sz
DEFVAR "BASE", base, 10

;; Basic stack operations.

DEFCODE "DROP", drop
    pop r8
NEXT

DEFCODE "SWAP", swap
    xchg r8, [rsp]
NEXT

DEFCODE "DUP", dup
    push r8
NEXT

DEFCODE "OVER", over
    push r8
    mov r8, [rsp + 8]
NEXT

DEFCODE "ROT", rot
    pop r9
    pop r10
    push r9
    push r8
    mov r8, r10
NEXT

DEFCODE "-ROT", nrot
    pop r9
    pop r10
    push r8
    push r10
    mov r8, r9
NEXT

DEFCODE "2DROP", twodrop
    pop r8
    pop r8
NEXT

DEFCODE "2DUP", twodup
    push r8
    push qword [rsp - 8]
NEXT

DEFCODE "2SWAP", twoswap
    pop r9
    pop r10
    pop r11
    push r9
    push r8
    push r11
    mov r8, r10
NEXT

DEFCODE "?DUP", qdup
    test r8, r8
    jz .else
        push r8
    .else:
NEXT

;; Arithmetic.

DEFCODE "1+", incr
    add r8, 1
NEXT

DEFCODE "1-", decr
    sub r8, 1
NEXT

DEFCODE "8+", incr8
    add r8, 8
NEXT

DEFCODE "8-", decr8
    sub r8, 8
NEXT

DEFCODE "+", add
    pop r9
    add r8, r9
NEXT

DEFCODE "-", sub
    pop r9
    sub r9, r8
    mov r8, r9
NEXT

DEFCODE "*", mul
    pop r9
    imul r8, r9
NEXT

DEFCODE "/MOD", divmod
    pop rax
    cqo
    idiv r8
    push rdx
    mov r8, rax
NEXT

DEFCODE "U/MOD", udivmod
    pop rax
    xor rdx, rdx
    div r8
    push rdx
    mov r8, rax
NEXT

;; Comparisons.

%macro SET 1
    set%+1 r8l
    movzx r8, r8l
    neg r8
%endmacro

DEFCODE "=", equ
    pop r9
    cmp r8, r9
    SET e
NEXT

DEFCODE "<>", nequ
    pop r9
    cmp r8, r9
    SET ne
NEXT

DEFCODE "<", lt
    pop r9
    cmp r8, r9
    SET l
NEXT

DEFCODE ">", gt
    pop r9
    cmp r8, r9
    SET g
NEXT

DEFCODE "<=", le
    pop r9
    cmp r8, r9
    SET le
NEXT

DEFCODE ">=", ge
    pop r9
    cmp r8, r9
    SET ge
NEXT

DEFCODE "0=", zequ
    test r8, r8
    SET z
NEXT

DEFCODE "0<>", znequ
    test r8, r8
    SET nz
NEXT

DEFCODE "0<", zlt
    test r8, r8
    SET l
NEXT

DEFCODE "0>", zgt
    test r8, r8
    SET g
NEXT

DEFCODE "0<=", zle
    test r8, r8
    SET le
NEXT

DEFCODE "0>=", zge
    test r8, r8
    SET ge
NEXT

;; Bitwise operations.

DEFCODE "AND", and
    pop r9
    and r8, r9
NEXT

DEFCODE "OR", or
    pop r9
    or r8, r9
NEXT

DEFCODE "XOR", xor
    pop r9
    xor r8, r9
NEXT

DEFCODE "INVERT", invert
    not r8
NEXT

;; Memory operations.

DEFCODE "!", store
    pop r9
    mov [r8], r9
    pop r8
NEXT

DEFCODE "@", fetch
    mov r8, [r8]
NEXT

DEFCODE "+!", addstore
    pop r9
    add [r8], r9
    pop r8
NEXT

DEFCODE "-!", substore
    pop r9
    sub [r8], r9
    pop r8
NEXT

DEFCODE "C!", cstore
    pop r9
    mov [r8], r9l
    pop r8
NEXT

DEFCODE "C@", cfetch
    mov r8l, [r8]
    movzx r8, r8l
NEXT

DEFCODE "C@C!", ccopy
    mov r15, rsi

    mov rdi, r8
    pop rsi
    movsb
    push rsi
    mov r8, rdi

    mov rsi, r15
NEXT

DEFCODE "CMOVE", cmove
    mov r15, rsi

    mov rcx, r8
    pop rdi
    pop rsi
    rep movsb
    pop r8

    mov rsi, r15
NEXT

;; Direct stack opreations.

DEFCODE ">R", tor
    RBPUSH r8
    pop r8
NEXT

DEFCODE "R>", fromr
    push r8
    RBPOP r8
NEXT

DEFCODE "RSP@", rspfetch
    push r8
    mov r8, rbp
NEXT

DEFCODE "RSP!", rspstore
    mov rbp, r8
    pop r8
NEXT

DEFCODE "RDROP", rdrop
    add rbp, 8
NEXT

DEFCODE "DSP@", dspfetch
    push r8
    mov r8, rsp
NEXT

DEFCODE "DSP!", dspstore
    mov rsp, r8
    pop r8
NEXT

;; Input, output.

DEFCODE "KEY", key
    push r8
    call _key
    mov r8, rax
NEXT

section .bss
alignb 4096
keybuf: resb 4096
    .#: equ $ - keybuf

section .data
align 8
    .@: dq keybuf
    .$: dq keybuf

section .text
align 16
_key:
    mov rbx, [rel keybuf.@]
    cmp rbx, [rel keybuf.$]
    jge .fill
    xor rax, rax
    mov al, [rbx]
    add qword [rel keybuf.@], 1
ret

align 16
.fill:
    SAVE
    SYSCALL SYS_READ, 0, keybuf, keybuf.#
    RESTORE
    test rax, rax
    jbe .exit
    mov rbx, keybuf
    add rax, rbx
    mov [rel keybuf.$], rax
    mov rax, keybuf
    mov [rel keybuf.@], rax
jmp _key

align 16
.exit:
    SYSCALL SYS_EXIT, rax
jmp .exit

DEFCODE "EMIT", emit
    push r8
    SAVE
    SYSCALL SYS_WRITE, 1, rsp, 1
    RESTORE
    pop rax
    pop r8
NEXT

DEFCODE "WORD", word
    push r8
    call _word
    push rdi
    mov r8, rcx
NEXT

section .bss
wordbuf: resb 64

section .text
align 16
_word:
    call _key
    cmp al, '\'
    je .comment
    cmp al, ' '
    jbe _word

    mov rdi, wordbuf
    .loop:
        stosb
        call _key
        cmp al, ' '
    ja .loop

    mov rax, wordbuf
    sub rdi, rax
    mov rcx, rdi
    mov rdi, rax
ret

align 16
.comment:
    call _key
    cmp al, `\n`
jne .comment
jmp _word

DEFCODE "NUMBER", number
    mov rcx, r8
    pop rdi
    call _number
    push rax
    mov r8, rcx
NEXT

align 16
_number:
    xor rax, rax
    xor rbx, rbx

    test rcx, rcx
    jz .empty

    mov rdx, [rel var_base]

    mov bl, [rdi]
    add rdi, 1
    cmp bl, '-'
    sete r9l
    jne .parse
    sub rcx, 1
    jz .error

    align 16
    .loop:
        imul rax, rdx
        mov bl, [rdi]
        add rdi, 1

        .parse:
        sub bl, '0'
        jb .break
        cmp bl, 10
        jb .base
        sub bl, 'A' - '0'
        jb .break
        add bl, 10

        align 16
        .base:
        cmp bl, dl
        jge .break

        add rax, rbx
        sub rcx, 1
    jnz .loop

    align 16
    .break:
    test r9l, r9l
    jnz .negate
ret

align 16
.negate:
    neg rax
ret

align 16
.error:
    mov rcx, 1
ret

align 16
.empty:
ret

DEFCODE "TELL", tell
    pop rax
    SAVE
    SYSCALL SYS_WRITE, 1, rax, r8
    RESTORE
    pop r8
NEXT

DEFCODE "CHAR", char
    push r8
    call _word
    xor r8, r8
    mov r8l, [rdi]
NEXT

;; Dictionary lookup.

DEFCODE "(FIND)", pfind
    mov rcx, r8
    pop rdi
    call _find
    mov r8, rax
NEXT

align 16
_find:
    mov r15, rsi

    mov rdx, var_latest
    align 16
    .loop:
        mov rdx, [rdx]
        test rdx, rdx
        jz .null

        xor rax, rax
        mov al, [rdx + 8]
        and al, F_LENMASK | F_HIDDEN
        cmp al, cl
        jne .loop

        mov r14, rcx
        mov r13, rdi
        lea rsi, [rdx + 9]
        repe cmpsb
        mov rdi, r13
        mov rcx, r14
    jne .loop

    mov rsi, r15
    mov rax, rdx
ret

align 16
.null:
    mov rsi, r15
    xor rax, rax
ret

DEFCODE ">CFA", tcfa
    mov rdi, r8
    call _tcfa
    mov r8, rdi
NEXT

align 16
_tcfa:
    xor rax, rax
    add rdi, 8
    mov al, [rdi]
    add rdi, 1
    and al, F_LENMASK
    add rdi, rax
    add rdi, 7
    and rdi, ~7
ret

DEFWORD ">DFA", tdfa
    dq tcfa
    dq incr8
dq exit

;; Compiling.

DEFCODE "HEADER,", header_comma
    mov rcx, r8
    pop rbx

    mov rdi, [rel var_dp]
    mov rax, [rel var_latest]
    stosq

    mov al, cl
    stosb
    mov r15, rsi
    mov rsi, rbx
    rep movsb
    mov rsi, r15
    add rdi, 7
    and rdi, ~7

    mov rax, [rel var_dp]
    mov [rel var_latest], rax
    mov [rel var_dp], rdi
NEXT

DEFCODE ",", comma
    mov rax, r8
    call _comma
    pop r8
NEXT

align 16
_comma:
    mov rdi, [rel var_dp]
    stosq
    mov [rel var_dp], rdi
ret

DEFCODE "[", lbrac, F_IMMED
    mov qword [rel var_state], 0
NEXT

DEFCODE "]", rbrac
    mov qword [rel var_state], 1
NEXT

DEFCODE "IMMEDIATE", immediate, F_IMMED
    mov rdi, [rel var_latest]
    add rdi, 8
    xor byte [rdi], F_IMMED
NEXT

DEFCODE "HIDDEN", hidden
    add r8, 8
    xor byte [r8], F_HIDDEN
NEXT

DEFWORD ":", colon
    dq $word
    dq header_comma
    dq lit, docol, comma
    dq latest, fetch, hidden
    dq rbrac
dq exit

DEFWORD ";", semicolon, F_IMMED
    dq lit, exit, comma
    dq latest, fetch, hidden
    dq lbrac
dq exit

DEFWORD "HIDE", hide
    dq $word
    dq pfind
    dq hidden
dq exit

DEFCODE "[']", bractick
    push r8
    lodsq
    mov r8, rax
NEXT

;; Branching.

DEFCODE "BRANCH", branch
    add rsi, [rsi]
NEXT

DEFCODE "0BRANCH", zbranch
    test r8, r8
    pop r8
    jz code_branch
    lodsq
NEXT

;; Interpreter.

section .text
align 16
dodoes:
    cmp qword [rax + 8], 0
    jz .push

    RBPUSH rsi
    mov rsi, [rax + 8]

    .push:
    push r8
    lea r8, [rax + 16]
NEXT

DEFCONST "DODOES", _dodoes, dodoes

DEFWORD "QUIT", quit
    dq rz, rspstore
    dq interpret
dq branch, -32

section .data
align 8
islit: db 0

section .rodata
errmsg: db "PARSE ERROR: "
    .#: equ $ - errmsg
    .nl: db `\n`

DEFCODE "INTERPRET", interpret
    call _word

    mov qword [rel islit], 0
    call _find
    test rax, rax
    jz .lit

    mov rdi, rax
    mov dl, [rdi + 8]
    call _tcfa
    and dl, F_IMMED
    mov rax, rdi
    jnz .exec
    jmp .compile

    align 16
    .lit:
    mov qword [rel islit], 1
    call _number
    test rcx, rcx
    jnz .error
    mov rbx, rax
    mov rax, lit

    align 16
    .compile:
    mov rdx, [rel var_state]
    test rdx, rdx
    jz .exec

    call _comma
    mov rcx, [rel islit]
    test rcx, rcx
    jnz .clit
NEXT

align 16
.clit:
    mov rax, rbx
    call _comma
NEXT

align 16
.exec:
    mov rcx, [rel islit]
    test rcx, rcx
    jnz .elit
jmp [rax]

align 16
.elit:
    push r8
    mov r8, rbx
NEXT

align 16
.error:
    SAVE
    SYSCALL SYS_WRITE, 2, errmsg, errmsg.#
    mov rcx, [rel keybuf.@]
    mov rdx, rcx
    mov rax, keybuf
    sub rdx, rax
    cmp rdx, 40
    jle .print
    mov rdx, 40
    .print:
    sub rcx, rdx
    SYSCALL SYS_WRITE, 2, rcx, rdx
    SYSCALL SYS_WRITE, 2, errmsg.nl, 1
    RESTORE
NEXT

DEFCODE "EXECUTE", execute
    mov rax, r8
    pop r8
jmp [rax]

DEFCODE "SYSCALL3", syscall3
    pop r9
    pop r10
    pop r11
    SAVE
    SYSCALL r8, r9, r10, r11
    RESTORE
    mov r8, rax
NEXT

DEFCODE "SYSCALL2", syscall2
    pop r9
    pop r10
    SAVE
    SYSCALL r8, r9, r10
    RESTORE
    mov r8, rax
NEXT

DEFCODE "SYSCALL1", syscall1
    pop r9
    SAVE
    SYSCALL r8, r9
    RESTORE
    mov r8, rax
NEXT

DEFCODE "SYSCALL0", syscall0
    SAVE
    SYSCALL r8
    RESTORE
    mov r8, rax
NEXT

DEFWORD "LAST", last, F_HIDDEN
dq exit

section .bss
alignb 4096
data: resb 65536
