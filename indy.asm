INDIANA_VERSION: equ 1

bits 64
default rel
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
    mov [var_sz], rsp
    xor r12, r12
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
        push r12
        mov r12, %3
    NEXT
%endmacro

;; DEFVAR "name", label, value, flags
%macro DEFVAR 2-4 0, 0
    DEFCODE %1, %2, %4
        push r12
        mov r12, var_%2
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
    push r12
    lodsq
    mov r12, rax
NEXT

DEFCODE "LITSTRING", litstring
    push r12
    lodsq
    push rsi
    mov r12, rax
    add rsi, rax
    add rsi, 7
    and rsi, ~7
NEXT

;; Constants.

DEFCONST "VERSION", version, INDIANA_VERSION
DEFCONST "R0", rz, rstack.$
DEFCONST "LIMIT", limit, data.$
DEFCONST "DOCOL", _docol, docol
DEFCONST "F_IMMED", f_immed, F_IMMED
DEFCONST "F_HIDDEN", f_hidden, F_HIDDEN
DEFCONST "F_LENMASK", f_lenmask, F_LENMASK

DEFCONST "SYS_EXIT", sys_exit, SYS_EXIT
DEFCONST "SYS_OPEN", sys_open, SYS_OPEN
DEFCONST "SYS_CLOSE", sys_close, SYS_CLOSE
DEFCONST "SYS_READ", sys_read, SYS_READ
DEFCONST "SYS_WRITE", sys_write, SYS_WRITE

DEFCONST "O_RDONLY", o_rdonly, O_RDONLY
DEFCONST "O_WRONLY", o_wronly, O_WRONLY
DEFCONST "O_RDWR", o_rdwr, O_RDWR
DEFCONST "O_CREAT", o_creat, O_CREAT
DEFCONST "O_EXCL", o_excl, O_EXCL
DEFCONST "O_TRUNC", o_trunc, O_TRUNC
DEFCONST "O_APPEND", o_append, O_APPEND
DEFCONST "O_NONBLOCK", o_nonblock, O_NONBLOCK

;; Variables.

DEFVAR "STATE", state
DEFVAR "HERE", here, data
DEFVAR "LATEST", latest, word_last
DEFVAR "S0", sz
DEFVAR "BASE", base, 10

;; Basic stack operations.

DEFCODE "DROP", drop
    pop r12
NEXT

DEFCODE "SWAP", swap
    xchg r12, [rsp]
NEXT

DEFCODE "DUP", dup
    push r12
NEXT

DEFCODE "OVER", over
    push r12
    mov r12, [rsp + 8]
NEXT

DEFCODE "ROT", rot
    xchg r12, [rsp]
    xchg r12, [rsp + 8]
NEXT

DEFCODE "-ROT", nrot
    xchg r12, [rsp + 8]
    xchg r12, [rsp]
NEXT

DEFCODE "2DROP", twodrop
    pop r12
    pop r12
NEXT

DEFCODE "2DUP", twodup
    push r12
    push qword [rsp + 8]
NEXT

DEFCODE "2SWAP", twoswap
    pop r13
    pop r14
    pop r15
    push r13
    push r12
    push r15
    mov r12, r14
NEXT

DEFCODE "?DUP", qdup
    test r12, r12
    jz .else
        push r12
    .else:
NEXT

;; Arithmetic.

DEFCODE "1+", incr
    add r12, 1
NEXT

DEFCODE "1-", decr
    sub r12, 1
NEXT

DEFCODE "8+", incr8
    add r12, 8
NEXT

DEFCODE "8-", decr8
    sub r12, 8
NEXT

DEFCODE "+", add
    pop r13
    add r12, r13
NEXT

DEFCODE "-", sub
    pop r13
    sub r13, r12
    mov r12, r13
NEXT

DEFCODE "*", mul
    pop r13
    imul r12, r13
NEXT

DEFCODE "/MOD", divmod
    pop rax
    cqo
    idiv r12
    push rdx
    mov r12, rax
NEXT

;; Comparisons.

%macro CMP 1
    xor rax, rax
    pop r13
    cmp r13, r12
    set%+1 al
    mov r12, rax
%endmacro

DEFCODE "=", equ
    CMP e
NEXT

DEFCODE "<>", nequ
    CMP ne
NEXT

DEFCODE "<", lt
    CMP l
NEXT

DEFCODE ">", gt
    CMP g
NEXT

DEFCODE "<=", le
    CMP le
NEXT

DEFCODE ">=", ge
    CMP ge
NEXT

%macro TEST 1
    xor rax, rax
    test r12, r12
    set%+1 al
    mov r12, rax
%endmacro

DEFCODE "0=", zequ
    TEST z
NEXT

DEFCODE "0<>", znequ
    TEST nz
NEXT

DEFCODE "0<", zlt
    TEST l
NEXT

DEFCODE "0>", zgt
    TEST g
NEXT

DEFCODE "0<=", zle
    TEST le
NEXT

DEFCODE "0>=", zge
    TEST ge
NEXT

;; Bitwise operations.

DEFCODE "AND", and
    pop r13
    and r12, r13
NEXT

DEFCODE "OR", or
    pop r13
    or r12, r13
NEXT

DEFCODE "XOR", xor
    pop r13
    xor r12, r13
NEXT

DEFCODE "INVERT", invert
    not r12
NEXT

;; Memory operations.

DEFCODE "!", store
    pop r13
    mov [r12], r13
    pop r12
NEXT

DEFCODE "@", fetch
    mov r12, [r12]
NEXT

DEFCODE "+!", addstore
    pop r13
    add [r12], r13
    pop r12
NEXT

DEFCODE "-!", substore
    pop r13
    sub [r12], r13
    pop r12
NEXT

DEFCODE "C!", cstore
    pop r13
    mov [r12], r13l
    pop r12
NEXT

DEFCODE "C@", cfetch
    mov r12l, [r12]
    movzx r12, r12l
NEXT

DEFCODE "C@C!", ccopy
    mov rbx, rsi

    mov rdi, r12
    pop rsi
    movsb
    push rsi
    mov r12, rdi

    mov rsi, rbx
NEXT

DEFCODE "CMOVE", cmove
    mov rbx, rsi

    mov rcx, r12
    pop rdi
    pop rsi
    rep movsb
    pop r12

    mov rsi, rbx
NEXT

;; Direct stack opreations.

DEFCODE ">R", tor
    RBPUSH r12
    pop r12
NEXT

DEFCODE "R>", fromr
    push r12
    RBPOP r12
NEXT

DEFCODE "RSP@", rspfetch
    push r12
    mov r12, rbp
NEXT

DEFCODE "RSP!", rspstore
    mov rbp, r12
    pop r12
NEXT

DEFCODE "RDROP", rdrop
    add rbp, 8
NEXT

DEFCODE "DSP@", dspfetch
    push r12
    mov r12, rsp
NEXT

DEFCODE "DSP!", dspstore
    mov rsp, r12
    pop r12
NEXT

;; Input, output.

DEFCODE "KEY", key
    push r12
    call _key
    mov r12, rax
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
    mov rbx, [keybuf.@]
    cmp rbx, [keybuf.$]
    jge .fill
    xor rax, rax
    mov al, [rbx]
    add qword [keybuf.@], 1
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
    mov [keybuf.$], rax
    mov rax, keybuf
    mov [keybuf.@], rax
jmp _key

align 16
.exit:
    SYSCALL SYS_EXIT, rax
jmp .exit

DEFCODE "EMIT", emit
    push r12
    SAVE
    SYSCALL SYS_WRITE, 1, rsp, 1
    RESTORE
    pop rax
    pop r12
NEXT

DEFCODE "WORD", word
    push r12
    call _word
    push rdi
    mov r12, rcx
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
        push rdi
        call _key
        pop rdi
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
    mov rcx, r12
    pop rdi
    call _number
    push rax
    mov r12, rcx
NEXT

align 16
_number:
    xor rax, rax
    xor rbx, rbx

    test rcx, rcx
    jz .empty

    mov rdx, [var_base]

    mov bl, [rdi]
    add rdi, 1
    cmp bl, '-'
    sete r13l
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
    test r13l, r13l
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
    SYSCALL SYS_WRITE, 1, rax, r12
    RESTORE
    pop r12
NEXT

DEFCODE "CHAR", char
    push r12
    call _word
    xor r12, r12
    mov r12l, [rdi]
NEXT

;; Dictionary lookup.

DEFCODE "FIND", find
    mov rcx, r12
    pop rdi
    call _find
    mov r12, rax
NEXT

align 16
_find:
    mov rbx, rsi

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

    mov rsi, rbx
    mov rax, rdx
ret

align 16
.null:
    mov rsi, rbx
    xor rax, rax
ret

DEFCODE ">CFA", tcfa
    mov rdi, r12
    call _tcfa
    mov r12, rdi
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

DEFCODE "CREATE", create
    mov rcx, r12
    pop rbx

    mov rdi, [var_here]
    mov rax, [var_latest]
    stosq

    mov al, cl
    stosb
    mov r15, rsi
    mov rsi, rbx
    rep movsb
    mov rsi, r15
    add rdi, 7
    and rdi, ~7

    mov rax, [var_here]
    mov [var_latest], rax
    mov [var_here], rdi

    pop r12
NEXT

DEFCODE ",", comma
    mov rax, r12
    call _comma
    pop r12
NEXT

align 16
_comma:
    mov rdi, [var_here]
    stosq
    mov [var_here], rdi
ret

DEFCODE "[", lbrac, F_IMMED
    mov qword [var_state], 0
NEXT

DEFCODE "]", rbrac
    mov qword [var_state], 1
NEXT

DEFCODE "IMMEDIATE", immediate, F_IMMED
    mov rdi, [var_latest]
    add rdi, 8
    xor byte [rdi], F_IMMED
NEXT

DEFCODE "HIDDEN", hidden
    add r12, 8
    xor byte [r12], F_HIDDEN
    pop r12
NEXT

DEFCODE "'", tick
    push r12
    lodsq
    mov r12, rax
NEXT

DEFWORD ":", colon
    dq $word
    dq create
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
    dq find
    dq hidden
dq exit

;; Branching.

DEFCODE "BRANCH", branch
    add rsi, [rsi]
NEXT

DEFCODE "0BRANCH", zbranch
    test r12, r12
    pop r12
    jz code_branch
    lodsq
NEXT

;; Interpreter.

DEFWORD "QUIT", quit
    dq rz, rspstore
    dq interpret
dq quit

section .data
align 8
islit: db 0

section .rodata
errmsg: db "PARSE ERROR: "
    .#: equ $ - errmsg
    .nl: db `\n`

DEFCODE "INTERPRET", interpret
    call _word

    mov qword [islit], 0
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
    mov qword [islit], 1
    call _number
    test rcx, rcx
    jnz .error
    mov rbx, rax
    mov rax, lit

    align 16
    .compile:
    mov rdx, [var_state]
    test rdx, rdx
    jz .exec

    call _comma
    mov rcx, [islit]
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
    mov rcx, [islit]
    test rcx, rcx
    jnz .elit
jmp [rax]

align 16
.elit:
    push r12
    mov r12, rbx
NEXT

align 16
.error:
    SAVE
    SYSCALL SYS_WRITE, 2, errmsg, errmsg.#
    mov rcx, [keybuf.@]
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
    mov rax, r12
    pop r12
jmp [rax]

DEFCODE "SYSCALL3", syscall3
    pop r13
    pop r14
    pop r15
    SAVE
    SYSCALL r12, r13, r14, r15
    RESTORE
    mov r12, rax
NEXT

DEFCODE "SYSCALL2", syscall2
    pop r13
    pop r14
    SAVE
    SYSCALL r12, r13, r14
    RESTORE
    mov r12, rax
NEXT

DEFCODE "SYSCALL1", syscall1
    pop r13
    SAVE
    SYSCALL r12, r13
    RESTORE
    mov r12, rax
NEXT

DEFCODE "SYSCALL0", syscall0
    SAVE
    SYSCALL r12
    RESTORE
    mov r12, rax
NEXT

DEFWORD "LAST", last, F_HIDDEN
dq exit

;; 640K ought to be enough for anybody.

section .bss
alignb 4096
data: resb 655360
    .$:
