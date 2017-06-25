\ Text constants.

: '\n' 10 ;
: BL 32 ;

: CR '\n' EMIT ;
: SPACE BL EMIT ;

\ Arithmetic, booleans.

: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: NEGATE 0 SWAP - ;

: TRUE 1 ;
: FALSE 0 ;
: NOT 0= ;

\ Character literals.

: LITERAL IMMEDIATE ' LIT , , ;
: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

\ Compiler helpers.

: [COMPILE] IMMEDIATE WORD FIND >CFA , ;

: RECURSE IMMEDIATE LATEST @ >CFA , ;

\ Control structures.

: IF IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: THEN IMMEDIATE DUP HERE @ SWAP - SWAP !  ;
: ELSE IMMEDIATE ' BRANCH , HERE @ 0 , SWAP DUP HERE @ SWAP - SWAP ! ;
: UNLESS IMMEDIATE ' NOT , [COMPILE] IF ;

: BEGIN IMMEDIATE HERE @ ;
: UNTIL IMMEDIATE ' 0BRANCH , HERE @ - , ;
: AGAIN IMMEDIATE ' BRANCH , HERE @ - , ;
: WHILE IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: REPEAT IMMEDIATE ' BRANCH , SWAP HERE @ - , DUP HERE @ SWAP - SWAP ! ;

\ The comment.

: ( IMMEDIATE
    1
    BEGIN
        KEY
        DUP '(' = IF
            DROP 1+
        ELSE
            ')' = IF 1- THEN
        THEN
    DUP 0= UNTIL
    DROP
;

\ More stack operations.

: NIP ( x y -- y) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u ) 1+ 8 * DSP@ + @ ;

\ Output.

: SPACES ( n -- )
    BEGIN
        DUP 0>
    WHILE
        SPACE
        1-
    REPEAT
    DROP
;

: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

: U. ( u -- )
    BASE @ /MOD
    ?DUP IF
        RECURSE
    THEN
    DUP 10 < IF
        '0'
    ELSE
        10 - 'A'
    THEN
    + EMIT
;

: .S ( -- )
    DSP@
    BEGIN
        DUP S0 @ <
    WHILE
        DUP @ U. SPACE
        8+
    REPEAT
    DROP
;

: UWIDTH ( u -- width )
    BASE @ /
    ?DUP IF
        RECURSE 1+
    ELSE
        1
    THEN
;

: U.R ( u width -- ) SWAP DUP UWIDTH ROT SWAP - SPACES U. ;

: .R ( n width -- )
    SWAP
    DUP 0< IF
        NEGATE 1 SWAP ROT 1-
    ELSE
        0 SWAP ROT
    THEN
    SWAP DUP UWIDTH ROT SWAP -
    SPACES
    SWAP IF
        '-' EMIT
    THEN
    U.
;

: . ( n -- ) 0 .R SPACE ;
: U. ( n -- ) U. SPACE ;
: ? ( addr -- ) @ . ;

\ Stack/memory manipulation.

: WITHIN ( c a b -- bool )
    -ROT OVER
    <= IF
        > IF
            TRUE
        ELSE
            FALSE
        THEN
    ELSE
        2DROP
        FALSE
    THEN
;

: DEPTH ( -- n ) S0 @ DSP@ - 8- ;

: ALIGNED ( addr -- addr ) 7 + 7 INVERT AND ;
: ALIGN ( -- ) HERE @ ALIGNED HERE ! ;

: C, ( byte -- ) HERE @ C! 1 HERE +! ;

\ Strings.

: S" IMMEDIATE ( -- addr len )
    STATE @ IF
        ' LITSTRING ,
        HERE @
        0 ,
        BEGIN
            KEY
            DUP '"' <>
        WHILE
            C,
        REPEAT
        DROP
        DUP
        HERE @ SWAP -
        8-
        SWAP !
        ALIGN
    ELSE
        HERE @
        BEGIN
            KEY
            DUP '"' <>
        WHILE
            OVER C!
            1+
        REPEAT
        DROP
        HERE @ -
        HERE @
        SWAP
    THEN
;

: ." IMMEDIATE ( -- )
    STATE @ IF
        [COMPILE] S"
        ' TELL ,
    ELSE
        BEGIN
            KEY
            DUP '"' = IF
                DROP
                EXIT
            THEN
            EMIT
        AGAIN
    THEN
;

\ Constants, variables, values.

: CONSTANT
    WORD
    CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
;

: ALLOT ( n -- addr )
    HERE @ SWAP
    HERE +!
;

: CELLS ( n -- n ) 8 * ;

: VARIABLE
    1 CELLS ALLOT
    WORD CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
;

: VALUE ( n -- )
    WORD CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
;

: TO IMMEDIATE ( n -- )
    WORD FIND >DFA 8+
    STATE @ IF
        ' LIT ,
        ,
        ' ! ,
    ELSE
        !
    THEN
;

: +TO IMMEDIATE ( n -- )
    WORD FIND >DFA 8+
    STATE @ IF
        ' LIT ,
        ,
        ' +! ,
    ELSE
        +!
    THEN
;

\ Dictionary inspection.

: ID. ( addr -- )
    8+
    DUP C@ F_LENMASK AND
    BEGIN
        DUP 0>
    WHILE
        SWAP 1+
        DUP C@
        EMIT
        SWAP 1-
    REPEAT
    2DROP
;

: ?HIDDEN ( addr -- bool ) 8+ C@ F_HIDDEN AND ;
: ?IMMEDIATE ( addr -- bool ) 8+ C@ F_IMMED AND ;

: WORDS ( -- )
    LATEST @
    BEGIN
        ?DUP
    WHILE
        DUP ?HIDDEN NOT IF
            DUP ID.
            SPACE
        THEN
        @
    REPEAT
    CR
;

: FORGET
    WORD FIND
    DUP @ LATEST !
    HERE !
;

: DUMP ( addr len -- )
    BASE @ -ROT
    HEX
    BEGIN
        ?DUP
    WHILE
        OVER 16 U.R SPACE
        2DUP 1- 15 AND 1+
        BEGIN
            ?DUP
        WHILE
            SWAP
            DUP C@
            2 .R SPACE
            1+ SWAP 1-
        REPEAT
        DROP
        2DUP 1- 15 AND 1+
        BEGIN
            ?DUP
        WHILE
            SWAP
            DUP C@
            DUP 32 128 WITHIN IF
                EMIT
            ELSE
                DROP '.' EMIT
            THEN
            1+ SWAP 1-
        REPEAT
        DROP
        CR
        DUP 1- 15 AND 1+
        TUCK - >R + R>
    REPEAT
    DROP
    BASE !
;

\ Case.

: CASE IMMEDIATE 0 ;
: OF IMMEDIATE
    ' OVER ,
    ' = ,
    [COMPILE] IF
    ' DROP ,
;
: ENDOF IMMEDIATE [COMPILE] ELSE ;
: ENDCASE IMMEDIATE
    ' DROP ,
    BEGIN
        ?DUP
    WHILE
        [COMPILE] THEN
    REPEAT
;

\ Decompiler.

: CFA> ( addr -- addr )
    LATEST @
    BEGIN
        ?DUP
    WHILE
        2DUP SWAP < IF
            NIP
            EXIT
        THEN
        @
    REPEAT
    DROP
    0
;

: SEE
    WORD FIND
    HERE @
    LATEST @
    BEGIN
        2 PICK OVER <>
    WHILE
        NIP
        DUP @
    REPEAT
    DROP SWAP
    ':' EMIT SPACE DUP ID. SPACE
    DUP ?IMMEDIATE IF ." IMMEDIATE " THEN
    >DFA
    BEGIN
        2DUP >
    WHILE
        DUP @
        CASE
        ' LIT OF
            8+ DUP @ .
        ENDOF
        ' LITSTRING OF
            [ CHAR S ] LITERAL EMIT '"' EMIT SPACE
            8+ DUP @
            SWAP 8+ SWAP
            2DUP TELL
            '"' EMIT SPACE
            + ALIGNED
            8-
        ENDOF
        ' 0BRANCH OF
            ." 0BRANCH ( " 8+ DUP @ . ." ) "
        ENDOF
        ' BRANCH OF
            ." BRANCH ( " 8+ DUP @ . ." ) "
        ENDOF
        ' ' OF
            [ CHAR ' ] LITERAL EMIT SPACE
            8+ DUP @
            CFA>
            ID. SPACE
        ENDOF
        ' EXIT OF
            2DUP 8+ <> IF
                ." EXIT "
            THEN
        ENDOF
        DUP CFA> ID. SPACE
        ENDCASE
        8+
    REPEAT
    ';' EMIT CR
    2DROP
;

\ Execution tokens.

: :NONAME ( -- xt )
    0 0 CREATE
    HERE @
    DOCOL ,
    ]
;

: ['] IMMEDIATE ' LIT , ;

\ Exceptions.

: EXCEPTION-MARKER RDROP 0 ;

: CATCH ( xt -- exn? )
    DSP@ 8+ >R
    ' EXCEPTION-MARKER 8+ >R
    EXECUTE
;

: THROW ( n -- )
    ?DUP IF
        RSP@
        BEGIN
            DUP R0 8- <
        WHILE
            DUP @ ' EXCEPTION-MARKER 8+ = IF
                8+ RSP!
                DUP DUP DUP
                R>
                8- SWAP OVER !
                DSP! EXIT
            THEN
            8+
        REPEAT
        DROP
        CASE
        -1 OF
            ." ABORTED" CR
        ENDOF
        ." UNCAUGHT THROW " DUP . CR
        ENDCASE
        QUIT
    THEN
;

: ABORT ( -- ) -1 THROW ;

: PRINT-STACK-TRACE ( -- )
    RSP@
    BEGIN
        DUP R0 8- <
    WHILE
        DUP @
        CASE
        ' EXCEPTION-MARKER 8+ OF
            ." CATCH ( DSP=" 8+ DUP @ U. ." ) "
        ENDOF
        DUP CFA>
        ?DUP IF
            2DUP ID.
            [ CHAR + ] LITERAL EMIT
            SWAP >DFA 8+ - .
        THEN
        ENDCASE
        8+
    REPEAT
    DROP
    CR
;

\ C strings.

: Z" IMMEDIATE
    STATE @ IF
        ' LITSTRING ,
        HERE @
        0 ,
        BEGIN
            KEY DUP '"' <>
        WHILE
            HERE @ C!
            1 HERE +!
        REPEAT
        0 HERE @ C!
        1 HERE +!
        DROP
        DUP HERE @ SWAP -
        8- SWAP !
        ALIGN
        ' DROP ,
    ELSE
        HERE @
        BEGIN
            KEY DUP '"' <>
        WHILE
            OVER C! 1+
        REPEAT
        DROP
        0 SWAP C!
        HERE @
    THEN
;

: STRLEN ( str -- len )
    DUP
    BEGIN
        DUP C@ 0<>
    WHILE
        1+
    REPEAT
    SWAP -
;

: CSTRING ( addr len -- c-addr )
    SWAP OVER
    HERE @ SWAP
    CMOVE
    HERE @ +
    0 SWAP C!
    HERE @
;

\ Environment.

: ARGC ( -- n ) S0 @ @ ;

: ARGV ( n -- str len )
    1+ CELLS S0 @ +
    @
    DUP STRLEN
;

: ENVIRON ( -- addr ) ARGC 2 + CELLS S0 @ + ;

\ Syscalls.

: BYE 0 SYS_EXIT SYSCALL1 ;

: UNUSED ( -- n ) LIMIT HERE @ - 8 / ;

\ File I/O.

: R/O ( -- fam ) O_RDONLY ;
: R/W ( -- fam ) O_RDWR ;

: OPEN-FILE ( str len fam -- fd errno )
    -ROT
    CSTRING
    SYS_OPEN SYSCALL2
    DUP
    DUP 0< IF
        NEGATE
    ELSE
        DROP 0
    THEN
;

: CREATE-FILE ( str len fam -- fd errno )
    O_CREAT O_TRUNC OR OR
    -ROT
    CSTRING
    420 -ROT
    SYS_OPEN SYSCALL3
    DUP
    DUP 0< IF
        NEGATE
    ELSE
        DROP 0
    THEN
;

: CLOSE-FILE ( fd -- errno ) SYS_CLOSE SYSCALL1 NEGATE ;

: READ-FILE ( str len fd -- rlen errno )
    >R SWAP R>
    SYS_READ SYSCALL3
    DUP
    DUP 0< IF
        NEGATE
    ELSE
        DROP 0
    THEN
;

: PERROR ( errno str len -- ) TELL ." : ERRNO=" . CR ;

\ Assembler.

HEX

: NEXT IMMEDIATE 48 C, AD C, FF C, 20 C, ;

: ;CODE IMMEDIATE
    [COMPILE] NEXT
    ALIGN
    LATEST @ DUP
    HIDDEN
    DUP >DFA SWAP >CFA !
    [COMPILE] [
;

: =NEXT ( addr -- next? )
       DUP C@ 48 <> IF DROP FALSE EXIT THEN
    1+ DUP C@ AD <> IF DROP FALSE EXIT THEN
    1+ DUP C@ FF <> IF DROP FALSE EXIT THEN
    1+     C@ 20 <> IF      FALSE EXIT THEN
    TRUE
;

DECIMAL

: (INLINE) ( cfa -- )
    @
    BEGIN
        DUP =NEXT NOT
    WHILE
        DUP C@ C,
        1+
    REPEAT
    DROP
;

: INLINE IMMEDIATE
    WORD FIND >CFA
    DUP @ DOCOL = IF
        ." Cannot INLINE FORTH words" CR ABORT
    THEN
    (INLINE)
;

HIDE =NEXT

\ Welcome.

: WELCOME ( -- )
    S" TEST-MODE" FIND NOT IF
        ." INDIANA VERSION " VERSION . CR
        UNUSED . ." CELLS REMAINING" CR
        ." OK "
    THEN
;

WELCOME
HIDE WELCOME
