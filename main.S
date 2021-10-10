//A forth interpreter based on jonesforth, written for x86_64
//by Kyle Neil

//register map:
//rsi -> pointer to next word
//rax -> pointer to current word
//rbp -> pointer to return stack (as usual)
//rsp -> pointer to parameter stack

//any register not mentioned here is free to use as a gp register

.macro NEXT //execute the next Forth word
	lodsq //(rsi) -> rax, rsi += 8
	jmp *(%rax)
.endm

.macro PUSHRSP reg //push register on return stack (actual stack)
	lea -8(%rbp), %rbp //make space for reg
	movq \reg, (%rbp) //put reg in there
.endm

.macro POPRSP reg //pop register from return stack
	mov (%rbp), \reg //get reg from rbp
	lea 8(%rbp), %rbp //delete reg space
.endm

.macro SAVEREG
	mov %rsi, %r14
	mov %rdi, %r15
.endm

.macro RESTREG
	mov %r15, %rdi
	mov %r14, %rsi
.endm

.text
.align 8 //align to 8-byte boundaries

//this is called when executing a new forth word (not continuing an existing one)
DOCOL:
	PUSHRSP %rsi //push previous address on return stack
	addq $8, %rax //rax points to codeword, so add 8 and make rsi point to first data word
	movq %rax, %rsi
	NEXT

.text
.globl main
main:
	//turn off line buffering for stdout
	mov stdout, %rdi
	mov $0, %rsi
	call setbuf
	
	cld //set string ops to move the right way
	mov %rsp, var_S0 //save initial stack frame
	mov $return_stack_top, %rbp //set up rbp to point to the right area
	
	call set_up_data_segment 
	
	mov $cold_start, %rsi
	
	NEXT //and we're off!
	
	.section .rodata
cold_start:
	.quad QUIT //our first forth word, which is just interpreting the next word infinitely

.set F_IMMED, 0x80 //flags for special compiled forth words
.set F_HIDDEN, 0x20
.set F_LENMASK, 0x1f
.set link, 0 //linked list for word defns

//this is a macro to generate a forth word
//it includes a link pointer to point to the previous word declared,
//a name to look up and a length of that name, 
//and the actual code, prefixed by a DOCOL, as always
.macro defword name, namelen, flags=0, label
	.section .rodata
	.align 8
	.global name_\label
name_\label:
	.quad link //pointer to previous word
	.set link, name_\label
	.byte \flags+\namelen
	.ascii "\name"
	.align 8
	.globl \label
\label:
	.quad DOCOL
.endm

//this is a similar macro, except it is written to define words directly
//in assembly.  The code field is the same, as forth does not treat forth words
//and assembly words differently.
.macro defcode name, namelen, flags=0, label
	.section .rodata
	.align 8
	.globl name_\label
name_\label:
	.quad link
	.set link, name_\label
	.byte \flags+\namelen
	.ascii "\name"
	.align 8
	.globl \label
\label:
	.quad code_\label
	.text
	.align 8
	.globl code_\label
code_\label:
	//asm here
.endm

//start of forth words implemented in assembly for speed, and because
//we have to start somewhere.
//this will form the base of the rest of our forth system

defcode "DROP",4,,DROP //(n -- )
	pop %rax
	NEXT

defcode "SWAP",4,,SWAP //(n1 n2 -- n2 n1)
	pop %rax
	pop %rbx
	push %rax
	push %rbx
	NEXT

defcode "DUP",3,,DUP //(n -- n n)
	mov (%rsp), %rax
	push %rax
	NEXT

defcode "OVER",4,,OVER //(n1 n2 -- n1 n2 n1)
	mov 8(%rsp), %rax
	push %rax
	NEXT

defcode "ROT",3,,ROT //(n1 n2 n3 -- n2 n3 n1)
	pop %rax
	pop %rbx
	pop %rcx
	push %rbx
	push %rax
	push %rcx
	NEXT

defcode "-ROT",4,,NROT //(n1 n2 n3 -- n3 n1 n2)
	pop %rax
	pop %rbx
	pop %rcx
	push %rax
	push %rcx
	push %rbx
	NEXT

defcode "2DROP",5,,TWODROP //(n n -- )
	pop %rax
	pop %rax
	NEXT

defcode "2DUP",4,,TWODUP //(n n -- n n n n)
	mov (%rsp), %rax
	mov 8(%rsp), %rbx
	push %rbx
	push %rax
	NEXT

defcode "2SWAP",5,,TWOSWAP //(n1 n2 n3 n4 -- n3 n4 n1 n2)
	pop %rax
	pop %rbx
	pop %rcx
	pop %rdx
	push %rbx
	push %rax
	push %rdx
	push %rcx
	NEXT

defcode "?DUP",4,,QDUP //DUP if non-zero
	movq (%rsp), %rax
	test %rax, %rax
	jz 1f
	push %rax
1:	NEXT

defcode "1+",2,,INCR //(n -- n+1)
	incq (%rsp)
	NEXT

defcode "1-",2,,DECR//(n -- n-1)
	decq (%rsp)
	NEXT

defcode "4+",2,,INCR4//(n -- n+4)
	addq $4, (%rsp)
	NEXT

defcode "4-",2,,DECR4//(n -- n-4)
	subq $4, (%rsp)
	NEXT

//going to throw in INCR8 and DECR8 because we're using 8-byte pointers and this is a 64-bit system
defcode "8+",2,,INCR8 //(n -- n+8)
	addq $8, (%rsp)
	NEXT

defcode "8-",2,,DECR8 //(n -- n-8)
	subq $8, (%rsp)
	NEXT

defcode "+",1,,ADD //(n1 n2 -- n1 + n2)
	pop %rax
	addq %rax, (%rsp)
	NEXT

defcode "-",1,,SUB //(n1 n2 -- n1 - n2)
	pop %rax
	subq %rax, (%rsp)
	NEXT

defcode "*",1,,MUL //(n1 n2 -- n1 * n2)
	pop %rax
	pop %rbx
	imulq %rbx, %rax
	push %rax
	NEXT

defcode "/MOD",4,,DIVMOD //basic division that pushes quotient and remainder on the stack
	xor %rdx, %rdx
	pop %rbx
	pop %rax
	idivq %rbx
	push %rdx
	push %rax
	NEXT

defcode "/",1,,DIV
	xor %rdx, %rdx
	pop %rbx
	pop %rax
	idivq %rbx
	push %rax
	NEXT

defcode "MOD",3,,MOD
	xor %rdx, %rdx
	pop %rbx
	pop %rax
	idivq %rbx
	push %rdx
	NEXT

//if I were to throw in MMX/SSE algorithms in here, this would be the place to put them


//here, a 1 is "TRUE" and a 0 is "FALSE"
//all of the following abide by (n1 n2 -- f) depending on condition
defcode "=",1,,EQU
	pop %rax
	pop %rbx
	cmp %rax, %rbx
	sete %al //set al to 1 if condition == 1
	movzbq %al, %rax //extend to 64-bit register
	pushq %rax
	NEXT

defcode "<>",2,,NEQU
	pop %rax
	pop %rbx
	cmp %rbx, %rax
	setne %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "<",1,,LT
	pop %rax
	pop %rbx
	cmp %rax, %rbx
	setl %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode ">",1,,GT
	pop %rax
	pop %rbx
	cmp %rax, %rbx
	setg %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "<=",2,,LE
	pop %rax
	pop %rbx
	cmp %rax, %rbx
	setle %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode ">=",2,,GE
	pop %rax
	pop %rbx
	cmp %rax, %rbx
	setge %al
	movzbq %al, %rax
	pushq %rax
	NEXT

//zero test words abide by (n -- f)
defcode "0=",2,,ZEQU
	pop %rax
	test %rax, %rax
	setz %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "0<>",3,,ZNEQU
	pop %rax
	test %rax, %rax
	setne %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "0<",2,,ZLT
	pop %rax
	test %rax, %rax
	setl %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "0>",2,,ZGT
	pop %rax
	test %rax, %rax
	setg %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "0<=",3,,ZLE
	pop %rax
	test %rax, %rax
	setle %al
	movzbq %al, %rax
	pushq %rax
	NEXT

defcode "0>=",3,,ZGE
	pop %rax
	test %rax, %rax
	setge %al
	movzbq %al, %rax
	pushq %rax
	NEXT


// ( a b flag -- b a | a b )
//not sure if above is correct...
//swaps top two items if flag is true
defcode "CSWAP",5,,CSWAP
	pop %rbx
	pop %rcx
	pop %rax
	test %rax, %rax
	cmovnz %rbx, %rdx
	cmovnz %rcx, %rbx
	cmovnz %rdx, %rcx
	push %rbx
	push %rcx
	NEXT

//finally at the bitwise ops
defcode "AND",3,,AND
	pop %rax
	andq %rax, (%rsp)
	NEXT

defcode "OR",2,,OR
	pop %rax
	orq %rax, (%rsp)
	NEXT

defcode "XOR",3,,XOR
	pop %rax
	xorq %rax, (%rsp)
	NEXT

defcode "INVERT",6,,INVERT
	notq (%rsp)
	NEXT

defcode "NEGATE",6,,NEGATE
	pop %rax
	NEG %rax
	push %rax
	NEXT

//back to forth operations
defcode "EXIT",4,,EXIT
	POPRSP %rsi //pop return stack into rsi
	NEXT

defcode "LIT",3,,LIT //prepend before a literal number.  This pushes the number on the stack instead of trying to execute it.
	mov %rsi, %rax
	add $8, %rsi //same as NEXT but no jmp *(%rax)
	push (%rax)
	NEXT

//memory read and write
defcode "!",1,,STORE //(addr data -- )
	pop %rbx
	pop %rax
	mov %rax, (%rbx)
	NEXT

defcode "@",1,,FETCH //(addr -- )
	pop %rbx
	mov (%rbx), %rax
	push %rax
	NEXT

//adds a specified amount to a memory location
defcode "+!",2,,ADDSTORE //(addr add -- )
	pop %rbx
	pop %rax
	addq %rax, (%rbx)
	NEXT

defcode "-!",2,,SUBSTORE
	pop %rbx
	pop %rax
	subq %rax, (%rbx)
	NEXT

//read and write a byte
defcode "C!",2,,STOREBYTE //(addr byte -- )
	pop %rbx
	pop %rax
	movb %al, (%rbx)
	NEXT

defcode "C@",2,,FETCHBYTE //(addr -- byte)
	pop %rbx
	xor %rax, %rax //clear upper part of rax
	movb (%rbx), %al
	push %rax
	NEXT

defcode "C@C!",3,,CCOPY //(src dest -- src+8 dest+8)
	movq 8(%rsp), %rbx
	movb (%rbx), %al
	pop %rdi
	mov %al, (%rdi)
	push %rdi
	incq 8(%rsp)
	NEXT

defcode "CMOVE",5,,CMOVE
	mov %rsi, %rdx
	pop %rcx
	pop %rdi
	pop %rsi
	rep movsb
	mov %rdx, %rsi
	NEXT

//variable macro to hold a few variables outside of the stack (global state)
.macro defvar name, namelen, flags=0, label, initial=0
	defcode \name, \namelen, \flags, \label
	push $var_\name
	NEXT	
	.data
	.align 8
var_\name:
	.quad \initial
.endm

defvar "STATE",5,,STATE
defvar "HERE",4,,HERE
defvar "LATEST",6,,LATEST,name_SYSCALL0 //must be last for some reason...?
defvar "S0",2,,SZ
defvar "BASE",4,,BASE,10 //radix

//constant macro, similar to defvar but only pushes to stack
.macro defconst name, namelen, flags=0, label, value
	defcode \name,\namelen,\flags,\label
	push $\value
	NEXT
.endm

defconst "VERSION",7,,VERSION,-1 //whatever the most recent git version is
defconst "R0",2,,RZ,return_stack_top //pointer to top of stack in memory
defconst "DOCOL",5,,__DOCOL,DOCOL
defconst "F_IMMED",7,,__F_IMMED,F_IMMED //should the word be executed immediately, even when being compiled?
defconst "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN //is the word visible to the user?
defconst "F_LENMASK",9,,__F_LENMASK,F_LENMASK //will become useful once we get to compiling words

defconst "O_RDONLY",8,,__O_RDONLY,0
defconst "O_WRONLY",8,,__O_WRONLY,1
defconst "O_RDWR",8,,__O_RDWR,2
defconst "O_CREAT",7,,__O_CREAT,0100
defconst "O_EXCL",6,,__O_EXCL,0200
defconst "O_TRUNC",7,,__O_TRUNC,01000
defconst "O_APPEND",8,,__O_APPEND,02000
defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000

//some words to manuipulate the return stack
//note: these values must be popped before the word returns or the interpreter will
//return to that address and probably crash
defcode ">R",2,,TOR //(n -- )
	pop %rax
	PUSHRSP %rax
	NEXT

defcode "R>",2,,FROMR //( -- n)
	POPRSP %rax
	push %rax
	NEXT

//these words actually get and set the return stack pointer (rbp)
defcode "RSP@",4,,RSPFETCH //( -- n)
	push %rbp
	NEXT

defcode "RSP!",4,,RSPSTORE //(n -- )
	pop %rbp
	NEXT

defcode "RDROP",5,,RDROP //( -- )
	addq $8, %rbp
	NEXT

//these words get and set the parameter stack pointer directly
defcode "DSP@",4,,DSPFETCH
	mov %rsp, %rax
	push %rax
	NEXT

defcode "DSP!",4,,DSPSTORE
	pop %rsp
	NEXT

//we're approaching a fully functional forth system here.  All the basic mechanics are in place, and all 
//that is left is to make it user-interactive instead of a fixed program.
//this is where we get to the big boy stuff.

//read a byte from stdin and push it on the stack.
defcode "KEY",3,,KEY
	mov %rsi, %r14 //save rsi, needed as operand register
	call _KEY
	mov %r14, %rsi
	push %rax
	NEXT
_KEY:	
	mov (currkey), %rbx //no $ indicates a memory read
	cmp (bufftop), %rbx //at end of buffer?
	jge 1f
	xor %rax, %rax
	mov (%rbx), %al //get next char, increment bufptr
	inc %rbx
	mov %rbx, (currkey) //increment currkey
	ret
1: //get input from read syscall
	xor %rax, %rax //read syscall
	xor %rdi, %rdi //use stdin
	mov $buffer, %rsi
	mov %rsi, currkey
	mov $BUFFER_SIZE, %rdx
	syscall
	
	test %rax, %rax //if rax <= 0, exit
	jbe 2f
	addq %rax, %rsi //buffer + rax = bufftop
	mov %rsi, bufftop
	jmp _KEY
2: //error / EOF, exit
	mov $1, %rdi //return exit code 1
	call exit
	
	.data
	.align 8
currkey:
	.quad buffer //current place
bufftop:
	.quad buffer //last valid data

defcode "EMIT",4,,EMIT
	SAVEREG
	
	pop %rdi
	call putchar
	
	RESTREG
	NEXT

//get a valid word (search for a space)
defcode "WORD",4,,WORD
	mov %rsi, %r14 //save rsi again
	call _WORD
	push %rdi
	push %rcx
	mov %r14, %rsi
	NEXT
_WORD:

1:
	call _KEY //char returned in rax
	cmpb $'\\', %al // if it's a comment, skip it
	je 3f
	cmpb $' ', %al 
	jbe 1b
	
	mov $word_buffer, %rdi //ptr to word buffer
2:
	stosb //add character to return buffer
	call _KEY //get next key
	cmpb $' ', %al //if not blank, keep looking
	ja 2b
	
	sub $word_buffer, %rdi
	mov %rdi, %rcx //return length
	mov $word_buffer, %rdi //return address
	ret
3:
	call _KEY
	cmpb $'\n', %al //eol yet?
	jne 3b
	jmp 1b

	.data
word_buffer: //forth words can only be 512 bytes long!
	.space 512

//parse a numeric string and push the result
//this would benefit from a libc linking...
defcode "NUMBER",6,,NUMBER
	pop %rcx
	pop %rdi
	call _NUMBER
	push %rax
	push %rcx
	NEXT

_NUMBER:
	xor %rax, %rax
	xor %rbx, %rbx
	
	test %rcx, %rcx //if length is 0, return 0
	jz 5f
	
	movq var_BASE, %rdx //get base value
	
	mov (%rdi), %bl //mov first char of string into bl
	inc %rdi
	push %rax
	cmpb $'-', %bl
	jnz 2f
	pop %rax
	push %rbx
	dec %rcx
	jnz 1f
	pop %rbx
	movq $1, %rcx
	ret
1:
	imulq %rdx, %rax //rax *= BASE
	movb (%rdi), %bl
	inc %rdi
2:
	subb $'0', %bl //< '0'?
	jb 4f
	cmp $10, %bl //<= '9'?
	jb 3f
	subb $17, %bl // < 'A'?
	jb 4f
	addb $10, %bl
3:
	cmp %dl, %bl // >= BASE?
	jge 4f
	
	add %rbx, %rax
	dec %rcx
	jnz 1b
4:
	pop %rbx
	test %rbx, %rbx
	jz 5f
	neg %rax
5:
	ret

//have enough infastructure to be able to read and parse text from the command line, and now we need to implement the dictonary that ties everything together.
//this could be a hash table, but is a linked list for now.
defcode "FIND",4,,FIND
	pop %rcx
	pop %rdi
	call _FIND
	push %rax
	NEXT

_FIND:
	push %rsi //save rsi so we can use it in a string comparison
	
	//search backwards through the dictonary to find the thing
	mov var_LATEST, %rdx
1:	test %rdx, %rdx //end of linked list?
	je 4f
	
	//cmp lengths of string, and if they are equal, cmp strings themselves
	xor %rax, %rax
	movb 8(%rdx), %al //al is flags and lengths field
	andb $(F_HIDDEN|F_LENMASK), %al
	cmpb %cl, %al
	jne 2f
	
	push %rcx //lengths are the same, compare the strings in detail
	push %rdi
	lea 9(%rdx), %rsi
	repe cmpsb //this is a really cool x86 instruction that compares bytes in a loop!
	pop %rdi
	pop %rcx
	jne 2f //not the same
	
	pop %rsi //strings are the same
	mov %rdx, %rax
	ret
2:
	mov (%rdx), %rdx //loop through the next entry in the list
	jmp 1b
4:
	pop %rsi
	xor %rax, %rax //return 0 (not found)
	ret

//return a pointer to the "DOCOL" portion, not the link portion of the word.
defcode ">CFA",4,,TCFA
	pop %rdi
	call _TCFA
	push %rdi
	NEXT

_TCFA:
	xor %rax, %rax
	add $8, %rdi //skip link pointer
	movb (%rdi), %al
	inc %rdi //skip flags byte
	andb $F_LENMASK, %al //get length of name
	add %rax, %rdi //skip that much
	addq $7, %rdi
	andq $~7,%rdi
	ret

//just >CFA minus the DOCOL
//notice that we're defining a Forth word here, not actual assembly code
defword ">DFA",4,,TDFA
	.quad TCFA
	.quad INCR8
	.quad EXIT

//make header for a word (add length and name, update here and latest pointers)
defcode "CREATE",6,,CREATE
	pop %rcx //length of name
	pop %rbx //address of name
	movq var_HERE, %rdi
	movq var_LATEST, %rax
	stosq
	
	mov %cl, %al
	stosb
	push %rsi
	mov %rbx, %rsi
	rep movsb
	pop %rsi
	addq $7, %rdi
	andq $~7, %rdi
	
	movq var_HERE, %rax
	movq %rax, var_LATEST
	movq %rdi, var_HERE
	NEXT

//appends a codeword to the current word
defcode ",",1,,COMMA
	pop %rax
	call _COMMA
	NEXT
_COMMA:
	movq var_HERE, %rdi
	stosq
	movq %rdi, var_HERE
	ret

//these are to force the compiler to execute Forth words until it sees a ']'
//this is how we let the user extend the compiler
defcode "[",1,F_IMMED,LBRAC
	xor %rax, %rax
	movq %rax, var_STATE //set state to 0
	NEXT

defcode "]",1,,RBRAC
	movq $1, var_STATE
	NEXT

//Forth word for creating a new Forth word
defword ":",1,,COLON
	.quad WORD //get name
	.quad CREATE //make space for it
	.quad LIT, DOCOL, COMMA //append a DOCOL
	.quad LATEST, FETCH, HIDDEN //make the word hidden for now
	.quad RBRAC //go into 
	.quad EXIT

//stop creating a Forth word
defword ";",1,F_IMMED,SEMICOLON
	.quad LIT, EXIT, COMMA //append an exit so word will return
	.quad LATEST, FETCH, HIDDEN //toggle hidden flag
	.quad LBRAC //go to immediate mode
	.quad EXIT //ret

//this word makes the defined word immediate
defcode "IMMEDIATE",9,F_IMMED,IMMEDIATE
	movq var_LATEST, %rdi
	addq $8, %rdi
	xorb $F_IMMED, (%rdi)
	NEXT

//this is to let Forth know that this is not a user word and is reserved for the system only
defcode "HIDDEN",6,,HIDDEN
	pop %rdi
	addq $8, %rdi
	xorb $F_HIDDEN, (%rdi)
	NEXT

//hide a word
defword "HIDE",4,,HIDE
	.quad WORD
	.quad FIND
	.quad HIDDEN
	.quad EXIT

//appends codeword to currently being defined word
defcode "'",1,,TICK
	lodsq
	pushq %rax
	NEXT

//jmp instruction in Forth
defcode "BRANCH",6,,BRANCH
	add (%rsi), %rsi //add offset to ip
	NEXT

//branch if zero
defcode "0BRANCH",7,,ZBRANCH
	pop %rax
	test %rax, %rax
	jz code_BRANCH
	lodsq
	NEXT

//this does something?
defcode "LITSTRING",9,,LITSTRING
	lodsq
	push %rsi
	push %rax
	addq %rax, %rsi
	addq $7, %rsi
	andq $~7, %rsi //align to 8-byte boundry
	NEXT

defcode "TELL",4,,TELL
	mov %rsi, %r14
	mov $1, %rax
	mov $1, %rdi
	pop %rdx
	pop %rsi
	syscall
	mov %r14, %rsi
	NEXT

//this is the actual interpreter loop we talked about before
defword "QUIT",4,,QUIT
	.quad RZ, RSPSTORE
	.quad INTERPRET
	.quad BRANCH, -16

//and finally, we have the holy grail of interpreters: the interpret function
defcode "INTERPRET",9,,INTERPRET
	mov %rsi, %r14
	call _WORD //get a word
	mov %r14, %rsi
	
	xor %rax, %rax
	movq %rax, interpret_is_lit
	call _FIND //find it in the internal dictionary
	test %rax, %rax
	jz 1f
	
	mov %rax, %rdi
	movb 8(%rdi), %al
	push %ax
	call _TCFA //?
	pop %ax
	andb $F_IMMED, %al
	mov %rdi, %rax
	jnz 4f
	
	jmp 2f
1:
	incq interpret_is_lit
	call _NUMBER
	test %rcx, %rcx
	jnz 6f
	mov %rax, %rbx
	mov $LIT, %rax //literal word
2:
	movq var_STATE, %rdx
	test %rdx, %rdx
	jz 4f //jump if executing
	
	call _COMMA
	mov interpret_is_lit, %rcx
	test %rcx, %rcx
	jz 3f
	mov %rbx, %rax
	call _COMMA
3:	NEXT
4:
	mov interpret_is_lit, %rcx
	test %rcx, %rcx
	jnz 5f
	
	jmp *(%rax)
5:
	push %rbx
	NEXT
6:	
	//can't figure out what the input is, throw an error
	mov %rsi, %r14
	mov $2, %rdi //stderr
	mov $errmsg, %rsi
	mov $errmsgend - errmsg, %rdx
	mov $1, %rax //write actual error message
	syscall
	
	mov (currkey), %rsi //throw some context into error message
	mov %rsi, %rdx
	sub $buffer, %rdx
	cmp $20, %rdx //the length of context is $20, $40 is too long and really confusing...
	jle 7f
	mov $20, %rdx
7:	sub %rdx, %rsi
	mov $1, %rax
	syscall
	
	mov $errmsgnl, %rsi
	mov $1, %rdx
	syscall
	
	mov %r14, %rsi
	NEXT

	.section .rodata
errmsg: .ascii "unknown input:"
errmsgend:
errmsgnl: .ascii "\n" 
	
	.data
	.align 8
interpret_is_lit:
	.quad 0

//?
defcode "CHAR",4,,CHAR
	call _WORD
	xor %rax, %rax
	movb (%rdi), %al
	push %rax
	NEXT

//execute whatever is in *rax
defcode "EXECUTE",7,,EXECUTE
	pop %rax
	jmp *(%rax)

//Forth wrappers for syscalls with different numbers of parameters
defcode "SYSCALL3",8,,SYSCALL3
	mov %rsi, %r14 //save rsi
	pop %rax
	pop %rdi
	pop %rsi
	pop %rdx
	syscall
	push %rax
	mov %r14, %rsi
	NEXT

defcode "SYSCALL2",8,,SYSCALL2
	mov %rsi, %r14
	pop %rax
	pop %rdi
	pop %rsi
	syscall
	push %rax
	mov %r14, %rsi
	NEXT

defcode "SYSCALL1",8,,SYSCALL1
	pop %rax
	pop %rdi
	syscall
	push %rax
	NEXT

defcode "SYSCALL0",8,,SYSCALL0
	pop %rax
	syscall
	push %rax
	NEXT

//this allocates 1M of memory for the user
.text
	.set INITIAL_DATA_SEGMENT_SIZE, 0xFFFFF
set_up_data_segment:
	xor %rdi, %rdi
	mov $12, %rax
	syscall //brk(0)
	
	movq %rax, var_HERE
	addq $INITIAL_DATA_SEGMENT_SIZE, %rax
	movq %rax, %rdi
	mov $12, %rax //allocate some memory and call brk again
	syscall
	ret

.set RET_STACK_SIZE, 0x7FFF
.set BUFFER_SIZE, 0x7FFF

.bss
	.align 4096
return_stack:
	.space RET_STACK_SIZE
return_stack_top:

	.align 4096
buffer:
	.space BUFFER_SIZE
