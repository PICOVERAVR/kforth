// gcc -no-pie -g test.S -o test.out
	.global main

	.text
main:
	
	// $scratch is address
	// scratch is data
	
	//mov $msg_3, %rdi
	//mov $nullptr, %rsi
	//mov $10, %rdx
	
	//call strtol
	
	//push %rax
	
	mov $word_buffer, %rdi
	mov $512, %rsi
	mov stdin, %rdx
	
	call fgets
	
	call strlen
	dec %rax
	push %rax
	push $word_buffer
	
	ret
	
	.data
msg_1:
	.asciz "-4"
msg_2:
	.asciz "4F"
msg_3:
	.asciz "592"

nullptr:
	.quad 0
	
	.bss
scratch:
	.quad
scratch2:
	.quad

word_buffer:
	.space 512
