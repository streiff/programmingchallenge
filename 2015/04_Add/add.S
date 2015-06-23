.data
errMessage:
	.asciz "Number to sum missing. :(\n"

errMessage2:
	.asciz "This is written in assember. You expect me to handle negatives???\n"

answerStart:
	.asciz "add(%ld) -> "

answerEnd:
	.asciz "%ld\n"

.text
	.global main

main:    
	# We'll be doing system calls. Align the stack.
	push %r12
	push %r13
	push %r14

	# Command line argument count
	cmp $2, %rdi
	jne printErrMsg

	# Call atoi on the first argument.
	mov 8(%rsi), %rdi
	call atoi
	mov %rax, %r13

	cmp $0, %r13
	jl printErrMsg2
 
printAnswerStart:
 	mov $answerStart, %rdi
	mov %rax, %rsi
	call printf

computeAnswer:
	mov %r13, %rax
	imul %r13, %rax
	add %r13, %rax
	shr %rax

printAnswerEnd:
 	mov $answerEnd, %rdi
	mov %rax, %rsi
	call printf
	jmp end

printErrMsg:
 	mov $errMessage, %rdi
	call printf
	jmp end

printErrMsg2:
 	mov $errMessage2, %rdi
	call printf
	jmp end

end:
	pop %r14
	pop %r13
	pop %r12
	mov $60, %rax
	mov $0, %rdi
	syscall
