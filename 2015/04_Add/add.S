.data
errMessage:
	.asciz "Number to sum missing. :(\n"

errMessage2:
	.asciz "This is written in assember. You expect me to handle negatives???\n"

answerStart:
	.asciz "add(%d) -> "

answerEnd:
	.asciz "%d\n"

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
	mov %rsi, %r12
	mov 8(%r12), %rdi
	call atoi
	mov %eax, %r13d

	cmp $0, %r13d
	jl printErrMsg2
 
printAnswerStart:
 	mov $answerStart, %rdi
	movslq %eax, %rsi
	call printf

computeAnswer:
	mov %r13d, %eax
	imul %r13d, %eax
	add %r13d, %eax
	shr %eax

printAnswerEnd:
 	mov $answerEnd, %rdi
	movslq %eax, %rsi
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
	ret
