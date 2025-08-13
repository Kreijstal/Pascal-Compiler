	.file	"/app/GPC/TestPrograms/CodeGeneration/refactor_test.p"
	.section	.rodata
	.text
.globl	WriteStringLn_s
WriteStringLn_s:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        call puts
	nop
	leave
	ret
.globl	WriteIntLn_i
WriteIntLn_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        call print_integer
	nop
	leave
	ret
.globl	refactor_test
refactor_test:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$10, %ebx
	movl	%ebx, -4(%rbp)
	movl	$20, %ebx
	movl	%ebx, -8(%rbp)
	movl	$0, %ebx
	movl	%ebx, -12(%rbp)
	movl	-4(%rbp), %ebx
	movl	-8(%rbp), %edi
	cmpl	%edi, %ebx
	jge	.L2
	.section	.rodata
.LC1:
	.string "i is less than j"
	.text
	movq $1, %rax
	movq $1, %rdi
	leaq .LC1(%rip), %rsi
	movq $17, %rdx
	syscall
	jmp	.L3
.L2:
	.section	.rodata
.LC2:
	.string "i is not less than j"
	.text
	movq $1, %rax
	movq $1, %rdi
	leaq .LC2(%rip), %rsi
	movq $21, %rdx
	syscall
.L3:
	jmp	.L4
.L5:
	leaq	.LC3(%rip), %rax
	pushq	%rax
	.section	.rodata
.LC3:
	.string "k = "
	.text
	.section	.rodata
.LC4:
	.string "%s\n\0"
	.text
	leaq	.LC4(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movl	-12(%rbp), %ebx
	pushq	%rbx
	.section	.rodata
.LC4:
	.string "%d\n\0"
	.text
	leaq	.LC4(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movl	-12(%rbp), %ebx
	addl	$1, %ebx
	movl	%ebx, -12(%rbp)
.L4:
	movl	-12(%rbp), %ebx
	movl	$5, %edi
	cmpl	%edi, %ebx
	jl	.L5
	movl	$1, %ebx
	movl	%ebx, -4(%rbp)
	jmp	.L6
.L7:
	leaq	.LC5(%rip), %rax
	pushq	%rax
	.section	.rodata
.LC5:
	.string "i = "
	.text
	.section	.rodata
.LC6:
	.string "%s\n\0"
	.text
	leaq	.LC6(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movl	-4(%rbp), %ebx
	pushq	%rbx
	.section	.rodata
.LC6:
	.string "%d\n\0"
	.text
	leaq	.LC6(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	leaq	.LC7(%rip), %rax
	pushq	%rax
	.section	.rodata
.LC7:
	.string ", "
	.text
	.section	.rodata
.LC8:
	.string "%s\n\0"
	.text
	leaq	.LC8(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movl	-4(%rbp), %ebx
	addl	$1, %ebx
	movl	%ebx, -4(%rbp)
.L6:
	movl	-4(%rbp), %ebx
	movl	$5, %edi
	cmpl	%edi, %ebx
	jl	.L7
	.section	.rodata
.LC8:
	.string ""
	.text
	movq $1, %rax
	movq $1, %rdi
	leaq .LC8(%rip), %rsi
	movq $1, %rdx
	syscall
	nop
	leave
	ret
	.section	.text
	.globl	main
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	call	refactor_test
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
