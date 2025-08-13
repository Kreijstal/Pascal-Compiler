	.file	"TestPrograms/sign_test.p"
	.section	.rodata
	.text
.globl	signum
signum:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %ebx
	movl	$0, %edi
	cmpl	%edi, %ebx
	jle	.L2
	movl	$1, %ebx
	movl	%ebx, -8(%rbp)
	jmp	.L3
.L2:
	movl	-4(%rbp), %ebx
	movl	$0, %edi
	cmpl	%edi, %ebx
	jge	.L4
	movl	$1, %ebx
	negl	%ebx
	movl	%ebx, -8(%rbp)
	jmp	.L5
.L4:
	movl	$0, %ebx
	movl	%ebx, -8(%rbp)
.L5:
.L3:
	movl	-8(%rbp), %eax
	nop
	leave
	ret
.globl	sign
sign:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	.section	.rodata
.LC1:
	.string "%d"
	.text
	leaq	.LC1(%rip), %rdi
	leaq	-4(%rbp), %rsi
	movl	$0, %eax
	call	__isoc99_scanf@PLT
	movl	-4(%rbp), %ebx
	movl	%ebx, %edi
	call	signum
	movl	%eax, %ebx
	pushq	%rbx
	.section	.rodata
.LC2:
	.string "%d\n\0"
	.text
	leaq	.LC2(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
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
	call	sign
	xor	%ecx, %ecx
	call	exit
	nop
	leave
	ret
