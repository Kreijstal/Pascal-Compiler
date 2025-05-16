	.file	"TestPrograms/printf_test.p"
	.text
.globl	PrintfTest
PrintfTest:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$42, %ebx
	movl	%ebx, -4(%rbp)
	movl	-4(%rbp), %ebx
	pushq	%%rbx
	.section	.rodata
.LC1:
	.string "%d
"
	.text
	leaq	.LC1(%rip), %rcx
	popq	%%rdx
	movl	$0, %eax
	call	printf@PLT
	nop
	leave
	ret
	.section	.text
	.globl	main
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	andq	$-16, %rsp
	call	PrintfTest
	movl	$0, %eax
	leave
	ret
.ident	"GPC: 0.0.0"
