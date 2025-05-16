.file	"TestPrograms/write_var_test.p"
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
.globl	WriteVarTest
WriteVarTest:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	$42, %ebx
	movl	%ebx, -4(%rbp)
	movl	-4(%rbp), %ebx
	leaq	.LC0(%rip), %rcx
	movl	%ebx, %edx
	call	printf
	nop
	leave
	ret
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	call	WriteVarTest
	movl	$0, %eax
	leave
	ret