.file	"TestPrograms/write_var_test.p"
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
.globl	WriteVarTest
WriteVarTest:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$42, %ebx
	movl	%ebx, -4(%rbp)
	movl	-4(%rbp), %ebx
	movl	%ebx, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	nop
	leave
	ret
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	call	WriteVarTest
	movl	$0, %eax
	popq	%rbp
	ret