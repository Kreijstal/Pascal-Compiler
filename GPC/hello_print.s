	.file	"TestPrograms\hello_print.p"
	.section	.rodata
	.LC0:
		.string	"%d\n"
		.text
.globl	HelloPrint
HelloPrint:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$42, %ebx
	movl	%ebx, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movl	$123, %ebx
	movl	%ebx, %esi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	nop
	leave
	ret
	.globl	main
	.def	main; .scl	2; .type	32; .endef
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	call	HelloPrint
	movl	$0, %eax
	popq	%rbp
	ret
	.def	main; .scl	-1; .endef
.ident	"GPC: 0.0.0"
