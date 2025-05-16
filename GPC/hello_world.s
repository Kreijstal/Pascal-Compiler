	.file	"TestPrograms/hello_world.p"
	.section	.rodata
	.LC0:
		.string	"%d\n"
		.text
.globl	HelloWorld
HelloWorld:
	pushq	%rbp
	movq	%rsp, %rbp
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
	call	HelloWorld
	movl	$0, %eax
	popq	%rbp
	ret
	.def	main; .scl	-1; .endef
.ident	"GPC: 0.0.0"
