	.file	"TestPrograms/empty_block.p"
	.section	.rodata
	.LC0:
		.string	"%d\n"
		.text
	.LC1:
		.string	"%d"
		.text
.globl	EmptyBlock
EmptyBlock:
	pushq	%rbp
	movq	%rsp, %rbp
	nop
	leave
	ret
.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	call	EmptyBlock
	movl	$0, %eax
	popq	%rbp
	ret
	.size	main, .-main
.ident	"GPC: 0.0.0"
.section	.note.GNU-stack,"",@progbits
