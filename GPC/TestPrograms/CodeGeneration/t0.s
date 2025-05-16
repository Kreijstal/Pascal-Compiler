	.file	"TestPrograms/CodeGeneration/t0.p"
	.section	.rodata
	.LC0:
		.string	"%d\n"
		.text
	.LC1:
		.string	"%d"
		.text
.globl	boo
.type	boo, @function
boo:
	pushq	%rbp
	movq	%rsp, %rbp
	nop
	leave
	ret
	.size	boo, .-boo
.globl	main
.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	call	boo
	movl	$0, %eax
	popq	%rbp
	ret
	.size	main, .-main
.ident	"GPC: 0.0.0"
.section	.note.GNU-stack,"",@progbits
