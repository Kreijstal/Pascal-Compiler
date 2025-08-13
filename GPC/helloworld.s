	.file	"TestPrograms/Over/helloworld_writeln.p"
	.section	.rodata
	.text
.globl	HelloWorld
HelloWorld:
	pushq	%rbp
	movq	%rsp, %rbp
	.section	.rodata
.LC1:
	.string "Hello, World!"
	.text
	movq $1, %rax
	movq $1, %rdi
	leaq .LC1(%rip), %rsi
	movq $14, %rdx
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
	call	HelloWorld
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
