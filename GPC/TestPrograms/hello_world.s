	.file	"TestPrograms/hello_world.p"
	.extern printf
	.text
.globl	HelloWorld
HelloWorld:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$72, %ebx
	pushq	%rbx
	.section	.rodata
.LC1:
	.string "%d\n\0"
	.text
	leaq	.LC1(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$101, %ebx
	pushq	%rbx
	.section	.rodata
.LC2:
	.string "%d\n\0"
	.text
	leaq	.LC2(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$108, %ebx
	pushq	%rbx
	.section	.rodata
.LC3:
	.string "%d\n\0"
	.text
	leaq	.LC3(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$108, %ebx
	pushq	%rbx
	.section	.rodata
.LC4:
	.string "%d\n\0"
	.text
	leaq	.LC4(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$111, %ebx
	pushq	%rbx
	.section	.rodata
.LC5:
	.string "%d\n\0"
	.text
	leaq	.LC5(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$32, %ebx
	pushq	%rbx
	.section	.rodata
.LC6:
	.string "%d\n\0"
	.text
	leaq	.LC6(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$87, %ebx
	pushq	%rbx
	.section	.rodata
.LC7:
	.string "%d\n\0"
	.text
	leaq	.LC7(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$111, %ebx
	pushq	%rbx
	.section	.rodata
.LC8:
	.string "%d\n\0"
	.text
	leaq	.LC8(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$114, %ebx
	pushq	%rbx
	.section	.rodata
.LC9:
	.string "%d\n\0"
	.text
	leaq	.LC9(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$108, %ebx
	pushq	%rbx
	.section	.rodata
.LC10:
	.string "%d\n\0"
	.text
	leaq	.LC10(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$100, %ebx
	pushq	%rbx
	.section	.rodata
.LC11:
	.string "%d\n\0"
	.text
	leaq	.LC11(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$33, %ebx
	pushq	%rbx
	.section	.rodata
.LC12:
	.string "%d\n\0"
	.text
	leaq	.LC12(%rip), %rcx
	popq	%rdx
	subq	$32, %rsp
	call	printf
	addq	$32, %rsp
	movl	$10, %ebx
	pushq	%rbx
	.section	.rodata
.LC13:
	.string "%d\n\0"
	.text
	leaq	.LC13(%rip), %rcx
	popq	%rdx
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
	call	HelloWorld
	xor	%ecx, %ecx
	call	exit
	nop
	leave
	ret
.ident	"GPC: 0.0.0"
