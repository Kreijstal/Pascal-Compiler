	.file	"GPC/TestPrograms/Over/overload_test.p"
	.section	.rodata
	.text
.globl	add_i_i
add_i_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %ebx
	addl	-8(%rbp), %ebx
	movl	%ebx, -12(%rbp)
	movl	-12(%rbp), %eax
	nop
	leave
	ret
.globl	add_r_r
add_r_r:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %ebx
	addl	-8(%rbp), %ebx
	movl	%ebx, -12(%rbp)
	movl	-12(%rbp), %eax
	nop
	leave
	ret
.globl	subtract
subtract:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %ebx
	subl	-8(%rbp), %ebx
	movl	%ebx, -12(%rbp)
	movl	-12(%rbp), %eax
	nop
	leave
	ret
.globl	WriteStringLn_a
WriteStringLn_a:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        call puts
	nop
	leave
	ret
.globl	WriteIntLn_i
WriteIntLn_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        call print_integer
	nop
	leave
	ret
.globl	overload_test
overload_test:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movl	$5, %ebx
	movl	%ebx, -4(%rbp)
	movl	$10, %ebx
	movl	%ebx, -8(%rbp)
	movl	-4(%rbp), %ebx
	movq	%rbx, %rdi
	movl	-8(%rbp), %ebx
	movq	%rbx, %rsi
	movl	$0, %eax
	call	add_i_i
	movl	%eax, %ebx
	movl	%ebx, -12(%rbp)
	movl	-12(%rbp), %ebx
	pushq	%rbx
	.section	.rodata
.LC1:
	.string "%d\n\0"
	.text
	leaq	.LC1(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movl	$2, %ebx
	movl	%ebx, -16(%rbp)
	movl	$3, %ebx
	movl	%ebx, -20(%rbp)
	movl	-16(%rbp), %ebx
	movq	%rbx, %rdi
	movl	-20(%rbp), %ebx
	movq	%rbx, %rsi
	movl	$0, %eax
	call	add_r_r
	movl	%eax, %ebx
	movl	%ebx, -24(%rbp)
	movl	-24(%rbp), %ebx
	pushq	%rbx
	.section	.rodata
.LC2:
	.string "%d\n\0"
	.text
	leaq	.LC2(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
	call	printf
	addq	$32, %rsp
	movl	-8(%rbp), %ebx
	movq	%rbx, %rdi
	movl	-4(%rbp), %ebx
	movq	%rbx, %rsi
	movl	$0, %eax
	call	subtract
	movl	%eax, %ebx
	movl	%ebx, -12(%rbp)
	movl	-12(%rbp), %ebx
	pushq	%rbx
	.section	.rodata
.LC3:
	.string "%d\n\0"
	.text
	leaq	.LC3(%rip), %rdi
	popq	%rsi
	subq	$32, %rsp
	movl	$0, %eax
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
	call	overload_test
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
