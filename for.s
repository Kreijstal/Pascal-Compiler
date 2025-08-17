	.file	"GPC/TestPrograms/CodeGeneration/for.p"
	.section	.rodata
	.text
.section .rodata
.format_str_s:
.string "%s"
.format_str_d:
.string "%d"
.format_str_sn:
.string "%s\n"
.format_str_dn:
.string "%d\n"
.format_str_n:
.string "\n"
.text
.globl	write_s
write_s:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_s(%rip), %rdi
        movl $0, %eax
        call printf
	nop
	leave
	ret
.globl	write_i
write_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call printf
	nop
	leave
	ret
.globl	writeln_s
writeln_s:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_sn(%rip), %rdi
        movl $0, %eax
        call printf
	nop
	leave
	ret
.globl	writeln_i
writeln_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_dn(%rip), %rdi
        movl $0, %eax
        call printf
	nop
	leave
	ret
.globl	writeln_li
writeln_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_dn(%rip), %rdi
        movl $0, %eax
        call printf
	nop
	leave
	ret
.globl	writeln_void
writeln_void:
	pushq	%rbp
	movq	%rsp, %rbp

        leaq .format_str_n(%rip), %rdi
        movl $0, %eax
        call printf
	nop
	leave
	ret
.globl	read_i
read_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call __isoc99_scanf
	nop
	leave
	ret
.globl	read_li
read_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)

        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call __isoc99_scanf
	nop
	leave
	ret
.globl	boo
boo:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	leaq	-4(%rbp), %rdi
	movl	$0, %eax
	call	read_i
	movl	$1, %ebx
	movl	%ebx, -8(%rbp)
	jmp	.L2
.L3:
	movl	-8(%rbp), %ebx
	movq	%rbx, %rdi
	movl	$0, %eax
	call	write_i
	movl	-8(%rbp), %ebx
	addl	$1, %ebx
	movl	%ebx, -8(%rbp)
.L2:
	movl	-8(%rbp), %ebx
	movl	-4(%rbp), %edi
	imull	$2, %edi
	cmpl	%edi, %ebx
	jle	.L3
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
	call	boo
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
