	.file	"mod_test.p"
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
.globl	write_u
write_u:
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
.globl	writeln_u
writeln_u:
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
.globl	mod_test
mod_test:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$10, %ebx
	movl	%ebx, %eax
	movl	$3, -4(%rbp)
	cltd
	idivl	-4(%rbp)
	movl	%edx, %ebx
	movq	%rbx, %rdi
	movl	$0, %eax
	call	writeln_i
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
	call	mod_test
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
