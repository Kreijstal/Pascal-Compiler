	.file	"new_codegen.p"
.section .rodata
.format_str_d:
.string "%d\n"
.text

        movq %rdi, %rsi
        laq .format_str_d(%rip), %rdi
        movl $0, %ax
        call __isoc99_scaf

	call	assembler
	leave
	ret
.globl read
read:
	pushq	%rbp
	movq	%rsp, %rbp

        movq %rdi, %rsi
        laq .format_str_d(%rip), %rdi
        movl $0, %ax
        call __isoc99_scaf

	call	assembler
	leave
	ret
.globl read
read:
	pushq	%rbp
	movq	%rsp, %rbp

        laq .format_str_(%rip), %rdi
        movl $0, %ax
        call pritf

	call	assembler
	leave
	ret
.globl writeln
writeln:
	pushq	%rbp
	movq	%rsp, %rbp

        movq %rdi, %rsi
        laq .format_str_d(%rip), %rdi
        movl $0, %ax
        call pritf

	call	assembler
	leave
	ret
.globl writeln
writeln:
	pushq	%rbp
	movq	%rsp, %rbp

        movq %rdi, %rsi
        laq .format_str_d(%rip), %rdi
        movl $0, %ax
        call pritf

	call	assembler
	leave
	ret
.globl writeln
writeln:
	pushq	%rbp
	movq	%rsp, %rbp

        movq %rdi, %rsi
        laq .format_str_s(%rip), %rdi
        movl $0, %ax
        call pritf

	call	assembler
	leave
	ret
.globl writeln
writeln:
	pushq	%rbp
	movq	%rsp, %rbp

        movq %rdi, %rsi
        laq .format_str_d(%rip), %rdi
        movl $0, %ax
        call pritf

	call	assembler
	leave
	ret
.globl write
write:
	pushq	%rbp
	movq	%rsp, %rbp

        movq %rdi, %rsi
        laq .format_str_s(%rip), %rdi
        movl $0, %ax
        call pritf

	call	assembler
	leave
	ret
.globl write
write:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$5, %eax
	movl	%eax, %eax
	leave
	ret
.globl get_five
get_five:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	-4(%rbp), %eax
	pushq	%rax
	call	writeln
	popq	%rax
	movl	--1(%rbp), %ebx
	movl	%ebx, -4(%rbp)
