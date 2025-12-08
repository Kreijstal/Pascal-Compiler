	.file	"implicitsystemimport.p"
	.section	.rodata
	.text
	.set	KGPC_TARGET_WINDOWS, 0
	.section	.rodata
.format_str_s:
.string "%s"
.format_str_d:
.string "%d"
.format_str_c:
.string "%c"
.format_str_lld:
.string "%ld"
.format_str_sn:
.string "%s\n"
.format_str_dn:
.string "%d\n"
.format_str_n:
.string "\n"
.text
.globl	GetLength_p
GetLength_p:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$4, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rdi, -8(%rbp)
	movq	%rdi, -8(%rbp)
	movl	$0, %eax
	movslq	%eax, %rax
	movq	%rax, -32(%rbp)
	jmp	.L2
.L3:
	movl	$1, %eax
	movslq	%eax, %rax
	addq	%rax, -32(%rbp)
	jmp	.L2
.L2:
	movq	-32(%rbp), %r10
	movq	-8(%rbp), %r11
	movslq	%r10d, %r10
	leaq	(%r11,%r10,1), %r10
	movzbl	(%r10), %eax
	movl	$0, %r10d
	cmpl	%r10d, %eax
	jne	.L3
.L4:
	movq	-32(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	nop
	leave
	ret
	.section	.comment
	.string	"KGPC: 0.0.0"
	.section	.note.GNU-stack,"",@progbits
