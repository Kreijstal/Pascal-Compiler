	.file	"system.p"
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
.globl	Randomize_void
Randomize_void:
	pushq	%rbp
	movq	%rsp, %rbp
call kgpc_randomize
    
	nop
	leave
	ret
.globl	SetRandSeed_li
SetRandSeed_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$2, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rdi, -8(%rbp)
	movq	%rdi, -8(%rbp)
call kgpc_set_randseed
    
	nop
	leave
	ret
	.section	.comment
	.string	"KGPC: 0.0.0"
	.section	.note.GNU-stack,"",@progbits
