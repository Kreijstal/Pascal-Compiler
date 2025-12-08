	.file	"const_expr_functions.p"
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
.globl	ConstExprFunctions
ConstExprFunctions:
	pushq	%rbp
	movq	%rsp, %rbp
	.section	.rodata
.LC1:
	.string "MaxByte: "
	.text
	leaq	.LC1(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
	movl	$255, %eax
	movq	$-1, %rsi
	movslq	%eax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_integer
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_newline
	.section	.rodata
.LC2:
	.string "CharSize: "
	.text
	leaq	.LC2(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
	movl	$1, %eax
	movq	$-1, %rsi
	movslq	%eax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_integer
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_newline
	.section	.rodata
.LC3:
	.string "CharA: "
	.text
	leaq	.LC3(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
	movl	$65, %eax
	movq	$-1, %rsi
	movslq	%eax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_integer
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_newline
	.section	.rodata
.LC4:
	.string "CharZ: "
	.text
	leaq	.LC4(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
	movl	$90, %eax
	movq	$-1, %rsi
	movslq	%eax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_integer
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_newline
	.section	.rodata
.LC5:
	.string "CalcValue: "
	.text
	leaq	.LC5(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
	movl	$30, %eax
	movq	$-1, %rsi
	movslq	%eax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_integer
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_newline
	.section	.rodata
.LC6:
	.string "ArraySize: "
	.text
	leaq	.LC6(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
	movl	$256, %eax
	movq	$-1, %rsi
	movslq	%eax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_integer
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_newline
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
	call	ConstExprFunctions
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
	.section	.comment
	.string	"KGPC: 0.0.0"
	.section	.note.GNU-stack,"",@progbits
