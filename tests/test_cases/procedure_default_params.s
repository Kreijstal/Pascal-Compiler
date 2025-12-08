	.file	"procedure_default_params.p"
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
.globl	ProcedureDefaultParams
ProcedureDefaultParams:
	pushq	%rbp
	movq	%rsp, %rbp
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
	call	ProcedureDefaultParams
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
	.section	.comment
	.string	"KGPC: 0.0.0"
	.section	.note.GNU-stack,"",@progbits
