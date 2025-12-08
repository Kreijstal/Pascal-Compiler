	.file	"dos_freemem.p"
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
	.comm	__kgpc_program_var_p_1,8,8
.globl	getmem_impl_u_li
getmem_impl_u_li:
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
	movq	%rsi, -16(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
call kgpc_getmem
    
	nop
	leave
	ret
.globl	freemem_impl_u
freemem_impl_u:
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
movl $KGPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lfreemem_sysv

        movq (%rcx), %rax
        movq %rax, %rcx
        call kgpc_freemem
        jmp .Lfreemem_done

.Lfreemem_sysv:
        movq (%rdi), %rax
        movq %rax, %rdi
        call kgpc_freemem

.Lfreemem_done:
    
	nop
	leave
	ret
.globl	FreeMem_u
FreeMem_u:
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
	movq	-8(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	freemem_impl_u
	nop
	leave
	ret
.globl	GetMem_li
GetMem_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$6, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rdi, -8(%rbp)
	movq	%rdi, -8(%rbp)
	leaq	-32(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, %rsi
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	getmem_impl_u_li
	movq	-32(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	nop
	leave
	ret
.globl	GetMem_i
GetMem_i:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$6, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rdi, -8(%rbp)
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movslq	%eax, %rax
	movq	%rax, -28(%rbp)
	movq	-28(%rbp), %r10
	movq	%r10, -36(%rbp)
	movq	-36(%rbp), %r10
	movq	%r10, %rdi
	call	GetMem_li
	movq	%rax, %rax
	movq	%rax, -20(%rbp)
	movq	-20(%rbp), %rax
	nop
	leave
	ret
.globl	FreeMem_p_li
FreeMem_p_li:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$6, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-8(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	freemem_impl_u
	nop
	leave
	ret
.globl	DosFreeMem
DosFreeMem:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, %r10
	movq	%rcx, %r11
	movq	%rsp, %rdi
	xorq	%rax, %rax
	movl	$6, %ecx
	rep stosq
	movq	%r10, %rdi
	movq	%r11, %rcx
	movl	$100, %r10d
	movq	%r10, -8(%rbp)
	movq	-8(%rbp), %r10
	movq	%r10, %rdi
	call	GetMem_i
	movq	%rax, %rax
	movq	%rax, __kgpc_program_var_p_1(%rip)
	leaq	__kgpc_program_var_p_1(%rip), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	FreeMem_u
	movl	$200, %r10d
	movq	%r10, -24(%rbp)
	movq	-24(%rbp), %r10
	movq	%r10, %rdi
	call	GetMem_i
	movq	%rax, %rax
	movq	%rax, __kgpc_program_var_p_1(%rip)
	movq	__kgpc_program_var_p_1(%rip), %rax
	movq	%rax, -32(%rbp)
	movl	$200, %eax
	movq	%rax, -40(%rbp)
	movslq	-40(%rbp), %rax
	movq	%rax, %rsi
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	movl	$0, %eax
	call	FreeMem_p_li
	.section	.rodata
.LC1:
	.string "FreeMem test passed"
	.text
	leaq	.LC1(%rip), %rax
	movq	$-1, %rsi
	movq	%rax, %rdx
	xorq	%rdi, %rdi
	movl	$0, %eax
	call	kgpc_write_string
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
	call	DosFreeMem
	xor	%edi, %edi
	call	exit
	nop
	leave
	ret
	.section	.comment
	.string	"KGPC: 0.0.0"
	.section	.note.GNU-stack,"",@progbits
