program stdlib;

procedure write(s: string);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwrite_s_sysv
        movq %rcx, %rdx
        leaq .format_str_s(%rip), %rcx
        jmp .Lwrite_s_args_done
.Lwrite_s_sysv:
        movq %rdi, %rsi
        leaq .format_str_s(%rip), %rdi
.Lwrite_s_args_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

procedure write(i: integer);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwrite_i_sysv
        movl %ecx, %edx
        leaq .format_str_d(%rip), %rcx
        jmp .Lwrite_i_args_done
.Lwrite_i_sysv:
        movl %edi, %esi
        leaq .format_str_d(%rip), %rdi
.Lwrite_i_args_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

procedure writeln(s: string);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwriteln_s_sysv
        movq %rcx, %rdx
        leaq .format_str_sn(%rip), %rcx
        jmp .Lwriteln_s_args_done
.Lwriteln_s_sysv:
        movq %rdi, %rsi
        leaq .format_str_sn(%rip), %rdi
.Lwriteln_s_args_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

procedure writeln(i: integer);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwriteln_i_sysv
        movl %ecx, %edx
        leaq .format_str_dn(%rip), %rcx
        jmp .Lwriteln_i_args_done
.Lwriteln_i_sysv:
        movl %edi, %esi
        leaq .format_str_dn(%rip), %rdi
.Lwriteln_i_args_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

procedure writeln(i: longint);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwriteln_li_sysv
        movq %rcx, %rdx
        leaq .format_str_dn(%rip), %rcx
        jmp .Lwriteln_li_args_done
.Lwriteln_li_sysv:
        movq %rdi, %rsi
        leaq .format_str_dn(%rip), %rdi
.Lwriteln_li_args_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

procedure writeln;
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwriteln_void_sysv
        leaq .format_str_n(%rip), %rcx
        jmp .Lwriteln_void_done
.Lwriteln_void_sysv:
        leaq .format_str_n(%rip), %rdi
.Lwriteln_void_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

procedure read(var i: integer);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lread_i_sysv
        movq %rcx, %rdx
        leaq .format_str_d(%rip), %rcx
        jmp .Lread_i_args_done
.Lread_i_sysv:
        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
.Lread_i_args_done:
        xorl %eax, %eax
        call gpc_scanf
    end
end;

procedure read(var i: longint);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lread_li_sysv
        movq %rcx, %rdx
        leaq .format_str_d(%rip), %rcx
        jmp .Lread_li_args_done
.Lread_li_sysv:
        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
.Lread_li_args_done:
        xorl %eax, %eax
        call gpc_scanf
    end
end;

procedure write(i: longint);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lwrite_longint_sysv
        movq %rcx, %rdx
        leaq .format_str_d(%rip), %rcx
        jmp .Lwrite_longint_args_done
.Lwrite_longint_sysv:
        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
.Lwrite_longint_args_done:
        xorl %eax, %eax
        call gpc_printf
    end
end;

function succ(i: integer): integer;
begin
    succ := i + 1;
end;

function succ(i: longint): longint;
begin
    succ := i + 1;
end;

function max(a, b: integer): integer;
begin
    if a >= b then
        max := a
    else
        max := b;
end;

function max(a, b: longint): longint;
begin
    if a >= b then
        max := a
    else
        max := b;
end;

procedure halt;
begin
    assembler;
    asm
        movl $0, %edi
        call exit
    end
end;


begin
    assembler;
    asm
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
    end
end.
