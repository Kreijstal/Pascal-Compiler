program stdlib;

procedure write(s: string);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lwrite_s_sysv
        movq %rcx, %rsi
        jmp .Lwrite_s_args_done
.Lwrite_s_sysv:
        movq %rdi, %rsi
.Lwrite_s_args_done:
        leaq .format_str_s(%rip), %rdi
        movl $0, %eax
        call gpc_printf
    end
end;

procedure write(i: integer);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lwrite_i_sysv
        movl %ecx, %esi
        jmp .Lwrite_i_args_done
.Lwrite_i_sysv:
        movl %edi, %esi
.Lwrite_i_args_done:
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call gpc_printf
    end
end;

procedure writeln(s: string);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lwriteln_s_sysv
        movq %rcx, %rsi
        jmp .Lwriteln_s_args_done
.Lwriteln_s_sysv:
        movq %rdi, %rsi
.Lwriteln_s_args_done:
        leaq .format_str_sn(%rip), %rdi
        movl $0, %eax
        call gpc_printf
    end
end;

procedure writeln(i: integer);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lwriteln_i_sysv
        movl %ecx, %esi
        jmp .Lwriteln_i_args_done
.Lwriteln_i_sysv:
        movl %edi, %esi
.Lwriteln_i_args_done:
        leaq .format_str_dn(%rip), %rdi
        movl $0, %eax
        call gpc_printf
    end
end;

procedure writeln(i: longint);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lwriteln_li_sysv
        movq %rcx, %rsi
        jmp .Lwriteln_li_args_done
.Lwriteln_li_sysv:
        movq %rdi, %rsi
.Lwriteln_li_args_done:
        leaq .format_str_dn(%rip), %rdi
        movl $0, %eax
        call gpc_printf
    end
end;

procedure writeln;
begin
    assembler;
    asm
        leaq .format_str_n(%rip), %rdi
        movl $0, %eax
        call gpc_printf
    end
end;

procedure read(var i: integer);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lread_i_sysv
        movq %rcx, %rsi
        jmp .Lread_i_args_done
.Lread_i_sysv:
        movq %rdi, %rsi
.Lread_i_args_done:
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call gpc_scanf
    end
end;

procedure read(var i: longint);
begin
    assembler;
    asm
        movl gpc_target_windows(%rip), %eax
        testl %eax, %eax
        je .Lread_li_sysv
        movq %rcx, %rsi
        jmp .Lread_li_args_done
.Lread_li_sysv:
        movq %rdi, %rsi
.Lread_li_args_done:
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call gpc_scanf
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
