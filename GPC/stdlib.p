program stdlib;

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
        leaq .format_str_lld(%rip), %rcx
        jmp .Lread_li_args_done
.Lread_li_sysv:
        movq %rdi, %rsi
        leaq .format_str_lld(%rip), %rdi
.Lread_li_args_done:
        xorl %eax, %eax
        call gpc_scanf
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

procedure halt;
begin
    assembler;
    asm
        movl $0, %edi
        call exit
    end
end;


procedure assign(var f: text; filename: string);
begin
    assembler;
    asm
        call gpc_text_assign
    end
end;

procedure rewrite(var f: text);
begin
    assembler;
    asm
        call gpc_text_rewrite
    end
end;

procedure reset(var f: text);
begin
    assembler;
    asm
        call gpc_text_reset
    end
end;

procedure close(var f: text);
begin
    assembler;
    asm
        call gpc_text_close
    end
end;

procedure readln(var value: string);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lreadln_str_sysv
        movq %rcx, %rdx
        xorq %rcx, %rcx
        jmp .Lreadln_str_call
.Lreadln_str_sysv:
        movq %rdi, %rsi
        xorq %rdi, %rdi
.Lreadln_str_call:
        call gpc_text_readln_into
    end
end;

procedure readln(f: text; var value: string);
begin
    assembler;
    asm
        call gpc_text_readln_into
    end
end;

procedure readln;
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lreadln_nofile_sysv
        xorq %rcx, %rcx
        jmp .Lreadln_nofile_call
.Lreadln_nofile_sysv:
        xorq %rdi, %rdi
.Lreadln_nofile_call:
        call gpc_text_readln_discard
    end
end;

procedure readln(f: text);
begin
    assembler;
    asm
        call gpc_text_readln_discard
    end
end;


begin
end.
