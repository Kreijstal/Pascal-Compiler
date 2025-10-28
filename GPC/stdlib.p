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
    .format_str_d:
        .string "%d"
        .text
    end
end.
