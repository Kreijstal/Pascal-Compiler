program stdlib;

procedure write(s: string);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_s(%rip), %rdi
        movl $0, %eax
        call printf
    end
end;

procedure write(i: integer);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call printf
    end
end;

procedure writeln(s: string);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_sn(%rip), %rdi
        movl $0, %eax
        call printf
    end
end;

procedure writeln(i: integer);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_dn(%rip), %rdi
        movl $0, %eax
        call printf
    end
end;

procedure writeln(i: longint);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_dn(%rip), %rdi
        movl $0, %eax
        call printf
    end
end;

procedure writeln;
begin
    assembler;
    asm
        leaq .format_str_n(%rip), %rdi
        movl $0, %eax
        call printf
    end
end;

procedure read(var i: integer);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call __isoc99_scanf
    end
end;

procedure read(var i: longint);
begin
    assembler;
    asm
        movq %rdi, %rsi
        leaq .format_str_d(%rip), %rdi
        movl $0, %eax
        call __isoc99_scanf
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
