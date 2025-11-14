program stdlib;

function succ(i: integer): integer;
begin
    succ := i + 1;
end;

function succ(i: longint): longint;
begin
    succ := i + 1;
end;

function pred(i: integer): integer;
begin
    pred := i - 1;
end;

function pred(i: longint): longint;
begin
    pred := i - 1;
end;

function file_is_text(var f: file): longint;
begin
    assembler;
    asm
        call gpc_file_is_text
    end
end;

procedure assign_text_internal(var f: text; filename: string);
begin
    assembler;
    asm
        call gpc_text_assign
    end
end;

procedure rewrite_text_internal(var f: text);
begin
    assembler;
    asm
        call gpc_text_rewrite
    end
end;

procedure halt;
begin
    assembler;
    asm
        movl $0, %edi
        call exit
    end
end;

procedure halt(exitcode: integer);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lhalt_sysv
        movl %ecx, %edi
        jmp .Lhalt_call
.Lhalt_sysv:
        movl %edi, %edi
.Lhalt_call:
        call exit
    end
end;


procedure assign(var f: text; filename: string); overload;
begin
    assign_text_internal(f, filename);
end;

procedure rewrite(var f: text); overload;
begin
    rewrite_text_internal(f);
end;

procedure reset(var f: text); overload;
begin
    assembler;
    asm
        call gpc_text_reset
    end
end;

procedure close(var f: text); overload;
begin
    assembler;
    asm
        call gpc_text_close
    end
end;

{ --------------------------------------------------------------------
  Typed file helpers (binary files). These are preliminary APIs that
  operate on generic file variables and map to gpc_tfile_* runtime
  functions. They do not interfere with existing text I/O.
  -------------------------------------------------------------------- }

procedure assign(var f: file; filename: string); overload;
begin
    assembler;
    asm
        call gpc_tfile_assign
    end
end;

procedure rewrite(var f: file); overload;
begin
    assembler;
    asm
        call gpc_tfile_rewrite
    end
end;

procedure reset(var f: file); overload;
begin
    assembler;
    asm
        call gpc_tfile_reset
    end
end;

procedure close(var f: file); overload;
begin
    assembler;
    asm
        call gpc_tfile_close
    end
end;

procedure file_read_integer(var f: file; var value: integer);
begin
    assembler;
    asm
        call gpc_tfile_read_int
    end
end;

procedure file_write_integer(var f: file; value: integer);
begin
    assembler;
    asm
        call gpc_tfile_write_int
    end
end;

procedure file_read_char(var f: file; var value: char);
begin
    assembler;
    asm
        call gpc_tfile_read_char
    end
end;

procedure file_write_char(var f: file; value: char);
begin
    assembler;
    asm
        call gpc_tfile_write_char
    end
end;

procedure file_read_real(var f: file; var value: real);
begin
    assembler;
    asm
        call gpc_tfile_read_real
    end
end;

procedure file_write_real(var f: file; value: real);
begin
    assembler;
    asm
        call gpc_tfile_write_real
    end
end;

procedure move_impl(var source; var dest; count: longint);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lmove_sysv

        /* Win64: RCX=source, RDX=dest, R8=count -> swap RCX/RDX */
        movq %rcx, %r9
        movq %rdx, %rcx
        movq %r9, %rdx
        jmp .Lmove_call

.Lmove_sysv:
        /* SysV: RDI=source, RSI=dest, RDX=count -> swap RDI/RSI */
        movq %rdi, %rax
        movq %rsi, %rdi
        movq %rax, %rsi

.Lmove_call:
        call gpc_move
    end
end;

procedure Move(var source; var dest; count: integer); overload;
var
    count_long: longint;
begin
    count_long := count;
    move_impl(source, dest, count_long);
end;

procedure Move(var source; var dest; count: longint); overload;
begin
    move_impl(source, dest, count);
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

procedure readln(var value: char);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lreadln_char_sysv
        movq %rcx, %rdx
        xorq %rcx, %rcx
        jmp .Lreadln_char_call
.Lreadln_char_sysv:
        movq %rdi, %rsi
        xorq %rdi, %rdi
.Lreadln_char_call:
        call gpc_text_readln_into_char
    end
end;

procedure readln(f: text; var value: string);
begin
    assembler;
    asm
        call gpc_text_readln_into
    end
end;

procedure readln(f: text; var value: char);
begin
    assembler;
    asm
        call gpc_text_readln_into_char
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
