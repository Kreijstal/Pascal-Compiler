program stdlib;

{ ============================================================================
  Standard Library Functions (previously built-ins)
  ============================================================================ }

{ Mathematical Functions }

function Sqr(x: integer): integer; overload;
begin
    assembler;
    asm
        call gpc_sqr_int32
        movl %eax, -12(%rbp)
    end
end;

function Sqr(x: longint): longint; overload;
begin
    assembler;
    asm
        call gpc_sqr_int64
        movq %rax, -24(%rbp)
    end
end;

function Sqr(x: real): real; overload;
begin
    assembler;
    asm
        call gpc_sqr_real
        movsd %xmm0, -24(%rbp)
    end
end;

function Odd(x: integer): boolean; overload;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_is_odd
    end;
    Odd := (result <> 0);
end;

function Odd(x: longint): boolean; overload;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_is_odd
    end;
    Odd := (result <> 0);
end;

{ Character and String Functions }

function Chr(x: integer): char;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_chr
    end;
    Chr := char(result);
end;

function Ord(ch: char): integer; overload;
begin
    assembler;
    asm
        { For char, just return the ASCII value }
        movzbl %dil, %eax
        movl $GPC_TARGET_WINDOWS, %edx
        testl %edx, %edx
        je .Lord_char_done
        movzbl %cl, %eax
.Lord_char_done:
    end
end;

function Ord(b: boolean): integer; overload;
begin
    if b then
        Ord := 1
    else
        Ord := 0;
end;

function UpCase(ch: char): char;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_upcase_char
    end;
    UpCase := char(result);
end;

function Copy(s: string; index: integer; count: integer): string; overload;
var
    idx_long, cnt_long: longint;
    result: string;
begin
    idx_long := index;
    cnt_long := count;
    assembler;
    asm
        call gpc_string_copy
    end;
    Copy := result;
end;

function Copy(s: string; index: longint; count: longint): string; overload;
var
    result: string;
begin
    assembler;
    asm
        call gpc_string_copy
    end;
    Copy := result;
end;

function Pos(substr: string; s: string): integer;
var
    result_long: longint;
begin
    assembler;
    asm
        call gpc_string_pos
    end;
    Pos := integer(result_long);
end;

{ Random Number Functions }

function Random: real; overload;
var
    result: real;
begin
    assembler;
    asm
        call gpc_random_real
    end;
    Random := result;
end;

function Random(upper: integer): integer; overload;
var
    upper_long: longint;
    result_long: longint;
begin
    upper_long := upper;
    assembler;
    asm
        call gpc_random_int
    end;
    Random := integer(result_long);
end;

function Random(upper: longint): longint; overload;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_random_int
    end;
    Random := result;
end;

function Random(upper: real): real; overload;
var
    result: real;
begin
    assembler;
    asm
        call gpc_random_real_upper
    end;
    Random := result;
end;

function RandomRange(low: integer; high: integer): integer; overload;
var
    low_long, high_long: longint;
    result_long: longint;
begin
    low_long := low;
    high_long := high;
    assembler;
    asm
        call gpc_random_range
    end;
    RandomRange := integer(result_long);
end;

function RandomRange(low: longint; high: longint): longint; overload;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_random_range
    end;
    RandomRange := result;
end;

{ I/O Functions }

function EOF(var f: text): boolean; overload;
var
    result: integer;
begin
    assembler;
    asm
        call gpc_text_eof
    end;
    EOF := (result <> 0);
end;

function EOF: boolean; overload;
var
    result: integer;
begin
    assembler;
    asm
        call gpc_text_eof_default
    end;
    EOF := (result <> 0);
end;

function EOLN(var f: text): boolean; overload;
var
    result: integer;
begin
    assembler;
    asm
        call gpc_text_eoln
    end;
    EOLN := (result <> 0);
end;

function EOLN: boolean; overload;
var
    result: integer;
begin
    assembler;
    asm
        call gpc_text_eoln_default
    end;
    EOLN := (result <> 0);
end;

{ Pointer Functions }

function Assigned(ptr: pointer): boolean;
var
    result: longint;
begin
    assembler;
    asm
        call gpc_assigned
    end;
    Assigned := (result <> 0);
end;

{ ============================================================================
  System Procedures and Functions
  ============================================================================ }

procedure Randomize;
begin
    assembler;
    asm
        call gpc_randomize
    end
end;

procedure SetRandSeed(seed: longint);
begin
    assembler;
    asm
        call gpc_set_randseed
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

procedure MkDir(path: string);
begin
    assembler;
    asm
        call gpc_directory_create
    end
end;

procedure RmDir(path: string);
begin
    assembler;
    asm
        call gpc_directory_remove
    end
end;

procedure append(var f: text); overload;
begin
    assembler;
    asm
        call gpc_text_app;
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

procedure blockread_impl(var f: file; var buffer; count: longint; result_ptr: pointer);
begin
    assembler;
    asm
        call gpc_tfile_blockread
    end
end;

procedure blockwrite_impl(var f: file; var buffer; count: longint; result_ptr: pointer);
begin
    assembler;
    asm
        call gpc_tfile_blockwrite
    end
end;

procedure filepos_impl(var f: file; var position: int64);
begin
    assembler;
    asm
        call gpc_tfile_filepos
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

procedure fillchar_impl(var dest; count: longint; value: integer);
begin
    assembler;
    asm
        call gpc_fillchar
    end
end;

procedure FillChar(var dest; count: integer; value: integer); overload;
var
    count_long: longint;
begin
    count_long := count;
    fillchar_impl(dest, count_long, value);
end;

procedure FillChar(var dest; count: longint; value: integer); overload;
begin
    fillchar_impl(dest, count, value);
end;

procedure getmem_impl(var target; size: longint);
begin
    assembler;
    asm
        call gpc_getmem
    end
end;

procedure reallocmem_impl(var target; size: longint);
begin
    assembler;
    asm
        call gpc_reallocmem
    end
end;

procedure freemem_impl(var target);
begin
    assembler;
    asm
        movl $GPC_TARGET_WINDOWS, %eax
        testl %eax, %eax
        je .Lfreemem_sysv

        movq (%rcx), %rax
        movq %rax, %rcx
        call gpc_freemem
        jmp .Lfreemem_done

.Lfreemem_sysv:
        movq (%rdi), %rax
        movq %rax, %rdi
        call gpc_freemem

.Lfreemem_done:
    end
end;

procedure GetMem(var target; size: integer); overload;
var
    size_long: longint;
begin
    size_long := size;
    getmem_impl(target, size_long);
end;

procedure GetMem(var target; size: longint); overload;
begin
    getmem_impl(target, size);
end;

procedure ReallocMem(var target; size: integer); overload;
var
    size_long: longint;
begin
    size_long := size;
    reallocmem_impl(target, size_long);
end;

procedure ReallocMem(var target; size: longint); overload;
begin
    reallocmem_impl(target, size);
end;

procedure FreeMem(var target);
begin
    freemem_impl(target);
end;

procedure BlockRead(var f: file; var buffer; count: longint); overload;
begin
    blockread_impl(f, buffer, count, nil);
end;

procedure BlockRead(var f: file; var buffer; count: longint; var result: longint); overload;
var
    actual64: int64;
begin
    actual64 := 0;
    blockread_impl(f, buffer, count, @actual64);
    result := longint(actual64);
end;


procedure Seek(var f: file; index: longint); overload;
begin
    assembler;
    asm
        call gpc_tfile_seek
    end
end;

function FilePos(var f: file): longint; overload;
var
    pos64: int64;
begin
    pos64 := 0;
    filepos_impl(f, pos64);
    FilePos := longint(pos64);
end;

procedure Truncate(var f: file); overload;
begin
    assembler;
    asm
        call gpc_tfile_truncate_current
    end
end;

procedure Truncate(var f: file; length: longint); overload;
begin
    assembler;
    asm
        call gpc_tfile_truncate
    end
end;

function IOResult: integer;
begin
    assembler;
    asm
        call gpc_ioresult_get_and_clear
    end
end;

procedure BlockWrite(var f: file; var buffer; count: longint); overload;
begin
    blockwrite_impl(f, buffer, count, nil);
end;

procedure BlockWrite(var f: file; var buffer; count: longint; var result: longint); overload;
var
    actual64: int64;
begin
    actual64 := 0;
    blockwrite_impl(f, buffer, count, @actual64);
    result := longint(actual64);
end;

begin
end.
