program stdlib;

{ ============================================================================
  FPC System Types for Bootstrap Compatibility
  ============================================================================ }

type
  { Size types - platform dependent }
  SizeInt = Int64;          { Signed size type - matches pointer size on 64-bit }
  { SizeUInt is now a built-in type with size 8 }
  
  { Pointer types }
  PAnsiChar = ^Char;        { Pointer to ANSI character }
  PPAnsiChar = ^PAnsiChar;  { Pointer to pointer to ANSI character }
  PChar = ^Char;            { Alias for PAnsiChar }
  PPointer = ^Pointer;      { Pointer to pointer }
  
  { Additional common pointer types }
  PByte = ^Byte;
  PWord = ^Word;
  PLongInt = ^LongInt;
  PLongWord = ^LongWord;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;

{ ============================================================================
  Compiler Intrinsic Functions
  
  The following functions are implemented as compiler intrinsics for
  performance and type safety. They are documented here for reference.
  ============================================================================ }

{ SizeOf(expr) - Returns the size in bytes of a type or expression  
  This function works in unevaluated contexts and on types.
  Example: SizeOf(integer) returns 4
}

{ Length(x) - Returns the length of an array or string
  For fixed-size arrays, this is a compile-time constant.
  Example: Length(myArray) 
}

{ Sqr(x) - Returns x * x
  Overloaded for integer, longint, and real types.
  The compiler can constant-fold literal arguments.
}

{ Odd(x) - Returns true if x is odd, false otherwise
  Implemented as (x mod 2) <> 0
  Overloaded for integer and longint types.
}

{ Chr(x) - Converts an integer to a character
  Example: Chr(65) returns 'A'
}

{ Ord(ch) - Returns the ordinal value of a character or boolean
  For characters, returns the ASCII value.
  For booleans, returns 0 or 1.
}

{ UpCase(ch) - Converts a character to uppercase
  Example: UpCase('a') returns 'A'
}

{ Random - Returns a random number
  Random: real - Returns a random real in [0, 1)
  Random(upper: integer): integer - Returns random integer in [0, upper)
  Random(upper: real): real - Returns random real in [0, upper)
}

{ RandomRange(low, high) - Returns a random integer in [low, high)
  Overloaded for integer and longint types.
}

{ Copy(s, index, count) - Returns a substring
  Example: Copy('Hello', 2, 3) returns 'ell'
  1-based indexing.
}

{ Pos(substr, s) - Finds the position of substr in s
  Returns 0 if not found, otherwise 1-based position.
}

{ EOF(f) - Returns true if at end of file
  EOF: boolean - Checks standard input
  EOF(var f: text): boolean - Checks specific file
}

{ EOLN(f) - Returns true if at end of line
  EOLN: boolean - Checks standard input  
  EOLN(var f: text): boolean - Checks specific file
}

{ Assigned(ptr) - Returns true if pointer is not nil
  Works with pointer and procedure variables.
}

{ ============================================================================
  System Procedures and Functions
  ============================================================================ }

procedure Randomize;
begin
    assembler;
    asm
        call kgpc_randomize
    end
end;

procedure SetRandSeed(seed: longint);
begin
    assembler;
    asm
        call kgpc_set_randseed
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
        call kgpc_file_is_text
    end
end;

procedure assign_text_internal(var f: text; filename: string);
begin
    assembler;
    asm
        call kgpc_text_assign
    end
end;

procedure rewrite_text_internal(var f: text);
begin
    assembler;
    asm
        call kgpc_text_rewrite
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
        movl $KGPC_TARGET_WINDOWS, %eax
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
        call kgpc_text_reset
    end
end;

procedure close(var f: text); overload;
begin
    assembler;
    asm
        call kgpc_text_close
    end
end;

procedure MkDir(path: string);
begin
    assembler;
    asm
        call kgpc_directory_create
    end
end;

procedure RmDir(path: string);
begin
    assembler;
    asm
        call kgpc_directory_remove
    end
end;

procedure append(var f: text); overload;
begin
    assembler;
    asm
        call kgpc_text_app;
    end
end;

{ --------------------------------------------------------------------
  Typed file helpers (binary files). These are preliminary APIs that
  operate on generic file variables and map to kgpc_tfile_* runtime
  functions. They do not interfere with existing text I/O.
  -------------------------------------------------------------------- }

procedure assign(var f: file; filename: string); overload;
begin
    assembler;
    asm
        call kgpc_tfile_assign
    end
end;

procedure rewrite(var f: file); overload;
begin
    assembler;
    asm
        call kgpc_tfile_rewrite
    end
end;

procedure reset(var f: file); overload;
begin
    assembler;
    asm
        call kgpc_tfile_reset
    end
end;

procedure close(var f: file); overload;
begin
    assembler;
    asm
        call kgpc_tfile_close
    end
end;

procedure file_read_integer(var f: file; var value: integer);
begin
    assembler;
    asm
        call kgpc_tfile_read_int
    end
end;

procedure file_write_integer(var f: file; value: integer);
begin
    assembler;
    asm
        call kgpc_tfile_write_int
    end
end;

procedure file_read_char(var f: file; var value: char);
begin
    assembler;
    asm
        call kgpc_tfile_read_char
    end
end;

procedure file_write_char(var f: file; value: char);
begin
    assembler;
    asm
        call kgpc_tfile_write_char
    end
end;

procedure file_read_real(var f: file; var value: real);
begin
    assembler;
    asm
        call kgpc_tfile_read_real
    end
end;

procedure file_write_real(var f: file; value: real);
begin
    assembler;
    asm
        call kgpc_tfile_write_real
    end
end;

procedure blockread_impl(var f: file; var buffer; count: longint; result_ptr: pointer);
begin
    assembler;
    asm
        call kgpc_tfile_blockread
    end
end;

procedure blockwrite_impl(var f: file; var buffer; count: longint; result_ptr: pointer);
begin
    assembler;
    asm
        call kgpc_tfile_blockwrite
    end
end;

procedure filepos_impl(var f: file; var position: int64);
begin
    assembler;
    asm
        call kgpc_tfile_filepos
    end
end;

procedure move_impl(var source; var dest; count: longint);
begin
    assembler;
    asm
        movl $KGPC_TARGET_WINDOWS, %eax
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
        call kgpc_move
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
        call kgpc_fillchar
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
        call kgpc_getmem
    end
end;

procedure reallocmem_impl(var target; size: longint);
begin
    assembler;
    asm
        call kgpc_reallocmem
    end
end;

procedure freemem_impl(var target);
begin
    assembler;
    asm
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

{ Function forms for FPC compatibility }
function GetMem(size: longint): Pointer; overload;
var
    p: Pointer;
begin
    getmem_impl(p, size);
    GetMem := p;
end;

function GetMem(size: integer): Pointer; overload;
var
    size_long: longint;
begin
    size_long := size;
    GetMem := GetMem(size_long);
end;

procedure FreeMem(p: Pointer; size: longint); overload;
begin
    { FPC's FreeMem with size parameter just ignores the size and calls freemem }
    freemem_impl(p);
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
        call kgpc_tfile_seek
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
        call kgpc_tfile_truncate_current
    end
end;

procedure Truncate(var f: file; length: longint); overload;
begin
    assembler;
    asm
        call kgpc_tfile_truncate
    end
end;

function IOResult: integer;
begin
    assembler;
    asm
        call kgpc_ioresult_get_and_clear
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
