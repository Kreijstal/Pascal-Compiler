program system;


{ ============================================================================
  FPC System Types for Bootstrap Compatibility
  ============================================================================ }

type
  { Small integer types (FPC-compatible sizes) }
  Byte = 0..255;                 { 8-bit unsigned }
  ShortInt = -128..127;          { 8-bit signed }
  Word = 0..65535;               { 16-bit unsigned }
  SmallInt = -32768..32767;      { 16-bit signed }
  Cardinal = 0..4294967295;      { 32-bit unsigned }
  LongWord = Cardinal;           { 32-bit unsigned }
  DWord = Cardinal;              { 32-bit unsigned }

  { 64-bit helpers
    Note: KGPC currently models these as signed 64-bit under the hood; code uses
    the identifier name to infer "unsigned" behavior where needed. }
  QWord = Int64;
  UInt64 = QWord;

  { Real aliases }
  Single = Real;
  Double = Real;
  Extended = Real;

  { Size types - platform dependent }
  NativeInt = Int64;
  NativeUInt = QWord;
  SizeInt = NativeInt;
  SizeUInt = NativeUInt;
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  
  { Pointer types }
  AnsiChar = Char;
  WideChar = Word;
  UnicodeChar = WideChar;
  PAnsiChar = ^AnsiChar;    { Pointer to ANSI character }
  PPAnsiChar = ^PAnsiChar;  { Pointer to pointer to ANSI character }
  PChar = ^Char;            { Alias for PAnsiChar }
  PPointer = ^Pointer;      { Pointer to pointer }
  PWideChar = ^WideChar;
  PUnicodeChar = ^UnicodeChar;
  
  { Additional common pointer types }
  PByte = ^Byte;
  PWord = ^Word;
  PLongInt = ^LongInt;
  PLongWord = ^LongWord;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PInt64 = ^Int64;
  PQWord = ^QWord;
  PBoolean = ^Boolean;

  { FPC bootstrap compatibility aliases }
  PText = ^text;
  TClass = Pointer;
  TypedFile = file;
  TRTLCriticalSection = array[0..39] of Byte;
  
  { Code page types - for FPC bootstrap compatibility }
  TSystemCodePage = Word;

  { Low-level I/O compatibility types }
  THandle = LongInt;
  HRESULT = LongInt;  { Windows COM result type }
  CodePointer = Pointer;
  
  { String types - for FPC bootstrap compatibility }
  AnsiString = String;
  UnicodeString = String;
  WideString = String;
  RawByteString = String;   { Alias for String type - KGPC doesn't distinguish encoding }
  PAnsiString = ^AnsiString;
  PString = ^String;
  { ShortString: length-prefixed string[255] compatible layout.
    Note: most bootstrap-compatible aliases live in KGPC/stdlib.p (the implicit prelude). }
  ShortString = array[0..255] of Char;
  PWideString = ^WideString;

  TLineEndStr = string[3];
  TextBuf = array[0..255] of AnsiChar;
  TTextBuf = TextBuf;

  { Root class for ObjPas compatibility }
  TObject = class
  end;
  TInterfacedObject = class(TObject)
  end;

  { GUID type for SysUtils compatibility }
  TGUID = record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

  TextRec = record
    Handle: THandle;
    Mode: LongInt;
    BufSize: SizeInt;
    PrivateData: SizeInt;
    BufPos: SizeInt;
    BufEnd: SizeInt;
    BufPtr: ^TextBuf;
    OpenFunc: CodePointer;
    InOutFunc: CodePointer;
    FlushFunc: CodePointer;
    CloseFunc: CodePointer;
    UserData: array[1..32] of Byte;
    Name: array[0..255] of AnsiChar;
    LineEnd: TLineEndStr;
    Buffer: TextBuf;
    CodePage: TSystemCodePage;
  end;

  FileRec = record
    Handle: THandle;
    Mode: LongInt;
    RecSize: SizeInt;
    PrivateData: array[1..64] of Byte;
    UserData: array[1..32] of Byte;
    Name: array[0..255] of AnsiChar;
  end;

  { ============================================================================
    FPC RTL Bootstrap Types
    Types required to compile FPC RTL units (sysutils, etc.)
    ============================================================================ }

  { Fixed-width integer types (FPC compatible) }
  Int8 = ShortInt;               { 8-bit signed }
  UInt8 = Byte;                  { 8-bit unsigned }
  Int16 = SmallInt;              { 16-bit signed }
  UInt16 = Word;                 { 16-bit unsigned }
  Int32 = LongInt;               { 32-bit signed }
  UInt32 = Cardinal;             { 32-bit unsigned }

  { FPC-compatible TDateTime (days since 1899-12-30, with fractional day) }
  TDateTime = type Double;

  { Comp type - 64-bit integer type (FPC Comp is special but Int64 is close) }
  { Note: 'Comp' conflicts with common variable names - use carefully }
  Comp = Int64;

  { Currency type - 64-bit fixed-point decimal scaled by 10000 (4 decimal places) }
  Currency = Int64;              { FPC Currency is Int64 scaled by 10000 }
  PCurrency = ^Currency;

  { Variant type - stub implementation for basic compatibility }
  { Note: Real Variant type requires complex runtime support for:
    - Type coercion between different value types
    - Memory management for string/array variants  
    - COM interoperability (OleVariant)
    This stub only provides the type name for parsing purposes. }
  Variant = Pointer;
  PVariant = ^Variant;
  TVarRec = record
    VType: LongInt;
    VData: Pointer;
  end;

  { Additional pointer types for FPC bootstrap }
  PShortString = ^ShortString;
  PDouble = ^Double;
  PSingle = ^Single;
  PExtended = ^Extended;
  PReal = ^Real;

  { Text line break style enumeration }
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);

  { Unix signal hook state used by SysUtils }
  TSignalState = (ssNotHooked, ssHooked, ssOverridden);

  { Extended boolean aliases (sized boolean types) }
  ByteBool = Boolean8;
  WordBool = Boolean16;
  LongBool = Boolean32;
  QWordBool = Boolean64;

  { Exception is defined in vendor unit sysutils.p - don't define here to avoid duplication }
  ExceptAddr = Pointer;
  TExceptAddr = ExceptAddr;
  PExceptAddr = ^TExceptAddr;

const
  TextRecNameLength = 256;
  TextRecBufSize = 256;

  LineEnding = #10;
  sLineBreak = LineEnding;
  DirectorySeparator: AnsiChar = '/';
  DriveSeparator: AnsiChar = #0;
  PathSeparator: AnsiChar = ':';
  ExtensionSeparator: AnsiChar = '.';
  AllowDirectorySeparators: set of AnsiChar = ['\', '/'];
  AllowDriveSeparators: set of AnsiChar = [];
  MaxPathLen = 4096;

  AllFilesMask = '*';
  FileNameCaseSensitive = true;
  FileNameCasePreserving = true;

  fmClosed = $D7B0;
  fmInput = $D7B1;
  fmOutput = $D7B2;
  fmInOut = $D7B3;

  ARG_MAX = 131072;
  NAME_MAX = 255;
  PATH_MAX = 4095;
  SYS_NMLN = 65;
  SIG_MAXSIG = 128;
  PRIO_PROCESS = 0;
  PRIO_PGRP = 1;
  PRIO_USER = 2;
  UTSNAME_LENGTH = 65;

  RTL_SIGINT = 0;
  RTL_SIGFPE = 1;
  RTL_SIGSEGV = 2;
  RTL_SIGILL = 3;
  RTL_SIGBUS = 4;
  RTL_SIGQUIT = 5;
  RTL_SIGLAST = RTL_SIGQUIT;
  RTL_SIGDEFAULT = -1;

var
  IsLibrary: Boolean = False;
  InOutRes: Word;
  FirstDotAtFileNameStartIsExtension: Boolean;
  DefaultSystemCodePage: TSystemCodePage;
  DefaultUnicodeCodePage: TSystemCodePage;
  DefaultFileSystemCodePage: TSystemCodePage;
  DefaultRTLFileSystemCodePage: TSystemCodePage;
  UTF8CompareLocale: TSystemCodePage;

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

procedure interlocked_exchange_add_i32_impl(var target: longint; value: longint; var result: longint);
begin
    assembler;
    asm
        call kgpc_interlocked_exchange_add_i32
    end
end;

procedure interlocked_exchange_add_i32_int_impl(var target: integer; value: integer; var result: integer);
begin
    assembler;
    asm
        call kgpc_interlocked_exchange_add_i32
    end
end;

procedure interlocked_exchange_add_i64_impl(var target: int64; value: int64; var result: int64);
begin
    assembler;
    asm
        call kgpc_interlocked_exchange_add_i64
    end
end;

procedure interlocked_exchange_add_ptr_impl(var target: Pointer; value: Pointer; var result: Pointer);
begin
    assembler;
    asm
        call kgpc_interlocked_exchange_add_ptr
    end
end;

function kgpc_get_current_dir: AnsiString; external;
function kgpc_set_current_dir(path: PChar): Integer; external;
function kgpc_ioresult_peek: Integer; external;

function InterlockedExchangeAdd(var target: integer; value: integer): integer; overload;
var
    result: integer;
begin
    result := 0;
    interlocked_exchange_add_i32_int_impl(target, value, result);
    InterlockedExchangeAdd := result;
end;

function InterlockedExchangeAdd(var target: longint; value: longint): longint; overload;
var
  EnvP: PPAnsiChar = nil;
  envp: PPAnsiChar = nil;
    result: longint;
begin
    result := 0;
    interlocked_exchange_add_i32_impl(target, value, result);
    InterlockedExchangeAdd := result;
end;

function InterlockedExchangeAdd(var target: int64; value: int64): int64; overload;
var
    result: int64;
begin
    result := 0;
    interlocked_exchange_add_i64_impl(target, value, result);
    InterlockedExchangeAdd := result;
end;

function InterlockedExchangeAdd(var target: Pointer; value: Pointer): Pointer; overload;
var
    result: Pointer;
begin
    result := nil;
    interlocked_exchange_add_ptr_impl(target, value, result);
    Exit(result);
end;

function UpCase(c: char): char; overload;
begin
    if (c >= 'a') and (c <= 'z') then
        UpCase := Chr(Ord(c) - 32)
    else
        UpCase := c;
end;

function UpCase(const s: string): string; overload;
var
    i: longint;
    ch: Char;
begin
    UpCase := s;
    i := 1;
    while i <= Length(s) do
    begin
        ch := s[i];
        if (ch >= 'a') and (ch <= 'z') then
            UpCase[i] := Chr(Ord(ch) - 32);
        i := i + 1;
    end;
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

{ Assign with PAnsiChar - used by FPC bootstrap (objpas.pp) }
procedure assign_text_pchar_internal(var f: text; filename: PAnsiChar);
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

procedure halt; overload;
begin
    assembler;
    asm
        movl $0, %edi
        call exit
    end
end;

procedure halt(exitcode: integer); overload;
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

{ Assign with PAnsiChar - used by FPC bootstrap (objpas.pp) }
procedure assign(var f: text; filename: PAnsiChar); overload;
begin
    assign_text_pchar_internal(f, filename);
end;

{ Assign with AnsiChar (single character) - used by FPC bootstrap (objpas.pp) }
procedure assign(var f: text; filename: AnsiChar); overload;
var
    s: string;
begin
    s := filename;
    assign_text_internal(f, s);
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

procedure tfile_configure_internal(var f: file; recsize: longint; tag: longint);
begin
    assembler;
    asm
        call kgpc_tfile_configure
    end
end;

procedure close(var f: text); overload;
begin
    assembler;
    asm
        call kgpc_text_close
    end
end;

{ SetTextCodePage - Sets the code page for text file encoding
  Note: This is a stub for FPC bootstrap compatibility.
  Currently does nothing as KGPC does not yet implement code page handling. }
procedure SetTextCodePage(var T: Text; CodePage: TSystemCodePage);
begin
    { Stub implementation - code page handling not yet implemented }
end;

procedure SetCodePage(var S: RawByteString; CodePage: TSystemCodePage; Convert: Boolean); overload;
begin
    assembler;
    asm
        call kgpc_set_codepage_string
    end
end;

procedure SetCodePage(var S: UnicodeString; CodePage: TSystemCodePage; Convert: Boolean); overload;
begin
    assembler;
    asm
        call kgpc_set_codepage_string
    end
end;

function ToSingleByteFileSystemEncodedFileName(const S: RawByteString): RawByteString;
begin
    ToSingleByteFileSystemEncodedFileName := S;
end;

function ArrayStringToPPchar(const S: array of RawByteString; reserveentries: LongInt): PPAnsiChar;
begin
    ArrayStringToPPchar := nil;
end;

procedure MkDir(path: string);
begin
    assembler;
    asm
        call kgpc_directory_create
    end
end;

procedure ChDir(const path: RawByteString); overload;
var
    res: integer;
begin
    res := kgpc_set_current_dir(PChar(path));
    InOutRes := Word(res);
end;

procedure ChDir(const path: UnicodeString); overload;
var
    res: integer;
begin
    res := kgpc_set_current_dir(PChar(path));
    InOutRes := Word(res);
end;

procedure GetDir(drivenr: byte; var dir: RawByteString); overload;
begin
    dir := kgpc_get_current_dir();
    InOutRes := Word(kgpc_ioresult_peek());
end;

procedure GetDir(drivenr: byte; var dir: UnicodeString); overload;
begin
    dir := kgpc_get_current_dir();
    InOutRes := Word(kgpc_ioresult_peek());
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

{ Assign with PAnsiChar for typed/untyped files - FPC bootstrap compatibility }
procedure assign(var f: file; filename: PAnsiChar); overload;
begin
    assembler;
    asm
        call kgpc_tfile_assign
    end
end;

{ Assign with AnsiChar (single character filename) for files - FPC bootstrap compatibility }
procedure assign(var f: file; filename: AnsiChar); overload;
var
    s: String;
begin
    s := filename;
    assign(f, s);
end;

procedure rewrite(var f: file); overload;
begin
    assembler;
    asm
        call kgpc_tfile_rewrite
    end
end;

procedure rewrite(var f: file; recsize: longint); overload;
begin
    tfile_configure_internal(f, recsize, 0);
    rewrite(f);
end;

procedure reset(var f: file); overload;
begin
    assembler;
    asm
        call kgpc_tfile_reset
    end
end;

procedure reset(var f: file; recsize: longint); overload;
begin
    tfile_configure_internal(f, recsize, 0);
    reset(f);
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

procedure GetMem(var target; size: longint); overload;
begin
    getmem_impl(target, size);
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
    InOutRes := 0;
    FirstDotAtFileNameStartIsExtension := False;
    DefaultSystemCodePage := 65001;
    DefaultUnicodeCodePage := 65001;
    DefaultFileSystemCodePage := 65001;
    DefaultRTLFileSystemCodePage := DefaultFileSystemCodePage;
    UTF8CompareLocale := DefaultSystemCodePage;
end.
