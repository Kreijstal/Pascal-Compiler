# FPC Bootstrap Gaps Analysis

This document identifies gaps between KGPC (Kreijstal Gwinn Pascal Compiler) and FPC (Free Pascal Compiler) that prevent bootstrapping FPC's RTL (Runtime Library).

## Investigation Summary

Date: 2025-12-06
FPC Version Analyzed: 3.2.2
FPC Source: https://github.com/fpc/FPCSource.git
Target RTL: rtl/linux/system.pp (737 lines)

## Identified Gaps

### 1. Pointer Indexing (CRITICAL - BLOCKING)

**Status:** ❌ NOT SUPPORTED  
**Priority:** HIGH  
**Impact:** BLOCKING for FPC RTL

FPC supports array-style indexing on pointers: `p[i]` which is equivalent to `(p+i)^` in standard Pascal.

**Example:**
```pascal
var
  arr: array[0..4] of Integer;
  p: PInteger;
  i: Integer;
begin
  p := @arr[0];
  writeln(p[i]);  // This fails in KGPC
end.
```

**Test Case:** `tests/test_cases/fpc_pointer_indexing.p`

**Error in KGPC:**
```
Error on line 0, expression is not indexable as an array.
Error on line 25, write argument 1 must be integer, longint, real, boolean, string, pointer, or enum.
```

**FPC Usage:** Used extensively throughout FPC RTL for efficient memory access and string operations.

**Workaround:** Use pointer dereferencing with inc/dec for pointer arithmetic:
```pascal
p := @arr[0];
writeln(p^);
inc(p);
writeln(p^);
```

### 2. ShortString Type (CRITICAL - BLOCKING)

**Status:** ❌ NOT SUPPORTED  
**Priority:** HIGH  
**Impact:** BLOCKING for FPC RTL

FPC's ShortString is a string type with a length byte at position 0. It's the default string type in FPC mode and heavily used in the RTL.

**Example:**
```pascal
var
  s: ShortString;
begin
  s := 'Hello';
  writeln(ord(s[0]));  // Length byte
  writeln(s[1]);       // First character
end.
```

**Error in KGPC:**
```
Error on line 5: undefined type ShortString
```

**FPC Usage:** Default string type in system unit, used for:
- Parameter passing (especially in syscalls)
- Path operations
- Executable path storage (see `execpathstr` in system.pp)

### 3. Features KGPC Already Supports ✓

The following FPC features ARE supported by KGPC:

1. **QWord type** - Unsigned 64-bit integer ✓
2. **Int64 type** - Signed 64-bit integer ✓
3. **Inc/Dec procedures** - With optional step parameter ✓
4. **FillChar procedure** - Memory initialization ✓
5. **Move procedure** - Memory copy ✓
6. **SHL/SHR operators** - Bitwise shifts ✓
7. **Pointer arithmetic with Inc** - Advancing pointers ✓
8. **Inline assembly** - asm blocks ✓
9. **Assembler functions** - Functions with assembler directive ✓
10. **Units and uses clause** - Modular compilation ✓
11. **Compiler directives** - {$mode objfpc}, etc. ✓

## FPC RTL system.pp Analysis

The Linux x86_64 system.pp file includes:

1. **Interface Section:**
   - Platform-specific includes from rtl/inc/
   - TLS (Thread-Local Storage) support
   - System type definitions
   - Entry point setup

2. **Implementation Section:**
   - Low-level memory management
   - Parameter handling (argc/argv/envp)
   - Exit handling
   - Thread-local storage initialization
   - Inline assembly for CPU-specific operations

3. **Critical Dependencies:**
   - Pointer indexing for memory operations
   - ShortString for path handling
   - External C library functions (when FPC_USE_LIBC defined)
   - Inline assembly for TLS and CPU control

## Build Process

FPC builds system.pp with commands like:
```bash
ppcx64 -Fi../inc -Fi../x86_64 -Fi../unix -Fix86_64 \
       -FE. -FU../../rtl/units/x86_64-linux \
       -Cg -Fl/usr/lib/gcc/x86_64-linux-gnu/13 \
       -dx86_64 -Us -Sg system.pp
```

Key flags:
- `-Fi`: Include directories (inc/, x86_64/, unix/)
- `-FE`: Output directory
- `-FU`: Unit output directory
- `-Cg`: Generate PIC code
- `-Us`: Build system unit
- `-Sg`: Enable goto support

## Next Steps for FPC Bootstrap

To bootstrap FPC with KGPC, the following features MUST be implemented:

### High Priority (Blocking)

1. **Pointer Indexing Support**
   - Implement `pointer[index]` syntax
   - Generate correct address calculation: base + (index * element_size)
   - Support for all pointer types (PChar, PInteger, etc.)

2. **ShortString Type Support**
   - Define ShortString as array[0..255] of char internally
   - Implement length byte at position 0
   - Support string assignment to ShortString
   - Support ShortString indexing (s[i])

### Medium Priority (Helpful)

3. **More Comprehensive Syscall Support**
   - External function declarations
   - cdecl calling convention
   - Platform-specific type sizes

4. **Preprocessor Directives**
   - {$ifdef}, {$endif}, {$else}
   - Platform defines (CPUX86_64, LINUX, etc.)
   - Mode switches ({$mode objfpc})

5. **Advanced Type Features**
   - Type aliases (type PChar = ^Char works, but ensure all cases)
   - Const parameters
   - Var parameters (already works)
   - Open array parameters

## Test Strategy

Each identified gap has:
1. A failing test case in `tests/test_cases/`
2. Expected output validated with FPC
3. Documentation of the failure mode in KGPC

Test naming convention: `fpc_<feature>_<description>.p`

## References

- FPC Source: https://github.com/fpc/FPCSource.git
- FPC Documentation: https://www.freepascal.org/docs.html
- KGPC Repository: https://github.com/Kreijstal/Pascal-Compiler
- System Unit Documentation: https://www.freepascal.org/docs-html/rtl/system/
