# FPC Bootstrap Gaps Analysis

This document identifies gaps between KGPC (Kreijstal Gwinn Pascal Compiler) and FPC (Free Pascal Compiler) that prevent bootstrapping FPC's RTL (Runtime Library).

## Investigation Summary

Date: 2025-12-06
FPC Version Analyzed: 3.2.2
FPC Source: https://github.com/fpc/FPCSource.git
Target RTL: rtl/linux/system.pp (737 lines)

## Identified Gaps

### 1. Pointer Indexing (NOW WORKING ✓)

**Status:** ✅ SUPPORTED  
**Priority:** N/A  
**Impact:** N/A - Feature now works

FPC supports array-style indexing on pointers: `p[i]` which is equivalent to `(p+i)^` in standard Pascal.

**Test Case:** `tests/test_cases/fpc_pointer_indexing.p`

**Status Update (2025-12-12):** This feature is now fully supported in KGPC.

### 1b. Pointer Arithmetic with +/- Integer (NOW WORKING ✓)

**Status:** ✅ SUPPORTED  
**Priority:** N/A  
**Impact:** N/A - Feature now works

FPC supports pointer arithmetic using + and - operators: `p + n` and `p - n` where p is a pointer and n is an integer.

**Test Case:** `tests/test_cases/fpc_pointer_arithmetic.p`

**Status Update (2025-12-12):** This feature is now fully supported in KGPC.

### 1c. Pointer-Pointer Subtraction (CRITICAL - BLOCKING)

**Status:** ❌ NOT SUPPORTED  
**Priority:** HIGH  
**Impact:** BLOCKING for FPC RTL

When two pointers of the same type are subtracted, the result should be the difference in elements (not bytes) between the two addresses. This is essential for string length calculations (e.g., strlen).

**Example:**
```pascal
var
  arr: array[0..9] of Byte;
  p, q: PByte;
  diff: LongInt;
begin
  p := @arr[7];
  q := @arr[2];
  diff := p - q;  // Should be 5
  writeln(diff);
end.
```

**Test Case:** `tests/test_cases/fpc_pointer_subtraction.p`

**Error in KGPC:**
```
Error: expected int/real on both sides of addop
Error: type mismatch in assignment statement for diff (lhs: longint, rhs: pointer)
```

**FPC Usage:** Used in string operations (strlen), memory management, and pointer difference calculations.

### 1d. Exit() with Return Value (CRITICAL - BLOCKING)

**Status:** ❌ NOT SUPPORTED  
**Priority:** HIGH  
**Impact:** BLOCKING for FPC RTL

FPC allows `Exit(value)` as a shorthand for setting the result and returning from a function.

**Example:**
```pascal
function GetValue: Integer;
begin
  Exit(42);  // Sets result to 42 and returns
end;
```

**Test Case:** `tests/test_cases/fpc_exit_value.p`

**Error in KGPC:**
```
Error in function GetFive: no return statement declared in function body!
```

**FPC Usage:** Common idiom throughout FPC RTL for early returns with values.

### 2. ShortString Type (NOW WORKING ✓)

**Status:** ✅ SUPPORTED  
**Priority:** N/A  
**Impact:** N/A - Feature now works

FPC's ShortString is a string type with a length byte at position 0. It's the default string type in FPC mode and heavily used in the RTL.

**Test Case:** `tests/test_cases/fpc_shortstring_type.p`

**Status Update (2025-12-12):** This feature is now fully supported in KGPC.

### 3. Set Constants in const Sections (CRITICAL - BLOCKING)

**Status:** ❌ NOT SUPPORTED  
**Priority:** HIGH  
**Impact:** BLOCKING for FPC RTL

KGPC cannot evaluate set constants such as `set of char` literals or ranges used inside `const` blocks.  
FPC's `system.pp` defines directory separator sets this way:

```pascal
const
  AllowDirectorySeparators : set of char = ['\', '/'];
  AllowDriveSeparators     : set of char = [];
```

**Test Case:** `tests/test_cases/fpc_set_constant_char.p`  
**KGPC Error:**
```
Error on line 4, unsupported const expression.
```

**FPC Usage:** Needed early in `system.pp` for path parsing; failure stops RTL bootstrap.

### 4. Features KGPC Already Supports ✓

The following FPC features ARE supported by KGPC:

1. **QWord type** - Unsigned 64-bit integer ✓
2. **Int64 type** - Signed 64-bit integer ✓
3. **Inc/Dec procedures** - With optional step parameter ✓
4. **FillChar procedure** - Memory initialization ✓
5. **Move procedure** - Memory copy ✓
6. **SHL/SHR operators** - Bitwise shifts ✓
7. **Pointer arithmetic with Inc/Dec** - Advancing pointers with inc(p) ✓
8. **Inline assembly** - asm blocks ✓
9. **Assembler functions** - Functions with assembler directive ✓
10. **Units and uses clause** - Modular compilation ✓
11. **Compiler directives** - {$mode objfpc}, etc. ✓
12. **SizeOf function** - Type and variable size calculation ✓
13. **Length function** - String length ✓
14. **High/Low functions** - Array bounds ✓

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
   - Related: pointer arithmetic with +/- operators

2. **Pointer Arithmetic Operators**
   - Implement `pointer + integer` and `pointer - integer`
   - Calculate: new_pointer = old_pointer + (offset * element_size)
   - Support for pointer subtraction to get element count
   - Ensure type safety (pointer of same type)

3. **ShortString Type Support**
   - Define ShortString as array[0..255] of char internally
   - Implement length byte at position 0
   - Support string assignment to ShortString
   - Support ShortString indexing (s[i])
   - Handle automatic conversion from string literals

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

**Current Test Cases:**
- `fpc_pointer_indexing.p` - Tests pointer[index] syntax
- `fpc_pointer_arithmetic.p` - Tests pointer +/- integer operators  
- `fpc_shortstring_type.p` - Tests ShortString type support

## References

- FPC Source: https://github.com/fpc/FPCSource.git
- FPC Documentation: https://www.freepascal.org/docs.html
- KGPC Repository: https://github.com/Kreijstal/Pascal-Compiler
- System Unit Documentation: https://www.freepascal.org/docs-html/rtl/system/
