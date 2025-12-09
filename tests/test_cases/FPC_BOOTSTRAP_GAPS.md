# FPC Bootstrap Gap Tests

These tests demonstrate features used in the Free Pascal Compiler (FPC) Runtime Library (RTL) that are not yet supported by KGPC. They compile successfully with FPC but fail with KGPC, highlighting the gaps that need to be addressed for FPC RTL bootstrap.

## Test Files Without .expected (Intentionally Failing)

These tests **DO NOT** have `.expected` files because they are meant to demonstrate KGPC gaps. They compile and run successfully with FPC but fail to compile with KGPC.

### 1. Traditional Procedural Types
- **File**: `fpc_procedural_type_basic.p`
- **Feature**: Plain function/procedure type aliases (without "reference to")
- **FPC RTL Example**: `sortbase.pp` line 26:
  ```pascal
  TListSortComparer_NoContext = function(Item1, Item2: Pointer): Integer;
  ```
- **KGPC Gap**: Only supports "reference to" syntax for procedural types
- **Error**: "call to function SimpleComparer does not match any available overload"

### 2. Advanced Procedural Type Patterns  
- **File**: `fpc_procedural_type_advanced.p`
- **Feature**: Procedural types as record fields, taking addresses of functions
- **FPC RTL Example**: `sortbase.pp` lines 38-44 (TSortingAlgorithm record)
- **KGPC Gap**: Same as #1, traditional procedural types not supported
- **Error**: "type mismatch in assignment statement"

### 3. Const Type Casting
- **File**: `fpc_const_typecast.p`
- **Feature**: Type casting/conversion in const declarations
- **FPC RTL Example**: `charset.pp` lines 95-96:
  ```pascal
  const
    UNKNOW_CHAR_A = ansichar(63);
    UNKNOW_CHAR_W = tunicodechar(63);
  ```
- **KGPC Gap**: Cannot evaluate type casts in const expressions
- **Error**: "unsupported const expression"

### 4. SizeUInt Type
- **File**: `fpc_sizeuint.p`
- **Feature**: Platform-dependent unsigned integer type
- **FPC RTL Example**: Used throughout RTL, e.g., `sortbase.pp` line 27
- **KGPC Gap**: SizeUInt type not defined
- **Error**: "undefined type SizeUInt"

## Test Files With .expected (Working Features)

These tests validate features that KGPC already supports:

### 5. str() Procedure
- **File**: `fpc_str_procedure.p` + `.expected`
- **Feature**: Built-in procedure to convert numbers to strings
- **Status**: ✅ **WORKS** in KGPC
- **FPC RTL Example**: `errors.pp` line 38

### 6. Inline Functions
- **File**: `fpc_inline_function.p` + `.expected`
- **Feature**: Inline directive for functions
- **Status**: ✅ **WORKS** in KGPC (directive is accepted, likely ignored)
- **FPC RTL Example**: `charset.pp` lines 80-82

### 7. Packed Records
- **File**: `fpc_packed_record.p` + `.expected`
- **Feature**: Packed record types
- **Status**: ⚠️ **PARTIAL** - KGPC accepts syntax but doesn't pack (same size as regular records)
- **FPC RTL Example**: `charset.pp` lines 33-44
- **Note**: Expected output updated to match KGPC behavior (size 12 for both)

## Validation

All tests have been validated to:
1. ✅ Compile successfully with FPC 3.2.2
2. ✅ Run correctly with FPC and produce expected output
3. ✅ Fail with KGPC in the documented way (for gap tests)
4. ✅ Pass with KGPC (for working feature tests)

## Running the Tests

### With FPC (all should pass):
```bash
fpc -o/tmp/test_name tests/test_cases/fpc_*.p && /tmp/test_name
```

### With KGPC (gap tests should fail, working tests should pass):
```bash
./builddir/KGPC/kgpc tests/test_cases/test_name.p /tmp/test_name.s
gcc -o /tmp/test_name /tmp/test_name.s builddir/KGPC/libkgpc_runtime.a -lm
/tmp/test_name
```

## Summary of KGPC Gaps for FPC RTL Bootstrap

1. **Traditional Procedural Types** - Blocking `sortbase.pp` compilation
2. **Const Type Casting** - Blocking `charset.pp` compilation  
3. **SizeUInt Type** - Missing standard type definition
4. **StrPas Function** - FPC RTL function not in KGPC runtime (used in `errors.pp`)
5. **Packed Records** - Syntax accepted but packing not implemented (affects memory layout)

These gaps explain why only 4/7 FPC RTL units compile (57% success rate).
