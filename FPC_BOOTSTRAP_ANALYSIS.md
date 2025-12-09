# FPC RTL Bootstrap Gap Analysis

This document summarizes the investigation into compiling the Free Pascal Compiler (FPC) Runtime Library (RTL) with KGPC.

## Current Status: 4/7 units compile (57%)

### ✅ Successfully Compiling Units
- `system.pp`
- `ctypes.pp`
- `strings.pp`
- `unixtype.pp`

### ❌ Failing Units
- `sortbase.pp` - Needs procedural types
- `charset.pp` - Needs const type casting
- `errors.pp` - Needs StrPas function

## Test Suite Created

A comprehensive test suite has been created in `tests/test_cases/` with the prefix `fpc_*` to demonstrate each gap. See detailed documentation in:

**`tests/test_cases/FPC_BOOTSTRAP_GAPS.md`**

## Identified Compiler Gaps

### 1. Traditional Procedural Types ⭐ HIGH PRIORITY
**Impact**: Blocks `sortbase.pp` compilation

**Issue**: KGPC only supports "reference to" syntax for procedural types, but FPC RTL uses traditional Pascal function pointer syntax.

**FPC Pattern**:
```pascal
type
  TComparer = function(Item1, Item2: Pointer): Integer;
```

**KGPC Limitation**: Must use `reference to`:
```pascal
type
  TComparer = reference to function(Item1, Item2: Pointer): Integer;
```

**Test Files**:
- `fpc_procedural_type_basic.p` - Basic usage
- `fpc_procedural_type_advanced.p` - In records
- `fpc_sortbase_minimal.p` - Complete sortbase.pp pattern

### 2. Const Type Casting ⭐ HIGH PRIORITY
**Impact**: Blocks `charset.pp` compilation

**Issue**: Cannot use type casting/conversion in const declarations.

**FPC Pattern**:
```pascal
const
  UNKNOW_CHAR_A = ansichar(63);
  MAX_BYTE = byte(255);
```

**KGPC Error**: "unsupported const expression"

**Test File**: `fpc_const_typecast.p`

### 3. Missing SizeUInt Type ⭐ MEDIUM PRIORITY
**Impact**: Used throughout FPC RTL

**Issue**: Platform-dependent unsigned integer type not defined in KGPC.

**FPC Usage**: `SizeUInt` is used extensively for array indices and memory sizes.

**Test File**: `fpc_sizeuint.p`

### 4. Missing StrPas Function ⭐ HIGH PRIORITY
**Impact**: Blocks `errors.pp` compilation

**Issue**: Runtime function to convert PChar to string is missing.

**FPC Pattern**:
```pascal
StrError := StrPas(Sys_ErrList[err]);
```

**Test File**: `fpc_pansichar_array.p`

### 5. Const Record Initialization
**Impact**: Blocks advanced sortbase.pp patterns

**Issue**: Cannot initialize const records with function pointer addresses.

**FPC Pattern**:
```pascal
const
  QuickSort: TSortingAlgorithm = (
    Sorter: @DefaultSorter;
    Comparer: @DefaultComparer;
  );
```

**KGPC Error**: "Circular reference detected in const section"

**Test File**: `fpc_sortbase_minimal.p`

### 6. Compiler Directives
**Impact**: FPC RTL uses conditional compilation extensively

**Issue**: KGPC preprocessor doesn't support FPC-style directives.

**FPC Pattern**:
```pascal
{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}
```

**KGPC Error**: "malformed conditional directive"

**Test File**: `fpc_compiler_directives.p`

### 7. Packed Records (Partial)
**Impact**: Memory layout compatibility

**Issue**: KGPC accepts `packed record` syntax but doesn't actually pack the data.

**Status**: Syntax works, but both regular and packed records have the same size.

**Test File**: `fpc_packed_record.p`

## Validation

All test files:
- ✅ Compile successfully with FPC 3.2.2
- ✅ Run correctly with FPC
- ❌ Fail with KGPC (demonstrating the gap)

## Recommendations for FPC Bootstrap Support

### Phase 1: Critical Gaps (Enable sortbase.pp, charset.pp, errors.pp)
1. **Implement traditional procedural types** - Most critical, blocks sortbase.pp
2. **Support const type casting** - Blocks charset.pp
3. **Add StrPas runtime function** - Blocks errors.pp

### Phase 2: Standard Types
4. **Define SizeUInt type** - Used throughout RTL
5. **Fix const record initialization** - Advanced patterns

### Phase 3: Enhanced Compatibility
6. **Improve compiler directives** - Better FPC source compatibility
7. **Implement proper packed records** - Memory layout compatibility

## Testing Strategy

The test suite in `tests/test_cases/` provides:
- **Minimal reproductions** of each gap
- **Validation** that features work in FPC
- **CI integration** that will fail until gaps are fixed
- **Regression prevention** once fixed

## References

- FPC Source: https://github.com/fpc/FPCSource.git
- FPC RTL Units: `/rtl/inc/` (sortbase.pp, charset.pp), `/rtl/unix/` (errors.pp)
- Test Documentation: `tests/test_cases/FPC_BOOTSTRAP_GAPS.md`
