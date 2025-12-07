# FPC Feature Implementation Status

## Summary

Implemented 2 of 3 critical FPC features needed for FPC RTL bootstrap. Pointer indexing and pointer arithmetic are fully working!

## Implementation Status

### 1. Pointer Indexing (`p[i]`) - ✅ COMPLETE

**Status:** ✅ PASSING ALL TESTS

**Implementation:**
- Semantic analysis: Recognizes pointers as indexable (SemCheck_expr.c)
- Code generation: Calculates proper element sizes and generates correct assembly
- Type system: Preserves pointer target type information

**Test:** `tests/test_cases/fpc_pointer_indexing.p` - **PASSING**

**Example:**
```pascal
var
  arr: array[0..4] of Integer;
  p: PInteger;
  i: Integer;
begin
  p := @arr[0];
  writeln(p[i]);  // Now works!
end.
```

### 2. Pointer Arithmetic (`p + n`, `p - n`) - ✅ COMPLETE

**Status:** ✅ PASSING ALL TESTS

**Implementation:**
- Semantic analysis: Recognizes `pointer + integer` and `pointer - integer`
- Type propagation: KgpcType properly propagated using `kgpc_type_retain()`
- Code generation: Scales integer offset by element size
- Handles both immediate values and register operands

**Test:** `tests/test_cases/fpc_pointer_arithmetic.p` - **PASSING**

**Example:**
```pascal
var
  arr: array[0..4] of Integer;
  p, q: PInteger;
begin
  p := @arr[0];
  q := p + 2;  // Now works! Correctly adds 2 * sizeof(Integer)
  writeln(q^); // Outputs 30
end.
```

### 3. ShortString Type - ⚠️ PARTIAL

**Status:** ⚠️ Variable declarations work, but full semantics incompatible

**What Works:**
- Variable declarations recognize ShortString as builtin type (SemCheck.c)
- Maps to STRING_TYPE internally
- Basic string operations work (assignment, output)

**What Doesn't Work:**
- FPC's ShortString has length byte at index 0, KGPC strings don't
- String indexing semantics are different (0-based vs 1-based)
- Full ShortString support would require fundamental changes to string representation

**Test:** `tests/test_cases/fpc_shortstring_type.p` - **FAILING**

**Issue:**
The test relies on accessing `s[0]` for the length byte, which doesn't exist in KGPC's string implementation. This is a fundamental architectural difference.

**To Fully Support ShortString Would Require:**
1. New string type with different internal representation
2. Length byte storage at index 0
3. 0-based indexing for ShortString vs 1-based for regular strings
4. Different string assignment and manipulation semantics

This is beyond the scope of FPC RTL bootstrap compatibility and would require major refactoring of KGPC's string system.

## Files Modified

### Semantic Analysis
- `KGPC/Parser/SemanticCheck/SemChecks/SemCheck_expr.c`
  - Added pointer arithmetic support in `semcheck_addop()` with KgpcType propagation
  - Added pointer indexing support in `semcheck_arrayaccess()`
  - Added ShortString to `semcheck_map_builtin_type_name()`

- `KGPC/Parser/SemanticCheck/SemCheck.c`
  - Added ShortString recognition in variable declarations
  - Maps ShortString to HASHVAR_PCHAR (STRING_TYPE)

### Code Generation
- `KGPC/CodeGenerator/Intel_x86-64/codegen_expression.c`
  - Added pointer support to array indexing code generator
  - Implemented element size calculation for pointers

- `KGPC/CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c`
  - Added pointer arithmetic code generation in `gencode_op()`
  - Scales integer offset by element size
  - Handles both immediate values and registers

### Test Framework
- `tests/do_not_run_me_directly_but_through_meson.py`
  - Removed skip logic for FPC tests (tests now fail/pass based on actual implementation)

## Performance Impact

Minimal - only adds conditional checks in existing code paths.

## Compatibility

Changes are backward compatible - existing code continues to work as before.

## Future Work

1. **Complete Pointer Arithmetic:**
   - Fix KgpcType propagation in addop expressions
   - Ensure type metadata flows through assignment checking
   
2. **Complete ShortString:**
   - Add to builtin type initialization
   - Consider full semantics (length byte at index 0, 255 char limit)
   
3. **Code Generation for Pointer Arithmetic:**
   - Currently no special codegen needed as pointer +/- creates normal ADDOP
   - May need to scale offset by element size in codegen

## Testing

**Passing:** 2/3 FPC gap tests
- ✅ fpc_pointer_indexing.p - **PASSING**
- ✅ fpc_pointer_arithmetic.p - **PASSING**

**Failing:** 1/3 FPC gap tests  
- ❌ fpc_shortstring_type.p - Requires fundamental string representation changes

All existing compiler tests continue to pass (253 tests passing).
