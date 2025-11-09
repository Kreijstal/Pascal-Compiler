# Negative Array Indexing Support

## Overview

The Gwinn Pascal Compiler (GPC) supports arrays with negative lower bounds, matching the behavior of Free Pascal Compiler (FPC) and standard Pascal. This document describes how negative array indices are implemented and tested.

## Problem Description

Before commit 21c466e, the compiler would crash with a segmentation fault when compiling programs that used arrays with negative lower bounds, such as:

```pascal
var
  SymbolNameList: array[-1..10] of integer;
begin
  SymbolNameList[-1] := 0;
end.
```

## Root Cause

The crash occurred due to incorrect parsing of negative array bounds:

1. **AST Representation**: When the parser encounters a negative number like `-1` in an array bound, it creates a unary minus expression node (AST type `PASCAL_T_NEG`) with a child node containing the absolute value (`1`).

2. **Parsing Failure**: The old code only checked for simple symbol nodes (`node->sym->name`) and failed to handle unary expression nodes, resulting in NULL values for the bounds.

3. **Incorrect Array Classification**: When array bounds couldn't be extracted, the array was incorrectly classified as a "dynamic array" (open array) instead of a static array with known bounds.

4. **Uninitialized Pointer**: Dynamic arrays in GPC use a pointer-based representation that requires dereferencing. Since this pointer was never initialized for what should have been a static array, accessing the array caused a segmentation fault.

## The Fix

The fix (implemented in commit 21c466e and enhanced in this PR) properly extracts negative bounds from the AST:

### Parsing Logic

```c
/* Get lower bound string */
if (lower != NULL) {
    if (lower->sym != NULL && lower->sym->name != NULL) {
        /* Simple identifier or literal (e.g., "1", "MaxValue") */
        lower_str = strdup(lower->sym->name);
    } else if (lower->child != NULL && lower->child->sym != NULL && 
               lower->child->sym->name != NULL) {
        /* Unary expression like -1 */
        if (lower->typ == PASCAL_T_NEG) {
            /* Prepend minus sign */
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "-%s", lower->child->sym->name);
            lower_str = strdup(buffer);
        } else {
            /* Other unary expressions (e.g., +1) */
            lower_str = strdup(lower->child->sym->name);
        }
    }
}
```

### Key Improvements

1. **Unary Expression Handling**: The code now checks for unary expression nodes and extracts the child's value.

2. **Negation Detection**: Uses the named constant `PASCAL_T_NEG` (instead of magic number `15`) to detect unary minus.

3. **Explicit Assertions**: Added assertions to catch buffer overflows and memory allocation failures early:
   ```c
   int written = snprintf(buffer, sizeof(buffer), "-%s", lower->child->sym->name);
   assert(written >= 0 && written < (int)sizeof(buffer) && 
          "Buffer overflow in lower bound formatting");
   ```

4. **Proper Memory Management**: All dynamically allocated strings are properly freed, maintaining the project's zero-leak policy.

## Code Generation

Once the bounds are correctly parsed, the code generator normalizes array indices:

```c
int lower_bound = expr_get_array_lower_bound(array_expr);
if (lower_bound > 0) {
    // For array[1..10], subtract 1 to get 0-based index
    snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", 
             lower_bound, index_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
} else if (lower_bound < 0) {
    // For array[-5..10], add 5 to get 0-based index
    snprintf(buffer, sizeof(buffer), "\taddl\t$%d, %s\n", 
             -lower_bound, index_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
}
```

For an array declared as `array[-1..10]`:
- Accessing index `-1` → normalized to `0` (first element)
- Accessing index `0` → normalized to `1` (second element)
- Accessing index `10` → normalized to `11` (last element)

## Test Coverage

Comprehensive tests ensure negative array indexing works correctly:

### Test Cases

| Test File | Description | Key Scenarios |
|-----------|-------------|---------------|
| `array_neg_index_const.p` | Original issue test case | `array[-1..MaximalList]` where MaximalList is a constant |
| `array_neg_both_bounds.p` | Both bounds negative | `array[-5..-1]` tests all negative indices |
| `array_neg_to_zero.p` | Negative to zero | `array[-10..0]` crosses zero boundary |
| `array_neg_large_range.p` | Large range | `array[-100..100]` tests large ranges |
| `array_neg_char.p` | Different element types | `array[-3..3] of char` tests with char elements |

### Expected Behavior

All tests verify:
1. ✅ Arrays compile without errors
2. ✅ Assignments to negative indices work
3. ✅ Reading from negative indices returns correct values
4. ✅ Boundary values (min and max indices) work correctly
5. ✅ No memory leaks (verified with valgrind)
6. ✅ No segmentation faults

## Limitations

### Known Issues

1. **Multidimensional Arrays**: There is a separate, pre-existing bug with multidimensional arrays where all elements in a row receive the same value. This is unrelated to negative indexing and affects positive indices as well. See GitHub issue #XXX.

2. **Runtime Bounds Checking**: The compiler does not currently perform runtime bounds checking. Accessing an out-of-bounds index will cause undefined behavior.

### Future Enhancements

Potential improvements for negative array indexing:

1. **Range Validation**: Add compile-time validation that array declarations have `lower_bound <= upper_bound`.

2. **Runtime Bounds Checking**: Add optional runtime bounds checking with `-frange-check` flag.

3. **Better Error Messages**: Provide clearer error messages when array bounds cannot be parsed.

## Compatibility

The implementation maintains compatibility with:

- **Free Pascal Compiler (FPC)**: Matches FPC's behavior for negative array indices
- **Standard Pascal**: Conforms to Pascal standard allowing arbitrary integer bounds
- **Delphi**: Compatible with Delphi's array indexing semantics

## References

- Commit 21c466e: "Fix array indexing with constant bounds - complete fix"
- Commit ad2be29: "Fix infinite loops from circular AST references" (added initial test)
- Parser implementation: `GPC/Parser/ParseTree/from_cparser.c`
- Code generator: `GPC/CodeGenerator/Intel_x86-64/codegen_expression.c`
- Test suite: `tests/test_cases/array_neg_*.p`
