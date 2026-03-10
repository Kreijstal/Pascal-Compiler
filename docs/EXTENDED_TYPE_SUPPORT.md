# Refactoring Plan: 80-bit Extended Floating-Point Type Support

## Background

The x86_64 architecture supports three floating-point sizes:
- **Single** (32-bit): `float` in C, `Single` in Pascal
- **Double** (64-bit): `double` in C, `Real`/`Double` in Pascal
- **Extended** (80-bit): `long double` in C (on Linux x86_64), `Extended` in Pascal

KGPC currently maps `Extended` to `Double` (64-bit) and does not define
`FPC_HAS_TYPE_EXTENDED`. This causes the FPC RTL to use SSE-based code paths,
which is correct for our current codegen.

### Why Extended Matters

The FPC RTL's x86_64 platform includes (`rtl/x86_64/mathu.inc`, `math.inc`)
inline assembly functions that use x87 FPU instructions with the `extended`
calling convention:
- Arguments passed **on the stack** (not in `%xmm` registers)
- Return values via **`st(0)`** (x87 FPU stack)
- Instructions: `fldt`, `fstpt`, `fyl2x`, `fpatan`, `fsincos`, etc.

These are guarded by `{$ifdef FPC_HAS_TYPE_EXTENDED}`.

### Current Workaround

We do NOT define `FPC_HAS_TYPE_EXTENDED`, so the FPC RTL falls back to:
- Generic Pascal implementations (e.g., `Log2 := Ln(x) / Ln(2.0)`)
- SSE-based `nostackframe` assembly (e.g., `cvttsd2si %xmm0, %rax` for Trunc)

## Architecture

### x86_64 System V ABI for `long double` (Extended)

| Aspect | Double (64-bit) | Extended (80-bit) |
|--------|----------------|-------------------|
| Storage | 8 bytes | 10 bytes (padded to 16) |
| Alignment | 8 bytes | 16 bytes |
| Parameter passing | `%xmm0`-`%xmm7` | On the **stack** (memory class) |
| Return value | `%xmm0` | `st(0)` (x87 FPU register) |
| Instructions | SSE: `movsd`, `addsd`, `mulsd` | x87: `fldt`, `faddp`, `fmulp` |
| Constant storage | `.quad` (8 bytes) | `.byte` (10 bytes) or `.tfloat` |

### x87 FPU Register Model

The x87 FPU uses a **stack** of 8 registers (`st(0)` through `st(7)`), not
named registers like SSE. Operations are LIFO:
- `fld` pushes a value onto the stack
- `fadd`, `fmul` operate on top-of-stack
- `fstp` pops and stores
- `fyl2x` computes `st(1) * log2(st(0))`, pops both, pushes result

This is fundamentally different from SSE's named-register model.

## Implementation Plan

### Phase 1: Type System

**Files:** `KGPC/Parser/ParseTree/type_tags.h`, `KgpcType.h`, `KgpcType.c`

1. Add `EXTENDED_TYPE` tag (value 12 or next available) in `type_tags.h`
2. Add `Extended` as a primitive type alias with `size_in_bytes = 16`
   (10 bytes data + 6 bytes padding for alignment)
3. Update `kgpc_type_sizeof()`, `kgpc_type_is_real()` to handle `EXTENDED_TYPE`
4. Update `pascal_identifier_equals` checks for "Extended" to resolve to
   `EXTENDED_TYPE` instead of `REAL_TYPE`

### Phase 2: Stack & Memory Layout

**Files:** `KGPC/CodeGenerator/Intel_x86-64/stackmng/stackmng.c`

1. Update `stack_slot_alignment()` to return 16 for extended-type variables
2. Update `add_l_x()` to accept a type tag or alignment hint so extended
   variables get 16-byte aligned stack slots
3. Ensure `codegen_stack_space()` emits 16-byte-aligned `subq` for the frame

### Phase 3: Load/Store Instructions

**Files:** `KGPC/CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c`

Replace the `load_real_operand_into_xmm()` path for extended operands:

```
; Load extended from memory to x87 stack
fldt    -offset(%rbp)          ; push 80-bit value onto st(0)

; Store extended from x87 stack to memory
fstpt   -offset(%rbp)          ; pop st(0) into 80-bit memory
```

Need a new function `load_extended_operand_to_x87()` parallel to
`load_real_operand_into_xmm()`.

### Phase 4: Arithmetic

**Files:** `expr_tree.c` — `gencode_real_binary_op()`

For extended operands, replace SSE binary ops with x87:

```
; Extended addition: a + b
fldt    a                      ; push a → st(0)
fldt    b                      ; push b → st(0), a → st(1)
faddp   %st, %st(1)           ; st(1) += st(0), pop → result in st(0)
fstpt   result                 ; pop result to memory
```

Key design decision: **always spill x87 results to memory** after each
operation (don't try to keep values in the x87 stack across expressions).
This simplifies register management at the cost of speed.

### Phase 5: Parameter Passing

**Files:** `KGPC/CodeGenerator/Intel_x86-64/codegen_expression.c`

Per x86_64 System V ABI, `long double` arguments are passed in **memory**
(the MEMORY class), meaning they go on the stack:

```
; Call foo(extended_val)
subq    $16, %rsp              ; make room on stack
fldt    extended_val
fstpt   (%rsp)                 ; push 80-bit value onto call stack
call    foo
addq    $16, %rsp              ; clean up
```

This is completely different from the xmm-register path. The codegen needs
to detect extended arguments and use the stack path instead of `movsd %xmmN`.

**Key locations to modify:**
- `codegen_expression.c:8363` — `is_real_arg` detection
- `codegen_expression.c:8562` — xmm register assignment
- Argument counting — extended args do NOT consume an xmm slot

### Phase 6: Return Values

**Files:** `KGPC/CodeGenerator/Intel_x86-64/codegen.c:5395-5424`

For functions returning `Extended`:

```
; Caller reads return value
; Result is in st(0) after call returns
fstpt   -offset(%rbp)          ; store to local variable
```

For function epilogue (returning extended):
```
; Load return variable into st(0)
fldt    -offset(%rbp)
; (leave/ret follows)
```

### Phase 7: Type Conversions

New conversion instructions needed:

| From | To | Instruction Sequence |
|------|----|--------------------|
| Double → Extended | `movsd xmm0, mem; fld qword ptr mem` |
| Extended → Double | `fldt mem; fstp qword ptr mem; movsd mem, xmm0` |
| Single → Extended | `movss xmm0, mem; fld dword ptr mem` |
| Extended → Integer | `fldt mem; fistp qword ptr mem` |
| Integer → Extended | `fild qword ptr mem; fstpt mem` |

### Phase 8: Constants

Extended-precision constants in `.rodata`:

```asm
.section .rodata
.align 16
.LC_ext_pi:
    .byte 0x35,0xc2,0x68,0x21,0xa2,0xda,0x0f,0xc9,0x00,0x40
    .zero 6    ; padding to 16 bytes
```

Or using GAS `.tfloat` directive if available.

### Phase 9: Enable FPC_HAS_TYPE_EXTENDED

Once all the above works:
1. Re-enable `FPC_HAS_TYPE_EXTENDED` in `preprocess_main.c` and
   `pascal_frontend.c`
2. The FPC RTL x87 asm functions (arctan2, tan, cotan, log2, sincos with
   extended params) will now work correctly
3. Remove the `nostackframe` epilogue fix (Phase 0) — it's still needed
   as a general correctness fix, but the x87 functions won't need it

### Phase 10: Testing

- All existing tests should still pass (Double codegen unchanged)
- Add test cases for Extended arithmetic, parameter passing, return values
- FPC RTL tests with `FPC_HAS_TYPE_EXTENDED` should now pass the x87 paths
- Verify `TExtended80Rec` type helper works

## Estimated Complexity

| Phase | Effort | Risk |
|-------|--------|------|
| 1. Type system | Small | Low |
| 2. Stack layout | Small | Low |
| 3. Load/Store | Medium | Medium |
| 4. Arithmetic | Medium | Medium |
| 5. Parameter passing | Large | High — must handle mixed extended/double args |
| 6. Return values | Medium | Medium |
| 7. Conversions | Medium | High — many edge cases |
| 8. Constants | Small | Low |
| 9. Enable define | Small | Medium — may expose other FPC RTL issues |

## Key Files

| File | Role |
|------|------|
| `KGPC/Parser/ParseTree/type_tags.h` | Add EXTENDED_TYPE constant |
| `KGPC/Parser/ParseTree/KgpcType.c` | Size/alignment for extended |
| `KGPC/CodeGenerator/Intel_x86-64/stackmng/stackmng.c` | 16-byte aligned slots |
| `KGPC/CodeGenerator/Intel_x86-64/expr_tree/expr_tree.c` | x87 load/store/arithmetic |
| `KGPC/CodeGenerator/Intel_x86-64/codegen_expression.c` | Stack-based param passing |
| `KGPC/CodeGenerator/Intel_x86-64/codegen.c` | x87 return values |
| `KGPC/CodeGenerator/Intel_x86-64/register_types.h` | x87 stack model (if needed) |
| `KGPC/preprocess_main.c` | Re-enable FPC_HAS_TYPE_EXTENDED |

## Alternative: Minimal Extended Support

Instead of full x87 codegen, a simpler approach:

1. Map `Extended` to a 16-byte storage type
2. Convert to/from `Double` at every load/store boundary
3. Use SSE for all arithmetic (losing precision beyond 53 bits)
4. Only use x87 for the specific FPC RTL asm functions via thunks

This would be much less work but wouldn't provide true 80-bit precision.
