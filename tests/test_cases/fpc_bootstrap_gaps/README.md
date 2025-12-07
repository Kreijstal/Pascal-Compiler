# FPC Bootstrap Gap Tests

This directory contains **FAILING** tests that demonstrate specific gaps preventing full FPC bootstrap with KGPC.

## Purpose

These tests are NOT part of the automatic test suite. They document features that FPC's RTL actually uses and that KGPC must implement for successful bootstrap.

Each test:
- ✅ Compiles and runs correctly with FPC
- ❌ Fails to compile with KGPC due to missing feature
- Documents which FPC RTL units need this feature

## Tests

### 1. `const_expr_functions.p`

**Gap**: Const expression function support

**FPC Behavior**: Supports `Chr()` function in const expressions

**KGPC Behavior**: Only supports `Ord()`, `High()`, `Low()`, `SizeOf()`

**Impact**: 
- `charset.pp` fails - uses Chr() extensively in const initialization
- Other RTL units with character/string const initialization fail

**Example from FPC RTL**:
```pascal
const
  UpperCaseA = Chr(65);  // FPC: OK, KGPC: Error
```

**Fix Needed**: Expand const expression evaluator to support Chr() function

---

### 2. `implicit_system_import.p`

**Gap**: Implicit system unit import

**FPC Behavior**: Automatically imports system unit types for all units

**KGPC Behavior**: Requires explicit `uses system` clause

**Impact**:
- `strings.pp` fails - uses SizeInt, PAnsiChar without importing
- `sortbase.pp` fails - uses SizeUInt, PPointer without importing
- Most RTL units fail - they depend on system types being available

**Example from FPC RTL**:
```pascal
unit Strings;
interface
// No 'uses system' clause, but uses system types:
function strcomp(str1, str2: PAnsiChar): SizeInt;  // FPC: OK, KGPC: Error
```

**Fix Needed**: Auto-import system unit for all units (except system itself)

---

## Running These Tests

### With FPC (should succeed):
```bash
cd tests/test_cases/fpc_bootstrap_gaps
fpc const_expr_functions.p
./const_expr_functions
```

### With KGPC (should fail):
```bash
cd tests/test_cases/fpc_bootstrap_gaps
../../builddir/KGPC/kgpc const_expr_functions.p output.s
```

## When to Remove Tests

When a gap is fixed:
1. The test should compile successfully with KGPC
2. Verify it produces correct output
3. Move to main test_cases/ directory (if appropriate)
4. Update this README

## Critical Blockers

Based on actual FPC RTL testing:

1. **Implicit system import** - Blocks: strings.pp, sortbase.pp, and most RTL units
2. **Chr() in const expressions** - Blocks: charset.pp and related units

These MUST be fixed before FPC bootstrap can progress beyond system.pp and ctypes.pp.
