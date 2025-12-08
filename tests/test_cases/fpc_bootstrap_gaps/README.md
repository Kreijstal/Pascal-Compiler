# FPC Bootstrap Gap Tests

This directory contains tests that demonstrate specific gaps preventing full FPC bootstrap with KGPC.

## Purpose

These tests document features that FPC's RTL actually uses and that KGPC must implement for successful bootstrap.

Each test:
- ✅ Compiles and runs correctly with FPC
- Status varies with KGPC (some now fixed, some still failing)
- Documents which FPC RTL units need this feature

## Tests

### 1. `const_expr_functions.p` - ✅ FIXED

**Gap**: Const expression function support - Chr()

**FPC Behavior**: Supports `Chr()` function in const expressions

**KGPC Behavior**: ✅ NOW SUPPORTS Chr() (fixed in commit b78ef42)

**Impact**: 
- `charset.pp` - uses Chr() extensively in const initialization

**Status**: ✅ NOW COMPILES with KGPC

---

### 2. `implicitsystemimport.p` - ✅ FIXED

**Gap**: Implicit system unit import

**FPC Behavior**: Automatically imports system unit types for all units

**KGPC Behavior**: ✅ NOW AUTOMATICALLY AVAILABLE (fixed in commit b78ef42)

**Impact**:
- `strings.pp` - uses SizeInt, PAnsiChar without importing
- `sortbase.pp` - uses SizeUInt, PPointer without importing
- Most RTL units - depend on system types being available

**Status**: ✅ NOW COMPILES with KGPC

---

### 3. `include_directive.p` - ✅ PASSING

**Gap**: None - include directives work

**FPC Behavior**: {$I filename} includes a file inline

**KGPC Behavior**: ✅ WORKS CORRECTLY

**Impact**: 
- Used by sysutils.pp, classes.pp, and many other RTL units

**Status**: ✅ COMPILES with KGPC

---

### 4. `procedure_default_params.p` - ⚠️ INVESTIGATION NEEDED

**Gap**: Default/optional parameters (objfpc mode)

**FPC Behavior**: Supports default parameter values in objfpc mode

**KGPC Behavior**: ⚠️ Compiles but may not handle defaults correctly

**Impact**: 
- Used in various RTL units for optional parameters

**Status**: ⚠️ Needs investigation

---

### 5. `dos_freemem.p` - ✅ FIXED

**Gap**: GetMem/FreeMem function forms

**FPC Behavior**: GetMem can be used as function returning Pointer, FreeMem with optional size

**KGPC Behavior**: ✅ NOW SUPPORTS function forms (fixed)

**Impact**: 
- Required by memory management code in RTL units

**Status**: ✅ NOW COMPILES with KGPC



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
