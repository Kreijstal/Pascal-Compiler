# FPC Bootstrap Failing Tests

This directory contains test cases that demonstrate gaps in KGPC's FPC compatibility.

These tests:
- ✅ Compile and run successfully with FPC (Free Pascal Compiler)
- ❌ Fail to compile with KGPC due to missing features

## Purpose

These tests document specific features needed for FPC bootstrap that are not yet implemented in KGPC.

## Tests

### fpc_pas_extension_test.p

**Gap:** Unit file extension support

**Description:** KGPC only searches for unit files with `.p` extension, while FPC uses `.pas` and `.pp` extensions.

**Unit file:** `fpc_pas_extension_unit.pas` (note the `.pas` extension)

**Expected behavior with FPC:**
```bash
fpc fpc_pas_extension_unit.pas
fpc fpc_pas_extension_test.p
./fpc_pas_extension_test
```
Output:
```
Color: Red
MaxValue: 100
GlobalCounter: 42
```

**Actual behavior with KGPC:**
```bash
kgpc fpc_pas_extension_test.p output.s
```
Fails with errors like:
```
Error on line 6: undefined type TColor
...
```

Because KGPC cannot find `fpc_pas_extension_unit.pas` (only looks for `.p` files).

**Fix needed:** Modify `KGPC/unit_paths.c:build_candidate_unit_path()` to try `.pas`, `.pp`, and `.p` extensions.

## Running These Tests

These tests are **NOT** part of the automatic test suite. They are kept separate because they are known to fail.

To manually test with FPC:
```bash
cd tests/test_cases/fpc_bootstrap_failing
fpc fpc_pas_extension_unit.pas
fpc fpc_pas_extension_test.p
./fpc_pas_extension_test
```

To verify they fail with KGPC:
```bash
cd tests/test_cases/fpc_bootstrap_failing
../../builddir/KGPC/kgpc fpc_pas_extension_test.p output.s
```

## When to Move Tests Out

When a gap is fixed, the corresponding test should be:
1. Moved from `fpc_bootstrap_failing/` to the main `test_cases/` directory
2. Updated to use `.p` extension for units (if needed for compatibility)
3. Verified to pass in the automatic test suite
