# FPC Bootstrap Investigation

## Overview

This document summarizes the investigation into using KGPC (Kreijstal Gwinn Pascal Compiler) to bootstrap the Free Pascal Compiler (FPC).

**Goal:** Compile FPC's Run-Time Library (RTL), starting with `system.pp`, using KGPC instead of requiring a pre-existing Pascal compiler.

**FPC Source:** https://github.com/fpc/FPCSource.git

## Summary of Findings

### ✅ What Works

KGPC successfully supports many FPC features:

1. **Unit System**
   - Unit compilation and linking
   - Interface/implementation sections
   - Exporting types, constants, variables, and functions
   - `uses` clause for importing units
   - Initialization/finalization sections

2. **Advanced Parameters**
   - `constref` parameters (pass-by-reference for efficiency)
   - `var` parameters
   - `out` parameters
   - `array of const` (variadic parameters)

3. **Directives and Attributes**
   - `{$I filename}` include directives
   - `public name 'symbol'` for symbol naming
   - `external name 'symbol'` for external declarations
   - `[public, alias: 'name']` attributes
   - `absolute` variable aliasing
   - `threadvar` declarations
   - `packed` records

4. **Language Features**
   - Inline assembly (`asm ... end`)
   - Forward declarations
   - Nested procedures/functions
   - Complex type system (records, arrays, enums, sets, pointers, etc.)

### ❌ Critical Gap: Unit File Extensions

**Issue:** KGPC only searches for unit files with `.p` extension, while FPC uses `.pas` and `.pp` extensions.

**Impact:** Cannot compile FPC RTL units as-is, since they all use `.pas` or `.pp` extensions.

**Location:** `KGPC/unit_paths.c`, function `build_candidate_unit_path()`, line 47:
```c
int written = snprintf(candidate, sizeof(candidate), "%s/%s.p", dir, unit_name);
```

**Solution Required:** Modify the function to try multiple extensions in priority order:
1. `.pas` (Free Pascal standard)
2. `.pp` (Free Pascal Package/Program)
3. `.p` (KGPC standard)

## Test Cases

### Passing Test: fpc_unit_exports_program.p

**Location:** `tests/test_cases/fpc_unit_exports_program.p`

**Purpose:** Demonstrates that KGPC correctly handles unit export/import when using `.p` extension.

**Unit:** `tests/test_cases/fpc_unit_exports.p`

**Features Tested:**
- Type export/import (record types)
- Constant export/import
- Variable export/import
- Function export/import

**Status:** ✅ PASSING (both FPC and KGPC)

### Failing Test: fpc_pas_extension_test.p

**Location:** `tests/test_cases/fpc_bootstrap_failing/fpc_pas_extension_test.p`

**Purpose:** Demonstrates the `.pas` extension gap.

**Unit:** `tests/test_cases/fpc_bootstrap_failing/fpc_pas_extension_unit.pas` (note `.pas` extension)

**Features Tested:**
- Enum type export/import
- Constant export/import
- Variable export/import
- Function export/import

**Status:** 
- ✅ PASSING with FPC (compiles and runs correctly)
- ❌ FAILING with KGPC (cannot find unit file with `.pas` extension)

**Error with KGPC:**
```
Error on line 6: undefined type TColor
...
```

Because KGPC cannot locate `fpc_pas_extension_unit.pas`.

## Documentation

### Main Documentation
- **FPC_BOOTSTRAP_INVESTIGATION.md** (this file) - Overall summary
- **tests/test_cases/FPC_BOOTSTRAP_GAPS.md** - Detailed gap analysis

### Test Documentation
- **tests/test_cases/fpc_bootstrap_failing/README.md** - Failing test documentation

## Next Steps

To enable FPC bootstrap:

1. **Immediate Fix:** Implement multi-extension unit file search
   - Modify `KGPC/unit_paths.c:build_candidate_unit_path()`
   - Try `.pas`, `.pp`, and `.p` extensions in order
   - Test with actual FPC RTL units

2. **Iterative Testing:**
   - Start with simple FPC units
   - Progressively test more complex RTL units
   - Document any additional gaps discovered

3. **System Unit Bootstrap:**
   - Attempt to compile `rtl/linux/system.pp`
   - Identify any missing features
   - Create minimal test cases for each gap

## FPC RTL Structure

The FPC Run-Time Library is organized as:

```
FPCSource/
├── rtl/
│   ├── linux/           # Linux-specific RTL
│   │   └── system.pp    # Core system unit
│   ├── inc/             # Platform-independent includes
│   │   ├── system.inc
│   │   ├── *.inc
│   │   └── ...
│   └── ...
```

Key files for bootstrap:
- `system.pp` - Core system unit (always implicitly used)
- `*.inc` files - Included implementation files
- Platform-specific units in `rtl/linux/`, `rtl/win/`, etc.

## Testing Methodology

All tests were validated using:

**FPC Version:** 3.2.2+dfsg-32 [2024/01/05] for x86_64

**KGPC Build:**
```bash
meson setup builddir --buildtype=release
meson compile -C builddir
```

**Test Execution:**
```bash
# With FPC
fpc unit_file.pas
fpc program_file.p
./program_file

# With KGPC
./builddir/KGPC/kgpc program_file.p output.s
gcc -no-pie -o program_file output.s builddir/KGPC/libkgpc_runtime.a -lm
./program_file
```

## Conclusion

KGPC has strong FPC compatibility for core language features. The primary barrier to FPC bootstrap is the unit file extension handling, which is a straightforward fix in the unit search mechanism.

Once this gap is addressed, KGPC will be able to process FPC RTL source files directly, enabling a true FPC bootstrap without requiring a pre-existing Pascal compiler.
