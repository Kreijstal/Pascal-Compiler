# FPC Bootstrap Compatibility Gaps

This document tracks the gaps discovered when attempting to compile Free Pascal Compiler (FPC) source code with KGPC.

## Investigation Summary

The goal is to bootstrap the Free Pascal Compiler using KGPC. The FPC source code is available at: https://github.com/fpc/FPCSource.git

The first file in the bootstrap process is `system.pp` from the RTL (Run-Time Library).

## Discovered Gaps

### 1. Unit File Extension (.pas vs .p)

**Status:** FAILING

**Description:** KGPC only searches for unit files with the `.p` extension, while FPC uses `.pas` and `.pp` extensions for unit files.

**Impact:** This is a critical gap for FPC bootstrap, as all FPC RTL units use `.pas` or `.pp` extensions.

**Location in Code:** `KGPC/unit_paths.c`, line 47:
```c
int written = snprintf(candidate, sizeof(candidate), "%s/%s.p", dir, unit_name);
```

**FPC Behavior:** FPC searches for units with `.pas`, `.pp`, and `.p` extensions (in that priority order).

**KGPC Behavior:** KGPC only searches for `.p` extension.

**Fix Required:** Modify `build_candidate_unit_path()` to try multiple extensions in order:
1. First try `.pas`
2. Then try `.pp`
3. Finally try `.p`

**Test Case:** `fpc_unit_exports_program.p` 

This test demonstrates unit export/import functionality, but currently the unit file must be named with `.p` extension to work with KGPC. For FPC compatibility, it should also work with `.pas` extension.

### 2. Unit Interface/Implementation Export/Import

**Status:** WORKING (when using .p extension)

**Description:** KGPC properly exports and imports types, constants, variables, and functions from unit interface sections.

**Note:** This was initially thought to be a gap, but testing revealed it works correctly when the unit file has a `.p` extension.

**Test Case:** `fpc_unit_exports_program.p` (with `fpc_unit_exports.p` unit)

**Example:**
```pascal
unit fpc_unit_exports;

interface

type
  TPoint = record
    x, y: Integer;
  end;

const
  PI = 3.14;

var
  Counter: Integer;

function GetX(const p: TPoint): Integer;

implementation
// ...
end.
```

When used from another program:
```pascal
program FPCUnitExports;

uses fpc_unit_exports;

var
  pt: TPoint;  // Works!
begin
  Counter := 5;  // Works!
  WriteLn('X: ', GetX(pt));  // Works!
end.
```

**KGPC Behavior:** Correctly exports/imports all interface declarations when unit file has `.p` extension.

## Working Features

The following FPC features were tested and work correctly in KGPC:

1. ✅ Basic units (can compile unit files with `.p` extension)
2. ✅ Unit interface/implementation sections
3. ✅ Exporting types, constants, variables from units
4. ✅ `constref` parameters
5. ✅ `{$I filename}` include directives
6. ✅ `public name` directive
7. ✅ `external name` directive
8. ✅ `alias` attribute (e.g., `[public, alias: 'name']`)
9. ✅ Inline assembly (`asm ... end`)
10. ✅ `array of const` parameters
11. ✅ `absolute` variable directive
12. ✅ `threadvar` declarations
13. ✅ `packed` records

## Next Steps

To continue the FPC bootstrap effort, the unit file extension handling needs to be fixed:

1. Modify `build_candidate_unit_path()` to try `.pas`, `.pp`, and `.p` extensions
2. Test with actual FPC RTL units (e.g., `system.pp`)
3. Identify any other FPC-specific features not yet supported

## Test Case Details

**File:** `tests/test_cases/fpc_unit_exports_program.p`

**Unit:** `tests/test_cases/fpc_unit_exports.p`

**Expected Output:** `tests/test_cases/fpc_unit_exports_program.expected`

**Status:** PASSING with KGPC (when unit has `.p` extension), PASSING with FPC (when unit has `.pas` extension)

This test demonstrates that unit export/import works correctly in KGPC.

## FPC RTL Structure

The FPC RTL (Run-Time Library) consists of:
- `system.pp` - Core system unit (always implicitly used)
- Various `.pas` and `.pp` units in `rtl/linux/`, `rtl/inc/`, etc.
- Include files (`.inc`) used within units

The `.pp` extension is commonly used for "Pascal Program/Pascal Package" files in FPC.
