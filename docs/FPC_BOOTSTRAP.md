# FPC Bootstrap Analysis

## Status: BLOCKED - Compiler Bugs/Limitations

The FPC RTL cannot be fully compiled because kgpc has bugs and missing features that need to be fixed.

## Previously Documented Features (Now Working)

- Pointer indexing (`p[i]`)
- Pointer +/- integer arithmetic
- Pointer - pointer subtraction
- Exit(value)
- Set constants (set of char, ranges)
- ShortString type

## Fixed Regressions (2026-01-15)

A regression was identified and fixed in commit fe74623 ("Fixing the pascal parser") that caused 
`baseunix.pp`, `unix.pp`, and `linux.pp` to fail compilation. The fix reverts the problematic 
parser changes:

- Reverted speculative assignment parsing in `pascal_statement.c`
- Reverted lvalue typecast changes that caused different parsing paths
- Removed associated tests that were testing broken functionality

### Bisect Information (for reference)
- **Last Good Commit:** 1a807b8 (Jan 7, 2026) - "tests from mistral (#368)"
- **First Bad Squash:** 5687905 (Jan 10, 2026) - "Gaps (#374)" - contains multiple squashed PRs  
- **Actual Bad Commit:** fe74623 (Jan 9, 2026) - "Fixing the pascal parser" - within PR #375

## Known Issues to Fix

1. **Type Resolution**
   - Unable to resolve record types for field access
   - Incompatible type assignments (char vs string)
   - Type mismatches in function arguments

2. **Constants**
   - Exception constants not found (SCannotCreateEmptyDir, SFileNotFound, SSeekFailed)
   - CP_* constants missing (CP_ACP, CP_UTF8, CP_NONE)
   - ThreadingAlreadyUsed not found
   - fpc_in_cpu_first not defined
   - **fmAppend** - File mode constant missing

3. **Operators**
   - AND/OR expressions with non-boolean operands
   - Complex expressions in if statements
   - Date/time arithmetic (FileDateToDateTime)

4. **Function Overloads**
   - SetCodePage - wrong overload matched
   - FileDateToDateTime/FileDateToUniversal - type mismatch
   - AllocMem - type mismatch
   - Create constructor for exceptions - not resolved

5. **Preprocessor**
   - Missing FPC preprocessor directives
   - **Changed behavior in 5687905** affecting include file selection

6. **Missing Built-ins (for FPC RTL)**
   - `strlen` - String length for PAnsiChar
   - `flush` - Text file flush procedure

## Build Command

### sysutils.pp (635 errors remaining as of 2025-01-11)
```bash
./builddir/KGPC/kgpc ./FPCSource/rtl/unix/sysutils.pp /tmp/sysutils.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/packages/rtl-objpas/src/inc
```

### Top error categories (635 total):
| Count | Error | Root Cause |
|-------|-------|------------|
| 174 | type mismatch on addop | **BUG**: Int64 return type in type helper overload calls |
| 88 | type mismatch on mulop | Same as above |
| 51 | Result type mismatch (string/unsupported) | Same as above |
| 48 | relational op type mismatch | Same as above |
| 40 | Result type mismatch (unsupported/integer) | Same as above |
| 33 | equality comparison type mismatch | Same as above |
| 24 | Move argument type mismatch | Unknown |
| 20 | Aligned function not found | Missing intrinsic |
| 20 | AND expression type mismatch | Unknown |
| 20 | expected relational inside if statement | Unknown |
| 16 | Result type mismatch (unsupported/pointer) | Int64 bug |
| 15 | Str value must be an integer | Unknown |
| 8 | undeclared identifier "Self" | Type helper bug |
| 8 | record field "Length" not found | Unknown |

### Key Bug: Int64/QWord return type in type helper overload calls

When a type helper method returns `Int64` or `QWord` and calls another overloaded method of the same name, `Result`'s type becomes "unsupported". This causes ~300+ cascading errors because FPC's sysutils uses `SizeInt` (=Int64 on x86_64) extensively.

**Reproduction**:
```pascal
type
  THelper = type helper for AnsiString
    function Foo: Int64;
    function Foo(x: Integer): Int64;
  end;

function THelper.Foo: Int64;
begin
  Result := Foo(0);  // BUG: Result is 'unsupported'
end;
```

Note: The `sysutils.pp` unit requires all the include paths above to resolve architecture-specific includes (stat.inc, strings.inc, etc.).

## Compiles Successfully (RTL Units)

- `system.pp` - Core system unit (dead code eliminated)
- `fpintres.pp` - Resource strings
- `si_prc.pp` - Process startup
- `si_c.pp` - C startup
- `si_g.pp` - GNU startup
- `si_dll.pp` - DLL startup
- `linux.pp` - Linux unit
- `unix.pp` - Unix unit
- `objpas.pp` - Object Pascal RTL
- `strings.pp` - String handling
- `ctypes.pp` - C types
- `sysconst.pp` - System constants
- `baseunix.pp` - Base Unix functions
- `types.pp` - Type helpers
- `cpu.pp` - CPU types (x86_64)
- `errors.pp` - Unix errors
- `rtlconsts.pp` - RTL constants
- `dl.pp` - Dynamic loading

## Units with Compilation Errors
- `sysutils.pp` - **635 errors** (as of 2025-01-11, with `--no-stdlib`):
  - Type mismatches in arithmetic ops (262 errors) - likely type helper related
  - Result type mismatches (122 errors)
  - Relational/comparison type mismatches (101 errors)
  - Missing `Aligned` intrinsic (20 errors)
  - Move argument type issues (24 errors)
  - AND expression type issues (20 errors)
  - Undeclared `Self` in type helpers (8 errors)
- `math.pp` - Depends on sysutils
- `cthreads.pp` - Missing ThreadingAlreadyUsed
- `charset.pp` - Type incompatibilities
- `unixcp.pp` - Missing CP_* constants
- `intrinsics.pp` - Missing fpc_in_cpu_first
- `character.pas` - Needs unicodedata unit
- `getopts.pp` - Missing argv from system
- `ports.pp` - x86-specific, not x86_64
- `cmem.pp` - Needs system unit types
- `si_uc.pp` - Missing si_uc.inc for x86_64
