# FPC Bootstrap Analysis

## Status: BLOCKED - Compiler Bugs/Limitations

The FPC RTL cannot be fully compiled because kgpc has bugs and missing features that need to be fixed.

## Known Issues to Fix

1. **Type Helper Issues**
   - IndexOfAny/IndexOfAnyUnQuoted overload resolution in type helpers
   - TGUIDHelper.Create type mismatch with type casts

2. **Forward References**
   - SysBeep used before declaration (forward reference support needed)
   - Some procedure overloads not found due to forward reference issues

3. **Overload Resolution**
   - Some procedure overloads not matched (InitInternational, InitExceptions, etc.)
   - Pipe functions (IOPipe, FlushPipe, ClosePipe, PCloseText) not finding overloads

4. **String/Array Type Compatibility**
   - ShortString/array[0..255] of char assignment errors
   - strlen ambiguous call errors

5. **Other Issues**
   - DoCapSizeInt type mismatch errors
   - GetTickCount result type mismatch (pointer vs procedure)
   - LowerCase function missing return statement
   - fpsignal assignment type mismatch

## Build Command

### sysutils.pp (38 errors as of 2025-01-24)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/sysutils.pp /tmp/sysutils.s \
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

## Error Reduction with C-Vise (Flatten-Only Preprocessor)

When minimizing sysutils failures, use the preprocessor's `--flatten-only` mode to expand `{$i ...}` includes into a single file while keeping compiler directives intact for FPC to evaluate. This avoids corrupting conditional branches during reduction.

### Flatten sysutils.pp
```bash
./build/kgpc-preprocess --flatten-only \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64 \
  ./FPCSource/rtl/unix/sysutils.pp sysutils_flat.pp
cp -f sysutils_flat.pp sysutils_indexofany.pp
```

### Interestingness test and cvise (example)
Create a small interestingness script locally (example below), then run cvise against the flattened file.

```bash
cat > /tmp/cvise_indexofany.sh <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

input="sysutils_indexofany.pp"
root="/home/kreijstal/git/Pascal-Compiler"

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
cp -f "$input" "$tmpdir/sysutils.pp"

(
  cd "$tmpdir"
  fpc \
    "-Fi$root/FPCSource/rtl/inc" \
    "-Fi$root/FPCSource/rtl/unix" \
    "-Fi$root/FPCSource/rtl/linux" \
    "-Fi$root/FPCSource/rtl/linux/x86_64" \
    "-Fi$root/FPCSource/rtl/objpas/sysutils" \
    "-Fi$root/FPCSource/rtl/x86_64" \
    "-Fu$root/FPCSource/rtl/unix" \
    "-Fu$root/FPCSource/rtl/objpas" \
    "-Fu$root/FPCSource/rtl/objpas/sysutils" \
    "-Fu$root/FPCSource/rtl/inc" \
    "-Fu$root/FPCSource/rtl/linux" \
    "-Fu$root/FPCSource/rtl/linux/x86_64" \
    "-Fu$root/FPCSource/rtl/x86_64" \
    "-Fu$root/FPCSource/packages/rtl-objpas/src/inc" \
    sysutils.pp >/dev/null 2>&1 || exit 1
)

output="$("$root/build/KGPC/kgpc" "$tmpdir/sysutils.pp" /tmp/sysutils_indexofany.s \
  --no-stdlib \
  "-I$root/FPCSource/rtl/unix" \
  "-I$root/FPCSource/rtl/objpas" \
  "-I$root/FPCSource/rtl/objpas/sysutils" \
  "-I$root/FPCSource/rtl/inc" \
  "-I$root/FPCSource/rtl/linux" \
  "-I$root/FPCSource/rtl/linux/x86_64" \
  "-I$root/FPCSource/rtl/x86_64" \
  "-I$root/FPCSource/packages/rtl-objpas/src/inc" 2>&1 || true)"

if echo "$output" | rg -q "IndexOfAnyUnQuoted does not match any available overload|IndexOfAny does not match any available overload|GetAnsiString, not enough arguments"; then
  exit 0
fi
exit 1
EOF
chmod +x /tmp/cvise_indexofany.sh

cvise --timeout 7200 /tmp/cvise_indexofany.sh sysutils_indexofany.pp
```

### Error categories (38 total as of 2025-01-24):
| Count | Error | Root Cause |
|-------|-------|------------|
| 6 | DoCapSizeInt type mismatch | Function call type mismatch |
| 6 | Pipe functions overload not found | Overload resolution issues |
| 5 | PCloseText overload not found | Forward reference/overload issues |
| 4 | incompatible types in assignment | Type compatibility issues |
| 3 | ShortString S assignment | Cascading from earlier errors |
| 3 | procedure overload not found (InitExceptions, etc.) | Forward reference issues |
| 2 | SysBeep/OnBeep undeclared | Forward reference support needed |
| 2 | GetTickCount result type mismatch | pointer vs procedure |
| 2 | strlen ambiguous call | Overload resolution |
| 1 | LowerCase no return statement | Missing return |
| 1 | fpsignal type mismatch | Type compatibility |
| 1 | Result real type mismatch | Cascading |
| 1 | Result char/string mismatch | Type compatibility |
| 1 | expression char/^char mismatch | Type compatibility |

## Compiles Successfully (RTL Units)

- `system.pp` - Core system unit
- `linux.pp` - Linux unit
- `unix.pp` - Unix unit
- `baseunix.pp` - Base Unix functions
- `objpas.pp` - Object Pascal RTL
- `strings.pp` - String handling
- `ctypes.pp` - C types
- `sysconst.pp` - System constants
- `types.pp` - Type helpers
- `cpu.pp` - CPU types (x86_64)
- `errors.pp` - Unix errors
- `rtlconsts.pp` - RTL constants
- `dl.pp` - Dynamic loading
- `fpintres.pp`, `si_prc.pp`, `si_c.pp`, `si_g.pp`, `si_dll.pp` - Startup units

## Units with Compilation Errors

- `sysutils.pp` - **38 errors** (with `--no-stdlib`, as of 2025-01-24)
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

## Recent Fixes (2025-01-24)

1. **Buffer overflow in MangleNameFromTypeList** - Fixed buffer size from 4 to 6 chars per suffix
2. **Absolute variable aliasing** - Added PASCAL_T_ABSOLUTE_CLAUSE type and fixed AST handling
3. **ABSOLUTE_CLAUSE expression errors** - Fixed var declaration conversion to skip absolute clause nodes
4. **Local set constant codegen for character sets** - Fixed 32-byte character sets in function-local constants generating `$0` instead of actual set value. Root cause: `const_int_value` only valid for sets ≤8 bytes. Fix: emit large sets to `.rodata` section and use label reference.
