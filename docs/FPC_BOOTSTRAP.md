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

## Build Command

### sysutils.pp (16 errors as of 2025-01-22)
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

The `--flatten-only` mode expands `{$I ...}` include directives into a single file while keeping compiler directives (`{$IFDEF}`, `{$DEFINE}`, etc.) intact. This is useful for c-vise reduction because:

1. **Single file**: Easier to reduce than multiple include files
2. **Preserves conditionals**: FPC can still verify the reduced file compiles correctly
3. **Keeps `uses` clauses**: External units are NOT inlined - you still need `-I` paths

**Important**: Flattening only expands `{$I ...}` includes. The `uses` clause still references external units (sysconst, baseunix, unix, etc.) that must be resolved via `-I` paths when compiling.

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
```

### Compile the flattened file (still needs -I paths for units)
```bash
./build/KGPC/kgpc sysutils_flat.pp /tmp/sysutils.s --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/packages/rtl-objpas/src/inc
```

### Interestingness test and cvise (example)
Create an interestingness script that:
1. Verifies the reduced file still compiles with FPC (to avoid invalid reductions)
2. Checks that kgpc still produces the target error

Both FPC and kgpc need `-I` paths to resolve units from the `uses` clause.

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

### Error categories (16 total as of 2025-01-22):
| Count | Error | Root Cause |
|-------|-------|------------|
| 6 | TGUIDHelper.Create argument type mismatch | Type casts in type helper |
| 4 | procedure overload not found (InitExceptions, etc.) | Forward reference issues |
| 3 | ShortString S assignment | Type compatibility |
| 2 | SysBeep undeclared | Forward reference support needed |
| 1 | OnBeep assignment type mismatch | Forward reference |
| 1 | Result real type mismatch | Cascading |

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

- `sysutils.pp` - **16 errors** (with `--no-stdlib`, as of 2025-01-22)
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
