# FPC Bootstrap Analysis

## Status: sysutils.pp compiles with 0 errors

## Prerequisites

Clone the FPC source code:
```bash
git clone https://github.com/fpc/FPCSource
```

## Build Commands

### baseunix.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/baseunix.pp /tmp/baseunix.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### sysutils.pp (0 errors)
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
Do not use the `--timeout` flag with `cvise`.

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

## Units with Compilation Errors

- `baseunix.pp` - **0 errors**
- `sysutils.pp` - **0 errors** (with `--no-stdlib`)
- `math.pp` - Depends on sysutils
