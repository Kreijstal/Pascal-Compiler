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

### classes.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/classes.pp /tmp/classes.s \
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

### types.pp (parse error - generic class procedures)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/types.pp /tmp/types.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### math.pp (blocked by types.pp)

types.pp now parses past the `TRectF.PlaceInto` qualified-default-parameter
issue but fails on `generic class procedure TBitConverter.From<T>(...)` —
a generic class procedure syntax the parser does not yet support.

#### Next blocker: generic class procedures in types.pp

```
generic class procedure TBitConverter.From<T>(const ASrcValue: T; var ADestination: Array of Byte; AOffset: Integer = 0);
```

The parser does not yet support the `generic class procedure` combined
modifier sequence with inline generic type parameters.

## Units with Compilation Errors

- `baseunix.pp` - **0 errors**
- `sysutils.pp` - **0 errors** (with `--no-stdlib`)
- `classes.pp` - **0 errors**
- `types.pp` - **parse error** (generic class procedures unsupported)
- `math.pp` - **blocked** by types.pp
- `fgl.pp` - **0 errors**
- `sysconst.pp` - **0 errors**
- `rtlconsts.pp` - **0 errors**
- `typinfo.pp` - **0 errors**

## Meson Test Suite

Failing compiler invocations dropped from **58 to 7** after fixing:
1. `codegen_sizeof_type_tag` missing `BYTE_TYPE` (1 byte), `WORD_TYPE` (2 bytes), `LONGWORD_TYPE` (4 bytes), `QWORD_TYPE` (8 bytes)
2. `PASCAL_T_NONE` empty statements reaching `convert_statement` and hitting the unsupported default case
3. Plain record properties (Delphi advanced records) triggering `record_type_is_class` heuristic

Additionally, `kgpc_type_sizeof` was missing cases for `BYTE_TYPE`, `WORD_TYPE`, `LONGWORD_TYPE`, and `QWORD_TYPE`, causing SizeOf to fail for arrays of these types (e.g., `array[0..3] of Byte`).

All 3 Pos(char, string) test failures fixed by:
1. Removing Pos from `builtin_arg_expects_string()` (semcheck already dispatches to typed overloads)
2. Adding `mangled_call_expects_char()` to suppress spurious char-to-string promotion
3. Swapping `_ca`/`_cs` runtime signatures to consistent `(ch, value)` argument order

The `**` (power/dot-product) operator is now supported in the expression parser,
unblocking types.pp up to the qualified-default-parameter issue.

Qualified identifiers in case labels (`THorzRectAlign.Left:`) now parse and
resolve correctly. Fixed by using `pascal_qualified_identifier()` in the
case expression parser and adding dot-split resolution in `semcheck_varid()`
for scoped enum values and unit-qualified constants.

## Error Reduction with C-Vise (Flatten-Only Preprocessor)

When minimizing failures, use the preprocessor's `--flatten-only` mode to expand `{$i ...}` includes into a single file while keeping compiler directives intact for FPC to evaluate. This avoids corrupting conditional branches during reduction.

### Flatten a unit
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

## FPC RTL Build Order (from make -n)

The FPC RTL builds from `FPCSource/rtl/linux/` with these flags:
```
ppcx64 -Fi../inc -Fi../x86_64 -Fi../unix -Fix86_64 -FE. -FU../../rtl/units/x86_64-linux
```

Build order (relevant units):
1. system.pp
2. unixtype.pp
3. ctypes.pp
4. baseunix.pp
5. objpas.pp
6. sysconst.pp
7. unix.pp
8. sysutils.pp (with `-Fi../objpas/sysutils`)
9. math.pp
10. types.pp
