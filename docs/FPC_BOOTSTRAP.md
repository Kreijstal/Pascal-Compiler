# FPC Bootstrap Analysis

## Status: pp.pas compiles with 5 semantic errors; next blockers are undefined constants and DirectorySeparator

## Prerequisites

Clone the FPC source code:
```bash
git clone https://github.com/fpc/FPCSource
```

## Build Commands

### Recommended (match make -n order)
```bash
make -n -B -C ./FPCSource/rtl/linux units
```

Then compile each unit in that order with KGPC using `--no-stdlib` and the include
paths listed below. This matches the FPC RTL bootstrap sequence and avoids
ordering issues.

### system.pp (0 errors, 158 warnings)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/linux/system.pp /tmp/system.s \
  --no-stdlib \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64
```

### fpintres.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/inc/fpintres.pp /tmp/fpintres.s \
  --no-stdlib \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/x86_64 \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/classes \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64
```

### unixtype.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/unixtype.pp /tmp/unixtype.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### ctypes.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/inc/ctypes.pp /tmp/ctypes.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

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

### objpas.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/objpas.pp /tmp/objpas.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### sysconst.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/sysconst.pp /tmp/sysconst.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/objpas/sysutils \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### unix.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/unix/unix.pp /tmp/unix.s \
  --no-stdlib \
  -I./FPCSource/rtl/unix \
  -I./FPCSource/rtl/objpas \
  -I./FPCSource/rtl/inc \
  -I./FPCSource/rtl/linux \
  -I./FPCSource/rtl/linux/x86_64 \
  -I./FPCSource/rtl/x86_64
```

### sysutils.pp (current blocker)
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

### types.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/types.pp /tmp/types.s \
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

### math.pp (0 errors)
```bash
./build/KGPC/kgpc ./FPCSource/rtl/objpas/math.pp /tmp/math.s \
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

### pp.pas (5 errors)
```bash
./build/KGPC/kgpc FPCSource/compiler/pp.pas --no-stdlib \
  -DCPU64 -DCPUX86_64 -DFPC -DLINUX -DUNIX -DFPC_HAS_TYPE_EXTENDED -Sg \
  -IFPCSource/rtl/objpas \
  -IFPCSource/rtl/objpas/sysutils \
  -IFPCSource/rtl/objpas/classes \
  -IFPCSource/rtl/linux \
  -IFPCSource/rtl/unix \
  -IFPCSource/rtl/inc \
  -IFPCSource/rtl/x86_64 \
  -IFPCSource/rtl/linux/x86_64 \
  -IFPCSource/rtl/unix/x86_64 \
  -IFPCSource/compiler \
  -FuFPCSource/rtl/unix \
  -FuFPCSource/rtl/linux \
  -FuFPCSource/rtl/objpas \
  -FuFPCSource/rtl/inc \
  -FuFPCSource/rtl/objpas/sysutils \
  -FuFPCSource/rtl/objpas/classes \
  -FuFPCSource/compiler
```

Remaining errors:
1. `constant first_int_imreg is undefined or not a const` — CPU-specific constant
2. `constant tmpmaxcpufpuintreg is undefined or not a const` — CPU-specific constant
3. `constant DirectorySeparator is undefined or not a string const` (×2) — sysutils typed constant not recognized
4. `WITH context must be a record or pointer to a record` — WITH on unresolved type

## Remaining Blockers

## Flags

- `--no-stdlib` loads the minimal KGPC prelude and then compiles the FPC RTL
  unit directly (required for `system.pp` and the bootstrap sequence so FPC’s
  `system.pp` is used instead of the KGPC stdlib).
  - When compiling RTL units, always include `-I./FPCSource/rtl/linux/x86_64`
    so `stat.inc` and other arch-specific includes resolve.
  - For `sysutils.pp`, include `-I./FPCSource/rtl/objpas/sysutils` to resolve
    `sysutilh.inc` and other sysutils include files.

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

Generic class procedures and functions inside advanced records (`generic class
procedure Foo<T>(...)`) now parse correctly by adding `optional(generic)` and
`create_method_type_param_list()` to the record member parsers.

The `specialize` expression now supports multiple comma-separated arguments
(`TFoo.specialize A<T>(X, Dest)`) instead of just a single argument, unblocking
the TBitConverter methods in types.pp.

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
2. fpintres.pp
3. si_prc.pp
4. si_c.pp
5. si_g.pp
6. si_dll.pp
7. uuchar.pp
8. unixtype.pp
9. ctypes.pp
10. baseunix.pp
11. strings.pp
12. objpas.pp
13. sysconst.pp
14. unixutil.pp
15. syscall.pp
16. unix.pp
17. errors.pp
18. initc.pp
19. linux.pp
20. sysutils.pp (with `-I./FPCSource/rtl/objpas/sysutils`)
