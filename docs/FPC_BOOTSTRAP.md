# FPC Bootstrap Analysis

## Status: FPC RTL test suite passes; `pp.pas` compiles and links as `pp_bootstrap`

## Prerequisites

Clone the FPC source code:
```bash
git clone https://github.com/fpc/FPCSource
```

Regenerate the compiler message includes before attempting `pp.pas`:
```bash
make -B -C ./FPCSource/compiler msg
```

Without this, a stale `FPCSource/compiler/msgtxt.inc` can be a documentation-text
file instead of the generated Pascal include, which makes `verbose.pas` fail in
preprocessing with a malformed compiler directive error.

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
  -I./FPCSource/rtl/objpas/classes \
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

### pp.pas (direct KGPC invocation)
```bash
./build-fpc/KGPC/kgpc FPCSource/compiler/pp.pas tests/output/pp_bootstrap.s --no-stdlib \
  -DCPU64 -DCPUX86_64 -Dx86_64 -DFPC -DLINUX -DUNIX -DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED -DFPC_BOOTSTRAP_INDIRECT_ENTRY -Sg \
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
  -IFPCSource/compiler/x86 \
  -IFPCSource/compiler/x86_64 \
  -FuFPCSource/rtl/unix \
  -FuFPCSource/rtl/linux \
  -FuFPCSource/rtl/objpas \
  -FuFPCSource/rtl/inc \
  -FuFPCSource/rtl/objpas/sysutils \
  -FuFPCSource/rtl/objpas/classes \
  -FuFPCSource/compiler \
  -FuFPCSource/compiler/x86 \
  -FuFPCSource/compiler/x86_64 \
  -FuFPCSource/compiler/systems
```

Then link the generated assembly with the KGPC runtime:
```bash
cc -O2 -no-pie \
  -o tests/output/pp_bootstrap \
  tests/output/pp_bootstrap.s \
  build-fpc/KGPC/libkgpc_runtime.a
```

The Meson test harness is the preferred way to run the complete bootstrap flow,
because it also ensures `msgtxt.inc` and `msgidx.inc` exist by compiling and
running `compiler/utils/msg2inc.pp` when needed:
```bash
meson setup build-fpc -Drun_fpc_rtl_tests=true
ninja -C build-fpc KGPC/kgpc KGPC/libkgpc_runtime.a
KGPC_FPC_RTL=1 \
KGPC_FPC_RTL_DIR=FPCSource \
KGPC_RUNTIME_LIB="$PWD/build-fpc/KGPC/libkgpc_runtime.a" \
MESON_BUILD_ROOT="$PWD/build-fpc" \
CC=cc \
python3 tests/do_not_run_me_directly_but_through_meson.py \
  TestCompiler.test_fpcrtl_pp_pas_bootstrap
```

This writes:
```text
tests/output/pp_bootstrap.s
tests/output/pp_bootstrap
```

Note: `-Dx86_64` is required (FPC's Makefile passes `-dx86_64` for x86_64 targets).
The x86/x86_64/systems subdirectories match FPC's `-Fux86 -Fix86 -Fux86_64 -Fix86_64 -Fusystems`.
`-DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED` are needed so `systemh.inc` defines `TExtended80Rec`
(used by `math.pp`'s `Frexp`/`Ldexp`). These flags propagate to all unit preprocessing.

`-DFPC_BOOTSTRAP_INDIRECT_ENTRY` makes the system unit declare `operatingsystem_parameter_argc`,
`operatingsystem_parameter_argv`, and `operatingsystem_parameter_envp` as `public name` globals
(emitted as `.comm` by codegen). Without it, they're declared `external name` and expected to come
from ASM startup object files (`si_c.inc` via `{$L}`), which KGPC doesn't support.

### Compile hello world with the generated `pp_bootstrap`

Pass the FPC RTL unit paths explicitly. Do not rely on guessed search paths.
```bash
./tests/output/pp_bootstrap -n \
  -FuFPCSource/rtl/linux \
  -FuFPCSource/rtl/unix \
  -FuFPCSource/rtl/inc \
  -FuFPCSource/rtl/objpas \
  -FuFPCSource/rtl/objpas/sysutils \
  -FuFPCSource/rtl/objpas/classes \
  tests/test_cases/helloworld.p

./tests/output/helloworld
```

Expected output:
```text
Hello, World!
```

The generated compiler also supports a quick startup check:
```bash
./tests/output/pp_bootstrap -h
```

### FPC RTL (56 units)

All 56 FPC RTL units now compile with 0 semantic errors via `meson test -C build-fpc "FPC RTL tests"`.
The build-fpc directory uses `meson setup build-fpc -Drun_fpc_rtl_tests=true`.

### FPC Compiler Units (99 units scanned standalone)

All 99 compiler `.pas` files fail when compiled individually. Total: 257 errors.

Top error categories:
| Category | Count | Root Cause |
|---|---|---|
| Enum/Tconstexprint relational ops | 42 | `Tconstexprint` is a record with operator overloading (not yet supported) |
| Overload resolution | 27 | Parameter type mismatches (`^record` subtype compatibility) |
| Field not found / field access | 27 | Class field resolution, nested types, property getters |
| Assignment type mismatch | 26 | Various type incompatibilities |
| Type mismatch on arithmetic ops | 25 | `Tconstexprint` operator overloading |
| Array literal element mismatch | 11 | Constant array type coercion |
| `^record` vs `procedure` | 11 | Class instances typed as procedure (constructor resolution) |
| String concat method shadowing | 8 | Built-in `Concat` shadows `TAsmList.Concat()` method |
| Char/String mismatch | 7 | Implicit Char↔String conversion |

### pp.pas (full compiler entry point)

Compiling `pp.pas` loads ~200+ units together and produces ~16,000+ errors due to
cascading from the root causes above.

## Performance

### Hash Table Size

`TABLE_SIZE` was increased from 211 to 4099 (prime). With 267 units merged into
a flat scope, the old 211-bucket hash table had severe collisions (~hundreds of
entries per bucket), making every identifier lookup O(n).

### Semantic Analysis of Imported Implementation Bodies

`pp.pas` now completes with full semantic analysis and code generation on the
normal path. Recent profiling on this checkout is approximately:
- parse user units: `3.4s`
- semantic analysis: `130s`
- code generation: `74s`
- total pipeline: `212s`

The main remaining performance cost is full semantic analysis across the merged
compiler unit graph, followed by code generation. Optimization work should
target those hot paths directly rather than relying on a reduced-analysis mode.
### Parser Cache

The AST parser cache (`kgpc_ast_cache_<hash>`) is keyed on the binary hash.
Every recompile invalidates the cache, requiring a full re-parse of all 267
units (~44 seconds uncached vs ~3-5 seconds cached).

## Remaining Blockers (0 errors in pp.pas)

All semantic errors resolved. pp.pas compiles and generates code successfully.

Previous blockers that were resolved:
- TExtended80Rec typecast errors (15) — fixed by passing `-DFPC_HAS_TYPE_EXTENDED -DSUPPORT_EXTENDED`
- Operator overloading, type helper resolution, overload resolution, etc. — all resolved in earlier iterations

### References
- Keep using the FPC-declared order from `make -n -B -C ./FPCSource/rtl/linux units`
  for RTL bootstrap work.
- For compiler bootstrap, `make -n -C ./FPCSource/compiler ppcx64` shows that
  FPC expects the RTL to be prebuilt into `../rtl/units/x86_64-linux` and then
  compiles `pp.pas` in one top-level invocation using:
  - `-Fu../rtl/units/x86_64-linux`
  - `-Fux86_64`
  - `-Fux86`
  - `-Fusystems`

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
