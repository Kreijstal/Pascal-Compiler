# Changelog

All notable changes to KGPC will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows a `0.0.x` cadence until the FPC self-host milestone
(see [`STATUS.md`](STATUS.md)).  Until then, every release is an alpha and
should be treated as such — language coverage, codegen, and the RTL are all
under active development.

## [Unreleased]

## [0.0.3] — 2026-06-03

Native Windows self-host fixpoint.  KGPC's Windows build of the FPC
compiler (`pp_win.exe`) now compiles `pp.pas` into a working `pp_win2`,
which goes on to reproduce a byte-identical stage4 == stage5 fixpoint —
the Win64 counterpart of the Linux self-host milestone.

### Compiler — code generator
- Load a function result *after* managed-local cleanup on `EXIT`, so the
  return value is no longer clobbered by finalization of local strings /
  dynamic arrays.
- Promote `Single` record-field reads to `double` consistently, matching
  the promotion already applied to plain `Single` variables.
- Honor `{$MAXSTACKSIZE}` / `{$MINSTACKSIZE}` on Win64: emit
  `-Wl,--stack,<reserve>` (reserve = the larger of the two). A Win64
  thread's stack cannot grow past the PE-header reserve, so programs that
  request a large stack no longer overflow the linker's default ~2 MB at
  startup.

### Compiler — front end
- Compile the released FPC 3.2.2 sources: assorted codegen, semantic-check
  and parser fixes uncovered by building 3.2.2 rather than trunk.
- Nest `{ ... }` brace comments correctly in the unit/program detector so
  commented-out `unit`/`program` keywords no longer confuse target
  selection.
- Make live-`TypeRef` tracking O(1), fixing O(n²) parse-tree teardown on
  large unit graphs.

### Docs
- `docs/FPC_BOOTSTRAP.md` documents the native Windows self-host fixpoint:
  the build command, the stage1→5 convergence, the `/tmp` unit-cache
  pitfall (stale `.o` masquerading as live codegen bugs), and the
  gdb-on-Windows debugging notes.

### Tests
- The AST-cache include-path regression now runs as a meson test.
- Run the `pp.pas` bootstrap stage-2 self-host on Win64 as well as Linux:
  `pp_bootstrap` builds the matching RTL from source (`rtl/win64` →
  `rtl/units/x86_64-win64`) and recompiles `pp.pas` into `pp_stage2`.

## [0.0.2] — 2026-06-03

Windows (PE-COFF) FPC-RTL-from-source bootstrap.  KGPC builds the FPC
runtime and compiler from source on Win64, and the native `pp_win.exe`
links and runs.

### Compiler — code generator
- Fix runtime set arithmetic membership for ordinals ≥ 32: set
  union/intersection/difference no longer truncates the bit-test to 32
  bits, so `e in (setA + [e44])` is correct for high-ordinal elements.
- Emit COFF-compatible sections, AT&T-syntax branches, and the correct
  `Initialize`/`Finalize` symbol casing for the Win64 target.
- Emit the `INITFINAL` table header as two native words (not a single
  4-byte field), matching FPC's `TInitFinalTable` layout so
  `FPC_FINALIZEUNITS` reads a valid count at exit.
- Size `SmallInt`/`ShortInt` record fields, and scalar subrange aliases,
  by their real storage width, fixing PACKRECORDS-C record strides.
- Load by-reference narrow ordinal params from the value type, not the
  slot, so `var shortint` reads sign-extend instead of widening garbage.
- Evaluate `sizeof()` inside inline array-variable bounds, so
  `array[0..sizeof(T)-1] of byte` gets its real length.
- Compare dynamic char arrays to `nil` as a pointer test rather than
  routing through string compare.
- Build `string[N]` type aliases as `ShortString`, store the length byte
  for ShortString elements of typed-const arrays, and materialize
  `string(PChar)` casts as managed strings — fixing length-byte clobbers.
- Widen nested-function class/pointer return size; convert fixed
  `WideChar` arrays to `UnicodeString` on assignment; emit AnsiString ↔
  UnicodeString and PChar / PWideChar conversion helpers.
- Resolve overloaded method calls by argument list (not name alone) and
  load typecast call targets with a full 64-bit `movq` (fixes pointer
  truncation and overloaded-method misdispatch — #749).

### Runtime / build
- Detect MSYS via compiler predefines (`KGPC_WIN64_ABI`) and select the
  Win64 ABI struct layout there; `GetHostName` falls back to POSIX
  `gethostname` without `_WIN32`.
- Target-dependent `FileRec`/`TextRec` sizes, QWord `Handle` on Win64, and
  member-based record alignment; wire up the FPC entry-info globals
  (`_FPC_SysInstance`, `_FPC_TlsKey`, resource-string tables) for native
  Windows programs.
- Self-host the RTL build in the `pp.pas` bootstrap CI test (no host fpc).

### Front end / semantic check
- Key the AST cache on the target ABI; resolve a routine's parameter types
  in its declaring unit; let RTL declarations override KGPC's builtin
  const defaults; don't collapse a `try..except` with an empty handler.

## [0.0.1] — 2026-05-26

First tagged alpha.  This release is the starting point from which further
changes will be tracked; previous progress lives in the git history.

### Added
- `kgpc --version` / `-v` flag prints the compiler version.
- `kgpc --help` / `-h` flag documents the supported options.
- Top-level `docs/ARCHITECTURE.md` describing the pipeline and source layout.
- `STATUS.md` listing FPC bootstrap stages, what works, and the known KGPC
  bugs blocking the next stage.
- `CHANGELOG.md`, `CONTRIBUTING.md`, `SECURITY.md`.
- Project version exposed via the Meson build (`meson introspect`).

### Compiler — language and front end
- Parser-combinator front end (`cparser/`) is the default; the legacy
  lex/yacc front end is retired.
- `KgpcType` first-class type system unifies semantic checking and codegen.
- Per-unit symbol-table scoping via a parent-pointer scope tree
  (`KGPC/Parser/SemanticCheck/SymTab/`).
- Extended type support: 80-bit `Extended` (x87 ABI), records, classes
  with VMT, generics, sets, type helpers, interfaces.

### Compiler — code generator
- Chaitin-style graph-coloring register allocator, on by default
  (`-Duse_graph_coloring_allocator=false` falls back to the simple LRU spiller).
- Dead-code elimination over the program subprogram graph
  (`--disable-dce` to opt out).
- Dual ABI support: System V AMD64 (default on Linux / macOS) and the
  Windows x64 ABI (`--target=windows`, default under `_WIN32` / `__CYGWIN__`).
- Constructor-result temp lifetime tracking with deterministic cleanup at
  scope exit.
- Recursive cleanup for managed dynamic-array elements at scope exit and at
  program exit.
- VMT-based virtual dispatch that matches by parameter signature, not just
  by name and arity, to handle FPC-style classref-overridden constructors.

### Runtime
- `KGPC/runtime_*.c` split per concern (baseunix, unix, fpc_init,
  fpc_assign, string, gmp, olevariant, widechar).
- AnsiString / RawByteString / UnicodeString interop, including FPC RTL
  passing-by-reference conventions.
- POSIX runtime layer (`runtime_baseunix.c`) with Windows stubs that return
  `-1` / `ENOSYS` so the same RTL units link on both platforms.
- `fpsigaction` wrapper with binary-compatible `sigactionrec` layout.
- ShortString / AnsiString / char promotion paths covering the
  `string := char` and `string := chr(ord)` cases that FPC accepts.
- GMP integration is optional (Meson `with_gmp=feature`).

### FPC RTL compatibility
- Substantial subset of `system.pp`, `sysutils.pp`, `classes.pp`,
  `dateutils.pp`, `math.pp`, `baseunix.pp`, `unix.pp`, `typinfo.pp`, `fgl.pp`,
  `keyboard.pp`, `crt.pp`, `objpas.pp`, and friends compiles against the
  KGPC-shipped Units or against the real FPC RTL under `FPCSource/`.
- **FPC self-host (Stages 1–3) verified.**  KGPC compiles `pp.pas` into
  `pp_bootstrap`; `pp_bootstrap` recompiles `pp.pas` into `pp_stage2`;
  `pp_stage2` builds and runs user programs end-to-end.  Exercised on
  every push by the `test_fpcrtl_pp_pas_bootstrap` CI test.  See
  [`docs/FPC_BOOTSTRAP.md`](https://github.com/Kreijstal/Pascal-Compiler/blob/master/docs/FPC_BOOTSTRAP.md)
  and
  [`STATUS.md`](https://github.com/Kreijstal/Pascal-Compiler/blob/master/STATUS.md).

### Tests and CI
- Test auto-discovery: any `tests/test_cases/foo.p` paired with `foo.expected`
  becomes a test automatically (see
  [`tests/README_TEST_AUTODISCOVERY.md`](https://github.com/Kreijstal/Pascal-Compiler/blob/master/tests/README_TEST_AUTODISCOVERY.md)).
- Over 1000 compiler tests; FPC RTL mode adds a second matrix when enabled
  via `-Drun_fpc_rtl_tests=true`.
- CI runs Linux, MSYS2 (MSYS / MINGW64 / UCRT64 / CLANG64), and a
  cross-compile matrix to MinGW.
- Test harness classifies POSIX-only and SysV-ABI-only tests via named sets
  so Cygwin and MSYS keep coverage that pure MinGW / Wine cannot run.

### Repo hygiene
- Dead source files removed: `KGPC/main.c` (superseded by `main_cparser.c`),
  `KGPC/harness.c` (zero references), `btpc.dpr` (tracked test scratch).
- Unused `KGPC/TestPrograms/` tree removed (~130 hand-written `.p` files of
  which only two were referenced by the test harness; those two moved to
  `tests/test_cases/legacy_sign_test.p` and `legacy_for_loop.p`).
- Three superseded planning docs removed (`docs/UNIT_SCOPING_PLAN.md`,
  `docs/SCOPE_TREE_REFACTORING.md`, `docs/EXTENDED_TYPE_SUPPORT.md`).
- `.gitignore` tightened so the root no longer hides release docs, and the
  agent-state directory (`.claude/`) is excluded.
- `KGPC/README.txt` rewritten from the original 14-line stub into a
  source-tree and flag reference.
