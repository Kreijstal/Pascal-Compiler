# Changelog

All notable changes to KGPC will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows a `0.0.x` cadence until the FPC self-host milestone
(see [`STATUS.md`](STATUS.md)).  Until then, every release is an alpha and
should be treated as such — language coverage, codegen, and the RTL are all
under active development.

## [Unreleased]

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
- `GpcType` first-class type system unifies semantic checking and codegen.
- Per-unit symbol-table scoping via a parent-pointer scope tree
  (`KGPC/Parser/SemanticCheck/SymTab/`).
- Extended type support: 80-bit `Extended` (x87 ABI), records, classes
  with VMT, generics, sets, type helpers, interfaces.

### Compiler — code generator
- Chaitin-style graph-coloring register allocator (toggle via Meson option
  `use_graph_coloring_allocator`).
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
  [`docs/FPC_BOOTSTRAP.md`](docs/FPC_BOOTSTRAP.md) and
  [`STATUS.md`](STATUS.md).

### Tests and CI
- Test auto-discovery: any `tests/test_cases/foo.p` paired with `foo.expected`
  becomes a test automatically (see
  [`tests/README_TEST_AUTODISCOVERY.md`](tests/README_TEST_AUTODISCOVERY.md)).
- Over 1000 compiler tests; FPC RTL mode adds a second matrix when enabled
  via `-Drun_fpc_rtl_tests=true`.
- CI runs Linux, MSYS2 (MSYS / MINGW64 / UCRT64 / CLANG64), and a
  cross-compile matrix to MinGW.
- Test harness classifies POSIX-only and SysV-ABI-only tests via named sets
  so Cygwin and MSYS keep coverage that pure MinGW / Wine cannot run.

### Repo hygiene
- 23 stale build directories removed from the source tree.
- `.gitignore` revamped to cover every supported build-dir naming and the
  agent-state directory.
- `KGPC/README.txt` rewritten from the original 14-line stub.
