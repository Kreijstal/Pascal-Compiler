# Contributing to KGPC

Thanks for being interested in KGPC.  This document covers the build/test
workflow and a few project conventions.  Read [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md)
first if you haven't — it points to where each kind of change lives.


## Quick build

```
meson setup build
meson compile -C build
meson test    -C build
```

The compiler binary lands at `build/KGPC/kgpc`.  Use `kgpc --help` to see
the current flag set and `kgpc --version` to confirm the build identifies
itself correctly.


## FPC RTL test mode

The default `meson test` runs ~1000 compiler tests against the KGPC-shipped
RTL units under `KGPC/Units/`.  A separate FPC-RTL mode compiles a curated
subset against the real Free Pascal RTL — this is the surface we use to
keep KGPC ABI-compatible with FPC.

```
meson setup build-fpc -Drun_fpc_rtl_tests=true
meson test -C build-fpc "FPC RTL tests"
```

You need an `FPCSource/` clone in the repo root (or point at it via
`-Dfpc_rtl_dir=...`).  The FPC RTL tests are *more important* than the
KGPC-only tests when changing codegen or the runtime; run them before
sending a PR.

**Do not** clear `pp-cache` manually — meson test manages it.
**Do not** run `meson compile` before `meson test` — meson test builds
automatically.


## Adding a test

The harness auto-discovers anything under `tests/test_cases/`:

1. Write `tests/test_cases/my_repro.p`.
2. Write `tests/test_cases/my_repro.expected` containing the exact stdout
   the program should produce.
3. (Optional) Add `tests/test_cases/my_repro.input` if the program reads
   stdin.
4. Run `meson test -C build` — your test is now discovered automatically.

For platform-specific cases see the `POSIX_ONLY_TESTS` /
`SYSV_ABI_ONLY_TESTS` sets in
[`tests/harness/auto_discovery.py`](tests/harness/auto_discovery.py).
Don't add per-test `if test_base_name == "..."` branches.


## Where to put things

| Change                                | Where                                                                      |
|---------------------------------------|-----------------------------------------------------------------------------|
| New language construct                | `KGPC/Parser/ParseTree/from_cparser_parts/`, then `KGPC/Parser/SemanticCheck/SemChecks/` |
| New built-in routine                  | `SemChecks/SemCheck_Expr_Builtins.c` and `CodeGenerator/.../codegen_builtins.c` |
| Calling-convention edit               | `CodeGenerator/Intel_x86-64/codegen_subprograms.c`, `abi_constants.h`       |
| Register-spill bug                    | `CodeGenerator/Intel_x86-64/graph_coloring_allocator.c`                     |
| RTL routine                           | one of the `KGPC/runtime_*.c` files (split by concern)                      |
| Pascal-level RTL unit                 | `KGPC/Units/`                                                               |
| New compiler flag                     | `KGPC/main_cparser.c` (`set_flags`) and `KGPC/flags.c/.h`                   |


## Code conventions

- **No workarounds or hardcoded special cases.**  If a fix takes the form
  `if (strcmp(name, "Foo") == 0)`, it is not a fix.  Find the structural
  reason and address that instead.
- **Don't modify test files to make the compiler pass.**  If a `.p` file
  used to compile and no longer does, the bug is in the compiler.
- **Keep debug output temporary.**  Don't commit `printf`-debug calls.
- **Commit messages: `topic: change`.**  Imperative, lowercase, with a
  meaningful body when the change has subtlety.  Avoid "Phase X" /
  "Stream Y" framing — those don't survive outside the contributor's
  short-term context.
- **One commit per logical change.**  Tests, fixes, and refactors can each
  be their own commit even when they land in the same PR.


## What "ready to ship" means

A bug-fix PR is ready when:

1. `meson compile -C build` succeeds.
2. `meson test -C build` is green.
3. `meson test -C build-fpc "FPC RTL tests"` is green (if your change
   touches codegen, semcheck, or the runtime).
4. A regression test under `tests/test_cases/` exercises the fix.

If any of those is failing or skipped, say so in the PR description.
Leaving a partial fix in place — labelled as such — is much better than
hiding it behind a workaround.


## Reporting bugs

Open a GitHub issue with:

- The smallest `.p` you can reduce the repro to.
- The exact `kgpc` invocation.
- The output you got and the output you expected.
- Whether the same source compiles correctly with FPC (this is our
  golden-standard reference).

For FPC bootstrap regressions, attach the failing stage (`pp_stage1`,
`pp_stage2`, etc.) and the relevant `pp_bootstrap` invocation.
