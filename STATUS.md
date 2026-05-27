# KGPC Status

KGPC is **alpha software** with **the headline self-hosting milestone
already met**: as of master `02633217` (May 2026), KGPC can compile FPC,
the resulting compiler can recompile FPC, and the resulting Stage 3
compiler can build and run user programs.  CI exercises this chain on
every push (`test_fpcrtl_pp_pas_bootstrap`).

The "alpha" label is about language coverage, code-quality, and API
stability — *not* about the bootstrap goal.

Last updated: 2026-05-26 (KGPC 0.0.1)

---

## Maturity at a glance

| Area                     | State                                          |
|--------------------------|------------------------------------------------|
| Build on Linux x86-64    | Stable                                         |
| Build via MSYS2 / MinGW  | Stable in CI (MSYS, MINGW64, UCRT64, CLANG64)  |
| Build via Wine cross     | Stable in CI                                   |
| Compiler self-test suite | 1000+ tests green                              |
| FPC RTL compatibility    | Substantial subset compiles & runs             |
| **FPC self-host**        | **Stages 1–3 verified** — see below            |
| Tagged releases          | None yet; 0.0.1 is the first                   |
| API / flag stability     | None promised; expect breaking changes         |

---

## The stated goal

KGPC's reason for existing is to compile the Free Pascal Compiler (FPC)
without needing FPC or a proprietary Delphi-compatible compiler.  Progress
is measured in *stages* — each stage uses the previous stage's output to
compile the next.

```
  ┌─ Stage 0: KGPC (this repository) ───────────────────────┐
  │  C source → x86-64 assembly                              │
  └────────────────────┬─────────────────────────────────────┘
                       │ compiles
                       ▼
  ┌─ Stage 1: pp_bootstrap (FPC, compiled by KGPC) ──────────┐
  │  pp.pas → pp_bootstrap binary                            │
  └────────────────────┬─────────────────────────────────────┘
                       │ recompiles pp.pas
                       ▼
  ┌─ Stage 2: pp_stage2 (FPC, compiled by pp_bootstrap) ─────┐
  │  Runs successfully; reports the FPC help banner          │
  └────────────────────┬─────────────────────────────────────┘
                       │ compiles user programs
                       ▼
  ┌─ Stage 3: hello-world built and executed by pp_stage2 ───┐
  │  End-to-end FPC self-hosting confirmed                   │
  └──────────────────────────────────────────────────────────┘
```

### Where we are

| Stage   | Status      | Notes                                                                 |
|---------|-------------|-----------------------------------------------------------------------|
| Stage 0 | ✅ Verified  | KGPC itself builds, runs, and passes its test suite                   |
| Stage 1 | ✅ Verified  | KGPC compiles & links `pp.pas` → `pp_bootstrap`; `pp_bootstrap -h` works |
| Stage 2 | ✅ Verified  | `pp_bootstrap` compiles `pp.pas` → `pp_stage2`; `pp_stage2 -h` works  |
| Stage 3 | ✅ Verified  | `pp_stage2` builds and runs a hello-world program end-to-end          |

Every push to `master` runs the whole chain as
`tests.harness.test_compiler.TestCompiler.test_fpcrtl_pp_pas_bootstrap`
under the `fpc-rtl-tests` CI job, plus the Linux, MSYS2, and Windows
cross-compile workflows.  See [`docs/FPC_BOOTSTRAP.md`](docs/FPC_BOOTSTRAP.md)
for the manual reproduction commands and the
[`scripts/fpc_bootstrap.sh`](scripts/fpc_bootstrap.sh) driver script.

### What's *not* yet verified

- **Fixed-point byte-identity** (`pp_stage2 == pp_stage3` byte-for-byte).
  The test confirms that Stage 3 *produces working binaries*; it does not
  yet bit-compare a compiler compiled by Stage 2 with one compiled by
  Stage 3.  This is the classic stricter "self-hosting confirmed" check.
- **Building the full FPC distribution** (compiler + RTL + packages) from
  scratch with KGPC at the seed.  Today's bootstrap leans on a same-source
  prebuilt RTL where the FPC `make` targets need a Pascal compiler to
  bring up.

---

## Known open compiler bugs

These are real KGPC defects that affect specific Pascal patterns.  None of
them gate the Stage 1-3 bootstrap chain (which is why CI is green), but
they limit how much *other* FPC-style Pascal you can throw at the compiler.

### Stage 4: AnsiString → RawByteString/UnicodeString var-param mismatch
- **Symptom:** FPC-built-by-KGPC rejects 8 specific `cutils.pas` call sites
  for `Delete(s,i,n)` / `Insert(s2,s,i)` when `s` is an AnsiString and the
  compilerproc has a RawByteString var-param.  FPC normally accepts these
  due to codepage compatibility rules.
- **Impact:** prevents some Pascal programs that exercise mixed string
  encodings from compiling, but does not block the Stage 2/3 self-host.
- **Repro/status:** captured in agent memory as
  `project_pp_bootstrap_stage4_remaining.md` (with a stale-memory caveat;
  re-verify before fixing).

### `@procedure` argument SIGSEGV (FPC-built-by-KGPC parse-time)
- **Symptom:** A minimal `function(x): T` procvar passed via `@proc_name`
  parses fine in KGPC directly but makes the KGPC-built FPC crash *while
  parsing* the same source.  Backtrace lands in
  `tprocsym.find_procdef_with_comparer`.
- **Impact:** prevents specific FPC-internal source files from being
  compilable by an unrebuilt pp_bootstrap, but pp_stage2 (built by
  pp_bootstrap from a self-compatible source set) is not affected — the
  full bootstrap chain works.

---

## What works well

- The bootstrap chain (Stage 0 → Stage 3), as above.
- Programs that target the KGPC-shipped RTL (`KGPC/Units/`) and stay within
  the language subset FPC accepts under `{$mode objfpc}`.
- Substantial portions of the FPC RTL — `system.pp`, `sysutils.pp`,
  `classes.pp`, `dateutils.pp`, `math.pp`, `baseunix.pp`, `unix.pp`,
  `typinfo.pp`, `fgl.pp`, `keyboard.pp`, `crt.pp`, `objpas.pp` — compile and
  link against either the bundled units or the real FPC RTL via
  `-Drun_fpc_rtl_tests=true`.
- Class hierarchies with virtual methods, constructors, destructors,
  abstract methods, type helpers, and generics.
- ShortString / AnsiString / UnicodeString interop along the FPC
  pass-by-reference conventions.
- ASAN-clean runs under `MALLOC_PERTURB_=249` — the 30+ AST-rewrite leaks
  in the Wave 3-5 series have all been chased down.

## What's known-broken or limited

- The two open bugs listed above.
- The legacy lex/yacc front end under `KGPC/Parser/LexAndYacc/` has been
  removed; the cparser combinator front end is the only supported entry.
- A small set of tests with platform-specific assumptions that the harness
  skips automatically (`POSIX_ONLY_TESTS`, `SYSV_ABI_ONLY_TESTS` in
  `tests/harness/auto_discovery.py`).

---

## Platforms

| Host                          | Status                                  |
|-------------------------------|-----------------------------------------|
| Linux x86-64                  | Primary target; fully tested            |
| MSYS2 (MSYS)                  | CI green; POSIX-emulating layer         |
| MSYS2 (MINGW64 / UCRT64 / CLANG64) | CI green; Windows ABI            |
| Wine cross-compile (MinGW64)  | CI green                                |
| macOS                         | Not regularly tested                    |
| BSD                           | Not regularly tested                    |
| ARM64                         | Not supported (no AArch64 codegen)      |

x86-64 is the only target ABI today.  The codegen tree is structured so
that adding another target would live under
`KGPC/CodeGenerator/<arch>/`, but no work has started on a second back end.

---

## Versioning and release cadence

Releases stay in the `0.0.x` range while the compiler is alpha — language
coverage and the flag surface are still moving.  `0.1.0` will be the first
release with a stable enough surface to make compatibility promises about.
`1.0.0` is reserved for a stable language and flag surface; there is no
timeline.

The FPC self-host milestone, which the original `0.1.0` plan was gated
on, has already been met — but other aspects of "ready to make promises
about" haven't been, which is why the version is still `0.0.x`.
