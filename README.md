# Kreijstal Gwinn Pascal Compiler (KGPC)

> **Status: alpha (v0.0.1).**  KGPC is pre-release software.  The headline
> goal — bootstrapping Free Pascal without FPC or a proprietary
> Delphi-compatible compiler — has been met: as of master `02633217` the
> Stage 1–3 self-host chain is verified and exercised in CI on every push.
> "Alpha" refers to language coverage, code quality, and API stability —
> all of which are still moving.  See [`STATUS.md`](STATUS.md) for what
> works, what doesn't, and the open compiler bugs.

A fork of [gwinndr/Pascal-Compiler](https://github.com/gwinndr/Pascal-Compiler).

## Overview

KGPC compiles Pascal source to x86-64 assembly (Intel syntax) and delegates
assembling / linking to `gcc` or `clang`.  The compiler is written in C; a
small portion of the runtime is in Pascal under [`KGPC/Units/`](KGPC/Units/).

The project's reason for existing is to compile FPC without already having
FPC installed.  FPC is the golden reference: **if FPC accepts a program and
KGPC doesn't, that's a KGPC bug.**

## Architecture

Pipeline: parse → semantic check → optimize → codegen.

For a real walkthrough — source layout, where to look for each kind of
change, the type system, ABI handling, and the linking story — see
[`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md).

## Dependencies

- **Build system:** `meson` (>= 0.60), `ninja`
- **C compiler:** `gcc` or `clang`
- **Assembler / linker:** `gcc` or `clang` (KGPC emits assembly only)
- **Optional:** `libgmp` for arbitrary-precision integer support
  (auto-detected; controlled by `-Dwith_gmp=enabled|disabled|auto`)
- **For the FPC RTL test mode:** an FPC source checkout under `FPCSource/`

## Build

```bash
meson setup build
meson compile -C build
```

The compiler executable will be at `build/KGPC/kgpc`.

## Test

```bash
meson test -C build
```

This runs the KGPC test suite (1000+ Pascal programs compiled, linked, and
diffed against expected output).  To additionally exercise compatibility
with the real FPC RTL:

```bash
meson setup build-fpc -Drun_fpc_rtl_tests=true
meson test -C build-fpc "FPC RTL tests"
```

See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the full test workflow.

## Usage

```bash
./build/KGPC/kgpc <input.p> <output.s> [flags]
gcc -o <output> <output.s>
./<output>
```

`kgpc --help` lists every supported flag.  `kgpc --version` prints the
compiler version.

## Repository layout

| Path              | What's there                                          |
|-------------------|-------------------------------------------------------|
| `KGPC/`           | The compiler — parser, semcheck, optimizer, codegen, runtime, RTL units |
| `cparser/`        | Parser-combinator library used by the front end       |
| `common/`         | Shared utilities (arena allocator, file locking)      |
| `docs/`           | Architecture, type-system, scoping, and IR notes      |
| `tests/`          | Test cases (`tests/test_cases/*.p`) and the harness   |
| `examples/`       | Small standalone programs you can compile             |
| `scripts/`        | Bootstrap orchestration (`fpc_bootstrap.sh`) and CI helpers |
| `quasi-msys2/`    | Linux→Windows cross-compilation environment used by the cross-compile CI workflow (external project, see its own README) |
| `FPCSource/`      | Optional FPC source checkout — not vendored; needed for FPC RTL test mode |
| `STATUS.md`       | What works, what doesn't, bootstrap progress          |
| `CHANGELOG.md`    | Release notes                                         |
| `CONTRIBUTING.md` | Build/test workflow + contributor conventions         |
| `SECURITY.md`     | Vulnerability reporting                               |

## License

KGPC is licensed under the **GNU Affero General Public License v3.0**
(AGPLv3).  See [`LICENSE`](LICENSE) for the full text.

The AGPL's network-deployment clause is unusual for a compiler; if you're
considering using KGPC inside a hosted service, read the licence
carefully.

## Acknowledgements

KGPC is built on top of [Damon Gwinn's Pascal-Compiler](https://github.com/gwinndr/Pascal-Compiler),
which provided the original parser and code generator the rest of this
project is grown from.
