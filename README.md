# Kreijstal Gwinn Pascal Compiler (KGPC)

**Fork based on [gwinndr/Pascal-Compiler](https://github.com/gwinndr/Pascal-Compiler)**

## Overview

KGPC is a Pascal Compiler written in C. It takes Pascal code and outputs gcc-targeted x86-64 assembly.

**Goal:** The true goal of this repository is merely bootstrapping Free Pascal Compiler (FPC) without needing FPC nor proprietary compilers. A very basic RTL was written, and FPC is used as the golden standard. If FPC can do it but we can't, it is a bug.

## Architecture

The compiler consists of several distinct stages:

1.  **Parser:** Uses `cparser` combinator library to generate an AST.
2.  **Semantic Analysis:** Performs type checking, scope resolution, and builds a unified `GpcType` system.
3.  **Optimizer:** Performs passes like Dead Code Elimination and Constant Folding.
4.  **Code Generator:** Outputs x86-64 assembly (Intel syntax).

## Dependencies

-   **Build System:** `meson`
-   **Assembler/Linker:** `gcc` or `clang` (KGPC only generates assembly; GCC/Clang is used to assemble and link)

## How to Build and Run

This project uses the Meson build system. It works on Windows, Linux, and Wine (checked in CI).

### Building

```bash
meson setup build
meson compile -C build
```

### Testing

```bash
meson test -C build
```

### Usage

After building, the compiler executable `kgpc` will be in the `build` directory.

```bash
./build/kgpc [Input File] [Output File] [Optional Flags]
```

Example:
```bash
./build/kgpc test.p test.s
gcc -o test test.s
./test
```

## License

This project is licensed under the GNU Affero General Public License v3.0 (AGPLv3). See the [LICENSE](LICENSE) file for details.
