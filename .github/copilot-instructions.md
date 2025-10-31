# Copilot Instructions for Pascal Compiler

## Project Overview

This is the Gwinn Pascal Compiler (GPC), a Pascal compiler written in C that outputs gcc-targeted x86-64 assembly. The project uses a custom parser combinator library (cparser) for parsing Pascal code.

## Architecture

The compiler consists of several key components:

- **Parser**: Located in `GPC/Parser/` - Handles parsing Pascal source code using the cparser combinator library
  - `ParseTree/`: Parse tree structure and manipulation
  - `SemanticCheck/`: Semantic analysis and type checking
- **Code Generator**: Located in `GPC/CodeGenerator/Intel_x86-64/` - Generates x86-64 assembly code
- **Optimizer**: Located in `GPC/Optimizer/` - Implements optimization passes (O1, O2)
- **cparser**: Located in `cparser/` - Parser combinator library
  - `examples/pascal_parser/`: Pascal-specific parser implementation

## Build System

The project uses **Meson** as the build system.

### Dependencies

Required packages:
- `flex` - Lexical analyzer
- `bison` (or `byacc`) - Parser generator  
- `meson` - Build system
- `ninja` - Build backend
- `libunwind-dev` - Stack unwinding (optional)
- `libgmp-dev` - GNU Multiple Precision library (optional, enable with `-Dwith_gmp=enabled`)

### Build Commands

#### Native Linux Build

```bash
# Configure build
meson setup builddir --buildtype=release

# With GMP support (recommended)
meson setup builddir --buildtype=release -Dwith_gmp=enabled

# Build
meson compile -C builddir

# Run tests
meson test -C builddir
```

#### Cross-Compilation for Windows (using quasi-msys2)

The project supports cross-compilation from Linux to Windows using [quasi-msys2](https://github.com/HolyBlackCat/quasi-msys2).

```bash
# Clone and set up quasi-msys2
git clone https://github.com/HolyBlackCat/quasi-msys2.git
cd quasi-msys2
echo "UCRT64" > msystem.txt  # or MINGW64

# Install required packages
make install _gcc _gmp _meson _ninja _python _flex _bison

# Configure and build (from project root)
bash -c 'source quasi-msys2/env/all.src && meson setup builddir-cross --cross-file quasi-msys2/env/meson_cross_file.ini --buildtype=release -Dwith_gmp=enabled'
bash -c 'source quasi-msys2/env/all.src && meson compile -C builddir-cross'

# Run tests with Wine
bash -c 'source quasi-msys2/env/all.src && meson test -C builddir-cross'
```

**Cross-compilation dependencies:**
- `wine` - Run Windows executables on Linux
- `llvm`, `clang`, `lld` - Cross-compilation toolchain
- `wget`, `tar`, `zstd`, `gawk`, `gpg` - For quasi-msys2 package management

**Quick test script:**
```bash
# Automated test script (requires network access to MSYS2 repos)
./test-cross-compile.sh [UCRT64|MINGW64]
```

### Build Outputs

- Main compiler executable: `builddir/GPC/gpc`
- Runtime library: `builddir/GPC/libgpc_runtime.a`
- Parser tests: `builddir/cparser_tests`
- Pascal parser tests: `builddir/pascal_tests` (when integration tests enabled)

## Testing

### Test Structure

- `cparser/tests.c` - Unit tests for parser combinators
- `cparser/examples/pascal_parser/pascal_tests.c` - Pascal parser unit tests
- `tests/test_runner.py` - Integration test runner (TAP protocol)
- `tests/test_cases/` - Pascal test programs
- `tests/golden_ast/` - Expected AST outputs

### Running Tests

```bash
# All tests
meson test -C builddir

# Specific test suites
meson test -C builddir "cparser unit tests"
meson test -C builddir "Compiler tests"
meson test -C builddir "pascal parser unit tests"
```

## Compiler Usage

```bash
# Compile Pascal to assembly
./builddir/GPC/gpc input.p output.s [flags]

# Assemble with gcc
gcc -o output output.s

# Run
./output
```

### Compiler Flags

- `-non-local` - Enable non-local variable references (buggy, work in progress)
- `-O1` - Level 1 optimizations (constant folding)
- `-O2` - Level 2 optimizations (dead code elimination)

## Code Style and Conventions

### C Code

- Use standard C99/C11 conventions
- Memory management: **No memory leaks allowed** - this project prides itself on zero memory leaks (verified with valgrind)
- Always free allocated memory appropriately
- Use meaningful variable names
- Include error handling for all operations that can fail

### File Organization

- Header files (.h) contain declarations
- Implementation files (.c) contain definitions
- Keep related functionality together in logical modules
- Use include guards in all headers

### Pascal Code

- Test programs in `tests/test_cases/` should follow Pascal conventions
- Use `.p` or `.pas` extension for Pascal files

## Important Files and Directories

### Main Entry Points

- `GPC/main_cparser.c` - Main compiler entry point using cparser
- `GPC/main.c` - Legacy main (if applicable)

### Core Modules

- `GPC/Parser/pascal_frontend.c` - Pascal language frontend
- `GPC/Parser/SemanticCheck/SemCheck.c` - Main semantic checker
- `GPC/CodeGenerator/Intel_x86-64/codegen.c` - Code generation entry point
- `GPC/Optimizer/optimizer.c` - Optimization pass implementation

### Parser Combinator Library

- `cparser/parser.c` - Core parser combinator implementation
- `cparser/combinators.c` - Basic combinator functions
- `cparser/examples/pascal_parser/pascal_parser.c` - Pascal grammar implementation

### Build Configuration

- `meson.build` - Root build configuration
- `GPC/meson.build` - GPC-specific build configuration
- `meson_options.txt` - Build options

## Memory Management

This project has **zero tolerance for memory leaks**. When writing code:

1. Every `malloc`/`calloc`/`strdup` must have a corresponding `free`
2. Use valgrind to verify no leaks: `valgrind --leak-check=full ./builddir/GPC/gpc test.p test.s`
3. Clean up parse trees, symbol tables, and other data structures properly
4. Pay special attention to error paths - they must also free resources

## Platform Support

- **Primary platform**: Linux (Ubuntu-based systems tested)
- **Windows support**: Available via MSYS2 or cross-compilation from Linux using quasi-msys2
- **Architecture**: x86-64 Intel only
- **Assembly target**: gcc-compatible x86-64 assembly

## CI/CD Workflows

The project has multiple CI workflows in `.github/workflows/`:

### 1. Main CI (`ci.yml`)
- Runs on Ubuntu Linux
- Tests native Linux builds
- Uses Meson build system
- Runs all test suites

### 2. MSYS2 CI (`msys2-ci.yml`)
- Runs on Windows using MSYS2
- Tests multiple MSYS2 environments: MSYS, MINGW64, UCRT64, CLANG64
- Native Windows builds and tests

### 3. Cross-Compilation CI (`cross-compile-ci.yml`)
- Runs on Ubuntu Linux with Wine
- Uses quasi-msys2 for Linux-to-Windows cross-compilation
- Tests UCRT64 and MINGW64 targets
- Validates that Windows executables can be built from Linux
- Experimental - may use `continue-on-error` for some steps

All workflows are triggered on pull requests and pushes to any branch.

## Optimization Levels

### O1 - Constant Folding
- Simplifies expressions with constant values
- Example: `x := 3 + 5` → `x := 8`
- Reduces assembly instructions for constant operations

### O2 - Dead Code Elimination
- Removes unreferenced variables and assignments
- Tracks variable references through the program
- Eliminates variables never meaningfully used
- More aggressive optimization with better runtime/memory impact

## Common Tasks

### Adding a New Language Feature

1. Update parser in `cparser/examples/pascal_parser/pascal_*.c`
2. Add parse tree node type if needed in `GPC/Parser/ParseTree/tree.c`
3. Implement semantic checking in `GPC/Parser/SemanticCheck/`
4. Add code generation in `GPC/CodeGenerator/Intel_x86-64/`
5. Add test cases in `tests/test_cases/`
6. Update documentation if user-facing

### Adding Tests

1. Create Pascal test program in `tests/test_cases/`
2. Add expected output file with `.expected` extension
3. Test runner will automatically discover and run new tests

### Debugging

- Use `stacktrace.c` for stack trace functionality
- Debug serializer/deserializer available in `GPC/debug_*.c`
- Check `builddir/meson-logs/` for build issues
- Use GDB for debugging compiled executables

## Documentation

- Main README: `README.md`
- GPC-specific: `GPC/README.txt`
- cparser integration: `CPARSER_INTEGRATION.md`
- Performance analysis: `PARSER_PERFORMANCE_ANALYSIS.md`
- Benchmark summary: `BENCHMARK_SUMMARY.md`
