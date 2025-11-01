# Agent Testing Requirements

## Testing Protocol

When working on this project, you must **always** test your changes using the Meson build system:

```bash
meson test -C builddir
```

## Why Meson Test?

- **Comprehensive Coverage**: Runs all test suites including unit tests, integration tests, and compiler tests
- **Consistent Environment**: Ensures tests run in a controlled, reproducible environment
- **TAP Protocol**: Uses Test Anything Protocol for standardized test reporting
- **Build Integration**: Automatically handles build dependencies and test discovery

## Test Suites

The project includes multiple test suites:

- **cparser unit tests**: Tests for the parser combinator library
- **Compiler tests**: Integration tests for Pascal compilation
- **pascal parser unit tests**: Tests for Pascal-specific parser functionality

## Running Specific Test Suites

```bash
# All tests
meson test -C builddir

# Specific test suites
meson test -C builddir "cparser unit tests"
meson test -C builddir "Compiler tests"
meson test -C builddir "pascal parser unit tests"
```

## Important Notes

- Always run the full test suite before committing changes
- If you add new functionality, add corresponding test cases in `tests/test_cases/`
- Test failures must be investigated and resolved
- Do not commit code that causes test regressions

## Cross-Platform Testing

The project supports multiple platforms. When testing:

- **Linux**: Native `meson test -C builddir`
- **Windows (MSYS2)**: `meson test -C builddir` in MSYS2 environment
- **Cross-compilation**: `meson test -C builddir-cross` with Wine

Refer to the main project documentation for detailed cross-compilation setup instructions.
