# Test Auto-Discovery

The test runner (`do_not_run_me_directly_but_through_meson.py`) now automatically discovers and runs test cases!

## How It Works

Any Pascal file in `tests/test_cases/` that has a corresponding `.expected` file will automatically become a test case. No manual test method definition is required.

## Adding a New Test

1. Create your Pascal test program: `tests/test_cases/my_new_test.p`
2. Create the expected output: `tests/test_cases/my_new_test.expected`
3. Run `meson test -C builddir` - your test will be automatically discovered and run!

The test will:
- Compile `my_new_test.p` to assembly
- Link it with the runtime library
- Execute the program
- Compare the output to `my_new_test.expected`

## Naming Convention

Auto-discovered tests are named `test_auto_<basename>` where `<basename>` is the name of the `.p` file without the extension. Hyphens and spaces in filenames are converted to underscores.

For example:
- `my_test.p` → `test_auto_my_test`
- `my-complex-test.p` → `test_auto_my_complex_test`

## Manual Tests

You can still define manual test methods for tests that need special behavior:
- Custom compilation flags
- Additional assertions (e.g., checking assembly output)
- Different execution patterns

Just define a test method in the `TestCompiler` class and it will run alongside the auto-discovered tests.

## Running Tests

**DO NOT** run `do_not_run_me_directly_but_through_meson.py` directly! Always use Meson:

```bash
# Run all tests
meson test -C builddir

# Run only compiler tests
meson test -C builddir "Compiler tests"

# Run with verbose output
meson test -C builddir "Compiler tests" -v
```
