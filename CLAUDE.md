# Pascal Compiler

## Build & Test

- Build: `meson compile -C build`
- Test: `meson test -C build`
- FPC RTL tests: `meson test -C build-fpc "FPC RTL tests"` (needs `meson setup build-fpc -Drun_fpc_rtl_tests=true`)
- Do NOT clear pp-cache manually — meson test manages it
- Do NOT run `meson compile` before `meson test` — meson test builds automatically
- Test individual files first before running the full suite
- Do NOT pipe meson test output — redirect to file or just check testlog.txt after: `build-fpc/meson-logs/testlog.txt`
- Do NOT re-run meson test when testlog.txt already has fresh results — grep it instead

## Code Standards

- No workarounds, fallbacks, or weak symbols
- No hardcoded special cases (e.g. `if strcmp(name, "Foo") == 0`)
- No modifying test files to fix compiler bugs
- Fix root causes structurally
- Verify before committing: build, compile test, link, run
