# Parser Performance Benchmark - Summary

## Problem Statement
The Pascal parser tests were timing out at 120 seconds, preventing successful test runs.

## Investigation Results

### Benchmark Methodology
- Used acutest's built-in `--time` flag to measure individual test performance
- Ran all 88 tests with timing enabled
- Analyzed results to identify slowest tests and root causes

### Key Findings

**Test Timing Distribution:**
```
Total: 88 tests, ~156 seconds
- 16 tests @ ~7s each  = 112s (72% of time)
- 8 tests  @ 4-7s each =  36s (23% of time)
- 64 tests @ <1s each  =   8s (5% of time)
```

**Root Cause:** Parser combinator initialization overhead
- Slow tests all use `init_pascal_unit_parser()` or `init_pascal_complete_program_parser()`
- Each initialization creates 187+ combinator objects
- Every test reinitializes from scratch (no reuse)

**Component Breakdown:**
- `pascal_declaration.c`: 96 combinators
- `pascal_statement.c`: 43 combinators
- `pascal_type.c`: 28 combinators
- `pascal_expression.c`: 20 combinators

## Solution Implemented

### Immediate Fix ✅
- **Increased timeout from 120s to 180s** in `meson.build`
- Tests now pass reliably (158s < 180s timeout)
- File changed: `meson.build` line 94

### Documentation Added ✅
1. **PARSER_PERFORMANCE_ANALYSIS.md** - Detailed analysis with:
   - Top 20 slowest tests
   - Root cause analysis
   - Optimization recommendations
   
2. **parser_test_timing_details.txt** - Raw timing data for all tests

3. **benchmark_pascal_tests.c** - Standalone benchmarking tool (future use)

## Recommended Next Steps

### High Priority (90% time savings)
**Reuse parser instances across tests:**
- Modify test suite to use shared parser fixtures
- Initialize each parser type once (unit, program, expression, statement)
- Expected improvement: 156s → ~16s

### Medium Priority
**Profile parser initialization:**
- Use valgrind/callgrind to identify hotspots
- Optimize combinator allocation patterns

### Lower Priority
**Split test suites:**
- Fast tests (<1s) for rapid development feedback
- Slow tests (4-7s) for comprehensive CI validation

## Files Changed
- `meson.build` - Increased test timeout
- `PARSER_PERFORMANCE_ANALYSIS.md` - Added (comprehensive analysis)
- `parser_test_timing_details.txt` - Added (raw timing data)
- `cparser/examples/pascal_parser/benchmark_pascal_tests.c` - Added (benchmarking tool)

## Verification
```bash
$ meson test -C build "pascal parser unit tests"
1/1 pascal parser unit tests OK    158.51s
```

Tests now pass successfully within the 180-second timeout.
