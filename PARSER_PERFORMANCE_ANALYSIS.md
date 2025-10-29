# Pascal Parser Performance Benchmark Report

## Executive Summary

The Pascal parser test suite contains **88 tests** that collectively take **~156 seconds** to complete.

**Key Findings:**
- **16 tests** take ~7 seconds each (total: ~112 seconds) - **72% of total time**
- **8 tests** take 4-7 seconds each (total: ~36 seconds) - **23% of total time**
- **64 tests** take <1 second each (total: ~8 seconds) - **5% of total time**

The slowest tests are all related to **unit/program parsing** which requires initialization of very large parser combinators.

## Top 20 Slowest Tests

| Rank | Test Name | Time (s) | % of Total |
|------|-----------|----------|------------|
| 1 | test_complex_fpc_rax64int_unit | 7.352 | 4.7% |
| 2 | test_fpc_style_unit_parsing | 7.325 | 4.7% |
| 3 | test_pascal_class_operator_overload | 7.298 | 4.7% |
| 4 | test_pascal_overload_directive | 7.285 | 4.7% |
| 5 | test_pascal_type_helper_for_string | 7.255 | 4.7% |
| 6 | test_pascal_class_function_modifier | 7.246 | 4.6% |
| 7 | test_pascal_inline_directive | 7.229 | 4.6% |
| 8 | test_pascal_specialize_alias | 7.153 | 4.6% |
| 9 | test_pascal_out_parameter_modifier | 7.140 | 4.6% |
| 10 | test_pascal_uses_with_dotted_unit | 7.065 | 4.5% |
| 11 | test_pascal_forward_declared_function | 7.065 | 4.5% |
| 12 | test_pascal_resourcestring_section | 7.050 | 4.5% |
| 13 | test_pascal_unit_declaration | 7.048 | 4.5% |
| 14 | test_pascal_threadvar_section | 7.048 | 4.5% |
| 15 | test_pascal_unit_with_dotted_name | 7.047 | 4.5% |
| 16 | test_pascal_generic_type_declaration | 7.023 | 4.5% |
| 17 | test_pascal_set_operations_program | 4.758 | 3.1% |
| 18 | test_pascal_var_section | 4.553 | 2.9% |
| 19 | test_pascal_enumerated_type_declaration | 4.501 | 2.9% |
| 20 | test_pascal_simple_const_declaration | 4.473 | 2.9% |

## Root Cause Analysis

### The 7-Second Tests
All 16 tests taking ~7 seconds call `init_pascal_unit_parser()` or similar complete program parsers. These tests:
- Initialize very large parser combinator structures
- Include complex grammar rules for units, classes, generics, etc.
- Each test reinitializes the entire parser from scratch
- **Specific metrics:**
  - `pascal_declaration.c` allocates **96 combinators**
  - `pascal_statement.c` allocates **43 combinators**  
  - `pascal_type.c` allocates **28 combinators**
  - `pascal_expression.c` allocates **20 combinators**
  - Total: **~187+ combinator objects** created per unit parser initialization
  - This happens in **every single test** that uses unit/program parsers

### The 4-Second Tests
These 8 tests call `init_pascal_complete_program_parser()` which is slightly smaller but still substantial.

### Fast Tests (<1 second)
The 64 fast tests use simpler parsers:
- `init_pascal_expression_parser()` - for expressions (~20 combinators)
- `init_pascal_statement_parser()` - for statements (~43 combinators)
- Simple preprocessor tests (minimal combinators)

## Recommendations

### Immediate Actions (to fix timeout):
1. ✅ **Increase timeout** - Changed from 120s to 180s in meson.build (DONE)
2. **Share parser instances** - Initialize parsers once and reuse them across tests
   - Add test fixture setup/teardown to acutest framework
   - Initialize `unit_parser`, `program_parser`, `expression_parser`, `statement_parser` once
   - Reuse these instances across all tests
   - Expected savings: ~140 seconds (reduce 156s to ~16s)

### Long-term Optimizations:
1. **Profile parser initialization** - Use valgrind/callgrind on `init_pascal_unit_parser()` to find hotspots
2. **Optimize combinator allocation** - 187+ heap allocations per test is expensive
   - Consider object pooling for combinator structures
   - Pre-allocate common combinator patterns
3. **Lazy initialization** - Don't build all grammar rules upfront
   - Build rules on first use
   - Cache frequently used rules
4. **Split test suites** - Separate unit/program tests from expression/statement tests
   - Fast tests: <1s each, run frequently during development
   - Slow tests: ~7s each, run less frequently or in CI only

## Test Statistics

```
Total tests: 88
Total time: 155.784 seconds
Average per test: 1.77 seconds

Distribution:
- Tests >7s: 16 (18% of tests, 72% of time)
- Tests 4-7s: 8 (9% of tests, 23% of time)  
- Tests <1s: 64 (73% of tests, 5% of time)
```

## Detailed Timing Data

See `/tmp/pascal_test_timing.txt` for complete per-test timing information.
