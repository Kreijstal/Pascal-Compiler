# Performance Benchmarking Analysis of GPC Parser and Semantic Checker

## Executive Summary

We investigated performance bottlenecks in the Gwinn Pascal Compiler (GPC) when compiling `btpc.dpr` (a 2776-line Pascal file that segfaults during codegen) and `timeout_test.p` (a 37-line malformed Pascal file that causes timeouts).

**Key Finding: The parser is the primary bottleneck, NOT the semantic checker.**

## Benchmark Results

### btpc.dpr (2776 lines)
- **Total compile time**: ~3.15 seconds (until segfault in codegen)
- **Parsing**: 2.97 seconds (94% of time)
  - Preprocessing: 0.0002 seconds
  - Core parsing: 2.95 seconds
  - AST conversion: 0.003 seconds
- **Semantic checking**: 0.002 seconds (0.07% of time)
  - Symbol table build: 0.0008 seconds
  - Type checking: 0.0016 seconds

### timeout_test.p (37 lines, malformed)
- **Behavior**: Times out (>5 seconds) during parsing
- Never reaches semantic checking phase
- Parser appears to enter infinite loop or extremely slow path

## System Call Analysis (strace)

Running on btpc.dpr revealed:
- **10,696 `brk` calls** (memory allocation) - 73% of system call time
- Heavy memory allocation pattern suggests:
  - Parser combinator backtracking
  - Inefficient memory management in parser
  - Possible memory fragmentation

## Root Cause Analysis

### Why Parsing is Slow

1. **Parser Combinator Overhead**
   - The cparser library uses parser combinators
   - Backtracking in combinator parsers can be exponential in pathological cases
   - Malformed input (like timeout_test.p) likely triggers worst-case backtracking

2. **Memory Allocation**
   - 10,696 memory allocations for a 2776-line file
   - ~3.9 allocations per line of code
   - Each backtrack/retry likely allocates new AST nodes

3. **AST Construction**
   - Building intermediate AST structures during parsing
   - Converting from cparser AST to legacy parse tree adds overhead

### Why Semantic Checking is Fast

- Well-optimized symbol table operations
- Type checking is straightforward once parse tree exists
- No backtracking or complex search algorithms
- Takes only 2.3ms for btpc.dpr

## Profiling Tools Attempted

1. **Custom Benchmark Infrastructure** ✅
   - Successfully implemented
   - Provides phase-level timing
   - Added to codebase with `--benchmark` flag

2. **gprof (with -pg)** ❌
   - Configured but program segfaults before writing gmon.out
   - Would work better on non-crashing programs

3. **perf** ❌
   - Blocked by kernel security settings (perf_event_paranoid=4)
   - Requires elevated permissions in CI environment

4. **valgrind --tool=callgrind** ⏱️
   - Available but extremely slow
   - Would timeout on already-slow test cases
   - Not practical for this analysis

5. **strace -c** ✅
   - Successfully identified heavy brk() usage
   - Revealed memory allocation patterns

## Recommendations for Optimization

### High Priority (Parser Bottleneck)

1. **Memoization in Parser Combinators**
   - Cache parsing results for repeated input positions
   - Prevents redundant parsing attempts
   - Classic packrat parsing technique

2. **Limit Backtracking Depth**
   - Add depth limits or timeouts to prevent infinite backtracking
   - Fail fast on malformed input

3. **Memory Pool Allocation**
   - Pre-allocate memory pools for AST nodes
   - Reduce syscall overhead from thousands of small allocations
   - Custom allocator for parser combinator framework

4. **Error Recovery**
   - Better error recovery to avoid catastrophic backtracking
   - Identify syntax errors earlier without trying all alternatives

### Medium Priority

5. **Direct AST Construction**
   - Consider building final parse tree directly
   - Skip intermediate cparser AST conversion step
   - Saves 0.003s per file (minor but measurable)

6. **Incremental Parsing**
   - For large files, consider line-based or block-based parsing
   - Can abandon early on malformed input

### Low Priority (Semantic Checker - Already Fast)

7. **Hash Table Tuning**
   - Already performant at 2.3ms
   - Not worth optimizing further unless doing millions of compilations

## Implementation of Profiling Infrastructure

We added comprehensive benchmarking:

### Files Added
- `GPC/benchmark.h` - Benchmark API and data structures
- `GPC/benchmark.c` - Cross-platform timing implementation

### Files Modified
- `GPC/flags.h/c` - Added `--benchmark` flag
- `GPC/main_cparser.c` - Integrated benchmark initialization
- `GPC/Parser/pascal_frontend.c` - Detailed parser phase timing
- `GPC/Parser/SemanticCheck/SemCheck.c` - Semantic phase timing
- `GPC/meson.build` - Added benchmark.c to build

### Usage
```bash
./gpc input.p output.s --benchmark
```

### Output
```
[benchmark] Preprocessing: 0.000194 s
[benchmark] Core parsing: 2.952475 s
[benchmark] AST conversion: 0.003341 s
[benchmark] Total pascal_parse_source: 2.960783 s

================================================================================
                         PERFORMANCE BENCHMARK SUMMARY
================================================================================

Phase                                         Calls    Total (s)     Avg (ms)
--------------------------------------------------------------------------------
Parse Total                                       2     3.057678  1528.838778
Parse: Program Structure                          2     3.021632  1510.815943
Semantic Total                                    1     0.002448     2.447692
Semantic: Symbol Table Build                      1     0.000830     0.829964
Semantic: Type Checking                           1     0.001614     1.613600
================================================================================
```

## Conclusion

The performance investigation clearly shows that **parser optimization is needed, not semantic checker optimization**. The parser is 1,200x slower than the semantic checker. Implementing memoization and memory pooling in the parser combinator library would likely yield the biggest performance improvements.

For malformed inputs that cause timeouts, adding backtracking limits or better error recovery would allow the compiler to fail fast instead of entering exponential-time parsing paths.

## Alternative Approach: Using Standard Tools

**Note**: Instead of custom benchmarking, standard profiling tools could be used:
- `gprof` with `-pg` compilation flag (if program doesn't crash)
- `perf record -g` (requires kernel permissions)
- `valgrind --tool=callgrind` (very slow but comprehensive)
- `strace -c` (system call analysis)

However, the custom benchmark provides adequate phase-level timing for identifying the bottleneck without needing elevated permissions or dealing with slow profilers.
