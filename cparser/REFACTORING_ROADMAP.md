# Parser Library Refactoring Roadmap

## Executive Summary

The cparser library is a parser combinator framework that exhibits significant performance and memory management issues. This document provides an architectural analysis and phased refactoring roadmap to address critical pain points while maintaining backward compatibility.

## Current Architecture Analysis

### Core Components

1. **Parser Combinators** (`parser.c`, `combinators.c` - 2,466 LOC)
   - Recursive descent parser using combinator pattern
   - Memoization via hash table for packrat parsing
   - Dynamic AST construction during parsing

2. **Memory Management**
   - Manual malloc/free throughout
   - No memory pooling or arena allocation
   - ~3.9 allocations per line of parsed code (10,696 allocations for 2,776 lines)

3. **Memoization System**
   - Hash table with linked-list buckets
   - Stores parse results at each position+combinator
   - Prevents re-parsing but adds memory overhead

## Pain Points Identified

### 1. Performance Issues

**Problem**: Parsing 2,776 lines takes ~3 seconds vs FPC's 0.6s for full compilation
- **Root Cause**: Exponential backtracking in combinator approach
- **Evidence**: 10,696 malloc calls, heavy system call overhead
- **Impact**: 5x slower than production compilers

**Specific Issues**:
- No backtracking depth limits → pathological cases timeout
- Memoization overhead for every combinator at every position
- No early failure detection for malformed input
- Heavy cloning of parse results for memoization

### 2. Memory Management

**Problem**: Fragmented allocations, no cleanup strategy for intermediate objects
- **Current State**: Each combinator, AST node, error uses individual malloc
- **Impact**: Memory fragmentation, poor cache locality
- **Leak Sources**:
  - ✅ FIXED: Memo table now properly freed via `free_input()`
  - ⚠️ REMAINING: Dynamically created combinators during parsing (e.g., `init_pascal_statement_parser`)
  - ⚠️ REMAINING: Intermediate AST nodes during backtracking

### 3. Error Handling

**Problem**: Error messages lack context, poor error recovery
- No incremental error reporting
- Committed errors prevent alternatives but still allocate
- Partial AST not always useful for diagnostics

### 4. API Design

**Problem**: Tight coupling, difficult to extend
- Combinator creation scattered across multiple functions
- No clear separation between parsing and AST construction
- Hard to inject custom allocators or profiling

## Refactoring Roadmap

### Phase 1: Foundation (Weeks 1-2)
**Goal**: Establish infrastructure for incremental improvements

#### 1.1 Memory Pool Allocator
```c
typedef struct {
    char* buffer;
    size_t size;
    size_t used;
    struct arena_block* next;
} arena_t;

arena_t* arena_create(size_t initial_size);
void* arena_alloc(arena_t* arena, size_t size);
void arena_reset(arena_t* arena);
void arena_destroy(arena_t* arena);
```

**Benefits**:
- Reduce malloc overhead from thousands to dozens
- Improve cache locality
- Simplify cleanup (one arena destroy vs many free calls)
- 50-70% reduction in allocation overhead expected

**Implementation**:
1. Add `arena_t* arena` to `input_t` structure
2. Replace `safe_malloc` with `arena_alloc` for AST nodes
3. Add arena reset on parse restart
4. Benchmark: Target <1000 malloc calls for btpc.dpr

#### 1.2 Combinator Registry
```c
typedef struct {
    combinator_t** combinators;
    size_t count;
    size_t capacity;
} combinator_registry_t;

void register_combinator(combinator_registry_t* reg, combinator_t* c);
void cleanup_combinators(combinator_registry_t* reg);
```

**Benefits**:
- Track all dynamically created combinators
- Enable proper cleanup
- Foundation for combinator reuse/caching

**Implementation**:
1. Add registry to parser context
2. Modify combinator constructors to register
3. Add cleanup in parser teardown
4. Fix remaining memory leaks from init_pascal_*_parser

### Phase 2: Performance Optimization (Weeks 3-5)

#### 2.1 Selective Memoization
**Problem**: Memoizing every combinator is expensive

**Solution**: Only memoize expensive operations
```c
#define MEMO_THRESHOLD 10  // Only memo if combinator called >10 times at position

typedef struct {
    size_t call_count;
    bool should_memoize;
} combinator_stats_t;
```

**Benefits**:
- Reduce memo table size by 60-80%
- Less cloning overhead
- Faster for simple parsers

**Implementation**:
1. Add call counting to combinator invocations
2. Dynamic threshold based on input size
3. Profile btpc.dpr to identify high-value memo targets

#### 2.2 Backtracking Depth Limit
```c
#define MAX_BACKTRACK_DEPTH 100

typedef struct {
    int depth;
    int max_depth;
    bool exceeded;
} backtrack_context_t;
```

**Benefits**:
- Prevent infinite loops on malformed input
- Fast-fail for pathological cases
- timeout_test.p would fail in <100ms instead of >5s

**Implementation**:
1. Add depth counter to input_t
2. Check depth before each parse attempt
3. Return early with helpful error

#### 2.3 Lazy Combinator Optimization
**Current Issue**: Lazy combinator creates new combinator on every call

**Solution**: Cache the result after first resolution
```c
typedef struct {
    combinator_t* (*fn)(void);
    combinator_t* cached;
    bool resolved;
} lazy_cache_t;
```

**Benefits**:
- Eliminate repeated combinator creation
- Reduce memory churn
- Faster recursive grammar support

### Phase 3: Architecture Improvements (Weeks 6-8)

#### 3.1 Streaming Parser API
**Problem**: Current API requires full input in memory

**Solution**: Support incremental parsing
```c
typedef struct {
    bool (*read_chunk)(void* ctx, char** buffer, size_t* len);
    void* context;
} stream_input_t;

ParseResult parse_stream(stream_input_t* stream, combinator_t* parser);
```

**Benefits**:
- Parse large files without full load
- Enable early error detection
- Support interactive parsing

#### 3.2 Error Recovery Framework
```c
typedef struct {
    ast_t* (*recover_fn)(ParseError* err, input_t* in);
    int max_errors;
} error_recovery_t;

void set_error_recovery(combinator_t* parser, error_recovery_t* recovery);
```

**Benefits**:
- Continue parsing after errors
- Better IDE integration
- More useful diagnostics

#### 3.3 AST Visitor Pattern
**Already exists but enhance**:
```c
typedef enum {
    VISIT_PREORDER,
    VISIT_POSTORDER,
    VISIT_SKIP_SUBTREE
} visit_action_t;

visit_action_t (*ast_visitor_enhanced)(ast_t* node, int depth, void* ctx);
```

**Benefits**:
- Better AST transformations
- Easier semantic analysis
- Support for optimization passes

### Phase 4: Advanced Features (Weeks 9-12)

#### 4.1 Parallel Parsing
**For very large files**: Parse independent sections concurrently

```c
typedef struct {
    int start_line;
    int end_line;
    ast_t* result;
} parse_section_t;

ast_t* parse_parallel(input_t* in, combinator_t* parser, int num_threads);
```

**Benefits**:
- Multi-core utilization
- 2-4x speedup on large files
- Better for batch processing

#### 4.2 Incremental Re-parsing
**For editor scenarios**: Only re-parse changed sections

```c
typedef struct {
    int changed_start;
    int changed_end;
    ast_t* old_ast;
} incremental_context_t;

ast_t* parse_incremental(incremental_context_t* ctx, combinator_t* parser);
```

**Benefits**:
- Near-instant re-parse for small edits
- Essential for language server protocol
- Better developer experience

#### 4.3 Grammar Optimization Tool
**Analyze grammar for performance issues**

```c
typedef struct {
    combinator_t* problematic;
    char* issue;
    char* suggestion;
} grammar_issue_t;

grammar_issue_t* analyze_grammar(combinator_t* root);
```

**Benefits**:
- Identify left-recursion issues
- Detect ambiguous patterns
- Suggest refactorings

## Implementation Strategy

### Prioritization Matrix

| Feature | Impact | Effort | Priority |
|---------|--------|--------|----------|
| Memory Pool Allocator | HIGH | MEDIUM | 1 |
| Combinator Registry | HIGH | LOW | 2 |
| Backtracking Depth Limit | HIGH | LOW | 3 |
| Selective Memoization | MEDIUM | MEDIUM | 4 |
| Lazy Combinator Cache | MEDIUM | LOW | 5 |
| Error Recovery | MEDIUM | HIGH | 6 |
| Streaming API | LOW | HIGH | 7 |
| Parallel Parsing | LOW | VERY HIGH | 8 |

### Success Metrics

**Phase 1 Targets**:
- ✅ Zero memory leaks in valgrind (definite/indirect)
- ✅ <1000 malloc calls for btpc.dpr (currently 10,696)
- Parse time: <1.5s (currently 3s)

**Phase 2 Targets**:
- Parse time: <1.0s for btpc.dpr
- timeout_test.p: Fail fast <100ms
- Memory usage: <50% of current

**Phase 3+ Targets**:
- Parse time: <0.5s (approaching FPC)
- Support files >100K lines
- IDE-ready with incremental parsing

## Backward Compatibility

### API Stability
- Keep existing combinator constructors unchanged
- Add `_v2` versions for new features
- Deprecation path: 2 versions before removal

### Migration Guide
```c
// Old style (still supported)
input_t* input = new_input();
// ... use ...
free_input(input);

// New style (recommended)
parse_context_t* ctx = parse_context_create();
ctx->enable_arena = true;
ctx->backtrack_limit = 100;
ParseResult res = parse_with_context(ctx, input, parser);
parse_context_destroy(ctx);
```

## Testing Strategy

### Performance Benchmarks
1. **Micro-benchmarks**: Individual combinator performance
2. **Macro-benchmarks**: Full file parsing (btpc.dpr, etc.)
3. **Regression suite**: Ensure no slowdowns

### Memory Profiling
1. Valgrind for leak detection
2. Massif for allocation profiling
3. Custom arena statistics

### Correctness
1. All existing tests must pass
2. Add property-based tests for combinators
3. Fuzzing for error cases

## Risk Mitigation

### Technical Risks
- **Performance regression**: Comprehensive benchmarking before/after
- **Memory safety**: Valgrind + AddressSanitizer in CI
- **Breaking changes**: Feature flags + versioned API

### Timeline Risks
- **Scope creep**: Strict phase boundaries
- **Complexity**: Incremental delivery, can stop after any phase
- **Testing burden**: Automated benchmarks in CI

## Conclusion

This roadmap provides a path to transform the cparser library from a prototype-quality parser combinator framework to a production-ready, high-performance parsing solution. The phased approach allows for incremental value delivery while managing risk.

**Recommended Start**: Begin with Phase 1 (Memory Pool Allocator + Combinator Registry) as these provide immediate value with manageable complexity.

**Expected Outcome After Phase 2**: 
- 2-3x performance improvement
- Zero memory leaks
- Production-ready for files <10K lines

**Full Implementation**: Would bring performance close to hand-written recursive descent parsers while maintaining the elegance and flexibility of combinator approach.
