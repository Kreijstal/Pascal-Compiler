/*
 * Benchmark infrastructure for GPC compiler
 * Provides detailed timing and performance profiling for parser and semantic checker
 */

#ifndef GPC_BENCHMARK_H
#define GPC_BENCHMARK_H

#include <stddef.h>
#include <stdbool.h>

/* Performance counter for tracking function/phase execution */
typedef struct {
    const char *name;
    double total_time;
    unsigned long call_count;
    double min_time;
    double max_time;
} PerfCounter;

/* Phase names for benchmark reporting */
typedef enum {
    BENCH_PHASE_PARSE_TOTAL,
    BENCH_PHASE_PARSE_LEXING,
    BENCH_PHASE_PARSE_PROGRAM,
    BENCH_PHASE_PARSE_DECLARATIONS,
    BENCH_PHASE_PARSE_STATEMENTS,
    BENCH_PHASE_PARSE_EXPRESSIONS,
    BENCH_PHASE_PARSE_TYPES,
    BENCH_PHASE_SEMANTIC_TOTAL,
    BENCH_PHASE_SEMANTIC_SYMTAB_BUILD,
    BENCH_PHASE_SEMANTIC_TYPE_CHECK,
    BENCH_PHASE_SEMANTIC_EXPR_CHECK,
    BENCH_PHASE_SEMANTIC_STMT_CHECK,
    BENCH_PHASE_SEMANTIC_LOOKUP,
    BENCH_PHASE_COUNT
} BenchmarkPhase;

/* Initialize benchmarking system */
void benchmark_init(void);

/* Cleanup benchmarking system */
void benchmark_cleanup(void);

/* Enable/disable benchmarking */
void benchmark_enable(bool enable);
bool benchmark_is_enabled(void);

/* Start timing a phase */
void benchmark_start_phase(BenchmarkPhase phase);

/* End timing a phase */
void benchmark_end_phase(BenchmarkPhase phase);

/* Record a single operation (for frequently called functions) */
void benchmark_record_operation(BenchmarkPhase phase, double elapsed_time);

/* Get current high-resolution time in seconds */
double benchmark_get_time(void);

/* Print benchmark summary to stderr */
void benchmark_print_summary(void);

/* Get phase name for reporting */
const char* benchmark_phase_name(BenchmarkPhase phase);

/* Helper macros for easy instrumentation */
#define BENCHMARK_START(phase) benchmark_start_phase(BENCH_PHASE_##phase)
#define BENCHMARK_END(phase) benchmark_end_phase(BENCH_PHASE_##phase)

/* Scoped timing helper */
typedef struct {
    BenchmarkPhase phase;
    double start_time;
} BenchmarkScope;

BenchmarkScope benchmark_scope_start(BenchmarkPhase phase);
void benchmark_scope_end(BenchmarkScope *scope);

/* RAII-style macro for C (requires compound statement) */
#define BENCHMARK_SCOPE(phase) \
    BenchmarkScope _bench_scope_##phase __attribute__((cleanup(benchmark_scope_end))) = \
        benchmark_scope_start(BENCH_PHASE_##phase)

/* Function call counting (for bottleneck identification) */
void benchmark_count_call(const char *function_name);
void benchmark_print_call_counts(void);

#endif /* GPC_BENCHMARK_H */
