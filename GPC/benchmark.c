/*
 * Benchmark infrastructure implementation
 */

#include "benchmark.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/time.h>
#endif

/* Global state */
static bool g_benchmark_enabled = false;
static PerfCounter g_phase_counters[BENCH_PHASE_COUNT];
static double g_phase_start_times[BENCH_PHASE_COUNT];

/* Call counting infrastructure */
#define MAX_TRACKED_FUNCTIONS 1000
typedef struct {
    char *name;
    unsigned long count;
} CallCounter;

static CallCounter g_call_counters[MAX_TRACKED_FUNCTIONS];
static size_t g_call_counter_count = 0;

static const char* g_phase_names[BENCH_PHASE_COUNT] = {
    "Parse Total",
    "Parse: Lexing",
    "Parse: Program Structure",
    "Parse: Declarations",
    "Parse: Statements",
    "Parse: Expressions",
    "Parse: Types",
    "Semantic Total",
    "Semantic: Symbol Table Build",
    "Semantic: Type Checking",
    "Semantic: Expression Checking",
    "Semantic: Statement Checking",
    "Semantic: Symbol Lookups"
};

double benchmark_get_time(void)
{
#ifdef _WIN32
    LARGE_INTEGER freq;
    LARGE_INTEGER counter;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&counter);
    return (double)counter.QuadPart / (double)freq.QuadPart;
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
        return ts.tv_sec + ts.tv_nsec / 1e9;
    
    /* Fallback to gettimeofday */
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + tv.tv_usec / 1e6;
#endif
}

void benchmark_init(void)
{
    memset(g_phase_counters, 0, sizeof(g_phase_counters));
    memset(g_phase_start_times, 0, sizeof(g_phase_start_times));
    memset(g_call_counters, 0, sizeof(g_call_counters));
    g_call_counter_count = 0;
    
    for (int i = 0; i < BENCH_PHASE_COUNT; i++) {
        g_phase_counters[i].name = g_phase_names[i];
        g_phase_counters[i].min_time = INFINITY;
        g_phase_counters[i].max_time = 0.0;
    }
}

void benchmark_cleanup(void)
{
    for (size_t i = 0; i < g_call_counter_count; i++) {
        if (g_call_counters[i].name != NULL) {
            free(g_call_counters[i].name);
            g_call_counters[i].name = NULL;
        }
    }
    g_call_counter_count = 0;
}

void benchmark_enable(bool enable)
{
    g_benchmark_enabled = enable;
}

bool benchmark_is_enabled(void)
{
    return g_benchmark_enabled;
}

void benchmark_start_phase(BenchmarkPhase phase)
{
    if (!g_benchmark_enabled || phase >= BENCH_PHASE_COUNT)
        return;
    
    g_phase_start_times[phase] = benchmark_get_time();
}

void benchmark_end_phase(BenchmarkPhase phase)
{
    if (!g_benchmark_enabled || phase >= BENCH_PHASE_COUNT)
        return;
    
    double end_time = benchmark_get_time();
    double elapsed = end_time - g_phase_start_times[phase];
    
    PerfCounter *counter = &g_phase_counters[phase];
    counter->total_time += elapsed;
    counter->call_count++;
    
    if (elapsed < counter->min_time)
        counter->min_time = elapsed;
    if (elapsed > counter->max_time)
        counter->max_time = elapsed;
}

void benchmark_record_operation(BenchmarkPhase phase, double elapsed_time)
{
    if (!g_benchmark_enabled || phase >= BENCH_PHASE_COUNT)
        return;
    
    PerfCounter *counter = &g_phase_counters[phase];
    counter->total_time += elapsed_time;
    counter->call_count++;
    
    if (elapsed_time < counter->min_time)
        counter->min_time = elapsed_time;
    if (elapsed_time > counter->max_time)
        counter->max_time = elapsed_time;
}

const char* benchmark_phase_name(BenchmarkPhase phase)
{
    if (phase >= BENCH_PHASE_COUNT)
        return "Unknown Phase";
    return g_phase_names[phase];
}

void benchmark_print_summary(void)
{
    if (!g_benchmark_enabled)
        return;
    
    fprintf(stderr, "\n");
    fprintf(stderr, "================================================================================\n");
    fprintf(stderr, "                         PERFORMANCE BENCHMARK SUMMARY\n");
    fprintf(stderr, "================================================================================\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "%-40s %10s %12s %12s %12s %12s\n",
            "Phase", "Calls", "Total (s)", "Avg (ms)", "Min (ms)", "Max (ms)");
    fprintf(stderr, "--------------------------------------------------------------------------------\n");
    
    for (int i = 0; i < BENCH_PHASE_COUNT; i++) {
        PerfCounter *counter = &g_phase_counters[i];
        if (counter->call_count == 0)
            continue;
        
        double avg_ms = (counter->total_time / counter->call_count) * 1000.0;
        double min_ms = counter->min_time * 1000.0;
        double max_ms = counter->max_time * 1000.0;
        
        fprintf(stderr, "%-40s %10lu %12.6f %12.6f %12.6f %12.6f\n",
                counter->name,
                counter->call_count,
                counter->total_time,
                avg_ms,
                min_ms,
                max_ms);
    }
    
    fprintf(stderr, "================================================================================\n");
    fprintf(stderr, "\n");
}

BenchmarkScope benchmark_scope_start(BenchmarkPhase phase)
{
    BenchmarkScope scope;
    scope.phase = phase;
    scope.start_time = benchmark_get_time();
    return scope;
}

void benchmark_scope_end(BenchmarkScope *scope)
{
    if (!g_benchmark_enabled || scope == NULL)
        return;
    
    double elapsed = benchmark_get_time() - scope->start_time;
    benchmark_record_operation(scope->phase, elapsed);
}

void benchmark_count_call(const char *function_name)
{
    if (!g_benchmark_enabled || function_name == NULL)
        return;
    
    /* Find existing counter */
    for (size_t i = 0; i < g_call_counter_count; i++) {
        if (g_call_counters[i].name != NULL &&
            strcmp(g_call_counters[i].name, function_name) == 0) {
            g_call_counters[i].count++;
            return;
        }
    }
    
    /* Add new counter */
    if (g_call_counter_count < MAX_TRACKED_FUNCTIONS) {
        g_call_counters[g_call_counter_count].name = strdup(function_name);
        g_call_counters[g_call_counter_count].count = 1;
        g_call_counter_count++;
    }
}

/* Comparison function for sorting call counters */
static int compare_call_counters(const void *a, const void *b)
{
    const CallCounter *ca = (const CallCounter *)a;
    const CallCounter *cb = (const CallCounter *)b;
    
    /* Sort by count (descending) */
    if (cb->count > ca->count) return 1;
    if (cb->count < ca->count) return -1;
    return 0;
}

void benchmark_print_call_counts(void)
{
    if (!g_benchmark_enabled || g_call_counter_count == 0)
        return;
    
    /* Sort by call count */
    qsort(g_call_counters, g_call_counter_count, sizeof(CallCounter), compare_call_counters);
    
    fprintf(stderr, "\n");
    fprintf(stderr, "================================================================================\n");
    fprintf(stderr, "                         FUNCTION CALL FREQUENCY\n");
    fprintf(stderr, "================================================================================\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "%-60s %15s\n", "Function", "Call Count");
    fprintf(stderr, "--------------------------------------------------------------------------------\n");
    
    /* Show top 50 most called functions */
    size_t limit = (g_call_counter_count > 50) ? 50 : g_call_counter_count;
    for (size_t i = 0; i < limit; i++) {
        if (g_call_counters[i].name != NULL) {
            fprintf(stderr, "%-60s %15lu\n",
                    g_call_counters[i].name,
                    g_call_counters[i].count);
        }
    }
    
    if (g_call_counter_count > 50) {
        fprintf(stderr, "... (%zu more functions)\n", g_call_counter_count - 50);
    }
    
    fprintf(stderr, "================================================================================\n");
    fprintf(stderr, "\n");
}
