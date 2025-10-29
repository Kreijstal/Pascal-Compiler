/*
 * Benchmark version of pascal_tests.c - measures individual test performance
 * This is a minimal wrapper around the original tests to add timing
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

// Define TEST_NO_MAIN to prevent acutest.h from defining main()
#define TEST_NO_MAIN
// Include the test functions and infrastructure
#include "acutest.h"
#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "pascal_preprocessor.h"
#include "pascal_keywords.h"

// Test list is defined in pascal_tests.c, and declared in acutest.h
// We just need to link against it

// Timing utilities
static double get_wall_time() {
    struct timeval time;
    if (gettimeofday(&time, NULL)) {
        return 0;
    }
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

typedef struct {
    const char* name;
    double duration;
} TestTiming;

static int compare_timings(const void* a, const void* b) {
    const TestTiming* ta = (const TestTiming*)a;
    const TestTiming* tb = (const TestTiming*)b;
    if (tb->duration > ta->duration) return 1;
    if (tb->duration < ta->duration) return -1;
    return 0;
}

int main(int argc, char** argv) {
    // Count tests
    int test_count = 0;
    for (int i = 0; acutest_list_[i].name != NULL; i++) {
        test_count++;
    }
    
    // Allocate timing array
    TestTiming* timings = (TestTiming*)malloc(test_count * sizeof(TestTiming));
    if (!timings) {
        fprintf(stderr, "Failed to allocate memory for timings\n");
        return 1;
    }
    
    printf("Running %d parser tests with timing...\n", test_count);
    printf("========================================\n\n");
    
    double total_time = 0.0;
    int test_idx = 0;
    
    // Run each test with timing
    for (int i = 0; acutest_list_[i].name != NULL; i++) {
        const char* test_name = acutest_list_[i].name;
        void (*test_func)(void) = acutest_list_[i].func;
        
        printf("Running: %-60s ", test_name);
        fflush(stdout);
        
        double start = get_wall_time();
        test_func();
        double end = get_wall_time();
        double duration = end - start;
        
        timings[test_idx].name = test_name;
        timings[test_idx].duration = duration;
        test_idx++;
        
        total_time += duration;
        
        printf("%.4f s\n", duration);
    }
    
    printf("\n========================================\n");
    printf("Total time: %.4f seconds\n", total_time);
    printf("Average: %.4f seconds per test\n", total_time / test_count);
    printf("\n");
    
    // Sort by duration (slowest first)
    qsort(timings, test_count, sizeof(TestTiming), compare_timings);
    
    // Print top 20 slowest tests
    printf("TOP 20 SLOWEST TESTS:\n");
    printf("========================================\n");
    int show_count = test_count < 20 ? test_count : 20;
    for (int i = 0; i < show_count; i++) {
        printf("%2d. %-60s %.4f s (%.1f%%)\n", 
               i + 1,
               timings[i].name, 
               timings[i].duration,
               (timings[i].duration / total_time) * 100.0);
    }
    
    printf("\n");
    
    // Print bottom 10 fastest tests
    printf("BOTTOM 10 FASTEST TESTS:\n");
    printf("========================================\n");
    int start_idx = test_count - 10;
    if (start_idx < 0) start_idx = 0;
    for (int i = start_idx; i < test_count; i++) {
        printf("%-60s %.6f s\n", timings[i].name, timings[i].duration);
    }
    
    free(timings);
    
    printf("\nBenchmark complete.\n");
    return 0;
}
