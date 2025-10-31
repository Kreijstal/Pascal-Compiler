#include <stdio.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#endif

#include "pascal_parser.h"

#ifdef _WIN32
typedef struct {
    LARGE_INTEGER value;
} monotonic_time_t;

static void get_monotonic_time(monotonic_time_t *t) {
    QueryPerformanceCounter(&t->value);
}

static double timespec_diff_sec(monotonic_time_t start, monotonic_time_t end) {
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(&frequency);
    double elapsed = (double)(end.value.QuadPart - start.value.QuadPart);
    return elapsed / (double)frequency.QuadPart;
}
#else
typedef struct timespec monotonic_time_t;

static void get_monotonic_time(monotonic_time_t *t) {
    clock_gettime(CLOCK_MONOTONIC, t);
}

static double timespec_diff_sec(monotonic_time_t start, monotonic_time_t end) {
    double seconds = (double)(end.tv_sec - start.tv_sec);
    double nanoseconds = (double)(end.tv_nsec - start.tv_nsec) / 1e9;
    return seconds + nanoseconds;
}
#endif

static double benchmark_unit_parser(int iterations) {
    double total_seconds = 0.0;

    for (int i = 0; i < iterations; ++i) {
        monotonic_time_t start;
        monotonic_time_t end;
        combinator_t* parser = new_combinator();

        get_monotonic_time(&start);

        init_pascal_unit_parser(&parser);

        get_monotonic_time(&end);

        double elapsed = timespec_diff_sec(start, end);
        total_seconds += elapsed;

        printf("Unit parser iteration %d: %.6f seconds\n", i + 1, elapsed);

        free_combinator(parser);
    }

    return total_seconds / iterations;
}

static double benchmark_statement_parser(int iterations) {
    double total_seconds = 0.0;

    for (int i = 0; i < iterations; ++i) {
        monotonic_time_t start;
        monotonic_time_t end;
        combinator_t* parser = new_combinator();

        get_monotonic_time(&start);

        init_pascal_statement_parser(&parser);

        get_monotonic_time(&end);

        double elapsed = timespec_diff_sec(start, end);
        total_seconds += elapsed;

        printf("Statement parser iteration %d: %.6f seconds\n", i + 1, elapsed);

        free_combinator(parser);
    }

    return total_seconds / iterations;
}

int main(void) {
    const int iterations = 5;

    double unit_average = benchmark_unit_parser(iterations);
    if (unit_average < 0.0) {
        return 1;
    }

    double statement_average = benchmark_statement_parser(iterations);
    if (statement_average < 0.0) {
        return 1;
    }

    printf("\nAverage initialization time over %d runs:\n", iterations);
    printf("  Unit parser: %.6f seconds\n", unit_average);
    printf("  Statement parser: %.6f seconds\n", statement_average);

    return 0;
}
