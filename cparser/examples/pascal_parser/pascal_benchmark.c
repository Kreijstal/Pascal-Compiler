#include <stdio.h>
#include <time.h>

#include "pascal_parser.h"

static double timespec_diff_sec(struct timespec start, struct timespec end) {
    double seconds = (double)(end.tv_sec - start.tv_sec);
    double nanoseconds = (double)(end.tv_nsec - start.tv_nsec) / 1e9;
    return seconds + nanoseconds;
}

static double benchmark_unit_parser(int iterations) {
    double total_seconds = 0.0;

    for (int i = 0; i < iterations; ++i) {
        struct timespec start;
        struct timespec end;
        combinator_t* parser = new_combinator();

        if (clock_gettime(CLOCK_MONOTONIC, &start) != 0) {
            perror("clock_gettime");
            free_combinator(parser);
            return -1.0;
        }

        init_pascal_unit_parser(&parser);

        if (clock_gettime(CLOCK_MONOTONIC, &end) != 0) {
            perror("clock_gettime");
            free_combinator(parser);
            return -1.0;
        }

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
        struct timespec start;
        struct timespec end;
        combinator_t* parser = new_combinator();

        if (clock_gettime(CLOCK_MONOTONIC, &start) != 0) {
            perror("clock_gettime");
            free_combinator(parser);
            return -1.0;
        }

        init_pascal_statement_parser(&parser);

        if (clock_gettime(CLOCK_MONOTONIC, &end) != 0) {
            perror("clock_gettime");
            free_combinator(parser);
            return -1.0;
        }

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
