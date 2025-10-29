/* Benchmark parser initialization overhead */
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"

static double get_wall_time() {
    struct timeval time;
    if (gettimeofday(&time, NULL)) {
        return 0;
    }
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

int main() {
    printf("Benchmarking parser initialization times...\n\n");
    
    // Benchmark expression parser init
    double start = get_wall_time();
    for (int i = 0; i < 100; i++) {
        combinator_t* p = new_combinator();
        init_pascal_expression_parser(&p);
        free_combinator(p);
    }
    double end = get_wall_time();
    printf("Expression parser: %.6f seconds per init (100 iterations)\n", (end - start) / 100.0);
    
    // Benchmark statement parser init
    start = get_wall_time();
    for (int i = 0; i < 10; i++) {
        combinator_t* p = new_combinator();
        init_pascal_statement_parser(&p);
        free_combinator(p);
    }
    end = get_wall_time();
    printf("Statement parser:  %.6f seconds per init (10 iterations)\n", (end - start) / 10.0);
    
    // Benchmark unit parser init
    start = get_wall_time();
    for (int i = 0; i < 3; i++) {
        combinator_t* p = new_combinator();
        init_pascal_unit_parser(&p);
        free_combinator(p);
    }
    end = get_wall_time();
    printf("Unit parser:       %.6f seconds per init (3 iterations)\n", (end - start) / 3.0);
    
    return 0;
}
