#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"

int main() {
    combinator_t* p = new_combinator();
    init_pascal_unit_parser(&p);

    input_t* input = new_input();
    char* unit_code = "unit MyUnit;\n"
                      "interface\n"
                      "  procedure DoSomething;\n"
                      "implementation\n"
                      "  procedure DoSomething;\n"
                      "  begin\n"
                      "  end;\n"
                      "begin\n"
                      "  DoSomething;\n"
                      "end.\n";
    input->buffer = strdup(unit_code);
    input->length = strlen(unit_code);

    printf("--- Running Isolated Test ---\n");
    ParseResult res = parse(input, p);

    printf("\n--- Parser Result ---\n");
    if (res.is_success) {
        printf("Parsing succeeded.\n");
        free_ast(res.value.ast);
    } else {
        printf("Parsing failed.\n");
        /* HARDENED: Removed NULL check. An error is guaranteed on failure. */
        printf("  Parser name: %s\n", res.value.error->parser_name ? res.value.error->parser_name : "N/A");
        printf("  Error: %s\n", res.value.error->message);
            if (res.value.error->unexpected) {
                printf("  Unexpected input: '%.10s...'\n", res.value.error->unexpected);
            }
            printf("  at line: %d, col: %d\n", res.value.error->line, res.value.error->col);
            free_error(res.value.error);
        }

    free_combinator(p);
    free(input->buffer);
    free(input);

    return 0;
}
