#define UNW_LOCAL_ONLY
#include <libunwind.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parser.h"
#include "json_parser.h"

void backtrace_handler(int sig) {
    unw_cursor_t cursor;
    unw_context_t context;

    fprintf(stderr, "\n--- Caught signal %d, printing backtrace ---\n", sig);

    unw_getcontext(&context);
    unw_init_local(&cursor, &context);

    while (unw_step(&cursor) > 0) {
        unw_word_t offset, pc;
        char sym[256];

        unw_get_reg(&cursor, UNW_REG_IP, &pc);
        if (unw_get_proc_name(&cursor, sym, sizeof(sym), &offset) == 0) {
            fprintf(stderr, "  at %s (+0x%lx) [0x%lx]\n", sym, offset, pc);
        } else {
            fprintf(stderr, "  at <unknown> [0x%lx]\n", pc);
        }
    }
    fprintf(stderr, "------------------------------------------\n");
    exit(sig);
}

int main(int argc, char *argv[]) {
    signal(SIGSEGV, backtrace_handler);

    if (argc != 2) {
        fprintf(stderr, "Usage: %s \"<json_string>\"\n", argv[0]);
        return 1;
    }

    // --- Parser Definition ---
    combinator_t *parser = json_parser();

    // --- Parsing ---
    input_t *in = new_input();
    in->buffer = argv[1];
    in->length = strlen(argv[1]);

    ast_nil = new_ast();
    ast_nil->typ = JSON_T_NONE;

    ParseResult result = parse(in, parser);

    // --- Output ---
    if (result.is_success) {
        if (in->start < in->length) {
            fprintf(stderr, "Error: Parser did not consume entire input. Trailing characters: '%s'\n", in->buffer + in->start);
            free_ast(result.value.ast);
        } else {
            printf("JSON parsed successfully.\n");
            // AST is not printed because it can be very large.
            free_ast(result.value.ast);
        }
    } else {
        fprintf(stderr, "Parsing Error at line %d, col %d: ",
                result.value.error->line,
                result.value.error->col);
        if (result.value.error->parser_name) {
            fprintf(stderr, "In parser '%s': ", result.value.error->parser_name);
        }
        fprintf(stderr, "%s\n", result.value.error->message);

        if (result.value.error->unexpected) {
            fprintf(stderr, "Unexpected input: \"%s\"\n", result.value.error->unexpected);
        }
        free_error(result.value.error);
    }

    // --- Cleanup ---
    free_combinator(parser);
    free(in);
    free(ast_nil);

    return 0;
}
