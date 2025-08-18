#include <stdio.h>
#include <string.h>
#include "Parser/ParsePascal.h"
#include "debug_flat_ast_printer.h"

extern int yydebug;

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: flat_ast_printer <input file>\n");
        return 1;
    }

    char *input_file = NULL;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--debug-parser") == 0) {
            yydebug = 1;
        } else {
            input_file = argv[i];
        }
    }

    if (input_file == NULL) {
        fprintf(stderr, "No input file specified.\n");
        return 1;
    }

    FlatNode *ast = ParsePascal(input_file);

    if (ast != NULL) {
        print_flat_ast(ast, stdout);
    } else {
        fprintf(stderr, "Parsing failed.\n");
        return 1;
    }

    return 0;
}
