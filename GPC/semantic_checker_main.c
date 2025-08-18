#include <stdio.h>
#include "Parser/ParsePascal.h"
#include "Parser/SemanticCheck/flat_sem_check.h"

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: semantic_checker <input file>\n");
        return 1;
    }

    char *input_file = argv[1];
    FlatNode *ast = ParsePascal(input_file);

    if (ast == NULL) {
        fprintf(stderr, "Parsing failed.\n");
        return 1;
    }

    printf("Parsing successful. Running semantic check...\n");
    int errors = sem_check(ast);

    if (errors > 0) {
        printf("Semantic check found %d error(s).\n", errors);
        return 1;
    }

    printf("Semantic check successful.\n");
    return 0;
}
