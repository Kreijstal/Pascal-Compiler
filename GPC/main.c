#include <stdio.h>
#include "Parser/ParsePascal.h"
#include "flags.h"
#include <stdio.h>
#include "Parser/ParsePascal.h"

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Usage: gpc <input file> [output file]\n");
        return 1;
    }

    // TODO: Handle flags properly
    init_flags();

    FlatNode *ast = ParsePascal(argv[1]);

    if (ast == NULL) {
        fprintf(stderr, "Parsing failed.\n");
        return 1;
    }

    // TODO: Add semantic analysis and codegen calls here

    printf("Compiler finished (no code generation yet).\n");

    return 0;
}
