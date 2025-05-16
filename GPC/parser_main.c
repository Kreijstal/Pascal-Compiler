#include <stdio.h>
#include <stdlib.h>
#include "Parser/ParsePascal.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/ErrVars.h"

/* Global variables needed by parser */
int line_num;
int col_num;
Tree_t *parse_tree;
char *file_to_parse;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <pascal_file.p>\n", argv[0]);
        return 1;
    }

    // Parse the input file
    Tree_t *parseTree = ParsePascal(argv[1]);
    if (!parseTree) {
        fprintf(stderr, "Error parsing file\n");
        return 1;
    }

    // Print parse tree (or could do other analysis)
    printf("Parse successful. Parse tree:\n");
    tree_print(parseTree, stdout, 0);

    // Cleanup
    destroy_tree(parseTree);

    return 0;
}