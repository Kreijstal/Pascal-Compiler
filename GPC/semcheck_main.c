#include <stdio.h>
#include <stdlib.h>
#include "Parser/ParsePascal.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/SemanticCheck/SemCheck.h"

// Global variables that are defined in main.c
Tree_t *parse_tree = NULL;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
int num_args_alloced = 0;

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "USAGE: %s [INPUT_FILE]\n", argv[0]);
        return 1;
    }

    file_to_parse = argv[1];
    parse_tree = ParsePascalOnly(file_to_parse);

    if (parse_tree != NULL)
    {
        int sem_result;
        SymTab_t *symtab = start_semcheck(parse_tree, &sem_result);
        if(sem_result > 0)
        {
            fprintf(stderr, "Semantic check failed with %d error(s)\n", sem_result);
            DestroySymTab(symtab);
            destroy_tree(parse_tree);
            return 1;
        }
        DestroySymTab(symtab);
        destroy_tree(parse_tree);
    }
    else
    {
        fprintf(stderr, "Failed to parse %s\n", file_to_parse);
        return 1;
    }

    return 0;
}
