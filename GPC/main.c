/* Damon Gwinn */
/* Where it all begins */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flags.h"
#include "Parser/ParseTree/tree.h"

/* Global variable definitions */
Tree_t *parse_tree = NULL;
int label_counter = 1;
int num_args_alloced = 0;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
#include "Parser/ParseTree/tree.h"
#include "Parser/ParsePascal.h"
#include "CodeGenerator/Intel_x86-64/codegen.h"

void set_flags(char **, int);

int main(int argc, char **argv)
{
    Tree_t * parse_tree;
    int required_args, args_left;

    required_args = 3;

    if(argc < required_args)
    {
        fprintf(stderr, "USAGE: [exec] [INPUT_FILE] [OUTPUT_FILE] [OPTIONAL_FLAG_1] ...\n");
        exit(1);
    }

    /* Setting flags */
    args_left = argc - required_args;
    if(args_left > 0)
    {
        set_flags(argv + required_args, args_left);
    }

    parse_tree = ParsePascal(argv[1]);
    if(parse_tree != NULL)
    {
        fprintf(stderr, "Generating code to file: %s\n", argv[2]);
        codegen(parse_tree, argv[1], argv[2]);

        destroy_tree(parse_tree);
        parse_tree = NULL;
    }
}

void set_flags(char **optional_args, int count)
{
    int i;

    i = 0;
    while(count > 0)
    {
        if(strcmp(optional_args[i], "-non-local") == 0)
        {
            fprintf(stderr, "Non-local codegen support enabled\n");
            fprintf(stderr, "WARNING: Non-local is still in development and is very buggy!\n\n");
            set_nonlocal_flag();
        }
        else if(strcmp(optional_args[i], "-O1") == 0)
        {
            fprintf(stderr, "O1 optimizations enabled!\n\n");
            set_o1_flag();
        }
        else if(strcmp(optional_args[i], "-O2") == 0)
        {
            fprintf(stderr, "O2 optimizations enabled!\n\n");
            set_o2_flag();
        }
        else
        {
            fprintf(stderr, "ERROR: Unrecognized flag: %s\n", optional_args[i]);
            exit(1);
        }

        --count;
        ++i;
    }
}
