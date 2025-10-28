/*
    Damon Gwinn
    Runs the pascal parser on the given file
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ErrVars.h"
#include "ParsePascal.h"
#include "ParseTree/tree.h"
#include "ParseTree/tree_types.h"
#include "SemanticCheck/SemCheck.h"
#include "../../GPC/Parser/pascal_frontend.h"

static void InitParser();

Tree_t *ParsePascalOnly(char *file)
{
    assert(file != NULL);

    InitParser();

    Tree_t *tree = NULL;
    ParseError *error = NULL;
    if (!pascal_parse_source(file, true, &tree, &error))
    {
        pascal_print_parse_error(file, error);
        if (error != NULL)
            free_error(error);
        return NULL;
    }

    parse_tree = tree;
    return tree;
}

Tree_t *ParsePascal(char *file)
{
    assert(file != NULL);

    Tree_t *tree = ParsePascalOnly(file);
    if (tree == NULL)
        return NULL;

    int sem_result = 0;
    SymTab_t *symtab = start_semcheck(tree, &sem_result);
    DestroySymTab(symtab);

    if (sem_result > 0)
    {
        destroy_tree(tree);
        parse_tree = NULL;
        return NULL;
    }

    return tree;
}

static void InitParser()
{
    line_num = 1;
    col_num = 1;
    parse_tree = NULL;
    file_to_parse = NULL;
}
