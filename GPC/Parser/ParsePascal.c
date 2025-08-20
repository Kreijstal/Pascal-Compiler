/*
    Damon Gwinn
    Runs the pascal parser on the given file
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ParsePascal.h"
#include "ErrVars.h"
#include "ParseTree/tree.h"
#include "ParseTree/tree_types.h"
#include "List/List.h"
#include "SemanticCheck/SemCheck.h"
#include "Grammar.tab.h"

extern FILE *yyin;
extern int yyparse();

/* Initializes parser globals */
void InitParser();

Tree_t *ParsePascalOnly(char *file)
{
    assert(file != NULL);
    /**** CREATING THE PARSE TREE ****/
    if(file != NULL)
    {
        yyin = fopen(file, "r");
        if(yyin == NULL)
        {
            fprintf(stderr, "Error opening file: %s\n", file);
            exit(1);
        }
        file_to_parse = file;
    }
    else
        file_to_parse = NULL;

    InitParser();
    yyparse();

    #ifdef DEBUG_BISON
        if(parse_tree != NULL)
            tree_print(parse_tree, stderr, 0);
    #endif

    return parse_tree;
}

Tree_t *ParsePascal(char *file)
{
    assert(file != NULL);
    int semcheck_return;
    Tree_t *tree = ParsePascalOnly(file);

    /**** SEMANTIC CHECKING ****/
    #ifndef PARSER_ONLY
    if(tree != NULL)
    {
        int sem_result;
        SymTab_t *symtab = start_semcheck(tree, &sem_result);
        semcheck_return = sem_result;
        DestroySymTab(symtab);

        if(semcheck_return > 0)
        {
            destroy_tree(tree);
            tree = NULL;
        }
    }
    #endif

    return tree;
}

void InitParser()
{
    line_num = 1;
    col_num = 1;
    parse_tree = NULL;
}
