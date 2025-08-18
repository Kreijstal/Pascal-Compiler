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
/*#include "SemanticCheck/SemCheck.h"*/
#include "Grammar.tab.h"
#include "flat_ast.h"

extern FILE *yyin;
extern int yyparse();

/* Initializes parser globals */
void InitParser();

// Global that holds the root of the parse tree
FlatNode *parse_tree = NULL;

// Globals needed by lexer/parser
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;

FlatNode *ParsePascalOnly(char *file)
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
        // TODO: Add a flat_ast printer
        /*if(parse_tree != NULL)
            tree_print(parse_tree, stderr, 0);*/
    #endif

    return parse_tree;
}

FlatNode *ParsePascal(char *file)
{
    assert(file != NULL);
    /*int semcheck_return;*/
    FlatNode *tree = ParsePascalOnly(file);

    /**** SEMANTIC CHECKING ****/
    // TODO: Re-implement semantic checker for flat AST
    /*if(tree != NULL)
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
    }*/

    return tree;
}

void InitParser()
{
    line_num = 1;
    col_num = 1;
    parse_tree = NULL;
}
