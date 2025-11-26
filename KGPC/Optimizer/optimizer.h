/*
    Damon Gwinn
    Simple optimizer for a parse tree

    OPTIMIZER INFO:
        - All non-referenced stack variables removed
        - All constant number expressions simplified to a single number

    WARNING: Optimizer designed to work in unison with the parser
*/

#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "../Parser/ParseTree/tree.h"
#include "../Parser/ParseTree/tree_types.h"
#include "../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../Parser/SemanticCheck/HashTable/HashTable.h"

typedef void (*optimizer_runner_fn)(SymTab_t *, Tree_t *);

void optimizer_set_runner(optimizer_runner_fn runner);
void optimize(SymTab_t *, Tree_t *);
void optimizer_pass_constant_folding(SymTab_t *symtab, Tree_t *tree);
void optimizer_pass_dead_code_elimination(SymTab_t *symtab, Tree_t *tree);

#endif
