/*
    Reachability analysis for subprograms
    Marks which functions are actually used starting from the main program
*/

#ifndef MARK_USED_H
#define MARK_USED_H

#include "Parser/ParseTree/tree.h"
#include "Parser/SemanticCheck/SymTab/SymTab.h"

/* Perform reachability analysis and mark used subprograms */
void mark_used_functions(Tree_t *program, SymTab_t *symtab);

#endif
