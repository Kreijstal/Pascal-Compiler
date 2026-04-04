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

/* Full mark-used analysis: builds the map once, marks program-level subprograms
 * as used, performs reachability with an internal fixpoint loop, and reconciles
 * forward declarations.  Replaces the old pattern of calling mark_used_functions
 * twice with mark_program_subs_used in between. */
void mark_used_functions_full(Tree_t *program, SymTab_t *symtab);

#endif
