/*
 * mark_used.h
 * 
 * Reachability analysis to mark used functions/procedures in the AST.
 * This enables the code generator to skip emitting unused standard library functions.
 */

#ifndef MARK_USED_H
#define MARK_USED_H

#include "../Parser/ParseTree/tree.h"
#include "../Parser/SemanticCheck/SymTab/SymTab.h"

/*
 * Mark all functions and procedures that are reachable from the main program.
 * This performs a depth-first traversal starting from the main program body,
 * following all function/procedure calls and marking them as used.
 * 
 * @param program: The program AST node
 * @param symtab: The symbol table containing all identifiers
 */
void mark_used_functions(Tree_t *program, SymTab_t *symtab);

#endif /* MARK_USED_H */
