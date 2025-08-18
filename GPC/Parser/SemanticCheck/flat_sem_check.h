#ifndef FLAT_SEM_CHECK_H
#define FLAT_SEM_CHECK_H

#include "../flat_ast.h"
#include "SymTab/SymTab.h"

/*
 * The main entry point for the semantic checker.
 * Traverses the flat AST and performs semantic analysis.
 * Returns the number of semantic errors found.
 */
int sem_check(FlatNode *ast);

/*
 * The recursive workhorse of the semantic checker.
 * Traverses a node, performs checks, and returns the type of the node
 * if it's an expression. Returns NULL for statements.
 * Increments the error_count pointer for each error found.
 */
Type_t *sem_check_node(FlatNode *node, SymTab_t *symtab, int *error_count);

#endif // FLAT_SEM_CHECK_H
