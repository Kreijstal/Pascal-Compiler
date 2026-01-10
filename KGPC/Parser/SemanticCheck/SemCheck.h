/*
    Damon Gwinn
    Performs semantic checking on a given parse tree
*/

#ifndef SEM_CHECK_H
#define SEM_CHECK_H

#include "../ParseTree/tree.h"
#include "SymTab/SymTab.h"

/* The main function for checking a tree */
/* Return values:
    0       -> Check successful
    -1      -> Check successful with warnings
    >= 1    -> Check failed with n errors
*/
SymTab_t *start_semcheck(Tree_t *parse_tree, int *sem_result);

/* Helper to print semantic error with source code context
 * line_num: line number where error occurred
 * col_num: column number where error occurred (0 if unknown)
 * format: printf-style format string for error message
 * ...: arguments for format string
 */
void semantic_error(int line_num, int col_num, const char *format, ...);
void semcheck_error_with_context(const char *format, ...);

HashNode_t *semcheck_find_type_node_with_kgpc_type(SymTab_t *symtab, const char *type_id);
int semcheck_is_unit_name(const char *name);

#endif
