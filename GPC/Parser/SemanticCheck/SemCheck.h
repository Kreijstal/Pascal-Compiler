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
 * format: printf-style format string for error message
 * ...: arguments for format string
 */
void semantic_error(int line_num, const char *format, ...);

#endif
