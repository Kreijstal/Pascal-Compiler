#ifndef EXPR_GEN_H
#define EXPR_GEN_H

#include <stdio.h>
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"

// Function prototypes for expression generation
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                FILE *o_file, int *relop_type);

#endif // EXPR_GEN_H
