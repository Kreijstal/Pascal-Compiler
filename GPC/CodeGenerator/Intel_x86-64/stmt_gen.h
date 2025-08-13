#ifndef STMT_GEN_H
#define STMT_GEN_H

#include <stdio.h>
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"

// Function prototypes for statement generation
ListNode_t *codegen_stmt(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_compound_stmt(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_var_assignment(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_proc_call(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_if_then(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_while(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);
ListNode_t *codegen_for(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file);

#endif // STMT_GEN_H
