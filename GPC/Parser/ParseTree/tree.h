#ifndef TREE_H
#define TREE_H

#include "../flat_ast.h"
#include <stdio.h>

/* Helper prototypes for grammar */
VarDecl_t *mk_var_decl(ListNode_t *id_list, Type_t *type);
VarDecl_t *ChainVarDecl(VarDecl_t *head, VarDecl_t *tail);
TypeDecl_t *mk_type_decl(char *id, Type_t *type);
Type_t *mk_type(BaseType base_type, int array_start, int array_end, Type_t *array_type);
Param_t *mk_param(ListNode_t *id_list, BaseType type, int pass_by_ref);


/* Flat AST prototypes */
FlatNode *mk_flat_program(int line_num, char *id, ListNode_t *args, VarDecl_t *declarations, ListNode_t *subprograms, FlatNode *compound_statement);
FlatNode *mk_flat_procedure(int line_num, char *id, ListNode_t *params, VarDecl_t *local_vars,
    ListNode_t *subprograms, FlatNode *compound_statement, int cname_flag, int overload_flag);
FlatNode *mk_flat_function(int line_num, char *id, ListNode_t *params, VarDecl_t *local_vars,
    ListNode_t *subprograms, FlatNode *compound_statement, int return_type, char *return_type_id, int cname_flag, int overload_flag);
FlatNode *mk_flat_compoundstatement(int line_num, ListNode_t *stmt_list);
FlatNode *mk_flat_while(int line_num, FlatNode *condition, FlatNode *while_stmt);
FlatNode *mk_flat_for(int line_num, FlatNode *for_assign, FlatNode *to_expr, FlatNode *step_expr, FlatNode *stmt);
FlatNode *mk_flat_ifthen(int line_num, FlatNode *condition, FlatNode *then_stmt, FlatNode *else_stmt);
FlatNode *mk_flat_varassign(int line_num, FlatNode *var, FlatNode *expr);
FlatNode *mk_flat_procedurecall(int line_num, char *id, ListNode_t *args);
FlatNode *mk_flat_varid(int line_num, char *id);
FlatNode *mk_flat_arrayaccess(int line_num, char *id, FlatNode *index_expr);
FlatNode *mk_flat_functioncall(int line_num, char *id, ListNode_t *args);
FlatNode *mk_flat_relop(int line_num, int op, FlatNode *left, FlatNode *right);
FlatNode *mk_flat_addop(int line_num, int op, FlatNode *left, FlatNode *right);
FlatNode *mk_flat_mulop(int line_num, int op, FlatNode *left, FlatNode *right);
FlatNode *mk_flat_inum(int line_num, int val);
FlatNode *mk_flat_rnum(int line_num, float val);
FlatNode *mk_flat_string(int line_num, char *str);
FlatNode *mk_flat_unop(int line_num, OpType op, FlatNode *operand);
FlatNode *mk_flat_asm_block(int line_num, char *code);

#endif
