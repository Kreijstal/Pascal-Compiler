#ifndef TREE_H
#define TREE_H

#include "tree_types.h"
#include "../flat_ast.h"
#include <stdio.h>

/* Prototypes */
void tree_print(Tree_t *tree, FILE *f, int num_indent);
void stmt_print(struct Statement *stmt, FILE *f, int num_indent);
void expr_print(struct Expression *expr, FILE *f, int num_indent);
void list_print(ListNode_t *list, FILE *f, int num_indent);

void destroy_tree(Tree_t *tree);
void destroy_stmt(struct Statement *stmt);
void destroy_expr(struct Expression *expr);
void destroy_list(ListNode_t *list);

/* Flat AST prototypes */
FlatNode *mk_flat_program(int line_num, char *id, VarDecl_t *declarations, ListNode_t *subprograms, FlatNode *compound_statement);
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


/* Old AST Prototypes */
Tree_t *mk_program(int line_num, char *id, ListNode_t *args, ListNode_t *var_decl,
    ListNode_t *type_decl, ListNode_t *subprograms, struct Statement *compound_statement);
struct Statement *mk_compoundstatement(int line_num, ListNode_t *stmt_list);
struct Statement *mk_while(int line_num, struct Expression *condition, struct Statement *while_stmt);
struct Statement *mk_forassign(int line_num, struct Statement *for_assign, struct Expression *to,
                               struct Statement *do_for);
struct Statement *mk_forvar(int line_num, struct Expression *for_var, struct Expression *to,
                              struct Statement *do_for);
struct Statement *mk_ifthen(int line_num, struct Expression *condition, struct Statement *then_stmt, struct Statement *else_stmt);
struct Statement *mk_varassign(int line_num, FlatNode *var, FlatNode *expr);
struct Statement *mk_procedurecall(int line_num, char *id, ListNode_t *args);
struct Expression *mk_varid(int line_num, char *id);
struct Expression *mk_arrayaccess(int line_num, char *id, struct Expression *index_expr);
struct Expression *mk_relop(int line_num, int op, struct Expression *left, struct Expression *right);
struct Expression *mk_addop(int line_num, int op, struct Expression *left, struct Expression *right);
struct Expression *mk_mulop(int line_num, int op, struct Expression *left, struct Expression *right);
struct Expression *mk_functioncall(int line_num, char *id, ListNode_t *args);
struct Expression *mk_signterm(int line_num, struct Expression *sign_term);
struct Expression *mk_inum(int line_num, int val);
struct Expression *mk_rnum(int line_num, float val);
struct Expression *mk_string(int line_num, char *str);
Tree_t *mk_typedecl(int line_num, char *id, int start, int end);
Tree_t *mk_vardecl(int line_num, ListNode_t *ids, int type, char *type_id, int is_var_param);
Tree_t *mk_arraydecl(int line_num, ListNode_t *ids, int type, int start, int end);
Tree_t *mk_procedure(int line_num, char *id, ListNode_t *args, ListNode_t *var_decl,
    ListNode_t *subprograms, struct Statement *compound_statement, int cname_flag, int overload_flag);
Tree_t *mk_function(int line_num, char *id, ListNode_t *args, ListNode_t *var_decl,
    ListNode_t *subprograms, struct Statement *compound_statement, int return_type, char *return_type_id, int cname_flag, int overload_flag);

#endif
