/*
    Damon Gwinn
    Parse tree for the Pascal Grammar
*/

#include "tree.h"
#include "tree_types.h"
#include "../flat_ast.h"
#include "Grammar.tab.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* Helper functions for grammar */
VarDecl_t *mk_var_decl(ListNode_t *id_list, Type_t *type) {
    VarDecl_t *node = (VarDecl_t *)malloc(sizeof(VarDecl_t));
    assert(node != NULL);
    node->id_list = id_list;
    node->type = type;
    node->next = NULL;
    return node;
}

TypeDecl_t *mk_type_decl(char *id, Type_t *type) {
    TypeDecl_t *node = (TypeDecl_t *)malloc(sizeof(TypeDecl_t));
    assert(node != NULL);
    node->id = id;
    node->type = type;
    node->next = NULL;
    return node;
}

Type_t *mk_type(BaseType base_type, int array_start, int array_end, Type_t *array_type) {
    Type_t *node = (Type_t *)malloc(sizeof(Type_t));
    assert(node != NULL);
    node->base_type = base_type;
    node->array_start = array_start;
    node->array_end = array_end;
    node->id = NULL;
    // Note: array_type is not used here, maybe it should be part of the struct?
    // For now, ignoring it to match the old mk_type behavior.
    return node;
}

Param_t *mk_param(ListNode_t *id_list, BaseType type, int pass_by_ref) {
    Param_t *node = (Param_t *)malloc(sizeof(Param_t));
    assert(node != NULL);
    node->id_list = id_list;
    node->type = type;
    node->pass_by_ref = pass_by_ref;
    return node;
}

VarDecl_t *ChainVarDecl(VarDecl_t *head, VarDecl_t *tail)
{
    if (head == NULL) return tail;
    VarDecl_t *cur = head;
    while (cur->next != NULL) {
        cur = cur->next;
    }
    cur->next = tail;
    return head;
}
#include <string.h>
#include <assert.h>

// new_flat_node helper function
FlatNode *new_flat_node(int line_num, FlatNodeType type) {
    FlatNode *node = (FlatNode *)malloc(sizeof(FlatNode));
    assert(node != NULL);
    node->line_num = line_num;
    node->node_type = type;
    return node;
}

/* Flat AST creation functions */
FlatNode *mk_flat_program(int line_num, char *id, ListNode_t *args, VarDecl_t *declarations, ListNode_t *subprograms, FlatNode *compound_statement) {
    FlatNode *node = new_flat_node(line_num, FL_PROGRAM);
    node->data.program.id = id;
    node->data.program.args = args;
    node->data.program.declarations = declarations;
    node->data.program.subprograms = subprograms;
    node->data.program.compound_statement = compound_statement;
    return node;
}

FlatNode *mk_flat_procedure(int line_num, char *id, ListNode_t *params, VarDecl_t *local_vars,
    ListNode_t *subprograms, FlatNode *compound_statement, int cname_flag, int overload_flag) {
    FlatNode *node = new_flat_node(line_num, FL_PROCEDURE);
    node->data.procedure.id = id;
    node->data.procedure.params = params;
    node->data.procedure.local_vars = local_vars;
    node->data.procedure.subprograms = subprograms;
    node->data.procedure.compound_statement = compound_statement;
    node->data.procedure.cname_flag = cname_flag;
    node->data.procedure.overload_flag = overload_flag;
    return node;
}

FlatNode *mk_flat_function(int line_num, char *id, ListNode_t *params, VarDecl_t *local_vars,
    ListNode_t *subprograms, FlatNode *compound_statement, int return_type, char *return_type_id, int cname_flag, int overload_flag) {
    FlatNode *node = new_flat_node(line_num, FL_FUNCTION);
    node->data.function.id = id;
    node->data.function.params = params;
    node->data.function.local_vars = local_vars;
    node->data.function.subprograms = subprograms;
    node->data.function.compound_statement = compound_statement;
    node->data.function.return_type = return_type;
    node->data.function.return_type_id = return_type_id;
    node->data.function.cname_flag = cname_flag;
    node->data.function.overload_flag = overload_flag;
    return node;
}

FlatNode *mk_flat_compoundstatement(int line_num, ListNode_t *stmt_list) {
    FlatNode *node = new_flat_node(line_num, FL_COMPOUND_STATEMENT);
    node->data.compound_statement.stmt_list = stmt_list;
    return node;
}

FlatNode *mk_flat_while(int line_num, FlatNode *condition, FlatNode *while_stmt) {
    FlatNode *node = new_flat_node(line_num, FL_WHILE_LOOP);
    node->data.while_loop.condition = condition;
    node->data.while_loop.while_stmt = while_stmt;
    return node;
}

FlatNode *mk_flat_for(int line_num, FlatNode *for_assign, FlatNode *to_expr, FlatNode *step_expr, FlatNode *stmt) {
    FlatNode *node = new_flat_node(line_num, FL_FOR_LOOP);
    node->data.for_loop.for_assign = for_assign;
    node->data.for_loop.to_expr = to_expr;
    node->data.for_loop.step_expr = step_expr;
    node->data.for_loop.for_stmt = stmt;
    return node;
}

FlatNode *mk_flat_ifthen(int line_num, FlatNode *condition, FlatNode *then_stmt, FlatNode *else_stmt) {
    FlatNode *node = new_flat_node(line_num, FL_IF_THEN);
    node->data.if_then.condition = condition;
    node->data.if_then.then_stmt = then_stmt;
    node->data.if_then.else_stmt = else_stmt;
    return node;
}

FlatNode *mk_flat_varassign(int line_num, FlatNode *var, FlatNode *expr) {
    FlatNode *node = new_flat_node(line_num, FL_VAR_ASSIGN);
    node->data.var_assign.var = var;
    node->data.var_assign.expr = expr;
    return node;
}

FlatNode *mk_flat_procedurecall(int line_num, char *id, ListNode_t *args) {
    FlatNode *node = new_flat_node(line_num, FL_PROCEDURE_CALL);
    node->data.procedure_call.id = id;
    node->data.procedure_call.args = args;
    return node;
}

FlatNode *mk_flat_varid(int line_num, char *id) {
    FlatNode *node = new_flat_node(line_num, FL_VAR_ID);
    node->data.var_id.id = id;
    return node;
}

FlatNode *mk_flat_arrayaccess(int line_num, char *id, FlatNode *index_expr) {
    FlatNode *node = new_flat_node(line_num, FL_ARRAY_ACCESS);
    node->data.array_access.id = id;
    node->data.array_access.index_expr = index_expr;
    return node;
}

FlatNode *mk_flat_functioncall(int line_num, char *id, ListNode_t *args) {
    FlatNode *node = new_flat_node(line_num, FL_FUNCTION_CALL);
    node->data.function_call.id = id;
    node->data.function_call.args = args;
    return node;
}

static OpType token_to_optype(int token) {
    fprintf(stderr, "DEBUG: token_to_optype received token %d\n", token);
    switch(token) {
        case PLUS: return OP_ADD;
        case MINUS: return OP_SUB;
        case OR: return OP_OR;
        case STAR: return OP_MUL;
        case SLASH: return OP_DIV;
        case DIV: return OP_DIV; // Integer division, handle in semcheck/codegen
        case MOD: return OP_MOD;
        case AND: return OP_AND;
        case EQUAL: return OP_EQ;
        case NOTEQUAL: return OP_NE;
        case LESSTHAN: return OP_LT;
        case GREATERTHAN: return OP_GT;
        case LESSEQUAL: return OP_LE;
        case GREATEREQUAL: return OP_GE;
        case NOT: return OP_NOT;
        default: assert(0 && "Unknown token for op type");
    }
}

FlatNode *mk_flat_relop(int line_num, int op, FlatNode *left, FlatNode *right) {
    FlatNode *node = new_flat_node(line_num, FL_RELOP);
    node->data.bin_op.op = token_to_optype(op);
    node->data.bin_op.left = left;
    node->data.bin_op.right = right;
    return node;
}

FlatNode *mk_flat_addop(int line_num, int op, FlatNode *left, FlatNode *right) {
    FlatNode *node = new_flat_node(line_num, FL_ADDOP);
    node->data.bin_op.op = token_to_optype(op);
    node->data.bin_op.left = left;
    node->data.bin_op.right = right;
    return node;
}

FlatNode *mk_flat_mulop(int line_num, int op, FlatNode *left, FlatNode *right) {
    FlatNode *node = new_flat_node(line_num, FL_MULOP);
    node->data.bin_op.op = token_to_optype(op);
    node->data.bin_op.left = left;
    node->data.bin_op.right = right;
    return node;
}

FlatNode *mk_flat_inum(int line_num, int val) {
    FlatNode *node = new_flat_node(line_num, FL_INUM);
    node->data.inum = val;
    return node;
}

FlatNode *mk_flat_rnum(int line_num, float val) {
    FlatNode *node = new_flat_node(line_num, FL_RNUM);
    node->data.rnum = val;
    return node;
}

FlatNode *mk_flat_string(int line_num, char *str) {
    FlatNode *node = new_flat_node(line_num, FL_STRING);
    node->data.string = str;
    return node;
}

FlatNode *mk_flat_unop(int line_num, OpType op, FlatNode *operand) {
    FlatNode *node = new_flat_node(line_num, FL_UNOP);
    node->data.un_op.op = op;
    node->data.un_op.operand = operand;
    return node;
}


