#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include "ir_generator.h"
#include "../../ir.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/List/List.h"
#include "Grammar.tab.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"

static IRValue *new_temp_var() {
    static int count = 0;
    IRValue *val = malloc(sizeof(IRValue));
    char *name = malloc(10);
    sprintf(name, "t%d", count++);
    val->name = name;
    val->is_global = 0;
    return val;
}

static char *new_label() {
    static int count = 0;
    char *label = malloc(20);
    sprintf(label, "L%d", count++);
    return label;
}

static ListNode_t *generate_expr_ir(struct Expression *expr, SymTab_t *table, ListNode_t *inst_list, IRValue **result);
static ListNode_t *generate_statement_ir(struct Statement *stmt, SymTab_t *table, ListNode_t *inst_list);
static ListNode_t *generate_subprogram_ir(Tree_t *subprogram, SymTab_t *table, ListNode_t *inst_list);

static bool is_builtin_procedure(const char *name) {
    if (name == NULL) return false;
    return strcmp(name, "writeln") == 0 ||
           strcmp(name, "write") == 0 ||
           strcmp(name, "read") == 0;
}

ListNode_t *generate_ir(Tree_t *ast, SymTab_t *table) {
    if (ast->type == TREE_PROGRAM_TYPE) {
        PushScope(table);
        ListNode_t *inst_list = NULL;
        ListNode_t *subprograms = ast->tree_data.program_data.subprograms;
        while (subprograms) {
            inst_list = generate_subprogram_ir(subprograms->cur, table, inst_list);
            subprograms = subprograms->next;
        }
        inst_list = generate_statement_ir(ast->tree_data.program_data.body_statement, table, inst_list);
        PopScope(table);
        return inst_list;
    }
    return NULL;
}

static ListNode_t *generate_subprogram_ir(Tree_t *subprogram, SymTab_t *table, ListNode_t *inst_list) {
    assert(subprogram->type == TREE_SUBPROGRAM);

    if (is_builtin_procedure(subprogram->tree_data.subprogram_data.id)) {
        return inst_list;
    }

    // New scope for the subprogram
    PushScope(table);

    IRInstruction *label_inst = calloc(1, sizeof(IRInstruction));
    label_inst->opcode = IR_LABEL;
    label_inst->label = subprogram->tree_data.subprogram_data.id;
    if (inst_list == NULL) {
        inst_list = CreateListNode(label_inst, LIST_UNSPECIFIED);
    } else {
        inst_list = PushListNodeBack(inst_list, CreateListNode(label_inst, LIST_UNSPECIFIED));
    }

    // TODO: Handle arguments

    // Generate IR for the body
    inst_list = generate_statement_ir(subprogram->tree_data.subprogram_data.statement_list, table, inst_list);

    IRInstruction *ret_inst = calloc(1, sizeof(IRInstruction));
    ret_inst->opcode = IR_RETURN;
    inst_list = PushListNodeBack(inst_list, CreateListNode(ret_inst, LIST_UNSPECIFIED));


    // Pop the scope
    PopScope(table);

    return inst_list;
}

static ListNode_t *generate_expr_ir(struct Expression *expr, SymTab_t *table, ListNode_t *inst_list, IRValue **result) {
    if (expr->type == EXPR_INUM) {
        IRInstruction *inst = calloc(1, sizeof(IRInstruction));
        inst->opcode = IR_LOAD_CONST;
        inst->dest = new_temp_var();
        inst->src1 = malloc(sizeof(IRValue));
        inst->src1->name = malloc(10);
        sprintf(inst->src1->name, "%d", expr->expr_data.i_num);
        inst->src2 = NULL;
        *result = inst->dest;
        ListNode_t *new_node = CreateListNode(inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            return new_node;
        PushListNodeBack(inst_list, new_node);
        return inst_list;
    }
    else if (expr->type == EXPR_VAR_ID) {
        IRInstruction *inst = calloc(1, sizeof(IRInstruction));
        inst->opcode = IR_LOAD_VAR;
        inst->dest = new_temp_var();

        HashNode_t *hash_node;
        int scope_level = FindIdent(&hash_node, table, expr->expr_data.id);
        if (scope_level == -1) {
            fprintf(stderr, "ERROR: Undefined variable %s\n", expr->expr_data.id);
            // TODO: This should probably not be a fatal error
            exit(1);
        }

        inst->src1 = malloc(sizeof(IRValue));
        inst->src1->name = expr->expr_data.id;
        inst->src1->is_global = (scope_level == 0);
        inst->src2 = NULL;
        *result = inst->dest;
        ListNode_t *new_node = CreateListNode(inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            return new_node;
        PushListNodeBack(inst_list, new_node);
        return inst_list;
    }
    else if (expr->type == EXPR_ADDOP) {
        IRValue *left_val, *right_val;
        inst_list = generate_expr_ir(expr->expr_data.addop_data.left_expr, table, inst_list, &left_val);
        ListNode_t *right_inst_list_head = generate_expr_ir(expr->expr_data.addop_data.right_term, table, NULL, &right_val);
        if (inst_list == NULL) {
            inst_list = right_inst_list_head;
        } else {
            ListNode_t *tail = inst_list;
            while (tail->next) {
                tail = tail->next;
            }
            tail->next = right_inst_list_head;
        }

        IRInstruction *inst = calloc(1, sizeof(IRInstruction));
        if (expr->expr_data.addop_data.addop_type == PLUS)
            inst->opcode = IR_ADD;
        else
            inst->opcode = IR_SUB;
        inst->dest = new_temp_var();
        inst->src1 = left_val;
        inst->src2 = right_val;
        *result = inst->dest;

        ListNode_t *new_node = CreateListNode(inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            return new_node;

        PushListNodeBack(inst_list, new_node);
        return inst_list;
    }
    else if (expr->type == EXPR_MULOP) {
        IRValue *left_val, *right_val;
        inst_list = generate_expr_ir(expr->expr_data.mulop_data.left_term, table, inst_list, &left_val);
        ListNode_t *right_inst_list_head = generate_expr_ir(expr->expr_data.mulop_data.right_factor, table, NULL, &right_val);
        if (inst_list == NULL) {
            inst_list = right_inst_list_head;
        } else {
            ListNode_t *tail = inst_list;
            while (tail->next) {
                tail = tail->next;
            }
            tail->next = right_inst_list_head;
        }

        IRInstruction *inst = calloc(1, sizeof(IRInstruction));
        if (expr->expr_data.mulop_data.mulop_type == STAR)
            inst->opcode = IR_MUL;
        else if (expr->expr_data.mulop_data.mulop_type == SLASH)
            inst->opcode = IR_DIV;
        else
            inst->opcode = IR_MOD;
        inst->dest = new_temp_var();
        inst->src1 = left_val;
        inst->src2 = right_val;
        *result = inst->dest;

        ListNode_t *new_node = CreateListNode(inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            return new_node;

        PushListNodeBack(inst_list, new_node);
        return inst_list;
    }
    return inst_list;
}

static ListNode_t *generate_statement_ir(struct Statement *stmt, SymTab_t *table, ListNode_t *inst_list) {
    if (stmt->type == STMT_VAR_ASSIGN) {
        IRValue *result;
        inst_list = generate_expr_ir(stmt->stmt_data.var_assign_data.expr, table, inst_list, &result);

        IRInstruction *store_inst = calloc(1, sizeof(IRInstruction));
        store_inst->opcode = IR_STORE_VAR;
        store_inst->src1 = result;

        HashNode_t *hash_node;
        int scope_level = FindIdent(&hash_node, table, stmt->stmt_data.var_assign_data.var->expr_data.id);
        if (scope_level == -1) {
            fprintf(stderr, "ERROR: Undefined variable %s\n", stmt->stmt_data.var_assign_data.var->expr_data.id);
            // TODO: This should probably not be a fatal error
            exit(1);
        }

        store_inst->dest = malloc(sizeof(IRValue));
        store_inst->dest->name = stmt->stmt_data.var_assign_data.var->expr_data.id;
        store_inst->dest->is_global = (scope_level == 0);

        ListNode_t *new_node = CreateListNode(store_inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            return new_node;

        return PushListNodeBack(inst_list, new_node);
    }
    else if (stmt->type == STMT_COMPOUND_STATEMENT) {
        ListNode_t *stmt_list = stmt->stmt_data.compound_statement;
        while (stmt_list) {
            inst_list = generate_statement_ir(stmt_list->cur, table, inst_list);
            stmt_list = stmt_list->next;
        }
        return inst_list;
    }
    else if (stmt->type == STMT_IF_THEN) {
        IRValue *left_val, *right_val;
        inst_list = generate_expr_ir(stmt->stmt_data.if_then_data.relop_expr->expr_data.relop_data.left, table, inst_list, &left_val);
        inst_list = generate_expr_ir(stmt->stmt_data.if_then_data.relop_expr->expr_data.relop_data.right, table, inst_list, &right_val);

        IRInstruction *cmp_inst = calloc(1, sizeof(IRInstruction));
        cmp_inst->opcode = IR_CMP;
        cmp_inst->src1 = left_val;
        cmp_inst->src2 = right_val;

        ListNode_t *cmp_node = CreateListNode(cmp_inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            inst_list = cmp_node;
        else
            inst_list = PushListNodeBack(inst_list, cmp_node);

        char *else_label = new_label();
        char *endif_label = new_label();

        IRInstruction *jump_inst = calloc(1, sizeof(IRInstruction));
        jump_inst->opcode = IR_JUMP_IF_ZERO;
        jump_inst->label = else_label;
        jump_inst->relop_type = stmt->stmt_data.if_then_data.relop_expr->expr_data.relop_data.type;
        inst_list = PushListNodeBack(inst_list, CreateListNode(jump_inst, LIST_UNSPECIFIED));

        inst_list = generate_statement_ir(stmt->stmt_data.if_then_data.if_stmt, table, inst_list);

        IRInstruction *endif_jump_inst = calloc(1, sizeof(IRInstruction));
        endif_jump_inst->opcode = IR_JUMP;
        endif_jump_inst->label = endif_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(endif_jump_inst, LIST_UNSPECIFIED));

        IRInstruction *else_label_inst = calloc(1, sizeof(IRInstruction));
        else_label_inst->opcode = IR_LABEL;
        else_label_inst->label = else_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(else_label_inst, LIST_UNSPECIFIED));

        if (stmt->stmt_data.if_then_data.else_stmt) {
            inst_list = generate_statement_ir(stmt->stmt_data.if_then_data.else_stmt, table, inst_list);
        }

        IRInstruction *endif_label_inst = calloc(1, sizeof(IRInstruction));
        endif_label_inst->opcode = IR_LABEL;
        endif_label_inst->label = endif_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(endif_label_inst, LIST_UNSPECIFIED));

        return inst_list;
    }
    else if (stmt->type == STMT_PROCEDURE_CALL) {
        IRInstruction *inst = calloc(1, sizeof(IRInstruction));
        inst->opcode = IR_CALL;
        inst->proc_name = stmt->stmt_data.procedure_call_data.id;

        ListNode_t *new_node = CreateListNode(inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            return new_node;

        return PushListNodeBack(inst_list, new_node);
    }
    else if (stmt->type == STMT_WHILE) {
        char *start_label = new_label();
        char *end_label = new_label();

        IRInstruction *start_label_inst = calloc(1, sizeof(IRInstruction));
        start_label_inst->opcode = IR_LABEL;
        start_label_inst->label = start_label;

        ListNode_t *start_node = CreateListNode(start_label_inst, LIST_UNSPECIFIED);
        if (inst_list == NULL)
            inst_list = start_node;
        else
            inst_list = PushListNodeBack(inst_list, start_node);

        IRValue *left_val, *right_val;
        inst_list = generate_expr_ir(stmt->stmt_data.while_data.relop_expr->expr_data.relop_data.left, table, inst_list, &left_val);
        inst_list = generate_expr_ir(stmt->stmt_data.while_data.relop_expr->expr_data.relop_data.right, table, inst_list, &right_val);

        IRInstruction *cmp_inst = calloc(1, sizeof(IRInstruction));
        cmp_inst->opcode = IR_CMP;
        cmp_inst->src1 = left_val;
        cmp_inst->src2 = right_val;
        inst_list = PushListNodeBack(inst_list, CreateListNode(cmp_inst, LIST_UNSPECIFIED));

        IRInstruction *jump_inst = calloc(1, sizeof(IRInstruction));
        jump_inst->opcode = IR_JUMP_IF_ZERO;
        jump_inst->label = end_label;
        jump_inst->relop_type = stmt->stmt_data.while_data.relop_expr->expr_data.relop_data.type;
        inst_list = PushListNodeBack(inst_list, CreateListNode(jump_inst, LIST_UNSPECIFIED));

        inst_list = generate_statement_ir(stmt->stmt_data.while_data.while_stmt, table, inst_list);

        IRInstruction *loop_jump_inst = calloc(1, sizeof(IRInstruction));
        loop_jump_inst->opcode = IR_JUMP;
        loop_jump_inst->label = start_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(loop_jump_inst, LIST_UNSPECIFIED));

        IRInstruction *end_label_inst = calloc(1, sizeof(IRInstruction));
        end_label_inst->opcode = IR_LABEL;
        end_label_inst->label = end_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(end_label_inst, LIST_UNSPECIFIED));

        return inst_list;
    }
    return inst_list;
}
