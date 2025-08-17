#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "ir_generator.h"
#include "../../ir.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/List/List.h"
#include "Grammar.tab.h"

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

static ListNode_t *generate_expr_ir(struct Expression *expr, ListNode_t *inst_list, IRValue **result) {
    if (expr->type == EXPR_INUM) {
        IRInstruction *inst = malloc(sizeof(IRInstruction));
        inst->opcode = IR_LOAD_CONST;
        inst->dest = new_temp_var();
        inst->src1 = malloc(sizeof(IRValue));
        inst->src1->name = malloc(10);
        sprintf(inst->src1->name, "%d", expr->expr_data.i_num);
        inst->src2 = NULL;
        *result = inst->dest;
        return CreateListNode(inst, LIST_UNSPECIFIED);
    }
    else if (expr->type == EXPR_VAR_ID) {
        IRInstruction *inst = malloc(sizeof(IRInstruction));
        inst->opcode = IR_LOAD_VAR;
        inst->dest = new_temp_var();
        inst->src1 = malloc(sizeof(IRValue));
        inst->src1->name = expr->expr_data.id;
        inst->src2 = NULL;
        *result = inst->dest;
        return CreateListNode(inst, LIST_UNSPECIFIED);
    }
    else if (expr->type == EXPR_ADDOP) {
        IRValue *left_val, *right_val;
        inst_list = generate_expr_ir(expr->expr_data.addop_data.left_expr, inst_list, &left_val);
        ListNode_t *right_inst_list = generate_expr_ir(expr->expr_data.addop_data.right_term, NULL, &right_val);
        if (inst_list == NULL)
            inst_list = right_inst_list;
        else
            PushListNodeBack(inst_list, right_inst_list);

        IRInstruction *inst = malloc(sizeof(IRInstruction));
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
        inst_list = generate_expr_ir(expr->expr_data.mulop_data.left_term, inst_list, &left_val);
        ListNode_t *right_inst_list = generate_expr_ir(expr->expr_data.mulop_data.right_factor, NULL, &right_val);
        if (inst_list == NULL)
            inst_list = right_inst_list;
        else
            PushListNodeBack(inst_list, right_inst_list);

        IRInstruction *inst = malloc(sizeof(IRInstruction));
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

static ListNode_t *generate_statement_ir(struct Statement *stmt, ListNode_t *inst_list) {
    if (stmt->type == STMT_VAR_ASSIGN) {
        IRValue *result;
        inst_list = generate_expr_ir(stmt->stmt_data.var_assign_data.expr, inst_list, &result);

        IRInstruction *store_inst = malloc(sizeof(IRInstruction));
        store_inst->opcode = IR_STORE_VAR;
        store_inst->src1 = result;
        store_inst->dest = malloc(sizeof(IRValue));
        store_inst->dest->name = stmt->stmt_data.var_assign_data.var->expr_data.id;

        return PushListNodeBack(inst_list, CreateListNode(store_inst, LIST_UNSPECIFIED));
    }
    else if (stmt->type == STMT_COMPOUND_STATEMENT) {
        ListNode_t *stmt_list = stmt->stmt_data.compound_statement;
        while (stmt_list) {
            inst_list = generate_statement_ir(stmt_list->cur, inst_list);
            stmt_list = stmt_list->next;
        }
        return inst_list;
    }
    else if (stmt->type == STMT_IF_THEN) {
        IRValue *left_val, *right_val;
        inst_list = generate_expr_ir(stmt->stmt_data.if_then_data.relop_expr->expr_data.relop_data.left, inst_list, &left_val);
        inst_list = generate_expr_ir(stmt->stmt_data.if_then_data.relop_expr->expr_data.relop_data.right, inst_list, &right_val);

        IRInstruction *cmp_inst = malloc(sizeof(IRInstruction));
        cmp_inst->opcode = IR_CMP;
        cmp_inst->src1 = left_val;
        cmp_inst->src2 = right_val;
        inst_list = PushListNodeBack(inst_list, CreateListNode(cmp_inst, LIST_UNSPECIFIED));

        char *else_label = new_label();
        char *endif_label = new_label();

        IRInstruction *jump_inst = malloc(sizeof(IRInstruction));
        jump_inst->opcode = IR_JUMP_IF_ZERO;
        jump_inst->label = else_label;
        jump_inst->relop_type = stmt->stmt_data.if_then_data.relop_expr->expr_data.relop_data.type;
        inst_list = PushListNodeBack(inst_list, CreateListNode(jump_inst, LIST_UNSPECIFIED));

        inst_list = generate_statement_ir(stmt->stmt_data.if_then_data.if_stmt, inst_list);

        IRInstruction *endif_jump_inst = malloc(sizeof(IRInstruction));
        endif_jump_inst->opcode = IR_JUMP;
        endif_jump_inst->label = endif_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(endif_jump_inst, LIST_UNSPECIFIED));

        IRInstruction *else_label_inst = malloc(sizeof(IRInstruction));
        else_label_inst->opcode = IR_LABEL;
        else_label_inst->label = else_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(else_label_inst, LIST_UNSPECIFIED));

        if (stmt->stmt_data.if_then_data.else_stmt) {
            inst_list = generate_statement_ir(stmt->stmt_data.if_then_data.else_stmt, inst_list);
        }

        IRInstruction *endif_label_inst = malloc(sizeof(IRInstruction));
        endif_label_inst->opcode = IR_LABEL;
        endif_label_inst->label = endif_label;
        inst_list = PushListNodeBack(inst_list, CreateListNode(endif_label_inst, LIST_UNSPECIFIED));

        return inst_list;
    }
    return inst_list;
}

ListNode_t *generate_ir(Tree_t *ast) {
    if (ast->type == TREE_PROGRAM_TYPE) {
        return generate_statement_ir(ast->tree_data.program_data.body_statement, NULL);
    }
    return NULL;
}
