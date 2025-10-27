#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "register_types.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"

/* Codegen for a statement */
ListNode_t *codegen_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);
    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            inst_list = codegen_var_assignment(stmt, inst_list, ctx);
            break;
        case STMT_PROCEDURE_CALL:
            inst_list = codegen_proc_call(stmt, inst_list, ctx, symtab);
            break;
        case STMT_COMPOUND_STATEMENT:
            inst_list = codegen_compound_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_IF_THEN:
            inst_list = codegen_if_then(stmt, inst_list, ctx, symtab);
            break;
        case STMT_WHILE:
            inst_list = codegen_while(stmt, inst_list, ctx, symtab);
            break;
        case STMT_REPEAT:
            inst_list = codegen_repeat(stmt, inst_list, ctx, symtab);
            break;
        case STMT_FOR:
            inst_list = codegen_for(stmt, inst_list, ctx, symtab);
            break;
        case STMT_ASM_BLOCK:
            inst_list = add_inst(inst_list, stmt->stmt_data.asm_block_data.code);
            break;
        default:
            assert(0 && "Unrecognized statement type in codegen");
            break;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_builtin_proc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);
    assert(ctx != NULL);

    char *proc_name;
    ListNode_t *args_expr;
    char buffer[50];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;

    HashNode_t *proc_node = stmt->stmt_data.procedure_call_data.resolved_proc;
    inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, proc_node);
    inst_list = codegen_vect_reg(inst_list, 0);
    snprintf(buffer, 50, "\tcall\t%s\n", proc_name);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Returns a list of instructions */
ListNode_t *codegen_compound_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_COMPOUND_STATEMENT);
    assert(ctx != NULL);
    assert(symtab != NULL);

    ListNode_t *stmt_list;
    struct Statement *cur_stmt;

    stmt_list = stmt->stmt_data.compound_statement;
    while(stmt_list != NULL)
    {
        cur_stmt = (struct Statement *)stmt_list->cur;
        inst_list = codegen_stmt(cur_stmt, inst_list, ctx, symtab);
        stmt_list = stmt_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for a variable assignment */
ListNode_t *codegen_var_assignment(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);
    assert(ctx != NULL);

    StackNode_t *var;
    Register_t *reg;
    char buffer[50];
    struct Expression *var_expr, *assign_expr;
    int offset;

    var_expr = stmt->stmt_data.var_assign_data.var;
    assign_expr = stmt->stmt_data.var_assign_data.expr;

    if (var_expr->type == EXPR_VAR_ID)
    {
        var = find_label(var_expr->expr_data.id);
        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        reg = front_reg_stack(get_reg_stack());

        if(var != NULL)
        {
            snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
        }
        else if(nonlocal_flag() == 1)
        {
            inst_list = codegen_get_nonlocal(inst_list, var_expr->expr_data.id, &offset);
            snprintf(buffer, 50, "\tmovq\t%s, -%d(%s)\n", reg->bit_64, offset, current_non_local_reg64());
        }
        else
        {
            fprintf(stderr, "ERROR: Non-local codegen support disabled (buggy)!\n");
            fprintf(stderr, "Enable with flag '-non-local' after required flags\n");
            exit(1);
        }
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return add_inst(inst_list, buffer);
    }
    else if (var_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        StackNode_t *array_node = NULL;
        inst_list = codegen_array_element_address(var_expr, inst_list, ctx, &addr_reg, &array_node);

        StackNode_t *addr_temp = add_l_t("array_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
        {
            fprintf(stderr, "ERROR: Unable to allocate register for array value.\n");
            exit(1);
        }

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            fprintf(stderr, "ERROR: Unable to allocate register for array store.\n");
            exit(1);
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        if (array_node != NULL && array_node->element_size >= 8)
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
        else
            snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else
    {
        assert(0 && "Unsupported assignment target");
    }
}

/* Code generation for a procedure call */
ListNode_t *codegen_proc_call(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char *proc_name;
    ListNode_t *args_expr;
    char buffer[50];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    char *unmangled_name = stmt->stmt_data.procedure_call_data.id;
    HashNode_t *proc_node = stmt->stmt_data.procedure_call_data.resolved_proc;

    if(proc_node == NULL)
    {
        FindIdent(&proc_node, symtab, unmangled_name);
        stmt->stmt_data.procedure_call_data.resolved_proc = proc_node;
    }


    if(proc_node != NULL && proc_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        return codegen_builtin_proc(stmt, inst_list, ctx);
    }
    else
    {
        inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, proc_node);
        inst_list = codegen_vect_reg(inst_list, 0);
        snprintf(buffer, 50, "\tcall\t%s\n", proc_name);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }
}

/* Code generation for if-then-else statements */
ListNode_t *codegen_if_then(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_IF_THEN);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type, inverse;
    struct Expression *expr;
    struct Statement *if_stmt, *else_stmt;
    char label1[18], label2[18], buffer[50];

    expr = stmt->stmt_data.if_then_data.relop_expr;
    inst_list = codegen_simple_relop(expr, inst_list, ctx, &relop_type);

    gen_label(label1, 18, ctx);
    gen_label(label2, 18, ctx);
    if_stmt = stmt->stmt_data.if_then_data.if_stmt;
    else_stmt = stmt->stmt_data.if_then_data.else_stmt;

    inverse = 1;
    inst_list = gencode_jmp(relop_type, inverse, label1, inst_list);
    inst_list = codegen_stmt(if_stmt, inst_list, ctx, symtab);

    if(else_stmt == NULL)
    {
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        inverse = 0;
        inst_list = gencode_jmp(NORMAL_JMP, inverse, label2, inst_list);
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_stmt(else_stmt, inst_list, ctx, symtab);
        snprintf(buffer, 50, "%s:\n", label2);
        inst_list = add_inst(inst_list, buffer);
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for while statements */
ListNode_t *codegen_while(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_WHILE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type, inverse;
    struct Expression *expr;
    struct Statement *while_stmt;
    char label1[18], label2[18], buffer[50];

    gen_label(label1, 18, ctx);
    gen_label(label2, 18, ctx);
    while_stmt = stmt->stmt_data.while_data.while_stmt;
    expr = stmt->stmt_data.while_data.relop_expr;

    inverse = 0;
    inst_list = gencode_jmp(NORMAL_JMP, inverse, label1, inst_list);

    snprintf(buffer, 50, "%s:\n", label2);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_stmt(while_stmt, inst_list, ctx, symtab);

    snprintf(buffer, 50, "%s:\n", label1);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_simple_relop(expr, inst_list, ctx, &relop_type);

    inverse = 0;
    inst_list = gencode_jmp(relop_type, inverse, label2, inst_list);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_repeat(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_REPEAT);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char body_label[18], buffer[50];
    int relop_type, inverse;
    ListNode_t *body_list = stmt->stmt_data.repeat_data.body_list;

    gen_label(body_label, 18, ctx);
    snprintf(buffer, 50, "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        inst_list = codegen_stmt(body_stmt, inst_list, ctx, symtab);
        body_list = body_list->next;
    }

    inst_list = codegen_simple_relop(stmt->stmt_data.repeat_data.until_expr, inst_list, ctx, &relop_type);
    inverse = 1;
    inst_list = gencode_jmp(relop_type, inverse, body_label, inst_list);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for for statements */
ListNode_t *codegen_for(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);
    assert(ctx != NULL);
    assert(symtab != NULL);

    int relop_type, inverse;
    struct Expression *expr, *for_var, *comparison_expr, *update_expr, *one_expr;
    struct Statement *for_body, *for_assign, *update_stmt;
    char label1[18], label2[18], buffer[50];

    gen_label(label1, 18, ctx);
    gen_label(label2, 18, ctx);
    for_body = stmt->stmt_data.for_data.do_for;
    expr = stmt->stmt_data.for_data.to;

    if(stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        inst_list = codegen_var_assignment(for_assign, inst_list, ctx);
        for_var = stmt->stmt_data.for_data.for_assign_data.var_assign->stmt_data.var_assign_data.var;
    }
    else
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
    }

    assert(for_var->type == EXPR_VAR_ID);
    comparison_expr = mk_relop(-1, LE, for_var, expr);
    one_expr = mk_inum(-1, 1);
    update_expr = mk_addop(-1, PLUS, for_var, one_expr);
    update_stmt = mk_varassign(-1, for_var, update_expr);

    inverse = 0;
    inst_list = gencode_jmp(NORMAL_JMP, inverse, label1, inst_list);

    snprintf(buffer, 50, "%s:\n", label2);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_stmt(for_body, inst_list, ctx, symtab);

    inst_list = codegen_stmt(update_stmt, inst_list, ctx, symtab);

    snprintf(buffer, 50, "%s:\n", label1);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_simple_relop(comparison_expr, inst_list, ctx, &relop_type);

    inverse = 0;
    inst_list = gencode_jmp(relop_type, inverse, label2, inst_list);

    free(comparison_expr);
    free(one_expr);
    free(update_expr);
    free(update_stmt);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}
