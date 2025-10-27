/*
    Damon Gwinn
    Code generation for expressions
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "codegen_expression.h"
#include "register_types.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"


/* Code generation for expressions */
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(ctx != NULL);
    expr_node_t *expr_tree = NULL;
    Register_t *target_reg;

    CODEGEN_DEBUG("DEBUG: Generating code for expression type %d\n", expr->type);

    switch(expr->type) {
        case EXPR_VAR_ID:
            CODEGEN_DEBUG("DEBUG: Processing variable ID expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ARRAY_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing array access expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_MULOP:
            CODEGEN_DEBUG("DEBUG: Processing mulop expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_INUM:
            CODEGEN_DEBUG("DEBUG: Processing integer constant expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RELOP:
            CODEGEN_DEBUG("DEBUG: Processing relational operator expression\n");
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return codegen_simple_relop(expr, inst_list, ctx, NULL);
        case EXPR_ADDOP:
            CODEGEN_DEBUG("DEBUG: Processing addop expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SIGN_TERM:
            CODEGEN_DEBUG("DEBUG: Processing sign term expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_FUNCTION_CALL:
            CODEGEN_DEBUG("DEBUG: Processing function call expression\n");
            expr_tree = build_expr_tree(expr);
            target_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);
            free_reg(get_reg_stack(), target_reg);
            free_expr_tree(expr_tree);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        default:
            assert(0 && "Unsupported expression type");
            break;
    }
}


ListNode_t *codegen_array_element_address(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);
    assert(ctx != NULL);
    assert(out_reg != NULL);

    const char *array_id = expr->expr_data.array_access_data.id;
    if (array_id == NULL)
    {
        fprintf(stderr, "ERROR: Missing array identifier in access expression.\n");
        exit(1);
    }

    inst_list = codegen_expr(expr->expr_data.array_access_data.array_expr, inst_list, ctx);
    Register_t *index_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (index_reg == NULL)
    {
        fprintf(stderr, "ERROR: Unable to allocate register for array index.\n");
        exit(1);
    }

    StackNode_t *array_node = find_label((char *)array_id);
    if (array_node == NULL || array_node->is_array == 0)
    {
        fprintf(stderr, "ERROR: Array %s not found on stack (non-local arrays unsupported).\n", array_id);
        exit(1);
    }

    int element_size = array_node->element_size;
    if (element_size <= 0)
        element_size = DOUBLEWORD;

    int lower_bound = array_node->array_lower_bound;
    char buffer[100];

    if (lower_bound > 0)
    {
        snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", lower_bound, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (lower_bound < 0)
    {
        snprintf(buffer, sizeof(buffer), "\taddl\t$%d, %s\n", -lower_bound, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    int scaled_sizes[] = {1, 2, 4, 8};
    int can_scale = 0;
    for (size_t i = 0; i < sizeof(scaled_sizes) / sizeof(scaled_sizes[0]); ++i)
    {
        if (element_size == scaled_sizes[i])
        {
            can_scale = 1;
            break;
        }
    }

    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", index_reg->bit_32, index_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (can_scale)
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp,%s,%d), %s\n", array_node->offset, index_reg->bit_64, element_size, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        if (element_size != 1)
        {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        StackNode_t *offset_temp = find_in_temp("array_index_offset");
        if (offset_temp == NULL)
            offset_temp = add_l_t("array_index_offset");

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", index_reg->bit_64, offset_temp->offset);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", array_node->offset, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "\taddq\t-%d(%%rbp), %s\n", offset_temp->offset, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = index_reg;
    return inst_list;
}

ListNode_t *codegen_array_access(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    assert(expr != NULL);
    assert(target_reg != NULL);

    Register_t *addr_reg = NULL;
    inst_list = codegen_array_element_address(expr, inst_list, ctx, &addr_reg);

    char buffer[100];
    snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}


/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, int *relop_type)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);
    assert(inst_list != NULL);
    assert(ctx != NULL);

    CODEGEN_DEBUG("DEBUG: Generating simple relop\n");

    *relop_type = expr->expr_data.relop_data.type;
    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, ctx);

    Register_t *left_reg = get_free_reg(get_reg_stack(), &inst_list);
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, ctx);
    Register_t *right_reg = front_reg_stack(get_reg_stack());

    char buffer[100];
    snprintf(buffer, 100, "\tcmpl\t%s, %s\n", right_reg->bit_32, left_reg->bit_32);
    free_reg(get_reg_stack(), left_reg);
    inst_list = add_inst(inst_list, buffer);

    CODEGEN_DEBUG("DEBUG: Simple relop generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for non-local variable access */
ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    CODEGEN_DEBUG("DEBUG: Generating non-local access for %s\n", var_id);

    assert(inst_list != NULL);
    assert(var_id != NULL);
    assert(offset != NULL);

    char buffer[100];
    StackNode_t *var = find_label(var_id);

    if(var == NULL) {
        fprintf(stderr, "ERROR: Could not find non-local variable %s\n", var_id);
        exit(1);
    }

    *offset = var->offset;
    snprintf(buffer, 100, "\tmovq\t-8(%%rbp), %s\n", current_non_local_reg64());
    inst_list = add_inst(inst_list, buffer);

    CODEGEN_DEBUG("DEBUG: Non-local access generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx, HashNode_t *proc_node)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int arg_num;
    Register_t *top_reg;
    char buffer[50];
    const char *arg_reg_char;
    expr_node_t *expr_tree;

    assert(ctx != NULL);

    ListNode_t *formal_args = NULL;
    if(proc_node != NULL)
        formal_args = proc_node->args;

    arg_num = 0;
    while(args != NULL)
    {
        CODEGEN_DEBUG("DEBUG: In codegen_pass_arguments loop, arg_num = %d\n", arg_num);
        struct Expression *arg_expr = (struct Expression *)args->cur;
        CODEGEN_DEBUG("DEBUG: arg_expr at %p, type %d\n", arg_expr, arg_expr->type);

        arg_reg_char = get_arg_reg64_num(arg_num);
        if(arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", arg_num);
            exit(1);
        }

        Tree_t *formal_arg_decl = NULL;
        if(formal_args != NULL)
            formal_arg_decl = (Tree_t *)formal_args->cur;

        if(formal_arg_decl != NULL && formal_arg_decl->tree_data.var_decl_data.is_var_param)
        {
            // Pass by reference
            assert(arg_expr->type == EXPR_VAR_ID);
            StackNode_t *var_node = find_label(arg_expr->expr_data.id);
            snprintf(buffer, 50, "\tleaq\t-%d(%%rbp), %s\n", var_node->offset, arg_reg_char);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            // Pass by value
            expr_tree = build_expr_tree(arg_expr);
            top_reg = get_free_reg(get_reg_stack(), &inst_list);
            CODEGEN_DEBUG("DEBUG: top_reg at %p\n", top_reg);
            inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, top_reg);
            free_expr_tree(expr_tree);

            snprintf(buffer, 50, "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg_char);
            free_reg(get_reg_stack(), top_reg);
            inst_list = add_inst(inst_list, buffer);
        }

        args = args->next;
        if(formal_args != NULL)
            formal_args = formal_args->next;
        ++arg_num;
    }

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Helper for codegen_get_nonlocal */
ListNode_t * codegen_goto_prev_scope(ListNode_t *inst_list, StackScope_t *cur_scope, char *base)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[50];

    assert(inst_list != NULL);
    assert(cur_scope != NULL);
    assert(base != NULL);

    snprintf(buffer, 50, "\tmovq\t(%s), %s\n", base, current_non_local_reg64());
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}
