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
#include "../../identifier_utils.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"

static int codegen_push_loop_exit(CodeGenContext *ctx, const char *label);
static void codegen_pop_loop_exit(CodeGenContext *ctx);
static const char *codegen_current_loop_exit(const CodeGenContext *ctx);
static int codegen_push_finally(CodeGenContext *ctx, ListNode_t *statements);
static void codegen_pop_finally(CodeGenContext *ctx);
static int codegen_has_finally(const CodeGenContext *ctx);
static ListNode_t *codegen_emit_finally_block(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *entry_label, const char *target_label);
static ListNode_t *codegen_branch_through_finally(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *target_label);
static int codegen_push_except(CodeGenContext *ctx, const char *label);
static void codegen_pop_except(CodeGenContext *ctx);
static const char *codegen_current_except_label(const CodeGenContext *ctx);
static ListNode_t *codegen_statement_list(ListNode_t *stmts, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
static ListNode_t *codegen_condition_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, int *relop_type);
static int record_type_is_mp_integer(const struct RecordType *record_type);
static int codegen_expr_is_mp_integer(struct Expression *expr);
static ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list, Register_t *addr_reg,
    Register_t *value_reg);
static ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx);

static unsigned long codegen_next_temp_suffix(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static StackNode_t *codegen_alloc_temp_slot(const char *prefix)
{
    char label[32];
    snprintf(label, sizeof(label), "%s_%lu", prefix != NULL ? prefix : "temp", codegen_next_temp_suffix());
    return add_l_t(label);
}

static int codegen_align_to(int value, int alignment)
{
    if (alignment <= 0)
        return value;
    int remainder = value % alignment;
    if (remainder == 0)
        return value;
    return value + (alignment - remainder);
}

static ListNode_t *codegen_call_with_shadow_space(ListNode_t *inst_list, CodeGenContext *ctx, const char *target)
{
    if (ctx == NULL || target == NULL)
        return inst_list;

    int shadow_space = 0;
    if (codegen_target_is_windows())
    {
        shadow_space = codegen_align_to(current_stack_home_space(), REQUIRED_OFFSET);
        if (shadow_space > 0)
        {
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "\tsubq\t$%d, %%rsp\n", shadow_space);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    char call_buffer[64];
    snprintf(call_buffer, sizeof(call_buffer), "\tcall\t%s\n", target);
    inst_list = add_inst(inst_list, call_buffer);

    if (shadow_space > 0)
    {
        char restore_buffer[64];
        snprintf(restore_buffer, sizeof(restore_buffer), "\taddq\t$%d, %%rsp\n", shadow_space);
        inst_list = add_inst(inst_list, restore_buffer);
    }

    return inst_list;
}

static ListNode_t *codegen_fail_register(CodeGenContext *ctx, ListNode_t *inst_list,
    Register_t **out_reg, const char *message)
{
    if (out_reg != NULL)
        *out_reg = NULL;
    if (message != NULL)
        codegen_report_error(ctx, "%s", message);
    return inst_list;
}

static ListNode_t *codegen_evaluate_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
        return inst_list;

    expr_node_t *expr_tree = build_expr_tree(expr);
    Register_t *reg = get_free_reg(get_reg_stack(), &inst_list);
    if (reg == NULL)
        return codegen_fail_register(ctx, inst_list, out_reg,
            "ERROR: Unable to allocate register for expression evaluation.");
    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, reg);
    free_expr_tree(expr_tree);
    *out_reg = reg;
    return inst_list;
}

static ListNode_t *codegen_condition_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, int *relop_type)
{
    if (expr == NULL)
        return inst_list;

    if (expr->type == EXPR_RELOP)
        return codegen_simple_relop(expr, inst_list, ctx, relop_type);

    Register_t *value_reg = NULL;
    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &value_reg);
    if (value_reg == NULL)
        return inst_list;

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", value_reg->bit_32, value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), value_reg);

    if (relop_type != NULL)
        *relop_type = NE;
    return inst_list;
}

ListNode_t *codegen_address_for_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
        return inst_list;

    if (expr->type == EXPR_VAR_ID)
    {
        StackNode_t *var_node = find_label(expr->expr_data.id);
        if (var_node == NULL)
        {
            if (nonlocal_flag() == 1)
            {
                int offset = 0;
                inst_list = codegen_get_nonlocal(inst_list, expr->expr_data.id, &offset);
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                    return codegen_fail_register(ctx, inst_list, out_reg,
                        "ERROR: Unable to allocate register for address expression.");
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n", offset,
                    current_non_local_reg64(), addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                *out_reg = addr_reg;
                return inst_list;
            }
            return codegen_evaluate_expr(expr, inst_list, ctx, out_reg);
        }
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
            return codegen_fail_register(ctx, inst_list, out_reg,
                "ERROR: Unable to allocate register for address expression.");
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", var_node->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        *out_reg = addr_reg;
        return inst_list;
    }
    else if (expr->type == EXPR_ARRAY_ACCESS)
    {
        return codegen_array_element_address(expr, inst_list, ctx, out_reg);
    }
    else if (expr->type == EXPR_RECORD_ACCESS)
    {
        return codegen_record_field_address(expr, inst_list, ctx, out_reg);
    }
    else if (expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_evaluate_expr(pointer_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            return inst_list;

        *out_reg = addr_reg;
        return inst_list;
    }

    return codegen_evaluate_expr(expr, inst_list, ctx, out_reg);
}

static int record_type_is_mp_integer(const struct RecordType *record_type)
{
    if (record_type == NULL)
        return 0;

    if (record_type->fields == NULL || record_type->fields->next != NULL)
        return 0;

    struct RecordField *field = (struct RecordField *)record_type->fields->cur;
    if (field == NULL || field->name == NULL)
        return 0;

    return strcmp(field->name, "__gpc_mp_handle") == 0;
}

static int codegen_expr_is_mp_integer(struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->resolved_type == RECORD_TYPE)
        return record_type_is_mp_integer(expr->record_type);

    if (expr->resolved_type == POINTER_TYPE && expr->pointer_subtype == RECORD_TYPE)
        return record_type_is_mp_integer(expr->record_type);

    return 0;
}

static ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list, Register_t *addr_reg,
    Register_t *value_reg)
{
    if (addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tgpc_gmp_mpint_assign\n");
    return inst_list;
}

static ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    long long record_size = 0;
    int size_status = codegen_get_record_size(ctx, dest_expr, &record_size);
    if (size_status != 0)
    {
        size_status = codegen_get_record_size(ctx, src_expr, &record_size);
        if (size_status != 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to determine record size for assignment.");
            return inst_list;
        }
    }

    if (record_size <= 0)
        return inst_list;

    Register_t *dest_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL)
    {
        if (dest_reg != NULL)
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    if (!codegen_expr_is_addressable(src_expr))
    {
        codegen_report_error(ctx,
            "ERROR: Unsupported record-valued source expression.");
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *src_reg = NULL;
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL)
    {
        if (src_reg != NULL)
            free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (count_reg == NULL)
    {
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for record copy size.");
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", record_size, count_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tgpc_move\n");

    free_reg(get_reg_stack(), dest_reg);
    free_reg(get_reg_stack(), src_reg);
    free_reg(get_reg_stack(), count_reg);
    free_arg_regs();
    return inst_list;
}

static int codegen_dynamic_array_element_size(CodeGenContext *ctx, StackNode_t *array_node,
    struct Expression *array_expr)
{
    if (array_node != NULL && array_node->element_size > 0)
        return array_node->element_size;

    const char *array_name = NULL;
    if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
        array_name = array_expr->expr_data.id;

    if (ctx != NULL)
    {
        if (array_name != NULL)
            codegen_report_error(ctx,
                "ERROR: Unable to determine element size for dynamic array %s.", array_name);
        else
            codegen_report_error(ctx,
                "ERROR: Unable to determine element size for dynamic array.");
    }

    return DOUBLEWORD;
}

static int codegen_push_loop_exit(CodeGenContext *ctx, const char *label)
{
    if (ctx == NULL || label == NULL)
        return 0;
    if (ctx->loop_depth == ctx->loop_capacity)
    {
        int new_capacity = (ctx->loop_capacity == 0) ? 4 : ctx->loop_capacity * 2;
        char **new_labels = realloc(ctx->loop_exit_labels, sizeof(char *) * new_capacity);
        if (new_labels == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate loop label stack.\n");
            return 0;
        }
        ctx->loop_exit_labels = new_labels;
        ctx->loop_capacity = new_capacity;
    }
    ctx->loop_exit_labels[ctx->loop_depth] = strdup(label);
    if (ctx->loop_exit_labels[ctx->loop_depth] == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate loop exit label.\n");
        return 0;
    }
    ctx->loop_depth += 1;
    return 1;
}

static void codegen_pop_loop_exit(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return;
    ctx->loop_depth -= 1;
    free(ctx->loop_exit_labels[ctx->loop_depth]);
    ctx->loop_exit_labels[ctx->loop_depth] = NULL;
}

static const char *codegen_current_loop_exit(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return NULL;
    return ctx->loop_exit_labels[ctx->loop_depth - 1];
}

static int codegen_push_finally(CodeGenContext *ctx, ListNode_t *statements)
{
    if (ctx == NULL)
        return 0;
    if (ctx->finally_capacity == ctx->finally_depth)
    {
        int new_capacity = (ctx->finally_capacity > 0) ? ctx->finally_capacity * 2 : 4;
        CodeGenFinallyFrame *new_stack = (CodeGenFinallyFrame *)realloc(
            ctx->finally_stack, sizeof(CodeGenFinallyFrame) * (size_t)new_capacity);
        if (new_stack == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate finally stack.\n");
            return 0;
        }
        memset(new_stack + ctx->finally_capacity, 0,
            sizeof(CodeGenFinallyFrame) * (size_t)(new_capacity - ctx->finally_capacity));
        ctx->finally_stack = new_stack;
        ctx->finally_capacity = new_capacity;
    }
    ctx->finally_stack[ctx->finally_depth].statements = statements;
    ctx->finally_depth += 1;
    return 1;
}

static void codegen_pop_finally(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->finally_depth <= 0)
        return;
    ctx->finally_depth -= 1;
    ctx->finally_stack[ctx->finally_depth].statements = NULL;
}

static int codegen_has_finally(const CodeGenContext *ctx)
{
    return (ctx != NULL && ctx->finally_depth > 0);
}

static ListNode_t *codegen_emit_finally_block(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *entry_label, const char *target_label)
{
    if (!codegen_has_finally(ctx))
        return inst_list;

    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%s:\n", entry_label);
    inst_list = add_inst(inst_list, buffer);

    int frame_index = ctx->finally_depth - 1;
    CodeGenFinallyFrame frame = ctx->finally_stack[frame_index];

    ctx->finally_depth -= 1;
    if (frame.statements != NULL)
        inst_list = codegen_statement_list(frame.statements, inst_list, ctx, symtab);
    ctx->finally_stack[frame_index] = frame;
    ctx->finally_depth += 1;

    if (target_label != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", target_label);
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

static ListNode_t *codegen_branch_through_finally(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *target_label)
{
    if (!codegen_has_finally(ctx))
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    int depth = ctx->finally_depth;
    if (depth <= 0)
        return inst_list;

    char (*entry_labels)[18] = (char (*)[18])malloc(sizeof(*entry_labels) * (size_t)depth);
    if (entry_labels == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate finally labels.\n");
        return inst_list;
    }

    for (int i = 0; i < depth; ++i)
        gen_label(entry_labels[i], 18, ctx);

    inst_list = gencode_jmp(NORMAL_JMP, 0, entry_labels[depth - 1], inst_list);

    int original_depth = ctx->finally_depth;
    for (int i = depth - 1; i >= 0; --i)
    {
        const char *target = (i == 0) ? target_label : entry_labels[i - 1];
        ctx->finally_depth = i + 1;
        inst_list = codegen_emit_finally_block(ctx, inst_list, symtab, entry_labels[i], target);
    }
    ctx->finally_depth = original_depth;

    free(entry_labels);
    return inst_list;
}

static int codegen_push_except(CodeGenContext *ctx, const char *label)
{
    if (ctx == NULL || label == NULL)
        return 0;
    if (ctx->except_capacity == ctx->except_depth)
    {
        int new_capacity = (ctx->except_capacity > 0) ? ctx->except_capacity * 2 : 4;
        char **new_labels = (char **)realloc(ctx->except_labels, sizeof(char *) * (size_t)new_capacity);
        if (new_labels == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate except stack.\n");
            return 0;
        }
        for (int i = ctx->except_capacity; i < new_capacity; ++i)
            new_labels[i] = NULL;
        ctx->except_labels = new_labels;
        ctx->except_capacity = new_capacity;
    }
    ctx->except_labels[ctx->except_depth] = strdup(label);
    if (ctx->except_labels[ctx->except_depth] == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to duplicate except label.\n");
        return 0;
    }
    ctx->except_depth += 1;
    return 1;
}

static void codegen_pop_except(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return;
    ctx->except_depth -= 1;
    free(ctx->except_labels[ctx->except_depth]);
    ctx->except_labels[ctx->except_depth] = NULL;
}

static const char *codegen_current_except_label(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return NULL;
    return ctx->except_labels[ctx->except_depth - 1];
}

static ListNode_t *codegen_statement_list(ListNode_t *stmts, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab)
{
    while (stmts != NULL)
    {
        if (stmts->cur != NULL)
            inst_list = codegen_stmt((struct Statement *)stmts->cur, inst_list, ctx, symtab);
        stmts = stmts->next;
    }
    return inst_list;
}

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
        case STMT_BREAK:
            inst_list = codegen_break_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_ASM_BLOCK:
            inst_list = add_inst(inst_list, stmt->stmt_data.asm_block_data.code);
            break;
        case STMT_EXIT:
        {
            inst_list = add_inst(inst_list, "\t# EXIT statement\n");
            if (codegen_has_finally(ctx))
            {
                char exit_label[18];
                gen_label(exit_label, sizeof(exit_label), ctx);
                inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, exit_label);
                char buffer[32];
                snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
                inst_list = add_inst(inst_list, buffer);
            }
            inst_list = add_inst(inst_list, "\tleave\n");
            inst_list = add_inst(inst_list, "\tret\n");
            break;
        }
        case STMT_CASE:
            inst_list = codegen_case(stmt, inst_list, ctx, symtab);
            break;
        case STMT_WITH:
            inst_list = codegen_with(stmt, inst_list, ctx, symtab);
            break;
        case STMT_TRY_FINALLY:
            inst_list = codegen_try_finally(stmt, inst_list, ctx, symtab);
            break;
        case STMT_TRY_EXCEPT:
            inst_list = codegen_try_except(stmt, inst_list, ctx, symtab);
            break;
        case STMT_RAISE:
            inst_list = codegen_raise(stmt, inst_list, ctx, symtab);
            break;
        case STMT_INHERITED:
            inst_list = codegen_inherited(stmt, inst_list, ctx, symtab);
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

static ListNode_t *codegen_builtin_setlength(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(stmt != NULL);
    assert(ctx != NULL);

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL)
    {
        fprintf(stderr, "ERROR: SetLength expects two arguments.\n");
        return inst_list;
    }

    struct Expression *array_expr = (struct Expression *)args_expr->cur;
    struct Expression *len_expr = (struct Expression *)args_expr->next->cur;

    if (array_expr == NULL || array_expr->type != EXPR_VAR_ID)
    {
        fprintf(stderr, "ERROR: SetLength first argument must be a variable identifier.\n");
        return inst_list;
    }

    StackNode_t *array_node = find_label(array_expr->expr_data.id);
    if (array_node == NULL || !array_node->is_dynamic)
    {
        fprintf(stderr, "ERROR: Dynamic array %s not found for SetLength.\n", array_expr->expr_data.id);
        return inst_list;
    }

    int element_size = codegen_dynamic_array_element_size(ctx, array_node, array_expr);

    inst_list = codegen_expr(len_expr, inst_list, ctx);
    if (codegen_had_error(ctx))
        return inst_list;
    Register_t *length_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (length_reg == NULL)
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for SetLength length.");

    Register_t *descriptor_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (descriptor_reg == NULL)
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for SetLength descriptor.");

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", array_node->offset, descriptor_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);

    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", length_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", element_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", descriptor_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", length_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", element_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tgpc_dynarray_setlength\n");

    free_reg(get_reg_stack(), descriptor_reg);
    free_reg(get_reg_stack(), length_reg);

    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_move(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL || args_expr->next == NULL || args_expr->next->next == NULL)
    {
        fprintf(stderr, "ERROR: Move expects three arguments.\n");
        return inst_list;
    }

    struct Expression *src_expr = (struct Expression *)args_expr->cur;
    struct Expression *dst_expr = (struct Expression *)args_expr->next->cur;
    struct Expression *count_expr = (struct Expression *)args_expr->next->next->cur;

    Register_t *dst_reg = NULL;
    Register_t *src_reg = NULL;
    Register_t *count_reg = NULL;

    inst_list = codegen_address_for_expr(dst_expr, inst_list, ctx, &dst_reg);
    if (codegen_had_error(ctx) || dst_reg == NULL)
        return inst_list;
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL)
        return inst_list;
    inst_list = codegen_evaluate_expr(count_expr, inst_list, ctx, &count_reg);
    if (codegen_had_error(ctx) || count_reg == NULL)
        return inst_list;
    inst_list = codegen_sign_extend32_to64(inst_list, count_reg->bit_32, count_reg->bit_64);

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dst_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dst_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = add_inst(inst_list, "\tcall\tgpc_move\n");

    free_reg(get_reg_stack(), dst_reg);
    free_reg(get_reg_stack(), src_reg);
    free_reg(get_reg_stack(), count_reg);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_builtin_inc(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    ListNode_t *args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    if (args_expr == NULL)
        return inst_list;

    struct Expression *target_expr = (struct Expression *)args_expr->cur;
    struct Expression *value_expr = (args_expr->next != NULL) ? (struct Expression *)args_expr->next->cur : NULL;

    Register_t *increment_reg = NULL;
    if (value_expr != NULL)
    {
        inst_list = codegen_expr_with_result(value_expr, inst_list, ctx, &increment_reg);
        if (codegen_had_error(ctx) || increment_reg == NULL)
            return inst_list;
    }
    else
    {
        increment_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (increment_reg == NULL)
            return inst_list;
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", increment_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    int target_is_long = (target_expr != NULL && target_expr->resolved_type == LONGINT_TYPE);
    if (target_is_long)
        inst_list = codegen_sign_extend32_to64(inst_list, increment_reg->bit_32, increment_reg->bit_64);

    if (target_expr != NULL && target_expr->type == EXPR_VAR_ID)
    {
        StackNode_t *var_node = find_label(target_expr->expr_data.id);
        char buffer[128];
        if (var_node != NULL)
        {
            if (target_is_long)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%%rbp)\n", increment_reg->bit_64, var_node->offset);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%%rbp)\n", increment_reg->bit_32, var_node->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (nonlocal_flag() == 1)
        {
            int offset = 0;
            inst_list = codegen_get_nonlocal(inst_list, target_expr->expr_data.id, &offset);
            if (target_is_long)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, -%d(%s)\n", increment_reg->bit_64, offset, current_non_local_reg64());
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, -%d(%s)\n", increment_reg->bit_32, offset, current_non_local_reg64());
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            codegen_report_error(ctx, "ERROR: Unable to locate variable %s for Inc.", target_expr->expr_data.id);
        }
    }
    else if (target_expr != NULL && target_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL)
        {
            char buffer[128];
            if (target_is_long)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", increment_reg->bit_64, addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", increment_reg->bit_32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
        }
    }
    else if (codegen_expr_is_addressable(target_expr))
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(target_expr, inst_list, ctx, &addr_reg);
        if (!codegen_had_error(ctx) && addr_reg != NULL)
        {
            char buffer[128];
            if (target_is_long)
                snprintf(buffer, sizeof(buffer), "\taddq\t%s, (%s)\n", increment_reg->bit_64, addr_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t%s, (%s)\n", increment_reg->bit_32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
        }
    }
    else
    {
        codegen_report_error(ctx, "ERROR: Unsupported Inc target.");
    }

    free_reg(get_reg_stack(), increment_reg);
    return inst_list;
}

static ListNode_t *codegen_builtin_write_like(struct Statement *stmt, ListNode_t *inst_list,
                                              CodeGenContext *ctx, int append_newline)
{
    if (stmt == NULL || ctx == NULL)
        return inst_list;

    char buffer[128];
    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;

        int expr_type = (expr != NULL) ? expr->resolved_type : UNKNOWN_TYPE;
        const int expr_is_real = (expr_type == REAL_TYPE);
        const char *width_dest64 = current_arg_reg64(0);
        const char *precision_dest64 = current_arg_reg64(1);
        const char *value_dest64 = current_arg_reg64(expr_is_real ? 2 : 1);

        Register_t *width_reg = NULL;
        Register_t *precision_reg = NULL;
        int has_width_reg = 0;
        int has_precision_reg = 0;

        if (expr != NULL && expr->field_width != NULL)
        {
            expr_node_t *width_tree = build_expr_tree(expr->field_width);
            width_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(width_tree, inst_list, ctx, width_reg);
            free_expr_tree(width_tree);
            has_width_reg = 1;
        }

        if (expr_is_real)
        {
            if (expr != NULL && expr->field_precision != NULL)
            {
                expr_node_t *precision_tree = build_expr_tree(expr->field_precision);
                precision_reg = get_free_reg(get_reg_stack(), &inst_list);
                inst_list = gencode_expr_tree(precision_tree, inst_list, ctx, precision_reg);
                free_expr_tree(precision_tree);
                has_precision_reg = 1;
            }
        }
        else if (expr != NULL && expr->field_precision != NULL)
        {
            expr_node_t *precision_tree = build_expr_tree(expr->field_precision);
            precision_reg = get_free_reg(get_reg_stack(), &inst_list);
            inst_list = gencode_expr_tree(precision_tree, inst_list, ctx, precision_reg);
            free_expr_tree(precision_tree);
            has_precision_reg = 1;
        }

        expr_node_t *expr_tree = build_expr_tree(expr);
        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, value_reg);
        free_expr_tree(expr_tree);

        if (expr_type == STRING_TYPE)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (expr_type == LONGINT_TYPE || expr_is_real || expr_type == POINTER_TYPE)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, value_dest64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_dest64);
        }

        free_reg(get_reg_stack(), value_reg);

        if (has_width_reg)
        {
            inst_list = codegen_sign_extend32_to64(inst_list, width_reg->bit_32, width_dest64);
            free_reg(get_reg_stack(), width_reg);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", width_dest64);
            inst_list = add_inst(inst_list, buffer);
        }

        if (expr_is_real)
        {
            if (has_precision_reg)
            {
                inst_list = codegen_sign_extend32_to64(inst_list, precision_reg->bit_32, precision_dest64);
                free_reg(get_reg_stack(), precision_reg);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t$-1, %s\n", precision_dest64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else if (has_precision_reg)
        {
            free_reg(get_reg_stack(), precision_reg);
        }

        inst_list = codegen_vect_reg(inst_list, 0);

        const char *call_target = "gpc_write_integer";
        if (expr_type == STRING_TYPE)
            call_target = "gpc_write_string";
        else if (expr_type == BOOL)
            call_target = "gpc_write_boolean";
        else if (expr_is_real)
            call_target = "gpc_write_real";
        else if (expr_type == POINTER_TYPE)
            call_target = "gpc_write_integer";  // Print pointers as integers (addresses)

        inst_list = codegen_call_with_shadow_space(inst_list, ctx, call_target);

        free_arg_regs();

        args = args->next;
    }

    if (append_newline)
    {
        inst_list = codegen_vect_reg(inst_list, 0);

        inst_list = codegen_call_with_shadow_space(inst_list, ctx, "gpc_write_newline");

        free_arg_regs();
    }

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

    const char *proc_id_lookup = stmt->stmt_data.procedure_call_data.id;

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "SetLength"))
    {
        inst_list = codegen_builtin_setlength(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "write"))
    {
        inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 0);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "writeln"))
    {
        inst_list = codegen_builtin_write_like(stmt, inst_list, ctx, 1);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Move"))
    {
        inst_list = codegen_builtin_move(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    if (proc_id_lookup != NULL && pascal_identifier_equals(proc_id_lookup, "Inc"))
    {
        inst_list = codegen_builtin_inc(stmt, inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }

    inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, NULL);
    inst_list = codegen_vect_reg(inst_list, 0);
    const char *call_target = (proc_name != NULL) ? proc_name : stmt->stmt_data.procedure_call_data.id;
    if (call_target == NULL)
        call_target = "";
    snprintf(buffer, 50, "\tcall\t%s\n", call_target);
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

    if (codegen_expr_is_mp_integer(var_expr))
    {
        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;

        Register_t *value_reg = front_reg_stack(get_reg_stack());
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || value_reg == NULL || addr_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        inst_list = codegen_call_mpint_assign(inst_list, addr_reg, value_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        free_arg_regs();
        return inst_list;
    }

    if (var_expr->resolved_type == RECORD_TYPE)
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    if (var_expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        var = find_label_with_depth(var_expr->expr_data.id, &scope_depth);
        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;
        reg = front_reg_stack(get_reg_stack());

        if(var != NULL)
        {
            int use_qword = (var->size >= 8);
            if (scope_depth == 0)
            {
                /* Variable is in current scope, assign normally */
                if (use_qword)
                    snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                else
                    snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
            }
            else if (scope_depth == 1)
            {
                /* Variable is in parent scope, use static link */
                StackNode_t *static_link_node = find_label("__static_link__");
                if (static_link_node != NULL)
                {
                    /* Load parent's frame pointer from static link */
                    char temp_buffer[100];
                    snprintf(temp_buffer, sizeof(temp_buffer), "\tmovq\t-%d(%%rbp), %%r11\n",
                        static_link_node->offset);
                    inst_list = add_inst(inst_list, temp_buffer);
                    /* Store to variable through static link */
                    if (use_qword)
                        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%r11)\n", reg->bit_64, var->offset);
                    else
                        snprintf(buffer, 50, "\tmovl\t%s, -%d(%%r11)\n", reg->bit_32, var->offset);
                }
                else
                {
                    /* No static link, fallback to direct access */
                    if (use_qword)
                        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                    else
                        snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
                }
            }
            else
            {
                fprintf(stderr, "ERROR: Variables nested more than 1 level deep not yet supported\n");
                exit(1);
            }
        }
        else if(nonlocal_flag() == 1)
        {
            inst_list = codegen_get_nonlocal(inst_list, var_expr->expr_data.id, &offset);
            snprintf(buffer, 50, "\tmovq\t%s, -%d(%s)\n", reg->bit_64, offset, current_non_local_reg64());
        }
        else
        {
            codegen_report_error(ctx,
                "ERROR: Non-local codegen support disabled (buggy)! Enable with flag '-non-local' after required flags");
            return inst_list;
        }
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return add_inst(inst_list, buffer);
    }
    else if (var_expr->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        StackNode_t *addr_temp = add_l_t("array_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;
        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for array value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for array store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int use_qword = codegen_type_uses_qword(var_expr->resolved_type);
        if (use_qword)
        {
            int value_is_qword = codegen_type_uses_qword(assign_expr->resolved_type);
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
        }
        else
            snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else if (var_expr->type == EXPR_RECORD_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_record_field_address(var_expr, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        StackNode_t *addr_temp = add_l_t("record_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;

        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for record value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for record store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int use_qword = codegen_type_uses_qword(var_expr->resolved_type);
        if (use_qword)
        {
            int value_is_qword = codegen_type_uses_qword(assign_expr->resolved_type);
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
        }
        else
        {
            snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else if (var_expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = var_expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            free_expr_tree(pointer_tree);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer assignment address.");
        }

        inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
        free_expr_tree(pointer_tree);

        StackNode_t *addr_temp = add_l_t("pointer_addr");
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, addr_temp->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);

        inst_list = codegen_expr(assign_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;

        Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        if (codegen_type_uses_qword(var_expr->resolved_type))
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

    int proc_scope_level = -1;
    if(proc_node == NULL)
    {
        proc_scope_level = FindIdent(&proc_node, symtab, unmangled_name);
        stmt->stmt_data.procedure_call_data.resolved_proc = proc_node;
    }
    else
    {
        /* proc_node already resolved, but we still need the scope level */
        HashNode_t *temp_node = NULL;
        proc_scope_level = FindIdent(&temp_node, symtab, unmangled_name);
    }

    if(proc_node != NULL && proc_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        return codegen_builtin_proc(stmt, inst_list, ctx);
    }
    else
    {
        /* Determine if we should pass a static link:
         * - The called procedure was declared at scope level 0 (in the program/current procedure)
         * - AND it has no parameters (for now, to avoid complexity)
         * This covers procedures nested in the main program or in the current procedure
         */
        int should_pass_static_link = (proc_scope_level == 0);
        int num_args = (args_expr == NULL) ? 0 : ListLength(args_expr);
        
        /* For nested procedures with no parameters, pass static link in %rdi */
        if (should_pass_static_link && num_args == 0)
        {
            /* Pass current frame pointer as static link */
            inst_list = add_inst(inst_list, "\tmovq\t%rbp, %rdi\n");
        }
        
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
    inst_list = codegen_condition_expr(expr, inst_list, ctx, &relop_type);

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

    int relop_type;
    struct Expression *expr;
    struct Statement *while_stmt;
    char cond_label[18], body_label[18], exit_label[18], buffer[50];

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    while_stmt = stmt->stmt_data.while_data.while_stmt;
    expr = stmt->stmt_data.while_data.relop_expr;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop_exit(ctx, exit_label))
        return inst_list;
    inst_list = codegen_stmt(while_stmt, inst_list, ctx, symtab);
    codegen_pop_loop_exit(ctx);
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_condition_expr(expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 0, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

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

    char body_label[18], exit_label[18], buffer[50];
    int relop_type;
    ListNode_t *body_list = stmt->stmt_data.repeat_data.body_list;

    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    snprintf(buffer, 50, "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    if (!codegen_push_loop_exit(ctx, exit_label))
        return inst_list;
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        inst_list = codegen_stmt(body_stmt, inst_list, ctx, symtab);
        body_list = body_list->next;
    }
    codegen_pop_loop_exit(ctx);

    inst_list = codegen_condition_expr(stmt->stmt_data.repeat_data.until_expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 1, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

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

    struct Expression *expr = NULL, *for_var = NULL, *update_expr = NULL, *one_expr = NULL;
    struct Statement *for_body = NULL, *for_assign = NULL, *update_stmt = NULL;
    Register_t *limit_reg = NULL;
    Register_t *loop_value_reg = NULL;
    char cond_label[18], body_label[18], exit_label[18], buffer[128];
    StackNode_t *limit_temp = NULL;

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
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

    if (for_var == NULL)
        return inst_list;

    one_expr = mk_inum(-1, 1);
    update_expr = mk_addop(-1, PLUS, for_var, one_expr);
    update_stmt = mk_varassign(-1, for_var, update_expr);

    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &limit_reg);
    if (codegen_had_error(ctx) || limit_reg == NULL)
        goto cleanup;

    limit_temp = codegen_alloc_temp_slot("for_to_temp");

    const int limit_is_qword = codegen_type_uses_qword(expr->resolved_type);
    const int limit_is_signed = codegen_expr_is_signed(expr);
    if (limit_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", limit_reg->bit_64, limit_temp->offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", limit_reg->bit_32, limit_temp->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), limit_reg);
    limit_reg = NULL;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop_exit(ctx, exit_label))
        goto cleanup;
    inst_list = codegen_stmt(for_body, inst_list, ctx, symtab);
    codegen_pop_loop_exit(ctx);

    inst_list = codegen_stmt(update_stmt, inst_list, ctx, symtab);
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_evaluate_expr(for_var, inst_list, ctx, &loop_value_reg);
    if (codegen_had_error(ctx) || loop_value_reg == NULL)
        goto cleanup;

    limit_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (limit_reg == NULL)
    {
        free_reg(get_reg_stack(), loop_value_reg);
        codegen_report_error(ctx, "ERROR: Unable to allocate register for for-loop bound.");
        goto cleanup;
    }

    if (limit_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", limit_temp->offset, limit_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", limit_temp->offset, limit_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    const int var_is_qword = codegen_type_uses_qword(for_var->resolved_type);
    const int var_is_signed = codegen_expr_is_signed(for_var);
    const int compare_as_qword = var_is_qword || limit_is_qword;
    if (compare_as_qword && !limit_is_qword)
    {
        if (limit_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_32);
    }
    if (compare_as_qword && !var_is_qword)
    {
        if (var_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_32);
    }

    const int use_unsigned_compare = !(limit_is_signed && var_is_signed);

    const char *cmp_instr = compare_as_qword ? "cmpq" : "cmpl";
    const char *limit_cmp_reg = compare_as_qword ? limit_reg->bit_64 : limit_reg->bit_32;
    const char *loop_cmp_reg = compare_as_qword ? loop_value_reg->bit_64 : loop_value_reg->bit_32;
    snprintf(buffer, sizeof(buffer), "\t%s\t%s, %s\n", cmp_instr, limit_cmp_reg, loop_cmp_reg);
    inst_list = add_inst(inst_list, buffer);

    const char *branch_instr = use_unsigned_compare ? "jbe" : "jle";
    snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", branch_instr, body_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), loop_value_reg);
    loop_value_reg = NULL;
    free_reg(get_reg_stack(), limit_reg);
    limit_reg = NULL;

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

cleanup:
    if (limit_reg != NULL)
        free_reg(get_reg_stack(), limit_reg);
    if (loop_value_reg != NULL)
        free_reg(get_reg_stack(), loop_value_reg);
    free(one_expr);
    free(update_expr);
    free(update_stmt);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for case statements */
ListNode_t *codegen_case(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_CASE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char end_label[18], buffer[100];
    gen_label(end_label, 18, ctx);
    
    /* Evaluate the selector expression once and keep the result in a register */
    struct Expression *selector = stmt->stmt_data.case_data.selector_expr;
    Register_t *selector_reg = NULL;
    inst_list = codegen_expr_with_result(selector, inst_list, ctx, &selector_reg);
    if (selector_reg == NULL)
        return inst_list;

    int selector_is_qword = codegen_type_uses_qword(selector->resolved_type);

    /* Generate code for each case branch */
    ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
    if (branch_node == NULL) {
        /* No branches - selector was already evaluated */
        free_reg(get_reg_stack(), selector_reg);
        return inst_list;
    }

    while (branch_node != NULL) {
        struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
        if (branch != NULL && branch->labels != NULL) {
            char branch_label[18], next_branch_label[18];
            gen_label(branch_label, 18, ctx);
            gen_label(next_branch_label, 18, ctx);
            
            /* Check each label in this branch */
            ListNode_t *label_node = branch->labels;
            while (label_node != NULL) {
                struct Expression *label_expr = (struct Expression *)label_node->cur;

                if (label_expr->type == EXPR_INUM) {
                    if (selector_is_qword)
                        snprintf(buffer, sizeof(buffer), "\tcmpq\t$%lld, %s\n",
                                 label_expr->expr_data.i_num, selector_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\tcmpl\t$%lld, %s\n",
                                 label_expr->expr_data.i_num, selector_reg->bit_32);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                    inst_list = add_inst(inst_list, buffer);
                } else {
                    Register_t *label_reg = NULL;
                    inst_list = codegen_expr_with_result(label_expr, inst_list, ctx, &label_reg);
                    if (label_reg != NULL) {
                        if (selector_is_qword)
                            snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, %s\n",
                                     label_reg->bit_64, selector_reg->bit_64);
                        else
                            snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n",
                                     label_reg->bit_32, selector_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), label_reg);
                    }
                }

                label_node = label_node->next;
            }
            
            /* If no match, jump to next branch */
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", next_branch_label);
            inst_list = add_inst(inst_list, buffer);
            
            /* Branch matched - execute statement */
            snprintf(buffer, sizeof(buffer), "%s:\n", branch_label);
            inst_list = add_inst(inst_list, buffer);
            if (branch->stmt != NULL)
                inst_list = codegen_stmt(branch->stmt, inst_list, ctx, symtab);
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", end_label);
            inst_list = add_inst(inst_list, buffer);
            
            /* Next branch label */
            snprintf(buffer, sizeof(buffer), "%s:\n", next_branch_label);
            inst_list = add_inst(inst_list, buffer);
        }
        branch_node = branch_node->next;
    }
    
    /* Else clause or fall-through */
    if (stmt->stmt_data.case_data.else_stmt != NULL) {
        inst_list = codegen_stmt(stmt->stmt_data.case_data.else_stmt, inst_list, ctx, symtab);
    }
    
    /* End label */
    snprintf(buffer, sizeof(buffer), "%s:\n", end_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    free_reg(get_reg_stack(), selector_reg);
    return inst_list;
}

static ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *exit_label = codegen_current_loop_exit(ctx);
    if (exit_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: BREAK statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    return codegen_branch_through_finally(ctx, inst_list, symtab, exit_label);
}

static ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    if (stmt->stmt_data.with_data.context_expr != NULL)
        inst_list = codegen_expr(stmt->stmt_data.with_data.context_expr, inst_list, ctx);
    if (stmt->stmt_data.with_data.body_stmt != NULL)
        inst_list = codegen_stmt(stmt->stmt_data.with_data.body_stmt, inst_list, ctx, symtab);
    return inst_list;
}

static ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_finally_data.try_statements;
    ListNode_t *finally_stmts = stmt->stmt_data.try_finally_data.finally_statements;

    if (finally_stmts == NULL)
        return codegen_statement_list(try_stmts, inst_list, ctx, symtab);

    if (!codegen_push_finally(ctx, finally_stmts))
        return inst_list;

    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);

    char final_entry[18];
    char after_label[18];
    gen_label(final_entry, sizeof(final_entry), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    inst_list = gencode_jmp(NORMAL_JMP, 0, final_entry, inst_list);
    inst_list = codegen_emit_finally_block(ctx, inst_list, symtab, final_entry, after_label);
    codegen_pop_finally(ctx);

    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

static ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
    ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;

    char except_label[18];
    char after_label[18];
    gen_label(except_label, sizeof(except_label), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    if (!codegen_push_except(ctx, except_label))
        return inst_list;

    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);
    inst_list = gencode_jmp(NORMAL_JMP, 0, after_label, inst_list);

    codegen_pop_except(ctx);

    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%s:\n", except_label);
    inst_list = add_inst(inst_list, buffer);

    if (except_stmts != NULL)
        inst_list = codegen_statement_list(except_stmts, inst_list, ctx, symtab);
    else
        inst_list = add_inst(inst_list, "\t# EXCEPT block with no handlers\n");

    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

static ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;

    inst_list = add_inst(inst_list, "\t# RAISE statement\n");

    struct Expression *exc_expr = stmt->stmt_data.raise_data.exception_expr;
    const char *except_label = codegen_current_except_label(ctx);

    if (except_label != NULL)
    {
        if (exc_expr != NULL)
            inst_list = codegen_expr(exc_expr, inst_list, ctx);
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, except_label);
        return inst_list;
    }

    if (codegen_has_finally(ctx))
    {
        char after_label[18];
        gen_label(after_label, sizeof(after_label), ctx);
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, after_label);

        char buffer[32];
        snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
        inst_list = add_inst(inst_list, buffer);
    }

    if (exc_expr != NULL)
        inst_list = codegen_expr(exc_expr, inst_list, ctx);
    else
        inst_list = add_inst(inst_list, "\txorl\t%eax, %eax\n");

    inst_list = add_inst(inst_list, "\tcall\tgpc_raise\n");
    inst_list = add_inst(inst_list, "\tud2\n");
    return inst_list;
}

static ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    (void)symtab;

    if (stmt == NULL)
        return inst_list;

    struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
    if (call_expr != NULL)
    {
        inst_list = codegen_expr(call_expr, inst_list, ctx);
        if (codegen_had_error(ctx))
            return inst_list;
    }
    else
    {
        inst_list = add_inst(inst_list, "\t# INHERITED statement without parent call\n");
    }
    return inst_list;
}
