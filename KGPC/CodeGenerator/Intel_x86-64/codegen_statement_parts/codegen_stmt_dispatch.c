#include "../codegen_stmt_internal.h"

int codegen_dynamic_array_element_size(CodeGenContext *ctx, StackNode_t *array_node,
    struct Expression *array_expr)
{
    if (array_node != NULL && array_node->element_size > 0)
        return array_node->element_size;

    if (array_expr != NULL)
    {
        long long elem_size = expr_get_array_element_size(array_expr, ctx);
        if (elem_size <= 0 && ctx != NULL && ctx->symtab != NULL)
        {
            KgpcType *array_type = expr_get_kgpc_type(array_expr);
            if (array_type != NULL && kgpc_type_is_array(array_type))
            {
                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(array_type, ctx->symtab);
                if (elem_type != NULL)
                    elem_size = kgpc_type_sizeof(elem_type);
            }
        }
        if (elem_size > 0 && elem_size <= INT_MAX)
            return (int)elem_size;
    }

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

int codegen_push_loop(CodeGenContext *ctx, const char *exit_label, const char *continue_label)
{
    if (ctx == NULL || exit_label == NULL || continue_label == NULL)
        return 0;
    if (ctx->loop_capacity == ctx->loop_depth)
    {
        int new_capacity = (ctx->loop_capacity > 0) ? ctx->loop_capacity * 2 : 4;
        CodeGenLoopFrame *new_frames = (CodeGenLoopFrame *)realloc(ctx->loop_frames, sizeof(CodeGenLoopFrame) * (size_t)new_capacity);
        if (new_frames == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate loop stack.\n");
            return 0;
        }
        for (int i = ctx->loop_capacity; i < new_capacity; ++i) {
            new_frames[i].label = NULL;
            new_frames[i].continue_label = NULL;
            new_frames[i].finally_depth = 0;
        }
        ctx->loop_frames = new_frames;
        ctx->loop_capacity = new_capacity;
    }
    ctx->loop_frames[ctx->loop_depth].label = strdup(exit_label);
    ctx->loop_frames[ctx->loop_depth].continue_label = strdup(continue_label);
    ctx->loop_frames[ctx->loop_depth].finally_depth = ctx->finally_depth;
    if (ctx->loop_frames[ctx->loop_depth].label == NULL || ctx->loop_frames[ctx->loop_depth].continue_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to duplicate loop labels.\n");
        if (ctx->loop_frames[ctx->loop_depth].label != NULL) free(ctx->loop_frames[ctx->loop_depth].label);
        if (ctx->loop_frames[ctx->loop_depth].continue_label != NULL) free(ctx->loop_frames[ctx->loop_depth].continue_label);
        return 0;
    }
    ctx->loop_depth += 1;
    return 1;
}

void codegen_pop_loop(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return;
    ctx->loop_depth -= 1;
    if (ctx->loop_frames[ctx->loop_depth].label != NULL)
    {
        free(ctx->loop_frames[ctx->loop_depth].label);
        ctx->loop_frames[ctx->loop_depth].label = NULL;
    }
    if (ctx->loop_frames[ctx->loop_depth].continue_label != NULL)
    {
        free(ctx->loop_frames[ctx->loop_depth].continue_label);
        ctx->loop_frames[ctx->loop_depth].continue_label = NULL;
    }
}

struct RecordType *codegen_resolve_with_record_type(struct Expression *context_expr,
    SymTab_t *symtab)
{
    if (context_expr == NULL || symtab == NULL)
        return NULL;
    if (context_expr->record_type != NULL)
        return context_expr->record_type;
    KgpcType *expr_type = expr_get_kgpc_type(context_expr);
    if (expr_type != NULL)
    {
        if (kgpc_type_is_record(expr_type))
            return kgpc_type_get_record(expr_type);
        if (kgpc_type_is_pointer(expr_type) &&
            expr_type->info.points_to != NULL &&
            kgpc_type_is_record(expr_type->info.points_to))
            return kgpc_type_get_record(expr_type->info.points_to);
    }
    if (context_expr->type == EXPR_VAR_ID && context_expr->expr_data.id != NULL)
    {
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, symtab, context_expr->expr_data.id) != 0 && var_node != NULL)
        {
            struct RecordType *rec = get_record_type_from_node(var_node);
            if (rec != NULL)
                return rec;
            if (var_node->type != NULL)
            {
                if (kgpc_type_is_record(var_node->type))
                    return kgpc_type_get_record(var_node->type);
                if (kgpc_type_is_pointer(var_node->type) &&
                    var_node->type->info.points_to != NULL &&
                    kgpc_type_is_record(var_node->type->info.points_to))
                    return kgpc_type_get_record(var_node->type->info.points_to);
            }
        }
    }
    if (context_expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_is_record(context_expr->resolved_kgpc_type))
            return kgpc_type_get_record(context_expr->resolved_kgpc_type);
        if (kgpc_type_is_pointer(context_expr->resolved_kgpc_type) &&
            context_expr->resolved_kgpc_type->info.points_to != NULL &&
            kgpc_type_is_record(context_expr->resolved_kgpc_type->info.points_to))
            return kgpc_type_get_record(context_expr->resolved_kgpc_type->info.points_to);
    }
    if (context_expr->type == EXPR_TYPECAST)
    {
        const char *target_id = context_expr->expr_data.typecast_data.target_type_id;
        if (target_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, target_id) != 0 && type_node != NULL)
                return get_record_type_from_node(type_node);
        }
    }
    if (context_expr->type == EXPR_FUNCTION_CALL)
    {
        const char *call_id = context_expr->expr_data.function_call_data.id;
        if (call_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, call_id) != 0 && type_node != NULL)
                return get_record_type_from_node(type_node);
        }
    }
    if (context_expr->type == EXPR_RECORD_ACCESS)
    {
        struct RecordField *field = codegen_lookup_record_field_expr(context_expr, NULL);
        if (field != NULL)
        {
            if (field->nested_record != NULL)
                return field->nested_record;
            if (field->type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, symtab, field->type_id) != 0 &&
                    type_node != NULL)
                {
                    struct RecordType *rec = get_record_type_from_node(type_node);
                    if (rec != NULL)
                        return rec;
                }
            }
            if (field->pointer_type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, symtab, field->pointer_type_id) != 0 &&
                    type_node != NULL)
                {
                    struct RecordType *rec = get_record_type_from_node(type_node);
                    if (rec != NULL)
                        return rec;
                }
            }
        }
    }
    if (context_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, context_expr->pointer_subtype_id) != 0 &&
            type_node != NULL)
            return get_record_type_from_node(type_node);
    }
    return NULL;
}

int codegen_with_push(CodeGenContext *ctx, struct Expression *context_expr,
    struct RecordType *record_type)
{
    if (ctx == NULL || record_type == NULL)
        return 0;
    if (ctx->with_capacity == ctx->with_depth)
    {
        int new_capacity = (ctx->with_capacity > 0) ? ctx->with_capacity * 2 : 4;
        CodeGenWithContext *new_stack = (CodeGenWithContext *)realloc(
            ctx->with_stack, sizeof(CodeGenWithContext) * (size_t)new_capacity);
        if (new_stack == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate WITH context stack.\n");
            return 0;
        }
        ctx->with_stack = new_stack;
        ctx->with_capacity = new_capacity;
    }
    ctx->with_stack[ctx->with_depth].context_expr = context_expr;
    ctx->with_stack[ctx->with_depth].record_type = record_type;
    ctx->with_depth += 1;
    return 1;
}

void codegen_with_pop(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->with_depth <= 0)
        return;
    ctx->with_depth -= 1;
    ctx->with_stack[ctx->with_depth].context_expr = NULL;
    ctx->with_stack[ctx->with_depth].record_type = NULL;
}

const char *codegen_current_loop_exit(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return NULL;
    return ctx->loop_frames[ctx->loop_depth - 1].label;
}

const char *codegen_current_loop_continue(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return NULL;
    return ctx->loop_frames[ctx->loop_depth - 1].continue_label;
}

void codegen_get_current_return_info(CodeGenContext *ctx, SymTab_t *symtab,
    int *out_is_real, int *out_size)
{
    if (out_is_real != NULL)
        *out_is_real = 0;
    if (out_size != NULL)
        *out_size = 0;
    if (ctx == NULL || symtab == NULL)
        return;

    const char *lookup_id = ctx->current_subprogram_id;
    const char *lookup_mangled = ctx->current_subprogram_mangled;
    if (lookup_id == NULL && lookup_mangled == NULL)
        return;

    HashNode_t *func_node = NULL;
    if (lookup_id != NULL)
        FindSymbol(&func_node, symtab, lookup_id);

    if (func_node == NULL && lookup_id != NULL && lookup_mangled != NULL)
    {
        ListNode_t *matches = FindAllIdents(symtab, lookup_id);
        for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, lookup_mangled) == 0)
            {
                func_node = candidate;
                break;
            }
        }
        if (matches != NULL)
            DestroyList(matches);
    }

    if (func_node != NULL &&
        func_node->type != NULL)
    {
        KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
        if (return_type == NULL)
            return;

        if (return_type->kind == TYPE_KIND_PRIMITIVE)
        {
            int tag = kgpc_type_get_primitive_tag(return_type);
            if (out_is_real != NULL && (tag == REAL_TYPE || tag == EXTENDED_TYPE))
                *out_is_real = 1;
            if (out_size != NULL)
                *out_size = codegen_statement_return_storage_size(return_type);
        }
        else if (return_type->kind == TYPE_KIND_POINTER)
        {
            if (out_size != NULL)
                *out_size = 8;
        }
        else if (return_type->kind == TYPE_KIND_RECORD)
        {
            /* Use actual record size for movl vs movq decision */
            if (out_size != NULL)
            {
                *out_size = codegen_statement_return_storage_size(return_type);
            }
        }
    }
}

int codegen_get_current_return_shortstring_capacity(CodeGenContext *ctx, SymTab_t *symtab)
{
    if (ctx == NULL || symtab == NULL)
        return 0;

    const char *lookup_id = ctx->current_subprogram_id;
    const char *lookup_mangled = ctx->current_subprogram_mangled;
    if (lookup_id == NULL && lookup_mangled == NULL)
        return 0;

    HashNode_t *func_node = NULL;
    if (lookup_id != NULL)
        FindSymbol(&func_node, symtab, lookup_id);

    if (func_node == NULL && lookup_id != NULL && lookup_mangled != NULL)
    {
        ListNode_t *matches = FindAllIdents(symtab, lookup_id);
        for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, lookup_mangled) == 0)
            {
                func_node = candidate;
                break;
            }
        }
        if (matches != NULL)
            DestroyList(matches);
    }

    if (func_node == NULL || func_node->type == NULL)
        return 0;

    KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
    if (return_type != NULL)
    {
        int capacity = codegen_shortstring_capacity_from_type_local(return_type);
        if (capacity > 0)
            return capacity;
    }

    if (func_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        const char *ret_id = func_node->type->info.proc_info.return_type_id;
        if (ret_id == NULL && func_node->type->info.proc_info.definition != NULL)
            ret_id = func_node->type->info.proc_info.definition->tree_data.subprogram_data.return_type_id;
        if (ret_id != NULL && pascal_identifier_equals(ret_id, "ShortString"))
            return 256;
    }

    return 0;
}

void codegen_get_current_return_slot_info(CodeGenContext *ctx,
    int *out_is_real, int *out_size)
{
    if (out_is_real != NULL)
        *out_is_real = 0;
    if (out_size != NULL)
        *out_size = 0;
    if (ctx == NULL)
        return;

    StackNode_t *return_var = ctx->current_return_slot;
    if (return_var == NULL)
        return;

    long long size = return_var->element_size > 0 ? return_var->element_size : return_var->size;
    if (out_size != NULL && size > 0 && size <= INT_MAX)
        *out_size = (int)size;

    if (out_is_real != NULL && return_var->element_size == 10)
        *out_is_real = 1;
}

int codegen_current_loop_finally_depth(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->loop_depth <= 0)
        return 0;
    return ctx->loop_frames[ctx->loop_depth - 1].finally_depth;
}

int codegen_push_finally(CodeGenContext *ctx, ListNode_t *statements)
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

void codegen_pop_finally(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->finally_depth <= 0)
        return;
    ctx->finally_depth -= 1;
    ctx->finally_stack[ctx->finally_depth].statements = NULL;
}

int codegen_has_finally(const CodeGenContext *ctx)
{
    return (ctx != NULL && ctx->finally_depth > 0);
}

ListNode_t *codegen_emit_finally_block(CodeGenContext *ctx, ListNode_t *inst_list,
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

ListNode_t *codegen_branch_through_finally(CodeGenContext *ctx, ListNode_t *inst_list,
    SymTab_t *symtab, const char *target_label, int limit_depth)
{
    if (!codegen_has_finally(ctx))
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    int depth = ctx->finally_depth;
    if (depth <= 0)
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    /* If limit_depth is negative, unwind everything (e.g. Exit) */
    if (limit_depth < 0)
        limit_depth = 0;

    /* If current depth is already at or below limit, just jump */
    if (depth <= limit_depth)
        return gencode_jmp(NORMAL_JMP, 0, (char *)target_label, inst_list);

    int unwind_count = depth - limit_depth;
    char (*entry_labels)[18] = (char (*)[18])malloc(sizeof(*entry_labels) * (size_t)unwind_count);
    if (entry_labels == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate finally labels.\n");
        return inst_list;
    }

    for (int i = 0; i < unwind_count; ++i)
        gen_label(entry_labels[i], 18, ctx);

    inst_list = gencode_jmp(NORMAL_JMP, 0, entry_labels[unwind_count - 1], inst_list);

    int original_depth = ctx->finally_depth;
    /* Iterate from top of stack down to limit_depth */
    for (int i = unwind_count - 1; i >= 0; --i)
    {
        const char *target = (i == 0) ? target_label : entry_labels[i - 1];
        const char *entry = entry_labels[i];

        /* codegen_emit_finally_block will emit the entry label, so we don't emit it here */
        inst_list = codegen_emit_finally_block(ctx, inst_list, symtab, entry, target);
    }
    ctx->finally_depth = original_depth;

    free(entry_labels);
    return inst_list;
}

int codegen_push_except(CodeGenContext *ctx, const char *label)
{
    if (ctx == NULL || label == NULL)
        return 0;
    if (ctx->except_capacity == ctx->except_depth)
    {
        int new_capacity = (ctx->except_capacity > 0) ? ctx->except_capacity * 2 : 4;
        CodeGenExceptFrame *new_frames = (CodeGenExceptFrame *)realloc(ctx->except_frames, sizeof(CodeGenExceptFrame) * (size_t)new_capacity);
        if (new_frames == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate except stack.\n");
            return 0;
        }
        for (int i = ctx->except_capacity; i < new_capacity; ++i) {
            new_frames[i].label = NULL;
            new_frames[i].finally_depth = 0;
        }
        ctx->except_frames = new_frames;
        ctx->except_capacity = new_capacity;
    }
    ctx->except_frames[ctx->except_depth].label = strdup(label);
    ctx->except_frames[ctx->except_depth].finally_depth = ctx->finally_depth;
    if (ctx->except_frames[ctx->except_depth].label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to duplicate except label.\n");
        return 0;
    }
    ctx->except_depth += 1;
    return 1;
}

void codegen_pop_except(CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return;
    ctx->except_depth -= 1;
    if (ctx->except_frames[ctx->except_depth].label != NULL)
    {
        free(ctx->except_frames[ctx->except_depth].label);
        ctx->except_frames[ctx->except_depth].label = NULL;
    }
}

const char *codegen_current_except_label(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return NULL;
    return ctx->except_frames[ctx->except_depth - 1].label;
}

int codegen_current_except_finally_depth(const CodeGenContext *ctx)
{
    if (ctx == NULL || ctx->except_depth <= 0)
        return 0;
    return ctx->except_frames[ctx->except_depth - 1].finally_depth;
}

ListNode_t *codegen_statement_list(ListNode_t *stmts, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmts == NULL) {
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
            fprintf(stderr, "[CodeGen] codegen_statement_list: stmts is NULL\n");
        }
        return inst_list;
    }

    if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
        fprintf(stderr, "[CodeGen] codegen_statement_list: starting\n");
    }

    ListNode_t *node = stmts;
    while (node != NULL)
    {
        if (node->type == LIST_STMT)
        {
            struct Statement *stmt = (struct Statement *)node->cur;
            if (stmt != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
                    fprintf(stderr, "[CodeGen]   generating statement type=%d line=%d\n", stmt->type, stmt->line_num);
                }
                inst_list = codegen_stmt(stmt, inst_list, ctx, symtab);
            }
        }
        node = node->next;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL) {
        fprintf(stderr, "[CodeGen] codegen_statement_list: finished\n");
    }

    return inst_list;
}

/* Codegen for a statement */
ListNode_t *codegen_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    if (stmt == NULL)
        return inst_list;
    
    

    assert(ctx != NULL);
    assert(symtab != NULL);

    /* Invalidate the static link register cache at each statement boundary.
     * The cached register may have been reused for other purposes by a
     * previous statement's codegen (e.g., the first inner while loop in
     * QuickSort caches r12 as the static link, but if its condition
     * short-circuits, r12 is never loaded at runtime yet the second while
     * loop's codegen reuses the cached register without emitting a reload). */
    codegen_reset_static_link_cache(ctx);

    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            inst_list = codegen_var_assignment(stmt, inst_list, ctx);
            break;
        case STMT_PROCEDURE_CALL:
            inst_list = codegen_proc_call(stmt, inst_list, ctx, symtab);
            break;
        case STMT_EXPR:
            if (stmt->stmt_data.expr_stmt_data.expr != NULL)
                inst_list = codegen_expr(stmt->stmt_data.expr_stmt_data.expr, inst_list, ctx);
            break;
        case STMT_COMPOUND_STATEMENT:
            inst_list = codegen_compound_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_LABEL:
        {
            char label_name[256];
            format_pascal_label(label_name, sizeof(label_name), ctx, stmt->stmt_data.label_data.label);
            char buffer[272];
            snprintf(buffer, sizeof(buffer), "%s:\n", label_name);
            inst_list = add_inst(inst_list, buffer);
            if (stmt->stmt_data.label_data.stmt != NULL)
                inst_list = codegen_stmt(stmt->stmt_data.label_data.stmt, inst_list, ctx, symtab);
            break;
        }
        case STMT_GOTO:
        {
            char label_name[256];
            format_pascal_label(label_name, sizeof(label_name), ctx, stmt->stmt_data.goto_data.label);
            char buffer[272];
            snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", label_name);
            inst_list = add_inst(inst_list, buffer);
            break;
        }
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
        case STMT_FOR_IN:
            inst_list = codegen_for_in(stmt, inst_list, ctx, symtab);
            break;
        case STMT_BREAK:
            inst_list = codegen_break_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_CONTINUE:
            inst_list = codegen_continue_stmt(stmt, inst_list, ctx, symtab);
            break;
        case STMT_ASM_BLOCK:
        {
            const char *src = stmt->stmt_data.asm_block_data.code;
            if (src != NULL)
            {
                /* Detect Intel-syntax inline assembly (emitted by {$asmmode intel} blocks).
                 * KGPC targets AT&T syntax; Intel-syntax asm needs wrapping with
                 * .intel_syntax noprefix / .att_syntax prefix directives.
                 *
                 * Detection heuristics (any match → Intel syntax):
                 *   1. Memory operand keywords: "dword ptr", "qword ptr", etc.
                 *   2. Bracket memory operands: [...] (AT&T uses parentheses)
                 *   3. Bare register operands without '%' prefix in operand position
                 * Heuristic 2 is the strongest signal since AT&T never uses [] for
                 * memory addressing, while Intel always does. */
                int is_intel_syntax = 0;
                if (pascal_strcasestr(src, "dword ptr") != NULL ||
                    pascal_strcasestr(src, "qword ptr") != NULL ||
                    pascal_strcasestr(src, "byte ptr")  != NULL ||
                    pascal_strcasestr(src, "word ptr")  != NULL)
                    is_intel_syntax = 1;

                /* Check for Intel-style bracket memory operands: [reg], [reg+off] etc.
                 * AT&T syntax uses parentheses for memory: (%reg), off(%reg) etc.
                 * Skip [...] inside comments {...} and strings. */
                if (!is_intel_syntax) {
                    int in_brace_comment = 0;
                    for (const char *p = src; *p != '\0'; p++) {
                        if (*p == '{') { in_brace_comment = 1; continue; }
                        if (*p == '}') { in_brace_comment = 0; continue; }
                        if (in_brace_comment) continue;
                        if (*p == '[') {
                            /* Skip Pascal asm clobber lists: end ['eax','edx']
                             * These have [ followed by ' — Intel memory operands
                             * use [reg] or [reg+offset], never ['...'] */
                            if (*(p + 1) == '\'')
                                continue;
                            /* Found a bracket — this is Intel memory syntax */
                            is_intel_syntax = 1;
                            break;
                        }
                    }
                }

                if (is_intel_syntax)
                {
                    /* Special case: sincos_r_r_r — generate a fallback that calls
                     * fpc_in_sin_real / fpc_in_cos_real so the function body is not empty.
                     * The Intel-mode asm in FPC's mathu.inc computes:
                     *   sinus   := sin(theta)
                     *   cosinus := cos(theta)
                     * theta may be Single (size=4) or Double (size=8); out-params are
                     * always 8-byte Double vars (KGPC's Real type). */
                    if (ctx->current_subprogram_mangled != NULL &&
                        strcmp(ctx->current_subprogram_mangled, "sincos_r_r_r") == 0)
                    {
                        StackNode_t *theta_node   = find_label((char *)"theta");
                        StackNode_t *sinus_node   = find_label((char *)"sinus");
                        StackNode_t *cosinus_node = find_label((char *)"cosinus");
                        if (theta_node != NULL && sinus_node != NULL && cosinus_node != NULL)
                        {
                            char buf[256];
                            int theta_off   = theta_node->offset;
                            int theta_size  = theta_node->size;
                            int sinus_off   = sinus_node->offset;
                            int cosinus_off = cosinus_node->offset;
                            /* Load theta into xmm0 as Double */
                            if (theta_size == 4)
                                snprintf(buf, sizeof(buf), "\tcvtss2sd\t-%d(%%rbp), %%xmm0\n", theta_off);
                            else
                                snprintf(buf, sizeof(buf), "\tmovsd\t-%d(%%rbp), %%xmm0\n", theta_off);
                            inst_list = add_inst(inst_list, strdup(buf));
                            /* Load sinus* and cosinus* pointers */
                            snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%r12\n", sinus_off);
                            inst_list = add_inst(inst_list, strdup(buf));
                            snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%r13\n", cosinus_off);
                            inst_list = add_inst(inst_list, strdup(buf));
                            /* Call fpc_in_sin_real; write Double result to *sinus */
                            inst_list = add_inst(inst_list, strdup("\tmovl\t$0, %eax\n"));
                            inst_list = add_inst(inst_list, strdup("\tcall\tfpc_in_sin_real\n"));
                            inst_list = add_inst(inst_list, strdup("\tmovsd\t%xmm0, (%r12)\n"));
                            /* Reload theta for fpc_in_cos_real */
                            if (theta_size == 4)
                                snprintf(buf, sizeof(buf), "\tcvtss2sd\t-%d(%%rbp), %%xmm0\n", theta_off);
                            else
                                snprintf(buf, sizeof(buf), "\tmovsd\t-%d(%%rbp), %%xmm0\n", theta_off);
                            inst_list = add_inst(inst_list, strdup(buf));
                            /* Call fpc_in_cos_real; write Double result to *cosinus */
                            inst_list = add_inst(inst_list, strdup("\tmovl\t$0, %eax\n"));
                            inst_list = add_inst(inst_list, strdup("\tcall\tfpc_in_cos_real\n"));
                            inst_list = add_inst(inst_list, strdup("\tmovsd\t%xmm0, (%r13)\n"));
                            break;
                        }
                    }
                    /* For other Intel-syntax asm blocks, wrap with GAS
                     * syntax-switching directives so they assemble correctly
                     * in the otherwise AT&T-syntax output file. */
                    inst_list = add_inst(inst_list, strdup(".intel_syntax noprefix\n"));
                    inst_list = add_inst(inst_list, strdup(src));
                    inst_list = add_inst(inst_list, strdup("\n.att_syntax prefix\n"));
                    break;
                }

                static int asm_block_counter = 0;
                int block_id = asm_block_counter++;
                size_t len = strlen(src);
                /* Allocate generously for label suffix expansion */
                size_t alloc_size = len * 2 + 4096;
                char *cleaned = malloc(alloc_size);
                if (cleaned != NULL)
                {
                    size_t j = 0;
                    for (size_t i = 0; i < len && j < alloc_size - 64; i++)
                    {
                        /* Strip Pascal-style {...} comments */
                        if (src[i] == '{')
                        {
                            while (i < len && src[i] != '}')
                                i++;
                            continue;
                        }
                        /* Strip C++ style // comments */
                        if (src[i] == '/' && i + 1 < len && src[i + 1] == '/')
                        {
                            while (i < len && src[i] != '\n')
                                i++;
                            if (i < len)
                                cleaned[j++] = '\n';
                            continue;
                        }
                        /* Fix jmpq -> jmp for direct jumps (jmpq only valid for indirect) */
                        if (i + 4 < len && strncmp(src + i, "jmpq", 4) == 0 &&
                            (i == 0 || isspace((unsigned char)src[i-1])) &&
                            isspace((unsigned char)src[i + 4]))
                        {
                            cleaned[j++] = 'j';
                            cleaned[j++] = 'm';
                            cleaned[j++] = 'p';
                            i += 3; /* skip "jmpq", loop will advance past 'q' */
                            continue;
                        }
                        /* Make local labels unique by appending block_id suffix.
                         * Match ".L" at start of line (label definition) or after whitespace/comma (reference) */
                        if (src[i] == '.' && i + 1 < len && src[i + 1] == 'L' &&
                            (i == 0 || src[i-1] == '\n' || isspace((unsigned char)src[i-1]) ||
                             src[i-1] == ',' || src[i-1] == '$'))
                        {
                            /* Copy the label name */
                            while (i < len && j < alloc_size - 32 &&
                                   (isalnum((unsigned char)src[i]) || src[i] == '.' || src[i] == '_' || src[i] == 'L'))
                            {
                                cleaned[j++] = src[i++];
                            }
                            /* Append unique suffix */
                            j += snprintf(cleaned + j, 16, "_%d", block_id);
                            i--; /* will be incremented by loop */
                            continue;
                        }
                        cleaned[j++] = src[i];
                    }
                    cleaned[j] = '\0';

                    /* Resolve identifiers in asm blocks:
                       1. For nostackframe: substitute parameter names → ABI registers
                       2. For all asm: resolve Pascal global variables → static labels
                       Pascal is case-insensitive, but ELF symbols are case-sensitive,
                       so we must resolve identifiers to their canonical label names. */
                    {
                        size_t clen = strlen(cleaned);
                        size_t sub_alloc = clen * 3 + 4096;
                        char *substituted = malloc(sub_alloc);
                        if (substituted != NULL) {
                            size_t si = 0, sj = 0;
                            /* Track whether we have seen the instruction mnemonic on
                             * the current line.  In AT&T syntax the FIRST identifier on
                             * each line (after optional whitespace / label) is the
                             * mnemonic — it must never be substituted with a Pascal
                             * variable name, or e.g. `sub %rdi,%rcx` becomes
                             * `__kgpc_program_var_sub_N %rdi,%rcx` when the program
                             * happens to declare a variable named "sub". */
                            int line_has_mnemonic = 0;
                            while (si < clen && sj < sub_alloc - 64) {
                                /* Reset mnemonic flag on each new line. */
                                if (cleaned[si] == '\n') {
                                    line_has_mnemonic = 0;
                                    substituted[sj++] = cleaned[si++];
                                    continue;
                                }
                                if ((isalpha((unsigned char)cleaned[si]) || cleaned[si] == '_') &&
                                    (si == 0 || (!isalnum((unsigned char)cleaned[si-1]) &&
                                                 cleaned[si-1] != '_' && cleaned[si-1] != '%' &&
                                                 cleaned[si-1] != '.')))
                                {
                                    size_t id_start = si;
                                    while (si < clen && (isalnum((unsigned char)cleaned[si]) || cleaned[si] == '_'))
                                        si++;
                                    size_t id_len = si - id_start;
                                    /* Skip past a ':' that makes this a label (not mnemonic). */
                                    size_t si_after = si;
                                    while (si_after < clen && cleaned[si_after] == ' ')
                                        si_after++;
                                    int is_label = (si_after < clen && cleaned[si_after] == ':');
                                    /* If we haven't seen a mnemonic yet and this is not a
                                     * label, it IS the mnemonic — copy verbatim, no subst. */
                                    int is_mnemonic = (!line_has_mnemonic && !is_label);
                                    if (!is_label)
                                        line_has_mnemonic = 1;
                                    char id_buf[256];
                                    int did_substitute = 0;
                                    if (!is_mnemonic && id_len < sizeof(id_buf)) {
                                        memcpy(id_buf, cleaned + id_start, id_len);
                                        id_buf[id_len] = '\0';
                                        /* Try nostackframe parameter substitution first */
                                        if (ctx->is_nostackframe && ctx->asm_param_count > 0) {
                                            for (int pi = 0; pi < ctx->asm_param_count; pi++) {
                                                if (ctx->asm_params[pi].name != NULL &&
                                                    strcasecmp(id_buf, ctx->asm_params[pi].name) == 0) {
                                                    /* Pick register width from parameter type size */
                                                    int ri = ctx->asm_params[pi].reg_index;
                                                    int sz = ctx->asm_params[pi].size_bytes;
                                                    const char *reg;
                                                    switch (sz) {
                                                    case 1:  reg = current_arg_reg8(ri); break;
                                                    case 2:  reg = current_arg_reg16(ri); break;
                                                    case 4:  reg = current_arg_reg32(ri); break;
                                                    default: reg = current_arg_reg64(ri); break;
                                                    }
                                                    if (reg != NULL) {
                                                        int n = snprintf(substituted + sj, sub_alloc - sj, "%s", reg);
                                                        sj += (n > 0 ? (size_t)n : 0);
                                                        did_substitute = 1;
                                                    }
                                                    break;
                                                }
                                            }
                                        }
                                        /* Try resolving as a global variable (case-insensitive) */
                                        if (!did_substitute) {
                                            StackNode_t *var = find_label(id_buf);
                                            if (var != NULL) {
                                                if (var->is_static && var->static_label != NULL) {
                                                    const char *label = var->static_label;
                                                    /* Use RIP-relative addressing for AT&T syntax
                                                     * to avoid 32-bit absolute relocation failures
                                                     * on Windows x64 (image base > 4GB).
                                                     * But skip if the template already has (%rip)
                                                     * immediately after the identifier. */
                                                    int already_has_rip = (si + 6 <= clen &&
                                                        strncmp(cleaned + si, "(%rip)", 6) == 0);
                                                    const char *fmt = (is_intel_syntax || already_has_rip)
                                                        ? "%s" : "%s(%%rip)";
                                                    int n = snprintf(substituted + sj, sub_alloc - sj,
                                                        fmt, label);
                                                    if (n > 0) {
                                                        sj += (size_t)n;
                                                        did_substitute = 1;
                                                    }
                                                } else if (var->offset > 0) {
                                                    int n = snprintf(substituted + sj, sub_alloc - sj, "-%d(%%rbp)", var->offset);
                                                    if (n > 0) {
                                                        sj += (size_t)n;
                                                        did_substitute = 1;
                                                    }
                                                }
                                            }
                                        }
                                        /* Try resolving as a function/procedure name */
                                        if (!did_substitute) {
                                            HashNode_t *proc_node = NULL;
                                            if (FindSymbol(&proc_node, symtab, id_buf) != 0 &&
                                                proc_node != NULL &&
                                                (proc_node->hash_type == HASHTYPE_FUNCTION ||
                                                 proc_node->hash_type == HASHTYPE_PROCEDURE) &&
                                                proc_node->mangled_id != NULL) {
                                                const char *label = proc_node->mangled_id;
                                                size_t llen = strlen(label);
                                                if (sj + llen < sub_alloc - 64) {
                                                    memcpy(substituted + sj, label, llen);
                                                    sj += llen;
                                                    did_substitute = 1;
                                                }
                                            }
                                        }
                                    }
                                    if (!did_substitute) {
                                        memcpy(substituted + sj, cleaned + id_start, id_len);
                                        sj += id_len;
                                    }
                                } else {
                                    substituted[sj++] = cleaned[si++];
                                }
                            }
                            substituted[sj] = '\0';
                            inst_list = add_inst(inst_list, substituted);
                            free(substituted);
                        } else {
                            inst_list = add_inst(inst_list, cleaned);
                        }
                    }
                    free(cleaned);
                }
                else
                {
                    inst_list = add_inst(inst_list, src);
                }
            }
            break;
        }
        case STMT_EXIT:
        {
            inst_list = add_inst(inst_list, "\t# EXIT statement\n");

            int return_is_real = 0;
            int return_size = 0;
            codegen_get_current_return_info(ctx, symtab, &return_is_real, &return_size);
            if (return_size == 0 || (return_is_real == 0 && return_size == 10))
            {
                int slot_is_real = 0;
                int slot_size = 0;
                codegen_get_current_return_slot_info(ctx, &slot_is_real, &slot_size);
                if (return_size == 0)
                    return_size = slot_size;
                if (return_is_real == 0)
                    return_is_real = slot_is_real;
            }

            /* Handle Exit(value) - evaluate value and return it */
            struct Expression *return_expr = stmt->stmt_data.exit_data.return_expr;
            if (return_expr != NULL)
            {
                /* Evaluate the return expression */
                Register_t *result_reg = NULL;
                inst_list = codegen_expr_with_result(return_expr, inst_list, ctx, &result_reg);

                if (result_reg != NULL && codegen_had_error(ctx) == 0)
                {
                    char buffer[128];
                    int expr_is_real = expr_has_type_tag(return_expr, REAL_TYPE) ||
                        expr_has_type_tag(return_expr, EXTENDED_TYPE);
                    int is_real = return_is_real || expr_is_real;

                    int use_qword = return_size >= 8;
                    if (return_size == 0)
                    {
                        KgpcType *expr_type = expr_get_kgpc_type(return_expr);
                        long long expr_size = 0;
                        if (expr_type != NULL)
                            expr_size = kgpc_type_sizeof(expr_type);

                        if (expr_has_type_tag(return_expr, STRING_TYPE) ||
                            expr_has_type_tag(return_expr, POINTER_TYPE) ||
                            expr_has_type_tag(return_expr, INT64_TYPE) ||
                            expr_uses_qword_kgpctype(return_expr) ||
                            expr_size >= 8)
                            use_qword = 1;
                    }

                    if (!is_real)
                    {
                        int value_is_qword = expr_uses_qword_kgpctype(return_expr);
                        if (!value_is_qword && return_expr != NULL)
                        {
                            int return_tag = expr_get_type_tag(return_expr);
                            if (codegen_type_uses_qword(return_tag))
                                value_is_qword = 1;
                        }
                        if (use_qword && !value_is_qword)
                            inst_list = codegen_sign_extend32_to64(inst_list,
                                result_reg->bit_32, result_reg->bit_64);

                        if (use_qword)
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rax\n", result_reg->bit_64);
                        else
                            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %%eax\n", result_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        /* For real types, arithmetic operations leave their result in
                         * xmm0 as a side effect, but simple variable reads put the
                         * 64-bit bit pattern into a GPR.  Always copy GPR → xmm0 so
                         * the caller sees the correct float return value regardless of
                         * which expression form was used. */
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", result_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    free_reg(get_reg_stack(), result_reg);
                }
            }
            else
            {
                StackNode_t *return_var = ctx != NULL ? ctx->current_return_slot : NULL;

                if (return_var != NULL)
                {
                    char buffer[128];

                    /* Check if this function returns a record/ShortString via SRET.
                     * The caller passed a destination buffer pointer in the first
                     * arg-reg slot; codegen_function spilled it to
                     * current_record_return_slot.  EXIT must memcpy the local
                     * Result (return_var) into that buffer, otherwise the early
                     * exit returns 8 stale bytes in %rax instead of populating
                     * the caller's buffer. */
                    if (ctx != NULL && ctx->current_record_return_slot != NULL &&
                        ctx->current_record_return_size > 0)
                    {
                        long long record_size = ctx->current_record_return_size;
                        StackNode_t *dest_slot = ctx->current_record_return_slot;
                        if (codegen_target_is_windows())
                        {
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t-%d(%%rbp), %%rcx\n", dest_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer),
                                "\tleaq\t-%d(%%rbp), %%rdx\n", return_var->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t$%lld, %%r8\n", record_size);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t-%d(%%rbp), %%rdi\n", dest_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer),
                                "\tleaq\t-%d(%%rbp), %%rsi\n", return_var->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t$%lld, %%rdx\n", record_size);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        inst_list = codegen_vect_reg(inst_list, 0);
                        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                        free_arg_regs();
                        /* Mirror the regular epilogue: leave the SRET pointer in
                         * %rax so callers that read the return value (instead of
                         * the buffer directly) still get the correct address. */
                        snprintf(buffer, sizeof(buffer),
                            "\tmovq\t-%d(%%rbp), %%rax\n", dest_slot->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    /* Check if this function returns a dynamic array */
                    else if (ctx != NULL && ctx->returns_dynamic_array)
                    {
                        /* For dynamic arrays, call kgpc_dynarray_clone_descriptor
                         * to return a cloned descriptor, not the local one */
                        if (codegen_target_is_windows())
                        {
                            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", return_var->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", ctx->dynamic_array_descriptor_size);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n", return_var->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%esi\n", ctx->dynamic_array_descriptor_size);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        /* Zero vector register count before call (required by ABI) */
                        inst_list = codegen_vect_reg(inst_list, 0);
                        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_clone_descriptor");
                        free_arg_regs();
                    }
                    else
                    {
                        int use_qword = return_size >= 8;
                        if (return_size == 0 && return_var->size >= 8)
                            use_qword = 1;

                        /* Check element_size for unaligned Single type (4 bytes) */
                        long long unaligned_return_size = return_var->element_size > 0 ? return_var->element_size : return_var->size;
                        if (return_is_real && return_var->element_size == 10)
                            snprintf(buffer, sizeof(buffer), "\tfldt\t-%d(%%rbp)\n", return_var->offset);
                        else if (return_is_real && unaligned_return_size <= 4)
                            snprintf(buffer, sizeof(buffer), "\tmovss\t-%d(%%rbp), %%xmm0\n", return_var->offset);
                        else if (return_is_real)
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t-%d(%%rbp), %%xmm0\n", return_var->offset);
                        else if (use_qword)
                            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", return_var->offset);
                        else
                            snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%eax\n", return_var->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
            }
            
            if (codegen_has_finally(ctx))
            {
                char exit_label[18];
                gen_label(exit_label, sizeof(exit_label), ctx);
                /* Exit unwinds everything, so limit_depth is 0 */
                inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, exit_label, 0);
                char buffer[32];
                snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
                inst_list = add_inst(inst_list, buffer);
            }
            /* Restore callee-saved registers before leaving the frame */
            if (ctx->callee_save_rbx_offset > 0) {
                char buf[64];
                snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
                inst_list = add_inst(inst_list, buf);
            }
            if (ctx->callee_save_r12_offset > 0) {
                char buf[64];
                snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
                inst_list = add_inst(inst_list, buf);
            }
            if (ctx->callee_save_r13_offset > 0) {
                char buf[64];
                snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
                inst_list = add_inst(inst_list, buf);
            }
            if (ctx->callee_save_r14_offset > 0) {
                char buf[64];
                snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
                inst_list = add_inst(inst_list, buf);
            }
            if (ctx->callee_save_r15_offset > 0) {
                char buf[64];
                snprintf(buf, sizeof(buf), "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
                inst_list = add_inst(inst_list, buf);
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
        case STMT_ON_EXCEPTION:
            inst_list = codegen_on_exception(stmt, inst_list, ctx, symtab);
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

