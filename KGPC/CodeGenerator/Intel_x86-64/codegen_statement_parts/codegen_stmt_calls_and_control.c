#include "../codegen_stmt_internal.h"

static KgpcType *codegen_expr_lookup_symtab_type(const struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL || symtab == NULL || expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
        return NULL;

    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, expr->expr_data.id) == 0 || node == NULL)
        return NULL;

    return node->type;
}

static int codegen_expr_is_unsigned_with_symtab(const struct Expression *expr, SymTab_t *symtab)
{
    KgpcType *type = codegen_expr_lookup_symtab_type(expr, symtab);
    if (type != NULL)
    {
        if (type->type_alias != NULL && type->type_alias->range_known)
            return (type->type_alias->range_start >= 0);

        switch (codegen_tag_from_kgpc(type))
        {
            case BYTE_TYPE:
            case WORD_TYPE:
            case LONGWORD_TYPE:
            case QWORD_TYPE:
            case CHAR_TYPE:
            case BOOL:
                return 1;
            default:
                break;
        }
    }

    switch (expr_get_type_tag(expr))
    {
        case BYTE_TYPE:
        case WORD_TYPE:
        case LONGWORD_TYPE:
        case QWORD_TYPE:
        case CHAR_TYPE:
        case BOOL:
            return 1;
        default:
            return 0;
    }
}

static int codegen_expr_is_signed_with_symtab(const struct Expression *expr, SymTab_t *symtab)
{
    return !codegen_expr_is_unsigned_with_symtab(expr, symtab);
}

static int codegen_expr_uses_qword_with_symtab(const struct Expression *expr, SymTab_t *symtab)
{
    if (expr_uses_qword_kgpctype(expr))
        return 1;

    KgpcType *type = codegen_expr_lookup_symtab_type(expr, symtab);
    return (type != NULL) ? kgpc_type_uses_qword(type) : 0;
}

static int codegen_stmt_expr_is_type_identifier(const struct Expression *expr,
    CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL || ctx->symtab == NULL ||
        expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
        return 0;

    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) == 0 || node == NULL)
        return 0;

    return node->hash_type == HASHTYPE_TYPE;
}

static int codegen_stmt_type_is_class_vmt_value(const KgpcType *type)
{
    if (type == NULL)
        return 0;

    if (type->type_alias != NULL && type->type_alias->is_class_reference)
        return 1;

    if (type->kind == TYPE_KIND_POINTER && type->info.points_to != NULL)
    {
        if (type->info.points_to->type_alias != NULL &&
            type->info.points_to->type_alias->is_class_reference)
        {
            return 1;
        }
    }

    return 0;
}

static int codegen_stmt_first_arg_is_class_vmt_value(const ListNode_t *args_expr,
    CodeGenContext *ctx)
{
    if (args_expr == NULL || args_expr->cur == NULL || ctx == NULL || ctx->symtab == NULL)
        return 0;

    const struct Expression *self_expr = (const struct Expression *)args_expr->cur;
    if (self_expr == NULL)
        return 0;

    if (codegen_stmt_expr_is_type_identifier(self_expr, ctx))
        return 1;

    if (codegen_stmt_type_is_class_vmt_value(self_expr->resolved_kgpc_type))
        return 1;

    if (self_expr->type == EXPR_VAR_ID && self_expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, self_expr->expr_data.id) != 0 &&
            node != NULL && codegen_stmt_type_is_class_vmt_value(node->type))
        {
            return 1;
        }
    }

    return 0;
}

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

int codegen_expr_is_extended_storage(const struct Expression *expr)
{
    KgpcType *type = expr_get_kgpc_type(expr);
    return kgpc_type_is_extended(type);
}

ListNode_t *codegen_assign_extended_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    Register_t *dest_addr = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_addr);
    if (codegen_had_error(ctx) || dest_addr == NULL)
    {
        if (dest_addr != NULL)
            free_reg(get_reg_stack(), dest_addr);
        return inst_list;
    }

    char buffer[CODEGEN_MAX_INST_BUF];
    if (codegen_expr_is_extended_storage(src_expr) && codegen_expr_is_addressable(src_expr))
    {
        Register_t *src_addr = NULL;
        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_addr);
        if (codegen_had_error(ctx) || src_addr == NULL)
        {
            free_reg(get_reg_stack(), dest_addr);
            if (src_addr != NULL)
                free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }

        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
        free_arg_regs();
        free_reg(get_reg_stack(), src_addr);
        free_reg(get_reg_stack(), dest_addr);
        return inst_list;
    }

    inst_list = codegen_materialize_extended_expr(src_expr, inst_list, ctx, dest_addr);
    free_reg(get_reg_stack(), dest_addr);
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
    char buffer[CODEGEN_MAX_INST_BUF];
    struct Expression *var_expr, *assign_expr;
    int offset;

    var_expr = stmt->stmt_data.var_assign_data.var;
    assign_expr = stmt->stmt_data.var_assign_data.expr;
    codegen_hydrate_array_literal_from_lhs(var_expr, assign_expr, ctx);

    /* Handle string typecast on LHS: e.g., RawByteString(ptr) := 'value'
     * The typecast means this pointer should be treated as a string variable.
     * Generate a string assignment (kgpc_string_assign) instead of direct store. */
    int lhs_is_string_typecast = 0;
    if (var_expr != NULL && var_expr->type == EXPR_TYPECAST &&
        var_expr->expr_data.typecast_data.expr != NULL)
    {
        int target_type = var_expr->expr_data.typecast_data.target_type;
        if (is_string_type(target_type))
            lhs_is_string_typecast = 1;
        else
            var_expr = var_expr->expr_data.typecast_data.expr;
    }

    /* Handle string typecast on LHS: e.g., RawByteString(ptr) := ''
     * Treat the pointer location as a string variable — get its address
     * and call kgpc_string_assign so refcounting works properly.
     * Special case: RHS is empty string '' → store nil directly (FPC
     * AnsiString semantics: empty string == nil pointer). */
    if (lhs_is_string_typecast)
    {
        struct Expression *inner = var_expr->expr_data.typecast_data.expr;

        /* Detect empty string literal: RawByteString(ptr) := ''
         * In FPC, this means "set pointer to nil". */
        int rhs_is_empty = (assign_expr != NULL && assign_expr->type == EXPR_STRING &&
            assign_expr->expr_data.string != NULL &&
            assign_expr->expr_data.string[0] == '\0');
        if (rhs_is_empty)
        {
            /* Store nil directly into the pointer location */
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(inner, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }
            char buffer[CODEGEN_MAX_INST_BUF];
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, (%s)\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        /* Non-empty string: evaluate RHS and call kgpc_string_assign */
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        /* Get address of the inner pointer (the location to assign to) */
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        inst_list = codegen_call_string_assign(inst_list, ctx, addr_reg, value_reg);
        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    if (codegen_expr_is_mp_integer(var_expr))
    {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

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

    int dest_is_static_array = (var_expr->is_array_expr && !expr_is_dynamic_array(var_expr)) ||
        expr_is_static_array_like(var_expr, ctx);
    int src_is_static_array = expr_is_static_array_like(assign_expr, ctx);

    if (dest_is_static_array && src_is_static_array)
    {
        /* Static array assignment (including record fields) */
        return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
    }
    /* EXPR_ARRAY_LITERAL is created with array_is_dynamic=1 by default,
     * so expr_is_static_array_like returns 0 for it.  When the destination
     * is a known static array (e.g. var of a named array type), we must
     * still route through codegen_assign_static_array to perform a memcpy
     * of the literal data instead of storing a dangling stack-descriptor
     * pointer into the variable. */
    if (dest_is_static_array && assign_expr != NULL &&
        assign_expr->type == EXPR_ARRAY_LITERAL)
    {
        return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
    }
    else if (var_expr->type == EXPR_RECORD_ACCESS)
    {
        struct RecordField *field = codegen_lookup_record_field(var_expr);
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL)
        {
            const char *fid = var_expr->expr_data.record_access_data.field_id;
            fprintf(stderr, "[codegen] static_array_check: field=%s found=%p is_array=%d\n",
                fid ? fid : "<null>", (void*)field, field ? field->is_array : -1);
        }
        if (field != NULL)
        {
            int field_is_static_array = (field->is_array && !field->array_is_open);
            if (!field_is_static_array && field->type_id != NULL)
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array && !alias->is_open_array)
                    field_is_static_array = 1;
            }

            if (field_is_static_array && src_is_static_array)
                return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
            /* EXPR_ARRAY_LITERAL is created with array_is_dynamic=1 by default,
             * but when assigned to a static array field (e.g., optypes: array[0..3]
             * of int64 in a packed record), it must be treated as a static array
             * copy, not a pointer store. */
            if (field_is_static_array && assign_expr != NULL &&
                assign_expr->type == EXPR_ARRAY_LITERAL)
                return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);

            /* Large set fields (> 4 bytes, e.g. set of enum with > 32 elements)
             * need memory-based construction via codegen_assign_record_value,
             * which will force the set literal into the 32-byte path. */
            if (field->type == SET_TYPE && field->has_cached_layout && field->cached_size > 4)
                return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);
        }
    }

    if (assign_expr != NULL && assign_expr->type == EXPR_RECORD_CONSTRUCTOR)
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    KgpcType *lhs_kgpc_type = expr_get_kgpc_type(var_expr);
    KgpcType *rhs_kgpc_type = expr_get_kgpc_type(assign_expr);
    int lhs_is_pointer_destination = 0;
    int rhs_is_address_value = 0;
    int rhs_is_addressable_record = 0;

    if (lhs_kgpc_type != NULL &&
        (kgpc_type_is_pointer(lhs_kgpc_type) || kgpc_type_is_procedure(lhs_kgpc_type)))
    {
        lhs_is_pointer_destination = 1;
    }
    else
    {
        int lhs_tag = expr_get_type_tag(var_expr);
        lhs_is_pointer_destination = (lhs_tag == POINTER_TYPE || lhs_tag == PROCEDURE);
    }

    rhs_is_address_value = (assign_expr != NULL &&
        (assign_expr->type == EXPR_ADDR || assign_expr->type == EXPR_ADDR_OF_PROC));
    rhs_is_addressable_record = (assign_expr != NULL &&
        rhs_kgpc_type != NULL &&
        kgpc_type_is_record(rhs_kgpc_type) &&
        (assign_expr->type == EXPR_VAR_ID || assign_expr->type == EXPR_RECORD_ACCESS));

    if (lhs_is_pointer_destination && rhs_is_addressable_record && !rhs_is_address_value)
    {
        assign_expr = mk_addressof(assign_expr->line_num, assign_expr);
        rhs_is_address_value = 1;
    }

    int lhs_is_record_value = 0;
    if (lhs_kgpc_type != NULL)
        lhs_is_record_value = kgpc_type_is_record(lhs_kgpc_type);
    else if (expr_get_type_tag(var_expr) == RECORD_TYPE && !var_expr->is_array_expr)
        lhs_is_record_value = 1;

    if (lhs_is_pointer_destination && rhs_is_address_value)
        lhs_is_record_value = 0;

    if (lhs_is_record_value)
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    if (codegen_expr_is_extended_storage(var_expr))
        return codegen_assign_extended_value(var_expr, assign_expr, inst_list, ctx);

    if (var_expr != NULL && assign_expr != NULL &&
        var_expr->type == EXPR_RECORD_ACCESS &&
        assign_expr->type == EXPR_ADDR_OF_PROC &&
        var_expr->expr_data.record_access_data.field_id != NULL &&
        pascal_identifier_equals(var_expr->expr_data.record_access_data.field_id, "finish_module"))
    {
        Register_t *addr_reg = NULL;
        Register_t *value_reg = NULL;
        inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL || value_reg == NULL)
        {
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
            value_reg->bit_64, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        StackNode_t *self_slot = find_label("Self");
        if (self_slot != NULL)
        {
            Register_t *self_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (self_reg == NULL)
                self_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (self_reg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    self_slot->offset, self_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, 8(%s)\n",
                    self_reg->bit_64, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), self_reg);
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    /* Character sets (set of char) need special handling like records due to 32-byte size */
    if (expr_get_type_tag(var_expr) == SET_TYPE && expr_is_char_set_ctx(var_expr, ctx))
        return codegen_assign_record_value(var_expr, assign_expr, inst_list, ctx);

    /* ShortStrings returned via SRET still need shortstring-aware copy semantics.
     * Route them through the static-array helper so sized shortstrings preserve
     * their actual capacity instead of being treated as 256-byte records. */
    if ((expr_get_type_tag(var_expr) == SHORTSTRING_TYPE ||
         codegen_expr_is_shortstring_array(var_expr)) &&
        !expr_is_dynamic_array(var_expr) &&
        assign_expr != NULL && assign_expr->type == EXPR_FUNCTION_CALL &&
        (expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE ||
         codegen_expr_is_shortstring_value_ctx(assign_expr, ctx)))
    {
        /* Check resolved_kgpc_type: if it's an array of char (not shortstring),
         * this is a plain char array mislabeled as shortstring. Don't intercept. */
        int really_shortstring = 1;
        KgpcType *lhs_ktype = expr_get_kgpc_type(var_expr);
        if (lhs_ktype != NULL && kgpc_type_is_array(lhs_ktype) &&
            !kgpc_type_is_shortstring(lhs_ktype))
        {
            KgpcType *elem = lhs_ktype->info.array_info.element_type;
            if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                (elem->info.primitive_type_tag == CHAR_TYPE ||
                 elem->info.primitive_type_tag == BYTE_TYPE))
                really_shortstring = 0;
        }
        if (really_shortstring)
        {
            Register_t *dest_addr = NULL;
            Register_t *src_addr = NULL;
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
            {
                long long direct_size = expr_effective_size_bytes(var_expr);
                if (direct_size > 1 && direct_size <= INT_MAX)
                    array_size = (int)direct_size;
                else
                    array_size = 256;
            }

            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &dest_addr);
            if (codegen_had_error(ctx) || dest_addr == NULL)
            {
                if (dest_addr != NULL)
                    free_reg(get_reg_stack(), dest_addr);
                return inst_list;
            }

            inst_list = codegen_address_for_expr(assign_expr, inst_list, ctx, &src_addr);
            if (codegen_had_error(ctx) || src_addr == NULL)
            {
                free_reg(get_reg_stack(), dest_addr);
                if (src_addr != NULL)
                    free_reg(get_reg_stack(), src_addr);
                return inst_list;
            }

            inst_list = codegen_call_shortstring_copy(inst_list, ctx, dest_addr, array_size, src_addr);
            free_reg(get_reg_stack(), dest_addr);
            free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }
        /* Fall through to EXPR_RECORD_ACCESS handler which uses char_array copy */
    }

    /* ShortString record field receiving a char literal or char-typed value.
     * codegen_expr_with_result returns the char's integer ordinal in a register,
     * which the EXPR_RECORD_ACCESS handler below would store verbatim —
     * overwriting the length byte at offset 0.  Instead, promote the char to a
     * one-character heap string and use kgpc_string_to_shortstring so that the
     * length byte and character data are written at the correct offsets. */
    if ((expr_get_type_tag(var_expr) == SHORTSTRING_TYPE ||
         codegen_expr_is_shortstring_array(var_expr)) &&
        !expr_is_dynamic_array(var_expr) &&
        assign_expr != NULL &&
        assign_expr->type != EXPR_FUNCTION_CALL &&
        var_expr->type == EXPR_RECORD_ACCESS)
    {
        int rhs_tag = expr_get_type_tag(assign_expr);
        /* expr_get_type_tag returns CHAR_TYPE for string[N] where N < 255
         * because those are internally array[0..N] of char and the
         * shortstring heuristic only fires for upper==255.  Guard against
         * misidentifying a shortstring variable/parameter as a bare char. */
        int rhs_is_shortstring = codegen_expr_is_shortstring_rhs(assign_expr, ctx) ||
                                 codegen_expr_is_shortstring_array(assign_expr);
        int rhs_is_char = !rhs_is_shortstring &&
                          (rhs_tag == CHAR_TYPE ||
                           (is_integer_type(rhs_tag) && assign_expr->type == EXPR_INUM));
        int rhs_is_string_like = (rhs_tag == STRING_TYPE ||
                                  rhs_tag == SHORTSTRING_TYPE ||
                                  rhs_is_shortstring ||
                                  assign_expr->type == EXPR_STRING);
        if (rhs_is_char || rhs_is_string_like)
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
            {
                long long direct_size = expr_effective_size_bytes(var_expr);
                if (direct_size > 1 && direct_size <= INT_MAX)
                    array_size = (int)direct_size;
                else
                    array_size = 256;
            }
            Register_t *value_reg = NULL;
            inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx) || value_reg == NULL)
            {
                free_reg(get_reg_stack(), addr_reg);
                if (value_reg != NULL)
                    free_reg(get_reg_stack(), value_reg);
                return inst_list;
            }
            if (rhs_is_char)
            {
                /* Promote char ordinal in register to a heap string,
                 * then copy into ShortString via kgpc_string_to_shortstring. */
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }
            if (codegen_expr_is_shortstring_rhs(assign_expr, ctx))
                inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reg, array_size, value_reg);
            else
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
    }

    if (var_expr->type == EXPR_VAR_ID)
    {
        if (expr_is_dynamic_array(var_expr))
        {
            inst_list = codegen_assign_dynamic_array(var_expr, assign_expr, inst_list, ctx);
            return inst_list;
        }

        int scope_depth = 0;
        var = find_label_with_depth(var_expr->expr_data.id, &scope_depth);

        if (var == NULL)
        {
            HashNode_t *target_node = NULL;
            if (FindSymbol(&target_node, ctx->symtab, var_expr->expr_data.id) != 0 &&
                target_node != NULL)
            {
                if (target_node->mangled_id != NULL)
                {
                    var = find_label_with_depth(target_node->mangled_id, &scope_depth);
                }
                if (var == NULL &&
                    (target_node->hash_type == HASHTYPE_FUNCTION_RETURN ||
                     target_node->hash_type == HASHTYPE_FUNCTION))
                {
                    long long size_bytes = 0;
                    if (var_expr->resolved_kgpc_type != NULL)
                        size_bytes = kgpc_type_sizeof(var_expr->resolved_kgpc_type);
                    if (size_bytes <= 0 || size_bytes > INT_MAX)
                        size_bytes = CODEGEN_POINTER_SIZE_BYTES;

                    var = add_l_x(var_expr->expr_data.id, (int)size_bytes);
                    scope_depth = 0;
                }
            }
        }

        
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);

        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        /* When assigning an extended-returning function call to a Real variable,
         * value_reg holds a pointer to the sret buffer containing the 10-byte
         * extended value.  Convert it to a double via kgpc_load_extended_to_bits
         * so the store below writes the correct IEEE-754 double bits. */
        int var_type = expr_get_type_tag(var_expr);
        if (var_type == REAL_TYPE &&
            assign_expr != NULL && assign_expr->type == EXPR_FUNCTION_CALL &&
            expr_returns_sret(assign_expr) &&
            codegen_expr_involves_extended(assign_expr))
        {
            char ext_buf[128];
            if (codegen_target_is_windows())
                snprintf(ext_buf, sizeof(ext_buf), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
            else
                snprintf(ext_buf, sizeof(ext_buf), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, ext_buf);
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
            free_arg_regs();
            /* Result is in %rax as double-bit-pattern; move to value_reg */
            snprintf(ext_buf, sizeof(ext_buf), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, ext_buf);
        }

        int assign_type = expr_get_type_tag(assign_expr);
        int skip_real_coercion = 0;
        if (var != NULL && var_type == REAL_TYPE)
        {
            long long unaligned_size_probe = var->element_size > 0 ? var->element_size : var->size;
            if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
            {
                long long field_size_probe = codegen_record_field_effective_size(var_expr, ctx);
                if (field_size_probe > 0)
                    unaligned_size_probe = field_size_probe;
            }
            if (is_single_float_type(var_type, unaligned_size_probe) &&
                (is_integer_type(assign_type) || assign_type == BOOL ||
                 assign_type == CHAR_TYPE || assign_type == ENUM_TYPE))
            {
                skip_real_coercion = 1;
            }
        }
        int coerced_to_real = 0;
        if (!skip_real_coercion)
        {
            inst_list = codegen_maybe_convert_int_like_to_real(var_type, assign_expr,
                value_reg, inst_list, &coerced_to_real);
        }

        /* Handle string assignment to string variables */
        if (var_type == STRING_TYPE)
        {
            /* Under {$H-}, a variable with STRING_TYPE tag may actually be backed
             * by ShortString storage (256-byte stack buffer with length byte).
             * Detect this and route to shortstring-aware assignment instead of
             * kgpc_string_assign which would corrupt the buffer by writing a pointer. */
            int lhs_is_actually_shortstring =
                codegen_expr_is_shortstring_array(var_expr) ||
                codegen_expr_is_shortstring_value_ctx(var_expr, ctx);
            if (lhs_is_actually_shortstring)
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
                if (codegen_had_error(ctx) || addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), value_reg);
                    if (addr_reg != NULL)
                        free_reg(get_reg_stack(), addr_reg);
                    return inst_list;
                }

                /* codegen_get_shortstring_capacity returns 256 when capacity
                 * cannot be determined; codegen_call_string_to_shortstring
                 * also guards against invalid (<= 1) values internally. */
                int array_size = codegen_get_shortstring_capacity(var_expr, ctx);

                if (codegen_expr_is_shortstring_rhs(assign_expr, ctx))
                {
                    /* Both sides are ShortString — copy preserving the length byte */
                    inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reg, array_size, value_reg);
                }
                else
                {
                    /* RHS is an AnsiString or string literal — convert to ShortString
                     * (kgpc_string_to_shortstring writes length byte + payload) */
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
                }
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int inner_is_shortstring = codegen_expr_is_shortstring_rhs(assign_expr, ctx);

            /* If assigning a char to string, promote it first.
             * Also check for typecasts from char to string (e.g. AnsiString(char_value))
             * where expr_get_type_tag returns STRING_TYPE but the actual value is a char. */
            int assign_type = expr_get_type_tag(assign_expr);
            int target_is_wide_string = codegen_expr_is_wide_string_value(var_expr);
            int source_is_wide_string = codegen_expr_is_wide_string_value(assign_expr);
            int source_needs_wide_promotion = (!source_is_wide_string);
            if (!source_needs_wide_promotion &&
                assign_expr != NULL &&
                assign_expr->type == EXPR_TYPECAST &&
                assign_expr->expr_data.typecast_data.expr != NULL &&
                codegen_expr_is_wide_string_value(assign_expr) &&
                !codegen_expr_is_wide_string_value(assign_expr->expr_data.typecast_data.expr) &&
                (expr_has_type_tag(assign_expr->expr_data.typecast_data.expr, STRING_TYPE) ||
                 expr_has_type_tag(assign_expr->expr_data.typecast_data.expr, SHORTSTRING_TYPE) ||
                 assign_expr->expr_data.typecast_data.expr->type == EXPR_STRING))
            {
                source_needs_wide_promotion = 1;
            }
            if (!inner_is_shortstring && assign_type == CHAR_TYPE)
            {
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }
            else if (!inner_is_shortstring &&
                     assign_expr != NULL && assign_expr->type == EXPR_TYPECAST &&
                     assign_expr->expr_data.typecast_data.expr != NULL &&
                     expr_get_type_tag(assign_expr->expr_data.typecast_data.expr) == CHAR_TYPE)
            {
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }

            int rhs_lower = 0;
            int rhs_upper = -1;
            int rhs_is_shortstring = 0;
            if (codegen_get_char_array_bounds(assign_expr, ctx, &rhs_lower, &rhs_upper, &rhs_is_shortstring) &&
                !rhs_is_shortstring)
            {
                Register_t *addr_reg = NULL;
                Register_t *src_reg = NULL;
                inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
                inst_list = codegen_address_for_expr(assign_expr, inst_list, ctx, &src_reg);
                free_reg(get_reg_stack(), value_reg);
                if (codegen_had_error(ctx) || addr_reg == NULL || src_reg == NULL)
                {
                    if (addr_reg != NULL)
                        free_reg(get_reg_stack(), addr_reg);
                    if (src_reg != NULL)
                        free_reg(get_reg_stack(), src_reg);
                    return inst_list;
                }

                int src_len = rhs_upper - rhs_lower + 1;
                if (src_len < 0)
                    src_len = 0;
                inst_list = codegen_call_string_assign_from_char_array(inst_list, ctx,
                    addr_reg, src_reg, src_len);
                free_reg(get_reg_stack(), addr_reg);
                free_reg(get_reg_stack(), src_reg);
                return inst_list;
            }

            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            /* When the RHS is a ShortString (e.g. from strpas/StrPas), use a dedicated
             * conversion function that strips the length byte and builds a proper AnsiString.
             * Also handle typecasts like TFormatString(HexStr(...)) where the outer type
             * is AnsiString but the inner expression returns ShortString. */
            if (inner_is_shortstring)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_string_assign_from_shortstring");
            else if (target_is_wide_string && source_is_wide_string)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_unicodestring_assign");
            else if (target_is_wide_string && source_needs_wide_promotion)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_unicodestring_assign_from_string");
            else if (!target_is_wide_string && source_is_wide_string)
                inst_list = codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg,
                    "kgpc_string_assign_from_unicodestring");
            else
                inst_list = codegen_call_string_assign(inst_list, ctx, addr_reg, value_reg);
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        
        /* Handle shortstring assignment to char arrays */
        int array_lower = 0;
        int array_upper = -1;
        int array_is_shortstring = 0;
        if (assign_expr != NULL &&
            assign_expr->type != EXPR_ARRAY_LITERAL &&
            codegen_get_char_array_bounds(var_expr, ctx, &array_lower, &array_upper, &array_is_shortstring) &&
            (codegen_expr_is_shortstring_value_local(assign_expr) ||
             expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int array_size = array_upper - array_lower + 1;
            if (array_size < 0)
                array_size = 0;
            if (array_is_shortstring)
            {
                if (var_expr != NULL && var_expr->type == EXPR_VAR_ID &&
                    codegen_is_current_return_var_id(var_expr, ctx))
                {
                    int short_capacity = codegen_get_current_return_shortstring_capacity(
                        ctx, ctx != NULL ? ctx->symtab : NULL);
                    if (short_capacity > 1)
                        array_size = short_capacity;
                }
                int short_capacity = codegen_get_shortstring_capacity(var_expr, ctx);
                if (short_capacity > 1)
                    array_size = short_capacity;
            }

            if (array_is_shortstring)
            {
                if (assign_expr != NULL && assign_expr->type == EXPR_STRING)
                {
                    /* Literal RHS is a C string pointer; build a proper ShortString
                     * (length byte + payload) instead of treating it as ShortString data. */
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
                }
                else
                {
                    /* Dest is ShortString — use shortstring-to-shortstring copy
                     * which preserves the length byte.
                     * kgpc_shortstring_to_shortstring(dest, dest_size, src) */
                    inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reg, array_size, value_reg);
                }
            }
            else
            {
                inst_list = codegen_call_shortstring_to_char_array(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        /* Handle string literal assignment to char arrays */
        array_lower = 0;
        array_upper = -1;
        array_is_shortstring = 0;
        if (expr_get_type_tag(assign_expr) == STRING_TYPE &&
            codegen_get_char_array_bounds(var_expr, ctx, &array_lower, &array_upper, &array_is_shortstring))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int array_size = array_upper - array_lower + 1;
            if (array_size < 0)
                array_size = 0;
            
            if (array_is_shortstring)
            {
                /* Use ShortString-specific copy that sets length byte at index 0 */
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            else
            {
                /* Regular char array copy */
                inst_list = codegen_call_string_to_char_array(inst_list, ctx, addr_reg, value_reg, array_size);
            }
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        /* Handle string literal assignment to SHORTSTRING_TYPE elements (e.g., Names[0] := 'ONE') */
        /* Also check codegen_array_access_targets_shortstring for cases where type info is not available
         * (like auto-generated initializer statements from const arrays) */
        int targets_shortstring = codegen_array_access_targets_shortstring(var_expr, ctx);
        int record_targets_shortstring = 0;
        if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
        {
            struct RecordField *target_field = codegen_lookup_record_field(var_expr);
            if (target_field != NULL && target_field->type == SHORTSTRING_TYPE)
                record_targets_shortstring = 1;
        }
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL && var_expr->type == EXPR_ARRAY_ACCESS)
        {
            struct Expression *base = var_expr->expr_data.array_access_data.array_expr;
            fprintf(stderr, "[codegen] var_assignment: var_type=%d, targets_shortstring=%d, assign_type=%d",
                var_type, targets_shortstring, expr_get_type_tag(assign_expr));
            if (base != NULL && base->type == EXPR_VAR_ID)
                fprintf(stderr, ", base_id=%s", base->expr_data.id);
            fprintf(stderr, "\n");
        }
        if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
             codegen_expr_is_shortstring_array(var_expr) ||
             targets_shortstring || record_targets_shortstring))
        {
            int rhs_lower = 0;
            int rhs_upper = -1;
            int rhs_is_shortstring = 0;
            int lhs_lower = 0;
            int lhs_upper = -1;
            int lhs_is_shortstring = 0;

            if (codegen_get_char_array_bounds(assign_expr, ctx, &rhs_lower, &rhs_upper, &rhs_is_shortstring) &&
                !rhs_is_shortstring &&
                codegen_get_char_array_bounds(var_expr, ctx, &lhs_lower, &lhs_upper, &lhs_is_shortstring))
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
                if (codegen_had_error(ctx) || addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), value_reg);
                    if (addr_reg != NULL)
                        free_reg(get_reg_stack(), addr_reg);
                    return inst_list;
                }

                int src_len = rhs_upper - rhs_lower + 1;
                int dest_size = lhs_upper - lhs_lower + 1;
                if (src_len < 0) src_len = 0;
                if (dest_size < 0) dest_size = 0;

                inst_list = codegen_call_char_array_to_shortstring(inst_list, ctx, addr_reg, value_reg,
                    src_len, dest_size);
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            if (expr_get_type_tag(assign_expr) != STRING_TYPE)
            {
                /* Fall through to default handling if RHS is not a string literal/value. */
            }
            else
            {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            /* For shortstrings, use the array bounds to determine size.
             * If bounds are not available, default to 256 (standard ShortString). */
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
                array_size = 256;
            
            /* Use ShortString-specific copy that sets length byte at index 0 */
            inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
            }
        }

        /* ShortString assignment should use shortstring-aware copy semantics,
         * not generic record copies. */
        if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
             codegen_expr_is_shortstring_array(var_expr) ||
             targets_shortstring || record_targets_shortstring) &&
            expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE)
        {
            return codegen_assign_static_array(var_expr, assign_expr, inst_list, ctx);
        }

        /* Handle CHAR assignment to ShortString arrays - set length=1 and store char at position 1 */
        if (codegen_expr_is_shortstring_array(var_expr) &&
            expr_get_type_tag(assign_expr) == CHAR_TYPE)
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
            {
                free_reg(get_reg_stack(), value_reg);
                if (addr_reg != NULL)
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            char buffer[128];
            /* Store length=1 at position 0 */
            snprintf(buffer, sizeof(buffer), "\tmovb\t$1, (%s)\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Store the character at position 1 */
            const char *char_reg = register_name8(value_reg);
            if (char_reg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, 1(%s)\n", char_reg, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                codegen_report_error(ctx, "ERROR: Could not get 8-bit register name for character storage");
            }
            
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        reg = value_reg;

        if(var != NULL)
        {
            int use_qword = codegen_type_uses_qword(var_type);
            /* Override for Single type (4-byte float): check actual storage size
             * Use element_size which stores the unaligned size, not size which may be padded */
            long long unaligned_size = var->element_size > 0 ? var->element_size : var->size;
            if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
            {
                long long field_size = codegen_record_field_effective_size(var_expr, ctx);
                if (field_size > 0)
                    unaligned_size = field_size;
            }
            int is_single_target = is_single_float_type(var_type, unaligned_size) && !var->is_reference;
            if (is_single_target)
                use_qword = 0;
            if (!var->is_reference && var->size >= 8 &&
                (var_expr == NULL || var_expr->type != EXPR_RECORD_ACCESS) &&
                !is_single_target)
                use_qword = 1;
            
            /* For Single targets with real source, convert double to single precision.
             * Only convert if the source expression is a real type (double), not integer etc. */
            if (is_single_target && assign_type == REAL_TYPE)
            {
                /* Real expressions are materialized at double precision in registers.
                 * Narrow to Single before storing into a Single target. */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = add_inst(inst_list, "\tcvtsd2ss\t%xmm0, %xmm0\n");
                snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
            else if (is_single_target &&
                (is_integer_type(assign_type) || assign_type == BOOL ||
                 assign_type == CHAR_TYPE || assign_type == ENUM_TYPE))
            {
                /* Integer-like source to Single target requires numeric conversion,
                 * not raw bit reinterpretation. */
                snprintf(buffer, sizeof(buffer), "\tcvtsi2ss\t%s, %%xmm0\n", reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
            
            int use_byte = 0;
            int use_word = 0;
            const char *value_reg8 = NULL;
            const char *value_reg16 = NULL;
            long long target_size = (var_expr->type == EXPR_RECORD_ACCESS) ?
                codegen_record_field_effective_size(var_expr, ctx) :
                expr_effective_size_bytes(var_expr);
            /* Cross-check with resolved_kgpc_type for sub-dword fields
             * (e.g. Integer=SmallInt in FPC mode where type tag says 4
             * but actual storage is 2 bytes). */
            if (target_size == 4 && var_expr != NULL &&
                var_expr->resolved_kgpc_type != NULL)
            {
                long long resolved_size = kgpc_type_sizeof(var_expr->resolved_kgpc_type);
                if (resolved_size > 0 && resolved_size < 4)
                    target_size = resolved_size;
            }
            if (!use_qword && var_type == CHAR_TYPE)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                {
                    use_byte = 1;
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
            }
            else if (!use_qword && target_size == 1)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                    use_byte = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for byte-sized assignment.");
            }
            else if (!use_qword && target_size == 2)
            {
                value_reg16 = codegen_register_name16(reg);
                if (value_reg16 != NULL)
                    use_word = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for assignment.");
            }
            if (use_qword)
            {
                int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
                /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
                if (!value_is_qword && assign_expr != NULL)
                {
                    int assign_tag = expr_get_type_tag(assign_expr);
                    if (codegen_type_uses_qword(assign_tag))
                        value_is_qword = 1;
                }
                if (coerced_to_real)
                    value_is_qword = 1;
                /* Check for procedural var calls with pointer return type */
                if (!value_is_qword && assign_expr != NULL &&
                    assign_expr->type == EXPR_FUNCTION_CALL &&
                    assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                    assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
                {
                    KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                    if (call_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                        if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                            value_is_qword = 1;
                        else if (call_type->info.proc_info.return_type_id != NULL)
                        {
                            const char *ret_id = call_type->info.proc_info.return_type_id;
                            if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                                value_is_qword = 1;
                        }
                    }
                }
                if (!value_is_qword)
                {
                    if (assign_expr != NULL && !codegen_expr_is_signed(assign_expr))
                        inst_list = codegen_zero_extend32_to64(inst_list, reg->bit_32, reg->bit_32);
                    else
                        inst_list = codegen_sign_extend32_to64(inst_list, reg->bit_32, reg->bit_64);
                }
            }
            if (var->is_static)
            {
                const char *label = (var->static_label != NULL) ?
                    var->static_label : var->label;
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s(%%rip)\n", reg->bit_64, label);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, %s(%%rip)\n", value_reg8, label);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, %s(%%rip)\n", value_reg16, label);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s(%%rip)\n", reg->bit_32, label);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), reg);
                return inst_list;
            }
            if (var->is_reference)
            {
                Register_t *ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (ptr_reg == NULL)
                {
                    free_reg(get_reg_stack(), reg);
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate register for reference assignment.");
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", var->offset, ptr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", reg->bit_64, ptr_reg->bit_64);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", value_reg8, ptr_reg->bit_64);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", value_reg16, ptr_reg->bit_64);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", reg->bit_32, ptr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), ptr_reg);
                free_reg(get_reg_stack(), reg);
                return inst_list;
            }
            else if (scope_depth == 0)
            {
                /* Variable is in current scope, assign normally */
                if (use_qword)
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                else if (use_byte)
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", value_reg8, var->offset);
                else if (use_word)
                    snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", value_reg16, var->offset);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
            }
            else
            {
                codegen_begin_expression(ctx);
                Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
                if (frame_reg != NULL)
                {
                    if (use_qword)
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%s)\n", reg->bit_64, var->offset, frame_reg->bit_64);
                    else if (use_byte)
                        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%s)\n", value_reg8, var->offset, frame_reg->bit_64);
                    else if (use_word)
                        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%s)\n", value_reg16, var->offset, frame_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%s)\n", reg->bit_32, var->offset, frame_reg->bit_64);
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for assignment to %s.",
                        var_expr->expr_data.id);
                    if (use_qword)
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, var->offset);
                    else if (use_byte)
                        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", value_reg8, var->offset);
                    else if (use_word)
                        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", value_reg16, var->offset);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
                }
                codegen_end_expression(ctx);
            }
        }
        else
        {

            inst_list = codegen_get_nonlocal(inst_list, var_expr->expr_data.id, &offset, ctx);
            int use_qword = codegen_type_uses_qword(var_type);
            /* Override for Single type (4-byte float): check resolved type storage */
            long long resolved_size = (var_expr->resolved_kgpc_type != NULL) ?
                kgpc_type_sizeof(var_expr->resolved_kgpc_type) : 8;
            if (is_single_float_type(var_type, resolved_size))
                use_qword = 0;
            int use_byte = 0;
            const char *value_reg8 = NULL;
            if (!use_qword && var_type == CHAR_TYPE)
            {
                value_reg8 = register_name8(reg);
                if (value_reg8 != NULL)
                    use_byte = 1;
                else
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
            }
            if (use_qword)
            {
                int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
                /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
                if (!value_is_qword && assign_expr != NULL)
                {
                    int assign_tag = expr_get_type_tag(assign_expr);
                    if (codegen_type_uses_qword(assign_tag))
                        value_is_qword = 1;
                }
                if (coerced_to_real)
                    value_is_qword = 1;
                /* Check for procedural var calls with pointer return type */
                if (!value_is_qword && assign_expr != NULL &&
                    assign_expr->type == EXPR_FUNCTION_CALL &&
                    assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                    assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
                {
                    KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                    if (call_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                        if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                            value_is_qword = 1;
                        else if (call_type->info.proc_info.return_type_id != NULL)
                        {
                            const char *ret_id = call_type->info.proc_info.return_type_id;
                            if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                                value_is_qword = 1;
                        }
                    }
                }
                if (!value_is_qword)
                {
                    if (assign_expr != NULL && !codegen_expr_is_signed(assign_expr))
                        inst_list = codegen_zero_extend32_to64(inst_list, reg->bit_32, reg->bit_32);
                    else
                        inst_list = codegen_sign_extend32_to64(inst_list, reg->bit_32, reg->bit_64);
                }
                snprintf(buffer, 50, "\tmovq\t%s, -%d(%s)\n", reg->bit_64, offset, current_non_local_reg64());
            }
            else if (use_byte)
            {
                snprintf(buffer, 50, "\tmovb\t%s, -%d(%s)\n", value_reg8, offset, current_non_local_reg64());
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, -%d(%s)\n", reg->bit_32, offset, current_non_local_reg64());
            }
        }
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), reg);
        return inst_list;
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

        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for array store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        int use_qword = codegen_type_uses_qword(var_type);
        /* Get element size from the base array expression, not the access expression */
        struct Expression *array_expr = var_expr->expr_data.array_access_data.array_expr;
        long long element_size = array_expr != NULL ? expr_get_array_element_size(array_expr, ctx) : -1;
        if (var_expr->array_element_size > element_size)
            element_size = var_expr->array_element_size;
        if (element_size < 2 &&
            var_expr->array_element_type_id != NULL &&
            (pascal_identifier_equals(var_expr->array_element_type_id, "WideChar") ||
             pascal_identifier_equals(var_expr->array_element_type_id, "UnicodeChar")))
        {
            element_size = 2;
        }
        if (element_size <= 0)
            element_size = expr_effective_size_bytes(var_expr);
        if (var_expr->array_element_type_id != NULL &&
            (pascal_identifier_equals(var_expr->array_element_type_id, "WideChar") ||
             pascal_identifier_equals(var_expr->array_element_type_id, "UnicodeChar")))
        {
            element_size = 2;
        }
        int use_word = (!use_qword && element_size == 2);
        
        /* Check if target is a shortstring element (e.g., array[...] of string[10]) */
        int targets_shortstring = codegen_array_access_targets_shortstring(var_expr, ctx);
        int record_targets_shortstring = 0;
        if (var_expr != NULL && var_expr->type == EXPR_RECORD_ACCESS)
        {
            struct RecordField *target_field = codegen_lookup_record_field(var_expr);
            if (target_field != NULL && target_field->type == SHORTSTRING_TYPE)
                record_targets_shortstring = 1;
        }
        
        if (var_type == STRING_TYPE)
        {
            if (codegen_expr_is_shortstring_value_ctx(assign_expr, ctx))
            {
                const char *arg_reg64 = current_arg_reg64(0);
                if (arg_reg64 == NULL)
                {
                    free_reg(get_reg_stack(), addr_reload);
                    free_reg(get_reg_stack(), value_reg);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, arg_reg64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_string");
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_arg_regs();
            }
            /* If assigning a char to a string array element, promote it first */
            else if (expr_get_type_tag(assign_expr) == CHAR_TYPE)
            {
                /* Save addr_reload to a temp since kgpc_char_to_string clobbers rax */
                StackNode_t *addr_save = add_l_t("addr_save");
                char arg_buffer[128];
                snprintf(arg_buffer, sizeof(arg_buffer), "\tmovq\t%s, -%d(%%rbp)\n", 
                    addr_reload->bit_64, addr_save->offset);
                inst_list = add_inst(inst_list, arg_buffer);
                
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
                
                /* Restore addr_reload */
                snprintf(arg_buffer, sizeof(arg_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    addr_save->offset, addr_reload->bit_64);
                inst_list = add_inst(inst_list, arg_buffer);
            }
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
                  targets_shortstring || record_targets_shortstring) &&
                 expr_get_type_tag(assign_expr) == STRING_TYPE)
        {
            /* Handle shortstring assignment - copy string content to shortstring buffer */
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
                array_size = 256;
            inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reload, value_reg, array_size);
        }
        else if ((var_type == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE) ||
                  targets_shortstring || record_targets_shortstring) &&
                 expr_get_type_tag(assign_expr) == CHAR_TYPE)
        {
            const char *value_reg8 = register_name8(value_reg);
            if (value_reg8 == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to select 8-bit register for shortstring char assignment.");
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovb\t$1, (%s)\n", addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, 1(%s)\n",
                    value_reg8, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else if (use_qword)
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
            if (!value_is_qword && assign_expr != NULL)
            {
                int assign_tag = expr_get_type_tag(assign_expr);
                if (codegen_type_uses_qword(assign_tag))
                    value_is_qword = 1;
            }
            if (coerced_to_real)
                value_is_qword = 1;
            /* Check for procedural var calls with pointer return type */
            if (!value_is_qword && assign_expr != NULL &&
                assign_expr->type == EXPR_FUNCTION_CALL &&
                assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                    if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                        value_is_qword = 1;
                    else if (call_type->info.proc_info.return_type_id != NULL)
                    {
                        const char *ret_id = call_type->info.proc_info.return_type_id;
                        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                            value_is_qword = 1;
                    }
                }
            }
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (use_word)
            {
                const char *value_reg16 = codegen_register_name16(value_reg);
                if (value_reg16 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for array assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (var_type == CHAR_TYPE || element_size == 1)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for byte assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

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

        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(assign_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for record store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type_2 = expr_get_type_tag(var_expr);
        if (var_type_2 == UNKNOWN_TYPE && var_expr->type == EXPR_RECORD_ACCESS)
        {
            const char *field_id = var_expr->expr_data.record_access_data.field_id;
            int resolved = lookup_record_field_type(var_expr->record_type, field_id);
            if (resolved != UNKNOWN_TYPE)
            {
                var_type_2 = resolved;
                /* no resolved_type mutation in codegen */
            }
        }
        long long record_element_size = codegen_record_field_effective_size(var_expr, ctx);
        int is_single_real_field = (var_type_2 == REAL_TYPE && record_element_size == 4);
        int assign_type = expr_get_type_tag(assign_expr);
        int assign_is_integer_like = (is_integer_type(assign_type) || assign_type == BOOL ||
            assign_type == CHAR_TYPE || assign_type == ENUM_TYPE);

        int coerced_to_real = 0;
        if (!(is_single_real_field && assign_is_integer_like))
        {
            inst_list = codegen_maybe_convert_int_like_to_real(var_type_2, assign_expr,
                value_reg, inst_list, &coerced_to_real);
        }
        int use_qword = codegen_type_uses_qword(var_type_2);
        if (is_single_real_field)
            use_qword = 0;
        if (!use_qword && record_element_size >= CODEGEN_POINTER_SIZE_BYTES)
            use_qword = 1;
        int use_word = (!use_qword && record_element_size == 2);

        if (is_single_real_field)
        {
            if (assign_is_integer_like)
            {
                snprintf(buffer, sizeof(buffer), "\tcvtsi2ss\t%s, %%xmm0\n", value_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", value_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
            else if (assign_type == REAL_TYPE)
            {
                int source_is_qword_real = expr_uses_qword_kgpctype(assign_expr) || coerced_to_real;
                if (source_is_qword_real)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", value_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcvtsd2ss\t%xmm0, %xmm0\n");
                    snprintf(buffer, sizeof(buffer), "\tmovd\t%%xmm0, %s\n", value_reg->bit_32);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
        }

        /* Check if the target record field is a char array (e.g., Name: array[0..255] of AnsiChar).
         * If so, use proper string-to-char-array copy instead of string pointer assignment. */
        int rec_arr_lower = 0, rec_arr_upper = -1, rec_arr_is_short = 0;
        int rec_field_is_char_array = codegen_get_char_array_bounds(var_expr, ctx,
            &rec_arr_lower, &rec_arr_upper, &rec_arr_is_short);
        if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL && var_expr->type == EXPR_RECORD_ACCESS)
        {
            const char *fid = var_expr->expr_data.record_access_data.field_id;
            fprintf(stderr, "[codegen] RECORD char_array_check: field=%s found=%d lower=%d upper=%d is_short=%d\n",
                fid ? fid : "<null>", rec_field_is_char_array, rec_arr_lower, rec_arr_upper, rec_arr_is_short);
        }
        if (rec_field_is_char_array &&
            assign_expr != NULL &&
            assign_expr->type != EXPR_ARRAY_LITERAL &&
            (expr_get_type_tag(assign_expr) == STRING_TYPE ||
             expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE ||
             codegen_expr_is_shortstring_value_local(assign_expr)))
        {
            int arr_size = rec_arr_upper - rec_arr_lower + 1;
            if (arr_size < 0) arr_size = 0;
            if (rec_arr_is_short)
            {
                int short_capacity = codegen_get_shortstring_capacity(var_expr, ctx);
                if (short_capacity > 1)
                    arr_size = short_capacity;
                if (assign_expr != NULL && assign_expr->type == EXPR_STRING)
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reload, value_reg, arr_size);
                else if (codegen_expr_is_shortstring_value_local(assign_expr))
                {
                    inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reload, arr_size, value_reg);
                }
                else
                    inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reload, value_reg, arr_size);
            }
            else
            {
                if (codegen_expr_is_shortstring_value_local(assign_expr))
                    inst_list = codegen_call_shortstring_to_char_array(inst_list, ctx, addr_reload, value_reg, arr_size);
                else
                    inst_list = codegen_call_string_to_char_array(inst_list, ctx, addr_reload, value_reg, arr_size);
            }
            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reload);
            return inst_list;
        }

        if ((var_type_2 == SHORTSTRING_TYPE || expr_has_type_tag(var_expr, SHORTSTRING_TYPE)) &&
            (expr_get_type_tag(assign_expr) == STRING_TYPE ||
             expr_get_type_tag(assign_expr) == SHORTSTRING_TYPE ||
             codegen_expr_is_shortstring_value_local(assign_expr)))
        {
            int array_size = codegen_get_shortstring_capacity(var_expr, ctx);
            if (array_size <= 1)
                array_size = 256;

            if (assign_expr != NULL && assign_expr->type == EXPR_STRING)
            {
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx,
                    addr_reload, value_reg, array_size);
            }
            else if (codegen_expr_is_shortstring_value_local(assign_expr))
            {
                inst_list = codegen_call_shortstring_copy(inst_list, ctx, addr_reload, array_size, value_reg);
            }
            else
            {
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx,
                    addr_reload, value_reg, array_size);
            }

            free_reg(get_reg_stack(), value_reg);
            free_reg(get_reg_stack(), addr_reload);
            return inst_list;
        }

        if (var_type_2 == STRING_TYPE)
        {
            /* If assigning a char to a string field, promote it first */
            int assign_type_2 = assign_expr != NULL ? expr_get_type_tag(assign_expr) : -1;
            if (assign_type_2 == CHAR_TYPE)
            {
                inst_list = codegen_promote_char_reg_to_string(inst_list, value_reg);
            }
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (use_qword)
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
            if (!value_is_qword && assign_expr != NULL)
            {
                int assign_tag = expr_get_type_tag(assign_expr);
                if (codegen_type_uses_qword(assign_tag))
                    value_is_qword = 1;
            }
            if (coerced_to_real)
                value_is_qword = 1;
            /* Check for procedural var calls with pointer return type */
            if (!value_is_qword && assign_expr != NULL &&
                assign_expr->type == EXPR_FUNCTION_CALL &&
                assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                    if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                        value_is_qword = 1;
                    else if (call_type->info.proc_info.return_type_id != NULL)
                    {
                        const char *ret_id = call_type->info.proc_info.return_type_id;
                        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                            value_is_qword = 1;
                    }
                }
            }
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type_2 == CHAR_TYPE || record_element_size == 1)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else if (use_word || record_element_size == 2)
            {
                const char *value_reg16 = codegen_register_name16(value_reg);
                if (value_reg16 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 16-bit register for record assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

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

        if (codegen_expr_is_shortstring_array(var_expr) &&
            codegen_expr_is_shortstring_rhs(assign_expr, ctx))
        {
            Register_t *dest_addr = NULL;
            Register_t *src_addr = NULL;

            inst_list = codegen_address_for_expr(var_expr, inst_list, ctx, &dest_addr);
            if (codegen_had_error(ctx) || dest_addr == NULL)
            {
                if (dest_addr != NULL)
                    free_reg(get_reg_stack(), dest_addr);
                return inst_list;
            }

            inst_list = codegen_address_for_expr(assign_expr, inst_list, ctx, &src_addr);
            if (codegen_had_error(ctx) || src_addr == NULL)
            {
                free_reg(get_reg_stack(), dest_addr);
                if (src_addr != NULL)
                    free_reg(get_reg_stack(), src_addr);
                return inst_list;
            }

            {
                char arg_buf[128];
                const char *arg_reg0 = current_arg_reg64(0);
                const char *arg_reg1 = current_arg_reg64(1);
                const char *arg_reg2 = current_arg_reg64(2);
                Register_t *len_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (arg_reg0 == NULL || arg_reg1 == NULL || arg_reg2 == NULL || len_reg == NULL)
                {
                    free_reg(get_reg_stack(), dest_addr);
                    free_reg(get_reg_stack(), src_addr);
                    if (len_reg != NULL)
                        free_reg(get_reg_stack(), len_reg);
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate registers for pointer shortstring copy.");
                }

                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", src_addr->bit_64, arg_reg0);
                inst_list = add_inst(inst_list, arg_buf);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_length");
                free_arg_regs();

                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%%rax, %s\n", len_reg->bit_64);
                inst_list = add_inst(inst_list, arg_buf);
                snprintf(arg_buf, sizeof(arg_buf), "\tincq\t%s\n", len_reg->bit_64);
                inst_list = add_inst(inst_list, arg_buf);

                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", dest_addr->bit_64, arg_reg0);
                inst_list = add_inst(inst_list, arg_buf);
                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", src_addr->bit_64, arg_reg1);
                inst_list = add_inst(inst_list, arg_buf);
                snprintf(arg_buf, sizeof(arg_buf), "\tmovq\t%s, %s\n", len_reg->bit_64, arg_reg2);
                inst_list = add_inst(inst_list, arg_buf);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                free_arg_regs();
                free_reg(get_reg_stack(), len_reg);
            }
            free_reg(get_reg_stack(), dest_addr);
            free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }

        expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
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
            value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (value_reg == NULL)
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer value.");

        Register_t *addr_reload = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
            addr_reload = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (addr_reload == NULL)
        {
            free_reg(get_reg_stack(), value_reg);
            return codegen_fail_register(ctx, inst_list, NULL,
                "ERROR: Unable to allocate register for pointer store.");
        }

        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", addr_temp->offset, addr_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);

        int var_type_3 = expr_get_type_tag(var_expr);
        int coerced_to_real = 0;
        inst_list = codegen_maybe_convert_int_like_to_real(var_type_3, assign_expr,
            value_reg, inst_list, &coerced_to_real);
        long long pointer_target_size = expr_effective_size_bytes(var_expr);
        int use_word = (!codegen_type_uses_qword(var_type_3) && pointer_target_size == 2);
        if (var_type_3 == STRING_TYPE)
        {
            inst_list = codegen_call_string_assign(inst_list, ctx, addr_reload, value_reg);
        }
        else if (codegen_type_uses_qword(var_type_3))
        {
            int value_is_qword = expr_uses_qword_kgpctype(assign_expr);
            /* Fallback: check expr type tag for pointer arithmetic / 64-bit ops */
            if (!value_is_qword && assign_expr != NULL)
            {
                int assign_tag = expr_get_type_tag(assign_expr);
                if (codegen_type_uses_qword(assign_tag))
                    value_is_qword = 1;
            }
            if (coerced_to_real)
                value_is_qword = 1;
            /* Check for procedural var calls with pointer return type */
            if (!value_is_qword && assign_expr != NULL &&
                assign_expr->type == EXPR_FUNCTION_CALL &&
                assign_expr->expr_data.function_call_data.is_procedural_var_call &&
                assign_expr->expr_data.function_call_data.call_kgpc_type != NULL)
            {
                KgpcType *call_type = assign_expr->expr_data.function_call_data.call_kgpc_type;
                if (call_type->kind == TYPE_KIND_PROCEDURE)
                {
                    KgpcType *ret_type = kgpc_type_get_return_type(call_type);
                    if (ret_type != NULL && kgpc_type_uses_qword(ret_type))
                        value_is_qword = 1;
                    else if (call_type->info.proc_info.return_type_id != NULL)
                    {
                        const char *ret_id = call_type->info.proc_info.return_type_id;
                        if (kgpc_type_id_uses_qword(ret_id, ctx->symtab))
                            value_is_qword = 1;
                    }
                }
            }
            if (!value_is_qword)
                inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
            snprintf(buffer, 50, "\tmovq\t%s, (%s)\n", value_reg->bit_64, addr_reload->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (var_type_3 == CHAR_TYPE)
            {
                const char *value_reg8 = register_name8(value_reg);
                if (value_reg8 == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to select 8-bit register for character assignment.");
                }
                else
                {
                    snprintf(buffer, 50, "\tmovb\t%s, (%s)\n", value_reg8, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            else
            {
                if (use_word)
                {
                    const char *value_reg16 = codegen_register_name16(value_reg);
                    if (value_reg16 == NULL)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to select 16-bit register for pointer assignment.");
                    }
                    else
                    {
                        snprintf(buffer, 50, "\tmovw\t%s, (%s)\n", value_reg16, addr_reload->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
                else
                {
                    snprintf(buffer, 50, "\tmovl\t%s, (%s)\n", value_reg->bit_32, addr_reload->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
        }

        free_reg(get_reg_stack(), value_reg);
        free_reg(get_reg_stack(), addr_reload);
        return inst_list;
    }
    else
    {
        assert(0 && "Unsupported assignment target");
        return inst_list;
    }
}

static struct Expression *codegen_build_temp_call_expr_from_stmt(
    const struct Statement *stmt, int call_hash_type, struct KgpcType *call_kgpc_type)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return NULL;

    struct Expression *call_expr = mk_functioncall(stmt->line_num,
        stmt->stmt_data.procedure_call_data.id != NULL ?
            strdup(stmt->stmt_data.procedure_call_data.id) : NULL,
        stmt->stmt_data.procedure_call_data.expr_args);
    if (call_expr == NULL)
        return NULL;

    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
    {
        call_expr->expr_data.function_call_data.mangled_id =
            strdup(stmt->stmt_data.procedure_call_data.mangled_id);
    }
    call_expr->expr_data.function_call_data.arg0_is_dynarray_descriptor =
        stmt->stmt_data.procedure_call_data.arg0_is_dynarray_descriptor;
    call_expr->expr_data.function_call_data.call_hash_type = call_hash_type;
    call_expr->expr_data.function_call_data.call_kgpc_type = call_kgpc_type;
    call_expr->expr_data.function_call_data.is_call_info_valid =
        stmt->stmt_data.procedure_call_data.is_call_info_valid;
    call_expr->expr_data.function_call_data.is_virtual_call =
        stmt->stmt_data.procedure_call_data.is_virtual_call;
    call_expr->expr_data.function_call_data.is_interface_call =
        stmt->stmt_data.procedure_call_data.is_interface_call;
    call_expr->expr_data.function_call_data.vmt_index =
        stmt->stmt_data.procedure_call_data.vmt_index;
    if (stmt->stmt_data.procedure_call_data.self_class_name != NULL)
    {
        call_expr->expr_data.function_call_data.self_class_name =
            strdup(stmt->stmt_data.procedure_call_data.self_class_name);
    }
    call_expr->expr_data.function_call_data.is_class_method_call =
        stmt->stmt_data.procedure_call_data.is_class_method_call;
    call_expr->expr_data.function_call_data.is_constructor_call =
        stmt->stmt_data.procedure_call_data.is_constructor_call;
    /* For constructor-as-statement, create a constructor_receiver_expr from the
     * class name so the codegen knows which class to allocate. */
    if (stmt->stmt_data.procedure_call_data.is_constructor_call &&
        stmt->stmt_data.procedure_call_data.constructor_class_name != NULL)
    {
        struct Expression *receiver = (struct Expression *)calloc(1, sizeof(struct Expression));
        if (receiver != NULL)
        {
            receiver->type = EXPR_VAR_ID;
            receiver->expr_data.id = strdup(stmt->stmt_data.procedure_call_data.constructor_class_name);
            call_expr->expr_data.function_call_data.constructor_receiver_expr = receiver;
        }
    }
    return call_expr;
}

static void codegen_destroy_temp_call_expr(struct Expression *call_expr)
{
    if (call_expr == NULL)
        return;

    call_expr->expr_data.function_call_data.args_expr = NULL;
    call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
    call_expr->expr_data.function_call_data.resolved_func = NULL;
    destroy_expr(call_expr);
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
    ListNode_t *call_args;
    /* Procedure calls can reference very long mangled identifiers, so keep plenty of space. */
    char buffer[CODEGEN_MAX_INST_BUF];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;
    call_args = args_expr;
    char *unmangled_name = stmt->stmt_data.procedure_call_data.id;


    /* CRITICAL FIX: Use cached information from AST instead of HashNode pointer.
     * The resolved_proc pointer may point to freed memory if the HashNode was in a scope
     * that has been popped (e.g., nested procedures' parameters/local variables).
     * 
     * During semantic checking, we now cache the essential information (hash_type and KgpcType)
     * directly in the AST, avoiding the need to dereference potentially freed HashNode pointers.
     */
    int call_hash_type;  /* HashType enum value */
    struct KgpcType *call_kgpc_type;
#ifdef DEBUG_CODEGEN
    HashNode_t *debug_proc_node = NULL;
#endif
    
    if (stmt->stmt_data.procedure_call_data.is_call_info_valid)
    {
        /* Use cached information from semantic checking */
        call_hash_type = stmt->stmt_data.procedure_call_data.call_hash_type;
        call_kgpc_type = stmt->stmt_data.procedure_call_data.call_kgpc_type;
        
        CODEGEN_DEBUG("DEBUG codegen_proc_call: id=%s, mangled=%s, hash_type=%d\n",
            unmangled_name ? unmangled_name : "NULL",
            proc_name ? proc_name : "NULL", 
            call_hash_type);
    }
    else
    {
        /* Fallback: look up the symbol (for old code paths or if semantic checker didn't set it) */
        HashNode_t *proc_node = NULL;
        if (unmangled_name != NULL)
            FindSymbol(&proc_node, symtab, unmangled_name);
        /* If unmangled name not found, try the mangled name */
        if (proc_node == NULL && proc_name != NULL && proc_name != unmangled_name)
            FindSymbol(&proc_node, symtab, proc_name);
#ifdef DEBUG_CODEGEN
        debug_proc_node = proc_node;
#endif
        if (proc_node != NULL)
        {
            call_hash_type = proc_node->hash_type;
            call_kgpc_type = proc_node->type;
        }
        else
        {
            /* Symbol not found - emit as a direct call and hope the linker resolves it.
             * This handles runtime procedures and methods that may not be in the symbol table. */
            codegen_report_warning(ctx,
                "WARNING: procedure %s not found during code generation, emitting direct call.",
                unmangled_name ? unmangled_name : "(unknown)");
            /* Emit a direct call to the mangled name */
            const char *call_target = proc_name ? proc_name : unmangled_name;
            if (call_target != NULL)
            {
                char call_buffer[CODEGEN_MAX_INST_BUF];
                inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, NULL,
                    unmangled_name, 0, NULL, 0);
                snprintf(call_buffer, sizeof(call_buffer), "\tcall\t%s\n", call_target);
                inst_list = add_inst(inst_list, call_buffer);
            }
            return inst_list;
        }
    }

    if(call_hash_type == HASHTYPE_FUNCTION)
    {
        struct Expression *call_expr = codegen_build_temp_call_expr_from_stmt(stmt,
            call_hash_type, stmt->stmt_data.procedure_call_data.call_kgpc_type);
        if (call_expr == NULL)
            return inst_list;

        Register_t *discard_reg = NULL;
        inst_list = codegen_evaluate_expr(call_expr, inst_list, ctx, &discard_reg);
        if (discard_reg != NULL)
            free_reg(get_reg_stack(), discard_reg);
        codegen_destroy_temp_call_expr(call_expr);
        return inst_list;
    }

    if(call_hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        return codegen_builtin_proc(stmt, inst_list, ctx);
    }

    {
        struct Expression *call_expr = codegen_build_temp_call_expr_from_stmt(stmt,
            call_hash_type, call_kgpc_type);
        if (call_expr != NULL)
        {
            char *owned_call_target = NULL;
            const char *resolved_call_target = NULL;
            resolved_call_target = codegen_resolve_function_call_target(ctx, call_expr,
                &owned_call_target);
            if (resolved_call_target != NULL && resolved_call_target[0] != '\0')
            {
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                {
                    free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
                }
                stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_call_target);
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
                else
                    proc_name = (char *)resolved_call_target;
            }
            if (owned_call_target != NULL)
                free(owned_call_target);
            codegen_destroy_temp_call_expr(call_expr);
        }
    }

    /* Deterministic external name selection (no defensive fallback):
     * If a procedure definition exists and declares an explicit external alias (cname_override),
     * prefer it as the call target unconditionally. Otherwise, rely on the call-site mangled name
     * populated by semantic checking. If neither is available, panic with a descriptive error. */
    if (stmt->stmt_data.procedure_call_data.is_call_info_valid &&
        call_kgpc_type != NULL && call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
        call_kgpc_type->info.proc_info.definition != NULL)
    {
        Tree_t *def = call_kgpc_type->info.proc_info.definition;
        const char *alias = def->tree_data.subprogram_data.cname_override;
        if (alias != NULL && alias[0] != '\0')
        {
            /* Overwrite any prior name with explicit external alias */
            if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
            {
                free(stmt->stmt_data.procedure_call_data.mangled_id);
                stmt->stmt_data.procedure_call_data.mangled_id = NULL;
            }
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(alias);
            proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
        }
        else if (def->tree_data.subprogram_data.owner_class != NULL &&
                 def->tree_data.subprogram_data.method_name != NULL)
        {
            const char *impl_target = codegen_find_class_method_impl_id(
                symtab, NULL, def->tree_data.subprogram_data.owner_class, NULL,
                def->tree_data.subprogram_data.method_name);
            if (impl_target != NULL &&
                (proc_name == NULL || proc_name[0] == '\0' ||
                 strcmp(proc_name, def->tree_data.subprogram_data.method_name) == 0 ||
                 (unmangled_name != NULL && strcmp(proc_name, unmangled_name) == 0)))
            {
                if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                {
                    free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = NULL;
                }
                stmt->stmt_data.procedure_call_data.mangled_id = strdup(impl_target);
                proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
            }
        }
    }

    if (proc_name != NULL && pascal_identifier_equals(proc_name, "fpc_in_prefetch_var"))
        proc_name = "kgpc_prefetch";
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
// removed assert on proc_name
    /* Check if this is an indirect call through a procedure variable */
    /* For indirect calls (procedure variables/parameters), we need to generate an indirect call.
     * 
     * IMPORTANT: We must check hash_type FIRST, before checking type information.
     * On some platforms (e.g., Cygwin), type information may not be properly set,
     * but hash_type is always reliable for distinguishing variables from procedures.
     */
    int is_indirect_call = stmt->stmt_data.procedure_call_data.is_procedural_var_call;
    struct Expression *callee_override = NULL;
    
    /* Case 1: If hash_type is VAR, this is a procedure variable or parameter.
     * It MUST be an indirect call, regardless of whether type info is present. */
    if (call_hash_type == HASHTYPE_VAR)
    {
        is_indirect_call = 1;
    }
    /* Case 1b: If hash_type is CONST with procedure type, this is a typed constant 
     * holding a procedure address. Treat as indirect call. */
    else if (call_hash_type == HASHTYPE_CONST &&
             call_kgpc_type != NULL && 
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
    {
        is_indirect_call = 1;
    }
    /* Case 2: If hash_type is PROCEDURE but type info indicates it's a procedure type,
     * and either proc_name is NULL or doesn't match, treat as indirect call.
     * This handles corrupted or improperly resolved procedure nodes. */
    else if (call_hash_type == HASHTYPE_PROCEDURE &&
             call_kgpc_type != NULL && 
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
             proc_name == NULL)
    {
        is_indirect_call = 1;
    }
    /* Case 3: If the call target is a procedural TYPE (e.g. FileFunc(...)(...)),
     * treat it as an indirect call through the first argument. */
    else if (call_hash_type == HASHTYPE_TYPE &&
             call_kgpc_type != NULL &&
             call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
    {
        is_indirect_call = 1;
        if (!stmt->stmt_data.procedure_call_data.is_procedural_var_call &&
            stmt->stmt_data.procedure_call_data.procedural_var_expr == NULL &&
            args_expr != NULL)
        {
            callee_override = (struct Expression *)args_expr->cur;
            call_args = args_expr->next;
        }
    }
    
    if (is_indirect_call)
    {
        /* INDIRECT CALL LOGIC */
        
        /* Validate that we have a procedure name for the indirect call */
        if (unmangled_name == NULL)
        {
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - indirect call with NULL procedure name. "
                "Please report this bug with your source code.");
            return inst_list;
        }

        /* 1. Create a temporary expression to evaluate the procedure variable */
        struct Expression *callee_expr = NULL;
        int callee_owned = 0;
        if (callee_override != NULL)
        {
            callee_expr = callee_override;
        }
        else if (stmt->stmt_data.procedure_call_data.is_procedural_var_call &&
            stmt->stmt_data.procedure_call_data.procedural_var_expr != NULL)
        {
            callee_expr = stmt->stmt_data.procedure_call_data.procedural_var_expr;
            if (callee_expr->resolved_kgpc_type == NULL)
                callee_expr->resolved_kgpc_type = create_procedure_type(NULL, NULL);
        }
        else
        {
            callee_expr = mk_varid(stmt->line_num, strdup(unmangled_name));
            callee_expr->resolved_kgpc_type = create_procedure_type(NULL, NULL);
            callee_owned = 1;
        }
        
        /* 2. Generate code to load the procedure's address into a register */
        Register_t *addr_reg = NULL;
        int load_from_memory = 0;
        int callee_is_bound_finish_module = 0;
        StackNode_t *bound_self_spill = NULL;

        if (callee_expr->type == EXPR_RECORD_ACCESS ||
            callee_expr->type == EXPR_ARRAY_ACCESS ||
            callee_expr->type == EXPR_POINTER_DEREF)
        {
            load_from_memory = 1;
            if (callee_expr->type == EXPR_RECORD_ACCESS &&
                callee_expr->expr_data.record_access_data.field_id != NULL &&
                pascal_identifier_equals(callee_expr->expr_data.record_access_data.field_id, "finish_module"))
                callee_is_bound_finish_module = 1;
        }
        else if (callee_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
        {
            HashNode_t *callee_node = NULL;
            if (FindSymbol(&callee_node, ctx->symtab, callee_expr->expr_data.id) != 0 &&
                callee_node != NULL &&
                (callee_node->hash_type == HASHTYPE_VAR ||
                 callee_node->hash_type == HASHTYPE_FUNCTION_RETURN))
            {
                load_from_memory = 1;
            }
        }

        if (load_from_memory)
        {
            inst_list = codegen_address_for_expr(callee_expr, inst_list, ctx, &addr_reg);
            if (!codegen_had_error(ctx) && addr_reg != NULL)
            {
                if (callee_is_bound_finish_module)
                {
                    Register_t *self_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (self_reg == NULL)
                        self_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    bound_self_spill = add_l_t_bytes("bound_method_self", 8);
                    if (self_reg != NULL && bound_self_spill != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t8(%s), %s\n",
                            addr_reg->bit_64, self_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            self_reg->bit_64, bound_self_spill->offset);
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), self_reg);
                    }
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                    addr_reg->bit_64, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            inst_list = codegen_evaluate_expr(callee_expr, inst_list, ctx, &addr_reg);
        }
        if (callee_owned)
            destroy_expr(callee_expr);
        
        if (codegen_had_error(ctx) || addr_reg == NULL)
        {
            return inst_list;
        }

        /* 3. Spill call target to stack so argument evaluation can't clobber it */
        StackNode_t *call_target_spill = add_l_t_bytes("indirect_call_target", 8);
        if (call_target_spill == NULL)
        {
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            addr_reg->bit_64, call_target_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        addr_reg = NULL;
        
        /* 4. Pass arguments as usual */
        const char *proc_name_hint = (unmangled_name != NULL) ? unmangled_name : proc_name;
        inst_list = codegen_pass_arguments(call_args, inst_list, ctx, call_kgpc_type,
            proc_name_hint, callee_is_bound_finish_module ? 1 : 0, NULL, 0);

        if (callee_is_bound_finish_module && bound_self_spill != NULL)
        {
            const char *self_arg = current_arg_reg64(0);
            if (self_arg != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    bound_self_spill->offset, self_arg);
                inst_list = add_inst(inst_list, buffer);
            }
        }

        /* 5. Zero out %eax for varargs ABI compatibility */
        inst_list = codegen_vect_reg(inst_list, 0);
        
        /* 6. Reload call target and perform the indirect call */
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n",
            call_target_spill->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
        inst_list = add_inst(inst_list, buffer);
        
        /* 7. Cleanup */
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (indirect call)\n", __func__);
        #endif
        return inst_list;
    }
    else if (call_hash_type == HASHTYPE_PROCEDURE)
    {
        /* DIRECT CALL LOGIC */
        int requires_static_link = 0;
        if (call_kgpc_type != NULL && call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
            call_kgpc_type->info.proc_info.definition != NULL)
        {
            requires_static_link = call_kgpc_type->info.proc_info.definition
                ->tree_data.subprogram_data.requires_static_link;
        }

        int callee_depth = 0;
        int have_depth = codegen_proc_static_link_depth(ctx, proc_name, &callee_depth);
        int current_depth = codegen_get_lexical_depth(ctx);
        int should_pass_static_link = requires_static_link;

        enum {
            STATIC_LINK_NONE = 0,
            STATIC_LINK_FROM_RBP,
            STATIC_LINK_FROM_SLOT,
            STATIC_LINK_FROM_REG,
            STATIC_LINK_FROM_SPILL
        } static_link_source = STATIC_LINK_NONE;
        int static_link_slot_offset = 0;
        Register_t *static_link_reg = NULL;
        int static_link_expr_active = 0;
        StackNode_t *static_link_spill = NULL;

        if (should_pass_static_link)
        {
            if (!have_depth)
            {
                static_link_source = STATIC_LINK_FROM_RBP;
            }
            else if (callee_depth > current_depth)
            {
                static_link_source = STATIC_LINK_FROM_RBP;
            }
            else if (callee_depth == current_depth)
            {
                StackNode_t *static_link_node = find_label("__static_link__");
                if (static_link_node != NULL)
                {
                    static_link_source = STATIC_LINK_FROM_SLOT;
                    static_link_slot_offset = static_link_node->offset;
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Missing static link slot while calling %s.",
                        unmangled_name);
                }
            }
            else
            {
                /* We need to traverse multiple scopes to get the static link.
                 * We must spill the result to a stack slot BEFORE evaluating arguments,
                 * because argument evaluation may call codegen_acquire_static_link again
                 * for different scope depths, which would free and reuse the register. */
                int levels_to_traverse = (current_depth - callee_depth) + 1;
                codegen_begin_expression(ctx);
                static_link_expr_active = 1;
                static_link_reg = codegen_acquire_static_link(ctx, &inst_list, levels_to_traverse);
                if (static_link_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to acquire static link for call to %s.",
                        unmangled_name);
                }
                else
                {
                    /* Spill the static link to a temporary stack slot to preserve it
                     * during argument evaluation */
                    static_link_spill = add_l_t("static_link_spill");
                    char spill_buffer[64];
                    snprintf(spill_buffer, sizeof(spill_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        static_link_reg->bit_64, static_link_spill->offset);
                    inst_list = add_inst(inst_list, spill_buffer);
                    free_reg(get_reg_stack(), static_link_reg);
                    static_link_reg = NULL;
                    static_link_source = STATIC_LINK_FROM_SPILL;
                }
                /* End the expression context to allow argument evaluation to use
                 * codegen_acquire_static_link independently */
                codegen_end_expression(ctx);
                static_link_expr_active = 0;
            }
        }
        
        /* When passing static link, shift arguments by 1 register position */
        int arg_start_index = should_pass_static_link ? 1 : 0;
        const char *proc_name_hint = (unmangled_name != NULL) ? unmangled_name : proc_name;
        inst_list = codegen_pass_arguments(args_expr, inst_list, ctx, call_kgpc_type,
            proc_name_hint, arg_start_index, NULL,
            stmt->stmt_data.procedure_call_data.is_class_method_call);

        if (should_pass_static_link)
        {
            const char *link_dest_reg = current_arg_reg64(0);
            assert(link_dest_reg != NULL && "current_arg_reg64(0) should never return NULL");

            char link_buffer[64];
            switch (static_link_source)
            {
                case STATIC_LINK_FROM_RBP:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%%rbp, %s\n", link_dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SLOT:
                    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        static_link_slot_offset, link_dest_reg);
                    inst_list = add_inst(inst_list, link_buffer);
                    break;
                case STATIC_LINK_FROM_SPILL:
                    /* Restore from spill slot where we saved it before argument evaluation */
                    if (static_link_spill != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t-%d(%%rbp), %s\n",
                            static_link_spill->offset, link_dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                    }
                    break;
                case STATIC_LINK_FROM_REG:
                    if (static_link_reg != NULL)
                    {
                        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, %s\n",
                            static_link_reg->bit_64, link_dest_reg);
                        inst_list = add_inst(inst_list, link_buffer);
                        free_reg(get_reg_stack(), static_link_reg);
                        static_link_reg = NULL;
                    }
                    break;
                default:
                    break;
            }
        }

        if (static_link_expr_active)
        {
            codegen_end_expression(ctx);
        }

        inst_list = codegen_vect_reg(inst_list, 0);
        CODEGEN_DEBUG("DEBUG PROC_CALL: proc_name=%s\n", proc_name ? proc_name : "NULL");
        if (stmt->stmt_data.procedure_call_data.is_interface_call &&
            stmt->stmt_data.procedure_call_data.vmt_index >= 0)
        {
            int self_arg_index = should_pass_static_link ? 1 : 0;
            inst_list = codegen_emit_interface_dispatch(
                inst_list, ctx, current_arg_reg64(self_arg_index),
                stmt->stmt_data.procedure_call_data.self_class_name,
                stmt->stmt_data.procedure_call_data.vmt_index,
                "intf_stmt", "__intf_stmt_target", 0,
                codegen_spill_call_arg_regs_stmt,
                codegen_restore_call_arg_regs_stmt);
        }
        else if (stmt->stmt_data.procedure_call_data.is_virtual_call &&
            stmt->stmt_data.procedure_call_data.vmt_index >= 0)
        {
            /* Virtual method call through VMT for procedure (void return type).
             * Self is the first argument register after the optional static link slot. */
            int vmt_index = stmt->stmt_data.procedure_call_data.vmt_index;
            int self_arg_index = should_pass_static_link ? 1 : 0;
            const char *self_reg = current_arg_reg64(self_arg_index);
            int self_is_vmt =
                (!stmt->stmt_data.procedure_call_data.is_constructor_call) &&
                codegen_stmt_first_arg_is_class_vmt_value(args_expr, ctx);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
            inst_list = add_inst(inst_list, buffer);
            if (!self_is_vmt)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t(%%r11), %%r11\n");
                inst_list = add_inst(inst_list, buffer);
            }
            int vmt_offset = vmt_index * 8;
            snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n", vmt_offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tcall\t*%%r11\n");
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", proc_name);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_cleanup_call_stack(inst_list, ctx);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
        #endif
        return inst_list;
    }
    
    if (call_hash_type == HASHTYPE_FUNCTION)
    {
        struct Expression *call_expr = codegen_build_temp_call_expr_from_stmt(stmt,
            call_hash_type, stmt->stmt_data.procedure_call_data.call_kgpc_type);
        if (call_expr == NULL)
            return inst_list;

        Register_t *discard_reg = NULL;
        inst_list = codegen_evaluate_expr(call_expr, inst_list, ctx, &discard_reg);
        if (discard_reg != NULL)
            free_reg(get_reg_stack(), discard_reg);
        codegen_destroy_temp_call_expr(call_expr);
        return inst_list;
    }

    /* If we reach here, it's a compiler bug - this should never happen.
     * In semantic checking, we should have caught any invalid procedure calls.
     * If we get here, it means:
     * 1. proc_node is NULL (semantic checker failed to resolve the symbol), OR
     * 2. proc_node has an unexpected hash_type (not VAR, PROCEDURE, or BUILTIN)
     * 
     * This indicates a serious internal error, so we assert in debug builds
     * and report a fatal error in release builds. */
    
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("FATAL: Reached unreachable code in %s - this is a compiler bug!\n", __func__);
    if (debug_proc_node != NULL) {
        CODEGEN_DEBUG("  proc_name: %s\n", proc_name ? proc_name : "(null)");
        CODEGEN_DEBUG("  unmangled_name: %s\n", unmangled_name ? unmangled_name : "(null)");
        CODEGEN_DEBUG("  hash_type: %d (expected VAR=%d, PROCEDURE=%d, or BUILTIN=%d)\n", 
                     debug_proc_node->hash_type, HASHTYPE_VAR, HASHTYPE_PROCEDURE, HASHTYPE_BUILTIN_PROCEDURE);
        CODEGEN_DEBUG("  type: %p\n", (void*)debug_proc_node->type);
        if (debug_proc_node->type != NULL) {
            CODEGEN_DEBUG("  type->kind: %d\n", debug_proc_node->type->kind);
        }
    } else {
        CODEGEN_DEBUG("  proc_node is NULL - semantic checker should have caught this!\n");
    }
    #endif
    
    KGPC_COMPILER_HARD_ASSERT(0,
        "unreachable procedure-call state for '%s' (hash_type=%d)",
        unmangled_name ? unmangled_name : "(unknown)",
        call_hash_type);

    /* In release builds, report a fatal error and stop code generation */
    codegen_report_error(ctx,
        "FATAL: Internal compiler error in procedure call code generation for '%s'. "
        "Please report this bug with your source code.",
        unmangled_name ? unmangled_name : "(unknown)");
    
    return inst_list;
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

    int relop_type = NE, inverse;
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

    int relop_type = NE;
    struct Expression *expr;
    struct Statement *while_stmt;
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);
    while_stmt = stmt->stmt_data.while_data.while_stmt;
    expr = stmt->stmt_data.while_data.relop_expr;

    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);
    if (!codegen_push_loop(ctx, exit_label, cond_label))
        return inst_list;
    inst_list = codegen_stmt(while_stmt, inst_list, ctx, symtab);
    codegen_pop_loop(ctx);
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
    int relop_type = NE;
    ListNode_t *body_list = stmt->stmt_data.repeat_data.body_list;

    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    snprintf(buffer, 50, "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    if (!codegen_push_loop(ctx, exit_label, body_label))
        return inst_list;
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        inst_list = codegen_stmt(body_stmt, inst_list, ctx, symtab);
        body_list = body_list->next;
    }
    codegen_pop_loop(ctx);

    inst_list = codegen_condition_expr(stmt->stmt_data.repeat_data.until_expr, inst_list, ctx, &relop_type);
    inst_list = gencode_jmp(relop_type, 1, body_label, inst_list);

    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for for-in statements - lower to regular for loop */
ListNode_t *codegen_for_in(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR_IN);
    assert(ctx != NULL);
    assert(symtab != NULL);

    // Extract for-in components
    struct Expression *loop_var = stmt->stmt_data.for_in_data.loop_var;
    struct Expression *collection = stmt->stmt_data.for_in_data.collection;
    struct Statement *body = stmt->stmt_data.for_in_data.do_stmt;

    if (loop_var == NULL || collection == NULL || body == NULL) {
        codegen_report_error(ctx, "ERROR: FOR-IN statement has NULL components");
        return inst_list;
    }

    // Get collection type info from semantic check
    KgpcType *array_type = collection->resolved_kgpc_type;
    int is_enum_domain = 0;
    int is_set_collection = (array_type != NULL && kgpc_type_is_set(array_type));
    long long enum_domain_lower = 0;
    int enum_domain_upper = -1;

    if (collection->type == EXPR_VAR_ID && collection->expr_data.id != NULL)
    {
        HashNode_t *collection_node = NULL;
        if (FindSymbol(&collection_node, symtab, collection->expr_data.id) != 0 &&
            collection_node != NULL &&
            collection_node->hash_type == HASHTYPE_TYPE &&
            collection_node->type != NULL)
        {
            int enum_count = codegen_enum_type_literal_count(collection_node->type);
            if (enum_count > 0)
            {
                is_enum_domain = 1;
                enum_domain_lower = 0;
                enum_domain_upper = enum_count - 1;
                array_type = collection_node->type;
            }
            else if (collection_node->type->type_alias != NULL &&
                collection_node->type->type_alias->is_range &&
                collection_node->type->type_alias->range_known)
            {
                long long lower = collection_node->type->type_alias->range_start;
                long long upper = collection_node->type->type_alias->range_end;
                if (upper >= lower && lower >= INT_MIN && upper <= INT_MAX)
                {
                    is_enum_domain = 1;
                    enum_domain_lower = lower;
                    enum_domain_upper = (int)upper;
                    array_type = collection_node->type;
                }
            }
        }
    }
    
    // Check if this is a list-like class with default indexed property
    int is_list_class = 0;
    struct RecordType *record_info = NULL;
    KgpcType *record_candidate = array_type;
    if (array_type != NULL && array_type->kind == TYPE_KIND_POINTER)
        record_candidate = array_type->info.points_to;

    if (record_candidate != NULL && record_candidate->kind == TYPE_KIND_RECORD) {
        record_info = kgpc_type_get_record(record_candidate);
        if (record_info != NULL) {
            /* Check for TFPGList$ pattern (generic list) - element type is encoded in class name.
             * This is kept separate from default_indexed_property for backwards compatibility. */
            if (record_info->type_id != NULL) {
                const char *prefix = "TFPGList$";
                size_t prefix_len = strlen(prefix);
                if (strncasecmp(record_info->type_id, prefix, prefix_len) == 0) {
                    is_list_class = 1;
                }
            }
            /* Check for default indexed property (handles TStringList and other classes with FItems) */
            if (!is_list_class && record_info->default_indexed_property != NULL) {
                is_list_class = 1;
            }
        }
    }

    int is_enumerator_class = 0;
    HashNode_t *getenum_node = NULL;
    HashNode_t *movenext_node = NULL;
    HashNode_t *current_node = NULL;
    KgpcType *current_type = NULL;
    struct RecordType *enum_record = NULL;
    if (!is_list_class && record_info != NULL)
    {
        getenum_node = codegen_find_zero_arg_method_node(symtab, record_info, "GetEnumerator");
        if (getenum_node != NULL && getenum_node->type != NULL)
        {
            KgpcType *enum_ret = kgpc_type_get_return_type(getenum_node->type);
            KgpcType *enum_candidate = enum_ret;
            if (enum_candidate != NULL && kgpc_type_is_pointer(enum_candidate))
                enum_candidate = enum_candidate->info.points_to;
            if (enum_candidate != NULL && kgpc_type_is_record(enum_candidate))
            {
                enum_record = kgpc_type_get_record(enum_candidate);
                if (enum_record != NULL)
                {
                    movenext_node = codegen_find_zero_arg_method_node(symtab, enum_record, "MoveNext");
                    if (movenext_node != NULL && movenext_node->type != NULL)
                    {
                        KgpcType *move_ret = kgpc_type_get_return_type(movenext_node->type);
                        if (move_ret != NULL && kgpc_type_is_boolean(move_ret) &&
                            codegen_get_enumerator_current_info(symtab, enum_record,
                                &current_node, &current_type))
                        {
                            is_enumerator_class = 1;
                        }
                    }
                }
            }
        }
    }

    if (is_enumerator_class) {
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        StackNode_t *enum_slot = codegen_alloc_temp_slot("enum_ptr");
        if (enum_slot == NULL) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for enumerator");
            return inst_list;
        }

        Register_t *collection_reg = NULL;
        inst_list = codegen_expr_with_result(collection, inst_list, ctx, &collection_reg);
        if (codegen_had_error(ctx) || collection_reg == NULL) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            return inst_list;
        }

        const char *arg0 = current_arg_reg64(0);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", collection_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), collection_reg);
        inst_list = codegen_call_with_shadow_space(inst_list,
            getenum_node->mangled_id != NULL ? getenum_node->mangled_id : getenum_node->id);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", enum_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            return inst_list;
        }

        Register_t *enum_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (enum_reg == NULL) {
            codegen_pop_loop(ctx);
            if (current_type != NULL) destroy_kgpc_type(current_type);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for enumerator");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", enum_slot->offset, enum_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", enum_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_call_with_shadow_space(inst_list,
            current_node->mangled_id != NULL ? current_node->mangled_id : current_node->id);

        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), enum_reg);
            codegen_pop_loop(ctx);
            if (current_type != NULL) destroy_kgpc_type(current_type);
            return inst_list;
        }

        long long current_size = current_type != NULL ? kgpc_type_sizeof(current_type) : 0;
        if (current_size <= 0 && current_type != NULL && kgpc_type_is_pointer(current_type))
            current_size = 8;
        if (current_size == 1) {
            snprintf(buffer, sizeof(buffer), "\tmovb\t%%al, (%s)\n", loop_var_addr_reg->bit_64);
        } else if (current_size == 2) {
            snprintf(buffer, sizeof(buffer), "\tmovw\t%%ax, (%s)\n", loop_var_addr_reg->bit_64);
        } else if (current_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, (%s)\n", loop_var_addr_reg->bit_64);
        } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, (%s)\n", loop_var_addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), enum_reg);

        inst_list = codegen_stmt(body, inst_list, ctx, symtab);
        codegen_pop_loop(ctx);

        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);
        Register_t *enum_cond_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (enum_cond_reg == NULL) {
            if (current_type != NULL) destroy_kgpc_type(current_type);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for enumerator condition");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", enum_slot->offset, enum_cond_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", enum_cond_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_call_with_shadow_space(inst_list,
            movenext_node->mangled_id != NULL ? movenext_node->mangled_id : movenext_node->id);
        inst_list = add_inst(inst_list, "\ttestl\t%eax, %eax\n");
        snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), enum_cond_reg);

        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);
        if (current_type != NULL)
            destroy_kgpc_type(current_type);
        return inst_list;
    }
    
    if (is_list_class) {
        // Generate FOR-IN loop for TFPGList by accessing FItems and FCount directly
        // Structure: for Item in L do body
        // Becomes: for i := 0 to L.FCount-1 do Item := L.FItems[i]; body
        
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        // Allocate stack slot for loop index
        StackNode_t *index_slot = codegen_alloc_temp_slot("fpg_idx");
        // Allocate stack slot for object pointer to survive loop calls
        StackNode_t *obj_ptr_slot = codegen_alloc_temp_slot("fpg_obj");
        // Allocate stack slot for loop upper bound (FCount) to survive loop body codegen
        StackNode_t *count_slot = codegen_alloc_temp_slot("fpg_count");
        
        if (index_slot == NULL || obj_ptr_slot == NULL || count_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for FOR-IN variables");
            return inst_list;
        }

        // Get address of collection object (L)
        Register_t *obj_addr_reg = NULL;
        inst_list = codegen_address_for_expr(collection, inst_list, ctx, &obj_addr_reg);
        if (obj_addr_reg == NULL || codegen_had_error(ctx)) {
            return inst_list;
        }
        
        // Dereference L to get the actual object pointer and store in stack slot
        Register_t *temp_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (temp_reg == NULL) {
            free_reg(get_reg_stack(), obj_addr_reg);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for object pointer load");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", obj_addr_reg->bit_64, temp_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        
        // Store object pointer to stack
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", temp_reg->bit_64, obj_ptr_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        free_reg(get_reg_stack(), obj_addr_reg);

        long long fitems_offset = 8;
        long long fcount_offset = 24;
        long long fcount_size = 8;
        struct RecordField *fitems_field = NULL;
        struct RecordField *fcount_field = NULL;

        int using_flist = 0;  /* Track if we're using TStringList's FList field */
        if (record_info != NULL) {
            if (resolve_record_field(symtab, record_info, "FItems", &fitems_field,
                    &fitems_offset, stmt->line_num, 1) != 0) {
                /* FItems not found — try FList (used by TStringList and TStrings) */
                if (resolve_record_field(symtab, record_info, "FList", &fitems_field,
                        &fitems_offset, stmt->line_num, 1) != 0) {
                    fitems_offset = 8;
                    fitems_field = NULL;
                } else {
                    using_flist = 1;
                }
            }
            if (resolve_record_field(symtab, record_info, "FCount", &fcount_field,
                    &fcount_offset, stmt->line_num, 1) != 0) {
                fcount_offset = 24;
                fcount_field = NULL;
            } else if (fcount_field != NULL) {
                long long size_out = 0;
                if (sizeof_from_type_ref(symtab, fcount_field->type, fcount_field->type_id,
                        &size_out, 0, stmt->line_num) == 0 && size_out > 0) {
                    fcount_size = size_out;
                }
            }
        }

        // Load FCount and store to stack slot (to prevent register clobbering in loop body)
        if (fcount_size <= 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%lld(%s), %s\n",
                fcount_offset, temp_reg->bit_64, temp_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                temp_reg->bit_64, count_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%lld(%s), %s\n",
                fcount_offset, temp_reg->bit_64, temp_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                temp_reg->bit_64, count_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        free_reg(get_reg_stack(), temp_reg);

        // Initialize index to 0
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to condition check
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        // Body label
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Push loop exit label for break statements
        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            return inst_list;
        }

        // Load loop variable: Item := L.FItems[index]
        // FItems is a dynamic array pointer at offset 8
        Register_t *fitems_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (fitems_reg == NULL) {
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for FItems");
            return inst_list;
        }
        
        // Load object pointer from stack to temp register (reusing fitems_reg temporarily to save registers? No, safer to use another temp)
        Register_t *obj_reload_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (obj_reload_reg == NULL) {
             free_reg(get_reg_stack(), fitems_reg);
             codegen_pop_loop(ctx);
             codegen_report_error(ctx, "ERROR: Unable to allocate register for object reload");
             return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", obj_ptr_slot->offset, obj_reload_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Load FItems.data pointer from object using two-step approach
        // Step 1: Get address of FItems descriptor (at Self+fitems_offset)
        // Step 2: Load data pointer from descriptor[0]
        Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (desc_addr_reg == NULL) {
            free_reg(get_reg_stack(), obj_reload_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for descriptor address");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tleaq\t%lld(%s), %s\n", fitems_offset,
            obj_reload_reg->bit_64, desc_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", desc_addr_reg->bit_64, fitems_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), desc_addr_reg);
        free_reg(get_reg_stack(), obj_reload_reg);

        // Load index into register
        Register_t *idx_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (idx_reg == NULL) {
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for index");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Calculate element offset: index * element_size
        int element_size = 0;
        const char *item_field_name = using_flist ? "FList" : "FItems";

        // Try to determine element size from the item array field
        if (fitems_field != NULL) {
            /* If the resolved field has array element type info, use it */
            if (fitems_field->array_element_type_id != NULL && ctx->symtab != NULL) {
                HashNode_t *tnode = NULL;
                if (FindSymbol(&tnode, ctx->symtab, fitems_field->array_element_type_id) != 0 &&
                    tnode != NULL && tnode->type != NULL) {
                    element_size = (int)kgpc_type_sizeof(tnode->type);
                }
            }
            if (element_size <= 0) {
                switch (fitems_field->array_element_type) {
                    case CHAR_TYPE: case BOOL: element_size = 1; break;
                    case INT_TYPE: case ENUM_TYPE: element_size = 4; break;
                    case LONGINT_TYPE: element_size = 4; break;
                    case POINTER_TYPE: case REAL_TYPE: element_size = 8; break;
                }
            }
        } else if (record_info != NULL) {
            /* Fallback: scan fields by name */
            ListNode_t *fields = record_info->fields;
            while (fields != NULL) {
                if (fields->type == LIST_RECORD_FIELD) {
                    struct RecordField *f = (struct RecordField *)fields->cur;
                    if (f != NULL && f->name != NULL &&
                        pascal_identifier_equals(f->name, item_field_name)) {
                        if (f->array_element_type_id != NULL && ctx->symtab != NULL) {
                            HashNode_t *tnode = NULL;
                            if (FindSymbol(&tnode, ctx->symtab, f->array_element_type_id) != 0 &&
                                tnode != NULL && tnode->type != NULL) {
                                element_size = (int)kgpc_type_sizeof(tnode->type);
                            }
                        }
                        if (element_size <= 0) {
                            switch (f->array_element_type) {
                                case CHAR_TYPE: case BOOL: element_size = 1; break;
                                case INT_TYPE: case ENUM_TYPE: element_size = 4; break;
                                case LONGINT_TYPE: element_size = 4; break;
                                case POINTER_TYPE: case REAL_TYPE: element_size = 8; break;
                            }
                        }
                        break;
                    }
                }
                fields = fields->next;
            }
        }

        /* When using FList (TStringList), try to resolve the element type from
         * the field's type chain: PStringItemList → ^TStringItemList →
         * array of TStringItem → sizeof(TStringItem) = 16. */
        if (element_size <= 0 && using_flist && fitems_field != NULL &&
            fitems_field->type_id != NULL && ctx->symtab != NULL) {
            HashNode_t *ftype_node = NULL;
            if (FindSymbol(&ftype_node, ctx->symtab, fitems_field->type_id) != 0 &&
                ftype_node != NULL && ftype_node->type != NULL) {
                KgpcType *ft = ftype_node->type;
                /* Dereference pointer: PStringItemList → TStringItemList */
                if (ft->kind == TYPE_KIND_POINTER && ft->info.points_to != NULL)
                    ft = ft->info.points_to;
                /* Array element: TStringItemList → TStringItem */
                if (ft->kind == TYPE_KIND_ARRAY &&
                    ft->info.array_info.element_type != NULL)
                    element_size = (int)kgpc_type_sizeof(ft->info.array_info.element_type);
                else if (ft->kind == TYPE_KIND_RECORD)
                    element_size = (int)kgpc_type_sizeof(ft);
            }
        }

        // Fallback to loop variable size if element size couldn't be determined
        if (element_size <= 0 && loop_var != NULL && loop_var->resolved_kgpc_type != NULL) {
             element_size = (int)kgpc_type_sizeof(loop_var->resolved_kgpc_type);
        }

        if (element_size <= 0) element_size = 4;
        
        if (element_size != 1) {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%d, %s\n", element_size, idx_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        // Load element: FItems[index]
        Register_t *elem_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (elem_reg == NULL) {
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for element");
            return inst_list;
        }
        if (element_size == 1) {
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 2) {
            snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_32);
        } else if (element_size == 8) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_64);
        } else if (using_flist && element_size > 8) {
            /* TStringList's FList stores TStringItem records (FString + FObject).
             * The stride is element_size (16) but we only need the first 8 bytes
             * (the FString pointer) from each entry. */
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s,%s), %s\n", fitems_reg->bit_64, idx_reg->bit_64, elem_reg->bit_64);
        } else {
            free_reg(get_reg_stack(), elem_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_report_error(ctx, "ERROR: TFPGList for-in only supports 1, 2, 4, 8 byte elements");
            return inst_list;
        }
        inst_list = add_inst(inst_list, buffer);

        // Assign element value to loop variable
        // Get address of loop variable
        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), elem_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), fitems_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        // Store element to loop variable
        if (element_size == 1) {
            const char *byte_reg = register_name8(elem_reg);
            if (byte_reg == NULL) {
                codegen_report_error(ctx, "codegen_for_in: unable to get 8-bit register name for element storage");
                free_reg(get_reg_stack(), elem_reg);
                free_reg(get_reg_stack(), idx_reg);
                free_reg(get_reg_stack(), fitems_reg);
                free_reg(get_reg_stack(), loop_var_addr_reg);
                codegen_pop_loop(ctx);
                return inst_list;
            }
            snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        } else if (element_size == 2) {
            const char *word_reg = codegen_register_name16(elem_reg);
            if (word_reg == NULL) {
                codegen_report_error(ctx, "codegen_for_in: unable to get 16-bit register name for element storage");
                free_reg(get_reg_stack(), elem_reg);
                free_reg(get_reg_stack(), idx_reg);
                free_reg(get_reg_stack(), fitems_reg);
                free_reg(get_reg_stack(), loop_var_addr_reg);
                codegen_pop_loop(ctx);
                return inst_list;
            }
            snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
        } else if (element_size == 4) {
            snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", elem_reg->bit_32, loop_var_addr_reg->bit_64);
        } else if (element_size == 8) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", elem_reg->bit_64, loop_var_addr_reg->bit_64);
        } else if (using_flist && element_size > 8) {
            /* For FList with record entries (e.g., TStringItem), we loaded the
             * first field (FString pointer, 8 bytes) — store as a pointer. */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", elem_reg->bit_64, loop_var_addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), elem_reg);
        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), fitems_reg);

        // Generate body
        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        codegen_pop_loop(ctx);

        // Increment label (for continue statements)
        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        // Increment index
        Register_t *inc_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (inc_reg != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, inc_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tincq\t%s\n", inc_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", inc_reg->bit_64, index_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), inc_reg);
        }

        // Condition check label
        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);

        // Compare index < count
        // Load index from stack
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        // Compare directly against cached FCount from stack slot
        snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %%rax\n", count_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        
        // Jump to body if index < count
        snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Exit label
        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);

        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (TFPGList path)\n", __func__);
        #endif
        return inst_list;
    }

    // Check if collection is a string type (dynamic string or shortstring)
    int collection_type_tag = expr_get_type_tag(collection);
    int is_string_collection = (collection_type_tag == STRING_TYPE || collection_type_tag == SHORTSTRING_TYPE);
    
    if (is_string_collection) {
        // Handle FOR-IN iteration over string
        // Structure: for C in S do body
        // Becomes: for i := 1 to Length(S) do C := S[i]; body
        
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        // Allocate stack slots for loop index and string length
        StackNode_t *index_slot = codegen_alloc_temp_slot("str_idx");
        StackNode_t *length_slot = codegen_alloc_temp_slot("str_len");
        StackNode_t *str_ptr_slot = codegen_alloc_temp_slot("str_ptr");
        
        if (index_slot == NULL || length_slot == NULL || str_ptr_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slots for FOR-IN string iteration");
            return inst_list;
        }

        // Get the string pointer value
        Register_t *str_reg = NULL;
        inst_list = codegen_expr_with_result(collection, inst_list, ctx, &str_reg);
        if (codegen_had_error(ctx) || str_reg == NULL) {
            return inst_list;
        }

        // Save string pointer to stack (needed for loop body)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", str_reg->bit_64, str_ptr_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Call kgpc_string_length to get string length
        // Move string pointer to platform-specific first argument register
        const char *arg0 = current_arg_reg64(0);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", str_reg->bit_64, arg0);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), str_reg);
        str_reg = NULL;

        // Call kgpc_string_length with proper shadow space on Windows
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_length");

        // Result is in %rax - save to length slot
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", length_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Initialize index to 1 (Pascal strings are 1-indexed)
        snprintf(buffer, sizeof(buffer), "\tmovq\t$1, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to condition check
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        // Body label
        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Push loop exit label for break/continue statements
        if (!codegen_push_loop(ctx, exit_label, incr_label)) {
            return inst_list;
        }

        // Load character: C := S[index]
        // First, load string pointer from stack
        Register_t *base_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (base_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for string base");
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", str_ptr_slot->offset, base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        // Load index (as 64-bit to use as offset)
        Register_t *idx_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (idx_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for string index");
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        if (collection_type_tag == STRING_TYPE)
        {
            // Subtract 1 from index (convert 1-based to 0-based for dynamic strings)
            snprintf(buffer, sizeof(buffer), "\tdecq\t%s\n", idx_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        // Load character from string: base[index]
        Register_t *char_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (char_reg == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for character");
            free_reg(get_reg_stack(), base_reg);
            free_reg(get_reg_stack(), idx_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s,1), %s\n", 
                 base_reg->bit_64, idx_reg->bit_64, char_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), base_reg);
        free_reg(get_reg_stack(), idx_reg);

        // Assign character to loop variable
        Register_t *loop_var_addr_reg = NULL;
        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), char_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        // Store byte to loop variable
        const char *byte_reg = register_name8(char_reg);
        if (byte_reg == NULL) {
            codegen_report_error(ctx, "codegen_for_in: unable to get 8-bit register name for char storage");
            free_reg(get_reg_stack(), char_reg);
            free_reg(get_reg_stack(), loop_var_addr_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), char_reg);
        free_reg(get_reg_stack(), loop_var_addr_reg);

        // Generate code for the loop body
        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        codegen_pop_loop(ctx);

        // Increment label (for continue statements)
        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);

        // Increment index
        snprintf(buffer, sizeof(buffer), "\tincq\t-%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Condition label
        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);

        // Compare index <= length
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %%rax\n", length_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        // Jump to body if index <= length
        snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        // Exit label
        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);

        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (string path)\n", __func__);
        #endif
        return inst_list;
    }

    if (is_enum_domain) {
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);

        StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_enum_idx");
        if (index_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for enum for-in index");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t$%lld, -%d(%%rbp)\n",
            enum_domain_lower, index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        if (!codegen_push_loop(ctx, exit_label, incr_label))
            return inst_list;

        Register_t *idx_reg = get_free_reg(get_reg_stack(), &inst_list);
        Register_t *loop_var_addr_reg = NULL;
        if (idx_reg == NULL) {
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for enum for-in value");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), idx_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        {
            long long elem_size = (loop_var != NULL && loop_var->resolved_kgpc_type != NULL) ?
                kgpc_type_sizeof(loop_var->resolved_kgpc_type) : 4;
            if (elem_size <= 1) {
                const char *byte_reg = register_name8(idx_reg);
                if (byte_reg == NULL) {
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get byte register for enum for-in");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size == 2) {
                const char *word_reg = codegen_register_name16(idx_reg);
                if (word_reg == NULL) {
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get word register for enum for-in");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size >= 8) {
                snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", idx_reg->bit_32, idx_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", idx_reg->bit_64, loop_var_addr_reg->bit_64);
            } else {
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", idx_reg->bit_32, loop_var_addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), loop_var_addr_reg);

        inst_list = codegen_stmt(body, inst_list, ctx, symtab);
        codegen_pop_loop(ctx);

        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", enum_domain_upper, index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }

    if (is_set_collection) {
        char cond_label[18], body_label[18], exit_label[18], incr_label[18], skip_body_label[18], buffer[256];
        gen_label(cond_label, 18, ctx);
        gen_label(body_label, 18, ctx);
        gen_label(exit_label, 18, ctx);
        gen_label(incr_label, 18, ctx);
        gen_label(skip_body_label, 18, ctx);

        int upper_bound = codegen_set_iteration_upper_bound(ctx, array_type);
        if (upper_bound < 0) {
            codegen_report_error(ctx, "ERROR: Unable to determine FOR-IN set bounds");
            return inst_list;
        }

        StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_set_idx");
        if (index_slot == NULL) {
            codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for set for-in index");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

        snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        if (!codegen_push_loop(ctx, exit_label, incr_label))
            return inst_list;

        Register_t *base_reg = NULL;
        Register_t *idx_reg = get_free_reg(get_reg_stack(), &inst_list);
        Register_t *byte_index_reg = NULL;
        Register_t *bit_reg = NULL;
        Register_t *byte_val_reg = NULL;
        Register_t *mask_reg = NULL;
        Register_t *loop_var_addr_reg = NULL;
        const char *bit_byte_reg = NULL;

        inst_list = codegen_address_for_expr(collection, inst_list, ctx, &base_reg);
        if (base_reg == NULL || codegen_had_error(ctx)) {
            codegen_pop_loop(ctx);
            return inst_list;
        }

        if (idx_reg == NULL) {
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate register for set for-in index");
            return inst_list;
        }
        byte_index_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        bit_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        byte_val_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        mask_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (byte_index_reg == NULL || bit_reg == NULL || byte_val_reg == NULL || mask_reg == NULL) {
            if (mask_reg) free_reg(get_reg_stack(), mask_reg);
            if (byte_val_reg) free_reg(get_reg_stack(), byte_val_reg);
            if (bit_reg) free_reg(get_reg_stack(), bit_reg);
            if (byte_index_reg) free_reg(get_reg_stack(), byte_index_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to allocate registers for set for-in body");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, idx_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", idx_reg->bit_32, byte_index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshrl\t$3, %s\n", byte_index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", idx_reg->bit_32, bit_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tandl\t$7, %s\n", bit_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s,%s,1), %s\n",
            base_reg->bit_64, byte_index_reg->bit_64, byte_val_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        bit_byte_reg = register_name8(bit_reg);
        if (bit_byte_reg == NULL) {
            free_reg(get_reg_stack(), mask_reg);
            free_reg(get_reg_stack(), byte_val_reg);
            free_reg(get_reg_stack(), bit_reg);
            free_reg(get_reg_stack(), byte_index_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            codegen_report_error(ctx, "ERROR: Unable to get byte register for set for-in bit index");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, %%cl\n", bit_byte_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tshll\t%%cl, %s\n", mask_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", mask_reg->bit_32, byte_val_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tjz\t%s\n", skip_body_label);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
        if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
            free_reg(get_reg_stack(), mask_reg);
            free_reg(get_reg_stack(), byte_val_reg);
            free_reg(get_reg_stack(), bit_reg);
            free_reg(get_reg_stack(), byte_index_reg);
            free_reg(get_reg_stack(), idx_reg);
            free_reg(get_reg_stack(), base_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }

        {
            long long elem_size = (loop_var != NULL && loop_var->resolved_kgpc_type != NULL) ?
                kgpc_type_sizeof(loop_var->resolved_kgpc_type) : 4;
            if (elem_size <= 1) {
                const char *byte_reg = register_name8(idx_reg);
                if (byte_reg == NULL) {
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    free_reg(get_reg_stack(), mask_reg);
                    free_reg(get_reg_stack(), byte_val_reg);
                    free_reg(get_reg_stack(), bit_reg);
                    free_reg(get_reg_stack(), byte_index_reg);
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), base_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get byte register for set for-in store");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size == 2) {
                const char *word_reg = codegen_register_name16(idx_reg);
                if (word_reg == NULL) {
                    free_reg(get_reg_stack(), loop_var_addr_reg);
                    free_reg(get_reg_stack(), mask_reg);
                    free_reg(get_reg_stack(), byte_val_reg);
                    free_reg(get_reg_stack(), bit_reg);
                    free_reg(get_reg_stack(), byte_index_reg);
                    free_reg(get_reg_stack(), idx_reg);
                    free_reg(get_reg_stack(), base_reg);
                    codegen_pop_loop(ctx);
                    codegen_report_error(ctx, "ERROR: Unable to get word register for set for-in store");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
            } else if (elem_size >= 8) {
                snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", idx_reg->bit_32, idx_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", idx_reg->bit_64, loop_var_addr_reg->bit_64);
            } else {
                snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", idx_reg->bit_32, loop_var_addr_reg->bit_64);
            }
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), loop_var_addr_reg);
        free_reg(get_reg_stack(), mask_reg);
        free_reg(get_reg_stack(), byte_val_reg);
        free_reg(get_reg_stack(), bit_reg);
        free_reg(get_reg_stack(), byte_index_reg);
        free_reg(get_reg_stack(), idx_reg);
        free_reg(get_reg_stack(), base_reg);

        inst_list = codegen_stmt(body, inst_list, ctx, symtab);

        snprintf(buffer, sizeof(buffer), "%s:\n", skip_body_label);
        inst_list = add_inst(inst_list, buffer);
        codegen_pop_loop(ctx);

        snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", upper_bound, index_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
        inst_list = add_inst(inst_list, buffer);
        return inst_list;
    }
    
    if (array_type == NULL || array_type->kind != TYPE_KIND_ARRAY) {
        codegen_report_error(ctx, "ERROR: FOR-IN collection is not an array type");
        return inst_list;
    }

    // Get array bounds
    int start_index = array_type->info.array_info.start_index;
    int end_index = array_type->info.array_info.end_index;

    // Generate labels for loop control
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[256];
    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);

    // Allocate stack slot for the index variable
    StackNode_t *index_slot = codegen_alloc_temp_slot("for_in_idx");
    if (index_slot == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to allocate temp slot for for-in index");
        return inst_list;
    }

    // Initialize index to start_index
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, -%d(%%rbp)\n", start_index, index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to condition check
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    // Body label
    snprintf(buffer, sizeof(buffer), "%s:\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    // Push loop exit label for break statements
    if (!codegen_push_loop(ctx, exit_label, incr_label))
        return inst_list;

    // Generate code for: loop_var := collection[index]
    // Load index from stack into a register
    Register_t *index_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (index_reg == NULL) {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for for-in index");
        codegen_pop_loop(ctx);
        return inst_list;
    }
    
    snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n", index_slot->offset, index_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    // Get the base address of the array (not its value)
    Register_t *array_base_reg = NULL;
    inst_list = codegen_address_for_expr(collection, inst_list, ctx, &array_base_reg);
    if (array_base_reg == NULL || codegen_had_error(ctx)) {
        free_reg(get_reg_stack(), index_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Calculate offset: (index - start_index) * element_size
    int element_size = kgpc_type_sizeof(array_type->info.array_info.element_type);
    
    // Subtract start_index if non-zero
    if (start_index != 0) {
        snprintf(buffer, sizeof(buffer), "\tsubl\t$%d, %s\n", start_index, index_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    // Multiply by element size
    if (element_size > 1) {
        if ((element_size & (element_size - 1)) == 0) {
            // Power of 2 - use shift
            int shift = 0;
            int temp = element_size;
            while (temp > 1) {
                temp >>= 1;
                shift++;
            }
            if (shift > 0) {
                snprintf(buffer, sizeof(buffer), "\tsall\t$%d, %s\n", shift, index_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
            }
        } else {
            // Not power of 2 - use multiply
            snprintf(buffer, sizeof(buffer), "\timull\t$%d, %s\n", element_size, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    // Sign extend to 64-bit and add to base address
    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", index_reg->bit_32, index_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", index_reg->bit_64, array_base_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    // Now array_base_reg points to the element - load it
    Register_t *element_reg = array_base_reg;  // Reuse the register
    free_reg(get_reg_stack(), index_reg);
    index_reg = NULL;

    if (element_size == 1) {
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 2) {
        snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 4) {
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", element_reg->bit_64, element_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 8) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", element_reg->bit_64, element_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else {
        codegen_report_error(ctx, "ERROR: FOR-IN with large array elements not yet supported");
        free_reg(get_reg_stack(), element_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Assign element value to loop variable
    // Get address of loop variable
    Register_t *loop_var_addr_reg = NULL;
    inst_list = codegen_address_for_expr(loop_var, inst_list, ctx, &loop_var_addr_reg);
    if (loop_var_addr_reg == NULL || codegen_had_error(ctx)) {
        free_reg(get_reg_stack(), element_reg);
        codegen_pop_loop(ctx);
        return inst_list;
    }

    // Store element to loop variable (using proper byte/word/dword/qword)
    if (element_size == 1) {
        const char *byte_reg = register_name8(element_reg);
        if (byte_reg == NULL) {
            codegen_report_error(ctx, "codegen_for_in: unable to get 8-bit register name for element storage");
            free_reg(get_reg_stack(), element_reg);
            free_reg(get_reg_stack(), loop_var_addr_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, (%s)\n", byte_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 2) {
        const char *word_reg = codegen_register_name16(element_reg);
        if (word_reg == NULL) {
            codegen_report_error(ctx, "codegen_for_in: unable to get 16-bit register name for element storage");
            free_reg(get_reg_stack(), element_reg);
            free_reg(get_reg_stack(), loop_var_addr_reg);
            codegen_pop_loop(ctx);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, (%s)\n", word_reg, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 4) {
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n", element_reg->bit_32, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    } else if (element_size == 8) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", element_reg->bit_64, loop_var_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    free_reg(get_reg_stack(), element_reg);
    free_reg(get_reg_stack(), loop_var_addr_reg);

    // Generate code for the loop body
    inst_list = codegen_stmt(body, inst_list, ctx, symtab);
    
    // Increment index
    snprintf(buffer, sizeof(buffer), "\tincl\t-%d(%%rbp)\n", index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to condition
    inst_list = gencode_jmp(NORMAL_JMP, 0, cond_label, inst_list);

    // Condition label
    snprintf(buffer, sizeof(buffer), "%s:\n", cond_label);
    inst_list = add_inst(inst_list, buffer);

    // Compare index with end_index
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$%d, -%d(%%rbp)\n", end_index, index_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    // Jump to body if index <= end_index
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", body_label);
    inst_list = add_inst(inst_list, buffer);

    // Exit label
    snprintf(buffer, sizeof(buffer), "%s:\n", exit_label);
    inst_list = add_inst(inst_list, buffer);

    // Pop loop exit label
    codegen_pop_loop(ctx);

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
    char cond_label[18], body_label[18], exit_label[18], incr_label[18], buffer[128];
    StackNode_t *limit_temp = NULL;

    gen_label(cond_label, 18, ctx);
    gen_label(body_label, 18, ctx);
    gen_label(exit_label, 18, ctx);
    gen_label(incr_label, 18, ctx);
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

    // Determine the direction of the loop
    const int is_downto = stmt->stmt_data.for_data.is_downto;
    
    // Create the update expression based on loop direction
    one_expr = mk_inum(-1, 1);
    if (is_downto) {
        update_expr = mk_addop(-1, MINUS, for_var, one_expr);
    } else {
        update_expr = mk_addop(-1, PLUS, for_var, one_expr);
    }
    // Propagate type information from the loop variable to the update expression
    // This ensures Int64 loop variables use 64-bit operations
    // Note: for_var is guaranteed non-NULL here (checked above at line 7336)
    if (update_expr != NULL)
    {
        if (for_var != NULL && for_var->resolved_kgpc_type != NULL)
        {
            kgpc_type_retain(for_var->resolved_kgpc_type);
            update_expr->resolved_kgpc_type = for_var->resolved_kgpc_type;
        }
        if (for_var->resolved_kgpc_type != NULL)
        {
            update_expr->resolved_kgpc_type = for_var->resolved_kgpc_type;
            kgpc_type_retain(for_var->resolved_kgpc_type);
        }
    }
    update_stmt = mk_varassign(-1, 0, for_var, update_expr);

    inst_list = codegen_evaluate_expr(expr, inst_list, ctx, &limit_reg);
    if (codegen_had_error(ctx) || limit_reg == NULL)
        goto cleanup;

    limit_temp = codegen_alloc_temp_slot("for_to_temp");

    const int limit_is_qword = expr_uses_qword_kgpctype(expr);
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
    if (!codegen_push_loop(ctx, exit_label, incr_label))
        goto cleanup;
    inst_list = codegen_stmt(for_body, inst_list, ctx, symtab);
    codegen_pop_loop(ctx);

    // Increment label (for continue statements)
    snprintf(buffer, sizeof(buffer), "%s:\n", incr_label);
    inst_list = add_inst(inst_list, buffer);

    // Overflow guard: if the loop variable already equals the limit,
    // skip the increment to prevent wrap-around for small types
    // (e.g., Byte 255 + 1 wraps to 0, causing an infinite loop).
    {
        Register_t *guard_reg = NULL;
        inst_list = codegen_evaluate_expr(for_var, inst_list, ctx, &guard_reg);
        if (!codegen_had_error(ctx) && guard_reg != NULL) {
            if (limit_is_qword)
                snprintf(buffer, sizeof(buffer), "\tcmpq\t-%d(%%rbp), %s\n",
                         limit_temp->offset, guard_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tcmpl\t-%d(%%rbp), %s\n",
                         limit_temp->offset, guard_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), guard_reg);
            snprintf(buffer, sizeof(buffer), "\tje\t%s\n", exit_label);
            inst_list = add_inst(inst_list, buffer);
        } else if (guard_reg != NULL) {
            free_reg(get_reg_stack(), guard_reg);
        }
    }

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

    const int var_is_qword = codegen_expr_uses_qword_with_symtab(for_var, symtab);
    const int var_is_signed = codegen_expr_is_signed_with_symtab(for_var, symtab);
    const int use_unsigned_compare = codegen_expr_is_unsigned_with_symtab(for_var, symtab);
    const int compare_as_qword = var_is_qword || limit_is_qword;
    if (compare_as_qword && !limit_is_qword)
    {
        if (var_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, limit_reg->bit_32, limit_reg->bit_64);
    }
    if (compare_as_qword && !var_is_qword)
    {
        if (var_is_signed)
            inst_list = codegen_sign_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_64);
        else
            inst_list = codegen_zero_extend32_to64(inst_list, loop_value_reg->bit_32, loop_value_reg->bit_32);
    }

    const char *cmp_instr = compare_as_qword ? "cmpq" : "cmpl";
    const char *limit_cmp_reg = compare_as_qword ? limit_reg->bit_64 : limit_reg->bit_32;
    const char *loop_cmp_reg = compare_as_qword ? loop_value_reg->bit_64 : loop_value_reg->bit_32;
    snprintf(buffer, sizeof(buffer), "\t%s\t%s, %s\n", cmp_instr, limit_cmp_reg, loop_cmp_reg);
    inst_list = add_inst(inst_list, buffer);

    // Choose the correct branch instruction based on loop direction
    const char *branch_instr;
    if (is_downto) {
        branch_instr = use_unsigned_compare ? "jae" : "jge";  // jump if above or equal / greater or equal
    } else {
        branch_instr = use_unsigned_compare ? "jbe" : "jle";  // jump if below or equal / less or equal
    }
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
    
    /* Evaluate the selector expression once and spill it to stack to free up registers.
     * This prevents register exhaustion when case branches contain complex expressions
     * or function calls that need registers for their own evaluation. */
    struct Expression *selector = stmt->stmt_data.case_data.selector_expr;
    Register_t *selector_reg = NULL;
    inst_list = codegen_expr_with_result(selector, inst_list, ctx, &selector_reg);
    if (selector_reg == NULL)
        return inst_list;

    int selector_is_string = codegen_expr_is_string_like(selector);
    int selector_is_qword = expr_uses_qword_kgpctype(selector);
    
    /* Spill selector value to stack to free the register */
    StackNode_t *selector_spill = (selector_is_string || selector_is_qword) ?
        add_l_t_bytes("case_selector", 8) : add_l_t("case_selector");
    if (selector_spill == NULL)
    {
        free_reg(get_reg_stack(), selector_reg);
        return inst_list;
    }
    
    if (selector_is_string || selector_is_qword)
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            selector_reg->bit_64, selector_spill->offset);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n",
            selector_reg->bit_32, selector_spill->offset);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), selector_reg);
    selector_reg = NULL;

    /* Generate code for each case branch */
    ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
    if (branch_node == NULL) {
        /* No branches - selector was already evaluated and spilled */
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
                if (label_node->type == LIST_EXPR) {
                    struct Expression *label_expr = (struct Expression *)label_node->cur;

                    if (selector_is_string) {
                        int label_is_string = codegen_expr_is_string_like(label_expr);
                        int label_needs_char_promo = (label_expr != NULL &&
                            (label_expr->type == EXPR_CHAR_CODE ||
                             (label_expr->type == EXPR_STRING && expr_get_type_tag(label_expr) == CHAR_TYPE)));
                        if (!label_is_string && !label_needs_char_promo) {
                            label_node = label_node->next;
                            continue;
                        }

                        Register_t *label_reg = NULL;
                        inst_list = codegen_expr_with_result(label_expr, inst_list, ctx, &label_reg);
                        if (label_reg != NULL) {
                            if (label_needs_char_promo) {
                                inst_list = codegen_promote_char_reg_to_string(inst_list, label_reg);
                            }

                            const char *arg0 = current_arg_reg64(0);
                            const char *arg1 = current_arg_reg64(1);
                            if (arg0 != NULL && arg1 != NULL) {
                                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                    selector_spill->offset, arg0);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                                    label_reg->bit_64, arg1);
                                inst_list = add_inst(inst_list, buffer);
                                inst_list = codegen_vect_reg(inst_list, 0);
                                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_compare");
                                snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", RETURN_REG_32);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                                inst_list = add_inst(inst_list, buffer);
                                free_arg_regs();
                            }
                            free_reg(get_reg_stack(), label_reg);
                        }
                    } else if (label_expr->type == EXPR_INUM) {
                        /* For constant labels, compare directly against the spilled value */
                        inst_list = codegen_emit_cmp_spill_immediate(inst_list, ctx,
                            selector_is_qword, label_expr->expr_data.i_num,
                            selector_spill->offset);
                        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                        inst_list = add_inst(inst_list, buffer);
                    } else if (!selector_is_string) {
                        /* For non-constant labels, evaluate label and compare with spilled selector */
                        Register_t *label_reg = NULL;
                        inst_list = codegen_expr_with_result(label_expr, inst_list, ctx, &label_reg);
                        if (label_reg != NULL) {
                            /* Compare selector value from stack with label */
                            if (selector_is_qword)
                                snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                         label_reg->bit_64, selector_spill->offset);
                            else
                                snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                         label_reg->bit_32, selector_spill->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tje\t%s\n", branch_label);
                            inst_list = add_inst(inst_list, buffer);
                            free_reg(get_reg_stack(), label_reg);
                        }
                    }
                } else if (!selector_is_string && label_node->type == LIST_SET_ELEMENT) {
                    struct SetElement *range = (struct SetElement *)label_node->cur;
                    if (range != NULL) {
                        char range_skip_label[18];
                        gen_label(range_skip_label, 18, ctx);

                        int emitted_lower_cmp = 0;
                        if (range->lower != NULL) {
                            if (range->lower->type == EXPR_INUM) {
                                /* Compare constant lower bound against spilled selector */
                                inst_list = codegen_emit_cmp_spill_immediate(inst_list, ctx,
                                    selector_is_qword, range->lower->expr_data.i_num,
                                    selector_spill->offset);
                                emitted_lower_cmp = 1;
                            } else {
                                Register_t *lower_reg = NULL;
                                inst_list = codegen_expr_with_result(range->lower, inst_list, ctx, &lower_reg);
                                if (lower_reg != NULL) {
                                    /* Compare spilled selector against lower bound */
                                    if (selector_is_qword)
                                        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                                 lower_reg->bit_64, selector_spill->offset);
                                    else
                                        snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                                 lower_reg->bit_32, selector_spill->offset);
                                    inst_list = add_inst(inst_list, buffer);
                                    emitted_lower_cmp = 1;
                                    free_reg(get_reg_stack(), lower_reg);
                                }
                            }

                            if (emitted_lower_cmp) {
                                snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", range_skip_label);
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }

                        int emitted_upper_cmp = 0;
                        if (range->upper != NULL) {
                            if (range->upper->type == EXPR_INUM) {
                                /* Compare constant upper bound against spilled selector */
                                inst_list = codegen_emit_cmp_spill_immediate(inst_list, ctx,
                                    selector_is_qword, range->upper->expr_data.i_num,
                                    selector_spill->offset);
                                emitted_upper_cmp = 1;
                            } else {
                                Register_t *upper_reg = NULL;
                                inst_list = codegen_expr_with_result(range->upper, inst_list, ctx, &upper_reg);
                                if (upper_reg != NULL) {
                                    /* Compare spilled selector against upper bound */
                                    if (selector_is_qword)
                                        snprintf(buffer, sizeof(buffer), "\tcmpq\t%s, -%d(%%rbp)\n",
                                                 upper_reg->bit_64, selector_spill->offset);
                                    else
                                        snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, -%d(%%rbp)\n",
                                                 upper_reg->bit_32, selector_spill->offset);
                                    inst_list = add_inst(inst_list, buffer);
                                    emitted_upper_cmp = 1;
                                    free_reg(get_reg_stack(), upper_reg);
                                }
                            }

                            if (emitted_upper_cmp) {
                                snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", branch_label);
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }

                        snprintf(buffer, sizeof(buffer), "%s:\n", range_skip_label);
                        inst_list = add_inst(inst_list, buffer);
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
    /* selector_reg was already freed after spilling to stack */
    return inst_list;
}

ListNode_t *codegen_break_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *exit_label = codegen_current_loop_exit(ctx);
    if (exit_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: BREAK statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    int limit_depth = codegen_current_loop_finally_depth(ctx);
    return codegen_branch_through_finally(ctx, inst_list, symtab, exit_label, limit_depth);
}

ListNode_t *codegen_continue_stmt(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    const char *continue_label = codegen_current_loop_continue(ctx);
    if (continue_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: CONTINUE statement outside of a loop at line %d.\n",
            stmt != NULL ? stmt->line_num : -1);
        return inst_list;
    }

    int limit_depth = codegen_current_loop_finally_depth(ctx);
    return codegen_branch_through_finally(ctx, inst_list, symtab, continue_label, limit_depth);
}



ListNode_t *codegen_with(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    int pushed = 0;
    if (stmt->stmt_data.with_data.context_expr != NULL)
    {
        struct Expression *context_expr = stmt->stmt_data.with_data.context_expr;
        struct RecordType *record_type = codegen_resolve_with_record_type(context_expr, symtab);
        if (record_type != NULL)
            pushed = codegen_with_push(ctx, context_expr, record_type);
        int is_record_context = expr_has_type_tag(context_expr, RECORD_TYPE) ||
            context_expr->record_type != NULL;
        if (is_record_context && codegen_expr_is_addressable(context_expr))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(context_expr, inst_list, ctx, &addr_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
        }
        else
        {
            inst_list = codegen_expr(context_expr, inst_list, ctx);
        }
    }
    if (stmt->stmt_data.with_data.body_stmt != NULL)
        inst_list = codegen_stmt(stmt->stmt_data.with_data.body_stmt, inst_list, ctx, symtab);
    if (pushed)
        codegen_with_pop(ctx);
    return inst_list;
}

ListNode_t *codegen_try_finally(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
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

ListNode_t *codegen_try_except(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;
    ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
    ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;

    char except_label[18];
    char after_label[18];
    gen_label(except_label, sizeof(except_label), ctx);
    gen_label(after_label, sizeof(after_label), ctx);

    /* ── push a runtime except frame (setjmp) ── */
    inst_list = add_inst(inst_list, "\t# TRY/EXCEPT: push runtime except frame\n");
    /* call kgpc_push_except_frame() → returns jmp_buf* in %rax */
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_push_except_frame");
    free_arg_regs();

    /* Spill the jmp_buf pointer to a stack slot so setjmp can use it */
    StackNode_t *jmpbuf_slot = add_l_t("try_except_jmpbuf");
    char buffer[96];
    if (jmpbuf_slot != NULL) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", jmpbuf_slot->offset);
        inst_list = add_inst(inst_list, buffer);
    }

    /* call setjmp(jmp_buf*) — argument is in %rax, move to first arg reg */
    if (codegen_target_is_windows())
    {
        /* Windows x64: _setjmp(jmp_buf*, frame_addr) — needs two args.
         * %rcx = jmp_buf pointer, %rdx = frame pointer (RBP) for SEH unwinding. */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %%rcx\n\tmovq\t%%rbp, %%rdx\n");
    }
    else
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %%rdi\n");
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);

    /* Windows x64: _setjmp requires frame pointer as 2nd arg for SEH unwinding.
     * Linux: _setjmp avoids saving signal mask for performance. */
    inst_list = codegen_call_with_shadow_space(inst_list, "_setjmp");
    free_arg_regs();

    /* If setjmp returned non-zero, we got here from longjmp → jump to except */
    inst_list = add_inst(inst_list, "\ttestl\t%eax, %eax\n");
    snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", except_label);
    inst_list = add_inst(inst_list, buffer);

    /* ── try body (setjmp returned 0) ── */
    if (!codegen_push_except(ctx, except_label)) {
        return inst_list;
    }
    inst_list = codegen_statement_list(try_stmts, inst_list, ctx, symtab);
    codegen_pop_except(ctx);

    /* Pop the runtime except frame (normal exit from try) */
    inst_list = add_inst(inst_list, "\t# TRY/EXCEPT: pop runtime except frame (normal exit)\n");
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_pop_except_frame");
    free_arg_regs();
    inst_list = gencode_jmp(NORMAL_JMP, 0, after_label, inst_list);

    /* ── except handler (setjmp returned non-zero, or local raise jumped here) ── */
    snprintf(buffer, sizeof(buffer), "%s:\n", except_label);
    inst_list = add_inst(inst_list, buffer);

    /* Set the after-label so codegen_on_exception handlers can jump to it
       after executing their body (only one 'on' clause should fire). */
    const char *saved_on_except_after = ctx->on_except_after_label;
    ctx->on_except_after_label = after_label;

    if (except_stmts != NULL)
        inst_list = codegen_statement_list(except_stmts, inst_list, ctx, symtab);
    else
        inst_list = add_inst(inst_list, "\t# EXCEPT block with no handlers\n");

    ctx->on_except_after_label = saved_on_except_after;

    snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}

ListNode_t *codegen_on_exception(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    StackNode_t *exception_var_node = NULL;
    char buffer[CODEGEN_MAX_INST_BUF];
    char *var_name = stmt->stmt_data.on_exception_data.exception_var_name;
    char *type_name = stmt->stmt_data.on_exception_data.exception_type_name;

    /* Determine the effective exception type for dispatch.
     * Pascal has two forms:
     *   on E: ExceptionType do ...   → var_name="E", type_name="ExceptionType"
     *   on ExceptionType do ...      → var_name="ExceptionType", type_name=NULL
     * In the second form, var_name is actually the type name. Detect this by
     * checking if var_name resolves to a class/record type in the symbol table. */
    const char *effective_type = type_name;
    int var_is_actually_type = 0;
    if (effective_type == NULL && var_name != NULL && symtab != NULL)
    {
        HashNode_t *sym = NULL;
        if (FindSymbol(&sym, symtab, var_name) != 0 && sym != NULL &&
            sym->hash_type == HASHTYPE_TYPE)
        {
            effective_type = var_name;
            var_is_actually_type = 1;
        }
    }

    /* ── Type-based dispatch: check if current exception matches this handler ── */
    char skip_label[18];
    int has_type_check = 0;
    if (effective_type != NULL)
    {
        /* Resolve the canonical (declaration-case) name for the exception type.
         * Pascal is case-insensitive, so the source might say "exception" while
         * the class was declared as "Exception". Assembly labels are case-sensitive,
         * so we must use the declaration name to match the _TYPEINFO label. */
        const char *canonical_type = effective_type;
        HashNode_t *type_sym = NULL;
        if (symtab != NULL && FindSymbol(&type_sym, symtab, effective_type) != 0 &&
            type_sym != NULL && type_sym->id != NULL)
        {
            canonical_type = type_sym->id;
        }

        gen_label(skip_label, sizeof(skip_label), ctx);
        has_type_check = 1;

        inst_list = add_inst(inst_list, "\t# ON exception type check\n");
        /* Load current exception object pointer */
        inst_list = add_inst(inst_list, "\tmovq\tkgpc_current_exception(%rip), %rax\n");
        /* If exception is nil, skip this handler */
        inst_list = add_inst(inst_list, "\ttestq\t%rax, %rax\n");
        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", skip_label);
        inst_list = add_inst(inst_list, buffer);
        /* Load typeinfo from exception instance: VMT pointer → typeinfo slot */
        inst_list = add_inst(inst_list, "\tmovq\t(%rax), %rax\n");       /* VMT pointer */
        inst_list = add_inst(inst_list, "\tmovq\t56(%rax), %rax\n");     /* vTypeInfo slot */

        /* Call kgpc_rtti_is(exception_typeinfo, handler_typeinfo) */
        if (codegen_target_is_windows())
        {
            inst_list = add_inst(inst_list, "\tmovq\t%rax, %rcx\n");
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rdx\n", canonical_type);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            inst_list = add_inst(inst_list, "\tmovq\t%rax, %rdi\n");
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rsi\n", canonical_type);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_is");
        free_arg_regs();
        /* If kgpc_rtti_is returned 0, skip this handler */
        inst_list = add_inst(inst_list, "\ttestl\t%eax, %eax\n");
        snprintf(buffer, sizeof(buffer), "\tje\t%s\n", skip_label);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Only create variable binding for the 'on E: Type do' form
     * (not the bare 'on Type do' form where var_name is the type). */
    if (var_name != NULL && !var_is_actually_type) {
        EnterScope(symtab, 0);
        exception_var_node = add_l_x(var_name, 8);

        if (exception_var_node != NULL) {
            snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rax\n");
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", exception_var_node->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    if (stmt->stmt_data.on_exception_data.handler_stmt != NULL)
        inst_list = codegen_stmt(stmt->stmt_data.on_exception_data.handler_stmt, inst_list, ctx, symtab);

    if (var_name != NULL && !var_is_actually_type) {
        remove_last_l_x(var_name);
        LeaveScope(symtab);
    }

    /* After executing the handler body, jump past the remaining handlers */
    if (has_type_check && ctx->on_except_after_label != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", ctx->on_except_after_label);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Skip label for when exception type doesn't match this handler */
    if (has_type_check)
    {
        snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

ListNode_t *codegen_raise(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    if (stmt == NULL)
        return inst_list;

    inst_list = add_inst(inst_list, "\t# RAISE statement\n");

    struct Expression *exc_expr = stmt->stmt_data.raise_data.exception_expr;
    const char *except_label = codegen_current_except_label(ctx);

    Register_t *value_reg = NULL;
    int stored_exception = 0;

    if (exc_expr != NULL)
    {
        inst_list = codegen_expr_with_result(exc_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
            return inst_list;
        inst_list = codegen_store_exception_value(inst_list, ctx, exc_expr, value_reg);
        stored_exception = 1;
        free_reg(get_reg_stack(), value_reg);
        value_reg = NULL;
    }

    if (except_label != NULL)
    {
        /* Pop the runtime except frame before local jump to handler */
        inst_list = add_inst(inst_list, "\t# RAISE: pop runtime except frame (local raise)\n");
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_pop_except_frame");
        free_arg_regs();

        int limit_depth = codegen_current_except_finally_depth(ctx);
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, except_label, limit_depth);
        return inst_list;
    }

    if (!stored_exception)
        inst_list = add_inst(inst_list, "\tmovq\t$0, kgpc_current_exception(%rip)\n");

    char after_label[18];
    int need_after_label = codegen_has_finally(ctx);
    if (need_after_label)
    {
        gen_label(after_label, sizeof(after_label), ctx);
        /* Unwind everything if no handler found */
        inst_list = codegen_branch_through_finally(ctx, inst_list, symtab, after_label, 0);

        char buffer[32];
        snprintf(buffer, sizeof(buffer), "%s:\n", after_label);
        inst_list = add_inst(inst_list, buffer);
    }

    char buffer[96];
    if (codegen_target_is_windows())
        snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rcx\n");
    else
        snprintf(buffer, sizeof(buffer), "\tmovq\tkgpc_current_exception(%%rip), %%rdi\n");
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_raise");
    free_arg_regs();
    inst_list = add_inst(inst_list, "\tud2\n");
    return inst_list;
}

ListNode_t *codegen_inherited(struct Statement *stmt, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    (void)symtab;

    if (stmt == NULL)
        return inst_list;

    int is_class_method = 0;
    if (ctx != NULL && ctx->current_subprogram_owner_class != NULL)
        is_class_method = 1;

    struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
    if (!is_class_method)
    {
        /* Outside of class methods, inherited statements are parsed for compatibility
         * but have no runtime effect yet. */
        inst_list = add_inst(inst_list, "\t# INHERITED statement ignored (no class context)\n");
        return inst_list;
    }

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
