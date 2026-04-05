#include "../codegen_stmt_internal.h"

int record_type_is_mp_integer(const struct RecordType *record_type)
{
    if (record_type == NULL)
        return 0;

    if (record_type->fields == NULL || record_type->fields->next != NULL)
        return 0;

    struct RecordField *field = (struct RecordField *)record_type->fields->cur;
    if (field == NULL || field->name == NULL)
        return 0;

    return strcmp(field->name, "__kgpc_mp_handle") == 0;
}

int codegen_expr_is_mp_integer(struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr_has_type_tag(expr, RECORD_TYPE))
        return record_type_is_mp_integer(expr->record_type);

    if (expr_has_type_tag(expr, POINTER_TYPE))
    {
        /* For pointers, check what they point to via KgpcType if available */
        if (expr->resolved_kgpc_type != NULL && kgpc_type_is_pointer(expr->resolved_kgpc_type))
        {
            int subtype = kgpc_type_get_pointer_subtype_tag(expr->resolved_kgpc_type);
            if (subtype == RECORD_TYPE)
                return record_type_is_mp_integer(expr->record_type);
        }
        /* Fallback to legacy field */
        else if (expr->pointer_subtype == RECORD_TYPE)
        {
            return record_type_is_mp_integer(expr->record_type);
        }
    }

    return 0;
}

ListNode_t *codegen_call_mpint_assign(ListNode_t *inst_list, Register_t *addr_reg,
    Register_t *value_reg)
{
    if (addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_gmp_mpint_assign");
    return inst_list;
}

/* Move two registers into the first two ABI argument registers (arg0, arg1),
 * handling all possible register conflict scenarios. Uses xchgq when both
 * registers are cross-assigned, otherwise moves in the correct order.
 * arg0_reg → first ABI arg register, arg1_reg → second ABI arg register. */
static ListNode_t *codegen_setup_two_arg_regs(ListNode_t *inst_list,
    Register_t *arg0_reg, Register_t *arg1_reg)
{
    char buffer[128];
    const char *abi_arg0 = codegen_target_is_windows() ? "%rcx" : "%rdi";
    const char *abi_arg1 = codegen_target_is_windows() ? "%rdx" : "%rsi";
    int arg0_id = codegen_target_is_windows() ? REG_RCX : REG_RDI;
    int arg1_id = codegen_target_is_windows() ? REG_RDX : REG_RSI;

    int val_in_arg0 = (arg1_reg->reg_id == arg0_id);
    int addr_in_arg1 = (arg0_reg->reg_id == arg1_id);

    if (val_in_arg0 && addr_in_arg1)
    {
        snprintf(buffer, sizeof(buffer), "\txchgq\t%s, %s\n", abi_arg0, abi_arg1);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (val_in_arg0)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg1_reg->bit_64, abi_arg1);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg0_reg->bit_64, abi_arg0);
        inst_list = add_inst(inst_list, buffer);
    }
    else if (addr_in_arg1)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg0_reg->bit_64, abi_arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg1_reg->bit_64, abi_arg1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg0_reg->bit_64, abi_arg0);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg1_reg->bit_64, abi_arg1);
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

/* Call a 2-arg runtime function: func(addr_reg, value_reg)
 * addr_reg → first arg (char**), value_reg → second arg (const char*) */
ListNode_t *codegen_call_string_assign_func(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, const char *func_name)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    inst_list = codegen_setup_two_arg_regs(inst_list, addr_reg, value_reg);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, func_name);
    free_arg_regs();
    return inst_list;
}

ListNode_t *codegen_call_string_assign(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg)
{
    return codegen_call_string_assign_func(inst_list, ctx, addr_reg, value_reg, "kgpc_string_assign");
}

int codegen_expr_is_wide_string_value(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->resolved_kgpc_type != NULL)
    {
        if (kgpc_type_is_wide_string(expr->resolved_kgpc_type))
            return 1;

        if (expr->resolved_kgpc_type->type_alias != NULL)
        {
            const char *alias_name = expr->resolved_kgpc_type->type_alias->alias_name;
            const char *target_name = expr->resolved_kgpc_type->type_alias->target_type_id;
            if ((alias_name != NULL &&
                 (pascal_identifier_equals(alias_name, "UnicodeString") ||
                  pascal_identifier_equals(alias_name, "WideString"))) ||
                (target_name != NULL &&
                 (pascal_identifier_equals(target_name, "UnicodeString") ||
                  pascal_identifier_equals(target_name, "WideString"))))
            {
                return 1;
            }
        }
    }

    if (expr->type == EXPR_FUNCTION_CALL &&
        expr->expr_data.function_call_data.call_kgpc_type != NULL &&
        expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
    {
        KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
        KgpcType *ret_type = kgpc_type_get_return_type(call_type);
        if (ret_type != NULL && kgpc_type_is_wide_string(ret_type))
            return 1;
        if (call_type->info.proc_info.return_type_id != NULL &&
            (pascal_identifier_equals(call_type->info.proc_info.return_type_id, "UnicodeString") ||
             pascal_identifier_equals(call_type->info.proc_info.return_type_id, "WideString")))
        {
            return 1;
        }
    }

    if (expr->type == EXPR_TYPECAST &&
        expr->expr_data.typecast_data.target_type_id != NULL &&
        (pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id, "UnicodeString") ||
         pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id, "WideString")))
    {
        return 1;
    }

    return 0;
}

/* Call kgpc_string_to_char_array(dest, src, size) to copy string to char array */
ListNode_t *codegen_call_string_to_char_array(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    inst_list = codegen_setup_two_arg_regs(inst_list, addr_reg, value_reg);

    char buffer[128];
    if (codegen_target_is_windows()) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
    } else {
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
    }
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_to_char_array");
    free_arg_regs();
    return inst_list;
}

/* Call kgpc_shortstring_to_char_array(dest, src, size) */
ListNode_t *codegen_call_shortstring_to_char_array(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    inst_list = codegen_setup_two_arg_regs(inst_list, addr_reg, value_reg);

    char buffer[128];
    if (codegen_target_is_windows()) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
    } else {
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
    }
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_char_array");
    free_arg_regs();
    return inst_list;
}

/* Call kgpc_char_array_to_shortstring(dest, src, src_len, dest_size) */
ListNode_t *codegen_call_char_array_to_shortstring(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int src_len, int dest_size)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        /* Windows x64 ABI: rcx, rdx, r8, r9 */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", src_len);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r9\n", dest_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* System V ABI: rdi, rsi, rdx, rcx */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", src_len);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rcx\n", dest_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_array_to_shortstring");
    free_arg_regs();
    return inst_list;
}

ListNode_t *codegen_call_string_assign_from_char_array(ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *addr_reg, Register_t *value_reg, int src_len)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", src_len);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", src_len);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_assign_from_char_array");
    free_arg_regs();
    return inst_list;
}

/* Check if an array access expression targets a shortstring element.
 * This handles cases like Names[0] where Names is array[...] of ShortString. */
int codegen_array_access_targets_shortstring(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL)
        return 0;
    if (expr->type != EXPR_ARRAY_ACCESS)
        return 0;
    if (expr->array_element_size == 2 ||
        (expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
        return 0;
    
    struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
    if (base_expr == NULL)
        return 0;
    if (base_expr->array_element_size == 2 ||
        (base_expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(base_expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(base_expr->array_element_type_id, "UnicodeChar"))))
        return 0;
    
    /* If the array access expression itself has shortstring type info, use that */
    if (codegen_expr_is_shortstring_array(expr))
        return 1;
    
    /* Check if base array is declared with shortstring element type */
    if (base_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 && node != NULL)
        {
            /* Check KgpcType for array element info */
            if (node->type != NULL && kgpc_type_is_array(node->type))
            {
                KgpcType *elem_type = kgpc_type_get_array_element_type(node->type);
                if (elem_type != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL)
                    {
                        fprintf(stderr, "[codegen] checking shortstring: base=%s elem_type->kind=%d\n",
                            base_expr->expr_data.id, elem_type->kind);
                    }
                    /* Element type is a shortstring if it's an array of char with proper bounds */
                    if (kgpc_type_is_array(elem_type))
                    {
                        KgpcType *inner_elem = kgpc_type_get_array_element_type(elem_type);
                        if (inner_elem != NULL && inner_elem->kind == TYPE_KIND_PRIMITIVE &&
                            inner_elem->info.primitive_type_tag == CHAR_TYPE)
                        {
                            return 1;  /* array element is array of char = shortstring */
                        }
                    }
                    /* Or if the element type tag is SHORTSTRING_TYPE */
                    if (elem_type->kind == TYPE_KIND_PRIMITIVE &&
                        elem_type->info.primitive_type_tag == SHORTSTRING_TYPE)
                    {
                        return 1;
                    }
                }
            }
        }
    }
    
    return 0;
}

int codegen_expr_is_shortstring_array(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if ((expr->array_element_size == 2) ||
        (expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
        return 0;
    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
        return 1;
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL && alias->is_shortstring)
            return 1;
    }
    /* For record field access, only the RecordField's type is authoritative.
     * Plain array[0..255] of AnsiChar fields are NOT shortstrings. */
    if (expr->type == EXPR_RECORD_ACCESS)
    {
        struct RecordField *field = codegen_lookup_record_field((struct Expression *)expr);
        if (field != NULL && field->type == SHORTSTRING_TYPE)
            return 1;
        /* Field lookup is authoritative — if it succeeded, use its type.
         * If it failed, conservatively return 0 rather than relying on the
         * bounds heuristic, which would false-positive on plain char
         * array[0..255] fields like TextRec.Name. */
        return 0;
    }
    /* Also check by type bounds: string[N] is char array with bounds 0..N */
    if (expr->is_array_expr &&
        expr->array_element_type == CHAR_TYPE &&
        expr->array_element_size != 2 &&
        (expr->array_element_type_id == NULL ||
         (!pascal_identifier_equals(expr->array_element_type_id, "WideChar") &&
          !pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))) &&
        expr_get_array_lower_bound(expr) == 0)
    {
        /* Any char array starting at 0 and sized up to 256 is treated as shortstring */
        int upper = expr_get_array_upper_bound(expr);
        if (upper >= 0 && upper <= 255)
            return 1;
    }
    return 0;
}

static int codegen_expr_has_widechar_array_metadata_local(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->array_element_size == 2)
        return 1;
    if (expr->array_element_type_id != NULL &&
        (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
         pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar")))
    {
        return 1;
    }
    return 0;
}

int codegen_expr_is_shortstring_value_local(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (codegen_expr_has_widechar_array_metadata_local(expr))
        return 0;
    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
        return 1;
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (kgpc_type_is_shortstring(expr->resolved_kgpc_type) ||
            (alias != NULL && alias->is_shortstring))
            return 1;
    }
    return 0;
}

int codegen_shortstring_capacity_from_type_local(KgpcType *type)
{
    if (type == NULL)
        return 0;

    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL && alias->is_shortstring)
    {
        if (alias->array_end >= alias->array_start && alias->array_end >= 0)
            return alias->array_end - alias->array_start + 1;
        if (alias->storage_size > 1)
            return (int)alias->storage_size;
    }

    if (kgpc_type_is_shortstring(type))
    {
        long long type_size = kgpc_type_sizeof(type);
        if (type_size > 1 && type_size <= INT_MAX)
            return (int)type_size;
        return 256;
    }

    return 0;
}

static int codegen_shortstring_capacity_from_array_element_local(const struct Expression *expr,
    CodeGenContext *ctx)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_ACCESS)
        return 0;

    struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
    if (base_expr == NULL)
        return 0;

    KgpcType *base_type = expr_get_kgpc_type(base_expr);
    if (base_type == NULL &&
        base_expr->type == EXPR_VAR_ID &&
        ctx != NULL &&
        ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
            node != NULL)
        {
            base_type = node->type;
        }
    }

    if (base_type != NULL && kgpc_type_is_array(base_type))
    {
        KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
        int capacity = codegen_shortstring_capacity_from_type_local(elem_type);
        if (capacity > 0)
            return capacity;
    }

    if (base_expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        StackNode_t *stack_node = find_label_with_depth(base_expr->expr_data.id, &scope_depth);
        if (stack_node != NULL && stack_node->element_size > 1)
            return stack_node->element_size;
    }

    return 0;
}

static int codegen_record_field_shortstring_capacity(const struct Expression *expr,
    CodeGenContext *ctx)
{
    if (expr == NULL || expr->type != EXPR_RECORD_ACCESS)
        return 0;

    struct RecordField *field = codegen_lookup_record_field((struct Expression *)expr);
    if (field == NULL)
        return 0;

    int shortstring_like = (field->type == SHORTSTRING_TYPE);
    if (!shortstring_like && expr != NULL)
    {
        KgpcType *expr_type = expr_get_kgpc_type(expr);
        if (expr_type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(expr_type);
            if (kgpc_type_is_shortstring(expr_type) ||
                (alias != NULL && alias->is_shortstring))
                shortstring_like = 1;
        }
    }

    if (!shortstring_like)
        return 0;

    if (field->type == SHORTSTRING_TYPE)
    {
        if (field->is_array && field->array_end >= field->array_start && field->array_end >= 0)
            return field->array_end - field->array_start + 1;
    }

    if (ctx != NULL)
    {
        long long field_size = codegen_record_field_effective_size((struct Expression *)expr, ctx);
        if (field_size > 1 && field_size <= INT_MAX)
            return (int)field_size;
    }

    return 0;
}
int codegen_is_current_return_var_id(const struct Expression *expr, CodeGenContext *ctx)
{
    const char *expr_id = NULL;
    const char *current_id = NULL;
    const char *suffix = NULL;
    HashNode_t *shadow_node = NULL;

    if (expr == NULL || ctx == NULL || expr->type != EXPR_VAR_ID)
        return 0;

    expr_id = expr->expr_data.id;
    if (expr_id == NULL)
        return 0;

    if (pascal_identifier_equals(expr_id, "Result"))
    {
        /* A real local/parameter named Result must win over the implicit
         * function-result designator. Only treat bare Result as implicit when
         * semantic lookup did not bind it to an actual symbol. */
        if (ctx->symtab != NULL &&
            FindSymbol(&shadow_node, ctx->symtab, expr_id) != 0 &&
            shadow_node != NULL)
            return 0;
        return 1;
    }

    current_id = ctx->current_subprogram_id;
    if (current_id == NULL)
        return 0;

    if (pascal_identifier_equals(expr_id, current_id))
        return 1;
    if (ctx->current_subprogram_method_name != NULL &&
        pascal_identifier_equals(expr_id, ctx->current_subprogram_method_name))
        return 1;
    if (ctx->current_subprogram_result_name != NULL &&
        pascal_identifier_equals(expr_id, ctx->current_subprogram_result_name))
        return 1;

    suffix = strstr(current_id, "__");
    if (suffix != NULL)
    {
        while (suffix != NULL)
        {
            const char *next = strstr(suffix + 2, "__");
            if (next == NULL)
                break;
            suffix = next;
        }
        suffix += 2;
        if (*suffix != '\0' && pascal_identifier_equals(expr_id, suffix))
            return 1;
    }

    return 0;
}

int codegen_get_char_array_bounds(const struct Expression *expr, CodeGenContext *ctx,
    int *lower_out, int *upper_out, int *is_shortstring_out)
{
    if (lower_out != NULL) *lower_out = 0;
    if (upper_out != NULL) *upper_out = -1;
    if (is_shortstring_out != NULL) *is_shortstring_out = 0;
    if (expr == NULL)
        return 0;

    int found = 0;
    int lower = 0;
    int upper = -1;

    if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
        codegen_is_current_return_var_id(expr, ctx))
    {
        int short_capacity = codegen_get_current_return_shortstring_capacity(ctx, ctx->symtab);
        if (short_capacity > 1)
        {
            lower = 0;
            upper = short_capacity - 1;
            found = 1;
            if (is_shortstring_out != NULL)
                *is_shortstring_out = 1;
        }
    }

    if (!found && expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
    {
        lower = expr_get_array_lower_bound(expr);
        upper = expr_get_array_upper_bound(expr);
        found = 1;
    }
    else
    {
        KgpcType *kgpc = expr_get_kgpc_type(expr);
        int short_capacity = codegen_shortstring_capacity_from_type_local(kgpc);
        if (short_capacity > 0)
        {
            lower = 0;
            upper = short_capacity - 1;
            found = 1;
            if (is_shortstring_out != NULL)
                *is_shortstring_out = 1;
        }
        else if (kgpc != NULL && kgpc_type_is_array(kgpc) &&
            kgpc->info.array_info.element_type != NULL &&
            kgpc->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
            kgpc->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
        {
            lower = kgpc->info.array_info.start_index;
            upper = kgpc->info.array_info.end_index;
            found = 1;
        }
        else if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 && node != NULL &&
            node->type != NULL)
            {
                int node_short_capacity = codegen_shortstring_capacity_from_type_local(node->type);
                if (node_short_capacity > 0)
                {
                    lower = 0;
                    upper = node_short_capacity - 1;
                    found = 1;
                    if (is_shortstring_out != NULL)
                        *is_shortstring_out = 1;
                }
                else if (node->type->kind == TYPE_KIND_PROCEDURE &&
                    node->type->info.proc_info.return_type != NULL)
                {
                    int return_short_capacity = codegen_shortstring_capacity_from_type_local(
                        node->type->info.proc_info.return_type);
                    if (return_short_capacity > 0)
                    {
                        lower = 0;
                        upper = return_short_capacity - 1;
                        found = 1;
                        if (is_shortstring_out != NULL)
                            *is_shortstring_out = 1;
                    }
                }
                else if (node->type->kind == TYPE_KIND_ARRAY &&
                    node->type->info.array_info.element_type != NULL &&
                    node->type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
                    node->type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
                {
                    lower = node->type->info.array_info.start_index;
                    upper = node->type->info.array_info.end_index;
                    found = 1;
                    if (is_shortstring_out != NULL &&
                        node->type->type_alias != NULL &&
                        node->type->type_alias->is_shortstring)
                    {
                        *is_shortstring_out = 1;
                    }
                }
            }

            if (!found &&
                (expr_get_type_tag(expr) == SHORTSTRING_TYPE ||
                 codegen_expr_is_shortstring_value_local(expr)))
            {
                int scope_depth = 0;
                StackNode_t *stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
                if (stack_node != NULL)
                {
                    int slot_size = stack_node->element_size > 0 ?
                        stack_node->element_size : stack_node->size;
                    if (slot_size > 1)
                    {
                        lower = 0;
                        upper = slot_size - 1;
                        found = 1;
                        if (is_shortstring_out != NULL)
                            *is_shortstring_out = 1;
                    }
                }
            }
        }
        else if (expr->type == EXPR_RECORD_ACCESS)
        {
            /* Look up the record field to check if it's a char array */
            struct RecordField *field = codegen_lookup_record_field((struct Expression *)expr);
            int short_capacity = codegen_record_field_shortstring_capacity(expr, ctx);
            if (field != NULL && field->is_array &&
                (field->array_element_type == CHAR_TYPE || field->array_element_type == BYTE_TYPE))
            {
                lower = field->array_start;
                upper = field->array_end;
                found = 1;
                /* Record fields: only SHORTSTRING_TYPE fields are shortstrings.
                 * Plain array[0..255] of AnsiChar is NOT a shortstring. */
                if (is_shortstring_out != NULL)
                    *is_shortstring_out = (field->type == SHORTSTRING_TYPE) ? 1 : 0;
            }
            else if (short_capacity > 1)
            {
                lower = 0;
                upper = short_capacity - 1;
                found = 1;
                if (is_shortstring_out != NULL)
                    *is_shortstring_out = 1;
            }
        }
    }

    if (!found)
        return 0;

    if (lower_out != NULL) *lower_out = lower;
    if (upper_out != NULL) *upper_out = upper;

    if (is_shortstring_out != NULL)
    {
        if (expr->type == EXPR_RECORD_ACCESS)
        {
            /* For record field access, the RecordField's type is authoritative.
             * Do NOT fall through to the array[0..255] heuristic. */
            struct RecordField *field = codegen_lookup_record_field((struct Expression *)expr);
            if (field != NULL && field->type == SHORTSTRING_TYPE)
                *is_shortstring_out = 1;
            else if (field != NULL)
                *is_shortstring_out = 0;
            else
            {
                /* Field lookup failed — conservatively treat as not-shortstring
                 * to avoid false positives on plain char array[0..255] fields
                 * like TextRec.Name.  codegen_expr_is_shortstring_array already
                 * returns 0 in this case. */
                *is_shortstring_out = 0;
            }
        }
        else
        {
            int is_short = (*is_shortstring_out != 0);
            if (!is_short)
                is_short = codegen_expr_is_shortstring_array(expr);
            if (!is_short)
            {
                KgpcType *kgpc = expr_get_kgpc_type(expr);
                if (kgpc != NULL && kgpc->type_alias != NULL && kgpc->type_alias->is_shortstring)
                    is_short = 1;
            }
            if (!is_short && lower == 0 && upper == 255)
                is_short = 1;
            *is_shortstring_out = is_short;
        }
    }

    return 1;
}

/* Detect whether an expression represents a ShortString value on the RHS
 * of an assignment.  Checks context-aware shortstring detection, local
 * shortstring markers, the SHORTSTRING_TYPE tag, and typecast wrappers.
 * Used by both the "LHS is shortstring" and "LHS is AnsiString" assignment
 * branches to keep the detection logic in a single place. */
int codegen_expr_is_shortstring_rhs(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;
    if (codegen_expr_is_shortstring_value_ctx(expr, ctx))
        return 1;
    if (codegen_expr_is_shortstring_value_local(expr))
        return 1;
    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
        return 1;
    /* Unwrap typecasts: e.g. TFormatString(HexStr(...)) where the outer type
     * is AnsiString but the inner expression returns ShortString. */
    if (expr->type == EXPR_TYPECAST &&
        expr->expr_data.typecast_data.expr != NULL &&
        codegen_expr_is_shortstring_value_ctx(expr->expr_data.typecast_data.expr, ctx))
        return 1;
    return 0;
}

int codegen_get_shortstring_capacity(const struct Expression *expr, CodeGenContext *ctx)
{
    int explicit_shortstring = 0;
    if (expr != NULL)
    {
        explicit_shortstring =
            (expr_get_type_tag(expr) == SHORTSTRING_TYPE) ||
            codegen_expr_is_shortstring_array(expr);
    }

    if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
        codegen_is_current_return_var_id(expr, ctx))
    {
        int short_capacity = codegen_get_current_return_shortstring_capacity(ctx, ctx->symtab);
        if (short_capacity > 0)
            return short_capacity;
    }

    if (expr != NULL)
    {
        int record_field_capacity = codegen_record_field_shortstring_capacity(expr, ctx);
        if (record_field_capacity > 1)
            return record_field_capacity;

        int array_element_capacity = codegen_shortstring_capacity_from_array_element_local(expr, ctx);
        if (array_element_capacity > 1)
            return array_element_capacity;

        KgpcType *expr_type = expr_get_kgpc_type(expr);
        if (expr_type != NULL &&
            expr_type->kind == TYPE_KIND_PRIMITIVE &&
            expr_type->info.primitive_type_tag == SHORTSTRING_TYPE)
        {
            int capacity = codegen_shortstring_capacity_from_type_local(expr_type);
            return capacity > 0 ? capacity : 256;
        }
        if (expr_type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(expr_type);
            if (alias != NULL && alias->is_shortstring &&
                alias->array_end >= alias->array_start && alias->array_end >= 0)
            {
                return alias->array_end - alias->array_start + 1;
            }
            if (kgpc_type_is_array(expr_type) &&
                expr_type->type_alias != NULL && expr_type->type_alias->is_shortstring)
            {
                int start = 0;
                int end = -1;
                if (kgpc_type_get_array_bounds(expr_type, &start, &end) == 0 &&
                    end >= start && end >= 0)
                {
                    return end - start + 1;
                }
            }
        }
    }

    if (explicit_shortstring)
    {
        if (expr != NULL && ctx != NULL)
        {
            int lower = 0, upper = -1, is_short = 0;
            if (codegen_get_char_array_bounds(expr, ctx, &lower, &upper, &is_short) &&
                is_short && upper >= lower && upper >= 0)
            {
                return upper - lower + 1;
            }
        }
        return 256;
    }

    if (expr != NULL && expr->is_array_expr)
    {
        int lower_bound = expr_get_array_lower_bound(expr);
        int upper_bound = expr_get_array_upper_bound(expr);
        if (upper_bound >= lower_bound && upper_bound >= 0)
            return upper_bound - lower_bound + 1;
    }

    if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS)
    {
        struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
        KgpcType *base_type = NULL;

        if (base_expr != NULL)
        {
            base_type = base_expr->resolved_kgpc_type;
            if (base_type == NULL &&
                base_expr->type == EXPR_VAR_ID &&
                ctx != NULL &&
                ctx->symtab != NULL)
            {
                HashNode_t *node = NULL;
                if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
                    node != NULL)
                {
                    base_type = node->type;
                }
            }
        }

        if (base_type != NULL && kgpc_type_is_array(base_type))
        {
            KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
            if (elem_type != NULL && kgpc_type_is_array(elem_type))
            {
                int start = 0;
                int end = -1;
                if (kgpc_type_get_array_bounds(elem_type, &start, &end) == 0 &&
                    end >= start && end >= 0)
                {
                    return end - start + 1;
                }
            }
        }
    }

    if (expr != NULL &&
        expr->type == EXPR_VAR_ID &&
        ctx != NULL &&
        ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
            node != NULL)
        {
            if (node->type != NULL)
            {
                int capacity = codegen_shortstring_capacity_from_type_local(node->type);
                if (capacity > 0)
                    return capacity;

                if (kgpc_type_is_procedure(node->type) &&
                    node->type->info.proc_info.return_type != NULL)
                {
                    KgpcType *ret_type = node->type->info.proc_info.return_type;
                    capacity = codegen_shortstring_capacity_from_type_local(ret_type);
                    if (capacity > 0)
                        return capacity;
                }
            }

            int start = 0;
            int end = -1;
            hashnode_get_array_bounds(node, &start, &end);
            if (end >= start && end >= 0)
                return end - start + 1;
        }
    }

    if (expr != NULL && expr->type == EXPR_VAR_ID &&
        (expr_get_type_tag(expr) == SHORTSTRING_TYPE ||
         codegen_expr_is_shortstring_value_local(expr)))
    {
        int scope_depth = 0;
        StackNode_t *stack_node = find_label_with_depth(expr->expr_data.id, &scope_depth);
        if (stack_node != NULL)
        {
            int slot_size = stack_node->element_size > 0 ?
                stack_node->element_size : stack_node->size;
            if (slot_size > 1)
                return slot_size;
        }
    }

    return 256;
}

/* Call kgpc_string_to_shortstring(dest, src, size) to copy string to ShortString */
ListNode_t *codegen_call_string_to_shortstring(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *addr_reg, Register_t *value_reg, int array_size)
{
    if (inst_list == NULL || ctx == NULL || addr_reg == NULL || value_reg == NULL)
        return inst_list;

    if (array_size <= 1)
        array_size = 256;

    char buffer[128];
    if (codegen_target_is_windows())
    {
        /* Windows x64 ABI: first arg in %rcx, second in %rdx, third in %r8 */
        int value_in_rcx = (value_reg->reg_id == REG_RCX);
        int addr_in_rdx = (addr_reg->reg_id == REG_RDX);

        if (value_in_rcx && addr_in_rdx)
        {
            inst_list = add_inst(inst_list, "\txchgq\t%rcx, %rdx\n");
        }
        else if (value_in_rcx)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rdx)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%r8\n", array_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* System V ABI: first arg in %rdi, second in %rsi, third in %rdx */
        int value_in_rdi = (value_reg->reg_id == REG_RDI);
        int addr_in_rsi = (addr_reg->reg_id == REG_RSI);

        if (value_in_rdi && addr_in_rsi)
        {
            inst_list = add_inst(inst_list, "\txchgq\t%rdi, %rsi\n");
        }
        else if (value_in_rdi)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (addr_in_rsi)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", array_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_to_shortstring");
    free_arg_regs();
    return inst_list;
}

ListNode_t *codegen_call_shortstring_copy(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, int dest_size, Register_t *src_reg)
{
    char buffer[128];

    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", dest_size);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n", dest_size);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_shortstring");
    free_arg_regs();
    return inst_list;
}

/* Assign a static array value (copy all elements) */
ListNode_t *codegen_assign_static_array(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    int dest_lower = 0, dest_upper = -1, dest_is_shortstring = 0;
    int src_lower = 0, src_upper = -1, src_is_shortstring = 0;
    int dest_is_char_array = codegen_get_char_array_bounds(dest_expr, ctx,
        &dest_lower, &dest_upper, &dest_is_shortstring);
    int src_is_char_array = codegen_get_char_array_bounds(src_expr, ctx,
        &src_lower, &src_upper, &src_is_shortstring);
    int src_is_shortstring_value = codegen_expr_is_shortstring_value_local(src_expr);
    int dest_is_shortstring_value = codegen_expr_is_shortstring_value_local(dest_expr);

    if ((dest_is_shortstring || dest_is_shortstring_value) &&
        (src_is_shortstring || src_is_shortstring_value))
    {
        Register_t *dest_reg = NULL;
        Register_t *src_reg = NULL;

        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL) free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || src_reg == NULL)
        {
            if (dest_reg != NULL) free_reg(get_reg_stack(), dest_reg);
            if (src_reg != NULL) free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }

        int dest_size = codegen_get_shortstring_capacity(dest_expr, ctx);
        if (dest_size <= 1)
            dest_size = 256;

        inst_list = codegen_call_shortstring_copy(inst_list, ctx, dest_reg, dest_size, src_reg);

        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return inst_list;
    }

    if ((dest_is_shortstring || dest_is_shortstring_value) && src_is_char_array && !src_is_shortstring)
    {
        Register_t *dest_reg = NULL;
        Register_t *src_reg = NULL;
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL || src_reg == NULL)
        {
            if (dest_reg != NULL) free_reg(get_reg_stack(), dest_reg);
            if (src_reg != NULL) free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }

        int src_len = src_upper - src_lower + 1;
        int dest_size = dest_upper - dest_lower + 1;
        if (src_len < 0) src_len = 0;
        if (dest_size < 0) dest_size = 0;
        inst_list = codegen_call_char_array_to_shortstring(inst_list, ctx,
            dest_reg, src_reg, src_len, dest_size);
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return inst_list;
    }

    if (dest_is_char_array && !dest_is_shortstring && (src_is_shortstring || src_is_shortstring_value))
    {
        Register_t *dest_reg = NULL;
        Register_t *src_reg = NULL;
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL || src_reg == NULL)
        {
            if (dest_reg != NULL) free_reg(get_reg_stack(), dest_reg);
            if (src_reg != NULL) free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }

        int dest_size = dest_upper - dest_lower + 1;
        if (dest_size < 0) dest_size = 0;
        inst_list = codegen_call_shortstring_to_char_array(inst_list, ctx,
            dest_reg, src_reg, dest_size);
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return inst_list;
    }

    /* Calculate array size: (upper - lower + 1) * element_size */
    long long lower_bound = expr_get_array_lower_bound(dest_expr);
    long long upper_bound = expr_get_array_upper_bound(dest_expr);
    long long num_elements = -1;
    if (upper_bound >= lower_bound)
        num_elements = upper_bound - lower_bound + 1;

    if (num_elements <= 0)
    {
        if (dest_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_array(dest_expr->resolved_kgpc_type))
        {
            int start = 0;
            int end = -1;
            if (kgpc_type_get_array_bounds(dest_expr->resolved_kgpc_type, &start, &end) == 0 &&
                end >= start)
            {
                num_elements = (long long)end - (long long)start + 1;
            }
        }

        if (num_elements <= 0)
        {
            if (dest_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL &&
                dest_expr->expr_data.id != NULL)
            {
                HashNode_t *var_node = NULL;
                if (FindSymbol(&var_node, ctx->symtab, dest_expr->expr_data.id) != 0 &&
                    var_node != NULL && var_node->type != NULL &&
                    kgpc_type_is_array(var_node->type))
                {
                    int start = 0;
                    int end = -1;
                    if (kgpc_type_get_array_bounds(var_node->type, &start, &end) == 0 &&
                        end >= start)
                    {
                        num_elements = (long long)end - (long long)start + 1;
                    }
                }
            }
        }

        /* For pointer dereference destinations (ptr^), extract the array
         * type from the pointer's pointee.  This handles typed constant
         * arrays assigned through pointer indirection. */
        if (num_elements <= 0 && dest_expr->type == EXPR_POINTER_DEREF)
        {
            KgpcType *deref_type = dest_expr->resolved_kgpc_type;
            /* Try the inner pointer expression's pointee type */
            if (deref_type == NULL || !kgpc_type_is_array(deref_type))
            {
                struct Expression *ptr_expr = dest_expr->expr_data.pointer_deref_data.pointer_expr;
                if (ptr_expr != NULL && ptr_expr->resolved_kgpc_type != NULL &&
                    kgpc_type_is_pointer(ptr_expr->resolved_kgpc_type))
                {
                    KgpcType *pointee = kgpc_type_resolve_pointer_pointee(
                        ptr_expr->resolved_kgpc_type, ctx->symtab);
                    if (pointee != NULL && kgpc_type_is_array(pointee))
                        deref_type = pointee;
                }
                /* Also check pointer_subtype_id on the inner expression */
                if ((deref_type == NULL || !kgpc_type_is_array(deref_type)) &&
                    ptr_expr != NULL && ptr_expr->pointer_subtype_id != NULL &&
                    ctx->symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, ctx->symtab, ptr_expr->pointer_subtype_id) != 0 &&
                        type_node != NULL && type_node->type != NULL &&
                        kgpc_type_is_array(type_node->type))
                    {
                        deref_type = type_node->type;
                    }
                }
            }
            if (deref_type != NULL && kgpc_type_is_array(deref_type))
            {
                int start = 0;
                int end = -1;
                if (kgpc_type_get_array_bounds(deref_type, &start, &end) == 0 &&
                    end >= start)
                {
                    num_elements = (long long)end - (long long)start + 1;
                }
            }
        }

        if (num_elements <= 0)
        {
            struct RecordField *field = codegen_lookup_record_field(dest_expr);
            if (field != NULL && field->is_array && !field->array_is_open)
                num_elements = (long long)field->array_end - (long long)field->array_start + 1;
            else if (field != NULL)
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array && !alias->is_open_array &&
                    alias->array_end >= alias->array_start)
                {
                    num_elements = (long long)alias->array_end - (long long)alias->array_start + 1;
                }
            }
        }

        /* Last resort: derive element count from source array literal.
         * Typed constant arrays (e.g. `const foo: array[0..N] of Rec = (...)`)
         * may have unresolved bounds on the destination when the type comes from
         * a cross-unit declaration.  The source literal knows its own length. */
        if (num_elements <= 0 && src_expr != NULL &&
            src_expr->type == EXPR_ARRAY_LITERAL &&
            src_expr->expr_data.array_literal_data.element_count > 0)
        {
            num_elements = src_expr->expr_data.array_literal_data.element_count;
        }
    }

    long long element_size = expr_get_array_element_size(dest_expr, ctx);

    /* For pointer dereference destinations, extract element size from
     * the pointer's pointee array type. */
    if (element_size <= 0 && dest_expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *ptr_expr = dest_expr->expr_data.pointer_deref_data.pointer_expr;
        KgpcType *arr_type = NULL;
        if (ptr_expr != NULL && ptr_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_pointer(ptr_expr->resolved_kgpc_type))
        {
            KgpcType *pointee = kgpc_type_resolve_pointer_pointee(
                ptr_expr->resolved_kgpc_type, ctx->symtab);
            if (pointee != NULL && kgpc_type_is_array(pointee))
                arr_type = pointee;
        }
        if (arr_type == NULL && ptr_expr != NULL && ptr_expr->pointer_subtype_id != NULL &&
            ctx->symtab != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, ptr_expr->pointer_subtype_id) != 0 &&
                type_node != NULL && type_node->type != NULL &&
                kgpc_type_is_array(type_node->type))
            {
                arr_type = type_node->type;
            }
        }
        if (arr_type != NULL)
        {
            long long elem_size = kgpc_type_get_array_element_size(arr_type);
            if (elem_size <= 0)
            {
                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(arr_type,
                    ctx->symtab);
                if (elem_type != NULL)
                    elem_size = kgpc_type_sizeof(elem_type);
            }
            if (elem_size > 0)
                element_size = elem_size;
        }
    }

    if (element_size <= 0)
    {
        if (dest_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL &&
            dest_expr->expr_data.id != NULL)
        {
            HashNode_t *var_node = NULL;
            if (FindSymbol(&var_node, ctx->symtab, dest_expr->expr_data.id) != 0 &&
                var_node != NULL && var_node->type != NULL &&
                kgpc_type_is_array(var_node->type))
            {
                long long elem_size = kgpc_type_get_array_element_size(var_node->type);
                if (elem_size <= 0)
                {
                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_node->type,
                        ctx->symtab);
                    if (elem_type != NULL)
                        elem_size = kgpc_type_sizeof(elem_type);
                }
                if (elem_size > 0)
                    element_size = elem_size;
            }
        }
    }

    if (element_size <= 0)
    {
        struct RecordField *field = codegen_lookup_record_field(dest_expr);
        if (field != NULL)
        {
            long long computed = 0;
            if (codegen_sizeof_type_reference(ctx, field->array_element_type,
                    field->array_element_type_id, field->nested_record, &computed) == 0 &&
                computed > 0)
            {
                element_size = computed;
            }
            else
            {
                struct TypeAlias *alias = codegen_lookup_type_alias(ctx, field->type_id);
                if (alias != NULL && alias->is_array)
                {
                    long long alias_size = 0;
                    if (codegen_sizeof_type_reference(ctx, alias->array_element_type,
                            alias->array_element_type_id, NULL, &alias_size) == 0 &&
                        alias_size > 0)
                    {
                        element_size = alias_size;
                    }
                }
            }
        }

        /* Fall back to the source array literal's element size when the
         * destination type information is incomplete (cross-unit typed consts). */
        if (element_size <= 0 && src_expr != NULL &&
            src_expr->type == EXPR_ARRAY_LITERAL &&
            src_expr->array_element_size > 0)
        {
            element_size = src_expr->array_element_size;
        }
        if (element_size <= 0 && src_expr != NULL &&
            src_expr->type == EXPR_ARRAY_LITERAL)
        {
            element_size = expr_get_array_element_size(src_expr, ctx);
        }

        if (element_size <= 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to determine element size for array assignment.");
            return inst_list;
        }
    }
    
    long long array_size = num_elements * element_size;
    if (array_size <= 0)
    {
        struct RecordField *field = codegen_lookup_record_field(dest_expr);
        if (field != NULL)
        {
            long long total_size = 0;
            if (codegen_sizeof_type_reference(ctx, field->type, field->type_id,
                    field->nested_record, &total_size) == 0 && total_size > 0)
            {
                array_size = total_size;
                num_elements = 1;
                element_size = total_size;
            }
        }

        /* Last resort: use sizeof the destination's resolved type directly.
         * This handles pointer dereference destinations (ptr^) where the
         * pointee is a fixed-size type (e.g. char array behind PChar). */
        if (array_size <= 0 && dest_expr->resolved_kgpc_type != NULL)
        {
            long long type_size = kgpc_type_sizeof(dest_expr->resolved_kgpc_type);
            if (type_size > 0)
            {
                array_size = type_size;
                num_elements = 1;
                element_size = type_size;
            }
        }

        if (array_size <= 0)
        {
            const char *dest_name = NULL;
            if (dest_expr != NULL)
            {
                if (dest_expr->type == EXPR_VAR_ID)
                    dest_name = dest_expr->expr_data.id;
                else if (dest_expr->type == EXPR_RECORD_ACCESS)
                    dest_name = dest_expr->expr_data.record_access_data.field_id;
            }
            codegen_report_error(ctx,
                "ERROR: Invalid array size for assignment: %lld elements * %lld bytes = %lld total (dest_type=%d%s%s).",
                num_elements, element_size, array_size,
                dest_expr != NULL ? dest_expr->type : -1,
                dest_name != NULL ? " dest=" : "",
                dest_name != NULL ? dest_name : "");
            return inst_list;
        }
    }

    /* Check if the source expression involves a function call.
     * If so, we need to spill the destination address to a stack slot
     * because the function call can clobber any caller-saved register. */
    int src_has_function_call = (src_expr->type == EXPR_FUNCTION_CALL) ||
        expr_returns_sret(src_expr);
    StackNode_t *dest_spill_slot = NULL;
    char buffer[128];

    /* Get address of destination */
    Register_t *dest_reg = NULL;
    inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
    if (codegen_had_error(ctx) || dest_reg == NULL)
    {
        if (dest_reg != NULL)
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    /* If source has a function call, spill destination address to preserve it */
    if (src_has_function_call)
    {
        dest_spill_slot = add_l_t("array_dest_spill");
        if (dest_spill_slot != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                dest_reg->bit_64, dest_spill_slot->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    /* Get address of source */
    Register_t *src_reg = NULL;
    inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
    if (codegen_had_error(ctx) || src_reg == NULL)
    {
        if (src_reg != NULL)
            free_reg(get_reg_stack(), src_reg);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    /* If we spilled the destination, reload it */
    if (dest_spill_slot != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            dest_spill_slot->offset, dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Get register for size */
    Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (count_reg == NULL)
    {
        free_reg(get_reg_stack(), dest_reg);
        free_reg(get_reg_stack(), src_reg);
        return codegen_fail_register(ctx, inst_list, NULL,
            "ERROR: Unable to allocate register for array copy size.");
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_size, count_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    /* Call memcpy(dest, src, size) */
    /* NOTE: We must be careful about register conflicts. Load arguments in reverse order
     * to avoid clobbering source registers before we've moved them to their destinations. */
    if (codegen_target_is_windows())
    {
        /* Windows calling convention: RCX (dest), RDX (src), R8 (size)
         * Move in reverse order to avoid conflicts */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* System V calling convention: RDI (dest), RSI (src), RDX (size)
         * Move in reverse order to avoid conflicts */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", count_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_memcpy_wrapper\n");
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), count_reg);
    free_reg(get_reg_stack(), src_reg);
    free_reg(get_reg_stack(), dest_reg);
    free_arg_regs();
    return inst_list;
}

ListNode_t *codegen_assign_record_value(struct Expression *dest_expr,
    struct Expression *src_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (dest_expr == NULL || src_expr == NULL || ctx == NULL)
        return inst_list;

    if (src_expr->type == EXPR_RECORD_CONSTRUCTOR && src_expr->record_type == NULL)
    {
        struct RecordType *dest_record = dest_expr->record_type;
        if (dest_record == NULL)
        {
            KgpcType *dest_type = expr_get_kgpc_type(dest_expr);
            if (dest_type != NULL && kgpc_type_is_record(dest_type))
                dest_record = kgpc_type_get_record(dest_type);
        }
        if (dest_record != NULL)
        {
            src_expr->record_type = dest_record;
            if (src_expr->resolved_kgpc_type == NULL)
            {
                KgpcType *record_type = create_record_type(dest_record);
                if (record_type != NULL)
                {
                    src_expr->resolved_kgpc_type = record_type;
                }
            }
        }
    }

    /* Check if this is a class assignment. Classes are represented as pointers,
     * so we should just copy the pointer value, not the entire instance. */
    int is_class_assignment = 0;
    if (dest_expr->record_type != NULL && record_type_is_class(dest_expr->record_type))
        is_class_assignment = 1;
    else if (src_expr->record_type != NULL && record_type_is_class(src_expr->record_type))
        is_class_assignment = 1;

    if (is_class_assignment)
    {
        /* For class variables, just copy the pointer (8 bytes) */
        Register_t *dest_reg = NULL;
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        Register_t *src_reg = NULL;
        
        /* For function calls (especially constructors), the expression evaluates to the pointer value directly.
         * For variable references, we need to load the pointer from the variable.
         * We check if the source is addressable to distinguish these cases. */
        int src_is_addressable = codegen_expr_is_addressable(src_expr);
        
        if (src_is_addressable)
        {
            /* Source is a variable - get its address and load the pointer value */
            inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
            if (codegen_had_error(ctx) || src_reg == NULL)
            {
                if (src_reg != NULL)
                    free_reg(get_reg_stack(), src_reg);
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
            
            /* Load the pointer value from the variable */
            Register_t *ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (ptr_reg == NULL)
            {
                free_reg(get_reg_stack(), dest_reg);
                free_reg(get_reg_stack(), src_reg);
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for class pointer copy.");
            }
            
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", src_reg->bit_64, ptr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", ptr_reg->bit_64, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            free_reg(get_reg_stack(), ptr_reg);
            free_reg(get_reg_stack(), src_reg);
        }
        else
        {
            /* Source is a function call or expression that returns the pointer value directly.
             * Save dest_reg to the stack before evaluating source to prevent it from being clobbered. */
            StackNode_t *dest_save_slot = add_l_x("__class_assign_dest__", CODEGEN_POINTER_SIZE_BYTES);
            if (dest_save_slot == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to reserve stack slot for class assignment destination.");
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
            
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                dest_reg->bit_64, dest_save_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), dest_reg);
            dest_reg = NULL;
            
            /* Evaluate the source expression (constructor call) */
            inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &src_reg);
            if (codegen_had_error(ctx) || src_reg == NULL)
            {
                if (src_reg != NULL)
                    free_reg(get_reg_stack(), src_reg);
                return inst_list;
            }
            
            /* Restore dest_reg from stack */
            dest_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (dest_reg == NULL)
            {
                free_reg(get_reg_stack(), src_reg);
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for class assignment destination restore.");
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                dest_save_slot->offset, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* src_reg already contains the pointer value - store it directly */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n", src_reg->bit_64, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            free_reg(get_reg_stack(), src_reg);
        }
        
        if (dest_reg != NULL)
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

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
    Register_t *src_reg = NULL;
    int dest_is_char_set = expr_is_char_set_ctx(dest_expr, ctx);

    /* Default(TRecord) intrinsic: zero-initialize destination without evaluating source */
    if (src_expr->is_default_initializer)
    {
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        const char *dest_arg_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
        const char *val_arg_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
        const char *size_arg_reg = codegen_target_is_windows() ? "%r8"  : "%rdx";

        char buffer[128];
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", dest_reg->bit_64, dest_arg_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\txorq\t%%rax, %%rax\n");
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", val_arg_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", record_size, size_arg_reg);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "memset");
        free_arg_regs();

        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    if (!codegen_expr_is_addressable(src_expr))
    {
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        if (src_expr->type == EXPR_FUNCTION_CALL)
        {
            struct KgpcType *func_type =
                codegen_resolve_function_call_type(ctx, src_expr, NULL);

            const char *func_mangled_name = src_expr->expr_data.function_call_data.mangled_id;
            const char *func_id = src_expr->expr_data.function_call_data.id;

            /* Handle string function results assigned to ShortString arrays.
             * Functions like Copy return AnsiString, which needs to be converted to ShortString format. */
            int dest_is_shortstring = codegen_expr_is_shortstring_array(dest_expr);
            int src_returns_string = (expr_get_type_tag(src_expr) == STRING_TYPE);
            
            if (dest_is_shortstring && src_returns_string)
            {
                char buffer[128];
                
                /* Save dest address to stack before calling function (function call may clobber registers) */
                StackNode_t *dest_save_slot = add_l_x("__shortstring_dest__", CODEGEN_POINTER_SIZE_BYTES);
                if (dest_save_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to reserve stack slot for ShortString destination.");
                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    dest_reg->bit_64, dest_save_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), dest_reg);
                dest_reg = NULL;
                
                /* Call the function to get the string result in %rax */
                inst_list = codegen_expr(src_expr, inst_list, ctx);
                if (codegen_had_error(ctx))
                {
                    return inst_list;
                }
                
                /* The string result is in %rax - save it to a register */
                Register_t *value_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (value_reg == NULL)
                {
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate register for string-to-shortstring conversion.");
                }
                
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                
                /* Reload dest address from stack */
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), value_reg);
                    return codegen_fail_register(ctx, inst_list, NULL,
                        "ERROR: Unable to allocate register for ShortString destination address.");
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    dest_save_slot->offset, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                
                /* Get ShortString capacity */
                int array_size = codegen_get_shortstring_capacity(dest_expr, ctx);
                if (array_size <= 1)
                    array_size = 256;
                
                /* Call the string-to-shortstring conversion */
                inst_list = codegen_call_string_to_shortstring(inst_list, ctx, addr_reg, value_reg, array_size);
                
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            int call_returns_sret = expr_returns_sret(src_expr);
            int call_returns_record = call_returns_sret;
            if (!call_returns_record && func_type != NULL &&
                kgpc_type_is_procedure(func_type))
            {
                KgpcType *return_type = kgpc_type_get_return_type(func_type);
                if (return_type != NULL &&
                    (kgpc_type_is_record(return_type) ||
                     (return_type->kind == TYPE_KIND_ARRAY &&
                      !kgpc_type_is_dynamic_array(return_type)) ||
                     kgpc_type_is_shortstring(return_type) ||
                     (return_type->type_alias != NULL && return_type->type_alias->is_shortstring)))
                {
                    call_returns_record = 1;
                }
            }
            if (!call_returns_record && src_expr->resolved_kgpc_type != NULL)
            {
                KgpcType *src_type = src_expr->resolved_kgpc_type;
                if (kgpc_type_is_record(src_type) ||
                    (src_type->kind == TYPE_KIND_ARRAY &&
                     !kgpc_type_is_dynamic_array(src_type)) ||
                    (src_type->type_alias != NULL && src_type->type_alias->is_shortstring))
                {
                    call_returns_record = 1;
                }
            }
            if (!call_returns_sret && call_returns_record && record_size > 8)
                call_returns_sret = 1;

            /* Detect constructors from semantic checker flag. */
            int is_constructor = src_expr->expr_data.function_call_data.is_constructor_call;

            /* Constructor chaining: when a constructor calls another constructor
             * on Self (e.g., Create(name, mode, 438) inside TFileStream.Create),
             * it's a regular method call, not a new allocation. The first arg
             * will be Self, injected by the semantic checker. */
            if (is_constructor && src_expr->expr_data.function_call_data.args_expr != NULL)
            {
                struct Expression *first_arg = (struct Expression *)
                    src_expr->expr_data.function_call_data.args_expr->cur;
                if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
                    first_arg->expr_data.id != NULL &&
                    pascal_identifier_equals(first_arg->expr_data.id, "Self"))
                    is_constructor = 0;
            }
            /* Record static factories can also be named Create but they are not
             * class constructors and must not use constructor/sret calling paths. */
            if (is_constructor && expr_has_type_tag(dest_expr, RECORD_TYPE))
                is_constructor = 0;
            if (is_constructor && func_type != NULL && kgpc_type_is_procedure(func_type))
            {
                KgpcType *ret_type = kgpc_type_get_return_type(func_type);
                if (ret_type == NULL ||
                    !kgpc_type_is_pointer(ret_type) ||
                    ret_type->info.points_to == NULL ||
                    !kgpc_type_is_record(ret_type->info.points_to) ||
                    !record_type_is_class(ret_type->info.points_to->info.record_info))
                {
                    is_constructor = 0;
                }
            }

            if (call_returns_record || is_constructor)
            {
                /* For constructors, allocate heap memory and initialize VMT */
                Register_t *constructor_instance_reg = NULL;
                if (is_constructor)
                {
                    /* Get the class type from the source expression or first argument */
                    struct RecordType *class_record = src_expr->record_type;
                    if (class_record == NULL && src_expr->resolved_kgpc_type != NULL)
                    {
                        KgpcType *src_type = src_expr->resolved_kgpc_type;
                        if (src_type->kind == TYPE_KIND_RECORD)
                            class_record = src_type->info.record_info;
                        else if (src_type->kind == TYPE_KIND_POINTER &&
                                 src_type->info.points_to != NULL &&
                                 src_type->info.points_to->kind == TYPE_KIND_RECORD)
                            class_record = src_type->info.points_to->info.record_info;
                    }
                    
                    if (class_record == NULL)
                    {
                        ListNode_t *first_arg = src_expr->expr_data.function_call_data.args_expr;
                        if (first_arg != NULL && first_arg->cur != NULL)
                        {
                            struct Expression *class_expr = (struct Expression *)first_arg->cur;
                            if (class_expr != NULL)
                            {
                                class_record = class_expr->record_type;
                                if (class_record == NULL && class_expr->resolved_kgpc_type != NULL)
                                {
                                    KgpcType *arg_type = class_expr->resolved_kgpc_type;
                                    if (arg_type->kind == TYPE_KIND_RECORD)
                                        class_record = arg_type->info.record_info;
                                    else if (arg_type->kind == TYPE_KIND_POINTER &&
                                             arg_type->info.points_to != NULL &&
                                             arg_type->info.points_to->kind == TYPE_KIND_RECORD)
                                        class_record = arg_type->info.points_to->info.record_info;
                                }
                                if (class_record == NULL && class_expr->type == EXPR_VAR_ID &&
                                    class_expr->expr_data.id != NULL && ctx != NULL && ctx->symtab != NULL)
                                {
                                    HashNode_t *class_node = NULL;
                                    if (FindSymbol(&class_node, ctx->symtab, class_expr->expr_data.id) != 0 &&
                                        class_node != NULL && class_node->hash_type == HASHTYPE_TYPE &&
                                        class_node->type != NULL)
                                    {
                                        if (class_node->type->kind == TYPE_KIND_RECORD)
                                            class_record = class_node->type->info.record_info;
                                        else if (class_node->type->kind == TYPE_KIND_POINTER &&
                                                 class_node->type->info.points_to != NULL &&
                                                 class_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                                            class_record = class_node->type->info.points_to->info.record_info;
                                    }
                                }
                            }
                        }
                    }
                    
                    if (class_record != NULL && record_type_is_class(class_record))
                    {
                        /* Get the size of the class instance */
                        long long instance_size = 0;
                        if (codegen_sizeof_record_type(ctx, class_record, &instance_size) == 0 &&
                            instance_size > 0)
                        {
                            char buffer[128];
                            
                            /* Save dest_reg to stack since it will be clobbered by function calls */
                            StackNode_t *dest_save_slot = add_l_x("__constructor_dest__", CODEGEN_POINTER_SIZE_BYTES);
                            if (dest_save_slot == NULL)
                            {
                                codegen_report_error(ctx,
                                    "ERROR: Unable to reserve stack slot for constructor destination.");
                                free_reg(get_reg_stack(), dest_reg);
                                return inst_list;
                            }
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                                dest_reg->bit_64, dest_save_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Allocate zero-initialized memory using calloc
                             * This ensures all fields (including dynamic array descriptors) start at zero */
                            const char *size_arg_reg = codegen_target_is_windows() ? "%rcx" : "%rdi";
                            const char *count_arg_reg = codegen_target_is_windows() ? "%rdx" : "%rsi";
                            
                            /* calloc(1, instance_size) - allocate 1 element of size instance_size */
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$1, %s\n", size_arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                                instance_size, count_arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            
                            inst_list = codegen_vect_reg(inst_list, 0);
                            inst_list = codegen_call_with_shadow_space(inst_list, "calloc");
                            free_arg_regs();
                            
                            /* Save the allocated instance pointer */
                            constructor_instance_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                            if (constructor_instance_reg == NULL)
                            {
                                codegen_report_error(ctx, 
                                    "ERROR: Unable to allocate register for constructor instance.");
                                free_reg(get_reg_stack(), dest_reg);
                                return inst_list;
                            }
                            
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n",
                                constructor_instance_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Initialize VMT pointer in the allocated instance */
                            /* Use the class's TYPEINFO label instead of evaluating the first argument
                             * to avoid side effects like storing into the destination variable */
                            const char *class_type_id = class_record->type_id;
                            if (class_type_id != NULL)
                            {
                                /* Load VMT address */
                                Register_t *vmt_reg = get_free_reg(get_reg_stack(), &inst_list);
                                if (vmt_reg != NULL)
                                {
                                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s_VMT(%%rip), %s\n",
                                        class_type_id, vmt_reg->bit_64);
                                    inst_list = add_inst(inst_list, buffer);
                                    
                                    /* Store VMT into first 8 bytes of instance */
                                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                                        vmt_reg->bit_64, constructor_instance_reg->bit_64);
                                    inst_list = add_inst(inst_list, buffer);

                                    free_reg(get_reg_stack(), vmt_reg);
                                }

                                inst_list = codegen_emit_interface_vtable_slot_init(
                                    inst_list, ctx, class_record, class_type_id,
                                    constructor_instance_reg);
                            }

                            /* Pass remaining arguments starting from index 1 (skip class type argument) */
                            inst_list = codegen_pass_arguments(
                                src_expr->expr_data.function_call_data.args_expr, inst_list, ctx,
                                func_type, func_id, 1, src_expr, 0);

                            /* Emit Self AFTER argument evaluation so that arg-passing code
                             * cannot clobber the Self register (e.g. %rdi on SysV). */
                            const char *self_arg_reg = current_arg_reg64(0);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                                constructor_instance_reg->bit_64, self_arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Call the constructor */
                            snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", func_mangled_name);
                            inst_list = add_inst(inst_list, buffer);
                            inst_list = codegen_cleanup_call_stack(inst_list, ctx);
                            codegen_release_function_call_mangled_id(src_expr);
                            
                            /* Restore dest_reg from stack */
                            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                dest_save_slot->offset, dest_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            
                            /* Store the instance pointer in the destination */
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                                constructor_instance_reg->bit_64, dest_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            
                            free_reg(get_reg_stack(), constructor_instance_reg);
                            free_reg(get_reg_stack(), dest_reg);
                            return inst_list;
                        }
                    }
                }
                
                /* Normal record-returning function via sret pointer. */
                if (call_returns_sret)
                {
                    const char *ret_ptr_reg = current_arg_reg64(0);
                    if (ret_ptr_reg == NULL)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to determine register for record return pointer.");
                        free_reg(get_reg_stack(), dest_reg);
                        return inst_list;
                    }

                    StackNode_t *dest_save_slot = add_l_x("__record_call_dest__", CODEGEN_POINTER_SIZE_BYTES);
                    if (dest_save_slot == NULL)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to reserve stack slot for record return destination.");
                        free_reg(get_reg_stack(), dest_reg);
                        return inst_list;
                    }

                    char buffer[128];
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        dest_reg->bit_64, dest_save_slot->offset);
                    inst_list = add_inst(inst_list, buffer);

                    inst_list = codegen_pass_arguments(
                        src_expr->expr_data.function_call_data.args_expr, inst_list, ctx,
                        func_type,
                        src_expr->expr_data.function_call_data.id, 1, src_expr, 0);

                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        dest_save_slot->offset, ret_ptr_reg);
                    inst_list = add_inst(inst_list, buffer);

                    /* For class method calls, dereference Self to get VMT pointer.
                     * Self is at arg reg 1 (after SRET buffer at arg reg 0). */
                    if (src_expr->expr_data.function_call_data.is_class_method_call)
                    {
                        const char *self_reg = current_arg_reg64(1);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", self_reg, self_reg);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    snprintf(buffer, sizeof(buffer), "\tcall\t%s\n",
                        src_expr->expr_data.function_call_data.mangled_id);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_cleanup_call_stack(inst_list, ctx);
                    codegen_release_function_call_mangled_id(src_expr);

                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }
            }

            /* Small record returns (<= 8 bytes) are returned in registers, not via sret.
             * Materialize the call result into a register and store the raw value bytes
             * directly into the destination record slot. */
            if (!is_constructor && call_returns_record && record_size <= 8)
            {
                StackNode_t *dest_save_slot = add_l_x("__small_record_dest__", CODEGEN_POINTER_SIZE_BYTES);
                if (dest_save_slot == NULL)
                {
                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }

                char buffer[128];
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    dest_reg->bit_64, dest_save_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), dest_reg);
                dest_reg = NULL;

                Register_t *value_reg = NULL;
                inst_list = codegen_expr_with_result(src_expr, inst_list, ctx, &value_reg);
                if (codegen_had_error(ctx) || value_reg == NULL)
                {
                    if (value_reg != NULL)
                        free_reg(get_reg_stack(), value_reg);
                    return inst_list;
                }

                dest_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (dest_reg == NULL)
                {
                    free_reg(get_reg_stack(), value_reg);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    dest_save_slot->offset, dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                if (record_size <= 4)
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, (%s)\n",
                        value_reg->bit_32, dest_reg->bit_64);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                        value_reg->bit_64, dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
        }

        /* Handle character set literals - they generate a temporary buffer address */
        if (src_expr->type == EXPR_SET &&
            (expr_is_char_set_ctx(src_expr, ctx) || dest_is_char_set))
        {
            /* Generate the set literal, which returns an address register */
            Register_t *src_reg = NULL;
            int force_char_literal = dest_is_char_set && !expr_is_char_set_ctx(src_expr, ctx);
            inst_list = codegen_set_literal(src_expr, inst_list, ctx, &src_reg, force_char_literal);
            if (codegen_had_error(ctx) || src_reg == NULL)
            {
                if (src_reg != NULL)
                    free_reg(get_reg_stack(), src_reg);
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }

            /* src_reg now contains the address of the temporary set buffer */
            /* Copy 32 bytes from src to dest using memcpy */
            Register_t *count_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (count_reg == NULL)
            {
                free_reg(get_reg_stack(), dest_reg);
                free_reg(get_reg_stack(), src_reg);
                return codegen_fail_register(ctx, inst_list, NULL,
                    "ERROR: Unable to allocate register for set copy size.");
            }

            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t$32, %s\n", count_reg->bit_64);
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

            snprintf(buffer, sizeof(buffer), "\tcall\tkgpc_memcpy_wrapper\n");
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), count_reg);
            free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), dest_reg);
            free_arg_regs();
            return inst_list;
        }

        /* Handle character set binary operations (union, intersection, difference).
         * For 32-byte sets (set of Char), the standard binary codegen only operates
         * on 4 bytes. Use runtime helpers that operate on all 32 bytes. */
        if (dest_is_char_set &&
            (src_expr->type == EXPR_ADDOP || src_expr->type == EXPR_MULOP))
        {
            const char *runtime_func = NULL;
            struct Expression *left_op = NULL;
            struct Expression *right_op = NULL;

            if (src_expr->type == EXPR_ADDOP)
            {
                int op = src_expr->expr_data.addop_data.addop_type;
                left_op = src_expr->expr_data.addop_data.left_expr;
                right_op = src_expr->expr_data.addop_data.right_term;
                if (op == PLUS)
                    runtime_func = "kgpc_set_union_256";
                else if (op == MINUS)
                    runtime_func = "kgpc_set_diff_256";
            }
            else /* EXPR_MULOP */
            {
                int op = src_expr->expr_data.mulop_data.mulop_type;
                left_op = src_expr->expr_data.mulop_data.left_term;
                right_op = src_expr->expr_data.mulop_data.right_factor;
                if (op == STAR)
                    runtime_func = "kgpc_set_intersect_256";
            }

            if (runtime_func != NULL && left_op != NULL && right_op != NULL)
            {
                /* Get addresses of both operands */
                Register_t *left_reg = NULL;
                Register_t *right_reg = NULL;

                inst_list = codegen_char_set_address(left_op, inst_list, ctx, &left_reg);
                if (codegen_had_error(ctx) || left_reg == NULL)
                {
                    if (left_reg != NULL) free_reg(get_reg_stack(), left_reg);
                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }

                inst_list = codegen_char_set_address(right_op, inst_list, ctx, &right_reg);
                if (codegen_had_error(ctx) || right_reg == NULL)
                {
                    if (right_reg != NULL) free_reg(get_reg_stack(), right_reg);
                    free_reg(get_reg_stack(), left_reg);
                    free_reg(get_reg_stack(), dest_reg);
                    return inst_list;
                }

                /* Call runtime_func(dest, left, right) */
                char buffer[128];
                if (codegen_target_is_windows())
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", left_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", right_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", left_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", right_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, runtime_func);

                free_reg(get_reg_stack(), right_reg);
                free_reg(get_reg_stack(), left_reg);
                free_reg(get_reg_stack(), dest_reg);
                free_arg_regs();
                return inst_list;
            }
        }

        /* Handle string literal assigned to ShortString (record-like) destination.
         * ShortStrings use Pascal format: length byte at index 0, followed by string data.
         * We need to convert the C string literal to a ShortString. */
        if (src_expr->type == EXPR_STRING)
        {
            const char *str_data = src_expr->expr_data.string;
            int str_len = (str_data != NULL) ? (int)strlen(str_data) : 0;
            if (str_len > 255) str_len = 255;  /* ShortString max length */
            
            /* Put string literal in rodata section */
            const char *readonly_section = codegen_readonly_section_directive();
            char label[64];
            snprintf(label, sizeof(label), ".LC%d", ctx->write_label_counter++);
            
            char escaped_str[CODEGEN_MAX_INST_BUF];
            escape_string(escaped_str, str_data ? str_data : "", sizeof(escaped_str));
            /* Use larger buffer for string literal embedding to avoid truncation */
            char str_literal_buffer[CODEGEN_MAX_INST_BUF + 128];
            snprintf(str_literal_buffer, sizeof(str_literal_buffer), "%s\n%s:\n\t.string \"%s\"\n%s\n",
                     readonly_section, label, escaped_str, codegen_text_section_resume());
            inst_list = add_inst(inst_list, str_literal_buffer);
            
            /* Get register for string literal address */
            Register_t *str_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (str_addr_reg == NULL)
            {
                free_reg(get_reg_stack(), dest_reg);
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for string literal address.");
                return inst_list;
            }
            
            /* Load string literal address */
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                label, str_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            
            /* Call kgpc_string_to_shortstring(dest, src, max_len).
             * Use the declared capacity for string[N] (= N+1) to avoid
             * overflowing smaller-than-255 buffers.
             * codegen_get_shortstring_capacity returns 256 when capacity
             * cannot be determined; codegen_call_string_to_shortstring
             * also guards against invalid (<= 1) values internally. */
            int dest_capacity = codegen_get_shortstring_capacity(dest_expr, ctx);
            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", str_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", dest_capacity);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", str_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", dest_capacity);
                inst_list = add_inst(inst_list, buffer);
            }
            
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_to_shortstring");
            free_arg_regs();
            
            free_reg(get_reg_stack(), str_addr_reg);
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        {
            Register_t *src_addr_reg = NULL;
            inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_addr_reg);
            if (!codegen_had_error(ctx) && src_addr_reg != NULL)
            {
                char copy_buf[128];
                long long size_val = record_size > 0 ? record_size : 1;

                if (codegen_target_is_windows())
                {
                    snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buf);
                    snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t%s, %%rdx\n", src_addr_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buf);
                    snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t$%lld, %%r8\n", size_val);
                    inst_list = add_inst(inst_list, copy_buf);
                }
                else
                {
                    snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buf);
                    snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t%s, %%rsi\n", src_addr_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buf);
                    snprintf(copy_buf, sizeof(copy_buf), "\tmovq\t$%lld, %%rdx\n", size_val);
                    inst_list = add_inst(inst_list, copy_buf);
                }

                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_memcpy_wrapper");
                free_arg_regs();

                free_reg(get_reg_stack(), src_addr_reg);
                free_reg(get_reg_stack(), dest_reg);
                return inst_list;
            }
            if (src_addr_reg != NULL)
                free_reg(get_reg_stack(), src_addr_reg);
        }

        codegen_report_error(ctx,
            "ERROR: Unsupported record-valued source expression (type=%d).", src_expr ? src_expr->type : -1);
        free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    int src_has_call = expr_contains_function_call(src_expr) ||
        (src_expr != NULL && src_expr->type == EXPR_RECORD_CONSTRUCTOR);
    int dest_has_call = expr_contains_function_call(dest_expr) ||
        (dest_expr != NULL && dest_expr->type == EXPR_RECORD_CONSTRUCTOR);
    StackNode_t *dest_spill = NULL;
    StackNode_t *src_spill = NULL;

    if (src_has_call && !dest_has_call)
    {
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }
        dest_spill = add_l_t_bytes("record_copy_dest_spill", 8);
        if (dest_spill == NULL)
        {
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }
        char spill_buf[128];
        snprintf(spill_buf, sizeof(spill_buf), "\tmovq\t%s, -%d(%%rbp)\n",
            dest_reg->bit_64, dest_spill->offset);
        inst_list = add_inst(inst_list, spill_buf);

        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || src_reg == NULL)
        {
            if (src_reg != NULL)
                free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }
        snprintf(spill_buf, sizeof(spill_buf), "\tmovq\t-%d(%%rbp), %s\n",
            dest_spill->offset, dest_reg->bit_64);
        inst_list = add_inst(inst_list, spill_buf);
    }
    else if (dest_has_call && !src_has_call)
    {
        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || src_reg == NULL)
        {
            if (src_reg != NULL)
                free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }
        src_spill = add_l_t_bytes("record_copy_src_spill", 8);
        if (src_spill == NULL)
        {
            free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }
        char spill_buf[128];
        snprintf(spill_buf, sizeof(spill_buf), "\tmovq\t%s, -%d(%%rbp)\n",
            src_reg->bit_64, src_spill->offset);
        inst_list = add_inst(inst_list, spill_buf);

        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            free_reg(get_reg_stack(), src_reg);
            return inst_list;
        }
        snprintf(spill_buf, sizeof(spill_buf), "\tmovq\t-%d(%%rbp), %s\n",
            src_spill->offset, src_reg->bit_64);
        inst_list = add_inst(inst_list, spill_buf);
    }
    else
    {
        inst_list = codegen_address_for_expr(dest_expr, inst_list, ctx, &dest_reg);
        if (codegen_had_error(ctx) || dest_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }

        inst_list = codegen_address_for_expr(src_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || src_reg == NULL)
        {
            if (src_reg != NULL)
                free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), dest_reg);
            return inst_list;
        }
    }

    char buffer[128];

    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r8\n", record_size);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%rdx\n", record_size);
        inst_list = add_inst(inst_list, buffer);
    }

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");

    free_reg(get_reg_stack(), dest_reg);
    free_reg(get_reg_stack(), src_reg);
    free_arg_regs();
    return inst_list;
}

