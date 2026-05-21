/*
    Damon Gwinn
    Code generation for argument-passing and nonlocal variable access
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>

#include "codegen.h"
#include "codegen_expression.h"
#include "codegen_expr_arguments.h"
#include "codegen_expr_array.h"
#include "codegen_expr_relop.h"
#include "codegen_expr_sizeof.h"
#include "register_types.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../identifier_utils.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_Expr_Internal.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemCheck.h"
#include "../../Parser/pascal_frontend.h"
#include "../../identifier_utils.h"
#include "../../format_arg.h"
#include "../../unit_registry.h"


/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);

#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_SIZEOF_RECURSION_LIMIT 32

/* Forward declarations for functions defined in codegen_expression.c */
int codegen_expr_is_char_array_like_ctx(const struct Expression *expr, CodeGenContext *ctx);
int codegen_get_char_array_length(const struct Expression *expr, CodeGenContext *ctx,
    long long *out_len);
struct RecordType *codegen_expr_record_type(const struct Expression *expr, SymTab_t *symtab);
ListNode_t *codegen_promote_shortstring_reg(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *value_reg);
ListNode_t *codegen_expr_tree_value(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);
int expr_is_char_pointer(const struct Expression *expr);
ListNode_t *codegen_spill_reg64_temp(ListNode_t *inst_list, CodeGenContext *ctx, const Register_t *reg,
    const char *temp_name, StackNode_t **spill_slot);
ListNode_t *codegen_restore_spilled_reg64(ListNode_t *inst_list, CodeGenContext *ctx, const Register_t *reg,
    StackNode_t *spill_slot);
int codegen_expr_function_call_returns_ansistring(CodeGenContext *ctx,
    const struct Expression *expr);

/* Static inline helpers duplicated from codegen_expression.c (established codebase pattern) */
static inline struct RecordType *codegen_get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

static Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx, const char *usage)
{
    Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
    if (reg == NULL)
        reg = get_reg_with_spill(get_reg_stack(), inst_list);
    if (reg == NULL)
        codegen_report_error(ctx, "ERROR: Unable to allocate register for %s.", usage);
    return reg;
}




static int codegen_var_decl_contains_id(const Tree_t *decl, const char *var_id)
{
    if (decl == NULL || var_id == NULL)
        return 0;

    ListNode_t *ids = NULL;
    if (decl->type == TREE_VAR_DECL)
        ids = decl->tree_data.var_decl_data.ids;
    else if (decl->type == TREE_ARR_DECL)
        ids = decl->tree_data.arr_decl_data.ids;
    else
        return 0;

    for (ListNode_t *cur = ids; cur != NULL; cur = cur->next)
    {
        const char *decl_id = (const char *)cur->cur;
        if (decl_id != NULL && pascal_identifier_equals(decl_id, var_id))
            return 1;
    }

    return 0;
}

static const Tree_t *codegen_find_var_decl_in_list(ListNode_t *decls, const char *var_id)
{
    for (ListNode_t *cur = decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL)
            continue;
        if ((decl->type == TREE_VAR_DECL || decl->type == TREE_ARR_DECL) &&
            codegen_var_decl_contains_id(decl, var_id))
            return decl;
    }

    return NULL;
}

static const Tree_t *codegen_find_var_decl_for_symbol(CodeGenContext *ctx,
    const HashNode_t *sym_node, const char *var_id)
{
    if (ctx == NULL || ctx->comp_ctx == NULL || var_id == NULL)
        return NULL;

    if (sym_node != NULL && sym_node->source_unit_index > 0)
    {
        LoadedUnit *loaded_unit = compilation_context_find_unit(
            ctx->comp_ctx, sym_node->source_unit_index);
        if (loaded_unit != NULL && loaded_unit->unit_tree != NULL &&
            loaded_unit->unit_tree->type == TREE_UNIT)
        {
            Tree_t *unit = loaded_unit->unit_tree;
            const Tree_t *decl = codegen_find_var_decl_in_list(
                unit->tree_data.unit_data.interface_var_decls, var_id);
            if (decl != NULL)
                return decl;
            return codegen_find_var_decl_in_list(
                unit->tree_data.unit_data.implementation_var_decls, var_id);
        }
    }

    if (ctx->comp_ctx->program != NULL &&
        ctx->comp_ctx->program->type == TREE_PROGRAM_TYPE)
    {
        return codegen_find_var_decl_in_list(
            ctx->comp_ctx->program->tree_data.program_data.var_declaration, var_id);
    }

    return NULL;
}

static const Tree_t *codegen_find_var_decl_for_unit(CodeGenContext *ctx,
    int source_unit_index, const char *var_id)
{
    if (ctx == NULL || ctx->comp_ctx == NULL || source_unit_index <= 0 || var_id == NULL)
        return NULL;

    LoadedUnit *loaded_unit = compilation_context_find_unit(
        ctx->comp_ctx, source_unit_index);
    if (loaded_unit == NULL || loaded_unit->unit_tree == NULL ||
        loaded_unit->unit_tree->type != TREE_UNIT)
        return NULL;

    Tree_t *unit = loaded_unit->unit_tree;
    const Tree_t *decl = codegen_find_var_decl_in_list(
        unit->tree_data.unit_data.interface_var_decls, var_id);
    if (decl != NULL)
        return decl;
    return codegen_find_var_decl_in_list(
        unit->tree_data.unit_data.implementation_var_decls, var_id);
}

static const char *codegen_global_access_symbol_for_decl(const Tree_t *decl,
    const char *var_id)
{
    if (decl == NULL || var_id == NULL)
        return NULL;

    if (decl->type == TREE_VAR_DECL)
    {
        const char *alias = decl->tree_data.var_decl_data.cname_override;
        if (alias != NULL && alias[0] != '\0')
            return alias;
        return var_id;
    }

    if (decl->type == TREE_ARR_DECL)
        return var_id;

    return NULL;
}

static struct Expression *codegen_unwrap_typecast_call_expr(struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL || symtab == NULL)
        return NULL;

    const char *id = expr->expr_data.function_call_data.id;
    if (id == NULL)
        return NULL;

    HashNode_t *type_node = NULL;
    if (FindSymbol(&type_node, symtab, id) == 0 ||
        type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
        return NULL;

    ListNode_t *args = expr->expr_data.function_call_data.args_expr;
    if (args == NULL || args->next != NULL)
        return NULL;

    return (struct Expression *)args->cur;
}

static int codegen_record_has_class_var_named(const struct RecordType *record,
    const char *field_id)
{
    if (record == NULL || field_id == NULL)
        return 0;

    for (ListNode_t *node = record->fields; node != NULL; node = node->next)
    {
        if (node->type != LIST_RECORD_FIELD || node->cur == NULL)
            continue;
        struct RecordField *field = (struct RecordField *)node->cur;
        if (field->is_class_var == 1 && field->name != NULL &&
            pascal_identifier_equals(field->name, field_id))
            return 1;
    }

    return 0;
}

static const char *codegen_outer_owner_class_from_full(const char *owner_class,
    const char *owner_class_full, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return NULL;
    buffer[0] = '\0';

    if (owner_class_full != NULL && owner_class != NULL)
    {
        const char *suffix = strstr(owner_class_full, owner_class);
        if (suffix != NULL && suffix > owner_class_full && suffix[-1] == '.')
        {
            size_t len = (size_t)((suffix - 1) - owner_class_full);
            if (len > 0 && len < size)
            {
                memcpy(buffer, owner_class_full, len);
                buffer[len] = '\0';
                return buffer;
            }
        }
    }

    return NULL;
}

static StackNode_t *codegen_find_nonlocal_lexical(CodeGenContext *ctx,
    const char *var_id, int *scope_depth, HashNode_t **sym_node_out)
{
    if (scope_depth != NULL)
        *scope_depth = 0;
    if (sym_node_out != NULL)
        *sym_node_out = NULL;

    StackNode_t *var = find_label_with_depth(var_id, scope_depth);
    if (var != NULL || ctx == NULL || ctx->symtab == NULL)
        return var;

    HashNode_t *sym_node = NULL;
    if (FindSymbol(&sym_node, ctx->symtab, var_id) != 0 && sym_node != NULL)
    {
        /* Typed-consts declared in a unit are stored under the qualified key
         * "<unit>_$_<name>" (see codegen_var_storage_key) so that same-named
         * typed-consts in multiple units (e.g. each FPC charmap unit's
         * `unicodemap`) don't alias.  Compute the qualified key directly. */
        if (sym_node->is_typed_const && sym_node->source_unit_index > 0)
        {
            char *qualified = codegen_make_unit_qualified_key(
                sym_node->source_unit_index, var_id);
            if (qualified != NULL)
            {
                var = find_label_with_depth(qualified, scope_depth);
                free(qualified);
            }
        }
        if (var == NULL && sym_node->mangled_id != NULL)
            var = find_label_with_depth(sym_node->mangled_id, scope_depth);
    }

    if (sym_node_out != NULL)
        *sym_node_out = sym_node;
    return var;
}

static ListNode_t *codegen_try_emit_nonlocal_global(ListNode_t *inst_list,
    const char *var_id, CodeGenContext *ctx, const HashNode_t *sym_node, int *offset)
{
    if (ctx == NULL || ctx->symtab == NULL || ctx->comp_ctx == NULL)
        return NULL;

    /* Unit-aware HashNode selection for typed-consts: when the current
     * codegen scope is inside a unit init/var-initializer (current_unit_index
     * > 0) and the referenced identifier is a typed-const declared in that
     * unit, prefer that unit's HashNode over any other matching candidate.
     * This makes `@unicodemap` inside cp1252.pas's init block resolve to
     * cp1252's typed-const HashNode regardless of which unit registered the
     * symbol first.  This is the primary resolution path for unit-scoped
     * typed-const references — not a fallback. */
    const HashNode_t *effective_node = NULL;
    if (ctx->symtab->current_unit_index > 0)
    {
        ListNode_t *cands = FindAllIdents(ctx->symtab, var_id);
        for (ListNode_t *c = cands; c != NULL; c = c->next)
        {
            HashNode_t *cand = (HashNode_t *)c->cur;
            if (cand == NULL)
                continue;
            if (cand->is_typed_const &&
                cand->source_unit_index == ctx->symtab->current_unit_index)
            {
                effective_node = cand;
                break;
            }
        }
        if (cands != NULL)
            DestroyList(cands);
    }
    if (effective_node == NULL)
        effective_node = sym_node;
    if (effective_node == NULL)
        effective_node = codegen_find_owner_unit_symbol(ctx, var_id);

    const Tree_t *decl = NULL;
    if (effective_node != NULL &&
        (effective_node->hash_type == HASHTYPE_VAR || effective_node->hash_type == HASHTYPE_ARRAY) &&
        (effective_node->source_unit_index > 0 || effective_node->defined_in_unit))
    {
        decl = codegen_find_var_decl_for_symbol(ctx, effective_node, var_id);
    }
    if (decl == NULL && ctx->symtab->current_unit_index > 0)
        decl = codegen_find_var_decl_for_unit(ctx, ctx->symtab->current_unit_index, var_id);

    const char *global_symbol = codegen_global_access_symbol_for_decl(decl, var_id);
    if (global_symbol == NULL || global_symbol[0] == '\0')
        return NULL;

    /* Typed-consts declared in a unit don't have a bare-name alias (they
     * collide across units — each FPC charmap unit's `unicodemap`), so emit
     * the per-unit static_label retrieved via the qualified storage key. */
    char emit_buf[128];
    const char *emit_label = global_symbol;
    char *qualified = NULL;
    if (decl != NULL &&
        ((decl->type == TREE_VAR_DECL &&
          decl->tree_data.var_decl_data.is_typed_const &&
          decl->tree_data.var_decl_data.defined_in_unit) ||
         (decl->type == TREE_ARR_DECL &&
          decl->tree_data.arr_decl_data.is_typed_const &&
          decl->tree_data.arr_decl_data.defined_in_unit)) &&
        effective_node != NULL && effective_node->source_unit_index > 0)
    {
        qualified = codegen_make_unit_qualified_key(
            effective_node->source_unit_index, var_id);
        if (qualified != NULL)
        {
            StackNode_t *sn = find_label(qualified);
            if (sn != NULL && sn->static_label != NULL)
            {
                snprintf(emit_buf, sizeof(emit_buf), "%s", sn->static_label);
                emit_label = emit_buf;
            }
        }
    }

    char buffer[128];
    *offset = 0;
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
        emit_label, current_non_local_reg64());
    if (qualified != NULL)
        free(qualified);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_try_emit_nonlocal_class_var(ListNode_t *inst_list,
    const char *var_id, CodeGenContext *ctx, int *offset)
{
    if (ctx == NULL || ctx->symtab == NULL ||
        ctx->current_subprogram_owner_class == NULL)
        return NULL;

    const char *class_labels[3] = {0};
    char outer_class_buf[256];
    int class_count = 0;

    class_labels[class_count++] = ctx->current_subprogram_owner_class;
    if (ctx->current_subprogram_owner_class_full != NULL)
    {
        const char *outer = codegen_outer_owner_class_from_full(
            ctx->current_subprogram_owner_class,
            ctx->current_subprogram_owner_class_full,
            outer_class_buf, sizeof(outer_class_buf));
        if (outer != NULL && outer[0] != '\0' &&
            !pascal_identifier_equals(outer, ctx->current_subprogram_owner_class))
        {
            class_labels[class_count++] = outer;
        }
    }

    for (int i = 0; i < class_count; ++i)
    {
        struct RecordType *class_record = semcheck_lookup_record_type(
            ctx->symtab, class_labels[i]);
        const struct RecordType *class_var_owner =
            codegen_record_class_var_owner_named(ctx->symtab, class_record, var_id);
        if (class_var_owner != NULL)
        {
            char buffer[128];
            *offset = 0;
            snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                var_id, current_non_local_reg64());
            return add_inst(inst_list, buffer);
        }
    }

    return NULL;
}

static struct RecordType *codegen_lookup_named_record_type(CodeGenContext *ctx, const char *type_name)
{
    HashNode_t *node = NULL;

    if (ctx == NULL || ctx->symtab == NULL || type_name == NULL)
        return NULL;
    if (FindSymbol(&node, ctx->symtab, (char *)type_name) == 0 || node == NULL)
        return NULL;
    if (node->type == NULL)
        return NULL;

    if (node->type->kind == TYPE_KIND_RECORD)
        return node->type->info.record_info;
    {
        struct TypeAlias *alias = hashnode_get_type_alias(node);
        if (alias != NULL && alias->inline_record_type != NULL)
            return alias->inline_record_type;
    }
    return NULL;
}

static int codegen_call_requires_class_method_vmt_self(const struct Expression *call_expr,
    CodeGenContext *ctx)
{
    const char *owner_class_name = NULL;
    const char *method_name = NULL;
    struct RecordType *owner_record = NULL;
    struct RecordType *check_record = NULL;
    const char *check_class = NULL;

    if (call_expr == NULL || call_expr->type != EXPR_FUNCTION_CALL)
        return 0;

    /* Use cached method identity from semantic checker (preferred). */
    owner_class_name = call_expr->expr_data.function_call_data.cached_owner_class;
    method_name = call_expr->expr_data.function_call_data.cached_method_name;

    /* Fallback: try deprecated resolved_func if cached fields not set. */
    if (owner_class_name == NULL || method_name == NULL)
    {
        HashNode_t *resolved = call_expr->expr_data.function_call_data.resolved_func;
        if (resolved != NULL)
        {
            owner_class_name = resolved->owner_class;
            method_name = resolved->method_name;
        }
    }

    if (owner_class_name != NULL && method_name != NULL)
    {
        owner_record = codegen_lookup_named_record_type(ctx, owner_class_name);
        if (from_cparser_is_method_nonstatic_class_method(owner_class_name, method_name) &&
            (owner_record == NULL || record_type_is_class(owner_record)))
            return 1;
    }

    /* Fall back to self_class_name for inherited/virtual calls. */
    method_name = call_expr->expr_data.function_call_data.id;
    check_class = call_expr->expr_data.function_call_data.self_class_name;
    check_record = codegen_lookup_named_record_type(ctx, check_class);
    while (check_class != NULL && method_name != NULL)
    {
        if (from_cparser_is_method_nonstatic_class_method(check_class, method_name) &&
            (check_record == NULL || record_type_is_class(check_record)))
            return 1;
        if (check_record == NULL || check_record->parent_class_name == NULL)
            break;
        check_class = check_record->parent_class_name;
        check_record = codegen_lookup_named_record_type(ctx, check_class);
    }

    if (call_expr->expr_data.function_call_data.is_class_method_call &&
        owner_class_name != NULL)
    {
        owner_record = codegen_lookup_named_record_type(ctx, owner_class_name);
        if (owner_record != NULL && record_type_is_class(owner_record))
            return 1;
    }

    if (call_expr->expr_data.function_call_data.is_class_method_call &&
        call_expr->expr_data.function_call_data.self_class_name != NULL)
    {
        owner_record = codegen_lookup_named_record_type(ctx,
            call_expr->expr_data.function_call_data.self_class_name);
        if (owner_record != NULL && record_type_is_class(owner_record))
            return 1;
    }

    if (method_name == NULL)
        return 0;
    return 0;
}

static int codegen_current_param_is_ansistring(const struct Expression *expr,
    CodeGenContext *ctx)
{
    if (expr == NULL || expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL ||
        ctx == NULL || ctx->current_subprogram_args == NULL)
    {
        return 0;
    }

    /* Search the enclosing subprogram's formal parameters for one matching
     * the argument expression by name, then check if it has an AnsiString
     * (or compatible) type.  This generalises across all functions instead
     * of being hardcoded to specific method names. */
    for (ListNode_t *cur = ctx->current_subprogram_args; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_VAR_DECL)
            continue;

        int matches = 0;
        for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
             id_node != NULL;
             id_node = id_node->next)
        {
            const char *id = (const char *)id_node->cur;
            if (id != NULL && pascal_identifier_equals(id, expr->expr_data.id))
            {
                matches = 1;
                break;
            }
        }
        if (!matches)
            continue;

        KgpcType *type = decl->tree_data.var_decl_data.cached_kgpc_type;
        if (type != NULL && kgpc_type_equals_tag(type, STRING_TYPE) &&
            !kgpc_type_is_shortstring(type))
        {
            return 1;
        }

        const char *type_id = decl->tree_data.var_decl_data.type_id;
        if (decl->tree_data.var_decl_data.type == STRING_TYPE &&
            (type_id == NULL || !pascal_identifier_equals(type_id, "ShortString")))
        {
            return 1;
        }
        if (type_id != NULL &&
            (pascal_identifier_equals(type_id, "AnsiString") ||
             pascal_identifier_equals(type_id, "RawByteString") ||
             pascal_identifier_equals(type_id, "UnicodeString")))
        {
            return 1;
        }

        return 0;
    }

    return 0;
}

static int codegen_self_param_is_class(Tree_t *formal_arg_decl, CodeGenContext *ctx)
{
    if (formal_arg_decl == NULL || formal_arg_decl->type != TREE_VAR_DECL)
        return 0;

    KgpcType *type = formal_arg_decl->tree_data.var_decl_data.cached_kgpc_type;
    const char *type_id = formal_arg_decl->tree_data.var_decl_data.type_id;
    if (type == NULL && ctx != NULL && ctx->symtab != NULL && type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab, type_id) != 0 &&
            type_node != NULL && type_node->type != NULL)
            type = type_node->type;
    }

    if (type == NULL)
        return 0;

    if (kgpc_type_is_pointer(type) &&
        type->info.points_to != NULL &&
        type->info.points_to->kind == TYPE_KIND_RECORD &&
        type->info.points_to->info.record_info != NULL)
        return record_type_is_class(type->info.points_to->info.record_info);

    if (type->kind == TYPE_KIND_RECORD && type->info.record_info != NULL)
        return record_type_is_class(type->info.record_info);

    return 0;
}

typedef struct ArgInfo
{
    Register_t *reg;
    StackNode_t *spill;
    struct Expression *expr;
    int spill_is_single;
    int spill_is_extended;
    int expected_type;
    int expected_real_size;
    int is_pointer_like;
    int assigned_class;
    int assigned_index;
    int pass_via_stack;
    int stack_slot;
    int stack_size;
    int stack_offset;
    int emitted_via_prepass;
} ArgInfo;

static void arginfo_register_spill_handler(Register_t *reg, StackNode_t *spill_slot, void *context)
{
    ArgInfo *info = (ArgInfo *)context;
    if (info == NULL || spill_slot == NULL)
        return;
    info->reg = NULL;
    info->spill = spill_slot;
    info->spill_is_single = 0;
    info->spill_is_extended = 0;
}

static void arginfo_assign_register(ArgInfo *info, Register_t *reg, struct Expression *expr)
{
    if (info == NULL)
        return;
    info->reg = reg;
    info->spill = NULL;
    info->expr = expr;
    if (reg != NULL)
        register_set_spill_callback(reg, arginfo_register_spill_handler, info);
}

static int kgpc_type_is_non_short_string(KgpcType *type)
{
    if (type == NULL)
        return 0;

    if (kgpc_type_equals_tag(type, STRING_TYPE) &&
        !kgpc_type_is_shortstring(type))
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(type);
        if (alias == NULL || !alias->is_shortstring)
            return 1;
    }

    return 0;
}

static int formal_decl_expects_string(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL)
        return 0;

    if (decl->type != TREE_VAR_DECL)
        return 0;

    if (decl->tree_data.var_decl_data.type == STRING_TYPE)
        return 1;

    KgpcType *cached = decl->tree_data.var_decl_data.cached_kgpc_type;
    if (kgpc_type_is_non_short_string(cached))
        return 1;

    if (decl->tree_data.var_decl_data.type_id != NULL)
    {
        const char *type_id = decl->tree_data.var_decl_data.type_id;
        if (pascal_identifier_equals(type_id, "string") ||
            pascal_identifier_equals(type_id, "ansistring") ||
            pascal_identifier_equals(type_id, "rawbytestring") ||
             pascal_identifier_equals(type_id, "utf8string") ||
             pascal_identifier_equals(type_id, "shortstring"))
            return 1;

        if (symtab != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, type_id) != 0 &&
                type_node != NULL &&
                kgpc_type_is_non_short_string(type_node->type))
                return 1;
        }
    }

    return 0;
}

static int formal_decl_expects_wide_string(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL || decl->type != TREE_VAR_DECL)
        return 0;

    if (decl->tree_data.var_decl_data.type_id != NULL)
    {
        const char *type_id = decl->tree_data.var_decl_data.type_id;
        if (pascal_identifier_equals(type_id, "UnicodeString") ||
            pascal_identifier_equals(type_id, "WideString"))
            return 1;

        if (symtab != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, type_id) != 0 &&
                type_node != NULL && type_node->type != NULL &&
                kgpc_type_is_wide_string(type_node->type))
            {
                return 1;
            }
        }
    }

    return 0;
}

static int builtin_arg_expects_string(const char *procedure_name, int arg_index)
{
    (void)procedure_name;
    (void)arg_index;
    /* Pos/AnsiPos dispatch is fully handled by the semantic checker which
     * selects typed runtime overloads (_ca, _cs, _cc, etc.).  Promoting
     * char arguments to strings here would break those overloads.  No
     * builtin currently needs argument-type promotion in the codegen. */
    return 0;
}

static int mangled_call_expects_char(const struct Expression *call_expr, int arg_index)
{
    if (call_expr == NULL || call_expr->type != EXPR_FUNCTION_CALL)
        return 0;
    const char *mangled = call_expr->expr_data.function_call_data.mangled_id;
    if (mangled == NULL)
        return 0;

    /* Match "kgpc_string_pos_XY" or "kgpc_string_pos_XY_from" where X,Y ∈ {c,a,s} */
    const char prefix[] = "kgpc_string_pos_";
    size_t plen = sizeof(prefix) - 1;
    if (strncmp(mangled, prefix, plen) != 0)
        return 0;
    const char *suffix = mangled + plen;
    /* suffix should be at least 2 chars: type_substr, type_value */
    if (suffix[0] == '\0' || suffix[1] == '\0')
        return 0;
    /* suffix[0] = substr type, suffix[1] = value type */
    char type_char = (arg_index == 0) ? suffix[0] : suffix[1];
    return type_char == 'c';
}

static int codegen_expr_is_wide_string_value(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->type == EXPR_ADDOP &&
        expr->expr_data.addop_data.addop_type == PLUS &&
        expr_get_type_tag(expr) == STRING_TYPE)
    {
        return codegen_expr_is_wide_string_value(expr->expr_data.addop_data.left_expr) ||
               codegen_expr_is_wide_string_value(expr->expr_data.addop_data.right_term);
    }

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

static int codegen_param_expected_type(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL)
        return UNKNOWN_TYPE;

    HashNode_t *type_node = NULL;
    char *type_id = NULL;

    if (decl->type == TREE_VAR_DECL)
    {
        type_id = decl->tree_data.var_decl_data.type_id;
        KgpcType *cached = decl->tree_data.var_decl_data.cached_kgpc_type;
        if (cached != NULL)
        {
            if (kgpc_type_is_shortstring(cached))
                return SHORTSTRING_TYPE;
            struct TypeAlias *alias = kgpc_type_get_type_alias(cached);
            if (alias != NULL && alias->is_shortstring)
                return SHORTSTRING_TYPE;
        }
        if (type_id != NULL && pascal_identifier_equals(type_id, "ShortString"))
            return SHORTSTRING_TYPE;
        if (type_id != NULL && symtab != NULL &&
            FindSymbol(&type_node, symtab, type_id) != 0 && type_node != NULL &&
            type_node->type != NULL)
        {
            if (kgpc_type_is_shortstring(type_node->type))
                return SHORTSTRING_TYPE;
            struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
            if (alias != NULL && alias->is_shortstring)
                return SHORTSTRING_TYPE;
        }
        if (decl->tree_data.var_decl_data.type != UNKNOWN_TYPE)
            return decl->tree_data.var_decl_data.type;
    }
    if (decl->type == TREE_ARR_DECL)
    {
        /* Open array params should not be treated as their element type. */
        return UNKNOWN_TYPE;
    }

    if (type_id != NULL && symtab != NULL &&
        FindSymbol(&type_node, symtab, type_id) != 0 && type_node != NULL &&
        type_node->type != NULL)
    {
        int resolved = codegen_tag_from_kgpc(type_node->type);
        if (resolved != UNKNOWN_TYPE)
            return resolved;
    }

    return UNKNOWN_TYPE;
}

static int codegen_param_real_storage_size(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL)
        return 8;

    if (decl->type == TREE_VAR_DECL)
    {
        if (decl->tree_data.var_decl_data.type == EXTENDED_TYPE)
            return 16;
        if (decl->tree_data.var_decl_data.type_id != NULL)
        {
            const char *type_id = decl->tree_data.var_decl_data.type_id;
            if (pascal_identifier_equals(type_id, "Single"))
                return 4;
            if (pascal_identifier_equals(type_id, "Extended"))
                return 16;
            if (pascal_identifier_equals(type_id, "Double") ||
                pascal_identifier_equals(type_id, "Real"))
                return 8;
        }
        struct TypeAlias *alias = decl->tree_data.var_decl_data.inline_type_alias;
        if (alias != NULL && alias->storage_size > 0)
        {
            return (int)alias->storage_size;
        }
        if (decl->tree_data.var_decl_data.cached_kgpc_type != NULL)
        {
            /* Extended (10-byte long double) is passed via the SysV X87 class
             * which the codegen lowers to a 16-byte stack slot.  Treat any
             * extended-typed parameter (including type aliases like
             * `bestreal = extended;`) as 16 so the caller-side classifier
             * matches the callee's prologue (codegen_real_param_storage_size). */
            if (kgpc_type_is_extended(decl->tree_data.var_decl_data.cached_kgpc_type))
                return 16;
            long long size = kgpc_type_sizeof(decl->tree_data.var_decl_data.cached_kgpc_type);
            if (size > 0)
            {
                return (int)size;
            }
        }
    }

    if (decl->type == TREE_VAR_DECL && decl->tree_data.var_decl_data.type_id != NULL &&
        symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, decl->tree_data.var_decl_data.type_id) != 0 &&
            type_node != NULL && type_node->type != NULL)
        {
            if (kgpc_type_is_extended(type_node->type))
                return 16;
            long long size = kgpc_type_sizeof(type_node->type);
            if (size > 0)
                return (int)size;
        }
    }

    return 8;
}

static int codegen_expr_is_extended_storage_arg(const struct Expression *expr)
{
    KgpcType *expr_type = expr_get_kgpc_type((struct Expression *)expr);
    return expr_type != NULL && kgpc_type_is_extended(expr_type);
}

static ListNode_t *codegen_materialize_extended_arg_spill(ArgInfo *info,
    struct Expression *arg_expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    char buffer[CODEGEN_MAX_INST_BUF];
    StackNode_t *arg_spill = add_l_t_bytes("arg_ext_eval", 10);
    if (info == NULL || arg_expr == NULL || arg_spill == NULL)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to allocate storage for Extended argument evaluation.");
        return inst_list;
    }

    if (codegen_expr_is_extended_storage_arg(arg_expr) && codegen_expr_is_addressable(arg_expr))
    {
        Register_t *src_addr = NULL;
        Register_t *dest_addr = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_addr == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate register for Extended argument destination.");
            return inst_list;
        }

        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            arg_spill->offset, dest_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_addr);
        if (codegen_had_error(ctx) || src_addr == NULL)
        {
            free_reg(get_reg_stack(), dest_addr);
            if (src_addr != NULL)
                free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }

        if (codegen_target_is_windows())
        {
            { Register_t *u[] = {dest_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
            { Register_t *u[] = {src_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
            inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
        }
        else
        {
            { Register_t *u[] = {dest_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n"); }
            { Register_t *u[] = {src_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
            inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
        free_arg_regs();
        free_reg(get_reg_stack(), src_addr);
        free_reg(get_reg_stack(), dest_addr);
    }
    else
    {
        Register_t *dest_addr = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_addr == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate register for Extended argument spill.");
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            arg_spill->offset, dest_addr->bit_64);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_materialize_extended_expr(arg_expr, inst_list, ctx, dest_addr);
        free_reg(get_reg_stack(), dest_addr);
    }

    info->reg = NULL;
    info->spill = arg_spill;
    info->expr = arg_expr;
    info->spill_is_extended = 1;
    info->spill_is_single = 0;
    return inst_list;
}

static int codegen_expr_real_storage_size(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 8;

    if (expr->type == EXPR_RECORD_ACCESS && ctx != NULL)
    {
        long long field_size = codegen_record_field_effective_size((struct Expression *)expr, ctx);
        if (field_size == 4 || field_size == 8 || field_size == 16)
            return (int)field_size;
    }

    KgpcType *type = expr_get_kgpc_type(expr);
    if (type == NULL && ctx != NULL && ctx->symtab != NULL &&
        expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
            node != NULL && node->type != NULL)
            type = node->type;
    }

    if (type != NULL)
    {
        long long size = kgpc_type_sizeof(type);
        if (size == 10)
            return 8;
        if (size > 0)
            return (int)size;
    }

    /* Fallback for record fields/properties where resolved KgpcType may only
     * carry the generic REAL tag while storage is actually Single (4 bytes). */
    if (expr_has_type_tag(expr, REAL_TYPE))
    {
        long long eff_size = expr_effective_size_bytes(expr);
        if (eff_size == 10)
            return 8;
        if (eff_size == 4 || eff_size == 8 || eff_size == 16)
            return (int)eff_size;
    }

    return 8;
}

static int codegen_expected_type_for_builtin(const char *name)
{
    if (name == NULL)
        return UNKNOWN_TYPE;

    if (pascal_identifier_equals(name, "Trunc") ||
        pascal_identifier_equals(name, "Int") ||
        pascal_identifier_equals(name, "Round") ||
        pascal_identifier_equals(name, "Frac") ||
        pascal_identifier_equals(name, "Ln") ||
        pascal_identifier_equals(name, "Exp") ||
        pascal_identifier_equals(name, "Sqrt") ||
        pascal_identifier_equals(name, "Sin") ||
        pascal_identifier_equals(name, "Csc") ||
        pascal_identifier_equals(name, "Sinh") ||
        pascal_identifier_equals(name, "Csch") ||
        pascal_identifier_equals(name, "Cos") ||
        pascal_identifier_equals(name, "Sec") ||
        pascal_identifier_equals(name, "Cosh") ||
        pascal_identifier_equals(name, "Sech") ||
        pascal_identifier_equals(name, "Tan") ||
        pascal_identifier_equals(name, "Cot") ||
        pascal_identifier_equals(name, "Tanh") ||
        pascal_identifier_equals(name, "Coth") ||
        pascal_identifier_equals(name, "ArcSin") ||
        pascal_identifier_equals(name, "ArcCos") ||
        pascal_identifier_equals(name, "ArcCosh") ||
        pascal_identifier_equals(name, "ArcSech") ||
        pascal_identifier_equals(name, "ArcCsch") ||
        pascal_identifier_equals(name, "ArcTan2") ||
        pascal_identifier_equals(name, "Hypot") ||
        pascal_identifier_equals(name, "ArcSinh") ||
        pascal_identifier_equals(name, "ArcTanh") ||
        pascal_identifier_equals(name, "ArcCot") ||
        pascal_identifier_equals(name, "ArcCoth") ||
        pascal_identifier_equals(name, "ArcTan") ||
        pascal_identifier_equals(name, "DegToRad") ||
        pascal_identifier_equals(name, "RadToDeg") ||
        pascal_identifier_equals(name, "DegToGrad") ||
        pascal_identifier_equals(name, "GradToDeg") ||
        pascal_identifier_equals(name, "GradToRad") ||
        pascal_identifier_equals(name, "RadToGrad") ||
        pascal_identifier_equals(name, "CycleToRad") ||
        pascal_identifier_equals(name, "RadToCycle") ||
        pascal_identifier_equals(name, "Ln") ||
        pascal_identifier_equals(name, "Log10") ||
        pascal_identifier_equals(name, "Log2") ||
        pascal_identifier_equals(name, "LogN") ||
        pascal_identifier_equals(name, "Exp"))
    {
        return REAL_TYPE;
    }

    if (pascal_identifier_equals(name, "Random"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(name, "RandomRange"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(name, "Power"))
        return REAL_TYPE;
    if (pascal_identifier_equals(name, "Ceil") ||
        pascal_identifier_equals(name, "Floor"))
        return REAL_TYPE;
    if (pascal_identifier_equals(name, "FloatToStr") ||
        pascal_identifier_equals(name, "floattostr_r"))
        return REAL_TYPE;

    return UNKNOWN_TYPE;
}

static int formal_decl_is_open_array(Tree_t *decl)
{
    if (decl == NULL || decl->type != TREE_ARR_DECL)
        return 0;

    struct Array *arr = &decl->tree_data.arr_decl_data;
    return (arr->e_range < arr->s_range);
}

static int formal_decl_is_char_set(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL || decl->type != TREE_VAR_DECL)
        return 0;

    struct TypeAlias *alias = decl->tree_data.var_decl_data.inline_type_alias;
    if (alias != NULL && alias->is_set)
    {
        if (alias->set_element_type == CHAR_TYPE ||
            alias->set_element_type == BYTE_TYPE ||
            (alias->set_element_type_id != NULL &&
             (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
              pascal_identifier_equals(alias->set_element_type_id, "AnsiChar") ||
              pascal_identifier_equals(alias->set_element_type_id, "Byte"))))
            return 1;
    }

    if (decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, decl->tree_data.var_decl_data.type_id) != 0 &&
            type_node != NULL && type_node->type != NULL)
        {
            if (type_node->type->kind == TYPE_KIND_PRIMITIVE &&
                kgpc_type_get_primitive_tag(type_node->type) == SET_TYPE &&
                kgpc_type_sizeof(type_node->type) > 4)
                return 1;
        }
    }

    return 0;
}

static int codegen_formal_shortstring_size(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL)
        return 256;

    if (decl->type == TREE_ARR_DECL)
    {
        struct Array *arr = &decl->tree_data.arr_decl_data;
        if (arr->is_shortstring && arr->e_range >= arr->s_range && arr->e_range >= 0)
        {
            int size = arr->e_range - arr->s_range + 1;
            /* A plain 'ShortString' type is 256 bytes (array[0..255] of Char).
             * If the bounds indicate a very small size (e.g. e_range=0 from
             * uninitialized/default values), use the standard 256. */
            if (size < 2)
                return 256;
            return size;
        }
    }

    if (decl->type == TREE_VAR_DECL)
    {
        struct TypeAlias *alias = decl->tree_data.var_decl_data.inline_type_alias;
        if (alias != NULL && alias->is_shortstring &&
            alias->array_end >= alias->array_start && alias->array_end >= 0)
        {
            int size = alias->array_end - alias->array_start + 1;
            if (size >= 2) return size;
            return 256;
        }

        KgpcType *cached = decl->tree_data.var_decl_data.cached_kgpc_type;
        if (cached != NULL)
        {
            /* Plain ShortString (not a bounded String[N]) is always 256 bytes */
            if (kgpc_type_is_shortstring(cached))
                return 256;
            struct TypeAlias *cached_alias = kgpc_type_get_type_alias(cached);
            if (cached_alias != NULL && cached_alias->is_shortstring &&
                cached_alias->array_end >= cached_alias->array_start && cached_alias->array_end >= 0)
            {
                int size = cached_alias->array_end - cached_alias->array_start + 1;
                if (size >= 2) return size;
                return 256;
            }
            if (kgpc_type_is_array(cached))
            {
                int start = 0;
                int end = -1;
                if (kgpc_type_get_array_bounds(cached, &start, &end) == 0 &&
                    end >= start && end >= 0)
                    return end - start + 1;
            }
        }

        if (decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, decl->tree_data.var_decl_data.type_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                /* If the type IS ShortString, return 256 immediately */
                if (kgpc_type_is_shortstring(type_node->type))
                    return 256;
                struct TypeAlias *type_alias = kgpc_type_get_type_alias(type_node->type);
                if (type_alias != NULL && type_alias->is_shortstring &&
                    type_alias->array_end >= type_alias->array_start && type_alias->array_end >= 0)
                    return type_alias->array_end - type_alias->array_start + 1;
                if (kgpc_type_is_array(type_node->type))
                {
                    int start = 0;
                    int end = -1;
                    if (kgpc_type_get_array_bounds(type_node->type, &start, &end) == 0 &&
                        end >= start && end >= 0)
                        return end - start + 1;
                }
            }
        }
    }

    return 256;
}

static long long codegen_static_array_length(const struct Expression *expr)
{
    if (expr == NULL || !expr->is_array_expr || expr->array_is_dynamic)
        return -1;

    long long lower = expr->array_lower_bound;
    long long upper = expr->array_upper_bound;
    if (upper < lower)
        return -1;

    return (upper - lower) + 1;
}

static inline RegisterId_t codegen_arg_reg_id_num(int num)
{
    static const RegisterId_t windows_regs[] = { REG_RCX, REG_RDX, REG_R8, REG_R9 };
    static const RegisterId_t sysv_regs[] = { REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9 };
    const RegisterId_t *regs = (g_current_codegen_abi == KGPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    int limit = kgpc_max_int_arg_regs();
    if (num < 0 || num >= limit)
        return REG_INVALID;
    return regs[num];
}

static int codegen_formal_is_dynamic_array(Tree_t *formal, SymTab_t *symtab)
{
    if (formal == NULL || formal->type != TREE_VAR_DECL)
        return 0;

    KgpcType *cached = formal->tree_data.var_decl_data.cached_kgpc_type;
    if (cached != NULL && cached->kind == TYPE_KIND_ARRAY &&
        kgpc_type_is_dynamic_array(cached))
    {
        return 1;
    }

    if (symtab != NULL && formal->tree_data.var_decl_data.type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, formal->tree_data.var_decl_data.type_id) != 0 &&
            type_node != NULL && type_node->type != NULL &&
            type_node->type->kind == TYPE_KIND_ARRAY &&
            kgpc_type_is_dynamic_array(type_node->type))
        {
            return 1;
        }
    }

    return 0;
}

static int codegen_expr_is_open_array_descriptor_arg(const struct Expression *expr,
    CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;

    if (expr->is_array_expr && expr->array_is_dynamic)
        return 1;

    KgpcType *arg_type = expr_get_kgpc_type((struct Expression *)expr);
    if (arg_type != NULL && kgpc_type_is_dynamic_array(arg_type))
        return 1;

    if (ctx == NULL || ctx->symtab == NULL ||
        expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
        return 0;

    HashNode_t *symbol = NULL;
    if (FindSymbol(&symbol, ctx->symtab, expr->expr_data.id) == 0 ||
        symbol == NULL || symbol->type == NULL)
        return 0;

    return kgpc_type_is_dynamic_array(symbol->type);
}

ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset,
    CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    CODEGEN_DEBUG("DEBUG: Generating non-local access for %s\n", var_id);

    assert(var_id != NULL);
    assert(offset != NULL);

    char buffer[128];
    int scope_depth = 0;
    HashNode_t *sym_node = NULL;
    StackNode_t *var = codegen_find_nonlocal_lexical(ctx, var_id, &scope_depth, &sym_node);

    if(var == NULL) {
        ListNode_t *resolved = codegen_try_emit_nonlocal_global(inst_list, var_id, ctx,
            sym_node, offset);
        if (resolved != NULL)
            return resolved;

        resolved = codegen_try_emit_nonlocal_class_var(inst_list, var_id, ctx, offset);
        if (resolved != NULL)
            return resolved;

        /* If we're inside a nonstatic class method, the bare name may be a field
         * of the owning class that semcheck didn't rewrite to Self.field (e.g.
         * because the method body came from an AST cache and bypassed semcheck).
         * Resolve it as Self + field_offset. */
        if (ctx != NULL && ctx->current_subprogram_owner_class != NULL &&
            ctx->symtab != NULL)
        {
            StackNode_t *self_slot = find_label("Self");
            if (self_slot != NULL)
            {
                struct RecordType *class_record = semcheck_lookup_record_type(
                    ctx->symtab, ctx->current_subprogram_owner_class);
                /* If short name lookup fails, try the full dotted class path
                 * (e.g. "HeapInc.ThreadState" for nested class types). */
                if (class_record == NULL &&
                    ctx->current_subprogram_owner_class_full != NULL)
                {
                    class_record = semcheck_lookup_record_type(ctx->symtab,
                        ctx->current_subprogram_owner_class_full);
                }
                if (class_record != NULL)
                {
                    struct RecordField *field_desc = NULL;
                    long long field_offset = 0;
                    if (resolve_record_field(ctx->symtab, class_record, var_id,
                            &field_desc, &field_offset, 0, 1) == 0 &&
                        field_desc != NULL)
                    {
                        *offset = 0;
                        if (codegen_record_has_class_var_named(class_record, var_id) ||
                            (ctx->current_subprogram_is_nonstatic_class_method &&
                             codegen_record_matches_owner_class(ctx, class_record)))
                        {
                            inst_list = codegen_emit_classvar_base_address_named(inst_list,
                                current_non_local_reg64(), class_record, field_offset);
                        }
                        else
                        {
                            /* Load Self pointer, then add field offset. */
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t-%d(%%rbp), %s\n",
                                self_slot->offset, current_non_local_reg64());
                            inst_list = add_inst(inst_list, buffer);
                            if (field_offset != 0)
                            {
                                snprintf(buffer, sizeof(buffer),
                                    "\taddq\t$%lld, %s\n",
                                    field_offset, current_non_local_reg64());
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }
                        return inst_list;
                    }

                    /* Check if the bare name is a method of the owning class.
                     * This handles @MethodName references in cached method bodies. */
                    const char *method_label = NULL;
                    char method_label_buf[256];
                    method_label_buf[0] = '\0';
                    for (ListNode_t *mn = class_record->methods; mn != NULL; mn = mn->next)
                    {
                        if (mn->cur == NULL)
                            continue;
                        struct MethodInfo *mi = (struct MethodInfo *)mn->cur;
                        if (mi->name != NULL && pascal_identifier_equals(mi->name, var_id))
                        {
                            if (mi->resolved_mangled_id != NULL)
                                method_label = mi->resolved_mangled_id;
                            else if (mi->mangled_name != NULL)
                                method_label = mi->mangled_name;
                            break;
                        }
                    }
                    /* Also search parent classes for inherited methods. */
                    if (method_label == NULL)
                    {
                        struct RecordType *search_record = class_record;
                        while (method_label == NULL && search_record != NULL &&
                               search_record->parent_class_name != NULL)
                        {
                            search_record = semcheck_lookup_record_type(ctx->symtab,
                                search_record->parent_class_name);
                            if (search_record == NULL)
                                break;
                            for (ListNode_t *mn = search_record->methods; mn != NULL; mn = mn->next)
                            {
                                if (mn->cur == NULL)
                                    continue;
                                struct MethodInfo *mi = (struct MethodInfo *)mn->cur;
                                if (mi->name != NULL && pascal_identifier_equals(mi->name, var_id))
                                {
                                    if (mi->resolved_mangled_id != NULL)
                                        method_label = mi->resolved_mangled_id;
                                    else if (mi->mangled_name != NULL)
                                        method_label = mi->mangled_name;
                                    break;
                                }
                            }
                        }
                    }
                    /* If not found in MethodInfo lists, try the symbol table.
                     * Methods may be registered under "ClassName.MethodName"
                     * or "ClassName__MethodName" (mangled form). */
                    if (method_label == NULL)
                    {
                        char qualified[256];
                        snprintf(qualified, sizeof(qualified), "%s.%s",
                            ctx->current_subprogram_owner_class, var_id);
                        HashNode_t *method_node = NULL;
                        if (FindSymbol(&method_node, ctx->symtab, qualified) != 0 &&
                            method_node != NULL &&
                            (method_node->hash_type == HASHTYPE_PROCEDURE ||
                             method_node->hash_type == HASHTYPE_FUNCTION))
                        {
                            if (method_node->mangled_id != NULL)
                                method_label = method_node->mangled_id;
                            else
                            {
                                strncpy(method_label_buf, qualified,
                                    sizeof(method_label_buf) - 1);
                                method_label_buf[sizeof(method_label_buf) - 1] = '\0';
                                method_label = method_label_buf;
                            }
                        }
                    }
                    if (method_label == NULL)
                    {
                        char qualified[256];
                        snprintf(qualified, sizeof(qualified), "%s__%s",
                            ctx->current_subprogram_owner_class, var_id);
                        ListNode_t *candidates = FindAllIdents(ctx->symtab, qualified);
                        for (ListNode_t *c = candidates; c != NULL; c = c->next) {
                            HashNode_t *cand = (HashNode_t *)c->cur;
                            if (cand != NULL && cand->mangled_id != NULL &&
                                cand->type != NULL &&
                                cand->type->kind == TYPE_KIND_PROCEDURE) {
                                method_label = cand->mangled_id;
                                break;
                            }
                        }
                        if (candidates != NULL) DestroyList(candidates);
                    }
                    if (method_label != NULL)
                    {
                        *offset = 0;
                        {
                            char method_buffer[384];
                            snprintf(method_buffer, sizeof(method_buffer),
                                "\tleaq\t%s(%%rip), %s\n",
                                method_label, current_non_local_reg64());
                            inst_list = add_inst(inst_list, method_buffer);
                        }
                        return inst_list;
                    }
                }
            }
        }

        /* Bare name inside a WITH block that semcheck did not rewrite to a
         * record access. Resolve it against the active WITH context stack. */
        if (ctx != NULL && ctx->symtab != NULL && ctx->with_depth > 0)
        {
            for (int i = ctx->with_depth; i > 0; --i)
            {
                struct RecordType *with_record = ctx->with_stack[i - 1].record_type;
                struct Expression *with_expr = ctx->with_stack[i - 1].context_expr;
                struct RecordField *field_desc = NULL;
                long long field_offset = 0;
                if (with_record == NULL || with_expr == NULL)
                    continue;
                if (resolve_record_field(ctx->symtab, with_record, var_id,
                        &field_desc, &field_offset, 0, 1) != 0 ||
                    field_desc == NULL)
                    continue;

                Register_t *addr_reg = NULL;
                if (with_record->type_id != NULL &&
                    (codegen_record_has_class_var_named(with_record, var_id) ||
                     codegen_nonstatic_class_method_owner_field_uses_classvar(
                         ctx, with_record, with_expr)))
                {
                    addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                        addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                        return inst_list;
                    inst_list = codegen_emit_classvar_base_address_named(inst_list,
                        addr_reg->bit_64, with_record, field_offset);
                }
                else
                {
                    inst_list = codegen_address_for_expr(with_expr, inst_list, ctx, &addr_reg);
                    if (addr_reg == NULL)
                        return inst_list;

                    if (record_type_is_class(with_record))
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                            addr_reg->bit_64, addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    if (field_offset != 0)
                    {
                        snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n",
                            field_offset, addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }

                *offset = 0;
                if (strcmp(addr_reg->bit_64, current_non_local_reg64()) != 0)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                        addr_reg->bit_64, current_non_local_reg64());
                    inst_list = add_inst(inst_list, buffer);
                }
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }
        }

        codegen_report_error(ctx,
            "ERROR: Unresolved non-local symbol %s reached codegen fallback.",
            var_id);
        /* When populating the codegen cache, all unit functions are emitted.
         * Some have unresolvable non-locals (e.g. local consts in functions
         * that DCE normally removes).  Don't abort — the per-function
         * memstream buffering will catch the error and emit a ud2 stub. */
        *offset = 0;
        return inst_list;
    }

    *offset = var->offset;
    if (var->is_static)
    {
        const char *label = (var->static_label != NULL) ? var->static_label : var->label;
        *offset = 0;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label,
            current_non_local_reg64());
        inst_list = add_inst(inst_list, buffer);
    }
    else if (scope_depth <= 0)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rbp, %s\n", current_non_local_reg64());
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        Register_t *frame_reg = codegen_acquire_static_link(ctx, &inst_list, scope_depth);
        if (frame_reg != NULL)
        {
            if (strcmp(frame_reg->bit_64, current_non_local_reg64()) != 0)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                    frame_reg->bit_64, current_non_local_reg64());
                inst_list = add_inst(inst_list, buffer);
            }
        }
        else
        {
            codegen_report_error(ctx,
                "ERROR: Failed to acquire static link for non-local variable %s.",
                var_id);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-8(%%rbp), %s\n",
                current_non_local_reg64());
            inst_list = add_inst(inst_list, buffer);
        }
    }

    CODEGEN_DEBUG("DEBUG: Non-local access generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list,
    CodeGenContext *ctx, struct KgpcType *proc_type, const char *procedure_name,
    int arg_start_index, const struct Expression *call_expr,
    int is_class_method_call_hint)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int arg_num;
    Register_t *top_reg;
    char buffer[128];
    const char *arg_reg_char;
    expr_node_t *expr_tree;

    assert(ctx != NULL);

    ListNode_t *formal_args = NULL;
    if(proc_type != NULL && proc_type->kind == TYPE_KIND_PROCEDURE)
    {
        /* Get formal parameters from the KgpcType.
         * This avoids use-after-free bugs by not relying on HashNode pointers
         * that may point to freed memory after leaving a semantic scope. */
        formal_args = proc_type->info.proc_info.params;
        CODEGEN_DEBUG("DEBUG: Using formal_args from KgpcType: %p\n", formal_args);
    }
    else if (procedure_name != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *proc_node = NULL;
        if (FindSymbol(&proc_node, ctx->symtab, procedure_name) != 0 &&
            proc_node != NULL && proc_node->type != NULL &&
            proc_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            formal_args = proc_node->type->info.proc_info.params;
            CODEGEN_DEBUG("DEBUG: Using formal_args from symtab fallback: %p\n", formal_args);
        }
    }
    int skip_formal_for_self = 0;
    if (formal_args != NULL)
    {
        int formal_count = ListLength(formal_args);
        int actual_count = ListLength(args);
        if (formal_count == actual_count + 1)
        {
            Tree_t *first_decl = (Tree_t *)formal_args->cur;
            if (first_decl != NULL && first_decl->type == TREE_VAR_DECL)
            {
                ListNode_t *ids = first_decl->tree_data.var_decl_data.ids;
                const char *first_id = (ids != NULL) ? (const char *)ids->cur : NULL;
                if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
                    formal_args = formal_args->next;
            }
        }
        else if (formal_count + 1 == actual_count && args != NULL)
        {
            struct Expression *first_arg = (struct Expression *)args->cur;
            if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
                first_arg->expr_data.id != NULL &&
                pascal_identifier_equals(first_arg->expr_data.id, "Self"))
            {
                /* Actual args include implicit Self but formal params omit it. */
                skip_formal_for_self = 1;
            }
        }
    }
    
    /* CRITICAL VALIDATION: Ensure formal_args is either NULL or properly structured.
     * This catches any remaining cases of corrupted list pointers. */
    if (formal_args != NULL)
    {
        /* Basic sanity check: formal_args should have a valid list type.
         * This catches cases where formal_args contains garbage data. */
        if (formal_args->type != LIST_TREE && formal_args->type != LIST_UNSPECIFIED)
        {
            codegen_report_error(ctx,
                "FATAL: Internal compiler error - corrupted formal_args list (invalid type %d). "
                "This may indicate a bug in the semantic checker or memory corruption.",
                formal_args->type);
            return inst_list;
        }
    }

    enum {
        ARG_CLASS_INT = 0,
        ARG_CLASS_SSE = 1
    };


    int total_args = 0;
    for (ListNode_t *cur = args; cur != NULL; cur = cur->next)
        ++total_args;

    ArgInfo *arg_infos = NULL;
    const int max_int_regs = kgpc_max_int_arg_regs();
    const int max_sse_regs = kgpc_max_sse_arg_regs();
    int stack_slot_count = 0;
    int is_external_c_function = 0;
    if (total_args > 0)
    {
        arg_infos = (ArgInfo *)calloc((size_t)total_args, sizeof(ArgInfo));
        if (arg_infos == NULL)
        {
            fprintf(stderr, "ERROR: Failed to allocate argument metadata.\n");
            exit(1);
        }
    }

    if (ctx != NULL)
        ctx->pending_stack_arg_bytes = 0;

    if (arg_start_index < 0)
        arg_start_index = 0;

    int is_varargs_function = 0;
    if (proc_type != NULL && proc_type->kind == TYPE_KIND_PROCEDURE &&
        proc_type->info.proc_info.definition != NULL)
    {
        Tree_t *def = proc_type->info.proc_info.definition;
        if (def->type == TREE_SUBPROGRAM || def->type == TREE_SUBPROGRAM_PROC || def->type == TREE_SUBPROGRAM_FUNC)
        {
            is_external_c_function = def->tree_data.subprogram_data.cname_flag;
            is_varargs_function = def->tree_data.subprogram_data.is_varargs;
        }
    }
    else if (procedure_name != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *proc_node = NULL;
        if (FindSymbol(&proc_node, ctx->symtab, procedure_name) != 0 &&
            proc_node != NULL && proc_node->type != NULL &&
            proc_node->type->kind == TYPE_KIND_PROCEDURE &&
            proc_node->type->info.proc_info.definition != NULL)
        {
            Tree_t *def = proc_node->type->info.proc_info.definition;
            if (def->type == TREE_SUBPROGRAM || def->type == TREE_SUBPROGRAM_PROC || def->type == TREE_SUBPROGRAM_FUNC)
            {
                is_external_c_function = def->tree_data.subprogram_data.cname_flag;
                is_varargs_function = def->tree_data.subprogram_data.is_varargs;
            }
        }
    }

    arg_num = 0;
    while(args != NULL)
    {
        CODEGEN_DEBUG("DEBUG: In codegen_pass_arguments loop, arg_num = %d\n", arg_num);
        struct Expression *arg_expr = (struct Expression *)args->cur;
        
        /* Validate argument expression */
        if (arg_expr == NULL)
        {
            const char *proc_name = procedure_name ? procedure_name : "(unknown)";
            codegen_report_error(ctx,
                "ERROR: NULL argument expression in call to %s at argument position %d",
                proc_name, arg_num);
            if (arg_infos != NULL)
                free(arg_infos);
            return inst_list;
        }
        
        CODEGEN_DEBUG("DEBUG: arg_expr at %p, type %d\n", arg_expr, arg_expr->type);
        Tree_t *formal_arg_decl = NULL;
        if(formal_args != NULL && !(skip_formal_for_self && arg_num == 0))
        {
            /* CRITICAL VALIDATION: Before dereferencing formal_args, verify it's not corrupted.
             * On Cygwin/MSYS, corrupted list nodes can cause segfaults when accessing ->cur.
             * We check the list type to detect garbage values early. */
            if (formal_args->type != LIST_TREE && formal_args->type != LIST_UNSPECIFIED)
            {
                const char *proc_name = "(unknown)";
                codegen_report_error(ctx,
                    "FATAL: Internal compiler error - corrupted formal_args list node (type=%d) at argument %d for procedure %s. "
                    "This indicates memory corruption or an improperly initialized list.",
                    formal_args->type, arg_num, proc_name);
                if (arg_infos != NULL)
                    free(arg_infos);
                return inst_list;
            }
            formal_arg_decl = (Tree_t *)formal_args->cur;
        }
        int is_self_param = 0;
        if (formal_arg_decl != NULL && formal_arg_decl->type == TREE_VAR_DECL)
        {
            ListNode_t *ids = formal_arg_decl->tree_data.var_decl_data.ids;
            const char *formal_id = (ids != NULL) ? (const char *)ids->cur : NULL;
            if (formal_id != NULL && pascal_identifier_equals(formal_id, "Self"))
                is_self_param = 1;
        }
        /* Also detect Self parameter when the argument expression IS the Self variable */
        if (!is_self_param && arg_num == 0 && arg_expr != NULL &&
            arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL &&
            pascal_identifier_equals(arg_expr->expr_data.id, "Self"))
        {
            is_self_param = 1;
        }

        int is_var_param = 0;
        if (formal_arg_decl != NULL && formal_arg_decl->type == TREE_VAR_DECL)
        {
            is_var_param =
                (formal_arg_decl->tree_data.var_decl_data.is_var_param ||
                 formal_arg_decl->tree_data.var_decl_data.is_untyped_param);

        }
        if (is_self_param && codegen_self_param_is_class(formal_arg_decl, ctx))
            is_var_param = 0;
        int is_array_param = (formal_arg_decl != NULL && formal_arg_decl->type == TREE_ARR_DECL);
        int formal_is_open_array = 0;
        int formal_is_char_set = 0;
        int formal_is_dynarray = 0;
        if (formal_arg_decl != NULL)
        {
            formal_is_open_array = formal_decl_is_open_array(formal_arg_decl);
            formal_is_char_set = formal_decl_is_char_set(formal_arg_decl, ctx->symtab);
            formal_is_dynarray = codegen_formal_is_dynamic_array(formal_arg_decl, ctx->symtab);
        }
        if (!formal_is_dynarray && arg_num == 0 && call_expr != NULL &&
            call_expr->type == EXPR_FUNCTION_CALL &&
            call_expr->expr_data.function_call_data.arg0_is_dynarray_descriptor)
        {
            formal_is_dynarray = 1;
        }
        
        /* Also check if we're passing a static array argument (even if not declared as var param) */
        int is_array_arg = (arg_expr != NULL && arg_expr->is_array_expr && !arg_expr->array_is_dynamic);
        if (!is_array_arg && arg_expr != NULL)
        {
            KgpcType *arg_type = expr_get_kgpc_type(arg_expr);
            if (arg_type != NULL)
            {
                struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type);
                if (kgpc_type_is_shortstring(arg_type) ||
                    (alias != NULL && alias->is_shortstring))
                {
                    is_array_arg = 1;
                }
            }
        }
        int expected_type = codegen_param_expected_type(formal_arg_decl, ctx->symtab);
        if (expected_type == UNKNOWN_TYPE && procedure_name != NULL)
            expected_type = codegen_expected_type_for_builtin(procedure_name);
        const char *call_mangled = NULL;
        if (call_expr != NULL && call_expr->type == EXPR_FUNCTION_CALL)
            call_mangled = call_expr->expr_data.function_call_data.mangled_id;
        /* Structural ShortString promotion for class-method params:
         * when the formal's cached_kgpc_type isn't ShortString-aware but
         * the parameter declaration's type tag IS SHORTSTRING_TYPE (e.g.
         * `s: string` under {$H-} which the parser already mapped to
         * SHORTSTRING_TYPE), force expected_type = SHORTSTRING_TYPE so
         * downstream argument adaptation uses ShortString semantics. */
        if (expected_type == UNKNOWN_TYPE && formal_arg_decl != NULL &&
            formal_arg_decl->type == TREE_VAR_DECL &&
            formal_arg_decl->tree_data.var_decl_data.type == SHORTSTRING_TYPE)
        {
            expected_type = SHORTSTRING_TYPE;
        }
        if (formal_arg_decl != NULL &&
            formal_arg_decl->type == TREE_VAR_DECL &&
            formal_arg_decl->tree_data.var_decl_data.is_untyped_param &&
            arg_expr != NULL &&
            arg_expr->type != EXPR_POINTER_DEREF)
        {
            KgpcType *arg_type = expr_get_kgpc_type(arg_expr);
            int arg_is_pointer = (expr_has_type_tag(arg_expr, POINTER_TYPE) ||
                (arg_type != NULL && kgpc_type_is_pointer(arg_type)));
            int pass_pointer_value = 0;
            if (arg_is_pointer)
            {
                if (formal_arg_decl->tree_data.var_decl_data.is_const_param)
                    pass_pointer_value = 1;
                else if ((procedure_name != NULL &&
                          pascal_identifier_equals(procedure_name, "FpRead")) ||
                         (call_mangled != NULL &&
                          strncmp(call_mangled, "fpread_", 7) == 0))
                    pass_pointer_value = 1;
            }
            if (pass_pointer_value)
                is_var_param = 0;
        }
        if ((procedure_name != NULL && strcmp(procedure_name, "kgpc_trunc_currency") == 0) ||
            (call_mangled != NULL && strcmp(call_mangled, "kgpc_trunc_currency") == 0))
            expected_type = INT64_TYPE;
        /* Sqr / Abs: the semcheck rewrites these builtins to call
         * kgpc_sqr_int32/int64/real or kgpc_abs_int/longint/real based on the
         * argument type.  When the FPC RTL system unit is loaded,
         * FindSymbol("Sqr"/"Abs") may resolve to the ValReal overload, causing
         * codegen_param_expected_type to return REAL_TYPE even for integer
         * arguments.  Override expected_type from the mangled call target. */
        if (call_mangled != NULL)
        {
            if (strcmp(call_mangled, "kgpc_sqr_int32") == 0 ||
                strcmp(call_mangled, "kgpc_abs_int") == 0)
                expected_type = INT_TYPE;
            else if (strcmp(call_mangled, "kgpc_sqr_int64") == 0 ||
                     strcmp(call_mangled, "kgpc_abs_longint") == 0)
                expected_type = INT64_TYPE;
            else if (strcmp(call_mangled, "kgpc_sqr_real") == 0 ||
                     strcmp(call_mangled, "kgpc_abs_real") == 0)
                expected_type = REAL_TYPE;
            else if (strcmp(call_mangled, "kgpc_abs_unsigned") == 0)
                expected_type = LONGINT_TYPE;
        }
        if (expected_type == UNKNOWN_TYPE && procedure_name != NULL &&
            pascal_identifier_equals(procedure_name, "Sqr"))
        {
            int arg_type = expr_get_type_tag(arg_expr);
            if (arg_type == REAL_TYPE)
                expected_type = REAL_TYPE;
            else if (arg_type == LONGINT_TYPE)
                expected_type = LONGINT_TYPE;
            else if (arg_type == INT_TYPE)
                expected_type = INT_TYPE;
        }
        if (procedure_name != NULL &&
            pascal_identifier_equals(procedure_name, "Random"))
        {
            int arg_type = expr_get_type_tag(arg_expr);
            if (arg_type == REAL_TYPE)
                expected_type = REAL_TYPE;
            else if (arg_type == LONGINT_TYPE)
                expected_type = LONGINT_TYPE;
            else if (arg_type == INT_TYPE)
                expected_type = INT_TYPE;
            else if (expected_type == UNKNOWN_TYPE)
                expected_type = LONGINT_TYPE;
        }
        /* For type helper Self parameters passed by value, infer type from expression.
         * This ensures floating-point Self parameters use xmm registers. */
        if (expected_type == UNKNOWN_TYPE && is_self_param && arg_expr != NULL)
        {
            int arg_type = expr_get_type_tag(arg_expr);
            if (arg_type == REAL_TYPE)
                expected_type = REAL_TYPE;
        }
        if (expected_type == UNKNOWN_TYPE && arg_expr != NULL &&
            arg_expr->type == EXPR_VAR_ID && arg_expr->expr_data.id != NULL &&
            ctx != NULL && ctx->symtab != NULL &&
            !formal_is_open_array && !is_array_param)
        {
            HashNode_t *arg_node = NULL;
            if (FindSymbol(&arg_node, ctx->symtab, arg_expr->expr_data.id) != 0 &&
                arg_node != NULL && arg_node->type != NULL)
            {
                int resolved = codegen_tag_from_kgpc(arg_node->type);
                if (resolved != UNKNOWN_TYPE)
                    expected_type = resolved;
            }
        }
        /* Self for type helpers over real types is passed by value (not by reference). */
        if (is_self_param && expected_type == REAL_TYPE)
            is_var_param = 0;
        /* If the actual argument is REAL, pass it via SSE even when formal metadata is missing. */
        if (arg_expr != NULL &&
            !((procedure_name != NULL && strcmp(procedure_name, "kgpc_trunc_currency") == 0) ||
              (call_mangled != NULL && strcmp(call_mangled, "kgpc_trunc_currency") == 0)))
        {
            int arg_type = expr_get_type_tag(arg_expr);
            if (arg_type == REAL_TYPE || arg_expr->type == EXPR_RNUM ||
                (arg_expr->type == EXPR_TYPECAST &&
                 arg_expr->expr_data.typecast_data.target_type == REAL_TYPE))
                expected_type = REAL_TYPE;
        }
        if (is_var_param && arg_num == 0 && arg_expr != NULL &&
            !codegen_expr_is_addressable(arg_expr))
        {
            const char *formal_id = NULL;
            if (formal_arg_decl != NULL && formal_arg_decl->type == TREE_VAR_DECL)
            {
                ListNode_t *ids = formal_arg_decl->tree_data.var_decl_data.ids;
                formal_id = (ids != NULL) ? (const char *)ids->cur : NULL;
            }
            if (formal_id != NULL && pascal_identifier_equals(formal_id, "Self"))
            {
                is_var_param = 0;
            }
        }

        /* Windows runtime expects text/file parameters to be passed by reference.
         * When formal parameter metadata is missing, infer by ref semantics from the
         * argument/expected type to avoid passing the file record by value. */
        if (!is_var_param)
        {
            int arg_type_tag = expr_get_type_tag(arg_expr);
            if (expected_type == FILE_TYPE || expected_type == TEXT_TYPE ||
                arg_type_tag == FILE_TYPE || arg_type_tag == TEXT_TYPE)
            {
                is_var_param = 1;
            }
        }

        if (is_array_arg && arg_expr != NULL && arg_expr->type == EXPR_STRING &&
            expected_type == POINTER_TYPE &&
            !is_array_param && !formal_is_open_array && !formal_is_dynarray)
        {
            /* String literals passed to pointer parameters (e.g. setlocale(..., ''))
             * are by-value pointer arguments, not by-ref static array arguments. */
            is_array_arg = 0;
        }
        int is_pointer_like = (is_var_param || is_array_param || is_array_arg || formal_is_dynarray);
        if (is_self_param && expected_type == REAL_TYPE)
            is_pointer_like = 0;

        if (arg_infos != NULL)
        {
            arg_infos[arg_num].expected_type = expected_type;
            arg_infos[arg_num].expected_real_size = 0;
            arg_infos[arg_num].is_pointer_like = is_pointer_like;
            arg_infos[arg_num].assigned_class = ARG_CLASS_INT;
            arg_infos[arg_num].assigned_index = -1;
            arg_infos[arg_num].spill_is_single = 0;
            arg_infos[arg_num].spill_is_extended = 0;
            arg_infos[arg_num].stack_size = CODEGEN_POINTER_SIZE_BYTES;
        }
        if (arg_infos != NULL && expected_type == REAL_TYPE)
            arg_infos[arg_num].expected_real_size =
                codegen_param_real_storage_size(formal_arg_decl, ctx->symtab);
        if (!is_var_param && formal_arg_decl != NULL &&
            formal_arg_decl->type == TREE_VAR_DECL &&
            expected_type == REAL_TYPE &&
            formal_arg_decl->tree_data.var_decl_data.type != POINTER_TYPE &&
            formal_arg_decl->tree_data.var_decl_data.cached_kgpc_type != NULL &&
            kgpc_type_is_pointer(formal_arg_decl->tree_data.var_decl_data.cached_kgpc_type))
        {
            /* Some out/var real formals retain their scalar source tag while
             * semantic analysis stores the ABI-level byref shape in cached_kgpc_type. */
            is_var_param = 1;
        }
        is_pointer_like = (is_var_param || is_array_param || is_array_arg || formal_is_dynarray);
        if (is_self_param && expected_type == REAL_TYPE)
            is_pointer_like = 0;
        if (arg_infos != NULL)
            arg_infos[arg_num].is_pointer_like = is_pointer_like;
        int force_runtime_real_qword = 0;
        if (expected_type == REAL_TYPE)
        {
            if ((procedure_name != NULL && strncmp(procedure_name, "kgpc_", 5) == 0) ||
                (call_mangled != NULL && strncmp(call_mangled, "kgpc_", 5) == 0))
            {
                force_runtime_real_qword = 1;
                if (arg_infos != NULL)
                    arg_infos[arg_num].expected_real_size = 8;
            }
        }
        if (arg_infos != NULL && expected_type == REAL_TYPE)
        {
            int expr_is_real = 0;
            if (arg_expr != NULL)
            {
                int arg_type = expr_get_type_tag(arg_expr);
                expr_is_real = (arg_type == REAL_TYPE) ||
                    arg_expr->type == EXPR_RNUM ||
                    (arg_expr->type == EXPR_TYPECAST &&
                     arg_expr->expr_data.typecast_data.target_type == REAL_TYPE);
            }

            if (!force_runtime_real_qword && arg_infos[arg_num].expected_real_size == 0)
            {
                if (is_self_param && expr_is_real)
                {
                    arg_infos[arg_num].expected_real_size =
                        codegen_expr_real_storage_size(arg_expr, ctx);
                }
                else
                {
                    /* Without formal metadata, default to double-width real ABI.
                     * This matches runtime helpers like kgpc_trunc(double). */
                    arg_infos[arg_num].expected_real_size = 8;
                }
            }
            else if (!force_runtime_real_qword &&
                arg_infos[arg_num].expected_real_size == 8 &&
                is_self_param && expr_is_real &&
                codegen_expr_real_storage_size(arg_expr, ctx) == 4)
            {
                arg_infos[arg_num].expected_real_size =
                    codegen_expr_real_storage_size(arg_expr, ctx);
            }
        }
        if (arg_infos != NULL && expected_type == REAL_TYPE &&
            arg_infos[arg_num].expected_real_size == 16)
        {
            arg_infos[arg_num].stack_size = 16;
        }

        int arg_handled = 0;
        if (formal_is_open_array && arg_expr != NULL)
        {
            KgpcType *arg_type = expr_get_kgpc_type(arg_expr);
            int arg_is_open_array = (arg_expr->is_array_expr && arg_expr->array_is_dynamic) ||
                (arg_type != NULL && kgpc_type_is_dynamic_array(arg_type));
            if (arg_is_open_array)
            {
                /* Open array argument already represented as a descriptor pointer (e.g., open array param). */
                int use_address = 0;
                if (arg_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
                {
                    StackNode_t *stack_node = find_label(arg_expr->expr_data.id);
                    if (stack_node != NULL && !stack_node->is_reference)
                        use_address = 1;

                    HashNode_t *arg_symbol = NULL;
                    if (FindSymbol(&arg_symbol, ctx->symtab, arg_expr->expr_data.id) != 0 &&
                        arg_symbol != NULL && arg_symbol->is_var_parameter)
                    {
                        use_address = 0;
                    }
                }

                Register_t *value_reg = NULL;
                if (use_address && codegen_expr_is_addressable(arg_expr))
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &value_reg);
                else
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                if (codegen_had_error(ctx) || value_reg == NULL)
                    return inst_list;

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        value_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), value_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], value_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                arg_handled = 1;
            }
            else if (is_array_arg)
            {
                long long element_count = codegen_static_array_length(arg_expr);
                if (element_count < 0)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to determine length for open array argument.");
                    return inst_list;
                }

                StackNode_t *desc_slot = codegen_alloc_temp_bytes("openarr_desc",
                    2 * CODEGEN_POINTER_SIZE_BYTES);
                if (desc_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to allocate descriptor storage for open array argument.");
                    return inst_list;
                }

                if (!codegen_expr_is_addressable(arg_expr))
                {
                    codegen_report_error(ctx,
                        "ERROR: Unsupported expression type for open array argument.");
                    return inst_list;
                }

                Register_t *data_addr_reg = NULL;
                inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &data_addr_reg);
                if (codegen_had_error(ctx) || data_addr_reg == NULL)
                    return inst_list;

                Register_t *desc_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (desc_addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), data_addr_reg);
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for open array descriptor.");
                    return inst_list;
                }

                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    desc_slot->offset, desc_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                    data_addr_reg->bit_64, desc_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), data_addr_reg);

                snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, 8(%s)\n",
                    element_count, desc_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        desc_addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), desc_addr_reg);
                    
                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], desc_addr_reg, arg_expr);
                }
                arg_handled = 1;
            }
        }
        if (!arg_handled)
        {
            if (formal_is_open_array && arg_expr != NULL && arg_expr->type == EXPR_STRING)
            {
                /* Handle string literal passed to open array of Char parameter.
                 * Create a descriptor: (pointer to string data, element count).
                 * The string is placed in read-only section. */
                const char *str_data = arg_expr->expr_data.string;
                int str_len = (str_data != NULL) ? (int)strlen(str_data) : 0;

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

                StackNode_t *desc_slot = codegen_alloc_temp_bytes("str_arr_desc",
                    2 * CODEGEN_POINTER_SIZE_BYTES);
                if (desc_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate descriptor for string literal to open array.");
                    return inst_list;
                }

                /* Get a register to hold the string address temporarily */
                Register_t *data_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (data_addr_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for string literal address.");
                    return inst_list;
                }

                /* Get descriptor address register */
                Register_t *desc_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (desc_addr_reg == NULL)
                {
                    free_reg(get_reg_stack(), data_addr_reg);
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for open array descriptor.");
                    return inst_list;
                }

                /* Load descriptor slot address */
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    desc_slot->offset, desc_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                /* Load string address */
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                         label, data_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                /* Store string pointer at descriptor[0] */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, (%s)\n",
                    data_addr_reg->bit_64, desc_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), data_addr_reg);

                /* Store element count at descriptor[1] (offset 8) */
                snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, 8(%s)\n",
                    str_len, desc_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        desc_addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), desc_addr_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], desc_addr_reg, arg_expr);
                }
                arg_handled = 1;
            }
            /* Handle ShortString parameter (by value): materialize a ShortString buffer
             * matching the formal size and pass its address. */
            else if (expected_type == SHORTSTRING_TYPE && arg_expr != NULL && !is_var_param)
            {
                int shortstr_size = codegen_formal_shortstring_size(formal_arg_decl, ctx->symtab);
                if (shortstr_size < 2)
                    shortstr_size = 2;

                StackNode_t *shortstr_buf = codegen_alloc_temp_bytes("shortstr_arg", shortstr_size);
                if (shortstr_buf == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate ShortString buffer for argument.");
                    if (arg_infos != NULL) free(arg_infos);
                    return inst_list;
                }

                Register_t *buf_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (buf_addr_reg == NULL)
                    buf_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (buf_addr_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for ShortString buffer.");
                    if (arg_infos != NULL) free(arg_infos);
                    return inst_list;
                }

                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    shortstr_buf->offset, buf_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                if (codegen_expr_is_shortstring_value_ctx(arg_expr, ctx) &&
                    !codegen_current_param_is_ansistring(arg_expr, ctx))
                {
                    Register_t *src_reg = NULL;
                    if (codegen_expr_is_addressable(arg_expr))
                    {
                        inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                    }
                    else
                    {
                        inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &src_reg);
                    }
                    if (codegen_had_error(ctx) || src_reg == NULL)
                    {
                        free_reg(get_reg_stack(), buf_addr_reg);
                        if (arg_infos != NULL) free(arg_infos);
                        return inst_list;
                    }

                    /* Re-emit leaq of the stack buffer into buf_addr_reg.  The
                     * earlier expression evaluation (codegen_address_for_expr /
                     * codegen_expr_with_result) may have clobbered this register
                     * because the register allocator does not know it is live
                     * across the inner evaluation. */
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        shortstr_buf->offset, buf_addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);

                    if (codegen_target_is_windows())
                    {
                        { Register_t *u[] = {buf_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                        { Register_t *u[] = {src_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n"); }
                    }
                    else
                    {
                        { Register_t *u[] = {buf_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                        { Register_t *u[] = {src_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                    }

                    inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_shortstring");
                    free_arg_regs();
                    free_reg(get_reg_stack(), src_reg);
                }
                else if (codegen_expr_is_char_array_like_ctx(arg_expr, ctx))
                {
                    long long array_len = 0;
                    if (!codegen_get_char_array_length(arg_expr, ctx, &array_len) || array_len <= 0)
                    {
                        free_reg(get_reg_stack(), buf_addr_reg);
                        if (arg_infos != NULL) free(arg_infos);
                        return inst_list;
                    }

                    Register_t *src_reg = NULL;
                    if (!codegen_expr_is_addressable(arg_expr))
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unsupported expression type for ShortString conversion.");
                        free_reg(get_reg_stack(), buf_addr_reg);
                        if (arg_infos != NULL) free(arg_infos);
                        return inst_list;
                    }
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                    {
                        free_reg(get_reg_stack(), buf_addr_reg);
                        if (arg_infos != NULL) free(arg_infos);
                        return inst_list;
                    }

                    /* Re-emit leaq of the stack buffer into buf_addr_reg; see
                     * companion comment above for the rationale. */
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        shortstr_buf->offset, buf_addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);

                    if (codegen_target_is_windows())
                    {
                        { Register_t *u[] = {buf_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
                        { Register_t *u[] = {src_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r8\n", array_len);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r9d\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        { Register_t *u[] = {buf_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n"); }
                        { Register_t *u[] = {src_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%rdx\n", array_len);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%ecx\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_array_to_shortstring");
                    free_arg_regs();
                    free_reg(get_reg_stack(), src_reg);
                }
                else
                {
                    Register_t *value_reg = NULL;
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                    if (codegen_had_error(ctx) || value_reg == NULL)
                    {
                        free_reg(get_reg_stack(), buf_addr_reg);
                        if (arg_infos != NULL) free(arg_infos);
                        return inst_list;
                    }

                    /* When the argument expression is a char value (e.g. a single-char
                     * string constant like version_nr = '3' which the semantic checker
                     * resolves to CHAR_TYPE with ordinal value), we must promote it to
                     * an AnsiString before kgpc_string_to_shortstring dereferences it
                     * as a pointer.  Without this, the raw char ordinal (e.g. 51 for '3')
                     * is passed as a pointer and causes a segfault. */
                    if (expr_has_type_tag(arg_expr, CHAR_TYPE))
                    {
                        /* Save buf_addr_reg across the call since kgpc_char_to_string
                         * clobbers caller-saved registers. */
                        StackNode_t *buf_save = add_l_t("shortstr_buf_save");
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            buf_addr_reg->bit_64, buf_save->offset);
                        inst_list = add_inst(inst_list, buffer);

                        const char *char_arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
                        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n",
                            value_reg->bit_32, char_arg_reg32);
                        inst_list = add_inst(inst_list, buffer);
                        inst_list = codegen_vect_reg(inst_list, 0);
                        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
                        { Register_t *d[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                        free_arg_regs();

                        /* Restore buf_addr_reg */
                        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                            buf_save->offset, buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    /* Re-emit leaq of the stack buffer into buf_addr_reg.  The
                     * earlier codegen_expr_with_result for arg_expr may have
                     * clobbered this register (e.g. when arg_expr requires a
                     * scratch register that happens to coincide with buf_addr_reg). */
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        shortstr_buf->offset, buf_addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);

                    if (codegen_target_is_windows())
                    {
                        { Register_t *u[] = {buf_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
                        { Register_t *u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        { Register_t *u[] = {buf_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n"); }
                        { Register_t *u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_to_shortstring");
                    free_arg_regs();
                    free_reg(get_reg_stack(), value_reg);
                }

                /* Reload buffer address after possible calls */
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    shortstr_buf->offset, buf_addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        buf_addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), buf_addr_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], buf_addr_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
            }
            else if (is_var_param || is_array_param || is_array_arg)
            {
                Register_t *addr_reg = NULL;
                if (arg_expr != NULL && arg_expr->type == EXPR_NIL)
                {
                    /* Passing nil to a var parameter: pass null pointer (0) */
                    addr_reg = codegen_try_get_reg(&inst_list, ctx, "nil_var_param");
                    if (addr_reg != NULL)
                    {
                        char buf[64];
                        snprintf(buf, sizeof(buf), "\txorq\t%s, %s\n",
                                 addr_reg->bit_64, addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buf);
                    }
                }
                else if (arg_expr != NULL && arg_expr->type == EXPR_ARRAY_LITERAL)
                {
                    inst_list = codegen_materialize_array_literal(arg_expr, inst_list, ctx, &addr_reg);
                }
                else
                {
                    struct Expression *address_expr = arg_expr;
                    int forward_open_array_data = 0;
                    if (!codegen_expr_is_addressable(address_expr) &&
                        address_expr != NULL &&
                        address_expr->type == EXPR_FUNCTION_CALL &&
                        ctx != NULL && ctx->symtab != NULL)
                    {
                        struct Expression *cast_inner =
                            codegen_unwrap_typecast_call_expr(address_expr, ctx->symtab);
                        if (cast_inner != NULL)
                            address_expr = cast_inner;
                    }
                    if (!codegen_expr_is_addressable(address_expr))
                    {
                        const char *call_id = NULL;
                        if (arg_expr != NULL && arg_expr->type == EXPR_FUNCTION_CALL)
                            call_id = arg_expr->expr_data.function_call_data.id;
                        codegen_report_error(ctx,
                            "ERROR: Unsupported expression type for var parameter (expr_type=%d%s%s) in call to %s arg %d.",
                            arg_expr != NULL ? arg_expr->type : -1,
                            call_id != NULL ? " call_id=" : "",
                            call_id != NULL ? call_id : "",
                            procedure_name ? procedure_name : "(unknown)",
                            arg_num);
                        return inst_list;
                    }
                    inst_list = codegen_address_for_expr(address_expr, inst_list, ctx, &addr_reg);

                    /* BUGFIX: For assigned() with comma-syntax multi-dimensional array
                     * accesses (e.g., assigned(fbitmap[x1,y1])), codegen_address_for_expr
                     * returns the address of the innermost pointer slot.  kgpc_assigned
                     * expects the pointer VALUE, not its address, so we must dereference. */
                    if (addr_reg != NULL &&
                        call_expr != NULL &&
                        call_expr->expr_data.function_call_data.builtin_call_lowering == BUILTIN_CALL_ASSIGNED &&
                        address_expr != NULL &&
                        address_expr->type == EXPR_ARRAY_ACCESS &&
                        address_expr->expr_data.array_access_data.extra_indices != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                            addr_reg->bit_64, addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    if (addr_reg != NULL &&
                        formal_arg_decl != NULL &&
                        formal_arg_decl->type == TREE_VAR_DECL &&
                        formal_arg_decl->tree_data.var_decl_data.is_untyped_param &&
                        codegen_expr_is_open_array_descriptor_arg(address_expr, ctx))
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                            addr_reg->bit_64, addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        forward_open_array_data = 1;
                    }

                    /* BUGFIX: For TRUE var parameters of class types, we pass the ADDRESS of the variable itself,
                     * not the value it contains. This allows the callee to update the variable (e.g., FreeAndNil).
                     *
                     * However, for class methods, Self (first parameter) needs to be dereferenced to pass the
                     * instance pointer, even though it's technically a var parameter internally. */

                    struct RecordType *arg_record = codegen_expr_record_type(arg_expr,
                        ctx != NULL ? ctx->symtab : NULL);
                    if (!forward_open_array_data &&
                        addr_reg != NULL && arg_expr != NULL && arg_expr->type != EXPR_AS &&
                        arg_record != NULL && record_type_is_class(arg_record))
                    {
                        /* Check if the argument expression is itself a var parameter variable.
                         * If so, codegen_address_for_expr already loaded the instance pointer via movq,
                         * so we should NOT dereference again. */
                        int arg_is_var_param = 0;
                        if (arg_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
                        {
                            HashNode_t *arg_symbol = NULL;
                            if (FindSymbol(&arg_symbol, ctx->symtab, arg_expr->expr_data.id) != 0 &&
                                arg_symbol != NULL && arg_symbol->is_var_parameter)
                            {
                                arg_is_var_param = 1;
                            }
                        }

                        /* Self parameters of class type have is_var_param cleared to 0
                         * (at line ~9895), so they never enter this is_var_param branch.
                         * Only real var parameters and array parameters reach here.
                         *
                         * For non-var class parameters (entered via is_array_param/is_array_arg),
                         * we need to dereference to get the instance pointer.
                         * For var parameters of class type, we pass the address as-is
                         * so the callee can modify the variable. */
                        int should_dereference = 0;
                        if (!is_var_param && !arg_is_var_param)
                        {
                            /* Non-var class parameter: dereference to get instance pointer */
                            should_dereference = 1;
                        }

                        if (should_dereference)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                                addr_reg->bit_64, addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                }
                if (codegen_had_error(ctx) || addr_reg == NULL)
                    return inst_list;

                /* When passing a ShortString argument to a formal AnsiString (STRING_TYPE)
                 * parameter, convert via kgpc_shortstring_to_string.  Without this, the
                 * raw ShortString buffer address (length-byte + data) is passed and the
                 * callee interprets the length byte as the first character of an AnsiString. */
                int formal_wants_ansistring = 0;
                if (expected_type == STRING_TYPE)
                    formal_wants_ansistring = 1;
                else if (expected_type == UNKNOWN_TYPE &&
                    formal_arg_decl != NULL &&
                    formal_arg_decl->type == TREE_VAR_DECL &&
                    formal_arg_decl->tree_data.var_decl_data.type == STRING_TYPE)
                    formal_wants_ansistring = 1;
                /* For calls through procedural type variables (function pointers),
                 * expected_type may remain UNKNOWN_TYPE because codegen_param_expected_type
                 * returns UNKNOWN when the formal has no shortstring marker.  Detect
                 * the AnsiString-family case from the formal's type_id. */
                if (!formal_wants_ansistring && formal_arg_decl != NULL &&
                    formal_arg_decl->type == TREE_VAR_DECL &&
                    formal_decl_expects_string(formal_arg_decl, ctx != NULL ? ctx->symtab : NULL))
                {
                    int formal_is_shortstring = 0;
                    KgpcType *cached = formal_arg_decl->tree_data.var_decl_data.cached_kgpc_type;
                    if (cached != NULL && kgpc_type_is_shortstring(cached))
                        formal_is_shortstring = 1;
                    if (formal_arg_decl->tree_data.var_decl_data.type == SHORTSTRING_TYPE)
                        formal_is_shortstring = 1;
                    const char *formal_type_id = formal_arg_decl->tree_data.var_decl_data.type_id;
                    if (formal_type_id != NULL &&
                        pascal_identifier_equals(formal_type_id, "ShortString"))
                        formal_is_shortstring = 1;
                    if (!formal_is_shortstring)
                        formal_wants_ansistring = 1;
                }
                if (is_array_arg && !is_var_param && !is_array_param &&
                    formal_wants_ansistring &&
                    (!expr_has_type_tag(arg_expr, STRING_TYPE) ||
                     expr_has_type_tag(arg_expr, SHORTSTRING_TYPE)) &&
                    !codegen_current_param_is_ansistring(arg_expr, ctx) &&
                    codegen_expr_is_shortstring_value_ctx(arg_expr, ctx))
                {
                    inst_list = codegen_promote_shortstring_reg(inst_list, ctx, addr_reg);
                    /* addr_reg now holds an AnsiString pointer; clear is_pointer_like
                     * so the spill is treated as a by-value string, not as a by-ref address. */
                    if (arg_infos != NULL)
                        arg_infos[arg_num].is_pointer_like = 0;
                }

                /* ARCHITECTURAL FIX: Spill address to stack to prevent clobbering by nested calls */
                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
                }
            }
            else if (formal_is_char_set && arg_expr != NULL && expr_has_type_tag(arg_expr, SET_TYPE))
            {
                Register_t *addr_reg = NULL;
                if (arg_expr->type == EXPR_SET)
                {
                    inst_list = codegen_set_literal(arg_expr, inst_list, ctx, &addr_reg, 1);
                }
                else
                {
                    if (!codegen_expr_is_addressable(arg_expr))
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unsupported expression type for set parameter.");
                        return inst_list;
                    }
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
                }
                if (codegen_had_error(ctx) || addr_reg == NULL)
                    return inst_list;

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
            }
            else if (formal_is_dynarray && arg_expr != NULL &&
                arg_expr->is_array_expr && arg_expr->array_is_dynamic)
            {
                Register_t *addr_reg = NULL;
                if (!codegen_expr_is_addressable(arg_expr))
                {
                    int descriptor_size = codegen_dynarray_descriptor_size(arg_expr);
                    StackNode_t *temp_slot = codegen_alloc_temp_bytes("dynarray_arg", descriptor_size);
                    if (temp_slot == NULL)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to allocate temporary storage for dynamic array argument.");
                        return inst_list;
                    }

                    Register_t *value_reg = NULL;
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                    if (codegen_had_error(ctx) || value_reg == NULL)
                    {
                        if (value_reg != NULL)
                            free_reg(get_reg_stack(), value_reg);
                        return inst_list;
                    }

                    addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (addr_reg == NULL)
                    {
                        free_reg(get_reg_stack(), value_reg);
                        codegen_report_error(ctx,
                            "ERROR: Unable to allocate register for dynamic array argument address.");
                        return inst_list;
                    }

                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        temp_slot->offset, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);

                    if (codegen_target_is_windows())
                    {
                        { Register_t *u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
                        { Register_t *u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", descriptor_size);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        { Register_t *u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n"); }
                        { Register_t *u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", descriptor_size);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    inst_list = codegen_vect_reg(inst_list, 0);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_assign_from_temp");
                    free_arg_regs();
                    free_reg(get_reg_stack(), value_reg);

                    /* The temp slot now owns the producer's element-data
                     * buffer.  It is unreachable through any user-declared
                     * variable, so register it for finalize-at-epilogue
                     * cleanup; otherwise the data leaks for the remainder
                     * of the enclosing function/program. */
                    codegen_track_managed_dynarray_temp(ctx, temp_slot->offset);

                    /* Reload address because the call may clobber caller-saved regs. */
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        temp_slot->offset, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
                }
                if (codegen_had_error(ctx) || addr_reg == NULL)
                    return inst_list;

                /* BUGFIX: For assigned() with comma-syntax multi-dimensional array
                 * accesses, codegen_address_for_expr returns the address of the
                 * innermost pointer slot.  kgpc_assigned expects the pointer VALUE,
                 * not its address, so we must dereference. */
                if (addr_reg != NULL &&
                    call_expr != NULL &&
                    call_expr->expr_data.function_call_data.builtin_call_lowering == BUILTIN_CALL_ASSIGNED &&
                    arg_expr != NULL &&
                    arg_expr->type == EXPR_ARRAY_ACCESS &&
                    arg_expr->expr_data.array_access_data.extra_indices != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                        addr_reg->bit_64, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
                }
            }
            else if (formal_arg_decl != NULL &&
                formal_arg_decl->type == TREE_VAR_DECL &&
                formal_arg_decl->tree_data.var_decl_data.type_id != NULL &&
                pascal_identifier_equals(
                    formal_arg_decl->tree_data.var_decl_data.type_id,
                    "Tconstexprint") &&
                arg_expr != NULL &&
                !expr_has_type_tag(arg_expr, RECORD_TYPE) &&
                (is_integer_type(expr_get_type_tag(arg_expr)) ||
                 expr_get_type_tag(arg_expr) == QWORD_TYPE))
            {
                StackNode_t *temp_slot = codegen_alloc_temp_bytes("tconstexprint_arg", 10);
                Register_t *value_reg = NULL;
                Register_t *addr_reg = NULL;
                int use_signed_assign =
                    expr_is_signed_kgpctype(arg_expr) &&
                    !(arg_expr->type == EXPR_INUM &&
                      arg_expr->expr_data.i_num >= 0);
                const char *assign_target =
                    use_signed_assign
                        ? "int64__op_assign_Tconstexprint"
                        : "qword__op_assign_Tconstexprint";
                if (temp_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate Tconstexprint argument temporary.");
                    return inst_list;
                }

                inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                if (codegen_had_error(ctx) || value_reg == NULL)
                    return inst_list;

                if (!expr_uses_qword_kgpctype(arg_expr) && use_signed_assign)
                    inst_list = codegen_sign_extend32_to64(inst_list,
                        value_reg->bit_32, value_reg->bit_64);

                if (codegen_target_is_windows())
                {
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n",
                        temp_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n",
                        value_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n",
                        temp_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n",
                        value_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, assign_target);
                free_arg_regs();
                free_reg(get_reg_stack(), value_reg);

                addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                    addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (addr_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for Tconstexprint argument.");
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    temp_slot->offset, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        addr_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);
                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
            }
            else if (arg_expr != NULL && expr_get_kgpc_type(arg_expr) != NULL &&
                     kgpc_type_is_method_pointer(expr_get_kgpc_type(arg_expr)) &&
                     call_expr != NULL &&
                     call_expr->expr_data.function_call_data.builtin_call_lowering == BUILTIN_CALL_ASSIGNED)
            {
                /* Assigned(method_ptr) must inspect only the code field
                 * (offset 0) of the TMethod aggregate.  Loading the entire
                 * 16-byte record into a stack temp and passing its address
                 * would always test non-null.  Take the address of the
                 * method pointer (or evaluate it into a register), load
                 * the code-field via [reg], and pass that as the scalar
                 * argument to kgpc_assigned. */
                Register_t *src_reg = NULL;
                if (codegen_expr_is_addressable(arg_expr))
                {
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }
                else
                {
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }
                char load_buf[128];
                snprintf(load_buf, sizeof(load_buf),
                    "\tmovq\t(%s), %s\n", src_reg->bit_64, src_reg->bit_64);
                inst_list = add_inst(inst_list, load_buf);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(load_buf, sizeof(load_buf), "\tmovq\t%s, -%d(%%rbp)\n",
                        src_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, load_buf);
                    free_reg(get_reg_stack(), src_reg);
                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], src_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
            }
            else if (arg_expr != NULL && expr_get_kgpc_type(arg_expr) != NULL &&
                     kgpc_type_is_dynamic_array(expr_get_kgpc_type(arg_expr)) &&
                     call_expr != NULL &&
                     call_expr->expr_data.function_call_data.builtin_call_lowering == BUILTIN_CALL_ASSIGNED)
            {
                /* Assigned(dynarray) must inspect only the data-pointer
                 * field (offset 0) of the dynamic-array descriptor.
                 * Passing the descriptor's address would always test
                 * non-null because the descriptor lives in the heap-
                 * allocated outer array.  Instead, load the data pointer
                 * via [reg] and pass that scalar to kgpc_assigned. */
                Register_t *src_reg = NULL;
                if (codegen_expr_is_addressable(arg_expr))
                {
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }
                else
                {
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }
                char load_buf[128];
                snprintf(load_buf, sizeof(load_buf),
                    "\tmovq\t(%s), %s\n", src_reg->bit_64, src_reg->bit_64);
                inst_list = add_inst(inst_list, load_buf);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(load_buf, sizeof(load_buf), "\tmovq\t%s, -%d(%%rbp)\n",
                        src_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, load_buf);
                    free_reg(get_reg_stack(), src_reg);
                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], src_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
            }
            else if (arg_expr != NULL && expr_get_kgpc_type(arg_expr) != NULL &&
                     kgpc_type_is_method_pointer(expr_get_kgpc_type(arg_expr)))
            {
                /* Method pointers (TMethod) are 16-byte aggregates passed
                 * like records: copy 16 bytes into a stack temp and pass
                 * the address.  Reuses the record-arg machinery below by
                 * faking record_size = 16. */
                long long record_size = 16;
                StackNode_t *temp_slot = codegen_alloc_record_temp(record_size);
                if (temp_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to allocate temporary storage for method-pointer argument.");
                    return inst_list;
                }

                Register_t *src_reg = NULL;
                if (codegen_expr_is_addressable(arg_expr))
                {
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }
                else
                {
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }

                /* Copy 16 bytes via kgpc_move into the temp slot. */
                char copy_buffer[128];
                if (codegen_target_is_windows())
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rdx\n",
                        src_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer),
                        "\tleaq\t-%d(%%rbp), %%rcx\n", temp_slot->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer),
                        "\tmovq\t$%lld, %%r8\n", record_size);
                    inst_list = add_inst(inst_list, copy_buffer);
                }
                else
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rsi\n",
                        src_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer),
                        "\tleaq\t-%d(%%rbp), %%rdi\n", temp_slot->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer),
                        "\tmovq\t$%lld, %%rdx\n", record_size);
                    inst_list = add_inst(inst_list, copy_buffer);
                }
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                free_arg_regs();
                free_reg(get_reg_stack(), src_reg);

                /* Pass the address of the temp as the argument. */
                Register_t *result_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (result_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for method-pointer argument.");
                    return inst_list;
                }
                snprintf(copy_buffer, sizeof(copy_buffer),
                    "\tleaq\t-%d(%%rbp), %s\n",
                    temp_slot->offset, result_reg->bit_64);
                inst_list = add_inst(inst_list, copy_buffer);

                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        result_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    free_reg(get_reg_stack(), result_reg);
                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                    arg_infos[arg_num].is_pointer_like = 1;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], result_reg, arg_expr);
                    arg_infos[arg_num].is_pointer_like = 1;
                }
            }
            else if (arg_expr != NULL && expr_has_type_tag(arg_expr, RECORD_TYPE))
            {
                long long record_size = 0;
                if (codegen_get_record_size(ctx, arg_expr, &record_size) != 0 || record_size < 0)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to determine record size for argument.");
                    return inst_list;
                }
                if (record_size == 0)
                    record_size = 1;


                if (record_size > INT_MAX)
                {
                    codegen_report_error(ctx,
                        "ERROR: Record argument size exceeds supported limits.");
                    return inst_list;
                }

                StackNode_t *temp_slot = codegen_alloc_record_temp(record_size);
                if (temp_slot == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Failed to allocate temporary storage for record argument.");
                    return inst_list;
                }

                Register_t *src_reg = NULL;

                /* Check if this is an interface type identifier used as a
                 * GUID argument (e.g. Supports(Obj, IObserver, I)).
                 * If so, load __kgpc_guid_<Name> directly instead of going
                 * through codegen_address_for_expr which would emit a
                 * reference to the bare interface label. */
                int is_iface_guid_arg = 0;
                if (record_size == 16 && arg_expr->type == EXPR_VAR_ID &&
                    codegen_expr_is_addressable(arg_expr) &&
                    ctx != NULL && ctx->symtab != NULL) {
                    ListNode_t *all_idents = FindAllIdents(ctx->symtab, arg_expr->expr_data.id);
                    for (ListNode_t *id_node = all_idents; id_node != NULL; id_node = id_node->next) {
                        HashNode_t *cand = (HashNode_t *)id_node->cur;
                        if (cand == NULL) continue;
                        struct RecordType *cand_rec = codegen_get_record_type_from_node(cand);
                        if (cand_rec == NULL && cand->type != NULL &&
                            cand->type->kind == TYPE_KIND_POINTER &&
                            cand->type->info.points_to != NULL &&
                            cand->type->info.points_to->kind == TYPE_KIND_RECORD)
                            cand_rec = cand->type->info.points_to->info.record_info;
                        if (cand_rec != NULL && cand_rec->is_interface) {
                            is_iface_guid_arg = 1;
                            break;
                        }
                    }
                    if (all_idents != NULL) DestroyList(all_idents);
                }

                if (is_iface_guid_arg) {
                    src_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (src_reg == NULL) {
                        codegen_report_error(ctx,
                            "ERROR: Failed to allocate register for interface GUID argument.");
                        return inst_list;
                    }
                    char guid_buf[512];
                    snprintf(guid_buf, sizeof(guid_buf),
                        "\tleaq\t__kgpc_guid_%s(%%rip), %s\n",
                        arg_expr->expr_data.id, src_reg->bit_64);
                    inst_list = add_inst(inst_list, guid_buf);
                }
                else if (codegen_expr_is_addressable(arg_expr))
                {
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                    if (codegen_had_error(ctx) || src_reg == NULL)
                        return inst_list;
                }
                else
                {
                    if (arg_expr->type == EXPR_FUNCTION_CALL && expr_returns_sret(arg_expr))
                    {
                        inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &src_reg);
                        if (codegen_had_error(ctx) || src_reg == NULL)
                            return inst_list;
                    }
                    else if (record_size > 8)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unsupported record argument expression.");
                        return inst_list;
                    }
                    else
                    {
                        Register_t *value_reg = NULL;
                        inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                        if (codegen_had_error(ctx) || value_reg == NULL)
                            return inst_list;

                        char materialize_buf[128];
                        if (record_size <= 4)
                            snprintf(materialize_buf, sizeof(materialize_buf), "\tmovl\t%s, -%d(%%rbp)\n",
                                value_reg->bit_32, temp_slot->offset);
                        else
                            snprintf(materialize_buf, sizeof(materialize_buf), "\tmovq\t%s, -%d(%%rbp)\n",
                                value_reg->bit_64, temp_slot->offset);
                        inst_list = add_inst(inst_list, materialize_buf);
                        free_reg(get_reg_stack(), value_reg);

                        src_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (src_reg == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate register for record argument address.");
                            return inst_list;
                        }
                        snprintf(materialize_buf, sizeof(materialize_buf), "\tleaq\t-%d(%%rbp), %s\n",
                            temp_slot->offset, src_reg->bit_64);
                        inst_list = add_inst(inst_list, materialize_buf);
                    }
                }

                char copy_buffer[128];

                if (codegen_target_is_windows())
                {
                    { Register_t *u[] = {src_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", temp_slot->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t$%lld, %%r8\n", record_size);
                    inst_list = add_inst(inst_list, copy_buffer);
                }
                else
                {
                    { Register_t *u[] = {src_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %%rdi\n", temp_slot->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t$%lld, %%rdx\n", record_size);
                    inst_list = add_inst(inst_list, copy_buffer);
                }

                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                free_arg_regs();

                free_reg(get_reg_stack(), src_reg);

                Register_t *result_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (result_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for record argument pointer.");
                    return inst_list;
                }

                /* For external C functions (cdecl), small structs (<=8 bytes) are passed by VALUE,
                 * but Pascal passes them by reference (pointer). We automatically dereference
                 * the pointer here so Pascal code doesn't need to change.
                 *
                 * Example: inet_ntoa(in_addr: TInAddr) where TInAddr is 4 bytes
                 * - Pascal passes pointer to TInAddr
                 * - C expects TInAddr value in register
                 * - We dereference: load the 4-byte value from the pointer
                 *
                 * CRITICAL FIX: Check the KgpcType's procedure definition for cname_flag
                 * instead of checking if the procedure name contains "cdecl" or "external".
                 * The procedure name is just "inet_ntoa", not "inet_ntoa_cdecl_external".
                 */
                if (is_external_c_function && record_size <= 8)
                {
                    /* Load address of the record copy */
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        temp_slot->offset, result_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buffer);

                    /* Dereference: load the value from the address */
                    if (record_size == 1)
                    {
                        snprintf(copy_buffer, sizeof(copy_buffer), "\tmovzbl\t(%s), %s\n",
                            result_reg->bit_64, result_reg->bit_32);
                    }
                    else if (record_size == 2)
                    {
                        snprintf(copy_buffer, sizeof(copy_buffer), "\tmovzwl\t(%s), %s\n",
                            result_reg->bit_64, result_reg->bit_32);
                    }
                    else if (record_size <= 4)
                    {
                        snprintf(copy_buffer, sizeof(copy_buffer), "\tmovl\t(%s), %s\n",
                            result_reg->bit_64, result_reg->bit_32);
                    }
                    else /* record_size <= 8 */
                    {
                        snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t(%s), %s\n",
                            result_reg->bit_64, result_reg->bit_64);
                    }
                    inst_list = add_inst(inst_list, copy_buffer);
                }
                else
                {
                    /* Normal case: pass pointer to struct */
                    {
                        char copy_buffer_tmpl[128];
                        snprintf(copy_buffer_tmpl, sizeof(copy_buffer_tmpl), "\tleaq\t-%d(%%rbp), %%0\n", temp_slot->offset);
                        Register_t *d[] = {result_reg};
                        inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, copy_buffer_tmpl);
                    }
                }

                /* ARCHITECTURAL FIX: Spill address to stack to prevent clobbering by nested calls */
                StackNode_t *arg_spill = add_l_t("arg_eval");
                if (arg_spill != NULL && arg_infos != NULL)
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        result_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    free_reg(get_reg_stack(), result_reg);

                    arg_infos[arg_num].reg = NULL;
                    arg_infos[arg_num].spill = arg_spill;
                    arg_infos[arg_num].expr = arg_expr;
                }
                else if (arg_infos != NULL)
                {
                    arginfo_assign_register(&arg_infos[arg_num], result_reg, arg_expr);
                }
            }
            else if (arg_expr != NULL && expr_has_type_tag(arg_expr, CHAR_TYPE))
            {
                int owns_formal_kgpc = 0;
                KgpcType *formal_kgpc = NULL;
                if (formal_arg_decl != NULL && ctx != NULL && ctx->symtab != NULL)
                    formal_kgpc = resolve_type_from_vardecl(formal_arg_decl, ctx->symtab, &owns_formal_kgpc);

                int formal_is_char_pointer =
                    (formal_kgpc != NULL &&
                     formal_kgpc->kind == TYPE_KIND_POINTER &&
                     formal_kgpc->info.points_to != NULL &&
                     formal_kgpc->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                     formal_kgpc->info.points_to->info.primitive_type_tag == CHAR_TYPE);

                if (formal_is_char_pointer && codegen_expr_is_addressable(arg_expr))
                {
                    Register_t *addr_reg = NULL;
                    inst_list = codegen_address_for_expr(arg_expr, inst_list, ctx, &addr_reg);
                    if (owns_formal_kgpc && formal_kgpc != NULL)
                        destroy_kgpc_type(formal_kgpc);
                    if (codegen_had_error(ctx) || addr_reg == NULL)
                        return inst_list;

                    StackNode_t *arg_spill = add_l_t("arg_eval");
                    if (arg_spill != NULL && arg_infos != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            addr_reg->bit_64, arg_spill->offset);
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), addr_reg);

                        arg_infos[arg_num].reg = NULL;
                        arg_infos[arg_num].spill = arg_spill;
                        arg_infos[arg_num].expr = arg_expr;
                        arg_infos[arg_num].is_pointer_like = 1;
                    }
                    else if (arg_infos != NULL)
                    {
                        arginfo_assign_register(&arg_infos[arg_num], addr_reg, arg_expr);
                        arg_infos[arg_num].is_pointer_like = 1;
                    }
                }
                else
                {
                    if (owns_formal_kgpc && formal_kgpc != NULL)
                        destroy_kgpc_type(formal_kgpc);
                    goto pass_value_arg;
                }
            }
            else
            {
pass_value_arg:
                // Pass by value
                if (expected_type == REAL_TYPE && arg_infos != NULL &&
                    arg_infos[arg_num].expected_real_size == 16)
                {
                    inst_list = codegen_materialize_extended_arg_spill(
                        &arg_infos[arg_num], arg_expr, inst_list, ctx);
                    if (codegen_had_error(ctx))
                    {
                        if (arg_infos != NULL)
                            free(arg_infos);
                        return inst_list;
                    }
                }
                else
                {
                if (arg_expr->type == EXPR_AS || arg_expr->type == EXPR_IS ||
                    arg_expr->type == EXPR_ARRAY_LITERAL ||
                    expr_has_type_tag(arg_expr, SET_TYPE))
                {
                    Register_t *value_reg = NULL;
                    inst_list = codegen_expr_with_result(arg_expr, inst_list, ctx, &value_reg);
                    if (codegen_had_error(ctx) || value_reg == NULL)
                    {
                        if (arg_infos != NULL)
                            free(arg_infos);
                        return inst_list;
                    }
                    top_reg = value_reg;
                }
                else if (arg_expr->type == EXPR_TYPECAST &&
                    arg_expr->expr_data.typecast_data.expr != NULL &&
                    arg_expr->expr_data.typecast_data.target_type == STRING_TYPE &&
                    codegen_expr_is_shortstring_value_ctx(
                        arg_expr->expr_data.typecast_data.expr, ctx))
                {
                    /* ShortString to AnsiString typecast: build_expr_tree
                     * strips EXPR_TYPECAST nodes, losing the conversion.
                     * Use codegen_expr_tree_value which handles this. */
                    Register_t *value_reg = NULL;
                    inst_list = codegen_expr_tree_value(arg_expr, inst_list, ctx, &value_reg);
                    if (codegen_had_error(ctx) || value_reg == NULL)
                    {
                        if (arg_infos != NULL)
                            free(arg_infos);
                        return inst_list;
                    }
                    top_reg = value_reg;
                }
                else
                {
                    expr_tree = build_expr_tree(arg_expr);
                    top_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (top_reg == NULL)
                    {
                        /* Try spilling to get a register */
                        top_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                    }
                    CODEGEN_DEBUG("DEBUG: top_reg at %p\n", top_reg);
                    if (top_reg == NULL)
                    {
                        free_expr_tree(expr_tree);
                        codegen_report_error(ctx,
                            "ERROR: Unable to allocate register for argument evaluation. "
                            "Expression may be too complex for available registers.");
                        if (arg_infos != NULL)
                            free(arg_infos);
                        return inst_list;
                    }
                    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, top_reg);
                    free_expr_tree(expr_tree);
                }

                /* BUGFIX: For assigned() with comma-syntax array accesses,
                 * gencode_expr_tree returns the address of the pointer slot.
                 * kgpc_assigned expects the pointer VALUE, so dereference. */
                if (top_reg != NULL &&
                    call_expr != NULL &&
                    call_expr->expr_data.function_call_data.builtin_call_lowering == BUILTIN_CALL_ASSIGNED &&
                    arg_expr != NULL &&
                    arg_expr->type == EXPR_ARRAY_ACCESS &&
                    arg_expr->expr_data.array_access_data.extra_indices != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                        top_reg->bit_64, top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                if (expected_type == REAL_TYPE)
                    inst_list = codegen_expr_maybe_convert_int_like_to_real(expected_type,
                        arg_expr, top_reg, inst_list, ctx);

                /* Extended sret function calls leave the buffer ADDRESS in the
                 * register.  Convert to double bits for callees expecting a
                 * regular double argument (e.g. kgpc_trunc). */
                if (expected_type == REAL_TYPE &&
                    expr_returns_sret(arg_expr) &&
                    codegen_expr_involves_extended(arg_expr))
                {
                    {
                        char buffer_tmpl[128];
                        if (codegen_target_is_windows())
                            snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %%rcx\n");
                        else
                            snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %%rdi\n");
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_vect_reg(inst_list, 0);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
                    free_arg_regs();
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                }

                /* Promote char arguments to strings when the formal parameter expects string,
                 * unless the semantic checker already rewrote the call to a runtime
                 * overload that accepts a char natively (e.g. kgpc_string_pos_ca).
                 * Also handle explicit char-to-string typecasts like AnsiString(c)
                 * where build_expr_tree strips the typecast, leaving a raw char value. */
                int arg_is_char_value = expr_has_type_tag(arg_expr, CHAR_TYPE);
                /* Detect explicit char-to-string typecasts like AnsiString(c).
                 * build_expr_tree strips the typecast, so the result in top_reg
                 * is a raw char value that needs promotion to a string. */
                int char_to_string_typecast = 0;
                if (!arg_is_char_value && arg_expr != NULL &&
                    arg_expr->type == EXPR_TYPECAST &&
                    arg_expr->expr_data.typecast_data.expr != NULL &&
                    is_string_type(arg_expr->expr_data.typecast_data.target_type) &&
                    expr_has_type_tag(arg_expr->expr_data.typecast_data.expr, CHAR_TYPE))
                {
                    arg_is_char_value = 1;
                    char_to_string_typecast = 1;
                }
                int formal_expects_string =
                    (formal_decl_expects_string(formal_arg_decl, ctx != NULL ? ctx->symtab : NULL) ||
                     builtin_arg_expects_string(procedure_name, arg_num));
                int formal_expects_wide_string =
                    formal_decl_expects_wide_string(formal_arg_decl, ctx->symtab);

                if (((formal_expects_string &&
                      !formal_expects_wide_string) &&
                     arg_is_char_value &&
                     !mangled_call_expects_char(call_expr, arg_num)) ||
                    char_to_string_typecast)
                {
                    const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovl\t%%0, %s\n", arg_reg32);
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                }

                if (formal_expects_wide_string &&
                    arg_is_char_value &&
                    !codegen_expr_is_wide_string_value(arg_expr))
                {
                    const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovl\t%%0, %s\n", arg_reg32);
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg_reg64);
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_unicodestring_from_string");
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                }

                if (formal_expects_string &&
                    !formal_expects_wide_string &&
                    arg_expr != NULL &&
                    codegen_expr_is_shortstring_value_ctx(arg_expr, ctx) &&
                    !codegen_current_param_is_ansistring(arg_expr, ctx))
                {
                    inst_list = codegen_promote_shortstring_reg(inst_list, ctx, top_reg);
                }

                if (formal_expects_string &&
                    !formal_expects_wide_string &&
                    arg_expr != NULL &&
                    codegen_expr_is_wide_string_value(arg_expr))
                {
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg_reg64);
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_from_unicodestring");
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                }

                if (formal_expects_string &&
                    !formal_expects_wide_string &&
                    arg_expr != NULL &&
                    arg_expr->type == EXPR_STRING &&
                    expr_get_type_tag(arg_expr) != CHAR_TYPE &&
                    !codegen_expr_is_wide_string_value(arg_expr))
                {
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg_reg64);
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_duplicate");
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                }

                if (formal_expects_wide_string &&
                    !codegen_expr_is_wide_string_value(arg_expr) &&
                    (expr_has_type_tag(arg_expr, STRING_TYPE) ||
                     expr_has_type_tag(arg_expr, SHORTSTRING_TYPE) ||
                     arg_expr->type == EXPR_STRING))
                {
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg_reg64);
                        Register_t *u[] = {top_reg};
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                    }
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_unicodestring_from_string");
                    { Register_t *d[] = {top_reg}; inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%rax, %0\n"); }
                }

                if (arg_num == 0 &&
                    codegen_call_requires_class_method_vmt_self(call_expr, ctx) &&
                    !codegen_expr_is_class_vmt_value(arg_expr, ctx) &&
                    codegen_expr_needs_class_method_vmt_self(arg_expr, ctx) &&
                    !(ctx != NULL && ctx->current_subprogram_is_nonstatic_class_method))
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                        top_reg->bit_64, top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                /* ARCHITECTURAL FIX: Immediately spill argument to stack to prevent
                 * nested function calls from clobbering this value. This ensures that
                 * even if subsequent argument evaluations (which may include nested
                 * function calls) reuse registers, we can restore the correct value. */
            StackNode_t *arg_spill = add_l_t("arg_eval");
            if (arg_spill != NULL && arg_infos != NULL)
            {
                int expected_real_size = arg_infos[arg_num].expected_real_size;
                int is_real_arg = (expected_type == REAL_TYPE);
                int is_xmm = (top_reg->bit_64 != NULL &&
                              strncmp(top_reg->bit_64, "%xmm", 4) == 0);
                int is_single_record_payload = 0;
                if (is_real_arg && expected_real_size == 4 && arg_expr != NULL)
                {
                    struct Expression *raw_arg_expr = arg_expr;
                    while (raw_arg_expr != NULL &&
                        raw_arg_expr->type == EXPR_TYPECAST &&
                        raw_arg_expr->expr_data.typecast_data.target_type == REAL_TYPE &&
                        raw_arg_expr->expr_data.typecast_data.expr != NULL)
                    {
                        raw_arg_expr = raw_arg_expr->expr_data.typecast_data.expr;
                    }
                    is_single_record_payload =
                        (raw_arg_expr != NULL && raw_arg_expr->type == EXPR_RECORD_ACCESS);
                }
                if (is_real_arg && is_xmm)
                {
                    if (expected_real_size == 4)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovss\t%s, -%d(%%rbp)\n",
                            top_reg->bit_64, arg_spill->offset);
                        arg_infos[arg_num].spill_is_single = 1;
                    }
                    else
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, -%d(%%rbp)\n",
                            top_reg->bit_64, arg_spill->offset);
                        arg_infos[arg_num].spill_is_single = 0;
                    }
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        top_reg->bit_64, arg_spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                    arg_infos[arg_num].spill_is_single = is_single_record_payload;
                }
                free_reg(get_reg_stack(), top_reg);
                
                arg_infos[arg_num].reg = NULL;
                arg_infos[arg_num].spill = arg_spill;
                arg_infos[arg_num].expr = arg_expr;
            }
            else if (arg_infos != NULL)
            {
                arginfo_assign_register(&arg_infos[arg_num], top_reg, arg_expr);
            }
            }
            }
        }

        args = args->next;
        if(formal_args != NULL && !(skip_formal_for_self && arg_num == 0))
        {
            formal_args = formal_args->next;
            
            /* CRITICAL VALIDATION: After advancing formal_args, check if the new node is valid.
             * On some platforms, corrupted list nodes may have garbage in their 'next' pointer.
             * We validate the next node before the next iteration to prevent segfaults. */
            if (formal_args != NULL && formal_args->type != LIST_TREE && formal_args->type != LIST_UNSPECIFIED)
            {
                const char *proc_name = procedure_name ? procedure_name : "(unknown)";
                codegen_report_error(ctx,
                    "FATAL: Internal compiler error - corrupted formal_args->next (type=%d) at argument %d for procedure %s. "
                    "This indicates the formal arguments list is not properly NULL-terminated or contains corrupted nodes.",
                    formal_args->type, arg_num, proc_name);
                if (arg_infos != NULL)
                    free(arg_infos);
                return inst_list;
            }
        }
        ++arg_num;
    }

    int next_gpr = arg_start_index;
    int next_sse = 0;
    if (arg_infos != NULL)
    {
        for (int i = 0; i < arg_num; ++i)
        {
            int actual_type = (arg_infos[i].expr != NULL)
                ? expr_get_type_tag(arg_infos[i].expr) : UNKNOWN_TYPE;
            int actual_is_real = (actual_type == REAL_TYPE) ||
                (arg_infos[i].expr != NULL &&
                 (arg_infos[i].expr->type == EXPR_RNUM ||
                  (arg_infos[i].expr->type == EXPR_TYPECAST &&
                   arg_infos[i].expr->expr_data.typecast_data.target_type == REAL_TYPE)));
            int is_extended_real = (arg_infos[i].expected_type == REAL_TYPE &&
                arg_infos[i].expected_real_size == 16 &&
                !arg_infos[i].is_pointer_like);
            int use_sse = ((arg_infos[i].expected_type == REAL_TYPE) || actual_is_real) &&
                !arg_infos[i].is_pointer_like && !is_extended_real;
            if (g_current_codegen_abi == KGPC_TARGET_ABI_WINDOWS && is_external_c_function)
            {
                /* Windows x64 C ABI: argument slots are positional across classes.
                 * The Nth argument uses RCX/RDX/R8/R9 or XMM0-3 based on its type. */
                int reg_slot = arg_start_index + i;
                if (use_sse)
                {
                    arg_infos[i].assigned_class = ARG_CLASS_SSE;
                    if (reg_slot < max_sse_regs)
                        arg_infos[i].assigned_index = reg_slot;
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
                else
                {
                    arg_infos[i].assigned_class = ARG_CLASS_INT;
                    if (!is_extended_real && reg_slot < max_int_regs)
                        arg_infos[i].assigned_index = reg_slot;
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
            }
            else
            {
                /* SysV: separate register files for SSE and INT */
                if (use_sse)
                {
                    arg_infos[i].assigned_class = ARG_CLASS_SSE;
                    if (next_sse < max_sse_regs)
                    {
                        arg_infos[i].assigned_index = next_sse++;
                    }
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
                else
                {
                    arg_infos[i].assigned_class = ARG_CLASS_INT;
                    if (!is_extended_real && next_gpr < max_int_regs)
                    {
                        arg_infos[i].assigned_index = next_gpr++;
                    }
                    else
                    {
                        arg_infos[i].assigned_index = -1;
                        arg_infos[i].pass_via_stack = 1;
                        arg_infos[i].stack_slot = stack_slot_count++;
                    }
                }
            }
        }
    }

    if (stack_slot_count > 0 || codegen_target_is_windows())
    {
        /* Windows x64 requires the caller to reserve 32 bytes of shadow space for
         * *every* call, even when all args fit in registers. Stack-passed args are
         * placed after that shadow space. */
        int shadow_space = codegen_target_is_windows() ? 32 : 0;
        int stack_bytes = 0;
        if (arg_infos != NULL)
        {
            for (int i = 0; i < arg_num; ++i)
            {
                if (!arg_infos[i].pass_via_stack)
                    continue;
                int stack_size = arg_infos[i].stack_size > 0 ?
                    arg_infos[i].stack_size : CODEGEN_POINTER_SIZE_BYTES;
                if (stack_size >= 16)
                    stack_bytes = codegen_expr_align_to(stack_bytes, 16);
                arg_infos[i].stack_offset = shadow_space + stack_bytes;
                stack_bytes += stack_size;
            }
        }
        else
        {
            stack_bytes = stack_slot_count * CODEGEN_POINTER_SIZE_BYTES;
        }
        /* Alignment padding must be placed AFTER the stack arguments.
         * Stack arguments start immediately after the Windows shadow space (if any),
         * i.e. at offset 32(%rsp) for the 5th argument. */
        int padding = codegen_expr_align_to(stack_bytes, REQUIRED_OFFSET) - stack_bytes;
        int total_stack_area = shadow_space + stack_bytes + padding;
        if (total_stack_area > 0)
        {
            snprintf(buffer, sizeof(buffer), "\tsubq\t$%d, %%rsp\n", total_stack_area);
            inst_list = add_inst(inst_list, buffer);
            if (ctx != NULL)
                ctx->pending_stack_arg_bytes += total_stack_area;
        }
    }

    /* Pre-pass: emit kgpc_move for stack-passed Extended (10-byte) arguments
     * BEFORE loading any register-passed arguments.  kgpc_move forwards to
     * memmove and clobbers caller-saved registers including the SysV argument
     * registers (rdi/rsi/rdx/rcx/r8/r9).  If a later iteration of the main
     * loop loads a register-passed arg (e.g. %rsi = def) and then the Extended
     * arg's kgpc_move runs, the register-passed arg's value is destroyed
     * before the actual call.  By doing all clobbering Extended copies first
     * (their source is already spilled to the stack frame and the destination
     * is rsp-relative, so they don't depend on any other arg setup), the
     * subsequent register-arg loads happen after the last clobber and reach
     * the callee intact. */
    if (arg_infos != NULL)
    {
        for (int i = arg_num - 1; i >= 0; --i)
        {
            int expected_type = arg_infos[i].expected_type;
            int expected_real_size = arg_infos[i].expected_real_size;
            int pass_on_stack = arg_infos[i].pass_via_stack;
            int is_ptr_like = arg_infos[i].is_pointer_like;
            if (!pass_on_stack)
                continue;
            if (arg_infos[i].spill == NULL)
                continue;
            if (expected_type != REAL_TYPE || expected_real_size != 16 || is_ptr_like)
                continue;

            Register_t *src_addr = get_free_reg(get_reg_stack(), &inst_list);
            if (src_addr == NULL)
                return inst_list;
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                arg_infos[i].spill->offset, src_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            if (codegen_target_is_windows())
            {
                Register_t *dst_addr = get_free_reg(get_reg_stack(), &inst_list);
                if (dst_addr == NULL)
                {
                    free_reg(get_reg_stack(), src_addr);
                    return inst_list;
                }
                snprintf(buffer, sizeof(buffer), "\tleaq\t%d(%%rsp), %s\n",
                    arg_infos[i].stack_offset, dst_addr->bit_64);
                inst_list = add_inst(inst_list, buffer);
                { Register_t *u[] = {dst_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
                { Register_t *u[] = {src_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
                free_reg(get_reg_stack(), dst_addr);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t%d(%%rsp), %%rdi\n",
                    arg_infos[i].stack_offset);
                inst_list = add_inst(inst_list, buffer);
                { Register_t *u[] = {src_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
                inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
            }
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
            free_arg_regs();
            free_reg(get_reg_stack(), src_addr);
            /* Mark this arg as already emitted so the main loop skips it. */
            arg_infos[i].emitted_via_prepass = 1;
        }
    }

    for (int i = arg_num - 1; i >= 0; --i)
    {
        int expected_type = (arg_infos != NULL) ? arg_infos[i].expected_type : UNKNOWN_TYPE;
        int expected_real_size = (arg_infos != NULL) ? arg_infos[i].expected_real_size : 0;
        int actual_type = (arg_infos != NULL && arg_infos[i].expr != NULL)
            ? expr_get_type_tag(arg_infos[i].expr) : UNKNOWN_TYPE;
        int is_ptr_like = (arg_infos != NULL && arg_infos[i].is_pointer_like);
        /* When an argument is passed by reference (var/out/array), the value
         * stored in the spill slot is a pointer (address), not the underlying
         * integer value.  Sign-extending a 64-bit pointer via movslq would
         * truncate it, so suppress the sign-extension for pointer-like args.
         *
         * Sign-extend (movslq) is required when a signed 32-bit value
         * (Integer / LongInt) is passed to a wider signed-or-unsigned 64-bit
         * parameter (Int64 / QWord).  Without this, a spilled longint of -1
         * is zero-extended to 0x00000000FFFFFFFF (4294967295) instead of
         * 0xFFFFFFFFFFFFFFFF, breaking FPC semantics. */
        int actual_is_s32 = (actual_type == INT_TYPE || actual_type == LONGINT_TYPE);
        int expected_is_wider_int = (expected_type == LONGINT_TYPE ||
                                     expected_type == INT64_TYPE ||
                                     expected_type == QWORD_TYPE);
        int needs_int_to_long = (actual_is_s32 && expected_is_wider_int && !is_ptr_like);
        int pass_on_stack = (arg_infos != NULL && arg_infos[i].pass_via_stack);
        if (arg_infos != NULL && arg_infos[i].emitted_via_prepass)
            continue;

        int reg_index = arg_start_index + i;
        if (!pass_on_stack && arg_infos != NULL && arg_infos[i].assigned_index >= 0)
            reg_index = arg_infos[i].assigned_index;

        if (!pass_on_stack)
        {
            if (arg_infos != NULL && arg_infos[i].assigned_class == ARG_CLASS_SSE)
                arg_reg_char = current_arg_reg_xmm(reg_index);
            else
                arg_reg_char = get_arg_reg64_num(reg_index);
            if (arg_reg_char == NULL)
            {
                fprintf(stderr, "ERROR: Could not get arg register: %d\n", i);
                exit(1);
            }

            if (arg_infos != NULL)
            {
                RegisterId_t arg_reg_id = REG_INVALID;
                if (arg_infos[i].assigned_class == ARG_CLASS_INT)
                    arg_reg_id = codegen_arg_reg_id_num(reg_index);

                for (int j = 0; j < i; ++j)
                {
                    if (arg_reg_id != REG_INVALID && arg_infos[j].reg != NULL &&
                        arg_infos[j].reg->reg_id == arg_reg_id)
                    {
                        StackNode_t *spill = add_l_t("arg_spill");
                        if (arg_infos[j].assigned_class == ARG_CLASS_SSE &&
                            arg_infos[j].expected_type == REAL_TYPE &&
                            arg_infos[j].expected_real_size == 4)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovss\t%s, -%d(%%rbp)\n",
                                arg_infos[j].reg->bit_64, spill->offset);
                            arg_infos[j].spill_is_single = 1;
                        }
                        else if (arg_infos[j].assigned_class == ARG_CLASS_SSE &&
                            arg_infos[j].expected_type == REAL_TYPE)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, -%d(%%rbp)\n",
                                arg_infos[j].reg->bit_64, spill->offset);
                            arg_infos[j].spill_is_single = 0;
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                                arg_infos[j].reg->bit_64, spill->offset);
                            arg_infos[j].spill_is_single = 0;
                        }
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), arg_infos[j].reg);
                        arg_infos[j].reg = NULL;
                        arg_infos[j].spill = spill;
                    }
                }
            }
        }

        Register_t *stored_reg = arg_infos != NULL ? arg_infos[i].reg : NULL;
        struct Expression *source_expr = arg_infos != NULL ? arg_infos[i].expr : NULL;
        if (stored_reg != NULL)
        {
            if (expected_type == REAL_TYPE && expected_real_size == 4 &&
                arg_infos != NULL && arg_infos[i].assigned_class == ARG_CLASS_SSE)
            {
                if (stored_reg->bit_64 != NULL &&
                    strncmp(stored_reg->bit_64, "%xmm", 4) == 0)
                {
                    { Register_t *u[] = {stored_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovss\t%0, %xmm0\n"); }
                }
                else
                {
                    /* Direct GP-backed Single arguments are carried as 32-bit payloads. */
                    {
                        char buffer_tmpl[128];
                        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovd\t%s, %%xmm0\n", stored_reg->bit_32);
                        inst_list = add_inst_du(inst_list, ctx, NULL, 0, NULL, 0, buffer_tmpl);
                    }
                }
                if (pass_on_stack)
                {
                    char stack_dest[64];
                    snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                    snprintf(buffer, sizeof(buffer), "\tmovss\t%%xmm0, %s\n", stack_dest);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovss\t%%xmm0, %s\n", arg_reg_char);
                }
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), stored_reg);
                continue;
            }
            if (expected_type == REAL_TYPE && expected_real_size == 8 &&
                arg_infos != NULL && arg_infos[i].assigned_class == ARG_CLASS_SSE)
            {
                struct Expression *raw_source_expr = source_expr;
                while (raw_source_expr != NULL &&
                    raw_source_expr->type == EXPR_TYPECAST &&
                    raw_source_expr->expr_data.typecast_data.target_type == REAL_TYPE &&
                    raw_source_expr->expr_data.typecast_data.expr != NULL)
                {
                    raw_source_expr = raw_source_expr->expr_data.typecast_data.expr;
                }
                if (raw_source_expr != NULL && raw_source_expr->type == EXPR_RECORD_ACCESS)
                {
                    long long field_size = codegen_record_field_effective_size(raw_source_expr, ctx);
                    if (field_size == 4)
                    {
                        {
                            char buffer_tmpl[128];
                            snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovd\t%s, %%xmm0\n", stored_reg->bit_32);
                            inst_list = add_inst_du(inst_list, ctx, NULL, 0, NULL, 0, buffer_tmpl);
                        }
                        inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
                        if (pass_on_stack)
                        {
                            char stack_dest[64];
                            snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, %s\n", stack_dest);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, %s\n", arg_reg_char);
                        }
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), stored_reg);
                        continue;
                    }
                }
            }
            if (needs_int_to_long && arg_infos != NULL &&
                arg_infos[i].assigned_class == ARG_CLASS_INT)
            {
                inst_list = codegen_sign_extend32_to64(inst_list,
                    stored_reg->bit_32, stored_reg->bit_64);
            }
            if (pass_on_stack)
            {
                char stack_dest[64];
                snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                {
                    char buffer_tmpl[128];
                    snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", stack_dest);
                    Register_t *u[] = {stored_reg};
                    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                }
            }
            else
            {
                {
                    char buffer_tmpl[128];
                    snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg_reg_char);
                    Register_t *u[] = {stored_reg};
                    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                }
            }
            free_reg(get_reg_stack(), stored_reg);
        }
        else if (arg_infos != NULL && arg_infos[i].spill != NULL)
        {
            Register_t *temp_reg = NULL;
            if (expected_type == REAL_TYPE && expected_real_size == 16 &&
                !(arg_infos != NULL && arg_infos[i].is_pointer_like))
            {
                if (!pass_on_stack)
                {
                    codegen_report_error(ctx,
                        "ERROR: Extended arguments currently require stack passing.");
                    return inst_list;
                }

                Register_t *src_addr = get_free_reg(get_reg_stack(), &inst_list);
                if (src_addr == NULL)
                    return inst_list;
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    arg_infos[i].spill->offset, src_addr->bit_64);
                inst_list = add_inst(inst_list, buffer);
                if (codegen_target_is_windows())
                {
                    Register_t *dst_addr = get_free_reg(get_reg_stack(), &inst_list);
                    if (dst_addr == NULL)
                    {
                        free_reg(get_reg_stack(), src_addr);
                        return inst_list;
                    }
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%d(%%rsp), %s\n",
                        arg_infos[i].stack_offset, dst_addr->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    { Register_t *u[] = {dst_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n"); }
                    { Register_t *u[] = {src_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n"); }
                    inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
                    free_reg(get_reg_stack(), dst_addr);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%d(%%rsp), %%rdi\n",
                        arg_infos[i].stack_offset);
                    inst_list = add_inst(inst_list, buffer);
                    { Register_t *u[] = {src_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n"); }
                    inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
                }
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                free_arg_regs();
                free_reg(get_reg_stack(), src_addr);
                continue;
            }
            if (expected_type == REAL_TYPE && expected_real_size == 4 &&
                arg_infos[i].assigned_class == ARG_CLASS_SSE)
            {
                int source_is_single_payload = arg_infos[i].spill_is_single;

                if (source_is_single_payload)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovss\t-%d(%%rbp), %%xmm0\n",
                        arg_infos[i].spill->offset);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    temp_reg = get_free_reg(get_reg_stack(), &inst_list);
                    if (temp_reg == NULL)
                        return inst_list;
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        arg_infos[i].spill->offset, temp_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    { Register_t *u[] = {temp_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %xmm0\n"); }
                    inst_list = add_inst(inst_list, "\tcvtsd2ss\t%xmm0, %xmm0\n");
                }
                if (pass_on_stack)
                {
                    char stack_dest[64];
                    snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                    snprintf(buffer, sizeof(buffer), "\tmovss\t%%xmm0, %s\n", stack_dest);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovss\t%%xmm0, %s\n", arg_reg_char);
                }
                inst_list = add_inst(inst_list, buffer);
                if (temp_reg != NULL)
                    free_reg(get_reg_stack(), temp_reg);
                continue;
            }
            /* Simple integer/pointer arguments can be loaded directly from the
             * spill slot into the destination register. This avoids grabbing a
             * temporary argument register (e.g., %r8) that might already hold a
             * later argument when emitting Windows calls. */
            if (!pass_on_stack && arg_infos[i].assigned_class == ARG_CLASS_INT)
            {
                if (needs_int_to_long)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovslq\t-%d(%%rbp), %s\n",
                        arg_infos[i].spill->offset, arg_reg_char);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        arg_infos[i].spill->offset, arg_reg_char);
                }
                inst_list = add_inst(inst_list, buffer);
                continue;
            }

            if (expected_type == REAL_TYPE && expected_real_size == 8 &&
                arg_infos[i].assigned_class == ARG_CLASS_SSE)
            {
                struct Expression *raw_source_expr = arg_infos[i].expr;
                while (raw_source_expr != NULL &&
                    raw_source_expr->type == EXPR_TYPECAST &&
                    raw_source_expr->expr_data.typecast_data.target_type == REAL_TYPE &&
                    raw_source_expr->expr_data.typecast_data.expr != NULL)
                {
                    raw_source_expr = raw_source_expr->expr_data.typecast_data.expr;
                }
                if (raw_source_expr != NULL && raw_source_expr->type == EXPR_RECORD_ACCESS)
                {
                    long long field_size = codegen_record_field_effective_size(raw_source_expr, ctx);
                    if (field_size == 4)
                    {
                        temp_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (temp_reg == NULL)
                            return inst_list;
                        snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n",
                            arg_infos[i].spill->offset, temp_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
                        {
                            char buffer_tmpl[128];
                            snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovd\t%s, %%xmm0\n", temp_reg->bit_32);
                            inst_list = add_inst_du(inst_list, ctx, NULL, 0, NULL, 0, buffer_tmpl);
                        }
                        inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
                        if (pass_on_stack)
                        {
                            char stack_dest[64];
                            snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, %s\n", stack_dest);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, %s\n", arg_reg_char);
                        }
                        inst_list = add_inst(inst_list, buffer);
                        free_reg(get_reg_stack(), temp_reg);
                        continue;
                    }
                }
            }

            if (needs_int_to_long && arg_infos[i].assigned_class == ARG_CLASS_INT)
            {
                temp_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (temp_reg == NULL)
                    return inst_list;
                snprintf(buffer, sizeof(buffer), "\tmovslq\t-%d(%%rbp), %s\n",
                    arg_infos[i].spill->offset, temp_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                temp_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (temp_reg == NULL)
                    return inst_list;
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    arg_infos[i].spill->offset, temp_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            if (pass_on_stack)
            {
                char stack_dest[64];
                snprintf(stack_dest, sizeof(stack_dest), "%d(%%rsp)", arg_infos[i].stack_offset);
                {
                    char buffer_tmpl[128];
                    snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", stack_dest);
                    Register_t *u[] = {temp_reg};
                    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                }
            }
            else
            {
                {
                    char buffer_tmpl[128];
                    snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t%%0, %s\n", arg_reg_char);
                    Register_t *u[] = {temp_reg};
                    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
                }
            }
            free_reg(get_reg_stack(), temp_reg);
        }
        else
        {
            const char *proc_name = procedure_name ? procedure_name : "(unknown)";
            fprintf(stderr,
                    "ERROR: Missing evaluated value for argument %d in call to %s (%s).\n",
                    i,
                    proc_name,
                    describe_expression_kind(source_expr));
            exit(1);
        }
    }

    /* Windows x64 varargs ABI: float/double arguments passed in XMM registers
     * must also be mirrored into the corresponding integer register.
     * The callee uses va_arg which reads from integer registers, so the
     * caller must place the value in both locations. */
    if (g_current_codegen_abi == KGPC_TARGET_ABI_WINDOWS && is_varargs_function && arg_infos != NULL)
    {
        for (int i = 0; i < arg_num; ++i)
        {
            if (arg_infos[i].assigned_class == ARG_CLASS_SSE &&
                !arg_infos[i].pass_via_stack &&
                arg_infos[i].assigned_index >= 0)
            {
                int reg_slot = arg_infos[i].assigned_index;
                const char *xmm_reg = current_arg_reg_xmm(reg_slot);
                const char *int_reg = get_arg_reg64_num(reg_slot);
                assert(xmm_reg != NULL);
                assert(int_reg != NULL);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                    xmm_reg, int_reg);
                inst_list = add_inst(inst_list, buffer);
            }
        }
    }

    free(arg_infos);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_cleanup_call_stack(ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (ctx != NULL && ctx->pending_stack_arg_bytes > 0)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\n\taddq\t$%d, %%rsp\n", ctx->pending_stack_arg_bytes);
        inst_list = add_inst(inst_list, buffer);
        ctx->pending_stack_arg_bytes = 0;
    }
    free_arg_regs();
    return inst_list;
}

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
