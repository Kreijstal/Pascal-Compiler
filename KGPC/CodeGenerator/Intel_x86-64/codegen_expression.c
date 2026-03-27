/*
    Damon Gwinn
    Code generation for expressions
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#if defined(__GLIBC__) || (defined(__APPLE__) && defined(__MACH__)) || \
    defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define HAVE_EXECINFO 1
#include <execinfo.h>
#endif

/* Forward declarations for unresolved method stubs — implementation after includes. */

#include "codegen.h"
#include "codegen_expression.h"
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
#include "../../identifier_utils.h"
#include "../../format_arg.h"


/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_SIZEOF_RECURSION_LIMIT 32

/* Helper functions for transitioning from legacy type fields to KgpcType */

/* Helper function to check if a node is a record type */
static inline int codegen_node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Collect method labels that need stubs (referenced via @MethodName but
 * body not available in the compilation unit).  Emitted as .weak stubs
 * in the final pass so they don't conflict with real definitions. */
static ListNode_t *g_unresolved_method_stubs = NULL;

void codegen_add_unresolved_method_stub(const char *label)
{
    if (label == NULL) return;
    for (ListNode_t *n = g_unresolved_method_stubs; n != NULL; n = n->next)
        if (n->cur != NULL && strcmp((const char *)n->cur, label) == 0)
            return;
    char *dup = strdup(label);
    if (dup != NULL) {
        ListNode_t *node = calloc(1, sizeof(ListNode_t));
        if (node != NULL) {
            node->cur = dup;
            if (g_unresolved_method_stubs == NULL)
                g_unresolved_method_stubs = node;
            else
                PushListNodeBack(g_unresolved_method_stubs, node);
        } else {
            free(dup);
        }
    }
}

void codegen_emit_unresolved_method_stubs(FILE *out, ListNode_t *emitted_subprograms)
{
    (void)out;
    (void)emitted_subprograms;
    /* Stubs removed: unresolved method references should produce linker
     * errors so that codegen bugs are caught instead of hidden. */
    ListNode_t *cur = g_unresolved_method_stubs;
    while (cur != NULL) {
        ListNode_t *next = cur->next;
        free(cur->cur);
        free(cur);
        cur = next;
    }
    g_unresolved_method_stubs = NULL;
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* codegen_get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
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

static KgpcType *codegen_function_call_return_type_from_expr(
    const struct Expression *expr)
{
    KgpcType *call_type = NULL;
    KgpcType *ret_type = NULL;
    const char *ret_id = NULL;
    static KgpcType *cached_shortstring = NULL;
    static KgpcType *cached_ansistring = NULL;

    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return NULL;

    if (expr->expr_data.function_call_data.mangled_id != NULL &&
        strncmp(expr->expr_data.function_call_data.mangled_id, "kgpc_", 5) == 0 &&
        expr->resolved_kgpc_type != NULL)
    {
        return expr->resolved_kgpc_type;
    }

    call_type = expr->expr_data.function_call_data.call_kgpc_type;
    if (call_type == NULL &&
        expr->expr_data.function_call_data.resolved_func != NULL)
    {
        call_type = expr->expr_data.function_call_data.resolved_func->type;
    }

    /* Builtin lowering sometimes rewrites a call directly to a runtime helper
     * and clears the semantic call cache. In that case, prefer the semchecked
     * expression result type over falling back to unrelated source-level
     * declarations that happen to share the original identifier. */
    if (call_type == NULL && expr->resolved_kgpc_type != NULL)
        return expr->resolved_kgpc_type;

    if (call_type == NULL || call_type->kind != TYPE_KIND_PROCEDURE)
        return NULL;

    ret_type = kgpc_type_get_return_type(call_type);
    if (ret_type != NULL)
        return ret_type;

    ret_id = call_type->info.proc_info.return_type_id;
    if (ret_id == NULL && call_type->info.proc_info.definition != NULL)
        ret_id = call_type->info.proc_info.definition->tree_data.subprogram_data.return_type_id;
    if (ret_id == NULL)
        return NULL;

    if (pascal_identifier_equals(ret_id, "ShortString"))
    {
        if (cached_shortstring == NULL)
            cached_shortstring = create_primitive_type(SHORTSTRING_TYPE);
        return cached_shortstring;
    }
    if (pascal_identifier_equals(ret_id, "AnsiString") ||
        pascal_identifier_equals(ret_id, "String"))
    {
        if (cached_ansistring == NULL)
            cached_ansistring = create_primitive_type(STRING_TYPE);
        return cached_ansistring;
    }
    return NULL;
}

static long long codegen_sizeof_type_tag(int type_tag);
static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth);
static struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr,
    CodeGenContext *ctx);
static long long codegen_record_field_effective_size(struct Expression *expr, CodeGenContext *ctx);
static struct RecordField *codegen_find_unique_record_field(SymTab_t *symtab,
    const char *field_id, struct RecordType **out_record);
struct RecordField *codegen_lookup_with_field(CodeGenContext *ctx,
    const char *field_id, struct RecordType **out_record);
long long codegen_array_elem_size_from_field(struct RecordField *field, CodeGenContext *ctx);
static int codegen_get_indexable_element_size(struct Expression *array_expr,
    CodeGenContext *ctx, long long *out_size);
static int codegen_collect_nested_array_access_chain(struct Expression *expr,
    struct Expression **base_expr_out, struct Expression **indices_out, int *index_count_out);
static ListNode_t *codegen_emit_linearized_array_address(struct Expression *base_expr,
    struct Expression **indices, int index_count, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);
static struct RecordType *codegen_expr_record_type(const struct Expression *expr, SymTab_t *symtab)
{
    if (expr == NULL)
        return NULL;
    if (expr->record_type != NULL)
        return expr->record_type;
    if (expr->type == EXPR_VAR_ID && symtab != NULL && expr->expr_data.id != NULL)
    {
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, symtab, expr->expr_data.id) != 0 && var_node != NULL)
        {
            struct RecordType *rec = codegen_get_record_type_from_node(var_node);
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
    if (expr->type == EXPR_TYPECAST && symtab != NULL)
    {
        const char *target_id = expr->expr_data.typecast_data.target_type_id;
        if (target_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, target_id) != 0 && type_node != NULL)
            {
                struct RecordType *rec = codegen_get_record_type_from_node(type_node);
                if (rec != NULL)
                    return rec;
                if (type_node->type != NULL)
                {
                    if (kgpc_type_is_record(type_node->type))
                        return kgpc_type_get_record(type_node->type);
                    if (kgpc_type_is_pointer(type_node->type) &&
                        type_node->type->info.points_to != NULL &&
                        kgpc_type_is_record(type_node->type->info.points_to))
                        return kgpc_type_get_record(type_node->type->info.points_to);
                }
            }
        }
    }
    if (expr->type == EXPR_FUNCTION_CALL && symtab != NULL)
    {
        const char *call_id = expr->expr_data.function_call_data.id;
        if (call_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, call_id) != 0 && type_node != NULL)
            {
                struct RecordType *rec = codegen_get_record_type_from_node(type_node);
                if (rec != NULL)
                    return rec;
                if (type_node->type != NULL)
                {
                    if (kgpc_type_is_record(type_node->type))
                        return kgpc_type_get_record(type_node->type);
                    if (kgpc_type_is_pointer(type_node->type) &&
                        type_node->type->info.points_to != NULL &&
                        kgpc_type_is_record(type_node->type->info.points_to))
                        return kgpc_type_get_record(type_node->type->info.points_to);
                }
            }
        }
    }
    if (expr->type == EXPR_RECORD_ACCESS && symtab != NULL)
    {
        if (expr->resolved_kgpc_type != NULL && kgpc_type_is_record(expr->resolved_kgpc_type))
            return kgpc_type_get_record(expr->resolved_kgpc_type);

        struct Expression *base_expr = expr->expr_data.record_access_data.record_expr;
        const char *field_id = expr->expr_data.record_access_data.field_id;
        if (base_expr != NULL && field_id != NULL)
        {
            struct RecordType *base_record = codegen_expr_record_type(base_expr, symtab);
            if (base_record != NULL)
            {
                struct RecordField *field = semcheck_find_class_field_including_hidden(
                    symtab, base_record, field_id, NULL);
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
                            struct RecordType *rec = codegen_get_record_type_from_node(type_node);
                            if (rec != NULL)
                                return rec;
                            if (type_node->type != NULL && kgpc_type_is_record(type_node->type))
                                return kgpc_type_get_record(type_node->type);
                        }
                    }
                }
            }
        }
    }

    KgpcType *expr_type = expr_get_kgpc_type((struct Expression *)expr);
    if (expr_type != NULL)
    {
        if (kgpc_type_is_record(expr_type))
            return kgpc_type_get_record(expr_type);
        if (kgpc_type_is_pointer(expr_type) && expr_type->info.points_to != NULL &&
            kgpc_type_is_record(expr_type->info.points_to))
            return kgpc_type_get_record(expr_type->info.points_to);
    }

    if (expr->pointer_subtype_id != NULL && symtab != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindSymbol(&target_node, symtab, expr->pointer_subtype_id) != 0 && target_node != NULL)
            return codegen_get_record_type_from_node(target_node);
    }

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL && symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL)
        {
            struct RecordType *record = codegen_get_record_type_from_node(node);
            if (record != NULL)
                return record;
            if (node->type != NULL && kgpc_type_is_pointer(node->type) &&
                node->type->info.points_to != NULL &&
                kgpc_type_is_record(node->type->info.points_to))
            {
                return kgpc_type_get_record(node->type->info.points_to);
            }
        }
    }

    return NULL;
}

static int codegen_expr_is_type_identifier(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL || ctx->symtab == NULL ||
        expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
        return 0;

    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) == 0 || node == NULL)
        return 0;
    return node->hash_type == HASHTYPE_TYPE;
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

static int codegen_expr_needs_class_method_vmt_self(const struct Expression *expr,
    CodeGenContext *ctx)
{
    struct RecordType *record = NULL;

    if (expr == NULL || ctx == NULL)
        return 0;
    if (codegen_expr_is_type_identifier(expr, ctx))
        return 0;

    record = codegen_expr_record_type(expr, ctx->symtab);
    return (record != NULL && record_type_is_class(record));
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

static int codegen_expr_is_shortstring_array_local(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if ((expr->array_element_size == 2) ||
        (expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
        return 0;
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL && alias->is_shortstring)
            return 1;
    }
    if (expr->is_array_expr &&
        expr->array_element_type == CHAR_TYPE &&
        expr_get_array_lower_bound(expr) == 0 &&
        expr_get_array_upper_bound(expr) == 255)
    {
        if (expr->resolved_kgpc_type == NULL ||
            kgpc_type_get_type_alias(expr->resolved_kgpc_type) == NULL)
            return 1;
    }
    return 0;
}

static int codegen_expr_has_widechar_array_metadata(const struct Expression *expr)
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

static int codegen_expr_is_shortstring_value(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (codegen_expr_has_widechar_array_metadata(expr))
        return 0;

    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
        return 1;

    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (expr_type != NULL)
    {
        if (kgpc_type_is_shortstring(expr_type))
            return 1;
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr_type);
        if (alias != NULL && alias->is_shortstring)
            return 1;
        if (kgpc_type_is_array(expr_type) &&
            expr_type->type_alias != NULL &&
            expr_type->type_alias->is_shortstring)
        {
            return 1;
        }
    }

    if (codegen_expr_is_shortstring_array_local(expr))
        return 1;

    if (expr_type != NULL && kgpc_type_is_array(expr_type))
    {
        KgpcType *elem_type = kgpc_type_get_array_element_type(expr_type);
        if (elem_type != NULL &&
            elem_type->kind == TYPE_KIND_PRIMITIVE &&
            elem_type->info.primitive_type_tag == CHAR_TYPE)
        {
            if (codegen_expr_has_widechar_array_metadata(expr))
                return 0;
            int start = 0;
            int end = -1;
            if (kgpc_type_get_array_bounds(expr_type, &start, &end) == 0 &&
                start == 0 && end >= 0 && end <= 255)
                return 1;
        }
    }

    if (expr->is_array_expr &&
        expr->array_element_type == CHAR_TYPE &&
        expr->array_element_size != 2 &&
        (expr->array_element_type_id == NULL ||
         (!pascal_identifier_equals(expr->array_element_type_id, "WideChar") &&
          !pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))) &&
        expr_get_array_lower_bound(expr) == 0)
    {
        int upper = expr_get_array_upper_bound(expr);
        if (upper >= 0 && upper <= 255)
            return 1;
    }

    return 0;
}

static int codegen_expr_is_char_array_like(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
        return 1;
    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (expr_type != NULL && kgpc_type_is_array(expr_type) &&
        expr_type->info.array_info.element_type != NULL &&
        expr_type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
        expr_type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
        return 1;
    return 0;
}

int codegen_expr_is_shortstring_value_ctx(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr != NULL && expr->type == EXPR_FUNCTION_CALL && ctx != NULL)
    {
        KgpcType *ret_type = NULL;
        const char *ret_id = NULL;

        ret_type = codegen_function_call_return_type_from_expr(expr);
        if (ret_type != NULL)
        {
            if (kgpc_type_is_shortstring(ret_type) ||
                (ret_type->type_alias != NULL && ret_type->type_alias->is_shortstring))
                return 1;
        }

        if (ret_type == NULL)
        {
            HashNode_t *call_node = NULL;
            KgpcType *call_type = codegen_resolve_function_call_type(ctx, expr, &call_node);
            if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE)
            {
                ret_type = kgpc_type_get_return_type(call_type);
                if (ret_type != NULL && kgpc_type_is_shortstring(ret_type))
                    return 1;
                ret_id = call_type->info.proc_info.return_type_id;
                if (ret_id == NULL && call_type->info.proc_info.definition != NULL)
                    ret_id = call_type->info.proc_info.definition->tree_data.subprogram_data.return_type_id;
                if (ret_id != NULL && pascal_identifier_equals(ret_id, "ShortString"))
                    return 1;
            }
            if (call_node != NULL && call_node->type != NULL &&
                call_node->type->kind == TYPE_KIND_PROCEDURE)
            {
                ret_type = kgpc_type_get_return_type(call_node->type);
                if (ret_type != NULL && kgpc_type_is_shortstring(ret_type))
                    return 1;
                ret_id = call_node->type->info.proc_info.return_type_id;
                if (ret_id == NULL && call_node->type->info.proc_info.definition != NULL)
                    ret_id = call_node->type->info.proc_info.definition
                        ->tree_data.subprogram_data.return_type_id;
                if (ret_id != NULL && pascal_identifier_equals(ret_id, "ShortString"))
                    return 1;
            }
        }
    }

    if (codegen_expr_is_shortstring_value(expr))
        return 1;
    if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 && node != NULL && node->type != NULL)
        {
            if (kgpc_type_is_shortstring(node->type))
                return 1;
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias != NULL && alias->is_shortstring)
                return 1;
            if (kgpc_type_is_array(node->type))
            {
                KgpcType *elem = kgpc_type_get_array_element_type(node->type);
                if (elem != NULL &&
                    elem->kind == TYPE_KIND_PRIMITIVE &&
                    elem->info.primitive_type_tag == CHAR_TYPE)
                {
                    int start = 0;
                    int end = -1;
                    if (kgpc_type_get_array_bounds(node->type, &start, &end) == 0 &&
                        start == 0 && end >= 0 && end <= 255)
                        return 1;
                }
            }
        }
    }
    return 0;
}

static int codegen_expr_is_char_array_like_ctx(const struct Expression *expr, CodeGenContext *ctx)
{
    if (codegen_expr_is_char_array_like(expr))
        return 1;
    if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 && node != NULL && node->type != NULL)
        {
            if (kgpc_type_is_array(node->type))
            {
                KgpcType *elem = kgpc_type_get_array_element_type(node->type);
                if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
                    elem->info.primitive_type_tag == CHAR_TYPE)
                    return 1;
            }
        }
    }
    return 0;
}

static ListNode_t *codegen_promote_shortstring_reg(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *value_reg)
{
    if (inst_list == NULL || ctx == NULL || value_reg == NULL)
        return inst_list;
    const char *arg_reg64 = current_arg_reg64(0);
    if (arg_reg64 == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", value_reg->bit_64, arg_reg64);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_string");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", value_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_spill_reg64_temp(ListNode_t *inst_list, const Register_t *reg,
    const char *temp_name, StackNode_t **spill_slot)
{
    if (spill_slot != NULL)
        *spill_slot = NULL;
    if (reg == NULL || spill_slot == NULL || temp_name == NULL)
        return inst_list;

    StackNode_t *slot = add_l_t((char *)temp_name);
    if (slot == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", reg->bit_64, slot->offset);
    inst_list = add_inst(inst_list, buffer);
    *spill_slot = slot;
    return inst_list;
}

static ListNode_t *codegen_restore_spilled_reg64(ListNode_t *inst_list, const Register_t *reg,
    StackNode_t *spill_slot)
{
    if (reg == NULL || spill_slot == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", spill_slot->offset, reg->bit_64);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_promote_char_reg_to_string(ListNode_t *inst_list, Register_t *value_reg)
{
    if (value_reg == NULL)
        return inst_list;

    const char *arg_reg32 = current_arg_reg32(0);
    if (arg_reg32 == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", value_reg->bit_32, arg_reg32);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", RETURN_REG_64, value_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    free_arg_regs();
    return inst_list;
}

static int codegen_get_char_array_length(const struct Expression *expr, CodeGenContext *ctx,
    long long *out_len)
{
    if (out_len != NULL)
        *out_len = 0;
    if (expr == NULL)
        return 0;

    long long lower = 0;
    long long upper = -1;
    int found = 0;

    if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
    {
        lower = expr_get_array_lower_bound(expr);
        upper = expr_get_array_upper_bound(expr);
        found = 1;
    }
    else
    {
        KgpcType *kgpc = expr_get_kgpc_type(expr);
        if (kgpc != NULL && kgpc_type_is_array(kgpc) &&
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
                node->type != NULL && kgpc_type_is_array(node->type) &&
                node->type->info.array_info.element_type != NULL &&
                node->type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
                node->type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
            {
                lower = node->type->info.array_info.start_index;
                upper = node->type->info.array_info.end_index;
                found = 1;
            }
        }
    }

    if (!found)
        return 0;
    if (upper < lower)
        return 0;
    if (out_len != NULL)
        *out_len = (upper - lower + 1);
    return 1;
}

static int codegen_array_access_targets_shortstring(const struct Expression *expr, CodeGenContext *ctx)
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
    if (codegen_expr_is_shortstring_value(expr))
        return 1;

    struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
    if (base_expr == NULL)
        return 0;
    if (base_expr->array_element_size == 2 ||
        (base_expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(base_expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(base_expr->array_element_type_id, "UnicodeChar"))))
        return 0;

    KgpcType *base_type = base_expr->resolved_kgpc_type;
    if (base_type == NULL && base_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 && node != NULL)
            base_type = node->type;
    }

    if (base_type != NULL && kgpc_type_is_array(base_type))
    {
        KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
        if (elem_type != NULL)
        {
            if (kgpc_type_is_shortstring(elem_type))
                return 1;
            struct TypeAlias *alias = kgpc_type_get_type_alias(elem_type);
            if (alias != NULL && alias->is_shortstring)
                return 1;
            if (kgpc_type_is_array(elem_type))
            {
                KgpcType *inner = kgpc_type_get_array_element_type(elem_type);
                int start = 0;
                int end = 0;
                if (inner != NULL && kgpc_type_is_char(inner) &&
                    kgpc_type_get_array_bounds(elem_type, &start, &end) == 0 &&
                    start == 0 && end >= 0 && end <= 255)
                {
                    return 1;
                }
            }
        }
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

static StackNode_t *codegen_alloc_temp_bytes(const char *prefix, int size);
static const char *codegen_register_name8(const Register_t *reg);
const char *codegen_register_name16(const Register_t *reg);
static ListNode_t *codegen_store_value_to_stack(ListNode_t *inst_list, Register_t *value_reg,
    int offset, int element_size);
static ListNode_t *codegen_expr_maybe_convert_int_like_to_real(int target_type,
    struct Expression *arg_expr, Register_t *top_reg, ListNode_t *inst_list);
ListNode_t *codegen_expr_with_result(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);
static ListNode_t *codegen_materialize_array_literal(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg);
static ListNode_t *codegen_materialize_array_of_const(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg);
static ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);
static int formal_decl_is_open_array(Tree_t *decl);
static long long codegen_static_array_length(const struct Expression *expr);
static Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx, const char *usage);

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

static const char *codegen_class_typeinfo_label(struct RecordType *record,
    const char *fallback_id)
{
    if (record != NULL && record->type_id != NULL)
        return record->type_id;
    return fallback_id;
}

static ListNode_t *codegen_load_typeinfo_from_instance_ptr(ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *instance_ptr_reg, Register_t **out_reg)
{
    if (out_reg != NULL)
        *out_reg = NULL;

    if (ctx == NULL || instance_ptr_reg == NULL)
        return inst_list;

    Register_t *typeinfo_reg = codegen_try_get_reg(&inst_list, ctx, "class RTTI");
    if (typeinfo_reg == NULL)
        return inst_list;

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
        instance_ptr_reg->bit_64, typeinfo_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t56(%s), %s\n",
        typeinfo_reg->bit_64, typeinfo_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    if (out_reg != NULL)
        *out_reg = typeinfo_reg;
    else
        free_reg(get_reg_stack(), typeinfo_reg);

    return inst_list;
}

static ListNode_t *codegen_load_class_typeinfo(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    if (out_reg != NULL)
        *out_reg = NULL;

    if (expr == NULL || ctx == NULL)
        return inst_list;

    if (!codegen_expr_is_addressable(expr))
    {
        if (codegen_expr_needs_class_method_vmt_self(expr, ctx))
        {
            Register_t *instance_ptr_reg = NULL;
            inst_list = codegen_expr_with_result(expr, inst_list, ctx, &instance_ptr_reg);
            if (codegen_had_error(ctx) || instance_ptr_reg == NULL)
                return inst_list;
            inst_list = codegen_load_typeinfo_from_instance_ptr(inst_list, ctx,
                instance_ptr_reg, out_reg);
            free_reg(get_reg_stack(), instance_ptr_reg);
            return inst_list;
        }
        codegen_report_error(ctx,
            "ERROR: RTTI operations currently require addressable class expressions.");
        return inst_list;
    }

    /* For class variables (which are pointers), we need to:
     * 1. Load the pointer value from the variable
     * 2. Dereference the pointer to get the typeinfo (first field of the instance)
     *
     * For non-class types, we only need:
     * 1. Get address and dereference to get typeinfo
     */
    struct RecordType *expr_record = codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
    int is_class_var = (expr_record != NULL && record_type_is_class(expr_record));

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    Register_t *typeinfo_reg = NULL;
    char buffer[128];
    if (is_class_var)
    {
        Register_t *instance_ptr_reg = codegen_try_get_reg(&inst_list, ctx, "class instance");
        if (instance_ptr_reg == NULL)
        {
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
            addr_reg->bit_64, instance_ptr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_load_typeinfo_from_instance_ptr(inst_list, ctx,
            instance_ptr_reg, &typeinfo_reg);
        free_reg(get_reg_stack(), instance_ptr_reg);
    }
    else
    {
        inst_list = codegen_load_typeinfo_from_instance_ptr(inst_list, ctx,
            addr_reg, &typeinfo_reg);
    }
    free_reg(get_reg_stack(), addr_reg);

    if (typeinfo_reg == NULL || codegen_had_error(ctx))
        return inst_list;

    if (out_reg != NULL)
        *out_reg = typeinfo_reg;
    else
        free_reg(get_reg_stack(), typeinfo_reg);
    return inst_list;
}

static void codegen_move_rtti_args(ListNode_t **inst_list,
    const Register_t *value_reg, const char *target_label)
{
    char buffer[128];
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
        *inst_list = add_inst(*inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rdx\n", target_label);
        *inst_list = add_inst(*inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
        *inst_list = add_inst(*inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rsi\n", target_label);
        *inst_list = add_inst(*inst_list, buffer);
    }
}
/* Helper to check if a formal parameter declaration expects a string type. */
static int formal_decl_expects_string(Tree_t *decl)
{
    if (decl == NULL)
        return 0;

    if (decl->type != TREE_VAR_DECL)
        return 0;

    if (decl->tree_data.var_decl_data.type == STRING_TYPE)
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

/* Return 1 if the mangled call target already expects a char (not a string)
 * for the given argument position.  This prevents the codegen from inserting
 * a spurious kgpc_char_to_string promotion when a user-defined wrapper
 * (e.g. SysUtils.Pos) declares the formal parameter as AnsiString but the
 * semantic checker has already rewritten the call to a char-specific runtime
 * overload.
 *
 * The naming convention for Pos overloads encodes argument types as a
 * two-letter suffix after "kgpc_string_pos_":
 *   first  letter = substr type  (c = char, a = ansistring, s = shortstring)
 *   second letter = value  type  (c = char, a = ansistring, s = shortstring)
 * arg_index 0 corresponds to the first letter, arg_index 1 to the second. */
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
        if (decl->tree_data.var_decl_data.type != UNKNOWN_TYPE)
            return decl->tree_data.var_decl_data.type;
    }
    if (decl->type == TREE_ARR_DECL)
    {
        /* Open array params should not be treated as their element type. */
        return UNKNOWN_TYPE;
    }

    /* Special case: ShortString type identifier */
    if (type_id != NULL && pascal_identifier_equals(type_id, "ShortString"))
        return SHORTSTRING_TYPE;

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

static ListNode_t *codegen_expr_convert_int_like_to_real(ListNode_t *inst_list,
    Register_t *value_reg, int source_type)
{
    if (inst_list == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (source_type == LONGINT_TYPE || source_type == INT64_TYPE || source_type == QWORD_TYPE)
        snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n", value_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tcvtsi2sdl\t%s, %%xmm0\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", value_reg->bit_64);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_expr_maybe_convert_int_like_to_real(int target_type,
    struct Expression *source_expr, Register_t *value_reg, ListNode_t *inst_list)
{
    if (inst_list == NULL || source_expr == NULL || value_reg == NULL)
        return inst_list;

    int source_type = expr_get_type_tag(source_expr);
    int conversion_type = source_type;
    if (target_type == REAL_TYPE && source_type == REAL_TYPE &&
        source_expr->type == EXPR_TYPECAST &&
        source_expr->expr_data.typecast_data.expr != NULL)
    {
        int inner_type = expr_get_type_tag(source_expr->expr_data.typecast_data.expr);
        if (inner_type == INT_TYPE || inner_type == LONGINT_TYPE ||
            inner_type == INT64_TYPE || inner_type == QWORD_TYPE)
            conversion_type = inner_type;
    }
    if (target_type == REAL_TYPE &&
        (conversion_type == INT_TYPE || conversion_type == LONGINT_TYPE ||
         conversion_type == INT64_TYPE || conversion_type == QWORD_TYPE))
    {
        inst_list = codegen_expr_convert_int_like_to_real(inst_list, value_reg, conversion_type);
    }

    return inst_list;
}

static unsigned long codegen_expr_next_temp_suffix(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static int codegen_expr_align_to(int value, int alignment)
{
    if (alignment <= 0)
        return value;
    int remainder = value % alignment;
    if (remainder == 0)
        return value;
    return value + (alignment - remainder);
}

static StackNode_t *codegen_alloc_temp_bytes(const char *prefix, int size)
{
    if (size <= 0)
        size = DOUBLEWORD;
    char label[32];
    snprintf(label, sizeof(label), "%s_%lu", prefix != NULL ? prefix : "temp",
        codegen_expr_next_temp_suffix());
    return add_l_t_bytes(label, size);
}

int codegen_expr_involves_extended(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    if (expr_get_kgpc_type(expr) != NULL && kgpc_type_is_extended(expr_get_kgpc_type(expr)))
        return 1;

    switch (expr->type)
    {
        case EXPR_SIGN_TERM:
            return codegen_expr_involves_extended(expr->expr_data.sign_term);
        case EXPR_ADDOP:
            return codegen_expr_involves_extended(expr->expr_data.addop_data.left_expr) ||
                codegen_expr_involves_extended(expr->expr_data.addop_data.right_term);
        case EXPR_MULOP:
            return codegen_expr_involves_extended(expr->expr_data.mulop_data.left_term) ||
                codegen_expr_involves_extended(expr->expr_data.mulop_data.right_factor);
        case EXPR_TYPECAST:
            return codegen_expr_involves_extended(expr->expr_data.typecast_data.expr);
        default:
            return 0;
    }
}

static ListNode_t *codegen_extended_copy(ListNode_t *inst_list, CodeGenContext *ctx,
    const char *dest_addr, const char *src_addr)
{
    if (dest_addr == NULL || src_addr == NULL)
        return inst_list;

    if (codegen_target_is_windows())
    {
        char buffer[CODEGEN_MAX_INST_BUF];
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_addr);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
    }
    else
    {
        char buffer[CODEGEN_MAX_INST_BUF];
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_addr);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_extended_store_from_reg(ListNode_t *inst_list, CodeGenContext *ctx,
    struct Expression *expr, Register_t *value_reg, const char *dest_addr)
{
    if (value_reg == NULL || dest_addr == NULL)
        return inst_list;

    int source_type = expr_get_type_tag(expr);
    char buffer[CODEGEN_MAX_INST_BUF];
    if (source_type == INT_TYPE || source_type == LONGINT_TYPE ||
        source_type == INT64_TYPE || source_type == QWORD_TYPE)
    {
        const char *src_reg64 = value_reg->bit_64;
        if (source_type == INT_TYPE)
        {
            inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32, value_reg->bit_64);
        }
        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg64);
            inst_list = add_inst(inst_list, buffer);
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_store_extended_from_int64");
        free_arg_regs();
        return inst_list;
    }

    inst_list = codegen_expr_maybe_convert_int_like_to_real(REAL_TYPE, expr, value_reg, inst_list);
    if (codegen_target_is_windows())
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_store_extended_from_bits");
    free_arg_regs();
    return inst_list;
}

static ListNode_t *codegen_materialize_extended_expr_internal(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *dest_addr_reg)
{
    if (expr == NULL || ctx == NULL || dest_addr_reg == NULL)
        return inst_list;

    char buffer[CODEGEN_MAX_INST_BUF];
    StackNode_t *dest_slot = add_l_t("ext_dest_ptr");
    if (dest_slot == NULL)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to allocate spill slot for Extended destination.");
        return inst_list;
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
        dest_addr_reg->bit_64, dest_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    switch (expr->type)
    {
        case EXPR_SIGN_TERM:
        {
            StackNode_t *src_slot = codegen_alloc_temp_bytes("ext_neg", 10);
            StackNode_t *src_ptr_slot = add_l_t("ext_neg_ptr");
            Register_t *src_addr = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_slot == NULL || src_ptr_slot == NULL || src_addr == NULL)
                return inst_list;
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                src_slot->offset, src_addr->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                src_addr->bit_64, src_ptr_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = codegen_materialize_extended_expr_internal(
                expr->expr_data.sign_term, inst_list, ctx, src_addr);
            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rcx\n", dest_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", src_ptr_slot->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdi\n", dest_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n", src_ptr_slot->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_extended_neg");
            free_arg_regs();
            free_reg(get_reg_stack(), src_addr);
            return inst_list;
        }
        case EXPR_ADDOP:
        case EXPR_MULOP:
        {
            int op_type = (expr->type == EXPR_ADDOP) ?
                expr->expr_data.addop_data.addop_type :
                expr->expr_data.mulop_data.mulop_type;
            const char *helper = NULL;
            switch (op_type)
            {
                case PLUS: helper = "kgpc_extended_add"; break;
                case MINUS: helper = "kgpc_extended_sub"; break;
                case STAR: helper = "kgpc_extended_mul"; break;
                case SLASH: helper = "kgpc_extended_div"; break;
                default: break;
            }
            if (helper != NULL)
            {
                struct Expression *left_expr = (expr->type == EXPR_ADDOP) ?
                    expr->expr_data.addop_data.left_expr :
                    expr->expr_data.mulop_data.left_term;
                struct Expression *right_expr = (expr->type == EXPR_ADDOP) ?
                    expr->expr_data.addop_data.right_term :
                    expr->expr_data.mulop_data.right_factor;
                StackNode_t *lhs_slot = codegen_alloc_temp_bytes("ext_lhs", 10);
                StackNode_t *rhs_slot = codegen_alloc_temp_bytes("ext_rhs", 10);
                StackNode_t *lhs_ptr_slot = add_l_t("ext_lhs_ptr");
                StackNode_t *rhs_ptr_slot = add_l_t("ext_rhs_ptr");
                Register_t *lhs_addr = get_reg_with_spill(get_reg_stack(), &inst_list);
                Register_t *rhs_addr = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (lhs_slot == NULL || rhs_slot == NULL || lhs_ptr_slot == NULL ||
                    rhs_ptr_slot == NULL || lhs_addr == NULL || rhs_addr == NULL)
                    return inst_list;
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    lhs_slot->offset, lhs_addr->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    rhs_slot->offset, rhs_addr->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    lhs_addr->bit_64, lhs_ptr_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    rhs_addr->bit_64, rhs_ptr_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                inst_list = codegen_materialize_extended_expr_internal(left_expr, inst_list, ctx, lhs_addr);
                inst_list = codegen_materialize_extended_expr_internal(right_expr, inst_list, ctx, rhs_addr);
                if (codegen_target_is_windows())
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rcx\n", dest_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", lhs_ptr_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r8\n", rhs_ptr_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdi\n", dest_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n", lhs_ptr_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", rhs_ptr_slot->offset);
                    inst_list = add_inst(inst_list, buffer);
                }
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, helper);
                free_arg_regs();
                free_reg(get_reg_stack(), rhs_addr);
                free_reg(get_reg_stack(), lhs_addr);
                return inst_list;
            }
            break;
        }
        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
            {
                struct Expression *inner = expr->expr_data.typecast_data.expr;
                if (codegen_expr_involves_extended(inner))
                    return codegen_materialize_extended_expr_internal(inner, inst_list, ctx, dest_addr_reg);
            }
            break;
        default:
            break;
    }

    if (codegen_expr_involves_extended(expr) && codegen_expr_is_addressable(expr))
    {
        Register_t *src_addr = NULL;
        Register_t *dest_reload = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (dest_reload == NULL)
            return inst_list;
        inst_list = codegen_address_for_expr(expr, inst_list, ctx, &src_addr);
        if (codegen_had_error(ctx) || src_addr == NULL)
            return inst_list;
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
            dest_slot->offset, dest_reload->bit_64);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_extended_copy(inst_list, ctx, dest_reload->bit_64, src_addr->bit_64);
        free_reg(get_reg_stack(), dest_reload);
        free_reg(get_reg_stack(), src_addr);
        return inst_list;
    }

    Register_t *value_reg = NULL;
    Register_t *dest_reload = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (dest_reload == NULL)
        return inst_list;
    inst_list = codegen_expr_with_result(expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL)
        return inst_list;
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
        dest_slot->offset, dest_reload->bit_64);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_extended_store_from_reg(inst_list, ctx, expr, value_reg,
        dest_reload->bit_64);
    free_reg(get_reg_stack(), dest_reload);
    free_reg(get_reg_stack(), value_reg);
    return inst_list;
}

ListNode_t *codegen_materialize_extended_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *dest_addr_reg)
{
    return codegen_materialize_extended_expr_internal(expr, inst_list, ctx, dest_addr_reg);
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
            (alias->set_element_type_id != NULL &&
             (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
              pascal_identifier_equals(alias->set_element_type_id, "AnsiChar"))))
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

ListNode_t *codegen_emit_is_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (out_reg != NULL)
        *out_reg = NULL;

    if (expr == NULL)
        return inst_list;

    const char *target_label = NULL;
    Register_t *target_typeinfo_reg = NULL;

    /* Support dynamic class-reference variables on RHS (Obj is ObjType).
     * This also handles bare field names in FPC RTL method bodies that
     * bypass semcheck — codegen_get_nonlocal resolves them as Self.field. */
    if (ctx != NULL && ctx->symtab != NULL &&
        expr->expr_data.is_data.target_record_type == NULL &&
        expr->expr_data.is_data.target_type_id != NULL)
    {
        HashNode_t *target_node = NULL;
        int found = FindSymbol(&target_node, ctx->symtab,
                               expr->expr_data.is_data.target_type_id);
        int is_dynamic_ref = (found != 0 && target_node != NULL &&
                              target_node->hash_type == HASHTYPE_VAR);
        /* Also treat as dynamic if the name is not in the symtab at all
         * but we're inside a class method (bare field name). */
        if (!is_dynamic_ref && (found == 0 || target_node == NULL) &&
            ctx->current_subprogram_owner_class != NULL &&
            find_label("Self") != NULL)
            is_dynamic_ref = 1;
        if (is_dynamic_ref)
        {
            Register_t *class_ref_reg = NULL;
            struct Expression target_expr;
            memset(&target_expr, 0, sizeof(target_expr));
            target_expr.line_num = expr->line_num;
            target_expr.col_num = expr->col_num;
            target_expr.type = EXPR_VAR_ID;
            target_expr.expr_data.id = expr->expr_data.is_data.target_type_id;
            inst_list = codegen_expr_with_result(&target_expr, inst_list, ctx, &class_ref_reg);
            if (class_ref_reg == NULL)
                return inst_list;
            /* The field value is a class reference (VMT pointer).
             * Extract TYPEINFO from VMT offset 56 (vTypeInfo slot). */
            char ti_buf[128];
            snprintf(ti_buf, sizeof(ti_buf), "\tmovq\t56(%s), %s\n",
                     class_ref_reg->bit_64, class_ref_reg->bit_64);
            inst_list = add_inst(inst_list, ti_buf);
            target_typeinfo_reg = class_ref_reg;
        }
    }

    if (target_typeinfo_reg == NULL)
    {
        target_label = codegen_class_typeinfo_label(
            expr->expr_data.is_data.target_record_type,
            expr->expr_data.is_data.target_type_id);
        if (target_label == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to resolve class type for \"is\" operator.");
            return inst_list;
        }
    }

    Register_t *value_reg = NULL;
    inst_list = codegen_load_class_typeinfo(expr->expr_data.is_data.expr, inst_list, ctx, &value_reg);
    if (value_reg == NULL)
        return inst_list;

    if (target_typeinfo_reg != NULL)
    {
        char buffer[128];
        if (codegen_target_is_windows())
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", target_typeinfo_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", value_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", target_typeinfo_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else
    {
        codegen_move_rtti_args(&inst_list, value_reg, target_label);
    }
    free_reg(get_reg_stack(), value_reg);
    if (target_typeinfo_reg != NULL)
        free_reg(get_reg_stack(), target_typeinfo_reg);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_is");
    free_arg_regs();

    if (out_reg != NULL)
    {
        Register_t *result_reg = codegen_try_get_reg(&inst_list, ctx, "is result");
        if (result_reg == NULL)
            return inst_list;

        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t%%eax, %s\n", result_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        *out_reg = result_reg;
    }

    return inst_list;
}

static ListNode_t *codegen_emit_class_cast_check_from_instance_ptr(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *instance_ptr_reg)
{
    if (expr == NULL || instance_ptr_reg == NULL)
        return inst_list;

    const char *target_label = codegen_class_typeinfo_label(
        expr->expr_data.as_data.target_record_type,
        expr->expr_data.as_data.target_type_id);
    if (target_label == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to resolve class type for \"as\" operator.");
        return inst_list;
    }

    Register_t *typeinfo_reg = NULL;
    inst_list = codegen_load_typeinfo_from_instance_ptr(inst_list, ctx,
        instance_ptr_reg, &typeinfo_reg);
    if (typeinfo_reg == NULL || codegen_had_error(ctx))
        return inst_list;

    char buffer[128];
    /* Preserve the instance pointer across the runtime call (caller-saved registers may be clobbered).
     * Reserve the 32-byte Windows shadow space as well so the saved pointer is not overwritten. */
    inst_list = add_inst(inst_list, "\tsubq\t$48, %rsp\n");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, 32(%%rsp)\n", instance_ptr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    codegen_move_rtti_args(&inst_list, typeinfo_reg, target_label);
    free_reg(get_reg_stack(), typeinfo_reg);

    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_check_cast");
    free_arg_regs();

    snprintf(buffer, sizeof(buffer), "\tmovq\t32(%%rsp), %s\n", instance_ptr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\taddq\t$48, %rsp\n");
    return inst_list;
}

ListNode_t *codegen_emit_class_cast_check_from_address(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *addr_reg)
{
    if (expr == NULL || addr_reg == NULL)
        return inst_list;

    struct Expression *source_expr = expr->expr_data.as_data.expr;
    struct RecordType *source_record = codegen_expr_record_type(source_expr,
        ctx != NULL ? ctx->symtab : NULL);
    int is_class_var = (source_record != NULL && record_type_is_class(source_record));

    Register_t *instance_ptr_reg = addr_reg;
    if (is_class_var)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
            addr_reg->bit_64, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    return codegen_emit_class_cast_check_from_instance_ptr(expr, inst_list, ctx,
        instance_ptr_reg);
}

ListNode_t *codegen_emit_class_cast_check(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx)
{
    if (expr == NULL || expr->expr_data.as_data.expr == NULL)
        return inst_list;

    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
        return inst_list;

    inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
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

static inline const char *codegen_register_id_to_8bit(RegisterId_t reg_id)
{
    switch (reg_id)
    {
        case REG_RAX: return "%al";
        case REG_RBX: return "%bl";
        case REG_RCX: return "%cl";
        case REG_RDX: return "%dl";
        case REG_RSI: return "%sil";
        case REG_RDI: return "%dil";
        case REG_RBP: return "%bpl";
        case REG_RSP: return "%spl";
        case REG_R8: return "%r8b";
        case REG_R9: return "%r9b";
        case REG_R10: return "%r10b";
        case REG_R11: return "%r11b";
        case REG_R12: return "%r12b";
        case REG_R13: return "%r13b";
        case REG_R14: return "%r14b";
        case REG_R15: return "%r15b";
        default: return NULL;
    }
}

static inline const char *codegen_register_id_to_16bit(RegisterId_t reg_id)
{
    switch (reg_id)
    {
        case REG_RAX: return "%ax";
        case REG_RBX: return "%bx";
        case REG_RCX: return "%cx";
        case REG_RDX: return "%dx";
        case REG_RSI: return "%si";
        case REG_RDI: return "%di";
        case REG_RBP: return "%bp";
        case REG_RSP: return "%sp";
        case REG_R8: return "%r8w";
        case REG_R9: return "%r9w";
        case REG_R10: return "%r10w";
        case REG_R11: return "%r11w";
        case REG_R12: return "%r12w";
        case REG_R13: return "%r13w";
        case REG_R14: return "%r14w";
        case REG_R15: return "%r15w";
        default: return NULL;
    }
}

static const char *codegen_register_name8(const Register_t *reg)
{
    if (reg == NULL || reg->bit_64 == NULL)
        return NULL;
    return codegen_register_id_to_8bit(reg->reg_id);
}

const char *codegen_register_name16(const Register_t *reg)
{
    if (reg == NULL || reg->bit_64 == NULL)
        return NULL;
    return codegen_register_id_to_16bit(reg->reg_id);
}

static ListNode_t *codegen_store_value_to_stack(ListNode_t *inst_list, Register_t *value_reg,
    int offset, int element_size)
{
    if (value_reg == NULL)
        return inst_list;

    char buffer[128];
    if (element_size == 1)
    {
        const char *reg8 = codegen_register_name8(value_reg);
        assert(reg8 != NULL && "8-bit register name not found for store operation");
        snprintf(buffer, sizeof(buffer), "\tmovb\t%s, -%d(%%rbp)\n", reg8, offset);
        return add_inst(inst_list, buffer);
    }
    else if (element_size == 2)
    {
        const char *reg16 = codegen_register_name16(value_reg);
        assert(reg16 != NULL && "16-bit register name not found for store operation");
        snprintf(buffer, sizeof(buffer), "\tmovw\t%s, -%d(%%rbp)\n", reg16, offset);
        return add_inst(inst_list, buffer);
    }
    else if (element_size == 4)
    {
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, -%d(%%rbp)\n", value_reg->bit_32, offset);
        return add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", value_reg->bit_64, offset);
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_materialize_array_literal(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL || out_reg == NULL)
        return inst_list;

    if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
        return codegen_materialize_array_of_const(expr, inst_list, ctx, out_reg);

    int element_size = expr_get_array_element_size(expr, ctx);
    if (element_size <= 0)
        element_size = DOUBLEWORD;

    int element_count = expr->expr_data.array_literal_data.element_count;
    if (element_count == 0)
    {
        const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
        int descriptor_size = codegen_expr_align_to(2 * pointer_bytes, pointer_bytes);
        if (expr->array_element_size > 0)
        {
            int candidate = expr->array_element_size * 2;
            if (descriptor_size < candidate)
                descriptor_size = codegen_expr_align_to(candidate, pointer_bytes);
        }
        StackNode_t *desc_slot = codegen_alloc_temp_bytes("arr_lit_desc", descriptor_size);
        if (desc_slot == NULL)
            return inst_list;

        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate register for array literal descriptor.");
            return inst_list;
        }

        char buffer[128];
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", desc_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", desc_slot->offset - pointer_bytes);
        inst_list = add_inst(inst_list, buffer);

        int field_count = descriptor_size / pointer_bytes;
        for (int field = 2; field < field_count; ++field)
        {
            int field_offset = desc_slot->offset - field * pointer_bytes;
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", field_offset);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", desc_slot->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        *out_reg = addr_reg;
        return inst_list;
    }
    int data_size = codegen_expr_align_to(element_count * element_size, DOUBLEWORD);
    StackNode_t *data_slot = codegen_alloc_temp_bytes("arr_lit_data", data_size);
    if (data_slot == NULL)
        return inst_list;

    ListNode_t *cur = expr->expr_data.array_literal_data.elements;
    int index = 0;
    while (cur != NULL)
    {
        struct Expression *element_expr = (struct Expression *)cur->cur;
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(element_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int element_offset = data_slot->offset - index * element_size;
        inst_list = codegen_store_value_to_stack(inst_list, value_reg, element_offset, element_size);
        free_reg(get_reg_stack(), value_reg);

        cur = cur->next;
        ++index;
    }

    const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
    int descriptor_size = codegen_expr_align_to(2 * pointer_bytes, pointer_bytes);
    if (expr->array_element_size > 0)
    {
        int candidate = expr->array_element_size * 2;
        if (descriptor_size < candidate)
            descriptor_size = codegen_expr_align_to(candidate, pointer_bytes);
    }
    StackNode_t *desc_slot = codegen_alloc_temp_bytes("arr_lit_desc", descriptor_size);
    if (desc_slot == NULL)
        return inst_list;

    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to allocate register for array literal descriptor.");
        return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", data_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64, desc_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", element_count, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", addr_reg->bit_64,
        desc_slot->offset - pointer_bytes);
    inst_list = add_inst(inst_list, buffer);

    int field_count = descriptor_size / pointer_bytes;
    for (int field = 2; field < field_count; ++field)
    {
        int field_offset = desc_slot->offset - field * pointer_bytes;
        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", field_offset);
        inst_list = add_inst(inst_list, buffer);
    }

    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", desc_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    *out_reg = addr_reg;
    return inst_list;
}

static int codegen_format_arg_kind_for_expr(struct Expression *expr)
{
    int type_tag = expr_get_type_tag(expr);
    switch (type_tag)
    {
        case INT_TYPE:
        case LONGINT_TYPE:
        case BYTE_TYPE:
        case WORD_TYPE:
        case LONGWORD_TYPE:
        case INT64_TYPE:
        case QWORD_TYPE:
        case ENUM_TYPE:
            return KGPC_TVAR_KIND_INT;
        case BOOL:
            return KGPC_TVAR_KIND_BOOL;
        case CHAR_TYPE:
            return KGPC_TVAR_KIND_CHAR;
        case REAL_TYPE:
            return KGPC_TVAR_KIND_REAL;
        case STRING_TYPE:
            return KGPC_TVAR_KIND_ANSISTRING;
        case SHORTSTRING_TYPE:
            return KGPC_TVAR_KIND_STRING;
        case POINTER_TYPE:
            return KGPC_TVAR_KIND_POINTER;
        default:
            return -1;
    }
}

static ListNode_t *codegen_materialize_array_of_const(struct Expression *expr,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL || out_reg == NULL)
        return inst_list;

    const int element_size = (int)sizeof(kgpc_tvarrec);
    int element_count = expr->expr_data.array_literal_data.element_count;
    int data_size = codegen_expr_align_to(element_count * element_size, DOUBLEWORD);
    StackNode_t *data_slot = codegen_alloc_temp_bytes("arr_const_data", data_size);
    if (data_slot == NULL)
        return inst_list;

    ListNode_t *cur = expr->expr_data.array_literal_data.elements;
    int index = 0;
    char buffer[128];

    while (cur != NULL)
    {
        struct Expression *element_expr = (struct Expression *)cur->cur;
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(element_expr, inst_list, ctx, &value_reg);
        if (codegen_had_error(ctx) || value_reg == NULL)
        {
            if (value_reg != NULL)
                free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int kind = codegen_format_arg_kind_for_expr(element_expr);
        if (kind < 0)
        {
            codegen_report_error(ctx,
                "ERROR: Unsupported argument type %d in array of const literal.",
                expr_get_type_tag(element_expr));
            free_reg(get_reg_stack(), value_reg);
            return inst_list;
        }

        int element_offset = data_slot->offset - index * element_size;
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, -%d(%%rbp)\n",
            kind, element_offset);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n",
            element_offset - 4);
        inst_list = add_inst(inst_list, buffer);

        int data_offset = element_offset - 8;
        if (kind == KGPC_TVAR_KIND_REAL)
        {
            /* FPC's vtExtended expects a pointer to the Extended/Double value,
             * not the raw value.  Allocate a temp slot, store the double there,
             * and put the address in the TVarRec data field. */
            StackNode_t *real_slot = codegen_alloc_temp_bytes("varrec_real", 8);
            if (real_slot != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                    value_reg->bit_64, real_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (addr_reg != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                        real_slot->offset, addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        addr_reg->bit_64, data_offset);
                    inst_list = add_inst(inst_list, buffer);
                    free_reg(get_reg_stack(), addr_reg);
                }
            }
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                value_reg->bit_64, data_offset);
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), value_reg);
        cur = cur->next;
        ++index;
    }

    const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
    StackNode_t *desc_slot = codegen_alloc_temp_bytes("arr_const_desc",
        codegen_expr_align_to(2 * pointer_bytes, pointer_bytes));
    if (desc_slot == NULL)
        return inst_list;

    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to allocate register for array of const descriptor.");
        return inst_list;
    }

    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
        data_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
        addr_reg->bit_64, desc_slot->offset);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n",
        element_count, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
        addr_reg->bit_64, desc_slot->offset - pointer_bytes);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
        desc_slot->offset, addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);

    *out_reg = addr_reg;
    return inst_list;
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias* codegen_get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

static unsigned long codegen_next_record_temp_id(void)
{
    static unsigned long counter = 0;
    return ++counter;
}

static StackNode_t *codegen_alloc_record_temp(long long size)
{
    if (size <= 0 || size > INT_MAX)
        return NULL;

    char label[32];
    snprintf(label, sizeof(label), "record_arg_%lu", codegen_next_record_temp_id());
    return add_l_t_bytes(label, (int)size);
}


static inline int type_is_file_like(int type_tag)
{
    return type_tag == FILE_TYPE || type_tag == TEXT_TYPE;
}

int codegen_type_uses_qword(int type_tag)
{
    return (type_tag == REAL_TYPE || type_tag == INT64_TYPE ||
        type_tag == QWORD_TYPE ||
        type_tag == POINTER_TYPE || type_tag == STRING_TYPE ||
        type_tag == SHORTSTRING_TYPE ||
        type_is_file_like(type_tag) || type_tag == PROCEDURE);
}

int codegen_type_is_signed(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
        case LONGINT_TYPE:
        case INT64_TYPE:
            return 1;
        default:
            return 0;
    }
}

static void codegen_typeinfo_label_for_type_id(SymTab_t *symtab, const char *type_id,
    char *buffer, size_t size)
{
    codegen_common_typeinfo_label_for_type_id(symtab, type_id, buffer, size);
}

/* Helper to get KgpcType from expression, preferring resolved_kgpc_type.
 * Returns the KgpcType if available, or creates a temporary one from legacy fields.
 * Returns NULL if type cannot be determined.
 * Note: The returned KgpcType should NOT be freed - it's either owned by the expression
 * or is a static/temporary type. */
KgpcType* expr_get_kgpc_type(const struct Expression *expr)
{
    if (expr == NULL)
        return NULL;

    if (expr->resolved_kgpc_type != NULL)
        return expr->resolved_kgpc_type;

    static KgpcType *primitive_cache[256];
    int tag = UNKNOWN_TYPE;

    switch (expr->type)
    {
        case EXPR_INUM:
            tag = INT_TYPE;
            break;
        case EXPR_RNUM:
            tag = REAL_TYPE;
            break;
        case EXPR_STRING:
            tag = STRING_TYPE;
            break;
        case EXPR_CHAR_CODE:
            tag = CHAR_TYPE;
            break;
        case EXPR_BOOL:
            tag = BOOL;
            break;
        case EXPR_NIL:
            return create_pointer_type(NULL);
        case EXPR_TYPEINFO:
            return create_pointer_type(NULL);
        case EXPR_RECORD_CONSTRUCTOR:
        {
            struct RecordType *record = codegen_expr_record_type(expr, NULL);
            if (record != NULL)
                return create_record_type(record);
            return NULL;
        }
        case EXPR_POINTER_DEREF:
        {
            struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
            if (pointer_expr == NULL)
                return NULL;
            KgpcType *ptr_type = expr_get_kgpc_type(pointer_expr);
            if (ptr_type != NULL && kgpc_type_is_pointer(ptr_type))
                return ptr_type->info.points_to;
            return NULL;
        }
        case EXPR_ARRAY_LITERAL:
        {
            if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
                return create_array_of_const_type();
            if (expr->array_element_type != UNKNOWN_TYPE)
            {
                KgpcType *elem_type = NULL;
                if (expr->array_element_type == RECORD_TYPE && expr->array_element_record_type != NULL)
                    elem_type = create_record_type(expr->array_element_record_type);
                else
                    elem_type = create_primitive_type(expr->array_element_type);

                if (elem_type != NULL)
                {
                    int end_index = expr->array_upper_bound;
                    if (end_index < expr->array_lower_bound)
                        end_index = expr->array_lower_bound - 1;
                    return create_array_type(elem_type, expr->array_lower_bound, end_index);
                }
            }
            return NULL;
        }
        case EXPR_FUNCTION_CALL:
        {
            /* First check if resolved_kgpc_type was set during semcheck */
            if (expr->resolved_kgpc_type != NULL)
                return expr->resolved_kgpc_type;
            
            /* For function calls, try to get the return type from call_kgpc_type */
            KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
            if (call_type == NULL &&
                expr->expr_data.function_call_data.resolved_func != NULL)
            {
                call_type = expr->expr_data.function_call_data.resolved_func->type;
            }
            if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE)
            {
                KgpcType *ret_type = codegen_function_call_return_type_from_expr(expr);
                if (ret_type != NULL)
                    return ret_type;
                
                /* If return_type is NULL, check return_type_id using type lookup */
                const char *ret_id = call_type->info.proc_info.return_type_id;
                if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL && ret_id != NULL) {
                    fprintf(stderr, "[CodeGen] expr_get_kgpc_type: EXPR_FUNCTION_CALL return_type_id='%s'\n", ret_id);
                }
                if (ret_id != NULL && kgpc_type_id_uses_qword(ret_id, NULL))
                {
                    /* Return a pointer type to indicate 64-bit return */
                    static KgpcType *cached_pointer = NULL;
                    if (cached_pointer == NULL)
                        cached_pointer = create_pointer_type(NULL);
                    return cached_pointer;
                }
            }
            return NULL;
        }
        default:
            break;
    }

    if (tag != UNKNOWN_TYPE)
    {
        if (tag >= 0 && tag < (int)(sizeof(primitive_cache) / sizeof(primitive_cache[0])))
        {
            if (primitive_cache[tag] == NULL)
                primitive_cache[tag] = create_primitive_type(tag);
            return primitive_cache[tag];
        }
        return create_primitive_type(tag);
    }

    return NULL;
}

long long expr_effective_size_bytes(const struct Expression *expr)
{
    /* For pointer dereference, try to get size from the pointer's subtype info.
     * This handles cases like PByte^ where Byte is a subrange type that maps to
     * INT_TYPE but should have size 1. */
    if (expr != NULL && expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type))
        {
            KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
            if (points_to != NULL)
            {
                long long size = kgpc_type_sizeof(points_to);
                if (size > 0)
                    return size;
            }
        }
    }

    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
    {
        long long size = kgpc_type_sizeof(type);
        if (size > 0)
            return size;
    }

    int tag = expr_get_type_tag(expr);
    switch (tag)
    {
        case CHAR_TYPE:
            return 1;
        case INT_TYPE:
        case BOOL:
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case FILE_TYPE:
            return 368;
        case TEXT_TYPE:
            return 632;
        case STRING_TYPE:
        case POINTER_TYPE:
        case REAL_TYPE:
            return 8;
        case LONGINT_TYPE:
            return 4;  // Match FPC's 32-bit LongInt
        default:
            return 0;
    }
}

/* Helper to get type tag from expression, preferring resolved_kgpc_type */
int expr_get_type_tag(const struct Expression *expr)
{
    if (expr == NULL)
        return UNKNOWN_TYPE;

    if (expr->type == EXPR_MULOP &&
        expr->expr_data.mulop_data.mulop_type == SLASH)
        return REAL_TYPE;

    if (expr->is_array_expr && expr->array_element_type != UNKNOWN_TYPE)
    {
        if (expr->array_element_type == CHAR_TYPE &&
            expr_get_array_lower_bound(expr) == 0 &&
            expr_get_array_upper_bound(expr) == 255)
            return SHORTSTRING_TYPE;
        return expr->array_element_type;
    }

    /* Prefer KgpcType if available */
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
    {
        int tag = codegen_tag_from_kgpc(type);
        if (tag != UNKNOWN_TYPE)
            return tag;
        switch (type->kind)
        {
            case TYPE_KIND_POINTER:
                return POINTER_TYPE;
            case TYPE_KIND_RECORD:
                return RECORD_TYPE;
            case TYPE_KIND_PROCEDURE:
                return PROCEDURE;
            case TYPE_KIND_ARRAY_OF_CONST:
                return ARRAY_OF_CONST_TYPE;
            default:
                break;
        }
    }
    
    return UNKNOWN_TYPE;
}

/* Helper to get array lower bound from expression, preferring resolved_kgpc_type */
int expr_get_array_lower_bound(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    /* Prefer KgpcType if available */
    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
    {
        int start = 0;
        if (kgpc_type_get_array_bounds(expr->resolved_kgpc_type, &start, NULL) == 0)
            return start;
    }
    
    /* Fall back to legacy field */
    return expr->array_lower_bound;
}

/* Helper to get array upper bound from expression, preferring resolved_kgpc_type */
int expr_get_array_upper_bound(const struct Expression *expr)
{
    if (expr == NULL)
        return -1;

    if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
    {
        int end = -1;
        if (kgpc_type_get_array_bounds(expr->resolved_kgpc_type, NULL, &end) == 0)
            return end;
    }

    return expr->array_upper_bound;
}

/* Check if an expression represents a character set (set of char) */
int expr_is_char_set_ctx(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return 0;
    
    /* Check if expression has a KgpcType with type_alias */
    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = expr->resolved_kgpc_type->type_alias;
        if (alias != NULL && alias->is_set &&
            (alias->set_element_type == CHAR_TYPE ||
             (alias->set_element_type_id != NULL &&
              (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
               pascal_identifier_equals(alias->set_element_type_id, "AnsiChar")))))
            return 1;
    }
    
    /* For variable references, look up the type in the symbol table */
    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) && node != NULL)
        {
            if (node->type != NULL)
            {
                struct TypeAlias *alias = node->type->type_alias;
                if (alias != NULL && alias->is_set &&
                    (alias->set_element_type == CHAR_TYPE ||
                     (alias->set_element_type_id != NULL &&
                      (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
                       pascal_identifier_equals(alias->set_element_type_id, "AnsiChar")))))
                    return 1;
            }
            if (node->hash_type == HASHTYPE_CONST &&
                node->const_set_value != NULL &&
                node->const_set_size > 4)
            {
                return 1;
            }
        }
    }
    
    /* For set literals, check if elements are characters or single-char strings */
    if (expr->type == EXPR_SET && expr->expr_data.set_data.elements != NULL)
    {
        ListNode_t *node = expr->expr_data.set_data.elements;
        while (node != NULL)
        {
            struct SetElement *element = (struct SetElement *)node->cur;
            if (element->lower != NULL)
            {
                int elem_type = expr_get_type_tag(element->lower);
                /* Character sets can have CHAR_TYPE or STRING_TYPE (single char) elements */
                if (elem_type == CHAR_TYPE)
                    return 1;
                if (elem_type == STRING_TYPE && element->lower->type == EXPR_STRING)
                {
                    /* Single-character string literal */
                    if (element->lower->expr_data.string != NULL &&
                        strlen(element->lower->expr_data.string) == 1)
                        return 1;
                }
                if (element->lower->type == EXPR_CHAR_CODE)
                    return 1;
                if (element->lower->type == EXPR_STRING &&
                    element->lower->expr_data.string != NULL &&
                    strlen(element->lower->expr_data.string) == 1)
                    return 1;
                /* Integer elements > 31 can't fit in a 32-bit register set;
                   route to memory-based 32-byte set (e.g. [97..122] for 'a'..'z') */
                if (element->lower->type == EXPR_INUM &&
                    element->lower->expr_data.i_num > 31)
                    return 1;
            }
            if (element->upper != NULL)
            {
                if (element->upper->type == EXPR_INUM &&
                    element->upper->expr_data.i_num > 31)
                    return 1;
            }
            node = node->next;
        }
    }
    
    return 0;
}

/* Wrapper that doesn't need context - for backward compatibility */
int expr_is_char_set(const struct Expression *expr)
{
    return expr_is_char_set_ctx(expr, NULL);
}

/* Helper to get array element size from expression, preferring resolved_kgpc_type
 * ctx parameter reserved for future use in computing complex type sizes */
long long expr_get_array_element_size(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL)
        return -1;

    int expects_array_metadata = 0;
    if (expr->is_array_expr || expr->type == EXPR_ARRAY_ACCESS ||
        expr->type == EXPR_ARRAY_LITERAL || expr->array_element_type != UNKNOWN_TYPE ||
        expr->array_element_type_id != NULL)
    {
        expects_array_metadata = 1;
    }

    if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
        return (long long)sizeof(kgpc_tvarrec);

    if (expr->type == EXPR_ARRAY_ACCESS && expr->resolved_kgpc_type != NULL)
    {
        /* If the result of this array access is itself an array, return that
         * array's element size (not the total sizeof the result array).
         * This ensures nested array stores use the correct write width. */
        if (kgpc_type_is_array(expr->resolved_kgpc_type))
        {
            long long elem_size = kgpc_type_get_array_element_size(expr->resolved_kgpc_type);
            if (elem_size <= 0 && ctx != NULL && ctx->symtab != NULL)
            {
                KgpcType *et = kgpc_type_get_array_element_type_resolved(
                    expr->resolved_kgpc_type, ctx->symtab);
                if (et != NULL)
                    elem_size = kgpc_type_sizeof(et);
            }
            if (elem_size > 0)
                return elem_size;
        }
        long long result_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
        if (result_size > 0)
            return result_size;
    }

    if (expr->array_element_size > 0)
    {
        long long tag_size = -1;
        if (expr->array_element_type != UNKNOWN_TYPE)
            tag_size = codegen_sizeof_type_tag(expr->array_element_type);
        if (tag_size <= 0 || expr->array_element_size != tag_size ||
            (expr->array_element_type_id != NULL &&
             (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
              pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
        {
            return expr->array_element_size;
        }
    }
    
    /* Prefer KgpcType if available */
    if (expr->resolved_kgpc_type != NULL && (kgpc_type_is_array(expr->resolved_kgpc_type) || kgpc_type_is_shortstring(expr->resolved_kgpc_type)))
    {
        expects_array_metadata = 1;
        if (kgpc_type_is_shortstring(expr->resolved_kgpc_type))
            return 1;
        long long size = kgpc_type_get_array_element_size(expr->resolved_kgpc_type);
        if (size > 0)
            return size;
        if (ctx != NULL && ctx->symtab != NULL)
        {
            KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(expr->resolved_kgpc_type, ctx->symtab);
            if (elem_type != NULL)
            {
                size = kgpc_type_sizeof(elem_type);
                if (size > 0)
                    return size;
            }
        }
    }

    if (expr->array_element_type != UNKNOWN_TYPE)
    {
        if (expr->array_element_size > 0)
        {
            long long tag_size = codegen_sizeof_type_tag(expr->array_element_type);
            if (expr->array_element_size != tag_size ||
                (expr->array_element_type_id != NULL &&
                 (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
                  pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
            {
                return expr->array_element_size;
            }
        }
        if (expr->array_element_type == CHAR_TYPE && ctx != NULL && ctx->symtab != NULL &&
            expr->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, expr->array_element_type_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                long long node_size = kgpc_type_sizeof(type_node->type);
                if (node_size > 0 &&
                    node_size != codegen_sizeof_type_tag(expr->array_element_type))
                {
                    return node_size;
                }
            }
        }
        long long tag_size = codegen_sizeof_type_tag(expr->array_element_type);
        if (tag_size > 0)
            return tag_size;
    }

    if (ctx != NULL && ctx->symtab != NULL && expr->array_element_type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab, expr->array_element_type_id) != 0 &&
            type_node != NULL && type_node->type != NULL)
        {
            long long node_size = kgpc_type_sizeof(type_node->type);
            if (node_size > 0)
                return node_size;
        }
    }

    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL &&
        expr->expr_data.id != NULL)
    {
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, ctx->symtab, expr->expr_data.id) != 0 &&
            var_node != NULL && var_node->type != NULL &&
            kgpc_type_is_array(var_node->type))
        {
            expects_array_metadata = 1;
            KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_node->type, ctx->symtab);
            if (elem_type != NULL)
            {
                long long elem_size = kgpc_type_sizeof(elem_type);
                if (elem_size > 0)
                    return elem_size;
            }
        }
    }

    if (expr->type == EXPR_POINTER_DEREF)
    {
        const struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        if (kgpc_getenv("KGPC_DEBUG_ARRAY_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_ARRAY_ACCESS] pointer_deref base_type=%d ptr_subtype=%d ptr_subtype_id=%s ptr_kgpc=%s%s%s\n",
                pointer_expr != NULL ? pointer_expr->type : -1,
                expr->pointer_subtype,
                expr->pointer_subtype_id ? expr->pointer_subtype_id : "<null>",
                (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL)
                    ? kgpc_type_to_string(pointer_expr->resolved_kgpc_type) : "<null>",
                (pointer_expr != NULL && pointer_expr->type == EXPR_VAR_ID && pointer_expr->expr_data.id != NULL)
                    ? " id=" : "",
                (pointer_expr != NULL && pointer_expr->type == EXPR_VAR_ID && pointer_expr->expr_data.id != NULL)
                    ? pointer_expr->expr_data.id : "");
        }
        if (pointer_expr != NULL &&
            pointer_expr->resolved_kgpc_type != NULL &&
            kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type))
        {
            KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
            if (points_to != NULL)
            {
                if (kgpc_type_is_array(points_to))
                {
                    long long elem_size = kgpc_type_get_array_element_size(points_to);
                    if (elem_size <= 0)
                    {
                        KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(points_to,
                            ctx != NULL ? ctx->symtab : NULL);
                        if (elem_type != NULL)
                            elem_size = kgpc_type_sizeof(elem_type);
                    }
                    if (elem_size > 0)
                        return elem_size;
                }
                else
                {
                    long long elem_size = kgpc_type_sizeof(points_to);
                    if (elem_size > 0)
                        return elem_size;
                }
            }
        }

        if (pointer_expr != NULL && pointer_expr->type == EXPR_VAR_ID &&
            ctx != NULL && ctx->symtab != NULL && pointer_expr->expr_data.id != NULL)
        {
            HashNode_t *var_node = NULL;
            if (FindSymbol(&var_node, ctx->symtab, pointer_expr->expr_data.id) != 0 &&
                var_node != NULL && var_node->type != NULL)
            {
                if (kgpc_type_is_pointer(var_node->type))
                {
                    KgpcType *points_to = var_node->type->info.points_to;
                    if (points_to != NULL)
                    {
                        if (kgpc_type_is_array(points_to))
                        {
                            long long elem_size = kgpc_type_get_array_element_size(points_to);
                            if (elem_size <= 0)
                            {
                                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(points_to,
                                    ctx->symtab);
                                if (elem_type != NULL)
                                    elem_size = kgpc_type_sizeof(elem_type);
                            }
                            if (elem_size > 0)
                                return elem_size;
                        }
                        else
                        {
                            long long elem_size = kgpc_type_sizeof(points_to);
                            if (elem_size > 0)
                                return elem_size;
                        }
                    }
                }
                if (kgpc_type_is_array(var_node->type))
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
                        return elem_size;
                }
            }
        }

        if (pointer_expr != NULL && pointer_expr->type == EXPR_VAR_ID &&
            ctx != NULL && pointer_expr->expr_data.id != NULL)
        {
            struct RecordField *with_field = codegen_lookup_with_field(ctx,
                pointer_expr->expr_data.id, NULL);
            if (with_field != NULL)
            {
                long long elem_size = codegen_array_elem_size_from_field(with_field, ctx);
                if (elem_size > 0)
                    return elem_size;
            }
            if (with_field == NULL && ctx->symtab != NULL)
            {
                struct RecordField *unique_field = codegen_find_unique_record_field(
                    ctx->symtab, pointer_expr->expr_data.id, NULL);
                if (unique_field != NULL)
                {
                    long long elem_size = codegen_array_elem_size_from_field(unique_field, ctx);
                    if (elem_size > 0)
                        return elem_size;
                }
            }
        }

        const struct Expression *lookup_expr = pointer_expr;
        if (lookup_expr != NULL && lookup_expr->type == EXPR_TYPECAST &&
            lookup_expr->expr_data.typecast_data.expr != NULL)
        {
            lookup_expr = lookup_expr->expr_data.typecast_data.expr;
        }

        if (lookup_expr != NULL && lookup_expr->type == EXPR_RECORD_ACCESS && ctx != NULL)
        {
            struct RecordField *field = codegen_lookup_record_field_expr((struct Expression *)lookup_expr, ctx);
            if (field != NULL)
            {
                if (kgpc_getenv("KGPC_DEBUG_ARRAY_ACCESS") != NULL)
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_ARRAY_ACCESS] record field=%s is_pointer=%d is_array=%d pointer_type=%d pointer_type_id=%s array_elem_type=%d array_elem_type_id=%s\n",
                        field->name ? field->name : "<null>",
                        field->is_pointer,
                        field->is_array,
                        field->pointer_type,
                        field->pointer_type_id ? field->pointer_type_id : "<null>",
                        field->array_element_type,
                        field->array_element_type_id ? field->array_element_type_id : "<null>");
                }
                {
                    long long elem_size = codegen_array_elem_size_from_field(field, ctx);
                    if (elem_size > 0)
                        return elem_size;
                }
            }
        }

        if (pointer_expr != NULL && pointer_expr->type == EXPR_TYPECAST &&
            ctx != NULL && ctx->symtab != NULL)
        {
            const char *target_id = pointer_expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, ctx->symtab, target_id) != 0 &&
                    type_node != NULL && type_node->type != NULL &&
                    kgpc_type_is_pointer(type_node->type))
                {
                    KgpcType *points_to = type_node->type->info.points_to;
                    if (points_to != NULL)
                    {
                        long long elem_size = kgpc_type_sizeof(points_to);
                        if (elem_size > 0)
                            return elem_size;
                    }
                }
            }
        }

        if (expr->pointer_subtype != UNKNOWN_TYPE)
        {
            long long tag_size = codegen_sizeof_type_tag(expr->pointer_subtype);
            if (tag_size > 0)
                return tag_size;
        }
        if (ctx != NULL && ctx->symtab != NULL && expr->pointer_subtype_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, expr->pointer_subtype_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                long long node_size = kgpc_type_sizeof(type_node->type);
                if (node_size > 0)
                    return node_size;
            }
        }
    }

    if (expr->type == EXPR_ARRAY_ACCESS)
    {
        const struct Expression *base = expr->expr_data.array_access_data.array_expr;
        if (base != NULL)
        {
            KgpcType *base_type = expr_get_kgpc_type(base);
            int base_is_pointer_like = expr_has_type_tag(base, POINTER_TYPE) ||
                (base_type != NULL && kgpc_type_is_pointer(base_type));
            int base_is_string_like = is_string_type(expr_get_type_tag(base)) ||
                (base_type != NULL && kgpc_type_is_string(base_type));
            long long indexed_size = -1;
            if ((base_is_pointer_like || base_is_string_like) &&
                codegen_get_indexable_element_size((struct Expression *)base, ctx, &indexed_size) &&
                indexed_size > 0)
            {
                return indexed_size;
            }
            long long base_elem_size = expr_get_array_element_size(base, ctx);
            if (base_elem_size > 0)
                return base_elem_size;
        }
    }

    if (!expects_array_metadata)
        return -1;

    /* With-stack lookup: the base expression might be a variable from
     * an enclosing `with` block that semcheck didn't resolve. */
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL &&
        ctx != NULL && ctx->with_depth > 0)
    {
        struct RecordField *with_field = codegen_lookup_with_field(ctx,
            expr->expr_data.id, NULL);
        if (with_field != NULL)
        {
            long long elem_size = codegen_array_elem_size_from_field(with_field, ctx);
            if (elem_size > 0)
                return elem_size;
        }
    }

    /* For EXPR_ARRAY_ACCESS base in EXPR_VAR_ID, try with-stack on the inner base */
    if (expr->type == EXPR_ARRAY_ACCESS)
    {
        const struct Expression *inner_base = expr->expr_data.array_access_data.array_expr;
        if (inner_base != NULL && inner_base->type == EXPR_VAR_ID &&
            inner_base->expr_data.id != NULL && ctx != NULL && ctx->with_depth > 0)
        {
            struct RecordField *with_field = codegen_lookup_with_field(ctx,
                inner_base->expr_data.id, NULL);
            if (with_field != NULL)
            {
                long long elem_size = codegen_array_elem_size_from_field(with_field, ctx);
                if (elem_size > 0)
                    return elem_size;
            }
        }
    }

    /* Hard invariant: metadata gaps must be fixed at source, not silently defaulted. */
    KGPC_COMPILER_HARD_ASSERT(expr->array_element_size > 0,
        "unable to determine array element size (expr_type=%d elem_tag=%d elem_type_id=%s)",
        expr->type, expr->array_element_type,
        expr->array_element_type_id != NULL ? expr->array_element_type_id : "<null>");
    return expr->array_element_size;
}

/* Check if expression is signed, working with KgpcType */
static int expr_is_signed_kgpctype(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
        return kgpc_type_is_signed(type);

    int tag = expr_get_type_tag(expr);
    if (tag != UNKNOWN_TYPE)
        return codegen_type_is_signed(tag);

    return 0;
}

/* Check if expression uses qword, working with KgpcType */
int expr_uses_qword_kgpctype(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr_has_type_tag(expr, REAL_TYPE))
    {
        long long eff_size = expr_effective_size_bytes(expr);
        if (eff_size == 4)
            return 0;
    }
    
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
        return kgpc_type_uses_qword(type);

    return 0;
}

/* Check if expression has a specific type tag, working with KgpcType */
int expr_has_type_tag(const struct Expression *expr, int type_tag)
{
    if (expr == NULL)
        return (type_tag == UNKNOWN_TYPE);
    
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL)
        return kgpc_type_equals_tag(type, type_tag);

    return 0;
}

static int expr_is_char_pointer(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    KgpcType *type = expr_get_kgpc_type(expr);
    if (type == NULL || !kgpc_type_is_pointer(type))
        return 0;

    if (expr->pointer_subtype == CHAR_TYPE)
        return 1;
    if (expr->pointer_subtype_id != NULL)
    {
        if (pascal_identifier_equals(expr->pointer_subtype_id, "AnsiChar") ||
            pascal_identifier_equals(expr->pointer_subtype_id, "WideChar") ||
            pascal_identifier_equals(expr->pointer_subtype_id, "Char"))
            return 1;
    }

    if (type != NULL && kgpc_type_is_pointer(type))
    {
        KgpcType *pointee = type->info.points_to;
        if (pointee != NULL &&
            pointee->kind == TYPE_KIND_PRIMITIVE &&
            pointee->info.primitive_type_tag == CHAR_TYPE)
            return 1;
    }

    return 0;
}

long long codegen_expr_sret_size(const struct Expression *expr)
{
    KgpcType *ret_type = NULL;
    KgpcType *type = NULL;
    long long size = 0;

    if (expr == NULL)
        return 0;

    if (expr->type == EXPR_FUNCTION_CALL)
    {
        ret_type = codegen_function_call_return_type_from_expr(expr);
        if (ret_type != NULL)
        {
            if (kgpc_type_is_shortstring(ret_type) ||
                (ret_type->type_alias != NULL && ret_type->type_alias->is_shortstring))
            {
                long long ret_size = kgpc_type_sizeof(ret_type);
                return ret_size > 0 ? ret_size : 256;
            }

            if (kgpc_type_is_record(ret_type) ||
                (ret_type->kind == TYPE_KIND_ARRAY &&
                 !kgpc_type_is_dynamic_array(ret_type)) ||
                (ret_type->type_alias != NULL && ret_type->type_alias->is_shortstring))
            {
                long long ret_size = kgpc_type_sizeof(ret_type);
                if (ret_size > 0)
                    return ret_size;
            }
            if (kgpc_type_is_extended(ret_type))
                return 10;
            return 0;
        }
    }

    if (expr_has_type_tag(expr, RECORD_TYPE))
    {
        KgpcType *record_type = expr_get_kgpc_type(expr);
        if (record_type != NULL)
        {
            long long size = kgpc_type_sizeof(record_type);
            if (size > 0)
                return size;
        }
        return 16;
    }

    /* ShortStrings are passed via SRET because they're small fixed-size arrays.
     * Use the actual sized-shortstring storage when type metadata is available. */
    if (expr_has_type_tag(expr, SHORTSTRING_TYPE))
    {
        type = expr_get_kgpc_type(expr);
        if (type != NULL)
        {
            size = kgpc_type_sizeof(type);
            if (size > 0)
                return size;
        }
        return 256;
    }

    type = expr_get_kgpc_type(expr);
    if (type != NULL && type->kind == TYPE_KIND_ARRAY &&
        !kgpc_type_is_dynamic_array(type))
    {
        long long size = kgpc_type_sizeof(type);
        return size > 0 ? size : 16;
    }

    /* Also check for shortstring type aliases */
    if (type != NULL && type->type_alias != NULL && type->type_alias->is_shortstring)
    {
        long long size = kgpc_type_sizeof(type);
        return size > 0 ? size : 256;
    }

    /* Extended (10 bytes) is returned via hidden sret pointer, matching
     * the callee convention which copies the result through kgpc_move.
     * Only applies to function calls — variables are not sret. */
    if (type != NULL && kgpc_type_is_extended(type) &&
        expr->type == EXPR_FUNCTION_CALL)
        return 10;

    return 0;
}

int expr_returns_sret(const struct Expression *expr)
{
    long long sret_size = codegen_expr_sret_size(expr);
    if (sret_size <= 0)
        return 0;
    if (expr != NULL && expr->type == EXPR_FUNCTION_CALL)
    {
        KgpcType *ret_type = codegen_function_call_return_type_from_expr(expr);
        if (ret_type != NULL && kgpc_type_is_shortstring(ret_type))
            return 1;
        if (ret_type != NULL && kgpc_type_is_extended(ret_type))
            return 1;
    }
    if (expr != NULL && expr_has_type_tag(expr, SHORTSTRING_TYPE))
        return 1;
    if (expr != NULL)
    {
        KgpcType *type = expr_get_kgpc_type(expr);
        if (type != NULL && type->type_alias != NULL && type->type_alias->is_shortstring)
            return 1;
        if (type != NULL && kgpc_type_is_extended(type) &&
            expr->type == EXPR_FUNCTION_CALL)
            return 1;
    }
    return sret_size > 8;
}

void codegen_release_function_call_mangled_id(struct Expression *expr)
{
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return;

    if (expr->expr_data.function_call_data.mangled_id != NULL)
    {
        free(expr->expr_data.function_call_data.mangled_id);
        expr->expr_data.function_call_data.mangled_id = NULL;
    }
}

int codegen_expr_is_signed(const struct Expression *expr)
{
    return expr_is_signed_kgpctype(expr);
}

static inline const char *register_name_for_type(const Register_t *reg, int type_tag)
{
    if (reg == NULL)
        return NULL;
    return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *register_name_for_expr(const Register_t *reg, const struct Expression *expr)
{
    if (expr == NULL)
        return register_name_for_type(reg, UNKNOWN_TYPE);
    /* Use KgpcType-based helper instead of converting to tag */
    return expr_uses_qword_kgpctype(expr) ? reg->bit_64 : reg->bit_32;
}

static inline int expression_uses_qword(const struct Expression *expr)
{
    return expr_uses_qword_kgpctype(expr);
}

static int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out, int depth);

static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth);
int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out);

static int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
    long long *size_out, int depth);

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

static int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth);

int codegen_expr_is_addressable(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_VAR_ID:
        case EXPR_ARRAY_ACCESS:
        case EXPR_RECORD_ACCESS:
        case EXPR_POINTER_DEREF:
        case EXPR_ADDR:
        case EXPR_RECORD_CONSTRUCTOR:
            return 1;
        case EXPR_FUNCTION_CALL:
            /* Function-call expressions are addressable only when they are lowered
             * through a hidden sret return buffer. */
            return expr_returns_sret(expr);
        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
                return codegen_expr_is_addressable(expr->expr_data.typecast_data.expr);
            return 0;
        case EXPR_AS:
            if (expr->expr_data.as_data.expr != NULL)
                return codegen_expr_is_addressable(expr->expr_data.as_data.expr);
            return 0;
        default:
            return 0;
    }
}

static int codegen_sizeof_array_node(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth)
{
    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx,
            "ERROR: Type resolution exceeded supported recursion depth.");
        return 1;
    }

    /* Check if array is dynamic */
    int is_dynamic = hashnode_is_dynamic_array(node);
    
    if (is_dynamic)
    {
        codegen_report_error(ctx,
            "ERROR: Unable to determine size of dynamic array %s.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    /* Get element size from KgpcType */
    long long element_size = hashnode_get_element_size(node);
    
    if (element_size <= 0)
    {
        struct TypeAlias *alias = codegen_get_type_alias_from_node(node);
        if (alias != NULL && alias->is_array)
        {
            if (codegen_sizeof_type(ctx, alias->array_element_type,
                    alias->array_element_type_id, NULL,
                    &element_size, depth + 1) != 0)
                return 1;
        }
        else if (codegen_node_is_record_type(node))
        {
            struct RecordType *record_type = codegen_get_record_type_from_node(node);
            if (record_type != NULL && codegen_sizeof_record(ctx, record_type, &element_size,
                    depth + 1) != 0)
                return 1;
        }
        else
        {
            if (node->type == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine element size for array %s (missing type info).",
                    node->id != NULL ? node->id : "");
                return 1;
            }

            long long base = kgpc_type_sizeof(node->type);
            if (base < 0)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine element size for array %s.",
                    node->id != NULL ? node->id : "");
                return 1;
            }
            element_size = base;
        }
    }

    /* Get array bounds from KgpcType if available */
    int array_start, array_end;
    hashnode_get_array_bounds(node, &array_start, &array_end);
    
    long long count = (long long)array_end - (long long)array_start + 1;
    if (count < 0)
    {
        codegen_report_error(ctx,
            "ERROR: Invalid bounds for array %s during size computation.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    *size_out = element_size * count;
    return 0;
}

static long long codegen_sizeof_type_tag(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
        case BOOL:
        case SET_TYPE:
        case ENUM_TYPE:
            return 4;
        case INT64_TYPE:
        case QWORD_TYPE:
            return 8;
        case LONGINT_TYPE:
        case LONGWORD_TYPE:
            return 4;  // Match FPC's 32-bit LongInt/LongWord
        case WORD_TYPE:
            return 2;
        case BYTE_TYPE:
            return 1;
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
        case POINTER_TYPE:
        case FILE_TYPE:
        case TEXT_TYPE:
        case PROCEDURE:
            return CODEGEN_POINTER_SIZE_BYTES;
        case SHORTSTRING_TYPE:
            return 256;  // ShortString is 256 bytes (1 byte length + 255 chars)
        case CHAR_TYPE:
            return 1;
        case RECORD_TYPE:
            return -1;
        default:
            return -1;
    }
}

static int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine type size due to excessive nesting.");
        return 1;
    }

    if (record_type != NULL)
        return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);

    if (type_tag == RECORD_TYPE && type_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to resolve anonymous record type for size computation.");
        return 1;
    }

    if (type_tag != UNKNOWN_TYPE)
    {
        long long base = codegen_sizeof_type_tag(type_tag);
        if (base >= 0)
        {
            *size_out = base;
            return 0;
        }
    }

    if (type_id != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, type_id) != 0 && node != NULL)
            return codegen_sizeof_hashnode(ctx, node, size_out, depth + 1);

        codegen_report_error(ctx, "ERROR: Unable to resolve type %s for size computation.", type_id);
        return 1;
    }

    if (kgpc_getenv("KGPC_DEBUG_CG_ERR"))
    {
        fprintf(stderr, "[codegen-debug] size-fail: type_tag=%d type_id=%s record_type=%p\n", type_tag, type_id ? type_id : "<null>", (void*)record_type);
        /* Print stack trace */
#ifdef HAVE_EXECINFO
        void *bt[20];
        int n = backtrace(bt, 20);
        backtrace_symbols_fd(bt, n, 2);
#endif
    }
    codegen_report_error(ctx, "ERROR: Unable to determine size for expression type %d.", type_tag);
    return 1;
}

static int codegen_sizeof_variant_part(CodeGenContext *ctx, struct VariantPart *variant,
    long long *size_out, int depth);

static int codegen_sizeof_record_members(CodeGenContext *ctx, ListNode_t *members,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    long long total = 0;
    ListNode_t *cur = members;
    while (cur != NULL)
    {
        if (cur->cur == NULL)
        {
            cur = cur->next;
            continue;
        }

        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            long long field_size = 0;

            if (field->is_array)
            {
                if (field->array_is_open || field->array_end < field->array_start)
                {
                    field_size = CODEGEN_POINTER_SIZE_BYTES;
                }
                else
                {
                    long long element_size = 0;
                    if (codegen_sizeof_type(ctx, field->array_element_type,
                            field->array_element_type_id, NULL,
                            &element_size, depth + 1) != 0)
                        return 1;

                    long long count = (long long)field->array_end - (long long)field->array_start + 1;
                    if (count < 0)
                    {
                        codegen_report_error(ctx,
                            "ERROR: Invalid bounds for array field %s.",
                            field->name != NULL ? field->name : "");
                        return 1;
                    }

                    field_size = element_size * count;
                }

                total += field_size;
                cur = cur->next;
                continue;
            }

            if (field->nested_record != NULL)
            {
                if (codegen_sizeof_record(ctx, field->nested_record, &field_size, depth + 1) != 0)
                    return 1;
            }
            else
            {
                if (codegen_sizeof_type(ctx, field->type, field->type_id, NULL,
                        &field_size, depth + 1) != 0)
                    return 1;
            }

            total += field_size;
        }
        else if (cur->type == LIST_VARIANT_PART)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            long long variant_size = 0;
            if (codegen_sizeof_variant_part(ctx, variant, &variant_size, depth + 1) != 0)
                return 1;
            total += variant_size;
        }

        cur = cur->next;
    }

    *size_out = total;
    return 0;
}

static int codegen_sizeof_variant_part(CodeGenContext *ctx, struct VariantPart *variant,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (variant == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (variant->has_cached_size)
    {
        *size_out = variant->cached_size;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Variant part nesting exceeds supported depth.");
        return 1;
    }

    long long max_size = 0;
    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH && cur->cur != NULL)
        {
            struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
            long long branch_size = 0;
            if (codegen_sizeof_record_members(ctx, branch->members, &branch_size, depth + 1) != 0)
                return 1;
            if (branch_size > max_size)
                max_size = branch_size;
        }
        cur = cur->next;
    }

    variant->cached_size = max_size;
    variant->has_cached_size = 1;
    *size_out = max_size;
    return 0;
}

static int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (record == NULL)
    {
        *size_out = 0;
        return 0;
    }

    if (record->has_cached_size && record->cached_size > 0)
    {
        *size_out = record->cached_size;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Record type nesting exceeds supported depth.");
        return 1;
    }

    long long members_size = 0;
    int result = codegen_sizeof_record_members(ctx, record->fields, &members_size, depth);
    if (result != 0)
        return result;

    /* For classes, add 8 bytes for the VMT pointer at the beginning */
    if (record_type_is_class(record))
        *size_out = 8 + members_size;
    else
        *size_out = members_size;
    record->cached_size = *size_out;
    record->has_cached_size = 1;

    return 0;
}

int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
    long long *size_out)
{
    return codegen_sizeof_record(ctx, record, size_out, 0);
}

static int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
    long long *size_out, int depth)
{
    if (size_out == NULL)
        return 1;

    if (alias == NULL)
    {
        codegen_report_error(ctx, "ERROR: Incomplete type alias encountered during size computation.");
        return 1;
    }

    if (alias->storage_size > 0 && !alias->is_array && !alias->is_set &&
        !alias->is_enum && !alias->is_file)
    {
        *size_out = alias->storage_size;
        return 0;
    }

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Type alias nesting exceeds supported depth.");
        return 1;
    }

    if (alias->is_array)
    {
        if (alias->is_open_array || alias->array_end < alias->array_start)
        {
            codegen_report_error(ctx, "ERROR: Unable to determine size of open array type.");
            return 1;
        }

        long long element_size = 0;
        if (codegen_sizeof_type(ctx, alias->array_element_type, alias->array_element_type_id,
                NULL, &element_size, depth + 1) != 0)
            return 1;

        long long count = (long long)alias->array_end - (long long)alias->array_start + 1;
        if (count < 0)
        {
            codegen_report_error(ctx, "ERROR: Invalid bounds for array type during size computation.");
            return 1;
        }

        *size_out = element_size * count;
        return 0;
    }

    if (alias->base_type != UNKNOWN_TYPE)
        return codegen_sizeof_type(ctx, alias->base_type, NULL, NULL, size_out, depth + 1);

    if (alias->target_type_id != NULL)
        return codegen_sizeof_type(ctx, UNKNOWN_TYPE, alias->target_type_id, NULL,
            size_out, depth + 1);

    codegen_report_error(ctx, "ERROR: Unable to resolve type alias size.");
    return 1;
}

static int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
    long long *size_out, int depth)
{
    if (size_out == NULL || node == NULL)
        return 1;

    if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    {
        codegen_report_error(ctx, "ERROR: Type resolution exceeded supported recursion depth.");
        return 1;
    }

    /* PREFERRED PATH: Try using KgpcType directly if available */
    if (node->type != NULL)
    {
        long long size = kgpc_type_sizeof(node->type);
        if (size > 0)
        {
            *size_out = size;
            return 0;
        }
        else if (size == 0)
        {
            /* Zero-sized type */
            *size_out = 0;
            return 0;
        }
        /* else size < 0: kgpc_type_sizeof couldn't determine size, fall through to legacy path */
    }

    /* LEGACY PATH: KgpcType not available or couldn't determine size */

    /* Check if this is an array */
    int is_array = hashnode_is_array(node);
    
    if (is_array)
        return codegen_sizeof_array_node(ctx, node, size_out, depth);

    if (node->hash_type == HASHTYPE_TYPE)
    {
        struct RecordType *record = codegen_get_record_type_from_node(node);
        if (record != NULL)
            return codegen_sizeof_record(ctx, record, size_out, depth + 1);
        struct TypeAlias *alias = codegen_get_type_alias_from_node(node);
        if (alias != NULL)
            return codegen_sizeof_alias(ctx, alias, size_out, depth + 1);
    }

    if (codegen_node_is_record_type(node))
    {
        struct RecordType *record_type = codegen_get_record_type_from_node(node);
        if (record_type != NULL)
            return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);
    }

    struct TypeAlias *alias = codegen_get_type_alias_from_node(node);
    if (alias != NULL)
        return codegen_sizeof_alias(ctx, alias, size_out, depth + 1);

    if (node->type != NULL)
    {
        long long base = kgpc_type_sizeof(node->type);
        if (base >= 0)
        {
            *size_out = base;
            return 0;
        }
    }
    else
    {
        codegen_report_error(ctx, "ERROR: Symbol %s has no type information.",
            node->id != NULL ? node->id : "");
        return 1;
    }

    codegen_report_error(ctx, "ERROR: Unable to determine size for symbol %s.",
        node->id != NULL ? node->id : "");
    return 1;
}

int codegen_get_record_size(CodeGenContext *ctx, struct Expression *expr,
    long long *size_out)
{
    if (expr == NULL || size_out == NULL)
        return 1;

    /* ShortString uses fixed 256-byte storage (length byte + 255 chars). */
    if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
    {
        *size_out = 256;
        return 0;
    }

    if (kgpc_getenv("KGPC_DEBUG_RECORD_SIZE") != NULL)
    {
        KgpcType *dbg_type = expr_get_kgpc_type(expr);
        struct RecordType *dbg_record = codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
        fprintf(stderr,
            "[KGPC_DEBUG_RECORD_SIZE] expr_type=%d resolved_tag=%d record_type=%p kgpc=%s\n",
            expr->type, codegen_tag_from_kgpc(dbg_type), (void *)dbg_record,
            dbg_type != NULL ? kgpc_type_to_string(dbg_type) : "<null>");
        if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
            fprintf(stderr, "[KGPC_DEBUG_RECORD_SIZE]   id=%s\n", expr->expr_data.id);
        else if (expr->type == EXPR_RECORD_ACCESS &&
            expr->expr_data.record_access_data.field_id != NULL)
            fprintf(stderr, "[KGPC_DEBUG_RECORD_SIZE]   field=%s\n",
                expr->expr_data.record_access_data.field_id);
    }

    /* Check resolved_kgpc_type first — the semantic checker may have
     * rewritten the type (e.g. interface identifier → TGUID). */
    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (expr_type != NULL)
    {
        if (kgpc_type_is_shortstring(expr_type))
        {
            long long short_size = kgpc_type_sizeof(expr_type);
            *size_out = (short_size > 1 && short_size <= INT_MAX) ? short_size : 256;
            return 0;
        }
        if (kgpc_type_is_string(expr_type))
        {
            /* AnsiString/UnicodeString are pointer-sized */
            *size_out = CODEGEN_POINTER_SIZE_BYTES;
            return 0;
        }
        if (kgpc_type_is_record(expr_type))
            return codegen_sizeof_record(ctx, expr_type->info.record_info, size_out, 0);
        if (kgpc_type_is_pointer(expr_type) && expr_type->info.points_to != NULL &&
            kgpc_type_is_record(expr_type->info.points_to))
            return codegen_sizeof_record(ctx, expr_type->info.points_to->info.record_info, size_out, 0);
    }

    struct RecordType *expr_record = codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
    if (expr_record != NULL)
        return codegen_sizeof_record(ctx, expr_record, size_out, 0);

    if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 && node != NULL)
            return codegen_sizeof_hashnode(ctx, node, size_out, 0);
    }

    if (expr->type == EXPR_POINTER_DEREF)
    {
        if (expr->pointer_subtype_id != NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, expr->pointer_subtype_id) != 0 && node != NULL)
                return codegen_sizeof_hashnode(ctx, node, size_out, 0);
        }

        struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
        while (pointer_expr != NULL && pointer_expr->type == EXPR_TYPECAST &&
            pointer_expr->expr_data.typecast_data.expr != NULL)
        {
            pointer_expr = pointer_expr->expr_data.typecast_data.expr;
        }

        if (pointer_expr != NULL)
        {
            struct RecordType *pointer_record = codegen_expr_record_type(pointer_expr,
                ctx != NULL ? ctx->symtab : NULL);
            if (pointer_record != NULL)
                return codegen_sizeof_record(ctx, pointer_record, size_out, 0);

            if (pointer_expr->pointer_subtype_id != NULL && ctx != NULL && ctx->symtab != NULL)
            {
                HashNode_t *node = NULL;
                if (FindSymbol(&node, ctx->symtab, pointer_expr->pointer_subtype_id) != 0 && node != NULL)
                    return codegen_sizeof_hashnode(ctx, node, size_out, 0);
            }
        }
    }

    if (expr->type == EXPR_ARRAY_ACCESS)
    {
        struct RecordType *elem_record = expr->array_element_record_type;
        if (elem_record == NULL && ctx != NULL && ctx->symtab != NULL &&
            expr->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, expr->array_element_type_id) != 0 && type_node != NULL)
            {
                elem_record = codegen_get_record_type_from_node(type_node);
                if (elem_record == NULL && type_node->type != NULL &&
                    kgpc_type_is_record(type_node->type))
                {
                    elem_record = kgpc_type_get_record(type_node->type);
                }
                if (elem_record == NULL)
                {
                    struct TypeAlias *alias = codegen_get_type_alias_from_node(type_node);
                    if (alias != NULL)
                    {
                        if (alias->inline_record_type != NULL)
                            elem_record = alias->inline_record_type;
                        else if (alias->target_type_id != NULL)
                        {
                            HashNode_t *target_node = NULL;
                            if (FindSymbol(&target_node, ctx->symtab, alias->target_type_id) != 0 &&
                                target_node != NULL)
                            {
                                elem_record = codegen_get_record_type_from_node(target_node);
                                if (elem_record == NULL && target_node->type != NULL &&
                                    kgpc_type_is_record(target_node->type))
                                {
                                    elem_record = kgpc_type_get_record(target_node->type);
                                }
                            }
                        }
                    }
                }
            }
        }
        if (elem_record == NULL && ctx != NULL)
        {
            struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
            struct RecordField *field = codegen_lookup_record_field_expr(array_expr, ctx);
            if (field != NULL)
            {
                if (field->array_element_record != NULL)
                    elem_record = field->array_element_record;
                else if (field->array_element_type_id != NULL && ctx->symtab != NULL)
                {
                    HashNode_t *field_node = NULL;
                    if (FindSymbol(&field_node, ctx->symtab, field->array_element_type_id) != 0 &&
                        field_node != NULL)
                    {
                        elem_record = codegen_get_record_type_from_node(field_node);
                        if (elem_record == NULL && field_node->type != NULL &&
                            kgpc_type_is_record(field_node->type))
                        {
                            elem_record = kgpc_type_get_record(field_node->type);
                        }
                    }
                }
            }
        }
        if (elem_record != NULL)
            return codegen_sizeof_record(ctx, elem_record, size_out, 0);
    }

    if (expr->type == EXPR_TYPECAST && expr->expr_data.typecast_data.expr != NULL)
        return codegen_get_record_size(ctx, expr->expr_data.typecast_data.expr, size_out);

    codegen_report_error(ctx, "ERROR: Unable to determine size for record expression.");
    return 1;
}

int codegen_sizeof_pointer_target(CodeGenContext *ctx, struct Expression *pointer_expr,
    long long *size_out)
{
    if (pointer_expr == NULL || size_out == NULL)
        return 1;

    KgpcType *pointer_type = expr_get_kgpc_type(pointer_expr);
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type))
    {
        KgpcType *points_to = pointer_type->info.points_to;
        if (points_to != NULL)
        {
            long long pointee_size = kgpc_type_sizeof(points_to);
            if (pointee_size > 0)
            {
                *size_out = pointee_size;
                return 0;
            }
        }
    }

    int subtype = pointer_expr->pointer_subtype;
    const char *type_id = pointer_expr->pointer_subtype_id;
    struct RecordType *record_type = codegen_expr_record_type(pointer_expr,
        ctx != NULL ? ctx->symtab : NULL);

    if (record_type == NULL && type_id != NULL && ctx != NULL && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, type_id) != 0 && node != NULL)
            record_type = codegen_get_record_type_from_node(node);
    }

    if (record_type == NULL && subtype == RECORD_TYPE && type_id == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine record size for pointer target.");
        return 1;
    }

    /* For untyped pointers (Pointer type with no target info), return failure
     * without reporting an error — the caller handles this by defaulting to step=1 */
    if (subtype == UNKNOWN_TYPE && type_id == NULL && record_type == NULL)
        return 1;

    return codegen_sizeof_type(ctx, subtype, type_id, record_type, size_out, 0);
}

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_record_access(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg);
ListNode_t *codegen_record_field_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg);

/* Lookup the RecordField metadata for a record access expression */
static struct RecordField *codegen_lookup_record_field_expr(struct Expression *record_access_expr,
    CodeGenContext *ctx)
{
    if (record_access_expr == NULL ||
        record_access_expr->type != EXPR_RECORD_ACCESS ||
        record_access_expr->expr_data.record_access_data.field_id == NULL)
        return NULL;

    const char *field_id = record_access_expr->expr_data.record_access_data.field_id;
    SymTab_t *symtab = (ctx != NULL) ? ctx->symtab : NULL;
    struct RecordType *record = codegen_expr_record_type(record_access_expr, symtab);
    if (record == NULL && record_access_expr->expr_data.record_access_data.record_expr != NULL)
        record = codegen_expr_record_type(record_access_expr->expr_data.record_access_data.record_expr, symtab);
    if (record == NULL && kgpc_getenv("KGPC_DEBUG_ARRAY_ACCESS") != NULL)
    {
        struct Expression *rec_expr = record_access_expr->expr_data.record_access_data.record_expr;
        fprintf(stderr,
            "[KGPC_DEBUG_ARRAY_ACCESS] record_field_lookup failed: field=%s rec_type=%d rec_kgpc=%s rec_type_id=%s\n",
            field_id,
            rec_expr != NULL ? rec_expr->type : -1,
            (rec_expr != NULL && rec_expr->resolved_kgpc_type != NULL)
                ? kgpc_type_to_string(rec_expr->resolved_kgpc_type) : "<null>",
            (rec_expr != NULL && rec_expr->type == EXPR_TYPECAST &&
             rec_expr->expr_data.typecast_data.target_type_id != NULL)
                ? rec_expr->expr_data.typecast_data.target_type_id : "<null>");
    }
    if (record == NULL)
        return NULL;

    ListNode_t *cur = record->fields;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field->name != NULL && pascal_identifier_equals(field->name, field_id))
                return field;
        }
        cur = cur->next;
    }
    return NULL;
}

static struct RecordField *codegen_lookup_record_field_in_members(ListNode_t *members,
    const char *field_id)
{
    for (ListNode_t *cur = members; cur != NULL; cur = cur->next)
    {
        if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field->name != NULL && pascal_identifier_equals(field->name, field_id))
                return field;
        }
        else if (cur->type == LIST_VARIANT_PART && cur->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)cur->cur;
            for (ListNode_t *b = variant->branches; b != NULL; b = b->next)
            {
                if (b->type != LIST_VARIANT_BRANCH || b->cur == NULL)
                    continue;
                struct VariantBranch *branch = (struct VariantBranch *)b->cur;
                struct RecordField *field =
                    codegen_lookup_record_field_in_members(branch->members, field_id);
                if (field != NULL)
                    return field;
            }
        }
    }
    return NULL;
}

static struct RecordField *codegen_lookup_record_field(struct RecordType *record,
    const char *field_id)
{
    if (record == NULL || field_id == NULL)
        return NULL;
    return codegen_lookup_record_field_in_members(record->fields, field_id);
}

static struct RecordField *codegen_find_unique_record_field(SymTab_t *symtab,
    const char *field_id, struct RecordType **out_record)
{
    if (symtab == NULL || field_id == NULL)
        return NULL;

    struct RecordField *found_field = NULL;
    struct RecordType *found_record = NULL;

    HashTable_t *tables[2];
    tables[0] = symtab->builtin_scope->table;
    tables[1] = NULL;

    ScopeNode *scope = symtab->current_scope;
    while (scope != NULL)
    {
        tables[1] = scope->table;
        for (int t = 0; t < 2; ++t)
        {
            HashTable_t *table = tables[t];
            if (table == NULL)
                continue;
            for (int i = 0; i < TABLE_SIZE; ++i)
            {
                ListNode_t *node_list = table->table[i];
                while (node_list != NULL)
                {
                    HashNode_t *node = (HashNode_t *)node_list->cur;
                    if (node != NULL && node->hash_type == HASHTYPE_TYPE)
                    {
                        struct RecordType *record = codegen_get_record_type_from_node(node);
                        if (record != NULL)
                        {
                            struct RecordField *field = codegen_lookup_record_field(record, field_id);
                            if (field != NULL)
                            {
                                if (found_field != NULL && found_record != record)
                                {
                                    if (field->is_pointer == found_field->is_pointer &&
                                        field->is_array == found_field->is_array &&
                                        field->pointer_type == found_field->pointer_type &&
                                        ((field->pointer_type_id == NULL && found_field->pointer_type_id == NULL) ||
                                         (field->pointer_type_id != NULL && found_field->pointer_type_id != NULL &&
                                          pascal_identifier_equals(field->pointer_type_id, found_field->pointer_type_id))) &&
                                        field->array_element_type == found_field->array_element_type &&
                                        ((field->array_element_type_id == NULL && found_field->array_element_type_id == NULL) ||
                                         (field->array_element_type_id != NULL && found_field->array_element_type_id != NULL &&
                                          pascal_identifier_equals(field->array_element_type_id, found_field->array_element_type_id))) &&
                                        field->array_element_record == found_field->array_element_record)
                                    {
                                        found_field = field;
                                        found_record = record;
                                        break;
                                    }
                                    return NULL;
                                }
                                found_field = field;
                                found_record = record;
                            }
                        }
                    }
                    node_list = node_list->next;
                }
            }
        }
        scope = scope->parent;
    }

    if (found_field != NULL && out_record != NULL)
        *out_record = found_record;
    return found_field;
}

struct RecordField *codegen_lookup_with_field(CodeGenContext *ctx,
    const char *field_id, struct RecordType **out_record)
{
    if (ctx == NULL || field_id == NULL || ctx->with_depth <= 0)
        return NULL;
    if (kgpc_getenv("KGPC_DEBUG_WITH_CODEGEN") != NULL)
    {
        fprintf(stderr, "[KGPC_DEBUG_WITH_CODEGEN] lookup field=%s depth=%d\n",
            field_id, ctx->with_depth);
    }
    for (int i = ctx->with_depth; i > 0; --i)
    {
        struct RecordType *record = ctx->with_stack[i - 1].record_type;
        struct RecordField *field = codegen_lookup_record_field(record, field_id);
        if (field != NULL)
        {
            if (out_record != NULL)
                *out_record = record;
            return field;
        }
    }
    return NULL;
}

long long codegen_array_elem_size_from_field(struct RecordField *field, CodeGenContext *ctx)
{
    if (field == NULL)
        return -1;
    if (field->is_pointer)
    {
        if (field->pointer_type != UNKNOWN_TYPE)
        {
            long long tag_size = codegen_sizeof_type_tag(field->pointer_type);
            if (tag_size > 0)
                return tag_size;
        }
        if (ctx != NULL && ctx->symtab != NULL && field->pointer_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, field->pointer_type_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                KgpcType *points_to = NULL;
                if (kgpc_type_is_pointer(type_node->type))
                    points_to = type_node->type->info.points_to;
                if (points_to != NULL)
                {
                    long long elem_size = kgpc_type_sizeof(points_to);
                    if (elem_size > 0)
                        return elem_size;
                }
                if (kgpc_type_is_array(type_node->type))
                {
                    long long elem_size = kgpc_type_get_array_element_size(type_node->type);
                    if (elem_size <= 0)
                    {
                        KgpcType *elem_type =
                            kgpc_type_get_array_element_type_resolved(type_node->type,
                                ctx->symtab);
                        if (elem_type != NULL)
                            elem_size = kgpc_type_sizeof(elem_type);
                    }
                    if (elem_size > 0)
                        return elem_size;
                }
                {
                    long long node_size = kgpc_type_sizeof(type_node->type);
                    if (node_size > 0)
                        return node_size;
                }
            }
        }
    }
    else if (field->is_array)
    {
        if (field->array_element_type != UNKNOWN_TYPE)
        {
            long long tag_size = codegen_sizeof_type_tag(field->array_element_type);
            if (tag_size > 0)
                return tag_size;
        }
        if (ctx != NULL && ctx->symtab != NULL && field->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, field->array_element_type_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                long long elem_size = kgpc_type_sizeof(type_node->type);
                if (elem_size > 0)
                    return elem_size;
            }
        }
        if (field->array_element_record != NULL)
        {
            long long elem_size = 0;
            if (codegen_sizeof_record(ctx, field->array_element_record, &elem_size, 0) == 0 &&
                elem_size > 0)
                return elem_size;
        }
    }
    return -1;
}

/* Best-effort size for a record field, respecting packed/range aliases */
static long long codegen_record_field_effective_size(struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL)
        return expr_effective_size_bytes(expr);

    struct RecordField *field = codegen_lookup_record_field_expr(expr, ctx);
    if (field == NULL && expr->expr_data.record_access_data.field_id != NULL &&
        expr->expr_data.record_access_data.record_expr != NULL &&
        ctx != NULL && ctx->symtab != NULL)
    {
        struct RecordType *record = codegen_expr_record_type(
            expr->expr_data.record_access_data.record_expr, ctx->symtab);
        if (record != NULL)
            field = semcheck_find_class_field_including_hidden(ctx->symtab, record,
                expr->expr_data.record_access_data.field_id, NULL);
    }
    long long field_size = 0;
    if (field != NULL && !field->is_array)
    {
        const char *field_type_id = field->type_id;
        if (field_type_id == NULL && field->type_ref != NULL)
            field_type_id = type_ref_base_name(field->type_ref);

        if (expr->resolved_kgpc_type != NULL &&
            expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
            expr->resolved_kgpc_type->info.primitive_type_tag == REAL_TYPE)
        {
            long long resolved_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
            if (resolved_size > 0)
                return resolved_size;
        }

        if (field->type == REAL_TYPE && field_type_id != NULL)
        {
            if (pascal_identifier_equals(field_type_id, "Single"))
                return 4;
            if (pascal_identifier_equals(field_type_id, "Double") ||
                pascal_identifier_equals(field_type_id, "Real"))
                return 8;
        }

        if (ctx->symtab != NULL && field_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, field_type_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                long long type_size = kgpc_type_sizeof(type_node->type);
                if (type_size > 0)
                    return type_size;
            }
        }

        struct RecordType *nested = field->nested_record;
        if (codegen_sizeof_type_reference(ctx, field->type, field->type_id, nested, &field_size) == 0 &&
            field_size > 0)
            return field_size;
    }

    long long size = expr_effective_size_bytes(expr);
    if (size > 0)
        return size;
    return field_size;
}


/* Code generation for expressions */
static const char *describe_expression_kind(const struct Expression *expr)
{
    if (expr == NULL)
        return "unknown";

    switch (expr->type)
    {
        case EXPR_VAR_ID:
            return "variable reference";
        case EXPR_ARRAY_ACCESS:
            return "array access";
        case EXPR_FUNCTION_CALL:
            return "function call";
        case EXPR_ADDOP:
            return "additive expression";
        case EXPR_MULOP:
            return "multiplicative expression";
        case EXPR_SIGN_TERM:
            return "signed term";
        case EXPR_RELOP:
            return "relational expression";
        case EXPR_INUM:
            return "integer literal";
        case EXPR_RNUM:
            return "real literal";
        default:
            return "expression";
    }
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

static ListNode_t *codegen_expr_tree_value(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{

    
    if (expr != NULL)
    {
        if (expr->type == EXPR_IS)
            return codegen_emit_is_expr(expr, inst_list, ctx, out_reg);
        if (expr->type == EXPR_TYPEINFO)
        {
            const char *type_id = expr->expr_data.typeinfo_data.type_id;
            if (type_id == NULL || type_id[0] == '\0')
            {
                codegen_report_error(ctx, "ERROR: TypeInfo missing type identifier.");
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }

            Register_t *tmp_reg = codegen_try_get_reg(&inst_list, ctx, "typeinfo");
            if (tmp_reg == NULL)
            {
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }

            char label[CODEGEN_MAX_INST_BUF];
            codegen_typeinfo_label_for_type_id(ctx->symtab, type_id, label, sizeof(label));
            int buf_len = snprintf(NULL, 0, "\tleaq\t%s(%%rip), %s\n", label, tmp_reg->bit_64);
            if (buf_len > 0)
            {
                char *buffer = (char *)malloc((size_t)buf_len + 1);
                if (buffer != NULL)
                {
                    snprintf(buffer, (size_t)buf_len + 1, "\tleaq\t%s(%%rip), %s\n", label, tmp_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    free(buffer);
                }
            }

            if (out_reg != NULL)
                *out_reg = tmp_reg;
            else
                free_reg(get_reg_stack(), tmp_reg);
            return inst_list;
        }
        if (expr->type == EXPR_ARRAY_LITERAL)
        {
            Register_t *tmp_reg = NULL;
            inst_list = codegen_materialize_array_literal(expr, inst_list, ctx, &tmp_reg);
            if (out_reg != NULL)
                *out_reg = tmp_reg;
            else if (tmp_reg != NULL)
                free_reg(get_reg_stack(), tmp_reg);
            return inst_list;
        }
        if (expr->type == EXPR_AS)
        {
            if (expr->expr_data.as_data.expr == NULL)
                return inst_list;

            if (codegen_expr_is_addressable(expr->expr_data.as_data.expr))
            {
                Register_t *addr_reg = NULL;
                inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
                if (addr_reg == NULL)
                    return inst_list;

                inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);

                if (out_reg != NULL)
                    *out_reg = addr_reg;
                else
                    free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            if (codegen_expr_needs_class_method_vmt_self(expr->expr_data.as_data.expr, ctx))
            {
                Register_t *value_reg = NULL;
                inst_list = codegen_expr_with_result(expr->expr_data.as_data.expr, inst_list, ctx, &value_reg);
                if (value_reg == NULL || codegen_had_error(ctx))
                    return inst_list;

                inst_list = codegen_emit_class_cast_check_from_instance_ptr(expr, inst_list, ctx, value_reg);

                if (out_reg != NULL)
                    *out_reg = value_reg;
                else
                    free_reg(get_reg_stack(), value_reg);
                return inst_list;
            }

            codegen_report_error(ctx,
                "ERROR: RTTI operations currently require addressable class expressions.");
            return inst_list;
        }
    }

    /* ShortString → AnsiString/RawByteString typecast: build_expr_tree strips
     * EXPR_TYPECAST, so the conversion would be lost.  Detect it here and emit
     * a call to kgpc_shortstring_to_string before the generic tree path. */
    if (expr != NULL && expr->type == EXPR_TYPECAST &&
        expr->expr_data.typecast_data.expr != NULL)
    {
        int tc_target = expr->expr_data.typecast_data.target_type;
        struct Expression *tc_inner = expr->expr_data.typecast_data.expr;
        int inner_is_ss = codegen_expr_is_shortstring_value_ctx(tc_inner, ctx);
        if (inner_is_ss && tc_target == STRING_TYPE)
        {
            /* Evaluate the inner ShortString expression to get a pointer to
             * the length-prefixed data, then convert it to a heap AnsiString. */
            Register_t *ss_reg = NULL;
            inst_list = codegen_expr_tree_value(tc_inner, inst_list, ctx, &ss_reg);
            if (ss_reg != NULL)
            {
                inst_list = codegen_promote_shortstring_reg(inst_list, ctx, ss_reg);
                if (out_reg != NULL)
                    *out_reg = ss_reg;
                else
                    free_reg(get_reg_stack(), ss_reg);
            }
            else if (out_reg != NULL)
            {
                *out_reg = NULL;
            }
            return inst_list;
        }
    }

    codegen_begin_expression(ctx);

    expr_node_t *expr_tree = build_expr_tree(expr);

    Register_t *target_reg = codegen_try_get_reg(&inst_list, ctx, describe_expression_kind(expr));
    if (target_reg == NULL)
    {

        free_expr_tree(expr_tree);

        if (out_reg != NULL)
            *out_reg = NULL;
        codegen_end_expression(ctx);
        return inst_list;
    }


    inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);

    free_expr_tree(expr_tree);


    if (out_reg != NULL)
    {
        *out_reg = target_reg;
        codegen_end_expression(ctx);
    }
    else
    {
        codegen_end_expression(ctx);
        free_reg(get_reg_stack(), target_reg);
    }

    return inst_list;
}

static int codegen_dynarray_descriptor_size(const struct Expression *expr)
{
    const int base_size = 4 * DOUBLEWORD;
    if (expr == NULL)
        return base_size;

    if (expr->type == EXPR_VAR_ID)
    {
        int scope_depth = 0;
        StackNode_t *node = find_label_with_depth(expr->expr_data.id, &scope_depth);
        if (node != NULL && node->is_dynamic && node->size > 0)
            return node->size;
    }

    if (expr->array_element_size > 0)
    {
        int descriptor_size = base_size;
        int needed = expr->array_element_size * 2;
        if (descriptor_size < needed)
            descriptor_size = needed;
        return descriptor_size;
    }

    return base_size;
}

static ListNode_t *codegen_expr_via_tree(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    return codegen_expr_tree_value(expr, inst_list, ctx, NULL);
}

ListNode_t *codegen_sign_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg64)
{
    assert(src_reg32 != NULL);
    assert(dst_reg64 != NULL);

    char buffer[CODEGEN_MAX_INST_BUF];
    snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", src_reg32, dst_reg64);
    return add_inst(inst_list, buffer);
}

ListNode_t *codegen_zero_extend32_to64(ListNode_t *inst_list, const char *src_reg32, const char *dst_reg32)
{
    assert(src_reg32 != NULL);
    assert(dst_reg32 != NULL);

    char buffer[CODEGEN_MAX_INST_BUF];
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", src_reg32, dst_reg32);
    return add_inst(inst_list, buffer);
}

int codegen_sizeof_type_reference(CodeGenContext *ctx, int type_tag, const char *type_id,
    struct RecordType *record_type, long long *size_out)
{
    return codegen_sizeof_type(ctx, type_tag, type_id, record_type, size_out, 0);
}

ListNode_t *codegen_pointer_deref_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    struct Expression *pointer_expr = expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
    {
        codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
        return inst_list;
    }

    expr_node_t *pointer_tree = build_expr_tree(pointer_expr);
    Register_t *addr_reg = codegen_try_get_reg(&inst_list, ctx, "pointer dereference");
    if (addr_reg == NULL)
    {
        free_expr_tree(pointer_tree);
        return inst_list;
    }

    inst_list = gencode_expr_tree(pointer_tree, inst_list, ctx, addr_reg);
    free_expr_tree(pointer_tree);

    KgpcType *deref_type = expr_get_kgpc_type(expr);
    if (deref_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(deref_type);
        if (kgpc_type_is_record(deref_type) ||
            kgpc_type_is_array(deref_type) ||
            (deref_type->kind == TYPE_KIND_PRIMITIVE &&
             deref_type->info.primitive_type_tag == SET_TYPE) ||
            kgpc_type_is_shortstring(deref_type) ||
            (alias != NULL && (alias->is_shortstring || alias->is_set)))
        {
            char addr_buf[64];
            snprintf(addr_buf, sizeof(addr_buf), "\tmovq\t%s, %s\n",
                addr_reg->bit_64, target_reg->bit_64);
            inst_list = add_inst(inst_list, addr_buf);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
    }

    long long load_size = expr_effective_size_bytes(expr);
    char buffer[CODEGEN_MAX_INST_BUF];
    if (load_size == 1)
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    else if (load_size == 2)
    {
        const int is_signed = expr_is_signed_kgpctype(expr);
        snprintf(buffer, sizeof(buffer), "\t%s\t(%s), %s\n",
            is_signed ? "movswl" : "movzwl", addr_reg->bit_64, target_reg->bit_32);
    }
    else if (expr_uses_qword_kgpctype(expr))
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

ListNode_t *codegen_addressof_leaf(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    struct Expression *inner = expr->expr_data.addr_data.expr;
    if (inner == NULL)
    {
        codegen_report_error(ctx, "ERROR: Address-of operator missing operand.");
        return inst_list;
    }

    char buffer[CODEGEN_MAX_INST_BUF];
    if (inner->type == EXPR_VAR_ID)
    {
        if (ctx != NULL && ctx->symtab != NULL)
        {
            const char *proc_label = NULL;
            char *resolved_label = NULL;
            HashNode_t *sym = NULL;
            if (FindSymbol(&sym, ctx->symtab, inner->expr_data.id) != 0 &&
                sym != NULL && sym->mangled_id != NULL && sym->type != NULL &&
                sym->type->kind == TYPE_KIND_PROCEDURE)
            {
                proc_label = sym->mangled_id;
            }
            else if (ctx->current_subprogram_owner_class != NULL)
            {
                const char *impl_target = codegen_find_class_method_impl_id(
                    ctx->symtab, NULL, ctx->current_subprogram_owner_class, NULL,
                    inner->expr_data.id);
                if (impl_target != NULL)
                    proc_label = impl_target;
                else
                {
                    int needed = snprintf(NULL, 0, "%s__%s",
                        ctx->current_subprogram_owner_class, inner->expr_data.id) + 1;
                    resolved_label = malloc((size_t)needed);
                    if (resolved_label != NULL)
                    {
                        snprintf(resolved_label, (size_t)needed, "%s__%s",
                            ctx->current_subprogram_owner_class, inner->expr_data.id);
                        ListNode_t *candidates = FindAllIdents(ctx->symtab, resolved_label);
                        int found = 0;
                        for (ListNode_t *c = candidates; c != NULL; c = c->next)
                        {
                            HashNode_t *cand = (HashNode_t *)c->cur;
                            if (cand != NULL && cand->mangled_id != NULL &&
                                cand->type != NULL &&
                                cand->type->kind == TYPE_KIND_PROCEDURE)
                            {
                                proc_label = cand->mangled_id;
                                found = 1;
                                break;
                            }
                        }
                        if (candidates != NULL)
                            DestroyList(candidates);
                        if (!found)
                        {
                            free(resolved_label);
                            resolved_label = NULL;
                        }
                    }
                }
            }
            if (proc_label != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                    proc_label, target_reg->bit_64);
                if (resolved_label != NULL)
                    free(resolved_label);
                return add_inst(inst_list, buffer);
            }
        }

        StackNode_t *var_node = find_label(inner->expr_data.id);
        if (var_node == NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *sym_node = NULL;
            if (FindSymbol(&sym_node, ctx->symtab, inner->expr_data.id) != 0 &&
                sym_node != NULL && sym_node->mangled_id != NULL)
            {
                var_node = find_label(sym_node->mangled_id);
            }
        }
        if (var_node != NULL)
        {
            if (var_node->is_static)
            {
                const char *label = (var_node->static_label != NULL) ?
                    var_node->static_label : var_node->label;
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                    label, target_reg->bit_64);
            }
            else if (var_node->is_reference)
            {
                /* For var/out/constref parameters, the stack slot already contains
                 * a pointer to the actual variable. So @param returns the VALUE
                 * stored in the slot (the pointer), not the address of the slot. */
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    var_node->offset, target_reg->bit_64);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                    var_node->offset, target_reg->bit_64);
            }
            return add_inst(inst_list, buffer);
        }
        else
        {
            /* Check for string constants before falling through to nonlocal */
            if (ctx != NULL && ctx->symtab != NULL)
            {
                HashNode_t *node = NULL;
                if (FindSymbol(&node, ctx->symtab, inner->expr_data.id) != 0 &&
                    node != NULL && node->hash_type == HASHTYPE_CONST &&
                    node->const_string_value != NULL &&
                    !(node->type != NULL && node->type->kind == TYPE_KIND_PROCEDURE))
                {
                    int label_id = ctx->write_label_counter++;
                    char str_label[32];
                    char ptr_label[40];
                    snprintf(str_label, sizeof(str_label), ".LC%d", label_id);
                    snprintf(ptr_label, sizeof(ptr_label), ".LC%d_ptr", label_id);
                    char rodata_buf[1024];
                    const char *readonly_section = codegen_readonly_section_directive();
                    char escaped[512];
                    escape_string(escaped, node->const_string_value, sizeof(escaped));
                    snprintf(rodata_buf, sizeof(rodata_buf),
                        "%s\n%s:\n\t.string \"%s\"\n%s:\n\t.quad\t%s\n\t.text\n",
                        readonly_section, str_label, escaped, ptr_label, str_label);
                    inst_list = add_inst(inst_list, rodata_buf);
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                        ptr_label, target_reg->bit_64);
                    return add_inst(inst_list, buffer);
                }
            }

            int offset = 0;
            inst_list = codegen_get_nonlocal(inst_list, inner->expr_data.id, &offset, ctx);
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%s), %s\n", offset, current_non_local_reg64(), target_reg->bit_64);
            return add_inst(inst_list, buffer);
        }
    }
    else if (inner->type == EXPR_ARRAY_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_array_element_address(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    else if (inner->type == EXPR_RECORD_ACCESS)
    {
        Register_t *addr_reg = NULL;
        inst_list = codegen_record_field_address(inner, inst_list, ctx, &addr_reg);
        if (codegen_had_error(ctx) || addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    else if (inner->type == EXPR_POINTER_DEREF)
    {
        struct Expression *pointer_expr = inner->expr_data.pointer_deref_data.pointer_expr;
        if (pointer_expr == NULL)
        {
            codegen_report_error(ctx, "ERROR: Pointer dereference missing operand.");
            return inst_list;
        }

        Register_t *addr_reg = NULL;
        inst_list = codegen_expr_tree_value(pointer_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            return inst_list;

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    codegen_report_error(ctx, "ERROR: Unsupported operand for address-of operator.");
    return inst_list;
}

/* Recompute the field offset for a class var access when the CLASSVAR storage
 * only contains class var fields (not the full instance layout).  Returns the
 * class-var-only offset for the field named `field_id`, or -1 if not found. */
ListNode_t *codegen_record_field_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
        return inst_list;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    if (record_expr == NULL)
        return inst_list;

    /* Check if this is a class field access. Classes are pointers, so we need to load
     * the instance pointer from variable storage for VAR_ID expressions. */
    struct RecordType *record_expr_record = codegen_expr_record_type(record_expr,
        ctx != NULL ? ctx->symtab : NULL);
    int is_class_field = (record_expr_record != NULL &&
                          record_type_is_class(record_expr_record));

    int is_type_ref = 0;
    const char *type_label = NULL;
    if (record_expr->type == EXPR_VAR_ID &&
        record_expr->expr_data.id != NULL && ctx->symtab != NULL)
    {
        HashNode_t *symbol = NULL;
        if (FindSymbol(&symbol, ctx->symtab, record_expr->expr_data.id) != 0 &&
            symbol != NULL && symbol->hash_type == HASHTYPE_TYPE)
        {
            is_type_ref = 1;
            if (record_expr_record != NULL && record_expr_record->type_id != NULL)
                type_label = record_expr_record->type_id;
            else
                type_label = record_expr->expr_data.id;
        }
    }
    
    Register_t *addr_reg = NULL;
    if (is_type_ref && type_label != NULL)
    {
        addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate register for class var access.");
            return inst_list;
        }

        char buffer[96];
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_CLASSVAR(%%rip), %s\n",
            type_label, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        inst_list = codegen_address_for_expr(record_expr, inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
            return inst_list;
    }

    /* For class types, addr_reg points to the variable holding the pointer when the
     * record expression is a VAR_ID, RECORD_ACCESS, or ARRAY_ACCESS yielding a
     * class-typed field/element. Load the pointer value to get the instance.
     * Non-var/non-record-access/non-array-access expressions (casts, function calls)
     * already yield the pointer value. */
    int needs_class_deref = (is_class_field && !is_type_ref);
    if (needs_class_deref && record_expr->type != EXPR_VAR_ID &&
        record_expr->type != EXPR_RECORD_ACCESS &&
        record_expr->type != EXPR_ARRAY_ACCESS)
        needs_class_deref = 0;
    if (needs_class_deref)
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    long long offset = expr->expr_data.record_access_data.field_offset;
    /* When accessing a class var via type reference (TMyClass.FValue), the
     * CLASSVAR storage layout starts at offset 0, but the semcheck computes
     * field offsets with a POINTER_SIZE_BYTES (8) prefix for the VMT pointer
     * (since classes have an implicit VMT slot at offset 0 in instances).
     * Subtract the VMT size so the offset matches the CLASSVAR layout. */
    if (is_type_ref && is_class_field && offset >= 8)
        offset -= 8;
    if (offset != 0)
    {
        char buffer[128];
        if (offset > INT32_MAX || offset < INT32_MIN)
        {
            /* x86-64 addq only accepts 32-bit sign-extended immediates.
             * For larger offsets, load into a scratch register first. */
            snprintf(buffer, sizeof(buffer), "\tmovabsq\t$%lld, %%r11\n", offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\taddq\t%%r11, %s\n", addr_reg->bit_64);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", offset, addr_reg->bit_64);
        }
        inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = addr_reg;
    return inst_list;
}

ListNode_t *codegen_record_access(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t *target_reg)
{
    if (expr == NULL || ctx == NULL || target_reg == NULL)
        return inst_list;

    if (expr_has_type_tag(expr, RECORD_TYPE))
    {
        struct RecordType *record_type = codegen_expr_record_type(expr,
            ctx != NULL ? ctx->symtab : NULL);
        if (record_type == NULL || !record_type_is_class(record_type))
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_record_field_address(expr, inst_list, ctx, &addr_reg);
            if (addr_reg == NULL)
                return inst_list;

            char buffer[64];
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                addr_reg->bit_64, target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
    }

    Register_t *addr_reg = NULL;
    inst_list = codegen_record_field_address(expr, inst_list, ctx, &addr_reg);
    if (addr_reg == NULL)
        return inst_list;

    if (expr_has_type_tag(expr, SHORTSTRING_TYPE))
    {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
            addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    char buffer[64];
    long long field_size = codegen_record_field_effective_size(expr, ctx);
    /* Cross-check with resolved_kgpc_type for sub-dword types that
       codegen_record_field_effective_size may miss (e.g. Integer=SmallInt in FPC mode) */
    if (field_size == 4 && expr->resolved_kgpc_type != NULL)
    {
        long long resolved_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
        if (resolved_size > 0 && resolved_size < 4)
            field_size = resolved_size;
    }
    /* Ensure REAL fields honor resolved size (Single vs Double) */
    if (expr_has_type_tag(expr, REAL_TYPE) && expr->resolved_kgpc_type != NULL)
    {
        long long resolved_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
        if (resolved_size == 4 || resolved_size == 8)
            field_size = resolved_size;
    }
    int type_tag = expr_get_type_tag(expr);
    int is_single_real_field = (expr_has_type_tag(expr, REAL_TYPE) && field_size == 4);
    if (!is_single_real_field && (expr_uses_qword_kgpctype(expr) || field_size == 8))
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
    else if (field_size == 1 || expr_has_type_tag(expr, CHAR_TYPE))
    {
        if (type_tag != CHAR_TYPE && codegen_type_is_signed(type_tag))
            snprintf(buffer, sizeof(buffer), "\tmovsbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        else
            snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    }
    else if (field_size == 2)
    {
        if (codegen_type_is_signed(type_tag))
            snprintf(buffer, sizeof(buffer), "\tmovswl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        else
            snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    }
    else
        snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static ListNode_t *codegen_set_emit_single(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *value_reg)
{
    if (dest_reg == NULL || value_reg == NULL)
        return inst_list;

    char buffer[128];
    char skip_label[18];
    gen_label(skip_label, sizeof(skip_label), ctx);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", skip_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", value_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", skip_label);
    inst_list = add_inst(inst_list, buffer);

    /* Use btsl to set the bit (avoids %ecx shift register conflict) */
    snprintf(buffer, sizeof(buffer), "\tbtsl\t%s, %s\n", value_reg->bit_32, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
}

static ListNode_t *codegen_set_emit_range(ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_reg, Register_t *start_reg, Register_t *end_reg)
{
    if (dest_reg == NULL || start_reg == NULL || end_reg == NULL)
        return inst_list;

    Register_t *temp_reg = codegen_try_get_reg(&inst_list, ctx, "set range temp");
    if (temp_reg == NULL)
        return inst_list;

    char order_label[18];
    char loop_label[18];
    char done_label[18];
    char start_floor_label[18];
    char end_cap_label[18];
    gen_label(order_label, sizeof(order_label), ctx);
    gen_label(loop_label, sizeof(loop_label), ctx);
    gen_label(done_label, sizeof(done_label), ctx);
    gen_label(start_floor_label, sizeof(start_floor_label), ctx);
    gen_label(end_cap_label, sizeof(end_cap_label), ctx);

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", order_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", start_reg->bit_32, temp_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", temp_reg->bit_32, end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", order_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjl\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjge\t%s\n", start_floor_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, %s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", start_floor_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tcmpl\t$31, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjle\t%s\n", end_cap_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$31, %s\n", end_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", end_cap_label);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "%s:\n", loop_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjg\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    /* Use btsl to set the bit (avoids %ecx shift register conflict) */
    snprintf(buffer, sizeof(buffer), "\tbtsl\t%s, %s\n", start_reg->bit_32, dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", end_reg->bit_32, start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tje\t%s\n", done_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tincl\t%s\n", start_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", loop_label);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
    inst_list = add_inst(inst_list, buffer);

    free_reg(get_reg_stack(), temp_reg);
    return inst_list;
}

ListNode_t *codegen_set_literal(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg, int force_char_set)
{
    if (expr == NULL)
        return inst_list;

    /* Check if this is a character set literal */
    int is_char_set = force_char_set || expr_is_char_set_ctx(expr, ctx);
    
    if (is_char_set)
    {
        /* Character sets need 32 bytes in memory, not a register */
        /* Allocate a temporary 32-byte buffer on the stack */
        StackNode_t *char_set_temp = codegen_alloc_record_temp(32);
        if (char_set_temp == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }
        
        /* Zero-initialize all 32 bytes */
        char buffer[128];
        for (int i = 0; i < 8; i++)
        {
            int offset = char_set_temp->offset - (i * 4);
            snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n", offset);
            inst_list = add_inst(inst_list, buffer);
        }
        
        /* Get address register for the set buffer */
        Register_t *addr_reg = codegen_try_get_reg(&inst_list, ctx, "char set addr");
        if (addr_reg == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }
        
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n", 
            char_set_temp->offset, addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        
        /* Now set each element in the character set */
        for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next)
        {
            struct SetElement *element = (struct SetElement *)cur->cur;
            if (element == NULL)
                continue;

            Register_t *value_reg = NULL;
            inst_list = codegen_expr_tree_value(element->lower, inst_list, ctx, &value_reg);
            if (codegen_had_error(ctx) || value_reg == NULL)
            {
                if (value_reg != NULL)
                    free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
            
            /* For character sets: Calculate dword index and bit index */
            Register_t *bit_reg = codegen_try_get_reg(&inst_list, ctx, "char set bit");
            Register_t *dword_reg = codegen_try_get_reg(&inst_list, ctx, "char set dword");
            if (bit_reg == NULL || dword_reg == NULL)
            {
                if (bit_reg != NULL)
                    free_reg(get_reg_stack(), bit_reg);
                if (dword_reg != NULL)
                    free_reg(get_reg_stack(), dword_reg);
                free_reg(get_reg_stack(), value_reg);
                free_reg(get_reg_stack(), addr_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
            
            /* btsl with memory operand auto-computes byte offset for any bit position.
               Only need value_reg and addr_reg — no separate bit_reg/dword_reg. */
            snprintf(buffer, sizeof(buffer), "\tbtsl\t%s, (%s)\n", value_reg->bit_32, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), dword_reg);
            free_reg(get_reg_stack(), bit_reg);
            free_reg(get_reg_stack(), value_reg);
            
            /* Handle ranges (element->upper != NULL): set bits from lower+1 to upper
               (lower bit was already set by btsl above). */
            if (element->upper != NULL)
            {
                Register_t *upper_reg = NULL;
                inst_list = codegen_expr_tree_value(element->upper, inst_list, ctx, &upper_reg);
                if (codegen_had_error(ctx) || upper_reg == NULL)
                {
                    if (upper_reg != NULL)
                        free_reg(get_reg_stack(), upper_reg);
                    free_reg(get_reg_stack(), addr_reg);
                    if (out_reg != NULL)
                        *out_reg = NULL;
                    return inst_list;
                }
                /* Get a register for the loop counter, re-evaluate lower */
                Register_t *loop_reg = NULL;
                inst_list = codegen_expr_tree_value(element->lower, inst_list, ctx, &loop_reg);
                if (codegen_had_error(ctx) || loop_reg == NULL)
                {
                    if (loop_reg != NULL)
                        free_reg(get_reg_stack(), loop_reg);
                    free_reg(get_reg_stack(), upper_reg);
                    free_reg(get_reg_stack(), addr_reg);
                    if (out_reg != NULL)
                        *out_reg = NULL;
                    return inst_list;
                }
                /* Start from lower+1 (lower already set above) */
                snprintf(buffer, sizeof(buffer), "\tincl\t%s\n", loop_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                /* Loop: while loop_reg <= upper_reg, btsl and increment */
                int range_label = ++ctx->label_counter;
                int end_label = ++ctx->label_counter;
                snprintf(buffer, sizeof(buffer), ".L%d:\n", range_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tcmpl\t%s, %s\n", upper_reg->bit_32, loop_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjg\t.L%d\n", end_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tbtsl\t%s, (%s)\n", loop_reg->bit_32, addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tincl\t%s\n", loop_reg->bit_32);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjmp\t.L%d\n", range_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), ".L%d:\n", end_label);
                inst_list = add_inst(inst_list, buffer);
                free_reg(get_reg_stack(), loop_reg);
                free_reg(get_reg_stack(), upper_reg);
            }
        }
        
        /* Return the address register */
        if (out_reg != NULL)
            *out_reg = addr_reg;
        else
            free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }

    /* Regular 32-bit sets */
    if (expr->expr_data.set_data.is_constant)
    {
        Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
        if (dest_reg == NULL)
        {
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        char buffer[64];
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%u, %s\n", expr->expr_data.set_data.bitmask,
            dest_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        if (out_reg != NULL)
            *out_reg = dest_reg;
        else
            free_reg(get_reg_stack(), dest_reg);
        return inst_list;
    }

    Register_t *dest_reg = codegen_try_get_reg(&inst_list, ctx, "set literal");
    if (dest_reg == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, %s\n", dest_reg->bit_32);
    inst_list = add_inst(inst_list, buffer);

    for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next)
    {
        struct SetElement *element = (struct SetElement *)cur->cur;
        if (element == NULL)
            continue;

        Register_t *lower_reg = NULL;
        inst_list = codegen_expr_tree_value(element->lower, inst_list, ctx, &lower_reg);
        if (codegen_had_error(ctx) || lower_reg == NULL)
        {
            if (lower_reg != NULL)
                free_reg(get_reg_stack(), lower_reg);
            free_reg(get_reg_stack(), dest_reg);
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        Register_t *upper_reg = NULL;
        if (element->upper != NULL)
        {
            inst_list = codegen_expr_tree_value(element->upper, inst_list, ctx, &upper_reg);
            if (codegen_had_error(ctx) || upper_reg == NULL)
            {
                if (upper_reg != NULL)
                    free_reg(get_reg_stack(), upper_reg);
                free_reg(get_reg_stack(), lower_reg);
                free_reg(get_reg_stack(), dest_reg);
                if (out_reg != NULL)
                    *out_reg = NULL;
                return inst_list;
            }
        }

        if (element->upper == NULL)
            inst_list = codegen_set_emit_single(inst_list, ctx, dest_reg, lower_reg);
        else
            inst_list = codegen_set_emit_range(inst_list, ctx, dest_reg, lower_reg, upper_reg);

        if (codegen_had_error(ctx))
        {
            if (upper_reg != NULL)
                free_reg(get_reg_stack(), upper_reg);
            free_reg(get_reg_stack(), lower_reg);
            free_reg(get_reg_stack(), dest_reg);
            if (out_reg != NULL)
                *out_reg = NULL;
            return inst_list;
        }

        if (upper_reg != NULL)
            free_reg(get_reg_stack(), upper_reg);
        free_reg(get_reg_stack(), lower_reg);
    }

    if (out_reg != NULL)
        *out_reg = dest_reg;
    else
        free_reg(get_reg_stack(), dest_reg);
    return inst_list;
}

static ListNode_t *codegen_set_expr(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    if (expr->type == EXPR_SET)
        return codegen_set_literal(expr, inst_list, ctx, out_reg, 0);

    return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
}

static Register_t *codegen_clone_register_if_rcx(ListNode_t **inst_list, CodeGenContext *ctx,
    Register_t *reg, const char *label)
{
    if (reg == NULL || inst_list == NULL)
        return reg;

    if (reg->reg_id != REG_RCX)
        return reg;

    static const RegisterId_t preferred_regs[] = { REG_RAX, REG_R10, REG_R11, REG_R8, REG_R9 };
    Register_t *replacement = NULL;
    for (size_t i = 0; i < sizeof(preferred_regs) / sizeof(preferred_regs[0]); ++i)
    {
        if (get_register_by_id(get_reg_stack(), preferred_regs[i], &replacement) == 0 && replacement != NULL)
            break;
        replacement = NULL;
    }

    if (replacement == NULL)
        replacement = codegen_try_get_reg(inst_list, ctx, label);

    if (replacement == NULL)
        return reg;

    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", reg->bit_64, replacement->bit_64);
    *inst_list = add_inst(*inst_list, buffer);
    free_reg(get_reg_stack(), reg);
    return replacement;
}

ListNode_t *codegen_char_set_address(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || out_reg == NULL)
        return inst_list;

    Register_t *addr_reg = NULL;
    if (codegen_expr_is_addressable(expr))
    {
        inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    }
    else if ((expr->type == EXPR_ADDOP || expr->type == EXPR_MULOP) &&
             expr_has_type_tag(expr, SET_TYPE))
    {
        /* Set binary operation on char sets: the standard expr tree path only
           handles 4-byte sets.  Recursively materialise 32-byte operand
           addresses and call the appropriate runtime helper. */
        struct Expression *left_op = NULL;
        struct Expression *right_op = NULL;
        const char *runtime_func = NULL;

        if (expr->type == EXPR_ADDOP)
        {
            int op = expr->expr_data.addop_data.addop_type;
            left_op  = expr->expr_data.addop_data.left_expr;
            right_op = expr->expr_data.addop_data.right_term;
            if (op == PLUS)       runtime_func = "kgpc_set_union_256";
            else if (op == MINUS) runtime_func = "kgpc_set_diff_256";
        }
        else /* EXPR_MULOP */
        {
            int op = expr->expr_data.mulop_data.mulop_type;
            left_op  = expr->expr_data.mulop_data.left_term;
            right_op = expr->expr_data.mulop_data.right_factor;
            if (op == STAR)       runtime_func = "kgpc_set_intersect_256";
            else if (op == XOR)   runtime_func = "kgpc_set_symdiff_256";
        }

        if (runtime_func != NULL && left_op != NULL && right_op != NULL)
        {
            /* Allocate 32-byte temp on stack for the result */
            StackNode_t *temp = codegen_alloc_temp_bytes("cset_binop", 32);
            if (temp == NULL)
            {
                *out_reg = NULL;
                return inst_list;
            }

            /* Get addresses of both operands (recursive for nested ops) */
            Register_t *left_reg = NULL;
            inst_list = codegen_char_set_address(left_op, inst_list, ctx, &left_reg);
            if (codegen_had_error(ctx) || left_reg == NULL)
            {
                if (left_reg != NULL) free_reg(get_reg_stack(), left_reg);
                *out_reg = NULL;
                return inst_list;
            }

            Register_t *right_reg = NULL;
            inst_list = codegen_char_set_address(right_op, inst_list, ctx, &right_reg);
            if (codegen_had_error(ctx) || right_reg == NULL)
            {
                if (right_reg != NULL) free_reg(get_reg_stack(), right_reg);
                free_reg(get_reg_stack(), left_reg);
                *out_reg = NULL;
                return inst_list;
            }

            /* Spill left/right to stack since the call clobbers argument regs */
            StackNode_t *left_spill  = codegen_alloc_temp_bytes("cset_lspill", 8);
            StackNode_t *right_spill = codegen_alloc_temp_bytes("cset_rspill", 8);
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                left_reg->bit_64, left_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                right_reg->bit_64, right_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), left_reg);
            free_reg(get_reg_stack(), right_reg);

            /* Set up call: runtime_func(dest, left, right) */
            addr_reg = get_free_reg(get_reg_stack(), &inst_list);
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                temp->offset, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", left_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r8\n", right_spill->offset);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rsi\n", left_spill->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rdx\n", right_spill->offset);
                inst_list = add_inst(inst_list, buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, runtime_func);
            free_arg_regs();

            /* addr_reg now points to the temp buffer with the 32-byte result.
               Reload it in case the call clobbered it. */
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                temp->offset, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            /* Unknown set op — fall back to value-based codegen */
            inst_list = codegen_set_expr(expr, inst_list, ctx, &addr_reg);
        }
    }
    else if (expr->type == EXPR_SET)
    {
        /* Force char set mode for set literals in a char set context */
        inst_list = codegen_set_literal(expr, inst_list, ctx, &addr_reg, 1);
    }
    else
    {
        inst_list = codegen_set_expr(expr, inst_list, ctx, &addr_reg);
    }

    if (codegen_had_error(ctx) || addr_reg == NULL)
    {
        *out_reg = NULL;
        return inst_list;
    }

    addr_reg = codegen_clone_register_if_rcx(&inst_list, ctx, addr_reg, "char set addr spill");
    *out_reg = addr_reg;
    return inst_list;
}

ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(expr != NULL);
    assert(ctx != NULL);
    CODEGEN_DEBUG("DEBUG: Generating code for expression type %d\n", expr->type);

    if (expr_has_type_tag(expr, SET_TYPE))
    {
        Register_t *set_reg = NULL;
        inst_list = codegen_set_expr(expr, inst_list, ctx, &set_reg);
        if (codegen_had_error(ctx))
            return inst_list;
        if (set_reg != NULL)
            free_reg(get_reg_stack(), set_reg);
        return inst_list;
    }

    switch(expr->type) {
        case EXPR_VAR_ID:
            CODEGEN_DEBUG("DEBUG: Processing variable ID expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RECORD_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing record access expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ARRAY_ACCESS:
            CODEGEN_DEBUG("DEBUG: Processing array access expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_MULOP:
            CODEGEN_DEBUG("DEBUG: Processing mulop expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_INUM:
            CODEGEN_DEBUG("DEBUG: Processing integer constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RNUM:
            CODEGEN_DEBUG("DEBUG: Processing real constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_BOOL:
            CODEGEN_DEBUG("DEBUG: Processing boolean constant expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_NIL:
            CODEGEN_DEBUG("DEBUG: Processing nil literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SET:
            CODEGEN_DEBUG("DEBUG: Processing set literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_STRING:
            CODEGEN_DEBUG("DEBUG: Processing string literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_CHAR_CODE:
            CODEGEN_DEBUG("DEBUG: Processing character code literal expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_POINTER_DEREF:
            CODEGEN_DEBUG("DEBUG: Processing pointer dereference expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_RECORD_CONSTRUCTOR:
        {
            Register_t *addr_reg = NULL;
            inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
            if (addr_reg != NULL)
                free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
        case EXPR_ADDR:
            CODEGEN_DEBUG("DEBUG: Processing address-of expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ADDR_OF_PROC:
            CODEGEN_DEBUG("DEBUG: Processing address-of-procedure expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_TYPEINFO:
            CODEGEN_DEBUG("DEBUG: Processing typeinfo expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_ARRAY_LITERAL:
            CODEGEN_DEBUG("DEBUG: Processing array literal expression\n");
            {
                Register_t *tmp_reg = NULL;
                inst_list = codegen_materialize_array_literal(expr, inst_list, ctx, &tmp_reg);
                if (tmp_reg != NULL)
                    free_reg(get_reg_stack(), tmp_reg);
            }
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
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_SIGN_TERM:
            CODEGEN_DEBUG("DEBUG: Processing sign term expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_FUNCTION_CALL:
            CODEGEN_DEBUG("DEBUG: Processing function call expression\n");
            inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_TYPECAST:
            CODEGEN_DEBUG("DEBUG: Processing typecast expression\n");
            if (expr->expr_data.typecast_data.expr != NULL)
                inst_list = codegen_expr(expr->expr_data.typecast_data.expr, inst_list, ctx);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_IS:
            CODEGEN_DEBUG("DEBUG: Processing RTTI is expression\n");
            inst_list = codegen_emit_is_expr(expr, inst_list, ctx, NULL);
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        case EXPR_AS:
            CODEGEN_DEBUG("DEBUG: Processing RTTI as expression\n");
            if (expr->expr_data.as_data.expr != NULL)
            {
                if (codegen_expr_is_addressable(expr->expr_data.as_data.expr))
                {
                    Register_t *addr_reg = NULL;
                    inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list, ctx, &addr_reg);
                    if (addr_reg != NULL)
                    {
                        inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx, addr_reg);
                        free_reg(get_reg_stack(), addr_reg);
                    }
                }
                else if (codegen_expr_needs_class_method_vmt_self(expr->expr_data.as_data.expr, ctx))
                {
                    Register_t *value_reg = NULL;
                    inst_list = codegen_expr_with_result(expr->expr_data.as_data.expr, inst_list, ctx, &value_reg);
                    if (value_reg != NULL)
                    {
                        inst_list = codegen_emit_class_cast_check_from_instance_ptr(expr, inst_list, ctx, value_reg);
                        free_reg(get_reg_stack(), value_reg);
                    }
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: RTTI operations currently require addressable class expressions.");
                }
            }
            #ifdef DEBUG_CODEGEN
            CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
            #endif
            return inst_list;
        default:
            assert(0 && "Unsupported expression type");
            break;
    }
}

static int codegen_expr_is_byref_var_id(const struct Expression *expr, CodeGenContext *ctx)
{
    if (expr == NULL || ctx == NULL || ctx->symtab == NULL ||
        expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
        return 0;

    HashNode_t *symbol = NULL;
    if (FindSymbol(&symbol, ctx->symtab, expr->expr_data.id) == 0 || symbol == NULL)
        return 0;

    return symbol->is_var_parameter;
}

static const struct Expression *codegen_unwrap_typecast_chain(
    const struct Expression *expr, int *saw_extended_cast)
{
    const struct Expression *cur = expr;
    if (saw_extended_cast != NULL)
        *saw_extended_cast = 0;

    while (cur != NULL && cur->type == EXPR_TYPECAST &&
           cur->expr_data.typecast_data.expr != NULL)
    {
        if (saw_extended_cast != NULL &&
            cur->expr_data.typecast_data.target_type == EXTENDED_TYPE)
        {
            *saw_extended_cast = 1;
        }
        cur = cur->expr_data.typecast_data.expr;
    }

    return cur;
}

ListNode_t *codegen_expr_with_result(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(out_reg != NULL);
    
    /* Handle set expressions specially - they need codegen_set_expr for proper bitmask generation */
    if (expr != NULL && expr_has_type_tag(expr, SET_TYPE))
    {
        inst_list = codegen_set_expr(expr, inst_list, ctx, out_reg);
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: LEAVING %s (SET_TYPE path)\n", __func__);
        #endif
        return inst_list;
    }

    if (expr != NULL && expr->type == EXPR_TYPECAST &&
        expr->expr_data.typecast_data.expr != NULL)
    {
        int target_tag = expr->expr_data.typecast_data.target_type;
        int saw_extended_cast = 0;
        const struct Expression *source_expr =
            codegen_unwrap_typecast_chain(expr->expr_data.typecast_data.expr,
                                          &saw_extended_cast);

        if (!codegen_expr_is_byref_var_id(source_expr, ctx))
            goto skip_byref_typecast_fastpath;

        if (target_tag == REAL_TYPE || target_tag == EXTENDED_TYPE)
        {
            Register_t *addr_reg = NULL;
            Register_t *result_reg = NULL;
            char buffer[128];

            inst_list = codegen_address_for_expr(
                (struct Expression *)source_expr, inst_list, ctx, &addr_reg);
            if (codegen_had_error(ctx) || addr_reg == NULL)
                return inst_list;

            result_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (result_reg == NULL)
                result_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (result_reg == NULL)
            {
                free_reg(get_reg_stack(), addr_reg);
                return inst_list;
            }

            if (target_tag == EXTENDED_TYPE || saw_extended_cast)
            {
                if (codegen_target_is_windows())
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
                else
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                inst_list = codegen_vect_reg(inst_list, 0);
                inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
                free_arg_regs();
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", result_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                long long real_size = expr_effective_size_bytes(
                    (struct Expression *)source_expr);
                if (real_size <= 0)
                    real_size = expr_effective_size_bytes(expr);
                if (real_size <= 4)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovss\t(%s), %%xmm0\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tmovsd\t(%s), %%xmm0\n", addr_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t%%xmm0, %s\n", result_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            free_reg(get_reg_stack(), addr_reg);
            *out_reg = result_reg;
            return inst_list;
        }
    }
skip_byref_typecast_fastpath:

    inst_list = codegen_expr_tree_value(expr, inst_list, ctx, out_reg);

    
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

static int codegen_resolve_is_array(struct Expression *array_expr, CodeGenContext *ctx,
    StackNode_t **out_stack_node)
{
    if (out_stack_node != NULL)
        *out_stack_node = NULL;
    if (array_expr == NULL || ctx == NULL)
        return 0;

    KgpcType *base_type = expr_get_kgpc_type(array_expr);
    int base_is_array = (array_expr->is_array_expr ||
        (base_type != NULL && (kgpc_type_is_array(base_type) || kgpc_type_is_shortstring(base_type))));
    if (!base_is_array && ctx->symtab != NULL && array_expr->type == EXPR_VAR_ID)
    {
        HashNode_t *array_node = NULL;
        if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
            array_node != NULL && hashnode_is_array(array_node))
        {
            base_is_array = 1;
        }
    }
    if (array_expr->type == EXPR_VAR_ID)
    {
        StackNode_t *stack_node = find_label(array_expr->expr_data.id);
        if (out_stack_node != NULL)
            *out_stack_node = stack_node;
        if (!base_is_array && stack_node != NULL && stack_node->is_array)
            base_is_array = 1;
    }

    /* With-stack lookup: when semcheck was skipped (e.g. imported bodies),
     * variables inside `with Record do` blocks remain as unresolved EXPR_VAR_ID.
     * Check if the variable name matches a field in any enclosing with-context record. */
    if (!base_is_array && array_expr->type == EXPR_VAR_ID &&
        array_expr->expr_data.id != NULL && ctx->with_depth > 0)
    {
        struct RecordField *with_field = codegen_lookup_with_field(ctx,
            array_expr->expr_data.id, NULL);
        if (with_field != NULL && with_field->is_array)
            base_is_array = 1;
    }

    /* Implicit Self field lookup: in unchecked class method bodies, field references
     * like `Args[i]` remain as EXPR_VAR_ID instead of being resolved to Self.Args.
     * Check if the variable name is a field of the current method's owning class. */
    if (!base_is_array && array_expr->type == EXPR_VAR_ID &&
        array_expr->expr_data.id != NULL && ctx->symtab != NULL &&
        ctx->current_subprogram_owner_class != NULL)
    {
        HashNode_t *class_node = NULL;
        if (FindSymbol(&class_node, ctx->symtab, ctx->current_subprogram_owner_class) != 0 &&
            class_node != NULL && class_node->type != NULL &&
            kgpc_type_is_record(class_node->type))
        {
            struct RecordType *class_record = kgpc_type_get_record(class_node->type);
            if (class_record != NULL)
            {
                struct RecordField *field = codegen_lookup_record_field(class_record,
                    array_expr->expr_data.id);
                if (field != NULL && field->is_array)
                    base_is_array = 1;
            }
        }
    }

    /* Typecast expressions: resolve via the typecast target type.
     * e.g. TSomeArrayType(expr)[i] where TSomeArrayType is an array typedef. */
    if (!base_is_array && array_expr->type == EXPR_TYPECAST && ctx->symtab != NULL)
    {
        const char *target_id = array_expr->expr_data.typecast_data.target_type_id;
        if (target_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, target_id) != 0 &&
                type_node != NULL && type_node->type != NULL &&
                kgpc_type_is_array(type_node->type))
            {
                base_is_array = 1;
            }
        }
    }

    return base_is_array;
}

static int codegen_get_indexable_element_size(struct Expression *array_expr,
    CodeGenContext *ctx, long long *out_size)
{
    assert(array_expr != NULL);
    assert(out_size != NULL);

    StackNode_t *array_stack_node = NULL;
    int base_is_array = codegen_resolve_is_array(array_expr, ctx, &array_stack_node);
    int base_is_string = (is_string_type(expr_get_type_tag(array_expr)) && !base_is_array);
    int base_is_pointer = (expr_has_type_tag(array_expr, POINTER_TYPE) && !base_is_array);
    struct RecordField *record_field = NULL;
    KgpcType *record_field_type = NULL;

    if (array_expr->type == EXPR_RECORD_ACCESS)
    {
        record_field = codegen_lookup_record_field_expr(array_expr, ctx);
        if (record_field != NULL)
        {
            if (record_field->is_array)
                base_is_array = 1;
            if (!base_is_pointer && record_field->is_pointer)
                base_is_pointer = 1;
            if (!base_is_string && is_string_type(record_field->type))
                base_is_string = 1;
            if (ctx != NULL && ctx->symtab != NULL && record_field->type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, ctx->symtab, record_field->type_id) != 0 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    record_field_type = type_node->type;
                    if (!base_is_array && kgpc_type_is_array(record_field_type))
                        base_is_array = 1;
                    if (!base_is_string && kgpc_type_is_string(record_field_type))
                        base_is_string = 1;
                    if (!base_is_pointer && kgpc_type_is_pointer(record_field_type))
                        base_is_pointer = 1;
                }
            }
        }
    }
    if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL)
    {
        KgpcType *resolved = array_expr->resolved_kgpc_type;
        if (!base_is_array && kgpc_type_is_array(resolved))
            base_is_array = 1;
        if (!base_is_string && kgpc_type_is_string(resolved))
            base_is_string = 1;
        if (!base_is_pointer && kgpc_type_is_pointer(resolved))
            base_is_pointer = 1;
    }
    /* Fallback: check stack node and symbol table for unresolved types */
    if (!base_is_array && !base_is_string && !base_is_pointer)
    {
        if (array_stack_node != NULL && (array_stack_node->is_array || array_stack_node->is_dynamic))
            base_is_array = 1;
        else if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
                 ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *var_node = NULL;
            if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                var_node != NULL && var_node->type != NULL)
            {
                if (kgpc_type_is_string(var_node->type))
                    base_is_string = 1;
                else if (kgpc_type_is_array(var_node->type))
                    base_is_array = 1;
                else if (kgpc_type_is_pointer(var_node->type))
                    base_is_pointer = 1;
            }
        }
    }
    if (!base_is_array && !base_is_string && !base_is_pointer)
    {
        if (array_expr->type == EXPR_POINTER_DEREF)
        {
            /* p^[i] where p points to an array - treat as array access */
            base_is_array = 1;
        }
        else if (array_expr->type == EXPR_RECORD_ACCESS)
        {
            /* Record field indexing (e.g., rec.field[i]) - trust semcheck info if present */
            if (array_expr->is_array_expr ||
                array_expr->array_element_type != UNKNOWN_TYPE ||
                array_expr->array_element_type_id != NULL ||
                array_expr->array_element_record_type != NULL)
            {
                base_is_array = 1;
            }
        }
    }
    long long element_size_ll = 1;

    if (base_is_string)
    {
        long long string_elem_size = 1;
        if (record_field_type != NULL && kgpc_type_is_wide_string(record_field_type))
        {
            string_elem_size = 2;
        }
        else
        {
            KgpcType *base_type = expr_get_kgpc_type(array_expr);
            if (base_type != NULL && kgpc_type_is_wide_string(base_type))
                string_elem_size = 2;
            else if (ctx != NULL && ctx->symtab != NULL && array_expr->type == EXPR_VAR_ID &&
                     array_expr->expr_data.id != NULL)
            {
                HashNode_t *var_node = NULL;
                if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                    var_node != NULL && var_node->type != NULL &&
                    kgpc_type_is_wide_string(var_node->type))
                {
                    string_elem_size = 2;
                }
            }
        }
        *out_size = string_elem_size;
        return 1;
    }

    if (base_is_array)
    {
        KgpcType *base_type = expr_get_kgpc_type(array_expr);
        if (base_type == NULL && array_expr->type == EXPR_VAR_ID &&
            ctx != NULL && ctx->symtab != NULL && array_expr->expr_data.id != NULL)
        {
            HashNode_t *var_node = NULL;
            if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                var_node != NULL)
            {
                base_type = var_node->type;
            }
        }
        if (base_type != NULL && kgpc_type_is_array(base_type) &&
            !codegen_expr_has_widechar_array_metadata(array_expr))
        {
            long long direct_elem_size = kgpc_type_get_array_element_size(base_type);
            if (direct_elem_size > 0)
            {
                *out_size = direct_elem_size;
                return 1;
            }
        }
        if (array_expr->array_element_size > 0)
        {
            *out_size = array_expr->array_element_size;
            return 1;
        }
    }

    if (base_is_pointer)
    {
        /* Plain Pointer arithmetic in FPC is byte-wise. Only attempt pointee-size
           scaling when the pointer still carries concrete target metadata. */
        if (array_expr->pointer_subtype == UNKNOWN_TYPE &&
            array_expr->pointer_subtype_id == NULL &&
            codegen_expr_record_type(array_expr, ctx != NULL ? ctx->symtab : NULL) == NULL)
        {
            element_size_ll = 1;
        }
        else if (codegen_sizeof_type(ctx, array_expr->pointer_subtype,
                     array_expr->pointer_subtype_id,
                     codegen_expr_record_type(array_expr, ctx != NULL ? ctx->symtab : NULL),
                     &element_size_ll, 0) != 0 || element_size_ll <= 0)
        {
            /* Typed-pointer metadata was present but incomplete. Fall back to
               byte-wise arithmetic instead of surfacing a codegen-only error. */
            element_size_ll = 1;
        }
        *out_size = element_size_ll;
        return 1;
    }

    if (base_is_array && record_field != NULL && record_field->is_array)
    {
        if (record_field->array_element_kgpc_type != NULL)
        {
            element_size_ll = kgpc_type_sizeof(record_field->array_element_kgpc_type);
        }
        else
        {
            struct RecordType *elem_record = record_field->array_element_record;
            if (codegen_sizeof_type(ctx, record_field->array_element_type,
                    record_field->array_element_type_id, elem_record,
                    &element_size_ll, 0) != 0)
            {
                element_size_ll = -1;
            }
        }

        if (element_size_ll > 0)
        {
            *out_size = element_size_ll;
            return 1;
        }
    }

    if (base_is_array && record_field_type != NULL && kgpc_type_is_array(record_field_type))
    {
        long long field_elem = kgpc_type_get_array_element_size(record_field_type);
        if (field_elem <= 0)
        {
            KgpcType *elem = kgpc_type_get_array_element_type_resolved(record_field_type,
                ctx != NULL ? ctx->symtab : NULL);
            if (elem != NULL)
                field_elem = kgpc_type_sizeof(elem);
        }
        if (field_elem > 0)
        {
            *out_size = field_elem;
            return 1;
        }
    }

    element_size_ll = expr_get_array_element_size(array_expr, ctx);
    if (element_size_ll > 0 && ctx != NULL && ctx->symtab != NULL)
    {
        if (array_expr->array_element_type_id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, array_expr->array_element_type_id) != 0 &&
                type_node != NULL && type_node->type != NULL)
            {
                long long node_size = kgpc_type_sizeof(type_node->type);
                if (node_size > element_size_ll)
                    element_size_ll = node_size;
            }
        }
        if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL)
        {
            HashNode_t *array_node = NULL;
            if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                array_node != NULL && array_node->type != NULL &&
                kgpc_type_is_array(array_node->type))
            {
                long long node_elem = kgpc_type_get_array_element_size(array_node->type);
                if (node_elem > element_size_ll)
                    element_size_ll = node_elem;
            }
        }
    }
    if (element_size_ll <= 0 && ctx != NULL && ctx->symtab != NULL &&
        array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL)
    {
        HashNode_t *array_node = NULL;
        if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
            array_node != NULL && array_node->type != NULL &&
            kgpc_type_is_array(array_node->type))
        {
            element_size_ll = kgpc_type_get_array_element_size(array_node->type);
            if (element_size_ll <= 0 && array_node->type->info.array_info.element_type != NULL)
                element_size_ll = kgpc_type_sizeof(array_node->type->info.array_info.element_type);
            if (element_size_ll > 0)
            {
                *out_size = element_size_ll;
                return 1;
            }
        }
    }
    if (element_size_ll <= 0 && array_stack_node != NULL &&
        array_stack_node->is_array && array_stack_node->element_size > 0)
    {
        *out_size = array_stack_node->element_size;
        return 1;
    }
    if (element_size_ll <= 0 && codegen_expr_is_shortstring_array_local(array_expr))
    {
        *out_size = 1;
        return 1;
    }

    int need_element_size = 0;
    if (element_size_ll <= 0)
        need_element_size = 1;
    else if (array_expr->array_element_record_type != NULL)
        need_element_size = 1;
    else if (array_expr->array_element_type == RECORD_TYPE)
        need_element_size = 1;
    else if (array_expr->array_element_type == UNKNOWN_TYPE &&
        array_expr->array_element_type_id != NULL)
        need_element_size = 1;

    if (need_element_size)
    {
        if (codegen_sizeof_type(ctx, array_expr->array_element_type,
                array_expr->array_element_type_id,
                array_expr->array_element_record_type,
                &element_size_ll, 0) != 0 || element_size_ll <= 0)
        {
            if (kgpc_getenv("KGPC_DEBUG_ARRAY_ACCESS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_ARRAY_ACCESS] elem size fail: expr_type=%d tag=%d array_elem_type=%d elem_id=%s is_array=%d dyn=%d\n",
                    array_expr->type,
                    expr_get_type_tag(array_expr),
                    array_expr->array_element_type,
                    array_expr->array_element_type_id ? array_expr->array_element_type_id : "<null>",
                    array_expr->is_array_expr,
                    array_expr->array_is_dynamic);
                fprintf(stderr,
                    "[KGPC_DEBUG_ARRAY_ACCESS] base_is_array=%d stack_is_array=%d stack_elem_size=%d\n",
                    base_is_array,
                    array_stack_node != NULL ? array_stack_node->is_array : -1,
                    array_stack_node != NULL ? array_stack_node->element_size : -1);
                if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL)
                    fprintf(stderr,
                        "[KGPC_DEBUG_ARRAY_ACCESS] base id: %s\n",
                        array_expr->expr_data.id);
                if (array_expr->resolved_kgpc_type != NULL)
                    fprintf(stderr,
                        "[KGPC_DEBUG_ARRAY_ACCESS] kgpc type: %s\n",
                        kgpc_type_to_string(array_expr->resolved_kgpc_type));
            }
            codegen_report_error(ctx, "ERROR: Unable to determine element size for array access.");
            return 0;
        }
    }

    *out_size = element_size_ll;
    return 1;
}

static int codegen_collect_nested_array_access_chain(struct Expression *expr,
    struct Expression **base_expr_out, struct Expression **indices_out, int *index_count_out)
{
    struct Expression *reversed_indices[16];
    int reversed_count = 0;
    struct Expression *current = expr;

    if (base_expr_out != NULL)
        *base_expr_out = NULL;
    if (index_count_out != NULL)
        *index_count_out = 0;
    if (expr == NULL || base_expr_out == NULL || indices_out == NULL || index_count_out == NULL)
        return 0;

    while (current != NULL && current->type == EXPR_ARRAY_ACCESS)
    {
        if (current->expr_data.array_access_data.extra_indices != NULL)
            return 0;
        if (reversed_count >= (int)(sizeof(reversed_indices) / sizeof(reversed_indices[0])))
            return 0;
        reversed_indices[reversed_count++] = current->expr_data.array_access_data.index_expr;
        current = current->expr_data.array_access_data.array_expr;
    }

    if (current == NULL || reversed_count <= 1)
        return 0;

    *base_expr_out = current;
    for (int i = 0; i < reversed_count; ++i)
        indices_out[i] = reversed_indices[reversed_count - 1 - i];
    *index_count_out = reversed_count;
    return 1;
}

static ListNode_t *codegen_emit_linearized_array_address(struct Expression *base_expr,
    struct Expression **indices, int index_count, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    KgpcType *array_type = NULL;
    KgpcType *current_type = NULL;
    KgpcArrayDimensionInfo info;
    Register_t *addr_reg = NULL;
    char buffer[128];

    if (out_reg != NULL)
        *out_reg = NULL;
    if (base_expr == NULL || indices == NULL || index_count <= 1 || ctx == NULL || out_reg == NULL)
        return inst_list;

    array_type = base_expr->resolved_kgpc_type;
    if (base_expr->type == EXPR_VAR_ID && ctx->symtab != NULL && base_expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
            node != NULL && node->type != NULL)
            array_type = node->type;
    }

    if (array_type == NULL || !kgpc_type_is_array(array_type) ||
        kgpc_type_get_array_dimension_info(array_type, ctx->symtab, &info) != 0 ||
        info.dim_count < index_count)
        return inst_list;

    current_type = array_type;
    for (int i = 0; i < index_count - 1; ++i)
    {
        KgpcType *next_type = kgpc_type_get_array_element_type_resolved(current_type, ctx->symtab);
        if (next_type == NULL || !kgpc_type_is_array(next_type) ||
            kgpc_type_is_string(next_type) || kgpc_type_is_shortstring(next_type))
            return inst_list;
        current_type = next_type;
    }

    inst_list = codegen_address_for_expr(base_expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    for (int i = 0; i < index_count; ++i)
    {
        struct Expression *index_expr = indices[i];
        Register_t *index_reg = NULL;
        long long lower_bound = info.dim_lowers[i];
        long long stride = info.strides[i];

        inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
        if (codegen_had_error(ctx) || index_reg == NULL)
        {
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }

        if (lower_bound > 0)
        {
            if (expression_uses_qword(index_expr))
                snprintf(buffer, sizeof(buffer), "\tsubq\t$%lld, %s\n", lower_bound, index_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tsubl\t$%lld, %s\n", lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (lower_bound < 0)
        {
            if (expression_uses_qword(index_expr))
                snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", -lower_bound, index_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t$%lld, %s\n", -lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }

        if (!expression_uses_qword(index_expr))
            inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);

        if (stride == 1 || stride == 2 || stride == 4 || stride == 8)
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n",
                addr_reg->bit_64, index_reg->bit_64, (int)stride, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%lld, %s\n", stride, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", index_reg->bit_64, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        free_reg(get_reg_stack(), index_reg);
    }

    *out_reg = addr_reg;
    return inst_list;
}


int expr_contains_function_call(const struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_FUNCTION_CALL:
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
            return 1;
        case EXPR_TYPECAST:
            return expr_contains_function_call(expr->expr_data.typecast_data.expr);
        case EXPR_AS:
            return expr_contains_function_call(expr->expr_data.as_data.expr);
        case EXPR_SIGN_TERM:
            return expr_contains_function_call(expr->expr_data.sign_term);
        case EXPR_RECORD_ACCESS:
            return expr_contains_function_call(expr->expr_data.record_access_data.record_expr);
        case EXPR_ARRAY_ACCESS:
            return expr_contains_function_call(expr->expr_data.array_access_data.array_expr) ||
                expr_contains_function_call(expr->expr_data.array_access_data.index_expr);
        case EXPR_POINTER_DEREF:
            return expr_contains_function_call(expr->expr_data.pointer_deref_data.pointer_expr);
        case EXPR_ADDR:
            return expr_contains_function_call(expr->expr_data.addr_data.expr);
        case EXPR_RELOP:
            return expr_contains_function_call(expr->expr_data.relop_data.left) ||
                expr_contains_function_call(expr->expr_data.relop_data.right);
        case EXPR_ADDOP:
            return expr_contains_function_call(expr->expr_data.addop_data.left_expr) ||
                expr_contains_function_call(expr->expr_data.addop_data.right_term);
        case EXPR_MULOP:
            return expr_contains_function_call(expr->expr_data.mulop_data.left_term) ||
                expr_contains_function_call(expr->expr_data.mulop_data.right_factor);
        case EXPR_RECORD_CONSTRUCTOR:
        {
            ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
            while (cur != NULL)
            {
                struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
                if (field != NULL && field->value != NULL &&
                    expr_contains_function_call(field->value))
                    return 1;
                cur = cur->next;
            }
            return 0;
        }
        case EXPR_ARRAY_LITERAL:
        {
            ListNode_t *cur = expr->expr_data.array_literal_data.elements;
            while (cur != NULL)
            {
                struct Expression *elem = (struct Expression *)cur->cur;
                if (expr_contains_function_call(elem))
                    return 1;
                cur = cur->next;
            }
            return 0;
        }
        default:
            return 0;
    }
}

ListNode_t *codegen_array_element_address(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_ARRAY_ACCESS);
    assert(ctx != NULL);
    assert(out_reg != NULL);

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;
    struct Expression *linear_base_expr = NULL;
    struct Expression *linear_indices[16];
    int linear_index_count = 0;

    if (array_expr == NULL)
    {
        codegen_report_error(ctx, "ERROR: Array access missing base expression.");
        return inst_list;
    }

    if (codegen_collect_nested_array_access_chain(expr, &linear_base_expr,
            linear_indices, &linear_index_count))
    {
        Register_t *linearized_reg = NULL;
        ListNode_t *linearized_list = codegen_emit_linearized_array_address(linear_base_expr,
            linear_indices, linear_index_count, inst_list, ctx, &linearized_reg);
        if (linearized_reg != NULL)
        {
            *out_reg = linearized_reg;
            return linearized_list;
        }
    }

    StackNode_t *array_stack_node = NULL;
    int base_is_array = codegen_resolve_is_array(array_expr, ctx, &array_stack_node);
    int base_is_string = (is_string_type(expr_get_type_tag(array_expr)) && !base_is_array);
    int base_is_pointer = (expr_has_type_tag(array_expr, POINTER_TYPE) && !base_is_array);
    struct RecordField *record_field = NULL;
    KgpcType *record_field_type = NULL;
    int record_field_lower_known = 0;
    long long record_field_lower = 0;

    if (array_expr->type == EXPR_RECORD_ACCESS)
    {
        record_field = codegen_lookup_record_field_expr(array_expr, ctx);
        if (record_field != NULL)
        {
            if (record_field->is_array)
            {
                base_is_array = 1;
                record_field_lower_known = 1;
                record_field_lower = record_field->array_start;
            }
            if (!base_is_pointer && record_field->is_pointer)
                base_is_pointer = 1;
            if (!base_is_string && is_string_type(record_field->type))
                base_is_string = 1;
            if (ctx != NULL && ctx->symtab != NULL && record_field->type_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, ctx->symtab, record_field->type_id) != 0 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    record_field_type = type_node->type;
                    if (!base_is_array && kgpc_type_is_array(record_field_type))
                        base_is_array = 1;
                    if (!base_is_string && kgpc_type_is_string(record_field_type))
                        base_is_string = 1;
                    if (!base_is_pointer && kgpc_type_is_pointer(record_field_type))
                        base_is_pointer = 1;
                    if (!record_field_lower_known && kgpc_type_is_array(record_field_type))
                    {
                        record_field_lower_known = 1;
                        record_field_lower = record_field_type->info.array_info.start_index;
                    }
                }
            }
        }
    }

    /* Fallback: if type tag is unknown, try looking up the variable's type from the
     * symbol table or stack. String parameters like RawByteString may not have their
     * type tag set but are still indexable. */
    if (!base_is_array && !base_is_string && !base_is_pointer)
    {
        if (array_stack_node != NULL && (array_stack_node->is_array || array_stack_node->is_dynamic))
            base_is_array = 1;
        else if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
                 ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *var_node = NULL;
            if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                var_node != NULL && var_node->type != NULL)
            {
                if (kgpc_type_is_string(var_node->type))
                    base_is_string = 1;
                else if (kgpc_type_is_array(var_node->type))
                    base_is_array = 1;
                else if (kgpc_type_is_pointer(var_node->type))
                    base_is_pointer = 1;
            }
        }
    }
    if (!base_is_array && !base_is_string && !base_is_pointer)
    {
        if (array_expr->type == EXPR_POINTER_DEREF)
        {
            /* p^[i] where p points to an array - treat as array access */
            base_is_array = 1;
        }
        else if (array_expr->type == EXPR_RECORD_ACCESS)
        {
            /* Record field indexing (e.g., rec.field[i]) - trust semcheck info if present */
            if (array_expr->is_array_expr ||
                array_expr->array_element_type != UNKNOWN_TYPE ||
                array_expr->array_element_type_id != NULL ||
                array_expr->array_element_record_type != NULL)
            {
                base_is_array = 1;
            }
        }
        else if (array_expr->type == EXPR_TYPECAST && ctx != NULL && ctx->symtab != NULL)
        {
            /* Typecast to an array type: TSomeArrayType(expr)[i] */
            const char *target_id = array_expr->expr_data.typecast_data.target_type_id;
            if (target_id != NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindSymbol(&type_node, ctx->symtab, target_id) != 0 &&
                    type_node != NULL && type_node->type != NULL)
                {
                    if (kgpc_type_is_array(type_node->type))
                        base_is_array = 1;
                    else if (kgpc_type_is_string(type_node->type))
                        base_is_string = 1;
                    else if (kgpc_type_is_pointer(type_node->type))
                        base_is_pointer = 1;
                }
            }
        }
    }

    /* With-stack lookup: variables inside `with Record do` blocks may be unresolved
     * EXPR_VAR_ID when semcheck was skipped for imported bodies. Check if the name
     * matches a field in any enclosing with-context record. */
    if (!base_is_array && !base_is_string && !base_is_pointer &&
        array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
        ctx != NULL && ctx->with_depth > 0)
    {
        struct RecordType *with_record = NULL;
        struct RecordField *with_field = codegen_lookup_with_field(ctx,
            array_expr->expr_data.id, &with_record);
        if (with_field != NULL)
        {
            if (with_field->is_array)
            {
                base_is_array = 1;
                if (!record_field_lower_known)
                {
                    record_field_lower_known = 1;
                    record_field_lower = with_field->array_start;
                }
                record_field = with_field;
            }
            else if (is_string_type(with_field->type))
                base_is_string = 1;
            else if (with_field->is_pointer)
                base_is_pointer = 1;
            else if (with_field->type_id != NULL && ctx->symtab != NULL)
            {
                HashNode_t *field_type_node = NULL;
                if (FindSymbol(&field_type_node, ctx->symtab, with_field->type_id) != 0 &&
                    field_type_node != NULL && field_type_node->type != NULL)
                {
                    if (kgpc_type_is_array(field_type_node->type))
                    {
                        base_is_array = 1;
                        record_field_type = field_type_node->type;
                        if (!record_field_lower_known)
                        {
                            record_field_lower_known = 1;
                            record_field_lower = field_type_node->type->info.array_info.start_index;
                        }
                    }
                    else if (kgpc_type_is_string(field_type_node->type))
                        base_is_string = 1;
                    else if (kgpc_type_is_pointer(field_type_node->type))
                        base_is_pointer = 1;
                }
            }
            if (with_field->is_array)
                record_field = with_field;
        }
    }

    /* Implicit Self field lookup: in unchecked class method bodies, field references
     * like `Args[i]` remain as EXPR_VAR_ID. Check the owning class type. */
    if (!base_is_array && !base_is_string && !base_is_pointer &&
        array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
        ctx != NULL && ctx->symtab != NULL &&
        ctx->current_subprogram_owner_class != NULL)
    {
        HashNode_t *class_node = NULL;
        if (FindSymbol(&class_node, ctx->symtab, ctx->current_subprogram_owner_class) != 0 &&
            class_node != NULL && class_node->type != NULL &&
            kgpc_type_is_record(class_node->type))
        {
            struct RecordType *class_record = kgpc_type_get_record(class_node->type);
            if (class_record != NULL)
            {
                struct RecordField *field = codegen_lookup_record_field(class_record,
                    array_expr->expr_data.id);
                if (field != NULL)
                {
                    if (field->is_array)
                    {
                        base_is_array = 1;
                        record_field = field;
                        if (!record_field_lower_known)
                        {
                            record_field_lower_known = 1;
                            record_field_lower = field->array_start;
                        }
                    }
                    else if (is_string_type(field->type))
                        base_is_string = 1;
                    else if (field->is_pointer)
                        base_is_pointer = 1;
                    else if (field->type_id != NULL)
                    {
                        HashNode_t *field_type_node = NULL;
                        if (FindSymbol(&field_type_node, ctx->symtab, field->type_id) != 0 &&
                            field_type_node != NULL && field_type_node->type != NULL)
                        {
                            if (kgpc_type_is_array(field_type_node->type))
                            {
                                base_is_array = 1;
                                record_field_type = field_type_node->type;
                                if (!record_field_lower_known)
                                {
                                    record_field_lower_known = 1;
                                    record_field_lower = field_type_node->type->info.array_info.start_index;
                                }
                            }
                            else if (kgpc_type_is_string(field_type_node->type))
                                base_is_string = 1;
                            else if (kgpc_type_is_pointer(field_type_node->type))
                                base_is_pointer = 1;
                        }
                    }
                }
            }
        }
    }

    /* EXPR_RECORD_ACCESS with unknown sub-record: try to resolve the field's record type
     * from the symbol table and look up the accessed field within it. */
    if (!base_is_array && !base_is_string && !base_is_pointer &&
        array_expr->type == EXPR_RECORD_ACCESS && record_field == NULL &&
        ctx != NULL && ctx->symtab != NULL)
    {
        const char *field_id = array_expr->expr_data.record_access_data.field_id;
        struct Expression *rec_expr = array_expr->expr_data.record_access_data.record_expr;
        if (field_id != NULL && rec_expr != NULL)
        {
            /* Try to get the record type from the sub-expression */
            struct RecordType *rec_type = NULL;
            if (rec_expr->record_type != NULL)
                rec_type = rec_expr->record_type;
            else if (rec_expr->type == EXPR_VAR_ID && rec_expr->expr_data.id != NULL)
            {
                /* Look up the variable in symtab to get its record type */
                HashNode_t *var_node = NULL;
                if (FindSymbol(&var_node, ctx->symtab, rec_expr->expr_data.id) != 0 &&
                    var_node != NULL && var_node->type != NULL)
                {
                    if (kgpc_type_is_record(var_node->type))
                        rec_type = kgpc_type_get_record(var_node->type);
                    else if (kgpc_type_is_pointer(var_node->type) &&
                             var_node->type->info.points_to != NULL &&
                             kgpc_type_is_record(var_node->type->info.points_to))
                        rec_type = kgpc_type_get_record(var_node->type->info.points_to);
                }
                /* Implicit Self: if var is "Self" and we're in a method, use the owning class */
                if (rec_type == NULL &&
                    pascal_identifier_equals(rec_expr->expr_data.id, "Self") &&
                    ctx->current_subprogram_owner_class != NULL)
                {
                    HashNode_t *class_node = NULL;
                    if (FindSymbol(&class_node, ctx->symtab, ctx->current_subprogram_owner_class) != 0 &&
                        class_node != NULL && class_node->type != NULL &&
                        kgpc_type_is_record(class_node->type))
                        rec_type = kgpc_type_get_record(class_node->type);
                }
                /* Also try with-stack for nested record access */
                if (rec_type == NULL && ctx->with_depth > 0)
                {
                    struct RecordField *parent_field = codegen_lookup_with_field(ctx,
                        rec_expr->expr_data.id, NULL);
                    if (parent_field != NULL && parent_field->nested_record != NULL)
                        rec_type = parent_field->nested_record;
                    else if (parent_field != NULL && parent_field->type_id != NULL)
                    {
                        HashNode_t *pt_node = NULL;
                        if (FindSymbol(&pt_node, ctx->symtab, parent_field->type_id) != 0 &&
                            pt_node != NULL && pt_node->type != NULL &&
                            kgpc_type_is_record(pt_node->type))
                            rec_type = kgpc_type_get_record(pt_node->type);
                    }
                }
                /* Implicit Self: if the variable name is a field of the current class,
                 * look up its type to resolve chained access like `some_field.sub_field[i]` */
                if (rec_type == NULL && ctx->current_subprogram_owner_class != NULL)
                {
                    HashNode_t *class_node = NULL;
                    if (FindSymbol(&class_node, ctx->symtab, ctx->current_subprogram_owner_class) != 0 &&
                        class_node != NULL && class_node->type != NULL &&
                        kgpc_type_is_record(class_node->type))
                    {
                        struct RecordType *class_record = kgpc_type_get_record(class_node->type);
                        if (class_record != NULL)
                        {
                            struct RecordField *parent_field = codegen_lookup_record_field(
                                class_record, rec_expr->expr_data.id);
                            if (parent_field != NULL && parent_field->nested_record != NULL)
                                rec_type = parent_field->nested_record;
                            else if (parent_field != NULL && parent_field->type_id != NULL)
                            {
                                HashNode_t *ft_node = NULL;
                                if (FindSymbol(&ft_node, ctx->symtab, parent_field->type_id) != 0 &&
                                    ft_node != NULL && ft_node->type != NULL &&
                                    kgpc_type_is_record(ft_node->type))
                                    rec_type = kgpc_type_get_record(ft_node->type);
                            }
                        }
                    }
                }
            }
            if (rec_type != NULL)
            {
                struct RecordField *resolved_field = codegen_lookup_record_field(rec_type, field_id);
                if (resolved_field != NULL)
                {
                    if (resolved_field->is_array)
                    {
                        base_is_array = 1;
                        record_field = resolved_field;
                        if (!record_field_lower_known)
                        {
                            record_field_lower_known = 1;
                            record_field_lower = resolved_field->array_start;
                        }
                    }
                    else if (is_string_type(resolved_field->type))
                        base_is_string = 1;
                    else if (resolved_field->is_pointer)
                        base_is_pointer = 1;
                }
            }
        }
    }

    if (!base_is_array && !base_is_string && !base_is_pointer)
    {
        if (kgpc_getenv("KGPC_DEBUG_ARRAY_ACCESS") != NULL)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_ARRAY_ACCESS] non-indexable base: expr_type=%d tag=%d base_is_array=%d base_is_string=%d base_is_pointer=%d\n",
                array_expr != NULL ? array_expr->type : -1,
                array_expr != NULL ? expr_get_type_tag(array_expr) : -1,
                base_is_array, base_is_string, base_is_pointer);
            if (ctx != NULL && ctx->current_subprogram_id != NULL)
                fprintf(stderr, "[KGPC_DEBUG_ARRAY_ACCESS] subprogram: %s\n", ctx->current_subprogram_id);
            if (array_expr != NULL)
            {
                if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL)
                    fprintf(stderr, "[KGPC_DEBUG_ARRAY_ACCESS] base id: %s\n", array_expr->expr_data.id);
                if (array_expr->resolved_kgpc_type != NULL)
                    fprintf(stderr, "[KGPC_DEBUG_ARRAY_ACCESS] kgpc type: %s\n",
                        kgpc_type_to_string(array_expr->resolved_kgpc_type));
                if (ctx != NULL && ctx->symtab != NULL &&
                    array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL)
                {
                    HashNode_t *dbg_node = NULL;
                    if (FindSymbol(&dbg_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                        dbg_node != NULL && dbg_node->type != NULL)
                    {
                        fprintf(stderr, "[KGPC_DEBUG_ARRAY_ACCESS] symtab type: %s\n",
                            kgpc_type_to_string(dbg_node->type));
                    }
                }
            }
        }
        codegen_report_error(ctx, "ERROR: Expression is not indexable as an array.");
        return inst_list;
    }

    Register_t *index_reg = NULL;
    inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL)
        return inst_list;

    StackNode_t *index_spill_slot = NULL;
    if (expr_contains_function_call(array_expr))
    {
        index_spill_slot = add_l_t("array_index_spill");
        if (index_spill_slot != NULL)
        {
            char spill_buf[128];
            snprintf(spill_buf, sizeof(spill_buf), "\tmovl\t%s, -%d(%%rbp)\n",
                index_reg->bit_32, index_spill_slot->offset);
            inst_list = add_inst(inst_list, spill_buf);
        }
    }

    Register_t *base_reg = NULL;
    if (base_is_string || base_is_pointer)
    {
        inst_list = codegen_expr_with_result(array_expr, inst_list, ctx, &base_reg);
        if (codegen_had_error(ctx) || base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }
    }
    else
    {
        inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &base_reg);
        if (codegen_had_error(ctx) || base_reg == NULL)
        {
            free_reg(get_reg_stack(), index_reg);
            return inst_list;
        }
    }

    if (index_spill_slot != NULL)
    {
        char reload_buf[128];
        snprintf(reload_buf, sizeof(reload_buf), "\tmovl\t-%d(%%rbp), %s\n",
            index_spill_slot->offset, index_reg->bit_32);
        inst_list = add_inst(inst_list, reload_buf);
    }

    char buffer[128];

    KgpcType *array_type = array_expr->resolved_kgpc_type;
    if (array_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, array_expr->expr_data.id) != 0 && node != NULL)
        {
            if (node->type != NULL)
            {
                if (array_type == NULL)
                    array_type = node->type;
                else if (kgpc_type_is_array(node->type))
                {
                    long long current_size = kgpc_type_sizeof(array_type);
                    long long declared_size = kgpc_type_sizeof(node->type);
                    if (!kgpc_type_is_array(array_type) ||
                        (declared_size > 0 && declared_size > current_size))
                    {
                        array_type = node->type;
                    }
                }
            }
        }
    }
    if (array_type == NULL && record_field_type != NULL && kgpc_type_is_array(record_field_type))
        array_type = record_field_type;
    int array_is_open_array = 0;
    if (array_type != NULL && array_type->type_alias != NULL &&
        array_type->type_alias->is_open_array)
    {
        array_is_open_array = 1;
    }
    if (array_expr->array_is_dynamic)
        array_is_open_array = 1;
    else if (array_expr->resolved_kgpc_type != NULL &&
        array_expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY_OF_CONST)
    {
        array_is_open_array = 1;
    }

    if (!base_is_string && !base_is_pointer && array_expr->array_is_dynamic &&
        !(array_stack_node != NULL && array_stack_node->is_array && !array_stack_node->is_dynamic))
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", base_reg->bit_64, base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    KgpcArrayDimensionInfo info;
    int has_info = 0;
    if (base_is_array && array_type != NULL && !array_is_open_array &&
        kgpc_type_get_array_dimension_info(array_type, ctx->symtab, &info) == 0)
    {
        has_info = 1;
    }
    long long first_index_stride = 1;
    long long first_lower_bound = 0;
    int shortstring_index = 0;
    long long indexed_elem_size = expr_get_array_element_size(expr, ctx);
    /* WideChar/UnicodeChar arrays need stride 2 for character indexing.
     * Only trigger on actual WideChar element types — not on Word arrays
     * which also have element size 2 but use a different stride when
     * the array is an element of a larger nested array. */
    int wide_char_index =
        (expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar")));
    /* Fall back to size-2 heuristic only when no array dimension info
     * is available and the result type is not itself an array. */
    if (!wide_char_index &&
        (indexed_elem_size == 2 || expr->array_element_size == 2) &&
        (expr->resolved_kgpc_type == NULL || !kgpc_type_is_array(expr->resolved_kgpc_type)))
    {
        wide_char_index = 1;
    }
    if (!wide_char_index && array_expr != NULL)
    {
        KgpcType *indexable_type = array_expr->resolved_kgpc_type;
        if (indexable_type == NULL &&
            array_expr->type == EXPR_VAR_ID &&
            array_expr->expr_data.id != NULL &&
            ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, array_expr->expr_data.id) != 0 &&
                node != NULL && node->type != NULL)
                indexable_type = node->type;
        }
        if (indexable_type != NULL && kgpc_type_is_array(indexable_type))
        {
            KgpcType *base_elem_type = kgpc_type_get_array_element_type_resolved(indexable_type, ctx->symtab);
            if (base_elem_type != NULL &&
                kgpc_type_is_char(base_elem_type) &&
                kgpc_type_sizeof(base_elem_type) == 2)
            {
                wide_char_index = 1;
            }
        }
    }

    if (has_info)
    {
        first_index_stride = info.strides[0];
        first_lower_bound = info.dim_lowers[0];
    }
    else
    {
        if (array_is_open_array)
            first_lower_bound = 0;
        else if (record_field_lower_known)
            first_lower_bound = record_field_lower;
        else
            first_lower_bound = base_is_pointer ? 0 : (base_is_string ? 1 : expr_get_array_lower_bound(array_expr));
        long long element_size_ll = 1;
        if (codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll))
            first_index_stride = element_size_ll;
    }
    if (wide_char_index)
    {
        first_index_stride = 2;
        if (first_lower_bound < 0)
            first_lower_bound = 0;
    }
    else if (expr->resolved_kgpc_type != NULL && kgpc_type_is_array(expr->resolved_kgpc_type))
    {
        long long result_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
        if (result_size > first_index_stride)
            first_index_stride = result_size;
    }
    /* ShortString indexing is 1-based even though it is stored with a length byte at index 0.
     * Only apply shortstring indexing for character access within a ShortString (stride == 1),
     * NOT for element access in an array of ShortStrings (stride == 256). */
    if (!wide_char_index &&
        first_index_stride <= 1 &&
        (codegen_array_access_targets_shortstring(expr, ctx) ||
         codegen_expr_is_shortstring_value(array_expr)))
    {
        shortstring_index = 1;
    }
    else if (!wide_char_index &&
             array_expr != NULL && array_expr->type == EXPR_POINTER_DEREF)
    {
        struct Expression *ptr_expr = array_expr->expr_data.pointer_deref_data.pointer_expr;
        KgpcType *ptr_type = NULL;
        if (ptr_expr != NULL)
        {
            if (ptr_expr->pointer_subtype == SHORTSTRING_TYPE ||
                (ptr_expr->pointer_subtype_id != NULL &&
                 pascal_identifier_equals(ptr_expr->pointer_subtype_id, "ShortString")))
            {
                shortstring_index = 1;
            }
            ptr_type = ptr_expr->resolved_kgpc_type;
        }
        if (ptr_type == NULL && ptr_expr != NULL && ptr_expr->type == EXPR_VAR_ID &&
            ctx != NULL && ctx->symtab != NULL && ptr_expr->expr_data.id != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, ptr_expr->expr_data.id) != 0 && node != NULL)
                ptr_type = node->type;
        }
        if (ptr_type == NULL && ptr_expr != NULL && ptr_expr->type == EXPR_RECORD_ACCESS)
        {
            struct RecordField *field = codegen_lookup_record_field_expr(ptr_expr, ctx);
            if (field != NULL)
            {
                if (field->is_pointer)
                {
                    if (field->pointer_type == SHORTSTRING_TYPE)
                        shortstring_index = 1;
                    else if (field->pointer_type_id != NULL && ctx != NULL && ctx->symtab != NULL)
                    {
                        HashNode_t *type_node = NULL;
                        if (FindSymbol(&type_node, ctx->symtab, field->pointer_type_id) != 0 &&
                            type_node != NULL && type_node->type != NULL)
                        {
                            if (kgpc_type_is_shortstring(type_node->type))
                                shortstring_index = 1;
                            else
                            {
                                struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
                                if (alias != NULL && alias->is_shortstring)
                                    shortstring_index = 1;
                                else if (kgpc_type_is_array(type_node->type))
                                {
                                    int start = 0;
                                    int end = 0;
                                    KgpcType *elem = kgpc_type_get_array_element_type(type_node->type);
                                    if (elem != NULL && kgpc_type_is_char(elem) &&
                                        kgpc_type_get_array_bounds(type_node->type, &start, &end) == 0 &&
                                        start == 0 && end >= 0 && end <= 255)
                                    {
                                        shortstring_index = 1;
                                    }
                                }
                            }
                        }
                    }
                }
                if (!shortstring_index && field->type_id != NULL && ctx != NULL && ctx->symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, ctx->symtab, field->type_id) != 0 &&
                        type_node != NULL)
                    {
                        if (type_node->type != NULL && kgpc_type_is_pointer(type_node->type))
                        {
                            KgpcType *points_to = type_node->type->info.points_to;
                            if (points_to != NULL)
                            {
                                if (kgpc_type_is_shortstring(points_to))
                                    shortstring_index = 1;
                                else
                                {
                                    struct TypeAlias *alias = kgpc_type_get_type_alias(points_to);
                                    if (alias != NULL && alias->is_shortstring)
                                        shortstring_index = 1;
                                    else if (kgpc_type_is_array(points_to))
                                    {
                                        int start = 0;
                                        int end = 0;
                                        KgpcType *elem = kgpc_type_get_array_element_type(points_to);
                                        if (elem != NULL && kgpc_type_is_char(elem) &&
                                            kgpc_type_get_array_bounds(points_to, &start, &end) == 0 &&
                                            start == 0 && end >= 0 && end <= 255)
                                        {
                                            shortstring_index = 1;
                                        }
                                    }
                                }
                            }
                        }
                        if (!shortstring_index)
                        {
                            struct TypeAlias *alias = codegen_get_type_alias_from_node(type_node);
                            if (alias != NULL && alias->is_pointer)
                            {
                                if (alias->pointer_type == SHORTSTRING_TYPE)
                                    shortstring_index = 1;
                                else if (alias->pointer_type_id != NULL)
                                {
                                    HashNode_t *sub_node = NULL;
                                    if (FindSymbol(&sub_node, ctx->symtab, alias->pointer_type_id) != 0 &&
                                        sub_node != NULL && sub_node->type != NULL)
                                    {
                                        if (kgpc_type_is_shortstring(sub_node->type))
                                            shortstring_index = 1;
                                        else
                                        {
                                            struct TypeAlias *sub_alias = kgpc_type_get_type_alias(sub_node->type);
                                            if (sub_alias != NULL && sub_alias->is_shortstring)
                                                shortstring_index = 1;
                                            else if (kgpc_type_is_array(sub_node->type))
                                            {
                                                int start = 0;
                                                int end = 0;
                                                KgpcType *elem = kgpc_type_get_array_element_type(sub_node->type);
                                                if (elem != NULL && kgpc_type_is_char(elem) &&
                                                    kgpc_type_get_array_bounds(sub_node->type, &start, &end) == 0 &&
                                                    start == 0 && end >= 0 && end <= 255)
                                                {
                                                    shortstring_index = 1;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if (ptr_type != NULL && kgpc_type_is_pointer(ptr_type))
        {
            KgpcType *points_to = ptr_type->info.points_to;
            if (points_to != NULL)
            {
                if (kgpc_type_is_shortstring(points_to))
                    shortstring_index = 1;
                else
                {
                    struct TypeAlias *alias = kgpc_type_get_type_alias(points_to);
                    if (alias != NULL && alias->is_shortstring)
                        shortstring_index = 1;
                    else if (kgpc_type_is_array(points_to))
                    {
                        int start = 0;
                        int end = 0;
                        KgpcType *elem = kgpc_type_get_array_element_type(points_to);
                        if (elem != NULL && kgpc_type_is_char(elem) &&
                            kgpc_type_get_array_bounds(points_to, &start, &end) == 0 &&
                            start == 0 && end >= 0 && end <= 255)
                        {
                            shortstring_index = 1;
                        }
                    }
                }
            }
        }
    }
    if (shortstring_index)
        first_lower_bound = 1;

    /* For ShortString, skip the length byte so index 1 maps to the first character. */
    if (shortstring_index)
    {
        snprintf(buffer, sizeof(buffer), "\taddq\t$1, %s\n", base_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    {
        int index_uses_qword = expression_uses_qword(index_expr);
        if (first_lower_bound > 0)
        {
            if (index_uses_qword)
                snprintf(buffer, sizeof(buffer), "\tsubq\t$%lld, %s\n", first_lower_bound, index_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tsubl\t$%lld, %s\n", first_lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (first_lower_bound < 0)
        {
            if (index_uses_qword)
                snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", -first_lower_bound, index_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\taddl\t$%lld, %s\n", -first_lower_bound, index_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }

        if (!index_uses_qword)
            inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32, index_reg->bit_64);
    }

    static const int scaled_sizes[] = {1, 2, 4, 8};
    int can_scale = 0;
    for (size_t i = 0; i < sizeof(scaled_sizes) / sizeof(scaled_sizes[0]); ++i)
    {
        if (first_index_stride == scaled_sizes[i])
        {
            can_scale = 1;
            break;
        }
    }

    if (can_scale)
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n",
            base_reg->bit_64, index_reg->bit_64, (int)first_index_stride, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        if (first_index_stride != 1)
        {
            snprintf(buffer, sizeof(buffer), "\timulq\t$%lld, %s\n", first_index_stride, index_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n", base_reg->bit_64, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }

    /* Handle extra indices for multi-dimensional arrays. */
    if (expr->expr_data.array_access_data.extra_indices != NULL)
    {
        StackNode_t *addr_spill_slot = NULL;
        int extra_idx_num = 1;
        ListNode_t *extra_idx_node = expr->expr_data.array_access_data.extra_indices;
        while (extra_idx_node != NULL)
        {
            struct Expression *extra_idx_expr = (struct Expression *)extra_idx_node->cur;
            if (extra_idx_expr != NULL)
            {
                int spill_addr = expr_contains_function_call(extra_idx_expr);
                if (spill_addr)
                {
                    if (addr_spill_slot == NULL)
                        addr_spill_slot = add_l_t("array_addr_spill");
                    if (addr_spill_slot != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            index_reg->bit_64, addr_spill_slot->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }

                Register_t *extra_idx_reg = NULL;
                inst_list = codegen_expr_with_result(extra_idx_expr, inst_list, ctx, &extra_idx_reg);
                if (codegen_had_error(ctx) || extra_idx_reg == NULL)
                    break;

                if (spill_addr && addr_spill_slot != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        addr_spill_slot->offset, index_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                long long stride = 1;
                long long extra_lower_bound = 0;

                if (has_info && extra_idx_num < info.dim_count)
                {
                    stride = info.strides[extra_idx_num];
                    extra_lower_bound = info.dim_lowers[extra_idx_num];
                }
                else
                {
                    /* Fallback for when dimension info is not available or exceeded */
                    long long element_size_ll = 1;
                    int element_size_ok = codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll);
                    KGPC_COMPILER_HARD_ASSERT(element_size_ok,
                        "codegen_get_indexable_element_size failed in stride computation");
                    stride = element_size_ll;
                    extra_lower_bound = 1; /* Default to 1-based */
                }

                {
                    int extra_uses_qword = expression_uses_qword(extra_idx_expr);
                    if (extra_lower_bound > 0)
                    {
                        if (extra_uses_qword)
                            snprintf(buffer, sizeof(buffer), "\tsubq\t$%lld, %s\n",
                                extra_lower_bound, extra_idx_reg->bit_64);
                        else
                            snprintf(buffer, sizeof(buffer), "\tsubl\t$%lld, %s\n",
                                extra_lower_bound, extra_idx_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else if (extra_lower_bound < 0)
                    {
                        if (extra_uses_qword)
                            snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n",
                                -extra_lower_bound, extra_idx_reg->bit_64);
                        else
                            snprintf(buffer, sizeof(buffer), "\taddl\t$%lld, %s\n",
                                -extra_lower_bound, extra_idx_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    if (!extra_uses_qword)
                        inst_list = codegen_sign_extend32_to64(inst_list,
                            extra_idx_reg->bit_32, extra_idx_reg->bit_64);
                }

                if (stride != 1)
                {
                    snprintf(buffer, sizeof(buffer), "\timulq\t$%lld, %s\n",
                        stride, extra_idx_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n",
                    extra_idx_reg->bit_64, index_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);

                free_reg(get_reg_stack(), extra_idx_reg);
            }
            extra_idx_num++;
            extra_idx_node = extra_idx_node->next;
        }
    }

    free_reg(get_reg_stack(), base_reg);
    *out_reg = index_reg;
    return inst_list;
}

ListNode_t *codegen_array_access(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx, Register_t *target_reg)
{
    assert(expr != NULL);
    assert(target_reg != NULL);

    Register_t *addr_reg = NULL;
    inst_list = codegen_array_element_address(expr, inst_list, ctx, &addr_reg);
    if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

    struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
    long long element_size_ll = 4; /* default to 4-byte integer */
    if (array_expr != NULL &&
        !codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll))
    {
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
    }
    int element_size = (int)element_size_ll;

    /* Class/pointer-typed array elements are always pointer-sized (8 bytes).
     * codegen_get_indexable_element_size may return the full class instance size
     * when the base expression's resolved type is ^record (class type), but the
     * array stores pointers, not inline instances. */
    if (expr_has_type_tag(expr, POINTER_TYPE) && element_size > CODEGEN_POINTER_SIZE_BYTES)
        element_size = CODEGEN_POINTER_SIZE_BYTES;

    /* For large elements, records (passed by address), and shortstrings,
     * return the address itself rather than loading a value. */
    {
        int is_record_element = expr_has_type_tag(expr, RECORD_TYPE);
        int is_big = (element_size > CODEGEN_POINTER_SIZE_BYTES);
        int is_shortstr = codegen_expr_is_shortstring_value(expr);
        int is_shortstr2 = codegen_array_access_targets_shortstring(expr, ctx);
        if (is_big ||
            is_record_element ||
            is_shortstr || is_shortstr2)
        {
            char buffer[100];
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n",
                addr_reg->bit_64, target_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), addr_reg);
            return inst_list;
        }
    }

    char buffer[100];
    if (expr_uses_qword_kgpctype(expr) || element_size == 8)
    {
        /* 8-byte elements (including pointers, int64, etc.) need 64-bit load */
        snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        if (element_size == 2)
        {
            int type_tag = expr_get_type_tag(expr);
            if (!codegen_type_is_signed(type_tag))
                snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
            else
                snprintf(buffer, sizeof(buffer), "\tmovswl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        else if (expr_has_type_tag(expr, CHAR_TYPE) || element_size == 1)
        {
            int type_tag = expr_get_type_tag(expr);
            if (type_tag == CHAR_TYPE || !codegen_type_is_signed(type_tag))
                snprintf(buffer, sizeof(buffer), "\tmovzbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
            else
                snprintf(buffer, sizeof(buffer), "\tmovsbl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        else if (element_size == 2)
        {
            int type_tag = expr_get_type_tag(expr);
            if (!codegen_type_is_signed(type_tag))
                snprintf(buffer, sizeof(buffer), "\tmovzwl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
            else
                snprintf(buffer, sizeof(buffer), "\tmovswl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t(%s), %s\n", addr_reg->bit_64, target_reg->bit_32);
        }
        inst_list = add_inst(inst_list, buffer);
        if (expr_has_type_tag(expr, LONGINT_TYPE)) {
            if (codegen_expr_is_signed(expr))
                inst_list = codegen_sign_extend32_to64(inst_list, target_reg->bit_32, target_reg->bit_64);
            else
                inst_list = codegen_zero_extend32_to64(inst_list, target_reg->bit_32, target_reg->bit_64);
        }
    }

    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
}

static int invert_relop_type(int relop_kind)
{
    switch (relop_kind)
    {
        case EQ:
            return NE;
        case NE:
            return EQ;
        case LT:
            return GE;
        case LE:
            return GT;
        case GT:
            return LE;
        case GE:
            return LT;
        case LT_U:
            return GE_U;
        case LE_U:
            return GT_U;
        case GT_U:
            return LE_U;
        case GE_U:
            return LT_U;
        default:
            return relop_kind;
    }
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
    assert(ctx != NULL);

    CODEGEN_DEBUG("DEBUG: Generating simple relop\n");

    struct Expression *left_expr = expr->expr_data.relop_data.left;
    struct Expression *right_expr = expr->expr_data.relop_data.right;
    int relop_kind = expr->expr_data.relop_data.type;

    if (relop_type != NULL)
        *relop_type = relop_kind;

    char buffer[128];
    if (relop_kind == NOT)
    {
        int inner_type = NE;
        inst_list = codegen_condition_expr(left_expr, inst_list, ctx, &inner_type);
        if (relop_type != NULL)
            *relop_type = invert_relop_type(inner_type);
        return inst_list;
    }

    if (relop_kind == IN && right_expr != NULL && expr_is_char_set_ctx(right_expr, ctx))
    {
        if (relop_type != NULL)
            *relop_type = NE;

        Register_t *left_reg = NULL;
        inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL)
            return inst_list;

        Register_t *set_addr_reg = NULL;
        inst_list = codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
        if (codegen_had_error(ctx) || set_addr_reg == NULL)
        {
            free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }

        /* btl with memory operand auto-computes byte offset for bit positions
           beyond 31, so we can test any bit 0..255 directly. Only 2 regs needed. */
        snprintf(buffer, sizeof(buffer), "\tbtl\t%s, (%s)\n", left_reg->bit_32, set_addr_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        /* Convert CF to ZF: sbb sets reg to -1 if CF (bit set), 0 if not */
        snprintf(buffer, sizeof(buffer), "\tsbbl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), set_addr_reg);
        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    /* Non-char-set IN: evaluate the full relop expression as a boolean value */
    if (relop_kind == IN)
    {
        if (relop_type != NULL)
            *relop_type = NE;

        Register_t *result_reg = NULL;
        inst_list = codegen_expr_with_result(expr, inst_list, ctx, &result_reg);
        if (result_reg != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n",
                result_reg->bit_32, result_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), result_reg);
        }
        return inst_list;
    }

    /* For floating-point comparisons, use expr_tree-based evaluation with spilling
     * to preserve the left operand across function calls that may occur when
     * evaluating the right operand. Without this, caller-saved registers like
     * %rax would be clobbered by subsequent function calls.
     */
    int left_is_real = (left_expr != NULL && expr_has_type_tag(left_expr, REAL_TYPE));
    int right_is_real = (right_expr != NULL && expr_has_type_tag(right_expr, REAL_TYPE));
    
    Register_t *left_reg = NULL;
    Register_t *right_reg = NULL;
    StackNode_t *left_spill = NULL;
    
    if (left_is_real || right_is_real)
    {
        /* Evaluate left operand using expr_tree which properly handles results */
        inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL)
            return inst_list;
        
        /* Spill left result to stack to preserve it across the right operand evaluation
         * which may involve function calls that clobber caller-saved registers */
        left_spill = add_l_t("relop_left_spill");
        if (left_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", 
                     left_reg->bit_64, left_spill->offset);
            inst_list = add_inst(inst_list, buffer);

            /* Free left_reg temporarily; we will get a fresh register after right eval */
            free_reg(get_reg_stack(), left_reg);
            left_reg = NULL;
        }
        
        /* Evaluate right operand */
        inst_list = codegen_expr_with_result(right_expr, inst_list, ctx, &right_reg);
        if (codegen_had_error(ctx) || right_reg == NULL)
        {
            if (left_reg != NULL)
                free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }
        
        /* If we spilled the left operand, get a fresh register and reload it.
         * Otherwise, the original left_reg is still live. */
        if (left_spill != NULL)
        {
            left_reg = codegen_try_get_reg(&inst_list, ctx, "relop_left_reload");
            if (left_reg == NULL)
            {
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }

            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", 
                     left_spill->offset, left_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    else
    {
        /* Non-floating-point comparisons: evaluate both operands into registers.
         * The right operand may contain function calls (e.g. Length(s)) that clobber
         * caller-saved registers, so preserve the left operand via a spill slot. */
        inst_list = codegen_expr_with_result(left_expr, inst_list, ctx, &left_reg);
        if (codegen_had_error(ctx) || left_reg == NULL)
            return inst_list;

        const int use_qword_spill = expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
        StackNode_t *left_int_spill = add_l_t("relop_left_spill");
        if (left_int_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmov%c\t%s, -%d(%%rbp)\n",
                use_qword_spill ? 'q' : 'l',
                use_qword_spill ? left_reg->bit_64 : left_reg->bit_32,
                left_int_spill->offset);
            inst_list = add_inst(inst_list, buffer);
            free_reg(get_reg_stack(), left_reg);
            left_reg = NULL;
        }

        inst_list = codegen_expr_with_result(right_expr, inst_list, ctx, &right_reg);
        if (codegen_had_error(ctx) || right_reg == NULL)
        {
            if (left_reg != NULL)
                free_reg(get_reg_stack(), left_reg);
            return inst_list;
        }

        if (left_int_spill != NULL)
        {
            left_reg = codegen_try_get_reg(&inst_list, ctx, "relop_left_reload");
            if (left_reg == NULL)
            {
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }
            snprintf(buffer, sizeof(buffer), "\tmov%c\t-%d(%%rbp), %s\n",
                use_qword_spill ? 'q' : 'l',
                left_int_spill->offset,
                use_qword_spill ? left_reg->bit_64 : left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
    }

    if (left_reg == NULL || right_reg == NULL)
        return inst_list;

    int left_is_shortstring = (left_expr != NULL && codegen_expr_is_shortstring_value_ctx(left_expr, ctx) &&
        !expr_has_type_tag(left_expr, STRING_TYPE));
    int right_is_shortstring = (right_expr != NULL && codegen_expr_is_shortstring_value_ctx(right_expr, ctx) &&
        !expr_has_type_tag(right_expr, STRING_TYPE));
    int left_is_char_array = (left_expr != NULL && codegen_expr_is_char_array_like_ctx(left_expr, ctx) &&
        !left_is_shortstring);
    int right_is_char_array = (right_expr != NULL && codegen_expr_is_char_array_like_ctx(right_expr, ctx) &&
        !right_is_shortstring);

    int left_is_string = (left_expr != NULL && (expr_has_type_tag(left_expr, STRING_TYPE) ||
        left_is_shortstring || left_is_char_array));
    int right_is_string = (right_expr != NULL && (expr_has_type_tag(right_expr, STRING_TYPE) ||
        right_is_shortstring || right_is_char_array));
    int left_is_char_ptr = expr_is_char_pointer(left_expr);
    int right_is_char_ptr = expr_is_char_pointer(right_expr);

    /* When comparing a string to a char (EXPR_CHAR_CODE or single-char literal),
     * promote the char operand to a string via kgpc_char_to_string so the
     * comparison uses kgpc_string_compare.  This fixes segfaults from passing
     * raw char integers where string pointers are expected.
     * 
     * The semantic checker may promote EXPR_CHAR_CODE to STRING_TYPE for equality
     * comparisons, so right_is_string can be TRUE even when the register holds
     * a raw char integer.  Detect this by checking the expression node type. */
    int right_needs_char_promo = (right_expr != NULL &&
        (right_expr->type == EXPR_CHAR_CODE ||
         (right_expr->type == EXPR_STRING && expr_get_type_tag(right_expr) == CHAR_TYPE) ||
         (right_expr->type == EXPR_VAR_ID && expr_get_type_tag(right_expr) == CHAR_TYPE)));
    int left_needs_char_promo = (left_expr != NULL &&
        (left_expr->type == EXPR_CHAR_CODE ||
         (left_expr->type == EXPR_STRING && expr_get_type_tag(left_expr) == CHAR_TYPE) ||
         (left_expr->type == EXPR_VAR_ID && expr_get_type_tag(left_expr) == CHAR_TYPE)));
    if ((left_is_string || right_is_string) && right_needs_char_promo)
    {
        StackNode_t *lhs_spill = add_l_t("relop_str_char_lhs");
        if (lhs_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                left_reg->bit_64, lhs_spill->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", right_reg->bit_32, arg_reg32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", right_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();
        if (lhs_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                lhs_spill->offset, left_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        right_is_string = 1;
        right_needs_char_promo = 0;
    }
    if ((left_is_string || right_is_string) && left_needs_char_promo)
    {
        StackNode_t *rhs_spill = add_l_t("relop_str_char_rhs");
        if (rhs_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                right_reg->bit_64, rhs_spill->offset);
            inst_list = add_inst(inst_list, buffer);
        }
        const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", left_reg->bit_32, arg_reg32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", left_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();
        if (rhs_spill != NULL)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                rhs_spill->offset, right_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
        }
        left_is_string = 1;
        left_needs_char_promo = 0;
    }

    if ((left_is_char_array || right_is_char_array) &&
        (left_is_string || right_is_string || left_is_char_ptr || right_is_char_ptr))
    {
        long long array_len = 0;
        long long rhs_array_len = 0;
        int invert_cmp = 0;
        const char *cmp_func = "kgpc_char_array_compare";
        int compare_full = ((left_is_char_array && right_is_string && !right_is_char_ptr) ||
                            (right_is_char_array && left_is_string && !left_is_char_ptr));

        if (left_is_char_array && right_is_char_array)
        {
            if (!codegen_get_char_array_length(left_expr, ctx, &array_len) ||
                !codegen_get_char_array_length(right_expr, ctx, &rhs_array_len))
            {
                free_reg(get_reg_stack(), right_reg);
                free_reg(get_reg_stack(), left_reg);
                if (relop_type != NULL)
                    *relop_type = relop_kind;
                return inst_list;
            }
            cmp_func = "kgpc_char_array_compare_array";
        }
        else if (left_is_char_array)
        {
            if (!codegen_get_char_array_length(left_expr, ctx, &array_len))
            {
                free_reg(get_reg_stack(), right_reg);
                free_reg(get_reg_stack(), left_reg);
                if (relop_type != NULL)
                    *relop_type = relop_kind;
                return inst_list;
            }
        }
        else
        {
            if (!codegen_get_char_array_length(right_expr, ctx, &array_len))
            {
                free_reg(get_reg_stack(), right_reg);
                free_reg(get_reg_stack(), left_reg);
                if (relop_type != NULL)
                    *relop_type = relop_kind;
                return inst_list;
            }
            invert_cmp = 1;
        }

        if (left_is_shortstring)
        {
            StackNode_t *right_preserve = NULL;
            if (right_reg != NULL)
            {
                right_preserve = add_l_t("relop_right_preserve");
                if (right_preserve != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        right_reg->bit_64, right_preserve->offset);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            inst_list = codegen_promote_shortstring_reg(inst_list, ctx, left_reg);
            if (right_preserve != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    right_preserve->offset, right_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }
        if (right_is_shortstring)
        {
            StackNode_t *left_preserve = NULL;
            if (left_reg != NULL)
            {
                left_preserve = add_l_t("relop_left_preserve");
                if (left_preserve != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                        left_reg->bit_64, left_preserve->offset);
                    inst_list = add_inst(inst_list, buffer);
                }
            }
            inst_list = codegen_promote_shortstring_reg(inst_list, ctx, right_reg);
            if (left_preserve != NULL)
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                    left_preserve->offset, left_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
        }

        const char *arg0 = current_arg_reg64(0);
        const char *arg1 = current_arg_reg64(1);
        const char *arg2 = current_arg_reg64(2);
        const char *arg3 = current_arg_reg64(3);
        if (arg0 == NULL || arg1 == NULL || (strcmp(cmp_func, "kgpc_char_array_compare_array") == 0 &&
            (arg2 == NULL || arg3 == NULL)))
        {
            free_reg(get_reg_stack(), right_reg);
            free_reg(get_reg_stack(), left_reg);
            if (relop_type != NULL)
                *relop_type = relop_kind;
            return inst_list;
        }

        if (strcmp(cmp_func, "kgpc_char_array_compare_array") == 0)
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", left_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len, arg1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", right_reg->bit_64, arg2);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", rhs_array_len, arg3);
            inst_list = add_inst(inst_list, buffer);
        }
        else if (!invert_cmp)
        {
            if (compare_full)
                cmp_func = "kgpc_char_array_compare_full";
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", left_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len, arg1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", right_reg->bit_64, arg2);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            if (compare_full)
                cmp_func = "kgpc_char_array_compare_full";
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", right_reg->bit_64, arg0);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", array_len, arg1);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", left_reg->bit_64, arg2);
            inst_list = add_inst(inst_list, buffer);
        }

        inst_list = codegen_vect_reg(inst_list, 0);
        snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", cmp_func);
        inst_list = add_inst(inst_list, buffer);
        if (invert_cmp)
            inst_list = add_inst(inst_list, "\tnegl\t%eax\n");
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", RETURN_REG_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();

        free_reg(get_reg_stack(), right_reg);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), left_reg);

        if (relop_type != NULL)
            *relop_type = relop_kind;
        return inst_list;
    }

    if ((left_is_string && right_is_string) ||
        (left_is_string && right_is_char_ptr) ||
        (right_is_string && left_is_char_ptr))
    {
        const char *lhs_arg = current_arg_reg64(0);
        const char *rhs_arg = current_arg_reg64(1);
        if (lhs_arg == NULL || rhs_arg == NULL)
        {
            free_reg(get_reg_stack(), right_reg);
            free_reg(get_reg_stack(), left_reg);
            if (relop_type != NULL)
                *relop_type = relop_kind;
            return inst_list;
        }

        if (left_is_shortstring)
        {
            StackNode_t *rhs_spill = NULL;
            inst_list = codegen_spill_reg64_temp(inst_list, right_reg,
                "relop_shortstr_rhs_spill", &rhs_spill);
            inst_list = codegen_promote_shortstring_reg(inst_list, ctx, left_reg);
            inst_list = codegen_restore_spilled_reg64(inst_list, right_reg, rhs_spill);
        }
        if (right_is_shortstring)
        {
            StackNode_t *lhs_spill = NULL;
            inst_list = codegen_spill_reg64_temp(inst_list, left_reg,
                "relop_shortstr_lhs_spill", &lhs_spill);
            inst_list = codegen_promote_shortstring_reg(inst_list, ctx, right_reg);
            inst_list = codegen_restore_spilled_reg64(inst_list, left_reg, lhs_spill);
        }

        /* Promote char-typed operands (EXPR_CHAR_CODE or single-char EXPR_STRING)
         * that hold raw integer values to string pointers before calling
         * kgpc_string_compare. The semantic checker may set resolved_kgpc_type
         * to STRING_TYPE but the expression evaluator still loads char integers. */
        if (left_needs_char_promo)
        {
            StackNode_t *rhs_spill = NULL;
            inst_list = codegen_spill_reg64_temp(inst_list, right_reg,
                "relop_rhs_char_promo", &rhs_spill);
            inst_list = codegen_promote_char_reg_to_string(inst_list, left_reg);
            inst_list = codegen_restore_spilled_reg64(inst_list, right_reg, rhs_spill);
        }
        if (right_needs_char_promo)
        {
            StackNode_t *lhs_spill = NULL;
            inst_list = codegen_spill_reg64_temp(inst_list, left_reg,
                "relop_lhs_char_promo", &lhs_spill);
            inst_list = codegen_promote_char_reg_to_string(inst_list, right_reg);
            inst_list = codegen_restore_spilled_reg64(inst_list, left_reg, lhs_spill);
        }

        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", right_reg->bit_64, rhs_arg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", left_reg->bit_64, lhs_arg);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_compare");
        snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", RETURN_REG_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();

        free_reg(get_reg_stack(), right_reg);
        snprintf(buffer, sizeof(buffer), "\tcmpl\t$0, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        free_reg(get_reg_stack(), left_reg);

        if (relop_type != NULL)
            *relop_type = relop_kind;
        return inst_list;
    }

    if (relop_kind == IN)
    {
        if (relop_type != NULL)
            *relop_type = NE;

        /* Check if this is a character set IN operation (32-byte set for values 0..255)
         * vs. a small integer set (4-byte bitmask for values 0..31).
         * 
         * Character sets require memory-based indexing (32-byte array).
         * Small integer sets can use simple btl instruction on a 32-bit register.
         */
        HashNode_t *right_const_set = NULL;
        int right_is_char_set = 0; /* Only set to 1 if actually a char set */
        
        if (right_expr != NULL && right_expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, right_expr->expr_data.id) && node != NULL &&
                node->hash_type == HASHTYPE_CONST && node->const_set_value != NULL &&
                node->const_set_size > 0)
            {
                right_const_set = node;
                /* Check if this is a char set (32 bytes) or small set (4 bytes) */
                right_is_char_set = (node->const_set_size > 4);
            }
        }
        
        /* For set expressions (literals and variables), check if they are char sets */
        if (!right_is_char_set && right_expr != NULL)
        {
            right_is_char_set = expr_is_char_set_ctx(right_expr, ctx);
        }

        if (right_const_set != NULL && right_const_set->const_set_value != NULL)
        {
            Register_t *set_addr_reg = NULL;

            inst_list = codegen_emit_const_set_rodata(right_const_set, inst_list, ctx);
            if (codegen_had_error(ctx) || right_const_set->const_set_label == NULL)
            {
                free_reg(get_reg_stack(), left_reg);
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }

            set_addr_reg = codegen_try_get_reg(&inst_list, ctx, "const set addr");
            if (set_addr_reg == NULL)
            {
                free_reg(get_reg_stack(), left_reg);
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }

            char buffer_addr[96];
            snprintf(buffer_addr, sizeof(buffer_addr), "\tleaq\t%s(%%rip), %s\n",
                right_const_set->const_set_label, set_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer_addr);

            /* btl with memory operand handles both 4-byte small sets and 32-byte char sets. */
            snprintf(buffer, sizeof(buffer), "\tbtl\t%s, (%s)\n", left_reg->bit_32, set_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tsbbl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), set_addr_reg);
            free_reg(get_reg_stack(), left_reg);
            free_reg(get_reg_stack(), right_reg);
            return inst_list;
        }

        if (right_is_char_set)
        {
            /* For character sets: right operand is 32-byte array, left is char value (0-255)
             * Algorithm:
             * 1. dword_index = value / 32  (which of 8 dwords)
             * 2. bit_index = value % 32    (which bit in that dword)
             * 3. Load the appropriate dword from set variable
             * 4. Test the appropriate bit
             */

            /* Get address of the set variable */
            Register_t *set_addr_reg = NULL;
            inst_list = codegen_char_set_address(right_expr, inst_list, ctx, &set_addr_reg);
            if (codegen_had_error(ctx) || set_addr_reg == NULL)
            {
                if (set_addr_reg != NULL)
                    free_reg(get_reg_stack(), set_addr_reg);
                free_reg(get_reg_stack(), left_reg);
                free_reg(get_reg_stack(), right_reg);
                return inst_list;
            }

            /* btl with memory operand auto-computes byte offset for any bit position */
            snprintf(buffer, sizeof(buffer), "\tbtl\t%s, (%s)\n", left_reg->bit_32, set_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            /* Convert CF to ZF: sbb sets reg to -1 if CF (bit set), 0 if not */
            snprintf(buffer, sizeof(buffer), "\tsbbl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), set_addr_reg);
            free_reg(get_reg_stack(), left_reg);
            free_reg(get_reg_stack(), right_reg);
            return inst_list;
        }

        /* Regular 32-bit sets — use btl (avoids %ecx conflict) */
        snprintf(buffer, sizeof(buffer), "\tbtl\t%s, %s\n", left_reg->bit_32, right_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        /* Convert CF to ZF: sbb sets reg to -1 if CF (bit set), 0 if not */
        snprintf(buffer, sizeof(buffer), "\tsbbl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), left_reg);
        free_reg(get_reg_stack(), right_reg);
        return inst_list;
    }

    if (left_expr != NULL && expr_has_type_tag(left_expr, REAL_TYPE))
    {
        char true_label[32];
        char done_label[32];
        gen_label(true_label, sizeof(true_label), ctx);
        gen_label(done_label, sizeof(done_label), ctx);
        const char *left_name = register_name_for_type(left_reg, REAL_TYPE);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm1\n", left_name);
        inst_list = add_inst(inst_list, buffer);

        int right_declared_integer_like = 0;
        if (right_expr != NULL && right_expr->type == EXPR_VAR_ID &&
            right_expr->expr_data.id != NULL && ctx != NULL && ctx->symtab != NULL)
        {
            HashNode_t *right_node = NULL;
            if (FindSymbol(&right_node, ctx->symtab, right_expr->expr_data.id) != 0 &&
                right_node != NULL && right_node->type != NULL)
            {
                int right_declared_tag = codegen_tag_from_kgpc(right_node->type);
                if (is_integer_type(right_declared_tag) || right_declared_tag == BOOL ||
                    right_declared_tag == CHAR_TYPE || right_declared_tag == ENUM_TYPE)
                {
                    right_declared_integer_like = 1;
                }
            }
        }

        if (right_expr != NULL && expr_has_type_tag(right_expr, REAL_TYPE) &&
            !right_declared_integer_like)
        {
            /* Values in GP registers are always promoted to double (64-bit),
             * even when the declared type is Single. Use movq, not movd. */
            const char *right_name = register_name_for_type(right_reg, REAL_TYPE);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", right_name);
            inst_list = add_inst(inst_list, buffer);
        }
        else
        {
            int right_tag = (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
            if (codegen_type_uses_qword(right_tag))
                snprintf(buffer, sizeof(buffer), "\tcvtsi2sdq\t%s, %%xmm0\n", right_reg->bit_64);
            else
                snprintf(buffer, sizeof(buffer), "\tcvtsi2sdl\t%s, %%xmm0\n", right_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
        }
        snprintf(buffer, sizeof(buffer), "\txorl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tucomisd\t%xmm0, %xmm1\n");

        int relop_kind = expr->expr_data.relop_data.type;
        switch (relop_kind)
        {
            case EQ:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tje\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case NE:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case LT:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjb\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case LE:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjbe\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case GT:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tja\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            case GE:
                snprintf(buffer, sizeof(buffer), "\tjp\t%s\n", done_label);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tjae\t%s\n", true_label);
                inst_list = add_inst(inst_list, buffer);
                break;
            default:
                break;
        }

        snprintf(buffer, sizeof(buffer), "\tjmp\t%s\n", done_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", true_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovl\t$1, %s\n", left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "%s:\n", done_label);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\ttestl\t%s, %s\n", left_reg->bit_32, left_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);

        if (relop_type != NULL)
            *relop_type = NE;

        free_reg(get_reg_stack(), right_reg);
        free_reg(get_reg_stack(), left_reg);
        return inst_list;
    }

    int use_qword = expression_uses_qword(left_expr) || expression_uses_qword(right_expr);
    /* When either operand is 64-bit (pointers, qword ints), make sure we also
     * use the 64-bit register names for the comparison. Using LONGINT_TYPE here
     * used to work when LongInt was 8 bytes, but now emits 32-bit names (rXXd)
     * which are invalid with a qword cmp suffix. */
    const char *left_name = use_qword
        ? left_reg->bit_64
        : register_name_for_expr(left_reg, left_expr);
    const char *right_name = use_qword
        ? right_reg->bit_64
        : register_name_for_expr(right_reg, right_expr);

    /* Sign/zero-extend 32-bit operands for 64-bit comparison.
     * When one side is qword and the other was computed as 32-bit,
     * the upper 32 bits may not match the intended value (e.g. negl
     * produces 32-bit -1 = 0x00000000FFFFFFFF instead of 64-bit -1). */
    if (use_qword)
    {
        if (left_reg != NULL && !expression_uses_qword(left_expr))
        {
            int left_tag = (left_expr != NULL) ? expr_get_type_tag(left_expr) : UNKNOWN_TYPE;
            if (codegen_type_is_signed(left_tag))
                inst_list = codegen_sign_extend32_to64(inst_list, left_reg->bit_32, left_reg->bit_64);
            else
                inst_list = codegen_zero_extend32_to64(inst_list, left_reg->bit_32, left_reg->bit_32);
        }
        if (right_reg != NULL && !expression_uses_qword(right_expr))
        {
            int right_tag = (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
            if (codegen_type_is_signed(right_tag))
                inst_list = codegen_sign_extend32_to64(inst_list, right_reg->bit_32, right_reg->bit_64);
            else
                inst_list = codegen_zero_extend32_to64(inst_list, right_reg->bit_32, right_reg->bit_32);
        }
    }

    snprintf(buffer, sizeof(buffer), "\tcmp%c\t%s, %s\n", use_qword ? 'q' : 'l', right_name, left_name);
    inst_list = add_inst(inst_list, buffer);
    free_reg(get_reg_stack(), right_reg);
    free_reg(get_reg_stack(), left_reg);

    /* For unsigned integer types, convert relop to unsigned variant so
     * gencode_jmp emits jb/jbe/ja/jae instead of jl/jle/jg/jge. */
    if (relop_type != NULL)
    {
        int left_tag = (left_expr != NULL) ? expr_get_type_tag(left_expr) : UNKNOWN_TYPE;
        int right_tag = (right_expr != NULL) ? expr_get_type_tag(right_expr) : UNKNOWN_TYPE;
        int either_unsigned = (!codegen_type_is_signed(left_tag) && left_tag != UNKNOWN_TYPE &&
                               left_tag != BOOL && left_tag != CHAR_TYPE && left_tag != ENUM_TYPE &&
                               left_tag != REAL_TYPE && left_tag != STRING_TYPE &&
                               left_tag != SHORTSTRING_TYPE && left_tag != POINTER_TYPE) ||
                              (!codegen_type_is_signed(right_tag) && right_tag != UNKNOWN_TYPE &&
                               right_tag != BOOL && right_tag != CHAR_TYPE && right_tag != ENUM_TYPE &&
                               right_tag != REAL_TYPE && right_tag != STRING_TYPE &&
                               right_tag != SHORTSTRING_TYPE && right_tag != POINTER_TYPE);
        if (either_unsigned)
        {
            switch (*relop_type)
            {
                case LT: *relop_type = LT_U; break;
                case LE: *relop_type = LE_U; break;
                case GT: *relop_type = GT_U; break;
                case GE: *relop_type = GE_U; break;
                default: break; /* EQ/NE don't care about signedness */
            }
        }
    }

    CODEGEN_DEBUG("DEBUG: Simple relop generated\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Code generation for relop expressions that need a value result (not just flags).
 * This is used by builtin write functions that need the boolean value in a register.
 * Returns the result in out_reg as 0 or 1.
 */
ListNode_t *codegen_relop_to_value(struct Expression *expr, ListNode_t *inst_list,
    CodeGenContext *ctx, Register_t **out_reg)
{
    if (expr == NULL || ctx == NULL || out_reg == NULL)
    {
        if (out_reg != NULL)
            *out_reg = NULL;
        return inst_list;
    }

    if (expr->type != EXPR_RELOP)
    {
        /* Not a relop - fall back to normal expression evaluation */
        return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
    }

    int relop_kind = expr->expr_data.relop_data.type;
    struct Expression *right_expr = expr->expr_data.relop_data.right;

    /* Check if this is an IN operation on a char set - requires special handling */
    int is_char_set_in = 0;
    if (relop_kind == IN && right_expr != NULL)
    {
        is_char_set_in = expr_is_char_set_ctx(right_expr, ctx);
        /* Also check for char set constants via variable reference */
        if (!is_char_set_in && right_expr->type == EXPR_VAR_ID && ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, right_expr->expr_data.id) != 0 && node != NULL &&
                node->hash_type == HASHTYPE_CONST && node->const_set_value != NULL &&
                node->const_set_size > 4) /* 32-byte char set */
            {
                is_char_set_in = 1;
            }
        }
    }

    if (!is_char_set_in)
    {
        /* For non-char-set operations, the expression tree approach works */
        return codegen_expr_tree_value(expr, inst_list, ctx, out_reg);
    }

    /* For char set IN operations, we need to use codegen_simple_relop
     * which properly handles the 32-byte set, then convert flags to value */
    
    int relop_type = 0;
    inst_list = codegen_simple_relop(expr, inst_list, ctx, &relop_type);
    
    /* codegen_simple_relop sets CPU flags. We need to convert to a 0/1 value.
     * The relop_type tells us what condition was tested:
     * - NE means "not equal" - we want setnz to get 1 when flag is set
     */
    Register_t *result_reg = codegen_try_get_reg(&inst_list, ctx, "relop result");
    if (result_reg == NULL)
    {
        *out_reg = NULL;
        return inst_list;
    }

    char buffer[64];
    const char *set_instr = "setnz"; /* Default for set membership */
    
    /* Convert flags to value based on relop type */
    switch (relop_type)
    {
        case EQ:
            set_instr = "sete";
            break;
        case NE:
            set_instr = "setne";
            break;
        case LT:
            set_instr = "setl";
            break;
        case LE:
            set_instr = "setle";
            break;
        case GT:
            set_instr = "setg";
            break;
        case GE:
            set_instr = "setge";
            break;
        default:
            set_instr = "setnz";
            break;
    }

    const char *reg8 = codegen_register_name8(result_reg);
    if (reg8 != NULL)
    {
        snprintf(buffer, sizeof(buffer), "\t%s\t%s\n", set_instr, reg8);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovzbl\t%s, %s\n", reg8, result_reg->bit_32);
        inst_list = add_inst(inst_list, buffer);
    }

    *out_reg = result_reg;
    return inst_list;
}

/* Code generation for non-local variable access */
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
    StackNode_t *var = find_label_with_depth(var_id, &scope_depth);
    HashNode_t *sym_node = NULL;

    if (var == NULL && ctx != NULL && ctx->symtab != NULL)
    {
        if (FindSymbol(&sym_node, ctx->symtab, var_id) != 0 &&
            sym_node != NULL && sym_node->mangled_id != NULL)
        {
            var = find_label_with_depth(sym_node->mangled_id, &scope_depth);
        }
    }

    if(var == NULL) {
        if (ctx != NULL && ctx->symtab != NULL)
        {
            const Tree_t *decl = NULL;
            const char *global_symbol = NULL;
            if (sym_node != NULL &&
                (sym_node->hash_type == HASHTYPE_VAR ||
                 sym_node->hash_type == HASHTYPE_ARRAY) &&
                (sym_node->source_unit_index > 0 || sym_node->defined_in_unit))
            {
                decl = codegen_find_var_decl_for_symbol(ctx, sym_node, var_id);
            }
            if (decl == NULL && ctx->symtab->current_unit_index > 0)
                decl = codegen_find_var_decl_for_unit(ctx, ctx->symtab->current_unit_index, var_id);

            global_symbol = codegen_global_access_symbol_for_decl(decl, var_id);
            if (global_symbol != NULL && global_symbol[0] != '\0')
            {
                if (sym_node != NULL)
                    sym_node->referenced += 1;
                *offset = 0;
                snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                    global_symbol, current_non_local_reg64());
                inst_list = add_inst(inst_list, buffer);
                return inst_list;
            }
        }

        if (ctx != NULL && ctx->symtab != NULL &&
            ctx->current_subprogram_owner_class != NULL)
        {
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
                if (class_record != NULL &&
                    codegen_record_has_class_var_named(class_record, var_id))
                {
                    *offset = 0;
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n",
                        var_id, current_non_local_reg64());
                    inst_list = add_inst(inst_list, buffer);
                    return inst_list;
                }
            }
        }

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
                        /* Load Self pointer, then add field offset. */
                        *offset = 0;
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
                        /* Record this method label so we can emit a weak
                         * stub in the final pass if no real definition
                         * appears in the assembly output. */
                        codegen_add_unresolved_method_stub(method_label);
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

        codegen_report_error(ctx,
            "ERROR: Unresolved non-local symbol %s reached codegen fallback.",
            var_id);
        assert(!"unresolved non-local symbol reached codegen fallback");
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

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list,
    CodeGenContext *ctx, struct KgpcType *proc_type, const char *procedure_name,
    int arg_start_index, const struct Expression *call_expr)
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

                Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
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
                snprintf(str_literal_buffer, sizeof(str_literal_buffer), "%s\n%s:\n\t.string \"%s\"\n\t.text\n",
                         readonly_section, label, escaped_str);
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
                Register_t *desc_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
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

                if (codegen_expr_is_shortstring_value_ctx(arg_expr, ctx))
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

                    if (codegen_target_is_windows())
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", src_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
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

                    if (codegen_target_is_windows())
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %%r8\n", array_len);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r9d\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
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

                    if (codegen_target_is_windows())
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", shortstr_size);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", buf_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
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

                    /* BUGFIX: For TRUE var parameters of class types, we pass the ADDRESS of the variable itself,
                     * not the value it contains. This allows the callee to update the variable (e.g., FreeAndNil).
                     *
                     * However, for class methods, Self (first parameter) needs to be dereferenced to pass the
                     * instance pointer, even though it's technically a var parameter internally. */

                    struct RecordType *arg_record = codegen_expr_record_type(arg_expr,
                        ctx != NULL ? ctx->symtab : NULL);
                    if (addr_reg != NULL && arg_expr != NULL && arg_expr->type != EXPR_AS &&
                        arg_record != NULL && record_type_is_class(arg_record))
                    {
                        int is_class_method = 0;

                        /* Detect if this is a class method from the codegen context. */
                        if (ctx != NULL && ctx->current_subprogram_owner_class != NULL)
                            is_class_method = 1;

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

                        /* For class methods, dereference the first argument (Self) to get instance pointer,
                         * BUT only if Self was not already loaded by value (i.e., not a var param).
                         * For non-methods with var parameters, don't dereference. */
                        int should_dereference = 0;
                        int called_is_method = 0;
                        if (procedure_name != NULL && ctx->symtab != NULL)
                        {
                            HashNode_t *called_func = NULL;
                            if (FindSymbol(&called_func, ctx->symtab, procedure_name) != 0 &&
                                called_func != NULL && called_func->owner_class != NULL)
                                called_is_method = 1;
                        }
                        if (is_class_method && arg_num == 0 && !arg_is_var_param &&
                            called_is_method)
                        {
                            /* Class method Self from local variable: dereference to get instance pointer */
                            should_dereference = 1;
                        }
                        else if (!is_var_param && !arg_is_var_param)
                        {
                            /* Non-var class parameter from local variable: dereference to get instance pointer */
                            should_dereference = 1;
                        }
                        else if (arg_num == 0 && is_var_param && !arg_is_var_param &&
                                 called_is_method)
                        {
                            /* Self parameter for a method call from outside a class method context.
                             * The formal Self parameter is a var param, but for a global/static
                             * class/interface variable, codegen_address_for_expr emits leaq (address
                             * of the variable), so we need to dereference to get the object pointer. */
                            should_dereference = 1;
                        }
                        /* else: var parameter of class type OR argument is already a var param:
                         * codegen_address_for_expr already loaded the value, don't dereference again */

                        if (should_dereference)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                                addr_reg->bit_64, addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                        }

                        /* For class method calls on instances, Self must be the VMT pointer,
                         * not the instance pointer. After the dereference above, addr_reg holds
                         * the instance pointer. Dereference again to get VMT from offset 0. */
                        if (should_dereference && arg_num == 0 &&
                            codegen_call_requires_class_method_vmt_self(call_expr, ctx))
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
                                addr_reg->bit_64, addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                }
                if (codegen_had_error(ctx) || addr_reg == NULL)
                    return inst_list;

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
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", value_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%r8d\n", descriptor_size);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", value_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", descriptor_size);
                        inst_list = add_inst(inst_list, buffer);
                    }

                    inst_list = codegen_vect_reg(inst_list, 0);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_assign_from_temp");
                    free_arg_regs();
                    free_reg(get_reg_stack(), value_reg);
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
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", temp_slot->offset);
                    inst_list = add_inst(inst_list, copy_buffer);
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t$%lld, %%r8\n", record_size);
                    inst_list = add_inst(inst_list, copy_buffer);
                }
                else
                {
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buffer);
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
                    snprintf(copy_buffer, sizeof(copy_buffer), "\tleaq\t-%d(%%rbp), %s\n", temp_slot->offset, result_reg->bit_64);
                    inst_list = add_inst(inst_list, copy_buffer);
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

                if (expected_type == REAL_TYPE)
                    inst_list = codegen_expr_maybe_convert_int_like_to_real(expected_type,
                        arg_expr, top_reg, inst_list);

                /* Extended sret function calls leave the buffer ADDRESS in the
                 * register.  Convert to double bits for callees expecting a
                 * regular double argument (e.g. kgpc_trunc). */
                if (expected_type == REAL_TYPE &&
                    expr_returns_sret(arg_expr) &&
                    codegen_expr_involves_extended(arg_expr))
                {
                    if (codegen_target_is_windows())
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", top_reg->bit_64);
                    else
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_vect_reg(inst_list, 0);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_load_extended_to_bits");
                    free_arg_regs();
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
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
                    (formal_decl_expects_string(formal_arg_decl) ||
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
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", top_reg->bit_32, arg_reg32);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                if (formal_expects_wide_string &&
                    arg_is_char_value &&
                    !codegen_expr_is_wide_string_value(arg_expr))
                {
                    const char *arg_reg32 = codegen_target_is_windows() ? "%ecx" : "%edi";
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", top_reg->bit_32, arg_reg32);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_unicodestring_from_string");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                if (formal_expects_string &&
                    !formal_expects_wide_string &&
                    arg_expr != NULL &&
                    codegen_expr_is_wide_string_value(arg_expr))
                {
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_from_unicodestring");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                if (formal_expects_string &&
                    !formal_expects_wide_string &&
                    arg_expr != NULL &&
                    arg_expr->type == EXPR_STRING &&
                    expr_get_type_tag(arg_expr) != CHAR_TYPE &&
                    !codegen_expr_is_wide_string_value(arg_expr))
                {
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_string_duplicate");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                if (formal_expects_wide_string &&
                    !codegen_expr_is_wide_string_value(arg_expr) &&
                    (expr_has_type_tag(arg_expr, STRING_TYPE) ||
                     expr_has_type_tag(arg_expr, SHORTSTRING_TYPE) ||
                     arg_expr->type == EXPR_STRING))
                {
                    const char *arg_reg64 = codegen_target_is_windows() ? "%rcx" : "%rdi";
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_unicodestring_from_string");
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s\n", top_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }

                if (arg_num == 0 &&
                    codegen_call_requires_class_method_vmt_self(call_expr, ctx) &&
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
         * truncate it, so suppress the sign-extension for pointer-like args. */
        int needs_int_to_long = (expected_type == LONGINT_TYPE && actual_type == INT_TYPE
                                 && !is_ptr_like);
        int pass_on_stack = (arg_infos != NULL && arg_infos[i].pass_via_stack);

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
                    snprintf(buffer, sizeof(buffer), "\tmovss\t%s, %%xmm0\n", stored_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                }
                else
                {
                    /* Direct GP-backed Single arguments are carried as 32-bit payloads. */
                    snprintf(buffer, sizeof(buffer), "\tmovd\t%s, %%xmm0\n", stored_reg->bit_32);
                    inst_list = add_inst(inst_list, buffer);
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
                        snprintf(buffer, sizeof(buffer), "\tmovd\t%s, %%xmm0\n", stored_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
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
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", stored_reg->bit_64, stack_dest);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", stored_reg->bit_64, arg_reg_char);
                inst_list = add_inst(inst_list, buffer);
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
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dst_addr->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_addr->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
                    free_reg(get_reg_stack(), dst_addr);
                }
                else
                {
                    snprintf(buffer, sizeof(buffer), "\tleaq\t%d(%%rsp), %%rdi\n",
                        arg_infos[i].stack_offset);
                    inst_list = add_inst(inst_list, buffer);
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_addr->bit_64);
                    inst_list = add_inst(inst_list, buffer);
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
                    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%xmm0\n", temp_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
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
                        snprintf(buffer, sizeof(buffer), "\tmovd\t%s, %%xmm0\n", temp_reg->bit_32);
                        inst_list = add_inst(inst_list, buffer);
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
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", temp_reg->bit_64, stack_dest);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", temp_reg->bit_64, arg_reg_char);
                inst_list = add_inst(inst_list, buffer);
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
