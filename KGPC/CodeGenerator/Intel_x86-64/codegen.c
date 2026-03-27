/*
    Damon Gwinn
    Code generation
    This is the dragon slayer

    See codegen.h for stack and implementation details
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
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
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/NameMangling.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../Parser/SemanticCheck/SemCheck.h"

#include "../../identifier_utils.h"
#include "../../unit_registry.h"

static int codegen_return_storage_size(KgpcType *return_type);
static int codegen_return_type_id_storage_size(const char *return_type_id);
static int codegen_float_native_distance(Tree_t *sub);
static int codegen_list_contains_string(ListNode_t *list, const char *value);
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);
const char *codegen_find_class_method_impl_id(SymTab_t *symtab,
    const struct RecordType *record, const char *fallback_class_label,
    const char *iface_name, const char *method_name);
static void codegen_collect_inferred_interfaces(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const char ***out_names, int *out_count);
static const struct RecordType *codegen_record_parent(
    const struct RecordType *record, SymTab_t *symtab);
static void codegen_emit_global_jump_stub(CodeGenContext *ctx,
    const char *exported_symbol, const char *target_symbol);
static void codegen_assert_interface_impl_resolved(const char *iface_name,
    const char *method_name, const char *class_label,
    const char *iface_symbol, const char *impl_symbol);
static void codegen_emit_const_decl_equivs_from_list(CodeGenContext *ctx,
    ListNode_t *const_decls);
static void codegen_register_owner_unit_scope(CodeGenContext *ctx,
    SymTab_t *symtab, int source_unit_index);
static void codegen_register_record_field_enum_literals(SymTab_t *symtab,
    struct RecordType *record);
static void codegen_register_type_enum_literals(ListNode_t *type_decls, SymTab_t *symtab);
static ListNode_t *g_codegen_available_subprograms = NULL;

static int codegen_runtime_owns_exported_symbol(const char *symbol)
{
    if (symbol == NULL)
        return 0;

    return strcmp(symbol, "FPC_SYSCALL0") == 0 ||
           strcmp(symbol, "FPC_SYSCALL1") == 0 ||
           strcmp(symbol, "FPC_SYSCALL2") == 0 ||
           strcmp(symbol, "FPC_SYSCALL3") == 0 ||
           strcmp(symbol, "FPC_SYSCALL4") == 0 ||
           strcmp(symbol, "FPC_SYSCALL5") == 0 ||
           strcmp(symbol, "FPC_SYSCALL6") == 0;
}

const char *codegen_subprogram_emission_symbol(HashNode_t *cand)
{
    if (cand == NULL)
        return NULL;
    if (cand->type != NULL && cand->type->kind == TYPE_KIND_PROCEDURE &&
        cand->type->info.proc_info.definition != NULL)
    {
        Tree_t *def = cand->type->info.proc_info.definition;
        const char *alias = def->tree_data.subprogram_data.cname_override;
        if (alias != NULL && alias[0] != '\0')
            return alias;
        if (def->tree_data.subprogram_data.mangled_id != NULL &&
            def->tree_data.subprogram_data.mangled_id[0] != '\0')
            return def->tree_data.subprogram_data.mangled_id;
    }
    if (cand->mangled_id != NULL && cand->mangled_id[0] != '\0')
        return cand->mangled_id;
    return NULL;
}

int codegen_has_available_subprogram_label(const char *label)
{
    if (label == NULL || g_codegen_available_subprograms == NULL)
        return 0;
    return codegen_list_contains_string(g_codegen_available_subprograms, label);
}

static int codegen_proc_def_has_matching_impl(SymTab_t *symtab, const Tree_t *proc_def)
{
    if (symtab == NULL || proc_def == NULL || proc_def->type != TREE_SUBPROGRAM)
        return 0;

    const char *proc_id = proc_def->tree_data.subprogram_data.id;
    if (proc_id == NULL || proc_id[0] == '\0')
        return 0;

    ListNode_t *candidates = FindAllIdents(symtab, proc_id);
    for (ListNode_t *node = candidates; node != NULL; node = node->next)
    {
        HashNode_t *cand = (HashNode_t *)node->cur;
        if (cand == NULL || cand->type == NULL ||
            cand->type->kind != TYPE_KIND_PROCEDURE ||
            cand->type->info.proc_info.definition == NULL)
            continue;

        Tree_t *cand_def = cand->type->info.proc_info.definition;
        if (cand_def == proc_def)
            continue;
        if (cand_def->tree_data.subprogram_data.statement_list == NULL)
            continue;
        if (cand->id != NULL && pascal_identifier_equals(cand->id, proc_id))
        {
            if (candidates != NULL)
                DestroyList(candidates);
            return 1;
        }
    }

    if (candidates != NULL)
        DestroyList(candidates);
    return 0;
}

const char *codegen_resolve_function_call_target(CodeGenContext *ctx,
    const struct Expression *expr, char **owned_target_out)
{
    const char *call_target = NULL;
    const char *owner_class_name = NULL;
    const char *method_name = NULL;
    int call_target_needs_resolution = 1;

    if (owned_target_out != NULL)
        *owned_target_out = NULL;
    if (ctx == NULL || expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return NULL;


    call_target = expr->expr_data.function_call_data.mangled_id;
    owner_class_name = expr->expr_data.function_call_data.cached_owner_class;
    method_name = expr->expr_data.function_call_data.cached_method_name;
    call_target_needs_resolution = (call_target == NULL || call_target[0] == '\0');

    if (!call_target_needs_resolution && ctx->symtab != NULL)
    {
        HashNode_t *target_node = NULL;
        if (FindSymbol(&target_node, ctx->symtab, call_target) == 0 || target_node == NULL)
        {
            call_target_needs_resolution = 1;
        }
        else if (target_node->type != NULL &&
                 target_node->type->kind == TYPE_KIND_PROCEDURE &&
                 target_node->type->info.proc_info.definition == NULL)
        {
            call_target_needs_resolution = 1;
        }
        else if (target_node->type != NULL &&
                 target_node->type->kind == TYPE_KIND_PROCEDURE &&
                 target_node->type->info.proc_info.definition != NULL)
        {
            Tree_t *def = target_node->type->info.proc_info.definition;
            int is_external_import =
                (def->tree_data.subprogram_data.statement_list == NULL) &&
                (def->tree_data.subprogram_data.cname_flag != 0 ||
                 def->tree_data.subprogram_data.cname_override != NULL);
            if (is_external_import)
                call_target_needs_resolution = 1;
        }
    }

    if (expr->expr_data.function_call_data.call_kgpc_type != NULL &&
        expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
        expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition != NULL)
    {
        Tree_t *def = expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition;
        const char *alias = def->tree_data.subprogram_data.cname_override;
        if (alias != NULL && alias[0] != '\0')
        {
            call_target = alias;
            call_target_needs_resolution = 0;
        }
        else if (def->tree_data.subprogram_data.statement_list == NULL &&
                 !codegen_proc_def_has_matching_impl(ctx->symtab, def) &&
                 def->tree_data.subprogram_data.id != NULL &&
                 def->tree_data.subprogram_data.id[0] != '\0')
        {
            call_target = def->tree_data.subprogram_data.id;
            call_target_needs_resolution = 0;
        }
        else if (def->tree_data.subprogram_data.statement_list == NULL &&
                 def->tree_data.subprogram_data.cname_flag != 0 &&
                 def->tree_data.subprogram_data.id != NULL &&
                 def->tree_data.subprogram_data.id[0] != '\0')
        {
            call_target = def->tree_data.subprogram_data.id;
            call_target_needs_resolution = 0;
        }
        else if (def->tree_data.subprogram_data.mangled_id != NULL &&
                 def->tree_data.subprogram_data.mangled_id[0] != '\0')
        {
            call_target = def->tree_data.subprogram_data.mangled_id;
            call_target_needs_resolution = 0;
        }
        if (owner_class_name == NULL)
            owner_class_name = def->tree_data.subprogram_data.owner_class;
        if (method_name == NULL)
            method_name = def->tree_data.subprogram_data.method_name;
    }

    if ((call_target == NULL || call_target[0] == '\0' || call_target_needs_resolution) &&
        !expr->expr_data.function_call_data.is_call_info_valid)
    {
        HashNode_t *resolved = expr->expr_data.function_call_data.resolved_func;
        if (resolved != NULL && resolved->mangled_id != NULL &&
            resolved->mangled_id[0] != '\0')
        {
            call_target = resolved->mangled_id;
        }
        else if (resolved != NULL && resolved->type != NULL &&
                 resolved->type->kind == TYPE_KIND_PROCEDURE)
        {
            Tree_t *def = resolved->type->info.proc_info.definition;
            if (def != NULL)
            {
                const char *alias = def->tree_data.subprogram_data.cname_override;
                if (alias != NULL && alias[0] != '\0')
                    call_target = alias;
                else if (def->tree_data.subprogram_data.statement_list == NULL &&
                         !codegen_proc_def_has_matching_impl(ctx->symtab, def) &&
                         def->tree_data.subprogram_data.id != NULL &&
                         def->tree_data.subprogram_data.id[0] != '\0')
                    call_target = def->tree_data.subprogram_data.id;
                else if (def->tree_data.subprogram_data.statement_list == NULL &&
                         def->tree_data.subprogram_data.cname_flag != 0 &&
                         def->tree_data.subprogram_data.id != NULL &&
                         def->tree_data.subprogram_data.id[0] != '\0')
                    call_target = def->tree_data.subprogram_data.id;
                else if (def->tree_data.subprogram_data.mangled_id != NULL &&
                         def->tree_data.subprogram_data.mangled_id[0] != '\0')
                    call_target = def->tree_data.subprogram_data.mangled_id;
            }
        }
    }

    if ((call_target == NULL || call_target[0] == '\0' || call_target_needs_resolution) &&
        ctx->symtab != NULL &&
        expr->expr_data.function_call_data.id != NULL &&
        expr->expr_data.function_call_data.mangled_id != NULL)
    {
        const char *stale_target = expr->expr_data.function_call_data.mangled_id;
        const char *last_sep = strrchr(stale_target, '_');
        size_t prefix_len = (last_sep != NULL) ?
            (size_t)(last_sep - stale_target + 1) : strlen(stale_target);
        ListNode_t *candidates = FindAllIdents(ctx->symtab, expr->expr_data.function_call_data.id);
        for (ListNode_t *node = candidates; node != NULL; node = node->next)
        {
            HashNode_t *cand = (HashNode_t *)node->cur;
            if (cand == NULL || cand->mangled_id == NULL || cand->type == NULL ||
                cand->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            if (strncmp(cand->mangled_id, stale_target, prefix_len) != 0)
                continue;
            Tree_t *def = cand->type->info.proc_info.definition;
            if (def == NULL || def->tree_data.subprogram_data.statement_list == NULL)
                continue;
            call_target = cand->mangled_id;
            break;
        }
        if (candidates != NULL)
            DestroyList(candidates);
    }

    if (ctx->symtab != NULL && owner_class_name != NULL && method_name != NULL)
    {
        const char *impl_target = codegen_find_class_method_impl_id(
            ctx->symtab, NULL, owner_class_name, NULL, method_name);
        if (impl_target != NULL &&
            (call_target == NULL || call_target[0] == '\0' ||
             strcmp(call_target, method_name) == 0 ||
             strcmp(call_target, expr->expr_data.function_call_data.id) == 0))
        {
            call_target = impl_target;
        }
    }

    if ((call_target == NULL || call_target[0] == '\0') &&
        ctx->symtab != NULL &&
        expr->expr_data.function_call_data.id != NULL)
    {
        HashNode_t *sym = NULL;
        if (FindSymbol(&sym, ctx->symtab, expr->expr_data.function_call_data.id) != 0 &&
            sym != NULL)
        {
            if (sym->mangled_id != NULL && sym->mangled_id[0] != '\0')
            {
                call_target = sym->mangled_id;
            }
            else if (sym->type != NULL && sym->type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *def = sym->type->info.proc_info.definition;
                if (def != NULL)
                {
                    const char *alias = def->tree_data.subprogram_data.cname_override;
                    if (alias != NULL && alias[0] != '\0')
                        call_target = alias;
                    else if (def->tree_data.subprogram_data.mangled_id != NULL &&
                             def->tree_data.subprogram_data.mangled_id[0] != '\0')
                        call_target = def->tree_data.subprogram_data.mangled_id;
                }
            }
        }
    }

    if (call_target != NULL && pascal_identifier_equals(call_target, "fpc_in_prefetch_var"))
        call_target = "kgpc_prefetch";

    if ((call_target == NULL || call_target[0] == '\0') &&
        expr->expr_data.function_call_data.call_kgpc_type != NULL &&
        expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE)
    {
        Tree_t *def = expr->expr_data.function_call_data.call_kgpc_type
            ->info.proc_info.definition;
        int is_external = 0;
        if (def != NULL)
        {
            is_external = def->tree_data.subprogram_data.cname_flag != 0 ||
                def->tree_data.subprogram_data.cname_override != NULL;
        }
        if (!is_external && expr->expr_data.function_call_data.id != NULL)
        {
            char *computed_mangled = MangleFunctionName(
                expr->expr_data.function_call_data.id,
                expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.params,
                ctx->symtab);
            if (computed_mangled != NULL && computed_mangled[0] != '\0')
            {
                call_target = computed_mangled;
                if (owned_target_out != NULL)
                    *owned_target_out = computed_mangled;
            }
            else if (computed_mangled != NULL)
            {
                free(computed_mangled);
            }
        }
    }

    if ((call_target == NULL || call_target[0] == '\0') &&
        ctx->symtab != NULL &&
        expr->expr_data.function_call_data.id != NULL)
    {
        int arg_count = ListLength(expr->expr_data.function_call_data.args_expr);
        ListNode_t *candidates = FindAllIdents(ctx->symtab,
            expr->expr_data.function_call_data.id);
        HashNode_t *unique = NULL;
        int matches = 0;
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *node = (HashNode_t *)cur->cur;
            if (node == NULL || node->type == NULL ||
                node->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            if (ListLength(node->type->info.proc_info.params) != arg_count)
                continue;
            unique = node;
            matches++;
            if (matches > 1)
                break;
        }
        if (matches == 1 && unique != NULL)
        {
            if (unique->mangled_id != NULL && unique->mangled_id[0] != '\0')
            {
                call_target = unique->mangled_id;
            }
            else
            {
                char *computed_mangled = MangleFunctionName(
                    unique->id, unique->type->info.proc_info.params, ctx->symtab);
                if (computed_mangled != NULL && computed_mangled[0] != '\0')
                {
                    call_target = computed_mangled;
                    if (owned_target_out != NULL)
                        *owned_target_out = computed_mangled;
                }
                else if (computed_mangled != NULL)
                {
                    free(computed_mangled);
                }
            }
        }
        if (candidates != NULL)
            DestroyList(candidates);
    }

    if (call_target == NULL)
        call_target = expr->expr_data.function_call_data.id;
    return call_target;
}

KgpcType *codegen_resolve_function_call_type(CodeGenContext *ctx,
    const struct Expression *expr, HashNode_t **resolved_node_out)
{
    HashNode_t *resolved_node = NULL;
    const char *resolved_target = NULL;

    if (resolved_node_out != NULL)
        *resolved_node_out = NULL;
    if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
        return NULL;

    if (expr->expr_data.function_call_data.is_call_info_valid)
    {
        if (resolved_node_out != NULL)
            *resolved_node_out = expr->expr_data.function_call_data.resolved_func;
        return expr->expr_data.function_call_data.call_kgpc_type;
    }

    if (expr->expr_data.function_call_data.resolved_func != NULL &&
        expr->expr_data.function_call_data.resolved_func->type != NULL)
    {
        if (resolved_node_out != NULL)
            *resolved_node_out = expr->expr_data.function_call_data.resolved_func;
        return expr->expr_data.function_call_data.resolved_func->type;
    }

    if (ctx != NULL && ctx->symtab != NULL &&
        expr->expr_data.function_call_data.mangled_id != NULL &&
        FindSymbol(&resolved_node, ctx->symtab,
            expr->expr_data.function_call_data.mangled_id) != 0 &&
        resolved_node != NULL && resolved_node->type != NULL)
    {
        if (resolved_node_out != NULL)
            *resolved_node_out = resolved_node;
        return resolved_node->type;
    }

    resolved_target = codegen_resolve_function_call_target(ctx, expr, NULL);
    if (ctx != NULL && ctx->symtab != NULL &&
        resolved_target != NULL &&
        (expr->expr_data.function_call_data.mangled_id == NULL ||
         strcmp(resolved_target, expr->expr_data.function_call_data.mangled_id) != 0) &&
        FindSymbol(&resolved_node, ctx->symtab, resolved_target) != 0 &&
        resolved_node != NULL && resolved_node->type != NULL)
    {
        if (resolved_node_out != NULL)
            *resolved_node_out = resolved_node;
        return resolved_node->type;
    }

    if (ctx != NULL && ctx->symtab != NULL &&
        expr->expr_data.function_call_data.id != NULL &&
        FindSymbol(&resolved_node, ctx->symtab,
            expr->expr_data.function_call_data.id) != 0 &&
        resolved_node != NULL && resolved_node->type != NULL)
    {
        if (resolved_node_out != NULL)
            *resolved_node_out = resolved_node;
        return resolved_node->type;
    }

    return NULL;
}

static int codegen_parse_guid_literal(const char *guid, uint32_t *d1,
    uint16_t *d2, uint16_t *d3, uint8_t d4[8])
{
    if (guid == NULL || d1 == NULL || d2 == NULL || d3 == NULL || d4 == NULL)
        return 0;

    const char *p = guid;
    if (*p == '\'')
        p++;
    if (*p != '{')
        return 0;
    p++;

    unsigned int td1 = 0, td2 = 0, td3 = 0;
    unsigned int td4[8];
    int matched = sscanf(p,
        "%8x-%4x-%4x-%2x%2x-%2x%2x%2x%2x%2x%2x",
        &td1, &td2, &td3,
        &td4[0], &td4[1], &td4[2], &td4[3],
        &td4[4], &td4[5], &td4[6], &td4[7]);
    if (matched != 11)
        return 0;

    *d1 = (uint32_t)td1;
    *d2 = (uint16_t)td2;
    *d3 = (uint16_t)td3;
    for (int i = 0; i < 8; ++i)
        d4[i] = (uint8_t)td4[i];
    return 1;
}

static int codegen_resolve_record_guid(SymTab_t *symtab, const struct RecordType *record,
    uint32_t *d1, uint16_t *d2, uint16_t *d3, uint8_t d4[8])
{
    if (record == NULL || d1 == NULL || d2 == NULL || d3 == NULL || d4 == NULL)
        return 0;

    if (record->has_guid)
    {
        *d1 = record->guid_d1;
        *d2 = record->guid_d2;
        *d3 = record->guid_d3;
        memcpy(d4, record->guid_d4, 8);
        return 1;
    }

    if (record->guid_string == NULL || record->guid_string[0] == '\0')
        return 0;

    if (codegen_parse_guid_literal(record->guid_string, d1, d2, d3, d4))
        return 1;

    if (symtab == NULL)
        return 0;

    ListNode_t *matches = FindAllIdents(symtab, record->guid_string);
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node == NULL || node->hash_type != HASHTYPE_CONST ||
            node->const_string_value == NULL)
            continue;
        if (codegen_parse_guid_literal(node->const_string_value, d1, d2, d3, d4))
        {
            if (matches != NULL)
                DestroyList(matches);
            return 1;
        }
    }
    if (matches != NULL)
        DestroyList(matches);
    return 0;
}

static int codegen_template_matches_methodinfo(const struct MethodTemplate *tmpl,
    const struct MethodInfo *method)
{
    if (tmpl == NULL || method == NULL || tmpl->name == NULL || method->name == NULL)
        return 0;
    if (strcasecmp(method->name, tmpl->name) != 0)
        return 0;

    int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
    if (wanted_params >= 0 && method->param_count >= 0)
        return wanted_params == method->param_count;
    return 1;
}

int codegen_tag_from_kgpc(const KgpcType *type)
{
    if (type == NULL)
        return UNKNOWN_TYPE;
    if (type->kind == TYPE_KIND_PRIMITIVE)
    {
        if (type->info.primitive_type_tag == EXTENDED_TYPE)
            return REAL_TYPE;
        return type->info.primitive_type_tag;
    }
    if (kgpc_type_is_array_of_const(type))
        return ARRAY_OF_CONST_TYPE;
    if (kgpc_type_is_array(type) &&
        type->type_alias != NULL &&
        type->type_alias->is_shortstring)
        return SHORTSTRING_TYPE;
    if (kgpc_type_is_record(type))
        return RECORD_TYPE;
    if (kgpc_type_is_pointer(type))
        return POINTER_TYPE;
    if (kgpc_type_is_procedure(type))
        return PROCEDURE;
    return UNKNOWN_TYPE;
}

#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_LABEL_BUFFER_SIZE 256

/* ---- Simple string hash set for O(1) label/name lookups ---- */
#define CODEGEN_HASHSET_SIZE 211

typedef struct CodeGenHashEntry {
    const char *key;
    struct CodeGenHashEntry *next;
} CodeGenHashEntry;

typedef struct {
    CodeGenHashEntry *buckets[CODEGEN_HASHSET_SIZE];
} CodeGenStringSet;

static CodeGenStringSet g_codegen_callable_exports;

static unsigned codegen_hash(const char *s)
{
    unsigned h = 0;
    for (; *s; s++)
        h = h * 31 + (unsigned char)*s;
    return h % CODEGEN_HASHSET_SIZE;
}

static int codegen_set_contains(const CodeGenStringSet *set, const char *key)
{
    unsigned idx = codegen_hash(key);
    for (CodeGenHashEntry *e = set->buckets[idx]; e != NULL; e = e->next)
        if (strcmp(e->key, key) == 0)
            return 1;
    return 0;
}

static int codegen_set_contains_ci(const CodeGenStringSet *set, const char *key)
{
    unsigned h = 0;
    for (const char *s = key; *s; s++)
        h = h * 31 + (unsigned char)tolower((unsigned char)*s);
    unsigned idx = h % CODEGEN_HASHSET_SIZE;
    for (CodeGenHashEntry *e = set->buckets[idx]; e != NULL; e = e->next)
        if (strcasecmp(e->key, key) == 0)
            return 1;
    return 0;
}

static void codegen_set_insert(CodeGenStringSet *set, const char *key)
{
    unsigned idx = codegen_hash(key);
    CodeGenHashEntry *entry = malloc(sizeof(CodeGenHashEntry));
    entry->key = key;
    entry->next = set->buckets[idx];
    set->buckets[idx] = entry;
}

static void codegen_set_insert_ci(CodeGenStringSet *set, const char *key)
{
    unsigned h = 0;
    for (const char *s = key; *s; s++)
        h = h * 31 + (unsigned char)tolower((unsigned char)*s);
    unsigned idx = h % CODEGEN_HASHSET_SIZE;
    CodeGenHashEntry *entry = malloc(sizeof(CodeGenHashEntry));
    entry->key = key;
    entry->next = set->buckets[idx];
    set->buckets[idx] = entry;
}

static void codegen_set_destroy(CodeGenStringSet *set)
{
    for (int i = 0; i < CODEGEN_HASHSET_SIZE; i++) {
        CodeGenHashEntry *e = set->buckets[i];
        while (e != NULL) {
            CodeGenHashEntry *next = e->next;
            free(e);
            e = next;
        }
        set->buckets[i] = NULL;
    }
}
/* ---- End string hash set ---- */

/* ---- String constant collection for local const strings ---- */
/* String constants from local const declarations (e.g. `const S = '...'`
 * inside function bodies) are registered into the symbol table via
 * PushStringConstOntoScope so the existing .LC label emission in
 * gencode_leaf_var handles them correctly with unique, scope-aware labels.
 * No global collection or separate emission pass is needed. */

static int codegen_list_contains_string(ListNode_t *list, const char *value)
{
    for (ListNode_t *cur = list; cur != NULL; cur = cur->next) {
        if (cur->type == LIST_STRING && cur->cur != NULL &&
            strcmp((const char *)cur->cur, value) == 0)
            return 1;
    }
    return 0;
}

static void codegen_keep_subprogram_label(const char *label)
{
    if (label == NULL)
        return;
    if (g_codegen_available_subprograms != NULL &&
        codegen_list_contains_string(g_codegen_available_subprograms, label))
        return;

    ListNode_t *node = CreateListNode((void *)label, LIST_STRING);
    if (g_codegen_available_subprograms == NULL)
        g_codegen_available_subprograms = node;
    else
        g_codegen_available_subprograms = PushListNodeBack(g_codegen_available_subprograms, node);
}

static void codegen_collect_available_subprogram_labels(ListNode_t *sub_list)
{
    while (sub_list != NULL) {
        Tree_t *sub = (Tree_t *)sub_list->cur;
        if (sub == NULL || sub->type != TREE_SUBPROGRAM) {
            sub_list = sub_list->next;
            continue;
        }

        const char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
        if (sub->tree_data.subprogram_data.statement_list == NULL) {
            sub_list = sub_list->next;
            continue;
        }

        if (mangled_id != NULL) {
            int this_unit = sub->tree_data.subprogram_data.source_unit_index;
            int has_later_override = 0;
            int current_dist = codegen_float_native_distance(sub);
            ListNode_t *later = sub_list->next;
            while (later != NULL) {
                if (later->type == LIST_TREE && later->cur != NULL) {
                    Tree_t *later_sub = (Tree_t *)later->cur;
                    if (later_sub->type == TREE_SUBPROGRAM &&
                        later_sub->tree_data.subprogram_data.statement_list != NULL &&
                        later_sub->tree_data.subprogram_data.mangled_id != NULL &&
                        later_sub->tree_data.subprogram_data.source_unit_index == this_unit &&
                        strcmp(later_sub->tree_data.subprogram_data.mangled_id, mangled_id) == 0) {
                        int later_dist = codegen_float_native_distance(later_sub);
                        if (later_dist <= current_dist) {
                            has_later_override = 1;
                            break;
                        }
                    }
                }
                later = later->next;
            }
            if (has_later_override) {
                sub_list = sub_list->next;
                continue;
            }
        }

        if (!disable_dce_flag() && !sub->tree_data.subprogram_data.is_used) {
            sub_list = sub_list->next;
            continue;
        }

        /* Skip unspecialized generic subprogram templates — only their
         * concrete specializations should be emitted. */
        if (sub->tree_data.subprogram_data.is_generic_template) {
            sub_list = sub_list->next;
            continue;
        }

        if (mangled_id != NULL && !codegen_list_contains_string(g_codegen_available_subprograms, mangled_id)) {
            ListNode_t *node = CreateListNode((void *)mangled_id, LIST_STRING);
            if (g_codegen_available_subprograms == NULL)
                g_codegen_available_subprograms = node;
            else
                g_codegen_available_subprograms = PushListNodeBack(g_codegen_available_subprograms, node);
        }

        if (sub->tree_data.subprogram_data.subprograms != NULL)
            codegen_collect_available_subprogram_labels(sub->tree_data.subprogram_data.subprograms);

        sub_list = sub_list->next;
    }
}

static void codegen_collect_callable_export_names(ListNode_t *sub_list)
{
    while (sub_list != NULL) {
        Tree_t *sub = (Tree_t *)sub_list->cur;
        if (sub == NULL || sub->type != TREE_SUBPROGRAM) {
            sub_list = sub_list->next;
            continue;
        }

        struct Subprogram *data = &sub->tree_data.subprogram_data;
        const char *export_name = NULL;
        if (data->internproc_id != NULL && data->internproc_id[0] != '\0')
            export_name = data->internproc_id;
        else if (data->cname_override != NULL && data->cname_override[0] != '\0')
            export_name = data->cname_override;
        else if (data->mangled_id != NULL && data->mangled_id[0] != '\0')
            export_name = data->mangled_id;

        if (export_name != NULL && !codegen_set_contains_ci(&g_codegen_callable_exports, export_name))
            codegen_set_insert_ci(&g_codegen_callable_exports, export_name);

        if (data->subprograms != NULL)
            codegen_collect_callable_export_names(data->subprograms);

        sub_list = sub_list->next;
    }
}

static int codegen_self_param_is_class(Tree_t *arg_decl, SymTab_t *symtab)
{
    if (arg_decl == NULL || arg_decl->type != TREE_VAR_DECL)
        return 0;

    KgpcType *type = arg_decl->tree_data.var_decl_data.cached_kgpc_type;
    const char *type_id = arg_decl->tree_data.var_decl_data.type_id;
    if (type == NULL && symtab != NULL && type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, type_id) != 0 &&
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

typedef struct RecordParamWork {
    const char *id;
    int size;
    int stack_arg_offset;
    int has_stack_arg;
    const char *arg_reg;
    int is_dynarray;
    int dynarray_elem_size;
    int dynarray_lower_bound;
    int arg_index;
} RecordParamWork;

/* Escape a string for use in assembly .string directive */
void escape_string(char *dest, const char *src, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size == 0)
        return;
    
    size_t i = 0, j = 0;
    while (src[i] != '\0' && j < dest_size - 1)
    {
        switch (src[i])
        {
            case '"':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = '"';
                }
                break;
            case '\\':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = '\\';
                }
                break;
            case '\n':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = 'n';
                }
                break;
            case '\t':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = 't';
                }
                break;
            case '\r':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = 'r';
                }
                break;
            default:
                dest[j++] = src[i];
                break;
        }
        i++;
    }
    dest[j] = '\0';
}

/* Helper functions for transitioning from legacy type fields to KgpcType */
static int codegen_dynamic_array_element_size_from_type(CodeGenContext *ctx, KgpcType *array_type);
static int codegen_dynamic_array_descriptor_bytes(int element_size);
static void add_alias_for_return_var(StackNode_t *return_var, const char *alias_label);
static int add_absolute_var_alias(const char *alias_label, const char *target_label);
static int add_absolute_var_alias_with_offset(const char *alias_label, const char *target_label,
    int field_offset, int alias_size);
static int add_absolute_static_symbol_alias(const char *alias_label, const char *target_symbol,
    int alias_size);
static void add_result_alias_for_return_var(StackNode_t *return_var);
static ListNode_t *codegen_store_class_typeinfo(ListNode_t *inst_list,
    CodeGenContext *ctx, StackNode_t *var_node, const char *type_name);
static ListNode_t *codegen_emit_tfile_configure(ListNode_t *inst_list,
    StackNode_t *file_node, long long element_size, int element_hash_tag);
static int codegen_resolve_file_component(const struct TypeAlias *alias, SymTab_t *symtab,
    long long *element_size_out, int *element_hash_tag_out);
static char *codegen_make_program_var_label(CodeGenContext *ctx, const char *name);
static void codegen_emit_bss_or_comm(FILE *out, const char *sym, const char *label,
                                     int size, int alignment, int defined_in_unit);

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Helper function to check if a node is a file type */
static inline int node_is_file_type(HashNode_t *node)
{
    if (node == NULL || node->type == NULL)
        return 0;
    return kgpc_type_equals_tag(node->type, FILE_TYPE) ||
        kgpc_type_equals_tag(node->type, TEXT_TYPE);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

/*
 * Compare two HashNode_t type entries to determine which is the better
 * candidate for type resolution.
 *
 * Returns < 0 if `a` is better, > 0 if `b` is better, 0 if equivalent.
 *
 * Criteria in strict priority order:
 *   1. Prefer entries from a named unit (source_unit_index > 0)
 *   2. Among unit entries, prefer higher unit index (later = closer scope)
 *   3. Prefer public declarations
 *   4. Prefer unit-defined types
 */
static int codegen_compare_type_nodes(const HashNode_t *a, const HashNode_t *b)
{
    assert(a != NULL && b != NULL);

    int a_has_unit = (a->source_unit_index > 0);
    int b_has_unit = (b->source_unit_index > 0);
    if (a_has_unit != b_has_unit)
        return a_has_unit ? -1 : 1;

    if (a->source_unit_index != b->source_unit_index)
        return (a->source_unit_index > b->source_unit_index) ? -1 : 1;

    if (a->unit_is_public != b->unit_is_public)
        return a->unit_is_public ? -1 : 1;

    if (a->defined_in_unit != b->defined_in_unit)
        return a->defined_in_unit ? -1 : 1;

    return 0;
}

static HashNode_t *codegen_pick_type_node_by_name(SymTab_t *symtab, const char *type_name)
{
    if (symtab == NULL || type_name == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_name);
    if (matches == NULL)
        return NULL;

    HashNode_t *best_node = NULL;
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        if (cand == NULL || cand->hash_type != HASHTYPE_TYPE)
            continue;
        if (best_node == NULL || codegen_compare_type_nodes(cand, best_node) < 0)
            best_node = cand;
    }

    DestroyList(matches);
    return best_node;
}

static struct RecordType *codegen_lookup_record_type_for_node(SymTab_t *symtab,
    HashNode_t *node, const char *type_name)
{
    if (node == NULL)
        return NULL;

    if (node->source_unit_index > 0 && type_name != NULL)
    {
        const char *unit_name = unit_registry_get(node->source_unit_index);
        if (unit_name != NULL)
        {
            size_t qualified_len = strlen(unit_name) + 1 + strlen(type_name) + 1;
            char *qualified_id = (char *)malloc(qualified_len);
            if (qualified_id != NULL)
            {
                snprintf(qualified_id, qualified_len, "%s.%s", unit_name, type_name);
                HashNode_t *qualified = NULL;
                if (FindSymbol(&qualified, symtab, qualified_id) != 0 && qualified != NULL)
                {
                    struct RecordType *qualified_record = get_record_type_from_node(qualified);
                    if (qualified_record != NULL)
                    {
                        free(qualified_id);
                        return qualified_record;
                    }
                }
                free(qualified_id);
            }
        }
    }

    return get_record_type_from_node(node);
}

static struct RecordType *codegen_lookup_record_type_by_name(SymTab_t *symtab,
    const char *type_name, int prefer_guid)
{
    if (symtab == NULL || type_name == NULL)
        return NULL;

    struct RecordType *record = semcheck_lookup_record_type(symtab, type_name);
    if (record != NULL)
    {
        if (!prefer_guid)
            return record;
        uint32_t guid_d1 = 0;
        uint16_t guid_d2 = 0;
        uint16_t guid_d3 = 0;
        uint8_t guid_d4[8] = {0};
        if (codegen_resolve_record_guid(symtab, record,
                &guid_d1, &guid_d2, &guid_d3, guid_d4))
            return record;
    }

    int n_units = unit_registry_count();
    for (int i = 1; i <= n_units && i < SYMTAB_MAX_UNITS; ++i)
    {
        ScopeNode *unit_scope = symtab->unit_scopes[i];
        if (unit_scope == NULL || unit_scope->table == NULL)
            continue;

        HashNode_t *node = FindIdentInTable(unit_scope->table, type_name);
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            return codegen_lookup_record_type_for_node(symtab, node, type_name);
        }

        node = FindTypeBySuffixInTable(unit_scope->table, type_name);
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            KGPC_COMPILER_HARD_ASSERT(0,
                "suffix-only record type lookup for '%s' reached codegen",
                type_name);
        }
    }

    HashNode_t *best_node = codegen_pick_type_node_by_name(symtab, type_name);
    if (best_node != NULL)
        return codegen_lookup_record_type_for_node(symtab, best_node, type_name);

    return NULL;
}

typedef struct EmittedClassSet
{
    const char **labels;
    int count;
    int capacity;
} EmittedClassSet;

static int emitted_class_set_contains(const EmittedClassSet *set, const char *label)
{
    if (set == NULL || label == NULL)
        return 0;

    for (int i = 0; i < set->count; ++i)
    {
        if (set->labels[i] != NULL && strcmp(set->labels[i], label) == 0)
            return 1;
    }

    return 0;
}

static int emitted_class_set_add(EmittedClassSet *set, const char *label)
{
    if (set == NULL || label == NULL)
        return 1;

    if (emitted_class_set_contains(set, label))
        return 0;

    if (set->count == set->capacity)
    {
        int new_capacity = (set->capacity > 0) ? set->capacity * 2 : 64;
        const char **new_labels = (const char **)realloc(set->labels,
            sizeof(const char *) * (size_t)new_capacity);
        if (new_labels == NULL)
            return 1;
        set->labels = new_labels;
        set->capacity = new_capacity;
    }

    set->labels[set->count++] = label;
    return 0;
}

static void emitted_class_set_destroy(EmittedClassSet *set)
{
    if (set == NULL)
        return;

    free((void *)set->labels);
    set->labels = NULL;
    set->count = 0;
    set->capacity = 0;
}

static int codegen_type_decl_suppressed(const Tree_t *decl)
{
    return (decl != NULL &&
        decl->type == TREE_TYPE_DECL &&
        decl->tree_data.type_decl_data.suppress_codegen);
}

static struct RecordType *codegen_record_from_type_decl_ex(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL || decl->type != TREE_TYPE_DECL)
        return NULL;

    KgpcType *kgpc = decl->tree_data.type_decl_data.kgpc_type;
    if (kgpc != NULL)
    {
        if (kgpc->kind == TYPE_KIND_RECORD && kgpc->info.record_info != NULL)
            return kgpc->info.record_info;
        if (kgpc->kind == TYPE_KIND_POINTER &&
            kgpc->info.points_to != NULL &&
            kgpc->info.points_to->kind == TYPE_KIND_RECORD &&
            kgpc->info.points_to->info.record_info != NULL)
            return kgpc->info.points_to->info.record_info;
    }

    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
    {
        struct RecordType *fallback = decl->tree_data.type_decl_data.info.record;
        /* When a forward-declared class is completed, the full declaration's
         * RecordType becomes a depleted shell (fields transferred to the
         * canonical RecordType in the symtab).  Detect this and look up
         * the canonical record instead. */
        if (fallback != NULL && fallback->is_class &&
            fallback->fields == NULL && fallback->parent_fields_merged &&
            symtab != NULL && decl->tree_data.type_decl_data.id != NULL)
        {
            HashNode_t *canon_node = NULL;
            if (FindSymbol(&canon_node, symtab, decl->tree_data.type_decl_data.id) &&
                canon_node != NULL && canon_node->type != NULL)
            {
                /* Extract record from symtab node, handling both direct
                 * record types and pointer-to-record (class types). */
                struct RecordType *canon = NULL;
                KgpcType *ct = canon_node->type;
                if (ct->kind == TYPE_KIND_RECORD && ct->info.record_info != NULL)
                    canon = ct->info.record_info;
                else if (ct->kind == TYPE_KIND_POINTER &&
                         ct->info.points_to != NULL &&
                         ct->info.points_to->kind == TYPE_KIND_RECORD &&
                         ct->info.points_to->info.record_info != NULL)
                    canon = ct->info.points_to->info.record_info;
                if (canon != NULL)
                    return canon;
            }
        }
        return fallback;
    }

    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        return decl->tree_data.type_decl_data.info.alias.inline_record_type;

    return NULL;
}

static struct RecordType *codegen_record_from_type_decl(Tree_t *decl)
{
    return codegen_record_from_type_decl_ex(decl, NULL);
}

/* Get field offset within a record by field name.
 * Returns -1 if field not found. */
static int record_type_get_field_offset(SymTab_t *symtab, struct RecordType *record,
    const char *field_name)
{
    if (record == NULL || field_name == NULL)
        return -1;

    struct RecordField *field_desc = NULL;
    long long offset = 0;
    if (resolve_record_field(symtab, record, field_name, &field_desc, &offset, 0, 1) != 0 ||
        field_desc == NULL)
        return -1;

    if (offset < 0 || offset > INT_MAX)
        return -1;

    return (int)offset;
}

static inline int node_is_class_type(HashNode_t *node)
{
    if (node == NULL)
        return 0;
    if (!node_is_record_type(node))
        return 0;
    struct RecordType *record = get_record_type_from_node(node);
    return record_type_is_class(record);
}

static int record_has_class_vars(const struct RecordType *record)
{
    if (record == NULL || record->fields == NULL)
        return 0;
    ListNode_t *field_node = record->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field != NULL && field->is_class_var == 1)
                return 1;
        }
        field_node = field_node->next;
    }
    return 0;
}

static int record_has_class_method_templates(const struct RecordType *record)
{
    if (record == NULL || record->method_templates == NULL)
        return 0;
    ListNode_t *node = record->method_templates;
    while (node != NULL)
    {
        if (node->type == LIST_METHOD_TEMPLATE && node->cur != NULL)
        {
            struct MethodTemplate *templ = (struct MethodTemplate *)node->cur;
            if (templ->is_class_method || templ->is_static)
                return 1;
        }
        node = node->next;
    }
    return 0;
}

static int record_has_method_decls(const struct RecordType *record)
{
    if (record == NULL || record->fields == NULL)
        return 0;
    ListNode_t *node = record->fields;
    while (node != NULL)
    {
        if (node->type == LIST_UNSPECIFIED && node->cur != NULL)
            return 1;
        node = node->next;
    }
    return 0;
}

static int codegen_class_var_field_size(SymTab_t *symtab, const struct RecordField *field)
{
    if (field == NULL)
        return DOUBLEWORD;

    int field_size = DOUBLEWORD;

    if (field->type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, field->type_id) != 0 && type_node != NULL &&
            type_node->type != NULL)
        {
            long long type_size = kgpc_type_sizeof(type_node->type);
            if (type_size > 0 && type_size <= INT_MAX)
                field_size = (int)type_size;
            return field_size;
        }
    }

    if (field->is_array)
    {
        int elem_size = 0;
        if (field->array_element_type_id != NULL)
        {
            HashNode_t *elem_node = NULL;
            if (FindSymbol(&elem_node, symtab, field->array_element_type_id) != 0 &&
                elem_node != NULL && elem_node->type != NULL)
            {
                long long type_size = kgpc_type_sizeof(elem_node->type);
                if (type_size > 0 && type_size <= INT_MAX)
                    elem_size = (int)type_size;
            }
        }
        if (elem_size == 0)
        {
            switch (field->array_element_type)
            {
                case CHAR_TYPE:
                case BOOL:
                case BYTE_TYPE:
                    elem_size = 1;
                    break;
                case WORD_TYPE:
                    elem_size = 2;
                    break;
                case LONGINT_TYPE:
                case LONGWORD_TYPE:
                case INT_TYPE:
                    elem_size = 4;
                    break;
                case INT64_TYPE:
                case QWORD_TYPE:
                case REAL_TYPE:
                case STRING_TYPE:
                case POINTER_TYPE:
                    elem_size = 8;
                    break;
                case EXTENDED_TYPE:
                    elem_size = 10;
                    break;
                default:
                    elem_size = DOUBLEWORD;
                    break;
            }
        }
        long long count = (long long)field->array_end - (long long)field->array_start + 1;
        if (count < 0)
            count = 0;
        long long total = count * elem_size;
        if (total > 0 && total <= INT_MAX)
            field_size = (int)total;
        return field_size;
    }

    switch (field->type)
    {
        case INT64_TYPE:
        case REAL_TYPE:
        case STRING_TYPE:
        case POINTER_TYPE:
        case QWORD_TYPE:
            field_size = 8;
            break;
        case EXTENDED_TYPE:
            field_size = 10;
            break;
        case CHAR_TYPE:
        case BOOL:
        case BYTE_TYPE:
            field_size = 1;
            break;
        case WORD_TYPE:
            field_size = 2;
            break;
        case LONGINT_TYPE:
        case LONGWORD_TYPE:
        case INT_TYPE:
            field_size = DOUBLEWORD;
            break;
        default:
            field_size = DOUBLEWORD;
            break;
    }

    return field_size;
}

static int codegen_record_field_alignment(const struct RecordField *field, int field_size)
{
    if (field != NULL)
    {
        if (field->type == EXTENDED_TYPE)
            return 16;
        if (field->type_id != NULL && pascal_identifier_equals(field->type_id, "Extended"))
            return 16;
    }
    return (field_size > 8) ? 16 : ((field_size >= 8) ? 8 : ((field_size >= 4) ? 4 : 1));
}

static long long codegen_class_var_storage_size(SymTab_t *symtab, const struct RecordType *record_info,
    int include_all_fields)
{
    if (record_info == NULL || record_info->fields == NULL)
        return 0;

    long long current_offset = 0;
    ListNode_t *field_node = record_info->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field != NULL && (include_all_fields || field->is_class_var == 1))
            {
                int field_size = codegen_class_var_field_size(symtab, field);
                int alignment = codegen_record_field_alignment(field, field_size);
                current_offset = (current_offset + alignment - 1) & ~(alignment - 1);
                current_offset += field_size;
            }
        }
        field_node = field_node->next;
    }
    return current_offset;
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

static const char *codegen_resolve_record_type_name(HashNode_t *node, SymTab_t *symtab)
{
    if (node == NULL)
        return NULL;
    if (node_is_record_type(node) && node->id != NULL)
        return node->id;
    struct TypeAlias *alias = get_type_alias_from_node(node);
    if (alias != NULL && alias->target_type_id != NULL && symtab != NULL)
    {
        HashNode_t *target = NULL;
        if (FindSymbol(&target, symtab, alias->target_type_id) != 0 && target != NULL)
            return codegen_resolve_record_type_name(target, symtab);
    }
    return NULL;
}

/**
 * For static class methods, register the class variables with the stack manager
 * so they can be found during code generation via find_label_with_depth.
 * 
 * This function extracts the class name from the mangled method name (ClassName__MethodName),
 * looks up the class type in the symbol table, and registers each class var field
 * with the stack manager using add_static_var.
 */
static void codegen_add_class_vars_for_static_method(const char *owner_class,
    const char *method_name_arg, SymTab_t *symtab, CodeGenContext *ctx)
{
    if (owner_class == NULL || symtab == NULL)
        return;

    if (method_name_arg == NULL)
        return;

    char *class_name = strdup(owner_class);
    if (class_name == NULL)
        return;

    int is_static_check = from_cparser_is_method_static(class_name, method_name_arg);
    if (!is_static_check)
    {
        free(class_name);
        return;
    }
    
    /* Look up the class type */
    HashNode_t *class_node = NULL;
    if (!FindSymbol(&class_node, symtab, class_name) || class_node == NULL)
    {
        free(class_name);
        return;
    }
    
    /* Get the record type - for classes, the type is a pointer to the record */
    struct RecordType *record_info = get_record_type_from_node(class_node);
    if (record_info == NULL)
    {
        /* Try to dereference if it's a pointer type (class types are pointers to records) */
        if (class_node->type != NULL && class_node->type->kind == TYPE_KIND_POINTER)
        {
            KgpcType *pointed_to = class_node->type->info.points_to;
            if (pointed_to != NULL && pointed_to->kind == TYPE_KIND_RECORD)
            {
                record_info = pointed_to->info.record_info;
            }
        }
    }
    
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }
    int has_class_vars = record_has_class_vars(record_info);
    int include_all_fields = 0;
    if (!has_class_vars)
        include_all_fields = 1;
    if (record_info->is_type_helper)
    {
        free(class_name);
        return;
    }
    
    /* Use the original class name from the type definition to match the CLASSVAR label.
     * The mangled name may have different casing, but the CLASSVAR label uses the original type_id. */
    const char *original_class_name = (record_info != NULL && record_info->type_id != NULL) ?
        record_info->type_id : class_name;
    
    /* Build the class var storage label */
    size_t label_len = strlen(original_class_name) + strlen("_CLASSVAR") + 1;
    char *classvar_label = (char *)malloc(label_len);
    if (classvar_label == NULL)
    {
        free(class_name);
        return;
    }
    snprintf(classvar_label, label_len, "%s_CLASSVAR", original_class_name);
    
    /* Iterate over fields and register each as a static var with proper offset */
    ListNode_t *field_node = record_info->fields;
    long long current_offset = 0;
    
    while (field_node != NULL)
    {
        if (field_node->type != LIST_RECORD_FIELD)
        {
            field_node = field_node->next;
            continue;
        }
        struct RecordField *field = (struct RecordField *)field_node->cur;
        if (field != NULL && field->name != NULL && field->name[0] != '\0')
        {
            if (!include_all_fields && field->is_class_var != 1)
            {
                field_node = field_node->next;
                continue;
            }

            /* Calculate field size */
            int field_size = codegen_class_var_field_size(symtab, field);
            
            /* Build the static label for this field: ClassName_CLASSVAR+offset */
            /* We register it with offset information */
            /* Buffer size: classvar_label + "+" + max_digits(long long) + null terminator */
            /* max_digits for long long is 20, so 32 provides ample margin */
            size_t field_label_len = strlen(classvar_label) + 32;
            char *field_static_label = (char *)malloc(field_label_len);
            if (field_static_label != NULL)
            {
                int written;
                if (current_offset == 0)
                    written = snprintf(field_static_label, field_label_len, "%s", classvar_label);
                else
                    written = snprintf(field_static_label, field_label_len, "%s+%lld", classvar_label, current_offset);
                
                /* Verify buffer was large enough */
                assert(written >= 0 && (size_t)written < field_label_len);
                
                /* Register class vars under a class-qualified key to avoid cross-class collisions. */
                size_t key_len = strlen(class_name) + 2 + strlen(field->name) + 1;
                char *classvar_key = (char *)malloc(key_len);
                if (classvar_key != NULL)
                {
                    snprintf(classvar_key, key_len, "%s::%s", class_name, field->name);
                    StackScope_t *cur_scope = get_cur_scope();
                    StackNode_t *existing = NULL;
                    if (cur_scope != NULL)
                        existing = stackscope_find_x(cur_scope, classvar_key);
                    if (existing == NULL)
                        add_static_var(classvar_key, field_size, field_static_label);

                    if (cur_scope != NULL &&
                        stackscope_find_z(cur_scope, field->name) == NULL &&
                        stackscope_find_x(cur_scope, field->name) == NULL &&
                        stackscope_find_t(cur_scope, field->name) == NULL)
                    {
                        add_absolute_var_alias(field->name, classvar_key);
                    }
                    free(classvar_key);
                }
                free(field_static_label);
            }
            
            /* Advance offset with alignment (using standard power-of-two alignment formula) */
            int alignment = codegen_record_field_alignment(field, field_size);
            current_offset = (current_offset + alignment - 1) & ~(alignment - 1);
            current_offset += field_size;
        }
        field_node = field_node->next;
    }
    
    free(classvar_label);
    free(class_name);
}

/* Allocate the next available integer argument register (general purpose). */
static const char *alloc_integer_arg_reg(int use_64bit, int *next_index)
{
    if (next_index == NULL)
        return NULL;

    const char *reg = use_64bit ?
        get_arg_reg64_num(*next_index) :
        get_arg_reg32_num(*next_index);
    if (reg == NULL)
        return NULL;

    ++(*next_index);
    return reg;
}

/* Allocate the next available SSE argument register for REAL parameters. */
static const char *alloc_sse_arg_reg(int *next_index)
{
    if (next_index == NULL)
        return NULL;

    const char *reg = current_arg_reg_xmm(*next_index);
    if (reg == NULL)
    {
        fprintf(stderr,
            "ERROR: Max SSE argument register limit exceeded (index=%d)\n",
            *next_index);
        exit(1);
    }

    ++(*next_index);
    return reg;
}

static int codegen_real_param_storage_size(Tree_t *arg_decl,
    HashNode_t *resolved_type_node, KgpcType *cached_arg_type)
{
    if (arg_decl != NULL && arg_decl->type == TREE_VAR_DECL)
    {
        if (arg_decl->tree_data.var_decl_data.type == EXTENDED_TYPE)
            return 16;
        if (arg_decl->tree_data.var_decl_data.type_id != NULL &&
            pascal_identifier_equals(arg_decl->tree_data.var_decl_data.type_id, "Extended"))
            return 16;
        struct TypeAlias *alias = arg_decl->tree_data.var_decl_data.inline_type_alias;
        if (alias != NULL && alias->storage_size > 0)
            return (int)alias->storage_size;
    }

    if (resolved_type_node != NULL && resolved_type_node->type != NULL)
    {
        if (kgpc_type_is_extended(resolved_type_node->type))
            return 16;
        if (kgpc_type_is_real(resolved_type_node->type))
        {
            long long real_size = kgpc_type_real_storage_size(resolved_type_node->type);
            if (real_size > 8)
                return 16;
            if (real_size > 0)
                return (int)real_size;
        }
        long long size = kgpc_type_sizeof(resolved_type_node->type);
        if (size > 0)
            return (int)size;
    }

    if (cached_arg_type != NULL)
    {
        if (kgpc_type_is_extended(cached_arg_type))
            return 16;
        if (kgpc_type_is_real(cached_arg_type))
        {
            long long real_size = kgpc_type_real_storage_size(cached_arg_type);
            if (real_size > 8)
                return 16;
            if (real_size > 0)
                return (int)real_size;
        }
        long long size = kgpc_type_sizeof(cached_arg_type);
        if (size > 0)
            return (int)size;
    }

    return 8;
}

/* Helper function to determine variable storage size (for stack allocation)
 * Returns size in bytes, or -1 on error */
static inline int get_var_storage_size(HashNode_t *node)
{
    if (node == NULL)
        return -1;
    
    /* Check KgpcType first */
    if (node->type != NULL)
    {
        if (node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            /* Honor explicit storage overrides from type aliases (e.g., Int64/QWord) */
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias != NULL)
            {
                if (alias->is_shortstring)
                    return 256;
                if (alias->storage_size > 0)
                    return (int)alias->storage_size;
            }

            int tag = kgpc_type_get_primitive_tag(node->type);
            switch (tag)
            {
                case LONGINT_TYPE:
                    return 4;  // Match FPC's 32-bit LongInt
                case INT64_TYPE:
                    return 8;
                case REAL_TYPE:
                {
                    long long size = kgpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return 8;
                }
                case STRING_TYPE:  /* PCHAR */
                case POINTER_TYPE:
                case PROCEDURE:
                    return 8;
                case SHORTSTRING_TYPE:
                    return 256;
                case FILE_TYPE:
                case TEXT_TYPE:
                {
                    long long size = kgpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return 8;
                }
                case SET_TYPE:
                {
                    /* Check if this is a character set */
                    long long size = kgpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return DOUBLEWORD;  /* Default for non-char sets */
                }
                case CHAR_TYPE:
                    return 1;
                default:
                    return DOUBLEWORD;  /* 4 bytes for most primitives */
            }
        }
        else if (node->type->kind == TYPE_KIND_POINTER)
        {
            return 8;
        }
        else if (node->type->kind == TYPE_KIND_PROCEDURE)
        {
            return 8;  /* Function pointers are 8 bytes */
        }
        else if (node->type->kind == TYPE_KIND_RECORD || node->type->kind == TYPE_KIND_ARRAY)
        {
            /* For classes, allocate only pointer size since instances are heap-allocated */
            if (node->type->kind == TYPE_KIND_RECORD && 
                node->type->info.record_info != NULL &&
                record_type_is_class(node->type->info.record_info))
            {
                return 8;  /* Class variables are pointers */
            }
            
            long long size = kgpc_type_sizeof(node->type);
            if (size > 0)
                return (int)size;
            return -1;
        }
    }
    return DOUBLEWORD;
}

ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
void codegen_vmt(CodeGenContext *ctx, SymTab_t *symtab, Tree_t *tree,
                 CompilationContext *comp_ctx);

kgpc_target_abi_t g_current_codegen_abi = KGPC_TARGET_ABI_SYSTEM_V;
int g_stack_home_space_bytes = 0;

static int align_to_multiple(int value, int alignment)
{
    if (alignment <= 0)
        return value;

    int remainder = value % alignment;
    if (remainder == 0)
        return value;

    return value + (alignment - remainder);
}

typedef struct StaticLinkInfo
{
    const char *mangled_name;
    int lexical_depth;
} StaticLinkInfo;

/* Helper: Increment lexical nesting depth when entering a procedure/function */
void codegen_enter_lexical_scope(CodeGenContext *ctx)
{
    if (ctx != NULL)
        ctx->lexical_depth++;
}

/* Helper: Decrement lexical nesting depth when leaving a procedure/function */
void codegen_leave_lexical_scope(CodeGenContext *ctx)
{
    if (ctx != NULL && ctx->lexical_depth > 0)
        ctx->lexical_depth--;
}

/* Helper: Get current lexical nesting depth */
int codegen_get_lexical_depth(const CodeGenContext *ctx)
{
    return (ctx != NULL) ? ctx->current_subprogram_lexical_depth : 0;
}

/* Helper: Check if we're currently in a nested context (depth > 0) */
int codegen_is_nested_context(const CodeGenContext *ctx)
{
    return codegen_get_lexical_depth(ctx) > 0;
}

void codegen_register_static_link_proc(CodeGenContext *ctx, const char *mangled_name, int lexical_depth)
{
    if (ctx == NULL || mangled_name == NULL)
        return;

    if (codegen_proc_requires_static_link(ctx, mangled_name))
        return;

    StaticLinkInfo *info = (StaticLinkInfo *)malloc(sizeof(StaticLinkInfo));
    if (info == NULL)
        return;

    info->mangled_name = mangled_name;
    info->lexical_depth = lexical_depth;
    ListNode_t *entry = CreateListNode(info, LIST_UNSPECIFIED);
    if (ctx->static_link_procs == NULL)
        ctx->static_link_procs = entry;
    else
        ctx->static_link_procs = PushListNodeFront(ctx->static_link_procs, entry);
}

int codegen_proc_requires_static_link(const CodeGenContext *ctx, const char *mangled_name)
{
    if (ctx == NULL || mangled_name == NULL)
        return 0;

    ListNode_t *node = ctx->static_link_procs;
    while (node != NULL)
    {
        StaticLinkInfo *info = (StaticLinkInfo *)node->cur;
        if (info != NULL && info->mangled_name != NULL &&
            strcmp(info->mangled_name, mangled_name) == 0)
            return 1;
        node = node->next;
    }

    return 0;
}

int codegen_proc_static_link_depth(const CodeGenContext *ctx, const char *mangled_name, int *out_depth)
{
    if (ctx == NULL || mangled_name == NULL || out_depth == NULL)
        return 0;

    ListNode_t *node = ctx->static_link_procs;
    while (node != NULL)
    {
        StaticLinkInfo *info = (StaticLinkInfo *)node->cur;
        if (info != NULL && info->mangled_name != NULL &&
            strcmp(info->mangled_name, mangled_name) == 0)
        {
            *out_depth = info->lexical_depth;
            return 1;
        }
        node = node->next;
    }

    return 0;
}

void codegen_reset_static_link_cache(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;

    if (ctx->static_link_reg != NULL)
    {
        free_reg(get_reg_stack(), ctx->static_link_reg);
        ctx->static_link_reg = NULL;
    }
    ctx->static_link_reg_level = 0;
    ctx->static_link_spill_slot = NULL;
}

static void codegen_register_local_types(ListNode_t *type_decls, SymTab_t *symtab)
{
    if (type_decls == NULL || symtab == NULL)
        return;

    /* First pass: register record/class types so pointer aliases can resolve. */
    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type != TREE_TYPE_DECL ||
            decl->tree_data.type_decl_data.id == NULL)
            continue;
        if (codegen_type_decl_suppressed(decl))
            continue;
        if (decl->tree_data.type_decl_data.kind != TYPE_DECL_RECORD)
            continue;

        KgpcType *kgpc = decl->tree_data.type_decl_data.kgpc_type;
        int created_kgpc = 0;
        if (kgpc == NULL && decl->tree_data.type_decl_data.info.record != NULL)
        {
            kgpc = create_record_type(decl->tree_data.type_decl_data.info.record);
            if (decl->tree_data.type_decl_data.info.record->is_class && kgpc != NULL)
                kgpc = create_pointer_type(kgpc);
            created_kgpc = 1;
        }

        if (kgpc != NULL)
        {
            PushTypeOntoScope_Typed(symtab, strdup(decl->tree_data.type_decl_data.id), kgpc);
            if (decl->tree_data.type_decl_data.info.record != NULL)
            {
                codegen_register_record_field_enum_literals(symtab,
                    decl->tree_data.type_decl_data.info.record);
            }
            if (created_kgpc)
                destroy_kgpc_type(kgpc);
        }
    }

    /* Second pass: register aliases and resolve pointer targets now that records exist. */
    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type != TREE_TYPE_DECL ||
            decl->tree_data.type_decl_data.id == NULL)
            continue;
        if (codegen_type_decl_suppressed(decl))
            continue;
        if (decl->tree_data.type_decl_data.kind != TYPE_DECL_ALIAS)
            continue;

        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
        KgpcType *kgpc = decl->tree_data.type_decl_data.kgpc_type;
        int created_kgpc = 0;
        if (kgpc == NULL)
        {
            kgpc = create_kgpc_type_from_type_alias(alias, symtab,
                decl->tree_data.type_decl_data.defined_in_unit);
            created_kgpc = 1;
        }

        if (kgpc != NULL && kgpc_type_is_pointer(kgpc) &&
            kgpc->info.points_to == NULL)
        {
            const char *pointee_id = alias->pointer_type_id;
            if (pointee_id == NULL)
                pointee_id = alias->target_type_id;
            if (pointee_id != NULL)
            {
                HashNode_t *pointee_node = NULL;
                if (FindSymbol(&pointee_node, symtab, pointee_id) != 0 &&
                    pointee_node != NULL && pointee_node->type != NULL)
                {
                    kgpc_type_retain(pointee_node->type);
                    kgpc->info.points_to = pointee_node->type;
                }
            }
        }

        if (kgpc != NULL)
        {
            PushTypeOntoScope_Typed(symtab, strdup(decl->tree_data.type_decl_data.id), kgpc);
            if (alias->is_enum && alias->enum_literals != NULL)
            {
                int ordinal = 0;
                for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
                {
                    const char *literal_name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (literal_name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, literal_name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, strdup(literal_name), ordinal, kgpc);
                }
            }
            if (created_kgpc)
                destroy_kgpc_type(kgpc);
        }
    }
}

static void codegen_register_record_field_enum_literals(SymTab_t *symtab,
    struct RecordType *record)
{
    if (symtab == NULL || record == NULL)
        return;

    for (ListNode_t *field_node = record->fields; field_node != NULL; field_node = field_node->next)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                for (ListNode_t *lit = field->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
                {
                    const char *name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)name, ordinal, enum_type);
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (field_node->type == LIST_VARIANT_PART && field_node->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)field_node->cur;
            if (variant->tag_field != NULL && variant->tag_field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                for (ListNode_t *lit = variant->tag_field->enum_literals; lit != NULL;
                     lit = lit->next, ++ordinal)
                {
                    const char *name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)name, ordinal, enum_type);
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }

            for (ListNode_t *branch_node = variant->branches; branch_node != NULL;
                 branch_node = branch_node->next)
            {
                if (branch_node->type == LIST_VARIANT_BRANCH && branch_node->cur != NULL)
                {
                    struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                    struct RecordType temp_rec;
                    memset(&temp_rec, 0, sizeof(temp_rec));
                    temp_rec.fields = branch->members;
                    codegen_register_record_field_enum_literals(symtab, &temp_rec);
                }
            }
        }
    }
}

static void codegen_register_type_enum_literals(ListNode_t *type_decls, SymTab_t *symtab)
{
    if (type_decls == NULL || symtab == NULL)
        return;

    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type != TREE_TYPE_DECL)
            continue;
        if (codegen_type_decl_suppressed(decl))
            continue;

        if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
            if (alias->is_enum && alias->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
                {
                    const char *name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)name, ordinal, enum_type);
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                 decl->tree_data.type_decl_data.info.record != NULL)
        {
            codegen_register_record_field_enum_literals(symtab,
                decl->tree_data.type_decl_data.info.record);
        }
    }
}

static int codegen_eval_const_expr(struct Expression *expr, SymTab_t *symtab,
    long long *out_value);

static void codegen_register_const_decls(ListNode_t *const_decls, SymTab_t *symtab)
{
    if (const_decls == NULL || symtab == NULL)
        return;

    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_CONST_DECL)
            continue;

        const char *id = decl->tree_data.const_decl_data.id;
        struct Expression *value = decl->tree_data.const_decl_data.value;

        if (id == NULL || value == NULL)
            continue;

        unsigned char set_bytes[32];
        size_t set_size = 0;
        long long set_mask = 0;
        int is_char_set = 0;

        if (expression_is_set_const_expr(symtab, value) &&
            evaluate_set_const_bytes(symtab, value, set_bytes, sizeof(set_bytes),
                &set_size, &set_mask, &is_char_set) == 0)
        {
            KgpcType *set_type = create_primitive_type(SET_TYPE);
            if (set_type != NULL)
            {
                if (is_char_set)
                    set_type->size_in_bytes = 32;
                else
                    set_type->size_in_bytes = 4;
            }
            PushSetConstOntoScope(symtab, (char *)id, set_bytes, (int)set_size, set_type);
            continue;
        }

        long long const_value = 0;
        if (codegen_eval_const_expr(value, symtab, &const_value))
            PushConstOntoScope(symtab, (char *)id, const_value);
        else if (value->type == EXPR_STRING && value->expr_data.string != NULL)
        {
            /* String constant — register in the symbol table so the existing
             * .LC label emission in gencode_leaf_var handles it with a unique,
             * scope-aware label.  PushStringConstOntoScope is a no-op if the
             * identifier already exists (e.g. from semcheck). */
            PushStringConstOntoScope(symtab, (char *)id, value->expr_data.string);
        }
    }
}

static void codegen_register_decl_list(ListNode_t *decls, SymTab_t *symtab, int is_param)
{
    if (decls == NULL || symtab == NULL)
        return;

    for (ListNode_t *cur = decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL)
            continue;
        if (decl->type != TREE_VAR_DECL && decl->type != TREE_ARR_DECL)
            continue;

        ListNode_t *ids = (decl->type == TREE_VAR_DECL)
            ? decl->tree_data.var_decl_data.ids
            : decl->tree_data.arr_decl_data.ids;

        KgpcType *decl_type = resolve_type_from_vardecl(decl, symtab, NULL);
        int is_array_decl = (decl->type == TREE_ARR_DECL);
        if (!is_array_decl && decl_type != NULL)
            is_array_decl = kgpc_type_is_array(decl_type);

        for (ListNode_t *id_node = ids; id_node != NULL; id_node = id_node->next)
        {
            if (id_node->cur == NULL)
                continue;
            if (is_array_decl)
                PushArrayOntoScope_Typed(symtab, (char *)id_node->cur, decl_type);
            else
                PushVarOntoScope_Typed(symtab, (char *)id_node->cur, decl_type);

            if (is_param)
            {
                HashNode_t *var_node = NULL;
                if (FindSymbol(&var_node, symtab, id_node->cur) != 0 && var_node != NULL)
                {
                    int is_var_param = 0;
                    int is_untyped_param = 0;
                    if (decl->type == TREE_VAR_DECL)
                    {
                        is_var_param = decl->tree_data.var_decl_data.is_var_param;
                        is_untyped_param = decl->tree_data.var_decl_data.is_untyped_param;
                    }
                    var_node->is_var_parameter = (is_var_param || is_untyped_param) ? 1 : 0;
                }
            }
        }

        if (decl_type != NULL)
            destroy_kgpc_type(decl_type);
    }
}

static void codegen_static_link_spilled(Register_t *reg, StackNode_t *spill_slot, void *context)
{
    (void)reg;
    CodeGenContext *ctx = (CodeGenContext *)context;
    if (ctx == NULL || spill_slot == NULL)
        return;
    ctx->static_link_spill_slot = spill_slot;
    ctx->static_link_reg = NULL;
}

static int codegen_find_static_link_offset(StackScope_t *scope, int *offset)
{
    if (scope == NULL || offset == NULL)
        return 0;

    ListNode_t *node = scope->x;
    while (node != NULL)
    {
        StackNode_t *stack_node = (StackNode_t *)node->cur;
        if (stack_node != NULL && stack_node->label != NULL &&
            strcmp(stack_node->label, "__static_link__") == 0)
        {
            *offset = stack_node->offset;
            return 1;
        }
        node = node->next;
    }

    return 0;
}

void codegen_begin_expression(CodeGenContext *ctx)
{
    codegen_reset_static_link_cache(ctx);
}

void codegen_end_expression(CodeGenContext *ctx)
{
    codegen_reset_static_link_cache(ctx);
}

Register_t *codegen_acquire_static_link(CodeGenContext *ctx, ListNode_t **inst_list,
    int levels_to_traverse)
{
    if (ctx == NULL || inst_list == NULL || levels_to_traverse <= 0)
        return NULL;

    if (ctx->static_link_reg != NULL)
    {
        if (ctx->static_link_reg_level == levels_to_traverse)
            return ctx->static_link_reg;

        free_reg(get_reg_stack(), ctx->static_link_reg);
        ctx->static_link_reg = NULL;
        ctx->static_link_reg_level = 0;
    }
    else if (ctx->static_link_spill_slot != NULL)
    {
        if (ctx->static_link_reg_level == levels_to_traverse)
        {
            Register_t *reloaded = get_free_reg(get_reg_stack(), inst_list);
            if (reloaded == NULL)
                reloaded = get_reg_with_spill(get_reg_stack(), inst_list);
            if (reloaded == NULL)
                return NULL;

            char buffer[64];
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                ctx->static_link_spill_slot->offset, reloaded->bit_64);
            *inst_list = add_inst(*inst_list, buffer);

            ctx->static_link_reg = reloaded;
            ctx->static_link_spill_slot = NULL;
            register_set_spill_callback(reloaded, codegen_static_link_spilled, ctx);
            return reloaded;
        }

        ctx->static_link_spill_slot = NULL;
    }

    StackScope_t *scope = get_cur_scope();
    if (scope == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine current scope for static link traversal.");
        return NULL;
    }

    int *offsets = (int *)calloc((size_t)levels_to_traverse, sizeof(int));
    if (offsets == NULL)
    {
        codegen_report_error(ctx, "ERROR: Failed to allocate static link traversal metadata.");
        return NULL;
    }

    StackScope_t *current_scope = scope;
    for (int i = 0; i < levels_to_traverse; ++i)
    {
        if (current_scope == NULL)
        {
            codegen_report_error(ctx, "ERROR: Static link chain shorter than requested depth.");
            free(offsets);
            return NULL;
        }

        if (!codegen_find_static_link_offset(current_scope, &offsets[i]))
        {
            codegen_report_error(ctx, "ERROR: Static link slot missing at depth %d.", i);
            free(offsets);
            return NULL;
        }

        current_scope = current_scope->prev_scope;
    }

    Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
    if (reg == NULL)
        reg = get_reg_with_spill(get_reg_stack(), inst_list);
    if (reg == NULL)
    {
        free(offsets);
        return NULL;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", offsets[0], reg->bit_64);
    *inst_list = add_inst(*inst_list, buffer);

    for (int i = 1; i < levels_to_traverse; ++i)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%s), %s\n", offsets[i],
            reg->bit_64, reg->bit_64);
        *inst_list = add_inst(*inst_list, buffer);
    }

    free(offsets);

    ctx->static_link_reg = reg;
    ctx->static_link_reg_level = levels_to_traverse;
    register_set_spill_callback(reg, codegen_static_link_spilled, ctx);
    return reg;
}

void codegen_report_error(CodeGenContext *ctx, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    if (fmt != NULL && fmt[0] != '\0')
    {
        size_t len = strlen(fmt);
        if (len == 0 || fmt[len - 1] != '\n')
            fputc('\n', stderr);
    }
    va_end(args);
    if (ctx != NULL)
        ctx->had_error = 1;
}

void codegen_report_warning(const CodeGenContext *ctx, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    if (fmt != NULL && fmt[0] != '\0')
    {
        size_t len = strlen(fmt);
        if (len == 0 || fmt[len - 1] != '\n')
            fputc('\n', stderr);
    }
    va_end(args);
    (void)ctx;
}

int codegen_had_error(const CodeGenContext *ctx)
{
    return (ctx != NULL) ? ctx->had_error : 0;
}

static void codegen_reset_finally_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->finally_stack != NULL)
    {
        free(ctx->finally_stack);
        ctx->finally_stack = NULL;
    }
    ctx->finally_depth = 0;
    ctx->finally_capacity = 0;
}

static void codegen_reset_except_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->except_frames != NULL)
    {
        for (int i = 0; i < ctx->except_depth; ++i)
        {
            free(ctx->except_frames[i].label);
            ctx->except_frames[i].label = NULL;
        }
        free(ctx->except_frames);
        ctx->except_frames = NULL;
    }
    ctx->except_depth = 0;
    ctx->except_capacity = 0;
}

static void codegen_reset_with_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->with_stack != NULL)
    {
        free(ctx->with_stack);
        ctx->with_stack = NULL;
    }
    ctx->with_depth = 0;
    ctx->with_capacity = 0;
}

static void codegen_reset_loop_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->loop_frames != NULL)
    {
        for (int i = 0; i < ctx->loop_depth; ++i)
        {
            free(ctx->loop_frames[i].label);
            ctx->loop_frames[i].label = NULL;
            free(ctx->loop_frames[i].continue_label);
            ctx->loop_frames[i].continue_label = NULL;
        }
        free(ctx->loop_frames);
        ctx->loop_frames = NULL;
    }
    ctx->loop_depth = 0;
    ctx->loop_capacity = 0;
}

/* -------------------------------------------------------------------------
 * Debug helpers for annotated assembly output
 * ------------------------------------------------------------------------- */
static void asm_debug_comment(FILE *out, const char *tag, int indent, const char *fmt, ...)
{
    if (out == NULL || tag == NULL || fmt == NULL)
        return;
    if (indent < 0)
        indent = 0;

    fprintf(out, "# [%s] ", tag);
    for (int i = 0; i < indent; ++i)
        fputs("  ", out);

    va_list args;
    va_start(args, fmt);
    vfprintf(out, fmt, args);
    va_end(args);
    fputc('\n', out);
}

static const char *hash_type_to_string(enum HashType type)
{
    switch (type)
    {
        case HASHTYPE_VAR: return "var";
        case HASHTYPE_ARRAY: return "array";
        case HASHTYPE_CONST: return "const";
        case HASHTYPE_PROCEDURE: return "procedure";
        case HASHTYPE_FUNCTION: return "function";
        case HASHTYPE_FUNCTION_RETURN: return "function-return";
        case HASHTYPE_BUILTIN_PROCEDURE: return "builtin-proc";
        case HASHTYPE_TYPE: return "type";
        default: return "unknown";
    }
}

static const char *hashnode_type_to_string(const HashNode_t *node)
{
    if (node == NULL)
        return "<null>";
    
    if (node->type != NULL)
        return kgpc_type_to_string(node->type);

    return "untyped";
}

static void summarize_string_literal(const char *src, char *dest, size_t dest_size)
{
    if (dest == NULL || dest_size == 0)
        return;
    dest[0] = '\0';
    if (src == NULL)
        return;

    size_t out_idx = 0;
    dest[out_idx++] = '"';
    size_t i = 0;
    const size_t max_chars = 24;
    while (src[i] != '\0' && out_idx + 2 < dest_size && i < max_chars)
    {
        unsigned char ch = (unsigned char)src[i++];
        if (!isprint(ch) || ch == '"' || ch == '\\')
            dest[out_idx++] = '?';
        else
            dest[out_idx++] = (char)ch;
    }
    if (src[i] != '\0' && out_idx + 4 < dest_size)
    {
        dest[out_idx++] = '.';
        dest[out_idx++] = '.';
        dest[out_idx++] = '.';
    }
    if (out_idx + 1 < dest_size)
        dest[out_idx++] = '"';
    dest[out_idx] = '\0';
}

static void codegen_emit_semantic_scope_comments(FILE *out, const HashTable_t *table,
    const char *label, int indent)
{
    if (out == NULL || label == NULL)
        return;

    asm_debug_comment(out, "semcheck", indent, "%s", label);
    if (table == NULL)
    {
        asm_debug_comment(out, "semcheck", indent + 1, "(empty)");
        return;
    }

    int entries = 0;
    for (int i = 0; i < TABLE_SIZE; ++i)
    {
        ListNode_t *entry = table->table[i];
        while (entry != NULL)
        {
            HashNode_t *node = (HashNode_t *)entry->cur;
            entry = entry->next;
            if (node == NULL)
                continue;

            ++entries;
            char mangled_buf[96] = "";
            if (node->mangled_id != NULL && node->mangled_id[0] != '\0')
                snprintf(mangled_buf, sizeof(mangled_buf), ", mangled=%s", node->mangled_id);

            char const_buf[128] = "";
            if (node->hash_type == HASHTYPE_CONST)
            {
                if (node->const_string_value != NULL)
                {
                    char snippet[48];
                    summarize_string_literal(node->const_string_value, snippet, sizeof(snippet));
                    snprintf(const_buf, sizeof(const_buf), ", const=%s", snippet);
                }
                else
                {
                    snprintf(const_buf, sizeof(const_buf), ", const=%lld",
                        (long long)node->const_int_value);
                }
            }

            char link_buf[48] = "";
            if (hashnode_requires_static_link(node))
                snprintf(link_buf, sizeof(link_buf), ", needs-static-link");

            asm_debug_comment(out, "semcheck", indent + 1,
                "%s kind=%s type=%s%s%s%s",
                (node->id != NULL) ? node->id : "<unnamed>",
                hash_type_to_string(node->hash_type),
                hashnode_type_to_string(node),
                mangled_buf,
                const_buf,
                link_buf);
        }
    }

    if (entries == 0)
        asm_debug_comment(out, "semcheck", indent + 1, "(empty)");
}

static void codegen_emit_semantic_debug_block(CodeGenContext *ctx)
{
    if (!asm_debug_flag() || ctx == NULL || ctx->symtab == NULL || ctx->output_file == NULL)
        return;

    FILE *out = ctx->output_file;
    asm_debug_comment(out, "semcheck", 0, "--- symbol table snapshot ---");
    codegen_emit_semantic_scope_comments(out, ctx->symtab->builtin_scope->table, "builtins", 0);

    ScopeNode *scope = ctx->symtab->current_scope;
    int depth = 0;
    while (scope != NULL)
    {
        HashTable_t *table = scope->table;
        char label[32];
        snprintf(label, sizeof(label), "scope %d", depth);
        codegen_emit_semantic_scope_comments(out, table, label, 0);
        scope = scope->parent;
        ++depth;
    }

    asm_debug_comment(out, "semcheck", 0, "--- end symbol table ---");
}

static void codegen_emit_function_debug_comments(const char *func_name, CodeGenContext *ctx)
{
    if (!asm_debug_flag() || ctx == NULL || ctx->output_file == NULL || func_name == NULL)
        return;

    asm_debug_comment(ctx->output_file, "codegen", 0,
        "function %s (lex-depth=%d)", func_name,
        ctx->current_subprogram_lexical_depth);

    StackScope_t *scope = get_cur_scope();
    if (scope != NULL)
    {
        int locals = scope->x_offset;
        int temps = scope->t_offset;
        int args = scope->z_offset;
        int total = get_full_stack_offset();
        asm_debug_comment(ctx->output_file, "codegen", 1,
            "stack locals=%dB temps=%dB args=%dB total=%dB",
            locals, temps, args, total);
    }
    else
    {
        asm_debug_comment(ctx->output_file, "codegen", 1,
            "stack scope unavailable");
    }

    int needs_link = codegen_proc_requires_static_link(ctx, func_name);
    asm_debug_comment(ctx->output_file, "codegen", 1,
        "static-link=%s", needs_link ? "required" : "not-required");
}

void codegen_sanitize_identifier_for_label(const char *value, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    size_t idx = 0;
    if (value == NULL || value[0] == '\0')
    {
        buffer[idx++] = 'v';
    }
    else
    {
        for (const char *p = value; *p != '\0' && idx + 1 < size; ++p)
        {
            unsigned char c = (unsigned char)*p;
            if (isalnum(c) || c == '_')
                buffer[idx++] = (char)c;
            else
                buffer[idx++] = '_';
        }
    }

    if (idx == 0)
        buffer[idx++] = 'v';
    buffer[idx] = '\0';
}

static char *codegen_make_program_var_label(CodeGenContext *ctx, const char *name)
{
    if (ctx == NULL)
        return NULL;

    char sanitized[128];
    codegen_sanitize_identifier_for_label(name, sanitized, sizeof(sanitized));

    char buffer[256];
    snprintf(buffer, sizeof(buffer), "__kgpc_program_var_%s_%d",
        sanitized, ++ctx->global_data_counter);
    return strdup(buffer);
}

/* Emit either a .bss allocation with a bare-name alias (when the variable's
 * user-facing symbol differs from its internal label, e.g. unit variables)
 * or a simple .comm directive.  Using .bss instead of .comm is required
 * because .set cannot target .comm symbols on most assemblers.
 *
 *   sym   – the user-facing symbol name (e.g. "MyVar")
 *   label – the internal storage label (e.g. "__kgpc_program_var_MyVar_1")
 *   size  – allocation size in bytes
 *   alignment – required alignment
 *   defined_in_unit – non-zero when the var comes from a unit (needs alias)
 */
static void codegen_emit_bss_or_comm(FILE *out, const char *sym, const char *label,
                                     int size, int alignment, int defined_in_unit)
{
    int need_alias = (label != NULL && sym != NULL &&
                      defined_in_unit && strcmp(sym, label) != 0);

    if (need_alias) {
        if (codegen_target_is_windows()) {
            fprintf(out, "\t.section .bss\n");
        } else {
            fprintf(out, "\t.pushsection .bss\n");
        }
        if (alignment > 0)
            fprintf(out, "\t.align\t%d\n", alignment);
        fprintf(out, ".globl\t%s\n", label);
        fprintf(out, "%s:\n", label);
        fprintf(out, "\t.zero\t%d\n", size);
        fprintf(out, ".globl\t%s\n", sym);
        fprintf(out, "\t.set\t%s, %s\n", sym, label);
        if (codegen_target_is_windows()) {
            fprintf(out, "\t.section .text\n");
        } else {
            fprintf(out, "\t.popsection\n");
        }
    } else {
        const char *effective = label != NULL ? label : sym;
        if (effective == NULL)
            return;
        if (alignment > 0)
            fprintf(out, "\t.comm\t%s,%d,%d\n", effective, size, alignment);
        else
            fprintf(out, "\t.comm\t%s,%d\n", effective, size);
    }
}

void codegen_common_enum_typeinfo_label(const char *type_id, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    buffer[0] = '\0';
    if (type_id == NULL || type_id[0] == '\0')
        return;

    char sanitized[CODEGEN_MAX_INST_BUF];
    codegen_sanitize_identifier_for_label(type_id, sanitized, sizeof(sanitized));
    {
        const char *prefix = "__kgpc_enum_typeinfo_";
        snprintf(buffer, size, "%s%.*s", prefix,
            (int)((size > strlen(prefix) + 1) ? (size - strlen(prefix) - 1) : 0),
            sanitized);
    }
}

void codegen_common_record_typeinfo_label(const char *type_id, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    buffer[0] = '\0';
    if (type_id == NULL || type_id[0] == '\0')
        return;

    char sanitized[CODEGEN_MAX_INST_BUF];
    codegen_sanitize_identifier_for_label(type_id, sanitized, sizeof(sanitized));
    snprintf(buffer, size, "%s_TYPEINFO", sanitized);
}

void codegen_common_typeinfo_label_for_type_id(SymTab_t *symtab, const char *type_id,
    char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    buffer[0] = '\0';
    if (type_id == NULL || type_id[0] == '\0')
        return;

    HashNode_t *type_node = NULL;
    if (symtab != NULL &&
        FindSymbol(&type_node, symtab, type_id) != 0 &&
        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_enum)
        {
            codegen_common_enum_typeinfo_label(type_id, buffer, size);
            return;
        }

        if (hashnode_get_record_type(type_node) != NULL)
        {
            codegen_common_record_typeinfo_label(type_id, buffer, size);
            return;
        }
    }

    codegen_common_enum_typeinfo_label(type_id, buffer, size);
}

static void codegen_emit_enum_typeinfo_for_alias(CodeGenContext *ctx, const char *type_name,
    struct TypeAlias *alias)
{
    if (ctx == NULL || ctx->output_file == NULL || type_name == NULL || alias == NULL)
        return;
    if (!alias->is_enum || alias->enum_literals == NULL)
        return;

    int count = ListLength(alias->enum_literals);
    if (count <= 0)
        return;

    char type_label[CODEGEN_MAX_INST_BUF];
    codegen_sanitize_identifier_for_label(type_name, type_label, sizeof(type_label));

    size_t typeinfo_len = strlen(type_label) + strlen("__kgpc_enum_typeinfo_") + 1;
    char *typeinfo_label = (char *)malloc(typeinfo_len);
    if (typeinfo_label == NULL)
        return;
    snprintf(typeinfo_label, typeinfo_len, "__kgpc_enum_typeinfo_%s", type_label);

    fprintf(ctx->output_file, "\n# Enum RTTI for %s\n", type_name);
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s\n", typeinfo_label);
    fprintf(ctx->output_file, "%s:\n", typeinfo_label);

    /* Emit enum TypeInfo/TTypeData in the layout KGPC's current FPC RTL
     * TypInfo build expects:
     *   Kind: Byte = tkEnumeration (3)
     *   Name: ShortString
     *   AttributeTable: Pointer = nil
     *   OrdType: Byte = otULong (5) for default zero-based enums
     *   MinValue, MaxValue: LongInt
     *   BaseType: PPTypeInfo (nil)
     *   NameList: packed shortstrings for each literal, followed by unit/type owner
     * This keeps KGPC-emitted enum RTTI aligned with the TypInfo offsets
     * produced by KGPC's own FPC RTL build. */
    {
        char escaped_type_name[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_type_name, type_name, sizeof(escaped_type_name));
        fprintf(ctx->output_file, "\t.byte\t3,%zu\n", strlen(type_name));
        if (type_name[0] != '\0')
            fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_type_name);
        fprintf(ctx->output_file, "\t.quad\t0\n");
        fprintf(ctx->output_file, "\t.byte\t5\n");
        fprintf(ctx->output_file, "\t.long\t0,%d\n", count - 1);
        fprintf(ctx->output_file, "\t.quad\t0\n");
    }

    for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next)
    {
        const char *literal = (lit->cur != NULL) ? (const char *)lit->cur : "";
        char escaped_literal[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_literal, literal, sizeof(escaped_literal));
        fprintf(ctx->output_file, "\t.byte\t%zu\n", strlen(literal));
        if (literal[0] != '\0')
            fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_literal);
    }

    {
        char escaped_owner_name[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_owner_name, type_name, sizeof(escaped_owner_name));
        fprintf(ctx->output_file, "\t.byte\t%zu\n", strlen(type_name));
        if (type_name[0] != '\0')
            fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_owner_name);
        fprintf(ctx->output_file, "\t.byte\t0\n");
    }
    free(typeinfo_label);
}

static void codegen_emit_enum_typeinfo_from_table(CodeGenContext *ctx, HashTable_t *table,
    int emit_unit_types, const char **emitted_labels, int *emitted_count, int *emitted_any)
{
    if (ctx == NULL || table == NULL)
        return;

    for (int i = 0; i < TABLE_SIZE; ++i)
    {
        ListNode_t *entry = table->table[i];
        while (entry != NULL)
        {
            HashNode_t *node = (HashNode_t *)entry->cur;
            entry = entry->next;
            if (node == NULL || node->hash_type != HASHTYPE_TYPE)
                continue;

            if (emit_unit_types)
            {
                if (!node->defined_in_unit)
                    continue;
            }
            else
            {
                if (node->defined_in_unit)
                    continue;
            }

            if (node->type == NULL)
                continue;

            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias == NULL || !alias->is_enum || alias->enum_literals == NULL)
                continue;

            const char *type_name = (alias->alias_name != NULL) ? alias->alias_name : node->id;
            if (type_name == NULL || type_name[0] == '\0')
                continue;

            char type_label[CODEGEN_MAX_INST_BUF];
            codegen_sanitize_identifier_for_label(type_name, type_label, sizeof(type_label));
            size_t label_len = strlen(type_label) + strlen("__kgpc_enum_typeinfo_") + 1;
            char *label = (char *)malloc(label_len);
            if (label == NULL)
                continue;
            snprintf(label, label_len, "__kgpc_enum_typeinfo_%s", type_label);

            int already_emitted = 0;
            for (int idx = 0; idx < *emitted_count; ++idx)
            {
                if (emitted_labels[idx] != NULL && strcmp(emitted_labels[idx], label) == 0)
                {
                    already_emitted = 1;
                    break;
                }
            }
            if (already_emitted)
                continue;

            if (*emitted_count < 512)
            {
                emitted_labels[*emitted_count] = strdup(label);
                if (emitted_labels[*emitted_count] != NULL)
                    (*emitted_count)++;
            }
            free(label);

            if (emitted_any != NULL && !(*emitted_any))
            {
                fprintf(ctx->output_file, "\n# Enum RTTI metadata\n");
                fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
                *emitted_any = 1;
            }

            codegen_emit_enum_typeinfo_for_alias(ctx, type_name, alias);
        }
    }
}

static void codegen_emit_enum_typeinfo(CodeGenContext *ctx, SymTab_t *symtab, int emit_unit_types)
{
    if (ctx == NULL || symtab == NULL)
        return;

    const char *emitted_labels[512];
    int emitted_count = 0;
    int emitted_any = 0;
    for (int i = 0; i < 512; ++i)
        emitted_labels[i] = NULL;

    if (symtab->builtin_scope->table != NULL)
        codegen_emit_enum_typeinfo_from_table(ctx, symtab->builtin_scope->table, emit_unit_types,
            emitted_labels, &emitted_count, &emitted_any);

    ScopeNode *scope = symtab->current_scope;
    while (scope != NULL)
    {
        HashTable_t *table = scope->table;
        codegen_emit_enum_typeinfo_from_table(ctx, table, emit_unit_types,
            emitted_labels, &emitted_count, &emitted_any);
        scope = scope->parent;
    }

    if (emitted_any)
        fprintf(ctx->output_file, ".text\n");

    for (int i = 0; i < emitted_count; ++i)
        free((void *)emitted_labels[i]);
}
/* Generates a label */
void gen_label(char *buf, int buf_len, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(buf != NULL);
    assert(ctx != NULL);
    snprintf(buf, buf_len, ".L%d", ++ctx->label_counter);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Adds instruction to instruction list */
/* WARNING: Makes copy of given char * */
/* Tail pointer for O(1) add_inst append.
 * Tracks the (head, tail) of the last inst_list built by add_inst.
 * When the same head is passed and the cached tail's ->next is still NULL,
 * we append in O(1) instead of walking the entire list (O(n)).
 * MUST be invalidated before free_inst_list and ConcatList. */
static ListNode_t *g_inst_tail = NULL;
static ListNode_t *g_inst_head = NULL;

void add_inst_invalidate_cache(void)
{
    g_inst_tail = NULL;
    g_inst_head = NULL;
}

ListNode_t *add_inst(ListNode_t *inst_list, const char *inst)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    ListNode_t *new_node;

    assert(inst != NULL);
    new_node = CreateListNode(strdup(inst), LIST_STRING);
    if(inst_list == NULL)
    {
        inst_list = new_node;
    }
    else if (g_inst_head == inst_list && g_inst_tail != NULL && g_inst_tail->next == NULL)
    {
        /* Fast path: cached tail is valid, O(1) append */
        g_inst_tail->next = new_node;
    }
    else
    {
        /* Slow path: walk to end */
        PushListNodeBack(inst_list, new_node);
    }
    g_inst_head = inst_list;
    g_inst_tail = new_node;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_emit_interface_vtable_slot_init(ListNode_t *inst_list,
    CodeGenContext *ctx, const struct RecordType *class_record,
    const char *class_type_id, Register_t *instance_reg)
{
    if (ctx == NULL || class_record == NULL || class_type_id == NULL || instance_reg == NULL ||
        class_record->num_interfaces <= 0)
        return inst_list;

    long long base_size = 0;
    codegen_sizeof_record_type(ctx, (struct RecordType *)class_record, &base_size);

    int iface_count = 0;
    for (int ii = 0; ii < class_record->num_interfaces; ++ii)
    {
        if (class_record->interface_names[ii] != NULL)
            iface_count++;
    }
    if (iface_count <= 0)
        return inst_list;

    if (class_record->has_cached_size)
        base_size = class_record->cached_size - iface_count * 8;

    Register_t *ivtbl_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (ivtbl_reg == NULL)
        return inst_list;

    char buffer[CODEGEN_MAX_INST_BUF];
    int slot_idx = 0;
    for (int ii = 0; ii < class_record->num_interfaces; ++ii)
    {
        if (class_record->interface_names[ii] == NULL)
            continue;
        long long offset = base_size + slot_idx * 8;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s_INTF_%s_VTABLE(%%rip), %s\n",
            class_type_id, class_record->interface_names[ii], ivtbl_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %lld(%s)\n",
            ivtbl_reg->bit_64, offset, instance_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        slot_idx++;
    }

    free_reg(get_reg_stack(), ivtbl_reg);
    return inst_list;
}

ListNode_t *codegen_emit_interface_dispatch(ListNode_t *inst_list,
    CodeGenContext *ctx, const char *self_reg, const char *iface_name,
    int vmt_index, const char *label_prefix, const char *target_slot_label,
    int preserve_indirect_call_regs, CodegenCallArgSpillFn spill_fn,
    CodegenCallArgRestoreFn restore_fn)
{
    if (ctx == NULL || self_reg == NULL || label_prefix == NULL || target_slot_label == NULL)
        return inst_list;

    char buffer[CODEGEN_MAX_INST_BUF];
    int label_id = ++ctx->label_counter;

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\tmovq\t(%r11), %r11\n");
    inst_list = add_inst(inst_list, "\tmovq\t(%r11), %rax\n");
    inst_list = add_inst(inst_list, "\taddq\t8(%r11), %rax\n");
    snprintf(buffer, sizeof(buffer), "\tjz\t.L%s_direct_%d\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n", vmt_index * 8);
    inst_list = add_inst(inst_list, buffer);
    if (preserve_indirect_call_regs)
    {
        CallerSaveState caller_state;
        regstack_caller_save(get_reg_stack(), &inst_list, &caller_state);
        inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
        regstack_caller_restore(get_reg_stack(), &inst_list, &caller_state);
    }
    else
    {
        inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
    }
    snprintf(buffer, sizeof(buffer), "\tjmp\t.L%s_done_%d\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), ".L%s_direct_%d:\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);

    StackNode_t *target_slot = add_l_t_bytes((char *)target_slot_label, 8);
    if (iface_name != NULL && iface_name[0] != '\0' && target_slot != NULL &&
        spill_fn != NULL && restore_fn != NULL)
    {
        char guid_label[640];
        int arg_spills[6] = {0};
        int xmm_spills[8] = {0};
        snprintf(guid_label, sizeof(guid_label), "__kgpc_guid_%s", iface_name);
        inst_list = spill_fn(inst_list, arg_spills, xmm_spills);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", self_reg, current_arg_reg64(0));
        inst_list = add_inst(inst_list, buffer);
        {
            char guid_buffer[768];
            snprintf(guid_buffer, sizeof(guid_buffer), "\tleaq\t%s(%%rip), %s\n", guid_label, current_arg_reg64(1));
            inst_list = add_inst(inst_list, guid_buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", vmt_index, current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tcall\t__kgpc_resolve_intf_method\n");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", target_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = restore_fn(inst_list, arg_spills, xmm_spills);
        if (preserve_indirect_call_regs)
        {
            CallerSaveState caller_state;
            regstack_caller_save(get_reg_stack(), &inst_list, &caller_state);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n", target_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
            regstack_caller_restore(get_reg_stack(), &inst_list, &caller_state);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n", target_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
        }
    }

    snprintf(buffer, sizeof(buffer), ".L%s_done_%d:\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);
    return inst_list;
}

/* Frees instruction list */
void free_inst_list(ListNode_t *inst_list)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    ListNode_t *cur;

    if(inst_list == NULL)
        return;

    /* Invalidate the tail cache — the nodes about to be freed
     * might include g_inst_tail. */
    add_inst_invalidate_cache();

    cur = inst_list;
    while(cur != NULL)
    {
        free(cur->cur);
        cur = cur->next;
    }

    DestroyList(inst_list);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates jmp */
/* Inverse jumps on the inverse of the type */
ListNode_t *gencode_jmp(int type, int inverse, char *label, ListNode_t *inst_list)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[30], jmp_buf[6];

    assert(label != NULL);

    switch(type)
    {
        case EQ:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jne");
            else
                snprintf(jmp_buf, 6, "je");
            break;
        case NE:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "je");
            else
                snprintf(jmp_buf, 6, "jne");
            break;
        case LT:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jge");
            else
                snprintf(jmp_buf, 6, "jl");
            break;
        case LE:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jg");
            else
                snprintf(jmp_buf, 6, "jle");
            break;
        case GT:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jle");
            else
                snprintf(jmp_buf, 6, "jg");
            break;
        case GE:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jl");
            else
                snprintf(jmp_buf, 6, "jge");
            break;

        /* Unsigned variants: use jb/jbe/ja/jae instead of jl/jle/jg/jge */
        case LT_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "jae" : "jb");
            break;
        case LE_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "ja" : "jbe");
            break;
        case GT_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "jbe" : "ja");
            break;
        case GE_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "jb" : "jae");
            break;

        case NORMAL_JMP:
            snprintf(jmp_buf, 6, "jmp");
            break;

        default:
            assert(0 && "Unrecognized relop type in jmp generation!");
            break;
    }

    snprintf(buffer, 30, "\t%s\t%s\n", jmp_buf, label);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return add_inst(inst_list, buffer);
}

/* Forward declaration */
void codegen_function_header_ex_alias_vis(char *func_name, CodeGenContext *ctx, int nostackframe, const char *cname_override, int emit_weak);

/* Generates a function header.
 * If nostackframe is set, only emits the label without prologue (push %rbp / mov %rsp, %rbp).
 * If cname_override is set and differs from func_name, emits an additional .globl + label alias. */
void codegen_function_header_ex_alias(char *func_name, CodeGenContext *ctx, int nostackframe, const char *cname_override)
{
    codegen_function_header_ex_alias_vis(func_name, ctx, nostackframe, cname_override, 0);
}

/* Emit the function header.  Always uses .globl for symbol visibility. */
void codegen_function_header_ex_alias_vis(char *func_name, CodeGenContext *ctx, int nostackframe, const char *cname_override, int emit_weak)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_name != NULL);
    assert(ctx != NULL);
    codegen_emit_function_debug_comments(func_name, ctx);
    /* All functions use .globl — the compiler produces a single .s file but
     * the runtime library (.a) needs to call many unit-defined functions. */
    const char *vis = ".globl";
    (void)emit_weak;
    /* Emit alias label from cname_override (e.g. [Public,Alias:'FPC_DO_EXIT']) */
    if (cname_override != NULL && strcmp(cname_override, func_name) != 0) {
        fprintf(ctx->output_file, "%s\t%s\n", vis, cname_override);
        fprintf(ctx->output_file, "%s:\n", cname_override);
    }
    fprintf(ctx->output_file, "%s\t%s\n", vis, func_name);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_proc\t%s\n", func_name);
    if (nostackframe) {
        fprintf(ctx->output_file, "%s:\n", func_name);
    } else {
        fprintf(ctx->output_file, "%s:\n\tpushq\t%%rbp\n", func_name);
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_pushreg\t%%rbp\n");
        fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rbp\n");
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_setframe\t%%rbp, 0\n");
    }

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_function_header_ex(char *func_name, CodeGenContext *ctx, int nostackframe)
{
    codegen_function_header_ex_alias(func_name, ctx, nostackframe, NULL);
}

void codegen_function_header(char *func_name, CodeGenContext *ctx)
{
    codegen_function_header_ex(func_name, ctx, 0);
}

/* Generates a function footer.
 * If nostackframe is set, emits only ret without leave. */
void codegen_function_footer_ex(char *func_name, CodeGenContext *ctx, int nostackframe)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_name != NULL);
    assert(ctx != NULL);
    if (nostackframe) {
        fprintf(ctx->output_file, "\tret\n");
    } else {
        /* Restore callee-saved GP registers before leaving the frame */
        if (ctx->callee_save_rbx_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
        if (ctx->callee_save_r12_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
        if (ctx->callee_save_r13_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
        if (ctx->callee_save_r14_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
        if (ctx->callee_save_r15_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
        fprintf(ctx->output_file, "\tnop\n\tleave\n\tret\n");
    }
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_endproc\n");

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_function_footer(char *func_name, CodeGenContext *ctx)
{
    codegen_function_footer_ex(func_name, ctx, 0);
}


/* This is the entry function */
void codegen(Tree_t *tree, const char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab,
             CompilationContext *comp_ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char *prgm_name;

    assert(tree != NULL);
    assert(input_file_name != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (ctx->target_abi != KGPC_TARGET_ABI_SYSTEM_V && ctx->target_abi != KGPC_TARGET_ABI_WINDOWS)
        ctx->target_abi = current_target_abi();

    g_current_codegen_abi = ctx->target_abi;
    g_stack_home_space_bytes = (ctx->target_abi == KGPC_TARGET_ABI_WINDOWS) ? 32 : 0;
    ctx->pending_stack_arg_bytes = 0;
    ctx->emitted_subprograms = NULL;
    ctx->comp_ctx = comp_ctx;
    g_codegen_available_subprograms = NULL;
    memset(&g_codegen_callable_exports, 0, sizeof(g_codegen_callable_exports));

    ctx->symtab = symtab;
    symtab->skip_unit_filter = 1;

    codegen_reset_finally_stack(ctx);
    codegen_reset_loop_stack(ctx);
    codegen_reset_except_stack(ctx);

    CODEGEN_DEBUG("DEBUG: ENTERING codegen\n");
    init_stackmng();

    codegen_program_header(input_file_name, ctx);

    /* Collect callable export names from loaded units, then program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT)
                codegen_collect_callable_export_names(unit->tree_data.unit_data.subprograms);
        }
    }
    codegen_collect_callable_export_names(tree->tree_data.program_data.subprograms);

    codegen_rodata(ctx, symtab);
    codegen_emit_enum_typeinfo(ctx, symtab, 0);

    /* Collect available subprogram labels from loaded units, then program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT)
                codegen_collect_available_subprogram_labels(unit->tree_data.unit_data.subprograms);
        }
    }
    codegen_collect_available_subprogram_labels(tree->tree_data.program_data.subprograms);

    codegen_vmt(ctx, symtab, tree, comp_ctx);

    prgm_name = codegen_program(tree, ctx, symtab, comp_ctx);
    codegen_main(prgm_name, ctx);

    /* Emit weak stubs for method labels that were referenced (e.g., via
     * @MethodName) but whose bodies are not available in this compilation.
     * Must run AFTER codegen_program so all method refs are collected. */
    codegen_emit_unresolved_method_stubs(ctx->output_file,
        ctx->emitted_subprograms);

    codegen_program_footer(ctx);

    if (ctx->emitted_subprograms != NULL)
    {
        DestroyList(ctx->emitted_subprograms);
        ctx->emitted_subprograms = NULL;
    }
    if (g_codegen_available_subprograms != NULL)
    {
        DestroyList(g_codegen_available_subprograms);
        g_codegen_available_subprograms = NULL;
    }
    codegen_set_destroy(&g_codegen_callable_exports);

    free_stackmng();
    codegen_reset_loop_stack(ctx);
    codegen_reset_finally_stack(ctx);
    codegen_reset_except_stack(ctx);
    codegen_reset_with_stack(ctx);

    CODEGEN_DEBUG("DEBUG: LEAVING codegen\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_unit(Tree_t *tree, const char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(tree != NULL);
    assert(tree->type == TREE_UNIT);
    assert(input_file_name != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (ctx->target_abi != KGPC_TARGET_ABI_SYSTEM_V && ctx->target_abi != KGPC_TARGET_ABI_WINDOWS)
        ctx->target_abi = current_target_abi();

    g_current_codegen_abi = ctx->target_abi;
    g_stack_home_space_bytes = (ctx->target_abi == KGPC_TARGET_ABI_WINDOWS) ? 32 : 0;
    ctx->pending_stack_arg_bytes = 0;
    ctx->emitted_subprograms = NULL;
    g_codegen_available_subprograms = NULL;
    memset(&g_codegen_callable_exports, 0, sizeof(g_codegen_callable_exports));

    ctx->symtab = symtab;
    symtab->skip_unit_filter = 1;

    codegen_reset_finally_stack(ctx);
    codegen_reset_loop_stack(ctx);
    codegen_reset_except_stack(ctx);

    init_stackmng();

    codegen_program_header(input_file_name, ctx);
    codegen_collect_callable_export_names(tree->tree_data.unit_data.subprograms);
    codegen_rodata(ctx, symtab);
    codegen_emit_enum_typeinfo(ctx, symtab, 1);
    codegen_collect_available_subprogram_labels(tree->tree_data.unit_data.subprograms);
    codegen_vmt(ctx, symtab, tree, NULL);

    /* Generate code for unit subprograms */
    codegen_subprograms(tree->tree_data.unit_data.subprograms, ctx, symtab);

    /* Generate initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL)
    {
        char *unit_id = tree->tree_data.unit_data.unit_id;
        char init_label[CODEGEN_LABEL_BUFFER_SIZE];
        snprintf(init_label, sizeof(init_label), "_UNIT_%s_INIT", unit_id ? unit_id : "UNKNOWN");

        int prev_callee_rbx = ctx->callee_save_rbx_offset;
        int prev_callee_r12 = ctx->callee_save_r12_offset;
        int prev_callee_r13 = ctx->callee_save_r13_offset;
        int prev_callee_r14 = ctx->callee_save_r14_offset;
        int prev_callee_r15 = ctx->callee_save_r15_offset;
        push_stackscope();
        codegen_function_locals(tree->tree_data.unit_data.interface_var_decls, ctx, symtab);
        codegen_function_locals(tree->tree_data.unit_data.implementation_var_decls, ctx, symtab);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.interface_const_decls);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.implementation_const_decls);
        {
            StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
            StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
            StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
            StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
            StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
            ctx->callee_save_rbx_offset = rbx_slot->offset;
            ctx->callee_save_r12_offset = r12_slot->offset;
            ctx->callee_save_r13_offset = r13_slot->offset;
            ctx->callee_save_r14_offset = r14_slot->offset;
            ctx->callee_save_r15_offset = r15_slot->offset;
        }
        ListNode_t *inst_list = NULL;
        inst_list = codegen_stmt(tree->tree_data.unit_data.initialization, inst_list, ctx, symtab);

        fprintf(ctx->output_file, "\t.globl\t%s\n", init_label);
        fprintf(ctx->output_file, "%s:\n", init_label);
        fprintf(ctx->output_file, "\tpushq\t%%rbp\n");
        fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rbp\n");
        codegen_stack_space(ctx);
        codegen_inst_list(inst_list, ctx);
        if (ctx->callee_save_rbx_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
        if (ctx->callee_save_r12_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
        if (ctx->callee_save_r13_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
        if (ctx->callee_save_r14_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
        if (ctx->callee_save_r15_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
        fprintf(ctx->output_file, "\tleave\n");
        fprintf(ctx->output_file, "\tret\n");

        free_inst_list(inst_list);
        pop_stackscope();
        ctx->callee_save_rbx_offset = prev_callee_rbx;
        ctx->callee_save_r12_offset = prev_callee_r12;
        ctx->callee_save_r13_offset = prev_callee_r13;
        ctx->callee_save_r14_offset = prev_callee_r14;
        ctx->callee_save_r15_offset = prev_callee_r15;
    }

    /* Generate finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL)
    {
        char *unit_id = tree->tree_data.unit_data.unit_id;
        char final_label[CODEGEN_LABEL_BUFFER_SIZE];
        snprintf(final_label, sizeof(final_label), "_UNIT_%s_FINAL", unit_id ? unit_id : "UNKNOWN");

        int prev_callee_rbx = ctx->callee_save_rbx_offset;
        int prev_callee_r12 = ctx->callee_save_r12_offset;
        int prev_callee_r13 = ctx->callee_save_r13_offset;
        int prev_callee_r14 = ctx->callee_save_r14_offset;
        int prev_callee_r15 = ctx->callee_save_r15_offset;
        push_stackscope();
        codegen_function_locals(tree->tree_data.unit_data.interface_var_decls, ctx, symtab);
        codegen_function_locals(tree->tree_data.unit_data.implementation_var_decls, ctx, symtab);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.interface_const_decls);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.implementation_const_decls);
        {
            StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
            StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
            StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
            StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
            StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
            ctx->callee_save_rbx_offset = rbx_slot->offset;
            ctx->callee_save_r12_offset = r12_slot->offset;
            ctx->callee_save_r13_offset = r13_slot->offset;
            ctx->callee_save_r14_offset = r14_slot->offset;
            ctx->callee_save_r15_offset = r15_slot->offset;
        }
        ListNode_t *inst_list = NULL;
        inst_list = codegen_stmt(tree->tree_data.unit_data.finalization, inst_list, ctx, symtab);

        fprintf(ctx->output_file, "\t.globl\t%s\n", final_label);
        fprintf(ctx->output_file, "%s:\n", final_label);
        fprintf(ctx->output_file, "\tpushq\t%%rbp\n");
        fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rbp\n");
        codegen_stack_space(ctx);
        codegen_inst_list(inst_list, ctx);
        if (ctx->callee_save_rbx_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
        if (ctx->callee_save_r12_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
        if (ctx->callee_save_r13_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
        if (ctx->callee_save_r14_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
        if (ctx->callee_save_r15_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
        fprintf(ctx->output_file, "\tleave\n");
        fprintf(ctx->output_file, "\tret\n");

        free_inst_list(inst_list);
        pop_stackscope();
        ctx->callee_save_rbx_offset = prev_callee_rbx;
        ctx->callee_save_r12_offset = prev_callee_r12;
        ctx->callee_save_r13_offset = prev_callee_r13;
        ctx->callee_save_r14_offset = prev_callee_r14;
        ctx->callee_save_r15_offset = prev_callee_r15;
    }

    codegen_program_footer(ctx);

    if (ctx->emitted_subprograms != NULL)
    {
        DestroyList(ctx->emitted_subprograms);
        ctx->emitted_subprograms = NULL;
    }
    if (g_codegen_available_subprograms != NULL)
    {
        DestroyList(g_codegen_available_subprograms);
        g_codegen_available_subprograms = NULL;
    }
    codegen_set_destroy(&g_codegen_callable_exports);

    free_stackmng();
    codegen_reset_loop_stack(ctx);
    codegen_reset_finally_stack(ctx);
    codegen_reset_except_stack(ctx);
    codegen_reset_with_stack(ctx);
}

static int codegen_is_valid_asm_symbol_name(const char *id)
{
    if (id == NULL || id[0] == '\0')
        return 0;

    if (!(isalpha((unsigned char)id[0]) || id[0] == '_'))
        return 0;

    for (size_t i = 1; id[i] != '\0'; ++i)
    {
        if (!(isalnum((unsigned char)id[i]) || id[i] == '_'))
            return 0;
    }

    return 1;
}

static int codegen_const_symbol_emitted(ListNode_t *emitted_symbols, const char *id)
{
    ListNode_t *cur = emitted_symbols;
    while (cur != NULL)
    {
        if (cur->cur != NULL && strcmp((const char *)cur->cur, id) == 0)
            return 1;
        cur = cur->next;
    }
    return 0;
}

static int codegen_should_emit_const_equiv_symbol(CodeGenContext *ctx,
    HashTable_t *table, const HashNode_t *symbol, const char *id)
{
    (void)ctx;
    (void)table;
    (void)symbol;
    if (id == NULL || !codegen_is_valid_asm_symbol_name(id))
        return 0;

    if (codegen_set_contains_ci(&g_codegen_callable_exports, id))
        return 0;

    return 1;
}

static void codegen_emit_integer_const_equivs_from_table(CodeGenContext *ctx,
    HashTable_t *table, ListNode_t **emitted_symbols)
{
    assert(ctx != NULL);
    assert(emitted_symbols != NULL);

    if (table == NULL)
        return;

    for (int bucket = 0; bucket < TABLE_SIZE; ++bucket)
    {
        ListNode_t *bucket_node = table->table[bucket];
        while (bucket_node != NULL)
        {
            HashNode_t *symbol = (HashNode_t *)bucket_node->cur;
            if (symbol != NULL &&
                symbol->hash_type == HASHTYPE_CONST &&
                symbol->is_constant &&
                symbol->const_string_value == NULL &&
                symbol->const_set_value == NULL &&
                symbol->id != NULL &&
                codegen_should_emit_const_equiv_symbol(ctx, table, symbol, symbol->id) &&
                !codegen_const_symbol_emitted(*emitted_symbols, symbol->id))
            {
                int type_tag = codegen_tag_from_kgpc(symbol->type);
                if (is_ordinal_type(type_tag))
                {
                    fprintf(ctx->output_file, ".equ %s, %lld\n", symbol->id, symbol->const_int_value);
                    char *emitted_copy = strdup(symbol->id);
                    if (emitted_copy != NULL)
                    {
                        ListNode_t *new_node = CreateListNode(emitted_copy, LIST_STRING);
                        if (*emitted_symbols == NULL)
                            *emitted_symbols = new_node;
                        else
                            *emitted_symbols = PushListNodeBack(*emitted_symbols, new_node);
                    }
                }
            }
            bucket_node = bucket_node->next;
        }
    }
}

static void codegen_emit_integer_const_equivs(CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(ctx != NULL);
    assert(symtab != NULL);

    ListNode_t *emitted_symbols = NULL;

    /* Prefer user/global scopes first, then builtins. */
    ScopeNode *scope_node = symtab->current_scope;
    while (scope_node != NULL)
    {
        codegen_emit_integer_const_equivs_from_table(ctx, scope_node->table,
            &emitted_symbols);
        scope_node = scope_node->parent;
    }
    codegen_emit_integer_const_equivs_from_table(ctx, symtab->builtin_scope->table, &emitted_symbols);

    ListNode_t *cur = emitted_symbols;
    while (cur != NULL)
    {
        free(cur->cur);
        cur = cur->next;
    }
    DestroyList(emitted_symbols);
}

static void codegen_emit_local_const_equivs(CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (symtab->current_scope == NULL || symtab->current_scope->table == NULL)
        return;

    ListNode_t *emitted_symbols = NULL;
    codegen_emit_integer_const_equivs_from_table(ctx, symtab->current_scope->table,
        &emitted_symbols);

    ListNode_t *cur = emitted_symbols;
    while (cur != NULL)
    {
        free(cur->cur);
        cur = cur->next;
    }
    DestroyList(emitted_symbols);
}

static int codegen_eval_const_expr(struct Expression *expr, SymTab_t *symtab, long long *out_value)
{
    if (expr == NULL || out_value == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_INUM:
            *out_value = expr->expr_data.i_num;
            return 1;
        case EXPR_BOOL:
            *out_value = expr->expr_data.bool_value ? 1 : 0;
            return 1;
        case EXPR_CHAR_CODE:
            *out_value = (unsigned char)(expr->expr_data.char_code & 0xFF);
            return 1;
        case EXPR_VAR_ID:
            if (symtab != NULL && expr->expr_data.id != NULL)
            {
                HashNode_t *node = NULL;
                if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 &&
                    node != NULL &&
                    (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                {
                    *out_value = node->const_int_value;
                    return 1;
                }
            }
            return 0;
        case EXPR_SIGN_TERM:
            if (expr->expr_data.sign_term != NULL)
            {
                long long inner;
                if (codegen_eval_const_expr(expr->expr_data.sign_term, symtab, &inner))
                {
                    *out_value = -inner;
                    return 1;
                }
            }
            return 0;
        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
                return codegen_eval_const_expr(expr->expr_data.typecast_data.expr, symtab, out_value);
            return 0;
        case EXPR_ADDOP:
        {
            long long left, right;
            if (codegen_eval_const_expr(expr->expr_data.addop_data.left_expr, symtab, &left) &&
                codegen_eval_const_expr(expr->expr_data.addop_data.right_term, symtab, &right))
            {
                switch (expr->expr_data.addop_data.addop_type)
                {
                    case PLUS:
                        *out_value = left + right;
                        return 1;
                    case MINUS:
                        *out_value = left - right;
                        return 1;
                    case OR:
                        *out_value = left | right;
                        return 1;
                    case XOR:
                        *out_value = left ^ right;
                        return 1;
                }
            }
            return 0;
        }
        case EXPR_MULOP:
        {
            long long left, right;
            if (codegen_eval_const_expr(expr->expr_data.mulop_data.left_term, symtab, &left) &&
                codegen_eval_const_expr(expr->expr_data.mulop_data.right_factor, symtab, &right))
            {
                switch (expr->expr_data.mulop_data.mulop_type)
                {
                    case STAR:
                        *out_value = left * right;
                        return 1;
                    case SLASH:
                        return 0;
                    case DIV:
                        if (right != 0)
                        {
                            *out_value = left / right;
                            return 1;
                        }
                        return 0;
                    case MOD:
                        if (right != 0)
                        {
                            *out_value = left % right;
                            return 1;
                        }
                        return 0;
                    case AND:
                        *out_value = left & right;
                        return 1;
                    case SHL:
                        *out_value = left << right;
                        return 1;
                    case SHR:
                        *out_value = (unsigned long long)left >> right;
                        return 1;
                }
            }
            return 0;
        }
        default:
            return 0;
    }
}

static void codegen_emit_const_decl_equivs_from_list(CodeGenContext *ctx, ListNode_t *const_decls)
{
    assert(ctx != NULL);

    if (const_decls == NULL)
        return;

    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_CONST_DECL)
            continue;

        const char *id = decl->tree_data.const_decl_data.id;
        struct Expression *value = decl->tree_data.const_decl_data.value;

        if (id == NULL || value == NULL)
            continue;

        if (!codegen_should_emit_const_equiv_symbol(ctx, NULL, NULL, id))
            continue;

        if (ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, (char *)id) != 0 &&
                node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                int type_tag = codegen_tag_from_kgpc(node->type);
                if (!is_ordinal_type(type_tag))
                    continue;
            }
        }

        long long const_value = 0;
        if (codegen_eval_const_expr(value, ctx != NULL ? ctx->symtab : NULL, &const_value))
            fprintf(ctx->output_file, ".equ %s, %lld\n", id, const_value);
    }
}

static void codegen_register_owner_unit_scope(CodeGenContext *ctx,
    SymTab_t *symtab, int source_unit_index)
{
    if (ctx == NULL || symtab == NULL || ctx->comp_ctx == NULL || source_unit_index <= 0)
        return;

    LoadedUnit *loaded_unit = compilation_context_find_unit(ctx->comp_ctx, source_unit_index);
    if (loaded_unit == NULL || loaded_unit->unit_tree == NULL ||
        loaded_unit->unit_tree->type != TREE_UNIT)
        return;

    Tree_t *unit = loaded_unit->unit_tree;
    ScopeNode *saved_scope = symtab->current_scope;
    int saved_unit_index = symtab->current_unit_index;
    ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, source_unit_index);
    if (unit_scope != NULL)
        symtab->current_scope = unit_scope;
    symtab->current_unit_index = source_unit_index;

    codegen_register_type_enum_literals(unit->tree_data.unit_data.interface_type_decls, symtab);
    codegen_register_type_enum_literals(unit->tree_data.unit_data.implementation_type_decls, symtab);
    codegen_register_decl_list(unit->tree_data.unit_data.interface_var_decls, symtab, 0);
    codegen_register_decl_list(unit->tree_data.unit_data.implementation_var_decls, symtab, 0);
    codegen_register_const_decls(unit->tree_data.unit_data.interface_const_decls, symtab);
    codegen_register_const_decls(unit->tree_data.unit_data.implementation_const_decls, symtab);

    symtab->current_scope = saved_scope;
    symtab->current_unit_index = saved_unit_index;
}

void codegen_rodata(CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    fprintf(ctx->output_file, ".format_str_s:\n");
    fprintf(ctx->output_file, ".string \"%%s\"\n");
    fprintf(ctx->output_file, ".format_str_d:\n");
    fprintf(ctx->output_file, ".string \"%%d\"\n");
    fprintf(ctx->output_file, ".format_str_c:\n");
    fprintf(ctx->output_file, ".string \"%%c\"\n");
    fprintf(ctx->output_file, ".format_str_lld:\n");
    if (codegen_target_is_windows())
    {
        fprintf(ctx->output_file, ".string \"%%lld\"\n");
    }
    else
    {
        fprintf(ctx->output_file, ".string \"%%ld\"\n");
    }
  
    fprintf(ctx->output_file, ".format_str_sn:\n");
    fprintf(ctx->output_file, ".string \"%%s\\n\"\n");
    fprintf(ctx->output_file, ".format_str_dn:\n");
    fprintf(ctx->output_file, ".string \"%%d\\n\"\n");
    fprintf(ctx->output_file, ".format_str_n:\n");
    fprintf(ctx->output_file, ".string \"\\n\"\n");
    fprintf(ctx->output_file, ".text\n");
    codegen_emit_integer_const_equivs(ctx, symtab);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Check whether a method uses the SRET calling convention (returns a
 * value >8 bytes via a hidden first pointer argument).  This shifts
 * Self from the first to the second argument register. */
static int codegen_method_uses_sret(CodeGenContext *ctx, SymTab_t *symtab,
    const char *owner_name, const char *fallback_owner, const char *method_name)
{
    char lookup_name[512];
    snprintf(lookup_name, sizeof(lookup_name), "%s__%s", owner_name, method_name);
    HashNode_t *method_sym = NULL;
    FindSymbol(&method_sym, symtab, lookup_name);
    if (method_sym == NULL && fallback_owner != NULL) {
        snprintf(lookup_name, sizeof(lookup_name), "%s__%s", fallback_owner, method_name);
        FindSymbol(&method_sym, symtab, lookup_name);
    }
    if (method_sym == NULL || method_sym->type == NULL)
        return 0;
    KgpcType *ret_type = kgpc_type_get_return_type(method_sym->type);
    if (ret_type == NULL)
        return 0;
    if (kgpc_type_is_shortstring(ret_type))
        return 1;
    if (kgpc_type_is_record(ret_type)) {
        long long ret_size = 0;
        struct RecordType *ret_rec = kgpc_type_get_record(ret_type);
        if (ret_rec != NULL &&
            codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                ret_rec, &ret_size) == 0 && ret_size > 8)
            return 1;
    }
    return 0;
}

static void codegen_emit_class_vmt(CodeGenContext *ctx, SymTab_t *symtab,
    struct RecordType *record_info, const char *class_label,
    EmittedClassSet *emitted_classes)
{
    if (record_info == NULL || !record_type_is_class(record_info) || class_label == NULL)
        return;

    if (emitted_class_set_contains(emitted_classes, class_label))
        return;

    if (emitted_class_set_add(emitted_classes, class_label) != 0)
        return;

    /* Emit FPC-compatible interface table (tinterfacetable) if this class
     * implements interfaces.  Layout per FPC objpash.inc:
     *   tinterfacetable: EntryCount (sizeuint=8), then entries (40 bytes each)
     *   tinterfaceentry: IIDRef(^pguid,8) VTable(8) IOffset(8)|IOffsetAsCodePtr(8)+IIDStrRef(8)+IType(4)+pad(4)
     * For each interface, emit standalone GUID constant + pointer indirection:
     *   __kgpc_guid_<Name>     = 16-byte GUID data
     *   __kgpc_guidref_<Name>  = pointer to __kgpc_guid_<Name>  (pguid)
     * The entry's IIDRef field points to __kgpc_guidref_<Name>. */
    int actual_iface_count = 0;
    const char **effective_iface_names = (const char **)record_info->interface_names;
    int effective_iface_count = record_info->num_interfaces;
    const char *dbg = getenv("KGPC_DEBUG_EMIT_INTERFACES");
    if (dbg != NULL && class_label != NULL &&
        (strcasecmp(class_label, "TList") == 0 ||
         strcasecmp(class_label, "TStringList") == 0 ||
         strcasecmp(class_label, "TComponent") == 0 ||
         strcasecmp(class_label, "TInterfaceList") == 0)) {
        fprintf(stderr,
            "[KGPC] emit class vmt %s rec=%p direct_ifaces=%d parent=%s methods=%d props=%d\n",
            class_label, (void *)record_info, record_info->num_interfaces,
            record_info->parent_class_name != NULL ? record_info->parent_class_name : "(null)",
            ListLength(record_info->method_templates), ListLength(record_info->properties));
        for (int i = 0; i < record_info->num_interfaces; i++) {
            fprintf(stderr, "[KGPC]   direct iface %s\n",
                record_info->interface_names[i] != NULL ? record_info->interface_names[i] : "(null)");
        }
    }
    int free_effective_iface_names = 0;
    long long base_instance_size = 0;
    if (effective_iface_count > 0) {
        /* First pass: emit standalone GUID constants for each interface
         * (deduplicated via emitted_classes set with "__kgpc_guid_" prefix). */
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            const char *iface_name = effective_iface_names[iidx];
            if (iface_name == NULL) continue;
            /* Check if we already emitted this interface's GUID */
            char guid_dedup_buf[512];
            snprintf(guid_dedup_buf, sizeof(guid_dedup_buf), "__kgpc_guid_%s", iface_name);
            if (emitted_class_set_contains(emitted_classes, guid_dedup_buf))
                continue;
            /* The set stores pointers without copying, so strdup the key
             * to avoid dangling stack references. */
            char *guid_dedup_key = strdup(guid_dedup_buf);
            if (guid_dedup_key == NULL) continue;
            emitted_class_set_add(emitted_classes, guid_dedup_key);
            /* Look up the interface type to get its GUID.
             * Use FindAllIdents to handle forward-declared interfaces where
             * the forward decl (without GUID) and full decl (with GUID) are
             * separate symbol table entries.  Prefer the one with has_guid. */
            struct RecordType *iface_record =
                codegen_lookup_record_type_by_name(symtab, iface_name, 1);
            uint32_t d1 = 0;
            uint16_t d2 = 0, d3 = 0;
            unsigned char d4[8] = {0};
            if (iface_record != NULL)
                codegen_resolve_record_guid(symtab, iface_record, &d1, &d2, &d3, d4);
            /* Emit GUID data constant (deduplicated within this TU by emitted_classes set).
             * Use .globl everywhere; dedup is handled by the emitted_classes set within
             * the single TU, and on COFF by .linkonce discard sections. */
            int is_win = codegen_target_is_windows();
            fprintf(ctx->output_file, "\n# GUID constant for interface %s\n", iface_name);
            if (is_win) {
                fprintf(ctx->output_file, "\t.section\t.rdata$__kgpc_guid_%s,\"dr\"\n", iface_name);
                fprintf(ctx->output_file, "\t.linkonce discard\n");
            } else {
                fprintf(ctx->output_file, "\t.data\n");
            }
            fprintf(ctx->output_file, "\t.align 8\n");
            fprintf(ctx->output_file, ".globl __kgpc_guid_%s\n", iface_name);
            fprintf(ctx->output_file, "__kgpc_guid_%s:\n", iface_name);
            fprintf(ctx->output_file, "\t.long\t0x%08X\n", d1);
            fprintf(ctx->output_file, "\t.short\t0x%04X\n", (unsigned)d2);
            fprintf(ctx->output_file, "\t.short\t0x%04X\n", (unsigned)d3);
            fprintf(ctx->output_file, "\t.byte\t0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X\n",
                d4[0], d4[1], d4[2], d4[3], d4[4], d4[5], d4[6], d4[7]);
            /* Emit pguid pointer */
            if (is_win) {
                fprintf(ctx->output_file, "\t.section\t.rdata$__kgpc_guidref_%s,\"dr\"\n", iface_name);
                fprintf(ctx->output_file, "\t.linkonce discard\n");
            }
            fprintf(ctx->output_file, "\t.align 8\n");
            fprintf(ctx->output_file, ".globl __kgpc_guidref_%s\n", iface_name);
            fprintf(ctx->output_file, "__kgpc_guidref_%s:\n", iface_name);
            fprintf(ctx->output_file, "\t.quad\t__kgpc_guid_%s\n", iface_name);
            fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
        }

        /* Count valid interfaces */
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            if (effective_iface_names[iidx] != NULL)
                actual_iface_count++;
        }

        /* Compute base instance size early — needed for IOffset values in the
         * interface table and for thunk adjustment.  codegen_sizeof_record_type
         * only counts fields in this record's field list; for classes with
         * parent classes whose fields were NOT merged into this record (common
         * when the record comes from a cached unit), we must add the parent
         * class size explicitly. */
        codegen_sizeof_record_type(ctx, record_info, &base_instance_size);
        if (record_info->parent_class_name != NULL) {
            /* Check if parent fields are included in the field list by
             * comparing against the parent's own size.  If the parent has
             * a larger base size than our field-only size, the parent
             * fields aren't merged; use parent_size + own_members instead. */
            HashNode_t *parent_cls_node = NULL;
            struct RecordType *parent_cls_rec = NULL;
            if (FindSymbol(&parent_cls_node, symtab, record_info->parent_class_name) != 0 &&
                parent_cls_node != NULL) {
                parent_cls_rec = get_record_type_from_node(parent_cls_node);
                if (parent_cls_rec == NULL && parent_cls_node->type != NULL &&
                    parent_cls_node->type->kind == TYPE_KIND_POINTER &&
                    parent_cls_node->type->info.points_to != NULL &&
                    parent_cls_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                    parent_cls_rec = parent_cls_node->type->info.points_to->info.record_info;
            }
            if (parent_cls_rec != NULL) {
                long long parent_base = 0;
                codegen_sizeof_record_type(ctx, parent_cls_rec, &parent_base);
                if (parent_base > 8) {
                    /* Add parent's fields to the child's base size.
                     * own_members = base_instance_size - 8 (VMT already
                     * counted in parent_base).  Total = parent_base +
                     * own_members, but only if the result is larger than
                     * the current base (to avoid shrinking when parent
                     * fields WERE already merged). */
                    long long own_members = base_instance_size - 8;
                    if (own_members < 0) own_members = 0;
                    long long own_start = parent_base;
                    if (own_start % 8 != 0)
                        own_start = (own_start + 7) & ~7LL;
                    long long new_base = own_start + own_members;
                    if (new_base > base_instance_size)
                        base_instance_size = new_base;
                }
            }
        }

        /* Emit FPC-compatible tinterfacetable */
        fprintf(ctx->output_file, "\n# Interface table (tinterfacetable) for class %s\n", class_label);
        fprintf(ctx->output_file, "\t.data\n");
        fprintf(ctx->output_file, "\t.align 8\n");
        fprintf(ctx->output_file, "%s_INTFTABLE:\n", class_label);
        fprintf(ctx->output_file, "\t.quad\t%d\t# EntryCount\n", actual_iface_count);
        int iface_slot_idx = 0;
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            const char *iface_name = effective_iface_names[iidx];
            if (iface_name == NULL) continue;
            long long ioffset = base_instance_size + iface_slot_idx * 8;
            fprintf(ctx->output_file, "\t# Entry for %s (40 bytes = tinterfaceentry)\n", iface_name);
            /* offset +0: IIDRef (^pguid) — pointer to the pguid indirection cell */
            fprintf(ctx->output_file, "\t.quad\t__kgpc_guidref_%s\n", iface_name);
            /* offset +8: VTable — pointer to interface vtable for this class */
            fprintf(ctx->output_file, "\t.quad\t%s_INTF_%s_VTABLE\n", class_label, iface_name);
            /* offset +16: IOffset (sizeuint) — byte offset from object start to interface slot */
            fprintf(ctx->output_file, "\t.quad\t%lld\n", ioffset);
            /* offset +24: IIDStrRef (^pshortstring) — NULL for now */
            fprintf(ctx->output_file, "\t.quad\t0\n");
            /* offset +32: IType (tinterfaceentrytype enum, 4 bytes) = etStandard = 0 */
            fprintf(ctx->output_file, "\t.long\t0\n");
            /* offset +36: padding to 40 bytes */
            fprintf(ctx->output_file, "\t.zero\t4\n");
            iface_slot_idx++;
        }
        fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

        /* Emit interface vtable arrays — one per interface this class implements.
         * Each vtable contains a .quad entry per method_template in the interface,
         * in declaration order, pointing to a thunk that adjusts Self back from
         * the interface pointer to the raw object pointer before jumping to the
         * implementing class method. */
        int vtbl_iface_slot_idx = 0;
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            const char *iface_name = effective_iface_names[iidx];
            if (iface_name == NULL) continue;
            long long ioffset_for_this_iface = base_instance_size + vtbl_iface_slot_idx * 8;
            vtbl_iface_slot_idx++;
            /* Look up the interface record to get its method_templates */
            struct RecordType *vtbl_iface_record =
                codegen_lookup_record_type_by_name(symtab, iface_name, 0);
            if (vtbl_iface_record == NULL) {
                /* No interface record — emit an empty vtable label */
                fprintf(ctx->output_file, "\n# Interface vtable for %s implementing %s (empty)\n", class_label, iface_name);
                fprintf(ctx->output_file, "\t.data\n");
                fprintf(ctx->output_file, "\t.align 8\n");
                fprintf(ctx->output_file, "%s_INTF_%s_VTABLE:\n", class_label, iface_name);
                fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
                continue;
            }
            fprintf(ctx->output_file, "\n# Interface vtable for %s implementing %s\n", class_label, iface_name);
            fprintf(ctx->output_file, "\t.data\n");
            fprintf(ctx->output_file, "\t.align 8\n");
            fprintf(ctx->output_file, "%s_INTF_%s_VTABLE:\n", class_label, iface_name);
            /* Iterate interface method_templates directly — inherited parent
             * methods were already prepended during semcheck. */
            ListNode_t *vtbl_iface_method = vtbl_iface_record->method_templates;
            while (vtbl_iface_method != NULL) {
                struct MethodTemplate *vtbl_imethod = (struct MethodTemplate *)vtbl_iface_method->cur;
                if (vtbl_imethod != NULL && vtbl_imethod->name != NULL) {
                    const char *vtbl_resolved_id = codegen_find_class_method_impl_id(
                        symtab, record_info, class_label, iface_name, vtbl_imethod->name);
                    if (vtbl_resolved_id != NULL) {
                        /* Emit thunk in .text that adjusts Self back from interface
                         * pointer to raw object pointer, then jumps to the real method.
                         * The thunk handles both adjusted interface pointers (Self =
                         * object + ioffset) and raw object pointers (Self = object)
                         * by checking if *(Self) is a VMT (vInstanceSize + vInstanceSize2
                         * == 0) or an interface vtable. */
                        char thunk_label[768];
                        snprintf(thunk_label, sizeof(thunk_label), "%s_INTF_%s_THUNK_%s",
                            class_label, iface_name, vtbl_imethod->name);
                        fprintf(ctx->output_file, "\t.text\n");
                        fprintf(ctx->output_file, "%s:\n", thunk_label);
                        /* If the method returns a large type (SRET), Self
                         * shifts from the first to the second arg register. */
                        int method_uses_sret = codegen_method_uses_sret(
                            ctx, symtab, iface_name, class_label, vtbl_imethod->name);
                        const char *self_reg = codegen_target_is_windows()
                            ? (method_uses_sret ? "%rdx" : "%rcx")
                            : (method_uses_sret ? "%rsi" : "%rdi");
                        /* Check if Self points to a VMT (raw object pointer) or
                         * an interface vtable (adjusted pointer).
                         * VMT has vInstanceSize at [0] and -vInstanceSize at [8],
                         * so their sum is 0.  Interface vtable entries are code
                         * pointers whose sum is extremely unlikely to be 0. */
                        fprintf(ctx->output_file, "\tmovq\t(%s), %%r11\n", self_reg);
                        fprintf(ctx->output_file, "\tmovq\t(%%r11), %%rax\n");
                        fprintf(ctx->output_file, "\taddq\t8(%%r11), %%rax\n");
                        fprintf(ctx->output_file, "\tjnz\t.L%s_adj\n", thunk_label);
                        /* Raw object pointer — no adjustment needed */
                        fprintf(ctx->output_file, "\tjmp\t%s\n", vtbl_resolved_id);
                        fprintf(ctx->output_file, ".L%s_adj:\n", thunk_label);
                        /* Adjusted interface pointer — subtract ioffset */
                        fprintf(ctx->output_file, "\tsubq\t$%lld, %s\n",
                            ioffset_for_this_iface, self_reg);
                        fprintf(ctx->output_file, "\tjmp\t%s\n", vtbl_resolved_id);
                        /* Switch back to data for the vtable entry */
                        fprintf(ctx->output_file, "\t.data\n");
                        fprintf(ctx->output_file, "\t.quad\t%s\t# %s\n", thunk_label, vtbl_imethod->name);
                    } else {
                        fprintf(ctx->output_file, "\t.quad\t__kgpc_abstract_method_error\t# %s (not implemented)\n", vtbl_imethod->name);
                    }
                }
                vtbl_iface_method = vtbl_iface_method->next;
            }
            fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
        }
    }

    /* Resolve the canonical label for the parent class (handles case mismatches
     * e.g. math.pp declares "EInvalidArgument = class(ematherror)" with lowercase,
     * but EMathError's VMT is emitted with its declaration-case label "EMathError"). */
    const char *parent_vmt_label = record_info->parent_class_name;
    if (parent_vmt_label != NULL) {
        HashNode_t *parent_node = NULL;
        if (FindSymbol(&parent_node, symtab, parent_vmt_label) != 0 && parent_node != NULL) {
            struct RecordType *parent_rec = get_record_type_from_node(parent_node);
            /* Only use the resolved type_id if it's actually a class. If FindIdent
             * resolved to a plain record (e.g. TTimeZone = timezone record alias
             * instead of TTimeZone = class abstract), keep the original name which
             * matches the class VMT label. */
            if (parent_rec != NULL && parent_rec->is_class && parent_rec->type_id != NULL)
                parent_vmt_label = parent_rec->type_id;
            else if (parent_rec != NULL && !parent_rec->is_class) {
                /* Resolved to a non-class record; keep original parent_class_name */
            } else if (parent_node->id != NULL)
                parent_vmt_label = parent_node->id;
        }
    }

    fprintf(ctx->output_file, "\n# RTTI for class %s\n", class_label);
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", class_label);
    fprintf(ctx->output_file, "%s_TYPEINFO:\n", class_label);
    if (record_info->parent_class_name != NULL)
        fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", parent_vmt_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");

    char name_label[256];
    snprintf(name_label, sizeof(name_label), "__kgpc_typeinfo_name_%s", class_label);
    fprintf(ctx->output_file, "\t.quad\t%s\n", name_label);
    /* Always emit VMT reference, even if no methods */
    fprintf(ctx->output_file, "\t.quad\t%s_VMT\n", class_label);
    /* Interface table pointer and count */
    if (actual_iface_count > 0)
        fprintf(ctx->output_file, "\t.quad\t%s_INTFTABLE\n", class_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");
    fprintf(ctx->output_file, "\t.quad\t%d\n", actual_iface_count);
    {
        char escaped_label[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_label, class_label, sizeof(escaped_label));
        fprintf(ctx->output_file, "%s:\n\t.string \"%s\"\n", name_label, escaped_label);
    }

    /* Emit class name as ShortString data for vClassName.
     * The generated TObject.ClassName body loads the slot once and then treats
     * the resulting address as a PShortString, so the VMT must point directly to
     * the ShortString payload rather than an intermediate pointer cell. */
    {
        char classname_ss_label[256];
        char escaped_classname[256];
        snprintf(classname_ss_label, sizeof(classname_ss_label),
            "__kgpc_vmt_classname_%s", class_label);
        escape_string(escaped_classname, class_label, sizeof(escaped_classname));
        fprintf(ctx->output_file, "%s:\n", classname_ss_label);
        fprintf(ctx->output_file, "\t.byte\t%d\n", (int)strlen(class_label));
        fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_classname);
    }

    /* Emit parent VMT reference storage for vParentRef (PPVmt) */
    if (record_info->parent_class_name != NULL) {
        fprintf(ctx->output_file, "\t.align 8\n");
        fprintf(ctx->output_file, "__kgpc_vmt_parentref_%s:\n", class_label);
        fprintf(ctx->output_file, "\t.quad\t%s_VMT\n", parent_vmt_label);
    }

    /* Compute instance size for vInstanceSize.
     * base_instance_size was computed earlier (before interface table emission)
     * when the class has interfaces.  For classes without interfaces, compute now. */
    if (base_instance_size == 0)
        codegen_sizeof_record_type(ctx, record_info, &base_instance_size);
    long long instance_size = base_instance_size;
    if (actual_iface_count > 0) {
        instance_size += actual_iface_count * 8;
        /* Update cached_size so constructor allocations use the new size */
        record_info->cached_size = instance_size;
        record_info->has_cached_size = 1;
    }
    /* Always emit VMT for classes, even if no virtual methods.
     * FPC VMT layout (TVmt record from objpash.inc):
     *   offset 0:  vInstanceSize      (SizeInt)
     *   offset 8:  vInstanceSize2     (SizeInt = -InstanceSize)
     *   offset 16: vParentRef         (PPVmt)
     *   offset 24: vClassName         (PShortString)
     *   offset 32: vDynamicTable      (Pointer)
     *   offset 40: vMethodTable       (Pointer)
     *   offset 48: vFieldTable        (Pointer)
     *   offset 56: vTypeInfo          (Pointer)
     *   offset 64: vInitTable         (Pointer)
     *   offset 72: vAutoTable         (Pointer)
     *   offset 80: vIntfTable         (PInterfaceTable)
     *   offset 88: vMsgStrPtr         (Pointer)
     *   offset 96+: virtual methods   (vmt_index 12+)
     */
    fprintf(ctx->output_file, "\n# VMT for class %s\n", class_label);
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_VMT\n", class_label);
    fprintf(ctx->output_file, "%s_VMT:\n", class_label);
    /* Slot 0: vInstanceSize */
    fprintf(ctx->output_file, "\t.quad\t%lld\n", instance_size);
    /* Slot 1: vInstanceSize2 = -InstanceSize */
    fprintf(ctx->output_file, "\t.quad\t%lld\n", -instance_size);
    /* Slot 2: vParentRef (PPVmt - pointer to location storing parent VMT pointer) */
    if (record_info->parent_class_name != NULL)
        fprintf(ctx->output_file, "\t.quad\t__kgpc_vmt_parentref_%s\n", class_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 3: vClassName (PShortString) */
    fprintf(ctx->output_file, "\t.quad\t__kgpc_vmt_classname_%s\n", class_label);
    /* Slot 4: vDynamicTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 5: vMethodTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 6: vFieldTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 7: vTypeInfo - point to our RTTI */
    fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", class_label);
    /* Slot 8: vInitTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 9: vAutoTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 10: vIntfTable (PInterfaceTable — FPC tinterfacetable) */
    if (actual_iface_count > 0)
        fprintf(ctx->output_file, "\t.quad\t%s_INTFTABLE\n", class_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 11: vMsgStrPtr */
    fprintf(ctx->output_file, "\t.quad\t0\n");

    /* Generic specializations can carry a cloned VMT from semcheck while the
     * actual specialized methods are only visible here by their emitted
     * symbols. Refresh matching virtual slots by method name before emitting
     * the final table so inherited slots point at the specialized overrides. */
    if (record_info->method_templates != NULL && record_info->methods != NULL &&
        class_label != NULL) {
        for (ListNode_t *tmpl_node = record_info->method_templates;
             tmpl_node != NULL; tmpl_node = tmpl_node->next) {
            if (tmpl_node->type != LIST_METHOD_TEMPLATE || tmpl_node->cur == NULL)
                continue;
            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_node->cur;
            if (tmpl->name == NULL || (!tmpl->is_virtual && !tmpl->is_override))
                continue;

            size_t base_len = strlen(class_label) + 2 + strlen(tmpl->name) + 1;
            char *base_name = (char *)malloc(base_len);
            if (base_name == NULL)
                continue;
            snprintf(base_name, base_len, "%s__%s", class_label, tmpl->name);

            const char *resolved_id = NULL;
            int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
            ListNode_t *matches = FindAllIdents(symtab, base_name);
            const char *fallback_id = NULL;
            for (ListNode_t *m = matches; m != NULL; m = m->next) {
                HashNode_t *cand = (HashNode_t *)m->cur;
                if (cand == NULL || cand->type == NULL ||
                    cand->type->kind != TYPE_KIND_PROCEDURE ||
                    cand->type->info.proc_info.definition == NULL)
                    continue;
                if (fallback_id == NULL) {
                    fallback_id = cand->mangled_id;
                    if (cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id != NULL)
                        fallback_id = cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id;
                }
                int count = ListLength(cand->type->info.proc_info.params);
                if (!tmpl->is_static && count > 0)
                    count -= 1;
                if (count != wanted_params)
                    continue;
                resolved_id = cand->mangled_id;
                if (cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id != NULL)
                    resolved_id = cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id;
                break;
            }
            if (matches != NULL)
                DestroyList(matches);
            free(base_name);

            if (resolved_id == NULL)
                resolved_id = fallback_id;
            if (resolved_id == NULL)
                continue;

            for (ListNode_t *method_node = record_info->methods;
                 method_node != NULL; method_node = method_node->next) {
                struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
                if (!codegen_template_matches_methodinfo(tmpl, method))
                    continue;
                if (method->resolved_mangled_id != NULL &&
                    method->resolved_mangled_id != method->mangled_name)
                    free(method->resolved_mangled_id);
                method->resolved_mangled_id = strdup(resolved_id);
                break;
            }
        }
    }

    /* Slots 12+: virtual methods (vmt_index * 8 gives correct offset) */
    if (record_info->methods != NULL) {
        ListNode_t *method_node = record_info->methods;
        while (method_node != NULL) {
            struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
            if (method != NULL && method->mangled_name != NULL) {
                const char *full_mangled = method->resolved_mangled_id;
                const char *fallback_mangled = method->mangled_name;
                const char *slot_label = NULL;
                if (full_mangled != NULL && g_codegen_available_subprograms != NULL &&
                    codegen_list_contains_string(g_codegen_available_subprograms, full_mangled))
                    slot_label = full_mangled;
                if (slot_label == NULL && fallback_mangled != NULL &&
                    g_codegen_available_subprograms != NULL &&
                    codegen_list_contains_string(g_codegen_available_subprograms, fallback_mangled))
                    slot_label = fallback_mangled;
                if (slot_label != NULL) {
                    fprintf(ctx->output_file, "\t.quad\t%s\n", slot_label);
                } else if (full_mangled != NULL) {
                    /* Not in available subprograms — check symtab for a real
                     * implementation (has statement_list).  This handles
                     * cross-unit methods while keeping abstract methods as
                     * error handlers. */
                    HashNode_t *sym = NULL;
                    int has_impl = 0;
                    if (FindSymbol(&sym, symtab, full_mangled) != 0 && sym != NULL &&
                        sym->type != NULL && sym->type->kind == TYPE_KIND_PROCEDURE &&
                        sym->type->info.proc_info.definition != NULL &&
                        sym->type->info.proc_info.definition->tree_data.subprogram_data.statement_list != NULL)
                        has_impl = 1;
                    if (has_impl)
                        fprintf(ctx->output_file, "\t.quad\t%s\n", full_mangled);
                    else
                        fprintf(ctx->output_file, "\t.quad\t__kgpc_abstract_method_error\n");
                } else {
                    fprintf(ctx->output_file, "\t.quad\t__kgpc_abstract_method_error\n");
                }
            }
            method_node = method_node->next;
        }
    }

    /* Emit interface method dispatch thunks.
     * For each interface a class directly implements, generate global symbols
     * for the interface method names that forward to the implementing class methods.
     * This enables interface method calls (e.g., FStream.Read(...)) to link
     * when emitted as direct calls to the interface method mangled name.
     *
     * Only direct implementers participate here. Inherited implementers reuse
     * the ancestor's interface entry points; emitting another global symbol for
     * the same interface method would not be well-defined.
     *
     * TODO: Replace with proper vtable-based interface dispatch for cases
     * where multiple classes implement the same interface. */
    if (record_info->num_interfaces > 0 && !record_info->is_interface) {
        for (int iidx = 0; iidx < record_info->num_interfaces; iidx++) {
            const char *iface_name = record_info->interface_names[iidx];
            if (iface_name == NULL) continue;
            /* Look up the interface to get its method list */
            struct RecordType *iface_record =
                codegen_lookup_record_type_by_name(symtab, iface_name, 0);
            if (iface_record == NULL) continue;
            /* Iterate interface method_templates directly — inherited parent
             * methods were already prepended during semcheck. */
            if (iface_record->method_templates == NULL) continue;
            ListNode_t *iface_method = iface_record->method_templates;
            while (iface_method != NULL) {
                struct MethodTemplate *imethod = (struct MethodTemplate *)iface_method->cur;
                if (imethod != NULL && imethod->name != NULL) {
                    const char *impl_resolved_id = codegen_find_class_method_impl_id(
                        symtab, record_info, class_label, iface_name, imethod->name);
                    /* Build the interface method mangled name: InterfaceName__MethodName */
                    char iface_base[512];
                    snprintf(iface_base, sizeof(iface_base), "%s__%s", iface_name, imethod->name);
                    /* Find the interface method's full mangled name */
                    ListNode_t *iface_candidates = FindAllIdents(symtab, iface_base);
                    HashNode_t *iface_func = NULL;
                    for (ListNode_t *ic = iface_candidates; ic != NULL; ic = ic->next) {
                        HashNode_t *cand = (HashNode_t *)ic->cur;
                        if (cand != NULL && cand->mangled_id != NULL &&
                            (cand->hash_type == HASHTYPE_FUNCTION ||
                             cand->hash_type == HASHTYPE_PROCEDURE)) {
                            iface_func = cand;
                            break;
                        }
                    }
                    if (iface_func != NULL && iface_func->mangled_id != NULL) {
                        /* Mark this interface method as handled so the
                         * symtab-wide abstract stub pass does not emit a
                         * duplicate definition. */
                        char stub_dedup[640];
                        snprintf(stub_dedup, sizeof(stub_dedup),
                                 "__kgpc_abstub_%s", iface_func->mangled_id);
                        if (emitted_class_set_contains(emitted_classes, stub_dedup)) {
                            /* Already emitted by a previous class — skip. */
                        } else {
                            char *stub_key = strdup(stub_dedup);
                            if (stub_key != NULL)
                                emitted_class_set_add(emitted_classes, stub_key);
                            /* If the class doesn't provide an implementation
                             * (e.g. TObject-derived class implementing an interface
                             * without inheriting TInterfacedObject), fall back to
                             * runtime default helpers for the IInterface trio. */
                            const char *final_target = impl_resolved_id;
                            if (final_target == NULL) {
                                if (strcasecmp(imethod->name, "QueryInterface") == 0)
                                    final_target = "kgpc_default_queryinterface";
                                else if (strcasecmp(imethod->name, "_AddRef") == 0)
                                    final_target = "kgpc_default_addref";
                                else if (strcasecmp(imethod->name, "_Release") == 0)
                                    final_target = "kgpc_default_release";
                                else
                                    codegen_assert_interface_impl_resolved(
                                        iface_name, imethod->name, class_label,
                                        iface_func->mangled_id, impl_resolved_id);
                            }
                            if (final_target != NULL) {
                                fprintf(ctx->output_file, "\n# Interface dispatch: %s.%s -> %s\n",
                                    iface_name, imethod->name, final_target);
                                codegen_emit_global_jump_stub(ctx,
                                    iface_func->mangled_id, final_target);
                            }
                        }
                    }
                    if (iface_candidates != NULL) DestroyList(iface_candidates);
                }
                iface_method = iface_method->next;
            }
        }
    }

    /* Emit writable storage for class vars. */
    if (record_type_is_class(record_info) || record_has_class_vars(record_info) ||
        record_has_class_method_templates(record_info) || record_has_method_decls(record_info))
    {
        int include_all_fields = (!record_has_class_vars(record_info) &&
            (record_has_class_method_templates(record_info) || record_has_method_decls(record_info)));
        long long class_var_size = codegen_class_var_storage_size(symtab, record_info,
            include_all_fields ? 1 : 0);
        if (class_var_size <= 0)
            class_var_size = 8;

        fprintf(ctx->output_file, "\n# Class variables for %s\n", class_label);
        fprintf(ctx->output_file, "\t.data\n");
        fprintf(ctx->output_file, "\t.align 8\n");
        fprintf(ctx->output_file, ".globl %s_CLASSVAR\n", class_label);
        fprintf(ctx->output_file, "%s_CLASSVAR:\n", class_label);

        /* For interfaces with GUIDs, store the 16-byte GUID as the class var
         * data.  The codegen references this via ClassName_CLASSVAR when passing
         * an interface type where a TGUID is expected. */
        uint32_t iface_d1 = 0;
        uint16_t iface_d2 = 0, iface_d3 = 0;
        uint8_t iface_d4[8] = {0};
        if (record_info->is_interface &&
            codegen_resolve_record_guid(symtab, record_info, &iface_d1, &iface_d2, &iface_d3, iface_d4)) {
            fprintf(ctx->output_file, "\t.long\t0x%08lX\n",
                (unsigned long)iface_d1);
            fprintf(ctx->output_file, "\t.short\t0x%04X\n",
                (unsigned int)iface_d2);
            fprintf(ctx->output_file, "\t.short\t0x%04X\n",
                (unsigned int)iface_d3);
            fprintf(ctx->output_file, "\t.byte\t0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X\n",
                iface_d4[0], iface_d4[1], iface_d4[2], iface_d4[3],
                iface_d4[4], iface_d4[5], iface_d4[6], iface_d4[7]);
        } else {
            /* Emit per-field labels for class var fields */
            {
                long long offset = 0;
                ListNode_t *fn = record_info->fields;
                while (fn != NULL) {
                    if (fn->type == LIST_RECORD_FIELD && fn->cur != NULL) {
                        struct RecordField *f = (struct RecordField *)fn->cur;
                        if (f != NULL && (include_all_fields || f->is_class_var == 1)) {
                            int fsz = codegen_class_var_field_size(symtab, f);
                            int align = (fsz >= 8) ? 8 : ((fsz >= 4) ? 4 : 1);
                            long long aligned_off = (offset + align - 1) & ~(align - 1);
                            long long pad = aligned_off - offset;
                            if (pad > 0)
                                fprintf(ctx->output_file, "\t.zero\t%lld\n", pad);
                            if (f->name != NULL && f->is_class_var == 1) {
                                fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), f->name);
                                fprintf(ctx->output_file, "%s:\n", f->name);
                            }
                            fprintf(ctx->output_file, "\t.zero\t%d\n", fsz);
                            offset = aligned_off + fsz;
                        }
                    }
                    fn = fn->next;
                }
                if (offset < class_var_size)
                    fprintf(ctx->output_file, "\t.zero\t%lld\n", class_var_size - offset);
            }
        }

        fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    }

    if (free_effective_iface_names)
        free((void *)effective_iface_names);
}

static void codegen_emit_record_classvar_storage(CodeGenContext *ctx, SymTab_t *symtab,
    struct RecordType *record_info, const char *class_label,
    EmittedClassSet *emitted_classes)
{
    if (record_info == NULL || record_type_is_class(record_info) || class_label == NULL)
        return;

    int has_class_vars = record_has_class_vars(record_info);
    int has_class_methods = record_has_class_method_templates(record_info) ||
        record_has_method_decls(record_info);
    if (!has_class_vars && !has_class_methods)
        return;

    if (emitted_class_set_contains(emitted_classes, class_label))
        return;

    if (emitted_class_set_add(emitted_classes, class_label) != 0)
        return;

    long long class_var_size = codegen_class_var_storage_size(symtab, record_info,
        has_class_vars ? 0 : 1);
    if (class_var_size <= 0)
        class_var_size = 8;

    fprintf(ctx->output_file, "\n# Class var storage for record %s\n", class_label);
    fprintf(ctx->output_file, "\t.data\n");
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_CLASSVAR\n", class_label);
    /* Emit a weak alias from the bare type name to the _CLASSVAR label
       so that codegen references like "leaq HeapInc(%rip)" resolve. */
    fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), class_label);
    fprintf(ctx->output_file, "%s:\n", class_label);
    fprintf(ctx->output_file, "%s_CLASSVAR:\n", class_label);

    /* Emit per-field labels so inline asm can reference class vars by bare name.
       Walk fields and emit .globl + label at each class var's offset. */
    {
        int include_all = has_class_vars ? 0 : 1;
        long long offset = 0;
        ListNode_t *fn = record_info->fields;
        while (fn != NULL) {
            if (fn->type == LIST_RECORD_FIELD && fn->cur != NULL) {
                struct RecordField *f = (struct RecordField *)fn->cur;
                if (f != NULL && (include_all || f->is_class_var == 1)) {
                    int fsz = codegen_class_var_field_size(symtab, f);
                    int align = (fsz >= 8) ? 8 : ((fsz >= 4) ? 4 : 1);
                    long long aligned_off = (offset + align - 1) & ~(align - 1);
                    long long pad = aligned_off - offset;
                    if (pad > 0)
                        fprintf(ctx->output_file, "\t.zero\t%lld\n", pad);
                    /* Emit a weak label with the bare field name, but only for
                       actual class vars (not regular fields that happen to be included). */
                    if (f->name != NULL && f->is_class_var == 1) {
                        fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), f->name);
                        fprintf(ctx->output_file, "%s:\n", f->name);
                    }
                    fprintf(ctx->output_file, "\t.zero\t%d\n", fsz);
                    offset = aligned_off + fsz;
                }
            }
            fn = fn->next;
        }
        /* Emit remaining padding if offset < class_var_size */
        if (offset < class_var_size)
            fprintf(ctx->output_file, "\t.zero\t%lld\n", class_var_size - offset);
    }

    /* Emit stub TYPEINFO/VMT symbols for advanced records.
     * This record was added to emitted_classes[], so the alias loop will
     * generate .set directives referencing these symbols (e.g., for pointer
     * type aliases like PPropInfo = ^TPropInfo).  Without these stubs,
     * strict linkers like ld.lld report undefined symbol errors. */
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", class_label);
    fprintf(ctx->output_file, "%s_TYPEINFO:\n", class_label);
    fprintf(ctx->output_file, "\t.quad\t0\n");  /* No parent class */
    fprintf(ctx->output_file, ".globl %s_VMT\n", class_label);
    fprintf(ctx->output_file, "%s_VMT:\n", class_label);
    fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", class_label);
}

static int codegen_should_emit_plain_record_typeinfo(const struct RecordType *record_info,
    const char *record_label)
{
    if (record_info == NULL || record_label == NULL || record_label[0] == '\0')
        return 0;
    if (record_info->type_id == NULL || record_info->type_id[0] == '\0')
        return 0;
    return strcmp(record_info->type_id, record_label) == 0;
}

static void codegen_emit_plain_record_typeinfo(CodeGenContext *ctx, const struct RecordType *record_info,
    const char *record_label, EmittedClassSet *emitted_classes)
{
    if (ctx == NULL || ctx->output_file == NULL || record_label == NULL)
        return;
    if (!codegen_should_emit_plain_record_typeinfo(record_info, record_label))
        return;
    if (emitted_class_set_contains(emitted_classes, record_label))
        return;
    if (emitted_class_set_add(emitted_classes, record_label) != 0)
        return;

    fprintf(ctx->output_file, "\n# TYPEINFO/VMT stubs for record %s\n", record_label);
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", record_label);
    fprintf(ctx->output_file, "%s_TYPEINFO:\n", record_label);
    fprintf(ctx->output_file, "\t.quad\t0\n");
    fprintf(ctx->output_file, ".globl %s_VMT\n", record_label);
    fprintf(ctx->output_file, "%s_VMT:\n", record_label);
    fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", record_label);
}

static int codegen_record_visible_field_count(const struct RecordType *record)
{
    int count = 0;
    if (record == NULL)
        return 0;
    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (!record_field_is_hidden(field))
            count++;
    }
    return count;
}

static int codegen_record_is_forward_stub(const struct RecordType *record)
{
    if (record == NULL)
        return 0;
    if (!record->is_class && !record->is_interface)
        return 0;
    if (record->parent_class_name != NULL)
        return 0;
    if (record->num_interfaces > 0)
        return 0;
    if (record->method_templates != NULL)
        return 0;
    if (record->properties != NULL)
        return 0;
    return codegen_record_visible_field_count(record) == 0;
}

static const struct RecordType *codegen_record_parent(const struct RecordType *record,
    SymTab_t *symtab)
{
    if (record == NULL || symtab == NULL || record->parent_class_name == NULL)
        return NULL;

    HashNode_t *best_node = codegen_pick_type_node_by_name(symtab, record->parent_class_name);
    return codegen_lookup_record_type_for_node(symtab, best_node, record->parent_class_name);
}


static const char *codegen_find_interface_delegate_target_name(
    const struct RecordType *record, const char *iface_name, const char *method_name)
{
    if (record == NULL || iface_name == NULL || method_name == NULL ||
        record->method_templates == NULL)
        return NULL;

    for (ListNode_t *cur = record->method_templates; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_METHOD_TEMPLATE || cur->cur == NULL)
            continue;
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (!tmpl->is_interface_delegation ||
            tmpl->delegated_interface_name == NULL ||
            tmpl->name == NULL ||
            tmpl->delegated_target_name == NULL)
            continue;
        if (strcasecmp(tmpl->delegated_interface_name, iface_name) == 0 &&
            strcasecmp(tmpl->name, method_name) == 0)
            return tmpl->delegated_target_name;
    }

    return NULL;
}

const char *codegen_find_class_method_impl_id(SymTab_t *symtab,
    const struct RecordType *record, const char *fallback_class_label,
    const char *iface_name, const char *method_name)
{
    const char *dbg_lookup = getenv("KGPC_DEBUG_METHOD_LOOKUP");
    const struct RecordType *cur_record = record;
    const char *cur_label = fallback_class_label;

    const struct RecordType *origin_record = record;

    while (cur_record != NULL || cur_label != NULL) {
        const char *owner_label = cur_label;
        if (cur_record != NULL && cur_record->type_id != NULL)
            owner_label = cur_record->type_id;
        if (owner_label == NULL)
            break;

        const char *lookup_method_name = method_name;
        if (cur_record != NULL && iface_name != NULL && cur_record == origin_record) {
            const char *delegate_target = codegen_find_interface_delegate_target_name(
                cur_record, iface_name, method_name);
            if (delegate_target != NULL)
                lookup_method_name = delegate_target;
        }

        char base_name[512];
        snprintf(base_name, sizeof(base_name), "%s__%s", owner_label, lookup_method_name);
        if (dbg_lookup != NULL &&
            fallback_class_label != NULL &&
            (strcasecmp(fallback_class_label, "TList") == 0 ||
             strcasecmp(fallback_class_label, "TComponent") == 0 ||
             strcasecmp(fallback_class_label, "TInterfaceList") == 0)) {
            fprintf(stderr, "[KGPC] lookup %s method %s via %s\n",
                fallback_class_label, lookup_method_name, base_name);
        }
        ListNode_t *impl_candidates = FindAllIdents(symtab, base_name);
        const char *resolved_id = NULL;
        Tree_t *resolved_def = NULL;
        for (ListNode_t *ic = impl_candidates; ic != NULL; ic = ic->next) {
            HashNode_t *cand = (HashNode_t *)ic->cur;
            if (cand == NULL || cand->mangled_id == NULL ||
                cand->type == NULL || cand->type->kind != TYPE_KIND_PROCEDURE ||
                cand->type->info.proc_info.definition == NULL)
                continue;
            Tree_t *def = cand->type->info.proc_info.definition;
            const char *emit_target = codegen_subprogram_emission_symbol(cand);
            if (emit_target == NULL)
                continue;
            if (dbg_lookup != NULL &&
                fallback_class_label != NULL &&
                (strcasecmp(fallback_class_label, "TList") == 0 ||
                 strcasecmp(fallback_class_label, "TComponent") == 0 ||
                 strcasecmp(fallback_class_label, "TInterfaceList") == 0)) {
                fprintf(stderr, "[KGPC]   cand %s emit=%s stmt=%p\n", cand->mangled_id,
                    emit_target,
                    (void *)cand->type->info.proc_info.definition->tree_data.subprogram_data.statement_list);
            }
            if (g_codegen_available_subprograms != NULL &&
                codegen_list_contains_string(g_codegen_available_subprograms, emit_target)) {
                resolved_id = emit_target;
                resolved_def = def;
                break;
            }
            if (def->tree_data.subprogram_data.statement_list != NULL) {
                resolved_id = emit_target;
                resolved_def = def;
                break;
            }
        }
        if (impl_candidates != NULL)
            DestroyList(impl_candidates);
        if (resolved_id != NULL) {
            if (resolved_def != NULL)
                resolved_def->tree_data.subprogram_data.is_used = 1;
            codegen_keep_subprogram_label(resolved_id);
            return resolved_id;
        }

        if (cur_record == NULL)
            break;
        cur_record = codegen_record_parent(cur_record, symtab);
        cur_label = NULL;
    }

    return NULL;
}

static int codegen_class_implements_interface(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const struct RecordType *iface_record)
{
    if (symtab == NULL || record == NULL || iface_record == NULL)
        return 0;

    /* method_templates already includes inherited parent methods (prepended
     * during semcheck), so iterate directly. */
    if (iface_record->method_templates == NULL)
        return 0;

    int result = 1;
    for (ListNode_t *cur = iface_record->method_templates; cur != NULL; cur = cur->next) {
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (tmpl == NULL || tmpl->name == NULL)
            continue;
        if (codegen_find_class_method_impl_id(symtab, record, class_label,
                iface_record->type_id, tmpl->name) == NULL) {
            result = 0;
            break;
        }
    }
    return result;
}

static void __attribute__((unused)) codegen_collect_inferred_interfaces(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const char ***out_names, int *out_count)
{
    *out_names = NULL;
    *out_count = 0;
    if (symtab == NULL || record == NULL || !record->is_class)
        return;

    int cap = 0;
    const char **names = NULL;

    const struct RecordType *parent = codegen_record_parent(record, symtab);
    if (parent != NULL && parent->num_interfaces > 0 &&
        parent->interface_names != NULL) {
        for (int i = 0; i < parent->num_interfaces; i++) {
            const char *iface = parent->interface_names[i];
            if (iface == NULL)
                continue;
            if (*out_count == cap) {
                cap = cap == 0 ? 8 : cap * 2;
                names = (const char **)realloc((void *)names, sizeof(char *) * cap);
            }
            names[*out_count] = iface;
            (*out_count)++;
        }
    }

    for (int unit_idx = 0; unit_idx < SYMTAB_MAX_UNITS; unit_idx++) {
        ScopeNode *scope = symtab->unit_scopes[unit_idx];
        HashTable_t *table = scope != NULL ? scope->table : NULL;
        if (table == NULL)
            continue;
        for (int b = 0; b < TABLE_SIZE; b++) {
            for (ListNode_t *node = table->table[b]; node != NULL; node = node->next) {
                HashNode_t *hash_node = (HashNode_t *)node->cur;
                if (hash_node == NULL || hash_node->hash_type != HASHTYPE_TYPE)
                    continue;
                struct RecordType *iface_record = get_record_type_from_node(hash_node);
                if (iface_record == NULL && hash_node->type != NULL &&
                    hash_node->type->kind == TYPE_KIND_POINTER &&
                    hash_node->type->info.points_to != NULL &&
                    hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                    iface_record = hash_node->type->info.points_to->info.record_info;
                if (iface_record == NULL || !iface_record->is_interface)
                    continue;
                const char *iface_name = iface_record->type_id != NULL ? iface_record->type_id : hash_node->id;
                if (iface_name == NULL)
                    continue;
                int already = 0;
                for (int i = 0; i < *out_count; i++) {
                    if (pascal_identifier_equals(names[i], iface_name)) {
                        already = 1;
                        break;
                    }
                }
                if (already)
                    continue;
                if (!codegen_class_implements_interface(symtab, record, class_label, iface_record))
                    continue;
                if (*out_count == cap) {
                    cap = cap == 0 ? 8 : cap * 2;
                    names = (const char **)realloc((void *)names, sizeof(char *) * cap);
                }
                names[*out_count] = iface_name;
                (*out_count)++;
            }
        }
    }

    *out_names = names;

    const char *dbg = getenv("KGPC_DEBUG_INFERRED_INTERFACES");
    if (dbg != NULL && class_label != NULL &&
        (strcasecmp(class_label, "TList") == 0 ||
         strcasecmp(class_label, "TStringList") == 0 ||
         strcasecmp(class_label, "TComponent") == 0 ||
         strcasecmp(class_label, "TInterfaceList") == 0)) {
        fprintf(stderr, "[KGPC] inferred interfaces for %s: %d\n",
            class_label, *out_count);
        for (int i = 0; i < *out_count; i++) {
            fprintf(stderr, "[KGPC]   %s\n", names[i] != NULL ? names[i] : "(null)");
        }
    }
}

static void codegen_canonicalize_record_for_emission(SymTab_t *symtab,
    const char **class_label, struct RecordType **record_info)
{
    if (symtab == NULL || class_label == NULL || *class_label == NULL)
        return;

    HashNode_t *best_node = codegen_pick_type_node_by_name(symtab, *class_label);
    struct RecordType *best_record =
        codegen_lookup_record_type_for_node(symtab, best_node, *class_label);
    if (best_record == NULL)
        return;

    *record_info = best_record;
    if (best_record->type_id != NULL)
        *class_label = best_record->type_id;
    else if (best_node != NULL && best_node->id != NULL)
        *class_label = best_node->id;
}

/* Helper: emit VMTs/RTTI for all type declarations in a list. */
static void codegen_vmt_from_type_list(CodeGenContext *ctx, SymTab_t *symtab,
                                        ListNode_t *type_decls,
                                        EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL) {
            if (codegen_type_decl_suppressed(type_tree))
            {
                cur = cur->next;
                continue;
            }
            struct RecordType *record_info = NULL;
            const char *class_label = NULL;

            if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
                record_info = codegen_record_from_type_decl_ex(type_tree, symtab);
                const char *type_name = type_tree->tree_data.type_decl_data.id;
                class_label = (record_info != NULL && record_info->type_id != NULL) ?
                    record_info->type_id : type_name;
            }
            else if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                record_info = codegen_record_from_type_decl_ex(type_tree, symtab);
                if (record_info != NULL && record_info->type_id != NULL) {
                    if (record_info->is_generic_specialization) {
                        class_label = record_info->type_id;
                    }
                }
            }

            codegen_canonicalize_record_for_emission(symtab, &class_label, &record_info);
            if (codegen_record_is_forward_stub(record_info)) {
                cur = cur->next;
                continue;
            }

            codegen_emit_class_vmt(ctx, symtab, record_info, class_label,
                emitted_classes);
            codegen_emit_record_classvar_storage(ctx, symtab, record_info, class_label,
                emitted_classes);
            if (record_info != NULL && !record_type_is_class(record_info) &&
                class_label != NULL && class_label[0] != '\0')
            {
                codegen_emit_plain_record_typeinfo(ctx, record_info, class_label, emitted_classes);
            }
        }
        cur = cur->next;
    }
}

static void codegen_emit_vmts_from_hash_table(CodeGenContext *ctx, SymTab_t *symtab,
    HashTable_t *table, EmittedClassSet *emitted_classes)
{
    if (table == NULL)
        return;

    for (int b = 0; b < TABLE_SIZE; b++)
    {
        ListNode_t *node = table->table[b];
        while (node != NULL)
        {
            HashNode_t *hash_node = (HashNode_t *)node->cur;
            if (hash_node != NULL && hash_node->hash_type == HASHTYPE_TYPE &&
                hash_node->type != NULL)
            {
                struct RecordType *record_info = NULL;
                const char *class_label = NULL;
                if (hash_node->type->kind == TYPE_KIND_RECORD)
                {
                    record_info = hash_node->type->info.record_info;
                }
                else if (hash_node->type->kind == TYPE_KIND_POINTER &&
                    hash_node->type->info.points_to != NULL &&
                    hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                {
                    record_info = hash_node->type->info.points_to->info.record_info;
                }
                if (record_info != NULL)
                    class_label = record_info->type_id;
                if (class_label == NULL)
                    class_label = hash_node->id;

                codegen_canonicalize_record_for_emission(symtab, &class_label, &record_info);
                if (codegen_record_is_forward_stub(record_info)) {
                    node = node->next;
                    continue;
                }

                codegen_emit_class_vmt(ctx, symtab, record_info, class_label,
                    emitted_classes);
                codegen_emit_record_classvar_storage(ctx, symtab, record_info, class_label,
                    emitted_classes);
                if (record_info != NULL && !record_type_is_class(record_info) &&
                    class_label != NULL && class_label[0] != '\0')
                {
                    codegen_emit_plain_record_typeinfo(ctx, record_info, class_label, emitted_classes);
                }
            }
            node = node->next;
        }
    }
}

/* Helper: emit abstract method stubs for old-style Pascal objects from type lists.
 * Old-style objects (not class, not interface) use direct calls, so virtual;abstract
 * methods need stubs jumping to __kgpc_abstract_method_error.
 * Uses the MethodInfo entries in record_info->methods to get the exact mangled names. */
static void codegen_emit_old_object_abstract_stubs_from_type_list(
    CodeGenContext *ctx, ListNode_t *type_decls, EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree == NULL || type_tree->type != TREE_TYPE_DECL ||
            codegen_type_decl_suppressed(type_tree)) {
            cur = cur->next;
            continue;
        }

        struct RecordType *record_info = codegen_record_from_type_decl(type_tree);
        if (record_info == NULL || record_info->is_interface ||
            record_type_is_class(record_info) || record_info->methods == NULL) {
            cur = cur->next;
            continue;
        }

        for (ListNode_t *method_node = record_info->methods;
             method_node != NULL; method_node = method_node->next) {
            struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
            if (method == NULL || method->mangled_name == NULL)
                continue;

            const char *mangled_id = method->mangled_name;

            /* Skip if a concrete implementation exists */
            if (g_codegen_available_subprograms != NULL &&
                codegen_list_contains_string(g_codegen_available_subprograms, mangled_id))
                continue;

            if (method->resolved_mangled_id != NULL &&
                g_codegen_available_subprograms != NULL &&
                codegen_list_contains_string(g_codegen_available_subprograms,
                                             method->resolved_mangled_id))
                continue;

            /* Deduplicate */
            char dedup_buf[1024];
            snprintf(dedup_buf, sizeof(dedup_buf), "__abstract_stub_%s", mangled_id);
            if (emitted_class_set_contains(emitted_classes, dedup_buf))
                continue;
            char *dedup_key = strdup(dedup_buf);
            if (dedup_key != NULL)
                emitted_class_set_add(emitted_classes, dedup_key);

            /* Emit abstract method stub — this is the correct implementation
             * for virtual;abstract methods in old-style objects.  The dedup
             * check above ensures we don't emit when a concrete impl exists. */
            fprintf(ctx->output_file,
                    "\n# Abstract method stub: %s\n", mangled_id);
            fprintf(ctx->output_file, "\t.text\n");
            fprintf(ctx->output_file, ".globl %s\n", mangled_id);
            fprintf(ctx->output_file, "%s:\n", mangled_id);
            fprintf(ctx->output_file, "\tjmp\t__kgpc_abstract_method_error\n");
        }
        cur = cur->next;
    }
}

/* Helper: emit GUID data for all interfaces with GUIDs in a type declaration list.
 * Used to emit GUIDs from loaded units whose interfaces are not in the local scope. */
static void codegen_emit_guids_from_type_list(CodeGenContext *ctx,
                                               ListNode_t *type_decls,
                                               EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL) {
            struct RecordType *record_info = codegen_record_from_type_decl(type_tree);
            uint32_t guid_d1 = 0;
            uint16_t guid_d2 = 0, guid_d3 = 0;
            uint8_t guid_d4[8] = {0};
            if (record_info != NULL && record_info->is_interface &&
                codegen_resolve_record_guid(ctx->symtab, record_info,
                    &guid_d1, &guid_d2, &guid_d3, guid_d4))
            {
                const char *iface_name = record_info->type_id;
                if (iface_name == NULL)
                    iface_name = type_tree->tree_data.type_decl_data.id;
                if (iface_name == NULL) { cur = cur->next; continue; }

                char guid_dedup_buf[512];
                snprintf(guid_dedup_buf, sizeof(guid_dedup_buf),
                         "__kgpc_guid_%s", iface_name);
                if (!emitted_class_set_contains(emitted_classes, guid_dedup_buf))
                {
                    char *guid_dedup_key = strdup(guid_dedup_buf);
                    if (guid_dedup_key != NULL)
                        emitted_class_set_add(emitted_classes, guid_dedup_key);

                    unsigned long d1 = (unsigned long)guid_d1;
                    unsigned int d2 = (unsigned int)guid_d2;
                    unsigned int d3 = (unsigned int)guid_d3;
                    unsigned char d4[8];
                    memcpy(d4, guid_d4, sizeof(d4));

                    int is_win = codegen_target_is_windows();
                    fprintf(ctx->output_file,
                            "\n# GUID constant for interface %s (from unit)\n",
                            iface_name);
                    if (is_win) {
                        fprintf(ctx->output_file,
                                "\t.section\t.rdata$__kgpc_guid_%s,\"dr\"\n",
                                iface_name);
                        fprintf(ctx->output_file, "\t.linkonce discard\n");
                    } else {
                        fprintf(ctx->output_file, "\t.data\n");
                    }
                    fprintf(ctx->output_file, "\t.align 8\n");
                    fprintf(ctx->output_file, ".globl __kgpc_guid_%s\n",
                            iface_name);
                    fprintf(ctx->output_file, "__kgpc_guid_%s:\n", iface_name);
                    fprintf(ctx->output_file, "\t.long\t0x%08lX\n", d1);
                    fprintf(ctx->output_file, "\t.short\t0x%04X\n", d2);
                    fprintf(ctx->output_file, "\t.short\t0x%04X\n", d3);
                    fprintf(ctx->output_file,
                            "\t.byte\t0x%02X, 0x%02X, 0x%02X, 0x%02X, "
                            "0x%02X, 0x%02X, 0x%02X, 0x%02X\n",
                            d4[0], d4[1], d4[2], d4[3],
                            d4[4], d4[5], d4[6], d4[7]);
                    /* Emit pguid pointer */
                    if (is_win) {
                        fprintf(ctx->output_file,
                                "\t.section\t.rdata$__kgpc_guidref_%s,\"dr\"\n",
                                iface_name);
                        fprintf(ctx->output_file, "\t.linkonce discard\n");
                    }
                    fprintf(ctx->output_file, "\t.align 8\n");
                    fprintf(ctx->output_file, ".globl __kgpc_guidref_%s\n",
                            iface_name);
                    fprintf(ctx->output_file, "__kgpc_guidref_%s:\n", iface_name);
                    fprintf(ctx->output_file, "\t.quad\t__kgpc_guid_%s\n",
                            iface_name);
                    fprintf(ctx->output_file, "%s\n",
                            codegen_readonly_section_directive());
                }
            }
        }
        cur = cur->next;
    }
}

/* Interface dispatch entry points must be emitted as concrete labels rather
 * than assembler aliases so COFF and ELF toolchains see the same symbols. */
static void codegen_emit_global_jump_stub(CodeGenContext *ctx,
    const char *exported_symbol, const char *target_symbol)
{
    if (ctx == NULL || exported_symbol == NULL || target_symbol == NULL)
        return;

    fprintf(ctx->output_file, "\t.text\n");
    fprintf(ctx->output_file, ".globl %s\n", exported_symbol);
    fprintf(ctx->output_file, "%s:\n", exported_symbol);
    fprintf(ctx->output_file, "\tjmp\t%s\n", target_symbol);
}

static void codegen_assert_interface_impl_resolved(const char *iface_name,
    const char *method_name, const char *class_label,
    const char *iface_symbol, const char *impl_symbol)
{
    if (iface_symbol == NULL || iface_symbol[0] == '\0')
        return;
    if (impl_symbol != NULL && impl_symbol[0] != '\0')
        return;

    fprintf(stderr,
        "[KGPC] unresolved interface dispatch: %s.%s for class %s (%s)\n",
        iface_name != NULL ? iface_name : "<interface>",
        method_name != NULL ? method_name : "<method>",
        class_label != NULL ? class_label : "<class>",
        iface_symbol);
    assert(0 && "unresolved interface dispatch target");
}

/* Helper: emit TYPEINFO/VMT aliases for type aliases pointing to class types. */
static void codegen_vmt_aliases_from_type_list(CodeGenContext *ctx,
                                                ListNode_t *type_decls,
                                                EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL &&
            !codegen_type_decl_suppressed(type_tree) &&
            type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
            const char *alias_name = type_tree->tree_data.type_decl_data.id;
            const char *target_name = type_tree->tree_data.type_decl_data.info.alias.target_type_id;
            if (alias_name != NULL && target_name != NULL) {
                int target_emitted = emitted_class_set_contains(emitted_classes, target_name);
                int alias_already_owned = emitted_class_set_contains(emitted_classes, alias_name);
                if (target_emitted && !alias_already_owned) {
                    fprintf(ctx->output_file, "\n# TYPEINFO alias: %s = %s\n", alias_name, target_name);
                    fprintf(ctx->output_file, "%s\t%s_TYPEINFO\n", codegen_weak_or_globl(), alias_name);
                    fprintf(ctx->output_file, "\t.set\t%s_TYPEINFO, %s_TYPEINFO\n", alias_name, target_name);
                    fprintf(ctx->output_file, "%s\t%s_VMT\n", codegen_weak_or_globl(), alias_name);
                    fprintf(ctx->output_file, "\t.set\t%s_VMT, %s_VMT\n", alias_name, target_name);
                    emitted_class_set_add(emitted_classes, alias_name);
                }
            }
        }
        cur = cur->next;
    }
}

/* Generate Virtual Method Tables (VMT) for classes with virtual methods.
 * Iterates type declarations from loaded units (via comp_ctx) and the program tree.
 * Uses a single EmittedClassSet to avoid duplicate emissions. */
void codegen_vmt(CodeGenContext *ctx, SymTab_t *symtab, Tree_t *tree,
                 CompilationContext *comp_ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    assert(symtab != NULL);
    assert(tree != NULL);

    /* RTTI metadata and VMTs are generated as read-only data structures */
    fprintf(ctx->output_file, "\n");
    fprintf(ctx->output_file, "# Class RTTI metadata and Virtual Method Tables (VMT)\n");
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

    EmittedClassSet emitted_classes = {0};

    /* Emit VMTs from loaded units first */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_vmt_from_type_list(ctx, symtab,
                    unit->tree_data.unit_data.interface_type_decls, &emitted_classes);
                codegen_vmt_from_type_list(ctx, symtab,
                    unit->tree_data.unit_data.implementation_type_decls, &emitted_classes);
            }
        }
    }

    /* Emit VMTs from the current compilation unit/program declarations.
     * When compiling a unit directly, it is not yet present in loaded_units,
     * so we must emit from the current TREE_UNIT explicitly. */
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_vmt_from_type_list(ctx, symtab,
            tree->tree_data.program_data.type_declaration, &emitted_classes);
    else if (tree->type == TREE_UNIT)
    {
        codegen_vmt_from_type_list(ctx, symtab,
            tree->tree_data.unit_data.interface_type_decls, &emitted_classes);
        codegen_vmt_from_type_list(ctx, symtab,
            tree->tree_data.unit_data.implementation_type_decls, &emitted_classes);
    }

    /* Also emit VMTs for class types that exist only in the symbol table
     * (e.g., specializations pulled in from units like FGL).
     * Restrict this fallback to whole-program codegen. Direct unit codegen
     * should emit from the unit's own declared types, not arbitrary symtab
     * entries that may include incomplete or transient records. */
    if (tree->type == TREE_PROGRAM_TYPE)
    {
        for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
        {
            codegen_emit_vmts_from_hash_table(ctx, symtab, scope->table, &emitted_classes);
        }

        for (int i = 0; i < SYMTAB_MAX_UNITS; i++)
        {
            ScopeNode *unit_scope = symtab->unit_scopes[i];
            if (unit_scope == NULL)
                continue;
            codegen_emit_vmts_from_hash_table(ctx, symtab, unit_scope->table, &emitted_classes);
        }
    }

    /* Emit GUID data for ALL interfaces with GUIDs found in the symbol table.
     * The previous passes only emit GUIDs when a class implements an interface,
     * but Supports() calls may reference interface GUIDs without any local class
     * implementing the interface.  This pass ensures all GUID symbols are defined. */
    for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
    {
        HashTable_t *table = scope->table;
        if (table == NULL)
            continue;
        for (int b = 0; b < TABLE_SIZE; b++)
        {
            ListNode_t *node = table->table[b];
            while (node != NULL)
            {
                HashNode_t *hash_node = (HashNode_t *)node->cur;
                if (hash_node != NULL && hash_node->hash_type == HASHTYPE_TYPE &&
                    hash_node->type != NULL)
                {
                    struct RecordType *record_info = NULL;
                    if (hash_node->type->kind == TYPE_KIND_RECORD)
                        record_info = hash_node->type->info.record_info;
                    else if (hash_node->type->kind == TYPE_KIND_POINTER &&
                             hash_node->type->info.points_to != NULL &&
                             hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                        record_info = hash_node->type->info.points_to->info.record_info;

                    uint32_t guid_d1 = 0;
                    uint16_t guid_d2 = 0, guid_d3 = 0;
                    uint8_t guid_d4[8] = {0};
                    if (record_info != NULL && record_info->is_interface &&
                        codegen_resolve_record_guid(symtab, record_info,
                            &guid_d1, &guid_d2, &guid_d3, guid_d4))
                    {
                        const char *iface_name = record_info->type_id;
                        if (iface_name == NULL)
                            iface_name = hash_node->id;
                        if (iface_name == NULL) { node = node->next; continue; }

                        char guid_dedup_buf[512];
                        snprintf(guid_dedup_buf, sizeof(guid_dedup_buf),
                                 "__kgpc_guid_%s", iface_name);
                        if (!emitted_class_set_contains(&emitted_classes, guid_dedup_buf))
                        {
                            char *guid_dedup_key = strdup(guid_dedup_buf);
                            if (guid_dedup_key != NULL)
                                emitted_class_set_add(&emitted_classes, guid_dedup_key);

                            unsigned long d1 = (unsigned long)guid_d1;
                            unsigned int d2 = (unsigned int)guid_d2;
                            unsigned int d3 = (unsigned int)guid_d3;
                            unsigned char d4[8];
                            memcpy(d4, guid_d4, sizeof(d4));

                            int is_win = codegen_target_is_windows();
                            fprintf(ctx->output_file,
                                    "\n# GUID constant for interface %s (standalone)\n",
                                    iface_name);
                            if (is_win) {
                                fprintf(ctx->output_file,
                                        "\t.section\t.rdata$__kgpc_guid_%s,\"dr\"\n",
                                        iface_name);
                                fprintf(ctx->output_file, "\t.linkonce discard\n");
                            } else {
                                fprintf(ctx->output_file, "\t.data\n");
                            }
                            fprintf(ctx->output_file, "\t.align 8\n");
                            fprintf(ctx->output_file, ".globl __kgpc_guid_%s\n",
                                    iface_name);
                            fprintf(ctx->output_file, "__kgpc_guid_%s:\n", iface_name);
                            fprintf(ctx->output_file, "\t.long\t0x%08lX\n", d1);
                            fprintf(ctx->output_file, "\t.short\t0x%04X\n", d2);
                            fprintf(ctx->output_file, "\t.short\t0x%04X\n", d3);
                            fprintf(ctx->output_file,
                                    "\t.byte\t0x%02X, 0x%02X, 0x%02X, 0x%02X, "
                                    "0x%02X, 0x%02X, 0x%02X, 0x%02X\n",
                                    d4[0], d4[1], d4[2], d4[3],
                                    d4[4], d4[5], d4[6], d4[7]);
                            /* Emit pguid pointer */
                            if (is_win) {
                                fprintf(ctx->output_file,
                                        "\t.section\t.rdata$__kgpc_guidref_%s,\"dr\"\n",
                                        iface_name);
                                fprintf(ctx->output_file, "\t.linkonce discard\n");
                            }
                            fprintf(ctx->output_file, "\t.align 8\n");
                            fprintf(ctx->output_file, ".globl __kgpc_guidref_%s\n",
                                    iface_name);
                            fprintf(ctx->output_file, "__kgpc_guidref_%s:\n", iface_name);
                            fprintf(ctx->output_file, "\t.quad\t__kgpc_guid_%s\n",
                                    iface_name);
                            fprintf(ctx->output_file, "%s\n",
                                    codegen_readonly_section_directive());
                        }
                    }
                }
                node = node->next;
            }
        }
    }

    /* Emit GUIDs from loaded units whose interfaces may not be in local scope */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_emit_guids_from_type_list(ctx,
                    unit->tree_data.unit_data.interface_type_decls, &emitted_classes);
                codegen_emit_guids_from_type_list(ctx,
                    unit->tree_data.unit_data.implementation_type_decls, &emitted_classes);
            }
        }
    }
    /* Also emit GUIDs from program type declarations */
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_emit_guids_from_type_list(ctx,
            tree->tree_data.program_data.type_declaration, &emitted_classes);

    /* Emit TYPEINFO/VMT aliases from loaded units and program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_vmt_aliases_from_type_list(ctx,
                    unit->tree_data.unit_data.interface_type_decls, &emitted_classes);
                codegen_vmt_aliases_from_type_list(ctx,
                    unit->tree_data.unit_data.implementation_type_decls, &emitted_classes);
            }
        }
    }
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_vmt_aliases_from_type_list(ctx,
            tree->tree_data.program_data.type_declaration, &emitted_classes);

    /* Emit abstract method stubs for old-style Pascal objects */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_emit_old_object_abstract_stubs_from_type_list(ctx,
                    unit->tree_data.unit_data.interface_type_decls,
                    &emitted_classes);
                codegen_emit_old_object_abstract_stubs_from_type_list(ctx,
                    unit->tree_data.unit_data.implementation_type_decls,
                    &emitted_classes);
            }
        }
    }
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_emit_old_object_abstract_stubs_from_type_list(ctx,
            tree->tree_data.program_data.type_declaration, &emitted_classes);

    emitted_class_set_destroy(&emitted_classes);

    fprintf(ctx->output_file, ".text\n");

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates platform-compatible headers */
void codegen_program_header(const char *fname, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(fname != NULL);
    assert(ctx != NULL);
    {
        char escaped_fname[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_fname, fname, sizeof(escaped_fname));
        fprintf(ctx->output_file, "\t.file\t\"%s\"\n", escaped_fname);
    }
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

    fprintf(ctx->output_file, "\t.text\n");
    fprintf(ctx->output_file, "\t.set\tKGPC_TARGET_WINDOWS, %d\n", codegen_target_is_windows());
    if (asm_debug_flag())
    {
        fputc('\n', ctx->output_file);
        codegen_emit_semantic_debug_block(ctx);
        fputc('\n', ctx->output_file);
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

/* Generates platform-compatible program footer */
void codegen_program_footer(CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    if (codegen_target_is_windows())
    {
        /* The COFF/PE assembler does not support .ident; omit it on Windows. */
    }
    else
    {
        fprintf(ctx->output_file, "\t.section\t.comment\n");
        fprintf(ctx->output_file, "\t.string\t\"KGPC: 0.0.0\"\n");
        fprintf(ctx->output_file, "\t.section\t.note.GNU-stack,\"\",@progbits\n");
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates main which calls our program */
void codegen_main(char *prgm_name, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(prgm_name != NULL);
    assert(ctx != NULL);
    int call_space;
    fprintf(ctx->output_file, "\t.section\t.text\n");
    fprintf(ctx->output_file, "\t.globl\tmain\n");
    codegen_function_header("main", ctx);
    call_space = codegen_target_is_windows() ? g_stack_home_space_bytes : 32;
    if (call_space > 0)
    {
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", call_space);
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_stackalloc\t%d\n", call_space);
    }
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_endprologue\n");
    if (codegen_target_is_windows())
    {
        fprintf(ctx->output_file, "\tcall\tkgpc_init_args\n");
    }
    else
    {
        fprintf(ctx->output_file, "\tcall\tkgpc_init_args\n");
    }
    fprintf(ctx->output_file, "\tcall\t%s\n", prgm_name);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\txor\t%%ecx, %%ecx\n");
    else
        fprintf(ctx->output_file, "\txor\t%%edi, %%edi\n");
    fprintf(ctx->output_file, "\tcall\texit\n");
    codegen_function_footer("main", ctx);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates code to allocate needed stack space */
void codegen_stack_space(CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int needed_space;

    assert(ctx != NULL);

    needed_space = get_full_stack_offset();
    assert(needed_space >= 0);

    int aligned_space = align_to_multiple(needed_space, REQUIRED_OFFSET);

    if(aligned_space != 0)
    {
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", aligned_space);
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_stackalloc\t%d\n", aligned_space);

        /* Zero-initialize the allocated stack space to ensure local variables start with zero values.
         * This is critical for code that assumes uninitialized variables are zero (like linked lists).
         * We use rep stosq for efficient zero-filling.
         *
         * Calling conventions differ between platforms:
         * - Windows x64: parameters in rcx, rdx, r8, r9
         * - System V AMD64 (Linux): parameters in rdi, rsi, rdx, rcx, r8, r9
         *
         * rep stosq uses rdi (destination), rax (value), rcx (count)
         * We need to save/restore these registers if they contain parameters.
         * r10 and r11 are caller-saved scratch registers safe to use on both platforms.
         */
        int quadwords = (aligned_space + 7) / 8;  /* Round up to nearest quadword */

        if (codegen_target_is_windows())
        {
            /* Windows x64 calling convention: rcx, rdx, r8, r9
             * rep stosq will clobber rcx and rdi.
             * Note: rdi is non-volatile (callee-saved) on Windows x64 ABI, so we must preserve it. */
            fprintf(ctx->output_file, "\tmovq\t%%rcx, %%r11\n");  /* Save rcx (1st param) to r11 */
            fprintf(ctx->output_file, "\tmovq\t%%rdi, %%r10\n");  /* Save rdi (callee-saved) to r10 */
            fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rdi\n");   /* rdi = stack pointer */
            fprintf(ctx->output_file, "\txorq\t%%rax, %%rax\n");   /* rax = 0 */
            fprintf(ctx->output_file, "\tmovl\t$%d, %%ecx\n", quadwords);  /* ecx = count */
            fprintf(ctx->output_file, "\trep stosq\n");            /* Zero-fill */
            fprintf(ctx->output_file, "\tmovq\t%%r10, %%rdi\n");  /* Restore rdi */
            fprintf(ctx->output_file, "\tmovq\t%%r11, %%rcx\n");  /* Restore rcx */
        }
        else
        {
            /* System V AMD64 (Linux) calling convention: rdi, rsi, rdx, rcx, r8, r9
             * rep stosq will clobber rdi, rcx */
            fprintf(ctx->output_file, "\tmovq\t%%rdi, %%r10\n");  /* Save rdi (1st param) to r10 */
            fprintf(ctx->output_file, "\tmovq\t%%rcx, %%r11\n");  /* Save rcx (4th param) to r11 */
            fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rdi\n");   /* rdi = stack pointer */
            fprintf(ctx->output_file, "\txorq\t%%rax, %%rax\n");   /* rax = 0 */
            fprintf(ctx->output_file, "\tmovl\t$%d, %%ecx\n", quadwords);  /* ecx = count */
            fprintf(ctx->output_file, "\trep stosq\n");            /* Zero-fill */
            fprintf(ctx->output_file, "\tmovq\t%%r10, %%rdi\n");  /* Restore rdi */
            fprintf(ctx->output_file, "\tmovq\t%%r11, %%rcx\n");  /* Restore rcx */
        }
    }
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_endprologue\n");

    /* Save callee-saved registers after the stack frame is set up */
    if (ctx->callee_save_rbx_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%rbx, -%d(%%rbp)\n", ctx->callee_save_rbx_offset);
    if (ctx->callee_save_r12_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r12, -%d(%%rbp)\n", ctx->callee_save_r12_offset);
    if (ctx->callee_save_r13_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r13, -%d(%%rbp)\n", ctx->callee_save_r13_offset);
    if (ctx->callee_save_r14_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r14, -%d(%%rbp)\n", ctx->callee_save_r14_offset);
    if (ctx->callee_save_r15_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r15, -%d(%%rbp)\n", ctx->callee_save_r15_offset);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Writes instruction list to file */
void codegen_inst_list(ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char *inst;

    assert(ctx != NULL);

    while(inst_list != NULL)
    {
        inst = (char *)inst_list->cur;
        assert(inst != NULL);

        fprintf(ctx->output_file, "%s", inst);

        inst_list = inst_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Returns the program name for use with main */
char * codegen_program(Tree_t *prgm, CodeGenContext *ctx, SymTab_t *symtab,
                       CompilationContext *comp_ctx)
{
    if (prgm == NULL)
        return NULL;

    struct Program *prog_data = &prgm->tree_data.program_data;
    if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
        fprintf(stderr, "[CodeGen] codegen_program: starting\n");
        if (prog_data->body_statement != NULL) {
            fprintf(stderr, "[CodeGen]   body_statement is NOT NULL, type=%d\n", prog_data->body_statement->type);
        } else {
            fprintf(stderr, "[CodeGen]   body_statement is NULL\n");
        }
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(prgm->type == TREE_PROGRAM_TYPE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char *prgm_name;
    struct Program *data;
    ListNode_t *inst_list;

    data = &prgm->tree_data.program_data;
    prgm_name = data->program_id;

    const char *prev_id = ctx->current_subprogram_id;
    const char *prev_mangled = ctx->current_subprogram_mangled;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    int prev_callee_rbx = ctx->callee_save_rbx_offset;
    int prev_callee_r12 = ctx->callee_save_r12_offset;
    int prev_callee_r13 = ctx->callee_save_r13_offset;
    int prev_callee_r14 = ctx->callee_save_r14_offset;
    int prev_callee_r15 = ctx->callee_save_r15_offset;
    ctx->current_subprogram_id = prgm_name;
    ctx->current_subprogram_mangled = prgm_name;
    ctx->current_subprogram_lexical_depth = 0;

    push_stackscope();

    /* Process var/const declarations from loaded units first, then program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            codegen_function_locals(unit->tree_data.unit_data.interface_var_decls, ctx, symtab);
            codegen_function_locals(unit->tree_data.unit_data.implementation_var_decls, ctx, symtab);
            codegen_emit_const_decl_equivs_from_list(ctx, unit->tree_data.unit_data.interface_const_decls);
            codegen_emit_const_decl_equivs_from_list(ctx, unit->tree_data.unit_data.implementation_const_decls);
        }
    }
    codegen_function_locals(data->var_declaration, ctx, symtab);
    codegen_emit_const_decl_equivs_from_list(ctx, data->const_declaration);

    /* Allocate callee-save slots AFTER locals so t-section offsets don't collide */
    {
        StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
        StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
        StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
        StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
        StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
        ctx->callee_save_rbx_offset = rbx_slot->offset;
        ctx->callee_save_r12_offset = r12_slot->offset;
        ctx->callee_save_r13_offset = r13_slot->offset;
        ctx->callee_save_r14_offset = r14_slot->offset;
        ctx->callee_save_r15_offset = r15_slot->offset;
    }

    /* Emit program subprograms first (they override unit versions with the
     * same mangled_id), then unit subprograms.
     * Units are iterated in REVERSE load order so that more fundamental units
     * (e.g. System) are processed last and their implementations win over
     * wrapper functions in higher-level units (e.g. objpas).  In the
     * loaded_units array, dependencies are added before their dependents
     * return from load_unit(), so the most fundamental unit has the highest
     * array index.  Iterating in reverse processes fundamentals first. */
    codegen_subprograms(data->subprograms, ctx, symtab);
    if (comp_ctx != NULL) {
        for (int i = comp_ctx->loaded_unit_count - 1; i >= 0; --i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            codegen_subprograms(unit->tree_data.unit_data.subprograms, ctx, symtab);
        }
    }

    /* Build hash set of emitted subprogram labels for O(1) lookups */
    CodeGenStringSet emitted_labels;
    memset(&emitted_labels, 0, sizeof(emitted_labels));
    {
        ListNode_t *_s = ctx->emitted_subprograms;
        while (_s != NULL) {
            if (_s->type == LIST_STRING && _s->cur != NULL)
                codegen_set_insert(&emitted_labels, (const char *)_s->cur);
            _s = _s->next;
        }
    }

    /* Build a temporary combined subprogram list for alias post-passes.
     * The alias passes need to scan all subprograms (units + program) to find
     * forward decl → implementation matches across unit boundaries. */
    ListNode_t *all_subprograms = NULL;
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            ListNode_t *usubs = unit->tree_data.unit_data.subprograms;
            while (usubs != NULL) {
                ListNode_t *copy = CreateListNode(usubs->cur, usubs->type);
                if (copy != NULL) {
                    copy->next = all_subprograms;
                    all_subprograms = copy;
                }
                usubs = usubs->next;
            }
        }
    }
    {
        ListNode_t *psubs = data->subprograms;
        while (psubs != NULL) {
            ListNode_t *copy = CreateListNode(psubs->cur, psubs->type);
            if (copy != NULL) {
                copy->next = all_subprograms;
                all_subprograms = copy;
            }
            psubs = psubs->next;
        }
    }

    /* Post-pass: emit .set aliases for cname_override values.
       Forward declarations with [Alias:'FPC_DO_EXIT'] have cname_override set
       but no body; the matching implementation has a body but no cname_override.
       Scan for forward decls with aliases and match them to implementations by id. */
    CodeGenStringSet emitted_cname_aliases;
    memset(&emitted_cname_aliases, 0, sizeof(emitted_cname_aliases));
    if (ctx->output_file != NULL) {
        int debug_alias = (getenv("KGPC_DEBUG_ALIAS") != NULL);
        ListNode_t *alias_scan = all_subprograms;
        while (alias_scan != NULL) {
            if (alias_scan->type == LIST_TREE && alias_scan->cur != NULL) {
                Tree_t *sub = (Tree_t *)alias_scan->cur;
                if (sub->type == TREE_SUBPROGRAM) {
                    const char *alias = sub->tree_data.subprogram_data.cname_override;
                    if (debug_alias && alias != NULL) {
                        fprintf(stderr, "[ALIAS] id=%s mangled=%s alias=%s has_body=%d\n",
                            sub->tree_data.subprogram_data.id ? sub->tree_data.subprogram_data.id : "<null>",
                            sub->tree_data.subprogram_data.mangled_id ? sub->tree_data.subprogram_data.mangled_id : "<null>",
                            alias, sub->tree_data.subprogram_data.statement_list != NULL);
                    }
                    if (alias != NULL) {
                        const char *mangled = sub->tree_data.subprogram_data.mangled_id;
                        const char *id = sub->tree_data.subprogram_data.id;
                        const char *label = (mangled != NULL) ? mangled : id;

                        /* Skip aliases that don't start with FPC_ or KGPC_ —
                           these are `external name` imports (getenv, read, time, etc.)
                           and emitting .set for them would override C library symbols. */
                        int is_internal_alias = (strncmp(alias, "FPC_", 4) == 0 ||
                                                 strncmp(alias, "KGPC_", 5) == 0);

                        /* If this node has a body AND was emitted, emit alias directly. */
                        if (is_internal_alias &&
                            sub->tree_data.subprogram_data.statement_list != NULL &&
                            label != NULL && strcmp(alias, label) != 0 &&
                            codegen_set_contains(&emitted_labels, label) &&
                            !codegen_set_contains(&emitted_cname_aliases, alias)) {
                            codegen_set_insert(&emitted_cname_aliases, alias);
                            fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), alias);
                            fprintf(ctx->output_file, "\t.set\t%s, %s\n", alias, label);
                        }
                        /* If this is a forward decl (no body), find the implementation.
                           Match by id, or by cname_override (e.g. `external name 'FPC_DO_EXIT'`
                           referencing a function that has [Alias:'FPC_DO_EXIT']). */
                        else if (is_internal_alias &&
                                 sub->tree_data.subprogram_data.statement_list == NULL) {
                            ListNode_t *impl_scan = all_subprograms;
                            while (impl_scan != NULL) {
                                if (impl_scan->type == LIST_TREE && impl_scan->cur != NULL) {
                                    Tree_t *impl = (Tree_t *)impl_scan->cur;
                                    if (impl != sub && impl->type == TREE_SUBPROGRAM &&
                                        impl->tree_data.subprogram_data.statement_list != NULL) {
                                        /* Match by id */
                                        int matched = 0;
                                        if (id != NULL && impl->tree_data.subprogram_data.id != NULL &&
                                            strcasecmp(impl->tree_data.subprogram_data.id, id) == 0)
                                            matched = 1;
                                        /* Match by alias matching impl's cname_override */
                                        if (!matched && impl->tree_data.subprogram_data.cname_override != NULL &&
                                            strcasecmp(impl->tree_data.subprogram_data.cname_override, alias) == 0)
                                            matched = 1;
                                        if (matched) {
                                            const char *impl_mangled = impl->tree_data.subprogram_data.mangled_id;
                                            const char *impl_label = (impl_mangled != NULL) ? impl_mangled : impl->tree_data.subprogram_data.id;
                                            if (impl_label != NULL && strcmp(alias, impl_label) != 0 &&
                                                codegen_set_contains(&emitted_labels, impl_label) &&
                                                !codegen_set_contains(&emitted_cname_aliases, alias)) {
                                                codegen_set_insert(&emitted_cname_aliases, alias);
                                                fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), alias);
                                                fprintf(ctx->output_file, "\t.set\t%s, %s\n", alias, impl_label);
                                            }
                                            /* Also emit alias from the forward decl's mangled_id to the impl.
                                               e.g., `int_finalize_p_p` → `fpc_finalize_p_p` so that
                                               `leaq int_finalize_p_p(%rip)` resolves correctly. */
                                            if (label != NULL && impl_label != NULL &&
                                                strcmp(label, impl_label) != 0 &&
                                                strcmp(label, alias) != 0 &&
                                                codegen_set_contains(&emitted_labels, impl_label) &&
                                                !codegen_set_contains(&emitted_cname_aliases, label)) {
                                                codegen_set_insert(&emitted_cname_aliases, label);
                                                fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), label);
                                                fprintf(ctx->output_file, "\t.set\t%s, %s\n", label, impl_label);
                                            }
                                            break;
                                        }
                                    }
                                }
                                impl_scan = impl_scan->next;
                            }
                        }
                    }
                }
            }
            alias_scan = alias_scan->next;
        }
    }

    /* Post-pass: emit .set aliases for forward declarations whose implementation
       has a different mangled_id (e.g. forward "runerror_i" → impl "FPC_RUNERROR").
       Call sites reference the forward's mangled_id, so we need an alias. */
    if (ctx->output_file != NULL) {
        ListNode_t *fwd_scan = all_subprograms;
        while (fwd_scan != NULL) {
            if (fwd_scan->type == LIST_TREE && fwd_scan->cur != NULL) {
                Tree_t *fwd = (Tree_t *)fwd_scan->cur;
                if (fwd->type == TREE_SUBPROGRAM &&
                    fwd->tree_data.subprogram_data.statement_list == NULL) {
                    const char *fwd_mangled = fwd->tree_data.subprogram_data.mangled_id;
                    const char *fwd_id = fwd->tree_data.subprogram_data.id;
                    /* Skip external imports (`external name 'getenv'` etc.) — their
                       mangled_id IS the external C symbol name.  Aliasing it to an
                       internal Pascal implementation would shadow the C library symbol
                       and cause infinite recursion when the Pascal implementation
                       tries to call the C function. */
                    const char *fwd_cname = fwd->tree_data.subprogram_data.cname_override;
                    int is_external_import = (fwd_cname != NULL &&
                        strncmp(fwd_cname, "FPC_", 4) != 0 &&
                        strncmp(fwd_cname, "KGPC_", 5) != 0);
                    if (fwd_mangled != NULL && fwd_id != NULL &&
                        !is_external_import &&
                        !codegen_set_contains(&emitted_labels, fwd_mangled) &&
                        !codegen_set_contains(&emitted_cname_aliases, fwd_mangled)) {
                        /* Find matching implementation by id.
                         * Skip overloaded functions — aliasing one overload to
                         * another (e.g. fileexists_rbs_b → fileexists_us_b)
                         * causes infinite recursion when the target calls the
                         * aliased overload. */
                        ListNode_t *impl_scan = all_subprograms;
                        while (impl_scan != NULL) {
                            if (impl_scan->type == LIST_TREE && impl_scan->cur != NULL) {
                                Tree_t *impl = (Tree_t *)impl_scan->cur;
                                if (impl != fwd && impl->type == TREE_SUBPROGRAM &&
                                    impl->tree_data.subprogram_data.statement_list != NULL &&
                                    impl->tree_data.subprogram_data.id != NULL &&
                                    strcasecmp(impl->tree_data.subprogram_data.id, fwd_id) == 0) {
                                    const char *impl_mangled = impl->tree_data.subprogram_data.mangled_id;
                                    if (impl_mangled != NULL &&
                                        strcasecmp(fwd_mangled, impl_mangled) != 0 &&
                                        codegen_set_contains(&emitted_labels, impl_mangled)) {
                                        /* Only alias when the impl used a cname_override
                                         * (e.g. [Alias:'FPC_ANSISTR_DECR_REF']) so the
                                         * label name differs from the mangled name purely
                                         * due to aliasing, NOT different overload signatures.
                                         * If neither has cname_override, the different
                                         * mangled names mean different parameter types
                                         * (e.g. fileexists_rbs_b vs fileexists_us_b). */
                                        if (fwd_cname == NULL &&
                                            impl->tree_data.subprogram_data.cname_override == NULL)
                                            goto next_fwd;
                                        codegen_set_insert(&emitted_cname_aliases, fwd_mangled);
                                        fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), fwd_mangled);
                                        fprintf(ctx->output_file, "\t.set\t%s, %s\n", fwd_mangled, impl_mangled);
                                        break;
                                    }
                                }
                            }
                            impl_scan = impl_scan->next;
                        }
                    }
                }
            }
            next_fwd:
            fwd_scan = fwd_scan->next;
        }
    }

    codegen_set_destroy(&emitted_labels);
    codegen_set_destroy(&emitted_cname_aliases);

    /* Free the temporary combined subprograms list (shallow copies only) */
    {
        ListNode_t *tmp = all_subprograms;
        while (tmp != NULL) {
            ListNode_t *next = tmp->next;
            tmp->cur = NULL;  /* Don't free the Tree_t — we don't own it */
            free(tmp);
            tmp = next;
        }
        all_subprograms = NULL;
    }

    inst_list = NULL;
    /* Emit var initializers from loaded units first, then program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            inst_list = codegen_var_initializers(unit->tree_data.unit_data.interface_var_decls, inst_list, ctx, symtab);
            inst_list = codegen_var_initializers(unit->tree_data.unit_data.implementation_var_decls, inst_list, ctx, symtab);
        }
    }
    inst_list = codegen_var_initializers(data->var_declaration, inst_list, ctx, symtab);

    /* Emit unit initialization blocks in dependency (load) order */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            struct Statement *init_stmt = unit->tree_data.unit_data.initialization;
            if (init_stmt == NULL)
                continue;
            /* Only inline the inner statements from compound statements */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT) {
                ListNode_t *stnode = init_stmt->stmt_data.compound_statement;
                while (stnode != NULL) {
                    if (stnode->type == LIST_STMT && stnode->cur != NULL)
                        inst_list = codegen_stmt((struct Statement *)stnode->cur, inst_list, ctx, symtab);
                    stnode = stnode->next;
                }
            } else {
                inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
            }
        }
    }

    if (data->body_statement == NULL && getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] WARNING: program body is NULL during codegen\n");
    }
    inst_list = codegen_stmt(data->body_statement, inst_list, ctx, symtab);

    /* Emit unit finalization blocks in reverse dependency order (LIFO) */
    if (comp_ctx != NULL) {
        for (int i = comp_ctx->loaded_unit_count - 1; i >= 0; --i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            struct Statement *final_stmt = unit->tree_data.unit_data.finalization;
            if (final_stmt == NULL)
                continue;
            inst_list = codegen_stmt(final_stmt, inst_list, ctx, symtab);
        }
    }

    codegen_function_header(prgm_name, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(prgm_name, ctx);
    free_inst_list(inst_list);

    /* Emit INITFINAL table — FPC system unit references this to run unit
       init/finalization.  KGPC inlines that code into main, so emit a
       minimal table with TableCount = 0. */
    if (ctx->output_file != NULL) {
        fprintf(ctx->output_file, "\n.data\n");
        fprintf(ctx->output_file, ".globl\tINITFINAL\n");
        fprintf(ctx->output_file, "INITFINAL:\n");
        fprintf(ctx->output_file, "\t.long\t0\n");  /* TableCount = 0 */
    }

    /* Emit FPC_RESOURCESTRINGTABLES as a zero-length table (no resource strings). */
    if (ctx->output_file != NULL) {
        fprintf(ctx->output_file, ".globl\tFPC_RESOURCESTRINGTABLES\n");
        fprintf(ctx->output_file, "FPC_RESOURCESTRINGTABLES:\n");
        fprintf(ctx->output_file, "\t.quad\t0\n");
    }

    pop_stackscope();

    ctx->current_subprogram_id = prev_id;
    ctx->current_subprogram_mangled = prev_mangled;
    ctx->current_subprogram_lexical_depth = prev_depth;
    ctx->callee_save_rbx_offset = prev_callee_rbx;
    ctx->callee_save_r12_offset = prev_callee_r12;
    ctx->callee_save_r13_offset = prev_callee_r13;
    ctx->callee_save_r14_offset = prev_callee_r14;
    ctx->callee_save_r15_offset = prev_callee_r15;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return prgm_name;
}

/* Pushes function locals onto the stack */
void codegen_function_locals(ListNode_t *local_decl, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
     ListNode_t *cur, *id_list;
     Tree_t *tree;

    assert(ctx != NULL);

    int is_program_scope = (codegen_get_lexical_depth(ctx) == 0);

     cur = local_decl;

     while(cur != NULL)
     {
         tree = (Tree_t *)cur->cur;
         assert(tree != NULL);

        if (tree->type == TREE_VAR_DECL)
        {
            id_list = tree->tree_data.var_decl_data.ids;
            HashNode_t *type_node = NULL;
            if (symtab != NULL && tree->tree_data.var_decl_data.type_id != NULL) {
                FindSymbol(&type_node, symtab, tree->tree_data.var_decl_data.type_id);
            }
            int decl_type_owned = 0;
            KgpcType *decl_type = resolve_type_from_vardecl(tree, symtab, &decl_type_owned);
            KgpcType *cached_type = tree->tree_data.var_decl_data.cached_kgpc_type;

            while(id_list != NULL)
            {
                HashNode_t decl_type_node;
                HashNode_t cached_type_node;
                HashNode_t *fallback_type_node = NULL;
                HashNode_t *var_info = NULL;
                if (decl_type != NULL)
                {
                    memset(&decl_type_node, 0, sizeof(decl_type_node));
                    decl_type_node.type = decl_type;
                    fallback_type_node = &decl_type_node;
                }
                if (cached_type != NULL)
                {
                    memset(&cached_type_node, 0, sizeof(cached_type_node));
                    cached_type_node.type = cached_type;
                    if (fallback_type_node == NULL)
                        fallback_type_node = &cached_type_node;
                }
                if (symtab != NULL)
                    FindSymbol(&var_info, symtab, id_list->cur);

                HashNode_t *effective_type_node = type_node;
                if (decl_type != NULL && kgpc_type_is_array(decl_type))
                    effective_type_node = &decl_type_node;
                if (effective_type_node == NULL)
                    effective_type_node = fallback_type_node;

                KgpcType *param_type = NULL;
                if (effective_type_node != NULL)
                    param_type = effective_type_node->type;
                if (param_type == NULL && var_info != NULL)
                    param_type = var_info->type;
                KGPC_COMPILER_HARD_ASSERT(param_type != NULL,
                    "missing type metadata for local '%s' (declared type '%s')",
                    (const char *)id_list->cur,
                    tree->tree_data.var_decl_data.type_id != NULL ?
                        tree->tree_data.var_decl_data.type_id : "<null>");

                if (param_type != NULL && kgpc_type_is_array(param_type))
                {
                    KgpcArrayDimensionInfo array_info;
                    int dim_info_result = kgpc_type_get_array_dimension_info(param_type, symtab, &array_info);
                    
                    /* If dimension info lookup succeeded, use its values; otherwise fall back to simpler methods */
                    int element_size;
                    int array_start;
                    long long total_size;
                    
                    if (dim_info_result == 0 && array_info.dim_count > 0 &&
                        array_info.element_size > 0 &&
                        array_info.element_size <= INT_MAX &&
                        array_info.dim_lowers[0] >= INT_MIN &&
                        array_info.dim_lowers[0] <= INT_MAX)
                    {
                        /* Use computed values from kgpc_type_get_array_dimension_info which handles
                         * multi-dimensional arrays and constant-based bounds correctly */
                        element_size = (int)array_info.element_size;
                        array_start = (int)array_info.dim_lowers[0];
                        total_size = array_info.total_size;
                    }
                    else
                    {
                        /* Fallback: use simpler methods that work for basic array types */
                        element_size = (int)kgpc_type_get_array_element_size(param_type);
                        if (element_size <= 0 && param_type != NULL &&
                            param_type->info.array_info.element_type != NULL &&
                            param_type->info.array_info.element_type->kind == TYPE_KIND_RECORD)
                        {
                            long long record_size = 0;
                            if (codegen_sizeof_record_type(ctx,
                                    param_type->info.array_info.element_type->info.record_info,
                                    &record_size) == 0 && record_size > 0 && record_size <= INT_MAX)
                            {
                                element_size = (int)record_size;
                            }
                        }
                        KGPC_COMPILER_HARD_ASSERT(element_size > 0,
                            "unable to resolve array element size for local '%s'",
                            (const char *)id_list->cur);
                        array_start = param_type->info.array_info.start_index;
                        total_size = kgpc_type_sizeof(param_type);
                    }
                    
                    int is_open_array = kgpc_type_is_dynamic_array(param_type);

                    if (is_open_array)
                    {
                        char *static_label = NULL;
                        if (is_program_scope)
                        {
                            static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int descriptor_bytes = codegen_dynamic_array_descriptor_bytes(element_size);
                                int alignment = descriptor_bytes >= 8 ? 8 : DOUBLEWORD;
                                fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                    static_label, descriptor_bytes, alignment);
                            }
                        }
                        add_dynamic_array((char *)id_list->cur, element_size,
                            array_start, is_program_scope, static_label);
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        if (total_size <= 0)
                            total_size = element_size;

                        if (is_program_scope)
                        {
                            char *static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, static_label,
                                    (int)total_size, alignment,
                                    tree->tree_data.var_decl_data.defined_in_unit);
                            }
                            add_static_array((char *)id_list->cur, (int)total_size, element_size,
                                array_start, static_label);
                            if (static_label != NULL)
                                free(static_label);
                        }
                        else
                        {
                            add_array((char *)id_list->cur, (int)total_size, element_size,
                                array_start);
                        }
                    }
                }
                else if (var_info != NULL && var_info->type != NULL && kgpc_type_is_array(var_info->type))
                {
                    KgpcType *array_type = var_info->type;
                    KgpcType *element_type = array_type->info.array_info.element_type;
                    long long element_size_ll = kgpc_type_get_array_element_size(array_type);
                    if (element_size_ll <= 0 && element_type != NULL)
                        element_size_ll = kgpc_type_sizeof(element_type);
                    if (element_size_ll <= 0 && element_type != NULL &&
                        element_type->kind == TYPE_KIND_RECORD)
                    {
                        long long record_size = 0;
                        if (codegen_sizeof_record_type(ctx, element_type->info.record_info,
                                &record_size) == 0 && record_size > 0)
                        {
                            element_size_ll = record_size;
                        }
                    }
                    if (element_size_ll <= 0)
                        element_size_ll = 4;

                    int start = 0;
                    int end = -1;
                    kgpc_type_get_array_bounds(array_type, &start, &end);
                    int is_open_array = kgpc_type_is_dynamic_array(array_type);

                    if (is_open_array)
                    {
                        char *static_label = NULL;
                        if (is_program_scope)
                        {
                            static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int descriptor_bytes = codegen_dynamic_array_descriptor_bytes((int)element_size_ll);
                                int alignment = descriptor_bytes >= 8 ? 8 : DOUBLEWORD;
                                fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                    static_label, descriptor_bytes, alignment);
                            }
                        }
                        add_dynamic_array((char *)id_list->cur, (int)element_size_ll,
                            start, is_program_scope, static_label);
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        int length = end - start + 1;
                        if (length < 0)
                            length = 0;
                        long long total_size = (long long)length * element_size_ll;
                        if (total_size <= 0)
                            total_size = element_size_ll;
                        if (is_program_scope)
                        {
                            char *static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, static_label,
                                    (int)total_size, alignment,
                                    tree->tree_data.var_decl_data.defined_in_unit);
                            }
                            add_static_array((char *)id_list->cur, (int)total_size,
                                (int)element_size_ll, start, static_label);
                            if (static_label != NULL)
                                free(static_label);
                        }
                        else
                        {
                            add_array((char *)id_list->cur, (int)total_size,
                                (int)element_size_ll, start);
                        }
                    }
                }
                else
                {
                    int alloc_size = DOUBLEWORD;
                    HashNode_t *size_node = NULL;  /* Node to get size from */
                    HashNode_t temp_size_node;
                    
                    if (symtab != NULL)
                    {
                        if (FindSymbol(&var_info, symtab, id_list->cur) != 0 && var_info != NULL)
                            size_node = var_info;
                    }
                    /* Use type_node if we don't have specific var_info */
                    if (size_node == NULL && effective_type_node != NULL)
                        size_node = effective_type_node;
                    if (size_node == NULL && cached_type != NULL)
                    {
                        memset(&temp_size_node, 0, sizeof(temp_size_node));
                        temp_size_node.type = cached_type;
                        size_node = &temp_size_node;
                    }
                    
                    /* Get allocation size using helper */
                    if (size_node != NULL)
                    {
                        int size = get_var_storage_size(size_node);
                        if (size > 0)
                        {
                            alloc_size = size;
                        }
                        else if (node_is_record_type(size_node))
                        {
                            /* For classes, allocate only pointer size (8 bytes)
                             * For records/objects, allocate the full struct size */
                            if (node_is_class_type(size_node))
                            {
                                CODEGEN_DEBUG("DEBUG ALLOC: Detected class type for '%s', allocating 8 bytes\n",
                                    (char *)id_list->cur);
                                alloc_size = 8;  /* Classes are heap-allocated; variable holds pointer */
                            }
                            else
                            {
                                CODEGEN_DEBUG("DEBUG ALLOC: Detected record type for '%s', allocating full size\n",
                                    (char *)id_list->cur);
                                /* For records/objects, get the full struct size */
                                struct RecordType *record_desc = get_record_type_from_node(size_node);
                                long long record_size = 0;
                                if (record_desc != NULL &&
                                    codegen_sizeof_record_type(ctx, record_desc, &record_size) == 0 &&
                                    record_size > 0)
                                {
                                    alloc_size = (int)record_size;
                                }
                            }
                        }
                    }

                    if (is_program_scope)
                    {
                        const char *absolute_target = tree->tree_data.var_decl_data.absolute_target;
                        const char *absolute_base = tree->tree_data.var_decl_data.absolute_base_id;
                        const char *absolute_field = tree->tree_data.var_decl_data.absolute_field_id;
                        if (absolute_target != NULL && id_list != NULL && id_list->next == NULL)
                        {
                            if (absolute_base != NULL && absolute_field != NULL)
                            {
                                /* Record field alias: extract base var and field name */
                                const char *base_var = absolute_base;
                                const char *field_name = absolute_field;

                                if (base_var != NULL && field_name != NULL)
                                {
                                    /* Look up base variable in symbol table to get record type */
                                    int field_offset = -1;
                                    HashNode_t *base_node = NULL;
                                    if (ctx->symtab != NULL &&
                                        FindSymbol(&base_node, ctx->symtab, base_var) != 0 &&
                                        base_node != NULL)
                                    {
                                        struct RecordType *record = get_record_type_from_node(base_node);
                                        if (record != NULL)
                                        {
                                            field_offset = record_type_get_field_offset(ctx->symtab, record, field_name);
                                        }
                                    }
                                    
                                    if (field_offset >= 0)
                                    {
                                        if (add_absolute_var_alias_with_offset((char *)id_list->cur, 
                                            (char *)base_var, field_offset, alloc_size) == 0)
                                        {
                                            id_list = id_list->next;
                                            continue;
                                        }
                                    }
                                }
                                fprintf(stderr,
                                    "Warning: absolute variable alias to record field '%s' failed to resolve.\n",
                                    absolute_target);
                            }
                            else if (absolute_base != NULL &&
                                (add_absolute_var_alias((char *)id_list->cur, absolute_base) == 0 ||
                                 add_absolute_static_symbol_alias((char *)id_list->cur,
                                     absolute_base, alloc_size) == 0))
                            {
                                id_list = id_list->next;
                                continue;
                            }
                            else
                            {
                                fprintf(stderr,
                                    "Warning: failed to resolve absolute variable alias target '%s'.\n",
                                    absolute_target);
                            }
                        }

                        char *static_label = NULL;
                        int is_external_var = tree->tree_data.var_decl_data.is_external;
                        char *cname_override = tree->tree_data.var_decl_data.cname_override;
                        
                        if (cname_override != NULL) {
                            /* Use the external/public name directly */
                            static_label = strdup(cname_override);
                        } else {
                            static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                        }
                        
                        if (ctx->output_file != NULL && static_label != NULL)
                        {
                            if (is_external_var) {
                                /* External variable: don't allocate storage, just reference the symbol */
                                /* No .comm directive needed - the symbol is defined elsewhere */
                            } else {
                                int alignment = alloc_size >= 16 ? 16 : (alloc_size >= 8 ? 8 : DOUBLEWORD);
                                /* Only emit a bare-name alias when there is no explicit cname override */
                                int defined_for_alias = (cname_override == NULL) ?
                                    tree->tree_data.var_decl_data.defined_in_unit : 0;
                                if (cname_override != NULL) {
                                    /* Public name: make it globally visible */
                                    fprintf(ctx->output_file, "\t.globl\t%s\n", static_label);
                                }
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, static_label,
                                    alloc_size, alignment, defined_for_alias);
                            }
                        }
                        add_static_var((char *)id_list->cur, alloc_size, static_label);
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        const char *absolute_target = tree->tree_data.var_decl_data.absolute_target;
                        const char *absolute_base = tree->tree_data.var_decl_data.absolute_base_id;
                        const char *absolute_field = tree->tree_data.var_decl_data.absolute_field_id;
                        if (absolute_target != NULL && id_list != NULL && id_list->next == NULL)
                        {
                            if (absolute_base != NULL && absolute_field != NULL)
                            {
                                /* Record field alias: extract base var and field name */
                                const char *base_var = absolute_base;
                                const char *field_name = absolute_field;

                                if (base_var != NULL && field_name != NULL)
                                {
                                    /* Look up base variable in symbol table to get record type */
                                    int field_offset = -1;
                                    HashNode_t *base_node = NULL;
                                    if (ctx->symtab != NULL &&
                                        FindSymbol(&base_node, ctx->symtab, base_var) != 0 &&
                                        base_node != NULL)
                                    {
                                        struct RecordType *record = get_record_type_from_node(base_node);
                                        if (record != NULL)
                                        {
                                            field_offset = record_type_get_field_offset(ctx->symtab, record, field_name);
                                        }
                                    }
                                    
                                    if (field_offset >= 0)
                                    {
                                        if (add_absolute_var_alias_with_offset((char *)id_list->cur, 
                                            (char *)base_var, field_offset, alloc_size) == 0)
                                        {
                                            id_list = id_list->next;
                                            continue;
                                        }
                                    }
                                }
                                fprintf(stderr,
                                    "Warning: absolute variable alias to record field '%s' failed to resolve.\n",
                                    absolute_target);
                            }
                            else if (absolute_base != NULL &&
                                (add_absolute_var_alias((char *)id_list->cur, absolute_base) == 0 ||
                                 add_absolute_static_symbol_alias((char *)id_list->cur,
                                     absolute_base, alloc_size) == 0))
                            {
                                id_list = id_list->next;
                                continue;
                            }
                            else
                            {
                                fprintf(stderr,
                                    "Warning: failed to resolve absolute variable alias target '%s'.\n",
                                    absolute_target);
                            }
                        }

                        add_l_x((char *)id_list->cur, alloc_size);
                    }
                }
                id_list = id_list->next;
            };

            if (decl_type_owned && decl_type != NULL)
                destroy_kgpc_type(decl_type);
        }
        else if (tree->type == TREE_ARR_DECL)
        {
            struct Array *arr = &tree->tree_data.arr_decl_data;
            id_list = arr->ids;

            int is_dynamic = (arr->e_range < arr->s_range);

            HashNode_t *type_node = NULL;
            if (arr->type_id != NULL && symtab != NULL)
                FindSymbol(&type_node, symtab, arr->type_id);

            struct RecordType *record_desc = NULL;
            if (type_node != NULL)
            {
                record_desc = get_record_type_from_node(type_node);
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (record_desc == NULL && alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node = NULL;
                    if (FindSymbol(&target_node, symtab, alias->target_type_id) != 0 &&
                        target_node != NULL)
                        record_desc = get_record_type_from_node(target_node);
                }
            }
            if (record_desc == NULL && arr->inline_record_type != NULL)
            {
                record_desc = arr->inline_record_type;
            }

            long long computed_size = 0;
            int element_size = 0;
            if (arr->element_kgpc_type != NULL)
            {
                computed_size = kgpc_type_sizeof(arr->element_kgpc_type);
                if (computed_size > 0 && computed_size <= INT_MAX)
                    element_size = (int)computed_size;
            }
            if (element_size <= 0 &&
                codegen_sizeof_type_reference(ctx, arr->type, arr->type_id,
                    record_desc, &computed_size) == 0 && computed_size > 0 &&
                computed_size <= INT_MAX)
            {
                element_size = (int)computed_size;
            }
            else if (record_desc != NULL &&
                codegen_sizeof_record_type(ctx, record_desc, &computed_size) == 0 &&
                computed_size > 0 && computed_size <= INT_MAX)
            {
                element_size = (int)computed_size;
            }

            if (element_size <= 0)
            {
                /* Fallback: determine element size from type */
                if (type_node != NULL)
                {
                    int size = get_var_storage_size(type_node);
                    if (size > 0)
                        element_size = size;
                    else
                        element_size = DOUBLEWORD;
                }
                else
                {
                    /* Use arr->type to determine element size */
                    switch (arr->type)
                    {
                        case LONGINT_TYPE:
                            element_size = 4;  // Match FPC's 32-bit LongInt
                            break;
                        case REAL_TYPE:
                        case STRING_TYPE:
                        case FILE_TYPE:
                        case TEXT_TYPE:
                            element_size = 8;
                            break;
                        case SHORTSTRING_TYPE:
                            element_size = 256;
                            break;
                        case BOOL:
                        case CHAR_TYPE:
                            element_size = 1;
                            break;
                        default:
                            element_size = DOUBLEWORD;
                            break;
                    }
                }
            }

            if (is_dynamic)
            {
                while (id_list != NULL)
                {
                    char *static_label = NULL;
                    if (is_program_scope)
                    {
                        static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                        if (ctx->output_file != NULL && static_label != NULL)
                        {
                            int descriptor_bytes = codegen_dynamic_array_descriptor_bytes(element_size);
                            int alignment = descriptor_bytes >= 8 ? 8 : DOUBLEWORD;
                            fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                static_label, descriptor_bytes, alignment);
                        }
                    }
                    add_dynamic_array((char *)id_list->cur, element_size, arr->s_range,
                        is_program_scope, static_label);
                    if (static_label != NULL)
                        free(static_label);
                    id_list = id_list->next;
                }
            }
            else
            {
                int length = arr->e_range - arr->s_range + 1;
                if (length < 0)
                    length = 0;
                int total_size = length * element_size;
                if (total_size <= 0)
                    total_size = element_size;

                /* For multi-dimensional arrays, kgpc_type_sizeof only accounts
                 * for the outer dimension.  Use kgpc_type_get_array_dimension_info
                 * which correctly multiplies all dimension sizes together. */
                if (id_list != NULL && symtab != NULL) {
                    const char *var_name = (const char *)id_list->cur;
                    HashNode_t *var_node = NULL;
                    if (FindSymbol(&var_node, symtab, var_name) && var_node != NULL && var_node->type != NULL) {
                        KgpcArrayDimensionInfo dim_info;
                        if (kgpc_type_get_array_dimension_info(var_node->type, symtab, &dim_info) == 0) {
                            if (dim_info.strides[0] > element_size &&
                                dim_info.strides[0] <= INT_MAX)
                            {
                                element_size = (int)dim_info.strides[0];
                            }
                            if (dim_info.total_size > total_size)
                                total_size = (int)dim_info.total_size;
                        } else {
                            long long kgpc_size = kgpc_type_sizeof(var_node->type);
                            if (kgpc_size > total_size)
                                total_size = (int)kgpc_size;
                        }
                    }
                }

                int use_static_storage = arr->has_static_storage || is_program_scope;
                if (arr->has_static_storage)
                {
                    if (!arr->static_storage_emitted)
                    {
                        if (arr->static_label != NULL)
                            fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                arr->static_label, total_size, DOUBLEWORD);
                        if (arr->init_guard_label != NULL)
                            fprintf(ctx->output_file, "\t.comm\t%s,1,1\n",
                                arr->init_guard_label);
                        arr->static_storage_emitted = 1;
                    }
                }

                if (use_static_storage)
                {
                    while (id_list != NULL)
                    {
                        const char *label_to_use = arr->static_label;
                        char *generated_label = NULL;
                        if (!arr->has_static_storage)
                        {
                            generated_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && generated_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, generated_label,
                                    total_size, alignment, arr->defined_in_unit);
                            }
                            label_to_use = generated_label;
                        }
                        add_static_array((char *)id_list->cur, total_size, element_size,
                            arr->s_range, label_to_use);
                        if (generated_label != NULL)
                            free(generated_label);
                        id_list = id_list->next;
                    }
                }
                else
                {
                    while (id_list != NULL)
                    {
                        add_array((char *)id_list->cur, total_size, element_size,
                            arr->s_range);
                        id_list = id_list->next;
                    }
                }
            }
        }

         cur = cur->next;
     }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Sets number of vector registers (floating points) before a function call */
ListNode_t *codegen_vect_reg(ListNode_t *inst_list, int num_vec)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[50];
    snprintf(buffer, 50, "\tmovl\t$%d, %%eax\n", num_vec);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return add_inst(inst_list, buffer);
}

/* Returns the distance between the total declared size of value (non-var) float
 * parameters and KGPC's native float size (8 bytes per param).  A distance of 0
 * means all value float params are Double/Real (8 bytes) — the KGPC-native size.
 * Used by has_later_override to prefer Double sincos over Single or Extended. */
static int codegen_float_native_distance(Tree_t *sub)
{
    int n_value = 0;
    int total_declared = 0;
    ListNode_t *p = sub->tree_data.subprogram_data.args_var;
    while (p != NULL)
    {
        Tree_t *decl = (Tree_t *)p->cur;
        if (decl != NULL && decl->type == TREE_VAR_DECL)
        {
            int is_var = decl->tree_data.var_decl_data.is_var_param;
            if (!is_var)
            {
                n_value++;
                const char *tid = decl->tree_data.var_decl_data.type_id;
                int sz = 8; /* default: double/real/float → 8 bytes (KGPC native) */
                if (tid != NULL)
                {
                    if (pascal_identifier_equals(tid, "single"))
                        sz = 4;
                    else if (pascal_identifier_equals(tid, "extended") ||
                             pascal_identifier_equals(tid, "extended80"))
                        sz = 16;
                    /* double, real, float, currency, longreal → sz stays 8 */
                }
                total_declared += sz;
            }
        }
        p = p->next;
    }
    if (n_value == 0)
        return 0; /* no value params: distance 0 (use original later-wins rule) */
    int native = n_value * 8;
    int dist = total_declared - native;
    return dist < 0 ? -dist : dist;
}

/* Codegen for a list of subprograms */
void codegen_subprograms(ListNode_t *sub_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    Tree_t *sub;

    assert(ctx != NULL);
    assert(symtab != NULL);

    while(sub_list != NULL)
    {
        sub = (Tree_t *)sub_list->cur;
        assert(sub != NULL);
        assert(sub->type == TREE_SUBPROGRAM);

        const char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
        int trace_tfplistenum = getenv("KGPC_TRACE_TFPLISTENUM") != NULL &&
            mangled_id != NULL &&
            strncasecmp(mangled_id, "tfplistenumerator__", 19) == 0;
        int trace_missing_calls = getenv("KGPC_TRACE_MISSING_CALLS") != NULL &&
            mangled_id != NULL &&
            (strcasecmp(mangled_id, "format_us_a") == 0 ||
             strcasecmp(mangled_id, "format_s_a") == 0 ||
             strcasecmp(mangled_id, "codepagenametocodepage_s") == 0 ||
             strcasecmp(mangled_id, "stringofchar_c_li") == 0 ||
             strcasecmp(mangled_id, "stringofchar_c_i64") == 0);

        if (trace_tfplistenum || trace_missing_calls)
        {
            fprintf(stderr,
                "[codegen] sub=%s id=%s owner=%s method=%s body=%d used=%d template=%d\n",
                mangled_id,
                sub->tree_data.subprogram_data.id != NULL ? sub->tree_data.subprogram_data.id : "(null)",
                sub->tree_data.subprogram_data.owner_class != NULL ? sub->tree_data.subprogram_data.owner_class : "(null)",
                sub->tree_data.subprogram_data.method_name != NULL ? sub->tree_data.subprogram_data.method_name : "(null)",
                sub->tree_data.subprogram_data.statement_list != NULL,
                sub->tree_data.subprogram_data.is_used,
                sub->tree_data.subprogram_data.is_generic_template);
        }

        if (mangled_id != NULL && ctx->emitted_subprograms != NULL)
        {
            ListNode_t *seen = ctx->emitted_subprograms;
            int already_emitted = 0;
            while (seen != NULL)
            {
                if (seen->type == LIST_STRING && seen->cur != NULL &&
                    strcmp((const char *)seen->cur, mangled_id) == 0)
                {
                    already_emitted = 1;
                    break;
                }
                seen = seen->next;
            }
            if (already_emitted)
            {
                if (trace_tfplistenum || trace_missing_calls)
                    fprintf(stderr, "[codegen] skip already emitted %s\n", mangled_id);
                sub_list = sub_list->next;
                continue;
            }
        }

        if (sub->tree_data.subprogram_data.statement_list == NULL)
        {
            if (trace_tfplistenum || trace_missing_calls)
                fprintf(stderr, "[codegen] skip no body %s\n", mangled_id != NULL ? mangled_id : "(null)");
            sub_list = sub_list->next;
            continue;
        }

        /* If a LATER subprogram from the SAME UNIT has the same mangled_id and
         * a body, skip this one.  This handles platform-specific overrides:
         * e.g. Unix sysutils.pp defines FileExists(RawByteString) after the
         * generic filutil.inc version.  The later definition wins.
         *
         * Cross-unit: if a LATER subprogram from a unit that this function's
         * unit depends on (i.e. a more fundamental unit) has the same mangled_id,
         * prefer the fundamental unit's version.  This handles the case where a
         * wrapper unit (e.g. objpas) defines a same-named function that wraps
         * the system unit's implementation — both compile to the same mangled
         * name, but the system unit's version is the real implementation.
         * We check the actual dependency graph via unit_registry_is_dep()
         * instead of assuming lower source_unit_index = more fundamental. */
        if (mangled_id != NULL)
        {
            int this_unit = sub->tree_data.subprogram_data.source_unit_index;
            int has_later_override = 0;
            /* For float overloads (e.g. sincos Single/Double/Extended), prefer the
             * one whose value param sizes are closest to KGPC's native 8-byte float.
             * Only skip the current if a later same-unit same-mangled body has a
             * distance to native that is ≤ the current one's distance. */
            int current_dist = codegen_float_native_distance(sub);
            ListNode_t *later = sub_list->next;
            while (later != NULL)
            {
                if (later->type == LIST_TREE && later->cur != NULL)
                {
                    Tree_t *later_sub = (Tree_t *)later->cur;
                    int later_unit = later_sub->tree_data.subprogram_data.source_unit_index;
                    if (later_sub->type == TREE_SUBPROGRAM &&
                        later_sub->tree_data.subprogram_data.statement_list != NULL &&
                        later_sub->tree_data.subprogram_data.mangled_id != NULL &&
                        strcmp(later_sub->tree_data.subprogram_data.mangled_id, mangled_id) == 0 &&
                        (later_unit == this_unit ||
                         (later_unit > 0 && this_unit > 0 &&
                          unit_registry_is_dep(this_unit, later_unit))))
                    {
                        int later_dist = codegen_float_native_distance(later_sub);
                        if (later_dist <= current_dist)
                        {
                            has_later_override = 1;
                            break;
                        }
                        /* later_dist > current_dist: the later body is farther from
                         * KGPC's native float size (e.g. Single at dist=4 vs Double
                         * at dist=0).  Don't let it override the current better one;
                         * continue scanning for a yet-better later body. */
                    }
                }
                later = later->next;
            }
            if (has_later_override)
            {
                if (trace_tfplistenum || trace_missing_calls)
                    fprintf(stderr, "[codegen] skip later override %s\n", mangled_id);
                sub_list = sub_list->next;
                continue;
            }
        }

        /* Skip unused functions (dead code elimination / reachability pass). */
        if (!disable_dce_flag() && !sub->tree_data.subprogram_data.is_used)
        {
            if (trace_tfplistenum || trace_missing_calls)
                fprintf(stderr, "[codegen] skip dce-unused %s\n", mangled_id != NULL ? mangled_id : "(null)");
            sub_list = sub_list->next;
            continue;
        }

        /* Skip unspecialized generic subprogram templates. */
        if (sub->tree_data.subprogram_data.is_generic_template)
        {
            if (trace_tfplistenum || trace_missing_calls)
                fprintf(stderr, "[codegen] skip generic template %s\n", mangled_id != NULL ? mangled_id : "(null)");
            sub_list = sub_list->next;
            continue;
        }

        if (mangled_id != NULL)
        {
            ListNode_t *node = CreateListNode((void *)mangled_id, LIST_STRING);
            if (ctx->emitted_subprograms == NULL)
                ctx->emitted_subprograms = node;
            else
                ctx->emitted_subprograms = PushListNodeBack(ctx->emitted_subprograms, node);
        }

        switch(sub->tree_data.subprogram_data.sub_type)
        {
            case TREE_SUBPROGRAM_PROC:
                codegen_procedure(sub, ctx, symtab);
                break;
            case TREE_SUBPROGRAM_FUNC:
                codegen_function(sub, ctx, symtab);
                break;
            default:
                assert(0 && "Unrecognized subprogram type in codegen!");
        }
        sub_list = sub_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for a procedure */
void codegen_procedure(Tree_t *proc_tree, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(proc_tree != NULL);
    assert(proc_tree->type == TREE_SUBPROGRAM);
    assert(proc_tree->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_PROC);
    assert(ctx != NULL);
    assert(symtab != NULL);

    struct Subprogram *proc;
    ListNode_t *inst_list;
    char *sub_id;

    proc = &proc_tree->tree_data.subprogram_data;
    sub_id = (proc->mangled_id != NULL) ? proc->mangled_id : proc->id;

    if (codegen_runtime_owns_exported_symbol(sub_id) ||
        codegen_runtime_owns_exported_symbol(proc->cname_override))
    {
        return;
    }

    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;
    const char *prev_sub_method_name = ctx->current_subprogram_method_name;
    const char *prev_sub_owner_class = ctx->current_subprogram_owner_class;
    const char *prev_sub_owner_class_full = ctx->current_subprogram_owner_class_full;
    int prev_is_nonstatic_class_method = ctx->current_subprogram_is_nonstatic_class_method;
    int prev_callee_rbx = ctx->callee_save_rbx_offset;
    int prev_callee_r12 = ctx->callee_save_r12_offset;
    int prev_callee_r13 = ctx->callee_save_r13_offset;
    int prev_callee_r14 = ctx->callee_save_r14_offset;
    int prev_callee_r15 = ctx->callee_save_r15_offset;

    push_stackscope();
    inst_list = NULL;

    /* Callee-save slots are allocated AFTER arguments and locals (below)
     * so that the t-section offsets account for the z and x section sizes. */
    if (proc_tree->tree_data.subprogram_data.nostackframe) {
        ctx->callee_save_rbx_offset = 0;
        ctx->callee_save_r12_offset = 0;
        ctx->callee_save_r13_offset = 0;
        ctx->callee_save_r14_offset = 0;
        ctx->callee_save_r15_offset = 0;
    }

    /* Static links are supported for nested procedures/functions (depth >= 1), but NOT for:
     * - Top-level procedures (depth 0)
     * - Class methods (which have owner_class set)
     *
     * Class methods receive 'self' in the first register and should not use static links.
     * When there are parameters, the static link is passed in %rdi and all arguments
     * are shifted by one register position. */
    int num_args = (proc->args_var == NULL) ? 0 : ListLength(proc->args_var);
    ctx->current_subprogram_id = proc->id;
    ctx->current_subprogram_mangled = sub_id;
    ctx->current_subprogram_method_name = proc->method_name;
    ctx->current_subprogram_owner_class = proc->owner_class;
    ctx->current_subprogram_owner_class_full = proc->owner_class_full;
    ctx->current_subprogram_is_nonstatic_class_method =
        (proc->owner_class != NULL && proc->method_name != NULL &&
         from_cparser_is_method_nonstatic_class_method(proc->owner_class, proc->method_name));
    EnterScope(symtab, 0);
    codegen_register_owner_unit_scope(ctx, symtab, proc->source_unit_index);
    codegen_register_local_types(proc->type_declarations, symtab);
    codegen_register_decl_list(proc->args_var, symtab, 1);
    codegen_register_decl_list(proc->declarations, symtab, 0);
    codegen_register_const_decls(proc->const_declarations, symtab);
    int lexical_depth = proc->nesting_level;
    if (lexical_depth < 0)
        lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    else if (lexical_depth <= 0 && ctx->current_subprogram_lexical_depth >= 0 &&
             ctx->current_subprogram_id != NULL)
        lexical_depth = ctx->current_subprogram_lexical_depth + 1;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    int is_nested_function = proc->is_nested;
    if (lexical_depth <= 0 && is_nested_function)
    {
        lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    }
    ctx->current_subprogram_lexical_depth = lexical_depth;
    int is_class_method = (proc->owner_class != NULL && !is_nested_function);
    StackNode_t *static_link = NULL;

    /* For static class methods, register class vars with the stack manager */
    if (is_class_method)
        codegen_add_class_vars_for_static_method(ctx->current_subprogram_owner_class,
            ctx->current_subprogram_method_name, symtab, ctx);

    /* Process arguments first to allocate their stack space */
    /* Nested procedures always receive a static link so they can forward it to callees,
     * even if they don't themselves capture any outer scope state. Class methods still
     * use the implicit `self` parameter instead. Top-level procedures do not receive
     * a static link. */
    int will_need_static_link = (!is_class_method && lexical_depth > 1);
    
    /* If there are arguments and we'll need a static link, shift argument registers by 1 */
    int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;
    /* For nostackframe functions, skip parameter saves — there is no frame,
     * so stores relative to %rbp would corrupt the caller's stack. */
    if (!proc_tree->tree_data.subprogram_data.nostackframe)
        inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, ctx, symtab, arg_start_index);

    /* Now add static link after arguments to avoid overlap */
    if (will_need_static_link)
    {
        /* Reserve space for static link (parent's frame pointer) after arguments
         * This ensures it doesn't overlap with argument storage */
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, sub_id, lexical_depth);
    }
    
    if (static_link != NULL)
    {
        char buffer[64];
        /* Static link always comes in the first argument register (platform-dependent) */
        const char *link_reg = current_arg_reg64(0);
        assert(link_reg != NULL && "current_arg_reg64(0) should never return NULL");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", link_reg, static_link->offset);
        inst_list = add_inst(inst_list, buffer);
    }
    
    codegen_function_locals(proc->declarations, ctx, symtab);

    /* Allocate callee-save slots AFTER args (z) and locals (x) so that
     * the t-section offset = z_offset + x_offset + t_offset doesn't collide. */
    if (!proc_tree->tree_data.subprogram_data.nostackframe) {
        StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
        StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
        StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
        StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
        StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
        ctx->callee_save_rbx_offset = rbx_slot->offset;
        ctx->callee_save_r12_offset = r12_slot->offset;
        ctx->callee_save_r13_offset = r13_slot->offset;
        ctx->callee_save_r14_offset = r14_slot->offset;
        ctx->callee_save_r15_offset = r15_slot->offset;
    }

    /* Recursively generate nested subprograms */
    codegen_subprograms(proc->subprograms, ctx, symtab);

    /* Set up asm parameter mapping for nostackframe functions.
       These functions skip the frame prologue, so inline asm should use
       ABI registers directly instead of stack offsets. */
    int prev_is_nostackframe = ctx->is_nostackframe;
    int prev_asm_param_count = ctx->asm_param_count;
    ctx->is_nostackframe = proc->nostackframe;
    ctx->asm_param_count = 0;
    if (proc->nostackframe && proc->args_var != NULL) {
        int pi = arg_start_index;
        ListNode_t *a = proc->args_var;
        while (a != NULL && pi < 16) {
            if (a->type == LIST_TREE && a->cur != NULL) {
                Tree_t *param = (Tree_t *)a->cur;
                if (param->type == TREE_VAR_DECL && param->tree_data.var_decl_data.ids != NULL) {
                    ListNode_t *id_node = param->tree_data.var_decl_data.ids;
                    while (id_node != NULL && pi < 16) {
                        if (id_node->cur != NULL) {
                            ctx->asm_params[ctx->asm_param_count].name = (const char *)id_node->cur;
                            ctx->asm_params[ctx->asm_param_count].reg_index = pi;
                            ctx->asm_param_count++;
                            pi++;
                        }
                        id_node = id_node->next;
                    }
                }
            }
            a = a->next;
        }
    }

    inst_list = codegen_var_initializers(proc->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(proc->statement_list, inst_list, ctx, symtab);

    /* For constructors, return Self in %rax.
     * Constructors receive Self in the first parameter and should return it
     * to allow constructor chaining and assignment. */
    int is_constructor = proc->is_constructor;

    if (is_constructor && num_args > 0)
    {
        /* Self is the first parameter. For class methods, it's in %rdi (or first stack slot).
         * Retrieve it and place in %rax for the return value. */
        ListNode_t *first_arg = (proc->args_var != NULL) ? proc->args_var : NULL;
        if (first_arg != NULL && first_arg->cur != NULL)
        {
            Tree_t *first_param = (Tree_t *)first_arg->cur;
            if (first_param != NULL && first_param->type == TREE_VAR_DECL)
            {
                struct Var *param_var = &first_param->tree_data.var_decl_data;
                if (param_var->ids != NULL && param_var->ids->cur != NULL)
                {
                    char *param_id = (char *)param_var->ids->cur;
                    StackNode_t *self_var = find_label(param_id);
                    if (self_var != NULL)
                    {
                        /* Self parameter is on the stack - load it into %rax for return */
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", self_var->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
            }
        }
    }
    
    codegen_emit_local_const_equivs(ctx, symtab);
    codegen_emit_const_decl_equivs_from_list(ctx, proc->const_declarations);
    codegen_function_header_ex_alias_vis(sub_id, ctx, proc->nostackframe, proc->cname_override, proc->defined_in_unit);
    if (!proc->nostackframe)
        codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer_ex(sub_id, ctx, proc->nostackframe);
    free_inst_list(inst_list);
    pop_stackscope();
    LeaveScope(symtab);

    ctx->is_nostackframe = prev_is_nostackframe;
    ctx->asm_param_count = prev_asm_param_count;
    ctx->current_subprogram_id = prev_sub_id;
    ctx->current_subprogram_mangled = prev_sub_mangled;
    ctx->current_subprogram_method_name = prev_sub_method_name;
    ctx->current_subprogram_owner_class = prev_sub_owner_class;
    ctx->current_subprogram_owner_class_full = prev_sub_owner_class_full;
    ctx->current_subprogram_is_nonstatic_class_method = prev_is_nonstatic_class_method;
    ctx->current_subprogram_lexical_depth = prev_depth;
    ctx->callee_save_rbx_offset = prev_callee_rbx;
    ctx->callee_save_r12_offset = prev_callee_r12;
    ctx->callee_save_r13_offset = prev_callee_r13;
    ctx->callee_save_r14_offset = prev_callee_r14;
    ctx->callee_save_r15_offset = prev_callee_r15;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for a function */
void codegen_function(Tree_t *func_tree, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_tree != NULL);
    assert(func_tree->type == TREE_SUBPROGRAM);
    assert(func_tree->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_FUNC);
    assert(ctx != NULL);
    assert(symtab != NULL);

    struct Subprogram *func;
    ListNode_t *inst_list;
    char buffer[50];
    char *sub_id;
    StackNode_t *return_var;
    StackNode_t *return_dest_slot = NULL;
    int has_record_return = 0;
    int returns_dynamic_array = 0;
    int dynamic_array_descriptor_size = 0;
    int dynamic_array_element_size = 0;
    int dynamic_array_lower_bound = 0;
    int prev_returns_dynamic_array = ctx->returns_dynamic_array;
    int prev_dynamic_array_descriptor_size = ctx->dynamic_array_descriptor_size;
    long long record_return_size = 0;

    func = &func_tree->tree_data.subprogram_data;
    sub_id = (func->mangled_id != NULL) ? func->mangled_id : func->id;

    if (codegen_runtime_owns_exported_symbol(sub_id) ||
        codegen_runtime_owns_exported_symbol(func->cname_override))
    {
        return;
    }

    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;
    const char *prev_sub_method_name = ctx->current_subprogram_method_name;
    const char *prev_sub_owner_class = ctx->current_subprogram_owner_class;
    const char *prev_sub_owner_class_full = ctx->current_subprogram_owner_class_full;
    int prev_is_nonstatic_class_method = ctx->current_subprogram_is_nonstatic_class_method;
    int prev_callee_rbx = ctx->callee_save_rbx_offset;
    int prev_callee_r12 = ctx->callee_save_r12_offset;
    int prev_callee_r13 = ctx->callee_save_r13_offset;
    int prev_callee_r14 = ctx->callee_save_r14_offset;
    int prev_callee_r15 = ctx->callee_save_r15_offset;

    push_stackscope();
    inst_list = NULL;

    /* Callee-save slots are allocated AFTER arguments and locals (below)
     * so that the t-section offsets account for the z and x section sizes. */
    if (func_tree->tree_data.subprogram_data.nostackframe) {
        ctx->callee_save_rbx_offset = 0;
        ctx->callee_save_r12_offset = 0;
        ctx->callee_save_r13_offset = 0;
        ctx->callee_save_r14_offset = 0;
        ctx->callee_save_r15_offset = 0;
    }

    /* Static links are supported for nested functions (depth >= 1), but NOT for:
     * - Top-level functions (depth 0)
     * - Class methods (which have owner_class set)
     *
     * Class methods receive 'self' in the first register and should not use static links.
     * When there are parameters, the static link is passed in %rdi (or second register
     * if function returns a record) and all arguments are shifted accordingly. */
    int num_args = (func->args_var == NULL) ? 0 : ListLength(func->args_var);
    ctx->current_subprogram_id = func->id;
    ctx->current_subprogram_mangled = sub_id;
    ctx->current_subprogram_method_name = func->method_name;
    ctx->current_subprogram_owner_class = func->owner_class;
    ctx->current_subprogram_owner_class_full = func->owner_class_full;
    ctx->current_subprogram_is_nonstatic_class_method =
        (func->owner_class != NULL && func->method_name != NULL &&
         from_cparser_is_method_nonstatic_class_method(func->owner_class, func->method_name));
    EnterScope(symtab, 0);
    codegen_register_owner_unit_scope(ctx, symtab, func->source_unit_index);
    codegen_register_local_types(func->type_declarations, symtab);
    codegen_register_decl_list(func->args_var, symtab, 1);
    codegen_register_decl_list(func->declarations, symtab, 0);
    codegen_register_const_decls(func->const_declarations, symtab);
    int lexical_depth = func->nesting_level;
    if (lexical_depth < 0)
        lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    else if (lexical_depth <= 0 && ctx->current_subprogram_lexical_depth >= 0 &&
             ctx->current_subprogram_id != NULL)
        lexical_depth = ctx->current_subprogram_lexical_depth + 1;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    int is_nested_function = func->is_nested;
    if (lexical_depth <= 0 && is_nested_function)
    {
        lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    }
    ctx->current_subprogram_lexical_depth = lexical_depth;
    int is_class_method = (func->owner_class != NULL && !is_nested_function);
    StackNode_t *static_link = NULL;

    /* For static class methods, register class vars with the stack manager */
    if (is_class_method)
        codegen_add_class_vars_for_static_method(ctx->current_subprogram_owner_class,
            ctx->current_subprogram_method_name, symtab, ctx);

    HashNode_t *func_node = NULL;

    if (symtab != NULL)
    {
        /* For overloaded functions, we need to find the correct overload by matching
         * the mangled name. FindIdent alone is insufficient because it returns the
         * first match, which might be a different overload. */
        if (func->mangled_id != NULL)
        {
            /* Try to find all identifiers with this name */
            ListNode_t *all_matches = FindAllIdents(symtab, func->id);
            ListNode_t *cur = all_matches;
            HashNode_t *same_mangled_same_unit = NULL;
            
            /* Find the one with matching mangled name */
            while (cur != NULL && func_node == NULL)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate != NULL &&
                    candidate->type != NULL &&
                    candidate->type->kind == TYPE_KIND_PROCEDURE &&
                    candidate->type->info.proc_info.definition == func_tree)
                {
                    func_node = candidate;
                    break;
                }
                if (candidate != NULL && candidate->mangled_id != NULL &&
                    strcmp(candidate->mangled_id, func->mangled_id) == 0)
                {
                    if (candidate->source_unit_index == func->source_unit_index)
                    {
                        same_mangled_same_unit = candidate;
                    }
                    else if (func_node == NULL)
                    {
                        func_node = candidate;
                    }
                }
                cur = cur->next;
            }

            if (same_mangled_same_unit != NULL)
                func_node = same_mangled_same_unit;
            
            if (all_matches != NULL)
                DestroyList(all_matches);
        }

        if (func_node != NULL &&
            !func->defined_in_unit &&
            func->source_unit_index == 0 &&
            func_node->source_unit_index != 0 &&
            func_node->type != NULL &&
            func_node->type->kind == TYPE_KIND_PROCEDURE &&
            func_node->type->info.proc_info.definition != func_tree)
        {
            func_node = NULL;
        }
        
        /* Fallback to simple lookup if no mangled name or no match found */
        if (func_node == NULL)
        {
            FindSymbol(&func_node, symtab, func->id);
            if (func_node != NULL &&
                func_node->type != NULL &&
                func_node->type->kind == TYPE_KIND_PROCEDURE &&
                func_node->type->info.proc_info.definition != NULL &&
                func_node->type->info.proc_info.definition != func_tree)
            {
                ListNode_t *all_matches = FindAllIdents(symtab, func->id);
                ListNode_t *cur = all_matches;
                while (cur != NULL)
                {
                    HashNode_t *candidate = (HashNode_t *)cur->cur;
                    if (candidate != NULL &&
                        candidate->type != NULL &&
                        candidate->type->kind == TYPE_KIND_PROCEDURE &&
                        candidate->type->info.proc_info.definition == func_tree)
                    {
                        func_node = candidate;
                        break;
                    }
                    cur = cur->next;
                }
                if (all_matches != NULL)
                    DestroyList(all_matches);
            }
        }
    }

    /* Check if function returns a record by examining KgpcType */
    if (func_node != NULL && func_node->type != NULL &&
        func_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
        if (return_type != NULL)
        {
            if (kgpc_type_is_record(return_type))
            {
                struct RecordType *record_desc = kgpc_type_get_record(return_type);
                if (record_desc != NULL &&
                    codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record_desc,
                        &record_return_size) == 0 && record_return_size > 0 &&
                    record_return_size <= INT_MAX)
                {
                    has_record_return = (record_return_size > 8);
                }
                else
                {
                    long long fallback_size = kgpc_type_sizeof(return_type);
                    if (fallback_size > 0 && fallback_size <= INT_MAX)
                    {
                        record_return_size = fallback_size;
                        has_record_return = (record_return_size > 8);
                    }
                    else
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to determine size for record return value of %s.", func->id);
                        record_return_size = 0;
                    }
                }
            }
            else if (return_type->kind == TYPE_KIND_ARRAY)
            {
                if (kgpc_type_is_dynamic_array(return_type))
                {
                    returns_dynamic_array = 1;
                    dynamic_array_element_size = codegen_dynamic_array_element_size_from_type(ctx, return_type);
                    dynamic_array_descriptor_size = codegen_dynamic_array_descriptor_bytes(dynamic_array_element_size);
                    dynamic_array_lower_bound = return_type->info.array_info.start_index;
                }
                else
                {
                    long long array_size = kgpc_type_sizeof(return_type);
                    if (array_size > 0 && array_size <= INT_MAX)
                    {
                        has_record_return = (array_size > 8);
                        record_return_size = array_size;
                    }
                    else
                    {
                        codegen_report_error(ctx,
                            "ERROR: Unable to determine size for array return value of %s.", func->id);
                        record_return_size = 0;
                    }
                }
            }
        }
    }
    else if (func_node != NULL && func_node->type != NULL &&
             kgpc_type_is_record(func_node->type))
    {
        struct RecordType *record = hashnode_get_record_type(func_node);
        if (record != NULL)
        {
            /* Get size from record */
            if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record,
                    &record_return_size) != 0 || record_return_size <= 0 ||
                record_return_size > INT_MAX)
            {
                long long fallback_size = kgpc_type_sizeof(func_node->type);
                if (fallback_size > 0 && fallback_size <= INT_MAX)
                {
                    record_return_size = fallback_size;
                    has_record_return = (record_return_size > 8);
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to determine size for record return value of %s.", func->id);
                    record_return_size = 0;
                }
            }
            else
            {
                has_record_return = (record_return_size > 8);
            }
        }
    }
    
    /* Also check return_type_id from the function tree for functions with record returns
     * that weren't looked up in symbol table correctly (e.g., class operators) */
    if (!has_record_return && func->return_type_id != NULL && symtab != NULL)
    {
        CODEGEN_DEBUG("DEBUG: Checking return_type_id='%s' for function '%s'\n", 
                func->return_type_id, func->id);
        HashNode_t *return_type_node = NULL;
        FindSymbol(&return_type_node, symtab, func->return_type_id);
        if (return_type_node != NULL)
        {
            CODEGEN_DEBUG("DEBUG: Found return type node\n");
            struct RecordType *record_type = hashnode_get_record_type(return_type_node);
            if (record_type != NULL)
            {
                CODEGEN_DEBUG("DEBUG: It's a record type!\n");
                if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                        record_type, &record_return_size) == 0 &&
                    record_return_size > 0 && record_return_size <= INT_MAX)
                {
                    CODEGEN_DEBUG("DEBUG: Setting has_record_return=1, size=%lld\n", record_return_size);
                    has_record_return = (record_return_size > 8);
                }
            }
            else if (return_type_node->type != NULL &&
                     return_type_node->type->kind != TYPE_KIND_ARRAY)
            {
                long long value_size = kgpc_type_sizeof(return_type_node->type);
                if (value_size > 0 && value_size <= INT_MAX)
                {
                    has_record_return = (value_size > 8);
                    record_return_size = value_size;
                }
            }
            else if (return_type_node->type != NULL &&
                     return_type_node->type->kind == TYPE_KIND_ARRAY &&
                     !kgpc_type_is_dynamic_array(return_type_node->type))
            {
                long long array_size = kgpc_type_sizeof(return_type_node->type);
                if (array_size > 0 && array_size <= INT_MAX)
                {
                    has_record_return = 1;
                    record_return_size = array_size;
                }
                else
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to determine size for array return value of %s.", func->id);
                    record_return_size = 0;
                }
            }
        }
        else
        {
            CODEGEN_DEBUG("DEBUG: return_type_node is NULL\n");
        }
    }
    else
    {
        CODEGEN_DEBUG("DEBUG: Skipped return_type_id check: has_record_return=%d, return_type_id=%s, symtab=%p\n",
                has_record_return, func->return_type_id ? func->return_type_id : "NULL", (void*)symtab);
    }
    
    /* Also check inline_return_type from the function tree for functions with inline record returns */
    if (!has_record_return && func->inline_return_type != NULL &&
        func->inline_return_type->base_type == RECORD_TYPE)
    {
        struct RecordType *inline_record = NULL;
        KgpcType *inline_kgpc = func->inline_return_type->kgpc_type;

        if (inline_kgpc != NULL && kgpc_type_is_record(inline_kgpc))
        {
            inline_record = kgpc_type_get_record(inline_kgpc);
        }

        if (inline_record == NULL &&
            func->inline_return_type->target_type_id != NULL && symtab != NULL)
        {
            HashNode_t *inline_type_node = NULL;
            FindSymbol(&inline_type_node, symtab,
                func->inline_return_type->target_type_id);
            if (inline_type_node != NULL)
                inline_record = hashnode_get_record_type(inline_type_node);
        }

        if (inline_record != NULL &&
            codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                inline_record, &record_return_size) == 0 &&
            record_return_size > 0 && record_return_size <= INT_MAX)
        {
            has_record_return = (record_return_size > 8);
        }
        else if (inline_kgpc != NULL)
        {
            long long inline_size = kgpc_type_sizeof(inline_kgpc);
            if (inline_size > 0 && inline_size <= INT_MAX)
            {
                record_return_size = inline_size;
                has_record_return = (record_return_size > 8);
            }
        }
    }

    if (!has_record_return && func->inline_return_type != NULL &&
        func->inline_return_type->kgpc_type != NULL &&
        func->inline_return_type->kgpc_type->kind == TYPE_KIND_ARRAY &&
        !kgpc_type_is_dynamic_array(func->inline_return_type->kgpc_type))
    {
        long long array_size = kgpc_type_sizeof(func->inline_return_type->kgpc_type);
        if (array_size > 0 && array_size <= INT_MAX)
        {
            has_record_return = (array_size > 8);
            record_return_size = array_size;
        }
        else
        {
            codegen_report_error(ctx,
                "ERROR: Unable to determine size for array return value of %s.", func->id);
            record_return_size = 0;
        }
    }

    /* Resolve dynamic array return types that were not found via func_node,
     * especially for class methods returning aliased dynamic arrays
     * (e.g. TUnicodeCharArray). */
    if (!returns_dynamic_array && func->return_type_id != NULL && symtab != NULL)
    {
        HashNode_t *return_type_node = NULL;
        FindSymbol(&return_type_node, symtab, func->return_type_id);
        if (return_type_node != NULL)
        {
            KgpcType *return_type = return_type_node->type;
            if (return_type == NULL)
            {
                struct TypeAlias *alias = hashnode_get_type_alias(return_type_node);
                if (alias != NULL)
                    return_type = create_kgpc_type_from_type_alias(alias, symtab, 0);
            }
            if (return_type != NULL && return_type->kind == TYPE_KIND_ARRAY &&
                kgpc_type_is_dynamic_array(return_type))
            {
                returns_dynamic_array = 1;
                dynamic_array_element_size =
                    codegen_dynamic_array_element_size_from_type(ctx, return_type);
                dynamic_array_descriptor_size =
                    codegen_dynamic_array_descriptor_bytes(dynamic_array_element_size);
                dynamic_array_lower_bound = return_type->info.array_info.start_index;
            }
        }
    }

    if (!has_record_return && func_node != NULL && func_node->type != NULL)
    {
        KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
        if (return_type != NULL && kgpc_type_is_shortstring(return_type))
        {
            long long shortstring_size = kgpc_type_sizeof(return_type);
            has_record_return = 1;
            record_return_size = shortstring_size > 0 ? shortstring_size : 256;
        }
    }

    /* Only nested functions receive static links (excluding class methods). */
    int will_need_static_link = (!is_class_method && lexical_depth > 1);
    
    /* Calculate argument start index:
     * - If function returns record: use index 1 (record pointer in first arg)
     * - If function will need static link: add 1 for static link
     * - Otherwise: use index 0 */
    int arg_start_index = has_record_return ? 1 : 0;
    if (will_need_static_link && num_args > 0)
        arg_start_index++;
    
    /* For nostackframe functions, skip parameter saves — there is no frame,
     * so stores relative to %rbp would corrupt the caller's stack. */
    if (!func_tree->tree_data.subprogram_data.nostackframe)
        inst_list = codegen_subprogram_arguments(func->args_var, inst_list, ctx, symtab,
            arg_start_index);

    /* Add static link after arguments to avoid stack overlap */
    if (will_need_static_link)
    {
        /* Reserve space for static link after arguments */
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, sub_id, lexical_depth);
    }
    
    if (static_link != NULL)
    {
        char link_buffer[64];
        /* Static link comes in the register right after the record return pointer (if any) */
        const char *link_reg = current_arg_reg64(has_record_return ? 1 : 0);
        assert(link_reg != NULL && "current_arg_reg64() should never return NULL for valid indices");
        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            link_reg, static_link->offset);
        inst_list = add_inst(inst_list, link_buffer);
    }
    
    int return_size = DOUBLEWORD;
    if (returns_dynamic_array)
        return_size = dynamic_array_descriptor_size;
    else if (has_record_return)
        return_size = (int)record_return_size;
    else if (func->return_type_id != NULL)
    {
        int return_type_id_size = codegen_return_type_id_storage_size(func->return_type_id);
        if (return_type_id_size > 0)
            return_size = return_type_id_size;
    }
    if (return_size == DOUBLEWORD && func_node != NULL && func_node->type != NULL &&
             func_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        /* Get return type from KgpcType */
        KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
        if (return_type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(return_type);
            if (alias != NULL && alias->target_type_id != NULL &&
                     pascal_identifier_equals(alias->target_type_id, "Single"))
            {
                return_size = 4;  /* Single is 4 bytes */
            }
            else
            {
                return_size = codegen_return_storage_size(return_type);
            }
        }
    }
    if (return_size == DOUBLEWORD && func_node != NULL && func_node->type != NULL &&
             func_node->type->kind == TYPE_KIND_PRIMITIVE)
    {
        int tag = kgpc_type_get_primitive_tag(func_node->type);
        struct TypeAlias *alias = kgpc_type_get_type_alias(func_node->type);
        if (alias != NULL && alias->storage_size > 0)
            return_size = (int)alias->storage_size;
        else if (tag == EXTENDED_TYPE)
            return_size = 10;
        else if (tag == REAL_TYPE || tag == STRING_TYPE || tag == POINTER_TYPE ||
                 tag == INT64_TYPE)
            return_size = 8;
        else if (tag == LONGINT_TYPE)
            return_size = DOUBLEWORD;  /* 32-bit FPC-compatible LongInt */
        else if (tag == BOOL)
            return_size = DOUBLEWORD;
    }
    if (return_size == DOUBLEWORD && func_node != NULL && func_node->type != NULL &&
             func_node->type->kind == TYPE_KIND_POINTER)
    {
        return_size = 8;
    }

    if (returns_dynamic_array)
        return_var = add_dynamic_array(func->id, dynamic_array_element_size,
            dynamic_array_lower_bound, 0, NULL);
    else
        return_var = add_l_x(func->id, return_size);

    /* Store dynamic array return info in context for exit statement handling */
    ctx->returns_dynamic_array = returns_dynamic_array;
    ctx->dynamic_array_descriptor_size = dynamic_array_descriptor_size;

    /* Allow Delphi-style Result alias in regular functions too. */
    add_result_alias_for_return_var(return_var);
    /* For class methods, also alias the unmangled method name to the return slot */
    if (func->method_name != NULL)
    {
        add_alias_for_return_var(return_var, func->method_name);
    }

    if (has_record_return)
        return_dest_slot = add_l_x("__record_return_dest__", (int)sizeof(void *));

    if (has_record_return && return_dest_slot != NULL)
    {
        const char *ret_reg = current_arg_reg64(0);
        if (ret_reg != NULL)
        {
            char ptr_buffer[64];
            snprintf(ptr_buffer, sizeof(ptr_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                ret_reg, return_dest_slot->offset);
            ListNode_t *record_return_inst = NULL;
            add_inst_invalidate_cache(); /* switching to different list */
            record_return_inst = add_inst(record_return_inst, ptr_buffer);
            add_inst_invalidate_cache(); /* ConcatList changes head */
            inst_list = ConcatList(record_return_inst, inst_list);
        }
    }

    codegen_function_locals(func->declarations, ctx, symtab);

    /* Allocate callee-save slots AFTER args (z) and locals (x) so that
     * the t-section offset = z_offset + x_offset + t_offset doesn't collide. */
    if (!func_tree->tree_data.subprogram_data.nostackframe) {
        StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
        StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
        StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
        StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
        StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
        ctx->callee_save_rbx_offset = rbx_slot->offset;
        ctx->callee_save_r12_offset = r12_slot->offset;
        ctx->callee_save_r13_offset = r13_slot->offset;
        ctx->callee_save_r14_offset = r14_slot->offset;
        ctx->callee_save_r15_offset = r15_slot->offset;
    }

    /* Recursively generate nested subprograms */
    {
        int saved_returns_dynamic_array = ctx->returns_dynamic_array;
        int saved_dynamic_array_descriptor_size = ctx->dynamic_array_descriptor_size;
        codegen_subprograms(func->subprograms, ctx, symtab);
        ctx->returns_dynamic_array = saved_returns_dynamic_array;
        ctx->dynamic_array_descriptor_size = saved_dynamic_array_descriptor_size;
    }

    /* Set up asm parameter mapping for nostackframe functions. */
    int prev_is_nostackframe = ctx->is_nostackframe;
    int prev_asm_param_count = ctx->asm_param_count;
    ctx->is_nostackframe = func->nostackframe;
    ctx->asm_param_count = 0;
    if (func->nostackframe && func->args_var != NULL) {
        int pi = arg_start_index;
        ListNode_t *a = func->args_var;
        while (a != NULL && pi < 16) {
            if (a->type == LIST_TREE && a->cur != NULL) {
                Tree_t *param = (Tree_t *)a->cur;
                if (param->type == TREE_VAR_DECL && param->tree_data.var_decl_data.ids != NULL) {
                    ListNode_t *id_node = param->tree_data.var_decl_data.ids;
                    while (id_node != NULL && pi < 16) {
                        if (id_node->cur != NULL) {
                            ctx->asm_params[ctx->asm_param_count].name = (const char *)id_node->cur;
                            ctx->asm_params[ctx->asm_param_count].reg_index = pi;
                            ctx->asm_param_count++;
                            pi++;
                        }
                        id_node = id_node->next;
                    }
                }
            }
            a = a->next;
        }
    }

    inst_list = codegen_var_initializers(func->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(func->statement_list, inst_list, ctx, symtab);

    /* For nostackframe+assembler functions, the asm block handles the return
     * value entirely.  Skip the compiler-generated return-value epilogue,
     * which would read from an invalid %rbp offset. */
    if (func->nostackframe)
    {
        /* Skip return value loading — asm is responsible */
    }
    else if (returns_dynamic_array)
    {
#if KGPC_ENABLE_REG_DEBUG
        const char *prev_reg_ctx = g_reg_debug_context;
        g_reg_debug_context = "dyn_array_return";
#endif
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
            addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate register for dynamic array return.");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                return_var->offset, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", dynamic_array_descriptor_size);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%esi\n", dynamic_array_descriptor_size);
                inst_list = add_inst(inst_list, buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_dynarray_clone_descriptor");
            free_arg_regs();
            free_reg(get_reg_stack(), addr_reg);
        }
#if KGPC_ENABLE_REG_DEBUG
        g_reg_debug_context = prev_reg_ctx;
#endif
    }
    else if (has_record_return && return_dest_slot != NULL && record_return_size > 0)
    {
        Register_t *dest_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_reg == NULL)
            dest_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        Register_t *src_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (src_reg == NULL)
            src_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (size_reg == NULL)
            size_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (dest_reg == NULL || src_reg == NULL || size_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            if (src_reg != NULL)
                free_reg(get_reg_stack(), src_reg);
            if (size_reg != NULL)
                free_reg(get_reg_stack(), size_reg);
            codegen_report_error(ctx,
                "ERROR: Unable to allocate registers for record return copy.");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                return_dest_slot->offset, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                return_var->offset, src_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                record_return_size, size_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                /* Move dest/src before size to avoid clobbering %r8. */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                /* Move in reverse order to avoid register conflicts when temp regs overlap with arg regs */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
            free_arg_regs();

            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                return_dest_slot->offset, RETURN_REG_64);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), dest_reg);
            free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), size_reg);
        }
    }
    else
    {
        /* Determine if return type is Real (floating-point) */
        int is_real_return = 0;
        if (func_node != NULL && func_node->type != NULL &&
            func_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
            if (return_type != NULL && return_type->kind == TYPE_KIND_PRIMITIVE)
            {
                int tag = kgpc_type_get_primitive_tag(return_type);
                if (is_real_family_type(tag))
                    is_real_return = 1;
            }
        }
        else if (func_node != NULL && func_node->type != NULL &&
                 func_node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            int tag = kgpc_type_get_primitive_tag(func_node->type);
            if (is_real_family_type(tag))
                is_real_return = 1;
        }
        
        /* Use movss for Single (4-byte), movsd for Double/Real (8-byte), return in xmm0.
         * Check element_size which stores the unaligned size, not size which may be padded. */
        long long unaligned_return_size = return_var->element_size > 0 ? return_var->element_size : return_var->size;
        if (is_real_return && return_var->element_size == 10)
            snprintf(buffer, 50, "\tfldt\t-%d(%%rbp)\n", return_var->offset);
        else if (is_real_return && unaligned_return_size <= 4)
            snprintf(buffer, 50, "\tmovss\t-%d(%%rbp), %%xmm0\n", return_var->offset);
        else if (is_real_return)
            snprintf(buffer, 50, "\tmovsd\t-%d(%%rbp), %%xmm0\n", return_var->offset);
        else
        {
            /* Use actual return type size (not stack slot size which may be
             * padded) to choose movl vs movq.  A 4-byte record allocated in
             * an 8-byte slot would otherwise read 4 bytes of garbage. */
            long long actual_return_size =
                return_var->element_size > 0 ? return_var->element_size : return_var->size;
            if (func->return_type_id != NULL)
            {
                int return_type_id_size = codegen_return_type_id_storage_size(func->return_type_id);
                if (return_type_id_size > 0)
                    actual_return_size = return_type_id_size;
            }
            if (func_node != NULL && func_node->type != NULL)
            {
                KgpcType *ret_type = kgpc_type_get_return_type(func_node->type);
                if (ret_type != NULL)
                {
                    actual_return_size = codegen_return_storage_size(ret_type);
                }
            }
            if (actual_return_size >= 8)
                snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_64);
            else
                snprintf(buffer, 50, "\tmovl\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_32);
        }
        inst_list = add_inst(inst_list, buffer);
    }

    codegen_emit_local_const_equivs(ctx, symtab);
    codegen_emit_const_decl_equivs_from_list(ctx, func->const_declarations);
    if (getenv("KGPC_DEBUG_NOSTACKFRAME") != NULL)
    {
        fprintf(stderr,
            "[KGPC_DEBUG_NOSTACKFRAME] func=%s nostackframe=%d method=%s owner=%s\n",
            sub_id ? sub_id : "<null>",
            func->nostackframe,
            func->method_name ? func->method_name : "<null>",
            func->owner_class ? func->owner_class : "<null>");
    }
    codegen_function_header_ex_alias_vis(sub_id, ctx, func->nostackframe, func->cname_override, func->defined_in_unit);
    if (!func->nostackframe)
        codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer_ex(sub_id, ctx, func->nostackframe);
    free_inst_list(inst_list);
    pop_stackscope();
    LeaveScope(symtab);

    ctx->is_nostackframe = prev_is_nostackframe;
    ctx->asm_param_count = prev_asm_param_count;
    ctx->current_subprogram_id = prev_sub_id;
    ctx->current_subprogram_mangled = prev_sub_mangled;
    ctx->current_subprogram_method_name = prev_sub_method_name;
    ctx->current_subprogram_owner_class = prev_sub_owner_class;
    ctx->current_subprogram_owner_class_full = prev_sub_owner_class_full;
    ctx->current_subprogram_is_nonstatic_class_method = prev_is_nonstatic_class_method;
    ctx->current_subprogram_lexical_depth = prev_depth;
    ctx->callee_save_rbx_offset = prev_callee_rbx;
    ctx->callee_save_r12_offset = prev_callee_r12;
    ctx->callee_save_r13_offset = prev_callee_r13;
    ctx->callee_save_r14_offset = prev_callee_r14;
    ctx->callee_save_r15_offset = prev_callee_r15;
    ctx->returns_dynamic_array = prev_returns_dynamic_array;
    ctx->dynamic_array_descriptor_size = prev_dynamic_array_descriptor_size;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Helper function to determine the size in bytes for a return type */
static int get_return_type_size(int return_type)
{
    if (return_type == EXTENDED_TYPE)
        return 10;
    if (return_type == STRING_TYPE || 
        return_type == POINTER_TYPE || return_type == REAL_TYPE ||
        return_type == INT64_TYPE)
        return 8;
    return 4; /* Default for INT_TYPE, LONGINT_TYPE, BOOL, CHAR_TYPE, etc. */
}

static int codegen_return_storage_size(KgpcType *return_type)
{
    if (return_type == NULL)
        return DOUBLEWORD;

    long long type_size = kgpc_type_sizeof(return_type);
    if (type_size > 0 && type_size <= INT_MAX)
        return (int)type_size;

    if (return_type->kind == TYPE_KIND_POINTER)
        return 8;

    if (return_type->kind == TYPE_KIND_PRIMITIVE)
    {
        int tag = kgpc_type_get_primitive_tag(return_type);
        if (tag == EXTENDED_TYPE)
            return 10;
        if (tag == REAL_TYPE || tag == STRING_TYPE || tag == POINTER_TYPE ||
            tag == INT64_TYPE || tag == QWORD_TYPE)
            return 8;
    }

    return DOUBLEWORD;
}

static int codegen_return_type_id_storage_size(const char *return_type_id)
{
    if (return_type_id == NULL)
        return 0;

    if (pascal_identifier_equals(return_type_id, "Single"))
        return 4;
    if (pascal_identifier_equals(return_type_id, "Extended"))
        return 10;
    if (pascal_identifier_equals(return_type_id, "Real") ||
        pascal_identifier_equals(return_type_id, "Double"))
        return 8;
    if (pascal_identifier_equals(return_type_id, "string") ||
        pascal_identifier_equals(return_type_id, "AnsiString") ||
        pascal_identifier_equals(return_type_id, "UnicodeString") ||
        pascal_identifier_equals(return_type_id, "WideString") ||
        pascal_identifier_equals(return_type_id, "Int64") ||
        pascal_identifier_equals(return_type_id, "QWord") ||
        pascal_identifier_equals(return_type_id, "UInt64") ||
        pascal_identifier_equals(return_type_id, "NativeInt") ||
        pascal_identifier_equals(return_type_id, "NativeUInt") ||
        pascal_identifier_equals(return_type_id, "SizeInt") ||
        pascal_identifier_equals(return_type_id, "SizeUInt") ||
        pascal_identifier_equals(return_type_id, "PtrInt") ||
        pascal_identifier_equals(return_type_id, "PtrUInt") ||
        pascal_identifier_equals(return_type_id, "IntPtr") ||
        pascal_identifier_equals(return_type_id, "UIntPtr") ||
        pascal_identifier_equals(return_type_id, "Pointer") ||
        pascal_identifier_equals(return_type_id, "PChar") ||
        pascal_identifier_equals(return_type_id, "PAnsiChar"))
        return 8;

    return 0;
}

/* Helper to add an alias label for a return variable so multiple identifiers share storage. */
static void add_alias_for_return_var(StackNode_t *return_var, const char *alias_label)
{
    if (return_var == NULL || alias_label == NULL || alias_label[0] == '\0')
        return;
    
    /* Create a stack node pointing to the same offset */
    StackNode_t *result_alias = init_stack_node(return_var->offset, (char *)alias_label, return_var->size);
    if (result_alias == NULL)
        return;
    
    result_alias->element_size = return_var->element_size;
    result_alias->is_alias = 1;
    result_alias->is_dynamic = return_var->is_dynamic;  /* Copy dynamic array flag */
    if (return_var->is_static && return_var->static_label != NULL)
        result_alias->static_label = strdup(return_var->static_label);
    
    /* Add it to the x list in the current stack scope using the list API */
    StackScope_t *cur_scope = get_cur_scope();
    if (cur_scope != NULL)
    {
        ListNode_t *new_list_node = CreateListNode(result_alias, LIST_UNSPECIFIED);
        if (new_list_node != NULL)
        {
            if (cur_scope->x == NULL)
            {
                cur_scope->x = new_list_node;
                cur_scope->x_tail = new_list_node;
            }
            else
            {
                cur_scope->x_tail->next = new_list_node;
                cur_scope->x_tail = new_list_node;
            }
        }
    }
}

static int add_absolute_var_alias(const char *alias_label, const char *target_label)
{
    if (alias_label == NULL || alias_label[0] == '\0' ||
        target_label == NULL || target_label[0] == '\0')
        return 1;

    StackNode_t *target = find_label((char *)target_label);
    if (target == NULL)
        return 1;

    StackNode_t *alias = init_stack_node(target->offset, (char *)alias_label, target->size);
    if (alias == NULL)
        return 1;

    alias->element_size = target->element_size;
    alias->is_alias = 1;
    alias->is_static = target->is_static;
    alias->is_reference = target->is_reference;
    if (target->static_label != NULL)
        alias->static_label = strdup(target->static_label);

    StackScope_t *cur_scope = get_cur_scope();
    if (cur_scope == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    ListNode_t *new_list_node = CreateListNode(alias, LIST_UNSPECIFIED);
    if (new_list_node == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    if (cur_scope->x == NULL)
    {
        cur_scope->x = new_list_node;
        cur_scope->x_tail = new_list_node;
    }
    else
    {
        cur_scope->x_tail->next = new_list_node;
        cur_scope->x_tail = new_list_node;
    }

    return 0;
}

static int add_absolute_static_symbol_alias(const char *alias_label, const char *target_symbol,
    int alias_size)
{
    if (alias_label == NULL || alias_label[0] == '\0' ||
        target_symbol == NULL || target_symbol[0] == '\0' ||
        alias_size <= 0)
        return 1;

    StackNode_t *alias = init_stack_node(0, (char *)alias_label, alias_size);
    if (alias == NULL)
        return 1;

    alias->element_size = alias_size;
    alias->is_alias = 1;
    alias->is_static = 1;
    alias->static_label = strdup(target_symbol);
    if (alias->static_label == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    StackScope_t *cur_scope = get_cur_scope();
    if (cur_scope == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    ListNode_t *new_list_node = CreateListNode(alias, LIST_UNSPECIFIED);
    if (new_list_node == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    if (cur_scope->x == NULL)
    {
        cur_scope->x = new_list_node;
        cur_scope->x_tail = new_list_node;
    }
    else
    {
        cur_scope->x_tail->next = new_list_node;
        cur_scope->x_tail = new_list_node;
    }

    return 0;
}

/* Add absolute alias with offset for record field access.
 * Creates an alias variable that points to base_var + field_offset. */
static int add_absolute_var_alias_with_offset(const char *alias_label, const char *target_label,
    int field_offset, int alias_size)
{
    if (alias_label == NULL || alias_label[0] == '\0' ||
        target_label == NULL || target_label[0] == '\0')
        return 1;

    StackNode_t *target = find_label((char *)target_label);
    if (target == NULL)
        return 1;

    /* Create alias with adjusted offset: target offset + field offset */
    int adjusted_offset = target->offset + field_offset;
    StackNode_t *alias = init_stack_node(adjusted_offset, (char *)alias_label, alias_size);
    if (alias == NULL)
        return 1;

    alias->element_size = alias_size;
    alias->is_alias = 1;
    alias->is_static = target->is_static;
    alias->is_reference = target->is_reference;
    if (target->static_label != NULL)
    {
        /* For static variables, create a new label with offset suffix.
         * The assembly will reference base+offset. */
        size_t label_len = strlen(target->static_label) + 32;
        char *offset_label = (char *)malloc(label_len);
        if (offset_label != NULL)
        {
            snprintf(offset_label, label_len, "%s+%d", target->static_label, field_offset);
            alias->static_label = offset_label;
        }
        else
        {
            alias->static_label = strdup(target->static_label);
        }
    }

    StackScope_t *cur_scope = get_cur_scope();
    if (cur_scope == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    ListNode_t *new_list_node = CreateListNode(alias, LIST_UNSPECIFIED);
    if (new_list_node == NULL)
    {
        destroy_stack_node(alias);
        return 1;
    }

    if (cur_scope->x == NULL)
    {
        cur_scope->x = new_list_node;
        cur_scope->x_tail = new_list_node;
    }
    else
    {
        cur_scope->x_tail->next = new_list_node;
        cur_scope->x_tail = new_list_node;
    }

    return 0;
}

/* Helper function to add a Result alias for anonymous function return variable */
static void add_result_alias_for_return_var(StackNode_t *return_var)
{
    add_alias_for_return_var(return_var, "Result");
}

static int codegen_dynamic_array_element_size_from_type(CodeGenContext *ctx, KgpcType *array_type)
{
    if (array_type == NULL || array_type->kind != TYPE_KIND_ARRAY)
        return DOUBLEWORD;

    KgpcType *element_type = array_type->info.array_info.element_type;
    if (element_type == NULL)
        return DOUBLEWORD;

    switch (element_type->kind)
    {
        case TYPE_KIND_PRIMITIVE:
        {
            if (element_type->type_alias != NULL &&
                element_type->type_alias->storage_size > 0 &&
                element_type->type_alias->storage_size <= INT_MAX)
            {
                return (int)element_type->type_alias->storage_size;
            }
            int tag = kgpc_type_get_primitive_tag(element_type);
            switch (tag)
            {
                case LONGINT_TYPE:
                    return DOUBLEWORD;  // 4 bytes for FPC's 32-bit LongInt
                case REAL_TYPE:
                case STRING_TYPE:
                case POINTER_TYPE:
                    return 8;
                case SHORTSTRING_TYPE:
                    return 256;
                case CHAR_TYPE:
                case BOOL:
                    return 1;
                default:
                    return DOUBLEWORD;
            }
        }
        case TYPE_KIND_RECORD:
        {
            struct RecordType *record = kgpc_type_get_record(element_type);
            long long size = 0;
            if (record != NULL &&
                codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record, &size) == 0 &&
                size > 0 && size <= INT_MAX)
                return (int)size;
            return DOUBLEWORD;
        }
        case TYPE_KIND_POINTER:
        case TYPE_KIND_PROCEDURE:
            return 8;
        case TYPE_KIND_ARRAY:
            return DOUBLEWORD;
        default:
            return DOUBLEWORD;
    }
}

static int codegen_dynamic_array_descriptor_bytes(int element_size)
{
    int descriptor_size = 4 * DOUBLEWORD;
    int needed = element_size * 2;
    if (needed > descriptor_size)
        descriptor_size = needed;
    return descriptor_size;
}

/* Code generation for an anonymous function/procedure
 * This generates the function body and returns the function's label name.
 * The caller is responsible for generating code to load the address of this function.
 */
void codegen_anonymous_method(struct Expression *expr, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    
    assert(expr != NULL);
    assert(expr->type == EXPR_ANONYMOUS_FUNCTION || expr->type == EXPR_ANONYMOUS_PROCEDURE);
    assert(ctx != NULL);
    assert(symtab != NULL);
    
    struct AnonymousMethod *anon = &expr->expr_data.anonymous_method_data;
    
    if (anon->generated_name == NULL)
    {
        codegen_report_error(ctx, "ERROR: Anonymous method missing generated name at line %d", expr->line_num);
        return;
    }
    
    if (anon->body == NULL)
    {
        /* Empty body - generate a no-op function */
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Anonymous method %s has no body, generating no-op\n", anon->generated_name);
        #endif
    }
    
    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;
    int prev_callee_rbx = ctx->callee_save_rbx_offset;
    int prev_callee_r12 = ctx->callee_save_r12_offset;
    int prev_callee_r13 = ctx->callee_save_r13_offset;
    int prev_callee_r14 = ctx->callee_save_r14_offset;
    int prev_callee_r15 = ctx->callee_save_r15_offset;

    push_stackscope();

    /* Allocate stack slots for callee-saved registers */
    ListNode_t *inst_list = NULL;
    int num_args = (anon->parameters == NULL) ? 0 : ListLength(anon->parameters);
    int lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    ctx->current_subprogram_lexical_depth = lexical_depth;
    int is_nested = (lexical_depth >= 1);

    ctx->current_subprogram_id = anon->generated_name;
    ctx->current_subprogram_mangled = anon->generated_name;

    /* Anonymous methods are always nested (they're defined inside some other context).
     * They always need a static link to access variables from their parent scope (closure).
     * The static link is passed in %rdi (first register) and parameters are shifted by 1.
     */
    StackNode_t *static_link = NULL;
    int will_need_static_link = is_nested;
    int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;

    /* Process parameters (convert from TREE_VAR_DECL to stack allocations) */
    inst_list = codegen_subprogram_arguments(anon->parameters, inst_list, ctx, symtab, arg_start_index);

    /* Add static link after parameters */
    if (will_need_static_link)
    {
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, anon->generated_name, lexical_depth);
        
        if (static_link != NULL)
        {
            char buffer[64];
            /* Static link always comes in %rdi (first register) */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rdi, -%d(%%rbp)\n", static_link->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    
    /* For functions (not procedures), allocate space for the return value */
    StackNode_t *return_var = NULL;
    if (anon->is_function && anon->return_type != -1)
    {
        int return_size = get_return_type_size(anon->return_type);
        return_var = add_l_x(anon->generated_name, return_size);
        
        /* Also add "Result" as an alias at the same stack offset */
        add_result_alias_for_return_var(return_var);
    }
    
    /* No local variable declarations in anonymous methods (they're inline) */
    /* No nested subprograms in anonymous methods */

    /* Allocate callee-save slots AFTER args (z) and locals (x) so that
     * the t-section offset = z_offset + x_offset + t_offset doesn't collide. */
    {
        StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
        StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
        StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
        StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
        StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
        ctx->callee_save_rbx_offset = rbx_slot->offset;
        ctx->callee_save_r12_offset = r12_slot->offset;
        ctx->callee_save_r13_offset = r13_slot->offset;
        ctx->callee_save_r14_offset = r14_slot->offset;
        ctx->callee_save_r15_offset = r15_slot->offset;
    }

    /* Generate the body */
    if (anon->body != NULL)
    {
        inst_list = codegen_stmt(anon->body, inst_list, ctx, symtab);
    }
    
    /* For functions, move return value to correct return register */
    if (anon->is_function && return_var != NULL)
    {
        char buffer[64];
        int return_is_real = (anon->return_type == REAL_TYPE || anon->return_type == EXTENDED_TYPE);
        int return_size = get_return_type_size(anon->return_type);
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
    
    /* Generate the function header, stack allocation, body, and footer */
    codegen_function_header(anon->generated_name, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(anon->generated_name, ctx);
    
    free_inst_list(inst_list);
    pop_stackscope();
    
    ctx->current_subprogram_id = prev_sub_id;
    ctx->current_subprogram_mangled = prev_sub_mangled;
    ctx->current_subprogram_lexical_depth = prev_depth;
    ctx->callee_save_rbx_offset = prev_callee_rbx;
    ctx->callee_save_r12_offset = prev_callee_r12;
    ctx->callee_save_r13_offset = prev_callee_r13;
    ctx->callee_save_r14_offset = prev_callee_r14;
    ctx->callee_save_r15_offset = prev_callee_r15;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for subprogram arguments */
ListNode_t *codegen_subprogram_arguments(ListNode_t *args, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab, int arg_start_index)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    Tree_t *arg_decl;
    int type;
    ListNode_t *arg_ids;
    const char *arg_reg;
    char buffer[50];
    StackNode_t *arg_stack;
    int next_gpr_index = 0;
    int next_sse_index = 0;
    /* Positive offsets from %rbp to reach stack-passed arguments.
     * System V: 16(%rbp) is the first stack arg (after saved rbp + return addr).
     * Windows x64: 48(%rbp) is the first stack arg (after saved rbp + return addr + 32-byte shadow space). */
    int stack_arg_offset = codegen_target_is_windows() ? 48 : 16;
    ListNode_t *record_param_queue = NULL;
    int param_index = 0;

    assert(ctx != NULL);

    if (arg_start_index < 0)
        arg_start_index = 0;

    next_gpr_index = arg_start_index;

    /* Pre-pass phase 1: Check if there's any record/dynarray parameter that will need kgpc_move.
     * If so, we need to pre-allocate ALL parameter storage and save registers to final locations
     * before processing starts, to avoid register clobbering issues. */
    ListNode_t *args_scan = args;
    int has_record_or_dynarray = 0;
    int param_count_for_alloc = 0;
    
    while(args_scan != NULL)
    {
        Tree_t *scan_decl = (Tree_t *)args_scan->cur;
        if (scan_decl->type == TREE_VAR_DECL)
        {
            ListNode_t *scan_ids = scan_decl->tree_data.var_decl_data.ids;
            int scan_type = scan_decl->tree_data.var_decl_data.type;
            KgpcType *scan_cached_type = scan_decl->tree_data.var_decl_data.cached_kgpc_type;
            int is_var = scan_decl->tree_data.var_decl_data.is_var_param;
            
            while(scan_ids != NULL)
            {
                param_count_for_alloc++;
                
                /* Check if this parameter is a record or dynarray that needs kgpc_move */
                if (!is_var)
                {
                    HashNode_t *scan_type_node = NULL;
                    if (scan_type == UNKNOWN_TYPE && scan_decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL)
                    {
                        FindSymbol(&scan_type_node, symtab, scan_decl->tree_data.var_decl_data.type_id);
                    }
                    
                    struct RecordType *rec = NULL;
                    if (scan_type_node != NULL)
                        rec = get_record_type_from_node(scan_type_node);
                    if (rec == NULL && scan_cached_type != NULL)
                    {
                        HashNode_t cached_node;
                        memset(&cached_node, 0, sizeof(cached_node));
                        cached_node.type = scan_cached_type;
                        rec = get_record_type_from_node(&cached_node);
                    }
                    
                    if (rec != NULL)
                    {
                        has_record_or_dynarray = 1;
                    }
                    else if (scan_cached_type != NULL &&
                             scan_cached_type->kind == TYPE_KIND_ARRAY &&
                             kgpc_type_is_dynamic_array(scan_cached_type))
                    {
                        has_record_or_dynarray = 1;
                    }
                    else if (scan_type_node != NULL && scan_type_node->type != NULL &&
                             scan_type_node->type->kind == TYPE_KIND_ARRAY &&
                             kgpc_type_is_dynamic_array(scan_type_node->type))
                    {
                        has_record_or_dynarray = 1;
                    }
                    else
                    {
                        KgpcType *param_type = NULL;
                        if (scan_type_node != NULL)
                            param_type = scan_type_node->type;
                        else if (scan_cached_type != NULL)
                            param_type = scan_cached_type;
                        if (param_type != NULL &&
                            param_type->kind == TYPE_KIND_PRIMITIVE &&
                            kgpc_type_get_primitive_tag(param_type) == SET_TYPE &&
                            kgpc_type_sizeof(param_type) > 4)
                        {
                            has_record_or_dynarray = 1;
                        }
                    }
                }
                scan_ids = scan_ids->next;
            }
        }
        else if (scan_decl->type == TREE_ARR_DECL)
        {
            ListNode_t *scan_ids = scan_decl->tree_data.arr_decl_data.ids;
            while(scan_ids != NULL)
            {
                param_count_for_alloc++;
                scan_ids = scan_ids->next;
            }
        }
        args_scan = args_scan->next;
    }
    
    /* Pre-pass phase 2: If there are record/dynarray parameters, pre-allocate ALL storage
     * and save registers to their final locations before processing starts. */
    if (has_record_or_dynarray && param_count_for_alloc > 0)
    {
        args_scan = args;
        int scan_gpr_index = arg_start_index;
        int scan_sse_index = 0;
        while(args_scan != NULL)
        {
            Tree_t *scan_decl = (Tree_t *)args_scan->cur;
            if (scan_decl->type == TREE_VAR_DECL)
            {
                int scan_type = scan_decl->tree_data.var_decl_data.type;
                int scan_is_var = scan_decl->tree_data.var_decl_data.is_var_param;
                KgpcType *scan_cached_type = scan_decl->tree_data.var_decl_data.cached_kgpc_type;
                ListNode_t *scan_ids = scan_decl->tree_data.var_decl_data.ids;
                while(scan_ids != NULL)
                {
                    int scan_real_storage_size = 8;
                    if (!scan_is_var && (scan_type == REAL_TYPE || scan_type == EXTENDED_TYPE))
                        scan_real_storage_size = codegen_real_param_storage_size(scan_decl,
                            NULL, scan_cached_type);
                    /* Float (REAL_TYPE) parameters that are not passed by reference
                     * use SSE/XMM registers, NOT integer registers. Skip integer
                     * register allocation for them so subsequent integer params
                     * get the correct registers. SSE regs are not clobbered by
                     * kgpc_move calls, so no presave slot is needed. */
                    if (!scan_is_var &&
                        (scan_type == REAL_TYPE || scan_type == EXTENDED_TYPE) &&
                        scan_real_storage_size < 16)
                    {
                        if (scan_sse_index < kgpc_max_sse_arg_regs())
                            scan_sse_index++;
                        scan_ids = scan_ids->next;
                        continue;
                    }
                    const char *param_reg = alloc_integer_arg_reg(1, &scan_gpr_index);
                    if (param_reg != NULL)
                    {
                        /* Allocate final storage slot and save register directly */
                        char temp_name[64];
                        snprintf(temp_name, sizeof(temp_name), "__presaved_%s__", (char *)scan_ids->cur);
                        StackNode_t *presaved_slot = add_q_z(temp_name);
                        if (presaved_slot != NULL)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                                param_reg, presaved_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                    scan_ids = scan_ids->next;
                }
            }
            else if (scan_decl->type == TREE_ARR_DECL)
            {
                ListNode_t *scan_ids = scan_decl->tree_data.arr_decl_data.ids;
                while(scan_ids != NULL)
                {
                    const char *param_reg = alloc_integer_arg_reg(1, &scan_gpr_index);
                    if (param_reg != NULL)
                    {
                        char temp_name[64];
                        snprintf(temp_name, sizeof(temp_name), "__presaved_%s__", (char *)scan_ids->cur);
                        StackNode_t *presaved_slot = add_q_z(temp_name);
                        if (presaved_slot != NULL)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                                param_reg, presaved_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                    scan_ids = scan_ids->next;
                }
            }
            args_scan = args_scan->next;
        }
    }

    /* Reset for main processing pass */
    next_gpr_index = arg_start_index;

    while(args != NULL)
    {
        arg_decl = (Tree_t *)args->cur;
        switch(arg_decl->type)
        {
            case TREE_VAR_DECL:
                arg_ids = arg_decl->tree_data.var_decl_data.ids;
                type = arg_decl->tree_data.var_decl_data.type;
                HashNode_t *resolved_type_node = NULL;
                KgpcType *cached_arg_type = arg_decl->tree_data.var_decl_data.cached_kgpc_type;
                int inferred_type_tag = type;
                HashNode_t cached_arg_node;
                HashNode_t *cached_arg_node_ptr = NULL;
                if (cached_arg_type != NULL)
                {
                    memset(&cached_arg_node, 0, sizeof(cached_arg_node));
                    cached_arg_node.type = cached_arg_type;
                    cached_arg_node_ptr = &cached_arg_node;
                }

                // Resolve type aliases if needed
                if (type == UNKNOWN_TYPE && arg_decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    FindSymbol(&type_node, symtab, arg_decl->tree_data.var_decl_data.type_id);
                    if (type_node != NULL)
                    {
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL)
                        {
                            type = alias->base_type;
                        }
                        resolved_type_node = type_node;
                    }
                }

                /* If the legacy type tag was UNKNOWN, derive it from the resolved KgpcType
                 * so that class/pointer parameters are treated as 64-bit values. */
                if (inferred_type_tag == UNKNOWN_TYPE)
                {
                    if (resolved_type_node != NULL && resolved_type_node->type != NULL)
                        inferred_type_tag = codegen_tag_from_kgpc(resolved_type_node->type);
                    else if (cached_arg_type != NULL)
                        inferred_type_tag = codegen_tag_from_kgpc(cached_arg_type);
                }

                while(arg_ids != NULL)
                {
                    int tree_is_var_param = arg_decl->tree_data.var_decl_data.is_var_param;
                    int is_untyped_param = arg_decl->tree_data.var_decl_data.is_untyped_param;
                    if (is_untyped_param)
                        tree_is_var_param = 1;
                    int symbol_is_var_param = tree_is_var_param;
                    int is_self_param = 0;
                    if (arg_decl->tree_data.var_decl_data.ids != NULL)
                    {
                        const char *first_id = (const char *)arg_decl->tree_data.var_decl_data.ids->cur;
                        if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
                            is_self_param = 1;
                    }
                    if (is_self_param && codegen_self_param_is_class(arg_decl, symtab))
                    {
                        tree_is_var_param = 0;
                        symbol_is_var_param = 0;
                    }
                    struct RecordType *record_type_info = NULL;
                    int is_dynarray_param = 0;
                    int dynarray_elem_size = 0;

                    if (getenv("KGPC_DEBUG_ARG_TYPES") != NULL && arg_ids->cur != NULL)
                    {
                        fprintf(stderr,
                            "[CODEGEN] Arg %s: type=%d inferred=%d type_id=%s resolved_type_node=%p cached=%p\n",
                            (char *)arg_ids->cur, type, inferred_type_tag,
                            arg_decl->tree_data.var_decl_data.type_id ?
                                arg_decl->tree_data.var_decl_data.type_id : "(null)",
                            (void *)resolved_type_node, (void *)cached_arg_type);
                        if (resolved_type_node != NULL && resolved_type_node->type != NULL)
                        {
                            fprintf(stderr, "[CODEGEN]   resolved_type_node->type kind=%d tag=%d\n",
                                resolved_type_node->type->kind,
                                codegen_tag_from_kgpc(resolved_type_node->type));
                        }
                        if (cached_arg_type != NULL)
                        {
                            fprintf(stderr, "[CODEGEN]   cached_arg_type kind=%d tag=%d\n",
                                cached_arg_type->kind,
                                codegen_tag_from_kgpc(cached_arg_type));
                        }
                    }
                    if (!symbol_is_var_param)
                    {
                        if (resolved_type_node != NULL)
                            record_type_info = get_record_type_from_node(resolved_type_node);
                        if (record_type_info == NULL && cached_arg_node_ptr != NULL)
                            record_type_info = get_record_type_from_node(cached_arg_node_ptr);
                        if (record_type_info == NULL)
                        {
                            KgpcType *param_type = NULL;
                            if (resolved_type_node != NULL)
                                param_type = resolved_type_node->type;
                            else if (cached_arg_type != NULL)
                                param_type = cached_arg_type;
                            if (param_type != NULL && kgpc_type_is_record(param_type))
                            {
                                record_type_info = kgpc_type_get_record(param_type);
                            }
                            else if (param_type != NULL &&
                                param_type->kind == TYPE_KIND_ARRAY &&
                                kgpc_type_is_dynamic_array(param_type))
                            {
                                is_dynarray_param = 1;
                                dynarray_elem_size = (int)kgpc_type_get_array_element_size(param_type);
                                if (dynarray_elem_size <= 0)
                                    dynarray_elem_size = 1;
                            }
                        }
                    }

                    int is_char_set_param = 0;
                    long long char_set_size = 0;
                    if (!symbol_is_var_param)
                    {
                        KgpcType *param_type = NULL;
                        if (resolved_type_node != NULL)
                            param_type = resolved_type_node->type;
                        else if (cached_arg_type != NULL)
                            param_type = cached_arg_type;
                        if (param_type != NULL &&
                            param_type->kind == TYPE_KIND_PRIMITIVE &&
                            kgpc_type_get_primitive_tag(param_type) == SET_TYPE)
                        {
                            long long size = kgpc_type_sizeof(param_type);
                            if (size > 4)
                            {
                                is_char_set_param = 1;
                                char_set_size = size;
                            }
                        }
                    }

                    if (record_type_info != NULL || is_dynarray_param || is_char_set_param)
                    {
                        long long record_size = 0;
                        if (is_dynarray_param)
                        {
                            record_size = codegen_dynamic_array_descriptor_bytes(dynarray_elem_size);
                        }
                        else if (is_char_set_param)
                        {
                            record_size = char_set_size;
                        }
                        else if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                                record_type_info, &record_size) != 0 || record_size < 0)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to determine size for record parameter %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        if (record_size > INT_MAX)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Record parameter %s exceeds supported size limits.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        RecordParamWork *work = (RecordParamWork *)malloc(sizeof(RecordParamWork));
                        if (work == NULL)
                        {
                            codegen_report_error(ctx, "ERROR: Unable to allocate record param work.");
                            return inst_list;
                        }

                        work->id = (const char *)arg_ids->cur;
                        work->size = (int)record_size;
                        work->stack_arg_offset = 0;
                        work->has_stack_arg = 0;
                        work->arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);
                        work->is_dynarray = is_dynarray_param;
                        work->dynarray_elem_size = dynarray_elem_size;
                        work->dynarray_lower_bound = 0;
                        work->arg_index = param_index;

                        if (work->arg_reg == NULL)
                        {
                            work->stack_arg_offset = stack_arg_offset;
                            work->has_stack_arg = 1;
                            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                        }

                        ListNode_t *work_node = CreateListNode(work, LIST_UNSPECIFIED);
                        if (work_node == NULL)
                        {
                            free(work);
                            codegen_report_error(ctx, "ERROR: Unable to enqueue record param.");
                            return inst_list;
                        }

                        if (record_param_queue == NULL)
                            record_param_queue = work_node;
                        else
                            record_param_queue = PushListNodeBack(record_param_queue, work_node);

                        arg_ids = arg_ids->next;
                        param_index++;
                        continue;
                    }

                    // Var parameters are passed by reference (as pointers), so always use 64-bit
                    // Also use 64-bit for strings, explicit pointers, arrays, or aliases that
                    // explicitly require 8-byte storage (e.g., NativeUInt/Int64).
                    int is_var_param = symbol_is_var_param;
                    int is_array_type = 0;
                    int type_requires_qword = 0;
                    int real_storage_size = 8;
                    int use_extended_stack_param = 0;
                    int is_shortstring_param = 0;
                    
                    /* Determine if parameter is an array type via resolved type only */
                    if (resolved_type_node != NULL && resolved_type_node->type != NULL &&
                             kgpc_type_is_array(resolved_type_node->type))
                    {
                        is_array_type = 1;
                        type_requires_qword = kgpc_type_uses_qword(resolved_type_node->type);
                        struct TypeAlias *alias = kgpc_type_get_type_alias(resolved_type_node->type);
                        if (kgpc_type_is_shortstring(resolved_type_node->type) ||
                            (alias != NULL && alias->is_shortstring))
                            is_shortstring_param = 1;
                    }
                    else if (cached_arg_type != NULL &&
                        kgpc_type_is_array(cached_arg_type))
                    {
                        is_array_type = 1;
                        type_requires_qword = kgpc_type_uses_qword(cached_arg_type);
                        struct TypeAlias *alias = kgpc_type_get_type_alias(cached_arg_type);
                        if (kgpc_type_is_shortstring(cached_arg_type) ||
                            (alias != NULL && alias->is_shortstring))
                            is_shortstring_param = 1;
                    }
                    else if (resolved_type_node != NULL && resolved_type_node->type != NULL)
                    {
                        type_requires_qword = kgpc_type_uses_qword(resolved_type_node->type);
                        struct TypeAlias *alias = kgpc_type_get_type_alias(resolved_type_node->type);
                        if (kgpc_type_is_shortstring(resolved_type_node->type) ||
                            (alias != NULL && alias->is_shortstring))
                            is_shortstring_param = 1;
                    }
                    else if (cached_arg_type != NULL)
                    {
                        type_requires_qword = kgpc_type_uses_qword(cached_arg_type);
                        struct TypeAlias *alias = kgpc_type_get_type_alias(cached_arg_type);
                        if (kgpc_type_is_shortstring(cached_arg_type) ||
                            (alias != NULL && alias->is_shortstring))
                            is_shortstring_param = 1;
                    }

                    if (inferred_type_tag == REAL_TYPE || inferred_type_tag == EXTENDED_TYPE)
                        real_storage_size = codegen_real_param_storage_size(arg_decl,
                            resolved_type_node, cached_arg_type);

                    use_extended_stack_param =
                        (!is_var_param && !is_array_type && !is_shortstring_param &&
                         (inferred_type_tag == REAL_TYPE || inferred_type_tag == EXTENDED_TYPE) &&
                         real_storage_size == 16);
                    
                     int use_64bit = is_var_param || is_array_type || type_requires_qword ||
                         (inferred_type_tag == STRING_TYPE || inferred_type_tag == POINTER_TYPE ||
                          type == PROCEDURE ||
                          ((inferred_type_tag == REAL_TYPE || inferred_type_tag == EXTENDED_TYPE) &&
                           real_storage_size > 4));
                    int use_sse_reg = 0;
                    if (!is_var_param && !is_array_type && !is_shortstring_param &&
                        (inferred_type_tag == REAL_TYPE || inferred_type_tag == EXTENDED_TYPE) &&
                        real_storage_size < 16)
                        use_sse_reg = 1;
                    if (use_extended_stack_param)
                        arg_stack = add_l_z_bytes((char *)arg_ids->cur, 10);
                    else
                        arg_stack = use_64bit ? add_q_z((char *)arg_ids->cur) : add_l_z((char *)arg_ids->cur);
                    if (arg_stack != NULL && (symbol_is_var_param || is_array_type || is_shortstring_param))
                        arg_stack->is_reference = 1;
                    if (use_extended_stack_param)
                    {
                        Register_t *src_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                        Register_t *dst_addr_reg = NULL;
                        if (src_addr_reg == NULL)
                            src_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                        if (src_addr_reg == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate register for Extended parameter %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        dst_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (dst_addr_reg == NULL)
                            dst_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                        if (dst_addr_reg == NULL)
                        {
                            free_reg(get_reg_stack(), src_addr_reg);
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate destination register for Extended parameter %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        snprintf(buffer, sizeof(buffer), "\tleaq\t%d(%%rbp), %s\n",
                            stack_arg_offset, src_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                            arg_stack->offset, dst_addr_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);

                        if (codegen_target_is_windows())
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dst_addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dst_addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_addr_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
                        }
                        inst_list = codegen_vect_reg(inst_list, 0);
                        inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
                        free_arg_regs();
                        free_reg(get_reg_stack(), dst_addr_reg);
                        free_reg(get_reg_stack(), src_addr_reg);
                        stack_arg_offset += 16;
                    }
                    else if (use_sse_reg)
                    {
                        if (next_sse_index < kgpc_max_sse_arg_regs())
                        {
                            const char *xmm_reg = alloc_sse_arg_reg(&next_sse_index);
                            if (real_storage_size == 4)
                                snprintf(buffer, sizeof(buffer), "\tmovss\t%s, -%d(%%rbp)\n",
                                    xmm_reg, arg_stack->offset);
                            else
                                snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, -%d(%%rbp)\n",
                                    xmm_reg, arg_stack->offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        else
                        {
                            if (real_storage_size == 4)
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovss\t%d(%%rbp), %%xmm0\n",
                                    stack_arg_offset);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tmovss\t%%xmm0, -%d(%%rbp)\n",
                                    arg_stack->offset);
                            }
                            else
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovsd\t%d(%%rbp), %%xmm0\n",
                                    stack_arg_offset);
                                inst_list = add_inst(inst_list, buffer);
                                snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, -%d(%%rbp)\n",
                                    arg_stack->offset);
                            }
                            inst_list = add_inst(inst_list, buffer);
                            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                        }
                    }
                    else
                    {
                        arg_reg = alloc_integer_arg_reg(use_64bit, &next_gpr_index);
                        Register_t *stack_value_reg = NULL;
                        const char *value_source = NULL;
                        
                        /* Check if we have a presaved slot from pre-pass. We must use it
                         * because the argument registers may have been clobbered by kgpc_move
                         * calls when processing earlier record/dynarray parameters. */
                        StackNode_t *presaved_slot = NULL;
                        if (has_record_or_dynarray)
                        {
                            char presaved_name[64];
                            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__", (char *)arg_ids->cur);
                            presaved_slot = find_label(presaved_name);
                        }
                        
                        if (presaved_slot != NULL && arg_reg != NULL)
                        {
                            /* Load from presaved slot since register may be clobbered */
                            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                            if (stack_value_reg == NULL)
                                stack_value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                            if (stack_value_reg != NULL)
                            {
                                if (use_64bit)
                                {
                                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                        presaved_slot->offset, stack_value_reg->bit_64);
                                    value_source = stack_value_reg->bit_64;
                                }
                                else
                                {
                                    snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n",
                                        presaved_slot->offset, stack_value_reg->bit_32);
                                    value_source = stack_value_reg->bit_32;
                                }
                                inst_list = add_inst(inst_list, buffer);
                            }
                        }
                        
                        if (value_source == NULL)
                        {
                            value_source = arg_reg;
                        }
                        
                        if (value_source == NULL)
                        {
                            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                            if (stack_value_reg == NULL)
                                stack_value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                            if (stack_value_reg == NULL)
                            {
                                codegen_report_error(ctx,
                                    "ERROR: Unable to allocate register for argument %s.",
                                    (char *)arg_ids->cur);
                                return inst_list;
                            }
                            if (use_64bit)
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                                    stack_arg_offset, stack_value_reg->bit_64);
                            }
                            else
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovl\t%d(%%rbp), %s\n",
                                    stack_arg_offset, stack_value_reg->bit_32);
                            }
                            inst_list = add_inst(inst_list, buffer);
                            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                            value_source = use_64bit ? stack_value_reg->bit_64 : stack_value_reg->bit_32;
                        }

                        if (use_64bit)
                            snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", value_source, arg_stack->offset);
                        else
                            snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", value_source, arg_stack->offset);
                        inst_list = add_inst(inst_list, buffer);
                        if (stack_value_reg != NULL)
                            free_reg(get_reg_stack(), stack_value_reg);
                    }
                    arg_ids = arg_ids->next;
                }
                break;
            case TREE_ARR_DECL:
                arg_ids = arg_decl->tree_data.arr_decl_data.ids;
                while(arg_ids != NULL)
                {
                    arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);
                    arg_stack = add_q_z((char *)arg_ids->cur);
                    if (arg_stack != NULL)
                        arg_stack->is_reference = 1;
                    Register_t *stack_value_reg = NULL;
                    const char *value_source = NULL;

                    /* Check if we have a presaved slot from pre-pass */
                    StackNode_t *presaved_slot = NULL;
                    if (has_record_or_dynarray)
                    {
                        char presaved_name[64];
                        snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__", (char *)arg_ids->cur);
                        presaved_slot = find_label(presaved_name);
                    }

                    if (presaved_slot != NULL && arg_reg != NULL)
                    {
                        /* Load from presaved slot since register may be clobbered */
                        stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (stack_value_reg != NULL)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                presaved_slot->offset, stack_value_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            value_source = stack_value_reg->bit_64;
                        }
                    }

                    if (value_source == NULL)
                        value_source = arg_reg;
                    if (value_source == NULL)
                    {
                        stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (stack_value_reg == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate register for array argument %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                            stack_arg_offset, stack_value_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                        value_source = stack_value_reg->bit_64;
                    }
                    snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", value_source, arg_stack->offset);
                    inst_list = add_inst(inst_list, buffer);
                    if (stack_value_reg != NULL)
                        free_reg(get_reg_stack(), stack_value_reg);
                    arg_ids = arg_ids->next;
                    param_index++;
                }
                break;
            default:
                fprintf(stderr,
                    "WARNING: Unknown argument type %d for procedure parameter.\n",
                    arg_decl ? arg_decl->type : -1);
                break;
        }
        args = args->next;
    }

    if (record_param_queue != NULL)
    {
        ListNode_t *rec_node = record_param_queue;
        while (rec_node != NULL)
        {
            RecordParamWork *work = (RecordParamWork *)rec_node->cur;
            StackNode_t *record_slot = add_l_x((char *)work->id, work->size);
            if (record_slot == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Failed to allocate storage for record parameter %s.",
                    work->id != NULL ? work->id : "(null)");
                free(work);
                rec_node = rec_node->next;
                continue;
            }
            if (work->is_dynarray)
            {
                record_slot->is_array = 1;
                record_slot->is_dynamic = 1;
                record_slot->element_size = (work->dynarray_elem_size > 0) ?
                    work->dynarray_elem_size : DOUBLEWORD;
                record_slot->array_lower_bound = work->dynarray_lower_bound;
            }

            Register_t *stack_value_reg = NULL;
            char presaved_name[64];
            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__", work->id);
            StackNode_t *presaved_slot = find_label(presaved_name);
            const char *record_src_reg = NULL;
            Register_t *loaded_param_reg = NULL;

            if (presaved_slot != NULL)
            {
                loaded_param_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (loaded_param_reg == NULL)
                    loaded_param_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
                if (loaded_param_reg != NULL)
                {
                    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                        presaved_slot->offset, loaded_param_reg->bit_64);
                    inst_list = add_inst(inst_list, buffer);
                    record_src_reg = loaded_param_reg->bit_64;
                }
            }
            if (record_src_reg == NULL && work->arg_reg != NULL)
            {
                record_src_reg = work->arg_reg;
            }

            if (record_src_reg == NULL)
            {
                if (getenv("KGPC_DEBUG_RECORD_PARAM") != NULL)
                {
                    fprintf(stderr,
                        "[KGPC] record param missing src: subprogram=%s param=%s arg_reg=%s has_stack=%d stack_off=%d arg_index=%d presaved=%p\n",
                        ctx != NULL && ctx->current_subprogram_id != NULL ? ctx->current_subprogram_id : "(null)",
                        work->id != NULL ? work->id : "(null)",
                        work->arg_reg != NULL ? work->arg_reg : "(null)",
                        work->has_stack_arg,
                        work->stack_arg_offset,
                        work->arg_index,
                        (void *)presaved_slot);
                }
                if (work->arg_reg == NULL && !work->has_stack_arg)
                {
                    const char *fallback_reg = get_arg_reg64_num(arg_start_index + work->arg_index);
                    if (fallback_reg != NULL)
                        record_src_reg = fallback_reg;
                }
            }

            if (record_src_reg == NULL)
            {
                if (!work->has_stack_arg)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to locate record parameter %s.",
                        work->id != NULL ? work->id : "(null)");
                    if (loaded_param_reg != NULL)
                        free_reg(get_reg_stack(), loaded_param_reg);
                    free(work);
                    rec_node = rec_node->next;
                    continue;
                }

                stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                if (stack_value_reg == NULL)
                {
                    codegen_report_error(ctx,
                        "ERROR: Unable to allocate register for record parameter %s.",
                        work->id != NULL ? work->id : "(null)");
                    if (loaded_param_reg != NULL)
                        free_reg(get_reg_stack(), loaded_param_reg);
                    free(work);
                    rec_node = rec_node->next;
                    continue;
                }
                snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                    work->stack_arg_offset, stack_value_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                record_src_reg = stack_value_reg->bit_64;
            }

            Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (size_reg == NULL)
                size_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (size_reg == NULL)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to allocate register for record parameter size.");
                if (stack_value_reg != NULL)
                    free_reg(get_reg_stack(), stack_value_reg);
                if (loaded_param_reg != NULL)
                    free_reg(get_reg_stack(), loaded_param_reg);
                free(work);
                rec_node = rec_node->next;
                continue;
            }

            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", work->size, size_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", record_src_reg);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", record_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", record_src_reg);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n", record_slot->offset);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
            free_arg_regs();
            free_reg(get_reg_stack(), size_reg);
            if (stack_value_reg != NULL)
                free_reg(get_reg_stack(), stack_value_reg);
            if (loaded_param_reg != NULL)
                free_reg(get_reg_stack(), loaded_param_reg);

            free(work);
            rec_node = rec_node->next;
        }
        DestroyList(record_param_queue);
    }

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

static ListNode_t *codegen_store_class_typeinfo(ListNode_t *inst_list,
    CodeGenContext *ctx, StackNode_t *var_node, const char *type_name)
{
    if (var_node == NULL || type_name == NULL || type_name[0] == '\0' || var_node->is_reference)
        return inst_list;

    char typeinfo_label[512];
    snprintf(typeinfo_label, sizeof(typeinfo_label), "%s_VMT", type_name);

    /* Class variables are pointers to instances. We need to:
     * 1. Allocate memory for the instance (size determined from type)
     * 2. Store the typeinfo pointer in the first field
     * 3. Store the instance pointer in the variable
     * 
     * For now, we use a simplified approach: allocate a fixed size (64 bytes should be enough for most classes)
     * and zero-initialize with calloc. A better approach would compute the actual size from the RecordType.
     */
    
    /* Call calloc to allocate and zero-initialize the instance */
    char buffer[1024];
    const char *size_reg = current_arg_reg64(0);  /* RDI on Linux, RCX on Windows */
    const char *count_reg = current_arg_reg64(1); /* RSI on Linux, RDX on Windows */
    
    if (size_reg != NULL && count_reg != NULL)
    {
        /* calloc(1, 64) - allocate one 64-byte block */
        snprintf(buffer, sizeof(buffer), "\tmovq\t$1, %s\n", size_reg);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, sizeof(buffer), "\tmovq\t$64, %s\n", count_reg);
        inst_list = add_inst(inst_list, buffer);
        inst_list = codegen_call_with_shadow_space(inst_list, "calloc");
        
        /* RAX now contains the pointer to the allocated instance */
        /* Store the typeinfo pointer in the first field */
        inst_list = add_inst(inst_list, "\tpushq\t%rax\n");  /* Save instance pointer */
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %%r10\n", typeinfo_label);
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tmovq\t%r10, (%rax)\n");  /* Store typeinfo in first field */
        inst_list = add_inst(inst_list, "\tpopq\t%rax\n");   /* Restore instance pointer */
        
        /* Store the instance pointer in the class variable */
        if (var_node->is_static)
        {
            const char *label = var_node->static_label != NULL ? var_node->static_label : var_node->label;
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s(%%rip)\n", label);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", var_node->offset);
        }
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        /* Fallback if we can't determine arg registers - just store NULL for now */
        if (ctx != NULL)
        {
            codegen_report_error(ctx, "ERROR: Unable to allocate class instance - register allocation failed");
        }
        if (var_node->is_static)
        {
            const char *label = var_node->static_label != NULL ? var_node->static_label : var_node->label;
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, %s(%%rip)\n", label);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", var_node->offset);
        }
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

static ListNode_t *codegen_emit_tfile_configure(ListNode_t *inst_list,
    StackNode_t *file_node, long long element_size, int element_hash_tag)
{
    if (file_node == NULL || element_size <= 0)
        return inst_list;

    const char *slot_reg = current_arg_reg64(0);
    const char *size_reg = current_arg_reg64(1);
    const char *tag_reg = current_arg_reg32(2);
    if (slot_reg == NULL || size_reg == NULL || tag_reg == NULL)
        return inst_list;

    char buffer[256];
    if (file_node->is_static)
    {
        const char *label = (file_node->static_label != NULL) ?
            file_node->static_label : file_node->label;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label, slot_reg);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            file_node->offset, slot_reg);
    }
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", element_size, size_reg);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_hash_tag, tag_reg);
    inst_list = add_inst(inst_list, buffer);

    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_tfile_configure");
    return inst_list;
}

static int codegen_type_tag_to_hashvar(int parser_tag)
{
    switch (parser_tag)
    {
        case CHAR_TYPE:
            return HASHVAR_CHAR;
        case BOOL:
            return HASHVAR_BOOLEAN;
        case LONGINT_TYPE:
            return HASHVAR_LONGINT;
        case REAL_TYPE:
            return HASHVAR_REAL;
        case INT_TYPE:
            return HASHVAR_INTEGER;
        default:
            return HASHVAR_INTEGER;
    }
}

static long long codegen_type_tag_size(int parser_tag)
{
    switch (parser_tag)
    {
        case CHAR_TYPE:
        case BOOL:
            return 1;
        case LONGINT_TYPE:
            return 4;  // Match FPC's 32-bit LongInt
        case REAL_TYPE:
            return 8;
        case INT_TYPE:
        default:
            return 4;
    }
}

static int codegen_resolve_file_component(const struct TypeAlias *alias, SymTab_t *symtab,
    long long *element_size_out, int *element_hash_tag_out)
{
    if (alias == NULL || !alias->is_file || element_size_out == NULL || element_hash_tag_out == NULL)
        return 0;

    int parser_tag = alias->file_type;
    HashNode_t *type_node = NULL;
    if (parser_tag == UNKNOWN_TYPE && alias->file_type_id != NULL && symtab != NULL)
    {
        if (FindSymbol(&type_node, symtab, alias->file_type_id) != 0 && type_node != NULL)
        {
            if (type_node->type != NULL)
                parser_tag = kgpc_type_get_primitive_tag(type_node->type);
        }
    }

    if (parser_tag == UNKNOWN_TYPE && type_node != NULL && type_node->type != NULL)
        parser_tag = kgpc_type_get_primitive_tag(type_node->type);

    if (parser_tag == UNKNOWN_TYPE)
        parser_tag = INT_TYPE;

    long long elem_size = codegen_type_tag_size(parser_tag);
    int hash_tag = codegen_type_tag_to_hashvar(parser_tag);

    if (type_node != NULL && type_node->type != NULL)
    {
        long long resolved_size = kgpc_type_sizeof(type_node->type);
        if (resolved_size > 0)
            elem_size = resolved_size;

        int resolved_tag = kgpc_type_get_primitive_tag(type_node->type);
        if (resolved_tag != UNKNOWN_TYPE)
            hash_tag = codegen_type_tag_to_hashvar(resolved_tag);
    }

    *element_size_out = elem_size;
    *element_hash_tag_out = hash_tag;
    return 1;
}



ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(ctx != NULL);
    assert(symtab != NULL);
    while (decls != NULL)
    {
        Tree_t *decl = (Tree_t *)decls->cur;
        if (decl == NULL)
        {
            decls = decls->next;
            continue;
        }

        if (decl->type == TREE_VAR_DECL)
        {
            HashNode_t *type_node = NULL;
            if (decl->tree_data.var_decl_data.type_id != NULL)
                FindSymbol(&type_node, symtab, decl->tree_data.var_decl_data.type_id);

            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array && alias->is_open_array)
            {
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *array_node = find_label(var_name);
                    if (array_node != NULL && array_node->is_dynamic && array_node->offset > 0)
                    {
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", array_node->offset);
                        inst_list = add_inst(inst_list, buffer);
                        int length_offset = array_node->offset - 2 * DOUBLEWORD;
                        if (length_offset < array_node->offset)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", length_offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                    ids = ids->next;
                }
            }

            /* Initialize FILE variables to NULL */
            if ((type_node != NULL && node_is_file_type(type_node)) ||
                (type_node == NULL && decl->tree_data.var_decl_data.type == FILE_TYPE))
            {
                struct TypeAlias *decl_inline_alias = decl->tree_data.var_decl_data.inline_type_alias;
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *file_node = find_label(var_name);
                    if (file_node != NULL)
                    {
                        char buffer[128];
                        if (!file_node->is_static)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", file_node->offset);
                            inst_list = add_inst(inst_list, buffer);
                        }

                        long long file_elem_size = 0;
                        int file_elem_hash = HASHVAR_INTEGER;
                        struct TypeAlias *file_alias = get_type_alias_from_node(type_node);
                        if (file_alias == NULL && decl_inline_alias != NULL)
                            file_alias = decl_inline_alias;
                        if (file_alias == NULL || !file_alias->is_file)
                        {
                            HashNode_t *var_hash = NULL;
                            if (FindSymbol(&var_hash, symtab, var_name) != 0 && var_hash != NULL)
                                file_alias = hashnode_get_type_alias(var_hash);
                        }

                        int have_component = codegen_resolve_file_component(
                            file_alias, symtab, &file_elem_size, &file_elem_hash);

                        if (have_component)
                        {
                            inst_list = codegen_emit_tfile_configure(inst_list,
                                file_node, file_elem_size, file_elem_hash);
                        }
                    }
                    ids = ids->next;
                }
            }

            struct Statement *init_stmt = decl->tree_data.var_decl_data.initializer;
            if (type_node != NULL && node_is_class_type(type_node))
            {
                struct RecordType *record_desc = get_record_type_from_node(type_node);
                const char *class_type_name = (record_desc != NULL && record_desc->type_id != NULL) ?
                    record_desc->type_id : codegen_resolve_record_type_name(type_node, symtab);
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *var_node = find_label(var_name);
                    inst_list = codegen_store_class_typeinfo(inst_list, ctx, var_node,
                        class_type_name);
                    ids = ids->next;
                }
            }

            if (init_stmt != NULL)
                inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
        }
        else if (decl->type == TREE_ARR_DECL)
        {
            struct Array *arr = &decl->tree_data.arr_decl_data;
            if (arr->e_range < arr->s_range)
            {
                ListNode_t *ids = arr->ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *array_node = find_label(var_name);
                    if (array_node != NULL && array_node->is_dynamic && array_node->offset > 0)
                    {
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                            array_node->offset);
                        inst_list = add_inst(inst_list, buffer);
                        int length_offset = array_node->offset - 2 * DOUBLEWORD;
                        if (length_offset < array_node->offset)
                        {
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t$0, -%d(%%rbp)\n", length_offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                    ids = ids->next;
                }
            }
            struct Statement *init_stmt = arr->initializer;
            if (init_stmt != NULL)
            {
                if (arr->is_typed_const && arr->init_guard_label != NULL)
                {
                    char done_label[64];
                    gen_label(done_label, sizeof(done_label), ctx);

                    char buffer[128];
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s(%%rip), %%al\n", arr->init_guard_label);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\ttestb\t%al, %al\n");
                    snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", done_label);
                    inst_list = add_inst(inst_list, buffer);

                    inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);

                    snprintf(buffer, sizeof(buffer), "\tmovb\t$1, %s(%%rip)\n", arr->init_guard_label);
                    inst_list = add_inst(inst_list, buffer);

                    char label_decl[96];
                    snprintf(label_decl, sizeof(label_decl), "%s:\n", done_label);
                    inst_list = add_inst(inst_list, label_decl);
                }
                else
                {
                    inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
                }
            }
        }
        decls = decls->next;
    }
    return inst_list;
}
#if KGPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif
