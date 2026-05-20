/*
 * codegen_symbol_resolution.c — Symbol/mangling resolution and label collection
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "codegen.h"
#include "codegen_string_set.h"
#include "codegen_symbol_resolution.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/ParseTree/ident_ref.h"
#include "../../Parser/SemanticCheck/NameMangling.h"
#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "../../flags.h"
#include "../../Parser/ParseTree/from_cparser.h"

/* Defined in Parser/SemanticCheck/SemCheck_parts/SemCheck_vmt_and_type_decls.c.
 * Build a parameter TypeRef array for overload disambiguation. */
TypeRef **semcheck_param_types_from_params(ListNode_t *params, int skip_first_param, int *out_count);

/* Globals defined in codegen.c; accessed here for label collection. */
extern ListNode_t *g_codegen_available_subprograms;
extern CodeGenStringSet g_available_subprograms_set;
extern ListNode_t *g_available_subprograms_tail;
extern CodeGenStringSet g_codegen_callable_exports;

/* Helper functions defined in codegen.c used by the functions below. */
extern int codegen_list_contains_string(ListNode_t *list, const char *value);
extern int codegen_float_native_distance(Tree_t *sub);

/* External declaration — defined in SemCheck_Expr_Internal.c */
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);

static char *codegen_build_unit_qualified_mangled(const char *base_mangled,
    int source_unit_index);

int codegen_runtime_owns_exported_symbol(const char *symbol)
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
            /* Strip unit$$ prefix from candidate mangled_id for comparison */
            const char *cand_base = mangled_id_get_base(cand->mangled_id);
            if (strncmp(cand_base, stale_target, prefix_len) != 0)
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

    /* Final fixup: if call_target is an unprefixed mangled name but all
     * definitions with that base name have been unit-qualified by the
     * codegen_apply_unit_mangled_prefixes pre-pass, look up the correct
     * unit-qualified label.  This handles cases where semcheck cached a
     * mangled_id before codegen added the unit$$ prefix. */
    if (call_target != NULL && !mangled_id_has_unit_prefix(call_target) &&
        ctx != NULL && ctx->symtab != NULL &&
        expr->expr_data.function_call_data.id != NULL)
    {
        if (expr->expr_data.function_call_data.call_kgpc_type != NULL &&
            expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
            expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition != NULL)
        {
            Tree_t *def = expr->expr_data.function_call_data.call_kgpc_type
                ->info.proc_info.definition;
            int source_unit_index = def->tree_data.subprogram_data.source_unit_index;
            if (source_unit_index > 0)
            {
                char *qualified_target = codegen_build_unit_qualified_mangled(
                    call_target, source_unit_index);
                if (qualified_target != NULL)
                {
                    HashNode_t *qualified_node = NULL;
                    if (FindSymbol(&qualified_node, ctx->symtab, qualified_target) != 0 ||
                        codegen_has_available_subprogram_label(qualified_target))
                    {
                        if (owned_target_out != NULL)
                        {
                            if (*owned_target_out != NULL)
                                free(*owned_target_out);
                            *owned_target_out = qualified_target;
                        }
                        else
                        {
                            return qualified_target;
                        }
                        return owned_target_out != NULL ? *owned_target_out : call_target;
                    }
                    free(qualified_target);
                }
            }
        }

        ListNode_t *candidates = FindAllIdents(ctx->symtab,
            expr->expr_data.function_call_data.id);
        for (ListNode_t *cur = candidates; cur != NULL; cur = cur->next)
        {
            HashNode_t *cand = (HashNode_t *)cur->cur;
            if (cand == NULL || cand->mangled_id == NULL)
                continue;
            /* Check if cand->mangled_id has the form "unit$$call_target" */
            if (!mangled_id_has_unit_prefix(cand->mangled_id))
                continue;
            const char *base = mangled_id_get_base(cand->mangled_id);
            if (strcmp(base, call_target) == 0)
            {
                /* Verify this candidate has a real implementation */
                if (cand->type != NULL &&
                    cand->type->kind == TYPE_KIND_PROCEDURE &&
                    cand->type->info.proc_info.definition != NULL &&
                    cand->type->info.proc_info.definition
                        ->tree_data.subprogram_data.statement_list != NULL)
                {
                    call_target = cand->mangled_id;
                    break;
                }
            }
        }
        if (candidates != NULL)
            DestroyList(candidates);
    }

    return call_target;
}

static int codegen_call_type_method_param_count(KgpcType *call_type)
{
    if (call_type == NULL || call_type->kind != TYPE_KIND_PROCEDURE)
        return -1;

    ListNode_t *params = call_type->info.proc_info.params;
    int count = ListLength(params);
    if (count <= 0)
        return count;

    Tree_t *first_param = (Tree_t *)params->cur;
    if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
        first_param->tree_data.var_decl_data.ids != NULL)
    {
        const char *first_name =
            (const char *)first_param->tree_data.var_decl_data.ids->cur;
        if (first_name != NULL && pascal_identifier_equals(first_name, "Self"))
            count--;
    }

    return count;
}

int codegen_resolve_virtual_vmt_index(CodeGenContext *ctx,
    const char *owner_class_name, const char *method_name,
    KgpcType *call_type)
{
    KGPC_COMPILER_HARD_ASSERT(ctx != NULL && ctx->symtab != NULL,
        "virtual VMT index resolution requires a codegen context and symbol table");
    KGPC_COMPILER_HARD_ASSERT(owner_class_name != NULL && owner_class_name[0] != '\0',
        "virtual VMT index resolution requires structured owner class metadata");
    KGPC_COMPILER_HARD_ASSERT(method_name != NULL && method_name[0] != '\0',
        "virtual VMT index resolution requires structured bare method metadata");
    KGPC_COMPILER_HARD_ASSERT(strstr(method_name, "__") == NULL,
        "virtual VMT index resolution received mangled method name '%s'; pass cached_method_name instead",
        method_name);

    struct RecordType *record = semcheck_lookup_record_type(ctx->symtab,
        owner_class_name);
    KGPC_COMPILER_HARD_ASSERT(record != NULL,
        "virtual VMT owner class '%s' is missing from the structured type table",
        owner_class_name);
    KGPC_COMPILER_HARD_ASSERT(record->methods != NULL,
        "virtual VMT owner class '%s' has no structured VMT method table",
        owner_class_name);

    int wanted_param_count = codegen_call_type_method_param_count(call_type);

    /* Build a structural TypeRef array for the call's params so we can
     * disambiguate overloaded methods that share a name+param_count but
     * differ in arg types (e.g. tloopnode.create(tnodetype,4*tnode) vs
     * tfornode.create(4*tnode,boolean)). */
    TypeRef **call_param_types = NULL;
    int call_param_types_count = 0;
    if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE)
    {
        ListNode_t *params = call_type->info.proc_info.params;
        int skip_self = 0;
        if (params != NULL)
        {
            Tree_t *first = (Tree_t *)params->cur;
            if (first != NULL && first->type == TREE_VAR_DECL &&
                first->tree_data.var_decl_data.ids != NULL)
            {
                const char *first_name =
                    (const char *)first->tree_data.var_decl_data.ids->cur;
                if (first_name != NULL && pascal_identifier_equals(first_name, "Self"))
                    skip_self = 1;
            }
        }
        call_param_types = semcheck_param_types_from_params(params, skip_self, &call_param_types_count);
    }

    struct MethodInfo *name_match = NULL;
    int name_match_count = 0;
    struct MethodInfo *first_count_match = NULL;
    int count_match_count = 0;
    struct MethodInfo *sig_match = NULL;
    for (ListNode_t *node = record->methods; node != NULL; node = node->next)
    {
        struct MethodInfo *method = (struct MethodInfo *)node->cur;
        if (method == NULL || method->name == NULL ||
            !(method->is_virtual || method->is_override) ||
            !pascal_identifier_equals(method->name, method_name))
            continue;

        name_match = method;
        name_match_count++;
        if (wanted_param_count >= 0)
        {
            KGPC_COMPILER_HARD_ASSERT(method->param_count >= 0,
                "virtual method '%s.%s' has no structured parameter count",
                owner_class_name, method_name);
            if (wanted_param_count != method->param_count)
                continue;
            if (first_count_match == NULL)
                first_count_match = method;
            count_match_count++;
            if (call_param_types != NULL &&
                method->param_types != NULL && method->param_types_count >= 0 &&
                type_ref_array_equal_ci(call_param_types, call_param_types_count,
                                        method->param_types, method->param_types_count))
                sig_match = method;
        }
    }

    param_types_free(call_param_types, call_param_types_count);

    if (sig_match != NULL)
        return sig_match->vmt_index;
    if (first_count_match != NULL)
        return first_count_match->vmt_index;

    KGPC_COMPILER_HARD_ASSERT(name_match != NULL,
        "virtual method '%s.%s' is missing from the structured VMT method table",
        owner_class_name, method_name);
    KGPC_COMPILER_HARD_ASSERT(name_match_count == 1,
        "virtual method '%s.%s' is overloaded but call type metadata is missing",
        owner_class_name, method_name);
    return name_match->vmt_index;
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

    char *owned_resolved_target = NULL;
    resolved_target = codegen_resolve_function_call_target(ctx, expr, &owned_resolved_target);
    if (ctx != NULL && ctx->symtab != NULL &&
        resolved_target != NULL &&
        (expr->expr_data.function_call_data.mangled_id == NULL ||
         strcmp(resolved_target, expr->expr_data.function_call_data.mangled_id) != 0) &&
        FindSymbol(&resolved_node, ctx->symtab, resolved_target) != 0 &&
        resolved_node != NULL && resolved_node->type != NULL)
    {
        if (resolved_node_out != NULL)
            *resolved_node_out = resolved_node;
        free(owned_resolved_target);
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
        free(owned_resolved_target);
        return resolved_node->type;
    }

    free(owned_resolved_target);
    return NULL;
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

/* Build a unit-qualified mangled name: "unitname$$base_mangled".  Returns a
 * malloc'd string.  The caller must free it. */
static char *codegen_build_unit_qualified_mangled(const char *base_mangled,
                                                   int source_unit_index)
{
    if (base_mangled == NULL || source_unit_index <= 0)
        return NULL;
    const char *unit_name = unit_registry_get(source_unit_index);
    if (unit_name == NULL || unit_name[0] == '\0')
        return NULL;
    size_t ulen = strlen(unit_name);
    size_t blen = strlen(base_mangled);
    char *result = malloc(ulen + 2 + blen + 1);
    if (result == NULL)
        return NULL;
    for (size_t i = 0; i < ulen; i++)
        result[i] = (char)tolower((unsigned char)unit_name[i]);
    result[ulen] = '$';
    result[ulen + 1] = '$';
    memcpy(result + ulen + 2, base_mangled, blen + 1);
    return result;
}

/* Returns 1 if a subprogram tree node is eligible for unit-prefix qualification
 * (non-method, non-external, non-internproc, has a mangled_id, from a unit). */
static int codegen_subprogram_is_prefix_eligible(Tree_t *sub)
{
    if (sub == NULL || sub->type != TREE_SUBPROGRAM)
        return 0;
    struct Subprogram *data = &sub->tree_data.subprogram_data;
    if (data->source_unit_index <= 0)
        return 0;
    if (data->owner_class != NULL)
        return 0;
    if (data->cname_override != NULL || data->cname_flag)
        return 0;
    if (data->mangled_id == NULL || data->mangled_id[0] == '\0')
        return 0;
    if (data->internproc_id != NULL && data->internproc_id[0] != '\0')
        return 0;
    return 1;
}

static unsigned cg_collision_hash(const char *s)
{
    unsigned h = 0;
    for (; *s; s++)
        h = h * 31 + (unsigned char)*s;
    return h % CG_COLLISION_BUCKETS;
}

/* Record a (mangled_id, unit_index) pair.  If the same mangled_id is seen with
 * a different unit_index, mark it as colliding. */
static void cg_collision_record(CgCollisionMap *map, const char *mangled_id,
                                 int unit_index)
{
    unsigned idx = cg_collision_hash(mangled_id);
    for (CgCollisionEntry *e = map->buckets[idx]; e != NULL; e = e->next) {
        if (strcmp(e->mangled_id, mangled_id) == 0) {
            if (e->first_unit_index != unit_index)
                e->is_colliding = 1;
            return;
        }
    }
    CgCollisionEntry *entry = malloc(sizeof(CgCollisionEntry));
    if (entry == NULL)
        return;
    entry->mangled_id = strdup(mangled_id);
    entry->first_unit_index = unit_index;
    entry->is_colliding = 0;
    entry->next = map->buckets[idx];
    map->buckets[idx] = entry;
}

static int cg_collision_is_colliding(const CgCollisionMap *map,
                                      const char *mangled_id)
{
    unsigned idx = cg_collision_hash(mangled_id);
    for (CgCollisionEntry *e = map->buckets[idx]; e != NULL; e = e->next) {
        if (strcmp(e->mangled_id, mangled_id) == 0)
            return e->is_colliding;
    }
    return 0;
}

void cg_collision_destroy(CgCollisionMap *map)
{
    for (int i = 0; i < CG_COLLISION_BUCKETS; i++) {
        CgCollisionEntry *e = map->buckets[i];
        while (e != NULL) {
            CgCollisionEntry *next = e->next;
            free(e->mangled_id);
            free(e);
            e = next;
        }
        map->buckets[i] = NULL;
    }
}

/* First pass: collect (mangled_id, source_unit_index) pairs from all eligible
 * subprograms to detect cross-unit mangled name collisions. */
void codegen_collect_mangled_collisions(ListNode_t *sub_list,
                                        CgCollisionMap *map)
{
    while (sub_list != NULL) {
        Tree_t *sub = (Tree_t *)sub_list->cur;
        if (codegen_subprogram_is_prefix_eligible(sub)) {
            cg_collision_record(map, sub->tree_data.subprogram_data.mangled_id,
                sub->tree_data.subprogram_data.source_unit_index);
        }
        if (sub != NULL && sub->type == TREE_SUBPROGRAM &&
            sub->tree_data.subprogram_data.subprograms != NULL)
            codegen_collect_mangled_collisions(
                sub->tree_data.subprogram_data.subprograms, map);
        sub_list = sub_list->next;
    }
}

/* Second pass: for subprograms whose mangled_id collides across units, apply
 * the unit$$ prefix to both the tree node and the corresponding HashNode. */
void codegen_apply_collision_prefixes(ListNode_t *sub_list,
                                      SymTab_t *symtab,
                                      const CgCollisionMap *map)
{
    while (sub_list != NULL) {
        Tree_t *sub = (Tree_t *)sub_list->cur;
        if (codegen_subprogram_is_prefix_eligible(sub)) {
            struct Subprogram *data = &sub->tree_data.subprogram_data;
            if (cg_collision_is_colliding(map, data->mangled_id)) {
                char *prefixed = codegen_build_unit_qualified_mangled(
                    data->mangled_id, data->source_unit_index);
                if (prefixed != NULL) {
                    /* Update corresponding HashNode in the symbol table. */
                    if (symtab != NULL && data->id != NULL) {
                        ListNode_t *candidates = FindAllIdents(symtab, data->id);
                        for (ListNode_t *c = candidates; c != NULL; c = c->next) {
                            HashNode_t *hn = (HashNode_t *)c->cur;
                            if (hn == NULL || hn->type == NULL ||
                                hn->type->kind != TYPE_KIND_PROCEDURE)
                                continue;
                            Tree_t *def = hn->type->info.proc_info.definition;
                            if (def != sub)
                                continue;
                            if (hn->mangled_id != NULL)
                                free(hn->mangled_id);
                            hn->mangled_id = strdup(prefixed);
                            break;
                        }
                        if (candidates != NULL)
                            DestroyList(candidates);
                    }
                    free(data->mangled_id);
                    data->mangled_id = prefixed;
                }
            }
        }
        if (sub != NULL && sub->type == TREE_SUBPROGRAM &&
            sub->tree_data.subprogram_data.subprograms != NULL)
            codegen_apply_collision_prefixes(
                sub->tree_data.subprogram_data.subprograms, symtab, map);
        sub_list = sub_list->next;
    }
}

void codegen_collect_available_subprogram_labels(ListNode_t *sub_list)
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

        if (mangled_id != NULL && !codegen_set_contains(&g_available_subprograms_set, mangled_id)) {
            codegen_set_insert(&g_available_subprograms_set, mangled_id);
            ListNode_t *node = CreateListNode((void *)mangled_id, LIST_STRING);
            if (g_codegen_available_subprograms == NULL) {
                g_codegen_available_subprograms = node;
                g_available_subprograms_tail = node;
            } else {
                g_available_subprograms_tail->next = node;
                g_available_subprograms_tail = node;
            }
        }

        if (sub->tree_data.subprogram_data.subprograms != NULL)
            codegen_collect_available_subprogram_labels(sub->tree_data.subprogram_data.subprograms);

        sub_list = sub_list->next;
    }
}

void codegen_collect_callable_export_names(ListNode_t *sub_list)
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

int codegen_self_param_is_class(Tree_t *arg_decl, SymTab_t *symtab)
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
