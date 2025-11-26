/*
    Damon Gwinn
    Performs semantic checking on a given statement

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include <stdint.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strncasecmp _strnicmp
#endif
#include "SemCheck_stmt.h"
#include "SemCheck_expr.h"
#include "../SemCheck.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/tree_types.h"
#include "../../List/List.h"
#include "../../ParseTree/type_tags.h"
#include "../../ParseTree/KgpcType.h"
#include "../../../identifier_utils.h"

static int semcheck_loop_depth = 0;
/* Debug helpers used for corruption watchdog logging. */
static struct Statement *g_debug_watch_stmt = NULL;
static struct Expression *g_debug_watch_to_expr = NULL;

/* Resolve the RecordType for a TFPGList specialization from a LHS expression.
 * This bypasses incomplete kgpc_type inference and looks directly at the symbol
 * table entry for the variable or type identifier. */
static int is_tfpglist_type_id(const char *type_id)
{
    return (type_id != NULL &&
            strncasecmp(type_id, "TFPGList$", strlen("TFPGList$")) == 0);
}

/* Resolve the RecordType for a TFPGList specialization from a LHS expression.
 * This bypasses incomplete kgpc_type inference and looks directly at the symbol
 * table entry for the variable or type identifier. */
static struct RecordType *resolve_tfpglist_record_from_lhs(SymTab_t *symtab,
    struct Expression *lhs)
{
    if (symtab == NULL || lhs == NULL)
        return NULL;

    if (lhs->type != EXPR_VAR_ID || lhs->expr_data.id == NULL)
        return NULL;

    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, lhs->expr_data.id) < 0 || node == NULL)
        return NULL;

    struct RecordType *record = hashnode_get_record_type(node);
    if (record == NULL || record->type_id == NULL)
        return NULL;

    if (!is_tfpglist_type_id(record->type_id))
        return NULL;

    return record;
}

static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

static HashNode_t *lookup_hashnode(SymTab_t *symtab, const char *id)
{
    if (symtab == NULL || id == NULL)
        return NULL;
    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, (char *)id) >= 0 && node != NULL)
        return node;
    return NULL;
}

static const char *resolve_tfpglist_specialized_id_from_typename(SymTab_t *symtab, const char *type_name)
{
    HashNode_t *type_node = lookup_hashnode(symtab, type_name);
    if (type_node == NULL)
        return NULL;

    struct RecordType *record = hashnode_get_record_type(type_node);
    if (record != NULL && is_tfpglist_type_id(record->type_id))
        return record->type_id;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL && alias->target_type_id != NULL &&
        is_tfpglist_type_id(alias->target_type_id))
        return alias->target_type_id;

    return NULL;
}

static const char *resolve_tfpglist_specialized_id_from_expr(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
        return resolve_tfpglist_specialized_id_from_typename(symtab, expr->expr_data.id);
    return NULL;
}

static struct Expression *make_tfpglist_ctor_expr(struct RecordType *record, int line_num)
{
    if (record == NULL || record->type_id == NULL)
        return NULL;

    const char *type_id = record->type_id;
    const char *prefix = "__tfpg_ctor$";
    size_t len = strlen(prefix) + strlen(type_id) + 1;
    char *ctor_name = (char *)malloc(len);
    if (ctor_name == NULL)
        return NULL;
    strcpy(ctor_name, prefix);
    strcat(ctor_name, type_id);

    struct Expression *call = mk_functioncall(line_num, ctor_name, NULL);
    if (call == NULL)
        return NULL;

    /* Set the mangled_id to the actual Create constructor method name */
    /* Format: ClassName__Create_u */
    size_t mangled_len = strlen(type_id) + strlen("__Create_u") + 1;
    char *mangled_name = (char *)malloc(mangled_len);
    if (mangled_name != NULL)
    {
        strcpy(mangled_name, type_id);
        strcat(mangled_name, "__Create_u");
        call->expr_data.function_call_data.mangled_id = mangled_name;
        
        if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
        {
            fprintf(stderr, "[KGPC] TFPG ctor: set mangled_id to %s\n", mangled_name);
        }
    }

    call->record_type = record;
    call->resolved_type = RECORD_TYPE;
    if (call->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(call->resolved_kgpc_type);
        call->resolved_kgpc_type = NULL;
    }
    call->resolved_kgpc_type = create_record_type(record);
    return call;
}

static int rewrite_tfpglist_constructor_if_needed(SymTab_t *symtab,
    int max_scope_lev, struct Expression *lhs, struct Expression **rhs_ptr)
{
    if (symtab == NULL || lhs == NULL || rhs_ptr == NULL || *rhs_ptr == NULL)
        return 0;
    const char *debug_env = getenv("KGPC_DEBUG_GENERIC_CLONES");

    struct RecordType *lhs_record = resolve_tfpglist_record_from_lhs(symtab, lhs);
    if (lhs_record == NULL || lhs_record->type_id == NULL)
    {
        if (debug_env)
        {
            fprintf(stderr,
                "[KGPC] TFPG ctor: lhs %s is not TFPGList specialization\n",
                (lhs->expr_data.id != NULL) ? lhs->expr_data.id : "<expr>");
        }
        return 0;
    }

    const char *expected_specialized_id = lhs_record->type_id;
    struct Expression *rhs = *rhs_ptr;

    int matches_pattern = 0;
    if (rhs->type == EXPR_RECORD_ACCESS &&
        rhs->expr_data.record_access_data.field_id != NULL &&
        strcasecmp(rhs->expr_data.record_access_data.field_id, "Create") == 0)
    {
        const char *candidate = resolve_tfpglist_specialized_id_from_expr(symtab,
            rhs->expr_data.record_access_data.record_expr);
        if (candidate != NULL &&
            strcasecmp(candidate, expected_specialized_id) == 0)
            matches_pattern = 1;
    }
    else if (rhs->type == EXPR_FUNCTION_CALL &&
             rhs->expr_data.function_call_data.id != NULL)
    {
        const char *candidate = resolve_tfpglist_specialized_id_from_typename(
            symtab, rhs->expr_data.function_call_data.id);
        if (candidate != NULL &&
            strcasecmp(candidate, expected_specialized_id) == 0)
        {
            /* Legacy lowering produced a dummy argument referencing the type */
            ListNode_t *args = rhs->expr_data.function_call_data.args_expr;
            if (args == NULL)
                matches_pattern = 1;
            else if (args->next == NULL)
            {
                struct Expression *arg_expr = (struct Expression *)args->cur;
                if (arg_expr != NULL && arg_expr->type == EXPR_VAR_ID &&
                    arg_expr->expr_data.id != NULL &&
                    pascal_identifier_equals(arg_expr->expr_data.id,
                        rhs->expr_data.function_call_data.id))
                    matches_pattern = 1;
            }
        }
    }

    if (!matches_pattern)
    {
        if (debug_env)
            fprintf(stderr, "[KGPC] TFPG ctor: rhs did not match constructor pattern\n");
        return 0;
    }

    struct Expression *ctor_expr =
        make_tfpglist_ctor_expr(lhs_record, rhs->line_num);
    if (ctor_expr == NULL)
    {
        if (debug_env)
            fprintf(stderr, "[KGPC] TFPG ctor: failed to build ctor expression\n");
        return 0;
    }

    if (debug_env)
        fprintf(stderr, "[KGPC] TFPG ctor: rewriting ctor for %s\n",
            expected_specialized_id);

    destroy_expr(rhs);
    *rhs_ptr = ctor_expr;
    return 1;
}
static void semcheck_stmt_set_call_kgpc_type(struct Statement *stmt, KgpcType *type,
    int owns_existing)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return;

    if (stmt->stmt_data.procedure_call_data.call_kgpc_type != NULL && owns_existing)
    {
        destroy_kgpc_type(stmt->stmt_data.procedure_call_data.call_kgpc_type);
    }
    stmt->stmt_data.procedure_call_data.call_kgpc_type = NULL;

    if (type != NULL)
    {
        kgpc_type_retain(type);
        stmt->stmt_data.procedure_call_data.call_kgpc_type = type;
    }
}

static int semcheck_expr_is_char_set(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = expr->resolved_kgpc_type->type_alias;
        if (alias != NULL && alias->is_set && alias->set_element_type == CHAR_TYPE)
            return 1;
    }

    if (expr->type == EXPR_VAR_ID && symtab != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL)
        {
            struct TypeAlias *alias = get_type_alias_from_node(node);
            if (alias != NULL && alias->is_set && alias->set_element_type == CHAR_TYPE)
                return 1;
        }
    }

    return 0;
}

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);

int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_funccall(int *type_return, SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_in(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev);

static int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts, int max_scope_lev);
static int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt, HashNode_t *proc_node,
    int max_scope_lev);
static int semcheck_try_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev);
static int semcheck_convert_property_assignment_to_setter(SymTab_t *symtab,
    struct Statement *stmt, struct Expression *lhs, HashNode_t *setter_node,
    int max_scope_lev);
static int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
    const char *call_suffix);
static HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
    const char *proc_id, const char *call_mangled);
static int semcheck_var_decl_is_untyped(Tree_t *decl);

static int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt, HashNode_t *proc_node,
    int max_scope_lev)
{
    (void)max_scope_lev;
    if (proc_node == NULL || proc_node->type == NULL ||
        proc_node->type->kind != TYPE_KIND_PROCEDURE)
        return 0;

    int return_val = 0;
    ListNode_t *formal_params = proc_node->type->info.proc_info.params;
    ListNode_t *args_given = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 0;

    while (formal_params != NULL && args_given != NULL)
    {
        ++arg_index;
        assert(formal_params->type == LIST_TREE);
        assert(args_given->type == LIST_EXPR);

        Tree_t *param_decl = (Tree_t *)formal_params->cur;
        struct Expression *arg_expr = (struct Expression *)args_given->cur;

        /* Phase 3: Use KgpcType for comprehensive type checking */
        
        /* Resolve KgpcType for the argument expression */
        int arg_type_owned = 0;
        KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);
        
        /* Resolve KgpcType for the formal parameter */
        int param_type_owned = 0;
        KgpcType *param_type = NULL;
        if (param_decl != NULL && param_decl->type == TREE_VAR_DECL)
        {
            param_type = resolve_type_from_vardecl(param_decl, symtab, &param_type_owned);
        }



        /* Both types must be resolved for proper type checking */
        int param_is_untyped = semcheck_var_decl_is_untyped(param_decl);

        if ((arg_type == NULL || param_type == NULL) && !param_is_untyped)
        {
            fprintf(stderr,
                "Error on line %d, on procedure call %s, argument %d: Unable to resolve type!\n\n",
                stmt->line_num,
                stmt->stmt_data.procedure_call_data.id,
                arg_index);
            ++return_val;
        }
        else if (!param_is_untyped)
        {
            /* Use comprehensive KgpcType-based type compatibility checking */
            if (!are_types_compatible_for_assignment(param_type, arg_type, symtab))
            {
                fprintf(stderr,
                    "Error on line %d, on procedure call %s, argument %d: Type mismatch (expected %s, got %s)!\n\n",
                    stmt->line_num,
                    stmt->stmt_data.procedure_call_data.id,
                    arg_index,
                    kgpc_type_to_string(param_type),
                    kgpc_type_to_string(arg_type));
                ++return_val;
            }
        }
        /* Untyped parameters accept any argument without additional checks */

        /* Clean up owned types */
        if (arg_type_owned && arg_type != NULL)
            destroy_kgpc_type(arg_type);
        if (param_type_owned && param_type != NULL)
            destroy_kgpc_type(param_type);

        formal_params = formal_params->next;
        args_given = args_given->next;
    }

    if (formal_params == NULL && args_given != NULL)
    {
        fprintf(stderr, "Error on line %d, on procedure call %s, too many arguments given!\n\n",
            stmt->line_num, stmt->stmt_data.procedure_call_data.id);
        ++return_val;
    }
    else if (formal_params != NULL && args_given == NULL)
    {
        fprintf(stderr, "Error on line %d, on procedure call %s, not enough arguments given!\n\n",
            stmt->line_num, stmt->stmt_data.procedure_call_data.id);
        ++return_val;
    }

    return return_val;
}

typedef int (*builtin_semcheck_handler_t)(SymTab_t *, struct Statement *, int);

static int try_resolve_builtin_procedure(SymTab_t *symtab,
    struct Statement *stmt,
    const char *expected_name,
    builtin_semcheck_handler_t handler,
    int max_scope_lev,
    int *handled)
{
    if (handled != NULL)
        *handled = 0;

    if (symtab == NULL || stmt == NULL || expected_name == NULL || handler == NULL)
        return 0;

    char *proc_id = stmt->stmt_data.procedure_call_data.id;
    if (proc_id == NULL || !pascal_identifier_equals(proc_id, expected_name))
        return 0;

    HashNode_t *builtin_node = NULL;
    if (FindIdent(&builtin_node, symtab, proc_id) != -1 && builtin_node != NULL &&
        builtin_node->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
    {
        stmt->stmt_data.procedure_call_data.resolved_proc = builtin_node;
        stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        
        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = builtin_node->hash_type;
        semcheck_stmt_set_call_kgpc_type(stmt, builtin_node->type,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        
        builtin_node->referenced += 1;
        if (handled != NULL)
            *handled = 1;
        return handler(symtab, stmt, max_scope_lev);
    }

    return 0;
}

static int semcheck_builtin_setlength(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, SetLength expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    struct Expression *array_expr = (struct Expression *)args->cur;
    struct Expression *length_expr = (struct Expression *)args->next->cur;
    
#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_builtin_setlength length_expr=%p\n", length_expr);
#endif

    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&target_type, symtab, array_expr, max_scope_lev, MUTATE);

    int target_is_string = (target_type == STRING_TYPE);

    if (target_is_string)
    {
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        stmt->stmt_data.procedure_call_data.mangled_id = strdup("__kgpc_setlength_string");
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL)
        {
            fprintf(stderr, "Error: failed to allocate mangled name for SetLength.\n");
            ++return_val;
        }
    }
    else
    {
        /* After semantic checking, check if the expression resolved to a dynamic array */
        /* The expression could be EXPR_VAR_ID, EXPR_RECORD_ACCESS, etc. */
        int is_valid_array = 0;
        
        if (array_expr != NULL && array_expr->type == EXPR_VAR_ID)
        {
            /* Simple variable reference */
            HashNode_t *array_node = NULL;
            if (FindIdent(&array_node, symtab, array_expr->expr_data.id) != -1 && array_node != NULL)
            {
                set_hash_meta(array_node, BOTH_MUTATE_REFERENCE);
                
                /* Check if it's a dynamic array using KgpcType first, then legacy field */
                int is_dynamic = 0;
                if (array_node->type != NULL)
                {
                    is_dynamic = kgpc_type_is_dynamic_array(array_node->type);
                }
                else
                {
                    is_dynamic = hashnode_is_dynamic_array(array_node);
                }
                
                if (array_node->hash_type == HASHTYPE_ARRAY && is_dynamic)
                {
                    is_valid_array = 1;
                }
            }
        }
        else if (array_expr != NULL && array_expr->type == EXPR_RECORD_ACCESS)
        {
            /* Record field access - if semantic check passed, assume it's valid
             * TODO: Could enhance this to verify the field is actually a dynamic array */
            is_valid_array = 1;
        }
        
        if (!is_valid_array)
        {
            fprintf(stderr, "Error on line %d, first argument to SetLength must be a dynamic array variable.\n", stmt->line_num);
            ++return_val;
        }
    }

    int length_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&length_type, symtab, length_expr, max_scope_lev, NO_MUTATE);
    if (length_type != INT_TYPE && length_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, SetLength length argument must be an integer.\n", stmt->line_num);
        ++return_val;
    }

    return return_val;
}

static int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts, int max_scope_lev)
{
    int result = 0;
    ListNode_t *cursor = stmts;
    while (cursor != NULL)
    {
        if (cursor->type == LIST_STMT && cursor->cur != NULL)
            result += semcheck_stmt_main(symtab, (struct Statement *)cursor->cur, max_scope_lev);
        cursor = cursor->next;
    }
    return result;
}

static int semcheck_var_decl_is_untyped(Tree_t *decl)
{
    if (decl == NULL || decl->type != TREE_VAR_DECL)
        return 0;
    struct Var *var_info = &decl->tree_data.var_decl_data;
    if (var_info->inline_record_type != NULL)
        return 0;
    return (var_info->type == UNKNOWN_TYPE && var_info->type_id == NULL);
}

static int semcheck_mangled_suffix_matches_untyped(const char *candidate_suffix,
    const char *call_suffix)
{
    if (candidate_suffix == NULL || call_suffix == NULL)
        return 0;

    if (*candidate_suffix == '\0' && *call_suffix == '\0')
        return 1;

    while (*candidate_suffix != '\0' && *call_suffix != '\0')
    {
        if (*candidate_suffix != '_' || *call_suffix != '_')
            return 0;
        candidate_suffix++;
        call_suffix++;

        const char *cand_end = candidate_suffix;
        while (*cand_end != '_' && *cand_end != '\0')
            cand_end++;
        const char *call_end = call_suffix;
        while (*call_end != '_' && *call_end != '\0')
            call_end++;

        size_t cand_len = (size_t)(cand_end - candidate_suffix);
        size_t call_len = (size_t)(call_end - call_suffix);
        int candidate_is_untyped = (cand_len == 1 && candidate_suffix[0] == 'u');

        if (!candidate_is_untyped)
        {
            if (cand_len != call_len || strncmp(candidate_suffix, call_suffix, cand_len) != 0)
                return 0;
        }

        candidate_suffix = cand_end;
        call_suffix = call_end;
    }

    return (*candidate_suffix == '\0' && *call_suffix == '\0');
}

static HashNode_t *semcheck_find_untyped_mangled_match(ListNode_t *candidates,
    const char *proc_id, const char *call_mangled)
{
    if (candidates == NULL || proc_id == NULL || call_mangled == NULL)
        return NULL;

    size_t call_prefix_len = strlen(proc_id);
    if (strlen(call_mangled) < call_prefix_len ||
        strncmp(call_mangled, proc_id, call_prefix_len) != 0)
        return NULL;

    const char *call_suffix = call_mangled + call_prefix_len;
    ListNode_t *cur = candidates;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->mangled_id != NULL && candidate->id != NULL)
        {
            if (pascal_identifier_equals(candidate->id, proc_id))
            {
                size_t cand_prefix_len = strlen(candidate->id);
                if (strlen(candidate->mangled_id) >= cand_prefix_len &&
                    strncmp(candidate->mangled_id, candidate->id, cand_prefix_len) == 0)
                {
                    const char *cand_suffix = candidate->mangled_id + cand_prefix_len;
                    if (semcheck_mangled_suffix_matches_untyped(cand_suffix, call_suffix))
                        return candidate;
                }
            }
        }
        cur = cur->next;
    }

    return NULL;
}

static int semcheck_builtin_strproc(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Str expects exactly two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *value_expr = (struct Expression *)args->cur;
    struct Expression *target_expr = (struct Expression *)args->next->cur;

    int value_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&value_type, symtab, value_expr, INT_MAX, NO_MUTATE);
    if (value_type != INT_TYPE && value_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Str value must be an integer.\n", stmt->line_num);
        ++return_val;
    }

    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    if (target_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Str output must be a string variable.\n", stmt->line_num);
        ++return_val;
    }

    return return_val;
}

static int semcheck_builtin_insert(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL ||
        args->next->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Insert expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;
    struct Expression *source_expr = (struct Expression *)args->cur;
    struct Expression *target_expr = (struct Expression *)args->next->cur;
    struct Expression *index_expr = (struct Expression *)args->next->next->cur;

    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    if (source_type != STRING_TYPE && source_type != CHAR_TYPE)
    {
        fprintf(stderr, "Error on line %d, Insert source must be a string or char.\n",
            stmt->line_num);
        ++error_count;
    }

    int target_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    if (target_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Insert target must be a string variable.\n",
            stmt->line_num);
        ++error_count;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (index_type != INT_TYPE && index_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Insert index must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    return error_count;
}

static int semcheck_builtin_delete(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL ||
        args->next->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Delete expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *index_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;

    int target_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    if (target_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Delete target must be a string variable.\n",
            stmt->line_num);
        ++error_count;
    }

    int index_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&index_type, symtab, index_expr, max_scope_lev, NO_MUTATE);
    if (index_type != INT_TYPE && index_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Delete index must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    int count_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&count_type, symtab, count_expr, max_scope_lev, NO_MUTATE);
    if (count_type != INT_TYPE && count_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Delete count must be an integer.\n",
            stmt->line_num);
        ++error_count;
    }

    return error_count;
}

static int semcheck_builtin_val(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL ||
        args->next->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Val expects exactly three arguments.\n",
            stmt->line_num);
        return 1;
    }

    int error_count = 0;

    struct Expression *source_expr = (struct Expression *)args->cur;
    int source_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&source_type, symtab, source_expr, max_scope_lev, NO_MUTATE);
    if (source_type != STRING_TYPE)
    {
        fprintf(stderr, "Error on line %d, Val expects its first argument to be a string.\n",
            stmt->line_num);
        ++error_count;
    }

    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, MUTATE);
    if (value_type != INT_TYPE && value_type != LONGINT_TYPE && value_type != REAL_TYPE)
    {
        fprintf(stderr,
            "Error on line %d, Val target must be an integer, longint, or real variable.\n",
            stmt->line_num);
        ++error_count;
    }

    struct Expression *code_expr = (struct Expression *)args->next->next->cur;
    int code_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&code_type, symtab, code_expr, max_scope_lev, MUTATE);
    if (code_type != INT_TYPE && code_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Val code argument must be an integer variable.\n",
            stmt->line_num);
        ++error_count;
    }

    return error_count;
}

static int semcheck_builtin_inc(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || (args->next != NULL && args->next->next != NULL))
    {
        fprintf(stderr, "Error on line %d, Inc expects one or two arguments.\n", stmt->line_num);
        return 1;
    }

    int return_val = 0;
    struct Expression *target_expr = (struct Expression *)args->cur;
    int target_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    int target_is_pointer = (target_type == POINTER_TYPE);
    if (target_type != INT_TYPE && target_type != LONGINT_TYPE && !target_is_pointer)
    {
        fprintf(stderr, "Error on line %d, Inc target must be an integer or pointer variable.\n", stmt->line_num);
        ++return_val;
    }

    if (args->next != NULL)
    {
        struct Expression *value_expr = (struct Expression *)args->next->cur;
        int value_type = UNKNOWN_TYPE;
        return_val += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
        if (value_type != INT_TYPE && value_type != LONGINT_TYPE)
        {
            fprintf(stderr, "Error on line %d, Inc increment must be an integer.\n", stmt->line_num);
            ++return_val;
        }
    }

    return return_val;
}

static int semcheck_builtin_dec(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_inc(symtab, stmt, max_scope_lev);
}

static int semcheck_builtin_include_like(SymTab_t *symtab, struct Statement *stmt,
    int max_scope_lev, const char *display_name)
{
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, %s expects exactly two arguments.\n",
            stmt->line_num, display_name);
        return 1;
    }

    int error_count = 0;
    struct Expression *set_expr = (struct Expression *)args->cur;
    int set_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&set_type, symtab, set_expr, max_scope_lev, MUTATE);
    if (set_type != SET_TYPE || !semcheck_expr_is_char_set(symtab, set_expr))
    {
        fprintf(stderr, "Error on line %d, %s target must be a set of char.\n",
            stmt->line_num, display_name);
        ++error_count;
    }

    struct Expression *value_expr = (struct Expression *)args->next->cur;
    int value_type = UNKNOWN_TYPE;
    error_count += semcheck_expr_main(&value_type, symtab, value_expr, max_scope_lev, NO_MUTATE);
    if (value_type != INT_TYPE && value_type != LONGINT_TYPE && value_type != CHAR_TYPE)
    {
        fprintf(stderr, "Error on line %d, %s element must be an ordinal value.\n",
            stmt->line_num, display_name);
        ++error_count;
    }

    return error_count;
}

static int semcheck_builtin_include(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_include_like(symtab, stmt, max_scope_lev, "Include");
}

static int semcheck_builtin_exclude(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_builtin_include_like(symtab, stmt, max_scope_lev, "Exclude");
}

static int semcheck_builtin_write_like(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 1;
    int saw_file_arg = 0;
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        return_val += semcheck_expr_main(&expr_type, symtab, expr, INT_MAX, NO_MUTATE);

        if (!saw_file_arg && expr_type == TEXT_TYPE)
        {
            saw_file_arg = 1;
            args = args->next;
            continue;
        }

        if (expr_type != INT_TYPE && expr_type != LONGINT_TYPE && expr_type != STRING_TYPE && expr_type != BOOL && expr_type != POINTER_TYPE && expr_type != REAL_TYPE && expr_type != CHAR_TYPE && expr_type != ENUM_TYPE)
        {
            fprintf(stderr, "Error on line %d, write argument %d must be integer, longint, real, boolean, string, pointer, or enum.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }

        if (expr != NULL && expr->field_width != NULL)
        {
            int width_type = UNKNOWN_TYPE;
            return_val += semcheck_expr_main(&width_type, symtab, expr->field_width, INT_MAX, NO_MUTATE);
            if (width_type != INT_TYPE && width_type != LONGINT_TYPE)
            {
                fprintf(stderr, "Error on line %d, field width for argument %d must be an integer.\n",
                        stmt->line_num, arg_index);
                ++return_val;
            }
        }

        if (expr != NULL && expr->field_precision != NULL)
        {
            int precision_type = UNKNOWN_TYPE;
            return_val += semcheck_expr_main(&precision_type, symtab, expr->field_precision, INT_MAX, NO_MUTATE);
            if (precision_type != INT_TYPE && precision_type != LONGINT_TYPE)
            {
                fprintf(stderr, "Error on line %d, field precision for argument %d must be an integer.\n",
                        stmt->line_num, arg_index);
                ++return_val;
            }
        }

        args = args->next;
        ++arg_index;
    }

    return return_val;
}

static int semcheck_builtin_read_like(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 1;
    int saw_file_arg = 0;
    
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        
        /* For read, we need to check if this is a file argument first */
        return_val += semcheck_expr_main(&expr_type, symtab, expr, max_scope_lev, NO_MUTATE);
        
        if (!saw_file_arg && expr_type == TEXT_TYPE)
        {
            saw_file_arg = 1;
            args = args->next;
            arg_index++;
            continue;
        }
        
        /* After file arg (if any), remaining args must be mutable lvalues */
        /* Re-check with MUTATE flag to ensure it's an lvalue */
        expr_type = UNKNOWN_TYPE;
        return_val += semcheck_expr_main(&expr_type, symtab, expr, max_scope_lev, MUTATE);
        
        if (expr_type != INT_TYPE && expr_type != LONGINT_TYPE && expr_type != CHAR_TYPE && expr_type != STRING_TYPE && expr_type != REAL_TYPE)
        {
            fprintf(stderr, "Error on line %d, read argument %d must be integer, longint, real, char, or string variable.\n",
                    stmt->line_num, arg_index);
            ++return_val;
        }
        
        args = args->next;
        ++arg_index;
    }

    return return_val;
}

static int semcheck_builtin_new(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, New expects exactly one argument.\\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    int pointer_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&pointer_type, symtab, target_expr, max_scope_lev, MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        fprintf(stderr, "Error on line %d, New expects a pointer variable argument.\\n", stmt->line_num);
        return ++return_val;
    }

    if (target_expr->pointer_subtype == UNKNOWN_TYPE && target_expr->pointer_subtype_id == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to determine allocation type for New.\\n", stmt->line_num);
        return ++return_val;
    }

    if (target_expr->record_type == NULL && target_expr->pointer_subtype_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindIdent(&type_node, symtab, target_expr->pointer_subtype_id) != -1 && type_node != NULL)
            target_expr->record_type = hashnode_get_record_type(type_node);
    }

    return return_val;
}

static int semcheck_builtin_dispose(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Dispose expects exactly one argument.\\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    int pointer_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&pointer_type, symtab, target_expr, max_scope_lev, MUTATE);

    if (pointer_type != POINTER_TYPE)
    {
        fprintf(stderr, "Error on line %d, Dispose expects a pointer variable argument.\\n", stmt->line_num);
        return ++return_val;
    }

    return return_val;
}

/* Semantic check on a normal statement */
int semcheck_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_stmt_main(symtab, stmt, max_scope_lev);
}

/* Semantic check on a function statement (no side effects allowed) */
int semcheck_func_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_stmt_main(symtab, stmt, max_scope_lev);
}

static int semcheck_break_stmt(struct Statement *stmt)
{
    if (semcheck_loop_depth <= 0)
    {
        if (stmt != NULL)
            fprintf(stderr, "Error on line %d, Break is only valid inside a loop.\n", stmt->line_num);
        return 1;
    }
    return 0;
}

static int semcheck_continue_stmt(struct Statement *stmt)
{
    if (semcheck_loop_depth <= 0)
    {
        if (stmt != NULL)
            fprintf(stderr, "Error on line %d, Continue is only valid inside a loop.\n", stmt->line_num);
        return 1;
    }
    return 0;
}


/* Main semantic checking */

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;

    assert(symtab != NULL);
    if (stmt == NULL)
        return 0;
    
    // In semcheck_for:
    // semcheck_loop_depth++;
    // 
    // fprintf(stderr, "DEBUG: semcheck_for stmt=%p line=%d to_expr=%p current_to=%p\n", 
    //         stmt, stmt->line_num, to_expr, stmt->stmt_data.for_data.to);
    // 
    // if (stmt->line_num == 42) {
    //     watch_stmt = stmt;
    //     watch_to_expr = stmt->stmt_data.for_data.to;
    //     fprintf(stderr, "DEBUG: Watching stmt at line 42\n");
    // }
    // 
    // if (to_expr != NULL && ((uintptr_t)to_expr == 0x686374616d || (uintptr_t)to_expr == 0x1db2)) {
    //     fprintf(stderr, "CRITICAL: to_expr is corrupted in semcheck_for!\n");
    // }
    // 
    // return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
    // semcheck_loop_depth--;
    // 
    // if (stmt->stmt_data.for_data.to != to_expr) {
    //     fprintf(stderr, "CRITICAL: stmt->stmt_data.for_data.to changed from %p to %p during body processing!\n",
    //             to_expr, stmt->stmt_data.for_data.to);
    // }
    // 
    // if (watch_stmt == stmt) {
    //     // We are returning from the watched statement.
    //     // It might be checked again in outer loops, but that's fine.
    // }

    return_val = 0;
    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            return_val += semcheck_varassign(symtab, stmt, max_scope_lev);
            break;

        case STMT_PROCEDURE_CALL:
            return_val += semcheck_proccall(symtab, stmt, max_scope_lev);
            break;

        case STMT_COMPOUND_STATEMENT:
            return_val += semcheck_compoundstmt(symtab, stmt, max_scope_lev);
            break;

        case STMT_LABEL:
            if (stmt->stmt_data.label_data.stmt != NULL)
                return_val += semcheck_stmt_main(symtab, stmt->stmt_data.label_data.stmt, max_scope_lev);
            break;

        case STMT_GOTO:
            /* TODO: Validate that the target label exists within scope */
            break;

        case STMT_IF_THEN:
            return_val += semcheck_ifthen(symtab, stmt, max_scope_lev);
            break;

        case STMT_WHILE:
            return_val += semcheck_while(symtab, stmt, max_scope_lev);
            break;

        case STMT_REPEAT:
            return_val += semcheck_repeat(symtab, stmt, max_scope_lev);
            break;

        case STMT_FOR:
            return_val += semcheck_for(symtab, stmt, max_scope_lev);
            break;

        case STMT_FOR_IN:
            return_val += semcheck_for_in(symtab, stmt, max_scope_lev);
            break;

        case STMT_BREAK:
            return_val = semcheck_break_stmt(stmt);
            break;
        case STMT_CONTINUE:
            return_val = semcheck_continue_stmt(stmt);
            break;

        case STMT_ASM_BLOCK:
            /* No semantic checking needed for asm blocks */
            break;

        case STMT_EXIT:
            /* No semantic checking needed for simple control flow statements */
            break;

        case STMT_CASE:
            /* Check the selector expression */
            {
                int selector_type;
                return_val += semcheck_expr(&selector_type, symtab, stmt->stmt_data.case_data.selector_expr, max_scope_lev, 0);
            }
            
            /* Check each case branch */
            {
                ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
                while (branch_node != NULL) {
                    struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
                    if (branch != NULL) {
                        /* Check case labels */
                        ListNode_t *label_node = branch->labels;
                        while (label_node != NULL) {
                            if (label_node->type == LIST_EXPR) {
                                struct Expression *label_expr = (struct Expression *)label_node->cur;
                                int label_type;
                                return_val += semcheck_expr(&label_type, symtab, label_expr, max_scope_lev, 0);
                            } else if (label_node->type == LIST_SET_ELEMENT) {
                                struct SetElement *range = (struct SetElement *)label_node->cur;
                                if (range != NULL) {
                                    if (range->lower != NULL) {
                                        int lower_type;
                                        return_val += semcheck_expr(&lower_type, symtab, range->lower, max_scope_lev, 0);
                                    }
                                    if (range->upper != NULL) {
                                        int upper_type;
                                        return_val += semcheck_expr(&upper_type, symtab, range->upper, max_scope_lev, 0);
                                    }
                                }
                            }
                            label_node = label_node->next;
                        }
                        /* Check the branch statement */
                        if (branch->stmt != NULL)
                            return_val += semcheck_stmt(symtab, branch->stmt, max_scope_lev);
                    }
                    branch_node = branch_node->next;
                }
            }
            
            /* Check the else statement if present */
            if (stmt->stmt_data.case_data.else_stmt != NULL)
                return_val += semcheck_stmt(symtab, stmt->stmt_data.case_data.else_stmt, max_scope_lev);
            break;

        case STMT_WITH:
        {
            struct Expression *context_expr = stmt->stmt_data.with_data.context_expr;
            struct Statement *body_stmt = stmt->stmt_data.with_data.body_stmt;
            struct RecordType *record_info = NULL;
            int ctx_type = UNKNOWN_TYPE;
            int pushed = 0;

            if (context_expr == NULL)
            {
                fprintf(stderr, "Error on line %d, WITH statement requires a context expression.\\n\\n",
                    stmt->line_num);
                ++return_val;
            }
            else
            {
                return_val += semcheck_expr_main(&ctx_type, symtab, context_expr, max_scope_lev, NO_MUTATE);
                record_info = semcheck_with_resolve_record_type(symtab, context_expr, ctx_type, stmt->line_num);
                if (record_info == NULL)
                {
                    fprintf(stderr,
                        "Error on line %d, WITH context must be a record or pointer to a record.\\n\\n",
                        stmt->line_num);
                    ++return_val;
                }
                else
                {
                    context_expr->record_type = record_info;
                    if (semcheck_with_push(context_expr, record_info) != 0)
                    {
                        ++return_val;
                    }
                    else
                    {
                        pushed = 1;
                    }
                }
            }

            if (body_stmt != NULL)
                return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);

            if (pushed)
                semcheck_with_pop();
            break;
        }

        case STMT_TRY_FINALLY:
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_finally_data.try_statements, max_scope_lev);
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_finally_data.finally_statements, max_scope_lev);
            break;

        case STMT_TRY_EXCEPT:
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.try_statements, max_scope_lev);
            
            /* If there's an 'on E: Exception do' clause, create a local scope for the exception variable */
            if (stmt->stmt_data.try_except_data.has_on_clause && 
                stmt->stmt_data.try_except_data.exception_var_name != NULL) {
                
                /* Push a new scope for the exception variable */
                PushScope(symtab);
                
                /* Add the exception variable to the symbol table */
                char *var_name = stmt->stmt_data.try_except_data.exception_var_name;
                char *type_name = stmt->stmt_data.try_except_data.exception_type_name;
                
                /* Look up the type if specified, otherwise use NULL (defaults to integer) */
                KgpcType *var_kgpc_type = NULL;
                if (type_name != NULL) {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, type_name) >= 0 && type_node != NULL) {
                        if (type_node->hash_type == HASHTYPE_TYPE) {
                            var_kgpc_type = type_node->type;
                        } else {
                            fprintf(stderr, "Error: '%s' is not a type at line %d\n", 
                                    type_name, stmt->line_num);
                            return_val++;
                        }
                    } else {
                        fprintf(stderr, "Error: Unknown exception type '%s' at line %d\n", 
                                type_name, stmt->line_num);
                        return_val++;
                    }
                }
                
                /* Push the exception variable onto the scope (NULL type defaults to integer) */
                PushVarOntoScope_Typed(symtab, var_name, var_kgpc_type);
                
                /* Semantic check the except statements in the new scope */
                return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.except_statements, max_scope_lev);
                
                /* Pop the scope */
                PopScope(symtab);
            } else {
                /* No exception variable - just check the except statements normally */
                return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.except_statements, max_scope_lev);
            }
            break;

        case STMT_RAISE:
            if (stmt->stmt_data.raise_data.exception_expr != NULL)
            {
                int raise_type = UNKNOWN_TYPE;
                return_val += semcheck_expr_main(&raise_type, symtab, stmt->stmt_data.raise_data.exception_expr, INT_MAX, NO_MUTATE);
            }
            break;

        case STMT_INHERITED:
            if (stmt->stmt_data.inherited_data.call_expr != NULL)
            {
                struct Expression *call_expr = stmt->stmt_data.inherited_data.call_expr;
                
                /* Handle EXPR_VAR_ID by converting to EXPR_FUNCTION_CALL */
                if (call_expr->type == EXPR_VAR_ID)
                {
                    /* Save the id from the VAR_ID before converting */
                    char *var_id = call_expr->expr_data.id;
                    
                    /* Convert to EXPR_FUNCTION_CALL */
                    call_expr->type = EXPR_FUNCTION_CALL;
                    call_expr->expr_data.function_call_data.id = var_id;
                    call_expr->expr_data.function_call_data.args_expr = NULL;
                    call_expr->expr_data.function_call_data.mangled_id = NULL;
                    call_expr->expr_data.function_call_data.resolved_func = NULL;
                    call_expr->expr_data.function_call_data.call_hash_type = 0;
                    call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
                    call_expr->expr_data.function_call_data.is_call_info_valid = 0;
                }
                
                if (call_expr->type == EXPR_FUNCTION_CALL)
                {
                    HashNode_t *target_symbol = NULL;
                    const char *call_id = call_expr->expr_data.function_call_data.id;
                    if (call_id != NULL)
                        FindIdent(&target_symbol, symtab, (char *)call_id);

                    int is_function_symbol = (target_symbol != NULL) &&
                        (target_symbol->hash_type == HASHTYPE_FUNCTION ||
                         target_symbol->hash_type == HASHTYPE_FUNCTION_RETURN);

                    if (!is_function_symbol)
                    {
                        /* For inherited procedure calls, check if we need to handle Create/Destroy with no parent */
                        const char *method_name = call_expr->expr_data.function_call_data.id;
                        HashNode_t *self_node = NULL;
                        const char *parent_class_name = NULL;
                        if (FindIdent(&self_node, symtab, "Self") != -1 && self_node != NULL &&
                            self_node->type != NULL && self_node->type->kind == TYPE_KIND_RECORD &&
                            self_node->type->info.record_info != NULL)
                        {
                            struct RecordType *current_class = self_node->type->info.record_info;
                            parent_class_name = current_class->parent_class_name;
                            
                            /* Check if there's no parent class and this is Create or Destroy */
                            if (current_class->parent_class_name == NULL && method_name != NULL &&
                                (strcasecmp(method_name, "Create") == 0 || strcasecmp(method_name, "Destroy") == 0))
                            {
                                /* No parent class - convert to empty compound statement (no-op) */
                                if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                                {
                                    fprintf(stderr, "[KGPC] Inherited %s with no parent class - converting to no-op\n",
                                            method_name);
                                }
                                /* Convert this inherited statement to an empty compound statement */
                                stmt->type = STMT_COMPOUND_STATEMENT;
                                stmt->stmt_data.compound_statement = NULL;
                                /* No errors */
                                break;
                            }
                        }

                        /* If a parent exists, direct the call to the parent's mangled name */
                        if (parent_class_name != NULL && method_name != NULL)
                        {
                            char parent_mangled[512];
                            snprintf(parent_mangled, sizeof(parent_mangled), "%s__%s",
                                parent_class_name, method_name);
                            if (call_expr->expr_data.function_call_data.mangled_id != NULL)
                                free(call_expr->expr_data.function_call_data.mangled_id);
                            call_expr->expr_data.function_call_data.mangled_id = strdup(parent_mangled);
                        }
                        
                        struct Statement temp_call;
                        memset(&temp_call, 0, sizeof(temp_call));
                        temp_call.type = STMT_PROCEDURE_CALL;
                        temp_call.line_num = stmt->line_num;
                        temp_call.stmt_data.procedure_call_data.id = call_expr->expr_data.function_call_data.id;
                        temp_call.stmt_data.procedure_call_data.expr_args = call_expr->expr_data.function_call_data.args_expr;
                        temp_call.stmt_data.procedure_call_data.mangled_id = NULL;
                        temp_call.stmt_data.procedure_call_data.resolved_proc = NULL;

                        return_val += semcheck_proccall(symtab, &temp_call, max_scope_lev);

                        if (temp_call.stmt_data.procedure_call_data.mangled_id != NULL)
                        {
                            if (call_expr->expr_data.function_call_data.mangled_id != NULL)
                            {
                                free(call_expr->expr_data.function_call_data.mangled_id);
                                call_expr->expr_data.function_call_data.mangled_id = NULL;
                            }
                            call_expr->expr_data.function_call_data.mangled_id = temp_call.stmt_data.procedure_call_data.mangled_id;
                            temp_call.stmt_data.procedure_call_data.mangled_id = NULL;
                        }
                        call_expr->expr_data.function_call_data.call_hash_type =
                            temp_call.stmt_data.procedure_call_data.call_hash_type;
                        if (call_expr->expr_data.function_call_data.call_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(call_expr->expr_data.function_call_data.call_kgpc_type);
                            call_expr->expr_data.function_call_data.call_kgpc_type = NULL;
                        }
                        if (temp_call.stmt_data.procedure_call_data.call_kgpc_type != NULL)
                        {
                            kgpc_type_retain(temp_call.stmt_data.procedure_call_data.call_kgpc_type);
                            call_expr->expr_data.function_call_data.call_kgpc_type =
                                temp_call.stmt_data.procedure_call_data.call_kgpc_type;
                        }
                        call_expr->expr_data.function_call_data.is_call_info_valid =
                            temp_call.stmt_data.procedure_call_data.is_call_info_valid;
                        semcheck_stmt_set_call_kgpc_type(&temp_call, NULL,
                            temp_call.stmt_data.procedure_call_data.is_call_info_valid == 1);
                        temp_call.stmt_data.procedure_call_data.is_call_info_valid = 0;
                    }
                    else
                    {
                        int inherited_type = UNKNOWN_TYPE;
                        return_val += semcheck_funccall(&inherited_type, symtab, call_expr, max_scope_lev, NO_MUTATE);
                    }
                }
                else
                {
                    /* For other expression types, use general expression checking */
                    int expr_type = UNKNOWN_TYPE;
                    return_val += semcheck_expr_main(&expr_type, symtab, call_expr, max_scope_lev, NO_MUTATE);
                }
            }
            break;

        default:
            assert(0 && "Bad type in semcheck_stmt!");
            break;
    }

    return return_val;
}


/****** STMT SEMCHECKS *******/

/** VAR_ASSIGN **/
static const char *type_tag_to_name(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:
            return "integer";
        case LONGINT_TYPE:
            return "longint";
        case REAL_TYPE:
            return "real";
        case STRING_TYPE:
            return "string";
        case BOOL:
            return "boolean";
        case CHAR_TYPE:
            return "char";
        case PROCEDURE:
            return "procedure";
        case SET_TYPE:
            return "set";
        case RECORD_TYPE:
            return "record";
        case POINTER_TYPE:
            return "pointer";
        case UNKNOWN_TYPE:
            return "unknown";
        default:
            return "unsupported";
    }
}

int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int type_first, type_second;
    struct Expression *var, *expr;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);

    return_val = 0;

    var = stmt->stmt_data.var_assign_data.var;
    expr = stmt->stmt_data.var_assign_data.expr;

    rewrite_tfpglist_constructor_if_needed(symtab, max_scope_lev, var,
        &stmt->stmt_data.var_assign_data.expr);
    expr = stmt->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    /* Left side var assigns must abide by scoping rules */
    return_val += semcheck_expr_main(&type_first, symtab, var, max_scope_lev, MUTATE);
    return_val += semcheck_expr_main(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL && expr->type == EXPR_FUNCTION_CALL &&
        expr->expr_data.function_call_data.id != NULL &&
        strcasecmp(expr->expr_data.function_call_data.id, "Create") == 0) {
        fprintf(stderr, "[SemCheck] semcheck_varassign calling semcheck_resolve_expression_kgpc_type:\n");
        fprintf(stderr, "[SemCheck]   expr=%p type=%d\n", (void*)expr, expr->type);
        fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
    }
    
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && expr != NULL && expr->type == EXPR_RECORD_ACCESS) {
        fprintf(stderr, "[SemCheck] semcheck_varassign: expr is EXPR_RECORD_ACCESS\n");
        fprintf(stderr, "[SemCheck]   expr=%p\n", (void*)expr);
        fprintf(stderr, "[SemCheck]   expr->resolved_kgpc_type=%p\n", (void*)expr->resolved_kgpc_type);
    }

    int lhs_owned = 0, rhs_owned = 0;
    KgpcType *lhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, var, max_scope_lev, MUTATE, &lhs_owned);
    KgpcType *rhs_kgpctype = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX, NO_MUTATE, &rhs_owned);
    int handled_by_kgpctype = 0;

    if (lhs_kgpctype != NULL && rhs_kgpctype != NULL)
    {
        handled_by_kgpctype = 1;
        
        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
            fprintf(stderr, "[SemCheck] Type compatibility check:\n");
            fprintf(stderr, "[SemCheck]   lhs_kgpctype=%p kind=%d\n", (void*)lhs_kgpctype, lhs_kgpctype->kind);
            fprintf(stderr, "[SemCheck]   rhs_kgpctype=%p kind=%d\n", (void*)rhs_kgpctype, rhs_kgpctype->kind);
            if (lhs_kgpctype->kind == TYPE_KIND_POINTER && lhs_kgpctype->info.points_to != NULL) {
                fprintf(stderr, "[SemCheck]   lhs points_to=%p kind=%d\n", 
                    (void*)lhs_kgpctype->info.points_to, lhs_kgpctype->info.points_to->kind);
                if (lhs_kgpctype->info.points_to->kind == TYPE_KIND_RECORD) {
                    fprintf(stderr, "[SemCheck]   lhs record_info=%p\n", 
                        (void*)lhs_kgpctype->info.points_to->info.record_info);
                }
            }
            if (rhs_kgpctype->kind == TYPE_KIND_POINTER && rhs_kgpctype->info.points_to != NULL) {
                fprintf(stderr, "[SemCheck]   rhs points_to=%p kind=%d\n", 
                    (void*)rhs_kgpctype->info.points_to, rhs_kgpctype->info.points_to->kind);
                if (rhs_kgpctype->info.points_to->kind == TYPE_KIND_RECORD) {
                    fprintf(stderr, "[SemCheck]   rhs record_info=%p\n", 
                        (void*)rhs_kgpctype->info.points_to->info.record_info);
                }
            }
        }
        
        if (!are_types_compatible_for_assignment(lhs_kgpctype, rhs_kgpctype, symtab))
        {
            const char *lhs_name = "<expression>";
            if (var != NULL && var->type == EXPR_VAR_ID)
                lhs_name = var->expr_data.id;
            fprintf(stderr,
                "Error on line %d, incompatible types in assignment for %s (lhs: %s, rhs: %s)!\n\n",
                stmt->line_num,
                lhs_name,
                kgpc_type_to_string(lhs_kgpctype),
                kgpc_type_to_string(rhs_kgpctype));
            ++return_val;
        }
        else if (type_first == PROCEDURE && type_second == PROCEDURE)
        {
            /* AST TRANSFORMATION: Mark RHS as procedure address if it's a direct procedure reference */
            /* Only transform if BOTH LHS and RHS are actual procedures (not functions) */
            /* Functions should be called, not have their address taken */
            int lhs_is_procedure = (lhs_kgpctype->kind == TYPE_KIND_PROCEDURE && 
                                    lhs_kgpctype->info.proc_info.return_type == NULL);
            int rhs_is_procedure = (rhs_kgpctype->kind == TYPE_KIND_PROCEDURE && 
                                    rhs_kgpctype->info.proc_info.return_type == NULL);
            
            if (lhs_is_procedure && rhs_is_procedure && expr != NULL && expr->type == EXPR_VAR_ID)
            {
                HashNode_t *rhs_symbol = NULL;
                if (FindIdent(&rhs_symbol, symtab, expr->expr_data.id) >= 0 &&
                    rhs_symbol != NULL && rhs_symbol->hash_type == HASHTYPE_PROCEDURE)
                {
                    /* Transform the expression to EXPR_ADDR_OF_PROC */
                    expr->type = EXPR_ADDR_OF_PROC;
                    expr->expr_data.addr_of_proc_data.procedure_symbol = rhs_symbol;
                }
            }
        }
    }

    if (!handled_by_kgpctype)
    {
        int coerced_rhs_type = type_second;
        int types_compatible = (type_first == type_second);
        
        /* Reject assignment of scalar to array or vice versa */
        /* Arrays must be assigned from arrays, scalars from scalars */
        if (types_compatible && var != NULL && expr != NULL &&
            var->is_array_expr != expr->is_array_expr)
        {
            types_compatible = 0;
        }

        if (!types_compatible)
        {
            if ((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
                (type_first == INT_TYPE && type_second == LONGINT_TYPE))
            {
                types_compatible = 1;
            }
            else if (type_first == REAL_TYPE &&
                (type_second == INT_TYPE || type_second == LONGINT_TYPE))
            {
                types_compatible = 1;
                coerced_rhs_type = REAL_TYPE;
                if (expr != NULL)
                {
                    if (expr->type == EXPR_INUM)
                    {
                        double coerced_value = (double)expr->expr_data.i_num;
                        expr->type = EXPR_RNUM;
                        expr->expr_data.r_num = coerced_value;
                    }
                    expr->resolved_type = REAL_TYPE;
                }
            }
            else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
                expr != NULL && expr->type == EXPR_STRING &&
                expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1)
            {
                types_compatible = 1;
                coerced_rhs_type = CHAR_TYPE;
                expr->resolved_type = CHAR_TYPE;
            }
            /* Allow char to string assignment - char will be promoted to single-character string */
            /* Only for actual string variables, not char arrays */
            else if (type_first == STRING_TYPE && type_second == CHAR_TYPE &&
                var != NULL && !var->is_array_expr)
            {
                types_compatible = 1;
                /* Keep CHAR_TYPE so code generator knows to promote */
            }
            /* Allow char assignment to char arrays (FPC compatibility) */
            else if (type_first == CHAR_TYPE && type_second == CHAR_TYPE &&
                var != NULL && var->is_array_expr && var->array_element_type == CHAR_TYPE &&
                (expr == NULL || !expr->is_array_expr))
            {
                types_compatible = 1;
                fprintf(stderr,
                    "Warning on line %d, assigning char to array of char copies only the first element (FPC compatibility).\n\n",
                    stmt->line_num);
            }
            /* Allow string literal assignment to char arrays */
            else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
                var != NULL && var->is_array_expr && var->array_element_type == CHAR_TYPE &&
                expr != NULL && expr->type == EXPR_STRING)
            {
                /* Verify string fits in array (including null terminator) */
                size_t string_len = expr->expr_data.string != NULL ? strlen(expr->expr_data.string) : 0;
                int array_size = var->array_upper_bound - var->array_lower_bound + 1;
                
                if (string_len > (size_t)array_size)
                {
                    const char *lhs_name = (var->type == EXPR_VAR_ID) ? var->expr_data.id : "<expression>";
                    fprintf(stderr,
                        "Error on line %d, string literal too long for array %s (string length: %zu, array size: %d)!\n\n",
                        stmt->line_num,
                        lhs_name,
                        string_len,
                        array_size);
                    ++return_val;
                }
                else
                {
                    types_compatible = 1;
                    /* Keep the type as STRING_TYPE to signal code generator to emit string-to-array copy */
                }
            }
        }

        if (!types_compatible)
        {
            const char *lhs_name = "<expression>";
            if (var != NULL && var->type == EXPR_VAR_ID)
                lhs_name = var->expr_data.id;
            semantic_error(stmt->line_num, stmt->col_num,
                "type mismatch in assignment statement for %s (lhs: %s, rhs: %s)",
                lhs_name,
                type_tag_to_name(type_first),
                type_tag_to_name(type_second));
            ++return_val;
        }
        else
        {
            type_second = coerced_rhs_type;
        }
    }

    int property_result = semcheck_try_property_assignment(symtab, stmt, max_scope_lev);
    if (property_result >= 0)
    {
        if (lhs_owned && lhs_kgpctype != NULL)
            destroy_kgpc_type(lhs_kgpctype);
        if (rhs_owned && rhs_kgpctype != NULL)
            destroy_kgpc_type(rhs_kgpctype);
        return return_val + property_result;
    }

    /* Clean up owned KgpcTypes */
    if (lhs_owned && lhs_kgpctype != NULL)
        destroy_kgpc_type(lhs_kgpctype);
    if (rhs_owned && rhs_kgpctype != NULL)
        destroy_kgpc_type(rhs_kgpctype);

    return return_val;
}

static int semcheck_convert_property_assignment_to_setter(SymTab_t *symtab,
    struct Statement *stmt, struct Expression *lhs, HashNode_t *setter_node,
    int max_scope_lev)
{
    struct Expression *object_expr = lhs->expr_data.record_access_data.record_expr;
    if (object_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, property assignment requires an object instance.\n\n",
            stmt->line_num);
        return 1;
    }

    lhs->expr_data.record_access_data.record_expr = NULL;
    struct Expression *value_expr = stmt->stmt_data.var_assign_data.expr;
    stmt->stmt_data.var_assign_data.expr = NULL;

    destroy_expr(lhs);
    stmt->stmt_data.var_assign_data.var = NULL;

    ListNode_t *self_arg = CreateListNode(object_expr, LIST_EXPR);
    if (self_arg == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to allocate setter argument list.\n\n",
            stmt->line_num);
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        return 1;
    }

    ListNode_t *value_arg = CreateListNode(value_expr, LIST_EXPR);
    if (value_arg == NULL)
    {
        fprintf(stderr, "Error on line %d, unable to allocate setter argument list.\n\n",
            stmt->line_num);
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        free(self_arg);
        return 1;
    }
    self_arg->next = value_arg;

    char *id_copy = setter_node->id != NULL ? strdup(setter_node->id) : NULL;
    char *mangled_copy = NULL;
    if (setter_node->mangled_id != NULL)
        mangled_copy = strdup(setter_node->mangled_id);

    if ((setter_node->id != NULL && id_copy == NULL) ||
        (setter_node->mangled_id != NULL && mangled_copy == NULL))
    {
        fprintf(stderr, "Error on line %d, unable to prepare property setter call.\n\n",
            stmt->line_num);
        free(id_copy);
        free(mangled_copy);
        value_arg->next = NULL;
        destroy_expr(object_expr);
        destroy_expr(value_expr);
        free(value_arg);
        free(self_arg);
        return 1;
    }

    stmt->type = STMT_PROCEDURE_CALL;
    stmt->stmt_data.procedure_call_data.id = id_copy;
    stmt->stmt_data.procedure_call_data.mangled_id = mangled_copy;
    stmt->stmt_data.procedure_call_data.expr_args = self_arg;
    stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
    stmt->stmt_data.procedure_call_data.call_hash_type = 0;
    semcheck_stmt_set_call_kgpc_type(stmt, NULL,
        stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
    stmt->stmt_data.procedure_call_data.is_call_info_valid = 0;

    return semcheck_proccall(symtab, stmt, max_scope_lev);
}

static int semcheck_try_property_assignment(SymTab_t *symtab,
    struct Statement *stmt, int max_scope_lev)
{
    if (stmt == NULL || stmt->type != STMT_VAR_ASSIGN)
        return -1;

    struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
    if (lhs == NULL || lhs->type != EXPR_RECORD_ACCESS)
        return -1;

    const char *property_name = lhs->expr_data.record_access_data.field_id;
    if (property_name == NULL)
        return -1;

    struct Expression *object_expr = lhs->expr_data.record_access_data.record_expr;
    if (object_expr == NULL)
        return -1;

    struct RecordType *object_record = semcheck_with_resolve_record_type(symtab,
        object_expr, object_expr->resolved_type, stmt->line_num);
    if (object_record == NULL)
        object_record = object_expr->record_type;

    if (object_record == NULL || !record_type_is_class(object_record))
        return -1;

    struct RecordType *property_owner = NULL;
    struct ClassProperty *property = semcheck_find_class_property(symtab,
        object_record, property_name, &property_owner);
    if (property == NULL || property->write_accessor == NULL)
        return -1;

    struct RecordField *write_field =
        semcheck_find_class_field_including_hidden(symtab,
            object_record, property->write_accessor, NULL);
    if (write_field != NULL)
        return -1;

    HashNode_t *setter_node = semcheck_find_class_method(symtab,
        property_owner, property->write_accessor, NULL);
    if (setter_node == NULL)
    {
        fprintf(stderr, "Error on line %d, setter %s for property %s not found.\n\n",
            stmt->line_num,
            property->write_accessor != NULL ? property->write_accessor : "<unknown>",
            property->name != NULL ? property->name : property_name);
        return 1;
    }

    if (setter_node->hash_type != HASHTYPE_PROCEDURE)
    {
        fprintf(stderr, "Error on line %d, property setter %s must be a procedure.\n\n",
            stmt->line_num, property->write_accessor);
        return 1;
    }

    return semcheck_convert_property_assignment_to_setter(symtab, stmt, lhs,
        setter_node, max_scope_lev);
}

/** PROCEDURE_CALL **/
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val, scope_return, cur_arg;
    HashNode_t *sym_return;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    Tree_t *arg_decl;
    char *proc_id;
    char *mangled_name;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);

    return_val = 0;

    proc_id = stmt->stmt_data.procedure_call_data.id;
    args_given = stmt->stmt_data.procedure_call_data.expr_args;

    int handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetLength",
        semcheck_builtin_setlength, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "write",
        semcheck_builtin_write_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "writeln",
        semcheck_builtin_write_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "read",
        semcheck_builtin_read_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "readln",
        semcheck_builtin_read_like, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Val",
        semcheck_builtin_val, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Str",
        semcheck_builtin_strproc, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Insert",
        semcheck_builtin_insert, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Delete",
        semcheck_builtin_delete, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Inc",
        semcheck_builtin_inc, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Dec",
        semcheck_builtin_dec, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Include",
        semcheck_builtin_include, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Exclude",
        semcheck_builtin_exclude, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "New",
        semcheck_builtin_new, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Dispose",
        semcheck_builtin_dispose, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    /* Check if this is a method call that needs type-based resolution.
     * Method calls have the pattern: ClassName__MethodName(object, ...)
     * We need to verify the ClassName matches the object's actual type.
     * If not, we need to correct it based on the object's type.
     */
    if (proc_id != NULL && strstr(proc_id, "__") != NULL && args_given != NULL) {
        char *double_underscore = strstr(proc_id, "__");
        if (double_underscore != NULL) {
            /* Extract the method name (part after __) */
            char *method_name_part = double_underscore + 2;
            
            /* Get the first argument (should be the object/Self parameter) */
            struct Expression *first_arg = (struct Expression *)args_given->cur;
            if (first_arg != NULL) {
                /* Resolve the type of the first argument */
                int arg_type_owned = 0;
                KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab, first_arg, INT_MAX, NO_MUTATE, &arg_type_owned);
                
                if (arg_type != NULL) {
                    struct RecordType *obj_record_type = NULL;
                    
                    if (arg_type->kind == TYPE_KIND_RECORD) {
                        obj_record_type = arg_type->info.record_info;
                    } else if (arg_type->kind == TYPE_KIND_POINTER && 
                               arg_type->info.points_to != NULL &&
                               arg_type->info.points_to->kind == TYPE_KIND_RECORD) {
                        obj_record_type = arg_type->info.points_to->info.record_info;
                    }
                    
                    if (obj_record_type != NULL) {
                        /* Found the object with a record type. Now find the class name for this type. */
                    
                    /* Search for a type declaration with this RecordType */
                    /* We need to iterate through all identifiers in the symbol table */
                    char *correct_class_name = NULL;
                    
                    /* Walk through symbol table scopes to find matching type */
                    ListNode_t *scope_list = symtab->stack_head;
                    while (scope_list != NULL && correct_class_name == NULL) {
                        HashTable_t *hash_table = (HashTable_t *)scope_list->cur;
                        if (hash_table != NULL) {
                            for (int i = 0; i < TABLE_SIZE && correct_class_name == NULL; i++) {
                                ListNode_t *bucket = hash_table->table[i];
                                while (bucket != NULL && correct_class_name == NULL) {
                                    HashNode_t *node = (HashNode_t *)bucket->cur;
                                    if (node != NULL && node->hash_type == HASHTYPE_TYPE && node->type != NULL) {
                                        /* Check direct record type */
                                        if (node->type->kind == TYPE_KIND_RECORD &&
                                            node->type->info.record_info == obj_record_type) {
                                            correct_class_name = node->id;
                                            break;
                                        }
                                        /* Check class type (pointer to record) */
                                        else if (node->type->kind == TYPE_KIND_POINTER &&
                                                 node->type->info.points_to != NULL &&
                                                 node->type->info.points_to->kind == TYPE_KIND_RECORD &&
                                                 node->type->info.points_to->info.record_info == obj_record_type) {
                                            correct_class_name = node->id;
                                            break;
                                        }
                                    }
                                    bucket = bucket->next;
                                }
                            }
                        }
                        scope_list = scope_list->next;
                    }
                    
                    
                    if (correct_class_name != NULL) {
                        /* Walk up the inheritance chain to find the method */
                        struct RecordType *current_record = obj_record_type;
                        char *current_class_name = correct_class_name;
                        int method_found = 0;
                        
                        while (current_record != NULL && current_class_name != NULL) {
                            /* Build the mangled name for the current class */
                            size_t class_len = strlen(current_class_name);
                            size_t method_len = strlen(method_name_part);
                            char *mangled_name = (char *)malloc(class_len + 2 + method_len + 1);
                            sprintf(mangled_name, "%s__%s", current_class_name, method_name_part);
                            
                            /* Check if this mangled name exists in the symbol table */
                            HashNode_t *proc_node = NULL;
                            if (FindIdent(&proc_node, symtab, mangled_name) != -1 && proc_node != NULL) {
                                /* Found it! Update the procedure ID */
                                free(proc_id);
                                proc_id = mangled_name;
                                stmt->stmt_data.procedure_call_data.id = proc_id;
                                stmt->stmt_data.procedure_call_data.mangled_id = strdup(proc_id);
                                method_found = 1;
                                break;
                            }
                            
                            free(mangled_name);
                            
                            /* Not found in this class, try parent */
                            if (current_record->parent_class_name != NULL) {
                                char *parent_name = current_record->parent_class_name;
                                
                                /* Look up parent class record type */
                                HashNode_t *parent_node = NULL;
                                if (FindIdent(&parent_node, symtab, parent_name) != -1 && 
                                    parent_node != NULL && parent_node->type != NULL) {
                                    
                                    if (parent_node->type->kind == TYPE_KIND_RECORD) {
                                        current_record = parent_node->type->info.record_info;
                                    } else if (parent_node->type->kind == TYPE_KIND_POINTER && 
                                               parent_node->type->info.points_to != NULL &&
                                               parent_node->type->info.points_to->kind == TYPE_KIND_RECORD) {
                                        current_record = parent_node->type->info.points_to->info.record_info;
                                    } else {
                                        current_record = NULL;
                                    }
                                    current_class_name = parent_name;
                                } else {
                                    /* Parent not found in symbol table (shouldn't happen for valid code) */
                                    current_record = NULL;
                                }
                            } else {
                                /* No parent */
                                current_record = NULL;
                            }
                        }
                        
                        if (!method_found) {
                            /* If we didn't find it in the hierarchy, fallback to the original class name 
                             * so the error message makes sense (or maybe it's a virtual method that will be resolved later?)
                             * Actually, if we don't find it, we should probably leave it as is or try to construct
                             * the name for the base class to let the standard check fail with a clear message.
                             */
                             size_t class_len = strlen(correct_class_name);
                             size_t method_len = strlen(method_name_part);
                             char *mangled_name = (char *)malloc(class_len + 2 + method_len + 1);
                             sprintf(mangled_name, "%s__%s", correct_class_name, method_name_part);
                             free(proc_id);
                             proc_id = mangled_name;
                             stmt->stmt_data.procedure_call_data.id = proc_id;
                             stmt->stmt_data.procedure_call_data.mangled_id = strdup(proc_id);
                        }
                    }
                }
            }
            }
        }
    }

    mangled_name = MangleFunctionNameFromCallSite(proc_id, args_given, symtab, INT_MAX);
    assert(mangled_name != NULL);

    ListNode_t *overload_candidates = FindAllIdents(symtab, proc_id);
    HashNode_t *resolved_proc = NULL;
    int match_count = 0;

    if (overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while(cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, mangled_name) == 0)
            {
                /* Found a match. For procedures registered in multiple scopes
                 * (e.g., for recursion), we may find the same mangled name multiple times.
                 * Just keep the first match - they're functionally equivalent. */
                if (resolved_proc == NULL) {
                    resolved_proc = candidate;
                }
                match_count++;
            }
            cur = cur->next;
        }
    }
    
    /* If no match found and this is a method call, try parent classes */
    if (resolved_proc == NULL && proc_id != NULL && strstr(proc_id, "__") != NULL) {
        char *double_underscore = strstr(proc_id, "__");
        if (double_underscore != NULL) {
            /* Extract class name and method name */
            size_t class_name_len = double_underscore - proc_id;
            char *class_name = (char *)malloc(class_name_len + 1);
            if (class_name != NULL) {
                memcpy(class_name, proc_id, class_name_len);
                class_name[class_name_len] = '\0';
            }
            char *method_name = strdup(double_underscore + 2);
            
            if (class_name != NULL && method_name != NULL) {
                if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                {
                    fprintf(stderr, "[KGPC] Trying to resolve inherited call: class=%s method=%s\n",
                            class_name, method_name);
                }
                /* Look up the class to find its parent */
                HashNode_t *class_node = NULL;
                if (FindIdent(&class_node, symtab, class_name) != -1 && class_node != NULL && 
                    class_node->type != NULL && class_node->type->kind == TYPE_KIND_RECORD &&
                    class_node->type->info.record_info != NULL) {
                    
                    struct RecordType *record_info = class_node->type->info.record_info;
                    char *parent_class_name = record_info->parent_class_name;
                    
                    if (getenv("KGPC_DEBUG_INHERITED") != NULL)
                    {
                        fprintf(stderr, "[KGPC]   Found class %s, parent_class_name=%s\n",
                                class_name, parent_class_name ? parent_class_name : "<NULL>");
                    }
                    
                    /* Walk up the inheritance chain */
                    while (parent_class_name != NULL && resolved_proc == NULL) {
                        /* Try to find the method in the parent class */
                        char *parent_method_name = (char *)malloc(strlen(parent_class_name) + 2 + strlen(method_name) + 1);
                        if (parent_method_name != NULL) {
                            snprintf(parent_method_name, strlen(parent_class_name) + 2 + strlen(method_name) + 1,
                                    "%s__%s", parent_class_name, method_name);
                            
                            /* Use the same name mangling function that's used for regular method calls */
                            char *parent_mangled_name = MangleFunctionNameFromCallSite(parent_method_name, args_given, symtab, INT_MAX);
                            if (parent_mangled_name != NULL) {
                                /* Look for the parent method using the base name, then check mangled names */
                                ListNode_t *parent_candidates = FindAllIdents(symtab, parent_method_name);
                                
                                if (parent_candidates != NULL) {
                                    ListNode_t *cur = parent_candidates;
                                    while (cur != NULL) {
                                        HashNode_t *candidate = (HashNode_t *)cur->cur;
                                        if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, parent_mangled_name) == 0) {
                                            /* Found the method in parent class - use it */
                                            resolved_proc = candidate;
                                            match_count = 1;
                                            
                                            /* Update the procedure call to use the parent method */
                                            free(stmt->stmt_data.procedure_call_data.id);
                                            stmt->stmt_data.procedure_call_data.id = strdup(parent_mangled_name);
                                            proc_id = stmt->stmt_data.procedure_call_data.id;
                                            
                                            break;
                                        }
                                        cur = cur->next;
                                    }
                                }
                                
                                free(parent_mangled_name);
                            }
                            
                            free(parent_method_name);
                        }
                        
                        if (resolved_proc != NULL) {
                            break;  /* Found the method, stop walking up the chain */
                        }
                        
                        /* Move to the next parent class */
                        if (parent_class_name != NULL) {
                            HashNode_t *parent_class_node = NULL;
                            if (FindIdent(&parent_class_node, symtab, parent_class_name) != -1 && parent_class_node != NULL &&
                                parent_class_node->type != NULL && parent_class_node->type->kind == TYPE_KIND_RECORD &&
                                parent_class_node->type->info.record_info != NULL) {
                                record_info = parent_class_node->type->info.record_info;
                                parent_class_name = record_info->parent_class_name;
                            } else {
                                break;  /* Parent class not found, stop the chain */
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
            
            if (class_name != NULL) free(class_name);
            if (method_name != NULL) free(method_name);
        }
    }

    /* If we found multiple matches but they all have the same mangled name,
     * treat it as a single match (they're duplicates from different scopes) */
    if (match_count > 1 && resolved_proc != NULL) {
        /* Verify all matches have the same mangled name */
        int same_mangled = 1;
        ListNode_t *cur = overload_candidates;
        while (cur != NULL && same_mangled) {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate->mangled_id != NULL && strcmp(candidate->mangled_id, mangled_name) == 0) {
                if (strcmp(candidate->mangled_id, resolved_proc->mangled_id) != 0) {
                    same_mangled = 0;
                }
            }
            cur = cur->next;
        }
        if (same_mangled) {
            match_count = 1;  /* Treat as single match */
        }
    }

    if (match_count == 0 && overload_candidates != NULL)
    {
        HashNode_t *wildcard_proc = semcheck_find_untyped_mangled_match(overload_candidates,
            proc_id, mangled_name);
        if (wildcard_proc != NULL)
        {
            resolved_proc = wildcard_proc;
            match_count = 1;
            if (wildcard_proc->mangled_id != NULL)
            {
                free(mangled_name);
                mangled_name = strdup(wildcard_proc->mangled_id);
            }
        }
    }

    /* If no exact mangled match, try forward declarations with matching parameter count */
    if (match_count == 0 && overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while (cur != NULL && match_count == 0)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if ((candidate->hash_type == HASHTYPE_PROCEDURE ||
                 candidate->hash_type == HASHTYPE_FUNCTION) &&
                candidate->id != NULL && pascal_identifier_equals(candidate->id, proc_id))
            {
                /* Check if parameter count matches */
                int candidate_param_count = 0;
                if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    ListNode_t *param = candidate->type->info.proc_info.params;
                    while (param != NULL)
                    {
                        candidate_param_count++;
                        param = param->next;
                    }
                }
                
                ListNode_t *call_args = stmt->stmt_data.procedure_call_data.expr_args;
                int call_arg_count = 0;
                while (call_args != NULL)
                {
                    call_arg_count++;
                    call_args = call_args->next;
                }
                
                if (candidate_param_count == call_arg_count)
                {
                    resolved_proc = candidate;
                    match_count = 1;
                    /* Use the mangled name from the forward declaration */
                    if (candidate->mangled_id != NULL)
                    {
                        free(mangled_name);
                        mangled_name = strdup(candidate->mangled_id);
                    }
                }
            }
            cur = cur->next;
        }
    }

    if (match_count == 1)
    {
        if (resolved_proc->mangled_id != NULL)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_proc->mangled_id);
        else
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL &&
            (resolved_proc->hash_type == HASHTYPE_PROCEDURE ||
             resolved_proc->hash_type == HASHTYPE_FUNCTION) &&
            resolved_proc->id != NULL)
        {
            /* Ensure direct calls have a concrete target name even without external alias */
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_proc->id);
        }
        /* External name override for procedures handled during codegen to avoid side-effects here */
        stmt->stmt_data.procedure_call_data.resolved_proc = resolved_proc;
        
        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = resolved_proc->hash_type;
        semcheck_stmt_set_call_kgpc_type(stmt, resolved_proc->type,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        semcheck_mark_call_requires_static_link(resolved_proc);
        
        sym_return = resolved_proc;
        scope_return = 0; // FIXME: This needs to be properly calculated
    }
    else if (match_count == 0)
    {
        HashNode_t *proc_var = NULL;
        int proc_scope = FindIdent(&proc_var, symtab, proc_id);
        if (proc_scope != -1 && proc_var != NULL && proc_var->hash_type == HASHTYPE_VAR &&
            proc_var->type != NULL && proc_var->type->kind == TYPE_KIND_PROCEDURE)
        {
            DestroyList(overload_candidates);
            free(mangled_name);

            proc_var->referenced += 1;
            if (proc_scope > max_scope_lev)
            {
                fprintf(stderr, "Error on line %d, %s cannot be called in the current context!\n\n",
                    stmt->line_num, proc_id);
                return_val++;
                return return_val;
            }

            /* Set the resolved_proc field so codegen knows this is an indirect call */
            stmt->stmt_data.procedure_call_data.resolved_proc = proc_var;
            
            /* Populate call info to avoid use-after-free when HashNode is freed */
            stmt->stmt_data.procedure_call_data.call_hash_type = proc_var->hash_type;
            semcheck_stmt_set_call_kgpc_type(stmt, proc_var->type,
                stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

            return return_val + semcheck_call_with_proc_var(symtab, stmt, proc_var, max_scope_lev);
        }

        fprintf(stderr, "Error on line %d, call to procedure %s does not match any available overload\n", stmt->line_num, proc_id);
        DestroyList(overload_candidates);
        free(mangled_name);
        return ++return_val;
    }
    else
    {
        fprintf(stderr, "Error on line %d, call to procedure %s is ambiguous\n", stmt->line_num, proc_id);
        DestroyList(overload_candidates);
        free(mangled_name);
        return ++return_val;
    }
    DestroyList(overload_candidates);
    free(mangled_name);

    if(scope_return == -1) // Should not happen if match_count > 0
    {
        fprintf(stderr, "Error on line %d, unrecognized procedure call %s\n", stmt->line_num,
            proc_id);
        ++return_val;
    }
    else
    {
        sym_return->referenced += 1; /* Moved here: only access if sym_return is valid */

        if(scope_return > max_scope_lev)
        {
            fprintf(stderr, "Error on line %d, %s cannot be called in the current context!\n\n",
                stmt->line_num, proc_id);
            fprintf(stderr, "[Was it defined above the current function context?]\n");

            ++return_val;
        }
        if(sym_return->hash_type != HASHTYPE_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_BUILTIN_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_FUNCTION)
        {
            fprintf(stderr, "Error on line %d, expected %s to be a procedure, function, or builtin!\n\n",
                stmt->line_num, proc_id);

            ++return_val;
        }

        /***** VERIFY ARGUMENTS USING KGPCTYPE ARCHITECTURE *****/
        cur_arg = 0;
        /* Get formal arguments from KgpcType instead of deprecated args field */
        true_args = kgpc_type_get_procedure_params(sym_return->type);
        while(args_given != NULL && true_args != NULL)
        {
            ++cur_arg;
            assert(args_given->type == LIST_EXPR);
            assert(true_args->type == LIST_TREE);
            
            arg_decl = (Tree_t *)true_args->cur;
            assert(arg_decl->type == TREE_VAR_DECL || arg_decl->type == TREE_ARR_DECL);
            true_arg_ids = (arg_decl->type == TREE_VAR_DECL) ? 
                arg_decl->tree_data.var_decl_data.ids : 
                arg_decl->tree_data.arr_decl_data.ids;

            while(true_arg_ids != NULL && args_given != NULL)
            {
                struct Expression *arg_expr = (struct Expression *)args_given->cur;

                if (arg_decl != NULL && arg_decl->type == TREE_ARR_DECL)
                {
                    if (semcheck_prepare_array_literal_argument(arg_decl, arg_expr,
                            symtab, INT_MAX, stmt->line_num) != 0)
                    {
                        ++return_val;
                        args_given = args_given->next;
                        true_arg_ids = true_arg_ids->next;
                        continue;
                    }
                }
                
                /* ALWAYS resolve both sides to KgpcType for proper type checking */
                int expected_type_owned = 0;
                KgpcType *expected_kgpc_type = resolve_type_from_vardecl(arg_decl, symtab, &expected_type_owned);
                
                int arg_type_owned = 0;
                KgpcType *arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);
                int param_is_untyped = semcheck_var_decl_is_untyped(arg_decl);

                /* Perform type compatibility check using KgpcType */
                int types_match = param_is_untyped ? 1 : 0;
                if ((expected_kgpc_type == NULL || arg_kgpc_type == NULL) && !param_is_untyped)
                {
                    /* Fallback: if we can't get KgpcTypes, report error */
                    fprintf(stderr, "Error on line %d, on procedure call %s, argument %d: Unable to resolve types (expected=%p, arg=%p)!\n\n",
                        stmt->line_num, proc_id, cur_arg, (void*)expected_kgpc_type, (void*)arg_kgpc_type);
                    if (expected_kgpc_type != NULL)
                        fprintf(stderr, "  Expected type: %s\n", kgpc_type_to_string(expected_kgpc_type));
                    if (arg_kgpc_type != NULL)
                        fprintf(stderr, "  Argument type: %s\n", kgpc_type_to_string(arg_kgpc_type));
                    ++return_val;
                }
                else if (!param_is_untyped)
                {
                    types_match = are_types_compatible_for_assignment(expected_kgpc_type, arg_kgpc_type, symtab);
                    
                    /* Special AST transformation for procedure parameters */
                    if (types_match && 
                        expected_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_expr != NULL && arg_expr->type == EXPR_VAR_ID)
                    {
                        HashNode_t *arg_node = NULL;
                        if (FindIdent(&arg_node, symtab, arg_expr->expr_data.id) != -1 &&
                            arg_node != NULL && arg_node->hash_type == HASHTYPE_PROCEDURE)
                        {
                            /* Transform the expression to EXPR_ADDR_OF_PROC */
                            arg_expr->type = EXPR_ADDR_OF_PROC;
                            arg_expr->expr_data.addr_of_proc_data.procedure_symbol = arg_node;
                        }
                    }
                }

                /* Clean up owned types */
                if (expected_type_owned && expected_kgpc_type != NULL)
                    destroy_kgpc_type(expected_kgpc_type);
                if (arg_type_owned && arg_kgpc_type != NULL)
                    destroy_kgpc_type(arg_kgpc_type);

                if (!types_match)
                {
                    fprintf(stderr, "Error on line %d, on procedure call %s, argument %d: Type mismatch!\n\n",
                        stmt->line_num, proc_id, cur_arg);
                    ++return_val;
                }

                args_given = args_given->next;
                true_arg_ids = true_arg_ids->next;
            }

            true_args = true_args->next;
        }

        /* Verify arg counts match up */
        if(true_args == NULL && args_given != NULL)
        {
            fprintf(stderr, "Error on line %d, on procedure call %s, too many arguments given!\n\n",
                stmt->line_num, proc_id);
            ++return_val;
        }
        else if(true_args != NULL && args_given == NULL)
        {
            fprintf(stderr, "Error on line %d, on procedure call %s, not enough arguments given!\n\n",
                stmt->line_num, proc_id);
            ++return_val;
        }
    }

    return return_val;
}

/** COMPOUNT_STMT **/
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    ListNode_t *stmt_list;
    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_COMPOUND_STATEMENT);

    return_val = 0;
    stmt_list = stmt->stmt_data.compound_statement;
    while (stmt_list != NULL)
    {
        assert(stmt_list->type == LIST_STMT);

        if (stmt_list->cur != NULL)
        {
            return_val += semcheck_stmt_main(symtab,
                (struct Statement *)stmt_list->cur, max_scope_lev);
        }

        stmt_list = stmt_list->next;
    }

    if (g_debug_watch_stmt != NULL) {
        if (g_debug_watch_stmt->stmt_data.for_data.to != g_debug_watch_to_expr) {
            fprintf(stderr, "CRITICAL: g_debug_watch_stmt corrupted at end of compoundstmt! Changed from %p to %p\n",
                    g_debug_watch_to_expr, g_debug_watch_stmt->stmt_data.for_data.to);
        } else {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: g_debug_watch_stmt OK at end of compoundstmt. to=%p\n", g_debug_watch_stmt->stmt_data.for_data.to);
#endif
        }
    }

    return return_val;
}

/** IF_THEN **/
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int if_type;
    struct Expression *relop_expr;
    struct Statement *if_stmt, *else_stmt;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_IF_THEN);

    return_val = 0;
    relop_expr = stmt->stmt_data.if_then_data.relop_expr;
    if_stmt = stmt->stmt_data.if_then_data.if_stmt;
    else_stmt = stmt->stmt_data.if_then_data.else_stmt;

    return_val += semcheck_expr_main(&if_type, symtab, relop_expr, INT_MAX, NO_MUTATE);

    if(if_type != BOOL)
    {
        fprintf(stderr, "Error on line %d, expected relational inside if statement!\n\n",
                stmt->line_num);
        ++return_val;
    }

    return_val += semcheck_stmt_main(symtab, if_stmt, max_scope_lev);
    if(else_stmt != NULL)
        return_val += semcheck_stmt_main(symtab, else_stmt, max_scope_lev);

    return return_val;
}

/** WHILE **/
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int while_type;
    struct Expression *relop_expr;
    struct Statement *while_stmt;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_WHILE);

    return_val = 0;
    relop_expr = stmt->stmt_data.while_data.relop_expr;
    while_stmt = stmt->stmt_data.while_data.while_stmt;

    return_val += semcheck_expr_main(&while_type, symtab, relop_expr, INT_MAX, NO_MUTATE);
    if(while_type != BOOL)
    {
        fprintf(stderr, "Error on line %d, expected relational inside while statement!\n\n",
                stmt->line_num);
        ++return_val;
    }

    semcheck_loop_depth++;
    return_val += semcheck_stmt_main(symtab, while_stmt, max_scope_lev);
    semcheck_loop_depth--;

    return return_val;
}

/** REPEAT **/
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    int until_type = UNKNOWN_TYPE;
    ListNode_t *body_list;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_REPEAT);

    body_list = stmt->stmt_data.repeat_data.body_list;
    semcheck_loop_depth++;
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        if (body_stmt != NULL)
            return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);
        body_list = body_list->next;
    }
    semcheck_loop_depth--;

    return_val += semcheck_expr_main(&until_type, symtab, stmt->stmt_data.repeat_data.until_expr, INT_MAX, NO_MUTATE);
    if (until_type != BOOL)
    {
        fprintf(stderr, "Error on line %d, expected relational inside repeat statement!\n\n",
            stmt->line_num);
        ++return_val;
    }

    return return_val;
}

/** FOR **/
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;
    int for_type, to_type;
    enum StmtType for_assign_type; /* Either var or var_assign */
    struct Statement *for_assign;
    struct Expression *for_var;

    struct Expression *to_expr;
    struct Statement *do_for;
    int for_type_owned = 0;
    int to_type_owned = 0;
    KgpcType *for_kgpc_type = NULL;
    KgpcType *to_kgpc_type = NULL;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);

    for_assign_type = stmt->stmt_data.for_data.for_assign_type;
    assert(for_assign_type == STMT_FOR_VAR || for_assign_type == STMT_FOR_ASSIGN_VAR);

    return_val = 0;
    for_var = NULL;
    if(for_assign_type == STMT_FOR_VAR)
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
        return_val += semcheck_expr_main(&for_type, symtab, for_var, max_scope_lev, BOTH_MUTATE_REFERENCE);
        /* Check for type */
        if(!is_ordinal_type(for_type))
        {
            fprintf(stderr, "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
                    stmt->line_num);
            ++return_val;
        }
    }
    else
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        /* For type checked in here */
        return_val += semcheck_for_assign(symtab, for_assign, max_scope_lev);
        for_var = NULL;
        if (for_assign != NULL)
        {
            for_var = for_assign->stmt_data.var_assign_data.var;
            for_type = (for_var != NULL) ? for_var->resolved_type : UNKNOWN_TYPE;
        }
    }


    to_expr = stmt->stmt_data.for_data.to;
    do_for = stmt->stmt_data.for_data.do_for;

    if (for_var != NULL)
    {
        for_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, for_var,
            max_scope_lev, BOTH_MUTATE_REFERENCE, &for_type_owned);
        if (for_type == UNKNOWN_TYPE && for_kgpc_type != NULL)
            for_type = kgpc_type_get_legacy_tag(for_kgpc_type);
    }

    return_val += semcheck_expr_main(&to_type, symtab, to_expr, INT_MAX, NO_MUTATE);
    to_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, to_expr,
        INT_MAX, NO_MUTATE, &to_type_owned);
    if (to_type == UNKNOWN_TYPE && to_kgpc_type != NULL)
        to_type = kgpc_type_get_legacy_tag(to_kgpc_type);

    int bounds_compatible = 1;
    if (for_type == UNKNOWN_TYPE && for_kgpc_type == NULL)
        bounds_compatible = 0;

    if (bounds_compatible)
    {
        if (for_type == to_type)
        {
            /* ok */
        }
        else if ((for_type == LONGINT_TYPE && to_type == INT_TYPE) ||
            (for_type == INT_TYPE && to_type == LONGINT_TYPE))
        {
            /* ok */
        }
        else if (for_type == CHAR_TYPE && to_type == STRING_TYPE &&
            to_expr != NULL && to_expr->type == EXPR_STRING &&
            to_expr->expr_data.string != NULL && strlen(to_expr->expr_data.string) == 1)
        {
            to_type = CHAR_TYPE;
            to_expr->resolved_type = CHAR_TYPE;
        }
        else if (for_kgpc_type != NULL && to_kgpc_type != NULL &&
            are_types_compatible_for_assignment(for_kgpc_type, to_kgpc_type, symtab))
        {
            /* ok */
        }
        else
        {
            bounds_compatible = 0;
        }
    }

    if (!bounds_compatible)
    {
        fprintf(stderr, "Error on line %d, type mismatch in \"to\" assignment!\n\n",
                stmt->line_num);
        ++return_val;
    }

    if (for_kgpc_type != NULL && !is_ordinal_type(for_type))
    {
        int legacy = kgpc_type_get_legacy_tag(for_kgpc_type);
        if (!is_ordinal_type(legacy))
        {
            fprintf(stderr, "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
                stmt->line_num);
            ++return_val;
        }
    }

    semcheck_loop_depth++;
    
    if (stmt->line_num == 42) {
        g_debug_watch_stmt = stmt;
        g_debug_watch_to_expr = stmt->stmt_data.for_data.to;
#ifdef DEBUG
        fprintf(stderr, "DEBUG: Watching stmt at line 42\n");
#endif
    }

    if (to_expr != NULL && ((uintptr_t)to_expr == 0x686374616d || (uintptr_t)to_expr == 0x1db2)) {
        fprintf(stderr, "CRITICAL: to_expr is corrupted in semcheck_for!\n");
    }
    
    return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
    semcheck_loop_depth--;

    if (stmt->stmt_data.for_data.to != to_expr) {
        fprintf(stderr, "CRITICAL: stmt->stmt_data.for_data.to changed from %p to %p during body processing!\n",
                to_expr, stmt->stmt_data.for_data.to);
    }

    if (for_type_owned && for_kgpc_type != NULL)
        destroy_kgpc_type(for_kgpc_type);
    if (to_type_owned && to_kgpc_type != NULL)
        destroy_kgpc_type(to_kgpc_type);

    return return_val;
}

/** FOR-IN **/
int semcheck_for_in(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    int loop_var_type, collection_type;
    int loop_var_nonordinal = 0;
    
    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR_IN);
    
    struct Expression *loop_var = stmt->stmt_data.for_in_data.loop_var;
    struct Expression *collection = stmt->stmt_data.for_in_data.collection;
    struct Statement *do_stmt = stmt->stmt_data.for_in_data.do_stmt;
    
    /* Check loop variable (must be a lvalue) */
    if (loop_var != NULL) {
        return_val += semcheck_expr_main(&loop_var_type, symtab, loop_var, max_scope_lev, BOTH_MUTATE_REFERENCE);
        
        if (!is_ordinal_type(loop_var_type) && loop_var_type != UNKNOWN_TYPE) {
            loop_var_nonordinal = 1;
        }
    } else {
        loop_var_type = UNKNOWN_TYPE;
    }
    
    /* Check collection expression */
    if (collection != NULL) {
        int collection_type_owned = 0;
        int collection_is_array = 0;
        int collection_is_list = 0;
        const char *list_element_id = NULL;

        return_val += semcheck_expr_main(&collection_type, symtab, collection, INT_MAX, NO_MUTATE);
        
        KgpcType *collection_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, collection, 
                                                                            INT_MAX, NO_MUTATE, &collection_type_owned);
        if (collection_kgpc_type != NULL) {
            if (kgpc_type_is_array(collection_kgpc_type)) {
                collection_is_array = 1;
            } else if (kgpc_type_is_record(collection_kgpc_type)) {
                struct RecordType *record_info = kgpc_type_get_record(collection_kgpc_type);
                if (record_info != NULL && record_info->type_id != NULL) {
                    const char *prefix = "TFPGList$";
                    size_t prefix_len = strlen(prefix);
                    if (strncasecmp(record_info->type_id, prefix, prefix_len) == 0) {
                        collection_is_list = 1;
                        list_element_id = record_info->type_id + prefix_len;
                    }
                }
            }
        }

        if (!collection_is_array && !collection_is_list) {
            fprintf(stderr, "Error on line %d: for-in loop requires an array expression!\n\n",
                    stmt->line_num);
            ++return_val;
        } else if (!collection_is_list && loop_var_nonordinal) {
            fprintf(stderr, "Error on line %d: for-in loop variable must be an ordinal type!\n\n",
                    stmt->line_num);
            ++return_val;
        }

        if (collection_type_owned && collection_kgpc_type != NULL)
            destroy_kgpc_type(collection_kgpc_type);
        (void)list_element_id;
    } else {
        collection_type = UNKNOWN_TYPE;
    }
    
    /* Check body statement */
    if (do_stmt != NULL) {
        return_val += semcheck_stmt(symtab, do_stmt, max_scope_lev);
    }
    
    return return_val;
}

/* Essentially the same as the var assignment but with a restriction that it must be an int */
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev)
{
    int return_val;
    int type_first, type_second;
    struct Expression *var, *expr;
    int lhs_owned = 0;
    int rhs_owned = 0;
    KgpcType *lhs_kgpc_type = NULL;
    KgpcType *rhs_kgpc_type = NULL;

    assert(symtab != NULL);
    assert(for_assign != NULL);
    assert(for_assign->type == STMT_VAR_ASSIGN);

    return_val = 0;

    var = for_assign->stmt_data.var_assign_data.var;
    expr = for_assign->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    return_val += semcheck_expr_main(&type_first, symtab, var, max_scope_lev, BOTH_MUTATE_REFERENCE);
    return_val += semcheck_expr_main(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

    lhs_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, var, max_scope_lev,
        BOTH_MUTATE_REFERENCE, &lhs_owned);
    rhs_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, expr, INT_MAX,
        NO_MUTATE, &rhs_owned);

    if (type_first == UNKNOWN_TYPE && lhs_kgpc_type != NULL)
        type_first = kgpc_type_get_legacy_tag(lhs_kgpc_type);
    if (type_second == UNKNOWN_TYPE && rhs_kgpc_type != NULL)
        type_second = kgpc_type_get_legacy_tag(rhs_kgpc_type);

    int types_compatible = (type_first == type_second);
    if (!types_compatible)
    {
        if ((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
            (type_first == INT_TYPE && type_second == LONGINT_TYPE))
        {
            types_compatible = 1;
        }
        else if (type_first == CHAR_TYPE && type_second == STRING_TYPE &&
            expr != NULL && expr->type == EXPR_STRING &&
            expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1)
        {
            types_compatible = 1;
            type_second = CHAR_TYPE;
            expr->resolved_type = CHAR_TYPE;
        }
        else if (lhs_kgpc_type != NULL && rhs_kgpc_type != NULL &&
            are_types_compatible_for_assignment(lhs_kgpc_type, rhs_kgpc_type, symtab))
        {
            types_compatible = 1;
        }
    }

    if (!types_compatible)
    {
        fprintf(stderr, "Error on line %d, type mismatch in \"for\" assignment statement!\n\n",
            for_assign->line_num);
        ++return_val;
    }

    if (!is_ordinal_type(type_first))
    {
        fprintf(stderr, "Error on line %d, expected ordinal type in \"for\" assignment statement!\n\n",
            for_assign->line_num);
        ++return_val;
    }

    if (return_val == 0)
    {
        var->resolved_type = type_first;
        expr->resolved_type = type_second;
    }

    if (lhs_owned && lhs_kgpc_type != NULL)
        destroy_kgpc_type(lhs_kgpc_type);
    if (rhs_owned && rhs_kgpc_type != NULL)
        destroy_kgpc_type(rhs_kgpc_type);

    return return_val;
}
