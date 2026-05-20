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
#include <ctype.h>
#include <stdint.h>
#include <time.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strncasecmp _strnicmp
#endif
#include "../../../common_utils.h"
#include "SemCheck_stmt.h"
#include "SemCheck_expr.h"
#include "SemCheck_overload.h"
#include "../SemCheck.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../../unit_registry.h"
#include "SemCheck_sizeof.h"
/* WithContextEntry is defined in SemCheck_Expr_Internal.h.  We can't include
 * that header here because of redefinition conflicts with helpers defined
 * statically in SemCheck_stmt.c.  Instead we forward-declare the with stack here. */
struct WithContextEntry_fwd {
    struct Expression *context_expr;
    struct RecordType *record_type;
};
extern struct WithContextEntry_fwd *with_context_stack;

void semcheck_debug_expr_brief(const struct Expression *expr, const char *label);
struct RecordType *get_record_type_from_node(HashNode_t *node);
#include "../../ParseTree/generic_types.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/from_cparser.h"

struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);

#include "../../ParseTree/tree_types.h"
#include "../../ParseTree/ident_ref.h"
#include "../../ParseTree/type_tags.h"
#include "../../List/List.h"

HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id);

/* Forward declaration from SemCheck_Expr_Resolve.c */
const char *semcheck_type_tag_name(int type_tag);
HashNode_t *semcheck_find_type_node_in_owner_chain(SymTab_t *symtab,
    const char *type_id, const char *owner_full, const char *owner_outer);
const char *semcheck_get_current_subprogram_owner_class_full(void);
const char *semcheck_get_current_subprogram_owner_class_outer(void);
int semcheck_typecheck_array_literal(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, int expected_type, const char *expected_type_id, int line_num);
int set_type_from_hashtype(int *type, HashNode_t *hash_node);
int semcheck_convert_set_literal_to_array_literal(struct Expression *expr);
int semcheck_try_reinterpret_as_typecast(int *type_return,
    SymTab_t *symtab, struct Expression *expr, int max_scope_lev);
void semcheck_reset_function_call_cache(struct Expression *expr);
int semcheck_expr_is_char_like(struct Expression *expr);
int semcheck_class_type_ids_compatible(SymTab_t *symtab,
    const char *formal_id, const char *actual_id);

#define SEMSTMT_TIMINGS_ENABLED() (kgpc_getenv("KGPC_DEBUG_SEMSTMT_TIMINGS") != NULL)

#include "SemCheck_stmt_internal.h"
#include "../../../identifier_utils.h"

/** PROCEDURE_CALL **/
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val, scope_return, cur_arg;
    HashNode_t *sym_return;
    ListNode_t *true_args, *true_arg_ids, *args_given;
    Tree_t *arg_decl;
    char *proc_id;
    char *mangled_name;
    int static_arg_already_removed = 0;
    int static_method_receiver = 0;
    int was_unit_qualified = 0;

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);

    return_val = 0;

    proc_id = stmt->stmt_data.procedure_call_data.id;
    args_given = stmt->stmt_data.procedure_call_data.expr_args;

    if (args_given != NULL)
    {
        const char *cur_sub_id = semcheck_get_current_subprogram_id();
        const char *result_var = semcheck_get_current_subprogram_result_var_name();
        const char *method_name = semcheck_get_current_subprogram_method_name();
        const char *replacement = (result_var != NULL && result_var[0] != '\0')
            ? result_var : "Result";
        for (ListNode_t *arg_cur = args_given; arg_cur != NULL; arg_cur = arg_cur->next)
        {
            if (arg_cur->type != LIST_EXPR || arg_cur->cur == NULL)
                continue;
            struct Expression *arg_expr = (struct Expression *)arg_cur->cur;
            if (arg_expr->type != EXPR_VAR_ID || arg_expr->expr_data.id == NULL)
                continue;
            const char *arg_id = arg_expr->expr_data.id;
            int is_result_name =
                (cur_sub_id != NULL && pascal_identifier_equals(arg_id, cur_sub_id)) ||
                (result_var != NULL && pascal_identifier_equals(arg_id, result_var)) ||
                (method_name != NULL && pascal_identifier_equals(arg_id, method_name));
            if (!is_result_name)
                continue;

            if (!pascal_identifier_equals(arg_id, replacement))
            {
                /* Don't rename if the original name is a local variable or
                 * parameter (it takes precedence over the result variable). */
                HashNode_t *orig_check = NULL;
                if (symtab != NULL && symtab->current_scope != NULL &&
                    symtab->current_scope->table != NULL)
                {
                    orig_check = FindIdentInTable(symtab->current_scope->table, arg_id);
                    if (orig_check != NULL &&
                        (orig_check->hash_type == HASHTYPE_VAR ||
                         orig_check->hash_type == HASHTYPE_ARRAY))
                        continue;
                }
                /* Don't rename if a user-declared local variable with the
                 * replacement name would shadow the function return slot. */
                HashNode_t *local_check = NULL;
                if (symtab != NULL && symtab->current_scope != NULL &&
                    symtab->current_scope->table != NULL)
                {
                    local_check = FindIdentInTable(symtab->current_scope->table, replacement);
                    if (local_check != NULL &&
                        local_check->hash_type == HASHTYPE_FUNCTION_RETURN)
                        local_check = NULL;
                }
                if (local_check != NULL)
                    continue;
                char *dup = strdup(replacement);
                if (dup == NULL)
                    return 1;
                free(arg_expr->expr_data.id);
                arg_expr->expr_data.id = dup;
            }
        }
    }

    /* If this is a method call placeholder with a type identifier receiver,
     * resolve it to the class method immediately to avoid type-helper detours. */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL && with_context_count > 0)
        {
            struct Expression *with_expr = NULL;
            int with_status = semcheck_with_try_resolve(first_arg->expr_data.id,
                symtab, &with_expr, stmt->line_num);
            if (with_status == 0 && with_expr != NULL)
            {
                char *field_id = first_arg->expr_data.id;
                memset(&first_arg->expr_data, 0, sizeof(first_arg->expr_data));
                first_arg->type = EXPR_RECORD_ACCESS;
                first_arg->expr_data.record_access_data.record_expr = with_expr;
                first_arg->expr_data.record_access_data.field_id = field_id;
                first_arg->expr_data.record_access_data.field_offset = 0;
                first_arg->record_type = NULL;
                first_arg->array_element_record_type = NULL;
                first_arg->array_element_type = UNKNOWN_TYPE;
                first_arg->array_element_type_id = NULL;
                first_arg->pointer_subtype = UNKNOWN_TYPE;
                semcheck_expr_set_resolved_type(first_arg, UNKNOWN_TYPE);
            }
            else if (with_expr != NULL)
            {
                destroy_expr(with_expr);
            }
        }
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            /* Keep unit qualifiers out of the type-receiver fast path.
             * Calls like System.Error(...) should be handled by the unit-qualified
             * rewrite below, not rewritten as TypeName__MethodName. */
            if (semcheck_is_unit_name(first_arg->expr_data.id))
                goto skip_type_receiver_rewrite;

            HashNode_t *type_node = NULL;
            int type_found = (FindSymbol(&type_node, symtab, first_arg->expr_data.id) != 0 &&
                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE);

            /* Handle specialized generic type receiver: specialize T<A>.Method(...)
             * The parser produces receiver="T$A" which may not be in the symbol
             * table directly, but the base type "T" is.  Fall back to the base name. */
            if (!type_found)
            {
                const char *dollar = strchr(first_arg->expr_data.id, '$');
                if (dollar != NULL && dollar > first_arg->expr_data.id)
                {
                    char *gen_base = strndup(first_arg->expr_data.id, (size_t)(dollar - first_arg->expr_data.id));
                    if (gen_base != NULL)
                    {
                        type_node = NULL;
                        if (FindSymbol(&type_node, symtab, gen_base) != 0 &&
                            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                        {
                            type_found = 1;
                        }
                        free(gen_base);
                    }
                }
            }

            if (type_found)
            {
                struct RecordType *record_info = semcheck_stmt_get_record_type_from_node(type_node);
                if (record_info != NULL && record_info->type_id != NULL &&
                    stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                {
                    const char *method_name = stmt->stmt_data.procedure_call_data.placeholder_method_name;

                    /* Check if method_name is actually a field of procedure type on the class.
                     * E.g., tmodule.finish_module(hp) where finish_module is a class var
                     * of procedural type, not a method. Since the receiver is a type name,
                     * only class vars (or static fields) are valid here. */
                    {
                        int is_classvar_proc = 0;
                        struct RecordType *walk_rec = record_info;
                        while (walk_rec != NULL && !is_classvar_proc)
                        {
                            for (ListNode_t *f = walk_rec->fields; f != NULL; f = f->next)
                            {
                                if (f->type != LIST_RECORD_FIELD || f->cur == NULL)
                                    continue;
                                struct RecordField *rf = (struct RecordField *)f->cur;
                                if (rf->name == NULL)
                                    continue;
                                if (strcasecmp(rf->name, method_name) != 0)
                                    continue;
                                /* Found field — check if it has procedure type */
                                if (rf->proc_type != NULL)
                                {
                                    is_classvar_proc = 1;
                                    break;
                                }
                                if (rf->type_id != NULL)
                                {
                                    HashNode_t *ft_node = NULL;
                                    if (FindSymbol(&ft_node, symtab, rf->type_id) != 0 &&
                                        ft_node != NULL && ft_node->type != NULL &&
                                        ft_node->type->kind == TYPE_KIND_PROCEDURE)
                                    {
                                        is_classvar_proc = 1;
                                        break;
                                    }
                                }
                            }
                            /* Walk parent class hierarchy */
                            const char *parent = walk_rec->parent_class_name;
                            walk_rec = (parent != NULL) ? semcheck_lookup_record_type(symtab, parent) : NULL;
                        }
                        if (is_classvar_proc)
                        {
                            /* Convert to a procedural variable call through the class var.
                             * Build TypeName.field_name as a class-field access expression. */
                            struct Expression *type_expr = mk_varid(stmt->line_num,
                                strdup(first_arg->expr_data.id));
                            struct Expression *field_access = mk_recordaccess(stmt->line_num,
                                type_expr, strdup(method_name));

                            /* Remove the type receiver from the argument list */
                            ListNode_t *remaining_args = args_given->next;
                            destroy_expr(first_arg);
                            args_given->cur = NULL;
                            free(args_given);
                            stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

                            stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                            stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                            stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;

                            int field_tag = UNKNOWN_TYPE;
                            return_val += semcheck_stmt_expr_tag(&field_tag, symtab, field_access,
                                max_scope_lev, NO_MUTATE);

                            return return_val;
                        }
                    }

                    int is_static_method = from_cparser_is_method_static(record_info->type_id,
                        method_name);
                    int is_nonstatic_class_method =
                        (!is_static_method &&
                         from_cparser_is_method_class_method(record_info->type_id,
                             method_name));
                    size_t class_len = strlen(record_info->type_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL)
                    {
                        sprintf(new_proc_id, "%s__%s", record_info->type_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        if (is_nonstatic_class_method)
                        {
                            stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                            if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                            {
                                stmt->stmt_data.procedure_call_data.self_class_name =
                                    strdup(record_info->type_id);
                            }
                        }
                        else
                        {
                            /* Check if this is a constructor call (e.g., TMyItem.Create(...)).
                             * If so, mark it for the codegen so it allocates a new instance. */
                            int is_ctor = 0;
                            if (strncasecmp(method_name, "Create", 6) == 0)
                            {
                                struct MethodTemplate *tmpl =
                                    from_cparser_get_method_template(record_info, method_name);
                                if (tmpl != NULL && tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR)
                                    is_ctor = 1;
                            }
                            if (is_ctor)
                            {
                                stmt->stmt_data.procedure_call_data.is_constructor_call = 1;
                                stmt->stmt_data.procedure_call_data.constructor_class_name =
                                    strdup(record_info->type_id);
                            }
                            ListNode_t *remaining_args = args_given->next;
                            destroy_expr(first_arg);
                            args_given->cur = NULL;
                            free(args_given);
                            stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                            args_given = remaining_args;
                            static_arg_already_removed = 1;
                        }
                    }
                }
            }
        }
    }
skip_type_receiver_rewrite:

    /* FPC Bootstrap Feature: Handle unit-qualified procedure calls.
     * When the parser sees Unit.Procedure(args), it creates a procedure call with id "__Procedure"
     * and passes Unit as the first argument (as if it were a method call).
     * We need to detect this pattern and transform it back to a direct procedure call.
     *
     * Pattern: proc_id starts with "__", first arg is a VAR_ID that names a known unit
     * (preferred) or is unresolved in the symbol table (secondary heuristic), and the procedure
     * name (without "__" prefix) exists in symbol table.
     */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder && args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            char *potential_unit_name = first_arg->expr_data.id;
            HashNode_t *unit_check = NULL;
            int is_unit_qualifier = semcheck_is_unit_name(potential_unit_name);

            /* Local variables/parameters shadow unit names in Pascal.
             * If the identifier resolves as a variable, don't treat it
             * as a unit qualifier (e.g., 'node' parameter vs 'node' unit). */
            if (is_unit_qualifier)
            {
                HashNode_t *var_check = NULL;
                if (FindSymbol(&var_check, symtab, potential_unit_name) != 0 &&
                    var_check != NULL &&
                    (var_check->hash_type == HASHTYPE_VAR ||
                     var_check->hash_type == HASHTYPE_CONST))
                {
                    is_unit_qualifier = 0;
                }
            }

            /* Prefer explicit unit-name recognition; keep unresolved-name secondary path for
             * parser shapes where unit qualifiers are not injected into symbol tables. */
            if (!is_unit_qualifier &&
                FindSymbol(&unit_check, symtab, potential_unit_name) == 0)
            {
                int looks_like_self_member = 0;
                HashNode_t *self_node = NULL;
                if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
                {
                    struct RecordType *self_record = semcheck_stmt_get_record_type_from_node(self_node);
                    if (self_record != NULL &&
                        semcheck_find_class_field_including_hidden(symtab, self_record,
                            potential_unit_name, NULL) != NULL)
                    {
                        looks_like_self_member = 1;
                    }
                    if (!looks_like_self_member &&
                        semcheck_find_class_property(symtab, self_record,
                            potential_unit_name, NULL) != NULL)
                    {
                        looks_like_self_member = 1;
                    }
                    if (!looks_like_self_member &&
                        semcheck_find_class_method(symtab, self_record,
                            potential_unit_name, NULL) != NULL)
                    {
                        looks_like_self_member = 1;
                    }
                }

                /* Before treating as a unit qualifier, check if the identifier
                 * resolves via active WITH contexts.  This prevents
                 * 'with s do Data.Reset' from being misidentified as a
                 * unit-qualified call when Data is a WITH-scoped property. */
                if (!looks_like_self_member && with_context_count > 0)
                {
                    struct Expression *with_check = NULL;
                    int ws = semcheck_with_try_resolve(potential_unit_name,
                        symtab, &with_check, stmt->line_num);
                    if (ws == 0 && with_check != NULL)
                    {
                        looks_like_self_member = 1; /* not a unit; it's a WITH field */
                        destroy_expr(with_check);
                    }
                }
                if (!looks_like_self_member)
                    is_unit_qualifier = 1;
            }

            if (is_unit_qualifier)
            {
                /* Unit-qualified call; resolve using the structured method name. */
                char *real_proc_name = NULL;
                char *unit_qualifier_copy = strdup(potential_unit_name);
                if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                {
                    real_proc_name =
                        strdup(stmt->stmt_data.procedure_call_data.placeholder_method_name);
                }
                else if (proc_id != NULL &&
                    proc_id[0] == '_' &&
                    proc_id[1] == '_' &&
                    proc_id[2] != '\0')
                {
                    real_proc_name = strdup(proc_id + 2);
                }
                if (real_proc_name == NULL)
                {
                    free(unit_qualifier_copy);
                    /* strdup failed - skip transformation, will report error later */
                }
                else
                {
                    if (pascal_identifier_equals(potential_unit_name, "System") &&
                        pascal_identifier_equals(real_proc_name, "Error"))
                    {
                        ListNode_t *remaining_args = args_given->next;
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);

                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                        free(proc_id);
                        proc_id = strdup("Halt");
                        if (proc_id == NULL)
                        {
                            free(real_proc_name);
                            return 1;
                        }
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        if (stmt->stmt_data.procedure_call_data.call_qualifier != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.call_qualifier);
                            stmt->stmt_data.procedure_call_data.call_qualifier = NULL;
                        }
                        stmt->stmt_data.procedure_call_data.call_qualifier = unit_qualifier_copy;
                        unit_qualifier_copy = NULL;
                        if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.placeholder_method_name);
                            stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
                        }
                        args_given = remaining_args;
                        was_unit_qualified = 1;
                        free(real_proc_name);
                    }
                    else
                    {
                    int force_strip_system_qualifier =
                        pascal_identifier_equals(potential_unit_name, "System");
                    ListNode_t *proc_candidates = FindAllIdents(symtab, real_proc_name);

                    if (proc_candidates != NULL || force_strip_system_qualifier)
                    {
                        /* Found the procedure by name. Transform the call:
                         * 1. Remove the first argument (the unit qualifier)
                         * 2. Change proc_id to the real procedure name (without "__")
                         *
                         * System-qualified builtins are not always present as ordinary
                         * symbol-table procedures, but semcheck still needs the
                         * placeholder receiver stripped so later builtin resolution can
                         * match names like Error, Halt, and Exit.
                         */
                        /* Save the remaining args before modifying the list */
                        ListNode_t *remaining_args = args_given->next;

                        /* Free the unit qualifier expression and list node.
                         * Note: remaining_args holds the saved pointer value, so
                         * freeing args_given doesn't affect it. */
                        destroy_expr(first_arg);
                        args_given->cur = NULL;
                        free(args_given);

                        /* Update the statement with the transformed call */
                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

                        /* Update proc_id - we already have real_proc_name allocated */
                        free(proc_id);
                        proc_id = real_proc_name;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        if (stmt->stmt_data.procedure_call_data.call_qualifier != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.call_qualifier);
                            stmt->stmt_data.procedure_call_data.call_qualifier = NULL;
                        }
                        stmt->stmt_data.procedure_call_data.call_qualifier = unit_qualifier_copy;
                        unit_qualifier_copy = NULL;
                        if (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                        {
                            free(stmt->stmt_data.procedure_call_data.placeholder_method_name);
                            stmt->stmt_data.procedure_call_data.placeholder_method_name = NULL;
                        }
                        args_given = remaining_args;
                        was_unit_qualified = 1;

                        if (proc_candidates != NULL)
                            DestroyList(proc_candidates);

                        /* Continue with normal procedure call handling using the transformed call */
                    }
                    else
                    {
                        /* Procedure not found - allow System.Exit without a symbol table entry. */
                        if (pascal_identifier_equals(real_proc_name, "Exit"))
                        {
                            ListNode_t *remaining_args = args_given->next;

                            destroy_expr(first_arg);
                            args_given->cur = NULL;
                            free(args_given);

                            stmt->stmt_data.procedure_call_data.expr_args = remaining_args;

                            free(proc_id);
                            proc_id = real_proc_name;
                            stmt->stmt_data.procedure_call_data.id = proc_id;
                            stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                            if (stmt->stmt_data.procedure_call_data.call_qualifier != NULL)
                            {
                                free(stmt->stmt_data.procedure_call_data.call_qualifier);
                                stmt->stmt_data.procedure_call_data.call_qualifier = NULL;
                            }
                            stmt->stmt_data.procedure_call_data.call_qualifier = unit_qualifier_copy;
                            unit_qualifier_copy = NULL;
                            args_given = remaining_args;
                            was_unit_qualified = 1;
                        }
                        else
                        {
                            /* Procedure not found - free real_proc_name and fall through to report error */
                            free(real_proc_name);
                        }
                    }
                    free(unit_qualifier_copy);
                    }
                }
            }
        }
    }

    /* Treat System.Exit (or unqualified Exit) as a built-in procedure call.
     * This avoids resolving Exit against class methods like TMonitor.Exit. */
    if (proc_id != NULL &&
        pascal_identifier_equals(proc_id, "Exit") &&
        !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        int arg_count = 0;
        for (ListNode_t *scan = args_given; scan != NULL; scan = scan->next)
            ++arg_count;

        if (arg_count > 1)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, Exit() expects at most one argument.\n\n",
                stmt->line_num);
            return 1;
        }

        struct Expression *exit_expr = NULL;
        if (arg_count == 1)
        {
            int expr_type = UNKNOWN_TYPE;
            exit_expr = (struct Expression *)args_given->cur;
            if (exit_expr != NULL)
                return_val += semcheck_stmt_expr_tag(&expr_type, symtab, exit_expr, max_scope_lev, 0);
        }

        /* Transform the procedure call into an Exit statement for codegen. */
        stmt->type = STMT_EXIT;
        stmt->stmt_data.exit_data.return_expr = exit_expr;
        stmt->stmt_data.procedure_call_data.expr_args = NULL;
        if (stmt->stmt_data.procedure_call_data.id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.id);
            stmt->stmt_data.procedure_call_data.id = NULL;
        }
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        while (args_given != NULL)
        {
            ListNode_t *next = args_given->next;
            args_given->cur = NULL;
            free(args_given);
            args_given = next;
        }

        return return_val;
    }

    /* INTERNPROC: Transform TypedFile Rewrite/Reset calls by injecting element size.
     * Must happen before overload resolution so the 2-arg variant is selected. */
    if (semcheck_internproc_typedfile_rewrite_reset(symtab, stmt))
        args_given = stmt->stmt_data.procedure_call_data.expr_args;

    /* If no explicit receiver was provided, but Self is in scope and defines this method,
     * prepend Self so unqualified method calls resolve correctly. */
    if (!was_unit_qualified && proc_id != NULL &&
        !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        HashNode_t *self_node = NULL;
        struct RecordType *self_record = NULL;
        if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL &&
            pascal_identifier_equals(proc_id, "Assign"))
        {
            HashNode_t *dbg_self = NULL;
            int dbg_found = FindSymbol(&dbg_self, symtab, "Self");
            HashNode_t *dbg_proc = NULL;
            FindSymbol(&dbg_proc, symtab, "Assign");
            const char *dbg_owner = semcheck_get_current_method_owner();
            fprintf(stderr, "[ASSIGN-TRACE] Self_found=%d owner=%s scope_kind=%d proc_owner=%s proc_hash=%d\n",
                dbg_found != 0, dbg_owner ? dbg_owner : "<null>",
                symtab->current_scope ? symtab->current_scope->num_deps : -1,
                (dbg_proc && dbg_proc->owner_class) ? dbg_proc->owner_class : "<null>",
                dbg_proc ? dbg_proc->hash_type : -1);
        }
        if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL)
        {
            self_record = semcheck_stmt_get_record_type_from_node(self_node);
            if (self_record == NULL)
            {
                int self_type_tag = UNKNOWN_TYPE;
                const char *self_type_name = NULL;
                set_type_from_hashtype(&self_type_tag, self_node);
                if (self_node->type != NULL &&
                    self_node->type->type_alias != NULL &&
                    self_node->type->type_alias->target_type_id != NULL)
                {
                    self_type_name = self_node->type->type_alias->target_type_id;
                }

                struct RecordType *helper_record = semcheck_lookup_type_helper(
                    symtab, self_type_tag, self_type_name);
                if (helper_record != NULL)
                    self_record = helper_record;
            }
        }

        if (self_record == NULL)
        {
            const char *current_owner = semcheck_get_current_method_owner();
            if (current_owner != NULL)
                self_record = semcheck_lookup_record_type(symtab, current_owner);
        }

        if (self_record != NULL)
        {
            const char *cur_owner = semcheck_get_current_method_owner();
            struct RecordType *owner_record = NULL;
            if (cur_owner != NULL)
                owner_record = semcheck_lookup_record_type(symtab, cur_owner);

            struct RecordType *lookup_record = (owner_record != NULL) ? owner_record : self_record;
            HashNode_t *method_node = semcheck_find_class_method(symtab, lookup_record, proc_id, NULL);
            /* If not found and self_record->type_id differs from the current
             * method owner (e.g. record has "timezone" but owner is "TTimeZone"),
             * retry with the owner's record. */
            if (method_node == NULL && self_record->type_id != NULL)
            {
                if (cur_owner != NULL && !pascal_identifier_equals(cur_owner, self_record->type_id))
                {
                    if (owner_record != NULL)
                    {
                        method_node = semcheck_find_class_method(symtab, owner_record, proc_id, NULL);
                        if (method_node != NULL)
                            self_record = owner_record;
                    }
                }
            }
            else if (method_node != NULL && owner_record != NULL)
            {
                self_record = owner_record;
            }
            if (method_node != NULL &&
                (method_node->hash_type == HASHTYPE_PROCEDURE ||
                 method_node->hash_type == HASHTYPE_FUNCTION))
            {
                /* WITH-context override: a method on the enclosing class's
                 * Self can shadow an unqualified call inside `with X do ...`
                 * when X's class also declares (or inherits) the same method.
                 * In Pascal/Delphi semantics, the innermost WITH target wins
                 * over the enclosing method's Self for unqualified method
                 * resolution.  Re-route through the WITH receiver here.
                 *
                 * Notable case: TObject.Free is reachable via Self in any
                 * method body, so without this override `with linkres do
                 * Free;` resolves to Self.Free instead of linkres.Free,
                 * causing a heap corruption double-free. */
                /* Skip the WITH override when the only/innermost WITH context
                 * is the synthetic `with Self do` wrapper that KGPC inserts
                 * around every instance method body (see
                 * convert_method_implementation in from_cparser_statements_and_programs.c).
                 * In that case the WITH target IS Self, so the regular
                 * Self-prepend path below resolves correctly with proper
                 * overload-aware metadata.  Only override when there is an
                 * outer user-written `with X do ...` whose target is a
                 * different expression. */
                int innermost_is_synthetic_self = 0;
                if (with_context_count > 0 &&
                    with_context_stack[with_context_count - 1].context_expr != NULL)
                {
                    struct Expression *innermost_ctx =
                        with_context_stack[with_context_count - 1].context_expr;
                    if (innermost_ctx->type == EXPR_VAR_ID &&
                        innermost_ctx->expr_data.id != NULL &&
                        pascal_identifier_equals(innermost_ctx->expr_data.id, "Self"))
                    {
                        innermost_is_synthetic_self = 1;
                    }
                }

                if (with_context_count > 0 && !innermost_is_synthetic_self)
                {
                    struct Expression *with_expr = NULL;
                    int wm = semcheck_with_try_resolve_method(proc_id, symtab,
                        &with_expr, stmt->line_num);
                    if (wm == 0 && with_expr != NULL)
                    {
                        ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
                        if (self_node != NULL)
                        {
                            self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                            stmt->stmt_data.procedure_call_data.expr_args = self_node;
                            stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                            if (stmt->stmt_data.procedure_call_data.placeholder_method_name == NULL)
                                stmt->stmt_data.procedure_call_data.placeholder_method_name = strdup(proc_id);
                            return semcheck_proccall(symtab, stmt, max_scope_lev);
                        }
                        destroy_expr(with_expr);
                    }
                    else if (wm == 2 && with_expr != NULL)
                    {
                        /* Procedural field on the WITH target: rewrite as
                         * with_expr.field(...) procedural-variable call. */
                        struct Expression *field_access = mk_recordaccess(
                            stmt->line_num, with_expr, strdup(proc_id));
                        if (field_access != NULL)
                        {
                            stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                            stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                            int field_tag = UNKNOWN_TYPE;
                            return return_val + semcheck_stmt_expr_tag(&field_tag, symtab,
                                field_access, max_scope_lev, NO_MUTATE);
                        }
                        destroy_expr(with_expr);
                    }
                }

                /* Save bare method name before rewrite for virtual dispatch check */
                char *bare_method_name = strdup(proc_id);

                /* Prepend Self to arguments only for non-static methods.
                 * Static class methods have no Self parameter. */
                const char *receiver_class_name =
                    (method_node->owner_class != NULL) ? method_node->owner_class :
                    self_record->type_id;
                int method_is_static = (receiver_class_name != NULL && bare_method_name != NULL) ?
                    from_cparser_is_method_static(receiver_class_name, bare_method_name) : 0;
                int method_is_class =
                    (receiver_class_name != NULL && bare_method_name != NULL && !method_is_static) ?
                    from_cparser_is_method_class_method(receiver_class_name, bare_method_name) : 0;
                if (!method_is_static)
                {
                    struct Expression *receiver_expr = mk_varid(stmt->line_num, strdup("Self"));
                    ListNode_t *receiver_arg = (receiver_expr != NULL) ?
                        CreateListNode(receiver_expr, LIST_EXPR) : NULL;
                    if (receiver_arg != NULL)
                    {
                        receiver_arg->next = args_given;
                        stmt->stmt_data.procedure_call_data.expr_args = receiver_arg;
                        args_given = receiver_arg;
                    }
                    if (method_is_class)
                    {
                        stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                        if (stmt->stmt_data.procedure_call_data.self_class_name == NULL &&
                            receiver_class_name != NULL)
                        {
                            stmt->stmt_data.procedure_call_data.self_class_name =
                                strdup(receiver_class_name);
                        }
                    }
                }
                else
                {
                    /* Mark that static method Self handling is already done, so the
                     * downstream placeholder-removal code doesn't strip an explicit
                     * Self argument that was part of the original call site. */
                    static_arg_already_removed = 1;
                }

                /* Update proc_id to the resolved method's id (e.g. TBase__Bump, not TDerived__Bump
                 * when the method is inherited from a parent class). */
                if (method_node->id != NULL)
                {
                    char *new_proc_id = strdup(method_node->id);
                    if (new_proc_id != NULL)
                    {
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                    }
                }

                /* Check if this is a virtual/abstract method call that needs VMT dispatch.
                 * Only for instance methods (not class/static methods), since class methods
                 * use a different VMT dispatch convention (single indirection). */
                /* Use the actual call argument count (excluding Self) for VMT overload
                 * matching instead of method_node's parameter count, because
                 * semcheck_find_class_method may return the wrong overload. The
                 * binding matcher accepts param_count <= b->param_count so calls
                 * that omit trailing default arguments still resolve correctly. */
                int method_param_count = -1;
                {
                    int actual_arg_count = ListLength(args_given);
                    if (!method_is_static && actual_arg_count > 0)
                        actual_arg_count -= 1; /* subtract Self */
                    method_param_count = actual_arg_count;
                }
                if (self_record->type_id != NULL && bare_method_name != NULL &&
                    from_cparser_is_method_virtual_with_types(
                        self_record->type_id,
                        bare_method_name,
                        method_param_count,
                        NULL, 0) &&
                    !from_cparser_is_method_static(self_record->type_id, bare_method_name))
                {
                    stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
                    int vmt_index = -1;
                    if (self_record->methods != NULL)
                    {
                        ListNode_t *method_entry = self_record->methods;
                        while (method_entry != NULL)
                        {
                            struct MethodInfo *info = (struct MethodInfo *)method_entry->cur;
                            if (info != NULL && info->name != NULL &&
                                (info->is_virtual || info->is_override) &&
                                strcasecmp(info->name, bare_method_name) == 0)
                            {
                                if (method_param_count >= 0 && info->param_count >= 0 &&
                                    method_param_count != info->param_count)
                                {
                                    method_entry = method_entry->next;
                                    continue;
                                }
                                vmt_index = info->vmt_index;
                                break;
                            }
                            method_entry = method_entry->next;
                        }
                    }
                    stmt->stmt_data.procedure_call_data.vmt_index = vmt_index;
                    /* Earlier method-placeholder resolution (line ~837) may
                     * have already set self_class_name; free it before
                     * reassigning so the prior strdup doesn't leak. */
                    if (stmt->stmt_data.procedure_call_data.self_class_name != NULL)
                        free(stmt->stmt_data.procedure_call_data.self_class_name);
                    stmt->stmt_data.procedure_call_data.self_class_name =
                        strdup(self_record->type_id);
                    if (stmt->stmt_data.procedure_call_data.cached_owner_class == NULL)
                        stmt->stmt_data.procedure_call_data.cached_owner_class =
                            strdup(self_record->type_id);
                    if (stmt->stmt_data.procedure_call_data.cached_method_name == NULL)
                        stmt->stmt_data.procedure_call_data.cached_method_name =
                            strdup(bare_method_name);
                }
                /* Interface method call check */
                if (self_record != NULL && self_record->is_interface &&
                    self_record->type_id != NULL && bare_method_name != NULL &&
                    !stmt->stmt_data.procedure_call_data.is_interface_call &&
                    self_record->method_templates != NULL)
                {
                    int idx = 0;
                    for (ListNode_t *mt = self_record->method_templates; mt != NULL; mt = mt->next, idx++)
                    {
                        struct MethodTemplate *tmpl = (struct MethodTemplate *)mt->cur;
                        if (tmpl != NULL && tmpl->name != NULL &&
                            strcasecmp(tmpl->name, bare_method_name) == 0)
                        {
                            stmt->stmt_data.procedure_call_data.is_interface_call = 1;
                            stmt->stmt_data.procedure_call_data.vmt_index = idx;
                            if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                                stmt->stmt_data.procedure_call_data.self_class_name =
                                    strdup(self_record->type_id);
                            break;
                        }
                    }
                }
                /* Mark class method calls so codegen passes VMT as Self.
                 * Walk the parent class chain since the method may be inherited. */
                if (self_record->type_id != NULL && bare_method_name != NULL)
                {
                    const char *check_class = self_record->type_id;
                    struct RecordType *check_record = self_record;
                    while (check_class != NULL)
                    {
                        if (from_cparser_is_method_nonstatic_class_method(check_class, bare_method_name))
                        {
                            stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                            break;
                        }
                        const char *parent = (check_record != NULL) ? check_record->parent_class_name : NULL;
                        if (parent == NULL) break;
                        check_record = semcheck_lookup_record_type(symtab, parent);
                        check_class = parent;
                    }
                }
                free(bare_method_name);
            }
            else if (self_record != NULL && self_record->type_id != NULL)
            {
                /* Check if proc_id is already a resolved method call (has owner_class in symbol table) */
                HashNode_t *proc_check_node = NULL;
                int is_already_method = 0;
                if (FindSymbol(&proc_check_node, symtab, proc_id) != 0 && proc_check_node != NULL &&
                    proc_check_node->owner_class != NULL)
                    is_already_method = 1;
                if (!is_already_method)
                {
                /* Check if proc_id is a procedural-type field or property of Self's class.
                 * This handles patterns like FCallBack(Self,a,b,c) and OnQueryInterface(x,y,z)
                 * where FCallBack is a field of type TThunkCallBack (procedural type)
                 * and OnQueryInterface is a property reading from a procedural-type field. */
                const char *field_name = proc_id;
                int is_proc_field = 0;

                /* Use semcheck_lookup_record_type to get a safe, validated RecordType from the symbol table,
                 * since self_record obtained from temp call contexts can have corrupt data */
                struct RecordType *safe_record = semcheck_lookup_record_type(symtab, self_record->type_id);
                if (safe_record != NULL)
                {
                    for (ListNode_t *f = safe_record->fields; f != NULL; f = f->next)
                    {
                        if (f->type != LIST_RECORD_FIELD || f->cur == NULL)
                            continue;
                        struct RecordField *rf = (struct RecordField *)f->cur;
                        if (rf->name == NULL)
                            continue;
                        if (strcasecmp(rf->name, field_name) == 0)
                        {
                            if (rf->proc_type != NULL)
                            {
                                is_proc_field = 1;
                                break;
                            }
                            else if (rf->type_id != NULL)
                            {
                                /* proc_type is resolved during semcheck_qualify_nested_types_for_record
                                 * so this path handles non-nested procedural types only. */
                                HashNode_t *type_node = NULL;
                                if (FindSymbol(&type_node, symtab, rf->type_id) != 0 &&
                                    type_node != NULL && type_node->type != NULL &&
                                    type_node->type->kind == TYPE_KIND_PROCEDURE)
                                {
                                    is_proc_field = 1;
                                    break;
                                }
                            }
                        }
                    }
                    if (!is_proc_field)
                    {
                        /* Check properties — if a property's read_accessor is a procedural-type field */
                        struct ClassProperty *prop = semcheck_find_class_property(symtab, safe_record, proc_id, NULL);
                        if (prop != NULL && prop->read_accessor != NULL)
                        {
                            for (ListNode_t *f2 = safe_record->fields; f2 != NULL; f2 = f2->next)
                            {
                                if (f2->type != LIST_RECORD_FIELD || f2->cur == NULL)
                                    continue;
                                struct RecordField *rf2 = (struct RecordField *)f2->cur;
                                if (rf2->name == NULL)
                                    continue;
                                if (strcasecmp(rf2->name, prop->read_accessor) == 0)
                                {
                                    if (rf2->proc_type != NULL)
                                    {
                                        is_proc_field = 1;
                                        field_name = prop->read_accessor;
                                        break;
                                    }
                                    else if (rf2->type_id != NULL)
                                    {
                                        HashNode_t *type_node = NULL;
                                        if (FindSymbol(&type_node, symtab, rf2->type_id) != 0 &&
                                            type_node != NULL && type_node->type != NULL &&
                                            type_node->type->kind == TYPE_KIND_PROCEDURE)
                                        {
                                            is_proc_field = 1;
                                            field_name = prop->read_accessor;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if (is_proc_field)
                {
                    /* Convert to Self.field(...) procedural variable call.
                     * Build a record access expression Self.field and set it as procedural_var_expr */
                    struct Expression *self_expr = mk_varid(stmt->line_num, strdup("Self"));
                    struct Expression *field_access = mk_recordaccess(stmt->line_num,
                        self_expr, strdup(field_name));

                    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                    stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

                    /* Check the expression for type resolution */
                    int field_tag = UNKNOWN_TYPE;
                    return_val += semcheck_stmt_expr_tag(&field_tag, symtab, field_access, max_scope_lev, NO_MUTATE);

                    return return_val;
                }
                }
            }
        }
    }

    int handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Halt",
        semcheck_builtin_halt, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Error",
        semcheck_builtin_error, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetLength",
        semcheck_builtin_setlength, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetString",
        semcheck_builtin_setstring, max_scope_lev, &handled_builtin);
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
    return_val += try_resolve_builtin_procedure(symtab, stmt, "writestr",
        semcheck_builtin_writestr, max_scope_lev, &handled_builtin);
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
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Assign",
        semcheck_builtin_assign, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Close",
        semcheck_builtin_close, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "SetTextCodePage",
        semcheck_builtin_settextcodepage, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "GetMem",
        semcheck_builtin_getmem, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "FreeMem",
        semcheck_builtin_freemem, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Move",
        semcheck_builtin_move, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "ReallocMem",
        semcheck_builtin_reallocmem, max_scope_lev, &handled_builtin);
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

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Initialize",
        semcheck_builtin_initialize, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Finalize",
        semcheck_builtin_finalize, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Assert",
        semcheck_builtin_assert, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    /* Handle procedural fields on records (advanced records) similarly to function calls */
    if (proc_id != NULL && args_given != NULL)
    {
        struct Expression *receiver_expr = (struct Expression *)args_given->cur;
        if (receiver_expr != NULL && receiver_expr->type == EXPR_RECORD_CONSTRUCTOR &&
            (receiver_expr->resolved_kgpc_type == NULL ||
             !kgpc_type_is_record(receiver_expr->resolved_kgpc_type)))
        {
            receiver_expr = NULL;
        }
        if (receiver_expr != NULL)
        {
            int recv_type = UNKNOWN_TYPE;
            semcheck_stmt_expr_tag(&recv_type, symtab, receiver_expr, max_scope_lev, NO_MUTATE);

            struct RecordType *recv_record = NULL;
            if (recv_type == RECORD_TYPE &&
                receiver_expr->resolved_kgpc_type != NULL &&
                receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_RECORD)
            {
                recv_record = kgpc_type_get_record(receiver_expr->resolved_kgpc_type);
            }
            else if (recv_type == POINTER_TYPE)
            {
                if (receiver_expr->resolved_kgpc_type != NULL &&
                    receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER)
                {
                    KgpcType *pointee = receiver_expr->resolved_kgpc_type->info.points_to;
                    if (pointee != NULL && kgpc_type_is_record(pointee))
                        recv_record = kgpc_type_get_record(pointee);
                }
            }
            /* Also try resolved_kgpc_type directly for record types when record_type is NULL */
            if (recv_record == NULL && receiver_expr->resolved_kgpc_type != NULL &&
                receiver_expr->resolved_kgpc_type->kind == TYPE_KIND_RECORD)
            {
                recv_record = kgpc_type_get_record(receiver_expr->resolved_kgpc_type);
            }
            if (recv_record == NULL && receiver_expr->type == EXPR_VAR_ID &&
                receiver_expr->expr_data.id != NULL)
            {
                HashNode_t *recv_node = NULL;
                if (FindSymbol(&recv_node, symtab, receiver_expr->expr_data.id) != 0 &&
                    recv_node != NULL)
                {
                    recv_record = semcheck_stmt_get_record_type_from_node(recv_node);
                    if (recv_record == NULL && recv_node->type != NULL &&
                        recv_node->type->kind == TYPE_KIND_POINTER &&
                        recv_node->type->info.points_to != NULL &&
                        kgpc_type_is_record(recv_node->type->info.points_to))
                    {
                        recv_record = kgpc_type_get_record(recv_node->type->info.points_to);
                    }
                }
            }

            if (recv_record != NULL)
            {
                const char *field_lookup = proc_id;
                while (field_lookup != NULL && field_lookup[0] == '_' && field_lookup[1] == '_')
                    field_lookup += 2;

                struct RecordField *field_desc = NULL;
                long long field_offset = 0;
                int field_found = resolve_record_field(symtab, recv_record, field_lookup, &field_desc,
                                         &field_offset, stmt->line_num, 1);
                if (field_found == 0 && field_desc != NULL)
                {
                    int is_proc_field = (field_desc->type == PROCEDURE);
                    KgpcType *proc_type = NULL;
                    if (field_desc->type_id != NULL)
                    {
                        HashNode_t *type_node = NULL;
                        if (FindSymbol(&type_node, symtab, field_desc->type_id) != 0 &&
                            type_node != NULL && type_node->type != NULL &&
                            type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            proc_type = type_node->type;
                            kgpc_type_retain(proc_type);
                            is_proc_field = 1;
                        }
                        /* If not found directly, try with class prefix (for nested types) */
                        if (!is_proc_field && recv_record != NULL && recv_record->type_id != NULL)
                        {
                            char qualified[512];
                            snprintf(qualified, sizeof(qualified), "%s.%s", recv_record->type_id, field_desc->type_id);
                            type_node = NULL;
                            if (FindSymbol(&type_node, symtab, qualified) != 0 &&
                                type_node != NULL && type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_PROCEDURE)
                            {
                                proc_type = type_node->type;
                                kgpc_type_retain(proc_type);
                                is_proc_field = 1;
                            }
                        }
                    }
                    else if (field_desc->proc_type != NULL &&
                             field_desc->proc_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        proc_type = field_desc->proc_type;
                        kgpc_type_retain(proc_type);
                        is_proc_field = 1;
                    }

                    if (is_proc_field)
                    {
                        /* Remove receiver argument */
                        ListNode_t *remaining_args = args_given->next;
                        stmt->stmt_data.procedure_call_data.expr_args = remaining_args;
                        args_given->cur = NULL;
                        free(args_given);

                        /* Build record access expression for the procedural field */
                        struct Expression *proc_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
                        if (proc_expr == NULL)
                        {
                            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: failed to allocate procedural field expression.\n",
                                stmt->line_num);
                            if (proc_type != NULL) destroy_kgpc_type(proc_type);
                            return ++return_val;
                        }
                        proc_expr->line_num = stmt->line_num;
                        proc_expr->type = EXPR_RECORD_ACCESS;
                        proc_expr->expr_data.record_access_data.record_expr = receiver_expr;
                        proc_expr->expr_data.record_access_data.field_id = strdup(field_lookup);
                        proc_expr->expr_data.record_access_data.field_offset = (int)field_offset;
                        if (proc_type != NULL)
                        {
                            if (proc_expr->resolved_kgpc_type != NULL)
                                destroy_kgpc_type(proc_expr->resolved_kgpc_type);
                            proc_expr->resolved_kgpc_type = proc_type;
                        }

                        /* Validate argument count/types if we know the procedural signature */
                        if (proc_type != NULL)
                        {
                            ListNode_t *formal_params = kgpc_type_get_procedure_params(proc_type);
                            if (ListLength(formal_params) != ListLength(remaining_args))
                            {
                                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, call to procedural field %s: expected %d arguments, got %d\n",
                                    stmt->line_num, proc_id, ListLength(formal_params), ListLength(remaining_args));
                                destroy_expr(proc_expr);
                                /* proc_type already released by destroy_expr via resolved_kgpc_type */
                                return ++return_val;
                            }

                            ListNode_t *formal = formal_params;
                            ListNode_t *actual = remaining_args;
                            while (formal != NULL && actual != NULL)
                            {
                                Tree_t *formal_decl = (Tree_t *)formal->cur;
                                struct Expression *actual_expr = (struct Expression *)actual->cur;
                                int formal_type = resolve_param_type(formal_decl, symtab);
                                int actual_type = UNKNOWN_TYPE;
                                semcheck_stmt_expr_tag(&actual_type, symtab, actual_expr, max_scope_lev, NO_MUTATE);
                                if (formal_type != UNKNOWN_TYPE && actual_type != UNKNOWN_TYPE &&
                                    formal_type != actual_type)
                                {
                                    if (!((formal_type == LONGINT_TYPE && actual_type == INT_TYPE) ||
                                          (formal_type == INT_TYPE && actual_type == LONGINT_TYPE) ||
                                          (formal_type == POINTER_TYPE) || (actual_type == POINTER_TYPE) ||
                                          (is_integer_type(formal_type) && is_integer_type(actual_type)) ||
                                          (is_real_family_type(formal_type) && is_integer_type(actual_type)) ||
                                          (is_integer_type(formal_type) && is_real_family_type(actual_type)) ||
                                          (is_real_family_type(formal_type) && is_real_family_type(actual_type)) ||
                                          (formal_type == VARIANT_TYPE) ||
                                          (actual_type == VARIANT_TYPE) ||
                                          (formal_type == BUILTIN_ANY_TYPE) ||
                                          (actual_type == BUILTIN_ANY_TYPE) ||
                                          (formal_type == RECORD_TYPE) ||
                                          (actual_type == RECORD_TYPE) ||
                                          (formal_type == STRING_TYPE && actual_type == CHAR_TYPE) ||
                                          (formal_type == CHAR_TYPE && actual_type == STRING_TYPE) ||
                                          (formal_type == SHORTSTRING_TYPE && actual_type == CHAR_TYPE) ||
                                          (formal_type == STRING_TYPE && actual_type == SHORTSTRING_TYPE) ||
                                          (formal_type == SHORTSTRING_TYPE && actual_type == STRING_TYPE)))
                                {
                                    semantic_error_at(stmt->line_num, stmt->col_num, -1,
                                        "Incompatible types: got \"%s\" expected \"%s\"",
                                        type_tag_to_string(actual_type),
                                        type_tag_to_string(formal_type));
                                    destroy_expr(proc_expr);
                                    return ++return_val;
                                }
                                }
                                formal = formal->next;
                                actual = actual->next;
                            }

                            kgpc_type_retain(proc_type);
                            stmt->stmt_data.procedure_call_data.call_kgpc_type = proc_type;
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                            stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                        }
                        else
                        {
                            stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                        }

                        stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                        stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
                        stmt->stmt_data.procedure_call_data.procedural_var_expr = proc_expr;
                        return return_val;
                    }
                }
            }
        }
    }

    /* When call_qualifier is already set to a known unit name (e.g. System.Seek
     * inside a method body), the parser may have set is_method_call_placeholder
     * with Self as the first arg.  Skip method resolution entirely and handle
     * as a unit-qualified free procedure call.  We cannot modify the AST here
     * because the same nodes are shared across unit copies. */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder &&
        stmt->stmt_data.procedure_call_data.call_qualifier != NULL &&
        semcheck_is_unit_name(stmt->stmt_data.procedure_call_data.call_qualifier) &&
        args_given != NULL)
    {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL && first_arg->type == EXPR_VAR_ID &&
            first_arg->expr_data.id != NULL &&
            pascal_identifier_equals(first_arg->expr_data.id, "Self"))
        {
            /* Type-check Self (first arg) so it doesn't leave unresolved types,
             * then skip it for the actual call. */
            int self_type_tag = UNKNOWN_TYPE;
            semcheck_stmt_expr_tag(&self_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
            /* Advance past Self for overload resolution */
            args_given = args_given->next;
            was_unit_qualified = 1;
            goto skip_method_placeholder_resolution;
        }
    }

    /* Check for method call with unresolved name (member-access placeholder) where first arg is the instance. */
    if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder && args_given != NULL) {
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        int first_arg_type_tag;
        semcheck_stmt_expr_tag(&first_arg_type_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);

        KgpcType *owner_type = first_arg->resolved_kgpc_type;
        struct RecordType *record_info = NULL;

        if (owner_type != NULL) {
            if (owner_type->kind == TYPE_KIND_RECORD) {
                record_info = owner_type->info.record_info;
            } else if (owner_type->kind == TYPE_KIND_POINTER) {
                /* Try lazy resolution of unresolved pointer pointees */
                KgpcType *pointee = kgpc_type_resolve_pointer_pointee(owner_type, symtab);
                if (pointee != NULL && pointee->kind == TYPE_KIND_RECORD) {
                    record_info = pointee->info.record_info;
                } else if (pointee != NULL && pointee->kind == TYPE_KIND_POINTER) {
                    KgpcType *pointee2 = kgpc_type_resolve_pointer_pointee(pointee, symtab);
                    if (pointee2 != NULL && pointee2->kind == TYPE_KIND_RECORD)
                        record_info = pointee2->info.record_info;
                }
            }
        }

        /* Do not rely on legacy record_type metadata; prefer resolved KgpcType only. */

        if (first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
        {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, first_arg->expr_data.id) != 0 &&
                type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
            {
                static_method_receiver = 1;
                if (record_info == NULL)
                    record_info = semcheck_stmt_get_record_type_from_node(type_node);
            }
        }

        if (record_info != NULL && record_info->type_id != NULL) {
            const char *method_name_source =
                (stmt->stmt_data.procedure_call_data.placeholder_method_name != NULL)
                    ? stmt->stmt_data.procedure_call_data.placeholder_method_name : proc_id;
            char *method_name_owned =
                (method_name_source != NULL) ? strdup(method_name_source) : NULL;
            const char *method_name = method_name_owned;

            struct RecordType *actual_method_owner = NULL;
            HashNode_t *method_node = semcheck_find_class_method(symtab, record_info, method_name, &actual_method_owner);
            int is_static = from_cparser_is_method_static(record_info->type_id, method_name);
            if (method_node == NULL && !record_info->is_type_helper)
            {
                struct RecordType *helper_record =
                    semcheck_lookup_type_helper_for_record_member(symtab,
                        record_info, method_name);
                if (helper_record != NULL)
                {
                    actual_method_owner = NULL;
                    method_node = semcheck_find_class_method(symtab, helper_record,
                        method_name, &actual_method_owner);
                    if (method_node != NULL)
                        record_info = helper_record;
                }
            }
            /* Check the actual method owner for inherited static methods */
            if (!is_static && actual_method_owner != NULL &&
                actual_method_owner->type_id != NULL && method_name != NULL) {
                is_static = from_cparser_is_method_static(actual_method_owner->type_id, method_name);
            }
            int is_nonstatic_class_method =
                (!is_static &&
                 from_cparser_is_method_class_method(record_info->type_id, method_name));
            if (!is_nonstatic_class_method && !is_static && actual_method_owner != NULL &&
                actual_method_owner->type_id != NULL && method_name != NULL) {
                is_nonstatic_class_method = from_cparser_is_method_class_method(actual_method_owner->type_id, method_name);
            }

            if (method_node != NULL) {
                /* Keep class-prefixed id for static/class calls (e.g. ClassName.Create),
                 * but canonicalize instance calls to the resolved owner id.
                 * Use the actual method owner's class name so inherited methods
                 * resolve to the defining class (e.g. TObject__Free, not TChild__Free). */
                if (static_method_receiver || is_nonstatic_class_method)
                {
                    const char *owner_id = (actual_method_owner != NULL && actual_method_owner->type_id != NULL)
                        ? actual_method_owner->type_id : record_info->type_id;
                    size_t class_len = strlen(owner_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL) {
                        sprintf(new_proc_id, "%s__%s", owner_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                    }
                }
                else if (method_node->id != NULL)
                {
                    char *new_proc_id = strdup(method_node->id);
                    if (new_proc_id != NULL) {
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                    }
                }
                else
                {
                    /* Synthesize class-prefixed method id from record type. */
                    size_t class_len = strlen(record_info->type_id);
                    size_t method_len = strlen(method_name);
                    char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                    if (new_proc_id != NULL) {
                        sprintf(new_proc_id, "%s__%s", record_info->type_id, method_name);
                        free(proc_id);
                        proc_id = new_proc_id;
                        stmt->stmt_data.procedure_call_data.id = proc_id;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                    }
                }

                semcheck_stmt_try_set_method_mangled_id(symtab, stmt, proc_id,
                    method_node->mangled_id);

                int receiver_is_type_ident = 0;
                if (args_given != NULL && args_given->cur != NULL)
                {
                    struct Expression *receiver_expr = (struct Expression *)args_given->cur;
                    if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                        receiver_expr->expr_data.id != NULL)
                    {
                        HashNode_t *receiver_node = NULL;
                        if (FindSymbol(&receiver_node, symtab, receiver_expr->expr_data.id) != 0 &&
                            receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                            receiver_is_type_ident = 1;
                    }
                }

                if (!is_static && !is_nonstatic_class_method &&
                    !stmt->stmt_data.procedure_call_data.is_tp_new_dispose_helper_call &&
                    !receiver_is_type_ident)
                {
                    semcheck_stmt_set_receiver_virtual_dispatch(stmt,
                        record_info, method_name, method_node->type);
                }

                {
                    struct RecordType *constructor_owner =
                        (actual_method_owner != NULL) ? actual_method_owner : record_info;
                    if (constructor_owner != NULL &&
                        semcheck_stmt_method_is_declared_constructor(symtab,
                            constructor_owner, method_name))
                    {
                        stmt->stmt_data.procedure_call_data.is_constructor_call = 1;
                        if (receiver_is_type_ident)
                        {
                            free(stmt->stmt_data.procedure_call_data.constructor_class_name);
                            stmt->stmt_data.procedure_call_data.constructor_class_name =
                                strdup(record_info->type_id);
                        }
                    }
                }

                if (is_nonstatic_class_method && receiver_is_type_ident)
                {
                    stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                    if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                    {
                        stmt->stmt_data.procedure_call_data.self_class_name =
                            strdup(record_info->type_id);
                    }
                }
                else if (is_static && receiver_is_type_ident) {
                    /* For static methods, remove the first argument (the instance/type identifier) */
                    ListNode_t *old_head = args_given;
                    stmt->stmt_data.procedure_call_data.expr_args = old_head->next;
                    destroy_expr((struct Expression *)old_head->cur);
                    old_head->cur = NULL;
                    old_head->next = NULL;
                    free(old_head);
                    args_given = stmt->stmt_data.procedure_call_data.expr_args;
                    static_arg_already_removed = 1;
                }
                else if (is_static && !receiver_is_type_ident && args_given != NULL)
                {
                    /* Static method called via instance variable or implicit Self.
                     * Static methods have no Self parameter, so strip the receiver. */
                    ListNode_t *old_head = args_given;
                    stmt->stmt_data.procedure_call_data.expr_args = old_head->next;
                    destroy_expr((struct Expression *)old_head->cur);
                    old_head->cur = NULL;
                    old_head->next = NULL;
                    free(old_head);
                    args_given = stmt->stmt_data.procedure_call_data.expr_args;
                    static_arg_already_removed = 1;
                }
            }
            else
            {
                /* Method not found — check if this is a procedural-type field being invoked. */
                struct RecordField *proc_field = NULL;
                long long proc_field_offset = 0;
                if (resolve_record_field(symtab, record_info, method_name,
                        &proc_field, &proc_field_offset, stmt->line_num, 1 /* silent */) == 0 &&
                    proc_field != NULL)
                {
                    KgpcType *proc_type = NULL;
                    if (proc_field->proc_type != NULL &&
                        proc_field->proc_type->kind == TYPE_KIND_PROCEDURE)
                    {
                        proc_type = proc_field->proc_type;
                    }
                    else if (proc_field->type_id != NULL)
                    {
                        HashNode_t *type_node = NULL;
                        if (FindSymbol(&type_node, symtab, proc_field->type_id) != 0 &&
                            type_node != NULL && type_node->type != NULL &&
                            type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            proc_type = type_node->type;
                        }
                    }

                    if (proc_type != NULL)
                    {
                        /* Remove first_arg from args (it becomes part of the field access expr) */
                        ListNode_t *old_head = args_given;
                        args_given = old_head->next;
                        old_head->cur = NULL; /* Don't free first_arg, we reuse it */
                        free(old_head);
                        stmt->stmt_data.procedure_call_data.expr_args = args_given;

                        /* Build record access expression for the procedural field */
                        struct Expression *proc_expr = mk_recordaccess(
                            stmt->line_num, first_arg, strdup(method_name));
                        proc_expr->expr_data.record_access_data.field_offset = (int)proc_field_offset;
                        kgpc_type_retain(proc_type);
                        proc_expr->resolved_kgpc_type = proc_type;

                        /* Type-check the arguments */
                        for (ListNode_t *arg_cur = args_given; arg_cur != NULL; arg_cur = arg_cur->next)
                        {
                            struct Expression *arg = (struct Expression *)arg_cur->cur;
                            if (arg != NULL)
                                semcheck_stmt_expr_tag(NULL, symtab, arg, max_scope_lev, NO_MUTATE);
                        }

                        kgpc_type_retain(proc_type);
                        stmt->stmt_data.procedure_call_data.call_kgpc_type = proc_type;
                        stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                        stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                        stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
                        stmt->stmt_data.procedure_call_data.procedural_var_expr = proc_expr;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;
                        free(method_name_owned);
                        return return_val;
                    }
                }
            }
            free(method_name_owned);
        }
    }

    /* First, check if this is a static method call.
     * Method calls can have two patterns:
     * 1. __MethodName(object, ...) - method call without class prefix
     * 2. ClassName__MethodName(object, ...) - method call with class prefix
     */
    /* Check if proc_id represents a method call.
     * Use symbol table lookup for structured identity instead of parsing "__". */
    HashNode_t *proc_method_node = NULL;
    int proc_is_method_placeholder = stmt->stmt_data.procedure_call_data.is_method_call_placeholder;
    int proc_is_method = proc_is_method_placeholder;
    const char *proc_method_name_resolved = NULL;
    const char *proc_owner_class_resolved = NULL;
    if (!proc_is_method && proc_id != NULL &&
        FindSymbol(&proc_method_node, symtab, proc_id) != 0 && proc_method_node != NULL &&
        proc_method_node->owner_class != NULL)
    {
        proc_is_method = 1;
        proc_method_name_resolved = proc_method_node->method_name;
        proc_owner_class_resolved = proc_method_node->owner_class;
    }
    if (proc_is_method && args_given != NULL && !static_arg_already_removed) {
        const char *method_name = proc_is_method_placeholder
            ? stmt->stmt_data.procedure_call_data.placeholder_method_name
            : proc_method_name_resolved;
        const char *class_name = proc_owner_class_resolved;
        int need_free_class_name = 0;

        if (proc_is_method_placeholder) {
            /* Case 1: __MethodName - need to get class from first argument */
            if (args_given != NULL && args_given->cur != NULL) {
                struct Expression *first_arg = (struct Expression *)args_given->cur;
                
                /* Try to get the record type of the first argument */
                struct RecordType *record_type = NULL;
                if (first_arg->type == EXPR_VAR_ID && first_arg->expr_data.id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindSymbol(&type_node, symtab, first_arg->expr_data.id) != 0 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                    {
                        record_type = semcheck_stmt_get_record_type_from_node(type_node);
                        if (record_type != NULL && record_type->type_id != NULL)
                            class_name = record_type->type_id;
                    }
                }
                if (first_arg->type == EXPR_VAR_ID) {
                    /* Look up the variable to get its type */
                    HashNode_t *var_node = NULL;
                    if (FindSymbol(&var_node, symtab, first_arg->expr_data.id) != 0 && var_node != NULL &&
                        var_node->type != NULL && var_node->type->kind == TYPE_KIND_RECORD) {
                        record_type = var_node->type->info.record_info;
                    }
                }

                /* Prefer resolved KgpcType for deref chains like pts^^.Method(...)
                 * where legacy record_type metadata may be absent. */
                if (record_type == NULL && first_arg->resolved_kgpc_type != NULL)
                {
                    KgpcType *arg_type = first_arg->resolved_kgpc_type;
                    if (arg_type->kind == TYPE_KIND_RECORD)
                    {
                        record_type = arg_type->info.record_info;
                    }
                    else if (arg_type->kind == TYPE_KIND_POINTER &&
                             arg_type->info.points_to != NULL)
                    {
                        KgpcType *pointee = arg_type->info.points_to;
                        if (pointee->kind == TYPE_KIND_RECORD)
                            record_type = pointee->info.record_info;
                        else if (pointee->kind == TYPE_KIND_POINTER &&
                                 pointee->info.points_to != NULL &&
                                 pointee->info.points_to->kind == TYPE_KIND_RECORD)
                            record_type = pointee->info.points_to->info.record_info;
                    }
                }

                if (record_type == NULL && first_arg->pointer_subtype_id != NULL)
                {
                    HashNode_t *subtype_node = NULL;
                    if (FindSymbol(&subtype_node, symtab, first_arg->pointer_subtype_id) != 0 &&
                        subtype_node != NULL)
                    {
                        record_type = semcheck_stmt_get_record_type_from_node(subtype_node);
                    }
                }

                if (record_type == NULL || record_type->type_id == NULL)
                {
                    int helper_tag = UNKNOWN_TYPE;
                    semcheck_stmt_expr_tag(&helper_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
                    /* Resolve arg_type AFTER semcheck_stmt_expr_tag — the latter may
                       free and replace types on the expression, invalidating earlier pointers. */
                    int arg_type_owned = 0;
                    KgpcType *arg_type = semcheck_resolve_expression_kgpc_type(symtab,
                        first_arg, max_scope_lev, NO_MUTATE, &arg_type_owned);
                    const char *helper_name = NULL;
                    if (arg_type != NULL)
                    {
                        if (arg_type->kind == TYPE_KIND_PRIMITIVE)
                            helper_tag = arg_type->info.primitive_type_tag;
                        struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type);
                        if (alias != NULL)
                        {
                            if (alias->target_type_id != NULL)
                                helper_name = alias->target_type_id;
                            else if (alias->alias_name != NULL)
                                helper_name = alias->alias_name;
                        }
                    }
                    struct RecordType *helper_record =
                        semcheck_lookup_type_helper_for_member(symtab,
                            helper_tag, helper_name, method_name);
                    if (helper_record == NULL && first_arg->type == EXPR_VAR_ID &&
                        first_arg->expr_data.id != NULL)
                    {
                        HashNode_t *var_node = NULL;
                        if (FindSymbol(&var_node, symtab, first_arg->expr_data.id) != 0 &&
                            var_node != NULL)
                        {
                            struct TypeAlias *var_alias = hashnode_get_type_alias(var_node);
                            const char *var_helper_name = NULL;
                            if (var_alias != NULL)
                            {
                                if (var_alias->target_type_id != NULL)
                                    var_helper_name = var_alias->target_type_id;
                                else if (var_alias->alias_name != NULL)
                                    var_helper_name = var_alias->alias_name;
                            }
                            if (var_helper_name != NULL)
                                helper_record = semcheck_lookup_type_helper_for_member(symtab,
                                    UNKNOWN_TYPE, var_helper_name, method_name);
                        }
                    }
                    if (helper_record != NULL)
                        record_type = helper_record;
                    if (arg_type_owned && arg_type != NULL)
                        destroy_kgpc_type(arg_type);
                }

                if (class_name == NULL && record_type != NULL && record_type->type_id != NULL) {
                    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
                        fprintf(stderr, "[SemCheck] method placeholder: resolved helper record %s for %s\n",
                            record_type->type_id, method_name != NULL ? method_name : "<null>");
                    }
                    class_name = record_type->type_id;
                }
            }
        } else {
            /* Case 2: ClassName__MethodName - class_name already set from symbol table lookup */
            /* class_name = proc_owner_class_resolved (set above) */
        }
        
        if (class_name != NULL && method_name != NULL) {
            int is_static = from_cparser_is_method_static(class_name, method_name);
            int is_nonstatic_class_method =
                (!is_static &&
                 from_cparser_is_method_class_method(class_name, method_name));
            HashNode_t *resolved_method = NULL;
            HashNode_t *class_node = NULL;
            if (FindSymbol(&class_node, symtab, class_name) != 0 && class_node != NULL)
            {
                struct RecordType *class_record = semcheck_stmt_get_record_type_from_node(class_node);
                if (class_record != NULL)
                    resolved_method = semcheck_find_class_method(symtab, class_record, method_name, NULL);
            }
            
            /* If proc_id started with __, update it to include the class name */
            if (proc_is_method_placeholder) {
                size_t class_len = strlen(class_name);
                size_t method_len = strlen(method_name);
                char *new_proc_id = (char *)malloc(class_len + 2 + method_len + 1);
                if (new_proc_id != NULL) {
                    sprintf(new_proc_id, "%s__%s", class_name, method_name);
                    free(proc_id);
                    proc_id = new_proc_id;
                    stmt->stmt_data.procedure_call_data.id = proc_id;
                }
            }

            {
                const char *overload_check_id =
                    (resolved_method != NULL && resolved_method->id != NULL)
                    ? resolved_method->id : proc_id;
                semcheck_stmt_try_set_method_mangled_id(symtab, stmt, overload_check_id,
                    resolved_method != NULL ? resolved_method->mangled_id : NULL);
            }
            if (!static_method_receiver && resolved_method != NULL && resolved_method->id != NULL)
            {
                char *resolved_proc_id = strdup(resolved_method->id);
                if (resolved_proc_id != NULL)
                {
                    free(proc_id);
                    proc_id = resolved_proc_id;
                    stmt->stmt_data.procedure_call_data.id = proc_id;
                }
            }
            
            int receiver_is_type_ident = 0;
            int receiver_is_self = 0;
            if (args_given != NULL && args_given->cur != NULL)
            {
                struct Expression *receiver_expr = (struct Expression *)args_given->cur;
                if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                    receiver_expr->expr_data.id != NULL)
                {
                    if (pascal_identifier_equals(receiver_expr->expr_data.id, "Self"))
                        receiver_is_self = 1;
                    HashNode_t *receiver_node = NULL;
                    if (FindSymbol(&receiver_node, symtab, receiver_expr->expr_data.id) != 0 &&
                        receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                        receiver_is_type_ident = 1;
                }
            }

            if (is_nonstatic_class_method && !receiver_is_type_ident && !receiver_is_self)
            {
                stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                    stmt->stmt_data.procedure_call_data.self_class_name = strdup(class_name);
            }

            if (is_static && receiver_is_type_ident) {
                /* For static methods, remove the first argument (the type identifier) */
                ListNode_t *old_head = args_given;
                args_given = old_head->next;
                stmt->stmt_data.procedure_call_data.expr_args = args_given;
                destroy_expr((struct Expression *)old_head->cur);
                old_head->cur = NULL;
                old_head->next = NULL;
                free(old_head);
                static_arg_already_removed = 1;
            }
            else if (is_static && !receiver_is_type_ident && args_given != NULL)
            {
                /* Static method called via instance variable or implicit Self.
                 * Static methods have no Self parameter, so strip the receiver. */
                ListNode_t *old_head = args_given;
                args_given = old_head->next;
                stmt->stmt_data.procedure_call_data.expr_args = args_given;
                destroy_expr((struct Expression *)old_head->cur);
                old_head->cur = NULL;
                old_head->next = NULL;
                free(old_head);
                static_arg_already_removed = 1;
            }
        }

        if (need_free_class_name && class_name != NULL) {
            free((void *)class_name);
        }
    }

    /* Re-check if proc_id is a method (may have been updated by previous block) */
    HashNode_t *type_res_method_node = NULL;
    const char *type_res_method_name = NULL;
    int proc_is_method_for_type_res = stmt->stmt_data.procedure_call_data.is_method_call_placeholder;
    if (!proc_is_method_for_type_res && proc_id != NULL &&
        FindSymbol(&type_res_method_node, symtab, proc_id) != 0 && type_res_method_node != NULL &&
        type_res_method_node->owner_class != NULL)
    {
        proc_is_method_for_type_res = 1;
        type_res_method_name = type_res_method_node->method_name;
    }
    if (proc_is_method_for_type_res && type_res_method_name == NULL)
        type_res_method_name = stmt->stmt_data.procedure_call_data.placeholder_method_name;
    if (proc_is_method_for_type_res && args_given != NULL &&
        !static_arg_already_removed &&
        stmt->stmt_data.procedure_call_data.mangled_id == NULL) {
        const char *method_name_part = type_res_method_name;
        
        /* Get the first argument (should be the object/Self parameter) */
        struct Expression *first_arg = (struct Expression *)args_given->cur;
        if (first_arg != NULL) {
            /* IMPORTANT: Call semcheck_expr_main FIRST to resolve the expression type.
             * This may modify/replace first_arg->resolved_kgpc_type.
             * Only AFTER this call should we get the KgpcType, otherwise we risk
             * getting a pointer that gets freed when semcheck_expr_main updates the type.
             * (e.g., for 'as' expressions which destroy and replace resolved_kgpc_type)
             */
            int helper_tag = UNKNOWN_TYPE;
            semcheck_stmt_expr_tag(&helper_tag, symtab, first_arg, max_scope_lev, NO_MUTATE);
            
            /* Now it's safe to get the KgpcType since semcheck_expr_main has finalized it */
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
                
                if (obj_record_type == NULL)
                {
                    const char *helper_name = NULL;
                    if (arg_type->kind == TYPE_KIND_PRIMITIVE)
                        helper_tag = arg_type->info.primitive_type_tag;
                    struct TypeAlias *alias = kgpc_type_get_type_alias(arg_type);
                    if (alias != NULL)
                    {
                        if (alias->target_type_id != NULL)
                            helper_name = alias->target_type_id;
                        else if (alias->alias_name != NULL)
                            helper_name = alias->alias_name;
                    }
                    struct RecordType *helper_record =
                        semcheck_lookup_type_helper_for_member(symtab,
                            helper_tag, helper_name, method_name_part);
                    if (helper_record != NULL)
                        obj_record_type = helper_record;
                }
                
                if (obj_record_type != NULL) {
                    if (!obj_record_type->is_type_helper &&
                        obj_record_type->type_id != NULL)
                    {
                        struct RecordType *helper_owner = NULL;
                        struct RecordType *helper_record =
                            semcheck_lookup_type_helper_for_record_member(symtab,
                                obj_record_type, method_name_part);
                        if (helper_record != NULL &&
                            semcheck_find_class_method(symtab, helper_record,
                                method_name_part, &helper_owner) != NULL)
                        {
                            obj_record_type = (helper_owner != NULL) ?
                                helper_owner : helper_record;
                        }
                    }

                    /* Found the object with a record type. Now find the class name for this type.
                     * Use the type_id stored directly on the RecordType, which is the canonical
                     * type name where methods are registered. This avoids issues with type aliases
                     * (e.g., IInterface = IUnknown) where walking the symbol table might find the
                     * alias name instead of the original type name. */
                char *correct_class_name = obj_record_type->type_id;
                
                
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
                        if (mangled_name == NULL) {
                            /* Malloc failed, skip to next iteration */
                            break;
                        }
                        sprintf(mangled_name, "%s__%s", current_class_name, method_name_part);
                        
                        /* Check if this mangled name exists in the symbol table */
                        HashNode_t *proc_node = NULL;
                        if (FindSymbol(&proc_node, symtab, mangled_name) != 0 && proc_node != NULL) {
                            /* Found it! Update the procedure ID */
                            free(proc_id);
                            proc_id = mangled_name;
                            stmt->stmt_data.procedure_call_data.id = proc_id;
                            /* Don't set mangled_id here - let the normal mangling process handle it */
                            method_found = 1;
                            break;
                        }
                        
                        free(mangled_name);
                        
                        /* Not found in this class, try parent */
                        if (current_record->parent_class_name != NULL) {
                            char *parent_name = current_record->parent_class_name;
                            
                            /* Look up parent class record type */
                            HashNode_t *parent_node = NULL;
                            if (FindSymbol(&parent_node, symtab, parent_name) != 0 && 
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
                        /* Not found in hierarchy: use original class name so the standard
                         * check produces a clear error message (or the virtual method resolves later).
                         */
                         size_t class_len = strlen(correct_class_name);
                         size_t method_len = strlen(method_name_part);
                         char *mangled_name = (char *)malloc(class_len + 2 + method_len + 1);
                         if (mangled_name != NULL) {
                             sprintf(mangled_name, "%s__%s", correct_class_name, method_name_part);
                             free(proc_id);
                             proc_id = mangled_name;
                             stmt->stmt_data.procedure_call_data.id = proc_id;
                             /* Don't set mangled_id here - let the normal mangling process handle it */
                         }
                    }
                }
            }
        }
            if (arg_type_owned && arg_type != NULL)
                destroy_kgpc_type(arg_type);
        }
    }

skip_method_placeholder_resolution:

    /* For inherited calls where mangled_id is already set, use it directly
     * instead of re-mangling based on the call site arguments.
     * The mangled_id already includes the correct parameter signature. */
    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL) {
        mangled_name = strdup(stmt->stmt_data.procedure_call_data.mangled_id);
    } else {
        mangled_name = MangleFunctionNameFromCallSite(proc_id, args_given, symtab, INT_MAX);
    }
    assert(mangled_name != NULL);

    /* Check for procedural type typecast-call pattern: TypeName(source)(args)
     * The parser creates a call to TypeName with (source, args...) as flat arguments.
     * If TypeName is a procedural type, the first arg is the typecast source and
     * remaining args are the actual call arguments. Transform into indirect call. */
    {
        HashNode_t *type_check = NULL;
        if (FindSymbol(&type_check, symtab, proc_id) && type_check != NULL &&
            type_check->hash_type == HASHTYPE_TYPE &&
            type_check->type != NULL && type_check->type->kind == TYPE_KIND_PROCEDURE &&
            args_given != NULL)
        {
            /* Count expected parameters from the procedural type */
            int expected_params = 0;
            if (type_check->type->info.proc_info.params != NULL)
            {
                for (ListNode_t *p = type_check->type->info.proc_info.params; p != NULL; p = p->next)
                    expected_params++;
            }

            /* Count actual args given */
            int actual_args = 0;
            for (ListNode_t *a = args_given; a != NULL; a = a->next)
                actual_args++;

            /* If we have exactly expected_params + 1 arguments, the first is the typecast source */
            if (actual_args == expected_params + 1)
            {
                struct Expression *typecast_source = (struct Expression *)args_given->cur;

                /* Create a typecast expression wrapping the source */
                struct Expression *typecast_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
                assert(typecast_expr != NULL);
                typecast_expr->type = EXPR_TYPECAST;
                typecast_expr->line_num = stmt->line_num;
                typecast_expr->col_num = stmt->col_num;
                typecast_expr->source_index = stmt->source_index;
                typecast_expr->expr_data.typecast_data.target_type_id = strdup(proc_id);
                typecast_expr->expr_data.typecast_data.expr = typecast_source;

                /* Semcheck the typecast */
                int typecast_tag = UNKNOWN_TYPE;
                return_val += semcheck_stmt_expr_tag(&typecast_tag, symtab, typecast_expr, max_scope_lev, NO_MUTATE);

                /* Remove first list node only; the Expression* it held is now owned
                 * by typecast_expr->expr_data.typecast_data.expr, so we NULL .cur
                 * before freeing the ListNode_t shell. */
                ListNode_t *call_args = args_given->next;
                args_given->next = NULL;
                args_given->cur = NULL;
                free(args_given);  /* Free only the ListNode_t, not the Expression */
                stmt->stmt_data.procedure_call_data.expr_args = call_args;
                args_given = call_args;

                /* Set up as a procedural var call through the typecast */
                stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                stmt->stmt_data.procedure_call_data.procedural_var_expr = typecast_expr;
                stmt->stmt_data.procedure_call_data.resolved_proc = type_check;

                free(mangled_name);

                return return_val + semcheck_call_with_proc_var(symtab, stmt, type_check, max_scope_lev);
            }
        }
    }

    ListNode_t *overload_candidates = FindAllIdents(symtab, proc_id);

    /* When the call was unit-qualified (e.g. System.Seek), filter candidates to
     * only those belonging to the specified unit.  This prevents a same-named
     * method in the current class from shadowing the intended unit's version.
     * Fall back to unfiltered results if filtering would leave no candidates. */
    if (was_unit_qualified && stmt->stmt_data.procedure_call_data.call_qualifier != NULL &&
        overload_candidates != NULL)
    {
        const char *uq_name = stmt->stmt_data.procedure_call_data.call_qualifier;
        ListNode_t *filtered = NULL;
        ListNode_t *filtered_tail = NULL;
        for (ListNode_t *cn = overload_candidates; cn != NULL; cn = cn->next)
        {
            HashNode_t *hn = (HashNode_t *)cn->cur;
            if (hn == NULL) continue;
            int match = 0;
            if (hn->source_unit_index != 0)
            {
                const char *src_name = unit_registry_get(hn->source_unit_index);
                if (src_name != NULL && pascal_identifier_equals(src_name, uq_name))
                    match = 1;
            }
            if (!match && hn->type != NULL && hn->type->kind == TYPE_KIND_PROCEDURE &&
                hn->type->info.proc_info.definition != NULL)
            {
                int def_unit_idx =
                    hn->type->info.proc_info.definition->tree_data.subprogram_data.source_unit_index;
                if (def_unit_idx != 0)
                {
                    const char *src_name = unit_registry_get(def_unit_idx);
                    if (src_name != NULL && pascal_identifier_equals(src_name, uq_name))
                        match = 1;
                }
            }
            if (match)
            {
                ListNode_t *new_node = CreateListNode(hn, LIST_UNSPECIFIED);
                if (filtered == NULL)
                    filtered = new_node;
                else
                    filtered_tail->next = new_node;
                filtered_tail = new_node;
            }
        }
        if (filtered != NULL)
        {
            DestroyList(overload_candidates);
            overload_candidates = filtered;
        }
    }

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
    HashNode_t *parent_lookup_node = NULL;
    if (resolved_proc == NULL && proc_id != NULL &&
        FindSymbol(&parent_lookup_node, symtab, proc_id) != 0 && parent_lookup_node != NULL &&
        parent_lookup_node->owner_class != NULL) {
        {
            char *class_name = strdup(parent_lookup_node->owner_class);
            char *method_name = strdup(parent_lookup_node->method_name);

            if (class_name != NULL && method_name != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
                {
                    fprintf(stderr, "[KGPC] Trying to resolve inherited call: class=%s method=%s\n",
                            class_name, method_name);
                }
                /* Look up the class to find its parent */
                HashNode_t *class_node = NULL;
                if (FindSymbol(&class_node, symtab, class_name) != 0 && class_node != NULL) {
                    struct RecordType *record_info = semcheck_stmt_get_record_type_from_node(class_node);
                    if (record_info == NULL)
                        goto proccall_parent_resolve_done;
                    char *parent_class_name = record_info->parent_class_name;
                    
                    if (kgpc_getenv("KGPC_DEBUG_INHERITED") != NULL)
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
                                            
                                            /* Use the resolved declaration id, not a mangled signature string. */
                                            if (candidate->id != NULL) {
                                                free(stmt->stmt_data.procedure_call_data.id);
                                                stmt->stmt_data.procedure_call_data.id = strdup(candidate->id);
                                                proc_id = stmt->stmt_data.procedure_call_data.id;
                                            }
                                            
                                            break;
                                        }
                                        cur = cur->next;
                                    }
                                    DestroyList(parent_candidates);
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
                            HashNode_t *parent_class_node = semcheck_find_preferred_type_node(symtab, parent_class_name);
                            if (parent_class_node != NULL) {
                                record_info = semcheck_stmt_get_record_type_from_node(parent_class_node);
                                if (record_info == NULL)
                                    break;
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
            
proccall_parent_resolve_done:
            if (class_name != NULL) free(class_name);
            if (method_name != NULL) free(method_name);
        }
    }

    /* If no overloads found and proc_id looks like ClassName__MethodName,
     * try walking the class hierarchy to find the method in a parent class.
     * This handles cases like TElfVersionDef.Create where the constructor
     * is inherited from TFPHashObject but not redeclared in the child. */
    if (resolved_proc == NULL && overload_candidates == NULL && proc_id != NULL)
    {
        const char *dunder = strstr(proc_id, "__");
        if (dunder != NULL && dunder > proc_id)
        {
            char *class_name = strndup(proc_id, (size_t)(dunder - proc_id));
            const char *method_name = dunder + 2;
            if (class_name != NULL && method_name[0] != '\0')
            {
                HashNode_t *class_node = semcheck_find_preferred_type_node(symtab, class_name);
                if (class_node == NULL)
                    FindSymbol(&class_node, symtab, class_name);
                struct RecordType *record_info = (class_node != NULL)
                    ? semcheck_stmt_get_record_type_from_node(class_node) : NULL;
                const char *parent_class_name = (record_info != NULL)
                    ? record_info->parent_class_name : NULL;
                while (parent_class_name != NULL && resolved_proc == NULL)
                {
                    size_t plen = strlen(parent_class_name);
                    size_t mlen = strlen(method_name);
                    char *parent_proc_id = (char *)malloc(plen + 2 + mlen + 1);
                    if (parent_proc_id == NULL)
                        break;
                    sprintf(parent_proc_id, "%s__%s", parent_class_name, method_name);

                    char *parent_mangled = MangleFunctionNameFromCallSite(
                        parent_proc_id, args_given, symtab, INT_MAX);
                    ListNode_t *parent_candidates = FindAllIdents(symtab, parent_proc_id);
                    if (parent_candidates != NULL)
                    {
                        for (ListNode_t *pc = parent_candidates; pc != NULL; pc = pc->next)
                        {
                            HashNode_t *cand = (HashNode_t *)pc->cur;
                            if (cand->mangled_id != NULL && parent_mangled != NULL &&
                                strcmp(cand->mangled_id, parent_mangled) == 0)
                            {
                                resolved_proc = cand;
                                match_count = 1;
                                if (cand->id != NULL)
                                {
                                    free(stmt->stmt_data.procedure_call_data.id);
                                    stmt->stmt_data.procedure_call_data.id = strdup(cand->id);
                                    proc_id = stmt->stmt_data.procedure_call_data.id;
                                }
                                break;
                            }
                        }
                        if (resolved_proc == NULL)
                        {
                            /* No mangled match — try overload resolution on parent candidates */
                            overload_candidates = parent_candidates;
                            parent_candidates = NULL;
                            free(stmt->stmt_data.procedure_call_data.id);
                            stmt->stmt_data.procedure_call_data.id = strdup(parent_proc_id);
                            proc_id = stmt->stmt_data.procedure_call_data.id;
                            free(mangled_name);
                            mangled_name = parent_mangled;
                            parent_mangled = NULL;
                        }
                        DestroyList(parent_candidates);
                    }
                    free(parent_mangled);
                    free(parent_proc_id);
                    if (resolved_proc != NULL || overload_candidates != NULL)
                        break;
                    /* Move to next parent */
                    HashNode_t *pnode = semcheck_find_preferred_type_node(symtab, parent_class_name);
                    struct RecordType *prec = (pnode != NULL)
                        ? semcheck_stmt_get_record_type_from_node(pnode) : NULL;
                    parent_class_name = (prec != NULL) ? prec->parent_class_name : NULL;
                }
            }
            free(class_name);
        }
    }

    /* If we found multiple matches but they all have the same mangled name,
     * treat it as a single match (they're duplicates from different scopes) */
    int force_best_match = 0;
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
            match_count = 1;
            /* resolved_proc already set to first match — keep it,
             * duplicates from different scopes are functionally equivalent */
        }
    }

    if (match_count == 0 && overload_candidates != NULL && !force_best_match)
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

    /* Explicit type-qualified class method calls can arrive here without the
     * synthetic Self/class receiver in statement context (e.g. TClass.Proc()).
     * Reinsert the class receiver before overload resolution so arity matches
     * the hidden Self parameter used by non-static class methods. */
    if (!static_arg_already_removed && overload_candidates != NULL &&
        stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        int receiver_is_type_ident = 0;
        int receiver_is_self = 0;
        if (args_given != NULL && args_given->cur != NULL)
        {
            struct Expression *receiver_expr = (struct Expression *)args_given->cur;
            if (receiver_expr != NULL && receiver_expr->type == EXPR_VAR_ID &&
                receiver_expr->expr_data.id != NULL)
            {
                if (pascal_identifier_equals(receiver_expr->expr_data.id, "Self"))
                    receiver_is_self = 1;
                else
                {
                    HashNode_t *receiver_node = NULL;
                    if (FindSymbol(&receiver_node, symtab, receiver_expr->expr_data.id) != 0 &&
                        receiver_node != NULL && receiver_node->hash_type == HASHTYPE_TYPE)
                        receiver_is_type_ident = 1;
                }
            }
        }

        if (!receiver_is_type_ident && !receiver_is_self)
        {
            const char *missing_class_self = NULL;
            int given_count = ListLength(args_given);
            for (ListNode_t *cur = overload_candidates; cur != NULL; cur = cur->next)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate == NULL || candidate->owner_class == NULL ||
                    candidate->method_name == NULL || candidate->type == NULL ||
                    candidate->type->kind != TYPE_KIND_PROCEDURE)
                    continue;
                if (!from_cparser_is_method_class_method(candidate->owner_class,
                        candidate->method_name) ||
                    from_cparser_is_method_static(candidate->owner_class,
                        candidate->method_name))
                    continue;

                ListNode_t *params = kgpc_type_get_procedure_params(candidate->type);
                if (params == NULL)
                    continue;
                Tree_t *first_param = (Tree_t *)params->cur;
                if (first_param == NULL || first_param->type != TREE_VAR_DECL ||
                    first_param->tree_data.var_decl_data.ids == NULL)
                    continue;
                const char *first_name =
                    (const char *)first_param->tree_data.var_decl_data.ids->cur;
                if (first_name == NULL || !pascal_identifier_equals(first_name, "Self"))
                    continue;

                if (ListLength(params) == given_count + 1)
                {
                    missing_class_self = candidate->owner_class;
                    break;
                }
            }

            if (missing_class_self != NULL)
            {
                struct Expression *class_expr = mk_varid(stmt->line_num,
                    strdup(missing_class_self));
                ListNode_t *class_arg = (class_expr != NULL) ?
                    CreateListNode(class_expr, LIST_EXPR) : NULL;
                if (class_arg != NULL)
                {
                    class_arg->next = args_given;
                    args_given = class_arg;
                    stmt->stmt_data.procedure_call_data.expr_args = args_given;
                }
                stmt->stmt_data.procedure_call_data.is_class_method_call = 1;
                if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                    stmt->stmt_data.procedure_call_data.self_class_name =
                        strdup(missing_class_self);
            }
        }
    }

    /* Before overload resolution, check if WITH context provides a method.
     * This prevents builtins like Concat from shadowing class methods in
     * `with obj do Concat(...)` patterns.
     * However, do NOT intercept if a global procedure/function with the same
     * name exists — e.g., system Move(src,dst,count) must not be hijacked
     * by TFPList.Move(curIndex,newIndex) in a WITH block. */
    if (match_count == 0 && proc_id != NULL && with_context_count > 0 &&
        !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
    {
        /* Skip WITH interception if a non-method, non-builtin symbol with this
         * name exists AND has matching arity.  Builtin functions like Concat are
         * registered with 0 params but are actually variadic — they must not
         * prevent WITH methods from being found (e.g., `with LinkScript do
         * Concat('...')` where LinkScript has a Concat method). */
        HashNode_t *global_proc = NULL;
        int has_global_proc = 0;
        if (FindSymbol(&global_proc, symtab, proc_id) && global_proc != NULL &&
            (global_proc->hash_type == HASHTYPE_FUNCTION ||
             global_proc->hash_type == HASHTYPE_PROCEDURE))
        {
            /* Check if the global proc's param count matches the call args.
             * If the global proc has 0 params but the call has args, it's
             * likely a variadic builtin and should not block WITH resolution. */
            int global_param_count = 0;
            if (global_proc->type != NULL && kgpc_type_is_procedure(global_proc->type))
                global_param_count = ListLength(global_proc->type->info.proc_info.params);
            int call_arg_count = ListLength(stmt->stmt_data.procedure_call_data.expr_args);
            if (global_param_count > 0 || call_arg_count == 0)
                has_global_proc = 1;
        }

        struct Expression *with_expr = NULL;
        int wm = has_global_proc ? 1 : semcheck_with_try_resolve_method(proc_id, symtab, &with_expr, stmt->line_num);
        if ((wm == 0 || wm == 2) && with_expr != NULL)
        {
            if (wm == 2)
            {
                struct Expression *field_access = mk_recordaccess(stmt->line_num,
                    with_expr, strdup(proc_id));
                if (field_access != NULL)
                {
                    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                    stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
                    DestroyList(overload_candidates);
                    free(mangled_name);
                    int field_tag = UNKNOWN_TYPE;
                    return return_val + semcheck_stmt_expr_tag(&field_tag, symtab,
                        field_access, max_scope_lev, NO_MUTATE);
                }
            }

            /* Prepend the WITH context expression as Self argument */
            ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
            if (self_node != NULL)
            {
                self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                stmt->stmt_data.procedure_call_data.expr_args = self_node;
                stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                DestroyList(overload_candidates);
                overload_candidates = NULL;
                free(mangled_name);
                mangled_name = NULL;
                return semcheck_proccall(symtab, stmt, max_scope_lev);
            }
            else
            {
                destroy_expr(with_expr);
            }
        }
    }

    /* If no exact mangled match, choose the best overload deterministically */
    if (match_count == 0 && overload_candidates != NULL)
    {
        HashNode_t *best_candidate = NULL;
        int num_best_matches = 0;
        struct Expression call_stub;
        memset(&call_stub, 0, sizeof(call_stub));
        call_stub.line_num = stmt->line_num;
        call_stub.type = EXPR_FUNCTION_CALL;

        int overload_status = semcheck_resolve_overload(&best_candidate, &num_best_matches,
            overload_candidates, args_given, symtab, &call_stub, max_scope_lev, 0);

        if (overload_status == 0 && best_candidate != NULL && num_best_matches == 1)
        {
            resolved_proc = best_candidate;
            match_count = 1;
            if (best_candidate->mangled_id != NULL)
            {
                free(mangled_name);
                mangled_name = strdup(best_candidate->mangled_id);
            }
        }
        else if (overload_status == 2)
        {
            match_count = num_best_matches > 0 ? num_best_matches : 2;
        }
        else if (overload_status == 3)
        {
            DestroyList(overload_candidates);
            free(mangled_name);
            return ++return_val;
        }
        else
        {
            match_count = 0;
        }
    }

    if (match_count == 1 && overload_candidates != NULL && overload_candidates->next != NULL)
    {
        HashNode_t *best_candidate = NULL;
        int num_best_matches = 0;
        struct Expression call_stub;
        memset(&call_stub, 0, sizeof(call_stub));
        call_stub.line_num = stmt->line_num;
        call_stub.type = EXPR_FUNCTION_CALL;

        int overload_status = semcheck_resolve_overload(&best_candidate, &num_best_matches,
            overload_candidates, args_given, symtab, &call_stub, max_scope_lev, 0);

        if (overload_status == 0 && best_candidate != NULL && num_best_matches == 1 &&
            best_candidate != resolved_proc)
        {
            /* Only override the exact mangled match if the secondary resolution
             * has the same formal param count.  The exact mangled match already
             * verified the arg count via name mangling; allowing a candidate with
             * different arity to replace it causes false "not enough arguments" errors
             * when allow_implicit_leading_self adjusts arity in the resolver. */
            int best_total = 0;
            if (best_candidate->type != NULL && best_candidate->type->kind == TYPE_KIND_PROCEDURE)
                best_total = ListLength(kgpc_type_get_procedure_params(best_candidate->type));
            int resolved_total = 0;
            if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
                resolved_total = ListLength(kgpc_type_get_procedure_params(resolved_proc->type));
            int given_count = ListLength(args_given);
            if (best_total == given_count || best_total == resolved_total)
            {
                resolved_proc = best_candidate;
                if (best_candidate->mangled_id != NULL)
                {
                    free(mangled_name);
                    mangled_name = strdup(best_candidate->mangled_id);
                }
            }
        }
    }

    if (match_count == 1)
    {
        int has_matching_impl = 0;
        if (resolved_proc != NULL && overload_candidates != NULL)
        {
            for (ListNode_t *cand_node = overload_candidates; cand_node != NULL; cand_node = cand_node->next)
            {
                HashNode_t *cand = (HashNode_t *)cand_node->cur;
                if (cand == NULL || cand == resolved_proc || cand->type == NULL ||
                    cand->type->kind != TYPE_KIND_PROCEDURE ||
                    cand->type->info.proc_info.definition == NULL)
                    continue;
                Tree_t *cand_def = cand->type->info.proc_info.definition;
                if (cand_def->tree_data.subprogram_data.statement_list == NULL)
                    continue;
                if (resolved_proc->id != NULL && cand->id != NULL &&
                    pascal_identifier_equals(cand->id, resolved_proc->id))
                {
                    has_matching_impl = 1;
                    break;
                }
            }
        }
        if (kgpc_getenv("KGPC_DEBUG_ASSIGN") != NULL &&
            pascal_identifier_equals(proc_id, "Assign"))
            fprintf(stderr, "[ASSIGN-RESOLVED] mangled=%s match_count=%d\n",
                resolved_proc->mangled_id ? resolved_proc->mangled_id : "<null>", match_count);
        if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
        {
            free(stmt->stmt_data.procedure_call_data.mangled_id);
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        }
        if (resolved_proc->mangled_id != NULL)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_proc->mangled_id);
        else if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(resolved_proc->type);
            if (formal_params != NULL)
                stmt->stmt_data.procedure_call_data.mangled_id =
                    MangleFunctionName(resolved_proc->id, formal_params, symtab);
        }
        else if (mangled_name != NULL)
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(mangled_name);
        else
            stmt->stmt_data.procedure_call_data.mangled_id = NULL;
        if (stmt->stmt_data.procedure_call_data.mangled_id == NULL &&
            (resolved_proc->hash_type == HASHTYPE_PROCEDURE ||
             resolved_proc->hash_type == HASHTYPE_BUILTIN_PROCEDURE ||
             resolved_proc->hash_type == HASHTYPE_FUNCTION) &&
            resolved_proc->id != NULL)
        {
            /* Ensure direct calls have a concrete target name even without external alias */
            stmt->stmt_data.procedure_call_data.mangled_id = strdup(resolved_proc->id);
        }
        if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            Tree_t *proc_def = resolved_proc->type->info.proc_info.definition;
            if (proc_def != NULL && proc_def->tree_data.subprogram_data.statement_list == NULL)
            {
                const char *target_name = proc_def->tree_data.subprogram_data.cname_override;
                if (target_name == NULL || target_name[0] == '\0')
                {
                    if (proc_def->tree_data.subprogram_data.cname_flag)
                        target_name = proc_def->tree_data.subprogram_data.id;
                    else if (!has_matching_impl &&
                             proc_def->tree_data.subprogram_data.id != NULL &&
                             proc_def->tree_data.subprogram_data.id[0] != '\0')
                        target_name = proc_def->tree_data.subprogram_data.id;
                    else if (proc_def->tree_data.subprogram_data.mangled_id != NULL &&
                             proc_def->tree_data.subprogram_data.mangled_id[0] != '\0')
                        target_name = proc_def->tree_data.subprogram_data.mangled_id;
                    else if (resolved_proc->mangled_id != NULL &&
                             resolved_proc->mangled_id[0] != '\0')
                        target_name = resolved_proc->mangled_id;
                    else
                        target_name = resolved_proc->id;
                }
                if (target_name != NULL && target_name[0] != '\0')
                {
                    if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
                        free(stmt->stmt_data.procedure_call_data.mangled_id);
                    stmt->stmt_data.procedure_call_data.mangled_id = strdup(target_name);
                }
            }
        }
        stmt->stmt_data.procedure_call_data.resolved_proc = resolved_proc;

        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = resolved_proc->hash_type;
        semcheck_stmt_set_call_kgpc_type(stmt, resolved_proc->type,
            stmt->stmt_data.procedure_call_data.is_call_info_valid == 1);
        semcheck_stmt_set_call_owner_info(stmt,
            resolved_proc->owner_class, resolved_proc->method_name);
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        semcheck_mark_call_requires_static_link(resolved_proc);

        /* Centralized virtual dispatch resolution — catches abstract virtual methods
         * that weren't detected by the early Self-injection check. Only applies to
         * methods without a body (abstract) and not class/static methods (which use
         * single-indirection VMT dispatch that codegen doesn't support yet). */
        int resolved_param_count = -1;
        if (resolved_proc->type != NULL && resolved_proc->type->kind == TYPE_KIND_PROCEDURE)
        {
            resolved_param_count = ListLength(resolved_proc->type->info.proc_info.params);
            if (resolved_proc->owner_class != NULL &&
                !from_cparser_is_method_static(resolved_proc->owner_class,
                    resolved_proc->method_name))
            {
                if (resolved_param_count > 0)
                    resolved_param_count -= 1;
                else
                    resolved_param_count = 0;
            }
        }
        if (resolved_proc->owner_class != NULL && resolved_proc->method_name != NULL &&
            !stmt->stmt_data.procedure_call_data.is_virtual_call &&
            !from_cparser_is_method_static(resolved_proc->owner_class,
                resolved_proc->method_name) &&
            from_cparser_is_method_virtual_with_types(resolved_proc->owner_class,
                resolved_proc->method_name,
                resolved_param_count,
                NULL, 0))
        {
            struct RecordType *class_record = semcheck_lookup_record_type(symtab,
                resolved_proc->owner_class);
            if (class_record != NULL && record_type_is_class(class_record) &&
                class_record->methods != NULL)
            {
                struct MethodInfo *first_virtual_match = NULL;
                for (ListNode_t *me = class_record->methods; me != NULL; me = me->next)
                {
                    struct MethodInfo *mi = (struct MethodInfo *)me->cur;
                    if (mi != NULL && mi->name != NULL &&
                        (mi->is_virtual || mi->is_override) &&
                        strcasecmp(mi->name, resolved_proc->method_name) == 0)
                    {
                        if (first_virtual_match == NULL)
                            first_virtual_match = mi;
                        if (resolved_param_count >= 0 && mi->param_count >= 0 &&
                            resolved_param_count != mi->param_count)
                        {
                            continue;
                        }
                        stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
                        stmt->stmt_data.procedure_call_data.vmt_index = mi->vmt_index;
                        if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                            stmt->stmt_data.procedure_call_data.self_class_name =
                                strdup(resolved_proc->owner_class);
                        if (stmt->stmt_data.procedure_call_data.cached_owner_class == NULL)
                            stmt->stmt_data.procedure_call_data.cached_owner_class =
                                strdup(resolved_proc->owner_class);
                        if (stmt->stmt_data.procedure_call_data.cached_method_name == NULL)
                            stmt->stmt_data.procedure_call_data.cached_method_name =
                                strdup(resolved_proc->method_name);
                        break;
                    }
                }
                if (!stmt->stmt_data.procedure_call_data.is_virtual_call &&
                    first_virtual_match != NULL)
                {
                    stmt->stmt_data.procedure_call_data.is_virtual_call = 1;
                    stmt->stmt_data.procedure_call_data.vmt_index = first_virtual_match->vmt_index;
                    if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                        stmt->stmt_data.procedure_call_data.self_class_name =
                            strdup(resolved_proc->owner_class);
                    if (stmt->stmt_data.procedure_call_data.cached_owner_class == NULL)
                        stmt->stmt_data.procedure_call_data.cached_owner_class =
                            strdup(resolved_proc->owner_class);
                    if (stmt->stmt_data.procedure_call_data.cached_method_name == NULL)
                        stmt->stmt_data.procedure_call_data.cached_method_name =
                            strdup(resolved_proc->method_name);
                }
            }
        }
        /* Interface method call check — if the owner class is an interface,
         * mark this as an interface call so codegen emits indirect vtable dispatch.
         * Only mark as interface call when Self is actually interface-typed:
         * check the first argument's resolved type to avoid false positives on
         * standalone procedures whose name matches an interface method pattern. */
        if (resolved_proc->owner_class != NULL && resolved_proc->method_name != NULL &&
            !stmt->stmt_data.procedure_call_data.is_interface_call)
        {
            struct RecordType *iface_record = semcheck_lookup_record_type(symtab,
                resolved_proc->owner_class);
            if (iface_record != NULL && iface_record->is_interface &&
                iface_record->method_templates != NULL)
            {
                /* Verify the first argument (Self) is actually interface-typed.
                 * This prevents false positives on direct calls to standalone
                 * procedures like IMyCounter__DoIncrement(p) where p is Pointer. */
                int self_is_interface = 0;
                ListNode_t *call_args = stmt->stmt_data.procedure_call_data.expr_args;
                if (call_args != NULL)
                {
                    struct Expression *self_arg = (struct Expression *)call_args->cur;
                    if (self_arg != NULL && self_arg->resolved_kgpc_type != NULL)
                    {
                        KgpcType *self_type = self_arg->resolved_kgpc_type;
                        /* Dereference pointer to get the underlying record type */
                        if (self_type->kind == TYPE_KIND_POINTER && self_type->info.points_to != NULL)
                            self_type = self_type->info.points_to;
                        if (self_type->kind == TYPE_KIND_RECORD && self_type->info.record_info != NULL &&
                            self_type->info.record_info->is_interface)
                            self_is_interface = 1;
                    }
                }
                if (self_is_interface)
                {
                    int idx = 0;
                    for (ListNode_t *mt = iface_record->method_templates; mt != NULL; mt = mt->next, idx++)
                    {
                        struct MethodTemplate *tmpl = (struct MethodTemplate *)mt->cur;
                        if (tmpl != NULL && tmpl->name != NULL &&
                            strcasecmp(tmpl->name, resolved_proc->method_name) == 0)
                        {
                            stmt->stmt_data.procedure_call_data.is_interface_call = 1;
                            stmt->stmt_data.procedure_call_data.vmt_index = idx;
                            if (stmt->stmt_data.procedure_call_data.self_class_name == NULL)
                                stmt->stmt_data.procedure_call_data.self_class_name =
                                    strdup(resolved_proc->owner_class);
                            break;
                        }
                    }
                }
            }
        }

        /* NOTE: default-argument injection is handled later via
         * append_default_args at the unified site below (stmt->expr_args =
         * args_given). A previous duplicate injection here mutated
         * stmt->expr_args directly, but was then overwritten by the unified
         * site — orphaning the listnode + cloned expression and leaking
         * them on every defaulted call. */

        sym_return = resolved_proc;
        scope_return = 1; // found
    }
    else if (match_count == 0)
    {
        HashNode_t *proc_var = NULL;
        /* Check for procedure variables (HASHTYPE_VAR) or procedure constants (HASHTYPE_CONST)
         * with a procedural type. This allows calling procedure variables and typed constants
         * that hold procedure addresses like: const MyProcRef: TProc = @MyProc; */
        if (FindSymbol(&proc_var, symtab, proc_id) && proc_var != NULL &&
            (proc_var->hash_type == HASHTYPE_VAR || proc_var->hash_type == HASHTYPE_CONST) &&
            proc_var->type != NULL && proc_var->type->kind == TYPE_KIND_PROCEDURE)
        {
            DestroyList(overload_candidates);
            free(mangled_name);

            proc_var->referenced += 1;
            if (0) /* scope depth check removed — tree scoping has no depth */
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s cannot be called in the current context!\n\n",
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

        /* WITH context resolution: if the procedure call couldn't be resolved in
         * normal scope, try resolving via active WITH contexts.  This handles
         * patterns like:  with SomeList.LockList do Add(Self);
         * where Add is a method of the WITH target's class. */
        if (proc_id != NULL && with_context_count > 0)
        {
            struct Expression *with_expr = NULL;
            int wm = semcheck_with_try_resolve_method(proc_id, symtab, &with_expr, stmt->line_num);
            if ((wm == 0 || wm == 2) && with_expr != NULL)
            {
                if (wm == 2)
                {
                    struct Expression *field_access = mk_recordaccess(stmt->line_num,
                        with_expr, strdup(proc_id));
                    if (field_access == NULL)
                    {
                        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d: failed to allocate procedural field expression.\n",
                            stmt->line_num);
                        DestroyList(overload_candidates);
                        free(mangled_name);
                        return return_val + 1;
                    }

                    stmt->stmt_data.procedure_call_data.is_procedural_var_call = 1;
                    stmt->stmt_data.procedure_call_data.procedural_var_expr = field_access;
                    stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;
                    stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;

                    DestroyList(overload_candidates);
                    free(mangled_name);

                    {
                        int field_tag = UNKNOWN_TYPE;
                        return return_val + semcheck_stmt_expr_tag(&field_tag, symtab,
                            field_access, max_scope_lev, NO_MUTATE);
                    }
                }

                /* Prepend the WITH context expression as Self argument */
                ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
                if (self_node != NULL)
                {
                    self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                    stmt->stmt_data.procedure_call_data.expr_args = self_node;
                    /* Mark as method call so the retry resolves via class method lookup */
                    stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                    /* Free overload list before retry */
                    DestroyList(overload_candidates);
                    overload_candidates = NULL;
                    free(mangled_name);
                    mangled_name = NULL;
                    /* Re-evaluate as a method call from scratch */
                    return semcheck_proccall(symtab, stmt, max_scope_lev);
                }
                else
                {
                    destroy_expr(with_expr);
                }
            }
        }

        /* Build detailed error message with argument types and available overloads */
        {
            /* First, build a string showing the actual argument types */
            char arg_types_buf[1024] = "(";
            int buf_pos = 1;
            int any_arg_unknown = 0;
            if (args_given != NULL)
            {
                int idx = 0;
                for (ListNode_t *cur = args_given; cur != NULL; cur = cur->next)
                {
                    struct Expression *arg = (struct Expression *)cur->cur;
                    int tag = UNKNOWN_TYPE;
                    semcheck_stmt_expr_tag(&tag, symtab, arg, max_scope_lev, NO_MUTATE);
                    if (tag == UNKNOWN_TYPE)
                        any_arg_unknown = 1;
                    const char *type_name = semcheck_type_tag_name(tag);
                    
                    /* Also check for resolved_kgpc_type for better type info */
                    if (arg != NULL && arg->resolved_kgpc_type != NULL)
                    {
                        const char *kgpc_str = kgpc_type_to_string(arg->resolved_kgpc_type);
                        if (kgpc_str != NULL && kgpc_str[0] != '\0')
                            type_name = kgpc_str;
                    }
                    
                    if (idx > 0 && buf_pos < (int)sizeof(arg_types_buf) - 3)
                    {
                        arg_types_buf[buf_pos++] = ',';
                        arg_types_buf[buf_pos++] = ' ';
                    }
                    int remaining = (int)sizeof(arg_types_buf) - buf_pos - 1;
                    if (remaining > 0)
                    {
                        int written = snprintf(arg_types_buf + buf_pos, remaining, "%s", type_name);
                        if (written > 0)
                            buf_pos += (written < remaining) ? written : remaining - 1;
                    }
                    idx++;
                }
            }
            if (buf_pos < (int)sizeof(arg_types_buf) - 1)
                arg_types_buf[buf_pos++] = ')';
            arg_types_buf[buf_pos] = '\0';

            /* Now build a string showing available overloads */
            char overloads_buf[2048] = "";
            int ovl_pos = 0;
            if (overload_candidates != NULL)
            {
                for (ListNode_t *cur = overload_candidates; cur != NULL; cur = cur->next)
                {
                    HashNode_t *cand = (HashNode_t *)cur->cur;
                    if (cand != NULL && cand->type != NULL &&
                        (cand->hash_type == HASHTYPE_FUNCTION ||
                         cand->hash_type == HASHTYPE_PROCEDURE))
                    {
                        int remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                        if (remaining <= 0) break;
                        
                        /* Format: "  - procedure_name(param_types)" */
                        int written = snprintf(overloads_buf + ovl_pos, remaining, "  - %s(",
                            cand->id ? cand->id : "<anonymous>");
                        if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                        
                        /* Add parameter types */
                        ListNode_t *params = kgpc_type_get_procedure_params(cand->type);
                        int param_idx = 0;
                        for (ListNode_t *p = params; p != NULL; p = p->next)
                        {
                            Tree_t *param = (Tree_t *)p->cur;
                            if (param != NULL)
                            {
                                remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                                if (remaining <= 0) break;
                                
                                if (param_idx > 0)
                                {
                                    written = snprintf(overloads_buf + ovl_pos, remaining, ", ");
                                    if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                                    remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                                }
                                
                                const char *param_type_str = "?";
                                if (param->tree_data.var_decl_data.cached_kgpc_type != NULL)
                                    param_type_str = kgpc_type_to_string(param->tree_data.var_decl_data.cached_kgpc_type);
                                else if (param->tree_data.var_decl_data.type_id != NULL)
                                    param_type_str = param->tree_data.var_decl_data.type_id;
                                else if (param->tree_data.var_decl_data.type != UNKNOWN_TYPE)
                                    param_type_str = semcheck_type_tag_name(param->tree_data.var_decl_data.type);
                                
                                written = snprintf(overloads_buf + ovl_pos, remaining, "%s", param_type_str);
                                if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                                param_idx++;
                            }
                        }
                        
                        remaining = (int)sizeof(overloads_buf) - ovl_pos - 1;
                        if (remaining > 0)
                        {
                            written = snprintf(overloads_buf + ovl_pos, remaining, ")\n");
                            if (written > 0) ovl_pos += (written < remaining) ? written : remaining - 1;
                        }
                    }
                }
            }

            /* Suppress error when any argument has UNKNOWN_TYPE — the root cause
             * error was already reported upstream. */
            if (any_arg_unknown)
            {
                /* Silently skip — cascading from unresolved arg types */
            }
            else if (overloads_buf[0] != '\0')
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, call to procedure %s%s does not match any available overload.\n"
                    "Available overloads:\n%s",
                    stmt->line_num, proc_id, arg_types_buf, overloads_buf);
                ++return_val;
            }
            else
            {
                if (stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
                {
                    HashNode_t *synth_node = NULL;
                    if (FindSymbol(&synth_node, symtab, proc_id) == 0)
                    {
                        KgpcType *synth_type = create_procedure_type(NULL, NULL);
                        if (synth_type != NULL)
                        {
                            (void)PushProcedureOntoScope_Typed(symtab, proc_id, proc_id, synth_type);
                            destroy_kgpc_type(synth_type);
                        }
                    }
                    DestroyList(overload_candidates);
                    free(mangled_name);
                    return return_val;
                }
                /* No overloads found - procedure is not declared */
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, procedure %s%s is not declared.\n",
                    stmt->line_num, proc_id, arg_types_buf);
                ++return_val;
            }
        }
        DestroyList(overload_candidates);
        free(mangled_name);
        return return_val;
    }
    else
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, call to procedure %s is ambiguous\n", stmt->line_num, proc_id);
        DestroyList(overload_candidates);
        free(mangled_name);
        return ++return_val;
    }
    DestroyList(overload_candidates);
    free(mangled_name);

    if(!scope_return) // Should not happen if match_count > 0
    {
        semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, unrecognized procedure call %s\n", stmt->line_num,
            proc_id);
        ++return_val;
    }
    else
    {
        if (with_context_count > 0 &&
            proc_id != NULL &&
            sym_return != NULL &&
            sym_return->owner_class == NULL &&
            !stmt->stmt_data.procedure_call_data.is_method_call_placeholder)
        {
            int try_with_override = 0;
            if (sym_return->type != NULL && sym_return->type->kind == TYPE_KIND_PROCEDURE)
            {
                ListNode_t *params = kgpc_type_get_procedure_params(sym_return->type);
                if (params != NULL && params->cur != NULL)
                {
                    Tree_t *first_decl = (Tree_t *)params->cur;
                    const char *first_type_id = NULL;
                    if (first_decl != NULL && first_decl->type == TREE_VAR_DECL)
                        first_type_id = first_decl->tree_data.var_decl_data.type_id;
                    else if (first_decl != NULL && first_decl->type == TREE_ARR_DECL)
                        first_type_id = first_decl->tree_data.arr_decl_data.type_id;
                    if (first_type_id != NULL &&
                        strlen(first_type_id) == 1 &&
                        first_type_id[0] >= 'A' && first_type_id[0] <= 'Z')
                    {
                        try_with_override = 1;
                    }
                }
            }
            if (try_with_override)
            {
                struct Expression *with_expr = NULL;
                int wm = semcheck_with_try_resolve_method(proc_id, symtab, &with_expr, stmt->line_num);
                if (wm == 0 && with_expr != NULL)
                {
                    ListNode_t *self_node = CreateListNode(with_expr, LIST_EXPR);
                    if (self_node != NULL)
                    {
                        self_node->next = stmt->stmt_data.procedure_call_data.expr_args;
                        stmt->stmt_data.procedure_call_data.expr_args = self_node;
                        stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
                        if (stmt->stmt_data.procedure_call_data.placeholder_method_name == NULL)
                            stmt->stmt_data.procedure_call_data.placeholder_method_name = strdup(proc_id);
                        return semcheck_proccall(symtab, stmt, max_scope_lev);
                    }
                    destroy_expr(with_expr);
                }
            }
        }

        sym_return->referenced += 1; /* Moved here: only access if sym_return is valid */

        if (sym_return->type != NULL && sym_return->type->kind == TYPE_KIND_PROCEDURE)
        {
            ListNode_t *formal_params = kgpc_type_get_procedure_params(sym_return->type);
            if (append_default_args(&args_given, formal_params, stmt->line_num) != 0)
                ++return_val;
            stmt->stmt_data.procedure_call_data.expr_args = args_given;
        }

        if(0) /* scope depth check removed — tree scoping has no depth */
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, %s cannot be called in the current context!\n\n",
                stmt->line_num, proc_id);
            fprintf(stderr, "[Was it defined above the current function context?]\n");

            ++return_val;
        }
        if(sym_return->hash_type != HASHTYPE_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_BUILTIN_PROCEDURE &&
            sym_return->hash_type != HASHTYPE_FUNCTION)
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index, "Error on line %d, expected %s to be a procedure, function, or builtin!\n\n",
                stmt->line_num, proc_id);

            ++return_val;
        }

        /***** VERIFY ARGUMENTS USING KGPCTYPE ARCHITECTURE *****/
        const char *callee_owner_full = sym_return->owner_class_full;
        const char *callee_owner_outer = sym_return->owner_class_outer;
        if (callee_owner_full == NULL && callee_owner_outer == NULL)
        {
            Tree_t *proc_def = sym_return->type->info.proc_info.definition;
            if (proc_def != NULL && proc_def->type == TREE_SUBPROGRAM)
            {
                callee_owner_full = proc_def->tree_data.subprogram_data.owner_class_full;
                callee_owner_outer = proc_def->tree_data.subprogram_data.owner_class_outer;
                if (callee_owner_full == NULL)
                    callee_owner_full = proc_def->tree_data.subprogram_data.owner_class;
            }
        }
        cur_arg = 0;
        /* Get formal arguments from KgpcType instead of deprecated args field */
        true_args = kgpc_type_get_procedure_params(sym_return->type);
        /* Skip implicit Self parameter when args don't include it
         * (e.g., ClassName.Create(args) where the type qualifier was stripped) */
        if (true_args != NULL && true_args->cur != NULL)
        {
            Tree_t *first_formal = (Tree_t *)true_args->cur;
            if (first_formal->type == TREE_VAR_DECL &&
                first_formal->tree_data.var_decl_data.ids != NULL)
            {
                const char *ff_id = (const char *)first_formal->tree_data.var_decl_data.ids->cur;
                if (ff_id != NULL && pascal_identifier_equals(ff_id, "Self"))
                {
                    int n_args = ListLength(args_given);
                    int n_params = ListLength(true_args);
                    if (n_args == n_params - 1)
                        true_args = true_args->next;
                }
            }
        }
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

                if (semcheck_prepare_array_literal_argument(arg_decl, arg_expr,
                        symtab, INT_MAX, stmt->line_num) != 0)
                {
                    ++return_val;
                    args_given = args_given->next;
                    true_arg_ids = true_arg_ids->next;
                    continue;
                }
                if (semcheck_prepare_record_constructor_argument(arg_decl, arg_expr,
                        symtab, INT_MAX, stmt->line_num) != 0)
                {
                    ++return_val;
                    args_given = args_given->next;
                    true_arg_ids = true_arg_ids->next;
                    continue;
                }
                
                /* ALWAYS resolve both sides to KgpcType for proper type checking */
                int expected_type_owned = 0;
                KgpcType *expected_kgpc_type = resolve_param_type_with_owner(arg_decl, symtab,
                    callee_owner_full, callee_owner_outer, &expected_type_owned);
                if (kgpc_getenv("KGPC_DEBUG_FMTSTR") != NULL && proc_id != NULL &&
                    strcasecmp(proc_id, "FmtStr") == 0)
                {
                    if (arg_decl->type == TREE_VAR_DECL)
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_FMTSTR] param %d VAR_DECL type=%d type_id=%s\n",
                            cur_arg,
                            arg_decl->tree_data.var_decl_data.type,
                            arg_decl->tree_data.var_decl_data.type_id ?
                                arg_decl->tree_data.var_decl_data.type_id : "<null>");
                    }
                    else if (arg_decl->type == TREE_ARR_DECL)
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_FMTSTR] param %d ARR_DECL elem_type=%d elem_type_id=%s\n",
                            cur_arg,
                            arg_decl->tree_data.arr_decl_data.type,
                            arg_decl->tree_data.arr_decl_data.type_id ?
                                arg_decl->tree_data.arr_decl_data.type_id : "<null>");
                    }
                }
                
                /* For var/out parameters, we need to mark the argument as mutated.
                 * This is important for tracking whether Result was assigned in a function. */
                int param_is_var_out = (arg_decl->type == TREE_VAR_DECL &&
                                        arg_decl->tree_data.var_decl_data.is_var_param);
                int mutate_flag = param_is_var_out ? MUTATE : NO_MUTATE;
                
                /* Call semcheck_expr_main to properly mark the variable as mutated */
                int dummy_type = UNKNOWN_TYPE;
                semcheck_stmt_expr_tag(&dummy_type, symtab, arg_expr, INT_MAX, mutate_flag);
                
                int arg_type_owned = 0;
                KgpcType *arg_kgpc_type = NULL;
                if (arg_expr != NULL && arg_expr->type == EXPR_ARRAY_LITERAL)
                {
                    arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, mutate_flag, &arg_type_owned);
                }
                else if (arg_expr != NULL && arg_expr->resolved_kgpc_type != NULL)
                {
                    arg_kgpc_type = arg_expr->resolved_kgpc_type;
                    arg_type_owned = 0;
                }
                else
                {
                    arg_type_owned = 0;
                    semcheck_expr_main(symtab, arg_expr, INT_MAX, mutate_flag, &arg_kgpc_type);
                }
                if (arg_kgpc_type == NULL)
                {
                    arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, arg_expr, INT_MAX, mutate_flag, &arg_type_owned);
                }
                if (kgpc_getenv("KGPC_DEBUG_FMTSTR") != NULL && proc_id != NULL &&
                    strcasecmp(proc_id, "FmtStr") == 0)
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_FMTSTR] param %d expected=%s arg=%s\n",
                        cur_arg,
                        expected_kgpc_type ? kgpc_type_to_string(expected_kgpc_type) : "<null>",
                        arg_kgpc_type ? kgpc_type_to_string(arg_kgpc_type) : "<null>");
                }
                int param_is_untyped = semcheck_var_decl_is_untyped(arg_decl);
                if (param_is_untyped &&
                    proc_id != NULL &&
                    cur_arg == 1 &&
                    (pascal_identifier_equals(proc_id, "write") ||
                     pascal_identifier_equals(proc_id, "read") ||
                     pascal_identifier_equals(proc_id, "fpWrite") ||
                     pascal_identifier_equals(proc_id, "fpRead")) &&
                    arg_expr != NULL &&
                    arg_expr->type != EXPR_ADDR &&
                    arg_kgpc_type != NULL &&
                    !kgpc_type_is_pointer(arg_kgpc_type) &&
                    (arg_expr->type == EXPR_VAR_ID ||
                     arg_expr->type == EXPR_ARRAY_ACCESS ||
                     arg_expr->type == EXPR_RECORD_ACCESS ||
                     arg_expr->type == EXPR_POINTER_DEREF))
                {
                    struct Expression *addr_expr = mk_addressof(arg_expr->line_num, arg_expr);
                    KgpcType *new_arg_kgpc_type = NULL;
                    int new_arg_type_owned = 0;
                    semcheck_expr_main(symtab, addr_expr, INT_MAX, NO_MUTATE, &new_arg_kgpc_type);
                    if (new_arg_kgpc_type == NULL)
                    {
                        new_arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, addr_expr,
                            INT_MAX, NO_MUTATE, &new_arg_type_owned);
                    }
                    if (new_arg_kgpc_type != NULL && kgpc_type_is_pointer(new_arg_kgpc_type))
                    {
                        args_given->cur = addr_expr;
                        arg_expr = addr_expr;
                        arg_kgpc_type = new_arg_kgpc_type;
                        arg_type_owned = new_arg_type_owned;
                    }
                    else if (addr_expr != NULL)
                    {
                        addr_expr->expr_data.addr_data.expr = NULL;
                        destroy_expr(addr_expr);
                    }
                }

                /* Perform type compatibility check using KgpcType */
                int types_match = param_is_untyped ? 1 : 0;
                if ((expected_kgpc_type == NULL || arg_kgpc_type == NULL) && !param_is_untyped)
                {
                    /* Suppress cascading errors when types can't be resolved —
                     * upstream UNKNOWN_TYPE already reported the root cause. */
                }
                else if (!param_is_untyped)
                {
                    types_match = are_types_compatible_for_assignment(expected_kgpc_type, arg_kgpc_type, symtab);
                    if (!types_match && expected_kgpc_type != NULL && arg_kgpc_type != NULL &&
                        expected_kgpc_type->kind == TYPE_KIND_ARRAY &&
                        arg_kgpc_type->kind == TYPE_KIND_ARRAY)
                    {
                        KgpcType *expected_elem = kgpc_type_get_array_element_type_resolved(expected_kgpc_type, symtab);
                        KgpcType *arg_elem = kgpc_type_get_array_element_type_resolved(arg_kgpc_type, symtab);
                        if (expected_elem != NULL && arg_elem != NULL)
                        {
                            if (kgpc_type_equals(expected_elem, arg_elem) ||
                                are_types_compatible_for_assignment(expected_elem, arg_elem, symtab) ||
                                (kgpc_type_is_pointer(expected_elem) &&
                                 kgpc_type_equals_tag(arg_elem, POINTER_TYPE)) ||
                                (kgpc_type_is_pointer(arg_elem) &&
                                 kgpc_type_equals_tag(expected_elem, POINTER_TYPE)))
                            {
                                types_match = 1;
                            }
                        }
                    }
                    if (!types_match && expected_kgpc_type != NULL && arg_kgpc_type != NULL &&
                        arg_expr != NULL &&
                        expected_kgpc_type->kind == TYPE_KIND_POINTER &&
                        expected_kgpc_type->info.points_to != NULL &&
                        expected_kgpc_type->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
                        expected_kgpc_type->info.points_to->info.primitive_type_tag == CHAR_TYPE &&
                        arg_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                        arg_kgpc_type->info.primitive_type_tag == CHAR_TYPE &&
                        arg_expr->type != EXPR_ADDR &&
                        (arg_expr->type == EXPR_VAR_ID ||
                         arg_expr->type == EXPR_ARRAY_ACCESS ||
                         arg_expr->type == EXPR_RECORD_ACCESS ||
                         arg_expr->type == EXPR_POINTER_DEREF))
                    {
                        struct Expression *addr_expr = mk_addressof(arg_expr->line_num, arg_expr);
                        KgpcType *new_arg_kgpc_type = NULL;
                        int new_arg_type_owned = 0;

                        semcheck_expr_main(symtab, addr_expr, INT_MAX, NO_MUTATE, &new_arg_kgpc_type);
                        if (new_arg_kgpc_type == NULL)
                        {
                            new_arg_kgpc_type = semcheck_resolve_expression_kgpc_type(symtab, addr_expr,
                                INT_MAX, NO_MUTATE, &new_arg_type_owned);
                        }

                        if (new_arg_kgpc_type != NULL &&
                            are_types_compatible_for_assignment(expected_kgpc_type, new_arg_kgpc_type, symtab))
                        {
                            args_given->cur = addr_expr;
                            arg_expr = addr_expr;
                            if (arg_type_owned && arg_kgpc_type != NULL)
                                destroy_kgpc_type(arg_kgpc_type);
                            arg_kgpc_type = new_arg_kgpc_type;
                            arg_type_owned = new_arg_type_owned;
                            types_match = 1;
                        }
                        else
                        {
                            if (new_arg_type_owned && new_arg_kgpc_type != NULL)
                                destroy_kgpc_type(new_arg_kgpc_type);
                            if (addr_expr != NULL)
                            {
                                addr_expr->expr_data.addr_data.expr = NULL;
                                destroy_expr(addr_expr);
                            }
                        }
                    }
                    /* Class method Self compatibility: if expected is record and given
                     * is ^record (or vice versa), and this is argument 1 (Self) of a
                     * class method, accept the match. Classes are reference types so
                     * Self is always a pointer, but the formal parameter may have been
                     * registered with the plain record type due to type alias collisions. */
                    if (!types_match && cur_arg == 1 &&
                        expected_kgpc_type != NULL && arg_kgpc_type != NULL)
                    {
                        int expected_is_record = (expected_kgpc_type->kind == TYPE_KIND_RECORD);
                        int given_is_ptr_record = (arg_kgpc_type->kind == TYPE_KIND_POINTER &&
                            arg_kgpc_type->info.points_to != NULL &&
                            arg_kgpc_type->info.points_to->kind == TYPE_KIND_RECORD);
                        int expected_is_ptr_record = (expected_kgpc_type->kind == TYPE_KIND_POINTER &&
                            expected_kgpc_type->info.points_to != NULL &&
                            expected_kgpc_type->info.points_to->kind == TYPE_KIND_RECORD);
                        int given_is_record = (arg_kgpc_type->kind == TYPE_KIND_RECORD);
                        if ((expected_is_record && given_is_ptr_record) ||
                            (expected_is_ptr_record && given_is_record))
                        {
                            types_match = 1;
                        }
                    }
                    if (!types_match && !param_is_var_out && expected_kgpc_type != NULL &&
                        arg_kgpc_type != NULL && arg_expr != NULL)
                    {
                        if (semcheck_try_record_conversion_expression(symtab, &arg_expr, NULL,
                                expected_kgpc_type, &arg_kgpc_type, &arg_type_owned))
                        {
                            args_given->cur = arg_expr;
                            types_match = are_types_compatible_for_assignment(
                                expected_kgpc_type, arg_kgpc_type, symtab);
                        }
                    }
                    
                    /* Special AST transformation for procedure parameters */
                    if (types_match && 
                        expected_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_expr != NULL && arg_expr->type == EXPR_VAR_ID)
                    {
                        HashNode_t *arg_node = NULL;
                        if (FindSymbol(&arg_node, symtab, arg_expr->expr_data.id) != 0 &&
                            arg_node != NULL && arg_node->hash_type == HASHTYPE_PROCEDURE)
                        {
                            /* Transform the expression to EXPR_ADDR_OF_PROC.
                             * expr_data is a union; free the EXPR_VAR_ID id
                             * before reassigning the slot, or it leaks. */
                            free(arg_expr->expr_data.id);
                            arg_expr->type = EXPR_ADDR_OF_PROC;
                            arg_expr->expr_data.addr_of_proc_data.receiver_expr = NULL;
                            arg_expr->expr_data.addr_of_proc_data.proc_mangled_id = arg_node->mangled_id ? strdup(arg_node->mangled_id) : NULL;
                            arg_expr->expr_data.addr_of_proc_data.proc_id = arg_node->id ? strdup(arg_node->id) : NULL;
                            arg_expr->expr_data.addr_of_proc_data.source_unit_index = arg_node->source_unit_index;
                            /* Resolve the type NOW while the symbol is still alive. */
                            if (arg_node->type != NULL)
                            {
                                kgpc_type_retain(arg_node->type);
                                arg_expr->resolved_kgpc_type = create_pointer_type(arg_node->type);
                                kgpc_type_release(arg_node->type);
                            }
                            else
                            {
                                arg_expr->resolved_kgpc_type = create_pointer_type(NULL);
                            }
                        }
                    }
                }

                /* Save type strings before cleanup for error message */
                char expected_type_str[256] = "<unknown>";
                char given_type_str[256] = "<unknown>";
                const char *formal_id_dbg = NULL;
                if (arg_decl != NULL && arg_decl->type == TREE_VAR_DECL)
                    formal_id_dbg = arg_decl->tree_data.var_decl_data.type_id;
                else if (arg_decl != NULL && arg_decl->type == TREE_ARR_DECL)
                    formal_id_dbg = arg_decl->tree_data.arr_decl_data.type_id;
                if (!types_match && arg_expr != NULL &&
                    formal_id_dbg != NULL &&
                    arg_expr->pointer_subtype_id != NULL &&
                    semcheck_class_type_ids_compatible(symtab, formal_id_dbg,
                        arg_expr->pointer_subtype_id))
                {
                    types_match = 1;
                }
                if (expected_kgpc_type != NULL)
                    snprintf(expected_type_str, sizeof(expected_type_str), "%s", kgpc_type_to_string(expected_kgpc_type));
                if (arg_kgpc_type != NULL)
                    snprintf(given_type_str, sizeof(given_type_str), "%s", kgpc_type_to_string(arg_kgpc_type));

                /* Check for UNKNOWN_TYPE before cleanup */
                int either_unknown = (kgpc_type_equals_tag(expected_kgpc_type, UNKNOWN_TYPE) ||
                                      kgpc_type_equals_tag(arg_kgpc_type, UNKNOWN_TYPE));

                /* Clean up owned types */
                if (expected_type_owned && expected_kgpc_type != NULL)
                    destroy_kgpc_type(expected_kgpc_type);
                if (arg_type_owned && arg_kgpc_type != NULL)
                    destroy_kgpc_type(arg_kgpc_type);

                if (!types_match && !either_unknown)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SYMCREAT_INSERTSYM") != NULL &&
                        proc_id != NULL &&
                        pascal_identifier_equals(proc_id, "TSymtable__insertsym"))
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_SYMCREAT_INSERTSYM] arg=%d formal_id=%s expected=%s actual=%s expr_type=%d ptr_sub=%d ptr_id=%s\n",
                            cur_arg,
                            formal_id_dbg != NULL ? formal_id_dbg : "<null>",
                            expected_type_str,
                            given_type_str,
                            arg_expr != NULL ? arg_expr->type : -1,
                            arg_expr != NULL ? arg_expr->pointer_subtype : -1,
                            (arg_expr != NULL && arg_expr->pointer_subtype_id != NULL)
                                ? arg_expr->pointer_subtype_id : "<null>");
                    }
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    {
                        fprintf(stderr,
                            "[SemCheck] proccall %s arg %d mismatch: expected=%s actual=%s\n",
                            proc_id ? proc_id : "<null>",
                            cur_arg,
                            expected_type_str,
                            given_type_str);
                    }
                    semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                        "Error on line %d, on procedure call %s, argument %d: Type mismatch (expected: %s, given: %s)!\n\n",
                        stmt->line_num, proc_id, cur_arg, expected_type_str, given_type_str);
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
            int allow_implicit_self_only = 0;
            if (args_given->next == NULL)
            {
                struct Expression *only_arg = (struct Expression *)args_given->cur;
                if (only_arg != NULL &&
                    ((only_arg->type == EXPR_VAR_ID &&
                      only_arg->expr_data.id != NULL &&
                      pascal_identifier_equals(only_arg->expr_data.id, "Self")) ||
                     only_arg->type == EXPR_NIL))
                {
                    allow_implicit_self_only = 1;
                }
            }
            if (allow_implicit_self_only)
                args_given = NULL;
        }
        if(true_args == NULL && args_given != NULL && !(sym_return != NULL && sym_return->is_varargs) &&
            !(sym_return != NULL && sym_return->hash_type == HASHTYPE_BUILTIN_PROCEDURE))
        {
            semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                "Error on line %d, on procedure call %s, too many arguments given!\n\n",
                stmt->line_num, proc_id);
            ++return_val;
        }
        else if(true_args != NULL && args_given == NULL)
        {
            /* Check if all remaining parameters have default values */
            int all_have_defaults = 1;
            ListNode_t *remaining = true_args;
            while (remaining != NULL)
            {
                Tree_t *decl = (Tree_t *)remaining->cur;
                if (!param_has_default_value(decl))
                {
                    all_have_defaults = 0;
                    break;
                }
                remaining = remaining->next;
            }
            
            if (!all_have_defaults)
            {
                semcheck_error_with_context_at(stmt->line_num, stmt->col_num, stmt->source_index,
                    "Error on line %d, on procedure call %s, not enough arguments given!\n\n",
                    stmt->line_num, proc_id);
                ++return_val;
            }
        }
    }

    return return_val;
}

/* Transform TP-style New(p, Constructor(args)) into New(p) + p^.Constructor(args)
 * and Dispose(p, Destructor) into p^.Destructor + Dispose(p).
 * Returns a new statement to insert after (for New) or before (for Dispose) the
 * current statement in the list, or NULL if no transformation needed. */
struct Statement *transform_two_arg_new_dispose(struct Statement *stmt, int *is_dispose)
{
    if (stmt == NULL || stmt->type != STMT_PROCEDURE_CALL)
        return NULL;

    char *proc_id = stmt->stmt_data.procedure_call_data.id;
    if (proc_id == NULL)
        return NULL;

    int is_new = pascal_identifier_equals(proc_id, "New");
    int is_disp = pascal_identifier_equals(proc_id, "Dispose");
    if (!is_new && !is_disp)
        return NULL;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || (args->next != NULL && args->next->next != NULL))
        return NULL;  /* Not exactly 2 args */

    if (is_dispose)
        *is_dispose = is_disp;

    /* Extract the pointer expr (first arg) and ctor/dtor expr (second arg) */
    struct Expression *ptr_expr = (struct Expression *)args->cur;
    struct Expression *method_expr = (struct Expression *)args->next->cur;

    /* Strip the second arg from the New/Dispose call, making it single-arg */
    args->next->cur = NULL;
    ListNode_t *second_node = args->next;
    args->next = NULL;
    free(second_node);

    /* Build the method call statement: p^.Method(args) */
    if (method_expr == NULL || ptr_expr == NULL)
        return NULL;

    /* Get the method name and args from the method_expr.
     * method_expr is either:
     *   - EXPR_FUNCTION_CALL for Create(42) — id="Create", args=[42]
     *   - EXPR_VAR_ID for Destroy — id="Destroy"
     */
    char *method_name = NULL;
    ListNode_t *method_args = NULL;

    if (method_expr->type == EXPR_FUNCTION_CALL)
    {
        const char *fn_id = method_expr->expr_data.function_call_data.id;
        if (fn_id != NULL)
            method_name = strdup(fn_id);
        method_args = method_expr->expr_data.function_call_data.args_expr;
        /* Detach args from the expression so we can reuse them */
        method_expr->expr_data.function_call_data.args_expr = NULL;
        destroy_expr(method_expr);
    }
    else if (method_expr->type == EXPR_VAR_ID)
    {
        if (method_expr->expr_data.id != NULL)
            method_name = strdup(method_expr->expr_data.id);
        destroy_expr(method_expr);
    }
    else
    {
        destroy_expr(method_expr);
        return NULL;
    }

    if (method_name == NULL)
        return NULL;

    /* Build the placeholder proc name: __MethodName */
    size_t name_len = strlen(method_name) + 3;
    char *placeholder_name = (char *)malloc(name_len);
    if (placeholder_name == NULL)
    {
        free(method_name);
        return NULL;
    }
    snprintf(placeholder_name, name_len, "__%s", method_name);

    /* Build the receiver expression: clone of ptr_expr followed by ^ deref */
    struct Expression *receiver = mk_pointer_deref(stmt->line_num,
        clone_expression(ptr_expr));

    /* Build the argument list: receiver (self) + method args */
    ListNode_t *call_args = (ListNode_t *)calloc(1, sizeof(ListNode_t));
    call_args->type = LIST_EXPR;
    call_args->cur = receiver;
    call_args->next = method_args;  /* may be NULL */

    struct Statement *method_call = mk_procedurecall(stmt->line_num, placeholder_name, call_args);
    if (method_call != NULL)
    {
        method_call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
        method_call->stmt_data.procedure_call_data.placeholder_method_name = method_name;
        method_call->stmt_data.procedure_call_data.is_tp_new_dispose_helper_call = 1;
    }
    else
    {
        free(method_name);
    }

    return method_call;
}
