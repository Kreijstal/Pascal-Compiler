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
#include "SemCheck_stmt.h"
#include "SemCheck_expr.h"
#include "../NameMangling.h"
#include "../HashTable/HashTable.h"
#include "../SymTab/SymTab.h"
#include "../../ParseTree/tree.h"
#include "../../ParseTree/tree_types.h"
#include "../../List/List.h"
#include "../../ParseTree/type_tags.h"
#include "../../ParseTree/GpcType.h"
#include "../../../identifier_utils.h"

static int semcheck_loop_depth = 0;

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);

int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_funccall(int *type_return, SymTab_t *symtab, struct Expression *expr, int max_scope_lev, int mutating);
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev);

static int semcheck_statement_list_nodes(SymTab_t *symtab, ListNode_t *stmts, int max_scope_lev);
static int semcheck_call_with_proc_var(SymTab_t *symtab, struct Statement *stmt, HashNode_t *proc_node,
    int max_scope_lev);

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

        /* Phase 3: Use GpcType for comprehensive type checking */
        
        /* Resolve GpcType for the argument expression */
        int arg_type_owned = 0;
        GpcType *arg_type = semcheck_resolve_expression_gpc_type(symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);
        
        /* Resolve GpcType for the formal parameter */
        int param_type_owned = 0;
        GpcType *param_type = NULL;
        if (param_decl != NULL && param_decl->type == TREE_VAR_DECL)
        {
            param_type = resolve_type_from_vardecl(param_decl, symtab, &param_type_owned);
        }



        /* Both types must be resolved for proper type checking */
        if (arg_type == NULL || param_type == NULL)
        {
            fprintf(stderr,
                "Error on line %d, on procedure call %s, argument %d: Unable to resolve type!\n\n",
                stmt->line_num,
                stmt->stmt_data.procedure_call_data.id,
                arg_index);
            ++return_val;
        }
        else
        {
            /* Use comprehensive GpcType-based type compatibility checking */
            if (!are_types_compatible_for_assignment(param_type, arg_type, symtab))
            {
                fprintf(stderr,
                    "Error on line %d, on procedure call %s, argument %d: Type mismatch (expected %s, got %s)!\n\n",
                    stmt->line_num,
                    stmt->stmt_data.procedure_call_data.id,
                    arg_index,
                    gpc_type_to_string(param_type),
                    gpc_type_to_string(arg_type));
                ++return_val;
            }
        }

        /* Clean up owned types */
        if (arg_type_owned && arg_type != NULL)
            destroy_gpc_type(arg_type);
        if (param_type_owned && param_type != NULL)
            destroy_gpc_type(param_type);

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
        stmt->stmt_data.procedure_call_data.call_gpc_type = builtin_node->type;
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

    if (array_expr == NULL || array_expr->type != EXPR_VAR_ID)
    {
        fprintf(stderr, "Error on line %d, first argument to SetLength must be a dynamic array variable.\n", stmt->line_num);
        ++return_val;
    }
    else
    {
        HashNode_t *array_node = NULL;
        if (FindIdent(&array_node, symtab, array_expr->expr_data.id) == -1 || array_node == NULL)
        {
            fprintf(stderr, "Error on line %d, undeclared identifier \"%s\" in SetLength.\n", stmt->line_num, array_expr->expr_data.id);
            ++return_val;
        }
        else
        {
            set_hash_meta(array_node, BOTH_MUTATE_REFERENCE);
            
            /* Check if it's a dynamic array using GpcType first, then legacy field */
            int is_dynamic = 0;
            if (array_node->type != NULL)
            {
                is_dynamic = gpc_type_is_dynamic_array(array_node->type);
            }
            else
            {
                is_dynamic = hashnode_is_dynamic_array(array_node);
            }
            
            if (array_node->hash_type != HASHTYPE_ARRAY || !is_dynamic)
            {
                fprintf(stderr, "Error on line %d, SetLength expects a dynamic array variable.\n", stmt->line_num);
                ++return_val;
            }
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

static int semcheck_builtin_move(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL || args->next == NULL || args->next->next == NULL || args->next->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Move expects exactly three arguments.\n", stmt->line_num);
        return 1;
    }

    struct Expression *src_expr = (struct Expression *)args->cur;
    struct Expression *dst_expr = (struct Expression *)args->next->cur;
    struct Expression *count_expr = (struct Expression *)args->next->next->cur;

    int expr_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&expr_type, symtab, src_expr, INT_MAX, NO_MUTATE);

    expr_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&expr_type, symtab, dst_expr, max_scope_lev, MUTATE);

    int count_type = UNKNOWN_TYPE;
    return_val += semcheck_expr_main(&count_type, symtab, count_expr, INT_MAX, NO_MUTATE);
    if (count_type != INT_TYPE && count_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Move count argument must be an integer.\n", stmt->line_num);
        ++return_val;
    }

    return return_val;
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
    if (target_type != INT_TYPE && target_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Inc target must be an integer variable.\n", stmt->line_num);
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

        if (!saw_file_arg && expr_type == FILE_TYPE)
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

/* Main semantic checking */
int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val;

    assert(symtab != NULL);
    assert(stmt != NULL);

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

        case STMT_BREAK:
            return_val += semcheck_break_stmt(stmt);
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
            return_val += semcheck_statement_list_nodes(symtab, stmt->stmt_data.try_except_data.except_statements, max_scope_lev);
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
                HashNode_t *target_symbol = NULL;
                const char *call_id = call_expr->expr_data.function_call_data.id;
                if (call_id != NULL)
                    FindIdent(&target_symbol, symtab, (char *)call_id);

                int is_function_symbol = (target_symbol != NULL) &&
                    (target_symbol->hash_type == HASHTYPE_FUNCTION ||
                     target_symbol->hash_type == HASHTYPE_FUNCTION_RETURN);

                if (call_expr->type == EXPR_FUNCTION_CALL && !is_function_symbol)
                {
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
                    call_expr->expr_data.function_call_data.resolved_func = temp_call.stmt_data.procedure_call_data.resolved_proc;
                }
                else
                {
                    int inherited_type = UNKNOWN_TYPE;
                    return_val += semcheck_funccall(&inherited_type, symtab, call_expr, max_scope_lev, NO_MUTATE);
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

    /* NOTE: Grammar will make sure the left side is a variable */
    /* Left side var assigns must abide by scoping rules */
    return_val += semcheck_expr_main(&type_first, symtab, var, max_scope_lev, MUTATE);
    return_val += semcheck_expr_main(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

    int lhs_owned = 0, rhs_owned = 0;
    GpcType *lhs_gpctype = semcheck_resolve_expression_gpc_type(symtab, var, max_scope_lev, MUTATE, &lhs_owned);
    GpcType *rhs_gpctype = semcheck_resolve_expression_gpc_type(symtab, expr, INT_MAX, NO_MUTATE, &rhs_owned);
    int handled_by_gpctype = 0;

    if (lhs_gpctype != NULL && rhs_gpctype != NULL &&
        (lhs_gpctype->kind == TYPE_KIND_PROCEDURE || rhs_gpctype->kind == TYPE_KIND_PROCEDURE))
    {
        handled_by_gpctype = 1;
        if (!are_types_compatible_for_assignment(lhs_gpctype, rhs_gpctype, symtab))
        {
            const char *lhs_name = "<expression>";
            if (var != NULL && var->type == EXPR_VAR_ID)
                lhs_name = var->expr_data.id;
            fprintf(stderr,
                "Error on line %d, incompatible types in assignment for %s (lhs: %s, rhs: %s)!\n\n",
                stmt->line_num,
                lhs_name,
                gpc_type_to_string(lhs_gpctype),
                gpc_type_to_string(rhs_gpctype));
            ++return_val;
        }
        else if (type_first == PROCEDURE && type_second == PROCEDURE)
        {
            /* AST TRANSFORMATION: Mark RHS as procedure address if it's a direct procedure reference */
            /* Only transform if BOTH LHS and RHS are actual procedures (not functions) */
            /* Functions should be called, not have their address taken */
            int lhs_is_procedure = (lhs_gpctype->kind == TYPE_KIND_PROCEDURE && 
                                    lhs_gpctype->info.proc_info.return_type == NULL);
            int rhs_is_procedure = (rhs_gpctype->kind == TYPE_KIND_PROCEDURE && 
                                    rhs_gpctype->info.proc_info.return_type == NULL);
            
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

    if (!handled_by_gpctype)
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
            fprintf(stderr,
                "Error on line %d, type mismatch in assignment statement for %s (lhs: %s, rhs: %s)!\n\n",
                stmt->line_num,
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

    /* Clean up owned GpcTypes */
    if (lhs_owned && lhs_gpctype != NULL)
        destroy_gpc_type(lhs_gpctype);
    if (rhs_owned && rhs_gpctype != NULL)
        destroy_gpc_type(rhs_gpctype);

    return return_val;
}

/** PROCEDURE_CALL **/
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val, scope_return, cur_arg, arg_type;
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
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Move",
        semcheck_builtin_move, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Val",
        semcheck_builtin_val, max_scope_lev, &handled_builtin);
    if (handled_builtin)
        return return_val;

    handled_builtin = 0;
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Inc",
        semcheck_builtin_inc, max_scope_lev, &handled_builtin);
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

    mangled_name = MangleFunctionNameFromCallSite(proc_id, args_given, symtab, max_scope_lev);
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

    /* If no exact mangled match, try forward declarations with matching parameter count */
    if (match_count == 0 && overload_candidates != NULL)
    {
        ListNode_t *cur = overload_candidates;
        while (cur != NULL && match_count == 0)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate->hash_type == HASHTYPE_PROCEDURE && 
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
        stmt->stmt_data.procedure_call_data.resolved_proc = resolved_proc;
        
        /* Populate call info to avoid use-after-free when HashNode is freed */
        stmt->stmt_data.procedure_call_data.call_hash_type = resolved_proc->hash_type;
        stmt->stmt_data.procedure_call_data.call_gpc_type = resolved_proc->type;
        stmt->stmt_data.procedure_call_data.is_call_info_valid = 1;
        
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
            stmt->stmt_data.procedure_call_data.call_gpc_type = proc_var->type;
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
            sym_return->hash_type != HASHTYPE_BUILTIN_PROCEDURE)
        {
            fprintf(stderr, "Error on line %d, expected %s to be a procedure or builtin!\n\n",
                stmt->line_num, proc_id);

            ++return_val;
        }

        /***** VERIFY ARGUMENTS USING GPCTYPE ARCHITECTURE *****/
        cur_arg = 0;
        /* Get formal arguments from GpcType instead of deprecated args field */
        true_args = gpc_type_get_procedure_params(sym_return->type);
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
                
                /* ALWAYS resolve both sides to GpcType for proper type checking */
                int expected_type_owned = 0;
                GpcType *expected_gpc_type = resolve_type_from_vardecl(arg_decl, symtab, &expected_type_owned);
                
                int arg_type_owned = 0;
                GpcType *arg_gpc_type = semcheck_resolve_expression_gpc_type(symtab, arg_expr, INT_MAX, NO_MUTATE, &arg_type_owned);
                
                /* Perform type compatibility check using GpcType */
                int types_match = 0;
                if (expected_gpc_type == NULL || arg_gpc_type == NULL)
                {
                    /* Fallback: if we can't get GpcTypes, report error */
                    fprintf(stderr, "Error on line %d, on procedure call %s, argument %d: Unable to resolve types (expected=%p, arg=%p)!\n\n",
                        stmt->line_num, proc_id, cur_arg, (void*)expected_gpc_type, (void*)arg_gpc_type);
                    if (expected_gpc_type != NULL)
                        fprintf(stderr, "  Expected type: %s\n", gpc_type_to_string(expected_gpc_type));
                    if (arg_gpc_type != NULL)
                        fprintf(stderr, "  Argument type: %s\n", gpc_type_to_string(arg_gpc_type));
                    ++return_val;
                }
                else
                {
                    types_match = are_types_compatible_for_assignment(expected_gpc_type, arg_gpc_type, symtab);
                    
                    if (!types_match)
                    {
                        fprintf(stderr, "DEBUG: Type mismatch - expected %s, got %s\n",
                            gpc_type_to_string(expected_gpc_type),
                            gpc_type_to_string(arg_gpc_type));
                    }
                    
                    /* Special AST transformation for procedure parameters */
                    if (types_match && 
                        expected_gpc_type->kind == TYPE_KIND_PROCEDURE &&
                        arg_gpc_type->kind == TYPE_KIND_PROCEDURE &&
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
                if (expected_type_owned && expected_gpc_type != NULL)
                    destroy_gpc_type(expected_gpc_type);
                if (arg_type_owned && arg_gpc_type != NULL)
                    destroy_gpc_type(arg_gpc_type);

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
    while(stmt_list != NULL)
    {
        assert(stmt_list->type == LIST_STMT);

        return_val += semcheck_stmt_main(symtab,
            (struct Statement *)stmt_list->cur, max_scope_lev);

        stmt_list = stmt_list->next;
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
    GpcType *for_gpc_type = NULL;
    GpcType *to_gpc_type = NULL;

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
        for_gpc_type = semcheck_resolve_expression_gpc_type(symtab, for_var,
            max_scope_lev, BOTH_MUTATE_REFERENCE, &for_type_owned);
        if (for_type == UNKNOWN_TYPE && for_gpc_type != NULL)
            for_type = gpc_type_get_legacy_tag(for_gpc_type);
    }

    return_val += semcheck_expr_main(&to_type, symtab, to_expr, INT_MAX, NO_MUTATE);
    to_gpc_type = semcheck_resolve_expression_gpc_type(symtab, to_expr,
        INT_MAX, NO_MUTATE, &to_type_owned);
    if (to_type == UNKNOWN_TYPE && to_gpc_type != NULL)
        to_type = gpc_type_get_legacy_tag(to_gpc_type);

    int bounds_compatible = 1;
    if (for_type == UNKNOWN_TYPE && for_gpc_type == NULL)
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
        else if (for_gpc_type != NULL && to_gpc_type != NULL &&
            are_types_compatible_for_assignment(for_gpc_type, to_gpc_type, symtab))
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

    if (for_gpc_type != NULL && !is_ordinal_type(for_type))
    {
        int legacy = gpc_type_get_legacy_tag(for_gpc_type);
        if (!is_ordinal_type(legacy))
        {
            fprintf(stderr, "Error on line %d, expected ordinal type in \"for\" assignment!\n\n",
                stmt->line_num);
            ++return_val;
        }
    }

    semcheck_loop_depth++;
    return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);
    semcheck_loop_depth--;

    if (for_type_owned && for_gpc_type != NULL)
        destroy_gpc_type(for_gpc_type);
    if (to_type_owned && to_gpc_type != NULL)
        destroy_gpc_type(to_gpc_type);

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
    GpcType *lhs_gpc_type = NULL;
    GpcType *rhs_gpc_type = NULL;

    assert(symtab != NULL);
    assert(for_assign != NULL);
    assert(for_assign->type == STMT_VAR_ASSIGN);

    return_val = 0;

    var = for_assign->stmt_data.var_assign_data.var;
    expr = for_assign->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    return_val += semcheck_expr_main(&type_first, symtab, var, max_scope_lev, BOTH_MUTATE_REFERENCE);
    return_val += semcheck_expr_main(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

    lhs_gpc_type = semcheck_resolve_expression_gpc_type(symtab, var, max_scope_lev,
        BOTH_MUTATE_REFERENCE, &lhs_owned);
    rhs_gpc_type = semcheck_resolve_expression_gpc_type(symtab, expr, INT_MAX,
        NO_MUTATE, &rhs_owned);

    if (type_first == UNKNOWN_TYPE && lhs_gpc_type != NULL)
        type_first = gpc_type_get_legacy_tag(lhs_gpc_type);
    if (type_second == UNKNOWN_TYPE && rhs_gpc_type != NULL)
        type_second = gpc_type_get_legacy_tag(rhs_gpc_type);

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
        else if (lhs_gpc_type != NULL && rhs_gpc_type != NULL &&
            are_types_compatible_for_assignment(lhs_gpc_type, rhs_gpc_type, symtab))
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

    if (lhs_owned && lhs_gpc_type != NULL)
        destroy_gpc_type(lhs_gpc_type);
    if (rhs_owned && rhs_gpc_type != NULL)
        destroy_gpc_type(rhs_gpc_type);

    return return_val;
}
