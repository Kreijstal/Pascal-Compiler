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
#include "../../../identifier_utils.h"

int semcheck_stmt_main(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);

int semcheck_varassign(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_proccall(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_compoundstmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_ifthen(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_while(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_repeat(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev);
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev);

static int var_type_to_expr_type(enum VarType var_type)
{
    switch (var_type)
    {
        case HASHVAR_INTEGER:
            return INT_TYPE;
        case HASHVAR_LONGINT:
            return LONGINT_TYPE;
        case HASHVAR_REAL:
            return REAL_TYPE;
        case HASHVAR_PCHAR:
            return STRING_TYPE;
        case HASHVAR_BOOLEAN:
            return BOOL;
        default:
            return UNKNOWN_TYPE;
    }
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
            if (array_node->hash_type != HASHTYPE_ARRAY || !array_node->is_dynamic_array)
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

static int semcheck_builtin_inc(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    if (symtab == NULL || stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    if (args == NULL)
    {
        fprintf(stderr, "Error on line %d, Inc expects at least one argument.\n", stmt->line_num);
        return 1;
    }

    struct Expression *target_expr = (struct Expression *)args->cur;
    struct Expression *amount_expr = (args->next != NULL) ? (struct Expression *)args->next->cur : NULL;

    if (args->next != NULL && args->next->next != NULL)
    {
        fprintf(stderr, "Error on line %d, Inc expects at most two arguments.\n", stmt->line_num);
        return 1;
    }

    int error_count = 0;
    int target_type = UNKNOWN_TYPE;

    if (target_expr == NULL)
    {
        fprintf(stderr, "Error on line %d, Inc target argument is missing.\n", stmt->line_num);
        return 1;
    }

    error_count += semcheck_expr_main(&target_type, symtab, target_expr, max_scope_lev, MUTATE);
    if (target_type != INT_TYPE && target_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, Inc target must be an integer variable.\n", stmt->line_num);
        error_count++;
    }

    if (amount_expr != NULL)
    {
        int amount_type = UNKNOWN_TYPE;
        error_count += semcheck_expr_main(&amount_type, symtab, amount_expr, max_scope_lev, NO_MUTATE);
        if (amount_type != INT_TYPE && amount_type != LONGINT_TYPE)
        {
            fprintf(stderr, "Error on line %d, Inc amount must be an integer expression.\n", stmt->line_num);
            error_count++;
        }
    }

    return error_count;
}

static int semcheck_builtin_write_like(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    int return_val = 0;
    if (stmt == NULL)
        return 0;

    ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
    int arg_index = 1;
    while (args != NULL)
    {
        struct Expression *expr = (struct Expression *)args->cur;
        int expr_type = UNKNOWN_TYPE;
        return_val += semcheck_expr_main(&expr_type, symtab, expr, INT_MAX, NO_MUTATE);

        if (expr_type != INT_TYPE && expr_type != LONGINT_TYPE && expr_type != STRING_TYPE && expr_type != BOOL)
        {
            fprintf(stderr, "Error on line %d, write argument %d must be integer, longint, boolean, or string.\n",
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

/* Semantic check on a normal statement */
int semcheck_stmt(SymTab_t *symtab, struct Statement *stmt, int max_scope_lev)
{
    return semcheck_stmt_main(symtab, stmt, max_scope_lev);
}

/* Semantic check on a function statement (no side effects allowed) */
int semcheck_func_stmt(SymTab_t *symtab, struct Statement *stmt)
{
    return semcheck_stmt_main(symtab, stmt, 0);
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
            /* BREAK is validated during code generation when loop context is available. */
            break;

        case STMT_ASM_BLOCK:
            /* No semantic checking needed for asm blocks */
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
        case PROCEDURE:
            return "procedure";
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

    if(type_first != type_second)
    {
        if (!((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
              (type_first == INT_TYPE && type_second == LONGINT_TYPE)))
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
    }

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
    return_val += try_resolve_builtin_procedure(symtab, stmt, "Inc",
        semcheck_builtin_inc, max_scope_lev, &handled_builtin);
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
                resolved_proc = candidate;
                match_count++;
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
        sym_return = resolved_proc;
        scope_return = 0; // FIXME: This needs to be properly calculated
    }
    else if (match_count == 0)
    {
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

        /***** THEN VERIFY ARGS INSIDE *****/
        cur_arg = 0;
        true_args = sym_return->args;
        while(args_given != NULL && true_args != NULL)
        {
            ++cur_arg;
            assert(args_given->type == LIST_EXPR);
            assert(true_args->type == LIST_TREE);
            return_val += semcheck_expr_main(&arg_type,
                symtab, (struct Expression *)args_given->cur, INT_MAX, NO_MUTATE);

            arg_decl = (Tree_t *)true_args->cur;
            assert(arg_decl->type == TREE_VAR_DECL);
            true_arg_ids = arg_decl->tree_data.var_decl_data.ids;

            while(true_arg_ids != NULL && args_given != NULL)
            {
                int expected_type = arg_decl->tree_data.var_decl_data.type;
                if ((expected_type == -1 || expected_type == UNKNOWN_TYPE) &&
                    arg_decl->tree_data.var_decl_data.type_id != NULL)
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, arg_decl->tree_data.var_decl_data.type_id) != -1 &&
                        type_node != NULL)
                    {
                        expected_type = var_type_to_expr_type(type_node->var_type);
                    }
                }

                if(arg_type != expected_type && expected_type != BUILTIN_ANY_TYPE)
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

    return_val += semcheck_stmt_main(symtab, while_stmt, max_scope_lev);

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
    while (body_list != NULL)
    {
        struct Statement *body_stmt = (struct Statement *)body_list->cur;
        if (body_stmt != NULL)
            return_val += semcheck_stmt_main(symtab, body_stmt, max_scope_lev);
        body_list = body_list->next;
    }

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

    assert(symtab != NULL);
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);

    for_assign_type = stmt->stmt_data.for_data.for_assign_type;
    assert(for_assign_type == STMT_FOR_VAR || for_assign_type == STMT_FOR_ASSIGN_VAR);

    return_val = 0;
    if(for_assign_type == STMT_FOR_VAR)
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
        return_val += semcheck_expr_main(&for_type, symtab, for_var, max_scope_lev, BOTH_MUTATE_REFERENCE);
        /* Check for type */
        if(for_type != INT_TYPE && for_type != LONGINT_TYPE)
        {
            fprintf(stderr, "Error on line %d, expected int in \"for\" assignment!\n\n",
                    stmt->line_num);
            ++return_val;
        }
    }
    else
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        /* For type checked in here */
        return_val += semcheck_for_assign(symtab, for_assign, max_scope_lev);
    }


    to_expr = stmt->stmt_data.for_data.to;
    do_for = stmt->stmt_data.for_data.do_for;

    return_val += semcheck_expr_main(&to_type, symtab, to_expr, INT_MAX, NO_MUTATE);
    if(to_type != INT_TYPE && to_type != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, expected int in \"to\" assignment!\n\n",
                stmt->line_num);
        ++return_val;
    }

    return_val += semcheck_stmt_main(symtab, do_for, max_scope_lev);

    return return_val;
}

/* Essentially the same as the var assignment but with a restriction that it must be an int */
int semcheck_for_assign(SymTab_t *symtab, struct Statement *for_assign, int max_scope_lev)
{
    int return_val;
    int type_first, type_second;
    struct Expression *var, *expr;

    assert(symtab != NULL);
    assert(for_assign != NULL);
    assert(for_assign->type == STMT_VAR_ASSIGN);

    return_val = 0;

    var = for_assign->stmt_data.var_assign_data.var;
    expr = for_assign->stmt_data.var_assign_data.expr;

    /* NOTE: Grammar will make sure the left side is a variable */
    return_val += semcheck_expr_main(&type_first, symtab, var, max_scope_lev, BOTH_MUTATE_REFERENCE);
    return_val += semcheck_expr_main(&type_second, symtab, expr, INT_MAX, NO_MUTATE);

    if(type_first != type_second)
    {
        // Allow integer/longint compatibility
        if (!((type_first == LONGINT_TYPE && type_second == INT_TYPE) ||
              (type_first == INT_TYPE && type_second == LONGINT_TYPE)))
        {
            fprintf(stderr, "Error on line %d, type mismatch in \"for\" assignment statement!\n\n",
                    for_assign->line_num);
            ++return_val;
        }
    }
    if(type_first != INT_TYPE && type_first != LONGINT_TYPE)
    {
        fprintf(stderr, "Error on line %d, expected int in \"for\" assignment statement!\n\n",
                for_assign->line_num);
        ++return_val;
    }

    return return_val;
}
