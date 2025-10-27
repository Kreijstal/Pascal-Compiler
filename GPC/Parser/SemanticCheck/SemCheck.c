/*
    Damon Gwinn
    Performs semantic checking on a given parse tree

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    TODO: CHECK FOR RETURN IN FUNCTIONS (Add a "referenced" flag to symbol table elements. Need it for optimizations anyway...)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include "SemCheck.h"
#include "../../flags.h"
#include "../../Optimizer/optimizer.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"
#include "../ParseTree/type_tags.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"

/* Adds built-in functions */
void semcheck_add_builtins(SymTab_t *symtab);

static enum VarType parser_type_to_var_type(int parser_type)
{
    switch (parser_type)
    {
        case INT_TYPE:
            return HASHVAR_INTEGER;
        case LONGINT_TYPE:
            return HASHVAR_LONGINT;
        case REAL_TYPE:
            return HASHVAR_REAL;
        case STRING_TYPE:
            return HASHVAR_PCHAR;
        default:
            return HASHVAR_UNTYPED;
    }
}

static int statement_contains_pascal_code(struct Statement *stmt)
{
    if (stmt == NULL)
        return 0;

    switch (stmt->type)
    {
        case STMT_ASM_BLOCK:
            return 0;
        case STMT_COMPOUND_STATEMENT:
        {
            ListNode_t *cur = stmt->stmt_data.compound_statement;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT &&
                    statement_contains_pascal_code((struct Statement *)cur->cur))
                {
                    return 1;
                }
                cur = cur->next;
            }
            return 0;
        }
        default:
            return 1;
    }
}

static int parser_type_to_size(int parser_type)
{
    switch (parser_type)
    {
        case REAL_TYPE:
            return 8;
        case STRING_TYPE:
            return 8;
        case INT_TYPE:
            return 4;
        case LONGINT_TYPE:
            return 8;
        default:
            return 4;
    }
}

/* Main is a special keyword at the moment for code generation */
int semcheck_id_not_main(char *id)
{
    if(strcmp(id, "main") == 0)
    {
        fprintf(stderr, "ERROR: main is special keyword, it cannot be a program, or subprogram\n");
        return 1;
    }

    return 0;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev);

static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, int *out_value)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_INUM:
            *out_value = expr->expr_data.i_num;
            return 0;
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL && node->hash_type == HASHTYPE_CONST)
            {
                *out_value = node->const_int_value;
                return 0;
            }
            fprintf(stderr, "Error: constant %s is undefined or not a const.\n", expr->expr_data.id);
            return 1;
        }
        case EXPR_SIGN_TERM:
        {
            int value;
            if (evaluate_const_expr(symtab, expr->expr_data.sign_term, &value) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_ADDOP:
        {
            int left, right;
            if (evaluate_const_expr(symtab, expr->expr_data.addop_data.left_expr, &left) != 0)
                return 1;
            if (evaluate_const_expr(symtab, expr->expr_data.addop_data.right_term, &right) != 0)
                return 1;
            switch (expr->expr_data.addop_data.addop_type)
            {
                case PLUS:
                    *out_value = left + right;
                    return 0;
                case MINUS:
                    *out_value = left - right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_MULOP:
        {
            int left, right;
            if (evaluate_const_expr(symtab, expr->expr_data.mulop_data.left_term, &left) != 0)
                return 1;
            if (evaluate_const_expr(symtab, expr->expr_data.mulop_data.right_factor, &right) != 0)
                return 1;
            switch (expr->expr_data.mulop_data.mulop_type)
            {
                case STAR:
                    *out_value = left * right;
                    return 0;
                case DIV:
                    if (right == 0)
                    {
                        fprintf(stderr, "Error: division by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left / right;
                    return 0;
                case MOD:
                    if (right == 0)
                    {
                        fprintf(stderr, "Error: modulo by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left % right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        default:
            break;
    }

    fprintf(stderr, "Error: unsupported const expression.\n");
    return 1;
}

/* The main function for checking a tree */
/* Return values:
    0       -> Check successful
    -1      -> Check successful with warnings
    >= 1    -> Check failed with n errors
*/
SymTab_t *start_semcheck(Tree_t *parse_tree, int *sem_result)
{
    SymTab_t *symtab;
    int return_val;

    assert(parse_tree != NULL);
    assert(sem_result != NULL);

    symtab = InitSymTab();
    semcheck_add_builtins(symtab);
    /*PrintSymTab(symtab, stderr, 0);*/

    return_val = semcheck_program(symtab, parse_tree);

    if(return_val > 0)
        fprintf(stderr, "\nCheck failed with %d error(s)!\n\n", return_val);
    else
        fprintf(stderr, "\nCheck successful!\n\n");

    *sem_result = return_val;
    return symtab;
}

/* Pushes a bunch of type declarations onto the current scope */
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls)
{
    ListNode_t *cur;
    Tree_t *tree;
    int return_val, func_return;
    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = type_decls;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_TYPE_DECL);

        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            var_type = HASHVAR_RECORD;
        else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
            if (alias->is_array)
            {
                if (alias->array_element_type != UNKNOWN_TYPE)
                    var_type = parser_type_to_var_type(alias->array_element_type);
                else if (alias->array_element_type_id != NULL)
                {
                    HashNode_t *resolved_alias = NULL;
                    if (FindIdent(&resolved_alias, symtab, alias->array_element_type_id) != -1 && resolved_alias != NULL)
                        var_type = resolved_alias->var_type;
                    else
                        var_type = HASHVAR_UNTYPED;
                }
                else
                    var_type = HASHVAR_UNTYPED;
            }
            else if (alias->base_type != UNKNOWN_TYPE)
            {
                var_type = parser_type_to_var_type(alias->base_type);
                if (var_type == HASHVAR_UNTYPED && alias->target_type_id != NULL)
                {
                    HashNode_t *resolved_alias = NULL;
                    if (FindIdent(&resolved_alias, symtab, alias->target_type_id) != -1 && resolved_alias != NULL)
                        var_type = resolved_alias->var_type;
                }
            }
            else
                var_type = HASHVAR_INTEGER;
        }
        else
            var_type = HASHVAR_INTEGER;

        struct RecordType *record_info = NULL;
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            record_info = tree->tree_data.type_decl_data.info.record;

        func_return = PushTypeOntoScope(symtab, tree->tree_data.type_decl_data.id, var_type, record_info);

        if (func_return == 0 && tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, tree->tree_data.type_decl_data.id) != -1 && type_node != NULL)
            {
                struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                type_node->is_type_alias = 1;
                type_node->alias_is_array = alias->is_array;
                type_node->alias_array_start = alias->array_start;
                type_node->alias_array_end = alias->array_end;
                type_node->alias_is_dynamic = alias->is_open_array;
                type_node->alias_element_type = parser_type_to_var_type(alias->array_element_type);
                if (alias->array_element_type == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                {
                    if (type_node->alias_element_type_id != NULL)
                        free(type_node->alias_element_type_id);
                    type_node->alias_element_type_id = strdup(alias->array_element_type_id);
                }
                else
                {
                    if (type_node->alias_element_type_id != NULL)
                    {
                        free(type_node->alias_element_type_id);
                        type_node->alias_element_type_id = NULL;
                    }
                }

                if (type_node->alias_is_array)
                {
                    int alias_element_size = 0;
                    if (alias->array_element_type != UNKNOWN_TYPE)
                        alias_element_size = parser_type_to_size(alias->array_element_type);

                    if (alias_element_size == 0 && alias->array_element_type_id != NULL)
                    {
                        HashNode_t *resolved_alias = NULL;
                        if (FindIdent(&resolved_alias, symtab, alias->array_element_type_id) != -1 && resolved_alias != NULL)
                        {
                            if (resolved_alias->alias_is_array && resolved_alias->alias_element_size > 0)
                                alias_element_size = resolved_alias->alias_element_size;
                            else if (resolved_alias->var_type == HASHVAR_REAL ||
                                     resolved_alias->var_type == HASHVAR_LONGINT)
                                alias_element_size = 8;
                            else if (resolved_alias->var_type == HASHVAR_INTEGER)
                                alias_element_size = 4;
                        }
                    }

                    if (alias_element_size == 0)
                    {
                        if (type_node->alias_element_type == HASHVAR_REAL ||
                            type_node->alias_element_type == HASHVAR_LONGINT)
                            alias_element_size = 8;
                        else
                            alias_element_size = 4;
                    }

                    type_node->alias_element_size = alias_element_size;
                }
                else
                {
                    type_node->alias_element_size = 0;
                }
            }
        }

        if(func_return > 0)
        {
            fprintf(stderr, "Error on line %d, redeclaration of name %s!\n",
                tree->line_num, tree->tree_data.type_decl_data.id);
            return_val += func_return;
        }

        cur = cur->next;
    }

    return return_val;
}

int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls)
{
    ListNode_t *cur = const_decls;
    int return_val = 0;

    while (cur != NULL)
    {
        assert(cur->type == LIST_TREE);
        Tree_t *tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_CONST_DECL);

        int value = 0;
        if (evaluate_const_expr(symtab, tree->tree_data.const_decl_data.value, &value) != 0)
        {
            fprintf(stderr, "Error on line %d, unsupported const expression.\n", tree->line_num);
            ++return_val;
        }
        else if (PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, value) > 0)
        {
            fprintf(stderr, "Error on line %d, redeclaration of const %s!\n",
                    tree->line_num, tree->tree_data.const_decl_data.id);
            ++return_val;
        }

        cur = cur->next;
    }

    return return_val;
}

/* Adds built-in functions */
/*TODO: these should be defined in pascal not in semantic analyzer */
void semcheck_add_builtins(SymTab_t *symtab)
{
    char *pchar_name = strdup("PChar");
    if (pchar_name != NULL) {
        AddBuiltinType(symtab, pchar_name, HASHVAR_PCHAR);
        free(pchar_name);
    }
    char *string_name = strdup("string");
    if (string_name != NULL) {
        AddBuiltinType(symtab, string_name, HASHVAR_PCHAR);
        free(string_name);
    }

    /* Builtins are now in stdlib.p */

    ListNode_t *setlength_args = NULL;
    ListNode_t *arr_ids = CreateListNode(strdup("__arr"), LIST_STRING);
    Tree_t *arr_param = mk_vardecl(0, arr_ids, BUILTIN_ANY_TYPE, NULL, 1, 0, NULL);
    setlength_args = CreateListNode(arr_param, LIST_TREE);

    ListNode_t *len_ids = CreateListNode(strdup("__len"), LIST_STRING);
    Tree_t *len_param = mk_vardecl(0, len_ids, INT_TYPE, NULL, 0, 0, NULL);
    PushListNodeBack(setlength_args, CreateListNode(len_param, LIST_TREE));

    AddBuiltinProc(symtab, strdup("SetLength"), setlength_args);
}

/* Semantic check for a program */
int semcheck_program(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_PROGRAM_TYPE);

    return_val = 0;

    PushScope(symtab);

    return_val += semcheck_id_not_main(tree->tree_data.program_data.program_id);

    /* TODO: Push program name onto scope */

    /* TODO: Fix line number bug here */
    return_val += semcheck_args(symtab, tree->tree_data.program_data.args_char,
      tree->line_num);

    return_val += semcheck_const_decls(symtab, tree->tree_data.program_data.const_declaration);
    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
    return_val += semcheck_decls(symtab, tree->tree_data.program_data.var_declaration);

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0);

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, 0);

    if(optimize_flag() > 0 && return_val == 0)
    {
        optimize(symtab, tree);
    }

    /* Keep the outermost scope alive for code generation. DestroySymTab will clean it up. */
    return return_val;
}


/* Adds arguments to the symbol table */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num)
{
    ListNode_t *cur;
    int return_val, func_return;
    assert(symtab != NULL);

    return_val = 0;

    cur = args;

    /* Checking if they are declarations
        NOTE: Mismatching arg types is an error */
    if(cur != NULL)
        if(cur->type == LIST_TREE)
            return semcheck_decls(symtab, args);

    while(cur != NULL)
    {
        /* If not a list of declarations, must be a list of strings */
        assert(cur->type == LIST_STRING);

        func_return = PushVarOntoScope(symtab, HASHVAR_UNTYPED, (char *)cur->cur);

        /* Greater than 0 signifies an error */
        if(func_return > 0)
        {
            fprintf(stderr, "Error on line %d, redeclaration of name %s!\n",
                line_num, (char *)cur->cur);
            return_val += func_return;
        }

        cur = cur->next;
    }

    return return_val;
}

/* Pushes a bunch of declarations onto the current scope */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls)
{
    ListNode_t *cur, *ids, *ids_head;
    Tree_t *tree;
    int return_val, func_return;

    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = decls;
    while(cur != NULL)
    {
        /* Any declaration is always a tree */
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_VAR_DECL || tree->type == TREE_ARR_DECL);

        if (tree->type == TREE_VAR_DECL)
            ids_head = tree->tree_data.var_decl_data.ids;
        else
            ids_head = tree->tree_data.arr_decl_data.ids;

        ids = ids_head;

        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                int is_alias_array = 0;
                HashNode_t *type_node = NULL;
                int alias_element_var_type = HASHVAR_UNTYPED;
                int alias_element_size = 4;
                int alias_start = 0;
                int alias_end = -1;
                int alias_is_dynamic = 0;

                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    if (FindIdent(&type_node, symtab, tree->tree_data.var_decl_data.type_id) == -1)
                    {
                        fprintf(stderr, "Error on line %d, undefined type %s!\n",
                            tree->line_num, tree->tree_data.var_decl_data.type_id);
                        return_val++;
                        var_type = HASHVAR_UNTYPED;
                    }
                    else
                    {
                        var_type = type_node->var_type;
                        if (type_node->is_type_alias && type_node->alias_is_array)
                        {
                            is_alias_array = 1;
                            alias_start = type_node->alias_array_start;
                            alias_end = type_node->alias_array_end;
                            alias_is_dynamic = type_node->alias_is_dynamic;
                            alias_element_var_type = type_node->alias_element_type;
                            if (alias_element_var_type == HASHVAR_UNTYPED && type_node->alias_element_type_id != NULL)
                            {
                                HashNode_t *resolved_alias = NULL;
                                if (FindIdent(&resolved_alias, symtab, type_node->alias_element_type_id) != -1 && resolved_alias != NULL)
                                    alias_element_var_type = resolved_alias->var_type;
                            }
                            if (alias_element_var_type == HASHVAR_UNTYPED)
                                alias_element_var_type = var_type;

                            if (type_node->alias_element_size > 0)
                                alias_element_size = type_node->alias_element_size;
                            else if (alias_element_var_type == HASHVAR_REAL)
                                alias_element_size = 8;
                            else
                                alias_element_size = 4;

                            tree->tree_data.var_decl_data.is_alias_array = 1;
                            tree->tree_data.var_decl_data.alias_array_is_dynamic = alias_is_dynamic;
                            tree->tree_data.var_decl_data.alias_array_start = alias_start;
                            tree->tree_data.var_decl_data.alias_array_end = alias_end;
                            tree->tree_data.var_decl_data.alias_element_type = alias_element_var_type;
                            tree->tree_data.var_decl_data.alias_element_size = alias_element_size;
                        }
                    }
                }
                else if (tree->tree_data.var_decl_data.inferred_type)
                    var_type = HASHVAR_UNTYPED;
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else
                    var_type = HASHVAR_REAL;

                if (is_alias_array)
                {
                    func_return = PushArrayOntoScope(symtab, alias_element_var_type, (char *)ids->cur,
                        alias_start, alias_end, alias_element_size);
                }
                else
                {
                    func_return = PushVarOntoScope(symtab, var_type, (char *)ids->cur);
                }
            }
            /* Array declarations */
            else
            {
                assert(tree->type == TREE_ARR_DECL);
                if(tree->tree_data.arr_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.arr_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else
                    var_type = HASHVAR_REAL;

                int element_size = (var_type == HASHVAR_REAL || var_type == HASHVAR_LONGINT) ? 8 : 4;
                func_return = PushArrayOntoScope(symtab, var_type, (char *)ids->cur,
                    tree->tree_data.arr_decl_data.s_range, tree->tree_data.arr_decl_data.e_range, element_size);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                fprintf(stderr, "Error on line %d, redeclaration of name %s!\n",
                    tree->line_num, (char *)ids->cur);
                return_val += func_return;
            }

            ids = ids->next;
        }

        cur = cur->next;

        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.initializer != NULL)
        {
            if (ids_head == NULL || ids_head->next != NULL)
            {
                fprintf(stderr, "Error on line %d, type inference initializers must declare a single identifier.\n",
                    tree->line_num);
                ++return_val;
            }
            else
            {
                char *var_name = (char *)ids_head->cur;
                HashNode_t *var_node = NULL;
                if (FindIdent(&var_node, symtab, var_name) == -1 || var_node == NULL)
                {
                    fprintf(stderr, "Error on line %d, failed to resolve variable %s for initializer.\n",
                        tree->line_num, var_name);
                    ++return_val;
                }
                else
                {
                    struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
                    struct Expression *init_expr = init_stmt->stmt_data.var_assign_data.expr;
                    int expr_type = UNKNOWN_TYPE;
                    return_val += semcheck_expr_main(&expr_type, symtab, init_expr, INT_MAX, NO_MUTATE);

                    if (expr_type == UNKNOWN_TYPE)
                    {
                        fprintf(stderr, "Error on line %d, unable to infer type for %s.\n", tree->line_num, var_name);
                        ++return_val;
                    }
                    else
                    {
                        enum VarType inferred_var_type = HASHVAR_UNTYPED;
                        int normalized_type = expr_type;

                        switch(expr_type)
                        {
                            case INT_TYPE:
                            case BOOL:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = INT_TYPE;
                                break;
                            case LONGINT_TYPE:
                                inferred_var_type = HASHVAR_LONGINT;
                                normalized_type = LONGINT_TYPE;
                                break;
                            case REAL_TYPE:
                                inferred_var_type = HASHVAR_REAL;
                                normalized_type = REAL_TYPE;
                                break;
                            case STRING_TYPE:
                                inferred_var_type = HASHVAR_PCHAR;
                                normalized_type = STRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("string");
                                break;
                            default:
                                fprintf(stderr, "Error on line %d, unsupported inferred type for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                                inferred_var_type = HASHVAR_UNTYPED;
                                break;
                        }

                        if (tree->tree_data.var_decl_data.inferred_type)
                        {
                            if (inferred_var_type != HASHVAR_UNTYPED)
                            {
                                tree->tree_data.var_decl_data.type = normalized_type;
                                var_node->var_type = inferred_var_type;
                            }
                        }
                        else
                        {
                            if (inferred_var_type != var_node->var_type)
                            {
                                fprintf(stderr, "Error on line %d, initializer type mismatch for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                            }
                        }
                    }
                }
            }
        }

    }

    return return_val;
}

/* Semantic check on an entire subprogram */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev)
{
    int return_val, func_return;
    int new_max_scope;
    enum VarType var_type;
    enum TreeType sub_type;
    HashNode_t *hash_return;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    // --- Name Mangling Logic ---
    if (subprogram->tree_data.subprogram_data.cname_flag) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(subprogram->tree_data.subprogram_data.id);
    } else {
        // Pass the symbol table to the mangler
        subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab); // <-- PASS symtab HERE
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;


    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        // Use the original name for the external symbol, to support overloading
        func_return = PushProcedureOntoScope(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        subprogram->tree_data.subprogram_data.args_var);

        PushScope(symtab);
        // Push it again in the new scope to allow recursion
        PushProcedureOntoScope(symtab, id_to_use_for_lookup,
            subprogram->tree_data.subprogram_data.mangled_id,
            subprogram->tree_data.subprogram_data.args_var);

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        /* Need to additionally extract the return type */
        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
        {
            HashNode_t *type_node;
            if (FindIdent(&type_node, symtab, subprogram->tree_data.subprogram_data.return_type_id) == -1)
            {
                fprintf(stderr, "Error on line %d, undefined type %s!\n",
                    subprogram->line_num, subprogram->tree_data.subprogram_data.return_type_id);
                return_val++;
                var_type = HASHVAR_UNTYPED;
            }
            else
            {
                var_type = type_node->var_type;
            }
        }
        else if(subprogram->tree_data.subprogram_data.return_type == INT_TYPE)
            var_type = HASHVAR_INTEGER;
        else if(subprogram->tree_data.subprogram_data.return_type == LONGINT_TYPE)
            var_type = HASHVAR_LONGINT;
        else
            var_type = HASHVAR_REAL;

        // Use the original name for the external symbol, to support overloading
        func_return = PushFunctionOntoScope(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        var_type, subprogram->tree_data.subprogram_data.args_var);

        PushScope(symtab);
        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable.
        PushFuncRetOntoScope(symtab, subprogram->tree_data.subprogram_data.id,
            var_type, subprogram->tree_data.subprogram_data.args_var);

        new_max_scope = 0;
    }

    /**** Check the subprogram internals now ****/

    /* Greater than 0 signifies an error */
    if(func_return > 0)
    {
        fprintf(stderr, "On line %d: redeclaration of name %s!\n",
            subprogram->line_num, subprogram->tree_data.subprogram_data.id);

        return_val += func_return;
    }

    /* These arguments are themselves like declarations */
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.args_var);

    return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);

    return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                    new_max_scope);

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        return_val += semcheck_stmt(symtab,
                subprogram->tree_data.subprogram_data.statement_list,
                new_max_scope);
    }
    else
    {
        assert(FindIdent(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    == 0);

        ResetHashNodeStatus(hash_return);
        return_val += semcheck_func_stmt(symtab,
                subprogram->tree_data.subprogram_data.statement_list);
        int has_pascal_body = statement_contains_pascal_code(
            subprogram->tree_data.subprogram_data.statement_list);
        subprogram->tree_data.subprogram_data.has_pascal_body = has_pascal_body;
        if(hash_return->mutated == NO_MUTATE && has_pascal_body)
        {
            fprintf(stderr,
                "Error in function %s: no return statement declared in function body!\n\n",
                subprogram->tree_data.subprogram_data.id);
            ++return_val;
        }
        else if (!has_pascal_body)
        {
            subprogram->tree_data.subprogram_data.has_pascal_body = 0;
        }
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        optimize(symtab, subprogram);
    }

    PopScope(symtab);
    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

    return_val = 0;
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        return_val += semcheck_subprogram(symtab, (Tree_t *)cur->cur, max_scope_lev);
        cur = cur->next;
    }

    return return_val;
}
