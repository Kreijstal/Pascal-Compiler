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

static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value)
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
            long long value;
            if (evaluate_const_expr(symtab, expr->expr_data.sign_term, &value) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_ADDOP:
        {
            long long left, right;
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
            long long left, right;
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
        else
            var_type = HASHVAR_INTEGER;

        struct RecordType *record_info = NULL;
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            record_info = tree->tree_data.type_decl_data.info.record;

        func_return = PushTypeOntoScope(symtab, tree->tree_data.type_decl_data.id, var_type, record_info);

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

        long long value = 0;
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
    char *integer_name = strdup("integer");
    if (integer_name != NULL) {
        AddBuiltinType(symtab, integer_name, HASHVAR_INTEGER);
        free(integer_name);
    }
    char *longint_name = strdup("longint");
    if (longint_name != NULL) {
        AddBuiltinType(symtab, longint_name, HASHVAR_LONGINT);
        free(longint_name);
    }
    char *real_name = strdup("real");
    if (real_name != NULL) {
        AddBuiltinType(symtab, real_name, HASHVAR_REAL);
        free(real_name);
    }
    char *single_name = strdup("single");
    if (single_name != NULL) {
        AddBuiltinType(symtab, single_name, HASHVAR_REAL);
        free(single_name);
    }
    char *string_name = strdup("string");
    if (string_name != NULL) {
        AddBuiltinType(symtab, string_name, HASHVAR_PCHAR);
        free(string_name);
    }

    char *setlength_name = strdup("SetLength");
    if (setlength_name != NULL) {
        AddBuiltinProc(symtab, setlength_name, NULL);
        free(setlength_name);
    }

    /* Builtins are now in stdlib.p */
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
    ListNode_t *cur, *ids;
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
            ids = tree->tree_data.var_decl_data.ids;
        else
            ids = tree->tree_data.arr_decl_data.ids;

        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    HashNode_t *type_node;
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
                    }
                }
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else
                    var_type = HASHVAR_REAL;

                func_return = PushVarOntoScope(symtab, var_type, (char *)ids->cur);
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

                int element_size;
                if (var_type == HASHVAR_REAL)
                    element_size = 8;
                else if (var_type == HASHVAR_LONGINT)
                    element_size = 8;
                else
                    element_size = 4;
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
        else if(subprogram->tree_data.subprogram_data.return_type == INT_TYPE || subprogram->tree_data.subprogram_data.return_type == LONGINT_TYPE)
            var_type = HASHVAR_INTEGER;
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
        if(hash_return->mutated == NO_MUTATE)
        {
            fprintf(stderr,
                "Error in function %s: no return statement declared in function body!\n\n",
                subprogram->tree_data.subprogram_data.id);
            ++return_val;
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
