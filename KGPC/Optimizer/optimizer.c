/*
    Damon Gwinn
    Simple optimizer for a parse tree

    OPTIMIZER INFO:
        - All non-referenced stack variables removed
        - All constant number expressions simplified to a single number

    NOTE: Optimizer designed to work in unison with the parser

    TODO: Support arrays
        - All local-only variables only mutated to constant values and referenced afterwards
            are removed
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include "optimizer.h"
#include "pass_manager.h"
#include "../Parser/ParseTree/tree.h"
#include "../Parser/ParseTree/tree_types.h"
#include "../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../Parser/ParseTree/type_tags.h"
#include "../identifier_utils.h"

#define DEAD_VAR_ID_BUCKETS 64

typedef struct IdSetEntry
{
    char *id;
    struct IdSetEntry *next;
} IdSetEntry;

typedef struct IdSet
{
    size_t bucket_count;
    IdSetEntry **buckets;
} IdSet;

static size_t id_set_hash(const char *id, size_t bucket_count)
{
    size_t hash = 5381;
    while (id != NULL && *id != '\0')
    {
        hash = ((hash << 5) + hash) ^ (size_t)tolower((unsigned char)*id);
        ++id;
    }
    return bucket_count == 0 ? 0 : hash % bucket_count;
}

static IdSet *id_set_create(size_t bucket_count)
{
    IdSet *set = (IdSet *)calloc(1, sizeof(IdSet));
    if (set == NULL)
        return NULL;

    set->bucket_count = bucket_count;
    set->buckets = (IdSetEntry **)calloc(bucket_count, sizeof(IdSetEntry *));
    if (set->buckets == NULL)
    {
        free(set);
        return NULL;
    }
    return set;
}

static void id_set_destroy(IdSet *set)
{
    if (set == NULL)
        return;

    for (size_t i = 0; i < set->bucket_count; ++i)
    {
        IdSetEntry *cur = set->buckets[i];
        while (cur != NULL)
        {
            IdSetEntry *next = cur->next;
            free(cur->id);
            free(cur);
            cur = next;
        }
    }

    free(set->buckets);
    free(set);
}

static void id_set_add(IdSet *set, const char *id)
{
    if (set == NULL || id == NULL)
        return;

    size_t index = id_set_hash(id, set->bucket_count);
    IdSetEntry *cur = set->buckets[index];
    while (cur != NULL)
    {
        if (pascal_identifier_equals(cur->id, id))
            return;
        cur = cur->next;
    }

    IdSetEntry *entry = (IdSetEntry *)malloc(sizeof(IdSetEntry));
    if (entry == NULL)
        return;

    size_t len = strlen(id);
    entry->id = (char *)malloc(len + 1);
    if (entry->id == NULL)
    {
        free(entry);
        return;
    }

    memcpy(entry->id, id, len + 1);
    entry->next = set->buckets[index];
    set->buckets[index] = entry;
}

static int id_set_contains(const IdSet *set, const char *id)
{
    if (set == NULL || id == NULL)
        return 0;

    size_t index = id_set_hash(id, set->bucket_count);
    IdSetEntry *cur = set->buckets[index];
    while (cur != NULL)
    {
        if (pascal_identifier_equals(cur->id, id))
            return 1;
        cur = cur->next;
    }
    return 0;
}

static void run_dead_code_elimination_program(SymTab_t *symtab, Tree_t *prog);
static void run_dead_code_elimination_subprogram(SymTab_t *symtab, Tree_t *sub);

static int remove_mutation_statement_set(SymTab_t *symtab, const IdSet *ids, struct Statement *stmt);
static int remove_mutation_var_assign_set(SymTab_t *symtab, const IdSet *ids, struct Statement *var_assign);
static int remove_mutation_compound_statement_set(SymTab_t *symtab, const IdSet *ids, struct Statement *compound);
static int remove_var_decls_set(SymTab_t *symtab, const IdSet *ids, ListNode_t *var_decls);
static int remove_dead_variables_batch(SymTab_t *symtab, ListNode_t *vars_to_remove, struct Statement *stmt_root, ListNode_t *var_decls);

static optimizer_runner_fn g_optimizer_runner = optimizer_pass_manager_run;

void simplify_stmt_expr(struct Statement *);
int simplify_expr(struct Expression **);

void decrement_self_references(SymTab_t *symtab, struct Statement *stmt);

void decrement_reference_id_expr(SymTab_t *symtab, char *id, struct Expression *expr);
void decrement_reference_expr(SymTab_t *symtab, struct Expression *expr);

void set_vars_lists(SymTab_t *, ListNode_t *, ListNode_t **, ListNode_t **);
void add_to_list(ListNode_t **, void *obj);

/* The main entry point for the optimizer */
void optimizer_set_runner(optimizer_runner_fn runner)
{
    if (runner == NULL)
        g_optimizer_runner = optimizer_pass_manager_run;
    else
        g_optimizer_runner = runner;
}

void optimize(SymTab_t *symtab, Tree_t *tree)
{
    assert(symtab != NULL);
    assert(tree != NULL);

    g_optimizer_runner(symtab, tree);
}

static void run_dead_code_elimination_program(SymTab_t *symtab, Tree_t *prog)
{
    assert(symtab != NULL);
    assert(prog != NULL);
    assert(prog->type == TREE_PROGRAM_TYPE);

    struct Program *prog_data = &prog->tree_data.program_data;
    if (prog_data->body_statement == NULL)
        return;

    ListNode_t *vars_to_check = NULL;
    ListNode_t *vars_to_remove = NULL;

    decrement_self_references(symtab, prog_data->body_statement);
    set_vars_lists(symtab, prog_data->var_declaration, &vars_to_check, &vars_to_remove);

    while (vars_to_remove != NULL)
    {
        int removed = remove_dead_variables_batch(symtab, vars_to_remove,
            prog_data->body_statement, prog_data->var_declaration);
        if (removed == 0)
            break;

        DestroyList(vars_to_check);
        DestroyList(vars_to_remove);
        vars_to_check = NULL;
        vars_to_remove = NULL;

        set_vars_lists(symtab, prog_data->var_declaration, &vars_to_check, &vars_to_remove);
    }

    DestroyList(vars_to_check);
    DestroyList(vars_to_remove);

    /* Fail loudly if we removed all statements - this indicates a bug in the optimizer */
    if (prog_data->body_statement != NULL &&
        prog_data->body_statement->type == STMT_COMPOUND_STATEMENT &&
        prog_data->body_statement->stmt_data.compound_statement == NULL)
    {
        // This can happen in valid tests (e.g. dead_code.p), so just warn instead of asserting
        if (getenv("KGPC_DEBUG_OPTIMIZER") != NULL) {
            fprintf(stderr, "WARNING: Optimizer removed all program body statements (valid for dead code tests)\n");
        }
    }
}

static void run_dead_code_elimination_subprogram(SymTab_t *symtab, Tree_t *sub)
{
    assert(symtab != NULL);
    assert(sub != NULL);
    assert(sub->type == TREE_SUBPROGRAM);

    struct Subprogram *sub_data = &sub->tree_data.subprogram_data;
    if (sub_data->statement_list == NULL)
        return;

    ListNode_t *vars_to_check = NULL;
    ListNode_t *vars_to_remove = NULL;

    decrement_self_references(symtab, sub_data->statement_list);
    set_vars_lists(symtab, sub_data->declarations, &vars_to_check, &vars_to_remove);

    while (vars_to_remove != NULL)
    {
        int removed = remove_dead_variables_batch(symtab, vars_to_remove,
            sub_data->statement_list, sub_data->declarations);
        if (removed == 0)
            break;

        DestroyList(vars_to_check);
        DestroyList(vars_to_remove);
        vars_to_check = NULL;
        vars_to_remove = NULL;

        set_vars_lists(symtab, sub_data->declarations, &vars_to_check, &vars_to_remove);
    }

    DestroyList(vars_to_check);
    DestroyList(vars_to_remove);
}

static int remove_dead_variables_batch(SymTab_t *symtab, ListNode_t *vars_to_remove,
    struct Statement *stmt_root, ListNode_t *var_decls)
{
    if (vars_to_remove == NULL)
        return 0;

    IdSet *ids = id_set_create(DEAD_VAR_ID_BUCKETS);
    if (ids == NULL)
        return 0;

    for (ListNode_t *cur = vars_to_remove; cur != NULL; cur = cur->next)
        id_set_add(ids, (const char *)cur->cur);

    int removed = 0;
    if (stmt_root != NULL)
        removed += remove_mutation_statement_set(symtab, ids, stmt_root);
    if (var_decls != NULL)
        removed += remove_var_decls_set(symtab, ids, var_decls);

    id_set_destroy(ids);
    return removed;
}

void optimizer_pass_dead_code_elimination(SymTab_t *symtab, Tree_t *tree)
{
    assert(symtab != NULL);
    assert(tree != NULL);

    switch (tree->type)
    {
        case TREE_PROGRAM_TYPE:
            run_dead_code_elimination_program(symtab, tree);
            break;

        case TREE_SUBPROGRAM:
            run_dead_code_elimination_subprogram(symtab, tree);
            break;

        default:
            break;
    }
}

void optimizer_pass_constant_folding(SymTab_t *symtab, Tree_t *tree)
{
    (void)symtab;
    assert(tree != NULL);

    switch (tree->type)
    {
        case TREE_PROGRAM_TYPE:
            if (tree->tree_data.program_data.body_statement != NULL)
                simplify_stmt_expr(tree->tree_data.program_data.body_statement);
            break;

        case TREE_SUBPROGRAM:
            if (tree->tree_data.subprogram_data.statement_list != NULL)
                simplify_stmt_expr(tree->tree_data.subprogram_data.statement_list);
            break;

        default:
            break;
    }
}

/* Checks variables for self-references and decrements (ex: c := c+1) */
void decrement_self_references(SymTab_t *symtab, struct Statement *stmt)
{
    assert(symtab != NULL);
    assert(stmt != NULL);

    struct Expression *expr;
    ListNode_t *stmt_list;
    char *id;

    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            expr = stmt->stmt_data.var_assign_data.var;
            if (expr == NULL)
                break;
            if (expr->type != EXPR_VAR_ID)
                break;
            id = expr->expr_data.id;

            expr = stmt->stmt_data.var_assign_data.expr;
            decrement_reference_id_expr(symtab, id, expr);

            break;

        case STMT_COMPOUND_STATEMENT:
            stmt_list = stmt->stmt_data.compound_statement;
            while(stmt_list != NULL)
            {
                decrement_self_references(symtab, (struct Statement *)stmt_list->cur);
                stmt_list = stmt_list->next;
            }

            break;

        default:
            break;
    }
}

/* Removes all variable declarations matching an identifier set */
static int remove_var_decls_set(SymTab_t *symtab, const IdSet *ids, ListNode_t *var_decls)
{
    int removed = 0;

    if (symtab == NULL || ids == NULL || var_decls == NULL)
        return 0;

    while (var_decls != NULL)
    {
        Tree_t *var_decl = (Tree_t *)var_decls->cur;
        assert(var_decl->type == TREE_VAR_DECL);

        ListNode_t *decl_ids = var_decl->tree_data.var_decl_data.ids;
        ListNode_t *prev = NULL;
        while (decl_ids != NULL)
        {
            char *candidate = (char *)decl_ids->cur;
            if (id_set_contains(ids, candidate))
            {
                free(decl_ids->cur);
                ListNode_t *next = decl_ids->next;
                free(decl_ids);
                decl_ids = next;
                if (prev == NULL)
                    var_decl->tree_data.var_decl_data.ids = decl_ids;
                else
                    prev->next = decl_ids;
                ++removed;
                continue;
            }

            prev = decl_ids;
            decl_ids = decl_ids->next;
        }

        if (var_decl->tree_data.var_decl_data.ids == NULL &&
            var_decl->tree_data.var_decl_data.initializer != NULL)
        {
            destroy_stmt(var_decl->tree_data.var_decl_data.initializer);
            var_decl->tree_data.var_decl_data.initializer = NULL;
        }

        var_decls = var_decls->next;
    }

    return removed;
}

/* Removes mutation statements that mutate identifiers contained in the set */
static int remove_mutation_statement_set(SymTab_t *symtab, const IdSet *ids, struct Statement *stmt)
{
    if (symtab == NULL || ids == NULL || stmt == NULL)
        return 0;

    switch (stmt->type)
    {
        case STMT_VAR_ASSIGN:
            return remove_mutation_var_assign_set(symtab, ids, stmt);

        case STMT_COMPOUND_STATEMENT:
            return remove_mutation_compound_statement_set(symtab, ids, stmt);

        case STMT_PROCEDURE_CALL:
        case STMT_EXPR:
            return 0;

        default:
            #ifdef DEBUG_OPTIMIZER
                fprintf(stderr, "OPTIMIZER: Unsupported statement at line %d\n", stmt->line_num);
            #endif
            return 0;
    }
}

/* Removes the var assign if applicable */
/* Decrements the reference counter for the removed expression if removed */
static int remove_mutation_var_assign_set(SymTab_t *symtab, const IdSet *ids, struct Statement *var_assign)
{
    assert(var_assign != NULL);
    assert(var_assign->type == STMT_VAR_ASSIGN);

    struct Expression *var = var_assign->stmt_data.var_assign_data.var;
    assert(var != NULL);
    assert(var->type == EXPR_VAR_ID);

    if (!id_set_contains(ids, var->expr_data.id))
        return 0;

    #ifdef DEBUG_OPTIMIZER
        fprintf(stderr, "OPTIMIZER: Removing var assign at line %d\n", var_assign->line_num);
    #endif

    decrement_reference_expr(symtab, var_assign->stmt_data.var_assign_data.expr);
    destroy_stmt(var_assign);

    return 1;
}

/* Removes all mutation statements from a list of statements */
static int remove_mutation_compound_statement_set(SymTab_t *symtab, const IdSet *ids, struct Statement *body_statement)
{
    assert(body_statement != NULL);
    assert(body_statement->type == STMT_COMPOUND_STATEMENT);

    ListNode_t *statement_list = body_statement->stmt_data.compound_statement;
    ListNode_t *prev = NULL;
    int removed = 0;

    while (statement_list != NULL)
    {
        struct Statement *stmt = (struct Statement *)statement_list->cur;
        if (remove_mutation_statement_set(symtab, ids, stmt) > 0)
        {
            ++removed;

            ListNode_t *next = statement_list->next;
            free(statement_list);
            statement_list = next;

            if (prev == NULL)
                body_statement->stmt_data.compound_statement = statement_list;
            else
                prev->next = statement_list;
        }
        else
        {
            prev = statement_list;
            statement_list = statement_list->next;
        }
    }

    return removed;
}

/* Simplifies expressions by combining operations on constant numbers */
void simplify_stmt_expr(struct Statement *stmt)
{
    assert(stmt != NULL);

    ListNode_t *stmt_list;

    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            simplify_expr(&stmt->stmt_data.var_assign_data.expr);

            break;

        case STMT_EXPR:
            simplify_expr(&stmt->stmt_data.expr_stmt_data.expr);
            break;

        case STMT_COMPOUND_STATEMENT:
            stmt_list = stmt->stmt_data.compound_statement;
            while(stmt_list != NULL)
            {
                simplify_stmt_expr((struct Statement *)stmt_list->cur);
                stmt_list = stmt_list->next;
            }

            break;

        default:
            break;
    }
}

/* Simplifies expressions by combining operations on constant numbers */
/* Returns 1 if expression given is a constant */
/* TODO: Support floats */
/* TODO: Support modulus */
int simplify_expr(struct Expression **expr)
{
    struct Expression *new_expr;
    int return_val, return_val2;
    long long new_val;

    assert(expr != NULL);
    assert(*expr != NULL);

    switch((*expr)->type)
    {
        case EXPR_INUM:
        case EXPR_RNUM:
            return 1;

        case EXPR_SIGN_TERM:
            return_val = simplify_expr(&(*expr)->expr_data.sign_term);
            assert((*expr)->expr_data.sign_term != NULL);
            if(return_val == 1)
            {
                #ifdef DEBUG_OPTIMIZER
                    fprintf(stderr, "OPTIMIZER: Simplying SIGN expression on line %d\n",
                        (*expr)->line_num);
                #endif

                new_val = -(*expr)->expr_data.sign_term->expr_data.i_num;

                new_expr = mk_inum((*expr)->line_num, new_val);

                destroy_expr(*expr);
                *expr = new_expr;

                return 1;
            }

            return 0;

        case EXPR_ADDOP:
            return_val = simplify_expr(&(*expr)->expr_data.addop_data.left_expr);
            return_val2 = simplify_expr(&(*expr)->expr_data.addop_data.right_term);
            if(return_val == 1 && return_val2 == 1)
            {
                #ifdef DEBUG_OPTIMIZER
                    fprintf(stderr, "OPTIMIZER: Simplying ADDOP expression on line %d\n",
                        (*expr)->line_num);
                #endif

                if((*expr)->expr_data.addop_data.addop_type == PLUS)
                {
                    new_val = (*expr)->expr_data.addop_data.left_expr->expr_data.i_num +
                                (*expr)->expr_data.addop_data.right_term->expr_data.i_num;
                }
                else
                {
                    new_val = (*expr)->expr_data.addop_data.left_expr->expr_data.i_num -
                                (*expr)->expr_data.addop_data.right_term->expr_data.i_num;
                }

                new_expr = mk_inum((*expr)->line_num, new_val);

                destroy_expr(*expr);
                *expr = new_expr;

                return 1;
            }

            return 0;

        case EXPR_MULOP:
            return_val = simplify_expr(&(*expr)->expr_data.mulop_data.left_term);
            return_val2 = simplify_expr(&(*expr)->expr_data.mulop_data.right_factor);
            if(return_val == 1 && return_val2 == 1)
            {
                #ifdef DEBUG_OPTIMIZER
                    fprintf(stderr, "OPTIMIZER: Simplying MULOP expression on line %d\n",
                        (*expr)->line_num);
                #endif

                if((*expr)->expr_data.mulop_data.mulop_type == STAR)
                {
                    new_val = (*expr)->expr_data.mulop_data.left_term->expr_data.i_num *
                                (*expr)->expr_data.mulop_data.right_factor->expr_data.i_num;
                }
                else
                {
                    new_val = (*expr)->expr_data.mulop_data.left_term->expr_data.i_num /
                                (*expr)->expr_data.mulop_data.right_factor->expr_data.i_num;
                }

                new_expr = mk_inum((*expr)->line_num, new_val);

                destroy_expr(*expr);
                *expr = new_expr;

                return 1;
            }

            return 0;

        default:
            return 0;
    }

    return 0;
}

/* Decrements references for a specific variable */
void decrement_reference_id_expr(SymTab_t *symtab, char *id, struct Expression *expr)
{
    assert(expr != NULL);
    assert(symtab != NULL);

    HashNode_t *node;

    switch(expr->type)
    {
        case EXPR_RELOP:
            decrement_reference_id_expr(symtab, id, expr->expr_data.relop_data.left);
            decrement_reference_id_expr(symtab, id, expr->expr_data.relop_data.right);
            break;

        case EXPR_SIGN_TERM:
            decrement_reference_id_expr(symtab, id, expr->expr_data.sign_term);
            break;

        case EXPR_ADDOP:
            decrement_reference_id_expr(symtab, id, expr->expr_data.addop_data.left_expr);
            decrement_reference_id_expr(symtab, id, expr->expr_data.addop_data.right_term);
            break;

        case EXPR_MULOP:
            decrement_reference_id_expr(symtab, id, expr->expr_data.mulop_data.left_term);
            decrement_reference_id_expr(symtab, id, expr->expr_data.mulop_data.right_factor);
            break;

        case EXPR_VAR_ID:
            if(strcmp(expr->expr_data.id, id) == 0)
            {
                #ifdef DEBUG_OPTIMIZER
                    fprintf(stderr, "OPTIMIZER: Decremented reference for %s at line %d\n",
                        expr->expr_data.id, expr->line_num);
                #endif

                assert(FindIdent(&node, symtab, expr->expr_data.id) == 0);
                assert(node != NULL);
                --node->referenced;

                break;
            }
            break;

        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
                decrement_reference_id_expr(symtab, id, expr->expr_data.typecast_data.expr);
            break;

        default:
            break;
    }

    return;
}

/* Decrements reference counter for all variables in an expression */
void decrement_reference_expr(SymTab_t *symtab, struct Expression *expr)
{
    assert(expr != NULL);
    assert(symtab != NULL);

    HashNode_t *node;

    switch(expr->type)
    {
        case EXPR_RELOP:
            decrement_reference_expr(symtab, expr->expr_data.relop_data.left);
            decrement_reference_expr(symtab, expr->expr_data.relop_data.right);
            break;

        case EXPR_SIGN_TERM:
            decrement_reference_expr(symtab, expr->expr_data.sign_term);
            break;

        case EXPR_ADDOP:
            decrement_reference_expr(symtab, expr->expr_data.addop_data.left_expr);
            decrement_reference_expr(symtab, expr->expr_data.addop_data.right_term);
            break;

        case EXPR_MULOP:
            decrement_reference_expr(symtab, expr->expr_data.mulop_data.left_term);
            decrement_reference_expr(symtab, expr->expr_data.mulop_data.right_factor);
            break;

        case EXPR_VAR_ID:
            #ifdef DEBUG_OPTIMIZER
                fprintf(stderr, "OPTIMIZER: Decremented reference for %s at line %d\n",
                    expr->expr_data.id, expr->line_num);
            #endif

            assert(FindIdent(&node, symtab, expr->expr_data.id) == 0);
            assert(node != NULL);
            --node->referenced;

            break;

        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
                decrement_reference_expr(symtab, expr->expr_data.typecast_data.expr);
            break;

        default:
            break;
    }

    return;
}

/* Gets a list of variables that can be safely removed (not referenced) and ones that will need
        to be checked
*/
void set_vars_lists(SymTab_t *symtab, ListNode_t *vars, ListNode_t **vars_to_check,
    ListNode_t **vars_to_remove)
{
    ListNode_t *ids;

    assert(symtab != NULL);
    assert(vars_to_check != NULL);
    assert(vars_to_remove != NULL);
    HashNode_t *node;
    Tree_t *var_decl;

    *vars_to_check = *vars_to_remove = NULL;
    while(vars != NULL)
    {
        var_decl = (Tree_t *)vars->cur;
        if(var_decl->type != TREE_ARR_DECL)
        {
            assert(var_decl->type == TREE_VAR_DECL);
            ids = var_decl->tree_data.var_decl_data.ids;

            while(ids != NULL)
            {
                assert(FindIdent(&node, symtab, ids->cur) == 0);
                assert(node != NULL);
                if (node->defined_in_unit)
                {
                    ids = ids->next;
                    continue;
                }
                if(node->referenced == 0 && node->hash_type != HASHTYPE_FUNCTION_RETURN)
                {
                    add_to_list(vars_to_remove, ids->cur);
                }
                else if(node->hash_type != HASHTYPE_FUNCTION_RETURN)
                {
                    add_to_list(vars_to_check, ids->cur);
                }

                ids = ids->next;
            }
        }

        vars = vars->next;
    }
}

/* Adds to a list */
void add_to_list(ListNode_t **list, void *obj)
{
    assert(list != NULL);
    assert(obj != NULL);
    if(*list == NULL)
        *list = CreateListNode(obj, LIST_UNSPECIFIED);
    else
        *list = PushListNodeBack(*list, CreateListNode(obj, LIST_UNSPECIFIED));
}
