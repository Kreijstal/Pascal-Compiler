/*
 * mark_used.c
 * 
 * Implementation of reachability analysis for unused function elimination.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "mark_used.h"
#include "../Parser/ParseTree/tree.h"
#include "../Parser/List/List.h"
#include "../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../identifier_utils.h"

/* Hash table to map mangled_id -> Tree_t *subprogram */
typedef struct SubprogramMapEntry {
    char *mangled_id;
    Tree_t *subprogram;
    struct SubprogramMapEntry *next;
} SubprogramMapEntry;

#define MAP_SIZE 1021  /* Prime number for better distribution */

typedef struct {
    SubprogramMapEntry *table[MAP_SIZE];
} SubprogramMap;

/* Simple hash function for strings */
static unsigned int hash_string(const char *str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash % MAP_SIZE;
}

/* Create a new subprogram map */
static SubprogramMap *create_subprogram_map(void) {
    SubprogramMap *map = (SubprogramMap *)calloc(1, sizeof(SubprogramMap));
    if (map == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate subprogram map\n");
        exit(1);
    }
    return map;
}

/* Add a subprogram to the map */
static void map_add(SubprogramMap *map, const char *mangled_id, Tree_t *subprogram) {
    if (map == NULL || mangled_id == NULL || subprogram == NULL)
        return;
    
    unsigned int index = hash_string(mangled_id);
    SubprogramMapEntry *entry = (SubprogramMapEntry *)malloc(sizeof(SubprogramMapEntry));
    if (entry == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate map entry\n");
        exit(1);
    }
    
    entry->mangled_id = strdup(mangled_id);
    entry->subprogram = subprogram;
    entry->next = map->table[index];
    map->table[index] = entry;
}

/* Look up a subprogram by mangled_id */
static Tree_t *map_lookup(SubprogramMap *map, const char *mangled_id) {
    if (map == NULL || mangled_id == NULL)
        return NULL;
    
    unsigned int index = hash_string(mangled_id);
    SubprogramMapEntry *entry = map->table[index];
    
    while (entry != NULL) {
        if (strcmp(entry->mangled_id, mangled_id) == 0)
            return entry->subprogram;
        entry = entry->next;
    }
    
    return NULL;
}

/* Free the subprogram map */
static void free_subprogram_map(SubprogramMap *map) {
    if (map == NULL)
        return;
    
    for (int i = 0; i < MAP_SIZE; i++) {
        SubprogramMapEntry *entry = map->table[i];
        while (entry != NULL) {
            SubprogramMapEntry *next = entry->next;
            free(entry->mangled_id);
            free(entry);
            entry = next;
        }
    }
    
    free(map);
}

/* Forward declarations */
static void mark_statement_calls(struct Statement *stmt, SubprogramMap *map);
static void mark_expression_calls(struct Expression *expr, SubprogramMap *map);

/* Mark a subprogram and all functions it calls as used */
static void mark_subprogram_used(Tree_t *subprogram, SubprogramMap *map) {
    if (subprogram == NULL || subprogram->type != TREE_SUBPROGRAM)
        return;
    
    /* If already marked, skip to avoid infinite recursion */
    if (subprogram->tree_data.subprogram_data.is_used)
        return;
    
    /* Mark this subprogram as used */
    subprogram->tree_data.subprogram_data.is_used = 1;
    
    /* Traverse the statement list to find calls */
    if (subprogram->tree_data.subprogram_data.statement_list != NULL)
        mark_statement_calls(subprogram->tree_data.subprogram_data.statement_list, map);
    
    /* Traverse nested subprograms */
    ListNode_t *nested = subprogram->tree_data.subprogram_data.subprograms;
    while (nested != NULL) {
        Tree_t *nested_sub = (Tree_t *)nested->cur;
        if (nested_sub != NULL && nested_sub->type == TREE_SUBPROGRAM) {
            /* Only mark nested subprograms if they're called (will be handled by mark_statement_calls) */
        }
        nested = nested->next;
    }
}

/* Mark all calls in a statement */
static void mark_statement_calls(struct Statement *stmt, SubprogramMap *map) {
    if (stmt == NULL)
        return;
    
    switch (stmt->type) {
        case STMT_PROCEDURE_CALL: {
            const char *mangled = stmt->stmt_data.procedure_call_data.mangled_id;
            if (mangled != NULL) {
                Tree_t *called_sub = map_lookup(map, mangled);
                if (called_sub != NULL) {
                    mark_subprogram_used(called_sub, map);
                }
            }
            
            /* Check arguments for function calls */
            ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
            while (args != NULL) {
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg != NULL) {
                    mark_expression_calls(arg, map);
                }
                args = args->next;
            }
            break;
        }
        
        case STMT_VAR_ASSIGN: {
            mark_expression_calls(stmt->stmt_data.var_assign_data.var, map);
            mark_expression_calls(stmt->stmt_data.var_assign_data.expr, map);
            break;
        }
        
        case STMT_COMPOUND_STATEMENT: {
            ListNode_t *stmts = stmt->stmt_data.compound_statement;
            while (stmts != NULL) {
                struct Statement *s = (struct Statement *)stmts->cur;
                if (s != NULL) {
                    mark_statement_calls(s, map);
                }
                stmts = stmts->next;
            }
            break;
        }
        
        case STMT_IF_THEN: {
            mark_expression_calls(stmt->stmt_data.if_then_data.relop_expr, map);
            mark_statement_calls(stmt->stmt_data.if_then_data.if_stmt, map);
            mark_statement_calls(stmt->stmt_data.if_then_data.else_stmt, map);
            break;
        }
        
        case STMT_WHILE: {
            mark_expression_calls(stmt->stmt_data.while_data.relop_expr, map);
            mark_statement_calls(stmt->stmt_data.while_data.while_stmt, map);
            break;
        }
        
        case STMT_REPEAT: {
            ListNode_t *body = stmt->stmt_data.repeat_data.body_list;
            while (body != NULL) {
                struct Statement *s = (struct Statement *)body->cur;
                if (s != NULL) {
                    mark_statement_calls(s, map);
                }
                body = body->next;
            }
            mark_expression_calls(stmt->stmt_data.repeat_data.until_expr, map);
            break;
        }
        
        case STMT_FOR:
        case STMT_FOR_VAR:
        case STMT_FOR_ASSIGN_VAR: {
            if (stmt->stmt_data.for_data.for_assign_type == STMT_VAR_ASSIGN) {
                mark_statement_calls(stmt->stmt_data.for_data.for_assign_data.var_assign, map);
            } else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR ||
                       stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR) {
                mark_expression_calls(stmt->stmt_data.for_data.for_assign_data.var, map);
            }
            mark_expression_calls(stmt->stmt_data.for_data.to, map);
            mark_statement_calls(stmt->stmt_data.for_data.do_for, map);
            break;
        }
        
        case STMT_FOR_IN: {
            mark_expression_calls(stmt->stmt_data.for_in_data.loop_var, map);
            mark_expression_calls(stmt->stmt_data.for_in_data.collection, map);
            mark_statement_calls(stmt->stmt_data.for_in_data.do_stmt, map);
            break;
        }
        
        case STMT_CASE: {
            mark_expression_calls(stmt->stmt_data.case_data.selector_expr, map);
            ListNode_t *branches = stmt->stmt_data.case_data.branches;
            while (branches != NULL) {
                struct CaseBranch *branch = (struct CaseBranch *)branches->cur;
                if (branch != NULL) {
                    mark_statement_calls(branch->stmt, map);
                }
                branches = branches->next;
            }
            mark_statement_calls(stmt->stmt_data.case_data.else_stmt, map);
            break;
        }
        
        case STMT_WITH: {
            mark_expression_calls(stmt->stmt_data.with_data.context_expr, map);
            mark_statement_calls(stmt->stmt_data.with_data.body_stmt, map);
            break;
        }
        
        case STMT_TRY_FINALLY: {
            ListNode_t *try_stmts = stmt->stmt_data.try_finally_data.try_statements;
            while (try_stmts != NULL) {
                struct Statement *s = (struct Statement *)try_stmts->cur;
                mark_statement_calls(s, map);
                try_stmts = try_stmts->next;
            }
            ListNode_t *finally_stmts = stmt->stmt_data.try_finally_data.finally_statements;
            while (finally_stmts != NULL) {
                struct Statement *s = (struct Statement *)finally_stmts->cur;
                mark_statement_calls(s, map);
                finally_stmts = finally_stmts->next;
            }
            break;
        }
        
        case STMT_TRY_EXCEPT: {
            ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
            while (try_stmts != NULL) {
                struct Statement *s = (struct Statement *)try_stmts->cur;
                mark_statement_calls(s, map);
                try_stmts = try_stmts->next;
            }
            ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;
            while (except_stmts != NULL) {
                struct Statement *s = (struct Statement *)except_stmts->cur;
                mark_statement_calls(s, map);
                except_stmts = except_stmts->next;
            }
            break;
        }
        
        case STMT_RAISE: {
            mark_expression_calls(stmt->stmt_data.raise_data.exception_expr, map);
            break;
        }
        
        case STMT_INHERITED: {
            mark_expression_calls(stmt->stmt_data.inherited_data.call_expr, map);
            break;
        }
        
        case STMT_LABEL:
            mark_statement_calls(stmt->stmt_data.label_data.stmt, map);
            break;
        
        case STMT_GOTO:
        case STMT_EXIT:
        case STMT_BREAK:
        case STMT_ASM_BLOCK:
            /* No function calls in these statements */
            break;
        
        default:
            /* Unknown statement type, but don't fail */
            break;
    }
}

/* Mark all calls in an expression */
static void mark_expression_calls(struct Expression *expr, SubprogramMap *map) {
    if (expr == NULL)
        return;
    
    switch (expr->type) {
        case EXPR_FUNCTION_CALL: {
            const char *mangled = expr->expr_data.function_call_data.mangled_id;
            if (mangled != NULL) {
                Tree_t *called_sub = map_lookup(map, mangled);
                if (called_sub != NULL) {
                    mark_subprogram_used(called_sub, map);
                }
            }
            
            /* Check arguments */
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            while (args != NULL) {
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg != NULL) {
                    mark_expression_calls(arg, map);
                }
                args = args->next;
            }
            break;
        }
        
        case EXPR_RELOP:
            mark_expression_calls(expr->expr_data.relop_data.left, map);
            mark_expression_calls(expr->expr_data.relop_data.right, map);
            break;
        
        case EXPR_SIGN_TERM:
            mark_expression_calls(expr->expr_data.sign_term, map);
            break;
        
        case EXPR_ADDOP:
            mark_expression_calls(expr->expr_data.addop_data.left_expr, map);
            mark_expression_calls(expr->expr_data.addop_data.right_term, map);
            break;
        
        case EXPR_MULOP:
            mark_expression_calls(expr->expr_data.mulop_data.left_term, map);
            mark_expression_calls(expr->expr_data.mulop_data.right_factor, map);
            break;
        
        case EXPR_ARRAY_ACCESS:
            mark_expression_calls(expr->expr_data.array_access_data.array_expr, map);
            mark_expression_calls(expr->expr_data.array_access_data.index_expr, map);
            break;
        
        case EXPR_RECORD_ACCESS:
            mark_expression_calls(expr->expr_data.record_access_data.record_expr, map);
            break;
        
        case EXPR_POINTER_DEREF:
            mark_expression_calls(expr->expr_data.pointer_deref_data.pointer_expr, map);
            break;
        
        case EXPR_ADDR:
            mark_expression_calls(expr->expr_data.addr_data.expr, map);
            break;
        
        case EXPR_TYPECAST:
            mark_expression_calls(expr->expr_data.typecast_data.expr, map);
            break;
        
        case EXPR_IS:
            mark_expression_calls(expr->expr_data.is_data.expr, map);
            break;
        
        case EXPR_AS:
            mark_expression_calls(expr->expr_data.as_data.expr, map);
            break;
        
        case EXPR_SET: {
            ListNode_t *elements = expr->expr_data.set_data.elements;
            while (elements != NULL) {
                struct SetElement *elem = (struct SetElement *)elements->cur;
                if (elem != NULL) {
                    mark_expression_calls(elem->lower, map);
                    mark_expression_calls(elem->upper, map);
                }
                elements = elements->next;
            }
            break;
        }
        
        case EXPR_ARRAY_LITERAL: {
            ListNode_t *elements = expr->expr_data.array_literal_data.elements;
            while (elements != NULL) {
                struct Expression *elem = (struct Expression *)elements->cur;
                if (elem != NULL) {
                    mark_expression_calls(elem, map);
                }
                elements = elements->next;
            }
            break;
        }
        
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE: {
            mark_statement_calls(expr->expr_data.anonymous_method_data.body, map);
            break;
        }
        
        case EXPR_VAR_ID:
        case EXPR_INUM:
        case EXPR_RNUM:
        case EXPR_STRING:
        case EXPR_CHAR_CODE:
        case EXPR_BOOL:
        case EXPR_NIL:
        case EXPR_ADDR_OF_PROC:
            /* No function calls in these expressions */
            break;
        
        default:
            /* Unknown expression type, but don't fail */
            break;
    }
}

/* Build the subprogram map by traversing the entire AST */
static void build_subprogram_map(SubprogramMap *map, ListNode_t *subprograms) {
    ListNode_t *cur = subprograms;
    while (cur != NULL) {
        Tree_t *sub = (Tree_t *)cur->cur;
        if (sub != NULL && sub->type == TREE_SUBPROGRAM) {
            const char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
            if (mangled_id != NULL) {
                map_add(map, mangled_id, sub);
            }
            
            /* Recursively add nested subprograms */
            build_subprogram_map(map, sub->tree_data.subprogram_data.subprograms);
        }
        cur = cur->next;
    }
}

/* Main entry point: mark all used functions */
void mark_used_functions(Tree_t *program, SymTab_t *symtab) {
    (void)symtab;  /* May be used in future for additional analysis */
    
    if (program == NULL || program->type != TREE_PROGRAM_TYPE)
        return;
    
    /* Build a map of all subprograms */
    SubprogramMap *map = create_subprogram_map();
    build_subprogram_map(map, program->tree_data.program_data.subprograms);
    
    /* Mark the main program body as always used */
    if (program->tree_data.program_data.body_statement != NULL) {
        mark_statement_calls(program->tree_data.program_data.body_statement, map);
    }
    
    /* Clean up */
    free_subprogram_map(map);
}
