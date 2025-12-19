/*
    Reachability analysis for subprograms
    Marks which functions are actually used starting from the main program
*/

#include "mark_used.h"
#include "Parser/ParseTree/tree_types.h"
#include "Parser/SemanticCheck/HashTable/HashTable.h"
#include "Parser/ParseTree/KgpcType.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* Hash table to map mangled_id -> Tree_t* (subprogram) */
typedef struct {
    char *mangled_id;
    Tree_t *subprogram;
} SubprogramEntry;

typedef struct {
    SubprogramEntry *entries;
    int count;
    int capacity;
} SubprogramMap;

static void map_init(SubprogramMap *map) {
    map->entries = NULL;
    map->count = 0;
    map->capacity = 0;
}

static void map_destroy(SubprogramMap *map) {
    free(map->entries);
    map->entries = NULL;
    map->count = 0;
    map->capacity = 0;
}

static void map_add(SubprogramMap *map, const char *mangled_id, Tree_t *subprogram) {
    if (mangled_id == NULL || subprogram == NULL) return;
    
    if (map->count >= map->capacity) {
        int new_capacity = map->capacity == 0 ? 64 : map->capacity * 2;
        SubprogramEntry *new_entries = realloc(map->entries, new_capacity * sizeof(SubprogramEntry));
        if (new_entries == NULL) return;
        map->entries = new_entries;
        map->capacity = new_capacity;
    }
    
    map->entries[map->count].mangled_id = (char*)mangled_id;
    map->entries[map->count].subprogram = subprogram;
    map->count++;
}

static Tree_t* map_find(SubprogramMap *map, const char *mangled_id) {
    if (mangled_id == NULL) return NULL;

    Tree_t *fallback = NULL;
    for (int i = 0; i < map->count; i++) {
        if (map->entries[i].mangled_id != NULL &&
            strcmp(map->entries[i].mangled_id, mangled_id) == 0) {
            Tree_t *sub = map->entries[i].subprogram;
            if (sub != NULL && sub->tree_data.subprogram_data.statement_list != NULL)
                return sub;
            if (fallback == NULL)
                fallback = sub;
        }
    }

    /* Fallback: try searching by id if mangled_id not found or only forward decls exist */
    Tree_t *id_fallback = NULL;
    for (int i = 0; i < map->count; i++) {
        Tree_t *sub = map->entries[i].subprogram;
        if (sub != NULL && sub->tree_data.subprogram_data.id != NULL &&
            strcmp(sub->tree_data.subprogram_data.id, mangled_id) == 0) {
            if (sub->tree_data.subprogram_data.statement_list != NULL)
                return sub;
            if (id_fallback == NULL)
                id_fallback = sub;
        }
    }

    if (fallback != NULL)
        return fallback;
    return id_fallback;
}

/* Forward declarations */
static void mark_expr_calls(struct Expression *expr, SubprogramMap *map);
static void mark_stmt_calls(struct Statement *stmt, SubprogramMap *map);

/* Mark a subprogram and recursively mark all functions it calls */
static void mark_subprogram_recursive(Tree_t *sub, SubprogramMap *map) {
    if (sub == NULL || sub->type != TREE_SUBPROGRAM) return;
    
    /* Already marked? */
    if (sub->tree_data.subprogram_data.is_used) return;
    
    /* Mark as used */
    sub->tree_data.subprogram_data.is_used = 1;
    
    /* IMPORTANT: If there's a forward declaration, we need to mark it too */
    /* The map contains the implementation, but codegen iterates through ALL nodes */
    /* So we need to find and mark any other nodes with the same mangled_id */
    char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
    if (mangled_id != NULL) {
        /* We'll mark all occurrences by iterating through the map entries */
        /* Actually, we can't easily do this without access to the full list */
        /* Instead, we'll handle this in build_subprogram_map by linking them */
    }
    
    /* Traverse the body to find calls */
    struct Statement *body = sub->tree_data.subprogram_data.statement_list;
    if (body != NULL) {
        mark_stmt_calls(body, map);
    }
}

/* Helper to check if a pointer looks valid (not garbage from uninitialized union members) */
static int is_valid_pointer(void *ptr) {
    uintptr_t addr = (uintptr_t)ptr;
    /* NULL is valid */
    if (addr == 0) return 1;
    /* On x86-64 Linux, valid heap/stack pointers are typically in these ranges */
    /* This filters out garbage like 0x3b2720646e756f66 (ASCII text) */
    if (addr < 0x1000 || addr > 0x7fffffffffff) return 0;
    return 1;
}

static void mark_expr_calls(struct Expression *expr, SubprogramMap *map) {
    if (expr == NULL) return;
    
    if (!is_valid_pointer(expr)) {
        return;
    }

    /* Early return for leaf expression types that don't contain function calls */
    /* We need to be careful accessing expr->type if expr is garbage but passes is_valid_pointer */
    /* But we can't easily check validity without OS-specific calls (IsBadReadPtr etc) */
    
    switch (expr->type) {
        case EXPR_VAR_ID:
        case EXPR_INUM:
        case EXPR_RNUM:
        case EXPR_STRING:
        case EXPR_CHAR_CODE:
        case EXPR_BOOL:
        case EXPR_NIL:
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
            return;
        default:
            break;
    }
    
    switch (expr->type) {
        case EXPR_FUNCTION_CALL: {
            const char *lookup_id = expr->expr_data.function_call_data.mangled_id;
            if (lookup_id == NULL)
                lookup_id = expr->expr_data.function_call_data.id;
            if (lookup_id != NULL) {
                Tree_t *called_sub = map_find(map, lookup_id);
                if (called_sub != NULL)
                    mark_subprogram_recursive(called_sub, map);
            } else if (expr->expr_data.function_call_data.call_kgpc_type != NULL &&
                       expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE) {
                Tree_t *called_sub = expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition;
                if (called_sub != NULL)
                    mark_subprogram_recursive(called_sub, map);
            }
            /* Also check arguments */
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            while (args != NULL) {
                if (args->type == LIST_EXPR && args->cur != NULL) {
                    mark_expr_calls((struct Expression*)args->cur, map);
                }
                args = args->next;
            }
            break;
        }
        
        case EXPR_RELOP:
            if (is_valid_pointer(expr->expr_data.relop_data.left) && 
                expr->expr_data.relop_data.left != NULL)
                mark_expr_calls(expr->expr_data.relop_data.left, map);
            if (is_valid_pointer(expr->expr_data.relop_data.right) && 
                expr->expr_data.relop_data.right != NULL)
                mark_expr_calls(expr->expr_data.relop_data.right, map);
            break;
            
        case EXPR_SIGN_TERM:
            if (is_valid_pointer(expr->expr_data.sign_term) && expr->expr_data.sign_term != NULL)
                mark_expr_calls(expr->expr_data.sign_term, map);
            break;
            
        case EXPR_ADDOP:
            if (expr->expr_data.addop_data.left_expr != NULL)
                mark_expr_calls(expr->expr_data.addop_data.left_expr, map);
            if (expr->expr_data.addop_data.right_term != NULL)
                mark_expr_calls(expr->expr_data.addop_data.right_term, map);
            break;
            
        case EXPR_MULOP:
            if (is_valid_pointer(expr->expr_data.mulop_data.left_term) && expr->expr_data.mulop_data.left_term != NULL)
                mark_expr_calls(expr->expr_data.mulop_data.left_term, map);
            if (is_valid_pointer(expr->expr_data.mulop_data.right_factor) && expr->expr_data.mulop_data.right_factor != NULL)
                mark_expr_calls(expr->expr_data.mulop_data.right_factor, map);
            break;
            
        case EXPR_ARRAY_ACCESS:
            if (is_valid_pointer(expr->expr_data.array_access_data.array_expr) && expr->expr_data.array_access_data.array_expr != NULL)
                mark_expr_calls(expr->expr_data.array_access_data.array_expr, map);
            if (is_valid_pointer(expr->expr_data.array_access_data.index_expr) && expr->expr_data.array_access_data.index_expr != NULL)
                mark_expr_calls(expr->expr_data.array_access_data.index_expr, map);
            break;
            
        case EXPR_RECORD_ACCESS:
            if (is_valid_pointer(expr->expr_data.record_access_data.record_expr) && 
                expr->expr_data.record_access_data.record_expr != NULL)
                mark_expr_calls(expr->expr_data.record_access_data.record_expr, map);
            break;
            
        case EXPR_POINTER_DEREF:
            if (is_valid_pointer(expr->expr_data.pointer_deref_data.pointer_expr) && 
                expr->expr_data.pointer_deref_data.pointer_expr != NULL)
                mark_expr_calls(expr->expr_data.pointer_deref_data.pointer_expr, map);
            break;
            
        case EXPR_ADDR:
            if (is_valid_pointer(expr->expr_data.addr_data.expr) && 
                expr->expr_data.addr_data.expr != NULL)
                mark_expr_calls(expr->expr_data.addr_data.expr, map);
            break;
            
        case EXPR_TYPECAST:
            if (is_valid_pointer(expr->expr_data.typecast_data.expr) && 
                expr->expr_data.typecast_data.expr != NULL)
                mark_expr_calls(expr->expr_data.typecast_data.expr, map);
            break;
            
        case EXPR_IS:
            if (is_valid_pointer(expr->expr_data.is_data.expr) && 
                expr->expr_data.is_data.expr != NULL)
                mark_expr_calls(expr->expr_data.is_data.expr, map);
            break;
            
        case EXPR_AS:
            if (is_valid_pointer(expr->expr_data.as_data.expr) && 
                expr->expr_data.as_data.expr != NULL)
                mark_expr_calls(expr->expr_data.as_data.expr, map);
            break;
            
        case EXPR_ADDR_OF_PROC: {
            if (expr->expr_data.addr_of_proc_data.procedure_symbol != NULL) {
                char *mangled_id = expr->expr_data.addr_of_proc_data.procedure_symbol->mangled_id;
                if (mangled_id != NULL) {
                    Tree_t *called_sub = map_find(map, mangled_id);
                    if (called_sub != NULL) {
                        mark_subprogram_recursive(called_sub, map);
                    }
                }
            }
            break;
        }
            
        case EXPR_SET:
            if (expr->expr_data.set_data.elements != NULL) {
                ListNode_t *elem = expr->expr_data.set_data.elements;
                while (elem != NULL) {
                    if (elem->cur != NULL) {
                        struct SetElement *set_elem = (struct SetElement*)elem->cur;
                        mark_expr_calls(set_elem->lower, map);
                        mark_expr_calls(set_elem->upper, map);
                    }
                    elem = elem->next;
                }
            }
            break;
            
        case EXPR_ARRAY_LITERAL:
            if (expr->expr_data.array_literal_data.elements != NULL) {
                ListNode_t *elem = expr->expr_data.array_literal_data.elements;
                while (elem != NULL) {
                    if (elem->type == LIST_EXPR && elem->cur != NULL) {
                        mark_expr_calls((struct Expression*)elem->cur, map);
                    }
                    elem = elem->next;
                }
            }
            break;
            
        default:
            /* Leaf nodes - nothing to traverse */
            break;
    }
}

static void mark_stmt_calls(struct Statement *stmt, SubprogramMap *map) {
    if (stmt == NULL) return;
    
    
    switch (stmt->type) {
        case STMT_PROCEDURE_CALL: {
            const char *lookup_id = stmt->stmt_data.procedure_call_data.mangled_id;
            if (lookup_id == NULL)
                lookup_id = stmt->stmt_data.procedure_call_data.id;
            if (lookup_id != NULL) {
                Tree_t *called_sub = map_find(map, lookup_id);
                if (called_sub != NULL) {
                    mark_subprogram_recursive(called_sub, map);
                }
            } else if (stmt->stmt_data.procedure_call_data.call_kgpc_type != NULL &&
                       stmt->stmt_data.procedure_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE) {
                Tree_t *called_sub = stmt->stmt_data.procedure_call_data.call_kgpc_type->info.proc_info.definition;
                if (called_sub != NULL) {
                    mark_subprogram_recursive(called_sub, map);
                }
            }
            /* Also check arguments */
            ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
            while (args != NULL) {
                if (args->type == LIST_EXPR && args->cur != NULL) {
                    mark_expr_calls((struct Expression*)args->cur, map);
                }
                args = args->next;
            }
            break;
        }
        
        case STMT_VAR_ASSIGN:
            mark_expr_calls(stmt->stmt_data.var_assign_data.var, map);
            mark_expr_calls(stmt->stmt_data.var_assign_data.expr, map);
            break;
            
        case STMT_COMPOUND_STATEMENT: {
            ListNode_t *stmts = stmt->stmt_data.compound_statement;
            while (stmts != NULL) {
                if (stmts->type == LIST_STMT && stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)stmts->cur, map);
                }
                stmts = stmts->next;
            }
            break;
        }
        
        case STMT_IF_THEN:
            mark_expr_calls(stmt->stmt_data.if_then_data.relop_expr, map);
            mark_stmt_calls(stmt->stmt_data.if_then_data.if_stmt, map);
            mark_stmt_calls(stmt->stmt_data.if_then_data.else_stmt, map);
            break;
            
        case STMT_WHILE:
            mark_expr_calls(stmt->stmt_data.while_data.relop_expr, map);
            mark_stmt_calls(stmt->stmt_data.while_data.while_stmt, map);
            break;
            
        case STMT_REPEAT: {
            ListNode_t *body = stmt->stmt_data.repeat_data.body_list;
            while (body != NULL) {
                if (body->type == LIST_STMT && body->cur != NULL) {
                    mark_stmt_calls((struct Statement*)body->cur, map);
                }
                body = body->next;
            }
            mark_expr_calls(stmt->stmt_data.repeat_data.until_expr, map);
            break;
        }
        
        case STMT_FOR:
        case STMT_FOR_VAR:
        case STMT_FOR_ASSIGN_VAR:

            if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR) {
                if (stmt->stmt_data.for_data.for_assign_data.var_assign != NULL) {
                    mark_stmt_calls(stmt->stmt_data.for_data.for_assign_data.var_assign, map);
                }
            } else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR) {

                if (stmt->stmt_data.for_data.for_assign_data.var != NULL)
                    mark_expr_calls(stmt->stmt_data.for_data.for_assign_data.var, map);
            }

            if (stmt->stmt_data.for_data.to != NULL) {

                mark_expr_calls(stmt->stmt_data.for_data.to, map);
            }
            if (stmt->stmt_data.for_data.do_for != NULL)
                mark_stmt_calls(stmt->stmt_data.for_data.do_for, map);
            break;
            
        case STMT_FOR_IN:
            mark_expr_calls(stmt->stmt_data.for_in_data.loop_var, map);
            mark_expr_calls(stmt->stmt_data.for_in_data.collection, map);
            mark_stmt_calls(stmt->stmt_data.for_in_data.do_stmt, map);
            break;
            
        case STMT_CASE: {
            mark_expr_calls(stmt->stmt_data.case_data.selector_expr, map);
            ListNode_t *branches = stmt->stmt_data.case_data.branches;
            while (branches != NULL) {
                if (branches->cur != NULL) {
                    struct CaseBranch *branch = (struct CaseBranch*)branches->cur;
                    /* Check label expressions */
                    ListNode_t *labels = branch->labels;
                    while (labels != NULL) {
                        if (labels->type == LIST_EXPR && labels->cur != NULL) {
                            mark_expr_calls((struct Expression*)labels->cur, map);
                        }
                        labels = labels->next;
                    }
                    mark_stmt_calls(branch->stmt, map);
                }
                branches = branches->next;
            }
            mark_stmt_calls(stmt->stmt_data.case_data.else_stmt, map);
            break;
        }
        
        case STMT_WITH:
            mark_expr_calls(stmt->stmt_data.with_data.context_expr, map);
            mark_stmt_calls(stmt->stmt_data.with_data.body_stmt, map);
            break;
            
        case STMT_TRY_FINALLY: {
            ListNode_t *try_stmts = stmt->stmt_data.try_finally_data.try_statements;
            while (try_stmts != NULL) {
                if (try_stmts->type == LIST_STMT && try_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)try_stmts->cur, map);
                }
                try_stmts = try_stmts->next;
            }
            ListNode_t *finally_stmts = stmt->stmt_data.try_finally_data.finally_statements;
            while (finally_stmts != NULL) {
                if (finally_stmts->type == LIST_STMT && finally_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)finally_stmts->cur, map);
                }
                finally_stmts = finally_stmts->next;
            }
            break;
        }
        
        case STMT_TRY_EXCEPT: {
            ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
            while (try_stmts != NULL) {
                if (try_stmts->type == LIST_STMT && try_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)try_stmts->cur, map);
                }
                try_stmts = try_stmts->next;
            }
            ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;
            while (except_stmts != NULL) {
                if (except_stmts->type == LIST_STMT && except_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)except_stmts->cur, map);
                }
                except_stmts = except_stmts->next;
            }
            break;
        }
        
        case STMT_RAISE:
            mark_expr_calls(stmt->stmt_data.raise_data.exception_expr, map);
            break;
            
        case STMT_INHERITED:
            mark_expr_calls(stmt->stmt_data.inherited_data.call_expr, map);
            break;
            
        case STMT_LABEL:
            mark_stmt_calls(stmt->stmt_data.label_data.stmt, map);
            break;
            
        default:
            /* Other statements don't have expressions to check */
            break;
    }
}

/* Build map of all subprograms (recursive for nested subprograms) */
static void build_subprogram_map(ListNode_t *sub_list, SubprogramMap *map) {
    while (sub_list != NULL) {
        if (sub_list->type == LIST_TREE && sub_list->cur != NULL) {
            Tree_t *sub = (Tree_t*)sub_list->cur;
            if (sub->type == TREE_SUBPROGRAM) {
                /* Initialize is_used to 0 */
                sub->tree_data.subprogram_data.is_used = 0;
                
                const char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
                const char *map_id = (mangled_id != NULL) ? mangled_id : sub->tree_data.subprogram_data.id;
                if (map_id != NULL) {
                    /* Check if this mangled_id already exists (forward declaration case) */
                    Tree_t *existing = map_find(map, map_id);
                    if (existing != NULL) {
                        /* We have both forward declaration and implementation */
                        /* We need to ensure both get marked as used */
                        /* Strategy: mark the forward declaration NOW if the implementation gets marked */
                        /* We'll use a simple approach: when we mark one, we mark the other */
                        
                        if (sub->tree_data.subprogram_data.statement_list != NULL) {
                            /* This is the implementation, replace in map */
                            /* But first, mark the forward declaration to match this one's status */
                            existing->tree_data.subprogram_data.is_used = sub->tree_data.subprogram_data.is_used;
                            map_add(map, map_id, sub);
                        } else {
                            /* This is another forward declaration, keep the existing one */
                            sub->tree_data.subprogram_data.is_used = existing->tree_data.subprogram_data.is_used;
                        }
                    } else {
                        map_add(map, map_id, sub);
                    }
                }
                
                /* Recursively process nested subprograms */
                if (sub->tree_data.subprogram_data.subprograms != NULL) {
                    build_subprogram_map(sub->tree_data.subprogram_data.subprograms, map);
                }
            }
        }
        sub_list = sub_list->next;
    }
}

void mark_used_functions(Tree_t *program, SymTab_t *symtab) {
    if (program == NULL || symtab == NULL || program->type != TREE_PROGRAM_TYPE) return;
    
    SubprogramMap map;
    map_init(&map);
    
    /* Build map of all subprograms */
    build_subprogram_map(program->tree_data.program_data.subprograms, &map);
    
    /* Start from the main program body */
    struct Statement *main_body = program->tree_data.program_data.body_statement;
    if (main_body != NULL) {
        mark_stmt_calls(main_body, &map);
    }
    
    /* Also traverse finalization statements */
    ListNode_t *final = program->tree_data.program_data.finalization_statements;
    while (final != NULL) {
        if (final->type == LIST_STMT && final->cur != NULL) {
            mark_stmt_calls((struct Statement*)final->cur, &map);
        }
        final = final->next;
    }
    
    /* IMPORTANT: Second pass to sync forward declarations with implementations */
    /* For each subprogram, if it has the same mangled_id as another, sync is_used */
    ListNode_t *sub_list = program->tree_data.program_data.subprograms;
    while (sub_list != NULL) {
        if (sub_list->type == LIST_TREE && sub_list->cur != NULL) {
            Tree_t *sub = (Tree_t*)sub_list->cur;
            if (sub->type == TREE_SUBPROGRAM) {
                char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
                if (mangled_id != NULL) {
                    /* Find the canonical version in the map */
                    Tree_t *canonical = map_find(&map, mangled_id);
                    if (canonical != NULL && canonical != sub) {
                        /* Sync the is_used flag */
                        sub->tree_data.subprogram_data.is_used = canonical->tree_data.subprogram_data.is_used;
                    }
                }
            }
        }
        sub_list = sub_list->next;
    }
    
    #ifdef DEBUG_OPTIMIZER
        fprintf(stderr, "DEBUG mark_used_functions: Reachability analysis complete\n");
    #endif
    
    map_destroy(&map);
}
