#include "flat_sem_check.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "SymTab/SymTab.h"
#include "../flat_ast.h"
#include "mangle.h"

// Forward declarations
Type_t *sem_check_node(FlatNode *node, SymTab_t *symtab, int *error_count);
static int sem_check_var_declarations(VarDecl_t *decls, SymTab_t *symtab, int *error_count);
static int sem_check_parameters(ListNode_t *params, SymTab_t *symtab, int *error_count);

// Helper for creating basic types
static Type_t *create_basic_type(BaseType base_type) {
    Type_t *type = (Type_t *)malloc(sizeof(Type_t));
    assert(type != NULL);
    type->base_type = base_type;
    return type;
}

static void sem_error(int line, const char *message) {
    fprintf(stderr, "Semantic Error on line %d: %s\n", line, message);
}

int sem_check(FlatNode *ast) {
    if (ast == NULL) {
        return 0;
    }

    SymTab_t *symtab = InitSymTab();
    // TODO: Add built-ins to symbol table

    int errors = 0;
    sem_check_node(ast, symtab, &errors);

    // TODO: Free symbol table properly
    /*DestroySymTab(symtab);*/
    return errors;
}

static int sem_check_var_declarations(VarDecl_t *decls, SymTab_t *symtab, int *error_count) {
    VarDecl_t *current_decl = decls;
    while (current_decl != NULL) {
        ListNode_t *current_id = current_decl->id_list;
        while (current_id != NULL) {
            char *id = (char *)current_id->cur;
            if (PushVarOntoScope(symtab, current_decl->type, id) != 0) {
                // TODO: Better line number reporting
                sem_error(0, "Redeclaration of variable");
                (*error_count)++;
            }
            current_id = current_id->next;
        }
        current_decl = current_decl->next;
    }
    return *error_count;
}

static int sem_check_parameters(ListNode_t *params, SymTab_t *symtab, int *error_count) {
    ListNode_t* current_param_group = params;
    while(current_param_group != NULL) {
        Param_t* param_group = (Param_t*)current_param_group->cur;

        ListNode_t* current_id = param_group->id_list;
        while(current_id != NULL) {
            char *id = (char *)current_id->cur;
            Type_t *type = create_basic_type(param_group->type);
            if (PushVarOntoScope(symtab, type, id) != 0) {
                sem_error(0, "Redeclaration of parameter");
                (*error_count)++;
            }
            current_id = current_id->next;
        }
        current_param_group = current_param_group->next;
    }
    return *error_count;
}

Type_t *sem_check_node(FlatNode *node, SymTab_t *symtab, int *error_count) {
    if (node == NULL) {
        return NULL;
    }

    Type_t *result_type = NULL;

    switch (node->node_type) {
        case FL_PROGRAM:
            PushScope(symtab);
            sem_check_var_declarations(node->data.program.declarations, symtab, error_count);

            // Process subprogram declarations
            ListNode_t *current_sub = node->data.program.subprograms;
            while (current_sub != NULL) {
                sem_check_node((FlatNode *)current_sub->cur, symtab, error_count);
                current_sub = current_sub->next;
            }

            sem_check_node(node->data.program.compound_statement, symtab, error_count);
            PopScope(symtab);
            break;
        case FL_PROCEDURE:
            {
                char *mangled_name = get_mangled_name(node->data.procedure.id, node->data.procedure.params);
                PushProcedureOntoScope(symtab, mangled_name, node->data.procedure.id, node->data.procedure.params);

                PushScope(symtab);

                // Add to own scope for recursion
                PushProcedureOntoScope(symtab, mangled_name, node->data.procedure.id, node->data.procedure.params);

                sem_check_parameters(node->data.procedure.params, symtab, error_count);
                sem_check_var_declarations(node->data.procedure.local_vars, symtab, error_count);
                sem_check_node(node->data.procedure.compound_statement, symtab, error_count);

                PopScope(symtab);
                free(mangled_name);
            }
            break;
        case FL_FUNCTION:
            {
                char *mangled_name = get_mangled_name(node->data.function.id, node->data.function.params);
                Type_t *return_type = create_basic_type(node->data.function.return_type);
                PushFunctionOntoScope(symtab, mangled_name, node->data.function.id, return_type, node->data.function.params);

                PushScope(symtab);

                // Add to own scope for recursion
                PushFunctionOntoScope(symtab, mangled_name, node->data.function.id, return_type, node->data.function.params);

                // Add function return variable to scope
                PushVarOntoScope(symtab, return_type, node->data.function.id);

                sem_check_parameters(node->data.function.params, symtab, error_count);
                sem_check_var_declarations(node->data.function.local_vars, symtab, error_count);
                sem_check_node(node->data.function.compound_statement, symtab, error_count);

                // TODO: Check for a return assignment

                PopScope(symtab);
                free(mangled_name);
            }
            break;
        case FL_COMPOUND_STATEMENT:
            {
                ListNode_t *current = node->data.compound_statement.stmt_list;
                while (current != NULL) {
                    sem_check_node((FlatNode *)current->cur, symtab, error_count);
                    current = current->next;
                }
            }
            break;
        case FL_VAR_ID:
            {
                HashNode_t *hash_node;
                if (FindIdent(&hash_node, symtab, node->data.var_id.id) == -1) {
                    sem_error(node->line_num, "Undeclared identifier");
                    (*error_count)++;
                    result_type = create_basic_type(TYPE_ID); // A dummy type
                } else {
                    result_type = hash_node->type;
                }
            }
            break;
        case FL_INUM:
            result_type = create_basic_type(TYPE_INTEGER);
            break;
        case FL_RNUM:
            result_type = create_basic_type(TYPE_REAL);
            break;
        case FL_STRING:
            result_type = create_basic_type(TYPE_STRING);
            break;
        case FL_VAR_ASSIGN:
            {
                Type_t *lhs_type = sem_check_node(node->data.var_assign.var, symtab, error_count);
                Type_t *rhs_type = sem_check_node(node->data.var_assign.expr, symtab, error_count);

                if (lhs_type != NULL && rhs_type != NULL && lhs_type->base_type != rhs_type->base_type) {
                    sem_error(node->line_num, "Type mismatch in assignment");
                    (*error_count)++;
                }
            }
            break;
        case FL_RELOP:
        case FL_ADDOP:
        case FL_MULOP:
            {
                Type_t *lhs_type = sem_check_node(node->data.bin_op.left, symtab, error_count);
                Type_t *rhs_type = sem_check_node(node->data.bin_op.right, symtab, error_count);

                if (lhs_type != NULL && rhs_type != NULL) {
                    if (lhs_type->base_type != rhs_type->base_type) {
                        sem_error(node->line_num, "Type mismatch in binary operation");
                        (*error_count)++;
                    }
                    // TODO: Add more specific checks (e.g., must be numeric)
                }

                if (node->node_type == FL_RELOP) {
                    result_type = create_basic_type(TYPE_BOOLEAN);
                } else {
                    result_type = lhs_type; // Result type is the same as operands
                }
            }
            break;
        case FL_IF_THEN:
            {
                Type_t *condition_type = sem_check_node(node->data.if_then.condition, symtab, error_count);
                if (condition_type != NULL && condition_type->base_type != TYPE_BOOLEAN) {
                    sem_error(node->line_num, "IF condition must be a boolean expression");
                    (*error_count)++;
                }
                sem_check_node(node->data.if_then.then_stmt, symtab, error_count);
                if (node->data.if_then.else_stmt) {
                    sem_check_node(node->data.if_then.else_stmt, symtab, error_count);
                }
            }
            break;
        case FL_WHILE_LOOP:
            {
                Type_t *condition_type = sem_check_node(node->data.while_loop.condition, symtab, error_count);
                if (condition_type != NULL && condition_type->base_type != TYPE_BOOLEAN) {
                    sem_error(node->line_num, "WHILE condition must be a boolean expression");
                    (*error_count)++;
                }
                sem_check_node(node->data.while_loop.while_stmt, symtab, error_count);
            }
            break;
        case FL_FOR_LOOP:
            {
                // The assignment part of the for loop also serves as the loop variable declaration
                Type_t *loop_var_type = sem_check_node(node->data.for_loop.for_assign, symtab, error_count);
                if (loop_var_type != NULL && loop_var_type->base_type != TYPE_INTEGER) {
                    sem_error(node->line_num, "FOR loop control variable must be an integer");
                    (*error_count)++;
                }

                Type_t *to_type = sem_check_node(node->data.for_loop.to_expr, symtab, error_count);
                if (to_type != NULL && to_type->base_type != TYPE_INTEGER) {
                    sem_error(node->line_num, "FOR loop end expression must be an integer");
                    (*error_count)++;
                }

                sem_check_node(node->data.for_loop.for_stmt, symtab, error_count);
            }
            break;
        case FL_PROCEDURE_CALL:
            {
                HashNode_t *hash_node;
                if (FindIdent(&hash_node, symtab, node->data.procedure_call.id) == -1) {
                    sem_error(node->line_num, "Undeclared procedure");
                    (*error_count)++;
                } else if (hash_node->hash_type != HASHTYPE_PROCEDURE && hash_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
                    sem_error(node->line_num, "Identifier is not a procedure");
                    (*error_count)++;
                }
                // TODO: Check arguments
            }
            break;
        case FL_FUNCTION_CALL:
            {
                HashNode_t *hash_node;
                if (FindIdent(&hash_node, symtab, node->data.function_call.id) == -1) {
                    sem_error(node->line_num, "Undeclared function");
                    (*error_count)++;
                    result_type = create_basic_type(TYPE_ID); // Dummy type
                } else if (hash_node->hash_type != HASHTYPE_FUNCTION) {
                    sem_error(node->line_num, "Identifier is not a function");
                    (*error_count)++;
                    result_type = create_basic_type(TYPE_ID); // Dummy type
                } else {
                    result_type = hash_node->type;
                }
                // TODO: Check arguments
            }
            break;
        default:
            // For other statement types, we don't need to do anything yet
            // and they don't produce a type.
            break;
    }

    return result_type;
}
