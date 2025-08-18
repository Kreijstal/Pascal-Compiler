#include "flat_sem_check.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "SymTab/SymTab.h"
#include "../flat_ast.h"

// Forward declarations
Type_t *sem_check_node(FlatNode *node, SymTab_t *symtab, int *error_count);
static int sem_check_declarations(VarDecl_t *decls, SymTab_t *symtab, int *error_count);

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

static int sem_check_declarations(VarDecl_t *decls, SymTab_t *symtab, int *error_count) {
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

Type_t *sem_check_node(FlatNode *node, SymTab_t *symtab, int *error_count) {
    if (node == NULL) {
        return NULL;
    }

    Type_t *result_type = NULL;

    switch (node->node_type) {
        case FL_PROGRAM:
            PushScope(symtab);
            sem_check_declarations(node->data.program.declarations, symtab, error_count);
            // sem_check_subprograms(node->data.program.subprograms, symtab, error_count);
            sem_check_node(node->data.program.compound_statement, symtab, error_count);
            PopScope(symtab);
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
        default:
            // For other statement types, we don't need to do anything yet
            // and they don't produce a type.
            break;
    }

    return result_type;
}
