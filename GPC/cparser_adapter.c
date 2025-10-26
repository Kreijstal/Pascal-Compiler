/*
 * cparser_adapter.c
 * Adapter to convert cparser AST to GPC parse tree format
 */

#include "cparser_adapter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Grammar.tab.h"
#include "List.h"

// External declarations from cparser
extern ast_t* ast_nil;

// Helper function to append to a list (convenience wrapper)
static ListNode_t* list_append(ListNode_t* head, void* data, enum ListType type) {
    ListNode_t* new_node = CreateListNode(data, type);
    return PushListNodeBack(head, new_node);
}

static struct Expression* convert_expression(ast_t* ast);
static struct Statement* convert_statement(ast_t* ast);
static Tree_t* convert_declaration(ast_t* ast);

// Helper function to convert cparser strings to C strings
static char* ast_to_string(ast_t* ast) {
    if (ast == NULL || ast == ast_nil || ast->sym == NULL) {
        return NULL;
    }
    return strdup(ast->sym->name);
}

// Helper to get child at index
static ast_t* get_child(ast_t* parent, int index) {
    if (parent == NULL || parent == ast_nil) return NULL;
    ast_t* child = parent->child;
    for (int i = 0; i < index && child != NULL && child != ast_nil; i++) {
        child = child->next;
    }
    return (child == ast_nil) ? NULL : child;
}

// Convert cparser integer constant to expression
static struct Expression* convert_integer(ast_t* ast) {
    if (ast == NULL || ast == ast_nil || ast->sym == NULL) {
        return mk_inum(0, 0);
    }
    int value = atoi(ast->sym->name);
    return mk_inum(0, value);
}

// Convert cparser identifier to expression
static struct Expression* convert_identifier(ast_t* ast) {
    if (ast == NULL || ast == ast_nil || ast->sym == NULL) {
        return mk_varid(0, strdup(""));
    }
    return mk_varid(0, strdup(ast->sym->name));
}

// Convert cparser expression to GPC expression
static struct Expression* convert_expression(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) {
        return NULL;
    }

    switch (ast->typ) {
        case PASCAL_T_INTEGER:
            return convert_integer(ast);
        
        case PASCAL_T_IDENTIFIER:
            return convert_identifier(ast);
        
        case PASCAL_T_ADD: {
            ast_t* left = get_child(ast, 0);
            ast_t* right = get_child(ast, 1);
            return mk_addop(0, PLUS, convert_expression(left), convert_expression(right));
        }
        
        case PASCAL_T_SUB: {
            ast_t* left = get_child(ast, 0);
            ast_t* right = get_child(ast, 1);
            return mk_addop(0, MINUS, convert_expression(left), convert_expression(right));
        }
        
        case PASCAL_T_MUL: {
            ast_t* left = get_child(ast, 0);
            ast_t* right = get_child(ast, 1);
            return mk_mulop(0, STAR, convert_expression(left), convert_expression(right));
        }
        
        case PASCAL_T_DIV:
        case PASCAL_T_INTDIV: {
            ast_t* left = get_child(ast, 0);
            ast_t* right = get_child(ast, 1);
            return mk_mulop(0, SLASH, convert_expression(left), convert_expression(right));
        }
        
        case PASCAL_T_MOD: {
            ast_t* left = get_child(ast, 0);
            ast_t* right = get_child(ast, 1);
            return mk_mulop(0, MOD, convert_expression(left), convert_expression(right));
        }
        
        case PASCAL_T_EQ:
        case PASCAL_T_NE:
        case PASCAL_T_LT:
        case PASCAL_T_GT:
        case PASCAL_T_LE:
        case PASCAL_T_GE: {
            ast_t* left = get_child(ast, 0);
            ast_t* right = get_child(ast, 1);
            int relop_type;
            switch (ast->typ) {
                case PASCAL_T_EQ: relop_type = EQ; break;
                case PASCAL_T_NE: relop_type = NE; break;
                case PASCAL_T_LT: relop_type = LT; break;
                case PASCAL_T_GT: relop_type = GT; break;
                case PASCAL_T_LE: relop_type = LE; break;
                case PASCAL_T_GE: relop_type = GE; break;
                default: relop_type = EQ;
            }
            return mk_relop(0, relop_type, convert_expression(left), convert_expression(right));
        }
        
        case PASCAL_T_STRING:
            return mk_string(0, ast_to_string(ast));
        
        case PASCAL_T_FUNC_CALL: {
            char* func_name = ast_to_string(get_child(ast, 0));
            ast_t* args_ast = get_child(ast, 1);
            ListNode_t* args = NULL;
            
            // Convert argument list
            ast_t* arg = args_ast ? args_ast->child : NULL;
            while (arg != NULL && arg != ast_nil) {
                struct Expression* arg_expr = convert_expression(arg);
                if (arg_expr) {
                    args = list_append(args, arg_expr, LIST_EXPR);
                }
                arg = arg->next;
            }
            
            return mk_functioncall(0, func_name, args);
        }
        
        default:
            // For unknown expression types, return a dummy integer
            fprintf(stderr, "Warning: Unhandled expression type %d\n", ast->typ);
            return mk_inum(0, 0);
    }
}

// Convert cparser statement to GPC statement
static struct Statement* convert_statement(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) {
        return NULL;
    }

    switch (ast->typ) {
        case PASCAL_T_ASSIGNMENT: {
            ast_t* var_ast = get_child(ast, 0);
            ast_t* expr_ast = get_child(ast, 1);
            return mk_varassign(0, convert_expression(var_ast), convert_expression(expr_ast));
        }
        
        case PASCAL_T_STATEMENT_LIST:
        case PASCAL_T_BEGIN_BLOCK: {
            ListNode_t* stmt_list = NULL;
            ast_t* child = ast->child;
            while (child != NULL && child != ast_nil) {
                struct Statement* stmt = convert_statement(child);
                if (stmt) {
                    stmt_list = list_append(stmt_list, stmt, LIST_STMT);
                }
                child = child->next;
            }
            return mk_compoundstatement(0, stmt_list);
        }
        
        case PASCAL_T_IF_STMT: {
            ast_t* cond = get_child(ast, 0);
            ast_t* then_stmt = get_child(ast, 1);
            ast_t* else_stmt = get_child(ast, 2);
            return mk_ifthen(0,
                convert_expression(cond),
                convert_statement(then_stmt),
                convert_statement(else_stmt));
        }
        
        case PASCAL_T_WHILE_STMT: {
            ast_t* cond = get_child(ast, 0);
            ast_t* body = get_child(ast, 1);
            return mk_while(0, convert_expression(cond), convert_statement(body));
        }
        
        case PASCAL_T_FOR_STMT: {
            ast_t* init = get_child(ast, 0);
            ast_t* limit = get_child(ast, 1);
            ast_t* body = get_child(ast, 2);
            
            // Convert initialization to assignment statement
            struct Statement* init_stmt = convert_statement(init);
            struct Expression* limit_expr = convert_expression(limit);
            struct Statement* body_stmt = convert_statement(body);
            
            return mk_forassign(0, init_stmt, limit_expr, body_stmt);
        }
        
        case PASCAL_T_FUNC_CALL: {
            // This is a procedure call (function call used as statement)
            char* proc_name = ast_to_string(get_child(ast, 0));
            ast_t* args_ast = get_child(ast, 1);
            ListNode_t* args = NULL;
            
            // Convert argument list
            ast_t* arg = args_ast ? args_ast->child : NULL;
            while (arg != NULL && arg != ast_nil) {
                struct Expression* arg_expr = convert_expression(arg);
                if (arg_expr) {
                    args = list_append(args, arg_expr, LIST_EXPR);
                }
                arg = arg->next;
            }
            
            return mk_procedurecall(0, proc_name, args);
        }
        
        case PASCAL_T_ASM_BLOCK: {
            char* code = ast_to_string(ast);
            return mk_asmblock(0, code ? code : strdup(""));
        }
        
        default:
            fprintf(stderr, "Warning: Unhandled statement type %d\n", ast->typ);
            return NULL;
    }
}

// Convert variable declaration
static Tree_t* convert_var_declaration(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) {
        return NULL;
    }
    
    // Get the identifier list and type
    ast_t* id_list = get_child(ast, 0);
    ast_t* type_spec = get_child(ast, 1);
    
    // Build list of identifiers
    ListNode_t* ids = NULL;
    ast_t* id = id_list ? id_list->child : NULL;
    while (id != NULL && id != ast_nil) {
        char* id_str = ast_to_string(id);
        if (id_str) {
            ids = list_append(ids, id_str, LIST_STRING);
        }
        id = id->next;
    }
    
    // Determine type
    int type = INT_TYPE; // Default
    if (type_spec && type_spec->sym) {
        const char* type_name = type_spec->sym->name;
        if (strcmp(type_name, "integer") == 0) {
            type = INT_TYPE;
        } else if (strcmp(type_name, "real") == 0) {
            type = REAL_TYPE;
        }
    }
    
    return mk_vardecl(0, ids, type, NULL, 0);
}

// Convert a procedure or function declaration
static Tree_t* convert_subprogram(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) {
        return NULL;
    }
    
    char* name = ast_to_string(get_child(ast, 0));
    ast_t* params_ast = get_child(ast, 1);
    ast_t* var_section = get_child(ast, 2);
    ast_t* body = get_child(ast, 3);
    
    // Convert parameter list
    ListNode_t* params = NULL;
    if (params_ast) {
        ast_t* param = params_ast->child;
        while (param != NULL && param != ast_nil) {
            Tree_t* param_decl = convert_var_declaration(param);
            if (param_decl) {
                params = list_append(params, param_decl, LIST_TREE);
            }
            param = param->next;
        }
    }
    
    // Convert variable declarations
    ListNode_t* vars = NULL;
    if (var_section) {
        ast_t* var = var_section->child;
        while (var != NULL && var != ast_nil) {
            Tree_t* var_decl = convert_var_declaration(var);
            if (var_decl) {
                vars = list_append(vars, var_decl, LIST_TREE);
            }
            var = var->next;
        }
    }
    
    // Convert body
    struct Statement* body_stmt = convert_statement(body);
    
    if (ast->typ == PASCAL_T_PROCEDURE_DECL) {
        return mk_procedure(0, name, params, vars, NULL, body_stmt, 0, 0);
    } else {
        // For functions, get return type
        ast_t* return_type = get_child(ast, 4);
        int ret_type = INT_TYPE;
        char* ret_type_id = NULL;
        
        if (return_type && return_type->sym) {
            const char* type_name = return_type->sym->name;
            if (strcmp(type_name, "integer") == 0) {
                ret_type = INT_TYPE;
            } else if (strcmp(type_name, "real") == 0) {
                ret_type = REAL_TYPE;
            } else {
                ret_type_id = strdup(type_name);
            }
        }
        
        return mk_function(0, name, params, vars, NULL, body_stmt, ret_type, ret_type_id, 0, 0);
    }
}

// Main conversion function: convert cparser AST to GPC parse tree
Tree_t* cparser_ast_to_tree(ast_t* root) {
    if (root == NULL || root == ast_nil) {
        return NULL;
    }
    
    // Expecting a program declaration at root
    if (root->typ != PASCAL_T_PROGRAM_DECL && root->typ != PASCAL_T_UNIT_DECL) {
        fprintf(stderr, "Error: Expected PROGRAM or UNIT declaration at root\n");
        return NULL;
    }
    
    // Get program components
    char* program_name = ast_to_string(get_child(root, 0));
    ast_t* program_params = get_child(root, 1);
    ast_t* var_section = get_child(root, 2);
    ast_t* type_section = get_child(root, 3);
    ast_t* subprograms_section = get_child(root, 4);
    ast_t* main_block = get_child(root, 5);
    
    // Convert program parameters (input, output, etc.)
    ListNode_t* params = NULL;
    if (program_params) {
        ast_t* param = program_params->child;
        while (param != NULL && param != ast_nil) {
            char* param_str = ast_to_string(param);
            if (param_str) {
                params = list_append(params, param_str, LIST_STRING);
            }
            param = param->next;
        }
    }
    
    // Convert variable declarations
    ListNode_t* var_decls = NULL;
    if (var_section) {
        ast_t* var = var_section->child;
        while (var != NULL && var != ast_nil) {
            Tree_t* var_decl = convert_var_declaration(var);
            if (var_decl) {
                var_decls = list_append(var_decls, var_decl, LIST_TREE);
            }
            var = var->next;
        }
    }
    
    // Convert type declarations
    ListNode_t* type_decls = NULL;
    if (type_section) {
        ast_t* type = type_section->child;
        while (type != NULL && type != ast_nil) {
            // TODO: Implement type declaration conversion
            type = type->next;
        }
    }
    
    // Convert subprograms
    ListNode_t* subprograms = NULL;
    if (subprograms_section) {
        ast_t* subprog = subprograms_section->child;
        while (subprog != NULL && subprog != ast_nil) {
            Tree_t* subprog_tree = convert_subprogram(subprog);
            if (subprog_tree) {
                subprograms = list_append(subprograms, subprog_tree, LIST_TREE);
            }
            subprog = subprog->next;
        }
    }
    
    // Convert main block
    struct Statement* main_stmt = convert_statement(main_block);
    
    return mk_program(0, program_name, params, var_decls, type_decls, subprograms, main_stmt);
}
