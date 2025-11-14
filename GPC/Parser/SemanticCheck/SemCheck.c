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
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#endif
#include <math.h>
#include "SemCheck.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../Optimizer/optimizer.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"
#include "../ParseTree/type_tags.h"
#include "../ParseTree/GpcType.h"
#include "../ParseTree/from_cparser.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"
#include <stdarg.h>

static int map_var_type_to_type_tag(enum VarType var_type)
{
    switch (var_type)
    {
        case HASHVAR_INTEGER:
            return INT_TYPE;
        case HASHVAR_LONGINT:
            return LONGINT_TYPE;
        case HASHVAR_REAL:
            return REAL_TYPE;
        case HASHVAR_BOOLEAN:
            return BOOL;
        case HASHVAR_CHAR:
            return CHAR_TYPE;
        case HASHVAR_PCHAR:
            return STRING_TYPE;
        default:
            return UNKNOWN_TYPE;
    }
}

/* Adds built-in functions */
void semcheck_add_builtins(SymTab_t *symtab);

/* Helper function to print semantic error with source code context */
void semantic_error(int line_num, int col_num, const char *format, ...)
{
    const char *file_path = (file_to_parse != NULL && *file_to_parse != '\0') ? file_to_parse : NULL;
    
    /* Print the error message */
    fprintf(stderr, "Error on line %d", line_num);
    if (col_num > 0)
        fprintf(stderr, ", column %d", col_num);
    fprintf(stderr, ": ");
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    
    /* Print source code context if we have a file */
    if (file_path != NULL && line_num > 0)
    {
        print_source_context(file_path, line_num, col_num, 2);
    }
    
    fprintf(stderr, "\n");
}

/* Main is a special keyword at the moment for code generation */
int semcheck_id_not_main(char *id)
{
    if(pascal_identifier_equals(id, "main"))
    {
        fprintf(stderr, "ERROR: main is special keyword, it cannot be a program, or subprogram\n");
        return 1;
    }
    return 0;
}

static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias)
{
    if (symtab == NULL || id == NULL || alias == NULL)
        return;

    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, (char *)id) != -1 && node != NULL)
    {
        if (node->mangled_id != NULL)
            free(node->mangled_id);
        node->mangled_id = strdup(alias);
    }
}

/* Helper function to get TypeAlias from HashNode, preferring GpcType when available */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    if (node == NULL)
        return NULL;
    
    /* Use hashnode helper which handles NULL GpcType */
    return hashnode_get_type_alias(node);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    /* Use hashnode helper which handles NULL GpcType */
    return hashnode_get_record_type(node);
}

/* Helper function to get VarType from HashNode */
static inline enum VarType get_var_type_from_node(HashNode_t *node)
{
    if (node == NULL || node->type == NULL)
        return HASHVAR_UNTYPED;

    switch (node->type->kind)
    {
        case TYPE_KIND_PRIMITIVE:
        {
            int tag = gpc_type_get_primitive_tag(node->type);
            switch (tag)
            {
                case INT_TYPE: return HASHVAR_INTEGER;
                case LONGINT_TYPE: return HASHVAR_LONGINT;
                case REAL_TYPE: return HASHVAR_REAL;
                case BOOL: return HASHVAR_BOOLEAN;
                case CHAR_TYPE: return HASHVAR_CHAR;
                case STRING_TYPE: return HASHVAR_PCHAR;
                case SET_TYPE: return HASHVAR_SET;
                case ENUM_TYPE: return HASHVAR_ENUM;
                case FILE_TYPE: return HASHVAR_FILE;
                default: return HASHVAR_UNTYPED;
            }
        }
        case TYPE_KIND_POINTER:
            return HASHVAR_POINTER;
        case TYPE_KIND_ARRAY:
            return HASHVAR_ARRAY;
        case TYPE_KIND_RECORD:
            return HASHVAR_RECORD;
        case TYPE_KIND_PROCEDURE:
            return HASHVAR_PROCEDURE;
        default:
            return HASHVAR_UNTYPED;
    }
}

static inline void mark_hashnode_unit_info(HashNode_t *node, int defined_in_unit, int is_public)
{
    if (node == NULL || !defined_in_unit)
        return;
    node->defined_in_unit = 1;
    node->unit_is_public = is_public ? 1 : 0;
}

static Tree_t *g_semcheck_current_subprogram = NULL;

void semcheck_mark_static_link_needed(int scope_level, HashNode_t *node)
{
    if (scope_level <= 0)
        return;
    if (g_semcheck_current_subprogram == NULL || node == NULL)
        return;

    switch (node->hash_type)
    {
        case HASHTYPE_VAR:
        case HASHTYPE_ARRAY:
        case HASHTYPE_FUNCTION_RETURN:
        case HASHTYPE_CONST:
            g_semcheck_current_subprogram->tree_data.subprogram_data.requires_static_link = 1;
            break;
        default:
            break;
    }
}

void semcheck_mark_call_requires_static_link(HashNode_t *callee)
{
    if (callee == NULL)
        return;
    if (g_semcheck_current_subprogram == NULL)
        return;
    if (!hashnode_requires_static_link(callee))
        return;
    g_semcheck_current_subprogram->tree_data.subprogram_data.requires_static_link = 1;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);

/* Resolve the return type for a function declaration once so callers share the same GpcType. */
HashNode_t *semcheck_find_type_node_with_gpc_type(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    HashNode_t *result = NULL;
    ListNode_t *all_nodes = FindAllIdents(symtab, (char *)type_id);
    ListNode_t *cur = all_nodes;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL)
        {
            if (node->type != NULL)
            {
                result = node;
                break;
            }
            if (result == NULL)
                result = node;
        }
        cur = cur->next;
    }

    if (all_nodes != NULL)
        DestroyList(all_nodes);

    return result;
}

static GpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab,
    int *error_count)
{
    if (subprogram == NULL || symtab == NULL)
        return NULL;

    /* TODO: Once the symbol table tracks placeholder types, this helper should
     * validate that any returned GpcType has been fully resolved. */
    HashNode_t *type_node = NULL;
    if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
    {
        type_node = semcheck_find_type_node_with_gpc_type(symtab, subprogram->tree_data.subprogram_data.return_type_id);
        if (type_node == NULL)
        {
            semantic_error(subprogram->line_num, 0, "undefined type %s",
                subprogram->tree_data.subprogram_data.return_type_id);
            if (error_count != NULL)
                ++(*error_count);
        }
    }

    return gpc_type_build_function_return(
        subprogram->tree_data.subprogram_data.inline_return_type,
        type_node,
        subprogram->tree_data.subprogram_data.return_type,
        symtab);
}

/* Helper to check if an expression contains a real number literal or real constant */
static int expression_contains_real_literal_impl(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    if (expr->type == EXPR_RNUM)
        return 1;
    
    if (expr->type == EXPR_VAR_ID && symtab != NULL)
    {
        /* Check if this variable ID refers to a real constant */
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 &&
            node != NULL && node->hash_type == HASHTYPE_CONST &&
            node->type != NULL && gpc_type_equals_tag(node->type, REAL_TYPE))
        {
            return 1;
        }
        return 0;
    }
    
    if (expr->type == EXPR_SIGN_TERM)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.sign_term);
    }
    
    if (expr->type == EXPR_ADDOP)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.addop_data.left_expr) ||
               expression_contains_real_literal_impl(symtab, expr->expr_data.addop_data.right_term);
    }
    
    if (expr->type == EXPR_MULOP)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.mulop_data.left_term) ||
               expression_contains_real_literal_impl(symtab, expr->expr_data.mulop_data.right_factor);
    }
    
    return 0;
}

/* Helper to check if an expression is a string expression */
static int expression_is_string(struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    if (expr->type == EXPR_STRING || expr->type == EXPR_CHAR_CODE)
        return 1;
    
    if (expr->type == EXPR_ADDOP && expr->expr_data.addop_data.addop_type == PLUS)
    {
        /* String concatenation */
        return expression_is_string(expr->expr_data.addop_data.left_expr) ||
               expression_is_string(expr->expr_data.addop_data.right_term);
    }
    
    return 0;
}

/* Evaluates a string constant expression, allocating a new string */
static int evaluate_string_const_expr(SymTab_t *symtab, struct Expression *expr, char **out_value)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_STRING:
            *out_value = strdup(expr->expr_data.string);
            return (*out_value == NULL) ? 1 : 0;
        
        case EXPR_CHAR_CODE:
        {
            /* Character code: convert to a single-character string */
            *out_value = (char *)malloc(2);
            if (*out_value == NULL)
                return 1;
            (*out_value)[0] = (char)(expr->expr_data.char_code & 0xFF);
            (*out_value)[1] = '\0';
            return 0;
        }
        
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL && 
                node->hash_type == HASHTYPE_CONST && node->const_string_value != NULL)
            {
                *out_value = strdup(node->const_string_value);
                return (*out_value == NULL) ? 1 : 0;
            }
            fprintf(stderr, "Error: constant %s is undefined or not a string const.\n", expr->expr_data.id);
            return 1;
        }
        
        case EXPR_ADDOP:
        {
            if (expr->expr_data.addop_data.addop_type != PLUS)
            {
                fprintf(stderr, "Error: only + operator is supported for string concatenation.\n");
                return 1;
            }
            
            char *left = NULL, *right = NULL;
            if (evaluate_string_const_expr(symtab, expr->expr_data.addop_data.left_expr, &left) != 0)
                return 1;
            if (evaluate_string_const_expr(symtab, expr->expr_data.addop_data.right_term, &right) != 0)
            {
                free(left);
                return 1;
            }
            
            /* Concatenate strings */
            size_t len = strlen(left) + strlen(right) + 1;
            *out_value = (char *)malloc(len);
            if (*out_value == NULL)
            {
                free(left);
                free(right);
                return 1;
            }
            strcpy(*out_value, left);
            strcat(*out_value, right);
            
            free(left);
            free(right);
            return 0;
        }
        
        default:
            break;
    }

    fprintf(stderr, "Error: unsupported string const expression.\n");
    return 1;
}

static int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_RNUM:
            *out_value = (double)expr->expr_data.r_num;
            return 0;
        case EXPR_INUM:
            /* Integer in real context - promote to real */
            *out_value = (double)expr->expr_data.i_num;
            return 0;
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL && node->hash_type == HASHTYPE_CONST)
            {
                if (node->type != NULL && gpc_type_equals_tag(node->type, REAL_TYPE))
                {
                    *out_value = node->const_real_value;
                    return 0;
                }
                *out_value = (double)node->const_int_value;
                return 0;
            }
            fprintf(stderr, "Error: constant %s is undefined or not a const.\n", expr->expr_data.id);
            return 1;
        }
        case EXPR_SIGN_TERM:
        {
            double value;
            if (evaluate_real_const_expr(symtab, expr->expr_data.sign_term, &value) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_ADDOP:
        {
            double left, right;
            if (evaluate_real_const_expr(symtab, expr->expr_data.addop_data.left_expr, &left) != 0)
                return 1;
            if (evaluate_real_const_expr(symtab, expr->expr_data.addop_data.right_term, &right) != 0)
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
            double left, right;
            if (evaluate_real_const_expr(symtab, expr->expr_data.mulop_data.left_term, &left) != 0)
                return 1;
            if (evaluate_real_const_expr(symtab, expr->expr_data.mulop_data.right_factor, &right) != 0)
                return 1;
            switch (expr->expr_data.mulop_data.mulop_type)
            {
                case STAR:
                    *out_value = left * right;
                    return 0;
                case SLASH:
                    if (right == 0.0)
                    {
                        fprintf(stderr, "Error: division by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left / right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        default:
            break;
    }

    fprintf(stderr, "Error: unsupported real const expression.\n");
    return 1;
}

static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_RNUM:
            /* Real numbers in integer context - truncate or error */
            fprintf(stderr, "Error: real number constant in integer context.\n");
            return 1;
        case EXPR_INUM:
            *out_value = expr->expr_data.i_num;
            return 0;
        case EXPR_BOOL:
            *out_value = expr->expr_data.bool_value ? 1 : 0;
            return 0;
        case EXPR_STRING:
            /* Handle character literals in const expressions */
            if (expr->expr_data.string != NULL && 
                expr->expr_data.string[0] != '\0' && 
                expr->expr_data.string[1] == '\0')
            {
                /* Single character literal - return its ASCII value */
                *out_value = (unsigned char)expr->expr_data.string[0];
                return 0;
            }
            fprintf(stderr, "Error: string literal in const expression must be a single character.\n");
            return 1;
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
        case EXPR_SET:
        {
            unsigned long long mask = 0;
            ListNode_t *element = expr->expr_data.set_data.elements;
            while (element != NULL)
            {
                if (element->cur == NULL)
                {
                    element = element->next;
                    continue;
                }

                struct SetElement *set_element = (struct SetElement *)element->cur;
                long long lower = 0;
                long long upper = 0;
                if (set_element->lower == NULL ||
                    evaluate_const_expr(symtab, set_element->lower, &lower) != 0)
                {
                    return 1;
                }

                if (set_element->upper != NULL)
                {
                    if (evaluate_const_expr(symtab, set_element->upper, &upper) != 0)
                        return 1;
                }
                else
                {
                    upper = lower;
                }

                if (lower > upper)
                {
                    long long tmp = lower;
                    lower = upper;
                    upper = tmp;
                }

                for (long long value = lower; value <= upper; ++value)
                {
                    if (value < 0 || value >= 32)
                    {
                        fprintf(stderr, "Error: set literal value %lld out of supported range 0..31.\n", value);
                        return 1;
                    }
                    mask |= (1ull << value);
                }

                element = element->next;
            }

            *out_value = (long long)mask;
            return 0;
        }
        case EXPR_FUNCTION_CALL:
        {
            /* Handle Ord() function for constant expressions */
            char *id = expr->expr_data.function_call_data.id;
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            
            if (id != NULL && pascal_identifier_equals(id, "Ord"))
            {
                /* Check that we have exactly one argument */
                if (args == NULL || args->next != NULL)
                {
                    fprintf(stderr, "Error: Ord in const expression requires exactly one argument.\n");
                    return 1;
                }
                
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg == NULL)
                {
                    fprintf(stderr, "Error: Ord argument is NULL.\n");
                    return 1;
                }
                
                /* Handle character literal */
                if (arg->type == EXPR_STRING)
                {
                    char *literal = arg->expr_data.string;
                    if (literal == NULL || literal[0] == '\0')
                    {
                        fprintf(stderr, "Error: Ord expects a non-empty character literal.\n");
                        return 1;
                    }
                    if (literal[1] != '\0')
                    {
                        fprintf(stderr, "Error: Ord expects a single character literal.\n");
                        return 1;
                    }
                    *out_value = (unsigned char)literal[0];
                    return 0;
                }
                /* Handle character code literal */
                else if (arg->type == EXPR_CHAR_CODE)
                {
                    *out_value = (unsigned char)arg->expr_data.char_code;
                    return 0;
                }
                /* Handle boolean literal */
                else if (arg->type == EXPR_BOOL)
                {
                    *out_value = arg->expr_data.bool_value ? 1 : 0;
                    return 0;
                }
                /* Handle integer literal */
                else if (arg->type == EXPR_INUM)
                {
                    *out_value = arg->expr_data.i_num;
                    return 0;
                }
                /* Handle const variable reference */
                else if (arg->type == EXPR_VAR_ID)
                {
                    HashNode_t *node = NULL;
                    if (FindIdent(&node, symtab, arg->expr_data.id) >= 0 && 
                        node != NULL && node->hash_type == HASHTYPE_CONST)
                    {
                        *out_value = node->const_int_value;
                        return 0;
                    }
                    fprintf(stderr, "Error: Ord argument %s is not a constant.\n", arg->expr_data.id);
                    return 1;
                }
                /* Handle nested const expressions */
                else
                {
                    long long arg_value;
                    if (evaluate_const_expr(symtab, arg, &arg_value) == 0)
                    {
                        *out_value = arg_value;
                        return 0;
                    }
                    fprintf(stderr, "Error: Ord argument is not a valid const expression.\n");
                    return 1;
                }
            }
            
            fprintf(stderr, "Error: only Ord() function calls are supported in const expressions.\n");
            return 1;
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
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                struct TypeAlias *alias_info = &tree->tree_data.type_decl_data.info.alias;
                if (alias_info != NULL && alias_info->is_enum && alias_info->enum_literals != NULL)
                {
                    /* Create ONE shared GpcType for this enum type if not already created */
                    if (alias_info->gpc_type == NULL)
                    {
                        alias_info->gpc_type = create_primitive_type(ENUM_TYPE);
                        if (alias_info->gpc_type == NULL)
                        {
                            fprintf(stderr, "Error: Failed to create enum type for %s\n",
                                    tree->tree_data.type_decl_data.id);
                            ++errors;
                        }
                    }
                    
                    if (alias_info->gpc_type != NULL)
                    {
                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->enum_literals;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                /* Use typed API with shared enum GpcType - all literals reference same type */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, alias_info->gpc_type) > 0)
                                {
                                    fprintf(stderr,
                                            "Error on line %d, redeclaration of enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        /* GpcType is owned by TypeAlias, will be cleaned up when tree is destroyed */
                    }
                }
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Helper function to check if a statement contains an asm block */
static int statement_contains_asm_block(struct Statement *stmt)
{
    if (stmt == NULL)
        return 0;
    
    if (stmt->type == STMT_ASM_BLOCK)
        return 1;
    
    if (stmt->type == STMT_COMPOUND_STATEMENT)
    {
        ListNode_t *cur = stmt->stmt_data.compound_statement;
        while (cur != NULL)
        {
            if (cur->type == LIST_STMT && cur->cur != NULL)
            {
                struct Statement *child_stmt = (struct Statement *)cur->cur;
                if (statement_contains_asm_block(child_stmt))
                    return 1;
            }
            cur = cur->next;
        }
    }
    
    return 0;
}

static int record_has_property(struct RecordType *record_info, const char *property_name)
{
    if (record_info == NULL || property_name == NULL)
        return 0;

    ListNode_t *node = record_info->properties;
    while (node != NULL)
    {
        if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)node->cur;
            if (property->name != NULL &&
                pascal_identifier_equals(property->name, property_name))
                return 1;
        }
        node = node->next;
    }
    return 0;
}

static struct ClassProperty *clone_class_property(const struct ClassProperty *property)
{
    if (property == NULL)
        return NULL;

    struct ClassProperty *clone = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    if (clone == NULL)
        return NULL;

    clone->name = property->name != NULL ? strdup(property->name) : NULL;
    clone->type = property->type;
    clone->type_id = property->type_id != NULL ? strdup(property->type_id) : NULL;
    clone->read_accessor = property->read_accessor != NULL ? strdup(property->read_accessor) : NULL;
    clone->write_accessor = property->write_accessor != NULL ? strdup(property->write_accessor) : NULL;

    if ((property->name != NULL && clone->name == NULL) ||
        (property->type_id != NULL && clone->type_id == NULL) ||
        (property->read_accessor != NULL && clone->read_accessor == NULL) ||
        (property->write_accessor != NULL && clone->write_accessor == NULL))
    {
        free(clone->name);
        free(clone->type_id);
        free(clone->read_accessor);
        free(clone->write_accessor);
        free(clone);
        return NULL;
    }

    return clone;
}

static ListNode_t *clone_property_list_unique(struct RecordType *record_info,
    const ListNode_t *properties)
{
    ListNode_t *head = NULL;
    ListNode_t **tail = &head;

    while (properties != NULL)
    {
        if (properties->type == LIST_CLASS_PROPERTY && properties->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)properties->cur;
            if (!record_has_property(record_info, property->name))
            {
                struct ClassProperty *clone = clone_class_property(property);
                if (clone != NULL)
                {
                    ListNode_t *node = CreateListNode(clone, LIST_CLASS_PROPERTY);
                    if (node != NULL)
                    {
                        *tail = node;
                        tail = &node->next;
                    }
                    else
                    {
                        free(clone->name);
                        free(clone->type_id);
                        free(clone->read_accessor);
                        free(clone->write_accessor);
                        free(clone);
                    }
                }
            }
        }
        properties = properties->next;
    }

    return head;
}

/* Helper function to check for circular inheritance */
static int check_circular_inheritance(SymTab_t *symtab, const char *class_name, const char *parent_name, int max_depth)
{
    if (class_name == NULL || parent_name == NULL)
        return 0;
    
    /* Check if parent is the same as this class (direct circular reference) */
    if (strcmp(class_name, parent_name) == 0)
        return 1;
    
    /* Prevent infinite recursion by limiting depth */
    if (max_depth <= 0)
        return 1;
    
    /* Look up parent class */
    HashNode_t *parent_node = NULL;
    if (FindIdent(&parent_node, symtab, (char *)parent_name) == -1 || parent_node == NULL)
        return 0;  /* Parent not found yet, not necessarily circular */
    
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL || parent_record->parent_class_name == NULL)
        return 0;  /* Parent has no parent, no circular reference */
    
    /* Recursively check parent's parent */
    return check_circular_inheritance(symtab, class_name, parent_record->parent_class_name, max_depth - 1);
}

static int add_class_padding_field(struct RecordType *record_info, long long padding_bytes)
{
    if (record_info == NULL || padding_bytes <= 0)
        return 0;

    struct RecordField *padding = (struct RecordField *)calloc(1, sizeof(struct RecordField));
    if (padding == NULL)
        return 1;

    padding->name = NULL;
    padding->type = CHAR_TYPE;
    padding->is_array = 1;
    padding->array_start = 0;
    padding->array_end = (int)padding_bytes - 1;
    padding->array_element_type = CHAR_TYPE;
    padding->array_is_open = 0;
    padding->is_hidden = 1;

    ListNode_t *node = CreateListNode(padding, LIST_RECORD_FIELD);
    record_info->fields = PushListNodeBack(record_info->fields, node);
    return 0;
}

static int compute_class_record_size(SymTab_t *symtab, struct RecordType *record_info,
    long long *size_out, int line_num)
{
    if (record_info == NULL || size_out == NULL)
        return 1;

    return semcheck_compute_record_size(symtab, record_info, size_out,
        line_num >= 0 ? line_num : 0);
}

static int ensure_class_storage_capacity(SymTab_t *symtab, struct RecordType *record_info,
    long long required_size, int line_num)
{
    if (record_info == NULL)
        return 0;

    long long current_size = 0;
    if (compute_class_record_size(symtab, record_info, &current_size, line_num) != 0)
        return 1;

    if (current_size >= required_size)
        return 0;

    long long padding_bytes = required_size - current_size;
    return add_class_padding_field(record_info, padding_bytes);
}

/* Helper function to merge parent class fields into derived class */
static int merge_parent_class_fields(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num)
{
    if (record_info == NULL || record_info->parent_class_name == NULL)
        return 0;  /* No parent class to merge */
    
    /* Check for circular inheritance */
    if (check_circular_inheritance(symtab, class_name, record_info->parent_class_name, 100))
    {
        fprintf(stderr, "Error on line %d, circular inheritance detected for class '%s'!\n",
                line_num, class_name ? class_name : "<unknown>");
        return 1;
    }
    
    /* Look up parent class in symbol table */
    HashNode_t *parent_node = NULL;
    if (FindIdent(&parent_node, symtab, record_info->parent_class_name) == -1 || parent_node == NULL)
    {
        fprintf(stderr, "Error on line %d, parent class '%s' not found!\n", 
                line_num, record_info->parent_class_name);
        return 1;
    }
    
    /* Get parent's RecordType */
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL)
    {
        fprintf(stderr, "Error on line %d, parent class '%s' is not a class/record type!\n",
                line_num, record_info->parent_class_name);
        return 1;
    }
    
    /* Clone parent's fields and prepend them to this record's fields */
    ListNode_t *parent_fields = parent_record->fields;
    if (parent_fields != NULL)
    {
        /* We need to clone the parent's field list and prepend it */
        ListNode_t *cloned_parent_fields = NULL;
        ListNode_t *cur = parent_fields;
        ListNode_t *last_cloned = NULL;
        
        while (cur != NULL)
        {
            assert(cur->type == LIST_RECORD_FIELD);
            struct RecordField *original_field = (struct RecordField *)cur->cur;
            
            /* Clone the field */
            struct RecordField *cloned_field = (struct RecordField *)malloc(sizeof(struct RecordField));
            if (cloned_field == NULL)
            {
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    free(field->array_element_type_id);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            cloned_field->name = original_field->name ? strdup(original_field->name) : NULL;
            cloned_field->type = original_field->type;
            cloned_field->type_id = original_field->type_id ? strdup(original_field->type_id) : NULL;
            cloned_field->nested_record = original_field->nested_record;  /* Share the nested record */
            cloned_field->is_array = original_field->is_array;
            cloned_field->array_start = original_field->array_start;
            cloned_field->array_end = original_field->array_end;
            cloned_field->array_element_type = original_field->array_element_type;
            cloned_field->array_element_type_id = original_field->array_element_type_id ? 
                strdup(original_field->array_element_type_id) : NULL;
            cloned_field->array_is_open = original_field->array_is_open;
            cloned_field->is_hidden = original_field->is_hidden;
            
            /* Create list node for cloned field */
            ListNode_t *new_node = (ListNode_t *)malloc(sizeof(ListNode_t));
            if (new_node == NULL)
            {
                free(cloned_field->name);
                free(cloned_field->type_id);
                free(cloned_field->array_element_type_id);
                free(cloned_field);
                
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    free(field->array_element_type_id);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            new_node->type = LIST_RECORD_FIELD;
            new_node->cur = cloned_field;
            new_node->next = NULL;
            
            /* Append to cloned list */
            if (cloned_parent_fields == NULL)
            {
                cloned_parent_fields = new_node;
                last_cloned = new_node;
            }
            else
            {
                last_cloned->next = new_node;
                last_cloned = new_node;
            }
            
            cur = cur->next;
        }
        
        /* Prepend cloned parent fields to this record's fields */
        if (last_cloned != NULL)
        {
            last_cloned->next = record_info->fields;
            record_info->fields = cloned_parent_fields;
        }
    }

    if (parent_record != NULL && parent_record->properties != NULL)
    {
        ListNode_t *cloned_properties = clone_property_list_unique(record_info,
            parent_record->properties);
        if (cloned_properties != NULL)
            record_info->properties = ConcatList(cloned_properties, record_info->properties);
    }

    if (parent_record != NULL)
    {
        long long derived_size = 0;
        if (compute_class_record_size(symtab, record_info, &derived_size, line_num) == 0)
            ensure_class_storage_capacity(symtab, parent_record, derived_size, line_num);
    }
    
    return 0;
}

/* Build Virtual Method Table for a class */
static int build_class_vmt(SymTab_t *symtab, struct RecordType *record_info, 
                            const char *class_name, int line_num) {
    if (record_info == NULL || class_name == NULL)
        return 0;
    
    /* Get methods registered for this class */
    ListNode_t *class_methods = NULL;
    int method_count = 0;
    get_class_methods(class_name, &class_methods, &method_count);
    
    
    /* Start with parent's VMT if this class has a parent */
    ListNode_t *vmt = NULL;
    int vmt_size = 0;
    
if (record_info->parent_class_name != NULL) {
        /* Look up parent class */
        HashNode_t *parent_node = NULL;
        if (FindIdent(&parent_node, symtab, record_info->parent_class_name) != -1 && 
            parent_node != NULL) {
            struct RecordType *parent_record = get_record_type_from_node(parent_node);
            if (parent_record != NULL && parent_record->methods != NULL) {
                /* Clone parent's VMT */
                ListNode_t *parent_vmt = parent_record->methods;
                ListNode_t **tail = &vmt;
                
                while (parent_vmt != NULL) {
                    struct MethodInfo *parent_method = (struct MethodInfo *)parent_vmt->cur;
                    if (parent_method != NULL) {
                        struct MethodInfo *cloned = (struct MethodInfo *)malloc(sizeof(struct MethodInfo));
                        if (cloned != NULL) {
                            cloned->name = parent_method->name ? strdup(parent_method->name) : NULL;
                            cloned->mangled_name = parent_method->mangled_name ? strdup(parent_method->mangled_name) : NULL;
                            cloned->is_virtual = parent_method->is_virtual;
                            cloned->is_override = 0;  /* Parent's methods aren't overrides in child */
                            cloned->vmt_index = parent_method->vmt_index;
                            
                            ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
                            if (node != NULL) {
                                node->type = LIST_UNSPECIFIED;
                                node->cur = cloned;
                                node->next = NULL;
                                *tail = node;
                                tail = &node->next;
                                vmt_size++;
                            } else {
                                free(cloned->name);
                                free(cloned->mangled_name);
                                free(cloned);
                            }
                        }
                    }
                    parent_vmt = parent_vmt->next;
                }
            }
        }
    }
    
    /* Process this class's methods */
    ListNode_t *cur_method = class_methods;
    while (cur_method != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur_method->cur;
        if (binding != NULL && binding->method_name != NULL) {
            /* Build mangled name: ClassName__MethodName */
            size_t class_len = strlen(class_name);
            size_t method_len = strlen(binding->method_name);
            char *mangled = (char *)malloc(class_len + 2 + method_len + 1);
            if (mangled != NULL) {
                snprintf(mangled, class_len + 2 + method_len + 1, "%s__%s", 
                         class_name, binding->method_name);
            }
            
            /* Check if this method overrides a parent method */
            int is_actual_override = 0;
            if (binding->is_virtual || binding->is_override) {
                /* Check if a method with this name exists in the parent VMT */
                ListNode_t *vmt_entry = vmt;
                while (vmt_entry != NULL) {
                    struct MethodInfo *info = (struct MethodInfo *)vmt_entry->cur;
                    if (info != NULL && info->name != NULL &&
                        strcasecmp(info->name, binding->method_name) == 0) {
                        /* Method exists in parent - this is an override */
                        is_actual_override = 1;
                        /* Replace with derived class's version */
                        free(info->mangled_name);
                        info->mangled_name = mangled ? strdup(mangled) : NULL;
                        info->is_override = 1;
                        break;
                    }
                    vmt_entry = vmt_entry->next;
                }
            }
            
            if (is_actual_override) {
                /* Method was found and replaced in parent VMT - nothing more to do */
            } else if (binding->is_virtual || binding->is_override) {
                /* Add new virtual method to VMT */
                /* Note: if binding->is_override is true but no parent method was found,
                 * this is an error, but we still add it as a new virtual method */
                if (binding->is_override) {
                    fprintf(stderr, "Warning on line %d: override method '%s' has no virtual parent method, treating as new virtual method\n",
                            line_num, binding->method_name);
                }
                
                struct MethodInfo *new_method = (struct MethodInfo *)malloc(sizeof(struct MethodInfo));
                if (new_method != NULL) {
                    new_method->name = binding->method_name ? strdup(binding->method_name) : NULL;
                    new_method->mangled_name = mangled ? strdup(mangled) : NULL;
                    new_method->is_virtual = 1;
                    new_method->is_override = 0;
                    new_method->vmt_index = vmt_size;
                    
                    ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
                    if (node != NULL) {
                        node->type = LIST_UNSPECIFIED;
                        node->cur = new_method;
                        node->next = NULL;
                        
                        /* Append to end */
                        if (vmt == NULL) {
                            vmt = node;
                        } else {
                            ListNode_t *last = vmt;
                            while (last->next != NULL)
                                last = last->next;
                            last->next = node;
                        }
                        vmt_size++;
                    } else {
                        free(new_method->name);
                        free(new_method->mangled_name);
                        free(new_method);
                    }
                }
            }
            
            free(mangled);
        }
        cur_method = cur_method->next;
    }
    
    /* Store VMT in record */
    record_info->methods = vmt;
    
    
    /* Clean up class_methods list (shallow - we don't own the bindings) */
    while (class_methods != NULL) {
        ListNode_t *next = class_methods->next;
        free(class_methods);
        class_methods = next;
    }
    
    return 0;
}

/* Helper function to resolve constant identifier to integer value
 * Returns 0 on success, 1 on failure */
static int resolve_const_identifier(SymTab_t *symtab, const char *id, long long *out_value)
{
    if (symtab == NULL || id == NULL || out_value == NULL)
        return 1;
    
    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, (char *)id) >= 0 && 
        node != NULL && node->hash_type == HASHTYPE_CONST)
    {
        *out_value = node->const_int_value;
        return 0;
    }
    
    return 1;
}

/* Resolves array bounds specified as constant identifiers in a GpcType
 * This is needed because parsing happens before constants are declared */
static void resolve_array_bounds_in_gpctype(SymTab_t *symtab, GpcType *gpc_type, struct TypeAlias *alias)
{
    if (gpc_type == NULL || alias == NULL || symtab == NULL)
        return;
    
    /* Only process array types */
    if (gpc_type->kind != TYPE_KIND_ARRAY)
        return;
    
    /* Check if we have array_dimensions with symbolic bounds */
    if (alias->array_dimensions != NULL)
    {
        /* Parse the first dimension string (format: "start..end") */
        ListNode_t *first_dim = alias->array_dimensions;
        if (first_dim != NULL && first_dim->type == LIST_STRING && first_dim->cur != NULL)
        {
            char *dim_str = (char *)first_dim->cur;
            char *separator = strstr(dim_str, "..");
            
            if (separator != NULL)
            {
                /* Extract start and end parts */
                size_t start_len = separator - dim_str;
                char *start_str = (char *)malloc(start_len + 1);
                char *end_str = strdup(separator + 2);
                
                if (start_str != NULL && end_str != NULL)
                {
                    strncpy(start_str, dim_str, start_len);
                    start_str[start_len] = '\0';
                    
                    /* Trim whitespace */
                    while (*start_str == ' ') start_str++;
                    while (*end_str == ' ') end_str++;
                    
                    /* Try to resolve as constants or parse as integers */
                    long long start_val = 0;
                    long long end_val = 0;
                    int start_resolved = 0;
                    int end_resolved = 0;
                    
                    /* Try constant lookup first */
                    if (resolve_const_identifier(symtab, start_str, &start_val) == 0)
                    {
                        start_resolved = 1;
                    }
                    else
                    {
                        /* Try parsing as integer */
                        char *endptr;
                        long val = strtol(start_str, &endptr, 10);
                        if (endptr != start_str && *endptr == '\0')
                        {
                            start_val = val;
                            start_resolved = 1;
                        }
                    }
                    
                    if (resolve_const_identifier(symtab, end_str, &end_val) == 0)
                    {
                        end_resolved = 1;
                    }
                    else
                    {
                        /* Try parsing as integer */
                        char *endptr;
                        long val = strtol(end_str, &endptr, 10);
                        if (endptr != end_str && *endptr == '\0')
                        {
                            end_val = val;
                            end_resolved = 1;
                        }
                    }
                    
                    /* Update GpcType with resolved bounds */
                    if (start_resolved && end_resolved)
                    {
                        gpc_type->info.array_info.start_index = (int)start_val;
                        gpc_type->info.array_info.end_index = (int)end_val;
                        
                        /* Also update the TypeAlias for consistency */
                        alias->array_start = (int)start_val;
                        alias->array_end = (int)end_val;
                        
                        /* Re-evaluate is_open_array based on resolved bounds */
                        alias->is_open_array = (alias->array_end < alias->array_start);
                    }
                    
                    free(start_str);
                    free(end_str);
                }
            }
        }
    }
}

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

        struct RecordType *record_info = NULL;
        struct TypeAlias *alias_info = NULL;
        


        switch (tree->tree_data.type_decl_data.kind)
        {
            case TYPE_DECL_RECORD:
                var_type = HASHVAR_RECORD;
                record_info = tree->tree_data.type_decl_data.info.record;
                
                /* Handle class inheritance - merge parent fields */
                if (record_info != NULL && record_info->parent_class_name != NULL)
                {
                    int merge_result = merge_parent_class_fields(symtab, record_info, 
                                                                  tree->tree_data.type_decl_data.id, 
                                                                  tree->line_num);
                    if (merge_result > 0)
                    {
                        return_val += merge_result;
                        /* Continue processing other type declarations even if this one failed */
                    }
                }
                
                /* Build VMT for classes with virtual methods */
                if (record_info != NULL)
                {
                    int vmt_result = build_class_vmt(symtab, record_info,
                                                      tree->tree_data.type_decl_data.id,
                                                      tree->line_num);
                    if (vmt_result > 0)
                    {
                        return_val += vmt_result;
                    }
                }
                break;
            case TYPE_DECL_ALIAS:
            {
                alias_info = &tree->tree_data.type_decl_data.info.alias;

                if (alias_info->is_array)
                {
                    int element_type = alias_info->array_element_type;
                    if (element_type == REAL_TYPE)
                        var_type = HASHVAR_REAL;
                    else if (element_type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if (element_type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if (element_type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if (element_type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if (element_type == POINTER_TYPE)
                        var_type = HASHVAR_POINTER;
                    else if (element_type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if (element_type == ENUM_TYPE)
                        var_type = HASHVAR_ENUM;
                    else if (element_type == FILE_TYPE)
                        var_type = HASHVAR_FILE;
                    else if (element_type == TEXT_TYPE)
                        var_type = HASHVAR_TEXT;
                    else
                        var_type = HASHVAR_INTEGER;
                }
                else
                {
                    int base_type = alias_info->base_type;
                    if (base_type == REAL_TYPE)
                        var_type = HASHVAR_REAL;
                    else if (base_type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if (base_type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if (base_type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if (base_type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if (base_type == POINTER_TYPE)
                        var_type = HASHVAR_POINTER;
                    else if (base_type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if (base_type == ENUM_TYPE)
                        var_type = HASHVAR_ENUM;
                    else if (base_type == FILE_TYPE)
                        var_type = HASHVAR_FILE;
                    else if (base_type == TEXT_TYPE)
                        var_type = HASHVAR_TEXT;
                    else if (base_type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if (base_type == PROCEDURE)
                        var_type = HASHVAR_PROCEDURE;
                    else
                        var_type = HASHVAR_UNTYPED;

                    if (alias_info->is_pointer)
                        var_type = HASHVAR_POINTER;
                    else if (alias_info->is_set)
                        var_type = HASHVAR_SET;
                    else if (alias_info->is_enum)
                        var_type = HASHVAR_ENUM;
                    else if (alias_info->is_file)
                        var_type = HASHVAR_FILE;

                    if (var_type == HASHVAR_UNTYPED && alias_info->target_type_id != NULL)
                    {
                        HashNode_t *target_node = NULL;
                        if (FindIdent(&target_node, symtab, alias_info->target_type_id) != -1 && target_node != NULL)
                        {
                            var_type = get_var_type_from_node(target_node);
                        }
                    }
                }
                break;
            }
            default:
                var_type = HASHVAR_INTEGER;
                break;
        }

        GpcType *gpc_type = tree->tree_data.type_decl_data.gpc_type;



        if (gpc_type != NULL) {
            /* Set type_alias on GpcType before pushing */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL)
            {
                gpc_type_set_type_alias(gpc_type, alias_info);
                
                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_gpctype(symtab, gpc_type, alias_info);
                }
            }
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD && record_info != NULL && gpc_type->kind == TYPE_KIND_RECORD)
                gpc_type->info.record_info = record_info;
            
            func_return = PushTypeOntoScope_Typed(symtab, tree->tree_data.type_decl_data.id, gpc_type);
            if (func_return == 0)
            {
                /* GpcType ownership transferred to symbol table */
                tree->tree_data.type_decl_data.gpc_type = NULL;
                /* Note: var_type is automatically set from GpcType in HashTable.c via set_var_type_from_gpctype() */
            }
        } else {
        /* Fall back to legacy API for types we can't convert yet */
        func_return = PushTypeOntoScope(symtab, tree->tree_data.type_decl_data.id, var_type,
            record_info, alias_info);
        }

        /* Note: Enum literals are declared in predeclare_enum_literals() during first pass.
         * We don't redeclare them here to avoid "redeclaration" errors. */

        if(func_return > 0)
        {
            semantic_error(tree->line_num, 0, "redeclaration of name %s",
                tree->tree_data.type_decl_data.id);
            return_val += func_return;
        }
        else
        {
            HashNode_t *type_node = NULL;
            if (FindIdent(&type_node, symtab, tree->tree_data.type_decl_data.id) != -1 && type_node != NULL)
            {
                mark_hashnode_unit_info(type_node,
                    tree->tree_data.type_decl_data.defined_in_unit,
                    tree->tree_data.type_decl_data.unit_is_public);
            }
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

        struct Expression *value_expr = tree->tree_data.const_decl_data.value;
        
        /* Determine the type of constant by checking the expression */
        int is_string_const = expression_is_string(value_expr);
        int is_real_const = !is_string_const && expression_contains_real_literal_impl(symtab, value_expr);
        
        if (is_string_const)
        {
            /* Evaluate as string constant */
            char *string_value = NULL;
            if (evaluate_string_const_expr(symtab, value_expr, &string_value) != 0)
            {
                fprintf(stderr, "Error on line %d, unsupported string const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                int push_result = PushStringConstOntoScope(symtab, tree->tree_data.const_decl_data.id, string_value);
                free(string_value);  /* PushStringConstOntoScope makes its own copy */
                if (push_result > 0)
                {
                    fprintf(stderr, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (is_real_const)
        {
            /* Evaluate as real constant */
            double real_value = 0.0;
            if (evaluate_real_const_expr(symtab, value_expr, &real_value) != 0)
            {
                fprintf(stderr, "Error on line %d, unsupported real const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                int push_result = PushRealConstOntoScope(symtab, tree->tree_data.const_decl_data.id, real_value);
                if (push_result > 0)
                {
                    fprintf(stderr, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else
        {
            /* Evaluate as integer constant */
            long long value = 0;
            if (evaluate_const_expr(symtab, value_expr, &value) != 0)
            {
                fprintf(stderr, "Error on line %d, unsupported const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                /* Create GpcType if this is a set constant */
                GpcType *const_type = NULL;
                if (value_expr != NULL && value_expr->type == EXPR_SET)
                {
                    const_type = create_primitive_type(SET_TYPE);
                }
                
                /* Use typed or legacy API depending on whether we have a GpcType */
                int push_result;
                if (const_type != NULL)
                {
                    push_result = PushConstOntoScope_Typed(symtab, tree->tree_data.const_decl_data.id, value, const_type);
                }
                else
                {
                    push_result = PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, value);
                }
                
                if (push_result > 0)
                {
                    fprintf(stderr, "Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
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
        GpcType *pchar_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(pchar_type != NULL && "Failed to create PChar type");
        AddBuiltinType_Typed(symtab, pchar_name, pchar_type);
        destroy_gpc_type(pchar_type);
        free(pchar_name);
    }
    char *integer_name = strdup("integer");
    if (integer_name != NULL) {
        GpcType *integer_type = gpc_type_from_var_type(HASHVAR_INTEGER);
        assert(integer_type != NULL && "Failed to create integer type");
        AddBuiltinType_Typed(symtab, integer_name, integer_type);
        destroy_gpc_type(integer_type);
        free(integer_name);
    }
    char *longint_name = strdup("longint");
    if (longint_name != NULL) {
        GpcType *longint_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(longint_type != NULL && "Failed to create longint type");
        AddBuiltinType_Typed(symtab, longint_name, longint_type);
        destroy_gpc_type(longint_type);
        free(longint_name);
    }
    char *int64_name = strdup("int64");
    if (int64_name != NULL) {
        GpcType *int64_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(int64_type != NULL && "Failed to create int64 type");
        AddBuiltinType_Typed(symtab, int64_name, int64_type);
        destroy_gpc_type(int64_type);
        free(int64_name);
    }
    char *real_name = strdup("real");
    if (real_name != NULL) {
        GpcType *real_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(real_type != NULL && "Failed to create real type");
        AddBuiltinType_Typed(symtab, real_name, real_type);
        destroy_gpc_type(real_type);
        free(real_name);
    }
    char *single_name = strdup("single");
    if (single_name != NULL) {
        GpcType *single_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(single_type != NULL && "Failed to create single type");
        AddBuiltinType_Typed(symtab, single_name, single_type);
        destroy_gpc_type(single_type);
        free(single_name);
    }
    char *double_name = strdup("double");
    if (double_name != NULL) {
        GpcType *double_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(double_type != NULL && "Failed to create double type");
        AddBuiltinType_Typed(symtab, double_name, double_type);
        destroy_gpc_type(double_type);
        free(double_name);
    }
    char *string_name = strdup("string");
    if (string_name != NULL) {
        GpcType *string_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(string_type != NULL && "Failed to create string type");
        AddBuiltinType_Typed(symtab, string_name, string_type);
        destroy_gpc_type(string_type);
        free(string_name);
    }
    char *unicode_name = strdup("UnicodeString");
    if (unicode_name != NULL) {
        GpcType *unicode_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(unicode_type != NULL && "Failed to create UnicodeString type");
        AddBuiltinType_Typed(symtab, unicode_name, unicode_type);
        destroy_gpc_type(unicode_type);
        free(unicode_name);
    }
    char *boolean_name = strdup("boolean");
    if (boolean_name != NULL) {
        GpcType *boolean_type = gpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(boolean_type != NULL && "Failed to create boolean type");
        AddBuiltinType_Typed(symtab, boolean_name, boolean_type);
        destroy_gpc_type(boolean_type);
        free(boolean_name);
    }
    char *char_name = strdup("char");
    if (char_name != NULL) {
        GpcType *char_type = gpc_type_from_var_type(HASHVAR_CHAR);
        assert(char_type != NULL && "Failed to create char type");
        AddBuiltinType_Typed(symtab, char_name, char_type);
        destroy_gpc_type(char_type);
        free(char_name);
    }
    char *file_name = strdup("file");
    if (file_name != NULL) {
        GpcType *file_type = gpc_type_from_var_type(HASHVAR_FILE);
        assert(file_type != NULL && "Failed to create file type");
        AddBuiltinType_Typed(symtab, file_name, file_type);
        destroy_gpc_type(file_type);
        free(file_name);
    }
    char *text_name = strdup("text");
    if (text_name != NULL) {
        GpcType *text_type = gpc_type_from_var_type(HASHVAR_TEXT);
        assert(text_type != NULL && "Failed to create text type");
        AddBuiltinType_Typed(symtab, text_name, text_type);
        destroy_gpc_type(text_type);
        free(text_name);
    }
    char *pointer_name = strdup("Pointer");
    if (pointer_name != NULL) {
        GpcType *pointer_type = create_pointer_type(NULL); // Untyped pointer
        assert(pointer_type != NULL && "Failed to create Pointer type");
        AddBuiltinType_Typed(symtab, pointer_name, pointer_type);
        destroy_gpc_type(pointer_type);
        free(pointer_name);
    }

    AddBuiltinRealConst(symtab, "Pi", acos(-1.0));

    /* Builtin procedures - procedures have no return type */
    char *setlength_name = strdup("SetLength");
    if (setlength_name != NULL) {
        GpcType *setlength_type = create_procedure_type(NULL, NULL);
        assert(setlength_type != NULL && "Failed to create SetLength procedure type");
        AddBuiltinProc_Typed(symtab, setlength_name, setlength_type);
        destroy_gpc_type(setlength_type);
        free(setlength_name);
    }

    char *write_name = strdup("write");
    if (write_name != NULL) {
        GpcType *write_type = create_procedure_type(NULL, NULL);
        assert(write_type != NULL && "Failed to create write procedure type");
        AddBuiltinProc_Typed(symtab, write_name, write_type);
        destroy_gpc_type(write_type);
        free(write_name);
    }

    char *writeln_name = strdup("writeln");
    if (writeln_name != NULL) {
        GpcType *writeln_type = create_procedure_type(NULL, NULL);
        assert(writeln_type != NULL && "Failed to create writeln procedure type");
        AddBuiltinProc_Typed(symtab, writeln_name, writeln_type);
        destroy_gpc_type(writeln_type);
        free(writeln_name);
    }

    char *read_name = strdup("read");
    if (read_name != NULL) {
        GpcType *read_type = create_procedure_type(NULL, NULL);
        assert(read_type != NULL && "Failed to create read procedure type");
        AddBuiltinProc_Typed(symtab, read_name, read_type);
        destroy_gpc_type(read_type);
        free(read_name);
    }

    char *readln_name = strdup("readln");
    if (readln_name != NULL) {
        GpcType *readln_type = create_procedure_type(NULL, NULL);
        assert(readln_type != NULL && "Failed to create readln procedure type");
        AddBuiltinProc_Typed(symtab, readln_name, readln_type);
        destroy_gpc_type(readln_type);
        free(readln_name);
    }

    char *val_name = strdup("Val");
    if (val_name != NULL) {
        GpcType *val_type = create_procedure_type(NULL, NULL);
        assert(val_type != NULL && "Failed to create Val procedure type");
        AddBuiltinProc_Typed(symtab, val_name, val_type);
        destroy_gpc_type(val_type);
        free(val_name);
    }
    char *str_name = strdup("Str");
    if (str_name != NULL) {
        GpcType *str_type = create_procedure_type(NULL, NULL);
        assert(str_type != NULL && "Failed to create Str procedure type");
        AddBuiltinProc_Typed(symtab, str_name, str_type);
        destroy_gpc_type(str_type);
        free(str_name);
    }
    char *insert_name = strdup("Insert");
    if (insert_name != NULL) {
        GpcType *insert_type = create_procedure_type(NULL, NULL);
        assert(insert_type != NULL && "Failed to create Insert procedure type");
        AddBuiltinProc_Typed(symtab, insert_name, insert_type);
        destroy_gpc_type(insert_type);
        free(insert_name);
    }
    char *delete_name = strdup("Delete");
    if (delete_name != NULL) {
        GpcType *delete_type = create_procedure_type(NULL, NULL);
        assert(delete_type != NULL && "Failed to create Delete procedure type");
        AddBuiltinProc_Typed(symtab, delete_name, delete_type);
        destroy_gpc_type(delete_type);
        free(delete_name);
    }

    char *sincos_name = strdup("SinCos");
    if (sincos_name != NULL) {
        GpcType *sincos_type = create_procedure_type(NULL, NULL);
        assert(sincos_type != NULL && "Failed to create SinCos procedure type");
        AddBuiltinProc_Typed(symtab, sincos_name, sincos_type);
        destroy_gpc_type(sincos_type);
        free(sincos_name);
    }

    char *inc_name = strdup("Inc");
    if (inc_name != NULL) {
        GpcType *inc_type = create_procedure_type(NULL, NULL);
        assert(inc_type != NULL && "Failed to create Inc procedure type");
        AddBuiltinProc_Typed(symtab, inc_name, inc_type);
        destroy_gpc_type(inc_type);
        free(inc_name);
    }

    char *dec_name = strdup("Dec");
    if (dec_name != NULL) {
        GpcType *dec_type = create_procedure_type(NULL, NULL);
        assert(dec_type != NULL && "Failed to create Dec procedure type");
        AddBuiltinProc_Typed(symtab, dec_name, dec_type);
        destroy_gpc_type(dec_type);
        free(dec_name);
    }

    char *include_name = strdup("Include");
    if (include_name != NULL) {
        GpcType *include_type = create_procedure_type(NULL, NULL);
        assert(include_type != NULL && "Failed to create Include procedure type");
        AddBuiltinProc_Typed(symtab, include_name, include_type);
        destroy_gpc_type(include_type);
        free(include_name);
    }

    char *exclude_name = strdup("Exclude");
    if (exclude_name != NULL) {
        GpcType *exclude_type = create_procedure_type(NULL, NULL);
        assert(exclude_type != NULL && "Failed to create Exclude procedure type");
        AddBuiltinProc_Typed(symtab, exclude_name, exclude_type);
        destroy_gpc_type(exclude_type);
        free(exclude_name);
    }

    char *randomize_name = strdup("Randomize");
    if (randomize_name != NULL) {
        GpcType *randomize_type = create_procedure_type(NULL, NULL);
        assert(randomize_type != NULL && "Failed to create Randomize procedure type");
        AddBuiltinProc_Typed(symtab, randomize_name, randomize_type);
        destroy_gpc_type(randomize_type);
        free(randomize_name);
    }

    char *setrandseed_name = strdup("SetRandSeed");
    if (setrandseed_name != NULL) {
        GpcType *setrandseed_type = create_procedure_type(NULL, NULL);
        assert(setrandseed_type != NULL && "Failed to create SetRandSeed procedure type");
        AddBuiltinProc_Typed(symtab, setrandseed_name, setrandseed_type);
        destroy_gpc_type(setrandseed_type);
        free(setrandseed_name);
    }

    char *new_name = strdup("New");
    if (new_name != NULL) {
        GpcType *new_type = create_procedure_type(NULL, NULL);
        assert(new_type != NULL && "Failed to create New procedure type");
        AddBuiltinProc_Typed(symtab, new_name, new_type);
        destroy_gpc_type(new_type);
        free(new_name);
    }

    char *dispose_name = strdup("Dispose");
    if (dispose_name != NULL) {
        GpcType *dispose_type = create_procedure_type(NULL, NULL);
        assert(dispose_type != NULL && "Failed to create Dispose procedure type");
        AddBuiltinProc_Typed(symtab, dispose_name, dispose_type);
        destroy_gpc_type(dispose_type);
        free(dispose_name);
    }

    /* Builtin functions - functions have return types */
    char *length_name = strdup("Length");
    if (length_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Length");
        GpcType *length_type = create_procedure_type(NULL, return_type);
        assert(length_type != NULL && "Failed to create Length function type");
        AddBuiltinFunction_Typed(symtab, length_name, length_type);
        destroy_gpc_type(length_type);
        free(length_name);
    }

    char *copy_name = strdup("Copy");
    if (copy_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(return_type != NULL && "Failed to create return type for Copy");
        GpcType *copy_type = create_procedure_type(NULL, return_type);
        assert(copy_type != NULL && "Failed to create Copy function type");
        AddBuiltinFunction_Typed(symtab, copy_name, copy_type);
        destroy_gpc_type(copy_type);
        free(copy_name);
    }
    char *eof_name = strdup("EOF");
    if (eof_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for EOF");
        GpcType *eof_type = create_procedure_type(NULL, return_type);
        assert(eof_type != NULL && "Failed to create EOF function type");
        AddBuiltinFunction_Typed(symtab, eof_name, eof_type);
        destroy_gpc_type(eof_type);
        free(eof_name);
    }

    char *sizeof_name = strdup("SizeOf");
    if (sizeof_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for SizeOf");
        GpcType *sizeof_type = create_procedure_type(NULL, return_type);
        assert(sizeof_type != NULL && "Failed to create SizeOf function type");
        AddBuiltinFunction_Typed(symtab, sizeof_name, sizeof_type);
        destroy_gpc_type(sizeof_type);
        free(sizeof_name);
    }

    char *chr_name = strdup("Chr");
    if (chr_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_CHAR);
        assert(return_type != NULL && "Failed to create return type for Chr");
        GpcType *chr_type = create_procedure_type(NULL, return_type);
        assert(chr_type != NULL && "Failed to create Chr function type");
        AddBuiltinFunction_Typed(symtab, chr_name, chr_type);
        destroy_gpc_type(chr_type);
        free(chr_name);
    }

    char *ord_name = strdup("Ord");
    if (ord_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Ord");
        GpcType *ord_type = create_procedure_type(NULL, return_type);
        assert(ord_type != NULL && "Failed to create Ord function type");
        AddBuiltinFunction_Typed(symtab, ord_name, ord_type);
        destroy_gpc_type(ord_type);
        free(ord_name);
    }

    char *odd_name = strdup("Odd");
    if (odd_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for Odd");
        GpcType *odd_type = create_procedure_type(NULL, return_type);
        assert(odd_type != NULL && "Failed to create Odd function type");
        AddBuiltinFunction_Typed(symtab, odd_name, odd_type);
        destroy_gpc_type(odd_type);
        free(odd_name);
    }
    char *upcase_name = strdup("UpCase");
    if (upcase_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_CHAR);
        assert(return_type != NULL && "Failed to create return type for UpCase");
        GpcType *upcase_type = create_procedure_type(NULL, return_type);
        assert(upcase_type != NULL && "Failed to create UpCase function type");
        AddBuiltinFunction_Typed(symtab, upcase_name, upcase_type);
        destroy_gpc_type(upcase_type);
        free(upcase_name);
    }

    char *randseed_name = strdup("RandSeed");
    if (randseed_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for RandSeed");
        GpcType *randseed_type = create_procedure_type(NULL, return_type);
        assert(randseed_type != NULL && "Failed to create RandSeed function type");
        AddBuiltinFunction_Typed(symtab, randseed_name, randseed_type);
        destroy_gpc_type(randseed_type);
        free(randseed_name);
    }

    char *sqr_name = strdup("Sqr");
    if (sqr_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Sqr");
        GpcType *sqr_type = create_procedure_type(NULL, return_type);
        assert(sqr_type != NULL && "Failed to create Sqr function type");
        AddBuiltinFunction_Typed(symtab, sqr_name, sqr_type);
        destroy_gpc_type(sqr_type);
        free(sqr_name);
    }

    char *ln_name = strdup("Ln");
    if (ln_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Ln");
        GpcType *ln_type = create_procedure_type(NULL, return_type);
        assert(ln_type != NULL && "Failed to create Ln function type");
        AddBuiltinFunction_Typed(symtab, ln_name, ln_type);
        destroy_gpc_type(ln_type);
        free(ln_name);
    }

    char *exp_name = strdup("Exp");
    if (exp_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Exp");
        GpcType *exp_type = create_procedure_type(NULL, return_type);
        assert(exp_type != NULL && "Failed to create Exp function type");
        AddBuiltinFunction_Typed(symtab, exp_name, exp_type);
        destroy_gpc_type(exp_type);
        free(exp_name);
    }

    char *random_name = strdup("Random");
    if (random_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Random");
        GpcType *random_type = create_procedure_type(NULL, return_type);
        assert(random_type != NULL && "Failed to create Random function type");
        AddBuiltinFunction_Typed(symtab, random_name, random_type);
        destroy_gpc_type(random_type);
        free(random_name);
    }
    char *randomrange_name = strdup("RandomRange");
    if (randomrange_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for RandomRange");
        GpcType *randomrange_type = create_procedure_type(NULL, return_type);
        assert(randomrange_type != NULL && "Failed to create RandomRange function type");
        AddBuiltinFunction_Typed(symtab, randomrange_name, randomrange_type);
        destroy_gpc_type(randomrange_type);
        free(randomrange_name);
    }

    char *high_name = strdup("High");
    if (high_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for High");
        GpcType *high_type = create_procedure_type(NULL, return_type);
        assert(high_type != NULL && "Failed to create High function type");
        AddBuiltinFunction_Typed(symtab, high_name, high_type);
        destroy_gpc_type(high_type);
        free(high_name);
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

    return_val += predeclare_enum_literals(symtab, tree->tree_data.program_data.type_declaration);
    return_val += semcheck_const_decls(symtab, tree->tree_data.program_data.const_declaration);
    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
    return_val += semcheck_decls(symtab, tree->tree_data.program_data.var_declaration);

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, 0);

    // Semantic check finalization statements from units
    if (tree->tree_data.program_data.finalization_statements != NULL) {
        ListNode_t *final_node = tree->tree_data.program_data.finalization_statements;
        while (final_node != NULL) {
            if (final_node->type == LIST_STMT && final_node->cur != NULL) {
                struct Statement *final_stmt = (struct Statement *)final_node->cur;
                return_val += semcheck_stmt(symtab, final_stmt, 0);
            }
            final_node = final_node->next;
        }
    }

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

        /* UNTYPED procedure parameters - use NULL GpcType */
        func_return = PushVarOntoScope_Typed(symtab, (char *)cur->cur, NULL);

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

        HashNode_t *resolved_type = NULL;
        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.type_id != NULL)
            FindIdent(&resolved_type, symtab, tree->tree_data.var_decl_data.type_id);

        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    HashNode_t *type_node = resolved_type;
                    if (type_node == NULL)
                    {
                        semantic_error(tree->line_num, 0, "undefined type %s",
                            tree->tree_data.var_decl_data.type_id);
                        return_val++;
                        var_type = HASHVAR_UNTYPED;
                    }
                    else
                    {
                        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                                {
                                    var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                    mark_hashnode_unit_info(var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }
                            goto next_identifier;
                        }

                        int declared_type_tag = tree->tree_data.var_decl_data.type;
                        int needs_inline_pointer = (declared_type_tag == POINTER_TYPE &&
                            (type_node->type == NULL || type_node->type->kind != TYPE_KIND_POINTER));

                        if (needs_inline_pointer)
                        {
                            GpcType *points_to = NULL;
                            if (type_node->type != NULL)
                            {
                                gpc_type_retain(type_node->type);
                                points_to = type_node->type;
                            }
                            else
                            {
                                struct RecordType *target_record = get_record_type_from_node(type_node);
                                if (target_record != NULL)
                                {
                                    points_to = create_record_type(target_record);
                                }
                                else
                                {
                                    enum VarType target_var_type = get_var_type_from_node(type_node);
                                    int primitive_tag = map_var_type_to_type_tag(target_var_type);
                                    if (primitive_tag != UNKNOWN_TYPE)
                                        points_to = create_primitive_type(primitive_tag);
                                }
                            }

                            GpcType *pointer_type = create_pointer_type(points_to);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                                {
                                    var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                    mark_hashnode_unit_info(var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }
                            goto next_identifier;
                        }

                        var_type = get_var_type_from_node(type_node);
                        int resolved_tag = map_var_type_to_type_tag(var_type);
                        if (resolved_tag != UNKNOWN_TYPE)
                            tree->tree_data.var_decl_data.type = resolved_tag;
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL && alias->is_array)
                        {
                            int start = alias->array_start;
                            int end = alias->array_end;
                            if (alias->is_open_array)
                            {
                                start = 0;
                                end = -1;
                            }

                            /* Get element type - it might be a primitive type or a type reference */
                            GpcType *element_type = NULL;
                            int element_type_tag = alias->array_element_type;
                            
                            /* If element type is a type reference, resolve it */
                            if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                            {
                                HashNode_t *element_type_node = NULL;
                                if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                    element_type_node != NULL && element_type_node->type != NULL)
                                {
                                    element_type = element_type_node->type;
                                }
                                else if (element_type_node != NULL)
                                {
                                    /* Get GpcType from element_type_node */
                                    element_type = element_type_node->type;
                                    assert(element_type != NULL && "Element type node must have GpcType");
                                }
                            }
                            else if (element_type_tag != UNKNOWN_TYPE)
                            {
                                /* Direct primitive type tag - use create_primitive_type */
                                element_type = create_primitive_type(element_type_tag);
                            }
                            
                            assert(element_type != NULL && "Array element type must be resolvable");
                            
                            /* Create array GpcType */
                            GpcType *array_type = create_array_type(element_type, start, end);
                            assert(array_type != NULL && "Failed to create array type");
                            
                            /* Set type_alias on GpcType so it's properly propagated */
                            gpc_type_set_type_alias(array_type, alias);
                            
                            func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                                {
                                    mark_hashnode_unit_info(var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }

                            goto next_identifier;
                        }
                        
                        /* For non-array type references (e.g., enum, set, file, record), create GpcType from type_node */
                        GpcType *var_gpc_type = NULL;
                        if (type_node->type != NULL)
                        {
                            /* Type node already has a GpcType - reference it (don't clone) */
                            var_gpc_type = type_node->type;
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_gpc_type);
                        }
                        else
                        {
                            /* Fallback: create GpcType from legacy fields using helpers */
                            struct RecordType *record_type = get_record_type_from_node(type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                var_gpc_type = create_record_type(record_type);
                            }
                            else if (var_type == HASHVAR_POINTER)
                            {
                                /* For pointer types, we need to create a pointer GpcType */
                                /* Get the TypeAlias to find what the pointer points to */
                                struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                                if (type_alias != NULL && type_alias->is_pointer)
                                {
                                    GpcType *points_to = NULL;
                                    
                                    /* Try to resolve the target type */
                                    if (type_alias->pointer_type_id != NULL)
                                    {
                                        HashNode_t *target_node = NULL;
                                        if (FindIdent(&target_node, symtab, type_alias->pointer_type_id) >= 0 &&
                                            target_node != NULL && target_node->type != NULL)
                                        {
                                            points_to = target_node->type;
                                        }
                                    }
                                    
                                    /* If we couldn't resolve it, create a placeholder based on pointer_type */
                                    if (points_to == NULL && type_alias->pointer_type != UNKNOWN_TYPE)
                                    {
                                        points_to = create_primitive_type(type_alias->pointer_type);
                                    }
                                    
                                    if (points_to != NULL)
                                    {
                                        var_gpc_type = create_pointer_type(points_to);
                                        gpc_type_set_type_alias(var_gpc_type, type_alias);
                                    }
                                }
                            }
                            else
                            {
                                var_gpc_type = gpc_type_from_var_type(var_type);
                            }
                            
                            struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                            if (var_gpc_type != NULL && type_alias != NULL && var_type != HASHVAR_POINTER)
                            {
                                gpc_type_set_type_alias(var_gpc_type, type_alias);
                            }
                            
                            /* Always use _Typed variant, even if GpcType is NULL (UNTYPED) */
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_gpc_type);
                        }
                        
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                                mark_hashnode_unit_info(var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }
                }
                else if (tree->tree_data.var_decl_data.inferred_type)
                {
                    /* For type inference, use INTEGER as placeholder - will be replaced later */
                    var_type = HASHVAR_INTEGER;  /* Placeholder */
                }
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else if(tree->tree_data.var_decl_data.type == BOOL)
                    var_type = HASHVAR_BOOLEAN;
                else if(tree->tree_data.var_decl_data.type == SET_TYPE)
                    var_type = HASHVAR_SET;
                else if(tree->tree_data.var_decl_data.type == ENUM_TYPE)
                    var_type = HASHVAR_ENUM;
                else if(tree->tree_data.var_decl_data.type == STRING_TYPE)
                    var_type = HASHVAR_PCHAR;
                else if(tree->tree_data.var_decl_data.type == RECORD_TYPE)
                    var_type = HASHVAR_RECORD;
                else if(tree->tree_data.var_decl_data.type == FILE_TYPE)
                    var_type = HASHVAR_FILE;
                else if(tree->tree_data.var_decl_data.type == TEXT_TYPE)
                    var_type = HASHVAR_TEXT;
                else
                    var_type = HASHVAR_REAL;
                
                /* Create GpcType for typed variables */
                GpcType *var_gpc_type = NULL;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use GpcType from resolved type if available */
                    var_gpc_type = resolved_type->type;
                }
                else if (tree->tree_data.var_decl_data.inline_record_type != NULL)
                {
                    /* Handle inline record type declarations */
                    var_gpc_type = create_record_type(clone_record_type(tree->tree_data.var_decl_data.inline_record_type));
                }
                else
                {
                    /* Create GpcType from var_type */
                    var_gpc_type = gpc_type_from_var_type(var_type);
                    
                    /* Add metadata from resolved_type if present */
                    if (var_gpc_type != NULL && resolved_type != NULL)
                    {
                        struct TypeAlias *type_alias = get_type_alias_from_node(resolved_type);
                        if (type_alias != NULL)
                        {
                            gpc_type_set_type_alias(var_gpc_type, type_alias);
                        }
                        struct RecordType *record_type = get_record_type_from_node(resolved_type);
                        if (record_type != NULL && var_gpc_type->kind == TYPE_KIND_RECORD)
                        {
                            /* Use the canonical RecordType from the symbol table,
                             * not a clone. This ensures type identity checks work correctly. */
                            var_gpc_type->info.record_info = record_type;
                        }
                    }
                }
                
                /* Always use _Typed variant, even if GpcType is NULL (UNTYPED) */
                func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_gpc_type);
                
                if (func_return == 0)
                {
                    HashNode_t *var_node = NULL;
                    if (FindIdent(&var_node, symtab, (char *)ids->cur) != -1 && var_node != NULL)
                    {
                        var_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                        mark_hashnode_unit_info(var_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                }
            }
            /* Array declarations */
            else
            {
                assert(tree->type == TREE_ARR_DECL);
                
                GpcType *element_type = NULL;
                
                /* If type_id is specified, resolve it to get the element type */
                if (tree->tree_data.arr_decl_data.type_id != NULL)
                {
                    HashNode_t *element_type_node = NULL;
                    if (FindIdent(&element_type_node, symtab, tree->tree_data.arr_decl_data.type_id) >= 0)
                    {
                        /* Use the GpcType from the resolved type node */
                        element_type = element_type_node->type;
                        if (element_type == NULL)
                        {
                            /* Fallback for migration: some nodes may not have GpcType populated yet.
                             * Try to construct GpcType from legacy record type information. */
                            struct RecordType *record_type = get_record_type_from_node(element_type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                element_type = create_record_type(record_type);
                            }
                        }
                    }
                    else
                    {
                        fprintf(stderr, "Error on line %d, undefined type %s!\n",
                            tree->line_num, tree->tree_data.arr_decl_data.type_id);
                        return_val++;
                    }
                }
                
                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL)
                {
                    if(tree->tree_data.arr_decl_data.type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if(tree->tree_data.arr_decl_data.type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if(tree->tree_data.arr_decl_data.type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if(tree->tree_data.arr_decl_data.type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else
                        var_type = HASHVAR_REAL;
                    
                    element_type = gpc_type_from_var_type(var_type);
                    assert(element_type != NULL && "Array element type must be createable from VarType");
                }
                
                /* Resolve array bounds from constant identifiers if necessary.
                 * This handles inline array declarations like: var arr: array[1..N] of integer
                 * where N is a const. The parser stores the original range string (e.g., "1..N")
                 * in range_str, which we parse and resolve here. */
                int start_bound = tree->tree_data.arr_decl_data.s_range;
                int end_bound = tree->tree_data.arr_decl_data.e_range;
                
                if (tree->tree_data.arr_decl_data.range_str != NULL)
                {
                    char *range_str = tree->tree_data.arr_decl_data.range_str;
                    char *sep = strstr(range_str, "..");
                    
                    if (sep != NULL)
                    {
                        /* Parse "start..end" format */
                        size_t start_len = sep - range_str;
                        char *start_str = (char *)malloc(start_len + 1);
                        char *end_str = strdup(sep + 2);
                        
                        if (start_str != NULL && end_str != NULL)
                        {
                            strncpy(start_str, range_str, start_len);
                            start_str[start_len] = '\0';
                            
                            /* Trim whitespace */
                            char *s = start_str;
                            while (*s == ' ' || *s == '\t') s++;
                            char *e = end_str;
                            while (*e == ' ' || *e == '\t') e++;
                            char *p = s + strlen(s) - 1;
                            while (p > s && (*p == ' ' || *p == '\t')) *p-- = '\0';
                            p = e + strlen(e) - 1;
                            while (p > e && (*p == ' ' || *p == '\t')) *p-- = '\0';
                            
                            /* Try to resolve start bound as constant */
                            long long start_val = 0;
                            if (resolve_const_identifier(symtab, s, &start_val) == 0)
                            {
                                start_bound = (int)start_val;
                            }
                            else
                            {
                                /* Try parsing as integer literal */
                                char *endptr;
                                long num = strtol(s, &endptr, 10);
                                if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                    start_bound = (int)num;
                            }
                            
                            /* Try to resolve end bound as constant */
                            long long end_val = 0;
                            if (resolve_const_identifier(symtab, e, &end_val) == 0)
                            {
                                end_bound = (int)end_val;
                            }
                            else
                            {
                                /* Try parsing as integer literal */
                                char *endptr;
                                long num = strtol(e, &endptr, 10);
                                if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                    end_bound = (int)num;
                            }
                            
                            free(start_str);
                            free(end_str);
                        }
                    }
                }
                
                /* CRITICAL FIX: Update the tree's s_range and e_range fields with the resolved bounds.
                 * Downstream code (especially the x86-64 code generator) checks arr->e_range < arr->s_range
                 * to determine if an array is dynamic. Without updating these fields, arrays declared with
                 * constant expressions (e.g., array[1..N] where N is a const) would be incorrectly treated
                 * as dynamic arrays, leading to segmentation faults. */
                tree->tree_data.arr_decl_data.s_range = start_bound;
                tree->tree_data.arr_decl_data.e_range = end_bound;
                
                GpcType *array_type = create_array_type(
                    element_type,
                    start_bound,
                    end_bound
                );
                assert(array_type != NULL && "Failed to create array type");
                
                /* If the element type was specified by a type_id (like TAlfa), preserve that information
                 * by creating a minimal TypeAlias and attaching it to the array_type. This allows
                 * nested array indexing to work correctly (e.g., Keywords[1][1] where Keywords is
                 * array[1..5] of TAlfa and TAlfa is array[1..10] of char). */
                if (tree->tree_data.arr_decl_data.type_id != NULL)
                {
                    /* Create a minimal TypeAlias just to store the element type ID */
                    struct TypeAlias *temp_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
                    if (temp_alias != NULL)
                    {
                        temp_alias->is_array = 1;
                        temp_alias->array_start = start_bound;
                        temp_alias->array_end = end_bound;
                        temp_alias->array_element_type_id = strdup(tree->tree_data.arr_decl_data.type_id);
                        temp_alias->array_element_type = tree->tree_data.arr_decl_data.type;
                        
                        gpc_type_set_type_alias(array_type, temp_alias);
                    }
                }
                
                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                semantic_error(tree->line_num, 0, "redeclaration of name %s",
                    (char *)ids->cur);
                return_val += func_return;
            }
            else
            {
                HashNode_t *decl_node = NULL;
                if (FindIdent(&decl_node, symtab, (char *)ids->cur) != -1 && decl_node != NULL)
                {
                    if (tree->type == TREE_VAR_DECL)
                    {
                        decl_node->is_var_parameter = tree->tree_data.var_decl_data.is_var_param ? 1 : 0;
                        mark_hashnode_unit_info(decl_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                    else
                    {
                        mark_hashnode_unit_info(decl_node,
                            tree->tree_data.arr_decl_data.defined_in_unit,
                            tree->tree_data.arr_decl_data.unit_is_public);
                    }
                }
            }

next_identifier:
            ids = ids->next;
        }

        cur = cur->next;

        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.initializer != NULL)
        {
            struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
            
            /* Handle COMPOUND_STATEMENT initializers (from record const lowering) separately */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT)
            {
                /* This is a lowered record const - just semantic check the compound statement */
                return_val += semcheck_stmt(symtab, init_stmt, INT_MAX);
            }
            else if (ids_head == NULL || ids_head->next != NULL)
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
                    if (init_expr == NULL)
                    {
                        fprintf(stderr, "Error on line %d, initializer expression is NULL for %s.\n",
                            tree->line_num, var_name);
                        ++return_val;
                        /* Skip remaining processing for this variable but continue with the loop */
                    }
                    else
                    {
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
                            case LONGINT_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = (expr_type == LONGINT_TYPE) ? LONGINT_TYPE : INT_TYPE;
                                break;
                            case BOOL:
                                inferred_var_type = HASHVAR_BOOLEAN;
                                normalized_type = BOOL;
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
                            case SET_TYPE:
                                inferred_var_type = HASHVAR_SET;
                                normalized_type = SET_TYPE;
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
                                /* Replace or create GpcType with inferred type */
                                GpcType *inferred_gpc_type = create_primitive_type(normalized_type);
                                if (inferred_gpc_type != NULL)
                                {
                                    if (var_node->type != NULL)
                                    {
                                        /* Free old type and replace */
                                        destroy_gpc_type(var_node->type);
                                    }
                                    var_node->type = inferred_gpc_type;
                                    /* Legacy field will be populated by helper if needed */
                                }
                            }
                        }
                        else
                        {
                            enum VarType current_var_type = get_var_type_from_node(var_node);
                            if (inferred_var_type != current_var_type)
                            {
                                fprintf(stderr, "Error on line %d, initializer type mismatch for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                            }
                        }
                    }
                    }  /* Close else at line 2054 */
                }  /* Close else at line 2043 */
            }  /* Close else at line 2033 */
        }  /* Close if at line 2017 */
        else if (tree->type == TREE_ARR_DECL && tree->tree_data.arr_decl_data.initializer != NULL)
        {
            return_val += semcheck_stmt(symtab, tree->tree_data.arr_decl_data.initializer, INT_MAX);
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
    enum TreeType sub_type;
    struct Statement *body;
    HashNode_t *hash_return;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    /* Record lexical nesting depth so codegen can reason about static links accurately.
     * Store depth as parent depth + 1 so the top-level program has depth 1 and
     * nested subprograms continue to increase. */
    subprogram->tree_data.subprogram_data.nesting_level = max_scope_lev + 1;
    int default_requires = (subprogram->tree_data.subprogram_data.nesting_level > 1 &&
        !subprogram->tree_data.subprogram_data.defined_in_unit);
    subprogram->tree_data.subprogram_data.requires_static_link = default_requires ? 1 : 0;

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    // --- Name Mangling Logic ---
    static int debug_external = -1;
    if (debug_external == -1)
        debug_external = (getenv("GPC_DEBUG_EXTERNAL") != NULL);
    const char *explicit_name = subprogram->tree_data.subprogram_data.cname_override;
    if (explicit_name != NULL) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
    } else if (subprogram->tree_data.subprogram_data.cname_flag) {
        const char *export_name = subprogram->tree_data.subprogram_data.id;
        if (debug_external) {
            fprintf(stderr, "[SemCheck] cname_flag id=%s alias=%s\n",
                subprogram->tree_data.subprogram_data.id,
                export_name != NULL ? export_name : "(null)");
        }
        if (export_name != NULL)
            subprogram->tree_data.subprogram_data.mangled_id = strdup(export_name);
        else
            subprogram->tree_data.subprogram_data.mangled_id = NULL;
    } else {
        // Pass the symbol table to the mangler
        subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab); // <-- PASS symtab HERE
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if already declared (e.g., by predeclare_subprogram in two-pass approach) */
    HashNode_t *existing_decl = NULL;
    int already_declared = (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0);

    if (already_declared && existing_decl != NULL &&
        subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (existing_decl->mangled_id != NULL)
            free(existing_decl->mangled_id);
        existing_decl->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create GpcType for the procedure */
        GpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        if (proc_type != NULL)
        {
            proc_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        
        /* ARCHITECTURAL FIX: Resolve array bounds in parameter types now that constants are in scope */
        if (proc_type != NULL && proc_type->info.proc_info.params != NULL)
        {
            ListNode_t *param = proc_type->info.proc_info.params;
            while (param != NULL)
            {
                if (param->type == LIST_TREE && param->cur != NULL)
                {
                    Tree_t *param_tree = (Tree_t *)param->cur;
                    /* For array parameters, resolve the bounds */
                    if (param_tree->type == TREE_ARR_DECL)
                    {
                        /* If element type is a named type, look it up to get proper GpcType */
                        if (param_tree->tree_data.arr_decl_data.type_id != NULL)
                        {
                            HashNode_t *elem_type_node = NULL;
                            if (FindIdent(&elem_type_node, symtab, param_tree->tree_data.arr_decl_data.type_id) != -1 &&
                                elem_type_node != NULL && elem_type_node->type != NULL)
                            {
                                /* Element type resolved - bounds should be in the tree node */
                                /* Nothing to do here */
                            }
                        }
                    }
                }
                param = param->next;
            }
        }
        
        // Use the typed version to properly set the GpcType
        // Skip if already declared
        if (!already_declared)
        {
            func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            proc_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
        }
        else
        {
            func_return = 0;  /* No error since it's expected to be already declared */
        }

        PushScope(symtab);
        
        /* Create another GpcType for the recursive scope (they're separate) */
        GpcType *proc_type_recursive = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL
        );
        if (proc_type_recursive != NULL)
        {
            proc_type_recursive->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type_recursive->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        if (proc_type_recursive != NULL)
            proc_type_recursive->info.proc_info.definition = subprogram;
        
        // Push it again in the new scope to allow recursion
        PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
            subprogram->tree_data.subprogram_data.mangled_id,
            proc_type_recursive);
        semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
            subprogram->tree_data.subprogram_data.mangled_id);

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        GpcType *return_gpc_type = NULL;

        /* Reuse the type created during predeclaration when possible. */
        if (already_declared && existing_decl != NULL &&
            existing_decl->type != NULL &&
            existing_decl->type->kind == TYPE_KIND_PROCEDURE)
        {
            return_gpc_type = gpc_type_get_return_type(existing_decl->type);
        }

        /* If the predeclare step could not resolve the type (e.g., inline array),
         * build it now and update the existing declaration. */
        if (return_gpc_type == NULL)
            return_gpc_type = build_function_return_type(subprogram, symtab, &return_val);

        GpcType *func_type = NULL;
        if (!already_declared)
        {
            func_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                return_gpc_type
            );
            if (func_type != NULL)
            {
                func_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    func_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            func_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
        }
        else
        {
            func_return = 0;
            if (existing_decl != NULL)
            {
                if (existing_decl->type == NULL)
                {
                    func_type = create_procedure_type(
                        subprogram->tree_data.subprogram_data.args_var,
                        return_gpc_type
                    );
                    if (func_type != NULL)
                    {
                        func_type->info.proc_info.definition = subprogram;
                        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                            func_type->info.proc_info.return_type_id =
                                strdup(subprogram->tree_data.subprogram_data.return_type_id);
                    }
                    existing_decl->type = func_type;
                }
                else if (return_gpc_type != NULL)
                {
                    existing_decl->type->info.proc_info.return_type = return_gpc_type;
                }
            }
        }

        PushScope(symtab);
        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with GpcType
        // Always use _Typed variant, even if GpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, subprogram->tree_data.subprogram_data.id, return_gpc_type);
        
/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" (or "result") is already used as a parameter or local variable */
        HashNode_t *result_check = NULL;
        if (FindIdent(&result_check, symtab, "Result") == -1)
        {
            /* "Result" is not already declared, so we can add it as an alias */
            PushFuncRetOntoScope_Typed(symtab, "Result", return_gpc_type);
        }
        /* Note: We don't check for "result" anymore since it conflicts with built-in Result alias */

        /* Note: Type metadata now in GpcType, no post-creation writes needed */

        new_max_scope = max_scope_lev+1;
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

    return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.declarations);
    return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);

    return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                    new_max_scope, subprogram);

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        PopScope(symtab);
        return return_val;
    }

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        return_val += semcheck_stmt(symtab,
                body,
                new_max_scope);
    }
    else
    {
        assert(FindIdent(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    == 0);

        ResetHashNodeStatus(hash_return);
        return_val += semcheck_func_stmt(symtab,
                body, new_max_scope);
        /* Allow functions with asm blocks to skip explicit return assignment */
        int has_asm = statement_contains_asm_block(body);
        
        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }
        
        if(!return_was_assigned && !has_asm)
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

    if (subprogram->tree_data.subprogram_data.id != NULL)
    {
        ListNode_t *defs = FindAllIdents(symtab, subprogram->tree_data.subprogram_data.id);
        ListNode_t *iter = defs;
        while (iter != NULL)
        {
            if (iter->cur != NULL)
            {
                HashNode_t *node = (HashNode_t *)iter->cur;
                if (node != NULL &&
                    node->mangled_id != NULL &&
                    subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                    strcmp(node->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    node->requires_static_link =
                        subprogram->tree_data.subprogram_data.requires_static_link ? 1 : 0;
                }
            }
            iter = iter->next;
        }
        DestroyList(defs);
    }

    g_semcheck_current_subprogram = prev_current_subprogram;
    PopScope(symtab);
    return return_val;
}


/* Pre-declare a subprogram (add to symbol table without processing body)
 * This is used for forward declarations so all procedures are visible
 * before any bodies are processed.
 */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev)
{
    int return_val = 0;
    int func_return;
    enum TreeType sub_type;
    
    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);
    
    char *id_to_use_for_lookup;
    
    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);
    
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);
    
    // --- Name Mangling Logic ---
    const char *predeclare_name = subprogram->tree_data.subprogram_data.cname_override;
    if (predeclare_name != NULL) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(predeclare_name);
    } else if (subprogram->tree_data.subprogram_data.cname_flag) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(subprogram->tree_data.subprogram_data.id);
    } else {
        // Pass the symbol table to the mangler
        subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab);
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;
    
    /**** PLACE SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create GpcType for the procedure */
        GpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        if (proc_type != NULL) {
            proc_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        
        // Add to current scope
        func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        proc_type);
        
        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }
    else // Function
    {
        GpcType *return_gpc_type = build_function_return_type(subprogram, symtab, &return_val);
        
        /* Create function GpcType */
        GpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_gpc_type  /* functions have a return type */
        );
        if (func_type != NULL) {
            func_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                func_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        
        // Add to current scope
        func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        func_type);
        
        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }
    
    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
/* Forward declaration - we'll define this after semcheck_subprogram */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

    return_val = 0;
    
    /* ARCHITECTURAL FIX: Two-pass approach to ensure all procedure declarations
     * are visible before processing any bodies. This fixes the issue where unit
     * procedures were not visible in nested user procedures because they came
     * later in the subprograms list.
     * 
     * Pass 1: Declare all procedures (add to symbol table)
     * Pass 2: Process bodies (which may reference procedures declared in pass 1)
     */
    
    /* Pass 1: Pre-declare all procedures at this level */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        return_val += predeclare_subprogram(symtab, (Tree_t *)cur->cur, max_scope_lev);
        cur = cur->next;
    }
    
    /* Pass 2: Process full semantic checking including bodies */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += semcheck_subprogram(symtab, child, max_scope_lev);
        if (parent_subprogram != NULL &&
            child != NULL &&
            child->tree_data.subprogram_data.requires_static_link)
        {
            parent_subprogram->tree_data.subprogram_data.requires_static_link = 1;
        }
        cur = cur->next;
    }

    return return_val;
}
