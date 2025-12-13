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
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/from_cparser.h"
#include "../ParseTree/operator_registry.h"
#include "../ParseTree/generic_types.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"
#include <stdarg.h>

/* Helper declared in SemCheck_expr.c */
static int semcheck_map_builtin_type_name_local(const char *id)
{
    if (id == NULL)
        return UNKNOWN_TYPE;

    if (pascal_identifier_equals(id, "Integer"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "LongInt"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(id, "Real") || pascal_identifier_equals(id, "Double"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "String") || pascal_identifier_equals(id, "AnsiString"))
        return STRING_TYPE;
    if (pascal_identifier_equals(id, "ShortString"))
        return SHORTSTRING_TYPE;
    if (pascal_identifier_equals(id, "Char"))
        return CHAR_TYPE;
    if (pascal_identifier_equals(id, "Boolean"))
        return BOOL;
    if (pascal_identifier_equals(id, "Pointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "Byte"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "Word"))
        return INT_TYPE;
    return UNKNOWN_TYPE;
}

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

/* Helper function to get TypeAlias from HashNode, preferring KgpcType when available */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    if (node == NULL)
        return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    return hashnode_get_type_alias(node);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    if (node == NULL) return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    struct RecordType *record = hashnode_get_record_type(node);
    if (record != NULL)
        return record;
        
    /* If not a direct record, check if it's a pointer to a record (Class types are pointers) */
    if (node->type != NULL && kgpc_type_is_pointer(node->type))
    {
        KgpcType *pointed_to = node->type->info.points_to;
        if (pointed_to != NULL && kgpc_type_is_record(pointed_to))
        {
            return kgpc_type_get_record(pointed_to);
        }
    }
    
    return NULL;
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
            int tag = kgpc_type_get_primitive_tag(node->type);
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
int semcheck_unit(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);

/* Resolve the return type for a function declaration once so callers share the same KgpcType. */
HashNode_t *semcheck_find_type_node_with_kgpc_type(SymTab_t *symtab, const char *type_id)
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

static KgpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab,
    int *error_count)
{
    const char *debug_env = getenv("KGPC_DEBUG_RETURN_TYPE");
    if (subprogram == NULL || symtab == NULL)
        return NULL;

    /* TODO: Once the symbol table tracks placeholder types, this helper should
     * validate that any returned KgpcType has been fully resolved. */
    HashNode_t *type_node = NULL;
    if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
    {
        type_node = semcheck_find_type_node_with_kgpc_type(symtab, subprogram->tree_data.subprogram_data.return_type_id);
        if (type_node == NULL)
        {
            semantic_error(subprogram->line_num, 0, "undefined type %s",
                subprogram->tree_data.subprogram_data.return_type_id);
            if (error_count != NULL)
                ++(*error_count);
        }
    }

    if (debug_env != NULL)
    {
        const char *rt_id = subprogram->tree_data.subprogram_data.return_type_id;
        int primitive_tag = subprogram->tree_data.subprogram_data.return_type;
        fprintf(stderr,
            "[KGPC] build_function_return_type: subprogram=%s return_type_id=%s primitive=%d type_node=%p kind=%d\n",
            subprogram->tree_data.subprogram_data.id ? subprogram->tree_data.subprogram_data.id : "<anon>",
            rt_id ? rt_id : "<null>",
            primitive_tag,
            (void *)type_node,
            (type_node != NULL && type_node->type != NULL) ? type_node->type->kind : -1);
    }

    return kgpc_type_build_function_return(
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
            node->type != NULL && kgpc_type_equals_tag(node->type, REAL_TYPE))
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

static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);

/* Evaluate a set literal into a byte array (supports up to 0..255) */
static int evaluate_set_const_bytes(SymTab_t *symtab, struct Expression *expr,
    unsigned char *out_bytes, size_t out_bytes_size, size_t *out_size,
    long long *out_mask, int *is_char_set)
{
    if (expr == NULL || out_bytes == NULL || out_bytes_size < 32)
        return 1;

    memset(out_bytes, 0, out_bytes_size);
    size_t used = 0;
    long long mask = 0;
    long long max_value = 0;
    int char_set = 0;

    ListNode_t *element = expr->expr_data.set_data.elements;
    while (element != NULL)
    {
        if (element->cur != NULL)
        {
            struct SetElement *set_element = (struct SetElement *)element->cur;
            long long lower = 0;
            long long upper = 0;
            if (set_element->lower == NULL ||
                evaluate_const_expr(symtab, set_element->lower, &lower) != 0)
            {
                fprintf(stderr, "Error: set element is not a constant expression.\n");
                return 1;
            }
            if (set_element->upper != NULL)
            {
                if (evaluate_const_expr(symtab, set_element->upper, &upper) != 0)
                {
                    fprintf(stderr, "Error: set element upper bound is not a constant expression.\n");
                    return 1;
                }
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

            if (upper > max_value)
                max_value = upper;
            if (upper > 31)
                char_set = 1;

            if (upper < 0 || upper > 255 || lower < 0)
            {
                fprintf(stderr, "Error: set literal value %lld out of supported range 0..255.\n", upper);
                return 1;
            }

            for (long long value = lower; value <= upper; ++value)
            {
                size_t byte_index = (size_t)(value / 8);
                unsigned bit_mask = 1u << (value % 8);
                out_bytes[byte_index] |= (unsigned char)bit_mask;
            }
        }
        element = element->next;
    }

    used = (max_value <= 31) ? 4 : 32;
    if (out_size != NULL)
        *out_size = used;
    if (out_mask != NULL && used <= sizeof(long long))
    {
        for (size_t i = 0; i < used; ++i)
            mask |= ((long long)out_bytes[i]) << (i * 8);
        *out_mask = mask;
    }
    if (is_char_set != NULL)
        *is_char_set = (char_set || max_value > 31);
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
                if (node->type != NULL && kgpc_type_equals_tag(node->type, REAL_TYPE))
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

/* Helper function to resolve a type name to its base primitive type name.
 * This follows type aliases until we reach a known primitive type.
 * Returns the resolved type name (caller should not free), or NULL if unknown.
 */
static const char *resolve_type_to_base_name(SymTab_t *symtab, const char *type_name)
{
    if (type_name == NULL) return NULL;
    
    /* First, check if it's already a known primitive type */
    if (pascal_identifier_equals(type_name, "Int64") ||
        pascal_identifier_equals(type_name, "QWord") ||
        pascal_identifier_equals(type_name, "UInt64") ||
        pascal_identifier_equals(type_name, "LongInt") ||
        pascal_identifier_equals(type_name, "Integer") ||
        pascal_identifier_equals(type_name, "Cardinal") ||
        pascal_identifier_equals(type_name, "LongWord") ||
        pascal_identifier_equals(type_name, "DWord") ||
        pascal_identifier_equals(type_name, "SmallInt") ||
        pascal_identifier_equals(type_name, "Word") ||
        pascal_identifier_equals(type_name, "ShortInt") ||
        pascal_identifier_equals(type_name, "Byte") ||
        pascal_identifier_equals(type_name, "Boolean") ||
        pascal_identifier_equals(type_name, "Char") ||
        pascal_identifier_equals(type_name, "WideChar") ||
        pascal_identifier_equals(type_name, "Pointer") ||
        pascal_identifier_equals(type_name, "PChar") ||
        pascal_identifier_equals(type_name, "Double") ||
        pascal_identifier_equals(type_name, "Real") ||
        pascal_identifier_equals(type_name, "Single"))
    {
        return type_name;
    }
    
    /* Try to look up as a type alias in the symbol table */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        /* Cast away const for FindIdent - it doesn't modify the string */
        if (FindIdent(&type_node, symtab, (char *)type_name) >= 0 && type_node != NULL)
        {
            if (type_node->hash_type == HASHTYPE_TYPE && type_node->type != NULL)
            {
                /* Get the underlying type from KgpcType */
                KgpcType *kgpc_type = type_node->type;
                
                /* CRITICAL: Check TypeAlias FIRST for target_type_id before checking primitive tag.
                 * This preserves the original type name for small integer types (Byte, Word, etc.)
                 * which are all mapped to LONGINT_TYPE by the predeclare_types function but need
                 * to retain their original type semantics for SizeOf/High/Low operations. */
                struct TypeAlias *alias = kgpc_type_get_type_alias(kgpc_type);
                if (alias != NULL)
                {
                    /* Recursively resolve via target_type_id if available.
                     * This will eventually reach a builtin type like "Word", "Byte", etc.
                     * which are handled by the builtin check at the top of this function. */
                    if (alias->target_type_id != NULL)
                    {
                        return resolve_type_to_base_name(symtab, alias->target_type_id);
                    }
                    /* Check for base_type tag if no target_type_id */
                    if (alias->base_type != UNKNOWN_TYPE && alias->base_type != 0)
                    {
                        switch (alias->base_type)
                        {
                            case INT_TYPE: return "Integer";
                            case LONGINT_TYPE: return "Int64";
                            case BOOL: return "Boolean";
                            case CHAR_TYPE: return "Char";
                            case REAL_TYPE: return "Real";
                            case POINTER_TYPE: return "Pointer";
                            default: break;
                        }
                    }
                }
                
                /* Fallback: Check primitive type tag if no alias info */
                if (kgpc_type->kind == TYPE_KIND_PRIMITIVE)
                {
                    switch (kgpc_type->info.primitive_type_tag)
                    {
                        case INT_TYPE: return "Integer";
                        case LONGINT_TYPE: return "Int64";
                        case BOOL: return "Boolean";
                        case CHAR_TYPE: return "Char";
                        case REAL_TYPE: return "Real";
                        case POINTER_TYPE: return "Pointer";
                        default: break;
                    }
                }
            }
        }
    }
    
    return NULL; /* Unknown type */
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
        case EXPR_NIL:
            /* nil is represented as 0 in pointer context */
            *out_value = 0;
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
        case EXPR_CHAR_CODE:
            *out_value = (unsigned char)(expr->expr_data.char_code & 0xFF);
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
        case EXPR_TYPECAST:
        {
            long long inner_value = 0;
            if (evaluate_const_expr(symtab, expr->expr_data.typecast_data.expr, &inner_value) != 0)
                return 1;

            int target_type = expr->expr_data.typecast_data.target_type;
            if (target_type == UNKNOWN_TYPE &&
                expr->expr_data.typecast_data.target_type_id != NULL)
            {
                const char *id = expr->expr_data.typecast_data.target_type_id;
                if (strcasecmp(id, "Byte") == 0 || strcasecmp(id, "Word") == 0 ||
                    strcasecmp(id, "Integer") == 0)
                    target_type = INT_TYPE;
                else if (strcasecmp(id, "LongInt") == 0)
                    target_type = LONGINT_TYPE;
                else if (strcasecmp(id, "Pointer") == 0)
                    target_type = POINTER_TYPE;
                else if (strcasecmp(id, "Char") == 0)
                    target_type = CHAR_TYPE;
                else if (strcasecmp(id, "Boolean") == 0)
                    target_type = BOOL;
            }

            switch (target_type)
            {
                case CHAR_TYPE:
                    if (inner_value < 0 || inner_value > 255)
                    {
                        fprintf(stderr, "Error: typecast value %lld out of range for Char.\n",
                            inner_value);
                        return 1;
                    }
                    *out_value = (unsigned char)inner_value;
                    return 0;
                case BOOL:
                    *out_value = (inner_value != 0);
                    return 0;
                case INT_TYPE:
                case LONGINT_TYPE:
                case POINTER_TYPE:
                case UNKNOWN_TYPE:
                    /* Treat other (or unresolved) integer-like casts as passthrough */
                    *out_value = inner_value;
                    return 0;
                default:
                    fprintf(stderr, "Error: unsupported const typecast target.\n");
                    return 1;
            }
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
            unsigned char bytes[32];
            size_t used = 0;
            long long mask = 0;
            if (evaluate_set_const_bytes(symtab, expr, bytes, sizeof(bytes), &used, &mask, NULL) != 0)
                return 1;
            if (used > 8)
            {
                fprintf(stderr, "Error: set literal requires 32-byte storage and cannot be reduced to integer const.\n");
                return 1;
            }
            *out_value = mask;
            return 0;
        }
        case EXPR_RECORD_ACCESS:
        {
            /* Handle qualified constants like UnitName.ConstName */
            /* The field_id is the constant name, look it up directly in the symbol table */
            /* since unit exports are already imported into the current scope */
            char *field_id = expr->expr_data.record_access_data.field_id;
            if (field_id != NULL)
            {
                HashNode_t *node = NULL;
                if (FindIdent(&node, symtab, field_id) >= 0 && node != NULL && node->hash_type == HASHTYPE_CONST)
                {
                    *out_value = node->const_int_value;
                    return 0;
                }
            }
            fprintf(stderr, "Error: qualified constant '%s' is undefined or not a const.\n",
                    field_id ? field_id : "(null)");
            return 1;
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
            
            /* Handle High() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "High"))
            {
                if (args == NULL || args->next != NULL)
                {
                    fprintf(stderr, "Error: High in const expression requires exactly one argument.\n");
                    return 1;
                }
                
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg == NULL)
                {
                    fprintf(stderr, "Error: High argument is NULL.\n");
                    return 1;
                }
                
                /* High expects a type identifier */
                if (arg->type == EXPR_VAR_ID)
                {
                    const char *type_name = arg->expr_data.id;
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_name);
                    if (resolved != NULL)
                        type_name = resolved;
                    
                    /* Map common type names to their High values */
                    if (pascal_identifier_equals(type_name, "Int64")) {
                        *out_value = 9223372036854775807LL; /* INT64_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "QWord") ||
                        pascal_identifier_equals(type_name, "UInt64")) {
                        /* Note: This will be -1 as a signed long long, but the bit pattern
                         * is correct for UINT64_MAX. The consumer must interpret appropriately. */
                        *out_value = (long long)0xFFFFFFFFFFFFFFFFULL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "LongInt") ||
                        pascal_identifier_equals(type_name, "Integer")) {
                        *out_value = 2147483647LL; /* INT32_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Cardinal") ||
                        pascal_identifier_equals(type_name, "LongWord") ||
                        pascal_identifier_equals(type_name, "DWord")) {
                        *out_value = 4294967295LL; /* UINT32_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "SmallInt")) {
                        *out_value = 32767LL; /* INT16_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Word")) {
                        *out_value = 65535LL; /* UINT16_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "ShortInt")) {
                        *out_value = 127LL; /* INT8_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Byte")) {
                        *out_value = 255LL; /* UINT8_MAX */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Boolean")) {
                        *out_value = 1LL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Char")) {
                        *out_value = 255LL;
                        return 0;
                    }
                    fprintf(stderr, "Error: High(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    return 1;
                }
                fprintf(stderr, "Error: High expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle Low() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Low"))
            {
                if (args == NULL || args->next != NULL)
                {
                    fprintf(stderr, "Error: Low in const expression requires exactly one argument.\n");
                    return 1;
                }
                
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg == NULL)
                {
                    fprintf(stderr, "Error: Low argument is NULL.\n");
                    return 1;
                }
                
                /* Low expects a type identifier */
                if (arg->type == EXPR_VAR_ID)
                {
                    const char *type_name = arg->expr_data.id;
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_name);
                    if (resolved != NULL)
                        type_name = resolved;
                        
                    /* Map common type names to their Low values */
                    if (pascal_identifier_equals(type_name, "Int64")) {
                        *out_value = (-9223372036854775807LL - 1); /* INT64_MIN */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "QWord") ||
                        pascal_identifier_equals(type_name, "UInt64")) {
                        *out_value = 0LL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "LongInt") ||
                        pascal_identifier_equals(type_name, "Integer")) {
                        *out_value = -2147483648LL; /* INT32_MIN */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Cardinal") ||
                        pascal_identifier_equals(type_name, "LongWord") ||
                        pascal_identifier_equals(type_name, "DWord")) {
                        *out_value = 0LL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "SmallInt")) {
                        *out_value = -32768LL; /* INT16_MIN */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Word")) {
                        *out_value = 0LL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "ShortInt")) {
                        *out_value = -128LL; /* INT8_MIN */
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Byte")) {
                        *out_value = 0LL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Boolean")) {
                        *out_value = 0LL;
                        return 0;
                    }
                    if (pascal_identifier_equals(type_name, "Char")) {
                        *out_value = 0LL;
                        return 0;
                    }
                    fprintf(stderr, "Error: Low(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    return 1;
                }
                fprintf(stderr, "Error: Low expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle SizeOf() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
            {
                if (args == NULL || args->next != NULL)
                {
                    fprintf(stderr, "Error: SizeOf in const expression requires exactly one argument.\n");
                    return 1;
                }
                
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg == NULL)
                {
                    fprintf(stderr, "Error: SizeOf argument is NULL.\n");
                    return 1;
                }
                
                /* SizeOf expects a type identifier */
                if (arg->type == EXPR_VAR_ID)
                {
                    const char *type_name = arg->expr_data.id;
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_name);
                    if (resolved != NULL)
                        type_name = resolved;
                        
                    /* Map common type names to their sizes (in bytes) */
                    /* 64-bit types */
                    if (pascal_identifier_equals(type_name, "Int64") ||
                        pascal_identifier_equals(type_name, "QWord") ||
                        pascal_identifier_equals(type_name, "UInt64") ||
                        pascal_identifier_equals(type_name, "Pointer") ||
                        pascal_identifier_equals(type_name, "PChar") ||
                        pascal_identifier_equals(type_name, "Double") ||
                        pascal_identifier_equals(type_name, "Real")) {
                        *out_value = 8LL;
                        return 0;
                    }
                    /* 32-bit types */
                    if (pascal_identifier_equals(type_name, "LongInt") ||
                        pascal_identifier_equals(type_name, "LongWord") ||
                        pascal_identifier_equals(type_name, "Cardinal") ||
                        pascal_identifier_equals(type_name, "DWord") ||
                        pascal_identifier_equals(type_name, "Integer") ||
                        pascal_identifier_equals(type_name, "Single")) {
                        *out_value = 4LL;
                        return 0;
                    }
                    /* 16-bit types */
                    if (pascal_identifier_equals(type_name, "SmallInt") ||
                        pascal_identifier_equals(type_name, "Word") ||
                        pascal_identifier_equals(type_name, "WideChar")) {
                        *out_value = 2LL;
                        return 0;
                    }
                    /* 8-bit types */
                    if (pascal_identifier_equals(type_name, "ShortInt") ||
                        pascal_identifier_equals(type_name, "Byte") ||
                        pascal_identifier_equals(type_name, "Char") ||
                        pascal_identifier_equals(type_name, "Boolean")) {
                        *out_value = 1LL;
                        return 0;
                    }
                    fprintf(stderr, "Error: SizeOf(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    return 1;
                }
                fprintf(stderr, "Error: SizeOf expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle Chr() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Chr"))
            {
                if (args == NULL || args->next != NULL)
                {
                    fprintf(stderr, "Error: Chr in const expression requires exactly one argument.\n");
                    return 1;
                }
                
                struct Expression *arg = (struct Expression *)args->cur;
                if (arg == NULL)
                {
                    fprintf(stderr, "Error: Chr argument is NULL.\n");
                    return 1;
                }
                
                /* Evaluate the argument as a const expression */
                long long char_code;
                if (evaluate_const_expr(symtab, arg, &char_code) != 0)
                {
                    fprintf(stderr, "Error: Chr argument must be a const expression.\n");
                    return 1;
                }
                
                /* Validate the character code is in valid range (0..255) */
                if (char_code < 0 || char_code > 255)
                {
                    fprintf(stderr, "Error: Chr argument %lld is out of valid range (0..255).\n", char_code);
                    return 1;
                }
                
                *out_value = char_code;
                return 0;
            }
            
            fprintf(stderr, "Error: only Ord(), High(), Low(), SizeOf(), and Chr() function calls are supported in const expressions.\n");
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

    if (parse_tree->type == TREE_UNIT) {
        return_val = semcheck_unit(symtab, parse_tree);
    } else {
        return_val = semcheck_program(symtab, parse_tree);
    }

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
                    /* Create ONE shared KgpcType for this enum type if not already created */
                    if (alias_info->kgpc_type == NULL)
                    {
                        alias_info->kgpc_type = create_primitive_type(ENUM_TYPE);
                        if (alias_info->kgpc_type == NULL)
                        {
                            fprintf(stderr, "Error: Failed to create enum type for %s\n",
                                    tree->tree_data.type_decl_data.id);
                            ++errors;
                        }
                    }
                    
                    if (alias_info->kgpc_type != NULL)
                    {
                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->enum_literals;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                /* Use typed API with shared enum KgpcType - all literals reference same type */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, alias_info->kgpc_type) > 0)
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
                        /* KgpcType is owned by TypeAlias, will be cleaned up when tree is destroyed */
                    }
                }
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Pre-declare types so they can be used in const expressions like High(MyType).
 * This function creates stub entries in the symbol table for SIMPLE type aliases
 * BEFORE const declarations are processed.
 *
 * This enables patterns like:
 *   type
 *     MyInt = Int64;
 *   const
 *     MaxMyInt = High(MyInt);  // Works because MyInt is pre-declared
 *
 * IMPORTANT: We only pre-declare simple primitive type aliases (e.g., MyInt = Int64).
 * Complex types (pointers, arrays, records, etc.) are NOT pre-declared because
 * they may have forward references that can't be resolved yet.
 */
static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls)
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
            if (tree->type == TREE_TYPE_DECL)
            {
                const char *type_id = tree->tree_data.type_decl_data.id;
                
                /* Skip if no type id */
                if (type_id == NULL)
                {
                    cur = cur->next;
                    continue;
                }
                
                /* Check if already declared (e.g., from a previous pass or builtin) */
                HashNode_t *existing = NULL;
                int scope_level = FindIdent(&existing, symtab, (char *)type_id);
                if (scope_level == 0 && existing != NULL)
                {
                    /* Already declared, skip */
                    cur = cur->next;
                    continue;
                }
                
                /* Predeclare record types so they can be referenced (e.g., as function returns) */
                if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;
                    
                    /* Annotate the record with its canonical name if missing */
                    if (record_info != NULL && record_info->type_id == NULL)
                        record_info->type_id = strdup(type_id);
                    
                    KgpcType *kgpc_type = create_record_type(record_info);
                    if (record_type_is_class(record_info))
                    {
                        /* Classes are reference types - register as pointers to the record */
                        kgpc_type = create_pointer_type(kgpc_type);
                    }
                    if (kgpc_type != NULL)
                    {
                        /* Store in tree for later reuse by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);
                        }
                        
                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (result > 0)
                            errors += result;
                    }
                }
                /* Only handle TYPE_DECL_ALIAS cases we can resolve early */
                else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                {
                    struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                    
                    /* Handle inline record aliases (e.g., generic specializations) */
                    if (alias->inline_record_type != NULL &&
                        alias->inline_record_type->type_id != NULL)
                    {
                        const char *record_name = alias->inline_record_type->type_id;
                        HashNode_t *existing_inline = NULL;
                        int inline_scope = FindIdent(&existing_inline, symtab, (char *)record_name);
                        if (inline_scope != 0 || existing_inline == NULL)
                        {
                            KgpcType *inline_kgpc = create_record_type(alias->inline_record_type);
                            if (record_type_is_class(alias->inline_record_type))
                                inline_kgpc = create_pointer_type(inline_kgpc);

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = inline_kgpc;
                                kgpc_type_retain(inline_kgpc);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)record_name, inline_kgpc);
                            if (result > 0)
                                errors += result;
                        }

                        /* Also register the alias name itself */
                        KgpcType *alias_kgpc = create_record_type(alias->inline_record_type);
                        if (record_type_is_class(alias->inline_record_type))
                            alias_kgpc = create_pointer_type(alias_kgpc);
                        kgpc_type_set_type_alias(alias_kgpc, alias);
                        int alias_result = PushTypeOntoScope_Typed(symtab, (char *)type_id, alias_kgpc);
                        if (alias_result > 0)
                            errors += alias_result;

                        cur = cur->next;
                        continue;
                    }

                    /* Skip other complex types - let semcheck_type_decls handle them */
                    if (alias->is_enum || alias->is_array || alias->is_set || 
                        alias->is_file)
                    {
                        cur = cur->next;
                        continue;
                    }
                    
                    /* Handle pointer aliases to already known element types */
                    if (alias->is_pointer)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(alias, symtab);
                        if (kgpc_type != NULL)
                        {
                            if (getenv("KGPC_DEBUG_PREDECLARE_POINTERS") != NULL)
                            {
                                fprintf(stderr, "[KGPC] predeclare pointer alias %s: ptr_type=%d ptr_id=%s kind=%d\n",
                                    type_id,
                                    alias->pointer_type,
                                    alias->pointer_type_id ? alias->pointer_type_id : "<null>",
                                    kgpc_type->kind);
                            }
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;

                            cur = cur->next;
                            continue;
                        }
                    }

                    /* Only pre-declare simple primitive type aliases */
                    KgpcType *kgpc_type = NULL;
                    
                    /* Case 1: Direct primitive type tag (e.g., MyInt = Integer where base_type is set)
                     * Exclude PROCEDURE - procedure types are NOT primitive and need special handling.
                     * IMPORTANT: If target_type_id is "WideChar", skip to Case 2 to look it up
                     * from the symbol table where it has correct storage_size=2. Without this check,
                     * WideChar aliases would get 4 bytes (INT_TYPE) instead of 2 bytes. */
                    int skip_case1_for_widechar = (alias->target_type_id != NULL &&
                        pascal_identifier_equals(alias->target_type_id, "WideChar"));
                    if (!skip_case1_for_widechar &&
                        alias->base_type != UNKNOWN_TYPE && alias->base_type != 0 &&
                        alias->base_type != PROCEDURE)
                    {
                        kgpc_type = create_primitive_type(alias->base_type);
                    }
                    /* Case 2: Reference to a known primitive type name */
                    else if (alias->target_type_id != NULL)
                    {
                        /* Check if target is a known builtin primitive type */
                        const char *target = alias->target_type_id;
                        if (pascal_identifier_equals(target, "Int64") ||
                            pascal_identifier_equals(target, "Integer") ||
                            pascal_identifier_equals(target, "LongInt") ||
                            pascal_identifier_equals(target, "SmallInt") ||
                            pascal_identifier_equals(target, "ShortInt") ||
                            pascal_identifier_equals(target, "Byte") ||
                            pascal_identifier_equals(target, "Word") ||
                            pascal_identifier_equals(target, "Cardinal") ||
                            pascal_identifier_equals(target, "LongWord") ||
                            pascal_identifier_equals(target, "DWord") ||
                            pascal_identifier_equals(target, "QWord") ||
                            pascal_identifier_equals(target, "UInt64") ||
                            pascal_identifier_equals(target, "SizeUInt") ||
                            pascal_identifier_equals(target, "NativeUInt"))
                        {
                            kgpc_type = create_primitive_type(LONGINT_TYPE);
                        }
                        else if (pascal_identifier_equals(target, "Real") ||
                                 pascal_identifier_equals(target, "Double") ||
                                 pascal_identifier_equals(target, "Single"))
                        {
                            kgpc_type = create_primitive_type(REAL_TYPE);
                        }
                        else if (pascal_identifier_equals(target, "Boolean"))
                        {
                            kgpc_type = create_primitive_type(BOOL);
                        }
                        else if (pascal_identifier_equals(target, "Char"))
                        {
                            kgpc_type = create_primitive_type(CHAR_TYPE);
                        }
                        /* If target is another user-defined type, check if it's already declared */
                        else
                        {
                            HashNode_t *target_node = NULL;
                            if (FindIdent(&target_node, symtab, (char *)target) >= 0 &&
                                target_node != NULL && target_node->hash_type == HASHTYPE_TYPE &&
                                target_node->type != NULL)
                            {
                                /* Target type already exists, retain and use it */
                                kgpc_type = target_node->type;
                                kgpc_type_retain(kgpc_type);
                            }
                            /* Otherwise, skip - can't resolve this type yet */
                        }
                    }
                    
                    if (kgpc_type != NULL)
                    {
                        /* IMPORTANT: Inherit storage_size from the original type's alias.
                         * This is critical for types like WideChar (2 bytes) where we retain
                         * the original type but need to preserve its custom storage_size. */
                        struct TypeAlias *original_alias = kgpc_type_get_type_alias(kgpc_type);
                        if (original_alias != NULL && original_alias->storage_size > 0 &&
                            alias->storage_size <= 0)
                        {
                            alias->storage_size = original_alias->storage_size;
                        }
                        
                        /* Set type_alias on KgpcType */
                        kgpc_type_set_type_alias(kgpc_type, alias);
                        
                        /* Store in tree for later use by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Tree owns a reference */
                        }
                        
                        /* Push onto symbol table */
                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (result > 0)
                        {
                            /* Should not happen since we checked above, but handle gracefully */
                            errors++;
                        }
                    }
                }
                /* Skip TYPE_DECL_RECORD, TYPE_DECL_GENERIC, TYPE_DECL_RANGE - 
                 * these have complex dependencies and are handled by semcheck_type_decls */
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
                    new_method->vmt_index = vmt_size + 1;
                    
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

/* Resolves array bounds specified as constant identifiers in a KgpcType
 * This is needed because parsing happens before constants are declared */
static void resolve_array_bounds_in_kgpctype(SymTab_t *symtab, KgpcType *kgpc_type, struct TypeAlias *alias)
{
    if (kgpc_type == NULL || alias == NULL || symtab == NULL)
        return;
    
    /* Only process array types */
    if (kgpc_type->kind != TYPE_KIND_ARRAY)
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
                    
                    /* Update KgpcType with resolved bounds */
                    if (start_resolved && end_resolved)
                    {
                        kgpc_type->info.array_info.start_index = (int)start_val;
                        kgpc_type->info.array_info.end_index = (int)end_val;
                        
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
        int loop_start_errors = return_val;
        
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_TYPE_DECL);
        
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_type_decls processing type: %s kind=%d\n",
            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
            tree->tree_data.type_decl_data.kind);
#endif

        const char *debug_env_check = getenv("KGPC_DEBUG_TFPG");
        if (debug_env_check != NULL)
        {
            fprintf(stderr, "[KGPC] semcheck_type_decls processing: id=%s kind=%d return_val=%d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, loop_start_errors);
        }

        int before_switch_errors = return_val;

        struct RecordType *record_info = NULL;
        struct TypeAlias *alias_info = NULL;
        
        switch (tree->tree_data.type_decl_data.kind)
        {
            case TYPE_DECL_RECORD:
                var_type = HASHVAR_RECORD;
                record_info = tree->tree_data.type_decl_data.info.record;
                
                /* Set the type_id on the RecordType so operator overloading can find it */
                if (record_info != NULL && record_info->type_id == NULL && tree->tree_data.type_decl_data.id != NULL)
                {
                    record_info->type_id = strdup(tree->tree_data.type_decl_data.id);
                }
                
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

                    /* Skip size computation for generic templates (not yet specialized)
                     * Size can only be computed after type parameters are substituted */
                    int is_unspecialized_generic = (record_info->generic_decl != NULL && 
                                                     record_info->num_generic_args == 0);
                    
                    if (!is_unspecialized_generic)
                    {
                        long long record_size = 0;
                        int size_result = semcheck_compute_record_size(symtab, record_info, &record_size,
                                tree->line_num);
                        if (size_result != 0)
                        {
#ifdef DEBUG
                            fprintf(stderr, "DEBUG: semcheck_compute_record_size FAILED for %s: result=%d\n",
                                tree->tree_data.type_decl_data.id, size_result);
#endif
                            return_val += 1;
                        }
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

                if (alias_info->base_type == RECORD_TYPE &&
                    tree->tree_data.type_decl_data.kgpc_type != NULL &&
                    kgpc_type_is_record(tree->tree_data.type_decl_data.kgpc_type))
                {
                    struct RecordType *alias_record =
                        kgpc_type_get_record(tree->tree_data.type_decl_data.kgpc_type);
                    if (alias_record != NULL)
                    {
                        /* Set the type_id on the RecordType for operator overloading */
                        if (alias_record->type_id == NULL && tree->tree_data.type_decl_data.id != NULL)
                        {
                            alias_record->type_id = strdup(tree->tree_data.type_decl_data.id);
                        }
                        
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (alias_record->generic_decl != NULL && 
                                                         alias_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            const char *debug_env3 = getenv("KGPC_DEBUG_TFPG");
                            if (debug_env3 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Computing size for alias %s, generic_decl=%p num_args=%d\n",
                                    tree->tree_data.type_decl_data.id,
                                    (void*)alias_record->generic_decl,
                                    alias_record->num_generic_args);
                            }
                            
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, alias_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                if (debug_env3 != NULL)
                                {
                                    fprintf(stderr, "[KGPC] Size computation FAILED for %s\n",
                                        tree->tree_data.type_decl_data.id);
                                }
                                return_val += 1;
                            }
                            else if (debug_env3 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Size computation succeeded for %s: %lld bytes\n",
                                    tree->tree_data.type_decl_data.id, record_size);
                            }
                        }
                    }
                }

                const char *debug_alias = getenv("KGPC_DEBUG_GENERIC_CLONES");
                if (debug_alias != NULL && tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] semcheck alias %s: base_type=%d target=%s kgpc_type=%p kind=%d\n",
                        tree->tree_data.type_decl_data.id,
                        alias_info->base_type,
                        alias_info->target_type_id != NULL ? alias_info->target_type_id : "<null>",
                        (void *)tree->tree_data.type_decl_data.kgpc_type,
                        tree->tree_data.type_decl_data.kgpc_type != NULL ?
                            tree->tree_data.type_decl_data.kgpc_type->kind : -1);
                }

                break;
            }
            case TYPE_DECL_GENERIC:
            {
                /* Register generic type declaration in the generic registry */
                struct GenericDecl *generic_info = &tree->tree_data.type_decl_data.info.generic;
                
                const char *debug_env = getenv("KGPC_DEBUG_TFPG");
                if (debug_env != NULL)
                {
                    fprintf(stderr, "[KGPC] semcheck TYPE_DECL_GENERIC: id=%s num_params=%d\n",
                        tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                        generic_info ? generic_info->num_type_params : -1);
                }
                
                if (generic_info != NULL && generic_info->num_type_params > 0 && 
                    generic_info->type_parameters != NULL)
                {
                    /* Register the generic declaration */
                    GenericTypeDecl *generic_decl = generic_registry_add_decl(
                        tree->tree_data.type_decl_data.id,
                        generic_info->type_parameters,
                        generic_info->num_type_params,
                        tree
                    );
                    
                    if (generic_decl != NULL)
                    {
                        /* Store reference to record template if this is a class/record */
                        if (generic_info->record_template != NULL)
                        {
                            generic_decl->record_template = generic_info->record_template;
                        }
                        
                        if (debug_env != NULL)
                        {
                            fprintf(stderr, "[KGPC] Registered generic type %s with %d parameters\n",
                                tree->tree_data.type_decl_data.id, generic_info->num_type_params);
                        }
                    }
                    else if (debug_env != NULL)
                    {
                        fprintf(stderr, "[KGPC] Failed to register generic type %s\n",
                            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>");
                    }
                }
                
                /* Generics don't go in symbol table directly, only specialized instances do */
                var_type = HASHVAR_INTEGER;  /* Placeholder */
                /* Skip the normal PushTypeOntoScope for generics */
                func_return = 0;
                break;
            }
            default:
                var_type = HASHVAR_INTEGER;
                break;
        }

        if (debug_env_check != NULL && return_val > before_switch_errors)
        {
            fprintf(stderr, "[KGPC] Error in switch for type %s (kind=%d), was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, before_switch_errors, return_val);
        }

        const char *debug_env2 = getenv("KGPC_DEBUG_TFPG");
        int before_symtab_errors = return_val;

        KgpcType *kgpc_type = tree->tree_data.type_decl_data.kgpc_type;
        if (record_info != NULL && record_type_is_class(record_info) &&
            kgpc_type != NULL && kgpc_type->kind == TYPE_KIND_RECORD)
        {
            /* Classes are reference types - represent them as pointers to the class record */
            KgpcType *wrapped = create_pointer_type(kgpc_type);
            kgpc_type = wrapped;
            tree->tree_data.type_decl_data.kgpc_type = wrapped;
        }

        /* Check if this type was already pre-declared by predeclare_types().
         * If so, skip the push to avoid "redeclaration" errors.
         * IMPORTANT: Only skip if the type exists in the CURRENT scope (scope level 0),
         * not if it exists as a builtin or in a parent scope. */
        HashNode_t *existing_type = NULL;
        int scope_level = FindIdent(&existing_type, symtab, tree->tree_data.type_decl_data.id);
        int already_predeclared = (scope_level == 0 && existing_type != NULL && 
                                   existing_type->hash_type == HASHTYPE_TYPE);

        /* Skip symbol table registration for generic declarations - they're only in the registry */
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_GENERIC)
        {
            /* Generic declarations are already registered in generic_registry */
            /* Skip the rest of symbol table handling */
            if (func_return == 0)
            {
                /* Continue to next type declaration */
            }
        }
        else if (already_predeclared)
        {
            /* Type was already registered by predeclare_types().
             * We still need to update any additional metadata like array bounds. */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL && 
                existing_type->type != NULL)
            {
                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_kgpctype(symtab, existing_type->type, alias_info);
                }
            }
            func_return = 0;  /* No error */
        }
        else if (kgpc_type != NULL) {
            /* Set type_alias on KgpcType before pushing */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL)
            {
                kgpc_type_set_type_alias(kgpc_type, alias_info);
                
                /* IMPORTANT: Inherit storage_size from target type for type aliases.
                 * This is critical for types like WideChar (2 bytes) that are represented
                 * as INT_TYPE (4 bytes) in the primitive type system but have a custom
                 * storage_size defined. Without this, SizeOf(TMyChar) where TMyChar = WideChar
                 * would return 4 instead of the correct 2 bytes. */
                if (alias_info->target_type_id != NULL && alias_info->storage_size <= 0)
                {
                    HashNode_t *target_node = NULL;
                    int found = FindIdent(&target_node, symtab, alias_info->target_type_id);
                    if (found != -1 && target_node != NULL && target_node->type != NULL)
                    {
                        /* Get the target type's storage_size */
                        struct TypeAlias *target_alias = kgpc_type_get_type_alias(target_node->type);
                        if (target_alias != NULL && target_alias->storage_size > 0)
                        {
                            /* Inherit storage_size from target type */
                            alias_info->storage_size = target_alias->storage_size;
                            /* Also update the KgpcType's type_alias storage_size */
                            struct TypeAlias *kgpc_alias = kgpc_type_get_type_alias(kgpc_type);
                            if (kgpc_alias != NULL)
                                kgpc_alias->storage_size = target_alias->storage_size;
                        }
                    }
                }
                
                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_kgpctype(symtab, kgpc_type, alias_info);
                }
            }
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD && record_info != NULL && kgpc_type->kind == TYPE_KIND_RECORD)
                kgpc_type->info.record_info = record_info;
            
            if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] Pushing type '%s' onto scope, kgpc_type=%p kind=%d\n", 
                    tree->tree_data.type_decl_data.id, (void*)kgpc_type, kgpc_type ? kgpc_type->kind : -1);
            }
            func_return = PushTypeOntoScope_Typed(symtab, tree->tree_data.type_decl_data.id, kgpc_type);
            if (func_return == 0)
            {
                /* KgpcType ownership transferred to symbol table */
                tree->tree_data.type_decl_data.kgpc_type = NULL;
                /* Note: var_type is automatically set from KgpcType in HashTable.c via set_var_type_from_kgpctype() */
                
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

            /* For generic specializations with inline record types, also register the
             * record type under its mangled name so cloned methods can find it */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && 
                alias_info != NULL && 
                alias_info->inline_record_type != NULL &&
                alias_info->inline_record_type->type_id != NULL)
            {
                const char *mangled_name = alias_info->inline_record_type->type_id;
                
                /* Only register if not already registered to avoid redeclaration errors */
                HashNode_t *existing = NULL;
                if (FindIdent(&existing, symtab, (char *)mangled_name) == -1)
                {
                    /* Create a KgpcType for the inline record if not already created */
                    KgpcType *inline_kgpc_type = create_record_type(alias_info->inline_record_type);
                    if (inline_kgpc_type != NULL)
                    {
                        int push_result = PushTypeOntoScope_Typed(symtab, (char *)mangled_name, inline_kgpc_type);
                        if (push_result == 0)
                        {
                            if (debug_env2 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Registered inline record type %s for alias %s\n",
                                        mangled_name, tree->tree_data.type_decl_data.id);
                            }
                            /* Ownership transferred to symbol table */
                        }
                        /* Note: On failure, we leak the KgpcType, but this should not happen
                         * in practice since we checked that the name doesn't exist */
                    }
                }
            }
        }

        if (debug_env2 != NULL && return_val > before_symtab_errors)
        {
            fprintf(stderr, "[KGPC] Error increased in type %s (kind=%d), was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, before_symtab_errors, return_val);
        }
        
        if (return_val > loop_start_errors)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_type_decls ERROR in type %s: was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                loop_start_errors, return_val);
#endif
        }

        cur = cur->next;
    }

    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && return_val > 0)
    {
        fprintf(stderr, "[KGPC] semcheck_type_decls returning error count: %d\n", return_val);
    }
    else if (debug_env != NULL)
    {
        fprintf(stderr, "[KGPC] semcheck_type_decls completed successfully\n");
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
        else if (value_expr != NULL && value_expr->type == EXPR_SET)
        {
            /* Evaluate as set constant (supports char sets up to 0..255) */
            unsigned char set_bytes[32];
            size_t set_size = 0;
            long long mask = 0;
            int is_char_set = 0;
            if (evaluate_set_const_bytes(symtab, value_expr, set_bytes, sizeof(set_bytes),
                    &set_size, &mask, &is_char_set) != 0)
            {
                fprintf(stderr, "Error on line %d, unsupported const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                KgpcType *const_type = create_primitive_type_with_size(SET_TYPE, (int)set_size);
                if (const_type != NULL && const_type->type_alias != NULL)
                {
                    const_type->type_alias->is_set = 1;
                    const_type->type_alias->set_element_type = is_char_set ? CHAR_TYPE : INT_TYPE;
                }
                int push_result = PushSetConstOntoScope(symtab, tree->tree_data.const_decl_data.id,
                    set_bytes, (int)set_size, const_type);
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
                /* Check if constant already exists with same value (for re-exports like ARG_MAX = UnixType.ARG_MAX) */
                HashNode_t *existing = NULL;
                int existing_scope = FindIdent(&existing, symtab, tree->tree_data.const_decl_data.id);
                if (existing_scope >= 0 && existing != NULL && 
                    existing->hash_type == HASHTYPE_CONST && 
                    existing->const_int_value == value)
                {
                    /* Same constant with same value - treat as re-export, skip silently */
                    cur = cur->next;
                    continue;
                }
                
                /* Create KgpcType if this is a set constant or has an explicit type annotation */
                KgpcType *const_type = NULL;
                /* If the const expression is an explicit typecast, prefer that as the const's type. */
                if (value_expr != NULL && value_expr->type == EXPR_TYPECAST)
                {
                    int target_tag = value_expr->resolved_type;
                    if (target_tag == UNKNOWN_TYPE &&
                        value_expr->expr_data.typecast_data.target_type_id != NULL)
                    {
                        target_tag = semcheck_map_builtin_type_name_local(
                            value_expr->expr_data.typecast_data.target_type_id);
                    }
                    if (target_tag != UNKNOWN_TYPE)
                        const_type = create_primitive_type(target_tag);
                }
                if (value_expr != NULL && value_expr->type == EXPR_SET)
                {
                    const_type = create_primitive_type(SET_TYPE);
                }
                else if (tree->tree_data.const_decl_data.type_id != NULL)
                {
                    /* Check if the type annotation is "boolean" */
                    const char *type_id = tree->tree_data.const_decl_data.type_id;
                    if (strcasecmp(type_id, "boolean") == 0)
                    {
                        const_type = create_primitive_type(BOOL);
                    }
                    /* Add more type checks as needed */
                }
                
                /* Use typed or legacy API depending on whether we have a KgpcType */
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
    /* Platform newline constants to support System/ObjPas resourcestring concatenations */
    AddBuiltinStringConst(symtab, "LineEnding", "\n");
    AddBuiltinStringConst(symtab, "sLineBreak", "\n");
    char *pchar_name = strdup("PChar");
    if (pchar_name != NULL) {
        KgpcType *pchar_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(pchar_type != NULL && "Failed to create PChar type");
        AddBuiltinType_Typed(symtab, pchar_name, pchar_type);
        destroy_kgpc_type(pchar_type);
        free(pchar_name);
    }
    char *integer_name = strdup("integer");
    if (integer_name != NULL) {
        KgpcType *integer_type = kgpc_type_from_var_type(HASHVAR_INTEGER);
        assert(integer_type != NULL && "Failed to create integer type");
        AddBuiltinType_Typed(symtab, integer_name, integer_type);
        destroy_kgpc_type(integer_type);
        free(integer_name);
    }
    char *longint_name = strdup("longint");
    if (longint_name != NULL) {
        KgpcType *longint_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(longint_type != NULL && "Failed to create longint type");
        AddBuiltinType_Typed(symtab, longint_name, longint_type);
        destroy_kgpc_type(longint_type);
        free(longint_name);
    }
    char *int64_name = strdup("int64");
    if (int64_name != NULL) {
        /* Int64 is 8 bytes (64-bit signed integer) - different from LongInt which is 4 bytes */
        KgpcType *int64_type = create_primitive_type_with_size(LONGINT_TYPE, 8);
        assert(int64_type != NULL && "Failed to create int64 type");
        AddBuiltinType_Typed(symtab, int64_name, int64_type);
        destroy_kgpc_type(int64_type);
        free(int64_name);
    }
    char *real_name = strdup("real");
    if (real_name != NULL) {
        KgpcType *real_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(real_type != NULL && "Failed to create real type");
        AddBuiltinType_Typed(symtab, real_name, real_type);
        destroy_kgpc_type(real_type);
        free(real_name);
    }
    char *single_name = strdup("single");
    if (single_name != NULL) {
        KgpcType *single_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(single_type != NULL && "Failed to create single type");
        AddBuiltinType_Typed(symtab, single_name, single_type);
        destroy_kgpc_type(single_type);
        free(single_name);
    }
    char *double_name = strdup("double");
    if (double_name != NULL) {
        KgpcType *double_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(double_type != NULL && "Failed to create double type");
        AddBuiltinType_Typed(symtab, double_name, double_type);
        destroy_kgpc_type(double_type);
        free(double_name);
    }
    char *string_name = strdup("string");
    if (string_name != NULL) {
        KgpcType *string_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(string_type != NULL && "Failed to create string type");
        AddBuiltinType_Typed(symtab, string_name, string_type);
        destroy_kgpc_type(string_type);
        free(string_name);
    char *ansistring_name = strdup("AnsiString");
    if (ansistring_name != NULL) {
        KgpcType *ansistring_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(ansistring_type != NULL && "Failed to create AnsiString type");
        AddBuiltinType_Typed(symtab, ansistring_name, ansistring_type);
        destroy_kgpc_type(ansistring_type);
        free(ansistring_name);
    }
    }
    char *unicode_name = strdup("UnicodeString");
    if (unicode_name != NULL) {
        KgpcType *unicode_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(unicode_type != NULL && "Failed to create UnicodeString type");
        AddBuiltinType_Typed(symtab, unicode_name, unicode_type);
        destroy_kgpc_type(unicode_type);
        free(unicode_name);
    }
    char *boolean_name = strdup("boolean");
    if (boolean_name != NULL) {
        KgpcType *boolean_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(boolean_type != NULL && "Failed to create boolean type");
        AddBuiltinType_Typed(symtab, boolean_name, boolean_type);
        destroy_kgpc_type(boolean_type);
        free(boolean_name);
    }
    char *char_name = strdup("char");
    if (char_name != NULL) {
        KgpcType *char_type = kgpc_type_from_var_type(HASHVAR_CHAR);
        assert(char_type != NULL && "Failed to create char type");
        AddBuiltinType_Typed(symtab, char_name, char_type);
        destroy_kgpc_type(char_type);
        free(char_name);
    }
    /* WideChar is a 2-byte character type (UTF-16 code unit) */
    char *widechar_name = strdup("WideChar");
    if (widechar_name != NULL) {
        KgpcType *widechar_type = create_primitive_type_with_size(CHAR_TYPE, 2);
        assert(widechar_type != NULL && "Failed to create WideChar type");
        AddBuiltinType_Typed(symtab, widechar_name, widechar_type);
        destroy_kgpc_type(widechar_type);
        free(widechar_name);
    }
    char *file_name = strdup("file");
    if (file_name != NULL) {
        KgpcType *file_type = kgpc_type_from_var_type(HASHVAR_FILE);
        assert(file_type != NULL && "Failed to create file type");
        AddBuiltinType_Typed(symtab, file_name, file_type);
        destroy_kgpc_type(file_type);
        free(file_name);
    }
    char *text_name = strdup("text");
    if (text_name != NULL) {
        KgpcType *text_type = kgpc_type_from_var_type(HASHVAR_TEXT);
        assert(text_type != NULL && "Failed to create text type");
        AddBuiltinType_Typed(symtab, text_name, text_type);
        destroy_kgpc_type(text_type);
        free(text_name);
    }
    char *pointer_name = strdup("Pointer");
    if (pointer_name != NULL) {
        KgpcType *pointer_type = create_pointer_type(NULL); // Untyped pointer
        assert(pointer_type != NULL && "Failed to create Pointer type");
        AddBuiltinType_Typed(symtab, pointer_name, pointer_type);
        destroy_kgpc_type(pointer_type);
        free(pointer_name);
    }
    /* Additional type aliases for FPC compatibility */
    /* Note: Cardinal, LongWord, DWord are 32-bit unsigned integers in Pascal.
     * Since we don't have distinct unsigned types, we map them to HASHVAR_INTEGER (32-bit).
     * Byte, Word, ShortInt, SmallInt are smaller types but we also use HASHVAR_INTEGER
     * since there's no HASHVAR for 8-bit or 16-bit integers. */
    char *cardinal_name = strdup("Cardinal");
    if (cardinal_name != NULL) {
        KgpcType *cardinal_type = kgpc_type_from_var_type(HASHVAR_INTEGER);
        assert(cardinal_type != NULL && "Failed to create Cardinal type");
        AddBuiltinType_Typed(symtab, cardinal_name, cardinal_type);
        destroy_kgpc_type(cardinal_type);
        free(cardinal_name);
    }
    char *longword_name = strdup("LongWord");
    if (longword_name != NULL) {
        KgpcType *longword_type = kgpc_type_from_var_type(HASHVAR_INTEGER);
        assert(longword_type != NULL && "Failed to create LongWord type");
        AddBuiltinType_Typed(symtab, longword_name, longword_type);
        destroy_kgpc_type(longword_type);
        free(longword_name);
    }
    char *dword_name = strdup("DWord");
    if (dword_name != NULL) {
        KgpcType *dword_type = kgpc_type_from_var_type(HASHVAR_INTEGER);
        assert(dword_type != NULL && "Failed to create DWord type");
        AddBuiltinType_Typed(symtab, dword_name, dword_type);
        destroy_kgpc_type(dword_type);
        free(dword_name);
    }
    char *byte_name = strdup("Byte");
    if (byte_name != NULL) {
        KgpcType *byte_type = create_primitive_type_with_size(INT_TYPE, 1);
        assert(byte_type != NULL && "Failed to create Byte type");
        AddBuiltinType_Typed(symtab, byte_name, byte_type);
        destroy_kgpc_type(byte_type);
        free(byte_name);
    }
    char *word_name = strdup("Word");
    if (word_name != NULL) {
        KgpcType *word_type = create_primitive_type_with_size(INT_TYPE, 2);
        assert(word_type != NULL && "Failed to create Word type");
        AddBuiltinType_Typed(symtab, word_name, word_type);
        destroy_kgpc_type(word_type);
        free(word_name);
    }
    char *shortint_name = strdup("ShortInt");
    if (shortint_name != NULL) {
        KgpcType *shortint_type = create_primitive_type_with_size(INT_TYPE, 1);
        assert(shortint_type != NULL && "Failed to create ShortInt type");
        AddBuiltinType_Typed(symtab, shortint_name, shortint_type);
        destroy_kgpc_type(shortint_type);
        free(shortint_name);
    }
    char *smallint_name = strdup("SmallInt");
    if (smallint_name != NULL) {
        KgpcType *smallint_type = create_primitive_type_with_size(INT_TYPE, 2);
        assert(smallint_type != NULL && "Failed to create SmallInt type");
        AddBuiltinType_Typed(symtab, smallint_name, smallint_type);
        destroy_kgpc_type(smallint_type);
        free(smallint_name);
    }
    /* QWord and UInt64 are 64-bit unsigned integers (8 bytes) */
    char *qword_name = strdup("QWord");
    if (qword_name != NULL) {
        KgpcType *qword_type = create_primitive_type_with_size(LONGINT_TYPE, 8);
        assert(qword_type != NULL && "Failed to create QWord type");
        AddBuiltinType_Typed(symtab, qword_name, qword_type);
        destroy_kgpc_type(qword_type);
        free(qword_name);
    }
    char *uint64_name = strdup("UInt64");
    if (uint64_name != NULL) {
        KgpcType *uint64_type = create_primitive_type_with_size(LONGINT_TYPE, 8);
        assert(uint64_type != NULL && "Failed to create UInt64 type");
        AddBuiltinType_Typed(symtab, uint64_name, uint64_type);
        destroy_kgpc_type(uint64_type);
        free(uint64_name);
    }

    AddBuiltinRealConst(symtab, "Pi", acos(-1.0));

    /* Builtin procedures - procedures have no return type */
    char *setlength_name = strdup("SetLength");
    if (setlength_name != NULL) {
        KgpcType *setlength_type = create_procedure_type(NULL, NULL);
        assert(setlength_type != NULL && "Failed to create SetLength procedure type");
        AddBuiltinProc_Typed(symtab, setlength_name, setlength_type);
        destroy_kgpc_type(setlength_type);
        free(setlength_name);
    }

    char *write_name = strdup("write");
    if (write_name != NULL) {
        KgpcType *write_type = create_procedure_type(NULL, NULL);
        assert(write_type != NULL && "Failed to create write procedure type");
        AddBuiltinProc_Typed(symtab, write_name, write_type);
        destroy_kgpc_type(write_type);
        free(write_name);
    }

    char *writeln_name = strdup("writeln");
    if (writeln_name != NULL) {
        KgpcType *writeln_type = create_procedure_type(NULL, NULL);
        assert(writeln_type != NULL && "Failed to create writeln procedure type");
        AddBuiltinProc_Typed(symtab, writeln_name, writeln_type);
        destroy_kgpc_type(writeln_type);
        free(writeln_name);
    }

    char *read_name = strdup("read");
    if (read_name != NULL) {
        KgpcType *read_type = create_procedure_type(NULL, NULL);
        assert(read_type != NULL && "Failed to create read procedure type");
        AddBuiltinProc_Typed(symtab, read_name, read_type);
        destroy_kgpc_type(read_type);
        free(read_name);
    }

    char *readln_name = strdup("readln");
    if (readln_name != NULL) {
        KgpcType *readln_type = create_procedure_type(NULL, NULL);
        assert(readln_type != NULL && "Failed to create readln procedure type");
        AddBuiltinProc_Typed(symtab, readln_name, readln_type);
        destroy_kgpc_type(readln_type);
        free(readln_name);
    }
    char *val_name = strdup("Val");
    if (val_name != NULL) {
        KgpcType *val_type = create_procedure_type(NULL, NULL);
        assert(val_type != NULL && "Failed to create Val procedure type");
        AddBuiltinProc_Typed(symtab, val_name, val_type);
        destroy_kgpc_type(val_type);
        free(val_name);
    }
    char *str_name = strdup("Str");
    if (str_name != NULL) {
        KgpcType *str_type = create_procedure_type(NULL, NULL);
        assert(str_type != NULL && "Failed to create Str procedure type");
        AddBuiltinProc_Typed(symtab, str_name, str_type);
        destroy_kgpc_type(str_type);
        free(str_name);
    }

    char *insert_name = strdup("Insert");
    if (insert_name != NULL) {
        KgpcType *insert_type = create_procedure_type(NULL, NULL);
        assert(insert_type != NULL && "Failed to create Insert procedure type");
        AddBuiltinProc_Typed(symtab, insert_name, insert_type);
        destroy_kgpc_type(insert_type);
        free(insert_name);
    }
    char *delete_name = strdup("Delete");
    if (delete_name != NULL) {
        KgpcType *delete_type = create_procedure_type(NULL, NULL);
        assert(delete_type != NULL && "Failed to create Delete procedure type");
        AddBuiltinProc_Typed(symtab, delete_name, delete_type);
        destroy_kgpc_type(delete_type);
        free(delete_name);
    }


    char *inc_name = strdup("Inc");
    if (inc_name != NULL) {
        KgpcType *inc_type = create_procedure_type(NULL, NULL);
        assert(inc_type != NULL && "Failed to create Inc procedure type");
        AddBuiltinProc_Typed(symtab, inc_name, inc_type);
        destroy_kgpc_type(inc_type);
        free(inc_name);
    }

    char *dec_name = strdup("Dec");
    if (dec_name != NULL) {
        KgpcType *dec_type = create_procedure_type(NULL, NULL);
        assert(dec_type != NULL && "Failed to create Dec procedure type");
        AddBuiltinProc_Typed(symtab, dec_name, dec_type);
        destroy_kgpc_type(dec_type);
        free(dec_name);
    }

    char *include_name = strdup("Include");
    if (include_name != NULL) {
        KgpcType *include_type = create_procedure_type(NULL, NULL);
        assert(include_type != NULL && "Failed to create Include procedure type");
        AddBuiltinProc_Typed(symtab, include_name, include_type);
        destroy_kgpc_type(include_type);
        free(include_name);
    }

    char *exclude_name = strdup("Exclude");
    if (exclude_name != NULL) {
        KgpcType *exclude_type = create_procedure_type(NULL, NULL);
        assert(exclude_type != NULL && "Failed to create Exclude procedure type");
        AddBuiltinProc_Typed(symtab, exclude_name, exclude_type);
        destroy_kgpc_type(exclude_type);
        free(exclude_name);
    }


    char *new_name = strdup("New");
    if (new_name != NULL) {
        KgpcType *new_type = create_procedure_type(NULL, NULL);
        assert(new_type != NULL && "Failed to create New procedure type");
        AddBuiltinProc_Typed(symtab, new_name, new_type);
        destroy_kgpc_type(new_type);
        free(new_name);
    }

    char *dispose_name = strdup("Dispose");
    if (dispose_name != NULL) {
        KgpcType *dispose_type = create_procedure_type(NULL, NULL);
        assert(dispose_type != NULL && "Failed to create Dispose procedure type");
        AddBuiltinProc_Typed(symtab, dispose_name, dispose_type);
        destroy_kgpc_type(dispose_type);
        free(dispose_name);
    }

    /* Builtin functions - functions have return types */
    char *length_name = strdup("Length");
    if (length_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Length");
        KgpcType *length_type = create_procedure_type(NULL, return_type);
        assert(length_type != NULL && "Failed to create Length function type");
        AddBuiltinFunction_Typed(symtab, length_name, length_type);
        destroy_kgpc_type(length_type);
        free(length_name);
    }

    char *copy_name = strdup("Copy");
    if (copy_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(return_type != NULL && "Failed to create return type for Copy");
        KgpcType *copy_type = create_procedure_type(NULL, return_type);
        assert(copy_type != NULL && "Failed to create Copy function type");
        AddBuiltinFunction_Typed(symtab, copy_name, copy_type);
        destroy_kgpc_type(copy_type);
        free(copy_name);
    }
    char *eof_name = strdup("EOF");
    if (eof_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for EOF");
        KgpcType *eof_type = create_procedure_type(NULL, return_type);
        assert(eof_type != NULL && "Failed to create EOF function type");
        AddBuiltinFunction_Typed(symtab, eof_name, eof_type);
        destroy_kgpc_type(eof_type);
        free(eof_name);
    }

    char *eoln_name = strdup("EOLN");
    if (eoln_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for EOLN");
        KgpcType *eoln_type = create_procedure_type(NULL, return_type);
        assert(eoln_type != NULL && "Failed to create EOLN function type");
        AddBuiltinFunction_Typed(symtab, eoln_name, eoln_type);
        destroy_kgpc_type(eoln_type);
        free(eoln_name);
    }

    char *sizeof_name = strdup("SizeOf");
    if (sizeof_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for SizeOf");
        KgpcType *sizeof_type = create_procedure_type(NULL, return_type);
        assert(sizeof_type != NULL && "Failed to create SizeOf function type");
        AddBuiltinFunction_Typed(symtab, sizeof_name, sizeof_type);
        destroy_kgpc_type(sizeof_type);
        free(sizeof_name);
    }

    char *chr_name = strdup("Chr");
    if (chr_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_CHAR);
        assert(return_type != NULL && "Failed to create return type for Chr");
        KgpcType *chr_type = create_procedure_type(NULL, return_type);
        assert(chr_type != NULL && "Failed to create Chr function type");
        AddBuiltinFunction_Typed(symtab, chr_name, chr_type);
        destroy_kgpc_type(chr_type);
        free(chr_name);
    }

    char *ord_name = strdup("Ord");
    if (ord_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Ord");
        KgpcType *ord_type = create_procedure_type(NULL, return_type);
        assert(ord_type != NULL && "Failed to create Ord function type");
        AddBuiltinFunction_Typed(symtab, ord_name, ord_type);
        destroy_kgpc_type(ord_type);
        free(ord_name);
    }

    char *odd_name = strdup("Odd");
    if (odd_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for Odd");
        KgpcType *odd_type = create_procedure_type(NULL, return_type);
        assert(odd_type != NULL && "Failed to create Odd function type");
        AddBuiltinFunction_Typed(symtab, odd_name, odd_type);
        destroy_kgpc_type(odd_type);
        free(odd_name);
    }
    char *upcase_name = strdup("UpCase");
    if (upcase_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_CHAR);
        assert(return_type != NULL && "Failed to create return type for UpCase");
        KgpcType *upcase_type = create_procedure_type(NULL, return_type);
        assert(upcase_type != NULL && "Failed to create UpCase function type");
        AddBuiltinFunction_Typed(symtab, upcase_name, upcase_type);
        destroy_kgpc_type(upcase_type);
        free(upcase_name);
    }

    char *randseed_name = strdup("RandSeed");
    if (randseed_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for RandSeed");
        KgpcType *randseed_type = create_procedure_type(NULL, return_type);
        assert(randseed_type != NULL && "Failed to create RandSeed function type");
        AddBuiltinFunction_Typed(symtab, randseed_name, randseed_type);
        destroy_kgpc_type(randseed_type);
        free(randseed_name);
    }

    char *sqr_name = strdup("Sqr");
    if (sqr_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Sqr");
        KgpcType *sqr_type = create_procedure_type(NULL, return_type);
        assert(sqr_type != NULL && "Failed to create Sqr function type");
        AddBuiltinFunction_Typed(symtab, sqr_name, sqr_type);
        destroy_kgpc_type(sqr_type);
        free(sqr_name);
    }

    char *ln_name = strdup("Ln");
    if (ln_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Ln");
        KgpcType *ln_type = create_procedure_type(NULL, return_type);
        assert(ln_type != NULL && "Failed to create Ln function type");
        AddBuiltinFunction_Typed(symtab, ln_name, ln_type);
        destroy_kgpc_type(ln_type);
        free(ln_name);
    }

    char *exp_name = strdup("Exp");
    if (exp_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Exp");
        KgpcType *exp_type = create_procedure_type(NULL, return_type);
        assert(exp_type != NULL && "Failed to create Exp function type");
        AddBuiltinFunction_Typed(symtab, exp_name, exp_type);
        destroy_kgpc_type(exp_type);
        free(exp_name);
    }

    char *random_name = strdup("Random");
    if (random_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Random");
        KgpcType *random_type = create_procedure_type(NULL, return_type);
        assert(random_type != NULL && "Failed to create Random function type");
        AddBuiltinFunction_Typed(symtab, random_name, random_type);
        destroy_kgpc_type(random_type);
        free(random_name);
    }
    char *randomrange_name = strdup("RandomRange");
    if (randomrange_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for RandomRange");
        KgpcType *randomrange_type = create_procedure_type(NULL, return_type);
        assert(randomrange_type != NULL && "Failed to create RandomRange function type");
        AddBuiltinFunction_Typed(symtab, randomrange_name, randomrange_type);
        destroy_kgpc_type(randomrange_type);
        free(randomrange_name);
    }

    char *high_name = strdup("High");
    if (high_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for High");
        KgpcType *high_type = create_procedure_type(NULL, return_type);
        assert(high_type != NULL && "Failed to create High function type");
        AddBuiltinFunction_Typed(symtab, high_name, high_type);
        destroy_kgpc_type(high_type);
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
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after args: %d\n", return_val);
#endif

    return_val += predeclare_enum_literals(symtab, tree->tree_data.program_data.type_declaration);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, tree->tree_data.program_data.type_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after type predeclare: %d\n", return_val);
#endif

    /* Predeclare subprograms so they can be referenced in const initializers */
    return_val += predeclare_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
    {
        ListNode_t *debug_cur = tree->tree_data.program_data.type_declaration;
        while (debug_cur != NULL)
        {
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *debug_tree = (Tree_t *)debug_cur->cur;
                if (debug_tree->type == TREE_TYPE_DECL && debug_tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] program type decl: %s kind=%d\n",
                        debug_tree->tree_data.type_decl_data.id,
                        debug_tree->tree_data.type_decl_data.kind);
                }
            }
            debug_cur = debug_cur->next;
        }
    }
    return_val += semcheck_const_decls(symtab, tree->tree_data.program_data.const_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after consts: %d\n", return_val);
#endif

    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after types: %d\n", return_val);
#endif

    return_val += semcheck_decls(symtab, tree->tree_data.program_data.var_declaration);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after vars: %d\n", return_val);
#endif

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after subprograms: %d\n", return_val);
#endif

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, INT_MAX);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after body: %d\n", return_val);
#endif

    // Semantic check finalization statements from units
    if (tree->tree_data.program_data.finalization_statements != NULL) {
        ListNode_t *final_node = tree->tree_data.program_data.finalization_statements;
        while (final_node != NULL) {
            if (final_node->type == LIST_STMT && final_node->cur != NULL) {
                struct Statement *final_stmt = (struct Statement *)final_node->cur;
                return_val += semcheck_stmt(symtab, final_stmt, INT_MAX);
            }
            final_node = final_node->next;
        }
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] Before optimize: body_statement = %p\n", 
                    (void*)tree->tree_data.program_data.body_statement);
            if (tree->tree_data.program_data.body_statement != NULL) {
                fprintf(stderr, "[KGPC] Body statement type: %d\n", 
                        tree->tree_data.program_data.body_statement->type);
                if (tree->tree_data.program_data.body_statement->type == STMT_COMPOUND_STATEMENT) {
                    fprintf(stderr, "[KGPC] Compound statement list: %p\n",
                            (void*)tree->tree_data.program_data.body_statement->stmt_data.compound_statement);
                }
            }
        }
        optimize(symtab, tree);
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] After optimize: body_statement = %p\n", 
                    (void*)tree->tree_data.program_data.body_statement);
        }
    }

    /* Keep the outermost scope alive for code generation. DestroySymTab will clean it up. */
    return return_val;
}

/* Semantic check for a unit */
int semcheck_unit(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    PushScope(symtab);

    /* Check interface section */
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.interface_type_decls);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.interface_type_decls);
    /* Predeclare interface subprograms before const evaluation */
    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    return_val += semcheck_const_decls(symtab, tree->tree_data.unit_data.interface_const_decls);
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.interface_type_decls);
    return_val += semcheck_decls(symtab, tree->tree_data.unit_data.interface_var_decls);

    /* Check implementation section */
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.implementation_type_decls);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.implementation_type_decls);
    /* Ensure implementation procedures are visible to const initializers */
    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    return_val += semcheck_const_decls(symtab, tree->tree_data.unit_data.implementation_const_decls);
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.implementation_type_decls);
    return_val += semcheck_decls(symtab, tree->tree_data.unit_data.implementation_var_decls);

    /* Check subprograms */
    return_val += semcheck_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);

    /* Check initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL) {
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.initialization, INT_MAX);
    }

    /* Check finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL) {
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.finalization, INT_MAX);
    }

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

        /* UNTYPED procedure parameters - use NULL KgpcType */
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
        if (tree->type == TREE_VAR_DECL)
        {
            if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
            }
            if (resolved_type != NULL && resolved_type->type != NULL)
            {
                kgpc_type_retain(resolved_type->type);
                tree->tree_data.var_decl_data.cached_kgpc_type = resolved_type->type;
            }
        }
        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (ids->cur != NULL && strcmp((char *)ids->cur, "Sock") == 0) {
#ifdef DEBUG
                     fprintf(stderr, "DEBUG: semcheck_decls processing Sock. type_id=%s resolved_type=%p\n",
                         tree->tree_data.var_decl_data.type_id ? tree->tree_data.var_decl_data.type_id : "<null>",
                         resolved_type);
#endif
                }

                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    HashNode_t *type_node = resolved_type;
                    const char *type_id = tree->tree_data.var_decl_data.type_id;
                    
                    /* Check if it's a builtin type even if not in symbol table */
                    if (type_node == NULL)
                    {
                        /* Check for builtin types */
                        if (pascal_identifier_equals(type_id, "Integer"))
                            var_type = HASHVAR_INTEGER;
                        else if (pascal_identifier_equals(type_id, "LongInt"))
                            var_type = HASHVAR_LONGINT;
                        else if (pascal_identifier_equals(type_id, "SizeUInt") || pascal_identifier_equals(type_id, "QWord") || 
                                 pascal_identifier_equals(type_id, "NativeUInt"))
                            var_type = HASHVAR_LONGINT;  /* Unsigned 64-bit on x86-64 */
                        else if (pascal_identifier_equals(type_id, "Real") || pascal_identifier_equals(type_id, "Double"))
                            var_type = HASHVAR_REAL;
                        else if (pascal_identifier_equals(type_id, "String") || pascal_identifier_equals(type_id, "AnsiString"))
                            var_type = HASHVAR_PCHAR;
                        else if (pascal_identifier_equals(type_id, "ShortString"))
                        {
                            /* ShortString is array[0..255] of Char with length at index 0 */
                            var_type = HASHVAR_ARRAY;
                        }
                        else if (pascal_identifier_equals(type_id, "Char"))
                            var_type = HASHVAR_CHAR;
                        else if (pascal_identifier_equals(type_id, "Boolean"))
                            var_type = HASHVAR_BOOLEAN;
                        else if (pascal_identifier_equals(type_id, "Pointer"))
                            var_type = HASHVAR_POINTER;
                        else if (pascal_identifier_equals(type_id, "Byte") || pascal_identifier_equals(type_id, "Word"))
                            var_type = HASHVAR_INTEGER;
                        else
                        {
                            semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                            return_val++;
                            var_type = HASHVAR_UNTYPED;
                        }
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
                            KgpcType *points_to = NULL;
                            if (type_node->type != NULL)
                            {
                                kgpc_type_retain(type_node->type);
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

                            KgpcType *pointer_type = create_pointer_type(points_to);
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
                            KgpcType *element_type = NULL;
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
                                    /* Get KgpcType from element_type_node */
                                    element_type = element_type_node->type;
                                    assert(element_type != NULL && "Element type node must have KgpcType");
                                }
                            }
                            else if (element_type_tag != UNKNOWN_TYPE)
                            {
                                /* Direct primitive type tag - use create_primitive_type */
                                element_type = create_primitive_type(element_type_tag);
                            }
                            
                            assert(element_type != NULL && "Array element type must be resolvable");
                            
                            /* Create array KgpcType */
                            KgpcType *array_type = create_array_type(element_type, start, end);
                            assert(array_type != NULL && "Failed to create array type");
                            
                            /* Set type_alias on KgpcType so it's properly propagated */
                            kgpc_type_set_type_alias(array_type, alias);
                            
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
                        
                        /* For non-array type references (e.g., enum, set, file, record), create KgpcType from type_node */
                        KgpcType *var_kgpc_type = NULL;
                        if (type_node->type != NULL)
                        {
                            /* Type node already has a KgpcType - reference it (don't clone) */
                            var_kgpc_type = type_node->type;
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                        }
                        else
                        {
                            /* Fallback: create KgpcType from legacy fields using helpers */
                            struct RecordType *record_type = get_record_type_from_node(type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                var_kgpc_type = create_record_type(record_type);
                            }
                            else if (var_type == HASHVAR_POINTER)
                            {
                                /* For pointer types, we need to create a pointer KgpcType */
                                /* Get the TypeAlias to find what the pointer points to */
                                struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                                if (type_alias != NULL && type_alias->is_pointer)
                                {
                                    KgpcType *points_to = NULL;
                                    
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
                                        var_kgpc_type = create_pointer_type(points_to);
                                        kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                                    }
                                }
                            }
                            else
                            {
                                var_kgpc_type = kgpc_type_from_var_type(var_type);
                            }
                            
                            struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                            if (var_kgpc_type != NULL && type_alias != NULL && var_type != HASHVAR_POINTER)
                            {
                                kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                            }
                            
                            /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
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
                
                /* Create KgpcType for typed variables */
                KgpcType *var_kgpc_type = NULL;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use KgpcType from resolved type if available */
                    var_kgpc_type = resolved_type->type;
                }
                else if (tree->tree_data.var_decl_data.inline_record_type != NULL)
                {
                    /* Handle inline record type declarations */
                    var_kgpc_type = create_record_type(clone_record_type(tree->tree_data.var_decl_data.inline_record_type));
                }
                else
                {
                    /* Special handling for ShortString - create as array[0..255] of Char */
                    if (var_type == HASHVAR_ARRAY && 
                        tree->tree_data.var_decl_data.type_id != NULL &&
                        pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"))
                    {
                        /* Create ShortString as array[0..255] of Char */
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        var_kgpc_type = create_array_type(char_type, 0, 255);
                    }
                    else
                    {
                        /* Create KgpcType from var_type */
                        var_kgpc_type = kgpc_type_from_var_type(var_type);
                    }
                    
                    /* Add metadata from resolved_type if present */
                    if (var_kgpc_type != NULL && resolved_type != NULL)
                    {
                        struct TypeAlias *type_alias = get_type_alias_from_node(resolved_type);
                        if (type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                        }
                        struct RecordType *record_type = get_record_type_from_node(resolved_type);
                        if (record_type != NULL && var_kgpc_type->kind == TYPE_KIND_RECORD)
                        {
                            /* Use the canonical RecordType from the symbol table,
                             * not a clone. This ensures type identity checks work correctly. */
                            var_kgpc_type->info.record_info = record_type;
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                        var_kgpc_type != NULL)
                    {
                        kgpc_type_set_type_alias(var_kgpc_type,
                            tree->tree_data.var_decl_data.inline_type_alias);
                    }
                }
                
                if (var_kgpc_type != NULL && kgpc_type_is_record(var_kgpc_type))
                {
                    struct RecordType *var_record = kgpc_type_get_record(var_kgpc_type);
                    if (var_record != NULL)
                    {
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (var_record->generic_decl != NULL && 
                                                         var_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, var_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                return_val += 1;
                            }
                        }
                    }
                }

                /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                /* Use PushArrayOntoScope_Typed for ShortString (which is array[0..255] of Char) */
                int is_shortstring = (var_type == HASHVAR_ARRAY && 
                                     tree->tree_data.var_decl_data.type_id != NULL &&
                                     pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"));
                
                if (is_shortstring)
                    func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                else
                    func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                
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
                
                KgpcType *element_type = NULL;
                
                /* If type_id is specified, resolve it to get the element type */
                if (tree->tree_data.arr_decl_data.type_id != NULL)
                {
                    HashNode_t *element_type_node = NULL;
                    if (FindIdent(&element_type_node, symtab, tree->tree_data.arr_decl_data.type_id) >= 0)
                    {
                        /* Use the KgpcType from the resolved type node */
                        element_type = element_type_node->type;
                        if (element_type == NULL)
                        {
                            /* Fallback for migration: some nodes may not have KgpcType populated yet.
                             * Try to construct KgpcType from legacy record type information. */
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
                    
                    element_type = kgpc_type_from_var_type(var_type);
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
                
                KgpcType *array_type = create_array_type(
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
                        
                        kgpc_type_set_type_alias(array_type, temp_alias);
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
                            case POINTER_TYPE:
                                inferred_var_type = HASHVAR_POINTER;
                                normalized_type = POINTER_TYPE;
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
                                /* Replace or create KgpcType with inferred type */
                                KgpcType *inferred_kgpc_type = create_primitive_type(normalized_type);
                                if (inferred_kgpc_type != NULL)
                                {
                                    if (var_node->type != NULL)
                                    {
                                        /* Free old type and replace */
                                        destroy_kgpc_type(var_node->type);
                                    }
                                    var_node->type = inferred_kgpc_type;
                                    /* Legacy field will be populated by helper if needed */
                                }
                            }
                        }
                        else
                        {
                            enum VarType current_var_type = get_var_type_from_node(var_node);
                            int compatible = (inferred_var_type == current_var_type);
                            
                            /* Allow mixing Integer and LongInt for initializers */
                            if (!compatible)
                            {
                                if ((inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT) &&
                                    (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }
                            
                            /* Allow pointer type initializers (including nil) for pointer variables.
                             * Both the initializer expression type and the declared variable type must
                             * be pointer types. This enables patterns like: var my_ptr: PChar = nil; */
                            if (!compatible && inferred_var_type == HASHVAR_POINTER &&
                                current_var_type == HASHVAR_POINTER)
                            {
                                compatible = 1;
                            }

                            if (!compatible)
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
    
#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprogram called for %s\n", subprogram->tree_data.subprogram_data.id);
#endif

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
        debug_external = (getenv("KGPC_DEBUG_EXTERNAL") != NULL);
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
    int already_declared = 0;
    
    /* For overloaded functions, find the correct overload by matching mangled name */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        
        while (cur != NULL && existing_decl == NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                existing_decl = candidate;
                already_declared = 1;
            }
            cur = cur->next;
        }
        
        if (all_matches != NULL)
            DestroyList(all_matches);
    }
    
    /* Fallback to simple lookup if no mangled name or no match found */
    if (!already_declared)
        already_declared = (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0);

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
        /* Create KgpcType for the procedure */
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = NULL;
        int created_new_type = 0;

        if (already_declared && existing_decl != NULL && existing_decl->type != NULL)
        {
            proc_type = existing_decl->type;
        }
        else
        {
            proc_type = create_procedure_type(
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
            created_new_type = 1;
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
                        /* If element type is a named type, look it up to get proper KgpcType */
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
        
        // Use the typed version to properly set the KgpcType
        // Skip if already declared
        if (!already_declared)
        {
            func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            proc_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            FindIdent(&existing_decl, symtab, id_to_use_for_lookup);
        }
        else
        {
            func_return = 0;  /* No error since it's expected to be already declared */
            
            /* If we created a new type but it was already declared (e.g. existing had no type), update it */
            if (created_new_type && existing_decl != NULL && existing_decl->type == NULL)
            {
                existing_decl->type = proc_type;
            }
            else if (created_new_type)
            {
                /* We created a type but didn't use it (shouldn't happen if logic is correct, but for safety) */
                destroy_kgpc_type(proc_type);
            }
        }

        PushScope(symtab);
        
        if (existing_decl != NULL && existing_decl->type != NULL)
        {
            kgpc_type_retain(existing_decl->type);
            PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id,
                existing_decl->type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            destroy_kgpc_type(existing_decl->type);
        }

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        KgpcType *return_kgpc_type = NULL;

        /* Reuse the type created during predeclaration when possible. */
        if (already_declared && existing_decl != NULL &&
            existing_decl->type != NULL &&
            existing_decl->type->kind == TYPE_KIND_PROCEDURE)
        {
            return_kgpc_type = kgpc_type_get_return_type(existing_decl->type);
        }

        /* If the predeclare step could not resolve the type (e.g., inline array),
         * build it now and update the existing declaration. */
        if (return_kgpc_type == NULL)
        {
            return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val);
#ifdef DEBUG
            if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        }

        KgpcType *func_type = NULL;
        if (!already_declared)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_subprogram %s NOT already declared in Pass 2!\n", subprogram->tree_data.subprogram_data.id);
#endif
            func_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                return_kgpc_type
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
                        return_kgpc_type
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
                else if (return_kgpc_type != NULL)
                {
                    existing_decl->type->info.proc_info.return_type = return_kgpc_type;
                }
            }
        }

        PushScope(symtab);
        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with KgpcType
        // Always use _Typed variant, even if KgpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, subprogram->tree_data.subprogram_data.id, return_kgpc_type);
        
/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" (or "result") is already used as a parameter or local variable */
        HashNode_t *result_check = NULL;
        if (FindIdent(&result_check, symtab, "Result") == -1)
        {
            /* "Result" is not already declared, so we can add it as an alias */
            PushFuncRetOntoScope_Typed(symtab, "Result", return_kgpc_type);
        }

        /* For class methods, also add an alias using the unmangled method name (suffix after __) */
        const char *alias_suffix = NULL;
        if (subprogram->tree_data.subprogram_data.id != NULL)
        {
            const char *sep = strstr(subprogram->tree_data.subprogram_data.id, "__");
            if (sep != NULL && sep[2] != '\0')
                alias_suffix = sep + 2;
        }
        if (alias_suffix != NULL && alias_suffix[0] != '\0')
        {
            size_t alias_len = strcspn(alias_suffix, "_");
            if (alias_len > 0 && alias_len < 128)
            {
                char alias_buf[128];
                memcpy(alias_buf, alias_suffix, alias_len);
                alias_buf[alias_len] = '\0';

                HashNode_t *suffix_check = NULL;
                if (FindIdent(&suffix_check, symtab, alias_buf) == -1)
                    PushFuncRetOntoScope_Typed(symtab, alias_buf, return_kgpc_type);
            }
        }
        /* Note: We don't check for "result" anymore since it conflicts with built-in Result alias */

        /* Note: Type metadata now in KgpcType, no post-creation writes needed */

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
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after args: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
    return_val += semcheck_type_decls(symtab, subprogram->tree_data.subprogram_data.type_declarations);
    return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after decls: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                    new_max_scope, subprogram);

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        PopScope(symtab);
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_subprogram %s returning (no body): %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
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
        int func_stmt_ret = semcheck_func_stmt(symtab,
                body, new_max_scope);
        return_val += func_stmt_ret;
#ifdef DEBUG
        if (func_stmt_ret > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after semcheck_func_stmt: %d\n", subprogram->tree_data.subprogram_data.id, func_stmt_ret);
#endif

        /* Allow functions with asm blocks to skip explicit return assignment */
        int has_asm = statement_contains_asm_block(body);
        
        /* Constructors implicitly yield the constructed instance, so do not
         * require an explicit assignment to the return variable. */
        int is_constructor = 0;
        if (subprogram->tree_data.subprogram_data.id != NULL) {
            const char *suffix = strstr(subprogram->tree_data.subprogram_data.id, "__");
            const char *name = (suffix != NULL && suffix[2] != '\0') ? suffix + 2
                                                                     : subprogram->tree_data.subprogram_data.id;
            if (strcasecmp(name, "create") == 0) {
                is_constructor = 1;
            }
        }

        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = is_constructor ? 1 : (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }

        /* Methods use mangled identifiers like Class__Func; allow assignments to the
         * unmangled function name (after the final '__') to satisfy the return check. */
        if (!return_was_assigned && subprogram->tree_data.subprogram_data.id != NULL) {
            const char *id = subprogram->tree_data.subprogram_data.id;
            const char *sep = strstr(id, "__");
            if (sep != NULL && sep[2] != '\0') {
                HashNode_t *suffix_node = NULL;
                if (FindIdent(&suffix_node, symtab, (char *)(sep + 2)) == 0 && suffix_node != NULL) {
                    return_was_assigned = (suffix_node->mutated != NO_MUTATE);
                }
            }
        }
        
        if(!return_was_assigned && !has_asm)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: Checking return for %s. cname_override=%s cname_flag=%d return_was_assigned=%d\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.cname_override ? subprogram->tree_data.subprogram_data.cname_override : "<null>",
                subprogram->tree_data.subprogram_data.cname_flag,
                return_was_assigned);
#endif

            /* Skip check for external functions (cname_flag or cname_override) or declarations without bodies */
            int is_external = (subprogram->tree_data.subprogram_data.cname_override != NULL) ||
                              (subprogram->tree_data.subprogram_data.cname_flag != 0);
            int has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
            if (is_external || !has_body)
            {
                /* External function, no body expected */
            }
            else
            {
                fprintf(stderr,
                    "Error in function %s: no return statement declared in function body!\n\n",
                    subprogram->tree_data.subprogram_data.id);
                ++return_val;
            }
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

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s returning at end: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
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
    
    /* Check if this specific overload is already declared (by matching mangled name) */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        int already_exists = 0;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                already_exists = 1;
                break;
            }
            /* If this exact subprogram was already registered (even with a different
             * mangled name), reuse that declaration instead of creating a duplicate. */
            if (candidate != NULL && candidate->type != NULL &&
                candidate->type->kind == TYPE_KIND_PROCEDURE &&
                candidate->type->info.proc_info.definition == subprogram)
            {
                if (candidate->mangled_id != NULL)
                    free(candidate->mangled_id);
                candidate->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
                already_exists = 1;
                break;
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);
        
        if (already_exists)
            return 0;  /* Already declared - skip to avoid duplicates */
    }
    
    /**** PLACE SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = create_procedure_type(
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
        KgpcType *return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val);
#ifdef DEBUG
        if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        
        /* Create function KgpcType */
        KgpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_kgpc_type  /* functions have a return type */
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
    
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s returning error: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
/* Forward declaration - we'll define this after semcheck_subprogram */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

/* Predeclare a list of subprograms without processing bodies.
 * Safe to call multiple times thanks to duplicate checks in predeclare_subprogram. */
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    int return_val = 0;
    ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev);
        /* Optionally predeclare nested subprograms so their names are also visible early */
        if (child->tree_data.subprogram_data.subprograms != NULL)
        {
            return_val += predeclare_subprograms(symtab,
                child->tree_data.subprogram_data.subprograms, max_scope_lev + 1, child);
        }
        cur = cur->next;
    }
    return return_val;
}

int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprograms called. subprograms=%p\n", subprograms);
#endif

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
    
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprograms error after Pass 1: %d\n", return_val);
#endif

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
