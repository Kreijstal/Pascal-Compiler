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
#include "SemCheck.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../Optimizer/optimizer.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"
#include "../ParseTree/type_tags.h"
#include "../ParseTree/GpcType.h"
#include "../ParseTree/from_cparser.h"
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
    if(pascal_identifier_equals(id, "main"))
    {
        fprintf(stderr, "ERROR: main is special keyword, it cannot be a program, or subprogram\n");
        return 1;
    }

    return 0;
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
    return hashnode_get_var_type(node);
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);

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
        case EXPR_BOOL:
            *out_value = expr->expr_data.bool_value ? 1 : 0;
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
            
            if (binding->is_override) {
                /* Find and replace parent's VMT entry */
                ListNode_t *vmt_entry = vmt;
                int found = 0;
                while (vmt_entry != NULL) {
                    struct MethodInfo *info = (struct MethodInfo *)vmt_entry->cur;
                    if (info != NULL && info->name != NULL &&
                        strcasecmp(info->name, binding->method_name) == 0) {
                        /* Replace with derived class's version */
                        free(info->mangled_name);
                        info->mangled_name = mangled ? strdup(mangled) : NULL;
                        info->is_override = 1;
                        found = 1;
                        break;
                    }
                    vmt_entry = vmt_entry->next;
                }
                
                if (!found) {
                    fprintf(stderr, "Error on line %d, override method '%s' has no virtual parent method\n",
                            line_num, binding->method_name);
                }
            } else if (binding->is_virtual) {
                /* Add new virtual method to VMT */
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
                gpc_type_set_type_alias(gpc_type, alias_info);
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

        struct Expression *value_expr = tree->tree_data.const_decl_data.value;
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
        free(pchar_name);
    }
    char *integer_name = strdup("integer");
    if (integer_name != NULL) {
        GpcType *integer_type = gpc_type_from_var_type(HASHVAR_INTEGER);
        assert(integer_type != NULL && "Failed to create integer type");
        AddBuiltinType_Typed(symtab, integer_name, integer_type);
        free(integer_name);
    }
    char *longint_name = strdup("longint");
    if (longint_name != NULL) {
        GpcType *longint_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(longint_type != NULL && "Failed to create longint type");
        AddBuiltinType_Typed(symtab, longint_name, longint_type);
        free(longint_name);
    }
    char *real_name = strdup("real");
    if (real_name != NULL) {
        GpcType *real_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(real_type != NULL && "Failed to create real type");
        AddBuiltinType_Typed(symtab, real_name, real_type);
        free(real_name);
    }
    char *single_name = strdup("single");
    if (single_name != NULL) {
        GpcType *single_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(single_type != NULL && "Failed to create single type");
        AddBuiltinType_Typed(symtab, single_name, single_type);
        free(single_name);
    }
    char *double_name = strdup("double");
    if (double_name != NULL) {
        GpcType *double_type = gpc_type_from_var_type(HASHVAR_REAL);
        assert(double_type != NULL && "Failed to create double type");
        AddBuiltinType_Typed(symtab, double_name, double_type);
        free(double_name);
    }
    char *string_name = strdup("string");
    if (string_name != NULL) {
        GpcType *string_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(string_type != NULL && "Failed to create string type");
        AddBuiltinType_Typed(symtab, string_name, string_type);
        free(string_name);
    }
    char *boolean_name = strdup("boolean");
    if (boolean_name != NULL) {
        GpcType *boolean_type = gpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(boolean_type != NULL && "Failed to create boolean type");
        AddBuiltinType_Typed(symtab, boolean_name, boolean_type);
        free(boolean_name);
    }
    char *char_name = strdup("char");
    if (char_name != NULL) {
        GpcType *char_type = gpc_type_from_var_type(HASHVAR_CHAR);
        assert(char_type != NULL && "Failed to create char type");
        AddBuiltinType_Typed(symtab, char_name, char_type);
        free(char_name);
    }
    char *file_name = strdup("file");
    if (file_name != NULL) {
        GpcType *file_type = gpc_type_from_var_type(HASHVAR_FILE);
        assert(file_type != NULL && "Failed to create file type");
        AddBuiltinType_Typed(symtab, file_name, file_type);
        free(file_name);
    }
    char *text_name = strdup("text");
    if (text_name != NULL) {
        GpcType *text_type = gpc_type_from_var_type(HASHVAR_FILE);
        assert(text_type != NULL && "Failed to create text type");
        AddBuiltinType_Typed(symtab, text_name, text_type);
        free(text_name);
    }

    /* Builtin procedures - procedures have no return type */
    char *setlength_name = strdup("SetLength");
    if (setlength_name != NULL) {
        GpcType *setlength_type = create_procedure_type(NULL, NULL);
        assert(setlength_type != NULL && "Failed to create SetLength procedure type");
        AddBuiltinProc_Typed(symtab, setlength_name, setlength_type);
        free(setlength_name);
    }

    char *write_name = strdup("write");
    if (write_name != NULL) {
        GpcType *write_type = create_procedure_type(NULL, NULL);
        assert(write_type != NULL && "Failed to create write procedure type");
        AddBuiltinProc_Typed(symtab, write_name, write_type);
        free(write_name);
    }

    char *writeln_name = strdup("writeln");
    if (writeln_name != NULL) {
        GpcType *writeln_type = create_procedure_type(NULL, NULL);
        assert(writeln_type != NULL && "Failed to create writeln procedure type");
        AddBuiltinProc_Typed(symtab, writeln_name, writeln_type);
        free(writeln_name);
    }

    char *move_name = strdup("Move");
    if (move_name != NULL) {
        GpcType *move_type = create_procedure_type(NULL, NULL);
        assert(move_type != NULL && "Failed to create Move procedure type");
        AddBuiltinProc_Typed(symtab, move_name, move_type);
        free(move_name);
    }
    char *val_name = strdup("Val");
    if (val_name != NULL) {
        GpcType *val_type = create_procedure_type(NULL, NULL);
        assert(val_type != NULL && "Failed to create Val procedure type");
        AddBuiltinProc_Typed(symtab, val_name, val_type);
        free(val_name);
    }

    char *inc_name = strdup("Inc");
    if (inc_name != NULL) {
        GpcType *inc_type = create_procedure_type(NULL, NULL);
        assert(inc_type != NULL && "Failed to create Inc procedure type");
        AddBuiltinProc_Typed(symtab, inc_name, inc_type);
        free(inc_name);
    }

    char *new_name = strdup("New");
    if (new_name != NULL) {
        GpcType *new_type = create_procedure_type(NULL, NULL);
        assert(new_type != NULL && "Failed to create New procedure type");
        AddBuiltinProc_Typed(symtab, new_name, new_type);
        free(new_name);
    }

    char *dispose_name = strdup("Dispose");
    if (dispose_name != NULL) {
        GpcType *dispose_type = create_procedure_type(NULL, NULL);
        assert(dispose_type != NULL && "Failed to create Dispose procedure type");
        AddBuiltinProc_Typed(symtab, dispose_name, dispose_type);
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
        free(length_name);
    }

    char *copy_name = strdup("Copy");
    if (copy_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(return_type != NULL && "Failed to create return type for Copy");
        GpcType *copy_type = create_procedure_type(NULL, return_type);
        assert(copy_type != NULL && "Failed to create Copy function type");
        AddBuiltinFunction_Typed(symtab, copy_name, copy_type);
        free(copy_name);
    }
    char *eof_name = strdup("EOF");
    if (eof_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for EOF");
        GpcType *eof_type = create_procedure_type(NULL, return_type);
        assert(eof_type != NULL && "Failed to create EOF function type");
        AddBuiltinFunction_Typed(symtab, eof_name, eof_type);
        free(eof_name);
    }

    char *sizeof_name = strdup("SizeOf");
    if (sizeof_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for SizeOf");
        GpcType *sizeof_type = create_procedure_type(NULL, return_type);
        assert(sizeof_type != NULL && "Failed to create SizeOf function type");
        AddBuiltinFunction_Typed(symtab, sizeof_name, sizeof_type);
        free(sizeof_name);
    }

    char *chr_name = strdup("Chr");
    if (chr_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_PCHAR);
        assert(return_type != NULL && "Failed to create return type for Chr");
        GpcType *chr_type = create_procedure_type(NULL, return_type);
        assert(chr_type != NULL && "Failed to create Chr function type");
        AddBuiltinFunction_Typed(symtab, chr_name, chr_type);
        free(chr_name);
    }

    char *ord_name = strdup("Ord");
    if (ord_name != NULL) {
        GpcType *return_type = gpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Ord");
        GpcType *ord_type = create_procedure_type(NULL, return_type);
        assert(ord_type != NULL && "Failed to create Ord function type");
        AddBuiltinFunction_Typed(symtab, ord_name, ord_type);
        free(ord_name);
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
                        fprintf(stderr, "Error on line %d, undefined type %s!\n",
                            tree->line_num, tree->tree_data.var_decl_data.type_id);
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
                                }
                            }
                            goto next_identifier;
                        }
                        var_type = get_var_type_from_node(type_node);
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
                                var_gpc_type = create_record_type(clone_record_type(record_type));
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
                else
                    var_type = HASHVAR_REAL;
                
                /* Create GpcType for typed variables */
                GpcType *var_gpc_type = NULL;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use GpcType from resolved type if available */
                    var_gpc_type = resolved_type->type;
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
                            var_gpc_type->info.record_info = clone_record_type(record_type);
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
                                element_type = create_record_type(clone_record_type(record_type));
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
                
                GpcType *array_type = create_array_type(
                    element_type,
                    tree->tree_data.arr_decl_data.s_range,
                    tree->tree_data.arr_decl_data.e_range
                );
                assert(array_type != NULL && "Failed to create array type");
                
                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                fprintf(stderr, "Error on line %d, redeclaration of name %s!\n",
                    tree->line_num, (char *)ids->cur);
                return_val += func_return;
            }

next_identifier:
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
                }
            }
        }
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

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    if (subprogram->tree_data.subprogram_data.statement_list == NULL)
    {
        subprogram->tree_data.subprogram_data.cname_flag = 1;
    }

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
        /* Create GpcType for the procedure */
        GpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        
        // Use the typed version to properly set the GpcType
        func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        proc_type);

        PushScope(symtab);
        
        /* Create another GpcType for the recursive scope (they're separate) */
        GpcType *proc_type_recursive = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL
        );
        
        // Push it again in the new scope to allow recursion
        PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
            subprogram->tree_data.subprogram_data.mangled_id,
            proc_type_recursive);

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        /* Need to additionally extract the return type */
        HashNode_t *return_type_node = NULL;
        GpcType *return_gpc_type = NULL;
        
        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
        {
            HashNode_t *type_node;
            if (FindIdent(&type_node, symtab, subprogram->tree_data.subprogram_data.return_type_id) == -1)
            {
                fprintf(stderr, "Error on line %d, undefined type %s!\n",
                    subprogram->line_num, subprogram->tree_data.subprogram_data.return_type_id);
                return_val++;
            }
            else
            {
                return_type_node = type_node;
                /* Get GpcType for the return type */
                assert(type_node->type != NULL && "Type node must have GpcType");
                return_gpc_type = type_node->type;
            }
        }

        /* Create a primitive GpcType for the return type if we don't have one */
        if (return_gpc_type == NULL && subprogram->tree_data.subprogram_data.return_type != -1)
        {
            return_gpc_type = create_primitive_type(subprogram->tree_data.subprogram_data.return_type);
        }
        
        /* Add type metadata from return_type_node to return_gpc_type */
        if (return_gpc_type != NULL && return_type_node != NULL)
        {
            struct TypeAlias *type_alias = get_type_alias_from_node(return_type_node);
            if (type_alias != NULL)
            {
                gpc_type_set_type_alias(return_gpc_type, type_alias);
            }
            struct RecordType *record_type = get_record_type_from_node(return_type_node);
            if (record_type != NULL && return_gpc_type->kind == TYPE_KIND_RECORD)
            {
                return_gpc_type->info.record_info = clone_record_type(record_type);
            }
        }
        
        /* Create GpcType for the function (which is also a procedure type with a return value) */
        GpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_gpc_type  /* functions have a return type */
        );
        
        // Use the typed version to properly set the GpcType
        func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        func_type);

        /* Note: Type metadata now in GpcType, no post-creation writes needed */

        PushScope(symtab);
        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with GpcType
        // Always use _Typed variant, even if GpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, subprogram->tree_data.subprogram_data.id, return_gpc_type);

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
                    new_max_scope);

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
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
