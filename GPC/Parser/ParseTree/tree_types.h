#ifndef TREE_TYPES_H
#define TREE_TYPES_H

#include "List/List.h"

// For %union in Grammar.y
typedef struct IdBisonUnion {
    char *id;
    int line_num;
    int cname_flag;
    int overload_flag;
} IdBisonUnion;

// For type declarations
typedef struct Type {
    int base_type;
    int array_start;
    int array_end;
    char* id; // For custom types
} Type_t;

typedef struct TypeDecl {
    char *id;
    Type_t *type;
    struct TypeDecl *next;
} TypeDecl_t;

// For variable declarations
typedef struct VarDecl {
    ListNode_t *id_list;
    Type_t *type;
    struct VarDecl *next;
} VarDecl_t;

// For function/procedure parameters
typedef struct Param {
    ListNode_t *id_list;
    int type;
    int pass_by_ref;
} Param_t;


// Old AST definitions (to be removed)
// These are here to allow the old code to compile for now.
// I will remove them once the refactoring is complete.
struct Statement;
struct Expression;
typedef struct Tree {
    int line_num;
    int type;
    union {
        struct {
            char *program_id;
            ListNode_t *args_char;
            ListNode_t *var_declaration;
            ListNode_t *type_declaration;
            ListNode_t *subprograms;
            struct Statement *body_statement;
        } program_data;
        struct {
            int sub_type;
            char *id;
            char *mangled_id;
            ListNode_t *args_var;
            int return_type;
            char *return_type_id;
            int cname_flag;
            int overload_flag;
            ListNode_t *declarations;
            ListNode_t *subprograms;
            struct Statement *statement_list;
        } subprogram_data;
        struct {
            ListNode_t *ids;
            int type;
            char *type_id;
            int is_var_param;
        } var_decl_data;
        struct {
            ListNode_t *ids;
            int type;
            int s_range;
            int e_range;
        } arr_decl_data;
        struct {
            char *id;
            int start;
            int end;
        } type_decl_data;
    } tree_data;
} Tree_t;


#endif // TREE_TYPES_H
