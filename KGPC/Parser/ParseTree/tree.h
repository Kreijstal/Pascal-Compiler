/*
    Damon Gwinn
    Parse tree for the Pascal Grammar
*/
#ifndef TREE_H
#define TREE_H

#define LEAF NULL

#include "../List/List.h"
#include "tree_types.h"

struct KgpcType;
#include <stdio.h>

/******* Trees and statement types ********/

/* Types */
typedef struct Tree Tree_t;

/* Enum for readability */
enum TreeType{TREE_PROGRAM_TYPE, TREE_SUBPROGRAM, TREE_VAR_DECL, TREE_ARR_DECL,
    TREE_CONST_DECL, TREE_STATEMENT_TYPE, TREE_SUBPROGRAM_PROC, TREE_SUBPROGRAM_FUNC,
    TREE_TYPE_DECL, TREE_UNIT};

typedef struct Tree
{
    int line_num;
    int type;
    union tree_data
    {
        /* Program Variables */
        struct Program
        {
            char *program_id;

            ListNode_t *args_char;
            ListNode_t *uses_units;
            ListNode_t *label_declaration;
            ListNode_t *const_declaration;
            ListNode_t *var_declaration;
            ListNode_t *type_declaration;
            ListNode_t *subprograms;
            struct Statement *body_statement;
            ListNode_t *finalization_statements; /* List of Statement* from units, stored in reverse order */
        } program_data;

        /* Pascal unit */
        struct Unit
        {
            char *unit_id;
            ListNode_t *interface_uses;
            ListNode_t *interface_const_decls;
            ListNode_t *interface_type_decls;
            ListNode_t *interface_var_decls;
            ListNode_t *implementation_uses;
            ListNode_t *implementation_const_decls;
            ListNode_t *implementation_type_decls;
            ListNode_t *implementation_var_decls;
            ListNode_t *subprograms;
            struct Statement *initialization;
            struct Statement *finalization;
        } unit_data;

        /* A type declaration */
        struct TypeDecl
        {
            char *id;
            enum TypeDeclKind kind;
            struct KgpcType *kgpc_type;
            int defined_in_unit;
            int unit_is_public;
            union
            {
                struct
                {
                    int start;
                    int end;
                } range;
                struct RecordType *record;
                struct TypeAlias alias;
                struct GenericDecl generic;
            } info;
        } type_decl_data;

        /* A subprogram */
        struct Subprogram
        {
            /* FUNCTION or PROCEDURE */
            enum TreeType sub_type;
            char *id;
            char *mangled_id;
            ListNode_t *args_var;
            ListNode_t *const_declarations;
            ListNode_t *label_declarations;
            ListNode_t *type_declarations;
            int return_type; /* Should be -1 for PROCEDURE */
            char *return_type_id;
            struct TypeAlias *inline_return_type;  /* For inline complex return types like array of string */
            int cname_flag;
            char *cname_override;
            int overload_flag;
            int nesting_level; /* Lexical nesting depth: 0 = top-level, 1 = nested in program, etc. */
            int requires_static_link;
            int defined_in_unit;
            int unit_is_public; /* 1 if declared in interface section, 0 if implementation only */

            ListNode_t *declarations;
            ListNode_t *subprograms;
            struct Statement *statement_list;
            int is_used;
        } subprogram_data;

        /* A variable declaration */
        /* Also used for variable arguments */
        struct Var
        {
            ListNode_t *ids;
            int type; /* Int, or real */
            char *type_id;
            int is_var_param;
            int inferred_type;
            struct Statement *initializer;
            int is_typed_const;
            int currency_scaled;
            struct RecordType *inline_record_type;  /* For inline record declarations */
            struct TypeAlias *inline_type_alias;   /* For inline complex aliases (file of T, etc.) */
            struct KgpcType *cached_kgpc_type;   /* Retained type info for codegen fallback */
            int defined_in_unit;
            int unit_is_public;
            char *cname_override;    /* External/public name alias (FPC bootstrap) */
            int is_external;         /* True if declared with 'external name' */
            char *absolute_target;   /* Absolute alias target name, if any */
        } var_decl_data;

        /* An array declaration */
        /* Also used for array arguments */
        struct Array
        {
            ListNode_t *ids;
            int type; /* Int, or real */
            char *type_id;

            int s_range;
            int e_range;
            char *range_str;  /* Original range string (e.g., "1..N") for constant resolution */
            struct Statement *initializer;
            int is_typed_const;
            int is_shortstring;
            int has_static_storage;
            int static_storage_emitted;
            char *static_label;
            char *init_guard_label;
            int defined_in_unit;
            int unit_is_public;
        } arr_decl_data;

        /* A constant declaration */
        struct Const
        {
            char *id;
            char *type_id;
            struct Expression *value;
            int defined_in_unit;
            int unit_is_public;
        } const_decl_data;

        /* A single statement (Can be made up of multiple statements) */
        /* See "tree_types.h" for details */
        struct Statement *statement_data;

    } tree_data;
} Tree_t;

/* GLOBAL TREE */
extern Tree_t *parse_tree;

/* WARNING: Copies are NOT made. Make sure given pointers are safe! */
/* WARNING: Destroying the tree WILL free given pointers. Do not reference after free! */

/* NOTE: tree_print and destroy_tree implicitely call stmt and expr functions */
/* Tree printing */
void list_print(ListNode_t *list, FILE *f, int num_indent);
void tree_print(Tree_t *tree, FILE *f, int num_indent);
void stmt_print(struct Statement *stmt, FILE *f, int num_indent);
void expr_print(struct Expression *expr, FILE *f, int num_indent);

/* Tree freeing */
/* WARNING: Also frees all c strings and other such types */
void destroy_list(ListNode_t *list);
void destroy_tree(Tree_t *tree);
void destroy_stmt(struct Statement *stmt);
void destroy_expr(struct Expression *expr);
void destroy_record_type(struct RecordType *record_type);
struct RecordType *clone_record_type(const struct RecordType *record_type);

/* Tree routines */
Tree_t *mk_program(int line_num, char *id, ListNode_t *args, ListNode_t *uses,
    ListNode_t *labels, ListNode_t *const_decl, ListNode_t *var_decl, ListNode_t *type_decl,
    ListNode_t *subprograms, struct Statement *compound_statement);

Tree_t *mk_unit(int line_num, char *id, ListNode_t *interface_uses,
    ListNode_t *interface_const_decls, ListNode_t *interface_type_decls,
    ListNode_t *interface_var_decls, ListNode_t *implementation_uses,
    ListNode_t *implementation_const_decls,
    ListNode_t *implementation_type_decls,
    ListNode_t *implementation_var_decls, ListNode_t *subprograms,
    struct Statement *initialization, struct Statement *finalization);

Tree_t *mk_typedecl(int line_num, char *id, int start, int end);
Tree_t *mk_typealiasdecl(int line_num, char *id, int is_array, int actual_type, char *type_id, int start, int end);
Tree_t *mk_record_type(int line_num, char *id, struct RecordType *record_type);

Tree_t *mk_procedure(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *label_decl, ListNode_t *type_decl, ListNode_t *var_decl,
    ListNode_t *subprograms, struct Statement *compound_statement,
    int cname_flag, int overload_flag);

Tree_t *mk_function(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *label_decl, ListNode_t *type_decl, ListNode_t *var_decl,
    ListNode_t *subprograms, struct Statement *compound_statement,
    int return_type, char *return_type_id, struct TypeAlias *inline_return_type, int cname_flag, int overload_flag);

Tree_t *mk_vardecl(int line_num, ListNode_t *ids, int type, char *type_id,
    int is_var_param, int inferred_type, struct Statement *initializer,
    struct RecordType *inline_record_type, struct TypeAlias *inline_type_alias,
    char *absolute_target);

Tree_t *mk_arraydecl(int line_num, ListNode_t *ids, int type, char *type_id, int start, int end,
    char *range_str, struct Statement *initializer);

Tree_t *mk_constdecl(int line_num, char *id, char *type_id, struct Expression *value);

/* Statement routines */
struct Statement *mk_varassign(int line_num, int col_num, struct Expression *var, struct Expression *expr);
struct Statement *mk_label(int line_num, char *label, struct Statement *stmt);
struct Statement *mk_goto(int line_num, char *label);

struct Statement *mk_procedurecall(int line_num, char *id, ListNode_t *expr_args);

struct Statement *mk_compoundstatement(int line_num, ListNode_t *compound_statement);

struct Statement *mk_ifthen(int line_num, struct Expression *eval_relop, struct Statement *if_stmt,
                            struct Statement *else_stmt);

struct Statement *mk_while(int line_num, struct Expression *eval_relop,
                            struct Statement *while_stmt);

struct Statement *mk_repeat(int line_num, ListNode_t *body_list,
                            struct Expression *until_expr);

struct Statement *mk_forassign(int line_num, struct Statement *for_assign, struct Expression *to,
                               struct Statement *do_for, int is_downto);

struct Statement *mk_forvar(int line_num, struct Expression *for_var, struct Expression *to,
                              struct Statement *do_for, int is_downto);

struct Statement *mk_for_in(int line_num, struct Expression *loop_var, struct Expression *collection,
                             struct Statement *do_stmt);

struct Statement *mk_asmblock(int line_num, char *code);

struct Statement *mk_exit(int line_num);
struct Statement *mk_exit_with_value(int line_num, struct Expression *return_expr);

struct Statement *mk_break(int line_num);
struct Statement *mk_continue(int line_num);

struct Statement *mk_case(int line_num, struct Expression *selector, ListNode_t *branches, struct Statement *else_stmt);

struct Statement *mk_with(int line_num, struct Expression *context, struct Statement *body);

struct Statement *mk_tryfinally(int line_num, ListNode_t *try_stmts, ListNode_t *finally_stmts);

struct Statement *mk_tryexcept(int line_num, ListNode_t *try_stmts, ListNode_t *except_stmts,
                               char *exception_var_name, char *exception_type_name);

struct Statement *mk_raise(int line_num, struct Expression *expr);

struct Statement *mk_inherited(int line_num, struct Expression *expr);

/* Expression routines */
struct Expression *mk_relop(int line_num, int type, struct Expression *left,
                                struct Expression *right);

struct Expression *mk_signterm(int line_num, struct Expression *sign_term);

struct Expression *mk_addop(int line_num, int type, struct Expression *left, struct Expression *right);

struct Expression *mk_mulop(int line_num, int type, struct Expression *left, struct Expression *right);

struct Expression *mk_varid(int line_num, char *id);

struct Expression *mk_arrayaccess(int line_num, struct Expression *array_expr, struct Expression *index_expr);

struct Expression *mk_recordaccess(int line_num, struct Expression *record_expr, char *field_id);

struct Expression *mk_pointer_deref(int line_num, struct Expression *pointer_expr);

struct Expression *mk_array_literal(int line_num, ListNode_t *elements, int element_count);

struct Expression *mk_addressof(int line_num, struct Expression *expr);

struct Expression *mk_functioncall(int line_num, char *id, ListNode_t *args);

struct Expression *mk_inum(int line_num, long long i_num);
struct Expression *mk_charcode(int line_num, unsigned int char_code);
struct Expression *mk_rnum(int line_num, float r_num);

struct Expression *mk_string(int line_num, char *string);

struct Expression *mk_bool(int line_num, int value);

struct Expression *mk_nil(int line_num);

struct SetElement *mk_set_element(struct Expression *lower, struct Expression *upper);
void destroy_set_element(struct SetElement *element);
struct Expression *mk_set(int line_num, unsigned int bitmask, ListNode_t *elements, int is_constant);
struct Expression *mk_record_constructor(int line_num, ListNode_t *fields, int field_count);

struct Expression *mk_typecast(int line_num, int target_type, char *target_type_id,
    struct Expression *expr);

struct Expression *mk_is(int line_num, struct Expression *expr,
    int target_type, char *target_type_id);

struct Expression *mk_as(int line_num, struct Expression *expr,
    int target_type, char *target_type_id);

struct Expression *mk_anonymous_function(int line_num, char *generated_name, 
    ListNode_t *parameters, int return_type, char *return_type_id, struct Statement *body);

struct Expression *mk_anonymous_procedure(int line_num, char *generated_name,
    ListNode_t *parameters, struct Statement *body);


#endif
