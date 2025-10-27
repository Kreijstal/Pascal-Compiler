/*
    Damon Gwinn
    Parse tree for the Pascal Grammar
*/
#ifndef TREE_H
#define TREE_H

#define LEAF NULL

#include "../List/List.h"
#include "tree_types.h"
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
            ListNode_t *const_declaration;
            ListNode_t *var_declaration;
            ListNode_t *type_declaration;
            ListNode_t *subprograms;
            struct Statement *body_statement;
        } program_data;

        /* Pascal unit */
        struct Unit
        {
            char *unit_id;
            ListNode_t *interface_uses;
            ListNode_t *interface_type_decls;
            ListNode_t *interface_var_decls;
            ListNode_t *implementation_uses;
            ListNode_t *implementation_type_decls;
            ListNode_t *implementation_var_decls;
            ListNode_t *subprograms;
            struct Statement *initialization;
        } unit_data;

        /* A type declaration */
        struct TypeDecl
        {
            char *id;
            enum TypeDeclKind kind;
            union
            {
                struct
                {
                    int start;
                    int end;
                } range;
                struct RecordType *record;
            } info;
        } type_decl_data;

        /* A subprogram */
        struct Subprogram
        {
            /* FUNCTION or PROCEDURE */
            enum TreeType sub_type;
            char *id;
            char *mangled_id; // <-- ADD THIS LINE
            ListNode_t *args_var;
            ListNode_t *const_declarations;
            int return_type; /* Should be -1 for PROCEDURE */
            char *return_type_id;
            int cname_flag;
            int overload_flag;

            ListNode_t *declarations;
            ListNode_t *subprograms;
            struct Statement *statement_list;
            int has_pascal_body;
        } subprogram_data;

        /* A variable declaration */
        /* Also used for variable arguments */
        struct Var
        {
            ListNode_t *ids;
            int type; /* Int, or real */
            char *type_id;
            int is_var_param;
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
        } arr_decl_data;

        /* A constant declaration */
        struct Const
        {
            char *id;
            char *type_id;
            struct Expression *value;
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
    ListNode_t *const_decl, ListNode_t *var_decl, ListNode_t *type_decl,
    ListNode_t *subprograms, struct Statement *compound_statement);

Tree_t *mk_unit(int line_num, char *id, ListNode_t *interface_uses,
    ListNode_t *interface_type_decls, ListNode_t *interface_var_decls,
    ListNode_t *implementation_uses, ListNode_t *implementation_type_decls,
    ListNode_t *implementation_var_decls, ListNode_t *subprograms,
    struct Statement *initialization);

Tree_t *mk_typedecl(int line_num, char *id, int start, int end);
Tree_t *mk_record_type(int line_num, char *id, struct RecordType *record_type);

Tree_t *mk_procedure(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *var_decl, ListNode_t *subprograms, struct Statement *compound_statement,
    int cname_flag, int overload_flag);

Tree_t *mk_function(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *var_decl, ListNode_t *subprograms, struct Statement *compound_statement,
    int return_type, char *return_type_id, int cname_flag, int overload_flag);

Tree_t *mk_vardecl(int line_num, ListNode_t *ids, int type, char *type_id, int is_var_param);

Tree_t *mk_arraydecl(int line_num, ListNode_t *ids, int type, char *type_id, int start, int end);

Tree_t *mk_constdecl(int line_num, char *id, char *type_id, struct Expression *value);

/* Statement routines */
struct Statement *mk_varassign(int line_num, struct Expression *var, struct Expression *expr);

struct Statement *mk_procedurecall(int line_num, char *id, ListNode_t *expr_args);

struct Statement *mk_compoundstatement(int line_num, ListNode_t *compound_statement);

struct Statement *mk_ifthen(int line_num, struct Expression *eval_relop, struct Statement *if_stmt,
                            struct Statement *else_stmt);

struct Statement *mk_while(int line_num, struct Expression *eval_relop,
                            struct Statement *while_stmt);

struct Statement *mk_forassign(int line_num, struct Statement *for_assign, struct Expression *to,
                               struct Statement *do_for);

struct Statement *mk_forvar(int line_num, struct Expression *for_var, struct Expression *to,
                              struct Statement *do_for);

struct Statement *mk_asmblock(int line_num, char *code);

/* Expression routines */
struct Expression *mk_relop(int line_num, int type, struct Expression *left,
                                struct Expression *right);

struct Expression *mk_signterm(int line_num, struct Expression *sign_term);

struct Expression *mk_addop(int line_num, int type, struct Expression *left, struct Expression *right);

struct Expression *mk_mulop(int line_num, int type, struct Expression *left, struct Expression *right);

struct Expression *mk_varid(int line_num, char *id);

struct Expression *mk_arrayaccess(int line_num, char *id, struct Expression *index_expr);

struct Expression *mk_functioncall(int line_num, char *id, ListNode_t *args);

struct Expression *mk_inum(int line_num, int i_num);
struct Expression *mk_rnum(int line_num, float r_num);

struct Expression *mk_string(int line_num, char *string);


#endif
