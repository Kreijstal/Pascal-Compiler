/*
    Damon Gwinn
    Helper types for tree.h
*/

#ifndef TREE_TYPES_H
#define TREE_TYPES_H

#include "../List/List.h"

/* Enums for readability with types */
enum StmtType{STMT_VAR_ASSIGN, STMT_PROCEDURE_CALL, STMT_COMPOUND_STATEMENT,
    STMT_IF_THEN, STMT_WHILE, STMT_REPEAT, STMT_FOR, STMT_FOR_VAR, STMT_FOR_ASSIGN_VAR,
    STMT_BREAK, STMT_ASM_BLOCK};

enum TypeDeclKind { TYPE_DECL_RANGE, TYPE_DECL_RECORD, TYPE_DECL_ALIAS };

struct TypeAlias
{
    int base_type;
    char *target_type_id;
    int is_array;
    int array_start;
    int array_end;
    int array_element_type;
    char *array_element_type_id;
    int is_open_array;
};

struct RecordType;

struct RecordField
{
    char *name;
    int type;
    char *type_id;
    struct RecordType *nested_record;
};

struct RecordType
{
    ListNode_t *fields;
};

/* A statement subtree */
struct Statement
{
    int line_num;
    enum StmtType type;
    union stmt_data
    {
        /* Variable assignment */
        struct VarAssign
        {
            struct Expression *var;
            struct Expression *expr;
        } var_assign_data;

        /* Asm block */
        struct AsmBlock
        {
            char *code;
        } asm_block_data;

        /* Procedure call */
        struct ProcedureCall
        {
            char *id;
            char *mangled_id;
            ListNode_t *expr_args;
        struct HashNode *resolved_proc;
        } procedure_call_data;

        /* Compound Statements */
        ListNode_t *compound_statement;

        /* IF THEN ELSE */
        struct IfThenElse
        {
            struct Expression *relop_expr;
            struct Statement *if_stmt;
            struct Statement *else_stmt; /* NOTE: can be null */
        } if_then_data;

        /* WHILE */
        struct While
        {
            struct Expression *relop_expr;
            struct Statement *while_stmt;
        } while_data;

        /* REPEAT */
        struct Repeat
        {
            ListNode_t *body_list;
            struct Expression *until_expr;
        } repeat_data;

        /* FOR */
        struct For
        {
            enum StmtType for_assign_type;
            struct Expression *to;
            struct Statement *do_for;

            union for_assign
            {
                struct Statement *var_assign;
                struct Expression *var; /* Grammar will validate the correctness */
            } for_assign_data;
        } for_data;
    } stmt_data;
};

/* Expression types */
enum ExprType {
    EXPR_RELOP,
    EXPR_SIGN_TERM,
    EXPR_ADDOP,
    EXPR_MULOP,
    EXPR_VAR_ID,
    EXPR_ARRAY_ACCESS,
    EXPR_FUNCTION_CALL,
    EXPR_INUM,
    EXPR_RNUM,
    EXPR_STRING,
    EXPR_BOOL
};

/* An expression subtree */
struct Expression
{
    int line_num;
    enum ExprType type;
    union expr_data
    {
        /* Relational expression */
        struct Relop
        {
            int type;

            /* NOTE: right subtree will be NULL when type is NOT or PAREN */
            struct Expression *left;
            struct Expression *right;
        } relop_data;

        /* Sign term */
        struct Expression *sign_term;

        /* ADDOP */
        struct Addop
        {
            int addop_type;

            struct Expression *left_expr;
            struct Expression *right_term;
        } addop_data;

        /* MULOP */
        struct Mulop
        {
            int mulop_type;

            struct Expression *left_term;
            struct Expression *right_factor;
        } mulop_data;

        /* A variable ID */
        char *id;

        /* An indexed array */
        struct ArrayAccess
        {
            char *id;
            struct Expression *array_expr;
        } array_access_data;

        /* Function call */
        struct FunctionCall
        {
            char *id;
            char *mangled_id;
            ListNode_t *args_expr;
        struct HashNode *resolved_func;
        } function_call_data;

        /* Integer number */
        long long i_num;

        /* Real number */
        float r_num;

        /* String literal */
        char *string;

        /* Boolean literal */
        int bool_value;
    } expr_data;
    struct Expression *field_width;
    struct Expression *field_precision;
    int resolved_type;
};


#endif
