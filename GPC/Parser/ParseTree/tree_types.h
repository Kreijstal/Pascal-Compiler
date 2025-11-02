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
    STMT_ASM_BLOCK, STMT_EXIT, STMT_BREAK, STMT_CASE, STMT_WITH, STMT_TRY_FINALLY,
    STMT_TRY_EXCEPT, STMT_RAISE, STMT_INHERITED};

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
    ListNode_t *array_dimensions;
    int is_pointer;
    int pointer_type;
    char *pointer_type_id;
    int is_set;
    int set_element_type;
    char *set_element_type_id;
    int is_enum;
    ListNode_t *enum_literals;
    int is_file;
    int file_type;
    char *file_type_id;
};

struct RecordType;

struct RecordField
{
    char *name;
    int type;
    char *type_id;
    struct RecordType *nested_record;
    int is_array;
    int array_start;
    int array_end;
    int array_element_type;
    char *array_element_type_id;
    int array_is_open;
};

struct RecordType
{
    ListNode_t *fields;
};

struct VariantBranch
{
    ListNode_t *labels; /* List of Expression */
    ListNode_t *members; /* List of RecordField / VariantPart */
};

struct VariantPart
{
    struct RecordField *tag_field; /* Optional discriminant field */
    int tag_type;
    char *tag_type_id;
    struct RecordType *tag_record;
    ListNode_t *branches; /* List of VariantBranch */
    int has_cached_size;
    long long cached_size;
};

/* Case branch structure */
struct CaseBranch
{
    ListNode_t *labels;  /* List of expressions or range elements representing case labels */
    struct Statement *stmt;
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

        /* CASE */
        struct Case
        {
            struct Expression *selector_expr;
            ListNode_t *branches;  /* List of CaseBranch */
            struct Statement *else_stmt;  /* Optional else branch */
        } case_data;

        /* WITH */
        struct With
        {
            struct Expression *context_expr;
            struct Statement *body_stmt;
        } with_data;

        /* TRY..FINALLY */
        struct TryFinally
        {
            ListNode_t *try_statements;   /* List of Statement */
            ListNode_t *finally_statements; /* List of Statement */
        } try_finally_data;

        /* TRY..EXCEPT */
        struct TryExcept
        {
            ListNode_t *try_statements;   /* List of Statement */
            ListNode_t *except_statements; /* List of Statement */
        } try_except_data;

        /* RAISE */
        struct Raise
        {
            struct Expression *exception_expr; /* Optional */
        } raise_data;

        /* INHERITED */
        struct Inherited
        {
            struct Expression *call_expr; /* Optional */
        } inherited_data;
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
    EXPR_RECORD_ACCESS,
    EXPR_FUNCTION_CALL,
    EXPR_INUM,
    EXPR_RNUM,
    EXPR_STRING,
    EXPR_CHAR_CODE,
    EXPR_BOOL,
    EXPR_NIL,
    EXPR_SET,
    EXPR_POINTER_DEREF,
    EXPR_ADDR,
    EXPR_TYPECAST,
    EXPR_ADDR_OF_PROC
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
            struct Expression *array_expr;
            struct Expression *index_expr;
        } array_access_data;

        /* Record field access */
        struct RecordAccess
        {
            struct Expression *record_expr;
            char *field_id;
            long long field_offset;
        } record_access_data;

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

        /* Character code literal */
        unsigned int char_code;

        /* Boolean literal */
        int bool_value;

        /* Set literal represented as a bitmask */
        struct SetLiteral
        {
            unsigned int bitmask;
            ListNode_t *elements;
            int is_constant;
        } set_data;

        /* Pointer dereference */
        struct PointerDeref
        {
            struct Expression *pointer_expr;
        } pointer_deref_data;

        /* Address-of operator */
        struct AddressOf
        {
            struct Expression *expr;
        } addr_data;

        /* Type cast */
        struct TypeCast
        {
            int target_type;
            char *target_type_id;
            struct Expression *expr;
        } typecast_data;

        /* Address of procedure */
        struct AddrOfProc
        {
            struct HashNode *procedure_symbol;
        } addr_of_proc_data;
    } expr_data;
    struct Expression *field_width;
    struct Expression *field_precision;
    int resolved_type;
    int pointer_subtype;
    char *pointer_subtype_id;
    struct RecordType *record_type;
    int is_array_expr;
    int array_element_type;
    char *array_element_type_id;
    int array_lower_bound;
    int array_upper_bound;
    int array_element_size;
    int array_is_dynamic;
    struct RecordType *array_element_record_type;
};

struct SetElement
{
    struct Expression *lower;
    struct Expression *upper;
};


#endif
