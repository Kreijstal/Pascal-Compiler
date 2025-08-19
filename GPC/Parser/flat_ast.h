#ifndef FLAT_AST_H
#define FLAT_AST_H

#include "List/List.h"
#include "tree_types.h" // For VarDecl_t, Param_t

typedef enum {
    FL_PROGRAM,
    FL_PROCEDURE,
    FL_FUNCTION,
    FL_COMPOUND_STATEMENT,
    FL_WHILE_LOOP,
    FL_FOR_LOOP,
    FL_IF_THEN,
    FL_VAR_ASSIGN,
    FL_PROCEDURE_CALL,
    FL_VAR_ID,
    FL_ARRAY_ACCESS,
    FL_FUNCTION_CALL,
    FL_RELOP,
    FL_ADDOP,
    FL_MULOP,
    FL_INUM,
    FL_RNUM,
    FL_STRING,
    FL_UNOP,
    FL_ASM_BLOCK
} FlatNodeType;

typedef enum {
    OP_ADD, OP_SUB, OP_OR,
    OP_MUL, OP_DIV, OP_MOD, OP_AND,
    OP_EQ, OP_NE, OP_LT, OP_GT, OP_LE, OP_GE,
    OP_U_MINUS, OP_NOT
} OpType;

typedef struct FlatNode FlatNode;

typedef struct Program {
    char *id;
    ListNode_t *args;
    VarDecl_t *declarations;
    ListNode_t *subprograms; // list of FlatNode*
    FlatNode *compound_statement;
} Program;

typedef struct Procedure {
    char *id;
    ListNode_t *params; // list of Param_t
    VarDecl_t *local_vars;
    ListNode_t *subprograms; // list of FlatNode* (functions/procedures)
    FlatNode *compound_statement;
    int cname_flag;
    int overload_flag;
} Procedure;

typedef struct Function {
    char *id;
    ListNode_t *params;
    VarDecl_t *local_vars;
    ListNode_t *subprograms;
    FlatNode *compound_statement;
    int return_type;
    char *return_type_id;
    int cname_flag;
    int overload_flag;
} Function;

typedef struct CompoundStatement {
    ListNode_t *stmt_list;
} CompoundStatement;

typedef struct WhileLoop {
    FlatNode *condition;
    FlatNode *while_stmt;
} WhileLoop;

typedef struct ForLoop {
    FlatNode *for_assign;
    FlatNode *to_expr;
    FlatNode *step_expr; // Can be NULL
    FlatNode *for_stmt;
} ForLoop;

typedef struct IfThen {
    FlatNode *condition;
    FlatNode *then_stmt;
    FlatNode *else_stmt; // Can be NULL
} IfThen;

typedef struct VarAssign {
    FlatNode *var;
    FlatNode *expr;
} VarAssign;

typedef struct ProcedureCall {
    char *id;
    ListNode_t *args; // list of FlatNode*
} ProcedureCall;

typedef struct VarId {
    char *id;
} VarId;

typedef struct ArrayAccess {
    char *id;
    FlatNode *index_expr;
} ArrayAccess;

typedef struct FunctionCall {
    char *id;
    ListNode_t *args; // list of FlatNode*
} FunctionCall;

typedef struct BinOp {
    OpType op;
    FlatNode *left;
    FlatNode *right;
} BinOp;

typedef struct UnOp {
    OpType op;
    FlatNode *operand;
} UnOp;

typedef struct AsmBlock {
    char *code;
} AsmBlock;

struct FlatNode {
    FlatNodeType node_type;
    int line_num;
    union {
        Program program;
        Procedure procedure;
        Function function;
        CompoundStatement compound_statement;
        WhileLoop while_loop;
        ForLoop for_loop;
        IfThen if_then;
        VarAssign var_assign;
        ProcedureCall procedure_call;
        VarId var_id;
        ArrayAccess array_access;
        FunctionCall function_call;
        BinOp bin_op;
        UnOp un_op;
        AsmBlock asm_block;
        int inum;
        float rnum;
        char *string;
    } data;
};

FlatNode *new_flat_node(int line_num, FlatNodeType type);

#endif // FLAT_AST_H
