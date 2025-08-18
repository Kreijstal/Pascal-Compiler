#ifndef IR_H
#define IR_H

#include "Parser/ParseTree/tree.h"

typedef enum {
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_MOD,
    IR_LOAD_CONST,
    IR_LOAD_STRING,
    IR_LOAD_VAR,
    IR_STORE_VAR,
    IR_JUMP,
    IR_JUMP_IF_ZERO,
    IR_JUMP_IF_NOT_ZERO,
    IR_JUMP_IF_TRUE,
    IR_LABEL,
    IR_CALL,
    IR_ARG,
    IR_RETURN,
    IR_RETRIEVE_RETURN_VAL,
    IR_CMP,
    IR_READ_INT
} IROpcode;

struct Register;

typedef struct {
    char *name;
    int is_global;
    struct Register *reg;
} IRValue;

typedef struct {
    IROpcode opcode;
    IRValue *dest;
    IRValue *src1;
    IRValue *src2;
    char *label;
    char *proc_name;
    ListNode_t *args;
    int relop_type;
    int arg_type;
    int newline;
} IRInstruction;

#endif // IR_H
