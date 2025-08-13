#include "stmt_gen.h"
#include <stdio.h>
#include "codegen.h"
#include "expr_gen.h"
#include "builtin.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../flags.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"
#include <assert.h>
#include <string.h>
#include "register_types.h"

#include "stmt_gen.h"
#include <stdio.h>
#include "codegen.h"
#include "expr_gen.h"
#include "builtin.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../flags.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"
#include <assert.h>
#include <string.h>
#include "register_types.h"

/* Codegen for a statement */
ListNode_t *codegen_stmt(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);

    ListNode_t *comp_list;

    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
            inst_list = codegen_var_assignment(stmt, inst_list, o_file);
            break;

        case STMT_PROCEDURE_CALL:
            inst_list = codegen_proc_call(stmt, inst_list, o_file);
            break;

        case STMT_COMPOUND_STATEMENT:
            inst_list = codegen_compound_stmt(stmt, inst_list, o_file);
            break;

        case STMT_IF_THEN:
            inst_list = codegen_if_then(stmt, inst_list, o_file);
            break;

        case STMT_WHILE:
            inst_list = codegen_while(stmt, inst_list, o_file);
            break;

        case STMT_FOR:
            inst_list = codegen_for(stmt, inst_list, o_file);
            break;

        case STMT_ASM_BLOCK:
            inst_list = add_inst(inst_list, stmt->stmt_data.asm_block_data.code);
            break;

        default:
            fprintf(stderr, "Critical error: Unrecognized statement type in codegen\n");
            exit(1);
    }

    return inst_list;
}

/* TODO: Only handles assignments and read/write builtins */
/* Returns a list of instructions */
ListNode_t *codegen_compound_stmt(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);
    assert(stmt->type == STMT_COMPOUND_STATEMENT);

    ListNode_t *stmt_list;
    struct Statement *cur_stmt;

    stmt_list = stmt->stmt_data.compound_statement;

    while(stmt_list != NULL)
    {
        cur_stmt = (struct Statement *)stmt_list->cur;

        inst_list = codegen_stmt(cur_stmt, inst_list, o_file);

        stmt_list = stmt_list->next;
    }

    return inst_list;
}

/* Code generation for a variable assignment */
/* TODO: Array assignments not currently supported */
ListNode_t *codegen_var_assignment(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);
    assert(stmt->type == STMT_VAR_ASSIGN);

    StackNode_t *var;
    Register_t *reg;
    char buffer[50];
    struct Expression *var_expr, *assign_expr;
    int offset;

    var_expr = stmt->stmt_data.var_assign_data.var;
    assign_expr = stmt->stmt_data.var_assign_data.expr;

    /* Getting stack address of variable to set */
    assert(var_expr->type == EXPR_VAR_ID);
    var = find_label(var_expr->expr_data.id);

    inst_list = codegen_expr(assign_expr, inst_list, o_file);

    reg = front_reg_stack(get_reg_stack());

    if(var != NULL)
    {
        snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", reg->bit_32, var->offset);
    }
    else if(nonlocal_flag() == 1)
    {
        inst_list = codegen_get_nonlocal(inst_list, var_expr->expr_data.id, &offset);
        snprintf(buffer, 50, "\tmovq\t%s, -%d(%s)\n", reg->bit_64, offset, NON_LOCAL_REG_64);
    }
    else
    {
        fprintf(stderr, "ERROR: Non-local codegen support disabled (buggy)!\n");
        fprintf(stderr, "Enable with flag '-non-local' after required flags\n");
        exit(1);
    }

    return add_inst(inst_list, buffer);
}

/* Code generation for a procedure call */
/* NOTE: This function will also recognize builtin procedures */
/* TODO: Currently only handles builtins */
/* TODO: Functions and procedures only handle max 2 arguments */
ListNode_t *codegen_proc_call(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);
    assert(stmt->type == STMT_PROCEDURE_CALL);

    char *proc_name;
    ListNode_t *args_expr;
    char buffer[50];

    proc_name = stmt->stmt_data.procedure_call_data.mangled_id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;

    char *unmangled_name = stmt->stmt_data.procedure_call_data.id;
    /* First check for builtins */
    if(strcmp("write", unmangled_name) == 0)
    {
        fprintf(stderr, "DEBUG: Generating code for write() builtin\n");
        inst_list = codegen_builtin_write(args_expr, inst_list, o_file, &write_label_counter);
        fprintf(stderr, "DEBUG: Finished generating code for write()\n");
    }
    else if(strcmp("writeln", unmangled_name) == 0 || strcmp("writeLn", unmangled_name) == 0)
    {
        fprintf(stderr, "DEBUG: Generating code for writeln() builtin\n");
        inst_list = codegen_builtin_writeln(args_expr, inst_list, o_file, &write_label_counter);
        fprintf(stderr, "DEBUG: Finished generating code for writeln()\n");
    }
    else if(strcmp("read", unmangled_name) == 0 || strcmp("readLn", unmangled_name) == 0)
    {
        fprintf(stderr, "DEBUG: Generating code for read() builtin\n");
        inst_list = codegen_builtin_read(args_expr, inst_list, o_file, &write_label_counter);
        fprintf(stderr, "DEBUG: Finished generating code for read()\n");
    }

    /* Not builtin */
    else
    {
        inst_list = codegen_pass_arguments(args_expr, inst_list, o_file);
        inst_list = codegen_vect_reg(inst_list, 0);
        snprintf(buffer, 50, "\tcall\t%s\n", proc_name);
        inst_list = add_inst(inst_list, buffer);
        free_arg_regs();
    }

    return inst_list;
}

/* Code generation for if-then-else statements */
/* TODO: Support more than simple relops */
ListNode_t *codegen_if_then(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);
    assert(stmt->type == STMT_IF_THEN);

    int relop_type, inverse;
    struct Expression *expr;
    struct Statement *if_stmt, *else_stmt;
    char label1[18], label2[18], buffer[50];

    /* Evaluating the relop */
    expr = stmt->stmt_data.if_then_data.relop_expr;
    inst_list = codegen_simple_relop(expr, inst_list, o_file, &relop_type);

    /* Preparing labels and data */
    gen_label(label1, 18);
    gen_label(label2, 18);
    if_stmt = stmt->stmt_data.if_then_data.if_stmt;
    else_stmt = stmt->stmt_data.if_then_data.else_stmt;

    /* IF STATEMENT */
    inverse = 1;
    inst_list = gencode_jmp(relop_type, inverse, label1, inst_list);
    inst_list = codegen_stmt(if_stmt, inst_list, o_file);

    /* ELSE STATEMENT (if applicable) */
    if(else_stmt == NULL)
    {
        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);
    }
    else
    {
        inverse = 0;
        inst_list = gencode_jmp(NORMAL_JMP, inverse, label2, inst_list);

        snprintf(buffer, 50, "%s:\n", label1);
        inst_list = add_inst(inst_list, buffer);

        inst_list = codegen_stmt(else_stmt, inst_list, o_file);

        snprintf(buffer, 50, "%s:\n", label2);
        inst_list = add_inst(inst_list, buffer);
    }

    return inst_list;
}

/* Code generation for while statements */
/* TODO: Support more than simple relops */
ListNode_t *codegen_while(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);
    assert(stmt->type == STMT_WHILE);

    int relop_type, inverse;
    struct Expression *expr;
    struct Statement *while_stmt;
    char label1[18], label2[18], buffer[50];

    /* Preparing labels and data */
    gen_label(label1, 18);
    gen_label(label2, 18);
    while_stmt = stmt->stmt_data.while_data.while_stmt;
    expr = stmt->stmt_data.while_data.relop_expr;

    /* First jmp to comparison area */
    inverse = 0;
    inst_list = gencode_jmp(NORMAL_JMP, inverse, label1, inst_list);

    /* WHILE STMT */
    snprintf(buffer, 50, "%s:\n", label2);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_stmt(while_stmt, inst_list, o_file);

    /* Comparison area */
    snprintf(buffer, 50, "%s:\n", label1);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_simple_relop(expr, inst_list, o_file, &relop_type);

    inverse = 0;
    inst_list = gencode_jmp(relop_type, inverse, label2, inst_list);

    return inst_list;
}

/* Code generation for for statements */
/* TODO: Support more than simple relops */
ListNode_t *codegen_for(struct Statement *stmt, ListNode_t *inst_list, FILE *o_file)
{
    assert(stmt != NULL);
    assert(stmt->type == STMT_FOR);

    int relop_type, inverse;
    struct Expression *expr, *for_var, *comparison_expr, *update_expr, *one_expr;
    struct Statement *for_body, *for_assign, *update_stmt;
    char label1[18], label2[18], buffer[50];

    /* Preparing labels and data */
    gen_label(label1, 18);
    gen_label(label2, 18);
    for_body = stmt->stmt_data.for_data.do_for;
    expr = stmt->stmt_data.for_data.to;

    /* First do for variable assignment (if applicable) */
    if(stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
    {
        for_assign = stmt->stmt_data.for_data.for_assign_data.var_assign;
        inst_list = codegen_var_assignment(for_assign, inst_list, o_file);
        for_var = stmt->stmt_data.for_data.for_assign_data.var_assign->stmt_data.var_assign_data.var;
    }
    else
    {
        for_var = stmt->stmt_data.for_data.for_assign_data.var;
    }

    assert(for_var->type == EXPR_VAR_ID);
    comparison_expr = mk_relop(-1, LT, for_var, expr);
    one_expr = mk_inum(-1, 1);
    update_expr = mk_addop(-1, PLUS, for_var, one_expr);
    update_stmt = mk_varassign(-1, for_var, update_expr);

    /* First jmp to comparison area */
    inverse = 0;
    inst_list = gencode_jmp(NORMAL_JMP, inverse, label1, inst_list);

    /* FOR STMT */
    snprintf(buffer, 50, "%s:\n", label2);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_stmt(for_body, inst_list, o_file);

    /* UPDATE */
    inst_list = codegen_stmt(update_stmt, inst_list, o_file);

    /* Comparison area */
    snprintf(buffer, 50, "%s:\n", label1);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_simple_relop(comparison_expr, inst_list, o_file, &relop_type);

    inverse = 0;
    inst_list = gencode_jmp(relop_type, inverse, label2, inst_list);

    free(comparison_expr);
    free(one_expr);
    free(update_expr);
    free(update_stmt);
    return inst_list;
}
