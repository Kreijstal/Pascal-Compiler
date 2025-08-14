/*
    Damon Gwinn
    Code generation for expressions
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "codegen_expression.h"
#include "register_types.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"


/* Code generation for expressions */
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx)
{
    assert(expr != NULL);
    expr_node_t *expr_tree = NULL;

    fprintf(stderr, "DEBUG: Generating code for expression type %d\n", expr->type);

    switch(expr->type) {
        case EXPR_VAR_ID:
            fprintf(stderr, "DEBUG: Processing variable ID expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_INUM:
            fprintf(stderr, "DEBUG: Processing integer constant expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_RELOP:
            fprintf(stderr, "DEBUG: Processing relational operator expression\n");
            return codegen_simple_relop(expr, inst_list, ctx, NULL);
        case EXPR_ADDOP:
            fprintf(stderr, "DEBUG: Processing addop expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_SIGN_TERM:
            fprintf(stderr, "DEBUG: Processing sign term expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);
            return inst_list;
        default:
            fprintf(stderr, "ERROR: Unsupported expression type %d\n", expr->type);
            exit(1);
    }
}

/* Code generation for write() builtin - handles multiple args in single printf */
ListNode_t *codegen_builtin_write(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    char *buffer;
    int curr_label = ctx->write_label_counter++;
    ListNode_t *cur_arg = args;
    int arg_count = 0;
    char format_str[200] = "";

    fprintf(stderr, "DEBUG: Generating write with %d args\n", args ? ListLength(args) : 0);
    fprintf(stderr, "DEBUG: Format string before processing: '%s'\n", format_str);

    /* First pass: build format string and generate code for args */
    while(cur_arg != NULL) {
        struct Expression *expr = (struct Expression *)cur_arg->cur;

        if(expr->type == EXPR_STRING) {
            /* Handle string arguments by adding %s to format */
            strcat(format_str, "%s");

            /* Generate code to push string address */
            int len = snprintf(NULL, 0, "\tleaq\t.LC%d(%%rip), %%rax\n", curr_label);
            buffer = malloc(len + 1);
            snprintf(buffer, len + 1, "\tleaq\t.LC%d(%%rip), %%rax\n", curr_label);
            inst_list = add_inst(inst_list, buffer);
            free(buffer);

            len = snprintf(NULL, 0, "\tpushq\t%%rax\n");
            buffer = malloc(len + 1);
            snprintf(buffer, len + 1, "\tpushq\t%%rax\n");
            inst_list = add_inst(inst_list, buffer);
            free(buffer);
            arg_count++;

            /* Add string to .rodata */
            len = snprintf(NULL, 0, "\t.section\t.rodata\n.LC%d:\n\t.string \"%s\"\n\t.text\n",
                    curr_label++, expr->expr_data.string);
            buffer = malloc(len + 1);
            snprintf(buffer, len + 1, "\t.section\t.rodata\n.LC%d:\n\t.string \"%s\"\n\t.text\n",
                    curr_label-1, expr->expr_data.string);
            inst_list = add_inst(inst_list, buffer);
            free(buffer);
        }
        else if(expr->type == EXPR_INUM) {
            /* Handle all integers with %d */
            strcat(format_str, "%d");

            /* Generate code to evaluate expression */
            expr_node_t *expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);

            /* Save register to stack to preserve for printf */
            Register_t *top_reg = front_reg_stack(get_reg_stack());
            int len = snprintf(NULL, 0, "\tpushq\t%%%s\n", top_reg->bit_64+1); // Skip first % in register name
            buffer = malloc(len + 1);
            snprintf(buffer, len + 1, "\tpushq\t%%%s\n", top_reg->bit_64+1);
            inst_list = add_inst(inst_list, buffer);
            free(buffer);
            arg_count++;
        }
        else // Default to expression evaluation
        {
            strcat(format_str, "%d");
            expr_node_t *expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
            free_expr_tree(expr_tree);

            Register_t *top_reg = front_reg_stack(get_reg_stack());
            int len = snprintf(NULL, 0, "\tpushq\t%%%s\n", top_reg->bit_64+1);
            buffer = malloc(len + 1);
            snprintf(buffer, len + 1, "\tpushq\t%%%s\n", top_reg->bit_64+1);
            inst_list = add_inst(inst_list, buffer);
            free(buffer);
            arg_count++;
        }

        cur_arg = cur_arg->next;
    }

    /* Add format string to .rodata */
    /* Ensure format string ends with newline and is properly escaped */
    char escaped_str[200] = "";
    for(int i = 0; format_str[i]; i++) {
        if(format_str[i] == '\n') {
            strcat(escaped_str, "\\n");
        } else if(format_str[i] == '\"') {
            strcat(escaped_str, "\\\"");
        } else {
            strncat(escaped_str, &format_str[i], 1);
        }
    }
    if (escaped_str[strlen(escaped_str)-1] != '\n') {
        fprintf(stderr, "DEBUG: Adding newline to format string\n");
        strcat(escaped_str, "\\n");
    } else {
        fprintf(stderr, "DEBUG: Format string already ends with newline\n");
    }

    int len = snprintf(NULL, 0, "\t.section\t.rodata\n.LC%d:\n\t.string \"%s\\0\"\n\t.text\n",
            curr_label, escaped_str);
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1, "\t.section\t.rodata\n.LC%d:\n\t.string \"%s\\0\"\n\t.text\n",
            curr_label, escaped_str);
    inst_list = add_inst(inst_list, buffer);
    free(buffer);

    /* Setup printf call */
    len = snprintf(NULL, 0, "\tleaq\t.LC%d(%%rip), %%rdi\n", curr_label);
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1, "\tleaq\t.LC%d(%%rip), %%rdi\n", curr_label);
    inst_list = add_inst(inst_list, buffer);
    free(buffer);

    /* Pop arguments into correct registers */
    for(int i = arg_count; i > 0; i--) {
        fprintf(stderr, "DEBUG: Processing argument %d\n", i);
        const char *reg = (i == 1) ? "%%rsi" :
                         (i == 2) ? "%%rdx" :
                         (i == 3) ? "%%rcx" : "%%rax";
        fprintf(stderr, "DEBUG: Using register %s for argument %d\n", reg, i);
        len = snprintf(NULL, 0, "\tpopq\t%s\n", reg+1); // Skip first % in register name
        buffer = malloc(len + 1);
        snprintf(buffer, len + 1, "\tpopq\t%s\n", reg+1);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);
    }

#if 0
    /* Linux syscall implementation */
    fprintf(stderr, "DEBUG: Using syscall for output\n");
    len = snprintf(NULL, 0,
            "\tmovq $1, %%rax\n"
            "\tmovq $1, %%rdi\n"
            "\tleaq .LC%d(%%rip), %%rsi\n"
            "\tmovq $%d, %%rdx\n"
            "\tsyscall\n",
            curr_label, strlen(escaped_str)+1);
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1,
            "\tmovq $1, %%rax\n"
            "\tmovq $1, %%rdi\n"
            "\tleaq .LC%d(%%rip), %%rsi\n"
            "\tmovq $%d, %%rdx\n"
            "\tsyscall\n",
            curr_label, strlen(escaped_str)+1);
    inst_list = add_inst(inst_list, buffer);
    free(buffer);
#else
    /* Windows x64 calling convention requires:
     * - 32 bytes shadow space
     * - Stack 16-byte aligned
     */
    fprintf(stderr, "DEBUG: Allocating shadow space\n");
    len = snprintf(NULL, 0, "\tsubq\t$32, %%rsp\n");
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1, "\tsubq\t$32, %%rsp\n");
    inst_list = add_inst(inst_list, buffer);
    free(buffer);

    fprintf(stderr, "DEBUG: Calling printf\n");
    len = snprintf(NULL, 0, "\tmovl\t$0, %%eax\n");
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1, "\tmovl\t$0, %%eax\n");
    inst_list = add_inst(inst_list, buffer);
    free(buffer);

    len = snprintf(NULL, 0, "\tcall\tprintf\n");
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1, "\tcall\tprintf\n");
    inst_list = add_inst(inst_list, buffer);
    free(buffer);

    fprintf(stderr, "DEBUG: Cleaning up shadow space\n");
    len = snprintf(NULL, 0, "\taddq\t$32, %%rsp\n");
    buffer = malloc(len + 1);
    snprintf(buffer, len + 1, "\taddq\t$32, %%rsp\n");
    inst_list = add_inst(inst_list, buffer);
    free(buffer);
#endif
    return inst_list;
}

/* Helper function to escape string for assembly */
void escape_string(char *dest, const char *src, size_t dest_size) {
    size_t i = 0, j = 0;
    while (src[i] != '\0' && j < dest_size - 1) {
        switch (src[i]) {
            case '\n':
                dest[j++] = '\\';
                dest[j++] = 'n';
                break;
            case '\t':
                dest[j++] = '\\';
                dest[j++] = 't';
                break;
            case '\"':
                dest[j++] = '\\';
                dest[j++] = '\"';
                break;
            case '\\':
                dest[j++] = '\\';
                dest[j++] = '\\';
                break;
            default:
                dest[j++] = src[i];
        }
        i++;
    }
    dest[j] = '\0';
}

/* Code generation for writeln() builtin */
ListNode_t *codegen_builtin_writeln(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    char *buffer;

    /* For writeln(), we'll handle newlines in the format string */
    /* Don't modify the original string to avoid duplicate newlines */

#if PLATFORM_LINUX
    /* Linux syscall implementation */
    if(args != NULL && ((struct Expression *)args->cur)->type == EXPR_STRING) {
        char *str = ((struct Expression *)args->cur)->expr_data.string;
        int len = strlen(str);

        /* Add string to .rodata */
        /* Generate properly formatted assembly string */
        char escaped_str[512];
        escape_string(escaped_str, str, sizeof(escaped_str));
        int str_len = strlen(escaped_str) + 1; // Include null terminator

        len = snprintf(NULL, 0,
                "\t.section\t.rodata\n"
                ".LC%d:\n"
                "\t.string \"%s\"\n"  // String with properly escaped content
                "\t.text\n"
                "\tmovq $1, %%rax\n"
                "\tmovq $1, %%rdi\n"
                "\tleaq .LC%d(%%rip), %%rsi\n"
                "\tmovq $%zu, %%rdx\n"
                "\tsyscall\n",
                ctx->write_label_counter, escaped_str,
                ctx->write_label_counter, strlen(escaped_str)+1);  // Length with escaped content
        buffer = malloc(len + 1);
        snprintf(buffer, len + 1,
                "\t.section\t.rodata\n"
                ".LC%d:\n"
                "\t.string \"%s\"\n"  // String with properly escaped content
                "\t.text\n"
                "\tmovq $1, %%rax\n"
                "\tmovq $1, %%rdi\n"
                "\tleaq .LC%d(%%rip), %%rsi\n"
                "\tmovq $%zu, %%rdx\n"
                "\tsyscall\n",
                ctx->write_label_counter, escaped_str,
                ctx->write_label_counter, strlen(escaped_str)+1);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);


        ctx->write_label_counter++;
    } else {
        /* Fall back to printf for non-string args */
        return codegen_builtin_write(args, inst_list, ctx);
    }
#else
    /* Windows implementation with proper string escaping */
    if(args != NULL && ((struct Expression *)args->cur)->type == EXPR_STRING) {
        char *str = ((struct Expression *)args->cur)->expr_data.string;
        char escaped_str[512];
        escape_string(escaped_str, str, sizeof(escaped_str));

        /* Add string to .rdata section with proper escaping */
        int len = snprintf(NULL, 0,
                "\t.section\t.rdata,\"dr\"\n"
                ".LC%d:\n"
                "\t.ascii \"%s\\0\"\n"  // Explicit null termination
                "\t.text\n",
                ctx->write_label_counter, escaped_str);
        buffer = malloc(len + 1);
        snprintf(buffer, len + 1,
                "\t.section\t.rdata,\"dr\"\n"
                ".LC%d:\n"
                "\t.ascii \"%s\\0\"\n"
                "\t.text\n",
                ctx->write_label_counter, escaped_str);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);

        /* Setup printf call with proper Windows calling convention */
        len = snprintf(NULL, 0,
                "\tleaq\t.LC%d(%%rip), %%rcx\n"
                "\tsubq\t$32, %%rsp\n"  // Shadow space
                "\tcall\tprintf\n"
                "\taddq\t$32, %%rsp\n",
                ctx->write_label_counter);
        buffer = malloc(len + 1);
        snprintf(buffer, len + 1,
                "\tleaq\t.LC%d(%%rip), %%rcx\n"
                "\tsubq\t$32, %%rsp\n"
                "\tcall\tprintf\n"
                "\taddq\t$32, %%rsp\n",
                ctx->write_label_counter);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);

        ctx->write_label_counter++;
    } else {
        /* Fall back to printf for non-string args */
        return codegen_builtin_write(args, inst_list, ctx);
    }
#endif

    return inst_list;
}

/* Code generation for read() builtin */
ListNode_t *codegen_builtin_read(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    fprintf(stderr, "DEBUG: Generating read syscall\n");

    if(args != NULL) {
        char buffer[100];
        struct Expression *arg_expr = (struct Expression *)args->cur;
        assert(arg_expr->type == EXPR_VAR_ID);
        char *var_id = arg_expr->expr_data.id;
        StackNode_t *var_node = find_label(var_id);

        if (var_node == NULL) {
            fprintf(stderr, "ERROR: Could not find variable %s for readLn\n", var_id);
            exit(1);
        }

        /* Setup scanf call */
        int label_num = ctx->write_label_counter++;
        snprintf(buffer, 100, "\t.section\t.rodata\n.LC%d:\n\t.string \"%%d\"\n\t.text\n", label_num);
        inst_list = add_inst(inst_list, buffer);
        snprintf(buffer, 100, "\tleaq\t.LC%d(%%rip), %%rdi\n", label_num);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, 100, "\tleaq\t-%d(%%rbp), %%rsi\n", var_node->offset);
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, 100, "\tmovl\t$0, %%eax\n");
        inst_list = add_inst(inst_list, buffer);

        snprintf(buffer, 100, "\tcall\t__isoc99_scanf@PLT\n");
        inst_list = add_inst(inst_list, buffer);
    }

    fprintf(stderr, "DEBUG: Read syscall generated\n");
    return inst_list;
}

/* Code generation for simple relops */
ListNode_t *codegen_simple_relop(struct Expression *expr, ListNode_t *inst_list,
                                CodeGenContext *ctx, int *relop_type)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);

    fprintf(stderr, "DEBUG: Generating simple relop\n");

    *relop_type = expr->expr_data.relop_data.type;
    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, ctx);

    Register_t *left_reg = pop_reg_stack(get_reg_stack());
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, ctx);
    Register_t *right_reg = front_reg_stack(get_reg_stack());

    char buffer[100];
    snprintf(buffer, 100, "\tcmpl\t%s, %s\n", right_reg->bit_32, left_reg->bit_32);
    push_reg_stack(get_reg_stack(), left_reg);
    inst_list = add_inst(inst_list, buffer);

    fprintf(stderr, "DEBUG: Simple relop generated\n");
    return inst_list;
}

/* Code generation for non-local variable access */
ListNode_t *codegen_get_nonlocal(ListNode_t *inst_list, char *var_id, int *offset)
{
    fprintf(stderr, "DEBUG: Generating non-local access for %s\n", var_id);

    char buffer[100];
    StackNode_t *var = find_label(var_id);

    if(var == NULL) {
        fprintf(stderr, "ERROR: Could not find non-local variable %s\n", var_id);
        exit(1);
    }

    *offset = var->offset;
    snprintf(buffer, 100, "\tmovq\t-8(%%rbp), %s\n", NON_LOCAL_REG_64);
    inst_list = add_inst(inst_list, buffer);

    fprintf(stderr, "DEBUG: Non-local access generated\n");
    return inst_list;
}

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    int arg_num;
    StackNode_t *stack_node;
    Register_t *top_reg;
    char buffer[50];
    char *arg_reg_char;
    expr_node_t *expr_tree;

    arg_num = 0;
    while(args != NULL)
    {
        arg_reg_char = get_arg_reg64_num(arg_num);
        if(arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", arg_num);
            exit(1);
        }

        expr_tree = build_expr_tree((struct Expression *)args->cur);
        inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list, ctx);
        free_expr_tree(expr_tree);

        top_reg = front_reg_stack(get_reg_stack());
        snprintf(buffer, 50, "\tmovq\t%s, %s\n", top_reg->bit_64, arg_reg_char);
        inst_list = add_inst(inst_list, buffer);

        args = args->next;
        ++arg_num;
    }

    return inst_list;
}

/* Helper for codegen_get_nonlocal */
ListNode_t * codegen_goto_prev_scope(ListNode_t *inst_list, StackScope_t *cur_scope, char *base)
{
    char buffer[50];

    snprintf(buffer, 50, "\tmovq\t(%s), %s\n", base, NON_LOCAL_REG_64);
    inst_list = add_inst(inst_list, buffer);

    return inst_list;
}
