#include "builtin.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "codegen.h"
#include "expr_gen.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../Parser/ParseTree/tree.h"
#include "register_types.h"

#include "builtin.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "codegen.h"
#include "expr_gen.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../Parser/ParseTree/tree.h"
#include "register_types.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"


/* Code generation for write() builtin - handles multiple args in single printf */
ListNode_t *codegen_builtin_write(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
{
    char *buffer;
    static int write_label_counter = 1;
    int curr_label = write_label_counter++;
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
                    (*write_label_counter)++, expr->expr_data.string);
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
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
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
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
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

/* Code generation for writeln() builtin */
ListNode_t *codegen_builtin_writeln(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
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

        static int writeln_label_counter = 1;
        int curr_label = writeln_label_counter++;
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
                (*write_label_counter)++, escaped_str,
                (*write_label_counter)-1, strlen(escaped_str)+1);  // Length with escaped content
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
                curr_label, escaped_str,
                curr_label, strlen(escaped_str)+1);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);
    } else {
        /* Fall back to printf for non-string args */
        return codegen_builtin_write(args, inst_list, o_file);
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
                write_label_counter, escaped_str);
        buffer = malloc(len + 1);
        snprintf(buffer, len + 1,
                "\t.section\t.rdata,\"dr\"\n"
                ".LC%d:\n"
                "\t.ascii \"%s\\0\"\n"
                "\t.text\n",
                write_label_counter, escaped_str);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);

        /* Setup printf call with proper Windows calling convention */
        len = snprintf(NULL, 0,
                "\tleaq\t.LC%d(%%rip), %%rcx\n"
                "\tsubq\t$32, %%rsp\n"  // Shadow space
                "\tcall\tprintf\n"
                "\taddq\t$32, %%rsp\n",
                write_label_counter);
        buffer = malloc(len + 1);
        snprintf(buffer, len + 1,
                "\tleaq\t.LC%d(%%rip), %%rcx\n"
                "\tsubq\t$32, %%rsp\n"
                "\tcall\tprintf\n"
                "\taddq\t$32, %%rsp\n",
                write_label_counter);
        inst_list = add_inst(inst_list, buffer);
        free(buffer);

        write_label_counter++;
    } else {
        /* Fall back to printf for non-string args */
        return codegen_builtin_write(args, inst_list, o_file);
    }
#endif

    return inst_list;
}

/* Code generation for read() builtin */
ListNode_t *codegen_builtin_read(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
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
        static int read_label_counter = 1;
        int label_num = (*write_label_counter)++;
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
