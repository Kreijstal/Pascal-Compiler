/*
    Damon Gwinn
    Code generation
    This is the dragon slayer

    See codegen.h for stack and implementation details
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "register_types.h"
#include "codegen.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/LexAndYacc/Grammar.tab.h"

/* Platform detection */
#if defined(__linux__) || defined(__unix__)
#define PLATFORM_LINUX 1
#else
#define PLATFORM_LINUX 0
#endif

/* Generates a label */
void gen_label(char *buf, int buf_len)
{
    snprintf(buf, buf_len, ".L%d", ++label_counter);
}

/* Adds instruction to instruction list */
/* WARNING: Makes copy of given char * */
ListNode_t *add_inst(ListNode_t *inst_list, char *inst)
{
    ListNode_t *new_node;

    new_node = CreateListNode(strdup(inst), LIST_STRING);
    if(inst_list == NULL)
    {
        inst_list = new_node;
    }
    else
    {
        PushListNodeBack(inst_list, new_node);
    }

    return inst_list;
}

/* Frees instruction list */
void free_inst_list(ListNode_t *inst_list)
{
    ListNode_t *cur;

    cur = inst_list;
    while(cur != NULL)
    {
        free(cur->cur);
        cur = cur->next;
    }

    DestroyList(inst_list);
}

/* Generates jmp */
/* Inverse jumps on the inverse of the type */
ListNode_t *gencode_jmp(int type, int inverse, char *label, ListNode_t *inst_list)
{
    char buffer[30], jmp_buf[6];

    switch(type)
    {
        case EQ:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jne");
            else
                snprintf(jmp_buf, 6, "je");
            break;
        case NE:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "je");
            else
                snprintf(jmp_buf, 6, "jne");
            break;
        case LT:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jge");
            else
                snprintf(jmp_buf, 6, "jl");
            break;
        case LE:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jg");
            else
                snprintf(jmp_buf, 6, "jle");
            break;
        case GT:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jle");
            else
                snprintf(jmp_buf, 6, "jg");
            break;
        case GE:
            if(inverse > 0)
                snprintf(jmp_buf, 6, "jge");
            else
                snprintf(jmp_buf, 6, "jl");
            break;

        case NORMAL_JMP:
            snprintf(jmp_buf, 6, "jmp");
            break;

        default:
            fprintf(stderr, "ERROR: Unrecognized relop type in jmp generation!\n");
            exit(1);
    }

    snprintf(buffer, 30, "\t%s\t%s\n", jmp_buf, label);

    return add_inst(inst_list, buffer);
}

/* Generates a function header */
void codegen_function_header(char *func_name, FILE *o_file)
{
    /*
        .globl	<func_name>
        <func_name>:
            pushq   %rbp
            movq    %rsp, %rbp
    */

    fprintf(o_file, ".globl\t%s\n", func_name);
    fprintf(o_file, "%s:\n\tpushq\t%%rbp\n\tmovq\t%%rsp, %%rbp\n", func_name);

    return;
}

/* Generates a function footer */
void codegen_function_footer(char *func_name, FILE *o_file)
{
    /*
        nop
        leave
        ret
    */

    fprintf(o_file, "\tnop\n\tleave\n\tret\n");

    return;
}


/* This is the entry function */
void codegen(Tree_t *tree, char *input_file_name, char *output_file_name)
{
    FILE *output_file;
    char *prgm_name;

    output_file = fopen(output_file_name, "w");
    if(output_file == NULL)
    {
        fprintf(stderr, "ERROR: Failed to open output file: %s\n", output_file_name);
        exit(1);
    }

    /* codegen.h */
    label_counter = 1;

    init_stackmng();

    codegen_program_header(input_file_name, output_file);

    prgm_name = codegen_program(tree, output_file);
    codegen_main(prgm_name, output_file);

    codegen_program_footer(output_file);

    fclose(output_file);
    output_file = NULL;

    free_stackmng();

    return;
}

/* Generates platform-compatible headers */
void codegen_program_header(char *fname, FILE *o_file)
{
    fprintf(o_file, "\t.file\t\"%s\"\n", fname);
    
#if PLATFORM_LINUX
    /* Linux sections */
    fprintf(o_file, "\t.section\t.rodata\n");
#else
    /* Windows sections */
    fprintf(o_file, "\t.section\t.rdata,\"dr\"\n");
#endif

    fprintf(o_file, "\t.text\n");
    return;
}

/* Generates platform-compatible program footer */
void codegen_program_footer(FILE *o_file)
{
#if PLATFORM_LINUX
    /* Linux doesn't need .ident */
#else
    /* Windows .ident directive */
    fprintf(o_file, ".ident\t\"GPC: 0.0.0\"\n");
#endif
}

/* Generates main which calls our program */
void codegen_main(char *prgm_name, FILE *o_file)
{
    /*
        HEADER
            movl	$0, %eax
            call	<prgm_name>
            movl	$0, %eax
            popq    %rbp
            ret
    */
    fprintf(o_file, "\t.section\t.text\n");
    fprintf(o_file, "\t.globl\tmain\n");
    codegen_function_header("main", o_file);
    fprintf(o_file, "\tsubq\t$32, %%rsp\n");  // Allocate stack space (32 bytes for shadow space)
    fprintf(o_file, "\tcall\t%s\n", prgm_name);
    // Windows-compatible exit
    fprintf(o_file, "\txor\t%%ecx, %%ecx\n"); // exit code 0
    fprintf(o_file, "\tcall\texit\n");         // call exit function
    codegen_function_footer("main", o_file);
}

/* Generates code to allocate needed stack space */
void codegen_stack_space(FILE *o_file)
{
    int needed_space;
    needed_space = get_full_stack_offset();
    assert(needed_space >= 0);

    if(needed_space != 0)
    {
        /* subq	$<needed_space>, %rsp */
        fprintf(o_file, "\tsubq\t$%d, %%rsp\n", needed_space);
    }
}

/* Writes instruction list to file */
/* A NULL inst_list is interpreted as no instructions */
void codegen_inst_list(ListNode_t *inst_list, FILE *o_file)
{
    char *inst;

    while(inst_list != NULL)
    {
        inst = (char *)inst_list->cur;
        assert(inst != NULL);

        fprintf(o_file, "%s", inst);

        inst_list = inst_list->next;
    }
}


/******* EVERYTHING BELOW THIS POINT IS VERY GRUESOME DRAGON SLAYING ********/

/* TODO: Currently only handles local variables and body_statement */
/* Returns the program name for use with main */
char * codegen_program(Tree_t *prgm, FILE *o_file)
{
    assert(prgm->type == TREE_PROGRAM_TYPE);

    char *prgm_name;
    struct Program *data;
    ListNode_t *inst_list;

    data = &prgm->tree_data.program_data;
    prgm_name = data->program_id;

    push_stackscope();

    codegen_function_locals(data->var_declaration, o_file);
    codegen_subprograms(data->subprograms, o_file);

    inst_list = NULL;
    inst_list = codegen_stmt(data->body_statement, inst_list, o_file);

    codegen_function_header(prgm_name, o_file);
    codegen_stack_space(o_file);
    codegen_inst_list(inst_list, o_file);
    codegen_function_footer(prgm_name, o_file);
    free_inst_list(inst_list);

    pop_stackscope();

    return prgm_name;
}

/* Pushes function locals onto the stack */
void codegen_function_locals(ListNode_t *local_decl, FILE *o_file)
{
     ListNode_t *cur, *id_list;
     Tree_t *tree;

     cur = local_decl;

     while(cur != NULL)
     {
         tree = (Tree_t *)cur->cur;
         assert(tree != NULL);
         assert(tree->type == TREE_VAR_DECL);

         id_list = tree->tree_data.var_decl_data.ids;
         if(tree->tree_data.var_decl_data.type == REAL_TYPE)
         {
             fprintf(stderr, "Warning: REAL types not supported, treating as integer\n");
         }

         while(id_list != NULL)
         {
             add_l_x((char *)id_list->cur);
             id_list = id_list->next;
         };

         cur = cur->next;
     }
}

/* Sets number of vector registers (floating points) before a function call */
ListNode_t *codegen_vect_reg(ListNode_t *inst_list, int num_vec)
{
    char buffer[50];

    snprintf(buffer, 50, "\tmovl\t$%d, %%eax\n", num_vec);

    return add_inst(inst_list, buffer);
}

/* Codegen for a list of subprograms */
/* NOTE: List can be null */
void codegen_subprograms(ListNode_t *sub_list, FILE *o_file)
{
    Tree_t *sub;

    while(sub_list != NULL)
    {
        sub = (Tree_t *)sub_list->cur;
        assert(sub != NULL);
        assert(sub->type == TREE_SUBPROGRAM);

        switch(sub->tree_data.subprogram_data.sub_type)
        {
            case TREE_SUBPROGRAM_PROC:
                codegen_procedure(sub, o_file);
                break;

            case TREE_SUBPROGRAM_FUNC:
                codegen_function(sub, o_file);
                break;

            default:
                fprintf(stderr, "ERROR: Unrecognized subprogram type in codegen!\n");
        }

        sub_list = sub_list->next;
    }
}

/* Code generation for a procedure */
/* TODO: Support non-local variables */
void codegen_procedure(Tree_t *proc_tree, FILE *o_file)
{
    assert(proc_tree != NULL);
    assert(proc_tree->type == TREE_SUBPROGRAM);
    assert(proc_tree->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_PROC);

    struct Subprogram *proc;
    ListNode_t *inst_list;
    char buffer[50];
    char *sub_id;

    proc = &proc_tree->tree_data.subprogram_data;
    sub_id = proc->id;

    push_stackscope();

    inst_list = NULL;
    inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, o_file);

    codegen_function_locals(proc->declarations, o_file);
    codegen_subprograms(proc->subprograms, o_file);

    inst_list = codegen_stmt(proc->statement_list, inst_list, o_file);

    codegen_function_header(sub_id, o_file);
    codegen_stack_space(o_file);
    codegen_inst_list(inst_list, o_file);
    codegen_function_footer(sub_id, o_file);
    free_inst_list(inst_list);

    pop_stackscope();
}

/* Code generation for a function */
void codegen_function(Tree_t *func_tree, FILE *o_file)
{
    assert(func_tree != NULL);
    assert(func_tree->type == TREE_SUBPROGRAM);
    assert(func_tree->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_FUNC);

    struct Subprogram *func;
    ListNode_t *inst_list;
    char buffer[50];
    char *sub_id;
    StackNode_t *return_var;

    func = &func_tree->tree_data.subprogram_data;
    sub_id = func->id;

    push_stackscope();

    inst_list = NULL;
    inst_list = codegen_subprogram_arguments(func->args_var, inst_list, o_file);

    /* Function name treated as return variable */
    /* For simplicity, just treating it as a local variable (let semcheck deal with shenanigans) */
    return_var = add_l_x(sub_id);
    
    codegen_function_locals(func->declarations, o_file);
    codegen_subprograms(func->subprograms, o_file);

    inst_list = codegen_stmt(func->statement_list, inst_list, o_file);

    /* Return statement */
    snprintf(buffer, 50, "\tmovl\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_32);
    inst_list = add_inst(inst_list, buffer);


    codegen_function_header(sub_id, o_file);
    codegen_stack_space(o_file);
    codegen_inst_list(inst_list, o_file);
    codegen_function_footer(sub_id, o_file);
    free_inst_list(inst_list);

    pop_stackscope();
}

/* Code generation for subprogram arguments */
/* Returns list of arguments to move arguements onto the stack */
/* NOTE: List can be NULL */
/* TODO: Support arrays */
/* TODO: Support any number of arguments */
ListNode_t *codegen_subprogram_arguments(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
{
    Tree_t *arg_decl;
    int type, arg_num;
    ListNode_t *arg_ids;
    char *arg_reg;
    char buffer[50];
    StackNode_t *arg_stack;

    while(args != NULL)
    {
        arg_decl = (Tree_t *)args->cur;

        switch(arg_decl->type)
        {
            case TREE_VAR_DECL:
                arg_ids = arg_decl->tree_data.var_decl_data.ids;
                type = arg_decl->tree_data.var_decl_data.type;
                if(type == REAL_TYPE)
                    fprintf(stderr, "WARNING: Only integers are supported!\n");

                arg_num = 0;
                while(arg_ids != NULL)
                {
                    arg_reg = get_arg_reg32_num(arg_num);
                    if(arg_reg == NULL)
                    {
                        fprintf(stderr, "ERROR: Max argument limit: %d\n", NUM_ARG_REG);
                        exit(1);
                    }

                    arg_stack = add_l_z((char *)arg_ids->cur);

                    snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", arg_reg, arg_stack->offset);
                    inst_list = add_inst(inst_list, buffer);

                    arg_ids = arg_ids->next;
                    ++arg_num;
                }

                break;

            case TREE_ARR_DECL:
                fprintf(stderr, "ERROR: Arrays not currently supported as arguments!\n");
                exit(1);
                break;

            default:
                fprintf(stderr, "ERROR: Unknown argument type!\n");
                exit(1);
        }

        args = args->next;
    }

    return inst_list;
}

/* Code generation for expressions */
ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list, FILE *o_file)
{
    assert(expr != NULL);
    expr_node_t *expr_tree = NULL;
    
    fprintf(stderr, "DEBUG: Generating code for expression type %d\n", expr->type);
    
    switch(expr->type) {
        case EXPR_VAR_ID:
            fprintf(stderr, "DEBUG: Processing variable ID expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_INUM:
            fprintf(stderr, "DEBUG: Processing integer constant expression\n");
            expr_tree = build_expr_tree(expr);
            inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
            free_expr_tree(expr_tree);
            return inst_list;
        case EXPR_RELOP:
            fprintf(stderr, "DEBUG: Processing relational operator expression\n");
            return codegen_simple_relop(expr, inst_list, o_file, NULL);
        default:
            fprintf(stderr, "ERROR: Unsupported expression type %d\n", expr->type);
            exit(1);
    }
}

int write_label_counter = 1;

/* Code generation for write() builtin - handles multiple args in single printf */
ListNode_t *codegen_builtin_write(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
{
    char buffer[100];
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
            snprintf(buffer, 100, "\tleaq\t.LC%d(%%rip), %%rax\n", curr_label);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, 100, "\tpushq\t%%rax\n");
            inst_list = add_inst(inst_list, buffer);
            arg_count++;
            
            /* Add string to .rodata */
            snprintf(buffer, 100, "\t.section\t.rodata\n.LC%d:\n\t.string \"%s\"\n\t.text\n",
                    curr_label++, expr->expr_data.string);
            inst_list = add_inst(inst_list, buffer);
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
            snprintf(buffer, 100, "\tpushq\t%%%s\n", top_reg->bit_64+1); // Skip first % in register name
            inst_list = add_inst(inst_list, buffer);
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
    
    snprintf(buffer, 100, "\t.section\t.rodata\n.LC%d:\n\t.string \"%s\\0\"\n\t.text\n",
            curr_label, escaped_str);
    inst_list = add_inst(inst_list, buffer);
    
    /* Setup printf call - Windows x64 calling convention */
    snprintf(buffer, 100, "\tleaq\t.LC%d(%%rip), %%rcx\n", curr_label);
    inst_list = add_inst(inst_list, buffer);
    
    /* Pop arguments into correct registers (rcx, rdx, r8, r9) */
    for(int i = arg_count; i > 0; i--) {
        fprintf(stderr, "DEBUG: Processing argument %d\n", i);
        const char *reg = (i == 1) ? "%%rdx" :
                         (i == 2) ? "%%r8" :
                         (i == 3) ? "%%r9" : "%%rax";
        fprintf(stderr, "DEBUG: Using register %s for argument %d\n", reg, i);
        snprintf(buffer, 100, "\tpopq\t%s\n", reg+1); // Skip first % in register name
        inst_list = add_inst(inst_list, buffer);
    }
    
#if PLATFORM_LINUX
    /* Linux syscall implementation */
    fprintf(stderr, "DEBUG: Using syscall for output\n");
    snprintf(buffer, sizeof(buffer),
            "\tmovq $1, %%rax\n"
            "\tmovq $1, %%rdi\n"
            "\tleaq .LC%d(%%rip), %%rsi\n"
            "\tmovq $%d, %%rdx\n"
            "\tsyscall\n",
            curr_label, strlen(escaped_str)+1);
    inst_list = add_inst(inst_list, buffer);
#else
    /* Windows x64 calling convention requires:
     * - 32 bytes shadow space
     * - Stack 16-byte aligned
     */
    fprintf(stderr, "DEBUG: Allocating shadow space\n");
    snprintf(buffer, 100, "\tsubq\t$32, %%rsp\n");
    inst_list = add_inst(inst_list, buffer);
    
    fprintf(stderr, "DEBUG: Calling printf\n");
    snprintf(buffer, 100, "\tcall\tprintf\n");
    inst_list = add_inst(inst_list, buffer);
    
    fprintf(stderr, "DEBUG: Cleaning up shadow space\n");
    snprintf(buffer, 100, "\taddq\t$32, %%rsp\n");
    inst_list = add_inst(inst_list, buffer);
#endif
    return inst_list;
}

/* Code generation for writeln() builtin */
ListNode_t *codegen_builtin_writeln(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
{
    char buffer[100];
    
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
        
        char buffer[512];
        snprintf(buffer, sizeof(buffer),
                "\t.section\t.rodata\n"
                ".LC%d:\n"
                "\t.string \"%s\"\n"  // String with properly escaped content
                "\t.text\n"
                "\tmovq $1, %%rax\n"
                "\tmovq $1, %%rdi\n"
                "\tleaq .LC%d(%%rip), %%rsi\n"
                "\tmovq $%d, %%rdx\n"
                "\tsyscall\n",
                write_label_counter, escaped_str,
                write_label_counter, strlen(escaped_str)+1);  // Length with escaped content
        inst_list = add_inst(inst_list, buffer);
        
        
        write_label_counter++;
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
        snprintf(buffer, sizeof(buffer),
                "\t.section\t.rdata,\"dr\"\n"
                ".LC%d:\n"
                "\t.ascii \"%s\\0\"\n"  // Explicit null termination
                "\t.text\n",
                write_label_counter, escaped_str);
        inst_list = add_inst(inst_list, buffer);
        
        /* Setup printf call with proper Windows calling convention */
        snprintf(buffer, sizeof(buffer),
                "\tleaq\t.LC%d(%%rip), %%rcx\n"
                "\tsubq\t$32, %%rsp\n"  // Shadow space
                "\tcall\tprintf\n"
                "\taddq\t$32, %%rsp\n",
                write_label_counter);
        inst_list = add_inst(inst_list, buffer);
        
        write_label_counter++;
    } else {
        /* Fall back to printf for non-string args */
        return codegen_builtin_write(args, inst_list, o_file);
    }
#endif

    return inst_list;
}

/* Removed duplicate function definition */

/* Code generation for read() builtin */
ListNode_t *codegen_builtin_read(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
{
    fprintf(stderr, "DEBUG: Generating read syscall\n");
    
    if(args != NULL) {
        char buffer[100];
        expr_node_t *expr_tree = build_expr_tree((struct Expression *)args->cur);
        inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
        free_expr_tree(expr_tree);
        
        Register_t *top_reg = front_reg_stack(get_reg_stack());
        
        /* Setup scanf call */
        snprintf(buffer, 100, "\tleaq\t.LC1(%%rip), %%rdi\n");
        inst_list = add_inst(inst_list, buffer);
        
        StackNode_t *stack_node = find_in_temp(top_reg->bit_64);
        if(stack_node == NULL) {
            fprintf(stderr, "ERROR: Could not find stack offset for register %s\n", top_reg->bit_64);
            exit(1);
        }
        snprintf(buffer, 100, "\tleaq\t-%d(%%rbp), %%rsi\n", stack_node->offset);
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
                                FILE *o_file, int *relop_type)
{
    assert(expr != NULL);
    assert(expr->type == EXPR_RELOP);
    
    fprintf(stderr, "DEBUG: Generating simple relop\n");
    
    *relop_type = expr->expr_data.relop_data.type;
    inst_list = codegen_expr(expr->expr_data.relop_data.left, inst_list, o_file);
    inst_list = codegen_expr(expr->expr_data.relop_data.right, inst_list, o_file);
    
    Register_t *left_reg = front_reg_stack(get_reg_stack());
    Register_t *right_reg = front_reg_stack(get_reg_stack());
    
    char buffer[100];
    snprintf(buffer, 100, "\tcmpq\t%s, %s\n", right_reg->bit_64, left_reg->bit_64);
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

    proc_name = stmt->stmt_data.procedure_call_data.id;
    args_expr = stmt->stmt_data.procedure_call_data.expr_args;

    /* First check for builtins */
    if(strcmp("write", proc_name) == 0)
    {
        fprintf(stderr, "DEBUG: Generating code for write() builtin\n");
        inst_list = codegen_builtin_write(args_expr, inst_list, o_file);
        fprintf(stderr, "DEBUG: Finished generating code for write()\n");
    }
    else if(strcmp("writeln", proc_name) == 0)
    {
        fprintf(stderr, "DEBUG: Generating code for writeln() builtin\n");
        inst_list = codegen_builtin_writeln(args_expr, inst_list, o_file);
        fprintf(stderr, "DEBUG: Finished generating code for writeln()\n");
    }
    else if(strcmp("read", proc_name) == 0)
    {
        fprintf(stderr, "DEBUG: Generating code for read() builtin\n");
        inst_list = codegen_builtin_read(args_expr, inst_list, o_file);
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

/* Code generation for passing arguments */
ListNode_t *codegen_pass_arguments(ListNode_t *args, ListNode_t *inst_list, FILE *o_file)
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
        arg_reg_char = get_arg_reg32_num(arg_num);
        if(arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", arg_num);
            exit(1);
        }

        expr_tree = build_expr_tree((struct Expression *)args->cur);
        inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
        free_expr_tree(expr_tree);

        top_reg = front_reg_stack(get_reg_stack());
        snprintf(buffer, 50, "\tmovl\t%s, %s\n", top_reg->bit_32, arg_reg_char);
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

/* Performs non-local variable chasing with the appropriate register */
/* Gives the offset to use on the register */
/* Removed duplicate function definition */
/* Removed malformed function fragments */

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
