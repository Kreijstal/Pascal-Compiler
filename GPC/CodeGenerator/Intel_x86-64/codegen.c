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
#include "stmt_gen.h"
#include "expr_gen.h"
#include "builtin.h"
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
                snprintf(jmp_buf, 6, "jl");
            else
                snprintf(jmp_buf, 6, "jge");
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
#if PLATFORM_LINUX
    // System V ABI (Linux) uses %edi for the first argument
    fprintf(o_file, "\txor\t%%edi, %%edi\n"); // exit code 0
#else
    // Windows x64 ABI uses %ecx for the first argument
    fprintf(o_file, "\txor\t%%ecx, %%ecx\n"); // exit code 0
#endif
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

    // Use mangled_id if it exists, otherwise fall back to id
    sub_id = (proc->mangled_id != NULL) ? proc->mangled_id : proc->id;

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

    // Use mangled_id if it exists, otherwise fall back to id
    sub_id = (func->mangled_id != NULL) ? func->mangled_id : func->id;

    push_stackscope();

    inst_list = NULL;
    inst_list = codegen_subprogram_arguments(func->args_var, inst_list, o_file);

    /* Function name treated as return variable */
    /* For simplicity, just treating it as a local variable (let semcheck deal with shenanigans) */
    return_var = add_l_x(func->id);
    
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
        arg_reg_char = get_arg_reg64_num(arg_num);
        if(arg_reg_char == NULL)
        {
            fprintf(stderr, "ERROR: Could not get arg register: %d\n", arg_num);
            exit(1);
        }

        expr_tree = build_expr_tree((struct Expression *)args->cur);
        inst_list = gencode_expr_tree(expr_tree, get_reg_stack(), inst_list);
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
