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
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "Grammar.tab.h"

/* Platform detection */
#if defined(__linux__) || defined(__unix__)
#define PLATFORM_LINUX 1
#else
#define PLATFORM_LINUX 0
#endif

/* Generates a label */
void gen_label(char *buf, int buf_len, CodeGenContext *ctx)
{
    assert(buf != NULL);
    assert(ctx != NULL);
    snprintf(buf, buf_len, ".L%d", ++ctx->label_counter);
}

/* Adds instruction to instruction list */
/* WARNING: Makes copy of given char * */
ListNode_t *add_inst(ListNode_t *inst_list, char *inst)
{
    ListNode_t *new_node;

    assert(inst != NULL);

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

    if(inst_list == NULL)
        return;

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

    assert(label != NULL);

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
            assert(0 && "Unrecognized relop type in jmp generation!");
            break;
    }

    snprintf(buffer, 30, "\t%s\t%s\n", jmp_buf, label);

    return add_inst(inst_list, buffer);
}

/* Generates a function header */
void codegen_function_header(char *func_name, CodeGenContext *ctx)
{
    assert(func_name != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, ".globl\t%s\n", func_name);
    fprintf(ctx->output_file, "%s:\n\tpushq\t%%rbp\n\tmovq\t%%rsp, %%rbp\n", func_name);

    return;
}

/* Generates a function footer */
void codegen_function_footer(char *func_name, CodeGenContext *ctx)
{
    assert(func_name != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, "\tnop\n\tleave\n\tret\n");

    return;
}


/* This is the entry function */
void codegen(Tree_t *tree, char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab)
{
    char *prgm_name;

    assert(tree != NULL);
    assert(input_file_name != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    fprintf(stderr, "DEBUG: ENTERING codegen\n");
    init_stackmng();

    codegen_program_header(input_file_name, ctx);
    codegen_rodata(ctx);

    prgm_name = codegen_program(tree, ctx, symtab);
    codegen_main(prgm_name, ctx);

    codegen_program_footer(ctx);

    free_stackmng();

    fprintf(stderr, "DEBUG: LEAVING codegen\n");
    return;
}

void codegen_rodata(CodeGenContext *ctx)
{
    assert(ctx != NULL);
    fprintf(ctx->output_file, ".section .rodata\n");
    fprintf(ctx->output_file, ".format_str_s:\n");
    fprintf(ctx->output_file, ".string \"%%s\"\n");
    fprintf(ctx->output_file, ".format_str_d:\n");
    fprintf(ctx->output_file, ".string \"%%d\"\n");
    fprintf(ctx->output_file, ".format_str_sn:\n");
    fprintf(ctx->output_file, ".string \"%%s\\n\"\n");
    fprintf(ctx->output_file, ".format_str_dn:\n");
    fprintf(ctx->output_file, ".string \"%%d\\n\"\n");
    fprintf(ctx->output_file, ".format_str_n:\n");
    fprintf(ctx->output_file, ".string \"\\n\"\n");
    fprintf(ctx->output_file, ".text\n");
}

/* Generates platform-compatible headers */
void codegen_program_header(char *fname, CodeGenContext *ctx)
{
    assert(fname != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, "\t.file\t\"%s\"\n", fname);
    
#if PLATFORM_LINUX
    fprintf(ctx->output_file, "\t.section\t.rodata\n");
#else
    fprintf(ctx->output_file, "\t.section\t.rdata,\"dr\"\n");
#endif

    fprintf(ctx->output_file, "\t.text\n");
    return;
}

/* Generates platform-compatible program footer */
void codegen_program_footer(CodeGenContext *ctx)
{
    assert(ctx != NULL);
#if PLATFORM_LINUX
#else
    fprintf(ctx->output_file, ".ident\t\"GPC: 0.0.0\"\n");
#endif
}

/* Generates main which calls our program */
void codegen_main(char *prgm_name, CodeGenContext *ctx)
{
    assert(prgm_name != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, "\t.section\t.text\n");
    fprintf(ctx->output_file, "\t.globl\tmain\n");
    codegen_function_header("main", ctx);
    fprintf(ctx->output_file, "\tsubq\t$32, %%rsp\n");
    fprintf(ctx->output_file, "\tcall\t%s\n", prgm_name);
#if PLATFORM_LINUX
    fprintf(ctx->output_file, "\txor\t%%edi, %%edi\n");
#else
    fprintf(ctx->output_file, "\txor\t%%ecx, %%ecx\n");
#endif
    fprintf(ctx->output_file, "\tcall\texit\n");
    codegen_function_footer("main", ctx);
}

/* Generates code to allocate needed stack space */
void codegen_stack_space(CodeGenContext *ctx)
{
    int needed_space;

    assert(ctx != NULL);

    needed_space = get_full_stack_offset();
    assert(needed_space >= 0);

    if(needed_space != 0)
    {
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", needed_space);
    }
}

/* Writes instruction list to file */
void codegen_inst_list(ListNode_t *inst_list, CodeGenContext *ctx)
{
    char *inst;

    assert(ctx != NULL);

    while(inst_list != NULL)
    {
        inst = (char *)inst_list->cur;
        assert(inst != NULL);

        fprintf(ctx->output_file, "%s", inst);

        inst_list = inst_list->next;
    }
}

/* Returns the program name for use with main */
char * codegen_program(Tree_t *prgm, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(prgm->type == TREE_PROGRAM_TYPE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char *prgm_name;
    struct Program *data;
    ListNode_t *inst_list;

    data = &prgm->tree_data.program_data;
    prgm_name = data->program_id;

    push_stackscope();

    codegen_function_locals(data->var_declaration, ctx);
    codegen_subprograms(data->subprograms, ctx, symtab);

    inst_list = NULL;
    inst_list = codegen_stmt(data->body_statement, inst_list, ctx, symtab);

    codegen_function_header(prgm_name, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(prgm_name, ctx);
    free_inst_list(inst_list);

    pop_stackscope();

    return prgm_name;
}

/* Pushes function locals onto the stack */
void codegen_function_locals(ListNode_t *local_decl, CodeGenContext *ctx)
{
     ListNode_t *cur, *id_list;
     Tree_t *tree;

    assert(ctx != NULL);

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
    assert(inst_list != NULL);
    snprintf(buffer, 50, "\tmovl\t$%d, %%eax\n", num_vec);
    return add_inst(inst_list, buffer);
}

/* Codegen for a list of subprograms */
void codegen_subprograms(ListNode_t *sub_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    Tree_t *sub;

    assert(ctx != NULL);
    assert(symtab != NULL);

    while(sub_list != NULL)
    {
        sub = (Tree_t *)sub_list->cur;
        assert(sub != NULL);
        assert(sub->type == TREE_SUBPROGRAM);

        switch(sub->tree_data.subprogram_data.sub_type)
        {
            case TREE_SUBPROGRAM_PROC:
                codegen_procedure(sub, ctx, symtab);
                break;
            case TREE_SUBPROGRAM_FUNC:
                codegen_function(sub, ctx, symtab);
                break;
            default:
                assert(0 && "Unrecognized subprogram type in codegen!");
        }
        sub_list = sub_list->next;
    }
}

/* Code generation for a procedure */
void codegen_procedure(Tree_t *proc_tree, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(proc_tree != NULL);
    assert(proc_tree->type == TREE_SUBPROGRAM);
    assert(proc_tree->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_PROC);
    assert(ctx != NULL);
    assert(symtab != NULL);

    struct Subprogram *proc;
    ListNode_t *inst_list;
    char *sub_id;

    proc = &proc_tree->tree_data.subprogram_data;
    sub_id = (proc->mangled_id != NULL) ? proc->mangled_id : proc->id;

    push_stackscope();
    inst_list = NULL;
    inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, ctx);
    codegen_function_locals(proc->declarations, ctx);

    codegen_subprograms(proc->subprograms, ctx, symtab);
    inst_list = codegen_stmt(proc->statement_list, inst_list, ctx, symtab);
    codegen_function_header(sub_id, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(sub_id, ctx);
    free_inst_list(inst_list);
    pop_stackscope();
}

/* Code generation for a function */
void codegen_function(Tree_t *func_tree, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(func_tree != NULL);
    assert(func_tree->type == TREE_SUBPROGRAM);
    assert(func_tree->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_FUNC);
    assert(ctx != NULL);
    assert(symtab != NULL);

    struct Subprogram *func;
    ListNode_t *inst_list;
    char buffer[50];
    char *sub_id;
    StackNode_t *return_var;

    func = &func_tree->tree_data.subprogram_data;
    sub_id = (func->mangled_id != NULL) ? func->mangled_id : func->id;

    push_stackscope();
    inst_list = NULL;
    inst_list = codegen_subprogram_arguments(func->args_var, inst_list, ctx);
    return_var = add_l_x(func->id);
    codegen_function_locals(func->declarations, ctx);

    codegen_subprograms(func->subprograms, ctx, symtab);
    inst_list = codegen_stmt(func->statement_list, inst_list, ctx, symtab);
    snprintf(buffer, 50, "\tmovl\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_32);
    inst_list = add_inst(inst_list, buffer);
    codegen_function_header(sub_id, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(sub_id, ctx);
    free_inst_list(inst_list);
    pop_stackscope();
}

/* Code generation for subprogram arguments */
ListNode_t *codegen_subprogram_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    Tree_t *arg_decl;
    int type, arg_num;
    ListNode_t *arg_ids;
    char *arg_reg;
    char buffer[50];
    StackNode_t *arg_stack;

    assert(ctx != NULL);

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
                assert(0 && "Unknown argument type!");
                break;
        }
        args = args->next;
    }
    return inst_list;
}
