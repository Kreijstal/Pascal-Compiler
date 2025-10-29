/*
    Damon Gwinn
    Code generation
    This is the dragon slayer

    See codegen.h for stack and implementation details
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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
#include "../../Parser/ParseTree/type_tags.h"

ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);

gpc_target_abi_t g_current_codegen_abi = GPC_TARGET_ABI_SYSTEM_V;
int g_stack_home_space_bytes = 0;

void codegen_report_error(CodeGenContext *ctx, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    if (fmt != NULL && fmt[0] != '\0')
    {
        size_t len = strlen(fmt);
        if (len == 0 || fmt[len - 1] != '\n')
            fputc('\n', stderr);
    }
    va_end(args);
    if (ctx != NULL)
        ctx->had_error = 1;
}

int codegen_had_error(const CodeGenContext *ctx)
{
    return (ctx != NULL) ? ctx->had_error : 0;
}

static void codegen_reset_finally_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->finally_stack != NULL)
    {
        free(ctx->finally_stack);
        ctx->finally_stack = NULL;
    }
    ctx->finally_depth = 0;
    ctx->finally_capacity = 0;
}

static void codegen_reset_except_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->except_labels != NULL)
    {
        for (int i = 0; i < ctx->except_depth; ++i)
        {
            free(ctx->except_labels[i]);
            ctx->except_labels[i] = NULL;
        }
        free(ctx->except_labels);
        ctx->except_labels = NULL;
    }
    ctx->except_depth = 0;
    ctx->except_capacity = 0;
}

static void codegen_reset_loop_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->loop_exit_labels != NULL)
    {
        for (int i = 0; i < ctx->loop_depth; ++i)
            free(ctx->loop_exit_labels[i]);
        free(ctx->loop_exit_labels);
        ctx->loop_exit_labels = NULL;
    }
    ctx->loop_depth = 0;
    ctx->loop_capacity = 0;
}

/* Generates a label */
void gen_label(char *buf, int buf_len, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(buf != NULL);
    assert(ctx != NULL);
    snprintf(buf, buf_len, ".L%d", ++ctx->label_counter);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Adds instruction to instruction list */
/* WARNING: Makes copy of given char * */
ListNode_t *add_inst(ListNode_t *inst_list, char *inst)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
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

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* Frees instruction list */
void free_inst_list(ListNode_t *inst_list)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
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
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates jmp */
/* Inverse jumps on the inverse of the type */
ListNode_t *gencode_jmp(int type, int inverse, char *label, ListNode_t *inst_list)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
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

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return add_inst(inst_list, buffer);
}

/* Generates a function header */
void codegen_function_header(char *func_name, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_name != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, ".globl\t%s\n", func_name);
    fprintf(ctx->output_file, "%s:\n\tpushq\t%%rbp\n\tmovq\t%%rsp, %%rbp\n", func_name);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

/* Generates a function footer */
void codegen_function_footer(char *func_name, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_name != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, "\tnop\n\tleave\n\tret\n");

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}


/* This is the entry function */
void codegen(Tree_t *tree, const char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char *prgm_name;

    assert(tree != NULL);
    assert(input_file_name != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (ctx->target_abi != GPC_TARGET_ABI_SYSTEM_V && ctx->target_abi != GPC_TARGET_ABI_WINDOWS)
        ctx->target_abi = current_target_abi();

    g_current_codegen_abi = ctx->target_abi;
    g_stack_home_space_bytes = (ctx->target_abi == GPC_TARGET_ABI_WINDOWS) ? 32 : 0;

    ctx->symtab = symtab;

    codegen_reset_finally_stack(ctx);
    codegen_reset_loop_stack(ctx);
    codegen_reset_except_stack(ctx);

    CODEGEN_DEBUG("DEBUG: ENTERING codegen\n");
    init_stackmng();

    codegen_program_header(input_file_name, ctx);
    codegen_rodata(ctx);

    prgm_name = codegen_program(tree, ctx, symtab);
    codegen_main(prgm_name, ctx);

    codegen_program_footer(ctx);

    free_stackmng();
    codegen_reset_loop_stack(ctx);
    codegen_reset_finally_stack(ctx);
    codegen_reset_except_stack(ctx);

    CODEGEN_DEBUG("DEBUG: LEAVING codegen\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_rodata(CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.section\t.rdata,\"dr\"\n");
    else
        fprintf(ctx->output_file, "\t.section\t.rodata\n");
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
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates platform-compatible headers */
void codegen_program_header(const char *fname, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(fname != NULL);
    assert(ctx != NULL);
    fprintf(ctx->output_file, "\t.file\t\"%s\"\n", fname);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.section\t.rdata,\"dr\"\n");
    else
        fprintf(ctx->output_file, "\t.section\t.rodata\n");

    fprintf(ctx->output_file, "\t.text\n");
    fprintf(ctx->output_file, "\t.set\tGPC_TARGET_WINDOWS, %d\n", codegen_target_is_windows());
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

/* Generates platform-compatible program footer */
void codegen_program_footer(CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, ".ident\t\"GPC: 0.0.0\"\n");
    else
        fprintf(ctx->output_file, "\t.section\t.note.GNU-stack,\"\",@progbits\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates main which calls our program */
void codegen_main(char *prgm_name, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(prgm_name != NULL);
    assert(ctx != NULL);
    int call_space;
    fprintf(ctx->output_file, "\t.section\t.text\n");
    fprintf(ctx->output_file, "\t.globl\tmain\n");
    codegen_function_header("main", ctx);
    call_space = codegen_target_is_windows() ? g_stack_home_space_bytes : 32;
    if (call_space > 0)
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", call_space);
    fprintf(ctx->output_file, "\tcall\t%s\n", prgm_name);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\txor\t%%ecx, %%ecx\n");
    else
        fprintf(ctx->output_file, "\txor\t%%edi, %%edi\n");
    fprintf(ctx->output_file, "\tcall\texit\n");
    codegen_function_footer("main", ctx);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Generates code to allocate needed stack space */
void codegen_stack_space(CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int needed_space;

    assert(ctx != NULL);

    needed_space = get_full_stack_offset();
    assert(needed_space >= 0);

    if(needed_space != 0)
    {
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", needed_space);
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Writes instruction list to file */
void codegen_inst_list(ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char *inst;

    assert(ctx != NULL);

    while(inst_list != NULL)
    {
        inst = (char *)inst_list->cur;
        assert(inst != NULL);

        fprintf(ctx->output_file, "%s", inst);

        inst_list = inst_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Returns the program name for use with main */
char * codegen_program(Tree_t *prgm, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(prgm->type == TREE_PROGRAM_TYPE);
    assert(ctx != NULL);
    assert(symtab != NULL);

    char *prgm_name;
    struct Program *data;
    ListNode_t *inst_list;

    data = &prgm->tree_data.program_data;
    prgm_name = data->program_id;

    push_stackscope();

    codegen_function_locals(data->var_declaration, ctx, symtab);
    codegen_subprograms(data->subprograms, ctx, symtab);

    inst_list = NULL;
    inst_list = codegen_var_initializers(data->var_declaration, inst_list, ctx, symtab);
    inst_list = codegen_stmt(data->body_statement, inst_list, ctx, symtab);

    codegen_function_header(prgm_name, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(prgm_name, ctx);
    free_inst_list(inst_list);

    pop_stackscope();

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return prgm_name;
}

/* Pushes function locals onto the stack */
void codegen_function_locals(ListNode_t *local_decl, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
     ListNode_t *cur, *id_list;
     Tree_t *tree;

    assert(ctx != NULL);

     cur = local_decl;

     while(cur != NULL)
     {
         tree = (Tree_t *)cur->cur;
         assert(tree != NULL);

         if (tree->type == TREE_VAR_DECL)
         {
             id_list = tree->tree_data.var_decl_data.ids;
             HashNode_t *type_node = NULL;
             if (symtab != NULL && tree->tree_data.var_decl_data.type_id != NULL)
                 FindIdent(&type_node, symtab, tree->tree_data.var_decl_data.type_id);

             if(tree->tree_data.var_decl_data.type == REAL_TYPE &&
                 (type_node == NULL || type_node->type_alias == NULL || !type_node->type_alias->is_array))
             {
                 fprintf(stderr, "Warning: REAL types not supported, treating as integer\n");
             }

             while(id_list != NULL)
             {
                 if (type_node != NULL && type_node->type_alias != NULL && type_node->type_alias->is_array)
                 {
                     struct TypeAlias *alias = type_node->type_alias;
                     int element_size = (type_node->var_type == HASHVAR_REAL) ? 8 : 4;
                     if (alias->is_open_array)
                     {
                         add_dynamic_array((char *)id_list->cur, element_size, alias->array_start);
                     }
                     else
                     {
                         int length = alias->array_end - alias->array_start + 1;
                         if (length < 0)
                             length = 0;
                         int total_size = length * element_size;
                         if (total_size <= 0)
                             total_size = element_size;
                         add_array((char *)id_list->cur, total_size, element_size, alias->array_start);
                     }
                }
                else
                {
                    int alloc_size = DOUBLEWORD;
                    enum VarType var_kind = HASHVAR_INTEGER;
                    if (symtab != NULL)
                    {
                        HashNode_t *var_info = NULL;
                        if (FindIdent(&var_info, symtab, (char *)id_list->cur) >= 0 && var_info != NULL)
                            var_kind = var_info->var_type;
                    }
                    if (type_node != NULL)
                        var_kind = type_node->var_type;

                    if (var_kind == HASHVAR_LONGINT || var_kind == HASHVAR_REAL || var_kind == HASHVAR_PCHAR)
                        alloc_size = 8;
                    else
                        alloc_size = DOUBLEWORD;

                    add_l_x((char *)id_list->cur, alloc_size);
                }
                id_list = id_list->next;
            };
        }
         else if (tree->type == TREE_ARR_DECL)
         {
             id_list = tree->tree_data.arr_decl_data.ids;
             int length = tree->tree_data.arr_decl_data.e_range - tree->tree_data.arr_decl_data.s_range + 1;
             if (length < 0)
                 length = 0;
             int total_size = length * DOUBLEWORD;
             if (total_size <= 0)
                 total_size = DOUBLEWORD;
             while (id_list != NULL)
             {
                 add_array((char *)id_list->cur, total_size, DOUBLEWORD, tree->tree_data.arr_decl_data.s_range);
                 id_list = id_list->next;
             }
         }

         cur = cur->next;
     }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Sets number of vector registers (floating points) before a function call */
ListNode_t *codegen_vect_reg(ListNode_t *inst_list, int num_vec)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char buffer[50];
    snprintf(buffer, 50, "\tmovl\t$%d, %%eax\n", num_vec);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return add_inst(inst_list, buffer);
}

/* Codegen for a list of subprograms */
void codegen_subprograms(ListNode_t *sub_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    Tree_t *sub;

    assert(ctx != NULL);
    assert(symtab != NULL);

    while(sub_list != NULL)
    {
        sub = (Tree_t *)sub_list->cur;
        assert(sub != NULL);
        assert(sub->type == TREE_SUBPROGRAM);

        if (sub->tree_data.subprogram_data.statement_list == NULL)
        {
            sub_list = sub_list->next;
            continue;
        }

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
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for a procedure */
void codegen_procedure(Tree_t *proc_tree, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
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
    codegen_function_locals(proc->declarations, ctx, symtab);

    codegen_subprograms(proc->subprograms, ctx, symtab);
    inst_list = codegen_var_initializers(proc->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(proc->statement_list, inst_list, ctx, symtab);
    codegen_function_header(sub_id, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(sub_id, ctx);
    free_inst_list(inst_list);
    pop_stackscope();
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for a function */
void codegen_function(Tree_t *func_tree, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
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
    int return_size = DOUBLEWORD;
    if (symtab != NULL)
    {
        HashNode_t *func_node = NULL;
        if (FindIdent(&func_node, symtab, func->id) >= 0 && func_node != NULL)
        {
            if (func_node->var_type == HASHVAR_LONGINT || func_node->var_type == HASHVAR_REAL || func_node->var_type == HASHVAR_PCHAR)
                return_size = 8;
            else if (func_node->var_type == HASHVAR_BOOLEAN)
                return_size = DOUBLEWORD;
        }
    }
    return_var = add_l_x(func->id, return_size);
    codegen_function_locals(func->declarations, ctx, symtab);

    codegen_subprograms(func->subprograms, ctx, symtab);
    inst_list = codegen_var_initializers(func->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(func->statement_list, inst_list, ctx, symtab);
    if (return_var->size >= 8)
        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_64);
    else
        snprintf(buffer, 50, "\tmovl\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_32);
    inst_list = add_inst(inst_list, buffer);
    codegen_function_header(sub_id, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(sub_id, ctx);
    free_inst_list(inst_list);
    pop_stackscope();
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for subprogram arguments */
ListNode_t *codegen_subprogram_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    Tree_t *arg_decl;
    int type;
    int arg_num = 0;
    ListNode_t *arg_ids;
    const char *arg_reg;
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
                while(arg_ids != NULL)
                {
                    int use_qword = (type == REAL_TYPE || type == STRING_TYPE || type == POINTER_TYPE ||
                        type == LONGINT_TYPE);
                    if (!use_qword && ctx->symtab != NULL && arg_decl->tree_data.var_decl_data.type_id != NULL)
                    {
                        HashNode_t *type_node = NULL;
                        if (FindIdent(&type_node, ctx->symtab, arg_decl->tree_data.var_decl_data.type_id) >= 0 &&
                            type_node != NULL && type_node->type_alias != NULL)
                        {
                            struct TypeAlias *alias = type_node->type_alias;
                            if (alias->base_type == STRING_TYPE || alias->is_pointer ||
                                alias->pointer_type == STRING_TYPE || alias->pointer_type == POINTER_TYPE)
                            {
                                use_qword = 1;
                            }
                        }
                    }
                    arg_reg = use_qword ? get_arg_reg64_num(arg_num) : get_arg_reg32_num(arg_num);
                    if(arg_reg == NULL)
                    {
                        fprintf(stderr, "ERROR: Max argument limit: %d\n", NUM_ARG_REG);
                        exit(1);
                    }
                    if (use_qword)
                        arg_stack = add_l_x((char *)arg_ids->cur, sizeof(void *));
                    else
                        arg_stack = add_l_z((char *)arg_ids->cur);
                    snprintf(buffer, 50, use_qword ? "\tmovq\t%s, -%d(%%rbp)\n" : "\tmovl\t%s, -%d(%%rbp)\n",
                        arg_reg, arg_stack->offset);
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
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(ctx != NULL);
    assert(symtab != NULL);
    while (decls != NULL)
    {
        Tree_t *decl = (Tree_t *)decls->cur;
        if (decl != NULL && decl->type == TREE_VAR_DECL)
        {
            HashNode_t *type_node = NULL;
            if (decl->tree_data.var_decl_data.type_id != NULL)
                FindIdent(&type_node, symtab, decl->tree_data.var_decl_data.type_id);

            if (type_node != NULL && type_node->type_alias != NULL &&
                type_node->type_alias->is_array && type_node->type_alias->is_open_array)
            {
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *array_node = find_label(var_name);
                    if (array_node != NULL && array_node->is_dynamic)
                    {
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", array_node->offset);
                        inst_list = add_inst(inst_list, buffer);
                        int length_offset = array_node->offset - 2 * DOUBLEWORD;
                        if (length_offset < array_node->offset)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", length_offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                    ids = ids->next;
                }
            }

            struct Statement *init_stmt = decl->tree_data.var_decl_data.initializer;
            if (init_stmt != NULL)
                inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
        }
        decls = decls->next;
    }
    return inst_list;
}
