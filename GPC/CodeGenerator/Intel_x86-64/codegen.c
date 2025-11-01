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
#include <limits.h>
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

static int align_to_multiple(int value, int alignment)
{
    if (alignment <= 0)
        return value;

    int remainder = value % alignment;
    if (remainder == 0)
        return value;

    return value + (alignment - remainder);
}

typedef struct StaticLinkInfo
{
    const char *mangled_name;
    int lexical_depth;
} StaticLinkInfo;

/* Helper: Increment lexical nesting depth when entering a procedure/function */
void codegen_enter_lexical_scope(CodeGenContext *ctx)
{
    if (ctx != NULL)
        ctx->lexical_depth++;
}

/* Helper: Decrement lexical nesting depth when leaving a procedure/function */
void codegen_leave_lexical_scope(CodeGenContext *ctx)
{
    if (ctx != NULL && ctx->lexical_depth > 0)
        ctx->lexical_depth--;
}

/* Helper: Get current lexical nesting depth */
int codegen_get_lexical_depth(const CodeGenContext *ctx)
{
    return (ctx != NULL) ? ctx->lexical_depth : 0;
}

/* Helper: Check if we're currently in a nested context (depth > 0) */
int codegen_is_nested_context(const CodeGenContext *ctx)
{
    return codegen_get_lexical_depth(ctx) > 0;
}

void codegen_register_static_link_proc(CodeGenContext *ctx, const char *mangled_name, int lexical_depth)
{
    if (ctx == NULL || mangled_name == NULL)
        return;

    if (codegen_proc_requires_static_link(ctx, mangled_name))
        return;

    StaticLinkInfo *info = (StaticLinkInfo *)malloc(sizeof(StaticLinkInfo));
    if (info == NULL)
        return;

    info->mangled_name = mangled_name;
    info->lexical_depth = lexical_depth;

    ListNode_t *entry = CreateListNode(info, LIST_UNSPECIFIED);
    if (ctx->static_link_procs == NULL)
        ctx->static_link_procs = entry;
    else
        ctx->static_link_procs = PushListNodeFront(ctx->static_link_procs, entry);
}

int codegen_proc_requires_static_link(const CodeGenContext *ctx, const char *mangled_name)
{
    if (ctx == NULL || mangled_name == NULL)
        return 0;

    ListNode_t *node = ctx->static_link_procs;
    while (node != NULL)
    {
        StaticLinkInfo *info = (StaticLinkInfo *)node->cur;
        if (info != NULL && info->mangled_name != NULL &&
            strcmp(info->mangled_name, mangled_name) == 0)
            return 1;
        node = node->next;
    }

    return 0;
}

int codegen_proc_static_link_depth(const CodeGenContext *ctx, const char *mangled_name, int *out_depth)
{
    if (ctx == NULL || mangled_name == NULL || out_depth == NULL)
        return 0;

    ListNode_t *node = ctx->static_link_procs;
    while (node != NULL)
    {
        StaticLinkInfo *info = (StaticLinkInfo *)node->cur;
        if (info != NULL && info->mangled_name != NULL &&
            strcmp(info->mangled_name, mangled_name) == 0)
        {
            *out_depth = info->lexical_depth;
            return 1;
        }
        node = node->next;
    }

    return 0;
}

static void codegen_reset_static_link_cache(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;

    if (ctx->static_link_reg != NULL)
    {
        free_reg(get_reg_stack(), ctx->static_link_reg);
        ctx->static_link_reg = NULL;
    }
    ctx->static_link_reg_level = 0;
}

static int codegen_find_static_link_offset(StackScope_t *scope, int *offset)
{
    if (scope == NULL || offset == NULL)
        return 0;

    ListNode_t *node = scope->x;
    while (node != NULL)
    {
        StackNode_t *stack_node = (StackNode_t *)node->cur;
        if (stack_node != NULL && stack_node->label != NULL &&
            strcmp(stack_node->label, "__static_link__") == 0)
        {
            *offset = stack_node->offset;
            return 1;
        }
        node = node->next;
    }

    return 0;
}

void codegen_begin_expression(CodeGenContext *ctx)
{
    codegen_reset_static_link_cache(ctx);
}

void codegen_end_expression(CodeGenContext *ctx)
{
    codegen_reset_static_link_cache(ctx);
}

Register_t *codegen_acquire_static_link(CodeGenContext *ctx, ListNode_t **inst_list,
    int levels_to_traverse)
{
    if (ctx == NULL || inst_list == NULL || levels_to_traverse <= 0)
        return NULL;

    if (ctx->static_link_reg != NULL)
    {
        if (ctx->static_link_reg_level == levels_to_traverse)
            return ctx->static_link_reg;

        free_reg(get_reg_stack(), ctx->static_link_reg);
        ctx->static_link_reg = NULL;
        ctx->static_link_reg_level = 0;
    }

    StackScope_t *scope = get_cur_scope();
    if (scope == NULL)
    {
        codegen_report_error(ctx, "ERROR: Unable to determine current scope for static link traversal.");
        return NULL;
    }

    int *offsets = (int *)calloc((size_t)levels_to_traverse, sizeof(int));
    if (offsets == NULL)
    {
        codegen_report_error(ctx, "ERROR: Failed to allocate static link traversal metadata.");
        return NULL;
    }

    StackScope_t *current_scope = scope;
    for (int i = 0; i < levels_to_traverse; ++i)
    {
        if (current_scope == NULL)
        {
            codegen_report_error(ctx, "ERROR: Static link chain shorter than requested depth.");
            free(offsets);
            return NULL;
        }

        if (!codegen_find_static_link_offset(current_scope, &offsets[i]))
        {
            codegen_report_error(ctx, "ERROR: Static link slot missing at depth %d.", i);
            free(offsets);
            return NULL;
        }

        current_scope = current_scope->prev_scope;
    }

    Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
    if (reg == NULL)
    {
        free(offsets);
        return NULL;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n", offsets[0], reg->bit_64);
    *inst_list = add_inst(*inst_list, buffer);

    for (int i = 1; i < levels_to_traverse; ++i)
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%s), %s\n", offsets[i],
            reg->bit_64, reg->bit_64);
        *inst_list = add_inst(*inst_list, buffer);
    }

    free(offsets);

    ctx->static_link_reg = reg;
    ctx->static_link_reg_level = levels_to_traverse;
    return reg;
}

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
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    fprintf(ctx->output_file, ".format_str_s:\n");
    fprintf(ctx->output_file, ".string \"%%s\"\n");
    fprintf(ctx->output_file, ".format_str_d:\n");
    fprintf(ctx->output_file, ".string \"%%d\"\n");
    fprintf(ctx->output_file, ".format_str_lld:\n");
    if (codegen_target_is_windows())
    {
        fprintf(ctx->output_file, ".string \"%%lld\"\n");
    }
    else
    {
        fprintf(ctx->output_file, ".string \"%%ld\"\n");
    }
  
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
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

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
    {
        /* The COFF/PE assembler does not support .ident; omit it on Windows. */
    }
    else
    {
        fprintf(ctx->output_file, "\t.section\t.comment\n");
        fprintf(ctx->output_file, "\t.string\t\"GPC: 0.0.0\"\n");
        fprintf(ctx->output_file, "\t.section\t.note.GNU-stack,\"\",@progbits\n");
    }
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

    int aligned_space = align_to_multiple(needed_space, REQUIRED_OFFSET);

    if(aligned_space != 0)
    {
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", aligned_space);
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

            while(id_list != NULL)
            {
                if (type_node != NULL && type_node->type_alias != NULL && type_node->type_alias->is_array)
                {
                    struct TypeAlias *alias = type_node->type_alias;
                    long long computed_size = 0;
                    int element_size = 0;
                    struct RecordType *element_record = NULL;

                    if (alias->array_element_type == RECORD_TYPE && ctx != NULL && ctx->symtab != NULL &&
                        alias->array_element_type_id != NULL)
                    {
                        HashNode_t *element_node = NULL;
                        if (FindIdent(&element_node, ctx->symtab, alias->array_element_type_id) >= 0 &&
                            element_node != NULL)
                            element_record = element_node->record_type;
                    }

                    if (codegen_sizeof_type_reference(ctx, alias->array_element_type,
                            alias->array_element_type_id, element_record, &computed_size) == 0 &&
                        computed_size > 0 && computed_size <= INT_MAX)
                    {
                        element_size = (int)computed_size;
                    }

                    if (element_size <= 0)
                    {
                        if (type_node->var_type == HASHVAR_REAL || type_node->var_type == HASHVAR_LONGINT)
                            element_size = 8;
                        else
                            element_size = 4;
                    }

                    if (alias->is_open_array)
                    {
                        add_dynamic_array((char *)id_list->cur, element_size, alias->array_start);
                    }
                    else
                    {
                        int length = alias->array_end - alias->array_start + 1;
                        if (length < 0)
                            length = 0;
                        long long total_size = (long long)length * (long long)element_size;
                        if (total_size <= 0)
                            total_size = element_size;
                        add_array((char *)id_list->cur, (int)total_size, element_size, alias->array_start);
                    }
                }
                else
                {
                    int alloc_size = DOUBLEWORD;
                    enum VarType var_kind = HASHVAR_INTEGER;
                    HashNode_t *var_info = NULL;
                    if (symtab != NULL)
                    {
                        if (FindIdent(&var_info, symtab, (char *)id_list->cur) >= 0 && var_info != NULL)
                            var_kind = var_info->var_type;
                    }
                    if (type_node != NULL)
                        var_kind = type_node->var_type;

                    if (var_kind == HASHVAR_LONGINT || var_kind == HASHVAR_REAL ||
                        var_kind == HASHVAR_PCHAR || var_kind == HASHVAR_POINTER ||
                        var_kind == HASHVAR_FILE)
                    {
                        alloc_size = 8;
                    }
                    else if (var_kind == HASHVAR_RECORD)
                    {
                        struct RecordType *record_desc = NULL;
                        if (var_info != NULL && var_info->record_type != NULL)
                            record_desc = var_info->record_type;
                        else if (type_node != NULL && type_node->record_type != NULL)
                            record_desc = type_node->record_type;

                        long long record_size = 0;
                        if (record_desc != NULL &&
                            codegen_sizeof_record_type(ctx, record_desc, &record_size) == 0 &&
                            record_size > 0)
                        {
                            alloc_size = (int)record_size;
                        }
                        else
                        {
                            alloc_size = DOUBLEWORD;
                        }
                    }
                    else
                    {
                        alloc_size = DOUBLEWORD;
                    }

                    add_l_x((char *)id_list->cur, alloc_size);
                }
                id_list = id_list->next;
            };
        }
        else if (tree->type == TREE_ARR_DECL)
        {
            struct Array *arr = &tree->tree_data.arr_decl_data;
            id_list = arr->ids;

            int is_dynamic = (arr->e_range < arr->s_range);

            HashNode_t *type_node = NULL;
            if (arr->type_id != NULL && symtab != NULL)
                FindIdent(&type_node, symtab, arr->type_id);

            enum VarType arr_var_type = HASHVAR_REAL;
            if (type_node != NULL)
                arr_var_type = type_node->var_type;
            else if (arr->type == INT_TYPE)
                arr_var_type = HASHVAR_INTEGER;
            else if (arr->type == LONGINT_TYPE)
                arr_var_type = HASHVAR_LONGINT;
            else if (arr->type == BOOL)
                arr_var_type = HASHVAR_BOOLEAN;
            else if (arr->type == STRING_TYPE)
                arr_var_type = HASHVAR_PCHAR;
            else if (arr->type == CHAR_TYPE)
                arr_var_type = HASHVAR_CHAR;

            struct RecordType *record_desc = NULL;
            if (type_node != NULL)
            {
                if (type_node->record_type != NULL)
                    record_desc = type_node->record_type;
                else if (type_node->type_alias != NULL &&
                    type_node->type_alias->target_type_id != NULL)
                {
                    HashNode_t *target_node = NULL;
                    if (FindIdent(&target_node, symtab,
                            type_node->type_alias->target_type_id) >= 0 &&
                        target_node != NULL && target_node->record_type != NULL)
                        record_desc = target_node->record_type;
                }
            }

            long long computed_size = 0;
            int element_size = 0;
            if (codegen_sizeof_type_reference(ctx, arr->type, arr->type_id,
                    record_desc, &computed_size) == 0 && computed_size > 0 &&
                computed_size <= INT_MAX)
            {
                element_size = (int)computed_size;
            }
            else if (record_desc != NULL &&
                codegen_sizeof_record_type(ctx, record_desc, &computed_size) == 0 &&
                computed_size > 0 && computed_size <= INT_MAX)
            {
                element_size = (int)computed_size;
            }

            if (element_size <= 0)
            {
                if (arr_var_type == HASHVAR_REAL || arr_var_type == HASHVAR_LONGINT ||
                    arr_var_type == HASHVAR_PCHAR || arr_var_type == HASHVAR_POINTER ||
                    arr_var_type == HASHVAR_PROCEDURE || arr_var_type == HASHVAR_FILE)
                    element_size = 8;
                else if (arr_var_type == HASHVAR_BOOLEAN || arr_var_type == HASHVAR_CHAR)
                    element_size = 1;
                else
                    element_size = DOUBLEWORD;
            }

            if (is_dynamic)
            {
                while (id_list != NULL)
                {
                    add_dynamic_array((char *)id_list->cur, element_size, arr->s_range);
                    id_list = id_list->next;
                }
            }
            else
            {
                int length = arr->e_range - arr->s_range + 1;
                if (length < 0)
                    length = 0;
                int total_size = length * element_size;
                if (total_size <= 0)
                    total_size = element_size;

                if (arr->has_static_storage)
                {
                    if (!arr->static_storage_emitted)
                    {
                        if (arr->static_label != NULL)
                            fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                arr->static_label, total_size, DOUBLEWORD);
                        if (arr->init_guard_label != NULL)
                            fprintf(ctx->output_file, "\t.comm\t%s,1,1\n",
                                arr->init_guard_label);
                        arr->static_storage_emitted = 1;
                    }

                    while (id_list != NULL)
                    {
                        add_static_array((char *)id_list->cur, total_size, element_size,
                            arr->s_range, arr->static_label);
                        id_list = id_list->next;
                    }
                }
                else
                {
                    while (id_list != NULL)
                    {
                        add_array((char *)id_list->cur, total_size, element_size,
                            arr->s_range);
                        id_list = id_list->next;
                    }
                }
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

    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;

    /* Enter a new lexical scope */
    codegen_enter_lexical_scope(ctx);
    push_stackscope();
    inst_list = NULL;

    /* For now, only support static links for procedures without parameters.
     * Supporting parameters requires a more complex calling convention.
     * Check argument count first to decide whether to set up static link. */
    int num_args = (proc->args_var == NULL) ? 0 : ListLength(proc->args_var);
    ctx->current_subprogram_id = proc->id;
    ctx->current_subprogram_mangled = sub_id;
    int is_nested = codegen_is_nested_context(ctx);
    StackNode_t *static_link = NULL;

    if (is_nested && num_args == 0)
    {
        /* Reserve space for static link (parent's frame pointer) as first local variable
         * This ensures it's at a predictable offset regardless of other locals */
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, sub_id, codegen_get_lexical_depth(ctx));
    }

    inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, ctx, symtab);
    
    if (static_link != NULL)
    {
        char buffer[64];
        /* No parameters, static link comes in %rdi */
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rdi, -%d(%%rbp)\n", static_link->offset);
        inst_list = add_inst(inst_list, buffer);
    }
    
    codegen_function_locals(proc->declarations, ctx, symtab);

    /* Recursively generate nested subprograms */
    codegen_subprograms(proc->subprograms, ctx, symtab);
    
    inst_list = codegen_var_initializers(proc->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(proc->statement_list, inst_list, ctx, symtab);
    codegen_function_header(sub_id, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(sub_id, ctx);
    free_inst_list(inst_list);
    pop_stackscope();

    ctx->current_subprogram_id = prev_sub_id;
    ctx->current_subprogram_mangled = prev_sub_mangled;

    /* Leave the lexical scope */
    codegen_leave_lexical_scope(ctx);
    
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

    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;

    /* Enter a new lexical scope */
    codegen_enter_lexical_scope(ctx);
    push_stackscope();
    inst_list = NULL;

    /* For now, only support static links for functions without parameters.
     * Supporting parameters requires a more complex calling convention. */
    int num_args = (func->args_var == NULL) ? 0 : ListLength(func->args_var);
    ctx->current_subprogram_id = func->id;
    ctx->current_subprogram_mangled = sub_id;
    int is_nested = codegen_is_nested_context(ctx);
    StackNode_t *static_link = NULL;

    if (is_nested && num_args == 0)
    {
        /* Reserve space for static link as first local variable */
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, sub_id, codegen_get_lexical_depth(ctx));
    }
    
    inst_list = codegen_subprogram_arguments(func->args_var, inst_list, ctx, symtab);
    
    if (static_link != NULL)
    {
        char link_buffer[64];
        /* No parameters, static link comes in %rdi */
        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%%rdi, -%d(%%rbp)\n", static_link->offset);
        inst_list = add_inst(inst_list, link_buffer);
    }
    
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

    /* Recursively generate nested subprograms */
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

    ctx->current_subprogram_id = prev_sub_id;
    ctx->current_subprogram_mangled = prev_sub_mangled;

    /* Leave the lexical scope */
    codegen_leave_lexical_scope(ctx);
    
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Code generation for subprogram arguments */
ListNode_t *codegen_subprogram_arguments(ListNode_t *args, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab)
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
                HashNode_t *resolved_type_node = NULL;

                // Resolve type aliases if needed
                if (type == UNKNOWN_TYPE && arg_decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL)
                {
                    HashNode_t *type_node = NULL;
                    FindIdent(&type_node, symtab, arg_decl->tree_data.var_decl_data.type_id);
                    if (type_node != NULL && type_node->type_alias != NULL)
                    {
                        type = type_node->type_alias->base_type;
                        resolved_type_node = type_node;
                    }
                    else if (type_node != NULL)
                    {
                        resolved_type_node = type_node;
                    }
                }

                while(arg_ids != NULL)
                {
                    int tree_is_var_param = arg_decl->tree_data.var_decl_data.is_var_param;
                    HashNode_t *arg_symbol = NULL;
                    if (symtab != NULL)
                        FindIdent(&arg_symbol, symtab, (char *)arg_ids->cur);

                    int symbol_is_var_param = tree_is_var_param;
                    if (arg_symbol != NULL && arg_symbol->is_var_parameter)
                        symbol_is_var_param = 1;
                    struct RecordType *record_type_info = NULL;
                    if (!symbol_is_var_param)
                    {
                        if (arg_symbol != NULL && arg_symbol->record_type != NULL)
                            record_type_info = arg_symbol->record_type;
                        else if (resolved_type_node != NULL && resolved_type_node->record_type != NULL)
                            record_type_info = resolved_type_node->record_type;
                    }

                    if (record_type_info != NULL)
                    {
                        long long record_size = 0;
                        if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                                record_type_info, &record_size) != 0 || record_size <= 0)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to determine size for record parameter %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        if (record_size > INT_MAX)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Record parameter %s exceeds supported size limits.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        StackNode_t *record_slot = add_l_x((char *)arg_ids->cur, (int)record_size);
                        if (record_slot == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Failed to allocate storage for record parameter %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }

                        arg_reg = get_arg_reg64_num(arg_num);
                        if (arg_reg == NULL)
                        {
                            fprintf(stderr, "ERROR: Max argument limit: %d\n", NUM_ARG_REG);
                            exit(1);
                        }

                        Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (size_reg == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate register for record parameter size.");
                            return inst_list;
                        }

                        snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", record_size, size_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);

                        if (codegen_target_is_windows())
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", record_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", size_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", arg_reg);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rdi\n", record_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                        }

                        inst_list = codegen_vect_reg(inst_list, 0);
                        inst_list = add_inst(inst_list, "\tcall\tgpc_move\n");
                        free_arg_regs();
                        free_reg(get_reg_stack(), size_reg);

                        arg_ids = arg_ids->next;
                        ++arg_num;
                        continue;
                    }

                    // Var parameters are passed by reference (as pointers), so always use 64-bit
                    // Also use 64-bit for strings and explicit pointers
                    int is_var_param = symbol_is_var_param;
                    int use_64bit = is_var_param ||
                        (type == STRING_TYPE || type == POINTER_TYPE ||
                         type == REAL_TYPE || type == LONGINT_TYPE);
                    arg_reg = use_64bit ? get_arg_reg64_num(arg_num) : get_arg_reg32_num(arg_num);
                    if(arg_reg == NULL)
                    {
                        fprintf(stderr, "ERROR: Max argument limit: %d\n", NUM_ARG_REG);
                        exit(1);
                    }
                    arg_stack = use_64bit ? add_q_z((char *)arg_ids->cur) : add_l_z((char *)arg_ids->cur);
                    if (arg_stack != NULL && symbol_is_var_param)
                        arg_stack->is_reference = 1;
                    if (use_64bit)
                        snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", arg_reg, arg_stack->offset);
                    else
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
        if (decl == NULL)
        {
            decls = decls->next;
            continue;
        }

        if (decl->type == TREE_VAR_DECL)
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

            /* Initialize FILE variables to NULL */
            if (type_node != NULL && type_node->var_type == HASHVAR_FILE)
            {
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *file_node = find_label(var_name);
                    if (file_node != NULL)
                    {
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", file_node->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    ids = ids->next;
                }
            }

            struct Statement *init_stmt = decl->tree_data.var_decl_data.initializer;
            if (init_stmt != NULL)
                inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
        }
        else if (decl->type == TREE_ARR_DECL)
        {
            struct Array *arr = &decl->tree_data.arr_decl_data;
            if (arr->e_range < arr->s_range)
            {
                ListNode_t *ids = arr->ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *array_node = find_label(var_name);
                    if (array_node != NULL && array_node->is_dynamic)
                    {
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                            array_node->offset);
                        inst_list = add_inst(inst_list, buffer);
                        int length_offset = array_node->offset - 2 * DOUBLEWORD;
                        if (length_offset < array_node->offset)
                        {
                            snprintf(buffer, sizeof(buffer),
                                "\tmovq\t$0, -%d(%%rbp)\n", length_offset);
                            inst_list = add_inst(inst_list, buffer);
                        }
                    }
                    ids = ids->next;
                }
            }
            struct Statement *init_stmt = arr->initializer;
            if (init_stmt != NULL)
            {
                if (arr->is_typed_const && arr->init_guard_label != NULL)
                {
                    char done_label[64];
                    gen_label(done_label, sizeof(done_label), ctx);

                    char buffer[128];
                    snprintf(buffer, sizeof(buffer), "\tmovb\t%s(%%rip), %%al\n", arr->init_guard_label);
                    inst_list = add_inst(inst_list, buffer);
                    inst_list = add_inst(inst_list, "\ttestb\t%al, %al\n");
                    snprintf(buffer, sizeof(buffer), "\tjne\t%s\n", done_label);
                    inst_list = add_inst(inst_list, buffer);

                    inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);

                    snprintf(buffer, sizeof(buffer), "\tmovb\t$1, %s(%%rip)\n", arr->init_guard_label);
                    inst_list = add_inst(inst_list, buffer);

                    char label_decl[96];
                    snprintf(label_decl, sizeof(label_decl), "%s:\n", done_label);
                    inst_list = add_inst(inst_list, label_decl);
                }
                else
                {
                    inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
                }
            }
        }
        decls = decls->next;
    }
    return inst_list;
}
