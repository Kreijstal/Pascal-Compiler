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
#include "../../Parser/ParseTree/GpcType.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"

/* Helper functions for transitioning from legacy type fields to GpcType */

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Helper function to get the primitive type tag from a node */
static inline int get_primitive_tag_from_node(HashNode_t *node)
{
    if (node == NULL)
        return -1;
    
    /* GpcType should be present for typed nodes */
    if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE)
    {
        return gpc_type_get_primitive_tag(node->type);
    }
    
    /* Use hashnode helper to get var_type, then map to primitive tags */
    enum VarType var_type = hashnode_get_var_type(node);
    switch (var_type)
    {
        case HASHVAR_INTEGER: return INT_TYPE;
        case HASHVAR_LONGINT: return LONGINT_TYPE;
        case HASHVAR_REAL: return REAL_TYPE;
        case HASHVAR_BOOLEAN: return BOOL;
        case HASHVAR_CHAR: return CHAR_TYPE;
        case HASHVAR_PCHAR: return STRING_TYPE;
        case HASHVAR_SET: return SET_TYPE;
        case HASHVAR_FILE: return FILE_TYPE;
        case HASHVAR_ENUM: return ENUM_TYPE;
        default: return UNKNOWN_TYPE;
    }
}

/* Helper function to check if a node is a file type */
static inline int node_is_file_type(HashNode_t *node)
{
    if (node == NULL)
        return 0;
    
    /* Check GpcType if present */
    if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE)
    {
        return gpc_type_get_primitive_tag(node->type) == FILE_TYPE;
    }
    
    /* Use hashnode helper */
    return hashnode_get_var_type(node) == HASHVAR_FILE;
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

/* Helper function to determine variable storage size (for stack allocation)
 * Returns size in bytes, or -1 on error */
static inline int get_var_storage_size(HashNode_t *node)
{
    if (node == NULL)
        return -1;
    
    /* Check GpcType first */
    if (node->type != NULL)
    {
        if (node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            int tag = gpc_type_get_primitive_tag(node->type);
            switch (tag)
            {
                case LONGINT_TYPE:
                case REAL_TYPE:
                case STRING_TYPE:  /* PCHAR */
                case FILE_TYPE:
                    return 8;
                case SET_TYPE:
                {
                    /* Check if this is a character set */
                    long long size = gpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return DOUBLEWORD;  /* Default for non-char sets */
                }
                default:
                    return DOUBLEWORD;  /* 4 bytes for most primitives */
            }
        }
        else if (node->type->kind == TYPE_KIND_POINTER)
        {
            return 8;
        }
        else if (node->type->kind == TYPE_KIND_PROCEDURE)
        {
            return 8;  /* Function pointers are 8 bytes */
        }
        else if (node->type->kind == TYPE_KIND_RECORD || node->type->kind == TYPE_KIND_ARRAY)
        {
            /* These should use sizeof, not this helper */
            return -1;
        }
    }
    
    /* Fallback using hashnode helper */
    enum VarType var_kind = hashnode_get_var_type(node);
    if (var_kind == HASHVAR_LONGINT || var_kind == HASHVAR_REAL ||
        var_kind == HASHVAR_PCHAR || var_kind == HASHVAR_POINTER ||
        var_kind == HASHVAR_FILE || var_kind == HASHVAR_PROCEDURE)
    {
        return 8;
    }
    
    return DOUBLEWORD;
}

ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);
void codegen_vmt(CodeGenContext *ctx, SymTab_t *symtab, Tree_t *tree);

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
    codegen_vmt(ctx, symtab, tree);

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
    fprintf(ctx->output_file, ".format_str_c:\n");
    fprintf(ctx->output_file, ".string \"%%c\"\n");
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

/* Generate Virtual Method Tables (VMT) for classes with virtual methods */
void codegen_vmt(CodeGenContext *ctx, SymTab_t *symtab, Tree_t *tree)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    assert(symtab != NULL);
    assert(tree != NULL);
    
    if (tree->type != TREE_PROGRAM_TYPE)
        return;
    
    ListNode_t *type_decls = tree->tree_data.program_data.type_declaration;
    if (type_decls == NULL)
        return;
    
    /* VMTs are generated as read-only data structures */
    fprintf(ctx->output_file, "\n");
    fprintf(ctx->output_file, "# Virtual Method Tables (VMT)\n");
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    
    /* Iterate through all type declarations */
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL) {
            /* Check if this is a record/class type with methods */
            if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
                struct RecordType *record_info = type_tree->tree_data.type_decl_data.info.record;
                const char *type_name = type_tree->tree_data.type_decl_data.id;
                
                /* Generate VMT if this class has virtual methods */
                if (record_info != NULL && record_info->methods != NULL && type_name != NULL) {
                    fprintf(ctx->output_file, "\n# VMT for class %s\n", type_name);
                    fprintf(ctx->output_file, "\t.align 8\n");
                    fprintf(ctx->output_file, ".globl %s_VMT\n", type_name);
                    fprintf(ctx->output_file, "%s_VMT:\n", type_name);
                    
                    /* Generate VMT entries for each method */
                    ListNode_t *method_node = record_info->methods;
                    while (method_node != NULL) {
                        struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
                        if (method != NULL && method->mangled_name != NULL) {
                            /* Each VMT entry is a pointer to the method */
                            fprintf(ctx->output_file, "\t.quad\t%s_u\n", method->mangled_name);
                        }
                        method_node = method_node->next;
                    }
                }
            }
        }
        cur = cur->next;
    }
    
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

    const char *prev_id = ctx->current_subprogram_id;
    const char *prev_mangled = ctx->current_subprogram_mangled;
    ctx->current_subprogram_id = prgm_name;
    ctx->current_subprogram_mangled = prgm_name;

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

    ctx->current_subprogram_id = prev_id;
    ctx->current_subprogram_mangled = prev_mangled;

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
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (alias != NULL && alias->is_array)
                {
                    long long computed_size = 0;
                    int element_size = 0;
                    struct RecordType *element_record = NULL;

                    if (alias->array_element_type == RECORD_TYPE && ctx != NULL && ctx->symtab != NULL &&
                        alias->array_element_type_id != NULL)
                    {
                        HashNode_t *element_node = NULL;
                        if (FindIdent(&element_node, ctx->symtab, alias->array_element_type_id) >= 0 &&
                            element_node != NULL)
                            element_record = get_record_type_from_node(element_node);
                    }

                    if (codegen_sizeof_type_reference(ctx, alias->array_element_type,
                            alias->array_element_type_id, element_record, &computed_size) == 0 &&
                        computed_size > 0 && computed_size <= INT_MAX)
                    {
                        element_size = (int)computed_size;
                    }

                    if (element_size <= 0)
                    {
                        int prim_tag = get_primitive_tag_from_node(type_node);
                        if (prim_tag == REAL_TYPE || prim_tag == LONGINT_TYPE)
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
                    HashNode_t *var_info = NULL;
                    HashNode_t *size_node = NULL;  /* Node to get size from */
                    
                    if (symtab != NULL)
                    {
                        if (FindIdent(&var_info, symtab, (char *)id_list->cur) >= 0 && var_info != NULL)
                            size_node = var_info;
                    }
                    /* Use type_node if we don't have specific var_info */
                    if (size_node == NULL && type_node != NULL)
                        size_node = type_node;
                    
                    /* Get allocation size using helper */
                    if (size_node != NULL)
                    {
                        int size = get_var_storage_size(size_node);
                        if (size > 0)
                        {
                            alloc_size = size;
                        }
                        else if (node_is_record_type(size_node))
                        {
                            /* For records, get the full struct size */
                            struct RecordType *record_desc = get_record_type_from_node(size_node);
                            long long record_size = 0;
                            if (record_desc != NULL &&
                                codegen_sizeof_record_type(ctx, record_desc, &record_size) == 0 &&
                                record_size > 0)
                            {
                                alloc_size = (int)record_size;
                            }
                        }
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

            struct RecordType *record_desc = NULL;
            if (type_node != NULL)
            {
                record_desc = get_record_type_from_node(type_node);
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (record_desc == NULL && alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node = NULL;
                    if (FindIdent(&target_node, symtab, alias->target_type_id) >= 0 &&
                        target_node != NULL)
                        record_desc = get_record_type_from_node(target_node);
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
                /* Fallback: determine element size from type */
                if (type_node != NULL)
                {
                    int size = get_var_storage_size(type_node);
                    if (size > 0)
                        element_size = size;
                    else
                        element_size = DOUBLEWORD;
                }
                else
                {
                    /* Use arr->type to determine element size */
                    switch (arr->type)
                    {
                        case LONGINT_TYPE:
                        case REAL_TYPE:
                        case STRING_TYPE:
                        case FILE_TYPE:
                            element_size = 8;
                            break;
                        case BOOL:
                        case CHAR_TYPE:
                            element_size = 1;
                            break;
                        default:
                            element_size = DOUBLEWORD;
                            break;
                    }
                }
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

    /* Static links are supported for nested procedures/functions (depth >= 1), but NOT for:
     * - Top-level procedures (depth 0)
     * - Class methods (which have __ in their mangled name)
     * 
     * Class methods receive 'self' in the first register and should not use static links.
     * When there are parameters, the static link is passed in %rdi and all arguments
     * are shifted by one register position. */
    int num_args = (proc->args_var == NULL) ? 0 : ListLength(proc->args_var);
    ctx->current_subprogram_id = proc->id;
    ctx->current_subprogram_mangled = sub_id;
    int lexical_depth = codegen_get_lexical_depth(ctx);
    int is_nested = (lexical_depth >= 1);
    int is_class_method = (sub_id != NULL && strstr(sub_id, "__") != NULL);
    StackNode_t *static_link = NULL;

    /* Process arguments first to allocate their stack space */
    /* Check if this procedure will need a static link:
     * - Depth >= 2: always need static link (truly nested)
     * - Depth == 1 with nested subprograms: need static link to pass to children
     * - Depth == 1 with no params: need static link to access parent (main) variables
     * - Depth == 1 with params but no nested subprograms: DON'T need static link
     */
    int has_nested_subprograms = (proc->subprograms != NULL && ListLength(proc->subprograms) > 0);
    int will_need_static_link = (is_nested && !is_class_method && 
                                (lexical_depth >= 2 || has_nested_subprograms || num_args == 0));
    
    /* If there are arguments and we'll need a static link, shift argument registers by 1 */
    int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;
    inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, ctx, symtab, arg_start_index);
    
    /* Now add static link after arguments to avoid overlap */
    /* Static links are needed for:
     * 1. Depth >= 2 (truly nested procedures), OR
     * 2. Depth == 1 procedures that have nested subprograms (they need to pass their frame to children)
     */
    if (will_need_static_link)
    {
        /* Reserve space for static link (parent's frame pointer) after arguments
         * This ensures it doesn't overlap with argument storage */
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, sub_id, lexical_depth);
    }
    
    if (static_link != NULL)
    {
        char buffer[64];
        /* Static link always comes in the first argument register (platform-dependent) */
        const char *link_reg = current_arg_reg64(0);
        assert(link_reg != NULL && "current_arg_reg64(0) should never return NULL");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", link_reg, static_link->offset);
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
    StackNode_t *return_dest_slot = NULL;
    int has_record_return = 0;
    long long record_return_size = 0;

    func = &func_tree->tree_data.subprogram_data;
    sub_id = (func->mangled_id != NULL) ? func->mangled_id : func->id;

    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;

    /* Enter a new lexical scope */
    codegen_enter_lexical_scope(ctx);
    push_stackscope();
    inst_list = NULL;

    /* Static links are supported for nested functions (depth >= 1), but NOT for:
     * - Top-level functions (depth 0)
     * - Class methods (which have __ in their mangled name)
     * 
     * Class methods receive 'self' in the first register and should not use static links.
     * When there are parameters, the static link is passed in %rdi (or second register
     * if function returns a record) and all arguments are shifted accordingly. */
    int num_args = (func->args_var == NULL) ? 0 : ListLength(func->args_var);
    ctx->current_subprogram_id = func->id;
    ctx->current_subprogram_mangled = sub_id;
    int lexical_depth = codegen_get_lexical_depth(ctx);
    int is_nested = (lexical_depth >= 1);
    int is_class_method = (sub_id != NULL && strstr(sub_id, "__") != NULL);
    StackNode_t *static_link = NULL;

    HashNode_t *func_node = NULL;
    if (symtab != NULL)
        FindIdent(&func_node, symtab, func->id);

    /* Check if function returns a record by examining GpcType */
    if (func_node != NULL && func_node->type != NULL &&
        func_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        GpcType *return_type = gpc_type_get_return_type(func_node->type);
        if (return_type != NULL && gpc_type_is_record(return_type))
        {
            struct RecordType *record_desc = gpc_type_get_record(return_type);
            if (record_desc != NULL &&
                codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record_desc,
                    &record_return_size) == 0 && record_return_size > 0 &&
                record_return_size <= INT_MAX)
            {
                has_record_return = 1;
            }
            else
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine size for record return value of %s.", func->id);
                record_return_size = 0;
            }
        }
    }
    else if (func_node != NULL && hashnode_get_var_type(func_node) == HASHVAR_RECORD)
    {
        struct RecordType *record = hashnode_get_record_type(func_node);
        if (record != NULL)
        {
            /* Get size from record */
            if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record,
                    &record_return_size) != 0 || record_return_size <= 0 ||
                record_return_size > INT_MAX)
            {
                codegen_report_error(ctx,
                    "ERROR: Unable to determine size for record return value of %s.", func->id);
                record_return_size = 0;
            }
            else
            {
                has_record_return = 1;
            }
        }
    }

    /* Check if this function will need a static link:
     * - Depth >= 2: always need static link (truly nested)
     * - Depth == 1 with nested subprograms: need static link to pass to children
     * - Depth == 1 with no params: need static link to access parent (main) variables
     * - Depth == 1 with params but no nested subprograms: DON'T need static link
     */
    int has_nested_subprograms = (func->subprograms != NULL && ListLength(func->subprograms) > 0);
    int will_need_static_link = (is_nested && !is_class_method && 
                                (lexical_depth >= 2 || has_nested_subprograms || num_args == 0));
    
    /* Calculate argument start index:
     * - If function returns record: use index 1 (record pointer in first arg)
     * - If function will need static link: add 1 for static link
     * - Otherwise: use index 0 */
    int arg_start_index = has_record_return ? 1 : 0;
    if (will_need_static_link && num_args > 0)
        arg_start_index++;
    
    inst_list = codegen_subprogram_arguments(func->args_var, inst_list, ctx, symtab,
        arg_start_index);
    
    /* Add static link after arguments to avoid stack overlap */
    if (will_need_static_link)
    {
        /* Reserve space for static link after arguments */
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, sub_id, lexical_depth);
    }
    
    if (static_link != NULL)
    {
        char link_buffer[64];
        /* Static link comes in the register right after the record return pointer (if any) */
        const char *link_reg = current_arg_reg64(has_record_return ? 1 : 0);
        if (link_reg == NULL)
            link_reg = "%rdi";
        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            link_reg, static_link->offset);
        inst_list = add_inst(inst_list, link_buffer);
    }
    
    int return_size = DOUBLEWORD;
    if (has_record_return)
        return_size = (int)record_return_size;
    else if (func_node != NULL && func_node->type != NULL &&
             func_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        /* Get return type from GpcType */
        GpcType *return_type = gpc_type_get_return_type(func_node->type);
        if (return_type != NULL && return_type->kind == TYPE_KIND_PRIMITIVE)
        {
            int tag = gpc_type_get_primitive_tag(return_type);
            switch (tag)
            {
                case LONGINT_TYPE:
                case REAL_TYPE:
                case STRING_TYPE:  /* PCHAR */
                    return_size = 8;
                    break;
                case BOOL:
                    return_size = DOUBLEWORD;
                    break;
                default:
                    return_size = DOUBLEWORD;
                    break;
            }
        }
    }
    else if (func_node != NULL)
    {
        /* Use hashnode helper to get var_type */
        enum VarType var_type = hashnode_get_var_type(func_node);
        if (var_type == HASHVAR_LONGINT || var_type == HASHVAR_REAL ||
            var_type == HASHVAR_PCHAR)
            return_size = 8;
        else if (var_type == HASHVAR_BOOLEAN)
            return_size = DOUBLEWORD;
    }

    return_var = add_l_x(func->id, return_size);

    if (has_record_return)
        return_dest_slot = add_l_x("__record_return_dest__", (int)sizeof(void *));

    if (has_record_return && return_dest_slot != NULL)
    {
        const char *ret_reg = current_arg_reg64(0);
        if (ret_reg != NULL)
        {
            char ptr_buffer[64];
            snprintf(ptr_buffer, sizeof(ptr_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                ret_reg, return_dest_slot->offset);
            inst_list = add_inst(inst_list, ptr_buffer);
        }
    }

    codegen_function_locals(func->declarations, ctx, symtab);

    /* Recursively generate nested subprograms */
    codegen_subprograms(func->subprograms, ctx, symtab);
    
    inst_list = codegen_var_initializers(func->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(func->statement_list, inst_list, ctx, symtab);
    if (has_record_return && return_dest_slot != NULL && record_return_size > 0)
    {
        Register_t *dest_reg = get_free_reg(get_reg_stack(), &inst_list);
        Register_t *src_reg = get_free_reg(get_reg_stack(), &inst_list);
        Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (dest_reg == NULL || src_reg == NULL || size_reg == NULL)
        {
            if (dest_reg != NULL)
                free_reg(get_reg_stack(), dest_reg);
            if (src_reg != NULL)
                free_reg(get_reg_stack(), src_reg);
            if (size_reg != NULL)
                free_reg(get_reg_stack(), size_reg);
            codegen_report_error(ctx,
                "ERROR: Unable to allocate registers for record return copy.");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                return_dest_slot->offset, dest_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                return_var->offset, src_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n",
                record_return_size, size_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                /* Move in reverse order to avoid register conflicts when temp regs overlap with arg regs */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                /* Move in reverse order to avoid register conflicts when temp regs overlap with arg regs */
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", size_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = add_inst(inst_list, "\tcall\tgpc_move\n");
            free_arg_regs();

            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                return_dest_slot->offset, RETURN_REG_64);
            inst_list = add_inst(inst_list, buffer);

            free_reg(get_reg_stack(), dest_reg);
            free_reg(get_reg_stack(), src_reg);
            free_reg(get_reg_stack(), size_reg);
        }
    }
    else
    {
        if (return_var->size >= 8)
            snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_64);
        else
            snprintf(buffer, 50, "\tmovl\t-%d(%%rbp), %s\n", return_var->offset, RETURN_REG_32);
        inst_list = add_inst(inst_list, buffer);
    }
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
ListNode_t *codegen_subprogram_arguments(ListNode_t *args, ListNode_t *inst_list,
    CodeGenContext *ctx, SymTab_t *symtab, int arg_start_index)
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

    if (arg_start_index < 0)
        arg_start_index = 0;

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
                    if (type_node != NULL)
                    {
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL)
                        {
                            type = alias->base_type;
                        }
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
                        if (arg_symbol != NULL)
                            record_type_info = get_record_type_from_node(arg_symbol);
                        else if (resolved_type_node != NULL)
                            record_type_info = get_record_type_from_node(resolved_type_node);
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

                        arg_reg = get_arg_reg64_num(arg_start_index + arg_num);
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
                         type == REAL_TYPE || type == LONGINT_TYPE || type == PROCEDURE);
                    arg_reg = use_64bit ?
                        get_arg_reg64_num(arg_start_index + arg_num) :
                        get_arg_reg32_num(arg_start_index + arg_num);
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

            struct TypeAlias *alias = get_type_alias_from_node(type_node);
            if (alias != NULL && alias->is_array && alias->is_open_array)
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
            if (type_node != NULL && node_is_file_type(type_node))
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
