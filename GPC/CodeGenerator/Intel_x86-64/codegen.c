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
#include <ctype.h>
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

#define CODEGEN_POINTER_SIZE_BYTES 8

/* Helper functions for transitioning from legacy type fields to GpcType */
static int codegen_dynamic_array_element_size_from_type(CodeGenContext *ctx, GpcType *array_type);
static int codegen_dynamic_array_descriptor_bytes(int element_size);
static void add_result_alias_for_return_var(StackNode_t *return_var);
static ListNode_t *codegen_store_class_typeinfo(ListNode_t *inst_list,
    CodeGenContext *ctx, StackNode_t *var_node, const char *type_name);
static ListNode_t *codegen_emit_tfile_configure(ListNode_t *inst_list,
    StackNode_t *file_node, long long element_size, int element_hash_tag);
static int codegen_resolve_file_component(const struct TypeAlias *alias, SymTab_t *symtab,
    long long *element_size_out, int *element_hash_tag_out);
static char *codegen_make_program_var_label(CodeGenContext *ctx, const char *name);

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Helper function to check if a node is a file type */
static inline int node_is_file_type(HashNode_t *node)
{
    if (node == NULL || node->type == NULL)
        return 0;
    return gpc_type_equals_tag(node->type, FILE_TYPE) ||
        gpc_type_equals_tag(node->type, TEXT_TYPE);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

static inline int node_is_class_type(HashNode_t *node)
{
    if (node == NULL)
        return 0;
    if (!node_is_record_type(node))
        return 0;
    struct RecordType *record = get_record_type_from_node(node);
    return record_type_is_class(record);
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

static const char *codegen_resolve_record_type_name(HashNode_t *node, SymTab_t *symtab)
{
    if (node == NULL)
        return NULL;
    if (node_is_record_type(node) && node->id != NULL)
        return node->id;
    struct TypeAlias *alias = get_type_alias_from_node(node);
    if (alias != NULL && alias->target_type_id != NULL && symtab != NULL)
    {
        HashNode_t *target = NULL;
        if (FindIdent(&target, symtab, alias->target_type_id) != -1 && target != NULL)
            return codegen_resolve_record_type_name(target, symtab);
    }
    return NULL;
}

/* Allocate the next available integer argument register (general purpose). */
static const char *alloc_integer_arg_reg(int use_64bit, int *next_index)
{
    if (next_index == NULL)
        return NULL;

    const char *reg = use_64bit ?
        get_arg_reg64_num(*next_index) :
        get_arg_reg32_num(*next_index);
    if (reg == NULL)
        return NULL;

    ++(*next_index);
    return reg;
}

/* Allocate the next available SSE argument register for REAL parameters. */
static const char *alloc_sse_arg_reg(int *next_index)
{
    if (next_index == NULL)
        return NULL;

    const char *reg = current_arg_reg_xmm(*next_index);
    if (reg == NULL)
    {
        fprintf(stderr,
            "ERROR: Max SSE argument register limit exceeded (index=%d)\n",
            *next_index);
        exit(1);
    }

    ++(*next_index);
    return reg;
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
                case TEXT_TYPE:
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
            /* For classes, allocate only pointer size since instances are heap-allocated */
            if (node->type->kind == TYPE_KIND_RECORD && 
                node->type->info.record_info != NULL &&
                record_type_is_class(node->type->info.record_info))
            {
                return 8;  /* Class variables are pointers */
            }
            
            long long size = gpc_type_sizeof(node->type);
            if (size > 0)
                return (int)size;
            return -1;
        }
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
    return (ctx != NULL) ? ctx->current_subprogram_lexical_depth : 0;
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
    ctx->static_link_spill_slot = NULL;
}

static void codegen_static_link_spilled(Register_t *reg, StackNode_t *spill_slot, void *context)
{
    (void)reg;
    CodeGenContext *ctx = (CodeGenContext *)context;
    if (ctx == NULL || spill_slot == NULL)
        return;
    ctx->static_link_spill_slot = spill_slot;
    ctx->static_link_reg = NULL;
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
    else if (ctx->static_link_spill_slot != NULL)
    {
        if (ctx->static_link_reg_level == levels_to_traverse)
        {
            Register_t *reloaded = get_free_reg(get_reg_stack(), inst_list);
            if (reloaded == NULL)
                reloaded = get_reg_with_spill(get_reg_stack(), inst_list);
            if (reloaded == NULL)
                return NULL;

            char buffer[64];
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                ctx->static_link_spill_slot->offset, reloaded->bit_64);
            *inst_list = add_inst(*inst_list, buffer);

            ctx->static_link_reg = reloaded;
            ctx->static_link_spill_slot = NULL;
            register_set_spill_callback(reloaded, codegen_static_link_spilled, ctx);
            return reloaded;
        }

        ctx->static_link_spill_slot = NULL;
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
    register_set_spill_callback(reg, codegen_static_link_spilled, ctx);
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

/* -------------------------------------------------------------------------
 * Debug helpers for annotated assembly output
 * ------------------------------------------------------------------------- */
static void asm_debug_comment(FILE *out, const char *tag, int indent, const char *fmt, ...)
{
    if (out == NULL || tag == NULL || fmt == NULL)
        return;
    if (indent < 0)
        indent = 0;

    fprintf(out, "# [%s] ", tag);
    for (int i = 0; i < indent; ++i)
        fputs("  ", out);

    va_list args;
    va_start(args, fmt);
    vfprintf(out, fmt, args);
    va_end(args);
    fputc('\n', out);
}

static const char *hash_type_to_string(enum HashType type)
{
    switch (type)
    {
        case HASHTYPE_VAR: return "var";
        case HASHTYPE_ARRAY: return "array";
        case HASHTYPE_CONST: return "const";
        case HASHTYPE_PROCEDURE: return "procedure";
        case HASHTYPE_FUNCTION: return "function";
        case HASHTYPE_FUNCTION_RETURN: return "function-return";
        case HASHTYPE_BUILTIN_PROCEDURE: return "builtin-proc";
        case HASHTYPE_TYPE: return "type";
        default: return "unknown";
    }
}

static const char *hashnode_type_to_string(const HashNode_t *node)
{
    if (node == NULL)
        return "<null>";
    
    if (node->type != NULL)
        return gpc_type_to_string(node->type);

    return "untyped";
}

static void summarize_string_literal(const char *src, char *dest, size_t dest_size)
{
    if (dest == NULL || dest_size == 0)
        return;
    dest[0] = '\0';
    if (src == NULL)
        return;

    size_t out_idx = 0;
    dest[out_idx++] = '"';
    size_t i = 0;
    const size_t max_chars = 24;
    while (src[i] != '\0' && out_idx + 2 < dest_size && i < max_chars)
    {
        unsigned char ch = (unsigned char)src[i++];
        if (!isprint(ch) || ch == '"' || ch == '\\')
            dest[out_idx++] = '?';
        else
            dest[out_idx++] = (char)ch;
    }
    if (src[i] != '\0' && out_idx + 4 < dest_size)
    {
        dest[out_idx++] = '.';
        dest[out_idx++] = '.';
        dest[out_idx++] = '.';
    }
    if (out_idx + 1 < dest_size)
        dest[out_idx++] = '"';
    dest[out_idx] = '\0';
}

static void codegen_emit_semantic_scope_comments(FILE *out, const HashTable_t *table,
    const char *label, int indent)
{
    if (out == NULL || label == NULL)
        return;

    asm_debug_comment(out, "semcheck", indent, "%s", label);
    if (table == NULL)
    {
        asm_debug_comment(out, "semcheck", indent + 1, "(empty)");
        return;
    }

    int entries = 0;
    for (int i = 0; i < TABLE_SIZE; ++i)
    {
        ListNode_t *entry = table->table[i];
        while (entry != NULL)
        {
            HashNode_t *node = (HashNode_t *)entry->cur;
            entry = entry->next;
            if (node == NULL)
                continue;

            ++entries;
            char mangled_buf[96] = "";
            if (node->mangled_id != NULL && node->mangled_id[0] != '\0')
                snprintf(mangled_buf, sizeof(mangled_buf), ", mangled=%s", node->mangled_id);

            char const_buf[128] = "";
            if (node->hash_type == HASHTYPE_CONST)
            {
                if (node->const_string_value != NULL)
                {
                    char snippet[48];
                    summarize_string_literal(node->const_string_value, snippet, sizeof(snippet));
                    snprintf(const_buf, sizeof(const_buf), ", const=%s", snippet);
                }
                else
                {
                    snprintf(const_buf, sizeof(const_buf), ", const=%lld",
                        (long long)node->const_int_value);
                }
            }

            char link_buf[48] = "";
            if (hashnode_requires_static_link(node))
                snprintf(link_buf, sizeof(link_buf), ", needs-static-link");

            asm_debug_comment(out, "semcheck", indent + 1,
                "%s kind=%s type=%s%s%s%s",
                (node->id != NULL) ? node->id : "<unnamed>",
                hash_type_to_string(node->hash_type),
                hashnode_type_to_string(node),
                mangled_buf,
                const_buf,
                link_buf);
        }
    }

    if (entries == 0)
        asm_debug_comment(out, "semcheck", indent + 1, "(empty)");
}

static void codegen_emit_semantic_debug_block(CodeGenContext *ctx)
{
    if (!asm_debug_flag() || ctx == NULL || ctx->symtab == NULL || ctx->output_file == NULL)
        return;

    FILE *out = ctx->output_file;
    asm_debug_comment(out, "semcheck", 0, "--- symbol table snapshot ---");
    codegen_emit_semantic_scope_comments(out, ctx->symtab->builtins, "builtins", 0);

    ListNode_t *scope = ctx->symtab->stack_head;
    int depth = 0;
    while (scope != NULL)
    {
        HashTable_t *table = (HashTable_t *)scope->cur;
        char label[32];
        snprintf(label, sizeof(label), "scope %d", depth);
        codegen_emit_semantic_scope_comments(out, table, label, 0);
        scope = scope->next;
        ++depth;
    }

    asm_debug_comment(out, "semcheck", 0, "--- end symbol table ---");
}

static void codegen_emit_function_debug_comments(const char *func_name, CodeGenContext *ctx)
{
    if (!asm_debug_flag() || ctx == NULL || ctx->output_file == NULL || func_name == NULL)
        return;

    asm_debug_comment(ctx->output_file, "codegen", 0,
        "function %s (lex-depth=%d)", func_name,
        ctx->current_subprogram_lexical_depth);

    StackScope_t *scope = get_cur_scope();
    if (scope != NULL)
    {
        int locals = scope->x_offset;
        int temps = scope->t_offset;
        int args = scope->z_offset;
        int total = get_full_stack_offset();
        asm_debug_comment(ctx->output_file, "codegen", 1,
            "stack locals=%dB temps=%dB args=%dB total=%dB",
            locals, temps, args, total);
    }
    else
    {
        asm_debug_comment(ctx->output_file, "codegen", 1,
            "stack scope unavailable");
    }

    int needs_link = codegen_proc_requires_static_link(ctx, func_name);
    asm_debug_comment(ctx->output_file, "codegen", 1,
        "static-link=%s", needs_link ? "required" : "not-required");
}

static void codegen_sanitize_identifier_for_label(const char *value, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    size_t idx = 0;
    if (value == NULL || value[0] == '\0')
    {
        buffer[idx++] = 'v';
    }
    else
    {
        for (const char *p = value; *p != '\0' && idx + 1 < size; ++p)
        {
            unsigned char c = (unsigned char)*p;
            if (isalnum(c) || c == '_')
                buffer[idx++] = (char)c;
            else
                buffer[idx++] = '_';
        }
    }

    if (idx == 0)
        buffer[idx++] = 'v';
    buffer[idx] = '\0';
}

static char *codegen_make_program_var_label(CodeGenContext *ctx, const char *name)
{
    if (ctx == NULL)
        return NULL;

    char sanitized[128];
    codegen_sanitize_identifier_for_label(name, sanitized, sizeof(sanitized));

    char buffer[256];
    snprintf(buffer, sizeof(buffer), "__gpc_program_var_%s_%d",
        sanitized, ++ctx->global_data_counter);
    return strdup(buffer);
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
    codegen_emit_function_debug_comments(func_name, ctx);
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
    ctx->pending_stack_arg_bytes = 0;

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
    
    /* RTTI metadata and VMTs are generated as read-only data structures */
    fprintf(ctx->output_file, "\n");
    fprintf(ctx->output_file, "# Class RTTI metadata and Virtual Method Tables (VMT)\n");
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    
    /* Iterate through all type declarations */
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL) {
            struct RecordType *record_info = NULL;
            const char *class_label = NULL;
            
            /* Check if this is a record/class type with methods */
            if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
                record_info = type_tree->tree_data.type_decl_data.info.record;
                const char *type_name = type_tree->tree_data.type_decl_data.id;
                class_label = (record_info != NULL && record_info->type_id != NULL) ?
                    record_info->type_id : type_name;
            }
            /* Also check for TYPE_DECL_ALIAS that points to specialized generic classes */
            else if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                record_info = type_tree->tree_data.type_decl_data.info.alias.inline_record_type;
                if (record_info != NULL && record_info->type_id != NULL) {
                    /* Check if this is a specialized generic (has $ in the name) */
                    if (strchr(record_info->type_id, '$') != NULL) {
                        class_label = record_info->type_id;
                    }
                }
            }

            if (record_info != NULL && record_type_is_class(record_info) && class_label != NULL) {
                    fprintf(ctx->output_file, "\n# RTTI for class %s\n", class_label);
                    fprintf(ctx->output_file, "\t.align 8\n");
                    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", class_label);
                    fprintf(ctx->output_file, "%s_TYPEINFO:\n", class_label);
                    if (record_info->parent_class_name != NULL)
                        fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", record_info->parent_class_name);
                    else
                        fprintf(ctx->output_file, "\t.quad\t0\n");

                    char name_label[256];
                    snprintf(name_label, sizeof(name_label), "__gpc_typeinfo_name_%s", class_label);
                    fprintf(ctx->output_file, "\t.quad\t%s\n", name_label);
                    if (record_info->methods != NULL)
                        fprintf(ctx->output_file, "\t.quad\t%s_VMT\n", class_label);
                    else
                        fprintf(ctx->output_file, "\t.quad\t0\n");
                    fprintf(ctx->output_file, "%s:\n\t.string \"%s\"\n", name_label, class_label);

                    if (record_info->methods != NULL) {
                        fprintf(ctx->output_file, "\n# VMT for class %s\n", class_label);
                        fprintf(ctx->output_file, "\t.align 8\n");
                        fprintf(ctx->output_file, ".globl %s_VMT\n", class_label);
                        fprintf(ctx->output_file, "%s_VMT:\n", class_label);
                        ListNode_t *method_node = record_info->methods;
                        while (method_node != NULL) {
                            struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
                            if (method != NULL && method->mangled_name != NULL) {
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
    if (asm_debug_flag())
    {
        fputc('\n', ctx->output_file);
        codegen_emit_semantic_debug_block(ctx);
        fputc('\n', ctx->output_file);
    }
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
        
        /* Zero-initialize the allocated stack space to ensure local variables start with zero values.
         * This is critical for code that assumes uninitialized variables are zero (like linked lists).
         * We use rep stosq for efficient zero-filling.
         * 
         * Calling conventions differ between platforms:
         * - Windows x64: parameters in rcx, rdx, r8, r9
         * - System V AMD64 (Linux): parameters in rdi, rsi, rdx, rcx, r8, r9
         * 
         * rep stosq uses rdi (destination), rax (value), rcx (count)
         * We need to save/restore these registers if they contain parameters.
         * r10 and r11 are caller-saved scratch registers safe to use on both platforms.
         */
        int quadwords = (aligned_space + 7) / 8;  /* Round up to nearest quadword */
        
        if (codegen_target_is_windows())
        {
            /* Windows x64 calling convention: rcx, rdx, r8, r9
             * rep stosq will clobber rcx, rdi (rdi not used for params on Windows) */
            fprintf(ctx->output_file, "\tmovq\t%%rcx, %%r11\n");  /* Save rcx (1st param) to r11 */
            fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rdi\n");   /* rdi = stack pointer */
            fprintf(ctx->output_file, "\txorq\t%%rax, %%rax\n");   /* rax = 0 */
            fprintf(ctx->output_file, "\tmovl\t$%d, %%ecx\n", quadwords);  /* ecx = count */
            fprintf(ctx->output_file, "\trep stosq\n");            /* Zero-fill */
            fprintf(ctx->output_file, "\tmovq\t%%r11, %%rcx\n");  /* Restore rcx */
        }
        else
        {
            /* System V AMD64 (Linux) calling convention: rdi, rsi, rdx, rcx, r8, r9
             * rep stosq will clobber rdi, rcx */
            fprintf(ctx->output_file, "\tmovq\t%%rdi, %%r10\n");  /* Save rdi (1st param) to r10 */
            fprintf(ctx->output_file, "\tmovq\t%%rcx, %%r11\n");  /* Save rcx (4th param) to r11 */
            fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rdi\n");   /* rdi = stack pointer */
            fprintf(ctx->output_file, "\txorq\t%%rax, %%rax\n");   /* rax = 0 */
            fprintf(ctx->output_file, "\tmovl\t$%d, %%ecx\n", quadwords);  /* ecx = count */
            fprintf(ctx->output_file, "\trep stosq\n");            /* Zero-fill */
            fprintf(ctx->output_file, "\tmovq\t%%r10, %%rdi\n");  /* Restore rdi */
            fprintf(ctx->output_file, "\tmovq\t%%r11, %%rcx\n");  /* Restore rcx */
        }
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
    int prev_depth = ctx->current_subprogram_lexical_depth;
    ctx->current_subprogram_id = prgm_name;
    ctx->current_subprogram_mangled = prgm_name;
    ctx->current_subprogram_lexical_depth = 0;

    push_stackscope();

    codegen_function_locals(data->var_declaration, ctx, symtab);
    codegen_subprograms(data->subprograms, ctx, symtab);

    inst_list = NULL;
    inst_list = codegen_var_initializers(data->var_declaration, inst_list, ctx, symtab);
    inst_list = codegen_stmt(data->body_statement, inst_list, ctx, symtab);

    // Generate finalization code from units (in LIFO order - already reversed in list)
    if (data->finalization_statements != NULL) {
        ListNode_t *final_node = data->finalization_statements;
        while (final_node != NULL) {
            if (final_node->type == LIST_STMT && final_node->cur != NULL) {
                struct Statement *final_stmt = (struct Statement *)final_node->cur;
                inst_list = codegen_stmt(final_stmt, inst_list, ctx, symtab);
            }
            final_node = final_node->next;
        }
    }

    codegen_function_header(prgm_name, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(prgm_name, ctx);
    free_inst_list(inst_list);

    pop_stackscope();

    ctx->current_subprogram_id = prev_id;
    ctx->current_subprogram_mangled = prev_mangled;
    ctx->current_subprogram_lexical_depth = prev_depth;

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

    int is_program_scope = (codegen_get_lexical_depth(ctx) == 0);

     cur = local_decl;

     while(cur != NULL)
     {
         tree = (Tree_t *)cur->cur;
         assert(tree != NULL);

        if (tree->type == TREE_VAR_DECL)
        {
            id_list = tree->tree_data.var_decl_data.ids;
           HashNode_t *type_node = NULL;
           if (symtab != NULL && tree->tree_data.var_decl_data.type_id != NULL) {
               FindIdent(&type_node, symtab, tree->tree_data.var_decl_data.type_id);
           }
            GpcType *cached_type = tree->tree_data.var_decl_data.cached_gpc_type;

            while(id_list != NULL)
            {
                HashNode_t cached_type_node;
                HashNode_t *fallback_type_node = NULL;
                if (cached_type != NULL)
                {
                    memset(&cached_type_node, 0, sizeof(cached_type_node));
                    cached_type_node.type = cached_type;
                    fallback_type_node = &cached_type_node;
                }

                HashNode_t *effective_type_node = type_node;
                if (effective_type_node == NULL)
                    effective_type_node = fallback_type_node;

                struct TypeAlias *alias = get_type_alias_from_node(effective_type_node);
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
                        switch (alias->array_element_type)
                        {
                            case LONGINT_TYPE:
                            case REAL_TYPE:
                            case STRING_TYPE:
                            case POINTER_TYPE:
                            case FILE_TYPE:
                            case TEXT_TYPE:
                                element_size = 8;
                                break;
                            case CHAR_TYPE:
                                element_size = 1;
                                break;
                            case INT_TYPE:
                            case BOOL:
                            case SET_TYPE:
                            case ENUM_TYPE:
                                element_size = 4;
                                break;
                            default:
                                element_size = 4;
                                break;
                        }
                    }

                    if (alias->is_open_array)
                    {
                        char *static_label = NULL;
                        if (is_program_scope)
                        {
                            static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int descriptor_bytes = codegen_dynamic_array_descriptor_bytes(element_size);
                                int alignment = descriptor_bytes >= 8 ? 8 : DOUBLEWORD;
                                fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                    static_label, descriptor_bytes, alignment);
                            }
                        }
                        add_dynamic_array((char *)id_list->cur, element_size,
                            alias->array_start, is_program_scope, static_label);
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        int length = alias->array_end - alias->array_start + 1;
                        if (length < 0)
                            length = 0;
                        long long total_size = (long long)length * (long long)element_size;
                        if (total_size <= 0)
                            total_size = element_size;
                        if (is_program_scope)
                        {
                            char *static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                    static_label, (int)total_size, alignment);
                            }
                            add_static_array((char *)id_list->cur, (int)total_size, element_size,
                                alias->array_start, static_label);
                            if (static_label != NULL)
                                free(static_label);
                        }
                        else
                        {
                            add_array((char *)id_list->cur, (int)total_size, element_size,
                                alias->array_start);
                        }
                    }
                }
                else
                {
                    int alloc_size = DOUBLEWORD;
                    HashNode_t *var_info = NULL;
                    HashNode_t *size_node = NULL;  /* Node to get size from */
                    HashNode_t temp_size_node;
                    
                    if (symtab != NULL)
                    {
                        if (FindIdent(&var_info, symtab, (char *)id_list->cur) >= 0 && var_info != NULL)
                            size_node = var_info;
                    }
                    /* Use type_node if we don't have specific var_info */
                    if (size_node == NULL && effective_type_node != NULL)
                        size_node = effective_type_node;
                    if (size_node == NULL && cached_type != NULL)
                    {
                        memset(&temp_size_node, 0, sizeof(temp_size_node));
                        temp_size_node.type = cached_type;
                        size_node = &temp_size_node;
                    }
                    
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
                            /* For classes, allocate only pointer size (8 bytes)
                             * For records/objects, allocate the full struct size */
                            if (node_is_class_type(size_node))
                            {
                                fprintf(stderr, "DEBUG ALLOC: Detected class type for '%s', allocating 8 bytes\n",
                                    (char *)id_list->cur);
                                alloc_size = 8;  /* Classes are heap-allocated; variable holds pointer */
                            }
                            else
                            {
                                fprintf(stderr, "DEBUG ALLOC: Detected record type for '%s', allocating full size\n",
                                    (char *)id_list->cur);
                                /* For records/objects, get the full struct size */
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
                    }

                    if (is_program_scope)
                    {
                        char *static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                        if (ctx->output_file != NULL && static_label != NULL)
                        {
                            int alignment = alloc_size >= 8 ? 8 : DOUBLEWORD;
                            fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                static_label, alloc_size, alignment);
                        }
                        add_static_var((char *)id_list->cur, alloc_size, static_label);
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        add_l_x((char *)id_list->cur, alloc_size);
                    }
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
                        case TEXT_TYPE:
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
                    char *static_label = NULL;
                    if (is_program_scope)
                    {
                        static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                        if (ctx->output_file != NULL && static_label != NULL)
                        {
                            int descriptor_bytes = codegen_dynamic_array_descriptor_bytes(element_size);
                            int alignment = descriptor_bytes >= 8 ? 8 : DOUBLEWORD;
                            fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                static_label, descriptor_bytes, alignment);
                        }
                    }
                    add_dynamic_array((char *)id_list->cur, element_size, arr->s_range,
                        is_program_scope, static_label);
                    if (static_label != NULL)
                        free(static_label);
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

                int use_static_storage = arr->has_static_storage || is_program_scope;
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
                }

                if (use_static_storage)
                {
                    while (id_list != NULL)
                    {
                        const char *label_to_use = arr->static_label;
                        char *generated_label = NULL;
                        if (!arr->has_static_storage)
                        {
                            generated_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && generated_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                    generated_label, total_size, alignment);
                            }
                            label_to_use = generated_label;
                        }
                        add_static_array((char *)id_list->cur, total_size, element_size,
                            arr->s_range, label_to_use);
                        if (generated_label != NULL)
                            free(generated_label);
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
    int lexical_depth = proc->nesting_level;
    if (lexical_depth < 0)
        lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    ctx->current_subprogram_lexical_depth = lexical_depth;
    int is_class_method = (sub_id != NULL && strstr(sub_id, "__") != NULL);
    StackNode_t *static_link = NULL;

    /* Process arguments first to allocate their stack space */
    /* Nested procedures always receive a static link so they can forward it to callees,
     * even if they don't themselves capture any outer scope state. Class methods still
     * use the implicit `self` parameter instead. */
    int will_need_static_link = (!is_class_method &&
        proc->requires_static_link);
    
    /* If there are arguments and we'll need a static link, shift argument registers by 1 */
    int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;
    inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, ctx, symtab, arg_start_index);
    
    /* Now add static link after arguments to avoid overlap */
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
    ctx->current_subprogram_lexical_depth = prev_depth;

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
    int returns_dynamic_array = 0;
    int dynamic_array_descriptor_size = 0;
    int dynamic_array_element_size = 0;
    int dynamic_array_lower_bound = 0;
    long long record_return_size = 0;

    func = &func_tree->tree_data.subprogram_data;
    sub_id = (func->mangled_id != NULL) ? func->mangled_id : func->id;

    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;

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
    int lexical_depth = func->nesting_level;
    if (lexical_depth < 0)
        lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    ctx->current_subprogram_lexical_depth = lexical_depth;
    int is_class_method = (sub_id != NULL && strstr(sub_id, "__") != NULL);
    StackNode_t *static_link = NULL;

    HashNode_t *func_node = NULL;
    if (symtab != NULL)
    {
        /* For overloaded functions, we need to find the correct overload by matching
         * the mangled name. FindIdent alone is insufficient because it returns the
         * first match, which might be a different overload. */
        if (func->mangled_id != NULL)
        {
            /* Try to find all identifiers with this name */
            ListNode_t *all_matches = FindAllIdents(symtab, func->id);
            ListNode_t *cur = all_matches;
            
            /* Find the one with matching mangled name */
            while (cur != NULL && func_node == NULL)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate != NULL && candidate->mangled_id != NULL &&
                    strcmp(candidate->mangled_id, func->mangled_id) == 0)
                {
                    func_node = candidate;
                }
                cur = cur->next;
            }
            
            if (all_matches != NULL)
                DestroyList(all_matches);
        }
        
        /* Fallback to simple lookup if no mangled name or no match found */
        if (func_node == NULL)
        {
            FindIdent(&func_node, symtab, func->id);
        }
    }

    /* Check if function returns a record by examining GpcType */
    if (func_node != NULL && func_node->type != NULL &&
        func_node->type->kind == TYPE_KIND_PROCEDURE)
    {
        GpcType *return_type = gpc_type_get_return_type(func_node->type);
        if (return_type != NULL)
        {
            if (gpc_type_is_record(return_type))
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
            else if (return_type->kind == TYPE_KIND_ARRAY &&
                     gpc_type_is_dynamic_array(return_type))
            {
                returns_dynamic_array = 1;
                dynamic_array_element_size = codegen_dynamic_array_element_size_from_type(ctx, return_type);
                dynamic_array_descriptor_size = codegen_dynamic_array_descriptor_bytes(dynamic_array_element_size);
                dynamic_array_lower_bound = return_type->info.array_info.start_index;
            }
        }
    }
    else if (func_node != NULL && func_node->type != NULL &&
             gpc_type_is_record(func_node->type))
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
    
    /* Also check return_type_id from the function tree for functions with record returns
     * that weren't looked up in symbol table correctly (e.g., class operators) */
    if (!has_record_return && func->return_type_id != NULL && symtab != NULL)
    {
        fprintf(stderr, "DEBUG: Checking return_type_id='%s' for function '%s'\n", 
                func->return_type_id, func->id);
        HashNode_t *return_type_node = NULL;
        FindIdent(&return_type_node, symtab, func->return_type_id);
        if (return_type_node != NULL)
        {
            fprintf(stderr, "DEBUG: Found return type node\n");
            struct RecordType *record_type = hashnode_get_record_type(return_type_node);
            if (record_type != NULL)
            {
                fprintf(stderr, "DEBUG: It's a record type!\n");
                if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                        record_type, &record_return_size) == 0 &&
                    record_return_size > 0 && record_return_size <= INT_MAX)
                {
                    fprintf(stderr, "DEBUG: Setting has_record_return=1, size=%lld\n", record_return_size);
                    has_record_return = 1;
                }
            }
        }
        else
        {
            fprintf(stderr, "DEBUG: return_type_node is NULL\n");
        }
    }
    else
    {
        fprintf(stderr, "DEBUG: Skipped return_type_id check: has_record_return=%d, return_type_id=%s, symtab=%p\n",
                has_record_return, func->return_type_id ? func->return_type_id : "NULL", (void*)symtab);
    }
    
    /* Also check inline_return_type from the function tree for functions with inline record returns */
    if (!has_record_return && func->inline_return_type != NULL &&
        func->inline_return_type->base_type == RECORD_TYPE)
    {
        struct RecordType *inline_record = NULL;

        if (func->inline_return_type->gpc_type != NULL &&
            gpc_type_is_record(func->inline_return_type->gpc_type))
        {
            inline_record = gpc_type_get_record(func->inline_return_type->gpc_type);
        }

        if (inline_record == NULL &&
            func->inline_return_type->target_type_id != NULL && symtab != NULL)
        {
            HashNode_t *inline_type_node = NULL;
            FindIdent(&inline_type_node, symtab,
                func->inline_return_type->target_type_id);
            if (inline_type_node != NULL)
                inline_record = hashnode_get_record_type(inline_type_node);
        }

        if (inline_record != NULL &&
            codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                inline_record, &record_return_size) == 0 &&
            record_return_size > 0 && record_return_size <= INT_MAX)
        {
            has_record_return = 1;
        }
    }

    /* Only functions that close over variables need static links (excluding class methods). */
    int will_need_static_link = (!is_class_method &&
        func->requires_static_link);
    
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
        assert(link_reg != NULL && "current_arg_reg64() should never return NULL for valid indices");
        snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
            link_reg, static_link->offset);
        inst_list = add_inst(inst_list, link_buffer);
    }
    
    int return_size = DOUBLEWORD;
    if (returns_dynamic_array)
        return_size = dynamic_array_descriptor_size;
    else if (has_record_return)
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
                case POINTER_TYPE:
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
        else if (return_type != NULL && return_type->kind == TYPE_KIND_POINTER)
        {
            /* Pointer return (e.g., PChar) */
            return_size = 8;
        }
    }
    else if (func_node != NULL && func_node->type != NULL &&
             func_node->type->kind == TYPE_KIND_PRIMITIVE)
    {
        int tag = gpc_type_get_primitive_tag(func_node->type);
        if (tag == LONGINT_TYPE || tag == REAL_TYPE || tag == STRING_TYPE || tag == POINTER_TYPE)
            return_size = 8;
        else if (tag == BOOL)
            return_size = DOUBLEWORD;
    }
    else if (func_node != NULL && func_node->type != NULL &&
             func_node->type->kind == TYPE_KIND_POINTER)
    {
        return_size = 8;
    }

    if (returns_dynamic_array)
        return_var = add_dynamic_array(func->id, dynamic_array_element_size,
            dynamic_array_lower_bound, 0, NULL);
    else
        return_var = add_l_x(func->id, return_size);

    /* Allow Delphi-style Result alias in regular functions too. */
    add_result_alias_for_return_var(return_var);

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
            ListNode_t *record_return_inst = NULL;
            record_return_inst = add_inst(record_return_inst, ptr_buffer);
            inst_list = ConcatList(record_return_inst, inst_list);
        }
    }

    codegen_function_locals(func->declarations, ctx, symtab);

    /* Recursively generate nested subprograms */
    codegen_subprograms(func->subprograms, ctx, symtab);
    
    inst_list = codegen_var_initializers(func->declarations, inst_list, ctx, symtab);
    inst_list = codegen_stmt(func->statement_list, inst_list, ctx, symtab);
    if (returns_dynamic_array)
    {
#if GPC_ENABLE_REG_DEBUG
        const char *prev_reg_ctx = g_reg_debug_context;
        g_reg_debug_context = "dyn_array_return";
#endif
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg == NULL)
        {
            codegen_report_error(ctx,
                "ERROR: Unable to allocate register for dynamic array return.");
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
                return_var->offset, addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);

            if (codegen_target_is_windows())
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n", dynamic_array_descriptor_size);
                inst_list = add_inst(inst_list, buffer);
            }
            else
            {
                snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", addr_reg->bit_64);
                inst_list = add_inst(inst_list, buffer);
                snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%esi\n", dynamic_array_descriptor_size);
                inst_list = add_inst(inst_list, buffer);
            }

            inst_list = codegen_vect_reg(inst_list, 0);
            inst_list = add_inst(inst_list, "\tcall\tgpc_dynarray_clone_descriptor\n");
            free_arg_regs();
            free_reg(get_reg_stack(), addr_reg);
        }
#if GPC_ENABLE_REG_DEBUG
        g_reg_debug_context = prev_reg_ctx;
#endif
    }
    else if (has_record_return && return_dest_slot != NULL && record_return_size > 0)
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
        /* Determine if return type is Real (floating-point) */
        int is_real_return = 0;
        if (func_node != NULL && func_node->type != NULL &&
            func_node->type->kind == TYPE_KIND_PROCEDURE)
        {
            GpcType *return_type = gpc_type_get_return_type(func_node->type);
            if (return_type != NULL && return_type->kind == TYPE_KIND_PRIMITIVE)
            {
                int tag = gpc_type_get_primitive_tag(return_type);
                if (tag == REAL_TYPE)
                    is_real_return = 1;
            }
        }
        else if (func_node != NULL && func_node->type != NULL &&
                 func_node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            int tag = gpc_type_get_primitive_tag(func_node->type);
            if (tag == REAL_TYPE)
                is_real_return = 1;
        }
        
        /* Use movsd for Real types (return in xmm0), movq/movl for others (return in rax/eax) */
        if (is_real_return)
            snprintf(buffer, 50, "\tmovsd\t-%d(%%rbp), %%xmm0\n", return_var->offset);
        else if (return_var->size >= 8)
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
    ctx->current_subprogram_lexical_depth = prev_depth;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Helper function to determine the size in bytes for a return type */
static int get_return_type_size(int return_type)
{
    if (return_type == LONGINT_TYPE || return_type == STRING_TYPE || 
        return_type == POINTER_TYPE || return_type == REAL_TYPE)
        return 8;
    return 4; /* Default for INT_TYPE, BOOL, CHAR_TYPE, etc. */
}

/* Helper function to add a Result alias for anonymous function return variable */
static void add_result_alias_for_return_var(StackNode_t *return_var)
{
    if (return_var == NULL)
        return;
    
    /* Create a stack node for "Result" pointing to the same offset */
    StackNode_t *result_alias = init_stack_node(return_var->offset, "Result", return_var->size);
    if (result_alias == NULL)
        return;
    
    result_alias->element_size = return_var->element_size;
    result_alias->is_alias = 1;
    
    /* Add it to the x list in the current stack scope using the list API */
    StackScope_t *cur_scope = get_cur_scope();
    if (cur_scope != NULL)
    {
        ListNode_t *new_list_node = CreateListNode(result_alias, LIST_UNSPECIFIED);
        if (new_list_node != NULL)
        {
            if (cur_scope->x == NULL)
                cur_scope->x = new_list_node;
            else
                cur_scope->x = PushListNodeBack(cur_scope->x, new_list_node);
        }
    }
}

static int codegen_dynamic_array_element_size_from_type(CodeGenContext *ctx, GpcType *array_type)
{
    if (array_type == NULL || array_type->kind != TYPE_KIND_ARRAY)
        return DOUBLEWORD;

    GpcType *element_type = array_type->info.array_info.element_type;
    if (element_type == NULL)
        return DOUBLEWORD;

    switch (element_type->kind)
    {
        case TYPE_KIND_PRIMITIVE:
        {
            int tag = gpc_type_get_primitive_tag(element_type);
            switch (tag)
            {
                case LONGINT_TYPE:
                case REAL_TYPE:
                case STRING_TYPE:
                case POINTER_TYPE:
                    return 8;
                case CHAR_TYPE:
                case BOOL:
                    return 1;
                default:
                    return DOUBLEWORD;
            }
        }
        case TYPE_KIND_RECORD:
        {
            struct RecordType *record = gpc_type_get_record(element_type);
            long long size = 0;
            if (record != NULL &&
                codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record, &size) == 0 &&
                size > 0 && size <= INT_MAX)
                return (int)size;
            return DOUBLEWORD;
        }
        case TYPE_KIND_POINTER:
        case TYPE_KIND_PROCEDURE:
            return 8;
        case TYPE_KIND_ARRAY:
            return DOUBLEWORD;
        default:
            return DOUBLEWORD;
    }
}

static int codegen_dynamic_array_descriptor_bytes(int element_size)
{
    int descriptor_size = 4 * DOUBLEWORD;
    int needed = element_size * 2;
    if (needed > descriptor_size)
        descriptor_size = needed;
    return descriptor_size;
}

/* Code generation for an anonymous function/procedure
 * This generates the function body and returns the function's label name.
 * The caller is responsible for generating code to load the address of this function.
 */
void codegen_anonymous_method(struct Expression *expr, CodeGenContext *ctx, SymTab_t *symtab)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    
    assert(expr != NULL);
    assert(expr->type == EXPR_ANONYMOUS_FUNCTION || expr->type == EXPR_ANONYMOUS_PROCEDURE);
    assert(ctx != NULL);
    assert(symtab != NULL);
    
    struct AnonymousMethod *anon = &expr->expr_data.anonymous_method_data;
    
    if (anon->generated_name == NULL)
    {
        codegen_report_error(ctx, "ERROR: Anonymous method missing generated name at line %d", expr->line_num);
        return;
    }
    
    if (anon->body == NULL)
    {
        /* Empty body - generate a no-op function */
        #ifdef DEBUG_CODEGEN
        CODEGEN_DEBUG("DEBUG: Anonymous method %s has no body, generating no-op\n", anon->generated_name);
        #endif
    }
    
    const char *prev_sub_id = ctx->current_subprogram_id;
    const char *prev_sub_mangled = ctx->current_subprogram_mangled;
    
    push_stackscope();
    
    ListNode_t *inst_list = NULL;
    int num_args = (anon->parameters == NULL) ? 0 : ListLength(anon->parameters);
    int lexical_depth = codegen_get_lexical_depth(ctx) + 1;
    int prev_depth = ctx->current_subprogram_lexical_depth;
    ctx->current_subprogram_lexical_depth = lexical_depth;
    int is_nested = (lexical_depth >= 1);
    
    ctx->current_subprogram_id = anon->generated_name;
    ctx->current_subprogram_mangled = anon->generated_name;
    
    /* Anonymous methods are always nested (they're defined inside some other context).
     * They always need a static link to access variables from their parent scope (closure).
     * The static link is passed in %rdi (first register) and parameters are shifted by 1.
     */
    StackNode_t *static_link = NULL;
    int will_need_static_link = is_nested;
    int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;
    
    /* Process parameters (convert from TREE_VAR_DECL to stack allocations) */
    inst_list = codegen_subprogram_arguments(anon->parameters, inst_list, ctx, symtab, arg_start_index);
    
    /* Add static link after parameters */
    if (will_need_static_link)
    {
        static_link = add_l_x("__static_link__", 8);
        codegen_register_static_link_proc(ctx, anon->generated_name, lexical_depth);
        
        if (static_link != NULL)
        {
            char buffer[64];
            /* Static link always comes in %rdi (first register) */
            snprintf(buffer, sizeof(buffer), "\tmovq\t%%rdi, -%d(%%rbp)\n", static_link->offset);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    
    /* For functions (not procedures), allocate space for the return value */
    StackNode_t *return_var = NULL;
    if (anon->is_function && anon->return_type != -1)
    {
        int return_size = get_return_type_size(anon->return_type);
        return_var = add_l_x(anon->generated_name, return_size);
        
        /* Also add "Result" as an alias at the same stack offset */
        add_result_alias_for_return_var(return_var);
    }
    
    /* No local variable declarations in anonymous methods (they're inline) */
    /* No nested subprograms in anonymous methods */
    
    /* Generate the body */
    if (anon->body != NULL)
    {
        inst_list = codegen_stmt(anon->body, inst_list, ctx, symtab);
    }
    
    /* For functions, move return value to correct return register */
    if (anon->is_function && return_var != NULL)
    {
        char buffer[64];
        int uses_qword = (anon->return_type == LONGINT_TYPE || anon->return_type == STRING_TYPE || 
                         anon->return_type == REAL_TYPE || anon->return_type == POINTER_TYPE);
        if (uses_qword)
        {
            if (anon->return_type == REAL_TYPE)
                snprintf(buffer, sizeof(buffer), "\tmovsd\t-%d(%%rbp), %%xmm0\n", return_var->offset);
            else
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n", return_var->offset);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%eax\n", return_var->offset);
        }
        inst_list = add_inst(inst_list, buffer);
    }
    
    /* Generate the function header, stack allocation, body, and footer */
    codegen_function_header(anon->generated_name, ctx);
    codegen_stack_space(ctx);
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(anon->generated_name, ctx);
    
    free_inst_list(inst_list);
    pop_stackscope();
    
    ctx->current_subprogram_id = prev_sub_id;
    ctx->current_subprogram_mangled = prev_sub_mangled;
    ctx->current_subprogram_lexical_depth = prev_depth;
    
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
    ListNode_t *arg_ids;
    const char *arg_reg;
    char buffer[50];
    StackNode_t *arg_stack;
    int next_gpr_index = 0;
    int next_sse_index = 0;
    int stack_arg_offset = 16;

    assert(ctx != NULL);

    if (arg_start_index < 0)
        arg_start_index = 0;

    next_gpr_index = arg_start_index;

    /* Pre-pass: Save all parameter registers to temp stack slots to prevent them from being
     * clobbered during gpc_move setup for record parameters. This is necessary because
     * processing one record parameter may clobber registers containing subsequent parameters. */
    ListNode_t *args_scan = args;
    int scan_gpr_index = arg_start_index;
    while(args_scan != NULL)
    {
        Tree_t *scan_decl = (Tree_t *)args_scan->cur;
        if (scan_decl->type == TREE_VAR_DECL)
        {
            ListNode_t *scan_ids = scan_decl->tree_data.var_decl_data.ids;
            while(scan_ids != NULL)
            {
                const char *param_reg = alloc_integer_arg_reg(1, &scan_gpr_index);
                if (param_reg != NULL)
                {
                    /* Allocate temp slot and save parameter register */
                    char temp_name[64];
                    snprintf(temp_name, sizeof(temp_name), "__param_%s__", (char *)scan_ids->cur);
                    StackNode_t *temp_slot = add_l_x(temp_name, CODEGEN_POINTER_SIZE_BYTES);
                    if (temp_slot != NULL)
                    {
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n",
                            param_reg, temp_slot->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                }
                scan_ids = scan_ids->next;
            }
        }
        args_scan = args_scan->next;
    }
    
    /* Reset for main processing pass */
    next_gpr_index = arg_start_index;

    while(args != NULL)
    {
        arg_decl = (Tree_t *)args->cur;
        switch(arg_decl->type)
        {
            case TREE_VAR_DECL:
                arg_ids = arg_decl->tree_data.var_decl_data.ids;
                type = arg_decl->tree_data.var_decl_data.type;
                HashNode_t *resolved_type_node = NULL;
                GpcType *cached_arg_type = arg_decl->tree_data.var_decl_data.cached_gpc_type;
                HashNode_t cached_arg_node;
                HashNode_t *cached_arg_node_ptr = NULL;
                if (cached_arg_type != NULL)
                {
                    memset(&cached_arg_node, 0, sizeof(cached_arg_node));
                    cached_arg_node.type = cached_arg_type;
                    cached_arg_node_ptr = &cached_arg_node;
                }

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
                    int symbol_is_var_param = tree_is_var_param;
                    struct RecordType *record_type_info = NULL;
                    if (!symbol_is_var_param)
                    {
                        if (resolved_type_node != NULL)
                            record_type_info = get_record_type_from_node(resolved_type_node);
                        if (record_type_info == NULL && cached_arg_node_ptr != NULL)
                            record_type_info = get_record_type_from_node(cached_arg_node_ptr);
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

                        arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);
                        Register_t *stack_value_reg = NULL;
                        
                        /* Load parameter from temp slot that was saved in pre-pass */
                        char temp_name[64];
                        snprintf(temp_name, sizeof(temp_name), "__param_%s__", (char *)arg_ids->cur);
                        StackNode_t *temp_param_slot = find_label(temp_name);
                        
                        const char *record_src_reg = NULL;
                        Register_t *loaded_param_reg = NULL;
                        
                        if (temp_param_slot != NULL)
                        {
                            /* Load from temp slot */
                            loaded_param_reg = get_free_reg(get_reg_stack(), &inst_list);
                            if (loaded_param_reg != NULL)
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                                    temp_param_slot->offset, loaded_param_reg->bit_64);
                                inst_list = add_inst(inst_list, buffer);
                                record_src_reg = loaded_param_reg->bit_64;
                            }
                        }
                        else if (arg_reg != NULL)
                        {
                            /* Fallback to register (shouldn't happen if pre-pass worked) */
                            record_src_reg = arg_reg;
                        }
                        
                        if (record_src_reg == NULL)
                        {
                            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                            if (stack_value_reg == NULL)
                            {
                                codegen_report_error(ctx,
                                    "ERROR: Unable to allocate register for record parameter %s.",
                                    (char *)arg_ids->cur);
                                return inst_list;
                            }
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                                stack_arg_offset, stack_value_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                            record_src_reg = stack_value_reg->bit_64;
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
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", record_src_reg);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %%rcx\n", record_slot->offset);
                            inst_list = add_inst(inst_list, buffer);
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", size_reg->bit_64);
                            inst_list = add_inst(inst_list, buffer);
                        }
                        else
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", record_src_reg);
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
                        if (stack_value_reg != NULL)
                            free_reg(get_reg_stack(), stack_value_reg);
                        if (loaded_param_reg != NULL)
                            free_reg(get_reg_stack(), loaded_param_reg);

                        arg_ids = arg_ids->next;
                        continue;
                    }

                    // Var parameters are passed by reference (as pointers), so always use 64-bit
                    // Also use 64-bit for strings, explicit pointers, and arrays
                    int is_var_param = symbol_is_var_param;
                    int is_array_type = 0;
                    
                    /* Determine if parameter is an array type via resolved type only */
                    if (resolved_type_node != NULL && resolved_type_node->type != NULL &&
                             gpc_type_is_array(resolved_type_node->type))
                    {
                        is_array_type = 1;
                    }
                    else if (cached_arg_type != NULL &&
                        gpc_type_is_array(cached_arg_type))
                    {
                        is_array_type = 1;
                    }
                    
                    int use_64bit = is_var_param || is_array_type ||
                        (type == STRING_TYPE || type == POINTER_TYPE ||
                         type == REAL_TYPE || type == LONGINT_TYPE || type == PROCEDURE);
                    int use_sse_reg = (!is_var_param && !is_array_type &&
                        type == REAL_TYPE);
                    arg_stack = use_64bit ? add_q_z((char *)arg_ids->cur) : add_l_z((char *)arg_ids->cur);
                    if (arg_stack != NULL && (symbol_is_var_param || is_array_type))
                        arg_stack->is_reference = 1;
                    if (use_sse_reg)
                    {
                        const char *xmm_reg = alloc_sse_arg_reg(&next_sse_index);
                        snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, -%d(%%rbp)\n",
                            xmm_reg, arg_stack->offset);
                        inst_list = add_inst(inst_list, buffer);
                    }
                    else
                    {
                        arg_reg = alloc_integer_arg_reg(use_64bit, &next_gpr_index);
                        Register_t *stack_value_reg = NULL;
                        const char *value_source = arg_reg;
                        if (value_source == NULL)
                        {
                            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                            if (stack_value_reg == NULL)
                            {
                                codegen_report_error(ctx,
                                    "ERROR: Unable to allocate register for argument %s.",
                                    (char *)arg_ids->cur);
                                return inst_list;
                            }
                            if (use_64bit)
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                                    stack_arg_offset, stack_value_reg->bit_64);
                            }
                            else
                            {
                                snprintf(buffer, sizeof(buffer), "\tmovl\t%d(%%rbp), %s\n",
                                    stack_arg_offset, stack_value_reg->bit_32);
                            }
                            inst_list = add_inst(inst_list, buffer);
                            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                            value_source = use_64bit ? stack_value_reg->bit_64 : stack_value_reg->bit_32;
                        }

                        if (use_64bit)
                            snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", value_source, arg_stack->offset);
                        else
                            snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", value_source, arg_stack->offset);
                        inst_list = add_inst(inst_list, buffer);
                        if (stack_value_reg != NULL)
                            free_reg(get_reg_stack(), stack_value_reg);
                    }
                    arg_ids = arg_ids->next;
                }
                break;
            case TREE_ARR_DECL:
                arg_ids = arg_decl->tree_data.arr_decl_data.ids;
                while(arg_ids != NULL)
                {
                    arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);
                    arg_stack = add_q_z((char *)arg_ids->cur);
                    if (arg_stack != NULL)
                        arg_stack->is_reference = 1;
                    Register_t *stack_value_reg = NULL;
                    const char *value_source = arg_reg;
                    if (value_source == NULL)
                    {
                        stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
                        if (stack_value_reg == NULL)
                        {
                            codegen_report_error(ctx,
                                "ERROR: Unable to allocate register for array argument %s.",
                                (char *)arg_ids->cur);
                            return inst_list;
                        }
                        snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                            stack_arg_offset, stack_value_reg->bit_64);
                        inst_list = add_inst(inst_list, buffer);
                        stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
                        value_source = stack_value_reg->bit_64;
                    }
                    snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", value_source, arg_stack->offset);
                    inst_list = add_inst(inst_list, buffer);
                    if (stack_value_reg != NULL)
                        free_reg(get_reg_stack(), stack_value_reg);
                    arg_ids = arg_ids->next;
                }
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

static ListNode_t *codegen_store_class_typeinfo(ListNode_t *inst_list,
    CodeGenContext *ctx, StackNode_t *var_node, const char *type_name)
{
    (void)ctx;
    if (var_node == NULL || type_name == NULL || type_name[0] == '\0' || var_node->is_reference)
        return inst_list;

    char typeinfo_label[512];
    snprintf(typeinfo_label, sizeof(typeinfo_label), "%s_TYPEINFO", type_name);

    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %%rax\n", typeinfo_label);
    inst_list = add_inst(inst_list, buffer);

    if (var_node->is_static)
    {
        const char *label = var_node->static_label != NULL ? var_node->static_label : var_node->label;
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s(%%rip)\n", label);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", var_node->offset);
    }
    return add_inst(inst_list, buffer);
}

static ListNode_t *codegen_emit_tfile_configure(ListNode_t *inst_list,
    StackNode_t *file_node, long long element_size, int element_hash_tag)
{
    if (file_node == NULL || element_size <= 0)
        return inst_list;

    const char *slot_reg = current_arg_reg64(0);
    const char *size_reg = current_arg_reg64(1);
    const char *tag_reg = current_arg_reg32(2);
    if (slot_reg == NULL || size_reg == NULL || tag_reg == NULL)
        return inst_list;

    char buffer[256];
    if (file_node->is_static)
    {
        const char *label = (file_node->static_label != NULL) ?
            file_node->static_label : file_node->label;
        snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label, slot_reg);
    }
    else
    {
        snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
            file_node->offset, slot_reg);
    }
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", element_size, size_reg);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_hash_tag, tag_reg);
    inst_list = add_inst(inst_list, buffer);

    inst_list = add_inst(inst_list, "\tcall\tgpc_tfile_configure\n");
    return inst_list;
}

static int codegen_type_tag_to_hashvar(int parser_tag)
{
    switch (parser_tag)
    {
        case CHAR_TYPE:
            return HASHVAR_CHAR;
        case BOOL:
            return HASHVAR_BOOLEAN;
        case LONGINT_TYPE:
            return HASHVAR_LONGINT;
        case REAL_TYPE:
            return HASHVAR_REAL;
        case INT_TYPE:
            return HASHVAR_INTEGER;
        default:
            return HASHVAR_INTEGER;
    }
}

static long long codegen_type_tag_size(int parser_tag)
{
    switch (parser_tag)
    {
        case CHAR_TYPE:
        case BOOL:
            return 1;
        case LONGINT_TYPE:
            return 8;
        case REAL_TYPE:
            return 8;
        case INT_TYPE:
        default:
            return 4;
    }
}

static int codegen_resolve_file_component(const struct TypeAlias *alias, SymTab_t *symtab,
    long long *element_size_out, int *element_hash_tag_out)
{
    if (alias == NULL || !alias->is_file || element_size_out == NULL || element_hash_tag_out == NULL)
        return 0;

    int parser_tag = alias->file_type;
    HashNode_t *type_node = NULL;
    if (parser_tag == UNKNOWN_TYPE && alias->file_type_id != NULL && symtab != NULL)
    {
        if (FindIdent(&type_node, symtab, (char *)alias->file_type_id) >= 0 && type_node != NULL)
        {
            if (type_node->type != NULL)
                parser_tag = gpc_type_get_primitive_tag(type_node->type);
        }
    }

    if (parser_tag == UNKNOWN_TYPE && type_node != NULL && type_node->type != NULL)
        parser_tag = gpc_type_get_primitive_tag(type_node->type);

    if (parser_tag == UNKNOWN_TYPE)
        parser_tag = INT_TYPE;

    long long elem_size = codegen_type_tag_size(parser_tag);
    int hash_tag = codegen_type_tag_to_hashvar(parser_tag);

    if (type_node != NULL && type_node->type != NULL)
    {
        long long resolved_size = gpc_type_sizeof(type_node->type);
        if (resolved_size > 0)
            elem_size = resolved_size;

        int resolved_tag = gpc_type_get_primitive_tag(type_node->type);
        if (resolved_tag != UNKNOWN_TYPE)
            hash_tag = codegen_type_tag_to_hashvar(resolved_tag);
    }

    *element_size_out = elem_size;
    *element_hash_tag_out = hash_tag;
    return 1;
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
                    if (array_node != NULL && array_node->is_dynamic && array_node->offset > 0)
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
            if ((type_node != NULL && node_is_file_type(type_node)) ||
                (type_node == NULL && decl->tree_data.var_decl_data.type == FILE_TYPE))
            {
                struct TypeAlias *decl_inline_alias = decl->tree_data.var_decl_data.inline_type_alias;
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *file_node = find_label(var_name);
                    if (file_node != NULL)
                    {
                        char buffer[128];
                        if (!file_node->is_static)
                        {
                            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", file_node->offset);
                            inst_list = add_inst(inst_list, buffer);
                        }

                        long long file_elem_size = 0;
                        int file_elem_hash = HASHVAR_INTEGER;
                        struct TypeAlias *file_alias = get_type_alias_from_node(type_node);
                        if (file_alias == NULL && decl_inline_alias != NULL)
                            file_alias = decl_inline_alias;
                        if (file_alias == NULL || !file_alias->is_file)
                        {
                            HashNode_t *var_hash = NULL;
                            if (FindIdent(&var_hash, symtab, var_name) >= 0 && var_hash != NULL)
                                file_alias = hashnode_get_type_alias(var_hash);
                        }

                        int have_component = codegen_resolve_file_component(
                            file_alias, symtab, &file_elem_size, &file_elem_hash);

                        if (have_component)
                        {
                            inst_list = codegen_emit_tfile_configure(inst_list,
                                file_node, file_elem_size, file_elem_hash);
                        }
                    }
                    ids = ids->next;
                }
            }

            struct Statement *init_stmt = decl->tree_data.var_decl_data.initializer;
            if (type_node != NULL && node_is_class_type(type_node))
            {
                struct RecordType *record_desc = get_record_type_from_node(type_node);
                const char *class_type_name = (record_desc != NULL && record_desc->type_id != NULL) ?
                    record_desc->type_id : codegen_resolve_record_type_name(type_node, symtab);
                ListNode_t *ids = decl->tree_data.var_decl_data.ids;
                while (ids != NULL)
                {
                    char *var_name = (char *)ids->cur;
                    StackNode_t *var_node = find_label(var_name);
                    inst_list = codegen_store_class_typeinfo(inst_list, ctx, var_node,
                        class_type_name);
                    ids = ids->next;
                }
            }

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
                    if (array_node != NULL && array_node->is_dynamic && array_node->offset > 0)
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
#if GPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif
