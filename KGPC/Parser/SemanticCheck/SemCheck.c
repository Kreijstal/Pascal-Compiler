/*
    Damon Gwinn
    Performs semantic checking on a given parse tree

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    TODO: CHECK FOR RETURN IN FUNCTIONS (Add a "referenced" flag to symbol table elements. Need it for optimizations anyway...)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <time.h>
#ifndef _WIN32
#include <strings.h>
#include <unistd.h>
#include <fcntl.h>
#else
#define strcasecmp _stricmp
#include <io.h>
#include <fcntl.h>
#endif
#include <math.h>
#include "SemCheck.h"
#include "../ParseTree/ident_ref.h"
#include "SemChecks/SemCheck_sizeof.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "../../string_intern.h"
#include "../../Optimizer/optimizer.h"
#include "../pascal_frontend.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"

/* From SemCheck_Expr_Constructors.c */
int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num);
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/type_tags.h"

HashNode_t *semcheck_find_type_node_in_owner_chain(SymTab_t *symtab,
    const char *type_id, const char *owner_full, const char *owner_outer);
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/from_cparser.h"
#include "../ParseTree/operator_registry.h"
#include "../ParseTree/generic_types.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"
#include <stdarg.h>

void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

/* Cached getenv() for KGPC_* environment variables.
 * getenv() does a linear scan of the environment on each call; with hundreds of
 * debug checks in hot loops, this was consuming 9% of total CPU time.
 * Uses a simple linear-probe cache (env var names are always string literals). */
#define KGPC_ENV_CACHE_SIZE 128
static struct {
    const char *name;   /* pointer to string literal (identity comparison) */
    const char *value;  /* cached getenv result (NULL or pointer) */
} g_env_cache[KGPC_ENV_CACHE_SIZE];
static int g_env_cache_count = 0;

const char *kgpc_getenv(const char *name) {
    /* Linear search — fast for <100 unique names with pointer identity */
    for (int i = 0; i < g_env_cache_count; i++) {
        if (g_env_cache[i].name == name)
            return g_env_cache[i].value;
    }
    /* Cache miss — do the real getenv and store */
    const char *val = getenv(name);
    if (g_env_cache_count < KGPC_ENV_CACHE_SIZE) {
        g_env_cache[g_env_cache_count].name = name;
        g_env_cache[g_env_cache_count].value = val;
        g_env_cache_count++;
    }
    return val;
}

#ifdef _WIN32
/* Windows CRT does not provide strndup; caller owns returned buffer. */
static char* strndup(const char* s, size_t n)
{
    size_t len = strnlen(s, n);
    char* buf = (char*)malloc(len + 1);
    if (buf == NULL)
        return NULL;
    memcpy(buf, s, len);
    buf[len] = '\0';
    return buf;
}
#endif

static ListNode_t *g_semcheck_unit_names = NULL;
static int g_semcheck_current_unit_index = 0;
static int g_semcheck_imported_decl_unit_index = 0;
static char *g_semcheck_source_path = NULL;
static char *g_semcheck_source_buffer = NULL;
static size_t g_semcheck_source_length = 0;
static int g_semcheck_warning_count = 0;

#define MAX_SOURCE_BUFFERS 64
static struct {
    char *path;
    char *buffer;
    size_t length;
    int global_start;  /* cumulative offset for globally-unique source_index values */
} g_source_buffer_registry[MAX_SOURCE_BUFFERS];
static int g_source_buffer_count = 0;
static int g_source_buffer_next_global = 0; /* next available global offset */

void semcheck_set_source_path(const char *path)
{
    if (g_semcheck_source_path != NULL)
    {
        free(g_semcheck_source_path);
        g_semcheck_source_path = NULL;
    }
    if (path != NULL)
        g_semcheck_source_path = strdup(path);
}

void semcheck_set_source_buffer(const char *buffer, size_t length)
{
    if (g_semcheck_source_buffer != NULL)
    {
        free(g_semcheck_source_buffer);
        g_semcheck_source_buffer = NULL;
    }
    g_semcheck_source_length = 0;

    if (buffer == NULL || length == 0)
        return;

    g_semcheck_source_buffer = (char *)malloc(length + 1);
    if (g_semcheck_source_buffer == NULL)
        return;

    memcpy(g_semcheck_source_buffer, buffer, length);
    g_semcheck_source_buffer[length] = '\0';
    g_semcheck_source_length = length;
}

int semcheck_register_source_buffer(const char *path, const char *buffer, size_t length)
{
    if (path == NULL || buffer == NULL || length == 0)
        return 0;
    if (g_source_buffer_count >= MAX_SOURCE_BUFFERS)
        return 0;

    for (int i = 0; i < g_source_buffer_count; i++)
    {
        if (g_source_buffer_registry[i].path != NULL &&
            strcmp(g_source_buffer_registry[i].path, path) == 0)
        {
            return g_source_buffer_registry[i].global_start;
        }
    }

    int global_start = g_source_buffer_next_global;
    g_source_buffer_registry[g_source_buffer_count].path = strdup(path);
    g_source_buffer_registry[g_source_buffer_count].buffer = (char *)malloc(length + 1);
    if (g_source_buffer_registry[g_source_buffer_count].path &&
        g_source_buffer_registry[g_source_buffer_count].buffer)
    {
        memcpy(g_source_buffer_registry[g_source_buffer_count].buffer, buffer, length);
        g_source_buffer_registry[g_source_buffer_count].buffer[length] = '\0';
        g_source_buffer_registry[g_source_buffer_count].length = length;
        g_source_buffer_registry[g_source_buffer_count].global_start = global_start;
        g_source_buffer_count++;
        g_source_buffer_next_global = global_start + (int)length + 1; /* +1 to avoid overlap */
    }
    else
    {
        free(g_source_buffer_registry[g_source_buffer_count].path);
        free(g_source_buffer_registry[g_source_buffer_count].buffer);
    }
    return global_start;
}

static int semcheck_print_context_from_file(const char *file_path, int line_num, int col_num, int context_lines)
{
    if (file_path == NULL || line_num <= 0)
        return 0;

    FILE *fp = fopen(file_path, "rb");
    if (fp == NULL)
        return 0;

    if (fseek(fp, 0, SEEK_END) != 0)
    {
        fclose(fp);
        return 0;
    }

    long size = ftell(fp);
    if (size < 0)
    {
        fclose(fp);
        return 0;
    }
    if (fseek(fp, 0, SEEK_SET) != 0)
    {
        fclose(fp);
        return 0;
    }

    char *buffer = (char *)malloc((size_t)size + 1);
    if (buffer == NULL)
    {
        fclose(fp);
        return 0;
    }

    size_t read_len = fread(buffer, 1, (size_t)size, fp);
    fclose(fp);
    buffer[read_len] = '\0';

    int printed = print_source_context_from_buffer(buffer, read_len, line_num, col_num, context_lines);
    free(buffer);
    return printed;
}

static void semcheck_unit_names_reset(void)
{
    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        if (cur->cur != NULL)
            free(cur->cur);
        cur = cur->next;
    }
    DestroyList(g_semcheck_unit_names);
    g_semcheck_unit_names = NULL;
    g_semcheck_current_unit_index = 0;
}

int semcheck_save_unit_context(void)
{
    return g_semcheck_current_unit_index;
}

void semcheck_restore_unit_context(int saved)
{
    g_semcheck_current_unit_index = saved;
}

static void semcheck_unit_name_add(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return;

    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        const char *existing = (const char *)cur->cur;
        if (existing != NULL && pascal_identifier_equals(existing, name))
            return;
        cur = cur->next;
    }

    char *dup = strdup(name);
    if (dup == NULL)
        return;

    ListNode_t *node = CreateListNode(dup, LIST_STRING);
    if (node == NULL)
    {
        free(dup);
        return;
    }

    if (g_semcheck_unit_names == NULL)
        g_semcheck_unit_names = node;
    else
    {
        ListNode_t *tail = g_semcheck_unit_names;
        while (tail->next != NULL)
            tail = tail->next;
        tail->next = node;
    }
}

static void semcheck_unit_names_add_list(ListNode_t *units)
{
    ListNode_t *cur = units;
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            semcheck_unit_name_add((const char *)cur->cur);
        cur = cur->next;
    }
}

static void semcheck_mark_type_decl_units(ListNode_t *type_decls, int unit_index)
{
    if (type_decls == NULL || unit_index <= 0)
        return;

    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_TYPE_DECL)
            continue;
        if (tree->tree_data.type_decl_data.source_unit_index == 0)
            tree->tree_data.type_decl_data.source_unit_index = unit_index;
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
            tree->tree_data.type_decl_data.info.record != NULL &&
            tree->tree_data.type_decl_data.info.record->source_unit_index == 0)
        {
            tree->tree_data.type_decl_data.info.record->source_unit_index = unit_index;
        }
    }
}

static void semcheck_mark_subprogram_units(ListNode_t *subprograms, int unit_index)
{
    if (subprograms == NULL || unit_index <= 0)
        return;

    for (ListNode_t *cur = subprograms; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;

        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_SUBPROGRAM)
            continue;

        if (tree->tree_data.subprogram_data.source_unit_index == 0)
            tree->tree_data.subprogram_data.source_unit_index = unit_index;
    }
}

static void semcheck_mark_resolved_forward_stub(ListNode_t *type_decls, ListNode_t *limit,
    const char *type_id, int source_unit_index, const struct RecordType *canonical_record)
{
    if (type_decls == NULL || type_id == NULL || canonical_record == NULL)
        return;

    for (ListNode_t *cur = type_decls; cur != NULL && cur != limit; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;

        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_TYPE_DECL ||
            tree->tree_data.type_decl_data.kind != TYPE_DECL_RECORD ||
            tree->tree_data.type_decl_data.id == NULL)
            continue;

        if (!pascal_identifier_equals(tree->tree_data.type_decl_data.id, type_id))
            continue;

        if (source_unit_index != 0 &&
            tree->tree_data.type_decl_data.source_unit_index != 0 &&
            tree->tree_data.type_decl_data.source_unit_index != source_unit_index)
            continue;

        if (tree->tree_data.type_decl_data.info.record != canonical_record)
            continue;

        tree->tree_data.type_decl_data.suppress_codegen = 1;
        return;
    }
}

static struct RecordType *semcheck_record_from_type_decl(Tree_t *tree)
{
    if (tree == NULL || tree->type != TREE_TYPE_DECL)
        return NULL;

    KgpcType *kgpc = tree->tree_data.type_decl_data.kgpc_type;
    if (kgpc != NULL)
    {
        if (kgpc->kind == TYPE_KIND_RECORD && kgpc->info.record_info != NULL)
            return kgpc->info.record_info;
        if (kgpc->kind == TYPE_KIND_POINTER &&
            kgpc->info.points_to != NULL &&
            kgpc->info.points_to->kind == TYPE_KIND_RECORD &&
            kgpc->info.points_to->info.record_info != NULL)
            return kgpc->info.points_to->info.record_info;
    }

    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
        return tree->tree_data.type_decl_data.info.record;

    return NULL;
}

static int semcheck_record_candidate_is_forward_stub(struct RecordType *record)
{
    if (record == NULL)
        return 0;
    if (!record->is_class && !record->is_interface)
        return 0;

    int has_non_hidden_field = 0;
    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (!record_field_is_hidden(field))
        {
            has_non_hidden_field = 1;
            break;
        }
    }

    if (has_non_hidden_field)
        return 0;
    int has_meaningful_property = 0;
    for (ListNode_t *cur = record->properties; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_CLASS_PROPERTY || cur->cur == NULL)
            continue;
        struct ClassProperty *prop = (struct ClassProperty *)cur->cur;
        if (prop->name == NULL)
            continue;
        if (!pascal_identifier_equals(prop->name, "Disposed"))
        {
            has_meaningful_property = 1;
            break;
        }
    }
    if (has_meaningful_property)
        return 0;
    if (record->method_templates != NULL)
        return 0;
    if (record->parent_class_name != NULL &&
        !pascal_identifier_equals(record->parent_class_name, "TObject"))
        return 0;

    return 1;
}

static int semcheck_type_candidate_is_forward_stub(HashNode_t *node)
{
    if (node == NULL)
        return 0;

    struct RecordType *record = hashnode_get_record_type(node);
    if (record == NULL && node->type != NULL &&
        node->type->kind == TYPE_KIND_POINTER &&
        node->type->info.points_to != NULL &&
        node->type->info.points_to->kind == TYPE_KIND_RECORD)
    {
        record = node->type->info.points_to->info.record_info;
    }

    return semcheck_record_candidate_is_forward_stub(record);
}

int semcheck_is_unit_name(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return 0;
    if (pascal_identifier_equals(name, "System"))
        return 1;
    if (g_semcheck_current_unit_index > 0)
    {
        const char *cur_name = unit_registry_get(g_semcheck_current_unit_index);
        if (cur_name != NULL && pascal_identifier_equals(name, cur_name))
            return 1;
    }

    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        const char *existing = (const char *)cur->cur;
        if (existing != NULL && pascal_identifier_equals(existing, name))
            return 1;
        cur = cur->next;
    }
    /* Treat any loaded unit name as a valid qualifier.  The call site
     * (semcheck_recordaccess) already checks semcheck_has_value_ident()
     * so local identifiers are preferred over unit names automatically. */
    if (unit_registry_contains(name))
        return 1;
    return 0;
}

/* Helper declared in SemCheck_expr.c */
static const char *semcheck_base_type_name(const char *id)
{
    /* Type identifiers are normalized before semcheck; avoid parsing strings here. */
    return id;
}

static char *type_ref_render_mangled_unqualified(const TypeRef *ref)
{
    if (ref == NULL)
        return NULL;
    const char *base = type_ref_base_name(ref);
    if (base == NULL)
        return NULL;
    if (ref->num_generic_args <= 0)
        return strdup(base);

    size_t total = strlen(base) + 1;
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_mangled(ref->generic_args[i]);
        if (arg != NULL)
        {
            total += strlen(arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            total += 1;
    }

    char *out = (char *)malloc(total + 1);
    if (out == NULL)
        return NULL;
    out[0] = '\0';
    strcat(out, base);
    strcat(out, "$");
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_mangled(ref->generic_args[i]);
        if (arg != NULL)
        {
            strcat(out, arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            strcat(out, "$");
    }
    return out;
}

static int semcheck_is_explicit_unit_qualified_type_ref(const TypeRef *ref)
{
    if (ref == NULL || ref->name == NULL || ref->name->count < 2)
        return 0;
    const char *prefix = ref->name->segments[0];
    return semcheck_is_unit_name(prefix);
}

HashNode_t *semcheck_find_preferred_type_node_with_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id);
static HashNode_t *semcheck_find_exact_type_node_for_qid(SymTab_t *symtab,
    const QualifiedIdent *type_ref);

static void semcheck_maybe_qualify_nested_type(SymTab_t *symtab,
    const char *owner_full, const char *owner_outer,
    char **type_id, TypeRef **type_ref)
{
    if (symtab == NULL || owner_full == NULL || type_id == NULL || *type_id == NULL)
        return;

    TypeRef *ref = type_ref != NULL ? *type_ref : NULL;
    if (ref != NULL && ref->name != NULL && ref->name->count > 1)
        return;

    if (semcheck_find_preferred_type_node_with_ref(symtab, ref, *type_id) != NULL)
        return;

    const char *owners[2] = { owner_full, owner_outer };
    for (int i = 0; i < 2; ++i)
    {
        const char *owner = owners[i];
        if (owner == NULL || owner[0] == '\0')
            continue;
        size_t owner_len = strlen(owner);
        size_t type_len = strlen(*type_id);
        char *qualified_name = (char *)malloc(owner_len + 1 + type_len + 1);
        if (qualified_name == NULL)
            continue;
        snprintf(qualified_name, owner_len + 1 + type_len + 1, "%s.%s", owner, *type_id);
        if (semcheck_find_preferred_type_node(symtab, qualified_name) != NULL)
        {
            char *orig_id = *type_id;
            char *qualified_copy = strdup(qualified_name);
            if (qualified_copy != NULL)
            {
                *type_id = qualified_copy;
                free(orig_id);
                if (type_ref != NULL && *type_ref != NULL)
                {
                    type_ref_free(*type_ref);
                    *type_ref = NULL;
                }
            }
            free(qualified_name);
            break;
        }
        free(qualified_name);
    }
}

static void semcheck_qualify_nested_types_for_record(SymTab_t *symtab, struct RecordType *record_info)
{
    if (symtab == NULL || record_info == NULL || record_info->type_id == NULL)
        return;

    ListNode_t *fnode = record_info->fields;
    while (fnode != NULL)
    {
        if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)fnode->cur;
            semcheck_maybe_qualify_nested_type(symtab, record_info->type_id, NULL,
                &field->type_id, &field->type_ref);
            semcheck_maybe_qualify_nested_type(symtab, record_info->type_id, NULL,
                &field->array_element_type_id, &field->array_element_type_ref);
            semcheck_maybe_qualify_nested_type(symtab, record_info->type_id, NULL,
                &field->pointer_type_id, &field->pointer_type_ref);
            /* Resolve proc_type for fields with named procedural types.
             * This avoids string parsing hacks later when detecting procedural
             * field calls (e.g., FCallBack(args) where FCallBack's type_id is a
             * dot-qualified nested type like TInterfaceThunk.TThunkCallback). */
            if (field->proc_type == NULL && field->type_id != NULL)
            {
                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                    field->type_id);
                if (type_node != NULL && type_node->type != NULL &&
                    type_node->type->kind == TYPE_KIND_PROCEDURE)
                {
                    field->proc_type = type_node->type;
                    kgpc_type_retain(field->proc_type);
                }
            }
        }
        fnode = fnode->next;
    }

    ListNode_t *pnode = record_info->properties;
    while (pnode != NULL)
    {
        if (pnode->type == LIST_CLASS_PROPERTY && pnode->cur != NULL)
        {
            struct ClassProperty *prop = (struct ClassProperty *)pnode->cur;
            semcheck_maybe_qualify_nested_type(symtab, record_info->type_id, NULL,
                &prop->type_id, &prop->type_ref);
        }
        pnode = pnode->next;
    }

    ListNode_t *rnode = record_info->record_properties;
    while (rnode != NULL)
    {
        if (rnode->type == LIST_CLASS_PROPERTY && rnode->cur != NULL)
        {
            struct ClassProperty *prop = (struct ClassProperty *)rnode->cur;
            semcheck_maybe_qualify_nested_type(symtab, record_info->type_id, NULL,
                &prop->type_id, &prop->type_ref);
        }
        rnode = rnode->next;
    }
}

static int semcheck_map_builtin_type_name_local(const char *id)
{
    if (id == NULL)
        return UNKNOWN_TYPE;
    id = semcheck_base_type_name(id);

    if (pascal_identifier_equals(id, "Integer"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "LongInt"))
        return LONGINT_TYPE;
    if (pascal_identifier_equals(id, "Int64"))
        return INT64_TYPE;
    if (pascal_identifier_equals(id, "Real") ||
        pascal_identifier_equals(id, "Float") ||
        pascal_identifier_equals(id, "ValReal"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "Single") ||
        pascal_identifier_equals(id, "Double"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "Extended"))
        return EXTENDED_TYPE;
    if (pascal_identifier_equals(id, "Byte") ||
        pascal_identifier_equals(id, "UInt8"))
        return BYTE_TYPE;
    if (pascal_identifier_equals(id, "Word") ||
        pascal_identifier_equals(id, "UInt16"))
        return WORD_TYPE;
    if (pascal_identifier_equals(id, "LongWord") ||
        pascal_identifier_equals(id, "Cardinal") ||
        pascal_identifier_equals(id, "DWord") ||
        pascal_identifier_equals(id, "UInt32"))
        return LONGWORD_TYPE;
    if (pascal_identifier_equals(id, "QWord") ||
        pascal_identifier_equals(id, "UInt64"))
        return QWORD_TYPE;
    if (pascal_identifier_equals(id, "ShortInt") ||
        pascal_identifier_equals(id, "SmallInt") ||
        pascal_identifier_equals(id, "Int8") ||
        pascal_identifier_equals(id, "Int16") ||
        pascal_identifier_equals(id, "Int32"))
        return INT_TYPE;
    if (pascal_identifier_equals(id, "NativeInt") ||
        pascal_identifier_equals(id, "PtrInt") ||
        pascal_identifier_equals(id, "SizeInt") ||
        pascal_identifier_equals(id, "IntPtr"))
        return INT64_TYPE;
    if (pascal_identifier_equals(id, "NativeUInt") ||
        pascal_identifier_equals(id, "PtrUInt") ||
        pascal_identifier_equals(id, "SizeUInt") ||
        pascal_identifier_equals(id, "UIntPtr"))
        return QWORD_TYPE;
    if (pascal_identifier_equals(id, "String") ||
        pascal_identifier_equals(id, "OpenString") ||
        pascal_identifier_equals(id, "AnsiString") ||
        pascal_identifier_equals(id, "RawByteString") ||
        pascal_identifier_equals(id, "UnicodeString") ||
        pascal_identifier_equals(id, "WideString"))
        return STRING_TYPE;
    if (pascal_identifier_equals(id, "ShortString"))
        return SHORTSTRING_TYPE;
    if (pascal_identifier_equals(id, "Char") ||
        pascal_identifier_equals(id, "AnsiChar") ||
        pascal_identifier_equals(id, "WideChar") ||
        pascal_identifier_equals(id, "UnicodeChar"))
        return CHAR_TYPE;
    if (pascal_identifier_equals(id, "Boolean"))
        return BOOL;
    if (pascal_identifier_equals(id, "Pointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "CodePointer"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "TClass"))
        return POINTER_TYPE;
    if (pascal_identifier_equals(id, "file"))
        return FILE_TYPE;
    return UNKNOWN_TYPE;
}

static int semcheck_is_builtin_pointer_type_id(const char *id)
{
    if (id == NULL)
        return 0;
    id = semcheck_base_type_name(id);
    return (pascal_identifier_equals(id, "PInt64") ||
            pascal_identifier_equals(id, "PByte") ||
            pascal_identifier_equals(id, "PWord") ||
            pascal_identifier_equals(id, "PLongInt") ||
            pascal_identifier_equals(id, "PLongWord") ||
            pascal_identifier_equals(id, "PInteger") ||
            pascal_identifier_equals(id, "PCardinal") ||
            pascal_identifier_equals(id, "PQWord") ||
            pascal_identifier_equals(id, "PPointer") ||
            pascal_identifier_equals(id, "PBoolean"));
}

static int semcheck_scope_level_for_type_candidate(SymTab_t *symtab, HashNode_t *candidate)
{
    if (symtab == NULL || candidate == NULL || candidate->id == NULL)
        return INT_MAX / 2;

    int level = 0;
    for (ListNode_t *scope = symtab->stack_head; scope != NULL; scope = scope->next, ++level)
    {
        ListNode_t *matches = FindAllIdentsInTable((HashTable_t *)scope->cur, candidate->id);
        for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
        {
            if (cur->cur == candidate)
            {
                if (matches != NULL)
                    DestroyList(matches);
                return level;
            }
        }
        if (matches != NULL)
            DestroyList(matches);
    }
    return INT_MAX / 2;
}

static int semcheck_helper_self_is_var(SymTab_t *symtab, const char *base_type_id)
{
    if (base_type_id == NULL)
        return 0;
    /* Real/Single/Double/Extended: helper Self is passed by value. */
    if (pascal_identifier_equals(base_type_id, "Real") ||
        pascal_identifier_equals(base_type_id, "Float") ||
        pascal_identifier_equals(base_type_id, "ValReal") ||
        pascal_identifier_equals(base_type_id, "Single") ||
        pascal_identifier_equals(base_type_id, "Double") ||
        pascal_identifier_equals(base_type_id, "Extended") ||
        pascal_identifier_equals(base_type_id, "Currency") ||
        pascal_identifier_equals(base_type_id, "Comp"))
        return 0;
    /* String types are heap-allocated pointers — by value is correct. */
    if (pascal_identifier_equals(base_type_id, "String") ||
        pascal_identifier_equals(base_type_id, "AnsiString") ||
        pascal_identifier_equals(base_type_id, "ShortString") ||
        pascal_identifier_equals(base_type_id, "WideString") ||
        pascal_identifier_equals(base_type_id, "UnicodeString") ||
        pascal_identifier_equals(base_type_id, "Char") ||
        pascal_identifier_equals(base_type_id, "AnsiChar") ||
        pascal_identifier_equals(base_type_id, "WideChar"))
        return 0;
    /* Class and pointer types: Self is already a pointer. */
    HashNode_t *type_node = NULL;
    if (FindIdent(&type_node, symtab, base_type_id) == 0 && type_node != NULL)
    {
        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_RECORD)
        {
            struct RecordType *rec = type_node->type->info.record_info;
            if (rec != NULL && rec->is_class)
                return 0;
        }
        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_POINTER)
            return 0;
    }
    /* Integer/ordinal value types: Self must be passed by reference
     * so that mutations (Self := Self or ...) persist at the call site. */
    return 1;
}

static inline struct RecordType* get_record_type_from_node(HashNode_t *node);

static char *semcheck_dup_type_id_from_ast(ast_t *node)
{
    if (node == NULL)
        return NULL;
    if (node->sym != NULL && node->sym->name != NULL)
        return strdup(node->sym->name);
    for (ast_t *child = node->child; child != NULL; child = child->next)
    {
        char *found = semcheck_dup_type_id_from_ast(child);
        if (found != NULL)
            return found;
    }
    return NULL;
}

static int semcheck_is_char_alias_name(const char *id)
{
    return (id != NULL &&
        (pascal_identifier_equals(id, "Char") ||
            pascal_identifier_equals(id, "AnsiChar") ||
            pascal_identifier_equals(id, "WideChar") ||
            pascal_identifier_equals(id, "UnicodeChar")));
}

static int semcheck_alias_should_be_char_like(const char *alias_id, const char *target_id)
{
    return (semcheck_is_char_alias_name(alias_id) ||
        semcheck_is_char_alias_name(target_id));
}

static int semcheck_kgpc_type_is_char_like(const KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (kgpc_type_is_char((KgpcType *)type))
        return 1;
    if (type->type_alias != NULL && type->type_alias->is_char_alias)
        return 1;
    return 0;
}

static int semcheck_is_currency_type_id(const char *type_id)
{
    const char *base = semcheck_base_type_name(type_id);
    return (base != NULL && pascal_identifier_equals(base, "Currency"));
}

static int map_var_type_to_type_tag(enum VarType var_type)
{
    switch (var_type)
    {
        case HASHVAR_INTEGER:
            return INT_TYPE;
        case HASHVAR_LONGINT:
            return LONGINT_TYPE;
        case HASHVAR_INT64:
            return INT64_TYPE;
        case HASHVAR_QWORD:
            return QWORD_TYPE;
        case HASHVAR_REAL:
            return REAL_TYPE;
        case HASHVAR_BOOLEAN:
            return BOOL;
        case HASHVAR_CHAR:
            return CHAR_TYPE;
        case HASHVAR_PCHAR:
            return STRING_TYPE;
        default:
    return UNKNOWN_TYPE;
}

}

/* Inverse of map_var_type_to_type_tag: converts a type tag from
 * semcheck_map_builtin_type_name_local() to the corresponding VarType. */
static enum VarType map_type_tag_to_var_type(int type_tag)
{
    switch (type_tag)
    {
        case INT_TYPE:        return HASHVAR_INTEGER;
        case LONGINT_TYPE:    return HASHVAR_LONGINT;
        case INT64_TYPE:      return HASHVAR_INT64;
        case REAL_TYPE:
        case EXTENDED_TYPE:   return HASHVAR_REAL;
        case BOOL:            return HASHVAR_BOOLEAN;
        case CHAR_TYPE:       return HASHVAR_CHAR;
        case STRING_TYPE:     return HASHVAR_PCHAR;
        case SHORTSTRING_TYPE:return HASHVAR_SHORTSTRING;
        case POINTER_TYPE:    return HASHVAR_POINTER;
        case BYTE_TYPE:       return HASHVAR_INTEGER;
        case WORD_TYPE:       return HASHVAR_INTEGER;
        case LONGWORD_TYPE:   return HASHVAR_LONGINT;
        case QWORD_TYPE:      return HASHVAR_QWORD;
        case FILE_TYPE:       return HASHVAR_FILE;
        default:              return HASHVAR_UNTYPED;
    }
}

static int semcheck_find_ident_with_qualified_fallback(HashNode_t **out, SymTab_t *symtab,
    const char *id)
{
    if (out == NULL || symtab == NULL || id == NULL)
        return -1;

    return FindIdent(out, symtab, id);
}

static int semcheck_find_ident_with_qualified_fallback_ref(HashNode_t **out, SymTab_t *symtab,
    const QualifiedIdent *id_ref)
{
    if (out == NULL || symtab == NULL || id_ref == NULL || id_ref->count <= 0)
        return -1;

    int found = -1;
    char *full = qualified_ident_join(id_ref, ".");
    if (full != NULL)
    {
        found = FindIdent(out, symtab, full);
        free(full);
    }
    if (found >= 0 && out != NULL && *out != NULL)
        return found;

    if (id_ref->count > 1)
    {
        const char *last = qualified_ident_last(id_ref);
        if (last != NULL)
            return FindIdent(out, symtab, last);
    }

    return found;
}

static ListNode_t *semcheck_clone_string_list(const ListNode_t *src)
{
    if (src == NULL)
        return NULL;

    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    for (const ListNode_t *cur = src; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_STRING || cur->cur == NULL)
            continue;

        ListNode_t *node = (ListNode_t *)calloc(1, sizeof(ListNode_t));
        if (node == NULL)
        {
            destroy_list(head);
            return NULL;
        }
        node->type = LIST_STRING;
        node->cur = strdup((const char *)cur->cur);
        if (node->cur == NULL)
        {
            free(node);
            destroy_list(head);
            return NULL;
        }

        if (head == NULL)
            head = node;
        else
            tail->next = node;
        tail = node;
    }

    return head;
}

static int semcheck_alias_targets_match(const struct TypeAlias *lhs, const struct TypeAlias *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return 0;

    if (lhs->target_type_ref != NULL && rhs->target_type_ref != NULL &&
        lhs->target_type_ref->name != NULL && rhs->target_type_ref->name != NULL &&
        qualified_ident_equals_ci(lhs->target_type_ref->name, rhs->target_type_ref->name))
    {
        return 1;
    }

    if (lhs->target_type_id != NULL && rhs->target_type_id != NULL &&
        pascal_identifier_equals(lhs->target_type_id, rhs->target_type_id))
    {
        return 1;
    }

    return 0;
}

static HashNode_t *semcheck_find_type_excluding_alias(SymTab_t *symtab, const char *type_id,
    struct TypeAlias *exclude_alias)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdentsInNearestScope(symtab, type_id);
    if (matches == NULL)
    {
        const char *base = semcheck_base_type_name(type_id);
        if (base != NULL && base != type_id)
            matches = FindAllIdents(symtab, base);
    }
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE &&
            candidate->type != NULL)
        {
            struct TypeAlias *candidate_alias = kgpc_type_get_type_alias(candidate->type);
            if (exclude_alias == NULL || candidate_alias != exclude_alias)
            {
                DestroyList(matches);
                return candidate;
            }
        }
        cur = cur->next;
    }

    DestroyList(matches);
    return NULL;
}

static int semcheck_prefer_unit_defined_owner(void);

static HashNode_t *semcheck_find_owner_record_type_node(SymTab_t *symtab, const char *owner_id)
{
    if (symtab == NULL || owner_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, owner_id);
    if (matches == NULL)
    {
        const char *base = semcheck_base_type_name(owner_id);
        if (base != NULL && base != owner_id)
            matches = FindAllIdents(symtab, base);
    }

    int prefer_unit_defined = semcheck_prefer_unit_defined_owner();
    HashNode_t *best_record = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->hash_type == HASHTYPE_TYPE)
        {
            HashNode_t *record_node = NULL;
            if (get_record_type_from_node(candidate) != NULL)
            {
                record_node = candidate;
            }
            else if (candidate->type != NULL)
            {
                struct TypeAlias *alias = kgpc_type_get_type_alias(candidate->type);
                if (alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node = semcheck_find_preferred_type_node(symtab,
                        alias->target_type_id);
                    if (target_node != NULL && get_record_type_from_node(target_node) != NULL)
                        record_node = target_node;
                }
            }

            if (record_node != NULL)
            {
                if (best_record == NULL)
                {
                    best_record = record_node;
                }
                else
                {
                    /* Prefer class types over plain records for method owner resolution:
                     * e.g. TTimeZone = class abstract (dateutil.inc) over
                     * TTimeZone = timezone (ostypes.inc plain record alias). */
                    struct RecordType *best_rec = get_record_type_from_node(best_record);
                    struct RecordType *cand_rec = get_record_type_from_node(record_node);
                    int best_is_class = (best_rec != NULL && record_type_is_class(best_rec));
                    int cand_is_class = (cand_rec != NULL && record_type_is_class(cand_rec));
                    if (!best_is_class && cand_is_class)
                    {
                        best_record = record_node;
                    }
                    else if (best_is_class == cand_is_class)
                    {
                        if (prefer_unit_defined)
                        {
                            if (!best_record->defined_in_unit && record_node->defined_in_unit)
                                best_record = record_node;
                        }
                        else if (best_record->defined_in_unit && !record_node->defined_in_unit)
                        {
                            best_record = record_node;
                        }
                    }
                }
            }
        }
        cur = cur->next;
    }

    if (matches != NULL)
        DestroyList(matches);

    return best_record;
}

static int semcheck_kgpc_type_is_record_like(const KgpcType *type)
{
    if (type == NULL)
        return 0;
    if (type->kind == TYPE_KIND_RECORD)
        return 1;
    if (type->kind == TYPE_KIND_POINTER &&
        type->info.points_to != NULL &&
        type->info.points_to->kind == TYPE_KIND_RECORD)
    {
        return 1;
    }
    return 0;
}

static int semcheck_param_decl_equivalent(const Tree_t *lhs, const Tree_t *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return 0;
    if (lhs->type != rhs->type)
        return 0;

    if (lhs->type == TREE_VAR_DECL)
    {
        const char *lhs_id = lhs->tree_data.var_decl_data.type_id;
        const char *rhs_id = rhs->tree_data.var_decl_data.type_id;
        int lhs_tag = lhs->tree_data.var_decl_data.type;
        int rhs_tag = rhs->tree_data.var_decl_data.type;
        int lhs_var = lhs->tree_data.var_decl_data.is_var_param;
        int rhs_var = rhs->tree_data.var_decl_data.is_var_param;

        if (lhs_var != rhs_var)
            return 0;
        if (lhs_id != NULL || rhs_id != NULL)
            return (lhs_id != NULL && rhs_id != NULL && pascal_identifier_equals(lhs_id, rhs_id));
        return lhs_tag == rhs_tag;
    }

    if (lhs->type == TREE_ARR_DECL)
    {
        const char *lhs_id = lhs->tree_data.arr_decl_data.type_id;
        const char *rhs_id = rhs->tree_data.arr_decl_data.type_id;
        int lhs_tag = lhs->tree_data.arr_decl_data.type;
        int rhs_tag = rhs->tree_data.arr_decl_data.type;
        int lhs_start = lhs->tree_data.arr_decl_data.s_range;
        int rhs_start = rhs->tree_data.arr_decl_data.s_range;
        int lhs_end = lhs->tree_data.arr_decl_data.e_range;
        int rhs_end = rhs->tree_data.arr_decl_data.e_range;

        if ((lhs_id != NULL || rhs_id != NULL) &&
            !(lhs_id != NULL && rhs_id != NULL && pascal_identifier_equals(lhs_id, rhs_id)))
            return 0;
        if (lhs_tag != rhs_tag)
            return 0;
        return (lhs_start == rhs_start && lhs_end == rhs_end);
    }

    return 0;
}

static int semcheck_subprogram_signatures_equivalent(const Tree_t *lhs, const Tree_t *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return 0;
    if (lhs->type != TREE_SUBPROGRAM || rhs->type != TREE_SUBPROGRAM)
        return 0;

    ListNode_t *lhs_args = lhs->tree_data.subprogram_data.args_var;
    ListNode_t *rhs_args = rhs->tree_data.subprogram_data.args_var;

    while (lhs_args != NULL && rhs_args != NULL)
    {
        Tree_t *lhs_decl = (Tree_t *)lhs_args->cur;
        Tree_t *rhs_decl = (Tree_t *)rhs_args->cur;
        if (!semcheck_param_decl_equivalent(lhs_decl, rhs_decl))
            return 0;
        lhs_args = lhs_args->next;
        rhs_args = rhs_args->next;
    }

    return (lhs_args == NULL && rhs_args == NULL);
}

static ListNode_t *semcheck_create_builtin_param(const char *name, int type_tag)
{
    char *param_name = strdup(name);
    if (param_name == NULL)
        return NULL;

    ListNode_t *ids = CreateListNode(param_name, LIST_STRING);
    if (ids == NULL)
        return NULL;

    Tree_t *decl = mk_vardecl(0, ids, type_tag, NULL, 0, 0, NULL, NULL, NULL, NULL);
    if (decl == NULL)
        return NULL;

    return CreateListNode(decl, LIST_TREE);
}

static ListNode_t *semcheck_create_builtin_param_var(const char *name, int type_tag)
{
    char *param_name = strdup(name);
    if (param_name == NULL)
        return NULL;

    ListNode_t *ids = CreateListNode(param_name, LIST_STRING);
    if (ids == NULL)
        return NULL;

    Tree_t *decl = mk_vardecl(0, ids, type_tag, NULL, 1, 0, NULL, NULL, NULL, NULL);
    if (decl == NULL)
        return NULL;

    return CreateListNode(decl, LIST_TREE);
}

static ListNode_t *semcheck_create_builtin_param_with_id(const char *name, int type_tag,
    const char *type_id, int is_var_param)
{
    char *param_name = strdup(name);
    if (param_name == NULL)
        return NULL;

    ListNode_t *ids = CreateListNode(param_name, LIST_STRING);
    if (ids == NULL)
        return NULL;

    char *type_id_copy = NULL;
    if (type_id != NULL)
        type_id_copy = strdup(type_id);

    Tree_t *decl = mk_vardecl(0, ids, type_tag, type_id_copy,
        is_var_param, 0, NULL, NULL, NULL, NULL);
    if (decl == NULL)
    {
        if (type_id_copy != NULL)
            free(type_id_copy);
        return NULL;
    }

    return CreateListNode(decl, LIST_TREE);
}

/* Adds built-in functions */
void semcheck_add_builtins(SymTab_t *symtab);

/* Internal helper to print context using either offset or line-based search */
static void print_error_context(int line_num, int col_num, int source_index, const char *directive_file)
{
    const char *file_path = (directive_file != NULL && directive_file[0] != '\0')
                                ? directive_file
                                : ((file_to_parse != NULL && *file_to_parse != '\0')
                                    ? file_to_parse
                                    : ((preprocessed_path != NULL && *preprocessed_path != '\0') ? preprocessed_path
                                                                                                  : pascal_frontend_current_path()));
    if (file_path == NULL)
        file_path = g_semcheck_source_path;
    size_t context_len = preprocessed_length;
    if (context_len == 0 && preprocessed_source != NULL)
        context_len = strlen(preprocessed_source);

    if (line_num > 0) {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = context_len;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        if (kgpc_getenv("KGPC_DEBUG_SEM_CONTEXT") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] context file=%s pre_len=%zu buf_len=%zu line=%d col=%d offset=%d\n",
                file_path != NULL ? file_path : "<null>",
                context_len,
                context_buf_len,
                line_num,
                col_num,
                source_index);
        }
        /* When the source file is known from a {#line} directive, read context
         * from the original file — the preprocessed buffer has different line
         * numbers due to inlined includes and stripped ifdef blocks. */
        int printed = 0;
        if (directive_file[0] != '\0')
            printed = semcheck_print_context_from_file(directive_file, line_num, col_num, 2);
        if (!printed)
            printed = print_source_context_at_offset(context_buf, context_buf_len, source_index, line_num, col_num, 2);
        if (!printed)
            printed = semcheck_print_context_from_file(file_path, line_num, col_num, 2);
        if (!printed && file_path != NULL)
            print_source_context(file_path, line_num, col_num, 2);
    }

    fprintf(stderr, "\n");
}

#define MAX_DIRECTIVE_FILENAME_LEN 512

static int semcheck_parse_line_directive(const char *line, size_t len,
    char *filename_out, size_t filename_size)
{
    if (filename_out != NULL && filename_size > 0)
        filename_out[0] = '\0';

    if (len < 8)
        return -1;
    if (line[0] != '{' || line[1] != '#')
        return -1;
    if (len < 7 || strncasecmp(line + 2, "line", 4) != 0)
        return -1;

    size_t pos = 6;
    while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
        ++pos;

    int line_num = 0;
    while (pos < len && line[pos] >= '0' && line[pos] <= '9')
    {
        line_num = line_num * 10 + (line[pos] - '0');
        ++pos;
    }

    /* Parse optional filename in quotes */
    while (pos < len && (line[pos] == ' ' || line[pos] == '\t'))
        ++pos;

    if (pos < len && line[pos] == '"' && filename_out != NULL && filename_size > 0)
    {
        ++pos; /* Skip opening quote */
        size_t fname_start = pos;
        while (pos < len && line[pos] != '"')
            ++pos;
        size_t fname_len = pos - fname_start;
        if (fname_len > 0 && fname_len < filename_size)
        {
            memcpy(filename_out, line + fname_start, fname_len);
            filename_out[fname_len] = '\0';
        }
    }

    return line_num > 0 ? line_num : -1;
}

static int semcheck_line_from_source_offset(const char *buffer, size_t length, int source_offset,
    char *file_out, size_t file_out_size)
{
    if (file_out != NULL && file_out_size > 0)
        file_out[0] = '\0';

    if (buffer == NULL || length == 0 || source_offset < 0 || (size_t)source_offset >= length)
        return -1;

    int scan_pos = source_offset;
    while (scan_pos > 0)
    {
        int line_start = scan_pos;
        while (line_start > 0 && buffer[line_start - 1] != '\n')
            --line_start;

        size_t line_len = 0;
        int temp_pos = line_start;
        while ((size_t)temp_pos < length && buffer[temp_pos] != '\n')
        {
            ++temp_pos;
            ++line_len;
        }

        int directive_line = semcheck_parse_line_directive(buffer + line_start, line_len,
            file_out, file_out_size);
        if (directive_line >= 0)
        {
            int lines_after_directive = 0;
            int pos = line_start;
            while ((size_t)pos < length && buffer[pos] != '\n')
                ++pos;
            if ((size_t)pos < length && buffer[pos] == '\n')
                ++pos;
            while (pos < source_offset)
            {
                if (buffer[pos] == '\n')
                    ++lines_after_directive;
                ++pos;
            }
            return directive_line + lines_after_directive;
        }

        if (line_start > 0)
            scan_pos = line_start - 1;
        else
            break;
    }

    /* No #line directive found — count newlines from buffer start */
    int line = 1;
    for (int i = 0; i < source_offset; i++)
    {
        if (buffer[i] == '\n')
            ++line;
    }
    return line;
}

/* Find the file from a source byte offset by scanning backwards for line directives */
static void semcheck_file_from_source_index(int source_index, char *file_out, size_t file_out_size)
{
    if (file_out == NULL || file_out_size == 0)
        return;
    
    file_out[0] = '\0';
    
    if (source_index < 0)
        return;
    
    const char *buffer = preprocessed_source;
    size_t length = preprocessed_length;
    if (buffer == NULL || length == 0)
    {
        buffer = g_semcheck_source_buffer;
        length = g_semcheck_source_length;
    }
    if (buffer == NULL || length == 0 || (size_t)source_index >= length)
        return;
    
    /* Scan backwards from source_index to find the most recent #line directive */
    int scan_pos = source_index;
    while (scan_pos > 0) {
        /* Find start of current line */
        int line_start = scan_pos;
        while (line_start > 0 && buffer[line_start - 1] != '\n')
            --line_start;
        
        /* Check if this line is a #line directive */
        size_t line_len = 0;
        int temp_pos = line_start;
        while ((size_t)temp_pos < length && buffer[temp_pos] != '\n') {
            ++temp_pos;
            ++line_len;
        }
        
        char directive_file[512] = "";
        int directive_line = semcheck_parse_line_directive(buffer + line_start, line_len,
                                                            directive_file, sizeof(directive_file));
        if (directive_line >= 0) {
            if (directive_file[0] != '\0') {
                snprintf(file_out, file_out_size, "%s", directive_file);
            }
            return;
        }
        
        /* Move to previous line */
        if (line_start > 0)
            scan_pos = line_start - 1;
        else
            break;
    }
}

/* Find the file that contains a given line number by scanning line directives.
 * NOTE: This is a fallback for when source_index is not available.
 * Prefer using source_index with semcheck_file_from_source_index when possible. */
static void semcheck_file_from_buffer_line_number(int line_num, char *file_out, size_t file_out_size)
{
    if (file_out == NULL || file_out_size == 0 || line_num <= 0)
        return;
    
    file_out[0] = '\0';
    
    const char *buffer = preprocessed_source;
    size_t length = preprocessed_length;
    if (buffer == NULL || length == 0)
    {
        buffer = g_semcheck_source_buffer;
        length = g_semcheck_source_length;
    }
    if (buffer == NULL || length == 0)
        return;
    
    /* Scan through buffer tracking line directives */
    int current_line = 1;
    char last_file[512] = "";
    size_t pos = 0;
    
    while (pos < length && current_line <= line_num)
    {
        /* Find end of current line */
        size_t line_start = pos;
        while (pos < length && buffer[pos] != '\n')
            ++pos;
        size_t line_len = pos - line_start;
        
        /* Check if this is a line directive */
        char directive_file[512] = "";
        int directive_line = semcheck_parse_line_directive(buffer + line_start, line_len,
            directive_file, sizeof(directive_file));
        
        if (directive_line >= 0)
        {
            if (directive_file[0] != '\0')
            {
                strncpy(last_file, directive_file, sizeof(last_file) - 1);
                last_file[sizeof(last_file) - 1] = '\0';
            }
            current_line = directive_line;
        }
        
        /* Check if we've reached the target line */
        if (current_line == line_num && last_file[0] != '\0')
        {
            strncpy(file_out, last_file, file_out_size - 1);
            file_out[file_out_size - 1] = '\0';
            return;
        }
        
        /* Move to next line */
        if (pos < length && buffer[pos] == '\n')
        {
            ++pos;
            ++current_line;
        }
    }
    
    /* If we didn't find the exact line, return the last file we saw */
    if (last_file[0] != '\0')
    {
        strncpy(file_out, last_file, file_out_size - 1);
        file_out[file_out_size - 1] = '\0';
    }
}

static int g_semcheck_error_line = 0;
static int g_semcheck_error_col = 0;
static int g_semcheck_error_source_index = -1;
/* When set, source_index values are from a foreign preprocessed buffer
 * (unit-imported subprogram bodies) and must not be stored in the error
 * context, since resolve_error_source_context cannot disambiguate which
 * buffer the offset belongs to. */
static int g_semcheck_error_suppress_source_index = 0;
/* When inside a unit subprogram body, holds the unit name for error reporting
 * fallback (e.g., "<unit System>") so errors don't show the main program file. */
static const char *g_semcheck_error_unit_context = NULL;
static int resolve_const_identifier(SymTab_t *symtab, const char *id, long long *out_value);

void semcheck_set_error_context(int line_num, int col_num, int source_index)
{
    /* Don't overwrite a valid line number with 0 from synthetic AST nodes.
     * This preserves the last known real line for error reporting. */
    if (line_num > 0 || g_semcheck_error_line == 0)
        g_semcheck_error_line = line_num;
    if (col_num > 0 || g_semcheck_error_col == 0)
        g_semcheck_error_col = col_num;
    if (!g_semcheck_error_suppress_source_index &&
        (source_index >= 0 || g_semcheck_error_source_index < 0))
        g_semcheck_error_source_index = source_index;
}

void semcheck_clear_error_context(void)
{
    g_semcheck_error_line = 0;
    g_semcheck_error_col = 0;
    g_semcheck_error_source_index = -1;
}

static void skip_const_expr_ws(const char **p)
{
    while (p != NULL && *p != NULL && (**p == ' ' || **p == '\t'))
        (*p)++;
}

static int match_const_expr_keyword(const char **p, const char *keyword)
{
    size_t len = strlen(keyword);
    if (strncasecmp(*p, keyword, len) != 0)
        return 0;
    char next = (*p)[len];
    if ((next >= 'A' && next <= 'Z') || (next >= 'a' && next <= 'z') ||
        (next >= '0' && next <= '9') || next == '_')
        return 0;
    *p += len;
    return 1;
}

static int eval_const_expr_parse_additive(SymTab_t *symtab, const char **p, long long *out_value);

static int eval_const_expr_parse_primary(SymTab_t *symtab, const char **p, long long *out_value)
{
    if (p == NULL || *p == NULL || out_value == NULL)
        return -1;

    skip_const_expr_ws(p);

    if (**p == '(')
    {
        (*p)++;
        if (eval_const_expr_parse_additive(symtab, p, out_value) != 0)
            return -1;
        skip_const_expr_ws(p);
        if (**p != ')')
            return -1;
        (*p)++;
        return 0;
    }

    if (match_const_expr_keyword(p, "sizeof"))
    {
        skip_const_expr_ws(p);
        if (**p != '(')
            return -1;
        (*p)++;
        skip_const_expr_ws(p);

        const char *start = *p;
        while ((**p >= 'A' && **p <= 'Z') || (**p >= 'a' && **p <= 'z') ||
               (**p >= '0' && **p <= '9') || **p == '_' || **p == '.')
            (*p)++;
        if (*p == start)
            return -1;
        size_t len = (size_t)(*p - start);
        char *type_id = (char *)malloc(len + 1);
        if (type_id == NULL)
            return -1;
        memcpy(type_id, start, len);
        type_id[len] = '\0';

        skip_const_expr_ws(p);
        if (**p != ')')
        {
            free(type_id);
            return -1;
        }
        (*p)++;

        long long size_val = 0;
        int ok = (sizeof_from_type_ref(symtab, UNKNOWN_TYPE, type_id, &size_val, 0, 0) == 0);
        free(type_id);
        if (!ok)
            return -1;
        *out_value = size_val;
        return 0;
    }

    if ((**p >= '0' && **p <= '9') ||
        ((**p == '+' || **p == '-') && ((*p)[1] >= '0' && (*p)[1] <= '9')))
    {
        char *endptr = NULL;
        long long value = strtoll(*p, &endptr, 10);
        if (endptr == *p)
            return -1;
        *p = endptr;
        *out_value = value;
        return 0;
    }

    if ((**p >= 'A' && **p <= 'Z') || (**p >= 'a' && **p <= 'z') || **p == '_')
    {
        const char *start = *p;
        while ((**p >= 'A' && **p <= 'Z') || (**p >= 'a' && **p <= 'z') ||
               (**p >= '0' && **p <= '9') || **p == '_' || **p == '.')
            (*p)++;
        size_t len = (size_t)(*p - start);
        char *id = (char *)malloc(len + 1);
        if (id == NULL)
            return -1;
        memcpy(id, start, len);
        id[len] = '\0';
        int ok = (resolve_const_identifier(symtab, id, out_value) == 0);
        free(id);
        return ok ? 0 : -1;
    }

    return -1;
}

static int eval_const_expr_parse_unary(SymTab_t *symtab, const char **p, long long *out_value)
{
    if (p == NULL || *p == NULL || out_value == NULL)
        return -1;

    skip_const_expr_ws(p);
    if (**p == '+')
    {
        (*p)++;
        return eval_const_expr_parse_unary(symtab, p, out_value);
    }
    if (**p == '-')
    {
        long long inner = 0;
        (*p)++;
        if (eval_const_expr_parse_unary(symtab, p, &inner) != 0)
            return -1;
        *out_value = -inner;
        return 0;
    }
    return eval_const_expr_parse_primary(symtab, p, out_value);
}

static int eval_const_expr_parse_multiplicative(SymTab_t *symtab, const char **p, long long *out_value)
{
    if (eval_const_expr_parse_unary(symtab, p, out_value) != 0)
        return -1;

    while (1)
    {
        long long rhs = 0;
        skip_const_expr_ws(p);
        if (**p == '*')
        {
            (*p)++;
            if (eval_const_expr_parse_unary(symtab, p, &rhs) != 0)
                return -1;
            *out_value *= rhs;
            continue;
        }
        if (match_const_expr_keyword(p, "div"))
        {
            if (eval_const_expr_parse_unary(symtab, p, &rhs) != 0 || rhs == 0)
                return -1;
            *out_value /= rhs;
            continue;
        }
        if (match_const_expr_keyword(p, "mod"))
        {
            if (eval_const_expr_parse_unary(symtab, p, &rhs) != 0 || rhs == 0)
                return -1;
            *out_value %= rhs;
            continue;
        }
        if (match_const_expr_keyword(p, "shl"))
        {
            if (eval_const_expr_parse_unary(symtab, p, &rhs) != 0 || rhs < 0)
                return -1;
            *out_value <<= rhs;
            continue;
        }
        if (match_const_expr_keyword(p, "shr"))
        {
            if (eval_const_expr_parse_unary(symtab, p, &rhs) != 0 || rhs < 0)
                return -1;
            *out_value >>= rhs;
            continue;
        }
        break;
    }

    return 0;
}

static int eval_const_expr_parse_additive(SymTab_t *symtab, const char **p, long long *out_value)
{
    if (eval_const_expr_parse_multiplicative(symtab, p, out_value) != 0)
        return -1;

    while (1)
    {
        long long rhs = 0;
        skip_const_expr_ws(p);
        if (**p == '+')
        {
            (*p)++;
            if (eval_const_expr_parse_multiplicative(symtab, p, &rhs) != 0)
                return -1;
            *out_value += rhs;
            continue;
        }
        if (**p == '-')
        {
            (*p)++;
            if (eval_const_expr_parse_multiplicative(symtab, p, &rhs) != 0)
                return -1;
            *out_value -= rhs;
            continue;
        }
        break;
    }

    return 0;
}

static int resolve_array_bound_expr(SymTab_t *symtab, const char *expr, int *out_value)
{
    if (expr == NULL || out_value == NULL)
        return -1;

    const char *p = expr;
    long long result = 0;
    if (eval_const_expr_parse_additive(symtab, &p, &result) != 0)
        return -1;

    skip_const_expr_ws(&p);
    if (*p != '\0')
        return -1;

    if (result < INT_MIN || result > INT_MAX)
        return -1;
    *out_value = (int)result;
    return 0;
}

int semcheck_tag_from_kgpc(const KgpcType *type)
{
    if (type == NULL)
        return UNKNOWN_TYPE;
    /* Check actual kind FIRST for record/pointer types.
     * type_alias overrides are only valid for primitive types; for records
     * and pointers the type_alias may carry a stale base_type (e.g. STRING_TYPE
     * on a class variable's pointer-to-record KgpcType). */
    if (kgpc_type_is_record((KgpcType *)type))
    {
        struct RecordType *rec = kgpc_type_get_record((KgpcType *)type);
        if (rec != NULL && rec->is_interface)
            return POINTER_TYPE;
        return RECORD_TYPE;
    }
    if (kgpc_type_is_pointer((KgpcType *)type))
        return POINTER_TYPE;
    if (kgpc_type_is_procedure((KgpcType *)type))
        return PROCEDURE;
    /* Check type_alias overrides before primitive tag — a PByte (^Byte) alias
     * may have kind=TYPE_KIND_PRIMITIVE with tag=BYTE_TYPE in unit contexts,
     * but the type_alias correctly records is_pointer=1. */
    if (type->type_alias != NULL)
    {
        int base = type->type_alias->base_type;
        if (base == STRING_TYPE || base == SHORTSTRING_TYPE)
            return base;
        if (type->type_alias->is_pointer)
            return POINTER_TYPE;
        if (type->type_alias->is_set)
            return SET_TYPE;
        if (type->type_alias->is_enum)
            return ENUM_TYPE;
        if (type->type_alias->is_file)
            return FILE_TYPE;
    }
    if (type->kind == TYPE_KIND_PRIMITIVE)
        return type->info.primitive_type_tag;
    if (kgpc_type_is_array_of_const((KgpcType *)type))
        return ARRAY_OF_CONST_TYPE;
    /* Check for ShortString: array[0..255] of Char or explicit is_shortstring flag */
    if (kgpc_type_is_array((KgpcType *)type)) {
        /* Check explicit shortstring flag */
        if (type->type_alias != NULL && type->type_alias->is_shortstring)
            return SHORTSTRING_TYPE;
        /* Check for ShortString-like array: bounds [0..255] with char element type */
        if (type->info.array_info.start_index == 0 &&
            type->info.array_info.end_index == 255) {
            KgpcType *elem = type->info.array_info.element_type;
            if (elem == NULL) {
                /* array[0..255] with NULL element_type is treated as ShortString */
                return SHORTSTRING_TYPE;
            }
            if (elem->kind == TYPE_KIND_PRIMITIVE &&
                elem->info.primitive_type_tag == CHAR_TYPE) {
                return SHORTSTRING_TYPE;
            }
        }
    }
    return UNKNOWN_TYPE;
}

static const char *semcheck_get_error_path(void)
{
    /* When inside a unit subprogram body, use the unit context path
     * so errors report the unit name instead of the main program file. */
    if (g_semcheck_error_unit_context != NULL)
        return g_semcheck_error_unit_context;
    const char *file_path = (file_to_parse != NULL && *file_to_parse != '\0')
                                ? file_to_parse
                                : ((preprocessed_path != NULL && *preprocessed_path != '\0') ? preprocessed_path
                                                                                              : pascal_frontend_current_path());
    if (file_path == NULL)
        file_path = g_semcheck_source_path;
    if (file_path == NULL)
        file_path = "<unknown>";
    return file_path;
}

static void semcheck_print_error_prefix(const char *file_path, int line_num, int col_num)
{
    fprintf(stderr, "%s:%d", file_path, line_num);
    if (col_num > 0) {
        fprintf(stderr, ":%d", col_num);
    }
    fprintf(stderr, ": ");
}

static int semcheck_debug_errors_enabled(void)
{
    static int cached = -1;
    if (cached == -1)
        cached = (kgpc_getenv("KGPC_DEBUG_ERRORS") != NULL);
    return cached;
}

static void semcheck_debug_error_step(const char *step, Tree_t *subprogram, int before, int after)
{
    if (!semcheck_debug_errors_enabled() || after <= before)
        return;

    const char *sub_id = NULL;
    int sub_line = 0;
    if (subprogram != NULL && subprogram->type == TREE_SUBPROGRAM)
    {
        sub_id = subprogram->tree_data.subprogram_data.id;
        sub_line = subprogram->line_num;
    }

    fprintf(stderr,
        "[KGPC_DEBUG_ERRORS] sub=%s sub_line=%d step=%s delta=%d err_line=%d err_col=%d err_src=%d\n",
        sub_id != NULL ? sub_id : "<null>",
        sub_line,
        step != NULL ? step : "<null>",
        after - before,
        g_semcheck_error_line,
        g_semcheck_error_col,
        g_semcheck_error_source_index);
}

/* Resolve source context from a byte offset: search the source buffer registry
 * and fall back to the preprocessed/raw source buffer.  Returns the resolved
 * line number (>0 on success, 0 when nothing could be resolved).  On success
 * the directive file name is written to *directive_file_out.  The caller must
 * provide a buffer of at least MAX_DIRECTIVE_FILENAME_LEN bytes. */
static int resolve_error_source_context(int source_index,
    char *directive_file_out, size_t dir_size, int search_registry)
{
    assert(directive_file_out != NULL);
    directive_file_out[0] = '\0';

    if (source_index < 0)
        return 0;

    if (search_registry)
    {
        for (int i = 0; i < g_source_buffer_count; i++)
        {
            int gs = g_source_buffer_registry[i].global_start;
            int len = (int)g_source_buffer_registry[i].length;
            if (source_index >= gs && source_index < gs + len)
            {
                int local_offset = source_index - gs;
                char temp_file[MAX_DIRECTIVE_FILENAME_LEN];
                temp_file[0] = '\0';
                int computed_line = semcheck_line_from_source_offset(
                    g_source_buffer_registry[i].buffer,
                    g_source_buffer_registry[i].length,
                    local_offset,
                    temp_file, sizeof(temp_file));
                if (computed_line > 0)
                {
                    if (temp_file[0] != '\0')
                    {
                        strncpy(directive_file_out, temp_file, dir_size - 1);
                        directive_file_out[dir_size - 1] = '\0';
                    }
                    else
                    {
                        /* No #line directive in this buffer — use the
                         * registered path as the file name. */
                        strncpy(directive_file_out,
                            g_source_buffer_registry[i].path, dir_size - 1);
                        directive_file_out[dir_size - 1] = '\0';
                    }
                    return computed_line;
                }
            }
        }
    }

    /* Fall back to main preprocessed/source buffer */
    const char *context_buf = preprocessed_source;
    size_t context_buf_len = preprocessed_length;
    if (context_buf == NULL || context_buf_len == 0)
    {
        context_buf = g_semcheck_source_buffer;
        context_buf_len = g_semcheck_source_length;
    }
    int computed_line = semcheck_line_from_source_offset(context_buf, context_buf_len, source_index,
        directive_file_out, dir_size);
    return (computed_line > 0) ? computed_line : 0;
}

/* Helper function to print semantic error with source code context */
void semantic_error(int line_num, int col_num, const char *format, ...)
{
    int effective_line = line_num;
    int effective_col = col_num;
    int effective_source_index = g_semcheck_error_source_index;

    /* Fall back to the last known good line/col from error context when the
     * AST node has line_num == 0 (synthetic nodes from WITH expansion etc.) */
    if (effective_line <= 0 && g_semcheck_error_line > 0)
        effective_line = g_semcheck_error_line;
    if (effective_col <= 0 && g_semcheck_error_col > 0)
        effective_col = g_semcheck_error_col;

    char directive_file[MAX_DIRECTIVE_FILENAME_LEN];
    directive_file[0] = '\0';
    if (g_semcheck_error_unit_context != NULL)
    {
        /* Inside unit body: skip line resolution, just show unit name */
    }
    else if (effective_source_index >= 0)
    {
        int resolved_line = resolve_error_source_context(
            effective_source_index, directive_file, sizeof(directive_file), 1);
        if (resolved_line > 0)
            effective_line = resolved_line;
        if (directive_file[0] != '\0')
        {
            fprintf(stderr, "  In %s:\n", directive_file);
        }
        if (effective_col <= 0 && g_semcheck_error_col > 0)
            effective_col = g_semcheck_error_col;
    }
    else if (effective_line > 0)
    {
        /* Look up file from line number when source_index is not available */
        semcheck_file_from_buffer_line_number(effective_line, directive_file, sizeof(directive_file));
    }

    const char *file_path = (directive_file[0] != '\0')
        ? directive_file
        : semcheck_get_error_path();
    semcheck_print_error_prefix(file_path, effective_line, effective_col);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");

    print_error_context(effective_line, effective_col, effective_source_index, directive_file);
}

/* Helper function to print semantic error with accurate source context using byte offset */
void semantic_error_at(int line_num, int col_num, int source_index, const char *format, ...)
{
    /* Fall back to last known good line/col for synthetic nodes with line 0 */
    if (line_num <= 0 && g_semcheck_error_line > 0)
        line_num = g_semcheck_error_line;
    if (col_num <= 0 && g_semcheck_error_col > 0)
        col_num = g_semcheck_error_col;

    char directive_file[MAX_DIRECTIVE_FILENAME_LEN];
    directive_file[0] = '\0';
    if (g_semcheck_error_unit_context != NULL)
    {
        /* Inside unit body: skip line resolution, just show unit name */
    }
    else if (source_index >= 0)
    {
        int resolved_line = resolve_error_source_context(
            source_index, directive_file, sizeof(directive_file), 1);
        if (resolved_line > 0)
            line_num = resolved_line;
        if (directive_file[0] != '\0')
        {
            fprintf(stderr, "  In %s:\n", directive_file);
        }
    }
    else if (line_num > 0)
    {
        /* Look up file from line number when source_index is not available */
        semcheck_file_from_buffer_line_number(line_num, directive_file, sizeof(directive_file));
    }

    const char *file_path = (directive_file[0] != '\0')
        ? directive_file
        : semcheck_get_error_path();
    semcheck_print_error_prefix(file_path, line_num, col_num);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");

    print_error_context(line_num, col_num, source_index, directive_file);
}

/* Core formatting helper for the _with_context error functions.
 * Prints the error prefix, rewrites the legacy "Error on line %d" format, and
 * emits a trailing newline.  The caller must have already resolved the
 * effective line/col/source_index and directive_file.
 *
 * After return, the va_list is in an indeterminate state. */
static void v_semcheck_format_error_with_context(
    const char *file_path, int effective_line, int effective_col,
    int source_index, const char *format, va_list args)
{
    semcheck_print_error_prefix(file_path, effective_line, effective_col);
    if (format != NULL && strncmp(format, "Error on line %d", 16) == 0)
    {
        int original_line = va_arg(args, int);
        (void)original_line;
        const char *rest = format + 16;
        if (rest[0] == ',' && rest[1] == ' ')
            rest += 2;
        else if (rest[0] == ',')
            rest += 1;
        else if (rest[0] == ':' && rest[1] == ' ')
            rest += 2;
        vfprintf(stderr, rest, args);
    }
    else
    {
        vfprintf(stderr, format, args);
    }

    size_t len = format ? strlen(format) : 0;
    if (len == 0 || format[len - 1] != '\n')
        fprintf(stderr, "\n");

    /* Inline source context */
    if (effective_line > 0)
    {
        size_t context_len = preprocessed_length;
        if (context_len == 0 && preprocessed_source != NULL)
            context_len = strlen(preprocessed_source);
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = context_len;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        if (kgpc_getenv("KGPC_DEBUG_SEM_CONTEXT") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] context file=%s pre_len=%zu buf_len=%zu line=%d col=%d offset=%d\n",
                file_path != NULL ? file_path : "<null>",
                context_len,
                context_buf_len,
                effective_line,
                effective_col,
                source_index);
        }
        int printed = print_source_context_at_offset(context_buf, context_buf_len,
            source_index, effective_line, effective_col, 2);
        if (!printed)
            printed = semcheck_print_context_from_file(file_path, effective_line, effective_col, 2);
        if (!printed && file_path != NULL)
            print_source_context(file_path, effective_line, effective_col, 2);
    }

    fprintf(stderr, "\n");
}

void semcheck_error_with_context_at(int line_num, int col_num, int source_index,
    const char *format, ...)
{
    int effective_line = line_num;
    int effective_col = col_num;

    /* Fall back to last known good line/col for synthetic nodes with line 0 */
    if (effective_line <= 0 && g_semcheck_error_line > 0)
        effective_line = g_semcheck_error_line;
    if (effective_col <= 0 && g_semcheck_error_col > 0)
        effective_col = g_semcheck_error_col;

    char directive_file[MAX_DIRECTIVE_FILENAME_LEN];
    directive_file[0] = '\0';
    if (g_semcheck_error_unit_context != NULL)
    {
        /* Inside unit body: skip line resolution, just show unit name */
    }
    else if (source_index >= 0)
    {
        int resolved_line = resolve_error_source_context(
            source_index, directive_file, sizeof(directive_file), 1);
        if (resolved_line > 0)
            effective_line = resolved_line;
        if (directive_file[0] != '\0')
            fprintf(stderr, "  In %s:\n", directive_file);
        if (effective_col <= 0 && g_semcheck_error_col > 0)
            effective_col = g_semcheck_error_col;
    }

    const char *file_path = (directive_file[0] != '\0')
        ? directive_file
        : semcheck_get_error_path();

    va_list args;
    va_start(args, format);
    v_semcheck_format_error_with_context(file_path, effective_line, effective_col,
        source_index, format, args);
    va_end(args);
}

/* Helper for legacy error prints that already include "Error on line %d". */
void semcheck_error_with_context(const char *format, ...)
{
    int line_num = 0;
    int effective_line = 0;
    int effective_col = 0;
    int effective_source_index = g_semcheck_error_source_index;

    va_list args;
    va_start(args, format);

    va_list args_copy;
    va_copy(args_copy, args);
    line_num = va_arg(args_copy, int);
    va_end(args_copy);

    char directive_file[MAX_DIRECTIVE_FILENAME_LEN];
    directive_file[0] = '\0';
    if (g_semcheck_error_unit_context != NULL)
    {
        /* Inside unit body: skip line resolution, just show unit name */
    }
    else if (effective_source_index >= 0)
    {
        int resolved_line = resolve_error_source_context(
            effective_source_index, directive_file, sizeof(directive_file), 1);
        if (resolved_line > 0)
            effective_line = resolved_line;
        if (directive_file[0] != '\0')
            fprintf(stderr, "  In %s:\n", directive_file);
        if (g_semcheck_error_col > 0)
            effective_col = g_semcheck_error_col;
    }
    if (effective_line <= 0)
        effective_line = line_num;
    /* Fall back to the last known good line from error context when the
     * AST node has line_num == 0 (synthetic nodes from WITH expansion etc.) */
    if (effective_line <= 0 && g_semcheck_error_line > 0)
        effective_line = g_semcheck_error_line;
    if (effective_col <= 0 && g_semcheck_error_col > 0)
        effective_col = g_semcheck_error_col;

    const char *file_path = (directive_file[0] != '\0')
        ? directive_file
        : semcheck_get_error_path();

    v_semcheck_format_error_with_context(file_path, effective_line, effective_col,
        effective_source_index, format, args);
    va_end(args);
}

/* Main is a special keyword at the moment for code generation */
int semcheck_id_not_main(char *id)
{
    if(pascal_identifier_equals(id, "main"))
    {
        fprintf(stderr, "ERROR: main is special keyword, it cannot be a program, or subprogram\n");
        return 1;
    }
    return 0;
}

static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias)
{
    if (symtab == NULL || id == NULL || alias == NULL)
        return;

    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, id) != -1 && node != NULL)
    {
        if (node->mangled_id != NULL)
            free(node->mangled_id);
        node->mangled_id = strdup(alias);
    }
}

/* Helper function to get TypeAlias from HashNode, preferring KgpcType when available */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    if (node == NULL)
        return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    return hashnode_get_type_alias(node);
}

static void apply_builtin_integer_alias_metadata(struct TypeAlias *alias, const char *type_name)
{
    if (alias == NULL || type_name == NULL)
        return;

    if (pascal_identifier_equals(type_name, "Byte"))
    {
        alias->base_type = BYTE_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = 255;
        alias->storage_size = 1;
    }
    else if (pascal_identifier_equals(type_name, "ShortInt"))
    {
        alias->base_type = INT_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = -128;
        alias->range_end = 127;
        alias->storage_size = 1;
    }
    else if (pascal_identifier_equals(type_name, "Word"))
    {
        alias->base_type = WORD_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = 65535;
        alias->storage_size = 2;
    }
    else if (pascal_identifier_equals(type_name, "SmallInt"))
    {
        alias->base_type = INT_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = -32768;
        alias->range_end = 32767;
        alias->storage_size = 2;
    }
    else if (pascal_identifier_equals(type_name, "Cardinal") ||
             pascal_identifier_equals(type_name, "LongWord") ||
             pascal_identifier_equals(type_name, "DWord"))
    {
        alias->base_type = LONGWORD_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = 4294967295LL;
        alias->storage_size = 4;
    }
    else if (pascal_identifier_equals(type_name, "QWord") ||
             pascal_identifier_equals(type_name, "UInt64"))
    {
        alias->base_type = QWORD_TYPE;
        alias->is_range = 1;
        alias->range_known = 1;
        alias->range_start = 0;
        alias->range_end = LLONG_MAX;
        alias->storage_size = 8;
    }
}

static void inherit_alias_metadata(SymTab_t *symtab, struct TypeAlias *alias)
{
    if (symtab == NULL || alias == NULL)
        return;

    HashNode_t *target_node = NULL;
    if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
        alias->target_type_ref->name->count > 1)
    {
        target_node = semcheck_find_exact_type_node_for_qid(symtab,
            alias->target_type_ref->name);
    }
    if (target_node == NULL &&
        (alias->target_type_ref != NULL || alias->target_type_id != NULL))
    {
        target_node = semcheck_find_preferred_type_node_with_ref(symtab,
            alias->target_type_ref, alias->target_type_id);
    }
    if (target_node == NULL)
        return;

    struct TypeAlias *target_alias = get_type_alias_from_node(target_node);
    if (target_alias == NULL)
        return;

    if (alias->storage_size <= 0 && target_alias->storage_size > 0)
        alias->storage_size = target_alias->storage_size;

    if (!alias->is_range && target_alias->is_range && target_alias->range_known)
    {
        alias->is_range = 1;
        alias->range_known = target_alias->range_known;
        alias->range_start = target_alias->range_start;
        alias->range_end = target_alias->range_end;
    }

    if (alias->base_type == UNKNOWN_TYPE && target_alias->base_type != UNKNOWN_TYPE)
        alias->base_type = target_alias->base_type;

    if (!alias->is_enum && target_alias->is_enum)
        alias->is_enum = 1;
    if (target_alias->enum_is_scoped)
        alias->enum_is_scoped = 1;
    if (target_alias->enum_has_explicit_values)
        alias->enum_has_explicit_values = 1;
    if (alias->enum_literals == NULL && target_alias->enum_literals != NULL)
        alias->enum_literals = semcheck_clone_string_list(target_alias->enum_literals);
    if (alias->kgpc_type == NULL && target_alias->kgpc_type != NULL)
    {
        alias->kgpc_type = target_alias->kgpc_type;
        kgpc_type_retain(alias->kgpc_type);
    }
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    if (node == NULL) return NULL;
    
    /* Use hashnode helper which handles NULL KgpcType */
    struct RecordType *record = hashnode_get_record_type(node);
    if (record != NULL)
        return record;
        
    /* If not a direct record, check if it's a pointer to a record (Class types are pointers) */
    if (node->type != NULL && kgpc_type_is_pointer(node->type))
    {
        KgpcType *pointed_to = node->type->info.points_to;
        if (pointed_to != NULL && kgpc_type_is_record(pointed_to))
        {
            return kgpc_type_get_record(pointed_to);
        }
    }
    
    return NULL;
}

/**
 * Copy default parameter values from forward declaration to implementation.
 * When a method is declared in a class with default values but implemented
 * without them, the implementation's params need the defaults for overload resolution.
 */
static void copy_default_values_to_impl_params(ListNode_t *fwd_params, ListNode_t *impl_params)
{
    if (fwd_params == NULL || impl_params == NULL)
        return;
    
    ListNode_t *fwd = fwd_params;
    ListNode_t *impl = impl_params;
    
    while (fwd != NULL && impl != NULL)
    {
        Tree_t *fwd_decl = (Tree_t *)fwd->cur;
        Tree_t *impl_decl = (Tree_t *)impl->cur;
        
        if (fwd_decl != NULL && impl_decl != NULL)
        {
            /* Copy default value from forward declaration if impl doesn't have one */
            if (fwd_decl->type == TREE_VAR_DECL && impl_decl->type == TREE_VAR_DECL)
            {
                struct Statement *fwd_init = fwd_decl->tree_data.var_decl_data.initializer;
                if (fwd_init != NULL &&
                    impl_decl->tree_data.var_decl_data.initializer == NULL)
                {
                    /* The initializer is a STMT_VAR_ASSIGN with NULL var, containing the expression */
                    if (fwd_init->type == STMT_VAR_ASSIGN && 
                        fwd_init->stmt_data.var_assign_data.expr != NULL)
                    {
                        struct Expression *cloned_expr = clone_expression(
                            fwd_init->stmt_data.var_assign_data.expr);
                        if (cloned_expr != NULL)
                        {
                            impl_decl->tree_data.var_decl_data.initializer =
                                mk_varassign(fwd_init->line_num, fwd_init->col_num, NULL, cloned_expr);
                        }
                    }
                }
            }
            else if (fwd_decl->type == TREE_ARR_DECL && impl_decl->type == TREE_ARR_DECL)
            {
                struct Statement *fwd_init = fwd_decl->tree_data.arr_decl_data.initializer;
                if (fwd_init != NULL &&
                    impl_decl->tree_data.arr_decl_data.initializer == NULL)
                {
                    if (fwd_init->type == STMT_VAR_ASSIGN && 
                        fwd_init->stmt_data.var_assign_data.expr != NULL)
                    {
                        struct Expression *cloned_expr = clone_expression(
                            fwd_init->stmt_data.var_assign_data.expr);
                        if (cloned_expr != NULL)
                        {
                            impl_decl->tree_data.arr_decl_data.initializer =
                                mk_varassign(fwd_init->line_num, fwd_init->col_num, NULL, cloned_expr);
                        }
                    }
                }
            }
        }
        
        fwd = fwd->next;
        impl = impl->next;
    }
}

/**
 * For a method implementation (ClassName__MethodName), add class vars to scope
 * so they can be referenced within the method body. This is essential for
 * static methods which have no implicit Self parameter.
 */
static void add_class_vars_to_method_scope_impl(SymTab_t *symtab,
    const char *owner, const char *method_name, int is_operator)
{
    if (symtab == NULL || owner == NULL || method_name == NULL || method_name[0] == '\0')
        return;

    if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
        fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] owner=%s method=%s\n", owner, method_name);

    char *class_name = strdup(owner);
    if (class_name == NULL)
        return;

    if (from_cparser_is_type_helper(class_name))
    {
        free(class_name);
        return;
    }

    /* Operator methods (class operator ...), especially on records, should
     * not have owner fields injected into local scope. Their parameters often
     * intentionally reuse field names (e.g. "a"), and injected fields would
     * shadow those parameters. */
    if (is_operator)
    {
        free(class_name);
        return;
    }

    /* Only add class fields for static methods. Non-static methods
     * access fields via Self which is handled differently. */
    int is_static = from_cparser_is_method_static(class_name, method_name);

    /* Look up the class type */
    HashNode_t *class_node = NULL;
    int lookup_result = FindIdent(&class_node, symtab, class_name);
    if (lookup_result == -1 || class_node == NULL)
    {
        free(class_name);
        return;
    }

    struct RecordType *record_info = get_record_type_from_node(class_node);
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }
    if (record_info->generic_decl != NULL || record_info->num_generic_args > 0)
    {
        free(class_name);
        return;
    }

    /* Skip type helpers entirely; they do not expose class vars like real classes. */
    if (record_info->is_type_helper)
    {
        free(class_name);
        return;
    }

    int has_class_vars = 0;
    if (record_info->fields != NULL)
    {
        for (ListNode_t *scan = record_info->fields; scan != NULL; scan = scan->next)
        {
            if (scan->type == LIST_RECORD_FIELD && scan->cur != NULL)
            {
                struct RecordField *field = (struct RecordField *)scan->cur;
                if (field->is_class_var == 1)
                {
                    has_class_vars = 1;
                    break;
                }
            }
        }
    }

    /* For class types (not objects), non-static methods access fields via Self.
     * For object types, class vars and consts are NOT accessible via Self,
     * so we must always push them into scope. */
    int is_object_type = !record_type_is_class(record_info) && !record_info->is_type_helper;
    if (!is_static && !is_object_type)
    {
        free(class_name);
        return;
    }
    if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
    {
        fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] class=%s static=%d has_class_vars=%d\n",
            class_name ? class_name : "<null>", is_static, has_class_vars);
    }

    /* Process class types, object types, and advanced records that have fields. */
    ListNode_t *field_node = record_info->fields;
    while (field_node != NULL)
    {
        if (field_node->type != LIST_RECORD_FIELD)
        {
            field_node = field_node->next;
            continue;
        }
        struct RecordField *field = (struct RecordField *)field_node->cur;
        if (field != NULL && field->name != NULL && field->name[0] != '\0')
        {
            int allow_all_fields = (record_type_is_class(record_info) && is_static && !has_class_vars);
            if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL)
            {
                fprintf(stderr, "[KGPC_DEBUG_CLASS_VAR] field=%s class_var=%d allow_all=%d\n",
                    field->name ? field->name : "<null>",
                    field->is_class_var, allow_all_fields);
            }
            if (record_type_is_class(record_info) && !allow_all_fields && field->is_class_var != 1)
            {
                field_node = field_node->next;
                continue;
            }
            if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                pascal_identifier_equals(field->name, "FStandardEncodings"))
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_CLASS_VAR] class=%s field=%s is_array=%d elem_type=%d elem_type_id=%s\n",
                    class_name ? class_name : "<null>",
                    field->name,
                    field->is_array,
                    field->array_element_type,
                    field->array_element_type_id ? field->array_element_type_id : "<null>");
            }
            /* Check if already defined in scope */
            HashNode_t *existing = NULL;
            if (FindIdent(&existing, symtab, field->name) == -1)
            {
                /* Build a KgpcType for this field if needed */
                KgpcType *field_type = NULL;
                if (field->type_id != NULL && field->type_id[0] != '\0')
                {
                    HashNode_t *type_node = NULL;
                    if (FindIdent(&type_node, symtab, field->type_id) != -1 &&
                        type_node != NULL && type_node->type != NULL)
                    {
                        field_type = type_node->type;
                        kgpc_type_retain(field_type);
                    }
                    else if (class_name != NULL)
                    {
                        char qualified_name[512];
                        snprintf(qualified_name, sizeof(qualified_name), "%s.%s", class_name, field->type_id);
                        if (FindIdent(&type_node, symtab, qualified_name) != -1 &&
                            type_node != NULL && type_node->type != NULL)
                        {
                            field_type = type_node->type;
                            kgpc_type_retain(field_type);
                            free(field->type_id);
                            field->type_id = strdup(qualified_name);
                        }
                    }
                }
                else if (field->is_array)
                {
                    KgpcType *elem_type = NULL;
                    if (field->array_element_kgpc_type != NULL)
                    {
                        /* Pre-built element type for nested arrays (array of array of ...) */
                        elem_type = field->array_element_kgpc_type;
                        kgpc_type_retain(elem_type);
                    }
                    else if (field->array_element_type_id != NULL)
                    {
                        HashNode_t *elem_node = NULL;
                        if (FindIdent(&elem_node, symtab, field->array_element_type_id) != -1 &&
                            elem_node != NULL && elem_node->type != NULL)
                        {
                            elem_type = elem_node->type;
                            kgpc_type_retain(elem_type);
                        }
                        else if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                            pascal_identifier_equals(field->name, "FStandardEncodings"))
                        {
                            fprintf(stderr,
                                "[KGPC_DEBUG_CLASS_VAR] element type lookup failed for %s (node=%p type=%p)\n",
                                field->array_element_type_id ? field->array_element_type_id : "<null>",
                                (void *)elem_node,
                                elem_node ? (void *)elem_node->type : NULL);
                        }
                        else if (elem_node != NULL)
                        {
                            struct RecordType *elem_record = get_record_type_from_node(elem_node);
                            if (elem_record != NULL)
                                elem_type = create_record_type(elem_record);
                        }
                    }
                    else if (field->array_element_type != UNKNOWN_TYPE)
                    {
                        elem_type = create_primitive_type(field->array_element_type);
                    }
                    if (elem_type != NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                            pascal_identifier_equals(field->name, "FStandardEncodings"))
                        {
                            fprintf(stderr,
                                "[KGPC_DEBUG_CLASS_VAR] resolved elem kgpc=%s kind=%d\n",
                                kgpc_type_to_string(elem_type),
                                elem_type ? elem_type->kind : -1);
                        }
                        field_type = create_array_type(elem_type, field->array_start, field->array_end);
                    }
                }
                
                /* Push onto scope - using typed variant */
                if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR") != NULL &&
                    pascal_identifier_equals(field->name, "FStandardEncodings"))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_CLASS_VAR] field_type=%s\n",
                        field_type ? kgpc_type_to_string(field_type) : "<null>");
                }
                PushVarOntoScope_Typed(symtab, field->name, field_type);
                
                /* Release our ref if we retained it */
                if (field_type != NULL)
                    destroy_kgpc_type(field_type);
            }
        }
        field_node = field_node->next;
    }

    /* Class properties are NOT pushed to scope here; they are resolved
     * dynamically in semcheck_varid() by rewriting the identifier to
     * the backing field (read accessor) at resolution time. This avoids
     * scope level issues in codegen. */

    /* Add other static methods of the same class to scope, so they can be
     * called without full qualification from within a static method. */
    ListNode_t *class_methods = NULL;
    int method_count = 0;
    get_class_methods(class_name, &class_methods, &method_count);
    
    ListNode_t *cur_method = class_methods;
    while (cur_method != NULL)
    {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur_method->cur;
        if (binding != NULL && binding->method_name != NULL && binding->method_name[0] != '\0')
        {
            /* Build the mangled name: ClassName__MethodName */
            size_t mangled_len = strlen(class_name) + 2 + strlen(binding->method_name) + 1;
            char *mangled_name = (char *)malloc(mangled_len);
            if (mangled_name != NULL)
            {
                snprintf(mangled_name, mangled_len, "%s__%s", class_name, binding->method_name);
                
                /* Look up the mangled method in the symbol table */
                HashNode_t *method_node = NULL;
                if (FindIdent(&method_node, symtab, mangled_name) != -1 && method_node != NULL)
                {
                    /* Add an alias using just the method name */
                    HashNode_t *existing = NULL;
                    if (FindIdent(&existing, symtab, binding->method_name) == -1)
                    {
                        /* Push the method onto scope using its short name,
                         * with the actual fully-mangled assembly label (not
                         * the intermediate ClassName__MethodName form). */
                        if (method_node->type != NULL)
                        {
                            const char *real_mangled = method_node->mangled_id ? method_node->mangled_id : mangled_name;
                            PushFunctionOntoScope_Typed(symtab, binding->method_name,
                                                        real_mangled, method_node->type);
                        }
                    }
                }
                free(mangled_name);
            }
        }
        cur_method = cur_method->next;
    }
    
    /* Clean up the class_methods list */
    while (class_methods != NULL)
    {
        ListNode_t *next = class_methods->next;
        free(class_methods);
        class_methods = next;
    }

    free(class_name);
}

static void add_class_vars_to_method_scope(SymTab_t *symtab, Tree_t *subprogram)
{
    if (subprogram == NULL)
        return;
    add_class_vars_to_method_scope_impl(symtab,
        subprogram->tree_data.subprogram_data.owner_class,
        subprogram->tree_data.subprogram_data.method_name,
        subprogram->tree_data.subprogram_data.is_operator);
}

static void add_outer_class_vars_to_method_scope(SymTab_t *symtab, Tree_t *subprogram)
{
    if (subprogram == NULL)
        return;
    add_class_vars_to_method_scope_impl(symtab,
        subprogram->tree_data.subprogram_data.owner_class_outer,
        subprogram->tree_data.subprogram_data.method_name,
        subprogram->tree_data.subprogram_data.is_operator);
}

/**
 * For a method implementation (ClassName__MethodName), copy default parameter
 * values from the class declaration to the implementation's parameters.
 * This is needed because default values are specified in the class declaration
 * but not repeated in the implementation.
 */
static void copy_method_decl_defaults_to_impl(SymTab_t *symtab, Tree_t *subprogram)
{
    if (symtab == NULL || subprogram == NULL)
        return;
    
    /* Check if this is a method */
    const char *owner = subprogram->tree_data.subprogram_data.owner_class;
    const char *method_name = subprogram->tree_data.subprogram_data.method_name;
    if (owner == NULL || method_name == NULL || method_name[0] == '\0')
        return;

    char *class_name = strdup(owner);
    if (class_name == NULL)
        return;
    
    /* Look up the class type */
    HashNode_t *class_node = NULL;
    if (FindIdent(&class_node, symtab, class_name) == -1 || class_node == NULL)
    {
        free(class_name);
        return;
    }
    
    struct RecordType *record_info = get_record_type_from_node(class_node);
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }
    
    /* Find the method template with the declaration's parameters */
    struct MethodTemplate *template = from_cparser_get_method_template(record_info, method_name);
    if (template == NULL || template->params_ast == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
            fprintf(stderr, "[copy_method_decl_defaults] No method template found for %s.%s\n",
                class_name, method_name);
        free(class_name);
        return;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
        fprintf(stderr, "[copy_method_decl_defaults] Found template for %s.%s, params_ast=%p typ=%d\n",
            class_name, method_name, (void*)template->params_ast, template->params_ast->typ);
    
    /* Convert the declaration's params_ast to a parameter list */
    /* The params_ast is a PASCAL_T_PARAM or PASCAL_T_PARAM_LIST node */
    ast_t *decl_params_ast = template->params_ast;
    ast_t *param_cursor = decl_params_ast;
    if (decl_params_ast->typ == PASCAL_T_PARAM_LIST)
        param_cursor = decl_params_ast->child;
    
    /* Iterate through implementation params and declaration params in parallel,
     * copying defaults from declaration to implementation */
    ListNode_t *impl_param = subprogram->tree_data.subprogram_data.args_var;
    
    /* Skip the Self parameter if present (it won't be in the declaration) */
    if (impl_param != NULL)
    {
        Tree_t *first_param = (Tree_t *)impl_param->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL)
        {
            ListNode_t *ids = first_param->tree_data.var_decl_data.ids;
            if (ids != NULL && ids->cur != NULL)
            {
                const char *param_name = (const char *)ids->cur;
                if (param_name != NULL && strcasecmp(param_name, "Self") == 0)
                    impl_param = impl_param->next;
            }
        }
    }
    
    while (param_cursor != NULL && impl_param != NULL)
    {
        ast_t *decl_param = param_cursor;
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
            fprintf(stderr, "[copy_method_decl_defaults] Processing decl_param typ=%d\n", decl_param->typ);
        
        if (decl_param->typ == PASCAL_T_PARAM)
        {
            /* Find the TYPE_SPEC and DEFAULT_VALUE in the declaration param */
            ast_t *type_spec = NULL;
            ast_t *default_value = NULL;
            
            for (ast_t *child = decl_param->child; child != NULL; child = child->next)
            {
                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    fprintf(stderr, "[copy_method_decl_defaults]   child typ=%d\n", child->typ);
                if (child->typ == PASCAL_T_TYPE_SPEC)
                    type_spec = child;
                if (child->typ == PASCAL_T_DEFAULT_VALUE)
                    default_value = child;
            }
            
            /* Check if TYPE_SPEC->next is DEFAULT_VALUE (alternate structure) */
            if (default_value == NULL && type_spec != NULL && type_spec->next != NULL && 
                type_spec->next->typ == PASCAL_T_DEFAULT_VALUE)
            {
                default_value = type_spec->next;
            }
            
            if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                fprintf(stderr, "[copy_method_decl_defaults] type_spec=%p default_value=%p\n",
                    (void*)type_spec, (void*)default_value);
            
            if (default_value != NULL)
            {
                /* Get the implementation parameter */
                Tree_t *impl_decl = (Tree_t *)impl_param->cur;
                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    fprintf(stderr, "[copy_method_decl_defaults] impl_decl=%p type=%d\n",
                        (void*)impl_decl, impl_decl ? impl_decl->type : -1);
                
                if (impl_decl != NULL && impl_decl->type == TREE_VAR_DECL)
                {
                    /* Only copy if impl doesn't already have a default */
                    if (impl_decl->tree_data.var_decl_data.initializer == NULL)
                    {
                        /* Convert default_value->child to Expression and wrap in initializer */
                        ast_t *expr_node = default_value->child;
                        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                            fprintf(stderr, "[copy_method_decl_defaults] expr_node=%p typ=%d\n",
                                (void*)expr_node, expr_node ? expr_node->typ : -1);
                        
                        if (expr_node != NULL)
                        {
                            struct Expression *default_expr = from_cparser_convert_expression(expr_node);
                            if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                                fprintf(stderr, "[copy_method_decl_defaults] default_expr=%p\n",
                                    (void*)default_expr);

                            if (default_expr != NULL)
                            {
                                impl_decl->tree_data.var_decl_data.initializer =
                                    mk_varassign(default_value->line, 0, NULL, default_expr);
                                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                                    fprintf(stderr, "[copy_method_decl_defaults] COPIED default value!\n");
                            }
                        }
                    }
                    else if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL)
                    {
                        fprintf(stderr, "[copy_method_decl_defaults] impl already has initializer\n");
                    }
                }
            }
        }
        
        param_cursor = param_cursor->next;
        impl_param = impl_param->next;
    }
    
    free(class_name);
}
                            
/* Copy method identity fields from a subprogram tree node into a hash node,
 * if the hash node doesn't already have them.  We strdup so the hash node
 * owns the memory and DestroyHashTable can free it uniformly. */
static void copy_method_identity_to_node(HashNode_t *node, Tree_t *subprogram)
{
    if (node == NULL || subprogram == NULL ||
        subprogram->tree_data.subprogram_data.method_name == NULL)
        return;
    if (node->method_name == NULL)
        node->method_name = strdup(subprogram->tree_data.subprogram_data.method_name);
    if (node->owner_class == NULL && subprogram->tree_data.subprogram_data.owner_class != NULL)
        node->owner_class = strdup(subprogram->tree_data.subprogram_data.owner_class);
    if (node->owner_class_full == NULL && subprogram->tree_data.subprogram_data.owner_class_full != NULL)
        node->owner_class_full = strdup(subprogram->tree_data.subprogram_data.owner_class_full);
    if (node->owner_class_outer == NULL && subprogram->tree_data.subprogram_data.owner_class_outer != NULL)
        node->owner_class_outer = strdup(subprogram->tree_data.subprogram_data.owner_class_outer);
    if (subprogram->tree_data.subprogram_data.is_operator)
        node->is_operator = 1;
}

static void semcheck_propagate_method_identity(SymTab_t *symtab, Tree_t *subprogram)
{
    if (symtab == NULL || subprogram == NULL)
        return;

    const char *id = subprogram->tree_data.subprogram_data.id;
    const char *mangled = subprogram->tree_data.subprogram_data.mangled_id;
    if (id == NULL || mangled == NULL)
        return;

    ListNode_t *candidates = FindAllIdents(symtab, id);
    if (candidates == NULL)
        return;

    ListNode_t *cur = candidates;
    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->mangled_id != NULL &&
            strcmp(candidate->mangled_id, mangled) == 0)
        {
            if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE &&
                candidate->type->info.proc_info.definition == NULL)
                candidate->type->info.proc_info.definition = subprogram;

            copy_method_identity_to_node(candidate, subprogram);
        }
        cur = cur->next;
    }

    DestroyList(candidates);
}

static HashNode_t *semcheck_find_preferred_type_node_ref_internal(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id, int skip_owner_qualify,
    int override_unit_index)
{
    if (symtab == NULL)
        return NULL;

    char *rendered = NULL;
    const char *lookup_id = type_id;
    if (type_ref != NULL)
    {
        rendered = type_ref_render_mangled(type_ref);
        if (rendered != NULL)
            lookup_id = rendered;
    }
    if (lookup_id == NULL)
    {
        free(rendered);
        return NULL;
    }

    int prefer_unit_defined = semcheck_is_explicit_unit_qualified_type_ref(type_ref);
    int qualified_unit_index = 0;
    char *qualified_unit_name = NULL;
    if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count > 1)
    {
        size_t qlen = 0;
        for (int i = 0; i < type_ref->name->count - 1; ++i)
        {
            const char *seg = type_ref->name->segments[i];
            if (seg != NULL)
                qlen += strlen(seg);
            if (i + 1 < type_ref->name->count - 1)
                qlen += 1;
        }
        if (qlen > 0)
        {
            qualified_unit_name = (char *)malloc(qlen + 1);
            if (qualified_unit_name != NULL)
            {
                qualified_unit_name[0] = '\0';
                for (int i = 0; i < type_ref->name->count - 1; ++i)
                {
                    const char *seg = type_ref->name->segments[i];
                    if (seg != NULL)
                        strcat(qualified_unit_name, seg);
                    if (i + 1 < type_ref->name->count - 1)
                        strcat(qualified_unit_name, ".");
                }
                if (unit_registry_contains(qualified_unit_name))
                    qualified_unit_index = unit_registry_add(qualified_unit_name);
            }
        }
    }
    ListNode_t *matches = FindAllIdents(symtab, lookup_id);
    if (matches == NULL && type_ref != NULL && type_ref->name != NULL &&
        type_ref->name->count > 1)
    {
        const char *base_name = type_ref_base_name(type_ref);
        if (base_name != NULL)
            matches = FindAllIdents(symtab, base_name);
    }

    if (matches == NULL && type_ref != NULL && type_ref->num_generic_args > 0)
    {
        const char *base_name = type_ref_base_name(type_ref);
        int arg_count = type_ref->num_generic_args;
        char **arg_types = NULL;
        if (base_name != NULL && arg_count > 0)
        {
            arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
            if (arg_types != NULL)
            {
                int ok = 1;
                for (int i = 0; i < arg_count; ++i)
                {
                    arg_types[i] = type_ref_render_mangled(type_ref->generic_args[i]);
                    if (arg_types[i] == NULL)
                        ok = 0;
                }
                if (!ok)
                {
                    for (int i = 0; i < arg_count; ++i)
                        free(arg_types[i]);
                    free(arg_types);
                    arg_types = NULL;
                }
            }
        }

        if (base_name != NULL && arg_types != NULL && arg_count > 0)
        {
            GenericTypeDecl *generic = generic_registry_find_decl(base_name);
            if (generic != NULL && generic->record_template != NULL &&
                generic->num_type_params == arg_count)
            {
                struct RecordType *record = clone_record_type(generic->record_template);
                if (record != NULL)
                {
                    if (record->type_id != NULL)
                        free(record->type_id);
                    record->type_id = strdup(lookup_id);
                    record->generic_decl = generic;
                    record->num_generic_args = arg_count;
                    record->generic_args = arg_types;
                    record->is_generic_specialization = 1;
                    arg_types = NULL;

                    /* Substitute generic parameters in record fields/properties. */
                    ListNode_t *field_node = record->fields;
                    while (field_node != NULL)
                    {
                        if (field_node->type == LIST_RECORD_FIELD)
                        {
                            struct RecordField *field = (struct RecordField *)field_node->cur;
                            if (field != NULL)
                            {
                                for (int i = 0; i < generic->num_type_params; ++i)
                                {
                                    const char *param = generic->type_parameters[i];
                                    const char *arg = record->generic_args[i];
                                    if (param != NULL && arg != NULL)
                                    {
                                        if (field->type_id != NULL &&
                                            strcasecmp(field->type_id, param) == 0)
                                        {
                                            free(field->type_id);
                                            field->type_id = strdup(arg);
                                        }
                                        if (field->array_element_type_id != NULL &&
                                            strcasecmp(field->array_element_type_id, param) == 0)
                                        {
                                            free(field->array_element_type_id);
                                            field->array_element_type_id = strdup(arg);
                                        }
                                    }
                                }
                            }
                        }
                        field_node = field_node->next;
                    }
                    ListNode_t *prop_node = record->properties;
                    while (prop_node != NULL)
                    {
                        if (prop_node->type == LIST_CLASS_PROPERTY)
                        {
                            struct ClassProperty *property = (struct ClassProperty *)prop_node->cur;
                            if (property != NULL && property->type_id != NULL)
                            {
                                for (int i = 0; i < generic->num_type_params; ++i)
                                {
                                    const char *param = generic->type_parameters[i];
                                    const char *arg = record->generic_args[i];
                                    if (param != NULL && arg != NULL &&
                                        strcasecmp(property->type_id, param) == 0)
                                    {
                                        free(property->type_id);
                                        property->type_id = strdup(arg);
                                        break;
                                    }
                                }
                            }
                        }
                        prop_node = prop_node->next;
                    }

                    generic_registry_add_specialization(base_name, record->generic_args, arg_count);

                    KgpcType *kgpc_type = create_record_type(record);
                    if (record_type_is_class(record))
                        kgpc_type = create_pointer_type(kgpc_type);
                    if (kgpc_type != NULL)
                    {
                        PushTypeOntoScope_Typed(symtab, (char *)lookup_id, kgpc_type);
                        /* Release creator's reference */
                        destroy_kgpc_type(kgpc_type);
                    }
                }
            }
        }

        if (arg_types != NULL)
        {
            for (int i = 0; i < arg_count; ++i)
                free(arg_types[i]);
            free(arg_types);
        }

        matches = FindAllIdents(symtab, lookup_id);
    }

    if (matches == NULL && type_ref != NULL)
    {
        const char *base = type_ref_base_name(type_ref);
        if (base != NULL && base != lookup_id)
            matches = FindAllIdents(symtab, base);
    }

    if (!skip_owner_qualify && matches == NULL)
    {
        int allow_owner_fallback = 0;
        if (type_ref == NULL)
        {
            allow_owner_fallback = 1;
        }
        else if (type_ref->name != NULL &&
                 type_ref->name->count == 1 &&
                 !semcheck_is_explicit_unit_qualified_type_ref(type_ref))
        {
            allow_owner_fallback = 1;
        }

        if (allow_owner_fallback)
        {
            const char *owner_id = semcheck_get_current_method_owner();
            if (owner_id != NULL)
            {
                char qualified_name[512];
                snprintf(qualified_name, sizeof(qualified_name), "%s.%s", owner_id, lookup_id);
                HashNode_t *qualified = semcheck_find_preferred_type_node_ref_internal(
                    symtab, NULL, qualified_name, 1, override_unit_index);
                if (qualified != NULL)
                {
                    free(qualified_unit_name);
                    if (rendered != NULL)
                        free(rendered);
                    return qualified;
                }
            }
        }
    }

    HashNode_t *best = NULL;
    int best_unit_rank = INT_MAX / 2;
    int best_scope_level = INT_MAX / 2;
    int best_same_unit = 0;
    int best_is_forward_stub = 1;
    int debug_tsize = (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL && rendered != NULL &&
                       pascal_identifier_equals(rendered, "TSize"));
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (qualified_unit_index > 0 &&
                node->source_unit_index > 0 &&
                node->source_unit_index != qualified_unit_index)
            {
                /* Accept if the type's unit or the qualifier's unit is
                 * a dependency of the other (covers transitive merging). */
                if (!unit_registry_is_dep(qualified_unit_index, node->source_unit_index) &&
                    !unit_registry_is_dep(node->source_unit_index, qualified_unit_index))
                {
                    cur = cur->next;
                    continue;
                }
            }
            if (node->source_unit_index != 0)
            {
                const char *unit_name = unit_registry_get(node->source_unit_index);
                int allowed = (unit_name == NULL) ? 1 : semcheck_is_unit_name(unit_name);
                if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL && lookup_id != NULL)
                {
                    if (pascal_identifier_equals(lookup_id, "TFloatFormatProfile") ||
                        pascal_identifier_equals(lookup_id, "TFloatSpecial") ||
                        pascal_identifier_equals(lookup_id, "TDIY_FP_Power_of_10") ||
                        pascal_identifier_equals(lookup_id, "pshortstring") ||
                        pascal_identifier_equals(lookup_id, "T"))
                    {
                        fprintf(stderr,
                            "[MISSING_TYPE] candidate id=%s src_unit=%d(%s) allowed=%d\n",
                            node->id ? node->id : "<null>",
                            node->source_unit_index,
                            unit_name ? unit_name : "<null>",
                            allowed);
                    }
                }
                if (unit_name != NULL && !allowed)
                {
                    cur = cur->next;
                    continue;
                }
            }
            int scope_level = semcheck_scope_level_for_type_candidate(symtab, node);
            int is_forward_stub = semcheck_type_candidate_is_forward_stub(node);

            int same_unit = 0;
            int effective_unit_index = override_unit_index;
            /* Prefer the active semantic unit context when available. Imported
             * declarations and unit subprogram bodies run under symtab->unit_context,
             * while g_semcheck_current_unit_index remains the root unit being loaded. */
            if (effective_unit_index == 0 && symtab->unit_context > 0)
                effective_unit_index = symtab->unit_context;
            if (effective_unit_index == 0)
                effective_unit_index = g_semcheck_current_unit_index;
            int unit_rank = 0;
            if (prefer_unit_defined || effective_unit_index > 0)
                /* In unit context, prefer unit-defined types over local program types */
                unit_rank = node->defined_in_unit ? 0 : 1;
            else
                unit_rank = node->defined_in_unit ? 1 : 0;
            if (!prefer_unit_defined && effective_unit_index != 0 &&
                node->source_unit_index != 0 &&
                node->source_unit_index == effective_unit_index)
                same_unit = 1;

            if (debug_tsize || (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                lookup_id != NULL && pascal_identifier_equals(lookup_id, "TSize")))
            {
                const char *uname = unit_registry_get(node->source_unit_index);
                fprintf(stderr, "[TSIZE] candidate id='%s' defined_in_unit=%d source_unit_idx=%d(%s) "
                    "unit_rank=%d scope_level=%d same_unit=%d forward_stub=%d effective_unit=%d unit_context=%d type=%p kind=%d\n",
                    node->id ? node->id : "<null>", node->defined_in_unit,
                    node->source_unit_index, uname ? uname : "?",
                    unit_rank, scope_level, same_unit, is_forward_stub, effective_unit_index,
                    symtab->unit_context,
                    (void *)node->type, node->type ? node->type->kind : -1);
            }

            int take = 0;
            if (best == NULL)
            {
                take = 1;
            }
            else if (prefer_unit_defined)
            {
                if (unit_rank < best_unit_rank)
                    take = 1;
                else if (unit_rank == best_unit_rank && scope_level < best_scope_level)
                    take = 1;
            }
            else if (same_unit > best_same_unit)
            {
                take = 1;
            }
            else if (same_unit == best_same_unit &&
                     (is_forward_stub < best_is_forward_stub) ||
                      (is_forward_stub == best_is_forward_stub &&
                       (scope_level < best_scope_level ||
                      (scope_level == best_scope_level && unit_rank < best_unit_rank) ||
                      (scope_level == best_scope_level && unit_rank == best_unit_rank &&
                       node->source_unit_index > 0 && best->source_unit_index > 0 &&
                       node->source_unit_index > best->source_unit_index))))
            {
                take = 1;
            }

            if (debug_tsize)
                fprintf(stderr, "[TSIZE]   take=%d\n", take);

            if (take)
            {
                best = node;
                best_unit_rank = unit_rank;
                best_scope_level = scope_level;
                best_same_unit = same_unit;
                best_is_forward_stub = is_forward_stub;
            }
        }
        cur = cur->next;
    }
    if (debug_tsize && best != NULL)
    {
        fprintf(stderr, "[TSIZE] WINNER id='%s' defined_in_unit=%d source_unit_idx=%d kind=%d",
            best->id ? best->id : "<null>", best->defined_in_unit,
            best->source_unit_index, best->type ? best->type->kind : -1);
        if (best->type != NULL && best->type->kind == TYPE_KIND_RECORD)
        {
            struct RecordType *ri = best->type->info.record_info;
            fprintf(stderr, " record_info=%p", (void *)ri);
            if (ri != NULL)
            {
                int fc = 0;
                ListNode_t *f = ri->fields;
                while (f != NULL) { fc++; f = f->next; }
                fprintf(stderr, " field_count=%d type_id='%s'", fc,
                    ri->type_id ? ri->type_id : "<null>");
            }
        }
        else if (best->type != NULL && best->type->kind == TYPE_KIND_PRIMITIVE)
        {
            fprintf(stderr, " prim_tag=%d", best->type->info.primitive_type_tag);
        }
        fprintf(stderr, "\n");
    }
    if (matches != NULL)
        DestroyList(matches);
    free(qualified_unit_name);
    if (rendered != NULL)
        free(rendered);
    return best;
}

HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id)
{
    return semcheck_find_preferred_type_node_ref_internal(symtab, NULL, type_id, 0, 0);
}

HashNode_t *semcheck_find_preferred_type_node_for_unit(SymTab_t *symtab,
    const char *type_id, int unit_index)
{
    return semcheck_find_preferred_type_node_ref_internal(symtab, NULL, type_id, 0, unit_index);
}

HashNode_t *semcheck_find_preferred_type_node_with_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id)
{
    return semcheck_find_preferred_type_node_ref_internal(symtab, type_ref, type_id, 0, 0);
}

static HashNode_t *semcheck_find_type_node_with_unit_flag(SymTab_t *symtab,
    const char *type_id, int defined_in_unit)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    HashNode_t *best = NULL;
    HashNode_t *fallback_outermost = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (node->defined_in_unit == defined_in_unit)
            {
                best = node;
                break;
            }
            if (best == NULL)
                best = node;
            fallback_outermost = node;
        }
        cur = cur->next;
    }
    if (best != NULL && best->defined_in_unit != defined_in_unit && defined_in_unit)
        best = fallback_outermost;
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

static HashNode_t *semcheck_find_type_node_with_unit_flag_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id, int defined_in_unit)
{
    if (symtab == NULL)
        return NULL;

    char *rendered = NULL;
    const char *lookup_id = type_id;
    if (type_ref != NULL)
    {
        rendered = type_ref_render_mangled(type_ref);
        if (rendered != NULL)
            lookup_id = rendered;
    }
    if (lookup_id == NULL)
    {
        free(rendered);
        return NULL;
    }

    /* Try the rendered/mangled name first */
    HashNode_t *result = semcheck_find_type_node_with_unit_flag(symtab, lookup_id, defined_in_unit);

    /* Fall back to the bare base name when the mangled name yields nothing */
    if (result == NULL && type_ref != NULL)
    {
        const char *base = type_ref_base_name(type_ref);
        if (base != NULL && base != lookup_id)
            result = semcheck_find_type_node_with_unit_flag(symtab, base, defined_in_unit);
    }

    free(rendered);
    return result;
}

/* Helper function to get VarType from HashNode */
static inline enum VarType get_var_type_from_node(HashNode_t *node)
{
    if (node == NULL || node->type == NULL)
        return HASHVAR_UNTYPED;

    switch (node->type->kind)
    {
        case TYPE_KIND_PRIMITIVE:
        {
            int tag = kgpc_type_get_primitive_tag(node->type);
            switch (tag)
            {
                case INT_TYPE: return HASHVAR_INTEGER;
                case LONGINT_TYPE: return HASHVAR_LONGINT;
                case INT64_TYPE: return HASHVAR_INT64;
                case BYTE_TYPE: return HASHVAR_INTEGER;
                case WORD_TYPE: return HASHVAR_INTEGER;
                case LONGWORD_TYPE: return HASHVAR_LONGINT;
                case QWORD_TYPE: return HASHVAR_QWORD;
                case REAL_TYPE:
                case EXTENDED_TYPE: return HASHVAR_REAL;
                case BOOL: return HASHVAR_BOOLEAN;
                case CHAR_TYPE: return HASHVAR_CHAR;
                case STRING_TYPE: return HASHVAR_PCHAR;
                case SET_TYPE: return HASHVAR_SET;
                case ENUM_TYPE: return HASHVAR_ENUM;
                case FILE_TYPE: return HASHVAR_FILE;
                default: return HASHVAR_UNTYPED;
            }
        }
        case TYPE_KIND_POINTER:
            return HASHVAR_POINTER;
        case TYPE_KIND_ARRAY:
            return HASHVAR_ARRAY;
        case TYPE_KIND_RECORD:
            return HASHVAR_RECORD;
        case TYPE_KIND_PROCEDURE:
            return HASHVAR_PROCEDURE;
        default:
            return HASHVAR_UNTYPED;
    }
}

static inline void mark_hashnode_source_unit(HashNode_t *node, int unit_index) {
    if (node == NULL || unit_index <= 0 || node->source_unit_index != 0) return;
    node->source_unit_index = unit_index;
}

static inline void mark_hashnode_unit_info(SymTab_t *symtab, HashNode_t *node,
    int defined_in_unit, int is_public)
{
    if (node == NULL || !defined_in_unit)
        return;
    node->defined_in_unit = 1;
    node->unit_is_public = is_public ? 1 : 0;
    mark_hashnode_source_unit(node, g_semcheck_current_unit_index);
    if (symtab != NULL)
        SymTab_MoveHashNodeToBack(symtab, node);

    const char *cur_unit_str = unit_registry_get(g_semcheck_current_unit_index);
    if (symtab == NULL || node->hash_type != HASHTYPE_TYPE ||
        cur_unit_str == NULL || node->id == NULL)
        return;

    size_t qualified_len = strlen(cur_unit_str) + 1 + strlen(node->id) + 1;
    char *qualified_id = (char *)malloc(qualified_len);
    if (qualified_id == NULL)
        return;
    snprintf(qualified_id, qualified_len, "%s.%s", cur_unit_str, node->id);

    HashNode_t *existing = NULL;
    if (FindIdent(&existing, symtab, qualified_id) < 0 || existing == NULL)
    {
        if (symtab->stack_head != NULL && symtab->stack_head->cur != NULL)
        {
            if (node->type != NULL)
                kgpc_type_retain(node->type);
            int add_result = AddIdentToTable((HashTable_t *)symtab->stack_head->cur, qualified_id,
                NULL, HASHTYPE_TYPE, node->type);
            if (add_result != 0 && node->type != NULL)
                kgpc_type_release(node->type);
            if (FindIdent(&existing, symtab, qualified_id) >= 0 && existing != NULL)
            {
                existing->defined_in_unit = 1;
                existing->unit_is_public = node->unit_is_public;
                SymTab_MoveHashNodeToBack(symtab, existing);
            }
        }
    }

    free(qualified_id);
}

static inline void mark_latest_type_node_unit_info(SymTab_t *symtab, const char *type_id,
    int defined_in_unit, int unit_is_public, int source_unit_index)
{
    if (symtab == NULL || type_id == NULL)
        return;
    HashNode_t *type_node = NULL;
    FindIdent(&type_node, symtab, type_id);
    if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
    {
        mark_hashnode_unit_info(symtab, type_node, defined_in_unit, unit_is_public);
        mark_hashnode_source_unit(type_node, source_unit_index);
    }
}

static Tree_t *g_semcheck_current_subprogram = NULL;

static int semcheck_prefer_unit_defined_owner(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return 0;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.defined_in_unit;
}

const char *semcheck_get_current_subprogram_id(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.id;
}

int semcheck_get_current_unit_index(void)
{
    return g_semcheck_current_unit_index;
}

const char *semcheck_get_current_subprogram_result_var_name(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.result_var_name;
}

const char *semcheck_get_current_subprogram_method_name(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.method_name;
}

const char *semcheck_get_current_subprogram_owner_class(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class;
}

const char *semcheck_get_current_subprogram_owner_class_full(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_full;
}

const char *semcheck_get_current_subprogram_owner_class_outer(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_outer;
}

int semcheck_get_current_subprogram_is_constructor(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return 0;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.is_constructor;
}

KgpcType *semcheck_get_current_subprogram_return_kgpc_type(SymTab_t *symtab, int *owns_type)
{
    if (owns_type != NULL)
        *owns_type = 0;
    if (g_semcheck_current_subprogram == NULL || symtab == NULL)
        return NULL;

    const char *rt_id = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type_id;
    const TypeRef *rt_ref = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type_ref;
    if (rt_id != NULL)
    {
        HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab, rt_ref, rt_id);
        if (type_node == NULL)
        {
            const char *owner_full = g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_full;
            const char *owner_outer = g_semcheck_current_subprogram->tree_data.subprogram_data.owner_class_outer;
            type_node = semcheck_find_type_node_in_owner_chain(symtab, rt_id, owner_full, owner_outer);
        }
        if (type_node != NULL && type_node->type != NULL)
        {
            if (owns_type != NULL)
                *owns_type = 0;
            return type_node->type;
        }

        int builtin_tag = semcheck_map_builtin_type_name_local(rt_id);
        if (builtin_tag != UNKNOWN_TYPE)
        {
            if (owns_type != NULL)
                *owns_type = 1;
            return create_primitive_type(builtin_tag);
        }
    }

    int rt_tag = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type;
    if (rt_tag != UNKNOWN_TYPE)
    {
        if (owns_type != NULL)
            *owns_type = 1;
        return create_primitive_type(rt_tag);
    }

    return NULL;
}

ListNode_t *semcheck_clone_current_subprogram_actual_args(int include_self)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;

    ListNode_t *result = NULL;
    ListNode_t *tail = NULL;
    for (ListNode_t *cur = g_semcheck_current_subprogram->tree_data.subprogram_data.args_var;
         cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        ListNode_t *ids = NULL;
        if (decl == NULL)
            continue;
        if (decl->type == TREE_VAR_DECL)
            ids = decl->tree_data.var_decl_data.ids;
        else if (decl->type == TREE_ARR_DECL)
            ids = decl->tree_data.arr_decl_data.ids;
        else
            continue;

        for (ListNode_t *id_node = ids; id_node != NULL; id_node = id_node->next)
        {
            const char *param_id = (const char *)id_node->cur;
            if (param_id == NULL)
                continue;
            if (!include_self && pascal_identifier_equals(param_id, "Self"))
                continue;

            struct Expression *arg_expr = mk_varid(g_semcheck_current_subprogram->line_num, strdup(param_id));
            if (arg_expr == NULL)
            {
                destroy_list(result);
                return NULL;
            }

            ListNode_t *node = CreateListNode(arg_expr, LIST_EXPR);
            if (node == NULL)
            {
                destroy_expr(arg_expr);
                destroy_list(result);
                return NULL;
            }

            if (result == NULL)
                result = node;
            else
                tail->next = node;
            tail = node;
        }
    }

    return result;
}

void semcheck_mark_static_link_needed(int scope_level, HashNode_t *node)
{
    if (scope_level <= 0)
        return;
    if (g_semcheck_current_subprogram == NULL || node == NULL)
        return;
    
    /* Only set requires_static_link if the function is actually nested (nesting_level > 1).
     * Top-level functions (nesting_level == 1) that access global variables do NOT need
     * a static link because global variables are accessed via their static labels.
     * 
     * This check prevents the issue where a top-level function like SetBacking, which
     * accesses a global variable, would incorrectly get requires_static_link=1 and then
     * callers would fail trying to pass a static link. */
    int nesting_level = g_semcheck_current_subprogram->tree_data.subprogram_data.nesting_level;
    if (nesting_level <= 1)
        return;

    switch (node->hash_type)
    {
        case HASHTYPE_VAR:
        case HASHTYPE_ARRAY:
        case HASHTYPE_FUNCTION_RETURN:
        case HASHTYPE_CONST:
            g_semcheck_current_subprogram->tree_data.subprogram_data.requires_static_link = 1;
            break;
        default:
            break;
    }
}

void semcheck_mark_call_requires_static_link(HashNode_t *callee)
{
    if (callee == NULL)
        return;
    if (g_semcheck_current_subprogram == NULL)
        return;
    if (!hashnode_requires_static_link(callee))
        return;
    /* When calling a function that requires a static link, the caller needs to
     * be able to PASS a static link. Mark the caller as having nested children
     * that need links, not as requiring a static link itself.
     * The caller only RECEIVES a static link if it's nested or accesses outer vars. */
    g_semcheck_current_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree);
int semcheck_unit(SymTab_t *symtab, Tree_t *tree);

int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num);
int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls);
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls);
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls);

/* Collect typed const declarations from a var_declaration list.
 * If from_unit_only is true, only collect those with defined_in_unit=1.
 * If from_unit_only is false, only collect those with defined_in_unit=0. */
static ListNode_t *collect_typed_const_decls_filtered(SymTab_t *symtab, ListNode_t *decls, int from_unit_only)
{
    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    ListNode_t *cur = decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if ((tree->type == TREE_VAR_DECL &&
                tree->tree_data.var_decl_data.is_typed_const) ||
                (tree->type == TREE_ARR_DECL &&
                tree->tree_data.arr_decl_data.is_typed_const))
            {
                /* Filter by origin */
                int is_from_unit = (tree->type == TREE_VAR_DECL) ?
                    tree->tree_data.var_decl_data.defined_in_unit :
                    tree->tree_data.arr_decl_data.defined_in_unit;
                if ((from_unit_only && !is_from_unit) || (!from_unit_only && is_from_unit))
                {
                    cur = cur->next;
                    continue;
                }
                
                int allow = 1;
                const char *type_id = NULL;
                int type_tag = UNKNOWN_TYPE;
                if (tree->type == TREE_VAR_DECL)
                {
                    type_id = tree->tree_data.var_decl_data.type_id;
                    type_tag = tree->tree_data.var_decl_data.type;
                }
                else
                {
                    type_id = tree->tree_data.arr_decl_data.type_id;
                    type_tag = tree->tree_data.arr_decl_data.type;
                }

                if (type_id != NULL)
                {
                    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_id);
                    if (type_node == NULL &&
                        semcheck_map_builtin_type_name_local(type_id) == UNKNOWN_TYPE)
                    {
                        allow = 0;
                    }
                }
                else if (type_tag == UNKNOWN_TYPE)
                {
                    if (tree->type == TREE_ARR_DECL &&
                        tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                    {
                        allow = 1;
                    }
                    else
                    {
                        allow = 0;
                    }
                }

                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.ids != NULL)
                {
                    const char *first_id = NULL;
                    if (tree->type == TREE_VAR_DECL)
                        first_id = tree->tree_data.var_decl_data.ids->cur ?
                            (const char *)tree->tree_data.var_decl_data.ids->cur : "<null>";
                    else
                        first_id = tree->tree_data.arr_decl_data.ids->cur ?
                            (const char *)tree->tree_data.arr_decl_data.ids->cur : "<null>";
                    fprintf(stderr, "[SemCheck] collect_typed_const: id=%s type_id=%s type=%d allow=%d inline_alias=%p\n",
                        first_id, type_id ? type_id : "<null>",
                        type_tag, allow,
                        (tree->type == TREE_VAR_DECL) ?
                            (void*)tree->tree_data.var_decl_data.inline_type_alias : NULL);
                }

                if (allow)
                {
                    ListNode_t *node = CreateListNode(tree, LIST_TREE);
                    if (head == NULL)
                    {
                        head = node;
                        tail = node;
                    }
                    else
                    {
                        tail->next = node;
                        tail = node;
                    }
                }
            }
        }
        cur = cur->next;
    }
    return head;
}

/* Collect non-typed var/array declarations (skip typed consts). */
static ListNode_t *collect_non_typed_var_decls(ListNode_t *decls)
{
    ListNode_t *head = NULL;
    ListNode_t *tail = NULL;
    ListNode_t *cur = decls;

    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            int skip = 0;
            if (tree->type == TREE_VAR_DECL)
                skip = tree->tree_data.var_decl_data.is_typed_const;
            else if (tree->type == TREE_ARR_DECL)
                skip = tree->tree_data.arr_decl_data.is_typed_const;
            else
                skip = 1;

            if (!skip)
            {
                ListNode_t *node = CreateListNode(tree, LIST_TREE);
                if (head == NULL)
                    head = node;
                else
                    tail->next = node;
                tail = node;
            }
        }
        cur = cur->next;
    }

    return head;
}

static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);

/* Resolve the return type for a function declaration once so callers share the same KgpcType. */
HashNode_t *semcheck_find_type_node_with_kgpc_type_ref(SymTab_t *symtab,
    const TypeRef *type_ref, const char *type_id)
{
    if (symtab == NULL || (type_id == NULL && type_ref == NULL))
        return NULL;

    /* Prefer the generic-aware lookup so we can instantiate types like
     * TFPGListEnumerator$TMyRecord on demand for function return types. */
    HashNode_t *preferred = semcheck_find_preferred_type_node_with_ref(symtab, type_ref, type_id);
    if (preferred != NULL && preferred->type != NULL)
        return preferred;

    char *rendered = NULL;
    const char *lookup_id = type_id;
    if (type_ref != NULL)
    {
        rendered = type_ref_render_mangled(type_ref);
        if (rendered != NULL)
            lookup_id = rendered;
    }
    if (lookup_id == NULL)
    {
        free(rendered);
        return NULL;
    }

    HashNode_t *result = NULL;
    ListNode_t *all_nodes = FindAllIdents(symtab, lookup_id);
    ListNode_t *cur = all_nodes;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL)
        {
            if (node->type != NULL)
            {
                result = node;
                break;
            }
            if (result == NULL)
                result = node;
        }
        cur = cur->next;
    }

    if (all_nodes != NULL)
        DestroyList(all_nodes);

    free(rendered);
    return result;
}

HashNode_t *semcheck_find_type_node_with_kgpc_type(SymTab_t *symtab, const char *type_id)
{
    return semcheck_find_type_node_with_kgpc_type_ref(symtab, NULL, type_id);
}

static KgpcType *build_function_return_type(Tree_t *subprogram, SymTab_t *symtab,
    int *error_count, int allow_undefined)
{
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_RETURN_TYPE");
    KgpcType *builtin_return = NULL;
    if (subprogram == NULL || symtab == NULL)
        return NULL;

    /* TODO: Once the symbol table tracks placeholder types, this helper should
     * validate that any returned KgpcType has been fully resolved. */
    HashNode_t *type_node = NULL;
    if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
    {
        const char *owner_full = subprogram->tree_data.subprogram_data.owner_class_full;
        const char *owner_outer = subprogram->tree_data.subprogram_data.owner_class_outer;
        if (owner_full == NULL)
            owner_full = subprogram->tree_data.subprogram_data.owner_class;
        if (owner_full != NULL)
        {
            semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                &subprogram->tree_data.subprogram_data.return_type_id,
                &subprogram->tree_data.subprogram_data.return_type_ref);
        }
        type_node = semcheck_find_type_node_with_kgpc_type_ref(symtab,
            subprogram->tree_data.subprogram_data.return_type_ref,
            subprogram->tree_data.subprogram_data.return_type_id);
        if (type_node == NULL)
        {
            const char *owner_full = subprogram->tree_data.subprogram_data.owner_class_full;
            const char *owner_outer = subprogram->tree_data.subprogram_data.owner_class_outer;
            type_node = semcheck_find_type_node_in_owner_chain(symtab,
                subprogram->tree_data.subprogram_data.return_type_id, owner_full, owner_outer);
        }
        if (type_node == NULL)
        {
            /* Before reporting error, check for builtin types not in symbol table */
            const char *type_id = subprogram->tree_data.subprogram_data.return_type_id;
            const TypeRef *type_ref = subprogram->tree_data.subprogram_data.return_type_ref;
            const char *type_base = type_ref != NULL ? type_ref_base_name(type_ref) : type_id;
            int builtin_type = semcheck_map_builtin_type_name_local(type_base);
            if (builtin_type == UNKNOWN_TYPE)
            {
                /* Check if this is a generic type parameter of the function */
                int is_generic_param = 0;
                for (int i = 0; i < subprogram->tree_data.subprogram_data.num_generic_type_params; i++) {
                    if (type_base != NULL &&
                        strcasecmp(type_base, subprogram->tree_data.subprogram_data.generic_type_params[i]) == 0) {
                        is_generic_param = 1;
                        break;
                    }
                }
                if (is_generic_param) {
                    builtin_return = create_primitive_type(POINTER_TYPE);
                } else if (!allow_undefined)
                {
                    semantic_error(subprogram->line_num, 0, "undefined type %s",
                        subprogram->tree_data.subprogram_data.return_type_id);
                    if (error_count != NULL)
                        ++(*error_count);
                }
            }
            else
            {
                subprogram->tree_data.subprogram_data.return_type = builtin_type;
                builtin_return = create_primitive_type(builtin_type);
            }
        }
    }

    if (debug_env != NULL)
    {
        const char *rt_id = subprogram->tree_data.subprogram_data.return_type_id;
        int primitive_tag = subprogram->tree_data.subprogram_data.return_type;
        const char *resolved_type = (type_node != NULL && type_node->type != NULL)
            ? kgpc_type_to_string(type_node->type)
            : "<null>";
        fprintf(stderr,
            "[KGPC] build_function_return_type: subprogram=%s return_type_id=%s primitive=%d type_node=%p kind=%d resolved=%s\n",
            subprogram->tree_data.subprogram_data.id ? subprogram->tree_data.subprogram_data.id : "<anon>",
            rt_id ? rt_id : "<null>",
            primitive_tag,
            (void *)type_node,
            (type_node != NULL && type_node->type != NULL) ? type_node->type->kind : -1,
            resolved_type);
    }

    if (builtin_return != NULL)
        return builtin_return;

    return kgpc_type_build_function_return(
        subprogram->tree_data.subprogram_data.inline_return_type,
        type_node,
        subprogram->tree_data.subprogram_data.return_type,
        symtab);
}

/* Forward declarations for type resolution helpers used in const evaluation. */
static HashNode_t *find_type_node_with_range_metadata(SymTab_t *symtab,
    const char *base_name, const struct TypeAlias *exclude_alias);
static HashNode_t *semcheck_find_exact_type_node_for_qid(SymTab_t *symtab,
    const QualifiedIdent *type_ref);
static const char *resolve_type_to_base_name(SymTab_t *symtab,
    const QualifiedIdent *type_id,
    const char *type_name);

static int is_real_type_name(SymTab_t *symtab, const char *type_name)
{
    if (type_name == NULL)
        return 0;

    const char *resolved = resolve_type_to_base_name(symtab, NULL, type_name);
    const char *name = (resolved != NULL) ? resolved : type_name;

    return (pascal_identifier_equals(name, "Real") ||
            pascal_identifier_equals(name, "Float") ||
            pascal_identifier_equals(name, "ValReal") ||
            pascal_identifier_equals(name, "Double") ||
            pascal_identifier_equals(name, "Single") ||
            pascal_identifier_equals(name, "Extended") ||
            pascal_identifier_equals(name, "Comp") ||
            pascal_identifier_equals(name, "Currency"));
}

/* Helper to check if an expression contains a real number literal or real constant */
static int expression_contains_real_literal_impl(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    if (expr->type == EXPR_RNUM)
        return 1;
    
    if (expr->type == EXPR_VAR_ID && symtab != NULL)
    {
        /* Check if this variable ID refers to a real constant */
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 &&
            node != NULL && node->hash_type == HASHTYPE_CONST &&
            node->type != NULL && kgpc_type_is_real(node->type))
        {
            return 1;
        }
        return 0;
    }
    
    if (expr->type == EXPR_SIGN_TERM)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.sign_term);
    }

    if (expr->type == EXPR_TYPECAST)
    {
        const char *target_id = expr->expr_data.typecast_data.target_type_id;
        int target_type = expr->expr_data.typecast_data.target_type;
        if (is_real_family_type(target_type) || is_real_type_name(symtab, target_id))
            return 1;
        return 0;
    }

    if (expr->type == EXPR_FUNCTION_CALL)
    {
        const char *id = expr->expr_data.function_call_data.id;
        if (id != NULL && pascal_identifier_equals(id, "Trunc"))
            return 0;
        if (id != NULL && is_real_type_name(symtab, id))
            return 1;
        return 0;
    }

    if (expr->type == EXPR_ADDOP)
    {
        return expression_contains_real_literal_impl(symtab, expr->expr_data.addop_data.left_expr) ||
               expression_contains_real_literal_impl(symtab, expr->expr_data.addop_data.right_term);
    }
    
    if (expr->type == EXPR_MULOP)
    {
        /* Pascal '/' (SLASH) always produces a real result, even with integer operands */
        if (expr->expr_data.mulop_data.mulop_type == SLASH)
            return 1;
        return expression_contains_real_literal_impl(symtab, expr->expr_data.mulop_data.left_term) ||
               expression_contains_real_literal_impl(symtab, expr->expr_data.mulop_data.right_factor);
    }
    
    return 0;
}

/* Helper to check if an expression is a string expression */
static int expression_is_string(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;
    
    if (expr->type == EXPR_STRING || expr->type == EXPR_CHAR_CODE)
        return 1;

    if (expr->type == EXPR_VAR_ID && symtab != NULL && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) != -1 && node != NULL)
        {
            if ((node->hash_type == HASHTYPE_CONST || node->is_typed_const) &&
                node->const_string_value != NULL)
                return 1;
            if (node->type != NULL && node->type->kind == TYPE_KIND_PRIMITIVE)
            {
                int tag = kgpc_type_get_primitive_tag(node->type);
                if (tag == STRING_TYPE || tag == SHORTSTRING_TYPE)
                    return 1;
            }
        }
    }
    
    if (expr->type == EXPR_ADDOP && expr->expr_data.addop_data.addop_type == PLUS)
    {
        /* String concatenation */
        return expression_is_string(symtab, expr->expr_data.addop_data.left_expr) ||
               expression_is_string(symtab, expr->expr_data.addop_data.right_term);
    }
    
    return 0;
}

static int const_fold_int_expr_mode(SymTab_t *symtab, struct Expression *expr,
    long long *out_value, int emit_diagnostics);
static int const_fold_real_expr_mode(SymTab_t *symtab, struct Expression *expr,
    double *out_value, int emit_diagnostics);
static int const_fold_int_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
static int const_fold_real_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value);
static int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name,
    const char *literal_name, long long *out_value);
static char *build_qualified_identifier_from_expr(struct Expression *expr);
static QualifiedIdent *build_qualified_ident_from_expr(struct Expression *expr);

static int expression_is_set_const_expr(SymTab_t *symtab, struct Expression *expr)
{
    if (expr == NULL)
        return 0;

    if (expr->type == EXPR_SET)
        return 1;

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) != -1 &&
            node != NULL && node->const_set_value != NULL)
            return 1;
    }

    if (expr->type == EXPR_RECORD_ACCESS &&
        expr->expr_data.record_access_data.field_id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.record_access_data.field_id) != -1 &&
            node != NULL && node->const_set_value != NULL)
            return 1;
    }

    if (expr->type == EXPR_ADDOP)
    {
        int op = expr->expr_data.addop_data.addop_type;
        if ((op == PLUS || op == MINUS) &&
            expression_is_set_const_expr(symtab, expr->expr_data.addop_data.left_expr) &&
            expression_is_set_const_expr(symtab, expr->expr_data.addop_data.right_term))
            return 1;
    }

    if (expr->type == EXPR_MULOP)
    {
        int op = expr->expr_data.mulop_data.mulop_type;
        if ((op == STAR || op == AND) &&
            expression_is_set_const_expr(symtab, expr->expr_data.mulop_data.left_term) &&
            expression_is_set_const_expr(symtab, expr->expr_data.mulop_data.right_factor))
            return 1;
    }

    return 0;
}

/* Evaluate a set literal into a byte array (supports up to 0..255) */
static int evaluate_set_const_bytes(SymTab_t *symtab, struct Expression *expr,
    unsigned char *out_bytes, size_t out_bytes_size, size_t *out_size,
    long long *out_mask, int *is_char_set)
{
    if (expr == NULL || out_bytes == NULL || out_bytes_size < 32)
        return 1;

    memset(out_bytes, 0, out_bytes_size);

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.id) != -1 &&
            node != NULL && node->const_set_value != NULL)
        {
            size_t size = (size_t)node->const_set_size;
            if (size > out_bytes_size)
                return 1;
            memcpy(out_bytes, node->const_set_value, size);
            if (out_size != NULL)
                *out_size = size;
            if (out_mask != NULL && size <= sizeof(long long))
            {
                long long mask = 0;
                for (size_t i = 0; i < size; ++i)
                    mask |= ((long long)out_bytes[i]) << (i * 8);
                *out_mask = mask;
            }
            if (is_char_set != NULL)
                *is_char_set = (size > 4);
            return 0;
        }
        return 1;
    }

    if (expr->type == EXPR_RECORD_ACCESS &&
        expr->expr_data.record_access_data.field_id != NULL)
    {
        HashNode_t *node = NULL;
        if (FindIdent(&node, symtab, expr->expr_data.record_access_data.field_id) != -1 &&
            node != NULL && node->const_set_value != NULL)
        {
            size_t size = (size_t)node->const_set_size;
            if (size > out_bytes_size)
                return 1;
            memcpy(out_bytes, node->const_set_value, size);
            if (out_size != NULL)
                *out_size = size;
            if (out_mask != NULL && size <= sizeof(long long))
            {
                long long mask = 0;
                for (size_t i = 0; i < size; ++i)
                    mask |= ((long long)out_bytes[i]) << (i * 8);
                *out_mask = mask;
            }
            if (is_char_set != NULL)
                *is_char_set = (size > 4);
            return 0;
        }
        return 1;
    }

    if (expr->type == EXPR_ADDOP || expr->type == EXPR_MULOP)
    {
        unsigned char left_bytes[32];
        unsigned char right_bytes[32];
        size_t left_size = 0;
        size_t right_size = 0;
        int left_char = 0;
        int right_char = 0;
        int op = (expr->type == EXPR_ADDOP) ?
            expr->expr_data.addop_data.addop_type :
            expr->expr_data.mulop_data.mulop_type;
        struct Expression *left_expr = (expr->type == EXPR_ADDOP) ?
            expr->expr_data.addop_data.left_expr :
            expr->expr_data.mulop_data.left_term;
        struct Expression *right_expr = (expr->type == EXPR_ADDOP) ?
            expr->expr_data.addop_data.right_term :
            expr->expr_data.mulop_data.right_factor;

        if (evaluate_set_const_bytes(symtab, left_expr, left_bytes, sizeof(left_bytes),
                &left_size, NULL, &left_char) != 0)
            return 1;
        if (evaluate_set_const_bytes(symtab, right_expr, right_bytes, sizeof(right_bytes),
                &right_size, NULL, &right_char) != 0)
            return 1;

        size_t used = (left_size > right_size) ? left_size : right_size;
        if (used == 0)
            used = 4;

        for (size_t i = 0; i < used; ++i)
        {
            unsigned char l = (i < left_size) ? left_bytes[i] : 0;
            unsigned char r = (i < right_size) ? right_bytes[i] : 0;
            if (expr->type == EXPR_ADDOP && op == PLUS)
                out_bytes[i] = (unsigned char)(l | r);
            else if (expr->type == EXPR_ADDOP && op == MINUS)
                out_bytes[i] = (unsigned char)(l & (unsigned char)(~r));
            else if (expr->type == EXPR_MULOP && (op == STAR || op == AND))
                out_bytes[i] = (unsigned char)(l & r);
            else
                return 1;
        }

        if (out_size != NULL)
            *out_size = used;
        if (out_mask != NULL && used <= sizeof(long long))
        {
            long long mask = 0;
            for (size_t i = 0; i < used; ++i)
                mask |= ((long long)out_bytes[i]) << (i * 8);
            *out_mask = mask;
        }
        if (is_char_set != NULL)
            *is_char_set = (left_char || right_char || used > 4);
        return 0;
    }

    if (expr->type != EXPR_SET)
        return 1;

    size_t used = 0;
    long long mask = 0;
    long long max_value = 0;
    int char_set = 0;

    ListNode_t *element = expr->expr_data.set_data.elements;
    while (element != NULL)
    {
        if (element->cur != NULL)
        {
            struct SetElement *set_element = (struct SetElement *)element->cur;
            long long lower = 0;
            long long upper = 0;
            if (set_element->lower == NULL ||
                evaluate_const_expr(symtab, set_element->lower, &lower) != 0)
            {
                fprintf(stderr, "Error: set element is not a constant expression.\n");
                return 1;
            }
            if (set_element->upper != NULL)
            {
                if (evaluate_const_expr(symtab, set_element->upper, &upper) != 0)
                {
                    fprintf(stderr, "Error: set element upper bound is not a constant expression.\n");
                    return 1;
                }
            }
            else
            {
                upper = lower;
            }

            if (lower > upper)
            {
                long long tmp = lower;
                lower = upper;
                upper = tmp;
            }

            if (upper > max_value)
                max_value = upper;
            if (upper > 31)
                char_set = 1;

            if (upper < 0 || upper > 255 || lower < 0)
            {
                fprintf(stderr, "Error: set literal value %lld out of supported range 0..255.\n", upper);
                return 1;
            }

            for (long long value = lower; value <= upper; ++value)
            {
                size_t byte_index = (size_t)(value / 8);
                unsigned bit_mask = 1u << (value % 8);
                out_bytes[byte_index] |= (unsigned char)bit_mask;
            }
        }
        element = element->next;
    }

    used = (max_value <= 31) ? 4 : 32;
    if (out_size != NULL)
        *out_size = used;
    if (out_mask != NULL && used <= sizeof(long long))
    {
        for (size_t i = 0; i < used; ++i)
            mask |= ((long long)out_bytes[i]) << (i * 8);
        *out_mask = mask;
    }
    if (is_char_set != NULL)
        *is_char_set = (char_set || max_value > 31);
    return 0;
}

/* Evaluates a string constant expression, allocating a new string */
static int evaluate_string_const_expr(SymTab_t *symtab, struct Expression *expr, char **out_value)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_STRING:
            *out_value = strdup(expr->expr_data.string);
            return (*out_value == NULL) ? 1 : 0;
        
        case EXPR_CHAR_CODE:
        {
            /* Character code: convert to a single-character string */
            *out_value = (char *)malloc(2);
            if (*out_value == NULL)
                return 1;
            (*out_value)[0] = (char)(expr->expr_data.char_code & 0xFF);
            (*out_value)[1] = '\0';
            return 0;
        }
        
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL && 
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const) &&
                node->const_string_value != NULL)
            {
                *out_value = strdup(node->const_string_value);
                return (*out_value == NULL) ? 1 : 0;
            }
            fprintf(stderr, "Error: constant %s is undefined or not a string const.\n", expr->expr_data.id);
            return 1;
        }
        
        case EXPR_ADDOP:
        {
            if (expr->expr_data.addop_data.addop_type != PLUS)
            {
                fprintf(stderr, "Error: only + operator is supported for string concatenation.\n");
                return 1;
            }
            
            char *left = NULL, *right = NULL;
            if (evaluate_string_const_expr(symtab, expr->expr_data.addop_data.left_expr, &left) != 0)
                return 1;
            if (evaluate_string_const_expr(symtab, expr->expr_data.addop_data.right_term, &right) != 0)
            {
                free(left);
                return 1;
            }
            
            /* Concatenate strings */
            size_t len = strlen(left) + strlen(right) + 1;
            *out_value = (char *)malloc(len);
            if (*out_value == NULL)
            {
                free(left);
                free(right);
                return 1;
            }
            strcpy(*out_value, left);
            strcat(*out_value, right);
            
            free(left);
            free(right);
            return 0;
        }
        
        default:
            break;
    }

    fprintf(stderr, "Error: unsupported string const expression.\n");
    return 1;
}

typedef struct SubprogramPredeclLookup
{
    HashNode_t *exact_match;
    HashNode_t *first_mangled_match;
    HashNode_t *tree_match;
    HashNode_t *body_pair_match;
} SubprogramPredeclLookup;

static int semcheck_subprogram_node_source_unit_index(HashNode_t *candidate)
{
    if (candidate == NULL)
        return 0;
    if (candidate->type != NULL &&
        candidate->type->kind == TYPE_KIND_PROCEDURE &&
        candidate->type->info.proc_info.definition != NULL)
    {
        int def_unit_idx =
            candidate->type->info.proc_info.definition->tree_data.subprogram_data.source_unit_index;
        if (def_unit_idx > 0)
            return def_unit_idx;
    }
    return candidate->source_unit_index;
}

static SubprogramPredeclLookup semcheck_lookup_subprogram_predecl(
    SymTab_t *symtab,
    Tree_t *subprogram,
    const char *lookup_id,
    const char *mangled_id)
{
    SubprogramPredeclLookup result;
    memset(&result, 0, sizeof(result));

    if (symtab == NULL || subprogram == NULL || lookup_id == NULL)
        return result;

    ListNode_t *all_matches = FindAllIdents(symtab, lookup_id);
    ListNode_t *cur = all_matches;
    int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
    int current_source_unit = subprogram->tree_data.subprogram_data.source_unit_index;

    while (cur != NULL)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        Tree_t *def = NULL;
        int existing_has_body = 0;
        int mangled_match = 0;
        int signature_match = 0;
        int candidate_source_unit = 0;

        if (candidate == NULL)
        {
            cur = cur->next;
            continue;
        }

        if (candidate->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
        {
            cur = cur->next;
            continue;
        }

        candidate_source_unit = semcheck_subprogram_node_source_unit_index(candidate);
        if ((current_source_unit > 0 && candidate_source_unit > 0 &&
             candidate_source_unit != current_source_unit) ||
            (current_source_unit == 0 && candidate_source_unit > 0))
        {
            cur = cur->next;
            continue;
        }

        if (candidate->mangled_id != NULL &&
            mangled_id != NULL &&
            strcmp(candidate->mangled_id, mangled_id) == 0)
        {
            mangled_match = 1;
            if (result.first_mangled_match == NULL)
                result.first_mangled_match = candidate;
        }

        if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
        {
            def = candidate->type->info.proc_info.definition;
            existing_has_body = (def != NULL &&
                def->tree_data.subprogram_data.statement_list != NULL);

            if (def == subprogram)
                result.tree_match = candidate;

            if (def != NULL &&
                semcheck_subprogram_signatures_equivalent(subprogram, def))
            {
                signature_match = 1;
                if (mangled_match && result.exact_match == NULL)
                    result.exact_match = candidate;
            }

            if (result.body_pair_match == NULL &&
                existing_has_body != current_has_body &&
                (signature_match || mangled_match))
            {
                result.body_pair_match = candidate;
            }
        }

        cur = cur->next;
    }

    if (all_matches != NULL)
        DestroyList(all_matches);

    return result;
}

static void semcheck_refresh_predecl_match(HashNode_t *node, Tree_t *subprogram)
{
    if (node == NULL || subprogram == NULL)
        return;

    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (node->mangled_id != NULL)
            free(node->mangled_id);
        node->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    if (subprogram->tree_data.subprogram_data.is_varargs)
        node->is_varargs = 1;
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
        node->defined_in_unit = 1;

    /* Propagate method identity so consumers can use structural fields
     * instead of parsing mangled identifiers. */
    copy_method_identity_to_node(node, subprogram);

    if (node->type != NULL &&
        node->type->kind == TYPE_KIND_PROCEDURE &&
        subprogram->tree_data.subprogram_data.statement_list != NULL)
    {
        Tree_t *prev_def = node->type->info.proc_info.definition;
        if (prev_def == NULL ||
            prev_def->tree_data.subprogram_data.statement_list == NULL)
        {
            node->type->info.proc_info.definition = subprogram;
        }
    }
}

static int semcheck_param_list_equivalent(ListNode_t *lhs, ListNode_t *rhs)
{
    ListNode_t *lcur = lhs;
    ListNode_t *rcur = rhs;
    while (lcur != NULL && rcur != NULL)
    {
        Tree_t *lhs_decl = (Tree_t *)lcur->cur;
        Tree_t *rhs_decl = (Tree_t *)rcur->cur;
        if (!semcheck_param_decl_equivalent(lhs_decl, rhs_decl))
            return 0;
        lcur = lcur->next;
        rcur = rcur->next;
    }
    return (lcur == NULL && rcur == NULL);
}

/* Extract and validate a single argument from a const-expression function call.
 * Returns the argument expression on success, or NULL on failure (with error
 * printed to stderr). */
static struct Expression *extract_single_const_arg(ListNode_t *args, const char *func_name)
{
    assert(func_name != NULL);
    if (args == NULL || args->next != NULL)
    {
        fprintf(stderr, "Error: %s in const expression requires exactly one argument.\n", func_name);
        return NULL;
    }
    struct Expression *arg = (struct Expression *)args->cur;
    if (arg == NULL)
    {
        fprintf(stderr, "Error: %s argument is NULL.\n", func_name);
        return NULL;
    }
    return arg;
}

/* Look up the low and high bounds of a built-in Pascal type by name.
 * Returns 1 on success (bounds written to *low_out and *high_out), 0 if the
 * type is not a recognised built-in. */
static int get_builtin_type_bounds(const char *base_name,
    long long *low_out, long long *high_out)
{
    assert(low_out != NULL && high_out != NULL);
    if (base_name == NULL)
        return 0;

    typedef struct {
        const char *names[3];
        long long low;
        long long high;
    } TypeBoundsEntry;
    /* Note: QWord/UInt64 high is INT64_MAX (not UINT64_MAX) because the
     * const-expression evaluator uses signed long long throughout.  Proper
     * unsigned 64-bit semantics are not yet supported. */
    static const TypeBoundsEntry table[] = {
        { {"Int64",    NULL,       NULL},         (-9223372036854775807LL - 1), 9223372036854775807LL },
        { {"QWord",    "UInt64",   NULL},         0LL,                         9223372036854775807LL  },
        { {"LongInt",  "Integer",  NULL},         -2147483648LL,               2147483647LL           },
        { {"ValSInt",  NULL,       NULL},         -2147483648LL,               2147483647LL           },
        { {"Cardinal", "LongWord", "DWord"},      0LL,                         4294967295LL           },
        { {"SmallInt", NULL,       NULL},         -32768LL,                    32767LL                },
        { {"Word",     NULL,       NULL},         0LL,                         65535LL                },
        { {"ShortInt", NULL,       NULL},         -128LL,                      127LL                  },
        { {"Byte",     NULL,       NULL},         0LL,                         255LL                  },
        { {"Boolean",  NULL,       NULL},         0LL,                         1LL                    },
        { {"Char",     "AnsiChar", NULL},         0LL,                         255LL                  },
    };
    if (pascal_identifier_equals(base_name, "SizeInt") ||
        pascal_identifier_equals(base_name, "NativeInt") ||
        pascal_identifier_equals(base_name, "PtrInt"))
    {
        if (sizeof(void*) >= 8)
        {
            *low_out = (-9223372036854775807LL - 1);
            *high_out = 9223372036854775807LL;
        }
        else
        {
            *low_out = -2147483648LL;
            *high_out = 2147483647LL;
        }
        return 1;
    }
    if (pascal_identifier_equals(base_name, "SizeUInt") ||
        pascal_identifier_equals(base_name, "NativeUInt") ||
        pascal_identifier_equals(base_name, "PtrUInt"))
    {
        if (sizeof(void*) >= 8)
        {
            *low_out = 0LL;
            *high_out = 9223372036854775807LL;
        }
        else
        {
            *low_out = 0LL;
            *high_out = 4294967295LL;
        }
        return 1;
    }
    for (size_t i = 0; i < sizeof(table) / sizeof(table[0]); ++i)
    {
        for (int j = 0; j < 3 && table[i].names[j] != NULL; ++j)
        {
            if (pascal_identifier_equals(base_name, table[i].names[j]))
            {
                *low_out = table[i].low;
                *high_out = table[i].high;
                return 1;
            }
        }
    }
    return 0;
}

static int const_fold_real_expr_mode(SymTab_t *symtab, struct Expression *expr, double *out_value,
    int emit_diagnostics)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_TYPECAST:
        {
            const char *target_id = expr->expr_data.typecast_data.target_type_id;
            int target_type = expr->expr_data.typecast_data.target_type;
            if (is_real_family_type(target_type) || is_real_type_name(symtab, target_id))
            {
                return const_fold_real_expr_mode(symtab, expr->expr_data.typecast_data.expr,
                    out_value, emit_diagnostics);
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: unsupported real const typecast target.\n");
            return 1;
        }
        case EXPR_RNUM:
            *out_value = (double)expr->expr_data.r_num;
            return 0;
        case EXPR_INUM:
            /* Integer in real context - promote to real */
            *out_value = (double)expr->expr_data.i_num;
            return 0;
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                if (node->type != NULL && kgpc_type_is_real(node->type))
                {
                    *out_value = node->const_real_value;
                    return 0;
                }
                *out_value = (double)node->const_int_value;
                return 0;
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: constant %s is undefined or not a const.\n", expr->expr_data.id);
            return 1;
        }
        case EXPR_SIGN_TERM:
        {
            double value;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.sign_term, &value,
                    emit_diagnostics) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_ADDOP:
        {
            double left, right;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.addop_data.left_expr, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.addop_data.right_term, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.addop_data.addop_type)
            {
                case PLUS:
                    *out_value = left + right;
                    return 0;
                case MINUS:
                    *out_value = left - right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_MULOP:
        {
            double left, right;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.mulop_data.left_term, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_real_expr_mode(symtab, expr->expr_data.mulop_data.right_factor, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.mulop_data.mulop_type)
            {
                case STAR:
                    *out_value = left * right;
                    return 0;
                case SLASH:
                    /* Allow 0.0/0.0 to produce NaN, and x/0.0 to produce Inf/-Inf
                     * This is required for FPC's TSingleHelper.NaN/TDoubleHelper.NaN etc */
                    *out_value = left / right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_FUNCTION_CALL:
        {
            const char *id = expr->expr_data.function_call_data.id;
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            if (id != NULL && is_real_type_name(symtab, id))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                if (arg == NULL)
                    return 1;
                return const_fold_real_expr_mode(symtab, arg, out_value, emit_diagnostics);
            }
            if (id != NULL && pascal_identifier_equals(id, "Ln"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Ln");
                if (arg == NULL)
                    return 1;
                double value = 0.0;
                if (const_fold_real_expr_mode(symtab, arg, &value, emit_diagnostics) != 0)
                    return 1;
                if (value <= 0.0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Ln argument must be > 0 in const expression.\n");
                    return 1;
                }
                *out_value = log(value);
                return 0;
            }
            break;
        }
        default:
            break;
    }

    if (emit_diagnostics)
        fprintf(stderr, "Error: unsupported real const expression.\n");
    return 1;
}

/* Helper function to resolve a type name to its base primitive type name.
 * This follows type aliases until we reach a known primitive type.
 * Returns the resolved type name (caller should not free), or NULL if unknown.
 */
/* Track recursion depth to prevent stack overflow from circular type definitions.
 * Note: This compiler is single-threaded, so static variable is safe. */
static int resolve_type_depth = 0;
#define MAX_RESOLVE_TYPE_DEPTH 100

static const char *resolve_type_to_base_name(SymTab_t *symtab,
    const QualifiedIdent *type_id,
    const char *type_name)
{
    const char *input_name = NULL;
    if (type_id != NULL)
        input_name = qualified_ident_last(type_id);
    if (input_name == NULL)
        input_name = type_name;
    if (input_name == NULL)
        return NULL;
    
    /* Prevent infinite recursion from circular type definitions */
    if (resolve_type_depth >= MAX_RESOLVE_TYPE_DEPTH)
    {
        KGPC_SEMCHECK_HARD_ASSERT(0,
            "type resolution depth limit reached for '%s' (possible circular definition)",
            input_name);
        return NULL;
    }
    
    /* First, check if it's already a known primitive type */
    if (pascal_identifier_equals(input_name, "Int64") ||
        pascal_identifier_equals(input_name, "QWord") ||
        pascal_identifier_equals(input_name, "UInt64") ||
        pascal_identifier_equals(input_name, "LongInt") ||
        pascal_identifier_equals(input_name, "Integer") ||
        pascal_identifier_equals(input_name, "Cardinal") ||
        pascal_identifier_equals(input_name, "LongWord") ||
        pascal_identifier_equals(input_name, "DWord") ||
        pascal_identifier_equals(input_name, "SmallInt") ||
        pascal_identifier_equals(input_name, "Word") ||
        pascal_identifier_equals(input_name, "ShortInt") ||
        pascal_identifier_equals(input_name, "Byte") ||
        pascal_identifier_equals(input_name, "Boolean") ||
        pascal_identifier_equals(input_name, "Char") ||
        pascal_identifier_equals(input_name, "AnsiChar") ||
        pascal_identifier_equals(input_name, "WideChar") ||
        pascal_identifier_equals(input_name, "Pointer") ||
        pascal_identifier_equals(input_name, "PChar") ||
        pascal_identifier_equals(input_name, "Double") ||
        pascal_identifier_equals(input_name, "Real") ||
        pascal_identifier_equals(input_name, "Single"))
    {
        return input_name;
    }
    
    /* Try to look up as a type alias in the symbol table */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        const char *lookup_name = type_name;
        char *qualified_name = NULL;

        /* Prefer unit-defined types when we have a structured unit qualification. */
        if (type_id != NULL && type_id->count > 1)
        {
            const char *unit_name = type_id->segments != NULL ? type_id->segments[0] : NULL;
            const char *base_name = qualified_ident_last(type_id);
            if (unit_name != NULL && base_name != NULL && semcheck_is_unit_name(unit_name))
            {
                type_node = semcheck_find_type_node_with_unit_flag(symtab, base_name, 1);
                if (type_node != NULL && type_node->hash_type != HASHTYPE_TYPE)
                    type_node = NULL;
            }
        }

        if (type_id != NULL && type_node == NULL)
        {
            qualified_name = qualified_ident_join(type_id, ".");
            if (qualified_name != NULL)
                lookup_name = qualified_name;
        }

        /* Resolve with preferred-type lookup (unit/scope-aware + nested suffix fallback). */
        if (type_node == NULL)
            type_node = semcheck_find_preferred_type_node(symtab, lookup_name);

        if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
        {
            fprintf(stderr, "[resolve_type] '%s' initial lookup node=%p (%s)\n",
                input_name, (void*)type_node, lookup_name != NULL ? lookup_name : "<null>");
        }
        
        /* If lookup failed and this is a qualified name, try the unqualified part */
        if (type_node == NULL)
        {
            const char *last = (type_id != NULL) ? qualified_ident_last(type_id) : NULL;
            if (last != NULL)
            {
                lookup_name = last;
                type_node = semcheck_find_preferred_type_node(symtab, lookup_name);
                if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
                {
                    fprintf(stderr, "[resolve_type] '%s' unqualified '%s' node=%p\n",
                        input_name, lookup_name, (void*)type_node);
                }
            }
        }
        if (qualified_name != NULL)
            free(qualified_name);

        if (type_node != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
            {
                fprintf(stderr, "[resolve_type] '%s' hash_type=%d type=%p\n",
                    input_name, type_node->hash_type, (void*)type_node->type);
            }
            if (type_node->hash_type == HASHTYPE_TYPE && type_node->type != NULL)
            {
                /* Get the underlying type from KgpcType */
                KgpcType *kgpc_type = type_node->type;
                
                /* CRITICAL: Check TypeAlias FIRST for target_type_id before checking primitive tag.
                 * This preserves the original type name for small integer types (Byte, Word, etc.)
                 * which are all mapped to LONGINT_TYPE by the predeclare_types function but need
                 * to retain their original type semantics for SizeOf/High/Low operations. */
                struct TypeAlias *alias = kgpc_type_get_type_alias(kgpc_type);
                if (alias != NULL && alias->target_type_id != NULL && type_id != NULL)
                {
                    char *qualified = qualified_ident_join(type_id, ".");
                    if (qualified != NULL &&
                        pascal_identifier_equals(alias->target_type_id, qualified))
                    {
                        HashNode_t *alt = semcheck_find_type_excluding_alias(symtab, qualified, alias);
                        if (alt != NULL && alt->type != NULL)
                        {
                            type_node = alt;
                            kgpc_type = alt->type;
                            alias = kgpc_type_get_type_alias(kgpc_type);
                        }
                    }
                    free(qualified);
                }
                if (kgpc_getenv("KGPC_DEBUG_RESOLVE_TYPE") != NULL)
                {
                    fprintf(stderr, "[resolve_type] '%s' kgpc_type kind=%d alias=%p\n",
                        input_name, kgpc_type->kind, (void*)alias);
                    if (alias != NULL)
                    {
                        fprintf(stderr, "[resolve_type] '%s' alias target_type_id='%s' base_type=%d\n",
                            input_name, alias->target_type_id ? alias->target_type_id : "<null>",
                            alias->base_type);
                    }
                }
                if (alias != NULL)
                {
                    int allow_recursion = 1;
                    if (alias->target_type_ref != NULL &&
                        alias->target_type_ref->name != NULL &&
                        type_id != NULL &&
                        qualified_ident_equals_ci(alias->target_type_ref->name, type_id))
                    {
                        allow_recursion = 0;
                    }
                    if (alias->target_type_id != NULL &&
                        pascal_identifier_equals(alias->target_type_id, input_name))
                    {
                        allow_recursion = 0;
                    }
                    if (alias->target_type_id != NULL &&
                        type_name != NULL &&
                        pascal_identifier_equals(alias->target_type_id, type_name))
                    {
                        allow_recursion = 0;
                    }

                    if (!allow_recursion && type_id != NULL)
                    {
                        const char *base_name = qualified_ident_last(type_id);
                        HashNode_t *alt = find_type_node_with_range_metadata(symtab, base_name, alias);
                        if (alt != NULL && alt->type != NULL)
                        {
                            kgpc_type = alt->type;
                            alias = kgpc_type_get_type_alias(kgpc_type);
                        }
                    }

                    /* Recursively resolve via target_type_id if available.
                     * This will eventually reach a builtin type like "Word", "Byte", etc.
                     * which are handled by the builtin check at the top of this function. */
                    if (allow_recursion &&
                        (alias->target_type_ref != NULL || alias->target_type_id != NULL))
                    {
                        resolve_type_depth++;
                        const char *result = resolve_type_to_base_name(symtab,
                            alias->target_type_ref != NULL ? alias->target_type_ref->name : NULL,
                            alias->target_type_id);
                        resolve_type_depth--;
                        return result;
                    }
                    /* Check for base_type tag if no target_type_id */
                    if (alias->base_type != UNKNOWN_TYPE && alias->base_type != 0)
                    {
                        switch (alias->base_type)
                        {
                            case INT_TYPE: return "Integer";
                            case LONGINT_TYPE: return "LongInt";
                            case INT64_TYPE: return "Int64";
                            case BOOL: return "Boolean";
                            case CHAR_TYPE: return "Char";
                            case REAL_TYPE: return "Real";
                            case EXTENDED_TYPE: return "Extended";
                            case POINTER_TYPE: return "Pointer";
                            default: break;
                        }
                    }
                }
                
                /* Fallback: Check primitive type tag if no alias info */
                if (kgpc_type->kind == TYPE_KIND_PRIMITIVE)
                {
                    switch (kgpc_type->info.primitive_type_tag)
                    {
                        case INT_TYPE: return "Integer";
                        case LONGINT_TYPE: return "LongInt";
                        case INT64_TYPE: return "Int64";
                        case BOOL: return "Boolean";
                        case CHAR_TYPE: return "Char";
                        case REAL_TYPE: return "Real";
                        case EXTENDED_TYPE: return "Extended";
                        case POINTER_TYPE: return "Pointer";
                        default: break;
                    }
                }
            }
        }
    }
    
    return NULL; /* Unknown type */
}

static int resolve_range_bounds_for_type_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    long long *out_low, long long *out_high);

static int resolve_range_bounds_for_type(SymTab_t *symtab, const char *type_name,
    long long *out_low, long long *out_high)
{
    if (symtab == NULL || type_name == NULL || out_low == NULL || out_high == NULL)
        return 0;

    HashNode_t *type_node = semcheck_find_preferred_type_node(symtab, type_name);
    if (type_node == NULL)
        return 0;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL)
    {
        if (alias->is_enum && alias->enum_literals != NULL &&
            !alias->enum_has_explicit_values)
        {
            int count = ListLength(alias->enum_literals);
            if (count > 0)
            {
                *out_low = 0;
                *out_high = (long long)count - 1;
                return 1;
            }
        }

        if (alias->range_known)
        {
            *out_low = alias->range_start;
            *out_high = alias->range_end;
            return 1;
        }

        if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
            resolve_range_bounds_for_type_ref(symtab, alias->target_type_ref->name,
                out_low, out_high))
        {
            return 1;
        }

        if (alias->target_type_id != NULL &&
            !pascal_identifier_equals(alias->target_type_id, type_name) &&
            resolve_range_bounds_for_type(symtab, alias->target_type_id, out_low, out_high))
        {
            return 1;
        }
    }

    const char *base_name = semcheck_base_type_name(type_name);
    HashNode_t *alt = find_type_node_with_range_metadata(symtab, base_name, alias);
    if (alt != NULL)
    {
        struct TypeAlias *alt_alias = get_type_alias_from_node(alt);
        if (alt_alias != NULL && alt_alias->range_known)
        {
            *out_low = alt_alias->range_start;
            *out_high = alt_alias->range_end;
            return 1;
        }
    }

    return 0;
}

static HashNode_t *find_type_node_with_range_metadata(SymTab_t *symtab,
    const char *base_name, const struct TypeAlias *exclude_alias)
{
    if (symtab == NULL || base_name == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, base_name);
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE && node->type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias == NULL || alias == exclude_alias)
            {
                cur = cur->next;
                continue;
            }
            if (alias->is_range && alias->range_known)
            {
                best = node;
                break;
            }
            if (best == NULL && alias->base_type != UNKNOWN_TYPE)
                best = node;
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

static HashNode_t *semcheck_find_exact_type_node_for_qid(SymTab_t *symtab,
    const QualifiedIdent *type_ref)
{
    if (symtab == NULL || type_ref == NULL)
        return NULL;

    HashNode_t *type_node = NULL;
    char *qualified = qualified_ident_join(type_ref, ".");
    if (qualified != NULL)
    {
        if (FindIdent(&type_node, symtab, qualified) >= 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            free(qualified);
            return type_node;
        }
        type_node = semcheck_find_preferred_type_node(symtab, qualified);
        if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
        {
            free(qualified);
            return type_node;
        }
        free(qualified);
    }

    if (type_ref->count > 1 && type_ref->segments != NULL && type_ref->segments[0] != NULL)
    {
        const char *unit_name = type_ref->segments[0];
        const char *base_name = qualified_ident_last(type_ref);
        if (base_name != NULL)
        {
            ListNode_t *matches = FindAllIdents(symtab, base_name);
            for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
            {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
                    continue;
                const char *candidate_unit = unit_registry_get(candidate->source_unit_index);
                if (candidate_unit != NULL &&
                    pascal_identifier_equals(candidate_unit, unit_name))
                {
                    type_node = candidate;
                    if (candidate->defined_in_unit)
                        break;
                }
            }
            if (matches != NULL)
                DestroyList(matches);
            if (type_node != NULL)
                return type_node;
        }
    }

    TypeRef temp_ref = {0};
    temp_ref.name = (QualifiedIdent *)type_ref;
    return semcheck_find_preferred_type_node_with_ref(symtab, &temp_ref, NULL);
}

static int resolve_range_bounds_for_type_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    long long *out_low, long long *out_high)
{
    if (symtab == NULL || type_ref == NULL || out_low == NULL || out_high == NULL)
        return 0;

    HashNode_t *type_node = semcheck_find_exact_type_node_for_qid(symtab, type_ref);
    if (type_node == NULL)
        return 0;
    if (type_node->hash_type != HASHTYPE_TYPE)
        return 0;

    struct TypeAlias *alias = get_type_alias_from_node(type_node);
    if (alias != NULL && alias->is_enum && alias->enum_literals != NULL &&
        !alias->enum_has_explicit_values)
    {
        int count = ListLength(alias->enum_literals);
        if (count > 0)
        {
            *out_low = 0;
            *out_high = (long long)count - 1;
            return 1;
        }
    }
    if (alias != NULL && alias->target_type_id != NULL)
    {
        char *qualified = qualified_ident_join(type_ref, ".");
        if (qualified != NULL &&
            pascal_identifier_equals(alias->target_type_id, qualified))
        {
            HashNode_t *alt = semcheck_find_type_excluding_alias(symtab, qualified, alias);
            if (alt != NULL && alt->hash_type == HASHTYPE_TYPE)
            {
                type_node = alt;
                alias = get_type_alias_from_node(type_node);
            }
        }
        free(qualified);
    }
    if (alias == NULL || !alias->range_known)
    {
        const char *base_name = qualified_ident_last(type_ref);
        HashNode_t *alt = find_type_node_with_range_metadata(symtab, base_name, alias);
        if (alt != NULL)
            alias = get_type_alias_from_node(alt);
    }
    if (alias != NULL && alias->range_known)
    {
        *out_low = alias->range_start;
        *out_high = alias->range_end;
        return 1;
    }

    if (alias != NULL)
    {
        if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
            !qualified_ident_equals_ci(alias->target_type_ref->name, type_ref) &&
            resolve_range_bounds_for_type_ref(symtab, alias->target_type_ref->name,
                out_low, out_high))
        {
            return 1;
        }

        if (alias->target_type_id != NULL)
        {
            char *qualified = qualified_ident_join(type_ref, ".");
            int same_target = (qualified != NULL &&
                pascal_identifier_equals(alias->target_type_id, qualified));
            free(qualified);
            if (!same_target &&
                resolve_range_bounds_for_type(symtab, alias->target_type_id,
                    out_low, out_high))
            {
                return 1;
            }
        }
    }

    return 0;
}

static int evaluate_const_expr_ordinal_bounds(SymTab_t *symtab, struct Expression *expr,
    long long *out_low, long long *out_high)
{
    if (expr == NULL || out_low == NULL || out_high == NULL)
        return 0;

    if (expr->resolved_kgpc_type != NULL)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(expr->resolved_kgpc_type);
        if (alias != NULL)
        {
            if (alias->is_enum && alias->enum_literals != NULL &&
                !alias->enum_has_explicit_values)
            {
                int count = ListLength(alias->enum_literals);
                if (count > 0)
                {
                    *out_low = 0;
                    *out_high = (long long)count - 1;
                    return 1;
                }
            }
            if (alias->range_known)
            {
                *out_low = alias->range_start;
                *out_high = alias->range_end;
                return 1;
            }
            if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL &&
                resolve_range_bounds_for_type_ref(symtab, alias->target_type_ref->name,
                    out_low, out_high))
            {
                return 1;
            }
            if (alias->target_type_id != NULL &&
                resolve_range_bounds_for_type(symtab, alias->target_type_id, out_low, out_high))
            {
                return 1;
            }
            if (alias->alias_name != NULL &&
                get_builtin_type_bounds(alias->alias_name, out_low, out_high))
            {
                return 1;
            }
            if (alias->target_type_id != NULL &&
                get_builtin_type_bounds(alias->target_type_id, out_low, out_high))
            {
                return 1;
            }
        }

        if (expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE)
        {
            switch (expr->resolved_kgpc_type->info.primitive_type_tag)
            {
                case BYTE_TYPE: *out_low = 0; *out_high = 255; return 1;
                case WORD_TYPE: *out_low = 0; *out_high = 65535; return 1;
                case LONGWORD_TYPE: *out_low = 0; *out_high = 4294967295LL; return 1;
                case QWORD_TYPE: *out_low = 0; *out_high = 9223372036854775807LL; return 1;
                case INT_TYPE:
                case LONGINT_TYPE: *out_low = -2147483648LL; *out_high = 2147483647LL; return 1;
                case INT64_TYPE: *out_low = (-9223372036854775807LL - 1); *out_high = 9223372036854775807LL; return 1;
                case BOOL: *out_low = 0; *out_high = 1; return 1;
                case CHAR_TYPE: *out_low = 0; *out_high = 255; return 1;
                default: break;
            }
        }
    }

    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL &&
        resolve_range_bounds_for_type(symtab, expr->expr_data.id, out_low, out_high))
    {
        return 1;
    }

    if (expr->type == EXPR_RECORD_ACCESS)
    {
        QualifiedIdent *qid = build_qualified_ident_from_expr(expr);
        if (qid != NULL)
        {
            int resolved = resolve_range_bounds_for_type_ref(symtab, qid, out_low, out_high);
            qualified_ident_free(qid);
            if (resolved)
                return 1;
        }
    }

    return 0;
}

static int const_typecast_target_is_ordinal(SymTab_t *symtab, struct Expression *expr,
    long long *out_low, long long *out_high)
{
    const TypeRef *target_ref = NULL;
    const char *target_id = NULL;

    if (expr == NULL || out_low == NULL || out_high == NULL)
        return 0;

    target_ref = expr->expr_data.typecast_data.target_type_ref;
    target_id = expr->expr_data.typecast_data.target_type_id;

    if (target_ref != NULL && target_ref->name != NULL &&
        resolve_range_bounds_for_type_ref(symtab, target_ref->name, out_low, out_high))
    {
        return 1;
    }
    if (target_id != NULL &&
        resolve_range_bounds_for_type(symtab, target_id, out_low, out_high))
    {
        return 1;
    }
    if (target_id != NULL &&
        get_builtin_type_bounds(semcheck_base_type_name(target_id), out_low, out_high))
    {
        return 1;
    }
    return 0;
}

static int const_fold_int_expr_mode(SymTab_t *symtab, struct Expression *expr, long long *out_value,
    int emit_diagnostics)
{
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_RNUM:
            /* Real numbers in integer context - truncate or error */
            if (emit_diagnostics)
                fprintf(stderr, "Error: real number constant in integer context.\n");
            return 1;
        case EXPR_INUM:
            *out_value = expr->expr_data.i_num;
            return 0;
        case EXPR_BOOL:
            *out_value = expr->expr_data.bool_value ? 1 : 0;
            return 0;
        case EXPR_NIL:
            /* nil is represented as 0 in pointer context */
            *out_value = 0;
            return 0;
        case EXPR_STRING:
            /* Handle character literals in const expressions */
            if (expr->expr_data.string != NULL && 
                expr->expr_data.string[0] != '\0' && 
                expr->expr_data.string[1] == '\0')
            {
                /* Single character literal - return its ASCII value */
                *out_value = (unsigned char)expr->expr_data.string[0];
                return 0;
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: string literal in const expression must be a single character.\n");
            return 1;
        case EXPR_CHAR_CODE:
            *out_value = (unsigned char)(expr->expr_data.char_code & 0xFF);
            return 0;
        case EXPR_VAR_ID:
        {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, expr->expr_data.id) >= 0 && node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                if (node->const_string_value != NULL)
                {
                    if (node->const_string_value[0] == '\0')
                    {
                        *out_value = 0;
                        return 0;
                    }
                    if (node->const_string_value[1] == '\0')
                    {
                        *out_value = (unsigned char)node->const_string_value[0];
                        return 0;
                    }
                }
                *out_value = node->const_int_value;
                return 0;
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: constant %s is undefined or not a const.\n", expr->expr_data.id);
            return 1;
        }
        case EXPR_SIGN_TERM:
        {
            long long value;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.sign_term, &value,
                    emit_diagnostics) != 0)
                return 1;
            *out_value = -value;
            return 0;
        }
        case EXPR_TYPECAST:
        {
            long long inner_value = 0;
            long long ordinal_low = 0;
            long long ordinal_high = 0;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.typecast_data.expr, &inner_value,
                    emit_diagnostics) != 0)
            {
                double real_value = 0.0;
                if (const_fold_real_expr_mode(symtab, expr->expr_data.typecast_data.expr, &real_value,
                        emit_diagnostics) != 0)
                    return 1;
                inner_value = (long long)real_value;
            }

            int target_type = expr->expr_data.typecast_data.target_type;
            const char *id = NULL;
            if (expr->expr_data.typecast_data.target_type_id != NULL)
                id = semcheck_base_type_name(expr->expr_data.typecast_data.target_type_id);
            if (kgpc_getenv("KGPC_DEBUG_CONST_CAST") != NULL)
            {
                fprintf(stderr, "[KGPC] const cast id=%s target_type=%d inner=%lld\n",
                    id != NULL ? id : "<null>", target_type, inner_value);
            }
            if (target_type == UNKNOWN_TYPE &&
                expr->expr_data.typecast_data.target_type_id != NULL)
            {
                if (id == NULL)
                    break;
                if (strcasecmp(id, "Byte") == 0 || strcasecmp(id, "Word") == 0 ||
                    strcasecmp(id, "Integer") == 0 || strcasecmp(id, "ShortInt") == 0 ||
                    strcasecmp(id, "SmallInt") == 0)
                {
                    if (strcasecmp(id, "Byte") == 0)
                        target_type = BYTE_TYPE;
                    else if (strcasecmp(id, "Word") == 0)
                        target_type = WORD_TYPE;
                    else
                        target_type = INT_TYPE;
                }
                else if (strcasecmp(id, "LongInt") == 0)
                    target_type = LONGINT_TYPE;
                else if (strcasecmp(id, "Cardinal") == 0 || strcasecmp(id, "LongWord") == 0 ||
                         strcasecmp(id, "DWord") == 0)
                    target_type = LONGWORD_TYPE;
                else if (strcasecmp(id, "Int64") == 0 || strcasecmp(id, "UInt64") == 0 ||
                         strcasecmp(id, "QWord") == 0 ||
                         strcasecmp(id, "SizeInt") == 0 || strcasecmp(id, "SizeUInt") == 0 ||
                         strcasecmp(id, "NativeInt") == 0 || strcasecmp(id, "NativeUInt") == 0)
                {
                    if (strcasecmp(id, "QWord") == 0 || strcasecmp(id, "UInt64") == 0)
                        target_type = QWORD_TYPE;
                    else
                        target_type = INT64_TYPE;
                }
                else if (strcasecmp(id, "Pointer") == 0)
                    target_type = POINTER_TYPE;
                else if (strcasecmp(id, "Char") == 0)
                    target_type = CHAR_TYPE;
                else if (strcasecmp(id, "Boolean") == 0)
                    target_type = BOOL;
            }

            switch (target_type)
            {
                case CHAR_TYPE:
                {
                    int is_wide_char = 0;
                    if (id != NULL &&
                        (strcasecmp(id, "WideChar") == 0 ||
                         strcasecmp(id, "UnicodeChar") == 0))
                    {
                        is_wide_char = 1;
                    }
                    long long max_char_value = is_wide_char ? 65535LL : 255LL;
                    if (inner_value < 0 || inner_value > max_char_value)
                    {
                            if (emit_diagnostics)
                                fprintf(stderr, "Error: typecast value %lld out of range for %s.\n",
                            inner_value, is_wide_char ? "WideChar" : "Char");
                        return 1;
                    }
                    if (is_wide_char)
                        *out_value = (uint16_t)inner_value;
                    else
                        *out_value = (unsigned char)inner_value;
                    return 0;
                }
                case BOOL:
                    *out_value = (inner_value != 0);
                    return 0;
                case ENUM_TYPE:
                    if (const_typecast_target_is_ordinal(symtab, expr, &ordinal_low, &ordinal_high))
                    {
                        if (inner_value < ordinal_low || inner_value > ordinal_high)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr,
                                "Error: typecast value %lld out of range for ordinal target.\n",
                                inner_value);
                            return 1;
                        }
                    }
                    *out_value = inner_value;
                    return 0;
                case INT_TYPE:
                case LONGINT_TYPE:
                    if (id != NULL)
                    {
                        if (strcasecmp(id, "ShortInt") == 0)
                        {
                            *out_value = (int8_t)inner_value;
                            return 0;
                        }
                        if (strcasecmp(id, "SmallInt") == 0)
                        {
                            *out_value = (int16_t)inner_value;
                            return 0;
                        }
                    }
                    *out_value = (int32_t)inner_value;
                    return 0;
                case BYTE_TYPE:
                    *out_value = (uint8_t)inner_value;
                    return 0;
                case WORD_TYPE:
                    *out_value = (uint16_t)inner_value;
                    return 0;
                case LONGWORD_TYPE:
                    *out_value = (uint32_t)inner_value;
                    return 0;
                case QWORD_TYPE:
                    *out_value = (uint64_t)inner_value;
                    return 0;
                case INT64_TYPE:
                    if (id != NULL &&
                        (strcasecmp(id, "QWord") == 0 || strcasecmp(id, "UInt64") == 0 ||
                         strcasecmp(id, "SizeUInt") == 0 || strcasecmp(id, "NativeUInt") == 0 ||
                         strcasecmp(id, "PtrUInt") == 0))
                    {
                        *out_value = (uint64_t)inner_value;
                        return 0;
                    }
                    *out_value = inner_value;
                    return 0;
                case POINTER_TYPE:
                    *out_value = inner_value;
                    return 0;
                case UNKNOWN_TYPE:
                    if (const_typecast_target_is_ordinal(symtab, expr, &ordinal_low, &ordinal_high))
                    {
                        if (inner_value < ordinal_low || inner_value > ordinal_high)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr,
                                "Error: typecast value %lld out of range for ordinal target.\n",
                                inner_value);
                            return 1;
                        }
                    }
                    /* Treat unresolved ordinal aliases as integer-like passthrough */
                    *out_value = inner_value;
                    return 0;
                default:
                    if (const_typecast_target_is_ordinal(symtab, expr, &ordinal_low, &ordinal_high))
                    {
                        if (inner_value < ordinal_low || inner_value > ordinal_high)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr,
                                "Error: typecast value %lld out of range for ordinal target.\n",
                                inner_value);
                            return 1;
                        }
                        *out_value = inner_value;
                        return 0;
                    }
            if (emit_diagnostics)
                fprintf(stderr, "Error: unsupported const typecast target.\n");
            return 1;
            }
        }
        case EXPR_ADDOP:
        {
            long long left, right;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.addop_data.left_expr, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.addop_data.right_term, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.addop_data.addop_type)
            {
                case PLUS:
                    *out_value = left + right;
                    return 0;
                case MINUS:
                    *out_value = left - right;
                    return 0;
                case OR:
                    /* Bitwise OR for const expressions (FPC bootstrap: S_IRWXU = S_IRUSR or S_IWUSR) */
                    *out_value = left | right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_MULOP:
        {
            long long left, right;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.mulop_data.left_term, &left,
                    emit_diagnostics) != 0)
                return 1;
            if (const_fold_int_expr_mode(symtab, expr->expr_data.mulop_data.right_factor, &right,
                    emit_diagnostics) != 0)
                return 1;
            switch (expr->expr_data.mulop_data.mulop_type)
            {
                case STAR:
                    *out_value = left * right;
                    return 0;
                case DIV:
                    if (right == 0)
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: division by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left / right;
                    return 0;
                case MOD:
                    if (right == 0)
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: modulo by zero in const expression.\n");
                        return 1;
                    }
                    *out_value = left % right;
                    return 0;
                case SHL:
                    /* Shift left: left shl right */
                    *out_value = left << right;
                    return 0;
                case SHR:
                    /* Shift right: left shr right */
                    *out_value = (unsigned long long)left >> right;
                    return 0;
                case AND:
                    /* Bitwise AND for const expressions (FPC bootstrap) */
                    *out_value = left & right;
                    return 0;
                case XOR:
                    /* Bitwise XOR for const expressions (FPC bootstrap) */
                    *out_value = left ^ right;
                    return 0;
                default:
                    break;
            }
            break;
        }
        case EXPR_SET:
        {
            unsigned char bytes[32];
            size_t used = 0;
            long long mask = 0;
            if (evaluate_set_const_bytes(symtab, expr, bytes, sizeof(bytes), &used, &mask, NULL) != 0)
                return 1;
            if (used > 8)
            {
                if (emit_diagnostics)
                    fprintf(stderr, "Error: set literal requires 32-byte storage and cannot be reduced to integer const.\n");
                return 1;
            }
            *out_value = mask;
            return 0;
        }
        case EXPR_RECORD_ACCESS:
        {
            /* Handle qualified constants like UnitName.ConstName */
            /* The field_id is the constant name, look it up directly in the symbol table */
            /* since unit exports are already imported into the current scope */
            char *field_id = expr->expr_data.record_access_data.field_id;
            if (field_id != NULL)
            {
                HashNode_t *node = NULL;
                if (FindIdent(&node, symtab, field_id) >= 0 && node != NULL &&
                    (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                {
                    *out_value = node->const_int_value;
                    return 0;
                }
                struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
                if (record_expr != NULL)
                {
                    char *owner_name = build_qualified_identifier_from_expr(record_expr);
                    if (owner_name != NULL)
                    {
                        long long enum_value = 0;
                        if (semcheck_resolve_scoped_enum_literal(symtab, owner_name,
                            field_id, &enum_value))
                        {
                            *out_value = enum_value;
                            free(owner_name);
                            return 0;
                        }
                    }

                    if (owner_name != NULL)
                    {
                        size_t qualified_len = strlen(owner_name) + 1 + strlen(field_id) + 1;
                        char *qualified = (char *)malloc(qualified_len);
                        if (qualified != NULL)
                        {
                            snprintf(qualified, qualified_len, "%s.%s", owner_name, field_id);
                            if (FindIdent(&node, symtab, qualified) >= 0 &&
                                node != NULL &&
                                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                            {
                                *out_value = node->const_int_value;
                                free(qualified);
                                free(owner_name);
                                return 0;
                            }
                            free(qualified);
                        }
                        free(owner_name);
                        owner_name = NULL;
                    }

                    long long enum_value = 0;
                    if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL &&
                        semcheck_resolve_scoped_enum_literal(symtab, record_expr->expr_data.id,
                            field_id, &enum_value))
                    {
                        *out_value = enum_value;
                        return 0;
                    }
                }
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: qualified constant '%s' is undefined or not a const.\n",
                        field_id ? field_id : "(null)");
            return 1;
        }
        case EXPR_FUNCTION_CALL:
        {
            /* Handle Ord() function for constant expressions */
            char *id = expr->expr_data.function_call_data.id;
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;

            if (id != NULL &&
                (pascal_identifier_equals(id, "Succ") || pascal_identifier_equals(id, "Pred")))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                long long arg_value = 0;
                long long low = 0;
                long long high = 0;
                long long result = 0;
                int is_succ = pascal_identifier_equals(id, "Succ");

                if (arg == NULL)
                    return 1;
                if (const_fold_int_expr_mode(symtab, arg, &arg_value, emit_diagnostics) != 0)
                    return 1;
                if (!evaluate_const_expr_ordinal_bounds(symtab, arg, &low, &high))
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s expects an ordinal constant argument.\n", id);
                    return 1;
                }

                result = is_succ ? (arg_value + 1) : (arg_value - 1);
                if (result < low || result > high)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s result out of range for ordinal type.\n", id);
                    return 1;
                }

                *out_value = result;
                return 0;
            }

            if (id != NULL && args != NULL && args->next == NULL)
            {
                HashNode_t *type_node = NULL;
                int found_type = (FindIdent(&type_node, symtab, id) >= 0 &&
                    type_node != NULL && type_node->hash_type == HASHTYPE_TYPE);
                const char *base_id = semcheck_base_type_name(id);
                if (!found_type && base_id != NULL && base_id != id)
                {
                    found_type = (FindIdent(&type_node, symtab, base_id) >= 0 &&
                        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE);
                }

                if (found_type || semcheck_map_builtin_type_name_local(id) != UNKNOWN_TYPE)
                {
                    struct Expression *arg = (struct Expression *)args->cur;
                    long long inner_value = 0;
                    if (const_fold_int_expr_mode(symtab, arg, &inner_value, emit_diagnostics) != 0)
                    {
                        double real_value = 0.0;
                        if (const_fold_real_expr_mode(symtab, arg, &real_value,
                                emit_diagnostics) != 0)
                            return 1;
                        inner_value = (long long)real_value;
                    }
                    *out_value = inner_value;
                    return 0;
                }
            }
            
            if (id != NULL && pascal_identifier_equals(id, "Ord"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Ord");
                if (arg == NULL)
                    return 1;
                
                /* Handle character literal */
                if (arg->type == EXPR_STRING)
                {
                    char *literal = arg->expr_data.string;
                    if (literal == NULL || literal[0] == '\0')
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: Ord expects a non-empty character literal.\n");
                        return 1;
                    }
                    if (literal[1] != '\0')
                    {
                        if (emit_diagnostics)
                            fprintf(stderr, "Error: Ord expects a single character literal.\n");
                        return 1;
                    }
                    *out_value = (unsigned char)literal[0];
                    return 0;
                }
                /* Handle character code literal */
                else if (arg->type == EXPR_CHAR_CODE)
                {
                    *out_value = (unsigned char)arg->expr_data.char_code;
                    return 0;
                }
                /* Handle boolean literal */
                else if (arg->type == EXPR_BOOL)
                {
                    *out_value = arg->expr_data.bool_value ? 1 : 0;
                    return 0;
                }
                /* Handle integer literal */
                else if (arg->type == EXPR_INUM)
                {
                    *out_value = arg->expr_data.i_num;
                    return 0;
                }
                /* Handle const variable reference */
                else if (arg->type == EXPR_VAR_ID)
                {
                    HashNode_t *node = NULL;
                    int found_scope = FindIdent(&node, symtab, arg->expr_data.id);
                    if (found_scope >= 0 && 
                        node != NULL &&
                        (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                    {
                        if (node->const_string_value != NULL &&
                            node->const_string_value[0] != '\0' &&
                            node->const_string_value[1] == '\0')
                        {
                            *out_value = (unsigned char)node->const_string_value[0];
                            return 0;
                        }
                        *out_value = node->const_int_value;
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Ord argument %s is not a constant.\n", arg->expr_data.id);
                    return 1;
                }
                /* Handle nested const expressions */
                else
                {
                    long long arg_value;
                    if (const_fold_int_expr_mode(symtab, arg, &arg_value, emit_diagnostics) == 0)
                    {
                        *out_value = arg_value;
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Ord argument is not a valid const expression.\n");
                    return 1;
                }
            }
            
            /* Handle High() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "High"))
            {
                struct Expression *arg = extract_single_const_arg(args, "High");
                if (arg == NULL)
                    return 1;
                
                /* High expects a type identifier */
                if (arg->type == EXPR_VAR_ID || arg->type == EXPR_RECORD_ACCESS)
                {
                    QualifiedIdent *type_id_ref = build_qualified_ident_from_expr(arg);
                    char *qualified_name = NULL;
                    const char *type_name = NULL;
                    int status = 1;
                    if (type_id_ref != NULL)
                        qualified_name = qualified_ident_join(type_id_ref, ".");
                    if (arg->type == EXPR_VAR_ID)
                        type_name = arg->expr_data.id;
                    if (type_name == NULL)
                        type_name = qualified_name;
                    const char *base_name = (type_id_ref != NULL) ?
                        qualified_ident_last(type_id_ref) :
                        semcheck_base_type_name(type_name);
                    const char *lookup_name = type_name;
                    long long range_low = 0;
                    long long range_high = 0;
                    if ((type_id_ref != NULL &&
                         resolve_range_bounds_for_type_ref(symtab, type_id_ref, &range_low, &range_high)) ||
                        (lookup_name != NULL &&
                         resolve_range_bounds_for_type(symtab, lookup_name, &range_low, &range_high)))
                    {
                        *out_value = range_high;
                        status = 0;
                        goto high_cleanup;
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, lookup_name);
                    if (resolved != NULL)
                        lookup_name = resolved;
                    
                    /* Map common type names to their High values */
                    {
                        long long bounds_low, bounds_high;
                        if (get_builtin_type_bounds(base_name, &bounds_low, &bounds_high))
                        {
                            *out_value = bounds_high;
                            status = 0;
                            goto high_cleanup;
                        }
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: High(%s) - unsupported type in const expression.\n",
                            qualified_name != NULL ? qualified_name : arg->expr_data.id);
                    status = 1;
high_cleanup:
                    if (qualified_name != NULL)
                        free(qualified_name);
                    qualified_ident_free(type_id_ref);
                    return status;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: High expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle Low() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Low"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Low");
                if (arg == NULL)
                    return 1;
                
                /* Low expects a type identifier */
                if (arg->type == EXPR_VAR_ID || arg->type == EXPR_RECORD_ACCESS)
                {
                    QualifiedIdent *type_id_ref = build_qualified_ident_from_expr(arg);
                    char *qualified_name = NULL;
                    const char *type_name = NULL;
                    int status = 1;
                    if (type_id_ref != NULL)
                        qualified_name = qualified_ident_join(type_id_ref, ".");
                    if (arg->type == EXPR_VAR_ID)
                        type_name = arg->expr_data.id;
                    if (type_name == NULL)
                        type_name = qualified_name;
                    const char *base_name = (type_id_ref != NULL) ?
                        qualified_ident_last(type_id_ref) :
                        semcheck_base_type_name(type_name);
                    const char *lookup_name = type_name;
                    long long range_low = 0;
                    long long range_high = 0;
                    if ((type_id_ref != NULL &&
                         resolve_range_bounds_for_type_ref(symtab, type_id_ref, &range_low, &range_high)) ||
                        (lookup_name != NULL &&
                         resolve_range_bounds_for_type(symtab, lookup_name, &range_low, &range_high)))
                    {
                        *out_value = range_low;
                        status = 0;
                        goto low_cleanup;
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, lookup_name);
                    if (resolved != NULL)
                        lookup_name = resolved;
                        
                    /* Map common type names to their Low values */
                    {
                        long long bounds_low, bounds_high;
                        if (get_builtin_type_bounds(base_name, &bounds_low, &bounds_high))
                        {
                            *out_value = bounds_low;
                            status = 0;
                            goto low_cleanup;
                        }
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Low(%s) - unsupported type in const expression.\n",
                            qualified_name != NULL ? qualified_name : arg->expr_data.id);
                    status = 1;
low_cleanup:
                    if (qualified_name != NULL)
                        free(qualified_name);
                    qualified_ident_free(type_id_ref);
                    return status;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: Low expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle SizeOf() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "SizeOf"))
            {
                struct Expression *arg = extract_single_const_arg(args, "SizeOf");
                if (arg == NULL)
                    return 1;
                
                /* SizeOf expects a type identifier */
                if (arg->type == EXPR_VAR_ID)
                {
                    QualifiedIdent *type_id_ref = NULL;
                    const char *type_name = arg->expr_data.id;
                    const char *base_name = type_name;
                    if (arg->id_ref != NULL)
                    {
                        type_id_ref = qualified_ident_clone(arg->id_ref);
                        base_name = qualified_ident_last(type_id_ref);
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, type_name);
                    if (resolved != NULL)
                        base_name = resolved;

                    /* Prefer computing from an actual type node (handles nested/local types). */
                    HashNode_t *size_type_node = NULL;
                    if (symtab != NULL)
                    {
                        if (type_id_ref != NULL && type_id_ref->count > 1)
                        {
                            char *qualified_name = qualified_ident_join(type_id_ref, ".");
                            if (qualified_name != NULL)
                            {
                                size_type_node = semcheck_find_preferred_type_node(symtab, qualified_name);
                                free(qualified_name);
                            }
                        }
                        if (size_type_node == NULL && type_name != NULL)
                            size_type_node = semcheck_find_preferred_type_node(symtab, type_name);
                    }
                    if (size_type_node != NULL && size_type_node->hash_type == HASHTYPE_TYPE)
                    {
                        long long computed_size = 0;
                        if (sizeof_from_hashnode(symtab, size_type_node, &computed_size, 0, expr->line_num) == 0)
                        {
                            *out_value = computed_size;
                            qualified_ident_free(type_id_ref);
                            return 0;
                        }
                    }
                        
                    /* Map common type names to their sizes (in bytes) */
                    /* 64-bit types */
                    if (pascal_identifier_equals(base_name, "Int64") ||
                        pascal_identifier_equals(base_name, "QWord") ||
                        pascal_identifier_equals(base_name, "UInt64") ||
                        pascal_identifier_equals(base_name, "Pointer") ||
                        pascal_identifier_equals(base_name, "PChar") ||
                        pascal_identifier_equals(base_name, "Double") ||
                        pascal_identifier_equals(base_name, "Real")) {
                        *out_value = 8LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 32-bit types */
                    if (pascal_identifier_equals(base_name, "LongInt") ||
                        pascal_identifier_equals(base_name, "LongWord") ||
                        pascal_identifier_equals(base_name, "Cardinal") ||
                        pascal_identifier_equals(base_name, "DWord") ||
                        pascal_identifier_equals(base_name, "Integer") ||
                        pascal_identifier_equals(base_name, "Single")) {
                        *out_value = 4LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 16-bit types */
                    if (pascal_identifier_equals(base_name, "SmallInt") ||
                        pascal_identifier_equals(base_name, "Word") ||
                        pascal_identifier_equals(base_name, "WideChar")) {
                        *out_value = 2LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 8-bit types */
                    if (pascal_identifier_equals(base_name, "ShortInt") ||
                        pascal_identifier_equals(base_name, "Byte") ||
                        pascal_identifier_equals(base_name, "Char") ||
                        pascal_identifier_equals(base_name, "AnsiChar") ||
                        pascal_identifier_equals(base_name, "Boolean")) {
                        *out_value = 1LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: SizeOf(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    qualified_ident_free(type_id_ref);
                    return 1;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: SizeOf expects a type identifier as argument.\n");
                return 1;
            }
            
            /* Handle BitSizeOf() function for constant expressions - returns SizeOf(T) * 8 */
            if (id != NULL && pascal_identifier_equals(id, "BitSizeOf"))
            {
                struct Expression *arg = extract_single_const_arg(args, "BitSizeOf");
                if (arg == NULL)
                    return 1;
                
                /* BitSizeOf expects a type identifier, same as SizeOf */
                if (arg->type == EXPR_VAR_ID)
                {
                    QualifiedIdent *type_id_ref = NULL;
                    const char *type_name = arg->expr_data.id;
                    const char *base_name = type_name;
                    if (arg->id_ref != NULL)
                    {
                        type_id_ref = qualified_ident_clone(arg->id_ref);
                        base_name = qualified_ident_last(type_id_ref);
                    }
                    /* Resolve type aliases to base type */
                    const char *resolved = resolve_type_to_base_name(symtab, type_id_ref, type_name);
                    if (resolved != NULL)
                        base_name = resolved;

                    /* Prefer computing from an actual type node (handles nested/local types). */
                    HashNode_t *size_type_node = NULL;
                    if (symtab != NULL)
                    {
                        if (type_id_ref != NULL && type_id_ref->count > 1)
                        {
                            char *qualified_name = qualified_ident_join(type_id_ref, ".");
                            if (qualified_name != NULL)
                            {
                                size_type_node = semcheck_find_preferred_type_node(symtab, qualified_name);
                                free(qualified_name);
                            }
                        }
                        if (size_type_node == NULL && type_name != NULL)
                            size_type_node = semcheck_find_preferred_type_node(symtab, type_name);
                    }
                    if (size_type_node != NULL && size_type_node->hash_type == HASHTYPE_TYPE)
                    {
                        long long computed_size = 0;
                        if (sizeof_from_hashnode(symtab, size_type_node, &computed_size, 0, expr->line_num) == 0)
                        {
                            *out_value = computed_size * 8;
                            qualified_ident_free(type_id_ref);
                            return 0;
                        }
                    }
                        
                    /* Map common type names to their sizes in bits */
                    /* 64-bit types */
                    if (pascal_identifier_equals(base_name, "Int64") ||
                        pascal_identifier_equals(base_name, "QWord") ||
                        pascal_identifier_equals(base_name, "UInt64") ||
                        pascal_identifier_equals(base_name, "Pointer") ||
                        pascal_identifier_equals(base_name, "PChar") ||
                        pascal_identifier_equals(base_name, "Double") ||
                        pascal_identifier_equals(base_name, "Real")) {
                        *out_value = 64LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 32-bit types */
                    if (pascal_identifier_equals(base_name, "LongInt") ||
                        pascal_identifier_equals(base_name, "LongWord") ||
                        pascal_identifier_equals(base_name, "Cardinal") ||
                        pascal_identifier_equals(base_name, "DWord") ||
                        pascal_identifier_equals(base_name, "Integer") ||
                        pascal_identifier_equals(base_name, "Single")) {
                        *out_value = 32LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 16-bit types */
                    if (pascal_identifier_equals(base_name, "SmallInt") ||
                        pascal_identifier_equals(base_name, "Word") ||
                        pascal_identifier_equals(base_name, "WideChar")) {
                        *out_value = 16LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    /* 8-bit types */
                    if (pascal_identifier_equals(base_name, "ShortInt") ||
                        pascal_identifier_equals(base_name, "Byte") ||
                        pascal_identifier_equals(base_name, "Char") ||
                        pascal_identifier_equals(base_name, "AnsiChar") ||
                        pascal_identifier_equals(base_name, "Boolean")) {
                        *out_value = 8LL;
                        qualified_ident_free(type_id_ref);
                        return 0;
                    }
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: BitSizeOf(%s) - unsupported type in const expression.\n", arg->expr_data.id);
                    qualified_ident_free(type_id_ref);
                    return 1;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: BitSizeOf expects a type identifier as argument.\n");
                return 1;
            }

            /* Handle Chr() function for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Chr"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Chr");
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long char_code;
                if (const_fold_int_expr_mode(symtab, arg, &char_code, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Chr argument must be a const expression.\n");
                    return 1;
                }
                
                /* Validate the character code is in valid range (0..255) */
                if (char_code < 0 || char_code > 255)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Chr argument %lld is out of valid range (0..255).\n", char_code);
                    return 1;
                }
                
                *out_value = char_code;
                return 0;
            }
            
            /* Handle Pointer() typecast for constant expressions (FPC bootstrap dl.pp) */
            if (id != NULL && pascal_identifier_equals(id, "Pointer"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Pointer");
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long ptr_value;
                if (const_fold_int_expr_mode(symtab, arg, &ptr_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: Pointer argument must be a const expression.\n");
                    return 1;
                }
                
                *out_value = ptr_value;
                return 0;
            }
            
            /* Handle PtrInt() and PtrUInt() typecasts for constant expressions */
            if (id != NULL && (pascal_identifier_equals(id, "PtrInt") || pascal_identifier_equals(id, "PtrUInt")))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long int_value;
                if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s argument must be a const expression.\n", id);
                    return 1;
                }
                
                *out_value = int_value;
                return 0;
            }

            /* Handle Trunc() for constant expressions */
            if (id != NULL && pascal_identifier_equals(id, "Trunc"))
            {
                struct Expression *arg = extract_single_const_arg(args, "Trunc");
                if (arg == NULL)
                    return 1;
                double real_value = 0.0;
                if (const_fold_real_expr_mode(symtab, arg, &real_value, emit_diagnostics) == 0)
                {
                    *out_value = (long long)real_value;
                    return 0;
                }
                long long int_value = 0;
                if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) == 0)
                {
                    *out_value = int_value;
                    return 0;
                }
                if (emit_diagnostics)
                    fprintf(stderr, "Error: Trunc argument must be a const expression.\n");
                return 1;
            }
            
            /* Handle Cardinal, LongWord, DWord, QWord, Int64, UInt64, HRESULT and other integer typecasts
             * for constant expressions (FPC bootstrap: Cardinal(not Cardinal(0)), HRESULT($80020004)) */
            if (id != NULL && (pascal_identifier_equals(id, "Cardinal") ||
                               pascal_identifier_equals(id, "LongWord") ||
                               pascal_identifier_equals(id, "DWord") ||
                               pascal_identifier_equals(id, "QWord") ||
                               pascal_identifier_equals(id, "Int64") ||
                               pascal_identifier_equals(id, "UInt64") ||
                               pascal_identifier_equals(id, "NativeInt") ||
                               pascal_identifier_equals(id, "NativeUInt") ||
                               pascal_identifier_equals(id, "SizeInt") ||
                               pascal_identifier_equals(id, "SizeUInt") ||
                               pascal_identifier_equals(id, "ShortInt") ||
                               pascal_identifier_equals(id, "SmallInt") ||
                               pascal_identifier_equals(id, "LongInt") ||
                               pascal_identifier_equals(id, "HRESULT")))
            {
                struct Expression *arg = extract_single_const_arg(args, id);
                if (arg == NULL)
                    return 1;
                
                /* Evaluate the argument as a const expression */
                long long int_value;
                if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: %s argument must be a const expression.\n", id);
                    return 1;
                }
                
                /* Apply appropriate mask for the target type */
                if (pascal_identifier_equals(id, "Cardinal") ||
                    pascal_identifier_equals(id, "LongWord") ||
                    pascal_identifier_equals(id, "DWord"))
                {
                    *out_value = (unsigned int)(int_value & 0xFFFFFFFFULL);
                }
                else if (pascal_identifier_equals(id, "ShortInt"))
                {
                    *out_value = (signed char)(int_value & 0xFF);
                }
                else if (pascal_identifier_equals(id, "SmallInt"))
                {
                    *out_value = (short)(int_value & 0xFFFF);
                }
                else if (pascal_identifier_equals(id, "LongInt") ||
                         pascal_identifier_equals(id, "HRESULT"))
                {
                    /* LongInt and HRESULT are 32-bit signed integers */
                    *out_value = (int)(int_value & 0xFFFFFFFFULL);
                }
                else
                {
                    /* QWord, Int64, UInt64, NativeInt, NativeUInt, SizeInt, SizeUInt - 64-bit */
                    *out_value = int_value;
                }
                return 0;
            }

            if (id != NULL && args != NULL && args->next == NULL)
            {
                HashNode_t *type_node = NULL;
                if (FindIdent(&type_node, symtab, id) >= 0 && type_node != NULL && type_node->type != NULL)
                {
                    int legacy_tag = semcheck_tag_from_kgpc(type_node->type);
                    if (legacy_tag == UNKNOWN_TYPE && type_node->type->type_alias != NULL)
                        legacy_tag = type_node->type->type_alias->base_type;
                    if (legacy_tag != UNKNOWN_TYPE)
                    {
                        struct Expression *arg = (struct Expression *)args->cur;
                        long long int_value;
                        if (arg == NULL)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr, "Error: %s argument is NULL.\n", id);
                            return 1;
                        }
                        if (const_fold_int_expr_mode(symtab, arg, &int_value, emit_diagnostics) != 0)
                        {
                            if (emit_diagnostics)
                                fprintf(stderr, "Error: %s argument must be a const expression.\n", id);
                            return 1;
                        }
                        if (type_node->type->type_alias != NULL)
                        {
                            int storage_size = type_node->type->type_alias->storage_size;
                            if (storage_size > 0 && storage_size < 8)
                            {
                                unsigned long long mask = (1ULL << (storage_size * 8)) - 1ULL;
                                int_value = (long long)((unsigned long long)int_value & mask);
                            }
                        }
                        *out_value = int_value;
                        return 0;
                    }
                }
            }

            if (emit_diagnostics)
            {
                if (id != NULL)
                    fprintf(stderr, "Error: const expression uses unsupported function %s on line %d.\n", id, expr->line_num);
                fprintf(stderr, "Error: only Ord(), High(), Low(), SizeOf(), BitSizeOf(), Chr(), Trunc(), and integer typecasts are supported in const expressions.\n");
            }
            return 1;
        }
        case EXPR_RELOP:
        {
            /* Handle NOT operator for constant expressions (bitwise NOT) */
            /* FPC bootstrap uses: Cardinal(not Cardinal(0)) */
            if (expr->expr_data.relop_data.type == NOT)
            {
                /* NOT is a unary operator - right operand is NULL */
                struct Expression *operand = expr->expr_data.relop_data.left;
                if (operand == NULL)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: NOT operator requires an operand.\n");
                    return 1;
                }
                
                long long operand_value;
                if (const_fold_int_expr_mode(symtab, operand, &operand_value, emit_diagnostics) != 0)
                {
                    if (emit_diagnostics)
                        fprintf(stderr, "Error: NOT operand must be a const expression.\n");
                    return 1;
                }
                
                /* Bitwise NOT */
                *out_value = ~operand_value;
                return 0;
            }
            /* Relational operators (comparisons) for const expressions */
            if (expr->expr_data.relop_data.left != NULL && expr->expr_data.relop_data.right != NULL)
            {
                long long left_val, right_val;
                if (const_fold_int_expr_mode(symtab, expr->expr_data.relop_data.left, &left_val,
                        emit_diagnostics) != 0)
                    return 1;
                if (const_fold_int_expr_mode(symtab, expr->expr_data.relop_data.right, &right_val,
                        emit_diagnostics) != 0)
                    return 1;
                
                switch (expr->expr_data.relop_data.type)
                {
                    case EQ: *out_value = (left_val == right_val) ? 1 : 0; return 0;
                    case NE: *out_value = (left_val != right_val) ? 1 : 0; return 0;
                    case LT: *out_value = (left_val < right_val) ? 1 : 0; return 0;
                    case LE: *out_value = (left_val <= right_val) ? 1 : 0; return 0;
                    case GT: *out_value = (left_val > right_val) ? 1 : 0; return 0;
                    case GE: *out_value = (left_val >= right_val) ? 1 : 0; return 0;
                    default:
                        break;
                }
            }
            if (emit_diagnostics)
                fprintf(stderr, "Error: unsupported relational operator in const expression.\n");
            return 1;
        }
        default:
            break;
    }

    if (emit_diagnostics)
        fprintf(stderr, "Error: unsupported const expression.\n");
    return 1;
}

static int const_fold_int_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value)
{
    return const_fold_int_expr_mode(symtab, expr, out_value, 0);
}

static int const_fold_real_expr(SymTab_t *symtab, struct Expression *expr, double *out_value)
{
    return const_fold_real_expr_mode(symtab, expr, out_value, 0);
}

static int evaluate_const_expr(SymTab_t *symtab, struct Expression *expr, long long *out_value)
{
    return const_fold_int_expr_mode(symtab, expr, out_value, 1);
}

static int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value)
{
    return const_fold_real_expr_mode(symtab, expr, out_value, 1);
}

/* The main function for checking a tree */
/* Return values:
    0       -> Check successful
    -1      -> Check successful with warnings
    >= 1    -> Check failed with n errors
*/
SymTab_t *start_semcheck(Tree_t *parse_tree, int *sem_result)
{
    SymTab_t *symtab;
    int return_val;
    double t0 = 0.0;

    assert(parse_tree != NULL);
    assert(sem_result != NULL);

    /* Reset warning counter at start of semantic check */
    g_semcheck_warning_count = 0;

    if (g_semcheck_source_path != NULL)
        file_to_parse = g_semcheck_source_path;
    if (g_semcheck_source_buffer != NULL)
    {
        if (preprocessed_source != NULL)
            free(preprocessed_source);
        preprocessed_source = strdup(g_semcheck_source_buffer);
        preprocessed_length = g_semcheck_source_length;
    }

    symtab = InitSymTab();
    PushScope(symtab);  /* Push global scope for built-in constants and types */
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    semcheck_add_builtins(symtab);
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_add_builtins: %.2f ms\n", t1 - t0);
    }
    /*PrintSymTab(symtab, stderr, 0);*/

    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    if (parse_tree->type == TREE_UNIT) {
        return_val = semcheck_unit(symtab, parse_tree);
    } else {
        return_val = semcheck_program(symtab, parse_tree);
    }
    if (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_program/unit: %.2f ms\n", t1 - t0);
    }

    if(return_val > 0)
        fprintf(stderr, "\nCheck failed with %d error(s)!\n\n", return_val);
    else if (g_semcheck_warning_count > 0)
        fprintf(stderr, "\nCheck successful with %d warning(s)!\n\n", g_semcheck_warning_count);
    else
        fprintf(stderr, "\nCheck successful!\n\n");

    /* Return -1 if there were warnings but no errors (per SemCheck.h API) */
    if (return_val == 0 && g_semcheck_warning_count > 0)
        return_val = -1;

    *sem_result = return_val;
    return symtab;
}

/* Register anonymous enum values from record field declarations.
 * For fields like `kind: (vInteger, vString, vNone)`, the enum values
 * must be visible in the enclosing scope. */
static int register_record_field_enum_literals(SymTab_t *symtab, struct RecordType *record)
{
    if (symtab == NULL || record == NULL)
        return 0;

    int errors = 0;
    ListNode_t *field_node = record->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                ListNode_t *lit = field->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL)
                    {
                        char *name = (char *)lit->cur;
                        HashNode_t *existing = NULL;
                        if (FindIdent(&existing, symtab, name) == -1)
                        {
                            if (PushConstOntoScope_Typed(symtab, name, ordinal, enum_type) > 0)
                                ++errors;
                        }
                    }
                    ++ordinal;
                    lit = lit->next;
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (field_node->type == LIST_VARIANT_PART && field_node->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)field_node->cur;
            /* Check tag field for anonymous enum */
            if (variant->tag_field != NULL && variant->tag_field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                ListNode_t *lit = variant->tag_field->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL)
                    {
                        char *name = (char *)lit->cur;
                        HashNode_t *existing = NULL;
                        if (FindIdent(&existing, symtab, name) == -1)
                        {
                            if (PushConstOntoScope_Typed(symtab, name, ordinal, enum_type) > 0)
                                ++errors;
                        }
                    }
                    ++ordinal;
                    lit = lit->next;
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
            /* Recurse into variant branches */
            ListNode_t *branch_node = variant->branches;
            while (branch_node != NULL)
            {
                if (branch_node->type == LIST_VARIANT_BRANCH && branch_node->cur != NULL)
                {
                    struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                    /* Create a temporary RecordType to recurse */
                    struct RecordType temp_rec;
                    memset(&temp_rec, 0, sizeof(temp_rec));
                    temp_rec.fields = branch->members;
                    errors += register_record_field_enum_literals(symtab, &temp_rec);
                }
                branch_node = branch_node->next;
            }
        }
        field_node = field_node->next;
    }
    return errors;
}

/* Pushes a bunch of type declarations onto the current scope */
static int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                struct TypeAlias *alias_info = &tree->tree_data.type_decl_data.info.alias;
                if (alias_info != NULL && alias_info->is_enum && alias_info->enum_literals != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_ENUM_LITERALS") != NULL)
                    {
                        fprintf(stderr, "[KGPC] enum predeclare %s scoped=%d line=%d\n",
                            tree->tree_data.type_decl_data.id != NULL ? tree->tree_data.type_decl_data.id : "<anon>",
                            alias_info->enum_is_scoped,
                            tree->line_num);
                    }
                    /* Create ONE shared KgpcType for this enum type if not already created */
                    if (alias_info->kgpc_type == NULL)
                    {
                        alias_info->kgpc_type = create_primitive_type(ENUM_TYPE);
                        if (alias_info->kgpc_type == NULL)
                        {
                            fprintf(stderr, "Error: Failed to create enum type for %s\n",
                                    tree->tree_data.type_decl_data.id);
                            ++errors;
                        }
                    }
                    if (alias_info->kgpc_type != NULL &&
                        alias_info->kgpc_type->type_alias == NULL)
                    {
                        kgpc_type_set_type_alias(alias_info->kgpc_type, alias_info);
                    }

                    /* Predeclare the enum type itself so consts can reference it
                     * before full type processing (e.g., array[TEnum] in consts). */
                    if (tree->tree_data.type_decl_data.id != NULL)
                    {
                        HashNode_t *existing_type = NULL;
                        int scope_level = FindIdent(&existing_type, symtab,
                            tree->tree_data.type_decl_data.id);
                        if (!(scope_level == 0 && existing_type != NULL))
                        {
                            PushTypeOntoScope_Typed(symtab,
                                tree->tree_data.type_decl_data.id, alias_info->kgpc_type);
                            HashNode_t *node = NULL;
                            if (FindIdent(&node, symtab, tree->tree_data.type_decl_data.id) != -1 &&
                                node != NULL)
                            {
                                mark_hashnode_unit_info(symtab, node,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(node,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                    }

                    /* Scoped enums expose values only through qualified lookup (TEnum.Value),
                     * so do not inject their literals as global constants in this scope. */
                    if (alias_info->kgpc_type != NULL && !alias_info->enum_is_scoped)
                    {
                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->enum_literals;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                
                                /* Check for collisions in the current scope only (allow shadowing). */
                                HashNode_t *existing = FindIdentInTable(
                                    (HashTable_t *)symtab->stack_head->cur, literal_name);
                                if (existing != NULL)
                                {
                                    /* Allow local enum literals to shadow imported unit literals. */
                                    if (!tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        if (existing->type != NULL && existing->type != alias_info->kgpc_type)
                                            kgpc_type_release(existing->type);
                                        existing->is_constant = 1;
                                        existing->const_int_value = ordinal;
                                        existing->type = alias_info->kgpc_type;
                                        existing->defined_in_unit = 0;
                                        kgpc_type_retain(existing->type);
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Imported unit collisions are allowed: keep first visible literal. */
                                    if (tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* If it exists as a constant with the same value, skip silently */
                                    if (existing->is_constant && existing->const_int_value == ordinal)
                                    {
                                        /* Same enum literal from prelude - not an error */
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Different value in local declarations is a real conflict. */
                                    semcheck_error_with_context(
                                        "Error on line %d, redeclaration of enum literal %s with different value!\n",
                                        tree->line_num, literal_name);
                                    ++errors;
                                    literal_node = literal_node->next;
                                    ++ordinal;
                                    continue;
                                }
                                
                                /* Use typed API with shared enum KgpcType - all literals reference same type */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, alias_info->kgpc_type) > 0)
                                {
                                    semcheck_error_with_context(
                                            "Error on line %d, redeclaration of enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                                else if (tree->tree_data.type_decl_data.defined_in_unit)
                                {
                                    HashNode_t *literal_node_entry = FindIdentInTable(
                                        (HashTable_t *)symtab->stack_head->cur, literal_name);
                                    if (literal_node_entry != NULL)
                                    {
                                        literal_node_entry->defined_in_unit = 1;
                                        literal_node_entry->unit_is_public =
                                            tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                    }
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        /* KgpcType is owned by TypeAlias, will be cleaned up when tree is destroyed */
                    }
                }
                /* Also handle set types with inline anonymous enum: set of (val1, val2, ...) */
                if (alias_info != NULL && alias_info->is_enum_set && alias_info->inline_enum_values != NULL)
                {
                    /* Create a local KgpcType for the inline enum values (not stored in alias).
                     * The set type itself remains SET_TYPE, but the enum values need their own type. */
                    KgpcType *inline_enum_type = create_primitive_type(ENUM_TYPE);
                    if (inline_enum_type == NULL)
                    {
                        fprintf(stderr, "Error: Failed to create inline enum type for set %s\n",
                                tree->tree_data.type_decl_data.id);
                        ++errors;
                    }
                    else
                    {
                        int ordinal = 0;
                        ListNode_t *literal_node = alias_info->inline_enum_values;
                        while (literal_node != NULL)
                        {
                            if (literal_node->cur != NULL)
                            {
                                char *literal_name = (char *)literal_node->cur;
                                
                                /* Check for collisions in the current scope only (allow shadowing). */
                                HashNode_t *existing = FindIdentInTable(
                                    (HashTable_t *)symtab->stack_head->cur, literal_name);
                                if (existing != NULL)
                                {
                                    /* Allow local enum literals to shadow imported unit literals. */
                                    if (!tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        if (existing->type != NULL && existing->type != inline_enum_type)
                                            kgpc_type_release(existing->type);
                                        existing->is_constant = 1;
                                        existing->const_int_value = ordinal;
                                        existing->type = inline_enum_type;
                                        existing->defined_in_unit = 0;
                                        kgpc_type_retain(existing->type);
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    if (tree->tree_data.type_decl_data.defined_in_unit)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* If it exists as a constant with the same value, skip silently */
                                    if (existing->is_constant && existing->const_int_value == ordinal)
                                    {
                                        literal_node = literal_node->next;
                                        ++ordinal;
                                        continue;
                                    }
                                    /* Different value in local declarations is a real conflict. */
                                    semcheck_error_with_context(
                                        "Error on line %d, redeclaration of inline enum literal %s with different value!\n",
                                        tree->line_num, literal_name);
                                    ++errors;
                                    literal_node = literal_node->next;
                                    ++ordinal;
                                    continue;
                                }
                                
                                /* Use typed API with shared enum KgpcType */
                                if (PushConstOntoScope_Typed(symtab, literal_name, ordinal, inline_enum_type) > 0)
                                {
                                    semcheck_error_with_context(
                                            "Error on line %d, redeclaration of inline enum literal %s!\n",
                                            tree->line_num, literal_name);
                                    ++errors;
                                }
                                else if (tree->tree_data.type_decl_data.defined_in_unit)
                                {
                                    HashNode_t *literal_node_entry = FindIdentInTable(
                                        (HashTable_t *)symtab->stack_head->cur, literal_name);
                                    if (literal_node_entry != NULL)
                                    {
                                        literal_node_entry->defined_in_unit = 1;
                                        literal_node_entry->unit_is_public =
                                            tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                    }
                                }
                            }
                            ++ordinal;
                            literal_node = literal_node->next;
                        }
                        /* The enum constants now hold references to inline_enum_type, so we can release our ref */
                        destroy_kgpc_type(inline_enum_type);
                    }
                }
            }
            /* Handle record types with fields that have anonymous enum types */
            if (tree->type == TREE_TYPE_DECL &&
                tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            {
                struct RecordType *record = tree->tree_data.type_decl_data.info.record;
                if (record != NULL)
                    errors += register_record_field_enum_literals(symtab, record);
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Pre-declare types so they can be used in const expressions like High(MyType).
 * This function creates stub entries in the symbol table for SIMPLE type aliases
 * BEFORE const declarations are processed.
 *
 * This enables patterns like:
 *   type
 *     MyInt = Int64;
 *   const
 *     MaxMyInt = High(MyInt);  // Works because MyInt is pre-declared
 *
 * IMPORTANT: We only pre-declare simple primitive type aliases (e.g., MyInt = Int64).
 * Complex types (pointers, arrays, records, etc.) are NOT pre-declared because
 * they may have forward references that can't be resolved yet.
 */
static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;
            if (tree->type == TREE_TYPE_DECL)
            {
                const char *type_id = tree->tree_data.type_decl_data.id;
                
                /* Skip if no type id */
                if (type_id == NULL)
                {
                    cur = cur->next;
                    continue;
                }
                
                /* Debug: print predeclare order */
                if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                {
                    fprintf(stderr, "[predeclare] type '%s' unit=%d",
                        type_id, tree->tree_data.type_decl_data.defined_in_unit);
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                    {
                        struct TypeAlias *a = &tree->tree_data.type_decl_data.info.alias;
                        fprintf(stderr, " alias target='%s'", a->target_type_id ? a->target_type_id : "<null>");
                    }
                    fprintf(stderr, "\n");
                }
                
                /* Check if already declared (e.g., from a previous pass or builtin) */
                HashNode_t *existing = NULL;
                int scope_level = FindIdent(&existing, symtab, type_id);
                if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL && type_id != NULL &&
                    pascal_identifier_equals(type_id, "TFloatFormatProfile"))
                {
                    fprintf(stderr, "[TFLOAT] predeclare existing scope=%d node=%p kind=%d\n",
                        scope_level, (void *)existing,
                        tree->tree_data.type_decl_data.kind);
                }
                if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                    pascal_identifier_equals(type_id, "TSize"))
                {
                    fprintf(stderr, "[TSIZE-FLOW] tree: defined_in_unit=%d src_unit=%d kind=%d | existing=%p scope=%d",
                        tree->tree_data.type_decl_data.defined_in_unit,
                        tree->tree_data.type_decl_data.source_unit_index,
                        tree->tree_data.type_decl_data.kind,
                        (void *)existing, scope_level);
                    if (existing != NULL)
                        fprintf(stderr, " ex_defined_in_unit=%d ex_src_unit=%d ex_has_record=%d",
                            existing->defined_in_unit, existing->source_unit_index,
                            get_record_type_from_node(existing) != NULL);
                    fprintf(stderr, "\n");
                }
                if (scope_level == 0 && existing != NULL)
                {
                    /* If the existing symbol is a non-record alias and we are now defining
                     * a record with the same name, replace the alias with the record.
                     * This is required for units that import a type alias (e.g. TSize = LongInt)
                     * and then define their own record TSize with methods.
                     * However, if they come from different units, push as a new entry
                     * so per-unit type resolution can pick the right one. */
                    int cross_unit_replace = 0;
                    if (tree->tree_data.type_decl_data.source_unit_index != 0 &&
                        existing->source_unit_index != 0 &&
                        tree->tree_data.type_decl_data.source_unit_index != existing->source_unit_index)
                        cross_unit_replace = 1;
                    /* A local type (source_unit_index=0) shadowing a unit type
                     * must also be pushed as a separate entry, not replaced
                     * in-place, so unit functions still see their original type. */
                    if (tree->tree_data.type_decl_data.source_unit_index == 0 &&
                        existing->source_unit_index != 0)
                        cross_unit_replace = 1;
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                        existing->hash_type == HASHTYPE_TYPE &&
                        get_record_type_from_node(existing) == NULL &&
                        !cross_unit_replace)
                    {
                        struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;
                        if (record_info != NULL)
                        {
                            if (record_info->type_id == NULL)
                            {
                                record_info->type_id = strdup(type_id);
                                /* For nested types like TOuter.TInner, extract outer_type_id */
                                if (record_info->outer_type_id == NULL && type_id != NULL)
                                {
                                    const char *dot = strrchr(type_id, '.');
                                    if (dot != NULL && dot != type_id)
                                        record_info->outer_type_id = strndup(type_id, (size_t)(dot - type_id));
                                }
                            }

                            KgpcType *kgpc_type = create_record_type(record_info);
                            if (record_type_is_class(record_info))
                                kgpc_type = create_pointer_type(kgpc_type);

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            if (existing->type != NULL)
                                destroy_kgpc_type(existing->type);
                            if (kgpc_type != NULL)
                                kgpc_type_retain(kgpc_type);
                            existing->type = kgpc_type;
                            mark_hashnode_unit_info(symtab, existing,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public);
                            mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        cur = cur->next;
                        continue;
                    }

                    /* If we already have a type alias without range/enum metadata and are now
                     * declaring a range/enum, replace the alias so Low/High work in consts.
                     * This is needed for circular unit references like Math -> Types. */
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                        existing->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *existing_alias = get_type_alias_from_node(existing);
                        int new_has_range = (alias->is_range || alias->range_known);
                        int existing_has_range = (existing_alias != NULL && existing_alias->range_known);
                        int new_has_enum = (alias->is_enum && alias->enum_literals != NULL);
                        int existing_has_enum =
                            (existing_alias != NULL && existing_alias->is_enum &&
                             existing_alias->enum_literals != NULL);

                        if ((new_has_range && !existing_has_range) ||
                            (new_has_enum && !existing_has_enum))
                        {
                            KgpcType *kgpc_type = NULL;
                            if (new_has_enum)
                            {
                                kgpc_type = create_primitive_type(ENUM_TYPE);
                            }
                            else
                            {
                                int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                                if (alias->storage_size > 0)
                                    kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                                else
                                    kgpc_type = create_primitive_type(base_tag);
                            }

                            if (kgpc_type != NULL)
                            {
                                kgpc_type_set_type_alias(kgpc_type, alias);
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                /* Release creator's reference */
                                destroy_kgpc_type(kgpc_type);
                            }
                            cur = cur->next;
                            continue;
                        }

                        /* If we already have a non-pointer alias and the new declaration
                         * is a pointer alias (e.g., punicodemap = ^tunicodemap replacing
                         * a predeclared punicodemap = Pointer stub), upgrade the existing
                         * entry to TYPE_KIND_POINTER so pointer dereferences work. */
                        struct TypeAlias *new_alias_ptr = &tree->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *existing_alias_ptr = get_type_alias_from_node(existing);
                        int new_is_pointer = (new_alias_ptr != NULL && new_alias_ptr->is_pointer);
                        int existing_is_pointer = (existing_alias_ptr != NULL && existing_alias_ptr->is_pointer);
                        if (new_is_pointer && !existing_is_pointer)
                        {
                            KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                                new_alias_ptr, symtab,
                                tree->tree_data.type_decl_data.defined_in_unit);
                            if (kgpc_type == NULL)
                            {
                                /* Fallback: create a basic pointer type */
                                kgpc_type = create_pointer_type(NULL);
                                kgpc_type_set_type_alias(kgpc_type, new_alias_ptr);
                            }
                            if (kgpc_type != NULL)
                            {
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                destroy_kgpc_type(kgpc_type);
                            }
                            cur = cur->next;
                            continue;
                        }
                    }

                    /* Allow record types from different units to coexist as separate entries.
                     * This handles cases like System.tsiginfo vs SysUtils.tsiginfo and
                     * UnixType.TSize (alias) vs Types.TSize (record).
                     * The ranking logic picks the right one based on same-unit preference. */
                    int existing_is_record = (existing->type != NULL &&
                        existing->type->kind == TYPE_KIND_RECORD);
                    struct TypeAlias *existing_alias = get_type_alias_from_node(existing);
                    int current_is_enum_alias = (
                        tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                        tree->tree_data.type_decl_data.info.alias.is_enum);
                    int existing_is_enum_alias = (
                        existing_alias != NULL &&
                        existing_alias->is_enum);
                    int cross_unit_coexist = (
                        (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD ||
                         (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                            (existing_is_record || current_is_enum_alias || existing_is_enum_alias))) &&
                        existing->source_unit_index != 0 &&
                        tree->tree_data.type_decl_data.source_unit_index != 0 &&
                        existing->source_unit_index != tree->tree_data.type_decl_data.source_unit_index);
                    if (!(existing->defined_in_unit &&
                          !tree->tree_data.type_decl_data.defined_in_unit) &&
                        !cross_unit_coexist)
                    {
                        /* Check if this is a forward class declaration being completed.
                         * Forward: "TObject = class;" creates empty class, then full definition follows.
                         * Allow the full definition to replace the forward declaration. */
                        int is_forward_class_completion = 0;
                        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                        {
                            struct RecordType *new_record = tree->tree_data.type_decl_data.info.record;
                            struct RecordType *existing_record = get_record_type_from_node(existing);
                            if (new_record != NULL && new_record->is_class &&
                                existing_record != NULL && existing_record->is_class)
                            {
                                /* Check if existing has only hidden fields (forward decl) */
                                int has_non_hidden = 0;
                                ListNode_t *fnode = existing_record->fields;
                                while (fnode != NULL)
                                {
                                    if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                    {
                                        struct RecordField *f = (struct RecordField *)fnode->cur;
                                        if (!record_field_is_hidden(f))
                                        {
                                            has_non_hidden = 1;
                                            break;
                                        }
                                    }
                                    fnode = fnode->next;
                                }
                                /* The full definition may have fields, properties,
                                 * method templates, or a parent class – any of these
                                 * indicate a real class body replacing the forward stub. */
                                int new_has_body = (new_record->fields != NULL ||
                                    new_record->properties != NULL ||
                                    new_record->method_templates != NULL ||
                                    new_record->parent_class_name != NULL);
                                if (!has_non_hidden && new_has_body)
                                    is_forward_class_completion = 1;
                            }
                            /* Also handle forward interface declarations:
                             * "IMyIntf = interface;" followed by the full definition. */
                            if (new_record != NULL && new_record->is_interface &&
                                existing_record != NULL && existing_record->is_interface)
                            {
                                int existing_has_body = (existing_record->fields != NULL ||
                                    existing_record->properties != NULL ||
                                    existing_record->method_templates != NULL);
                                int new_has_body = (new_record->fields != NULL ||
                                    new_record->properties != NULL ||
                                    new_record->method_templates != NULL ||
                                    new_record->parent_class_name != NULL);
                                if (!existing_has_body && new_has_body)
                                    is_forward_class_completion = 1;
                            }
                        }
                        /* Allow a simple type alias from a unit to override an
                         * existing alias when the target type differs AND both
                         * are plain (non-array, non-pointer) aliases.  This is
                         * required for objpas.pp which redefines
                         *   Integer = LongInt   (overriding system's Integer = SmallInt)
                         * when {$mode objfpc} or {$mode delphi} is active. */
                        int is_alias_override = 0;
                        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                            existing->hash_type == HASHTYPE_TYPE &&
                            tree->tree_data.type_decl_data.defined_in_unit)
                        {
                            struct TypeAlias *new_alias = &tree->tree_data.type_decl_data.info.alias;
                            struct TypeAlias *old_alias = get_type_alias_from_node(existing);
                            if (new_alias != NULL && old_alias != NULL &&
                                new_alias->target_type_id != NULL &&
                                old_alias->target_type_id != NULL &&
                                !new_alias->is_array && !new_alias->is_pointer &&
                                !new_alias->is_set && !new_alias->is_file &&
                                !new_alias->inline_record_type &&
                                /* Also require the OLD alias to not be a pointer/array/set/file.
                                 * This prevents objpas PString=PAnsiString from overriding
                                 * system pstring=^shortstring (which has is_pointer=1). */
                                !old_alias->is_array && !old_alias->is_pointer &&
                                !old_alias->is_set && !old_alias->is_file &&
                                !old_alias->inline_record_type &&
                                !pascal_identifier_equals(new_alias->target_type_id, old_alias->target_type_id))
                            {
                                /* Only allow override for the specific case where
                                 * objpas.pp redefines Integer=LongInt over system's
                                 * Integer=SmallInt.  We also handle MaxInt redefinition.
                                 * Other aliases must not be overridden to avoid breaking
                                 * NativeInt, TDateTime, PString, etc. */
                                if (pascal_identifier_equals(tree->tree_data.type_decl_data.id, "Integer") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PInteger") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "SizeInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PtrInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "CodePtrInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "ValSInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "ValUInt"))
                                    is_alias_override = 1;
                            }
                        }
                        if (!is_forward_class_completion && !is_alias_override)
                        {
                            /* Already declared, skip */
                            cur = cur->next;
                            continue;
                        }
                        if (is_alias_override)
                        {
                            /* Replace the existing type with the new alias */
                            struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                            if (alias->target_type_id != NULL)
                                apply_builtin_integer_alias_metadata(alias, alias->target_type_id);
                            int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                            KgpcType *kgpc_type = NULL;
                            if (alias->storage_size > 0)
                                kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                            else
                                kgpc_type = create_primitive_type(base_tag);
                            if (kgpc_type != NULL)
                            {
                                kgpc_type_set_type_alias(kgpc_type, alias);
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                /* Release creator's reference */
                                destroy_kgpc_type(kgpc_type);
                            }
                            cur = cur->next;
                            continue;
                        }
                        else
                        {
                            /* Forward class completion: update existing record in-place.
                             * We must update in-place because existing code (e.g. Self
                             * parameters in method implementations) already holds pointers
                             * to the forward declaration's RecordType. Shadowing would not
                             * update those existing references. */
                            struct RecordType *new_record = tree->tree_data.type_decl_data.info.record;
                            struct RecordType *existing_record = get_record_type_from_node(existing);
                            if (existing_record != NULL && new_record != NULL)
                            {
                                /* Move new fields into existing record, keeping hidden fields at front */
                                ListNode_t *old_fields = existing_record->fields;
                                existing_record->fields = new_record->fields;
                                new_record->fields = NULL; /* Transfer ownership to existing */
                                /* Mark the depleted record so merge_parent_class_fields
                                 * won't re-merge parent fields into this empty shell. */
                                new_record->parent_fields_merged = 1;
                                /* Re-add hidden fields from forward decl at front,
                                 * but ONLY if the full declaration has no parent class.
                                 * When a parent exists, the hidden fields (e.g.
                                 * __kgpc_class_typeinfo) will be inherited from the
                                 * parent during merge_parent_class_fields, so re-adding
                                 * them here would create duplicates. */
                                if (new_record->parent_class_name == NULL)
                                {
                                    while (old_fields != NULL)
                                    {
                                        ListNode_t *next = old_fields->next;
                                        if (old_fields->type == LIST_RECORD_FIELD && old_fields->cur != NULL)
                                        {
                                            struct RecordField *f = (struct RecordField *)old_fields->cur;
                                            if (record_field_is_hidden(f))
                                            {
                                                /* Skip if transferred fields already have this hidden field
                                                 * (both forward and full root-class decls get __kgpc_class_typeinfo
                                                 * from from_cparser.c, so re-adding would create duplicates). */
                                                int already_has = 0;
                                                if (f->name != NULL) {
                                                    ListNode_t *chk = existing_record->fields;
                                                    while (chk != NULL) {
                                                        if (chk->type == LIST_RECORD_FIELD && chk->cur != NULL) {
                                                            struct RecordField *ef = (struct RecordField *)chk->cur;
                                                            if (record_field_is_hidden(ef) && ef->name != NULL &&
                                                                strcmp(ef->name, f->name) == 0) {
                                                                already_has = 1;
                                                                break;
                                                            }
                                                        }
                                                        chk = chk->next;
                                                    }
                                                }
                                                if (!already_has)
                                                {
                                                    old_fields->next = NULL;
                                                    if (existing_record->fields != NULL)
                                                        existing_record->fields = PushListNodeFront(existing_record->fields, old_fields);
                                                    else
                                                        existing_record->fields = old_fields;
                                                }
                                            }
                                        }
                                        old_fields = next;
                                    }
                                }
                                /* Transfer method templates, properties, parent class, interfaces */
                                if (new_record->method_templates != NULL)
                                {
                                    existing_record->method_templates = new_record->method_templates;
                                    new_record->method_templates = NULL;
                                }
                                if (new_record->properties != NULL)
                                {
                                    existing_record->properties = new_record->properties;
                                    new_record->properties = NULL;
                                }
                                if (new_record->parent_class_name != NULL)
                                {
                                    free(existing_record->parent_class_name);
                                    existing_record->parent_class_name = strdup(new_record->parent_class_name);
                                }
                                if (new_record->interface_names != NULL)
                                {
                                    existing_record->interface_names = new_record->interface_names;
                                    existing_record->num_interfaces = new_record->num_interfaces;
                                    new_record->interface_names = NULL;
                                    new_record->num_interfaces = 0;
                                }
                                /* Store kgpc_type in tree for later reuse */
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL && existing->type != NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = existing->type;
                                    kgpc_type_retain(existing->type);
                                }
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                semcheck_mark_resolved_forward_stub(type_decls, cur,
                                    type_id,
                                    tree->tree_data.type_decl_data.source_unit_index,
                                    existing_record);
                            }
                            cur = cur->next;
                            continue;
                        }
                    }
                }

                /* Predeclare record types so they can be referenced (e.g., as function returns) */
                if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;

                    /* Annotate the record with its canonical name if missing */
                    if (record_info != NULL && record_info->type_id == NULL)
                    {
                        record_info->type_id = strdup(type_id);
                        if (record_info->outer_type_id == NULL && type_id != NULL)
                        {
                            const char *dot = strrchr(type_id, '.');
                            if (dot != NULL && dot != type_id)
                                record_info->outer_type_id = strndup(type_id, (size_t)(dot - type_id));
                        }
                    }
                    
                    KgpcType *kgpc_type = create_record_type(record_info);
                    if (record_type_is_class(record_info))
                    {
                        /* Classes are reference types - register as pointers to the record */
                        KgpcType *ptr = create_pointer_type(kgpc_type);
                        kgpc_type_release(kgpc_type); /* create_pointer_type retained inner */
                        kgpc_type = ptr;
                    }
                    if (kgpc_type != NULL)
                    {
                        /* Store in tree for later reuse by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);
                        }

                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                            pascal_identifier_equals(type_id, "TSize"))
                        {
                            fprintf(stderr, "[TSIZE-PUSH] push result=%d tree_defined_in_unit=%d tree_src_unit=%d\n",
                                result, tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_FORWARD_CLASS") != NULL && record_info != NULL && record_info->is_class)
                        {
                            fprintf(stderr, "[FWD-PRE] type='%s' push_result=%d fields=%p\n",
                                type_id, result, (void *)record_info->fields);
                            ListNode_t *dbg = record_info->fields;
                            while (dbg != NULL)
                            {
                                if (dbg->type == LIST_RECORD_FIELD && dbg->cur != NULL)
                                {
                                    struct RecordField *f = (struct RecordField *)dbg->cur;
                                    fprintf(stderr, "[FWD-PRE]   field: %s hidden=%d\n",
                                        f->name ? f->name : "<null>", record_field_is_hidden(f));
                                }
                                dbg = dbg->next;
                            }
                        }
                        if (result > 0)
                        {
                            /* Check if this is a forward class declaration being resolved.
                             * Forward class: "TObject = class;" creates an empty class,
                             * then "TObject = class ... end;" provides the full definition.
                             * Update the existing symbol's type with the full definition. */
                            if (record_info != NULL && record_info->is_class)
                            {
                                HashNode_t *existing = NULL;
                                int find_result = FindIdent(&existing, symtab, type_id);
                                if (kgpc_getenv("KGPC_DEBUG_FORWARD_CLASS") != NULL)
                                    fprintf(stderr, "[FWD] type='%s' find=%d existing=%p hash_type=%d\n",
                                        type_id, find_result, (void*)existing,
                                        existing ? existing->hash_type : -1);
                                if (find_result >= 0 &&
                                    existing != NULL &&
                                    existing->hash_type == HASHTYPE_TYPE &&
                                    existing->type != NULL)
                                {
                                    struct RecordType *existing_record = get_record_type_from_node(existing);
                                    if (existing_record != NULL && existing_record->is_class)
                                    {
                                        /* Check if existing is a forward declaration (only hidden fields) */
                                        int has_non_hidden_fields = 0;
                                        ListNode_t *fnode = existing_record->fields;
                                        while (fnode != NULL)
                                        {
                                            if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                            {
                                                struct RecordField *f = (struct RecordField *)fnode->cur;
                                                if (!record_field_is_hidden(f))
                                                {
                                                    has_non_hidden_fields = 1;
                                                    break;
                                                }
                                            }
                                            fnode = fnode->next;
                                        }
                                        if (!has_non_hidden_fields && record_info->fields != NULL)
                                        {
                                            /* Forward declaration being resolved - update existing record's fields */
                                            /* Prepend the hidden typeinfo field from existing to new record's fields */
                                            ListNode_t *existing_fields = existing_record->fields;
                                            existing_record->fields = record_info->fields;
                                            /* Re-add hidden fields at front, skipping duplicates */
                                            while (existing_fields != NULL)
                                            {
                                                ListNode_t *next = existing_fields->next;
                                                if (existing_fields->type == LIST_RECORD_FIELD && existing_fields->cur != NULL)
                                                {
                                                    struct RecordField *f = (struct RecordField *)existing_fields->cur;
                                                    if (record_field_is_hidden(f))
                                                    {
                                                        int already_has = 0;
                                                        if (f->name != NULL) {
                                                            ListNode_t *chk = existing_record->fields;
                                                            while (chk != NULL) {
                                                                if (chk->type == LIST_RECORD_FIELD && chk->cur != NULL) {
                                                                    struct RecordField *ef = (struct RecordField *)chk->cur;
                                                                    if (record_field_is_hidden(ef) && ef->name != NULL &&
                                                                        strcmp(ef->name, f->name) == 0) {
                                                                        already_has = 1;
                                                                        break;
                                                                    }
                                                                }
                                                                chk = chk->next;
                                                            }
                                                        }
                                                        if (!already_has)
                                                        {
                                                            existing_fields->next = NULL;
                                                            existing_record->fields = PushListNodeFront(existing_record->fields, existing_fields);
                                                        }
                                                    }
                                                    else
                                                    {
                                                        /* Non-hidden field from forward decl - shouldn't happen, just free */
                                                        free(existing_fields);
                                                    }
                                                }
                                                else
                                                {
                                                    free(existing_fields);
                                                }
                                                existing_fields = next;
                                            }
                                            /* Copy over method templates and properties */
                                            if (record_info->method_templates != NULL)
                                                existing_record->method_templates = record_info->method_templates;
                                            if (record_info->properties != NULL)
                                                existing_record->properties = record_info->properties;
                                            if (record_info->parent_class_name != NULL && existing_record->parent_class_name == NULL)
                                                existing_record->parent_class_name = strdup(record_info->parent_class_name);
                                            if (record_info->interface_names != NULL)
                                            {
                                                existing_record->interface_names = record_info->interface_names;
                                                existing_record->num_interfaces = record_info->num_interfaces;
                                                record_info->interface_names = NULL;
                                                record_info->num_interfaces = 0;
                                            }
                                            semcheck_mark_resolved_forward_stub(type_decls, cur,
                                                type_id,
                                                tree->tree_data.type_decl_data.source_unit_index,
                                                existing_record);
                                            result = 0; /* Suppress the error */
                                        }
                                    }
                                }
                            }
                            if (result > 0)
                                errors += result;
                        }
                        else
                        {
                            /* Use FindIdent to get the most recently pushed entry (front of list).
                             * We can't use semcheck_find_type_node_with_unit_flag here because
                             * the newly pushed entry hasn't been marked yet (defined_in_unit=0),
                             * so that function would skip it and find an older entry instead. */
                            HashNode_t *type_node = NULL;
                            FindIdent(&type_node, symtab, type_id);
                            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                            {
                                mark_hashnode_unit_info(symtab, type_node,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(type_node, tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                        /* Release creator's reference - tree and hash table have their own */
                        destroy_kgpc_type(kgpc_type);
                    }
                }
                /* Only handle TYPE_DECL_ALIAS cases we can resolve early */
                else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                {
                    struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                    alias->is_char_alias = semcheck_alias_should_be_char_like(type_id,
                        alias->target_type_id) &&
                        !alias->is_pointer && !alias->is_array && !alias->is_set &&
                        !alias->is_enum && !alias->is_file;

                    /* WideChar/UnicodeChar must be predeclared as CHAR_TYPE before
                     * apply_builtin_integer_alias_metadata converts them to WORD_TYPE. */
                    if (alias->is_char_alias && type_id != NULL &&
                        (pascal_identifier_equals(type_id, "WideChar") ||
                         pascal_identifier_equals(type_id, "UnicodeChar")))
                    {
                        if (alias->alias_name == NULL)
                            alias->alias_name = strdup(type_id);
                        alias->storage_size = 2;
                        KgpcType *kgpc_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        cur = cur->next;
                        continue;
                    }

                    /* Do NOT apply integer alias metadata to pointer types.
                     * For PWord = ^Word, target_type_id is "Word" (the pointee),
                     * and applying Word's metadata (is_range, storage_size=2) would
                     * corrupt the pointer alias, causing it to be treated as a
                     * 2-byte range type instead of an 8-byte pointer. */
                    if (!alias->is_pointer)
                    {
                        if (alias->target_type_id != NULL)
                            apply_builtin_integer_alias_metadata(alias, alias->target_type_id);
                        else if (type_id != NULL)
                            apply_builtin_integer_alias_metadata(alias, type_id);
                    }

                    /* Predeclare range aliases early so Low/High on subranges work in consts. */
                    if (alias->is_range || alias->range_known)
                    {
                        int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                        KgpcType *kgpc_type = NULL;
                        if (alias->storage_size > 0)
                            kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                        else
                            kgpc_type = create_primitive_type(base_tag);
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        cur = cur->next;
                        continue;
                    }

                    /* Handle inline record aliases (e.g., generic specializations) */
                    if (alias->inline_record_type != NULL)
                    {
                        if (alias->inline_record_type->type_id == NULL)
                            alias->inline_record_type->type_id = strdup(type_id);
                        const char *record_name = alias->inline_record_type->type_id;
                        HashNode_t *existing_inline = NULL;
                        int inline_scope = FindIdent(&existing_inline, symtab, record_name);
                        if (inline_scope != 0 || existing_inline == NULL)
                        {
                            KgpcType *inline_kgpc = create_record_type(alias->inline_record_type);
                            if (record_type_is_class(alias->inline_record_type))
                                inline_kgpc = create_pointer_type(inline_kgpc);

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = inline_kgpc;
                                kgpc_type_retain(inline_kgpc);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)record_name, inline_kgpc);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, record_name,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(inline_kgpc);
                        }

                        /* Also register the alias name itself */
                        KgpcType *alias_kgpc = create_record_type(alias->inline_record_type);
                        if (record_type_is_class(alias->inline_record_type))
                            alias_kgpc = create_pointer_type(alias_kgpc);
                        kgpc_type_set_type_alias(alias_kgpc, alias);
                        int alias_result = PushTypeOntoScope_Typed(symtab, (char *)type_id, alias_kgpc);
                        if (alias_result > 0)
                            errors += alias_result;
                        else
                        {
                            mark_latest_type_node_unit_info(symtab, type_id,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        /* Release creator's reference */
                        destroy_kgpc_type(alias_kgpc);

                        cur = cur->next;
                        continue;
                    }

                    /* Pre-declare enum types so they can be used as function return types.
                     * This creates a stub entry - the actual enum literals are registered
                     * separately in predeclare_enum_literals(). */
                    if (alias->is_enum)
                    {
                        KgpcType *kgpc_type = NULL;
                        /* Check if already created during predeclare_enum_literals */
                        if (alias->kgpc_type != NULL)
                        {
                            kgpc_type = alias->kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Own a reference so we can release uniformly */
                        }
                        else
                        {
                            kgpc_type = create_primitive_type(ENUM_TYPE);
                            if (kgpc_type != NULL)
                            {
                                /* Store in alias - alias now owns this reference */
                                alias->kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);  /* Alias holds reference */
                            }
                        }
                        
                        if (kgpc_type != NULL)
                        {
                            /* Attach the type_alias so scoped enum lookup can find enum_literals */
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);  /* Tree holds reference */
                            }
                            
                            /* PushTypeOntoScope_Typed will retain kgpc_type when adding to symbol table */
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        cur = cur->next;
                        continue;
                    }

                    /* Predeclare array/set/file aliases so return types can resolve early. */
                    if (alias->is_array || alias->is_set || alias->is_file)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type != NULL)
                        {
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                            cur = cur->next;
                            continue;
                        }
                        cur = cur->next;
                        continue;
                    }
                    
                    /* Handle pointer aliases to already known element types */
                    if (alias->is_pointer)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type != NULL)
                        {
                            if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_POINTERS") != NULL)
                            {
                                fprintf(stderr, "[KGPC] predeclare pointer alias %s: ptr_type=%d ptr_id=%s kind=%d\n",
                                    type_id,
                                    alias->pointer_type,
                                    alias->pointer_type_id ? alias->pointer_type_id : "<null>",
                                    kgpc_type->kind);
                            }
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);

                            cur = cur->next;
                            continue;
                        }
                    }

                    /* Pre-declare simple alias types (e.g., TEndian = ObjPas.TEndian) so they
                     * are available for const expressions before full type checking. */
                    if (!alias->is_array && !alias->is_set && !alias->is_file &&
                        !alias->is_pointer && alias->base_type == UNKNOWN_TYPE)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type == NULL)
                        {
                            kgpc_type = create_primitive_type(UNKNOWN_TYPE);
                            if (kgpc_type != NULL)
                                kgpc_type_set_type_alias(kgpc_type, alias);
                        }
                        if (kgpc_type != NULL)
                        {
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                            {
                                HashNode_t *existing_type = NULL;
                                if (FindIdent(&existing_type, symtab, type_id) >= 0 &&
                                    existing_type != NULL &&
                                    existing_type->hash_type == HASHTYPE_TYPE &&
                                    existing_type->type != NULL)
                                {
                                    struct TypeAlias *existing_alias =
                                        kgpc_type_get_type_alias(existing_type->type);
                                    int can_overlay_existing =
                                        (existing_alias != NULL) &&
                                        (alias->target_type_ref != NULL ||
                                         alias->target_type_id != NULL) &&
                                        !alias->is_pointer &&
                                        !alias->is_array &&
                                        !alias->is_set &&
                                        !alias->is_file &&
                                        existing_type->source_unit_index == 0 &&
                                        !existing_type->defined_in_unit;
                                    if (can_overlay_existing)
                                    {
                                        inherit_alias_metadata(symtab, alias);
                                        kgpc_type_set_type_alias(existing_type->type, alias);
                                        if (existing_type->type->type_alias != NULL &&
                                            alias->storage_size > 0)
                                        {
                                            existing_type->type->type_alias->storage_size =
                                                alias->storage_size;
                                        }
                                        result = 0;
                                    }
                                }
                                if (result > 0)
                                    errors += result;
                            }
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                            cur = cur->next;
                            continue;
                        }
                    }

                    /* Pre-declare procedural type aliases so const initializers can use them */
                    if (alias->base_type == PROCEDURE ||
                        (tree->tree_data.type_decl_data.kgpc_type != NULL &&
                         tree->tree_data.type_decl_data.kgpc_type->kind == TYPE_KIND_PROCEDURE))
                    {
                        KgpcType *kgpc_type = tree->tree_data.type_decl_data.kgpc_type;
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                        cur = cur->next;
                        continue;
                    }

                    /* Only pre-declare simple primitive type aliases */
                    KgpcType *kgpc_type = NULL;
                    int created_new_type = 0;

                    /* WideChar/UnicodeChar should be treated as 2-byte CHAR_TYPE, even if aliased to Word. */
                    if (type_id != NULL &&
                        (pascal_identifier_equals(type_id, "WideChar") ||
                         pascal_identifier_equals(type_id, "UnicodeChar")))
                    {
                        kgpc_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                        if (kgpc_type != NULL)
                            created_new_type = 1;
                    }
                    
                    /* Case 1: Direct primitive type tag (e.g., MyInt = Integer where base_type is set)
                     * Exclude PROCEDURE - procedure types are NOT primitive and need special handling.
                     * IMPORTANT: If target_type_id is "WideChar", skip to Case 2 to look it up
                     * from the symbol table where it has correct storage_size=2. Without this check,
                     * WideChar aliases would get 4 bytes (INT_TYPE) instead of 2 bytes. */
                    int skip_case1_for_widechar = (alias->target_type_id != NULL &&
                        pascal_identifier_equals(alias->target_type_id, "WideChar"));
                    if (kgpc_type == NULL && !skip_case1_for_widechar &&
                        alias->base_type != UNKNOWN_TYPE && alias->base_type != 0 &&
                        alias->base_type != PROCEDURE)
                    {
                        if (alias->storage_size > 0)
                            kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        else
                            kgpc_type = create_primitive_type(alias->base_type);
                        if (kgpc_type != NULL)
                            created_new_type = 1;
                    }
                    /* Case 2: Reference to a known primitive type name */
                    else if (kgpc_type == NULL && alias->target_type_id != NULL)
                    {
                        /* Check if target is a known builtin primitive type */
                        const TypeRef *target_ref = alias->target_type_ref;
                        const char *target = alias->target_type_id;
                        const char *target_base = (target_ref != NULL)
                            ? type_ref_base_name(target_ref)
                            : target;
                        if (pascal_identifier_equals(target_base, "Int64") ||
                            pascal_identifier_equals(target_base, "QWord") ||
                            pascal_identifier_equals(target_base, "UInt64") ||
                            pascal_identifier_equals(target_base, "SizeUInt") ||
                            pascal_identifier_equals(target_base, "SizeInt") ||
                            pascal_identifier_equals(target_base, "PtrUInt") ||
                            pascal_identifier_equals(target_base, "PtrInt") ||
                            pascal_identifier_equals(target_base, "NativeUInt") ||
                            pascal_identifier_equals(target_base, "NativeInt"))
                        {
                            kgpc_type = create_primitive_type(INT64_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "LongInt") ||
                            pascal_identifier_equals(target_base, "Cardinal") ||
                            pascal_identifier_equals(target_base, "LongWord") ||
                            pascal_identifier_equals(target_base, "DWord"))
                        {
                            kgpc_type = create_primitive_type(LONGINT_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Integer") ||
                            pascal_identifier_equals(target_base, "SmallInt") ||
                            pascal_identifier_equals(target_base, "ShortInt") ||
                            pascal_identifier_equals(target_base, "Byte") ||
                            pascal_identifier_equals(target_base, "Word"))
                        {
                            int storage_size = 0;
                            if (pascal_identifier_equals(target_base, "Byte") ||
                                pascal_identifier_equals(target_base, "ShortInt"))
                                storage_size = 1;
                            else if (pascal_identifier_equals(target_base, "Word") ||
                                     pascal_identifier_equals(target_base, "SmallInt"))
                                storage_size = 2;
                            if (storage_size > 0)
                                kgpc_type = create_primitive_type_with_size(INT_TYPE, storage_size);
                            else
                                kgpc_type = create_primitive_type(INT_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Real") ||
                                 pascal_identifier_equals(target_base, "Double") ||
                                 pascal_identifier_equals(target_base, "Single"))
                        {
                            kgpc_type = create_primitive_type(REAL_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Extended"))
                        {
                            kgpc_type = create_primitive_type(EXTENDED_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Boolean"))
                        {
                            kgpc_type = create_primitive_type(BOOL);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Char") ||
                                 pascal_identifier_equals(target_base, "AnsiChar"))
                        {
                            kgpc_type = create_primitive_type(CHAR_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        /* If target is another user-defined type, check if it's already declared */
                        else
                        {
                            HashNode_t *target_node = NULL;
                            char *lookup_target = NULL;
                            char *lookup_unqualified = NULL;
                            const char *lookup_name = target;
                            int has_qualification = 0;
                            if (target_ref != NULL)
                            {
                                lookup_target = type_ref_render_mangled(target_ref);
                                if (lookup_target != NULL)
                                    lookup_name = lookup_target;
                                if (target_ref->name != NULL && target_ref->name->count > 1)
                                    has_qualification = 1;
                            }

                            /* Handle qualified type names like "UnixType.culong" */
                            int found = FindIdent(&target_node, symtab, lookup_name);
                            if (found < 0 || target_node == NULL)
                            {
                                /* Try unqualified name if we have structured qualification */
                                if (has_qualification)
                                {
                                    lookup_unqualified = type_ref_render_mangled_unqualified(target_ref);
                                    if (lookup_unqualified != NULL)
                                    {
                                        lookup_name = lookup_unqualified;
                                        found = FindIdent(&target_node, symtab, lookup_name);
                                    }
                                }
                            }
                            if (found >= 0 && target_node != NULL && has_qualification &&
                                target_node->type != NULL &&
                                kgpc_type_get_type_alias(target_node->type) == alias)
                            {
                                target_node = semcheck_find_type_excluding_alias(symtab, lookup_name, alias);
                            }
                            free(lookup_target);
                            free(lookup_unqualified);
                            
                            if (found >= 0 &&
                                target_node != NULL && target_node->hash_type == HASHTYPE_TYPE &&
                                target_node->type != NULL)
                            {
                                /* Target type already exists, retain and use it.
                                 * Flag that we're reusing an existing type so we don't
                                 * overwrite its alias info later. */
                                kgpc_type = target_node->type;
                                kgpc_type_retain(kgpc_type);
                            }
                            /* Otherwise, skip - can't resolve this type yet */
                        }
                    }
                    
                    /* Flag to track if we're reusing an existing type (vs creating new) */
                    int reusing_existing = (!created_new_type && kgpc_type != NULL &&
                        kgpc_type_get_type_alias(kgpc_type) != NULL &&
                        kgpc_type_get_type_alias(kgpc_type) != alias);
                    
                    if (kgpc_type != NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                        {
                            fprintf(stderr, "[predeclare] SUCCESS type '%s' kgpc_type=%p kind=%d reusing=%d\n",
                                type_id, (void*)kgpc_type, kgpc_type->kind, reusing_existing);
                        }
                        
                        /* Only modify alias info for newly created types, not reused ones */
                        if (!reusing_existing)
                        {
                            /* IMPORTANT: Inherit storage_size from the original type's alias.
                             * This is critical for types like WideChar (2 bytes) where we retain
                             * the original type but need to preserve its custom storage_size. */
                            struct TypeAlias *original_alias = kgpc_type_get_type_alias(kgpc_type);
                            if (original_alias != NULL && original_alias->storage_size > 0 &&
                                alias->storage_size <= 0)
                            {
                                alias->storage_size = original_alias->storage_size;
                            }
                            
                            /* Set type_alias on KgpcType - only for newly created types */
                            kgpc_type_set_type_alias(kgpc_type, alias);
                        }
                        
                        /* Store in tree for later use by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Tree owns a reference */
                        }
                        
                        /* Push onto symbol table */
                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (result > 0)
                        {
                            /* Should not happen since we checked above, but handle gracefully */
                            errors++;
                        }
                        else
                        {
                            mark_latest_type_node_unit_info(symtab, type_id,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        /* Release creator's reference */
                        destroy_kgpc_type(kgpc_type);
                    }
                    else if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                    {
                        fprintf(stderr, "[predeclare] SKIP type '%s' - kgpc_type is NULL\n", type_id);
                    }
                }
                /* Skip TYPE_DECL_RECORD, TYPE_DECL_GENERIC, TYPE_DECL_RANGE - 
                 * these have complex dependencies and are handled by semcheck_type_decls */
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Helper function to check if a statement contains an asm block */
static int statement_contains_asm_block(struct Statement *stmt)
{
    if (stmt == NULL)
        return 0;
    
    if (stmt->type == STMT_ASM_BLOCK)
        return 1;
    
    if (stmt->type == STMT_COMPOUND_STATEMENT)
    {
        ListNode_t *cur = stmt->stmt_data.compound_statement;
        while (cur != NULL)
        {
            if (cur->type == LIST_STMT && cur->cur != NULL)
            {
                struct Statement *child_stmt = (struct Statement *)cur->cur;
                if (statement_contains_asm_block(child_stmt))
                    return 1;
            }
            cur = cur->next;
        }
    }
    
    return 0;
}

static int record_has_property(struct RecordType *record_info, const char *property_name)
{
    if (record_info == NULL || property_name == NULL)
        return 0;

    ListNode_t *node = record_info->properties;
    while (node != NULL)
    {
        if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)node->cur;
            if (property->name != NULL &&
                pascal_identifier_equals(property->name, property_name))
                return 1;
        }
        node = node->next;
    }
    return 0;
}

static struct ClassProperty *clone_class_property(const struct ClassProperty *property)
{
    if (property == NULL)
        return NULL;

    struct ClassProperty *clone = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    if (clone == NULL)
        return NULL;

    clone->name = property->name != NULL ? strdup(property->name) : NULL;
    clone->type = property->type;
    clone->type_id = property->type_id != NULL ? strdup(property->type_id) : NULL;
    clone->type_ref = property->type_ref != NULL ? type_ref_clone(property->type_ref) : NULL;
    clone->read_accessor = property->read_accessor != NULL ? strdup(property->read_accessor) : NULL;
    clone->write_accessor = property->write_accessor != NULL ? strdup(property->write_accessor) : NULL;
    clone->is_indexed = property->is_indexed;
    clone->is_default = property->is_default;

    if ((property->name != NULL && clone->name == NULL) ||
        (property->type_id != NULL && clone->type_id == NULL) ||
        (property->read_accessor != NULL && clone->read_accessor == NULL) ||
        (property->write_accessor != NULL && clone->write_accessor == NULL))
    {
        free(clone->name);
        free(clone->type_id);
        if (clone->type_ref != NULL)
            type_ref_free(clone->type_ref);
        free(clone->read_accessor);
        free(clone->write_accessor);
        free(clone);
        return NULL;
    }

    return clone;
}

static ListNode_t *clone_property_list_unique(struct RecordType *record_info,
    const ListNode_t *properties)
{
    ListNode_t *head = NULL;
    ListNode_t **tail = &head;

    while (properties != NULL)
    {
        if (properties->type == LIST_CLASS_PROPERTY && properties->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)properties->cur;
            if (!record_has_property(record_info, property->name))
            {
                struct ClassProperty *clone = clone_class_property(property);
                if (clone != NULL)
                {
                    ListNode_t *node = CreateListNode(clone, LIST_CLASS_PROPERTY);
                    if (node != NULL)
                    {
                        *tail = node;
                        tail = &node->next;
                    }
                    else
                    {
                        free(clone->name);
                        free(clone->type_id);
                        free(clone->read_accessor);
                        free(clone->write_accessor);
                        free(clone);
                    }
                }
            }
        }
        properties = properties->next;
    }

    return head;
}

/* Helper function to check for circular inheritance */
static int check_circular_inheritance(SymTab_t *symtab, const char *class_name, const char *parent_name, int max_depth)
{
    if (class_name == NULL || parent_name == NULL)
        return 0;
    
    /* Check if parent is the same as this class (direct circular reference) */
    if (strcmp(class_name, parent_name) == 0)
        return 1;
    
    /* Prevent infinite recursion by limiting depth */
    if (max_depth <= 0)
        return 1;
    
    /* Look up parent class */
    HashNode_t *parent_node = NULL;
    if (FindIdent(&parent_node, symtab, parent_name) == -1 || parent_node == NULL)
        return 0;  /* Parent not found yet, not necessarily circular */
    
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL || parent_record->parent_class_name == NULL)
        return 0;  /* Parent has no parent, no circular reference */
    
    /* Recursively check parent's parent */
    return check_circular_inheritance(symtab, class_name, parent_record->parent_class_name, max_depth - 1);
}

static int add_class_padding_field(struct RecordType *record_info, long long padding_bytes)
{
    if (record_info == NULL || padding_bytes <= 0)
        return 0;

    struct RecordField *padding = (struct RecordField *)calloc(1, sizeof(struct RecordField));
    if (padding == NULL)
        return 1;

    padding->name = NULL;
    padding->type = CHAR_TYPE;
    padding->is_array = 1;
    padding->array_start = 0;
    padding->array_end = (int)padding_bytes - 1;
    padding->array_element_type = CHAR_TYPE;
    padding->array_is_open = 0;
    padding->is_hidden = 1;

    ListNode_t *node = CreateListNode(padding, LIST_RECORD_FIELD);
    if (record_info->fields == NULL)
        record_info->fields = node;
    else
        record_info->fields = PushListNodeBack(record_info->fields, node);
    return 0;
}

static int compute_class_record_size(SymTab_t *symtab, struct RecordType *record_info,
    long long *size_out, int line_num)
{
    if (record_info == NULL || size_out == NULL)
        return 1;

    return semcheck_compute_record_size(symtab, record_info, size_out,
        line_num >= 0 ? line_num : 0);
}

static int ensure_class_storage_capacity(SymTab_t *symtab, struct RecordType *record_info,
    long long required_size, int line_num)
{
    if (record_info == NULL)
        return 0;

    long long current_size = 0;
    if (compute_class_record_size(symtab, record_info, &current_size, line_num) != 0)
        return 1;

    if (current_size >= required_size)
        return 0;

    long long padding_bytes = required_size - current_size;
    return add_class_padding_field(record_info, padding_bytes);
}

/*
 * Detect and populate default_indexed_property for a class.
 * This enables array-like indexing (obj[i]) to be transformed to field access (obj.field[i]).
 * 
 * Detection rules:
 * 1. If a class has an open array field (array_is_open == 1) and it's the only such field,
 *    or specifically named "FItems" (Delphi convention), use it as the default indexed property.
 * 2. Also inherit from parent class if parent has a default_indexed_property and child doesn't.
 * 
 * @param record_info The class RecordType to configure. Must not be NULL.
 * @param parent_record The parent class RecordType, or NULL for base classes like TObject.
 */
static void detect_default_indexed_property(struct RecordType *record_info, const struct RecordType *parent_record)
{
    assert(record_info != NULL);
    
    /* Already set - don't override */
    if (record_info->default_indexed_property != NULL)
        return;

    /* Prefer explicitly default indexed properties when available */
    for (int pass = 0; pass < 2; ++pass)
    {
        ListNode_t *prop_node = (pass == 0) ? record_info->properties : record_info->record_properties;
        while (prop_node != NULL)
        {
            if (prop_node->type == LIST_CLASS_PROPERTY && prop_node->cur != NULL)
            {
                struct ClassProperty *prop = (struct ClassProperty *)prop_node->cur;
                if (prop->is_default && prop->is_indexed && prop->name != NULL)
                {
                    record_info->default_indexed_property = strdup(prop->name);
                    record_info->default_indexed_element_type = prop->type;
                    record_info->default_indexed_element_type_id = prop->type_id != NULL ?
                        strdup(prop->type_id) : NULL;
                    return;
                }
            }
            prop_node = prop_node->next;
        }
    }
    
    /* First, try to detect from this class's fields */
    struct RecordField *candidate = NULL;
    ListNode_t *cur = record_info->fields;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field != NULL && field->name != NULL && field->is_array && field->array_is_open)
            {
                /* Found an open array field - it's a candidate for default indexed property */
                /* Prefer "FItems" if we find it (Delphi naming convention for list items) */
                if (pascal_identifier_equals(field->name, "FItems"))
                {
                    candidate = field;
                    break;  /* FItems is the preferred choice */
                }
                else if (candidate == NULL)
                {
                    /* First open array found - tentative candidate */
                    candidate = field;
                }
            }
        }
        cur = cur->next;
    }
    
    if (candidate != NULL)
    {
        record_info->default_indexed_property = strdup(candidate->name);
        record_info->default_indexed_element_type = candidate->array_element_type;
        record_info->default_indexed_element_type_id = candidate->array_element_type_id != NULL ?
            strdup(candidate->array_element_type_id) : NULL;
        return;
    }
    
    /* Inherit from parent if available */
    if (parent_record != NULL && parent_record->default_indexed_property != NULL)
    {
        record_info->default_indexed_property = strdup(parent_record->default_indexed_property);
        record_info->default_indexed_element_type = parent_record->default_indexed_element_type;
        record_info->default_indexed_element_type_id = parent_record->default_indexed_element_type_id != NULL ?
            strdup(parent_record->default_indexed_element_type_id) : NULL;
    }
}

/* Helper function to merge parent class fields into derived class */
static int merge_parent_class_fields(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num)
{
    if (record_info == NULL || record_info->parent_class_name == NULL)
        return 0;  /* No parent class to merge */

    record_info->has_cached_size = 0;

    /* Guard against double-merging: if parent fields have already been
     * merged into this record, skip to avoid duplicating inherited fields.
     * Also check the canonical RecordType in the symbol table — if another
     * RecordType for the same class was already merged (e.g. forward decl
     * vs full decl creating separate RecordType objects), skip this one. */
    if (record_info->parent_fields_merged)
        return 0;
    if (class_name != NULL) {
        HashNode_t *self_node = semcheck_find_preferred_type_node(symtab, class_name);
        if (self_node != NULL) {
            struct RecordType *canonical = get_record_type_from_node(self_node);
            if (canonical != NULL && canonical != record_info && canonical->parent_fields_merged)
                return 0;
        }
    }

    /* Check for circular inheritance */
    if (check_circular_inheritance(symtab, class_name, record_info->parent_class_name, 100))
    {
        semcheck_error_with_context("Error on line %d, circular inheritance detected for class '%s'!\n",
                line_num, class_name ? class_name : "<unknown>");
        return 1;
    }
    
    /* Look up parent class in symbol table */
    HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record_info->parent_class_name);
    if (parent_node == NULL)
    {
        semcheck_error_with_context("Error on line %d, parent class '%s' not found!\n", 
                line_num, record_info->parent_class_name);
        return 1;
    }

    /* Get parent's RecordType */
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL)
    {
        semcheck_error_with_context("Error on line %d, parent class '%s' is not a class/record type!\n",
                line_num, record_info->parent_class_name);
        return 1;
    }

    /* Ensure parent has its own parent fields merged before we clone from it */
    if (parent_record->parent_class_name != NULL && !parent_record->parent_fields_merged)
    {
        int parent_merge_result = merge_parent_class_fields(symtab, parent_record,
            record_info->parent_class_name, line_num);
        if (parent_merge_result > 0)
            return parent_merge_result;
    }

    /* Clone parent's fields and prepend them to this record's fields */
    ListNode_t *parent_fields = parent_record->fields;
    if (parent_fields != NULL)
    {
        /* We need to clone the parent's field list and prepend it */
        ListNode_t *cloned_parent_fields = NULL;
        ListNode_t *cur = parent_fields;
        ListNode_t *last_cloned = NULL;
        
        while (cur != NULL)
        {
            if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            {
                cur = cur->next;
                continue;
            }
            struct RecordField *original_field = (struct RecordField *)cur->cur;

            /* Skip hidden fields from parent if the record already has them
             * (e.g., __kgpc_class_typeinfo added to forward declarations). */
            if (record_field_is_hidden(original_field) && original_field->name != NULL)
            {
                int already_has = 0;
                ListNode_t *own = record_info->fields;
                while (own != NULL)
                {
                    if (own->type == LIST_RECORD_FIELD && own->cur != NULL)
                    {
                        struct RecordField *f = (struct RecordField *)own->cur;
                        if (record_field_is_hidden(f) && f->name != NULL &&
                            strcmp(f->name, original_field->name) == 0)
                        {
                            already_has = 1;
                            break;
                        }
                    }
                    own = own->next;
                }
                if (already_has)
                {
                    cur = cur->next;
                    continue;
                }
            }

            /* Clone the field */
            struct RecordField *cloned_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
            if (cloned_field == NULL)
            {
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    if (field->type_ref != NULL)
                        type_ref_free(field->type_ref);
                    free(field->array_element_type_id);
                    if (field->array_element_type_ref != NULL)
                        type_ref_free(field->array_element_type_ref);
                    if (field->pointer_type_ref != NULL)
                        type_ref_free(field->pointer_type_ref);
                    if (field->proc_type != NULL)
                        kgpc_type_release(field->proc_type);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            cloned_field->name = original_field->name ? strdup(original_field->name) : NULL;
            cloned_field->type = original_field->type;
            cloned_field->type_id = original_field->type_id ? strdup(original_field->type_id) : NULL;
            cloned_field->type_ref = original_field->type_ref != NULL
                ? type_ref_clone(original_field->type_ref) : NULL;
            cloned_field->nested_record = original_field->nested_record;  /* Share the nested record */
            cloned_field->proc_type = original_field->proc_type;
            if (cloned_field->proc_type != NULL)
                kgpc_type_retain(cloned_field->proc_type);
            cloned_field->is_array = original_field->is_array;
            cloned_field->array_start = original_field->array_start;
            cloned_field->array_end = original_field->array_end;
            cloned_field->array_element_type = original_field->array_element_type;
            cloned_field->array_element_type_id = original_field->array_element_type_id ? 
                strdup(original_field->array_element_type_id) : NULL;
            cloned_field->array_element_type_ref = original_field->array_element_type_ref != NULL
                ? type_ref_clone(original_field->array_element_type_ref) : NULL;
            cloned_field->array_is_open = original_field->array_is_open;
            cloned_field->is_hidden = original_field->is_hidden;
            cloned_field->is_class_var = original_field->is_class_var;
            cloned_field->is_pointer = original_field->is_pointer;
            cloned_field->pointer_type = original_field->pointer_type;
            cloned_field->pointer_type_id = original_field->pointer_type_id ?
                strdup(original_field->pointer_type_id) : NULL;
            cloned_field->pointer_type_ref = original_field->pointer_type_ref != NULL
                ? type_ref_clone(original_field->pointer_type_ref) : NULL;
            /* array_element_record is zeroed by calloc — not cloned to avoid double-free */
            cloned_field->enum_literals = NULL;
            
            /* Create list node for cloned field */
            ListNode_t *new_node = (ListNode_t *)malloc(sizeof(ListNode_t));
            if (new_node == NULL)
            {
                free(cloned_field->name);
                free(cloned_field->type_id);
                if (cloned_field->type_ref != NULL)
                    type_ref_free(cloned_field->type_ref);
                free(cloned_field->array_element_type_id);
                if (cloned_field->array_element_type_ref != NULL)
                    type_ref_free(cloned_field->array_element_type_ref);
                if (cloned_field->pointer_type_ref != NULL)
                    type_ref_free(cloned_field->pointer_type_ref);
                if (cloned_field->proc_type != NULL)
                    kgpc_type_release(cloned_field->proc_type);
                free(cloned_field);
                
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    free(field->array_element_type_id);
                    if (field->proc_type != NULL)
                        kgpc_type_release(field->proc_type);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            new_node->type = LIST_RECORD_FIELD;
            new_node->cur = cloned_field;
            new_node->next = NULL;
            
            /* Append to cloned list */
            if (cloned_parent_fields == NULL)
            {
                cloned_parent_fields = new_node;
                last_cloned = new_node;
            }
            else
            {
                last_cloned->next = new_node;
                last_cloned = new_node;
            }
            
            cur = cur->next;
        }
        
        /* Prepend cloned parent fields to this record's fields */
        if (last_cloned != NULL)
        {
            last_cloned->next = record_info->fields;
            record_info->fields = cloned_parent_fields;
        }
    }

    if (parent_record != NULL && parent_record->properties != NULL)
    {
        ListNode_t *cloned_properties = clone_property_list_unique(record_info,
            parent_record->properties);
        if (cloned_properties != NULL)
            record_info->properties = ConcatList(cloned_properties, record_info->properties);
    }

    /* Detect default indexed property after merging parent fields */
    detect_default_indexed_property(record_info, parent_record);

    record_info->parent_fields_merged = 1;

    /* Class instances are accessed via pointers; parent storage need not grow.
     * Only ensure capacity for non-class (plain record) inheritance. */
    if (parent_record != NULL && !record_info->is_class)
    {
        long long derived_size = 0;
        if (compute_class_record_size(symtab, record_info, &derived_size, line_num) == 0)
            ensure_class_storage_capacity(symtab, parent_record, derived_size, line_num);
    }

    return 0;
}

/* Build Virtual Method Table for a class */
static char *semcheck_param_sig_from_params(ListNode_t *params, int skip_first_param);
static int build_class_vmt(SymTab_t *symtab, struct RecordType *record_info, 
                            const char *class_name, int line_num) {
    if (record_info == NULL || class_name == NULL)
        return 0;
    
    /* Get methods registered for this class.
     * For nested types (e.g., TMarshaller.TDeferBase), methods may have been registered
     * under the unqualified name before renaming. Fall back to unqualified name if needed. */
    ListNode_t *class_methods = NULL;
    int method_count = 0;
    get_class_methods(class_name, &class_methods, &method_count);
    if (method_count == 0 && record_info->type_id != NULL) {
        /* For nested types (e.g. TMarshaller.TDeferBase), methods may be
         * registered under the unqualified type_id. */
        get_class_methods(record_info->type_id, &class_methods, &method_count);
    }

    /* Start with parent's VMT if this class has a parent */
    ListNode_t *vmt = NULL;
    int vmt_size = 0;

if (record_info->parent_class_name != NULL) {
        /* Look up parent class */
        HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record_info->parent_class_name);
        if (parent_node != NULL) {
            struct RecordType *parent_record = get_record_type_from_node(parent_node);
            /* If parent has method bindings but VMT not built yet, build it first */
            if (parent_record != NULL && parent_record->methods == NULL) {
                ListNode_t *parent_bindings = NULL;
                int parent_binding_count = 0;
                const char *parent_name = record_info->parent_class_name;
                /* Look up the parent's type_id from its record if available */
                if (parent_record->type_id != NULL)
                    parent_name = parent_record->type_id;
                get_class_methods(parent_name, &parent_bindings, &parent_binding_count);
                if (parent_binding_count > 0) {
                    /* Parent has methods but VMT not built — build it now */
                    build_class_vmt(symtab, parent_record, parent_name, line_num);
                }
                DestroyList(parent_bindings);
            }
            if (parent_record != NULL && parent_record->methods != NULL) {
                /* Clone parent's VMT */
                ListNode_t *parent_vmt = parent_record->methods;
                ListNode_t **tail = &vmt;
                
                while (parent_vmt != NULL) {
                    struct MethodInfo *parent_method = (struct MethodInfo *)parent_vmt->cur;
                    if (parent_method != NULL) {
                        struct MethodInfo *cloned = (struct MethodInfo *)malloc(sizeof(struct MethodInfo));
                        if (cloned != NULL) {
                            cloned->name = parent_method->name ? strdup(parent_method->name) : NULL;
                            cloned->mangled_name = parent_method->mangled_name ? strdup(parent_method->mangled_name) : NULL;
                            cloned->is_virtual = parent_method->is_virtual;
                            cloned->is_override = 0;  /* Parent's methods aren't overrides in child */
                            cloned->vmt_index = parent_method->vmt_index;
                            cloned->param_count = parent_method->param_count;
                            cloned->param_sig = parent_method->param_sig ? strdup(parent_method->param_sig) : NULL;
                            cloned->resolved_mangled_id = parent_method->resolved_mangled_id ? strdup(parent_method->resolved_mangled_id) : NULL;
                            
                            ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
                            if (node != NULL) {
                                node->type = LIST_UNSPECIFIED;
                                node->cur = cloned;
                                node->next = NULL;
                                *tail = node;
                                tail = &node->next;
                                vmt_size++;
                            } else {
                                free(cloned->name);
                                free(cloned->mangled_name);
                                free(cloned->param_sig);
                                free(cloned);
                            }
                        }
                    }
                    parent_vmt = parent_vmt->next;
                }
            }
        }
    }
    
    /* Process this class's methods */
    ListNode_t *cur_method = class_methods;
    while (cur_method != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur_method->cur;
        if (binding != NULL && binding->method_name != NULL) {
            /* Resolve the full mangled name for this method overload when possible. */
            size_t class_len = strlen(class_name);
            size_t method_len = strlen(binding->method_name);
            char *base_name = (char *)malloc(class_len + 2 + method_len + 1);
            if (base_name != NULL) {
                snprintf(base_name, class_len + 2 + method_len + 1, "%s__%s",
                         class_name, binding->method_name);
            }
            char *mangled = NULL;
            if (base_name != NULL && symtab != NULL)
            {
                ListNode_t *matches = FindAllIdents(symtab, base_name);
                HashNode_t *best = NULL;
                if (binding->param_sig != NULL)
                {
                    for (ListNode_t *m = matches; m != NULL; m = m->next)
                    {
                        HashNode_t *cand = (HashNode_t *)m->cur;
                        if (cand == NULL || cand->type == NULL ||
                            cand->type->kind != TYPE_KIND_PROCEDURE)
                            continue;
                        int skip_self = (!binding->is_static);
                        char *sig = semcheck_param_sig_from_params(
                            cand->type->info.proc_info.params, skip_self);
                        if (sig != NULL && strcmp(sig, binding->param_sig) == 0)
                        {
                            best = cand;
                            free(sig);
                            break;
                        }
                        if (sig != NULL)
                            free(sig);
                    }
                }
                if (best == NULL && binding->param_count >= 0)
                {
                    int match_count = 0;
                    HashNode_t *last_match = NULL;
                    for (ListNode_t *m = matches; m != NULL; m = m->next)
                    {
                        HashNode_t *cand = (HashNode_t *)m->cur;
                        if (cand == NULL || cand->type == NULL ||
                            cand->type->kind != TYPE_KIND_PROCEDURE)
                            continue;
                        int count = ListLength(cand->type->info.proc_info.params);
                        if (!binding->is_static && count > 0)
                            count -= 1; /* drop implicit Self */
                        if (count == binding->param_count)
                        {
                            match_count++;
                            last_match = cand;
                        }
                    }
                    if (match_count == 1)
                        best = last_match;
                }
                if (best != NULL) {
                    /* Prefer definition's mangled_id: declaration may have wrong type
                     * resolution (e.g., PByte → _i_ before type resolution completes) */
                    const char *chosen_id = best->mangled_id;
                    if (best->type != NULL && best->type->info.proc_info.definition != NULL) {
                        Tree_t *bdef = best->type->info.proc_info.definition;
                        if (bdef->tree_data.subprogram_data.mangled_id != NULL)
                            chosen_id = bdef->tree_data.subprogram_data.mangled_id;
                    }
                    if (chosen_id != NULL)
                        mangled = strdup(chosen_id);
                }
                DestroyList(matches);
            }
            if (mangled == NULL && base_name != NULL)
                mangled = strdup(base_name);
            if (base_name != NULL)
                free(base_name);
            
            /* Check if this method overrides a parent method */
            int is_actual_override = 0;
            if (binding->is_virtual || binding->is_override) {
                /* Check if a method with this name/signature exists in the parent VMT */
                ListNode_t *vmt_entry = vmt;
                while (vmt_entry != NULL) {
                    struct MethodInfo *info = (struct MethodInfo *)vmt_entry->cur;
                    if (info != NULL && info->name != NULL &&
                        strcasecmp(info->name, binding->method_name) == 0) {
                        int signature_matches = 0;
                        if (binding->param_sig != NULL && info->param_sig != NULL) {
                            if (strcmp(binding->param_sig, info->param_sig) == 0)
                                signature_matches = 1;
                        } else if (binding->param_count >= 0 && info->param_count >= 0) {
                            if (binding->param_count == info->param_count)
                                signature_matches = 1;
                        } else {
                            signature_matches = 1;
                        }
                        if (!signature_matches) {
                            vmt_entry = vmt_entry->next;
                            continue;
                        }
                        /* Method exists in parent - this is an override */
                        is_actual_override = 1;
                        /* Replace with derived class's version */
                        free(info->mangled_name);
                        info->mangled_name = mangled ? strdup(mangled) : NULL;
                        /* Clear resolved_mangled_id so the resolve phase re-resolves
                         * for the overriding class (the cloned parent value is stale) */
                        if (info->resolved_mangled_id != NULL) {
                            free(info->resolved_mangled_id);
                            info->resolved_mangled_id = NULL;
                        }
                        info->is_override = 1;
                        if (info->param_count < 0 && binding->param_count >= 0)
                            info->param_count = binding->param_count;
                        if (info->param_sig == NULL && binding->param_sig != NULL)
                            info->param_sig = strdup(binding->param_sig);
                        break;
                    }
                    vmt_entry = vmt_entry->next;
                }
            }
            
            if (is_actual_override) {
                /* Method was found and replaced in parent VMT - nothing more to do */
            } else if (binding->is_virtual || binding->is_override) {
                /* Add new virtual method to VMT */
                /* Note: if binding->is_override is true but no parent method was found,
                 * this could be because the parent's VMT was incompletely built
                 * (e.g., FPC system unit with unsupported features).
                 * Treat as a new virtual method instead of an error. */
                
                struct MethodInfo *new_method = (struct MethodInfo *)malloc(sizeof(struct MethodInfo));
                if (new_method != NULL) {
                    new_method->name = binding->method_name ? strdup(binding->method_name) : NULL;
                    new_method->mangled_name = mangled ? strdup(mangled) : NULL;
                    new_method->is_virtual = 1;
                    new_method->is_override = 0;
                    new_method->param_count = binding->param_count;
                    new_method->param_sig = binding->param_sig ? strdup(binding->param_sig) : NULL;
                    new_method->resolved_mangled_id = NULL;
                    /* FPC VMT has 12 metadata slots (96 bytes) before virtual methods:
                     * vInstanceSize, vInstanceSize2, vParentRef, vClassName,
                     * vDynamicTable, vMethodTable, vFieldTable, vTypeInfo,
                     * vInitTable, vAutoTable, vIntfTable, vMsgStrPtr.
                     * Virtual methods start at offset 96 (slot 12). */
                    new_method->vmt_index = vmt_size + 12;

                    ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
                    if (node != NULL) {
                        node->type = LIST_UNSPECIFIED;
                        node->cur = new_method;
                        node->next = NULL;
                        
                        /* Append to end */
                        if (vmt == NULL) {
                            vmt = node;
                        } else {
                            ListNode_t *last = vmt;
                            while (last->next != NULL)
                                last = last->next;
                            last->next = node;
                        }
                        vmt_size++;
                    } else {
                        free(new_method->name);
                        free(new_method->mangled_name);
                        free(new_method->param_sig);
                        free(new_method);
                    }
                }
            }
            
            free(mangled);
        }
        cur_method = cur_method->next;
    }
    
    /* Store VMT in record */
    record_info->methods = vmt;

    /* Resolve mangled IDs for VMT entries so codegen doesn't need to
     * do symbol table lookups or parse mangled names. */
    for (ListNode_t *vmt_node = vmt; vmt_node != NULL; vmt_node = vmt_node->next) {
        struct MethodInfo *mi = (struct MethodInfo *)vmt_node->cur;
        if (mi == NULL || mi->mangled_name == NULL)
            continue;
        if (mi->resolved_mangled_id != NULL)
            continue;  /* Already resolved (e.g. cloned from parent) */

        /* Primary lookup: direct symbol table lookup by mangled_name */
        HashNode_t *func_sym = NULL;
        if (FindIdent(&func_sym, symtab, mi->mangled_name) == 0 &&
            func_sym != NULL && func_sym->mangled_id != NULL &&
            func_sym->type != NULL &&
            func_sym->type->kind == TYPE_KIND_PROCEDURE &&
            func_sym->type->info.proc_info.definition != NULL) {
            mi->resolved_mangled_id = strdup(func_sym->mangled_id);
            continue;
        }

        /* Fallback: search by class_name + method_name with param matching */
        if (mi->name == NULL)
            continue;
        size_t base_len = strlen(class_name) + 2 + strlen(mi->name) + 1;
        char *base_name = (char *)malloc(base_len);
        if (base_name == NULL)
            continue;
        snprintf(base_name, base_len, "%s__%s", class_name, mi->name);
        ListNode_t *matches = FindAllIdents(symtab, base_name);

        /* Priority 0: exact mangled_id match — only for defined (non-abstract) functions
         * to avoid pointing VMT slots to labels that are never emitted */
        for (ListNode_t *m = matches; m != NULL && mi->resolved_mangled_id == NULL; m = m->next) {
            HashNode_t *cand = (HashNode_t *)m->cur;
            if (cand == NULL || cand->mangled_id == NULL ||
                cand->type == NULL || cand->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            if (strcmp(cand->mangled_id, mi->mangled_name) == 0 &&
                cand->type->info.proc_info.definition != NULL)
                mi->resolved_mangled_id = strdup(cand->mangled_id);
        }

        if (mi->resolved_mangled_id == NULL) {
        const char *unique_mangled = NULL;
        const char *def_candidate_mangled = NULL;
        const char *single_mangled = NULL;
        int unique_ok = 1;
        int match_count = 0;
        int def_match_count = 0;
        int exact_name_match = 0; /* mi->mangled_name exactly matched a passing candidate */
        for (ListNode_t *m = matches; m != NULL; m = m->next) {
            HashNode_t *cand = (HashNode_t *)m->cur;
            if (cand == NULL || cand->type == NULL ||
                cand->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            /* Match by param signature if available */
            if (mi->param_sig != NULL) {
                char *cand_sig = semcheck_param_sig_from_params(
                    cand->type->info.proc_info.params, 1);
                int sig_match = (cand_sig != NULL && strcmp(cand_sig, mi->param_sig) == 0);
                free(cand_sig);
                if (!sig_match)
                    continue;
            }
            /* Match by param count */
            int count = ListLength(cand->type->info.proc_info.params);
            if (count > 0) count -= 1; /* drop implicit Self */
            if (mi->param_count >= 0 && count != mi->param_count)
                continue;

            match_count++;
            /* Track whether Phase-1 mangled_name exactly matches this candidate.
             * Phase 1 uses param_sig to find the correct declaration; if it found
             * mi->mangled_name, we should trust that label for the VMT slot. */
            if (cand->mangled_id != NULL && mi->mangled_name != NULL &&
                strcmp(cand->mangled_id, mi->mangled_name) == 0)
                exact_name_match = 1;
            const char *cand_mangled = cand->mangled_id;
            if (cand->type->info.proc_info.definition != NULL) {
                Tree_t *def = cand->type->info.proc_info.definition;
                if (def != NULL &&
                    def->tree_data.subprogram_data.mangled_id != NULL)
                    cand_mangled = def->tree_data.subprogram_data.mangled_id;
                def_match_count++;
                def_candidate_mangled = cand_mangled;
            }
            if (cand_mangled != NULL) {
                if (unique_mangled == NULL)
                    unique_mangled = cand_mangled;
                else if (strcmp(unique_mangled, cand_mangled) != 0)
                    unique_ok = 0;
            } else {
                unique_ok = 0;
            }
            if (match_count == 1)
                single_mangled = (cand->type->info.proc_info.definition != NULL)
                    ? cand_mangled : NULL;
        }
        /* If Phase 1 identified mi->mangled_name via param_sig matching, and that
         * name exists as a candidate, and there is at least one definition for this
         * method (possibly under a stale mangled_id from early type resolution),
         * trust mi->mangled_name — the implementation will be emitted under that label
         * once semcheck_subprograms processes it with fully resolved types. */
        if (exact_name_match && def_match_count > 0 && mi->mangled_name != NULL)
            mi->resolved_mangled_id = strdup(mi->mangled_name);
        else if (unique_ok && unique_mangled != NULL && def_match_count > 0)
            mi->resolved_mangled_id = strdup(unique_mangled);
        else if (def_match_count == 1 && def_candidate_mangled != NULL)
            mi->resolved_mangled_id = strdup(def_candidate_mangled);
        else if (match_count == 1 && single_mangled != NULL)
            mi->resolved_mangled_id = strdup(single_mangled);
        } /* end if (mi->resolved_mangled_id == NULL) */
        if (matches != NULL)
            DestroyList(matches);
        free(base_name);
    }
    
    
    /* Clean up class_methods list (shallow - we don't own the bindings) */
    while (class_methods != NULL) {
        ListNode_t *next = class_methods->next;
        free(class_methods);
        class_methods = next;
    }
    
    return 0;
}

static int semcheck_template_matches_binding(const struct MethodTemplate *tmpl,
    const ClassMethodBinding *binding)
{
    if (tmpl == NULL || binding == NULL || tmpl->name == NULL || binding->method_name == NULL)
        return 0;
    if (strcasecmp(binding->method_name, tmpl->name) != 0)
        return 0;

    int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
    if (wanted_params >= 0 && binding->param_count >= 0)
        return wanted_params == binding->param_count;
    return 1;
}

static int semcheck_template_matches_vmt_method(const struct MethodTemplate *tmpl,
    const struct MethodInfo *mi)
{
    if (tmpl == NULL || mi == NULL || tmpl->name == NULL || mi->name == NULL)
        return 0;
    if (strcasecmp(mi->name, tmpl->name) != 0)
        return 0;

    int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
    if (wanted_params >= 0 && mi->param_count >= 0)
        return wanted_params == mi->param_count;
    return 1;
}

static void semcheck_refresh_generic_specialization_vmts(SymTab_t *symtab,
    ListNode_t *type_decls)
{
    if (symtab == NULL || type_decls == NULL)
        return;

    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;

        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_TYPE_DECL)
            continue;

        struct RecordType *record_info = NULL;
        const char *class_name = tree->tree_data.type_decl_data.id;

        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
            struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
            if (alias->inline_record_type != NULL)
                record_info = alias->inline_record_type;
            else if (tree->tree_data.type_decl_data.kgpc_type != NULL) {
                KgpcType *alias_type = tree->tree_data.type_decl_data.kgpc_type;
                if (kgpc_type_is_record(alias_type))
                    record_info = kgpc_type_get_record(alias_type);
                else if (kgpc_type_is_pointer(alias_type) &&
                         alias_type->info.points_to != NULL &&
                         kgpc_type_is_record(alias_type->info.points_to))
                    record_info = kgpc_type_get_record(alias_type->info.points_to);
            }
        }

        if (record_info == NULL || !record_info->is_class ||
            record_info->method_templates == NULL || class_name == NULL)
            continue;

        for (ListNode_t *tmpl_cur = record_info->method_templates;
             tmpl_cur != NULL; tmpl_cur = tmpl_cur->next) {
            if (tmpl_cur->type != LIST_METHOD_TEMPLATE || tmpl_cur->cur == NULL)
                continue;

            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_cur->cur;
            if (tmpl->name == NULL)
                continue;

            ListNode_t *bindings = NULL;
            int method_count = 0;
            int already_registered = 0;
            get_class_methods(class_name, &bindings, &method_count);
            for (ListNode_t *bcur = bindings; bcur != NULL; bcur = bcur->next) {
                ClassMethodBinding *binding = (ClassMethodBinding *)bcur->cur;
                if (semcheck_template_matches_binding(tmpl, binding)) {
                    already_registered = 1;
                    break;
                }
            }
            if (bindings != NULL)
                DestroyList(bindings);

            if (!already_registered) {
                from_cparser_register_method_template(
                    class_name,
                    tmpl->name,
                    tmpl->is_virtual,
                    tmpl->is_override,
                    tmpl->is_static,
                    from_cparser_count_params_ast(tmpl->params_ast));
            }
        }

        build_class_vmt(symtab, record_info, class_name, tree->line_num);

        for (ListNode_t *tmpl_cur = record_info->method_templates;
             tmpl_cur != NULL; tmpl_cur = tmpl_cur->next) {
            if (tmpl_cur->type != LIST_METHOD_TEMPLATE || tmpl_cur->cur == NULL)
                continue;

            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_cur->cur;
            if (tmpl->name == NULL || (!tmpl->is_virtual && !tmpl->is_override))
                continue;

            size_t base_len = strlen(class_name) + 2 + strlen(tmpl->name) + 1;
            char *base_name = (char *)malloc(base_len);
            if (base_name == NULL)
                continue;
            snprintf(base_name, base_len, "%s__%s", class_name, tmpl->name);

            const char *resolved_id = NULL;
            int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
            ListNode_t *matches = FindAllIdents(symtab, base_name);
            for (ListNode_t *m = matches; m != NULL; m = m->next) {
                HashNode_t *cand = (HashNode_t *)m->cur;
                if (cand == NULL || cand->type == NULL ||
                    cand->type->kind != TYPE_KIND_PROCEDURE)
                    continue;
                if (cand->type->info.proc_info.definition == NULL)
                    continue;
                int count = ListLength(cand->type->info.proc_info.params);
                if (!tmpl->is_static && count > 0)
                    count -= 1;
                if (count != wanted_params)
                    continue;
                resolved_id = cand->mangled_id;
                if (cand->type->info.proc_info.definition != NULL &&
                    cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id != NULL)
                    resolved_id = cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id;
                break;
            }
            if (matches != NULL)
                DestroyList(matches);
            free(base_name);

            if (resolved_id == NULL)
                continue;

            for (ListNode_t *vmt_node = record_info->methods; vmt_node != NULL; vmt_node = vmt_node->next) {
                struct MethodInfo *mi = (struct MethodInfo *)vmt_node->cur;
                if (!semcheck_template_matches_vmt_method(tmpl, mi))
                    continue;
                free(mi->mangled_name);
                mi->mangled_name = strdup(resolved_id);
                if (mi->resolved_mangled_id != NULL)
                    free(mi->resolved_mangled_id);
                mi->resolved_mangled_id = strdup(resolved_id);
                mi->is_override = 1;
                break;
            }
        }
    }
}

/* Helper function to resolve constant identifier to integer value
 * Returns 0 on success, 1 on failure */
static int resolve_const_identifier(SymTab_t *symtab, const char *id, long long *out_value)
{
    if (symtab == NULL || id == NULL || out_value == NULL)
        return 1;
    
    HashNode_t *node = NULL;
    if (FindIdent(&node, symtab, id) >= 0 && 
        node != NULL && (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
    {
        *out_value = node->const_int_value;
        return 0;
    }
    
    return 1;
}

static int semcheck_try_resolve_enum_literal_by_base(SymTab_t *symtab, const char *base_name,
    const char *literal_name, long long *out_value)
{
    if (symtab == NULL || base_name == NULL || literal_name == NULL || out_value == NULL)
        return 0;

    ListNode_t *matches = FindAllIdents(symtab, base_name);
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE && node->type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
            {
                int ordinal = 0;
                ListNode_t *lit = alias->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL &&
                        pascal_identifier_equals((char *)lit->cur, literal_name))
                    {
                        *out_value = ordinal;
                        if (matches != NULL)
                            DestroyList(matches);
                        return 1;
                    }
                    ++ordinal;
                    lit = lit->next;
                }
            }
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return 0;
}

static char *semcheck_append_param_sig(char *sig, const char *type_str)
{
    const char *part = (type_str != NULL) ? type_str : "<unknown>";
    size_t part_len = strlen(part);
    if (sig == NULL)
    {
        char *out = (char *)malloc(part_len + 1);
        if (out == NULL)
            return NULL;
        memcpy(out, part, part_len + 1);
        return out;
    }
    size_t sig_len = strlen(sig);
    char *out = (char *)realloc(sig, sig_len + 1 + part_len + 1);
    if (out == NULL)
    {
        free(sig);
        return NULL;
    }
    out[sig_len] = ',';
    memcpy(out + sig_len + 1, part, part_len + 1);
    return out;
}

static char *semcheck_param_sig_from_params(ListNode_t *params, int skip_first_param)
{
    if (params == NULL)
        return NULL;

    char *sig = NULL;
    ListNode_t *cur = params;
    int skipped = 0;
    while (cur != NULL)
    {
        Tree_t *param = (Tree_t *)cur->cur;
        cur = cur->next;
        if (skip_first_param && !skipped)
        {
            skipped = 1;
            continue;
        }
        if (param == NULL)
            continue;

        char *type_str = NULL;
        int name_count = 1;
        if (param->type == TREE_VAR_DECL)
        {
            if (param->tree_data.var_decl_data.type_ref != NULL)
                type_str = type_ref_render_mangled(param->tree_data.var_decl_data.type_ref);
            else if (param->tree_data.var_decl_data.type_id != NULL)
                type_str = strdup(param->tree_data.var_decl_data.type_id);
            if (param->tree_data.var_decl_data.ids != NULL)
                name_count = ListLength(param->tree_data.var_decl_data.ids);
        }
        else if (param->type == TREE_ARR_DECL)
        {
            if (param->tree_data.arr_decl_data.type_ref != NULL)
                type_str = type_ref_render_mangled(param->tree_data.arr_decl_data.type_ref);
            else if (param->tree_data.arr_decl_data.type_id != NULL)
                type_str = strdup(param->tree_data.arr_decl_data.type_id);
            if (param->tree_data.arr_decl_data.ids != NULL)
                name_count = ListLength(param->tree_data.arr_decl_data.ids);
        }

        if (name_count <= 0)
            name_count = 1;
        for (int i = 0; i < name_count; i++)
            sig = semcheck_append_param_sig(sig, type_str);

        if (type_str != NULL)
            free(type_str);
    }

    return sig;
}

int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name,
    const char *literal_name, long long *out_value)
{
    if (symtab == NULL || type_name == NULL || literal_name == NULL || out_value == NULL)
        return 0;

    QualifiedIdent *qualified_ref = qualified_ident_from_dotted(type_name);
    if (qualified_ref != NULL && qualified_ref->count > 1)
    {
        int resolved = semcheck_resolve_scoped_enum_literal_ref(symtab, qualified_ref,
            literal_name, out_value);
        qualified_ident_free(qualified_ref);
        if (resolved)
            return 1;
    }

    const char *current_type = type_name;
    char *owned_type = NULL;
    for (int depth = 0; depth < 8; ++depth)
    {
        HashNode_t *type_node = NULL;
        ListNode_t *matches = FindAllIdents(symtab, current_type);
        HashNode_t *best_type_node = NULL;
        for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
                continue;

            struct TypeAlias *candidate_alias = kgpc_type_get_type_alias(candidate->type);
            int has_enum_literals = (candidate_alias != NULL &&
                candidate_alias->is_enum &&
                candidate_alias->enum_literals != NULL);
            int has_alias_target = (candidate_alias != NULL &&
                (candidate_alias->target_type_ref != NULL ||
                 candidate_alias->target_type_id != NULL));
            int unit_match = (symtab->unit_context > 0 &&
                candidate->source_unit_index == symtab->unit_context);

            if (best_type_node == NULL)
            {
                best_type_node = candidate;
                if (has_enum_literals && unit_match)
                    break;
                continue;
            }

            struct TypeAlias *best_alias = kgpc_type_get_type_alias(best_type_node->type);
            int best_has_enum_literals = (best_alias != NULL &&
                best_alias->is_enum &&
                best_alias->enum_literals != NULL);
            int best_has_alias_target = (best_alias != NULL &&
                (best_alias->target_type_ref != NULL ||
                 best_alias->target_type_id != NULL));
            int best_unit_match = (symtab->unit_context > 0 &&
                best_type_node->source_unit_index == symtab->unit_context);

            if ((has_enum_literals && !best_has_enum_literals) ||
                (unit_match && !best_unit_match) ||
                (has_alias_target && !best_has_alias_target))
            {
                best_type_node = candidate;
                if (has_enum_literals && unit_match)
                    break;
            }
        }
        if (matches != NULL)
            DestroyList(matches);
        type_node = best_type_node;

        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
        {
            QualifiedIdent *qid = qualified_ident_from_dotted(current_type);
            if (qid != NULL && qid->count > 1)
            {
                const char *base_name = qualified_ident_last(qid);
                if (base_name != NULL &&
                    semcheck_try_resolve_enum_literal_by_base(symtab, base_name, literal_name, out_value))
                {
                    qualified_ident_free(qid);
                    if (owned_type != NULL)
                        free(owned_type);
                    return 1;
                }
            }
            if (qid != NULL)
                qualified_ident_free(qid);

            const char *base = semcheck_base_type_name(current_type);
            if (base == NULL || base == current_type ||
                FindIdent(&type_node, symtab, base) < 0 || type_node == NULL ||
                type_node->hash_type != HASHTYPE_TYPE)
            {
                break;
            }
        }

        if (type_node->type == NULL)
            return 0;
        struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
        {
            int ordinal = 0;
            ListNode_t *literal_node = alias->enum_literals;
            while (literal_node != NULL)
            {
                if (literal_node->cur != NULL &&
                    pascal_identifier_equals((char *)literal_node->cur, literal_name))
                {
                    *out_value = ordinal;
                    return 1;
                }
                ++ordinal;
                literal_node = literal_node->next;
            }
            return 0;
        }

        if (alias != NULL && alias->target_type_ref != NULL &&
            alias->target_type_ref->name != NULL)
        {
            char *qualified = qualified_ident_join(alias->target_type_ref->name, ".");
            if (qualified != NULL && !pascal_identifier_equals(qualified, current_type))
            {
                if (owned_type != NULL)
                    free(owned_type);
                owned_type = qualified;
                current_type = owned_type;
                continue;
            }
            free(qualified);
        }

        if (alias == NULL || alias->target_type_id == NULL ||
            pascal_identifier_equals(alias->target_type_id, current_type))
            break;
        current_type = alias->target_type_id;
    }

    if (owned_type != NULL)
        free(owned_type);
    return 0;
}

int semcheck_resolve_scoped_enum_literal_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    const char *literal_name, long long *out_value)
{
    if (symtab == NULL || type_ref == NULL || literal_name == NULL || out_value == NULL)
        return 0;

    const QualifiedIdent *current_ref = type_ref;
    QualifiedIdent *owned_ref = NULL;
    for (int depth = 0; depth < 8; ++depth)
    {
        HashNode_t *type_node = semcheck_find_exact_type_node_for_qid(symtab, current_ref);
        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
            break;

        if (type_node->type == NULL)
            return 0;
        struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
        {
            int ordinal = 0;
            ListNode_t *literal_node = alias->enum_literals;
            while (literal_node != NULL)
            {
                if (literal_node->cur != NULL &&
                    pascal_identifier_equals((char *)literal_node->cur, literal_name))
                {
                    *out_value = ordinal;
                    return 1;
                }
                ++ordinal;
                literal_node = literal_node->next;
            }
            return 0;
        }

        if (alias != NULL && alias->target_type_ref != NULL &&
            alias->target_type_ref->name != NULL &&
            !qualified_ident_equals_ci(alias->target_type_ref->name, current_ref))
        {
            if (owned_ref != NULL)
            {
                qualified_ident_free(owned_ref);
                owned_ref = NULL;
            }
            owned_ref = qualified_ident_clone(alias->target_type_ref->name);
            if (owned_ref == NULL)
                break;
            current_ref = owned_ref;
            continue;
        }

        if (alias != NULL && alias->target_type_ref != NULL &&
            alias->target_type_ref->name != NULL)
        {
            const char *base = qualified_ident_last(alias->target_type_ref->name);
            if (base != NULL &&
                semcheck_try_resolve_enum_literal_by_base(symtab, base, literal_name, out_value))
            {
                if (owned_ref != NULL)
                    qualified_ident_free(owned_ref);
                return 1;
            }
        }

        if (alias == NULL || alias->target_type_id == NULL)
            break;
        if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL)
        {
            char *current_name = qualified_ident_join(current_ref, ".");
            if (current_name != NULL)
            {
                int same_target = pascal_identifier_equals(alias->target_type_id, current_name);
                free(current_name);
                if (same_target)
                    break;
            }
        }
        /* Fall back to string-based resolution for non-structured aliases. */
        if (owned_ref != NULL)
        {
            qualified_ident_free(owned_ref);
            owned_ref = NULL;
        }
        int resolved = semcheck_resolve_scoped_enum_literal(symtab, alias->target_type_id,
            literal_name, out_value);
        return resolved;
    }

    if (owned_ref != NULL)
        qualified_ident_free(owned_ref);
    return 0;
}

static char *build_qualified_identifier_from_expr(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
        return strdup(expr->expr_data.id);
    if (expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
        return NULL;

    char *base = build_qualified_identifier_from_expr(record_expr);
    if (base == NULL)
        return NULL;
    size_t qualified_len = strlen(base) + 1 + strlen(field_id) + 1;
    char *qualified = (char *)malloc(qualified_len);
    if (qualified != NULL)
        snprintf(qualified, qualified_len, "%s.%s", base, field_id);
    free(base);
    return qualified;
}

static QualifiedIdent *build_qualified_ident_from_expr(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID)
    {
        if (expr->id_ref != NULL)
            return qualified_ident_clone(expr->id_ref);
        if (expr->expr_data.id != NULL)
            return qualified_ident_from_single(expr->expr_data.id);
        return NULL;
    }
    if (expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
        return NULL;

    QualifiedIdent *base = build_qualified_ident_from_expr(record_expr);
    if (base == NULL || base->count <= 0 || base->segments == NULL)
    {
        qualified_ident_free(base);
        return NULL;
    }

    int new_count = base->count + 1;
    char **segments = (char **)calloc((size_t)new_count, sizeof(char *));
    if (segments == NULL)
    {
        qualified_ident_free(base);
        return NULL;
    }
    for (int i = 0; i < base->count; ++i)
    {
        if (base->segments[i] != NULL)
            segments[i] = strdup(base->segments[i]);
    }
    segments[new_count - 1] = strdup(field_id);

    QualifiedIdent *out = qualified_ident_from_segments(segments, new_count, 1);
    if (out == NULL)
    {
        for (int i = 0; i < new_count; ++i)
            free(segments[i]);
        free(segments);
    }
    qualified_ident_free(base);
    return out;
}

static void sync_alias_array_bounds(KgpcType *kgpc_type, struct TypeAlias *alias)
{
    if (kgpc_type == NULL || alias == NULL)
        return;
    if (kgpc_type->type_alias != NULL && kgpc_type->type_alias != alias)
    {
        kgpc_type->type_alias->array_start = alias->array_start;
        kgpc_type->type_alias->array_end = alias->array_end;
        kgpc_type->type_alias->is_open_array = alias->is_open_array;
    }
}

/* Resolves array bounds specified as constant identifiers in a KgpcType
 * This is needed because parsing happens before constants are declared */
static void resolve_array_bounds_in_kgpctype(SymTab_t *symtab, KgpcType *kgpc_type, struct TypeAlias *alias)
{
    if (kgpc_type == NULL || alias == NULL || symtab == NULL)
        return;
    
    /* Only process array types */
    if (kgpc_type->kind != TYPE_KIND_ARRAY)
        return;
    
    /* Check if we have array_dimensions with symbolic bounds */
    if (alias->array_dimensions != NULL)
    {
        /* Use structured bounds from parser if available */
        const char *start_str_ref = alias->array_dim_start_str;
        const char *end_str_ref = alias->array_dim_end_str;
        char *start_str_alloc = NULL;
        char *end_str_alloc = NULL;
        if (start_str_ref == NULL || end_str_ref == NULL)
        {
            /* Fallback: parse from dimension string for legacy data */
            ListNode_t *first_dim = alias->array_dimensions;
            if (first_dim != NULL && first_dim->type == LIST_STRING && first_dim->cur != NULL)
            {
                char *dim_str = (char *)first_dim->cur;
                char *separator = strstr(dim_str, "..");
                if (separator != NULL)
                {
                    size_t slen = (size_t)(separator - dim_str);
                    start_str_alloc = strndup(dim_str, slen);
                    end_str_alloc = strdup(separator + 2);
                    start_str_ref = start_str_alloc;
                    end_str_ref = end_str_alloc;
                }
            }
        }
        if (start_str_ref != NULL && end_str_ref != NULL)
        {
            const char *start_str = start_str_ref;
            const char *end_str = end_str_ref;
                    if (kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
                        fprintf(stderr, "[KGPC] array dim start='%s' end='%s'\n",
                            start_str, end_str);

                    /* Trim whitespace */
                    while (*start_str == ' ') start_str++;
                    while (*end_str == ' ') end_str++;
                    
                    /* Try to resolve as constants or parse as integers */
                    long long start_val = 0;
                    long long end_val = 0;
                    int start_resolved = 0;
                    int end_resolved = 0;
                    
                    /* Try constant lookup first */
                    int tmp_val = 0;
                    int tmp_resolved = (resolve_array_bound_expr(symtab, start_str, &tmp_val) == 0);
                    if (resolve_const_identifier(symtab, start_str, &start_val) == 0 || tmp_resolved)
                    {
                        if (tmp_resolved)
                            start_val = tmp_val;
                        start_resolved = 1;
                    }
                    else
                    {
                        /* Try parsing as integer */
                        char *endptr;
                        long val = strtol(start_str, &endptr, 10);
                        if (endptr != start_str && *endptr == '\0')
                        {
                            start_val = val;
                            start_resolved = 1;
                        }
                    }
                    
                    int tmp_end_val = 0;
                    int tmp_end_resolved = (resolve_array_bound_expr(symtab, end_str, &tmp_end_val) == 0);
                    if (resolve_const_identifier(symtab, end_str, &end_val) == 0 || tmp_end_resolved)
                    {
                        if (tmp_end_resolved)
                            end_val = tmp_end_val;
                        end_resolved = 1;
                    }
                    else
                    {
                        /* Try parsing as integer */
                        char *endptr;
                        long val = strtol(end_str, &endptr, 10);
                        if (endptr != end_str && *endptr == '\0')
                        {
                            end_val = val;
                            end_resolved = 1;
                        }
                    }
                    
                    /* Update KgpcType with resolved bounds */
                    if (start_resolved && end_resolved)
                    {
                        kgpc_type->info.array_info.start_index = (int)start_val;
                        kgpc_type->info.array_info.end_index = (int)end_val;
                        
                        /* Also update the TypeAlias for consistency */
                        alias->array_start = (int)start_val;
                        alias->array_end = (int)end_val;
                        
                        /* Re-evaluate is_open_array based on resolved bounds */
                        alias->is_open_array = (alias->array_end < alias->array_start);
                        sync_alias_array_bounds(kgpc_type, alias);
                    }
                    
            free(start_str_alloc);
            free(end_str_alloc);
        }
        else
        {
            /* Single identifier dimension (e.g., array[Boolean] or array[MyEnum]) */
            ListNode_t *first_dim = alias->array_dimensions;
            const char *dim_str = (first_dim != NULL && first_dim->type == LIST_STRING) ?
                (const char *)first_dim->cur : NULL;
            HashNode_t *type_node = NULL;
            if (dim_str != NULL &&
                FindIdent(&type_node, symtab, dim_str) >= 0 && type_node != NULL &&
                    type_node->hash_type == HASHTYPE_TYPE)
                {
                    struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                    if (type_alias != NULL)
                    {
                        if (type_alias->is_enum && type_alias->enum_literals != NULL)
                        {
                            int count = ListLength(type_alias->enum_literals);
                            if (count > 0)
                            {
                                kgpc_type->info.array_info.start_index = 0;
                                kgpc_type->info.array_info.end_index = count - 1;
                                alias->array_start = 0;
                                alias->array_end = count - 1;
                                alias->is_open_array = 0;
                                sync_alias_array_bounds(kgpc_type, alias);
                            }
                        }
                        else if (type_alias->is_range && type_alias->range_known)
                        {
                            kgpc_type->info.array_info.start_index = type_alias->range_start;
                            kgpc_type->info.array_info.end_index = type_alias->range_end;
                            alias->array_start = type_alias->range_start;
                            alias->array_end = type_alias->range_end;
                            alias->is_open_array = 0;
                            sync_alias_array_bounds(kgpc_type, alias);
                        }
                    }
                }
                else if (pascal_identifier_equals(dim_str, "Boolean"))
                {
                    kgpc_type->info.array_info.start_index = 0;
                    kgpc_type->info.array_info.end_index = 1;
                    alias->array_start = 0;
                    alias->array_end = 1;
                    alias->is_open_array = 0;
                    sync_alias_array_bounds(kgpc_type, alias);
                }
                else if (pascal_identifier_equals(dim_str, "Char") ||
                         pascal_identifier_equals(dim_str, "AnsiChar"))
                {
                    kgpc_type->info.array_info.start_index = 0;
                    kgpc_type->info.array_info.end_index = 255;
                    alias->array_start = 0;
                    alias->array_end = 255;
                    alias->is_open_array = 0;
                    sync_alias_array_bounds(kgpc_type, alias);
                }
            }
        }
    }

int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls)
{
    ListNode_t *cur;
    Tree_t *tree;
    int return_val, func_return;
    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = type_decls;
    while(cur != NULL)
    {
        int loop_start_errors = return_val;
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_TYPE_DECL);

        const char *debug_pss = kgpc_getenv("KGPC_DEBUG_PSHORTSTRING");
        if (debug_pss != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
        {
            struct TypeAlias *a = &tree->tree_data.type_decl_data.info.alias;
            fprintf(stderr,
                "[PShortString] decl kind=%d is_pointer=%d pointer_type_id=%s target=%s\n",
                tree->tree_data.type_decl_data.kind,
                a->is_pointer,
                a->pointer_type_id ? a->pointer_type_id : "<null>",
                a->target_type_id ? a->target_type_id : "<null>");
        }
        if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "TFloatFormatProfile"))
        {
            fprintf(stderr, "[TFLOAT] semcheck_type_decls kind=%d defined_in_unit=%d\n",
                tree->tree_data.type_decl_data.kind,
                tree->tree_data.type_decl_data.defined_in_unit);
        }
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_type_decls processing type: %s kind=%d\n",
            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
            tree->tree_data.type_decl_data.kind);
#endif

        const char *debug_env_check = kgpc_getenv("KGPC_DEBUG_TFPG");
        if (debug_env_check != NULL)
        {
            fprintf(stderr, "[KGPC] semcheck_type_decls processing: id=%s kind=%d return_val=%d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, loop_start_errors);
        }

        int before_switch_errors = return_val;

        struct RecordType *record_info = NULL;
        struct TypeAlias *alias_info = NULL;
        

        switch (tree->tree_data.type_decl_data.kind)
        {
            case TYPE_DECL_RECORD:
                var_type = HASHVAR_RECORD;
                record_info = semcheck_record_from_type_decl(tree);

                /* Set the type_id on the RecordType so operator overloading can find it */
                if (record_info != NULL && record_info->type_id == NULL && tree->tree_data.type_decl_data.id != NULL)
                {
                    record_info->type_id = strdup(tree->tree_data.type_decl_data.id);
                }

                semcheck_qualify_nested_types_for_record(symtab, record_info);

                if (record_info != NULL && record_info->is_type_helper)
                    semcheck_register_type_helper(record_info, symtab);

                /* In objfpc mode, classes without explicit parent inherit from TObject,
                 * unless this IS TObject itself (to avoid circular inheritance). */
                if (record_info != NULL && record_info->is_class &&
                    record_info->parent_class_name == NULL &&
                    pascal_frontend_is_objfpc_mode())
                {
                    const char *class_name = tree->tree_data.type_decl_data.id;
                    if (class_name == NULL || strcasecmp(class_name, "TObject") != 0)
                    {
                        record_info->parent_class_name = strdup("TObject");
                    }
                }

                /* Handle class inheritance - merge parent fields */
                if (record_info != NULL && record_info->parent_class_name != NULL)
                {
                    int merge_result = merge_parent_class_fields(symtab, record_info,
                                                                  tree->tree_data.type_decl_data.id,
                                                                  tree->line_num);
                    if (merge_result > 0)
                    {
                        return_val += merge_result;
                        /* Continue processing other type declarations even if this one failed */
                    }
                }
                else if (record_info != NULL && record_info->is_class)
                {
                    /* Classes without parents (like TObject) still need default indexed property detection */
                    detect_default_indexed_property(record_info, NULL);
                }
                else if (record_info != NULL && !record_info->is_class)
                {
                    /* Advanced records can declare default indexed properties too. */
                    detect_default_indexed_property(record_info, NULL);
                }
                
                /* Process method templates and register them with class method bindings.
                 * Methods declared in class type sections (e.g., abstract methods)
                 * may not have been registered during parsing if they were only forward
                 * declarations. We need to ensure they're registered for VMT building. */
                if (record_info->method_templates != NULL)
                {
                    ListNode_t *template_cur = record_info->method_templates;
                    while (template_cur != NULL)
                    {
                        if (template_cur->type == LIST_METHOD_TEMPLATE)
                        {
                            struct MethodTemplate *tmpl = (struct MethodTemplate *)template_cur->cur;
                            if (tmpl != NULL && tmpl->name != NULL)
                            {
                                /* Build mangled name: ClassName__MethodName */
                                size_t class_len = strlen(tree->tree_data.type_decl_data.id);
                                size_t method_len = strlen(tmpl->name);
                                char *mangled = (char *)malloc(class_len + 2 + method_len + 1);
                                if (mangled != NULL)
                                {
                                    snprintf(mangled, class_len + 2 + method_len + 1, "%s__%s",
                                             tree->tree_data.type_decl_data.id, tmpl->name);

                                    /* Check if already registered in class_method_bindings */
                                    ListNode_t *bindings = NULL;
                                    int method_count = 0;
                                    get_class_methods(tree->tree_data.type_decl_data.id, &bindings, &method_count);
                                    
                                    int already_registered = 0;
                                    if (bindings != NULL)
                                    {
                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] Checking %d existing methods for %s\n",
                                                method_count, tree->tree_data.type_decl_data.id);
                                        }
                                        ListNode_t *cur = bindings;
                                        while (cur != NULL)
                                        {
                                            ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
                                            if (binding != NULL && binding->method_name != NULL)
                                            {
                                                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                                    fprintf(stderr, "[SemCheck] Existing method: %s.%s\n",
                                                        binding->class_name, binding->method_name);
                                                }
                                                if (strcasecmp(binding->method_name, tmpl->name) == 0)
                                                {
                                                    already_registered = 1;
                                                    break;
                                                }
                                            }
                                            cur = cur->next;
                                        }
                                    }
                                    else
                                    {
                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] No existing methods found for %s\n",
                                                tree->tree_data.type_decl_data.id);
                                        }
                                    }
                                    DestroyList(bindings);

                                    /* If not already registered, register it now */
                                    if (!already_registered)
                                    {
                                        from_cparser_register_method_template(
                                            tree->tree_data.type_decl_data.id,
                                            tmpl->name,
                                            tmpl->is_virtual,
                                            tmpl->is_override,
                                            tmpl->is_static,
                                            from_cparser_count_params_ast(tmpl->params_ast));

                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] Registered method template: %s.%s (virtual=%d, override=%d, static=%d)\n",
                                                tree->tree_data.type_decl_data.id,
                                                tmpl->name,
                                                tmpl->is_virtual,
                                                tmpl->is_override,
                                                tmpl->is_static);
                                        }
                                    }

                                    /* Add as forward declaration to symbol table (allow overloads). */
                                    HashNode_t *existing = NULL;
                                    ListNode_t *existing_list = FindAllIdents(symtab, mangled);
                                    int has_matching_signature = 0;

                                    ListNode_t *params = NULL;
                                    if (tmpl->params_ast != NULL)
                                        params = from_cparser_convert_params_ast(tmpl->params_ast);
                                    if (!tmpl->is_static &&
                                        tmpl->kind != METHOD_TEMPLATE_OPERATOR)
                                    {
                                        const char *self_type_id = tree->tree_data.type_decl_data.id;
                                        int self_is_var = 1;
                                        if (record_info != NULL && record_info->is_class)
                                        {
                                            /* Class instances are already pointers; Self should not be var. */
                                            self_is_var = 0;
                                        }
                                        if (record_info != NULL && record_info->is_type_helper)
                                        {
                                            if (record_info->helper_base_type_id != NULL)
                                                self_type_id = record_info->helper_base_type_id;
                                            self_is_var = semcheck_helper_self_is_var(symtab, self_type_id);
                                        }
                                        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
                                        if (self_ids != NULL && self_type_id != NULL)
                                        {
                                            Tree_t *self_param = mk_vardecl(0, self_ids, UNKNOWN_TYPE,
                                                strdup(self_type_id), self_is_var, 0, NULL, NULL, NULL, NULL);
                                            ListNode_t *self_node = CreateListNode(self_param, LIST_TREE);
                                            if (self_node != NULL)
                                            {
                                                self_node->next = params;
                                                params = self_node;
                                            }
                                        }
                                    }

                                    if (existing_list != NULL)
                                    {
                                        ListNode_t *cur = existing_list;
                                        while (cur != NULL)
                                        {
                                            HashNode_t *candidate = (HashNode_t *)cur->cur;
                                            if (candidate != NULL && candidate->type != NULL &&
                                                candidate->type->kind == TYPE_KIND_PROCEDURE)
                                            {
                                                if (semcheck_param_list_equivalent(
                                                        candidate->type->info.proc_info.params, params))
                                                {
                                                    existing = candidate;
                                                    has_matching_signature = 1;
                                                    break;
                                                }
                                            }
                                            cur = cur->next;
                                        }
                                    }

                                    if (!has_matching_signature)
                                    {
                                        KgpcType *return_type = NULL;
                                        char *return_type_id = NULL;
                                        ast_t *return_type_node = tmpl->return_type_ast;
                                        if (return_type_node == NULL && tmpl->method_ast != NULL)
                                        {
                                            struct ast_t *method_ast = tmpl->method_ast;
                                            for (ast_t *child = method_ast->child; child != NULL; child = child->next)
                                            {
                                                if (child->typ == PASCAL_T_RETURN_TYPE)
                                                {
                                                    return_type_node = child;
                                                    if (child->sym != NULL && child->sym->name != NULL)
                                                        return_type_id = strdup(child->sym->name);
                                                    break;
                                                }
                                            }
                                        }
                                        int is_function_template = (tmpl->kind == METHOD_TEMPLATE_FUNCTION ||
                                            tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR ||
                                            tmpl->has_return_type ||
                                            return_type_node != NULL);

                                        if (return_type_node != NULL)
                                        {
                                            if (return_type_node->child != NULL)
                                                return_type = convert_type_spec_to_kgpctype(return_type_node->child, symtab);
                                            if (return_type_id == NULL)
                                                return_type_id = semcheck_dup_type_id_from_ast(return_type_node);
                                        }
                                        if (return_type_id != NULL)
                                        {
                                            const char *owner_full = (record_info != NULL && record_info->type_id != NULL)
                                                ? record_info->type_id : tree->tree_data.type_decl_data.id;
                                            const char *owner_outer = (record_info != NULL) ? record_info->outer_type_id : NULL;
                                            if (owner_full != NULL)
                                                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                                    &return_type_id, NULL);
                                        }
                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tmpl->name != NULL &&
                                            (strcasecmp(tmpl->name, "GetAnsiString") == 0 ||
                                             strcasecmp(tmpl->name, "GetString") == 0))
                                        {
                                            fprintf(stderr,
                                                "[SemCheck] tmpl %s return_type_node=%p return_type_id=%s return_type=%p\n",
                                                tmpl->name,
                                                (void *)return_type_node,
                                                return_type_id != NULL ? return_type_id : "<null>",
                                                (void *)return_type);
                                        }
                                        if (return_type == NULL && return_type_id != NULL)
                                        {
                                            HashNode_t *type_node = NULL;
                                            if (FindIdent(&type_node, symtab, return_type_id) != -1 &&
                                                type_node != NULL && type_node->type != NULL)
                                            {
                                                kgpc_type_retain(type_node->type);
                                                return_type = type_node->type;
                                            }
                                            else
                                            {
                                                int tag = semcheck_map_builtin_type_name_local(return_type_id);
                                                if (tag != UNKNOWN_TYPE)
                                                    return_type = create_primitive_type(tag);
                                            }
                                        }

                                        KgpcType *proc_type = create_procedure_type(params, return_type);
                                        /* create_procedure_type retains return_type; release our ref */
                                        if (return_type != NULL)
                                            kgpc_type_release(return_type);
                                        if (proc_type != NULL) {
                                            if (return_type_id != NULL)
                                                proc_type->info.proc_info.return_type_id = strdup(return_type_id);
                                            /* Transfer param ownership to proc_type's shallow copy */
                                            proc_type->info.proc_info.owns_params = 1;
                                        }

                                        if (return_type_id != NULL)
                                            free(return_type_id);

                                        char *overload_mangled = MangleFunctionName(mangled, params, symtab);
                                        if (overload_mangled == NULL)
                                            overload_mangled = strdup(mangled);
                                        /* Free original param list nodes (data owned by proc_type now) */
                                        DestroyList(params);
                                        params = NULL;

                                        if (is_function_template)
                                            PushFunctionOntoScope_Typed(symtab, mangled, overload_mangled, proc_type);
                                        else
                                            PushProcedureOntoScope_Typed(symtab, mangled, overload_mangled, proc_type);

                                        /* Set method identity on the newly-pushed symbol */
                                        {
                                            HashNode_t *pushed_node = NULL;
                                            if (FindIdent(&pushed_node, symtab, mangled) != -1 && pushed_node != NULL)
                                            {
                                                if (pushed_node->method_name == NULL && tmpl->name != NULL)
                                                    pushed_node->method_name = strdup(tmpl->name);
                                                if (pushed_node->owner_class == NULL)
                                                    pushed_node->owner_class = strdup(tree->tree_data.type_decl_data.id);
                                            }
                                        }

                                        if (proc_type != NULL)
                                            destroy_kgpc_type(proc_type);
                                        free(overload_mangled);

                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] Added method forward declaration: %s -> %s\n",
                                                tmpl->name, mangled);
                                        }
                                    }
                                    else
                                    {
                                        int is_function_template = (tmpl->kind == METHOD_TEMPLATE_FUNCTION ||
                                            tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR ||
                                            tmpl->has_return_type ||
                                            tmpl->return_type_ast != NULL);
                                        if (is_function_template && existing != NULL &&
                                            existing->hash_type == HASHTYPE_PROCEDURE)
                                            existing->hash_type = HASHTYPE_FUNCTION;
                                        if (existing != NULL && existing->type != NULL &&
                                            existing->type->kind == TYPE_KIND_PROCEDURE &&
                                            existing->type->info.proc_info.return_type == NULL)
                                        {
                                            KgpcType *return_type = NULL;
                                            char *return_type_id = NULL;
                                        ast_t *return_type_node = tmpl->return_type_ast;
                                        if (return_type_node == NULL && tmpl->method_ast != NULL)
                                        {
                                            struct ast_t *method_ast = tmpl->method_ast;
                                            for (ast_t *child = method_ast->child; child != NULL; child = child->next)
                                            {
                                                if (child->typ == PASCAL_T_RETURN_TYPE)
                                                {
                                                    return_type_node = child;
                                                    if (child->sym != NULL && child->sym->name != NULL)
                                                        return_type_id = strdup(child->sym->name);
                                                    break;
                                                }
                                            }
                                        }

                                        if (return_type_node != NULL)
                                        {
                                            if (return_type_node->child != NULL)
                                                return_type = convert_type_spec_to_kgpctype(return_type_node->child, symtab);
                                            if (return_type_id == NULL)
                                                return_type_id = semcheck_dup_type_id_from_ast(return_type_node);
                                        }
                                        if (return_type_id != NULL)
                                        {
                                            const char *owner_full = (record_info != NULL && record_info->type_id != NULL)
                                                ? record_info->type_id : tree->tree_data.type_decl_data.id;
                                            const char *owner_outer = (record_info != NULL) ? record_info->outer_type_id : NULL;
                                            if (owner_full != NULL)
                                                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                                    &return_type_id, NULL);
                                        }
                                        if (return_type == NULL && return_type_id != NULL)
                                        {
                                            HashNode_t *type_node = NULL;
                                            if (FindIdent(&type_node, symtab, return_type_id) != -1 &&
                                                    type_node != NULL && type_node->type != NULL)
                                                {
                                                    kgpc_type_retain(type_node->type);
                                                    return_type = type_node->type;
                                                }
                                                else
                                                {
                                                    int tag = semcheck_map_builtin_type_name_local(return_type_id);
                                                    if (tag != UNKNOWN_TYPE)
                                                        return_type = create_primitive_type(tag);
                                                }
                                            }

                                            if (return_type != NULL)
                                                existing->type->info.proc_info.return_type = return_type;
                                            if (return_type_id != NULL)
                                                free(return_type_id);
                                        }
                                        if (existing != NULL && existing->type != NULL &&
                                            existing->type->kind == TYPE_KIND_PROCEDURE &&
                                            existing->type->info.proc_info.params == NULL)
                                        {
                                            existing->type->info.proc_info.params = CopyListShallow(params);
                                            /* Transfer Tree_t ownership to existing type's shallow copy */
                                            existing->type->info.proc_info.owns_params = 1;
                                            DestroyList(params);
                                            params = NULL;
                                        }
                                        else if (existing != NULL && existing->type != NULL &&
                                            existing->type->kind == TYPE_KIND_PROCEDURE &&
                                            existing->type->info.proc_info.params != NULL &&
                                            params != NULL)
                                        {
                                            /* Keep defaults declared in the type/template declaration
                                             * when the same signature was already introduced by an
                                             * implementation header that omits them. */
                                            copy_default_values_to_impl_params(
                                                params,
                                                existing->type->info.proc_info.params);
                                            /* params not stored; free everything */
                                            destroy_list(params);
                                            params = NULL;
                                        }
                                        /* Free any remaining params not consumed above */
                                        if (params != NULL)
                                            destroy_list(params);
                                    }

                                    /* Set method identity on existing node */
                                    if (existing != NULL)
                                    {
                                        if (existing->method_name == NULL && tmpl->name != NULL)
                                            existing->method_name = strdup(tmpl->name);
                                        if (existing->owner_class == NULL)
                                            existing->owner_class = strdup(tree->tree_data.type_decl_data.id);
                                    }

                                    if (params != NULL)
                                        DestroyList(params);
                                    if (existing_list != NULL)
                                        DestroyList(existing_list);
                                    free(mangled);
                                }
                            }
                        }
                        template_cur = template_cur->next;
                    }
                }

                /* Interface methods have declarations but no implementations.
                 * Register synthetic symbols so method resolution can succeed. */
                if (record_info != NULL && record_info->is_interface &&
                    record_info->method_templates != NULL &&
                    record_info->type_id != NULL)
                {
                    ListNode_t *tmpl_cur = record_info->method_templates;
                    while (tmpl_cur != NULL)
                    {
                        if (tmpl_cur->type == LIST_METHOD_TEMPLATE)
                        {
                            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_cur->cur;
                            if (tmpl != NULL && tmpl->name != NULL)
                            {
                                size_t class_len = strlen(record_info->type_id);
                                size_t method_len = strlen(tmpl->name);
                                char *mangled = (char *)malloc(class_len + 2 + method_len + 1);
                                if (mangled != NULL)
                                {
                                    snprintf(mangled, class_len + 2 + method_len + 1,
                                        "%s__%s", record_info->type_id, tmpl->name);
                                    HashNode_t *existing = NULL;
                                    if (FindIdent(&existing, symtab, mangled) == -1)
                                    {
                                        KgpcType *proc_type =
                                            from_cparser_method_template_to_proctype(tmpl, record_info, symtab);
                                        if (proc_type != NULL)
                                        {
                                            char *mangled_dup = strdup(mangled);
                                            if (proc_type->info.proc_info.return_type != NULL ||
                                                tmpl->has_return_type)
                                            {
                                                PushFunctionOntoScope_Typed(symtab, mangled, mangled_dup, proc_type);
                                            }
                                            else
                                            {
                                                PushProcedureOntoScope_Typed(symtab, mangled, mangled_dup, proc_type);
                                            }
                                            /* Set method identity on the newly-pushed symbol */
                                            {
                                                HashNode_t *pushed_node = NULL;
                                                if (FindIdent(&pushed_node, symtab, mangled) != -1 && pushed_node != NULL)
                                                {
                                                    if (pushed_node->method_name == NULL && tmpl->name != NULL)
                                                        pushed_node->method_name = strdup(tmpl->name);
                                                    if (pushed_node->owner_class == NULL && record_info->type_id != NULL)
                                                        pushed_node->owner_class = strdup(record_info->type_id);
                                                }
                                            }
                                            mangled = NULL;
                                        }
                                    }
                                    else if (existing != NULL)
                                    {
                                        /* Set method identity on existing interface method node */
                                        if (existing->method_name == NULL && tmpl->name != NULL)
                                            existing->method_name = strdup(tmpl->name);
                                        if (existing->owner_class == NULL && record_info->type_id != NULL)
                                            existing->owner_class = strdup(record_info->type_id);
                                    }
                                    if (mangled != NULL)
                                        free(mangled);
                                }
                            }
                        }
                        tmpl_cur = tmpl_cur->next;
                    }
                }

                /* Now also check if this class has method implementations that need to be added.
                 * These are in the subprogram list (nested_subs of the class type decl) */
                if (tree->tree_data.type_decl_data.info.record != NULL &&
                    tree->tree_data.type_decl_data.info.record->methods != NULL)
                {
                    /* Methods have been processed - nothing more to do */
                }

                /* Build VMT for classes with virtual methods */
                if (record_info != NULL)
                {
                    int vmt_result = build_class_vmt(symtab, record_info,
                                                      tree->tree_data.type_decl_data.id,
                                                      tree->line_num);
                    if (vmt_result > 0)
                    {
                        return_val += vmt_result;
                    }

                    /* Skip size computation for generic templates (not yet specialized)
                     * Size can only be computed after type parameters are substituted */
                    int is_unspecialized_generic = (record_info->generic_decl != NULL && 
                                                     record_info->num_generic_args == 0);
                    
                    if (!is_unspecialized_generic)
                    {
                        long long record_size = 0;
                        int size_result = semcheck_compute_record_size(symtab, record_info, &record_size,
                                tree->line_num);
                        if (size_result != 0)
                        {
#ifdef DEBUG
                            fprintf(stderr, "DEBUG: semcheck_compute_record_size FAILED for %s: result=%d\n",
                                tree->tree_data.type_decl_data.id, size_result);
#endif
                            if (kgpc_getenv("KGPC_DEBUG_SEMSTEPS") != NULL)
                            {
                                fprintf(stderr, "[SemCheck] record size failed for %s (line %d)\n",
                                        tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                                        tree->line_num);
                            }
                            return_val += 1;
                        }
                    }
                }
                break;
            case TYPE_DECL_ALIAS:
            {
                alias_info = &tree->tree_data.type_decl_data.info.alias;
                if (alias_info->alias_name == NULL && tree->tree_data.type_decl_data.id != NULL)
                    alias_info->alias_name = strdup(tree->tree_data.type_decl_data.id);

                if (alias_info->alias_name != NULL &&
                    (pascal_identifier_equals(alias_info->alias_name, "UnicodeString") ||
                     pascal_identifier_equals(alias_info->alias_name, "WideString")))
                {
                    alias_info->is_wide_string = 1;
                }
                else if (alias_info->target_type_id != NULL &&
                         (pascal_identifier_equals(alias_info->target_type_id, "UnicodeString") ||
                          pascal_identifier_equals(alias_info->target_type_id, "WideString")))
                {
                    alias_info->is_wide_string = 1;
                }
                
                alias_info->is_char_alias = semcheck_alias_should_be_char_like(
                    tree->tree_data.type_decl_data.id, alias_info->target_type_id) &&
                    !alias_info->is_pointer && !alias_info->is_array && !alias_info->is_set &&
                    !alias_info->is_enum && !alias_info->is_file;
                if (tree->tree_data.type_decl_data.kgpc_type == NULL &&
                    (alias_info->is_array || alias_info->is_pointer ||
                     alias_info->is_set || alias_info->is_file))
                {
                    KgpcType *alias_type = create_kgpc_type_from_type_alias(
                        alias_info, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                    if (alias_type != NULL)
                    {
                        tree->tree_data.type_decl_data.kgpc_type = alias_type;
                        kgpc_type_retain(alias_type);
                    }
                }
                if (alias_info->inline_record_type != NULL)
                {
                    if (alias_info->inline_record_type->type_id == NULL &&
                        tree->tree_data.type_decl_data.id != NULL)
                    {
                        alias_info->inline_record_type->type_id =
                            strdup(tree->tree_data.type_decl_data.id);
                    }
                    if (tree->tree_data.type_decl_data.kgpc_type == NULL ||
                        (record_type_is_class(alias_info->inline_record_type) &&
                         !kgpc_type_is_pointer(tree->tree_data.type_decl_data.kgpc_type)))
                    {
                        KgpcType *inline_kgpc = create_record_type(alias_info->inline_record_type);
                        if (record_type_is_class(alias_info->inline_record_type))
                            inline_kgpc = create_pointer_type(inline_kgpc);
                        kgpc_type_set_type_alias(inline_kgpc, alias_info);
                        if (tree->tree_data.type_decl_data.kgpc_type != NULL)
                            destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                        tree->tree_data.type_decl_data.kgpc_type = inline_kgpc;
                        kgpc_type_retain(inline_kgpc);
                    }
                }
                if (alias_info->is_pointer &&
                    tree->tree_data.type_decl_data.kgpc_type != NULL &&
                    kgpc_type_is_pointer(tree->tree_data.type_decl_data.kgpc_type) &&
                    tree->tree_data.type_decl_data.kgpc_type->info.points_to == NULL)
                {
                    HashNode_t *pointee_node = NULL;
                    if (alias_info->pointer_type_ref != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node_with_ref(symtab,
                            alias_info->pointer_type_ref, alias_info->pointer_type_id);
                    }
                    else if (alias_info->pointer_type_id != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node(symtab,
                            alias_info->pointer_type_id);
                    }
                    if (pointee_node != NULL && pointee_node->type != NULL)
                    {
                        kgpc_type_retain(pointee_node->type);
                        tree->tree_data.type_decl_data.kgpc_type->info.points_to = pointee_node->type;
                    }
                }
                if (kgpc_getenv("KGPC_DEBUG_PROC_TYPE") != NULL &&
                    tree->tree_data.type_decl_data.id != NULL &&
                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "TCreateEncodingProc"))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_PROC_TYPE] alias=%s base=%d kgpc=%s kind=%d return_id=%s\n",
                        tree->tree_data.type_decl_data.id,
                        alias_info->base_type,
                        tree->tree_data.type_decl_data.kgpc_type ?
                            kgpc_type_to_string(tree->tree_data.type_decl_data.kgpc_type) : "<null>",
                        tree->tree_data.type_decl_data.kgpc_type ?
                            tree->tree_data.type_decl_data.kgpc_type->kind : -1,
                        (tree->tree_data.type_decl_data.kgpc_type &&
                         tree->tree_data.type_decl_data.kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                         tree->tree_data.type_decl_data.kgpc_type->info.proc_info.return_type_id) ?
                            tree->tree_data.type_decl_data.kgpc_type->info.proc_info.return_type_id : "<null>");
                }
                if (alias_info->is_array)
                {
                    int element_type = alias_info->array_element_type;
                    if (is_real_family_type(element_type))
                        var_type = HASHVAR_REAL;
                    else if (element_type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if (element_type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if (element_type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if (element_type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if (element_type == POINTER_TYPE)
                        var_type = HASHVAR_POINTER;
                    else if (element_type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if (element_type == ENUM_TYPE)
                        var_type = HASHVAR_ENUM;
                    else if (element_type == FILE_TYPE)
                        var_type = HASHVAR_FILE;
                    else if (element_type == TEXT_TYPE)
                        var_type = HASHVAR_TEXT;
                    else
                        var_type = HASHVAR_INTEGER;
                }
                else
                {
                    int base_type = alias_info->base_type;
                    if (is_real_family_type(base_type))
                        var_type = HASHVAR_REAL;
                    else if (base_type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if (base_type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if (base_type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if (base_type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if (base_type == POINTER_TYPE)
                        var_type = HASHVAR_POINTER;
                    else if (base_type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if (base_type == ENUM_TYPE)
                        var_type = HASHVAR_ENUM;
                    else if (base_type == FILE_TYPE)
                        var_type = HASHVAR_FILE;
                    else if (base_type == TEXT_TYPE)
                        var_type = HASHVAR_TEXT;
                    else if (base_type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if (base_type == PROCEDURE)
                        var_type = HASHVAR_PROCEDURE;
                    else
                        var_type = HASHVAR_UNTYPED;

                    if (alias_info->is_pointer)
                        var_type = HASHVAR_POINTER;
                    else if (alias_info->is_set)
                        var_type = HASHVAR_SET;
                    else if (alias_info->is_enum)
                        var_type = HASHVAR_ENUM;
                    else if (alias_info->is_file)
                        var_type = HASHVAR_FILE;

                    if (var_type == HASHVAR_UNTYPED &&
                        (alias_info->target_type_ref != NULL || alias_info->target_type_id != NULL))
                    {
                        HashNode_t *target_node = NULL;
                        if (alias_info->target_type_ref != NULL)
                            target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                                alias_info->target_type_ref, alias_info->target_type_id);
                        else
                            target_node = semcheck_find_preferred_type_node(symtab,
                                alias_info->target_type_id);

                        if (target_node != NULL && target_node->type != NULL)
                        {
                            struct TypeAlias *target_alias = kgpc_type_get_type_alias(target_node->type);
                            if (target_alias == alias_info)
                            {
                                char *lookup = NULL;
                                if (alias_info->target_type_ref != NULL)
                                    lookup = type_ref_render_mangled(alias_info->target_type_ref);
                                const char *name = (lookup != NULL) ? lookup : alias_info->target_type_id;
                                HashNode_t *alt = semcheck_find_type_excluding_alias(symtab, name, alias_info);
                                if (alt != NULL)
                                    target_node = alt;
                                free(lookup);
                            }
                        }

                        if (target_node != NULL)
                        {
                            if (alias_info->target_type_ref != NULL &&
                                semcheck_is_explicit_unit_qualified_type_ref(alias_info->target_type_ref) &&
                                target_node->hash_type == HASHTYPE_TYPE &&
                                !target_node->defined_in_unit)
                            {
                                const char *unit_name =
                                    (alias_info->target_type_ref->name != NULL &&
                                     alias_info->target_type_ref->name->segments != NULL &&
                                     alias_info->target_type_ref->name->count > 0)
                                        ? alias_info->target_type_ref->name->segments[0]
                                        : NULL;
                                const char *base_name = type_ref_base_name(alias_info->target_type_ref);
                                if (base_name != NULL)
                                {
                                    ListNode_t *matches = FindAllIdents(symtab, base_name);
                                    HashNode_t *unit_match = NULL;
                                    for (ListNode_t *mcur = matches; mcur != NULL; mcur = mcur->next)
                                    {
                                        HashNode_t *candidate = (HashNode_t *)mcur->cur;
                                        if (candidate != NULL &&
                                            candidate->hash_type == HASHTYPE_TYPE)
                                        {
                                            if (unit_name != NULL)
                                            {
                                                const char *candidate_unit =
                                                    unit_registry_get(candidate->source_unit_index);
                                                if (candidate_unit != NULL &&
                                                    pascal_identifier_equals(candidate_unit, unit_name))
                                                {
                                                    unit_match = candidate;
                                                    break;
                                                }
                                            }
                                            if (unit_match == NULL && candidate->defined_in_unit)
                                                unit_match = candidate;
                                        }
                                    }
                                    if (unit_match != NULL)
                                        target_node = unit_match;
                                    if (matches != NULL)
                                        DestroyList(matches);
                                }
                            }

                            var_type = get_var_type_from_node(target_node);
                            if (target_node->type != NULL &&
                                !alias_info->is_array && !alias_info->is_pointer &&
                                !alias_info->is_set && !alias_info->is_enum &&
                                !alias_info->is_file && alias_info->inline_record_type == NULL)
                            {
                                KgpcType *existing = tree->tree_data.type_decl_data.kgpc_type;
                                int replace_existing = 0;
                                if (existing == NULL)
                                    replace_existing = 1;
                                else if (existing->kind == TYPE_KIND_PRIMITIVE &&
                                    existing->info.primitive_type_tag == UNKNOWN_TYPE)
                                    replace_existing = 1;
                                if (replace_existing)
                                {
                                    if (existing != NULL)
                                        destroy_kgpc_type(existing);
                                    tree->tree_data.type_decl_data.kgpc_type = target_node->type;
                                    kgpc_type_retain(target_node->type);
                                }
                            }
                        }
                    }
                }

                if (alias_info->base_type == RECORD_TYPE)
                {
                    struct RecordType *alias_record = NULL;
                    KgpcType *alias_type = tree->tree_data.type_decl_data.kgpc_type;

                    if (alias_info->inline_record_type != NULL)
                    {
                        alias_record = alias_info->inline_record_type;
                    }
                    else if (alias_type != NULL)
                    {
                        if (kgpc_type_is_record(alias_type))
                        {
                            alias_record = kgpc_type_get_record(alias_type);
                        }
                        else if (kgpc_type_is_pointer(alias_type))
                        {
                            KgpcType *pointee = alias_type->info.points_to;
                            if (pointee != NULL && kgpc_type_is_record(pointee))
                                alias_record = kgpc_type_get_record(pointee);
                        }
                    }

                    if (alias_record != NULL)
                    {
                        /* Set the type_id on the RecordType for operator overloading */
                        if (alias_record->type_id == NULL && tree->tree_data.type_decl_data.id != NULL)
                        {
                            alias_record->type_id = strdup(tree->tree_data.type_decl_data.id);
                        }

                        /* For generic specializations that are classes with a parent,
                         * merge parent class fields into the specialized record.
                         * This handles e.g. TFPGList<TMyRecord> inheriting FCapacity,
                         * FList etc. from TFPSList. */
                        if (alias_record->parent_class_name != NULL &&
                            alias_record->is_class)
                        {
                            int merge_result = merge_parent_class_fields(symtab, alias_record,
                                tree->tree_data.type_decl_data.id, tree->line_num);
                            if (merge_result > 0)
                                return_val += merge_result;
                        }
                        else if (alias_record->is_class)
                        {
                            detect_default_indexed_property(alias_record, NULL);
                        }

                        if (alias_record->is_class)
                        {
                            int vmt_result = build_class_vmt(symtab, alias_record,
                                tree->tree_data.type_decl_data.id, tree->line_num);
                            if (vmt_result > 0)
                                return_val += vmt_result;
                        }
                        
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (alias_record->generic_decl != NULL && 
                                                         alias_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            const char *debug_env3 = kgpc_getenv("KGPC_DEBUG_TFPG");
                            if (debug_env3 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Computing size for alias %s, generic_decl=%p num_args=%d\n",
                                    tree->tree_data.type_decl_data.id,
                                    (void*)alias_record->generic_decl,
                                    alias_record->num_generic_args);
                            }
                            
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, alias_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                if (debug_env3 != NULL)
                                {
                                    fprintf(stderr, "[KGPC] Size computation FAILED for %s\n",
                                        tree->tree_data.type_decl_data.id);
                                }
                                if (kgpc_getenv("KGPC_DEBUG_SEMSTEPS") != NULL)
                                {
                                    fprintf(stderr, "[SemCheck] alias record size failed for %s (line %d)\n",
                                            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                                            tree->line_num);
                                }
                                return_val += 1;
                            }
                            else if (debug_env3 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Size computation succeeded for %s: %lld bytes\n",
                                    tree->tree_data.type_decl_data.id, record_size);
                            }
                        }
                    }
                }

                const char *debug_alias = kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES");
                if (debug_alias != NULL && tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] semcheck alias %s: base_type=%d target=%s kgpc_type=%p kind=%d\n",
                        tree->tree_data.type_decl_data.id,
                        alias_info->base_type,
                        alias_info->target_type_id != NULL ? alias_info->target_type_id : "<null>",
                        (void *)tree->tree_data.type_decl_data.kgpc_type,
                        tree->tree_data.type_decl_data.kgpc_type != NULL ?
                            tree->tree_data.type_decl_data.kgpc_type->kind : -1);
                }

                break;
            }
            case TYPE_DECL_GENERIC:
            {
                /* Register generic type declaration in the generic registry */
                struct GenericDecl *generic_info = &tree->tree_data.type_decl_data.info.generic;
                
                const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
                if (debug_env != NULL)
                {
                    fprintf(stderr, "[KGPC] semcheck TYPE_DECL_GENERIC: id=%s num_params=%d\n",
                        tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                        generic_info ? generic_info->num_type_params : -1);
                }
                
                if (generic_info != NULL && generic_info->num_type_params > 0 && 
                    generic_info->type_parameters != NULL)
                {
                    /* Register the generic declaration */
                    GenericTypeDecl *generic_decl = generic_registry_add_decl(
                        tree->tree_data.type_decl_data.id,
                        generic_info->type_parameters,
                        generic_info->num_type_params,
                        tree
                    );
                    
                    if (generic_decl != NULL)
                    {
                        /* Store reference to record template if this is a class/record */
                        if (generic_info->record_template != NULL)
                        {
                            generic_decl->record_template = generic_info->record_template;
                        }
                        
                        if (debug_env != NULL)
                        {
                            fprintf(stderr, "[KGPC] Registered generic type %s with %d parameters\n",
                                tree->tree_data.type_decl_data.id, generic_info->num_type_params);
                        }
                    }
                    else if (debug_env != NULL)
                    {
                        fprintf(stderr, "[KGPC] Failed to register generic type %s\n",
                            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>");
                    }
                }
                
                /* Generics don't go in symbol table directly, only specialized instances do */
                var_type = HASHVAR_INTEGER;  /* Placeholder */
                /* Skip the normal PushTypeOntoScope for generics */
                func_return = 0;
                break;
            }
            default:
                var_type = HASHVAR_INTEGER;
                break;
        }

        if (debug_env_check != NULL && return_val > before_switch_errors)
        {
            fprintf(stderr, "[KGPC] Error in switch for type %s (kind=%d), was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, before_switch_errors, return_val);
        }

        const char *debug_env2 = kgpc_getenv("KGPC_DEBUG_TFPG");
        int before_symtab_errors = return_val;

        KgpcType *kgpc_type = tree->tree_data.type_decl_data.kgpc_type;
        if (record_info != NULL && record_type_is_class(record_info) &&
            kgpc_type != NULL && kgpc_type->kind == TYPE_KIND_RECORD)
        {
            /* Classes are reference types - represent them as pointers to the class record */
            KgpcType *wrapped = create_pointer_type(kgpc_type);
            kgpc_type_release(tree->tree_data.type_decl_data.kgpc_type); /* release old tree ref */
            kgpc_type = wrapped;
            tree->tree_data.type_decl_data.kgpc_type = wrapped;
        }
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
            alias_info != NULL)
        {
            /* Rebuild KgpcType from alias when parser's pre-built type is missing
             * or wrong (e.g., string[3] parsed as primitive SHORTSTRING instead of
             * array[0..3] of char, or pointer alias parsed as primitive). */
            int need_rebuild = (kgpc_type == NULL);
            if (!need_rebuild && kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                (alias_info->is_array || alias_info->is_pointer))
                need_rebuild = 1;
            if (need_rebuild)
            {
                KgpcType *rebuilt = create_kgpc_type_from_type_alias(alias_info, symtab,
                    tree->tree_data.type_decl_data.defined_in_unit);
                if (rebuilt != NULL)
                {
                    if (kgpc_type != NULL)
                        kgpc_type_release(kgpc_type);
                    kgpc_type = rebuilt;
                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                }
            }
        }

        /* Check if this type was already pre-declared by predeclare_types().
         * If so, skip the push to avoid "redeclaration" errors.
         * IMPORTANT: Only skip if the type exists in the CURRENT scope (scope level 0),
         * not if it exists as a builtin or in a parent scope.
         * Also don't treat as predeclared if existing is from a different source unit. */
        HashNode_t *existing_type = NULL;
        int scope_level = FindIdent(&existing_type, symtab, tree->tree_data.type_decl_data.id);
        int already_predeclared = (scope_level == 0 && existing_type != NULL &&
                                   existing_type->hash_type == HASHTYPE_TYPE);
        if (debug_pss != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
        {
            fprintf(stderr, "[PShortString] predeclared=%d scope=%d existing_type=%p\n",
                already_predeclared, scope_level, (void *)existing_type);
            if (existing_type != NULL)
            {
                fprintf(stderr,
                    "[PShortString] existing defined_in_unit=%d unit_is_public=%d source_unit_index=%d\n",
                    existing_type->defined_in_unit, existing_type->unit_is_public,
                    existing_type->source_unit_index);
            }
        }

        /* If existing type is from a different source than the current declaration,
         * find the correct predeclared entry from the same source. With cross-unit
         * coexistence, multiple entries with the same name may exist in the same scope.
         * This handles:
         *  - record-record conflicts (e.g. System.tsiginfo vs SysUtils.tsiginfo)
         *  - alias-record conflicts (e.g. System.TSize alias vs Types.TSize record)
         *  - unit-vs-program conflicts (e.g. prelude TGUID vs program's own TGUID) */
        int skip_type_tree = 0;
        if (already_predeclared &&
            ((existing_type->source_unit_index != 0 &&
              tree->tree_data.type_decl_data.source_unit_index != 0 &&
              existing_type->source_unit_index != tree->tree_data.type_decl_data.source_unit_index) ||
             (tree->tree_data.type_decl_data.defined_in_unit &&
              !existing_type->defined_in_unit)))
        {
            /* Search all entries for the one from the same source unit */
            ListNode_t *all_matches = FindAllIdents(symtab, tree->tree_data.type_decl_data.id);
            HashNode_t *same_unit_entry = NULL;
            ListNode_t *m = all_matches;
            int target_idx = tree->tree_data.type_decl_data.source_unit_index;
            while (m != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)m->cur;
                if (candidate != NULL &&
                    candidate->hash_type == HASHTYPE_TYPE &&
                    candidate->source_unit_index == target_idx)
                {
                    same_unit_entry = candidate;
                    break;
                }
                m = m->next;
            }
            destroy_list(all_matches);
            if (same_unit_entry != NULL)
                existing_type = same_unit_entry;
            else
            {
                /* No predeclared entry from the same unit exists.
                 * For the specific case of an alias conflicting with an
                 * existing record from a different unit (e.g. UnixType.TSize
                 * alias vs Types.TSize record), treat as a fresh declaration
                 * so it gets pushed as a new entry.  The hash table's
                 * TYPE+TYPE collision allowance (HashTable.c) permits this
                 * when the existing entry is from a unit.
                 * Only do this when both have non-zero source_unit_index
                 * (true cross-unit conflict).  When existing has index 0
                 * it's a stale/builtin entry and should be skipped. */
                int existing_is_record = (existing_type->type != NULL &&
                    existing_type->type->kind == TYPE_KIND_RECORD);
                struct TypeAlias *existing_alias = get_type_alias_from_node(existing_type);
                int current_is_enum_alias = (
                    tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                    tree->tree_data.type_decl_data.info.alias.is_enum);
                int existing_is_enum_alias = (
                    existing_alias != NULL &&
                    existing_alias->is_enum);
                if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                    (existing_is_record || current_is_enum_alias || existing_is_enum_alias) &&
                    existing_type->defined_in_unit &&
                    existing_type->source_unit_index != 0 &&
                    tree->tree_data.type_decl_data.source_unit_index != 0)
                    already_predeclared = 0;
                else
                    skip_type_tree = 1;
            }
        }
        if (skip_type_tree)
        {
            if (debug_pss != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
            {
                fprintf(stderr, "[PShortString] skip_type_tree=1\n");
            }
            /* Clean up kgpc_type ownership before skipping */
            if (tree->tree_data.type_decl_data.kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
            }
            cur = cur->next;
            continue;
        }


        /* Skip symbol table registration for generic declarations - they're only in the registry */
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_GENERIC)
        {
            /* Generic declarations are already registered in generic_registry */
            /* Skip the rest of symbol table handling */
            if (func_return == 0)
            {
                /* Continue to next type declaration */
            }
        }
        else if (already_predeclared)
        {
            if (debug_pss != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
            {
                fprintf(stderr,
                    "[PShortString] already_predeclared kgpc_type=%p existing_type->type=%p\n",
                    (void *)kgpc_type,
                    existing_type ? (void *)existing_type->type : NULL);
            }
            mark_hashnode_source_unit(existing_type, tree->tree_data.type_decl_data.source_unit_index);

            /* Type was already registered by predeclare_types().
             * We still need to update any additional metadata like array bounds. */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL &&
                alias_info->is_pointer &&
                kgpc_type != NULL &&
                kgpc_type_is_pointer(kgpc_type) &&
                (existing_type->type == NULL || !kgpc_type_is_pointer(existing_type->type)))
            {
                /* Replace predeclared non-pointer placeholder with proper pointer alias type.
                 * Properly manage ref counts: retain new, release old. */
                kgpc_type_retain(kgpc_type);
                if (existing_type->type != NULL)
                    kgpc_type_release(existing_type->type);
                existing_type->type = kgpc_type;
                if (debug_pss != NULL &&
                    tree->tree_data.type_decl_data.id != NULL &&
                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
                {
                    fprintf(stderr, "[PShortString] updated existing pointer type=%p\n",
                        (void *)existing_type->type);
                }
            }
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                     kgpc_type != NULL &&
                     existing_type->type != NULL &&
                     existing_type->type->kind == TYPE_KIND_PRIMITIVE &&
                     existing_type->type->info.primitive_type_tag == UNKNOWN_TYPE &&
                     !(kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                       kgpc_type->info.primitive_type_tag == UNKNOWN_TYPE))
            {
                /* Replace predeclared UNKNOWN placeholders for simple aliases once the
                 * real target type becomes available (e.g. SysUtils.TEndian -> ObjPas.TEndian). */
                kgpc_type_retain(kgpc_type);
                kgpc_type_release(existing_type->type);
                existing_type->type = kgpc_type;
            }

            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL && 
                existing_type->type != NULL)
            {
                struct TypeAlias *existing_alias = kgpc_type_get_type_alias(existing_type->type);
                int can_override_alias = 1;
                if (existing_alias != NULL && existing_alias != alias_info &&
                    alias_info->target_type_id != NULL)
                {
                    int existing_is_unknown_placeholder =
                        (existing_type->type->kind == TYPE_KIND_PRIMITIVE &&
                         existing_type->type->info.primitive_type_tag == UNKNOWN_TYPE);
                    if (!existing_is_unknown_placeholder &&
                        !semcheck_alias_targets_match(existing_alias, alias_info))
                    {
                        /* Avoid overwriting unrelated alias metadata when reusing another type. */
                        can_override_alias = 0;
                    }
                }
                if (existing_alias != NULL)
                {
                    if (alias_info->storage_size <= 0 && existing_alias->storage_size > 0)
                        alias_info->storage_size = existing_alias->storage_size;
                    if (!alias_info->is_range && existing_alias->is_range && existing_alias->range_known)
                    {
                        alias_info->is_range = 1;
                        alias_info->range_known = existing_alias->range_known;
                        alias_info->range_start = existing_alias->range_start;
                        alias_info->range_end = existing_alias->range_end;
                    }
                }

                if (can_override_alias)
                {
                    inherit_alias_metadata(symtab, alias_info);
                    kgpc_type_set_type_alias(existing_type->type, alias_info);
                    if (existing_type->type->type_alias != NULL && alias_info->storage_size > 0)
                        existing_type->type->type_alias->storage_size = alias_info->storage_size;
                }
                else
                {
                    inherit_alias_metadata(symtab, alias_info);
                }

                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_kgpctype(symtab, existing_type->type, alias_info);
                }
                if (alias_info->is_pointer &&
                    kgpc_type_is_pointer(existing_type->type) &&
                    existing_type->type->info.points_to == NULL)
                {
                    HashNode_t *pointee_node = NULL;
                    if (alias_info->pointer_type_ref != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node_with_ref(symtab,
                            alias_info->pointer_type_ref, alias_info->pointer_type_id);
                    }
                    else if (alias_info->pointer_type_id != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node(symtab,
                            alias_info->pointer_type_id);
                    }
                    if (pointee_node != NULL && pointee_node->type != NULL)
                    {
                        kgpc_type_retain(pointee_node->type);
                        existing_type->type->info.points_to = pointee_node->type;
                    }
                }
            }
            /* Release tree's retained reference (predeclare_types retained it). */
            if (tree->tree_data.type_decl_data.kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
            }
            func_return = 0;  /* No error */
        }
        else if (kgpc_type != NULL) {
            /* Set type_alias on KgpcType before pushing */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL)
            {
                struct TypeAlias *existing_alias = kgpc_type_get_type_alias(kgpc_type);
                int can_override_alias = 1;
                if (existing_alias != NULL && existing_alias != alias_info &&
                    alias_info->target_type_id != NULL)
                {
                    if (!semcheck_alias_targets_match(existing_alias, alias_info))
                    {
                        /* Avoid overwriting unrelated alias metadata when reusing another type. */
                        can_override_alias = 0;
                    }
                }
                if (existing_alias != NULL)
                {
                    if (alias_info->storage_size <= 0 && existing_alias->storage_size > 0)
                        alias_info->storage_size = existing_alias->storage_size;
                    if (!alias_info->is_range && existing_alias->is_range && existing_alias->range_known)
                    {
                        alias_info->is_range = 1;
                        alias_info->range_known = existing_alias->range_known;
                        alias_info->range_start = existing_alias->range_start;
                        alias_info->range_end = existing_alias->range_end;
                    }
                }
                /* IMPORTANT: Inherit storage_size from target type for type aliases.
                 * This is critical for types like WideChar (2 bytes) that are represented
                 * as INT_TYPE (4 bytes) in the primitive type system but have a custom
                 * storage_size defined. Without this, SizeOf(TMyChar) where TMyChar = WideChar
                 * would return 4 instead of the correct 2 bytes. */
                inherit_alias_metadata(symtab, alias_info);
                if (can_override_alias)
                {
                    kgpc_type_set_type_alias(kgpc_type, alias_info);
                }
                if (can_override_alias &&
                    kgpc_type_get_type_alias(kgpc_type) != NULL &&
                    alias_info->storage_size > 0)
                {
                    kgpc_type_get_type_alias(kgpc_type)->storage_size = alias_info->storage_size;
                }
                
                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_kgpctype(symtab, kgpc_type, alias_info);
                }
            }
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD && record_info != NULL && kgpc_type->kind == TYPE_KIND_RECORD)
                kgpc_type->info.record_info = record_info;
            
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] Pushing type '%s' onto scope, kgpc_type=%p kind=%d\n", 
                    tree->tree_data.type_decl_data.id, (void*)kgpc_type, kgpc_type ? kgpc_type->kind : -1);
            }
            func_return = PushTypeOntoScope_Typed(symtab, tree->tree_data.type_decl_data.id, kgpc_type);
            if (debug_pss != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
            {
                fprintf(stderr, "[PShortString] PushTypeOntoScope_Typed result=%d kgpc_type=%p\n",
                    func_return, (void *)kgpc_type);
            }
            if (func_return == 0)
            {
                /* Hash table retained its own reference. Release the tree/creator reference. */
                if (tree->tree_data.type_decl_data.kgpc_type != NULL)
                    destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
                /* Note: var_type is automatically set from KgpcType in HashTable.c via set_var_type_from_kgpctype() */
            }
        } else {
        /* Fall back to legacy API for types we can't convert yet */
        func_return = PushTypeOntoScope(symtab, tree->tree_data.type_decl_data.id, var_type,
            record_info, alias_info);
        if (debug_pss != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
        {
            fprintf(stderr, "[PShortString] PushTypeOntoScope (legacy) result=%d\n", func_return);
        }
        }

        /* Note: Enum literals are declared in predeclare_enum_literals() during first pass.
         * We don't redeclare them here to avoid "redeclaration" errors. */

        if(func_return > 0)
        {
            semantic_error(tree->line_num, 0, "redeclaration of name %s",
                tree->tree_data.type_decl_data.id);
            return_val += func_return;
        }
        else
        {
            /* Find the entry to mark with unit info.
             * For predeclared types, use the already-resolved existing_type
             * (which may have been redirected by the cross-unit search above).
             * For newly pushed types, use FindIdent to get the fresh entry. */
            HashNode_t *type_node = NULL;
            if (already_predeclared)
            {
                type_node = existing_type;
            }
            else
            {
                FindIdent(&type_node, symtab, tree->tree_data.type_decl_data.id);
                if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
                    type_node = semcheck_find_type_node_with_unit_flag(symtab,
                        tree->tree_data.type_decl_data.id,
                        tree->tree_data.type_decl_data.defined_in_unit);
            }
            if (type_node != NULL)
            {
                mark_hashnode_unit_info(symtab, type_node,
                    tree->tree_data.type_decl_data.defined_in_unit,
                    tree->tree_data.type_decl_data.unit_is_public);
                mark_hashnode_source_unit(type_node, tree->tree_data.type_decl_data.source_unit_index);
            }

            /* For generic specializations with inline record types, also register the
             * record type under its mangled name so cloned methods can find it */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && 
                alias_info != NULL && 
                alias_info->inline_record_type != NULL &&
                alias_info->inline_record_type->type_id != NULL)
            {
                const char *mangled_name = alias_info->inline_record_type->type_id;
                
                /* Only register if not already registered to avoid redeclaration errors */
                HashNode_t *existing = NULL;
                if (FindIdent(&existing, symtab, mangled_name) == -1)
                {
                    /* Create a KgpcType for the inline record if not already created */
                    KgpcType *inline_kgpc_type = create_record_type(alias_info->inline_record_type);
                    if (inline_kgpc_type != NULL)
                    {
                        int push_result = PushTypeOntoScope_Typed(symtab, (char *)mangled_name, inline_kgpc_type);
                        if (push_result == 0)
                        {
                            if (debug_env2 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Registered inline record type %s for alias %s\n",
                                        mangled_name, tree->tree_data.type_decl_data.id);
                            }
                        }
                        /* Release creator's reference - hash table retained on success */
                        destroy_kgpc_type(inline_kgpc_type);
                    }
                }
            }
        }

        if (debug_env2 != NULL && return_val > before_symtab_errors)
        {
            fprintf(stderr, "[KGPC] Error increased in type %s (kind=%d), was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, before_symtab_errors, return_val);
        }
        
        if (return_val > loop_start_errors)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_type_decls ERROR in type %s: was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                loop_start_errors, return_val);
#endif
        }

        cur = cur->next;
    }

    /* Post-pass: resolve forward pointer aliases now that all types are in scope. */
    cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
        {
            cur = cur->next;
            continue;
        }
        tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_TYPE_DECL ||
            tree->tree_data.type_decl_data.kind != TYPE_DECL_ALIAS)
        {
            cur = cur->next;
            continue;
        }
        struct TypeAlias *post_alias = &tree->tree_data.type_decl_data.info.alias;
        if (post_alias == NULL || !post_alias->is_pointer ||
            post_alias->pointer_type_id == NULL)
        {
            cur = cur->next;
            continue;
        }
        HashNode_t *alias_node = semcheck_find_type_node_with_unit_flag(symtab,
            tree->tree_data.type_decl_data.id,
            tree->tree_data.type_decl_data.defined_in_unit);
        if (alias_node == NULL || alias_node->type == NULL ||
            !kgpc_type_is_pointer(alias_node->type) ||
            alias_node->type->info.points_to != NULL)
        {
            cur = cur->next;
            continue;
        }
        HashNode_t *pointee_node = NULL;
        if (post_alias->pointer_type_ref != NULL)
        {
            pointee_node = semcheck_find_preferred_type_node_with_ref(symtab,
                post_alias->pointer_type_ref, post_alias->pointer_type_id);
        }
        else
        {
            pointee_node = semcheck_find_preferred_type_node(symtab,
                post_alias->pointer_type_id);
        }
        if (pointee_node != NULL && pointee_node->type != NULL)
        {
            kgpc_type_retain(pointee_node->type);
            alias_node->type->info.points_to = pointee_node->type;
        }
        cur = cur->next;
    }

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && return_val > 0)
    {
        fprintf(stderr, "[KGPC] semcheck_type_decls returning error count: %d\n", return_val);
    }
    else if (debug_env != NULL)
    {
        fprintf(stderr, "[KGPC] semcheck_type_decls completed successfully\n");
    }

    return return_val;
}

/* Helper function to process a single constant declaration */
static int semcheck_single_const_decl(SymTab_t *symtab, Tree_t *tree)
{
    int return_val = 0;
    assert(tree != NULL);
    assert(tree->type == TREE_CONST_DECL);

    int saved_unit_context = symtab->unit_context;
    int saved_imported_unit = g_semcheck_imported_decl_unit_index;
    if (tree->tree_data.const_decl_data.defined_in_unit &&
        tree->tree_data.const_decl_data.source_unit_index > 0)
    {
        symtab->unit_context = tree->tree_data.const_decl_data.source_unit_index;
        g_semcheck_imported_decl_unit_index =
            tree->tree_data.const_decl_data.source_unit_index;
    }

    struct Expression *value_expr = tree->tree_data.const_decl_data.value;
    if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL &&
        tree->tree_data.const_decl_data.id != NULL)
    {
        fprintf(stderr, "[KGPC] const decl %s value_expr=%p type=%d\n",
            tree->tree_data.const_decl_data.id,
            (void *)value_expr,
            value_expr ? value_expr->type : -1);
    }
        
        /* Determine the type of constant by checking the expression */
        int is_string_const = expression_is_string(symtab, value_expr);
        int is_real_const = !is_string_const && expression_contains_real_literal_impl(symtab, value_expr);
        
        if (is_string_const)
        {
            /* Evaluate as string constant */
            char *string_value = NULL;
            if (evaluate_string_const_expr(symtab, value_expr, &string_value) != 0)
            {
                semcheck_error_with_context("Error on line %d, unsupported string const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                int push_result;
                /* In Pascal, a single-character string literal constant is a Char, not a String.
                 * e.g., const PathDelim = '/' should be Char type. */
                if (string_value != NULL && strlen(string_value) == 1)
                {
                    long long char_val = (unsigned char)string_value[0];
                    KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                    push_result = PushConstOntoScope_Typed(symtab,
                        tree->tree_data.const_decl_data.id, char_val, char_type);
                    destroy_kgpc_type(char_type);
                    if (push_result == 0)
                    {
                        HashNode_t *const_node = NULL;
                        if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                        {
                            const_node->const_string_value = strdup(string_value);
                        }
                    }
                }
                else
                {
                    push_result = PushStringConstOntoScope(symtab, tree->tree_data.const_decl_data.id, string_value);
                }
                free(string_value);
                if (push_result > 0)
                {
                    semcheck_error_with_context("Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL)
                        {
                            fprintf(stderr, "[KGPC] const pushed: %s\n",
                                tree->tree_data.const_decl_data.id);
                        }
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (is_real_const)
        {
            /* Evaluate as real constant */
            double real_value = 0.0;
            if (evaluate_real_const_expr(symtab, value_expr, &real_value) != 0)
            {
                semcheck_error_with_context("Error on line %d, unsupported real const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                int push_result = PushRealConstOntoScope(symtab, tree->tree_data.const_decl_data.id, real_value);
                if (push_result > 0)
                {
                    semcheck_error_with_context("Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (value_expr != NULL && expression_is_set_const_expr(symtab, value_expr))
        {
            /* Evaluate as set constant (supports char sets up to 0..255) */
            unsigned char set_bytes[32];
            size_t set_size = 0;
            long long mask = 0;
            int is_char_set = 0;
            if (evaluate_set_const_bytes(symtab, value_expr, set_bytes, sizeof(set_bytes),
                    &set_size, &mask, &is_char_set) != 0)
            {
                semcheck_error_with_context("Error on line %d, unsupported const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                KgpcType *const_type = create_primitive_type_with_size(SET_TYPE, (int)set_size);
                if (const_type != NULL && const_type->type_alias != NULL)
                {
                    const_type->type_alias->is_set = 1;
                    const_type->type_alias->set_element_type = is_char_set ? CHAR_TYPE : INT_TYPE;
                }
                int push_result = PushSetConstOntoScope(symtab, tree->tree_data.const_decl_data.id,
                    set_bytes, (int)set_size, const_type);
                destroy_kgpc_type(const_type);  /* Release creator's ref */
                if (push_result > 0)
                {
                    semcheck_error_with_context("Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
        else if (value_expr != NULL && value_expr->type == EXPR_ADDR &&
                 value_expr->expr_data.addr_data.expr != NULL &&
                 value_expr->expr_data.addr_data.expr->type == EXPR_VAR_ID)
        {
            /* Procedure address constant: const MyProcRef: TProc = @MyProc;
             * Handle forward-declared procedure references.
             * The procedure might be declared as forward and implemented later,
             * so we store the procedure name for later resolution in codegen. */
            char *proc_name = value_expr->expr_data.addr_data.expr->expr_data.id;
            if (proc_name != NULL)
            {
                /* Look up the procedure/function in the symbol table.
                 * It may be forward-declared, so we check if it exists at all. */
                HashNode_t *proc_node = NULL;
                int found_scope = FindIdent(&proc_node, symtab, proc_name);
                
                /* Accept if it's a procedure, function, or not yet defined (forward ref).
                 * Forward references will be resolved later during codegen. */
                if (found_scope >= 0 && proc_node != NULL &&
                    (proc_node->hash_type == HASHTYPE_PROCEDURE ||
                     proc_node->hash_type == HASHTYPE_FUNCTION))
                {
                    /* Found the procedure - store as a procedure address constant */
                    KgpcType *const_type = create_procedure_type(NULL, NULL);

                    /* Use 0 as the value - actual address will be resolved at link time */
                    int push_result = PushConstOntoScope_Typed(symtab,
                        tree->tree_data.const_decl_data.id, 0, const_type);
                    destroy_kgpc_type(const_type);  /* Release creator's ref */

                    if (push_result > 0)
                    {
                        semcheck_error_with_context("Error on line %d, redeclaration of const %s!\n",
                                tree->line_num, tree->tree_data.const_decl_data.id);
                        ++return_val;
                    }
                    else
                    {
                        /* Store the procedure name in the const node for codegen */
                        HashNode_t *const_node = NULL;
                        if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                        {
                            /* Store the procedure name as const_string_value for codegen to use */
                            const_node->const_string_value = strdup(proc_name);
                            mark_hashnode_unit_info(symtab, const_node,
                                tree->tree_data.const_decl_data.defined_in_unit,
                                tree->tree_data.const_decl_data.unit_is_public);
                        }
                    }
                }
                else if (found_scope < 0)
                {
                    /* Not found - might be a forward-declared procedure.
                     * Create the constant anyway; codegen will resolve it. */
                    KgpcType *const_type = create_procedure_type(NULL, NULL);

                    int push_result = PushConstOntoScope_Typed(symtab,
                        tree->tree_data.const_decl_data.id, 0, const_type);
                    destroy_kgpc_type(const_type);  /* Release creator's ref */

                    if (push_result > 0)
                    {
                        semcheck_error_with_context("Error on line %d, redeclaration of const %s!\n",
                                tree->line_num, tree->tree_data.const_decl_data.id);
                        ++return_val;
                    }
                    else
                    {
                        HashNode_t *const_node = NULL;
                        if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                        {
                            const_node->const_string_value = strdup(proc_name);
                            mark_hashnode_unit_info(symtab, const_node,
                                tree->tree_data.const_decl_data.defined_in_unit,
                                tree->tree_data.const_decl_data.unit_is_public);
                        }
                    }
                }
                else
                {
                    semcheck_error_with_context("Error on line %d, '%s' is not a procedure or function.\n",
                            tree->line_num, proc_name);
                    ++return_val;
                }
            }
            else
            {
                semcheck_error_with_context("Error on line %d, invalid procedure address expression.\n",
                        tree->line_num);
                ++return_val;
            }
        }
        else
        {
            /* Evaluate as integer constant */
            long long value = 0;
            
            /* Run semantic check on the value expression to resolve types properly.
             * This is important for scoped enum literals like TEndian.Little
             * which need to have their resolved_kgpc_type set. */
            KgpcType *expr_type = NULL;
            if (value_expr != NULL && value_expr->type == EXPR_RECORD_CONSTRUCTOR)
            {
                const char *decl_type_id = tree->tree_data.const_decl_data.type_id;
                const TypeRef *decl_type_ref = tree->tree_data.const_decl_data.type_ref;
                HashNode_t *type_node = NULL;
                struct RecordType *record_type = NULL;
                type_node = semcheck_find_preferred_type_node_with_ref(symtab, decl_type_ref, decl_type_id);
                if (type_node != NULL && type_node->type != NULL)
                {
                    record_type = kgpc_type_get_record(type_node->type);
                    if (record_type == NULL &&
                        type_node->type->kind == TYPE_KIND_POINTER &&
                        type_node->type->info.points_to != NULL)
                    {
                        record_type = kgpc_type_get_record(type_node->type->info.points_to);
                    }
                }
                if (record_type == NULL && type_node != NULL)
                {
                    record_type = get_record_type_from_node(type_node);
                }
                if (record_type != NULL)
                {
                    semcheck_typecheck_record_constructor(value_expr, symtab, INT_MAX, record_type, tree->line_num);
                }
            }
            if (value_expr != NULL && value_expr->type == EXPR_ARRAY_LITERAL &&
                value_expr->array_element_type == UNKNOWN_TYPE &&
                value_expr->array_element_type_id == NULL)
            {
                const char *decl_type_id = tree->tree_data.const_decl_data.type_id;
                const TypeRef *decl_type_ref = tree->tree_data.const_decl_data.type_ref;
                HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(
                    symtab, decl_type_ref, decl_type_id);
                KgpcType *decl_type = (type_node != NULL) ? type_node->type : NULL;
                if (decl_type != NULL && kgpc_type_is_array(decl_type))
                {
                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(decl_type, symtab);
                    if (elem_type != NULL)
                    {
                        value_expr->array_element_type = semcheck_tag_from_kgpc(elem_type);
                        if (elem_type->kind == TYPE_KIND_RECORD &&
                            elem_type->info.record_info != NULL)
                        {
                            value_expr->array_element_record_type = elem_type->info.record_info;
                            if (value_expr->array_element_type_id == NULL &&
                                elem_type->info.record_info->type_id != NULL)
                            {
                                value_expr->array_element_type_id =
                                    strdup(elem_type->info.record_info->type_id);
                            }
                        }
                        if (value_expr->array_element_type_id == NULL &&
                            elem_type->type_alias != NULL &&
                            elem_type->type_alias->target_type_id != NULL)
                        {
                            value_expr->array_element_type_id =
                                strdup(elem_type->type_alias->target_type_id);
                        }
                    }
                    if (value_expr->array_element_type_id == NULL &&
                        decl_type->info.array_info.element_type_id != NULL)
                    {
                        value_expr->array_element_type_id =
                            strdup(decl_type->info.array_info.element_type_id);
                    }
                }
            }
            semcheck_expr_main(symtab, value_expr, INT_MAX, NO_MUTATE, &expr_type);
            
            if (evaluate_const_expr(symtab, value_expr, &value) != 0)
            {
                semcheck_error_with_context("Error on line %d, unsupported const expression.\n", tree->line_num);
                ++return_val;
            }
            else
            {
                /* Check if constant already exists with same value (for re-exports like ARG_MAX = UnixType.ARG_MAX) */
                HashNode_t *existing = NULL;
                int existing_scope = FindIdent(&existing, symtab, tree->tree_data.const_decl_data.id);
                if (existing_scope >= 0 && existing != NULL &&
                    existing->hash_type == HASHTYPE_CONST &&
                    existing->const_int_value == value)
                {
                    /* Same constant with same value - treat as re-export, skip silently */
                    g_semcheck_imported_decl_unit_index = saved_imported_unit;
                    symtab->unit_context = saved_unit_context;
                    return 0;
                }


                
                /* Create KgpcType if this is a set constant or has an explicit type annotation */
                KgpcType *const_type = NULL;
                /* If the const expression is an explicit typecast, prefer that as the const's type. */
                if (value_expr != NULL && value_expr->type == EXPR_TYPECAST)
                {
                    int target_tag = (value_expr->resolved_kgpc_type != NULL)
                        ? semcheck_tag_from_kgpc(value_expr->resolved_kgpc_type)
                        : UNKNOWN_TYPE;
                    if (target_tag == UNKNOWN_TYPE &&
                        value_expr->expr_data.typecast_data.target_type_id != NULL)
                    {
                        target_tag = semcheck_map_builtin_type_name_local(
                            value_expr->expr_data.typecast_data.target_type_id);
                    }
                    if (target_tag != UNKNOWN_TYPE)
                        const_type = create_primitive_type(target_tag);
                }
                if (value_expr != NULL && value_expr->type == EXPR_SET)
                {
                    const_type = create_primitive_type(SET_TYPE);
                }
                else if (value_expr != NULL && value_expr->type == EXPR_BOOL)
                {
                    const_type = create_primitive_type(BOOL);
                }
                /* Check if the expression has enum type (e.g., scoped enum literals like TEnum.Value) */
                else if (value_expr != NULL && value_expr->resolved_kgpc_type != NULL)
                {
                    struct TypeAlias *alias = kgpc_type_get_type_alias(value_expr->resolved_kgpc_type);
                    if (alias != NULL && alias->is_enum)
                    {
                        /* Preserve the enum type for the constant */
                        const_type = value_expr->resolved_kgpc_type;
                        kgpc_type_retain(const_type);
                    }
                    else
                    {
                        /* Preserve unsigned integer types (e.g. QWORD_TYPE from High(QWord))
                         * so that the codegen can use unsigned comparisons. */
                        int tag = semcheck_tag_from_kgpc(value_expr->resolved_kgpc_type);
                        if (is_unsigned_integer_type(tag))
                        {
                            const_type = value_expr->resolved_kgpc_type;
                            kgpc_type_retain(const_type);
                        }
                    }
                }
                else if (tree->tree_data.const_decl_data.type_id != NULL)
                {
                    /* Check if the type annotation is "boolean" */
                    const char *type_id = tree->tree_data.const_decl_data.type_id;
                    if (strcasecmp(type_id, "boolean") == 0)
                    {
                        const_type = create_primitive_type(BOOL);
                    }
                    /* Add more type checks as needed */
                }
                
                /* Use typed or legacy API depending on whether we have a KgpcType */
                int push_result;
                if (const_type != NULL)
                {
                    push_result = PushConstOntoScope_Typed(symtab, tree->tree_data.const_decl_data.id, value, const_type);
                    destroy_kgpc_type(const_type);  /* Release creator's ref */
                }
                else
                {
                    push_result = PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, value);
                }

                if (push_result > 0)
                {
                    semcheck_error_with_context("Error on line %d, redeclaration of const %s!\n",
                            tree->line_num, tree->tree_data.const_decl_data.id);
                    ++return_val;
                }
                else
                {
                    HashNode_t *const_node = NULL;
                    if (FindIdent(&const_node, symtab, tree->tree_data.const_decl_data.id) != -1 && const_node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, const_node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }

    g_semcheck_imported_decl_unit_index = saved_imported_unit;
    symtab->unit_context = saved_unit_context;
    return return_val;
}

/* Quick pre-pass: push trivially evaluable imported constants (literal integers,
 * literal reals, simple identifier references) onto the symbol table so that
 * cross-unit qualified references like types.EqualsValue can resolve them.
 * This handles merge-ordering issues where a re-exporting unit (Math) is
 * merged before the defining unit (Types). */
static void prepush_trivial_imported_consts(SymTab_t *symtab, ListNode_t *const_decls)
{
    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        Tree_t *tree = (Tree_t *)cur->cur;
        if (!tree->tree_data.const_decl_data.defined_in_unit)
            continue;
        if (tree->tree_data.const_decl_data.id == NULL)
            continue;

        /* Skip if already in symbol table */
        HashNode_t *existing = NULL;
        if (FindIdent(&existing, symtab, tree->tree_data.const_decl_data.id) >= 0 &&
            existing != NULL &&
            (existing->hash_type == HASHTYPE_CONST || existing->is_typed_const))
            continue;

        struct Expression *value_expr = tree->tree_data.const_decl_data.value;
        if (value_expr == NULL)
            continue;

        /* Handle string constants (EXPR_STRING, EXPR_CHAR_CODE) */
        if (value_expr->type == EXPR_STRING && value_expr->expr_data.string != NULL)
        {
            const char *string_value = value_expr->expr_data.string;
            /* Single-character strings are Char type in Pascal */
            if (strlen(string_value) == 1)
            {
                long long char_val = (unsigned char)string_value[0];
                KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                int push_result = PushConstOntoScope_Typed(symtab,
                    tree->tree_data.const_decl_data.id, char_val, char_type);
                destroy_kgpc_type(char_type);
                if (push_result == 0)
                {
                    HashNode_t *node = NULL;
                    if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
                    {
                        node->const_string_value = strdup(string_value);
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
            else
            {
                PushStringConstOntoScope(symtab, tree->tree_data.const_decl_data.id,
                    string_value);
                HashNode_t *node = NULL;
                if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
                {
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                }
            }
            continue;
        }
        if (value_expr->type == EXPR_CHAR_CODE)
        {
            /* Push as typed Char constant (matching normal semcheck behavior) */
            long long char_val = (unsigned char)(value_expr->expr_data.char_code & 0xFF);
            KgpcType *char_type = create_primitive_type(CHAR_TYPE);
            int push_result = PushConstOntoScope_Typed(symtab,
                tree->tree_data.const_decl_data.id, char_val, char_type);
            destroy_kgpc_type(char_type);
            if (push_result == 0)
            {
                HashNode_t *node = NULL;
                if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
                {
                    /* Also set string value like normal semcheck does */
                    char str[2] = { (char)(char_val & 0xFF), '\0' };
                    node->const_string_value = strdup(str);
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                }
            }
            continue;
        }

        /* Only handle trivially evaluable expressions */
        if (value_expr->type == EXPR_INUM)
        {
            long long val = value_expr->expr_data.i_num;
            PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, val);
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
            {
                mark_hashnode_unit_info(symtab, node,
                    tree->tree_data.const_decl_data.defined_in_unit,
                    tree->tree_data.const_decl_data.unit_is_public);
            }
        }
        else if (value_expr->type == EXPR_RNUM)
        {
            double val = value_expr->expr_data.r_num;
            PushRealConstOntoScope(symtab, tree->tree_data.const_decl_data.id, val);
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
            {
                mark_hashnode_unit_info(symtab, node,
                    tree->tree_data.const_decl_data.defined_in_unit,
                    tree->tree_data.const_decl_data.unit_is_public);
            }
        }
        else if (value_expr->type == EXPR_VAR_ID && value_expr->expr_data.id != NULL)
        {
            /* Simple reference to another constant */
            HashNode_t *ref = NULL;
            if (FindIdent(&ref, symtab, value_expr->expr_data.id) >= 0 && ref != NULL &&
                (ref->hash_type == HASHTYPE_CONST || ref->is_typed_const))
            {
                if (ref->const_string_value != NULL)
                {
                    const char *sv = ref->const_string_value;
                    if (strlen(sv) == 1 && ref->type != NULL &&
                        kgpc_type_equals_tag(ref->type, CHAR_TYPE))
                    {
                        /* Reference to a Char const — push as typed Char */
                        long long char_val = (unsigned char)sv[0];
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        int pr = PushConstOntoScope_Typed(symtab,
                            tree->tree_data.const_decl_data.id, char_val, char_type);
                        destroy_kgpc_type(char_type);
                        if (pr == 0)
                        {
                            HashNode_t *n2 = NULL;
                            if (FindIdent(&n2, symtab, tree->tree_data.const_decl_data.id) >= 0 && n2 != NULL)
                                n2->const_string_value = strdup(sv);
                        }
                    }
                    else
                    {
                        PushStringConstOntoScope(symtab, tree->tree_data.const_decl_data.id, sv);
                    }
                }
                else if (ref->type != NULL && kgpc_type_is_real(ref->type))
                    PushRealConstOntoScope(symtab, tree->tree_data.const_decl_data.id, ref->const_real_value);
                else
                    PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, ref->const_int_value);
                HashNode_t *node = NULL;
                if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
                {
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                }
            }
        }
        else if (value_expr->type == EXPR_SIGN_TERM && value_expr->expr_data.sign_term != NULL &&
                 value_expr->expr_data.sign_term->type == EXPR_INUM)
        {
            long long val = -value_expr->expr_data.sign_term->expr_data.i_num;
            PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, val);
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
            {
                mark_hashnode_unit_info(symtab, node,
                    tree->tree_data.const_decl_data.defined_in_unit,
                    tree->tree_data.const_decl_data.unit_is_public);
            }
        }
        else if (value_expr->type == EXPR_FUNCTION_CALL)
        {
            /* Try full const evaluation silently for expressions like Low()/High(). */
            long long val = 0;
            int ok = const_fold_int_expr(symtab, value_expr, &val);
            if (ok == 0)
            {
                PushConstOntoScope(symtab, tree->tree_data.const_decl_data.id, val);
                HashNode_t *node = NULL;
                if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
                {
                    mark_hashnode_unit_info(symtab, node,
                        tree->tree_data.const_decl_data.defined_in_unit,
                        tree->tree_data.const_decl_data.unit_is_public);
                }
            }
            else
            {
                /* Also try real const evaluation */
                double rval = 0.0;
                ok = const_fold_real_expr(symtab, value_expr, &rval);
                if (ok == 0)
                {
                    PushRealConstOntoScope(symtab, tree->tree_data.const_decl_data.id, rval);
                    HashNode_t *node = NULL;
                    if (FindIdent(&node, symtab, tree->tree_data.const_decl_data.id) >= 0 && node != NULL)
                    {
                        mark_hashnode_unit_info(symtab, node,
                            tree->tree_data.const_decl_data.defined_in_unit,
                            tree->tree_data.const_decl_data.unit_is_public);
                    }
                }
            }
        }
    }
}

static int semcheck_const_decls_imported(SymTab_t *symtab, ListNode_t *const_decls)
{
    int return_val = 0;
    ListNode_t *cur = const_decls;
    while (cur != NULL)
    {
        assert(cur->type == LIST_TREE);
        Tree_t *tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_CONST_DECL);
        if (tree->tree_data.const_decl_data.defined_in_unit)
            return_val += semcheck_single_const_decl(symtab, tree);
        cur = cur->next;
    }
    return return_val;
}

static int semcheck_const_decls_local(SymTab_t *symtab, ListNode_t *const_decls)
{
    int return_val = 0;
    ListNode_t *cur = const_decls;
    while (cur != NULL)
    {
        assert(cur->type == LIST_TREE);
        Tree_t *tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_CONST_DECL);
        if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL &&
            tree->tree_data.const_decl_data.id != NULL)
        {
            fprintf(stderr, "[KGPC] local const seen: %s (defined_in_unit=%d)\n",
                tree->tree_data.const_decl_data.id,
                tree->tree_data.const_decl_data.defined_in_unit);
        }
        if (!tree->tree_data.const_decl_data.defined_in_unit)
            return_val += semcheck_single_const_decl(symtab, tree);
        cur = cur->next;
    }
    return return_val;
}

static int semcheck_const_expr_refs_current_unit_qualified_id(struct Expression *expr)
{
    if (expr == NULL || g_semcheck_current_unit_index == 0)
        return 0;
    if (expr->type != EXPR_RECORD_ACCESS)
        return 0;

    struct Expression *owner = expr->expr_data.record_access_data.record_expr;
    if (owner == NULL || owner->type != EXPR_VAR_ID || owner->expr_data.id == NULL)
        return 0;

    const char *cur_unit_str = unit_registry_get(g_semcheck_current_unit_index);
    return cur_unit_str != NULL && pascal_identifier_equals(owner->expr_data.id, cur_unit_str);
}

static int semcheck_const_decls_imported_filtered(SymTab_t *symtab, ListNode_t *const_decls,
    int defer_current_unit_qualified)
{
    int return_val = 0;
    ListNode_t *cur = const_decls;
    while (cur != NULL)
    {
        assert(cur->type == LIST_TREE);
        Tree_t *tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_CONST_DECL);
        if (tree->tree_data.const_decl_data.defined_in_unit)
        {
            int is_deferred =
                semcheck_const_expr_refs_current_unit_qualified_id(
                    tree->tree_data.const_decl_data.value);
            if ((defer_current_unit_qualified && is_deferred) ||
                (!defer_current_unit_qualified && !is_deferred))
            {
                cur = cur->next;
                continue;
            }
            return_val += semcheck_single_const_decl(symtab, tree);
        }
        cur = cur->next;
    }
    return return_val;
}

/* Semantic check on constant declarations.
 *
 * ARCHITECTURAL FIX: Two-pass processing to handle qualified constant references.
 * When a unit (e.g., baseunix) imports another unit (e.g., UnixType) and re-aliases
 * constants like: ARG_MAX = UnixType.ARG_MAX;
 *
 * The imported unit's constants must be pushed to the symbol table BEFORE the
 * local constants are evaluated. This requires:
 *   Pass 1: Process only constants from imported units (defined_in_unit=1)
 *   Pass 2: Process remaining constants (local constants that may reference imported ones)
 */
int semcheck_const_decls(SymTab_t *symtab, ListNode_t *const_decls)
{
    int return_val = 0;
    prepush_trivial_imported_consts(symtab, const_decls);
    return_val += semcheck_const_decls_imported_filtered(symtab, const_decls, 1);
    return_val += semcheck_const_decls_local(symtab, const_decls);
    /* Retry deferred imported constants that reference CurrentUnit.ConstName.
     * In cyclic unit dependencies (e.g. types <-> math), these can only be
     * resolved after local constants have been declared. */
    return_val += semcheck_const_decls_imported_filtered(symtab, const_decls, 0);
    return return_val;
}

/* Adds built-in functions */
/*TODO: these should be defined in pascal not in semantic analyzer */
static void add_builtin_type_owned(SymTab_t *symtab, const char *name, KgpcType *type)
{
    if (symtab == NULL || name == NULL || type == NULL)
        return;
    if (type->type_alias != NULL && type->type_alias->target_type_id == NULL)
        type->type_alias->target_type_id = strdup(name);
    AddBuiltinType_Typed(symtab, (char *)name, type);
    destroy_kgpc_type(type);  /* Release creator's ref; hash table retained its own */
}

static void add_builtin_alias_type(SymTab_t *symtab, const char *name, int base_type,
    int storage_size)
{
    if (symtab == NULL || name == NULL)
        return;

    struct TypeAlias alias = {0};
    alias.alias_name = (char *)name;  /* Will be duplicated by copy_type_alias */
    alias.base_type = base_type;
    alias.storage_size = storage_size;

    KgpcType *type = create_primitive_type(base_type);
    if (type == NULL)
    {
        return;
    }
    kgpc_type_set_type_alias(type, &alias);
    AddBuiltinType_Typed(symtab, (char *)name, type);
    destroy_kgpc_type(type);
}

static void add_builtin_from_vartype(SymTab_t *symtab, const char *name, enum VarType vt)
{
    KgpcType *t = kgpc_type_from_var_type(vt);
    assert(t != NULL && "Failed to create builtin type");
    add_builtin_type_owned(symtab, name, t);
}

/* Add a string type with explicit type alias to preserve type identity for mangling */
static void add_builtin_string_type_with_alias(SymTab_t *symtab, const char *name,
    enum VarType vt, int is_wide_string)
{
    KgpcType *t = kgpc_type_from_var_type(vt);
    assert(t != NULL && "Failed to create builtin type");
    
    /* Create and attach a type alias with the specific type name */
    struct TypeAlias alias = {0};
    alias.base_type = STRING_TYPE;
    alias.target_type_id = (char *)name;  /* Will be duplicated by copy_type_alias */
    alias.alias_name = (char *)name;      /* Will be duplicated by copy_type_alias */
    alias.is_wide_string = is_wide_string;
    kgpc_type_set_type_alias(t, &alias);
    
    add_builtin_type_owned(symtab, name, t);
}

static void add_builtin_shortstring_type(SymTab_t *symtab, const char *name)
{
    if (symtab == NULL || name == NULL)
        return;

    KgpcType *t = create_primitive_type(SHORTSTRING_TYPE);
    if (t == NULL)
        return;

    struct TypeAlias alias = {0};
    alias.base_type = SHORTSTRING_TYPE;
    alias.target_type_id = (char *)name;  /* Will be duplicated by copy_type_alias */
    alias.alias_name = (char *)name;      /* Will be duplicated by copy_type_alias */
    alias.is_shortstring = 1;
    kgpc_type_set_type_alias(t, &alias);

    add_builtin_type_owned(symtab, name, t);
}

static int semcheck_has_symbol(SymTab_t *symtab, const char *name)
{
    HashNode_t *node = NULL;
    if (symtab == NULL || name == NULL)
        return 0;
    return (FindIdent(&node, symtab, (char *)name) >= 0 && node != NULL);
}

static void ensure_builtin_char_const_if_missing(SymTab_t *symtab, const char *name, int value)
{
    if (!semcheck_has_symbol(symtab, name))
        AddBuiltinCharConst(symtab, name, value);
}

static void ensure_builtin_int_const_if_missing(SymTab_t *symtab, const char *name, long long value)
{
    if (!semcheck_has_symbol(symtab, name))
        AddBuiltinIntConst(symtab, name, value);
}

static void ensure_builtin_alias_type_if_missing(SymTab_t *symtab, const char *name, int base_type, int storage_size)
{
    if (!semcheck_has_symbol(symtab, name))
        add_builtin_alias_type(symtab, name, base_type, storage_size);
}

static void register_simple_builtin_proc(SymTab_t *symtab, const char *name)
{
    assert(name != NULL && "register_simple_builtin_proc: name must not be NULL");
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_simple_builtin_proc: strdup failed");
    KgpcType *proc_type = create_procedure_type(NULL, NULL);
    assert(proc_type != NULL && "register_simple_builtin_proc: failed to create procedure type");
    AddBuiltinProc_Typed(symtab, dup_name, proc_type);
    destroy_kgpc_type(proc_type);
    free(dup_name);
}

/* Register a simple built-in function (no parameters) with the given return
 * type tag (e.g. LONGINT_TYPE, CHAR_TYPE, POINTER_TYPE). */
static void register_simple_builtin_func(SymTab_t *symtab, const char *name,
    int return_type_tag)
{
    assert(name != NULL && "register_simple_builtin_func: name must not be NULL");
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_simple_builtin_func: strdup failed");
    KgpcType *return_type = create_primitive_type(return_type_tag);
    assert(return_type != NULL && "register_simple_builtin_func: failed to create return type");
    KgpcType *func_type = create_procedure_type(NULL, return_type);
    assert(func_type != NULL && "register_simple_builtin_func: failed to create function type");
    AddBuiltinFunction_Typed(symtab, dup_name, func_type);
    destroy_kgpc_type(func_type);
    destroy_kgpc_type(return_type);
    free(dup_name);
}

/* Register an overloaded unary built-in function: name(param_name: arg_type): ret_type.
 * Caller may invoke this multiple times with different type tags to register overloads. */
static void register_unary_builtin_func(SymTab_t *symtab, const char *name,
    const char *param_name, int arg_type_tag, int return_type_tag)
{
    assert(name != NULL && param_name != NULL);
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_unary_builtin_func: strdup failed");
    ListNode_t *param = semcheck_create_builtin_param(param_name, arg_type_tag);
    KgpcType *return_type = create_primitive_type(return_type_tag);
    KgpcType *func_type = create_procedure_type(param, return_type);
    if (func_type != NULL)
    {
        AddBuiltinFunction_Typed(symtab, dup_name, func_type);
        destroy_kgpc_type(func_type);
    }
    if (param != NULL)
        DestroyList(param);
    destroy_kgpc_type(return_type);
    free(dup_name);
}

/* Register an overloaded binary built-in function:
 * name(p1_name: p1_type; p2_name: p2_type): ret_type.
 * Caller may invoke multiple times with different types for overloads. */
static void register_binary_builtin_func(SymTab_t *symtab, const char *name,
    const char *p1_name, int p1_type_tag,
    const char *p2_name, int p2_type_tag,
    int return_type_tag)
{
    assert(name != NULL && p1_name != NULL && p2_name != NULL);
    char *dup_name = strdup(name);
    assert(dup_name != NULL && "register_binary_builtin_func: strdup failed");
    ListNode_t *p1 = semcheck_create_builtin_param(p1_name, p1_type_tag);
    ListNode_t *p2 = semcheck_create_builtin_param(p2_name, p2_type_tag);
    ListNode_t *params = ConcatList(p1, p2);
    KgpcType *return_type = create_primitive_type(return_type_tag);
    KgpcType *func_type = create_procedure_type(params, return_type);
    if (func_type != NULL)
    {
        AddBuiltinFunction_Typed(symtab, dup_name, func_type);
        destroy_kgpc_type(func_type);
    }
    if (params != NULL)
        DestroyList(params);
    destroy_kgpc_type(return_type);
    free(dup_name);
}

void semcheck_add_builtins(SymTab_t *symtab)
{

    if (!stdlib_loaded_flag())
    {
        /* Platform newline constants to support System/ObjPas resourcestring concatenations */
        AddBuiltinStringConst(symtab, "LineEnding", "\n");
        AddBuiltinStringConst(symtab, "sLineBreak", "\n");
        AddBuiltinCharConst(symtab, "DirectorySeparator", '/');
        AddBuiltinCharConst(symtab, "DriveSeparator", 0);
        AddBuiltinCharConst(symtab, "PathSeparator", ':');
        AddBuiltinCharConst(symtab, "ExtensionSeparator", '.');
        AddBuiltinIntConst(symtab, "MaxPathLen", 4096);

        /* Unix/Linux baseline limits needed by unix.pp aliases (UT.*) */
        AddBuiltinIntConst(symtab, "ARG_MAX", 131072);
        AddBuiltinIntConst(symtab, "NAME_MAX", 255);
        AddBuiltinIntConst(symtab, "PATH_MAX", 4095);
        AddBuiltinIntConst(symtab, "SYS_NMLN", 65);
        AddBuiltinIntConst(symtab, "SIG_MAXSIG", 128);
        AddBuiltinIntConst(symtab, "PRIO_PROCESS", 0);
        AddBuiltinIntConst(symtab, "PRIO_PGRP", 1);
        AddBuiltinIntConst(symtab, "PRIO_USER", 2);
        AddBuiltinIntConst(symtab, "UTSNAME_LENGTH", 65);
        AddBuiltinIntConst(symtab, "fmClosed", 0xD7B0);
        AddBuiltinIntConst(symtab, "fmInput", 0xD7B1);

        /* Sysutils signal helpers (interface declarations may be skipped during parsing). */
        AddBuiltinIntConst(symtab, "RTL_SIGINT", 0);
        AddBuiltinIntConst(symtab, "RTL_SIGFPE", 1);
        AddBuiltinIntConst(symtab, "RTL_SIGSEGV", 2);
        AddBuiltinIntConst(symtab, "RTL_SIGILL", 3);
        AddBuiltinIntConst(symtab, "RTL_SIGBUS", 4);
        AddBuiltinIntConst(symtab, "RTL_SIGQUIT", 5);
        AddBuiltinIntConst(symtab, "RTL_SIGLAST", 5);
        AddBuiltinIntConst(symtab, "RTL_SIGDEFAULT", -1);
        AddBuiltinIntConst(symtab, "ssNotHooked", 0);
        AddBuiltinIntConst(symtab, "ssHooked", 1);
        AddBuiltinIntConst(symtab, "ssOverridden", 2);
    }

    /* Bootstrap stability guard:
     * Some stdlib unit compile paths (notably RTL bootstrap units) may reference
     * these core symbols before imported declarations are visible in the current
     * semcheck pass. Seed only what is missing; never overwrite parsed symbols. */
    ensure_builtin_char_const_if_missing(symtab, "DirectorySeparator", '/');
    ensure_builtin_char_const_if_missing(symtab, "DriveSeparator", 0);
    ensure_builtin_char_const_if_missing(symtab, "PathSeparator", ':');
    ensure_builtin_char_const_if_missing(symtab, "ExtensionSeparator", '.');
    ensure_builtin_int_const_if_missing(symtab, "MaxPathLen", 4096);
    ensure_builtin_int_const_if_missing(symtab, "reRangeError", 4);
    ensure_builtin_int_const_if_missing(symtab, "reInvalidCast", 10);
    ensure_builtin_alias_type_if_missing(symtab, "RTLString", STRING_TYPE, 0);
    
    /* Integer boundary constants - required by FPC's objpas.pp and system.pp */
    {
        char *name;
        /* MaxInt: 32767 in System scope (Integer = SmallInt in FPC's system unit).
         * system.p overrides this to 2147483647 (Integer = LongInt) for unqualified access. */
        name = strdup("MaxInt");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 32767LL);
            free(name);
        }
        /* MaxLongint: Maximum value for LongInt (32-bit signed) = 2^31 - 1 */
        name = strdup("MaxLongint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 2147483647LL);
            free(name);
        }
        /* MaxSmallint: Maximum value for SmallInt (16-bit signed) = 2^15 - 1 */
        name = strdup("MaxSmallint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 32767LL);
            free(name);
        }
        name = strdup("DefaultSystemCodePage");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 65001LL);
            free(name);
        }
        name = strdup("DefaultFileSystemCodePage");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 65001LL);
            free(name);
        }
        /* MaxShortint: Maximum value for ShortInt (8-bit signed) = 2^7 - 1 */
        name = strdup("MaxShortint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 127LL);
            free(name);
        }
        /* MaxInt64: Maximum value for Int64 (64-bit signed) = 2^63 - 1 */
        name = strdup("MaxInt64");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 9223372036854775807LL);
            free(name);
        }
    }

    {
        KgpcType *pchar = create_pointer_type(create_primitive_type(CHAR_TYPE));
        KgpcType *ppchar = create_pointer_type(pchar);
        if (ppchar != NULL)
        {
            char *envp_name = strdup("EnvP");
            if (envp_name != NULL)
                PushVarOntoScope_Typed(symtab, envp_name, ppchar);
            char *envp_lower = strdup("envp");
            if (envp_lower != NULL)
                PushVarOntoScope_Typed(symtab, envp_lower, ppchar);
            destroy_kgpc_type(ppchar);
        }
    }
    
    /* Primitive core types required to semcheck system.p and user code.
     * Everything else should live in KGPC/Units/system.p (aliases, pointer helpers, etc.). */
    /* Integer is defined by system.p as "Integer = LongInt" (32-bit in objfpc mode).
     * In FPC RTL mode, system.pp defines "Integer = SmallInt" (16-bit) and then
     * objpas.pp overrides it to "Integer = LongInt" via the alias override logic.
     * We do NOT register Integer as a builtin to avoid conflicting with system.p. */
    add_builtin_from_vartype(symtab, "LongInt", HASHVAR_LONGINT);
    add_builtin_type_owned(symtab, "Int64", create_primitive_type_with_size(INT64_TYPE, 8));
    if (!stdlib_loaded_flag())
        add_builtin_alias_type(symtab, "Currency", INT64_TYPE, 8);
    add_builtin_from_vartype(symtab, "Real", HASHVAR_REAL);
    add_builtin_from_vartype(symtab, "Boolean", HASHVAR_BOOLEAN);
    /* FPC-compatible extended boolean types */
    add_builtin_alias_type(symtab, "Boolean8", BOOL, 1);
    add_builtin_alias_type(symtab, "Boolean16", BOOL, 2);
    add_builtin_alias_type(symtab, "Boolean32", BOOL, 4);
    add_builtin_alias_type(symtab, "Boolean64", BOOL, 8);
    /* FPC-compatible byte/word/long boolean types (synonyms) */
    add_builtin_alias_type(symtab, "ByteBool", BOOL, 1);
    add_builtin_alias_type(symtab, "WordBool", BOOL, 2);
    add_builtin_alias_type(symtab, "LongBool", BOOL, 4);
    add_builtin_alias_type(symtab, "QWordBool", BOOL, 8);
    if (!stdlib_loaded_flag())
    {
        add_builtin_alias_type(symtab, "TSignalState", INT_TYPE, 4);
    }
    add_builtin_from_vartype(symtab, "Char", HASHVAR_CHAR);
    add_builtin_from_vartype(symtab, "AnsiChar", HASHVAR_CHAR);
    add_builtin_type_owned(symtab, "WideChar", create_primitive_type_with_size(CHAR_TYPE, 2));
    add_builtin_from_vartype(symtab, "String", HASHVAR_PCHAR);
    add_builtin_from_vartype(symtab, "OpenString", HASHVAR_PCHAR);
    add_builtin_from_vartype(symtab, "AnsiString", HASHVAR_PCHAR);
    add_builtin_shortstring_type(symtab, "ShortString");
    add_builtin_string_type_with_alias(symtab, "RawByteString", HASHVAR_PCHAR, 0);
    add_builtin_string_type_with_alias(symtab, "UnicodeString", HASHVAR_PCHAR, 1);
    add_builtin_string_type_with_alias(symtab, "WideString", HASHVAR_PCHAR, 1);
    if (!stdlib_loaded_flag())
    {
        add_builtin_type_owned(symtab, "PAnsiString",
            create_pointer_type(create_primitive_type(STRING_TYPE)));
        add_builtin_type_owned(symtab, "PString",
            create_pointer_type(create_primitive_type(STRING_TYPE)));
    }
    add_builtin_type_owned(symtab, "PAnsiChar",
        create_pointer_type(create_primitive_type(CHAR_TYPE)));

    /* Primitive pointer type */
    add_builtin_type_owned(symtab, "Pointer", create_primitive_type(POINTER_TYPE));
    /* Common ordinal aliases (match KGPC system.p sizes) */
    add_builtin_type_owned(symtab, "Byte", create_primitive_type_with_size(BYTE_TYPE, 1));
    add_builtin_type_owned(symtab, "ShortInt", create_primitive_type_with_size(INT_TYPE, 1));
    add_builtin_type_owned(symtab, "SmallInt", create_primitive_type_with_size(INT_TYPE, 2));
    add_builtin_type_owned(symtab, "Word", create_primitive_type_with_size(WORD_TYPE, 2));
    add_builtin_type_owned(symtab, "LongWord", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "Cardinal", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "DWord", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "QWord", create_primitive_type_with_size(QWORD_TYPE, 8));
    add_builtin_type_owned(symtab, "UInt64", create_primitive_type_with_size(QWORD_TYPE, 8));
    add_builtin_type_owned(symtab, "Int8", create_primitive_type_with_size(INT_TYPE, 1));
    add_builtin_type_owned(symtab, "UInt8", create_primitive_type_with_size(BYTE_TYPE, 1));
    add_builtin_type_owned(symtab, "Int16", create_primitive_type_with_size(INT_TYPE, 2));
    add_builtin_type_owned(symtab, "UInt16", create_primitive_type_with_size(WORD_TYPE, 2));
    add_builtin_type_owned(symtab, "Int32", create_primitive_type_with_size(INT_TYPE, 4));
    add_builtin_type_owned(symtab, "UInt32", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "Float", create_primitive_type_with_size(REAL_TYPE, 4));
    add_builtin_type_owned(symtab, "ValReal", create_primitive_type_with_size(REAL_TYPE, 8));
    add_builtin_type_owned(symtab, "Single", create_primitive_type_with_size(REAL_TYPE, 4));
    add_builtin_type_owned(symtab, "Double", create_primitive_type_with_size(REAL_TYPE, 8));
    add_builtin_type_owned(symtab, "Extended", create_primitive_type_with_size(EXTENDED_TYPE, 10));

    /* Variant and OleVariant (COM interop) - treated as opaque 16-byte types.
     * VARIANT_TYPE auto-coerces to/from any value type at the semantic level. */
    add_builtin_type_owned(symtab, "Variant", create_primitive_type_with_size(VARIANT_TYPE, 16));
    add_builtin_type_owned(symtab, "OleVariant", create_primitive_type_with_size(VARIANT_TYPE, 16));

    /* File/Text primitives (sizes align with system.p TextRec/FileRec layout) */
    add_builtin_type_owned(symtab, "file", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "File", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "TypedFile", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "text", create_primitive_type_with_size(TEXT_TYPE, 672));
    add_builtin_type_owned(symtab, "Text", create_primitive_type_with_size(TEXT_TYPE, 672));

    AddBuiltinRealConst(symtab, "Pi", acos(-1.0));

    /* Builtin procedures - procedures have no return type */
    {
        static const char *simple_procs[] = {
            "SetLength", "SetString", "write", "writeln", "writestr",
            "read", "readln", "Halt", "Error", "Assign", "Close",
            "SetTextCodePage", "GetMem", "ReallocMem",
            "FreeMem", "Val", "Str", "Insert", "Delete", "Inc", "Dec",
            "Include", "Exclude", "New", "Dispose", "Assert"
        };
        for (size_t i = 0; i < sizeof(simple_procs) / sizeof(simple_procs[0]); ++i)
            register_simple_builtin_proc(symtab, simple_procs[i]);
    }

    char *pchar_to_shortstr_name = strdup("fpc_pchar_to_shortstr");
    if (pchar_to_shortstr_name != NULL)
    {
        ListNode_t *param_res = semcheck_create_builtin_param_with_id(
            "res", SHORTSTRING_TYPE, "shortstring", 1);
        ListNode_t *param_p = semcheck_create_builtin_param_with_id(
            "p", POINTER_TYPE, "PAnsiChar", 0);
        if (param_res != NULL)
        {
            param_res->next = param_p;
            KgpcType *proc_type = create_procedure_type(param_res, NULL);
            assert(proc_type != NULL &&
                "Failed to create fpc_pchar_to_shortstr procedure type");
            AddBuiltinProc_Typed(symtab, pchar_to_shortstr_name, proc_type);
            destroy_kgpc_type(proc_type);
        }
        free(pchar_to_shortstr_name);
    }

    const char *sysutils_hooks[] = {
        "InitExceptions",
        "InitInternational",
        "DoneExceptions",
        "FreeDriveStr",
        "FreeTerminateProcs",
        "SysBeep"
    };
    for (size_t i = 0; i < sizeof(sysutils_hooks) / sizeof(sysutils_hooks[0]); ++i)
        register_simple_builtin_proc(symtab, sysutils_hooks[i]);

    char *move_proc = strdup("Move");
    if (move_proc != NULL) {
        ListNode_t *move_params = NULL;
        ListNode_t *move_tail = NULL;
        ListNode_t *param = NULL;

        param = semcheck_create_builtin_param_var("source", UNKNOWN_TYPE);
        if (param != NULL) {
            move_params = param;
            move_tail = param;
        }

        param = semcheck_create_builtin_param_var("dest", UNKNOWN_TYPE);
        if (param != NULL) {
            if (move_tail != NULL)
                move_tail->next = param;
            else
                move_params = param;
            move_tail = param;
        }

        param = semcheck_create_builtin_param("count", LONGINT_TYPE);
        if (param != NULL) {
            if (move_tail != NULL)
                move_tail->next = param;
            else
                move_params = param;
        }

        KgpcType *move_type = create_procedure_type(move_params, NULL);
        assert(move_type != NULL && "Failed to create Move procedure type");
        AddBuiltinProc_Typed(symtab, move_proc, move_type);
        destroy_kgpc_type(move_type);
        free(move_proc);
    }

    {
        static const int swap_types[] = {INT_TYPE, LONGINT_TYPE, INT64_TYPE};
        for (size_t i = 0; i < sizeof(swap_types) / sizeof(swap_types[0]); ++i)
            register_unary_builtin_func(symtab, "SwapEndian", "AValue", swap_types[i], swap_types[i]);
    }

    /* Builtin functions - functions have return types */
    {
        typedef struct { const char *name; int return_tag; } SimpleBuiltinFunc;
        static const SimpleBuiltinFunc simple_funcs[] = {
            {"Length",       LONGINT_TYPE},
            {"GetMem",       POINTER_TYPE},
            {"ArrayStringToPPchar", POINTER_TYPE},
            {"Copy",         STRING_TYPE},
            {"Concat",       STRING_TYPE},
            {"EOF",          BOOL},
            {"EOLN",         BOOL},
            {"SizeOf",       LONGINT_TYPE},
            {"Chr",          CHAR_TYPE},
            {"Ord",          LONGINT_TYPE},
            {"Odd",          BOOL},
            {"UpCase",       CHAR_TYPE},
            {"Sqr",          LONGINT_TYPE},
            {"Ln",           REAL_TYPE},
            {"Exp",          REAL_TYPE},
            {"RandomRange",  LONGINT_TYPE},
            {"High",         LONGINT_TYPE},
        };
        for (size_t i = 0; i < sizeof(simple_funcs) / sizeof(simple_funcs[0]); ++i)
            register_simple_builtin_func(symtab, simple_funcs[i].name, simple_funcs[i].return_tag);
    }
    /* Standard I/O file variables - stdin, stdout, stderr */
    /* These are Text file variables that can be passed to Write/WriteLn/Read/ReadLn */
    {
        char *stdin_name = strdup("stdin");
        if (stdin_name != NULL) {
            KgpcType *stdin_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stdin_type != NULL && "Failed to create stdin type");
            PushVarOntoScope_Typed(symtab, stdin_name, stdin_type);
            destroy_kgpc_type(stdin_type);
            /* Note: stdin_name ownership transferred to symtab, don't free */
        }
        char *stdout_name = strdup("stdout");
        if (stdout_name != NULL) {
            KgpcType *stdout_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stdout_type != NULL && "Failed to create stdout type");
            PushVarOntoScope_Typed(symtab, stdout_name, stdout_type);
            destroy_kgpc_type(stdout_type);
        }
        char *stderr_name = strdup("stderr");
        if (stderr_name != NULL) {
            KgpcType *stderr_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stderr_type != NULL && "Failed to create stderr type");
            PushVarOntoScope_Typed(symtab, stderr_name, stderr_type);
            destroy_kgpc_type(stderr_type);
        }
        /* Input and Output - standard Pascal file variables */
        int sys_unit_idx = unit_registry_add("System");
        char *input_name = strdup("Input");
        if (input_name != NULL) {
            KgpcType *input_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(input_type != NULL && "Failed to create Input type");
            PushVarOntoScope_Typed(symtab, input_name, input_type);
            destroy_kgpc_type(input_type);
            HashNode_t *input_node = NULL;
            if (FindIdent(&input_node, symtab, "Input") != -1 && input_node != NULL) {
                input_node->defined_in_unit = 1;
                input_node->unit_is_public = 1;
                mark_hashnode_source_unit(input_node, sys_unit_idx);
            }
        }
        char *output_name = strdup("Output");
        if (output_name != NULL) {
            KgpcType *output_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(output_type != NULL && "Failed to create Output type");
            PushVarOntoScope_Typed(symtab, output_name, output_type);
            destroy_kgpc_type(output_type);
            HashNode_t *output_node = NULL;
            if (FindIdent(&output_node, symtab, "Output") != -1 && output_node != NULL) {
                output_node->defined_in_unit = 1;
                output_node->unit_is_public = 1;
                mark_hashnode_source_unit(output_node, sys_unit_idx);
            }
        }
    }

    /* StringOfChar: function StringOfChar(c: Char; l: SizeInt): string
     *                       StringOfChar(c: WideChar; l: SizeInt): UnicodeString */
    {
        const char *func_name = "StringOfChar";
        KgpcType *ansi_return_type = create_primitive_type(STRING_TYPE);
        KgpcType *wide_return_type = create_primitive_type(STRING_TYPE);
        struct TypeAlias *wide_return_alias = calloc(1, sizeof(struct TypeAlias));
        if (wide_return_type != NULL && wide_return_alias != NULL)
        {
            wide_return_alias->alias_name = strdup("UnicodeString");
            wide_return_alias->target_type_id = strdup("UnicodeString");
            wide_return_alias->base_type = STRING_TYPE;
            wide_return_alias->is_wide_string = 1;
            kgpc_type_set_type_alias(wide_return_type, wide_return_alias);
        }

        /* Char + LongInt */
        ListNode_t *param_c = semcheck_create_builtin_param("c", CHAR_TYPE);
        ListNode_t *param_l = semcheck_create_builtin_param("l", LONGINT_TYPE);
        ListNode_t *params = ConcatList(param_c, param_l);
        KgpcType *func_type = create_procedure_type(params, ansi_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* Char + Int64 (SizeInt on 64-bit) */
        param_c = semcheck_create_builtin_param("c", CHAR_TYPE);
        param_l = semcheck_create_builtin_param("l", INT64_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, ansi_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* WideChar (Word) + LongInt */
        param_c = semcheck_create_builtin_param("c", WORD_TYPE);
        param_l = semcheck_create_builtin_param("l", LONGINT_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, wide_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* WideChar (Word) + Int64 (SizeInt on 64-bit) */
        param_c = semcheck_create_builtin_param("c", WORD_TYPE);
        param_l = semcheck_create_builtin_param("l", INT64_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, wide_return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        destroy_kgpc_type(ansi_return_type);
        destroy_kgpc_type(wide_return_type);
    }

    /* BinStr: function BinStr(Val: int64; cnt: byte): shortstring */
    {
        const char *func_name = "BinStr";
        ListNode_t *param_val = semcheck_create_builtin_param("Val", INT64_TYPE);
        ListNode_t *param_cnt = semcheck_create_builtin_param("cnt", BYTE_TYPE);
        ListNode_t *params = ConcatList(param_val, param_cnt);
        KgpcType *return_type = create_primitive_type(SHORTSTRING_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
        destroy_kgpc_type(return_type);
    }

    /* PopCnt: function PopCnt(AValue: QWord): Byte - counts set bits */
    {
        static const int popcnt_types[] = {BYTE_TYPE, WORD_TYPE, LONGWORD_TYPE, QWORD_TYPE};
        for (size_t i = 0; i < sizeof(popcnt_types) / sizeof(popcnt_types[0]); ++i)
            register_unary_builtin_func(symtab, "PopCnt", "AValue", popcnt_types[i], BYTE_TYPE);
    }

    /* IndexChar: function IndexChar(const buf; len: SizeInt; b: Char): SizeInt */
    {
        const char *func_name = "IndexChar";
        ListNode_t *param_buf = semcheck_create_builtin_param("buf", UNKNOWN_TYPE);
        ListNode_t *param_len = semcheck_create_builtin_param("len", LONGINT_TYPE);
        ListNode_t *param_b = semcheck_create_builtin_param("b", CHAR_TYPE);
        ListNode_t *params = ConcatList(ConcatList(param_buf, param_len), param_b);
        KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
        destroy_kgpc_type(return_type);
    }

    /* CompareByte: function CompareByte(const buf1, buf2; len: SizeInt): SizeInt */
    {
        const char *func_name = "CompareByte";
        ListNode_t *param_buf1 = semcheck_create_builtin_param("buf1", UNKNOWN_TYPE);
        ListNode_t *param_buf2 = semcheck_create_builtin_param("buf2", UNKNOWN_TYPE);
        ListNode_t *param_len = semcheck_create_builtin_param("len", LONGINT_TYPE);
        ListNode_t *params = ConcatList(ConcatList(param_buf1, param_buf2), param_len);
        KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
        destroy_kgpc_type(return_type);
    }

    /* UniqueString: procedure UniqueString(var S: String) */
    {
        const char *func_name = "UniqueString";
        ListNode_t *param_s = semcheck_create_builtin_param_var("S", STRING_TYPE);
        KgpcType *func_type = create_procedure_type(param_s, NULL);
        if (func_type != NULL)
        {
            AddBuiltinProc_Typed(symtab, (char *)func_name, func_type);
            destroy_kgpc_type(func_type);
        }
        if (param_s != NULL)
            DestroyList(param_s);
    }

    /* Bit scan builtins (FPC compatibility) */
    {
        struct {
            const char *name;
            int param_tag;
        } bsr_bsf[] = {
            {"BsrByte", BYTE_TYPE}, {"BsfByte", BYTE_TYPE},
            {"BsrWord", WORD_TYPE}, {"BsfWord", WORD_TYPE},
            {"BsrDWord", LONGWORD_TYPE}, {"BsfDWord", LONGWORD_TYPE},
            {"BsrQWord", QWORD_TYPE}, {"BsfQWord", QWORD_TYPE},
        };

        for (size_t i = 0; i < sizeof(bsr_bsf) / sizeof(bsr_bsf[0]); i++)
        {
            ListNode_t *param = semcheck_create_builtin_param("AValue", bsr_bsf[i].param_tag);
            KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
            KgpcType *func_type = create_procedure_type(param, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, (char *)bsr_bsf[i].name, func_type);
                destroy_kgpc_type(func_type);
            }
            if (param != NULL)
                DestroyList(param);
            destroy_kgpc_type(return_type);
        }
    }

    /* FPC compiler intrinsics for stack frame access.
     * These return Pointer and are used by error handling code.
     * Register both zero-arg and one-arg (Pointer) overloads to match FPC semantics.
     * Also register kgpc_get_frame (the runtime stub these get rewritten to). */
    {
        const char *frame_intrinsics[] = {
            "get_frame", "get_pc_addr", "get_caller_addr", "get_caller_frame",
            "Get_Frame", "Get_Caller_Addr", "Get_Caller_Frame",
            "kgpc_get_frame",
        };
        for (size_t i = 0; i < sizeof(frame_intrinsics) / sizeof(frame_intrinsics[0]); i++)
        {
            /* Zero-argument overload: get_frame() -> Pointer */
            {
                KgpcType *return_type = create_primitive_type(POINTER_TYPE);
                KgpcType *func_type = create_procedure_type(NULL, return_type);
                if (func_type != NULL)
                {
                    AddBuiltinFunction_Typed(symtab, (char *)frame_intrinsics[i], func_type);
                    destroy_kgpc_type(func_type);
                }
                destroy_kgpc_type(return_type);
            }
            /* One-argument overload: get_caller_addr(frame: Pointer) -> Pointer */
            {
                ListNode_t *param = semcheck_create_builtin_param("p", POINTER_TYPE);
                KgpcType *return_type = create_primitive_type(POINTER_TYPE);
                KgpcType *func_type = create_procedure_type(param, return_type);
                if (func_type != NULL)
                {
                    AddBuiltinFunction_Typed(symtab, (char *)frame_intrinsics[i], func_type);
                    destroy_kgpc_type(func_type);
                }
                if (param != NULL)
                    DestroyList(param);
                destroy_kgpc_type(return_type);
            }
        }
    }

    /* Builtins are now in system.p */

    /* Atomic operations: AtomicCmpExchange, AtomicExchange, AtomicIncrement, AtomicDecrement.
     * In FPC these are compiler intrinsics that work on any ordinal/pointer type.
     * We register them with Integer parameters as a common overload. */
    {
        /* AtomicCmpExchange(var Target: T; NewValue: T; Comparand: T): T */
        {
            static const int cmpxchg_types[] = {INT_TYPE, INT64_TYPE, POINTER_TYPE};
            for (size_t i = 0; i < sizeof(cmpxchg_types) / sizeof(cmpxchg_types[0]); ++i)
            {
                int t = cmpxchg_types[i];
                ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                ListNode_t *p2 = semcheck_create_builtin_param("NewValue", t);
                ListNode_t *p3 = semcheck_create_builtin_param("Comparand", t);
                p1->next = p2;
                p2->next = p3;
                KgpcType *return_type = create_primitive_type(t);
                KgpcType *func_type = create_procedure_type(p1, return_type);
                if (func_type != NULL)
                {
                    char *n1 = strdup("AtomicCmpExchange");
                    char *n2 = strdup("InterlockedCompareExchange");
                    AddBuiltinFunction_Typed(symtab, n1, func_type);
                    AddBuiltinFunction_Typed(symtab, n2, func_type);
                    destroy_kgpc_type(func_type);
                    free(n1);
                    free(n2);
                }
                DestroyList(p1);
                destroy_kgpc_type(return_type);
            }
        }
        /* AtomicExchange(var Target: T; Value: T): T
         * Target is a var parameter — must be passed by reference. */
        {
            static const int xchg_types[] = {INT_TYPE, INT64_TYPE, POINTER_TYPE};
            for (size_t i = 0; i < sizeof(xchg_types) / sizeof(xchg_types[0]); ++i)
            {
                int t = xchg_types[i];
                ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                ListNode_t *p2 = semcheck_create_builtin_param("Value", t);
                p1->next = p2;
                KgpcType *return_type = create_primitive_type(t);
                KgpcType *func_type = create_procedure_type(p1, return_type);
                if (func_type != NULL)
                {
                    char *n1 = strdup("AtomicExchange");
                    char *n2 = strdup("InterlockedExchange");
                    AddBuiltinFunction_Typed(symtab, n1, func_type);
                    AddBuiltinFunction_Typed(symtab, n2, func_type);
                    destroy_kgpc_type(func_type);
                    free(n1);
                    free(n2);
                }
                DestroyList(p1);
                destroy_kgpc_type(return_type);
            }
        }
        /* AtomicIncrement/AtomicDecrement(var Target: T[; Value: T]): T
         * Target is a var parameter — must be passed by reference.
         * In FPC these are compiler intrinsics that work on any ordinal type.
         * Register overloads for Integer and Int64 to cover common usage. */
        {
            const char *names[] = {
                "AtomicIncrement", "AtomicDecrement",
                "InterlockedIncrement", "InterlockedDecrement",
            };
            static const int atomic_types[] = {INT_TYPE, INT64_TYPE};
            for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); i++)
            {
                for (size_t ti = 0; ti < sizeof(atomic_types) / sizeof(atomic_types[0]); ti++)
                {
                    int t = atomic_types[ti];
                    /* One-arg overload: name(var Target: T): T */
                    {
                        char *dup_name = strdup(names[i]);
                        ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                        KgpcType *return_type = create_primitive_type(t);
                        KgpcType *func_type = create_procedure_type(p1, return_type);
                        if (func_type != NULL)
                        {
                            AddBuiltinFunction_Typed(symtab, dup_name, func_type);
                            destroy_kgpc_type(func_type);
                        }
                        if (p1 != NULL)
                            DestroyList(p1);
                        destroy_kgpc_type(return_type);
                        free(dup_name);
                    }
                    /* Two-arg overload: name(var Target: T; Value: T): T */
                    {
                        char *dup_name = strdup(names[i]);
                        ListNode_t *p1 = semcheck_create_builtin_param_var("Target", t);
                        ListNode_t *p2 = semcheck_create_builtin_param("Value", t);
                        p1->next = p2;
                        KgpcType *return_type = create_primitive_type(t);
                        KgpcType *func_type = create_procedure_type(p1, return_type);
                        if (func_type != NULL)
                        {
                            AddBuiltinFunction_Typed(symtab, dup_name, func_type);
                            destroy_kgpc_type(func_type);
                        }
                        DestroyList(p1);
                        destroy_kgpc_type(return_type);
                        free(dup_name);
                    }
                }
            }
        }
        /* bitsizeof(T): Integer - returns size in bits */
        {
            register_unary_builtin_func(symtab, "bitsizeof", "x", INT_TYPE, INT_TYPE);
            register_unary_builtin_func(symtab, "BitSizeOf", "x", INT_TYPE, INT_TYPE);
        }
        /* Finalize(var v): frees managed resources.
         * POINTER_TYPE is used as a placeholder parameter type; actual type
         * validation is bypassed in the builtin handler which accepts any type. */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("v", POINTER_TYPE);
            KgpcType *proc_type = create_procedure_type(p1, NULL);
            if (proc_type != NULL)
            {
                AddBuiltinProc_Typed(symtab, "Finalize", proc_type);
                destroy_kgpc_type(proc_type);
            }
            DestroyList(p1);
        }
        /* Initialize(var v): initializes managed resources.
         * POINTER_TYPE is used as a placeholder parameter type; actual type
         * validation is bypassed in the builtin handler which accepts any type. */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("v", POINTER_TYPE);
            KgpcType *proc_type = create_procedure_type(p1, NULL);
            if (proc_type != NULL)
            {
                AddBuiltinProc_Typed(symtab, "Initialize", proc_type);
                destroy_kgpc_type(proc_type);
            }
            DestroyList(p1);
        }
    }
}

/* Semantic check for a program */
#define SEMCHECK_TIMINGS_ENABLED() (kgpc_getenv("KGPC_DEBUG_TIMINGS") != NULL)

static double semcheck_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

static void semcheck_timing_step(const char *label, double *last_ms) {
    if (!SEMCHECK_TIMINGS_ENABLED() || last_ms == NULL)
        return;
    double now = semcheck_now_ms();
    fprintf(stderr, "[timing] semcheck_program %s: %.2f ms\n", label, now - *last_ms);
    *last_ms = now;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_PROGRAM_TYPE);

    return_val = 0;
    double t0 = 0.0;
    if (SEMCHECK_TIMINGS_ENABLED())
        t0 = semcheck_now_ms();

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_names_add_list(tree->tree_data.program_data.uses_units);
    semcheck_timing_step("unit names", &t0);

    return_val += semcheck_id_not_main(tree->tree_data.program_data.program_id);
    semcheck_timing_step("id check", &t0);

    /* TODO: Push program name onto scope */

    /* TODO: Fix line number bug here */
    return_val += semcheck_args(symtab, tree->tree_data.program_data.args_char,
      tree->line_num);
    semcheck_timing_step("args", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after args: %d\n", return_val);
#endif

    return_val += predeclare_enum_literals(symtab, tree->tree_data.program_data.type_declaration);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("predeclare types", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after type predeclare: %d\n", return_val);
#endif

    /* Predeclare subprograms so they can be referenced in const initializers. */
    return_val += predeclare_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("predeclare subprograms", &t0);
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
    {
        ListNode_t *debug_cur = tree->tree_data.program_data.type_declaration;
        while (debug_cur != NULL)
        {
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *debug_tree = (Tree_t *)debug_cur->cur;
                if (debug_tree->type == TREE_TYPE_DECL && debug_tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] program type decl: %s kind=%d\n",
                        debug_tree->tree_data.type_decl_data.id,
                        debug_tree->tree_data.type_decl_data.kind);
                }
            }
            debug_cur = debug_cur->next;
        }
    }
    
    /* Four-pass processing for constants to handle all reference patterns:
     *
     * Pass 1: Imported unit untyped constants.
     * Pass 2: Imported unit typed constants (e.g., DirectorySeparator from system.p).
     * Pass 3: Local untyped constants.
     * Pass 4: Local typed constants.
     */

    /* Pre-push trivially evaluable imported consts (literal integers, reals, etc.)
     * to handle cross-unit forward references due to merge ordering. */
    prepush_trivial_imported_consts(symtab, tree->tree_data.program_data.const_declaration);

    /* Pass 1: Imported unit untyped constants */
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.program_data.const_declaration);
    semcheck_timing_step("consts pass1 imported untyped", &t0);

    /* Pass 2: Imported unit typed constants */
    ListNode_t *unit_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 1);  /* from_unit_only=true */
    if (unit_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, unit_typed_consts);
        DestroyList(unit_typed_consts);
    }
    semcheck_timing_step("consts pass2 imported typed", &t0);

    /* Pass 3: Local untyped constants */
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.program_data.const_declaration);
    semcheck_timing_step("consts pass3 local untyped", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after consts: %d\n", return_val);
#endif

    /* Pass 4: Local typed constants (can reference both unit typed consts and local untyped consts) */
    ListNode_t *local_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 0);  /* from_unit_only=false */
    if (local_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, local_typed_consts);
        DestroyList(local_typed_consts);
    }
    semcheck_timing_step("consts pass4 local typed", &t0);

    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("type decls", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after types: %d\n", return_val);
#endif

    ListNode_t *program_vars = collect_non_typed_var_decls(tree->tree_data.program_data.var_declaration);
    if (program_vars != NULL)
    {
        return_val += semcheck_decls(symtab, program_vars);
        DestroyList(program_vars);
    }
    semcheck_timing_step("var decls", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after vars: %d\n", return_val);
#endif

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("subprograms", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after subprograms: %d\n", return_val);
#endif

    semcheck_refresh_generic_specialization_vmts(
        symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("refresh generic vmts", &t0);

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, INT_MAX);
    semcheck_timing_step("body", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after body: %d\n", return_val);
#endif

    // Semantic check finalization statements from units
    if (tree->tree_data.program_data.finalization_statements != NULL) {
        ListNode_t *final_node = tree->tree_data.program_data.finalization_statements;
        while (final_node != NULL) {
            if (final_node->type == LIST_STMT && final_node->cur != NULL) {
                struct Statement *final_stmt = (struct Statement *)final_node->cur;
                /* Set unit error context so errors show the correct unit
                 * instead of the main program file. */
                int saved_suppress = g_semcheck_error_suppress_source_index;
                const char *saved_unit_ctx = g_semcheck_error_unit_context;
                if (final_stmt->source_unit_index > 0) {
                    const char *uname = unit_registry_get(final_stmt->source_unit_index);
                    g_semcheck_error_unit_context = uname ? uname : "<unit>";
                    g_semcheck_error_suppress_source_index = 1;
                }
                return_val += semcheck_stmt(symtab, final_stmt, INT_MAX);
                g_semcheck_error_suppress_source_index = saved_suppress;
                g_semcheck_error_unit_context = saved_unit_ctx;
            }
            final_node = final_node->next;
        }
    }
    semcheck_timing_step("finalization", &t0);

    if(optimize_flag() > 0 && return_val == 0)
    {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] Before optimize: body_statement = %p\n", 
                    (void*)tree->tree_data.program_data.body_statement);
            if (tree->tree_data.program_data.body_statement != NULL) {
                fprintf(stderr, "[KGPC] Body statement type: %d\n", 
                        tree->tree_data.program_data.body_statement->type);
                if (tree->tree_data.program_data.body_statement->type == STMT_COMPOUND_STATEMENT) {
                    fprintf(stderr, "[KGPC] Compound statement list: %p\n",
                            (void*)tree->tree_data.program_data.body_statement->stmt_data.compound_statement);
                }
            }
        }
        optimize(symtab, tree);
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] After optimize: body_statement = %p\n", 
                    (void*)tree->tree_data.program_data.body_statement);
        }
    }

    /* Keep the outermost scope alive for code generation. DestroySymTab will clean it up. */
    semcheck_unit_names_reset();
    return return_val;
}

/* Semantic check for a unit */
int semcheck_unit(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    const char *debug_steps = kgpc_getenv("KGPC_DEBUG_SEMSTEPS");
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_name_add(tree->tree_data.unit_data.unit_id);
    if (tree->tree_data.unit_data.unit_id != NULL)
        g_semcheck_current_unit_index = unit_registry_add(tree->tree_data.unit_data.unit_id);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.interface_uses);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.implementation_uses);
    semcheck_mark_type_decl_units(tree->tree_data.unit_data.interface_type_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_type_decl_units(tree->tree_data.unit_data.implementation_type_decls,
        g_semcheck_current_unit_index);
    semcheck_mark_subprogram_units(tree->tree_data.unit_data.subprograms,
        g_semcheck_current_unit_index);

    /* Check interface section */
    int before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface type predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Check implementation section - predeclare types BEFORE subprograms */
    before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl type predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Now predeclare subprograms AFTER all types (interface + implementation) are predeclared.
     * This ensures function return types referencing implementation types can be resolved.
     * The same subprograms list is shared by interface and implementation. */
    before = return_val;

    /* Debug: dump subprograms list before predeclaration */
    if (kgpc_getenv("KGPC_DEBUG_SUBPROGRAMS_LIST") != NULL)
    {
        fprintf(stderr, "[SUBPROGRAMS_LIST] Dumping subprograms list for unit:\n");
        ListNode_t *debug_cur = tree->tree_data.unit_data.subprograms;
        int count = 0;
        while (debug_cur != NULL)
        {
            count++;
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *sub = (Tree_t *)debug_cur->cur;
                if (sub->type == TREE_SUBPROGRAM)
                {
                    fprintf(stderr, "[SUBPROGRAMS_LIST] %d: %s (line %d)\n",
                            count, sub->tree_data.subprogram_data.id, sub->line_num);
                }
            }
            debug_cur = debug_cur->next;
        }
        fprintf(stderr, "[SUBPROGRAMS_LIST] Total: %d subprograms\n", count);
    }

    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprogram predeclare +%d (total %d)\n",
                return_val - before, return_val);
    
    /* Continue interface section processing */
    /* Pre-push trivially evaluable imported consts for cross-unit forward references */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.interface_const_decls);
    /* Pass 1: Imported unit untyped constants. */
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.interface_const_decls, 1);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants (e.g., prelude/system typed consts). */
    before = return_val;
    ListNode_t *typed_iface_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 1);
    if (typed_iface_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_unit_consts);
        DestroyList(typed_iface_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.interface_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface consts +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.interface_const_decls, 0);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface deferred imported consts +%d (total %d)\n",
                return_val - before, return_val);
                
    /* Pass 4: Local interface typed constants - they can reference regular constants */
    before = return_val;
    ListNode_t *typed_iface_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 0);
    if (typed_iface_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_consts);
        DestroyList(typed_iface_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    ListNode_t *iface_vars = collect_non_typed_var_decls(tree->tree_data.unit_data.interface_var_decls);
    if (iface_vars != NULL)
    {
        return_val += semcheck_decls(symtab, iface_vars);
        DestroyList(iface_vars);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Continue implementation section processing */
    /* Pre-push trivially evaluable imported consts from implementation */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.implementation_const_decls);
    /* Pass 1: Imported unit untyped constants from implementation section. */
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.implementation_const_decls, 1);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants from implementation section. */
    before = return_val;
    ListNode_t *typed_impl_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 1);
    if (typed_impl_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_unit_consts);
        DestroyList(typed_impl_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.implementation_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl consts +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.implementation_const_decls, 0);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl deferred imported consts +%d (total %d)\n",
                return_val - before, return_val);
                
    /* Pass 4: Local implementation typed constants */
    before = return_val;
    ListNode_t *typed_impl_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 0);
    if (typed_impl_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_consts);
        DestroyList(typed_impl_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    ListNode_t *impl_vars = collect_non_typed_var_decls(tree->tree_data.unit_data.implementation_var_decls);
    if (impl_vars != NULL)
    {
        return_val += semcheck_decls(symtab, impl_vars);
        DestroyList(impl_vars);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Check subprograms */
    before = return_val;
    return_val += semcheck_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprograms +%d (total %d)\n",
                return_val - before, return_val);

    /* Check initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.initialization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] initialization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    /* Check finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.finalization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] finalization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    semcheck_unit_names_reset();
    return return_val;
}


/* Adds arguments to the symbol table */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num)
{
    ListNode_t *cur;
    int return_val, func_return;
    assert(symtab != NULL);

    return_val = 0;

    cur = args;

    /* Checking if they are declarations
        NOTE: Mismatching arg types is an error */
    if(cur != NULL)
        if(cur->type == LIST_TREE)
            return semcheck_decls(symtab, args);

    while(cur != NULL)
    {
        /* If not a list of declarations, must be a list of strings */
        assert(cur->type == LIST_STRING);

        /* UNTYPED procedure parameters - use NULL KgpcType */
        func_return = PushVarOntoScope_Typed(symtab, (char *)cur->cur, NULL);

        /* Greater than 0 signifies an error */
        if(func_return > 0)
        {
            semcheck_error_with_context("Error on line %d, redeclaration of name %s!\n",
                line_num, (char *)cur->cur);
            return_val += func_return;
        }

        cur = cur->next;
    }

    return return_val;
}

/* Pushes a bunch of declarations onto the current scope */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls)
{
    ListNode_t *cur, *ids, *ids_head;
    Tree_t *tree;
    int return_val, func_return;

    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = decls;
    while(cur != NULL)
    {
        /* Any declaration is always a tree */
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_VAR_DECL || tree->type == TREE_ARR_DECL);

        if (tree->type == TREE_VAR_DECL)
            ids_head = tree->tree_data.var_decl_data.ids;
        else
            ids_head = tree->tree_data.arr_decl_data.ids;

        ids = ids_head;

        HashNode_t *resolved_type = NULL;
        int owner_type_match = 0;
        const TypeRef *decl_type_ref = NULL;
        const char *decl_type_id = NULL;
        const char *decl_type_base = NULL;
        if (tree->type == TREE_VAR_DECL)
        {
            decl_type_ref = tree->tree_data.var_decl_data.type_ref;
            decl_type_id = tree->tree_data.var_decl_data.type_id;
            decl_type_base = (decl_type_ref != NULL)
                ? type_ref_base_name(decl_type_ref)
                : decl_type_id;
        }
        if (tree->type == TREE_VAR_DECL && decl_type_id != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_PSHORTSTRING") != NULL &&
                pascal_identifier_equals(decl_type_id, "pshortstring"))
            {
                int count = 0;
                ListNode_t *matches = FindAllIdents(symtab, decl_type_id);
                for (ListNode_t *m = matches; m != NULL; m = m->next)
                {
                    HashNode_t *n = (HashNode_t *)m->cur;
                    if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                    {
                        fprintf(stderr,
                            "[PShortString] lookup match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                            n->id ? n->id : "<null>", (void *)n->type,
                            n->defined_in_unit, n->source_unit_index);
                        count++;
                    }
                }
                if (matches != NULL)
                    DestroyList(matches);
                fprintf(stderr, "[PShortString] lookup total matches=%d\n", count);
            }
            if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL &&
                (pascal_identifier_equals(decl_type_id, "TFloatFormatProfile") ||
                 pascal_identifier_equals(decl_type_id, "TReal_Type")))
            {
                int count = 0;
                ListNode_t *matches = FindAllIdents(symtab, decl_type_id);
                for (ListNode_t *m = matches; m != NULL; m = m->next)
                {
                    HashNode_t *n = (HashNode_t *)m->cur;
                    if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                    {
                        fprintf(stderr,
                            "[TFLOAT] lookup match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                            n->id ? n->id : "<null>", (void *)n->type,
                            n->defined_in_unit, n->source_unit_index);
                        count++;
                    }
                }
                if (matches != NULL)
                    DestroyList(matches);
                fprintf(stderr, "[TFLOAT] lookup total matches=%d for %s\n",
                    count, decl_type_id);
            }
            if (tree->tree_data.var_decl_data.defined_in_unit)
            {
                /* Imported declarations must stay bound to imported symbols.
                 * When the subprogram's source unit is known, prefer a type
                 * that was visible at the function's declaration site:
                 *  1) Exact match: type from the same source unit
                 *  2) Closest ancestor: type with highest source_unit_index
                 *     that is <= the function's source unit index (i.e., a
                 *     type from a unit loaded before/with the function's unit)
                 *  3) Generic fallback */
                resolved_type = NULL;
                if (g_semcheck_imported_decl_unit_index > 0 && decl_type_id != NULL)
                {
                    HashNode_t *closest_ancestor = NULL;
                    int closest_idx = 0;
                    ListNode_t *candidates = FindAllIdents(symtab, decl_type_id);
                    ListNode_t *c = candidates;
                    int debug_imported = (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                                         pascal_identifier_equals(decl_type_id, "TSize"));
                    if (debug_imported)
                        fprintf(stderr, "[IMPORTED_DECL] TSize lookup, imported_unit_idx=%d\n",
                            g_semcheck_imported_decl_unit_index);
                    while (c != NULL)
                    {
                        HashNode_t *n = (HashNode_t *)c->cur;
                        if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                        {
                            if (debug_imported)
                                fprintf(stderr, "[IMPORTED_DECL]   candidate src_idx=%d kind=%d\n",
                                    n->source_unit_index, n->type ? n->type->kind : -1);
                            if (n->source_unit_index == g_semcheck_imported_decl_unit_index)
                            {
                                resolved_type = n;
                                break;
                            }
                            /* Prefer symbol from a unit that the importing unit uses */
                            if (n->source_unit_index > 0 &&
                                unit_registry_is_dep(g_semcheck_imported_decl_unit_index,
                                                     n->source_unit_index))
                            {
                                closest_ancestor = n;
                                closest_idx = n->source_unit_index;
                                /* Don't break — exact match still wins */
                            }
                            else if (closest_ancestor == NULL &&
                                     n->source_unit_index > 0 &&
                                     n->source_unit_index <= g_semcheck_imported_decl_unit_index)
                            {
                                /* Fallback: highest index still <= function's unit */
                                closest_ancestor = n;
                                closest_idx = n->source_unit_index;
                            }
                        }
                        c = c->next;
                    }
                    if (debug_imported)
                        fprintf(stderr, "[IMPORTED_DECL]   result: exact=%p ancestor=%p (idx=%d)\n",
                            (void *)resolved_type, (void *)closest_ancestor, closest_idx);
                    if (resolved_type == NULL && closest_ancestor != NULL)
                        resolved_type = closest_ancestor;
                    if (candidates != NULL)
                        DestroyList(candidates);
                }
                if (resolved_type == NULL)
                    resolved_type = semcheck_find_type_node_with_unit_flag_ref(symtab,
                        decl_type_ref, decl_type_id, 1);
            }
            else
            {
                /* In method parameter declarations, a type_id matching the current
                 * owner must bind to that owner type before generic lookup. This
                 * prevents imported aliases with the same name from corrupting
                 * Self/parameter record field resolution. */
                const char *owner_id = semcheck_get_current_method_owner();
                const char *owner_innermost = semcheck_get_current_subprogram_owner_class();
                if (owner_id != NULL &&
                    (decl_type_base != NULL &&
                     (pascal_identifier_equals(decl_type_base, owner_id) ||
                      (owner_innermost != NULL && pascal_identifier_equals(decl_type_base, owner_innermost)))))
                {
                    owner_type_match = 1;
                    if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                        semcheck_kgpc_type_is_record_like(
                            tree->tree_data.var_decl_data.cached_kgpc_type))
                    {
                        /* Method-owner parameters (Self / owner-typed args) may already
                         * carry precise record KgpcType from parser conversion.
                         * Never rebind them through ambiguous type_id lookup. */
                        resolved_type = NULL;
                    }
                    else
                    {
                    resolved_type = semcheck_find_owner_record_type_node(symtab, owner_id);
                    if (resolved_type == NULL && owner_innermost != NULL)
                        resolved_type = semcheck_find_owner_record_type_node(symtab, owner_innermost);
                    if (resolved_type == NULL)
                        resolved_type = semcheck_find_preferred_type_node(symtab, owner_id);
                    }
                }
                if (resolved_type == NULL && !owner_type_match)
                    resolved_type = semcheck_find_preferred_type_node_with_ref(symtab,
                        decl_type_ref, decl_type_id);
            }
        }
        if (tree->type == TREE_VAR_DECL &&
            decl_type_id != NULL &&
            resolved_type == NULL)
        {
            const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
            const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
            if (owner_full == NULL)
                owner_full = semcheck_get_current_method_owner();
            if (owner_full != NULL)
            {
                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                    &tree->tree_data.var_decl_data.type_id,
                    &tree->tree_data.var_decl_data.type_ref);
                decl_type_ref = tree->tree_data.var_decl_data.type_ref;
                decl_type_id = tree->tree_data.var_decl_data.type_id;
                decl_type_base = (decl_type_ref != NULL)
                    ? type_ref_base_name(decl_type_ref)
                    : decl_type_id;
                if (decl_type_id != NULL)
                {
                    resolved_type = semcheck_find_preferred_type_node_with_ref(symtab,
                        decl_type_ref, decl_type_id);
                }
            }
        }
        if (tree->type == TREE_VAR_DECL)
        {
            int keep_imported_cached =
                (tree->tree_data.var_decl_data.defined_in_unit &&
                 tree->tree_data.var_decl_data.cached_kgpc_type != NULL);
            if (resolved_type != NULL && resolved_type->type != NULL)
            {
                if (keep_imported_cached)
                {
                    /* Imported declarations should retain their original resolved type.
                     * Rebinding in later scopes can corrupt aliases like BaseUnix.TSize. */
                }
                else
                {
                /* Clear any pre-existing cached type if we're replacing with resolved type */
                if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                {
                    destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                    tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                }
                kgpc_type_retain(resolved_type->type);
                tree->tree_data.var_decl_data.cached_kgpc_type = resolved_type->type;
                }
            }
            else if (resolved_type != NULL && resolved_type->type == NULL && decl_type_id != NULL)
            {
                const char *type_name = decl_type_base != NULL ? decl_type_base : decl_type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_name);
                if (builtin_tag == STRING_TYPE || builtin_tag == SHORTSTRING_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type =
                        create_primitive_type(builtin_tag);
                }
            }
            else if (tree->tree_data.var_decl_data.cached_kgpc_type == NULL &&
                     resolved_type == NULL && decl_type_id != NULL)
            {
                /* Fallback: Create KgpcType for built-in type names not in symbol table */
                const char *type_id = decl_type_base != NULL ? decl_type_base : decl_type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                if (builtin_tag != UNKNOWN_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type = create_primitive_type(builtin_tag);
                }
                else if (tree->tree_data.var_decl_data.type_ref != NULL &&
                         tree->tree_data.var_decl_data.type_ref->num_generic_args > 0)
                {
                    /* Handle parameterized string types like AnsiString(CP_NONE) */
                    const char *base_name = decl_type_base != NULL ? decl_type_base : decl_type_id;
                    int base_tag = semcheck_map_builtin_type_name_local(base_name);
                    if (base_tag == STRING_TYPE || base_tag == SHORTSTRING_TYPE)
                    {
                        tree->tree_data.var_decl_data.cached_kgpc_type =
                            create_primitive_type(base_tag);
                    }
                    else
                    {
                        tree->tree_data.var_decl_data.cached_kgpc_type =
                            create_primitive_type(POINTER_TYPE);
                    }
                }
            }
            /* Note: If cached_kgpc_type is already set (e.g., for inline procedure types from parser),
             * preserve it rather than destroying it. */
            /* AUDIT: Log when KgpcType is missing for a type_id */
            if (kgpc_getenv("KGPC_AUDIT_TYPES") != NULL && tree->tree_data.var_decl_data.type_id != NULL)
            {
                const char *var_name = (ids_head && ids_head->cur) ? (char*)ids_head->cur : "<unknown>";
                const char *type_id = tree->tree_data.var_decl_data.type_id;
                KgpcType *kgpc = tree->tree_data.var_decl_data.cached_kgpc_type;
                if (kgpc == NULL)
                {
                    fprintf(stderr, "[TYPE_AUDIT] MISSING: var=%s type_id=%s resolved_node=%p\n",
                        var_name, type_id, (void*)resolved_type);
                }
                else if (kgpc->kind == TYPE_KIND_PRIMITIVE && kgpc->info.primitive_type_tag == POINTER_TYPE)
                {
                    /* Pointer type but no points_to info - this is a problem */
                    fprintf(stderr, "[TYPE_AUDIT] INCOMPLETE_PTR: var=%s type_id=%s kind=PRIMITIVE tag=POINTER (should be TYPE_KIND_POINTER)\n",
                        var_name, type_id);
                }
            }
        }
        int skip_initializer = 0;
        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (tree->tree_data.var_decl_data.is_typed_const)
                {
                    HashNode_t *existing_node = NULL;
                    if (FindIdent(&existing_node, symtab, ids->cur) >= 0 &&
                        existing_node != NULL && existing_node->is_typed_const)
                    {
                        mark_hashnode_unit_info(symtab, existing_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                        skip_initializer = 1;
                        goto next_identifier;
                    }
                }
                if (ids->cur != NULL && strcmp((char *)ids->cur, "Sock") == 0) {
#ifdef DEBUG
                     fprintf(stderr, "DEBUG: semcheck_decls processing Sock. type_id=%s resolved_type=%p\n",
                         tree->tree_data.var_decl_data.type_id ? tree->tree_data.var_decl_data.type_id : "<null>",
                         resolved_type);
#endif
                }

                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.is_typed_const)
                        fprintf(stderr, "[SemCheck] Typed const with type_id: %s, var: %s\n",
                            tree->tree_data.var_decl_data.type_id,
                            ids && ids->cur ? (char*)ids->cur : "<null>");
                    HashNode_t *type_node = resolved_type;
                    const char *type_id = tree->tree_data.var_decl_data.type_id;
                    int declared_type = tree->tree_data.var_decl_data.type;
                    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                        pascal_identifier_equals((char*)ids->cur, "Self"))
                    {
                        fprintf(stderr, "[KGPC] semcheck_decls Self: type_id=%s declared_type=%d type_node=%p type_node->type=%p kind=%d\n",
                            type_id ? type_id : "<null>", declared_type, (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            type_node && type_node->type ? type_node->type->kind : -1);
                    }
                    if (kgpc_getenv("KGPC_DEBUG_VAR_TYPES") != NULL && ids && ids->cur)
                    {
                        fprintf(stderr,
                            "[KGPC] semcheck_decls var=%s type_id=%s declared_type=%d type_node=%p type_node->type=%p cached=%p cached_kind=%d\n",
                            (char*)ids->cur,
                            type_id ? type_id : "<null>",
                            declared_type,
                            (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            (void*)tree->tree_data.var_decl_data.cached_kgpc_type,
                            tree->tree_data.var_decl_data.cached_kgpc_type ?
                                tree->tree_data.var_decl_data.cached_kgpc_type->kind : -1);
                    }

                    if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                        (type_node == NULL || type_node->type == NULL))
                    {
                        KgpcType *cached = tree->tree_data.var_decl_data.cached_kgpc_type;
                        kgpc_type_retain(cached);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, cached);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }

                    if (declared_type == SET_TYPE)
                    {
                        KgpcType *set_type = create_primitive_type(SET_TYPE);
                        if (set_type != NULL &&
                            tree->tree_data.var_decl_data.inline_type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(set_type,
                                tree->tree_data.var_decl_data.inline_type_alias);
                        }
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        if (set_type != NULL)
                        {
                            kgpc_type_retain(set_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = set_type;
                        }
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, set_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }

                    /* If declared as pointer type (^TypeName), handle inline pointer */
                    if (declared_type == POINTER_TYPE)
                    {
                        /* This is ^TypeName where TypeName is stored in type_id.
                         * We need to create a pointer type pointing to TypeName. */
                        KgpcType *points_to = NULL;
                        
                        /* Try to resolve the target type */
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            kgpc_type_retain(type_node->type);
                            points_to = type_node->type;
                        }
                        else
                        {
                            /* Check builtin types for the pointer target */
                            int target_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (target_tag != UNKNOWN_TYPE)
                            {
                                points_to = create_primitive_type(target_tag);
                            }
                            else
                            {
                                /* Unknown target type - create a placeholder */
                                points_to = NULL;
                            }
                        }
                        
                        KgpcType *pointer_type = create_pointer_type(points_to);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }

                    /* Check if it's a builtin type even if not in symbol table */
                    if (type_node == NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur)
                            fprintf(stderr, "[KGPC] semcheck_decls: %s has type_id=%s but type_node is NULL\n",
                                (char*)ids->cur, type_id);
                        /* Use the central type-name mapper first */
                        int tag = semcheck_map_builtin_type_name_local(type_id);
                        if (tag == SHORTSTRING_TYPE)
                        {
                            /* ShortString is array[0..255] of Char with length at index 0 */
                            var_type = HASHVAR_ARRAY;
                        }
                        else if (tag != UNKNOWN_TYPE)
                        {
                            var_type = map_type_tag_to_var_type(tag);
                        }
                        /* Handle FPC system pointer types (PInt64, PByte, etc.) */
                        else if (semcheck_is_builtin_pointer_type_id(type_id))
                            var_type = HASHVAR_POINTER;
                        else if (tree->tree_data.var_decl_data.type_ref != NULL &&
                                 tree->tree_data.var_decl_data.type_ref->num_generic_args > 0)
                            var_type = HASHVAR_POINTER;
                        /* Try resolving via TypeRef (handles unit-qualified types
                         * like baseunix.stat where the qualifier is structural). */
                        else if (decl_type_ref != NULL)
                        {
                            HashNode_t *ref_node = semcheck_find_preferred_type_node_with_ref(
                                symtab, decl_type_ref, type_id);
                            if (ref_node != NULL)
                            {
                                type_node = ref_node;
                                goto var_decl_type_resolved;
                            }
                            else
                            {
                                semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                                return_val++;
                                var_type = HASHVAR_UNTYPED;
                            }
                        }
                        else
                        {
                            semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                            return_val++;
                            var_type = HASHVAR_UNTYPED;
                        }
                    }
                    var_decl_type_resolved:
                    if (type_node != NULL)
                    {
                        if (type_node->type == NULL && type_id != NULL)
                        {
                            int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (builtin_tag != UNKNOWN_TYPE)
                            {
                                KgpcType *builtin_type = create_primitive_type(builtin_tag);
                                func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, builtin_type);
                                if (func_return == 0)
                                {
                                    HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                    if (var_node != NULL)
                                    {
                                        var_node->is_var_parameter =
                                            (tree->tree_data.var_decl_data.is_var_param ||
                                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                        mark_hashnode_unit_info(symtab, var_node,
                                            tree->tree_data.var_decl_data.defined_in_unit,
                                            tree->tree_data.var_decl_data.unit_is_public);
                                        mark_hashnode_source_unit(var_node,
                                            tree->tree_data.var_decl_data.source_unit_index);
                                    }
                                }
                                goto next_identifier;
                            }
                        }

                        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            kgpc_type_retain(type_node->type);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                if (var_node != NULL)
                                {
                                    var_node->is_var_parameter =
                                        (tree->tree_data.var_decl_data.is_var_param ||
                                         tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                    mark_hashnode_source_unit(var_node,
                                        tree->tree_data.var_decl_data.source_unit_index);
                                }
                            }
                            goto next_identifier;
                        }

                        int declared_type_tag = tree->tree_data.var_decl_data.type;
                        int needs_inline_pointer = (declared_type_tag == POINTER_TYPE &&
                            (type_node->type == NULL || type_node->type->kind != TYPE_KIND_POINTER));

                        if (needs_inline_pointer)
                        {
                            KgpcType *points_to = NULL;
                            if (type_node->type != NULL)
                            {
                                kgpc_type_retain(type_node->type);
                                points_to = type_node->type;
                            }
                            else
                            {
                                struct RecordType *target_record = get_record_type_from_node(type_node);
                                if (target_record != NULL)
                                {
                                    points_to = create_record_type(target_record);
                                }
                                else
                                {
                                    enum VarType target_var_type = get_var_type_from_node(type_node);
                                    int primitive_tag = map_var_type_to_type_tag(target_var_type);
                                    if (primitive_tag != UNKNOWN_TYPE)
                                        points_to = create_primitive_type(primitive_tag);
                                }
                            }

                            KgpcType *pointer_type = create_pointer_type(points_to);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                if (var_node != NULL)
                                {
                                    var_node->is_var_parameter =
                                        (tree->tree_data.var_decl_data.is_var_param ||
                                         tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                    mark_hashnode_source_unit(var_node,
                                        tree->tree_data.var_decl_data.source_unit_index);
                                }
                            }
                            goto next_identifier;
                        }

                        var_type = get_var_type_from_node(type_node);
                        int resolved_tag = map_var_type_to_type_tag(var_type);
                        if (resolved_tag != UNKNOWN_TYPE)
                            tree->tree_data.var_decl_data.type = resolved_tag;
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL && alias->is_array)
                        {
                            /* Ensure array bounds are resolved for aliases using symbolic dimensions. */
                            if (alias->array_dimensions != NULL &&
                                type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_ARRAY)
                            {
                                resolve_array_bounds_in_kgpctype(symtab, type_node->type, alias);
                            }

                            /* Prefer the KgpcType from the type node if it is already an array. */
                            if (type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_ARRAY)
                            {
                                kgpc_type_retain(type_node->type);
                                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                                kgpc_type_release(type_node->type);  /* Release caller's ref */
                                if (func_return == 0)
                                {
                                    HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                    if (var_node != NULL)
                                    {
                                        mark_hashnode_unit_info(symtab, var_node,
                                            tree->tree_data.var_decl_data.defined_in_unit,
                                            tree->tree_data.var_decl_data.unit_is_public);
                                        mark_hashnode_source_unit(var_node,
                                            tree->tree_data.var_decl_data.source_unit_index);
                                    }
                                }

                                goto next_identifier;
                            }

                            int start = alias->array_start;
                            int end = alias->array_end;
                            if (alias->is_open_array)
                            {
                                start = 0;
                                end = -1;
                            }

                            /* Get element type - it might be a primitive type or a type reference */
                            KgpcType *element_type = NULL;
                            int element_type_tag = alias->array_element_type;

                            /* If element type is a type reference, resolve it */
                            if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                            {
                                HashNode_t *element_type_node = NULL;
                                if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                    element_type_node != NULL && element_type_node->type != NULL)
                                {
                                    element_type = element_type_node->type;
                                    /* CRITICAL: Retain element_type since it's borrowed from symbol table
                                     * and create_array_type takes ownership. Without this, both the symbol
                                     * table and array_type would try to free the same type, causing double-free. */
                                    kgpc_type_retain(element_type);
                                }
                            }
                            else if (element_type_tag != UNKNOWN_TYPE)
                            {
                                /* Direct primitive type tag - use create_primitive_type */
                                element_type = create_primitive_type(element_type_tag);
                            }

                            /* If element type is still NULL, create an unknown type to avoid crash */
                            if (element_type == NULL)
                            {
                                element_type = create_primitive_type(UNKNOWN_TYPE);
                            }

                            /* Create array KgpcType - takes ownership of element_type */
                            KgpcType *array_type = create_array_type(element_type, start, end);
                            assert(array_type != NULL && "Failed to create array type");
                            
                            /* Set type_alias on KgpcType so it's properly propagated */
                            kgpc_type_set_type_alias(array_type, alias);
                            
                            func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                            destroy_kgpc_type(array_type);  /* Release creator's ref */
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                                if (var_node != NULL)
                                {
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                    mark_hashnode_source_unit(var_node,
                                        tree->tree_data.var_decl_data.source_unit_index);
                                }
                            }

                            goto next_identifier;
                        }

                        /* For non-array type references (e.g., enum, set, file, record), create KgpcType from type_node */
                        KgpcType *var_kgpc_type = NULL;
                        if (type_node->type != NULL)
                        {
                            var_kgpc_type = type_node->type;
                            /* If the type node has a primitive KgpcType but the alias says
                             * it's a pointer, rebuild as a proper pointer type so downstream
                             * pointer dereferences resolve correctly. */
                            if (var_kgpc_type->kind == TYPE_KIND_PRIMITIVE && alias != NULL &&
                                alias->is_pointer)
                            {
                                var_kgpc_type = create_kgpc_type_from_type_alias(
                                    alias, symtab, tree->tree_data.var_decl_data.defined_in_unit);
                                if (var_kgpc_type == NULL)
                                {
                                    var_kgpc_type = type_node->type;
                                    kgpc_type_retain(var_kgpc_type);
                                }
                            }
                            else
                            {
                                kgpc_type_retain(var_kgpc_type);
                            }
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                        }
                        else
                        {
                            /* Fallback: create KgpcType from legacy fields using helpers */
                            struct RecordType *record_type = get_record_type_from_node(type_node);
                            struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                            if (type_alias != NULL)
                            {
                                var_kgpc_type = create_kgpc_type_from_type_alias(
                                    type_alias, symtab, tree->tree_data.var_decl_data.defined_in_unit);
                            }
                            if (var_kgpc_type != NULL)
                            {
                                /* No further legacy handling needed */
                            }
                            else if (type_id != NULL && semcheck_is_builtin_pointer_type_id(type_id))
                            {
                                var_type = HASHVAR_POINTER;
                                var_kgpc_type = create_pointer_type(NULL);
                            }
                            else if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                var_kgpc_type = create_record_type(record_type);
                            }
                            else if (var_type == HASHVAR_POINTER)
                            {
                                /* For pointer types, we need to create a pointer KgpcType */
                                /* Get the TypeAlias to find what the pointer points to */
                                if (type_alias != NULL && type_alias->is_pointer)
                                {
                                    KgpcType *points_to = NULL;
                                    
                                    /* Try to resolve the target type */
                                    if (type_alias->pointer_type_id != NULL)
                                    {
                                        HashNode_t *target_node = NULL;
                                        if (FindIdent(&target_node, symtab, type_alias->pointer_type_id) >= 0 &&
                                            target_node != NULL && target_node->type != NULL)
                                        {
                                            kgpc_type_retain(target_node->type);
                                            points_to = target_node->type;
                                        }
                                    }
                                    
                                    /* If we couldn't resolve it, create a placeholder based on pointer_type */
                                    if (points_to == NULL && type_alias->pointer_type != UNKNOWN_TYPE)
                                    {
                                        points_to = create_primitive_type(type_alias->pointer_type);
                                    }
                                    
                                    if (points_to != NULL)
                                    {
                                        var_kgpc_type = create_pointer_type(points_to);
                                        kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                                    }
                                }
                            }
                            else
                            {
                                var_kgpc_type = kgpc_type_from_var_type(var_type);
                            }

                            if (var_kgpc_type != NULL && type_alias != NULL && var_type != HASHVAR_POINTER)
                            {
                                kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                            }
                            
                            /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                            if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                                pascal_identifier_equals((char*)ids->cur, "Self"))
                                fprintf(stderr, "[KGPC] semcheck_decls: PushVarOntoScope_Typed for Self returned %d\n", func_return);
                        }
                        /* Release caller's ref (either created or retained above) */
                        destroy_kgpc_type(var_kgpc_type);

                        if (func_return == 0)
                        {
                            HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                            if (var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                                mark_hashnode_source_unit(var_node,
                                    tree->tree_data.var_decl_data.source_unit_index);
                            }
                        }
                        goto next_identifier;
                    }
                }
                else if (tree->tree_data.var_decl_data.inferred_type)
                {
                    /* For type inference, use INTEGER as placeholder - will be replaced later */
                    var_type = HASHVAR_INTEGER;  /* Placeholder */
                }
                else if (tree->tree_data.var_decl_data.type == UNKNOWN_TYPE &&
                    tree->tree_data.var_decl_data.type_id == NULL &&
                    tree->tree_data.var_decl_data.is_untyped_param)
                {
                    var_type = HASHVAR_UNTYPED;
                }
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else if(tree->tree_data.var_decl_data.type == BOOL)
                    var_type = HASHVAR_BOOLEAN;
                else if(tree->tree_data.var_decl_data.type == SET_TYPE)
                    var_type = HASHVAR_SET;
                else if(tree->tree_data.var_decl_data.type == ENUM_TYPE)
                    var_type = HASHVAR_ENUM;
                else if(tree->tree_data.var_decl_data.type == STRING_TYPE)
                    var_type = HASHVAR_PCHAR;
                else if(tree->tree_data.var_decl_data.type == RECORD_TYPE)
                    var_type = HASHVAR_RECORD;
                else if(tree->tree_data.var_decl_data.type == FILE_TYPE)
                    var_type = HASHVAR_FILE;
                else if(tree->tree_data.var_decl_data.type == TEXT_TYPE)
                    var_type = HASHVAR_TEXT;
                else if(tree->tree_data.var_decl_data.type == PROCEDURE)
                    var_type = HASHVAR_PROCEDURE;
                else
                    var_type = HASHVAR_REAL;
                
                /* Create KgpcType for typed variables */
                KgpcType *var_kgpc_type = NULL;
                int var_kgpc_borrowed = 0;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use KgpcType from resolved type if available */
                    var_kgpc_type = resolved_type->type;
                    var_kgpc_borrowed = 1;
                }
                else if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                {
                    /* Use pre-cached KgpcType (e.g., for inline procedure types) */
                    var_kgpc_type = tree->tree_data.var_decl_data.cached_kgpc_type;
                    var_kgpc_borrowed = 1;
                }
                else if (tree->tree_data.var_decl_data.inline_record_type != NULL)
                {
                    /* Handle inline record type declarations */
                    var_kgpc_type = create_record_type(clone_record_type(tree->tree_data.var_decl_data.inline_record_type));
                }
                else
                {
                    /* Special handling for ShortString - create as array[0..255] of Char */
                    if (var_type == HASHVAR_ARRAY &&
                        tree->tree_data.var_decl_data.type_id != NULL &&
                        pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"))
                    {
                        /* Create ShortString as array[0..255] of Char */
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        var_kgpc_type = create_array_type(char_type, 0, 255);
                        if (var_kgpc_type != NULL)
                        {
                            struct TypeAlias alias = {0};
                            alias.is_array = 1;
                            alias.array_start = 0;
                            alias.array_end = 255;
                            alias.array_element_type = CHAR_TYPE;
                            alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            alias.is_shortstring = 1;
                            kgpc_type_set_type_alias(var_kgpc_type, &alias);
                        }
                    }
                    /* Handle inline array types (e.g., array[0..2] of PChar) */
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                             tree->tree_data.var_decl_data.inline_type_alias->is_array)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                            fprintf(stderr, "[SemCheck] Processing inline array for var: %s\n",
                                ids && ids->cur ? (char*)ids->cur : "<null>");
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        int start = alias->array_start;
                        int end = alias->array_end;
                        if (alias->is_open_array)
                        {
                            start = 0;
                            end = -1;
                        }

                        /* Get element type */
                        KgpcType *element_type = NULL;
                        int element_type_tag = alias->array_element_type;
                        int element_type_borrowed = 0;  /* Track if borrowed from symbol table */

                        if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                        {
                            HashNode_t *element_type_node = NULL;
                            if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                element_type_node != NULL && element_type_node->type != NULL)
                            {
                                element_type = element_type_node->type;
                                element_type_borrowed = 1;
                            }
                            else
                            {
                                /* Check for builtin type */
                                int builtin_tag = semcheck_map_builtin_type_name_local(alias->array_element_type_id);
                                if (builtin_tag != UNKNOWN_TYPE)
                                    element_type = create_primitive_type(builtin_tag);
                            }
                        }
                        else if (element_type_tag != UNKNOWN_TYPE)
                        {
                            element_type = create_primitive_type(element_type_tag);
                        }

                        if (element_type != NULL)
                        {
                            /* CRITICAL: Retain element_type if borrowed from symbol table
                             * since create_array_type takes ownership. */
                            if (element_type_borrowed)
                                kgpc_type_retain(element_type);
                            var_kgpc_type = create_array_type(element_type, start, end);
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                        }
                        else
                        {
                            /* Fallback to var_type if element type unresolved */
                            var_kgpc_type = kgpc_type_from_var_type(var_type);
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL)
                    {
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        if (alias->storage_size > 0)
                        {
                            var_kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        }
                        else
                        {
                            var_kgpc_type = create_primitive_type(alias->base_type);
                        }
                        if (var_kgpc_type != NULL)
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                    }
                    else
                    {
                        /* Create KgpcType from var_type */
                        var_kgpc_type = kgpc_type_from_var_type(var_type);
                        if (var_kgpc_type == NULL && var_type == HASHVAR_POINTER)
                        {
                            var_kgpc_type = create_pointer_type(NULL);
                        }
                    }
                    
                    if (var_kgpc_type != NULL)
                    {
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        kgpc_type_retain(var_kgpc_type);
                        tree->tree_data.var_decl_data.cached_kgpc_type = var_kgpc_type;
                    }

                    /* Add metadata from resolved_type if present */
                    if (var_kgpc_type != NULL && resolved_type != NULL)
                    {
                        struct TypeAlias *type_alias = get_type_alias_from_node(resolved_type);
                        if (type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                        }
                        struct RecordType *record_type = get_record_type_from_node(resolved_type);
                        if (record_type != NULL && var_kgpc_type->kind == TYPE_KIND_RECORD)
                        {
                            /* Use the canonical RecordType from the symbol table,
                             * not a clone. This ensures type identity checks work correctly. */
                            var_kgpc_type->info.record_info = record_type;
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                        var_kgpc_type != NULL)
                    {
                        kgpc_type_set_type_alias(var_kgpc_type,
                            tree->tree_data.var_decl_data.inline_type_alias);
                    }
                }
                
                if (var_kgpc_type != NULL && kgpc_type_is_record(var_kgpc_type))
                {
                    struct RecordType *var_record = kgpc_type_get_record(var_kgpc_type);
                    if (var_record != NULL)
                    {
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (var_record->generic_decl != NULL && 
                                                         var_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, var_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                return_val += 1;
                            }
                        }
                    }
                }

                /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                /* Use PushArrayOntoScope_Typed for ShortString (which is array[0..255] of Char) */
                int is_shortstring = (var_type == HASHVAR_ARRAY && 
                                     tree->tree_data.var_decl_data.type_id != NULL &&
                                     pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"));
                
                if (is_shortstring)
                {
                    if (var_kgpc_borrowed && var_kgpc_type != NULL)
                        kgpc_type_retain(var_kgpc_type);
                    func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                }
                else
                {
                    if (var_kgpc_borrowed && var_kgpc_type != NULL)
                        kgpc_type_retain(var_kgpc_type);
                    func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                }
                /* Release caller's ref (either created or retained above) */
                destroy_kgpc_type(var_kgpc_type);

                if (func_return == 0)
                {
                    HashNode_t *var_node = FindIdentInCurrentScope(symtab, ids->cur);
                    if (var_node != NULL)
                    {
                        var_node->is_var_parameter =
                            (tree->tree_data.var_decl_data.is_var_param ||
                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                        mark_hashnode_unit_info(symtab, var_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                        mark_hashnode_source_unit(var_node,
                            tree->tree_data.var_decl_data.source_unit_index);
                    }
                }
            }
            /* Array declarations */
            else
            {
                assert(tree->type == TREE_ARR_DECL);
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Processing TREE_ARR_DECL: %s is_typed_const=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        tree->tree_data.arr_decl_data.is_typed_const);

                KgpcType *element_type = NULL;
                int element_type_borrowed = 0;  /* Track if borrowed from symbol table */
                int is_array_of_const = (tree->tree_data.arr_decl_data.type == ARRAY_OF_CONST_TYPE);

                if (!is_array_of_const &&
                    tree->tree_data.arr_decl_data.element_kgpc_type != NULL)
                {
                    element_type = tree->tree_data.arr_decl_data.element_kgpc_type;
                    element_type_borrowed = 1;
                }

                /* If type_id is specified, resolve it to get the element type */
                const TypeRef *element_type_ref = tree->tree_data.arr_decl_data.type_ref;
                if (kgpc_getenv("KGPC_DEBUG_TZINFO") != NULL &&
                    ids != NULL && ids->cur != NULL &&
                    (pascal_identifier_equals((const char *)ids->cur, "CurrentTZinfo") ||
                     pascal_identifier_equals((const char *)ids->cur, "CurrentTzinfoEx")))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_TZINFO] arr=%s type_id=%s type_ref_base=%s\n",
                        (const char *)ids->cur,
                        tree->tree_data.arr_decl_data.type_id ? tree->tree_data.arr_decl_data.type_id : "(null)",
                        element_type_ref ? type_ref_base_name(element_type_ref) : "(null)");
                }
                if (element_type == NULL &&
                    !is_array_of_const &&
                    (tree->tree_data.arr_decl_data.type_id != NULL || element_type_ref != NULL))
                {
                    if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL)
                    {
                        const char *arr_type_id = tree->tree_data.arr_decl_data.type_id;
                        if (arr_type_id == NULL && element_type_ref != NULL)
                            arr_type_id = type_ref_base_name(element_type_ref);
                        if (arr_type_id != NULL &&
                            pascal_identifier_equals(arr_type_id, "TFloatFormatProfile"))
                        {
                            int count = 0;
                            ListNode_t *matches = FindAllIdents(symtab, arr_type_id);
                            for (ListNode_t *m = matches; m != NULL; m = m->next)
                            {
                                HashNode_t *n = (HashNode_t *)m->cur;
                                if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                                {
                                    fprintf(stderr,
                                        "[TFLOAT] array elem match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                                        n->id ? n->id : "<null>", (void *)n->type,
                                        n->defined_in_unit, n->source_unit_index);
                                    count++;
                                }
                            }
                            if (matches != NULL)
                                DestroyList(matches);
                            fprintf(stderr, "[TFLOAT] array elem total matches=%d\n", count);
                        }
                    }
                    HashNode_t *element_type_node = NULL;
                    element_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        element_type_ref, tree->tree_data.arr_decl_data.type_id);
                    if (element_type_node == NULL &&
                        !tree->tree_data.arr_decl_data.defined_in_unit)
                    {
                        const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                        const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                        if (owner_full == NULL)
                            owner_full = semcheck_get_current_method_owner();
                        if (owner_full != NULL)
                        {
                            semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                &tree->tree_data.arr_decl_data.type_id,
                                &tree->tree_data.arr_decl_data.type_ref);
                            element_type_ref = tree->tree_data.arr_decl_data.type_ref;
                            element_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                                element_type_ref, tree->tree_data.arr_decl_data.type_id);
                        }
                    }
                    if (element_type_node == NULL)
                    {
                        const char *fallback_id = tree->tree_data.arr_decl_data.type_id;
                        if (fallback_id == NULL && element_type_ref != NULL)
                            fallback_id = type_ref_base_name(element_type_ref);
                        if (fallback_id != NULL)
                        {
                            ListNode_t *matches = FindAllIdents(symtab, fallback_id);
                            for (ListNode_t *m = matches; m != NULL; m = m->next)
                            {
                                HashNode_t *n = (HashNode_t *)m->cur;
                                if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                                {
                                    element_type_node = n;
                                    break;
                                }
                            }
                            if (matches != NULL)
                                DestroyList(matches);
                        }
                    }
                    if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL)
                    {
                        const char *check_id = tree->tree_data.arr_decl_data.type_id;
                        if (check_id == NULL && element_type_ref != NULL)
                            check_id = type_ref_base_name(element_type_ref);
                        if (check_id != NULL &&
                            (pascal_identifier_equals(check_id, "TFloatFormatProfile") ||
                             pascal_identifier_equals(check_id, "TFloatSpecial") ||
                             pascal_identifier_equals(check_id, "TDIY_FP_Power_of_10") ||
                             pascal_identifier_equals(check_id, "pshortstring") ||
                             pascal_identifier_equals(check_id, "T")))
                        {
                            fprintf(stderr, "[MISSING_TYPE] element_type_node=%p for %s line=%d\n",
                                (void *)element_type_node, check_id, tree->line_num);
                        }
                    }
                    if (element_type_node != NULL)
                    {
                        /* Use the KgpcType from the resolved type node */
                        element_type = element_type_node->type;
                        element_type_borrowed = 1;  /* Mark as borrowed */
                        if (element_type == NULL)
                        {
                            /* Fallback for migration: some nodes may not have KgpcType populated yet.
                             * Try to construct KgpcType from legacy record type information. */
                            struct RecordType *record_type = get_record_type_from_node(element_type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                element_type = create_record_type(record_type);
                                element_type_borrowed = 0;  /* New type, not borrowed */
                            }
                        }
                    }
                    if (kgpc_getenv("KGPC_DEBUG_TZINFO") != NULL &&
                        ids != NULL && ids->cur != NULL &&
                        (pascal_identifier_equals((const char *)ids->cur, "CurrentTZinfo") ||
                         pascal_identifier_equals((const char *)ids->cur, "CurrentTzinfoEx")))
                    {
                        fprintf(stderr,
                            "[KGPC_DEBUG_TZINFO] element_type_node=%p type=%p type_id=%s\n",
                            (void *)element_type_node,
                            element_type_node ? (void *)element_type_node->type : NULL,
                            tree->tree_data.arr_decl_data.type_id ? tree->tree_data.arr_decl_data.type_id : "(null)");
                    }
                    else
                    {
                        /* Fallback: check for builtin types not in symbol table */
                        const char *type_id = tree->tree_data.arr_decl_data.type_id;
                        if (type_id == NULL && element_type_ref != NULL)
                            type_id = type_ref_base_name(element_type_ref);
                        int builtin_type = semcheck_map_builtin_type_name_local(type_id);
                        if (builtin_type != UNKNOWN_TYPE)
                        {
                            if (builtin_type == CHAR_TYPE && type_id != NULL &&
                                (pascal_identifier_equals(type_id, "WideChar") ||
                                 pascal_identifier_equals(type_id, "UnicodeChar")))
                            {
                                element_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                            }
                            else
                            {
                                element_type = create_primitive_type(builtin_type);
                            }
                        }
                        else
                        {
                            const char *missing_id = tree->tree_data.arr_decl_data.type_id;
                            if (missing_id == NULL && element_type_ref != NULL)
                                missing_id = type_ref_base_name(element_type_ref);
                            if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL)
                            {
                                fprintf(stderr, "[MISSING_TYPE] array elem unresolved id=%s line=%d\n",
                                    missing_id ? missing_id : "<null>", tree->line_num);
                            }
                            if (missing_id != NULL)
                            {
                                ListNode_t *matches = FindAllIdents(symtab, missing_id);
                                for (ListNode_t *m = matches; m != NULL; m = m->next)
                                {
                                    HashNode_t *n = (HashNode_t *)m->cur;
                                    if (n != NULL && n->hash_type == HASHTYPE_TYPE)
                                    {
                                        if (kgpc_getenv("KGPC_DEBUG_MISSING_TYPE") != NULL)
                                        {
                                            fprintf(stderr,
                                                "[MISSING_TYPE]   match id=%s type=%p defined_in_unit=%d source_unit_index=%d\n",
                                                n->id ? n->id : "<null>", (void *)n->type,
                                                n->defined_in_unit, n->source_unit_index);
                                        }
                                        if (n->type != NULL)
                                        {
                                            element_type = n->type;
                                            element_type_borrowed = 1;
                                            break;
                                        }
                                    }
                                }
                                if (matches != NULL)
                                    DestroyList(matches);
                            }
                            if (element_type == NULL)
                            {
                                semcheck_error_with_context("Error on line %d: undefined type %s\n",
                                    tree->line_num, tree->tree_data.arr_decl_data.type_id);
                                return_val++;
                            }
                        }
                    }
                }
                
                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const &&
                    tree->tree_data.arr_decl_data.inline_record_type != NULL)
                {
                    element_type = create_record_type(tree->tree_data.arr_decl_data.inline_record_type);
                    element_type_borrowed = 0;
                }

                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const)
                {
                    if(tree->tree_data.arr_decl_data.type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if(tree->tree_data.arr_decl_data.type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if(tree->tree_data.arr_decl_data.type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if(tree->tree_data.arr_decl_data.type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if(tree->tree_data.arr_decl_data.type == SHORTSTRING_TYPE)
                        var_type = HASHVAR_PCHAR;  /* ShortString is array of char */
                    else if(tree->tree_data.arr_decl_data.type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if(tree->tree_data.arr_decl_data.type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if(is_real_family_type(tree->tree_data.arr_decl_data.type))
                        var_type = HASHVAR_REAL;
                    else {
                        semcheck_error_with_context(
                            "Error on line %d, unknown array element type %d for %s.\n\n",
                            tree->line_num,
                            tree->tree_data.arr_decl_data.type,
                            ids && ids->cur ? (char*)ids->cur : "<unknown>");
                        return_val++;
                        var_type = HASHVAR_REAL;
                    }
                    
                    element_type = kgpc_type_from_var_type(var_type);
                    assert(element_type != NULL && "Array element type must be createable from VarType");
                }

                if (element_type != NULL && kgpc_type_is_record(element_type))
                {
                    struct RecordType *element_record = kgpc_type_get_record(element_type);
                    if (element_record != NULL && !element_record->has_cached_size)
                    {
                        long long record_size = 0;
                        if (semcheck_compute_record_size(symtab, element_record, &record_size,
                                tree->line_num) != 0)
                        {
                            return_val += 1;
                        }
                    }
                }
                
                /* Resolve array bounds from constant identifiers if necessary.
                 * This handles inline array declarations like: var arr: array[1..N] of integer
                 * where N is a const. The parser stores the original range string (e.g., "1..N")
                 * in range_str, which we parse and resolve here. */
                int start_bound = tree->tree_data.arr_decl_data.s_range;
                int end_bound = tree->tree_data.arr_decl_data.e_range;
                
                /* Use pre-split bounds from parser (avoids strstr on ".." at semcheck time) */
                {
                    const char *s = tree->tree_data.arr_decl_data.range_start_str;
                    const char *e = tree->tree_data.arr_decl_data.range_end_str;
                    if (s != NULL && e != NULL)
                    {
                        while (*s == ' ' || *s == '\t') s++;
                        while (*e == ' ' || *e == '\t') e++;

                        long long start_val = 0;
                        if (resolve_const_identifier(symtab, s, &start_val) == 0)
                            start_bound = (int)start_val;
                        else {
                            char *endptr;
                            long num = strtol(s, &endptr, 10);
                            if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                start_bound = (int)num;
                        }

                        long long end_val = 0;
                        if (resolve_const_identifier(symtab, e, &end_val) == 0)
                            end_bound = (int)end_val;
                        else {
                            char *endptr;
                            long num = strtol(e, &endptr, 10);
                            if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                end_bound = (int)num;
                        }
                    }
                }
                
                /* CRITICAL FIX: Update the tree's s_range and e_range fields with the resolved bounds.
                 * Downstream code (especially the x86-64 code generator) checks arr->e_range < arr->s_range
                 * to determine if an array is dynamic. Without updating these fields, arrays declared with
                 * constant expressions (e.g., array[1..N] where N is a const) would be incorrectly treated
                 * as dynamic arrays, leading to segmentation faults. */
                tree->tree_data.arr_decl_data.s_range = start_bound;
                tree->tree_data.arr_decl_data.e_range = end_bound;
                
                KgpcType *array_type = NULL;
                if (is_array_of_const)
                {
                    array_type = create_array_of_const_type();
                }
                else
                {
                    /* CRITICAL: Retain element_type if borrowed from symbol table
                     * since create_array_type takes ownership. */
                    if (element_type_borrowed && element_type != NULL)
                        kgpc_type_retain(element_type);
                    array_type = create_array_type(
                        element_type,
                        start_bound,
                        end_bound
                    );
                }
                assert(array_type != NULL && "Failed to create array type");

                /* If the element type was specified by a type_id (like TAlfa), preserve that information
                 * by creating a minimal TypeAlias and attaching it to the array_type. This allows
                 * nested array indexing to work correctly (e.g., Keywords[1][1] where Keywords is
                 * array[1..5] of TAlfa and TAlfa is array[1..10] of char). */
                if (!is_array_of_const)
                {
                    struct TypeAlias temp_alias = {0};
                    int has_alias = 0;
                    
                    if (tree->tree_data.arr_decl_data.type_id != NULL)
                    {
                        temp_alias.is_array = 1;
                        temp_alias.array_start = start_bound;
                        temp_alias.array_end = end_bound;
                        temp_alias.array_element_type_id = tree->tree_data.arr_decl_data.type_id;  /* Will be duplicated by copy_type_alias */
                        temp_alias.array_element_type = tree->tree_data.arr_decl_data.type;
                        has_alias = 1;
                    }

                    if (tree->tree_data.arr_decl_data.is_shortstring)
                    {
                        if (!has_alias)
                        {
                            temp_alias.is_array = 1;
                            temp_alias.array_start = start_bound;
                            temp_alias.array_end = end_bound;
                            temp_alias.array_element_type = CHAR_TYPE;
                            temp_alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            has_alias = 1;
                        }
                        temp_alias.is_shortstring = 1;
                    }

                    /* Attach multi-dim array_dimensions from the AST declaration.
                     * Only when there are 2+ dimensions — single-dim arrays
                     * don't need linearization metadata. */
                    if (tree->tree_data.arr_decl_data.array_dimensions != NULL &&
                        tree->tree_data.arr_decl_data.array_dimensions->next != NULL)
                    {
                        temp_alias.is_array = 1;
                        if (!has_alias)
                        {
                            temp_alias.array_start = start_bound;
                            temp_alias.array_end = end_bound;
                            temp_alias.array_element_type = tree->tree_data.arr_decl_data.type;
                            if (tree->tree_data.arr_decl_data.type_id != NULL)
                                temp_alias.array_element_type_id = tree->tree_data.arr_decl_data.type_id;
                        }
                        temp_alias.array_dimensions = tree->tree_data.arr_decl_data.array_dimensions;
                        has_alias = 1;
                    }

                    if (has_alias)
                        kgpc_type_set_type_alias(array_type, &temp_alias);

                    /* Clear temp_alias.array_dimensions so it isn't freed (owned by AST) */
                    temp_alias.array_dimensions = NULL;
                }
                
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Pushing array: %s, array_type=%p kind=%d elem_kind=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        (void*)array_type, array_type ? array_type->kind : -1,
                        (array_type && array_type->kind == TYPE_KIND_ARRAY && array_type->info.array_info.element_type) ?
                            array_type->info.array_info.element_type->kind : -1);
                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                destroy_kgpc_type(array_type);  /* Release creator's ref */
                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] PushArrayOntoScope_Typed returned: %d\n", func_return);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                semantic_error(tree->line_num, 0, "redeclaration of name %s",
                    (char *)ids->cur);
                return_val += func_return;
            }
            else
            {
                HashNode_t *decl_node = NULL;
                if (FindIdent(&decl_node, symtab, ids->cur) != -1 && decl_node != NULL)
                {
                    if (tree->type == TREE_VAR_DECL)
                    {
                        decl_node->is_var_parameter =
                            (tree->tree_data.var_decl_data.is_var_param ||
                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                        mark_hashnode_unit_info(symtab, decl_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                    else
                    {
                        mark_hashnode_unit_info(symtab, decl_node,
                            tree->tree_data.arr_decl_data.defined_in_unit,
                            tree->tree_data.arr_decl_data.unit_is_public);
                        if (tree->tree_data.arr_decl_data.is_typed_const)
                            decl_node->is_typed_const = 1;
                    }
                }
            }

next_identifier:
            /* Propagate source_unit_index from AST to HashNode for unit-aware resolution */
            if (func_return == 0 && ids->cur != NULL)
            {
                int ast_source_unit = 0;
                if (tree->type == TREE_VAR_DECL)
                    ast_source_unit = tree->tree_data.var_decl_data.source_unit_index;
                else if (tree->type == TREE_ARR_DECL)
                    ast_source_unit = tree->tree_data.arr_decl_data.source_unit_index;
                if (ast_source_unit > 0)
                {
                    HashNode_t *pushed_node = NULL;
                    if (FindIdent(&pushed_node, symtab, ids->cur) != -1 && pushed_node != NULL)
                        mark_hashnode_source_unit(pushed_node, ast_source_unit);
                }
            }
            ids = ids->next;
        }

        cur = cur->next;
        if (skip_initializer)
            continue;

        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.initializer != NULL)
        {
            struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
            
            /* Handle COMPOUND_STATEMENT initializers (from record const lowering) separately */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT)
            {
                /* This is a lowered record const - just semantic check the compound statement */
                return_val += semcheck_stmt(symtab, init_stmt, INT_MAX);
            }
            else if (ids_head == NULL || ids_head->next != NULL)
            {
                semcheck_error_with_context("Error on line %d, type inference initializers must declare a single identifier.\n",
                    tree->line_num);
                ++return_val;
            }
            else
            {
                char *var_name = (char *)ids_head->cur;
                HashNode_t *var_node = NULL;
                if (FindIdent(&var_node, symtab, var_name) == -1 || var_node == NULL)
                {
                    semcheck_error_with_context("Error on line %d, failed to resolve variable %s for initializer.\n",
                        tree->line_num, var_name);
                    ++return_val;
                }
                else
                {
                    struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
                    struct Expression *init_expr = init_stmt->stmt_data.var_assign_data.expr;
                    int is_default_param = (init_stmt->type == STMT_VAR_ASSIGN &&
                        init_stmt->stmt_data.var_assign_data.var == NULL);
                    if (init_expr == NULL)
                    {
                        semcheck_error_with_context("Error on line %d, initializer expression is NULL for %s.\n",
                            tree->line_num, var_name);
                        ++return_val;
                        /* Skip remaining processing for this variable but continue with the loop */
                    }
                    else
                    {
                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL)
                            var_node->is_typed_const = 1;

                        if (tree->tree_data.var_decl_data.is_typed_const &&
                            !tree->tree_data.var_decl_data.currency_scaled &&
                            semcheck_is_currency_type_id(tree->tree_data.var_decl_data.type_id))
                        {
                            double real_value = 0.0;
                            if (const_fold_real_expr(symtab, init_expr, &real_value) == 0)
                            {
                                long long scaled = llround(real_value * 10000.0);
                                destroy_expr(init_expr);
                                init_expr = mk_inum(tree->line_num, scaled);
                                init_stmt->stmt_data.var_assign_data.expr = init_expr;
                                tree->tree_data.var_decl_data.currency_scaled = 1;
                            }
                        }

                        KgpcType *expr_type = NULL;
                        if (init_expr->type == EXPR_RECORD_CONSTRUCTOR &&
                            (init_expr->resolved_kgpc_type == NULL ||
                             !kgpc_type_is_record(init_expr->resolved_kgpc_type)))
                        {
                            struct RecordType *record_type = NULL;
                            if (var_node->type != NULL && kgpc_type_is_record(var_node->type))
                                record_type = kgpc_type_get_record(var_node->type);
                            else if (var_node->type != NULL && kgpc_type_is_pointer(var_node->type) &&
                                var_node->type->info.points_to != NULL &&
                                kgpc_type_is_record(var_node->type->info.points_to))
                                record_type = kgpc_type_get_record(var_node->type->info.points_to);
                            else if (tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = NULL;
                                if (FindIdent(&type_node, symtab,
                                        tree->tree_data.var_decl_data.type_id) >= 0 &&
                                    type_node != NULL)
                                {
                                    record_type = hashnode_get_record_type(type_node);
                                    if (record_type == NULL && type_node->type != NULL)
                                    {
                                        if (kgpc_type_is_record(type_node->type))
                                            record_type = kgpc_type_get_record(type_node->type);
                                        else if (kgpc_type_is_pointer(type_node->type) &&
                                            type_node->type->info.points_to != NULL &&
                                            kgpc_type_is_record(type_node->type->info.points_to))
                                            record_type = kgpc_type_get_record(type_node->type->info.points_to);
                                        else if (type_node->type->type_alias != NULL &&
                                            type_node->type->type_alias->target_type_id != NULL)
                                        {
                                            HashNode_t *target_node = NULL;
                                            if (FindIdent(&target_node, symtab,
                                                    type_node->type->type_alias->target_type_id) >= 0 &&
                                                target_node != NULL)
                                            {
                                                record_type = hashnode_get_record_type(target_node);
                                                if (record_type == NULL && target_node->type != NULL &&
                                                    kgpc_type_is_record(target_node->type))
                                                    record_type = kgpc_type_get_record(target_node->type);
                                            }
                                        }
                                    }
                                }
                                if (record_type == NULL)
                                {
                                    HashNode_t *decl_type_node = NULL;
                                    if (FindIdent(&decl_type_node, symtab,
                                            tree->tree_data.var_decl_data.type_id) >= 0 &&
                                        decl_type_node != NULL)
                                    {
                                        record_type = hashnode_get_record_type(decl_type_node);
                                        if (record_type == NULL && decl_type_node->type != NULL &&
                                            kgpc_type_is_record(decl_type_node->type))
                                            record_type = kgpc_type_get_record(decl_type_node->type);
                                    }
                                }
                            }
                            if (record_type != NULL)
                            {
                                init_expr->record_type = record_type;
                                KgpcType *record_kgpc = create_record_type(record_type);
                                if (record_kgpc != NULL)
                                {
                                    if (init_expr->resolved_kgpc_type != NULL)
                                        destroy_kgpc_type(init_expr->resolved_kgpc_type);
                                    init_expr->resolved_kgpc_type = record_kgpc;
                                    kgpc_type_retain(record_kgpc);
                                    destroy_kgpc_type(record_kgpc);
                                }
                            }
                        }
                        if (init_expr->type == EXPR_ARRAY_LITERAL &&
                            init_expr->array_element_type == UNKNOWN_TYPE &&
                            init_expr->array_element_type_id == NULL)
                        {
                            KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                            if ((var_type == NULL || !kgpc_type_is_array(var_type)) &&
                                tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                                    tree->tree_data.var_decl_data.type_id);
                                if (type_node != NULL)
                                    var_type = type_node->type;
                            }
                            if (var_type != NULL && kgpc_type_is_array(var_type))
                            {
                                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                if (elem_type != NULL)
                                {
                                    int elem_tag = semcheck_tag_from_kgpc(elem_type);
                                    if (init_expr->array_element_type == UNKNOWN_TYPE)
                                        init_expr->array_element_type = elem_tag;
                                    if (elem_type->kind == TYPE_KIND_RECORD)
                                    {
                                        struct RecordType *rec = kgpc_type_get_record(elem_type);
                                        init_expr->array_element_record_type = rec;
                                        if (init_expr->array_element_type_id == NULL &&
                                            rec != NULL && rec->type_id != NULL)
                                        {
                                            init_expr->array_element_type_id = strdup(rec->type_id);
                                        }
                                    }
                                    if (init_expr->array_element_type_id == NULL &&
                                        elem_type->type_alias != NULL &&
                                        elem_type->type_alias->target_type_id != NULL)
                                    {
                                        init_expr->array_element_type_id =
                                            strdup(elem_type->type_alias->target_type_id);
                                    }
                                }
                                if (init_expr->array_element_type_id == NULL &&
                                    var_type->info.array_info.element_type_id != NULL)
                                {
                                    init_expr->array_element_type_id =
                                        strdup(var_type->info.array_info.element_type_id);
                                }
                            }
                        }
                        return_val += semcheck_expr_main(symtab, init_expr, INT_MAX, NO_MUTATE, &expr_type);

                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL &&
                            init_expr->type != EXPR_ADDR && init_expr->type != EXPR_ADDR_OF_PROC &&
                            init_expr->type != EXPR_RECORD_CONSTRUCTOR &&
                            init_expr->type != EXPR_ARRAY_LITERAL)
                        {
                        if (expression_is_string(symtab, init_expr))
                            {
                                char *string_value = NULL;
                                if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                {
                                    if (var_node->const_string_value != NULL)
                                        free(var_node->const_string_value);
                                    var_node->const_string_value = string_value;
                                    if (string_value != NULL &&
                                        string_value[0] != '\0' && string_value[1] == '\0')
                                    {
                                        var_node->const_int_value = (unsigned char)string_value[0];
                                    }
                                }
                            }
                            else if (expression_contains_real_literal_impl(symtab, init_expr))
                            {
                                double real_value = 0.0;
                                if (const_fold_real_expr(symtab, init_expr, &real_value) == 0)
                                    var_node->const_real_value = real_value;
                            }
                            else if (expression_is_set_const_expr(symtab, init_expr))
                            {
                                unsigned char set_bytes[32];
                                size_t set_size = 0;
                                long long mask = 0;
                                int is_char_set = 0;
                                if (evaluate_set_const_bytes(symtab, init_expr, set_bytes, sizeof(set_bytes),
                                        &set_size, &mask, &is_char_set) == 0)
                                {
                                    unsigned char *copy = (unsigned char *)malloc(set_size);
                                    if (copy != NULL)
                                    {
                                        memcpy(copy, set_bytes, set_size);
                                        if (var_node->const_set_value != NULL)
                                            free(var_node->const_set_value);
                                        var_node->const_set_value = copy;
                                        var_node->const_set_size = (int)set_size;
                                    }
                                }
                            }
                            else
                            {
                                long long value = 0;
                                if (const_fold_int_expr(symtab, init_expr, &value) == 0)
                                    var_node->const_int_value = value;
                            }
                        }

                    int expr_tag = expr_type != NULL ? semcheck_tag_from_kgpc(expr_type) : UNKNOWN_TYPE;
                    if (expr_tag == UNKNOWN_TYPE &&
                        tree->tree_data.var_decl_data.is_typed_const &&
                        init_expr != NULL &&
                        init_expr->type == EXPR_ADDR &&
                        var_node != NULL &&
                        var_node->type != NULL)
                    {
                        int declared_tag = semcheck_tag_from_kgpc(var_node->type);
                        if (declared_tag == POINTER_TYPE || declared_tag == PROCEDURE)
                        {
                            expr_type = var_node->type;
                            kgpc_type_retain(expr_type);
                            expr_tag = declared_tag;
                            if (init_expr->resolved_kgpc_type != NULL)
                                destroy_kgpc_type(init_expr->resolved_kgpc_type);
                            init_expr->resolved_kgpc_type = var_node->type;
                            kgpc_type_retain(init_expr->resolved_kgpc_type);
                        }
                    }
                    if (expr_tag == UNKNOWN_TYPE)
                    {
                        if (!is_default_param)
                        {
                            semcheck_error_with_context("Error on line %d, unable to infer type for %s.\n", tree->line_num, var_name);
                            ++return_val;
                        }
                    }
                    else
                    {
                        enum VarType inferred_var_type = HASHVAR_UNTYPED;
                        int normalized_type = expr_tag;

                        switch(expr_tag)
                        {
                            case INT_TYPE:
                            case LONGINT_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = (expr_tag == LONGINT_TYPE) ? LONGINT_TYPE : INT_TYPE;
                                break;
                            case BYTE_TYPE:
                            case WORD_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = expr_tag;
                                break;
                            case LONGWORD_TYPE:
                                inferred_var_type = HASHVAR_LONGINT;
                                normalized_type = LONGWORD_TYPE;
                                break;
                            case QWORD_TYPE:
                                inferred_var_type = HASHVAR_QWORD;
                                normalized_type = QWORD_TYPE;
                                break;
                            case INT64_TYPE:
                                inferred_var_type = HASHVAR_INT64;
                                normalized_type = INT64_TYPE;
                                break;
                            case BOOL:
                                inferred_var_type = HASHVAR_BOOLEAN;
                                normalized_type = BOOL;
                                break;
                            case REAL_TYPE:
                            case EXTENDED_TYPE:
                                inferred_var_type = HASHVAR_REAL;
                                normalized_type = expr_tag;
                                break;
                            case CHAR_TYPE:
                                inferred_var_type = HASHVAR_CHAR;
                                normalized_type = CHAR_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("char");
                                break;
                            case STRING_TYPE:
                                inferred_var_type = HASHVAR_PCHAR;
                                normalized_type = STRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("string");
                                break;
                            case SHORTSTRING_TYPE:
                                inferred_var_type = HASHVAR_SHORTSTRING;
                                normalized_type = SHORTSTRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("shortstring");
                                break;
                            case SET_TYPE:
                                inferred_var_type = HASHVAR_SET;
                                normalized_type = SET_TYPE;
                                break;
                            case ENUM_TYPE:
                                inferred_var_type = HASHVAR_ENUM;
                                normalized_type = ENUM_TYPE;
                                break;
                            case POINTER_TYPE:
                                inferred_var_type = HASHVAR_POINTER;
                                normalized_type = POINTER_TYPE;
                                break;
                            case RECORD_TYPE:
                                inferred_var_type = HASHVAR_RECORD;
                                normalized_type = RECORD_TYPE;
                                break;
                            default:
                                semcheck_error_with_context("Error on line %d, unsupported inferred type for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                                inferred_var_type = HASHVAR_UNTYPED;
                                break;
                        }

                        if (tree->tree_data.var_decl_data.inferred_type)
                        {
                            if (inferred_var_type != HASHVAR_UNTYPED)
                            {
                                tree->tree_data.var_decl_data.type = normalized_type;
                                /* Replace or create KgpcType with inferred type */
                                KgpcType *inferred_kgpc_type = create_primitive_type(normalized_type);
                                if (inferred_kgpc_type != NULL)
                                {
                                    if (var_node->type != NULL)
                                    {
                                        /* Free old type and replace */
                                        destroy_kgpc_type(var_node->type);
                                    }
                                    var_node->type = inferred_kgpc_type;
                                    /* Legacy field will be populated by helper if needed */
                                }
                            }
                        }
                        else if (!is_default_param)
                        {
                            enum VarType current_var_type = get_var_type_from_node(var_node);
                            int compatible = (inferred_var_type == current_var_type);
                            
                            /* Allow mixing Integer and LongInt for initializers */
                            if (!compatible)
                            {
                                if ((inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT) &&
                                    (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if (current_var_type == HASHVAR_REAL &&
                                    (inferred_var_type == HASHVAR_INTEGER ||
                                     inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64 ||
                                     inferred_var_type == HASHVAR_QWORD))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if ((current_var_type == HASHVAR_INT64 || current_var_type == HASHVAR_QWORD) &&
                                    (inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64 || inferred_var_type == HASHVAR_QWORD))
                                {
                                    compatible = 1;
                                }
                                else if ((inferred_var_type == HASHVAR_INT64 || inferred_var_type == HASHVAR_QWORD) &&
                                         (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }

                            /* In {$H-} mode string literals resolve as ShortString.
                             * Allow typed initializers between String and ShortString. */
                            if (!compatible)
                            {
                                if ((current_var_type == HASHVAR_PCHAR && inferred_var_type == HASHVAR_SHORTSTRING) ||
                                    (current_var_type == HASHVAR_SHORTSTRING && inferred_var_type == HASHVAR_PCHAR))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible && current_var_type == HASHVAR_PCHAR && expr_tag == CHAR_TYPE)
                            {
                                compatible = 1;
                            }
                            if (!compatible &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE) &&
                                init_expr != NULL && init_expr->type == EXPR_STRING &&
                                init_expr->expr_data.string != NULL &&
                                strlen(init_expr->expr_data.string) == 1 &&
                                var_node != NULL && semcheck_kgpc_type_is_char_like(var_node->type))
                            {
                                compatible = 1;
                                semcheck_expr_set_resolved_type(init_expr, CHAR_TYPE);
                            }
                            /* Allow string literal initializer for array of char or shortstring-like arrays */
                            if (!compatible && current_var_type == HASHVAR_ARRAY &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE))
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && var_type->kind == TYPE_KIND_ARRAY &&
                                    var_type->info.array_info.element_type != NULL &&
                                    var_type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
                                    var_type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
                                {
                                    char *string_value = NULL;
                                    size_t string_len = 0;
                                    if (init_expr != NULL && init_expr->type == EXPR_STRING &&
                                        init_expr->expr_data.string != NULL)
                                    {
                                        string_len = strlen(init_expr->expr_data.string);
                                    }
                                    else if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                    {
                                        if (string_value != NULL)
                                            string_len = strlen(string_value);
                                    }
                                    if (string_value != NULL)
                                        free(string_value);

                                    int array_size = var_type->info.array_info.end_index -
                                                     var_type->info.array_info.start_index + 1;
                                    if (array_size < 0)
                                        array_size = 0;
                                    if (string_len <= (size_t)array_size)
                                    {
                                        compatible = 1;
                                    }
                                    else
                                    {
                                        semcheck_error_with_context(
                                            "Error on line %d, initializer string literal too long for %s (string length: %zu, array size: %d).\n",
                                            tree->line_num, var_name, string_len, array_size);
                                        ++return_val;
                                    }
                                }
                            }

                            /* Allow array literal initializer for array variables when element types match */
                            if (!compatible && current_var_type == HASHVAR_ARRAY &&
                                init_expr != NULL && init_expr->type == EXPR_ARRAY_LITERAL)
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && kgpc_type_is_array(var_type))
                                {
                                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                    int elem_tag = semcheck_tag_from_kgpc(elem_type);
                                    int literal_elem_tag = init_expr->array_element_type;
                                    if (elem_tag != UNKNOWN_TYPE && literal_elem_tag != UNKNOWN_TYPE &&
                                        elem_tag == literal_elem_tag)
                                    {
                                        compatible = 1;
                                    }
                                }
                            }
                            
                            /* Allow pointer type initializers (including nil) for pointer variables.
                             * Both the initializer expression type and the declared variable type must
                             * be pointer types. This enables patterns like: var my_ptr: PChar = nil; */
                            if (!compatible)
                            {
                                int inferred_is_pointer = (inferred_var_type == HASHVAR_POINTER || expr_tag == POINTER_TYPE);
                                int current_is_pointer = (current_var_type == HASHVAR_POINTER);
                                int current_is_proc = (current_var_type == HASHVAR_PROCEDURE);
                                if (var_node->type != NULL)
                                {
                                    current_is_pointer |= kgpc_type_is_pointer(var_node->type);
                                    if (semcheck_tag_from_kgpc(var_node->type) == POINTER_TYPE)
                                        current_is_pointer = 1;
                                    if (var_node->type->kind == TYPE_KIND_PROCEDURE)
                                        current_is_proc = 1;
                                }
                                if (tree->tree_data.var_decl_data.type == POINTER_TYPE)
                                    current_is_pointer = 1;
                                if (tree->tree_data.var_decl_data.type == PROCEDURE)
                                    current_is_proc = 1;
                                if (inferred_is_pointer && current_is_pointer)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && current_is_proc)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && var_node != NULL)
                                {
                                    struct TypeAlias *alias = hashnode_get_type_alias(var_node);
                                    if (alias != NULL && alias->is_pointer)
                                        compatible = 1;
                                }
                            }

                            /* Allow string literal initializer for PAnsiChar / PChar
                             * pointer-to-char types.  The variable's KgpcType is
                             * TYPE_KIND_POINTER with a CHAR_TYPE subtarget, e.g.:
                             *   const signature: PAnsiChar = 'TPF0'; */
                            if (!compatible &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE))
                            {
                                int var_is_pchar = 0;
                                if (var_node != NULL && var_node->type != NULL &&
                                    kgpc_type_is_pointer(var_node->type))
                                {
                                    int sub_tag = kgpc_type_get_pointer_subtype_tag(var_node->type);
                                    if (sub_tag == CHAR_TYPE)
                                        var_is_pchar = 1;
                                }
                                /* Also check by type_id (PAnsiChar, PChar, PWideChar) */
                                if (!var_is_pchar)
                                {
                                    const char *tid = tree->tree_data.var_decl_data.type_id;
                                    if (tid != NULL &&
                                        (strcasecmp(tid, "PAnsiChar") == 0 ||
                                         strcasecmp(tid, "PChar") == 0 ||
                                         strcasecmp(tid, "PWideChar") == 0))
                                    {
                                        var_is_pchar = 1;
                                    }
                                }
                                if (var_is_pchar)
                                    compatible = 1;
                            }

                            if (!compatible && current_var_type == HASHVAR_RECORD &&
                                (expr_tag == STRING_TYPE || expr_tag == SHORTSTRING_TYPE))
                            {
                                const char *record_id = NULL;
                                if (var_node->type != NULL)
                                {
                                    struct RecordType *record = kgpc_type_get_record(var_node->type);
                                    if (record != NULL && record->type_id != NULL)
                                        record_id = record->type_id;
                                }
                                if (record_id == NULL)
                                    record_id = tree->tree_data.var_decl_data.type_id;
                                if (record_id != NULL &&
                                    (strcasecmp(record_id, "TGuid") == 0 || strcasecmp(record_id, "GUID") == 0))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible)
                            {
                                semcheck_error_with_context("Error on line %d, initializer type mismatch for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                            }
                        }
                    }
                    }  /* Close else at line 2054 */
                }  /* Close else at line 2043 */
            }  /* Close else at line 2033 */
        }  /* Close if at line 2017 */
        else if (tree->type == TREE_ARR_DECL && tree->tree_data.arr_decl_data.initializer != NULL)
        {
            return_val += semcheck_stmt(symtab, tree->tree_data.arr_decl_data.initializer, INT_MAX);
        }

    }

    return return_val;
}

/* Semantic check on an entire subprogram */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev)
{
    int return_val, func_return;
    int new_max_scope;
    enum TreeType sub_type;
    struct Statement *body;
    HashNode_t *hash_return;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    
#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprogram called for %s\n", subprogram->tree_data.subprogram_data.id);
#endif

    assert(subprogram->type == TREE_SUBPROGRAM);

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    const char *prev_owner = semcheck_get_current_method_owner();
    char *owner_copy = NULL;
    if (subprogram->tree_data.subprogram_data.owner_class != NULL)
    {
        /* Use full dotted path when available (e.g. "TOuter.TInner") so that
         * semcheck_get_current_method_owner can walk up to outer classes. */
        const char *owner_src = subprogram->tree_data.subprogram_data.owner_class_full;
        if (owner_src == NULL)
            owner_src = subprogram->tree_data.subprogram_data.owner_class;
        owner_copy = strdup(owner_src);
        if (owner_copy != NULL)
            semcheck_set_current_method_owner(owner_copy);
    }

    /* For class methods, copy default parameter values from the class declaration
     * to the implementation. This is needed because Pascal allows defaults only in
     * the declaration, not in the implementation. */
    copy_method_decl_defaults_to_impl(symtab, subprogram);

    /* Record lexical nesting depth so codegen can reason about static links accurately.
     * Store depth as parent depth + 1 so the top-level program has depth 1 and
     * nested subprograms continue to increase. */
    subprogram->tree_data.subprogram_data.nesting_level = max_scope_lev + 1;
    subprogram->tree_data.subprogram_data.is_nested =
        (subprogram->tree_data.subprogram_data.owner_class == NULL &&
         subprogram->tree_data.subprogram_data.nesting_level > 1);
    int default_requires = (subprogram->tree_data.subprogram_data.nesting_level > 1 &&
        !subprogram->tree_data.subprogram_data.defined_in_unit);
    subprogram->tree_data.subprogram_data.requires_static_link = default_requires ? 1 : 0;

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Imported subprogram parameter declarations must retain imported-origin
     * metadata; otherwise semcheck_decls may rebind type_id-based cached types
     * (e.g. UnixType.TSize) against local shadows (e.g. Types.TSize record). */
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
    {
        ListNode_t *arg_mark = subprogram->tree_data.subprogram_data.args_var;
        while (arg_mark != NULL)
        {
            if (arg_mark->type == LIST_TREE && arg_mark->cur != NULL)
            {
                Tree_t *arg_tree = (Tree_t *)arg_mark->cur;
                if (arg_tree->type == TREE_VAR_DECL)
                {
                    arg_tree->tree_data.var_decl_data.defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        arg_tree->tree_data.var_decl_data.unit_is_public = 1;
                }
                else if (arg_tree->type == TREE_ARR_DECL)
                {
                    arg_tree->tree_data.arr_decl_data.defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        arg_tree->tree_data.arr_decl_data.unit_is_public = 1;
                }
            }
            arg_mark = arg_mark->next;
        }
    }

    // --- Name Mangling Logic ---
    // Only set mangled_id if not already set by predeclare_subprogram (which handles
    // nested functions with unique parent$child naming)
    if (subprogram->tree_data.subprogram_data.mangled_id == NULL) {
        static int debug_external = -1;
        if (debug_external == -1)
            debug_external = (kgpc_getenv("KGPC_DEBUG_EXTERNAL") != NULL);
        const char *explicit_name = subprogram->tree_data.subprogram_data.cname_override;
        if (explicit_name != NULL) {
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else
                subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
        } else if (subprogram->tree_data.subprogram_data.cname_flag) {
            const char *export_name = subprogram->tree_data.subprogram_data.id;
            if (debug_external) {
                fprintf(stderr, "[SemCheck] cname_flag id=%s alias=%s\n",
                    subprogram->tree_data.subprogram_data.id,
                    export_name != NULL ? export_name : "(null)");
            }
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else if (export_name != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = strdup(export_name);
            else
                subprogram->tree_data.subprogram_data.mangled_id = NULL;
        } else {
            // Pass the symbol table to the mangler
            subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
        }
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if already declared (e.g., by predeclare_subprogram in two-pass approach) */
    HashNode_t *existing_decl = NULL;
    int already_declared = 0;
    
    if (subprogram->tree_data.subprogram_data.cached_predecl_node != NULL)
    {
        existing_decl = (HashNode_t *)subprogram->tree_data.subprogram_data.cached_predecl_node;
        already_declared = 1;
    }
    
    if (!already_declared)
    {
        /* For overloaded functions, find the correct overload by matching mangled name */
        if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
        {
            SubprogramPredeclLookup lookup = semcheck_lookup_subprogram_predecl(
                symtab,
                subprogram,
                id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);

            if (kgpc_getenv("KGPC_DEBUG_OVERLOAD_MATCH") != NULL)
            {
                ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
                fprintf(stderr, "[KGPC] Matching impl %s mangled=%s\n",
                    id_to_use_for_lookup ? id_to_use_for_lookup : "<null>",
                    subprogram->tree_data.subprogram_data.mangled_id);
                for (ListNode_t *debug_cur = all_matches; debug_cur != NULL; debug_cur = debug_cur->next)
                {
                    HashNode_t *debug_cand = (HashNode_t *)debug_cur->cur;
                    fprintf(stderr, "  candidate: id=%s mangled=%s\n",
                        debug_cand && debug_cand->id ? debug_cand->id : "<null>",
                        debug_cand && debug_cand->mangled_id ? debug_cand->mangled_id : "<null>");
                }
                if (all_matches != NULL)
                    DestroyList(all_matches);
            }

            existing_decl = lookup.exact_match != NULL ?
                lookup.exact_match : lookup.first_mangled_match;
            already_declared = (existing_decl != NULL);
        }
        
        /* Fallback to simple lookup if no mangled name or no match found */
        if (!already_declared)
            already_declared = (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0);
    }

    if (already_declared && existing_decl != NULL &&
        subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (existing_decl->mangled_id != NULL)
            free(existing_decl->mangled_id);
        existing_decl->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = NULL;
        int created_new_type = 0;

        if (already_declared && existing_decl != NULL && existing_decl->type != NULL)
        {
            proc_type = existing_decl->type;
            Tree_t *prev_def = proc_type->info.proc_info.definition;
            if (subprogram->tree_data.subprogram_data.statement_list != NULL &&
                (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL))
            {
                proc_type->info.proc_info.definition = subprogram;
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                proc_type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }
        else
        {
            proc_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                NULL  /* procedures have no return type */
            );
            if (proc_type != NULL)
            {
                proc_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    proc_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            created_new_type = 1;
        }
        
        /* ARCHITECTURAL FIX: Resolve array bounds in parameter types now that constants are in scope */
        if (proc_type != NULL && proc_type->info.proc_info.params != NULL)
        {
            ListNode_t *param = proc_type->info.proc_info.params;
            while (param != NULL)
            {
                if (param->type == LIST_TREE && param->cur != NULL)
                {
                    Tree_t *param_tree = (Tree_t *)param->cur;
                    /* For array parameters, resolve the bounds */
                    if (param_tree->type == TREE_ARR_DECL)
                    {
                        /* If element type is a named type, look it up to get proper KgpcType */
                        if (param_tree->tree_data.arr_decl_data.type_id != NULL)
                        {
                            HashNode_t *elem_type_node = NULL;
                            if (FindIdent(&elem_type_node, symtab, param_tree->tree_data.arr_decl_data.type_id) != -1 &&
                                elem_type_node != NULL && elem_type_node->type != NULL)
                            {
                                /* Element type resolved - bounds should be in the tree node */
                                /* Nothing to do here */
                            }
                        }
                    }
                }
                param = param->next;
            }
        }
        
        // Use the typed version to properly set the KgpcType
        // Skip if already declared
        if (!already_declared)
        {
            func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            proc_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            FindIdent(&existing_decl, symtab, id_to_use_for_lookup);
            if (existing_decl != NULL && subprogram->tree_data.subprogram_data.defined_in_unit)
                existing_decl->defined_in_unit = 1;
        }
        else
        {
            func_return = 0;  /* No error since it's expected to be already declared */

            /* If we created a new type but it was already declared (e.g. existing had no type), update it */
            if (created_new_type && existing_decl != NULL && existing_decl->type == NULL)
            {
                existing_decl->type = proc_type;
            }
            else if (created_new_type)
            {
                /* We created a type but didn't use it (shouldn't happen if logic is correct, but for safety) */
                destroy_kgpc_type(proc_type);
            }
        }

        copy_method_identity_to_node(existing_decl, subprogram);
        semcheck_propagate_method_identity(symtab, subprogram);

        /* Propagate varargs flag from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.is_varargs)
            existing_decl->is_varargs = 1;

        /* Propagate INTERNPROC identifier from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.internproc_id != NULL)
        {
            if (existing_decl->internproc_id != NULL)
                free(existing_decl->internproc_id);
            existing_decl->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
        }

        PushScope(symtab);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram);
        /* For nested types (e.g. TOuter.TInner), also add outer class
         * vars/consts to scope. owner_class_full contains the dotted path. */
        if (subprogram->tree_data.subprogram_data.owner_class_full != NULL)
            add_outer_class_vars_to_method_scope(symtab, subprogram);

        if (existing_decl != NULL && existing_decl->type != NULL)
        {
            PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id,
                existing_decl->type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
        }

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        KgpcType *return_kgpc_type = NULL;

        /* Reuse the type created during predeclaration when possible. */
        if (already_declared && existing_decl != NULL &&
            existing_decl->type != NULL &&
            existing_decl->type->kind == TYPE_KIND_PROCEDURE)
        {
            return_kgpc_type = kgpc_type_get_return_type(existing_decl->type);
            if (subprogram->tree_data.subprogram_data.statement_list != NULL)
            {
                Tree_t *prev_def = existing_decl->type->info.proc_info.definition;
                if (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL)
                {
                    existing_decl->type->info.proc_info.definition = subprogram;
                }
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                existing_decl->type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }

        /* If the predeclare step could not resolve the type (e.g., inline array),
         * build it now and update the existing declaration. */
        if (return_kgpc_type == NULL)
        {
            int before_ret = return_val;
            return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 0);
            semcheck_debug_error_step("build_return_type", subprogram, before_ret, return_val);
#ifdef DEBUG
            if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        }

        KgpcType *func_type = NULL;
        if (!already_declared)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_subprogram %s NOT already declared in Pass 2!\n", subprogram->tree_data.subprogram_data.id);
#endif
            func_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                return_kgpc_type
            );
            if (func_type != NULL)
            {
                func_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    func_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            func_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            if (subprogram->tree_data.subprogram_data.defined_in_unit)
            {
                HashNode_t *new_func_decl = NULL;
                if (FindIdent(&new_func_decl, symtab, id_to_use_for_lookup) == 0 && new_func_decl != NULL)
                    new_func_decl->defined_in_unit = 1;
            }
        }
        else
        {
            func_return = 0;
            if (existing_decl != NULL)
            {
                if (existing_decl->type == NULL)
                {
                    func_type = create_procedure_type(
                        subprogram->tree_data.subprogram_data.args_var,
                        return_kgpc_type
                    );
                    if (func_type != NULL)
                    {
                        func_type->info.proc_info.definition = subprogram;
                        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                            func_type->info.proc_info.return_type_id =
                                strdup(subprogram->tree_data.subprogram_data.return_type_id);
                    }
                    existing_decl->type = func_type;
                }
                else if (return_kgpc_type != NULL)
                {
                    existing_decl->type->info.proc_info.return_type = return_kgpc_type;
                }
            }
        }

        copy_method_identity_to_node(existing_decl, subprogram);
        semcheck_propagate_method_identity(symtab, subprogram);

        /* Propagate varargs flag from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.is_varargs)
            existing_decl->is_varargs = 1;

        /* Propagate INTERNPROC identifier from tree to hash node */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.internproc_id != NULL)
        {
            if (existing_decl->internproc_id != NULL)
                free(existing_decl->internproc_id);
            existing_decl->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
        }

        PushScope(symtab);
        if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            fprintf(stderr, "[KGPC] semcheck_subprogram (func): PushScope for %s\n",
                subprogram->tree_data.subprogram_data.id);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram);
        /* For nested types (e.g. TOuter.TInner), also add outer class
         * vars/consts to scope. owner_class_full contains the dotted path. */
        if (subprogram->tree_data.subprogram_data.owner_class_full != NULL)
            add_outer_class_vars_to_method_scope(symtab, subprogram);

        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with KgpcType
        // Always use _Typed variant, even if KgpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, id_to_use_for_lookup, return_kgpc_type);
        
/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" is already used in the current scope */
        HashNode_t *result_check = NULL;
        HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
        result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
        if (result_check == NULL)
        {
            /* "Result" is not already declared in this scope, so add it as an alias */
            if (return_kgpc_type != NULL)
                kgpc_type_retain(return_kgpc_type);
            PushFuncRetOntoScope_Typed(symtab, "Result", return_kgpc_type);
            if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] add Result alias for %s type=%s\n",
                    subprogram->tree_data.subprogram_data.id ?
                        subprogram->tree_data.subprogram_data.id : "<anon>",
                    kgpc_type_to_string(return_kgpc_type));
            }
        }
        else if (kgpc_getenv("KGPC_DEBUG_RESULT") != NULL)
        {
            fprintf(stderr,
                "[KGPC] Result alias already exists for %s existing_type=%s\n",
                subprogram->tree_data.subprogram_data.id ?
                    subprogram->tree_data.subprogram_data.id : "<anon>",
                kgpc_type_to_string(result_check->type));
        }

        /* For operator declarations with named result variables (e.g., "operator :=(src) dest: variant"),
         * push the named result variable as an additional alias for the return variable. */
        if (subprogram->tree_data.subprogram_data.result_var_name != NULL)
        {
            PushFuncRetOntoScope_Typed(symtab, subprogram->tree_data.subprogram_data.result_var_name, return_kgpc_type);
        }

        /* For class methods, also add an alias using the unmangled method name.
         * The method_name field contains the bare name (e.g., "_AddRef", "ReadNext"). */
        const char *alias_suffix = subprogram->tree_data.subprogram_data.method_name;
        if (alias_suffix != NULL && alias_suffix[0] != '\0')
        {
            size_t alias_len = strlen(alias_suffix);
            if (alias_len > 0 && alias_len < 128)
            {
                char alias_buf[128];
                memcpy(alias_buf, alias_suffix, alias_len);
                alias_buf[alias_len] = '\0';

                HashNode_t *suffix_check = NULL;
                if (FindIdent(&suffix_check, symtab, alias_buf) == -1)
                    PushFuncRetOntoScope_Typed(symtab, alias_buf, return_kgpc_type);
            }
        }
        /* Note: We don't check for "result" anymore since it conflicts with built-in Result alias */

        /* Note: Type metadata now in KgpcType, no post-creation writes needed */

        new_max_scope = max_scope_lev+1;
    }

    /**** Check the subprogram internals now ****/

    /* Greater than 0 signifies an error */
    if (func_return > 0)
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        HashNode_t *existing_decl = NULL;
        if (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0 &&
            existing_decl != NULL &&
            existing_decl->mangled_id != NULL &&
            subprogram->tree_data.subprogram_data.mangled_id != NULL &&
            strcmp(existing_decl->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
        {
            func_return = 0;
        }
        else
        {
            ListNode_t *candidates = FindAllIdents(symtab, id_to_use_for_lookup);
            ListNode_t *iter = candidates;
            while (iter != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)iter->cur;
                if (candidate != NULL && candidate->type != NULL &&
                    candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *def = candidate->type->info.proc_info.definition;
                    int existing_has_body = (def != NULL &&
                        def->tree_data.subprogram_data.statement_list != NULL);
                    if (existing_has_body != current_has_body)
                    {
                        func_return = 0;
                        break;
                    }
                }
                iter = iter->next;
            }
            if (candidates != NULL)
                DestroyList(candidates);
        }
    }
    if(func_return > 0)
    {
        fprintf(stderr, "On line %d: redeclaration of name %s!\n",
            subprogram->line_num, subprogram->tree_data.subprogram_data.id);

        return_val += func_return;
    }

    /* These arguments are themselves like declarations */
    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
        ListNode_t *arg_debug = subprogram->tree_data.subprogram_data.args_var;
        fprintf(stderr, "[KGPC] semcheck_subprogram: %s args:\n", subprogram->tree_data.subprogram_data.id);
        while (arg_debug != NULL) {
            if (arg_debug->type == LIST_TREE && arg_debug->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_debug->cur;
                if (arg_tree->type == TREE_VAR_DECL) {
                    ListNode_t *ids = arg_tree->tree_data.var_decl_data.ids;
                    if (ids != NULL && ids->cur != NULL)
                        fprintf(stderr, "  - %s (type_id=%s)\n", (char*)ids->cur,
                            arg_tree->tree_data.var_decl_data.type_id ? arg_tree->tree_data.var_decl_data.type_id : "<null>");
                }
            }
            arg_debug = arg_debug->next;
        }
    }
    /* Register generic type parameters (e.g., T, U) as opaque types in the function scope.
     * This allows parameters of type T and expressions using T to pass semantic checking. */
    if (subprogram->tree_data.subprogram_data.num_generic_type_params > 0) {
        for (int i = 0; i < subprogram->tree_data.subprogram_data.num_generic_type_params; i++) {
            const char *tparam = subprogram->tree_data.subprogram_data.generic_type_params[i];
            assert(tparam != NULL);
            KgpcType *opaque = create_primitive_type(POINTER_TYPE);
            PushTypeOntoScope_Typed(symtab, (char *)tparam, opaque);
            destroy_kgpc_type(opaque);
        }
    } else {
        /* Some parsed method implementations (notably generic class methods in FPC RTL)
         * may omit explicit generic_type_params metadata even though their signatures
         * contain placeholders like T/U. Infer only single-letter uppercase params. */
        ListNode_t *arg_node = subprogram->tree_data.subprogram_data.args_var;
        while (arg_node != NULL) {
            if (arg_node->type == LIST_TREE && arg_node->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_node->cur;
                const char *type_id = NULL;
                if (arg_tree->type == TREE_VAR_DECL)
                    type_id = arg_tree->tree_data.var_decl_data.type_id;
                else if (arg_tree->type == TREE_ARR_DECL)
                    type_id = arg_tree->tree_data.arr_decl_data.type_id;
                if (type_id != NULL && type_id[0] >= 'A' && type_id[0] <= 'Z' &&
                    type_id[1] == '\0')
                {
                    HashNode_t *existing = NULL;
                    if (FindIdent(&existing, symtab, type_id) == -1) {
                        KgpcType *opaque = create_primitive_type(POINTER_TYPE);
                        PushTypeOntoScope_Typed(symtab, (char *)type_id, opaque);
                        destroy_kgpc_type(opaque);
                    }
                }
            }
            arg_node = arg_node->next;
        }

        const char *ret_type_id = subprogram->tree_data.subprogram_data.return_type_id;
        if (ret_type_id != NULL && ret_type_id[0] >= 'A' && ret_type_id[0] <= 'Z' &&
            ret_type_id[1] == '\0')
        {
            HashNode_t *existing = NULL;
            if (FindIdent(&existing, symtab, ret_type_id) == -1) {
                KgpcType *opaque = create_primitive_type(POINTER_TYPE);
                PushTypeOntoScope_Typed(symtab, (char *)ret_type_id, opaque);
                destroy_kgpc_type(opaque);
            }
        }
    }
    /* Set unit_context early — before args processing — so type resolution
     * for parameter types (e.g. TSize) picks the correct unit-specific type
     * rather than a shadowing local program type. */
    int saved_unit_context = symtab->unit_context;
    if (subprogram->tree_data.subprogram_data.defined_in_unit &&
        subprogram->tree_data.subprogram_data.source_unit_index > 0)
    {
        symtab->unit_context = subprogram->tree_data.subprogram_data.source_unit_index;
    }

    {
        int before_args = return_val;
        int saved_imported_unit = g_semcheck_imported_decl_unit_index;
        if (subprogram->tree_data.subprogram_data.defined_in_unit)
        {
            g_semcheck_imported_decl_unit_index = subprogram->tree_data.subprogram_data.source_unit_index;
            if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL)
            {
                /* Check if any arg has TSize type_id */
                ListNode_t *a = subprogram->tree_data.subprogram_data.args_var;
                while (a != NULL)
                {
                    if (a->type == LIST_TREE && a->cur != NULL)
                    {
                        Tree_t *ad = (Tree_t *)a->cur;
                        if (ad->type == TREE_VAR_DECL && ad->tree_data.var_decl_data.type_id != NULL &&
                            pascal_identifier_equals(ad->tree_data.var_decl_data.type_id, "TSize"))
                        {
                            fprintf(stderr, "[SUBPROGRAM_TSIZE] func=%s source_unit_idx=%d param_type_id=%s\n",
                                subprogram->tree_data.subprogram_data.id ? subprogram->tree_data.subprogram_data.id : "<null>",
                                subprogram->tree_data.subprogram_data.source_unit_index,
                                ad->tree_data.var_decl_data.type_id);
                        }
                    }
                    a = a->next;
                }
            }
        }
        return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.args_var);
        g_semcheck_imported_decl_unit_index = saved_imported_unit;
        semcheck_debug_error_step("args_decls", subprogram, before_args, return_val);
    }
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after args: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    /* Ensure parameter identifiers are present in scope even if parsing produced
     * incomplete type info (e.g., untyped const params in helper methods). */
    {
        ListNode_t *param_node = subprogram->tree_data.subprogram_data.args_var;
        while (param_node != NULL)
        {
            if (param_node->type == LIST_TREE && param_node->cur != NULL)
            {
                Tree_t *param_tree = (Tree_t *)param_node->cur;
                ListNode_t *ids = NULL;
                if (param_tree->type == TREE_VAR_DECL)
                    ids = param_tree->tree_data.var_decl_data.ids;
                else if (param_tree->type == TREE_ARR_DECL)
                    ids = param_tree->tree_data.arr_decl_data.ids;

                while (ids != NULL)
                {
                    HashNode_t *existing = NULL;
                    if (FindIdent(&existing, symtab, ids->cur) == -1)
                    {
                        KgpcType *param_type = NULL;
                        if (param_tree->type == TREE_VAR_DECL)
                        {
                            param_type = param_tree->tree_data.var_decl_data.cached_kgpc_type;
                            if (param_type == NULL && param_tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                int builtin_tag = semcheck_map_builtin_type_name_local(
                                    param_tree->tree_data.var_decl_data.type_id);
                                if (builtin_tag != UNKNOWN_TYPE)
                                    param_type = create_primitive_type(builtin_tag);
                            }
                            if (param_type != NULL &&
                                param_type == param_tree->tree_data.var_decl_data.cached_kgpc_type)
                            {
                                kgpc_type_retain(param_type);
                            }
                            PushVarOntoScope_Typed(symtab, (char *)ids->cur, param_type);
                            if (param_type != NULL &&
                                param_type != param_tree->tree_data.var_decl_data.cached_kgpc_type)
                            {
                                destroy_kgpc_type(param_type);
                            }
                        }
                        else if (param_tree->type == TREE_ARR_DECL)
                        {
                            PushArrayOntoScope_Typed(symtab, (char *)ids->cur, NULL);
                        }

                        if (FindIdent(&existing, symtab, ids->cur) != -1 && existing != NULL)
                        {
                            int is_var_param = 0;
                            int is_untyped = 0;
                            if (param_tree->type == TREE_VAR_DECL)
                            {
                                is_var_param = param_tree->tree_data.var_decl_data.is_var_param;
                                is_untyped = param_tree->tree_data.var_decl_data.is_untyped_param;
                            }
                            existing->is_var_parameter = (is_var_param || is_untyped) ? 1 : 0;
                        }
                    }
                    ids = ids->next;
                }
            }
            param_node = param_node->next;
        }
    }

    /* Ensure methods always have an implicit Self in scope when missing.
     * This is critical for record/class method bodies that access bare fields
     * (e.g. cx/cy) and for helper methods with expanded signatures. */
    if (kgpc_getenv("KGPC_ASSERT_STATIC_SELF") != NULL)
    {
        if (subprogram->tree_data.subprogram_data.is_static_method)
        {
            HashNode_t *self_node = NULL;
            assert(FindIdent(&self_node, symtab, "Self") == -1);
        }
    }
    if (!subprogram->tree_data.subprogram_data.is_static_method)
    {
        HashNode_t *self_node = NULL;
        const char *owner_id = semcheck_get_current_method_owner();
        struct RecordType *owner_record = NULL;
        int self_is_var_param = 0;
        if (owner_id != NULL)
        {
            HashNode_t *owner_node = semcheck_find_owner_record_type_node(symtab, owner_id);
            if (owner_node == NULL)
                owner_node = semcheck_find_preferred_type_node(symtab, owner_id);
            if (owner_node != NULL)
                owner_record = get_record_type_from_node(owner_node);
        }
        if (owner_record != NULL)
        {
            KgpcType *self_type = NULL;

            if (owner_record->is_type_helper && owner_record->helper_base_type_id != NULL)
            {
                self_is_var_param = semcheck_helper_self_is_var(symtab,
                    owner_record->helper_base_type_id);
                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                    owner_record->helper_base_type_id);
                if (type_node != NULL && type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    self_type = type_node->type;
                }
                else
                {
                    int builtin_tag = semcheck_map_builtin_type_name_local(
                        owner_record->helper_base_type_id);
                    if (builtin_tag != UNKNOWN_TYPE)
                        self_type = create_primitive_type(builtin_tag);
                }
            }
            else
            {
                KgpcType *owner_kgpc = create_record_type(owner_record);
                if (owner_kgpc != NULL && record_type_is_class(owner_record))
                {
                    KgpcType *ptr = create_pointer_type(owner_kgpc);
                    if (ptr != NULL)
                    {
                        kgpc_type_release(owner_kgpc);
                        owner_kgpc = ptr;
                    }
                    self_is_var_param = 0;
                }
                else
                {
                    /* Non-class records pass Self by reference. */
                    self_is_var_param = 1;
                }
                self_type = owner_kgpc;
            }

            if (self_type != NULL)
            {
                if (FindIdent(&self_node, symtab, "Self") == -1)
                {
                    kgpc_type_retain(self_type);
                    PushVarOntoScope_Typed(symtab, "Self", self_type);
                    if (FindIdent(&self_node, symtab, "Self") != -1 && self_node != NULL)
                        self_node->is_var_parameter = self_is_var_param;
                }
                else if (self_node->type == NULL || !kgpc_type_equals(self_node->type, self_type))
                {
                    if (self_node->type != NULL)
                        destroy_kgpc_type(self_node->type);
                    kgpc_type_retain(self_type);
                    self_node->type = self_type;
                }
                if (self_node != NULL)
                    self_node->is_var_parameter = self_is_var_param;
                destroy_kgpc_type(self_type);
            }
        }
    }

    {
        int before_local = return_val;
        return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        /* Pre-declare types so they're available for const expressions like High(MyType) */
        return_val += predeclare_types(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
        return_val += semcheck_type_decls(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);
        semcheck_debug_error_step("local_decls", subprogram, before_local, return_val);
    }
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after decls: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    {
        int before_nested = return_val;
        return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                        new_max_scope, subprogram);
        semcheck_debug_error_step("nested_subprograms", subprogram, before_nested, return_val);
    }

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        symtab->unit_context = saved_unit_context;
        PopScope(symtab);
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_subprogram %s returning (no body): %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        return return_val;
    }

    /* Suppress source_index usage for unit-imported subprogram bodies.
     * Their expressions carry source_index values from a different
     * preprocessed buffer, and resolve_error_source_context cannot
     * disambiguate buffers, producing misleading file:line locations. */
    int saved_suppress = g_semcheck_error_suppress_source_index;
    int saved_error_source_index = g_semcheck_error_source_index;
    const char *saved_error_unit_context = g_semcheck_error_unit_context;
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
    {
        g_semcheck_error_suppress_source_index = 1;
        g_semcheck_error_source_index = -1;
        /* Show unit name instead of misleading main-program file:line */
        if (subprogram->tree_data.subprogram_data.source_unit_index > 0)
        {
            const char *uname = unit_registry_get(subprogram->tree_data.subprogram_data.source_unit_index);
            g_semcheck_error_unit_context = uname ? uname : "<unit>";
            /* Set unit context so FindIdent resolves to this unit's symbols
             * when variable names collide across units (e.g., Input: Text vs Input: String) */
            symtab->unit_context = subprogram->tree_data.subprogram_data.source_unit_index;
        }
    }

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        {
            int before_body = return_val;
            return_val += semcheck_stmt(symtab, body, new_max_scope);
            semcheck_debug_error_step("proc_body", subprogram, before_body, return_val);
        }
    }
    else
    {
        assert(FindIdent(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    == 0);

        ResetHashNodeStatus(hash_return);
        int func_stmt_ret = 0;
        {
            int before_body = return_val;
            func_stmt_ret = semcheck_func_stmt(symtab, body, new_max_scope);
            return_val += func_stmt_ret;
            semcheck_debug_error_step("func_body", subprogram, before_body, return_val);
        }
#ifdef DEBUG
        if (func_stmt_ret > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after semcheck_func_stmt: %d\n", subprogram->tree_data.subprogram_data.id, func_stmt_ret);
#endif

        /* Allow functions with asm blocks to skip explicit return assignment */
        int has_asm = statement_contains_asm_block(body);
        
        /* Constructors implicitly yield the constructed instance, so do not
         * require an explicit assignment to the return variable. */
        int is_constructor = subprogram->tree_data.subprogram_data.is_constructor;

        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = is_constructor ? 1 : (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }

        /* Methods use mangled identifiers like Class__Func; allow assignments to the
         * unmangled method name to satisfy the return check. */
        if (!return_was_assigned && subprogram->tree_data.subprogram_data.method_name != NULL) {
            const char *mname = subprogram->tree_data.subprogram_data.method_name;
            HashNode_t *suffix_node = NULL;
            if (FindIdent(&suffix_node, symtab, mname) == 0 && suffix_node != NULL) {
                return_was_assigned = (suffix_node->mutated != NO_MUTATE);
            }
        }
        
        /* Pascal allows functions to exit without explicitly assigning the result;
         * the value is simply uninitialized. Do not emit a warning for this. */
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        optimize(symtab, subprogram);
    }

    if (subprogram->tree_data.subprogram_data.id != NULL)
    {
        ListNode_t *defs = FindAllIdents(symtab, subprogram->tree_data.subprogram_data.id);
        ListNode_t *iter = defs;
        while (iter != NULL)
        {
            if (iter->cur != NULL)
            {
                HashNode_t *node = (HashNode_t *)iter->cur;
                if (node != NULL &&
                    node->mangled_id != NULL &&
                    subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                    strcmp(node->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    node->requires_static_link =
                        subprogram->tree_data.subprogram_data.requires_static_link ? 1 : 0;
                    node->has_nested_requiring_link =
                        subprogram->tree_data.subprogram_data.has_nested_requiring_link ? 1 : 0;
                    if (subprogram->tree_data.subprogram_data.is_varargs)
                        node->is_varargs = 1;
                }
            }
            iter = iter->next;
        }
        DestroyList(defs);
    }

    g_semcheck_current_subprogram = prev_current_subprogram;
    PopScope(symtab);

    /* Restore error context / suppress flag after body processing. */
    g_semcheck_error_suppress_source_index = saved_suppress;
    g_semcheck_error_source_index = saved_error_source_index;
    g_semcheck_error_unit_context = saved_error_unit_context;
    symtab->unit_context = saved_unit_context;

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s returning at end: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
    if (owner_copy != NULL)
    {
        semcheck_set_current_method_owner(prev_owner);
        free(owner_copy);
    }

    return return_val;
}


/* Pre-declare a subprogram (add to symbol table without processing body)
 * This is used for forward declarations so all procedures are visible
 * before any bodies are processed.
 */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram)
{
    int return_val = 0;
    int func_return;
    enum TreeType sub_type;
    SubprogramPredeclLookup lookup;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    /* Set unit_context during predeclaration so type resolution (e.g. for
     * parameter types like TSize) picks the correct unit-specific type
     * instead of a local program type that happens to shadow it. */
    int saved_unit_context = symtab->unit_context;
    if (subprogram->tree_data.subprogram_data.defined_in_unit &&
        subprogram->tree_data.subprogram_data.source_unit_index > 0)
    {
        symtab->unit_context = subprogram->tree_data.subprogram_data.source_unit_index;
    }

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Debug output for procedure predeclaration */
    if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC] %s (line %d) parent=%s\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->line_num,
                parent_subprogram != NULL ? parent_subprogram->tree_data.subprogram_data.id : "(null)");
    }

    // --- Name Mangling Logic ---
    /* Preserve any dotted class prefix from the original mangled_id set by
     * convert_method_impl (e.g. "TOuter.TInner__DoWork").  This is needed
     * so that semcheck_get_current_method_owner can walk up to outer classes
     * when resolving class vars / consts in nested object methods. */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        free(subprogram->tree_data.subprogram_data.mangled_id);
        subprogram->tree_data.subprogram_data.mangled_id = NULL;
    }

    const char *predeclare_name = subprogram->tree_data.subprogram_data.cname_override;
    if (predeclare_name != NULL) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(predeclare_name);
    } else if (subprogram->tree_data.subprogram_data.cname_flag) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(subprogram->tree_data.subprogram_data.id);
    } else {
        // Pass the symbol table to the mangler
        char *base_mangled = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab);

        // For nested functions, prepend the parent's mangled_id to make the name unique
        if (parent_subprogram != NULL && parent_subprogram->tree_data.subprogram_data.mangled_id != NULL) {
            const char *parent_mangled = parent_subprogram->tree_data.subprogram_data.mangled_id;
            size_t len = strlen(parent_mangled) + 2 + strlen(base_mangled) + 1; // parent$nested\0
            char *nested_mangled = malloc(len);
            if (nested_mangled != NULL) {
                snprintf(nested_mangled, len, "%s$%s", parent_mangled, base_mangled);
                free(base_mangled);
                subprogram->tree_data.subprogram_data.mangled_id = nested_mangled;
            } else {
                subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
            }
        } else {
            subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC]   -> mangled_id=%s\n",
                subprogram->tree_data.subprogram_data.mangled_id);
    }

    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;
    subprogram->tree_data.subprogram_data.cached_predecl_node = NULL;
    lookup = semcheck_lookup_subprogram_predecl(
        symtab,
        subprogram,
        id_to_use_for_lookup,
        subprogram->tree_data.subprogram_data.mangled_id);

    if (lookup.tree_match != NULL)
    {
        semcheck_refresh_predecl_match(lookup.tree_match, subprogram);
        subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)lookup.tree_match;
        symtab->unit_context = saved_unit_context;
        return 0;  /* Already declared - skip to avoid duplicates */
    }

    if (lookup.exact_match != NULL)
    {
        semcheck_refresh_predecl_match(lookup.exact_match, subprogram);
        subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)lookup.exact_match;
        symtab->unit_context = saved_unit_context;
        return 0;  /* Already declared - skip to avoid duplicates */
    }

    if (lookup.body_pair_match != NULL)
    {
        semcheck_refresh_predecl_match(lookup.body_pair_match, subprogram);
        subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)lookup.body_pair_match;
        symtab->unit_context = saved_unit_context;
        return 0;  /* Declaration/body pair already tracked */
    }
    
    /**** PLACE SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        if (proc_type != NULL) {
            proc_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }
        
        // Add to current scope
        func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        proc_type);

        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }

        /* Propagate flags and method identity to the hash node */
        if (func_return == 0) {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, id_to_use_for_lookup) == 0 && node != NULL) {
                if (subprogram->tree_data.subprogram_data.is_varargs)
                    node->is_varargs = 1;
                if (subprogram->tree_data.subprogram_data.defined_in_unit)
                    node->defined_in_unit = 1;
                if (subprogram->tree_data.subprogram_data.internproc_id != NULL) {
                    if (node->internproc_id != NULL) free(node->internproc_id);
                    node->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
                }
                copy_method_identity_to_node(node, subprogram);
            }
        }
        if (func_return == 0)
        {
            SubprogramPredeclLookup pushed_lookup = semcheck_lookup_subprogram_predecl(
                symtab, subprogram, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            HashNode_t *cached = pushed_lookup.exact_match != NULL ?
                pushed_lookup.exact_match : pushed_lookup.first_mangled_match;
            subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)cached;
        }
        /* Release creator's reference - hash table retained its own */
        destroy_kgpc_type(proc_type);
    }
    else // Function
    {
        KgpcType *return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 1);
#ifdef DEBUG
        if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

        /* Create function KgpcType */
        KgpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_kgpc_type  /* functions have a return type */
        );
        if (func_type != NULL) {
            func_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                func_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }

        // Add to current scope
        func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        func_type);

        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }

        /* Propagate flags and method identity to the hash node */
        if (func_return == 0) {
            HashNode_t *node = NULL;
            if (FindIdent(&node, symtab, id_to_use_for_lookup) == 0 && node != NULL) {
                if (subprogram->tree_data.subprogram_data.is_varargs)
                    node->is_varargs = 1;
                if (subprogram->tree_data.subprogram_data.defined_in_unit)
                    node->defined_in_unit = 1;
                if (subprogram->tree_data.subprogram_data.internproc_id != NULL) {
                    if (node->internproc_id != NULL) free(node->internproc_id);
                    node->internproc_id = strdup(subprogram->tree_data.subprogram_data.internproc_id);
                }
                copy_method_identity_to_node(node, subprogram);
            }
        }
        if (func_return == 0)
        {
            SubprogramPredeclLookup pushed_lookup = semcheck_lookup_subprogram_predecl(
                symtab, subprogram, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            HashNode_t *cached = pushed_lookup.exact_match != NULL ?
                pushed_lookup.exact_match : pushed_lookup.first_mangled_match;
            subprogram->tree_data.subprogram_data.cached_predecl_node = (struct HashNode *)cached;
        }
        /* Release creator's reference - hash table retained its own */
        destroy_kgpc_type(func_type);
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s returning error: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    symtab->unit_context = saved_unit_context;
    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
/* Forward declaration - we'll define this after semcheck_subprogram */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram);
static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

/* Predeclare a list of subprograms without processing bodies.
 * Safe to call multiple times thanks to duplicate checks in predeclare_subprogram. */
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    int return_val = 0;
    ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev, parent_subprogram);
        /* Note: We intentionally do NOT predeclare nested subprograms here globally.
         * Nested subprograms are predeclared within their parent's scope when
         * semcheck_subprogram calls semcheck_subprograms (which has its own Pass 1).
         * This ensures nested functions with the same name in different parents
         * don't shadow each other. */
        cur = cur->next;
    }
    return return_val;
}

int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprograms called. subprograms=%p\n", subprograms);
#endif

    return_val = 0;
    
    /* ARCHITECTURAL FIX: Two-pass approach to ensure all procedure declarations
     * are visible before processing any bodies. This fixes the issue where unit
     * procedures were not visible in nested user procedures because they came
     * later in the subprograms list.
     * 
     * Pass 1: Declare all procedures (add to symbol table)
     * Pass 2: Process bodies (which may reference procedures declared in pass 1)
     */
    
    /* Pass 1: Pre-declare all procedures at this level.
     * NOTE: Do NOT skip subprograms that already have cached_predecl_node.
     * predeclare_subprograms() is called multiple times (once globally from
     * semcheck_program, then again per-scope from semcheck_subprograms).
     * Between calls, type aliases like PByte get resolved from integer to
     * pointer, changing the mangled name (e.g. _p_i_i → _p_p_i).  Skipping
     * the second predeclare leaves the HashNode with the stale mangled name,
     * so the VMT references an undefined symbol at link time. */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev, parent_subprogram);
        cur = cur->next;
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprograms error after Pass 1: %d\n", return_val);
#endif

    /* Pass 2: Process full semantic checking including bodies */
    int skip_imported_bodies = (kgpc_getenv("KGPC_SKIP_IMPORTED_IMPL_BODIES") != NULL);
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        if (child != NULL &&
            child->tree_data.subprogram_data.defined_in_unit &&
            child->tree_data.subprogram_data.statement_list != NULL &&
            skip_imported_bodies)
        {
            /* Imported unit implementation bodies are not part of the consumer
             * unit's semantic pass. Keep declarations (pass 1) but skip bodies. */
            cur = cur->next;
            continue;
        }
        if (child != NULL &&
            child->tree_data.subprogram_data.statement_list == NULL &&
            child->tree_data.subprogram_data.cname_flag == 0 &&
            child->tree_data.subprogram_data.cname_override == NULL)
        {
            /* Interface/forward declaration with no body. */
            cur = cur->next;
            continue;
        }
        return_val += semcheck_subprogram(symtab, child, max_scope_lev);
        /* If child needs a static link, mark parent as having nested children that need links.
         * This is used by codegen to know when to PASS a static link when calling nested functions.
         * We do NOT propagate requires_static_link to parent - the parent only needs to RECEIVE
         * a static link if it's nested itself or accesses outer scope variables. */
        if (parent_subprogram != NULL &&
            child != NULL &&
            child->tree_data.subprogram_data.requires_static_link)
        {
            parent_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
        }
        cur = cur->next;
    }

    return return_val;
}
