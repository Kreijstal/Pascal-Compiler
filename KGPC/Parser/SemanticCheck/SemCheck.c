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
#else
#define strcasecmp _stricmp
#endif
#include <math.h>
#include "SemCheck.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../Optimizer/optimizer.h"
#include "../pascal_frontend.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/type_tags.h"
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
#include "SemCheck_Internal.h"
#include <stdarg.h>

ListNode_t *g_semcheck_unit_names = NULL;
char *g_semcheck_current_unit_name = NULL;
char *g_semcheck_source_path = NULL;
char *g_semcheck_source_buffer = NULL;
size_t g_semcheck_source_length = 0;
int g_semcheck_warning_count = 0;

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

int semcheck_print_context_from_file(const char *file_path, int line_num, int col_num, int context_lines)
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
    if (g_semcheck_current_unit_name != NULL)
    {
        free(g_semcheck_current_unit_name);
        g_semcheck_current_unit_name = NULL;
    }
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

int semcheck_is_unit_name(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return 0;
    if (pascal_identifier_equals(name, "System"))
        return 1;
    if (g_semcheck_current_unit_name != NULL &&
        pascal_identifier_equals(name, g_semcheck_current_unit_name))
        return 1;

    ListNode_t *cur = g_semcheck_unit_names;
    while (cur != NULL)
    {
        const char *existing = (const char *)cur->cur;
        if (existing != NULL && pascal_identifier_equals(existing, name))
            return 1;
        cur = cur->next;
    }
    return 0;
}

/* Helper declared in SemCheck_expr.c */
const char *semcheck_base_type_name(const char *id)
{
    if (id == NULL)
        return NULL;
    const char *dot = strrchr(id, '.');
    return (dot != NULL && dot[1] != '\0') ? (dot + 1) : id;
}

int semcheck_map_builtin_type_name_local(const char *id)
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
    if (pascal_identifier_equals(id, "Real"))
        return REAL_TYPE;
    if (pascal_identifier_equals(id, "Single") ||
        pascal_identifier_equals(id, "Double") ||
        pascal_identifier_equals(id, "Extended"))
        return REAL_TYPE;
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
    if (pascal_identifier_equals(id, "String") ||
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

char *semcheck_dup_type_id_from_ast(ast_t *node)
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

int semcheck_is_char_alias_name(const char *id)
{
    return (id != NULL &&
        (pascal_identifier_equals(id, "Char") ||
            pascal_identifier_equals(id, "AnsiChar") ||
            pascal_identifier_equals(id, "WideChar") ||
            pascal_identifier_equals(id, "UnicodeChar")));
}

int semcheck_alias_should_be_char_like(const char *alias_id, const char *target_id)
{
    return (semcheck_is_char_alias_name(alias_id) ||
        semcheck_is_char_alias_name(target_id));
}

int semcheck_is_currency_type_id(const char *type_id)
{
    const char *base = semcheck_base_type_name(type_id);
    return (base != NULL && pascal_identifier_equals(base, "Currency"));
}

int map_var_type_to_type_tag(enum VarType var_type)
{
    switch (var_type)
    {
        case HASHVAR_INTEGER:
            return INT_TYPE;
        case HASHVAR_LONGINT:
            return LONGINT_TYPE;
        case HASHVAR_INT64:
            return INT64_TYPE;
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

int semcheck_find_ident_with_qualified_fallback(HashNode_t **out, SymTab_t *symtab,
    const char *id)
{
    if (out == NULL || symtab == NULL || id == NULL)
        return -1;

    int found = FindIdent(out, symtab, id);
    if (found >= 0 && out != NULL && *out != NULL)
        return found;

    const char *dot = strrchr(id, '.');
    if (dot != NULL && dot[1] != '\0')
        return FindIdent(out, symtab, dot + 1);

    return found;
}

HashNode_t *semcheck_find_type_excluding_alias(SymTab_t *symtab, const char *type_id,
    struct TypeAlias *exclude_alias)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
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

int semcheck_param_decl_equivalent(const Tree_t *lhs, const Tree_t *rhs)
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

int semcheck_subprogram_signatures_equivalent(const Tree_t *lhs, const Tree_t *rhs)
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

ListNode_t *semcheck_create_builtin_param(const char *name, int type_tag)
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

ListNode_t *semcheck_create_builtin_param_var(const char *name, int type_tag)
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

/* Adds built-in functions */
void semcheck_add_builtins(SymTab_t *symtab);

/* Internal helper to print context using either offset or line-based search */
void print_error_context(int line_num, int col_num, int source_index)
{
    const char *file_path = (file_to_parse != NULL && *file_to_parse != '\0')
                                ? file_to_parse
                                : ((preprocessed_path != NULL && *preprocessed_path != '\0') ? preprocessed_path
                                                                                              : pascal_frontend_current_path());
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
        if (getenv("KGPC_DEBUG_SEM_CONTEXT") != NULL)
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
        /* Use offset-based context if available, otherwise fall back to line-based */
        int printed = print_source_context_at_offset(context_buf, context_buf_len, source_index, line_num, col_num, 2);
        if (!printed)
            printed = semcheck_print_context_from_file(file_path, line_num, col_num, 2);
        if (!printed && file_path != NULL)
            print_source_context(file_path, line_num, col_num, 2);
    }

    fprintf(stderr, "\n");
}

int semcheck_parse_line_directive(const char *line, size_t len)
{
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

    return line_num > 0 ? line_num : -1;
}

int semcheck_line_from_source_offset(const char *buffer, size_t length, int source_offset)
{
    if (buffer == NULL || length == 0 || source_offset < 0 || (size_t)source_offset >= length)
        return -1;

    int current_line_at_offset = 1;
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

        int directive_line = semcheck_parse_line_directive(buffer + line_start, line_len);
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
            current_line_at_offset = directive_line + lines_after_directive;
            break;
        }

        if (line_start > 0)
            scan_pos = line_start - 1;
        else
            break;
    }

    return current_line_at_offset;
}

int g_semcheck_error_line = 0;
int g_semcheck_error_col = 0;
int g_semcheck_error_source_index = -1;

void semcheck_set_error_context(int line_num, int col_num, int source_index)
{
    g_semcheck_error_line = line_num;
    g_semcheck_error_col = col_num;
    g_semcheck_error_source_index = source_index;
}

void semcheck_clear_error_context(void)
{
    g_semcheck_error_line = 0;
    g_semcheck_error_col = 0;
    g_semcheck_error_source_index = -1;
}

int semcheck_tag_from_kgpc(const KgpcType *type)
{
    if (type == NULL)
        return UNKNOWN_TYPE;
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
    return UNKNOWN_TYPE;
}

/* Helper function to print semantic error with source code context */
void semantic_error(int line_num, int col_num, const char *format, ...)
{
    int effective_line = line_num;
    int effective_col = col_num;
    int effective_source_index = g_semcheck_error_source_index;

    if (effective_source_index >= 0)
    {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = preprocessed_length;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        int computed_line = semcheck_line_from_source_offset(context_buf, context_buf_len, effective_source_index);
        if (computed_line > 0)
            effective_line = computed_line;
        if (effective_col <= 0 && g_semcheck_error_col > 0)
            effective_col = g_semcheck_error_col;
    }

    fprintf(stderr, "Error on line %d", effective_line);
    if (effective_col > 0) {
        fprintf(stderr, ", column %d", effective_col);
    }
    fprintf(stderr, ": ");

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");

    print_error_context(effective_line, effective_col, effective_source_index);
}

/* Helper function to print semantic error with accurate source context using byte offset */
void semantic_error_at(int line_num, int col_num, int source_index, const char *format, ...)
{
    if (source_index >= 0)
    {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = preprocessed_length;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        int computed_line = semcheck_line_from_source_offset(context_buf, context_buf_len, source_index);
        if (computed_line > 0)
            line_num = computed_line;
    }

    fprintf(stderr, "Error on line %d", line_num);
    if (col_num > 0) {
        fprintf(stderr, ", column %d", col_num);
    }
    fprintf(stderr, ": ");

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");

    print_error_context(line_num, col_num, source_index);
}

void semcheck_error_with_context_at(int line_num, int col_num, int source_index,
    const char *format, ...)
{
    const char *file_path = (file_to_parse != NULL && *file_to_parse != '\0')
                                ? file_to_parse
                                : ((preprocessed_path != NULL && *preprocessed_path != '\0') ? preprocessed_path
                                                                                              : pascal_frontend_current_path());
    if (file_path == NULL)
        file_path = g_semcheck_source_path;
    size_t context_len = preprocessed_length;
    if (context_len == 0 && preprocessed_source != NULL)
        context_len = strlen(preprocessed_source);

    int effective_line = line_num;
    int effective_col = col_num;
    if (source_index >= 0)
    {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = context_len;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        int computed_line = semcheck_line_from_source_offset(context_buf, context_buf_len, source_index);
        if (computed_line > 0)
            effective_line = computed_line;
        if (effective_col <= 0 && g_semcheck_error_col > 0)
            effective_col = g_semcheck_error_col;
    }

    va_list args;
    va_start(args, format);
    if (format != NULL && strncmp(format, "Error on line %d", 16) == 0)
    {
        int original_line = va_arg(args, int);
        (void)original_line;
        fprintf(stderr, "Error on line %d", effective_line);
        const char *rest = format + 16;
        vfprintf(stderr, rest, args);
    }
    else
    {
        vfprintf(stderr, format, args);
    }
    va_end(args);

    size_t len = format ? strlen(format) : 0;
    if (len == 0 || format[len - 1] != '\n') {
        fprintf(stderr, "\n");
    }

    if (effective_line > 0) {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = context_len;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        if (getenv("KGPC_DEBUG_SEM_CONTEXT") != NULL)
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

/* Helper for legacy error prints that already include "Error on line %d". */
void semcheck_error_with_context(const char *format, ...)
{
    const char *file_path = (file_to_parse != NULL && *file_to_parse != '\0')
                                ? file_to_parse
                                : ((preprocessed_path != NULL && *preprocessed_path != '\0') ? preprocessed_path
                                                                                              : pascal_frontend_current_path());
    if (file_path == NULL)
        file_path = g_semcheck_source_path;
    size_t context_len = preprocessed_length;
    if (context_len == 0 && preprocessed_source != NULL)
        context_len = strlen(preprocessed_source);
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

    if (effective_source_index >= 0)
    {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = preprocessed_length;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        int computed_line = semcheck_line_from_source_offset(context_buf, context_buf_len, effective_source_index);
        if (computed_line > 0)
            effective_line = computed_line;
        if (g_semcheck_error_col > 0)
            effective_col = g_semcheck_error_col;
    }
    if (effective_line <= 0)
        effective_line = line_num;

    /* If the format starts with "Error on line %d", rewrite that part to use the effective line. */
    if (format != NULL && strncmp(format, "Error on line %d", 16) == 0)
    {
        int original_line = va_arg(args, int);
        (void)original_line;
        fprintf(stderr, "Error on line %d", effective_line);
        const char *rest = format + 16;
        vfprintf(stderr, rest, args);
    }
    else
    {
        vfprintf(stderr, format, args);
    }
    va_end(args);

    size_t len = format ? strlen(format) : 0;
    if (len == 0 || format[len - 1] != '\n') {
        fprintf(stderr, "\n");
    }

    if (effective_line > 0) {
        const char *context_buf = preprocessed_source;
        size_t context_buf_len = context_len;
        if (context_buf == NULL || context_buf_len == 0)
        {
            context_buf = g_semcheck_source_buffer;
            context_buf_len = g_semcheck_source_length;
        }
        if (getenv("KGPC_DEBUG_SEM_CONTEXT") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] context file=%s pre_len=%zu buf_len=%zu line=%d col=%d\n",
                file_path != NULL ? file_path : "<null>",
                context_len,
                context_buf_len,
                effective_line,
                effective_col);
        }
        int printed = print_source_context_at_offset(context_buf, context_buf_len,
            effective_source_index, effective_line, effective_col, 2);
        if (!printed)
            printed = semcheck_print_context_from_file(file_path, effective_line, effective_col, 2);
        if (!printed && file_path != NULL)
            print_source_context(file_path, effective_line, effective_col, 2);
    }

    fprintf(stderr, "\n");
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

void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias)
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
void apply_builtin_integer_alias_metadata(struct TypeAlias *alias, const char *type_name)
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

void inherit_alias_metadata(SymTab_t *symtab, struct TypeAlias *alias)
{
    if (symtab == NULL || alias == NULL || alias->target_type_id == NULL)
        return;

    HashNode_t *target_node = NULL;
    if (semcheck_find_ident_with_qualified_fallback(&target_node, symtab,
        alias->target_type_id) == -1 || target_node == NULL)
        return;

    struct TypeAlias *target_alias = hashnode_get_type_alias(target_node);
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
}

/* Helper function to get RecordType from HashNode */
/**
 * Copy default parameter values from forward declaration to implementation.
 * When a method is declared in a class with default values but implemented
 * without them, the implementation's params need the defaults for overload resolution.
 */
HashNode_t *semcheck_find_preferred_type_node(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    if (matches == NULL && strchr(type_id, '$') != NULL)
    {
        /* On-demand generic specialization (e.g., TFPGListEnumerator$TMyRecord). */
        char *base_name = NULL;
        char **arg_types = NULL;
        int arg_count = 0;
        const char *cursor = type_id;
        const char *dollar = strchr(cursor, '$');
        if (dollar != NULL && dollar != cursor)
        {
            base_name = (char *)malloc((size_t)(dollar - cursor + 1));
            if (base_name != NULL)
            {
                memcpy(base_name, cursor, (size_t)(dollar - cursor));
                base_name[dollar - cursor] = '\0';
            }
        }

        if (base_name != NULL)
        {
            const char *tmp = dollar;
            while (tmp != NULL)
            {
                arg_count++;
                tmp = strchr(tmp + 1, '$');
            }
            if (arg_count > 0)
            {
                arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
                if (arg_types != NULL)
                {
                    int idx = 0;
                    const char *seg = dollar + 1;
                    while (seg != NULL && idx < arg_count)
                    {
                        const char *next = strchr(seg, '$');
                        size_t seg_len = next ? (size_t)(next - seg) : strlen(seg);
                        arg_types[idx] = (char *)malloc(seg_len + 1);
                        if (arg_types[idx] == NULL)
                            break;
                        memcpy(arg_types[idx], seg, seg_len);
                        arg_types[idx][seg_len] = '\0';
                        idx++;
                        if (next == NULL)
                            break;
                        seg = next + 1;
                    }
                    if (idx != arg_count)
                    {
                        for (int i = 0; i < arg_count; ++i)
                            free(arg_types[i]);
                        free(arg_types);
                        arg_types = NULL;
                        arg_count = 0;
                    }
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
                    record->type_id = strdup(type_id);
                    record->generic_decl = generic;
                    record->num_generic_args = arg_count;
                    record->generic_args = arg_types;
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
                        PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
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
        free(base_name);

        matches = FindAllIdents(symtab, type_id);
    }
    if (matches == NULL)
    {
        const char *base = semcheck_base_type_name(type_id);
        if (base != NULL && base != type_id)
            matches = FindAllIdents(symtab, base);
    }
    HashNode_t *best = NULL;
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            if (best == NULL)
                best = node;
            else if (best->defined_in_unit && !node->defined_in_unit)
                best = node;
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

HashNode_t *semcheck_find_type_node_with_unit_flag(SymTab_t *symtab,
    const char *type_id, int defined_in_unit)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_id);
    HashNode_t *best = NULL;
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
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return best;
}

/* Helper function to get VarType from HashNode */
enum VarType get_var_type_from_node(HashNode_t *node)
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
                case QWORD_TYPE: return HASHVAR_INT64;
                case REAL_TYPE: return HASHVAR_REAL;
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

void mark_hashnode_unit_info(HashNode_t *node, int defined_in_unit, int is_public)
{
    if (node == NULL || !defined_in_unit)
        return;
    node->defined_in_unit = 1;
    node->unit_is_public = is_public ? 1 : 0;
}

Tree_t *g_semcheck_current_subprogram = NULL;

const char *semcheck_get_current_subprogram_id(void)
{
    if (g_semcheck_current_subprogram == NULL)
        return NULL;
    return g_semcheck_current_subprogram->tree_data.subprogram_data.id;
}

KgpcType *semcheck_get_current_subprogram_return_kgpc_type(SymTab_t *symtab, int *owns_type)
{
    if (owns_type != NULL)
        *owns_type = 0;
    if (g_semcheck_current_subprogram == NULL || symtab == NULL)
        return NULL;

    const char *rt_id = g_semcheck_current_subprogram->tree_data.subprogram_data.return_type_id;
    if (rt_id != NULL)
    {
        HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, rt_id);
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
                    allow = 0;
                }

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.ids != NULL)
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

int predeclare_enum_literals(SymTab_t *symtab, ListNode_t *type_decls);
int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls);
int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev, Tree_t *parent_subprogram);

int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev);
int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram);

/* Resolve the return type for a function declaration once so callers share the same KgpcType. */
HashNode_t *semcheck_find_type_node_with_kgpc_type(SymTab_t *symtab, const char *type_id)
{
    if (symtab == NULL || type_id == NULL)
        return NULL;

    /* Prefer the generic-aware lookup so we can instantiate types like
     * TFPGListEnumerator$TMyRecord on demand for function return types. */
    HashNode_t *preferred = semcheck_find_preferred_type_node(symtab, type_id);
    if (preferred != NULL && preferred->type != NULL)
        return preferred;

    HashNode_t *result = NULL;
    ListNode_t *all_nodes = FindAllIdents(symtab, type_id);
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

    return result;
}

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
    if (getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    semcheck_add_builtins(symtab);
    if (getenv("KGPC_DEBUG_TIMINGS") != NULL) {
        double t1 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
        fprintf(stderr, "[timing] semcheck_add_builtins: %.2f ms\n", t1 - t0);
    }
    /*PrintSymTab(symtab, stderr, 0);*/

    if (getenv("KGPC_DEBUG_TIMINGS") != NULL)
        t0 = (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
    if (parse_tree->type == TREE_UNIT) {
        return_val = semcheck_unit(symtab, parse_tree);
    } else {
        return_val = semcheck_program(symtab, parse_tree);
    }
    if (getenv("KGPC_DEBUG_TIMINGS") != NULL) {
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

/* Pushes a bunch of type declarations onto the current scope */
/* Semantic check for a program */
#define SEMCHECK_TIMINGS_ENABLED() (getenv("KGPC_DEBUG_TIMINGS") != NULL)

double semcheck_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

void semcheck_timing_step(const char *label, double *last_ms) {
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

    /* Predeclare subprograms so they can be referenced in const initializers */
    return_val += predeclare_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("predeclare subprograms", &t0);
    if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
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
                return_val += semcheck_stmt(symtab, final_stmt, INT_MAX);
            }
            final_node = final_node->next;
        }
    }
    semcheck_timing_step("finalization", &t0);

    if(optimize_flag() > 0 && return_val == 0)
    {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
    const char *debug_steps = getenv("KGPC_DEBUG_SEMSTEPS");
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_name_add(tree->tree_data.unit_data.unit_id);
    if (tree->tree_data.unit_data.unit_id != NULL)
        g_semcheck_current_unit_name = strdup(tree->tree_data.unit_data.unit_id);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.interface_uses);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.implementation_uses);

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
    if (getenv("KGPC_DEBUG_SUBPROGRAMS_LIST") != NULL)
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
    /* Pass 1: Imported unit untyped constants. */
    before = return_val;
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.unit_data.interface_const_decls);
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
    /* Pass 1: Imported unit untyped constants from implementation section. */
    before = return_val;
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.unit_data.implementation_const_decls);
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
