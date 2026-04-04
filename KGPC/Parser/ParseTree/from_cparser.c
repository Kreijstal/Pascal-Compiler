#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include <limits.h>
#include <errno.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
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

#include "from_cparser.h"
#include "../../string_intern.h"
#include "../../unit_registry.h"
#include "../../compilation_context.h"
#include "../pascal_frontend.h"

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);

#include "../List/List.h"
#include "tree.h"
#include "tree_types.h"
#include "ident_ref.h"
#include "type_tags.h"
#include "pascal_parser.h"
#include "KgpcType.h"
#include "generic_types.h"
#include "../SemanticCheck/SymTab/SymTab.h"
#include "../../identifier_utils.h"
#include "../pascal_frontend.h"
#include "../ErrVars.h"
/* ============================================================================
 * Circular Reference Detection for AST Traversal
 * ============================================================================
 * The parser can create circular AST structures where node->next or node->child
 * point back to themselves or ancestors, causing infinite loops. This module
 * provides a visited set mechanism to detect and prevent such loops.
 */

/* Simple hash set implementation for tracking visited AST nodes */
#define VISITED_SET_INITIAL_CAPACITY 256
#define VISITED_SET_LOAD_FACTOR 0.75

typedef struct VisitedSetEntry {
    ast_t *node;
    struct VisitedSetEntry *next;
} VisitedSetEntry;

typedef struct {
    VisitedSetEntry **buckets;
    size_t capacity;
    size_t size;
} VisitedSet;

static int kgpc_debug_subprog_enabled(void)
{
    static int cached = -1;
    if (cached == -1)
        cached = (kgpc_getenv("KGPC_DEBUG_SUBPROG") != NULL) ? 1 : 0;
    return cached;
}

static int kgpc_debug_decl_scan_enabled(void)
{
    static int cached = -1;
    if (cached == -1)
        cached = (kgpc_getenv("KGPC_DEBUG_DECL_SCAN") != NULL) ? 1 : 0;
    return cached;
}

struct TypeHelperMapping
{
    char *helper_id;
    char *base_type_id;
};

static ListNode_t *type_helper_mappings = NULL;
static ast_t *g_interface_type_section_ast = NULL;
static ast_t *g_implementation_type_section_ast = NULL;
static ast_t *g_interface_section_ast = NULL;
static ast_t *g_implementation_section_ast = NULL;
/* Method context for expression conversion (e.g., bare "inherited" expressions). */
static const char *g_current_method_name = NULL;
/* When instantiating a generic method template, this points to the owning
 * RecordType so that type_name_is_class_like can answer without touching the
 * (possibly freed) raw parser AST globals. */
static struct RecordType *g_instantiate_record = NULL;

/* Global registry of enum type ranges across unit loads.
 * Persists across calls to tree_from_pascal_ast so that enum types
 * defined in already-loaded units can be resolved when parsing later units.
 * Uses a hash table for O(1) lookup. */
typedef struct EnumTypeEntry {
    char *name;
    int start;
    int end;
    struct EnumTypeEntry *next;
} EnumTypeEntry;

#define ENUM_HT_INITIAL_CAPACITY 64

static struct {
    EnumTypeEntry **buckets;
    size_t capacity;
    size_t size;
} g_enum_ht = {NULL, 0, 0};

/* Reuse the same FNV-1a case-insensitive hash */
static size_t enum_ht_hash(const char *name, size_t capacity) {
    size_t h = 2166136261u;
    for (const char *p = name; *p; p++) {
        unsigned char c = (unsigned char)*p;
        if (c >= 'A' && c <= 'Z') c += 32;
        h ^= c;
        h *= 16777619u;
    }
    return h & (capacity - 1);
}

static void enum_ht_init(void) {
    if (g_enum_ht.buckets != NULL) return;
    g_enum_ht.capacity = ENUM_HT_INITIAL_CAPACITY;
    g_enum_ht.buckets = (EnumTypeEntry **)calloc(g_enum_ht.capacity, sizeof(EnumTypeEntry *));
    g_enum_ht.size = 0;
}

static void enum_registry_add(const char *name, int start, int end) {
    if (name == NULL) return;
    enum_ht_init();
    size_t idx = enum_ht_hash(name, g_enum_ht.capacity);
    /* Check for existing entry (case-insensitive) */
    for (EnumTypeEntry *e = g_enum_ht.buckets[idx]; e != NULL; e = e->next) {
        if (strcasecmp(e->name, name) == 0) {
            e->start = start;
            e->end = end;
            return;
        }
    }
    EnumTypeEntry *entry = (EnumTypeEntry *)calloc(1, sizeof(EnumTypeEntry));
    if (entry == NULL) return;
    entry->name = strdup(name);
    entry->start = start;
    entry->end = end;
    entry->next = g_enum_ht.buckets[idx];
    g_enum_ht.buckets[idx] = entry;
    g_enum_ht.size++;
}

static int enum_registry_lookup(const char *name, int *out_start, int *out_end) {
    if (name == NULL || g_enum_ht.buckets == NULL) return -1;
    size_t idx = enum_ht_hash(name, g_enum_ht.capacity);
    for (EnumTypeEntry *e = g_enum_ht.buckets[idx]; e != NULL; e = e->next) {
        if (strcasecmp(e->name, name) == 0) {
            *out_start = e->start;
            *out_end = e->end;
            return 0;
        }
    }
    return -1;
}

static void enum_registry_free(void) {
    if (g_enum_ht.buckets == NULL) return;
    for (size_t i = 0; i < g_enum_ht.capacity; i++) {
        EnumTypeEntry *e = g_enum_ht.buckets[i];
        while (e != NULL) {
            EnumTypeEntry *next = e->next;
            free(e->name);
            free(e);
            e = next;
        }
    }
    free(g_enum_ht.buckets);
    g_enum_ht.buckets = NULL;
    g_enum_ht.capacity = 0;
    g_enum_ht.size = 0;
}

/* Forward declaration for const expression evaluator (defined later in the file). */
static int evaluate_const_int_expr(ast_t *expr, int *out_value, int depth);

/* Scan a PASCAL_T_TYPE_SECTION and register all enumerated and subrange types in the global registry. */
static void enum_registry_scan_type_section(ast_t *type_section) {
    if (type_section == NULL) return;
    for (ast_t *decl = type_section->child; decl != NULL; decl = decl->next) {
        if (decl->typ != PASCAL_T_TYPE_DECL) continue;
        ast_t *id_node = decl->child;
        if (id_node == NULL || id_node->typ != PASCAL_T_IDENTIFIER ||
            id_node->sym == NULL || id_node->sym->name == NULL)
            continue;
        /* Find the type spec */
        ast_t *spec = id_node->next;
        while (spec != NULL && spec->typ != PASCAL_T_TYPE_SPEC &&
               spec->typ != PASCAL_T_ENUMERATED_TYPE &&
               spec->typ != PASCAL_T_RANGE_TYPE)
            spec = spec->next;
        if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
            spec = spec->child;
        if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
            int count = 0;
            for (ast_t *lit = spec->child; lit != NULL; lit = lit->next) {
                if (lit->typ == PASCAL_T_IDENTIFIER) count++;
            }
            if (count > 0)
                enum_registry_add(id_node->sym->name, 0, count - 1);
        } else if (spec != NULL && spec->typ == PASCAL_T_RANGE_TYPE) {
            ast_t *lower = spec->child;
            ast_t *upper = (lower != NULL) ? lower->next : NULL;
            int low_val = 0, high_val = 0;
            if (evaluate_const_int_expr(lower, &low_val, 0) == 0 &&
                evaluate_const_int_expr(upper, &high_val, 0) == 0) {
                enum_registry_add(id_node->sym->name, low_val, high_val);
            }
        }
    }
}

static int is_external_directive(const char *directive)
{
    if (directive == NULL)
        return 0;
    return (strcasecmp(directive, "external") == 0 ||
            strcasecmp(directive, "weakexternal") == 0);
}

static void register_type_helper_mapping(const char *helper_id, const char *base_type_id)
{
    if (helper_id == NULL || base_type_id == NULL)
        return;

    ListNode_t *cur = type_helper_mappings;
    while (cur != NULL) {
        struct TypeHelperMapping *entry = (struct TypeHelperMapping *)cur->cur;
        if (entry != NULL && entry->helper_id != NULL &&
            strcasecmp(entry->helper_id, helper_id) == 0)
        {
            return;
        }
        cur = cur->next;
    }

    struct TypeHelperMapping *entry = (struct TypeHelperMapping *)calloc(1, sizeof(struct TypeHelperMapping));
    if (entry == NULL)
        return;
    entry->helper_id = strdup(helper_id);
    entry->base_type_id = strdup(base_type_id);
    if (entry->helper_id == NULL || entry->base_type_id == NULL)
    {
        free(entry->helper_id);
        free(entry->base_type_id);
        free(entry);
        return;
    }
    ListNode_t *node = CreateListNode(entry, LIST_UNSPECIFIED);
    if (node == NULL)
    {
        free(entry->helper_id);
        free(entry->base_type_id);
        free(entry);
        return;
    }
    node->next = type_helper_mappings;
    type_helper_mappings = node;
}

static const char *lookup_type_helper_base(const char *helper_id)
{
    if (helper_id == NULL)
        return NULL;
    ListNode_t *cur = type_helper_mappings;
    while (cur != NULL) {
        struct TypeHelperMapping *entry = (struct TypeHelperMapping *)cur->cur;
        if (entry != NULL && entry->helper_id != NULL &&
            strcasecmp(entry->helper_id, helper_id) == 0)
        {
            return entry->base_type_id;
        }
        cur = cur->next;
    }
    return NULL;
}

static const char *ast_symbol_name(ast_t *node)
{
    if (node == NULL)
        return NULL;
    if (node->sym != NULL && node->sym->name != NULL)
        return node->sym->name;
    return NULL;
}

static VisitedSet *visited_set_create(void) {
    VisitedSet *set = (VisitedSet *)malloc(sizeof(VisitedSet));
    if (set == NULL) return NULL;
    
    set->capacity = VISITED_SET_INITIAL_CAPACITY;
    set->size = 0;
    set->buckets = (VisitedSetEntry **)calloc(set->capacity, sizeof(VisitedSetEntry *));
    if (set->buckets == NULL) {
        free(set);
        return NULL;
    }
    
    return set;
}

static void visited_set_destroy(VisitedSet *set) {
    if (set == NULL) return;
    
    for (size_t i = 0; i < set->capacity; i++) {
        VisitedSetEntry *entry = set->buckets[i];
        while (entry != NULL) {
            VisitedSetEntry *next = entry->next;
            free(entry);
            entry = next;
        }
    }
    
    free(set->buckets);
    free(set);
}

static size_t visited_set_hash(ast_t *node, size_t capacity) {
    /* Simple pointer hash */
    uintptr_t addr = (uintptr_t)node;
    return (size_t)(addr % capacity);
}

static bool visited_set_contains(VisitedSet *set, ast_t *node) {
    if (set == NULL || node == NULL) return false;
    
    size_t index = visited_set_hash(node, set->capacity);
    VisitedSetEntry *entry = set->buckets[index];
    
    while (entry != NULL) {
        if (entry->node == node) {
            return true;
        }
        entry = entry->next;
    }
    
    return false;
}

static void visited_set_rehash(VisitedSet *set) {
    size_t new_capacity = set->capacity * 2;
    VisitedSetEntry **new_buckets = (VisitedSetEntry **)calloc(new_capacity, sizeof(VisitedSetEntry *));
    if (new_buckets == NULL) return; /* rehash failed, continue with old table */

    for (size_t i = 0; i < set->capacity; i++) {
        VisitedSetEntry *entry = set->buckets[i];
        while (entry != NULL) {
            VisitedSetEntry *next = entry->next;
            size_t new_index = (size_t)((uintptr_t)entry->node % new_capacity);
            entry->next = new_buckets[new_index];
            new_buckets[new_index] = entry;
            entry = next;
        }
    }

    free(set->buckets);
    set->buckets = new_buckets;
    set->capacity = new_capacity;
}

static bool visited_set_add(VisitedSet *set, ast_t *node) {
    if (set == NULL || node == NULL) return false;

    /* Check if already present */
    if (visited_set_contains(set, node)) {
        return true; /* Already visited - indicates circular reference */
    }

    /* Rehash if load factor exceeded */
    if (set->size >= (size_t)(set->capacity * VISITED_SET_LOAD_FACTOR)) {
        visited_set_rehash(set);
    }

    /* Add new entry */
    size_t index = visited_set_hash(node, set->capacity);
    VisitedSetEntry *new_entry = (VisitedSetEntry *)malloc(sizeof(VisitedSetEntry));
    if (new_entry == NULL) return false;

    new_entry->node = node;
    new_entry->next = set->buckets[index];
    set->buckets[index] = new_entry;
    set->size++;

    return false; /* Not a duplicate */
}

/* Macro for safe AST iteration with circular reference detection.
 * Usage: SAFE_AST_ITER(visited_set, node, { ... body ... })
 * The macro will break the loop if a circular reference is detected.
 */
#define SAFE_AST_ITER(visited_set, node_var) \
    for (ast_t *node_var = NULL, *__iter_tmp = node_var; \
         ((node_var = __iter_tmp) != NULL || (__iter_tmp = node_var) != NULL) && \
         (!visited_set_add(visited_set, node_var)); \
         __iter_tmp = node_var->next)

/* Helper to check if continuing iteration is safe */
static inline bool is_safe_to_continue(VisitedSet *visited, ast_t *node) {
    if (node == NULL) return false;
    if (node == ast_nil) return false;
    
    /* Check if we've visited this node before (circular reference) */
    if (visited_set_contains(visited, node)) {
        return false;
    }
    
    /* Mark as visited */
    visited_set_add(visited, node);
    return true;
}

typedef struct {
    int is_array;
    int is_array_of_const;
    int start;
    int end;
    int element_type;
    char *element_type_id;
    TypeRef *element_type_ref;
    struct KgpcType *element_kgpc_type; /* For nested array elements (array of array of ...) */
    int is_shortstring;
    int is_open_array;
    ListNode_t *array_dimensions;
    char *array_dim_start_str;  /* Symbolic lower bound */
    char *array_dim_end_str;    /* Symbolic upper bound */
    int array_dims_parsed;
    int is_pointer;
    int pointer_type;
    char *pointer_type_id;
    TypeRef *pointer_type_ref;
    int is_set;
    int set_element_type;
    char *set_element_type_id;
    TypeRef *set_element_type_ref;
    int is_enum_set;           /* Set with inline anonymous enum as element type */
    ListNode_t *inline_enum_values; /* Enum values for inline enum in set type */
    int is_enum;
    int enum_is_scoped;
    int enum_has_explicit_values;
    ListNode_t *enum_literals;
    int is_file;
    int file_type;
    char *file_type_id;
    TypeRef *file_type_ref;
    int is_record;
    struct RecordType *record_type;
    int is_generic_specialization;
    char *generic_base_name;
    ListNode_t *generic_type_args;
    TypeRef *type_ref;
    int is_range;
    int range_known;
    long long range_start;
    long long range_end;
    char *range_start_str;  /* Symbolic lower bound for range aliases */
    char *range_end_str;    /* Symbolic upper bound for range aliases */
    int is_class_reference;  /* For "class of T" types */
    char *unresolved_index_type;  /* Deferred enum index type name */
} TypeInfo;

/* Frontend error counter for errors during AST to tree conversion */
static int g_frontend_error_count = 0;
static char *g_scoped_enum_source_path = NULL;
static char *g_scoped_enum_source_buffer = NULL;
static size_t g_scoped_enum_source_length = 0;

static ast_t *unwrap_pascal_node(ast_t *node);
static char *dup_symbol(ast_t *node);
static char *extract_external_name_from_node(ast_t *node);

static int split_absolute_target(const char *absolute_target,
    char **out_base, char **out_field)
{
    if (out_base != NULL)
        *out_base = NULL;
    if (out_field != NULL)
        *out_field = NULL;
    if (absolute_target == NULL || absolute_target[0] == '\0')
        return 0;

    const char *dot = strchr(absolute_target, '.');
    if (dot == NULL)
    {
        if (out_base != NULL)
            *out_base = strdup(absolute_target);
        return 1;
    }

    if (dot == absolute_target || dot[1] == '\0')
        return 0;

    if (out_base != NULL)
    {
        size_t base_len = (size_t)(dot - absolute_target);
        *out_base = (char *)malloc(base_len + 1);
        if (*out_base != NULL)
        {
            memcpy(*out_base, absolute_target, base_len);
            (*out_base)[base_len] = '\0';
        }
    }
    if (out_field != NULL)
        *out_field = strdup(dot + 1);
    return 1;
}

static char *qualified_ident_join_prefix(const QualifiedIdent *qid, int count)
{
    if (qid == NULL || qid->segments == NULL || count <= 0 || count > qid->count)
        return NULL;
    size_t total = 0;
    for (int i = 0; i < count; ++i)
    {
        if (qid->segments[i] != NULL)
            total += strlen(qid->segments[i]);
        if (i + 1 < count)
            total += 1;
    }
    char *out = (char *)malloc(total + 1);
    if (out == NULL)
        return NULL;
    out[0] = '\0';
    for (int i = 0; i < count; ++i)
    {
        if (qid->segments[i] != NULL)
            strcat(out, qid->segments[i]);
        if (i + 1 < count)
            strcat(out, ".");
    }
    return out;
}

static TypeRef *type_ref_from_single_name(const char *name)
{
    if (name == NULL)
        return NULL;
    QualifiedIdent *qid = NULL;
    if (strchr(name, '.') != NULL)
        qid = qualified_ident_from_dotted(name);
    else
        qid = qualified_ident_from_single(name);
    if (qid == NULL)
        return NULL;
    return type_ref_create(qid, NULL, 0);
}

static TypeRef *type_ref_from_name_and_args(const char *base_name, ListNode_t *type_args)
{
    if (base_name == NULL)
        return NULL;
    QualifiedIdent *qid = NULL;
    if (strchr(base_name, '.') != NULL)
        qid = qualified_ident_from_dotted(base_name);
    else
        qid = qualified_ident_from_single(base_name);
    if (qid == NULL)
        return NULL;
    int arg_count = ListLength(type_args);
    TypeRef **args = NULL;
    if (arg_count > 0)
    {
        args = (TypeRef **)calloc((size_t)arg_count, sizeof(TypeRef *));
        if (args == NULL)
        {
            qualified_ident_free(qid);
            return NULL;
        }
        int idx = 0;
        for (ListNode_t *cur = type_args; cur != NULL && idx < arg_count; cur = cur->next)
        {
            const char *arg_name = (const char *)cur->cur;
            args[idx++] = type_ref_from_single_name(arg_name);
        }
    }
    return type_ref_create(qid, args, arg_count);
}

static int append_qualified_segment(char ***segments, int *count, int *cap, const char *segment)
{
    if (segment == NULL)
        return 0;
    if (*count + 1 > *cap)
    {
        int new_cap = (*cap == 0) ? 4 : (*cap * 2);
        char **next = (char **)realloc(*segments, (size_t)new_cap * sizeof(char *));
        if (next == NULL)
            return 0;
        *segments = next;
        *cap = new_cap;
    }
    (*segments)[*count] = strdup(segment);
    if ((*segments)[*count] == NULL)
        return 0;
    (*count)++;
    return 1;
}

static int append_qualified_segments_from_dotted(char ***segments, int *count, int *cap,
    const char *name)
{
    if (name == NULL)
        return 0;
    const char *start = name;
    const char *cursor = name;
    int ok = 1;
    while (*cursor != '\0')
    {
        if (*cursor == '.')
        {
            if (cursor > start)
            {
                size_t len = (size_t)(cursor - start);
                char *segment = (char *)malloc(len + 1);
                if (segment == NULL)
                    return 0;
                memcpy(segment, start, len);
                segment[len] = '\0';
                ok = append_qualified_segment(segments, count, cap, segment);
                free(segment);
                if (!ok)
                    return 0;
            }
            start = cursor + 1;
        }
        ++cursor;
    }
    if (cursor > start)
    {
        size_t len = (size_t)(cursor - start);
        char *segment = (char *)malloc(len + 1);
        if (segment == NULL)
            return 0;
        memcpy(segment, start, len);
        segment[len] = '\0';
        ok = append_qualified_segment(segments, count, cap, segment);
        free(segment);
        if (!ok)
            return 0;
    }
    return 1;
}

static QualifiedIdent *qualified_ident_from_ast(ast_t *node)
{
    ast_t *unwrapped = unwrap_pascal_node(node);
    if (unwrapped == NULL)
        return NULL;

    char **segments = NULL;
    int count = 0;
    int cap = 0;

    if (unwrapped->typ == PASCAL_T_IDENTIFIER || unwrapped->typ == PASCAL_T_BOOLEAN)
    {
        char *name = dup_symbol(unwrapped);
        if (name == NULL)
            return NULL;
        int ok = 1;
        if (strchr(name, '.') != NULL)
            ok = append_qualified_segments_from_dotted(&segments, &count, &cap, name);
        else
            ok = append_qualified_segment(&segments, &count, &cap, name);
        free(name);
        if (!ok)
            return NULL;
        return qualified_ident_from_segments(segments, count, 1);
    }

    if (unwrapped->typ == PASCAL_T_QUALIFIED_IDENTIFIER)
    {
        for (ast_t *cur = unwrapped->child; cur != NULL; cur = cur->next)
        {
            ast_t *item = unwrap_pascal_node(cur);
            if (item == NULL)
                item = cur;
            if (item != NULL &&
                (item->typ == PASCAL_T_IDENTIFIER || item->typ == PASCAL_T_BOOLEAN))
            {
                char *name = dup_symbol(item);
                if (name != NULL)
                {
                    if (!append_qualified_segment(&segments, &count, &cap, name))
                    {
                        free(name);
                        return NULL;
                    }
                    free(name);
                }
            }
        }
        if (count == 0)
            return NULL;
        return qualified_ident_from_segments(segments, count, 1);
    }

    if (unwrapped->typ == PASCAL_T_MEMBER_ACCESS)
    {
        ast_t *base = unwrapped->child;
        ast_t *field = (base != NULL) ? base->next : NULL;
        QualifiedIdent *base_id = qualified_ident_from_ast(base);
        if (base_id == NULL)
            return NULL;
        for (int i = 0; i < base_id->count; ++i)
        {
            if (!append_qualified_segment(&segments, &count, &cap, base_id->segments[i]))
            {
                qualified_ident_free(base_id);
                return NULL;
            }
        }
        qualified_ident_free(base_id);
        if (field != NULL)
        {
            ast_t *field_node = unwrap_pascal_node(field);
            if (field_node != NULL &&
                (field_node->typ == PASCAL_T_IDENTIFIER || field_node->typ == PASCAL_T_BOOLEAN))
            {
                char *name = dup_symbol(field_node);
                if (name != NULL)
                {
                    if (!append_qualified_segment(&segments, &count, &cap, name))
                    {
                        free(name);
                        return NULL;
                    }
                    free(name);
                }
            }
        }
        if (count == 0)
            return NULL;
        return qualified_ident_from_segments(segments, count, 1);
    }

    return NULL;
}

static TypeRef *type_ref_from_info_or_id(const TypeInfo *info, const char *type_id)
{
    if (info != NULL && info->type_ref != NULL)
        return type_ref_clone(info->type_ref);
    if (type_id != NULL)
        return type_ref_from_single_name(type_id);
    return NULL;
}

static TypeRef *type_ref_from_element_info(const TypeInfo *info, const char *type_id)
{
    if (info != NULL && info->element_type_ref != NULL)
        return type_ref_clone(info->element_type_ref);
    if (type_id != NULL)
        return type_ref_from_single_name(type_id);
    return NULL;
}

static TypeRef *type_ref_from_pointer_info(const TypeInfo *info, const char *type_id)
{
    if (info != NULL && info->pointer_type_ref != NULL)
        return type_ref_clone(info->pointer_type_ref);
    if (type_id != NULL)
        return type_ref_from_single_name(type_id);
    return NULL;
}

static TypeRef *type_ref_from_qualifier_and_id(const char *qualifier, const char *id)
{
    if (id == NULL)
        return NULL;
    if (qualifier == NULL)
        return type_ref_from_single_name(id);
    char **segments = (char **)calloc(2, sizeof(char *));
    if (segments == NULL)
        return NULL;
    segments[0] = strdup(qualifier);
    segments[1] = strdup(id);
    if (segments[0] == NULL || segments[1] == NULL)
    {
        free(segments[0]);
        free(segments[1]);
        free(segments);
        return NULL;
    }
    QualifiedIdent *qid = qualified_ident_from_segments(segments, 2, 1);
    if (qid == NULL)
        return NULL;
    return type_ref_create(qid, NULL, 0);
}

static void from_cparser_trim_ascii(char *s)
{
    if (s == NULL)
        return;
    char *start = s;
    while (*start == ' ' || *start == '\t' || *start == '\r' || *start == '\n')
        ++start;
    if (start != s)
        memmove(s, start, strlen(start) + 1);
    size_t len = strlen(s);
    while (len > 0)
    {
        char c = s[len - 1];
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
            break;
        s[len - 1] = '\0';
        --len;
    }
}

static int parse_guid_literal(const char *guid, uint32_t *d1, uint16_t *d2, uint16_t *d3, uint8_t d4[8])
{
    if (d1 != NULL)
        *d1 = 0;
    if (d2 != NULL)
        *d2 = 0;
    if (d3 != NULL)
        *d3 = 0;
    if (d4 != NULL)
        memset(d4, 0, 8);

    if (guid == NULL)
        return 0;

    const char *p = guid;
    if (*p == '\'' || *p == '"')
        p++;
    if (*p == '{')
        p++;

    char *end = NULL;
    unsigned long v = strtoul(p, &end, 16);
    if (end == p || *end != '-')
        return 0;
    if (d1 != NULL)
        *d1 = (uint32_t)v;
    p = end + 1;

    v = strtoul(p, &end, 16);
    if (end == p || *end != '-')
        return 0;
    if (d2 != NULL)
        *d2 = (uint16_t)v;
    p = end + 1;

    v = strtoul(p, &end, 16);
    if (end == p || *end != '-')
        return 0;
    if (d3 != NULL)
        *d3 = (uint16_t)v;
    p = end + 1;

    v = strtoul(p, &end, 16);
    if (end == p || *end != '-')
        return 0;
    if (d4 != NULL) {
        d4[0] = (uint8_t)((v >> 8) & 0xFF);
        d4[1] = (uint8_t)(v & 0xFF);
    }
    p = end + 1;

    for (int i = 2; i < 8; i++) {
        if (p[0] == '\0' || p[1] == '\0')
            return 0;
        if (!isxdigit((unsigned char)p[0]) || !isxdigit((unsigned char)p[1]))
            return 0;
        char hx[3];
        hx[0] = p[0];
        hx[1] = p[1];
        hx[2] = '\0';
        if (d4 != NULL)
            d4[i] = (uint8_t)strtoul(hx, NULL, 16);
        p += 2;
    }

    return 1;
}

/* Determine SCOPEDENUMS state at a parser line by scanning compiler directives
 * in the preprocessed source up to that logical line. */
static int from_cparser_scopedenums_enabled_at_line(int target_line)
{
    if (target_line <= 0)
        return 0;

    const char *buffer = NULL;
    size_t length = 0;

    if (file_to_parse != NULL && file_to_parse[0] != '\0')
    {
        if (g_scoped_enum_source_path == NULL ||
            strcmp(g_scoped_enum_source_path, file_to_parse) != 0)
        {
            FILE *fp = fopen(file_to_parse, "rb");
            if (fp != NULL)
            {
                if (fseek(fp, 0, SEEK_END) == 0)
                {
                    long size = ftell(fp);
                    if (size >= 0 && fseek(fp, 0, SEEK_SET) == 0)
                    {
                        char *new_buf = (char *)malloc((size_t)size + 1);
                        if (new_buf != NULL)
                        {
                            size_t read_len = fread(new_buf, 1, (size_t)size, fp);
                            new_buf[read_len] = '\0';

                            if (g_scoped_enum_source_path != NULL)
                                free(g_scoped_enum_source_path);
                            if (g_scoped_enum_source_buffer != NULL)
                                free(g_scoped_enum_source_buffer);

                            g_scoped_enum_source_path = strdup(file_to_parse);
                            g_scoped_enum_source_buffer = new_buf;
                            g_scoped_enum_source_length = read_len;
                        }
                    }
                }
                fclose(fp);
            }
        }

        if (g_scoped_enum_source_path != NULL &&
            strcmp(g_scoped_enum_source_path, file_to_parse) == 0 &&
            g_scoped_enum_source_buffer != NULL &&
            g_scoped_enum_source_length > 0)
        {
            buffer = g_scoped_enum_source_buffer;
            length = g_scoped_enum_source_length;
        }
    }

    if (buffer == NULL || length == 0)
    {
        buffer = preprocessed_source;
        length = preprocessed_length;
    }
    if (buffer == NULL || length == 0)
        return 0;

    int scoped = 0;
    int scoped_stack[64];
    int scoped_sp = 0;
    int current_line = 1;
    size_t pos = 0;

    while (pos < length && current_line <= target_line)
    {
        size_t line_start = pos;
        while (pos < length && buffer[pos] != '\n')
            ++pos;
        size_t line_len = pos - line_start;

        if (line_len >= 4 && buffer[line_start] == '{' && buffer[line_start + 1] == '$')
        {
            size_t inner_start = line_start + 2;
            size_t inner_end = line_start + line_len;
            if (inner_end > inner_start && buffer[inner_end - 1] == '}')
                --inner_end;

            size_t inner_len = (inner_end > inner_start) ? (inner_end - inner_start) : 0;
            if (inner_len > 0 && inner_len < 256)
            {
                char directive[256];
                memcpy(directive, buffer + inner_start, inner_len);
                directive[inner_len] = '\0';
                from_cparser_trim_ascii(directive);

                if (strcasecmp(directive, "PUSH") == 0)
                {
                    if (scoped_sp < (int)(sizeof(scoped_stack) / sizeof(scoped_stack[0])))
                        scoped_stack[scoped_sp++] = scoped;
                }
                else if (strcasecmp(directive, "POP") == 0)
                {
                    if (scoped_sp > 0)
                        scoped = scoped_stack[--scoped_sp];
                }
                else if (strncasecmp(directive, "SCOPEDENUMS", 11) == 0)
                {
                    const char *arg = directive + 11;
                    while (*arg == ' ' || *arg == '\t')
                        ++arg;
                    if (strcasecmp(arg, "ON") == 0 || strcmp(arg, "+") == 0)
                        scoped = 1;
                    else if (strcasecmp(arg, "OFF") == 0 || strcmp(arg, "-") == 0)
                        scoped = 0;
                }
            }
        }

        if (pos < length && buffer[pos] == '\n')
        {
            ++pos;
            ++current_line;
        }
    }

    return scoped;
}

/* Reset the frontend error counter */
void from_cparser_reset_error_count(void) {
    g_frontend_error_count = 0;
}

/* Get the frontend error count */
int from_cparser_get_error_count(void) {
    return g_frontend_error_count;
}

/* Report a frontend error and increment the counter */
static void frontend_error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    g_frontend_error_count++;
}

typedef struct PendingGenericAlias {
    Tree_t *decl;
    char *base_name;
    ListNode_t *type_args;
    struct PendingGenericAlias *next;
} PendingGenericAlias;

static PendingGenericAlias *g_pending_generic_aliases = NULL;
static int g_allow_pending_specializations = 0;

/* Deferred inline specializations: when `specialize Foo<T>` appears in
 * expression context (e.g. `specialize Foo<T>.Method(...)`), we need to
 * instantiate the generic record and create a type declaration so that
 * append_generic_method_clones() can emit method implementations. */
typedef struct DeferredInlineSpec {
    Tree_t *type_decl;
    struct DeferredInlineSpec *next;
} DeferredInlineSpec;
static DeferredInlineSpec *g_deferred_inline_specs = NULL;
static ListNode_t *g_const_sections = NULL;
static ListNode_t *g_const_sections_tail = NULL; /* tail pointer for O(1) append */

/* ---- Hash table for const int cache (replaces linear-search linked list) ---- */
#define CONST_INT_HT_INITIAL_CAPACITY 256
#define CONST_INT_HT_LOAD_FACTOR_NUM 3   /* numerator */
#define CONST_INT_HT_LOAD_FACTOR_DEN 4   /* denominator: grow when size > 3/4 capacity */

typedef struct ConstIntEntry {
    char *name;
    int value;
    struct ConstIntEntry *next;  /* chaining for collisions */
} ConstIntEntry;

typedef struct {
    ConstIntEntry **buckets;
    size_t capacity;
    size_t size;
} ConstIntHashTable;

static ConstIntHashTable g_const_int_ht = {NULL, 0, 0};

static size_t const_int_ht_hash(const char *name, size_t capacity) {
    /* FNV-1a case-insensitive hash */
    size_t h = 2166136261u;
    for (const char *p = name; *p; p++) {
        unsigned char c = (unsigned char)*p;
        if (c >= 'A' && c <= 'Z') c += 32;
        h ^= c;
        h *= 16777619u;
    }
    return h & (capacity - 1);  /* capacity is always a power of 2 */
}

static void const_int_ht_init(void) {
    if (g_const_int_ht.buckets != NULL) return;
    g_const_int_ht.capacity = CONST_INT_HT_INITIAL_CAPACITY;
    g_const_int_ht.buckets = (ConstIntEntry **)calloc(g_const_int_ht.capacity, sizeof(ConstIntEntry *));
    g_const_int_ht.size = 0;
}

static void const_int_ht_grow(void) {
    size_t new_cap = g_const_int_ht.capacity * 2;
    ConstIntEntry **new_buckets = (ConstIntEntry **)calloc(new_cap, sizeof(ConstIntEntry *));
    if (new_buckets == NULL) return;
    for (size_t i = 0; i < g_const_int_ht.capacity; i++) {
        ConstIntEntry *e = g_const_int_ht.buckets[i];
        while (e != NULL) {
            ConstIntEntry *next = e->next;
            size_t idx = const_int_ht_hash(e->name, new_cap);
            e->next = new_buckets[idx];
            new_buckets[idx] = e;
            e = next;
        }
    }
    free(g_const_int_ht.buckets);
    g_const_int_ht.buckets = new_buckets;
    g_const_int_ht.capacity = new_cap;
}

static void const_int_ht_destroy(void) {
    if (g_const_int_ht.buckets == NULL) return;
    for (size_t i = 0; i < g_const_int_ht.capacity; i++) {
        ConstIntEntry *e = g_const_int_ht.buckets[i];
        while (e != NULL) {
            ConstIntEntry *next = e->next;
            free(e->name);
            free(e);
            e = next;
        }
    }
    free(g_const_int_ht.buckets);
    g_const_int_ht.buckets = NULL;
    g_const_int_ht.capacity = 0;
    g_const_int_ht.size = 0;
}

/* ---- Const-decl AST index: name → value AST node ----
 * Built when const sections are registered, enables O(1) lookup of const
 * declaration value nodes instead of recursive AST walking. */
#define CONST_DECL_INDEX_CAPACITY 512

typedef struct ConstDeclIndexEntry {
    const char *name;        /* NOT owned - points into AST sym->name */
    ast_t *value_node;       /* the value expression AST node */
    ast_t *const_section;    /* which section it came from */
    struct ConstDeclIndexEntry *next;
} ConstDeclIndexEntry;

static struct {
    ConstDeclIndexEntry **buckets;
    size_t capacity;
    size_t size;
} g_const_decl_index = {NULL, 0, 0};

static void const_decl_index_init(void) {
    if (g_const_decl_index.buckets != NULL) return;
    g_const_decl_index.capacity = CONST_DECL_INDEX_CAPACITY;
    g_const_decl_index.buckets = (ConstDeclIndexEntry **)calloc(
        g_const_decl_index.capacity, sizeof(ConstDeclIndexEntry *));
    g_const_decl_index.size = 0;
}

static void const_decl_index_destroy(void) {
    if (g_const_decl_index.buckets == NULL) return;
    for (size_t i = 0; i < g_const_decl_index.capacity; i++) {
        ConstDeclIndexEntry *e = g_const_decl_index.buckets[i];
        while (e != NULL) {
            ConstDeclIndexEntry *next = e->next;
            free(e);
            e = next;
        }
    }
    free(g_const_decl_index.buckets);
    g_const_decl_index.buckets = NULL;
    g_const_decl_index.capacity = 0;
    g_const_decl_index.size = 0;
}

/* Insert a const declaration into the index.  Does NOT overwrite existing entries
 * (first definition wins, matching Pascal semantics). */
static void const_decl_index_insert(const char *name, ast_t *value_node, ast_t *section) {
    if (name == NULL || g_const_decl_index.buckets == NULL) return;
    size_t idx = const_int_ht_hash(name, g_const_decl_index.capacity);
    /* Check if already present */
    for (ConstDeclIndexEntry *e = g_const_decl_index.buckets[idx]; e != NULL; e = e->next) {
        if (strcasecmp(e->name, name) == 0)
            return; /* first definition wins */
    }
    ConstDeclIndexEntry *e = (ConstDeclIndexEntry *)malloc(sizeof(ConstDeclIndexEntry));
    if (e == NULL) return;
    e->name = name;
    e->value_node = value_node;
    e->const_section = section;
    e->next = g_const_decl_index.buckets[idx];
    g_const_decl_index.buckets[idx] = e;
    g_const_decl_index.size++;
}

/* Lookup a const declaration by name. Returns the value AST node or NULL. */
static ast_t *const_decl_index_lookup(const char *name) {
    if (name == NULL || g_const_decl_index.buckets == NULL) return NULL;
    size_t idx = const_int_ht_hash(name, g_const_decl_index.capacity);
    for (ConstDeclIndexEntry *e = g_const_decl_index.buckets[idx]; e != NULL; e = e->next) {
        if (strcasecmp(e->name, name) == 0)
            return e->value_node;
    }
    return NULL;
}

/* Scan a const section and index all CONST_DECL entries.
 * Walks the AST iteratively (siblings only) to find CONST_DECL nodes. */
static void const_decl_index_scan_section(ast_t *section) {
    if (section == NULL) return;
    const_decl_index_init();
    for (ast_t *node = section->child; node != NULL; node = node->next) {
        if (node->typ == PASCAL_T_CONST_DECL) {
            ast_t *id_node = node->child;
            if (id_node != NULL && id_node->sym != NULL && id_node->sym->name != NULL) {
                /* Find the value node (skip optional type spec) */
                ast_t *value_node = id_node->next;
                if (value_node != NULL && value_node->typ == PASCAL_T_TYPE_SPEC)
                    value_node = value_node->next;
                const_decl_index_insert(id_node->sym->name, value_node, section);
            }
        }
    }
}

static void destroy_type_info_contents(TypeInfo *info) {
    if (info == NULL)
        return;

    if (info->type_ref != NULL) {
        type_ref_free(info->type_ref);
        info->type_ref = NULL;
    }
    if (info->element_type_id != NULL) {
        free(info->element_type_id);
        info->element_type_id = NULL;
    }
    if (info->element_type_ref != NULL) {
        type_ref_free(info->element_type_ref);
        info->element_type_ref = NULL;
    }
    if (info->pointer_type_id != NULL) {
        free(info->pointer_type_id);
        info->pointer_type_id = NULL;
    }
    if (info->pointer_type_ref != NULL) {
        type_ref_free(info->pointer_type_ref);
        info->pointer_type_ref = NULL;
    }
    if (info->set_element_type_id != NULL) {
        free(info->set_element_type_id);
        info->set_element_type_id = NULL;
    }
    if (info->set_element_type_ref != NULL) {
        type_ref_free(info->set_element_type_ref);
        info->set_element_type_ref = NULL;
    }
    if (info->inline_enum_values != NULL) {
        destroy_list(info->inline_enum_values);
        info->inline_enum_values = NULL;
    }
    if (info->file_type_id != NULL) {
        free(info->file_type_id);
        info->file_type_id = NULL;
    }
    if (info->file_type_ref != NULL) {
        type_ref_free(info->file_type_ref);
        info->file_type_ref = NULL;
    }
    if (info->generic_base_name != NULL) {
        free(info->generic_base_name);
        info->generic_base_name = NULL;
    }
    if (info->generic_type_args != NULL) {
        destroy_list(info->generic_type_args);
        info->generic_type_args = NULL;
    }
    if (info->enum_literals != NULL) {
        destroy_list(info->enum_literals);
        info->enum_literals = NULL;
    }
    if (info->array_dimensions != NULL) {
        destroy_list(info->array_dimensions);
        info->array_dimensions = NULL;
    }
    if (info->array_dim_start_str != NULL) {
        free(info->array_dim_start_str);
        info->array_dim_start_str = NULL;
    }
    if (info->array_dim_end_str != NULL) {
        free(info->array_dim_end_str);
        info->array_dim_end_str = NULL;
    }
    if (info->range_start_str != NULL) {
        free(info->range_start_str);
        info->range_start_str = NULL;
    }
    if (info->range_end_str != NULL) {
        free(info->range_end_str);
        info->range_end_str = NULL;
    }
    if (info->record_type != NULL) {
        destroy_record_type(info->record_type);
        info->record_type = NULL;
    }
    if (info->type_ref != NULL) {
        type_ref_free(info->type_ref);
        info->type_ref = NULL;
    }
    if (info->element_kgpc_type != NULL) {
        destroy_kgpc_type(info->element_kgpc_type);
        info->element_kgpc_type = NULL;
    }
    if (info->unresolved_index_type != NULL) {
        free(info->unresolved_index_type);
        info->unresolved_index_type = NULL;
    }
}

static void reset_const_sections(void) {
    if (g_const_sections != NULL) {
        DestroyList(g_const_sections);
        g_const_sections = NULL;
        g_const_sections_tail = NULL;
    }
    /* Keep const_int_ht alive across unit conversions so that constants
       defined in one unit (e.g. NR_ES in cpubase) remain available when
       parsing array bounds in a later unit (e.g. aasmcpu). The hash table
       stores only {name, int} pairs — no AST pointers — so it is safe to
       retain. */
    const_decl_index_destroy();
}

static void register_const_section(ast_t *const_section) {
    if (const_section == NULL)
        return;
    for (ListNode_t *cur = g_const_sections; cur != NULL; cur = cur->next) {
        if (cur->cur == const_section)
            return;
    }
    ListNode_t *node = CreateListNode(const_section, LIST_UNSPECIFIED);
    if (g_const_sections == NULL) {
        g_const_sections = node;
        g_const_sections_tail = node;
    } else {
        g_const_sections_tail->next = node;
        g_const_sections_tail = node;
    }
    /* Index const declarations for O(1) lookup */
    const_decl_index_scan_section(const_section);
}

static int const_section_is_resourcestring(ast_t *const_section) {
    if (const_section == NULL)
        return 0;

    if (const_section->sym != NULL && const_section->sym->name != NULL &&
        strcasecmp(const_section->sym->name, "resourcestring") == 0)
        return 1;

    ast_t *cur = const_section->child;
    if (cur != NULL && cur->sym != NULL && cur->sym->name != NULL &&
        strcasecmp(cur->sym->name, "resourcestring") == 0)
        return 1;

    if (cur != NULL && cur->typ == PASCAL_T_NONE && cur->child != NULL)
    {
        ast_t *inner = cur->child;
        if (inner->sym != NULL && inner->sym->name != NULL &&
            strcasecmp(inner->sym->name, "resourcestring") == 0)
            return 1;
    }

    return 0;
}

static int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result);

/* Parse a range bound string that may be a number or boolean literal.
 * Used for multi-dimensional array range extraction. */
static int parse_range_bound(const char *s) {
    if (s == NULL) return 0;
    if (strcasecmp(s, "true") == 0) return 1;
    if (strcasecmp(s, "false") == 0) return 0;
    return atoi(s);
}

static int select_range_primitive_tag(const TypeInfo *info);

static int resolve_const_expr_from_sections(const char *expr, int *result)
{
    if (expr == NULL || result == NULL)
        return -1;

    if (g_const_sections != NULL)
    {
        for (ListNode_t *cur = g_const_sections; cur != NULL; cur = cur->next)
        {
            ast_t *section = (ast_t *)cur->cur;
            if (evaluate_simple_const_expr(expr, section, result) == 0)
                return 0;
        }
    }

    char *endptr = NULL;
    long num = strtol(expr, &endptr, 10);
    if (endptr != NULL && *endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
    {
        *result = (int)num;
        return 0;
    }
    return -1;
}

static int lookup_const_int(const char *name, int *out_value) {
    if (name == NULL || out_value == NULL)
        return -1;
    if (g_const_int_ht.buckets == NULL)
        return -1;
    size_t idx = const_int_ht_hash(name, g_const_int_ht.capacity);
    for (ConstIntEntry *cur = g_const_int_ht.buckets[idx]; cur != NULL; cur = cur->next) {
        if (strcasecmp(cur->name, name) == 0) {
            *out_value = cur->value;
            return 0;
        }
    }
    return -1;
}

static void register_const_int(const char *name, int value) {
    if (name == NULL)
        return;
    const_int_ht_init();
    size_t idx = const_int_ht_hash(name, g_const_int_ht.capacity);
    /* Check for existing entry */
    for (ConstIntEntry *cur = g_const_int_ht.buckets[idx]; cur != NULL; cur = cur->next) {
        if (strcasecmp(cur->name, name) == 0) {
            cur->value = value;
            return;
        }
    }
    /* Insert new entry */
    ConstIntEntry *entry = (ConstIntEntry *)malloc(sizeof(ConstIntEntry));
    if (entry == NULL)
        return;
    entry->name = strdup(name);
    if (entry->name == NULL) {
        free(entry);
        return;
    }
    entry->value = value;
    entry->next = g_const_int_ht.buckets[idx];
    g_const_int_ht.buckets[idx] = entry;
    g_const_int_ht.size++;
    /* Grow if load factor exceeded */
    if (g_const_int_ht.size * CONST_INT_HT_LOAD_FACTOR_DEN >
        g_const_int_ht.capacity * CONST_INT_HT_LOAD_FACTOR_NUM)
        const_int_ht_grow();
}

static void register_pending_generic_alias(Tree_t *decl, TypeInfo *type_info) {
    if (!g_allow_pending_specializations || decl == NULL || type_info == NULL)
        return;
    if (type_info->generic_base_name == NULL || type_info->generic_type_args == NULL)
        return;

    PendingGenericAlias *entry = (PendingGenericAlias *)calloc(1, sizeof(PendingGenericAlias));
    if (entry == NULL)
        return;

    entry->decl = decl;
    entry->base_name = type_info->generic_base_name;
    entry->type_args = type_info->generic_type_args;
    entry->next = g_pending_generic_aliases;
    g_pending_generic_aliases = entry;
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL &&
        decl->tree_data.type_decl_data.id != NULL &&
        entry->base_name != NULL)
    {
        fprintf(stderr, "[KGPC] deferred generic alias %s base %s\n",
                decl->tree_data.type_decl_data.id, entry->base_name);
    }

    type_info->generic_base_name = NULL;
    type_info->generic_type_args = NULL;
}

void from_cparser_enable_pending_specializations(void) {
    g_allow_pending_specializations = 1;
}

void from_cparser_disable_pending_specializations(void) {
    g_allow_pending_specializations = 0;
}

static ast_t *unwrap_pascal_node(ast_t *node);
static struct Expression *convert_expression(ast_t *expr_node);

/* Global offset added to all source_index values during conversion.
 * Set via from_cparser_set_source_offset() before converting each unit's AST,
 * so that source_index values are globally unique across all source buffers. */
static int g_source_offset = 0;

void from_cparser_set_source_offset(int offset)
{
    g_source_offset = offset;
}

/* Helper to copy source index from AST node to Expression for accurate error context.
 * node->index == -1 means the parser position is unknown; preserve that sentinel
 * rather than producing a bogus global offset (-1 + g_source_offset). */
static inline struct Expression *set_expr_source_index(struct Expression *expr, ast_t *node) {
    if (expr != NULL && node != NULL && node->index >= 0) {
        expr->source_index = node->index + g_source_offset;
    }
    return expr;
}
static int convert_type_spec(ast_t *type_spec, char **type_id_out,
                             struct RecordType **record_out, TypeInfo *type_info);
static int extract_constant_int(struct Expression *expr, long long *out_value);
static struct Expression *convert_set_literal(ast_t *set_node);
static char *pop_last_identifier(ListNode_t **ids);
static int resolve_const_int_from_ast(const char *identifier, ast_t *const_section, int fallback_value);
static int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result);
static int resolve_enum_ordinal_from_ast(const char *identifier, ast_t *type_section);
static int resolve_enum_type_range_from_ast(const char *type_name, ast_t *type_section, int *out_start, int *out_end);
static int resolve_enum_type_range_with_fallback(const char *type_name, ast_t *type_section,
                                                 int *out_start, int *out_end);
static int resolve_enum_literal_in_type(const char *type_name, const char *literal, ast_t *type_section);
static ast_t *find_type_decl_in_section(ast_t *type_section, const char *type_name);
static int resolve_array_type_info_from_ast(const char *type_name, ast_t *type_section, TypeInfo *out_info, int depth);
static void resolve_array_bounds(TypeInfo *info, ast_t *type_section, ast_t *const_section, const char *id_for_error);
static ast_t *find_node_by_type(ast_t *node, int target_type);
static void substitute_generic_identifiers(ast_t *node, char **params, char **args, int count);


/* ClassMethodBinding typedef moved to from_cparser.h */

static ListNode_t *class_method_bindings = NULL;

/* ---- Class-method hash index (keyed by interned class_name pointer) ----
 *
 * The flat class_method_bindings list is O(n) per lookup.  With hundreds
 * of RTL classes this makes is_method_static / is_method_virtual / … the
 * dominant cost during compilation.  The hash index maps an interned
 * class_name to a small chain of ClassMethodBinding pointers for that
 * class, giving O(1) class lookup + O(k) method scan within the class.
 *
 * Because class_name strings are interned (string_intern.h), we can use
 * pointer identity for hashing and comparison.
 */
#define CMB_INDEX_SIZE 512

typedef struct CMBIndexEntry {
    const char            *class_name;  /* interned pointer – identity key */
    ClassMethodBinding   **bindings;    /* dynamic array of binding pointers */
    int                    count;
    int                    capacity;
    struct CMBIndexEntry  *next;        /* hash chain */
} CMBIndexEntry;

static CMBIndexEntry *cmb_index[CMB_INDEX_SIZE];

static unsigned int cmb_ptr_hash(const char *ptr)
{
    uintptr_t v = (uintptr_t)ptr;
    v ^= v >> 16;
    v *= 0x45d9f3bu;  /* murmurhash3 finalizer constant */
    v ^= v >> 16;
    return (unsigned int)(v % CMB_INDEX_SIZE);
}

static CMBIndexEntry *cmb_index_find(const char *interned_class)
{
    unsigned int h = cmb_ptr_hash(interned_class);
    for (CMBIndexEntry *e = cmb_index[h]; e != NULL; e = e->next)
        if (e->class_name == interned_class)
            return e;
    return NULL;
}

static CMBIndexEntry *cmb_index_get_or_create(const char *interned_class)
{
    unsigned int h = cmb_ptr_hash(interned_class);
    for (CMBIndexEntry *e = cmb_index[h]; e != NULL; e = e->next)
        if (e->class_name == interned_class)
            return e;

    CMBIndexEntry *e = (CMBIndexEntry *)malloc(sizeof(CMBIndexEntry));
    assert(e != NULL);
    e->class_name = interned_class;
    e->count    = 0;
    e->capacity = 4;
    e->bindings = (ClassMethodBinding **)malloc(sizeof(ClassMethodBinding *) * (size_t)e->capacity);
    assert(e->bindings != NULL);
    e->next = cmb_index[h];
    cmb_index[h] = e;
    return e;
}

static void cmb_index_add(const char *interned_class, ClassMethodBinding *binding)
{
    CMBIndexEntry *e = cmb_index_get_or_create(interned_class);
    if (e->count == e->capacity) {
        e->capacity *= 2;
        e->bindings = (ClassMethodBinding **)realloc(e->bindings,
                        sizeof(ClassMethodBinding *) * (size_t)e->capacity);
        assert(e->bindings != NULL);
    }
    e->bindings[e->count++] = binding;
}

static void cmb_index_reset(void)
{
    for (int i = 0; i < CMB_INDEX_SIZE; i++) {
        CMBIndexEntry *e = cmb_index[i];
        while (e != NULL) {
            CMBIndexEntry *next = e->next;
            free(e->bindings);
            free(e);
            e = next;
        }
        cmb_index[i] = NULL;
    }
}

/* ---- Method-name index (keyed by interned method_name pointer) ----
 *
 * Used by find_class_for_method and from_cparser_find_classes_with_method
 * which look up by method_name across all classes.
 */
#define CMB_METHOD_INDEX_SIZE 512

typedef struct CMBMethodEntry {
    const char            *method_name;  /* interned pointer – identity key */
    ClassMethodBinding   **bindings;
    int                    count;
    int                    capacity;
    struct CMBMethodEntry *next;
} CMBMethodEntry;

static CMBMethodEntry *cmb_method_index[CMB_METHOD_INDEX_SIZE];

static CMBMethodEntry *cmb_method_index_find(const char *interned_method)
{
    unsigned int h = cmb_ptr_hash(interned_method);
    h = h % CMB_METHOD_INDEX_SIZE;
    for (CMBMethodEntry *e = cmb_method_index[h]; e != NULL; e = e->next)
        if (e->method_name == interned_method)
            return e;
    return NULL;
}

static CMBMethodEntry *cmb_method_index_get_or_create(const char *interned_method)
{
    unsigned int h = cmb_ptr_hash(interned_method);
    h = h % CMB_METHOD_INDEX_SIZE;
    for (CMBMethodEntry *e = cmb_method_index[h]; e != NULL; e = e->next)
        if (e->method_name == interned_method)
            return e;

    CMBMethodEntry *e = (CMBMethodEntry *)malloc(sizeof(CMBMethodEntry));
    assert(e != NULL);
    e->method_name = interned_method;
    e->count    = 0;
    e->capacity = 4;
    e->bindings = (ClassMethodBinding **)malloc(sizeof(ClassMethodBinding *) * (size_t)e->capacity);
    assert(e->bindings != NULL);
    e->next = cmb_method_index[h];
    cmb_method_index[h] = e;
    return e;
}

static void cmb_method_index_add(const char *interned_method, ClassMethodBinding *binding)
{
    CMBMethodEntry *e = cmb_method_index_get_or_create(interned_method);
    if (e->count == e->capacity) {
        e->capacity *= 2;
        e->bindings = (ClassMethodBinding **)realloc(e->bindings,
                        sizeof(ClassMethodBinding *) * (size_t)e->capacity);
        assert(e->bindings != NULL);
    }
    e->bindings[e->count++] = binding;
}

static void cmb_method_index_reset(void)
{
    for (int i = 0; i < CMB_METHOD_INDEX_SIZE; i++) {
        CMBMethodEntry *e = cmb_method_index[i];
        while (e != NULL) {
            CMBMethodEntry *next = e->next;
            free(e->bindings);
            free(e);
            e = next;
        }
        cmb_method_index[i] = NULL;
    }
}

/* Counter for generating unique anonymous method names */
static int anonymous_method_counter = 0;

#define ANON_METHOD_NAME_SIZE 64

/* Helper function to generate unique names for anonymous methods */
static char *generate_anonymous_method_name(int is_function) {
    char *name = (char *)malloc(ANON_METHOD_NAME_SIZE);
    if (name == NULL) return NULL;
    snprintf(name, ANON_METHOD_NAME_SIZE, "_anon_%s_%d", is_function ? "func" : "proc", ++anonymous_method_counter);
    return name;
}

static int is_param_modifier(const char *name) {
    if (name == NULL) return 0;
    return (strcasecmp(name, "const") == 0 ||
            strcasecmp(name, "var") == 0 ||
            strcasecmp(name, "out") == 0 ||
            strcasecmp(name, "constref") == 0);
}

static int count_param_names_in_param(ast_t *param) {
    if (param == NULL || param->typ != PASCAL_T_PARAM)
        return 0;
    int count = 0;
    for (ast_t *c = param->child; c != NULL; c = c->next) {
        if (c->typ == PASCAL_T_TYPE_SPEC)
            break;
        if (c->typ != PASCAL_T_IDENTIFIER || c->sym == NULL || c->sym->name == NULL)
            continue;
        if (is_param_modifier(c->sym->name))
            continue;
        count++;
    }
    return count;
}

int from_cparser_count_params_ast(struct ast_t *params_ast) {
    if (params_ast == NULL)
        return 0;

    ast_t *param = params_ast;
    if (param->typ == PASCAL_T_PARAM_LIST)
        param = param->child;

    int count = 0;
    for (; param != NULL; param = param->next) {
        if (param->typ != PASCAL_T_PARAM)
            continue;
        count += count_param_names_in_param(param);
    }
    return count;
}

static ast_t *find_param_type_spec(ast_t *param) {
    if (param == NULL)
        return NULL;
    for (ast_t *c = param->child; c != NULL; c = c->next) {
        if (c->typ == PASCAL_T_TYPE_SPEC)
            return c;
    }
    return NULL;
}

static char *append_sig(char *sig, const char *type_str) {
    const char *part = (type_str != NULL) ? type_str : "<unknown>";
    size_t part_len = strlen(part);
    if (sig == NULL) {
        char *out = (char *)malloc(part_len + 1);
        if (out == NULL)
            return NULL;
        memcpy(out, part, part_len + 1);
        return out;
    }
    size_t sig_len = strlen(sig);
    char *out = (char *)realloc(sig, sig_len + 1 + part_len + 1);
    if (out == NULL) {
        free(sig);
        return NULL;
    }
    out[sig_len] = ',';
    memcpy(out + sig_len + 1, part, part_len + 1);
    return out;
}

static char *param_type_string_from_type_node(ast_t *type_node) {
    if (type_node == NULL)
        return strdup("<unknown>");

    char *type_id = NULL;
    TypeInfo type_info;
    memset(&type_info, 0, sizeof(TypeInfo));
    convert_type_spec(type_node, &type_id, NULL, &type_info);
    TypeRef *type_ref = type_ref_from_info_or_id(&type_info, type_id);
    char *rendered = NULL;
    if (type_ref != NULL)
        rendered = type_ref_render_mangled(type_ref);
    if (rendered == NULL && type_id != NULL)
        rendered = strdup(type_id);
    if (rendered == NULL)
        rendered = strdup("<unknown>");
    if (type_ref != NULL)
        type_ref_free(type_ref);
    destroy_type_info_contents(&type_info);
    if (type_id != NULL)
        free(type_id);
    return rendered;
}

static char *param_type_signature_from_params_ast(ast_t *params_ast) {
    if (params_ast == NULL)
        return NULL;
    ast_t *param = params_ast;
    if (param->typ == PASCAL_T_PARAM_LIST)
        param = param->child;

    char *sig = NULL;
    for (; param != NULL; param = param->next) {
        if (param->typ != PASCAL_T_PARAM)
            continue;
        int name_count = count_param_names_in_param(param);
        ast_t *type_node = find_param_type_spec(param);
        char *type_str = param_type_string_from_type_node(type_node);
        for (int i = 0; i < name_count; i++)
            sig = append_sig(sig, type_str);
        if (type_str != NULL)
            free(type_str);
    }
    return sig;
}

static int count_params_in_method_impl(ast_t *method_node) {
    if (method_node == NULL)
        return -1;
    int count = 0;
    for (ast_t *cur = method_node->child; cur != NULL; cur = cur->next) {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;
        if (node->typ == PASCAL_T_PARAM_LIST || node->typ == PASCAL_T_PARAM)
            count += from_cparser_count_params_ast(node);
    }
    return count;
}

static char *param_type_signature_from_method_impl(ast_t *method_node) {
    if (method_node == NULL)
        return NULL;
    char *sig = NULL;
    for (ast_t *cur = method_node->child; cur != NULL; cur = cur->next) {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;
        if (node->typ == PASCAL_T_PARAM_LIST || node->typ == PASCAL_T_PARAM) {
            char *chunk = param_type_signature_from_params_ast(node);
            if (chunk == NULL)
                continue;
            if (sig == NULL) {
                sig = chunk;
            } else {
                char *combined = append_sig(sig, chunk);
                free(chunk);
                sig = combined;
            }
        }
    }
    return sig;
}

static void register_class_method_ex(const char *class_name, const char *method_name,
                                      int is_virtual, int is_override, int is_static,
                                      int is_class_method,
                                      int param_count, char *param_sig) {
    if (class_name == NULL || method_name == NULL)
        return;

    ClassMethodBinding *binding = (ClassMethodBinding *)malloc(sizeof(ClassMethodBinding));
    if (binding == NULL)
        return;

    binding->class_name = (char *)string_intern(class_name);
    binding->method_name = (char *)string_intern(method_name);
    binding->is_virtual = is_virtual;
    binding->is_override = is_override;
    binding->is_static = is_static;
    binding->is_class_method = is_class_method;
    binding->param_count = param_count;
    binding->param_sig = param_sig;

    ListNode_t *node = NULL;
    if (binding->class_name != NULL && binding->method_name != NULL)
        node = CreateListNode(binding, LIST_UNSPECIFIED);

    if (node == NULL) {
        /* class_name and method_name are interned -- do not free */
        if (binding->param_sig != NULL)
            free(binding->param_sig);
        free(binding);
        return;
    }

    node->next = class_method_bindings;
    class_method_bindings = node;

    /* Maintain hash indices for O(1) lookup by class and method name. */
    cmb_index_add(binding->class_name, binding);
    cmb_method_index_add(binding->method_name, binding);

    if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL) {
        fprintf(stderr,
            "[KGPC] Registered method %s.%s (virtual=%d, override=%d, static=%d, class_method=%d, params=%d, sig=%s)\n",
            class_name, method_name, is_virtual, is_override, is_static, is_class_method,
            param_count, param_sig != NULL ? param_sig : "<null>");
    }
}

void from_cparser_register_method_template(const char *class_name, const char *method_name,
    int is_virtual, int is_override, int is_static, int param_count) {
    register_class_method_ex(class_name, method_name, is_virtual, is_override, is_static,
        0, param_count, NULL);
}



static const char *find_class_for_method(const char *method_name) {
    if (method_name == NULL)
        return NULL;

    const char *interned = string_intern(method_name);
    if (interned == NULL)
        return NULL;

    CMBMethodEntry *me = cmb_method_index_find(interned);
    if (me != NULL && me->count > 0)
        return me->bindings[0]->class_name;
    return NULL;
}

/* Check if a method is static (no Self parameter) */
static int is_method_static(const char *class_name, const char *method_name) {
    if (class_name == NULL || method_name == NULL)
        return 0;

    const char *cn = string_intern(class_name);
    const char *mn = string_intern(method_name);
    if (cn == NULL || mn == NULL)
        return 0;

    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry == NULL)
        return 0;

    int has_static = 0;
    int has_instance = 0;
    for (int i = 0; i < entry->count; i++) {
        ClassMethodBinding *binding = entry->bindings[i];
        if (binding->method_name == mn) {
            if (binding->is_static)
                has_static = 1;
            else
                has_instance = 1;
        }
    }
    if (has_instance)
        return 0;
    return has_static;
}

static int is_method_static_with_signature(const char *class_name, const char *method_name,
                                           int param_count, const char *param_sig) {
    if (class_name == NULL || method_name == NULL)
        return 0;
    if (param_sig == NULL && param_count < 0)
        return is_method_static(class_name, method_name);

    const char *cn = string_intern(class_name);
    const char *mn = string_intern(method_name);
    if (cn == NULL || mn == NULL)
        return 0;

    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry == NULL)
        return is_method_static(class_name, method_name);

    int has_static = 0;
    int has_instance = 0;
    int has_match = 0;
    for (int i = 0; i < entry->count; i++) {
        ClassMethodBinding *binding = entry->bindings[i];
        if (binding->method_name == mn) {
            int matches = 0;
            if (param_sig != NULL && binding->param_sig != NULL) {
                if (strcmp(binding->param_sig, param_sig) == 0)
                    matches = 1;
            } else if (param_count >= 0 && binding->param_count == param_count) {
                matches = 1;
            }
            if (matches) {
                has_match = 1;
                if (binding->is_static)
                    has_static = 1;
                else
                    has_instance = 1;
            }
        }
    }
    if (has_match) {
        if (has_instance)
            return 0;
        return has_static;
    }
    return is_method_static(class_name, method_name);
}

/* Public wrapper for is_method_static */
int from_cparser_is_method_static(const char *class_name, const char *method_name) {
    return is_method_static(class_name, method_name);
}

/* Check if a method is declared with 'class' keyword (Self = VMT pointer, not instance).
 * Returns 1 if class method, 0 otherwise. */
int from_cparser_is_method_class_method(const char *class_name, const char *method_name) {
    if (class_name == NULL || method_name == NULL)
        return 0;

    const char *cn = string_intern(class_name);
    const char *mn = string_intern(method_name);
    if (cn == NULL || mn == NULL)
        return 0;

    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry == NULL)
        return 0;

    for (int i = 0; i < entry->count; i++) {
        ClassMethodBinding *binding = entry->bindings[i];
        if (binding->method_name == mn && binding->is_class_method)
            return 1;
    }
    return 0;
}

/* Check if a method is a non-static class method (Self = VMT pointer).
 * Returns 1 only if ALL matching bindings with is_class_method=1 are also
 * non-static. Returns 0 if no class method bindings exist, or if there are
 * mixed overloads (some class, some instance) with conflicting staticness. */
int from_cparser_is_method_nonstatic_class_method(const char *class_name, const char *method_name) {
    if (class_name == NULL || method_name == NULL)
        return 0;

    const char *cn = string_intern(class_name);
    const char *mn = string_intern(method_name);
    if (cn == NULL || mn == NULL)
        return 0;

    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry == NULL)
        return 0;

    int found_nonstatic_class = 0;
    int found_instance = 0;
    for (int i = 0; i < entry->count; i++) {
        ClassMethodBinding *binding = entry->bindings[i];
        if (binding->method_name == mn) {
            if (binding->is_class_method && !binding->is_static)
                found_nonstatic_class = 1;
            else if (!binding->is_class_method)
                found_instance = 1;
        }
    }
    /* If there are mixed overloads (class + instance with same name),
     * don't mark as class method — overload resolution should pick correctly. */
    if (found_instance && found_nonstatic_class)
        return 0;
    return found_nonstatic_class;
}

int from_cparser_is_type_helper(const char *helper_id) {
    return lookup_type_helper_base(helper_id) != NULL;
}

int from_cparser_class_has_method_name(const char *class_name, const char *method_name)
{
    if (class_name == NULL || method_name == NULL)
        return 0;

    const char *cn = string_intern(class_name);
    const char *mn = string_intern(method_name);
    if (cn == NULL || mn == NULL)
        return 0;

    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry != NULL) {
        for (int i = 0; i < entry->count; i++) {
            ClassMethodBinding *binding = entry->bindings[i];
            if (binding != NULL && binding->method_name == mn)
                return 1;
        }
    }

    const char *dot = strrchr(class_name, '.');
    if (dot == NULL)
        return 0;
    const char *unqualified = string_intern(dot + 1);
    if (unqualified == NULL)
        return 0;

    entry = cmb_index_find(unqualified);
    if (entry == NULL)
        return 0;

    for (int i = 0; i < entry->count; i++) {
        ClassMethodBinding *binding = entry->bindings[i];
        if (binding != NULL && binding->method_name == mn)
            return 1;
    }
    return 0;
}

/* Check if a method is virtual (needs VMT dispatch) */
int from_cparser_is_method_virtual(const char *class_name, const char *method_name) {
    if (class_name == NULL || method_name == NULL)
        return 0;

    const char *cn = string_intern(class_name);
    const char *mn = string_intern(method_name);
    if (cn == NULL || mn == NULL)
        return 0;

    /* Check ALL overloads — return 1 if ANY overload with this name is virtual.
     * Overloaded methods may have both virtual and non-virtual variants
     * (e.g. TEncoding.GetAnsiBytes has virtual abstract + non-virtual overloads). */
    /* First pass: search under exact class name. */
    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry != NULL) {
        for (int i = 0; i < entry->count; i++) {
            ClassMethodBinding *binding = entry->bindings[i];
            if (binding->method_name == mn &&
                (binding->is_virtual || binding->is_override))
                return 1;
        }
    }
    /* Second pass: for nested types (e.g., TMarshaller.TDeferBase), methods may have been
     * registered under the unqualified name before renaming. Only try if no exact match. */
    const char *dot = strrchr(class_name, '.');
    if (dot == NULL)
        return 0;
    const char *unqualified = string_intern(dot + 1);
    if (unqualified == NULL)
        return 0;
    entry = cmb_index_find(unqualified);
    if (entry != NULL) {
        for (int i = 0; i < entry->count; i++) {
            ClassMethodBinding *binding = entry->bindings[i];
            if (binding->method_name == mn &&
                (binding->is_virtual || binding->is_override))
                return 1;
        }
    }
    return 0;
}

int from_cparser_is_method_virtual_with_signature(const char *class_name, const char *method_name,
    int param_count, const char *param_sig)
{
    if (class_name == NULL || method_name == NULL)
        return 0;

    if (param_sig == NULL && param_count < 0)
        return from_cparser_is_method_virtual(class_name, method_name);

    const char *mn = string_intern(method_name);
    if (mn == NULL)
        return 0;

    int has_match = 0;
    int has_virtual = 0;

    /* Helper: check bindings for a given interned class name. */
    #define CHECK_INDEX_FOR(interned_cname) \
        do { \
            CMBIndexEntry *_entry = cmb_index_find(interned_cname); \
            if (_entry != NULL) { \
                for (int _i = 0; _i < _entry->count; _i++) { \
                    ClassMethodBinding *_b = _entry->bindings[_i]; \
                    if (_b->method_name == mn) { \
                        int _matches = 0; \
                        if (param_sig != NULL && _b->param_sig != NULL) { \
                            if (strcmp(_b->param_sig, param_sig) == 0) _matches = 1; \
                        } else if (param_count >= 0 && _b->param_count == param_count) { \
                            _matches = 1; \
                        } \
                        if (_matches) { \
                            has_match = 1; \
                            if (_b->is_virtual || _b->is_override) has_virtual = 1; \
                        } \
                    } \
                } \
            } \
        } while (0)

    /* First pass: exact class name. */
    const char *cn = string_intern(class_name);
    if (cn != NULL)
        CHECK_INDEX_FOR(cn);
    /* Second pass: unqualified fallback — only if nothing found under exact name. */
    if (!has_match) {
        const char *dot = strrchr(class_name, '.');
        if (dot != NULL) {
            const char *uq = string_intern(dot + 1);
            if (uq != NULL)
                CHECK_INDEX_FOR(uq);
        }
    }
    #undef CHECK_INDEX_FOR
    if (has_match)
        return has_virtual;
    return 0;
}

/* Find all class names that have a method with the given name */
ListNode_t *from_cparser_find_classes_with_method(const char *method_name, int *count_out) {
    if (count_out != NULL) *count_out = 0;
    if (method_name == NULL) return NULL;

    const char *mn = string_intern(method_name);
    if (mn == NULL) return NULL;

    CMBMethodEntry *me = cmb_method_index_find(mn);
    if (me == NULL)
        return NULL;

    ListNode_t *result = NULL;
    int count = 0;

    for (int i = 0; i < me->count; i++) {
        ClassMethodBinding *binding = me->bindings[i];
        if (binding->class_name == NULL) continue;
        /* Check if we already have this class in the result (pointer comparison). */
        ListNode_t *check = result;
        int found = 0;
        while (check != NULL) {
            if (check->cur == binding->class_name) {
                found = 1;
                break;
            }
            check = check->next;
        }
        if (!found) {
            char *class_copy = strdup(binding->class_name);
            if (class_copy != NULL) {
                ListNode_t *node = CreateListNode(class_copy, LIST_UNSPECIFIED);
                if (node != NULL) {
                    node->next = result;
                    result = node;
                    count++;
                } else {
                    free(class_copy);
                }
            }
        }
    }

    if (count_out != NULL) *count_out = count;
    return result;
}

static int typed_const_counter = 0;
static const char *g_typed_const_unit_tag = "p"; /* set per unit in tree_from_pascal_ast */

/* Mark a TREE_VAR_DECL as having static storage (for local typed constants) */
static void mark_var_decl_static_storage(Tree_t *decl)
{
    if (decl == NULL || decl->type != TREE_VAR_DECL)
        return;
    decl->tree_data.var_decl_data.has_static_storage = 1;
    char label_buffer[128];
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_var_%s_%d", g_typed_const_unit_tag, typed_const_counter);
    decl->tree_data.var_decl_data.static_label = strdup(label_buffer);
    ++typed_const_counter;
}

static int is_operator_token_name(const char *name)
{
    if (name == NULL)
        return 0;
    return (strcmp(name, "+") == 0 || strcmp(name, "-") == 0 ||
            strcmp(name, "*") == 0 || strcmp(name, "/") == 0 ||
            strcmp(name, "=") == 0 || strcmp(name, "<>") == 0 ||
            strcmp(name, "<") == 0 || strcmp(name, ">") == 0 ||
            strcmp(name, "<=") == 0 || strcmp(name, ">=") == 0 ||
            strcmp(name, "**") == 0 || strcmp(name, ":=") == 0 ||
            strcasecmp(name, "div") == 0 || strcasecmp(name, "mod") == 0 ||
            strcasecmp(name, "and") == 0 || strcasecmp(name, "or") == 0 ||
            strcasecmp(name, "not") == 0 || strcasecmp(name, "xor") == 0 ||
            strcasecmp(name, "shl") == 0 || strcasecmp(name, "shr") == 0 ||
            strcasecmp(name, "in") == 0 || strcasecmp(name, "is") == 0 ||
            strcasecmp(name, "as") == 0 ||
            strcasecmp(name, "Implicit") == 0 || strcasecmp(name, "Explicit") == 0 ||
            strcasecmp(name, "Equal") == 0 || strcasecmp(name, "NotEqual") == 0 ||
            strcasecmp(name, "GreaterThan") == 0 ||
            strcasecmp(name, "GreaterThanOrEqual") == 0 ||
            strcasecmp(name, "LessThan") == 0 ||
            strcasecmp(name, "LessThanOrEqual") == 0);
}

/* Tag a function call expression as an operator dispatch if applicable. */
static void tag_operator_call(struct Expression *expr, int is_operator)
{
    if (expr != NULL && is_operator)
        expr->expr_data.function_call_data.is_operator_call = 1;
}

/* Encode operator symbols into valid identifier names for assembly */
static char *encode_operator_name(const char *op_name) {
    if (op_name == NULL)
        return NULL;
    
    /* Map common operator symbols to readable names */
    if (strcmp(op_name, "+") == 0) return strdup("op_add");
    if (strcmp(op_name, "-") == 0) return strdup("op_sub");
    if (strcmp(op_name, "*") == 0) return strdup("op_mul");
    if (strcmp(op_name, "/") == 0) return strdup("op_div");
    if (strcmp(op_name, "=") == 0) return strdup("op_eq");
    if (strcmp(op_name, "<>") == 0) return strdup("op_ne");
    if (strcmp(op_name, "<") == 0) return strdup("op_lt");
    if (strcmp(op_name, ">") == 0) return strdup("op_gt");
    if (strcmp(op_name, "<=") == 0) return strdup("op_le");
    if (strcmp(op_name, ">=") == 0) return strdup("op_ge");
    if (strcmp(op_name, "**") == 0) return strdup("op_pow");
    if (strcmp(op_name, "div") == 0 || strcmp(op_name, "DIV") == 0) return strdup("op_intdiv");
    if (strcmp(op_name, "mod") == 0 || strcmp(op_name, "MOD") == 0) return strdup("op_mod");
    if (strcmp(op_name, "and") == 0 || strcmp(op_name, "AND") == 0) return strdup("op_and");
    if (strcmp(op_name, "or") == 0 || strcmp(op_name, "OR") == 0) return strdup("op_or");
    if (strcmp(op_name, "not") == 0 || strcmp(op_name, "NOT") == 0) return strdup("op_not");
    if (strcmp(op_name, "xor") == 0 || strcmp(op_name, "XOR") == 0) return strdup("op_xor");
    if (strcmp(op_name, "shl") == 0 || strcmp(op_name, "SHL") == 0) return strdup("op_shl");
    if (strcmp(op_name, "shr") == 0 || strcmp(op_name, "SHR") == 0) return strdup("op_shr");
    if (strcmp(op_name, "in") == 0 || strcmp(op_name, "IN") == 0) return strdup("op_in");
    if (strcmp(op_name, "is") == 0 || strcmp(op_name, "IS") == 0) return strdup("op_is");
    if (strcmp(op_name, "as") == 0 || strcmp(op_name, "AS") == 0) return strdup("op_as");
    if (strcasecmp(op_name, "Equal") == 0) return strdup("op_eq");
    if (strcasecmp(op_name, "NotEqual") == 0) return strdup("op_ne");
    if (strcasecmp(op_name, "GreaterThan") == 0) return strdup("op_gt");
    if (strcasecmp(op_name, "GreaterThanOrEqual") == 0) return strdup("op_ge");
    if (strcasecmp(op_name, "LessThan") == 0) return strdup("op_lt");
    if (strcasecmp(op_name, "LessThanOrEqual") == 0) return strdup("op_le");
    if (strcasecmp(op_name, "Add") == 0) return strdup("op_add");
    if (strcasecmp(op_name, "Subtract") == 0) return strdup("op_sub");
    if (strcasecmp(op_name, "Multiply") == 0) return strdup("op_mul");
    if (strcasecmp(op_name, "Divide") == 0) return strdup("op_div");
    if (strcmp(op_name, ":=") == 0) return strdup("op_assign");
    /* FPC class operator Implicit is assignment-style conversion. */
    if (strcasecmp(op_name, "Implicit") == 0) return strdup("op_assign");
    
    /* For other operators or named operators, use the name as-is */
    return strdup(op_name);
}

static char *mangle_method_name(const char *class_name, const char *method_name) {
    if (method_name == NULL)
        return NULL;

    if (class_name == NULL || class_name[0] == '\0')
        return encode_operator_name(method_name);

    /* Encode operator names to valid identifiers */
    char *encoded_method = encode_operator_name(method_name);
    if (encoded_method == NULL)
        return NULL;

    size_t class_len = strlen(class_name);
    size_t method_len = strlen(encoded_method);
    size_t total = class_len + 2 + method_len + 1;
    char *result = (char *)malloc(total);
    if (result == NULL) {
        free(encoded_method);
        return NULL;
    }

    snprintf(result, total, "%s__%s", class_name, encoded_method);
    free(encoded_method);
    return result;
}

/* Method mangling without operator alias encoding.
 * Use this for ordinary method declarations/implementations so names like Add
 * are preserved as methods instead of being treated as operators. */
static char *mangle_method_name_raw(const char *class_name, const char *method_name) {
    if (method_name == NULL)
        return NULL;
    if (class_name == NULL || class_name[0] == '\0')
        return strdup(method_name);

    size_t class_len = strlen(class_name);
    size_t method_len = strlen(method_name);
    size_t total = class_len + 2 + method_len + 1;
    char *result = (char *)malloc(total);
    if (result == NULL)
        return NULL;
    snprintf(result, total, "%s__%s", class_name, method_name);
    return result;
}

static char *method_param_type_suffix(Tree_t *param_decl)
{
    if (param_decl == NULL)
        return NULL;

    if (param_decl->type == TREE_VAR_DECL)
    {
        if (param_decl->tree_data.var_decl_data.type_ref != NULL)
            return type_ref_render_mangled(param_decl->tree_data.var_decl_data.type_ref);
        if (param_decl->tree_data.var_decl_data.type_id != NULL)
            return strdup(param_decl->tree_data.var_decl_data.type_id);
    }
    else if (param_decl->type == TREE_ARR_DECL)
    {
        if (param_decl->tree_data.arr_decl_data.type_ref != NULL)
            return type_ref_render_mangled(param_decl->tree_data.arr_decl_data.type_ref);
        if (param_decl->tree_data.arr_decl_data.type_id != NULL)
            return strdup(param_decl->tree_data.arr_decl_data.type_id);
    }

    return NULL;
}

/* Get method information for a class */
void get_class_methods(const char *class_name, ListNode_t **methods_out, int *count_out) {
    if (methods_out != NULL)
        *methods_out = NULL;
    if (count_out != NULL)
        *count_out = 0;

    if (class_name == NULL || methods_out == NULL || count_out == NULL)
        return;

    const char *cn = string_intern(class_name);
    if (cn == NULL)
        return;

    CMBIndexEntry *entry = cmb_index_find(cn);
    if (entry == NULL)
        return;

    ListNode_t *head = NULL;
    ListNode_t **tail = &head;
    int count = 0;

    for (int i = 0; i < entry->count; i++) {
        ClassMethodBinding *binding = entry->bindings[i];
        ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
        if (node != NULL) {
            node->type = LIST_UNSPECIFIED;
            node->cur = binding;
            node->next = NULL;
            *tail = node;
            tail = &node->next;
            count++;
        }
    }

    *methods_out = head;
    *count_out = count;
}

static char *dup_symbol(ast_t *node) {
    while (node != NULL) {
        if (node->sym != NULL && node->sym->name != NULL)
            return strdup(node->sym->name);
        node = node->child;
    }
    return NULL;
}

typedef struct {
    ListNode_t *head;
    ListNode_t **tail_next;
} ListBuilder;

static void list_builder_init(ListBuilder *builder) {
    if (builder == NULL)
        return;
    builder->head = NULL;
    builder->tail_next = &builder->head;
}

static ListNode_t *list_builder_append(ListBuilder *builder, void *value, enum ListType type) {
    if (builder == NULL)
        return NULL;

    ListNode_t *node = CreateListNode(value, type);
    *builder->tail_next = node;
    builder->tail_next = &node->next;
    return node;
}

static ListNode_t *list_builder_finish(ListBuilder *builder) {
    if (builder == NULL)
        return NULL;
    return builder->head;
}

static void list_builder_extend(ListBuilder *builder, ListNode_t *nodes) {
    if (builder == NULL || nodes == NULL)
        return;

    *builder->tail_next = nodes;
    while (*builder->tail_next != NULL)
        builder->tail_next = &(*builder->tail_next)->next;
}

static char *dup_first_identifier_in_node(ast_t *node)
{
    if (node == NULL)
        return NULL;
    node = unwrap_pascal_node(node);
    if (node == NULL)
        return NULL;
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] inspect node typ=%d (%s) sym=%s\n",
            node->typ, pascal_tag_to_string(node->typ),
            (node->sym != NULL && node->sym->name != NULL) ? node->sym->name : "<null>");
    if ((node->typ == PASCAL_T_IDENTIFIER ||
         node->typ == PASCAL_T_TYPE_ARG) &&
        node->sym != NULL && node->sym->name != NULL)
        return dup_symbol(node);
    ast_t *child = node->child;
    while (child != NULL)
    {
        char *dup = dup_first_identifier_in_node(child);
        if (dup != NULL)
            return dup;
        child = child->next;
    }
    return NULL;
}

static ListNode_t *collect_constructed_type_args(ast_t *args_node) {
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] collect_constructed_type_args start node=%p typ=%d (%s)\n",
            (void *)args_node,
            args_node != NULL ? args_node->typ : -1,
            args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>");
    if (args_node == NULL)
        return NULL;

    ListBuilder builder;
    list_builder_init(&builder);

    ast_t *cursor = args_node;
    if (cursor->typ == PASCAL_T_TYPE_ARG_LIST)
        cursor = cursor->child;

    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;

        if (node != NULL &&
            (node->typ == PASCAL_T_TYPE_ARG ||
             node->typ == PASCAL_T_IDENTIFIER ||
             node->typ == PASCAL_T_QUALIFIED_IDENTIFIER))
        {
            char *dup = dup_first_identifier_in_node(node);
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
                fprintf(stderr, "[KGPC] collect args extracted=%s\n", dup != NULL ? dup : "<null>");
            if (dup != NULL)
            {
                ListNode_t *append_node = list_builder_append(&builder, dup, LIST_STRING);
                if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
                    fprintf(stderr, "[KGPC] appended type_arg node=%p type=%d\n",
                        (void *)append_node, append_node != NULL ? append_node->type : -1);
            }
        }

        cursor = cursor->next;
    }
    return list_builder_finish(&builder);
}

static int extract_constructed_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out) {
    if (base_name_out != NULL)
        *base_name_out = NULL;
    if (type_args_out != NULL)
        *type_args_out = NULL;

    if (spec_node == NULL)
        return 0;

    ast_t *node = spec_node;
    if (node->typ == PASCAL_T_TYPE_SPEC && node->child != NULL)
        node = node->child;
    node = unwrap_pascal_node(node);
    if (node == NULL || node->typ != PASCAL_T_CONSTRUCTED_TYPE)
        return 0;

    ast_t *name_node = node->child;
    while (name_node != NULL && name_node->typ == PASCAL_T_NONE)
        name_node = name_node->child;
    if (name_node == NULL)
        return 0;
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
    {
        fprintf(stderr, "[KGPC] constructed type base node typ=%d (%s) sym=%s next_typ=%d (%s)\n",
            name_node->typ,
            pascal_tag_to_string(name_node->typ),
            (name_node->sym != NULL && name_node->sym->name != NULL) ? name_node->sym->name : "<null>",
            name_node->next != NULL ? name_node->next->typ : -1,
            name_node->next != NULL ? pascal_tag_to_string(name_node->next->typ) : "<null>");
    }

    char *base_name = dup_symbol(name_node);
    if (base_name == NULL)
        return 0;

    ListNode_t *type_args = collect_constructed_type_args(name_node->next);
    if (type_args == NULL) {
        free(base_name);
        return 0;
    }

    if (base_name_out != NULL)
        *base_name_out = base_name;
    else
        free(base_name);

    if (type_args_out != NULL)
        *type_args_out = type_args;
    else
        destroy_list(type_args);

    return 1;
}

static char *mangle_specialized_type_name(const char *base_name, char **type_ids, int num_types) {
    if (base_name == NULL)
        return NULL;
    size_t length = strlen(base_name) + 1;
    for (int i = 0; i < num_types; ++i) {
        if (type_ids[i] != NULL)
            length += 1 + strlen(type_ids[i]);
    }
    char *result = (char *)malloc(length);
    if (result == NULL)
        return NULL;
    strcpy(result, base_name);
    for (int i = 0; i < num_types; ++i) {
        strcat(result, "$");
        if (type_ids[i] != NULL)
            strcat(result, type_ids[i]);
    }
    return result;
}

static char *mangle_specialized_name_from_list(const char *base_name, ListNode_t *type_args) {
    if (base_name == NULL)
        return NULL;
    int count = ListLength(type_args);
    if (count <= 0)
        return strdup(base_name);
    char **arg_ids = (char **)calloc((size_t)count, sizeof(char *));
    if (arg_ids == NULL)
        return NULL;
    int idx = 0;
    for (ListNode_t *cur = type_args; cur != NULL && idx < count; cur = cur->next, ++idx) {
        arg_ids[idx] = (char *)(cur->cur);
    }
    char *result = mangle_specialized_type_name(base_name, arg_ids, count);
    free(arg_ids);
    return result;
}

static void substitute_identifier(char **type_id, GenericTypeDecl *generic_decl, char **arg_types) {
    if (type_id == NULL || *type_id == NULL || generic_decl == NULL || arg_types == NULL)
        return;

    for (int i = 0; i < generic_decl->num_type_params; ++i) {
        if (generic_decl->type_parameters[i] != NULL &&
            strcasecmp(*type_id, generic_decl->type_parameters[i]) == 0) {
            char *replacement = NULL;
            if (arg_types[i] != NULL)
                replacement = strdup(arg_types[i]);
            free(*type_id);
            *type_id = replacement;
            return;
        }
    }
}

static void substitute_type_ref(TypeRef **type_ref, GenericTypeDecl *generic_decl, char **arg_types)
{
    if (type_ref == NULL || *type_ref == NULL || generic_decl == NULL || arg_types == NULL)
        return;

    TypeRef *ref = *type_ref;
    if (ref->generic_args != NULL && ref->num_generic_args > 0)
    {
        for (int i = 0; i < ref->num_generic_args; ++i)
            substitute_type_ref(&ref->generic_args[i], generic_decl, arg_types);
    }

    if (ref->name == NULL || ref->name->count != 1)
        return;

    const char *base_name = type_ref_base_name(ref);
    if (base_name == NULL)
        return;

    if (ref->num_generic_args > 0)
        return;

    for (int i = 0; i < generic_decl->num_type_params; ++i)
    {
        if (generic_decl->type_parameters[i] != NULL &&
            pascal_identifier_equals(base_name, generic_decl->type_parameters[i]))
        {
            TypeRef *replacement = NULL;
            if (arg_types[i] != NULL)
                replacement = type_ref_from_single_name(arg_types[i]);
            type_ref_free(ref);
            *type_ref = replacement;
            return;
        }
    }
}

static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types);

static void substitute_record_field(struct RecordField *field, GenericTypeDecl *generic_decl, char **arg_types) {
    if (field == NULL)
        return;
    
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && field->name != NULL)
    {
        fprintf(stderr, "[KGPC] substitute_record_field BEFORE: name=%s is_array=%d type_id=%s array_element_type_id=%s\n",
            field->name, field->is_array,
            field->type_id ? field->type_id : "<null>",
            field->array_element_type_id ? field->array_element_type_id : "<null>");
    }
    
    substitute_identifier(&field->type_id, generic_decl, arg_types);
    substitute_type_ref(&field->type_ref, generic_decl, arg_types);
    if (field->array_element_type_id != NULL)
        substitute_identifier(&field->array_element_type_id, generic_decl, arg_types);
    substitute_type_ref(&field->array_element_type_ref, generic_decl, arg_types);
    if (field->pointer_type_id != NULL)
        substitute_identifier(&field->pointer_type_id, generic_decl, arg_types);
    substitute_type_ref(&field->pointer_type_ref, generic_decl, arg_types);
    if (field->nested_record != NULL)
        substitute_record_type_parameters(field->nested_record, generic_decl, arg_types);
    
    if (debug_env != NULL && field->name != NULL)
    {
        fprintf(stderr, "[KGPC] substitute_record_field AFTER: name=%s is_array=%d type_id=%s array_element_type_id=%s\n",
            field->name, field->is_array,
            field->type_id ? field->type_id : "<null>",
            field->array_element_type_id ? field->array_element_type_id : "<null>");
    }
}

static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types) {
    if (record == NULL || generic_decl == NULL || arg_types == NULL)
        return;

    ListNode_t *field_node = record->fields;
    while (field_node != NULL) {
        if (field_node->type == LIST_RECORD_FIELD)
            substitute_record_field((struct RecordField *)field_node->cur, generic_decl, arg_types);
        field_node = field_node->next;
    }

    ListNode_t *prop_node = record->properties;
    while (prop_node != NULL) {
        if (prop_node->type == LIST_CLASS_PROPERTY) {
            struct ClassProperty *property = (struct ClassProperty *)prop_node->cur;
            substitute_identifier(&property->type_id, generic_decl, arg_types);
            substitute_type_ref(&property->type_ref, generic_decl, arg_types);
        }
        prop_node = prop_node->next;
    }
}

static struct RecordType *instantiate_generic_record(const char *base_name, ListNode_t *type_args, char **specialized_name_out) {
    if (specialized_name_out != NULL)
        *specialized_name_out = NULL;
    if (base_name == NULL)
        return NULL;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    GenericTypeDecl *generic = generic_registry_find_decl(base_name);
    if (generic == NULL || generic->record_template == NULL) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record missing decl for %s\n", base_name);
        return NULL;
    }

    int arg_count = ListLength(type_args);
    if (arg_count != generic->num_type_params || arg_count <= 0) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record wrong arg count for %s: got %d expected %d\n",
                    base_name, arg_count, generic->num_type_params);
        destroy_list(type_args);
        return NULL;
    }

    char **arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
    if (arg_types == NULL) {
        return NULL;
    }

    int idx = 0;
    ListNode_t *cur = type_args;
    while (cur != NULL && idx < arg_count) {
        if (cur->type == LIST_STRING && cur->cur != NULL) {
            arg_types[idx] = strdup((char *)cur->cur);
            if (arg_types[idx] == NULL)
                break;
            idx++;
        }
        cur = cur->next;
    }

    if (idx != arg_count) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record failed to copy args for %s\n", base_name);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return NULL;
    }

    char *specialized_name = mangle_specialized_type_name(base_name, arg_types, arg_count);
    if (specialized_name == NULL) {
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return NULL;
    }

    struct RecordType *record = clone_record_type(generic->record_template);
    if (record == NULL) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] instantiate_generic_record failed to clone template for %s\n", base_name);
        free(specialized_name);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return NULL;
    }

    if (record->type_id != NULL)
        free(record->type_id);
    record->type_id = strdup(specialized_name);
    record->generic_decl = generic;
    record->num_generic_args = arg_count;
    record->generic_args = arg_types;
    arg_types = NULL;

    substitute_record_type_parameters(record, generic, record->generic_args);

    /* Rewrite references to nested types within the generic class.
     * E.g. field FOnCompare with type_id="TCompareFunc" needs to become
     * "TFPGList$TMyRecord.TCompareFunc" for proper resolution. */
    if (generic->nested_type_decls != NULL) {
        /* Collect the short names of nested types (without the prefix) */
        size_t gen_prefix_len = strlen(base_name);
        ListNode_t *field_node = record->fields;
        while (field_node != NULL) {
            if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL) {
                struct RecordField *field = (struct RecordField *)field_node->cur;
                if (field->type_id != NULL) {
                    /* Check if this type_id matches a nested type name */
                    ListNode_t *nt = generic->nested_type_decls;
                    while (nt != NULL) {
                        if (nt->type == LIST_TREE && nt->cur != NULL) {
                            Tree_t *nt_tree = (Tree_t *)nt->cur;
                            if (nt_tree->type == TREE_TYPE_DECL && nt_tree->tree_data.type_decl_data.id != NULL) {
                                const char *nt_id = nt_tree->tree_data.type_decl_data.id;
                                /* nested type id is like "TFPGList.TCompareFunc" */
                                if (strncmp(nt_id, base_name, gen_prefix_len) == 0 &&
                                    nt_id[gen_prefix_len] == '.') {
                                    const char *short_name = nt_id + gen_prefix_len + 1;
                                    if (strcasecmp(field->type_id, short_name) == 0) {
                                        /* Rewrite to specialized: "TFPGList$TMyRecord.TCompareFunc" */
                                        size_t new_len = strlen(specialized_name) + 1 + strlen(short_name) + 1;
                                        char *new_id = (char *)malloc(new_len);
                                        if (new_id != NULL) {
                                            snprintf(new_id, new_len, "%s.%s", specialized_name, short_name);
                                            free(field->type_id);
                                            field->type_id = new_id;
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        nt = nt->next;
                    }
                }
            }
            field_node = field_node->next;
        }
    }

    generic_registry_add_specialization(base_name, record->generic_args, arg_count);

    if (specialized_name_out != NULL)
        *specialized_name_out = specialized_name;
    else
        free(specialized_name);

    if (debug_env != NULL && record->type_id != NULL)
        fprintf(stderr, "[KGPC] instantiated generic record %s\n", record->type_id);
    return record;
}

static int build_generic_arg_array(ListNode_t *type_args, char ***arg_types_out, int *arg_count_out)
{
    if (arg_types_out == NULL || arg_count_out == NULL)
        return 0;
    *arg_types_out = NULL;
    *arg_count_out = 0;

    int arg_count = 0;
    for (ListNode_t *cur = type_args; cur != NULL; cur = cur->next)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            arg_count++;
    }
    if (arg_count == 0)
        return 0;

    char **arg_types = (char **)calloc((size_t)arg_count, sizeof(char *));
    if (arg_types == NULL)
        return 0;

    int idx = 0;
    ListNode_t *cur = type_args;
    while (cur != NULL && idx < arg_count) {
        if (cur->type == LIST_STRING && cur->cur != NULL) {
            arg_types[idx] = strdup((char *)cur->cur);
            if (arg_types[idx] == NULL)
                break;
            idx++;
        }
        cur = cur->next;
    }

    if (idx != arg_count) {
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return 0;
    }

    *arg_types_out = arg_types;
    *arg_count_out = arg_count;
    return 1;
}

static int type_info_has_resolution(const TypeInfo *info)
{
    if (info == NULL)
        return 0;
    return (info->is_array || info->is_set || info->is_record || info->is_file ||
            info->is_enum || info->is_range || info->is_pointer ||
            info->is_class_reference || info->is_array_of_const);
}

static int resolve_generic_alias_type(const char *base_name, ListNode_t *type_args,
    char **type_id_out, TypeInfo *type_info, int *result_out)
{
    if (base_name == NULL || type_info == NULL)
        return 0;
    if (result_out != NULL)
        *result_out = UNKNOWN_TYPE;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    GenericTypeDecl *generic = generic_registry_find_decl(base_name);
    Tree_t *generic_decl_tree = generic != NULL ? generic->original_decl : NULL;
    ast_t *generic_ast = generic_decl_tree != NULL ?
        generic_decl_tree->tree_data.type_decl_data.info.generic.original_ast : NULL;
    if (generic == NULL || generic->record_template != NULL || generic_ast == NULL)
    {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] resolve_generic_alias_type skip base=%s generic=%p record_template=%p original_ast=%p\n",
                base_name, (void *)generic,
                generic != NULL ? (void *)generic->record_template : NULL,
                (void *)generic_ast);
        return 0;
    }

    char **arg_types = NULL;
    int arg_count = 0;
    if (!build_generic_arg_array(type_args, &arg_types, &arg_count))
        return 0;

    if (arg_count != generic->num_type_params) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] resolve_generic_alias_type arg mismatch base=%s got=%d expected=%d\n",
                base_name, arg_count, generic->num_type_params);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return 0;
    }

    ast_t *ast_copy = copy_ast(generic_ast);
    if (ast_copy == NULL) {
        if (debug_env != NULL)
            fprintf(stderr, "[KGPC] resolve_generic_alias_type copy failed base=%s\n", base_name);
        for (int i = 0; i < arg_count; ++i)
            free(arg_types[i]);
        free(arg_types);
        return 0;
    }

    substitute_generic_identifiers(ast_copy, generic->type_parameters, arg_types, arg_count);
    int result = convert_type_spec(ast_copy, type_id_out, NULL, type_info);
    if (debug_env != NULL)
        fprintf(stderr, "[KGPC] resolve_generic_alias_type resolved base=%s result=%d is_array=%d\n",
            base_name, result, type_info->is_array);
    free_ast(ast_copy);

    for (int i = 0; i < arg_count; ++i)
        free(arg_types[i]);
    free(arg_types);

    if (result_out != NULL)
        *result_out = result;

    if (result != UNKNOWN_TYPE || type_info_has_resolution(type_info) ||
        (type_id_out != NULL && *type_id_out != NULL))
        return 1;

    return 0;
}

static void record_generic_method_impl(const char *class_name, const char *method_name, ast_t *method_ast)
{
    if (class_name == NULL || method_name == NULL || method_ast == NULL)
        return;

    GenericTypeDecl *generic = generic_registry_find_decl(class_name);
    if (generic == NULL || generic->record_template == NULL) {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && class_name != NULL)
            fprintf(stderr, "[KGPC] record_generic_method_impl: no generic decl for %s\n", class_name);
        return;
    }

    ListNode_t *cur = generic->record_template->method_templates;
    while (cur != NULL)
    {
        if (cur->type == LIST_METHOD_TEMPLATE)
        {
            struct MethodTemplate *template = (struct MethodTemplate *)cur->cur;
            if (template != NULL && template->method_impl_ast == NULL &&
                strcasecmp(template->name, method_name) == 0)
            {
                template->method_impl_ast = copy_ast_detached(method_ast);
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL)
                    fprintf(stderr, "[KGPC] recorded method implementation for %s.%s\n", class_name, method_name);
                break;
            }
        }
        cur = cur->next;
    }
}

static void substitute_generic_identifier_nodes(ast_t *node, struct RecordType *record)
{
    if (node == NULL || record == NULL || record->generic_decl == NULL ||
        record->generic_args == NULL || record->num_generic_args <= 0)
        return;

    const char *generic_name = record->generic_decl->name;
    size_t gen_prefix_len = generic_name != NULL ? strlen(generic_name) : 0;

    ast_t *cursor = node;
    while (cursor != NULL)
    {
        if (cursor->sym != NULL && cursor->sym->name != NULL)
        {
            /* Substitute generic type parameters (e.g. T -> TMyRecord) */
            for (int i = 0; i < record->generic_decl->num_type_params && i < record->num_generic_args; ++i)
            {
                const char *param_name = record->generic_decl->type_parameters[i];
                const char *arg_name = record->generic_args[i];
                if (param_name != NULL && arg_name != NULL &&
                    strcasecmp(cursor->sym->name, param_name) == 0)
                {
                    free(cursor->sym->name);
                    cursor->sym->name = strdup(arg_name);
                    break;
                }
            }
            /* Substitute the generic class name itself with the specialized name
             * (e.g. TFPGList -> TFPGList$TMyRecord in method parameter types) */
            if (generic_name != NULL && record->type_id != NULL &&
                strcasecmp(cursor->sym->name, generic_name) == 0)
            {
                free(cursor->sym->name);
                cursor->sym->name = strdup(record->type_id);
            }
            /* Substitute nested type short names with their specialized full names.
             * E.g. TFPGListEnumeratorSpec -> TFPGList$TMyRecord.TFPGListEnumeratorSpec
             * inside method bodies of the specialized class. */
            if (record->generic_decl->nested_type_decls != NULL && record->type_id != NULL)
            {
                ListNode_t *nt = record->generic_decl->nested_type_decls;
                while (nt != NULL)
                {
                    if (nt->type == LIST_TREE && nt->cur != NULL)
                    {
                        Tree_t *nt_tree = (Tree_t *)nt->cur;
                        if (nt_tree->type == TREE_TYPE_DECL && nt_tree->tree_data.type_decl_data.id != NULL)
                        {
                            const char *nt_id = nt_tree->tree_data.type_decl_data.id;
                            /* nested type id is like "TFPGList.TFPGListEnumeratorSpec" */
                            if (gen_prefix_len > 0 &&
                                strncmp(nt_id, generic_name, gen_prefix_len) == 0 &&
                                nt_id[gen_prefix_len] == '.')
                            {
                                const char *short_name = nt_id + gen_prefix_len + 1;
                                if (strcasecmp(cursor->sym->name, short_name) == 0)
                                {
                                    /* Rewrite to "TFPGList$TMyRecord.TFPGListEnumeratorSpec" */
                                    size_t new_len = strlen(record->type_id) + 1 + strlen(short_name) + 1;
                                    char *new_name = (char *)malloc(new_len);
                                    if (new_name != NULL)
                                    {
                                        snprintf(new_name, new_len, "%s.%s", record->type_id, short_name);
                                        free(cursor->sym->name);
                                        cursor->sym->name = new_name;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    nt = nt->next;
                }
            }
        }
        if (cursor->child != NULL)
            substitute_generic_identifier_nodes(cursor->child, record);
        cursor = cursor->next;
    }
}

static void rewrite_method_impl_ast(ast_t *method_ast, struct RecordType *record)
{
    if (method_ast == NULL || record == NULL)
        return;

    ast_t *qualified = unwrap_pascal_node(method_ast->child);
    if (qualified != NULL && qualified->typ == PASCAL_T_QUALIFIED_IDENTIFIER)
    {
        ast_t *class_node = qualified->child;
        while (class_node != NULL && class_node->typ != PASCAL_T_IDENTIFIER)
            class_node = class_node->next;
        if (class_node != NULL && class_node->sym != NULL && record->type_id != NULL)
        {
            free(class_node->sym->name);
            class_node->sym->name = strdup(record->type_id);
        }
    }

    substitute_generic_identifier_nodes(method_ast, record);
}

static Tree_t *convert_method_impl(ast_t *method_node);

static Tree_t *convert_procedure(ast_t *proc_node);
static Tree_t *convert_function(ast_t *func_node);
static Tree_t *convert_method_impl(ast_t *method_node);

static Tree_t *instantiate_method_template(struct MethodTemplate *method_template, struct RecordType *record)
{
    if (method_template == NULL || method_template->method_impl_ast == NULL || record == NULL)
        return NULL;

    /* Register method bindings for the specialized class name so that
     * convert_method_impl can look up is_static / is_class_method correctly.
     * The original bindings are registered under the generic class name,
     * but the cloned method will have the specialized class name. */
    if (record->type_id != NULL && method_template->name != NULL)
    {
        int param_count = from_cparser_count_params_ast(method_template->params_ast);
        char *param_sig = param_type_signature_from_params_ast(method_template->params_ast);
        register_class_method_ex(record->type_id, method_template->name,
            method_template->is_virtual, method_template->is_override,
            method_template->is_static, method_template->is_class_method,
            param_count, param_sig);
    }

    ast_t *method_copy = copy_ast(method_template->method_impl_ast);
    if (method_copy == NULL)
        return NULL;

    rewrite_method_impl_ast(method_copy, record);

    /* Restore the source offset from when the template was parsed,
     * so source_index values map to the correct source buffer. */
    int saved_offset = g_source_offset;
    g_source_offset = method_template->source_offset;

    /* Set the current generic context so that generic_registry_is_type_param()
     * only checks the enclosing generic's type parameters, not all generics. */
    GenericTypeDecl *saved_context = generic_registry_current_context();
    generic_registry_set_context(record->generic_decl);

    /* Let type_name_is_class_like resolve via the already-converted record
     * instead of the raw parser AST (which may have been freed). */
    struct RecordType *saved_record = g_instantiate_record;
    g_instantiate_record = record;

    Tree_t *method_tree = convert_method_impl(method_copy);

    g_instantiate_record = saved_record;
    generic_registry_set_context(saved_context);

    g_source_offset = saved_offset;

    free_ast(method_copy);
    return method_tree;
}

static void append_subprogram_node(ListNode_t **dest, Tree_t *tree)
{
    if (dest == NULL || tree == NULL)
        return;

    ListNode_t *node = CreateListNode(tree, LIST_TREE);
    if (node == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;
    *tail = node;
}

static int subprogram_list_has_id(const ListNode_t *subprograms, const char *id)
{
    if (subprograms == NULL || id == NULL)
        return 0;

    const ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            const Tree_t *tree = (const Tree_t *)cur->cur;
            if (tree->type == TREE_SUBPROGRAM &&
                tree->tree_data.subprogram_data.id != NULL &&
                strcasecmp(tree->tree_data.subprogram_data.id, id) == 0)
            {
                return 1;
            }
        }
        cur = cur->next;
    }
    return 0;
}

static int parse_generic_mangled_id(const char *mangled, char **base_out,
    char ***args_out, int *argc_out)
{
    if (mangled == NULL || base_out == NULL || args_out == NULL || argc_out == NULL)
        return 0;

    const char *dollar = strchr(mangled, '$');
    if (dollar == NULL || dollar == mangled || dollar[1] == '\0')
        return 0;

    size_t base_len = (size_t)(dollar - mangled);
    char *base = (char *)malloc(base_len + 1);
    if (base == NULL)
        return 0;
    memcpy(base, mangled, base_len);
    base[base_len] = '\0';

    int count = 0;
    for (const char *p = dollar; p != NULL; p = strchr(p + 1, '$'))
        count++;
    if (count <= 0)
    {
        free(base);
        return 0;
    }

    char **args = (char **)calloc((size_t)count, sizeof(char *));
    if (args == NULL)
    {
        free(base);
        return 0;
    }

    int idx = 0;
    const char *seg = dollar + 1;
    while (seg != NULL && idx < count)
    {
        const char *next = strchr(seg, '$');
        size_t seg_len = next ? (size_t)(next - seg) : strlen(seg);
        args[idx] = (char *)malloc(seg_len + 1);
        if (args[idx] == NULL)
            break;
        memcpy(args[idx], seg, seg_len);
        args[idx][seg_len] = '\0';
        idx++;
        if (next == NULL)
            break;
        seg = next + 1;
    }

    if (idx != count)
    {
        for (int i = 0; i < count; ++i)
            free(args[i]);
        free(args);
        free(base);
        return 0;
    }

    *base_out = base;
    *args_out = args;
    *argc_out = count;
    return 1;
}

static void substitute_generic_identifiers(ast_t *node, char **params, char **args, int count)
{
    if (node == NULL || params == NULL || args == NULL || count <= 0)
        return;

    ast_t *cursor = node;
    while (cursor != NULL)
    {
        if (cursor->sym != NULL && cursor->sym->name != NULL)
        {
            for (int i = 0; i < count; ++i)
            {
                if (params[i] != NULL && args[i] != NULL &&
                    strcasecmp(cursor->sym->name, params[i]) == 0)
                {
                    free(cursor->sym->name);
                    cursor->sym->name = strdup(args[i]);
                    break;
                }
            }
        }
        if (cursor->child != NULL)
            substitute_generic_identifiers(cursor->child, params, args, count);
        cursor = cursor->next;
    }
}

/* Collect local type names from TYPE_DECL nodes within a subprogram AST.
 * Only walks children of the root node (not siblings) to avoid picking up
 * global type declarations from the program tree.
 * Returns allocated arrays via out params; caller must free. */
static void collect_local_type_names(ast_t *node, char ***names_out, int *count_out)
{
    *names_out = NULL;
    *count_out = 0;
    if (node == NULL || node->child == NULL)
        return;

    int capacity = 4;
    int count = 0;
    char **names = (char **)malloc(capacity * sizeof(char *));
    if (names == NULL)
        return;

    /* Walk only children of the root (subprogram body), not siblings */
    ast_t *stack[256];
    int sp = 0;
    /* Push only children of root, not root->next */
    for (ast_t *c = node->child; c != NULL; c = c->next)
    {
        if (sp < 256)
            stack[sp++] = c;
    }

    while (sp > 0)
    {
        ast_t *cur = stack[--sp];
        if (cur->typ == PASCAL_T_TYPE_DECL)
        {
            ast_t *id_node = cur->child;
            if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER &&
                id_node->sym != NULL && id_node->sym->name != NULL)
            {
                if (count >= capacity)
                {
                    capacity *= 2;
                    char **tmp = (char **)realloc(names, capacity * sizeof(char *));
                    if (tmp == NULL) break;
                    names = tmp;
                }
                names[count++] = strdup(id_node->sym->name);
            }
            /* Don't recurse into type decl children */
            continue;
        }
        if (cur->child != NULL && sp < 256)
            stack[sp++] = cur->child;
        /* Follow next for non-root nodes */
        if (cur->next != NULL && sp < 256)
            stack[sp++] = cur->next;
    }

    *names_out = names;
    *count_out = count;
}

static void rewrite_generic_subprogram_ast(ast_t *subprogram_ast, const char *specialized_name,
    char **params, char **args, int count)
{
    if (subprogram_ast == NULL || specialized_name == NULL)
        return;

    ast_t *cursor = subprogram_ast->child;
    while (cursor != NULL && cursor->typ != PASCAL_T_IDENTIFIER)
        cursor = cursor->next;
    if (cursor != NULL && cursor->sym != NULL)
    {
        free(cursor->sym->name);
        cursor->sym->name = strdup(specialized_name);
    }

    substitute_generic_identifiers(subprogram_ast, params, args, count);

    /* Strip the generic type parameter list node (PASCAL_T_TYPE_PARAM_LIST)
     * from the rewritten AST.  The specialization is a concrete subprogram;
     * leaving the node would cause convert_procedure/convert_function to
     * re-detect it as a generic template and create a spurious
     * generic_template_ast (a use-after-free hazard, see issue #478). */
    {
        ast_t *prev = NULL;
        ast_t *node = subprogram_ast->child;
        while (node != NULL)
        {
            if (node->typ == PASCAL_T_TYPE_PARAM_LIST)
            {
                /* Unlink the node from the sibling chain. */
                if (prev == NULL)
                    subprogram_ast->child = node->next;
                else
                    prev->next = node->next;
                /* Sever the sibling link before freeing: free_ast() follows
                 * both child and next pointers, and we must not free the
                 * remaining AST siblings that follow this node. */
                node->next = NULL;
                free_ast(node);
                break;
            }
            prev = node;
            node = node->next;
        }
    }

    /* Rename local type declarations so each specialization gets unique names.
     * E.g., local type "RawT" in Swap<LongInt> becomes "Swap$LongInt.RawT". */
    char **local_names = NULL;
    int local_count = 0;
    collect_local_type_names(subprogram_ast, &local_names, &local_count);
    if (local_count > 0 && local_names != NULL)
    {
        char **renamed = (char **)malloc(local_count * sizeof(char *));
        if (renamed != NULL)
        {
            for (int i = 0; i < local_count; i++)
            {
                size_t len = strlen(specialized_name) + 1 + strlen(local_names[i]) + 1;
                renamed[i] = (char *)malloc(len);
                if (renamed[i] != NULL)
                    snprintf(renamed[i], len, "%s$%s", specialized_name, local_names[i]);
            }
            substitute_generic_identifiers(subprogram_ast, local_names, renamed, local_count);
            for (int i = 0; i < local_count; i++)
                free(renamed[i]);
            free(renamed);
        }
    }
    if (local_names != NULL)
    {
        for (int i = 0; i < local_count; i++)
            free(local_names[i]);
        free(local_names);
    }
}

static Tree_t *instantiate_generic_subprogram(Tree_t *template,
    char **arg_types, int arg_count, const char *specialized_name)
{
    if (template == NULL || arg_types == NULL || arg_count <= 0 ||
        template->tree_data.subprogram_data.generic_template_ast == NULL)
        return NULL;

    assert(template->tree_data.subprogram_data.is_generic_template);

    ast_t *ast_copy = copy_ast(template->tree_data.subprogram_data.generic_template_ast);
    if (ast_copy == NULL)
        return NULL;

    rewrite_generic_subprogram_ast(ast_copy, specialized_name,
        template->tree_data.subprogram_data.generic_type_params, arg_types, arg_count);

    /* Restore the source offset from the unit that defined the template,
     * so that source_index values in the specialized code map to the
     * correct source buffer (e.g., fgl.pp, not the main program). */
    int saved_offset = g_source_offset;
    g_source_offset = template->tree_data.subprogram_data.generic_template_source_offset;

    Tree_t *result = NULL;
    if (template->type == TREE_SUBPROGRAM &&
        template->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_PROC)
        result = convert_procedure(ast_copy);
    else if (template->type == TREE_SUBPROGRAM &&
        template->tree_data.subprogram_data.sub_type == TREE_SUBPROGRAM_FUNC)
        result = convert_function(ast_copy);

    g_source_offset = saved_offset;
    free_ast(ast_copy);

    if (result != NULL)
    {
        /* Verify that the specialization has no generic metadata.
         * rewrite_generic_subprogram_ast() strips TYPE_PARAM_LIST so
         * convert_procedure/convert_function should not detect any
         * generic type parameters on the rewritten AST. */
        assert(result->tree_data.subprogram_data.num_generic_type_params == 0);
        assert(result->tree_data.subprogram_data.generic_type_params == NULL);
        assert(!result->tree_data.subprogram_data.is_generic_template);
        assert(result->tree_data.subprogram_data.generic_template_ast == NULL);
    }

    return result;
}

/* Check if a subprogram ID matches a generic base name.
 * Handles both exact match ("UnfixArray" == "UnfixArray") and
 * class-prefixed match ("TMarshal__UnfixArray" ends with "__UnfixArray").
 * This is needed because specialize calls inside generic record methods
 * use the short method name (e.g. "UnfixArray$T") while the subprogram
 * templates carry the class-qualified ID (e.g. "TMarshal__UnfixArray"). */
static int generic_base_name_matches(Tree_t *sub, const char *base)
{
    if (sub == NULL || base == NULL)
        return 0;
    /* For class methods, the bare method_name is the relevant identifier */
    if (sub->tree_data.subprogram_data.method_name != NULL &&
        strcasecmp(sub->tree_data.subprogram_data.method_name, base) == 0)
        return 1;
    /* For standalone subprograms, check the id directly */
    if (sub->tree_data.subprogram_data.id != NULL &&
        strcasecmp(sub->tree_data.subprogram_data.id, base) == 0)
        return 1;
    return 0;
}

static void collect_specialize_from_expr(struct Expression *expr, Tree_t *program_tree)
{
    if (expr == NULL || program_tree == NULL)
        return;

    switch (expr->type)
    {
        case EXPR_TYPECAST:
        {
            const char *target_id = expr->expr_data.typecast_data.target_type_id;
            char *base = NULL;
            char **args = NULL;
            int argc = 0;
            if (target_id != NULL &&
                parse_generic_mangled_id(target_id, &base, &args, &argc))
            {
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
                    fprintf(stderr, "[KGPC] specialize expr target=%s base=%s argc=%d\n",
                        target_id, base ? base : "<null>", argc);
                /* Skip if any type arg looks like an unresolved type parameter */
                int has_unresolved_arg = 0;
                for (int i = 0; i < argc; ++i)
                {
                    if (args[i] != NULL && strlen(args[i]) == 1 && isupper((unsigned char)args[i][0]))
                    {
                        has_unresolved_arg = 1;
                        break;
                    }
                }
                if (!has_unresolved_arg)
                {
                    ListNode_t *cur = program_tree->tree_data.program_data.subprograms;
                    while (cur != NULL)
                {
                    if (cur->type == LIST_TREE && cur->cur != NULL)
                    {
                        Tree_t *sub = (Tree_t *)cur->cur;
                        if (sub->type == TREE_SUBPROGRAM &&
                            sub->tree_data.subprogram_data.id != NULL &&
                            sub->tree_data.subprogram_data.num_generic_type_params == argc &&
                            generic_base_name_matches(sub, base))
                        {
                            if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
                                fprintf(stderr, "[KGPC] generic template match %s\n",
                                    sub->tree_data.subprogram_data.id);
                            /* Reconstruct proper specialized name with class prefix */
                            const char *sub_id = sub->tree_data.subprogram_data.id;
                            const char *spec_name = target_id;
                            char *constructed_name = NULL;
                            if (strcasecmp(sub_id, base) != 0)
                            {
                                size_t sub_len = strlen(sub_id);
                                size_t base_len = strlen(base);
                                size_t prefix_len = sub_len - base_len;
                                const char *dollar = strchr(target_id, '$');
                                if (dollar != NULL)
                                {
                                    size_t suffix_len = strlen(dollar);
                                    constructed_name = (char *)malloc(prefix_len + base_len + suffix_len + 1);
                                    if (constructed_name != NULL)
                                    {
                                        memcpy(constructed_name, sub_id, prefix_len);
                                        memcpy(constructed_name + prefix_len, base, base_len);
                                        memcpy(constructed_name + prefix_len + base_len, dollar, suffix_len);
                                        constructed_name[prefix_len + base_len + suffix_len] = '\0';
                                        spec_name = constructed_name;
                                    }
                                }
                            }
                            if (!subprogram_list_has_id(program_tree->tree_data.program_data.subprograms, spec_name))
                            {
                                Tree_t *specialized = instantiate_generic_subprogram(
                                    sub, args, argc, spec_name);
                                if (specialized != NULL)
                                    append_subprogram_node(&program_tree->tree_data.program_data.subprograms,
                                        specialized);
                            }
                            free(constructed_name);
                            break;
                        }
                    }
                    cur = cur->next;
                }
                }
            }
            if (args != NULL)
            {
                for (int i = 0; i < argc; ++i)
                    free(args[i]);
                free(args);
            }
            if (base != NULL)
                free(base);
            collect_specialize_from_expr(expr->expr_data.typecast_data.expr, program_tree);
            break;
        }
        case EXPR_FUNCTION_CALL:
        {
            const char *call_id = expr->expr_data.function_call_data.id;
            char *base = NULL;
            char **args = NULL;
            int argc = 0;
            if (call_id != NULL &&
                parse_generic_mangled_id(call_id, &base, &args, &argc))
            {
                /* Skip if any type arg looks like an unresolved type parameter
                 * (e.g. single uppercase letter "T").  These come from bodies
                 * of generic record methods that haven't been specialized yet. */
                int has_unresolved_arg = 0;
                for (int i = 0; i < argc; ++i)
                {
                    if (args[i] != NULL && strlen(args[i]) == 1 && isupper((unsigned char)args[i][0]))
                    {
                        has_unresolved_arg = 1;
                        break;
                    }
                }
                if (!has_unresolved_arg)
                {
                    ListNode_t *cur = program_tree->tree_data.program_data.subprograms;
                    while (cur != NULL)
                    {
                        if (cur->type == LIST_TREE && cur->cur != NULL)
                        {
                            Tree_t *sub = (Tree_t *)cur->cur;
                            if (sub->type == TREE_SUBPROGRAM &&
                                sub->tree_data.subprogram_data.id != NULL &&
                                sub->tree_data.subprogram_data.num_generic_type_params == argc &&
                                generic_base_name_matches(sub, base))
                            {
                                /* Build the proper specialized name: if the template has
                                 * a class prefix (e.g., "TMarshal__UnfixArray") and the
                                 * call_id is just "UnfixArray$TPtrWrapper", reconstruct
                                 * to "TMarshal__UnfixArray$TPtrWrapper". */
                                const char *sub_id = sub->tree_data.subprogram_data.id;
                                const char *spec_name = call_id;
                                char *constructed_name = NULL;
                                if (strcasecmp(sub_id, base) != 0)
                                {
                                    /* Template has a prefix: extract it */
                                    size_t sub_len = strlen(sub_id);
                                    size_t base_len = strlen(base);
                                    size_t prefix_len = sub_len - base_len;
                                    const char *dollar = strchr(call_id, '$');
                                    if (dollar != NULL)
                                    {
                                        size_t suffix_len = strlen(dollar);
                                        constructed_name = (char *)malloc(prefix_len + base_len + suffix_len + 1);
                                        if (constructed_name != NULL)
                                        {
                                            memcpy(constructed_name, sub_id, prefix_len);
                                            memcpy(constructed_name + prefix_len, base, base_len);
                                            memcpy(constructed_name + prefix_len + base_len, dollar, suffix_len);
                                            constructed_name[prefix_len + base_len + suffix_len] = '\0';
                                            spec_name = constructed_name;
                                        }
                                    }
                                }
                                if (!subprogram_list_has_id(program_tree->tree_data.program_data.subprograms, spec_name))
                                {
                                    Tree_t *specialized = instantiate_generic_subprogram(
                                        sub, args, argc, spec_name);
                                    if (specialized != NULL)
                                        append_subprogram_node(&program_tree->tree_data.program_data.subprograms,
                                            specialized);
                                }
                                free(constructed_name);
                                break;
                            }
                        }
                        cur = cur->next;
                    }
                }
            }
            if (args != NULL)
            {
                for (int i = 0; i < argc; ++i)
                    free(args[i]);
                free(args);
            }
            if (base != NULL)
                free(base);

            ListNode_t *arg = expr->expr_data.function_call_data.args_expr;
            while (arg != NULL)
            {
                if (arg->type == LIST_EXPR)
                    collect_specialize_from_expr((struct Expression *)arg->cur, program_tree);
                arg = arg->next;
            }
            break;
        }
        case EXPR_RECORD_ACCESS:
            collect_specialize_from_expr(expr->expr_data.record_access_data.record_expr, program_tree);
            break;
        case EXPR_ARRAY_ACCESS:
            collect_specialize_from_expr(expr->expr_data.array_access_data.array_expr, program_tree);
            collect_specialize_from_expr(expr->expr_data.array_access_data.index_expr, program_tree);
            break;
        case EXPR_RELOP:
            collect_specialize_from_expr(expr->expr_data.relop_data.left, program_tree);
            collect_specialize_from_expr(expr->expr_data.relop_data.right, program_tree);
            break;
        case EXPR_SIGN_TERM:
            collect_specialize_from_expr(expr->expr_data.sign_term, program_tree);
            break;
        case EXPR_ADDOP:
            collect_specialize_from_expr(expr->expr_data.addop_data.left_expr, program_tree);
            collect_specialize_from_expr(expr->expr_data.addop_data.right_term, program_tree);
            break;
        case EXPR_MULOP:
            collect_specialize_from_expr(expr->expr_data.mulop_data.left_term, program_tree);
            collect_specialize_from_expr(expr->expr_data.mulop_data.right_factor, program_tree);
            break;
        case EXPR_ADDR:
            collect_specialize_from_expr(expr->expr_data.addr_data.expr, program_tree);
            break;
        case EXPR_POINTER_DEREF:
            collect_specialize_from_expr(expr->expr_data.pointer_deref_data.pointer_expr, program_tree);
            break;
        default:
            break;
    }
}

static void collect_specialize_from_stmt(struct Statement *stmt, Tree_t *program_tree)
{
    if (stmt == NULL || program_tree == NULL)
        return;

    switch (stmt->type)
    {
        case STMT_VAR_ASSIGN:
            collect_specialize_from_expr(stmt->stmt_data.var_assign_data.var, program_tree);
            collect_specialize_from_expr(stmt->stmt_data.var_assign_data.expr, program_tree);
            break;
        case STMT_PROCEDURE_CALL:
        {
            ListNode_t *arg = stmt->stmt_data.procedure_call_data.expr_args;
            while (arg != NULL)
            {
                if (arg->type == LIST_EXPR)
                    collect_specialize_from_expr((struct Expression *)arg->cur, program_tree);
                arg = arg->next;
            }
            break;
        }
        case STMT_EXPR:
            collect_specialize_from_expr(stmt->stmt_data.expr_stmt_data.expr, program_tree);
            break;
        case STMT_COMPOUND_STATEMENT:
        {
            ListNode_t *cur = stmt->stmt_data.compound_statement;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            break;
        }
        case STMT_LABEL:
            collect_specialize_from_stmt(stmt->stmt_data.label_data.stmt, program_tree);
            break;
        case STMT_IF_THEN:
            collect_specialize_from_expr(stmt->stmt_data.if_then_data.relop_expr, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.if_then_data.if_stmt, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.if_then_data.else_stmt, program_tree);
            break;
        case STMT_WHILE:
            collect_specialize_from_expr(stmt->stmt_data.while_data.relop_expr, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.while_data.while_stmt, program_tree);
            break;
        case STMT_REPEAT:
        {
            ListNode_t *cur = stmt->stmt_data.repeat_data.body_list;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            collect_specialize_from_expr(stmt->stmt_data.repeat_data.until_expr, program_tree);
            break;
        }
        case STMT_FOR:
            collect_specialize_from_expr(stmt->stmt_data.for_data.to, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.for_data.do_for, program_tree);
            if (stmt->stmt_data.for_data.for_assign_type == STMT_VAR_ASSIGN)
                collect_specialize_from_stmt(stmt->stmt_data.for_data.for_assign_data.var_assign, program_tree);
            else
                collect_specialize_from_expr(stmt->stmt_data.for_data.for_assign_data.var, program_tree);
            break;
        case STMT_FOR_IN:
            collect_specialize_from_expr(stmt->stmt_data.for_in_data.loop_var, program_tree);
            collect_specialize_from_expr(stmt->stmt_data.for_in_data.collection, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.for_in_data.do_stmt, program_tree);
            break;
        case STMT_CASE:
        {
            collect_specialize_from_expr(stmt->stmt_data.case_data.selector_expr, program_tree);
            ListNode_t *branch = stmt->stmt_data.case_data.branches;
            while (branch != NULL)
            {
                if (branch->type == LIST_CASE_BRANCH && branch->cur != NULL)
                {
                    struct CaseBranch *cb = (struct CaseBranch *)branch->cur;
                    ListNode_t *label = cb->labels;
                    while (label != NULL)
                    {
                        if (label->type == LIST_EXPR)
                            collect_specialize_from_expr((struct Expression *)label->cur, program_tree);
                        label = label->next;
                    }
                    collect_specialize_from_stmt(cb->stmt, program_tree);
                }
                branch = branch->next;
            }
            collect_specialize_from_stmt(stmt->stmt_data.case_data.else_stmt, program_tree);
            break;
        }
        case STMT_WITH:
            collect_specialize_from_expr(stmt->stmt_data.with_data.context_expr, program_tree);
            collect_specialize_from_stmt(stmt->stmt_data.with_data.body_stmt, program_tree);
            break;
        case STMT_TRY_FINALLY:
        {
            ListNode_t *cur = stmt->stmt_data.try_finally_data.try_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            cur = stmt->stmt_data.try_finally_data.finally_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            break;
        }
        case STMT_TRY_EXCEPT:
        {
            ListNode_t *cur = stmt->stmt_data.try_except_data.try_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            cur = stmt->stmt_data.try_except_data.except_statements;
            while (cur != NULL)
            {
                if (cur->type == LIST_STMT)
                    collect_specialize_from_stmt((struct Statement *)cur->cur, program_tree);
                cur = cur->next;
            }
            break;
        }
        case STMT_RAISE:
            collect_specialize_from_expr(stmt->stmt_data.raise_data.exception_expr, program_tree);
            break;
        case STMT_INHERITED:
            collect_specialize_from_expr(stmt->stmt_data.inherited_data.call_expr, program_tree);
            break;
        case STMT_EXIT:
            collect_specialize_from_expr(stmt->stmt_data.exit_data.return_expr, program_tree);
            break;
        default:
            break;
    }
}

void resolve_pending_generic_subprograms(Tree_t *program_tree)
{
    if (program_tree == NULL || program_tree->type != TREE_PROGRAM_TYPE)
        return;

    if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
        fprintf(stderr, "[KGPC] resolve_pending_generic_subprograms\n");

    ListNode_t *sub = program_tree->tree_data.program_data.subprograms;
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL && sub == NULL)
        fprintf(stderr, "[KGPC] program has no subprograms\n");
    while (sub != NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL)
            fprintf(stderr, "[KGPC] subprogram node type=%d\n", sub->type);
        if (sub->type == LIST_TREE && sub->cur != NULL)
        {
            Tree_t *sub_tree = (Tree_t *)sub->cur;
            if (sub_tree->type == TREE_SUBPROGRAM)
            {
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_SUBPROGRAM") != NULL &&
                    sub_tree->tree_data.subprogram_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] subprogram %s generic_params=%d\n",
                        sub_tree->tree_data.subprogram_data.id,
                        sub_tree->tree_data.subprogram_data.num_generic_type_params);
                }
                /* Skip bodies of unresolved generic templates — their expressions
                 * still contain raw type parameter names (e.g. "T") which would
                 * produce invalid specializations if matched. */
                if (sub_tree->tree_data.subprogram_data.generic_type_params == NULL &&
                    sub_tree->tree_data.subprogram_data.num_generic_type_params == 0)
                    collect_specialize_from_stmt(sub_tree->tree_data.subprogram_data.statement_list, program_tree);
            }
        }
        sub = sub->next;
    }

    collect_specialize_from_stmt(program_tree->tree_data.program_data.body_statement, program_tree);
    /* Scan unit init/final blocks for generic specializations */
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                if (unit_tree->tree_data.unit_data.initialization != NULL)
                    collect_specialize_from_stmt(unit_tree->tree_data.unit_data.initialization, program_tree);
                if (unit_tree->tree_data.unit_data.finalization != NULL)
                    collect_specialize_from_stmt(unit_tree->tree_data.unit_data.finalization, program_tree);
            }
        }
    }
}

static int subprogram_list_has_decl(const ListNode_t *subprograms, const Tree_t *tree)
{
    if (subprograms == NULL || tree == NULL || tree->type != TREE_SUBPROGRAM)
        return 0;

    const char *id = tree->tree_data.subprogram_data.id;
    int line_num = tree->line_num;
    enum TreeType sub_type = tree->tree_data.subprogram_data.sub_type;

    const ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        Tree_t *existing = (Tree_t *)cur->cur;
        if (existing != NULL && existing->type == TREE_SUBPROGRAM)
        {
            if (existing->line_num == line_num &&
                existing->tree_data.subprogram_data.sub_type == sub_type)
            {
                const char *existing_id = existing->tree_data.subprogram_data.id;
                if (existing_id != NULL && id != NULL &&
                    pascal_identifier_equals(existing_id, id))
                {
                    return 1;
                }
            }
        }
        cur = cur->next;
    }

    return 0;
}

static void append_subprogram_if_unique(ListNode_t **dest, Tree_t *tree)
{
    if (dest == NULL || tree == NULL)
        return;

    if (subprogram_list_has_decl(*dest, tree))
    {
        destroy_tree(tree);
        return;
    }

    append_subprogram_node(dest, tree);
}

static void append_subprograms_from_ast_recursive(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited)
{
    /* Iterate over siblings to avoid deep recursion on node->next chains */
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next)
    {
        if (subprograms == NULL || visited == NULL)
            return;

        if (!is_safe_to_continue(visited, cur))
            continue;

        switch (cur->typ)
        {
        case PASCAL_T_PROCEDURE_DECL: {
            Tree_t *proc = convert_procedure(cur);
            append_subprogram_if_unique(subprograms, proc);
            continue; /* skip child traversal for subprograms */
        }
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *func = convert_function(cur);
            append_subprogram_if_unique(subprograms, func);
            continue;
        }
        case PASCAL_T_METHOD_IMPL:
        case PASCAL_T_CONSTRUCTOR_DECL:
        case PASCAL_T_DESTRUCTOR_DECL: {
            Tree_t *method_tree = convert_method_impl(cur);
            append_subprogram_if_unique(subprograms, method_tree);
            continue;
        }
        default:
            break;
        }

        /* Recurse into children (tree depth is bounded) */
        append_subprograms_from_ast_recursive(cur->child, subprograms, visited);
    }
}

static void append_top_level_subprograms_from_ast(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited, int in_subprogram)
{
    /* Iterate over siblings to avoid deep recursion on node->next chains */
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next)
    {
        if (subprograms == NULL || visited == NULL)
            return;

        if (visited_set_contains(visited, cur))
            continue;
        visited_set_add(visited, cur);

        int is_subprogram = (cur->typ == PASCAL_T_PROCEDURE_DECL ||
            cur->typ == PASCAL_T_FUNCTION_DECL ||
            cur->typ == PASCAL_T_METHOD_IMPL ||
            cur->typ == PASCAL_T_CONSTRUCTOR_DECL ||
            cur->typ == PASCAL_T_DESTRUCTOR_DECL);

        if (is_subprogram && !in_subprogram)
        {
            if (cur->typ == PASCAL_T_PROCEDURE_DECL)
            {
                Tree_t *proc = convert_procedure(cur);
                append_subprogram_if_unique(subprograms, proc);
            }
            else if (cur->typ == PASCAL_T_FUNCTION_DECL)
            {
                Tree_t *func = convert_function(cur);
                append_subprogram_if_unique(subprograms, func);
            }
            else
            {
                Tree_t *method_tree = convert_method_impl(cur);
                append_subprogram_if_unique(subprograms, method_tree);
            }
        }

        int child_in_subprogram = in_subprogram || is_subprogram;
        /* Recurse into children (tree depth is bounded) */
        append_top_level_subprograms_from_ast(cur->child, subprograms, visited, child_in_subprogram);
    }
}

static void sync_method_impls_from_generic_template(struct RecordType *record)
{
    if (record == NULL || record->generic_decl == NULL ||
        record->generic_decl->record_template == NULL ||
        record->method_templates == NULL)
        return;

    ListNode_t *source = record->generic_decl->record_template->method_templates;
    for (ListNode_t *cur = record->method_templates; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_METHOD_TEMPLATE)
            continue;
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (tmpl == NULL || tmpl->method_impl_ast != NULL || tmpl->name == NULL)
            continue;

        for (ListNode_t *src = source; src != NULL; src = src->next)
        {
            if (src->type != LIST_METHOD_TEMPLATE)
                continue;
            struct MethodTemplate *src_tmpl = (struct MethodTemplate *)src->cur;
            if (src_tmpl != NULL && src_tmpl->name != NULL &&
                strcasecmp(src_tmpl->name, tmpl->name) == 0 &&
                src_tmpl->method_impl_ast != NULL)
            {
                tmpl->method_impl_ast = copy_ast_detached(src_tmpl->method_impl_ast);
                break;
            }
        }
    }
}

static int subprogram_list_has_mangled(const ListNode_t *subprograms, const char *mangled_id)
{
    if (subprograms == NULL || mangled_id == NULL)
        return 0;

    const ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            const Tree_t *tree = (const Tree_t *)cur->cur;
            if ((tree->type == TREE_SUBPROGRAM_FUNC || tree->type == TREE_SUBPROGRAM_PROC) &&
                tree->tree_data.subprogram_data.mangled_id != NULL &&
                strcmp(tree->tree_data.subprogram_data.mangled_id, mangled_id) == 0)
            {
                return 1;
            }
        }
        cur = cur->next;
    }
    return 0;
}

static void append_specialized_method_clones(Tree_t *decl, ListNode_t **subprograms)
{
    if (decl == NULL || subprograms == NULL)
        return;
    if (decl->type != TREE_TYPE_DECL)
        return;
    
    struct RecordType *record = NULL;
    
    // Get the record from either TYPE_DECL_RECORD or TYPE_DECL_ALIAS
    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
        record = decl->tree_data.type_decl_data.info.record;
    } else if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
        record = decl->tree_data.type_decl_data.info.alias.inline_record_type;
        if (record == NULL && decl->tree_data.type_decl_data.kgpc_type != NULL)
        {
            KgpcType *alias_type = decl->tree_data.type_decl_data.kgpc_type;
            if (kgpc_type_is_record(alias_type))
                record = kgpc_type_get_record(alias_type);
            else if (kgpc_type_is_pointer(alias_type) && alias_type->info.points_to != NULL &&
                     kgpc_type_is_record(alias_type->info.points_to))
                record = kgpc_type_get_record(alias_type->info.points_to);
        }
    } else {
        return;
    }

    sync_method_impls_from_generic_template(record);

    if (record == NULL || record->method_templates == NULL ||
        record->generic_decl == NULL || record->generic_args == NULL ||
        record->num_generic_args <= 0)
    {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL &&
            record != NULL && record->type_id != NULL)
            fprintf(stderr, "[KGPC] skipping clone for %s (missing templates)\n", record->type_id);
        return;
    }

    /* Skip records whose generic_args are still unresolved type parameters.
     * E.g. TFoo$T with generic_args=["T"] matching generic_decl params=["T"].
     * Method clones from these would produce bodies with unresolved type refs. */
    if (record->generic_decl->type_parameters != NULL)
    {
        int all_unresolved = 1;
        for (int i = 0; i < record->num_generic_args && i < record->generic_decl->num_type_params; ++i)
        {
            const char *arg = record->generic_args[i];
            const char *param = record->generic_decl->type_parameters[i];
            if (arg == NULL || param == NULL || strcasecmp(arg, param) != 0)
            {
                all_unresolved = 0;
                break;
            }
        }
        if (all_unresolved)
        {
            if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL &&
                record->type_id != NULL)
                fprintf(stderr, "[KGPC] skipping clone for %s (unresolved type params)\n", record->type_id);
            return;
        }
    }
    if (record->method_clones_emitted)
        return;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES");
    int appended_any = 0;
    ListNode_t *cur = record->method_templates;
    while (cur != NULL)
    {
        if (cur->type == LIST_METHOD_TEMPLATE)
        {
            struct MethodTemplate *template = (struct MethodTemplate *)cur->cur;
            if (template != NULL)
            {
                Tree_t *method_tree = instantiate_method_template(template, record);
                if (method_tree != NULL) {
                    if (method_tree->tree_data.subprogram_data.mangled_id != NULL &&
                        subprogram_list_has_mangled(*subprograms,
                            method_tree->tree_data.subprogram_data.mangled_id))
                    {
                        destroy_tree(method_tree);
                        method_tree = NULL;
                    }
                }
                if (method_tree != NULL) {
                    append_subprogram_node(subprograms, method_tree);
                    appended_any = 1;
                    if (debug_env != NULL && record->type_id != NULL && template->name != NULL)
                        fprintf(stderr, "[KGPC] cloned method %s.%s\n", record->type_id, template->name);
                } else if (debug_env != NULL && record->type_id != NULL && template->name != NULL) {
                    fprintf(stderr, "[KGPC] failed to clone method %s.%s (missing implementation)\n",
                            record->type_id, template->name);
                }
            }
        }
        cur = cur->next;
    }
    if (appended_any)
        record->method_clones_emitted = 1;
}

void flush_deferred_inline_specializations(Tree_t *program_tree)
{
    DeferredInlineSpec *cur = g_deferred_inline_specs;
    g_deferred_inline_specs = NULL;

    ListNode_t **type_list = NULL;
    ListNode_t **subprograms = NULL;
    if (program_tree != NULL && program_tree->type == TREE_PROGRAM_TYPE) {
        type_list = &program_tree->tree_data.program_data.type_declaration;
        subprograms = &program_tree->tree_data.program_data.subprograms;
    } else if (program_tree != NULL && program_tree->type == TREE_UNIT) {
        type_list = &program_tree->tree_data.unit_data.interface_type_decls;
        subprograms = &program_tree->tree_data.unit_data.subprograms;
    }

    if (type_list == NULL) {
        while (cur != NULL) {
            DeferredInlineSpec *next = cur->next;
            if (cur->type_decl != NULL)
                destroy_tree(cur->type_decl);
            free(cur);
            cur = next;
        }
        return;
    }

    while (cur != NULL) {
        DeferredInlineSpec *next = cur->next;
        if (cur->type_decl != NULL) {
            /* Emit method clones for this inline specialization */
            if (subprograms != NULL)
                append_specialized_method_clones(cur->type_decl, subprograms);
            ListNode_t *node = CreateListNode(cur->type_decl, LIST_TREE);
            if (node != NULL) {
                node->next = *type_list;
                *type_list = node;
            } else {
                destroy_tree(cur->type_decl);
            }
        }
        free(cur);
        cur = next;
    }
}

void append_generic_method_clones(Tree_t *program_tree)
{
    if (program_tree == NULL || program_tree->type != TREE_PROGRAM_TYPE)
        return;

    ListNode_t *type_cursor = program_tree->tree_data.program_data.type_declaration;
    while (type_cursor != NULL)
    {
        if (type_cursor->type == LIST_TREE && type_cursor->cur != NULL)
        {
            Tree_t *decl = (Tree_t *)type_cursor->cur;
            append_specialized_method_clones(decl,
                &program_tree->tree_data.program_data.subprograms);
        }
        type_cursor = type_cursor->next;
    }
}

/* Clone nested type declarations from a generic class for a specialization.
 * For example, if TFPGList has nested type TFPGList.PT = ^T, then specializing
 * TFPGList<TMyRecord> produces TFPGList$TMyRecord.PT = ^TMyRecord.
 * The cloned declarations are appended to the program's type declaration list. */
static void clone_nested_types_for_specialization(
    GenericTypeDecl *generic_decl,
    const char *specialized_name, /* e.g. "TFPGList$TMyRecord" */
    ListNode_t *type_args,        /* concrete type arguments */
    ListNode_t **type_list_out,   /* where to append new type decls */
    ListNode_t **subprograms_out) /* where to append method clones (may be NULL) */
{
    if (generic_decl == NULL || generic_decl->nested_type_decls == NULL ||
        specialized_name == NULL || type_list_out == NULL)
        return;

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    const char *generic_name = generic_decl->name;
    size_t prefix_len = generic_name != NULL ? strlen(generic_name) : 0;

    /* Build arg_types array for substitution */
    int arg_count = 0;
    ListNode_t *arg_cur = type_args;
    while (arg_cur != NULL) {
        if (arg_cur->type == LIST_STRING) arg_count++;
        arg_cur = arg_cur->next;
    }

    ListNode_t *cur = generic_decl->nested_type_decls;
    while (cur != NULL) {
        if (cur->type == LIST_TREE && cur->cur != NULL) {
            Tree_t *orig = (Tree_t *)cur->cur;
            if (orig->type == TREE_TYPE_DECL && orig->tree_data.type_decl_data.id != NULL) {
                const char *orig_id = orig->tree_data.type_decl_data.id;
                /* Check that orig_id starts with generic_name + "." */
                if (prefix_len > 0 && strncmp(orig_id, generic_name, prefix_len) == 0 &&
                    orig_id[prefix_len] == '.')
                {
                    const char *suffix = orig_id + prefix_len; /* e.g., ".PT" */
                    /* Build new id: specialized_name + suffix */
                    size_t new_id_len = strlen(specialized_name) + strlen(suffix) + 1;
                    char *new_id = (char *)malloc(new_id_len);
                    if (new_id == NULL) { cur = cur->next; continue; }
                    snprintf(new_id, new_id_len, "%s%s", specialized_name, suffix);

                    Tree_t *clone = mk_typedecl(orig->line_num, new_id, 0, 0);
                    if (clone == NULL) { free(new_id); cur = cur->next; continue; }

                    clone->source_index = orig->source_index;
                    clone->tree_data.type_decl_data.kind = orig->tree_data.type_decl_data.kind;
                    clone->tree_data.type_decl_data.defined_in_unit = orig->tree_data.type_decl_data.defined_in_unit;

                    if (orig->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *src_alias = &orig->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *dst_alias = &clone->tree_data.type_decl_data.info.alias;
                        *dst_alias = *src_alias; /* Shallow copy */
                        dst_alias->target_type_id = src_alias->target_type_id ?
                            strdup(src_alias->target_type_id) : NULL;
                        dst_alias->pointer_type_id = src_alias->pointer_type_id ?
                            strdup(src_alias->pointer_type_id) : NULL;
                        dst_alias->array_element_type_id = src_alias->array_element_type_id ?
                            strdup(src_alias->array_element_type_id) : NULL;
                        dst_alias->inline_record_type = NULL;
                        dst_alias->target_type_ref = src_alias->target_type_ref ?
                            type_ref_clone(src_alias->target_type_ref) : NULL;
                        dst_alias->array_element_type_ref = src_alias->array_element_type_ref ?
                            type_ref_clone(src_alias->array_element_type_ref) : NULL;

                        /* Substitute generic type parameters in type identifiers.
                         * For example, if T → TMyRecord, then "T" → "TMyRecord",
                         * and pointer_type_id "T" → "TMyRecord". */
                        for (int pi = 0; pi < generic_decl->num_type_params && pi < arg_count; pi++) {
                            const char *param = generic_decl->type_parameters[pi];
                            /* Get the concrete type for this parameter */
                            ListNode_t *a = type_args;
                            for (int ai = 0; ai < pi && a != NULL; ai++)
                                a = a->next;
                            if (a == NULL || a->type != LIST_STRING || a->cur == NULL)
                                continue;
                            const char *concrete = (const char *)a->cur;

                            if (dst_alias->target_type_id != NULL &&
                                strcasecmp(dst_alias->target_type_id, param) == 0) {
                                free(dst_alias->target_type_id);
                                dst_alias->target_type_id = strdup(concrete);
                            }
                            if (dst_alias->pointer_type_id != NULL &&
                                strcasecmp(dst_alias->pointer_type_id, param) == 0) {
                                free(dst_alias->pointer_type_id);
                                dst_alias->pointer_type_id = strdup(concrete);
                            }
                            if (dst_alias->array_element_type_id != NULL &&
                                strcasecmp(dst_alias->array_element_type_id, param) == 0) {
                                free(dst_alias->array_element_type_id);
                                dst_alias->array_element_type_id = strdup(concrete);
                            }
                        }

                        /* Substitute type parameters in mangled names like
                         * "TFPGListEnumerator$T" -> "TFPGListEnumerator$TMyRecord" */
                        if (dst_alias->target_type_id != NULL) {
                            for (int pi = 0; pi < generic_decl->num_type_params && pi < arg_count; pi++) {
                                const char *param = generic_decl->type_parameters[pi];
                                ListNode_t *a = type_args;
                                for (int ai = 0; ai < pi && a != NULL; ai++)
                                    a = a->next;
                                if (a == NULL || a->type != LIST_STRING || a->cur == NULL)
                                    continue;
                                const char *concrete = (const char *)a->cur;
                                if (param == NULL || concrete == NULL) continue;
                                size_t param_len = strlen(param);
                                /* Look for "$T" pattern at the end or "$T$" in the middle */
                                char *pos = dst_alias->target_type_id;
                                while ((pos = strchr(pos, '$')) != NULL) {
                                    pos++; /* skip '$' */
                                    if (strncasecmp(pos, param, param_len) == 0 &&
                                        (pos[param_len] == '\0' || pos[param_len] == '$')) {
                                        /* Found "$T" — rebuild the string with "$TMyRecord" */
                                        size_t prefix_len2 = (pos - dst_alias->target_type_id);
                                        size_t suffix_len = strlen(pos + param_len);
                                        size_t concrete_len = strlen(concrete);
                                        char *new_target = malloc(prefix_len2 + concrete_len + suffix_len + 1);
                                        if (new_target != NULL) {
                                            memcpy(new_target, dst_alias->target_type_id, prefix_len2);
                                            memcpy(new_target + prefix_len2, concrete, concrete_len);
                                            memcpy(new_target + prefix_len2 + concrete_len,
                                                   pos + param_len, suffix_len + 1);
                                            free(dst_alias->target_type_id);
                                            dst_alias->target_type_id = new_target;
                                        }
                                        break;
                                    }
                                }
                            }
                        }

                        /* Also substitute qualified names: if target_type_id refers
                         * to another nested type of the same generic (e.g., "TFPGList.PT"),
                         * rewrite to "TFPGList$TMyRecord.PT" */
                        if (dst_alias->target_type_id != NULL &&
                            strncmp(dst_alias->target_type_id, generic_name, prefix_len) == 0 &&
                            dst_alias->target_type_id[prefix_len] == '.') {
                            const char *inner_suffix = dst_alias->target_type_id + prefix_len;
                            size_t new_target_len = strlen(specialized_name) + strlen(inner_suffix) + 1;
                            char *new_target = (char *)malloc(new_target_len);
                            if (new_target != NULL) {
                                snprintf(new_target, new_target_len, "%s%s", specialized_name, inner_suffix);
                                free(dst_alias->target_type_id);
                                dst_alias->target_type_id = new_target;
                            }
                        }
                    }

                    /* If the target_type_id is a mangled generic name like
                     * "TFPGListEnumerator$TMyRecord", ensure that specialization
                     * actually exists by triggering instantiation. */
                    int alias_type_set = 0;
                    if (clone->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *dst_alias = &clone->tree_data.type_decl_data.info.alias;
                        if (debug_env != NULL && dst_alias->target_type_id != NULL)
                            fprintf(stderr, "[KGPC] clone alias %s target_type_id=%s\n",
                                clone->tree_data.type_decl_data.id, dst_alias->target_type_id);
                        if (dst_alias->target_type_id != NULL) {
                            char *dollar = strchr(dst_alias->target_type_id, '$');
                            if (dollar != NULL) {
                                /* Extract base name and type args from mangled name */
                                size_t base_len = dollar - dst_alias->target_type_id;
                                char *inner_base = strndup(dst_alias->target_type_id, base_len);
                                GenericTypeDecl *inner_generic = generic_registry_find_decl(inner_base);
                                if (inner_generic != NULL) {
                                    /* Build type args from the suffix */
                                    const char *arg_str = dollar + 1;
                                    ListNode_t *inner_args = CreateListNode(strdup(arg_str), LIST_STRING);
                                    char *inner_spec_name = NULL;
                                    struct RecordType *inner_record =
                                        instantiate_generic_record(inner_base, inner_args, &inner_spec_name);
                                    if (inner_record != NULL) {
                                        /* Set the alias clone's inline_record_type to the
                                         * specialized record.  The semantic checker will
                                         * register both the mangled name (inner_record->type_id)
                                         * and the alias name in the symbol table (see line 7012+
                                         * in SemCheck.c).  This also enables constructor
                                         * resolution via record_info. */
                                        dst_alias->inline_record_type = inner_record;
                                        dst_alias->base_type = RECORD_TYPE;
                                        KgpcType *alias_type = create_record_type(inner_record);
                                        if (inner_record->is_class) {
                                            KgpcType *ptr = create_pointer_type(alias_type);
                                            kgpc_type_release(alias_type);
                                            alias_type = ptr;
                                        }
                                        clone->tree_data.type_decl_data.kgpc_type = alias_type;
                                        alias_type_set = 1;
                                        if (debug_env != NULL)
                                            fprintf(stderr, "[KGPC] triggered nested specialization %s\n",
                                                inner_spec_name);
                                    }
                                    if (inner_spec_name != NULL) free(inner_spec_name);
                                    destroy_list(inner_args);
                                }
                                free(inner_base);
                            }
                        }
                    }

                    /* Also handle procedure/function type declarations */
                    if (!alias_type_set && orig->tree_data.type_decl_data.kgpc_type != NULL) {
                        clone->tree_data.type_decl_data.kgpc_type = orig->tree_data.type_decl_data.kgpc_type;
                        kgpc_type_retain(clone->tree_data.type_decl_data.kgpc_type);
                    }

                    ListNode_t *node = CreateListNode(clone, LIST_TREE);
                    if (node != NULL) {
                        node->next = *type_list_out;
                        *type_list_out = node;
                    } else {
                        destroy_tree(clone);
                    }

                    /* If we triggered a nested specialization via inline_record_type,
                     * also emit method clones for the inner specialization. */
                    if (alias_type_set && subprograms_out != NULL)
                        append_specialized_method_clones(clone, subprograms_out);

                    if (debug_env != NULL)
                        fprintf(stderr, "[KGPC] cloned nested type %s -> %s\n", orig_id, clone->tree_data.type_decl_data.id);
                }
            }
        }
        cur = cur->next;
    }
}

void resolve_pending_generic_aliases(Tree_t *program_tree)
{
    PendingGenericAlias *cur = g_pending_generic_aliases;
    g_pending_generic_aliases = NULL;
    ListNode_t **clone_dest = NULL;
    ListNode_t **type_list = NULL;
    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    if (program_tree != NULL && program_tree->type == TREE_PROGRAM_TYPE) {
        clone_dest = &program_tree->tree_data.program_data.subprograms;
        type_list = &program_tree->tree_data.program_data.type_declaration;
    }

    while (cur != NULL) {
        PendingGenericAlias *next = cur->next;
        char *specialized_name = NULL;
        struct RecordType *record = instantiate_generic_record(cur->base_name, cur->type_args, &specialized_name);
        if (record != NULL && cur->decl != NULL &&
            cur->decl->type == TREE_TYPE_DECL &&
            cur->decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &cur->decl->tree_data.type_decl_data.info.alias;
            if (alias->inline_record_type != NULL)
                destroy_record_type(alias->inline_record_type);
            alias->inline_record_type = record;
            alias->base_type = RECORD_TYPE;
            if (cur->decl->tree_data.type_decl_data.kgpc_type == NULL) {
                KgpcType *inline_type = create_record_type(record);
                if (record->is_class) {
                    KgpcType *ptr = create_pointer_type(inline_type);
                    kgpc_type_release(inline_type);
                    inline_type = ptr;
                }
                cur->decl->tree_data.type_decl_data.kgpc_type = inline_type;
            }
            if (clone_dest != NULL)
                append_specialized_method_clones(cur->decl, clone_dest);

            /* Clone nested type declarations for this specialization */
            if (type_list != NULL && specialized_name != NULL && cur->base_name != NULL) {
                GenericTypeDecl *generic = generic_registry_find_decl(cur->base_name);
                if (generic != NULL && generic->nested_type_decls != NULL) {
                    clone_nested_types_for_specialization(generic, specialized_name,
                        cur->type_args, type_list, clone_dest);
                }
            }

            if (debug_env != NULL && cur->decl->tree_data.type_decl_data.id != NULL &&
                cur->base_name != NULL)
            {
                fprintf(stderr, "[KGPC] resolved generic alias %s for %s\n",
                        cur->decl->tree_data.type_decl_data.id, cur->base_name);
            }
        } else {
            fprintf(stderr, "ERROR: Failed to instantiate generic record %s for deferred alias.\n",
                    cur->base_name != NULL ? cur->base_name : "<unknown>");
            if (record != NULL)
                destroy_record_type(record);
        }
        if (specialized_name != NULL)
            free(specialized_name);
        if (cur->base_name != NULL)
            free(cur->base_name);
        if (cur->type_args != NULL)
            destroy_list(cur->type_args);
        free(cur);
        cur = next;
    }
}

static ListNode_t *collect_constructed_type_args(ast_t *args_node);
static int extract_constructed_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out);
static struct RecordType *instantiate_generic_record(const char *base_name, ListNode_t *type_args, char **specialized_name_out);
static char *mangle_specialized_type_name(const char *base_name, char **type_ids, int num_types);
static char *mangle_specialized_name_from_list(const char *base_name, ListNode_t *type_args);
static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types);
static void substitute_record_field(struct RecordField *field, GenericTypeDecl *generic_decl, char **arg_types);
static void record_generic_method_impl(const char *class_name, const char *method_name, ast_t *method_ast);
static Tree_t *instantiate_method_template(struct MethodTemplate *method_template, struct RecordType *record);

static ast_t *find_ast_node_type(ast_t *node, int typ)
{
    if (node == NULL)
        return NULL;
    if (node->typ == typ)
        return node;
    for (ast_t *child = node->child; child != NULL; child = child->next)
    {
        ast_t *found = find_ast_node_type(child, typ);
        if (found != NULL)
            return found;
    }
    return NULL;
}

static int is_method_decl_keyword(const char *sym_name)
{
    if (sym_name == NULL)
        return 0;
    return (strcasecmp(sym_name, "virtual") == 0 ||
            strcasecmp(sym_name, "override") == 0 ||
            strcasecmp(sym_name, "static") == 0 ||
            strcasecmp(sym_name, "abstract") == 0 ||
            strcasecmp(sym_name, "inline") == 0 ||
            strcasecmp(sym_name, "cdecl") == 0 ||
            strcasecmp(sym_name, "stdcall") == 0 ||
            strcasecmp(sym_name, "constructor") == 0 ||
            strcasecmp(sym_name, "destructor") == 0 ||
            strcasecmp(sym_name, "function") == 0 ||
            strcasecmp(sym_name, "procedure") == 0 ||
            strcasecmp(sym_name, "operator") == 0 ||
            strcasecmp(sym_name, "class") == 0);
}
static void append_specialized_method_clones(Tree_t *decl, ListNode_t **subprograms);
static void rewrite_method_impl_ast(ast_t *method_ast, struct RecordType *record);
static void substitute_generic_identifier_nodes(ast_t *node, struct RecordType *record);
static void append_subprogram_node(ListNode_t **dest, Tree_t *tree);
static int extract_generic_type_params(ast_t *type_param_list, char ***out_params);
static char *dup_first_identifier_in_node(ast_t *node);
static char *dup_first_identifier_in_node(ast_t *node);



/* Collect type argument identifiers from a \"specialize\" type argument list.
 * The argument subtree is simpler than CONSTRUCTED_TYPE: it consists of identifiers
 * separated by punctuation inside a PASCAL_T_NONE wrapper created by specialize_args. */
static ListNode_t *collect_specialize_type_args(ast_t *args_node) {
    if (args_node == NULL)
        return NULL;

    ListBuilder builder;
    list_builder_init(&builder);

    ast_t *cursor = args_node;
    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;
        if (node != NULL && node->typ == PASCAL_T_IDENTIFIER) {
            char *dup = dup_symbol(node);
            if (dup != NULL)
                list_builder_append(&builder, dup, LIST_STRING);
        }
        cursor = cursor->next;
    }

    ListNode_t *result = list_builder_finish(&builder);
    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] collect_constructed_type_args result=%p\n", (void *)result);
    return result;
}

/* Extract generic base name and type arguments from a \"specialize\" type
 * specification, such as:
 *   specialize TFPGList<TMyRecord>
 * The input node may be either the PASCAL_T_TYPE_SPEC wrapper or its child. */
static int extract_specialize_type_info(ast_t *spec_node, char **base_name_out, ListNode_t **type_args_out) {
    if (base_name_out != NULL)
        *base_name_out = NULL;
    if (type_args_out != NULL)
        *type_args_out = NULL;

    if (spec_node == NULL)
        return 0;

    ast_t *node = spec_node;
    if (node->typ == PASCAL_T_TYPE_SPEC && node->child != NULL)
        node = node->child;
    node = unwrap_pascal_node(node);
    if (node == NULL)
        return 0;

    /* Expect first child to be the "specialize" keyword */
    if (node->sym == NULL || node->sym->name == NULL ||
        strcasecmp(node->sym->name, "specialize") != 0)
        return 0;

    /* Next sibling should be the generic type identifier (possibly qualified) */
    ast_t *base_node = node->next;
    while (base_node != NULL && base_node->typ == PASCAL_T_NONE)
        base_node = base_node->child;
    if (base_node == NULL)
        return 0;

    char *base_name = dup_symbol(base_node);
    if (base_name == NULL)
        return 0;

    /* Optional argument list node (created by specialize_args) */
    ListNode_t *type_args = NULL;
    ast_t *args_node = base_node->next;
    if (args_node != NULL) {
        type_args = collect_specialize_type_args(args_node);
        if (type_args == NULL) {
            free(base_name);
            return 0;
        }
    }

    if (base_name_out != NULL)
        *base_name_out = base_name;
    else
        free(base_name);

    if (type_args_out != NULL)
        *type_args_out = type_args;
    else
        destroy_list(type_args);

    return 1;
}

static struct Expression *make_varid_from_qualified_ast(ast_t *node)
{
    if (node == NULL)
        return NULL;
    QualifiedIdent *qid = qualified_ident_from_ast(node);
    if (qid == NULL)
        return NULL;
    char *joined = qualified_ident_join(qid, ".");
    struct Expression *expr = mk_varid(node->line, joined);
    if (expr != NULL)
    {
        if (expr->id_ref != NULL)
            qualified_ident_free(expr->id_ref);
        expr->id_ref = qid;
        qid = NULL;
    }
    if (qid != NULL)
        qualified_ident_free(qid);
    return expr;
}

static struct Expression *convert_case_label_expression(ast_t *node) {
    ast_t *unwrapped = unwrap_pascal_node(node);
    if (unwrapped != NULL &&
        (unwrapped->typ == PASCAL_T_QUALIFIED_IDENTIFIER ||
         unwrapped->typ == PASCAL_T_MEMBER_ACCESS ||
         unwrapped->typ == PASCAL_T_IDENTIFIER))
    {
        struct Expression *qualified = make_varid_from_qualified_ast(unwrapped);
        if (qualified != NULL)
            return qualified;
    }

    struct Expression *expr = convert_expression(node);
    if (expr != NULL && expr->type == EXPR_STRING && expr->expr_data.string != NULL) {
        const char *value = expr->expr_data.string;
        if (value[0] != '\0' && value[1] == '\0') {
            unsigned int code = (unsigned char)value[0];
            int line = (node != NULL) ? node->line : 0;
            destroy_expr(expr);
            return mk_charcode(line, code);
        }
    }
    return expr;
}

static void append_case_label(ListBuilder *builder, ast_t *label_node) {
    if (builder == NULL || label_node == NULL)
        return;

    ast_t *unwrapped = unwrap_pascal_node(label_node);
    if (unwrapped == NULL)
        return;

    if (unwrapped->typ == PASCAL_T_CASE_LABEL) {
        append_case_label(builder, unwrapped->child);
        return;
    }

    if (unwrapped->typ == PASCAL_T_RANGE) {
        ast_t *lower_node = unwrap_pascal_node(unwrapped->child);
        ast_t *upper_node = NULL;
        if (lower_node != NULL)
            upper_node = unwrap_pascal_node(lower_node->next);

        struct Expression *lower_expr = convert_case_label_expression(lower_node);
        struct Expression *upper_expr = convert_case_label_expression(upper_node);

        if (lower_expr != NULL && upper_expr != NULL) {
            struct SetElement *range = mk_set_element(lower_expr, upper_expr);
            list_builder_append(builder, range, LIST_SET_ELEMENT);
        } else {
            if (lower_expr != NULL)
                destroy_expr(lower_expr);
            if (upper_expr != NULL)
                destroy_expr(upper_expr);
        }
        return;
    }

    struct Expression *label_expr = convert_case_label_expression(unwrapped);
    if (label_expr != NULL)
        list_builder_append(builder, label_expr, LIST_EXPR);
}

static void extend_list(ListNode_t **dest, ListNode_t *src) {
    if (src == NULL) {
        return;
    }
    if (*dest == NULL) {
        *dest = src;
        return;
    }
    ListNode_t *cur = *dest;
    while (cur->next != NULL) {
        cur = cur->next;
    }
    cur->next = src;
}

static ast_t *unwrap_pascal_node(ast_t *node) {
    ast_t *cur = node;
    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_NONE:
        case PASCAL_T_THEN:
        case PASCAL_T_ELSE:
        case PASCAL_T_DO:
            if (cur->child != NULL) {
                cur = cur->child;
                continue;
            }
            break;
        default:
            break;
        }
        break;
    }
    return cur;
}

/* Under {$H-}, bare 'string' mapped to STRING_TYPE should become
 * SHORTSTRING_TYPE.  Only remap when the name is literally "string"
 * (case-insensitive) so that explicit AnsiString/UnicodeString are
 * left untouched. */
static int apply_shortstring_mode(int type_tag, const char *name) {
    if (type_tag == STRING_TYPE && pascal_frontend_default_shortstring() &&
        name != NULL && strcasecmp(name, "string") == 0)
        return SHORTSTRING_TYPE;
    return type_tag;
}

static int map_type_name(const char *name, char **type_id_out) {
    if (name == NULL) {
        return UNKNOWN_TYPE;
    }
    if (strcasecmp(name, "integer") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("integer");
        return INT_TYPE;
    }
    if (strcasecmp(name, "longint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("longint");
        return LONGINT_TYPE;
    }
    if (strcasecmp(name, "int64") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("int64");
        return INT64_TYPE;
    }
    if (strcasecmp(name, "int32") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("int32");
        return LONGINT_TYPE;
    }
    if (strcasecmp(name, "int16") == 0 || strcasecmp(name, "int8") == 0 ||
        strcasecmp(name, "shortint") == 0 || strcasecmp(name, "smallint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return INT_TYPE;
    }
    if (strcasecmp(name, "byte") == 0 || strcasecmp(name, "uint8") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return BYTE_TYPE;
    }
    if (strcasecmp(name, "word") == 0 || strcasecmp(name, "uint16") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return WORD_TYPE;
    }
    if (strcasecmp(name, "longword") == 0 || strcasecmp(name, "dword") == 0 ||
        strcasecmp(name, "cardinal") == 0 || strcasecmp(name, "uint32") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return LONGWORD_TYPE;
    }
    if (strcasecmp(name, "qword") == 0 || strcasecmp(name, "uint64") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return QWORD_TYPE;
    }
    if (strcasecmp(name, "nativeint") == 0 ||
        strcasecmp(name, "sizeint") == 0 ||
        strcasecmp(name, "ptrint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return INT64_TYPE;
    }
    if (strcasecmp(name, "nativeuint") == 0 ||
        strcasecmp(name, "sizeuint") == 0 ||
        strcasecmp(name, "ptruint") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return INT64_TYPE;
    }
    if (strcasecmp(name, "real") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("real");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "float") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("Float");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "double") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("double");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "extended") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("Extended");
        return EXTENDED_TYPE;
    }
    if (strcasecmp(name, "valreal") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("ValReal");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "openstring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("shortstring");
        return SHORTSTRING_TYPE;
    }
    if (strcasecmp(name, "string") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("string");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "ansistring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("AnsiString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "widestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("WideString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "shortstring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("shortstring");
        return SHORTSTRING_TYPE;
    }
    /* RawByteString and UnicodeString need to preserve their original names
     * for correct name mangling of overloaded procedures */
    if (strcasecmp(name, "rawbytestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("RawByteString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "unicodestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("UnicodeString");
        return STRING_TYPE;
    }
    if (strcasecmp(name, "text") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("text");
        return TEXT_TYPE;
    }
    /* C-style char aliases from ctypes/sysutils */
    if (strcasecmp(name, "cchar") == 0 || strcasecmp(name, "cschar") == 0 ||
        strcasecmp(name, "cuchar") == 0 || strcasecmp(name, "ansichar") == 0)
    {
        if (type_id_out != NULL)
            *type_id_out = strdup(name);
        return CHAR_TYPE;
    }
    if (strcasecmp(name, "char") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("char");
        return CHAR_TYPE;
    }
    /* WideChar is a 2-byte character type; resolve via symbol table for correct size. */
    if (strcasecmp(name, "widechar") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("widechar");
        return UNKNOWN_TYPE;
    }
    if (strcasecmp(name, "file") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("file");
        return FILE_TYPE;
    }
    if (strcasecmp(name, "single") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("single");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "boolean") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("boolean");
        return BOOL;
    }
    /* Procedure and Function as bare type names (no parameters) - procedure pointers */
    if (strcasecmp(name, "procedure") == 0 || strcasecmp(name, "function") == 0) {
        /* Don't set type_id_out - PROCEDURE type tag is sufficient */
        return PROCEDURE;
    }
    if (type_id_out != NULL) {
        *type_id_out = strdup(name);
    }
    return UNKNOWN_TYPE;
}

static int helper_self_param_is_var(const char *base_type_id, struct SymTab *symtab)
{
    if (base_type_id == NULL)
        return 0;
    /* Real/Single/Double/Extended: helper Self is passed by value. */
    int type_tag = map_type_name(base_type_id, NULL);
    if (is_real_family_type(type_tag))
        return 0;
    /* String types are heap-allocated pointers — by value is correct. */
    if (type_tag == STRING_TYPE || type_tag == SHORTSTRING_TYPE ||
        type_tag == CHAR_TYPE)
        return 0;
    /* Class and pointer types: Self is already a pointer. */
    if (symtab != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, base_type_id) != 0 && type_node != NULL)
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
    }
    /* Integer/ordinal value types: Self must be passed by reference
     * so that mutations (Self := Self or ...) persist at the call site. */
    return 1;
}

static struct TypeAlias *helper_self_real_alias(const char *base_type_id)
{
    if (base_type_id == NULL)
        return NULL;

    int type_tag = map_type_name(base_type_id, NULL);
    if (!is_real_family_type(type_tag))
        return NULL;

    struct TypeAlias *alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
    if (alias == NULL)
        return NULL;

    alias->base_type = type_tag;
    if (pascal_identifier_equals(base_type_id, "Single"))
        alias->storage_size = 4;
    else if (pascal_identifier_equals(base_type_id, "Extended"))
        alias->storage_size = 10;
    else
        alias->storage_size = 8;

    return alias;
}

static struct RecordType *convert_record_type(ast_t *record_node);
static struct Expression *convert_expression(ast_t *expr_node);
static struct Expression *convert_member_access(ast_t *node);
static struct Expression *convert_field_width_expr(ast_t *field_width_node);
static ListNode_t *convert_expression_list(ast_t *arg_node);
static ListNode_t *convert_statement_list(ast_t *stmt_list_node);
static struct Statement *build_nested_with_statements(int line,
                                                      ast_t *context_node,
                                                      struct Statement *body_stmt);
static void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest,
    ListNode_t **subprograms, ListNode_t **const_decls, ListBuilder *var_builder,
    const char *parent_type_name);

static ast_t *unwrap_record_constructor_elem(ast_t *elem)
{
    ast_t *unwrapped = unwrap_pascal_node(elem);
    if (unwrapped != NULL && unwrapped->typ == PASCAL_T_STATEMENT &&
        unwrapped->child != NULL)
        unwrapped = unwrap_pascal_node(unwrapped->child);
    return unwrapped;
}

static int tuple_is_record_constructor(ast_t *tuple_node)
{
    if (tuple_node == NULL || tuple_node->typ != PASCAL_T_TUPLE)
        return 0;

    int has_fields = 0;
    int has_record_like = 0;
    int debug_tuple = (kgpc_getenv("KGPC_DEBUG_RECORD_TUPLE") != NULL);
    int debug_this = debug_tuple && tuple_node->line >= 2165 && tuple_node->line <= 2175;
    if (debug_this)
    {
        fprintf(stderr, "[KGPC] tuple_is_record_constructor line=%d\n", tuple_node->line);
    }
    for (ast_t *elem = tuple_node->child; elem != NULL; elem = elem->next)
    {
        ast_t *unwrapped = unwrap_record_constructor_elem(elem);
        if (unwrapped == NULL)
            continue;
        if (unwrapped->typ == PASCAL_T_NONE && unwrapped->child == NULL)
            continue;
        if (debug_this)
        {
            fprintf(stderr, "  elem typ=%d\n", unwrapped->typ);
        }
        if (unwrapped->typ == PASCAL_T_ASSIGNMENT || unwrapped->typ == PASCAL_T_FIELD_WIDTH)
        {
            has_fields = 1;
            has_record_like = 1;
            continue;
        }
        if (unwrapped->typ == PASCAL_T_ASSIGNMENT || unwrapped->typ == PASCAL_T_FIELD_WIDTH)
            has_record_like = 1;
        return 0;
    }
    if (debug_this)
        fprintf(stderr, "  has_fields=%d\n", has_fields);
    if (debug_tuple && !has_fields && has_record_like)
    {
        fprintf(stderr, "[KGPC] tuple skipped record constructor at line=%d\n", tuple_node->line);
    }
    return has_fields;
}

static struct Expression *convert_record_constructor_expr(ast_t *expr_node)
{
    if (expr_node == NULL)
        return NULL;

    ListNode_t *fields = NULL;
    ListNode_t *fields_tail = NULL;
    int field_count = 0;

    for (ast_t *field_assignment = expr_node->child;
         field_assignment != NULL;
         field_assignment = field_assignment->next)
    {
        ast_t *assignment_node = unwrap_record_constructor_elem(field_assignment);
        if (assignment_node == NULL)
            continue;

        ast_t *field_name_node = NULL;
        ast_t *field_value_node = NULL;

        if (assignment_node->typ == PASCAL_T_ASSIGNMENT)
        {
            field_name_node = assignment_node->child;
            field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
        }
        else if (assignment_node->typ == PASCAL_T_FIELD_WIDTH)
        {
            field_name_node = assignment_node->child;
            field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
        }
        else
        {
            continue;
        }
        if (field_name_node == NULL || field_value_node == NULL ||
            field_name_node->sym == NULL || field_name_node->sym->name == NULL)
        {
            fprintf(stderr, "ERROR: Malformed record constructor field at line %d.\n",
                expr_node->line);
            goto record_ctor_cleanup;
        }

        struct Expression *field_value = NULL;
        if (field_value_node != NULL && field_value_node->typ == PASCAL_T_TUPLE &&
            tuple_is_record_constructor(field_value_node))
        {
            field_value = convert_record_constructor_expr(field_value_node);
        }
        else
        {
            field_value = convert_expression(field_value_node);
        }
        if (field_value == NULL)
        {
            fprintf(stderr, "ERROR: Failed to convert record constructor field value at line %d.\n",
                expr_node->line);
            goto record_ctor_cleanup;
        }

        struct RecordConstructorField *field =
            (struct RecordConstructorField *)calloc(1, sizeof(struct RecordConstructorField));
        if (field == NULL)
            goto record_ctor_cleanup;
        field->field_id = strdup(field_name_node->sym->name);
        field->value = field_value;

        ListNode_t *node = CreateListNode(field, LIST_UNSPECIFIED);
        if (node == NULL)
            goto record_ctor_cleanup;
        if (fields == NULL)
        {
            fields = node;
            fields_tail = node;
        }
        else
        {
            fields_tail->next = node;
            fields_tail = node;
        }
        ++field_count;
    }

    return mk_record_constructor(expr_node->line, fields, field_count);

record_ctor_cleanup:
    if (fields != NULL)
    {
        ListNode_t *cur = fields;
        while (cur != NULL)
        {
            struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
            if (field != NULL)
            {
                if (field->value != NULL)
                    destroy_expr(field->value);
                free(field->field_id);
                free(field->field_type_id);
                free(field->array_element_type_id);
                free(field);
            }
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
        }
    }
    return NULL;
}

/* Helper function to resolve enum literal identifier to its ordinal value
 * by searching through AST type section.
 * Returns the ordinal value if found (>= 0), -1 if not found.
 */
static int resolve_enum_ordinal_from_ast(const char *identifier, ast_t *type_section) {
    if (identifier == NULL || type_section == NULL)
        return -1;
    
    /* Iterate through type declarations */
    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            /* Find the type spec within the type declaration */
            ast_t *type_spec_node = type_decl->child;
            if (type_spec_node != NULL)
                type_spec_node = type_spec_node->next; /* Skip the identifier */
            
            while (type_spec_node != NULL && 
                   type_spec_node->typ != PASCAL_T_TYPE_SPEC &&
                   type_spec_node->typ != PASCAL_T_ENUMERATED_TYPE) {
                type_spec_node = type_spec_node->next;
            }
            
            /* Unwrap TYPE_SPEC if needed */
            ast_t *spec = type_spec_node;
            if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
                spec = spec->child;
            
            /* Check if it's an enumerated type */
            if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
                int ordinal = 0;
                ast_t *literal = spec->child;
                while (literal != NULL) {
                    ast_t *literal_id = literal;
                    if (literal_id != NULL && literal_id->typ == PASCAL_T_ASSIGNMENT)
                        literal_id = literal_id->child;
                    if (literal_id != NULL && literal_id->typ == PASCAL_T_IDENTIFIER &&
                        literal_id->sym != NULL) {
                        if (strcmp(literal_id->sym->name, identifier) == 0) {
                            return ordinal; /* Found it! Return the ordinal value */
                        }
                    }
                    ordinal++;
                    literal = literal->next;
                }
            }
        }
        type_decl = type_decl->next;
    }
    
    return -1; /* Not found */
}

/* Helper to resolve an enum literal within a specific enumerated type.
 * Returns ordinal value if found, -1 otherwise. */
static int resolve_enum_literal_in_type(const char *type_name, const char *literal, ast_t *type_section) {
    if (type_name == NULL || literal == NULL || type_section == NULL)
        return -1;

    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            ast_t *id_node = type_decl->child;
            if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER &&
                id_node->sym != NULL && id_node->sym->name != NULL &&
                strcasecmp(id_node->sym->name, type_name) == 0) {
                ast_t *type_spec_node = id_node->next;
                while (type_spec_node != NULL &&
                       type_spec_node->typ != PASCAL_T_TYPE_SPEC &&
                       type_spec_node->typ != PASCAL_T_ENUMERATED_TYPE) {
                    type_spec_node = type_spec_node->next;
                }
                ast_t *spec = type_spec_node;
                if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
                    spec = spec->child;
                if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
                    int ordinal = 0;
                    for (ast_t *lit = spec->child; lit != NULL; lit = lit->next) {
                        ast_t *lit_id = lit;
                        if (lit_id != NULL && lit_id->typ == PASCAL_T_ASSIGNMENT)
                            lit_id = lit_id->child;
                        if (lit_id != NULL && lit_id->typ == PASCAL_T_IDENTIFIER &&
                            lit_id->sym != NULL &&
                            lit_id->sym->name != NULL &&
                            strcasecmp(lit_id->sym->name, literal) == 0) {
                            return ordinal;
                        }
                        ordinal++;
                    }
                }
                return -1;
            }
        }
        type_decl = type_decl->next;
    }

    return -1;
}

/* Helper function to resolve the range of an enumerated type by type name.
 * For example, if color = (red, blue, yellow), then resolve_enum_type_range_from_ast("color", ...)
 * will set out_start=0 and out_end=2 (for 3 values: red, blue, yellow).
 * Returns 0 on success, -1 if the type is not found or is not an enum type.
 */
static int resolve_enum_type_range_from_ast(const char *type_name, ast_t *type_section, int *out_start, int *out_end) {
    if (type_name == NULL || type_section == NULL || out_start == NULL || out_end == NULL)
        return -1;
    
    /* Iterate through type declarations to find the matching type name */
    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            /* Get the type name (first child) */
            ast_t *type_id = type_decl->child;
            if (type_id != NULL && type_id->typ == PASCAL_T_IDENTIFIER && type_id->sym != NULL) {
                if (strcasecmp(type_id->sym->name, type_name) == 0) {
                    /* Found the type - now check if it's an enumerated type */
                    ast_t *type_spec_node = type_id->next;
                    
                    while (type_spec_node != NULL &&
                           type_spec_node->typ != PASCAL_T_TYPE_SPEC &&
                           type_spec_node->typ != PASCAL_T_ENUMERATED_TYPE &&
                           type_spec_node->typ != PASCAL_T_RANGE_TYPE) {
                        type_spec_node = type_spec_node->next;
                    }

                    /* Unwrap TYPE_SPEC if needed */
                    ast_t *spec = type_spec_node;
                    if (spec != NULL && spec->typ == PASCAL_T_TYPE_SPEC && spec->child != NULL)
                        spec = spec->child;

                    /* Check if it's an enumerated type */
                    if (spec != NULL && spec->typ == PASCAL_T_ENUMERATED_TYPE) {
                        /* Count the enum values */
                        int count = 0;
                        ast_t *literal = spec->child;
                        while (literal != NULL) {
                            if (literal->typ == PASCAL_T_IDENTIFIER ||
                                literal->typ == PASCAL_T_ASSIGNMENT)
                                count++;
                            literal = literal->next;
                        }

                        if (count > 0) {
                            *out_start = 0;
                            *out_end = count - 1;
                            enum_registry_add(type_name, 0, count - 1);
                            return 0; /* Success */
                        }
                    }

                    /* Check if it's a subrange type (e.g., 0..NUM_REGS - 1) */
                    if (spec != NULL && spec->typ == PASCAL_T_RANGE_TYPE) {
                        ast_t *lower = spec->child;
                        ast_t *upper = (lower != NULL) ? lower->next : NULL;
                        int low_val = 0, high_val = 0;
                        if (evaluate_const_int_expr(lower, &low_val, 0) == 0 &&
                            evaluate_const_int_expr(upper, &high_val, 0) == 0) {
                            *out_start = low_val;
                            *out_end = high_val;
                            enum_registry_add(type_name, low_val, high_val);
                            return 0; /* Success */
                        }
                    }

                    /* Found the type but it's not an enum or resolved subrange */
                    return -1;
                }
            }
        }
        type_decl = type_decl->next;
    }
    
    return -1; /* Type not found */
}

/* Helper function to resolve a const integer identifier from the same const section.
 * This is needed for array ranges like [C_Low .. C_High].
 * Returns the integer value if found, otherwise returns fallback_value.
 * Note: Only resolves simple integer consts; does not handle forward references
 * or complex const expressions.
 */
static int resolve_const_int_from_ast_internal(const char *identifier, ast_t *const_section,
                                               int fallback_value, int depth);
static int evaluate_const_int_expr(ast_t *expr, int *out_value, int depth);
static int resolve_const_int_in_node(const char *identifier, ast_t *node,
                                     ast_t *const_section, int *out_value, int depth) {
    if (node == NULL)
        return -1;
    if (depth > 32)
        return -1;

    if (node->typ == PASCAL_T_CONST_DECL) {
        ast_t *id_node = node->child;
        if (id_node != NULL && id_node->sym != NULL) {
            if (strcasecmp(id_node->sym->name, identifier) == 0) {
                ast_t *value_node = id_node->next;
                if (value_node != NULL && value_node->typ == PASCAL_T_TYPE_SPEC)
                    value_node = value_node->next;
                value_node = unwrap_pascal_node(value_node);

                if (value_node != NULL && value_node->sym != NULL) {
                    char *endptr;
                    long val = strtol(value_node->sym->name, &endptr, 10);
                    if (*endptr == '\0') {
                        if (val >= INT_MIN && val <= INT_MAX) {
                            *out_value = (int)val;
                            return 0;
                        }
                    }

                    if (value_node->typ == PASCAL_T_IDENTIFIER &&
                        strcasecmp(value_node->sym->name, identifier) != 0) {
                        int resolved = resolve_const_int_from_ast_internal(value_node->sym->name,
                                                                           const_section,
                                                                           INT_MIN,
                                                                           depth + 1);
                        if (resolved != INT_MIN) {
                            *out_value = resolved;
                            return 0;
                        }
                    }
                } else if (value_node != NULL && value_node->typ == PASCAL_T_NEG &&
                           value_node->child != NULL) {
                    ast_t *inner = unwrap_pascal_node(value_node->child);
                    if (inner != NULL && inner->sym != NULL) {
                        int resolved = resolve_const_int_from_ast_internal(inner->sym->name,
                                                                           const_section,
                                                                           INT_MIN,
                                                                           depth + 1);
                        if (resolved != INT_MIN) {
                            *out_value = -resolved;
                            return 0;
                        }
                    }
                }
            }
        }
    }

    if (resolve_const_int_in_node(identifier, node->child, const_section, out_value, depth) == 0)
        return 0;
    if (resolve_const_int_in_node(identifier, node->next, const_section, out_value, depth) == 0)
        return 0;
    return -1;
}

static int resolve_const_string_in_node(const char *identifier, ast_t *node,
                                        ast_t *const_section, const char **out_value, int depth);
static int resolve_const_string_from_ast_internal(const char *identifier, ast_t *const_section,
                                                  const char **out_value, int depth);

typedef struct AstStringValue {
    char *data;
    size_t len;
} AstStringValue;

static void ast_string_value_reset(AstStringValue *value)
{
    if (value == NULL)
        return;
    free(value->data);
    value->data = NULL;
    value->len = 0;
}

static int ast_string_value_assign_bytes(AstStringValue *value, const char *data, size_t len)
{
    char *copy = NULL;
    if (value == NULL)
        return -1;
    if (len > 0)
    {
        copy = (char *)malloc(len);
        if (copy == NULL)
            return -1;
        memcpy(copy, data, len);
    }
    ast_string_value_reset(value);
    value->data = copy;
    value->len = len;
    return 0;
}

static int ast_string_value_append(AstStringValue *dst, const AstStringValue *src)
{
    char *combined = NULL;
    if (dst == NULL || src == NULL)
        return -1;
    if (src->len == 0)
        return 0;
    combined = (char *)malloc(dst->len + src->len);
    if (combined == NULL)
        return -1;
    if (dst->len > 0 && dst->data != NULL)
        memcpy(combined, dst->data, dst->len);
    memcpy(combined + dst->len, src->data, src->len);
    free(dst->data);
    dst->data = combined;
    dst->len += src->len;
    return 0;
}

static int parse_ast_char_code(ast_t *node, unsigned int *out_value)
{
    const char *literal;
    const char *digits;
    int base = 10;

    if (node == NULL || out_value == NULL)
        return -1;

    node = unwrap_pascal_node(node);
    if (node == NULL || node->typ != PASCAL_T_CHAR_CODE)
        return -1;

    literal = (node->sym != NULL) ? node->sym->name : NULL;
    if (literal == NULL)
        return -1;

    digits = literal;
    if (*digits == '#')
        ++digits;
    if (*digits == '$')
    {
        base = 16;
        ++digits;
    }
    if (*digits == '\0')
        return -1;

    {
        char *endptr = NULL;
        long parsed = strtol(digits, &endptr, base);
        if (endptr == NULL || *endptr != '\0' || parsed < 0)
            return -1;
        *out_value = (unsigned int)parsed;
    }
    return 0;
}

static int evaluate_const_string_ast(ast_t *node, ast_t *const_section,
                                     AstStringValue *out_value, int depth)
{
    ast_t *unwrapped;
    if (node == NULL || out_value == NULL || depth > 32)
        return -1;

    unwrapped = unwrap_pascal_node(node);
    if (unwrapped == NULL)
        return -1;

    switch (unwrapped->typ)
    {
        case PASCAL_T_STRING:
        case PASCAL_T_CHAR:
        {
            const char *value = (unwrapped->sym != NULL) ? unwrapped->sym->name : NULL;
            if (value == NULL)
                return -1;
            return ast_string_value_assign_bytes(out_value, value, strlen(value));
        }

        case PASCAL_T_CHAR_CODE:
        {
            unsigned int ch = 0;
            unsigned char byte;
            if (parse_ast_char_code(unwrapped, &ch) != 0)
                return -1;
            byte = (unsigned char)(ch & 0xffu);
            return ast_string_value_assign_bytes(out_value, (const char *)&byte, 1);
        }

        case PASCAL_T_IDENTIFIER:
        {
            const char *resolved = NULL;
            if (unwrapped->sym != NULL && unwrapped->sym->name != NULL &&
                resolve_const_string_from_ast_internal(unwrapped->sym->name, const_section,
                                                       &resolved, depth + 1) == 0 &&
                resolved != NULL)
            {
                return ast_string_value_assign_bytes(out_value, resolved, strlen(resolved));
            }
            return -1;
        }

        case PASCAL_T_ADD:
        {
            AstStringValue left = {0};
            AstStringValue right = {0};
            ast_t *lhs = unwrapped->child;
            ast_t *rhs = (lhs != NULL) ? lhs->next : NULL;
            int result = -1;

            if (evaluate_const_string_ast(lhs, const_section, &left, depth + 1) != 0)
                goto concat_cleanup;
            if (evaluate_const_string_ast(rhs, const_section, &right, depth + 1) != 0)
                goto concat_cleanup;
            if (ast_string_value_assign_bytes(out_value, left.data, left.len) != 0)
                goto concat_cleanup;
            if (ast_string_value_append(out_value, &right) != 0)
            {
                ast_string_value_reset(out_value);
                goto concat_cleanup;
            }
            result = 0;

concat_cleanup:
            ast_string_value_reset(&left);
            ast_string_value_reset(&right);
            return result;
        }

        default:
            return -1;
    }
}

static int type_info_targets_char_array(const TypeInfo *type_info, int *is_widechar_out)
{
    int is_char_array_target = 0;
    int is_widechar_array_target = 0;

    if (is_widechar_out != NULL)
        *is_widechar_out = 0;
    if (type_info == NULL)
        return 0;

    is_char_array_target = (type_info->element_type == CHAR_TYPE);
    is_widechar_array_target = (type_info->element_type_id != NULL &&
                                strcasecmp(type_info->element_type_id, "widechar") == 0);
    if (!is_char_array_target && type_info->element_type_id != NULL &&
        (strcasecmp(type_info->element_type_id, "char") == 0 ||
         strcasecmp(type_info->element_type_id, "ansichar") == 0))
    {
        is_char_array_target = 1;
    }

    if (is_widechar_out != NULL)
        *is_widechar_out = is_widechar_array_target;
    return is_char_array_target || is_widechar_array_target;
}

static struct Expression *mk_const_array_element_lhs(int line_num, const char *array_name,
    int outer_index, int inner_index, int is_multidim)
{
    struct Expression *base_expr = mk_varid(line_num, strdup(array_name));
    struct Expression *outer_index_expr = mk_inum(line_num, outer_index);
    struct Expression *inner_index_expr = mk_inum(line_num, inner_index);

    if (is_multidim)
    {
        struct Expression *lhs = mk_arrayaccess(line_num, base_expr, outer_index_expr);
        lhs->expr_data.array_access_data.extra_indices =
            CreateListNode(inner_index_expr, LIST_EXPR);
        return lhs;
    }

    {
        struct Expression *outer_access = mk_arrayaccess(line_num, base_expr, outer_index_expr);
        return mk_arrayaccess(line_num, outer_access, inner_index_expr);
    }
}

static int resolve_const_string_in_section(const char *identifier, ast_t *const_section,
                                           const char **out_value, int depth) {
    if (identifier == NULL || const_section == NULL || out_value == NULL)
        return -1;
    return resolve_const_string_in_node(identifier, const_section->child, const_section, out_value, depth);
}

static int resolve_const_string_from_ast_internal(const char *identifier, ast_t *const_section,
                                                  const char **out_value, int depth) {
    if (identifier == NULL || const_section == NULL || out_value == NULL)
        return -1;
    if (depth > 32)
        return -1;

    int found = 0;
    if (g_const_sections != NULL) {
        for (ListNode_t *cur = g_const_sections; cur != NULL; cur = cur->next) {
            if (resolve_const_string_in_section(identifier, (ast_t *)cur->cur, out_value, depth) == 0) {
                found = 1;
                break;
            }
        }
    } else {
        if (resolve_const_string_in_section(identifier, const_section, out_value, depth) == 0)
            found = 1;
    }

    return found ? 0 : -1;
}

static int resolve_const_string_in_node(const char *identifier, ast_t *node,
                                        ast_t *const_section, const char **out_value, int depth) {
    if (node == NULL)
        return -1;
    if (depth > 32)
        return -1;

    node = unwrap_pascal_node(node);
    if (node == NULL)
        return -1;

    if (node->typ == PASCAL_T_CONST_DECL) {
        ast_t *id_node = node->child;
        if (id_node != NULL && id_node->sym != NULL) {
            if (strcasecmp(id_node->sym->name, identifier) == 0) {
                ast_t *value_node = id_node->next;
                if (value_node != NULL && value_node->typ == PASCAL_T_TYPE_SPEC)
                    value_node = value_node->next;
                value_node = unwrap_pascal_node(value_node);

                if (value_node != NULL && value_node->typ == PASCAL_T_STRING &&
                    value_node->sym != NULL && value_node->sym->name != NULL) {
                    *out_value = value_node->sym->name;
                    return 0;
                }

                if (value_node != NULL && value_node->typ == PASCAL_T_IDENTIFIER &&
                    value_node->sym != NULL &&
                    strcasecmp(value_node->sym->name, identifier) != 0) {
                    if (resolve_const_string_from_ast_internal(value_node->sym->name, const_section,
                                                               out_value, depth + 1) == 0)
                        return 0;
                }
            }
        }
    }

    if (resolve_const_string_in_node(identifier, node->child, const_section, out_value, depth) == 0)
        return 0;
    if (resolve_const_string_in_node(identifier, node->next, const_section, out_value, depth) == 0)
        return 0;
    return -1;
}

static int resolve_enum_type_range_in_section_chain(const char *type_name, ast_t *section_root,
                                                    int *out_start, int *out_end) {
    if (type_name == NULL || section_root == NULL || out_start == NULL || out_end == NULL)
        return -1;

    ast_t *section = section_root->child;
    while (section != NULL) {
        ast_t *node = unwrap_pascal_node(section);
        for (ast_t *cursor = node; cursor != NULL;
             cursor = (section->typ == PASCAL_T_NONE) ? cursor->next : NULL) {
            if (cursor->typ == PASCAL_T_TYPE_SECTION) {
                if (resolve_enum_type_range_from_ast(type_name, cursor, out_start, out_end) == 0)
                    return 0;
            }
        }
        section = section->next;
    }

    return -1;
}

static int resolve_enum_type_range_with_fallback(const char *type_name, ast_t *type_section,
                                                 int *out_start, int *out_end) {
    if (resolve_enum_type_range_from_ast(type_name, type_section, out_start, out_end) == 0)
        return 0;
    if (resolve_enum_type_range_in_section_chain(type_name, g_interface_section_ast, out_start, out_end) == 0)
        return 0;
    if (resolve_enum_type_range_in_section_chain(type_name, g_implementation_section_ast, out_start, out_end) == 0)
        return 0;
    /* Fallback: check the cross-unit enum registry */
    if (enum_registry_lookup(type_name, out_start, out_end) == 0)
        return 0;
    return -1;
}

static ast_t *find_type_decl_in_section(ast_t *type_section, const char *type_name) {
    if (type_section == NULL || type_name == NULL)
        return NULL;

    for (ast_t *type_decl = type_section->child; type_decl != NULL; type_decl = type_decl->next) {
        if (type_decl->typ != PASCAL_T_TYPE_DECL)
            continue;
        ast_t *id_node = type_decl->child;
        if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER && id_node->sym != NULL &&
            id_node->sym->name != NULL && strcasecmp(id_node->sym->name, type_name) == 0) {
            return type_decl;
        }
    }
    return NULL;
}

static int type_name_is_class_like(const char *type_name) {
    if (type_name == NULL)
        return 0;

    /* When called during generic method instantiation the raw parser AST
     * may already have been freed.  Use the converted RecordType instead. */
    if (g_instantiate_record != NULL) {
        if (g_instantiate_record->type_id != NULL &&
            strcasecmp(g_instantiate_record->type_id, type_name) == 0)
            return g_instantiate_record->is_class;
    }

    ast_t *type_decl = find_type_decl_in_section(g_interface_type_section_ast, type_name);
    if (type_decl == NULL)
        type_decl = find_type_decl_in_section(g_implementation_type_section_ast, type_name);
    if (type_decl == NULL)
        return 0;

    ast_t *spec_node = type_decl->child;
    while (spec_node != NULL &&
           spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_CLASS_TYPE &&
           spec_node->typ != PASCAL_T_INTERFACE_TYPE)
    {
        spec_node = spec_node->next;
    }

    if (spec_node == NULL)
        return 0;

    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;

    return (spec_node->typ == PASCAL_T_CLASS_TYPE ||
            spec_node->typ == PASCAL_T_INTERFACE_TYPE);
}

static int resolve_array_type_info_from_ast(const char *type_name, ast_t *type_section, TypeInfo *out_info, int depth) {
    if (type_name == NULL || type_section == NULL || out_info == NULL)
        return -1;
    if (depth > 16)
        return -1;

    ast_t *type_decl = find_type_decl_in_section(type_section, type_name);
    if (type_decl == NULL)
        return -1;

    ast_t *spec_node = type_decl->child;
    while (spec_node != NULL && spec_node->typ != PASCAL_T_TYPE_SPEC)
        spec_node = spec_node->next;
    if (spec_node == NULL)
        return -1;

    TypeInfo tmp_info = {0};
    char *tmp_id = NULL;
    struct RecordType *tmp_record = NULL;
    int mapped = convert_type_spec(spec_node, &tmp_id, &tmp_record, &tmp_info);
    if (tmp_record != NULL)
        destroy_record_type(tmp_record);

    if (tmp_info.is_array) {
        *out_info = tmp_info;
        if (tmp_id != NULL)
            free(tmp_id);
        return 0;
    }

    destroy_type_info_contents(&tmp_info);
    if (tmp_id != NULL)
        free(tmp_id);

    ast_t *spec_child = spec_node->child;
    spec_child = unwrap_pascal_node(spec_child);
    if (spec_child != NULL && spec_child->typ == PASCAL_T_IDENTIFIER && spec_child->sym != NULL &&
        spec_child->sym->name != NULL) {
        return resolve_array_type_info_from_ast(spec_child->sym->name, type_section, out_info, depth + 1);
    }

    (void)mapped;
    return -1;
}

static void resolve_array_bounds(TypeInfo *info, ast_t *type_section, ast_t *const_section, const char *id_for_error) {
    if (info == NULL)
        return;
    if (info->start != 0 || info->end != 0)
        return;
    if (info->array_dimensions == NULL || info->array_dimensions->cur == NULL)
        return;

    char *range_str = (char *)info->array_dimensions->cur;
    char *range_copy = strdup(range_str);
    if (range_copy == NULL)
        return;

    char *sep = strstr(range_copy, "..");
    if (sep != NULL) {
        *sep = '\0';
        char *start_id = range_copy;
        char *end_id = sep + 2;

        while (*start_id == ' ' || *start_id == '\t') start_id++;
        while (*end_id == ' ' || *end_id == '\t') end_id++;
        char *p = start_id + strlen(start_id) - 1;
        while (p > start_id && (*p == ' ' || *p == '\t')) *p-- = '\0';
        p = end_id + strlen(end_id) - 1;
        while (p > end_id && (*p == ' ' || *p == '\t')) *p-- = '\0';

        int start_ordinal = resolve_enum_ordinal_from_ast(start_id, type_section);
        int end_ordinal = resolve_enum_ordinal_from_ast(end_id, type_section);
        if (start_ordinal >= 0 && end_ordinal >= 0) {
            info->start = start_ordinal;
            info->end = end_ordinal;
        } else {
            int start_val;
            if (evaluate_simple_const_expr(start_id, const_section, &start_val) == 0) {
                info->start = start_val;
            } else {
                int resolved_start = resolve_const_int_from_ast(start_id, const_section, 0);
                if (resolved_start != 0 || strcmp(start_id, "0") == 0) {
                    info->start = resolved_start;
                } else {
                    char *endptr;
                    long num = strtol(start_id, &endptr, 10);
                    if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                        info->start = (int)num;
                }
            }

            int end_val;
            if (evaluate_simple_const_expr(end_id, const_section, &end_val) == 0) {
                info->end = end_val;
            } else {
                int resolved_end = resolve_const_int_from_ast(end_id, const_section, 0);
                if (resolved_end != 0 || strcmp(end_id, "0") == 0) {
                    info->end = resolved_end;
                } else {
                    char *endptr;
                    long num = strtol(end_id, &endptr, 10);
                    if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                        info->end = (int)num;
                }
            }
        }
    } else {
        int enum_start, enum_end;
        if (resolve_enum_type_range_with_fallback(range_str, type_section, &enum_start, &enum_end) == 0) {
            info->start = enum_start;
            info->end = enum_end;
        } else {
            /* Defer resolution — store the type name for a post-load fixup pass */
            info->unresolved_index_type = strdup(range_str);
        }
    }

    free(range_copy);
}

static int resolve_const_int_in_section(const char *identifier, ast_t *const_section,
                                        int *out_value, int depth) {
    if (identifier == NULL || const_section == NULL || out_value == NULL)
        return -1;
    return resolve_const_int_in_node(identifier, const_section->child, const_section, out_value, depth);
}

static int type_name_exists_in_section(const char *name, ast_t *type_section) {
    if (name == NULL || type_section == NULL)
        return 0;
    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        if (type_decl->typ == PASCAL_T_TYPE_DECL) {
            ast_t *id_node = type_decl->child;
            if (id_node != NULL && id_node->typ == PASCAL_T_IDENTIFIER &&
                id_node->sym != NULL && id_node->sym->name != NULL &&
                strcasecmp(id_node->sym->name, name) == 0) {
                return 1;
            }
        }
        type_decl = type_decl->next;
    }
    return 0;
}

static int evaluate_const_int_expr(ast_t *expr, int *out_value, int depth) {
    if (expr == NULL || out_value == NULL)
        return -1;
    if (depth > 32)
        return -1;

    expr = unwrap_pascal_node(expr);
    if (expr == NULL)
        return -1;

    switch (expr->typ) {
    case PASCAL_T_INTEGER:
    {
        const char *num_str = (expr->sym != NULL) ? expr->sym->name : "0";
        int base = 10;
        if (num_str[0] == '$') {
            base = 16;
            num_str++;
        } else if (num_str[0] == '%') {
            base = 2;
            num_str++;
        } else if (num_str[0] == '&') {
            base = 8;
            num_str++;
        }
        char *endptr = NULL;
        long val = strtol(num_str, &endptr, base);
        if (endptr != NULL && *endptr == '\0' && val >= INT_MIN && val <= INT_MAX) {
            *out_value = (int)val;
            return 0;
        }
        return -1;
    }
    case PASCAL_T_IDENTIFIER:
        return lookup_const_int(expr->sym != NULL ? expr->sym->name : NULL, out_value);
    case PASCAL_T_NEG:
    {
        int inner = 0;
        if (evaluate_const_int_expr(expr->child, &inner, depth + 1) == 0) {
            *out_value = -inner;
            return 0;
        }
        return -1;
    }
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    {
        int left = 0;
        int right = 0;
        if (expr->child == NULL || expr->child->next == NULL)
            return -1;
        if (evaluate_const_int_expr(expr->child, &left, depth + 1) != 0)
            return -1;
        if (evaluate_const_int_expr(expr->child->next, &right, depth + 1) != 0)
            return -1;
        if (expr->typ == PASCAL_T_ADD)
            *out_value = left + right;
        else
            *out_value = left - right;
        return 0;
    }
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    {
        int left = 0;
        int right = 0;
        if (expr->child == NULL || expr->child->next == NULL)
            return -1;
        if (evaluate_const_int_expr(expr->child, &left, depth + 1) != 0)
            return -1;
        if (evaluate_const_int_expr(expr->child->next, &right, depth + 1) != 0)
            return -1;
        switch (expr->typ) {
            case PASCAL_T_MUL:
                *out_value = left * right;
                return 0;
            case PASCAL_T_DIV:
            case PASCAL_T_INTDIV:
                if (right == 0)
                    return -1;
                *out_value = left / right;
                return 0;
            case PASCAL_T_MOD:
                if (right == 0)
                    return -1;
                *out_value = left % right;
                return 0;
            case PASCAL_T_SHL:
                *out_value = left << right;
                return 0;
            case PASCAL_T_SHR:
                *out_value = (unsigned int)left >> right;
                return 0;
            default:
                break;
        }
        return -1;
    }
    case PASCAL_T_NOT:
    {
        int inner = 0;
        if (evaluate_const_int_expr(expr->child, &inner, depth + 1) == 0) {
            *out_value = ~inner;
            return 0;
        }
        return -1;
    }
    case PASCAL_T_AND:
    case PASCAL_T_OR:
    case PASCAL_T_XOR:
    {
        int left = 0, right = 0;
        if (expr->child == NULL || expr->child->next == NULL)
            return -1;
        if (evaluate_const_int_expr(expr->child, &left, depth + 1) != 0)
            return -1;
        if (evaluate_const_int_expr(expr->child->next, &right, depth + 1) != 0)
            return -1;
        if (expr->typ == PASCAL_T_AND)
            *out_value = left & right;
        else if (expr->typ == PASCAL_T_OR)
            *out_value = left | right;
        else
            *out_value = left ^ right;
        return 0;
    }
    case PASCAL_T_FUNC_CALL:
    {
        /* Typecast expressions like tregister($05000000) are parsed as
         * FUNC_CALL where child is the type name and child->next is the
         * argument.  Evaluate the argument as an integer constant. */
        if (expr->child != NULL && expr->child->next != NULL)
            return evaluate_const_int_expr(expr->child->next, out_value, depth + 1);
        return -1;
    }
    default:
        return -1;
    }
}

static int resolve_const_int_from_ast_internal(const char *identifier, ast_t *const_section,
                                               int fallback_value, int depth) {
    if (identifier == NULL || const_section == NULL)
        return fallback_value;
    if (depth > 32)
        return fallback_value;

    int cached = 0;
    if (lookup_const_int(identifier, &cached) == 0)
        return cached;

    /* Fast path: use the indexed const-decl map for O(1) lookup.
     * This replaces the recursive resolve_const_int_in_section walk. */
    ast_t *indexed_value = const_decl_index_lookup(identifier);
    if (indexed_value != NULL) {
        ast_t *value_node = unwrap_pascal_node(indexed_value);
        if (value_node != NULL && value_node->sym != NULL) {
            char *endptr;
            long val = strtol(value_node->sym->name, &endptr, 10);
            if (*endptr == '\0' && val >= INT_MIN && val <= INT_MAX) {
                register_const_int(identifier, (int)val);
                return (int)val;
            }
            /* Value is another identifier — resolve recursively */
            if (value_node->typ == PASCAL_T_IDENTIFIER &&
                strcasecmp(value_node->sym->name, identifier) != 0) {
                int resolved = resolve_const_int_from_ast_internal(
                    value_node->sym->name, const_section, INT_MIN, depth + 1);
                if (resolved != INT_MIN) {
                    register_const_int(identifier, resolved);
                    return resolved;
                }
            }
        } else if (value_node != NULL && value_node->typ == PASCAL_T_NEG &&
                   value_node->child != NULL) {
            ast_t *inner = unwrap_pascal_node(value_node->child);
            if (inner != NULL && inner->sym != NULL) {
                int resolved = resolve_const_int_from_ast_internal(
                    inner->sym->name, const_section, INT_MIN, depth + 1);
                if (resolved != INT_MIN) {
                    register_const_int(identifier, -resolved);
                    return -resolved;
                }
            }
        } else if (value_node != NULL) {
            /* Try evaluating as a const expression (handles +, -, *, etc.) */
            int eval_result = 0;
            if (evaluate_const_int_expr(value_node, &eval_result, depth + 1) == 0) {
                register_const_int(identifier, eval_result);
                return eval_result;
            }
        }
    }

    /* Slow fallback: recursive AST walk (for const sections not yet indexed) */
    int resolved = INT_MIN;
    int found = 0;
    if (g_const_sections != NULL) {
        for (ListNode_t *cur = g_const_sections; cur != NULL; cur = cur->next) {
            if (resolve_const_int_in_section(identifier, (ast_t *)cur->cur, &resolved, depth) == 0) {
                found = 1;
                break;
            }
        }
    } else {
        /* Index this const_section for future O(1) lookups */
        const_decl_index_scan_section(const_section);
        if (resolve_const_int_in_section(identifier, const_section, &resolved, depth) == 0)
            found = 1;
    }

    if (found) {
        /* Cache the resolved value so future lookups are O(1) */
        register_const_int(identifier, resolved);
        return resolved;
    }
    return fallback_value; /* Not found */
}

static int resolve_const_int_from_ast(const char *identifier, ast_t *const_section, int fallback_value) {
    return resolve_const_int_from_ast_internal(identifier, const_section, fallback_value, 0);
}

/* Evaluate simple const expression like "NUM-1" or "NUM+1" */
typedef struct ConstExprScanner {
    const char *input;
    size_t pos;
    ast_t *const_section;
} ConstExprScanner;

static void const_expr_skip_ws(ConstExprScanner *scanner) {
    while (scanner->input[scanner->pos] != '\0' &&
           isspace((unsigned char)scanner->input[scanner->pos])) {
        scanner->pos++;
    }
}

static int const_expr_match_char(ConstExprScanner *scanner, char ch) {
    const_expr_skip_ws(scanner);
    if (scanner->input[scanner->pos] == ch) {
        scanner->pos++;
        return 1;
    }
    return 0;
}

static int const_expr_match_keyword(ConstExprScanner *scanner, const char *kw) {
    const_expr_skip_ws(scanner);
    size_t len = strlen(kw);
    if (strncasecmp(scanner->input + scanner->pos, kw, len) != 0)
        return 0;
    char next = scanner->input[scanner->pos + len];
    if (isalnum((unsigned char)next) || next == '_')
        return 0;
    scanner->pos += len;
    return 1;
}

static int parse_integer_literal(const char *num_str, int base, long long *out_value, char **out_endptr)
{
    if (num_str == NULL || out_value == NULL)
        return -1;

    errno = 0;
    char *endptr = NULL;
    unsigned long long value = strtoull(num_str, &endptr, base);
    if (endptr == num_str)
        return -1;
    if (errno == ERANGE)
        errno = 0;

    *out_value = (long long)value;
    if (out_endptr != NULL)
        *out_endptr = endptr;
    return 0;
}

static int const_expr_parse_number(ConstExprScanner *scanner, long long *out_value) {
    const_expr_skip_ws(scanner);
    const char *start = scanner->input + scanner->pos;
    if (*start == '\0')
        return -1;

    int base = 10;
    const char *num_start = start;
    if (*start == '$') {
        base = 16;
        num_start = start + 1;
    } else if (*start == '%') {
        base = 2;
        num_start = start + 1;
    } else if (*start == '&') {
        base = 8;
        num_start = start + 1;
    } else if (!isdigit((unsigned char)*start)) {
        return -1;
    }

    char *endptr = NULL;
    long long value = 0;
    if (parse_integer_literal(num_start, base, &value, &endptr) != 0)
        return -1;

    scanner->pos = (size_t)(endptr - scanner->input);
    *out_value = value;
    return 0;
}

/* Resolve SizeOf(typename) to a compile-time constant for known types.
   Returns the size in bytes on x86_64, or -1 if the type is unknown. */
static int resolve_sizeof_type(const char *type_name) {
    if (type_name == NULL)
        return -1;
    /* Pointer-sized types (8 bytes on x86_64) */
    if (strcasecmp(type_name, "Pointer") == 0 ||
        strcasecmp(type_name, "CodePointer") == 0 ||
        strcasecmp(type_name, "SizeInt") == 0 ||
        strcasecmp(type_name, "SizeUInt") == 0 ||
        strcasecmp(type_name, "PtrInt") == 0 ||
        strcasecmp(type_name, "PtrUInt") == 0 ||
        strcasecmp(type_name, "Int64") == 0 ||
        strcasecmp(type_name, "QWord") == 0 ||
        strcasecmp(type_name, "NativeInt") == 0 ||
        strcasecmp(type_name, "NativeUInt") == 0 ||
        strcasecmp(type_name, "ValSInt") == 0 ||
        strcasecmp(type_name, "ValUInt") == 0 ||
        strcasecmp(type_name, "CodePtrInt") == 0 ||
        strcasecmp(type_name, "CodePtrUInt") == 0)
        return 8;
    /* 4-byte types */
    if (strcasecmp(type_name, "LongInt") == 0 ||
        strcasecmp(type_name, "LongWord") == 0 ||
        strcasecmp(type_name, "DWord") == 0 ||
        strcasecmp(type_name, "Cardinal") == 0 ||
        strcasecmp(type_name, "Integer") == 0 ||
        strcasecmp(type_name, "Single") == 0)
        return 4;
    /* 2-byte types */
    if (strcasecmp(type_name, "SmallInt") == 0 ||
        strcasecmp(type_name, "Word") == 0 ||
        strcasecmp(type_name, "WideChar") == 0)
        return 2;
    /* 1-byte types */
    if (strcasecmp(type_name, "Byte") == 0 ||
        strcasecmp(type_name, "ShortInt") == 0 ||
        strcasecmp(type_name, "Boolean") == 0 ||
        strcasecmp(type_name, "Char") == 0 ||
        strcasecmp(type_name, "AnsiChar") == 0)
        return 1;
    /* 8-byte float */
    if (strcasecmp(type_name, "Double") == 0 ||
        strcasecmp(type_name, "Real") == 0)
        return 8;
    /* 10-byte extended (padded to 16 on x86_64 in some ABIs, but SizeOf returns 10) */
    if (strcasecmp(type_name, "Extended") == 0)
        return 10;
    return -1;
}

static int const_expr_parse_identifier(ConstExprScanner *scanner, long long *out_value) {
    const_expr_skip_ws(scanner);
    size_t start = scanner->pos;
    char ch = scanner->input[start];
    if (!(isalpha((unsigned char)ch) || ch == '_'))
        return -1;
    scanner->pos++;
    while (scanner->input[scanner->pos] != '\0') {
        char c = scanner->input[scanner->pos];
        if (!(isalnum((unsigned char)c) || c == '_'))
            break;
        scanner->pos++;
    }

    size_t len = scanner->pos - start;
    char *ident = (char *)malloc(len + 1);
    if (ident == NULL)
        return -1;
    memcpy(ident, scanner->input + start, len);
    ident[len] = '\0';

    /* Handle SizeOf(typename) as a compile-time builtin */
    if (strcasecmp(ident, "sizeof") == 0) {
        free(ident);
        const_expr_skip_ws(scanner);
        if (scanner->input[scanner->pos] != '(')
            return -1;
        scanner->pos++;  /* skip '(' */
        const_expr_skip_ws(scanner);
        /* Read the type name argument */
        size_t arg_start = scanner->pos;
        while (scanner->input[scanner->pos] != '\0' &&
               scanner->input[scanner->pos] != ')' &&
               (isalnum((unsigned char)scanner->input[scanner->pos]) || scanner->input[scanner->pos] == '_'))
            scanner->pos++;
        size_t arg_len = scanner->pos - arg_start;
        if (arg_len == 0)
            return -1;
        char *type_name = (char *)malloc(arg_len + 1);
        if (type_name == NULL)
            return -1;
        memcpy(type_name, scanner->input + arg_start, arg_len);
        type_name[arg_len] = '\0';
        const_expr_skip_ws(scanner);
        if (scanner->input[scanner->pos] != ')') {
            free(type_name);
            return -1;
        }
        scanner->pos++;  /* skip ')' */
        int size = resolve_sizeof_type(type_name);
        free(type_name);
        if (size < 0)
            return -1;
        *out_value = size;
        return 0;
    }

    /* Handle boolean literals */
    if (strcasecmp(ident, "false") == 0) {
        free(ident);
        *out_value = 0;
        return 0;
    }
    if (strcasecmp(ident, "true") == 0) {
        free(ident);
        *out_value = 1;
        return 0;
    }

    int val = resolve_const_int_from_ast(ident, scanner->const_section, INT_MIN);
    free(ident);
    if (val == INT_MIN)
        return -1;
    *out_value = val;
    return 0;
}

static int const_expr_parse_expression(ConstExprScanner *scanner, long long *out_value);

static int const_expr_parse_primary(ConstExprScanner *scanner, long long *out_value) {
    const_expr_skip_ws(scanner);
    if (const_expr_match_char(scanner, '(')) {
        if (const_expr_parse_expression(scanner, out_value) != 0)
            return -1;
        if (!const_expr_match_char(scanner, ')'))
            return -1;
        return 0;
    }
    if (const_expr_parse_number(scanner, out_value) == 0)
        return 0;
    return const_expr_parse_identifier(scanner, out_value);
}

static int const_expr_parse_unary(ConstExprScanner *scanner, long long *out_value) {
    if (const_expr_match_char(scanner, '-')) {
        long long inner = 0;
        if (const_expr_parse_unary(scanner, &inner) != 0)
            return -1;
        *out_value = -inner;
        return 0;
    }
    if (const_expr_match_keyword(scanner, "not")) {
        long long inner = 0;
        if (const_expr_parse_unary(scanner, &inner) != 0)
            return -1;
        *out_value = ~inner;
        return 0;
    }
    return const_expr_parse_primary(scanner, out_value);
}

static int const_expr_parse_mul(ConstExprScanner *scanner, long long *out_value) {
    long long lhs = 0;
    if (const_expr_parse_unary(scanner, &lhs) != 0)
        return -1;

    while (1) {
        if (const_expr_match_char(scanner, '*')) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            lhs *= rhs;
            continue;
        }
        if (const_expr_match_char(scanner, '/')) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            if (rhs == 0)
                return -1;
            lhs /= rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "div")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            if (rhs == 0)
                return -1;
            lhs /= rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "mod")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            if (rhs == 0)
                return -1;
            lhs %= rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "shl")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            lhs = lhs << rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "shr")) {
            long long rhs = 0;
            if (const_expr_parse_unary(scanner, &rhs) != 0)
                return -1;
            lhs = (unsigned long long)lhs >> rhs;
            continue;
        }
        break;
    }

    *out_value = lhs;
    return 0;
}

static int const_expr_parse_add(ConstExprScanner *scanner, long long *out_value) {
    long long lhs = 0;
    if (const_expr_parse_mul(scanner, &lhs) != 0)
        return -1;

    while (1) {
        if (const_expr_match_char(scanner, '+')) {
            long long rhs = 0;
            if (const_expr_parse_mul(scanner, &rhs) != 0)
                return -1;
            lhs += rhs;
            continue;
        }
        if (const_expr_match_char(scanner, '-')) {
            long long rhs = 0;
            if (const_expr_parse_mul(scanner, &rhs) != 0)
                return -1;
            lhs -= rhs;
            continue;
        }
        break;
    }

    *out_value = lhs;
    return 0;
}

static int const_expr_parse_expression(ConstExprScanner *scanner, long long *out_value) {
    long long lhs = 0;
    if (const_expr_parse_add(scanner, &lhs) != 0)
        return -1;

    while (1) {
        if (const_expr_match_keyword(scanner, "and")) {
            long long rhs = 0;
            if (const_expr_parse_add(scanner, &rhs) != 0)
                return -1;
            lhs = lhs & rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "or")) {
            long long rhs = 0;
            if (const_expr_parse_add(scanner, &rhs) != 0)
                return -1;
            lhs = lhs | rhs;
            continue;
        }
        if (const_expr_match_keyword(scanner, "xor")) {
            long long rhs = 0;
            if (const_expr_parse_add(scanner, &rhs) != 0)
                return -1;
            lhs = lhs ^ rhs;
            continue;
        }
        break;
    }

    *out_value = lhs;
    return 0;
}

static int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result) {
    if (expr == NULL || result == NULL)
        return -1;

    ConstExprScanner scanner = {expr, 0, const_section};
    long long value = 0;
    if (const_expr_parse_expression(&scanner, &value) != 0)
        return -1;
    const_expr_skip_ws(&scanner);
    if (scanner.input[scanner.pos] != '\0')
        return -1;
    if (value < INT_MIN || value > INT_MAX)
        return -1;
    *result = (int)value;
    return 0;
}

/* Serialize an expression AST node to a string representation.
 * Handles simple identifiers, literals, and binary operations (+, -).
 * Returns a malloc'd string or NULL on failure. */
static int const_expr_precedence(ast_t *expr) {
    if (expr == NULL)
        return 0;
    switch (expr->typ) {
        case PASCAL_T_ADD:
        case PASCAL_T_SUB:
            return 1;
        case PASCAL_T_MUL:
        case PASCAL_T_DIV:
        case PASCAL_T_INTDIV:
        case PASCAL_T_MOD:
        case PASCAL_T_SHL:
        case PASCAL_T_SHR:
            return 2;
        case PASCAL_T_NEG:
            return 3;
        default:
            return 4;
    }
}

static char *serialize_expr_to_string_internal(ast_t *expr, int parent_prec) {
    if (expr == NULL)
        return NULL;
    expr = unwrap_pascal_node(expr);
    if (expr == NULL)
        return NULL;

    /* Simple identifier or literal with a symbol */
    if (expr->sym != NULL && expr->sym->name != NULL) {
        if (strcasecmp(expr->sym->name, "sizeof") == 0 &&
            kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
        {
            fprintf(stderr,
                "[KGPC] sizeof node typ=%d child=%p child_typ=%d child_sym=%s next_typ=%d\n",
                expr->typ,
                (void *)expr->child,
                expr->child ? expr->child->typ : -1,
                (expr->child && expr->child->sym && expr->child->sym->name) ? expr->child->sym->name : "<null>",
                expr->next ? expr->next->typ : -1);
        }
        if (strcasecmp(expr->sym->name, "sizeof") == 0 && expr->child != NULL) {
            char *inner = serialize_expr_to_string_internal(expr->child, 0);
            if (inner == NULL)
                return NULL;
            size_t len = strlen(inner) + 9;  /* "sizeof(" + inner + ")" + '\0' */
            char *result = (char *)malloc(len);
            if (result == NULL) {
                free(inner);
                return NULL;
            }
            snprintf(result, len, "sizeof(%s)", inner);
            free(inner);
            return result;
        }
        return strdup(expr->sym->name);
    }

    /* Unary minus: -X */
    if (expr->typ == PASCAL_T_NEG && expr->child != NULL) {
        int prec = const_expr_precedence(expr);
        char *inner = serialize_expr_to_string_internal(expr->child, prec);
        if (inner == NULL)
            return NULL;
        size_t len = strlen(inner) + 2;
        char *result = (char *)malloc(len + 2);
        if (result == NULL) {
            free(inner);
            return NULL;
        }
        if (const_expr_precedence(expr->child) < prec)
            snprintf(result, len + 2, "-(%s)", inner);
        else
            snprintf(result, len + 2, "-%s", inner);
        free(inner);
        if (prec < parent_prec) {
            size_t wrap_len = strlen(result) + 3;
            char *wrapped = (char *)malloc(wrap_len);
            if (wrapped == NULL) {
                free(result);
                return NULL;
            }
            snprintf(wrapped, wrap_len, "(%s)", result);
            free(result);
            return wrapped;
        }
        return result;
    }

    if ((expr->typ == PASCAL_T_ADD || expr->typ == PASCAL_T_SUB ||
         expr->typ == PASCAL_T_MUL || expr->typ == PASCAL_T_DIV ||
         expr->typ == PASCAL_T_INTDIV || expr->typ == PASCAL_T_MOD || expr->typ == PASCAL_T_SHL ||
         expr->typ == PASCAL_T_SHR) &&
        expr->child != NULL && expr->child->next != NULL) {
        int prec = const_expr_precedence(expr);
        char *left = serialize_expr_to_string_internal(expr->child, prec);
        char *right = serialize_expr_to_string_internal(expr->child->next, prec);
        if (left == NULL || right == NULL) {
            if (left) free(left);
            if (right) free(right);
            return NULL;
        }
        const char *op = NULL;
        switch (expr->typ) {
            case PASCAL_T_ADD: op = "+"; break;
            case PASCAL_T_SUB: op = "-"; break;
            case PASCAL_T_MUL: op = "*"; break;
            case PASCAL_T_DIV: op = "div"; break;
            case PASCAL_T_INTDIV: op = "div"; break;
            case PASCAL_T_MOD: op = "mod"; break;
            case PASCAL_T_SHL: op = "shl"; break;
            case PASCAL_T_SHR: op = "shr"; break;
            default: break;
        }
        size_t len = strlen(left) + strlen(right) + strlen(op) + 3;
        char *result = (char *)malloc(len + 2);
        if (result == NULL) {
            free(left);
            free(right);
            return NULL;
        }
        snprintf(result, len + 2, "%s %s %s", left, op, right);
        free(left);
        free(right);
        if (prec < parent_prec) {
            size_t wrap_len = strlen(result) + 3;
            char *wrapped = (char *)malloc(wrap_len);
            if (wrapped == NULL) {
                free(result);
                return NULL;
            }
            snprintf(wrapped, wrap_len, "(%s)", result);
            free(result);
            return wrapped;
        }
        return result;
    }

    /* If we have a child with a symbol (wrapped expression) */
    if (expr->child != NULL && expr->child->sym != NULL && expr->child->sym->name != NULL) {
        if (strcasecmp(expr->child->sym->name, "sizeof") == 0) {
            ast_t *arg_node = expr->child->child;
            if (arg_node == NULL)
                arg_node = expr->child->next;
            if (kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] sizeof wrapper typ=%d arg_typ=%d arg_sym=%s\n",
                    expr->child->typ,
                    arg_node ? arg_node->typ : -1,
                    (arg_node && arg_node->sym && arg_node->sym->name) ? arg_node->sym->name : "<null>");
            }
            if (arg_node != NULL)
            {
                char *inner = serialize_expr_to_string_internal(arg_node, 0);
                if (inner != NULL)
                {
                    size_t len = strlen(inner) + 9;
                    char *result = (char *)malloc(len);
                    if (result == NULL) {
                        free(inner);
                        return NULL;
                    }
                    snprintf(result, len, "sizeof(%s)", inner);
                    free(inner);
                    return result;
                }
            }
        }
        return strdup(expr->child->sym->name);
    }

    return NULL;
}

static char *serialize_expr_to_string(ast_t *expr) {
    return serialize_expr_to_string_internal(expr, 0);
}

static int resolve_const_expr_to_int(ast_t *expr, int *out_value)
{
    if (out_value == NULL)
        return -1;
    *out_value = 0;
    if (expr == NULL)
        return -1;

    int direct_value = 0;
    if (evaluate_const_int_expr(expr, &direct_value, 0) == 0) {
        *out_value = direct_value;
        return 0;
    }

    char *expr_str = serialize_expr_to_string(expr);
    if (expr_str == NULL)
        return -1;

    int value = 0;
    if (resolve_const_expr_from_sections(expr_str, &value) != 0)
        value = atoi(expr_str);
    free(expr_str);

    *out_value = value;
    return 0;
}

static struct RecordField *find_record_field_by_name(const struct RecordType *record,
    const char *field_name)
{
    if (record == NULL || field_name == NULL)
        return NULL;
    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (field != NULL && field->name != NULL &&
            pascal_identifier_equals(field->name, field_name))
        {
            return field;
        }
    }
    return NULL;
}

static int convert_type_spec(ast_t *type_spec, char **type_id_out,
                             struct RecordType **record_out, TypeInfo *type_info) {
    if (type_id_out != NULL)
        *type_id_out = NULL;
    if (record_out != NULL)
        *record_out = NULL;
    if (type_info != NULL) {
        type_info->is_array = 0;
        type_info->start = 0;
        type_info->end = 0;
        type_info->element_type = UNKNOWN_TYPE;
        type_info->element_type_id = NULL;
        type_info->element_type_ref = NULL;
        type_info->is_open_array = 0;
        type_info->array_dimensions = NULL;
        type_info->is_pointer = 0;
        type_info->pointer_type = UNKNOWN_TYPE;
        type_info->pointer_type_id = NULL;
        type_info->pointer_type_ref = NULL;
        type_info->is_set = 0;
        type_info->set_element_type = UNKNOWN_TYPE;
        type_info->set_element_type_id = NULL;
        type_info->set_element_type_ref = NULL;
        type_info->is_enum = 0;
        type_info->enum_has_explicit_values = 0;
        type_info->enum_literals = NULL;
        type_info->is_file = 0;
        type_info->file_type = UNKNOWN_TYPE;
        type_info->file_type_id = NULL;
        type_info->file_type_ref = NULL;
        type_info->is_record = 0;
        type_info->record_type = NULL;
        type_info->is_generic_specialization = 0;
        type_info->generic_base_name = NULL;
        type_info->generic_type_args = NULL;
        type_info->type_ref = NULL;
        type_info->is_range = 0;
        type_info->range_known = 0;
        type_info->range_start = 0;
        type_info->range_end = 0;
    }

    if (type_spec == NULL)
        return UNKNOWN_TYPE;

    ast_t *spec_node = type_spec;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;
    spec_node = unwrap_pascal_node(spec_node);

    if (spec_node == NULL)
        return UNKNOWN_TYPE;

    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] convert_type_spec node typ=%d (%s) sym=%s\n",
            spec_node->typ,
            pascal_tag_to_string(spec_node->typ),
            (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>");

    if (spec_node->typ == PASCAL_T_QUALIFIED_IDENTIFIER) {
        QualifiedIdent *qid = qualified_ident_from_ast(spec_node);
        const char *last = qualified_ident_last(qid);
        char *last_dup = last ? strdup(last) : NULL;
        char *qualified_name = qualified_ident_join(qid, ".");
        int result = map_type_name(last_dup, type_id_out);
        if (type_info != NULL && qid != NULL)
            type_info->type_ref = type_ref_create(qid, NULL, 0);
        else if (qid != NULL)
            qualified_ident_free(qid);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
            if (type_info->file_type_id != NULL)
                type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
        }
        if (result == UNKNOWN_TYPE && type_id_out != NULL) {
            /* map_type_name sets *type_id_out to the bare last component even
             * for UNKNOWN_TYPE.  Override with the full qualified name so that
             * unit-qualified references like baseunix.stat are preserved. */
            if (qualified_name != NULL) {
                free(*type_id_out);
                *type_id_out = qualified_name;
                qualified_name = NULL;
            } else if (*type_id_out == NULL) {
                *type_id_out = last_dup;
                last_dup = NULL;
            }
        }
        free(qualified_name);
        free(last_dup);
        return result;
    }
    if (spec_node->typ == PASCAL_T_MEMBER_ACCESS) {
        QualifiedIdent *qid = qualified_ident_from_ast(spec_node);
        const char *last = qualified_ident_last(qid);
        char *last_dup = last ? strdup(last) : NULL;
        char *qualified_name = qualified_ident_join(qid, ".");
        int result = map_type_name(last_dup, type_id_out);
        if (type_info != NULL && qid != NULL)
            type_info->type_ref = type_ref_create(qid, NULL, 0);
        else if (qid != NULL)
            qualified_ident_free(qid);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
            if (type_info->file_type_id != NULL)
                type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
        }
        if (result == UNKNOWN_TYPE && type_id_out != NULL) {
            if (qualified_name != NULL) {
                free(*type_id_out);
                *type_id_out = qualified_name;
                qualified_name = NULL;
            } else if (*type_id_out == NULL) {
                *type_id_out = last_dup;
                last_dup = NULL;
            }
        }
        free(qualified_name);
        free(last_dup);
        return result;
    }
    if (spec_node->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(spec_node);
        if (dup != NULL && strchr(dup, '.') != NULL)
        {
            QualifiedIdent *qid = qualified_ident_from_ast(spec_node);
            const char *last = qualified_ident_last(qid);
            char *last_dup = last ? strdup(last) : NULL;
            char *qualified_name = qualified_ident_join(qid, ".");
            int result = map_type_name(last_dup, type_id_out);
            if (type_info != NULL && qid != NULL)
                type_info->type_ref = type_ref_create(qid, NULL, 0);
            else if (qid != NULL)
                qualified_ident_free(qid);
            if (type_info != NULL && result == FILE_TYPE) {
                type_info->is_file = 1;
                type_info->file_type = FILE_TYPE;
                if (type_id_out != NULL && *type_id_out != NULL)
                    type_info->file_type_id = strdup(*type_id_out);
                if (type_info->file_type_id != NULL)
                    type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
            }
            if (result == UNKNOWN_TYPE && type_id_out != NULL) {
                if (qualified_name != NULL) {
                    free(*type_id_out);
                    *type_id_out = qualified_name;
                    qualified_name = NULL;
                } else if (*type_id_out == NULL) {
                    if (dup != NULL)
                        *type_id_out = strdup(dup);
                    else {
                        *type_id_out = last_dup;
                        last_dup = NULL;
                    }
                }
            }
            free(qualified_name);
            free(last_dup);
            free(dup);
            return result;
        }

        if (dup != NULL && spec_node->child != NULL &&
            strcasecmp(dup, "string") == 0)
        {
            int size_val = 0;
            if (resolve_const_expr_to_int(spec_node->child, &size_val) != 0)
            {
                frontend_error("Error on line %d: string bound must be constant",
                    spec_node->line);
                size_val = 0;
            }
            if (size_val < 0)
            {
                frontend_error("Error on line %d: string bound must be >= 0",
                    spec_node->line);
                size_val = 0;
            }

            if (type_info != NULL)
            {
                type_info->is_array = 1;
                type_info->start = 0;
                type_info->end = size_val;
                type_info->element_type = CHAR_TYPE;
                type_info->element_type_id = strdup("char");
                type_info->element_type_ref = type_ref_from_single_name("char");
                type_info->is_shortstring = 1;

                ListBuilder dims_builder;
                list_builder_init(&dims_builder);
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "0..%d", size_val);
                list_builder_append(&dims_builder, strdup(buffer), LIST_STRING);
                type_info->array_dimensions = list_builder_finish(&dims_builder);
                /* Numeric bounds are in type_info->start/end; no symbolic strings needed */
                type_info->array_dims_parsed = 1;
            }

            free(dup);
            return UNKNOWN_TYPE;
        }

        int result = map_type_name(dup, type_id_out);
        result = apply_shortstring_mode(result, dup);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
            if (type_info->file_type_id != NULL)
                type_info->file_type_ref = type_ref_from_single_name(type_info->file_type_id);
        }
        if (type_info != NULL && type_info->type_ref == NULL && dup != NULL)
            type_info->type_ref = type_ref_from_single_name(dup);
        if (result == UNKNOWN_TYPE && type_id_out != NULL && *type_id_out == NULL) {
            *type_id_out = dup;
        } else {
            free(dup);
        }
        return result;
    }
    /* Generic type specializations: either constructed syntax TFoo<T> or
     * FPC-style \"specialize TFoo<T>\". Both map to generic instantiation
     * using the same mangling and RecordType template. */
    {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        int is_generic = 0;

        if (spec_node->typ == PASCAL_T_CONSTRUCTED_TYPE) {
            is_generic = extract_constructed_type_info(type_spec, &base_name, &type_args);
        } else {
            is_generic = extract_specialize_type_info(type_spec, &base_name, &type_args);
        }

        if (is_generic) {
            char *specialized_name = NULL;
            struct RecordType *record = instantiate_generic_record(base_name, type_args, &specialized_name);

            if (type_info != NULL && type_info->type_ref == NULL)
                type_info->type_ref = type_ref_from_name_and_args(base_name, type_args);

            if (record != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL && specialized_name != NULL)
                {
                    fprintf(stderr, "[KGPC] convert_type_spec generic %s -> record=%p type_info_ptr=%p record_out_ptr=%p\n",
                        specialized_name, (void *)record, (void *)type_info, (void *)record_out);
                }
                if (type_info != NULL) {
                    type_info->is_record = 1;
                    type_info->record_type = record;
                    type_info->is_generic_specialization = 1;
                    record = NULL;
                } else if (record_out != NULL) {
                    *record_out = record;
                    record = NULL;
                }
                if (record != NULL)
                    destroy_record_type(record);
                if (type_id_out != NULL && specialized_name != NULL)
                    *type_id_out = specialized_name;
                else
                    free(specialized_name);
                free(base_name);
                if (type_args != NULL)
                    destroy_list(type_args);
                return RECORD_TYPE;
            }

            if (type_info != NULL) {
                int alias_result = UNKNOWN_TYPE;
                if (resolve_generic_alias_type(base_name, type_args, type_id_out, type_info,
                        &alias_result)) {
                    if (specialized_name != NULL)
                        free(specialized_name);
                    free(base_name);
                    if (type_args != NULL)
                        destroy_list(type_args);
                    return alias_result;
                }
            }

            int can_defer = (g_allow_pending_specializations && type_info != NULL);
            if (can_defer) {
                if (type_info != NULL) {
                    type_info->is_record = 1;
                    type_info->is_generic_specialization = 1;
                    type_info->generic_base_name = base_name;
                    type_info->generic_type_args = type_args;
                }
                if (type_id_out != NULL) {
                    if (*type_id_out != NULL) {
                        free(*type_id_out);
                    }
                    if (specialized_name == NULL)
                        specialized_name = mangle_specialized_name_from_list(base_name, type_args);
                    *type_id_out = specialized_name;
                    specialized_name = NULL;
                }
                if (specialized_name != NULL)
                    free(specialized_name);
                return RECORD_TYPE;
            }

            if (specialized_name != NULL)
                free(specialized_name);
            free(base_name);
            if (type_args != NULL)
                destroy_list(type_args);
        }
    }
    if (spec_node->typ == PASCAL_T_RANGE_TYPE) {
        ast_t *lower = unwrap_pascal_node(spec_node->child);
        ast_t *upper = (lower != NULL) ? unwrap_pascal_node(lower->next) : NULL;
        long long start_value = 0;
        long long end_value = 0;
        int have_start = 0;
        int have_end = 0;
        struct Expression *lower_expr = convert_expression(lower);
        struct Expression *upper_expr = convert_expression(upper);

        if (lower_expr != NULL) {
            have_start = (extract_constant_int(lower_expr, &start_value) == 0);
            destroy_expr(lower_expr);
        }
        if (upper_expr != NULL) {
            have_end = (extract_constant_int(upper_expr, &end_value) == 0);
            destroy_expr(upper_expr);
        }

        /* Fallback: try evaluating bounds from the AST using const int registry
         * (handles cases like 0..NUM_REGS - 1 where NUM_REGS is a named constant) */
        if (!have_start) {
            int val = 0;
            if (evaluate_const_int_expr(spec_node->child, &val, 0) == 0) {
                start_value = val;
                have_start = 1;
            }
        }
        if (!have_end && spec_node->child != NULL) {
            int val = 0;
            if (evaluate_const_int_expr(spec_node->child->next, &val, 0) == 0) {
                end_value = val;
                have_end = 1;
            }
        }

        if (type_info != NULL) {
            char *lower_str = serialize_expr_to_string(spec_node->child);
            char *upper_str = serialize_expr_to_string(spec_node->child != NULL ?
                spec_node->child->next : NULL);
            type_info->is_range = 1;
            type_info->range_start = start_value;
            type_info->range_end = end_value;
            type_info->range_known = (have_start && have_end);
            type_info->range_start_str = lower_str;
            type_info->range_end_str = upper_str;
        }

        return UNKNOWN_TYPE;
    }
    if (spec_node->typ == PASCAL_T_ARRAY_TYPE) {
        if (type_info != NULL) {
            type_info->is_array = 1;
            type_info->start = 0;
            type_info->end = -1;
            ListBuilder dims_builder;
            list_builder_init(&dims_builder);

            ast_t *child = spec_node->child;
            ast_t *element_node = child;
            while (element_node != NULL && element_node->next != NULL)
                element_node = element_node->next;

            if (kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL) {
                fprintf(stderr, "[KGPC] array type children:");
                for (ast_t *dim = child; dim != NULL; dim = dim->next) {
                    fprintf(stderr, " %d(%s)", dim->typ, pascal_tag_to_string(dim->typ));
                }
                fprintf(stderr, "\n");
            }

            // Check if there are any dimensions specified
            int has_dimensions = 0;
            for (ast_t *dim = child; dim != NULL && dim != element_node; dim = dim->next) {
                has_dimensions = 1;
                break;
            }
            
            // If no dimensions specified, it's an open array (e.g., "array of string")
            if (!has_dimensions) {
                type_info->is_open_array = 1;
            }

            for (ast_t *dim = child; dim != NULL && dim != element_node; dim = dim->next) {
                if (dim->typ == PASCAL_T_RANGE_TYPE) {
                    ast_t *lower = dim->child;
                    ast_t *upper = (lower != NULL) ? lower->next : NULL;
                    
                    /* Extract lower and upper bound strings, handling various AST structures:
                     * - Simple literals/identifiers (e.g., "1", "N") have sym->name set
                     * - Unary expressions (e.g., "-1") are PASCAL_T_NEG
                     * - Binary expressions (e.g., "N-1") are PASCAL_T_SUB
                     * Use serialize_expr_to_string to handle all cases uniformly */
                    char *lower_str = serialize_expr_to_string(lower);
                    char *upper_str = serialize_expr_to_string(upper);
                    int lower_val = 0;
                    int upper_val = 0;
                    int lower_ok = (evaluate_const_int_expr(lower, &lower_val, 0) == 0);
                    int upper_ok = (evaluate_const_int_expr(upper, &upper_val, 0) == 0);

                    if (lower_str == NULL && lower_ok) {
                        char buffer[64];
                        snprintf(buffer, sizeof(buffer), "%d", lower_val);
                        lower_str = strdup(buffer);
                    }
                    if (upper_str == NULL && upper_ok) {
                        char buffer[64];
                        snprintf(buffer, sizeof(buffer), "%d", upper_val);
                        upper_str = strdup(buffer);
                    }

                    if ((lower_str == NULL || upper_str == NULL) &&
                        kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
                    {
                        fprintf(stderr,
                            "[KGPC] array bounds serialize failed: lower typ=%d (%s) upper typ=%d (%s)\n",
                            lower ? lower->typ : -1,
                            lower ? pascal_tag_to_string(lower->typ) : "<null>",
                            upper ? upper->typ : -1,
                            upper ? pascal_tag_to_string(upper->typ) : "<null>");
                    }

                    if (lower_str != NULL && upper_str != NULL) {
                        if (dims_builder.head == NULL) {
                            int start_val = 0;
                            int end_val = 0;
                            if (lower_ok)
                                start_val = lower_val;
                            else if (resolve_const_expr_from_sections(lower_str, &start_val) != 0)
                                start_val = atoi(lower_str);
                            if (upper_ok)
                                end_val = upper_val;
                            else if (resolve_const_expr_from_sections(upper_str, &end_val) != 0)
                                end_val = atoi(upper_str);
                            type_info->start = start_val;
                            type_info->end = end_val;
                            /* Store structured bounds for semcheck */
                            type_info->array_dim_start_str = strdup(lower_str);
                            type_info->array_dim_end_str = strdup(upper_str);
                            type_info->array_dims_parsed = 1;
                        }
                        char buffer[128];
                        snprintf(buffer, sizeof(buffer), "%s..%s", lower_str, upper_str);
                        list_builder_append(&dims_builder, strdup(buffer), LIST_STRING);
                        free(lower_str);
                        free(upper_str);
                    } else {
                        if (lower_str != NULL) free(lower_str);
                        if (upper_str != NULL) free(upper_str);
                        type_info->is_open_array = 1;
                    }
                } else if (dim->typ == PASCAL_T_IDENTIFIER) {
                    // Single identifier as array dimension (e.g., array[color], array[boolean])
                    // This is NOT an open array - it's an array indexed by an enum or subrange type
                    char *dim_name = dup_symbol(dim);
                    if (dim_name != NULL) {
                        // Check if this is the first dimension (no nodes appended yet)
                        int is_first_dim = (dims_builder.head == NULL);
                        list_builder_append(&dims_builder, dim_name, LIST_STRING);
                        // Set bounds based on the dimension type
                        // For boolean: 0..1 (False=0, True=1)
                        if (is_first_dim) {
                            // This is the first dimension - set bounds
                            if (strcasecmp(dim_name, "boolean") == 0) {
                                type_info->start = 0;
                                type_info->end = 1;
                            } else if (strcasecmp(dim_name, "char") == 0) {
                                type_info->start = 0;
                                type_info->end = 255;
                            } else if (strcasecmp(dim_name, "byte") == 0) {
                                type_info->start = 0;
                                type_info->end = 255;
                            } else if (strcasecmp(dim_name, "shortint") == 0) {
                                type_info->start = -128;
                                type_info->end = 127;
                            } else {
                                // For other types (enums, etc.), we'll need to resolve them later
                                // For now, mark as needing resolution but set safe bounds
                                // to avoid being marked as open array
                                type_info->start = 0;
                                type_info->end = 0;  // Will be resolved in semantic checking
                            }
                        }
                    }
                    // Note: is_open_array is NOT set to 1 here
                } else {
                    type_info->is_open_array = 1;
                }
            }

            type_info->array_dimensions = list_builder_finish(&dims_builder);

            if (element_node != NULL) {
                if (element_node->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(element_node);
                    if (dup != NULL && strcasecmp(dup, "const") == 0) {
                        type_info->is_array_of_const = 1;
                        type_info->element_type = ARRAY_OF_CONST_TYPE;
                        type_info->is_open_array = 1;
                        free(dup);
                    } else {
                        int mapped = map_type_name(dup, &type_info->element_type_id);
                        type_info->element_type = mapped;
                        if (mapped == UNKNOWN_TYPE && type_info->element_type_id == NULL)
                            type_info->element_type_id = dup;
                        else
                            free(dup);
                    }
                } else if (element_node->typ == PASCAL_T_TYPE_SPEC) {
                    char *nested_id = NULL;
                    struct RecordType *nested_record = NULL;
                    TypeInfo nested_info = {0};
                    int mapped = convert_type_spec(element_node, &nested_id, &nested_record, &nested_info);
                    /* Handle shortstring element types (e.g., string[10]) */
                    if (nested_info.is_shortstring) {
                        type_info->element_type = SHORTSTRING_TYPE;
                        if (type_info->element_type_id == NULL)
                            type_info->element_type_id = strdup("ShortString");
                        /* Preserve the shortstring capacity so codegen uses
                           the correct element size (N+1 bytes for string[N])
                           instead of defaulting to 256. */
                        if (nested_info.end > 0 && type_info->element_kgpc_type == NULL) {
                            type_info->element_kgpc_type =
                                create_primitive_type_with_size(SHORTSTRING_TYPE,
                                                               nested_info.end + 1);
                        }
                        if (nested_id != NULL)
                            free(nested_id);
                    } else if (nested_info.is_range && mapped == UNKNOWN_TYPE) {
                        /* Subrange element type (e.g., array[0..15] of 0..15).
                         * The element is a range type wrapped in TYPE_SPEC —
                         * treat as integer. */
                        type_info->element_type = INT_TYPE;
                        if (nested_id != NULL)
                            free(nested_id);
                    } else {
                        type_info->element_type = mapped;
                        if (type_info->element_type_id == NULL)
                            type_info->element_type_id = nested_id;
                        else if (nested_id != NULL)
                            free(nested_id);
                    }
                    if (nested_record != NULL)
                    {
                        type_info->record_type = nested_record;
                        type_info->is_record = 1;
                        type_info->element_type = RECORD_TYPE;
                    }
                    /* Build KgpcType for nested array elements (array of array of ...) */
                    if (nested_info.is_array)
                    {
                        KgpcType *inner_elem = NULL;
                        if (nested_info.element_kgpc_type != NULL)
                        {
                            /* Deeper nesting: element is already a pre-built KgpcType */
                            inner_elem = nested_info.element_kgpc_type;
                            kgpc_type_retain(inner_elem);
                        }
                        else if (nested_info.record_type != NULL)
                        {
                            inner_elem = create_record_type(clone_record_type(nested_info.record_type));
                        }
                        else if (nested_info.element_type != UNKNOWN_TYPE)
                        {
                            inner_elem = create_primitive_type(nested_info.element_type);
                        }
                        /* Create the inner array KgpcType.
                         * If inner_elem is NULL but element_type_id is set, create a
                         * deferred-resolution array type */
                        if (inner_elem != NULL)
                        {
                            type_info->element_kgpc_type = create_array_type(
                                inner_elem, nested_info.start, nested_info.end);
                        }
                        else if (nested_info.element_type_id != NULL)
                        {
                            /* Named element type: create array with deferred element resolution */
                            type_info->element_kgpc_type = create_array_type(
                                NULL, nested_info.start, nested_info.end);
                            if (type_info->element_kgpc_type != NULL)
                                type_info->element_kgpc_type->info.array_info.element_type_id =
                                    strdup(nested_info.element_type_id);
                        }
                    }
                    destroy_type_info_contents(&nested_info);
                }
                if (type_info->element_type_id != NULL &&
                    strcasecmp(type_info->element_type_id, "const") == 0)
                {
                    free(type_info->element_type_id);
                    type_info->element_type_id = NULL;
                    type_info->is_array_of_const = 1;
                    type_info->element_type = ARRAY_OF_CONST_TYPE;
                    type_info->is_open_array = 1;
                }
            }
        }
        return UNKNOWN_TYPE;
    }

    if (spec_node->typ == PASCAL_T_POINTER_TYPE) {
        if (type_info != NULL) {
            type_info->is_pointer = 1;
            ast_t *target = spec_node->child;
            while (target != NULL && target->typ != PASCAL_T_IDENTIFIER)
                target = target->next;
            if (target != NULL) {
                char *dup = dup_symbol(target);
                int mapped = map_type_name(dup, &type_info->pointer_type_id);
                type_info->pointer_type = mapped;
                if (mapped == UNKNOWN_TYPE && type_info->pointer_type_id == NULL)
                    type_info->pointer_type_id = dup;
                else
                    free(dup);
                if (type_id_out != NULL && *type_id_out == NULL && type_info->pointer_type_id != NULL)
                    *type_id_out = strdup(type_info->pointer_type_id);
            }
        }
        return POINTER_TYPE;
    }

    if (spec_node->typ == PASCAL_T_SET) {
        if (type_info != NULL) {
            type_info->is_set = 1;
            ast_t *elem = spec_node->child;
            while (elem != NULL && elem->typ == PASCAL_T_NONE)
                elem = elem->child;
            if (elem != NULL) {
                if (elem->typ == PASCAL_T_IDENTIFIER) {
                    char *dup = dup_symbol(elem);
                    int mapped = map_type_name(dup, &type_info->set_element_type_id);
                    type_info->set_element_type = mapped;
                    if (mapped == UNKNOWN_TYPE && type_info->set_element_type_id == NULL)
                        type_info->set_element_type_id = dup;
                    else
                        free(dup);
                }
                /* Handle subrange as set element type: set of 0..31 */
                else if (elem->typ == PASCAL_T_RANGE_TYPE) {
                    ast_t *lower = unwrap_pascal_node(elem->child);
                    ast_t *upper = (lower != NULL) ? unwrap_pascal_node(lower->next) : NULL;
                    long long start_value = 0;
                    long long end_value = 0;
                    int have_start = 0;
                    int have_end = 0;
                    struct Expression *lower_expr = convert_expression(lower);
                    struct Expression *upper_expr = convert_expression(upper);
                    if (lower_expr != NULL) {
                        have_start = (extract_constant_int(lower_expr, &start_value) == 0);
                        destroy_expr(lower_expr);
                    }
                    if (upper_expr != NULL) {
                        have_end = (extract_constant_int(upper_expr, &end_value) == 0);
                        destroy_expr(upper_expr);
                    }
                    if (!have_start && elem->child != NULL) {
                        int val = 0;
                        if (evaluate_const_int_expr(elem->child, &val, 0) == 0) {
                            start_value = val;
                            have_start = 1;
                        }
                    }
                    if (!have_end && elem->child != NULL) {
                        int val = 0;
                        if (evaluate_const_int_expr(elem->child->next, &val, 0) == 0) {
                            end_value = val;
                            have_end = 1;
                        }
                    }
                    if (have_start && have_end) {
                        type_info->range_known = 1;
                        type_info->range_start = start_value;
                        type_info->range_end = end_value;
                    }
                    type_info->set_element_type = INT_TYPE;
                }
                /* Handle anonymous enum as set element type: set of (val1, val2, ...) */
                else if (elem->typ == PASCAL_T_ENUMERATED_TYPE) {
                    type_info->is_enum_set = 1;
                    type_info->set_element_type = ENUM_TYPE;
                    ListBuilder enum_builder;
                    list_builder_init(&enum_builder);
                    ast_t *value = elem->child;
                    while (value != NULL) {
                        if (value->typ == PASCAL_T_IDENTIFIER)
                            list_builder_append(&enum_builder, dup_symbol(value), LIST_STRING);
                        value = value->next;
                    }
                    type_info->inline_enum_values = list_builder_finish(&enum_builder);
                }
            }
        }
        return SET_TYPE;
    }

    if (spec_node->typ == PASCAL_T_FILE_TYPE) {
        if (type_info != NULL) {
            type_info->is_file = 1;
            ast_t *elem = spec_node->child;
            while (elem != NULL && (elem->typ == PASCAL_T_NONE || elem->typ == PASCAL_T_TYPE_SPEC))
                elem = elem->child;
            if (elem != NULL && elem->typ == PASCAL_T_IDENTIFIER) {
                char *dup = dup_symbol(elem);
                int mapped = map_type_name(dup, &type_info->file_type_id);
                type_info->file_type = mapped;
                if (mapped == UNKNOWN_TYPE && type_info->file_type_id == NULL)
                    type_info->file_type_id = dup;
                else
                    free(dup);
            }
        }
        return FILE_TYPE;
    }

    if (spec_node->typ == PASCAL_T_ENUMERATED_TYPE) {
        if (type_info != NULL) {
            type_info->is_enum = 1;
            type_info->enum_is_scoped = from_cparser_scopedenums_enabled_at_line(spec_node->line);
            ListBuilder enum_builder;
            list_builder_init(&enum_builder);
            ast_t *value = spec_node->child;
            while (value != NULL) {
                if (value->typ == PASCAL_T_IDENTIFIER)
                    list_builder_append(&enum_builder, dup_symbol(value), LIST_STRING);
                else if (value->typ == PASCAL_T_ASSIGNMENT &&
                         value->child != NULL && value->child->typ == PASCAL_T_IDENTIFIER) {
                    list_builder_append(&enum_builder, dup_symbol(value->child), LIST_STRING);
                    type_info->enum_has_explicit_values = 1;
                }
                value = value->next;
            }
            type_info->enum_literals = list_builder_finish(&enum_builder);
        }
        return ENUM_TYPE;
    }

    if (spec_node->typ == PASCAL_T_PROCEDURE_TYPE || spec_node->typ == PASCAL_T_FUNCTION_TYPE) {
        // Note: For now, we return PROCEDURE as the type tag for both procedures and functions
        // The actual KgpcType object will be created later in the semantic checker
        // This is a temporary bridge solution until full migration to KgpcType
        return PROCEDURE;
    }

    if (spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE) {
        // Reference to type wraps a procedure/function type
        // For now, treat it the same as PROCEDURE_TYPE/FUNCTION_TYPE
        return PROCEDURE;
    }

    if (spec_node->typ == PASCAL_T_RECORD_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE) {
        struct RecordType *record = convert_record_type(spec_node);
        if (type_info != NULL) {
            type_info->is_record = 1;
            type_info->record_type = clone_record_type(record);
        }
        if (record_out != NULL) {
            *record_out = record;
        } else {
            destroy_record_type(record);
        }
        return RECORD_TYPE;
    }

    /* Distinct type: type Real = Double - creates a strong alias.
     * The target may be a full type expression (e.g. "type ^TFoo"),
     * not just an identifier, so recurse through convert_type_spec. */
    if (spec_node->typ == PASCAL_T_DISTINCT_TYPE) {
        /* The child is the target type spec */
        ast_t *target = spec_node->child;
        if (target != NULL) {
            target = unwrap_pascal_node(target);
            if (target != NULL) {
                TypeInfo nested_info = {0};
                struct RecordType *nested_record = NULL;
                char *nested_type_id = NULL;
                int result = convert_type_spec(target, &nested_type_id, &nested_record, &nested_info);

                if (type_info != NULL) {
                    /* Preserve full structural metadata from the distinct target.
                     * Distinct aliases remain strong aliases semantically, but they
                     * must still carry pointer/array/etc. shape for semcheck/codegen. */
                    destroy_type_info_contents(type_info);
                    *type_info = nested_info;
                    memset(&nested_info, 0, sizeof(nested_info));
                }

                if (record_out != NULL && nested_record != NULL) {
                    *record_out = nested_record;
                    nested_record = NULL;
                }

                if (type_id_out != NULL && *type_id_out == NULL && nested_type_id != NULL) {
                    *type_id_out = nested_type_id;
                    nested_type_id = NULL;
                }

                if (nested_type_id != NULL)
                    free(nested_type_id);
                if (nested_record != NULL)
                    destroy_record_type(nested_record);
                destroy_type_info_contents(&nested_info);
                return result;
            }
        }
        /* Malformed distinct type target */
        return UNKNOWN_TYPE;
    }

    /* Handle "class of T" - class reference type */
    if (spec_node->typ == PASCAL_T_CLASS_OF_TYPE) {
        /* Class reference type (class of TMyClass).
         * In Pascal, when T is a class type, T is already a pointer to the class record.
         * "class of T" is semantically a reference to the class itself (metaclass),
         * but structurally it's the same as T - a pointer to the class record.
         * 
         * We treat "class of T" the same as T, which allows:
         *   var ClassRef: class of TMyClass;
         *   ClassRef := TMyClass;  // Works because both are ^record
         */
        if (type_info != NULL) {
            type_info->is_class_reference = 1;
            ast_t *target = spec_node->child;
            while (target != NULL && target->typ != PASCAL_T_IDENTIFIER)
                target = target->next;
            if (target != NULL) {
                char *dup = dup_symbol(target);
                /* Check if this is a builtin type (non-class) - reject it */
                int mapped = map_type_name(dup, NULL);
                if (mapped != UNKNOWN_TYPE) {
                    /* Builtin types like Integer, String, etc. are not class types */
                    frontend_error("Error: 'class of' requires a class type, but got %s", dup);
                    free(dup);
                    return UNKNOWN_TYPE;
                }
                /* Not a builtin type - assume it's a class type.
                 * Further validation will happen during semantic checking. */
                type_info->pointer_type_id = dup;
                if (type_id_out != NULL && *type_id_out == NULL)
                    *type_id_out = strdup(dup);
                type_info->is_pointer = 1;
                type_info->pointer_type = RECORD_TYPE;
            }
        }
        return POINTER_TYPE;
    }

    return UNKNOWN_TYPE;
}

/* Forward declare functions we need */
static ListNode_t *convert_param_list(ast_t **cursor);
static ListNode_t *convert_param(ast_t *param_node);
static struct RecordType *convert_interface_type_ex(const char *interface_name, ast_t *interface_node, ListNode_t **nested_types_out);

/* Convert an AST type specification to a KgpcType object.
 * This is the Phase 2 bridge function that creates KgpcType objects from AST nodes.
 * Returns NULL if the type cannot be converted or if symtab is needed but not provided.
 */
KgpcType *convert_type_spec_to_kgpctype(ast_t *type_spec, struct SymTab *symtab) {
    if (type_spec == NULL)
        return NULL;

    ast_t *spec_node = type_spec;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;
    spec_node = unwrap_pascal_node(spec_node);

    if (spec_node == NULL)
        return NULL;

    if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] convert_type_spec_to_kgpctype node typ=%d sym=%s\n",
            spec_node->typ,
            (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>");
    /* Generic type specializations: constructed TFoo<T> or \"specialize TFoo<T>\" */
    {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        int is_generic = 0;

        if (spec_node->typ == PASCAL_T_CONSTRUCTED_TYPE) {
            is_generic = extract_constructed_type_info(type_spec, &base_name, &type_args);
        } else {
            is_generic = extract_specialize_type_info(type_spec, &base_name, &type_args);
        }

        if (is_generic) {
            char *specialized_name = NULL;
            struct RecordType *record = instantiate_generic_record(base_name, type_args, &specialized_name);
            free(base_name);
            if (type_args != NULL)
                destroy_list(type_args);
            if (specialized_name != NULL)
                free(specialized_name);
            if (record != NULL) {
                return create_record_type(record);
            }
        }
    }

    /* Handle primitive types by identifier */
    if (spec_node->typ == PASCAL_T_IDENTIFIER) {
        char *type_name = dup_symbol(spec_node);
        if (type_name == NULL)
            return NULL;

        /* Get the preserved type name for RawByteString/UnicodeString */
        char *preserved_type_id = NULL;
        int type_tag = map_type_name(type_name, &preserved_type_id);
        type_tag = apply_shortstring_mode(type_tag, type_name);
        if (type_tag != UNKNOWN_TYPE) {
            KgpcType *type = create_primitive_type(type_tag);
            /* Preserve distinct string-family aliases needed for helper lookup and overloads. */
            if (type != NULL && preserved_type_id != NULL &&
                (strcasecmp(preserved_type_id, "AnsiString") == 0 ||
                 strcasecmp(preserved_type_id, "RawByteString") == 0 ||
                 strcasecmp(preserved_type_id, "UnicodeString") == 0 ||
                 strcasecmp(preserved_type_id, "WideString") == 0)) {
                /* Create a TypeAlias to preserve the original type name */
                struct TypeAlias *alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
                if (alias != NULL) {
                    alias->alias_name = strdup(preserved_type_id);
                    alias->base_type = type_tag;
                    /* Set type_alias on the KgpcType */
                    kgpc_type_set_type_alias(type, alias);
                    /* Free the temporary alias after copying */
                    free(alias->alias_name);
                    free(alias);
                }
            }
            free(type_name);
            free(preserved_type_id);
            return type;
        }
        
        free(type_name);
        free(preserved_type_id);

        /* If unknown type and we have a symbol table, try to look it up */
        if (symtab != NULL) {
            /* This will be resolved later during semantic checking */
            /* For now, we need to look up the type in the symbol table */
            /* But that requires SymTab.h which creates circular dependency */
            /* So we return NULL and let the caller handle named type lookup */
            return NULL;
        }

        return NULL;
    }
    if (spec_node->typ == PASCAL_T_CONSTRUCTED_TYPE) {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        if (extract_constructed_type_info(spec_node, &base_name, &type_args)) {
            char *specialized_name = NULL;
            struct RecordType *record = instantiate_generic_record(base_name, type_args, &specialized_name);
            free(base_name);
            if (record != NULL) {
                free(specialized_name);
                return create_record_type(record);
            }
            if (specialized_name != NULL)
                free(specialized_name);
        }
    }

    /* Handle array types */
    if (spec_node->typ == PASCAL_T_ARRAY_TYPE) {
        ast_t *child = spec_node->child;
        ast_t *element_node = child;
        
        /* Find the element type node (last child) */
        while (element_node != NULL && element_node->next != NULL)
            element_node = element_node->next;

        /* Get range if available (first dimension only for now) */
        int start = 0, end = -1;
        if (child != NULL && child->typ == PASCAL_T_RANGE_TYPE) {
            ast_t *lower = child->child;
            ast_t *upper = (lower != NULL) ? lower->next : NULL;
            if (lower != NULL && upper != NULL && lower->sym != NULL && upper->sym != NULL) {
                start = atoi(lower->sym->name);
                end = atoi(upper->sym->name);
            }
        }

        /* Recursively convert element type */
        KgpcType *element_type = convert_type_spec_to_kgpctype(element_node, symtab);
        if (element_type == NULL) {
            /* Try to get a primitive type from identifier */
            if (element_node != NULL && element_node->typ == PASCAL_T_IDENTIFIER) {
                char *elem_type_name = dup_symbol(element_node);
                if (elem_type_name != NULL) {
                    int elem_tag = map_type_name(elem_type_name, NULL);
                    free(elem_type_name);
                    if (elem_tag != UNKNOWN_TYPE) {
                        element_type = create_primitive_type(elem_tag);
                    }
                }
            }
        }

        if (element_type == NULL)
            return NULL;

        {
            KgpcType *arr = create_array_type(element_type, start, end);
            kgpc_type_release(element_type);
            return arr;
        }
    }

    /* Handle file types */
    if (spec_node->typ == PASCAL_T_FILE_TYPE) {
        ast_t *elem = spec_node->child;
        while (elem != NULL && (elem->typ == PASCAL_T_NONE || elem->typ == PASCAL_T_TYPE_SPEC))
            elem = elem->child;

        if (elem == NULL) {
            /* Untyped file - represented as primitive FILE_TYPE */
            return create_primitive_type(FILE_TYPE);
        }

        /* Typed file: attempt to convert element type */
        KgpcType *element_type = convert_type_spec_to_kgpctype(elem, symtab);
        if (element_type == NULL && elem->typ == PASCAL_T_IDENTIFIER) {
            char *elem_name = dup_symbol(elem);
            if (elem_name != NULL) {
                int elem_tag = map_type_name(elem_name, NULL);
                free(elem_name);
                if (elem_tag != UNKNOWN_TYPE) {
                    element_type = create_primitive_type(elem_tag);
                }
            }
        }

        if (element_type == NULL) {
            /* Fallback: treat as generic file */
            return create_primitive_type(FILE_TYPE);
        }

        /* For now, represent file types as primitive FILE_TYPE with alias metadata.
         * The detailed element type is tracked in the TypeAlias/TypeInfo structures. */
        destroy_kgpc_type(element_type);
        return create_primitive_type(FILE_TYPE);
    }

    /* Handle pointer types */
    if (spec_node->typ == PASCAL_T_POINTER_TYPE) {
        ast_t *target = spec_node->child;
        while (target != NULL && target->typ != PASCAL_T_IDENTIFIER && target->typ != PASCAL_T_TYPE_SPEC)
            target = target->next;

        if (target != NULL) {
            KgpcType *points_to = convert_type_spec_to_kgpctype(target, symtab);
            if (points_to == NULL) {
                /* Try primitive type lookup */
                if (target->typ == PASCAL_T_IDENTIFIER) {
                    char *target_name = dup_symbol(target);
                    if (target_name != NULL) {
                        int target_tag = map_type_name(target_name, NULL);
                        free(target_name);
                        if (target_tag != UNKNOWN_TYPE) {
                            points_to = create_primitive_type(target_tag);
                        }
                    }
                }
            }

            KgpcType *ptr = create_pointer_type(points_to);
            if (points_to != NULL) kgpc_type_release(points_to);
            return ptr;
        }
        return create_pointer_type(NULL);
    }

    /* Handle procedure and function types */
    if (spec_node->typ == PASCAL_T_PROCEDURE_TYPE || spec_node->typ == PASCAL_T_FUNCTION_TYPE) {
        int is_function = (spec_node->typ == PASCAL_T_FUNCTION_TYPE);
        
        #ifdef DEBUG_KGPC_TYPE_CREATION
        fprintf(stderr, "DEBUG: convert_type_spec_to_kgpctype: handling %s\n",
                is_function ? "FUNCTION_TYPE" : "PROCEDURE_TYPE");
        #endif
        
        /* Parse parameters */
        ast_t *cursor = spec_node->child;
        ListNode_t *params = NULL;
        
        /* Check if first child is a PARAM_LIST */
        if (cursor != NULL && cursor->typ == PASCAL_T_PARAM_LIST) {
            #ifdef DEBUG_KGPC_TYPE_CREATION
            fprintf(stderr, "DEBUG: Found PARAM_LIST node\n");
            #endif
            /* The PARAM_LIST node should contain PARAM children */
            ast_t *param_cursor = cursor->child;
            params = convert_param_list(&param_cursor);
            /* Move cursor to the next sibling after PARAM_LIST */
            cursor = cursor->next;
            #ifdef DEBUG_KGPC_TYPE_CREATION
            fprintf(stderr, "DEBUG: After PARAM_LIST, cursor=%p, cursor->typ=%d\n",
                    (void*)cursor, cursor ? cursor->typ : -1);
            #endif
        } else {
            /* Skip to parameter list if present, but stop at return type (for parameterless functions) */
            while (cursor != NULL && 
                   cursor->typ != PASCAL_T_PARAM && 
                   cursor->typ != PASCAL_T_TYPE_SPEC &&
                   cursor->typ != PASCAL_T_RETURN_TYPE)
                cursor = cursor->next;
            
            if (cursor != NULL && cursor->typ == PASCAL_T_PARAM) {
                params = convert_param_list(&cursor);
            }
        }
        
        /* For functions, get return type */
        KgpcType *return_type = NULL;
        char *return_type_id = NULL;
        int owns_return_type = 0;
        if (is_function) {
            #ifdef DEBUG_KGPC_TYPE_CREATION
            fprintf(stderr, "DEBUG: Looking for return type, cursor=%p, cursor->typ=%d, cursor->sym=%s, cursor->child=%p\n",
                    (void*)cursor, cursor ? cursor->typ : -1,
                    (cursor && cursor->sym && cursor->sym->name) ? cursor->sym->name : "NULL",
                    cursor ? (void*)cursor->child : NULL);
            if (cursor && cursor->child) {
                fprintf(stderr, "DEBUG: cursor->child->typ=%d, cursor->child->sym=%s\n",
                        cursor->child->typ,
                        (cursor->child->sym && cursor->child->sym->name) ? cursor->child->sym->name : "NULL");
            }
            #endif
            /* Look for return type specification */
            /* The return type might be:
             * 1. A direct sibling (PASCAL_T_TYPE_SPEC or PASCAL_T_IDENTIFIER)
             * 2. A child of an intermediate wrapper node (check child->typ)
             * 3. Wrapped in PASCAL_T_RETURN_TYPE node
             */
            while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC && cursor->typ != PASCAL_T_IDENTIFIER)
            {
                /* Check for RETURN_TYPE wrapper */
                if (cursor->typ == PASCAL_T_RETURN_TYPE) {
                    if (cursor->child != NULL &&
                        (cursor->child->typ == PASCAL_T_TYPE_SPEC || cursor->child->typ == PASCAL_T_IDENTIFIER))
                    {
                        cursor = cursor->child;
                        break;
                    }
                }
                /* Check if the child is a TYPE_SPEC or IDENTIFIER */
                if (cursor->child != NULL && 
                    (cursor->child->typ == PASCAL_T_TYPE_SPEC || cursor->child->typ == PASCAL_T_IDENTIFIER))
                {
                    cursor = cursor->child;
                    break;
                }
                #ifdef DEBUG_KGPC_TYPE_CREATION
                fprintf(stderr, "DEBUG: Skipping node type %d (PASCAL_T_TYPE_SPEC=%d, PASCAL_T_IDENTIFIER=%d)\n", 
                        cursor->typ, PASCAL_T_TYPE_SPEC, PASCAL_T_IDENTIFIER);
                #endif
                cursor = cursor->next;
            }

            if (cursor == NULL)
            {
                /* Fallback: scan the function type node for a return type */
                ast_t *scan = spec_node->child;
                while (scan != NULL)
                {
                    ast_t *node = unwrap_pascal_node(scan);
                    if (node != NULL)
                    {
                        if (node->typ == PASCAL_T_RETURN_TYPE)
                        {
                            if (node->child != NULL)
                            {
                                cursor = node->child;
                                break;
                            }
                        }
                        else if (node->typ == PASCAL_T_TYPE_SPEC || node->typ == PASCAL_T_IDENTIFIER)
                        {
                            cursor = node;
                            break;
                        }
                    }
                    scan = scan->next;
                }
            }
                
            if (cursor != NULL) {
                #ifdef DEBUG_KGPC_TYPE_CREATION
                fprintf(stderr, "DEBUG: Found return type node, typ=%d\n", cursor->typ);
                #endif
                if (cursor->typ == PASCAL_T_TYPE_SPEC) {
                    return_type = convert_type_spec_to_kgpctype(cursor, symtab);
                    owns_return_type = (return_type != NULL);
                    if (return_type_id == NULL && cursor->child != NULL &&
                        cursor->child->sym != NULL && cursor->child->sym->name != NULL)
                    {
                        return_type_id = strdup(cursor->child->sym->name);
                    }
                } else if (cursor->typ == PASCAL_T_IDENTIFIER) {
                    char *ret_type_name = dup_symbol(cursor);
                    if (ret_type_name != NULL) {
                        // First check if it's a primitive type
                        int ret_tag = map_type_name(ret_type_name, NULL);
                        ret_tag = apply_shortstring_mode(ret_tag, ret_type_name);
                        if (ret_tag != UNKNOWN_TYPE) {
                            return_type = create_primitive_type(ret_tag);
                            owns_return_type = 1;
                        } else {
                            // Check if it's a user-defined type in the symbol table
                            HashNode_t *type_node = NULL;
                            if (symtab != NULL && FindSymbol(&type_node, symtab, ret_type_name) != 0 &&
                                type_node != NULL && type_node->type != NULL) {
                                return_type = type_node->type;
                            }
                        }
                        if (return_type_id == NULL)
                            return_type_id = strdup(ret_type_name);
                        free(ret_type_name);
                    }
                }
            }
            #ifdef DEBUG_KGPC_TYPE_CREATION
            else {
                fprintf(stderr, "DEBUG: No return type node found!\n");
            }
            #endif
        }
        
        KgpcType *proc_type = create_procedure_type(params, return_type);
        /* create_procedure_type retains return_type; release our ref if we own it */
        if (owns_return_type && return_type != NULL)
            kgpc_type_release(return_type);
        if (proc_type != NULL) {
            /* create_procedure_type makes a shallow copy of params; tell the
             * type to own (deeply free) its copy so the TREE_VAR_DECL param
             * nodes are eventually freed.  Then free the original list nodes. */
            proc_type->info.proc_info.owns_params = 1;
            DestroyList(params);
            if (return_type_id != NULL)
                proc_type->info.proc_info.return_type_id = return_type_id;
        } else {
            destroy_list(params);
            if (return_type_id != NULL)
                free(return_type_id);
        }
        return proc_type;
    }

    /* Handle reference to types (wraps procedure/function types) */
    if (spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE) {
        // The child should be a PROCEDURE_TYPE or FUNCTION_TYPE
        ast_t *wrapped_type = spec_node->child;
        if (wrapped_type != NULL) {
            return convert_type_spec_to_kgpctype(wrapped_type, symtab);
        }
        return NULL;
    }

    /* Handle record types and legacy object types */
    if (spec_node->typ == PASCAL_T_RECORD_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE) {
        struct RecordType *record = convert_record_type(spec_node);
        if (record != NULL) {
            return create_record_type(record);
        }
        return NULL;
    }

    /* Handle interface types */
    if (spec_node->typ == PASCAL_T_INTERFACE_TYPE) {
        struct RecordType *record = convert_interface_type_ex(NULL, spec_node, NULL);
        if (record != NULL) {
            KgpcType *rec_type = create_record_type(record);
            if (rec_type != NULL) {
                KgpcType *ptr = create_pointer_type(rec_type);
                kgpc_type_release(rec_type);
                return ptr;
            }
        }
        return NULL;
    }

    /* Handle set types */
    if (spec_node->typ == PASCAL_T_SET) {
        /* For sets, we currently just return a primitive SET_TYPE */
        /* In the future, we could extend KgpcType to include set element type info */
        return create_primitive_type(SET_TYPE);
    }

    /* Handle enum types */
    if (spec_node->typ == PASCAL_T_ENUMERATED_TYPE) {
        /* For enums, we currently just return a primitive ENUM_TYPE */
        return create_primitive_type(ENUM_TYPE);
    }

    /* Handle class types */
    if (spec_node->typ == PASCAL_T_CLASS_TYPE) {
        /* Note: We can't get the class name here because we're inside the type spec, not the type decl.
         * For now, we return NULL to let the class be handled by the legacy path.
         * The class's RecordType will be properly populated during semantic checking. */
        return NULL;
    }

    /* Handle "class of T" - class reference type */
    if (spec_node->typ == PASCAL_T_CLASS_OF_TYPE) {
        /* Class reference type (class of TMyClass).
         * This is structurally the same as the target class type - a pointer to the class record.
         * Return NULL here to let it be handled by the convert_type_spec path which creates
         * the appropriate pointer type. */
        return NULL;
    }

    return NULL;
}

static ListNode_t *convert_identifier_list(ast_t **cursor) {
    ListBuilder builder;
    list_builder_init(&builder);
    ast_t *cur = *cursor;
    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(cur);
        if (dup != NULL)
            list_builder_append(&builder, dup, LIST_STRING);
        cur = cur->next;
    }
    *cursor = cur;
    return list_builder_finish(&builder);
}

static ListNode_t *convert_field_decl(ast_t *field_decl_node);
static void convert_record_members(ast_t *node, ListBuilder *builder,
    ListBuilder *property_builder, ListBuilder *method_template_builder);
static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields);
static struct VariantBranch *convert_variant_branch(ast_t *branch_node);
static void qualify_param_decl_types(ListNode_t *params, const char *owner_full,
    const char *owner_outer, SymTab_t *symtab);

static ListNode_t *convert_class_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    int is_class_var = 0;
    for (ast_t *scan = field_decl_node->child; scan != NULL; scan = scan->next)
    {
        ast_t *node = unwrap_pascal_node(scan);
        if (node != NULL && node->sym != NULL && node->sym->name != NULL &&
            strcasecmp(node->sym->name, "class") == 0)
        {
            is_class_var = 1;
            break;
        }
    }

    ast_t *cursor = field_decl_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_IDENTIFIER &&
        cursor->sym != NULL && cursor->sym->name != NULL &&
        strcasecmp(cursor->sym->name, "class") == 0)
    {
        ast_t *next = cursor->next;
        if (next != NULL && next->typ == PASCAL_T_IDENTIFIER &&
            next->sym != NULL && next->sym->name != NULL &&
            strcasecmp(next->sym->name, "var") == 0)
        {
            cursor = next->next;
        }
    }
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        return NULL;
    }

    /* Skip to the type specification */
    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_OBJECT_TYPE &&
           cursor->typ != PASCAL_T_IDENTIFIER && cursor->typ != PASCAL_T_ARRAY_TYPE &&
           cursor->typ != PASCAL_T_SET &&
           cursor->typ != PASCAL_T_POINTER_TYPE &&
           cursor->typ != PASCAL_T_ENUMERATED_TYPE &&
           cursor->typ != PASCAL_T_FILE_TYPE &&
           cursor->typ != PASCAL_T_PROCEDURE_TYPE &&
           cursor->typ != PASCAL_T_FUNCTION_TYPE &&
           cursor->typ != PASCAL_T_REFERENCE_TO_TYPE) {
        cursor = cursor->next;
    }

    char *field_type_id = NULL;
    struct RecordType *nested_record = NULL;
    TypeInfo field_info;
    memset(&field_info, 0, sizeof(TypeInfo));
    int field_type = UNKNOWN_TYPE;

    if (cursor != NULL) {
        /* Use convert_type_spec to properly handle all type forms including arrays */
        field_type = convert_type_spec(cursor, &field_type_id, &nested_record, &field_info);
    } else if (names != NULL) {
        /* Fallback: if no type spec, try to parse last name as type */
        char *candidate = pop_last_identifier(&names);
        if (candidate != NULL) {
            char *mapped_id = NULL;
            int mapped_type = map_type_name(candidate, &mapped_id);
            if (mapped_type != UNKNOWN_TYPE) {
                mapped_type = apply_shortstring_mode(mapped_type, candidate);
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
                free(mapped_id);
                field_type_id = candidate;
            }
        }
    }

    ListBuilder result;
    list_builder_init(&result);

    ListNode_t *name_node = names;
    while (name_node != NULL) {
        char *field_name = (char *)name_node->cur;
        char *type_id_copy = NULL;
        if (field_type_id != NULL)
            type_id_copy = strdup(field_type_id);

        struct RecordType *nested_copy = NULL;
        if (nested_record != NULL) {
            if (name_node->next == NULL) {
                nested_copy = nested_record;
                nested_record = NULL;
            } else {
                nested_copy = clone_record_type(nested_record);
            }
        }

        struct RecordField *field_desc = (struct RecordField *)calloc(1, sizeof(struct RecordField));
        if (field_desc != NULL) {
            field_desc->name = field_name;
            field_desc->type = field_type;
            /* string[N] fields are shortstrings (array[0..N] of Char with length byte) */
            if (field_desc->type == UNKNOWN_TYPE && field_info.is_shortstring)
                field_desc->type = SHORTSTRING_TYPE;
            field_desc->type_id = type_id_copy;
            field_desc->type_ref = type_ref_from_info_or_id(&field_info, type_id_copy);
            field_desc->nested_record = nested_copy;
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_ref =
                type_ref_from_element_info(&field_info, field_info.element_type_id);
            field_desc->array_is_open = field_info.is_open_array;
            /* For multi-name fields (e.g. a, b: array[0..1] of T),
             * duplicate shared resources; transfer ownership only for the last name. */
            if (name_node->next == NULL)
            {
                field_desc->array_element_type_id = field_info.element_type_id;
                field_info.element_type_id = NULL;
                field_desc->array_element_record = field_info.record_type;
                field_info.record_type = NULL;
            }
            else
            {
                field_desc->array_element_type_id =
                    field_info.element_type_id ? strdup(field_info.element_type_id) : NULL;
                field_desc->array_element_record =
                    field_info.record_type ? clone_record_type(field_info.record_type) : NULL;
            }
            /* Transfer pre-built element KgpcType for nested arrays */
            if (field_info.element_kgpc_type != NULL)
            {
                field_desc->array_element_kgpc_type = field_info.element_kgpc_type;
                kgpc_type_retain(field_desc->array_element_kgpc_type);
            }
            field_desc->is_hidden = 0;
            field_desc->is_class_var = is_class_var;
            /* Copy pointer type info for inline pointer fields like ^Char */
            field_desc->is_pointer = field_info.is_pointer;
            field_desc->pointer_type = field_info.pointer_type;
            if (field_info.pointer_type_id != NULL)
                field_desc->pointer_type_id = strdup(field_info.pointer_type_id);
            else
                field_desc->pointer_type_id = NULL;
            field_desc->pointer_type_ref =
                type_ref_from_pointer_info(&field_info, field_info.pointer_type_id);
            /* Copy set element type info for proper set size computation */
            if (field_info.is_set && field_info.set_element_type_id != NULL)
                field_desc->set_element_type_id = strdup(field_info.set_element_type_id);
            list_builder_append(&result, field_desc, LIST_RECORD_FIELD);
        } else {
            if (field_name != NULL)
                free(field_name);
            if (type_id_copy != NULL)
                free(type_id_copy);
            destroy_record_type(nested_copy);
        }

        ListNode_t *next_name = name_node->next;
        free(name_node);
        name_node = next_name;
    }

    if (field_type_id != NULL)
        free(field_type_id);
    if (nested_record != NULL)
        destroy_record_type(nested_record);

    return list_builder_finish(&result);
}

static struct ClassProperty *convert_property_decl(ast_t *property_node)
{
    if (property_node == NULL)
        return NULL;

    char *property_name = NULL;
    ast_t *type_node = NULL;
    char *read_accessor = NULL;
    char *write_accessor = NULL;
    int has_indexer = 0;
    int is_default = 0;
    int next_accessor_is_write = 0; /* set when we see "write" keyword */
    int next_accessor_is_read = 0;  /* set when we see "read" keyword */

    ast_t *cursor = property_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_NONE && cursor->child != NULL)
        cursor = cursor->child;
    if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL)
    {
        fprintf(stderr, "[KGPC] property decl child list:\n");
        for (ast_t *dbg = cursor; dbg != NULL; dbg = dbg->next)
        {
            fprintf(stderr, "  - typ=%d (%s)\n", dbg->typ, pascal_tag_to_string(dbg->typ));
        }
    }
    while (cursor != NULL)
    {
        if (cursor->typ == PASCAL_T_PARAM_LIST)
        {
            has_indexer = 1;
            cursor = cursor->next;
            continue;
        }
        if (cursor->typ == PASCAL_T_DEFAULT_PROPERTY)
        {
            is_default = 1;
            cursor = cursor->next;
            continue;
        }
        ast_t *unwrapped = unwrap_pascal_node(cursor);
        if (unwrapped == NULL)
        {
            cursor = cursor->next;
            continue;
        }

        if (unwrapped->typ == PASCAL_T_IDENTIFIER)
        {
            char *dup = dup_symbol(unwrapped);
            if (dup == NULL)
            {
                cursor = cursor->next;
                continue;
            }

            if (property_name == NULL &&
                (strcasecmp(dup, "class") == 0 || strcasecmp(dup, "generic") == 0))
            {
                free(dup);
                cursor = cursor->next;
                continue;
            }

            if (property_name != NULL && strcasecmp(dup, "default") == 0)
            {
                is_default = 1;
                free(dup);
                cursor = cursor->next;
                continue;
            }

            if (property_name == NULL)
            {
                property_name = dup;
            }
            else if (type_node == NULL)
            {
                type_node = unwrapped;
                free(dup);
            }
            else if (strcasecmp(dup, "read") == 0)
            {
                next_accessor_is_read = 1;
                next_accessor_is_write = 0;
                free(dup);
            }
            else if (strcasecmp(dup, "write") == 0)
            {
                next_accessor_is_write = 1;
                next_accessor_is_read = 0;
                free(dup);
            }
            else if (next_accessor_is_read && read_accessor == NULL)
            {
                read_accessor = dup;
                next_accessor_is_read = 0;
            }
            else if (next_accessor_is_write && write_accessor == NULL)
            {
                write_accessor = dup;
                next_accessor_is_write = 0;
            }
            else if (read_accessor == NULL && !next_accessor_is_write)
            {
                read_accessor = dup;
            }
            else if (write_accessor == NULL)
            {
                write_accessor = dup;
            }
            else
            {
                free(dup);
            }
        }
        else if (type_node == NULL &&
                 (unwrapped->typ == PASCAL_T_TYPE_SPEC ||
                  unwrapped->typ == PASCAL_T_ARRAY_TYPE ||
                  unwrapped->typ == PASCAL_T_POINTER_TYPE ||
                  unwrapped->typ == PASCAL_T_CONSTRUCTED_TYPE ||
                  unwrapped->typ == PASCAL_T_FUNCTION_TYPE ||
                  unwrapped->typ == PASCAL_T_PROCEDURE_TYPE))
        {
            type_node = unwrapped;
        }

        cursor = cursor->next;
    }

    if (property_name == NULL)
        return NULL;
    (void)has_indexer;

    int property_type = UNKNOWN_TYPE;
    char *property_type_id = NULL;
    struct RecordType *inline_record = NULL;
    TypeInfo type_info;
    memset(&type_info, 0, sizeof(TypeInfo));
    TypeRef *property_type_ref = NULL;

    if (type_node != NULL)
    {
        property_type = convert_type_spec(type_node, &property_type_id, &inline_record, &type_info);
        property_type_ref = type_ref_from_info_or_id(&type_info, property_type_id);
        destroy_type_info_contents(&type_info);
        if (inline_record != NULL)
            destroy_record_type(inline_record);
    }

    struct ClassProperty *property = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    if (property == NULL)
    {
        free(property_name);
        free(property_type_id);
        free(read_accessor);
        free(write_accessor);
        return NULL;
    }

    property->name = property_name;
    property->type = property_type;
    property->type_id = property_type_id;
    property->type_ref = property_type_ref;
    if (property->type_ref == NULL)
        property->type_ref = type_ref_from_single_name(property_type_id);
    property->read_accessor = read_accessor;
    property->write_accessor = write_accessor;
    property->is_indexed = has_indexer;
    property->is_default = is_default;

    return property;
}

static void append_module_property_wrappers(ListNode_t **subprograms, ast_t *property_node)
{
    if (subprograms == NULL || property_node == NULL)
        return;

    struct ClassProperty *prop = convert_property_decl(property_node);
    if (prop == NULL)
        return;

    if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL)
    {
        fprintf(stderr,
            "[KGPC] module property name=%s type=%d type_id=%s read=%s write=%s\n",
            prop->name ? prop->name : "<null>",
            prop->type,
            prop->type_id ? prop->type_id : "<null>",
            prop->read_accessor ? prop->read_accessor : "<null>",
            prop->write_accessor ? prop->write_accessor : "<null>");
    }

    if (prop->name != NULL && !prop->is_indexed)
    {
        if (prop->write_accessor != NULL)
        {
            char *proc_name = strdup(prop->name);
            if (proc_name != NULL)
            {
                char *param_name = strdup("Value");
                ListNode_t *param_ids = NULL;
                if (param_name != NULL)
                    param_ids = CreateListNode(param_name, LIST_STRING);

                char *param_type_id = prop->type_id != NULL ? strdup(prop->type_id) : NULL;
                Tree_t *param_decl = NULL;
                if (param_ids != NULL)
                    param_decl = mk_vardecl(0, param_ids, prop->type, param_type_id,
                        0, 0, NULL, NULL, NULL, NULL);
                if (param_decl == NULL && param_type_id != NULL)
                    free(param_type_id);
                if (param_decl != NULL)
                    param_decl->tree_data.var_decl_data.type_ref =
                        prop->type_ref != NULL ? type_ref_clone(prop->type_ref)
                                               : type_ref_from_single_name(param_type_id);

                ListNode_t *params = NULL;
                if (param_decl != NULL)
                    params = CreateListNode(param_decl, LIST_TREE);

                struct Expression *value_expr = mk_varid(0, strdup("Value"));
                ListNode_t *call_args = NULL;
                if (value_expr != NULL)
                    call_args = CreateListNode(value_expr, LIST_EXPR);
                struct Statement *call_stmt = NULL;
                if (call_args != NULL)
                    call_stmt = mk_procedurecall(0, strdup(prop->write_accessor), call_args);

                ListNode_t *body_list = NULL;
                if (call_stmt != NULL)
                    body_list = CreateListNode(call_stmt, LIST_STMT);
                struct Statement *body_stmt = NULL;
                if (body_list != NULL)
                    body_stmt = mk_compoundstatement(0, body_list);

                Tree_t *proc_tree = NULL;
                if (body_stmt != NULL)
                    proc_tree = mk_procedure(0, proc_name, params, NULL, NULL, NULL, NULL, NULL,
                        body_stmt, 0, 0);

                if (proc_tree != NULL)
                {
                    append_subprogram_node(subprograms, proc_tree);
                }
                else
                {
                    if (body_stmt != NULL)
                        destroy_stmt(body_stmt);
                    free(proc_name);
                }
            }
        }

        if (prop->read_accessor != NULL)
        {
            char *func_name = strdup(prop->name);
            if (func_name != NULL)
            {
                struct Expression *accessor_call = mk_functioncall(0, strdup(prop->read_accessor), NULL);
                if (accessor_call != NULL)
                {
                    struct Expression *result_var = mk_varid(0, strdup("Result"));
                    if (result_var != NULL)
                    {
                        struct Statement *assign_stmt = mk_varassign(0, 0, result_var, accessor_call);
                        if (assign_stmt != NULL)
                        {
                            ListNode_t *body_list = CreateListNode((void *)assign_stmt, LIST_STMT);
                            struct Statement *body_stmt = mk_compoundstatement(0, body_list);
                            if (body_stmt != NULL)
                            {
                                char *return_type_id = prop->type_id != NULL ? strdup(prop->type_id) : NULL;
                                Tree_t *func_tree = mk_function(0, func_name, NULL, NULL, NULL, NULL, NULL, NULL, body_stmt,
                                    prop->type, return_type_id, NULL, 0, 0);
                                if (func_tree != NULL)
                                {
                                    func_tree->tree_data.subprogram_data.return_type_ref =
                                        prop->type_ref != NULL ? type_ref_clone(prop->type_ref)
                                                               : type_ref_from_single_name(return_type_id);
                                    append_subprogram_node(subprograms, func_tree);
                                    if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL)
                                    {
                                        fprintf(stderr, "[KGPC] Created synthetic function '%s' for module property\n", prop->name);
                                    }
                                }
                                else
                                {
                                    destroy_stmt(body_stmt);
                                    free(func_name);
                                }
                            }
                            else
                            {
                                destroy_stmt(assign_stmt);
                                free(func_name);
                            }
                        }
                        else
                        {
                            destroy_expr(accessor_call);
                            destroy_expr(result_var);
                            free(func_name);
                        }
                    }
                    else
                    {
                        destroy_expr(accessor_call);
                        free(func_name);
                    }
                }
                else
                {
                    free(func_name);
                }
            }
        }
    }

    if (prop->name) free(prop->name);
    if (prop->type_id) free(prop->type_id);
    if (prop->read_accessor) free(prop->read_accessor);
    if (prop->write_accessor) free(prop->write_accessor);
    free(prop);
}

static void annotate_method_template(struct MethodTemplate *method_template, ast_t *method_ast)
{
    if (method_template == NULL || method_ast == NULL)
        return;

    method_template->kind = METHOD_TEMPLATE_UNKNOWN;

    /* Check the node type for constructor/destructor declarations.
     * The parser uses PASCAL_T_CONSTRUCTOR_DECL / PASCAL_T_DESTRUCTOR_DECL
     * for methods declared with the 'constructor' / 'destructor' keyword,
     * so the kind must be set from the node type, not from child sym_names. */
    if (method_ast->typ == PASCAL_T_CONSTRUCTOR_DECL)
        method_template->kind = METHOD_TEMPLATE_CONSTRUCTOR;
    else if (method_ast->typ == PASCAL_T_DESTRUCTOR_DECL)
        method_template->kind = METHOD_TEMPLATE_DESTRUCTOR;
    
    /* First pass: check ALL children for "class" keyword before the method name.
     * The parser produces an IDENTIFIER child with sym->name="class" when
     * create_keyword_parser("class", PASCAL_T_IDENTIFIER) matches. */
    ast_t *cursor = method_ast->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;
        const char *sym_name = (node->sym != NULL) ? node->sym->name : NULL;

        /* Check for "class" keyword in any child node.
         * Note: "class function" has Self = VMT pointer; "class function ... static" has no Self.
         * Only set is_class_method here; is_static is set by the "static" directive. */
        if (sym_name != NULL && strcasecmp(sym_name, "class") == 0) {
            method_template->is_class_method = 1;
        }
        cursor = cursor->next;
    }
    
    /* Second pass: process all other attributes */
    cursor = method_ast->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;

        const char *sym_name = (node->sym != NULL) ? node->sym->name : NULL;
        switch (node->typ)
        {
            case PASCAL_T_IDENTIFIER:
                if (method_template->return_type_ast == NULL &&
                    sym_name != NULL &&
                    method_template->name != NULL &&
                    strcasecmp(sym_name, method_template->name) != 0 &&
                    !is_method_decl_keyword(sym_name))
                {
                    method_template->return_type_ast = node;
                    method_template->has_return_type = 1;
                }
                break;
            case PASCAL_T_PARAM_LIST:
            case PASCAL_T_PARAM:
                if (method_template->params_ast == NULL)
                    method_template->params_ast = node;
                break;
            case PASCAL_T_METHOD_DIRECTIVE:
                method_template->directives_ast = node;
                /* Check for directive keywords (virtual, override, static) in symbol name and children */
                if (sym_name != NULL)
                {
                    if (strcasecmp(sym_name, "virtual") == 0)
                        method_template->is_virtual = 1;
                    else if (strcasecmp(sym_name, "abstract") == 0)
                        method_template->is_virtual = 1;  /* Abstract methods are implicitly virtual */
                    else if (strcasecmp(sym_name, "override") == 0)
                    {
                        method_template->is_override = 1;
                        method_template->is_virtual = 1;
                    }
                    else if (strcasecmp(sym_name, "static") == 0)
                        method_template->is_static = 1;
                }
                /* Also check the child for all directive keywords */
                if (node->child != NULL) {
                    ast_t *dir_child = unwrap_pascal_node(node->child);
                    if (dir_child != NULL && dir_child->sym != NULL && dir_child->sym->name != NULL) {
                        const char *child_name = dir_child->sym->name;
                        if (strcasecmp(child_name, "static") == 0)
                            method_template->is_static = 1;
                        else if (strcasecmp(child_name, "abstract") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(child_name, "virtual") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(child_name, "override") == 0) {
                            method_template->is_override = 1;
                            method_template->is_virtual = 1;
                        }
                    }
                }
                /* Also recursively check all children of the directive to find directives */
                ast_t *dir_cur = node->child;
                while (dir_cur != NULL) {
                    ast_t *unwrapped_dir = unwrap_pascal_node(dir_cur);
                    if (unwrapped_dir != NULL && unwrapped_dir->sym != NULL &&
                        unwrapped_dir->sym->name != NULL) {
                        const char *dir_name = unwrapped_dir->sym->name;
                        if (strcasecmp(dir_name, "static") == 0)
                            method_template->is_static = 1;
                        else if (strcasecmp(dir_name, "abstract") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(dir_name, "virtual") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(dir_name, "override") == 0) {
                            method_template->is_override = 1;
                            method_template->is_virtual = 1;
                        }
                    }
                    /* Check identifier child of token */
                    if (unwrapped_dir != NULL && unwrapped_dir->typ == PASCAL_T_IDENTIFIER &&
                        unwrapped_dir->sym != NULL && unwrapped_dir->sym->name != NULL) {
                        const char *id_name = unwrapped_dir->sym->name;
                        if (strcasecmp(id_name, "static") == 0)
                            method_template->is_static = 1;
                        else if (strcasecmp(id_name, "abstract") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(id_name, "virtual") == 0)
                            method_template->is_virtual = 1;
                        else if (strcasecmp(id_name, "override") == 0) {
                            method_template->is_override = 1;
                            method_template->is_virtual = 1;
                        }
                    }
                    dir_cur = dir_cur->next;
                }
                break;
            case PASCAL_T_RETURN_TYPE:
                method_template->return_type_ast = node;
                method_template->has_return_type = 1;
                break;
            default:
                if (sym_name != NULL)
                {
                    if (strcasecmp(sym_name, "class") == 0) {
                        method_template->is_class_method = 1;
                        method_template->is_static = 1;
                    }
                    else if (strcasecmp(sym_name, "static") == 0)
                        method_template->is_static = 1;
                    else if (strcasecmp(sym_name, "constructor") == 0)
                        method_template->kind = METHOD_TEMPLATE_CONSTRUCTOR;
                    else if (strcasecmp(sym_name, "destructor") == 0)
                        method_template->kind = METHOD_TEMPLATE_DESTRUCTOR;
                    else if (strcasecmp(sym_name, "function") == 0)
                        method_template->kind = METHOD_TEMPLATE_FUNCTION;
                    else if (strcasecmp(sym_name, "procedure") == 0)
                        method_template->kind = METHOD_TEMPLATE_PROCEDURE;
                    else if (strcasecmp(sym_name, "operator") == 0)
                        method_template->kind = METHOD_TEMPLATE_OPERATOR;
                }
                break;
        }
        cursor = cursor->next;
    }

    if (method_template->kind == METHOD_TEMPLATE_UNKNOWN)
    {
        method_template->kind = method_template->has_return_type ?
            METHOD_TEMPLATE_FUNCTION : METHOD_TEMPLATE_PROCEDURE;
    }
    if (method_template->return_type_ast == NULL)
    {
        method_template->return_type_ast = find_ast_node_type(method_ast, PASCAL_T_RETURN_TYPE);
        if (method_template->return_type_ast != NULL)
            method_template->has_return_type = 1;
    }
}

static int extract_interface_delegation_info(ast_t *method_decl_node,
    char **iface_name_out, char **iface_method_out, char **target_name_out)
{
    if (iface_name_out != NULL)
        *iface_name_out = NULL;
    if (iface_method_out != NULL)
        *iface_method_out = NULL;
    if (target_name_out != NULL)
        *target_name_out = NULL;
    if (method_decl_node == NULL)
        return 0;

    ast_t *significant[3] = {0};
    int count = 0;
    for (ast_t *cur = method_decl_node->child; cur != NULL; cur = cur->next)
    {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;
        if (node == NULL)
            continue;

        if (node->typ == PASCAL_T_PARAM_LIST || node->typ == PASCAL_T_PARAM ||
            node->typ == PASCAL_T_RETURN_TYPE || node->typ == PASCAL_T_METHOD_DIRECTIVE)
            return 0;

        if (node->typ == PASCAL_T_IDENTIFIER || node->typ == PASCAL_T_QUALIFIED_IDENTIFIER ||
            (node->typ == PASCAL_T_NONE && node->child != NULL))
        {
            if (count >= 3)
                return 0;
            significant[count++] = node;
        }
    }

    if (count != 3)
        return 0;

    QualifiedIdent *iface_qid = qualified_ident_from_ast(significant[0]);
    QualifiedIdent *target_qid = qualified_ident_from_ast(significant[2]);
    char *iface_name = iface_qid != NULL ? qualified_ident_join(iface_qid, ".") : dup_symbol(significant[0]);
    char *iface_method = dup_symbol(significant[1]);
    char *target_name = NULL;
    if (target_qid != NULL)
    {
        const char *last = qualified_ident_last(target_qid);
        if (last != NULL)
            target_name = strdup(last);
    }
    if (target_name == NULL)
        target_name = dup_symbol(significant[2]);
    if (iface_qid != NULL)
        qualified_ident_free(iface_qid);
    if (target_qid != NULL)
        qualified_ident_free(target_qid);

    if (iface_name == NULL || iface_method == NULL || target_name == NULL)
    {
        free(iface_name);
        free(iface_method);
        free(target_name);
        return 0;
    }

    if (iface_name_out != NULL)
        *iface_name_out = iface_name;
    else
        free(iface_name);
    if (iface_method_out != NULL)
        *iface_method_out = iface_method;
    else
        free(iface_method);
    if (target_name_out != NULL)
        *target_name_out = target_name;
    else
        free(target_name);
    return 1;
}

static struct MethodTemplate *create_method_template(ast_t *method_decl_node)
{
    if (method_decl_node == NULL)
        return NULL;

    struct MethodTemplate *template = (struct MethodTemplate *)calloc(1, sizeof(struct MethodTemplate));
    if (template == NULL)
        return NULL;

    if (!extract_interface_delegation_info(method_decl_node,
            &template->delegated_interface_name, &template->name,
            &template->delegated_target_name))
    {
        ast_t *name_node = method_decl_node->child;
        while (name_node != NULL)
        {
            if (name_node->typ == PASCAL_T_IDENTIFIER)
            {
                const char *sym_name = name_node->sym != NULL ? name_node->sym->name : NULL;
                if (sym_name != NULL &&
                    (is_method_decl_keyword(sym_name) || strcasecmp(sym_name, "generic") == 0))
                {
                    name_node = name_node->next;
                    continue;
                }
                break;
            }
            name_node = name_node->next;
        }
        if (name_node == NULL || name_node->sym == NULL || name_node->sym->name == NULL)
        {
            free(template);
            return NULL;
        }
        template->name = strdup(name_node->sym->name);
        if (template->name == NULL)
        {
            free(template);
            return NULL;
        }
    }
    else
    {
        template->is_interface_delegation = 1;
    }

    template->method_ast = copy_ast_detached(method_decl_node);
    if (template->method_ast == NULL)
    {
        free(template->name);
        free(template->delegated_interface_name);
        free(template->delegated_target_name);
        free(template);
        return NULL;
    }

    annotate_method_template(template, template->method_ast);
    template->method_impl_ast = NULL;
    template->source_offset = g_source_offset;
    template->owns_ast = 1; /* Template owns its detached AST copy */
    return template;
}

static void destroy_method_template_instance(struct MethodTemplate *template)
{
    if (template == NULL)
        return;
    if (template->name != NULL)
        free(template->name);
    if (template->method_tree != NULL)
        destroy_tree(template->method_tree);
    free(template);
}

static void collect_class_members(ast_t *node, const char *class_name,
    ListBuilder *field_builder, ListBuilder *property_builder,
    ListBuilder *method_builder, ListBuilder *nested_type_builder) {
    if (node == NULL)
        return;

    ast_t *cursor = node;
    while (cursor != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(cursor);
        if (unwrapped != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                fprintf(stderr, "[KGPC] collect_class_members: node typ=%d (%s) raw_typ=%d sym=%s in %s\n",
                    unwrapped->typ, pascal_tag_to_string(unwrapped->typ),
                    cursor->typ,
                    (cursor->sym && cursor->sym->name) ? cursor->sym->name : "(null)",
                    class_name ? class_name : "<unknown>");
            }
            switch (unwrapped->typ) {
            case PASCAL_T_CLASS_MEMBER:
            {
                int saw_class = 0;
                int saw_var = 0;
                for (ast_t *scan = unwrapped->child; scan != NULL; scan = scan->next)
                {
                    ast_t *node = unwrap_pascal_node(scan);
                    if (node != NULL && node->sym != NULL && node->sym->name != NULL)
                    {
                        if (strcasecmp(node->sym->name, "class") == 0)
                            saw_class = 1;
                        else if (strcasecmp(node->sym->name, "var") == 0)
                            saw_var = 1;
                    }
                }
                if (saw_class && saw_var)
                {
                    for (ast_t *child = unwrapped->child; child != NULL; child = child->next)
                    {
                        ast_t *node = unwrap_pascal_node(child);
                        if (node != NULL && node->typ == PASCAL_T_FIELD_DECL)
                        {
                            ListNode_t *fields = convert_class_field_decl(node);
                            if (fields != NULL)
                            {
                                for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next)
                                {
                                    if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                    {
                                        struct RecordField *field = (struct RecordField *)fnode->cur;
                                        field->is_class_var = 1;
                                    }
                                }
                            }
                            list_builder_extend(field_builder, fields);
                        }
                    }
                }
                else if (saw_class)
                {
                    /* "class function/procedure" member: propagate is_class_method to templates */
                    for (ast_t *child = unwrapped->child; child != NULL; child = child->next)
                    {
                        ast_t *cn = unwrap_pascal_node(child);
                        if (cn == NULL) cn = child;
                        if (cn->typ == PASCAL_T_METHOD_DECL ||
                            cn->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                            cn->typ == PASCAL_T_DESTRUCTOR_DECL)
                        {
                            struct MethodTemplate *template = create_method_template(cn);
                            if (template != NULL)
                            {
                                template->is_class_method = 1;
                                if (!template->is_interface_delegation) {
                                    int param_count = from_cparser_count_params_ast(template->params_ast);
                                    char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                                    register_class_method_ex(class_name, template->name,
                                        template->is_virtual, template->is_override, template->is_static,
                                        template->is_class_method,
                                        param_count, param_sig);
                                }
                                if (method_builder != NULL)
                                    list_builder_append(method_builder, template, LIST_METHOD_TEMPLATE);
                                else
                                    destroy_method_template_instance(template);
                            }
                        }
                        else if (cn->typ == PASCAL_T_PROPERTY_DECL)
                        {
                            struct ClassProperty *property = convert_property_decl(cn);
                            if (property != NULL && property_builder != NULL)
                                list_builder_append(property_builder, property, LIST_CLASS_PROPERTY);
                        }
                    }
                }
                else
                {
                    collect_class_members(unwrapped->child, class_name, field_builder, property_builder,
                        method_builder, nested_type_builder);
                }
                break;
            }
            case PASCAL_T_FIELD_DECL: {
                ListNode_t *fields = convert_class_field_decl(unwrapped);
                list_builder_extend(field_builder, fields);
                break;
            }
            case PASCAL_T_VAR_SECTION: {
                /* Handle var / class var sections inside classes. */
                int is_class_var_section = 0;
                if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR_PARSE") != NULL)
                {
                    fprintf(stderr, "[KGPC] class var section nodes:");
                    for (ast_t *dbg = unwrapped->child; dbg != NULL; dbg = dbg->next)
                    {
                        ast_t *node = unwrap_pascal_node(dbg);
                        const char *name = (node != NULL && node->sym != NULL) ? node->sym->name : NULL;
                        fprintf(stderr, " (%s:%d)", name ? name : "<null>",
                            node != NULL ? node->typ : -1);
                    }
                    fprintf(stderr, "\n");
                }
                for (ast_t *scan = unwrapped->child; scan != NULL; scan = scan->next)
                {
                    ast_t *node = unwrap_pascal_node(scan);
                    if (node != NULL && node->sym != NULL && node->sym->name != NULL &&
                        strcasecmp(node->sym->name, "class") == 0)
                    {
                        is_class_var_section = 1;
                        break;
                    }
                }
                for (ast_t *child = unwrapped->child; child != NULL; child = child->next)
                {
                    ast_t *node = unwrap_pascal_node(child);
                    if (node != NULL && node->typ == PASCAL_T_FIELD_DECL)
                    {
                        ListNode_t *fields = convert_class_field_decl(node);
                        if (is_class_var_section && fields != NULL)
                        {
                            for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next)
                            {
                                if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                {
                                    struct RecordField *field = (struct RecordField *)fnode->cur;
                                    field->is_class_var = 1;
                                }
                            }
                        }
                        list_builder_extend(field_builder, fields);
                    }
                }
                break;
            }
            case PASCAL_T_METHOD_DECL:
            case PASCAL_T_CONSTRUCTOR_DECL:
            case PASCAL_T_DESTRUCTOR_DECL: {
                struct MethodTemplate *template = create_method_template(unwrapped);
                if (template == NULL)
                    break;

                if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL && template->name != NULL)
                    fprintf(stderr, "[KGPC] captured template %s.%s\n",
                        class_name != NULL ? class_name : "<unknown>", template->name);

                if (!template->is_interface_delegation) {
                    int param_count = from_cparser_count_params_ast(template->params_ast);
                    char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                    register_class_method_ex(class_name, template->name,
                        template->is_virtual, template->is_override, template->is_static,
                        template->is_class_method,
                        param_count, param_sig);
                }

                if (method_builder != NULL)
                    list_builder_append(method_builder, template, LIST_METHOD_TEMPLATE);
                else
                    destroy_method_template_instance(template);
                break;
            }
            case PASCAL_T_PROPERTY_DECL: {
                struct ClassProperty *property = convert_property_decl(unwrapped);
                if (property != NULL && property_builder != NULL)
                    list_builder_append(property_builder, property, LIST_CLASS_PROPERTY);
                break;
            }
            case PASCAL_T_TYPE_SECTION:
            case PASCAL_T_NESTED_TYPE_SECTION: {
                /* Nested type declarations inside record/class (Delphi syntax: public type ...) */
                if (nested_type_builder != NULL) {
                    /* Store a pointer to the AST node for later processing */
                    list_builder_append(nested_type_builder, unwrapped, LIST_UNSPECIFIED);
                    if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                        fprintf(stderr, "[KGPC] collect_class_members: found NESTED_TYPE_SECTION in %s at line %d\n",
                            class_name ? class_name : "<unknown>", unwrapped->line);
                }
                break;
            }
            case PASCAL_T_CLASS_BODY:
            case PASCAL_T_ACCESS_MODIFIER:
            case PASCAL_T_PRIVATE_SECTION:
            case PASCAL_T_PUBLIC_SECTION:
            case PASCAL_T_PROTECTED_SECTION:
            case PASCAL_T_PUBLISHED_SECTION:
                collect_class_members(unwrapped->child, class_name, field_builder,
                    property_builder, method_builder, nested_type_builder);
                break;
            default:
                break;
            }
        }
        cursor = cursor->next;
    }
}

static struct RecordType *convert_class_type_ex(const char *class_name, ast_t *class_node, ListNode_t **nested_types_out) {
    if (class_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL && class_name != NULL)
        fprintf(stderr, "[KGPC] convert_class_type %s\n", class_name);

    ListBuilder field_builder;
    ListBuilder property_builder;
    ListBuilder method_template_builder;
    ListBuilder nested_type_builder;
    list_builder_init(&field_builder);
    list_builder_init(&property_builder);
    list_builder_init(&method_template_builder);
    list_builder_init(&nested_type_builder);

    // Check if first child is a parent class identifier
    char *parent_class_name = NULL;
    ast_t *body_start = class_node->child;

    while (body_start != NULL && body_start->typ == PASCAL_T_NONE &&
        body_start->child != NULL && body_start->next == NULL)
    {
        body_start = body_start->child;
    }

    if (body_start != NULL && body_start->typ == PASCAL_T_IDENTIFIER) {
        // First child is parent class name, extract it
        if (body_start->sym != NULL && body_start->sym->name != NULL)
            parent_class_name = strdup(body_start->sym->name);
        // Move to the actual class body (next sibling)
        body_start = body_start->next;
    }

    while (body_start != NULL && body_start->typ == PASCAL_T_NONE &&
        body_start->child != NULL && body_start->next == NULL)
    {
        body_start = body_start->child;
    }

    /* Collect additional IDENTIFIER siblings as interface names */
    int iface_count = 0;
    int iface_cap = 0;
    char **iface_names = NULL;
    {
        ast_t *scan = body_start;
        while (scan != NULL && scan->typ == PASCAL_T_IDENTIFIER) {
            if (scan->sym != NULL && scan->sym->name != NULL) {
                if (iface_count >= iface_cap) {
                    iface_cap = (iface_cap == 0) ? 4 : iface_cap * 2;
                    iface_names = (char **)realloc(iface_names, iface_cap * sizeof(char *));
                }
                iface_names[iface_count++] = strdup(scan->sym->name);
            }
            scan = scan->next;
        }
        body_start = scan;
    }

    if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_class_type: processing class %s\n", class_name ? class_name : "<null>");
        if (body_start != NULL) {
            fprintf(stderr, "[KGPC]   body_start type: %d\n", body_start->typ);
        } else {
            fprintf(stderr, "[KGPC]   body_start is NULL\n");
        }
        if (class_name != NULL &&
            (strcasecmp(class_name, "TList") == 0 ||
             strcasecmp(class_name, "TStringList") == 0 ||
             strcasecmp(class_name, "TComponent") == 0 ||
             strcasecmp(class_name, "TInterfaceList") == 0)) {
            ast_t *raw = class_node->child;
            int idx = 0;
            while (raw != NULL && idx < 12) {
                fprintf(stderr, "[KGPC]   raw[%d] typ=%d name=%s child=%p next=%p\n",
                    idx, raw->typ,
                    (raw->sym && raw->sym->name) ? raw->sym->name : "<null>",
                    (void *)raw->child, (void *)raw->next);
                raw = raw->next;
                idx++;
            }
        }
        /* Debug: show additional parent identifiers (interfaces) */
        ast_t *dbg = body_start;
        while (dbg != NULL && dbg->typ == PASCAL_T_IDENTIFIER) {
            fprintf(stderr, "[KGPC]   additional parent/interface: %s (type=%d)\n",
                (dbg->sym && dbg->sym->name) ? dbg->sym->name : "<null>", dbg->typ);
            dbg = dbg->next;
        }
    }

    collect_class_members(body_start, class_name, &field_builder, &property_builder,
        &method_template_builder, &nested_type_builder);

    /* Output collected nested type sections */
    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(field_builder.head);
        destroy_list(property_builder.head);
        destroy_list(method_template_builder.head);
        free(parent_class_name);
        return NULL;
    }

    record->fields = list_builder_finish(&field_builder);
    record->properties = list_builder_finish(&property_builder);
    record->method_templates = list_builder_finish(&method_template_builder);
    
    record->parent_class_name = parent_class_name;
    record->methods = NULL;  /* Methods list will be populated during semantic checking */
    record->is_class = 1;
    record->is_interface = 0;
    record->is_packed = 0;
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = class_name != NULL ? strdup(class_name) : NULL;
    record->outer_type_id = NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->is_generic_specialization = 0;
    record->method_clones_emitted = 0;
    record->parent_fields_merged = 0;
    record->source_unit_index = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->default_indexed_element_type_id = NULL;
    record->record_properties = NULL;
    record->guid_string = NULL;
    record->has_guid = 0;
    record->guid_d1 = 0;
    record->guid_d2 = 0;
    record->guid_d3 = 0;
    memset(record->guid_d4, 0, sizeof(record->guid_d4));
    record->has_guid = 0;
    record->guid_d1 = 0;
    record->guid_d2 = 0;
    record->guid_d3 = 0;
    memset(record->guid_d4, 0, sizeof(record->guid_d4));
    record->interface_names = iface_names;
    record->num_interfaces = iface_count;

    if (parent_class_name == NULL)
    {
        struct RecordField *typeinfo_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
        if (typeinfo_field != NULL)
        {
            typeinfo_field->name = strdup("__kgpc_class_typeinfo");
            typeinfo_field->type = POINTER_TYPE;
            typeinfo_field->type_id = NULL;
            typeinfo_field->nested_record = NULL;
            typeinfo_field->is_array = 0;
            typeinfo_field->array_start = 0;
            typeinfo_field->array_end = 0;
            typeinfo_field->array_element_type = UNKNOWN_TYPE;
            typeinfo_field->array_element_type_id = NULL;
            typeinfo_field->array_is_open = 0;
            typeinfo_field->is_hidden = 1;
            ListNode_t *node = CreateListNode(typeinfo_field, LIST_RECORD_FIELD);
            if (node != NULL) {
                if (record->fields == NULL) {
                    record->fields = node;
                } else {
                    record->fields = PushListNodeFront(record->fields, node);
                }
            }
            else
            {
                free(typeinfo_field->name);
                free(typeinfo_field);
            }
        }
    }
    return record;
}

static struct RecordType *convert_interface_type_ex(const char *interface_name, ast_t *interface_node, ListNode_t **nested_types_out)
{
    if (interface_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    ListBuilder field_builder;
    ListBuilder property_builder;
    ListBuilder method_template_builder;
    ListBuilder nested_type_builder;
    list_builder_init(&field_builder);
    list_builder_init(&property_builder);
    list_builder_init(&method_template_builder);
    list_builder_init(&nested_type_builder);

    /* Optional parent interface identifier is stored first if present. */
    char *parent_interface_name = NULL;
    char *guid_string = NULL;
    int has_guid = 0;
    uint32_t guid_d1 = 0;
    uint16_t guid_d2 = 0;
    uint16_t guid_d3 = 0;
    uint8_t guid_d4[8] = {0};
    ast_t *body_start = interface_node->child;
    /* Skip optional GUID attribute: ['{...}'] or [SGUIDConst].
     * With PASCAL_T_INTERFACE_GUID, the GUID is now wrapped in its own node. */
    if (body_start != NULL && body_start->typ == PASCAL_T_INTERFACE_GUID) {
        /* Extract GUID string from inside the GUID node if available */
        ast_t *guid_child = body_start->child;
        while (guid_child != NULL) {
            if ((guid_child->typ == PASCAL_T_STRING ||
                 guid_child->typ == PASCAL_T_IDENTIFIER) &&
                guid_child->sym != NULL && guid_child->sym->name != NULL) {
                guid_string = strdup(guid_child->sym->name);
                has_guid = parse_guid_literal(guid_child->sym->name, &guid_d1, &guid_d2, &guid_d3, guid_d4);
                break;
            }
            guid_child = guid_child->next;
        }
        body_start = body_start->next;
    }

    if (body_start != NULL && body_start->typ == PASCAL_T_IDENTIFIER) {
        if (body_start->sym != NULL && body_start->sym->name != NULL)
            parent_interface_name = strdup(body_start->sym->name);
        body_start = body_start->next;
    }

    /* Optional GUID string follows parent identifier (when GUID is a string literal) */
    if (body_start != NULL && body_start->typ == PASCAL_T_STRING) {
        if (body_start->sym != NULL && body_start->sym->name != NULL) {
            free(guid_string);
            guid_string = strdup(body_start->sym->name);
            has_guid = parse_guid_literal(body_start->sym->name, &guid_d1, &guid_d2, &guid_d3, guid_d4);
        }
        body_start = body_start->next;
    }

    /* Skip GUID attribute if it follows the parent identifier */
    if (body_start != NULL && body_start->typ == PASCAL_T_INTERFACE_GUID) {
        ast_t *guid_child = body_start->child;
        while (guid_child != NULL && !has_guid) {
            if ((guid_child->typ == PASCAL_T_STRING ||
                 guid_child->typ == PASCAL_T_IDENTIFIER) &&
                guid_child->sym != NULL && guid_child->sym->name != NULL) {
                free(guid_string);
                guid_string = strdup(guid_child->sym->name);
                has_guid = parse_guid_literal(guid_child->sym->name, &guid_d1, &guid_d2, &guid_d3, guid_d4);
                break;
            }
            guid_child = guid_child->next;
        }
        body_start = body_start->next;
    }

    collect_class_members(body_start, interface_name, &field_builder, &property_builder,
        &method_template_builder, &nested_type_builder);

    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    struct RecordType *record = (struct RecordType *)calloc(1, sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(field_builder.head);
        destroy_list(property_builder.head);
        destroy_list(method_template_builder.head);
        free(parent_interface_name);
        return NULL;
    }

    record->fields = list_builder_finish(&field_builder);
    record->properties = list_builder_finish(&property_builder);
    record->method_templates = list_builder_finish(&method_template_builder);
    record->parent_class_name = parent_interface_name;
    /* In FPC, interfaces without an explicit parent implicitly inherit from
     * IInterface (a.k.a. IUnknown).  Set the default parent so that methods
     * like _AddRef, _Release, QueryInterface are found by the class-method
     * walker when resolving field access on derived interfaces. */
    if (record->parent_class_name == NULL && interface_name != NULL &&
        !pascal_identifier_equals(interface_name, "IInterface") &&
        !pascal_identifier_equals(interface_name, "IUnknown"))
    {
        record->parent_class_name = strdup("IInterface");
    }
    record->methods = NULL;
    record->is_class = 0;
    record->is_interface = 1;
    record->is_packed = 0;
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = interface_name != NULL ? strdup(interface_name) : NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->is_generic_specialization = 0;
    record->is_generic_specialization = 0;
    record->method_clones_emitted = 0;
    record->parent_fields_merged = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->default_indexed_element_type_id = NULL;
    record->record_properties = NULL;
    record->guid_string = guid_string;
    record->has_guid = has_guid;
    record->guid_d1 = guid_d1;
    record->guid_d2 = guid_d2;
    record->guid_d3 = guid_d3;
    memcpy(record->guid_d4, guid_d4, sizeof(record->guid_d4));
    record->interface_names = NULL;
    record->num_interfaces = 0;

    return record;
}

static ListNode_t *convert_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    int is_class_var = 0;
    int saw_class = 0;
    int saw_var = 0;
    if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR_PARSE") != NULL)
    {
        fprintf(stderr, "[KGPC] field_decl identifiers:");
        for (ast_t *dbg = field_decl_node->child; dbg != NULL; dbg = dbg->next)
        {
            ast_t *node = unwrap_pascal_node(dbg);
            if (node != NULL && node->sym != NULL && node->sym->name != NULL)
                fprintf(stderr, " %s", node->sym->name);
        }
        fprintf(stderr, "\n");
    }
    for (ast_t *scan = field_decl_node->child; scan != NULL; scan = scan->next)
    {
        ast_t *node = unwrap_pascal_node(scan);
        if (node != NULL && node->sym != NULL && node->sym->name != NULL)
        {
            if (strcasecmp(node->sym->name, "class") == 0)
                saw_class = 1;
            else if (strcasecmp(node->sym->name, "var") == 0)
                saw_var = 1;
        }
    }
    if (saw_class && saw_var)
        is_class_var = 1;
    ast_t *cursor = field_decl_node->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
        {
            cursor = cursor->next;
            continue;
        }
        if (node->typ == PASCAL_T_IDENTIFIER &&
            node->sym != NULL && node->sym->name != NULL &&
            (strcasecmp(node->sym->name, "class") == 0 ||
             strcasecmp(node->sym->name, "var") == 0))
        {
            cursor = cursor->next;
            continue;
        }
        if (node->typ != PASCAL_T_IDENTIFIER)
        {
            cursor = cursor->next;
            continue;
        }
        break;
    }
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        fprintf(stderr, "ERROR: record field declaration missing identifier list.\n");
        return NULL;
    }

    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_OBJECT_TYPE &&
           cursor->typ != PASCAL_T_IDENTIFIER && cursor->typ != PASCAL_T_ARRAY_TYPE &&
           cursor->typ != PASCAL_T_SET &&
           cursor->typ != PASCAL_T_POINTER_TYPE &&
           cursor->typ != PASCAL_T_ENUMERATED_TYPE &&
           cursor->typ != PASCAL_T_FILE_TYPE &&
           cursor->typ != PASCAL_T_PROCEDURE_TYPE &&
           cursor->typ != PASCAL_T_FUNCTION_TYPE &&
           cursor->typ != PASCAL_T_REFERENCE_TO_TYPE) {
        cursor = cursor->next;
    }

    char *field_type_id = NULL;
    struct RecordType *nested_record = NULL;
    TypeInfo field_info;
    memset(&field_info, 0, sizeof(TypeInfo));
    int field_type = UNKNOWN_TYPE;
    KgpcType *inline_proc_type = NULL;

    if (cursor != NULL) {
        field_type = convert_type_spec(cursor, &field_type_id, &nested_record, &field_info);
        if (field_type == UNKNOWN_TYPE && field_info.is_range)
            field_type = select_range_primitive_tag(&field_info);
        /* Capture inline procedural type signatures for record fields */
        {
            ast_t *spec_node = cursor;
            if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
                spec_node = spec_node->child;
            spec_node = unwrap_pascal_node(spec_node);
            if (spec_node != NULL &&
                (spec_node->typ == PASCAL_T_PROCEDURE_TYPE ||
                 spec_node->typ == PASCAL_T_FUNCTION_TYPE ||
                 spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE))
            {
                inline_proc_type = convert_type_spec_to_kgpctype(cursor, NULL);
            }
        }
    } else if (names != NULL) {
        char *candidate = pop_last_identifier(&names);
        if (candidate != NULL) {
            char *mapped_id = NULL;
            int mapped_type = map_type_name(candidate, &mapped_id);
            if (mapped_type != UNKNOWN_TYPE) {
                mapped_type = apply_shortstring_mode(mapped_type, candidate);
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
                free(mapped_id);
                field_type_id = candidate;
            }
        }
    }

    ListBuilder result_builder;
    list_builder_init(&result_builder);

    ListNode_t *name_node = names;
    while (name_node != NULL) {
        char *field_name = (char *)name_node->cur;
        char *type_id_copy = NULL;
        if (field_type_id != NULL)
            type_id_copy = strdup(field_type_id);

        struct RecordType *nested_copy = NULL;
        if (nested_record != NULL) {
            if (name_node->next == NULL) {
                nested_copy = nested_record;
                nested_record = NULL;
            } else {
                nested_copy = clone_record_type(nested_record);
            }
        }

        struct RecordField *field_desc = (struct RecordField *)calloc(1, sizeof(struct RecordField));
        if (field_desc != NULL) {
            field_desc->name = field_name;
            field_desc->type = field_type;
            /* string[N] fields are shortstrings (array[0..N] of Char with length byte) */
            if (field_desc->type == UNKNOWN_TYPE && field_info.is_shortstring)
                field_desc->type = SHORTSTRING_TYPE;
            field_desc->type_id = type_id_copy;
            field_desc->type_ref = type_ref_from_info_or_id(&field_info, type_id_copy);
            field_desc->nested_record = nested_copy;
            field_desc->proc_type = inline_proc_type;
            if (field_desc->proc_type != NULL)
                kgpc_type_retain(field_desc->proc_type);
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_ref =
                type_ref_from_element_info(&field_info, field_info.element_type_id);
            field_desc->array_is_open = field_info.is_open_array;
            /* For multi-name fields (e.g. a, b: array[0..1] of T),
             * duplicate shared resources; transfer ownership only for the last name. */
            if (name_node->next == NULL)
            {
                field_desc->array_element_type_id = field_info.element_type_id;
                field_info.element_type_id = NULL;
                field_desc->array_element_record = field_info.record_type;
                field_info.record_type = NULL;
            }
            else
            {
                field_desc->array_element_type_id =
                    field_info.element_type_id ? strdup(field_info.element_type_id) : NULL;
                field_desc->array_element_record =
                    field_info.record_type ? clone_record_type(field_info.record_type) : NULL;
            }
            /* Transfer pre-built element KgpcType for nested arrays */
            if (field_info.element_kgpc_type != NULL)
            {
                field_desc->array_element_kgpc_type = field_info.element_kgpc_type;
                kgpc_type_retain(field_desc->array_element_kgpc_type);
            }
            field_desc->is_class_var = is_class_var;
            /* Copy pointer type info for inline pointer fields like ^Char */
            field_desc->is_pointer = field_info.is_pointer;
            field_desc->pointer_type = field_info.pointer_type;
            if (field_info.pointer_type_id != NULL)
                field_desc->pointer_type_id = strdup(field_info.pointer_type_id);
            else
                field_desc->pointer_type_id = NULL;
            field_desc->pointer_type_ref =
                type_ref_from_pointer_info(&field_info, field_info.pointer_type_id);
            /* Transfer anonymous enum values for fields like `kind: (a, b, c)` */
            if (field_info.enum_literals != NULL && name_node->next == NULL)
            {
                field_desc->enum_literals = field_info.enum_literals;
                field_info.enum_literals = NULL;
            }
            else if (field_info.enum_literals != NULL)
            {
                /* Clone for multi-name fields (e.g. a, b: (x, y, z)) */
                ListBuilder clone_builder;
                list_builder_init(&clone_builder);
                for (ListNode_t *en = field_info.enum_literals; en != NULL; en = en->next)
                    if (en->cur != NULL)
                        list_builder_append(&clone_builder, strdup((char *)en->cur), LIST_STRING);
                field_desc->enum_literals = list_builder_finish(&clone_builder);
            }
            /* Copy set element type info for proper set size computation */
            if (field_info.is_set && field_info.set_element_type_id != NULL)
                field_desc->set_element_type_id = strdup(field_info.set_element_type_id);
            list_builder_append(&result_builder, field_desc, LIST_RECORD_FIELD);
        } else {
            if (field_name != NULL)
                free(field_name);
            if (type_id_copy != NULL)
                free(type_id_copy);
            destroy_record_type(nested_copy);
        }

        ListNode_t *next_name = name_node->next;
        free(name_node);
        name_node = next_name;
    }

    if (field_type_id != NULL)
        free(field_type_id);
    if (nested_record != NULL)
        destroy_record_type(nested_record);
    if (inline_proc_type != NULL)
        kgpc_type_release(inline_proc_type);
    destroy_type_info_contents(&field_info);

    return list_builder_finish(&result_builder);
}

static ListNode_t *convert_variant_labels(ast_t *labels_node) {
    if (labels_node == NULL)
        return NULL;

    ListBuilder labels_builder;
    list_builder_init(&labels_builder);

    ast_t *label_cursor = labels_node;
    while (label_cursor != NULL) {
        append_case_label(&labels_builder, label_cursor);
        label_cursor = label_cursor->next;
    }

    return list_builder_finish(&labels_builder);
}

static struct VariantBranch *convert_variant_branch(ast_t *branch_node) {
    if (branch_node == NULL || branch_node->typ != PASCAL_T_VARIANT_BRANCH)
        return NULL;

    struct VariantBranch *branch = (struct VariantBranch *)calloc(1, sizeof(struct VariantBranch));
    if (branch == NULL)
        return NULL;

    ast_t *cursor = branch_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_CASE_LABEL_LIST) {
        branch->labels = convert_variant_labels(cursor->child);
        cursor = cursor->next;
    } else if (cursor != NULL && cursor->typ == PASCAL_T_CASE_LABEL) {
        branch->labels = convert_variant_labels(cursor);
        cursor = cursor->next;
    }

    ListBuilder members_builder;
    list_builder_init(&members_builder);
    convert_record_members(cursor, &members_builder, NULL, NULL);
    branch->members = list_builder_finish(&members_builder);

    return branch;
}

static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields) {
    if (out_tag_fields != NULL)
        *out_tag_fields = NULL;

    if (variant_node == NULL || variant_node->typ != PASCAL_T_VARIANT_PART)
        return NULL;

    struct VariantPart *variant = (struct VariantPart *)calloc(1, sizeof(struct VariantPart));
    if (variant == NULL)
        return NULL;

    ast_t *cursor = variant_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_VARIANT_TAG) {
        ast_t *tag_cursor = cursor->child;
        char *tag_name = NULL;
        ast_t *type_cursor = tag_cursor;
        if (tag_cursor != NULL && tag_cursor->typ == PASCAL_T_IDENTIFIER && tag_cursor->next != NULL) {
            tag_name = dup_symbol(tag_cursor);
            type_cursor = tag_cursor->next;
        }

        char *tag_type_id = NULL;
        struct RecordType *tag_record = NULL;
        int tag_type = UNKNOWN_TYPE;
        if (type_cursor != NULL)
            tag_type = convert_type_spec(type_cursor, &tag_type_id, &tag_record, NULL);

        if (tag_name != NULL) {
            struct RecordField *tag_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
            if (tag_field != NULL) {
                tag_field->name = tag_name;
                tag_field->type = tag_type;
                tag_field->type_id = (tag_type_id != NULL) ? strdup(tag_type_id) : NULL;
                tag_field->nested_record = tag_record;
                tag_field->is_array = 0;
                tag_field->array_start = 0;
                tag_field->array_end = 0;
                tag_field->array_element_type = UNKNOWN_TYPE;
                tag_field->array_element_type_id = NULL;
                tag_field->array_is_open = 0;

                ListNode_t *field_node = CreateListNode(tag_field, LIST_RECORD_FIELD);
                if (out_tag_fields != NULL)
                    *out_tag_fields = field_node;
                variant->tag_field = tag_field;
                variant->tag_type = tag_type;
                variant->tag_type_id = NULL;
                variant->tag_record = NULL;
                tag_record = NULL;
            } else {
                free(tag_name);
                destroy_record_type(tag_record);
            }
        } else {
            variant->tag_field = NULL;
            variant->tag_type = tag_type;
            variant->tag_type_id = tag_type_id;
            variant->tag_record = tag_record;
            tag_type_id = NULL;
            tag_record = NULL;
        }

        variant->tag_type = tag_type;

        if (tag_type_id != NULL)
            free(tag_type_id);
        if (tag_record != NULL)
            destroy_record_type(tag_record);

        cursor = cursor->next;
    }

    ListBuilder branches_builder;
    list_builder_init(&branches_builder);
    for (; cursor != NULL; cursor = cursor->next) {
        if (cursor->typ != PASCAL_T_VARIANT_BRANCH)
            continue;
        struct VariantBranch *branch = convert_variant_branch(cursor);
        if (branch != NULL)
            list_builder_append(&branches_builder, branch, LIST_VARIANT_BRANCH);
    }
    variant->branches = list_builder_finish(&branches_builder);
    variant->has_cached_size = 0;
    variant->cached_size = 0;

    return variant;
}

static void convert_record_members(ast_t *node, ListBuilder *builder,
    ListBuilder *property_builder, ListBuilder *method_template_builder) {
    for (ast_t *cur = node; cur != NULL; cur = cur->next) {
        if (cur->typ == PASCAL_T_FIELD_DECL) {
            ListNode_t *fields = convert_field_decl(cur);
            list_builder_extend(builder, fields);
        } else if (cur->typ == PASCAL_T_VARIANT_PART) {
            ListNode_t *tag_fields = NULL;
            struct VariantPart *variant = convert_variant_part(cur, &tag_fields);
            list_builder_extend(builder, tag_fields);
            if (variant != NULL)
                list_builder_append(builder, variant, LIST_VARIANT_PART);
        } else if (cur->typ == PASCAL_T_METHOD_DECL ||
                   cur->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                   cur->typ == PASCAL_T_DESTRUCTOR_DECL) {
            /* Store method declaration as a special marker node for operator overloading */
            /* We'll handle this during semantic check when we know the record type name */
            list_builder_append(builder, cur, LIST_UNSPECIFIED);
            if (method_template_builder != NULL) {
                struct MethodTemplate *template = create_method_template(cur);
                if (template != NULL)
                    list_builder_append(method_template_builder, template, LIST_METHOD_TEMPLATE);
            }
        } else if (cur->typ == PASCAL_T_PROPERTY_DECL && property_builder != NULL) {
            struct ClassProperty *property = convert_property_decl(cur);
            if (property != NULL)
                list_builder_append(property_builder, property, LIST_CLASS_PROPERTY);
        } else if (cur->typ == PASCAL_T_VAR_SECTION) {
            /* Handle var / class var / class threadvar sections inside objects.
             * The VAR_SECTION wraps keyword nodes and FIELD_DECL children. */
            int is_class_var_section = 0;
            if (kgpc_getenv("KGPC_DEBUG_CLASS_VAR_PARSE") != NULL)
            {
                fprintf(stderr, "[KGPC] var section nodes:");
                for (ast_t *dbg = cur->child; dbg != NULL; dbg = dbg->next)
                {
                    ast_t *node = unwrap_pascal_node(dbg);
                    const char *name = (node != NULL && node->sym != NULL) ? node->sym->name : NULL;
                    fprintf(stderr, " (%s:%d)", name ? name : "<null>",
                        node != NULL ? node->typ : -1);
                }
                fprintf(stderr, "\n");
            }
            for (ast_t *scan = cur->child; scan != NULL; scan = scan->next) {
                ast_t *node = unwrap_pascal_node(scan);
                if (node != NULL && node->sym != NULL && node->sym->name != NULL &&
                    strcasecmp(node->sym->name, "class") == 0)
                {
                    is_class_var_section = 1;
                    break;
                }
            }
            for (ast_t *child = cur->child; child != NULL; child = child->next) {
                if (child->typ == PASCAL_T_FIELD_DECL) {
                    ListNode_t *fields = convert_field_decl(child);
                    if (is_class_var_section && fields != NULL)
                    {
                        for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next)
                        {
                            if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                            {
                                struct RecordField *field = (struct RecordField *)fnode->cur;
                                field->is_class_var = 1;
                            }
                        }
                    }
                    list_builder_extend(builder, fields);
                }
            }
        } else if (cur->typ == PASCAL_T_CLASS_MEMBER) {
            /* CLASS_MEMBER may wrap either:
             * 1. Visibility sections (e.g. public/private) — recurse
             * 2. "class var" fields from object types — set is_class_var on each */
            int has_field_decls = 0;
            for (ast_t *chk = cur->child; chk != NULL; chk = chk->next) {
                if (chk->typ == PASCAL_T_FIELD_DECL) { has_field_decls = 1; break; }
            }
            if (has_field_decls) {
                /* This is a "class var" section — convert fields and mark is_class_var */
                for (ast_t *child = cur->child; child != NULL; child = child->next) {
                    if (child->typ == PASCAL_T_FIELD_DECL) {
                        ListNode_t *fields = convert_field_decl(child);
                        if (fields != NULL) {
                            for (ListNode_t *fnode = fields; fnode != NULL; fnode = fnode->next) {
                                if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL) {
                                    struct RecordField *field = (struct RecordField *)fnode->cur;
                                    field->is_class_var = 1;
                                }
                            }
                            list_builder_extend(builder, fields);
                        }
                    }
                }
            } else {
                convert_record_members(cur->child, builder, property_builder, method_template_builder);
            }
        }
    }
}

/* Helper: scan record AST for nested type sections (Delphi advanced records) */
static void collect_record_nested_types(ast_t *node, ListBuilder *nested_type_builder) {
    for (ast_t *cur = node; cur != NULL; cur = cur->next) {
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped == NULL)
            continue;

        if (unwrapped->typ == PASCAL_T_CLASS_MEMBER) {
            /* Recurse into CLASS_MEMBER nodes */
            collect_record_nested_types(unwrapped->child, nested_type_builder);
        } else if (unwrapped->typ == PASCAL_T_NESTED_TYPE_SECTION || unwrapped->typ == PASCAL_T_TYPE_SECTION) {
            /* Found a nested type section */
            if (nested_type_builder != NULL) {
                list_builder_append(nested_type_builder, unwrapped, LIST_UNSPECIFIED);
                if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                    fprintf(stderr, "[KGPC] collect_record_nested_types: found NESTED_TYPE_SECTION at line %d\n",
                        unwrapped->line);
            }
        }
    }
}

static struct RecordType *convert_record_type_ex(ast_t *record_node, ListNode_t **nested_types_out) {
    if (record_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    if (record_node->sym != NULL &&
        record_node->sym->name != NULL &&
        strcasecmp(record_node->sym->name, "helper") == 0)
    {
        struct RecordType *record = (struct RecordType *)calloc(1, sizeof(struct RecordType));
        if (record == NULL)
            return NULL;
        ListBuilder helper_fields_builder;
        list_builder_init(&helper_fields_builder);
        ListBuilder helper_properties_builder;
        list_builder_init(&helper_properties_builder);
        ListBuilder method_template_builder;
        list_builder_init(&method_template_builder);
        record->fields = NULL;
        record->properties = NULL;
        record->parent_class_name = NULL;
        record->methods = NULL;
        record->method_templates = NULL;
        record->is_class = 0;
        record->is_interface = 0;
        record->is_packed = 0;
        record->is_type_helper = 1;
        record->helper_base_type_id = NULL;
        record->helper_parent_id = NULL;
        record->is_generic_specialization = 0;
        record->type_id = NULL;
        record->has_cached_size = 0;
        record->cached_size = 0;
        record->generic_decl = NULL;
        record->generic_args = NULL;
        record->num_generic_args = 0;
        record->method_clones_emitted = 0;

        /* Parse helper children:
         * For "type helper(ParentHelper) for BaseType":
         *   - First identifier: ParentHelper
         *   - Second identifier: BaseType
         * For "type helper for BaseType":
         *   - Only one identifier: BaseType
         */
        ast_t *first_ident = NULL;
        ast_t *second_ident = NULL;
        ast_t *base_node = record_node->child;
        
        while (base_node != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(base_node);
            ast_t *type_ident = unwrapped;
            while (type_ident != NULL && type_ident->typ == PASCAL_T_TYPE_SPEC &&
                type_ident->child != NULL)
            {
                type_ident = unwrap_pascal_node(type_ident->child);
            }
            if (type_ident != NULL &&
                (type_ident->typ == PASCAL_T_IDENTIFIER || type_ident->typ == PASCAL_T_QUALIFIED_IDENTIFIER) &&
                type_ident->sym != NULL && type_ident->sym->name != NULL)
            {
                if (first_ident == NULL) {
                    first_ident = type_ident;
                } else if (second_ident == NULL) {
                    second_ident = type_ident;
                }
                base_node = base_node->next;
                continue;
            }

            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_CLASS_MEMBER)
            {
                ast_t *member = unwrapped->child;
                while (member != NULL)
                {
                    ast_t *member_unwrapped = unwrap_pascal_node(member);
                    if (member_unwrapped != NULL)
                    {
                        if (member_unwrapped->typ == PASCAL_T_METHOD_DECL ||
                            member_unwrapped->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                            member_unwrapped->typ == PASCAL_T_DESTRUCTOR_DECL)
                        {
                            list_builder_append(&helper_fields_builder, member_unwrapped, LIST_UNSPECIFIED);
                            struct MethodTemplate *template = create_method_template(member_unwrapped);
                            if (template != NULL) {
                                list_builder_append(&method_template_builder, template, LIST_METHOD_TEMPLATE);
                            }
                        }
                        else if (member_unwrapped->typ == PASCAL_T_PROPERTY_DECL)
                        {
                            struct ClassProperty *property = convert_property_decl(member_unwrapped);
                            if (property != NULL)
                                list_builder_append(&helper_properties_builder, property, LIST_CLASS_PROPERTY);
                        }
                    }
                    member = member->next;
                }
                base_node = base_node->next;
                continue;
            }

            if (unwrapped != NULL &&
                (unwrapped->typ == PASCAL_T_METHOD_DECL ||
                 unwrapped->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                 unwrapped->typ == PASCAL_T_DESTRUCTOR_DECL))
            {
                list_builder_append(&helper_fields_builder, unwrapped, LIST_UNSPECIFIED);
                /* Also create method template to preserve default parameter values */
                struct MethodTemplate *template = create_method_template(unwrapped);
                if (template != NULL) {
                    list_builder_append(&method_template_builder, template, LIST_METHOD_TEMPLATE);
                }
            }
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_PROPERTY_DECL)
            {
                struct ClassProperty *property = convert_property_decl(unwrapped);
                if (property != NULL)
                    list_builder_append(&helper_properties_builder, property, LIST_CLASS_PROPERTY);
            }
            base_node = base_node->next;
        }
        
        /* If we have two identifiers, first is parent helper, second is base type.
         * If we have one identifier, it's the base type (no parent helper). */
        if (second_ident != NULL) {
            record->helper_parent_id = strdup(first_ident->sym->name);
            record->helper_base_type_id = strdup(second_ident->sym->name);
        } else if (first_ident != NULL) {
            record->helper_base_type_id = strdup(first_ident->sym->name);
        }

        record->fields = list_builder_finish(&helper_fields_builder);
        record->properties = list_builder_finish(&helper_properties_builder);
        record->method_templates = list_builder_finish(&method_template_builder);
        return record;
    }

    /* Scan for nested type sections in the record */
    ListBuilder nested_type_builder;
    list_builder_init(&nested_type_builder);

    /* For OBJECT_TYPE, the first child may be the parent type identifier
     * (inserted by the parser for object(BaseType) syntax).
     * Extract it before processing members. */
    char *parent_class_name = NULL;
    ast_t *members_start = record_node->child;
    if (record_node->typ == PASCAL_T_OBJECT_TYPE &&
        members_start != NULL &&
        members_start->typ == PASCAL_T_IDENTIFIER &&
        members_start->sym != NULL &&
        members_start->sym->name != NULL)
    {
        parent_class_name = strdup(members_start->sym->name);
        members_start = members_start->next;
    }

    collect_record_nested_types(members_start, &nested_type_builder);

    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    ListBuilder fields_builder;
    ListBuilder property_builder;
    ListBuilder method_template_builder;
    list_builder_init(&fields_builder);
    list_builder_init(&property_builder);
    list_builder_init(&method_template_builder);
    convert_record_members(members_start, &fields_builder, &property_builder, &method_template_builder);

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(fields_builder.head);
        destroy_list(property_builder.head);
        free(parent_class_name);
        return NULL;
    }
    record->fields = list_builder_finish(&fields_builder);
    record->properties = NULL;
    record->parent_class_name = parent_class_name;
    record->methods = NULL;  /* Regular records don't have methods */
    record->method_templates = list_builder_finish(&method_template_builder);
    record->is_class = 0;
    record->is_interface = 0;
    record->is_packed = (record_node->sym != NULL &&
        record_node->sym->name != NULL &&
        (strcasecmp(record_node->sym->name, "packed") == 0 ||
         strcasecmp(record_node->sym->name, "bitpacked") == 0));
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = NULL;
    record->outer_type_id = NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->is_generic_specialization = 0;
    record->method_clones_emitted = 0;
    record->parent_fields_merged = 0;
    record->source_unit_index = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->record_properties = list_builder_finish(&property_builder);
    record->default_indexed_element_type_id = NULL;
    record->guid_string = NULL;
    record->interface_names = NULL;
    record->num_interfaces = 0;
    return record;
}

static struct RecordType *convert_record_type(ast_t *record_node) {
    return convert_record_type_ex(record_node, NULL);
}

static char *pop_last_identifier(ListNode_t **ids) {
    if (ids == NULL || *ids == NULL)
        return NULL;

    ListNode_t *prev = NULL;
    ListNode_t *cur = *ids;
    while (cur->next != NULL) {
        prev = cur;
        cur = cur->next;
    }

    char *value = (char *)cur->cur;
    if (prev != NULL)
        prev->next = NULL;
    else
        *ids = NULL;

    free(cur);
    return value;
}

static ListNode_t *convert_param(ast_t *param_node) {
    if (param_node == NULL || param_node->typ != PASCAL_T_PARAM)
        return NULL;

    ast_t *modifier_node = param_node->child;
    ast_t *ids_cursor = modifier_node != NULL ? modifier_node->next : NULL;

    int is_var_param = 0;
    int is_const_param = 0;
    if (modifier_node != NULL && modifier_node->sym != NULL && modifier_node->sym->name != NULL) {
        const char *modifier_name = modifier_node->sym->name;
        if (strcasecmp(modifier_name, "var") == 0 || strcasecmp(modifier_name, "out") == 0)
            is_var_param = 1;
        else if (strcasecmp(modifier_name, "const") == 0)
            is_const_param = 1;
        else if (strcasecmp(modifier_name, "constref") == 0)
        {
            /* constref is like var (pass by reference) but also const (not modifiable).
             * For codegen purposes, we treat it as is_var_param=1 to pass by reference.
             * The const aspect is enforced by semantic checking. */
            is_var_param = 1;
            is_const_param = 1;
        }
    }

    ast_t *cursor = ids_cursor;
    ListNode_t *ids = convert_identifier_list(&cursor);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: parameter missing identifier list.\n");
        return NULL;
    }

    ast_t *type_node = cursor;
    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;
    TypeInfo type_info = {0};
    ast_t *default_value_node = NULL;

    if (type_node == NULL || type_node->typ != PASCAL_T_TYPE_SPEC) {
        if (!(is_var_param || is_const_param)) {
            fprintf(stderr, "ERROR: parameter missing type specification.\n");
            destroy_list(ids);
            return NULL;
        }
        var_type = UNKNOWN_TYPE;
        type_id = NULL;
    }
    else
    {
        /* ARCHITECTURAL FIX: Pass TypeInfo to preserve array information */
        var_type = convert_type_spec(type_node, &type_id, NULL, &type_info);
        /* Check for default value node after type spec.
         * Some parser shapes wrap optional/default nodes so `type_node->next`
         * may be NULL even when a default exists. */
        if (type_node->next != NULL && type_node->next->typ == PASCAL_T_DEFAULT_VALUE) {
            default_value_node = type_node->next;
        } else {
            default_value_node = find_ast_node_type(param_node, PASCAL_T_DEFAULT_VALUE);
        }
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[convert_param] type_node=%p type_node->next=%p next_typ=%d\n",
                (void*)type_node, 
                (void*)(type_node ? type_node->next : NULL),
                (type_node && type_node->next) ? type_node->next->typ : -1);
        }
    }

    /* Convert default value if present */
    struct Statement *default_init = NULL;
    if (default_value_node != NULL) {
        /* PASCAL_T_DEFAULT_VALUE's child IS the expression (no "=" token since match returns ast_nil) */
        ast_t *expr_node = default_value_node->child;
        if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[convert_param] default_value_node=%p expr_node=%p\n",
                (void*)default_value_node, (void*)expr_node);
        }
        if (expr_node != NULL) {
            struct Expression *default_expr = convert_expression(expr_node);
            if (default_expr != NULL) {
                /* Wrap expression in a var_assign statement with NULL var for storage */
                default_init = mk_varassign(default_value_node->line, default_value_node->col, 
                                            NULL, default_expr);
                if (kgpc_getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
                    fprintf(stderr, "[convert_param] Created default_init=%p\n", (void*)default_init);
                }
            }
        }
    }

    ListBuilder result_builder;
    list_builder_init(&result_builder);
    ListNode_t *id_node = ids;

    while (id_node != NULL) {
        ListNode_t *next_id = id_node->next;
        id_node->next = NULL;
        char *type_id_copy = type_id != NULL ? strdup(type_id) : NULL;
        
        Tree_t *param_decl = NULL;
        struct TypeAlias *inline_alias = NULL;
        if (type_info.is_set)
        {
            inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
            if (inline_alias != NULL)
            {
                inline_alias->is_set = 1;
                inline_alias->set_element_type = type_info.set_element_type;
                inline_alias->base_type = SET_TYPE;
                if (type_info.set_element_type_id != NULL)
                    inline_alias->set_element_type_id = strdup(type_info.set_element_type_id);
                inline_alias->set_element_type_ref =
                    type_ref_from_single_name(type_info.set_element_type_id);
            }
        }
        /* Create TREE_ARR_DECL for inline array parameters */
        if (type_info.is_array)
        {
            int element_type = type_info.element_type;
            char *element_type_id = type_info.element_type_id != NULL ? strdup(type_info.element_type_id) : NULL;
            char *range_str = NULL;
            struct RecordType *inline_record = type_info.record_type;
            if (inline_record != NULL)
                type_info.record_type = NULL;
            if (type_info.array_dimensions != NULL && type_info.array_dimensions->cur != NULL) {
                range_str = strdup((char *)type_info.array_dimensions->cur);
            }
            param_decl = mk_arraydecl(param_node->line, id_node, element_type, element_type_id,
                                      type_info.start, type_info.end, range_str, NULL, inline_record);
            if (param_decl != NULL)
                param_decl->tree_data.arr_decl_data.is_shortstring = type_info.is_shortstring;
            if (param_decl != NULL)
                param_decl->tree_data.arr_decl_data.type_ref =
                    type_ref_from_element_info(&type_info, element_type_id);
            /* Set var parameter flag on array declaration */
            if (is_var_param && param_decl != NULL)
                param_decl->tree_data.arr_decl_data.type = var_type; // Store this for compatibility
            if (kgpc_getenv("KGPC_DEBUG_ARRAY_PARAM") != NULL && param_decl != NULL)
            {
                fprintf(stderr,
                    "[KGPC] array param %s: element_type=%d element_id=%s is_array_of_const=%d\n",
                    id_node != NULL && id_node->cur != NULL ? (const char *)id_node->cur : "<unnamed>",
                    element_type,
                    element_type_id != NULL ? element_type_id : "<null>",
                    type_info.is_array_of_const);
            }
            /* Note: array parameters with default values are rare but could be supported */
        }
        else
        {
            param_decl = mk_vardecl(param_node->line, id_node, var_type, type_id_copy,
                is_var_param, 0, default_init, NULL, inline_alias, NULL);
            if (param_decl != NULL)
                param_decl->tree_data.var_decl_data.is_const_param = is_const_param;
            if (param_decl != NULL && (type_node == NULL || type_node->typ != PASCAL_T_TYPE_SPEC))
                param_decl->tree_data.var_decl_data.is_untyped_param = 1;
            if (param_decl != NULL)
                param_decl->tree_data.var_decl_data.type_ref =
                    type_ref_from_info_or_id(&type_info, type_id_copy);
        }
        
        list_builder_append(&result_builder, param_decl, LIST_TREE);
        id_node = next_id;
    }

    if (type_id != NULL)
        free(type_id);
    
    destroy_type_info_contents(&type_info);

    return list_builder_finish(&result_builder);
}

static ListNode_t *convert_param_list(ast_t **cursor) {
    ListNode_t *params = NULL;
    ast_t *cur = *cursor;
    ast_t *slow = cur;
    ast_t *fast = cur;
    int guard = 0;
    const int guard_limit = 100000;

    while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
        guard++;
        if (guard > guard_limit) {
            fprintf(stderr, "ERROR: convert_param_list exceeded guard limit (%d); possible cycle in param list (node=%p typ=%d).\n",
                guard_limit, (void*)cur, cur->typ);
            break;
        }
        if (fast != NULL && fast->next != NULL) {
            fast = fast->next->next;
            slow = slow ? slow->next : NULL;
            if (fast != NULL && slow == fast) {
                fprintf(stderr, "ERROR: Cycle detected in param list during conversion (node=%p typ=%d).\n",
                    (void*)cur, cur->typ);
                break;
            }
        }
        ListNode_t *param_nodes = convert_param(cur);
        extend_list(&params, param_nodes);
        cur = cur->next;
    }

    *cursor = cur;
    return params;
}

ListNode_t *from_cparser_convert_params_ast(ast_t *params_ast)
{
    if (params_ast == NULL)
        return NULL;

    ast_t *cursor = params_ast;
    if (cursor->typ == PASCAL_T_PARAM_LIST)
        cursor = cursor->child;

    return convert_param_list(&cursor);
}

KgpcType *from_cparser_method_template_to_proctype(struct MethodTemplate *method_template,
    struct RecordType *record, struct SymTab *symtab)
{
    if (method_template == NULL)
        return NULL;

    ListNode_t *params = NULL;
    ListBuilder params_builder;
    list_builder_init(&params_builder);

    /* Add implicit Self parameter for instance methods and non-static class methods.
     * For instance methods, Self = instance pointer.
     * For class methods (non-static), Self = VMT pointer (class reference). */
    if (!method_template->is_static) {
        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
        char *self_type_id = NULL;
        int self_type_tag = UNKNOWN_TYPE;
        struct TypeAlias *self_type_alias = NULL;
        int self_is_var = 1;
        if (record != NULL) {
            if (record->is_type_helper && record->helper_base_type_id != NULL) {
                self_type_id = strdup(record->helper_base_type_id);
                self_type_tag = map_type_name(record->helper_base_type_id, NULL);
                self_is_var = helper_self_param_is_var(record->helper_base_type_id, symtab);
                self_type_alias = helper_self_real_alias(record->helper_base_type_id);
            } else if (record->is_class || record->is_interface) {
                self_is_var = 0;
            } else if (record->type_id != NULL) {
                self_type_id = strdup(record->type_id);
            }
        }
        Tree_t *self_param = mk_vardecl(0, self_ids, self_type_tag, self_type_id,
            self_is_var, 0, NULL, NULL, self_type_alias, NULL);
        if (self_param != NULL)
            self_param->tree_data.var_decl_data.type_ref = type_ref_from_single_name(self_type_id);
        if (self_param != NULL)
            list_builder_append(&params_builder, self_param, LIST_TREE);
    }

    if (method_template->params_ast != NULL) {
        ListNode_t *extra_params = from_cparser_convert_params_ast(method_template->params_ast);
        if (extra_params != NULL)
            list_builder_extend(&params_builder, extra_params);
    }

    params = list_builder_finish(&params_builder);

    const char *owner_full = (record != NULL) ? record->type_id : NULL;
    char *owner_outer = NULL;
    if (owner_full != NULL) {
        const char *dot = strrchr(owner_full, '.');
        if (dot != NULL && dot[1] != '\0' && dot != owner_full)
            owner_outer = strndup(owner_full, (size_t)(dot - owner_full));
    }
    qualify_param_decl_types(params, owner_full, owner_outer, symtab);
    if (owner_outer != NULL)
        free(owner_outer);

    KgpcType *return_type = NULL;
    char *return_type_id = NULL;
    if (method_template->has_return_type && method_template->return_type_ast != NULL) {
        ast_t *ret_node = method_template->return_type_ast;
        if (ret_node->typ == PASCAL_T_RETURN_TYPE && ret_node->child != NULL)
            ret_node = ret_node->child;
        return_type = convert_type_spec_to_kgpctype(ret_node, symtab);
        if (return_type == NULL && ret_node != NULL && ret_node->typ == PASCAL_T_IDENTIFIER) {
            char *ret_name = dup_symbol(ret_node);
            if (ret_name != NULL) {
                int ret_tag = map_type_name(ret_name, NULL);
                ret_tag = apply_shortstring_mode(ret_tag, ret_name);
                if (ret_tag != UNKNOWN_TYPE)
                    return_type = create_primitive_type(ret_tag);
                return_type_id = strdup(ret_name);
                free(ret_name);
            }
        } else if (ret_node != NULL && ret_node->typ == PASCAL_T_IDENTIFIER) {
            return_type_id = dup_symbol(ret_node);
        }
    }

    KgpcType *proc_type = create_procedure_type(params, return_type);
    if (proc_type != NULL) {
        proc_type->info.proc_info.owns_params = 1;
        DestroyList(params);
        if (return_type_id != NULL)
            proc_type->info.proc_info.return_type_id = return_type_id;
    } else {
        destroy_list(params);
        if (return_type_id != NULL)
            free(return_type_id);
    }

    return proc_type;
}

static bool is_var_hint_clause(ast_t *node) {
    if (node == NULL || node->typ != PASCAL_T_NONE)
        return false;
    ast_t *child = node->child;
    if (child == NULL || child->typ != PASCAL_T_IDENTIFIER || child->sym == NULL)
        return false;
    const char *name = child->sym->name;
    if (name == NULL)
        return false;
    return strcasecmp(name, "deprecated") == 0
        || strcasecmp(name, "platform") == 0
        || strcasecmp(name, "library") == 0;
}

static ast_t *absolute_clause_target(ast_t *node) {
    if (node == NULL || node->typ != PASCAL_T_ABSOLUTE_CLAUSE)
        return NULL;
    /* The PASCAL_T_ABSOLUTE_CLAUSE node contains the target identifier as its child
     * (the "absolute" keyword itself is consumed by keyword_ci and not in the AST) */
    ast_t *child = node->child;
    if (child == NULL || child->typ != PASCAL_T_IDENTIFIER)
        return NULL;
    return child;
}

/* Check if a node should be skipped as an initializer.
 * This handles:
 * - PASCAL_T_ABSOLUTE_CLAUSE: absolute variable aliasing (e.g., "X: Integer absolute Y")
 * - PASCAL_T_IDENTIFIER: trailing identifiers that aren't initializers (legacy case)
 */
static int is_node_to_skip_as_initializer(ast_t *node) {
    if (node == NULL)
        return 0;
    return (node->typ == PASCAL_T_IDENTIFIER || node->typ == PASCAL_T_ABSOLUTE_CLAUSE);
}

static int select_range_primitive_tag(const TypeInfo *info);
static long long compute_range_storage_size(const TypeInfo *info);

static Tree_t *convert_var_decl(ast_t *decl_node) {
    ast_t *cur = decl_node->child;
    ast_t *first_non_identifier = cur;
    while (first_non_identifier != NULL && first_non_identifier->typ == PASCAL_T_IDENTIFIER)
        first_non_identifier = first_non_identifier->next;

    ListNode_t *ids = convert_identifier_list(&cur);
    if (ids == NULL) {
        fprintf(stderr, "ERROR: variable declaration missing identifier list.\n");
        return NULL;
    }
    char *type_id = NULL;
    int var_type = UNKNOWN_TYPE;
    TypeInfo type_info = {0};
    ast_t *type_node = first_non_identifier;
    if (type_node != NULL && type_node->typ == PASCAL_T_TYPE_SPEC) {
        var_type = convert_type_spec(type_node, &type_id, NULL, &type_info);
        cur = type_node->next;
    } else if (type_node != NULL && type_node->typ == PASCAL_T_IDENTIFIER && type_node->next == NULL) {
        char *type_name = dup_symbol(type_node);
        if (type_name != NULL) {
            var_type = map_type_name(type_name, &type_id);
            var_type = apply_shortstring_mode(var_type, type_name);
            if (var_type == UNKNOWN_TYPE && type_id == NULL)
                type_id = type_name;
            else
                free(type_name);
        }
        cur = type_node->next;
    } else {
        cur = type_node;
    }

    if (var_type == UNKNOWN_TYPE && (type_node == NULL || var_type == UNKNOWN_TYPE)) {
        ast_t *search = decl_node->child;
        while (search != NULL && search->typ == PASCAL_T_IDENTIFIER)
            search = search->next;
        if (search != NULL && search->typ == PASCAL_T_TYPE_SPEC) {
            if (type_id != NULL) {
                free(type_id);
                type_id = NULL;
            }
            /* Free any type_info from the first convert_type_spec call,
             * since convert_type_spec resets all fields to NULL without freeing */
            destroy_type_info_contents(&type_info);
            var_type = convert_type_spec(search, &type_id, NULL, &type_info);
        } else if (search != NULL && search->typ == PASCAL_T_IDENTIFIER) {
            char *type_name = dup_symbol(search);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
                mapped = apply_shortstring_mode(mapped, type_name);
                if (mapped == UNKNOWN_TYPE && type_id == NULL)
                    type_id = type_name;
                else
                    free(type_name);
                var_type = mapped;
            }
        }
    }

    /* Handle inline procedure/function types for variables */
    KgpcType *inline_proc_type = NULL;
    if (var_type == PROCEDURE) {
        /* Find the procedure type specification node */
        ast_t *search = decl_node->child;
        while (search != NULL && search->typ == PASCAL_T_IDENTIFIER)
            search = search->next;
        if (search != NULL) {
            ast_t *spec_node = search;
            if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
                spec_node = spec_node->child;
            spec_node = unwrap_pascal_node(spec_node);
            if (spec_node != NULL &&
                (spec_node->typ == PASCAL_T_PROCEDURE_TYPE ||
                 spec_node->typ == PASCAL_T_FUNCTION_TYPE ||
                 spec_node->typ == PASCAL_T_REFERENCE_TO_TYPE))
            {
                inline_proc_type = convert_type_spec_to_kgpctype(search, NULL);
            }
        }
    }

    if (type_info.is_array) {
        int element_type = type_info.element_type;
        char *element_type_id = type_info.element_type_id;
        struct RecordType *inline_record = type_info.record_type;
        if (inline_record != NULL)
            type_info.record_type = NULL;
        
        /* Handle optional initializer for arrays */
        struct Statement *initializer_stmt = NULL;
        if (cur != NULL) {
            /* Handle optional initializer wrapper: optional(seq(...)) creates PASCAL_T_NONE node */
            ast_t *init_node = cur;
            if (init_node->typ == PASCAL_T_NONE && init_node->child != NULL) {
                if (absolute_clause_target(init_node) != NULL || is_var_hint_clause(init_node)) {
                    init_node = NULL;
                } else {
                /* The wrapper contains "=" token followed by the expression.
                 * Skip to find the actual expression node (not the "=" token) */
                ast_t *child = init_node->child;
                if (child != NULL && child->next != NULL) {
                    /* Skip the first child (the "=" token) and use the second */
                    init_node = child->next;
                }
                }
            }

            /* Skip nodes that should not be treated as initializers */
            if (is_node_to_skip_as_initializer(init_node)) {
                init_node = NULL;
            }
            
            if (init_node != NULL && ids != NULL && ids->next == NULL) {
                char *var_name = (char *)ids->cur;
                
                /* Check if initializer is a tuple (array literal) */
                if (init_node->typ == PASCAL_T_TUPLE) {
                    /* Create multiple assignment statements for array elements */
                    ListBuilder stmt_builder;
                    list_builder_init(&stmt_builder);
                    
                    int index = type_info.start;
                    for (ast_t *elem = init_node->child; elem != NULL; elem = elem->next) {
                        struct Expression *elem_expr = convert_expression(elem);
                        if (elem_expr != NULL) {
                            struct Expression *index_expr = mk_inum(decl_node->line, index);
                            struct Expression *base_expr = mk_varid(decl_node->line, strdup(var_name));
                            struct Expression *array_access = mk_arrayaccess(decl_node->line, base_expr, index_expr);
                            struct Statement *assign_stmt = mk_varassign(decl_node->line, decl_node->col, array_access, elem_expr);
                            list_builder_append(&stmt_builder, assign_stmt, LIST_STMT);
                        }
                        index++;
                    }
                    
                    /* Create compound statement from all assignments */
                    ListNode_t *stmt_list = list_builder_finish(&stmt_builder);
                    if (stmt_list != NULL) {
                        initializer_stmt = mk_compoundstatement(decl_node->line, stmt_list);
                    }
                } else {
                    /* Single expression initializer (not a tuple) */
                    struct Expression *init_expr = convert_expression(init_node);
                    if (init_expr != NULL) {
                        struct Expression *lhs = mk_varid(decl_node->line, strdup(var_name));
                        initializer_stmt = mk_varassign(decl_node->line, decl_node->col, lhs, init_expr);
                    }
                }
            }
        }
        
        /* Extract the range string from array_dimensions if available */
        char *range_str = NULL;
        if (type_info.array_dimensions != NULL && type_info.array_dimensions->cur != NULL) {
            range_str = strdup((char *)type_info.array_dimensions->cur);
        }
        
        Tree_t *decl = mk_arraydecl(decl_node->line, ids, element_type, element_type_id,
                                    type_info.start, type_info.end, range_str, initializer_stmt,
                                    inline_record);
        if (decl != NULL)
            decl->tree_data.arr_decl_data.is_shortstring = type_info.is_shortstring;
        if (decl != NULL)
            decl->tree_data.arr_decl_data.type_ref =
                type_ref_from_element_info(&type_info, element_type_id);
        if (decl != NULL && type_info.unresolved_index_type != NULL) {
            decl->tree_data.arr_decl_data.unresolved_index_type = type_info.unresolved_index_type;
            type_info.unresolved_index_type = NULL;  /* ownership transferred */
        }
        /* Transfer array_dimensions for multi-dim linearization */
        if (decl != NULL && type_info.array_dimensions != NULL &&
            type_info.array_dimensions->next != NULL) {
            decl->tree_data.arr_decl_data.array_dimensions = type_info.array_dimensions;
            type_info.array_dimensions = NULL;
        }
        /* For single-dimension enum-indexed arrays (e.g., array[TColor] of ...),
         * the bounds are 0,0 because the enum wasn't resolved at parse time.
         * Transfer the dimension name as unresolved_index_type so semcheck can resolve it. */
        if (decl != NULL && decl->tree_data.arr_decl_data.unresolved_index_type == NULL &&
            type_info.start == 0 && type_info.end == 0 &&
            type_info.array_dimensions != NULL &&
            type_info.array_dimensions->type == LIST_STRING &&
            type_info.array_dimensions->cur != NULL)
        {
            const char *dim_str = (const char *)type_info.array_dimensions->cur;
            /* Only if it's a single identifier (no "..") */
            if (strstr(dim_str, "..") == NULL) {
                decl->tree_data.arr_decl_data.unresolved_index_type = strdup(dim_str);
            }
        }
        type_info.element_type_id = NULL;
        destroy_type_info_contents(&type_info);
        if (type_id != NULL)
            free(type_id);
        return decl;
    }

    if (type_info.is_pointer) {
        if (type_info.pointer_type_id != NULL && type_id == NULL)
            type_id = strdup(type_info.pointer_type_id);
        var_type = POINTER_TYPE;
    } else if (type_info.is_set) {
        if (type_info.set_element_type_id != NULL && type_id == NULL)
            type_id = strdup(type_info.set_element_type_id);
        var_type = SET_TYPE;
    } else if (type_info.is_enum) {
        var_type = ENUM_TYPE;
    } else if (type_info.is_file) {
        var_type = FILE_TYPE;
    } else if (type_info.is_record) {
        var_type = RECORD_TYPE;
    }

    int inferred = 0;
    struct Statement *initializer_stmt = NULL;
    if (cur != NULL) {
        /* Handle optional initializer wrapper: optional(seq(...)) creates PASCAL_T_NONE node */
        ast_t *init_node = cur;
        if (init_node->typ == PASCAL_T_NONE && init_node->child != NULL) {
            if (absolute_clause_target(init_node) != NULL || is_var_hint_clause(init_node)) {
                init_node = NULL;
            } else {
            /* The wrapper contains "=" token followed by the expression.
             * Skip to find the actual expression node (not the "=" token) */
            ast_t *child = init_node->child;
            if (child != NULL && child->next != NULL) {
                /* Skip the first child (the "=" token) and use the second */
                init_node = child->next;
            }
            }
        }

        /* Skip nodes that should not be treated as initializers */
        if (is_node_to_skip_as_initializer(init_node)) {
            init_node = NULL;
        }
        
        /* Skip EXTERNAL_NAME and PUBLIC_NAME modifiers - they're handled later */
        if (init_node != NULL && 
            init_node->typ != PASCAL_T_EXTERNAL_NAME && 
            init_node->typ != PASCAL_T_EXTERNAL_NAME_EXPR &&
            init_node->typ != PASCAL_T_PUBLIC_NAME) {
            struct Expression *init_expr = convert_expression(init_node);
            if (init_expr != NULL) {
                if (ids != NULL && ids->next == NULL) {
                    inferred = (var_type == UNKNOWN_TYPE && type_id == NULL) ? 1 : 0;
                    char *var_name = (char *)ids->cur;
                    struct Expression *lhs = mk_varid(decl_node->line, strdup(var_name));
                    initializer_stmt = mk_varassign(decl_node->line, decl_node->col, lhs, init_expr);
                } else {
                    destroy_expr(init_expr);
                }
            }
        }
    }

    if (var_type == UNKNOWN_TYPE && ids != NULL && ids->next != NULL && type_node == NULL) {
        ListNode_t *prev = NULL;
        ListNode_t *iter = ids;
        while (iter->next != NULL) {
            prev = iter;
            iter = iter->next;
        }
        if (prev != NULL && iter != NULL && iter->type == LIST_STRING) {
            char *type_name = strdup((char *)iter->cur);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
                mapped = apply_shortstring_mode(mapped, type_name);
                if (mapped == UNKNOWN_TYPE && type_id == NULL)
                    type_id = type_name;
                else
                    free(type_name);
                var_type = mapped;
            }
            prev->next = NULL;
            free(iter->cur);
            free(iter);
        }
    }

    /* Transfer ownership of inline record type from type_info to the vardecl */
    struct RecordType *inline_record = NULL;
    if (type_info.is_record && type_info.record_type != NULL) {
        inline_record = type_info.record_type;
        type_info.record_type = NULL;  /* Transfer ownership */
    }

    struct TypeAlias *inline_alias = NULL;
    if (type_info.is_range)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_range = 1;
            inline_alias->range_known = type_info.range_known;
            inline_alias->range_start = type_info.range_start;
            inline_alias->range_end = type_info.range_end;
            inline_alias->base_type = select_range_primitive_tag(&type_info);
            inline_alias->storage_size = compute_range_storage_size(&type_info);
            if (var_type == UNKNOWN_TYPE)
                var_type = inline_alias->base_type;
        }
    }
    if (inline_alias == NULL && type_info.is_file && type_id == NULL)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_file = 1;
            inline_alias->file_type = type_info.file_type;
            inline_alias->base_type = FILE_TYPE;
            if (type_info.file_type_id != NULL)
                inline_alias->file_type_id = strdup(type_info.file_type_id);
            inline_alias->file_type_ref =
                type_ref_from_single_name(type_info.file_type_id);
        }
    }
    if (inline_alias == NULL && type_info.is_set)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_set = 1;
            inline_alias->set_element_type = type_info.set_element_type;
            inline_alias->base_type = SET_TYPE;
            if (type_info.set_element_type_id != NULL)
                inline_alias->set_element_type_id = strdup(type_info.set_element_type_id);
            inline_alias->set_element_type_ref =
                type_ref_from_single_name(type_info.set_element_type_id);
        }
    }
    if (inline_alias == NULL && type_info.is_enum)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_enum = 1;
            inline_alias->base_type = ENUM_TYPE;
            inline_alias->enum_is_scoped = type_info.enum_is_scoped;
            inline_alias->enum_has_explicit_values = type_info.enum_has_explicit_values;
            inline_alias->enum_literals = type_info.enum_literals;
            type_info.enum_literals = NULL;
        }
    }

    /* Scan for external/public name modifiers (FPC bootstrap compatibility) */
    char *cname_override = NULL;
    int is_external = 0;
    ast_t *scan = decl_node->child;
    while (scan != NULL) {
        if (scan->typ == PASCAL_T_EXTERNAL_NAME) {
            /* External name: variable is defined externally, use specified symbol */
            if (scan->child != NULL && scan->child->typ == PASCAL_T_STRING) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = dup_symbol(scan->child);
            } else if (scan->child != NULL) {
                /* Child may be EXTERNAL_NAME_EXPR wrapping the string */
                char *name = extract_external_name_from_node(scan);
                if (name != NULL) {
                    if (cname_override != NULL)
                        free(cname_override);
                    cname_override = name;
                }
            }
            is_external = 1;
        } else if (scan->typ == PASCAL_T_EXTERNAL_NAME_EXPR) {
            char *name = extract_external_name_from_node(scan);
            if (name != NULL) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = name;
            }
            is_external = 1;
        } else if (scan->typ == PASCAL_T_PUBLIC_NAME) {
            /* Public name: variable is exported with specified symbol.
             * The parser wraps the string in EXTERNAL_NAME_EXPR, so
             * structure is: PUBLIC_NAME -> EXTERNAL_NAME_EXPR -> STRING */
            char *name = extract_external_name_from_node(scan);
            if (name != NULL) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = name;
            } else if (scan->child != NULL && scan->child->typ == PASCAL_T_STRING) {
                if (cname_override != NULL)
                    free(cname_override);
                cname_override = dup_symbol(scan->child);
            }
            /* is_external = 0 for public; variable is defined here but exported */
        }
        scan = scan->next;
    }

    char *absolute_target = NULL;
    {
        ast_t *abs_node = decl_node->child;
        if (kgpc_getenv("KGPC_DEBUG_ABSOLUTE") != NULL) {
            int idx = 0;
            for (ast_t *dbg = abs_node; dbg != NULL; dbg = dbg->next) {
                fprintf(stderr, "[KGPC] absolute scan %d: typ=%d sym=%s\n",
                    idx++, dbg->typ,
                    (dbg->sym != NULL && dbg->sym->name != NULL) ? dbg->sym->name : "<null>");
            }
        }
        while (abs_node != NULL) {
            ast_t *target = absolute_clause_target(abs_node);
            if (target != NULL) {
                absolute_target = dup_symbol(target);
                break;
            }
            abs_node = abs_node->next;
        }
    }
    if (kgpc_debug_decl_scan_enabled() && absolute_target != NULL && ids != NULL && ids->type == LIST_STRING) {
        fprintf(stderr, "[KGPC] var absolute: %s -> %s\n",
            (char *)ids->cur, absolute_target);
    }

    Tree_t *decl = mk_vardecl(decl_node->line, ids, var_type, type_id, 0,
        inferred, initializer_stmt, inline_record, inline_alias, absolute_target);
    if (decl == NULL && absolute_target != NULL)
        free(absolute_target);

    if (decl != NULL)
    {
        decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
        if (absolute_target != NULL)
        {
            char *abs_base = NULL;
            char *abs_field = NULL;
            if (split_absolute_target(absolute_target, &abs_base, &abs_field))
            {
                decl->tree_data.var_decl_data.absolute_base_id = abs_base;
                decl->tree_data.var_decl_data.absolute_field_id = abs_field;
            }
            else
            {
                free(abs_base);
                free(abs_field);
            }
        }
    }
    
    /* Store inline procedure type in cached_kgpc_type */
    if (decl != NULL && inline_proc_type != NULL) {
        decl->tree_data.var_decl_data.cached_kgpc_type = inline_proc_type;
        inline_proc_type = NULL;  /* Transfer ownership */
    } else if (inline_proc_type != NULL) {
        kgpc_type_release(inline_proc_type);
    }
    
    /* Apply external/public name override */
    if (decl != NULL && cname_override != NULL) {
        decl->tree_data.var_decl_data.cname_override = cname_override;
        decl->tree_data.var_decl_data.is_external = is_external;
    } else if (cname_override != NULL) {
        free(cname_override);
    }

    destroy_type_info_contents(&type_info);

    return decl;
}

static ListNode_t *convert_var_section(ast_t *section_node) {
    ListBuilder decls_builder;
    list_builder_init(&decls_builder);
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for var section traversal\n");
        return NULL;
    }
    
    ast_t *cur = section_node->child;
    while (cur != NULL && cur->typ == PASCAL_T_VAR_DECL) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in var section, stopping traversal\n");
            break;
        }
        if (kgpc_debug_decl_scan_enabled()) {
            ast_t *id_node = cur->child;
            if (id_node != NULL && id_node->sym != NULL) {
                fprintf(stderr, "[KGPC] var decl: %s\n", id_node->sym->name);
            }
        }

        Tree_t *decl = convert_var_decl(cur);
        if (decl != NULL)
            list_builder_append(&decls_builder, decl, LIST_TREE);
        cur = cur->next;
    }

    visited_set_destroy(visited);
    return list_builder_finish(&decls_builder);
}

static int lower_const_array(ast_t *const_decl_node, char **id_ptr, TypeInfo *type_info,
                             ast_t *value_node, ListBuilder *var_builder, ast_t *type_section, ast_t *const_section) {
    if (id_ptr == NULL || *id_ptr == NULL || type_info == NULL)
        return -1;

    if (var_builder == NULL) {
        fprintf(stderr,
                "ERROR: Cannot lower const array %s without a variable declaration list.\n",
                *id_ptr);
        return -1;
    }

    /* For multi-dimensional arrays like array[a..b, c..d], we handle them
     * by treating the second dimension as an inner array. The outer dimension
     * determines the number of rows, and the inner dimension determines columns.
     * We support 2D and 3D arrays (one or two levels of nesting). */
    int is_multidim = (type_info->array_dimensions != NULL && type_info->array_dimensions->next != NULL);
    int is_3d = 0;
    int is_4d = 0;
    int multidim_inner_start = 0, multidim_inner_end = -1;
    int multidim_infer_bounds = 0;  /* Set to 1 if we need to infer bounds from initializer */
    int dim3_start = 0, dim3_end = -1;
    int dim3_infer_bounds = 0;
    int dim4_start = 0, dim4_end = -1;
    int dim4_infer_bounds = 0;
    if (is_multidim) {
        /* Check for 3 dimensions */
        if (type_info->array_dimensions->next->next != NULL) {
            is_3d = 1;
            /* Check for 4 dimensions */
            if (type_info->array_dimensions->next->next->next != NULL) {
                is_4d = 1;
                /* Check for more than 4 dimensions - not yet supported */
                if (type_info->array_dimensions->next->next->next->next != NULL) {
                    fprintf(stderr, "ERROR: Unsupported 5+ dimensional const array %s.\n", *id_ptr);
                    return -1;
                }
            }
            /* Extract 2nd dimension bounds */
            const char *dim2_range = (const char *)type_info->array_dimensions->next->cur;
            if (dim2_range != NULL) {
                char *range_copy = strdup(dim2_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        multidim_inner_start = parse_range_bound(range_copy);
                        multidim_inner_end = parse_range_bound(dotdot + 2);
                        if (multidim_inner_start == 0 && multidim_inner_end == 0) {
                            int has_alpha = 0;
                            for (const char *p = range_copy; *p; ++p)
                                if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                            if (has_alpha) {
                                int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                if (s >= 0 && e >= 0) {
                                    multidim_inner_start = s;
                                    multidim_inner_end = e;
                                } else {
                                    multidim_infer_bounds = 1;
                                }
                            }
                        }
                    } else {
                        multidim_infer_bounds = 1;
                    }
                    free(range_copy);
                }
            }
            /* Extract 3rd dimension bounds */
            const char *dim3_range = (const char *)type_info->array_dimensions->next->next->cur;
            if (dim3_range != NULL) {
                char *range_copy = strdup(dim3_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        dim3_start = parse_range_bound(range_copy);
                        dim3_end = parse_range_bound(dotdot + 2);
                        if (dim3_start == 0 && dim3_end == 0) {
                            int has_alpha = 0;
                            for (const char *p = range_copy; *p; ++p)
                                if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                            if (has_alpha) {
                                int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                if (s >= 0 && e >= 0) {
                                    dim3_start = s;
                                    dim3_end = e;
                                } else {
                                    dim3_infer_bounds = 1;
                                }
                            }
                        }
                    } else {
                        dim3_infer_bounds = 1;
                    }
                    free(range_copy);
                }
            }
            /* Extract 4th dimension bounds */
            if (is_4d) {
                const char *dim4_range = (const char *)type_info->array_dimensions->next->next->next->cur;
                if (dim4_range != NULL) {
                    char *range_copy = strdup(dim4_range);
                    if (range_copy != NULL) {
                        char *dotdot = strstr(range_copy, "..");
                        if (dotdot != NULL) {
                            *dotdot = '\0';
                            dim4_start = parse_range_bound(range_copy);
                            dim4_end = parse_range_bound(dotdot + 2);
                            if (dim4_start == 0 && dim4_end == 0) {
                                int has_alpha = 0;
                                for (const char *p = range_copy; *p; ++p)
                                    if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                                if (has_alpha) {
                                    int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                    int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                    if (s >= 0 && e >= 0) {
                                        dim4_start = s;
                                        dim4_end = e;
                                    } else {
                                        dim4_infer_bounds = 1;
                                    }
                                }
                            }
                        } else {
                            dim4_infer_bounds = 1;
                        }
                        free(range_copy);
                    }
                }
            }
        } else {
            /* Extract inner dimension bounds from the second dimension string */
            const char *inner_range = (const char *)type_info->array_dimensions->next->cur;
            if (inner_range != NULL) {
                /* Parse range like "1..240" or "0..3" */
                char *range_copy = strdup(inner_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        multidim_inner_start = parse_range_bound(range_copy);
                        multidim_inner_end = parse_range_bound(dotdot + 2);
                        /* Enum subrange like OS_F32..OS_F128 parses as 0..0;
                         * try resolving enum ordinals first */
                        if (multidim_inner_start == 0 && multidim_inner_end == 0) {
                            int has_alpha = 0;
                            for (const char *p = range_copy; *p; ++p)
                                if (isalpha((unsigned char)*p)) { has_alpha = 1; break; }
                            if (has_alpha) {
                                int s = resolve_enum_ordinal_from_ast(range_copy, type_section);
                                int e = resolve_enum_ordinal_from_ast(dotdot + 2, type_section);
                                if (s >= 0 && e >= 0) {
                                    multidim_inner_start = s;
                                    multidim_inner_end = e;
                                } else {
                                    multidim_infer_bounds = 1;
                                }
                            }
                        }
                    } else {
                        /* Enum type - need to infer bounds from initializer */
                        multidim_infer_bounds = 1;
                    }
                    free(range_copy);
                }
            }
        }
    }

    if (type_info->is_open_array) {
        /* Open array typed constants: count initializer elements and convert
         * to a static array[0..N-1].  This matches FPC behaviour where
         * "const V: array of Integer = (1,2,3)" is laid out as a fixed-size
         * array whose bounds are inferred from the initializer. */
        ast_t *open_tuple = value_node;
        if (open_tuple != NULL) {
            ast_t *uw = unwrap_pascal_node(open_tuple);
            if (uw != NULL &&
                (uw->typ == PASCAL_T_TYPE_SPEC ||
                 uw->typ == PASCAL_T_ARRAY_TYPE ||
                 uw->typ == PASCAL_T_RECORD_TYPE ||
                 uw->typ == PASCAL_T_POINTER_TYPE ||
                 uw->typ == PASCAL_T_PROCEDURE_TYPE ||
                 uw->typ == PASCAL_T_FUNCTION_TYPE) &&
                uw->next != NULL) {
                open_tuple = unwrap_pascal_node(uw->next);
            } else {
                open_tuple = uw;
            }
        }
        int elem_count = 0;
        if (open_tuple == NULL) {
            fprintf(stderr, "ERROR: Open array typed const %s has no elements.\n", *id_ptr);
            return -1;
        } else if (open_tuple->typ == PASCAL_T_TUPLE) {
            for (ast_t *e = open_tuple->child; e != NULL; e = e->next)
                ++elem_count;
        } else {
            /* Single-element parenthesized initializers like "(42)" are parsed
             * as parenthesized expressions rather than one-element tuples. The
             * normal tuple-wrapping path below will canonicalize the AST. */
            elem_count = 1;
        }
        if (elem_count == 0) {
            fprintf(stderr, "ERROR: Open array typed const %s has no elements.\n", *id_ptr);
            return -1;
        }
        type_info->start = 0;
        type_info->end = elem_count - 1;
        type_info->is_open_array = 0;
    }

    ast_t *tuple_node = value_node;
    int is_string_initializer = 0;
    const char *string_initializer = NULL;
    AstStringValue owned_string_initializer = {0};
    if (tuple_node != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(tuple_node);
        if (unwrapped != NULL &&
            (unwrapped->typ == PASCAL_T_TYPE_SPEC ||
             unwrapped->typ == PASCAL_T_ARRAY_TYPE ||
             unwrapped->typ == PASCAL_T_RECORD_TYPE ||
             unwrapped->typ == PASCAL_T_POINTER_TYPE ||
             unwrapped->typ == PASCAL_T_PROCEDURE_TYPE ||
             unwrapped->typ == PASCAL_T_FUNCTION_TYPE) &&
            unwrapped->next != NULL) {
            tuple_node = unwrap_pascal_node(unwrapped->next);
        } else {
            tuple_node = unwrapped;
        }
    }
    if (tuple_node == NULL) {
        fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                *id_ptr);
        return -1;
    }
    int single_record_element = 0;
    int is_widechar_array_target = 0;
    int is_char_array_target = type_info_targets_char_array(type_info, &is_widechar_array_target);
    if (tuple_node->typ == PASCAL_T_STRING) {
        if (is_char_array_target || is_widechar_array_target) {
            is_string_initializer = 1;
            if (tuple_node->sym != NULL && tuple_node->sym->name != NULL)
                string_initializer = tuple_node->sym->name;
        } else {
            fprintf(stderr, "ERROR: Const array %s string initializer requires a char array type.\n",
                    *id_ptr);
            return -1;
        }
    } else if (tuple_node->typ == PASCAL_T_IDENTIFIER) {
        if (!is_char_array_target && !is_widechar_array_target) {
            fprintf(stderr, "ERROR: Const array %s string initializer requires a char array type.\n",
                    *id_ptr);
            return -1;
        }
        if (tuple_node->sym != NULL && tuple_node->sym->name != NULL) {
            if (resolve_const_string_from_ast_internal(tuple_node->sym->name, const_section,
                                                       &string_initializer, 0) == 0 &&
                string_initializer != NULL) {
                is_string_initializer = 1;
            } else {
                fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                        *id_ptr);
                return -1;
            }
        } else {
            fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                    *id_ptr);
            return -1;
        }
    } else if (tuple_node->typ == PASCAL_T_RECORD_CONSTRUCTOR) {
        single_record_element = 1;
    } else if (is_char_array_target || is_widechar_array_target) {
        if (evaluate_const_string_ast(tuple_node, const_section, &owned_string_initializer, 0) == 0) {
            is_string_initializer = 1;
        } else if (tuple_node->typ != PASCAL_T_TUPLE) {
            fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                    *id_ptr);
            ast_string_value_reset(&owned_string_initializer);
            return -1;
        }
    } else if (tuple_node->typ != PASCAL_T_TUPLE) {
        /* Single-element parenthesized initializer: (nil), (0), (expr), etc.
         * The parser parses (expr) as a parenthesized expression rather than
         * a 1-element tuple, so wrap the value into a synthetic TUPLE node. */
        ast_t *wrapper = new_ast();
        wrapper->typ = PASCAL_T_TUPLE;
        wrapper->child = tuple_node;
        wrapper->next = NULL;
        /* Detach from any sibling chain so iteration sees exactly 1 element */
        ast_t *saved_next = tuple_node->next;
        tuple_node->next = NULL;
        tuple_node = wrapper;
        /* Note: wrapper is a small allocation; it is not freed here because
         * the AST is freed in bulk after translation.  saved_next is unused
         * since single-element array consts only have one value. */
        (void)saved_next;
    }

    int start = type_info->start;
    int end = type_info->end;
    
    resolve_array_bounds(type_info, type_section, const_section, *id_ptr);
    start = type_info->start;
    end = type_info->end;
    
    int expected_count = -1;
    if (end >= start)
        expected_count = end - start + 1;

    while (!single_record_element &&
           expected_count != 1 &&
           tuple_node != NULL &&
           tuple_node->typ == PASCAL_T_TUPLE &&
           tuple_node->child != NULL &&
           tuple_node->child->next == NULL) {
        ast_t *nested_tuple = unwrap_pascal_node(tuple_node->child);
        if (nested_tuple == NULL ||
            nested_tuple->typ != PASCAL_T_TUPLE ||
            tuple_is_record_constructor(nested_tuple)) {
            break;
        }
        tuple_node = nested_tuple;
    }

    int actual_count = 0;
    int is_shortstring_target = (type_info->is_shortstring != 0);
    if (is_string_initializer) {
        if (owned_string_initializer.data != NULL || owned_string_initializer.len > 0)
            actual_count = (int)owned_string_initializer.len;
        else if (string_initializer != NULL)
            actual_count = (int)strlen(string_initializer);
    } else if (single_record_element) {
        actual_count = 1;
    } else {
        for (ast_t *elem = tuple_node->child; elem != NULL; elem = elem->next)
            ++actual_count;
    }

    if (expected_count >= 0 && actual_count != expected_count) {
        if (start == 0 && end == 0 && type_info->array_dimensions != NULL &&
            type_info->array_dimensions->cur != NULL) {
            const char *range_str = (const char *)type_info->array_dimensions->cur;
            int has_alpha = 0;
            for (const char *p = range_str; p != NULL && *p != '\0'; ++p) {
                if (isalpha((unsigned char)*p)) {
                    has_alpha = 1;
                    break;
                }
            }
            if (has_alpha) {
                end = start + actual_count - 1;
                expected_count = actual_count;
            }
        }
    }

    if (is_shortstring_target && expected_count > 0) {
        int visible_capacity = expected_count - 1;
        if (actual_count > visible_capacity) {
            ast_string_value_reset(&owned_string_initializer);
            fprintf(stderr,
                    "ERROR: Const shortstring %s initializer length %d exceeds declared capacity %d.\n",
                    *id_ptr, actual_count, visible_capacity);
            return -1;
        }
    } else if (expected_count >= 0 && actual_count != expected_count) {
        ast_string_value_reset(&owned_string_initializer);
        fprintf(stderr,
                "ERROR: Const array %s initializer count %d does not match declared range %d..%d.\n",
                *id_ptr, actual_count, start, end);
        return -1;
    }

    if (expected_count < 0)
        end = start + actual_count - 1;

    ListBuilder stmt_builder;
    list_builder_init(&stmt_builder);

    int index = start;
    TypeInfo element_array_info = {0};
    int element_is_array = 0;
    if (type_info->element_type == UNKNOWN_TYPE && type_info->element_type_id != NULL) {
        if (resolve_array_type_info_from_ast(type_info->element_type_id, type_section, &element_array_info, 0) == 0 &&
            element_array_info.is_array) {
            element_is_array = 1;
            if (element_array_info.array_dimensions != NULL &&
                element_array_info.array_dimensions->next != NULL) {
                fprintf(stderr, "ERROR: Unsupported multi-dimensional array element for const %s.\n", *id_ptr);
                destroy_type_info_contents(&element_array_info);
                return -1;
            }
            resolve_array_bounds(&element_array_info, type_section, const_section, type_info->element_type_id);
        }
    }

    /* For 2D/3D/4D arrays (array[a..b, c..d, ...]), treat as nested arrays */
    int element_is_2d_array = 0;  /* For 3D arrays, elements are 2D arrays */
    int element_is_3d_array = 0;  /* For 4D arrays, elements are 3D arrays */
    TypeInfo element_2d_inner_info = {0};
    TypeInfo element_3d_inner_info = {0};
    if (is_multidim && !element_is_array) {
        element_is_array = 1;
        element_array_info.is_array = 1;
        element_array_info.start = multidim_inner_start;
        element_array_info.end = multidim_inner_end;
        element_array_info.element_type = type_info->element_type;
        element_array_info.element_type_id = type_info->element_type_id != NULL ? strdup(type_info->element_type_id) : NULL;

        /* For 3D arrays, elements of element_array are themselves arrays */
        if (is_3d) {
            element_is_2d_array = 1;
            element_2d_inner_info.is_array = 1;
            element_2d_inner_info.start = dim3_start;
            element_2d_inner_info.end = dim3_end;
            element_2d_inner_info.element_type = type_info->element_type;
            element_2d_inner_info.element_type_id = type_info->element_type_id != NULL ? strdup(type_info->element_type_id) : NULL;
        }

        /* For 4D arrays, elements of element_2d_inner are themselves arrays */
        if (is_4d) {
            element_is_3d_array = 1;
            element_3d_inner_info.is_array = 1;
            element_3d_inner_info.start = dim4_start;
            element_3d_inner_info.end = dim4_end;
            element_3d_inner_info.element_type = type_info->element_type;
            element_3d_inner_info.element_type_id = type_info->element_type_id != NULL ? strdup(type_info->element_type_id) : NULL;
        }

        /* No need to flatten — array_dimensions will be preserved for linearization */

        /* For enum-indexed arrays, infer inner dimension from first row of initializer */
        if (multidim_infer_bounds && tuple_node != NULL && tuple_node->typ == PASCAL_T_TUPLE) {
            ast_t *first_row = tuple_node->child;
            if (first_row != NULL) {
                ast_t *first_row_unwrapped = unwrap_pascal_node(first_row);
                if (first_row_unwrapped != NULL && first_row_unwrapped->typ == PASCAL_T_TUPLE) {
                    int inner_count = 0;
                    for (ast_t *inner = first_row_unwrapped->child; inner != NULL; inner = inner->next)
                        ++inner_count;
                    element_array_info.start = 0;
                    element_array_info.end = inner_count - 1;
                }
            }
        }
        /* For 3D arrays with enum-indexed 3rd dimension, infer from first element */
        if (is_3d && dim3_infer_bounds && tuple_node != NULL && tuple_node->typ == PASCAL_T_TUPLE) {
            ast_t *first_row = tuple_node->child;
            if (first_row != NULL) {
                ast_t *first_row_unwrapped = unwrap_pascal_node(first_row);
                if (first_row_unwrapped != NULL && first_row_unwrapped->typ == PASCAL_T_TUPLE) {
                    ast_t *first_inner = first_row_unwrapped->child;
                    if (first_inner != NULL) {
                        ast_t *first_inner_unwrapped = unwrap_pascal_node(first_inner);
                        if (first_inner_unwrapped != NULL && first_inner_unwrapped->typ == PASCAL_T_TUPLE) {
                            int innermost_count = 0;
                            for (ast_t *elem = first_inner_unwrapped->child; elem != NULL; elem = elem->next)
                                ++innermost_count;
                            element_2d_inner_info.start = 0;
                            element_2d_inner_info.end = innermost_count - 1;
                        }
                    }
                }
            }
        }
        /* For 4D arrays with enum-indexed 4th dimension, infer from first element */
        if (is_4d && dim4_infer_bounds && tuple_node != NULL && tuple_node->typ == PASCAL_T_TUPLE) {
            ast_t *first_row = tuple_node->child;
            if (first_row != NULL) {
                ast_t *first_row_unwrapped = unwrap_pascal_node(first_row);
                if (first_row_unwrapped != NULL && first_row_unwrapped->typ == PASCAL_T_TUPLE) {
                    ast_t *first_inner = first_row_unwrapped->child;
                    if (first_inner != NULL) {
                        ast_t *first_inner_unwrapped = unwrap_pascal_node(first_inner);
                        if (first_inner_unwrapped != NULL && first_inner_unwrapped->typ == PASCAL_T_TUPLE) {
                            ast_t *first_inner2 = first_inner_unwrapped->child;
                            if (first_inner2 != NULL) {
                                ast_t *first_inner2_unwrapped = unwrap_pascal_node(first_inner2);
                                if (first_inner2_unwrapped != NULL && first_inner2_unwrapped->typ == PASCAL_T_TUPLE) {
                                    int innermost_count = 0;
                                    for (ast_t *elem = first_inner2_unwrapped->child; elem != NULL; elem = elem->next)
                                        ++innermost_count;
                                    element_3d_inner_info.start = 0;
                                    element_3d_inner_info.end = innermost_count - 1;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (is_string_initializer) {
        const unsigned char *str = NULL;
        if (owned_string_initializer.data != NULL)
            str = (const unsigned char *)owned_string_initializer.data;
        else if (string_initializer != NULL)
            str = (const unsigned char *)string_initializer;
        if (is_shortstring_target) {
            struct Expression *len_rhs = mk_charcode(const_decl_node->line, (unsigned int)actual_count);
            struct Expression *len_index_expr = mk_inum(const_decl_node->line, index);
            struct Expression *len_base_expr = mk_varid(const_decl_node->line, strdup(*id_ptr));
            struct Expression *len_lhs = mk_arrayaccess(const_decl_node->line, len_base_expr, len_index_expr);
            struct Statement *len_assign = mk_varassign(const_decl_node->line, const_decl_node->col,
                len_lhs, len_rhs);
            list_builder_append(&stmt_builder, len_assign, LIST_STMT);
            ++index;
        }
        for (int i = 0; i < actual_count; ++i) {
            unsigned char byte = (str != NULL) ? str[i] : 0;
            struct Expression *rhs = mk_charcode(const_decl_node->line, (unsigned int)byte);
            struct Expression *index_expr = mk_inum(const_decl_node->line, index);
            struct Expression *base_expr = mk_varid(const_decl_node->line, strdup(*id_ptr));
            if (is_widechar_array_target) {
                base_expr->is_array_expr = 1;
                base_expr->array_element_type = CHAR_TYPE;
                base_expr->array_element_size = 2;
                base_expr->array_element_type_id = strdup("WideChar");
            }
            struct Expression *lhs = mk_arrayaccess(const_decl_node->line, base_expr, index_expr);
            if (is_widechar_array_target) {
                lhs->array_element_type = CHAR_TYPE;
                lhs->array_element_size = 2;
                lhs->array_element_type_id = strdup("WideChar");
                lhs->array_lower_bound = start;
                lhs->array_upper_bound = end;
            }
            struct Statement *assign = mk_varassign(const_decl_node->line, const_decl_node->col, lhs, rhs);
            list_builder_append(&stmt_builder, assign, LIST_STMT);
            ++index;
        }
    } else {
        ast_t *element = single_record_element ? tuple_node : tuple_node->child;
        while (element != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(element);

            if (element_is_array) {
                AstStringValue row_string_initializer = {0};
                int row_is_string_initializer = 0;
                int row_is_widechar_target = 0;
                int row_is_char_array_target =
                    (!element_is_2d_array && !element_is_3d_array &&
                     type_info_targets_char_array(&element_array_info, &row_is_widechar_target));

                if (row_is_char_array_target && unwrapped != NULL &&
                    evaluate_const_string_ast(unwrapped, const_section,
                        &row_string_initializer, 0) == 0)
                {
                    row_is_string_initializer = 1;
                }

                if (!row_is_string_initializer &&
                    (unwrapped == NULL || unwrapped->typ != PASCAL_T_TUPLE)) {
                    fprintf(stderr, "ERROR: Const array %s expects tuple initializer for element %d.\n",
                            *id_ptr, index);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    ast_string_value_reset(&row_string_initializer);
                    return -1;
                }

                int inner_start = element_array_info.start;
                int inner_end = element_array_info.end;
                int inner_expected = -1;
                if (inner_end >= inner_start)
                    inner_expected = inner_end - inner_start + 1;

                int inner_actual = 0;
                if (row_is_string_initializer) {
                    inner_actual = (int)row_string_initializer.len;
                } else {
                    for (ast_t *inner = unwrapped->child; inner != NULL; inner = inner->next)
                        ++inner_actual;
                }

                if (!row_is_string_initializer &&
                    inner_expected >= 0 && inner_actual != inner_expected) {
                    if (inner_start == 0 && inner_end == 0 &&
                        element_array_info.array_dimensions != NULL &&
                        element_array_info.array_dimensions->cur != NULL) {
                        const char *range_str = (const char *)element_array_info.array_dimensions->cur;
                        int has_alpha = 0;
                        for (const char *p = range_str; p != NULL && *p != '\0'; ++p) {
                            if (isalpha((unsigned char)*p)) {
                                has_alpha = 1;
                                break;
                            }
                        }
                        if (has_alpha) {
                            inner_end = inner_start + inner_actual - 1;
                            inner_expected = inner_actual;
                        }
                    }
                }

                if (row_is_string_initializer && inner_expected >= 0 && inner_actual > inner_expected) {
                    fprintf(stderr,
                            "ERROR: Const array %s element %d string initializer length %d exceeds declared range %d..%d.\n",
                            *id_ptr, index, inner_actual, inner_start, inner_end);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    ast_string_value_reset(&row_string_initializer);
                    return -1;
                }

                if (!row_is_string_initializer &&
                    inner_expected >= 0 && inner_actual != inner_expected) {
                    fprintf(stderr,
                            "ERROR: Const array %s element %d initializer count %d does not match declared range %d..%d.\n",
                            *id_ptr, index, inner_actual, inner_start, inner_end);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    ast_string_value_reset(&row_string_initializer);
                    return -1;
                }

                if (row_is_string_initializer) {
                    int fill_count = (inner_expected >= 0) ? inner_expected : inner_actual;
                    int inner_index = inner_start;
                    for (int byte_index = 0; byte_index < fill_count; ++byte_index) {
                        unsigned char byte = 0;
                        struct Expression *rhs;
                        struct Expression *lhs;

                        if ((size_t)byte_index < row_string_initializer.len &&
                            row_string_initializer.data != NULL)
                            byte = (unsigned char)row_string_initializer.data[byte_index];
                        rhs = mk_charcode(element->line, (unsigned int)byte);
                        lhs = mk_const_array_element_lhs(element->line, *id_ptr, index,
                            inner_index, is_multidim);
                        if (row_is_widechar_target) {
                            lhs->array_element_type = CHAR_TYPE;
                            lhs->array_element_size = 2;
                            lhs->array_element_type_id = strdup("WideChar");
                            lhs->array_lower_bound = inner_start;
                            lhs->array_upper_bound = (inner_expected >= 0) ? inner_end :
                                (inner_start + fill_count - 1);
                        }
                        list_builder_append(&stmt_builder,
                            mk_varassign(element->line, element->col, lhs, rhs), LIST_STMT);
                        ++inner_index;
                    }

                    ast_string_value_reset(&row_string_initializer);
                    ++index;
                    element = single_record_element ? NULL : element->next;
                    continue;
                }

                int inner_index = inner_start;
                for (ast_t *inner = unwrapped->child; inner != NULL; inner = inner->next) {
                    ast_t *inner_unwrapped = unwrap_pascal_node(inner);

                    /* For 3D arrays, handle the innermost dimension */
                    if (element_is_2d_array) {
                        if (inner_unwrapped == NULL || inner_unwrapped->typ != PASCAL_T_TUPLE) {
                            fprintf(stderr, "ERROR: Const 3D array %s expects tuple for element [%d][%d].\n",
                                    *id_ptr, index, inner_index);
                            destroy_list(stmt_builder.head);
                            destroy_type_info_contents(&element_array_info);
                            destroy_type_info_contents(&element_2d_inner_info);
                            return -1;
                        }

                        int innermost_index = element_2d_inner_info.start;
                        for (ast_t *innermost = inner_unwrapped->child; innermost != NULL; innermost = innermost->next) {
                            ast_t *innermost_unwrapped = unwrap_pascal_node(innermost);

                            /* For 4D arrays, handle the 4th dimension */
                            if (element_is_3d_array) {
                                if (innermost_unwrapped == NULL || innermost_unwrapped->typ != PASCAL_T_TUPLE) {
                                    fprintf(stderr, "ERROR: Const 4D array %s expects tuple for element [%d][%d][%d].\n",
                                            *id_ptr, index, inner_index, innermost_index);
                                    destroy_list(stmt_builder.head);
                                    destroy_type_info_contents(&element_array_info);
                                    destroy_type_info_contents(&element_2d_inner_info);
                                    destroy_type_info_contents(&element_3d_inner_info);
                                    return -1;
                                }

                                int dim4_index = element_3d_inner_info.start;
                                for (ast_t *dim4_elem = innermost_unwrapped->child; dim4_elem != NULL; dim4_elem = dim4_elem->next) {
                                    ast_t *dim4_unwrapped = unwrap_pascal_node(dim4_elem);
                                    struct Expression *rhs = convert_expression(dim4_unwrapped);
                                    if (rhs == NULL) {
                                        fprintf(stderr, "ERROR: Unsupported const array element in %s[%d][%d][%d][%d].\n",
                                                *id_ptr, index, inner_index, innermost_index, dim4_index);
                                        destroy_list(stmt_builder.head);
                                        destroy_type_info_contents(&element_array_info);
                                        destroy_type_info_contents(&element_2d_inner_info);
                                        destroy_type_info_contents(&element_3d_inner_info);
                                        return -1;
                                    }

                                    struct Expression *outer_index_expr = mk_inum(element->line, index);
                                    struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                                    struct Expression *inner_index_expr = mk_inum(element->line, inner_index);
                                    struct Expression *innermost_index_expr = mk_inum(element->line, innermost_index);
                                    struct Expression *dim4_index_expr = mk_inum(element->line, dim4_index);
                                    struct Expression *lhs;
                                    if (is_multidim) {
                                        /* True multi-dim: arr[d1, d2, d3, d4] */
                                        lhs = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                        ListNode_t *extra3 = CreateListNode(dim4_index_expr, LIST_EXPR);
                                        ListNode_t *extra2 = CreateListNode(innermost_index_expr, LIST_EXPR);
                                        ListNode_t *extra1 = CreateListNode(inner_index_expr, LIST_EXPR);
                                        extra1->next = extra2;
                                        extra2->next = extra3;
                                        lhs->expr_data.array_access_data.extra_indices = extra1;
                                    } else {
                                        /* Array-of-array-of-array-of-array: arr[d1][d2][d3][d4] */
                                        struct Expression *a1 = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                        struct Expression *a2 = mk_arrayaccess(element->line, a1, inner_index_expr);
                                        struct Expression *a3 = mk_arrayaccess(element->line, a2, innermost_index_expr);
                                        lhs = mk_arrayaccess(element->line, a3, dim4_index_expr);
                                    }
                                    struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                                    list_builder_append(&stmt_builder, assign, LIST_STMT);
                                    ++dim4_index;
                                }
                            } else {
                            struct Expression *rhs = convert_expression(innermost_unwrapped);
                            if (rhs == NULL) {
                                fprintf(stderr, "ERROR: Unsupported const array element in %s[%d][%d][%d].\n",
                                        *id_ptr, index, inner_index, innermost_index);
                                destroy_list(stmt_builder.head);
                                destroy_type_info_contents(&element_array_info);
                                destroy_type_info_contents(&element_2d_inner_info);
                                return -1;
                            }

                            struct Expression *outer_index_expr = mk_inum(element->line, index);
                            struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                            struct Expression *inner_index_expr = mk_inum(element->line, inner_index);
                            struct Expression *innermost_index_expr = mk_inum(element->line, innermost_index);
                            struct Expression *lhs;
                            if (is_multidim) {
                                /* True multi-dim: arr[outer, inner, innermost] */
                                lhs = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                ListNode_t *extra2 = CreateListNode(innermost_index_expr, LIST_EXPR);
                                ListNode_t *extra1 = CreateListNode(inner_index_expr, LIST_EXPR);
                                extra1->next = extra2;
                                lhs->expr_data.array_access_data.extra_indices = extra1;
                            } else {
                                /* Array-of-array-of-array: arr[outer][inner][innermost] */
                                struct Expression *outer_access = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                                struct Expression *middle_access = mk_arrayaccess(element->line, outer_access, inner_index_expr);
                                lhs = mk_arrayaccess(element->line, middle_access, innermost_index_expr);
                            }
                            struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                            list_builder_append(&stmt_builder, assign, LIST_STMT);
                            }
                            ++innermost_index;
                        }
                    } else {
                        struct Expression *rhs = convert_expression(inner_unwrapped);
                        if (rhs == NULL) {
                            fprintf(stderr, "ERROR: Unsupported const array element in %s[%d].\n", *id_ptr, index);
                            destroy_list(stmt_builder.head);
                            destroy_type_info_contents(&element_array_info);
                            ast_string_value_reset(&row_string_initializer);
                            return -1;
                        }

                        struct Expression *lhs = mk_const_array_element_lhs(
                            element->line, *id_ptr, index, inner_index, is_multidim);
                        if (row_is_widechar_target) {
                            lhs->array_element_type = CHAR_TYPE;
                            lhs->array_element_size = 2;
                            lhs->array_element_type_id = strdup("WideChar");
                            lhs->array_lower_bound = inner_start;
                            lhs->array_upper_bound = inner_end;
                        }
                        struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                        list_builder_append(&stmt_builder, assign, LIST_STMT);
                    }
                    ++inner_index;
                }

                ast_string_value_reset(&row_string_initializer);
                ++index;
                element = single_record_element ? NULL : element->next;
                continue;
            }

            /* A single-field record constructor (Ch: [...]) gets parsed as FIELD_WIDTH.
               Convert it to a single-child RECORD_CONSTRUCTOR for uniform handling.
               Skip when the field value is a TUPLE — convert_expression's FIELD_WIDTH
               handler already recognizes TUPLE values as record constructors. */
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FIELD_WIDTH &&
                unwrapped->child != NULL && unwrapped->child->typ == PASCAL_T_IDENTIFIER) {
                ast_t *fval = unwrapped->child->next;
                ast_t *fval_unwrapped = unwrap_pascal_node(fval);
                if (fval_unwrapped == NULL || fval_unwrapped->typ != PASCAL_T_TUPLE) {
                    unwrapped->typ = PASCAL_T_ASSIGNMENT;
                    ast_t *wrapper = new_ast();
                    *wrapper = *unwrapped;
                    wrapper->next = NULL;
                    unwrapped->typ = PASCAL_T_RECORD_CONSTRUCTOR;
                    unwrapped->child = wrapper;
                    unwrapped->sym = NULL;
                }
            }
            /* Special handling for record constructors in const arrays */
            if (unwrapped != NULL &&
                (unwrapped->typ == PASCAL_T_RECORD_CONSTRUCTOR ||
                 tuple_is_record_constructor(unwrapped))) {
                /* Generate field assignments for each field in the record constructor */
                ast_t *field_assignment = unwrapped->child;
                while (field_assignment != NULL) {
                    ast_t *assignment_node = unwrap_record_constructor_elem(field_assignment);
                    if (assignment_node != NULL &&
                        (assignment_node->typ == PASCAL_T_ASSIGNMENT ||
                         assignment_node->typ == PASCAL_T_FIELD_WIDTH)) {
                        ast_t *field_name_node = assignment_node->child;
                        ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
                        
                        if (field_name_node != NULL && field_value_node != NULL && field_name_node->sym != NULL) {
                            char *field_name = field_name_node->sym->name;
                            struct Expression *field_value = NULL;
                            if (field_value_node != NULL && field_value_node->typ == PASCAL_T_TUPLE &&
                                tuple_is_record_constructor(field_value_node))
                            {
                                field_value = convert_record_constructor_expr(field_value_node);
                            }
                            else
                            {
                                field_value = convert_expression(field_value_node);
                            }
                            
                            if (field_value == NULL) {
                                fprintf(stderr, "ERROR: Failed to convert field value for %s[%d].%s.\n",
                                        *id_ptr, index, field_name);
                                destroy_list(stmt_builder.head);
                                return -1;
                            }
                            
                            if (field_value != NULL) {
                                /* Create expression: array_name[index].field_name */
                                struct Expression *index_expr = mk_inum(element->line, index);
                                struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                                struct Expression *array_elem = mk_arrayaccess(element->line, base_expr, index_expr);
                                struct Expression *lhs = mk_recordaccess(element->line, array_elem, strdup(field_name));
                                
                                /* Generate assignment statement */
                                struct Statement *field_assign = mk_varassign(element->line, element->col, lhs, field_value);
                                list_builder_append(&stmt_builder, field_assign, LIST_STMT);
                            } else {
                                fprintf(stderr, "ERROR: Unsupported field value in record constructor for %s[%d].%s.\n",
                                        *id_ptr, index, field_name);
                                destroy_list(stmt_builder.head);
                                return -1;
                            }
                        }
                    }
                    field_assignment = field_assignment->next;
                }
            } else {
                /* Regular expression element */
                struct Expression *rhs = convert_expression(unwrapped);
                if (rhs == NULL) {
                    fprintf(stderr, "ERROR: Unsupported const array element in %s.\n", *id_ptr);
                    destroy_list(stmt_builder.head);
                    return -1;
                }

                struct Expression *index_expr = mk_inum(element->line, index);
                struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                struct Expression *lhs = mk_arrayaccess(element->line, base_expr, index_expr);
                struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                list_builder_append(&stmt_builder, assign, LIST_STMT);
            }

            ++index;
            element = single_record_element ? NULL : element->next;
        }
    }

    destroy_type_info_contents(&element_array_info);
    destroy_type_info_contents(&element_2d_inner_info);
    destroy_type_info_contents(&element_3d_inner_info);
    ast_string_value_reset(&owned_string_initializer);

    ListNode_t *assignments = list_builder_finish(&stmt_builder);
    struct Statement *initializer = NULL;
    if (assignments != NULL)
        initializer = mk_compoundstatement(const_decl_node->line, assignments);

    ListNode_t *ids = CreateListNode(*id_ptr, LIST_STRING);
    char *range_str = NULL;
    if (type_info->array_dimensions != NULL && type_info->array_dimensions->cur != NULL) {
        range_str = strdup((char *)type_info->array_dimensions->cur);
    }
    int decl_element_type = type_info->element_type;
    char *decl_element_type_id = type_info->element_type_id != NULL ?
        strdup(type_info->element_type_id) : NULL;
    struct RecordType *inline_record = type_info->record_type;
    if (inline_record != NULL)
        type_info->record_type = NULL;
    Tree_t *array_decl = mk_arraydecl(const_decl_node->line, ids, decl_element_type,
                                      decl_element_type_id, start, end, range_str, initializer,
                                      inline_record);
    array_decl->tree_data.arr_decl_data.type_ref =
        type_ref_from_element_info(type_info, type_info->element_type_id);
    if (type_info->element_kgpc_type != NULL)
    {
        array_decl->tree_data.arr_decl_data.element_kgpc_type = type_info->element_kgpc_type;
        kgpc_type_retain(array_decl->tree_data.arr_decl_data.element_kgpc_type);
    }
    type_info->element_type_id = NULL;

    /* Transfer array_dimensions to declaration for multi-dim linearization.
     * Only when 2+ dimensions — single-dim doesn't need linearization. */
    if (type_info->array_dimensions != NULL &&
        type_info->array_dimensions->next != NULL) {
        array_decl->tree_data.arr_decl_data.array_dimensions = type_info->array_dimensions;
        type_info->array_dimensions = NULL;
    }

    if (type_info->unresolved_index_type != NULL) {
        array_decl->tree_data.arr_decl_data.unresolved_index_type = type_info->unresolved_index_type;
        type_info->unresolved_index_type = NULL;
    }
    /* Fallback: for single-dim enum-indexed arrays where resolve_array_bounds
     * didn't set unresolved_index_type (e.g., enum was defined later), extract
     * the dimension name from array_dimensions. */
    if (array_decl->tree_data.arr_decl_data.unresolved_index_type == NULL &&
        start == 0 && end == 0 &&
        type_info->array_dimensions != NULL &&
        type_info->array_dimensions->type == LIST_STRING &&
        type_info->array_dimensions->cur != NULL)
    {
        const char *dim_str = (const char *)type_info->array_dimensions->cur;
        if (strstr(dim_str, "..") == NULL) {
            array_decl->tree_data.arr_decl_data.unresolved_index_type = strdup(dim_str);
        }
    }
    array_decl->tree_data.arr_decl_data.is_typed_const = 1;
    array_decl->tree_data.arr_decl_data.has_static_storage = 1;

    char label_buffer[128];
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_array_%s_%d", g_typed_const_unit_tag, typed_const_counter);
    array_decl->tree_data.arr_decl_data.static_label = strdup(label_buffer);
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_guard_%s_%d", g_typed_const_unit_tag, typed_const_counter);
    array_decl->tree_data.arr_decl_data.init_guard_label = strdup(label_buffer);
    ++typed_const_counter;

    list_builder_append(var_builder, array_decl, LIST_TREE);

    *id_ptr = NULL;
    return 0;
}

static Tree_t *convert_const_decl(ast_t *const_decl_node, ListBuilder *var_builder, ast_t *type_section, ast_t *const_section) {
    if (const_decl_node == NULL)
        return NULL;

    ast_t *cur = const_decl_node->child;
    if (cur == NULL)
        return NULL;

    char *id = dup_symbol(cur);
    if (id == NULL)
        return NULL;

    cur = cur->next;
    char *type_id = NULL;
    TypeInfo type_info = {0};

    if (cur != NULL &&
        (cur->typ == PASCAL_T_TYPE_SPEC ||
         cur->typ == PASCAL_T_ARRAY_TYPE ||
         cur->typ == PASCAL_T_RECORD_TYPE ||
         cur->typ == PASCAL_T_POINTER_TYPE ||
         cur->typ == PASCAL_T_PROCEDURE_TYPE ||
         cur->typ == PASCAL_T_FUNCTION_TYPE)) {
        int spec_type = convert_type_spec(cur, &type_id, NULL, &type_info);
        /* For inline procedure/function type constants, create a typed variable.
         * convert_type_spec returns PROCEDURE but doesn't set type_id for inline
         * procedure types like: VarClearProc: procedure(var v: TVarData) = nil; */
        ast_t *unwrapped_cur = cur;
        if (unwrapped_cur->typ == PASCAL_T_TYPE_SPEC && unwrapped_cur->child != NULL)
            unwrapped_cur = unwrapped_cur->child;
        unwrapped_cur = unwrap_pascal_node(unwrapped_cur);
        if (spec_type == PROCEDURE && type_id == NULL && unwrapped_cur != NULL &&
            (unwrapped_cur->typ == PASCAL_T_PROCEDURE_TYPE || unwrapped_cur->typ == PASCAL_T_FUNCTION_TYPE)) {
            ast_t *proc_type_node = unwrapped_cur;
            cur = cur->next;
            ast_t *init_value = unwrap_pascal_node(cur);
            struct Expression *init_expr = convert_expression(init_value);
            ListNode_t *ids = CreateListNode(id, LIST_STRING);
            struct Expression *lhs = mk_varid(const_decl_node->line, strdup(id));
            struct Statement *initializer_stmt = NULL;
            if (init_expr != NULL)
                initializer_stmt = mk_varassign(const_decl_node->line, const_decl_node->col,
                                                 lhs, init_expr);
            else
                destroy_expr(lhs);
            Tree_t *decl = mk_vardecl(const_decl_node->line, ids, PROCEDURE, NULL, 0, 0,
                                       initializer_stmt, NULL, NULL, NULL);
            decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(decl);
            decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
            /* Attach the inline procedure type for proper type checking */
            KgpcType *proc_kgpc = convert_type_spec_to_kgpctype(proc_type_node, NULL);
            if (proc_kgpc != NULL) {
                decl->tree_data.var_decl_data.cached_kgpc_type = proc_kgpc;
                kgpc_type_retain(proc_kgpc);
            }
            list_builder_append(var_builder, decl, LIST_TREE);
            destroy_type_info_contents(&type_info);
            return NULL;
        }
        cur = cur->next;
    } else if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER &&
               cur->sym != NULL && cur->sym->name != NULL &&
               cur->next != NULL) {
        /* Bare identifier as type annotation (common in record/class const sections).
         * AST: IDENTIFIER("Single") -> REAL("1.5")
         * The identifier is the type name, the next sibling is the value.
         * Only consume when map_type_name recognizes it as a known type
         * to avoid misinterpreting keywords like "set" in "set of TEnum". */
        char *type_name = dup_symbol(cur);
        if (type_name != NULL) {
            int mapped = map_type_name(type_name, &type_id);
            mapped = apply_shortstring_mode(mapped, type_name);
            if (mapped != UNKNOWN_TYPE || type_id != NULL) {
                /* Known type name — consume and advance */
                free(type_name);
                cur = cur->next;
            } else {
                /* Unknown type name — treat as type annotation for typed consts
                 * like: cnodeutils: tnodeutilsclass = tnodeutils
                 * where 'tnodeutilsclass' is not a builtin type. */
                type_id = type_name;
                cur = cur->next;
            }
        }
    } else if (cur != NULL && cur->typ == PASCAL_T_NONE) {
        ast_t *type_node = cur->child;
        while (type_node != NULL &&
               type_node->typ != PASCAL_T_TYPE_SPEC &&
               type_node->typ != PASCAL_T_ARRAY_TYPE &&
               type_node->typ != PASCAL_T_RECORD_TYPE &&
               type_node->typ != PASCAL_T_POINTER_TYPE &&
               type_node->typ != PASCAL_T_PROCEDURE_TYPE &&
               type_node->typ != PASCAL_T_FUNCTION_TYPE &&
               type_node->typ != PASCAL_T_IDENTIFIER) {
            type_node = type_node->next;
        }
        if (type_node != NULL) {
            if (type_node->typ == PASCAL_T_TYPE_SPEC ||
                type_node->typ == PASCAL_T_ARRAY_TYPE ||
                type_node->typ == PASCAL_T_RECORD_TYPE ||
                type_node->typ == PASCAL_T_POINTER_TYPE ||
                type_node->typ == PASCAL_T_PROCEDURE_TYPE ||
                type_node->typ == PASCAL_T_FUNCTION_TYPE) {
                convert_type_spec(type_node, &type_id, NULL, &type_info);
            } else if (type_node->typ == PASCAL_T_IDENTIFIER) {
                char *type_name = dup_symbol(type_node);
                if (type_name != NULL) {
                    int mapped = map_type_name(type_name, &type_id);
                    mapped = apply_shortstring_mode(mapped, type_name);
                    if (mapped == UNKNOWN_TYPE && type_id == NULL)
                        type_id = type_name;
                    else
                        free(type_name);
                }
            }
            cur = cur->next;
        }
    }

    /* When the type is a named identifier (like TNames), resolve it to check
     * if it's an array type alias so we can use lower_const_array. */
    if (type_id != NULL && !type_info.is_array) {
        TypeInfo resolved_info = {0};
        if (resolve_array_type_info_from_ast(type_id, type_section, &resolved_info, 0) == 0) {
            resolve_array_bounds(&resolved_info, type_section, const_section, id);
            type_info = resolved_info;
        }
    }

    if (type_id == NULL && const_section_is_resourcestring(const_section)) {
        type_id = strdup("AnsiString");
    }

    ast_t *value_node = unwrap_pascal_node(cur);
    if (value_node == NULL) {
        fprintf(stderr, "ERROR: Unsupported const declaration for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        destroy_type_info_contents(&type_info);
        return NULL;
    }

    /* A single-field record constructor like (Ch: [Ch_Mop2]) gets parsed as a
       parenthesized FIELD_WIDTH expression (the : is the WriteLn format operator).
       When we have a typed const, convert FIELD_WIDTH back to a single-field
       RECORD_CONSTRUCTOR so the lowering code handles it correctly.
       FIELD_WIDTH structure:  child=IDENTIFIER(Ch) -> next=SET(...)
       Needed structure:       RECORD_CONSTRUCTOR -> child=ASSIGNMENT -> child=IDENTIFIER(Ch) -> next=SET(...)
       We repurpose the FIELD_WIDTH node as the ASSIGNMENT and create a wrapper. */
    if (value_node->typ == PASCAL_T_FIELD_WIDTH && type_id != NULL &&
        value_node->child != NULL && value_node->child->typ == PASCAL_T_IDENTIFIER) {
        /* Check the field value — if it's a TUPLE, the existing convert_expression
           FIELD_WIDTH handler already handles it as a single-field record constructor.
           Only convert for non-TUPLE values (e.g. set constructors) that need help. */
        ast_t *field_val = value_node->child->next;
        ast_t *field_val_unwrapped = unwrap_pascal_node(field_val);
        if (field_val_unwrapped == NULL || field_val_unwrapped->typ != PASCAL_T_TUPLE) {
            /* Turn the FIELD_WIDTH into an ASSIGNMENT (same child structure) */
            value_node->typ = PASCAL_T_ASSIGNMENT;
            /* Wrap: create a RECORD_CONSTRUCTOR that replaces value_node in the AST.
               We do this by swapping: copy value_node's ASSIGNMENT data into a new node,
               then make value_node the RECORD_CONSTRUCTOR pointing to it. */
            ast_t *assignment = new_ast();
            *assignment = *value_node;
            assignment->next = NULL;
            value_node->typ = PASCAL_T_RECORD_CONSTRUCTOR;
            value_node->child = assignment;
            value_node->sym = NULL;
        }
    }

    if (type_info.is_array) {
        if (lower_const_array(const_decl_node, &id, &type_info, value_node, var_builder, type_section, const_section) != 0)
            free(id);

        if (type_id != NULL)
            free(type_id);
        destroy_type_info_contents(&type_info);
        return NULL;
    }

    if (type_id != NULL) {
        int empty_tuple_record_const = 0;
        if (value_node != NULL && (value_node->typ == PASCAL_T_TUPLE || value_node->typ == PASCAL_T_NONE)) {
            ast_t *child = value_node->child;
            if (child == NULL ||
                (child->typ == PASCAL_T_NONE && child->child == NULL && child->next == NULL)) {
                empty_tuple_record_const = 1;
            }
        }

        int typed_const_tag = map_type_name(type_id, NULL);
        if (typed_const_tag == STRING_TYPE) {
            struct Expression *init_expr = convert_expression(value_node);
            if (init_expr == NULL) {
                fprintf(stderr, "ERROR: Unsupported typed const expression for %s.\n", id);
                free(id);
                free(type_id);
                destroy_type_info_contents(&type_info);
                return NULL;
            }

            ListNode_t *ids = CreateListNode(id, LIST_STRING);
            struct Expression *lhs = mk_varid(const_decl_node->line, strdup(id));
            struct Statement *initializer_stmt = mk_varassign(const_decl_node->line, const_decl_node->col,
                                                              lhs, init_expr);
            Tree_t *decl = mk_vardecl(const_decl_node->line, ids, typed_const_tag, type_id, 0, 0,
                                      initializer_stmt, NULL, NULL, NULL);
            decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(decl);
            decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
            list_builder_append(var_builder, decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }
        /* If the value is a tuple that looks like a record constructor (field: value; ...),
           treat it as a record constructor instead of a plain expression. */
        if (value_node != NULL && value_node->typ == PASCAL_T_TUPLE &&
            tuple_is_record_constructor(value_node)) {
            value_node->typ = PASCAL_T_RECORD_CONSTRUCTOR;
        }
        if (value_node != NULL && value_node->typ != PASCAL_T_RECORD_CONSTRUCTOR && !empty_tuple_record_const) {
            struct Expression *init_expr = convert_expression(value_node);
            if (init_expr == NULL) {
                fprintf(stderr, "ERROR: Unsupported typed const expression for %s.\n", id);
                free(id);
                free(type_id);
                destroy_type_info_contents(&type_info);
                return NULL;
            }

            ListNode_t *ids = CreateListNode(id, LIST_STRING);
            struct Expression *lhs = mk_varid(const_decl_node->line, strdup(id));
            struct Statement *initializer_stmt = mk_varassign(const_decl_node->line, const_decl_node->col,
                                                              lhs, init_expr);
            Tree_t *decl = mk_vardecl(const_decl_node->line, ids, typed_const_tag, type_id, 0, 0,
                                      initializer_stmt, NULL, NULL, NULL);
            decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(decl);
            decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);
            list_builder_append(var_builder, decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }

        if (empty_tuple_record_const) {
            int var_type = UNKNOWN_TYPE;
            struct RecordType *inline_record = NULL;
            if (type_id != NULL) {
                var_type = map_type_name(type_id, NULL);
                var_type = apply_shortstring_mode(var_type, type_id);
            } else if (type_info.is_record && type_info.record_type != NULL)
            {
                var_type = RECORD_TYPE;
                inline_record = type_info.record_type;
                type_info.record_type = NULL;
            }

            ListNode_t *var_ids = CreateListNode(id, LIST_STRING);
            Tree_t *var_decl = mk_vardecl(const_decl_node->line, var_ids, var_type,
                type_id, 0, 0, NULL, inline_record, NULL, NULL);
            var_decl->tree_data.var_decl_data.is_typed_const = 1;
            mark_var_decl_static_storage(var_decl);
            var_decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);

            if (var_builder != NULL)
                list_builder_append(var_builder, var_decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }
    }

    /* Handle record constructor constants by lowering to variable with field initializers */
    if (value_node != NULL && value_node->typ == PASCAL_T_RECORD_CONSTRUCTOR) {
        /* Lower record const to variable declaration with field-by-field initialization */
        ListBuilder field_stmts;
        list_builder_init(&field_stmts);
        
        struct RecordType *record_info = NULL;
        if (type_info.is_record && type_info.record_type != NULL)
            record_info = type_info.record_type;

        ast_t *field_assignment = value_node->child;
        while (field_assignment != NULL) {
            if (field_assignment->typ == PASCAL_T_ASSIGNMENT ||
                field_assignment->typ == PASCAL_T_FIELD_WIDTH) {
                ast_t *field_name_node = field_assignment->child;
                ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
                
                if (field_name_node != NULL && field_value_node != NULL && field_name_node->sym != NULL) {
                    char *field_name = field_name_node->sym->name;
                    struct Expression *field_value = NULL;
                    if (field_value_node != NULL && field_value_node->typ == PASCAL_T_TUPLE &&
                        tuple_is_record_constructor(field_value_node))
                    {
                        field_value = convert_record_constructor_expr(field_value_node);
                    }
                    else
                    {
                        field_value = convert_expression(field_value_node);
                    }
                    
                    if (field_value != NULL) {
                        if (record_info != NULL && field_value->type == EXPR_ARRAY_LITERAL) {
                            struct RecordField *field_desc =
                                find_record_field_by_name(record_info, field_name);
                            if (field_desc != NULL && field_desc->is_array) {
                                if (kgpc_getenv("KGPC_DEBUG_RECORD_CONST") != NULL)
                                {
                                    fprintf(stderr,
                                        "[KGPC] record const %s.%s array elem type=%d id=%s\n",
                                        id,
                                        field_name,
                                        field_desc->array_element_type,
                                        field_desc->array_element_type_id != NULL
                                            ? field_desc->array_element_type_id
                                            : "<null>");
                                }
                                if (field_value->array_element_type == UNKNOWN_TYPE)
                                    field_value->array_element_type = field_desc->array_element_type;
                                if (field_value->array_element_type_id == NULL &&
                                    field_desc->array_element_type_id != NULL)
                                    field_value->array_element_type_id =
                                        strdup(field_desc->array_element_type_id);
                            }
                        }
                        /* Create expression: const_name.field_name */
                        struct Expression *base_expr = mk_varid(const_decl_node->line, strdup(id));
                        struct Expression *lhs = mk_recordaccess(const_decl_node->line, base_expr, strdup(field_name));
                        
                        /* Generate assignment statement */
                        struct Statement *field_assign = mk_varassign(const_decl_node->line, const_decl_node->col, lhs, field_value);
                        list_builder_append(&field_stmts, field_assign, LIST_STMT);
                    } else {
                        fprintf(stderr, "ERROR: Unsupported field value in record constructor for %s.%s.\n",
                                id, field_name);
                        destroy_list(field_stmts.head);
                        if (type_id != NULL)
                            free(type_id);
                        free(id);
                        destroy_type_info_contents(&type_info);
                        return NULL;
                    }
                }
            }
            field_assignment = field_assignment->next;
        }
        
        ListNode_t *field_assignments = list_builder_finish(&field_stmts);
        struct Statement *initializer = NULL;
        if (field_assignments != NULL)
            initializer = mk_compoundstatement(const_decl_node->line, field_assignments);
        
        /* Determine the record type from type_id or inline type info */
        int var_type = UNKNOWN_TYPE;
        struct RecordType *inline_record = NULL;
        if (type_id != NULL) {
            var_type = map_type_name(type_id, NULL);
            var_type = apply_shortstring_mode(var_type, type_id);
        } else if (type_info.is_record && type_info.record_type != NULL) {
            var_type = RECORD_TYPE;
            inline_record = type_info.record_type;
            type_info.record_type = NULL;
        }
        
        /* Create variable declaration for the record const */
        ListNode_t *var_ids = CreateListNode(id, LIST_STRING);
        Tree_t *var_decl = mk_vardecl(const_decl_node->line, var_ids, var_type,
            type_id, 0, 0, initializer, inline_record, NULL, NULL);
        var_decl->tree_data.var_decl_data.is_typed_const = 1;
        mark_var_decl_static_storage(var_decl);
        var_decl->tree_data.var_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);

        if (var_builder != NULL)
            list_builder_append(var_builder, var_decl, LIST_TREE);
        
        destroy_type_info_contents(&type_info);
        return NULL; /* Record const is lowered to variable, no const decl returned */
    }

    int const_value = 0;
    if (evaluate_const_int_expr(value_node, &const_value, 0) == 0) {
        register_const_int(id, const_value);
    } else {
        /* Handle scoped enum literals like TEnum.Value */
        ast_t *unwrapped_value = unwrap_pascal_node(value_node);
        if (unwrapped_value != NULL && unwrapped_value->typ == PASCAL_T_MEMBER_ACCESS) {
            ast_t *base = unwrap_pascal_node(unwrapped_value->child);
            if (base != NULL && base->typ == PASCAL_T_IDENTIFIER &&
                base->sym != NULL && base->sym->name != NULL &&
                unwrapped_value->sym != NULL && unwrapped_value->sym->name != NULL) {
                int ordinal = resolve_enum_literal_in_type(
                    base->sym->name, unwrapped_value->sym->name, type_section);
                if (ordinal >= 0) {
                    register_const_int(id, ordinal);
                }
            }
        } else if (unwrapped_value != NULL && unwrapped_value->typ == PASCAL_T_FUNC_CALL) {
            /* Treat TypeName(expr) as a typecast in const expressions. */
            const char *callee = (unwrapped_value->sym != NULL) ? unwrapped_value->sym->name : NULL;
            int is_type = 0;
            if (callee != NULL) {
                if (map_type_name(callee, NULL) != UNKNOWN_TYPE)
                    is_type = 1;
                else if (type_name_exists_in_section(callee, type_section))
                    is_type = 1;
            }

            if (is_type) {
                ast_t *args = unwrap_pascal_node(unwrapped_value->child);
                ast_t *arg = (args != NULL) ? unwrap_pascal_node(args->child) : NULL;
                if (arg != NULL && (args->child == NULL || args->child->next == NULL)) {
                    if (evaluate_const_int_expr(arg, &const_value, 0) == 0)
                        register_const_int(id, const_value);
                }
            }
        }
    }

    struct Expression *value_expr = convert_expression(value_node);
    if (value_expr == NULL) {
        fprintf(stderr, "ERROR: Unsupported const expression for %s.\n", id);
        if (type_id != NULL)
            free(type_id);
        free(id);
        return NULL;
    }

    Tree_t *decl = mk_constdecl(const_decl_node->line, id, type_id, value_expr);
    if (decl != NULL)
        decl->tree_data.const_decl_data.type_ref = type_ref_from_info_or_id(&type_info, type_id);

    destroy_type_info_contents(&type_info);

    return decl;
}

static void append_const_decls_from_section(ast_t *const_section, ListNode_t **dest,
                                            ListBuilder *var_builder, ast_t *type_section) {
    if (const_section == NULL || dest == NULL)
        return;

    register_const_section(const_section);

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ast_t *const_decl = const_section->child;
    while (const_decl != NULL) {
        ast_t *node = unwrap_pascal_node(const_decl);
        if (node == NULL)
            node = const_decl;
        if (node != NULL && node->typ == PASCAL_T_CONST_DECL) {
            if (kgpc_debug_decl_scan_enabled()) {
                ast_t *id_node = node->child;
                if (id_node != NULL && id_node->sym != NULL) {
                    fprintf(stderr, "[KGPC] const decl: %s\n", id_node->sym->name);
                }
            }
            Tree_t *decl = convert_const_decl(node, var_builder, type_section, const_section);
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
        }
        const_decl = const_decl->next;
    }
}

static int select_range_primitive_tag(const TypeInfo *info)
{
    if (info == NULL || !info->is_range)
        return INT_TYPE;

    if (!info->range_known)
        return INT_TYPE;

    long long start = info->range_start;
    long long end = info->range_end;
    if (start >= 0 && end < 0)
    {
        unsigned long long u_end = (unsigned long long)end;
        if (u_end <= 0xFFULL)
            return BYTE_TYPE;
        if (u_end <= 0xFFFFULL)
            return WORD_TYPE;
        if (u_end <= 0xFFFFFFFFULL)
            return LONGWORD_TYPE;
        return QWORD_TYPE;
    }

    if (start > end) {
        long long tmp = start;
        start = end;
        end = tmp;
    }

    if (start >= 0)
    {
        if (end <= 0xFF)
            return BYTE_TYPE;
        if (end <= 0xFFFF)
            return WORD_TYPE;
        if (end <= 0xFFFFFFFFLL)
            return LONGWORD_TYPE;
        return QWORD_TYPE;
    }

    if (start >= INT_MIN && end <= INT_MAX)
        return INT_TYPE;

    /* Signed range exceeds 32-bit, use 64-bit Int64 */
    return INT64_TYPE;
}

static long long compute_range_storage_size(const TypeInfo *info);

static long long compute_range_storage_size(const TypeInfo *info)
{
    if (info == NULL || !info->is_range || !info->range_known)
        return 0;

    long long start = info->range_start;
    long long end = info->range_end;
    if (start >= 0 && end < 0)
    {
        unsigned long long u_end = (unsigned long long)end;
        if (u_end <= 0xFFULL)
            return 1;
        if (u_end <= 0xFFFFULL)
            return 2;
        if (u_end <= 0xFFFFFFFFULL)
            return 4;
        return 8;
    }

    if (start > end)
    {
        long long tmp = start;
        start = end;
        end = tmp;
    }

    if (start >= 0)
    {
        if (end <= 0xFF)
            return 1;
        if (end <= 0xFFFF)
            return 2;
        if (end <= 0xFFFFFFFFLL)
            return 4;
        return 8;
    }

    /* Signed ranges */
    if (start >= -128 && end <= 127)
        return 1;
    if (start >= -32768 && end <= 32767)
        return 2;
    if (start >= INT_MIN && end <= INT_MAX)
        return 4;

    return 8;
}

static Tree_t *convert_type_decl_ex(ast_t *type_decl_node, ListNode_t **method_clones, ListNode_t **nested_type_decls_out) {
    if (type_decl_node == NULL)
        return NULL;

    if (nested_type_decls_out != NULL)
        *nested_type_decls_out = NULL;

    type_decl_node = unwrap_pascal_node(type_decl_node);
    if (type_decl_node == NULL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
        id_node = id_node->next;
    if (id_node == NULL)
        return NULL;

    char *id = dup_symbol(id_node);
    if (id == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL && spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_RECORD_TYPE && spec_node->typ != PASCAL_T_OBJECT_TYPE &&
           spec_node->typ != PASCAL_T_CLASS_TYPE &&
           spec_node->typ != PASCAL_T_INTERFACE_TYPE &&
           spec_node->typ != PASCAL_T_PROCEDURE_TYPE &&
           spec_node->typ != PASCAL_T_FUNCTION_TYPE &&
           spec_node->typ != PASCAL_T_REFERENCE_TO_TYPE) {
        spec_node = spec_node->next;
    }

    char *type_id = NULL;
    struct RecordType *record_type = NULL;
    TypeInfo type_info = {0};
    int mapped_type = UNKNOWN_TYPE;
    ast_t *class_spec = NULL;
    ast_t *interface_spec = NULL;
    ListNode_t *nested_type_sections = NULL;
    if (spec_node != NULL) {
        const char *trace_sym = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_sym != NULL && id != NULL && pascal_identifier_equals(id, trace_sym))
        {
            ast_t *dbg = spec_node;
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] raw_type_decl id=%s spec typ=%d sym=%s child_typ=%d child_sym=%s next_typ=%d next_sym=%s\n",
                id,
                dbg != NULL ? dbg->typ : -1,
                (dbg != NULL && dbg->sym != NULL && dbg->sym->name != NULL) ? dbg->sym->name : "<null>",
                (dbg != NULL && dbg->child != NULL) ? dbg->child->typ : -1,
                (dbg != NULL && dbg->child != NULL && dbg->child->sym != NULL && dbg->child->sym->name != NULL) ? dbg->child->sym->name : "<null>",
                (dbg != NULL && dbg->next != NULL) ? dbg->next->typ : -1,
                (dbg != NULL && dbg->next != NULL && dbg->next->sym != NULL && dbg->next->sym->name != NULL) ? dbg->next->sym->name : "<null>");
            if (dbg != NULL && dbg->child != NULL)
            {
                fprintf(stderr,
                    "[KGPC_TRACE_NONLOCAL] raw_type_decl child child_typ=%d child_sym=%s next_typ=%d next_sym=%s\n",
                    dbg->child->child != NULL ? dbg->child->child->typ : -1,
                    (dbg->child->child != NULL && dbg->child->child->sym != NULL && dbg->child->child->sym->name != NULL) ? dbg->child->child->sym->name : "<null>",
                    dbg->child->next != NULL ? dbg->child->next->typ : -1,
                    (dbg->child->next != NULL && dbg->child->next->sym != NULL && dbg->child->next->sym->name != NULL) ? dbg->child->next->sym->name : "<null>");
            }
        }
        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
            fprintf(stderr, "[KGPC] convert_type_decl spec_node typ=%d sym=%s for id=%s\n",
                spec_node->typ,
                (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>",
                id != NULL ? id : "<null>");
        if (spec_node->typ == PASCAL_T_CLASS_TYPE) {
            class_spec = spec_node;
        } else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                   spec_node->child->typ == PASCAL_T_CLASS_TYPE) {
            class_spec = spec_node->child;
        }
        /* Handle "class of T" - check if TYPE_SPEC wraps CLASS_OF_TYPE */
        else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                 spec_node->child->typ == PASCAL_T_CLASS_OF_TYPE) {
            /* For class of T, we need to find T's record type and use it */
            ast_t *class_of_node = spec_node->child;
            ast_t *target_name = class_of_node->child;
            while (target_name != NULL && target_name->typ != PASCAL_T_IDENTIFIER)
                target_name = target_name->next;
            if (target_name != NULL && kgpc_getenv("KGPC_DEBUG_TFPG") != NULL) {
                fprintf(stderr, "[KGPC] convert_type_decl: class of target=%s for id=%s\n",
                    (target_name->sym && target_name->sym->name) ? target_name->sym->name : "<null>",
                    id);
            }
        }

        if (spec_node->typ == PASCAL_T_INTERFACE_TYPE) {
            interface_spec = spec_node;
        } else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                   spec_node->child->typ == PASCAL_T_INTERFACE_TYPE) {
            interface_spec = spec_node->child;
        }

        if (interface_spec != NULL) {
            record_type = convert_interface_type_ex(id, interface_spec, &nested_type_sections);
            if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                ListNode_t *section_cursor = nested_type_sections;
                while (section_cursor != NULL) {
                    ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                    if (type_section_ast != NULL) {
                        append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                            NULL, NULL, NULL, id);
                    }
                    section_cursor = section_cursor->next;
                }
            }
            while (nested_type_sections != NULL) {
                ListNode_t *next = nested_type_sections->next;
                free(nested_type_sections);
                nested_type_sections = next;
            }
        } else if (class_spec != NULL) {
            record_type = convert_class_type_ex(id, class_spec, &nested_type_sections);
            /* Process nested type sections - extract and convert nested type declarations */
            if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                ListNode_t *section_cursor = nested_type_sections;
                while (section_cursor != NULL) {
                    ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                    if (type_section_ast != NULL) {
                        /* Recursively process this nested type section */
                        append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                            NULL, NULL, NULL, id);
                        if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                            fprintf(stderr, "[KGPC] convert_type_decl: processed nested TYPE_SECTION for class %s\n", id);
                    }
                    section_cursor = section_cursor->next;
                }
            }
            /* Clean up the section list (don't destroy AST nodes, just the list) */
            while (nested_type_sections != NULL) {
                ListNode_t *next = nested_type_sections->next;
                free(nested_type_sections);
                nested_type_sections = next;
            }
        } else {
            /* Check if this is a record type - also extract nested types */
            ast_t *record_spec = NULL;
            if (spec_node->typ == PASCAL_T_RECORD_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE) {
                record_spec = spec_node;
            } else if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
                       (spec_node->child->typ == PASCAL_T_RECORD_TYPE || spec_node->child->typ == PASCAL_T_OBJECT_TYPE)) {
                record_spec = spec_node->child;
            }

            if (record_spec != NULL) {
                /* Handle record type with nested type extraction */
                record_type = convert_record_type_ex(record_spec, &nested_type_sections);
                mapped_type = RECORD_TYPE;
                if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                    ListNode_t *section_cursor = nested_type_sections;
                    while (section_cursor != NULL) {
                        ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                        if (type_section_ast != NULL) {
                            append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                                NULL, NULL, NULL, id);
                            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                                fprintf(stderr, "[KGPC] convert_type_decl: processed nested TYPE_SECTION for record %s\n", id);
                        }
                        section_cursor = section_cursor->next;
                    }
                }
                while (nested_type_sections != NULL) {
                    ListNode_t *next = nested_type_sections->next;
                    free(nested_type_sections);
                    nested_type_sections = next;
                }
            } else {
        mapped_type = convert_type_spec(spec_node, &type_id, &record_type, &type_info);
        /* Preserve qualified type names for alias targets like ObjPas.TEndian. */
        {
            ast_t *spec_child = spec_node;
            if (spec_child != NULL && spec_child->typ == PASCAL_T_TYPE_SPEC && spec_child->child != NULL)
                spec_child = spec_child->child;
            spec_child = unwrap_pascal_node(spec_child);
            if (spec_child != NULL &&
                (spec_child->typ == PASCAL_T_IDENTIFIER ||
                 spec_child->typ == PASCAL_T_QUALIFIED_IDENTIFIER ||
                 spec_child->typ == PASCAL_T_MEMBER_ACCESS))
            {
                QualifiedIdent *qid = qualified_ident_from_ast(spec_child);
                if (qid != NULL)
                {
                    if (type_info.type_ref == NULL)
                        type_info.type_ref = type_ref_create(qualified_ident_clone(qid), NULL, 0);
                    if (qid->count > 1)
                    {
                        char *qualified = qualified_ident_join(qid, ".");
                        if (qualified != NULL)
                        {
                            free(type_id);
                            type_id = qualified;
                        }
                    }
                    qualified_ident_free(qid);
                }
            }
        }
            }
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL)
                fprintf(stderr, "[KGPC] convert_type_decl after convert_type_spec id=%s mapped=%d type_id=%s record_type=%p type_info.record=%p\n",
                    id, mapped_type,
                    type_id != NULL ? type_id : "<null>",
                    (void *)record_type, (void *)type_info.record_type);
        }
    }

    KgpcType *kgpc_type = NULL;
    if (record_type != NULL) {
        KgpcType *rec_type = create_record_type(record_type);
        if (record_type->is_class || record_type->is_interface) {
            /* Classes and interfaces are pointers to records */
            kgpc_type = create_pointer_type(rec_type);
            kgpc_type_release(rec_type);
        } else {
            kgpc_type = rec_type;
        }
    } else if (spec_node != NULL) {
        kgpc_type = convert_type_spec_to_kgpctype(spec_node, NULL);
    }

    Tree_t *decl = NULL;
    if (record_type != NULL) {
        /* Set the type ID for the record */
        if (record_type->type_id == NULL && id != NULL)
            record_type->type_id = strdup(id);

        if (record_type->is_type_helper && record_type->helper_base_type_id != NULL &&
            record_type->type_id != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
                fprintf(stderr, "[KGPC] Registering type helper mapping: %s -> %s\n",
                    record_type->type_id, record_type->helper_base_type_id);
            register_type_helper_mapping(record_type->type_id, record_type->helper_base_type_id);
        }
        
        /* For advanced records, register any method declarations */
        /* Walk the fields list looking for stored method AST nodes */
        ListNode_t *field_cur = record_type->fields;
        while (field_cur != NULL) {
            if (field_cur->type == LIST_UNSPECIFIED) {
                /* This is a method AST node stored during convert_record_members */
                ast_t *method_ast = (ast_t *)field_cur->cur;
                if (method_ast != NULL && (method_ast->typ == PASCAL_T_METHOD_DECL ||
                    method_ast->typ == PASCAL_T_CONSTRUCTOR_DECL ||
                    method_ast->typ == PASCAL_T_DESTRUCTOR_DECL)) {
                    struct MethodTemplate *template = create_method_template(method_ast);
                    if (template != NULL) {
                        if (!template->is_interface_delegation) {
                            int param_count = from_cparser_count_params_ast(template->params_ast);
                            char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                            register_class_method_ex(id, template->name,
                                template->is_virtual, template->is_override, template->is_static,
                                template->is_class_method,
                                param_count, param_sig);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL)
                            fprintf(stderr, "[KGPC] Registered record method %s.%s (static=%d)\n",
                                id, template->name, template->is_static);
                        destroy_method_template_instance(template);
                    }
                }
            }
            field_cur = field_cur->next;
        }

        if (record_type->is_type_helper && record_type->method_templates != NULL && id != NULL)
        {
            ListNode_t *tmpl_cur = record_type->method_templates;
            while (tmpl_cur != NULL)
            {
                if (tmpl_cur->type == LIST_METHOD_TEMPLATE)
                {
                    struct MethodTemplate *template = (struct MethodTemplate *)tmpl_cur->cur;
                    if (template != NULL)
                    {
                        if (template->is_class_method)
                            template->is_static = 1;
                        if (!template->is_interface_delegation) {
                            int param_count = from_cparser_count_params_ast(template->params_ast);
                            char *param_sig = param_type_signature_from_params_ast(template->params_ast);
                            register_class_method_ex(id, template->name,
                                template->is_virtual, template->is_override, template->is_static,
                                template->is_class_method,
                                param_count, param_sig);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_CLASS_METHODS") != NULL)
                            fprintf(stderr, "[KGPC] Registered helper method %s.%s (static=%d)\n",
                                id, template->name, template->is_static);
                    }
                }
                tmpl_cur = tmpl_cur->next;
            }
        }
        
        /* Direct record/class type declaration */
        decl = mk_record_type(type_decl_node->line, id, record_type);
    } else if (type_info.is_array) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 1, type_info.element_type,
                                 type_info.element_type_id, type_info.start, type_info.end);
        if (decl != NULL) {
            decl->tree_data.type_decl_data.info.alias.is_shortstring =
                type_info.is_shortstring ||
                (id != NULL && pascal_identifier_equals(id, "ShortString"));
            if (type_info.element_kgpc_type != NULL) {
                long long esize = kgpc_type_sizeof(type_info.element_kgpc_type);
                if (esize > 0 && esize <= INT_MAX)
                    decl->tree_data.type_decl_data.info.alias.array_element_storage_size = (int)esize;
            }
        }
        type_info.element_type_id = NULL;
    } else if (type_info.is_record && type_id != NULL) {
        /* Alias to a record type (including generic specializations) */
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, RECORD_TYPE, type_id, 0, 0);
        type_id = NULL;
    } else if (mapped_type != UNKNOWN_TYPE || type_id != NULL) {
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, mapped_type, type_id, 0, 0);
        type_id = NULL;
    } else if (type_info.is_range) {
        int base_tag = select_range_primitive_tag(&type_info);
        decl = mk_typealiasdecl(type_decl_node->line, id, 0, base_tag, NULL, 0, 0);
    } else {
        decl = mk_typedecl(type_decl_node->line, id, 0, 0);
    }

    if (decl != NULL)
    {
        const char *trace_sym = kgpc_getenv("KGPC_TRACE_NONLOCAL");
        if (trace_sym != NULL && id != NULL && pascal_identifier_equals(id, trace_sym) &&
            decl->type == TREE_TYPE_DECL &&
            decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
            fprintf(stderr,
                "[KGPC_TRACE_NONLOCAL] parse_type_decl id=%s base=%d target=%s is_array=%d [%d,%d] short=%d kgpc=%p\n",
                id,
                alias->base_type,
                alias->target_type_id != NULL ? alias->target_type_id : "<null>",
                alias->is_array,
                alias->array_start,
                alias->array_end,
                alias->is_shortstring,
                (void *)decl->tree_data.type_decl_data.kgpc_type);
        }
        decl->tree_data.type_decl_data.kgpc_type = kgpc_type;
        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL &&
            decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
            decl->tree_data.type_decl_data.id != NULL)
        {
            fprintf(stderr, "[KGPC] convert_type_decl alias %s: base=%d target=%s kgpc=%p kind=%d type_info.is_record=%d record=%p generic=%d\n",
                decl->tree_data.type_decl_data.id,
                decl->tree_data.type_decl_data.info.alias.base_type,
                decl->tree_data.type_decl_data.info.alias.target_type_id ?
                    decl->tree_data.type_decl_data.info.alias.target_type_id : "<null>",
                (void *)decl->tree_data.type_decl_data.kgpc_type,
                decl->tree_data.type_decl_data.kgpc_type ?
                    decl->tree_data.type_decl_data.kgpc_type->kind : -1,
                type_info.is_record,
                (void *)type_info.record_type,
                type_info.is_generic_specialization);
        }
    }
    else if (kgpc_type != NULL)
        destroy_kgpc_type(kgpc_type);

    if (decl != NULL && decl->type == TREE_TYPE_DECL &&
        decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
        /* Set the alias name */
        if (decl->tree_data.type_decl_data.id != NULL && alias->alias_name == NULL) {
            alias->alias_name = strdup(decl->tree_data.type_decl_data.id);
        }
        if (type_info.array_dimensions != NULL) {
            alias->array_dimensions = type_info.array_dimensions;
            type_info.array_dimensions = NULL;
        }
        if (type_info.array_dims_parsed) {
            alias->array_dim_start_str = type_info.array_dim_start_str;
            alias->array_dim_end_str = type_info.array_dim_end_str;
            alias->array_dims_parsed = 1;
            type_info.array_dim_start_str = NULL;
            type_info.array_dim_end_str = NULL;
        }
        if (type_info.type_ref != NULL) {
            type_ref_free(alias->target_type_ref);
            alias->target_type_ref = type_info.type_ref;
            type_info.type_ref = NULL;
        }
        if (type_info.range_start_str != NULL) {
            free(alias->range_start_str);
            alias->range_start_str = type_info.range_start_str;
            type_info.range_start_str = NULL;
        }
        if (type_info.range_end_str != NULL) {
            free(alias->range_end_str);
            alias->range_end_str = type_info.range_end_str;
            type_info.range_end_str = NULL;
        }
        if (type_info.element_type_ref != NULL) {
            type_ref_free(alias->array_element_type_ref);
            alias->array_element_type_ref = type_info.element_type_ref;
            type_info.element_type_ref = NULL;
        }
        alias->is_pointer = type_info.is_pointer;
        alias->is_class_reference = type_info.is_class_reference;
        alias->pointer_type = type_info.pointer_type;
        if (type_info.pointer_type_id != NULL) {
            alias->pointer_type_id = type_info.pointer_type_id;
            type_info.pointer_type_id = NULL;
        }
        if (type_info.pointer_type_ref != NULL) {
            type_ref_free(alias->pointer_type_ref);
            alias->pointer_type_ref = type_info.pointer_type_ref;
            type_info.pointer_type_ref = NULL;
        }
        alias->is_set = type_info.is_set;
        alias->set_element_type = type_info.set_element_type;
        if (type_info.set_element_type_id != NULL) {
            alias->set_element_type_id = type_info.set_element_type_id;
            type_info.set_element_type_id = NULL;
        }
        if (type_info.set_element_type_ref != NULL) {
            type_ref_free(alias->set_element_type_ref);
            alias->set_element_type_ref = type_info.set_element_type_ref;
            type_info.set_element_type_ref = NULL;
        }
        if (type_info.is_set && type_info.range_known) {
            alias->range_known = 1;
            alias->range_start = type_info.range_start;
            alias->range_end = type_info.range_end;
        }
        alias->is_enum_set = type_info.is_enum_set;
        if (type_info.inline_enum_values != NULL) {
            alias->inline_enum_values = type_info.inline_enum_values;
            type_info.inline_enum_values = NULL;
        }
        alias->is_enum = type_info.is_enum;
        alias->enum_is_scoped = type_info.enum_is_scoped;
        alias->enum_has_explicit_values = type_info.enum_has_explicit_values;
        if (type_info.enum_literals != NULL) {
            alias->enum_literals = type_info.enum_literals;
            type_info.enum_literals = NULL;
        }
        alias->is_file = type_info.is_file;
        alias->file_type = type_info.file_type;
        if (type_info.file_type_id != NULL) {
            alias->file_type_id = type_info.file_type_id;
            type_info.file_type_id = NULL;
        }
        if (type_info.file_type_ref != NULL) {
            type_ref_free(alias->file_type_ref);
            alias->file_type_ref = type_info.file_type_ref;
            type_info.file_type_ref = NULL;
        }
        if (type_info.is_record && type_info.record_type != NULL) {
            alias->inline_record_type = type_info.record_type;
            type_info.record_type = NULL;
            if (alias->inline_record_type->is_class) {
                KgpcType *rec = create_record_type(alias->inline_record_type);
                KgpcType *inline_type = create_pointer_type(rec);
                kgpc_type_release(rec);
                if (decl->tree_data.type_decl_data.kgpc_type != NULL)
                    destroy_kgpc_type(decl->tree_data.type_decl_data.kgpc_type);
                decl->tree_data.type_decl_data.kgpc_type = inline_type;
            } else if (decl->tree_data.type_decl_data.kgpc_type == NULL) {
                decl->tree_data.type_decl_data.kgpc_type =
                    create_record_type(alias->inline_record_type);
            }
        }
        if (type_info.is_range) {
            alias->is_range = 1;
            alias->range_known = type_info.range_known;
            alias->range_start = type_info.range_start;
            alias->range_end = type_info.range_end;
            alias->storage_size = compute_range_storage_size(&type_info);
        }
        if (type_info.is_generic_specialization && type_info.record_type == NULL) {
            register_pending_generic_alias(decl, &type_info);
        }
    }

    if (type_id != NULL)
        free(type_id);
    destroy_type_info_contents(&type_info);

    if (decl == NULL) {
        free(id);
        destroy_record_type(record_type);
    }

    if (decl != NULL && method_clones != NULL)
        append_specialized_method_clones(decl, method_clones);

    return decl;
}

static Tree_t *convert_generic_type_decl(ast_t *type_decl_node) {
    if (type_decl_node == NULL)
        return NULL;

    type_decl_node = unwrap_pascal_node(type_decl_node);
    if (type_decl_node == NULL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    if (id_node == NULL)
        return NULL;

    char *id = dup_symbol(id_node);
    if (id == NULL)
        return NULL;

    ast_t *param_list = id_node->next;
    ast_t *type_spec_node = param_list != NULL ? param_list->next : NULL;
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
        fprintf(stderr, "[KGPC] convert_generic_type_decl %s (type_spec_node=%p typ=%d)\n",
            id, (void *)type_spec_node, type_spec_node != NULL ? type_spec_node->typ : -1);
    struct RecordType *record_template = NULL;

    int param_count = 0;
    char **param_names = NULL;
    if (param_list != NULL && param_list->typ == PASCAL_T_TYPE_PARAM_LIST) {
        /* First pass: count parameters */
        for (ast_t *param = param_list->child; param != NULL; param = param->next) {
            ast_t *name_node = param;
            if (name_node->typ == PASCAL_T_TYPE_PARAM && name_node->child != NULL)
                name_node = name_node->child;
            while (name_node != NULL && name_node->typ != PASCAL_T_IDENTIFIER)
                name_node = name_node->next;
            if (name_node != NULL && name_node->sym != NULL)
                param_count++;
        }

        if (param_count > 0) {
            param_names = (char **)calloc((size_t)param_count, sizeof(char *));
            if (param_names == NULL) {
                free(id);
                return NULL;
            }

            int index = 0;
            for (ast_t *param = param_list->child; param != NULL && index < param_count; param = param->next) {
                ast_t *name_node = param;
                if (name_node->typ == PASCAL_T_TYPE_PARAM && name_node->child != NULL)
                    name_node = name_node->child;
                while (name_node != NULL && name_node->typ != PASCAL_T_IDENTIFIER)
                    name_node = name_node->next;
                if (name_node != NULL && name_node->sym != NULL) {
                    param_names[index] = strdup(name_node->sym->name);
                    if (param_names[index] == NULL) {
                        /* Cleanup previously allocated names */
                        for (int i = 0; i < index; ++i)
                            free(param_names[i]);
                        free(param_names);
                        free(id);
                        return NULL;
                    }
                    index++;
                }
            }
            param_count = index;
        }
    }

    ListNode_t *generic_nested_types = NULL;
    if (type_spec_node != NULL) {
        ast_t *spec_body = type_spec_node;
        if (spec_body->typ == PASCAL_T_TYPE_SPEC && spec_body->child != NULL)
            spec_body = spec_body->child;
        if (spec_body != NULL) {
            if (spec_body->typ == PASCAL_T_CLASS_TYPE)
            {
                ListNode_t *nested_type_sections = NULL;
                if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
                    fprintf(stderr, "[KGPC] generic class decl %s\n", id);
                record_template = convert_class_type_ex(id, spec_body, &nested_type_sections);
                /* Convert nested type sections into type declarations for later specialization */
                if (nested_type_sections != NULL) {
                    ListNode_t *section_cursor = nested_type_sections;
                    while (section_cursor != NULL) {
                        ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                        if (type_section_ast != NULL) {
                            append_type_decls_from_section(type_section_ast, &generic_nested_types,
                                NULL, NULL, NULL, id);
                        }
                        section_cursor = section_cursor->next;
                    }
                    /* Clean up section list (not AST nodes) */
                    while (nested_type_sections != NULL) {
                        ListNode_t *next = nested_type_sections->next;
                        free(nested_type_sections);
                        nested_type_sections = next;
                    }
                }
            }
            else if (spec_body->typ == PASCAL_T_RECORD_TYPE || spec_body->typ == PASCAL_T_OBJECT_TYPE)
                record_template = convert_record_type(spec_body);
        }
    }

    Tree_t *decl = mk_typedecl(type_decl_node->line, id, 0, 0);
    if (decl == NULL) {
        if (param_names != NULL) {
            for (int i = 0; i < param_count; ++i)
                free(param_names[i]);
            free(param_names);
        }
        free(id);
        destroy_record_type(record_template);
        return NULL;
    }

    decl->tree_data.type_decl_data.kind = TYPE_DECL_GENERIC;
    decl->tree_data.type_decl_data.info.generic.type_parameters = param_names;
    decl->tree_data.type_decl_data.info.generic.num_type_params = param_count;
    decl->tree_data.type_decl_data.info.generic.original_ast = NULL;
    if (type_spec_node != NULL)
        decl->tree_data.type_decl_data.info.generic.original_ast = copy_ast_detached(type_spec_node);
    decl->tree_data.type_decl_data.info.generic.record_template = record_template;

    /* Register the generic declaration for future specialization */
    GenericTypeDecl *generic_decl = generic_registry_add_decl(id, param_names, param_count, decl);
    if (generic_decl != NULL && generic_nested_types != NULL) {
        generic_decl->nested_type_decls = generic_nested_types;
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL) {
            int count = 0;
            ListNode_t *cur = generic_nested_types;
            while (cur != NULL) { count++; cur = cur->next; }
            fprintf(stderr, "[KGPC] stored %d nested type decls for generic %s\n", count, id);
        }
    }

    (void)type_spec_node; /* Placeholder for future template storage */

    return decl;
}

static struct Statement *convert_statement(ast_t *stmt_node);
static struct Statement *convert_block(ast_t *block_node);
static Tree_t *convert_procedure(ast_t *proc_node);
static Tree_t *convert_function(ast_t *func_node);
static Tree_t *convert_method_impl(ast_t *method_node);
static struct Statement *convert_method_call_statement(ast_t *member_node, ast_t *args_start);

static void append_labels_from_section(ast_t *label_node, ListBuilder *builder);

static void convert_routine_body(ast_t *body_node, ListNode_t **const_decls,
                                 ListBuilder *var_builder,
                                 ListBuilder *label_builder,
                                 ListNode_t **nested_subs,
                                 struct Statement **body_out,
                                 ListNode_t **type_decl_list) {
    if (body_node == NULL)
        return;

    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    
    ast_t *cursor = body_node->child;
    while (cursor != NULL) {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node != NULL) {
            switch (node->typ) {
            case PASCAL_T_TYPE_SECTION:
                if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                    fprintf(stderr, "[KGPC] convert_routine_body TYPE_SECTION at line=%d\n", node->line);
                }
                if (type_decl_list != NULL)
                    append_type_decls_from_section(node, type_decl_list, nested_subs,
                        const_decls, var_builder, NULL);
                type_section_ast = node;  /* Save for const array enum resolution */
                break;
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(node, const_decls, var_builder, type_section_ast);
                break;
            case PASCAL_T_VAR_SECTION:
                list_builder_extend(var_builder, convert_var_section(node));
                break;
            case PASCAL_T_LABEL_SECTION:
                append_labels_from_section(node, label_builder);
                break;
            case PASCAL_T_PROCEDURE_DECL: {
                if (nested_subs != NULL) {
                    Tree_t *proc = convert_procedure(node);
                    append_subprogram_node(nested_subs, proc);
                }
                break;
            }
            case PASCAL_T_FUNCTION_DECL: {
                if (nested_subs != NULL) {
                    Tree_t *func = convert_function(node);
                    append_subprogram_node(nested_subs, func);
                }
                break;
            }
            case PASCAL_T_METHOD_IMPL: {
                if (nested_subs != NULL) {
                    Tree_t *method_tree = convert_method_impl(node);
                    append_subprogram_node(nested_subs, method_tree);
                }
                break;
            }
            case PASCAL_T_BEGIN_BLOCK:
            case PASCAL_T_MAIN_BLOCK:
                if (body_out != NULL)
                    *body_out = convert_block(node);
                break;
            case PASCAL_T_ASM_BLOCK: {
                if (body_out != NULL) {
                    struct Statement *stmt = convert_statement(node);
                    ListBuilder stmts_builder;
                    list_builder_init(&stmts_builder);
                    if (stmt != NULL)
                        list_builder_append(&stmts_builder, stmt, LIST_STMT);
                    *body_out = mk_compoundstatement(node->line, list_builder_finish(&stmts_builder));
                }
                break;
            }
            default:
                break;
            }
        }
        cursor = cursor->next;
    }
}

static void append_uses_from_section(ast_t *uses_node, ListNode_t **dest) {
    if (uses_node == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for uses traversal\n");
        return;
    }

    ast_t *unit = uses_node->child;
    while (unit != NULL) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, unit)) {
            fprintf(stderr, "ERROR: Circular reference detected in uses section, stopping traversal\n");
            break;
        }
        
        if (unit->typ == PASCAL_T_USES_UNIT) {
            char *dup = dup_symbol(unit);
            if (dup != NULL) {
                ListNode_t *node = CreateListNode(dup, LIST_STRING);
                *tail = node;
                tail = &node->next;
            }
        }
        unit = unit->next;
    }
    
    visited_set_destroy(visited);
}

static void append_labels_from_section(ast_t *label_node, ListBuilder *builder) {
    if (label_node == NULL || builder == NULL)
        return;

    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for labels traversal\n");
        return;
    }

    ast_t *cur = label_node->child;
    while (cur != NULL) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in label section, stopping traversal\n");
            break;
        }
        
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;

        if (node != NULL && (node->typ == PASCAL_T_INTEGER || node->typ == PASCAL_T_IDENTIFIER)) {
            char *label = dup_symbol(node);
            if (label != NULL)
                list_builder_append(builder, label, LIST_STRING);
        }
        cur = cur->next;
    }
    
    visited_set_destroy(visited);
}

static char *mangle_helper_const_name(const char *helper_id, const char *const_id)
{
    if (helper_id == NULL || const_id == NULL)
        return NULL;
    size_t len = strlen(helper_id) + strlen(const_id) + 3;
    char *name = (char *)malloc(len);
    if (name == NULL)
        return NULL;
    snprintf(name, len, "%s__%s", helper_id, const_id);
    return name;
}

typedef struct {
    char *original;
    char *mangled;
} ClassConstMap;

static const char *lookup_class_const_map(ListNode_t *map, const char *name)
{
    if (map == NULL || name == NULL)
        return NULL;
    for (ListNode_t *cur = map; cur != NULL; cur = cur->next) {
        ClassConstMap *entry = (ClassConstMap *)cur->cur;
        if (entry != NULL && entry->original != NULL &&
            pascal_identifier_equals(entry->original, name)) {
            return entry->mangled;
        }
    }
    return NULL;
}

static int type_name_exists_in_sections(const char *name, ListNode_t *sections)
{
    if (name == NULL || sections == NULL)
        return 0;
    for (ListNode_t *cur = sections; cur != NULL; cur = cur->next)
    {
        if (cur->cur == NULL)
            continue;
        ast_t *section = (ast_t *)cur->cur;
        if (type_name_exists_in_section(name, section))
            return 1;
    }
    return 0;
}

static void qualify_expr_type_id(struct Expression *expr, const char *owner_id)
{
    if (expr == NULL || owner_id == NULL)
        return;
    if (expr->expr_data.id == NULL)
        return;
    if (expr->id_ref != NULL && expr->id_ref->count > 1)
        return;

    char *base_name = strdup(expr->expr_data.id);
    if (base_name == NULL)
        return;
    size_t len = strlen(owner_id) + 1 + strlen(base_name) + 1;
    char *qualified = (char *)malloc(len);
    if (qualified == NULL)
    {
        free(base_name);
        return;
    }
    snprintf(qualified, len, "%s.%s", owner_id, base_name);
    free(expr->expr_data.id);
    expr->expr_data.id = qualified;

    if (expr->id_ref != NULL)
        qualified_ident_free(expr->id_ref);
    char **segments = (char **)calloc(2, sizeof(char *));
    if (segments == NULL)
    {
        free(base_name);
        return;
    }
    segments[0] = strdup(owner_id);
    segments[1] = base_name;
    if (segments[0] == NULL || segments[1] == NULL)
    {
        free(segments[0]);
        free(segments[1]);
        free(segments);
        return;
    }
    expr->id_ref = qualified_ident_from_segments(segments, 2, 1);
}

static void qualify_type_ref_id(char **type_id, struct TypeRef **type_ref,
    const char *owner_id, const char *base_name)
{
    if (type_id == NULL || owner_id == NULL || base_name == NULL)
        return;
    char *base_dup = strdup(base_name);
    if (base_dup == NULL)
        return;
    size_t len = strlen(owner_id) + 1 + strlen(base_name) + 1;
    char *qualified = (char *)malloc(len);
    if (qualified == NULL)
    {
        free(base_dup);
        return;
    }
    snprintf(qualified, len, "%s.%s", owner_id, base_name);
    free(*type_id);
    *type_id = qualified;
    if (type_ref != NULL)
    {
        if (*type_ref != NULL)
            type_ref_free(*type_ref);
        char **segments = (char **)calloc(2, sizeof(char *));
        if (segments == NULL)
        {
            free(base_dup);
            return;
        }
        segments[0] = strdup(owner_id);
        segments[1] = base_dup;
        if (segments[0] == NULL || segments[1] == NULL)
        {
            free(segments[0]);
            free(segments[1]);
            free(segments);
            return;
        }
        QualifiedIdent *qid = qualified_ident_from_segments(segments, 2, 1);
        *type_ref = type_ref_create(qid, NULL, 0);
    }
    else
    {
        free(base_dup);
    }
}

static int type_id_is_qualified(const char *type_id, const struct TypeRef *type_ref)
{
    (void)type_id;
    if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count > 1)
        return 1;
    return 0;
}

static struct TypeRef *ensure_type_ref_from_id(char **type_id, struct TypeRef **type_ref);

static void qualify_param_type_id(char **type_id, struct TypeRef **type_ref,
    const char *owner_full, const char *owner_outer, SymTab_t *symtab)
{
    if (type_id == NULL || *type_id == NULL || symtab == NULL)
        return;
    if (type_ref != NULL && *type_ref == NULL)
        ensure_type_ref_from_id(type_id, type_ref);
    if (type_id_is_qualified(*type_id, type_ref != NULL ? *type_ref : NULL))
        return;
    if (map_type_name(*type_id, NULL) != UNKNOWN_TYPE)
        return;

    const char *owners[2] = { owner_full, owner_outer };
    for (size_t i = 0; i < 2; ++i)
    {
        const char *owner = owners[i];
        if (owner == NULL)
            continue;
        size_t len = strlen(owner) + 1 + strlen(*type_id) + 1;
        char *qualified = (char *)malloc(len);
        if (qualified == NULL)
            continue;
        snprintf(qualified, len, "%s.%s", owner, *type_id);
        HashNode_t *node = NULL;
        if (FindSymbol(&node, symtab, qualified) != 0 &&
            node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            free(qualified);
            qualify_type_ref_id(type_id, type_ref, owner, *type_id);
            break;
        }
        free(qualified);
    }
}

static void qualify_param_decl_types(ListNode_t *params,
    const char *owner_full, const char *owner_outer, SymTab_t *symtab)
{
    if (params == NULL || symtab == NULL)
        return;
    for (ListNode_t *cur = params; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type == TREE_VAR_DECL)
        {
            qualify_param_type_id(&decl->tree_data.var_decl_data.type_id,
                &decl->tree_data.var_decl_data.type_ref,
                owner_full, owner_outer, symtab);
        }
        else if (decl->type == TREE_ARR_DECL)
        {
            qualify_param_type_id(&decl->tree_data.arr_decl_data.type_id,
                &decl->tree_data.arr_decl_data.type_ref,
                owner_full, owner_outer, symtab);
        }
    }
}

static void rewrite_class_const_expr_nested_types(struct Expression *expr,
    const char *owner_id, ListNode_t *nested_type_sections)
{
    if (expr == NULL || owner_id == NULL || nested_type_sections == NULL)
        return;

    switch (expr->type) {
        case EXPR_FUNCTION_CALL: {
            const char *func_id = expr->expr_data.function_call_data.id;
            /* A function call whose id matches a nested type is really a typecast
               (e.g. Node(nil^) where Node is a nested record type).  Qualify
               the id so the semantic checker can resolve it. */
            if (func_id != NULL &&
                type_name_exists_in_sections(func_id, nested_type_sections))
            {
                size_t olen = strlen(owner_id);
                size_t flen = strlen(func_id);
                char *qualified = (char *)malloc(olen + 1 + flen + 1);
                if (qualified != NULL)
                {
                    snprintf(qualified, olen + 1 + flen + 1, "%s.%s", owner_id, func_id);
                    free(expr->expr_data.function_call_data.id);
                    expr->expr_data.function_call_data.id = qualified;
                    func_id = qualified;
                }
            }
            int is_type_intrinsic = func_id != NULL &&
                (pascal_identifier_equals(func_id, "SizeOf") ||
                 pascal_identifier_equals(func_id, "TypeInfo") ||
                 pascal_identifier_equals(func_id, "IsManagedType") ||
                 pascal_identifier_equals(func_id, "High") ||
                 pascal_identifier_equals(func_id, "Low"));
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            for (ListNode_t *cur = args; cur != NULL; cur = cur->next) {
                if (cur->type != LIST_EXPR)
                    continue;
                struct Expression *arg = (struct Expression *)cur->cur;
                if (is_type_intrinsic && arg != NULL && arg->type == EXPR_VAR_ID &&
                    arg->expr_data.id != NULL &&
                    type_name_exists_in_sections(arg->expr_data.id, nested_type_sections))
                {
                    qualify_expr_type_id(arg, owner_id);
                }
                rewrite_class_const_expr_nested_types(arg, owner_id, nested_type_sections);
            }
            break;
        }
        case EXPR_TYPEINFO:
            if (expr->expr_data.typeinfo_data.type_id != NULL &&
                type_name_exists_in_sections(expr->expr_data.typeinfo_data.type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.typeinfo_data.type_id,
                    &expr->expr_data.typeinfo_data.type_ref,
                    owner_id, expr->expr_data.typeinfo_data.type_id);
            }
            break;
        case EXPR_TYPECAST:
            rewrite_class_const_expr_nested_types(expr->expr_data.typecast_data.expr,
                owner_id, nested_type_sections);
            if (expr->expr_data.typecast_data.target_type_id != NULL &&
                expr->expr_data.typecast_data.type_qualifier == NULL &&
                type_name_exists_in_sections(expr->expr_data.typecast_data.target_type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.typecast_data.target_type_id,
                    &expr->expr_data.typecast_data.target_type_ref,
                    owner_id, expr->expr_data.typecast_data.target_type_id);
            }
            break;
        case EXPR_IS:
            rewrite_class_const_expr_nested_types(expr->expr_data.is_data.expr,
                owner_id, nested_type_sections);
            if (expr->expr_data.is_data.target_type_id != NULL &&
                type_name_exists_in_sections(expr->expr_data.is_data.target_type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.is_data.target_type_id,
                    &expr->expr_data.is_data.target_type_ref,
                    owner_id, expr->expr_data.is_data.target_type_id);
            }
            break;
        case EXPR_AS:
            rewrite_class_const_expr_nested_types(expr->expr_data.as_data.expr,
                owner_id, nested_type_sections);
            if (expr->expr_data.as_data.target_type_id != NULL &&
                type_name_exists_in_sections(expr->expr_data.as_data.target_type_id,
                    nested_type_sections))
            {
                qualify_type_ref_id(&expr->expr_data.as_data.target_type_id,
                    &expr->expr_data.as_data.target_type_ref,
                    owner_id, expr->expr_data.as_data.target_type_id);
            }
            break;
        case EXPR_ADDOP:
            rewrite_class_const_expr_nested_types(expr->expr_data.addop_data.left_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.addop_data.right_term,
                owner_id, nested_type_sections);
            break;
        case EXPR_MULOP:
            rewrite_class_const_expr_nested_types(expr->expr_data.mulop_data.left_term,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.mulop_data.right_factor,
                owner_id, nested_type_sections);
            break;
        case EXPR_RELOP:
            rewrite_class_const_expr_nested_types(expr->expr_data.relop_data.left,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.relop_data.right,
                owner_id, nested_type_sections);
            break;
        case EXPR_SIGN_TERM:
            rewrite_class_const_expr_nested_types(expr->expr_data.sign_term,
                owner_id, nested_type_sections);
            break;
        case EXPR_ARRAY_ACCESS:
            rewrite_class_const_expr_nested_types(expr->expr_data.array_access_data.array_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(expr->expr_data.array_access_data.index_expr,
                owner_id, nested_type_sections);
            for (ListNode_t *cur = expr->expr_data.array_access_data.extra_indices;
                 cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr_nested_types((struct Expression *)cur->cur,
                        owner_id, nested_type_sections);
            }
            break;
        case EXPR_RECORD_ACCESS:
            rewrite_class_const_expr_nested_types(expr->expr_data.record_access_data.record_expr,
                owner_id, nested_type_sections);
            break;
        case EXPR_SET:
            for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_SET_ELEMENT && cur->cur != NULL) {
                    struct SetElement *elem = (struct SetElement *)cur->cur;
                    rewrite_class_const_expr_nested_types(elem->lower, owner_id, nested_type_sections);
                    rewrite_class_const_expr_nested_types(elem->upper, owner_id, nested_type_sections);
                }
            }
            break;
        case EXPR_ARRAY_LITERAL:
            for (ListNode_t *cur = expr->expr_data.array_literal_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr_nested_types((struct Expression *)cur->cur,
                        owner_id, nested_type_sections);
            }
            break;
        case EXPR_RECORD_CONSTRUCTOR:
            for (ListNode_t *cur = expr->expr_data.record_constructor_data.fields; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_UNSPECIFIED && cur->cur != NULL) {
                    struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
                    rewrite_class_const_expr_nested_types(field->value, owner_id, nested_type_sections);
                }
            }
            break;
        case EXPR_ADDR:
            rewrite_class_const_expr_nested_types(expr->expr_data.addr_data.expr,
                owner_id, nested_type_sections);
            break;
        case EXPR_POINTER_DEREF:
            rewrite_class_const_expr_nested_types(expr->expr_data.pointer_deref_data.pointer_expr,
                owner_id, nested_type_sections);
            break;
        default:
            break;
    }
}

static void rewrite_class_const_statement_nested_types(struct Statement *stmt,
    const char *owner_id, ListNode_t *nested_type_sections)
{
    if (stmt == NULL || owner_id == NULL || nested_type_sections == NULL)
        return;

    switch (stmt->type) {
        case STMT_VAR_ASSIGN:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.var_assign_data.var,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(stmt->stmt_data.var_assign_data.expr,
                owner_id, nested_type_sections);
            break;
        case STMT_COMPOUND_STATEMENT:
            for (ListNode_t *cur = stmt->stmt_data.compound_statement; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement_nested_types((struct Statement *)cur->cur,
                        owner_id, nested_type_sections);
            }
            break;
        case STMT_IF_THEN:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.if_then_data.relop_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.if_then_data.if_stmt,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.if_then_data.else_stmt,
                owner_id, nested_type_sections);
            break;
        case STMT_WHILE:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.while_data.relop_expr,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.while_data.while_stmt,
                owner_id, nested_type_sections);
            break;
        case STMT_REPEAT:
            for (ListNode_t *cur = stmt->stmt_data.repeat_data.body_list; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement_nested_types((struct Statement *)cur->cur,
                        owner_id, nested_type_sections);
            }
            rewrite_class_const_expr_nested_types(stmt->stmt_data.repeat_data.until_expr,
                owner_id, nested_type_sections);
            break;
        case STMT_FOR:
            if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
                rewrite_class_const_statement_nested_types(stmt->stmt_data.for_data.for_assign_data.var_assign,
                    owner_id, nested_type_sections);
            else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR)
                rewrite_class_const_expr_nested_types(stmt->stmt_data.for_data.for_assign_data.var,
                    owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(stmt->stmt_data.for_data.to,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.for_data.do_for,
                owner_id, nested_type_sections);
            break;
        case STMT_FOR_IN:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.for_in_data.loop_var,
                owner_id, nested_type_sections);
            rewrite_class_const_expr_nested_types(stmt->stmt_data.for_in_data.collection,
                owner_id, nested_type_sections);
            rewrite_class_const_statement_nested_types(stmt->stmt_data.for_in_data.do_stmt,
                owner_id, nested_type_sections);
            break;
        case STMT_EXPR:
            rewrite_class_const_expr_nested_types(stmt->stmt_data.expr_stmt_data.expr,
                owner_id, nested_type_sections);
            break;
        default:
            break;
    }
}

static void rewrite_class_const_expr(struct Expression *expr, ListNode_t *map)
{
    if (expr == NULL || map == NULL)
        return;

    switch (expr->type) {
        case EXPR_VAR_ID: {
            const char *mangled = lookup_class_const_map(map, expr->expr_data.id);
            if (mangled != NULL) {
                free(expr->expr_data.id);
                expr->expr_data.id = strdup(mangled);
            }
            break;
        }
        case EXPR_ADDOP:
            rewrite_class_const_expr(expr->expr_data.addop_data.left_expr, map);
            rewrite_class_const_expr(expr->expr_data.addop_data.right_term, map);
            break;
        case EXPR_MULOP:
            rewrite_class_const_expr(expr->expr_data.mulop_data.left_term, map);
            rewrite_class_const_expr(expr->expr_data.mulop_data.right_factor, map);
            break;
        case EXPR_RELOP:
            rewrite_class_const_expr(expr->expr_data.relop_data.left, map);
            rewrite_class_const_expr(expr->expr_data.relop_data.right, map);
            break;
        case EXPR_SIGN_TERM:
            rewrite_class_const_expr(expr->expr_data.sign_term, map);
            break;
        case EXPR_FUNCTION_CALL: {
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            for (ListNode_t *cur = args; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr((struct Expression *)cur->cur, map);
            }
            break;
        }
        case EXPR_ARRAY_ACCESS: {
            rewrite_class_const_expr(expr->expr_data.array_access_data.array_expr, map);
            rewrite_class_const_expr(expr->expr_data.array_access_data.index_expr, map);
            for (ListNode_t *cur = expr->expr_data.array_access_data.extra_indices;
                 cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr((struct Expression *)cur->cur, map);
            }
            break;
        }
        case EXPR_RECORD_ACCESS:
            rewrite_class_const_expr(expr->expr_data.record_access_data.record_expr, map);
            break;
        case EXPR_TYPECAST:
            rewrite_class_const_expr(expr->expr_data.typecast_data.expr, map);
            break;
        case EXPR_SET:
            for (ListNode_t *cur = expr->expr_data.set_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_SET_ELEMENT && cur->cur != NULL) {
                    struct SetElement *elem = (struct SetElement *)cur->cur;
                    rewrite_class_const_expr(elem->lower, map);
                    rewrite_class_const_expr(elem->upper, map);
                }
            }
            break;
        case EXPR_ARRAY_LITERAL:
            for (ListNode_t *cur = expr->expr_data.array_literal_data.elements; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_EXPR)
                    rewrite_class_const_expr((struct Expression *)cur->cur, map);
            }
            break;
        case EXPR_RECORD_CONSTRUCTOR:
            for (ListNode_t *cur = expr->expr_data.record_constructor_data.fields; cur != NULL; cur = cur->next) {
                if (cur->type == LIST_UNSPECIFIED && cur->cur != NULL) {
                    struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
                    rewrite_class_const_expr(field->value, map);
                }
            }
            break;
        case EXPR_ADDR:
            rewrite_class_const_expr(expr->expr_data.addr_data.expr, map);
            break;
        case EXPR_POINTER_DEREF:
            rewrite_class_const_expr(expr->expr_data.pointer_deref_data.pointer_expr, map);
            break;
        case EXPR_IS:
            rewrite_class_const_expr(expr->expr_data.is_data.expr, map);
            break;
        case EXPR_AS:
            rewrite_class_const_expr(expr->expr_data.as_data.expr, map);
            break;
        default:
            break;
    }
}

static void rewrite_class_const_statement(struct Statement *stmt, ListNode_t *map)
{
    if (stmt == NULL || map == NULL)
        return;

    switch (stmt->type) {
        case STMT_VAR_ASSIGN:
            rewrite_class_const_expr(stmt->stmt_data.var_assign_data.var, map);
            rewrite_class_const_expr(stmt->stmt_data.var_assign_data.expr, map);
            break;
        case STMT_COMPOUND_STATEMENT:
            for (ListNode_t *cur = stmt->stmt_data.compound_statement; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement((struct Statement *)cur->cur, map);
            }
            break;
        case STMT_IF_THEN:
            rewrite_class_const_expr(stmt->stmt_data.if_then_data.relop_expr, map);
            rewrite_class_const_statement(stmt->stmt_data.if_then_data.if_stmt, map);
            rewrite_class_const_statement(stmt->stmt_data.if_then_data.else_stmt, map);
            break;
        case STMT_WHILE:
            rewrite_class_const_expr(stmt->stmt_data.while_data.relop_expr, map);
            rewrite_class_const_statement(stmt->stmt_data.while_data.while_stmt, map);
            break;
        case STMT_REPEAT:
            for (ListNode_t *cur = stmt->stmt_data.repeat_data.body_list; cur != NULL; cur = cur->next)
            {
                if (cur->type == LIST_STMT)
                    rewrite_class_const_statement((struct Statement *)cur->cur, map);
            }
            rewrite_class_const_expr(stmt->stmt_data.repeat_data.until_expr, map);
            break;
        case STMT_FOR:
            if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR)
                rewrite_class_const_statement(stmt->stmt_data.for_data.for_assign_data.var_assign, map);
            else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR)
                rewrite_class_const_expr(stmt->stmt_data.for_data.for_assign_data.var, map);
            rewrite_class_const_expr(stmt->stmt_data.for_data.to, map);
            rewrite_class_const_statement(stmt->stmt_data.for_data.do_for, map);
            break;
        case STMT_FOR_IN:
            rewrite_class_const_expr(stmt->stmt_data.for_in_data.loop_var, map);
            rewrite_class_const_expr(stmt->stmt_data.for_in_data.collection, map);
            rewrite_class_const_statement(stmt->stmt_data.for_in_data.do_stmt, map);
            break;
        case STMT_EXPR:
            rewrite_class_const_expr(stmt->stmt_data.expr_stmt_data.expr, map);
            break;
        default:
            break;
    }
}

static void add_class_const_map_entry(ListBuilder *builder, const char *original,
                                      const char *mangled)
{
    if (builder == NULL || original == NULL || mangled == NULL)
        return;
    ClassConstMap *entry = (ClassConstMap *)calloc(1, sizeof(ClassConstMap));
    if (entry == NULL)
        return;
    entry->original = strdup(original);
    entry->mangled = strdup(mangled);
    list_builder_append(builder, entry, LIST_UNSPECIFIED);
}

static ast_t *find_class_spec(ast_t *type_decl_node)
{
    if (type_decl_node == NULL || type_decl_node->typ != PASCAL_T_TYPE_DECL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
        id_node = id_node->next;
    if (id_node == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL &&
           spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_CLASS_TYPE &&
           spec_node->typ != PASCAL_T_OBJECT_TYPE &&
           spec_node->typ != PASCAL_T_RECORD_TYPE)
    {
        spec_node = spec_node->next;
    }
    if (spec_node == NULL)
        return NULL;

    if (spec_node->typ == PASCAL_T_CLASS_TYPE || spec_node->typ == PASCAL_T_OBJECT_TYPE ||
        spec_node->typ == PASCAL_T_RECORD_TYPE)
        return spec_node;
    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL &&
        (spec_node->child->typ == PASCAL_T_CLASS_TYPE || spec_node->child->typ == PASCAL_T_OBJECT_TYPE ||
         spec_node->child->typ == PASCAL_T_RECORD_TYPE))
        return spec_node->child;

    return NULL;
}

static ast_t *find_helper_record_spec(ast_t *type_decl_node)
{
    if (type_decl_node == NULL || type_decl_node->typ != PASCAL_T_TYPE_DECL)
        return NULL;

    ast_t *id_node = type_decl_node->child;
    while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
        id_node = id_node->next;
    if (id_node == NULL)
        return NULL;

    ast_t *spec_node = id_node->next;
    while (spec_node != NULL &&
           spec_node->typ != PASCAL_T_TYPE_SPEC &&
           spec_node->typ != PASCAL_T_RECORD_TYPE &&
           spec_node->typ != PASCAL_T_OBJECT_TYPE)
    {
        spec_node = spec_node->next;
    }

    if (spec_node == NULL)
        return NULL;

    if (spec_node->typ == PASCAL_T_TYPE_SPEC && spec_node->child != NULL)
        spec_node = spec_node->child;

    if (spec_node->sym == NULL || spec_node->sym->name == NULL)
        return NULL;

    if (strcasecmp(spec_node->sym->name, "helper") != 0)
        return NULL;

    return spec_node;
}

static void append_class_const_decls_from_type_decl(ast_t *type_decl_node,
    const char *class_id, ListNode_t **const_decls, ListBuilder *var_builder,
    ast_t *type_section)
{
    if (type_decl_node == NULL || class_id == NULL || const_decls == NULL ||
        var_builder == NULL)
        return;

    ast_t *class_spec = find_class_spec(type_decl_node);
    if (class_spec == NULL)
        return;

    /* Skip if this is a type helper — append_helper_const_decls_from_type_decl
     * already handles const sections for helpers with proper mangling. */
    if (find_helper_record_spec(type_decl_node) != NULL)
        return;

    ListNode_t **tail = const_decls;
    while (*tail != NULL)
        tail = &(*tail)->next;

    ListBuilder nested_type_builder;
    list_builder_init(&nested_type_builder);
    if (class_spec != NULL)
        collect_record_nested_types(class_spec->child, &nested_type_builder);
    ListNode_t *nested_type_sections = list_builder_finish(&nested_type_builder);

    ListBuilder class_map_builder;
    list_builder_init(&class_map_builder);

    for (ast_t *cur = class_spec->child; cur != NULL; cur = cur->next)
    {
        ast_t *node = cur;
        ast_t *unwrapped = unwrap_pascal_node(cur);
        int is_const_section = 0;
        if (node != NULL && node->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        else if (node != NULL && node->typ == PASCAL_T_NONE &&
                 node->sym != NULL && node->sym->name != NULL &&
                 strcasecmp(node->sym->name, "const") == 0)
            is_const_section = 1;
        else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        if (!is_const_section)
            continue;

        if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL) {
            fprintf(stderr, "[KGPC] class const section in %s at line %d\n",
                class_id, node != NULL ? node->line : -1);
        }

        ListNode_t *class_consts = NULL;
        ListNode_t **var_tail_before = var_builder->tail_next;
        append_const_decls_from_section(node, &class_consts, var_builder, type_section);
        ListNode_t *new_var_nodes = (var_tail_before != NULL) ? *var_tail_before : NULL;

        ListNode_t *class_iter = class_consts;
        while (class_iter != NULL)
        {
            Tree_t *decl = (Tree_t *)class_iter->cur;
            if (decl != NULL && decl->type == TREE_CONST_DECL &&
                decl->tree_data.const_decl_data.id != NULL)
            {
                if (kgpc_getenv("KGPC_DEBUG_CLASS_CONST") != NULL) {
                    fprintf(stderr, "[KGPC]   class const %s.%s\n",
                        class_id, decl->tree_data.const_decl_data.id);
                }
                char *mangled = mangle_helper_const_name(class_id,
                    decl->tree_data.const_decl_data.id);
                if (mangled != NULL)
                {
                    add_class_const_map_entry(&class_map_builder,
                        decl->tree_data.const_decl_data.id, mangled);
                    free(mangled);
                }
            }
            class_iter = class_iter->next;
        }

        for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl == NULL)
                continue;
            if (decl->type == TREE_VAR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(class_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&class_map_builder, old_id, mangled);
                            free(mangled);
                        }
                    }
                }
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.arr_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(class_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&class_map_builder, old_id, mangled);
                            free(mangled);
                        }
                    }
                }
            }
        }

        ListNode_t *map_list = list_builder_finish(&class_map_builder);

        ListNode_t *iter = class_consts;
        while (iter != NULL)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl != NULL && decl->type == TREE_CONST_DECL &&
                decl->tree_data.const_decl_data.id != NULL)
            {
                const char *mangled = lookup_class_const_map(map_list,
                    decl->tree_data.const_decl_data.id);
                if (mangled != NULL)
                {
                    free(decl->tree_data.const_decl_data.id);
                    decl->tree_data.const_decl_data.id = strdup(mangled);
                }
            }

            if (decl != NULL && decl->type == TREE_CONST_DECL)
            {
                rewrite_class_const_expr(decl->tree_data.const_decl_data.value, map_list);
                rewrite_class_const_expr_nested_types(decl->tree_data.const_decl_data.value,
                    class_id, nested_type_sections);
            }

            ListNode_t *next = iter->next;
            iter->next = NULL;
            *tail = iter;
            tail = &iter->next;
            iter = next;
        }

        for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl == NULL)
                continue;
            if (decl->type == TREE_VAR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        const char *mangled = lookup_class_const_map(map_list,
                            (char *)id_node->cur);
                        if (mangled != NULL)
                        {
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                        }
                    }
                }
                rewrite_class_const_statement(decl->tree_data.var_decl_data.initializer, map_list);
                rewrite_class_const_statement_nested_types(decl->tree_data.var_decl_data.initializer,
                    class_id, nested_type_sections);
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.arr_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        const char *mangled = lookup_class_const_map(map_list,
                            (char *)id_node->cur);
                        if (mangled != NULL)
                        {
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                        }
                    }
                }
                rewrite_class_const_statement(decl->tree_data.arr_decl_data.initializer, map_list);
                rewrite_class_const_statement_nested_types(decl->tree_data.arr_decl_data.initializer,
                    class_id, nested_type_sections);
            }
        }

    }

    ListNode_t *map_list = list_builder_finish(&class_map_builder);
    for (ListNode_t *cur_map = map_list; cur_map != NULL; cur_map = cur_map->next)
    {
        ClassConstMap *entry = (ClassConstMap *)cur_map->cur;
        if (entry != NULL)
        {
            free(entry->original);
            free(entry->mangled);
            free(entry);
        }
    }
    destroy_list(map_list);
}

static void append_helper_const_decls_from_type_decl(ast_t *type_decl_node,
    const char *helper_id, ListNode_t **const_decls, ListBuilder *var_builder,
    ast_t *type_section)
{
    if (type_decl_node == NULL || helper_id == NULL || const_decls == NULL ||
        var_builder == NULL)
        return;

    ast_t *record_spec = find_helper_record_spec(type_decl_node);
    if (record_spec == NULL)
        return;

    ListNode_t **tail = const_decls;
    while (*tail != NULL)
        tail = &(*tail)->next;

    for (ast_t *cur = record_spec->child; cur != NULL; cur = cur->next)
    {
        ast_t *node = cur;
        ast_t *unwrapped = unwrap_pascal_node(cur);
        int is_const_section = 0;
        if (node != NULL && node->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        else if (node != NULL && node->typ == PASCAL_T_NONE &&
                 node->sym != NULL && node->sym->name != NULL &&
                 strcasecmp(node->sym->name, "const") == 0)
            is_const_section = 1;
        else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_CONST_SECTION)
            is_const_section = 1;
        if (!is_const_section)
            continue;

        ListNode_t *helper_consts = NULL;
        ListNode_t **var_tail_before = var_builder->tail_next;
        append_const_decls_from_section(node, &helper_consts, var_builder, type_section);
        ListNode_t *new_var_nodes = (var_tail_before != NULL) ? *var_tail_before : NULL;

        ListNode_t *const_cur = helper_consts;
        while (const_cur != NULL)
        {
            ListNode_t *next = const_cur->next;
            const_cur->next = NULL;

            Tree_t *decl = (Tree_t *)const_cur->cur;
            if (decl != NULL && decl->type == TREE_CONST_DECL &&
                decl->tree_data.const_decl_data.id != NULL)
            {
                char *mangled = mangle_helper_const_name(helper_id,
                    decl->tree_data.const_decl_data.id);
                if (mangled != NULL)
                {
                    free(decl->tree_data.const_decl_data.id);
                    decl->tree_data.const_decl_data.id = mangled;
                }
            }

            *tail = const_cur;
            tail = &const_cur->next;
            const_cur = next;
        }

        /* Also rename any typed var decls that were appended to var_builder
         * from this const section (e.g., MaxValue : Single = ...) */
        ListBuilder helper_map_builder;
        list_builder_init(&helper_map_builder);

        for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
        {
            Tree_t *decl = (Tree_t *)iter->cur;
            if (decl == NULL)
                continue;
            if (decl->type == TREE_VAR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.var_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(helper_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&helper_map_builder, old_id, mangled);
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                            free(mangled);
                        }
                    }
                }
            }
            else if (decl->type == TREE_ARR_DECL)
            {
                for (ListNode_t *id_node = decl->tree_data.arr_decl_data.ids;
                     id_node != NULL; id_node = id_node->next)
                {
                    if (id_node->type == LIST_STRING && id_node->cur != NULL)
                    {
                        char *old_id = (char *)id_node->cur;
                        char *mangled = mangle_helper_const_name(helper_id, old_id);
                        if (mangled != NULL)
                        {
                            add_class_const_map_entry(&helper_map_builder, old_id, mangled);
                            free(id_node->cur);
                            id_node->cur = strdup(mangled);
                            free(mangled);
                        }
                    }
                }
            }
        }

        /* Rewrite initializer statements so LHS uses mangled names too */
        ListNode_t *helper_map = list_builder_finish(&helper_map_builder);
        if (helper_map != NULL)
        {
            for (ListNode_t *iter = new_var_nodes; iter != NULL; iter = iter->next)
            {
                Tree_t *decl = (Tree_t *)iter->cur;
                if (decl == NULL)
                    continue;
                if (decl->type == TREE_VAR_DECL)
                    rewrite_class_const_statement(decl->tree_data.var_decl_data.initializer, helper_map);
                else if (decl->type == TREE_ARR_DECL)
                    rewrite_class_const_statement(decl->tree_data.arr_decl_data.initializer, helper_map);
            }
            for (ListNode_t *cur_map = helper_map; cur_map != NULL; cur_map = cur_map->next)
            {
                ClassConstMap *entry = (ClassConstMap *)cur_map->cur;
                if (entry != NULL)
                {
                    free(entry->original);
                    free(entry->mangled);
                    free(entry);
                }
            }
            destroy_list(helper_map);
        }
    }

}

static int nested_type_section_has_id(ast_t *section, const char *id)
{
    if (section == NULL || id == NULL)
        return 0;
    for (ast_t *child = section->child; child != NULL; child = child->next)
    {
        ast_t *unwrapped = unwrap_pascal_node(child);
        if (unwrapped == NULL)
            unwrapped = child;
        if (unwrapped == NULL)
            continue;
        if (unwrapped->typ == PASCAL_T_TYPE_DECL ||
            unwrapped->typ == PASCAL_T_GENERIC_TYPE_DECL)
        {
            char *name = dup_first_identifier_in_node(unwrapped);
            int match = (name != NULL && pascal_identifier_equals(name, id));
            if (name != NULL)
                free(name);
            if (match)
                return 1;
        }
    }
    return 0;
}

static struct TypeRef *ensure_type_ref_from_id(char **type_id, struct TypeRef **type_ref)
{
    if (type_ref == NULL)
        return NULL;
    if (*type_ref != NULL)
        return *type_ref;
    if (type_id == NULL || *type_id == NULL)
        return NULL;

    QualifiedIdent *qid = qualified_ident_from_dotted(*type_id);
    if (qid == NULL)
        qid = qualified_ident_from_single(*type_id);
    if (qid == NULL)
        return NULL;
    *type_ref = type_ref_create(qid, NULL, 0);
    return *type_ref;
}

static void qualify_record_field_nested_types(struct RecordType *record,
    const char *parent_type_name, ast_t *type_section)
{
    if (record == NULL || parent_type_name == NULL || type_section == NULL)
        return;

    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;

        struct TypeRef *type_ref = ensure_type_ref_from_id(&field->type_id, &field->type_ref);
        if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count == 1)
        {
            const char *base = qualified_ident_last(type_ref->name);
            if (base != NULL && nested_type_section_has_id(type_section, base))
            {
                qualify_type_ref_id(&field->type_id, &field->type_ref, parent_type_name, base);
            }
        }

        struct TypeRef *elem_ref = ensure_type_ref_from_id(&field->array_element_type_id,
            &field->array_element_type_ref);
        if (elem_ref != NULL && elem_ref->name != NULL && elem_ref->name->count == 1)
        {
            const char *base = qualified_ident_last(elem_ref->name);
            if (base != NULL && nested_type_section_has_id(type_section, base))
            {
                qualify_type_ref_id(&field->array_element_type_id, &field->array_element_type_ref,
                    parent_type_name, base);
            }
        }

        struct TypeRef *ptr_ref = ensure_type_ref_from_id(&field->pointer_type_id,
            &field->pointer_type_ref);
        if (ptr_ref != NULL && ptr_ref->name != NULL && ptr_ref->name->count == 1)
        {
            const char *base = qualified_ident_last(ptr_ref->name);
            if (base != NULL && nested_type_section_has_id(type_section, base))
            {
                qualify_type_ref_id(&field->pointer_type_id, &field->pointer_type_ref,
                    parent_type_name, base);
            }
        }
    }
}

static void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest,
    ListNode_t **subprograms, ListNode_t **const_decls, ListBuilder *var_builder,
    const char *parent_type_name) {
    if (type_section == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
        fprintf(stderr, "[KGPC] append_type_decls_from_section: type_section->typ=%d child=%p line=%d parent=%s\n",
                type_section->typ, (void*)type_section->child, type_section->line,
                parent_type_name ? parent_type_name : "<none>");

        /* Count children and print info for each */
        int child_count = 0;
        for (ast_t *c = type_section->child; c != NULL; c = c->next) {
            char *c_id = dup_first_identifier_in_node(c);
            fprintf(stderr, "[KGPC]   child[%d] typ=%d (%s) line=%d id=%s\n",
                    child_count++, c->typ, pascal_tag_to_string(c->typ),
                    c->line, c_id != NULL ? c_id : "<none>");
            if (c_id != NULL) free(c_id);
        }
        fprintf(stderr, "[KGPC]   total children: %d\n", child_count);
    }

    ast_t *type_decl = type_section->child;
    while (type_decl != NULL) {
        ast_t *unwrapped = unwrap_pascal_node(type_decl);
        if (unwrapped == NULL)
            unwrapped = type_decl;

        if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL && unwrapped != NULL) {
            char *name = dup_first_identifier_in_node(unwrapped);
            fprintf(stderr, "[KGPC] type-section decl tag=%d (%s) name=%s\n",
                    unwrapped->typ,
                    pascal_tag_to_string(unwrapped->typ),
                    name != NULL ? name : "<none>");
            if (name != NULL)
                free(name);
        }

        int treat_as_generic = 0;
        if (unwrapped != NULL) {
            if (unwrapped->typ == PASCAL_T_GENERIC_TYPE_DECL) {
                treat_as_generic = 1;
            } else if (unwrapped->typ == PASCAL_T_TYPE_DECL) {
                ast_t *id_node = unwrapped->child;
                while (id_node != NULL && id_node->typ != PASCAL_T_IDENTIFIER)
                    id_node = id_node->next;
                ast_t *next = id_node != NULL ? id_node->next : NULL;
                if (next != NULL && next->typ == PASCAL_T_TYPE_PARAM_LIST)
                    treat_as_generic = 1;
            }
        }

        if (treat_as_generic && unwrapped != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_TFPG") != NULL) {
                char *type_name = dup_first_identifier_in_node(unwrapped);
                if (type_name != NULL) {
                    fprintf(stderr, "[KGPC] append_type_decls_from_section saw generic-like %s (tag=%d)\n",
                            type_name, unwrapped->typ);
                    free(type_name);
                }
            }
            Tree_t *decl = convert_generic_type_decl(unwrapped);
            if (decl != NULL) {
                if (parent_type_name != NULL && decl->tree_data.type_decl_data.id != NULL) {
                    char *orig_id = decl->tree_data.type_decl_data.id;
                    size_t len = strlen(parent_type_name) + 1 + strlen(orig_id) + 1;
                    char *qualified_id = (char *)malloc(len);
                    if (qualified_id != NULL) {
                        snprintf(qualified_id, len, "%s.%s", parent_type_name, orig_id);
                        free(orig_id);
                        decl->tree_data.type_decl_data.id = qualified_id;
                        /* Set outer_type_id on record types for nested type qualification */
                        if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                            decl->tree_data.type_decl_data.info.record != NULL &&
                            decl->tree_data.type_decl_data.info.record->outer_type_id == NULL)
                            decl->tree_data.type_decl_data.info.record->outer_type_id = strdup(parent_type_name);
                    }
                    /* Also qualify pointer_type_id for pointer aliases referencing
                     * sibling types in the same nested scope */
                    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
                        if (alias->is_pointer) {
                            struct TypeRef *ptr_ref = ensure_type_ref_from_id(
                                &alias->pointer_type_id, &alias->pointer_type_ref);
                            if (ptr_ref != NULL && ptr_ref->name != NULL && ptr_ref->name->count == 1) {
                                const char *base = qualified_ident_last(ptr_ref->name);
                                if (base != NULL && nested_type_section_has_id(type_section, base)) {
                                    qualify_type_ref_id(&alias->pointer_type_id, &alias->pointer_type_ref,
                                        parent_type_name, base);
                                }
                            }
                        }
                    }
                }
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
        } else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_TYPE_DECL) {
            ListNode_t *nested_type_decls = NULL;
            char *nested_parent_name = NULL;
            if (const_decls != NULL && var_builder != NULL) {
                char *helper_id = dup_first_identifier_in_node(unwrapped);
                if (helper_id != NULL) {
                    append_helper_const_decls_from_type_decl(unwrapped, helper_id,
                        const_decls, var_builder, type_section);
                    append_class_const_decls_from_type_decl(unwrapped, helper_id,
                        const_decls, var_builder, type_section);
                    if (parent_type_name != NULL) {
                        size_t len = strlen(parent_type_name) + 1 + strlen(helper_id) + 1;
                        nested_parent_name = (char *)malloc(len);
                        if (nested_parent_name != NULL) {
                            snprintf(nested_parent_name, len, "%s.%s", parent_type_name, helper_id);
                        }
                    } else {
                        nested_parent_name = strdup(helper_id);
                    }
                    free(helper_id);
                }
            } else if (parent_type_name != NULL) {
                char *type_id = dup_first_identifier_in_node(unwrapped);
                if (type_id != NULL) {
                    size_t len = strlen(parent_type_name) + 1 + strlen(type_id) + 1;
                    nested_parent_name = (char *)malloc(len);
                    if (nested_parent_name != NULL) {
                        snprintf(nested_parent_name, len, "%s.%s", parent_type_name, type_id);
                    }
                    free(type_id);
                }
            }
            Tree_t *decl = convert_type_decl_ex(unwrapped, subprograms, &nested_type_decls);
            if (decl != NULL && parent_type_name != NULL && decl->tree_data.type_decl_data.id != NULL) {
                char *orig_id = decl->tree_data.type_decl_data.id;
                size_t len = strlen(parent_type_name) + 1 + strlen(orig_id) + 1;
                char *qualified_id = (char *)malloc(len);
                if (qualified_id != NULL) {
                    snprintf(qualified_id, len, "%s.%s", parent_type_name, orig_id);
                    free(orig_id);
                    decl->tree_data.type_decl_data.id = qualified_id;
                    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                        decl->tree_data.type_decl_data.info.record != NULL &&
                        decl->tree_data.type_decl_data.info.record->type_id != NULL)
                    {
                        free(decl->tree_data.type_decl_data.info.record->type_id);
                        decl->tree_data.type_decl_data.info.record->type_id = strdup(qualified_id);
                        if (decl->tree_data.type_decl_data.info.record->outer_type_id == NULL)
                            decl->tree_data.type_decl_data.info.record->outer_type_id = strdup(parent_type_name);
                        qualify_record_field_nested_types(decl->tree_data.type_decl_data.info.record,
                            parent_type_name, type_section);
                        if (decl->tree_data.type_decl_data.info.record->parent_class_name != NULL &&
                            nested_type_section_has_id(type_section,
                                decl->tree_data.type_decl_data.info.record->parent_class_name))
                        {
                            char *orig_parent = decl->tree_data.type_decl_data.info.record->parent_class_name;
                            size_t plen = strlen(parent_type_name) + 1 + strlen(orig_parent) + 1;
                            char *qualified_parent = (char *)malloc(plen);
                            if (qualified_parent != NULL)
                            {
                                snprintf(qualified_parent, plen, "%s.%s", parent_type_name, orig_parent);
                                free(orig_parent);
                                decl->tree_data.type_decl_data.info.record->parent_class_name = qualified_parent;
                            }
                        }
                    }
                    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
                        /* Qualify pointer_type_id so symbol table lookup resolves
                         * the sibling nested type correctly */
                        if (alias->is_pointer) {
                            struct TypeRef *ptr_ref = ensure_type_ref_from_id(
                                &alias->pointer_type_id, &alias->pointer_type_ref);
                            if (ptr_ref != NULL && ptr_ref->name != NULL && ptr_ref->name->count == 1) {
                                const char *base = qualified_ident_last(ptr_ref->name);
                                if (base != NULL && nested_type_section_has_id(type_section, base)) {
                                    qualify_type_ref_id(&alias->pointer_type_id, &alias->pointer_type_ref,
                                        parent_type_name, base);
                                }
                            }
                        }
                        if (alias->kgpc_type != NULL &&
                            alias->kgpc_type->kind == TYPE_KIND_POINTER &&
                            alias->kgpc_type->info.points_to != NULL &&
                            alias->kgpc_type->info.points_to->kind == TYPE_KIND_RECORD &&
                            alias->kgpc_type->info.points_to->info.record_info != NULL)
                        {
                            struct RecordType *points_to_record = alias->kgpc_type->info.points_to->info.record_info;
                            if (points_to_record->type_id != NULL)
                            {
                                free(points_to_record->type_id);
                                points_to_record->type_id = strdup(qualified_id);
                            }
                        }
                    }
                }
            }
            if (nested_type_decls != NULL) {
                ListNode_t *nested_cursor = nested_type_decls;
                while (nested_cursor != NULL) {
                    ListNode_t *next = nested_cursor->next;
                    nested_cursor->next = NULL;
                    *tail = nested_cursor;
                    tail = &nested_cursor->next;
                    nested_cursor = next;
                }
            }
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
            if (nested_parent_name != NULL)
                free(nested_parent_name);
        }
        type_decl = type_decl->next;
    }
}

static int map_relop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_EQ: return EQ;
    case PASCAL_T_NE: return NE;
    case PASCAL_T_LT: return LT;
    case PASCAL_T_LE: return LE;
    case PASCAL_T_GT: return GT;
    case PASCAL_T_GE: return GE;
    case PASCAL_T_IN: return IN;
    case PASCAL_T_AND: return AND;
    case PASCAL_T_OR:  return OR;
    default: return UNKNOWN_TYPE;
    }
}

static int map_addop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_ADD: return PLUS;
    case PASCAL_T_SUB: return MINUS;
    case PASCAL_T_OR:  return OR;
    default: return UNKNOWN_TYPE;
    }
}

static int map_mulop_tag(int tag) {
    switch (tag) {
    case PASCAL_T_MUL: return STAR;
    case PASCAL_T_DIV: return SLASH;
    case PASCAL_T_INTDIV: return DIV;
    case PASCAL_T_MOD: return MOD;
    case PASCAL_T_POWER: return POWER;
    case PASCAL_T_AND: return AND;
    case PASCAL_T_XOR: return XOR;
    case PASCAL_T_SHL: return SHL;
    case PASCAL_T_SHR: return SHR;
    case PASCAL_T_ROL: return ROL;
    case PASCAL_T_ROR: return ROR;
    default: return UNKNOWN_TYPE;
    }
}

static void append_asm_line(char **buffer, size_t *length, const char *line) {
    if (line == NULL)
        return;
    size_t line_len = strlen(line);
    size_t new_len = *length + line_len + 1; /* newline */
    char *tmp = realloc(*buffer, new_len + 1);
    if (tmp == NULL)
        return;
    memcpy(tmp + *length, line, line_len);
    tmp[*length + line_len] = '\n';
    tmp[new_len] = '\0';
    *buffer = tmp;
    *length = new_len;
}

static void collect_asm_lines(ast_t *node, char **buffer, size_t *length) {
    for (ast_t *cur = node; cur != NULL && cur != ast_nil; cur = cur->next) {
        if (cur->sym != NULL && cur->sym->name != NULL)
            append_asm_line(buffer, length, cur->sym->name);
        if (cur->child != NULL)
            collect_asm_lines(cur->child, buffer, length);
    }
}

static char *collect_asm_text(ast_t *block_node) {
    char *buffer = NULL;
    size_t length = 0;
    collect_asm_lines(block_node, &buffer, &length);
    return buffer;
}

static int extract_constant_int(struct Expression *expr, long long *out_value) {
    if (expr == NULL || out_value == NULL)
        return 1;

    switch (expr->type) {
    case EXPR_INUM:
        *out_value = expr->expr_data.i_num;
        return 0;
    case EXPR_CHAR_CODE:
        *out_value = expr->expr_data.char_code;
        return 0;
    case EXPR_STRING:
        /* Handle single-character string literals as char values */
        if (expr->expr_data.string != NULL && strlen(expr->expr_data.string) == 1) {
            *out_value = (unsigned char)expr->expr_data.string[0];
            return 0;
        }
        return 1;
    case EXPR_SIGN_TERM:
        if (extract_constant_int(expr->expr_data.sign_term, out_value) != 0)
            return 1;
        {
            unsigned long long bits = (unsigned long long)(*out_value);
            bits = 0ULL - bits;
            *out_value = (long long)bits;
        }
        return 0;
    case EXPR_ADDOP: {
        long long left_value = 0;
        long long right_value = 0;
        if (extract_constant_int(expr->expr_data.addop_data.left_expr, &left_value) != 0)
            return 1;
        if (extract_constant_int(expr->expr_data.addop_data.right_term, &right_value) != 0)
            return 1;
        if (expr->expr_data.addop_data.addop_type == PLUS)
            *out_value = left_value + right_value;
        else if (expr->expr_data.addop_data.addop_type == MINUS)
            *out_value = left_value - right_value;
        else
            return 1;
        return 0;
    }
    case EXPR_MULOP: {
        long long left_value = 0;
        long long right_value = 0;
        if (extract_constant_int(expr->expr_data.mulop_data.left_term, &left_value) != 0)
            return 1;
        if (extract_constant_int(expr->expr_data.mulop_data.right_factor, &right_value) != 0)
            return 1;
        if (expr->expr_data.mulop_data.mulop_type == STAR) {
            *out_value = left_value * right_value;
            return 0;
        }
        return 1;
    }
    default:
        break;
    }

    return 1;
}

static ListNode_t *append_set_element(ListNode_t *elements, struct SetElement *element)
{
    if (element == NULL)
        return elements;

    ListNode_t *node = CreateListNode(element, LIST_SET_ELEMENT);
    if (elements == NULL)
        return node;
    return PushListNodeBack(elements, node);
}

static struct Expression *convert_set_literal(ast_t *set_node) {
    if (set_node == NULL)
        return mk_set(0, 0, NULL, 1);

    uint32_t mask = 0;
    int all_constant = 1;
    ListNode_t *elements = NULL;

    for (ast_t *element = set_node->child; element != NULL && element != ast_nil; element = element->next) {
        ast_t *unwrapped = unwrap_pascal_node(element);
        if (unwrapped == NULL)
            continue;

        if (unwrapped->typ == PASCAL_T_RANGE) {
            ast_t *start_node = unwrap_pascal_node(unwrapped->child);
            ast_t *end_node = NULL;
            if (start_node != NULL)
                end_node = unwrap_pascal_node(start_node->next);

            struct Expression *start_expr = convert_expression(start_node);
            struct Expression *end_expr = convert_expression(end_node);
            elements = append_set_element(elements, mk_set_element(start_expr, end_expr));

            long long start_value = 0;
            long long end_value = 0;
            int have_start = (start_expr != NULL) && extract_constant_int(start_expr, &start_value) == 0;
            int have_end = (end_expr != NULL) && extract_constant_int(end_expr, &end_value) == 0;

            if (!have_start || !have_end)
            {
                all_constant = 0;
                continue;
            }

            if (start_value > end_value) {
                long long tmp = start_value;
                start_value = end_value;
                end_value = tmp;
            }
            for (long long value = start_value; value <= end_value; ++value) {
                if (value >= 0 && value < 32)
                    mask |= (uint32_t)1u << value;
                else
                    all_constant = 0;  /* Value outside bitmask range */
            }
        } else {
            struct Expression *value_expr = convert_expression(unwrapped);
            elements = append_set_element(elements, mk_set_element(value_expr, NULL));

            long long value = 0;
            if (value_expr == NULL || extract_constant_int(value_expr, &value) != 0) {
                all_constant = 0;
                continue;
            }

            if (value >= 0 && value < 32)
                mask |= (uint32_t)1u << value;
            else
                all_constant = 0;  /* Value outside bitmask range */
        }
    }

    return mk_set(set_node->line, mask, elements, all_constant);
}

static struct Expression *convert_inherited_operator_expr(int line_num, const char *op_id, ast_t *arg_node);

static struct Expression *convert_factor(ast_t *expr_node) {
    expr_node = unwrap_pascal_node(expr_node);
    if (expr_node == NULL) {
        return NULL;
    }

    switch (expr_node->typ) {
    case PASCAL_T_INTEGER:
    {
        const char *num_str = (expr_node->sym != NULL) ? expr_node->sym->name : "0";
        int base = 10;
        /* Handle hex literals ($FF), binary literals (%1010), octal literals (&777) */
        if (num_str[0] == '$')
        {
            base = 16;
            num_str++;  /* Skip the $ prefix */
        }
        else if (num_str[0] == '%')
        {
            base = 2;
            num_str++;  /* Skip the % prefix */
        }
        else if (num_str[0] == '&')
        {
            base = 8;
            num_str++;  /* Skip the & prefix */
        }
        long long literal_value = 0;
        if (parse_integer_literal(num_str, base, &literal_value, NULL) != 0)
            literal_value = 0;
        return mk_inum(expr_node->line, literal_value);
    }
    case PASCAL_T_REAL:
        return mk_rnum(expr_node->line, strtod(expr_node->sym->name, NULL));
    case PASCAL_T_STRING:
        return mk_string(expr_node->line, dup_symbol(expr_node));
    case PASCAL_T_CHAR:
    {
        /* Convert character literal to character code */
        const char *char_str = (expr_node->sym != NULL) ? expr_node->sym->name : NULL;
        if (char_str != NULL && strlen(char_str) >= 1)
        {
            return mk_charcode(expr_node->line, (unsigned int)char_str[0]);
        }
        else
        {
            /* Fallback: create empty character */
            return mk_charcode(expr_node->line, 0);
        }
    }
    case PASCAL_T_CHAR_CODE:
    {
        unsigned int char_value = 0;
        const char *literal = (expr_node->sym != NULL) ? expr_node->sym->name : NULL;
        if (literal != NULL)
        {
            const char *digits = literal;
            if (*digits == '#')
                ++digits;

            int base = 10;
            if (*digits == '$')
            {
                base = 16;
                ++digits;
            }

            if (*digits != '\0')
            {
                char *endptr = NULL;
                long parsed = strtol(digits, &endptr, base);
                if (endptr != NULL && *endptr == '\0')
                {
                    if (parsed < 0)
                        parsed = 0;
                    char_value = (unsigned int)parsed;
                }
                else
                {
                    fprintf(stderr,
                        "ERROR: invalid character code literal %s at line %d.\n",
                        literal, expr_node->line);
                }
            }
            else
            {
                fprintf(stderr,
                    "ERROR: invalid character code literal %s at line %d.\n",
                    literal, expr_node->line);
            }
        }
        else
        {
            fprintf(stderr,
                "ERROR: invalid character code literal at line %d.\n",
                expr_node->line);
        }
        return mk_charcode(expr_node->line, char_value);
    }
    case PASCAL_T_BOOLEAN:
        if (expr_node->sym != NULL && expr_node->sym->name != NULL) {
            const char *value = expr_node->sym->name;
            int bool_value = (strcasecmp(value, "true") == 0);
            return mk_bool(expr_node->line, bool_value);
        }
        return mk_bool(expr_node->line, 0);
    case PASCAL_T_SET:
        return convert_set_literal(expr_node);
    case PASCAL_T_NIL:
        return mk_nil(expr_node->line);
    case PASCAL_T_IDENTIFIER:
        return mk_varid(expr_node->line, dup_symbol(expr_node));
    case PASCAL_T_QUALIFIED_IDENTIFIER:
    {
        QualifiedIdent *qid = qualified_ident_from_ast(expr_node);
        char *joined = qualified_ident_join(qid, ".");
        struct Expression *expr = mk_varid(expr_node->line, joined);
        if (expr != NULL)
        {
            if (expr->id_ref != NULL)
                qualified_ident_free(expr->id_ref);
            expr->id_ref = qid;
            qid = NULL;
        }
        if (qid != NULL)
            qualified_ident_free(qid);
        return expr;
    }
    case PASCAL_T_CONSTRUCTED_TYPE:
    {
        char *base_name = NULL;
        ListNode_t *type_args = NULL;
        if (extract_constructed_type_info(expr_node, &base_name, &type_args))
        {
            char *specialized_name = mangle_specialized_name_from_list(base_name, type_args);
            if (specialized_name == NULL && base_name != NULL)
                specialized_name = strdup(base_name);

            /* Trigger generic instantiation for inline specialize in expression
             * context — but only if this specialization hasn't already been
             * registered by the normal type-declaration pipeline. */
            if (base_name != NULL && type_args != NULL) {
                /* Skip inline spec creation if any type arg is itself a
                 * generic type parameter (e.g. specialize T<T> inside a
                 * generic method body — not a concrete specialization). */
                int has_type_param_arg = 0;
                if (generic_registry_current_context() != NULL) {
                    for (ListNode_t *a = type_args; a != NULL; a = a->next) {
                        if (a->type == LIST_STRING && a->cur != NULL &&
                            generic_registry_is_type_param((const char *)a->cur)) {
                            has_type_param_arg = 1;
                            break;
                        }
                    }
                }
                if (has_type_param_arg) goto skip_inline_spec;

                ListNode_t *args_copy = NULL;
                ListNode_t *args_tail = NULL;
                for (ListNode_t *a = type_args; a != NULL; a = a->next) {
                    if (a->type == LIST_STRING && a->cur != NULL) {
                        ListNode_t *n = CreateListNode(strdup((char *)a->cur), LIST_STRING);
                        if (n != NULL) {
                            if (args_tail != NULL) { args_tail->next = n; args_tail = n; }
                            else { args_copy = n; args_tail = n; }
                        }
                    }
                }
                if (args_copy != NULL) {
                    char *inst_name = NULL;
                    struct RecordType *record = instantiate_generic_record(
                        base_name, args_copy, &inst_name);
                    if (record != NULL) {
                        char *rec_id = inst_name != NULL ? strdup(inst_name) : strdup(specialized_name);
                        Tree_t *type_decl = mk_record_type(expr_node->line, rec_id, record);
                        if (type_decl != NULL) {
                            DeferredInlineSpec *entry = (DeferredInlineSpec *)calloc(1, sizeof(DeferredInlineSpec));
                            if (entry != NULL) {
                                entry->type_decl = type_decl;
                                entry->next = g_deferred_inline_specs;
                                g_deferred_inline_specs = entry;
                            } else {
                                destroy_tree(type_decl);
                            }
                        }
                    }
                    /* args_copy ownership: instantiate_generic_record takes ownership
                     * on success (it stores args in the record); on failure it may or
                     * may not have freed them, but we can safely destroy_list a NULL. */
                    if (record == NULL)
                        destroy_list(args_copy);
                    if (inst_name != NULL)
                        free(inst_name);
                }
                skip_inline_spec: ;
            }

            if (type_args != NULL)
                destroy_list(type_args);
            if (base_name != NULL)
                free(base_name);
            if (specialized_name != NULL)
                return mk_varid(expr_node->line, specialized_name);
        }
        return NULL;
    }
    case PASCAL_T_FUNC_CALL: {
        ast_t *child = expr_node->child;
        char *id = NULL;
        if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL)
        {
            fprintf(stderr, "[KGPC_DEBUG_SPECIALIZE_CALLS] FUNC_CALL line=%d child_typ=%d(%s)\n",
                expr_node->line,
                child != NULL ? child->typ : -1,
                child != NULL ? pascal_tag_to_string(child->typ) : "<null>");
            if (child != NULL && child->sym != NULL && child->sym->name != NULL)
            {
                fprintf(stderr, "[KGPC_DEBUG_SPECIALIZE_CALLS]   child sym=%s\n",
                    child->sym->name);
            }
            if (child != NULL && child->child != NULL)
            {
                fprintf(stderr, "[KGPC_DEBUG_SPECIALIZE_CALLS]   child->child typ=%d(%s) sym=%s\n",
                    child->child->typ,
                    pascal_tag_to_string(child->child->typ),
                    (child->child->sym != NULL && child->child->sym->name != NULL)
                        ? child->child->sym->name : "<null>");
            }
        }
        if (child != NULL && child->typ == PASCAL_T_MEMBER_ACCESS)
        {
            struct Expression *callee = convert_expression(child);
            ListNode_t *args = convert_expression_list(child->next);
            if (callee != NULL && callee->type == EXPR_RECORD_ACCESS &&
                callee->expr_data.record_access_data.record_expr != NULL &&
                callee->expr_data.record_access_data.field_id != NULL)
            {
                struct Expression *base_expr =
                    callee->expr_data.record_access_data.record_expr;
                char *method_id = strdup(callee->expr_data.record_access_data.field_id);
                callee->expr_data.record_access_data.record_expr = NULL;
                destroy_expr(callee);
                if (base_expr != NULL)
                    args = PushListNodeFront(args, CreateListNode(base_expr, LIST_EXPR));
                struct Expression *call_expr = mk_functioncall(expr_node->line, method_id, args);
                tag_operator_call(call_expr, is_operator_token_name(method_id));
                if (call_expr != NULL)
                {
                    call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
                    call_expr->expr_data.function_call_data.placeholder_method_name =
                        method_id != NULL ? strdup(method_id) : NULL;
                }
                return call_expr;
            }
            if (callee != NULL)
                destroy_expr(callee);
            return mk_functioncall(expr_node->line, NULL, args);
        }
        int saw_inherited = 0;
        if (child != NULL) {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                const char *name = ast_symbol_name(child);
                if (name != NULL && strcasecmp(name, "inherited") == 0) {
                    saw_inherited = 1;
                    child = child->next;
                    /* Now child points to the method name; extract it */
                    if (child != NULL && child->typ == PASCAL_T_IDENTIFIER) {
                        id = dup_symbol(child);
                        child = child->next;
                    } else if (child != NULL) {
                        ast_t *unwrapped = unwrap_pascal_node(child);
                        if (unwrapped != NULL && unwrapped->typ == PASCAL_T_IDENTIFIER) {
                            id = dup_symbol(unwrapped);
                            child = child->next;
                        }
                    }
                }
                else if (name != NULL)
                    id = dup_symbol(child);
            } else if (child->typ == PASCAL_T_TYPECAST &&
                       child->child != NULL &&
                       child->child->typ == PASCAL_T_IDENTIFIER) {
                /* Handle chained call: TypeName(source)(outer_args)
                 * Parser creates: FUNC_CALL(TYPECAST(TypeName, source), outer_args)
                 * Convert the typecast part to an expression, then create a call
                 * with the outer args that invokes the typecast result. */
                struct Expression *typecast_expr = convert_expression(child);
                ast_t *outer_args_start = child->next;
                ListNode_t *outer_args = convert_expression_list(outer_args_start);
                /* Create a function call where the callee is the typecast expression */
                char *type_id = dup_symbol(child->child);
                struct Expression *call_expr = mk_functioncall(expr_node->line, type_id, outer_args);
                if (call_expr != NULL) {
                    call_expr->expr_data.function_call_data.is_procedural_var_call = 1;
                    call_expr->expr_data.function_call_data.procedural_var_expr = typecast_expr;
                }
                return call_expr;
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                const char *name = ast_symbol_name(child->child);
                if (name != NULL && strcasecmp(name, "inherited") == 0)
                    saw_inherited = 1;
                else if (name != NULL)
                    id = dup_symbol(child->child);
            }
            if (!saw_inherited)
                child = child->next;
        }
        if (id == NULL && !saw_inherited) {
            for (ast_t *scan = expr_node->child; scan != NULL; scan = scan->next) {
                ast_t *node = unwrap_pascal_node(scan);
                if (node == NULL)
                    node = scan;
                if (node == NULL)
                    continue;
                if (node->typ == PASCAL_T_ARG_LIST)
                    break;
                if (node->typ == PASCAL_T_IDENTIFIER) {
                    const char *name = ast_symbol_name(node);
                    if (name == NULL)
                        continue;
                    id = strdup(name);
                    break;
                }
            }
        }
        int is_bare_inherited = 0;
        if (id == NULL && saw_inherited && g_current_method_name != NULL) {
            /* cparser encodes bare "inherited" as FUNC_CALL with an empty child node.
             * In method bodies, that means "inherited <current-method-name>". */
            id = strdup(g_current_method_name);
            is_bare_inherited = 1;
        }
        if (saw_inherited && id != NULL)
        {
            struct Expression *inherited_op = convert_inherited_operator_expr(expr_node->line, id, child);
            if (inherited_op != NULL)
            {
                free(id);
                return inherited_op;
            }
        }
        ListNode_t *args = convert_expression_list(child);
        struct Expression *result = mk_functioncall(expr_node->line, id, args);
        tag_operator_call(result, is_operator_token_name(id));
        if (saw_inherited && result != NULL) {
            result->expr_data.function_call_data.is_inherited_call = 1;
            result->expr_data.function_call_data.is_bare_inherited = is_bare_inherited;
        }
        return result;
    }
    case PASCAL_T_ARRAY_ACCESS: {
        ast_t *array_node = expr_node->child;
        ast_t *index_expr_node = array_node != NULL ? array_node->next : NULL;
        struct Expression *base = convert_expression(array_node);
        struct Expression *first_index = convert_expression(index_expr_node);
        struct Expression *result = mk_arrayaccess(expr_node->line, base, first_index);
        
        /* For multi-dimensional arrays like arr[x, y, z], the indices are chained
         * via ->next. Store additional indices in extra_indices list. */
        if (index_expr_node != NULL && index_expr_node->next != NULL) {
            ast_t *extra_idx_node = index_expr_node->next;
            ListNode_t *extra_indices = NULL;
            ListNode_t *extra_tail = NULL;
            
            while (extra_idx_node != NULL) {
                struct Expression *extra_idx = convert_expression(extra_idx_node);
                if (extra_idx != NULL) {
                    ListNode_t *new_node = CreateListNode(extra_idx, LIST_EXPR);
                    if (new_node != NULL) {
                        if (extra_tail == NULL) {
                            extra_indices = new_node;
                            extra_tail = new_node;
                        } else {
                            extra_tail->next = new_node;
                            extra_tail = new_node;
                        }
                    }
                }
                extra_idx_node = extra_idx_node->next;
            }
            result->expr_data.array_access_data.extra_indices = extra_indices;
        }
        return result;
    }
    default:
        return NULL;
    }
}

static struct Expression *convert_binary_expr(ast_t *node, int type) {
    ast_t *left_node = node->child;
    ast_t *right_node = left_node != NULL ? left_node->next : NULL;
    struct Expression *left = convert_expression(left_node);
    struct Expression *right = convert_expression(right_node);

    switch (type) {
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_OR:
        return mk_addop(node->line, map_addop_tag(type), left, right);
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_POWER:
    case PASCAL_T_AND:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    case PASCAL_T_ROL:
    case PASCAL_T_ROR:
        return mk_mulop(node->line, map_mulop_tag(type), left, right);
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
    case PASCAL_T_IN:
        return mk_relop(node->line, map_relop_tag(type), left, right);
    default:
        break;
    }
    return NULL;
}

static struct Expression *convert_inherited_operator_expr(int line_num, const char *op_id, ast_t *arg_node)
{
    if (g_current_method_name == NULL || op_id == NULL || arg_node == NULL || arg_node->next != NULL)
        return NULL;

    struct Expression *lhs = mk_functioncall(line_num, strdup(g_current_method_name), NULL);
    if (lhs == NULL)
        return NULL;
    lhs->expr_data.function_call_data.is_inherited_call = 1;

    struct Expression *rhs = convert_expression(arg_node);
    if (rhs == NULL)
    {
        destroy_expr(lhs);
        return NULL;
    }

    if (strcasecmp(op_id, "and") == 0)
        return mk_mulop(line_num, AND, lhs, rhs);
    if (strcasecmp(op_id, "xor") == 0)
        return mk_mulop(line_num, XOR, lhs, rhs);
    if (strcasecmp(op_id, "or") == 0)
        return mk_addop(line_num, OR, lhs, rhs);

    destroy_expr(lhs);
    destroy_expr(rhs);
    return NULL;
}

static struct Expression *convert_unary_expr(ast_t *node) {
    struct Expression *inner = convert_expression(node->child);
    // Only negate for PASCAL_T_NEG, unary plus (PASCAL_T_POS) returns the inner expression as-is
    if (node->typ == PASCAL_T_NEG) {
        return mk_signterm(node->line, inner);
    }
    return inner;
}

static const char *tag_name(tag_t tag) {
    const char *name = pascal_tag_to_string(tag);
    return (name != NULL) ? name : "UNKNOWN";
}

static struct Expression *convert_expression(ast_t *expr_node) {
    ast_t *original_node = expr_node;  /* Save for source_index */
    expr_node = unwrap_pascal_node(expr_node);

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_expression: typ=%d line=%d\n", expr_node->typ, expr_node->line);
        if (expr_node->next != NULL) {
            fprintf(stderr, "[KGPC] convert_expression: has next sibling typ=%d\n", expr_node->next->typ);
        }
    }

    if (expr_node == NULL || expr_node == ast_nil)
        return NULL;

    /* PASCAL_T_NONE nodes are empty/placeholder nodes from the parser
     * (e.g., optional clauses that were absent).  Treat as NULL silently. */
    if (expr_node->typ == PASCAL_T_NONE)
        return NULL;

    struct Expression *result = NULL;
    switch (expr_node->typ) {
    case PASCAL_T_INTEGER:
    case PASCAL_T_REAL:
    case PASCAL_T_STRING:
    case PASCAL_T_CHAR:
    case PASCAL_T_CHAR_CODE:
    case PASCAL_T_BOOLEAN:
    case PASCAL_T_IDENTIFIER:
    case PASCAL_T_QUALIFIED_IDENTIFIER:
    case PASCAL_T_CONSTRUCTED_TYPE:
    case PASCAL_T_FUNC_CALL:
    case PASCAL_T_ARRAY_ACCESS:
    case PASCAL_T_SET:
    case PASCAL_T_NIL:
        result = convert_factor(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_MEMBER_ACCESS:
        result = convert_member_access(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_MUL:
    case PASCAL_T_POWER:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
    case PASCAL_T_EQ:
    case PASCAL_T_NE:
    case PASCAL_T_LT:
    case PASCAL_T_LE:
    case PASCAL_T_GT:
    case PASCAL_T_GE:
    case PASCAL_T_IN:
    case PASCAL_T_AND:
    case PASCAL_T_OR:
    case PASCAL_T_XOR:
    case PASCAL_T_SHL:
    case PASCAL_T_SHR:
    case PASCAL_T_ROL:
    case PASCAL_T_ROR:
        result = convert_binary_expr(expr_node, expr_node->typ);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_UNION:
        result = convert_binary_expr(expr_node, PASCAL_T_ADD);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_INTERSECT:
        result = convert_binary_expr(expr_node, PASCAL_T_MUL);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_DIFF:
        result = convert_binary_expr(expr_node, PASCAL_T_SUB);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_SET_SYM_DIFF:
        result = convert_binary_expr(expr_node, PASCAL_T_XOR);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_IS:
    {
        ast_t *value_node = expr_node->child;
        ast_t *type_node = value_node != NULL ? value_node->next : NULL;
        struct Expression *value_expr = convert_expression(value_node);
        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        struct RecordType *inline_record = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));
        TypeRef *type_ref_local = NULL;
        
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_expression IS: value_node=%p type_node=%p\n", value_node, type_node);
            if (type_node) fprintf(stderr, "[KGPC]   type_node typ=%d\n", type_node->typ);
        }
        
        if (type_node != NULL)
        {
            target_type = convert_type_spec(type_node, &target_type_id, &inline_record, &type_info);
            
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   convert_type_spec result: type=%d id=%s\n", target_type, target_type_id ? target_type_id : "<null>");
            }
            
            type_ref_local = type_ref_from_info_or_id(&type_info, target_type_id);
            destroy_type_info_contents(&type_info);
            if (inline_record != NULL)
                destroy_record_type(inline_record);
        }
        result = mk_is(expr_node->line, value_expr, target_type, target_type_id);
        if (result != NULL)
            result->expr_data.is_data.target_type_ref = type_ref_local;
        else if (type_ref_local != NULL)
            type_ref_free(type_ref_local);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_AS:
    {
        ast_t *value_node = expr_node->child;
        ast_t *type_node = value_node != NULL ? value_node->next : NULL;
        struct Expression *value_expr = convert_expression(value_node);
        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        struct RecordType *inline_record = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));
        TypeRef *type_ref_local = NULL;
        if (type_node != NULL)
        {
            target_type = convert_type_spec(type_node, &target_type_id, &inline_record, &type_info);
            type_ref_local = type_ref_from_info_or_id(&type_info, target_type_id);
            destroy_type_info_contents(&type_info);
            if (inline_record != NULL)
                destroy_record_type(inline_record);
        }
        result = mk_as(expr_node->line, value_expr, target_type, target_type_id);
        if (result != NULL)
            result->expr_data.as_data.target_type_ref = type_ref_local;
        else if (type_ref_local != NULL)
            type_ref_free(type_ref_local);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_NEG:
    case PASCAL_T_POS:
        result = convert_unary_expr(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_NOT:
    {
        struct Expression *not_operand = convert_expression(expr_node->child);
        result = mk_relop(expr_node->line, NOT, not_operand, NULL);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_TUPLE:
    {
        if (tuple_is_record_constructor(expr_node))
        {
            return convert_record_constructor_expr(expr_node);
        }

        ListNode_t *elements = NULL;
        ListNode_t *tail = NULL;
        int count = 0;

        for (ast_t *elem = expr_node->child; elem != NULL; elem = elem->next)
        {
            ast_t *unwrapped = unwrap_pascal_node(elem);
            if (unwrapped == NULL)
                continue;
            struct Expression *elem_expr = convert_expression(unwrapped);
            if (elem_expr == NULL)
                goto tuple_cleanup;

            ListNode_t *node = CreateListNode(elem_expr, LIST_EXPR);
            if (node == NULL)
            {
                destroy_expr(elem_expr);
                goto tuple_cleanup;
            }
            if (elements == NULL)
            {
                elements = node;
                tail = node;
            }
            else
            {
                tail->next = node;
                tail = node;
            }
            ++count;
        }

        return mk_array_literal(expr_node->line, elements, count);

tuple_cleanup:
        while (elements != NULL)
        {
            ListNode_t *next = elements->next;
            if (elements->cur != NULL)
                destroy_expr((struct Expression *)elements->cur);
            free(elements);
            elements = next;
        }
        return NULL;
    }
    case PASCAL_T_RECORD_CONSTRUCTOR:
    {
        return convert_record_constructor_expr(expr_node);
    }
    case PASCAL_T_FIELD_WIDTH:
    {
        /* FPC record constructors with a single field like: (state: (1,2,3))
         * can be parsed as FIELD_WIDTH. Detect tuple RHS and reinterpret as record ctor. */
        ast_t *base_node = expr_node->child;
        ast_t *format_node = (base_node != NULL) ? base_node->next : NULL;
        ast_t *format_unwrapped = unwrap_pascal_node(format_node);
        if (base_node != NULL && base_node->typ == PASCAL_T_IDENTIFIER &&
            base_node->sym != NULL && base_node->sym->name != NULL &&
            format_unwrapped != NULL && format_unwrapped->typ == PASCAL_T_TUPLE)
        {
            struct Expression *field_value = convert_expression(format_unwrapped);
            if (field_value == NULL)
                return NULL;

            struct RecordConstructorField *field =
                (struct RecordConstructorField *)calloc(1, sizeof(struct RecordConstructorField));
            if (field == NULL)
            {
                destroy_expr(field_value);
                return NULL;
            }
            field->field_id = strdup(base_node->sym->name);
            field->value = field_value;

            ListNode_t *node = CreateListNode(field, LIST_UNSPECIFIED);
            if (node == NULL)
            {
                if (field->field_id != NULL)
                    free(field->field_id);
                destroy_expr(field_value);
                free(field);
                return NULL;
            }

            return mk_record_constructor(expr_node->line, node, 1);
        }

        return convert_field_width_expr(expr_node);
    }
    case PASCAL_T_TYPECAST:
    {
        ast_t *type_node = expr_node->child;
        ast_t *value_node = (type_node != NULL) ? type_node->next : NULL;

        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        char *target_type_qualifier = NULL;
        struct RecordType *record_type = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));
        TypeRef *type_ref_local = NULL;

        ast_t *unwrapped_type = unwrap_pascal_node(type_node);
        if (unwrapped_type != NULL)
        {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
                fprintf(stderr, "[KGPC] TYPECAST handler: type_node typ=%d unwrapped typ=%d\n",
                    type_node ? type_node->typ : -1, unwrapped_type->typ);
            target_type = convert_type_spec(unwrapped_type, &target_type_id,
                &record_type, &type_info);
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
                fprintf(stderr, "[KGPC] TYPECAST handler: after convert_type_spec target_type=%d target_type_id=%s\n",
                    target_type, target_type_id ? target_type_id : "(null)");
            type_ref_local = type_ref_from_info_or_id(&type_info, target_type_id);
            destroy_type_info_contents(&type_info);
            if (record_type != NULL)
            {
                destroy_record_type(record_type);
                record_type = NULL;
            }

            if (target_type_id == NULL && type_ref_local != NULL && type_ref_local->name != NULL)
            {
                QualifiedIdent *qid = type_ref_local->name;
                if (qid->count > 0 && qid->segments != NULL)
                {
                    const char *base = qid->segments[qid->count - 1];
                    if (base != NULL)
                        target_type_id = strdup(base);
                    if (target_type_id != NULL && target_type_qualifier == NULL && qid->count > 1)
                    {
                        target_type_qualifier = qualified_ident_join_prefix(qid, qid->count - 1);
                    }
                }
            }

            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_IDENTIFIER)
            {
                target_type_id = dup_symbol(unwrapped_type);
            }
            /* Handle qualified identifiers like widestringmanager.UpperUnicodeStringProc
             * parsed as member access in typecast position */
            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_MEMBER_ACCESS)
            {
                ast_t *base = unwrapped_type->child;
                ast_t *field = (base != NULL) ? base->next : NULL;
                char *base_name = dup_symbol(base);
                char *field_name = dup_symbol(field);
                if (base_name != NULL && field_name != NULL)
                {
                    /* Store components separately as structured data:
                     * - target_type_id: bare name (field/type name)
                     * - target_type_qualifier: base (record variable or parent type)
                     * Semcheck constructs the qualified lookup key when needed. */
                    target_type_id = field_name;
                    field_name = NULL;  /* ownership transferred */
                    target_type_qualifier = base_name;
                    base_name = NULL;   /* ownership transferred */
                }
                if (base_name != NULL) free(base_name);
                if (field_name != NULL) free(field_name);
            }
            if (target_type_id != NULL && target_type_qualifier == NULL &&
                type_ref_local != NULL && type_ref_local->name != NULL &&
                type_ref_local->name->count > 1)
            {
                QualifiedIdent *qid = type_ref_local->name;
                int prefix_count = qid->count - 1;
                char *prefix = qualified_ident_join_prefix(qid, prefix_count);
                const char *base = qid->segments[qid->count - 1];
                if (prefix != NULL && base != NULL)
                {
                    free(target_type_id);
                    target_type_id = strdup(base);
                    if (target_type_id == NULL)
                    {
                        target_type_id = NULL;
                        free(prefix);
                    }
                    else
                    {
                        target_type_qualifier = prefix;
                    }
                }
                else
                {
                    free(prefix);
                }
            }
            /* Handle generic specialization typecasts like specialize TArray<T>(Result).
             * Extract the base type name and build a TypeRef with args. */
            if (target_type == UNKNOWN_TYPE && target_type_id == NULL)
            {
                char *gen_base = NULL;
                ListNode_t *gen_args = NULL;
                if (extract_specialize_type_info(type_node, &gen_base, &gen_args))
                {
                    target_type_id = strdup(gen_base);
                    if (type_ref_local != NULL)
                        type_ref_free(type_ref_local);
                    type_ref_local = type_ref_from_name_and_args(gen_base, gen_args);
                    free(gen_base);
                    if (gen_args != NULL)
                        destroy_list(gen_args);
                }
            }
            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_CONSTRUCTED_TYPE)
            {
                char *gen_base = NULL;
                ListNode_t *gen_args = NULL;
                if (extract_constructed_type_info(type_node, &gen_base, &gen_args))
                {
                    target_type_id = strdup(gen_base);
                    if (type_ref_local != NULL)
                        type_ref_free(type_ref_local);
                    type_ref_local = type_ref_from_name_and_args(gen_base, gen_args);
                    free(gen_base);
                    if (gen_args != NULL) destroy_list(gen_args);
                }
            }
            if (type_ref_local == NULL && unwrapped_type != NULL)
            {
                char *gen_base = NULL;
                ListNode_t *gen_args = NULL;
                if (extract_constructed_type_info(type_node, &gen_base, &gen_args) ||
                    extract_specialize_type_info(type_node, &gen_base, &gen_args))
                {
                    if (type_ref_local != NULL)
                        type_ref_free(type_ref_local);
                    type_ref_local = type_ref_from_name_and_args(gen_base, gen_args);
                    free(gen_base);
                    if (gen_args != NULL) destroy_list(gen_args);
                }
            }
        }

        ListNode_t *args = NULL;
        int arg_count = 0;
        if (value_node != NULL)
        {
            args = convert_expression_list(value_node);
            arg_count = ListLength(args);
        }

        if (arg_count > 1)
        {
            char *call_id = target_type_id;
            target_type_id = NULL;
            struct Expression *call_expr = mk_functioncall(expr_node->line, call_id, args);
            if (call_expr != NULL)
                call_expr->expr_data.function_call_data.call_qualifier = target_type_qualifier;
            else if (target_type_qualifier != NULL)
                free(target_type_qualifier);
            target_type_qualifier = NULL;
            return call_expr;
        }

        struct Expression *inner_expr = NULL;
        if (arg_count == 1 && args != NULL)
        {
            inner_expr = (struct Expression *)args->cur;
            free(args);
        }
        else
        {
            if (args != NULL)
            {
                DestroyList(args);
                args = NULL;
            }
            inner_expr = convert_expression(value_node);
        }
        struct Expression *tc_expr = mk_typecast(expr_node->line, target_type, target_type_id, inner_expr);
        if (tc_expr != NULL)
        {
            tc_expr->expr_data.typecast_data.type_qualifier = target_type_qualifier;
            if (type_ref_local != NULL)
            {
                tc_expr->expr_data.typecast_data.target_type_ref = type_ref_local;
                type_ref_local = NULL;
            }
            else
            {
                tc_expr->expr_data.typecast_data.target_type_ref =
                    type_ref_from_qualifier_and_id(target_type_qualifier, target_type_id);
            }
        }
        else if (target_type_qualifier != NULL)
            free(target_type_qualifier);
        target_type_qualifier = NULL;
        if (type_ref_local != NULL)
            type_ref_free(type_ref_local);
        return tc_expr;
    }
    case PASCAL_T_DEREF:
    {
        struct Expression *result = mk_pointer_deref(expr_node->line, convert_expression(expr_node->child));
        return set_expr_source_index(result, expr_node);
    }
    case PASCAL_T_ADDR:
    {
        /* Resolve @specialize Type<T>.Method at conversion time while
         * preprocessed_source still points to the correct unit's buffer.
         * Once units are merged into the program tree the source indices
         * become invalid for the main program's buffer, so the semcheck
         * fallback (which re-parses source text) would fail. */
        ast_t *child = expr_node->child;
        if (child != NULL && child->sym != NULL && child->sym->name != NULL &&
            strcasecmp(child->sym->name, "specialize") == 0 &&
            preprocessed_source != NULL && preprocessed_length > 0 &&
            expr_node->index >= 0 && (size_t)expr_node->index < preprocessed_length)
        {
            size_t idx = (size_t)expr_node->index;
            /* Scan forward to find '@' on this line */
            size_t line_end = idx;
            while (line_end < preprocessed_length &&
                   preprocessed_source[line_end] != '\n' &&
                   preprocessed_source[line_end] != '\r' &&
                   preprocessed_source[line_end] != ';')
                line_end++;

            size_t at_pos = idx;
            for (size_t s = idx; s < line_end; ++s)
            {
                if (preprocessed_source[s] == '@') { at_pos = s; break; }
            }
            size_t i = at_pos;
            if (i < line_end && preprocessed_source[i] == '@')
                i++;
            while (i < line_end && (preprocessed_source[i] == ' ' || preprocessed_source[i] == '\t'))
                i++;
            /* Skip optional 'specialize' keyword */
            const char *kw = "specialize";
            size_t kw_len = 10;
            if (i + kw_len <= line_end && strncasecmp(preprocessed_source + i, kw, kw_len) == 0)
            {
                i += kw_len;
                while (i < line_end && (preprocessed_source[i] == ' ' || preprocessed_source[i] == '\t'))
                    i++;
            }
            /* Parse type name including angle brackets: Type<T> */
            size_t type_start = i;
            int angle_depth = 0;
            while (i < line_end)
            {
                char ch = preprocessed_source[i];
                if (ch == '<') angle_depth++;
                else if (ch == '>') { if (angle_depth > 0) angle_depth--; }
                else if (ch == '.' && angle_depth == 0) break;
                else if (ch == '\n' || ch == ';' || ch == '\r') break;
                i++;
            }
            if (i < line_end && preprocessed_source[i] == '.' && i > type_start)
            {
                /* Extract type name */
                size_t type_len = i - type_start;
                char *type_raw = (char *)malloc(type_len + 1);
                if (type_raw != NULL)
                {
                    memcpy(type_raw, preprocessed_source + type_start, type_len);
                    type_raw[type_len] = '\0';
                    i++; /* skip '.' */
                    /* Parse method name */
                    size_t method_start = i;
                    while (i < line_end && (isalnum((unsigned char)preprocessed_source[i]) || preprocessed_source[i] == '_'))
                        i++;
                    if (i > method_start)
                    {
                        size_t method_len = i - method_start;
                        char *method_id = (char *)malloc(method_len + 1);
                        if (method_id != NULL)
                        {
                            memcpy(method_id, preprocessed_source + method_start, method_len);
                            method_id[method_len] = '\0';
                            /* Mangle generic type: Type<T> -> Type$T */
                            char *lt = strchr(type_raw, '<');
                            char *mangled = NULL;
                            if (lt != NULL)
                            {
                                char *gt = strrchr(type_raw, '>');
                                if (gt != NULL && gt > lt)
                                {
                                    size_t base_len = (size_t)(lt - type_raw);
                                    char args_buf[256];
                                    size_t args_len = 0;
                                    int adepth = 0;
                                    for (const char *p = lt + 1; p < gt; ++p)
                                    {
                                        char ac = *p;
                                        if (ac == '<') { adepth++; continue; }
                                        if (ac == '>') { if (adepth > 0) adepth--; continue; }
                                        if (adepth == 0 && ac == ',') ac = '$';
                                        if (adepth == 0 && ac == ' ') continue;
                                        if (args_len + 1 < sizeof(args_buf))
                                            args_buf[args_len++] = ac;
                                    }
                                    args_buf[args_len] = '\0';
                                    size_t out_len = base_len + 1 + args_len;
                                    mangled = (char *)malloc(out_len + 1);
                                    if (mangled != NULL)
                                        snprintf(mangled, out_len + 1, "%.*s$%s", (int)base_len, type_raw, args_buf);
                                }
                            }
                            if (mangled == NULL)
                                mangled = strdup(type_raw);

                            if (mangled != NULL)
                            {
                                /* Rewrite: @(specialize) -> @(MangledType.Method) */
                                struct Expression *base_expr = mk_varid(expr_node->line, mangled);
                                struct Expression *access = mk_recordaccess(expr_node->line, base_expr, method_id);
                                if (access != NULL)
                                {
                                    access->is_specialize_addr_target = 1;
                                    struct Expression *result = mk_addressof(expr_node->line, access);
                                    free(type_raw);
                                    return set_expr_source_index(result, expr_node);
                                }
                                /* access creation failed; fall through */
                                destroy_expr(base_expr);
                            }
                            else
                            {
                                free(method_id);
                            }
                        }
                    }
                    free(type_raw);
                }
            }
        }
        struct Expression *result = mk_addressof(expr_node->line, convert_expression(expr_node->child));
        return set_expr_source_index(result, expr_node);
    }
    case PASCAL_T_ANONYMOUS_FUNCTION:
    {
        /* Anonymous function: params -> return_type -> body 
         * Structure from parser:
         * - If params exist: child is first PARAM, params are linked via ->next
         * - Return type is in a sibling after all params
         * - Body is after return type
         */
        char *generated_name = generate_anonymous_method_name(1);
        if (generated_name == NULL) {
            fprintf(stderr, "ERROR: Failed to generate name for anonymous function at line %d\n", expr_node->line);
            return NULL;
        }
        
        /* Navigate the AST structure */
        ast_t *current = expr_node->child;
        ast_t *params_start = NULL;
        ast_t *return_type_node = NULL;
        ast_t *body_node = NULL;
        
        /* Check if first child is a PARAM (parameters exist) or RETURN_TYPE (no params) */
        if (current != NULL) {
            if (current->typ == PASCAL_T_PARAM) {
                /* We have parameters - collect them all */
                params_start = current;
                /* Find the last PARAM in the chain */
                while (current != NULL && current->typ == PASCAL_T_PARAM) {
                    current = current->next;
                }
                /* Now current should be RETURN_TYPE */
                if (current != NULL && current->typ == PASCAL_T_RETURN_TYPE) {
                    return_type_node = current;
                    body_node = current->next;
                }
            } else if (current->typ == PASCAL_T_RETURN_TYPE) {
                /* No parameters, directly to return type */
                return_type_node = current;
                body_node = current->next;
            }
        }
        
        /* Convert parameters */
        ListNode_t *parameters = NULL;
        if (params_start != NULL) {
            ast_t *param_cursor = params_start;
            parameters = convert_param_list(&param_cursor);
        }
        
        /* Convert return type */
        int return_type = UNKNOWN_TYPE;
        char *return_type_id = NULL;
        if (return_type_node != NULL && return_type_node->child != NULL) {
            if (return_type_node->child->sym != NULL && return_type_node->child->sym->name != NULL) {
                return_type_id = strdup(return_type_node->child->sym->name);
            }
        }
        
        /* Convert body */
        struct Statement *body = NULL;
        if (body_node != NULL) {
            body = convert_statement(body_node);
        }
        
        return mk_anonymous_function(expr_node->line, generated_name, parameters, 
                                      return_type, return_type_id, body);
    }
    case PASCAL_T_ANONYMOUS_PROCEDURE:
    {
        /* Anonymous procedure: params -> body
         * Structure from parser (same as anonymous function but without return type):
         * - If params exist: child is first PARAM, params are linked via ->next
         * - Body is after all params (or first child if no params)
         */
        char *generated_name = generate_anonymous_method_name(0);
        if (generated_name == NULL) {
            fprintf(stderr, "ERROR: Failed to generate name for anonymous procedure at line %d\n", expr_node->line);
            return NULL;
        }
        
        /* Navigate the AST structure */
        ast_t *current = expr_node->child;
        ast_t *params_start = NULL;
        ast_t *body_node = NULL;
        
        /* Check if first child is a PARAM (parameters exist) or a statement (no params) */
        if (current != NULL) {
            /* Skip NONE nodes */
            while (current != NULL && current->typ == PASCAL_T_NONE) {
                current = current->child;
            }
            
            if (current != NULL && current->typ == PASCAL_T_PARAM) {
                /* We have parameters - collect them all */
                params_start = current;
                /* Find the last PARAM in the chain */
                while (current != NULL && current->typ == PASCAL_T_PARAM) {
                    current = current->next;
                }
                /* Now current should be the body */
                body_node = current;
            } else {
                /* No parameters, directly to body */
                body_node = current;
            }
        }
        
        /* Convert parameters */
        ListNode_t *parameters = NULL;
        if (params_start != NULL) {
            ast_t *param_cursor = params_start;
            parameters = convert_param_list(&param_cursor);
        }
        
        /* Convert body */
        struct Statement *body = NULL;
        if (body_node != NULL) {
            body = convert_statement(body_node);
            if (body == NULL) {
                fprintf(stderr, "ERROR: Failed to convert anonymous procedure body at line %d\n", 
                        expr_node->line);
                if (generated_name) free(generated_name);
                if (parameters) destroy_list(parameters);
                return NULL;
            }
        } else {
            fprintf(stderr, "WARNING: Anonymous procedure has no body at line %d\n", expr_node->line);
        }
        
        return mk_anonymous_procedure(expr_node->line, generated_name, parameters, body);
    }
    default: {
        const char *name = tag_name(expr_node->typ);
        fprintf(stderr, "ERROR: unsupported expression tag %d (%s) at line %d.\n",
                expr_node->typ, name, expr_node->line);
        break;
    }
    }

    return NULL;
}

static ListNode_t *convert_expression_list(ast_t *arg_node) {
    ListBuilder builder;
    list_builder_init(&builder);
    
    /* Pre-check: verify the next chain doesn't have cycles using Floyd's algorithm.
     * If a cycle is found, break it by setting the back-edge to NULL.
     * This fixes cycles introduced by the parser's suffix chain construction
     * (e.g., typecast-then-call patterns like TProc(x)(args)). */
    {
        ast_t *slow = arg_node, *fast = arg_node;
        int has_cycle = 0;
        while (fast != NULL && fast != ast_nil && fast->next != NULL && fast->next != ast_nil) {
            slow = slow->next;
            fast = fast->next->next;
            if (slow == fast) {
                has_cycle = 1;
                break;
            }
        }
        if (has_cycle) {
            /* Find cycle start using Floyd's phase 2 */
            slow = arg_node;
            while (slow != fast) { slow = slow->next; fast = fast->next; }
            /* Walk around the cycle to find the back-edge */
            ast_t *cycle_prev = slow;
            ast_t *cycle_cur = slow->next;
            while (cycle_cur != slow) {
                cycle_prev = cycle_cur;
                cycle_cur = cycle_cur->next;
            }
            /* Break the cycle */
            cycle_prev->next = NULL;
        }
    }
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for expression list traversal\n");
        return NULL;
    }
    
    /* Pre-check: verify the next chain doesn't have cycles using Floyd's algorithm */
    {
        ast_t *slow = arg_node, *fast = arg_node;
        int has_cycle = 0;
        while (fast != NULL && fast != ast_nil && fast->next != NULL && fast->next != ast_nil) {
            slow = slow->next;
            fast = fast->next->next;
            if (slow == fast) {
                has_cycle = 1;
                break;
            }
        }
        if (has_cycle) {
            fprintf(stderr, "PRE-CHECK: Cycle detected in expression list next-chain starting at %p\n", (void*)arg_node);
            /* Find cycle start */
            slow = arg_node;
            while (slow != fast) { slow = slow->next; fast = fast->next; }
            fprintf(stderr, "  Cycle starts at: %p type=%d line=%d sym=%s\n", (void*)slow,
                    slow->typ, slow->line, (slow->sym && slow->sym->name) ? slow->sym->name : "?");
            /* Just dump first few nodes */
            ast_t *d = arg_node;
            for (int i = 0; i < 10 && d != NULL && d != ast_nil; i++) {
                fprintf(stderr, "  [%d] %p type=%d line=%d sym=%s%s\n", i, (void*)d,
                        d->typ, d->line, (d->sym && d->sym->name) ? d->sym->name : "?",
                        d == slow ? " <-- CYCLE START" : "");
                d = d->next;
                if (d == slow && i > 0) break;
            }
        }
    }
    
    ast_t *cur = arg_node;

    /*
     * cparser often wraps Pascal arguments in synthetic tuple/statement nodes. Unwrap each
     * layer so we only convert real expressions and preserve field-width wrappers explicitly.
     */
    while (cur != NULL && cur != ast_nil) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in expression list, stopping traversal\n");
            break;
        }
        
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FIELD_WIDTH) {
            struct Expression *expr = convert_field_width_expr(unwrapped);
            if (expr != NULL)
                list_builder_append(&builder, expr, LIST_EXPR);
        } else if (unwrapped != NULL) {
            struct Expression *expr = convert_expression(unwrapped);
            if (expr != NULL)
                list_builder_append(&builder, expr, LIST_EXPR);
        }
        cur = cur->next;
    }

    visited_set_destroy(visited);
    return list_builder_finish(&builder);
}

static struct Expression *convert_field_width_expr(ast_t *field_width_node) {
    if (field_width_node == NULL)
        return NULL;

    ast_t *cursor = field_width_node;
    struct Expression *formats[2] = { NULL, NULL };
    int format_count = 0;

    while (cursor != NULL && cursor->typ == PASCAL_T_FIELD_WIDTH) {
        ast_t *base_node = cursor->child;
        ast_t *format_node = (base_node != NULL) ? base_node->next : NULL;

        if (format_node != NULL && format_node != ast_nil) {
            struct Expression *format_expr = convert_expression(format_node);
            if (format_expr != NULL) {
                if (format_count < 2) {
                    formats[format_count++] = format_expr;
                } else {
                    destroy_expr(formats[0]);
                    formats[0] = formats[1];
                    formats[1] = format_expr;
                    format_count = 2;
                }
            }
        }

        cursor = base_node;
    }

    struct Expression *base_expr = convert_expression(cursor);
    if (base_expr == NULL) {
        for (int i = 0; i < format_count; ++i) {
            if (formats[i] != NULL)
                destroy_expr(formats[i]);
        }
        return NULL;
    }

    if (format_count >= 1) {
        struct Expression *width_expr = formats[format_count - 1];
        if (base_expr->field_width == NULL)
            base_expr->field_width = width_expr;
        else if (width_expr != NULL)
            destroy_expr(width_expr);
    }

    if (format_count >= 2) {
        base_expr->field_precision = formats[format_count - 2];
    }

    return base_expr;
}

static struct Expression *convert_member_access_chain(int line,
    struct Expression *base_expr, ast_t *field_node);

static struct Expression *convert_member_access(ast_t *node) {
    if (node == NULL)
        return NULL;

    ast_t *base_node = node->child;
    ast_t *field_node = (base_node != NULL) ? base_node->next : NULL;
    ast_t *args_node = (field_node != NULL) ? field_node->next : NULL;
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL &&
        node != NULL && (node->line == 255 || node->line == 256))
    {
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] MEMBER_ACCESS line=%d base=%d(%s:%s) field=%d(%s:%s) args=%d(%s:%s)\n",
            node->line,
            base_node != NULL ? base_node->typ : -1,
            base_node != NULL ? pascal_tag_to_string(base_node->typ) : "<null>",
            (base_node != NULL && base_node->sym != NULL && base_node->sym->name != NULL)
                ? base_node->sym->name : "<null>",
            field_node != NULL ? field_node->typ : -1,
            field_node != NULL ? pascal_tag_to_string(field_node->typ) : "<null>",
            (field_node != NULL && field_node->sym != NULL && field_node->sym->name != NULL)
                ? field_node->sym->name : "<null>",
            args_node != NULL ? args_node->typ : -1,
            args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>",
            (args_node != NULL && args_node->sym != NULL && args_node->sym->name != NULL)
                ? args_node->sym->name : "<null>");
        for (ast_t *it = field_node != NULL ? field_node->child : NULL; it != NULL; it = it->next)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   field.child typ=%d(%s:%s) grandchild=%d(%s)\n",
                it->typ,
                pascal_tag_to_string(it->typ),
                (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>",
                it->child != NULL ? it->child->typ : -1,
                it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
        }
    }
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL)
    {
        const char *field_sym = (field_node != NULL && field_node->sym != NULL &&
                                 field_node->sym->name != NULL)
            ? field_node->sym->name : "<null>";
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] MEMBER_ACCESS base=%d(%s) field=%d(%s:%s) args=%d(%s)\n",
            base_node != NULL ? base_node->typ : -1,
            base_node != NULL ? pascal_tag_to_string(base_node->typ) : "<null>",
            field_node != NULL ? field_node->typ : -1,
            field_node != NULL ? pascal_tag_to_string(field_node->typ) : "<null>",
            field_sym,
            args_node != NULL ? args_node->typ : -1,
            args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>");
        if (field_node != NULL && field_node->typ == PASCAL_T_FUNC_CALL && field_node->child != NULL)
        {
            ast_t *fc = field_node->child;
            const char *fc_sym = (fc->sym != NULL && fc->sym->name != NULL) ? fc->sym->name : "<null>";
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   MEMBER_ACCESS func child=%d(%s:%s)\n",
                fc->typ, pascal_tag_to_string(fc->typ), fc_sym);
            if (fc->child != NULL)
            {
                const char *fc2_sym = (fc->child->sym != NULL && fc->child->sym->name != NULL)
                    ? fc->child->sym->name : "<null>";
                fprintf(stderr,
                    "[KGPC_DEBUG_SPECIALIZE_CALLS]   MEMBER_ACCESS func child->child=%d(%s:%s)\n",
                    fc->child->typ, pascal_tag_to_string(fc->child->typ), fc2_sym);
            }
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_member_access: base=%d field=%d args=%d\n",
            base_node ? base_node->typ : -1,
            field_node ? field_node->typ : -1,
            args_node ? args_node->typ : -1);
    }

    /* Check for function call (MEMBER_ACCESS with ARG_LIST as 3rd child) */
    if (args_node != NULL && args_node->typ == PASCAL_T_ARG_LIST) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access: detected ARG_LIST child, converting to function call\n");
        }
        
        struct Expression *base_expr = convert_expression(base_node);
        char *method_id = dup_symbol(field_node);
        
        /* Convert args */
        ListNode_t *args_list = NULL;
        ListNode_t *tail = NULL;
        ast_t *arg_child = args_node->child;
        while (arg_child != NULL) {
            struct Expression *arg_expr = convert_expression(arg_child);
            if (arg_expr != NULL) {
                ListNode_t *node = CreateListNode(arg_expr, LIST_EXPR);
                if (args_list == NULL) {
                    args_list = node;
                    tail = node;
                } else {
                    tail->next = node;
                    tail = node;
                }
            }
            arg_child = arg_child->next;
        }
        
        /* Prepend base expression to args */
        args_list = PushListNodeFront(args_list, CreateListNode(base_expr, LIST_EXPR));
        
        struct Expression *call_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
        call_expr->line_num = node->line;
        call_expr->type = EXPR_FUNCTION_CALL;
        call_expr->expr_data.function_call_data.id = method_id;
        call_expr->expr_data.function_call_data.args_expr = args_list;
        call_expr->expr_data.function_call_data.resolved_func = NULL;
        call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
        call_expr->expr_data.function_call_data.placeholder_method_name = strdup(method_id);

        return call_expr;
    }

    struct Expression *record_expr = convert_expression(base_node);
    if (record_expr == NULL)
        return NULL;

    return convert_member_access_chain(node->line, record_expr, field_node);
}

static struct Expression *convert_member_access_chain(int line,
    struct Expression *base_expr, ast_t *field_node) {
    if (base_expr == NULL)
        return NULL;

    if (field_node == NULL) {
        destroy_expr(base_expr);
        return NULL;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_member_access_chain: field_node typ=%d, has_next=%d\n",
            field_node->typ, field_node->next != NULL);
        if (field_node->next != NULL) {
            fprintf(stderr, "[KGPC]   next typ=%d\n", field_node->next->typ);
        }
        if (field_node->child != NULL) {
            fprintf(stderr, "[KGPC]   child typ=%d\n", field_node->child->typ);
            if (field_node->child->next != NULL) {
                fprintf(stderr, "[KGPC]   child->next typ=%d\n", field_node->child->next->typ);
            }
        }
    }

    ast_t *unwrapped = unwrap_pascal_node(field_node);
    if (unwrapped == NULL)
        unwrapped = field_node;

    /* Check if this is PASCAL_T_FUNC_CALL (type 43) */
    if (unwrapped != NULL && unwrapped->typ == PASCAL_T_FUNC_CALL) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access_chain: detected FUNC_CALL, converting to method call\n");
        }
        
        ast_t *method_id_node = unwrapped->child;
        ast_t *args_node = (method_id_node != NULL) ? method_id_node->next : NULL;
        
        char *method_id = NULL;
        char *placeholder_name = NULL;
        if (method_id_node != NULL) {
            ast_t *method_unwrapped = unwrap_pascal_node(method_id_node);
            if (method_unwrapped == NULL)
                method_unwrapped = method_id_node;
            if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL &&
                method_unwrapped != NULL &&
                method_unwrapped->typ == PASCAL_T_IDENTIFIER &&
                method_unwrapped->sym != NULL &&
                method_unwrapped->sym->name != NULL &&
                strcasecmp(method_unwrapped->sym->name, "specialize") == 0)
            {
                fprintf(stderr,
                    "[KGPC_DEBUG_SPECIALIZE_CALLS] METHOD specialize-shape line=%d base_expr_type=%d args_node=%d(%s)\n",
                    line,
                    base_expr != NULL ? base_expr->type : -1,
                    args_node != NULL ? args_node->typ : -1,
                    args_node != NULL ? pascal_tag_to_string(args_node->typ) : "<null>");
                for (ast_t *dbg = args_node, *it = dbg; it != NULL; it = it->next)
                {
                    const char *sym = (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>";
                    fprintf(stderr,
                        "[KGPC_DEBUG_SPECIALIZE_CALLS]   method-arg node typ=%d(%s) sym=%s child=%d(%s)\n",
                        it->typ, pascal_tag_to_string(it->typ), sym,
                        it->child != NULL ? it->child->typ : -1,
                        it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
                }
            }
            if (method_unwrapped->typ == PASCAL_T_IDENTIFIER) {
                method_id = dup_symbol(method_unwrapped);
                if (method_id != NULL)
                    placeholder_name = strdup(method_id);
            } else if (method_unwrapped->typ == PASCAL_T_CONSTRUCTED_TYPE) {
                char *base_name = NULL;
                ListNode_t *type_args = NULL;
                if (extract_constructed_type_info(method_unwrapped, &base_name, &type_args)) {
                    if (base_name != NULL) {
                        method_id = mangle_specialized_name_from_list(base_name, type_args);
                        if (method_id == NULL)
                            method_id = strdup(base_name);
                        placeholder_name = strdup(base_name);
                    }
                }
                if (base_name != NULL)
                    free(base_name);
                if (type_args != NULL)
                    destroy_list(type_args);
            }
        }

        if (method_id != NULL) {
            
            /* Convert args - handle both ARG_LIST and direct siblings */
            ListNode_t *args_list = NULL;
            if (args_node != NULL && args_node->typ == PASCAL_T_ARG_LIST) {
                /* Arguments wrapped in ARG_LIST node - iterate through children */
                ast_t *arg_child = args_node->child;
                ListNode_t *tail = NULL;
                while (arg_child != NULL) {
                    struct Expression *arg_expr = convert_expression(arg_child);
                    if (arg_expr != NULL) {
                        ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                        if (args_list == NULL) {
                            args_list = new_node;
                            tail = new_node;
                        } else {
                            if (tail == NULL) {
                                // Find tail if we're appending to existing list
                                tail = args_list;
                                while (tail->next != NULL) tail = tail->next;
                            }
                            tail->next = new_node;
                            tail = new_node;
                        }
                    }
                    arg_child = arg_child->next;
                }
            } else if (args_node != NULL) {
                /* Arguments as direct siblings - iterate through all siblings */
                ast_t *arg_child = args_node;
                ListNode_t *tail = NULL;
                while (arg_child != NULL) {
                    struct Expression *arg_expr = convert_expression(arg_child);
                    if (arg_expr != NULL) {
                        ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                        if (args_list == NULL) {
                            args_list = new_node;
                            tail = new_node;
                        } else {
                            if (tail == NULL) {
                                tail = args_list;
                                while (tail->next != NULL) tail = tail->next;
                            }
                            tail->next = new_node;
                            tail = new_node;
                        }
                    }
                    arg_child = arg_child->next;
                }
            }
            /* If args_node is NULL, args_list remains NULL (zero arguments) */
            
            /* Prepend base expression to args */
            ListNode_t *base_node_list = CreateListNode(base_expr, LIST_EXPR);
            if (args_list == NULL) {
                args_list = base_node_list;
            } else {
                args_list = PushListNodeFront(args_list, base_node_list);
            }
            
            struct Expression *call_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
            call_expr->line_num = line;
            call_expr->type = EXPR_FUNCTION_CALL;
            call_expr->expr_data.function_call_data.id = method_id;
            call_expr->expr_data.function_call_data.args_expr = args_list;
            call_expr->expr_data.function_call_data.resolved_func = NULL;
            call_expr->resolved_kgpc_type = NULL;
            call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
            call_expr->expr_data.function_call_data.placeholder_method_name =
                (placeholder_name != NULL) ? placeholder_name : strdup(method_id);
            placeholder_name = NULL;

            return call_expr;
        }
        if (placeholder_name != NULL)
            free(placeholder_name);
    }

    /* Check if field_node has ARG_LIST as sibling (method call) */
    if (unwrapped != NULL && unwrapped->next != NULL && unwrapped->next->typ == PASCAL_T_ARG_LIST) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access_chain: detected ARG_LIST sibling, converting to function call\n");
        }
        
        char *method_id = dup_symbol(unwrapped);
        
        /* Convert args from ARG_LIST */
        ListNode_t *args_list = NULL;
        ListNode_t *tail = NULL;
        ast_t *arg_child = unwrapped->next->child;
        while (arg_child != NULL) {
            struct Expression *arg_expr = convert_expression(arg_child);
            if (arg_expr != NULL) {
                ListNode_t *node = CreateListNode(arg_expr, LIST_EXPR);
                if (args_list == NULL) {
                    args_list = node;
                    tail = node;
                } else {
                    tail->next = node;
                    tail = node;
                }
            }
            arg_child = arg_child->next;
        }
        
        /* Prepend base expression to args */
        args_list = PushListNodeFront(args_list, CreateListNode(base_expr, LIST_EXPR));
        
        struct Expression *call_expr = (struct Expression *)calloc(1, sizeof(struct Expression));
        call_expr->line_num = line;
        call_expr->type = EXPR_FUNCTION_CALL;
        call_expr->expr_data.function_call_data.id = method_id;
        call_expr->expr_data.function_call_data.args_expr = args_list;
        call_expr->expr_data.function_call_data.resolved_func = NULL;
        call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
        call_expr->expr_data.function_call_data.placeholder_method_name = strdup(method_id);

        return call_expr;
    }

    if (unwrapped == NULL)
        unwrapped = field_node;

    if (unwrapped == NULL) {
        destroy_expr(base_expr);
        return NULL;
    }

    int node_line = (unwrapped->line != 0) ? unwrapped->line : line;

    switch (unwrapped->typ) {
    case PASCAL_T_TYPECAST: {
        /* Static/generic class method syntax may be encoded as a TYPECAST
         * in the member field position:
         *   TMarshal.specialize FixArray<T>(Arr)
         * => MEMBER_ACCESS(base=TMarshal, field=TYPECAST(CONSTRUCTED_TYPE(FixArray<T>), Arr))
         */
        ast_t *cast_type_node = unwrapped->child;
        ast_t *cast_value_node = (cast_type_node != NULL) ? cast_type_node->next : NULL;
        ast_t *cast_type = unwrap_pascal_node(cast_type_node);
        if (cast_type == NULL)
            cast_type = cast_type_node;

        char *method_id = NULL;
        char *placeholder_name = NULL;
        if (cast_type != NULL && cast_type->typ == PASCAL_T_IDENTIFIER) {
            method_id = dup_symbol(cast_type);
            if (method_id != NULL)
                placeholder_name = strdup(method_id);
        } else if (cast_type != NULL && cast_type->typ == PASCAL_T_CONSTRUCTED_TYPE) {
            char *base_name = NULL;
            ListNode_t *type_args = NULL;
            if (extract_constructed_type_info(cast_type, &base_name, &type_args)) {
                if (base_name != NULL) {
                    method_id = mangle_specialized_name_from_list(base_name, type_args);
                    if (method_id == NULL)
                        method_id = strdup(base_name);
                    placeholder_name = strdup(base_name);
                }
            }
            if (base_name != NULL)
                free(base_name);
            if (type_args != NULL)
                destroy_list(type_args);
        }

        if (method_id == NULL) {
            destroy_expr(base_expr);
            return NULL;
        }

        ListNode_t *args_list = NULL;
        ListNode_t *tail = NULL;
        if (cast_value_node != NULL && cast_value_node->typ == PASCAL_T_ARG_LIST) {
            for (ast_t *arg = cast_value_node->child; arg != NULL; arg = arg->next) {
                struct Expression *arg_expr = convert_expression(arg);
                if (arg_expr == NULL)
                    continue;
                ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                if (new_node == NULL)
                {
                    destroy_expr(arg_expr);
                    continue;
                }
                if (args_list == NULL) {
                    args_list = new_node;
                    tail = new_node;
                } else {
                    tail->next = new_node;
                    tail = new_node;
                }
            }
        } else if (cast_value_node != NULL) {
            /* Convert all argument expressions (siblings linked via ->next).
             * With sep_by in the parser, multi-argument specialize calls
             * produce sibling nodes rather than a single expression. */
            for (ast_t *arg = cast_value_node; arg != NULL; arg = arg->next) {
                struct Expression *arg_expr = convert_expression(arg);
                if (arg_expr == NULL)
                    continue;
                ListNode_t *new_node = CreateListNode(arg_expr, LIST_EXPR);
                if (new_node == NULL)
                {
                    destroy_expr(arg_expr);
                    continue;
                }
                if (args_list == NULL) {
                    args_list = new_node;
                    tail = new_node;
                } else {
                    tail->next = new_node;
                    tail = new_node;
                }
            }
        }

        args_list = PushListNodeFront(args_list, CreateListNode(base_expr, LIST_EXPR));
        struct Expression *call_expr = mk_functioncall(line, method_id, args_list);
        if (call_expr != NULL) {
            call_expr->expr_data.function_call_data.is_method_call_placeholder = 1;
            call_expr->expr_data.function_call_data.placeholder_method_name =
                (placeholder_name != NULL) ? placeholder_name : strdup(method_id);
            placeholder_name = NULL;
        }
        if (placeholder_name != NULL)
            free(placeholder_name);
        return call_expr;
    }
    case PASCAL_T_IDENTIFIER: {
        char *field_id = dup_symbol(unwrapped);
        if (field_id == NULL) {
            destroy_expr(base_expr);
            return NULL;
        }
        return mk_recordaccess(node_line, base_expr, field_id);
    }
    case PASCAL_T_BOOLEAN: {
        /* Handle scoped enum literals like TUseBoolStrs.False or TUseBoolStrs.True
         * The parser treats 'False' and 'True' as boolean keywords, but in the context
         * of a member access (after a dot), they should be treated as identifiers
         * that reference enum literals. */
        char *field_id = dup_symbol(unwrapped);
        if (field_id == NULL) {
            destroy_expr(base_expr);
            return NULL;
        }
        return mk_recordaccess(node_line, base_expr, field_id);
    }
    case PASCAL_T_ARRAY_ACCESS: {
        ast_t *array_base = unwrapped->child;
        ast_t *index_node = (array_base != NULL) ? array_base->next : NULL;
        struct Expression *field_expr = convert_member_access_chain(node_line, base_expr, array_base);
        if (field_expr == NULL)
            return NULL;

        struct Expression *index_expr = convert_expression(index_node);
        if (index_expr == NULL) {
            destroy_expr(field_expr);
            return NULL;
        }

        struct Expression *result = mk_arrayaccess(node_line, field_expr, index_expr);
        if (result == NULL)
            return NULL;

        if (index_node != NULL && index_node->next != NULL)
        {
            ast_t *extra_idx_node = index_node->next;
            ListNode_t *extra_indices = NULL;
            ListNode_t *extra_tail = NULL;
            while (extra_idx_node != NULL)
            {
                struct Expression *extra_idx = convert_expression(extra_idx_node);
                if (extra_idx != NULL)
                {
                    ListNode_t *new_node = CreateListNode(extra_idx, LIST_EXPR);
                    if (new_node != NULL)
                    {
                        if (extra_tail == NULL)
                        {
                            extra_indices = new_node;
                            extra_tail = new_node;
                        }
                        else
                        {
                            extra_tail->next = new_node;
                            extra_tail = new_node;
                        }
                    }
                }
                extra_idx_node = extra_idx_node->next;
            }
            result->expr_data.array_access_data.extra_indices = extra_indices;
        }

        return result;
    }
    case PASCAL_T_MEMBER_ACCESS: {
        ast_t *inner_base = unwrapped->child;
        ast_t *inner_field = (inner_base != NULL) ? inner_base->next : NULL;
        struct Expression *next_base = convert_member_access_chain(node_line, base_expr, inner_base);
        if (next_base == NULL)
            return NULL;
        return convert_member_access_chain(node_line, next_base, inner_field);
    }
    case PASCAL_T_DEREF: {
        ast_t *inner = unwrapped->child;
        struct Expression *target = convert_member_access_chain(node_line, base_expr, inner);
        if (target == NULL)
            return NULL;
        return mk_pointer_deref(node_line, target);
    }
    default:
        break;
    }

    if (unwrapped->child != NULL) {
        ast_t *child_id = unwrap_pascal_node(unwrapped->child);
        if (child_id != NULL && child_id->typ == PASCAL_T_IDENTIFIER) {
            char *field_id = dup_symbol(child_id);
            if (field_id == NULL) {
                destroy_expr(base_expr);
                return NULL;
            }
            return mk_recordaccess(node_line, base_expr, field_id);
        }
    }

    destroy_expr(base_expr);
    return NULL;
}

static struct Statement *convert_assignment(ast_t *assign_node) {
    ast_t *lhs = assign_node->child;
    ast_t *rhs = lhs != NULL ? lhs->next : NULL;

    struct Expression *left = convert_expression(lhs);
    struct Expression *right = convert_expression(rhs);
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL && right == NULL)
    {
        ast_t *u_rhs = unwrap_pascal_node(rhs);
        ast_t *dbg_rhs = (u_rhs != NULL) ? u_rhs : rhs;
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] ASSIGN rhs-convert-null line=%d lhs_typ=%d rhs_typ=%d(%s) rhs_sym=%s rhs_child_typ=%d(%s)\n",
            assign_node != NULL ? assign_node->line : -1,
            lhs != NULL ? lhs->typ : -1,
            dbg_rhs != NULL ? dbg_rhs->typ : -1,
            dbg_rhs != NULL ? pascal_tag_to_string(dbg_rhs->typ) : "<null>",
            (dbg_rhs != NULL && dbg_rhs->sym != NULL && dbg_rhs->sym->name != NULL)
                ? dbg_rhs->sym->name : "<null>",
            (dbg_rhs != NULL && dbg_rhs->child != NULL) ? dbg_rhs->child->typ : -1,
            (dbg_rhs != NULL && dbg_rhs->child != NULL)
                ? pascal_tag_to_string(dbg_rhs->child->typ) : "<null>");
    }
    if (kgpc_getenv("KGPC_DEBUG_SPECIALIZE_CALLS") != NULL &&
        assign_node != NULL && assign_node->line == 256)
    {
        ast_t *u_rhs = unwrap_pascal_node(rhs);
        ast_t *dbg_rhs = (u_rhs != NULL) ? u_rhs : rhs;
        fprintf(stderr,
            "[KGPC_DEBUG_SPECIALIZE_CALLS] ASSIGN line=256 rhs=%d(%s:%s)\n",
            dbg_rhs != NULL ? dbg_rhs->typ : -1,
            dbg_rhs != NULL ? pascal_tag_to_string(dbg_rhs->typ) : "<null>",
            (dbg_rhs != NULL && dbg_rhs->sym != NULL && dbg_rhs->sym->name != NULL)
                ? dbg_rhs->sym->name : "<null>");
        for (ast_t *it = rhs != NULL ? rhs->next : NULL; it != NULL; it = it->next)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   ASSIGN raw rhs.next typ=%d(%s:%s) child=%d(%s)\n",
                it->typ,
                pascal_tag_to_string(it->typ),
                (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>",
                it->child != NULL ? it->child->typ : -1,
                it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
        }
        for (ast_t *it = dbg_rhs != NULL ? dbg_rhs->next : NULL; it != NULL; it = it->next)
        {
            fprintf(stderr,
                "[KGPC_DEBUG_SPECIALIZE_CALLS]   ASSIGN rhs.next typ=%d(%s:%s) child=%d(%s)\n",
                it->typ,
                pascal_tag_to_string(it->typ),
                (it->sym != NULL && it->sym->name != NULL) ? it->sym->name : "<null>",
                it->child != NULL ? it->child->typ : -1,
                it->child != NULL ? pascal_tag_to_string(it->child->typ) : "<null>");
        }
    }
    struct Statement *stmt = mk_varassign(assign_node->line, assign_node->col, left, right);
    if (stmt != NULL && assign_node->index >= 0) {
        stmt->source_index = assign_node->index + g_source_offset;
    }
    return stmt;
}

static struct Statement *convert_proc_call(ast_t *call_node, bool implicit_identifier) {
    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_proc_call: typ=%d line=%d\n",
            call_node ? call_node->typ : -1, call_node ? call_node->line : -1);
        if (call_node && call_node->child) {
            fprintf(stderr, "[KGPC]   child typ=%d\n", call_node->child->typ);
        }
    }
    ast_t *child = call_node->child;
    ast_t *args_start = NULL;
    char *id = NULL;

    if (child != NULL && child->typ == PASCAL_T_MEMBER_ACCESS) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   Handling MEMBER_ACCESS\n");
        }
        /* Check for unit-qualified procedure calls (e.g. System.SetLength).
         * If the left side is a known unit name, create a direct procedure call
         * with call_qualifier set instead of a method call placeholder. */
        ast_t *lhs = child->child;
        ast_t *rhs = (lhs != NULL) ? lhs->next : NULL;
        if (lhs != NULL && lhs->typ == PASCAL_T_IDENTIFIER && rhs != NULL)
        {
            char *lhs_name = dup_symbol(lhs);
            if (lhs_name != NULL && unit_registry_contains(lhs_name))
            {
                /* Extract the procedure name from rhs */
                ast_t *method_ident = unwrap_pascal_node(rhs);
                ast_t *call_args = child->next;
                if (method_ident != NULL && method_ident->typ == PASCAL_T_FUNC_CALL)
                {
                    call_args = method_ident->child ? method_ident->child->next : NULL;
                    method_ident = method_ident->child;
                    if (method_ident != NULL)
                        method_ident = unwrap_pascal_node(method_ident);
                }
                if (method_ident != NULL && method_ident->typ == PASCAL_T_IDENTIFIER)
                {
                    char *proc_name = dup_symbol(method_ident);
                    ListNode_t *args = convert_expression_list(call_args);
                    struct Statement *call = mk_procedurecall(call_node->line, proc_name, args);
                    if (call != NULL) {
                        if (call_node->index >= 0)
                            call->source_index = call_node->index + g_source_offset;
                        call->stmt_data.procedure_call_data.call_qualifier = lhs_name;
                    } else {
                        free(lhs_name);
                    }
                    return call;
                }
            }
            free(lhs_name);
        }
        ast_t *args_node = child->next;
        struct Statement *method_stmt = convert_method_call_statement(child, args_node);
        if (method_stmt != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   convert_method_call_statement returned statement\n");
            }
            return method_stmt;
        }
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   convert_method_call_statement returned NULL\n");
        }
    }

    if (call_node->typ == PASCAL_T_IDENTIFIER) {
        id = dup_symbol(call_node);
        args_start = call_node->next;
    } else if (child != NULL) {
        if (implicit_identifier && child->typ == PASCAL_T_IDENTIFIER) {
            id = dup_symbol(child);
            args_start = child->next;
        } else {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child);
                args_start = child->next;
            } else if (child->typ == PASCAL_T_TYPECAST && child->child != NULL &&
                       child->child->typ == PASCAL_T_IDENTIFIER) {
                /* Handle chained call pattern: TypeName(source)(args)
                 * Parser creates: FUNC_CALL -> TYPECAST(TypeName, source) -> outer args
                 * child is TYPECAST with: child->child = IDENTIFIER(TypeName), rest = source args
                 * call_node has: child->next = outer call args
                 * We need to preserve both: create a procedure call where the first arg
                 * is the typecast source, followed by the outer call args. */
                id = dup_symbol(child->child);
                /* Build args: inner args (typecast source) + outer args (call args) */
                ast_t *inner_args = child->child->next;
                ast_t *outer_args = child->next;
                if (inner_args != NULL) {
                    /* Chain inner args before outer args */
                    ast_t *tail = inner_args;
                    while (tail->next != NULL)
                        tail = tail->next;
                    tail->next = outer_args;
                    args_start = inner_args;
                } else {
                    args_start = outer_args;
                }
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child->child);
                args_start = child->next;
            }
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC]   id=%s\n", id ? id : "(null)");
    }

    ListNode_t *args = convert_expression_list(args_start);
    struct Statement *call = mk_procedurecall(call_node->line, id, args);
    if (call != NULL) {
        if (call_node->index >= 0)
            call->source_index = call_node->index + g_source_offset;
        /* If id starts with __, it's a method call placeholder from convert_method_call_statement.
         * Extract the bare method name and set the structured flag. */
        if (id != NULL && strncmp(id, "__", 2) == 0) {
            call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
            call->stmt_data.procedure_call_data.placeholder_method_name = strdup(id + 2);
        }
    }
    return call;
}

static struct Statement *convert_method_call_statement(ast_t *member_node, ast_t *args_start) {
    if (member_node == NULL)
        return NULL;

    ast_t *object_node = member_node->child;
    ast_t *field_node = (object_node != NULL) ? object_node->next : NULL;
    if (field_node == NULL)
        return NULL;

    ast_t *identifier_node = unwrap_pascal_node(field_node);
    if (identifier_node == NULL)
        identifier_node = field_node;

    /* Handle the case where field_node is a FUNC_CALL: obj.method(args)
     * In this case, we need to extract the method name from the FUNC_CALL node */
    ast_t *method_name_node = identifier_node;
    if (identifier_node != NULL && identifier_node->typ == PASCAL_T_FUNC_CALL) {
        /* The function name is the first child of the FUNC_CALL node */
        method_name_node = identifier_node->child;
        if (method_name_node != NULL) {
            method_name_node = unwrap_pascal_node(method_name_node);
        }
        /* args_start should be the second child (the arguments) if not already provided */
        if (args_start == NULL && identifier_node->child != NULL) {
            args_start = identifier_node->child->next;
        }
    }

    if (method_name_node == NULL || method_name_node->typ != PASCAL_T_IDENTIFIER)
        return NULL;

    char *method_name = dup_symbol(method_name_node);
    if (method_name == NULL)
        return NULL;

    /* For method calls, we need the object's type to determine which class's
     * method to call. However, at parse time we don't have full type information.
     * 
     * Solution: Don't mangle the name during parsing. Instead, store the unmangled
     * method name and let the semantic checker resolve it based on the object's type.
     * 
     * For now, we'll use find_class_for_method as a heuristic to try to determine
     * the correct class. This works when there's only one class with this method,
     * but fails when multiple classes have the same method name (polymorphism).
     * 
     * A proper fix would require semantic analysis to resolve the method name
     * based on the actual object type.
     */
    /* Disable heuristic - always use semantic resolution */
    const char *class_name = NULL; /* find_class_for_method(method_name); */
    
    char *proc_name = NULL;
    if (class_name != NULL) {
        proc_name = mangle_method_name(class_name, method_name);
    } else {
        /* No class found at parse time. Create a placeholder name that semantic checker
         * will resolve based on the object's type. Use format "__methodname" to indicate
         * this is a method call that needs type-based resolution. */
        size_t len = strlen(method_name) + 3;  /* __ + name + \0 */
        proc_name = (char *)malloc(len);
        if (proc_name != NULL) {
            snprintf(proc_name, len, "__%s", method_name);
        }
    }
    
    if (proc_name == NULL) {
        free(method_name);
        return NULL;
    }

    struct Expression *object_expr = convert_expression(object_node);
    if (object_expr == NULL) {
        free(proc_name);
        free(method_name);
        return NULL;
    }

    ListBuilder arg_builder;
    list_builder_init(&arg_builder);
    list_builder_append(&arg_builder, object_expr, LIST_EXPR);

    if (args_start != NULL) {
        ListNode_t *converted_args = convert_expression_list(args_start);
        list_builder_extend(&arg_builder, converted_args);
    }

    ListNode_t *args = list_builder_finish(&arg_builder);
    struct Statement *call = mk_procedurecall(member_node->line, proc_name, args);
    if (call != NULL) {
        call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
        call->stmt_data.procedure_call_data.placeholder_method_name = method_name;
    } else {
        free(method_name);
    }
    return call;
}

static struct Statement *build_nested_with_statements(int line,
                                                      ast_t *context_node,
                                                      struct Statement *body_stmt) {
    if (context_node == NULL || context_node == ast_nil)
        return NULL;

    ast_t *next_context = context_node->next;
    struct Statement *inner_stmt = body_stmt;
    if (next_context != NULL && next_context != ast_nil) {
        inner_stmt = build_nested_with_statements(line, next_context, body_stmt);
        if (inner_stmt == NULL)
            return NULL;
    }

    ast_t *unwrapped = unwrap_pascal_node(context_node);
    struct Expression *expr = convert_expression(unwrapped);
    if (expr == NULL)
        return NULL;

    return mk_with(line, expr, inner_stmt);
}

static struct Statement *convert_statement(ast_t *stmt_node) {
    stmt_node = unwrap_pascal_node(stmt_node);
    if (stmt_node == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_statement: stmt_node NULL after unwrap\n");
        return NULL;
    }
    /* PASCAL_T_NONE with NULL child means empty statement - skip it */
    if (stmt_node->typ == PASCAL_T_NONE && stmt_node->child == NULL)
        return NULL;
    switch (stmt_node->typ) {
    case PASCAL_T_ASSIGNMENT:
        return convert_assignment(stmt_node);
    case PASCAL_T_LABEL_STMT: {
        ast_t *label_node = stmt_node->child;
        ast_t *stmt_part = NULL;
        if (label_node != NULL && label_node != ast_nil)
            stmt_part = label_node->next;

        ast_t *unwrapped_label = unwrap_pascal_node(label_node);
        char *label = NULL;
        if (unwrapped_label != NULL && unwrapped_label != ast_nil)
            label = dup_symbol(unwrapped_label);

        struct Statement *inner_stmt = convert_statement(unwrap_pascal_node(stmt_part));
        return mk_label(stmt_node->line, label, inner_stmt);
    }
    case PASCAL_T_GOTO_STMT: {
        ast_t *label_node = unwrap_pascal_node(stmt_node->child);
        char *label = NULL;
        if (label_node != NULL && label_node != ast_nil)
            label = dup_symbol(label_node);
        return mk_goto(stmt_node->line, label);
    }
    case PASCAL_T_STATEMENT_LIST: {
        /* A STATEMENT_LIST appearing as a statement — flatten as compound statement */
        ast_t *inner = stmt_node->child;
        if (inner == NULL)
            return NULL;
        ListNode_t *stmts = convert_statement_list(inner);
        return mk_compoundstatement(stmt_node->line, stmts);
    }
    case PASCAL_T_STATEMENT: {
        ast_t *inner = unwrap_pascal_node(stmt_node->child);
        if (inner == NULL)
            return NULL;
        if (inner->typ == PASCAL_T_IDENTIFIER) {
            char *name = dup_symbol(inner);
            if (name != NULL) {
                bool is_assembler = strcasecmp(name, "assembler") == 0;
                free(name);
                if (is_assembler)
                    return NULL;
            }
        }
        if (inner->typ == PASCAL_T_IDENTIFIER || inner->typ == PASCAL_T_FUNC_CALL)
            return convert_proc_call(inner, inner->typ == PASCAL_T_IDENTIFIER);
        if (inner->typ == PASCAL_T_MEMBER_ACCESS) {
            struct Statement *method_stmt = convert_method_call_statement(inner, NULL);
            if (method_stmt != NULL)
                return method_stmt;
        }
        {
            struct Expression *expr = convert_expression(inner);
            if (expr != NULL)
                return mk_exprstmt(stmt_node->line, stmt_node->col, expr);
        }
        return convert_statement(inner);
    }
    case PASCAL_T_FUNC_CALL:
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_statement: FUNC_CALL at line %d\n", stmt_node->line);
        return convert_proc_call(stmt_node, true);
    case PASCAL_T_MEMBER_ACCESS: {
        struct Statement *method_stmt = convert_method_call_statement(stmt_node, NULL);
        if (method_stmt != NULL)
            return method_stmt;
        return NULL;
    }
    case PASCAL_T_ASM_BLOCK: {
        /* stmt_node->child is the asm body (PASCAL_T_NONE with asm text
         * in sym->name).  Its ->next siblings may include clobber reglist
         * nodes ['eax', 'ebx', ...] which must NOT be collected as asm.
         * Temporarily sever the sibling chain to isolate the body. */
        ast_t *asm_body_node = stmt_node->child;
        assert(asm_body_node != NULL && asm_body_node->typ == PASCAL_T_NONE);
        ast_t *saved_next = asm_body_node->next;
        asm_body_node->next = NULL;
        char *code = collect_asm_text(asm_body_node);
        asm_body_node->next = saved_next;
        return mk_asmblock(stmt_node->line, code);
    }
    case PASCAL_T_BREAK_STMT:
        return mk_break(stmt_node->line);
    case PASCAL_T_CONTINUE_STMT:
        return mk_continue(stmt_node->line);
    case PASCAL_T_BEGIN_BLOCK:
        return convert_block(stmt_node);
    case PASCAL_T_IF_STMT: {
        ast_t *cond = stmt_node->child;
        ast_t *then_wrapper = cond != NULL ? cond->next : NULL;
        ast_t *else_wrapper = then_wrapper != NULL ? then_wrapper->next : NULL;

        struct Expression *condition = convert_expression(cond);
        struct Statement *then_stmt = convert_statement(unwrap_pascal_node(then_wrapper));
        struct Statement *else_stmt = convert_statement(unwrap_pascal_node(else_wrapper));
        return mk_ifthen(stmt_node->line, condition, then_stmt, else_stmt);
    }
    case PASCAL_T_WHILE_STMT: {
        ast_t *cond = stmt_node->child;
        ast_t *body = unwrap_pascal_node(cond != NULL ? cond->next : NULL);
        struct Expression *condition = convert_expression(cond);
        struct Statement *body_stmt = convert_statement(body);
        return mk_while(stmt_node->line, condition, body_stmt);
    }
    case PASCAL_T_REPEAT_STMT: {
        ast_t *body_wrapper = stmt_node->child;
        ast_t *cond_node = body_wrapper != NULL ? body_wrapper->next : NULL;

        ListNode_t *body_list = NULL;
        if (body_wrapper != NULL) {
            ast_t *body_nodes = body_wrapper->child != NULL ? body_wrapper->child : body_wrapper;
            body_list = convert_statement_list(body_nodes);
        }

        struct Expression *condition = convert_expression(cond_node);
        return mk_repeat(stmt_node->line, body_list, condition);
    }
    case PASCAL_T_FOR_STMT: {
        ast_t *init_node = unwrap_pascal_node(stmt_node->child);
        ast_t *direction_wrapper = init_node != NULL ? init_node->next : NULL;
        ast_t *direction_node = unwrap_pascal_node(direction_wrapper);
        ast_t *end_wrapper = direction_wrapper != NULL ? direction_wrapper->next : NULL;
        ast_t *end_node = unwrap_pascal_node(end_wrapper);
        ast_t *body_node = unwrap_pascal_node(end_wrapper != NULL ? end_wrapper->next : NULL);

        // Determine if this is a downto loop based on the direction node
        int is_downto = 0;
        if (direction_node != NULL && direction_node->typ == PASCAL_T_DOWNTO) {
            is_downto = 1;
        }

        struct Expression *end_expr = convert_expression(end_node);
        struct Statement *body_stmt = convert_statement(body_node);

        if (init_node != NULL && init_node->typ == PASCAL_T_ASSIGNMENT) {
            struct Statement *assign_stmt = convert_assignment(init_node);
            return mk_forassign(stmt_node->line, assign_stmt, end_expr, body_stmt, is_downto);
        }

        struct Expression *var_expr = convert_expression(init_node);
        if (var_expr == NULL)
            return NULL;
        return mk_forvar(stmt_node->line, var_expr, end_expr, body_stmt, is_downto);
    }
    case PASCAL_T_FOR_IN_STMT: {
        // Pattern matches for_stmt: unwrap the nested NONE seq, then walk siblings
        ast_t *id_node = unwrap_pascal_node(stmt_node->child);
        ast_t *expr_wrapper = id_node != NULL ? id_node->next : NULL;
        ast_t *expr_node = unwrap_pascal_node(expr_wrapper);
        ast_t *body_wrapper = expr_wrapper != NULL ? expr_wrapper->next : NULL;
        ast_t *body_node = unwrap_pascal_node(body_wrapper);
        
        #ifdef DEBUG_FOR_IN_STATEMENT
        fprintf(stderr, "DEBUG FOR_IN: id=%p typ=%d, expr=%p typ=%d, body=%p typ=%d\n",
                (void*)id_node, id_node ? id_node->typ : -1,
                (void*)expr_node, expr_node ? expr_node->typ : -1,
                (void*)body_node, body_node ? body_node->typ : -1);
        if (id_node && id_node->sym) {
            fprintf(stderr, "  id.sym->name=%s\n", id_node->sym->name);
        }
        #endif
        
        if (id_node == NULL || id_node->typ != PASCAL_T_IDENTIFIER) {
            fprintf(stderr, "ERROR: FOR_IN missing loop variable identifier\n");
            return NULL;
        }
        
        if (expr_node == NULL) {
            fprintf(stderr, "ERROR: FOR_IN missing collection expression\n");
            return NULL;
        }
        
        if (body_node == NULL) {
            fprintf(stderr, "ERROR: FOR_IN missing body statement\n");
            return NULL;
        }
        
        // Convert identifier to expression
        struct Expression *var_expr = NULL;
        if (id_node->sym != NULL) {
            var_expr = mk_varid(id_node->line, strdup(id_node->sym->name));
        } else {
            fprintf(stderr, "ERROR: FOR_IN identifier has no symbol\n");
            return NULL;
        }
        
        // Convert collection expression
        struct Expression *collection_expr = convert_expression(expr_node);
        if (collection_expr == NULL) {
            fprintf(stderr, "ERROR: FOR_IN collection expression conversion failed\n");
            destroy_expr(var_expr);
            return NULL;
        }
        
        // Convert body statement  
        struct Statement *body_stmt = convert_statement(body_node);
        if (body_stmt == NULL) {
            fprintf(stderr, "ERROR: FOR_IN body statement conversion failed\n");
            destroy_expr(var_expr);
            destroy_expr(collection_expr);
            return NULL;
        }

        return mk_for_in(stmt_node->line, var_expr, collection_expr, body_stmt);
    }
    case PASCAL_T_EXIT_STMT: {
        /* Exit statement may have an optional return expression: exit(expression) */
        ast_t *child = stmt_node->child;
        struct Expression *return_expr = NULL;
        
        /* Skip the 'exit' keyword token if present */
        while (child != NULL && child->typ == PASCAL_T_NONE && child->child == NULL) {
            child = child->next;
        }
        
        /* Check for a parenthesized expression - the NONE node with a child expression */
        if (child != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(child);
            /* Look inside the optional NONE wrapper that contains: '(' expr ')' */
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_NONE && unwrapped->child != NULL) {
                /* Skip opening paren, get the expression */
                ast_t *expr_node = unwrapped->child;
                /* Skip to actual expression (skip the opening parenthesis) */
                while (expr_node != NULL && 
                       (expr_node->typ == PASCAL_T_NONE || expr_node->typ == PASCAL_T_CHAR) &&
                       expr_node->child == NULL) {
                    expr_node = expr_node->next;
                }
                if (expr_node != NULL) {
                    ast_t *actual_expr = unwrap_pascal_node(expr_node);
                    if (actual_expr != NULL) {
                        return_expr = convert_expression(actual_expr);
                    }
                }
            } else if (unwrapped != NULL) {
                /* Try to convert as expression (shouldn't happen but handle anyway) */
                return_expr = convert_expression(unwrapped);
            }
        }
        
        return mk_exit_with_value(stmt_node->line, return_expr);
    }
    case PASCAL_T_WITH_STMT: {
        ast_t *contexts_wrapper = stmt_node->child;
        ast_t *body_node = unwrap_pascal_node(contexts_wrapper != NULL ? contexts_wrapper->next : NULL);

        struct Statement *body_stmt = convert_statement(body_node);
        if (body_stmt == NULL)
            return NULL;

        if (contexts_wrapper == NULL)
            return NULL;

        if (contexts_wrapper->typ != PASCAL_T_WITH_CONTEXTS) {
            ast_t *single_context = unwrap_pascal_node(contexts_wrapper);
            struct Expression *context_expr = convert_expression(single_context);
            if (context_expr == NULL)
                return NULL;
            return mk_with(stmt_node->line, context_expr, body_stmt);
        }

        ast_t *contexts_start = contexts_wrapper->child;
        if (contexts_start == NULL || contexts_start == ast_nil)
            return NULL;

        return build_nested_with_statements(stmt_node->line, contexts_start, body_stmt);
    }
    case PASCAL_T_TRY_BLOCK: {
        ListBuilder try_builder;
        ListBuilder finally_builder;
        ListBuilder except_builder;
        list_builder_init(&try_builder);
        list_builder_init(&finally_builder);
        list_builder_init(&except_builder);
        
        ast_t *cur = stmt_node->child;
        while (cur != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(cur);
            if (unwrapped == NULL) {
                cur = cur->next;
                continue;
            }
            if (unwrapped->typ == PASCAL_T_FINALLY_BLOCK || unwrapped->typ == PASCAL_T_EXCEPT_BLOCK) {
                ListBuilder *target = (unwrapped->typ == PASCAL_T_FINALLY_BLOCK) ? &finally_builder : &except_builder;
                ast_t *inner = unwrapped->child;
                while (inner != NULL) {
                    ast_t *inner_unwrapped = unwrap_pascal_node(inner);
                    
                    /* Check if this is an 'on' clause with exception variable */
                    if (inner_unwrapped != NULL && inner_unwrapped->typ == PASCAL_T_ON_CLAUSE) {
                        char *exception_var_name = NULL;
                        char *exception_type_name = NULL;
                        /* Extract exception variable and type from on clause */
                        /* Structure: on <var> [: <type>] do <stmt> */
                        ast_t *on_child = inner_unwrapped->child;
                        
                        /* First child should be the variable name */
                        if (on_child != NULL && on_child->typ == PASCAL_T_IDENTIFIER) {
                            if (on_child->sym != NULL)
                                exception_var_name = strdup(on_child->sym->name);
                            on_child = on_child->next;
                        }
                        
                        /* Next might be a NONE node containing the type, or directly the type */
                        if (on_child != NULL) {
                            /* Unwrap if it's a NONE node */
                            ast_t *type_node = (on_child->typ == PASCAL_T_NONE && on_child->child != NULL) ? on_child->child : on_child;
                            
                            /* Look for the type identifier */
                            if (type_node->typ == PASCAL_T_IDENTIFIER) {
                                if (type_node->sym != NULL)
                                    exception_type_name = strdup(type_node->sym->name);
                            }
                            on_child = on_child->next;
                        }
                        
                        /* Find the statement (should be after all the header stuff) */
                        while (on_child != NULL && on_child->typ != PASCAL_T_STATEMENT && 
                               on_child->typ != PASCAL_T_BEGIN_BLOCK && on_child->typ != PASCAL_T_ASSIGNMENT &&
                               on_child->typ != PASCAL_T_FUNC_CALL) {
                            on_child = on_child->next;
                        }
                        
                        /* Convert the statement */
                        if (on_child != NULL) {
                            struct Statement *inner_stmt = convert_statement(unwrap_pascal_node(on_child));
                            if (inner_stmt != NULL) {
                                struct Statement *on_stmt = mk_on_exception(stmt_node->line,
                                    exception_var_name, exception_type_name, inner_stmt);
                                list_builder_append(target, on_stmt, LIST_STMT);
                                exception_var_name = NULL;
                                exception_type_name = NULL;
                            }
                        }
                        if (exception_var_name != NULL)
                            free(exception_var_name);
                        if (exception_type_name != NULL)
                            free(exception_type_name);
                    } else {
                        struct Statement *inner_stmt = convert_statement(inner_unwrapped);
                        if (inner_stmt != NULL)
                            list_builder_append(target, inner_stmt, LIST_STMT);
                    }
                    inner = inner->next;
                }
            } else {
                struct Statement *try_stmt = convert_statement(unwrapped);
                if (try_stmt != NULL)
                    list_builder_append(&try_builder, try_stmt, LIST_STMT);
            }
            cur = cur->next;
        }

        ListNode_t *try_stmts = list_builder_finish(&try_builder);
        ListNode_t *finally_stmts = list_builder_finish(&finally_builder);
        ListNode_t *except_stmts = list_builder_finish(&except_builder);

        if (finally_stmts == NULL && except_stmts == NULL) {
            if (try_stmts == NULL)
                return NULL;
            if (try_stmts->next == NULL) {
                struct Statement *passthrough = (struct Statement *)try_stmts->cur;
                DestroyList(try_stmts);
                return passthrough;
            }
            return mk_compoundstatement(stmt_node->line, try_stmts);
        }

        if (finally_stmts != NULL)
            return mk_tryfinally(stmt_node->line, try_stmts, finally_stmts);
        return mk_tryexcept(stmt_node->line, try_stmts, except_stmts, NULL, NULL);
    }
    case PASCAL_T_RAISE_STMT: {
        struct Expression *exc_expr = convert_expression(unwrap_pascal_node(stmt_node->child));
        return mk_raise(stmt_node->line, exc_expr);
    }
    case PASCAL_T_INHERITED_STMT: {
        struct Expression *call_expr = convert_expression(unwrap_pascal_node(stmt_node->child));
        return mk_inherited(stmt_node->line, call_expr);
    }
    case PASCAL_T_CASE_STMT: {
        ast_t *selector = stmt_node->child;
        ast_t *branches_start = selector != NULL ? selector->next : NULL;

        struct Expression *selector_expr = convert_expression(selector);
        ListBuilder branches_builder;
        list_builder_init(&branches_builder);
        struct Statement *else_stmt = NULL;
        
        /* Walk through all siblings after the selector */
        ast_t *cur = branches_start;
        while (cur != NULL) {
            if (cur->typ == PASCAL_T_CASE_BRANCH) {
                /* Process this branch */
                ListBuilder labels_builder;
                list_builder_init(&labels_builder);
                struct Statement *branch_stmt = NULL;
                
                /* Walk through children of the branch */
                ast_t *child = cur->child;
                while (child != NULL) {
                    if (child->typ == PASCAL_T_CASE_LABEL_LIST) {
                        /* CASE_LABEL_LIST contains the labels */
                        ast_t *label_node = child->child;
                while (label_node != NULL) {
                    append_case_label(&labels_builder, label_node);
                    label_node = label_node->next;
                }
            } else if (child->typ == PASCAL_T_CASE_LABEL ||
                       child->typ == PASCAL_T_INTEGER ||
                       child->typ == PASCAL_T_IDENTIFIER ||
                       child->typ == PASCAL_T_CHAR ||
                       child->typ == PASCAL_T_CHAR_CODE ||
                       child->typ == PASCAL_T_STRING ||
                       child->typ == PASCAL_T_RANGE) {
                append_case_label(&labels_builder, child);
            } else {
                /* Any other child type is the statement for this branch
                 * (e.g. PASCAL_T_STATEMENT, PASCAL_T_ASSIGNMENT,
                 * PASCAL_T_FUNC_CALL, PASCAL_T_BEGIN_BLOCK,
                 * PASCAL_T_IF_STMT, PASCAL_T_FOR_STMT, PASCAL_T_WHILE_STMT,
                 * PASCAL_T_REPEAT_STMT, PASCAL_T_CASE_STMT, PASCAL_T_WITH_STMT,
                 * PASCAL_T_EXIT_STMT, PASCAL_T_BREAK_STMT, etc.) */
                branch_stmt = convert_statement(child);
                break; /* Statement is last */
                    }
                    child = child->next;
                }
                
                /* Create the branch */
                struct CaseBranch *branch = (struct CaseBranch *)malloc(sizeof(struct CaseBranch));
                if (branch != NULL) {
                    branch->labels = list_builder_finish(&labels_builder);
                    branch->stmt = branch_stmt;
                    list_builder_append(&branches_builder, branch, LIST_CASE_BRANCH);
                }
            } else if (cur->typ == PASCAL_T_ELSE) {
                /* Optional else clause */
                if (cur->child != NULL) {
                    else_stmt = convert_statement(cur->child);
                }
            }
            cur = cur->next;
        }
        
        return mk_case(stmt_node->line, selector_expr, list_builder_finish(&branches_builder), else_stmt);
    }
    default: {
        /* Try to convert unknown tags as expressions (e.g., NOT, AND, OR in const contexts) */
        struct Expression *expr = convert_expression(stmt_node);
        if (expr != NULL)
            return mk_exprstmt(stmt_node->line, stmt_node->col, expr);

        const char *name = tag_name(stmt_node->typ);
        fprintf(stderr, "ERROR: unsupported statement tag %d (%s) at line %d.",
                stmt_node->typ, name, stmt_node->line);
        if (stmt_node->sym != NULL && stmt_node->sym->name != NULL)
            fprintf(stderr, " (symbol: %s)", stmt_node->sym->name);
        if (stmt_node->child != NULL)
            fprintf(stderr, " (child: %s/%d)", tag_name(stmt_node->child->typ), stmt_node->child->line);
        fprintf(stderr, "\n");
        break;
    }
    }

    return NULL;
}

static ListNode_t *convert_statement_list(ast_t *stmt_list_node) {
    ListBuilder builder;
    list_builder_init(&builder);
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for statement list traversal\n");
        return NULL;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_statement_list: starting, stmt_list_node=%p\n", (void*)stmt_list_node);
    }
    
    ast_t *cur = stmt_list_node;
    int stmt_count = 0;
    while (cur != NULL && cur != ast_nil) {
        /* Check for circular reference before processing */
        if (!is_safe_to_continue(visited, cur)) {
            fprintf(stderr, "ERROR: Circular reference detected in statement list, stopping traversal\n");
            break;
        }
        
        ast_t *unwrapped = unwrap_pascal_node(cur);
        if (unwrapped == NULL) {
            cur = cur->next;
            continue;
        }

        if (unwrapped->typ == PASCAL_T_NONE && unwrapped->child == NULL) {
            cur = cur->next;
            continue;
        }

        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_statement_list: processing stmt %d, typ=%d line=%d\n", 
                    stmt_count, unwrapped->typ, unwrapped->line);
            fprintf(stderr, "[KGPC]   calling convert_statement...\n");
            fflush(stderr);
        }

        struct Statement *stmt = convert_statement(unwrapped);
        
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   convert_statement returned: %p\n", (void*)stmt);
            fflush(stderr);
        }
        
        if (stmt != NULL) {
            list_builder_append(&builder, stmt, LIST_STMT);
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   -> converted successfully\n");
                fflush(stderr);
            }
        } else if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   -> convert_statement returned NULL, dropped statement typ=%d line=%d\n",
                unwrapped->typ, unwrapped->line);
            fflush(stderr);
        }
        cur = cur->next;
        stmt_count++;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_statement_list: processed %d statements\n", stmt_count);
    }

    visited_set_destroy(visited);
    return list_builder_finish(&builder);
}

static struct Statement *convert_block(ast_t *block_node) {
    if (block_node == NULL)
    {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_block: block_node is NULL\n");
        return NULL;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_block: typ=%d line=%d\n", block_node->typ, block_node->line);
        if (block_node->child) {
            fprintf(stderr, "[KGPC]   child typ=%d line=%d\n", 
                    block_node->child->typ, block_node->child->line);
            // Check if child is ast_nil
            if (block_node->child == ast_nil) {
                fprintf(stderr, "[KGPC]   child is ast_nil!\n");
            }
        } else {
            fprintf(stderr, "[KGPC]   child is NULL\n");
        }
    }

    ast_t *stmts = block_node->child;
    if (stmts == ast_nil) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_block: stmts is ast_nil, treating as NULL\n");
        }
        stmts = NULL;
    }
    ListNode_t *list = convert_statement_list(stmts);
    if (list == NULL && kgpc_getenv("KGPC_DEBUG_BODY") != NULL)
        fprintf(stderr, "[KGPC] convert_block: statement list is NULL\n");
    return mk_compoundstatement(block_node->line, list);
}

/* Recursively search an AST subtree for a keyword (case-insensitive).
 * Returns 1 if found, 0 otherwise. Limits depth to avoid runaway recursion. */
/* Search an AST node list (linked via ->next) and their children for a keyword. */
static int ast_has_keyword_in_list(ast_t *node, const char *keyword, int max_depth) {
    if (node == NULL || max_depth <= 0)
        return 0;
    if (node->sym != NULL && node->sym->name != NULL &&
        strcasecmp(node->sym->name, keyword) == 0)
        return 1;
    if (ast_has_keyword_in_list(node->child, keyword, max_depth - 1))
        return 1;
    return ast_has_keyword_in_list(node->next, keyword, max_depth);
}

static int ast_list_has_directive_keyword(ast_t *node, const char *keyword, int max_depth)
{
    if (node == NULL || keyword == NULL || max_depth <= 0)
        return 0;

    for (ast_t *cur = node; cur != NULL; cur = cur->next)
    {
        if (cur->typ == PASCAL_T_FUNCTION_BODY ||
            cur->typ == PASCAL_T_BEGIN_BLOCK ||
            cur->typ == PASCAL_T_ASM_BLOCK ||
            cur->typ == PASCAL_T_CONST_SECTION ||
            cur->typ == PASCAL_T_VAR_SECTION ||
            cur->typ == PASCAL_T_TYPE_SECTION ||
            cur->typ == PASCAL_T_LABEL_SECTION ||
            cur->typ == PASCAL_T_PROCEDURE_DECL ||
            cur->typ == PASCAL_T_FUNCTION_DECL ||
            cur->typ == PASCAL_T_METHOD_IMPL)
            break;

        if (cur->sym != NULL && cur->sym->name != NULL &&
            strcasecmp(cur->sym->name, keyword) == 0)
            return 1;

        switch (cur->typ)
        {
            case PASCAL_T_NONE:
            case PASCAL_T_IDENTIFIER:
            case PASCAL_T_EXTERNAL_NAME:
            case PASCAL_T_EXTERNAL_NAME_EXPR:
                if (ast_has_keyword_in_list(cur->child, keyword, max_depth - 1))
                    return 1;
                break;
            default:
                break;
        }
    }

    return 0;
}

static int ast_has_routine_directive(ast_t *node, const char *keyword, int max_depth)
{
    if (node == NULL || keyword == NULL || max_depth <= 0)
        return 0;
    return ast_list_has_directive_keyword(node->child, keyword, max_depth);
}

/* Build a TypeAlias capturing a complex return type from TypeInfo, transferring
 * ownership of heap-allocated fields.  Cleans up remaining TypeInfo contents. */
static struct TypeAlias *build_inline_return_alias(TypeInfo *type_info, int return_type,
                                                   char *return_type_id)
{
    struct TypeAlias *alias = NULL;
    if (type_info->is_array || type_info->is_pointer || type_info->is_set ||
        type_info->is_enum || type_info->is_file || type_info->is_record) {
        alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (alias != NULL) {
            alias->base_type = return_type;
            alias->target_type_id = return_type_id;
            if (type_info->is_array) {
                alias->is_array = 1;
                alias->array_start = type_info->start;
                alias->array_end = type_info->end;
                alias->array_element_type = type_info->element_type;
                alias->array_element_type_id = type_info->element_type_id;
                if (type_info->element_kgpc_type != NULL) {
                    long long esize = kgpc_type_sizeof(type_info->element_kgpc_type);
                    if (esize > 0 && esize <= INT_MAX)
                        alias->array_element_storage_size = (int)esize;
                }
                alias->is_shortstring = type_info->is_shortstring;
                alias->is_open_array = type_info->is_open_array;
            }
            if (type_info->is_pointer) {
                alias->is_pointer = 1;
                alias->pointer_type = type_info->pointer_type;
                alias->pointer_type_id = type_info->pointer_type_id;
            }
            if (type_info->is_set) {
                alias->is_set = 1;
                alias->set_element_type = type_info->set_element_type;
                alias->set_element_type_id = type_info->set_element_type_id;
            }
            if (type_info->is_enum) {
                alias->is_enum = 1;
                alias->enum_is_scoped = type_info->enum_is_scoped;
                alias->enum_literals = type_info->enum_literals;
            }
            if (type_info->is_file) {
                alias->is_file = 1;
                alias->file_type = type_info->file_type;
                alias->file_type_id = type_info->file_type_id;
            }
            /* NULL out transferred pointers so destroy_type_info_contents
             * won't double-free them. */
            type_info->element_type_id = NULL;
            type_info->pointer_type_id = NULL;
            type_info->set_element_type_id = NULL;
            type_info->enum_literals = NULL;
            type_info->file_type_id = NULL;
        }
    }
    destroy_type_info_contents(type_info);
    return alias;
}

static Tree_t *convert_method_impl(ast_t *method_node) {
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl entry (method_node=%p)\n", (void*)method_node);
    }

    if (method_node == NULL)
        return NULL;

    ast_t *cur = method_node->child;
    /* Skip optional keyword identifiers like "class" or "generic" that appear
     * before the qualified identifier (e.g., "class function THost.SeedSum"). */
    int method_decl_uses_operator_keyword = 0;
    while (cur != NULL) {
        ast_t *skip_node = unwrap_pascal_node(cur);
        if (skip_node == NULL) skip_node = cur;
        if (skip_node->typ == PASCAL_T_IDENTIFIER &&
            skip_node->sym != NULL && skip_node->sym->name != NULL &&
            is_method_decl_keyword(skip_node->sym->name)) {
            if (strcasecmp(skip_node->sym->name, "operator") == 0)
                method_decl_uses_operator_keyword = 1;
            cur = cur->next;
            continue;
        }
        break;
    }
    ast_t *qualified = unwrap_pascal_node(cur);

    if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
        fprintf(stderr, "[Operator] convert_method_impl: cur=%p qualified=%p\n", (void*)cur, (void*)qualified);
        if (qualified != NULL) {
            fprintf(stderr, "[Operator]   qualified->typ=%d\n", qualified->typ);
            if (qualified->sym && qualified->sym->name) {
                fprintf(stderr, "[Operator]   qualified->sym->name=%s\n", qualified->sym->name);
            }
        }
    }

    /* Check for standalone operator: the first child is directly the operator symbol like "+" or "-"
     * This is detected by checking if the first identifier is a known operator symbol */
    if (qualified != NULL && qualified->typ == PASCAL_T_IDENTIFIER) {
        char *potential_op = dup_symbol(qualified);
        int is_operator_symbol = (potential_op != NULL && is_operator_token_name(potential_op));
        int should_parse_standalone_operator = method_decl_uses_operator_keyword || is_operator_symbol;
        /* Check if this is an operator symbol (encoded to op_xxx means it's an operator) */
        char *encoded_op = (potential_op != NULL && should_parse_standalone_operator)
            ? encode_operator_name(potential_op) : NULL;
        free(potential_op);

        if (should_parse_standalone_operator) {
            /* This is a standalone operator - qualified is the operator symbol directly */
            if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                fprintf(stderr, "[Operator] Detected operator symbol: encoded=%s\n", encoded_op);
            }
            /* Get the param list to determine the type.
             * The next node could be PASCAL_T_PARAM_LIST or directly a PASCAL_T_PARAM */
            ast_t *param_node = qualified->next;
            if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                fprintf(stderr, "[Operator] param_node=%p typ=%d (PARAM_LIST=%d, PARAM=%d)\n",
                    (void*)param_node, param_node ? param_node->typ : -1,
                    PASCAL_T_PARAM_LIST, PASCAL_T_PARAM);
            }
            ast_t *first_param = NULL;
            if (param_node != NULL) {
                if (param_node->typ == PASCAL_T_PARAM_LIST) {
                    first_param = param_node->child;
                } else if (param_node->typ == PASCAL_T_PARAM) {
                    first_param = param_node;
                }
            }
            if (first_param != NULL) {
                /* Parse the parameter to get its type */
                ast_t *param_cursor = first_param;
                ListNode_t *params = convert_param_list(&param_cursor);
                if (params != NULL) {
                    Tree_t *first_param_tree = (Tree_t *)params->cur;
                    if (first_param_tree != NULL &&
                        first_param_tree->type == TREE_VAR_DECL &&
                        first_param_tree->tree_data.var_decl_data.type_id != NULL) {
                        const char *param_type_id = first_param_tree->tree_data.var_decl_data.type_id;
                        /* Get second param type for binary operator disambiguation */
                        const char *second_param_type_id = NULL;
                        if (params->next != NULL) {
                            Tree_t *second_param_tree = (Tree_t *)params->next->cur;
                            if (second_param_tree != NULL && second_param_tree->type == TREE_VAR_DECL &&
                                second_param_tree->tree_data.var_decl_data.type_id != NULL)
                                second_param_type_id = second_param_tree->tree_data.var_decl_data.type_id;
                        }

                        /* Build mangled name: TypeName__op_suffix[_SecondParamType] */
                        size_t name_len = strlen(param_type_id) + strlen(encoded_op) + 3
                            + (second_param_type_id != NULL ? strlen(second_param_type_id) + 1 : 0);
                        char *mangled_name = (char *)malloc(name_len);
                        if (mangled_name != NULL) {
                            if (second_param_type_id != NULL)
                                snprintf(mangled_name, name_len, "%s__%s_%s", param_type_id, encoded_op, second_param_type_id);
                            else
                                snprintf(mangled_name, name_len, "%s__%s", param_type_id, encoded_op);

                            if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                                fprintf(stderr, "[Operator] Standalone operator: %s\n", mangled_name);
                            }

                            /* Find return type by scanning siblings of param_node */
                            ast_t *return_type_node = NULL;
                            char *result_var_name_method = NULL;
                            ast_t *scan = param_node->next;
                            while (scan != NULL) {
                                if (scan->typ == PASCAL_T_RETURN_TYPE) {
                                    return_type_node = scan;
                                    break;
                                }
                                /* Named result identifier before RETURN_TYPE (e.g., dest : variant) */
                                if (scan->typ == PASCAL_T_IDENTIFIER && scan->next != NULL &&
                                    scan->next->typ == PASCAL_T_RETURN_TYPE) {
                                    result_var_name_method = dup_symbol(scan);
                                    scan = scan->next;
                                    return_type_node = scan;
                                    break;
                                }
                                /* Skip other params */
                                if (scan->typ != PASCAL_T_PARAM)
                                    break;
                                scan = scan->next;
                            }

                            char *return_type_id = NULL;
                            int return_type = UNKNOWN_TYPE;
                            struct TypeAlias *inline_return_type = NULL;
                            TypeRef *return_type_ref = NULL;

                            if (return_type_node != NULL) {
                                TypeInfo type_info;
                                memset(&type_info, 0, sizeof(TypeInfo));
                                return_type = convert_type_spec(return_type_node->child, &return_type_id, NULL, &type_info);
                                if (return_type_ref == NULL)
                                    return_type_ref = type_ref_from_info_or_id(&type_info, return_type_id);
                                if (return_type_id == NULL && return_type_node->sym != NULL &&
                                    return_type_node->sym->name != NULL)
                                    return_type_id = strdup(return_type_node->sym->name);
                                destroy_type_info_contents(&type_info);
                            }

                            /* Keep the base name (TypeName__op_suffix) as the
                             * symbol table id so FindAllIdents can find all
                             * overloads.  The full suffixed name goes into
                             * mangled_id for unique assembly labels. */
                            size_t base_id_len = strlen(param_type_id) + strlen(encoded_op) + 3;
                            char *base_id = (char *)malloc(base_id_len);
                            if (base_id != NULL)
                                snprintf(base_id, base_id_len, "%s__%s", param_type_id, encoded_op);
                            const char *ret_suffix = return_type_id;
                            if (ret_suffix == NULL) ret_suffix = result_var_name_method;
                            if (ret_suffix != NULL) {
                                size_t new_len = strlen(mangled_name) + strlen(ret_suffix) + 2;
                                char *new_name = (char *)malloc(new_len);
                                if (new_name != NULL) {
                                    snprintf(new_name, new_len, "%s_%s", mangled_name, ret_suffix);
                                    free(mangled_name);
                                    mangled_name = new_name;
                                }
                            }

                            /* Find the body */
                            struct Statement *body = NULL;
                            ListNode_t *op_const_decls = NULL;
                            ListBuilder op_var_builder; list_builder_init(&op_var_builder);
                            ListBuilder op_label_builder; list_builder_init(&op_label_builder);
                            ListNode_t *op_nested_subs = NULL;
                            ListNode_t *op_type_decls = NULL;
                            ast_t *body_cursor = return_type_node ? return_type_node->next : param_node->next;
                            while (body_cursor != NULL) {
                                if (body_cursor->typ == PASCAL_T_BEGIN_BLOCK) {
                                    body = convert_block(body_cursor);
                                    break;
                                } else if (body_cursor->typ == PASCAL_T_FUNCTION_BODY) {
                                    convert_routine_body(body_cursor, &op_const_decls, &op_var_builder, &op_label_builder,
                                        &op_nested_subs, &body, &op_type_decls);
                                    break;
                                }
                                body_cursor = body_cursor->next;
                            }

                            /* Create the function tree: use base_id as the
                             * symbol table key; store the full suffixed name
                             * as mangled_id for unique assembly labels. */
                            ListNode_t *op_var_decls = list_builder_finish(&op_var_builder);
                            ListNode_t *op_label_decls = list_builder_finish(&op_label_builder);
                            Tree_t *tree = mk_function(method_node->line,
                                base_id != NULL ? base_id : mangled_name,
                                params, op_const_decls,
                                op_label_decls, op_type_decls, op_var_decls, op_nested_subs, body, return_type, return_type_id, inline_return_type, 0, 0);
                            if (tree != NULL) {
                                if (base_id != NULL)
                                    tree->tree_data.subprogram_data.mangled_id = mangled_name;
                                else
                                    tree->tree_data.subprogram_data.mangled_id = strdup(tree->tree_data.subprogram_data.id);
                                tree->tree_data.subprogram_data.is_operator = 1;
                            } else
                                free(mangled_name);
                            if (base_id != NULL && tree != NULL)
                                ; /* base_id ownership transferred to mk_function */
                            else if (base_id != NULL && tree == NULL)
                                free(base_id);
                            if (tree != NULL && method_node->index >= 0)
                                tree->source_index = method_node->index + g_source_offset;
                            if (tree != NULL && result_var_name_method != NULL) {
                                tree->tree_data.subprogram_data.return_type_ref =
                                    return_type_ref != NULL ? type_ref_clone(return_type_ref)
                                                            : type_ref_from_single_name(return_type_id);
                                tree->tree_data.subprogram_data.result_var_name = result_var_name_method;
                            } else if (result_var_name_method != NULL) {
                                free(result_var_name_method);
                            }

                            free(encoded_op);
                            return tree;
                        }
                    }
                    /* Don't destroy params here - they'll be used in the tree */
                }
            }
            free(encoded_op);
            return NULL; /* Couldn't process the operator */
        }
        free(encoded_op);
    }

    if (qualified == NULL || qualified->typ != PASCAL_T_QUALIFIED_IDENTIFIER) {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: cur=%p typ=%d\n",
                    (void*)cur, cur ? cur->typ : -1);
            if (cur && cur->sym && cur->sym->name) {
                fprintf(stderr, "[KGPC]   cur->sym->name=%s\n", cur->sym->name);
            }
            fprintf(stderr, "[KGPC] convert_method_impl: qualified=%p typ=%d\n",
                    (void*)qualified, qualified ? qualified->typ : -1);
            if (qualified && qualified->sym && qualified->sym->name) {
                fprintf(stderr, "[KGPC]   qualified->sym->name=%s\n", qualified->sym->name);
            }
            if (qualified && qualified->child) {
                fprintf(stderr, "[KGPC]   qualified->child->typ=%d\n", qualified->child->typ);
            }
            fprintf(stderr, "[KGPC] convert_method_impl: no qualified identifier (typ=%d)\n",
                    qualified ? qualified->typ : -1);
        }
        return NULL;
    }

    ast_t *class_node = qualified->child;
    if (class_node == NULL)
        return NULL;

    // Collect all IDENTIFIER nodes from the qualified name.
    // The grammar produces: IDENTIFIER [TYPE_PARAM_LIST] IDENTIFIER [IDENTIFIER]*
    // For "Class.Method" we get 2 identifiers, for "Class.Inner.Method" we get 3, etc.
    // The LAST identifier is always the method name; all preceding ones form the class path.
    ast_t *ident_nodes[32];  // more than enough for any reasonable nesting depth
    int ident_count = 0;

    ast_t *method_type_param_list = NULL;  /* TYPE_PARAM_LIST after the last IDENTIFIER (method-level generic) */
    ast_t *pending_type_param_list = NULL;
    for (ast_t *cursor = class_node; cursor != NULL; cursor = cursor->next) {
        if (cursor->typ == PASCAL_T_IDENTIFIER && ident_count < 32) {
            ident_nodes[ident_count++] = cursor;
            pending_type_param_list = NULL;  /* TYPE_PARAM_LIST before an IDENTIFIER is class-level, reset */
        }
        if (cursor->typ == PASCAL_T_TYPE_PARAM_LIST) {
            pending_type_param_list = cursor;
        }
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: child typ=%d (%s) name=%s\n",
                    cursor->typ, pascal_tag_to_string(cursor->typ),
                    (cursor->sym && cursor->sym->name) ? cursor->sym->name : "<null>");
        }
    }
    method_type_param_list = pending_type_param_list;  /* Last TYPE_PARAM_LIST with no IDENTIFIER after it */

    if (ident_count < 2)
        return NULL;  // Need at least ClassName and MethodName

    ast_t *method_id_node = ident_nodes[ident_count - 1];  // last identifier = method name

    // Build class_name by joining all identifiers except the last with "."
    size_t class_name_len = 0;
    for (int i = 0; i < ident_count - 1; i++) {
        char *seg = dup_symbol(ident_nodes[i]);
        if (seg != NULL) {
            class_name_len += strlen(seg) + 1;  // +1 for '.' or '\0'
            free(seg);
        }
    }

    char *class_name = (char *)malloc(class_name_len);
    if (class_name == NULL)
        return NULL;
    class_name[0] = '\0';
    for (int i = 0; i < ident_count - 1; i++) {
        char *seg = dup_symbol(ident_nodes[i]);
        if (seg != NULL) {
            if (i > 0) strcat(class_name, ".");
            strcat(class_name, seg);
            free(seg);
        }
    }

    char *method_name = dup_symbol(method_id_node);
    if (method_name == NULL) {
        free(class_name);
        return NULL;
    }
    
    // For generic classes, strip the type parameters from the class name
    // e.g., "TFPGList<T>" -> "TFPGList"
    char *cleaned_class_name = NULL;
    if (class_name != NULL) {
        char *bracket = strchr(class_name, '<');
        if (bracket != NULL) {
            size_t len = (size_t)(bracket - class_name);
            cleaned_class_name = (char *)malloc(len + 1);
            if (cleaned_class_name != NULL) {
                memcpy(cleaned_class_name, class_name, len);
                cleaned_class_name[len] = '\0';
            }
        }
    }

    const char *registered_class = find_class_for_method(method_name);
    /* Prefer the explicitly specified class name from the qualified identifier,
     * falling back to the registered class if no explicit class was given */
    const char *effective_class = (cleaned_class_name != NULL) ? cleaned_class_name : 
                                  (class_name != NULL) ? class_name : registered_class;
    
    /* For nested types like TManager.TState, the effective class for mangling
     * should use just the last component (TState) since that's the actual type
     * name that methods are registered under.  Keep the full dotted name in
     * effective_class_full for const/var lookups.  */
    const char *effective_class_full = effective_class;
    char *effective_class_last = NULL;
    char *effective_class_outer = NULL;
    if (effective_class != NULL)
    {
        const char *last_dot = strrchr(effective_class, '.');
        if (last_dot != NULL && last_dot[1] != '\0')
        {
            /* "TOuter.TInner" -> outer="TOuter", last="TInner" */
            effective_class_outer = strndup(effective_class, (size_t)(last_dot - effective_class));
            effective_class_last = strdup(last_dot + 1);
            effective_class = effective_class_last;
        }
    }

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[FromCParser] convert_method_impl: class_name=%s method_name=%s effective_class=%s effective_class_full=%s\n",
            class_name ? class_name : "<null>",
            method_name ? method_name : "<null>",
            effective_class ? effective_class : "<null>",
            effective_class_full ? effective_class_full : "<null>");
    }
    
    /* Don't re-register the method here - it was already registered during class declaration */
    
    /* Check if this method was declared as static. Use signature-aware lookup
     * for overload disambiguation. Also detect class methods (Self = VMT). */
    int is_static_method = 0;
    int is_class_method_impl = from_cparser_is_method_class_method(effective_class, method_name);
    int method_param_count = count_params_in_method_impl(method_node);
    char *method_param_sig = param_type_signature_from_method_impl(method_node);
    int method_declares_operator = method_decl_uses_operator_keyword;
    if (method_node != NULL && method_name != NULL)
    {
        struct MethodTemplate impl_template = {0};
        impl_template.name = (char *)method_name;
        annotate_method_template(&impl_template, method_node);
        /* Only truly static methods skip Self; class methods have Self = VMT pointer */
        if (impl_template.is_static)
            is_static_method = 1;
        if (impl_template.is_class_method)
            is_class_method_impl = 1;
    }
    if (!is_static_method)
        is_static_method = is_method_static_with_signature(effective_class, method_name,
            method_param_count, method_param_sig);
    /* A static class method (class function ... static) has no Self */
    if (is_class_method_impl && is_static_method)
        is_static_method = 1;
    /* A non-static class method (class function) has Self = VMT, so don't skip it */
    else if (is_class_method_impl && !is_static_method)
        is_static_method = 0;
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class=%s method=%s is_static=%d\n",
                effective_class ? effective_class : "<null>", 
                method_name ? method_name : "<null>",
                is_static_method);
    }
    
    const char *mangle_owner = effective_class;
    if (effective_class_full != NULL && effective_class_full != effective_class)
        mangle_owner = effective_class_full;
    char *proc_name = mangle_method_name_raw(mangle_owner, method_name);
    if (proc_name == NULL) {
        free(class_name);
        free(method_name);
        free(effective_class_last);
        if (method_param_sig != NULL)
            free(method_param_sig);
        return NULL;
    }
    
    /* Check if this is a class operator.
     * Named operators like Add/Subtract must come from "operator" declarations,
     * otherwise normal methods (e.g. TList.Add) are misclassified. */
    int is_class_operator = 0;
    if (method_declares_operator)
    {
        is_class_operator = 1;
    }
    else if (method_name != NULL)
    {
        /* Fallback only for symbolic operators to keep legacy parser cases working.
         * Named operators (Equal, Add, etc.) must NOT be matched here because
         * regular methods with those names (e.g. tdynamicarray.equal) would be
         * misclassified as operators and lose their implicit Self parameter. */
        if (strcmp(method_name, "+") == 0 || strcmp(method_name, "-") == 0 ||
            strcmp(method_name, "*") == 0 || strcmp(method_name, "/") == 0 ||
            strcmp(method_name, "=") == 0 || strcmp(method_name, "<>") == 0 ||
            strcmp(method_name, "<") == 0 || strcmp(method_name, ">") == 0 ||
            strcmp(method_name, "<=") == 0 || strcmp(method_name, ">=") == 0 ||
            strcmp(method_name, "**") == 0 || strcmp(method_name, ":=") == 0) {
            is_class_operator = 1;
        }
    }
    int is_constructor = (method_node->typ == PASCAL_T_CONSTRUCTOR_DECL) ||
        (method_name != NULL && strcasecmp(method_name, "create") == 0);

    ListBuilder params_builder;
    list_builder_init(&params_builder);
    char **generic_type_params = NULL;
    int num_generic_type_params = 0;
    /* Extract method-level generic type params found inside the qualified identifier */
    if (method_type_param_list != NULL) {
        num_generic_type_params = extract_generic_type_params(method_type_param_list, &generic_type_params);
    }
    ListNode_t *const_decls = NULL;
    ListBuilder var_builder;
    list_builder_init(&var_builder);
    ListBuilder label_builder;
    list_builder_init(&label_builder);
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;
    int is_nostackframe = 0;
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;

    /* Return type information for methods that are functions (like class operators) */
    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;
    struct TypeAlias *inline_return_type = NULL;
    int has_return_type = 0;
    TypeRef *return_type_ref = NULL;

    const char *helper_base = (effective_class != NULL) ? lookup_type_helper_base(effective_class) : NULL;
    int is_helper_method = (helper_base != NULL);
    if (kgpc_getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && effective_class != NULL)
        fprintf(stderr, "[KGPC] convert_method_impl: looking up helper base for %s -> %s\n",
            effective_class, helper_base ? helper_base : "(not found)");

    /* Add Self parameter only for instance methods, not for class operators or static methods */
    if (kgpc_getenv("KGPC_ASSERT_STATIC_SELF") != NULL && is_static_method) {
        assert(!is_class_operator && "static method should not be class operator");
        assert(method_param_sig != NULL || method_param_count >= 0);
    }

    if (!is_class_operator && !is_static_method) {
        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
        char *self_type_id = NULL;
        int self_type_tag = UNKNOWN_TYPE;
        struct TypeAlias *self_type_alias = NULL;
        int self_is_var = 1;
        if (effective_class != NULL) {
            if (is_helper_method) {
                self_type_id = strdup(helper_base);
                /* Resolve the type tag for type helper Self parameters.
                 * This is important for correct calling convention (e.g., Double should use xmm0). */
                self_type_tag = map_type_name(helper_base, NULL);
                self_type_alias = helper_self_real_alias(helper_base);
                self_is_var = helper_self_param_is_var(helper_base, NULL);
            } else {
                self_type_id = strdup(effective_class_full ? effective_class_full : effective_class);
                if (type_name_is_class_like(effective_class))
                    self_is_var = 0;
            }
        }
        Tree_t *self_param = mk_vardecl(method_node->line, self_ids, self_type_tag,
            self_type_id, self_is_var, 0, NULL, NULL, self_type_alias, NULL);
        if (self_param != NULL)
            self_param->tree_data.var_decl_data.type_ref = type_ref_from_single_name(self_type_id);
        list_builder_append(&params_builder, self_param, LIST_TREE);
    }

    const char *prev_method_name_ctx = g_current_method_name;
    g_current_method_name = method_name;
    cur = qualified->next;
    while (cur != NULL) {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;

        switch (node->typ) {
        case PASCAL_T_TYPE_PARAM_LIST:
            if (num_generic_type_params == 0) {
                num_generic_type_params = extract_generic_type_params(node, &generic_type_params);
            }
            break;
        case PASCAL_T_PARAM_LIST: {
            ast_t *param_cursor = node->child;
            ListNode_t *extra_params = convert_param_list(&param_cursor);
            if (extra_params != NULL)
                list_builder_extend(&params_builder, extra_params);
            break;
        }
        case PASCAL_T_PARAM: {
            ListNode_t *extra_params = convert_param(node);
            if (extra_params != NULL)
                list_builder_extend(&params_builder, extra_params);
            break;
        }
        case PASCAL_T_RETURN_TYPE: {
            /* Method has a return type - it's a function, not a procedure */
            has_return_type = 1;
            TypeInfo type_info = {0};
            return_type = convert_type_spec(node->child, &return_type_id, NULL, &type_info);
            /* Same NULL-child fallback as convert_function: when the parser
             * produces a RETURN_TYPE node without a child subtree (forward
             * declarations), use the sym name for {$H-} remapping. */
            if (return_type == UNKNOWN_TYPE && return_type_id == NULL &&
                node->sym != NULL && node->sym->name != NULL)
            {
                if (pascal_frontend_default_shortstring() &&
                    strcasecmp(node->sym->name, "string") == 0)
                {
                    return_type = SHORTSTRING_TYPE;
                    return_type_id = strdup(node->sym->name);
                }
            }
            if (return_type_ref == NULL)
                return_type_ref = type_ref_from_info_or_id(&type_info, return_type_id);
            if (return_type_id == NULL && node->sym != NULL && node->sym->name != NULL)
                return_type_id = strdup(node->sym->name);
            inline_return_type = build_inline_return_alias(&type_info, return_type, return_type_id);
            break;
        }
        case PASCAL_T_TYPE_SECTION:
            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                fprintf(stderr, "[KGPC] convert_function TYPE_SECTION at line=%d\n", node->line);
            }
            append_type_decls_from_section(node, &type_decls, &nested_subs,
                &const_decls, &var_builder, NULL);
            type_section_ast = node;  /* Save for const array enum resolution */
            break;
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(node, &const_decls, &var_builder, type_section_ast);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&var_builder, convert_var_section(node));
            break;
        case PASCAL_T_LABEL_SECTION:
            append_labels_from_section(node, &label_builder);
            break;
        case PASCAL_T_FUNCTION_DECL:
        case PASCAL_T_PROCEDURE_DECL: {
            /* Handle nested functions/procedures within method implementations */
            Tree_t *sub = (node->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(node)
                              : convert_function(node);
            append_subprogram_node(&nested_subs, sub);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(node, &const_decls, &var_builder, &label_builder,
                                 &nested_subs, &body, &type_decls);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(node);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(node);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            body = mk_compoundstatement(node->line, list_builder_finish(&stmts_builder));
            break;
        }
        case PASCAL_T_IDENTIFIER: {
            char *dir_sym = dup_symbol(node);
            if (dir_sym != NULL) {
                free(dir_sym);
            }
            break;
        }
        default:
            break;
        }
        cur = cur->next;
    }

    /* Constructors implicitly return their class instance as a pointer. */
    if (is_constructor && !has_return_type) {
        has_return_type = 1;
        if (return_type_id != NULL)
            free(return_type_id);
        return_type_id = (effective_class != NULL) ? strdup(effective_class) : NULL;
        return_type = POINTER_TYPE;
    }

    /* Wrap method body in WITH Self for instance methods, but not for helpers, class operators, or static methods */
    if (body != NULL && !is_class_operator && !is_static_method && !is_helper_method) {
        struct Expression *self_expr = mk_varid(method_node->line, strdup("Self"));
        body = mk_with(method_node->line, self_expr, body);
    }

    ListNode_t *params = list_builder_finish(&params_builder);
    ListNode_t *label_decls = list_builder_finish(&label_builder);

    if (is_class_operator && method_name != NULL && mangle_owner != NULL)
    {
        char *encoded_method = encode_operator_name(method_name);
        char *param_suffix = NULL;
        char *ret_suffix = NULL;
        if (params != NULL && params->cur != NULL)
            param_suffix = method_param_type_suffix((Tree_t *)params->cur);
        if (return_type_ref != NULL)
            ret_suffix = type_ref_render_mangled(return_type_ref);
        if (ret_suffix == NULL && return_type_id != NULL)
            ret_suffix = strdup(return_type_id);

        if (encoded_method != NULL && (param_suffix != NULL || ret_suffix != NULL))
        {
            size_t name_len = strlen(mangle_owner) + strlen(encoded_method) + 3;
            if (param_suffix != NULL)
                name_len += strlen(param_suffix) + 1;
            if (ret_suffix != NULL)
                name_len += strlen(ret_suffix) + 1;
            char *disambiguated = (char *)malloc(name_len);
            if (disambiguated != NULL)
            {
                int written = snprintf(disambiguated, name_len, "%s__%s", mangle_owner, encoded_method);
                if (written > 0 && (size_t)written < name_len)
                {
                    size_t used = (size_t)written;
                    if (param_suffix != NULL)
                    {
                        snprintf(disambiguated + used, name_len - used, "_%s", param_suffix);
                        used = strlen(disambiguated);
                    }
                    if (ret_suffix != NULL)
                        snprintf(disambiguated + used, name_len - used, "_%s", ret_suffix);
                    free(proc_name);
                    proc_name = disambiguated;
                }
                else
                {
                    free(disambiguated);
                }
            }
        }

        free(encoded_method);
        free(param_suffix);
        free(ret_suffix);
    }
    
    Tree_t *tree;
    if (has_return_type) {
        tree = mk_function(method_node->line, proc_name, params, const_decls,
                          label_decls, type_decls, list_builder_finish(&var_builder),
                          nested_subs, body, return_type, return_type_id, inline_return_type, 0, 0);
    } else {
        tree = mk_procedure(method_node->line, proc_name, params, const_decls,
                           label_decls, type_decls, list_builder_finish(&var_builder),
                           nested_subs, body, 0, 0);
    }
    if (tree != NULL && method_node->index >= 0)
        tree->source_index = method_node->index + g_source_offset;
    if (!is_nostackframe)
        is_nostackframe = ast_has_routine_directive(method_node, "nostackframe", 8);
    if (tree != NULL && is_nostackframe)
        tree->tree_data.subprogram_data.nostackframe = 1;
    if (tree != NULL) {
        if (has_return_type)
            tree->tree_data.subprogram_data.return_type_ref =
                return_type_ref != NULL ? return_type_ref : type_ref_from_single_name(return_type_id);
        else if (return_type_ref != NULL)
            type_ref_free(return_type_ref);
        /* Use the full dotted class path for mangled_id so that
         * semcheck_get_current_method_owner returns the full nested path.
         * This allows const lookup to fall back to outer classes.
         * E.g., for TManager.TState.Init, mangled_id = "TManager.TState__Init" */
        if (effective_class_full != NULL && effective_class_full != effective_class)
        {
            char *full_mangled = mangle_method_name_raw(effective_class_full, method_name);
            tree->tree_data.subprogram_data.mangled_id = full_mangled != NULL ? full_mangled : strdup(proc_name);
        }
        else
        {
            tree->tree_data.subprogram_data.mangled_id = strdup(proc_name);
        }
        tree->tree_data.subprogram_data.method_name = (char *)string_intern(method_name);
        tree->tree_data.subprogram_data.owner_class = (char *)string_intern(effective_class);
        tree->tree_data.subprogram_data.is_constructor = is_constructor;
        tree->tree_data.subprogram_data.is_static_method = is_static_method;
        tree->tree_data.subprogram_data.is_operator = method_declares_operator;
        if (effective_class_full != NULL && effective_class_full != effective_class)
            tree->tree_data.subprogram_data.owner_class_full = (char *)string_intern(effective_class_full);
        if (tree->tree_data.subprogram_data.owner_class_full == NULL &&
            tree->tree_data.subprogram_data.owner_class != NULL &&
            strchr(tree->tree_data.subprogram_data.owner_class, '.') != NULL)
        {
            tree->tree_data.subprogram_data.owner_class_full =
                tree->tree_data.subprogram_data.owner_class;
        }
        if (effective_class_outer != NULL)
            tree->tree_data.subprogram_data.owner_class_outer = (char *)string_intern(effective_class_outer);
        if (tree->tree_data.subprogram_data.owner_class_outer == NULL &&
            tree->tree_data.subprogram_data.owner_class_full != NULL)
        {
            const char *full = tree->tree_data.subprogram_data.owner_class_full;
            const char *last_dot = strrchr(full, '.');
            if (last_dot != NULL && last_dot > full)
            {
                size_t len = (size_t)(last_dot - full);
                char *temp = strndup(full, len);
                if (temp != NULL)
                {
                    tree->tree_data.subprogram_data.owner_class_outer = (char *)string_intern(temp);
                    free(temp);
                }
            }
        }
        if (num_generic_type_params > 0) {
            tree->tree_data.subprogram_data.generic_type_params = generic_type_params;
            tree->tree_data.subprogram_data.num_generic_type_params = num_generic_type_params;
            tree->tree_data.subprogram_data.is_generic_template = 1;
            tree->tree_data.subprogram_data.generic_template_ast = copy_ast_detached(method_node);
            tree->tree_data.subprogram_data.generic_template_source_offset = g_source_offset;
            generic_type_params = NULL;
            num_generic_type_params = 0;
        }
    }
    else if (return_type_ref != NULL)
    {
        type_ref_free(return_type_ref);
    }

    record_generic_method_impl(effective_class, method_name, method_node);
    
    /* Check if this method belongs to a generic type. If so, we've recorded the AST
     * template above and should NOT generate a concrete implementation. Return NULL
     * to prevent adding this to the subprograms list. */
    GenericTypeDecl *generic_decl = generic_registry_find_decl(effective_class);
    if (generic_decl != NULL && generic_decl->record_template != NULL) {
        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && effective_class != NULL && method_name != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: recorded template for %s.%s, not generating concrete impl\n", 
                    effective_class, method_name);
        }
        if (cleaned_class_name != NULL)
            free(cleaned_class_name);
        free(class_name);
        free(method_name);
        free(effective_class_last);
        free(effective_class_outer);
        if (method_param_sig != NULL)
            free(method_param_sig);
        g_current_method_name = prev_method_name_ctx;
        return NULL;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && effective_class != NULL && method_name != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class=%s method=%s\n", effective_class, method_name);
    }

    if (cleaned_class_name != NULL)
        free(cleaned_class_name);
    if (generic_type_params != NULL) {
        for (int i = 0; i < num_generic_type_params; i++)
            free(generic_type_params[i]);
        free(generic_type_params);
    }
    free(class_name);
    free(method_name);
    free(effective_class_last);
    free(effective_class_outer);
    if (method_param_sig != NULL)
        free(method_param_sig);
    g_current_method_name = prev_method_name_ctx;
    return tree;
}

/* Extract generic type parameter names from a PASCAL_T_TYPE_PARAM_LIST AST node.
 * Returns the number of type parameters found, and fills *out_params with
 * a malloc'd array of strdup'd parameter names. */
static int extract_generic_type_params(ast_t *type_param_list, char ***out_params) {
    assert(type_param_list != NULL);
    assert(type_param_list->typ == PASCAL_T_TYPE_PARAM_LIST);
    assert(out_params != NULL);

    int count = 0;
    for (ast_t *p = type_param_list->child; p != NULL; p = p->next) {
        if (p->typ == PASCAL_T_IDENTIFIER)
            count++;
    }
    if (count == 0) {
        *out_params = NULL;
        return 0;
    }
    char **params = (char **)malloc(sizeof(char *) * count);
    assert(params != NULL);
    int i = 0;
    for (ast_t *p = type_param_list->child; p != NULL; p = p->next) {
        if (p->typ == PASCAL_T_IDENTIFIER) {
            params[i] = dup_symbol(p);
            assert(params[i] != NULL);
            i++;
        }
    }
    *out_params = params;
    return count;
}

static char *extract_external_name_from_node(ast_t *node)
{
    if (node == NULL || node->child == NULL)
        return NULL;

    if (node->child->typ == PASCAL_T_STRING)
        return dup_symbol(node->child);

    if (node->child->typ == PASCAL_T_IDENTIFIER)
        return dup_symbol(node->child);

    if (node->child->typ == PASCAL_T_EXTERNAL_NAME_EXPR &&
        node->child->child != NULL &&
        (node->child->child->typ == PASCAL_T_STRING ||
         node->child->child->typ == PASCAL_T_IDENTIFIER))
        return dup_symbol(node->child->child);

    for (ast_t *c = node->child; c != NULL; c = c->next) {
        if (c->typ == PASCAL_T_STRING || c->typ == PASCAL_T_IDENTIFIER)
            return dup_symbol(c);
    }

    return NULL;
}

static Tree_t *convert_procedure(ast_t *proc_node) {
    ast_t *cur = proc_node->child;
    char *id = NULL;
    static int debug_external_nodes = -1;
    if (debug_external_nodes == -1)
        debug_external_nodes = (kgpc_getenv("KGPC_DEBUG_EXTERNAL") != NULL);

    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER &&
           cur->sym != NULL && cur->sym->name != NULL &&
           (strcasecmp(cur->sym->name, "generic") == 0 ||
            strcasecmp(cur->sym->name, "class") == 0))
    {
        cur = cur->next;
    }

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    /* Standalone operator declarations sometimes arrive through the
     * procedure-declaration grammar path even though they have a return type.
     * Reuse convert_function() so interface declarations get the same
     * mangled ids as implementations (e.g. TMyType__op_sub_TMyType). */
    if (id != NULL &&
        (strcasecmp(id, "operator") == 0 || is_operator_token_name(id)))
    {
        free(id);
        return convert_function(proc_node);
    }

    if (cur != NULL)
        cur = cur->next;

    /* Skip generic type parameter list (e.g., <T, U>) */
    char **generic_type_params = NULL;
    int num_generic_type_params = 0;
    if (cur != NULL && cur->typ == PASCAL_T_TYPE_PARAM_LIST) {
        num_generic_type_params = extract_generic_type_params(cur, &generic_type_params);
        cur = cur->next;
    }

    ListNode_t *params = NULL;
    if (cur != NULL && cur->typ == PASCAL_T_PARAM_LIST) {
        ast_t *param_cursor = cur->child;
        params = convert_param_list(&param_cursor);
        cur = cur->next;
    } else {
        while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
            ListNode_t *param_nodes = convert_param(cur);
            extend_list(&params, param_nodes);
            cur = cur->next;
        }
    }

    ListNode_t *const_decls = NULL;
    ListBuilder var_decls_builder;
    list_builder_init(&var_decls_builder);
    ListBuilder label_decls_builder;
    list_builder_init(&label_decls_builder);
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;
    int is_external = 0;
    int is_nostackframe = 0;
    int is_varargs = 0;
    char *external_alias = NULL;
    char *internproc_id_str = NULL;  /* Raw FPC INTERNPROC name (e.g. "fpc_in_Rewrite_TypedFile") */
    char *internconst_id_str = NULL; /* Raw FPC INTERNCONST name (e.g. "fpc_in_const_ptr") */
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;

    while (cur != NULL) {
        if (kgpc_getenv("KGPC_DEBUG_PROC_DIRECTIVE") != NULL) {
            fprintf(stderr, "[KGPC] convert_procedure directive node: typ=%d sym=%s child_typ=%d\n",
                cur->typ,
                (cur->sym != NULL && cur->sym->name != NULL) ? cur->sym->name : "<null>",
                (cur->child != NULL) ? cur->child->typ : -1);
        }
        switch (cur->typ) {
        case PASCAL_T_TYPE_SECTION:
            append_type_decls_from_section(cur, &type_decls, &nested_subs,
                &const_decls, &var_decls_builder, NULL);
            type_section_ast = cur;  /* Save for const array enum resolution */
            break;
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &const_decls, &var_decls_builder, type_section_ast);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&var_decls_builder, convert_var_section(cur));
            break;
        case PASCAL_T_LABEL_SECTION:
            append_labels_from_section(cur, &label_decls_builder);
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            append_subprogram_node(&nested_subs, sub);
            break;
        }
        case PASCAL_T_METHOD_IMPL: {
            Tree_t *method_tree = convert_method_impl(cur);
            append_subprogram_node(&nested_subs, method_tree);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &const_decls, &var_decls_builder, &label_decls_builder,
                                 &nested_subs, &body, &type_decls);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            body = mk_compoundstatement(cur->line, list_builder_finish(&stmts_builder));
            break;
        }
        case PASCAL_T_IDENTIFIER: {
            char *self_sym = dup_symbol(cur);
            if (self_sym != NULL) {
                if (strcasecmp(self_sym, "alias") == 0) {
                    /* [Alias:'NAME'] bracket directive — the string value is
                       either a child node or the next sibling (depending on
                       how the PEG grammar flattened the seq). */
                    ast_t *val = cur->child;
                    if (val == NULL || val->typ != PASCAL_T_STRING)
                        val = cur->next;  /* sibling layout */
                    if (val != NULL && val->typ == PASCAL_T_STRING) {
                        char *name = dup_symbol(val);
                        if (name != NULL) {
                            if (external_alias != NULL)
                                free(external_alias);
                            external_alias = name;
                        }
                        /* Skip the string node so the outer loop doesn't revisit it */
                        if (val == cur->next)
                            cur = cur->next;
                    }
                } else if (strcasecmp(self_sym, "internproc") == 0 ||
                           strcasecmp(self_sym, "internconst") == 0 ||
                           strcasecmp(self_sym, "compilerproc") == 0) {
                    is_external = 1;
                    /* The intrinsic value (e.g. fpc_in_trunc_real / fpc_in_const_ptr)
                       is the next sibling. */
                    ast_t *val = cur->next;
                    if (val != NULL && val->typ == PASCAL_T_IDENTIFIER &&
                        val->sym != NULL && val->sym->name != NULL) {
                        if (strcasecmp(self_sym, "internconst") == 0) {
                            if (internconst_id_str != NULL)
                                free(internconst_id_str);
                            internconst_id_str = strdup(val->sym->name);
                        } else {
                            if (internproc_id_str != NULL)
                                free(internproc_id_str);
                            internproc_id_str = strdup(val->sym->name);
                        }
                        if (external_alias != NULL)
                            free(external_alias);
                        external_alias = strdup(val->sym->name);
                        cur = cur->next;  /* Skip the value node */
                    }
                } else {
                    /* Fallback: if the identifier itself is an internproc value
                       (e.g. fpc_in_trunc_real), the "internproc" keyword was
                       consumed by the parser and only the value survived.
                       Only treat it as an internproc if it looks like an FPC intrinsic name. */
                    if (strncasecmp(self_sym, "fpc_in_", 7) == 0 ||
                        strncasecmp(self_sym, "fpc_", 4) == 0) {
                        is_external = 1;
                        if (external_alias != NULL)
                            free(external_alias);
                        external_alias = strdup(self_sym);
                        if (internproc_id_str != NULL)
                            free(internproc_id_str);
                        internproc_id_str = strdup(self_sym);
                    }
                }
                free(self_sym);
            }
            if (cur->child != NULL && cur->child->typ == PASCAL_T_IDENTIFIER) {
                char *directive = dup_symbol(cur->child);
                if (directive != NULL) {
                    if (is_external_directive(directive))
                        is_external = 1;
                }
                free(directive);
            }
            break;
        }
        case PASCAL_T_EXTERNAL_NAME:
        case PASCAL_T_EXTERNAL_NAME_EXPR:
            {
                char *name = extract_external_name_from_node(cur);
                if (name != NULL) {
                    if (external_alias != NULL)
                        free(external_alias);
                    external_alias = name;
                }
                is_external = 1;
            }
            break;
        case PASCAL_T_NONE:
            {
                for (ast_t *child = cur->child; child != NULL; child = child->next) {
                    if (child->typ == PASCAL_T_IDENTIFIER && child->sym != NULL) {
                        char *kw = child->sym->name;
                        if (strcasecmp(kw, "internproc") == 0 ||
                            strcasecmp(kw, "internconst") == 0 ||
                            strcasecmp(kw, "compilerproc") == 0) {
                            is_external = 1;
                            ast_t *val = child->next;
                            while (val != NULL && val->typ == PASCAL_T_NONE)
                                val = val->child;
                            if (val != NULL && val->typ == PASCAL_T_IDENTIFIER) {
                                if (strcasecmp(kw, "internconst") == 0) {
                                    if (internconst_id_str != NULL)
                                        free(internconst_id_str);
                                    internconst_id_str = dup_symbol(val);
                                } else {
                                    if (internproc_id_str != NULL)
                                        free(internproc_id_str);
                                    internproc_id_str = dup_symbol(val);
                                }
                                if (external_alias != NULL)
                                    free(external_alias);
                                external_alias = dup_symbol(val);
                            } else if (val != NULL && val->typ == PASCAL_T_STRING) {
                                if (strcasecmp(kw, "internconst") == 0) {
                                    if (internconst_id_str != NULL)
                                        free(internconst_id_str);
                                    internconst_id_str = dup_symbol(val);
                                } else {
                                    if (internproc_id_str != NULL)
                                        free(internproc_id_str);
                                    internproc_id_str = dup_symbol(val);
                                }
                                if (external_alias != NULL)
                                    free(external_alias);
                                external_alias = dup_symbol(val);
                            }
                        } else if (strcasecmp(kw, "external") == 0) {
                            is_external = 1;
                        }
                    } else if (child->typ == PASCAL_T_EXTERNAL_NAME || child->typ == PASCAL_T_EXTERNAL_NAME_EXPR) {
                        char *name = extract_external_name_from_node(child);
                        if (name != NULL) {
                            if (external_alias != NULL)
                                free(external_alias);
                            external_alias = name;
                        }
                        is_external = 1;
                    }
                }
            }
            break;
        default:
            break;
        }
        cur = cur->next;
    }

    if (!is_varargs)
        is_varargs = ast_has_routine_directive(proc_node, "varargs", 8);
    ListNode_t *label_decls = list_builder_finish(&label_decls_builder);
    Tree_t *tree = mk_procedure(proc_node->line, id, params, const_decls,
                                label_decls, type_decls, list_builder_finish(&var_decls_builder),
                                nested_subs, body, is_external, 0);
    if (tree != NULL && proc_node->index >= 0)
        tree->source_index = proc_node->index + g_source_offset;
    if (!is_nostackframe)
        is_nostackframe = ast_has_routine_directive(proc_node, "nostackframe", 8);
    if (tree != NULL && is_nostackframe)
        tree->tree_data.subprogram_data.nostackframe = 1;
    if (tree != NULL && is_varargs)
        tree->tree_data.subprogram_data.is_varargs = 1;
    if (tree != NULL && is_external && external_alias == NULL &&
        tree->tree_data.subprogram_data.id != NULL)
        external_alias = strdup(tree->tree_data.subprogram_data.id);
    if (tree != NULL && external_alias != NULL)
        tree->tree_data.subprogram_data.cname_override = external_alias;
    else if (external_alias != NULL)
        free(external_alias);
    if (tree != NULL && internproc_id_str != NULL)
        tree->tree_data.subprogram_data.internproc_id = internproc_id_str;
    else if (internproc_id_str != NULL)
        free(internproc_id_str);
    if (tree != NULL && internconst_id_str != NULL)
        tree->tree_data.subprogram_data.internconst_id = internconst_id_str;
    else if (internconst_id_str != NULL)
        free(internconst_id_str);
    if (tree != NULL && num_generic_type_params > 0) {
        tree->tree_data.subprogram_data.generic_type_params = generic_type_params;
        tree->tree_data.subprogram_data.num_generic_type_params = num_generic_type_params;
        tree->tree_data.subprogram_data.is_generic_template = 1;
        tree->tree_data.subprogram_data.generic_template_ast = copy_ast_detached(proc_node);
        tree->tree_data.subprogram_data.generic_template_source_offset = g_source_offset;
    } else if (generic_type_params != NULL) {
        for (int i = 0; i < num_generic_type_params; i++)
            free(generic_type_params[i]);
        free(generic_type_params);
    }
    return tree;
}

static Tree_t *convert_function(ast_t *func_node) {
    ast_t *cur = func_node->child;
    char *id = NULL;
    char *operator_symbol = NULL;  /* For standalone operators */
    int is_standalone_operator = 0;
    static int debug_external_nodes = -1;
    if (debug_external_nodes == -1)
        debug_external_nodes = (kgpc_getenv("KGPC_DEBUG_EXTERNAL") != NULL);

    while (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER &&
           cur->sym != NULL && cur->sym->name != NULL &&
           (strcasecmp(cur->sym->name, "generic") == 0 ||
            strcasecmp(cur->sym->name, "class") == 0))
    {
        cur = cur->next;
    }

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    /* Check if this is a standalone operator declaration */
    if (id != NULL &&
        (strcasecmp(id, "operator") == 0 || is_operator_token_name(id))) {
        is_standalone_operator = 1;
        if (is_operator_token_name(id)) {
            operator_symbol = strdup(id);
        } else if (cur != NULL) {
            /* Get the operator symbol from the next child */
            cur = cur->next;
            if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
                operator_symbol = dup_symbol(cur);
            }
        }
    }

    if (cur != NULL) {
        cur = cur->next;
    }

    /* Skip generic type parameter list (e.g., <T, U>) */
    char **generic_type_params = NULL;
    int num_generic_type_params = 0;
    if (cur != NULL && cur->typ == PASCAL_T_TYPE_PARAM_LIST) {
        num_generic_type_params = extract_generic_type_params(cur, &generic_type_params);
        cur = cur->next;
    }

    ListNode_t *params = NULL;
    if (cur != NULL && cur->typ == PASCAL_T_PARAM_LIST) {
        ast_t *param_cursor = cur->child;
        params = convert_param_list(&param_cursor);
        cur = cur->next;
    } else {
        while (cur != NULL && cur->typ == PASCAL_T_PARAM) {
            ListNode_t *param_nodes = convert_param(cur);
            extend_list(&params, param_nodes);
            cur = cur->next;
        }
    }

    /* For standalone operators, save operator info now but defer name mangling
     * until after the return type is parsed, so we can include the return type
     * in the mangled name to disambiguate overloads like variant__op_assign_byte
     * vs variant__op_assign_real. */
    char *deferred_encoded_op = NULL;
    const char *deferred_param_type_id = NULL;
    const char *deferred_second_param_type_id = NULL;
    if (is_standalone_operator && operator_symbol != NULL && params != NULL) {
        /* Get the type of the first parameter (params is a list of Tree_t* with TREE_VAR_DECL) */
        Tree_t *first_param = (Tree_t *)params->cur;
        if (kgpc_getenv("KGPC_DEBUG_OPERATOR") != NULL) {
            fprintf(stderr, "[Operator] is_standalone_operator=%d operator_symbol=%s\n",
                is_standalone_operator, operator_symbol);
        }
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.type_id != NULL) {
            deferred_encoded_op = encode_operator_name(operator_symbol);
            /* Capture second param type for binary operator disambiguation */
            if (params->next != NULL) {
                Tree_t *second_param = (Tree_t *)params->next->cur;
                if (second_param != NULL && second_param->type == TREE_VAR_DECL &&
                    second_param->tree_data.var_decl_data.type_id != NULL)
                    deferred_second_param_type_id = second_param->tree_data.var_decl_data.type_id;
            }
            deferred_param_type_id = first_param->tree_data.var_decl_data.type_id;
        }
        free(operator_symbol);
        operator_symbol = NULL;
    } else if (operator_symbol != NULL) {
        free(operator_symbol);
    }

    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;
    struct TypeAlias *inline_return_type = NULL;
    char *result_var_name = NULL;
    TypeRef *return_type_ref = NULL;

    /* For operator declarations with named results (e.g., "operator :=(source: byte) dest: variant"),
     * the named result identifier appears as a PASCAL_T_IDENTIFIER node before PASCAL_T_RETURN_TYPE. */
    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER && is_standalone_operator) {
        result_var_name = dup_symbol(cur);
        cur = cur->next;
    }

    if (cur != NULL && cur->typ == PASCAL_T_RETURN_TYPE) {
        TypeInfo type_info = {0};
        return_type = convert_type_spec(cur->child, &return_type_id, NULL, &type_info);
        /* When the PASCAL_T_RETURN_TYPE node has no child (e.g. interface/forward
         * declarations where the parser strips the type subtree), fall back to
         * the sym name on the RETURN_TYPE node itself.  Only apply the {$H-}
         * remapping for bare 'string'; leave other types as UNKNOWN_TYPE for
         * semcheck to resolve via the symbol table. */
        if (return_type == UNKNOWN_TYPE && return_type_id == NULL &&
            cur->sym != NULL && cur->sym->name != NULL)
        {
            if (pascal_frontend_default_shortstring() &&
                strcasecmp(cur->sym->name, "string") == 0)
            {
                return_type = SHORTSTRING_TYPE;
                return_type_id = strdup(cur->sym->name);
            }
        }
        if (return_type_ref == NULL)
            return_type_ref = type_ref_from_info_or_id(&type_info, return_type_id);
        if (return_type_id == NULL && cur->sym != NULL && cur->sym->name != NULL)
            return_type_id = strdup(cur->sym->name);
        inline_return_type = build_inline_return_alias(&type_info, return_type, return_type_id);
        cur = cur->next;
    }

    /* Build operator names: the base name (TypeName__op_suffix) goes into id
     * so FindAllIdents can find all overloads.  The full suffixed name (with
     * param types and return type) goes into mangled_id for unique asm labels,
     * avoiding collisions between overloads with same params but different
     * return types (e.g. Tconstexprint__op_assign returning qword vs int64). */
    char *operator_mangled_id = NULL;
    if (deferred_encoded_op != NULL && deferred_param_type_id != NULL) {
        const char *ret_suffix = return_type_id;
        if (ret_suffix == NULL) ret_suffix = result_var_name;

        /* Set id to the base name: TypeName__op_suffix */
        size_t base_len = strlen(deferred_param_type_id) + strlen(deferred_encoded_op) + 3;
        char *base_name = (char *)malloc(base_len);
        if (base_name != NULL) {
            snprintf(base_name, base_len, "%s__%s",
                deferred_param_type_id, deferred_encoded_op);
            free(id);
            id = base_name;
        }

        /* Build the full suffixed name for mangled_id */
        size_t name_len = strlen(deferred_param_type_id) + strlen(deferred_encoded_op) + 3
            + (ret_suffix != NULL ? strlen(ret_suffix) + 1 : 0)
            + (deferred_second_param_type_id != NULL ? strlen(deferred_second_param_type_id) + 1 : 0);
        char *mangled_name = (char *)malloc(name_len);
        if (mangled_name != NULL) {
            if (ret_suffix != NULL && deferred_second_param_type_id != NULL) {
                snprintf(mangled_name, name_len, "%s__%s_%s_%s",
                    deferred_param_type_id, deferred_encoded_op,
                    deferred_second_param_type_id, ret_suffix);
            } else if (ret_suffix != NULL) {
                snprintf(mangled_name, name_len, "%s__%s_%s",
                    deferred_param_type_id, deferred_encoded_op, ret_suffix);
            } else if (deferred_second_param_type_id != NULL) {
                snprintf(mangled_name, name_len, "%s__%s_%s",
                    deferred_param_type_id, deferred_encoded_op,
                    deferred_second_param_type_id);
            } else {
                snprintf(mangled_name, name_len, "%s__%s",
                    deferred_param_type_id, deferred_encoded_op);
            }
            operator_mangled_id = mangled_name;
        }
        free(deferred_encoded_op);
        deferred_encoded_op = NULL;
    }
    if (deferred_encoded_op != NULL) free(deferred_encoded_op);

    ListNode_t *const_decls = NULL;
    ListBuilder var_decls_builder;
    list_builder_init(&var_decls_builder);
    ListBuilder label_decls_builder;
    list_builder_init(&label_decls_builder);
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;
    int is_external = 0;
    int is_nostackframe = 0;
    int is_varargs = 0;
    char *external_alias = NULL;
    char *internproc_id_str = NULL;  /* Raw FPC INTERNPROC name */
    char *internconst_id_str = NULL; /* Raw FPC INTERNCONST name */
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;

    while (cur != NULL) {

        switch (cur->typ) {
        case PASCAL_T_TYPE_SECTION:
            append_type_decls_from_section(cur, &type_decls, &nested_subs,
                &const_decls, &var_decls_builder, NULL);
            type_section_ast = cur;  /* Save for const array enum resolution */
            break;
        case PASCAL_T_CONST_SECTION:
            append_const_decls_from_section(cur, &const_decls, &var_decls_builder, type_section_ast);
            break;
        case PASCAL_T_VAR_SECTION:
            list_builder_extend(&var_decls_builder, convert_var_section(cur));
            break;
        case PASCAL_T_LABEL_SECTION:
            append_labels_from_section(cur, &label_decls_builder);
            break;
        case PASCAL_T_PROCEDURE_DECL:
        case PASCAL_T_FUNCTION_DECL: {
            Tree_t *sub = (cur->typ == PASCAL_T_PROCEDURE_DECL)
                              ? convert_procedure(cur)
                              : convert_function(cur);
            append_subprogram_node(&nested_subs, sub);
            break;
        }
        case PASCAL_T_METHOD_IMPL: {
            Tree_t *method_tree = convert_method_impl(cur);
            append_subprogram_node(&nested_subs, method_tree);
            break;
        }
        case PASCAL_T_FUNCTION_BODY:
            convert_routine_body(cur, &const_decls, &var_decls_builder, &label_decls_builder,
                                 &nested_subs, &body, &type_decls);
            break;
        case PASCAL_T_BEGIN_BLOCK:
            body = convert_block(cur);
            break;
        case PASCAL_T_ASM_BLOCK: {
            struct Statement *stmt = convert_statement(cur);
            ListBuilder stmts_builder;
            list_builder_init(&stmts_builder);
            if (stmt != NULL)
                list_builder_append(&stmts_builder, stmt, LIST_STMT);
            body = mk_compoundstatement(cur->line, list_builder_finish(&stmts_builder));
            break;
        }
        case PASCAL_T_IDENTIFIER: {
            char *self_sym = dup_symbol(cur);
            if (self_sym != NULL) {
                if (is_external_directive(self_sym))
                    is_external = 1;
                else if (strcasecmp(self_sym, "alias") == 0) {
                    /* [Alias:'NAME'] bracket directive — the string value is
                       either a child node or the next sibling (depending on
                       how the PEG grammar flattened the seq). */
                    ast_t *val = cur->child;
                    if (val == NULL || val->typ != PASCAL_T_STRING)
                        val = cur->next;  /* sibling layout */
                    if (val != NULL && val->typ == PASCAL_T_STRING) {
                        char *name = dup_symbol(val);
                        if (name != NULL) {
                            if (external_alias != NULL)
                                free(external_alias);
                            external_alias = name;
                        }
                        /* Skip the string node so the outer loop doesn't revisit it */
                        if (val == cur->next)
                            cur = cur->next;
                    }
                } else if (strcasecmp(self_sym, "internproc") == 0 ||
                           strcasecmp(self_sym, "internconst") == 0 ||
                           strcasecmp(self_sym, "compilerproc") == 0) {
                    is_external = 1;
                    /* The intrinsic value (e.g. fpc_in_trunc_real / fpc_in_const_ptr)
                       is the next sibling. */
                    ast_t *val = cur->next;
                    if (val != NULL && val->typ == PASCAL_T_IDENTIFIER &&
                        val->sym != NULL && val->sym->name != NULL) {
                        if (strcasecmp(self_sym, "internconst") == 0) {
                            if (internconst_id_str != NULL)
                                free(internconst_id_str);
                            internconst_id_str = strdup(val->sym->name);
                        } else {
                            if (internproc_id_str != NULL)
                                free(internproc_id_str);
                            internproc_id_str = strdup(val->sym->name);
                        }
                        if (external_alias != NULL)
                            free(external_alias);
                        external_alias = strdup(val->sym->name);
                        cur = cur->next;  /* Skip the value node */
                    }
                } else {
                    /* Fallback: if the identifier itself is an internproc value
                       (e.g. fpc_in_trunc_real), the "internproc" keyword was
                       consumed by the parser and only the value survived.
                       Only treat it as an internproc if it looks like an FPC intrinsic name. */
                    if (strncasecmp(self_sym, "fpc_in_", 7) == 0 ||
                        strncasecmp(self_sym, "fpc_", 4) == 0) {
                        is_external = 1;
                        if (external_alias != NULL)
                            free(external_alias);
                        external_alias = strdup(self_sym);
                        if (internproc_id_str != NULL)
                            free(internproc_id_str);
                        internproc_id_str = strdup(self_sym);
                    }
                }
                free(self_sym);
            }

            if (cur->child != NULL && cur->child->typ == PASCAL_T_IDENTIFIER) {
                char *directive = dup_symbol(cur->child);
                if (directive != NULL) {
                    if (is_external_directive(directive))
                        is_external = 1;
                }
                free(directive);
            }
            break;
        }
        case PASCAL_T_EXTERNAL_NAME:
        case PASCAL_T_EXTERNAL_NAME_EXPR:
            {
                char *name = extract_external_name_from_node(cur);
                if (name != NULL) {
                    if (external_alias != NULL)
                        free(external_alias);
                    external_alias = name;
                }
                is_external = 1;
            }
            break;
        case PASCAL_T_NONE:
            {
                for (ast_t *child = cur->child; child != NULL; child = child->next) {
                    if (child->typ == PASCAL_T_IDENTIFIER && child->sym != NULL) {
                        char *kw = child->sym->name;
                        if (strcasecmp(kw, "internproc") == 0 ||
                            strcasecmp(kw, "internconst") == 0 ||
                            strcasecmp(kw, "compilerproc") == 0) {
                            is_external = 1;
                            ast_t *val = child->next;
                            while (val != NULL && val->typ == PASCAL_T_NONE)
                                val = val->child;
                            if (val != NULL && val->typ == PASCAL_T_IDENTIFIER) {
                                if (strcasecmp(kw, "internconst") == 0) {
                                    if (internconst_id_str != NULL)
                                        free(internconst_id_str);
                                    internconst_id_str = dup_symbol(val);
                                } else {
                                    if (internproc_id_str != NULL)
                                        free(internproc_id_str);
                                    internproc_id_str = dup_symbol(val);
                                }
                                if (external_alias != NULL)
                                    free(external_alias);
                                external_alias = dup_symbol(val);
                            } else if (val != NULL && val->typ == PASCAL_T_STRING) {
                                if (strcasecmp(kw, "internconst") == 0) {
                                    if (internconst_id_str != NULL)
                                        free(internconst_id_str);
                                    internconst_id_str = dup_symbol(val);
                                } else {
                                    if (internproc_id_str != NULL)
                                        free(internproc_id_str);
                                    internproc_id_str = dup_symbol(val);
                                }
                                if (external_alias != NULL)
                                    free(external_alias);
                                external_alias = dup_symbol(val);
                            }
                        } else if (strcasecmp(kw, "external") == 0) {
                            is_external = 1;
                        }
                    } else if (child->typ == PASCAL_T_EXTERNAL_NAME || child->typ == PASCAL_T_EXTERNAL_NAME_EXPR) {
                        char *name = extract_external_name_from_node(child);
                        if (name != NULL) {
                            if (external_alias != NULL)
                                free(external_alias);
                            external_alias = name;
                        }
                        is_external = 1;
                    }
                }
            }
            break;
        default:
            break;
        }
        cur = cur->next;
    }

    if (!is_varargs)
        is_varargs = ast_has_routine_directive(func_node, "varargs", 8);
    ListNode_t *label_decls = list_builder_finish(&label_decls_builder);
    Tree_t *tree = mk_function(func_node->line, id, params, const_decls,
                                label_decls, type_decls, list_builder_finish(&var_decls_builder), nested_subs, body,
                                return_type, return_type_id, inline_return_type, is_external, 0);
    if (tree != NULL && func_node->index >= 0)
        tree->source_index = func_node->index + g_source_offset;
    if (!is_nostackframe)
        is_nostackframe = ast_has_routine_directive(func_node, "nostackframe", 8);
    if (tree != NULL && is_nostackframe)
        tree->tree_data.subprogram_data.nostackframe = 1;
    if (tree != NULL && is_varargs)
        tree->tree_data.subprogram_data.is_varargs = 1;
    if (tree != NULL)
        tree->tree_data.subprogram_data.return_type_ref =
            return_type_ref != NULL ? return_type_ref : type_ref_from_single_name(return_type_id);
    else if (return_type_ref != NULL)
        type_ref_free(return_type_ref);
    if (tree != NULL && is_external && external_alias == NULL &&
        tree->tree_data.subprogram_data.id != NULL)
        external_alias = strdup(tree->tree_data.subprogram_data.id);
    if (tree != NULL && external_alias != NULL)
        tree->tree_data.subprogram_data.cname_override = external_alias;
    else if (external_alias != NULL)
        free(external_alias);
    if (tree != NULL && internproc_id_str != NULL)
        tree->tree_data.subprogram_data.internproc_id = internproc_id_str;
    else if (internproc_id_str != NULL)
        free(internproc_id_str);
    if (tree != NULL && internconst_id_str != NULL)
        tree->tree_data.subprogram_data.internconst_id = internconst_id_str;
    else if (internconst_id_str != NULL)
        free(internconst_id_str);
    if (tree != NULL && num_generic_type_params > 0) {
        tree->tree_data.subprogram_data.generic_type_params = generic_type_params;
        tree->tree_data.subprogram_data.num_generic_type_params = num_generic_type_params;
        tree->tree_data.subprogram_data.is_generic_template = 1;
        tree->tree_data.subprogram_data.generic_template_ast = copy_ast_detached(func_node);
        tree->tree_data.subprogram_data.generic_template_source_offset = g_source_offset;
    } else if (generic_type_params != NULL) {
        for (int i = 0; i < num_generic_type_params; i++)
            free(generic_type_params[i]);
        free(generic_type_params);
    }
    if (tree != NULL && result_var_name != NULL) {
        tree->tree_data.subprogram_data.result_var_name = result_var_name;
    } else if (result_var_name != NULL) {
        free(result_var_name);
    }
    if (tree != NULL && is_standalone_operator)
        tree->tree_data.subprogram_data.is_operator = 1;
    if (tree != NULL && operator_mangled_id != NULL)
        tree->tree_data.subprogram_data.mangled_id = operator_mangled_id;
    else if (operator_mangled_id != NULL)
        free(operator_mangled_id);
    return tree;
}

/**
 * Helper function to recursively search for an AST node with a specific type.
 * Uses depth-first search, checking children before siblings.
 * 
 * Traversal order:
 * 1. Check if current node matches target_type
 * 2. Recursively search in child chain
 * 3. Recursively search in sibling chain
 * 
 * @param node The node to start searching from
 * @param target_type The type ID to search for
 * @return The first node found with the target type, or NULL if not found
 */
static ast_t *find_node_by_type(ast_t *node, int target_type) {
    if (node == NULL) {
        return NULL;
    }
    
    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] find_node_by_type: visiting node typ=%d, looking for typ=%d\n", 
                node->typ, target_type);
    }
    
    if (node->typ == target_type) {
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] find_node_by_type: FOUND target typ=%d\n", target_type);
        }
        return node;
    }
    
    /* Search in children */
    ast_t *result = find_node_by_type(node->child, target_type);
    if (result != NULL) {
        return result;
    }
    
    /* Search in siblings */
    return find_node_by_type(node->next, target_type);
}

/**
 * Helper to find the last occurrence of a node type in a subtree.
 * Performs a depth-first traversal but keeps updating the result so that
 * the deepest/rightmost match is returned. This is useful when multiple
 * BEGIN blocks exist (e.g., function bodies plus program body) and we want
 * the final program block rather than the first nested block.
 */
static ast_t *find_last_node_by_type(ast_t *node, int target_type) {
    ast_t *last = NULL;
    while (node != NULL) {
        /* Recurse into children first to honor depth-first ordering */
        ast_t *child_last = find_last_node_by_type(node->child, target_type);
        if (child_last != NULL)
            last = child_last;
        
        if (node->typ == target_type)
            last = node;
        
        node = node->next;
    }
    return last;
}

static void resolve_deferred_arrays_in_list(ListNode_t *decl_list)
{
    for (ListNode_t *cur = decl_list; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_TREE) continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_ARR_DECL) continue;
        if (decl->tree_data.arr_decl_data.unresolved_index_type == NULL) continue;

        const char *type_name = decl->tree_data.arr_decl_data.unresolved_index_type;
        int start, end;
        if (enum_registry_lookup(type_name, &start, &end) == 0) {
            decl->tree_data.arr_decl_data.s_range = start;
            decl->tree_data.arr_decl_data.e_range = end;
            free(decl->tree_data.arr_decl_data.unresolved_index_type);
            decl->tree_data.arr_decl_data.unresolved_index_type = NULL;
        } else {
            const char *id = "(unknown)";
            if (decl->tree_data.arr_decl_data.ids != NULL &&
                decl->tree_data.arr_decl_data.ids->cur != NULL)
                id = (const char *)decl->tree_data.arr_decl_data.ids->cur;
            /* Not an error: semcheck will resolve enum-indexed array bounds
             * from the symbol table at a later stage. */
        }
    }
}

void from_cparser_resolve_deferred_arrays(Tree_t *program)
{
    if (program == NULL) return;

    if (program->type == TREE_PROGRAM_TYPE) {
        resolve_deferred_arrays_in_list(program->tree_data.program_data.var_declaration);
    }
}

void from_cparser_cleanup(void)
{
    /* Free type helper mappings (strdup'd strings + struct + list nodes) */
    while (type_helper_mappings != NULL) {
        ListNode_t *next = type_helper_mappings->next;
        struct TypeHelperMapping *entry = (struct TypeHelperMapping *)type_helper_mappings->cur;
        if (entry != NULL) {
            free(entry->helper_id);
            free(entry->base_type_id);
            free(entry);
        }
        free(type_helper_mappings);
        type_helper_mappings = next;
    }

    /* Free class method bindings (interned strings - do NOT free, just free structs + list nodes) */
    cmb_index_reset();
    cmb_method_index_reset();
    while (class_method_bindings != NULL) {
        ListNode_t *next = class_method_bindings->next;
        ClassMethodBinding *binding = (ClassMethodBinding *)class_method_bindings->cur;
        if (binding != NULL && binding->param_sig != NULL)
            free(binding->param_sig);
        free(binding); /* ClassMethodBinding struct */
        free(class_method_bindings);
        class_method_bindings = next;
    }

    /* Free pending generic aliases */
    while (g_pending_generic_aliases != NULL) {
        PendingGenericAlias *next = g_pending_generic_aliases->next;
        free(g_pending_generic_aliases->base_name);
        if (g_pending_generic_aliases->type_args != NULL)
            destroy_list(g_pending_generic_aliases->type_args);
        free(g_pending_generic_aliases);
        g_pending_generic_aliases = next;
    }

    /* Free deferred inline specializations */
    while (g_deferred_inline_specs != NULL) {
        DeferredInlineSpec *next = g_deferred_inline_specs->next;
        if (g_deferred_inline_specs->type_decl != NULL)
            destroy_tree(g_deferred_inline_specs->type_decl);
        free(g_deferred_inline_specs);
        g_deferred_inline_specs = next;
    }

    /* Free scoped enum source cache */
    free(g_scoped_enum_source_path);
    g_scoped_enum_source_path = NULL;
    free(g_scoped_enum_source_buffer);
    g_scoped_enum_source_buffer = NULL;
    g_scoped_enum_source_length = 0;

    /* Reset const sections */
    reset_const_sections();

    /* Clear borrowed AST pointers */
    g_interface_type_section_ast = NULL;
    g_implementation_type_section_ast = NULL;
    g_interface_section_ast = NULL;
    g_implementation_section_ast = NULL;
    g_current_method_name = NULL;

    /* Reset counters */
    anonymous_method_counter = 0;
    typed_const_counter = 0;
    g_allow_pending_specializations = 0;
    g_frontend_error_count = 0;

    /* Free cross-unit enum registry */
    enum_registry_free();
}

Tree_t *tree_from_pascal_ast(ast_t *program_ast) {
    Tree_t *final_tree = NULL;
    if (program_ast == NULL)
        return NULL;

    typed_const_counter = 0;
    g_typed_const_unit_tag = "p";
    reset_const_sections();
    g_interface_type_section_ast = NULL;
    g_implementation_type_section_ast = NULL;

    ast_t *cur = program_ast;
    if (cur->typ == PASCAL_T_NONE)
        cur = cur->child;

    if (cur == NULL) {
        fprintf(stderr, "ERROR: Empty Pascal AST.\n");
        return NULL;
    }

    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] tree_from_pascal_ast: root typ=%d\n", cur->typ);
    }
    
    if (cur->typ == PASCAL_T_PROGRAM_DECL || cur->typ == 88) {
        /* The structure of PASCAL_T_PROGRAM_DECL is:
         *   - optional(program_header)       [PASCAL_T_PROGRAM_HEADER or NULL/skipped]
         *   - optional(uses_section)          [PASCAL_T_USES_SECTION or NULL/skipped]
         *   - many(declaration_or_section)    [list of sections]
         *   - optional(main_block)            [PASCAL_T_MAIN_BLOCK or NULL/skipped]
         *   - "."
         * We need to check if the first child is a PASCAL_T_PROGRAM_HEADER.
         */
        ast_t *first_child = cur->child;
        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] tree_from_pascal_ast: program block entered. first_child=%p\n", first_child);
            if (first_child) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: first_child->typ=%d\n", first_child->typ);
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: first_child->next=%p\n", first_child->next);
                
                // Print all siblings to understand the structure
                int sibling_count = 0;
                ast_t *sibling = first_child;
                while (sibling != NULL && sibling_count < 100) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: sibling[%d] typ=%d next=%p child=%p\n", 
                            sibling_count, sibling->typ, sibling->next, sibling->child);
                    sibling = sibling->next;
                    sibling_count++;
                }
                if (sibling != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: ... more siblings ...\n");
                }
                
                // Also check if there are children beyond the sibling chain
                // by looking at the last sibling's child
                if (sibling_count > 0) {
                    ast_t *last = first_child;
                    while (last->next != NULL) last = last->next;
                    if (last->child != NULL) {
                        fprintf(stderr, "[KGPC] tree_from_pascal_ast: Last sibling (typ=%d) has children:\n", last->typ);
                        ast_t *child = last->child;
                        int child_count = 0;
                        while (child != NULL && child_count < 20) {
                            fprintf(stderr, "[KGPC] tree_from_pascal_ast:   child[%d] typ=%d next=%p\n", child_count, child->typ, child->next);
                            
                            // Print siblings of this child
                            if (child->next != NULL) {
                                fprintf(stderr, "[KGPC] tree_from_pascal_ast:     Siblings of child[%d]:\n", child_count);
                                ast_t *sibling = child->next;
                                int sib_count = 0;
                                while (sibling != NULL && sib_count < 10) {
                                    fprintf(stderr, "[KGPC] tree_from_pascal_ast:       sibling[%d] typ=%d\n", sib_count, sibling->typ);
                                    sibling = sibling->next;
                                    sib_count++;
                                }
                            }
                            
                            child = child->next;
                            child_count++;
                        }
                    }
                }
            }
        }
        ast_t *program_header_node = NULL;
        char *program_id = NULL;
        
        /* Check if the first child is a program header */
        if (first_child != NULL && first_child->typ == PASCAL_T_PROGRAM_HEADER) {
            program_header_node = first_child;
            /* PASCAL_T_PROGRAM_HEADER structure:
             *   - "program" keyword (discarded by parser)
             *   - identifier (program name)
             *   - optional parameter list
             *   - ";" (discarded)
             */
            ast_t *name_node = program_header_node->child;
            if (name_node != NULL) {
                program_id = dup_symbol(name_node);
            }
        }
        
        /* If no program header or no program name, use default */
        if (program_id == NULL) {
            program_id = strdup("program");
        }

        ListNode_t *args = NULL;  /* Program parameters not currently supported */
        ListNode_t *uses = NULL;
        ListNode_t *const_decls = NULL;
        ListBuilder var_decls_builder;
        list_builder_init(&var_decls_builder);
        ListBuilder label_builder;
        list_builder_init(&label_builder);
        ListNode_t *type_decls = NULL;
        ast_t *type_section_ast = NULL;  /* Keep AST for enum resolution */
        ListNode_t *subprograms = NULL;
        struct Statement *body = NULL;
        int body_line = -1;

        /* Create visited set to detect circular references in top-level sections */
        VisitedSet *visited = visited_set_create();
        if (visited == NULL) {
            fprintf(stderr, "ERROR: Failed to allocate visited set for program sections\n");
            free(program_id);
            return NULL;
        }

        /* Start iterating from the first child, or the node after the program header */
        ast_t *section = program_header_node != NULL ? program_header_node->next : first_child;
        while (section != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_PROGRAM_SECTIONS") != NULL) {
                fprintf(stderr, "[kgpc program] section typ=%d (%s) line=%d\n",
                    section->typ, pascal_tag_to_string(section->typ), section->line);
            }
            /* Check for circular reference before processing */
            if (!is_safe_to_continue(visited, section)) {
                fprintf(stderr, "ERROR: Circular reference detected in program sections, stopping traversal\n");
                break;
            }
            
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting PROGRAM section type %d\n", section->typ);
            }
            
            switch (section->typ) {
            case PASCAL_T_CONST_SECTION:
                append_const_decls_from_section(section, &const_decls, &var_decls_builder, type_section_ast);
                break;
            case PASCAL_T_VAR_SECTION:
                list_builder_extend(&var_decls_builder, convert_var_section(section));
                break;
            case PASCAL_T_TYPE_SECTION:
                type_section_ast = section;  /* Save for enum resolution */
                append_type_decls_from_section(section, &type_decls, &subprograms,
                    &const_decls, &var_decls_builder, NULL);
                break;
            case PASCAL_T_USES_SECTION:
                append_uses_from_section(section, &uses);
                break;
            case PASCAL_T_LABEL_SECTION:
                append_labels_from_section(section, &label_builder);
                break;
            case PASCAL_T_PROCEDURE_DECL:
            case PASCAL_T_FUNCTION_DECL: {
                Tree_t *sub = (section->typ == PASCAL_T_PROCEDURE_DECL)
                                  ? convert_procedure(section)
                                  : convert_function(section);
                append_subprogram_node(&subprograms, sub);
                break;
            }
            case PASCAL_T_METHOD_IMPL:
            case PASCAL_T_CONSTRUCTOR_DECL:
            case PASCAL_T_DESTRUCTOR_DECL: {
                Tree_t *method_tree = convert_method_impl(section);
                append_subprogram_node(&subprograms, method_tree);
                break;
            }
            case PASCAL_T_PROPERTY_DECL: {
                /* Module-level property declaration (FPC extension). */
                append_module_property_wrappers(&subprograms, section);
                break;
            }
            case PASCAL_T_BEGIN_BLOCK:
            case PASCAL_T_MAIN_BLOCK: {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found block type %d\n", section->typ);
                }
                struct Statement *candidate_body = convert_block(section);
                /* Prefer the last non-empty block. If the candidate has no statements,
                 * keep the previous body (if any) but still remember the candidate if
                 * we haven't found anything else. */
                if (candidate_body != NULL &&
                    candidate_body->stmt_data.compound_statement != NULL) {
                    if (section->line >= body_line) {
                        body = candidate_body;
                        body_line = section->line;
                    }
                } else if (body == NULL) {
                    body = candidate_body;
                    body_line = section->line;
                }
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: body assigned, body=%p\n", body);
                }
                break;
            }
            case PASCAL_T_TYPE_SPEC:
            case PASCAL_T_VAR_DECL:
            case PASCAL_T_TYPE_DECL:
            case PASCAL_T_CONST_DECL:
                /* These are components of sections, not top-level sections themselves.
                 * They should be children of VAR_SECTION, TYPE_SECTION, or CONST_SECTION.
                 * If we encounter them here, it means the AST structure is malformed,
                 * but we can safely skip them to continue parsing. */
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Skipping declaration component type %d (should be child of section)\n", section->typ);
                }
                break;
            default:
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Skipping unknown node type %d\n", section->typ);
                }
                break;
            }
            section = section->next;
        }
        
        /* If we haven't found a main block, search recursively in the AST tree.
         * This handles the case where the parser's seq() and many() combinators
         * don't properly link subsequent children in the sibling chain. */
        if (body == NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Main block not found in sibling chain, searching recursively\n");
            }
            
            /* Search for VAR_SECTION */
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Searching for VAR_SECTION (typ=%d)\n", PASCAL_T_VAR_SECTION);
            }
            ast_t* var_section_node = find_node_by_type(cur->child, PASCAL_T_VAR_SECTION);
            if (var_section_node != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found VAR_SECTION via recursive search\n");
                }
                list_builder_extend(&var_decls_builder, convert_var_section(var_section_node));
            }
            
            /* Search for MAIN_BLOCK */
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Searching for MAIN_BLOCK (typ=%d)\n", PASCAL_T_MAIN_BLOCK);
            }
            ast_t* main_block_node = find_last_node_by_type(cur->child, PASCAL_T_MAIN_BLOCK);
            if (main_block_node == NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found, searching for BEGIN_BLOCK (typ=%d)\n", PASCAL_T_BEGIN_BLOCK);
                }
                main_block_node = find_last_node_by_type(cur->child, PASCAL_T_BEGIN_BLOCK);
            }
            /* Also try typ=112 which appears in the AST */
            if (main_block_node == NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: BEGIN_BLOCK not found, trying typ=112\n");
                }
                main_block_node = find_last_node_by_type(cur->child, 112);
            }
            if (main_block_node != NULL) {
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found MAIN_BLOCK via recursive search (typ=%d)\n", main_block_node->typ);
                }
                body = convert_block(main_block_node);
            } else {
                /* MAIN_BLOCK not found directly. Check if there's a typ=100 node that contains it */
                if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found directly, searching for typ=100 wrapper\n");
                }
                ast_t* wrapper_node = find_last_node_by_type(cur->child, 100);
                if (wrapper_node != NULL && wrapper_node->child != NULL) {
                    if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                        fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found typ=100 wrapper, checking its children\n");
                    }
                    /* Check if the wrapper's child is a MAIN_BLOCK or BEGIN_BLOCK */
                    ast_t* child = wrapper_node->child;
                    int child_count = 0;
                    while (child != NULL) {
                        if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                            fprintf(stderr, "[KGPC] tree_from_pascal_ast: typ=100 child[%d] has typ=%d\n", child_count, child->typ);
                        }
                        if (child->typ == PASCAL_T_MAIN_BLOCK || child->typ == PASCAL_T_BEGIN_BLOCK) {
                            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found MAIN_BLOCK (typ=%d) inside typ=100 wrapper\n", child->typ);
                            }
                            main_block_node = child;
                            body = convert_block(main_block_node);
                            break;
                        }
                        child = child->next;
                        child_count++;
                    }
                }
                
                if (body == NULL && kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found via recursive search\n");
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: PASCAL_T_MAIN_BLOCK=%d, PASCAL_T_BEGIN_BLOCK=%d\n", 
                            PASCAL_T_MAIN_BLOCK, PASCAL_T_BEGIN_BLOCK);
                }
            }
        }

        visited_set_destroy(visited);
        
        
        ListNode_t *label_decls = list_builder_finish(&label_builder);
        
        /* If no main block was found, create an empty one to avoid NULL body */
        if (body == NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: No main block found, creating empty body\\n");
            }
            body = mk_compoundstatement(cur->line, NULL);
        }
        
        Tree_t *tree = mk_program(cur->line, program_id, args, uses, label_decls, const_decls,
                                  list_builder_finish(&var_decls_builder), type_decls, subprograms, body);
        final_tree = tree;
        /* Clear borrowed AST pointers before the caller frees the raw AST. */
        g_interface_type_section_ast = NULL;
        g_implementation_type_section_ast = NULL;
        return final_tree;
    }


    if (cur->typ == PASCAL_T_UNIT_DECL) {
        ast_t *unit_name_node = cur->child;
        char *unit_id = unit_name_node != NULL ? dup_symbol(unit_name_node) : strdup("unit");
        /* Lowercase the unit tag for deterministic typed-const labels */
        for (char *p = unit_id; p != NULL && *p != '\0'; p++)
            *p = (char)tolower((unsigned char)*p);
        g_typed_const_unit_tag = unit_id;
        typed_const_counter = 0;
        ast_t *unit_scan_copy = copy_ast(cur);

        ListNode_t *interface_uses = NULL;
        ListNode_t *interface_const_decls = NULL;
        ListNode_t *interface_type_decls = NULL;
        ListBuilder interface_var_builder;
        list_builder_init(&interface_var_builder);
        ast_t *interface_type_section_ast = NULL;  /* Track interface types for enum resolution */
        ListNode_t *implementation_uses = NULL;
        ListNode_t *implementation_const_decls = NULL;
        ListNode_t *implementation_type_decls = NULL;
        ListBuilder implementation_var_builder;
        list_builder_init(&implementation_var_builder);
        ast_t *implementation_type_section_ast = NULL;  /* Track implementation types for enum resolution */
        ListNode_t *subprograms = NULL;
        struct Statement *initialization = NULL;
        struct Statement *finalization = NULL;

        // Navigate through unit children to find sections by type
        ast_t *section = unit_name_node != NULL ? unit_name_node->next : NULL;
        ast_t *interface_node = NULL;
        ast_t *implementation_node = NULL;
        ast_t *initialization_node = NULL;
        ast_t *finalization_node = NULL;

        /* Create visited set for unit-level sections */
        VisitedSet *visited_unit = visited_set_create();
        if (visited_unit == NULL) {
            fprintf(stderr, "ERROR: Failed to allocate visited set for unit sections\n");
            free(unit_id);
            if (unit_scan_copy != NULL)
                free_ast(unit_scan_copy);
            return NULL;
        }

        while (section != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting section type %d\n", section->typ);
            }
            /* Check for circular reference */
            if (!is_safe_to_continue(visited_unit, section)) {
                fprintf(stderr, "ERROR: Circular reference detected in unit sections, stopping traversal\n");
                break;
            }
            
            if (section->typ == PASCAL_T_INTERFACE_SECTION) {
                interface_node = section;
                if (kgpc_getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] interface at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
                implementation_node = section;
                if (kgpc_getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] implementation at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_INITIALIZATION_SECTION) {
                initialization_node = section;
                if (kgpc_getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] initialization at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_FINALIZATION_SECTION) {
                finalization_node = section;
            }
            section = section->next;
        }
        
        visited_set_destroy(visited_unit);

        g_interface_section_ast = interface_node;
        g_implementation_section_ast = implementation_node;

        if (interface_node != NULL && interface_node->typ == PASCAL_T_INTERFACE_SECTION) {
            /* Create visited set for interface sections */
            VisitedSet *visited_if = visited_set_create();
            if (visited_if == NULL) {
                fprintf(stderr, "ERROR: Failed to allocate visited set for interface sections\n");
            } else {
                ast_t *section = interface_node->child;
                while (section != NULL) {
            if (kgpc_getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting PROGRAM section type %d\n", section->typ);
            }
            /* Check for circular reference */
                    if (!is_safe_to_continue(visited_if, section)) {
                        section = section->next;
                        continue;
                    }
                    
                    ast_t *node_cursor = unwrap_pascal_node(section);
                    if (node_cursor != NULL) {
                        if (kgpc_getenv("KGPC_DEBUG_PROPERTY") != NULL) {
                            fprintf(stderr, "[KGPC] interface node typ=%d (%s)\n",
                                node_cursor->typ, pascal_tag_to_string(node_cursor->typ));
                        }
                        switch (node_cursor->typ) {
                        case PASCAL_T_USES_SECTION:
                            append_uses_from_section(node_cursor, &interface_uses);
                            break;
                        case PASCAL_T_TYPE_SECTION:
                            if (kgpc_getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                                fprintf(stderr, "[KGPC] interface TYPE_SECTION at line=%d\n", node_cursor->line);
                            }
                            interface_type_section_ast = node_cursor;  /* Save for const array enum resolution */
                            g_interface_type_section_ast = node_cursor;
                            enum_registry_scan_type_section(node_cursor);
                            append_type_decls_from_section(node_cursor, &interface_type_decls,
                                NULL, &interface_const_decls, &interface_var_builder, NULL);
                            break;
                        case PASCAL_T_CONST_SECTION:
                            append_const_decls_from_section(node_cursor, &interface_const_decls,
                                                            &interface_var_builder, interface_type_section_ast);
                            break;
                        case PASCAL_T_VAR_SECTION:
                            list_builder_extend(&interface_var_builder, convert_var_section(node_cursor));
                            break;
                        case PASCAL_T_PROCEDURE_DECL: {
                            Tree_t *proc = convert_procedure(node_cursor);
                            if (kgpc_debug_subprog_enabled()) {
                                char *proc_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] convert_procedure(%s) => %p\n", proc_id, (void*)proc);
                                free(proc_id);
                            }
                            if (proc != NULL) {
                                proc->tree_data.subprogram_data.unit_is_public = 1;  /* Interface procedure */
                            }
                            append_subprogram_node(&subprograms, proc);
                            break;
                        }
                        case PASCAL_T_FUNCTION_DECL: {
                            Tree_t *func = convert_function(node_cursor);
                            if (kgpc_debug_subprog_enabled()) {
                                char *func_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] convert_function(%s) => %p\n", func_id, (void*)func);
                                free(func_id);
                            }
                            if (func != NULL) {
                                func->tree_data.subprogram_data.unit_is_public = 1;  /* Interface function */
                            }
                            append_subprogram_node(&subprograms, func);
                            break;
                        }
                        case PASCAL_T_PROPERTY_DECL:
                            /* Module-level property declaration (FPC extension). */
                            append_module_property_wrappers(&subprograms, node_cursor);
                            break;
                        default:
                            break;
                        }
                    }
                    section = section->next;
                }
                visited_set_destroy(visited_if);
            }
        }

        if (interface_node != NULL)
        {
            VisitedSet *visited_subs = visited_set_create();
            if (visited_subs != NULL)
            {
                append_subprograms_from_ast_recursive(interface_node->child, &subprograms,
                    visited_subs);
                visited_set_destroy(visited_subs);
            }
        }

        if (implementation_node != NULL && implementation_node->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
            /* Debug: count implementation section nodes */
            if (kgpc_getenv("KGPC_DEBUG_IMPL_SECTION") != NULL) {
                ast_t *dbg = implementation_node->child;
                int count = 0;
                int max_line = 0;
                int proc_count = 0, func_count = 0, method_count = 0, type_count = 0;
                while (dbg != NULL) {
                    count++;
                    if (dbg->line > max_line) max_line = dbg->line;
                    if (dbg->typ == PASCAL_T_PROCEDURE_DECL) proc_count++;
                    else if (dbg->typ == PASCAL_T_FUNCTION_DECL) func_count++;
                    else if (dbg->typ == PASCAL_T_METHOD_IMPL) method_count++;
                    else if (dbg->typ == PASCAL_T_TYPE_SECTION) type_count++;
                    dbg = dbg->next;
                }
                fprintf(stderr, "[IMPL_SECTION] child count=%d max_line=%d procs=%d funcs=%d methods=%d types=%d\n",
                        count, max_line, proc_count, func_count, method_count, type_count);
            }

            /* Create visited set for implementation sections */
            VisitedSet *visited_impl = visited_set_create();
            if (visited_impl == NULL) {
                fprintf(stderr, "ERROR: Failed to allocate visited set for implementation sections\n");
            } else {
                ast_t *definition = implementation_node->child;
                while (definition != NULL && definition != ast_nil) {
                    /* Check for circular reference */
                    if (!is_safe_to_continue(visited_impl, definition)) {
                        definition = definition->next;
                        continue;
                    }

                    if (kgpc_getenv("KGPC_DEBUG_IMPL_NONE") != NULL &&
                        definition->typ == PASCAL_T_NONE &&
                        definition->child == NULL &&
                        definition->sym != NULL &&
                        definition->sym->name != NULL) {
                        fprintf(stderr, "[KGPC] impl NONE at line=%d: %.120s\n",
                            definition->line, definition->sym->name);
                    }
                    ast_t *node_cursor = unwrap_pascal_node(definition);
                    if (node_cursor != NULL) {
                        if (kgpc_getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
                            fprintf(stderr, "[KGPC] implementation section node typ=%d\n", node_cursor->typ);
                        }
                        switch (node_cursor->typ) {
                        case PASCAL_T_USES_SECTION:
                            append_uses_from_section(node_cursor, &implementation_uses);
                            break;
                        case PASCAL_T_TYPE_SECTION:
                            implementation_type_section_ast = node_cursor;  /* Save for const array enum resolution */
                            g_implementation_type_section_ast = node_cursor;
                            enum_registry_scan_type_section(node_cursor);
                            append_type_decls_from_section(node_cursor, &implementation_type_decls,
                                &subprograms, &implementation_const_decls, &implementation_var_builder, NULL);
                            break;
                        case PASCAL_T_CONST_SECTION:
                            append_const_decls_from_section(node_cursor, &implementation_const_decls,
                                                            &implementation_var_builder, implementation_type_section_ast);
                            break;
                        case PASCAL_T_VAR_SECTION:
                            list_builder_extend(&implementation_var_builder, convert_var_section(node_cursor));
                            break;
                        case PASCAL_T_PROCEDURE_DECL: {
                            Tree_t *proc = convert_procedure(node_cursor);
                            if (kgpc_debug_subprog_enabled() || kgpc_getenv("KGPC_DEBUG_IMPL_PROCS") != NULL) {
                                char *proc_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] impl convert_procedure(%s) line=%d => %p\n", proc_id, node_cursor->line, (void*)proc);
                                free(proc_id);
                            }
                            append_subprogram_node(&subprograms, proc);
                            break;
                        }
                        case PASCAL_T_FUNCTION_DECL: {
                            Tree_t *func = convert_function(node_cursor);
                            if (kgpc_debug_subprog_enabled() || kgpc_getenv("KGPC_DEBUG_IMPL_PROCS") != NULL) {
                                char *func_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] impl convert_function(%s) line=%d => %p\n", func_id, node_cursor->line, (void*)func);
                                free(func_id);
                            }
                            append_subprogram_node(&subprograms, func);
                            break;
                        }
                        case PASCAL_T_METHOD_IMPL:
                        case PASCAL_T_CONSTRUCTOR_DECL:
                        case PASCAL_T_DESTRUCTOR_DECL: {
                            Tree_t *method_tree = convert_method_impl(node_cursor);
                            append_subprogram_node(&subprograms, method_tree);
                            break;
                        }
                        default:
                            break;
                        }
                    }
                    definition = definition->next;
                }
                visited_set_destroy(visited_impl);
            }
        }

        if (implementation_node != NULL)
        {
            VisitedSet *visited_subs = visited_set_create();
            if (visited_subs != NULL)
            {
                append_subprograms_from_ast_recursive(implementation_node->child, &subprograms,
                    visited_subs);
                visited_set_destroy(visited_subs);
            }
        }

        {
            /* Final safety scan: walk the full unit AST to pick up any subprograms
             * that are not wired into the interface/implementation sibling chains. */
            VisitedSet *visited_subs = visited_set_create();
            if (visited_subs != NULL)
            {
                ast_t *scan_root = unit_scan_copy != NULL ? unit_scan_copy : cur;
                append_top_level_subprograms_from_ast(scan_root, &subprograms, visited_subs, 0);
                visited_set_destroy(visited_subs);
            }
        }

        if (initialization_node != NULL && initialization_node->typ == PASCAL_T_INITIALIZATION_SECTION) {
            /* The initialization section's child can be:
             * 1. A PASCAL_T_NONE wrapper from make_stmt_list_parser, with stmt_list as child
             * 2. The first statement directly (if the wrapper was optimized away)
             * We handle both cases by checking if child is PASCAL_T_NONE. */
            ast_t *stmt_list_seq = initialization_node->child;
            if (kgpc_getenv("KGPC_DEBUG_UNIT_INIT") != NULL) {
                fprintf(stderr, "[KGPC] initialization_node: typ=%d line=%d\n", 
                        initialization_node->typ, initialization_node->line);
                if (stmt_list_seq != NULL) {
                    fprintf(stderr, "[KGPC]   stmt_list_seq: typ=%d line=%d\n", 
                            stmt_list_seq->typ, stmt_list_seq->line);
                }
            }
            if (stmt_list_seq != NULL) {
                ListNode_t *stmts = NULL;
                if (stmt_list_seq->typ == PASCAL_T_NONE) {
                    /* Wrapped structure: NONE -> child is stmt list */
                    if (stmt_list_seq->child != NULL) {
                        stmts = convert_statement_list(stmt_list_seq->child);
                    }
                } else {
                    /* Direct structure: child IS the first statement in a linked list */
                    stmts = convert_statement_list(stmt_list_seq);
                }
                initialization = mk_compoundstatement(initialization_node->line, stmts);
            }
        }

        if (finalization_node != NULL && finalization_node->typ == PASCAL_T_FINALIZATION_SECTION) {
            /* Same structure handling as initialization */
            ast_t *stmt_list_seq = finalization_node->child;
            if (stmt_list_seq != NULL) {
                ListNode_t *stmts = NULL;
                if (stmt_list_seq->typ == PASCAL_T_NONE) {
                    if (stmt_list_seq->child != NULL) {
                        stmts = convert_statement_list(stmt_list_seq->child);
                    }
                } else {
                    stmts = convert_statement_list(stmt_list_seq);
                }
                finalization = mk_compoundstatement(finalization_node->line, stmts);
            }
        }

        Tree_t *tree = mk_unit(cur->line, unit_id, interface_uses,
                               interface_const_decls, interface_type_decls,
                               list_builder_finish(&interface_var_builder), implementation_uses,
                               implementation_const_decls,
                               implementation_type_decls, list_builder_finish(&implementation_var_builder),
                               subprograms, initialization, finalization);
        if (unit_scan_copy != NULL)
            free_ast(unit_scan_copy);
        /* Clear borrowed AST pointers before the caller frees the raw AST. */
        g_interface_type_section_ast = NULL;
        g_implementation_type_section_ast = NULL;
        return tree;
    }

    fprintf(stderr, "ERROR: Unsupported Pascal AST root type %d.\n", cur->typ);
    return NULL;
}

/* Get the method template for a class method.
 * This is used to copy default parameter values from class declarations to implementations.
 * For overloaded methods, we need to match by parameter count.
 */
struct MethodTemplate *from_cparser_get_method_template(struct RecordType *record, const char *method_name)
{
    if (record == NULL || method_name == NULL)
        return NULL;
    
    ListNode_t *cur = record->method_templates;
    struct MethodTemplate *fallback_template = NULL;
    
    while (cur != NULL)
    {
        struct MethodTemplate *template = (struct MethodTemplate *)cur->cur;
        if (template != NULL && template->name != NULL &&
            strcasecmp(template->name, method_name) == 0)
        {
            /* For overloaded methods, prefer templates that have default values */
            if (template->params_ast != NULL)
            {
                /* Check if this template has default values */
                ast_t *param = template->params_ast;
                if (param->typ == PASCAL_T_PARAM_LIST)
                    param = param->child;
                
                while (param != NULL)
                {
                    if (param->typ == PASCAL_T_PARAM)
                    {
                        for (ast_t *child = param->child; child != NULL; child = child->next)
                        {
                            if (child->typ == PASCAL_T_DEFAULT_VALUE)
                                return template;  /* Found one with defaults */
                        }
                    }
                    param = param->next;
                }
            }
            
            /* Remember first matching template as fallback */
            if (fallback_template == NULL)
                fallback_template = template;
        }
        cur = cur->next;
    }
    
    /* Return fallback if no template with defaults found */
    return fallback_template;
}

/* Public wrapper for convert_expression */
struct Expression *from_cparser_convert_expression(ast_t *expr_node)
{
    return convert_expression(expr_node);
}
