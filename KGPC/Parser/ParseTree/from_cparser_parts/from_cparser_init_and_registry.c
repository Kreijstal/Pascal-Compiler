#include "../from_cparser_internal.h"

/* ============================================================================
 * Circular Reference Detection for AST Traversal
 * ============================================================================
 * The parser can create circular AST structures where node->next or node->child
 * point back to themselves or ancestors, causing infinite loops. This module
 * provides a visited set mechanism to detect and prevent such loops.
 */

/* VisitedSetEntry, VisitedSet typedefs moved to from_cparser_internal.h */

int kgpc_debug_subprog_enabled(void)
{
    static int cached = -1;
    if (cached == -1)
        cached = (kgpc_getenv("KGPC_DEBUG_SUBPROG") != NULL) ? 1 : 0;
    return cached;
}

int kgpc_debug_decl_scan_enabled(void)
{
    static int cached = -1;
    if (cached == -1)
        cached = (kgpc_getenv("KGPC_DEBUG_DECL_SCAN") != NULL) ? 1 : 0;
    return cached;
}

/* struct TypeHelperMapping moved to from_cparser_internal.h */

ListNode_t *type_helper_mappings = NULL;
ast_t *g_interface_type_section_ast = NULL;
ast_t *g_implementation_type_section_ast = NULL;
ast_t *g_interface_section_ast = NULL;
ast_t *g_implementation_section_ast = NULL;
/* Method context for expression conversion (e.g., bare "inherited" expressions). */
const char *g_current_method_name = NULL;
/* When instantiating a generic method template, this points to the owning
 * RecordType so that type_name_is_class_like can answer without touching the
 * (possibly freed) raw parser AST globals. */
struct RecordType *g_instantiate_record = NULL;

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

void enum_registry_add(const char *name, int start, int end) {
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

int enum_registry_lookup(const char *name, int *out_start, int *out_end) {
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

void enum_registry_free(void) {
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

/* Scan a PASCAL_T_TYPE_SECTION and register all enumerated and subrange types in the global registry. */
void enum_registry_scan_type_section(ast_t *type_section) {
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

int is_external_directive(const char *directive)
{
    if (directive == NULL)
        return 0;
    return (strcasecmp(directive, "external") == 0 ||
            strcasecmp(directive, "weakexternal") == 0);
}

void register_type_helper_mapping(const char *helper_id, const char *base_type_id)
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

const char *lookup_type_helper_base(const char *helper_id)
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

const char *ast_symbol_name(ast_t *node)
{
    if (node == NULL)
        return NULL;
    if (node->sym != NULL && node->sym->name != NULL)
        return node->sym->name;
    return NULL;
}

VisitedSet *visited_set_create(void) {
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

void visited_set_destroy(VisitedSet *set) {
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

bool visited_set_contains(VisitedSet *set, ast_t *node) {
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

bool visited_set_add(VisitedSet *set, ast_t *node) {
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
bool is_safe_to_continue(VisitedSet *visited, ast_t *node) {
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

/* TypeInfo typedef moved to from_cparser_internal.h */

/* Frontend error counter for errors during AST to tree conversion */
int g_frontend_error_count = 0;
char *g_scoped_enum_source_path = NULL;
char *g_scoped_enum_source_buffer = NULL;
size_t g_scoped_enum_source_length = 0;


int split_absolute_target(const char *absolute_target,
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

char *qualified_ident_join_prefix(const QualifiedIdent *qid, int count)
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

TypeRef *type_ref_from_single_name(const char *name)
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

TypeRef *type_ref_from_name_and_args(const char *base_name, ListNode_t *type_args)
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

QualifiedIdent *qualified_ident_from_ast(ast_t *node)
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

TypeRef *type_ref_from_info_or_id(const TypeInfo *info, const char *type_id)
{
    if (info != NULL && info->type_ref != NULL)
        return type_ref_clone(info->type_ref);
    if (type_id != NULL)
        return type_ref_from_single_name(type_id);
    return NULL;
}

TypeRef *type_ref_from_element_info(const TypeInfo *info, const char *type_id)
{
    if (info != NULL && info->element_type_ref != NULL)
        return type_ref_clone(info->element_type_ref);
    if (type_id != NULL)
        return type_ref_from_single_name(type_id);
    return NULL;
}

TypeRef *type_ref_from_pointer_info(const TypeInfo *info, const char *type_id)
{
    if (info != NULL && info->pointer_type_ref != NULL)
        return type_ref_clone(info->pointer_type_ref);
    if (type_id != NULL)
        return type_ref_from_single_name(type_id);
    return NULL;
}

TypeRef *type_ref_from_qualifier_and_id(const char *qualifier, const char *id)
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

int parse_guid_literal(const char *guid, uint32_t *d1, uint16_t *d2, uint16_t *d3, uint8_t d4[8])
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
int from_cparser_scopedenums_enabled_at_line(int target_line)
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
void frontend_error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    g_frontend_error_count++;
}

/* PendingGenericAlias typedef moved to from_cparser_internal.h */

PendingGenericAlias *g_pending_generic_aliases = NULL;
int g_allow_pending_specializations = 0;

/* Deferred inline specializations: when `specialize Foo<T>` appears in
 * expression context (e.g. `specialize Foo<T>.Method(...)`), we need to
 * instantiate the generic record and create a type declaration so that
 * append_generic_method_clones() can emit method implementations. */
/* DeferredInlineSpec typedef moved to from_cparser_internal.h */
DeferredInlineSpec *g_deferred_inline_specs = NULL;
ListNode_t *g_const_sections = NULL;
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
ast_t *const_decl_index_lookup(const char *name) {
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
void const_decl_index_scan_section(ast_t *section) {
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

void destroy_type_info_contents(TypeInfo *info) {
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

void reset_const_sections(void) {
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

void register_const_section(ast_t *const_section) {
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

int const_section_is_resourcestring(ast_t *const_section) {
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


/* Parse a range bound string that may be a number or boolean literal.
 * Used for multi-dimensional array range extraction. */
int parse_range_bound(const char *s) {
    if (s == NULL) return 0;
    if (strcasecmp(s, "true") == 0) return 1;
    if (strcasecmp(s, "false") == 0) return 0;
    return atoi(s);
}


int resolve_const_expr_from_sections(const char *expr, int *result)
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

int lookup_const_int(const char *name, int *out_value) {
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

void register_const_int(const char *name, int value) {
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

void register_pending_generic_alias(Tree_t *decl, TypeInfo *type_info) {
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


/* Global offset added to all source_index values during conversion.
 * Set via from_cparser_set_source_offset() before converting each unit's AST,
 * so that source_index values are globally unique across all source buffers. */
int g_source_offset = 0;

void from_cparser_set_source_offset(int offset)
{
    g_source_offset = offset;
}

/* Helper to copy source index from AST node to Expression for accurate error context.
 * node->index == -1 means the parser position is unknown; preserve that sentinel
 * rather than producing a bogus global offset (-1 + g_source_offset). */
struct Expression *set_expr_source_index(struct Expression *expr, ast_t *node) {
    if (expr != NULL && node != NULL && node->index >= 0) {
        expr->source_index = node->index + g_source_offset;
    }
    return expr;
}


/* ClassMethodBinding typedef moved to from_cparser.h */

ListNode_t *class_method_bindings = NULL;

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

void cmb_index_reset(void)
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

void cmb_method_index_reset(void)
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
int anonymous_method_counter = 0;

#define ANON_METHOD_NAME_SIZE 64

/* Helper function to generate unique names for anonymous methods */
char *generate_anonymous_method_name(int is_function) {
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

char *param_type_signature_from_params_ast(ast_t *params_ast) {
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

int count_params_in_method_impl(ast_t *method_node) {
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

char *param_type_signature_from_method_impl(ast_t *method_node) {
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

void register_class_method_ex(const char *class_name, const char *method_name,
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



const char *find_class_for_method(const char *method_name) {
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

int is_method_static_with_signature(const char *class_name, const char *method_name,
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

int typed_const_counter = 0;
const char *g_typed_const_unit_tag = "p"; /* set per unit in tree_from_pascal_ast */

/* Mark a TREE_VAR_DECL as having static storage (for local typed constants) */
void mark_var_decl_static_storage(Tree_t *decl)
{
    if (decl == NULL || decl->type != TREE_VAR_DECL)
        return;
    decl->tree_data.var_decl_data.has_static_storage = 1;
    char label_buffer[128];
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_var_%s_%d", g_typed_const_unit_tag, typed_const_counter);
    decl->tree_data.var_decl_data.static_label = strdup(label_buffer);
    ++typed_const_counter;
}

int is_operator_token_name(const char *name)
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
void tag_operator_call(struct Expression *expr, int is_operator)
{
    if (expr != NULL && is_operator)
        expr->expr_data.function_call_data.is_operator_call = 1;
}

/* Encode operator symbols into valid identifier names for assembly */
char *encode_operator_name(const char *op_name) {
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

char *mangle_method_name(const char *class_name, const char *method_name) {
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
char *mangle_method_name_raw(const char *class_name, const char *method_name) {
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

char *method_param_type_suffix(Tree_t *param_decl)
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

