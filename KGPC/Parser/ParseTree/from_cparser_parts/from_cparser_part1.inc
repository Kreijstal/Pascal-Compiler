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

/* list_builder_* functions are now in List.h - no local definition needed */

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

