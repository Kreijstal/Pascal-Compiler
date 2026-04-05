
void semcheck_expr_set_resolved_type(struct Expression *expr, int type_tag);

/* Cached getenv() for KGPC_* environment variables.
 * getenv() does a linear scan of the environment on each call; with hundreds of
 * debug checks in hot loops, this was consuming 6.75% of total CPU time.
 * O(1) pointer-hash cache: all callers pass string literals with stable addresses. */
#define KGPC_ENV_CACHE_SIZE 256  /* must be power of 2 */
static struct {
    const char *name;   /* pointer to string literal (identity comparison) */
    const char *value;  /* cached getenv result (NULL or pointer) */
    int         valid;  /* 1 if slot is populated */
} g_env_cache[KGPC_ENV_CACHE_SIZE];

static unsigned int env_ptr_hash(const char *ptr)
{
    uintptr_t v = (uintptr_t)ptr;
    v ^= v >> 16;
    v *= 0x45d9f3bu;  /* murmurhash3 finalizer constant */
    v ^= v >> 16;
    return (unsigned int)(v & (KGPC_ENV_CACHE_SIZE - 1));
}

const char *kgpc_getenv(const char *name) {
    unsigned int h = env_ptr_hash(name);
    /* Open-address probe (linear, max 4 steps) for collisions.
     * With 256 slots and ~112 unique names this is nearly always 1 step. */
    for (int step = 0; step < 4; step++) {
        unsigned int idx = (h + (unsigned int)step) & (KGPC_ENV_CACHE_SIZE - 1);
        if (g_env_cache[idx].valid && g_env_cache[idx].name == name)
            return g_env_cache[idx].value;
        if (!g_env_cache[idx].valid) {
            /* Empty slot – fill it. */
            const char *val = getenv(name);
            g_env_cache[idx].name  = name;
            g_env_cache[idx].value = val;
            g_env_cache[idx].valid = 1;
            return val;
        }
    }
    /* Extremely unlikely fallback: all 4 probe slots are occupied by
     * different keys.  Just do a real getenv without caching. */
    return getenv(name);
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

/* Track which units have been fully semchecked via semcheck_unit().
 * Declarations from these units are skipped in semcheck_program(). */
static int g_unit_fully_semchecked[SYMTAB_MAX_UNITS];

int semcheck_is_unit_fully_checked(int unit_index)
{
    if (unit_index <= 0 || unit_index >= SYMTAB_MAX_UNITS)
        return 0;
    return g_unit_fully_semchecked[unit_index];
}

void semcheck_mark_unit_fully_checked(int unit_index)
{
    if (unit_index > 0 && unit_index < SYMTAB_MAX_UNITS)
        g_unit_fully_semchecked[unit_index] = 1;
}
static char *g_semcheck_source_path = NULL;
static char *g_semcheck_source_buffer = NULL;
static size_t g_semcheck_source_length = 0;
static int g_semcheck_warning_count = 0;

#define MAX_SOURCE_BUFFERS 512
static struct {
    char *path;
    char *buffer;
    size_t length;
    int global_start;  /* cumulative offset for globally-unique source_index values */
} g_source_buffer_registry[MAX_SOURCE_BUFFERS];
static int g_source_buffer_count = 0;
static int g_source_buffer_next_global = 0; /* next available global offset */

static ScopeNode *semcheck_switch_to_unit_scope(SymTab_t *symtab, int unit_index)
{
    if (symtab == NULL || unit_index <= 0)
        return NULL;

    ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, unit_index);
    if (unit_scope == NULL)
        return NULL;

    ScopeNode *saved_scope = symtab->current_scope;
    symtab->current_scope = unit_scope;
    return saved_scope;
}

static void semcheck_restore_scope(SymTab_t *symtab, ScopeNode *saved_scope)
{
    if (symtab != NULL && saved_scope != NULL)
        symtab->current_scope = saved_scope;
}

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
    assert(g_source_buffer_count < MAX_SOURCE_BUFFERS &&
           "source buffer registry full — increase MAX_SOURCE_BUFFERS");

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
        /* Debug: report accumulated source buffer memory */
        if (kgpc_getenv("KGPC_DEBUG_MEMBUF") != NULL)
        {
            size_t total = 0;
            for (int i = 0; i < g_source_buffer_count; i++)
                total += g_source_buffer_registry[i].length;
            fprintf(stderr, "[MEMBUF] source_buffer_registry: %d entries, %zu bytes (%.1f MB)\n",
                    g_source_buffer_count, total, (double)total / (1024.0 * 1024.0));
        }
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

static void semcheck_mark_const_decl_units(ListNode_t *const_decls, int unit_index)
{
    if (const_decls == NULL || unit_index <= 0)
        return;

    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_CONST_DECL)
            continue;
        if (tree->tree_data.const_decl_data.source_unit_index == 0)
            tree->tree_data.const_decl_data.source_unit_index = unit_index;
    }
}

static void semcheck_mark_var_decl_units(ListNode_t *var_decls, int unit_index)
{
    if (var_decls == NULL || unit_index <= 0)
        return;

    for (ListNode_t *cur = var_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type == TREE_VAR_DECL)
        {
            if (tree->tree_data.var_decl_data.source_unit_index == 0)
                tree->tree_data.var_decl_data.source_unit_index = unit_index;
        }
        else if (tree->type == TREE_ARR_DECL)
        {
            if (tree->tree_data.arr_decl_data.source_unit_index == 0)
                tree->tree_data.arr_decl_data.source_unit_index = unit_index;
        }
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

static struct RecordType *semcheck_record_from_type_decl(Tree_t *tree, SymTab_t *symtab)
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
    {
        struct RecordType *record = tree->tree_data.type_decl_data.info.record;
        /* When a forward-declared class is completed, the full declaration's
         * RecordType becomes a depleted shell.  Look up the canonical record
         * from the symbol table instead. */
        if (record != NULL && record->is_class &&
            record->fields == NULL && record->parent_fields_merged &&
            symtab != NULL && tree->tree_data.type_decl_data.id != NULL)
        {
            HashNode_t *canon_node = NULL;
            if (FindSymbol(&canon_node, symtab, tree->tree_data.type_decl_data.id) &&
                canon_node != NULL && canon_node->type != NULL)
            {
                struct RecordType *canon = NULL;
                KgpcType *ct = canon_node->type;
                if (ct->kind == TYPE_KIND_RECORD)
                    canon = ct->info.record_info;
                else if (ct->kind == TYPE_KIND_POINTER &&
                         ct->info.points_to != NULL &&
                         ct->info.points_to->kind == TYPE_KIND_RECORD)
                    canon = ct->info.points_to->info.record_info;
                if (canon != NULL)
                    return canon;
            }
        }
        return record;
    }

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
        !pascal_identifier_equals(record->parent_class_name, "TObject") &&
        !pascal_identifier_equals(record->parent_class_name, "IInterface") &&
        !pascal_identifier_equals(record->parent_class_name, "IUnknown"))
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

/* Build a method lookup key in the form "ClassName__MethodName".
 * Centralises the __ convention so it lives in one place.
 * Caller must free() the returned string. */
static char *make_method_lookup_key(const char *class_name, const char *method_name)
{
    if (class_name == NULL || method_name == NULL)
        return NULL;
    size_t len = strlen(class_name) + 2 + strlen(method_name) + 1;
    char *key = (char *)malloc(len);
    if (key != NULL)
        snprintf(key, len, "%s__%s", class_name, method_name);
    return key;
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

/* Debug assertion: pointer type aliases (PInt64, PByte, PComp, etc.) are
 * declared in systemh.inc and should resolve through normal type resolution.
 * This function fires a warning if any of them fail to resolve, indicating
 * a preprocessor or type registration bug. */
static void semcheck_assert_pointer_type_resolved(const char *id)
{
    if (id == NULL) return;
    static const char *expected_pointer_types[] = {
        "PInt64", "PByte", "PWord", "PLongInt", "PLongWord",
        "PInteger", "PCardinal", "PQWord", "PPointer", "PBoolean",
        "PComp", NULL
    };
    for (int i = 0; expected_pointer_types[i] != NULL; i++) {
        if (pascal_identifier_equals(id, expected_pointer_types[i])) {
            fprintf(stderr, "[KGPC] WARNING: pointer type '%s' not resolved through "
                    "normal type paths — check systemh.inc preprocessing\n", id);
            return;
        }
    }
}

static int semcheck_scope_level_for_type_candidate(SymTab_t *symtab, HashNode_t *candidate)
{
    if (symtab == NULL || candidate == NULL || candidate->id == NULL)
        return INT_MAX / 2;

    int level = 0;
    for (ScopeNode *scope = symtab->current_scope; scope != NULL && scope != symtab->builtin_scope; scope = scope->parent, ++level)
    {
        ListNode_t *matches = FindAllIdentsInTable(scope->table, candidate->id);
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
        case SET_TYPE:        return HASHVAR_SET;
        case ENUM_TYPE:       return HASHVAR_ENUM;
        case FILE_TYPE:       return HASHVAR_FILE;
        default:              return HASHVAR_UNTYPED;
    }
}

__attribute__((unused)) static int semcheck_find_ident_with_qualified_fallback(HashNode_t **out, SymTab_t *symtab,
    const char *id)
{
    if (out == NULL || symtab == NULL || id == NULL)
        return -1;

    return FindSymbol(out, symtab, id);
}

__attribute__((unused)) static int semcheck_find_ident_with_qualified_fallback_ref(HashNode_t **out, SymTab_t *symtab,
    const QualifiedIdent *id_ref)
{
    if (out == NULL || symtab == NULL || id_ref == NULL || id_ref->count <= 0)
        return -1;

    int found = -1;
    char *full = qualified_ident_join(id_ref, ".");
    if (full != NULL)
    {
        found = FindSymbol(out, symtab, full);
        free(full);
    }
    if (found && out != NULL && *out != NULL)
        return found;

    if (id_ref->count > 1)
    {
        const char *last = qualified_ident_last(id_ref);
        if (last != NULL)
            return FindSymbol(out, symtab, last);
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

__attribute__((unused)) static ListNode_t *semcheck_create_builtin_param_with_id(const char *name, int type_tag,
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
__attribute__((unused)) static void semcheck_file_from_source_index(int source_index, char *file_out, size_t file_out_size)
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
                memcpy(last_file, directive_file, sizeof(last_file) - 1);
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
static const char *g_semcheck_error_unit_name = NULL;
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
    if (g_semcheck_error_unit_name != NULL)
        return g_semcheck_error_unit_name;
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

/* Resolve error location inside a unit body.
 * Uses {#line} directives in the preprocessed source to find the actual
 * include file and line number.  Returns the resolved line (>0 on success).
 * On success, directive_file_out contains the include file path and
 * unit_chain_out contains a string like " (in unit System from program.p)". */
static int resolve_unit_error_location(int source_index, int line_num,
    char *directive_file_out, size_t dir_size,
    char *unit_chain_out, size_t chain_size)
{
    if (directive_file_out != NULL && dir_size > 0)
        directive_file_out[0] = '\0';
    if (unit_chain_out != NULL && chain_size > 0)
        unit_chain_out[0] = '\0';

    int resolved_line = 0;
    if (source_index >= 0)
    {
        resolved_line = resolve_error_source_context(
            source_index, directive_file_out, dir_size, 1);
    }
    if (resolved_line <= 0)
        resolved_line = line_num;

    /* Build the include chain annotation */
    if (unit_chain_out != NULL && chain_size > 0 && g_semcheck_error_unit_name != NULL)
    {
        /* Find the registered path for this unit's buffer */
        const char *unit_path = NULL;
        if (source_index >= 0)
        {
            for (int i = 0; i < g_source_buffer_count; i++)
            {
                int gs = g_source_buffer_registry[i].global_start;
                int len = (int)g_source_buffer_registry[i].length;
                if (source_index >= gs && source_index < gs + len)
                {
                    unit_path = g_source_buffer_registry[i].path;
                    break;
                }
            }
        }

        if (directive_file_out[0] != '\0' && unit_path != NULL)
        {
            /* Include file resolved — show chain:
             * "generic.inc:3652 (included in FPCSource/rtl/linux/system.pp)" */
            snprintf(unit_chain_out, chain_size, " (included in %s)", unit_path);
        }
        else if (unit_path != NULL)
        {
            /* No include file — error is directly in the unit source */
            strncpy(directive_file_out, unit_path, dir_size - 1);
            directive_file_out[dir_size - 1] = '\0';
        }
        else
        {
            /* source_index unavailable — find unit path by matching unit name
             * against registered source buffer filenames. */
            const char *uname = g_semcheck_error_unit_name;
            if (uname != NULL)
            {
                size_t ulen = strlen(uname);
                for (int i = 0; i < g_source_buffer_count; i++)
                {
                    const char *p = g_source_buffer_registry[i].path;
                    if (p == NULL) continue;
                    const char *slash = strrchr(p, '/');
                    const char *base = slash ? slash + 1 : p;
                    if (strlen(base) > ulen && base[ulen] == '.' &&
                        strncasecmp(base, uname, ulen) == 0)
                    {
                        strncpy(directive_file_out, p, dir_size - 1);
                        directive_file_out[dir_size - 1] = '\0';
                        break;
                    }
                }
                if (directive_file_out[0] == '\0')
                {
                    strncpy(directive_file_out, uname, dir_size - 1);
                    directive_file_out[dir_size - 1] = '\0';
                }
            }
        }
    }

    return resolved_line;
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
    char unit_chain[MAX_DIRECTIVE_FILENAME_LEN];
    unit_chain[0] = '\0';
    if (g_semcheck_error_unit_name != NULL)
    {
        int resolved_line = resolve_unit_error_location(
            effective_source_index, effective_line,
            directive_file, sizeof(directive_file),
            unit_chain, sizeof(unit_chain));
        if (resolved_line > 0)
            effective_line = resolved_line;
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
    if (unit_chain[0] != '\0')
        fprintf(stderr, "%s", unit_chain);
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
    char unit_chain[MAX_DIRECTIVE_FILENAME_LEN];
    unit_chain[0] = '\0';
    if (g_semcheck_error_unit_name != NULL)
    {
        int resolved_line = resolve_unit_error_location(
            source_index, line_num,
            directive_file, sizeof(directive_file),
            unit_chain, sizeof(unit_chain));
        if (resolved_line > 0)
            line_num = resolved_line;
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
    if (unit_chain[0] != '\0')
        fprintf(stderr, "%s", unit_chain);
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
        /* Look up the correct source buffer for this error's source_index.
         * Previously this always used the main preprocessed_source, which
         * showed wrong file contents for errors in imported units. */
        const char *context_buf = NULL;
        size_t context_buf_len = 0;
        int local_source_offset = source_index; /* offset into context_buf */
        if (source_index >= 0)
        {
            for (int i = 0; i < g_source_buffer_count; i++)
            {
                int gs = g_source_buffer_registry[i].global_start;
                int len = (int)g_source_buffer_registry[i].length;
                if (source_index >= gs && source_index < gs + len)
                {
                    context_buf = g_source_buffer_registry[i].buffer;
                    context_buf_len = g_source_buffer_registry[i].length;
                    local_source_offset = source_index - gs;
                    break;
                }
            }
        }
        if (context_buf == NULL || context_buf_len == 0)
        {
            /* Do NOT fall back to the main program's preprocessed buffer
             * when we're inside a unit — it shows wrong source content.
             * Use preprocessed_source only when NOT inside a unit context. */
            if (g_semcheck_error_unit_name == NULL)
            {
                context_buf = preprocessed_source;
                context_buf_len = preprocessed_length;
                if (context_buf_len == 0 && context_buf != NULL)
                    context_buf_len = strlen(context_buf);
            }
        }
        if (context_buf == NULL || context_buf_len == 0)
        {
            if (g_semcheck_error_unit_name == NULL)
            {
                context_buf = g_semcheck_source_buffer;
                context_buf_len = g_semcheck_source_length;
            }
        }
        if (kgpc_getenv("KGPC_DEBUG_SEM_CONTEXT") != NULL)
        {
            fprintf(stderr,
                "[SemCheck] context file=%s buf_len=%zu line=%d col=%d offset=%d local_offset=%d\n",
                file_path != NULL ? file_path : "<null>",
                context_buf_len,
                effective_line,
                effective_col,
                source_index,
                local_source_offset);
        }
        int printed = 0;
        /* When the source offset is valid and within the buffer, use
         * offset-based context — it correctly locates the right position
         * even in preprocessed buffers that contain multiple files.
         * When the offset is invalid (out of bounds or -1), go straight
         * to the file-based fallback because line-number-based search in
         * a preprocessed buffer can match lines from the wrong include
         * file (e.g. fpcdefs.inc instead of nutils.pas). */
        if (local_source_offset >= 0 &&
            (size_t)local_source_offset < context_buf_len)
        {
            printed = print_source_context_at_offset(context_buf, context_buf_len,
                local_source_offset, effective_line, effective_col, 2);
        }
        /* Fall back to reading the original source file from disk — this
         * avoids the #line-directive ambiguity in preprocessed buffers. */
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
    int effective_source_index = source_index;

    /* Fall back to last known good line/col for synthetic nodes with line 0.
     * Also discard source_index — synthetic nodes often carry stale offsets
     * from a different source buffer, leading to wrong file/line display. */
    if (effective_line <= 0 && g_semcheck_error_line > 0)
    {
        effective_line = g_semcheck_error_line;
        effective_source_index = g_semcheck_error_source_index;
    }
    if (effective_col <= 0 && g_semcheck_error_col > 0)
        effective_col = g_semcheck_error_col;

    char directive_file[MAX_DIRECTIVE_FILENAME_LEN];
    directive_file[0] = '\0';
    char unit_chain[MAX_DIRECTIVE_FILENAME_LEN];
    unit_chain[0] = '\0';
    if (g_semcheck_error_unit_name != NULL)
    {
        int resolved_line = resolve_unit_error_location(
            effective_source_index, effective_line,
            directive_file, sizeof(directive_file),
            unit_chain, sizeof(unit_chain));
        if (resolved_line > 0)
            effective_line = resolved_line;
    }
    else if (effective_source_index >= 0)
    {
        int resolved_line = resolve_error_source_context(
            effective_source_index, directive_file, sizeof(directive_file), 1);
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
        effective_source_index, format, args);
    if (unit_chain[0] != '\0')
        fprintf(stderr, "  %s\n", unit_chain);
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
    char unit_chain[MAX_DIRECTIVE_FILENAME_LEN];
    unit_chain[0] = '\0';
    if (g_semcheck_error_unit_name != NULL)
    {
        int resolved_line = resolve_unit_error_location(
            effective_source_index, line_num,
            directive_file, sizeof(directive_file),
            unit_chain, sizeof(unit_chain));
        if (resolved_line > 0)
            effective_line = resolved_line;
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
    if (unit_chain[0] != '\0')
        fprintf(stderr, "  %s\n", unit_chain);
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

