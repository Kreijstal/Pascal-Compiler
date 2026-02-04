#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <limits.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

#include "from_cparser.h"

#include "../List/List.h"
#include "tree.h"
#include "tree_types.h"
#include "type_tags.h"
#include "pascal_parser.h"
#include "KgpcType.h"
#include "generic_types.h"
#include "../SemanticCheck/SymTab/SymTab.h"
#include "../../identifier_utils.h"
#include "../pascal_frontend.h"

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
        cached = (getenv("KGPC_DEBUG_SUBPROG") != NULL) ? 1 : 0;
    return cached;
}

static int kgpc_debug_decl_scan_enabled(void)
{
    static int cached = -1;
    if (cached == -1)
        cached = (getenv("KGPC_DEBUG_DECL_SCAN") != NULL) ? 1 : 0;
    return cached;
}

struct TypeHelperMapping
{
    char *helper_id;
    char *base_type_id;
};

static ListNode_t *type_helper_mappings = NULL;

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

static bool visited_set_add(VisitedSet *set, ast_t *node) {
    if (set == NULL || node == NULL) return false;
    
    /* Check if already present */
    if (visited_set_contains(set, node)) {
        return true; /* Already visited - indicates circular reference */
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
        fprintf(stderr, "WARNING: Circular AST reference detected at node %p (type=%d)\n", 
                (void*)node, node->typ);
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
    int is_shortstring;
    int is_open_array;
    ListNode_t *array_dimensions;
    int is_pointer;
    int pointer_type;
    char *pointer_type_id;
    int is_set;
    int set_element_type;
    char *set_element_type_id;
    int is_enum_set;           /* Set with inline anonymous enum as element type */
    ListNode_t *inline_enum_values; /* Enum values for inline enum in set type */
    int is_enum;
    ListNode_t *enum_literals;
    int is_file;
    int file_type;
    char *file_type_id;
    int is_record;
    struct RecordType *record_type;
    int is_generic_specialization;
    char *generic_base_name;
    ListNode_t *generic_type_args;
    int is_range;
    int range_known;
    long long range_start;
    long long range_end;
    int is_class_reference;  /* For "class of T" types */
} TypeInfo;

/* Frontend error counter for errors during AST to tree conversion */
static int g_frontend_error_count = 0;

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
static ListNode_t *g_const_sections = NULL;
typedef struct ConstIntEntry {
    char *name;
    int value;
    struct ConstIntEntry *next;
} ConstIntEntry;
static ConstIntEntry *g_const_ints = NULL;

static void destroy_type_info_contents(TypeInfo *info) {
    if (info == NULL)
        return;

    if (info->element_type_id != NULL) {
        free(info->element_type_id);
        info->element_type_id = NULL;
    }
    if (info->pointer_type_id != NULL) {
        free(info->pointer_type_id);
        info->pointer_type_id = NULL;
    }
    if (info->set_element_type_id != NULL) {
        free(info->set_element_type_id);
        info->set_element_type_id = NULL;
    }
    if (info->inline_enum_values != NULL) {
        destroy_list(info->inline_enum_values);
        info->inline_enum_values = NULL;
    }
    if (info->file_type_id != NULL) {
        free(info->file_type_id);
        info->file_type_id = NULL;
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
    if (info->record_type != NULL) {
        destroy_record_type(info->record_type);
        info->record_type = NULL;
    }
}

static void reset_const_sections(void) {
    if (g_const_sections != NULL) {
        DestroyList(g_const_sections);
        g_const_sections = NULL;
    }
    while (g_const_ints != NULL) {
        ConstIntEntry *next = g_const_ints->next;
        free(g_const_ints->name);
        free(g_const_ints);
        g_const_ints = next;
    }
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
    } else {
        PushListNodeBack(g_const_sections, node);
    }
}

static int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result);

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
    for (ConstIntEntry *cur = g_const_ints; cur != NULL; cur = cur->next) {
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
    for (ConstIntEntry *cur = g_const_ints; cur != NULL; cur = cur->next) {
        if (strcasecmp(cur->name, name) == 0) {
            cur->value = value;
            return;
        }
    }
    ConstIntEntry *entry = (ConstIntEntry *)malloc(sizeof(ConstIntEntry));
    if (entry == NULL)
        return;
    entry->name = strdup(name);
    if (entry->name == NULL) {
        free(entry);
        return;
    }
    entry->value = value;
    entry->next = g_const_ints;
    g_const_ints = entry;
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
    if (getenv("KGPC_DEBUG_TFPG") != NULL &&
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

/* Helper to copy source index from AST node to Expression for accurate error context */
static inline struct Expression *set_expr_source_index(struct Expression *expr, ast_t *node) {
    if (expr != NULL && node != NULL) {
        expr->source_index = node->index;
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
static int resolve_enum_literal_in_type(const char *type_name, const char *literal, ast_t *type_section);
static ast_t *find_type_decl_in_section(ast_t *type_section, const char *type_name);
static int resolve_array_type_info_from_ast(const char *type_name, ast_t *type_section, TypeInfo *out_info, int depth);
static void resolve_array_bounds(TypeInfo *info, ast_t *type_section, ast_t *const_section, const char *id_for_error);
static ast_t *find_node_by_type(ast_t *node, int target_type);


/* ClassMethodBinding typedef moved to from_cparser.h */

static ListNode_t *class_method_bindings = NULL;

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

static void register_class_method_ex(const char *class_name, const char *method_name, 
                                      int is_virtual, int is_override, int is_static) {
    if (class_name == NULL || method_name == NULL)
        return;

    ClassMethodBinding *binding = (ClassMethodBinding *)malloc(sizeof(ClassMethodBinding));
    if (binding == NULL)
        return;

    binding->class_name = strdup(class_name);
    binding->method_name = strdup(method_name);
    binding->is_virtual = is_virtual;
    binding->is_override = is_override;
    binding->is_static = is_static;

    ListNode_t *node = NULL;
    if (binding->class_name != NULL && binding->method_name != NULL)
        node = CreateListNode(binding, LIST_UNSPECIFIED);

    if (node == NULL) {
        free(binding->class_name);
        free(binding->method_name);
        free(binding);
        return;
    }

    node->next = class_method_bindings;
    class_method_bindings = node;
    
    if (getenv("KGPC_DEBUG_CLASS_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] Registered method %s.%s (virtual=%d, override=%d, static=%d)\n",
            class_name, method_name, is_virtual, is_override, is_static);
    }
}

void from_cparser_register_method_template(const char *class_name, const char *method_name,
    int is_virtual, int is_override, int is_static) {
    register_class_method_ex(class_name, method_name, is_virtual, is_override, is_static);
}



static const char *find_class_for_method(const char *method_name) {
    if (method_name == NULL)
        return NULL;

    ListNode_t *cur = class_method_bindings;
    while (cur != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
        if (binding != NULL && binding->method_name != NULL &&
            strcasecmp(binding->method_name, method_name) == 0)
            return binding->class_name;
        cur = cur->next;
    }
    return NULL;
}

/* Check if a method is static (no Self parameter) */
static int is_method_static(const char *class_name, const char *method_name) {
    if (class_name == NULL || method_name == NULL)
        return 0;

    int has_static = 0;
    int has_instance = 0;
    ListNode_t *cur = class_method_bindings;
    while (cur != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
        if (binding != NULL && binding->class_name != NULL && binding->method_name != NULL &&
            strcasecmp(binding->class_name, class_name) == 0 &&
            strcasecmp(binding->method_name, method_name) == 0)
        {
            if (binding->is_static)
                has_static = 1;
            else
                has_instance = 1;
        }
        cur = cur->next;
    }
    if (has_instance)
        return 0;
    return has_static;
}

/* Public wrapper for is_method_static */
int from_cparser_is_method_static(const char *class_name, const char *method_name) {
    return is_method_static(class_name, method_name);
}

/* Check if a method is virtual (needs VMT dispatch) */
int from_cparser_is_method_virtual(const char *class_name, const char *method_name) {
    if (class_name == NULL || method_name == NULL)
        return 0;

    ListNode_t *cur = class_method_bindings;
    while (cur != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
        if (binding != NULL && binding->class_name != NULL && binding->method_name != NULL &&
            strcasecmp(binding->class_name, class_name) == 0 &&
            strcasecmp(binding->method_name, method_name) == 0)
            return binding->is_virtual || binding->is_override;
        cur = cur->next;
    }
    return 0;
}

/* Find all class names that have a method with the given name */
ListNode_t *from_cparser_find_classes_with_method(const char *method_name, int *count_out) {
    if (count_out != NULL) *count_out = 0;
    if (method_name == NULL) return NULL;

    ListNode_t *result = NULL;
    int count = 0;

    ListNode_t *cur = class_method_bindings;
    while (cur != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
        if (binding != NULL && binding->method_name != NULL &&
            strcasecmp(binding->method_name, method_name) == 0) {
            /* Check if we already have this class in the result */
            ListNode_t *check = result;
            int found = 0;
            while (check != NULL) {
                char *existing_class = (char *)check->cur;
                if (existing_class != NULL && strcasecmp(existing_class, binding->class_name) == 0) {
                    found = 1;
                    break;
                }
                check = check->next;
            }
            if (!found && binding->class_name != NULL) {
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
        cur = cur->next;
    }

    if (count_out != NULL) *count_out = count;
    return result;
}

static int typed_const_counter = 0;

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
    if (strcmp(op_name, ":=") == 0) return strdup("op_assign");
    
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

/* Get method information for a class */
void get_class_methods(const char *class_name, ListNode_t **methods_out, int *count_out) {
    if (methods_out != NULL)
        *methods_out = NULL;
    if (count_out != NULL)
        *count_out = 0;
    
    if (class_name == NULL || methods_out == NULL || count_out == NULL)
        return;
    
    ListNode_t *head = NULL;
    ListNode_t **tail = &head;
    int count = 0;
    
    ListNode_t *cur = class_method_bindings;
    while (cur != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
        if (binding != NULL && binding->class_name != NULL &&
            strcasecmp(binding->class_name, class_name) == 0) {
            /* Found a method for this class - create a copy of the list node */
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
        cur = cur->next;
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
    if (getenv("KGPC_DEBUG_TFPG") != NULL)
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
    if (getenv("KGPC_DEBUG_TFPG") != NULL)
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

        if (node != NULL && node->typ == PASCAL_T_TYPE_ARG)
        {
            char *dup = dup_first_identifier_in_node(node);
            if (getenv("KGPC_DEBUG_TFPG") != NULL)
                fprintf(stderr, "[KGPC] collect args extracted=%s\n", dup != NULL ? dup : "<null>");
            if (dup != NULL)
            {
                ListNode_t *append_node = list_builder_append(&builder, dup, LIST_STRING);
                if (getenv("KGPC_DEBUG_TFPG") != NULL)
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
    if (getenv("KGPC_DEBUG_TFPG") != NULL)
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

static void substitute_record_type_parameters(struct RecordType *record, GenericTypeDecl *generic_decl, char **arg_types);

static void substitute_record_field(struct RecordField *field, GenericTypeDecl *generic_decl, char **arg_types) {
    if (field == NULL)
        return;
    
    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && field->name != NULL)
    {
        fprintf(stderr, "[KGPC] substitute_record_field BEFORE: name=%s is_array=%d type_id=%s array_element_type_id=%s\n",
            field->name, field->is_array,
            field->type_id ? field->type_id : "<null>",
            field->array_element_type_id ? field->array_element_type_id : "<null>");
    }
    
    substitute_identifier(&field->type_id, generic_decl, arg_types);
    if (field->array_element_type_id != NULL)
        substitute_identifier(&field->array_element_type_id, generic_decl, arg_types);
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
        }
        prop_node = prop_node->next;
    }
}

static struct RecordType *instantiate_generic_record(const char *base_name, ListNode_t *type_args, char **specialized_name_out) {
    if (specialized_name_out != NULL)
        *specialized_name_out = NULL;
    if (base_name == NULL)
        return NULL;

    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
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
    generic_registry_add_specialization(base_name, record->generic_args, arg_count);

    if (specialized_name_out != NULL)
        *specialized_name_out = specialized_name;
    else
        free(specialized_name);

    if (debug_env != NULL && record->type_id != NULL)
        fprintf(stderr, "[KGPC] instantiated generic record %s\n", record->type_id);
    return record;
}

static void record_generic_method_impl(const char *class_name, const char *method_name, ast_t *method_ast)
{
    if (class_name == NULL || method_name == NULL || method_ast == NULL)
        return;

    GenericTypeDecl *generic = generic_registry_find_decl(class_name);
    if (generic == NULL || generic->record_template == NULL) {
        if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && class_name != NULL)
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
                template->method_impl_ast = copy_ast(method_ast);
                if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL)
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

    ast_t *cursor = node;
    while (cursor != NULL)
    {
        if (cursor->sym != NULL && cursor->sym->name != NULL)
        {
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

static Tree_t *instantiate_method_template(struct MethodTemplate *method_template, struct RecordType *record)
{
    if (method_template == NULL || method_template->method_impl_ast == NULL || record == NULL)
        return NULL;

    ast_t *method_copy = copy_ast(method_template->method_impl_ast);
    if (method_copy == NULL)
        return NULL;

    rewrite_method_impl_ast(method_copy, record);
    Tree_t *method_tree = convert_method_impl(method_copy);
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

static Tree_t *convert_procedure(ast_t *proc_node);
static Tree_t *convert_function(ast_t *func_node);
static Tree_t *convert_method_impl(ast_t *method_node);

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
        return;

    append_subprogram_node(dest, tree);
}

static void append_subprograms_from_ast_recursive(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited)
{
    if (node == NULL || node == ast_nil || subprograms == NULL || visited == NULL)
        return;

    if (!is_safe_to_continue(visited, node))
        return;

    switch (node->typ)
    {
    case PASCAL_T_PROCEDURE_DECL: {
        Tree_t *proc = convert_procedure(node);
        append_subprogram_if_unique(subprograms, proc);
        append_subprograms_from_ast_recursive(node->next, subprograms, visited);
        return;
    }
    case PASCAL_T_FUNCTION_DECL: {
        Tree_t *func = convert_function(node);
        append_subprogram_if_unique(subprograms, func);
        append_subprograms_from_ast_recursive(node->next, subprograms, visited);
        return;
    }
    case PASCAL_T_METHOD_IMPL:
    case PASCAL_T_CONSTRUCTOR_DECL:
    case PASCAL_T_DESTRUCTOR_DECL: {
        Tree_t *method_tree = convert_method_impl(node);
        append_subprogram_if_unique(subprograms, method_tree);
        append_subprograms_from_ast_recursive(node->next, subprograms, visited);
        return;
    }
    default:
        break;
    }

    append_subprograms_from_ast_recursive(node->child, subprograms, visited);
    append_subprograms_from_ast_recursive(node->next, subprograms, visited);
}

static void append_top_level_subprograms_from_ast(ast_t *node, ListNode_t **subprograms,
    VisitedSet *visited, int in_subprogram)
{
    if (node == NULL || node == ast_nil || subprograms == NULL || visited == NULL)
        return;

    if (visited_set_contains(visited, node))
    {
        /* Still traverse siblings to avoid skipping shared subtrees. */
        append_top_level_subprograms_from_ast(node->next, subprograms, visited, in_subprogram);
        return;
    }
    visited_set_add(visited, node);

    int is_subprogram = (node->typ == PASCAL_T_PROCEDURE_DECL ||
        node->typ == PASCAL_T_FUNCTION_DECL ||
        node->typ == PASCAL_T_METHOD_IMPL ||
        node->typ == PASCAL_T_CONSTRUCTOR_DECL ||
        node->typ == PASCAL_T_DESTRUCTOR_DECL);

    if (is_subprogram && !in_subprogram)
    {
        if (node->typ == PASCAL_T_PROCEDURE_DECL)
        {
            Tree_t *proc = convert_procedure(node);
            append_subprogram_if_unique(subprograms, proc);
        }
        else if (node->typ == PASCAL_T_FUNCTION_DECL)
        {
            Tree_t *func = convert_function(node);
            append_subprogram_if_unique(subprograms, func);
        }
        else
        {
            Tree_t *method_tree = convert_method_impl(node);
            append_subprogram_if_unique(subprograms, method_tree);
        }
    }

    int child_in_subprogram = in_subprogram || is_subprogram;
    append_top_level_subprograms_from_ast(node->child, subprograms, visited, child_in_subprogram);
    append_top_level_subprograms_from_ast(node->next, subprograms, visited, in_subprogram);
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
                tmpl->method_impl_ast = copy_ast(src_tmpl->method_impl_ast);
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
        if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL &&
            record != NULL && record->type_id != NULL)
            fprintf(stderr, "[KGPC] skipping clone for %s (missing templates)\n", record->type_id);
        return;
    }
    if (record->method_clones_emitted)
        return;

    const char *debug_env = getenv("KGPC_DEBUG_GENERIC_CLONES");
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

void resolve_pending_generic_aliases(Tree_t *program_tree)
{
    PendingGenericAlias *cur = g_pending_generic_aliases;
    g_pending_generic_aliases = NULL;
    ListNode_t **clone_dest = NULL;
    const char *debug_env = getenv("KGPC_DEBUG_TFPG");
    if (program_tree != NULL && program_tree->type == TREE_PROGRAM_TYPE)
        clone_dest = &program_tree->tree_data.program_data.subprograms;

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
                if (record->is_class)
                    inline_type = create_pointer_type(inline_type);
                cur->decl->tree_data.type_decl_data.kgpc_type = inline_type;
            }
            if (clone_dest != NULL)
                append_specialized_method_clones(cur->decl, clone_dest);
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
    if (getenv("KGPC_DEBUG_TFPG") != NULL)
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

static struct Expression *convert_case_label_expression(ast_t *node) {
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
    if (strcasecmp(name, "double") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("double");
        return REAL_TYPE;
    }
    if (strcasecmp(name, "string") == 0 ||
        strcasecmp(name, "ansistring") == 0 ||
        strcasecmp(name, "widestring") == 0) {
        if (type_id_out != NULL)
            *type_id_out = strdup("string");
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
    ListNode_t **subprograms, ListNode_t **const_decls, ListBuilder *var_builder);

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
                    if (literal->typ == PASCAL_T_IDENTIFIER && literal->sym != NULL) {
                        if (strcmp(literal->sym->name, identifier) == 0) {
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
                        if (lit->typ == PASCAL_T_IDENTIFIER && lit->sym != NULL &&
                            lit->sym->name != NULL &&
                            strcasecmp(lit->sym->name, literal) == 0) {
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
                if (strcmp(type_id->sym->name, type_name) == 0) {
                    /* Found the type - now check if it's an enumerated type */
                    ast_t *type_spec_node = type_id->next;
                    
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
                        /* Count the enum values */
                        int count = 0;
                        ast_t *literal = spec->child;
                        while (literal != NULL) {
                            if (literal->typ == PASCAL_T_IDENTIFIER)
                                count++;
                            literal = literal->next;
                        }
                        
                        if (count > 0) {
                            *out_start = 0;
                            *out_end = count - 1;
                            return 0; /* Success */
                        }
                    }
                    
                    /* Found the type but it's not an enum */
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
        if (resolve_enum_type_range_from_ast(range_str, type_section, &enum_start, &enum_end) == 0) {
            info->start = enum_start;
            info->end = enum_end;
        } else if (id_for_error != NULL) {
            fprintf(stderr, "ERROR: Could not resolve array index type '%s' for %s.\n",
                    range_str, id_for_error);
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
        if (resolve_const_int_in_section(identifier, const_section, &resolved, depth) == 0)
            found = 1;
    }

    if (found)
        return resolved;
    return fallback_value; /* Not found */
}

static int resolve_const_int_from_ast(const char *identifier, ast_t *const_section, int fallback_value) {
    return resolve_const_int_from_ast_internal(identifier, const_section, fallback_value, 0);
}

/* Evaluate simple const expression like "NUM-1" or "NUM+1" */
static int evaluate_simple_const_expr(const char *expr, ast_t *const_section, int *result) {
    if (expr == NULL || result == NULL)
        return -1;
    
    /* Try to find '-' or '+' operator */
    const char *minus = strstr(expr, "-");
    const char *plus = strstr(expr, "+");
    
    if (minus == NULL && plus == NULL) {
        /* No operator - try as simple identifier */
        int val = resolve_const_int_from_ast(expr, const_section, INT_MIN);
        if (val != INT_MIN) {
            *result = val;
            return 0;
        }
        /* Try as numeric literal */
        char *endptr;
        long num = strtol(expr, &endptr, 10);
        if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX) {
            *result = (int)num;
            return 0;
        }
        return -1;
    }
    
    const char *op = (minus != NULL) ? minus : plus;
    int is_minus = (minus != NULL);
    
    /* Extract left and right parts */
    size_t left_len = op - expr;
    char *left_part = (char *)malloc(left_len + 1);
    if (left_part == NULL)
        return -1;
    strncpy(left_part, expr, left_len);
    left_part[left_len] = '\0';
    
    const char *right_part = op + 1;
    
    /* Trim leading whitespace from left_part using pointer advance */
    char *trimmed_left = left_part;
    while (*trimmed_left == ' ' || *trimmed_left == '\t') trimmed_left++;
    /* If we advanced, shift content to beginning */
    if (trimmed_left != left_part) {
        memmove(left_part, trimmed_left, strlen(trimmed_left) + 1);
    }
    /* Trim trailing whitespace */
    size_t len = strlen(left_part);
    char *p = (len > 0) ? (left_part + len - 1) : left_part;
    while (p > left_part && (*p == ' ' || *p == '\t')) *p-- = '\0';
    
    /* Skip leading whitespace from right_part */
    while (*right_part == ' ' || *right_part == '\t') right_part++;
    
    /* Evaluate left part */
    int left_val;
    int left_result = resolve_const_int_from_ast(left_part, const_section, INT_MIN);
    if (left_result != INT_MIN) {
        left_val = left_result;
    } else {
        char *endptr;
        long num = strtol(left_part, &endptr, 10);
        if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX) {
            left_val = (int)num;
        } else {
            free(left_part);
            return -1;
        }
    }
    free(left_part);
    
    /* Evaluate right part */
    int right_val;
    int right_result = resolve_const_int_from_ast(right_part, const_section, INT_MIN);
    if (right_result != INT_MIN) {
        right_val = right_result;
    } else {
        char *endptr;
        long num = strtol(right_part, &endptr, 10);
        if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX) {
            right_val = (int)num;
        } else {
            return -1;
        }
    }
    
    /* Compute result */
    if (is_minus) {
        *result = left_val - right_val;
    } else {
        *result = left_val + right_val;
    }
    return 0;
}

/* Serialize an expression AST node to a string representation.
 * Handles simple identifiers, literals, and binary operations (+, -).
 * Returns a malloc'd string or NULL on failure. */
static char *serialize_expr_to_string(ast_t *expr) {
    if (expr == NULL)
        return NULL;
    
    /* Simple identifier or literal with a symbol */
    if (expr->sym != NULL && expr->sym->name != NULL) {
        return strdup(expr->sym->name);
    }
    
    /* Unary minus: -X */
    if (expr->typ == PASCAL_T_NEG && expr->child != NULL) {
        char *inner = serialize_expr_to_string(expr->child);
        if (inner == NULL)
            return NULL;
        size_t len = strlen(inner) + 2;  /* '-' + inner + '\0' */
        char *result = (char *)malloc(len);
        if (result == NULL) {
            free(inner);
            return NULL;
        }
        snprintf(result, len, "-%s", inner);
        free(inner);
        return result;
    }
    
    /* Binary subtraction: X - Y */
    if (expr->typ == PASCAL_T_SUB && expr->child != NULL && expr->child->next != NULL) {
        char *left = serialize_expr_to_string(expr->child);
        char *right = serialize_expr_to_string(expr->child->next);
        if (left == NULL || right == NULL) {
            if (left) free(left);
            if (right) free(right);
            return NULL;
        }
        size_t len = strlen(left) + strlen(right) + 4;  /* left + '-' + right + '\0' + extra */
        char *result = (char *)malloc(len);
        if (result == NULL) {
            free(left);
            free(right);
            return NULL;
        }
        snprintf(result, len, "%s-%s", left, right);
        free(left);
        free(right);
        return result;
    }
    
    /* Binary addition: X + Y */
    if (expr->typ == PASCAL_T_ADD && expr->child != NULL && expr->child->next != NULL) {
        char *left = serialize_expr_to_string(expr->child);
        char *right = serialize_expr_to_string(expr->child->next);
        if (left == NULL || right == NULL) {
            if (left) free(left);
            if (right) free(right);
            return NULL;
        }
        size_t len = strlen(left) + strlen(right) + 4;
        char *result = (char *)malloc(len);
        if (result == NULL) {
            free(left);
            free(right);
            return NULL;
        }
        snprintf(result, len, "%s+%s", left, right);
        free(left);
        free(right);
        return result;
    }
    
    /* If we have a child with a symbol (wrapped expression) */
    if (expr->child != NULL && expr->child->sym != NULL && expr->child->sym->name != NULL) {
        return strdup(expr->child->sym->name);
    }
    
    return NULL;
}

static int resolve_const_expr_to_int(ast_t *expr, int *out_value)
{
    if (out_value == NULL)
        return -1;
    *out_value = 0;
    if (expr == NULL)
        return -1;

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
        type_info->is_open_array = 0;
        type_info->array_dimensions = NULL;
        type_info->is_pointer = 0;
        type_info->pointer_type = UNKNOWN_TYPE;
        type_info->pointer_type_id = NULL;
        type_info->is_set = 0;
        type_info->set_element_type = UNKNOWN_TYPE;
        type_info->set_element_type_id = NULL;
        type_info->is_enum = 0;
        type_info->enum_literals = NULL;
        type_info->is_file = 0;
        type_info->file_type = UNKNOWN_TYPE;
        type_info->file_type_id = NULL;
        type_info->is_record = 0;
        type_info->record_type = NULL;
        type_info->is_generic_specialization = 0;
        type_info->generic_base_name = NULL;
        type_info->generic_type_args = NULL;
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

    if (getenv("KGPC_DEBUG_TFPG") != NULL)
        fprintf(stderr, "[KGPC] convert_type_spec node typ=%d (%s) sym=%s\n",
            spec_node->typ,
            pascal_tag_to_string(spec_node->typ),
            (spec_node->sym != NULL && spec_node->sym->name != NULL) ? spec_node->sym->name : "<null>");

    if (spec_node->typ == PASCAL_T_IDENTIFIER) {
        char *dup = dup_symbol(spec_node);

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
                type_info->is_shortstring = 1;

                ListBuilder dims_builder;
                list_builder_init(&dims_builder);
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "0..%d", size_val);
                list_builder_append(&dims_builder, strdup(buffer), LIST_STRING);
                type_info->array_dimensions = list_builder_finish(&dims_builder);
            }

            free(dup);
            return UNKNOWN_TYPE;
        }

        int result = map_type_name(dup, type_id_out);
        if (type_info != NULL && result == FILE_TYPE) {
            type_info->is_file = 1;
            type_info->file_type = FILE_TYPE;
            if (type_id_out != NULL && *type_id_out != NULL)
                type_info->file_type_id = strdup(*type_id_out);
        }
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

            if (record != NULL) {
                if (getenv("KGPC_DEBUG_TFPG") != NULL && specialized_name != NULL)
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

        if (type_info != NULL) {
            type_info->is_range = 1;
            type_info->range_start = start_value;
            type_info->range_end = end_value;
            type_info->range_known = (have_start && have_end);
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
                    
                    if (lower_str != NULL && upper_str != NULL) {
                        if (dims_builder.head == NULL) {
                            int start_val = 0;
                            int end_val = 0;
                            if (resolve_const_expr_from_sections(lower_str, &start_val) != 0)
                                start_val = atoi(lower_str);
                            if (resolve_const_expr_from_sections(upper_str, &end_val) != 0)
                                end_val = atoi(upper_str);
                            type_info->start = start_val;
                            type_info->end = end_val;
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
                        else if (mapped != UNKNOWN_TYPE)
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
                        destroy_record_type(nested_record);
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
                else if (mapped != UNKNOWN_TYPE)
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
                    else if (mapped != UNKNOWN_TYPE)
                        free(dup);
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
            while (elem != NULL && elem->typ == PASCAL_T_NONE)
                elem = elem->child;
            if (elem != NULL && elem->typ == PASCAL_T_IDENTIFIER) {
                char *dup = dup_symbol(elem);
                int mapped = map_type_name(dup, &type_info->file_type_id);
                type_info->file_type = mapped;
                if (mapped == UNKNOWN_TYPE && type_info->file_type_id == NULL)
                    type_info->file_type_id = dup;
                else if (mapped != UNKNOWN_TYPE)
                    free(dup);
            }
        }
        return FILE_TYPE;
    }

    if (spec_node->typ == PASCAL_T_ENUMERATED_TYPE) {
        if (type_info != NULL) {
            type_info->is_enum = 1;
            ListBuilder enum_builder;
            list_builder_init(&enum_builder);
            ast_t *value = spec_node->child;
            while (value != NULL) {
                if (value->typ == PASCAL_T_IDENTIFIER)
                    list_builder_append(&enum_builder, dup_symbol(value), LIST_STRING);
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

    /* Distinct type: type Real = Double - creates a strong alias */
    if (spec_node->typ == PASCAL_T_DISTINCT_TYPE) {
        /* The child should be the target type identifier */
        ast_t *target = spec_node->child;
        if (target != NULL) {
            target = unwrap_pascal_node(target);
            if (target != NULL && target->typ == PASCAL_T_IDENTIFIER) {
                char *dup = dup_symbol(target);
                int result = map_type_name(dup, type_id_out);
                if (result == UNKNOWN_TYPE && type_id_out != NULL && *type_id_out == NULL) {
                    *type_id_out = dup;
                } else {
                    free(dup);
                }
                return result;
            }
        }
        /* Fallback for distinct types with complex target */
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

    if (getenv("KGPC_DEBUG_TFPG") != NULL)
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
        
        if (type_tag != UNKNOWN_TYPE) {
            KgpcType *type = create_primitive_type(type_tag);
            /* If this is RawByteString or UnicodeString, create a type_alias to preserve the name */
            if (type != NULL && preserved_type_id != NULL &&
                (strcasecmp(preserved_type_id, "RawByteString") == 0 ||
                 strcasecmp(preserved_type_id, "UnicodeString") == 0)) {
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

        return create_array_type(element_type, start, end);
    }

    /* Handle file types */
    if (spec_node->typ == PASCAL_T_FILE_TYPE) {
        ast_t *elem = spec_node->child;
        while (elem != NULL && elem->typ == PASCAL_T_NONE)
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

            if (points_to != NULL) {
                return create_pointer_type(points_to);
            }
        }
        return NULL;
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
                        if (ret_tag != UNKNOWN_TYPE) {
                            return_type = create_primitive_type(ret_tag);
                        } else {
                            // Check if it's a user-defined type in the symbol table
                            HashNode_t *type_node = NULL;
                            if (symtab != NULL && FindIdent(&type_node, symtab, ret_type_name) != -1 && 
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
        if (proc_type != NULL && return_type_id != NULL)
            proc_type->info.proc_info.return_type_id = return_type_id;
        else if (return_type_id != NULL)
            free(return_type_id);
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
static void convert_record_members(ast_t *node, ListBuilder *builder);
static struct VariantPart *convert_variant_part(ast_t *variant_node, ListNode_t **out_tag_fields);
static struct VariantBranch *convert_variant_branch(ast_t *branch_node);

static ListNode_t *convert_class_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    ast_t *cursor = field_decl_node->child;
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        return NULL;
    }

    /* Skip to the type specification */
    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_OBJECT_TYPE &&
           cursor->typ != PASCAL_T_IDENTIFIER && cursor->typ != PASCAL_T_ARRAY_TYPE) {
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
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
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
            field_desc->type_id = type_id_copy;
            field_desc->nested_record = nested_copy;
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_id = field_info.element_type_id;
            field_desc->array_is_open = field_info.is_open_array;
            field_info.element_type_id = NULL;  /* Ownership transferred */
            field_desc->is_hidden = 0;
            /* Copy pointer type info for inline pointer fields like ^Char */
            field_desc->is_pointer = field_info.is_pointer;
            field_desc->pointer_type = field_info.pointer_type;
            if (field_info.pointer_type_id != NULL)
                field_desc->pointer_type_id = strdup(field_info.pointer_type_id);
            else
                field_desc->pointer_type_id = NULL;
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

    ast_t *cursor = property_node->child;
    if (cursor != NULL && cursor->typ == PASCAL_T_NONE && cursor->child != NULL)
        cursor = cursor->child;
    if (getenv("KGPC_DEBUG_PROPERTY") != NULL)
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

            if (property_name == NULL)
            {
                property_name = dup;
            }
            else if (type_node == NULL)
            {
                type_node = unwrapped;
                free(dup);
            }
            else if (read_accessor == NULL)
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

    if (type_node != NULL)
    {
        property_type = convert_type_spec(type_node, &property_type_id, &inline_record, &type_info);
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
    property->read_accessor = read_accessor;
    property->write_accessor = write_accessor;
    property->is_indexed = has_indexer;

    return property;
}

static void append_module_property_wrappers(ListNode_t **subprograms, ast_t *property_node)
{
    if (subprograms == NULL || property_node == NULL)
        return;

    struct ClassProperty *prop = convert_property_decl(property_node);
    if (prop == NULL)
        return;

    if (getenv("KGPC_DEBUG_PROPERTY") != NULL)
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
                                    append_subprogram_node(subprograms, func_tree);
                                    if (getenv("KGPC_DEBUG_PROPERTY") != NULL)
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
    
    /* First pass: check ALL children for "class" keyword before the method name.
     * The parser places optional(token(keyword_ci("class"))) before the function keyword,
     * so we need to scan all children to find it. */
    ast_t *cursor = method_ast->child;
    while (cursor != NULL)
    {
        ast_t *node = unwrap_pascal_node(cursor);
        if (node == NULL)
            node = cursor;
        const char *sym_name = (node->sym != NULL) ? node->sym->name : NULL;
        
        /* Check for "class" keyword in any child node */
        if (sym_name != NULL && strcasecmp(sym_name, "class") == 0) {
            method_template->is_class_method = 1;
            method_template->is_static = 1;
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
                    method_template->params_ast != NULL &&
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

static struct MethodTemplate *create_method_template(ast_t *method_decl_node)
{
    if (method_decl_node == NULL)
        return NULL;

    ast_t *name_node = method_decl_node->child;
    while (name_node != NULL && name_node->typ != PASCAL_T_IDENTIFIER)
        name_node = name_node->next;
    if (name_node == NULL || name_node->sym == NULL || name_node->sym->name == NULL)
        return NULL;

    struct MethodTemplate *template = (struct MethodTemplate *)calloc(1, sizeof(struct MethodTemplate));
    if (template == NULL)
        return NULL;

    template->name = strdup(name_node->sym->name);
    if (template->name == NULL)
    {
        free(template);
        return NULL;
    }

    template->method_ast = copy_ast(method_decl_node);
    if (template->method_ast == NULL)
    {
        free(template->name);
        free(template);
        return NULL;
    }

    annotate_method_template(template, template->method_ast);
    template->method_impl_ast = NULL;
    return template;
}

static void destroy_method_template_instance(struct MethodTemplate *template)
{
    if (template == NULL)
        return;
    if (template->name != NULL)
        free(template->name);
    if (template->method_ast != NULL)
        free_ast(template->method_ast);
    if (template->method_impl_ast != NULL)
        free_ast(template->method_impl_ast);
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
            if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                fprintf(stderr, "[KGPC] collect_class_members: node typ=%d (%s) in %s\n",
                    unwrapped->typ, pascal_tag_to_string(unwrapped->typ),
                    class_name ? class_name : "<unknown>");
            }
            switch (unwrapped->typ) {
            case PASCAL_T_CLASS_MEMBER:
                collect_class_members(unwrapped->child, class_name, field_builder, property_builder, method_builder, nested_type_builder);
                break;
            case PASCAL_T_FIELD_DECL: {
                ListNode_t *fields = convert_class_field_decl(unwrapped);
                list_builder_extend(field_builder, fields);
                break;
            }
            case PASCAL_T_METHOD_DECL:
            case PASCAL_T_CONSTRUCTOR_DECL:
            case PASCAL_T_DESTRUCTOR_DECL: {
                struct MethodTemplate *template = create_method_template(unwrapped);
                if (template == NULL)
                    break;

                if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL && template->name != NULL)
                    fprintf(stderr, "[KGPC] captured template %s.%s\n",
                        class_name != NULL ? class_name : "<unknown>", template->name);

                register_class_method_ex(class_name, template->name,
                    template->is_virtual, template->is_override, template->is_static);

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
                    if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
                        fprintf(stderr, "[KGPC] collect_class_members: found NESTED_TYPE_SECTION in %s at line %d\n",
                            class_name ? class_name : "<unknown>", unwrapped->line);
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

static struct RecordType *convert_class_type_ex(const char *class_name, ast_t *class_node, ListNode_t **nested_types_out) {
    if (class_node == NULL)
        return NULL;

    if (nested_types_out != NULL)
        *nested_types_out = NULL;

    if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL && class_name != NULL)
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

    if (body_start != NULL && body_start->typ == PASCAL_T_IDENTIFIER) {
        // First child is parent class name, extract it
        if (body_start->sym != NULL && body_start->sym->name != NULL) {
            parent_class_name = strdup(body_start->sym->name);
        }
        // Move to the actual class body (next sibling)
        body_start = body_start->next;
    }

    if (getenv("KGPC_DEBUG_CLASS_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_class_type: processing class %s\n", class_name ? class_name : "<null>");
        if (body_start != NULL) {
            fprintf(stderr, "[KGPC]   body_start type: %d\n", body_start->typ);
        } else {
            fprintf(stderr, "[KGPC]   body_start is NULL\n");
        }
    }

    collect_class_members(body_start, class_name, &field_builder, &property_builder, &method_template_builder, &nested_type_builder);

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
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = class_name != NULL ? strdup(class_name) : NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->method_clones_emitted = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->default_indexed_element_type_id = NULL;

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

static ListNode_t *convert_field_decl(ast_t *field_decl_node) {
    if (field_decl_node == NULL || field_decl_node->typ != PASCAL_T_FIELD_DECL)
        return NULL;

    ast_t *cursor = field_decl_node->child;
    ListNode_t *names = convert_identifier_list(&cursor);
    if (names == NULL) {
        fprintf(stderr, "ERROR: record field declaration missing identifier list.\n");
        return NULL;
    }

    while (cursor != NULL && cursor->typ != PASCAL_T_TYPE_SPEC &&
           cursor->typ != PASCAL_T_RECORD_TYPE && cursor->typ != PASCAL_T_OBJECT_TYPE &&
           cursor->typ != PASCAL_T_IDENTIFIER &&
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
                field_type = mapped_type;
                field_type_id = mapped_id;
                free(candidate);
            } else {
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
            field_desc->type_id = type_id_copy;
            field_desc->nested_record = nested_copy;
            field_desc->proc_type = inline_proc_type;
            if (field_desc->proc_type != NULL)
                kgpc_type_retain(field_desc->proc_type);
            field_desc->is_array = field_info.is_array;
            field_desc->array_start = field_info.start;
            field_desc->array_end = field_info.end;
            field_desc->array_element_type = field_info.element_type;
            field_desc->array_element_type_id = field_info.element_type_id;
            field_desc->array_is_open = field_info.is_open_array;
            field_info.element_type_id = NULL;
            /* Copy pointer type info for inline pointer fields like ^Char */
            field_desc->is_pointer = field_info.is_pointer;
            field_desc->pointer_type = field_info.pointer_type;
            if (field_info.pointer_type_id != NULL)
                field_desc->pointer_type_id = strdup(field_info.pointer_type_id);
            else
                field_desc->pointer_type_id = NULL;
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
    convert_record_members(cursor, &members_builder);
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

static void convert_record_members(ast_t *node, ListBuilder *builder) {
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
        } else if (cur->typ == PASCAL_T_METHOD_DECL) {
            /* Store method declaration as a special marker node for operator overloading */
            /* We'll handle this during semantic check when we know the record type name */
            list_builder_append(builder, cur, LIST_UNSPECIFIED);
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
                if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
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
        record->is_type_helper = 1;
        record->helper_base_type_id = NULL;
        record->helper_parent_id = NULL;
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
            if (unwrapped != NULL &&
                (unwrapped->typ == PASCAL_T_IDENTIFIER || unwrapped->typ == PASCAL_T_QUALIFIED_IDENTIFIER) &&
                unwrapped->sym != NULL && unwrapped->sym->name != NULL)
            {
                if (first_ident == NULL) {
                    first_ident = unwrapped;
                } else if (second_ident == NULL) {
                    second_ident = unwrapped;
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
    collect_record_nested_types(record_node->child, &nested_type_builder);

    if (nested_types_out != NULL)
        *nested_types_out = list_builder_finish(&nested_type_builder);
    else
        destroy_list(nested_type_builder.head);

    ListBuilder fields_builder;
    list_builder_init(&fields_builder);
    convert_record_members(record_node->child, &fields_builder);

    struct RecordType *record = (struct RecordType *)malloc(sizeof(struct RecordType));
    if (record == NULL) {
        destroy_list(fields_builder.head);
        return NULL;
    }
    record->fields = list_builder_finish(&fields_builder);
    record->properties = NULL;
    record->parent_class_name = NULL;  /* Regular records don't have parent classes */
    record->methods = NULL;  /* Regular records don't have methods */
    record->method_templates = NULL;
    record->is_class = 0;
    record->is_type_helper = 0;
    record->helper_base_type_id = NULL;
    record->helper_parent_id = NULL;
    record->type_id = NULL;
    record->has_cached_size = 0;
    record->cached_size = 0;
    record->generic_decl = NULL;
    record->generic_args = NULL;
    record->num_generic_args = 0;
    record->method_clones_emitted = 0;
    record->default_indexed_property = NULL;
    record->default_indexed_element_type = UNKNOWN_TYPE;
    record->default_indexed_element_type_id = NULL;
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
        /* Check for default value node after type spec */
        if (type_node->next != NULL && type_node->next->typ == PASCAL_T_DEFAULT_VALUE) {
            default_value_node = type_node->next;
        }
        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
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
        if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
            fprintf(stderr, "[convert_param] default_value_node=%p expr_node=%p\n",
                (void*)default_value_node, (void*)expr_node);
        }
        if (expr_node != NULL) {
            struct Expression *default_expr = convert_expression(expr_node);
            if (default_expr != NULL) {
                /* Wrap expression in a var_assign statement with NULL var for storage */
                default_init = mk_varassign(default_value_node->line, default_value_node->col, 
                                            NULL, default_expr);
                if (getenv("KGPC_DEBUG_DEFAULT_PARAMS") != NULL) {
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
            }
        }
        /* Create TREE_ARR_DECL for inline array parameters */
        if (type_info.is_array)
        {
            int element_type = type_info.element_type;
            char *element_type_id = type_info.element_type_id != NULL ? strdup(type_info.element_type_id) : NULL;
            char *range_str = NULL;
            if (type_info.array_dimensions != NULL && type_info.array_dimensions->cur != NULL) {
                range_str = strdup((char *)type_info.array_dimensions->cur);
            }
            param_decl = mk_arraydecl(param_node->line, id_node, element_type, element_type_id,
                                      type_info.start, type_info.end, range_str, NULL);
            if (param_decl != NULL)
                param_decl->tree_data.arr_decl_data.is_shortstring = type_info.is_shortstring;
            /* Set var parameter flag on array declaration */
            if (is_var_param && param_decl != NULL)
                param_decl->tree_data.arr_decl_data.type = var_type; // Store this for compatibility
            /* Note: array parameters with default values are rare but could be supported */
        }
        else
        {
            param_decl = mk_vardecl(param_node->line, id_node, var_type, type_id_copy,
                is_var_param, 0, default_init, NULL, inline_alias, NULL);
            if (param_decl != NULL && (type_node == NULL || type_node->typ != PASCAL_T_TYPE_SPEC))
                param_decl->tree_data.var_decl_data.is_untyped_param = 1;
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
            var_type = convert_type_spec(search, &type_id, NULL, &type_info);
        } else if (search != NULL && search->typ == PASCAL_T_IDENTIFIER) {
            char *type_name = dup_symbol(search);
            if (type_name != NULL) {
                int mapped = map_type_name(type_name, &type_id);
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
                                    type_info.start, type_info.end, range_str, initializer_stmt);
        if (decl != NULL)
            decl->tree_data.arr_decl_data.is_shortstring = type_info.is_shortstring;
        type_info.element_type_id = NULL;
        destroy_type_info_contents(&type_info);
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
    if (type_info.is_file && type_id == NULL)
    {
        inline_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
        if (inline_alias != NULL)
        {
            inline_alias->is_file = 1;
            inline_alias->file_type = type_info.file_type;
            inline_alias->base_type = FILE_TYPE;
            if (type_info.file_type_id != NULL)
                inline_alias->file_type_id = strdup(type_info.file_type_id);
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
            }
            is_external = 1;
        } else if (scan->typ == PASCAL_T_PUBLIC_NAME) {
            /* Public name: variable is exported with specified symbol */
            if (scan->child != NULL && scan->child->typ == PASCAL_T_STRING) {
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
        if (getenv("KGPC_DEBUG_ABSOLUTE") != NULL) {
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
    int multidim_inner_start = 0, multidim_inner_end = -1;
    int multidim_infer_bounds = 0;  /* Set to 1 if we need to infer bounds from initializer */
    int dim3_start = 0, dim3_end = -1;
    int dim3_infer_bounds = 0;
    if (is_multidim) {
        /* Check for 3 dimensions */
        if (type_info->array_dimensions->next->next != NULL) {
            is_3d = 1;
            /* Check for more than 3 dimensions - not yet supported */
            if (type_info->array_dimensions->next->next->next != NULL) {
                fprintf(stderr, "ERROR: Unsupported 4+ dimensional const array %s.\n", *id_ptr);
                return -1;
            }
            /* Extract 2nd dimension bounds */
            const char *dim2_range = (const char *)type_info->array_dimensions->next->cur;
            if (dim2_range != NULL) {
                char *range_copy = strdup(dim2_range);
                if (range_copy != NULL) {
                    char *dotdot = strstr(range_copy, "..");
                    if (dotdot != NULL) {
                        *dotdot = '\0';
                        multidim_inner_start = atoi(range_copy);
                        multidim_inner_end = atoi(dotdot + 2);
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
                        dim3_start = atoi(range_copy);
                        dim3_end = atoi(dotdot + 2);
                    } else {
                        dim3_infer_bounds = 1;
                    }
                    free(range_copy);
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
                        multidim_inner_start = atoi(range_copy);
                        multidim_inner_end = atoi(dotdot + 2);
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
        fprintf(stderr, "ERROR: Open array typed const %s is not supported.\n", *id_ptr);
        return -1;
    }

    ast_t *tuple_node = value_node;
    int is_string_initializer = 0;
    if (tuple_node == NULL) {
        fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                *id_ptr);
        return -1;
    }
    if (tuple_node->typ == PASCAL_T_STRING) {
        int is_char_array = (type_info->element_type == CHAR_TYPE);
        int is_widechar_array = (type_info->element_type_id != NULL &&
                                 strcasecmp(type_info->element_type_id, "widechar") == 0);
        if (is_char_array || is_widechar_array) {
            is_string_initializer = 1;
        } else {
            fprintf(stderr, "ERROR: Const array %s string initializer requires a char array type.\n",
                    *id_ptr);
            return -1;
        }
    } else if (tuple_node->typ != PASCAL_T_TUPLE) {
        fprintf(stderr, "ERROR: Const array %s must use tuple syntax for its initializer.\n",
                *id_ptr);
        return -1;
    }

    int start = type_info->start;
    int end = type_info->end;
    
    resolve_array_bounds(type_info, type_section, const_section, *id_ptr);
    start = type_info->start;
    end = type_info->end;
    
    int expected_count = -1;
    if (end >= start)
        expected_count = end - start + 1;

    int actual_count = 0;
    if (is_string_initializer) {
        if (tuple_node->sym != NULL && tuple_node->sym->name != NULL)
            actual_count = (int)strlen(tuple_node->sym->name);
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

    if (expected_count >= 0 && actual_count != expected_count) {
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

    /* For 2D/3D arrays (array[a..b, c..d] or array[a..b, c..d, e..f]), treat as nested arrays */
    int element_is_2d_array = 0;  /* For 3D arrays, elements are 2D arrays */
    TypeInfo element_2d_inner_info = {0};
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
    }

    if (is_string_initializer) {
        const char *str = (tuple_node->sym != NULL) ? tuple_node->sym->name : NULL;
        if (str == NULL)
            str = "";
        for (int i = 0; i < actual_count; ++i) {
            struct Expression *rhs = mk_charcode(const_decl_node->line, (unsigned int)(unsigned char)str[i]);
            struct Expression *index_expr = mk_inum(const_decl_node->line, index);
            struct Expression *base_expr = mk_varid(const_decl_node->line, strdup(*id_ptr));
            struct Expression *lhs = mk_arrayaccess(const_decl_node->line, base_expr, index_expr);
            struct Statement *assign = mk_varassign(const_decl_node->line, const_decl_node->col, lhs, rhs);
            list_builder_append(&stmt_builder, assign, LIST_STMT);
            ++index;
        }
    } else {
        ast_t *element = tuple_node->child;
        while (element != NULL) {
            ast_t *unwrapped = unwrap_pascal_node(element);

            if (element_is_array) {
                if (unwrapped == NULL || unwrapped->typ != PASCAL_T_TUPLE) {
                    fprintf(stderr, "ERROR: Const array %s expects tuple initializer for element %d.\n",
                            *id_ptr, index);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    return -1;
                }

                int inner_start = element_array_info.start;
                int inner_end = element_array_info.end;
                int inner_expected = -1;
                if (inner_end >= inner_start)
                    inner_expected = inner_end - inner_start + 1;

                int inner_actual = 0;
                for (ast_t *inner = unwrapped->child; inner != NULL; inner = inner->next)
                    ++inner_actual;

                if (inner_expected >= 0 && inner_actual != inner_expected) {
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

                if (inner_expected >= 0 && inner_actual != inner_expected) {
                    fprintf(stderr,
                            "ERROR: Const array %s element %d initializer count %d does not match declared range %d..%d.\n",
                            *id_ptr, index, inner_actual, inner_start, inner_end);
                    destroy_list(stmt_builder.head);
                    destroy_type_info_contents(&element_array_info);
                    return -1;
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
                            struct Expression *outer_access = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                            struct Expression *inner_index_expr = mk_inum(element->line, inner_index);
                            struct Expression *middle_access = mk_arrayaccess(element->line, outer_access, inner_index_expr);
                            struct Expression *innermost_index_expr = mk_inum(element->line, innermost_index);
                            struct Expression *lhs = mk_arrayaccess(element->line, middle_access, innermost_index_expr);
                            struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                            list_builder_append(&stmt_builder, assign, LIST_STMT);
                            ++innermost_index;
                        }
                    } else {
                        struct Expression *rhs = convert_expression(inner_unwrapped);
                        if (rhs == NULL) {
                            fprintf(stderr, "ERROR: Unsupported const array element in %s[%d].\n", *id_ptr, index);
                            destroy_list(stmt_builder.head);
                            destroy_type_info_contents(&element_array_info);
                            return -1;
                        }

                        struct Expression *outer_index_expr = mk_inum(element->line, index);
                        struct Expression *base_expr = mk_varid(element->line, strdup(*id_ptr));
                        struct Expression *outer_access = mk_arrayaccess(element->line, base_expr, outer_index_expr);
                        struct Expression *inner_index_expr = mk_inum(element->line, inner_index);
                        struct Expression *lhs = mk_arrayaccess(element->line, outer_access, inner_index_expr);
                        struct Statement *assign = mk_varassign(element->line, element->col, lhs, rhs);
                        list_builder_append(&stmt_builder, assign, LIST_STMT);
                    }
                    ++inner_index;
                }

                ++index;
                element = element->next;
                continue;
            }

            /* Special handling for record constructors in const arrays */
            if (unwrapped != NULL && unwrapped->typ == PASCAL_T_RECORD_CONSTRUCTOR) {
                /* Generate field assignments for each field in the record constructor */
                ast_t *field_assignment = unwrapped->child;
                while (field_assignment != NULL) {
                    if (field_assignment->typ == PASCAL_T_ASSIGNMENT) {
                        ast_t *field_name_node = field_assignment->child;
                        ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
                        
                        if (field_name_node != NULL && field_value_node != NULL && field_name_node->sym != NULL) {
                            char *field_name = field_name_node->sym->name;
                            struct Expression *field_value = convert_expression(field_value_node);
                            
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
            element = element->next;
        }
    }

    destroy_type_info_contents(&element_array_info);
    destroy_type_info_contents(&element_2d_inner_info);

    ListNode_t *assignments = list_builder_finish(&stmt_builder);
    struct Statement *initializer = NULL;
    if (assignments != NULL)
        initializer = mk_compoundstatement(const_decl_node->line, assignments);

    ListNode_t *ids = CreateListNode(*id_ptr, LIST_STRING);
    char *range_str = NULL;
    if (type_info->array_dimensions != NULL && type_info->array_dimensions->cur != NULL) {
        range_str = strdup((char *)type_info->array_dimensions->cur);
    }
    Tree_t *array_decl = mk_arraydecl(const_decl_node->line, ids, type_info->element_type,
                                      type_info->element_type_id, start, end, range_str, initializer);
    type_info->element_type_id = NULL;

    if (type_info->array_dimensions != NULL) {
        destroy_list(type_info->array_dimensions);
        type_info->array_dimensions = NULL;
    }

    array_decl->tree_data.arr_decl_data.is_typed_const = 1;
    array_decl->tree_data.arr_decl_data.has_static_storage = 1;

    char label_buffer[64];
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_array_%d", typed_const_counter);
    array_decl->tree_data.arr_decl_data.static_label = strdup(label_buffer);
    snprintf(label_buffer, sizeof(label_buffer), "__kgpc_tconst_guard_%d", typed_const_counter);
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

    if (cur != NULL && cur->typ == PASCAL_T_TYPE_SPEC) {
        convert_type_spec(cur, &type_id, NULL, &type_info);
        cur = cur->next;
    } else if (cur != NULL && cur->typ == PASCAL_T_NONE) {
        ast_t *type_node = cur->child;
        while (type_node != NULL &&
               type_node->typ != PASCAL_T_TYPE_SPEC &&
               type_node->typ != PASCAL_T_IDENTIFIER) {
            type_node = type_node->next;
        }
        if (type_node != NULL) {
            if (type_node->typ == PASCAL_T_TYPE_SPEC) {
                convert_type_spec(type_node, &type_id, NULL, &type_info);
            } else if (type_node->typ == PASCAL_T_IDENTIFIER) {
                char *type_name = dup_symbol(type_node);
                if (type_name != NULL) {
                    int mapped = map_type_name(type_name, &type_id);
                    if (mapped == UNKNOWN_TYPE && type_id == NULL)
                        type_id = type_name;
                    else
                        free(type_name);
                }
            }
            cur = cur->next;
        }
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
            list_builder_append(var_builder, decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
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
            list_builder_append(var_builder, decl, LIST_TREE);

            destroy_type_info_contents(&type_info);
            return NULL;
        }

        if (empty_tuple_record_const) {
            int var_type = UNKNOWN_TYPE;
            if (type_id != NULL)
                var_type = map_type_name(type_id, NULL);

            ListNode_t *var_ids = CreateListNode(id, LIST_STRING);
            Tree_t *var_decl = mk_vardecl(const_decl_node->line, var_ids, var_type,
                type_id, 0, 0, NULL, NULL, NULL, NULL);
            var_decl->tree_data.var_decl_data.is_typed_const = 1;

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
        
        ast_t *field_assignment = value_node->child;
        while (field_assignment != NULL) {
            if (field_assignment->typ == PASCAL_T_ASSIGNMENT) {
                ast_t *field_name_node = field_assignment->child;
                ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
                
                if (field_name_node != NULL && field_value_node != NULL && field_name_node->sym != NULL) {
                    char *field_name = field_name_node->sym->name;
                    struct Expression *field_value = convert_expression(field_value_node);
                    
                    if (field_value != NULL) {
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
        
        /* Determine the record type from type_id if available */
        int var_type = UNKNOWN_TYPE;
        if (type_id != NULL) {
            var_type = map_type_name(type_id, NULL);
        }
        
        /* Create variable declaration for the record const */
        ListNode_t *var_ids = CreateListNode(id, LIST_STRING);
        Tree_t *var_decl = mk_vardecl(const_decl_node->line, var_ids, var_type,
            type_id, 0, 0, initializer, NULL, NULL, NULL);
        var_decl->tree_data.var_decl_data.is_typed_const = 1;
        
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

static long long compute_range_storage_size(const TypeInfo *info)
{
    if (info == NULL || !info->is_range || !info->range_known)
        return 0;

    long long start = info->range_start;
    long long end = info->range_end;
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
    ListNode_t *nested_type_sections = NULL;
    if (spec_node != NULL) {
        if (getenv("KGPC_DEBUG_TFPG") != NULL)
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
            if (target_name != NULL && getenv("KGPC_DEBUG_TFPG") != NULL) {
                fprintf(stderr, "[KGPC] convert_type_decl: class of target=%s for id=%s\n",
                    (target_name->sym && target_name->sym->name) ? target_name->sym->name : "<null>",
                    id);
            }
        }

        if (class_spec != NULL) {
            record_type = convert_class_type_ex(id, class_spec, &nested_type_sections);
            /* Process nested type sections - extract and convert nested type declarations */
            if (nested_type_sections != NULL && nested_type_decls_out != NULL) {
                ListNode_t *section_cursor = nested_type_sections;
                while (section_cursor != NULL) {
                    ast_t *type_section_ast = (ast_t *)section_cursor->cur;
                    if (type_section_ast != NULL) {
                        /* Recursively process this nested type section */
                        append_type_decls_from_section(type_section_ast, nested_type_decls_out,
                            NULL, NULL, NULL);
                        if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
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
                                NULL, NULL, NULL);
                            if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL)
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
            }
            if (getenv("KGPC_DEBUG_TFPG") != NULL)
                fprintf(stderr, "[KGPC] convert_type_decl after convert_type_spec id=%s mapped=%d type_id=%s record_type=%p type_info.record=%p\n",
                    id, mapped_type,
                    type_id != NULL ? type_id : "<null>",
                    (void *)record_type, (void *)type_info.record_type);
        }
    }

    KgpcType *kgpc_type = NULL;
    if (spec_node != NULL)
        kgpc_type = convert_type_spec_to_kgpctype(spec_node, NULL);

    /* If KgpcType wasn't created (e.g. for classes/records handled by legacy path), create it now */
    if (kgpc_type == NULL && record_type != NULL) {
        KgpcType *rec_type = create_record_type(record_type);
        if (record_type->is_class) {
            /* Classes are pointers to records */
            kgpc_type = create_pointer_type(rec_type);
        } else {
            kgpc_type = rec_type;
        }
    }

    Tree_t *decl = NULL;
    if (record_type != NULL) {
        /* Set the type ID for the record */
        if (record_type->type_id == NULL && id != NULL)
            record_type->type_id = strdup(id);

        if (record_type->is_type_helper && record_type->helper_base_type_id != NULL &&
            record_type->type_id != NULL)
        {
            if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
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
                if (method_ast != NULL && method_ast->typ == PASCAL_T_METHOD_DECL) {
                    struct MethodTemplate *template = create_method_template(method_ast);
                    if (template != NULL) {
                        register_class_method_ex(id, template->name,
                            template->is_virtual, template->is_override, template->is_static);
                        if (getenv("KGPC_DEBUG_CLASS_METHODS") != NULL)
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
                        register_class_method_ex(id, template->name,
                            template->is_virtual, template->is_override, template->is_static);
                        if (getenv("KGPC_DEBUG_CLASS_METHODS") != NULL)
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
        if (decl != NULL)
            decl->tree_data.type_decl_data.info.alias.is_shortstring =
                type_info.is_shortstring ||
                (id != NULL && pascal_identifier_equals(id, "ShortString"));
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
        decl->tree_data.type_decl_data.kgpc_type = kgpc_type;
        if (getenv("KGPC_DEBUG_TFPG") != NULL &&
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
        alias->is_pointer = type_info.is_pointer;
        alias->pointer_type = type_info.pointer_type;
        if (type_info.pointer_type_id != NULL) {
            alias->pointer_type_id = type_info.pointer_type_id;
            type_info.pointer_type_id = NULL;
        }
        alias->is_set = type_info.is_set;
        alias->set_element_type = type_info.set_element_type;
        if (type_info.set_element_type_id != NULL) {
            alias->set_element_type_id = type_info.set_element_type_id;
            type_info.set_element_type_id = NULL;
        }
        alias->is_enum_set = type_info.is_enum_set;
        if (type_info.inline_enum_values != NULL) {
            alias->inline_enum_values = type_info.inline_enum_values;
            type_info.inline_enum_values = NULL;
        }
        alias->is_enum = type_info.is_enum;
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
        if (type_info.is_record && type_info.record_type != NULL) {
            alias->inline_record_type = type_info.record_type;
            type_info.record_type = NULL;
            if (alias->inline_record_type->is_class) {
                KgpcType *inline_type = create_record_type(alias->inline_record_type);
                inline_type = create_pointer_type(inline_type);
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
    if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
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

    if (type_spec_node != NULL) {
        ast_t *spec_body = type_spec_node;
        if (spec_body->typ == PASCAL_T_TYPE_SPEC && spec_body->child != NULL)
            spec_body = spec_body->child;
        if (spec_body != NULL) {
            if (spec_body->typ == PASCAL_T_CLASS_TYPE)
            {
                if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
                    fprintf(stderr, "[KGPC] generic class decl %s\n", id);
                record_template = convert_class_type_ex(id, spec_body, NULL);
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
        decl->tree_data.type_decl_data.info.generic.original_ast = copy_ast(type_spec_node);
    decl->tree_data.type_decl_data.info.generic.record_template = record_template;

    /* Register the generic declaration for future specialization */
    generic_registry_add_decl(id, param_names, param_count, decl);

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
                if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                    fprintf(stderr, "[KGPC] convert_routine_body TYPE_SECTION at line=%d\n", node->line);
                }
                if (type_decl_list != NULL)
                    append_type_decls_from_section(node, type_decl_list, nested_subs,
                        const_decls, var_builder);
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
        append_const_decls_from_section(node, &helper_consts, var_builder, type_section);

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
    }
}

static void append_type_decls_from_section(ast_t *type_section, ListNode_t **dest,
    ListNode_t **subprograms, ListNode_t **const_decls, ListBuilder *var_builder) {
    if (type_section == NULL || dest == NULL)
        return;

    ListNode_t **tail = dest;
    while (*tail != NULL)
        tail = &(*tail)->next;

    if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
        fprintf(stderr, "[KGPC] append_type_decls_from_section: type_section->typ=%d child=%p line=%d\n",
                type_section->typ, (void*)type_section->child, type_section->line);

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

        if (getenv("KGPC_DEBUG_TFPG") != NULL && unwrapped != NULL) {
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
            if (getenv("KGPC_DEBUG_TFPG") != NULL) {
                char *type_name = dup_first_identifier_in_node(unwrapped);
                if (type_name != NULL) {
                    fprintf(stderr, "[KGPC] append_type_decls_from_section saw generic-like %s (tag=%d)\n",
                            type_name, unwrapped->typ);
                    free(type_name);
                }
            }
            Tree_t *decl = convert_generic_type_decl(unwrapped);
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
        } else if (unwrapped != NULL && unwrapped->typ == PASCAL_T_TYPE_DECL) {
            ListNode_t *nested_type_decls = NULL;
            if (const_decls != NULL && var_builder != NULL) {
                char *helper_id = dup_first_identifier_in_node(unwrapped);
                if (helper_id != NULL) {
                    append_helper_const_decls_from_type_decl(unwrapped, helper_id,
                        const_decls, var_builder, type_section);
                    free(helper_id);
                }
            }
            Tree_t *decl = convert_type_decl_ex(unwrapped, subprograms, &nested_type_decls);
            /* Insert nested type declarations first (they must be available before the container type) */
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
            /* Then insert the main type declaration */
            if (decl != NULL) {
                ListNode_t *node = CreateListNode(decl, LIST_TREE);
                *tail = node;
                tail = &node->next;
            }
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
        *out_value = -*out_value;
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
        return mk_inum(expr_node->line, strtoll(num_str, NULL, base));
    }
    case PASCAL_T_REAL:
        return mk_rnum(expr_node->line, strtof(expr_node->sym->name, NULL));
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
    case PASCAL_T_FUNC_CALL: {
        ast_t *child = expr_node->child;
        char *id = NULL;
        if (child != NULL) {
            if (child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child);
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child->child);
            }
            child = child->next;
        }
        ListNode_t *args = convert_expression_list(child);
        return mk_functioncall(expr_node->line, id, args);
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

    /*
     * cparser precedence bug workaround:
     * For write/writeln format specs like `write(x:Width-2)`, cparser can parse it as
     * `(x:Width) - 2` (a binary op with a formatted LHS), which then fails semantic
     * checking because `x:Width` is not a numeric expression.
     *
     * If we see any arithmetic binary op where the LHS carries a field width, treat the
     * RHS as part of the field-width expression instead.
     */
    if (left != NULL && left->field_width != NULL && right != NULL) {
        switch (type) {
        case PASCAL_T_ADD:
        case PASCAL_T_SUB:
        case PASCAL_T_OR:
            left->field_width = mk_addop(node->line, map_addop_tag(type), left->field_width, right);
            return left;
        case PASCAL_T_MUL:
        case PASCAL_T_DIV:
        case PASCAL_T_INTDIV:
        case PASCAL_T_MOD:
        case PASCAL_T_AND:
        case PASCAL_T_XOR:
        case PASCAL_T_SHL:
        case PASCAL_T_SHR:
        case PASCAL_T_ROL:
        case PASCAL_T_ROR:
            left->field_width = mk_mulop(node->line, map_mulop_tag(type), left->field_width, right);
            return left;
        default:
            break;
        }
    }

    switch (type) {
    case PASCAL_T_ADD:
    case PASCAL_T_SUB:
    case PASCAL_T_OR:
        return mk_addop(node->line, map_addop_tag(type), left, right);
    case PASCAL_T_MUL:
    case PASCAL_T_DIV:
    case PASCAL_T_INTDIV:
    case PASCAL_T_MOD:
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

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_expression: typ=%d line=%d\n", expr_node->typ, expr_node->line);
        if (expr_node->next != NULL) {
            fprintf(stderr, "[KGPC] convert_expression: has next sibling typ=%d\n", expr_node->next->typ);
        }
    }

    if (expr_node == NULL || expr_node == ast_nil)
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
        
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_expression IS: value_node=%p type_node=%p\n", value_node, type_node);
            if (type_node) fprintf(stderr, "[KGPC]   type_node typ=%d\n", type_node->typ);
        }
        
        if (type_node != NULL)
        {
            target_type = convert_type_spec(type_node, &target_type_id, &inline_record, &type_info);
            
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   convert_type_spec result: type=%d id=%s\n", target_type, target_type_id ? target_type_id : "<null>");
            }
            
            destroy_type_info_contents(&type_info);
            if (inline_record != NULL)
                destroy_record_type(inline_record);
        }
        result = mk_is(expr_node->line, value_expr, target_type, target_type_id);
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
        if (type_node != NULL)
        {
            target_type = convert_type_spec(type_node, &target_type_id, &inline_record, &type_info);
            destroy_type_info_contents(&type_info);
            if (inline_record != NULL)
                destroy_record_type(inline_record);
        }
        result = mk_as(expr_node->line, value_expr, target_type, target_type_id);
        return set_expr_source_index(result, original_node);
    }
    case PASCAL_T_NEG:
    case PASCAL_T_POS:
        result = convert_unary_expr(expr_node);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_NOT:
        result = mk_relop(expr_node->line, NOT, convert_expression(expr_node->child), NULL);
        return set_expr_source_index(result, original_node);
    case PASCAL_T_TUPLE:
    {
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
        ListNode_t *fields = NULL;
        ListNode_t *fields_tail = NULL;
        int field_count = 0;

        for (ast_t *field_assignment = expr_node->child;
             field_assignment != NULL;
             field_assignment = field_assignment->next)
        {
            if (field_assignment->typ != PASCAL_T_ASSIGNMENT)
                continue;

            ast_t *field_name_node = field_assignment->child;
            ast_t *field_value_node = (field_name_node != NULL) ? field_name_node->next : NULL;
            if (field_name_node == NULL || field_value_node == NULL ||
                field_name_node->sym == NULL || field_name_node->sym->name == NULL)
            {
                fprintf(stderr, "ERROR: Malformed record constructor field at line %d.\n",
                    expr_node->line);
                goto record_ctor_cleanup;
            }

            struct Expression *field_value = convert_expression(field_value_node);
            if (field_value == NULL)
            {
                fprintf(stderr, "ERROR: Failed to convert record constructor field value at line %d.\n",
                    expr_node->line);
                goto record_ctor_cleanup;
            }

            struct RecordConstructorField *field = (struct RecordConstructorField *)calloc(1, sizeof(struct RecordConstructorField));
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
    case PASCAL_T_FIELD_WIDTH:
        return convert_field_width_expr(expr_node);
    case PASCAL_T_TYPECAST:
    {
        ast_t *type_node = expr_node->child;
        ast_t *value_node = (type_node != NULL) ? type_node->next : NULL;

        int target_type = UNKNOWN_TYPE;
        char *target_type_id = NULL;
        struct RecordType *record_type = NULL;
        TypeInfo type_info;
        memset(&type_info, 0, sizeof(TypeInfo));

        ast_t *unwrapped_type = unwrap_pascal_node(type_node);
        if (unwrapped_type != NULL)
        {
            target_type = convert_type_spec(unwrapped_type, &target_type_id,
                &record_type, &type_info);
            destroy_type_info_contents(&type_info);
            if (record_type != NULL)
            {
                destroy_record_type(record_type);
                record_type = NULL;
            }

            if (target_type == UNKNOWN_TYPE && target_type_id == NULL &&
                unwrapped_type->typ == PASCAL_T_IDENTIFIER)
            {
                target_type_id = dup_symbol(unwrapped_type);
            }
        }

        struct Expression *inner_expr = convert_expression(value_node);
        return mk_typecast(expr_node->line, target_type, target_type_id, inner_expr);
    }
    case PASCAL_T_DEREF:
        return mk_pointer_deref(expr_node->line, convert_expression(expr_node->child));
    case PASCAL_T_ADDR:
        return mk_addressof(expr_node->line, convert_expression(expr_node->child));
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
        fprintf(stderr, "ERROR: unsupported expression tag %d (%s) at line %d.",
                expr_node->typ, name, expr_node->line);
        if (expr_node->sym != NULL && expr_node->sym->name != NULL)
            fprintf(stderr, " (symbol: %s)", expr_node->sym->name);
        fprintf(stderr, "\n");
        break;
    }
    }

    return NULL;
}

static ListNode_t *convert_expression_list(ast_t *arg_node) {
    ListBuilder builder;
    list_builder_init(&builder);
    
    /* Create visited set to detect circular references */
    VisitedSet *visited = visited_set_create();
    if (visited == NULL) {
        fprintf(stderr, "ERROR: Failed to allocate visited set for expression list traversal\n");
        return NULL;
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

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_member_access: base=%d field=%d args=%d\n",
            base_node ? base_node->typ : -1,
            field_node ? field_node->typ : -1,
            args_node ? args_node->typ : -1);
    }

    /* Check for function call (MEMBER_ACCESS with ARG_LIST as 3rd child) */
    if (args_node != NULL && args_node->typ == PASCAL_T_ARG_LIST) {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_member_access_chain: detected FUNC_CALL, converting to method call\n");
        }
        
        ast_t *method_id_node = unwrapped->child;
        ast_t *args_node = (method_id_node != NULL) ? method_id_node->next : NULL;
        
        if (method_id_node != NULL && method_id_node->typ == PASCAL_T_IDENTIFIER) {
            char *method_id = dup_symbol(method_id_node);
            
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
            
            return call_expr;
        }
    }

    /* Check if field_node has ARG_LIST as sibling (method call) */
    if (unwrapped != NULL && unwrapped->next != NULL && unwrapped->next->typ == PASCAL_T_ARG_LIST) {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
    struct Statement *stmt = mk_varassign(assign_node->line, assign_node->col, left, right);
    if (stmt != NULL) {
        stmt->source_index = assign_node->index;
    }
    return stmt;
}

static struct Statement *convert_proc_call(ast_t *call_node, bool implicit_identifier) {
    if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   Handling MEMBER_ACCESS\n");
        }
        ast_t *args_node = child->next;
        struct Statement *method_stmt = convert_method_call_statement(child, args_node);
        if (method_stmt != NULL) {
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   convert_method_call_statement returned statement\n");
            }
            return method_stmt;
        }
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
            } else if (child->child != NULL && child->child->typ == PASCAL_T_IDENTIFIER) {
                id = dup_symbol(child->child);
                args_start = child->next;
            }
        }
    }

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC]   id=%s\n", id ? id : "(null)");
    }

    ListNode_t *args = convert_expression_list(args_start);
    struct Statement *call = mk_procedurecall(call_node->line, id, args);
    if (call != NULL) {
        call->source_index = call_node->index;
        /* If id starts with __, it might be a method call placeholder from convert_method_call_statement */
        if (id != NULL && strncmp(id, "__", 2) == 0)
            call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
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
    
    free(method_name);

    if (proc_name == NULL)
        return NULL;

    struct Expression *object_expr = convert_expression(object_node);
    if (object_expr == NULL) {
        free(proc_name);
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
    if (call != NULL)
        call->stmt_data.procedure_call_data.is_method_call_placeholder = 1;
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
        if (getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_statement: stmt_node NULL after unwrap\n");
        return NULL;
    }

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
        return convert_statement(inner);
    }
    case PASCAL_T_FUNC_CALL:
        if (getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_statement: FUNC_CALL at line %d\n", stmt_node->line);
        return convert_proc_call(stmt_node, true);
    case PASCAL_T_MEMBER_ACCESS: {
        struct Statement *method_stmt = convert_method_call_statement(stmt_node, NULL);
        if (method_stmt != NULL)
            return method_stmt;
        return NULL;
    }
    case PASCAL_T_ASM_BLOCK: {
        char *code = collect_asm_text(stmt_node->child);
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
        
        char *exception_var_name = NULL;
        char *exception_type_name = NULL;

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
                        /* Extract exception variable and type from on clause */
                        /* Structure: on <var> [: <type>] do <stmt> */
                        ast_t *on_child = inner_unwrapped->child;
                        
                        /* First child should be the variable name */
                        if (on_child != NULL && on_child->typ == PASCAL_T_IDENTIFIER) {
                            if (exception_var_name == NULL && on_child->sym != NULL)
                                exception_var_name = strdup(on_child->sym->name);
                            on_child = on_child->next;
                        }
                        
                        /* Next might be a NONE node containing the type, or directly the type */
                        if (on_child != NULL) {
                            /* Unwrap if it's a NONE node */
                            ast_t *type_node = (on_child->typ == PASCAL_T_NONE && on_child->child != NULL) ? on_child->child : on_child;
                            
                            /* Look for the type identifier */
                            if (type_node->typ == PASCAL_T_IDENTIFIER) {
                                if (exception_type_name == NULL && type_node->sym != NULL)
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
                            if (inner_stmt != NULL)
                                list_builder_append(target, inner_stmt, LIST_STMT);
                        }
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
        return mk_tryexcept(stmt_node->line, try_stmts, except_stmts, exception_var_name, exception_type_name);
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
            } else if (child->typ == PASCAL_T_STATEMENT ||
                       child->typ == PASCAL_T_ASSIGNMENT ||
                               child->typ == PASCAL_T_FUNC_CALL ||
                               child->typ == PASCAL_T_BEGIN_BLOCK) {
                /* This is the statement for this branch */
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
        const char *name = tag_name(stmt_node->typ);
        fprintf(stderr, "ERROR: unsupported statement tag %d (%s) at line %d.",
                stmt_node->typ, name, stmt_node->line);
        if (stmt_node->sym != NULL && stmt_node->sym->name != NULL)
            fprintf(stderr, " (symbol: %s)", stmt_node->sym->name);
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
    
    if (getenv("KGPC_DEBUG_BODY") != NULL) {
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

        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_statement_list: processing stmt %d, typ=%d line=%d\n", 
                    stmt_count, unwrapped->typ, unwrapped->line);
            fprintf(stderr, "[KGPC]   calling convert_statement...\n");
            fflush(stderr);
        }

        struct Statement *stmt = convert_statement(unwrapped);
        
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   convert_statement returned: %p\n", (void*)stmt);
            fflush(stderr);
        }
        
        if (stmt != NULL) {
            list_builder_append(&builder, stmt, LIST_STMT);
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC]   -> converted successfully\n");
                fflush(stderr);
            }
        } else if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC]   -> convert_statement returned NULL, dropped statement typ=%d line=%d\n",
                unwrapped->typ, unwrapped->line);
            fflush(stderr);
        }
        cur = cur->next;
        stmt_count++;
    }

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] convert_statement_list: processed %d statements\n", stmt_count);
    }

    visited_set_destroy(visited);
    return list_builder_finish(&builder);
}

static struct Statement *convert_block(ast_t *block_node) {
    if (block_node == NULL)
    {
        if (getenv("KGPC_DEBUG_BODY") != NULL)
            fprintf(stderr, "[KGPC] convert_block: block_node is NULL\n");
        return NULL;
    }

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] convert_block: stmts is ast_nil, treating as NULL\n");
        }
        stmts = NULL;
    }
    ListNode_t *list = convert_statement_list(stmts);
    if (list == NULL && getenv("KGPC_DEBUG_BODY") != NULL)
        fprintf(stderr, "[KGPC] convert_block: statement list is NULL\n");
    return mk_compoundstatement(block_node->line, list);
}

static Tree_t *convert_method_impl(ast_t *method_node) {
    if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl entry (method_node=%p)\n", (void*)method_node);
    }

    if (method_node == NULL)
        return NULL;

    ast_t *cur = method_node->child;
    ast_t *qualified = unwrap_pascal_node(cur);

    if (getenv("KGPC_DEBUG_OPERATOR") != NULL) {
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
        /* Check if this is an operator symbol (encoded to op_xxx means it's an operator) */
        char *encoded_op = (potential_op != NULL) ? encode_operator_name(potential_op) : NULL;
        int is_operator_symbol = (encoded_op != NULL && strcmp(potential_op, encoded_op) != 0);
        free(potential_op);

        if (is_operator_symbol) {
            /* This is a standalone operator - qualified is the operator symbol directly */
            if (getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                fprintf(stderr, "[Operator] Detected operator symbol: encoded=%s\n", encoded_op);
            }
            /* Get the param list to determine the type.
             * The next node could be PASCAL_T_PARAM_LIST or directly a PASCAL_T_PARAM */
            ast_t *param_node = qualified->next;
            if (getenv("KGPC_DEBUG_OPERATOR") != NULL) {
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

                        /* Build mangled name: TypeName__op_suffix */
                        size_t name_len = strlen(param_type_id) + strlen(encoded_op) + 3;
                        char *mangled_name = (char *)malloc(name_len);
                        if (mangled_name != NULL) {
                            snprintf(mangled_name, name_len, "%s__%s", param_type_id, encoded_op);

                            if (getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                                fprintf(stderr, "[Operator] Standalone operator: %s\n", mangled_name);
                            }

                            /* Find return type by scanning siblings of param_node */
                            ast_t *return_type_node = NULL;
                            ast_t *scan = param_node->next;
                            while (scan != NULL) {
                                if (scan->typ == PASCAL_T_RETURN_TYPE) {
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

                            if (return_type_node != NULL) {
                                TypeInfo type_info;
                                return_type = convert_type_spec(return_type_node->child, &return_type_id, NULL, &type_info);
                                if (return_type_id == NULL && return_type_node->sym != NULL && return_type_node->sym->name != NULL) {
                                    return_type_id = strdup(return_type_node->sym->name);
                                }
                            }

                            /* Find the body */
                            struct Statement *body = NULL;
                            ast_t *body_cursor = return_type_node ? return_type_node->next : param_node->next;
                            while (body_cursor != NULL) {
                                if (body_cursor->typ == PASCAL_T_BEGIN_BLOCK) {
                                    body = convert_block(body_cursor);
                                    break;
                                } else if (body_cursor->typ == PASCAL_T_FUNCTION_BODY) {
                                    ListNode_t *ignored_const = NULL;
                                    ListBuilder ignored_var; list_builder_init(&ignored_var);
                                    ListBuilder ignored_label; list_builder_init(&ignored_label);
                                    ListNode_t *ignored_subs = NULL;
                                    ListNode_t *ignored_types = NULL;
                                    convert_routine_body(body_cursor, &ignored_const, &ignored_var, &ignored_label,
                                        &ignored_subs, &body, &ignored_types);
                                    break;
                                }
                                body_cursor = body_cursor->next;
                            }

                            /* Create the function tree */
                            Tree_t *tree = mk_function(method_node->line, mangled_name, params, NULL,
                                NULL, NULL, NULL, NULL, body, return_type, return_type_id, inline_return_type, 0, 0);

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
        if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
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
    
    // Skip over the optional type parameter list (PASCAL_T_TYPE_PARAM_LIST) if present
    // The structure is: class_node [type_params] . method_id_node
    ast_t *cursor = class_node->next;
    if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class_node->typ=%d\n", class_node->typ);
        if (class_node->sym && class_node->sym->name)
            fprintf(stderr, "[KGPC]   class_node->name=%s\n", class_node->sym->name);
        if (cursor) {
            fprintf(stderr, "[KGPC]   cursor->typ=%d\n", cursor->typ);
            if (cursor->sym && cursor->sym->name)
                fprintf(stderr, "[KGPC]   cursor->name=%s\n", cursor->sym->name);
        }
    }
    
    // Skip type parameter list if present
    if (cursor != NULL && cursor->typ == PASCAL_T_TYPE_PARAM_LIST) {
        cursor = cursor->next;
        if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && cursor) {
            fprintf(stderr, "[KGPC]   after type_params cursor->typ=%d\n", cursor->typ);
            if (cursor->sym && cursor->sym->name)
                fprintf(stderr, "[KGPC]   after type_params cursor->name=%s\n", cursor->sym->name);
        }
    }
    
    // Skip the dot token if present (it may be wrapped)
    while (cursor != NULL && cursor->typ != PASCAL_T_IDENTIFIER) {
        cursor = cursor->next;
        if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && cursor) {
            fprintf(stderr, "[KGPC]   skipping non-identifier cursor->typ=%d\n", cursor->typ);
        }
    }
    
    ast_t *method_id_node = cursor;
    if (method_id_node == NULL)
        return NULL;

    char *class_name = dup_symbol(class_node);
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
    
    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
        fprintf(stderr, "[FromCParser] convert_method_impl: class_name=%s method_name=%s effective_class=%s\n",
            class_name ? class_name : "<null>",
            method_name ? method_name : "<null>",
            effective_class ? effective_class : "<null>");
    }
    
    /* Don't re-register the method here - it was already registered during class declaration */
    
    /* Check if this method was declared as static in the record/class declaration */
    int is_static_method = is_method_static(effective_class, method_name);
    if (method_node != NULL && method_name != NULL)
    {
        struct MethodTemplate impl_template = {0};
        impl_template.name = (char *)method_name;
        annotate_method_template(&impl_template, method_node);
        if (impl_template.is_static || impl_template.is_class_method)
            is_static_method = 1;
    }
    if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class=%s method=%s is_static=%d\n",
                effective_class ? effective_class : "<null>", 
                method_name ? method_name : "<null>",
                is_static_method);
    }
    
    char *proc_name = mangle_method_name(effective_class, method_name);
    if (proc_name == NULL) {
        free(class_name);
        free(method_name);
        return NULL;
    }
    
    /* Check if this is a class operator (static method) by checking the method name */
    int is_class_operator = 0;
    if (method_name != NULL) {
        /* Class operators have operator symbols as names */
        if (strcmp(method_name, "+") == 0 || strcmp(method_name, "-") == 0 ||
            strcmp(method_name, "*") == 0 || strcmp(method_name, "/") == 0 ||
            strcmp(method_name, "=") == 0 || strcmp(method_name, "<>") == 0 ||
            strcmp(method_name, "<") == 0 || strcmp(method_name, ">") == 0 ||
            strcmp(method_name, "<=") == 0 || strcmp(method_name, ">=") == 0 ||
            strcmp(method_name, "**") == 0 ||
            strcasecmp(method_name, "div") == 0 || strcasecmp(method_name, "mod") == 0 ||
            strcasecmp(method_name, "and") == 0 || strcasecmp(method_name, "or") == 0 ||
            strcasecmp(method_name, "not") == 0 || strcasecmp(method_name, "xor") == 0 ||
            strcasecmp(method_name, "shl") == 0 || strcasecmp(method_name, "shr") == 0 ||
            strcasecmp(method_name, "in") == 0 || strcasecmp(method_name, "is") == 0 ||
            strcasecmp(method_name, "as") == 0 || strcmp(method_name, ":=") == 0) {
            is_class_operator = 1;
        }
    }
    int is_constructor = (method_name != NULL && strcasecmp(method_name, "create") == 0);

    ListBuilder params_builder;
    list_builder_init(&params_builder);
    ListNode_t *const_decls = NULL;
    ListBuilder var_builder;
    list_builder_init(&var_builder);
    ListBuilder label_builder;
    list_builder_init(&label_builder);
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;
    
    /* Return type information for methods that are functions (like class operators) */
    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;
    struct TypeAlias *inline_return_type = NULL;
    int has_return_type = 0;

    const char *helper_base = (effective_class != NULL) ? lookup_type_helper_base(effective_class) : NULL;
    int is_helper_method = (helper_base != NULL);
    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && effective_class != NULL)
        fprintf(stderr, "[KGPC] convert_method_impl: looking up helper base for %s -> %s\n",
            effective_class, helper_base ? helper_base : "(not found)");

    /* Add Self parameter only for instance methods, not for class operators or static methods */
    if (!is_class_operator && !is_static_method) {
        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
        char *self_type_id = NULL;
        int self_type_tag = UNKNOWN_TYPE;
        struct TypeAlias *self_type_alias = NULL;
        if (effective_class != NULL) {
            if (is_helper_method) {
                self_type_id = strdup(helper_base);
                /* Resolve the type tag for type helper Self parameters.
                 * This is important for correct calling convention (e.g., Double should use xmm0). */
                self_type_tag = map_type_name(helper_base, NULL);
                /* Create type alias with storage size for Single (4) vs Double (8).
                 * This is needed for correct SSE register operations (movss vs movsd). */
                if (self_type_tag == REAL_TYPE) {
                    self_type_alias = (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
                    if (self_type_alias != NULL) {
                        self_type_alias->base_type = REAL_TYPE;
                        /* Determine storage size from type name */
                        if (pascal_identifier_equals(helper_base, "Single"))
                            self_type_alias->storage_size = 4;
                        else
                            self_type_alias->storage_size = 8;  /* Double or other real types */
                    }
                }
            } else {
                self_type_id = strdup(effective_class);
            }
        }
        Tree_t *self_param = mk_vardecl(method_node->line, self_ids, self_type_tag,
            self_type_id, is_helper_method ? 0 : 1, 0, NULL, NULL, self_type_alias, NULL);
        list_builder_append(&params_builder, self_param, LIST_TREE);
    }

    cur = qualified->next;
    while (cur != NULL) {
        ast_t *node = unwrap_pascal_node(cur);
        if (node == NULL)
            node = cur;

        switch (node->typ) {
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
            TypeInfo type_info;
            return_type = convert_type_spec(node->child, &return_type_id, NULL, &type_info);

            if (return_type_id == NULL && node->sym != NULL && node->sym->name != NULL)
            {
                return_type_id = strdup(node->sym->name);
            }
            
            /* If it's a complex type (array, pointer, etc.), create a TypeAlias to store the info */
            if (type_info.is_array || type_info.is_pointer || type_info.is_set || 
                type_info.is_enum || type_info.is_file || type_info.is_record) {
                inline_return_type = (struct TypeAlias *)malloc(sizeof(struct TypeAlias));
                if (inline_return_type != NULL) {
                    memset(inline_return_type, 0, sizeof(struct TypeAlias));
                    inline_return_type->base_type = return_type;
                    inline_return_type->target_type_id = return_type_id;
                    
                    if (type_info.is_array) {
                        inline_return_type->is_array = 1;
                        inline_return_type->array_start = type_info.start;
                        inline_return_type->array_end = type_info.end;
                        inline_return_type->array_element_type = type_info.element_type;
                        inline_return_type->array_element_type_id = type_info.element_type_id;
                        inline_return_type->is_shortstring = type_info.is_shortstring;
                        inline_return_type->is_open_array = type_info.is_open_array;
                    }
                    
                    if (type_info.is_pointer) {
                        inline_return_type->is_pointer = 1;
                        inline_return_type->pointer_type = type_info.pointer_type;
                        inline_return_type->pointer_type_id = type_info.pointer_type_id;
                    }
                    
                    if (type_info.is_set) {
                        inline_return_type->is_set = 1;
                        inline_return_type->set_element_type = type_info.set_element_type;
                        inline_return_type->set_element_type_id = type_info.set_element_type_id;
                    }
                    
                    if (type_info.is_enum) {
                        inline_return_type->is_enum = 1;
                        inline_return_type->enum_literals = type_info.enum_literals;
                    }
                    
                    if (type_info.is_file) {
                        inline_return_type->is_file = 1;
                        inline_return_type->file_type = type_info.file_type;
                        inline_return_type->file_type_id = type_info.file_type_id;
                    }
                }
            }
            break;
        }
        case PASCAL_T_TYPE_SECTION:
            if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                fprintf(stderr, "[KGPC] convert_function TYPE_SECTION at line=%d\n", node->line);
            }
            append_type_decls_from_section(node, &type_decls, &nested_subs,
                &const_decls, &var_builder);
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
    if (tree != NULL) {
        tree->tree_data.subprogram_data.mangled_id = strdup(proc_name);
    }

    record_generic_method_impl(effective_class, method_name, method_node);
    
    /* Check if this method belongs to a generic type. If so, we've recorded the AST
     * template above and should NOT generate a concrete implementation. Return NULL
     * to prevent adding this to the subprograms list. */
    GenericTypeDecl *generic_decl = generic_registry_find_decl(effective_class);
    if (generic_decl != NULL && generic_decl->record_template != NULL) {
        if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && effective_class != NULL && method_name != NULL) {
            fprintf(stderr, "[KGPC] convert_method_impl: recorded template for %s.%s, not generating concrete impl\n", 
                    effective_class, method_name);
        }
        if (cleaned_class_name != NULL)
            free(cleaned_class_name);
        free(class_name);
        free(method_name);
        return NULL;
    }
    
    if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL && effective_class != NULL && method_name != NULL) {
        fprintf(stderr, "[KGPC] convert_method_impl: class=%s method=%s\n", effective_class, method_name);
    }

    if (cleaned_class_name != NULL)
        free(cleaned_class_name);
    free(class_name);
    free(method_name);
    return tree;
}

static Tree_t *convert_procedure(ast_t *proc_node) {
    ast_t *cur = proc_node->child;
    char *id = NULL;
    static int debug_external_nodes = -1;
    if (debug_external_nodes == -1)
        debug_external_nodes = (getenv("KGPC_DEBUG_EXTERNAL") != NULL);

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    if (cur != NULL)
        cur = cur->next;

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
    char *external_alias = NULL;
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;

    while (cur != NULL) {
        switch (cur->typ) {
        case PASCAL_T_TYPE_SECTION:
            append_type_decls_from_section(cur, &type_decls, &nested_subs,
                &const_decls, &var_decls_builder);
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
            if (cur->child != NULL && cur->child->typ == PASCAL_T_IDENTIFIER) {
                char *directive = dup_symbol(cur->child);
                if (directive != NULL) {
                    if (strcasecmp(directive, "external") == 0) {
                        is_external = 1;
                    }
                }
                free(directive);
            }
            break;
        }
        case PASCAL_T_EXTERNAL_NAME:
            if (cur->child != NULL && cur->child->typ == PASCAL_T_STRING) {
                if (external_alias != NULL)
                    free(external_alias);
                external_alias = dup_symbol(cur->child);
            }
            break;
        default:
            break;
        }
        cur = cur->next;
    }

    ListNode_t *label_decls = list_builder_finish(&label_decls_builder);
    Tree_t *tree = mk_procedure(proc_node->line, id, params, const_decls,
                                label_decls, type_decls, list_builder_finish(&var_decls_builder),
                                nested_subs, body, is_external, 0);
    if (tree != NULL && external_alias != NULL)
        tree->tree_data.subprogram_data.cname_override = external_alias;
    else if (external_alias != NULL)
        free(external_alias);
    return tree;
}

static Tree_t *convert_function(ast_t *func_node) {
    ast_t *cur = func_node->child;
    char *id = NULL;
    char *operator_symbol = NULL;  /* For standalone operators */
    int is_standalone_operator = 0;
    static int debug_external_nodes = -1;
    if (debug_external_nodes == -1)
        debug_external_nodes = (getenv("KGPC_DEBUG_EXTERNAL") != NULL);

    if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER)
        id = dup_symbol(cur);

    /* Check if this is a standalone operator declaration */
    if (id != NULL && strcasecmp(id, "operator") == 0) {
        is_standalone_operator = 1;
        /* Get the operator symbol from the next child */
        if (cur != NULL) {
            cur = cur->next;
            if (cur != NULL && cur->typ == PASCAL_T_IDENTIFIER) {
                operator_symbol = dup_symbol(cur);
            }
        }
    }

    if (cur != NULL) {
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

    /* For standalone operators, derive the mangled name from operator symbol and first param type */
    if (is_standalone_operator && operator_symbol != NULL && params != NULL) {
        /* Get the type of the first parameter (params is a list of Tree_t* with TREE_VAR_DECL) */
        Tree_t *first_param = (Tree_t *)params->cur;
        if (getenv("KGPC_DEBUG_OPERATOR") != NULL) {
            fprintf(stderr, "[Operator] is_standalone_operator=%d operator_symbol=%s params=%p first_param=%p\n",
                is_standalone_operator, operator_symbol, (void*)params, (void*)first_param);
            if (first_param != NULL) {
                fprintf(stderr, "[Operator] first_param->type=%d type_id=%s\n",
                    first_param->type,
                    first_param->tree_data.var_decl_data.type_id ?
                    first_param->tree_data.var_decl_data.type_id : "(null)");
            }
        }
        if (first_param != NULL && first_param->type == TREE_VAR_DECL &&
            first_param->tree_data.var_decl_data.type_id != NULL) {
            char *encoded_op = encode_operator_name(operator_symbol);
            if (encoded_op != NULL) {
                /* Build mangled name: TypeName__op_suffix */
                const char *param_type_id = first_param->tree_data.var_decl_data.type_id;
                size_t name_len = strlen(param_type_id) + strlen(encoded_op) + 3;
                char *mangled_name = (char *)malloc(name_len);
                if (mangled_name != NULL) {
                    snprintf(mangled_name, name_len, "%s__%s", param_type_id, encoded_op);
                    if (getenv("KGPC_DEBUG_OPERATOR") != NULL) {
                        fprintf(stderr, "[Operator] mangled name: %s\n", mangled_name);
                    }
                    free(id);
                    id = mangled_name;
                }
                free(encoded_op);
            }
        }
        free(operator_symbol);
        operator_symbol = NULL;
    } else if (operator_symbol != NULL) {
        free(operator_symbol);
    }

    char *return_type_id = NULL;
    int return_type = UNKNOWN_TYPE;
    struct TypeAlias *inline_return_type = NULL;

    if (cur != NULL && cur->typ == PASCAL_T_RETURN_TYPE) {
        TypeInfo type_info;
        return_type = convert_type_spec(cur->child, &return_type_id, NULL, &type_info);

        if (return_type_id == NULL && cur->sym != NULL && cur->sym->name != NULL)
        {
            return_type_id = strdup(cur->sym->name);
        }
        
        /* If it's a complex type (array, pointer, etc.), create a TypeAlias to store the info */
        if (type_info.is_array || type_info.is_pointer || type_info.is_set || 
            type_info.is_enum || type_info.is_file || type_info.is_record) {
            inline_return_type = (struct TypeAlias *)malloc(sizeof(struct TypeAlias));
            if (inline_return_type != NULL) {
                memset(inline_return_type, 0, sizeof(struct TypeAlias));
                inline_return_type->base_type = return_type;
                inline_return_type->target_type_id = return_type_id;
                
                if (type_info.is_array) {
                    inline_return_type->is_array = 1;
                    inline_return_type->array_start = type_info.start;
                    inline_return_type->array_end = type_info.end;
                    inline_return_type->array_element_type = type_info.element_type;
                    inline_return_type->array_element_type_id = type_info.element_type_id;
                    inline_return_type->is_shortstring = type_info.is_shortstring;
                    inline_return_type->is_open_array = type_info.is_open_array;
                }
                
                if (type_info.is_pointer) {
                    inline_return_type->is_pointer = 1;
                    inline_return_type->pointer_type = type_info.pointer_type;
                    inline_return_type->pointer_type_id = type_info.pointer_type_id;
                }
                
                if (type_info.is_set) {
                    inline_return_type->is_set = 1;
                    inline_return_type->set_element_type = type_info.set_element_type;
                    inline_return_type->set_element_type_id = type_info.set_element_type_id;
                }
                
                if (type_info.is_enum) {
                    inline_return_type->is_enum = 1;
                    inline_return_type->enum_literals = type_info.enum_literals;
                }
                
                if (type_info.is_file) {
                    inline_return_type->is_file = 1;
                    inline_return_type->file_type = type_info.file_type;
                    inline_return_type->file_type_id = type_info.file_type_id;
                }
            }
        }
        
        cur = cur->next;
    }

    ListNode_t *const_decls = NULL;
    ListBuilder var_decls_builder;
    list_builder_init(&var_decls_builder);
    ListBuilder label_decls_builder;
    list_builder_init(&label_decls_builder);
    ListNode_t *nested_subs = NULL;
    struct Statement *body = NULL;
    int is_external = 0;
    char *external_alias = NULL;
    ast_t *type_section_ast = NULL;  /* Track local type section for enum resolution */
    ListNode_t *type_decls = NULL;

    while (cur != NULL) {

        switch (cur->typ) {
        case PASCAL_T_TYPE_SECTION:
            append_type_decls_from_section(cur, &type_decls, &nested_subs,
                &const_decls, &var_decls_builder);
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
                if (strcasecmp(self_sym, "external") == 0) {
                    is_external = 1;
                }
                free(self_sym);
            }

            if (cur->child != NULL && cur->child->typ == PASCAL_T_IDENTIFIER) {
                char *directive = dup_symbol(cur->child);
                if (directive != NULL) {
                    if (strcasecmp(directive, "external") == 0) {
                        is_external = 1;
                    }
                }
                free(directive);
            }
            break;
        }
        case PASCAL_T_EXTERNAL_NAME:
            if (cur->child != NULL && cur->child->typ == PASCAL_T_STRING) {
                if (external_alias != NULL)
                    free(external_alias);
                external_alias = dup_symbol(cur->child);
            }
            break;
        default:
            break;
        }
        cur = cur->next;
    }

    ListNode_t *label_decls = list_builder_finish(&label_decls_builder);
    Tree_t *tree = mk_function(func_node->line, id, params, const_decls,
                                label_decls, type_decls, list_builder_finish(&var_decls_builder), nested_subs, body,
                                return_type, return_type_id, inline_return_type, is_external, 0);
    if (tree != NULL && external_alias != NULL)
        tree->tree_data.subprogram_data.cname_override = external_alias;
    else if (external_alias != NULL)
        free(external_alias);
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
    
    if (getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] find_node_by_type: visiting node typ=%d, looking for typ=%d\n", 
                node->typ, target_type);
    }
    
    if (node->typ == target_type) {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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

Tree_t *tree_from_pascal_ast(ast_t *program_ast) {
    Tree_t *final_tree = NULL;
    if (program_ast == NULL)
        return NULL;

    reset_const_sections();

    ast_t *cur = program_ast;
    if (cur->typ == PASCAL_T_NONE)
        cur = cur->child;

    if (cur == NULL) {
        fprintf(stderr, "ERROR: Empty Pascal AST.\n");
        return NULL;
    }

    if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
            if (getenv("KGPC_DEBUG_PROGRAM_SECTIONS") != NULL) {
                fprintf(stderr, "[kgpc program] section typ=%d (%s) line=%d\n",
                    section->typ, pascal_tag_to_string(section->typ), section->line);
            }
            /* Check for circular reference before processing */
            if (!is_safe_to_continue(visited, section)) {
                fprintf(stderr, "ERROR: Circular reference detected in program sections, stopping traversal\n");
                break;
            }
            
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
                    &const_decls, &var_decls_builder);
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
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Skipping declaration component type %d (should be child of section)\n", section->typ);
                }
                break;
            default:
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Main block not found in sibling chain, searching recursively\n");
            }
            
            /* Search for VAR_SECTION */
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Searching for VAR_SECTION (typ=%d)\n", PASCAL_T_VAR_SECTION);
            }
            ast_t* var_section_node = find_node_by_type(cur->child, PASCAL_T_VAR_SECTION);
            if (var_section_node != NULL) {
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found VAR_SECTION via recursive search\n");
                }
                list_builder_extend(&var_decls_builder, convert_var_section(var_section_node));
            }
            
            /* Search for MAIN_BLOCK */
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Searching for MAIN_BLOCK (typ=%d)\n", PASCAL_T_MAIN_BLOCK);
            }
            ast_t* main_block_node = find_last_node_by_type(cur->child, PASCAL_T_MAIN_BLOCK);
            if (main_block_node == NULL) {
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found, searching for BEGIN_BLOCK (typ=%d)\n", PASCAL_T_BEGIN_BLOCK);
                }
                main_block_node = find_last_node_by_type(cur->child, PASCAL_T_BEGIN_BLOCK);
            }
            /* Also try typ=112 which appears in the AST */
            if (main_block_node == NULL) {
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: BEGIN_BLOCK not found, trying typ=112\n");
                }
                main_block_node = find_last_node_by_type(cur->child, 112);
            }
            if (main_block_node != NULL) {
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found MAIN_BLOCK via recursive search (typ=%d)\n", main_block_node->typ);
                }
                body = convert_block(main_block_node);
            } else {
                /* MAIN_BLOCK not found directly. Check if there's a typ=100 node that contains it */
                if (getenv("KGPC_DEBUG_BODY") != NULL) {
                    fprintf(stderr, "[KGPC] tree_from_pascal_ast: MAIN_BLOCK not found directly, searching for typ=100 wrapper\n");
                }
                ast_t* wrapper_node = find_last_node_by_type(cur->child, 100);
                if (wrapper_node != NULL && wrapper_node->child != NULL) {
                    if (getenv("KGPC_DEBUG_BODY") != NULL) {
                        fprintf(stderr, "[KGPC] tree_from_pascal_ast: Found typ=100 wrapper, checking its children\n");
                    }
                    /* Check if the wrapper's child is a MAIN_BLOCK or BEGIN_BLOCK */
                    ast_t* child = wrapper_node->child;
                    int child_count = 0;
                    while (child != NULL) {
                        if (getenv("KGPC_DEBUG_BODY") != NULL) {
                            fprintf(stderr, "[KGPC] tree_from_pascal_ast: typ=100 child[%d] has typ=%d\n", child_count, child->typ);
                        }
                        if (child->typ == PASCAL_T_MAIN_BLOCK || child->typ == PASCAL_T_BEGIN_BLOCK) {
                            if (getenv("KGPC_DEBUG_BODY") != NULL) {
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
                
                if (body == NULL && getenv("KGPC_DEBUG_BODY") != NULL) {
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
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: No main block found, creating empty body\\n");
            }
            body = mk_compoundstatement(cur->line, NULL);
        }
        
        Tree_t *tree = mk_program(cur->line, program_id, args, uses, label_decls, const_decls,
                                  list_builder_finish(&var_decls_builder), type_decls, subprograms, body);
        final_tree = tree;
        return final_tree;
    }


    if (cur->typ == PASCAL_T_UNIT_DECL) {
        ast_t *unit_name_node = cur->child;
        char *unit_id = unit_name_node != NULL ? dup_symbol(unit_name_node) : strdup("unit");
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
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting section type %d\n", section->typ);
            }
            /* Check for circular reference */
            if (!is_safe_to_continue(visited_unit, section)) {
                fprintf(stderr, "ERROR: Circular reference detected in unit sections, stopping traversal\n");
                break;
            }
            
            if (section->typ == PASCAL_T_INTERFACE_SECTION) {
                interface_node = section;
                if (getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] interface at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
                implementation_node = section;
                if (getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] implementation at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_INITIALIZATION_SECTION) {
                initialization_node = section;
                if (getenv("KGPC_DEBUG_UNIT_SECTIONS") != NULL) {
                    fprintf(stderr, "[UNIT_SECTIONS] initialization at line %d\n", section->line);
                }
            } else if (section->typ == PASCAL_T_FINALIZATION_SECTION) {
                finalization_node = section;
            }
            section = section->next;
        }
        
        visited_set_destroy(visited_unit);

        if (interface_node != NULL && interface_node->typ == PASCAL_T_INTERFACE_SECTION) {
            /* Create visited set for interface sections */
            VisitedSet *visited_if = visited_set_create();
            if (visited_if == NULL) {
                fprintf(stderr, "ERROR: Failed to allocate visited set for interface sections\n");
            } else {
                ast_t *section = interface_node->child;
                while (section != NULL) {
            if (getenv("KGPC_DEBUG_BODY") != NULL) {
                fprintf(stderr, "[KGPC] tree_from_pascal_ast: Visiting PROGRAM section type %d\n", section->typ);
            }
            /* Check for circular reference */
                    if (!is_safe_to_continue(visited_if, section)) {
                        fprintf(stderr, "ERROR: Circular reference detected in interface sections, stopping traversal\n");
                        break;
                    }
                    
                    ast_t *node = unwrap_pascal_node(section);
                    for (ast_t *node_cursor = node; node_cursor != NULL;
                         node_cursor = (section->typ == PASCAL_T_NONE) ? node_cursor->next : NULL) {
                        if (getenv("KGPC_DEBUG_PROPERTY") != NULL) {
                            fprintf(stderr, "[KGPC] interface node typ=%d (%s)\n",
                                node_cursor->typ, pascal_tag_to_string(node_cursor->typ));
                        }
                        switch (node_cursor->typ) {
                        case PASCAL_T_USES_SECTION:
                            append_uses_from_section(node_cursor, &interface_uses);
                            break;
                        case PASCAL_T_TYPE_SECTION:
                            if (getenv("KGPC_DEBUG_TYPE_SECTION") != NULL) {
                                fprintf(stderr, "[KGPC] interface TYPE_SECTION at line=%d\n", node_cursor->line);
                            }
                            interface_type_section_ast = node_cursor;  /* Save for const array enum resolution */
                            append_type_decls_from_section(node_cursor, &interface_type_decls,
                                NULL, &interface_const_decls, &interface_var_builder);
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
            if (getenv("KGPC_DEBUG_IMPL_SECTION") != NULL) {
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
                while (definition != NULL) {
                    /* Check for circular reference */
                    if (!is_safe_to_continue(visited_impl, definition)) {
                        fprintf(stderr, "ERROR: Circular reference detected in implementation sections, stopping traversal\n");
                        break;
                    }

                    if (getenv("KGPC_DEBUG_IMPL_NONE") != NULL &&
                        definition->typ == PASCAL_T_NONE &&
                        definition->child == NULL &&
                        definition->sym != NULL &&
                        definition->sym->name != NULL) {
                        fprintf(stderr, "[KGPC] impl NONE at line=%d: %.120s\n",
                            definition->line, definition->sym->name);
                    }
                    ast_t *node = unwrap_pascal_node(definition);
                    for (ast_t *node_cursor = node; node_cursor != NULL;
                         node_cursor = (definition->typ == PASCAL_T_NONE) ? node_cursor->next : NULL) {
                        if (getenv("KGPC_DEBUG_GENERIC_METHODS") != NULL) {
                            fprintf(stderr, "[KGPC] implementation section node typ=%d\n", node_cursor->typ);
                        }
                        switch (node_cursor->typ) {
                        case PASCAL_T_USES_SECTION:
                            append_uses_from_section(node_cursor, &implementation_uses);
                            break;
                        case PASCAL_T_TYPE_SECTION:
                            implementation_type_section_ast = node_cursor;  /* Save for const array enum resolution */
                            append_type_decls_from_section(node_cursor, &implementation_type_decls,
                                &subprograms, &implementation_const_decls, &implementation_var_builder);
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
                            if (kgpc_debug_subprog_enabled() || getenv("KGPC_DEBUG_IMPL_PROCS") != NULL) {
                                char *proc_id = (node_cursor->child != NULL) ? dup_symbol(node_cursor->child) : strdup("?");
                                fprintf(stderr, "[KGPC] impl convert_procedure(%s) line=%d => %p\n", proc_id, node_cursor->line, (void*)proc);
                                free(proc_id);
                            }
                            append_subprogram_node(&subprograms, proc);
                            break;
                        }
                        case PASCAL_T_FUNCTION_DECL: {
                            Tree_t *func = convert_function(node_cursor);
                            if (kgpc_debug_subprog_enabled() || getenv("KGPC_DEBUG_IMPL_PROCS") != NULL) {
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
            if (getenv("KGPC_DEBUG_UNIT_INIT") != NULL) {
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
