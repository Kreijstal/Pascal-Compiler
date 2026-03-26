/*
    Reachability analysis for subprograms
    Marks which functions are actually used starting from the main program
*/

#include "mark_used.h"
#include "Parser/ParseTree/tree_types.h"
#include "Parser/SemanticCheck/HashTable/HashTable.h"
#include "Parser/ParseTree/KgpcType.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>
#include "identifier_utils.h"
#include "compilation_context.h"

/* Hash map to map mangled_id -> Tree_t* (subprogram) with O(1) lookup */
#define SUBPROG_MAP_BUCKETS 211

typedef struct SubprogramEntry {
    char *canonical_id;
    Tree_t *subprogram;
    struct SubprogramEntry *next;
} SubprogramEntry;

typedef struct {
    SubprogramEntry *buckets[SUBPROG_MAP_BUCKETS];
    /* Secondary index: keyed by lowered unmangled id for fallback lookups */
    SubprogramEntry *id_buckets[SUBPROG_MAP_BUCKETS];
} SubprogramMap;

static unsigned subprog_hash(const char *s);

static void map_add_secondary(SubprogramEntry **buckets, const char *key, Tree_t *subprogram)
{
    if (key == NULL || subprogram == NULL)
        return;

    char *lower_key = pascal_identifier_lower_dup(key);
    if (lower_key == NULL)
        return;

    unsigned idx = subprog_hash(lower_key);
    SubprogramEntry *entry = malloc(sizeof(SubprogramEntry));
    if (entry == NULL) {
        free(lower_key);
        return;
    }
    entry->canonical_id = lower_key;
    entry->subprogram = subprogram;
    entry->next = buckets[idx];
    buckets[idx] = entry;
}

static unsigned subprog_hash(const char *s) {
    unsigned h = 0;
    for (; *s; s++)
        h = h * 31 + (unsigned char)*s;
    return h % SUBPROG_MAP_BUCKETS;
}

static void map_init(SubprogramMap *map) {
    memset(map, 0, sizeof(*map));
}

static void map_destroy(SubprogramMap *map) {
    if (map == NULL)
        return;
    for (int i = 0; i < SUBPROG_MAP_BUCKETS; i++) {
        SubprogramEntry *e = map->buckets[i];
        while (e != NULL) {
            SubprogramEntry *next = e->next;
            free(e->canonical_id);
            free(e);
            e = next;
        }
        map->buckets[i] = NULL;

        e = map->id_buckets[i];
        while (e != NULL) {
            SubprogramEntry *next = e->next;
            free(e->canonical_id);
            free(e);
            e = next;
        }
        map->id_buckets[i] = NULL;
    }
}

static void map_add(SubprogramMap *map, const char *mangled_id, Tree_t *subprogram) {
    if (mangled_id == NULL || subprogram == NULL) return;
    char *canonical_id = pascal_identifier_lower_dup(mangled_id);
    if (canonical_id == NULL)
        return;

    /* Primary index: by canonical (lowered) mangled_id */
    unsigned idx = subprog_hash(canonical_id);
    SubprogramEntry *entry = malloc(sizeof(SubprogramEntry));
    if (entry == NULL) { free(canonical_id); return; }
    entry->canonical_id = canonical_id;
    entry->subprogram = subprogram;
    entry->next = map->buckets[idx];
    map->buckets[idx] = entry;

    /* Secondary index: by lowered unmangled id */
    const char *id = subprogram->tree_data.subprogram_data.id;
    if (id != NULL)
        map_add_secondary(map->id_buckets, id, subprogram);

    /* Methods are frequently referenced later by bare method_name
     * (e.g. forward decl/body reconciliation or @SetStatus), so index
     * them there too. */
    const char *method_name = subprogram->tree_data.subprogram_data.method_name;
    if (method_name != NULL && (id == NULL || strcasecmp(method_name, id) != 0))
        map_add_secondary(map->id_buckets, method_name, subprogram);
}

static Tree_t* map_find(SubprogramMap *map, const char *mangled_id) {
    if (mangled_id == NULL) return NULL;
    char *lookup_id = pascal_identifier_lower_dup(mangled_id);
    if (lookup_id == NULL)
        return NULL;

    /* Primary lookup: by canonical mangled_id */
    unsigned idx = subprog_hash(lookup_id);
    Tree_t *fallback = NULL;
    for (SubprogramEntry *e = map->buckets[idx]; e != NULL; e = e->next) {
        if (strcmp(e->canonical_id, lookup_id) == 0) {
            Tree_t *sub = e->subprogram;
            if (sub != NULL && sub->tree_data.subprogram_data.statement_list != NULL) {
                free(lookup_id);
                return sub;
            }
            if (fallback == NULL)
                fallback = sub;
        }
    }

    /* Fallback: try searching by unmangled id */
    unsigned id_idx = subprog_hash(lookup_id);
    Tree_t *id_fallback = NULL;
    for (SubprogramEntry *e = map->id_buckets[id_idx]; e != NULL; e = e->next) {
        if (strcmp(e->canonical_id, lookup_id) == 0) {
            Tree_t *sub = e->subprogram;
            if (sub != NULL && sub->tree_data.subprogram_data.statement_list != NULL) {
                free(lookup_id);
                return sub;
            }
            if (id_fallback == NULL)
                id_fallback = sub;
        }
    }

    free(lookup_id);
    if (fallback != NULL)
        return fallback;
    return id_fallback;
}

/* Forward declarations */
static void mark_expr_calls(struct Expression *expr, SubprogramMap *map);
static void mark_stmt_calls(struct Statement *stmt, SubprogramMap *map);
static void mark_vmt_methods_used(Tree_t *program, SubprogramMap *map);
static void mark_class_constructors_from_types(ListNode_t *type_list, SubprogramMap *map);
static void mark_class_constructors_from_subprograms(ListNode_t *sub_list, SubprogramMap *map);
static void mark_subprograms_by_id(SubprogramMap *map, const char *id);
static void mark_class_methods_by_owner(ListNode_t *sub_list, const char *owner_class,
    const char *method_name, SubprogramMap *map);
static void mark_subprogram_recursive(Tree_t *sub, SubprogramMap *map);
static int lookup_id_parse_owner_method(const char *lookup_id, char *owner_buf,
    size_t owner_buf_sz, char *method_buf, size_t method_buf_sz);
static void scan_used_subprogram_bodies(ListNode_t *sub_list, SubprogramMap *map);

static Tree_t *find_record_type_decl(ListNode_t *type_list, const char *type_id)
{
    if (type_list == NULL || type_id == NULL)
        return NULL;

    for (ListNode_t *node = type_list; node != NULL; node = node->next) {
        if (node->type != LIST_TREE || node->cur == NULL)
            continue;
        Tree_t *tree = (Tree_t *)node->cur;
        if (tree->type != TREE_TYPE_DECL || tree->tree_data.type_decl_data.id == NULL)
            continue;
        if (strcasecmp(tree->tree_data.type_decl_data.id, type_id) != 0)
            continue;
        return tree;
    }

    return NULL;
}

static const char *find_interface_delegate_target_name(
    const struct RecordType *record, const char *iface_name, const char *method_name)
{
    if (record == NULL || iface_name == NULL || method_name == NULL ||
        record->method_templates == NULL)
        return NULL;

    for (ListNode_t *node = record->method_templates; node != NULL; node = node->next) {
        if (node->type != LIST_METHOD_TEMPLATE || node->cur == NULL)
            continue;
        struct MethodTemplate *tmpl = (struct MethodTemplate *)node->cur;
        if (!tmpl->is_interface_delegation ||
            tmpl->delegated_interface_name == NULL ||
            tmpl->name == NULL ||
            tmpl->delegated_target_name == NULL)
            continue;
        if (strcasecmp(tmpl->delegated_interface_name, iface_name) == 0 &&
            strcasecmp(tmpl->name, method_name) == 0)
            return tmpl->delegated_target_name;
    }

    return NULL;
}

static void mark_interface_dispatch_target(ListNode_t *type_list,
    const char *class_id, const struct RecordType *record,
    const char *iface_name, const char *method_name, SubprogramMap *map)
{
    const char *cur_class = class_id;
    const struct RecordType *cur_record = record;

    while (cur_class != NULL && cur_record != NULL) {
        const char *lookup_name = method_name;
        const char *delegate_name = find_interface_delegate_target_name(
            cur_record, iface_name, method_name);
        if (delegate_name != NULL)
            lookup_name = delegate_name;

        char impl_id[512];
        snprintf(impl_id, sizeof(impl_id), "%s__%s", cur_class, lookup_name);
        Tree_t *sub = map_find(map, impl_id);
        if (sub != NULL) {
            mark_subprogram_recursive(sub, map);
            mark_subprograms_by_id(map, impl_id);
            return;
        }

        if (cur_record->parent_class_name == NULL)
            break;
        Tree_t *parent_tree = find_record_type_decl(type_list, cur_record->parent_class_name);
        if (parent_tree == NULL ||
            parent_tree->type != TREE_TYPE_DECL ||
            parent_tree->tree_data.type_decl_data.kind != TYPE_DECL_RECORD ||
            parent_tree->tree_data.type_decl_data.info.record == NULL)
            break;
        cur_class = parent_tree->tree_data.type_decl_data.id;
        cur_record = parent_tree->tree_data.type_decl_data.info.record;
    }
}

/* Mark a subprogram and recursively mark all functions it calls */
static void mark_subprogram_recursive(Tree_t *sub, SubprogramMap *map) {
    if (sub == NULL || sub->type != TREE_SUBPROGRAM) return;

    /* Already marked? */
    if (sub->tree_data.subprogram_data.is_used) return;

    /* Mark as used */
    sub->tree_data.subprogram_data.is_used = 1;

    /* If this node has no body, try to find the implementation by plain id.
     * This handles the case where a forward declaration (mangled_id="runerror_i")
     * and implementation (mangled_id="FPC_RUNERROR") have different mangled names
     * but the same Pascal identifier.  Mark ALL overloads with the same name —
     * map_find alone picks only one and may pick the wrong overload. */
    struct Statement *body = sub->tree_data.subprogram_data.statement_list;
    if (body == NULL) {
        const char *plain_id = sub->tree_data.subprogram_data.id;
        if (plain_id != NULL) {
            mark_subprograms_by_id(map, plain_id);
        }
        /* If this forward decl has a cname_override (e.g., [external name 'FPC_FINALIZE']),
         * also mark the implementation that shares the same alias. */
        const char *cname = sub->tree_data.subprogram_data.cname_override;
        if (cname != NULL) {
            Tree_t *impl = map_find(map, cname);
            if (impl != NULL && impl != sub)
                mark_subprogram_recursive(impl, map);
        }
        return;
    }

    /* Traverse the body to find calls */
    mark_stmt_calls(body, map);
}

static void mark_related_subprogram_overloads(Tree_t *sub, SubprogramMap *map)
{
    if (sub == NULL || sub->type != TREE_SUBPROGRAM || map == NULL)
        return;

    const char *plain_id = sub->tree_data.subprogram_data.id;
    if (plain_id != NULL)
        mark_subprograms_by_id(map, plain_id);
}

/* Helper to check if a pointer looks valid (not garbage from uninitialized union members) */
static int is_valid_pointer(void *ptr) {
    uintptr_t addr = (uintptr_t)ptr;
    /* NULL is valid */
    if (addr == 0) return 1;
    /* On x86-64 Linux, valid heap/stack pointers are typically in these ranges */
    /* This filters out garbage like 0x3b2720646e756f66 (ASCII text) */
    if (addr < 0x1000 || addr > 0x7fffffffffff) return 0;
    return 1;
}

static void mark_expr_calls(struct Expression *expr, SubprogramMap *map) {
    if (expr == NULL) return;
    
    if (!is_valid_pointer(expr)) {
        return;
    }

    /* Early return for leaf expression types that don't contain function calls */
    /* We need to be careful accessing expr->type if expr is garbage but passes is_valid_pointer */
    /* But we can't easily check validity without OS-specific calls (IsBadReadPtr etc) */
    
    switch (expr->type) {
        case EXPR_VAR_ID:
        case EXPR_INUM:
        case EXPR_RNUM:
        case EXPR_STRING:
        case EXPR_CHAR_CODE:
        case EXPR_BOOL:
        case EXPR_NIL:
        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
            return;
        default:
            break;
    }
    
    switch (expr->type) {
        case EXPR_FUNCTION_CALL: {
            const char *lookup_id = expr->expr_data.function_call_data.mangled_id;
            if (lookup_id == NULL)
                lookup_id = expr->expr_data.function_call_data.id;
            Tree_t *called_sub = NULL;
            if (getenv("KGPC_TRACE_TFPLISTENUM") != NULL &&
                expr->expr_data.function_call_data.is_constructor_call) {
                struct Expression *recv = expr->expr_data.function_call_data.constructor_receiver_expr;
                fprintf(stderr,
                    "[mark_used] ctor call lookup=%s id=%s owner=%s method=%s recv_type=%d recv_id=%s procdef=%p\n",
                    lookup_id != NULL ? lookup_id : "(null)",
                    expr->expr_data.function_call_data.id != NULL ? expr->expr_data.function_call_data.id : "(null)",
                    expr->expr_data.function_call_data.cached_owner_class != NULL ? expr->expr_data.function_call_data.cached_owner_class : "(null)",
                    expr->expr_data.function_call_data.cached_method_name != NULL ? expr->expr_data.function_call_data.cached_method_name : "(null)",
                    recv != NULL ? recv->type : -1,
                    (recv != NULL && recv->type == EXPR_VAR_ID && recv->expr_data.id != NULL) ? recv->expr_data.id : "(n/a)",
                    (void *)(expr->expr_data.function_call_data.call_kgpc_type != NULL ? expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition : NULL));
            }
            if (lookup_id != NULL) {
                int trace_missing_calls = getenv("KGPC_TRACE_MISSING_CALLS") != NULL &&
                    (strcasecmp(lookup_id, "format_us_a") == 0 ||
                     strcasecmp(lookup_id, "format_s_a") == 0 ||
                     strcasecmp(lookup_id, "codepagenametocodepage_s") == 0 ||
                     strcasecmp(lookup_id, "stringofchar_c_li") == 0 ||
                     strcasecmp(lookup_id, "stringofchar_c_i64") == 0);
                if (trace_missing_calls)
                {
                    fprintf(stderr,
                        "[mark_used] funccall lookup=%s id=%s owner=%s method=%s procdef=%p\n",
                        lookup_id,
                        expr->expr_data.function_call_data.id != NULL ? expr->expr_data.function_call_data.id : "(null)",
                        expr->expr_data.function_call_data.cached_owner_class != NULL ? expr->expr_data.function_call_data.cached_owner_class : "(null)",
                        expr->expr_data.function_call_data.cached_method_name != NULL ? expr->expr_data.function_call_data.cached_method_name : "(null)",
                        (void *)(expr->expr_data.function_call_data.call_kgpc_type != NULL ? expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition : NULL));
                }
                called_sub = map_find(map, lookup_id);
                if (called_sub != NULL) {
                    mark_subprogram_recursive(called_sub, map);
                    mark_related_subprogram_overloads(called_sub, map);
                }
                else
                {
                    char owner_buf[256];
                    char method_buf[256];
                    if (lookup_id_parse_owner_method(lookup_id, owner_buf, sizeof(owner_buf),
                            method_buf, sizeof(method_buf)))
                        mark_class_methods_by_owner(NULL, owner_buf, method_buf, map);
                }
            }
            if (called_sub == NULL) {
                const char *owner = expr->expr_data.function_call_data.cached_owner_class;
                const char *method = expr->expr_data.function_call_data.cached_method_name;
                if (owner != NULL && method != NULL)
                    mark_class_methods_by_owner(NULL, owner, method, map);
            }
            if (called_sub == NULL &&
                expr->expr_data.function_call_data.is_constructor_call &&
                expr->expr_data.function_call_data.constructor_receiver_expr != NULL) {
                struct Expression *recv = expr->expr_data.function_call_data.constructor_receiver_expr;
                if (recv->type == EXPR_VAR_ID && recv->expr_data.id != NULL) {
                    const char *method = expr->expr_data.function_call_data.cached_method_name;
                    if (method == NULL)
                        method = expr->expr_data.function_call_data.id;
                    if (method == NULL)
                        method = "create";
                    mark_class_methods_by_owner(NULL, recv->expr_data.id, method, map);
                }
            }
            if (called_sub == NULL &&
                expr->expr_data.function_call_data.call_kgpc_type != NULL &&
                expr->expr_data.function_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE) {
                called_sub = expr->expr_data.function_call_data.call_kgpc_type->info.proc_info.definition;
                if (called_sub != NULL) {
                    mark_subprogram_recursive(called_sub, map);
                    mark_related_subprogram_overloads(called_sub, map);
                }
            }
            /* Also check constructor receiver and procedural var expressions */
            if (is_valid_pointer(expr->expr_data.function_call_data.constructor_receiver_expr) &&
                    expr->expr_data.function_call_data.constructor_receiver_expr != NULL)
                mark_expr_calls(expr->expr_data.function_call_data.constructor_receiver_expr, map);
            if (is_valid_pointer(expr->expr_data.function_call_data.procedural_var_expr) &&
                    expr->expr_data.function_call_data.procedural_var_expr != NULL)
                mark_expr_calls(expr->expr_data.function_call_data.procedural_var_expr, map);
            /* Also check arguments */
            ListNode_t *args = expr->expr_data.function_call_data.args_expr;
            while (args != NULL) {
                if (args->type == LIST_EXPR && args->cur != NULL) {
                    mark_expr_calls((struct Expression*)args->cur, map);
                }
                args = args->next;
            }
            if (expr->expr_data.function_call_data.constructor_receiver_expr != NULL)
                mark_expr_calls(expr->expr_data.function_call_data.constructor_receiver_expr, map);
            if (expr->expr_data.function_call_data.procedural_var_expr != NULL)
                mark_expr_calls(expr->expr_data.function_call_data.procedural_var_expr, map);
            break;
        }
        
        case EXPR_RELOP:
            if (is_valid_pointer(expr->expr_data.relop_data.left) && 
                expr->expr_data.relop_data.left != NULL)
                mark_expr_calls(expr->expr_data.relop_data.left, map);
            if (is_valid_pointer(expr->expr_data.relop_data.right) && 
                expr->expr_data.relop_data.right != NULL)
                mark_expr_calls(expr->expr_data.relop_data.right, map);
            break;
            
        case EXPR_SIGN_TERM:
            if (is_valid_pointer(expr->expr_data.sign_term) && expr->expr_data.sign_term != NULL)
                mark_expr_calls(expr->expr_data.sign_term, map);
            break;
            
        case EXPR_ADDOP:
            if (expr->expr_data.addop_data.left_expr != NULL)
                mark_expr_calls(expr->expr_data.addop_data.left_expr, map);
            if (expr->expr_data.addop_data.right_term != NULL)
                mark_expr_calls(expr->expr_data.addop_data.right_term, map);
            break;
            
        case EXPR_MULOP:
            if (is_valid_pointer(expr->expr_data.mulop_data.left_term) && expr->expr_data.mulop_data.left_term != NULL)
                mark_expr_calls(expr->expr_data.mulop_data.left_term, map);
            if (is_valid_pointer(expr->expr_data.mulop_data.right_factor) && expr->expr_data.mulop_data.right_factor != NULL)
                mark_expr_calls(expr->expr_data.mulop_data.right_factor, map);
            break;
            
        case EXPR_ARRAY_ACCESS:
            if (is_valid_pointer(expr->expr_data.array_access_data.array_expr) && expr->expr_data.array_access_data.array_expr != NULL)
                mark_expr_calls(expr->expr_data.array_access_data.array_expr, map);
            if (is_valid_pointer(expr->expr_data.array_access_data.index_expr) && expr->expr_data.array_access_data.index_expr != NULL)
                mark_expr_calls(expr->expr_data.array_access_data.index_expr, map);
            break;
            
        case EXPR_RECORD_ACCESS:
            if (is_valid_pointer(expr->expr_data.record_access_data.record_expr) && 
                expr->expr_data.record_access_data.record_expr != NULL)
                mark_expr_calls(expr->expr_data.record_access_data.record_expr, map);
            break;
            
        case EXPR_POINTER_DEREF:
            if (is_valid_pointer(expr->expr_data.pointer_deref_data.pointer_expr) && 
                expr->expr_data.pointer_deref_data.pointer_expr != NULL)
                mark_expr_calls(expr->expr_data.pointer_deref_data.pointer_expr, map);
            break;
            
        case EXPR_ADDR:
            if (is_valid_pointer(expr->expr_data.addr_data.expr) &&
                expr->expr_data.addr_data.expr != NULL)
            {
                struct Expression *addr_inner = expr->expr_data.addr_data.expr;
                /* When semcheck doesn't convert @MethodName to EXPR_ADDR_OF_PROC
                 * (e.g. unit method bodies not fully semchecked), the expression
                 * stays as EXPR_ADDR(EXPR_VAR_ID("MethodName")).  Try to find the
                 * named procedure/method in the subprogram map and mark it used. */
                const char *addr_id = NULL;
                if (addr_inner->type == EXPR_VAR_ID)
                    addr_id = addr_inner->expr_data.id;
                else if (addr_inner->type == EXPR_FUNCTION_CALL &&
                         addr_inner->expr_data.function_call_data.args_expr == NULL)
                    addr_id = addr_inner->expr_data.function_call_data.id;
                if (addr_id != NULL)
                {
                    Tree_t *addr_sub = map_find(map, addr_id);
                    if (addr_sub != NULL)
                        mark_subprogram_recursive(addr_sub, map);
                    /* Also mark all overloads with the same bare name in case
                     * multiple classes define methods with this name (e.g.
                     * TSimpleStatusThread.SetStatus and
                     * TSimpleStatusProcThread.SetStatus). */
                    mark_subprograms_by_id(map, addr_id);
                }
                mark_expr_calls(addr_inner, map);
            }
            break;
            
        case EXPR_TYPECAST:
            if (is_valid_pointer(expr->expr_data.typecast_data.expr) && 
                expr->expr_data.typecast_data.expr != NULL)
                mark_expr_calls(expr->expr_data.typecast_data.expr, map);
            break;
            
        case EXPR_IS:
            if (is_valid_pointer(expr->expr_data.is_data.expr) && 
                expr->expr_data.is_data.expr != NULL)
                mark_expr_calls(expr->expr_data.is_data.expr, map);
            break;
            
        case EXPR_AS:
            if (is_valid_pointer(expr->expr_data.as_data.expr) && 
                expr->expr_data.as_data.expr != NULL)
                mark_expr_calls(expr->expr_data.as_data.expr, map);
            break;
            
        case EXPR_ADDR_OF_PROC: {
            const char *mangled_id = expr->expr_data.addr_of_proc_data.proc_mangled_id;
            if (mangled_id == NULL)
                mangled_id = expr->expr_data.addr_of_proc_data.proc_id;
            assert(mangled_id != NULL && "EXPR_ADDR_OF_PROC must have proc_mangled_id or proc_id set");
            Tree_t *called_sub = map_find(map, mangled_id);
            if (called_sub != NULL) {
                mark_subprogram_recursive(called_sub, map);
            }
            else
            {
                char owner_buf[256];
                char method_buf[256];
                if (lookup_id_parse_owner_method(mangled_id, owner_buf, sizeof(owner_buf),
                        method_buf, sizeof(method_buf)))
                    mark_class_methods_by_owner(NULL, owner_buf, method_buf, map);
            }
            break;
        }
            
        case EXPR_SET:
            if (expr->expr_data.set_data.elements != NULL) {
                ListNode_t *elem = expr->expr_data.set_data.elements;
                while (elem != NULL) {
                    if (elem->cur != NULL) {
                        struct SetElement *set_elem = (struct SetElement*)elem->cur;
                        mark_expr_calls(set_elem->lower, map);
                        mark_expr_calls(set_elem->upper, map);
                    }
                    elem = elem->next;
                }
            }
            break;
            
        case EXPR_ARRAY_LITERAL:
            if (expr->expr_data.array_literal_data.elements != NULL) {
                ListNode_t *elem = expr->expr_data.array_literal_data.elements;
                while (elem != NULL) {
                    if (elem->type == LIST_EXPR && elem->cur != NULL) {
                        mark_expr_calls((struct Expression*)elem->cur, map);
                    }
                    elem = elem->next;
                }
            }
            break;
            
        case EXPR_RECORD_CONSTRUCTOR:
            if (expr->expr_data.record_constructor_data.fields != NULL) {
                ListNode_t *field = expr->expr_data.record_constructor_data.fields;
                while (field != NULL) {
                    struct RecordConstructorField *entry =
                        (struct RecordConstructorField *)field->cur;
                    if (entry != NULL && entry->value != NULL)
                        mark_expr_calls(entry->value, map);
                    field = field->next;
                }
            }
            break;
            
        default:
            /* Leaf nodes - nothing to traverse */
            break;
    }
}

static void mark_stmt_calls(struct Statement *stmt, SubprogramMap *map) {
    if (stmt == NULL) return;
    
    
    switch (stmt->type) {
        case STMT_PROCEDURE_CALL: {
            const char *lookup_id = stmt->stmt_data.procedure_call_data.mangled_id;
            if (lookup_id == NULL)
                lookup_id = stmt->stmt_data.procedure_call_data.id;
            Tree_t *called_sub = NULL;
            if (lookup_id != NULL) {
                called_sub = map_find(map, lookup_id);
                if (called_sub != NULL) {
                    mark_subprogram_recursive(called_sub, map);
                    mark_related_subprogram_overloads(called_sub, map);
                }
                else
                {
                    char owner_buf[256];
                    char method_buf[256];
                    if (lookup_id_parse_owner_method(lookup_id, owner_buf, sizeof(owner_buf),
                            method_buf, sizeof(method_buf)))
                        mark_class_methods_by_owner(NULL, owner_buf, method_buf, map);
                }
            }
            if (called_sub == NULL) {
                const char *owner = stmt->stmt_data.procedure_call_data.cached_owner_class;
                const char *method = stmt->stmt_data.procedure_call_data.cached_method_name;
                if (owner != NULL && method != NULL)
                    mark_class_methods_by_owner(NULL, owner, method, map);
            }
            if (called_sub == NULL &&
                stmt->stmt_data.procedure_call_data.call_kgpc_type != NULL &&
                stmt->stmt_data.procedure_call_data.call_kgpc_type->kind == TYPE_KIND_PROCEDURE) {
                called_sub = stmt->stmt_data.procedure_call_data.call_kgpc_type->info.proc_info.definition;
                if (called_sub != NULL) {
                    mark_subprogram_recursive(called_sub, map);
                    mark_related_subprogram_overloads(called_sub, map);
                }
            }
            /* Also check arguments */
            ListNode_t *args = stmt->stmt_data.procedure_call_data.expr_args;
            while (args != NULL) {
                if (args->type == LIST_EXPR && args->cur != NULL) {
                    mark_expr_calls((struct Expression*)args->cur, map);
                }
                args = args->next;
            }
            break;
        }
        
        case STMT_VAR_ASSIGN:
            mark_expr_calls(stmt->stmt_data.var_assign_data.var, map);
            mark_expr_calls(stmt->stmt_data.var_assign_data.expr, map);
            break;

        case STMT_EXPR:
            mark_expr_calls(stmt->stmt_data.expr_stmt_data.expr, map);
            break;
            
        case STMT_COMPOUND_STATEMENT: {
            ListNode_t *stmts = stmt->stmt_data.compound_statement;
            while (stmts != NULL) {
                if (stmts->type == LIST_STMT && stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)stmts->cur, map);
                }
                stmts = stmts->next;
            }
            break;
        }
        
        case STMT_IF_THEN:
            mark_expr_calls(stmt->stmt_data.if_then_data.relop_expr, map);
            mark_stmt_calls(stmt->stmt_data.if_then_data.if_stmt, map);
            mark_stmt_calls(stmt->stmt_data.if_then_data.else_stmt, map);
            break;
            
        case STMT_WHILE:
            mark_expr_calls(stmt->stmt_data.while_data.relop_expr, map);
            mark_stmt_calls(stmt->stmt_data.while_data.while_stmt, map);
            break;
            
        case STMT_REPEAT: {
            ListNode_t *body = stmt->stmt_data.repeat_data.body_list;
            while (body != NULL) {
                if (body->type == LIST_STMT && body->cur != NULL) {
                    mark_stmt_calls((struct Statement*)body->cur, map);
                }
                body = body->next;
            }
            mark_expr_calls(stmt->stmt_data.repeat_data.until_expr, map);
            break;
        }
        
        case STMT_FOR:
        case STMT_FOR_VAR:
        case STMT_FOR_ASSIGN_VAR:

            if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_ASSIGN_VAR) {
                if (stmt->stmt_data.for_data.for_assign_data.var_assign != NULL) {
                    mark_stmt_calls(stmt->stmt_data.for_data.for_assign_data.var_assign, map);
                }
            } else if (stmt->stmt_data.for_data.for_assign_type == STMT_FOR_VAR) {

                if (stmt->stmt_data.for_data.for_assign_data.var != NULL)
                    mark_expr_calls(stmt->stmt_data.for_data.for_assign_data.var, map);
            }

            if (stmt->stmt_data.for_data.to != NULL) {

                mark_expr_calls(stmt->stmt_data.for_data.to, map);
            }
            if (stmt->stmt_data.for_data.do_for != NULL)
                mark_stmt_calls(stmt->stmt_data.for_data.do_for, map);
            break;
            
        case STMT_FOR_IN:
            mark_expr_calls(stmt->stmt_data.for_in_data.loop_var, map);
            mark_expr_calls(stmt->stmt_data.for_in_data.collection, map);
            mark_stmt_calls(stmt->stmt_data.for_in_data.do_stmt, map);
            break;
            
        case STMT_CASE: {
            mark_expr_calls(stmt->stmt_data.case_data.selector_expr, map);
            ListNode_t *branches = stmt->stmt_data.case_data.branches;
            while (branches != NULL) {
                if (branches->cur != NULL) {
                    struct CaseBranch *branch = (struct CaseBranch*)branches->cur;
                    /* Check label expressions */
                    ListNode_t *labels = branch->labels;
                    while (labels != NULL) {
                        if (labels->type == LIST_EXPR && labels->cur != NULL) {
                            mark_expr_calls((struct Expression*)labels->cur, map);
                        }
                        labels = labels->next;
                    }
                    mark_stmt_calls(branch->stmt, map);
                }
                branches = branches->next;
            }
            mark_stmt_calls(stmt->stmt_data.case_data.else_stmt, map);
            break;
        }
        
        case STMT_WITH:
            mark_expr_calls(stmt->stmt_data.with_data.context_expr, map);
            mark_stmt_calls(stmt->stmt_data.with_data.body_stmt, map);
            break;
            
        case STMT_TRY_FINALLY: {
            ListNode_t *try_stmts = stmt->stmt_data.try_finally_data.try_statements;
            while (try_stmts != NULL) {
                if (try_stmts->type == LIST_STMT && try_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)try_stmts->cur, map);
                }
                try_stmts = try_stmts->next;
            }
            ListNode_t *finally_stmts = stmt->stmt_data.try_finally_data.finally_statements;
            while (finally_stmts != NULL) {
                if (finally_stmts->type == LIST_STMT && finally_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)finally_stmts->cur, map);
                }
                finally_stmts = finally_stmts->next;
            }
            break;
        }
        
        case STMT_TRY_EXCEPT: {
            ListNode_t *try_stmts = stmt->stmt_data.try_except_data.try_statements;
            while (try_stmts != NULL) {
                if (try_stmts->type == LIST_STMT && try_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)try_stmts->cur, map);
                }
                try_stmts = try_stmts->next;
            }
            ListNode_t *except_stmts = stmt->stmt_data.try_except_data.except_statements;
            while (except_stmts != NULL) {
                if (except_stmts->type == LIST_STMT && except_stmts->cur != NULL) {
                    mark_stmt_calls((struct Statement*)except_stmts->cur, map);
                }
                except_stmts = except_stmts->next;
            }
            break;
        }
        
        case STMT_RAISE:
            mark_expr_calls(stmt->stmt_data.raise_data.exception_expr, map);
            break;
            
        case STMT_INHERITED:
            mark_expr_calls(stmt->stmt_data.inherited_data.call_expr, map);
            break;

        case STMT_EXIT:
            if (stmt->stmt_data.exit_data.return_expr != NULL)
                mark_expr_calls(stmt->stmt_data.exit_data.return_expr, map);
            break;

        case STMT_LABEL:
            mark_stmt_calls(stmt->stmt_data.label_data.stmt, map);
            break;

        case STMT_ASM_BLOCK: {
            /* Scan inline asm text for call/jmp/leaq/movq targets and mark
               referenced subprograms as used so DCE doesn't eliminate them. */
            const char *asm_code = stmt->stmt_data.asm_block_data.code;
            if (asm_code != NULL) {
                const char *p = asm_code;
                while (*p != '\0') {
                    /* Skip leading whitespace */
                    while (*p != '\0' && isspace((unsigned char)*p)) p++;
                    if (*p == '\0') break;

                    /* Read mnemonic */
                    const char *mnem_start = p;
                    while (*p != '\0' && !isspace((unsigned char)*p) && *p != '\n') p++;
                    size_t mnem_len = (size_t)(p - mnem_start);

                    int is_ref_insn = 0;
                    if ((mnem_len == 4 && strncmp(mnem_start, "call", 4) == 0) ||
                        (mnem_len == 5 && strncmp(mnem_start, "callq", 5) == 0) ||
                        (mnem_len == 3 && strncmp(mnem_start, "jmp", 3) == 0) ||
                        (mnem_len == 4 && strncmp(mnem_start, "jmpq", 4) == 0) ||
                        (mnem_len == 4 && strncmp(mnem_start, "leaq", 4) == 0) ||
                        (mnem_len == 4 && strncmp(mnem_start, "movq", 4) == 0) ||
                        (mnem_len == 3 && strncmp(mnem_start, "lea", 3) == 0) ||
                        (mnem_len == 3 && strncmp(mnem_start, "mov", 3) == 0)) {
                        is_ref_insn = 1;
                    }

                    if (is_ref_insn) {
                        /* Skip whitespace after mnemonic */
                        while (*p != '\0' && *p != '\n' && isspace((unsigned char)*p)) p++;
                        /* Extract operand: a symbol name before (, or end of line/comma */
                        const char *sym_start = p;
                        while (*p != '\0' && *p != '\n' && *p != '(' && *p != ',' &&
                               *p != ' ' && *p != '\t') p++;
                        size_t sym_len = (size_t)(p - sym_start);
                        if (sym_len > 0 && sym_len < 256) {
                            char sym_buf[256];
                            memcpy(sym_buf, sym_start, sym_len);
                            sym_buf[sym_len] = '\0';
                            /* Strip leading $ or * prefix */
                            const char *sym = sym_buf;
                            if (*sym == '$' || *sym == '*') sym++;
                            /* Skip purely numeric operands and register refs */
                            if (*sym != '\0' && *sym != '%' && *sym != '-' &&
                                !(*sym >= '0' && *sym <= '9')) {
                                Tree_t *found = map_find(map, sym);
                                if (found != NULL) {
                                    mark_subprogram_recursive(found, map);
                                }
                            }
                        }
                    }
                    /* Advance to next line */
                    while (*p != '\0' && *p != '\n') p++;
                    if (*p == '\n') p++;
                }
            }
            break;
        }

        default:
            /* Other statements don't have expressions to check */
            break;
    }
}

/* Build map of all subprograms (recursive for nested subprograms) */
static void build_subprogram_map(ListNode_t *sub_list, SubprogramMap *map) {
    while (sub_list != NULL) {
        if (sub_list->type == LIST_TREE && sub_list->cur != NULL) {
            Tree_t *sub = (Tree_t*)sub_list->cur;
            if (sub->type == TREE_SUBPROGRAM) {
                /* Don't reset is_used if already marked (e.g., by mark_program_subs_used) */
                
                const char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
                const char *map_id = (mangled_id != NULL) ? mangled_id : sub->tree_data.subprogram_data.id;
                if (map_id != NULL) {
                    /* Check if this mangled_id already exists (forward declaration case) */
                    Tree_t *existing = map_find(map, map_id);
                    if (existing != NULL) {
                        /* We have both forward declaration and implementation */
                        /* We need to ensure both get marked as used */
                        /* Strategy: mark the forward declaration NOW if the implementation gets marked */
                        /* We'll use a simple approach: when we mark one, we mark the other */
                        
                        if (sub->tree_data.subprogram_data.statement_list != NULL) {
                            /* This is the implementation, replace in map */
                            /* But first, mark the forward declaration to match this one's status */
                            existing->tree_data.subprogram_data.is_used = sub->tree_data.subprogram_data.is_used;
                            map_add(map, map_id, sub);
                        } else {
                            /* This is another forward declaration, keep the existing one */
                            sub->tree_data.subprogram_data.is_used = existing->tree_data.subprogram_data.is_used;
                        }
                    } else {
                        map_add(map, map_id, sub);
                    }
                }
                
                /* Also register the unmangled id so that inline asm references
                   (which use original Pascal names) can find the function.
                 * Always add (even if a prior entry exists) because map_find
                 * prefers entries with bodies — this ensures forward declarations
                 * registered earlier don't shadow the implementation. */
                const char *plain_id = sub->tree_data.subprogram_data.id;
                if (plain_id != NULL && (mangled_id == NULL || strcasecmp(plain_id, mangled_id) != 0)) {
                    map_add(map, plain_id, sub);
                }
                /* Also register cname_override (alias) so DCE can match alias references. */
                const char *cname = sub->tree_data.subprogram_data.cname_override;
                if (cname != NULL && map_find(map, cname) == NULL) {
                    map_add(map, cname, sub);
                }

                /* Recursively process nested subprograms */
                if (sub->tree_data.subprogram_data.subprograms != NULL) {
                    build_subprogram_map(sub->tree_data.subprogram_data.subprograms, map);
                }
            }
        }
        sub_list = sub_list->next;
    }

}

static void build_loaded_unit_subprogram_map(CompilationContext *comp_ctx, SubprogramMap *map)
{
    if (comp_ctx == NULL || map == NULL)
        return;

    for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui)
    {
        Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
        if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
            continue;

        if (unit_tree->tree_data.unit_data.subprograms != NULL)
            build_subprogram_map(unit_tree->tree_data.unit_data.subprograms, map);
    }
}

static void scan_used_subprogram_bodies(ListNode_t *sub_list, SubprogramMap *map)
{
    while (sub_list != NULL) {
        if (sub_list->type == LIST_TREE && sub_list->cur != NULL) {
            Tree_t *sub = (Tree_t *)sub_list->cur;
            if (sub->type == TREE_SUBPROGRAM && sub->tree_data.subprogram_data.is_used) {
                struct Statement *body = sub->tree_data.subprogram_data.statement_list;
                if (body != NULL)
                    mark_stmt_calls(body, map);
            }
        }
        sub_list = sub_list->next;
    }
}

static void mark_class_constructors_from_types(ListNode_t *type_list, SubprogramMap *map)
{
    while (type_list != NULL)
    {
        if (type_list->type == LIST_TREE && type_list->cur != NULL)
        {
            Tree_t *type_tree = (Tree_t *)type_list->cur;
            if (type_tree->type == TREE_TYPE_DECL && type_tree->tree_data.type_decl_data.id != NULL)
            {
                struct RecordType *record_info = NULL;
                if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                    record_info = type_tree->tree_data.type_decl_data.info.record;
                else if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                    record_info = type_tree->tree_data.type_decl_data.info.alias.inline_record_type;

                if (record_info != NULL && record_type_is_class(record_info))
                {
                    if (getenv("KGPC_TRACE_TFPLISTENUM") != NULL &&
                        strcasecmp(type_tree->tree_data.type_decl_data.id, "TFPListEnumerator") == 0)
                    {
                        fprintf(stderr, "[mark_used] type-scan hit class %s\n",
                            type_tree->tree_data.type_decl_data.id);
                    }
                    mark_class_methods_by_owner(NULL, type_tree->tree_data.type_decl_data.id, "Create", map);
                }
            }
        }
        type_list = type_list->next;
    }
}

static void mark_class_constructors_from_subprograms(ListNode_t *sub_list, SubprogramMap *map)
{
    while (sub_list != NULL)
    {
        if (sub_list->type == LIST_TREE && sub_list->cur != NULL)
        {
            Tree_t *sub = (Tree_t *)sub_list->cur;
            if (sub->type == TREE_SUBPROGRAM)
            {
                const char *owner = sub->tree_data.subprogram_data.owner_class;
                const char *method = sub->tree_data.subprogram_data.method_name;
                if (owner != NULL && method != NULL && strcasecmp(method, "Create") == 0)
                    mark_subprogram_recursive(sub, map);
                if (sub->tree_data.subprogram_data.subprograms != NULL)
                    mark_class_constructors_from_subprograms(sub->tree_data.subprogram_data.subprograms, map);
            }
        }
        sub_list = sub_list->next;
    }
}

void mark_used_functions(Tree_t *program, SymTab_t *symtab) {
    if (program == NULL || symtab == NULL || program->type != TREE_PROGRAM_TYPE) return;
    
    SubprogramMap map;
    map_init(&map);
    
    /* Build map of all program and loaded-unit subprograms. */
    build_subprogram_map(program->tree_data.program_data.subprograms, &map);
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL)
            build_loaded_unit_subprogram_map(comp_ctx, &map);
    }
    
    /* First, traverse bodies of already-used subprograms (e.g., from previous call).
     * This ensures that functions called by specialized methods are discovered. */
    scan_used_subprogram_bodies(program->tree_data.program_data.subprograms, &map);
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                scan_used_subprogram_bodies(unit_tree->tree_data.unit_data.subprograms, &map);
            }
        }
    }
    
    /* Start from the main program body */
    struct Statement *main_body = program->tree_data.program_data.body_statement;
    if (main_body != NULL) {
        mark_stmt_calls(main_body, &map);
    }
    
    /* Also traverse unit initialization and finalization blocks */
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                if (unit_tree->tree_data.unit_data.initialization != NULL)
                    mark_stmt_calls(unit_tree->tree_data.unit_data.initialization, &map);
                if (unit_tree->tree_data.unit_data.finalization != NULL)
                    mark_stmt_calls(unit_tree->tree_data.unit_data.finalization, &map);
            }
        }
    }

    /* Scan typed constant and variable initializers for function references.
       These contain EXPR_ADDR_OF_PROC (e.g. @NoBeginThread) that DCE must preserve. */
    {
        ListNode_t *var_node = program->tree_data.program_data.var_declaration;
        while (var_node != NULL) {
            if (var_node->type == LIST_TREE && var_node->cur != NULL) {
                Tree_t *vdecl = (Tree_t*)var_node->cur;
                if (vdecl->type == TREE_VAR_DECL) {
                    struct Statement *init = vdecl->tree_data.var_decl_data.initializer;
                    if (init != NULL) {
                        mark_stmt_calls(init, &map);
                    }
                }
            }
            var_node = var_node->next;
        }
    }

    /* Ensure VMT methods are retained even if they are not explicitly called. */
    mark_vmt_methods_used(program, &map);
    mark_class_constructors_from_types(program->tree_data.program_data.type_declaration, &map);
    mark_class_constructors_from_subprograms(program->tree_data.program_data.subprograms, &map);
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                mark_class_constructors_from_types(unit_tree->tree_data.unit_data.interface_type_decls, &map);
                mark_class_constructors_from_types(unit_tree->tree_data.unit_data.implementation_type_decls, &map);
                mark_class_constructors_from_subprograms(unit_tree->tree_data.unit_data.subprograms, &map);
            }
        }
    }

    /* VMT/interface rooting can mark additional class methods in loaded units.
     * Traverse used bodies again so nested calls from those newly-retained
     * methods are discovered too (e.g. unit GetEnumerator -> constructor). */
    scan_used_subprogram_bodies(program->tree_data.program_data.subprograms, &map);
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                scan_used_subprogram_bodies(unit_tree->tree_data.unit_data.subprograms, &map);
            }
        }
    }
    
    /* IMPORTANT: Second pass to sync forward declarations with implementations */
    /* For each subprogram, if it has the same mangled_id as another, sync is_used */
    ListNode_t *sub_list = program->tree_data.program_data.subprograms;
    while (sub_list != NULL) {
        if (sub_list->type == LIST_TREE && sub_list->cur != NULL) {
            Tree_t *sub = (Tree_t*)sub_list->cur;
            if (sub->type == TREE_SUBPROGRAM) {
                char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
                if (mangled_id != NULL) {
                    /* Find the canonical version in the map */
                    Tree_t *canonical = map_find(&map, mangled_id);
                    if (canonical != NULL && canonical != sub) {
                        /* Sync the is_used flag */
                        sub->tree_data.subprogram_data.is_used = canonical->tree_data.subprogram_data.is_used;
                    }
                }
            }
        }
        sub_list = sub_list->next;
    }

    /* Forward/body reconciliation can mark additional implementations as used
     * after the earlier scans. Walk used bodies one last time so nested calls
     * from those synchronized unit methods are not missed. */
    scan_used_subprogram_bodies(program->tree_data.program_data.subprograms, &map);
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL) {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui) {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree == NULL || unit_tree->type != TREE_UNIT)
                    continue;
                scan_used_subprogram_bodies(unit_tree->tree_data.unit_data.subprograms, &map);
            }
        }
    }
    
    #ifdef DEBUG_OPTIMIZER
        fprintf(stderr, "DEBUG mark_used_functions: Reachability analysis complete\n");
    #endif
    
    map_destroy(&map);
}

static void mark_vmt_methods_used(Tree_t *program, SubprogramMap *map)
{
    if (program == NULL || program->type != TREE_PROGRAM_TYPE || map == NULL)
        return;

    ListNode_t *type_node = program->tree_data.program_data.type_declaration;
    while (type_node != NULL)
    {
        if (type_node->type == LIST_TREE && type_node->cur != NULL)
        {
            Tree_t *type_tree = (Tree_t *)type_node->cur;
            if (type_tree->type == TREE_TYPE_DECL)
            {
                struct RecordType *record_info = NULL;
                if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    record_info = type_tree->tree_data.type_decl_data.info.record;
                }
                else if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                {
                    record_info = type_tree->tree_data.type_decl_data.info.alias.inline_record_type;
                }

                if (record_info != NULL && record_type_is_class(record_info))
                {
                    ListNode_t *method_node = record_info->methods;
                    while (method_node != NULL)
                    {
                        struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
                        const char *lookup_id = NULL;
                        if (method != NULL)
                            lookup_id = method->resolved_mangled_id != NULL ? method->resolved_mangled_id :
                                (method->mangled_name != NULL ? method->mangled_name : method->name);
                        if (lookup_id != NULL)
                        {
                            Tree_t *sub = map_find(map, lookup_id);
                            if (sub != NULL)
                                mark_subprogram_recursive(sub, map);
                            /* Also mark all overloads that share the base id. */
                            mark_subprograms_by_id(map, lookup_id);
                            /* Some FPC RTL VMT slots resolve to wrapper labels whose
                             * emitted bodies are tracked in the subprogram map under the
                             * Pascal method id rather than the final VMT target symbol.
                             * If exact lookup misses, fall back to the method name so
                             * wrapper overloads are still retained for the VMT. */
                            if (sub == NULL && method->name != NULL)
                            {
                                mark_class_methods_by_owner(
                                    program->tree_data.program_data.subprograms,
                                    type_tree->tree_data.type_decl_data.id,
                                    method->name, map);
                                mark_subprograms_by_id(map, method->name);
                            }
                        }
                        method_node = method_node->next;
                    }

                    /* Mark interface dispatch targets: for each interface this class
                     * implements, mark the class's implementing methods as used.
                     * e.g., TInterfacedObject._AddRef for IUnknown._AddRef */
                    if (record_info->num_interfaces > 0 && record_info->interface_names != NULL)
                    {
                        const char *class_id = type_tree->tree_data.type_decl_data.id;
                        for (int iidx = 0; iidx < record_info->num_interfaces; iidx++)
                        {
                            const char *iface_name = record_info->interface_names[iidx];
                            if (iface_name == NULL || class_id == NULL) continue;
                            /* Find the interface type in the type declarations list */
                            struct RecordType *iface_record = NULL;
                            ListNode_t *search = program->tree_data.program_data.type_declaration;
                            while (search != NULL) {
                                if (search->type == LIST_TREE && search->cur != NULL) {
                                    Tree_t *st = (Tree_t *)search->cur;
                                    if (st->type == TREE_TYPE_DECL && st->tree_data.type_decl_data.id != NULL &&
                                        strcasecmp(st->tree_data.type_decl_data.id, iface_name) == 0) {
                                        if (st->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                                            iface_record = st->tree_data.type_decl_data.info.record;
                                        else if (st->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                                            iface_record = st->tree_data.type_decl_data.info.alias.inline_record_type;
                                        break;
                                    }
                                }
                                search = search->next;
                            }
                            /* Use method_templates for interfaces (methods is NULL for interfaces) */
                            ListNode_t *imethod_list = NULL;
                            int use_method_templates = 0;
                            if (iface_record != NULL) {
                                if (iface_record->methods != NULL)
                                    imethod_list = iface_record->methods;
                                else if (iface_record->method_templates != NULL) {
                                    imethod_list = iface_record->method_templates;
                                    use_method_templates = 1;
                                }
                            }
                            if (imethod_list != NULL)
                            {
                                ListNode_t *imethod = imethod_list;
                                while (imethod != NULL) {
                                    const char *method_name = NULL;
                                    if (use_method_templates) {
                                        struct MethodTemplate *mt = (struct MethodTemplate *)imethod->cur;
                                        if (mt != NULL) method_name = mt->name;
                                    } else {
                                        struct MethodInfo *mi = (struct MethodInfo *)imethod->cur;
                                        if (mi != NULL) method_name = mi->name;
                                    }
                                    if (method_name != NULL) {
                                        mark_interface_dispatch_target(
                                            program->tree_data.program_data.type_declaration,
                                            class_id, record_info, iface_name, method_name, map);
                                    }
                                    imethod = imethod->next;
                                }
                            }
                        }
                    }
                }
            }
        }
        type_node = type_node->next;
    }
}

static void mark_subprograms_by_id(SubprogramMap *map, const char *id)
{
    if (map == NULL || id == NULL)
        return;

    char *lower_id = pascal_identifier_lower_dup(id);
    if (lower_id == NULL)
        return;

    unsigned idx = subprog_hash(lower_id);
    for (SubprogramEntry *e = map->id_buckets[idx]; e != NULL; e = e->next)
    {
        if (strcmp(e->canonical_id, lower_id) == 0)
        {
            Tree_t *sub = e->subprogram;
            if (sub != NULL && sub->type == TREE_SUBPROGRAM)
                mark_subprogram_recursive(sub, map);
        }
    }
    free(lower_id);
}

static void mark_class_methods_by_owner(ListNode_t *sub_list, const char *owner_class,
    const char *method_name, SubprogramMap *map)
{
    if (owner_class == NULL || method_name == NULL || map == NULL)
        return;

    int trace_tfplistenum = getenv("KGPC_TRACE_TFPLISTENUM") != NULL &&
        strcasecmp(owner_class, "TFPListEnumerator") == 0;
    if (trace_tfplistenum && sub_list == NULL)
        fprintf(stderr, "[mark_used] begin owner-scan %s.%s across loaded units\n", owner_class, method_name);

    if (sub_list == NULL)
    {
        CompilationContext *comp_ctx = compilation_context_get_active();
        if (comp_ctx != NULL)
        {
            for (int ui = 0; ui < comp_ctx->loaded_unit_count; ++ui)
            {
                Tree_t *unit_tree = comp_ctx->loaded_units[ui].unit_tree;
                if (unit_tree != NULL && unit_tree->type == TREE_UNIT &&
                    unit_tree->tree_data.unit_data.subprograms != NULL)
                    mark_class_methods_by_owner(unit_tree->tree_data.unit_data.subprograms,
                        owner_class, method_name, map);
            }
        }
        return;
    }

    size_t owner_len = strlen(owner_class);
    size_t method_len = strlen(method_name);

    for (ListNode_t *node = sub_list; node != NULL; node = node->next)
    {
        if (node->type != LIST_TREE || node->cur == NULL)
            continue;

        Tree_t *sub = (Tree_t *)node->cur;
        if (sub->type != TREE_SUBPROGRAM)
            continue;

        const char *sub_owner = sub->tree_data.subprogram_data.owner_class;
        const char *sub_method = sub->tree_data.subprogram_data.method_name;
        const char *sub_mangled = sub->tree_data.subprogram_data.mangled_id;
        int matches_mangled_prefix = 0;
        if (sub_mangled != NULL)
        {
            if (strncasecmp(sub_mangled, owner_class, owner_len) == 0 &&
                sub_mangled[owner_len] == '_' &&
                sub_mangled[owner_len + 1] == '_' &&
                strncasecmp(sub_mangled + owner_len + 2, method_name, method_len) == 0)
            {
                matches_mangled_prefix = 1;
            }
        }
        if (trace_tfplistenum)
        {
            fprintf(stderr,
                "[mark_used] scan owner=%s method=%s sub_owner=%s sub_method=%s mangled=%s body=%d used=%d\n",
                owner_class, method_name,
                sub_owner != NULL ? sub_owner : "(null)",
                sub_method != NULL ? sub_method : "(null)",
                sub_mangled != NULL ? sub_mangled : "(null)",
                sub->tree_data.subprogram_data.statement_list != NULL,
                sub->tree_data.subprogram_data.is_used);
        }
        if (sub_owner != NULL && sub_method != NULL &&
            strcasecmp(sub_owner, owner_class) == 0 &&
            strcasecmp(sub_method, method_name) == 0)
        {
            if (trace_tfplistenum)
                fprintf(stderr, "[mark_used] direct match -> %s\n",
                    sub_mangled != NULL ? sub_mangled : "(null)");
            mark_subprogram_recursive(sub, map);
        }
        else if (matches_mangled_prefix)
        {
            if (trace_tfplistenum)
                fprintf(stderr, "[mark_used] prefix match -> %s\n",
                    sub_mangled != NULL ? sub_mangled : "(null)");
            mark_subprogram_recursive(sub, map);
        }

        if (sub->tree_data.subprogram_data.subprograms != NULL)
        {
            mark_class_methods_by_owner(sub->tree_data.subprogram_data.subprograms,
                owner_class, method_name, map);
        }
    }
}

static int lookup_id_parse_owner_method(const char *lookup_id, char *owner_buf,
    size_t owner_buf_sz, char *method_buf, size_t method_buf_sz)
{
    if (lookup_id == NULL || owner_buf == NULL || method_buf == NULL ||
        owner_buf_sz == 0 || method_buf_sz == 0)
        return 0;

    const char *sep = strstr(lookup_id, "__");
    if (sep == NULL || sep == lookup_id || sep[2] == '\0')
        return 0;

    size_t owner_len = (size_t)(sep - lookup_id);
    size_t method_len = 0;
    const char *method_start = sep + 2;
    while (method_start[method_len] != '\0' && method_start[method_len] != '_')
        method_len++;
    if (owner_len + 1 > owner_buf_sz || method_len + 1 > method_buf_sz || method_len == 0)
        return 0;

    memcpy(owner_buf, lookup_id, owner_len);
    owner_buf[owner_len] = '\0';
    memcpy(method_buf, method_start, method_len);
    method_buf[method_len] = '\0';
    return 1;
}
