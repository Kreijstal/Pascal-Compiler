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
#include "codegen_string_set.h"
#include "codegen_symbol_resolution.h"
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/NameMangling.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../Parser/SemanticCheck/SemCheck.h"

#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "ir/ir_inst.h"
#include "ir/ir_cfg.h"
#include "ir/ir_liveness.h"
#include "ir/ir_peephole.h"
#if USE_GRAPH_COLORING_ALLOCATOR
#include "graph_coloring_allocator.h"
#endif

#include "codegen_subprograms_internal.h"
#include "codegen_vmt_internal.h"

int codegen_float_native_distance(Tree_t *sub);
int codegen_list_contains_string(ListNode_t *list, const char *value);
struct RecordType *semcheck_lookup_record_type(SymTab_t *symtab, const char *type_id);
/* Defined in SemCheck_const_eval.c — declared here so codegen can register
 * function-local real consts in cache-miss mode. */
int evaluate_real_const_expr(SymTab_t *symtab, struct Expression *expr, double *out_value);
int expression_contains_real_literal_impl(SymTab_t *symtab, struct Expression *expr);
const char *codegen_find_class_method_impl_id(SymTab_t *symtab,
    const struct RecordType *record, const char *fallback_class_label,
    const char *iface_name, const char *method_name);
void codegen_emit_const_decl_equivs_from_list(CodeGenContext *ctx,
    ListNode_t *const_decls);
void codegen_register_owner_unit_scope(CodeGenContext *ctx,
    SymTab_t *symtab, int source_unit_index);
static void codegen_register_record_field_enum_literals(SymTab_t *symtab,
    struct RecordType *record);
static void codegen_register_type_enum_literals(ListNode_t *type_decls, SymTab_t *symtab);
ListNode_t *g_codegen_available_subprograms = NULL;
/* g_available_subprograms_set, g_codegen_callable_exports, and
 * g_available_subprograms_tail declared after CodeGenStringSet definition below */

#if USE_GRAPH_COLORING_ALLOCATOR
/*
 * ir_liveness_allocate — graph-coloring register allocator driven by IR
 * liveness analysis.
 *
 * Uses stable integer vreg IDs (stored in IrInst_t.vreg_ids[] at
 * add_inst_du() time) rather than Register_t* pointers, so this is safe
 * to call after reset_reg_stack() has freed the original register objects.
 *
 * Algorithm:
 *  1. Build the CFG for the function's instruction list.
 *  2. Compute live-in / live-out sets via backward dataflow (liveness_compute).
 *  3. Discover the physical register pool from the current reg_stack.
 *  4. Scan the instruction list to:
 *       a. determine the maximum vreg_id used, and
 *       b. map each vreg_id to its current physical register name (preferred_color).
 *  5. Build an interference graph: two vregs interfere if they appear
 *     simultaneously in any live_out[B] or live_in[B] set, or if they are
 *     both defs of the same instruction.
 *  6. Run the simplify/select/color loop (preferred_color keeps existing
 *     assignments stable when there is no conflict).
 *  7. If the coloring succeeds with no spills, apply it by updating
 *     IrInst_t.reg_names_64[] and reg_names_32[] in every instruction.
 *  8. Free all temporary data structures.
 */
void ir_liveness_allocate(ListNode_t *inst_list)
{
    if (inst_list == NULL)
        return;

    /* ------------------------------------------------------------------ */
    /* Step 1 & 2: CFG + liveness                                          */
    /* ------------------------------------------------------------------ */
    Cfg_t *cfg = cfg_build(inst_list);
    if (cfg == NULL)
        return;

    LivenessInfo_t *liveness = liveness_compute(cfg);
    if (liveness == NULL)
    {
        cfg_free(cfg);
        return;
    }

    /* ------------------------------------------------------------------ */
    /* Step 3: physical register pool from reg_stack                       */
    /* The reg_stack always holds the same 5 callee-saved registers        */
    /* (%rbx, %r12-%r15).  We enumerate them to build a color→name table. */
    /* ------------------------------------------------------------------ */
#define LIVENESS_MAX_POOL 16
    const char *pool_names_64[LIVENESS_MAX_POOL];
    const char *pool_names_32[LIVENESS_MAX_POOL];
    int pool_size = 0;

    RegStack_t *rstack = get_reg_stack();
    if (rstack != NULL)
    {
        for (ListNode_t *n = rstack->registers_free;
             n != NULL && pool_size < LIVENESS_MAX_POOL; n = n->next)
        {
            Register_t *reg = (Register_t *)n->cur;
            if (reg != NULL && reg->bit_64 != NULL)
            {
                pool_names_64[pool_size] = reg->bit_64;
                pool_names_32[pool_size] = reg->bit_32;
                pool_size++;
            }
        }
        for (ListNode_t *n = rstack->registers_allocated;
             n != NULL && pool_size < LIVENESS_MAX_POOL; n = n->next)
        {
            Register_t *reg = (Register_t *)n->cur;
            if (reg != NULL && reg->bit_64 != NULL)
            {
                pool_names_64[pool_size] = reg->bit_64;
                pool_names_32[pool_size] = reg->bit_32;
                pool_size++;
            }
        }
    }

    if (pool_size == 0)
    {
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    /* ------------------------------------------------------------------ */
    /* Step 4a: find maximum vreg_id used in this function's instruction   */
    /* list.                                                               */
    /* ------------------------------------------------------------------ */
    int max_vreg = -1;
    for (ListNode_t *n = inst_list; n != NULL; n = n->next)
    {
        if (n->type == LIST_IR_INST)
        {
            const IrInst_t *inst = (const IrInst_t *)n->cur;
            if (inst == NULL) continue;
            int total = inst->n_defs + inst->n_uses;
            for (int i = 0; i < total && i < IR_MAX_DEFS + IR_MAX_USES; ++i)
            {
                if (inst->vreg_ids[i] > max_vreg)
                    max_vreg = inst->vreg_ids[i];
            }
        }
    }

    if (max_vreg < 0)
    {
        /* No virtual registers — nothing to allocate. */
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    int n_vregs = max_vreg + 1;

    /* ------------------------------------------------------------------ */
    /* Step 4b: map vreg_id → preferred color (= current pool index)       */
    /* ------------------------------------------------------------------ */
    int *vreg_to_color = (int *)malloc((size_t)n_vregs * sizeof(int));
    if (vreg_to_color == NULL)
    {
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }
    for (int i = 0; i < n_vregs; ++i)
        vreg_to_color[i] = -1;

    /* Build vreg_id → preferred color (= pool index) map by scanning the
     * instruction list.  All occurrences of the same vreg_id must map to the
     * same physical register; any divergence is a compiler bug. */
    for (ListNode_t *n = inst_list; n != NULL; n = n->next)
    {
        if (n->type != LIST_IR_INST) continue;
        const IrInst_t *inst = (const IrInst_t *)n->cur;
        if (inst == NULL) continue;
        int total = inst->n_defs + inst->n_uses;
        for (int i = 0; i < total && i < IR_MAX_DEFS + IR_MAX_USES; ++i)
        {
            int v = inst->vreg_ids[i];
            if (v < 0 || v >= n_vregs) continue;
            if (inst->reg_names_64[i][0] == '\0') continue;
            /* Find this register's pool index. */
            int c = -1;
            for (int k = 0; k < pool_size; ++k)
            {
                if (pool_names_64[k] != NULL &&
                    strcmp(inst->reg_names_64[i], pool_names_64[k]) == 0)
                {
                    c = k;
                    break;
                }
            }
            if (c < 0) continue;   /* not a pooled register */
            if (vreg_to_color[v] < 0)
            {
                vreg_to_color[v] = c;
            }
            else
            {
                /* Same vreg_id must always map to the same physical register.
                 * A mismatch indicates a compiler bug in vreg ID scoping — the
                 * register state was not properly reset between nested function
                 * compilations, causing stale IDs to leak into the outer
                 * function's instruction list. */
                assert(vreg_to_color[v] == c &&
                       "vreg ID collision: same vreg_id maps to two physical registers");
            }
        }
    }

    /* ------------------------------------------------------------------ */
    /* Step 5: build interference graph                                    */
    /* ------------------------------------------------------------------ */
    InterferenceGraph_t *graph = create_interference_graph(pool_size);
    if (graph == NULL)
    {
        free(vreg_to_color);
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    LiveRange_t **ranges = (LiveRange_t **)calloc((size_t)n_vregs,
                                                   sizeof(LiveRange_t *));
    if (ranges == NULL)
    {
        free_interference_graph(graph);
        free(vreg_to_color);
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    /* Create one LiveRange per live vreg (those with a known pool mapping). */
    int ranges_ok = 1;
    for (int v = 0; v < n_vregs; ++v)
    {
        if (vreg_to_color[v] < 0)
            continue;
        ranges[v] = create_live_range(v, 0, 0);
        if (ranges[v] == NULL) { ranges_ok = 0; break; }
        ranges[v]->preferred_color = vreg_to_color[v];
    }
    if (!ranges_ok)
    {
        /* Ranges not yet added to graph — free them directly. */
        for (int v = 0; v < n_vregs; ++v)
            free(ranges[v]);
        free(ranges);
        free_interference_graph(graph);
        free(vreg_to_color);
        liveness_free(liveness);
        cfg_free(cfg);
        return;
    }

    /* Now add all successfully created ranges to the graph. */
    for (int v = 0; v < n_vregs; ++v)
    {
        if (ranges[v] != NULL)
            add_live_range(graph, ranges[v]);
    }

    /* Add interference edges for each block's live_out set (all pairs). */
    for (int b = 0; b < liveness->n_blocks; ++b)
    {
        const LiveSet_t *lout = &liveness->live_out[b];
        for (int i = 0; i < lout->n_regs; ++i)
        {
            int vi = lout->vreg_ids[i];
            if (vi < 0 || vi >= n_vregs || ranges[vi] == NULL) continue;
            for (int j = i + 1; j < lout->n_regs; ++j)
            {
                int vj = lout->vreg_ids[j];
                if (vj < 0 || vj >= n_vregs || vj == vi || ranges[vj] == NULL)
                    continue;
                add_interference_edge(ranges[vi], ranges[vj]);
            }
        }
        /* Also add interference for live_in pairs (handles values live at
         * block entry that are never defined within the block, e.g. function
         * parameters that flow across multiple blocks). */
        const LiveSet_t *lin = &liveness->live_in[b];
        for (int i = 0; i < lin->n_regs; ++i)
        {
            int vi = lin->vreg_ids[i];
            if (vi < 0 || vi >= n_vregs || ranges[vi] == NULL) continue;
            for (int j = i + 1; j < lin->n_regs; ++j)
            {
                int vj = lin->vreg_ids[j];
                if (vj < 0 || vj >= n_vregs || vj == vi || ranges[vj] == NULL)
                    continue;
                add_interference_edge(ranges[vi], ranges[vj]);
            }
        }
    }

    /* Add interference between co-defs of the same instruction. */
    for (ListNode_t *n = inst_list; n != NULL; n = n->next)
    {
        if (n->type != LIST_IR_INST) continue;
        const IrInst_t *inst = (const IrInst_t *)n->cur;
        if (inst == NULL) continue;
        for (int di = 0; di < inst->n_defs; ++di)
        {
            int d1 = inst->vreg_ids[di];
            if (d1 < 0 || d1 >= n_vregs || ranges[d1] == NULL) continue;
            for (int di2 = di + 1; di2 < inst->n_defs; ++di2)
            {
                int d2 = inst->vreg_ids[di2];
                if (d2 < 0 || d2 >= n_vregs || d2 == d1 || ranges[d2] == NULL)
                    continue;
                add_interference_edge(ranges[d1], ranges[d2]);
            }
        }
    }

    /* ------------------------------------------------------------------ */
    /* Step 6: run graph coloring                                          */
    /* ------------------------------------------------------------------ */
    ListNode_t *spilled = allocate_registers_graph_coloring_prebuilt(graph);
    /* The register pool consists solely of the callee-saved registers that
     * the LRU allocator has already pre-assigned.  With preferred colors
     * locked in, graph coloring is effectively just a consistency check and
     * should never produce spills.  A spill here indicates a compiler bug. */
    assert(spilled == NULL &&
           "graph coloring produced spills — impossible with pre-allocated callee-saved registers");

    /* ------------------------------------------------------------------ */
    /* Step 7: apply coloring                                              */
    /* ------------------------------------------------------------------ */
    {
        for (ListNode_t *n = inst_list; n != NULL; n = n->next)
        {
            if (n->type != LIST_IR_INST) continue;
            IrInst_t *inst = (IrInst_t *)n->cur;
            if (inst == NULL) continue;
            int total = inst->n_defs + inst->n_uses;
            for (int i = 0; i < total && i < IR_MAX_DEFS + IR_MAX_USES; ++i)
            {
                int v = inst->vreg_ids[i];
                if (v < 0 || v >= n_vregs || ranges[v] == NULL) continue;
                int color = ranges[v]->assigned_reg_num;
                if (color < 0 || color >= pool_size) continue;
                if (pool_names_64[color] != NULL)
                    snprintf(inst->reg_names_64[i], IR_REG_NAME_BUF,
                             "%s", pool_names_64[color]);
                if (pool_names_32[color] != NULL)
                    snprintf(inst->reg_names_32[i], IR_REG_NAME_BUF,
                             "%s", pool_names_32[color]);
            }
        }
    }

    /* ------------------------------------------------------------------ */
    /* Step 8: cleanup                                                     */
    /* free_interference_graph frees neighbor lists (sets them to NULL);   */
    /* we then free the LiveRange_t objects via free_live_range.           */
    /* ------------------------------------------------------------------ */
    free_interference_graph(graph);
    for (int v = 0; v < n_vregs; ++v)
        free_live_range(ranges[v]);
    free(ranges);
    free(vreg_to_color);
    liveness_free(liveness);
    cfg_free(cfg);
}
#undef LIVENESS_MAX_POOL
#endif /* USE_GRAPH_COLORING_ALLOCATOR */

/* Return a lazily-created, process-lifetime canonical KgpcType for
 * SHORTSTRING_TYPE.  This avoids allocating ad-hoc instances via
 * create_primitive_type(SHORTSTRING_TYPE) in code paths that only need
 * a type descriptor for identification (e.g. ctx->current_return_type,
 * proc_info.return_type patching).  Borrowers must not free it; owners that
 * store it in ref-counted fields must retain it like any other KgpcType. */
KgpcType *codegen_canonical_shortstring_type(void)
{
    static KgpcType *canonical = NULL;
    if (canonical == NULL)
        canonical = create_primitive_type(SHORTSTRING_TYPE);
    return canonical;
}

#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_LABEL_BUFFER_SIZE 256

/* Module-level string sets for callable exports and available subprogram labels.
 * Declared non-static so codegen_symbol_resolution.c can access them via extern. */
CodeGenStringSet g_codegen_callable_exports;
CodeGenStringSet g_available_subprograms_set;
ListNode_t *g_available_subprograms_tail = NULL;

/* Module-level emitted-subprograms hash set + tail pointer for O(1) operations.
 * Reset in codegen() before each compilation. */
CodeGenStringSet g_emitted_set;
ListNode_t *g_emitted_tail = NULL;

static void codegen_reset_emitted_set(void)
{
    codegen_set_destroy(&g_emitted_set);
    memset(&g_emitted_set, 0, sizeof(g_emitted_set));
    g_emitted_tail = NULL;
}

/* ---- String constant collection for local const strings ---- */
/* String constants from local const declarations (e.g. `const S = '...'`
 * inside function bodies) are registered into the symbol table via
 * PushStringConstOntoScope so the existing .LC label emission in
 * gencode_leaf_var handles them correctly with unique, scope-aware labels.
 * No global collection or separate emission pass is needed. */

int codegen_list_contains_string(ListNode_t *list, const char *value)
{
    for (ListNode_t *cur = list; cur != NULL; cur = cur->next) {
        if (cur->type == LIST_STRING && cur->cur != NULL &&
            strcmp((const char *)cur->cur, value) == 0)
            return 1;
    }
    return 0;
}

void codegen_keep_subprogram_label(const char *label)
{
    if (label == NULL)
        return;
    if (codegen_set_contains(&g_available_subprograms_set, label))
        return;

    codegen_set_insert(&g_available_subprograms_set, label);
    ListNode_t *node = CreateListNode((void *)label, LIST_STRING);
    if (g_codegen_available_subprograms == NULL) {
        g_codegen_available_subprograms = node;
        g_available_subprograms_tail = node;
    } else {
        g_available_subprograms_tail->next = node;
        g_available_subprograms_tail = node;
    }
}


typedef struct RecordParamWork {
    const char *id;
    int size;
    int stack_arg_offset;
    int has_stack_arg;
    const char *arg_reg;
    int is_dynarray;
    int dynarray_elem_size;
    int dynarray_lower_bound;
    int arg_index;
} RecordParamWork;

/* Escape a string for use in assembly .string directive */
void escape_string(char *dest, const char *src, size_t dest_size)
{
    if (dest == NULL || src == NULL || dest_size == 0)
        return;
    
    size_t i = 0, j = 0;
    while (src[i] != '\0' && j < dest_size - 1)
    {
        switch (src[i])
        {
            case '"':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = '"';
                }
                break;
            case '\\':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = '\\';
                }
                break;
            case '\n':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = 'n';
                }
                break;
            case '\t':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = 't';
                }
                break;
            case '\r':
                if (j + 2 < dest_size - 1)
                {
                    dest[j++] = '\\';
                    dest[j++] = 'r';
                }
                break;
            default:
                dest[j++] = src[i];
                break;
        }
        i++;
    }
    dest[j] = '\0';
}

/* Helper functions for transitioning from legacy type fields to KgpcType */
static char *codegen_make_program_var_label(CodeGenContext *ctx, const char *name);
static int codegen_typed_const_name_collides_ctx(CodeGenContext *ctx, const char *bare_id);
static void codegen_emit_bss_or_comm(FILE *out, const char *sym, const char *label,
                                     int size, int alignment, int defined_in_unit);

/* Helper function to check if a node is a record type */
static inline int node_is_record_type(HashNode_t *node)
{
    return hashnode_is_record(node);
}

/* Helper function to check if a node is a file type */
int node_is_file_type(HashNode_t *node)
{
    if (node == NULL || node->type == NULL)
        return 0;
    return kgpc_type_equals_tag(node->type, FILE_TYPE) ||
        kgpc_type_equals_tag(node->type, TEXT_TYPE);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

HashNode_t *codegen_pick_type_node_by_name(SymTab_t *symtab, const char *type_name)
{
    if (symtab == NULL || type_name == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, type_name);
    if (matches == NULL)
        return NULL;

    HashNode_t *best_node = NULL;
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        if (cand == NULL || cand->hash_type != HASHTYPE_TYPE)
            continue;

        if (best_node == NULL ||
            (best_node->source_unit_index <= 0 && cand->source_unit_index > 0) ||
            (cand->source_unit_index > 0 && best_node->source_unit_index > 0 &&
             cand->source_unit_index > best_node->source_unit_index) ||
            (cand->source_unit_index == best_node->source_unit_index &&
             !best_node->unit_is_public && cand->unit_is_public) ||
            (cand->source_unit_index == best_node->source_unit_index &&
             cand->unit_is_public == best_node->unit_is_public &&
             !best_node->defined_in_unit && cand->defined_in_unit)) {
            best_node = cand;
        }
    }

    DestroyList(matches);
    return best_node;
}

struct RecordType *codegen_lookup_record_type_for_node(SymTab_t *symtab,
    HashNode_t *node, const char *type_name)
{
    if (node == NULL)
        return NULL;

    if (node->source_unit_index > 0 && type_name != NULL)
    {
        const char *unit_name = unit_registry_get(node->source_unit_index);
        if (unit_name != NULL)
        {
            size_t qualified_len = strlen(unit_name) + 1 + strlen(type_name) + 1;
            char *qualified_id = (char *)malloc(qualified_len);
            if (qualified_id != NULL)
            {
                snprintf(qualified_id, qualified_len, "%s.%s", unit_name, type_name);
                HashNode_t *qualified = NULL;
                if (FindSymbol(&qualified, symtab, qualified_id) != 0 && qualified != NULL)
                {
                    struct RecordType *qualified_record = get_record_type_from_node(qualified);
                    if (qualified_record != NULL)
                    {
                        free(qualified_id);
                        return qualified_record;
                    }
                }
                free(qualified_id);
            }
        }
    }

    return get_record_type_from_node(node);
}

int codegen_parse_guid_literal(const char *guid, uint32_t *d1,
    uint16_t *d2, uint16_t *d3, uint8_t d4[8])
{
    if (guid == NULL || d1 == NULL || d2 == NULL || d3 == NULL || d4 == NULL)
        return 0;

    const char *p = guid;
    if (*p == '\'')
        p++;
    if (*p != '{')
        return 0;
    p++;

    unsigned int td1 = 0, td2 = 0, td3 = 0;
    unsigned int td4[8];
    int matched = sscanf(p,
        "%8x-%4x-%4x-%2x%2x-%2x%2x%2x%2x%2x%2x",
        &td1, &td2, &td3,
        &td4[0], &td4[1], &td4[2], &td4[3],
        &td4[4], &td4[5], &td4[6], &td4[7]);
    if (matched != 11)
        return 0;

    *d1 = (uint32_t)td1;
    *d2 = (uint16_t)td2;
    *d3 = (uint16_t)td3;
    for (int i = 0; i < 8; ++i)
        d4[i] = (uint8_t)td4[i];
    return 1;
}

int codegen_resolve_record_guid(SymTab_t *symtab, const struct RecordType *record,
    uint32_t *d1, uint16_t *d2, uint16_t *d3, uint8_t d4[8])
{
    if (record == NULL || d1 == NULL || d2 == NULL || d3 == NULL || d4 == NULL)
        return 0;

    if (record->has_guid)
    {
        *d1 = record->guid_d1;
        *d2 = record->guid_d2;
        *d3 = record->guid_d3;
        memcpy(d4, record->guid_d4, 8);
        return 1;
    }

    if (record->guid_string == NULL || record->guid_string[0] == '\0')
        return 0;

    if (codegen_parse_guid_literal(record->guid_string, d1, d2, d3, d4))
        return 1;

    if (symtab == NULL)
        return 0;

    ListNode_t *matches = FindAllIdents(symtab, record->guid_string);
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node == NULL || node->hash_type != HASHTYPE_CONST ||
            node->const_string_value == NULL)
            continue;
        if (codegen_parse_guid_literal(node->const_string_value, d1, d2, d3, d4))
        {
            if (matches != NULL)
                DestroyList(matches);
            return 1;
        }
    }
    if (matches != NULL)
        DestroyList(matches);
    return 0;
}

int codegen_template_matches_methodinfo(const struct MethodTemplate *tmpl,
    const struct MethodInfo *method)
{
    if (tmpl == NULL || method == NULL || tmpl->name == NULL || method->name == NULL)
        return 0;
    if (strcasecmp(method->name, tmpl->name) != 0)
        return 0;

    int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
    if (wanted_params >= 0 && method->param_count >= 0)
        return wanted_params == method->param_count;
    return 1;
}

struct RecordType *codegen_lookup_record_type_by_name(SymTab_t *symtab,
    const char *type_name, int prefer_guid)
{
    if (symtab == NULL || type_name == NULL)
        return NULL;

    struct RecordType *record = semcheck_lookup_record_type(symtab, type_name);
    if (record != NULL)
    {
        if (!prefer_guid)
            return record;
        uint32_t guid_d1 = 0;
        uint16_t guid_d2 = 0;
        uint16_t guid_d3 = 0;
        uint8_t guid_d4[8] = {0};
        if (codegen_resolve_record_guid(symtab, record,
                &guid_d1, &guid_d2, &guid_d3, guid_d4))
            return record;
    }

    int n_units = unit_registry_count();
    for (int i = 1; i <= n_units && i < SYMTAB_MAX_UNITS; ++i)
    {
        ScopeNode *unit_scope = symtab->unit_scopes[i];
        if (unit_scope == NULL || unit_scope->table == NULL)
            continue;

        HashNode_t *node = FindIdentInTable(unit_scope->table, type_name);
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            return codegen_lookup_record_type_for_node(symtab, node, type_name);
        }

        node = FindTypeBySuffixInTable(unit_scope->table, type_name);
        if (node != NULL && node->hash_type == HASHTYPE_TYPE)
        {
            KGPC_COMPILER_HARD_ASSERT(0,
                "suffix-only record type lookup for '%s' reached codegen",
                type_name);
        }
    }

    HashNode_t *best_node = codegen_pick_type_node_by_name(symtab, type_name);
    if (best_node != NULL)
        return codegen_lookup_record_type_for_node(symtab, best_node, type_name);

    return NULL;
}

int emitted_class_set_contains(const EmittedClassSet *set, const char *label)
{
    if (set == NULL || label == NULL)
        return 0;

    for (int i = 0; i < set->count; ++i)
    {
        if (set->labels[i] != NULL && strcmp(set->labels[i], label) == 0)
            return 1;
    }

    return 0;
}

int emitted_class_set_add(EmittedClassSet *set, const char *label)
{
    if (set == NULL || label == NULL)
        return 1;

    if (emitted_class_set_contains(set, label))
        return 0;

    if (set->count == set->capacity)
    {
        int new_capacity = (set->capacity > 0) ? set->capacity * 2 : 64;
        const char **new_labels = (const char **)realloc(set->labels,
            sizeof(const char *) * (size_t)new_capacity);
        if (new_labels == NULL)
            return 1;
        set->labels = new_labels;
        set->capacity = new_capacity;
    }

    char *owned = strdup(label);
    if (owned == NULL)
        return 1;
    set->labels[set->count++] = owned;
    return 0;
}

void emitted_class_set_destroy(EmittedClassSet *set)
{
    if (set == NULL)
        return;

    for (int i = 0; i < set->count; ++i)
        free((void *)set->labels[i]);
    free((void *)set->labels);
    set->labels = NULL;
    set->count = 0;
    set->capacity = 0;
}

int codegen_type_decl_suppressed(const Tree_t *decl)
{
    return (decl != NULL &&
        decl->type == TREE_TYPE_DECL &&
        decl->tree_data.type_decl_data.suppress_codegen);
}

struct RecordType *codegen_record_from_type_decl_ex(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL || decl->type != TREE_TYPE_DECL)
        return NULL;

    KgpcType *kgpc = decl->tree_data.type_decl_data.kgpc_type;
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

    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
    {
        struct RecordType *fallback = decl->tree_data.type_decl_data.info.record;
        /* When a forward-declared class is completed, the full declaration's
         * RecordType becomes a depleted shell (fields transferred to the
         * canonical RecordType in the symtab).  Detect this and look up
         * the canonical record instead. */
        if (fallback != NULL && fallback->is_class &&
            fallback->fields == NULL && fallback->parent_fields_merged &&
            symtab != NULL && decl->tree_data.type_decl_data.id != NULL)
        {
            HashNode_t *canon_node = NULL;
            if (FindSymbol(&canon_node, symtab, decl->tree_data.type_decl_data.id) &&
                canon_node != NULL && canon_node->type != NULL)
            {
                /* Extract record from symtab node, handling both direct
                 * record types and pointer-to-record (class types). */
                struct RecordType *canon = NULL;
                KgpcType *ct = canon_node->type;
                if (ct->kind == TYPE_KIND_RECORD && ct->info.record_info != NULL)
                    canon = ct->info.record_info;
                else if (ct->kind == TYPE_KIND_POINTER &&
                         ct->info.points_to != NULL &&
                         ct->info.points_to->kind == TYPE_KIND_RECORD &&
                         ct->info.points_to->info.record_info != NULL)
                    canon = ct->info.points_to->info.record_info;
                if (canon != NULL)
                    return canon;
            }
        }
        return fallback;
    }

    if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        return decl->tree_data.type_decl_data.info.alias.inline_record_type;

    return NULL;
}

struct RecordType *codegen_record_from_type_decl(Tree_t *decl)
{
    return codegen_record_from_type_decl_ex(decl, NULL);
}

/* Get field offset within a record by field name.
 * Returns -1 if field not found. */
static int record_type_get_field_offset(SymTab_t *symtab, struct RecordType *record,
    const char *field_name)
{
    if (record == NULL || field_name == NULL)
        return -1;

    struct RecordField *field_desc = NULL;
    long long offset = 0;
    if (resolve_record_field(symtab, record, field_name, &field_desc, &offset, 0, 1) != 0 ||
        field_desc == NULL)
        return -1;

    if (offset < 0 || offset > INT_MAX)
        return -1;

    return (int)offset;
}

int node_is_class_type(HashNode_t *node)
{
    if (node == NULL)
        return 0;
    if (!node_is_record_type(node))
        return 0;
    struct RecordType *record = get_record_type_from_node(node);
    return record_type_is_class(record);
}

int record_has_class_vars(const struct RecordType *record)
{
    if (record == NULL || record->fields == NULL)
        return 0;
    ListNode_t *field_node = record->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field != NULL && field->is_class_var == 1)
                return 1;
        }
        field_node = field_node->next;
    }
    return 0;
}

int record_has_class_method_templates(const struct RecordType *record)
{
    if (record == NULL || record->method_templates == NULL)
        return 0;
    ListNode_t *node = record->method_templates;
    while (node != NULL)
    {
        if (node->type == LIST_METHOD_TEMPLATE && node->cur != NULL)
        {
            struct MethodTemplate *templ = (struct MethodTemplate *)node->cur;
            if (templ->is_class_method || templ->is_static)
                return 1;
        }
        node = node->next;
    }
    return 0;
}

static const char *codegen_class_constructor_target(SymTab_t *symtab,
    const char *owner, const struct MethodTemplate *tmpl, Tree_t **definition_out)
{
    assert(symtab != NULL);
    assert(owner != NULL);
    assert(tmpl != NULL);
    assert(tmpl->name != NULL);

    if (definition_out != NULL)
        *definition_out = NULL;

    char lookup[512];
    snprintf(lookup, sizeof(lookup), "%s__%s", owner, tmpl->name);

    HashNode_t *method_node = NULL;
    assert(FindSymbol(&method_node, symtab, lookup) != 0);
    assert(method_node != NULL);
    assert(method_node->type != NULL);
    assert(method_node->type->kind == TYPE_KIND_PROCEDURE);
    assert(method_node->type->info.proc_info.definition != NULL);

    Tree_t *definition = method_node->type->info.proc_info.definition;
    if (definition_out != NULL)
        *definition_out = definition;

    const char *target = codegen_subprogram_emission_symbol(method_node);
    assert(target != NULL);
    assert(target[0] != '\0');
    return target;
}

static void codegen_mark_class_constructors_used(ListNode_t *type_decls,
    SymTab_t *symtab)
{
    for (ListNode_t *node = type_decls; node != NULL; node = node->next)
    {
        if (node->type != LIST_TREE || node->cur == NULL)
            continue;
        Tree_t *type_tree = (Tree_t *)node->cur;
        if (type_tree->type != TREE_TYPE_DECL ||
            type_tree->tree_data.type_decl_data.kind != TYPE_DECL_RECORD)
            continue;
        struct RecordType *record = type_tree->tree_data.type_decl_data.info.record;
        const char *owner = type_tree->tree_data.type_decl_data.id;
        if (record == NULL || owner == NULL)
            continue;
        for (ListNode_t *mnode = record->method_templates; mnode != NULL; mnode = mnode->next)
        {
            if (mnode->type != LIST_METHOD_TEMPLATE || mnode->cur == NULL)
                continue;
            struct MethodTemplate *tmpl = (struct MethodTemplate *)mnode->cur;
            if (tmpl->kind != METHOD_TEMPLATE_CONSTRUCTOR ||
                !tmpl->is_class_method || !tmpl->is_static ||
                tmpl->name == NULL)
                continue;

            Tree_t *definition = NULL;
            const char *target = codegen_class_constructor_target(symtab, owner, tmpl, &definition);
            assert(definition != NULL);
            definition->tree_data.subprogram_data.is_used = 1;
            codegen_keep_subprogram_label(target);
        }
    }
}

static ListNode_t *codegen_class_constructor_calls(ListNode_t *inst_list,
    ListNode_t *type_decls, SymTab_t *symtab)
{
    for (ListNode_t *node = type_decls; node != NULL; node = node->next)
    {
        if (node->type != LIST_TREE || node->cur == NULL)
            continue;
        Tree_t *type_tree = (Tree_t *)node->cur;
        if (type_tree->type != TREE_TYPE_DECL ||
            type_tree->tree_data.type_decl_data.kind != TYPE_DECL_RECORD)
            continue;
        struct RecordType *record = type_tree->tree_data.type_decl_data.info.record;
        const char *owner = type_tree->tree_data.type_decl_data.id;
        if (record == NULL || owner == NULL)
            continue;
        for (ListNode_t *mnode = record->method_templates; mnode != NULL; mnode = mnode->next)
        {
            if (mnode->type != LIST_METHOD_TEMPLATE || mnode->cur == NULL)
                continue;
            struct MethodTemplate *tmpl = (struct MethodTemplate *)mnode->cur;
            if (tmpl->kind != METHOD_TEMPLATE_CONSTRUCTOR ||
                !tmpl->is_class_method || !tmpl->is_static ||
                tmpl->name == NULL)
                continue;
            char buffer[1024];
            const char *target = codegen_class_constructor_target(symtab, owner, tmpl, NULL);
            snprintf(buffer, sizeof(buffer), "\tcall\t%s\n", target);
            inst_list = add_inst(inst_list, buffer);
        }
    }
    return inst_list;
}

int record_has_method_decls(const struct RecordType *record)
{
    if (record == NULL || record->fields == NULL)
        return 0;
    ListNode_t *node = record->fields;
    while (node != NULL)
    {
        if (node->type == LIST_UNSPECIFIED && node->cur != NULL)
            return 1;
        node = node->next;
    }
    return 0;
}

int codegen_class_var_field_size(SymTab_t *symtab, const struct RecordField *field)
{
    if (field == NULL)
        return DOUBLEWORD;

    int field_size = DOUBLEWORD;

    if (field->type_id != NULL)
    {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, field->type_id) != 0 && type_node != NULL &&
            type_node->type != NULL)
        {
            long long type_size = kgpc_type_sizeof(type_node->type);
            if (type_size > 0 && type_size <= INT_MAX)
                field_size = (int)type_size;
            return field_size;
        }
    }

    if (field->is_array)
    {
        int elem_size = 0;
        if (field->array_element_type_id != NULL)
        {
            HashNode_t *elem_node = NULL;
            if (FindSymbol(&elem_node, symtab, field->array_element_type_id) != 0 &&
                elem_node != NULL && elem_node->type != NULL)
            {
                long long type_size = kgpc_type_sizeof(elem_node->type);
                if (type_size > 0 && type_size <= INT_MAX)
                    elem_size = (int)type_size;
            }
        }
        if (elem_size == 0)
        {
            switch (field->array_element_type)
            {
                case CHAR_TYPE:
                case BOOL:
                case BYTE_TYPE:
                    elem_size = 1;
                    break;
                case WORD_TYPE:
                    elem_size = 2;
                    break;
                case LONGINT_TYPE:
                case LONGWORD_TYPE:
                case INT_TYPE:
                    elem_size = 4;
                    break;
                case INT64_TYPE:
                case QWORD_TYPE:
                case REAL_TYPE:
                case STRING_TYPE:
                case POINTER_TYPE:
                    elem_size = 8;
                    break;
                case EXTENDED_TYPE:
                    elem_size = 10;
                    break;
                default:
                    elem_size = DOUBLEWORD;
                    break;
            }
        }
        long long count = (long long)field->array_end - (long long)field->array_start + 1;
        if (count < 0)
            count = 0;
        long long total = count * elem_size;
        if (total > 0 && total <= INT_MAX)
            field_size = (int)total;
        return field_size;
    }

    switch (field->type)
    {
        case INT64_TYPE:
        case REAL_TYPE:
        case STRING_TYPE:
        case POINTER_TYPE:
        case QWORD_TYPE:
            field_size = 8;
            break;
        case EXTENDED_TYPE:
            field_size = 10;
            break;
        case CHAR_TYPE:
        case BOOL:
        case BYTE_TYPE:
            field_size = 1;
            break;
        case WORD_TYPE:
            field_size = 2;
            break;
        case LONGINT_TYPE:
        case LONGWORD_TYPE:
        case INT_TYPE:
            field_size = DOUBLEWORD;
            break;
        default:
            field_size = DOUBLEWORD;
            break;
    }

    return field_size;
}

int codegen_record_field_alignment(const struct RecordField *field, int field_size)
{
    if (field != NULL)
    {
        if (field->type == EXTENDED_TYPE)
            return 16;
        if (field->type_id != NULL && pascal_identifier_equals(field->type_id, "Extended"))
            return 16;
    }
    return (field_size > 8) ? 16 : ((field_size >= 8) ? 8 : ((field_size >= 4) ? 4 : 1));
}

long long codegen_class_var_storage_size(SymTab_t *symtab, const struct RecordType *record_info,
    int include_all_fields)
{
    if (record_info == NULL || record_info->fields == NULL)
        return 0;

    long long current_offset = 0;
    ListNode_t *field_node = record_info->fields;
    while (field_node != NULL)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field != NULL && (include_all_fields || field->is_class_var == 1))
            {
                int field_size = codegen_class_var_field_size(symtab, field);
                int alignment = codegen_record_field_alignment(field, field_size);
                current_offset = (current_offset + alignment - 1) & ~(alignment - 1);
                current_offset += field_size;
            }
        }
        field_node = field_node->next;
    }
    return current_offset;
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias* get_type_alias_from_node(HashNode_t *node)
{
    return hashnode_get_type_alias(node);
}

/**
 * For class methods, register the class variables with the stack manager
 * so they can be found during code generation via find_label_with_depth.
 * 
 * This function extracts the class name from the mangled method name (ClassName__MethodName),
 * looks up the class type in the symbol table, and registers each class var field
 * with the stack manager using add_static_var.
 */
void codegen_add_class_vars_for_method(const char *owner_class,
    const char *method_name_arg, SymTab_t *symtab, CodeGenContext *ctx)
{
    if (owner_class == NULL || symtab == NULL)
        return;

    if (method_name_arg == NULL)
        return;

    char *class_name = strdup(owner_class);
    if (class_name == NULL)
        return;

    /* Look up the class type */
    HashNode_t *class_node = NULL;
    if (!FindSymbol(&class_node, symtab, class_name) || class_node == NULL)
    {
        free(class_name);
        return;
    }
    
    /* Get the record type - for classes, the type is a pointer to the record */
    struct RecordType *record_info = get_record_type_from_node(class_node);
    if (record_info == NULL)
    {
        /* Try to dereference if it's a pointer type (class types are pointers to records) */
        if (class_node->type != NULL && class_node->type->kind == TYPE_KIND_POINTER)
        {
            KgpcType *pointed_to = class_node->type->info.points_to;
            if (pointed_to != NULL && pointed_to->kind == TYPE_KIND_RECORD)
            {
                record_info = pointed_to->info.record_info;
            }
        }
    }
    
    if (record_info == NULL)
    {
        free(class_name);
        return;
    }
    int has_class_vars = record_has_class_vars(record_info);
    int include_all_fields = 0;
    if (!has_class_vars)
        include_all_fields = 1;
    if (record_info->is_type_helper)
    {
        free(class_name);
        return;
    }
    int is_static_check = from_cparser_is_method_static(class_name, method_name_arg);
    int is_nonstatic_class_method =
        from_cparser_is_method_nonstatic_class_method(class_name, method_name_arg);
    if (!is_static_check && !is_nonstatic_class_method && !has_class_vars)
    {
        free(class_name);
        return;
    }
    
    /* Use the original class name from the type definition to match the CLASSVAR label.
     * The mangled name may have different casing, but the CLASSVAR label uses the original type_id. */
    const char *original_class_name = (record_info != NULL && record_info->type_id != NULL) ?
        record_info->type_id : class_name;
    
    /* Build the class var storage label */
    size_t label_len = strlen(original_class_name) + strlen("_CLASSVAR") + 1;
    char *classvar_label = (char *)malloc(label_len);
    if (classvar_label == NULL)
    {
        free(class_name);
        return;
    }
    snprintf(classvar_label, label_len, "%s_CLASSVAR", original_class_name);
    
    /* Iterate over fields and register each as a static var with proper offset */
    ListNode_t *field_node = record_info->fields;
    long long current_offset = 0;
    
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
            if (!include_all_fields && field->is_class_var != 1)
            {
                field_node = field_node->next;
                continue;
            }

            /* Calculate field size */
            int field_size = codegen_class_var_field_size(symtab, field);
            
            /* Build the static label for this field: ClassName_CLASSVAR+offset */
            /* We register it with offset information */
            /* Buffer size: classvar_label + "+" + max_digits(long long) + null terminator */
            /* max_digits for long long is 20, so 32 provides ample margin */
            size_t field_label_len = strlen(classvar_label) + 32;
            char *field_static_label = (char *)malloc(field_label_len);
            if (field_static_label != NULL)
            {
                int written;
                if (current_offset == 0)
                    written = snprintf(field_static_label, field_label_len, "%s", classvar_label);
                else
                    written = snprintf(field_static_label, field_label_len, "%s+%lld", classvar_label, current_offset);
                
                /* Verify buffer was large enough */
                assert(written >= 0 && (size_t)written < field_label_len);
                
                /* Register class vars under a class-qualified key to avoid cross-class collisions. */
                size_t key_len = strlen(class_name) + 2 + strlen(field->name) + 1;
                char *classvar_key = (char *)malloc(key_len);
                if (classvar_key != NULL)
                {
                    snprintf(classvar_key, key_len, "%s::%s", class_name, field->name);
                    StackScope_t *cur_scope = get_cur_scope();
                    StackNode_t *existing = NULL;
                    if (cur_scope != NULL)
                        existing = stackscope_find_x(cur_scope, classvar_key);
                    if (existing == NULL)
                        add_static_var(classvar_key, field_size, field_static_label);

                    if (cur_scope != NULL &&
                        stackscope_find_z(cur_scope, field->name) == NULL &&
                        stackscope_find_x(cur_scope, field->name) == NULL &&
                        stackscope_find_t(cur_scope, field->name) == NULL)
                    {
                        add_absolute_var_alias(field->name, classvar_key);
                    }
                    free(classvar_key);
                }
                free(field_static_label);
            }
            
            /* Advance offset with alignment (using standard power-of-two alignment formula) */
            int alignment = codegen_record_field_alignment(field, field_size);
            current_offset = (current_offset + alignment - 1) & ~(alignment - 1);
            current_offset += field_size;
        }
        field_node = field_node->next;
    }
    
    free(classvar_label);
    free(class_name);
}

int codegen_real_param_storage_size(Tree_t *arg_decl,
    HashNode_t *resolved_type_node, KgpcType *cached_arg_type)
{
    if (arg_decl != NULL && arg_decl->type == TREE_VAR_DECL)
    {
        if (arg_decl->tree_data.var_decl_data.type == EXTENDED_TYPE)
            return 16;
        if (arg_decl->tree_data.var_decl_data.type_id != NULL &&
            pascal_identifier_equals(arg_decl->tree_data.var_decl_data.type_id, "Extended"))
            return 16;
        struct TypeAlias *alias = arg_decl->tree_data.var_decl_data.inline_type_alias;
        if (alias != NULL && alias->storage_size > 0)
            return (int)alias->storage_size;
    }

    if (resolved_type_node != NULL && resolved_type_node->type != NULL)
    {
        if (kgpc_type_is_extended(resolved_type_node->type))
            return 16;
        if (kgpc_type_is_real(resolved_type_node->type))
        {
            long long real_size = kgpc_type_real_storage_size(resolved_type_node->type);
            if (real_size > 8)
                return 16;
            if (real_size > 0)
                return (int)real_size;
        }
        long long size = kgpc_type_sizeof(resolved_type_node->type);
        if (size > 0)
            return (int)size;
    }

    if (cached_arg_type != NULL)
    {
        if (kgpc_type_is_extended(cached_arg_type))
            return 16;
        if (kgpc_type_is_real(cached_arg_type))
        {
            long long real_size = kgpc_type_real_storage_size(cached_arg_type);
            if (real_size > 8)
                return 16;
            if (real_size > 0)
                return (int)real_size;
        }
        long long size = kgpc_type_sizeof(cached_arg_type);
        if (size > 0)
            return (int)size;
    }

    return 8;
}

int codegen_shortstring_storage_size(KgpcType *type)
{
    if (type == NULL)
        return 0;

    int primitive_shortstring = (type->kind == TYPE_KIND_PRIMITIVE &&
        kgpc_type_get_primitive_tag(type) == SHORTSTRING_TYPE);

    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL && alias->is_shortstring)
    {
        if (alias->storage_size > 1 && alias->storage_size <= INT_MAX)
            return (int)alias->storage_size;
        if (alias->array_end >= alias->array_start && alias->array_end >= 0)
        {
            int alias_size = alias->array_end - alias->array_start + 1;
            if (alias_size > 1)
                return alias_size;
        }
        if (primitive_shortstring)
            return 256;
    }

    if (kgpc_type_is_shortstring(type))
    {
        long long size = kgpc_type_sizeof(type);
        if (size > 1 && size <= INT_MAX)
            return (int)size;
        return 256;
    }

    return 0;
}

/* Helper function to determine variable storage size (for stack allocation)
 * Returns size in bytes, or -1 on error */
static inline int get_var_storage_size(HashNode_t *node)
{
    if (node == NULL)
        return -1;

    struct TypeAlias *node_alias = get_type_alias_from_node(node);
    if (node_alias != NULL)
    {
        if (node_alias->is_pointer || node_alias->is_class_reference)
            return 8;
        if (node_alias->is_shortstring)
        {
            if (node_alias->storage_size > 1 && node_alias->storage_size <= INT_MAX)
                return (int)node_alias->storage_size;
            if (node_alias->array_end >= node_alias->array_start && node_alias->array_end >= 0)
                return node_alias->array_end - node_alias->array_start + 1;
            return 256;
        }
        if (node_alias->storage_size > 0 && node_alias->storage_size <= INT_MAX)
            return (int)node_alias->storage_size;
    }
    
    /* Check KgpcType first */
    if (node->type != NULL)
    {
        if (node->type->kind == TYPE_KIND_PRIMITIVE)
        {
            /* Honor explicit storage overrides from type aliases (e.g., Int64/QWord) */
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias != NULL)
            {
                if (alias->is_shortstring)
                {
                    int short_size = codegen_shortstring_storage_size(node->type);
                    return short_size > 0 ? short_size : 256;
                }
                if (alias->storage_size > 0)
                    return (int)alias->storage_size;
            }

            int tag = kgpc_type_get_primitive_tag(node->type);
            switch (tag)
            {
                case LONGINT_TYPE:
                    return 4;  // Match FPC's 32-bit LongInt
                case INT64_TYPE:
                    return 8;
                case REAL_TYPE:
                {
                    long long size = kgpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return 8;
                }
                case STRING_TYPE:  /* PCHAR */
                case POINTER_TYPE:
                case PROCEDURE:
                    return 8;
                case SHORTSTRING_TYPE:
                {
                    int short_size = codegen_shortstring_storage_size(node->type);
                    return short_size > 0 ? short_size : 256;
                }
                case FILE_TYPE:
                case TEXT_TYPE:
                {
                    long long size = kgpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return 8;
                }
                case SET_TYPE:
                {
                    /* Check if this is a character set */
                    long long size = kgpc_type_sizeof(node->type);
                    if (size > 0)
                        return (int)size;
                    return DOUBLEWORD;  /* Default for non-char sets */
                }
                case CHAR_TYPE:
                    return 1;
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
            
            long long size = kgpc_type_sizeof(node->type);
            if (size > 0)
                return (int)size;
            return -1;
        }
    }
    return DOUBLEWORD;
}

static int codegen_storage_size_from_type(KgpcType *type)
{
    if (type == NULL)
        return -1;

    if (kgpc_type_is_shortstring(type))
    {
        int short_size = codegen_shortstring_storage_size(type);
        return short_size > 0 ? short_size : 256;
    }

    struct TypeAlias *alias = kgpc_type_get_type_alias(type);
    if (alias != NULL)
    {
        if (alias->is_pointer || alias->is_class_reference)
            return 8;
        if (alias->is_set)
        {
            long long set_size = kgpc_type_sizeof(type);
            KGPC_COMPILER_HARD_ASSERT(set_size > 0 && set_size <= INT_MAX,
                "set type '%s' has no structured storage size",
                alias->alias_name != NULL ? alias->alias_name : "(anonymous set)");
            return (int)set_size;
        }
        if (alias->is_shortstring)
        {
            int short_size = codegen_shortstring_storage_size(type);
            return short_size > 0 ? short_size : 256;
        }
        if (alias->storage_size > 0 && alias->storage_size <= INT_MAX)
            return (int)alias->storage_size;
    }

    long long size = kgpc_type_sizeof(type);
    if (size > 0 && size <= INT_MAX)
        return (int)size;

    if (type->kind == TYPE_KIND_POINTER || type->kind == TYPE_KIND_PROCEDURE)
        return 8;

    if (type->kind == TYPE_KIND_PRIMITIVE)
    {
        switch (kgpc_type_get_primitive_tag(type))
        {
            case CHAR_TYPE:
            case BOOL:
                return 1;
            case LONGINT_TYPE:
                return 4;
            case INT64_TYPE:
            case QWORD_TYPE:
            case STRING_TYPE:
            case POINTER_TYPE:
            case PROCEDURE:
            case REAL_TYPE:
                return 8;
            case SHORTSTRING_TYPE:
                return 256;
            default:
                return DOUBLEWORD;
        }
    }

    return -1;
}

static int codegen_storage_size_from_type_alias(const struct TypeAlias *alias)
{
    if (alias == NULL)
        return -1;

    if (alias->is_pointer || alias->is_class_reference)
        return 8;

    if (alias->is_set)
    {
        KgpcType *set_type = create_primitive_type(SET_TYPE);
        KGPC_COMPILER_HARD_ASSERT(set_type != NULL,
            "failed to allocate structured set type for storage sizing");
        kgpc_type_set_type_alias(set_type, (struct TypeAlias *)alias);
        long long set_size = kgpc_type_sizeof(set_type);
        destroy_kgpc_type(set_type);
        KGPC_COMPILER_HARD_ASSERT(set_size > 0 && set_size <= INT_MAX,
            "set type '%s' has no structured storage size",
            alias->alias_name != NULL ? alias->alias_name : "(anonymous set)");
        return (int)set_size;
    }

    if (alias->is_shortstring)
    {
        if (alias->storage_size > 1 && alias->storage_size <= INT_MAX)
            return (int)alias->storage_size;
        if (alias->array_end >= alias->array_start && alias->array_end >= 0)
        {
            int alias_size = alias->array_end - alias->array_start + 1;
            if (alias_size > 1)
                return alias_size;
        }
        return 256;
    }

    if (alias->storage_size > 0 && alias->storage_size <= INT_MAX)
        return (int)alias->storage_size;

    return -1;
}

ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list, CodeGenContext *ctx, SymTab_t *symtab);

kgpc_target_abi_t g_current_codegen_abi = KGPC_TARGET_ABI_SYSTEM_V;
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

void codegen_destroy_static_link_procs(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;

    ListNode_t *node = ctx->static_link_procs;
    while (node != NULL)
    {
        ListNode_t *next = node->next;
        free(node->cur);
        free(node);
        node = next;
    }
    ctx->static_link_procs = NULL;
}

void codegen_reset_static_link_cache(CodeGenContext *ctx)
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

/* Drop any cached static-link state WITHOUT touching the register pool.
 * Used immediately after reset_reg_stack(), which has already freed all
 * registers — ctx->static_link_reg would otherwise dangle into freed
 * memory and produce a use-after-free the next time codegen_acquire_static_link
 * sees a stale non-NULL pointer with a matching level. The spill slot also
 * belongs to a stack scope that may no longer be live. */
void codegen_invalidate_static_link_cache(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    ctx->static_link_reg = NULL;
    ctx->static_link_reg_level = 0;
    ctx->static_link_spill_slot = NULL;
}

void codegen_register_local_types(ListNode_t *type_decls, SymTab_t *symtab)
{
    if (type_decls == NULL || symtab == NULL)
        return;

    /* First pass: register record/class types so pointer aliases can resolve. */
    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type != TREE_TYPE_DECL ||
            decl->tree_data.type_decl_data.id == NULL)
            continue;
        if (codegen_type_decl_suppressed(decl))
            continue;
        if (decl->tree_data.type_decl_data.kind != TYPE_DECL_RECORD)
            continue;

        KgpcType *kgpc = decl->tree_data.type_decl_data.kgpc_type;
        int created_kgpc = 0;
        if (kgpc == NULL && decl->tree_data.type_decl_data.info.record != NULL)
        {
            kgpc = create_record_type(decl->tree_data.type_decl_data.info.record);
            if (decl->tree_data.type_decl_data.info.record->is_class && kgpc != NULL)
                kgpc = create_pointer_type(kgpc);
            created_kgpc = 1;
        }

        if (kgpc != NULL)
        {
            PushTypeOntoScope_Typed(symtab, decl->tree_data.type_decl_data.id, kgpc);
            if (decl->tree_data.type_decl_data.info.record != NULL)
            {
                codegen_register_record_field_enum_literals(symtab,
                    decl->tree_data.type_decl_data.info.record);
            }
            if (created_kgpc)
                destroy_kgpc_type(kgpc);
        }
    }

    /* Second pass: register aliases and resolve pointer targets now that records exist. */
    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type != TREE_TYPE_DECL ||
            decl->tree_data.type_decl_data.id == NULL)
            continue;
        if (codegen_type_decl_suppressed(decl))
            continue;
        if (decl->tree_data.type_decl_data.kind != TYPE_DECL_ALIAS)
            continue;

        struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
        KgpcType *kgpc = decl->tree_data.type_decl_data.kgpc_type;
        int created_kgpc = 0;
        if (kgpc == NULL)
        {
            kgpc = create_kgpc_type_from_type_alias(alias, symtab,
                decl->tree_data.type_decl_data.defined_in_unit);
            created_kgpc = 1;
        }

        if (kgpc != NULL && kgpc_type_is_pointer(kgpc) &&
            kgpc->info.points_to == NULL)
        {
            const char *pointee_id = alias->pointer_type_id;
            if (pointee_id == NULL)
                pointee_id = alias->target_type_id;
            if (pointee_id != NULL)
            {
                HashNode_t *pointee_node = NULL;
                if (FindSymbol(&pointee_node, symtab, pointee_id) != 0 &&
                    pointee_node != NULL && pointee_node->type != NULL)
                {
                    kgpc_type_retain(pointee_node->type);
                    kgpc->info.points_to = pointee_node->type;
                }
            }
        }

        if (kgpc != NULL)
        {
            PushTypeOntoScope_Typed(symtab, decl->tree_data.type_decl_data.id, kgpc);
            if (alias->is_enum && alias->enum_literals != NULL)
            {
                int ordinal = 0;
                for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
                {
                    const char *literal_name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (literal_name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, literal_name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)literal_name, ordinal, kgpc);
                }
            }
            if (created_kgpc)
                destroy_kgpc_type(kgpc);
        }
    }
}

static void codegen_register_record_field_enum_literals(SymTab_t *symtab,
    struct RecordType *record)
{
    if (symtab == NULL || record == NULL)
        return;

    for (ListNode_t *field_node = record->fields; field_node != NULL; field_node = field_node->next)
    {
        if (field_node->type == LIST_RECORD_FIELD && field_node->cur != NULL)
        {
            struct RecordField *field = (struct RecordField *)field_node->cur;
            if (field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                for (ListNode_t *lit = field->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
                {
                    const char *name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)name, ordinal, enum_type);
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (field_node->type == LIST_VARIANT_PART && field_node->cur != NULL)
        {
            struct VariantPart *variant = (struct VariantPart *)field_node->cur;
            if (variant->tag_field != NULL && variant->tag_field->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                for (ListNode_t *lit = variant->tag_field->enum_literals; lit != NULL;
                     lit = lit->next, ++ordinal)
                {
                    const char *name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)name, ordinal, enum_type);
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }

            for (ListNode_t *branch_node = variant->branches; branch_node != NULL;
                 branch_node = branch_node->next)
            {
                if (branch_node->type == LIST_VARIANT_BRANCH && branch_node->cur != NULL)
                {
                    struct VariantBranch *branch = (struct VariantBranch *)branch_node->cur;
                    struct RecordType temp_rec;
                    memset(&temp_rec, 0, sizeof(temp_rec));
                    temp_rec.fields = branch->members;
                    codegen_register_record_field_enum_literals(symtab, &temp_rec);
                }
            }
        }
    }
}

static void codegen_register_type_enum_literals(ListNode_t *type_decls, SymTab_t *symtab)
{
    if (type_decls == NULL || symtab == NULL)
        return;

    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl->type != TREE_TYPE_DECL)
            continue;
        if (codegen_type_decl_suppressed(decl))
            continue;

        if (decl->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
        {
            struct TypeAlias *alias = &decl->tree_data.type_decl_data.info.alias;
            if (alias->is_enum && alias->enum_literals != NULL)
            {
                KgpcType *enum_type = create_primitive_type(ENUM_TYPE);
                int ordinal = 0;
                for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
                {
                    const char *name = (const char *)lit->cur;
                    HashNode_t *existing = NULL;
                    if (name == NULL)
                        continue;
                    if (FindSymbol(&existing, symtab, name) == 0 || existing == NULL)
                        PushConstOntoScope_Typed(symtab, (char *)name, ordinal, enum_type);
                }
                if (enum_type != NULL)
                    kgpc_type_release(enum_type);
            }
        }
        else if (decl->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                 decl->tree_data.type_decl_data.info.record != NULL)
        {
            codegen_register_record_field_enum_literals(symtab,
                decl->tree_data.type_decl_data.info.record);
        }
    }
}

static int codegen_eval_const_expr(struct Expression *expr, SymTab_t *symtab,
    long long *out_value);
static int codegen_eval_string_const_expr(struct Expression *expr, SymTab_t *symtab,
    char **out_value);

static HashNode_t *codegen_find_const_eval_symbol(SymTab_t *symtab, const char *id)
{
    if (symtab == NULL || id == NULL)
        return NULL;

    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, (char *)id) != 0 && node != NULL)
        return node;

    int unit_index = symtab->current_unit_index;
    if (unit_index > 0 && unit_index < SYMTAB_MAX_UNITS &&
        symtab->unit_scopes[unit_index] != NULL &&
        symtab->unit_scopes[unit_index]->table != NULL)
    {
        node = FindIdentInTableForUnit(symtab->unit_scopes[unit_index]->table,
            (char *)id, unit_index);
        if (node != NULL)
            return node;
        node = FindIdentInTable(symtab->unit_scopes[unit_index]->table, (char *)id);
        if (node != NULL)
            return node;
    }

    ListNode_t *matches = FindAllIdents(symtab, (char *)id);
    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
    {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL &&
            (candidate->hash_type == HASHTYPE_CONST || candidate->is_constant ||
             candidate->is_typed_const))
        {
            node = candidate;
            break;
        }
    }
    if (matches != NULL)
        DestroyList(matches);
    if (node != NULL)
        return node;

    return NULL;
}

static int codegen_eval_string_const_expr(struct Expression *expr, SymTab_t *symtab,
    char **out_value)
{
    if (out_value == NULL)
        return 1;
    *out_value = NULL;
    if (expr == NULL)
        return 1;

    switch (expr->type)
    {
        case EXPR_STRING:
            if (expr->expr_data.string == NULL)
                return 1;
            *out_value = strdup(expr->expr_data.string);
            return (*out_value == NULL);

        case EXPR_CHAR_CODE:
        {
            char *value = (char *)calloc(2, sizeof(char));
            if (value == NULL)
                return 1;
            value[0] = (char)(expr->expr_data.char_code & 0xFF);
            *out_value = value;
            return 0;
        }

        case EXPR_VAR_ID:
        {
            if (symtab == NULL || expr->expr_data.id == NULL)
                return 1;
            HashNode_t *node = codegen_find_const_eval_symbol(symtab, expr->expr_data.id);
            if (node == NULL)
                return 1;
            if (node->const_string_value != NULL)
            {
                *out_value = strdup(node->const_string_value);
                return (*out_value == NULL);
            }
            KgpcType *expr_type = expr->resolved_kgpc_type;
            if ((node->hash_type == HASHTYPE_CONST || node->is_constant) &&
                ((node->type != NULL && kgpc_type_is_char(node->type)) ||
                 (expr_type != NULL && kgpc_type_is_char(expr_type))))
            {
                char *value = (char *)calloc(2, sizeof(char));
                if (value == NULL)
                    return 1;
                value[0] = (char)(node->const_int_value & 0xFF);
                *out_value = value;
                return 0;
            }
            return 1;
        }

        case EXPR_ADDOP:
            if (expr->expr_data.addop_data.addop_type != PLUS)
                return 1;
            break;

        default:
            return 1;
    }

    char *left = NULL;
    char *right = NULL;
    if (codegen_eval_string_const_expr(expr->expr_data.addop_data.left_expr, symtab, &left) != 0)
        return 1;
    if (codegen_eval_string_const_expr(expr->expr_data.addop_data.right_term, symtab, &right) != 0)
    {
        free(left);
        return 1;
    }

    size_t left_len = strlen(left);
    size_t right_len = strlen(right);
    char *combined = malloc(left_len + right_len + 1);
    if (combined == NULL)
    {
        free(left);
        free(right);
        return 1;
    }

    memcpy(combined, left, left_len);
    memcpy(combined + left_len, right, right_len + 1);
    free(left);
    free(right);
    *out_value = combined;
    return 0;
}

void codegen_register_const_decls(ListNode_t *const_decls, SymTab_t *symtab)
{
    if (const_decls == NULL || symtab == NULL)
        return;

    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_CONST_DECL)
            continue;

        const char *id = decl->tree_data.const_decl_data.id;
        struct Expression *value = decl->tree_data.const_decl_data.value;
        if (id == NULL || value == NULL)
            continue;

        unsigned char set_bytes[32];
        size_t set_size = 0;
        long long set_mask = 0;
        int is_char_set = 0;

        if (expression_is_set_const_expr(symtab, value) &&
            evaluate_set_const_bytes(symtab, value, set_bytes, sizeof(set_bytes),
                &set_size, &set_mask, &is_char_set) == 0)
        {
            KgpcType *set_type = create_primitive_type(SET_TYPE);
            if (set_type != NULL)
            {
                if (is_char_set)
                    set_type->size_in_bytes = 32;
                else
                    set_type->size_in_bytes = 4;
            }
            PushSetConstOntoScope(symtab, (char *)id, set_bytes, (int)set_size, set_type);
            if (set_type != NULL)
                destroy_kgpc_type(set_type);
            continue;
        }

        long long const_value = 0;
        if (codegen_eval_const_expr(value, symtab, &const_value))
        {
            int pushed = 0;
            KgpcType *value_type = (value != NULL) ? value->resolved_kgpc_type : NULL;
            if ((value != NULL && value->type == EXPR_CHAR_CODE) ||
                (value_type != NULL && kgpc_type_is_char(value_type)))
            {
                KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                if (char_type != NULL)
                {
                    if (PushConstOntoScope_Typed(symtab, (char *)id, const_value, char_type) == 0)
                    {
                        HashNode_t *const_node = NULL;
                        if (FindSymbol(&const_node, symtab, id) != 0 && const_node != NULL &&
                            const_node->const_string_value == NULL)
                        {
                            char char_string[2];
                            char_string[0] = (char)(const_value & 0xFF);
                            char_string[1] = '\0';
                            const_node->const_string_value = strdup(char_string);
                        }
                    }
                    destroy_kgpc_type(char_type);
                    pushed = 1;
                }
            }
            if (!pushed)
                PushConstOntoScope(symtab, (char *)id, const_value);
        }
        else if (expression_contains_real_literal_impl(symtab, value))
        {
            /* Real const (e.g. `const DELTA = 0.001;` or `c = 1.0/$10000`).
             * The cache-miss codegen path enters a fresh scope per subprogram,
             * so without this branch the const goes unresolved at use site and
             * codegen emits "Unresolved non-local symbol". */
            double real_value = 0.0;
            if (evaluate_real_const_expr(symtab, value, &real_value) == 0)
                PushRealConstOntoScope(symtab, (char *)id, real_value);
        }
        else
        {
            char *string_value = NULL;
            int string_eval_ok = (codegen_eval_string_const_expr(value, symtab, &string_value) == 0);
            if (string_eval_ok &&
                string_value != NULL)
            {
                /* String constant — register in the symbol table so the existing
                 * .LC label emission in gencode_leaf_var handles it with a unique,
                 * scope-aware label. PushStringConstOntoScope is a no-op if the
                 * identifier already exists (e.g. from semcheck). */
                PushStringConstOntoScope(symtab, (char *)id, string_value);
                free(string_value);
            }
        }
    }
}

static void codegen_register_inline_var_enum_literals(Tree_t *decl, SymTab_t *symtab)
{
    if (decl == NULL || symtab == NULL || decl->type != TREE_VAR_DECL)
        return;

    struct TypeAlias *alias = decl->tree_data.var_decl_data.inline_type_alias;
    if (alias == NULL || !alias->is_enum || alias->enum_literals == NULL)
        return;

    KgpcType *enum_type = alias->kgpc_type;
    int created_enum_type = 0;
    if (enum_type == NULL)
    {
        enum_type = create_primitive_type(ENUM_TYPE);
        created_enum_type = (enum_type != NULL);
    }

    if (enum_type == NULL)
        return;

    int ordinal = 0;
    for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next, ++ordinal)
    {
        const char *literal_name = (const char *)lit->cur;
        if (literal_name == NULL)
            continue;

        /* Inline enum literals belong to the local declaration scope. They must
         * shadow nonlocal methods/types with the same name, so only suppress the
         * insertion if the current scope already has an exact symbol. */
        HashNode_t *existing = FindIdentInCurrentScope(symtab, literal_name);
        if (existing == NULL)
            PushConstOntoScope_Typed(symtab, (char *)literal_name, ordinal, enum_type);
    }

    if (created_enum_type)
        kgpc_type_release(enum_type);
}

static KgpcType *codegen_prefer_promoted_shortstring_type(KgpcType *decl_type,
    const HashNode_t *var_node)
{
    if (decl_type != NULL && var_node != NULL && var_node->type != NULL &&
        kgpc_type_is_string(decl_type) &&
        !kgpc_type_is_shortstring(decl_type) &&
        kgpc_type_is_shortstring(var_node->type))
    {
        return var_node->type;
    }
    return decl_type;
}

void codegen_register_decl_list(CodeGenContext *ctx, ListNode_t *decls, SymTab_t *symtab, int is_param)
{
    if (decls == NULL || symtab == NULL)
        return;

    for (ListNode_t *cur = decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL)
            continue;
        if (decl->type != TREE_VAR_DECL && decl->type != TREE_ARR_DECL)
            continue;

        ListNode_t *ids = (decl->type == TREE_VAR_DECL)
            ? decl->tree_data.var_decl_data.ids
            : decl->tree_data.arr_decl_data.ids;

        KgpcType *decl_type = resolve_type_from_vardecl(decl, symtab, NULL);
        int is_array_decl = (decl->type == TREE_ARR_DECL);
        if (!is_array_decl && decl_type != NULL)
            is_array_decl = kgpc_type_is_array(decl_type);

        for (ListNode_t *id_node = ids; id_node != NULL; id_node = id_node->next)
        {
            if (id_node->cur == NULL)
                continue;
            HashNode_t *promoted_source = NULL;
            KgpcType *effective_decl_type = decl_type;
            if (FindSymbol(&promoted_source, symtab, id_node->cur) != 0 &&
                promoted_source != NULL &&
                (promoted_source->hash_type == HASHTYPE_VAR ||
                 promoted_source->hash_type == HASHTYPE_ARRAY ||
                 promoted_source->hash_type == HASHTYPE_FUNCTION_RETURN))
            {
                effective_decl_type = codegen_prefer_promoted_shortstring_type(
                    effective_decl_type, promoted_source);
            }
            if (is_array_decl)
                PushArrayOntoScope_Typed(symtab, (char *)id_node->cur, effective_decl_type);
            else
                PushVarOntoScope_Typed(symtab, (char *)id_node->cur, effective_decl_type);

            /* Propagate is_typed_const and source_unit_index from the decl
             * tree onto the freshly-registered HashNode so codegen lookups
             * (e.g. codegen_try_emit_nonlocal_global) can disambiguate
             * same-named typed-consts across units. */
            {
                HashNode_t *registered = FindIdentInCurrentScope(symtab, id_node->cur);
                if (registered != NULL)
                {
                    if (decl->type == TREE_VAR_DECL)
                    {
                        if (decl->tree_data.var_decl_data.is_typed_const)
                            registered->is_typed_const = 1;
                        if (decl->tree_data.var_decl_data.source_unit_index > 0 &&
                            registered->source_unit_index == 0)
                            registered->source_unit_index =
                                decl->tree_data.var_decl_data.source_unit_index;
                        if (decl->tree_data.var_decl_data.defined_in_unit)
                            registered->defined_in_unit = 1;
                    }
                    else if (decl->type == TREE_ARR_DECL)
                    {
                        if (decl->tree_data.arr_decl_data.is_typed_const)
                            registered->is_typed_const = 1;
                        if (decl->tree_data.arr_decl_data.source_unit_index > 0 &&
                            registered->source_unit_index == 0)
                            registered->source_unit_index =
                                decl->tree_data.arr_decl_data.source_unit_index;
                        if (decl->tree_data.arr_decl_data.defined_in_unit)
                            registered->defined_in_unit = 1;
                    }

                    /* Set mangled_id to the unit-qualified storage key only
                     * when this typed-const name actually collides across
                     * units.  Singletons (e.g. System.MemoryManager) keep a
                     * NULL mangled_id so find_label resolves their bare
                     * `MemoryManager` symbol — needed for C-runtime linkage
                     * (KGPC/runtime_string.c references the bare name). */
                    if (registered->is_typed_const &&
                        registered->defined_in_unit &&
                        registered->source_unit_index > 0 &&
                        registered->mangled_id == NULL &&
                        codegen_typed_const_name_collides_ctx(ctx,
                            (const char *)id_node->cur))
                    {
                        registered->mangled_id = codegen_make_unit_qualified_key(
                            registered->source_unit_index,
                            (const char *)id_node->cur);
                    }
                }
            }

            if (is_param)
            {
                HashNode_t *param_node = FindIdentInCurrentScope(symtab, id_node->cur);
                if (param_node != NULL)
                {
                    int is_var_param = 0;
                    int is_untyped_param = 0;
                    if (decl->type == TREE_VAR_DECL)
                    {
                        is_var_param = decl->tree_data.var_decl_data.is_var_param;
                        is_untyped_param = decl->tree_data.var_decl_data.is_untyped_param;
                    }
                    param_node->is_var_parameter = (is_var_param || is_untyped_param) ? 1 : 0;
                }
            }
        }

        codegen_register_inline_var_enum_literals(decl, symtab);

        if (decl_type != NULL)
            destroy_kgpc_type(decl_type);
    }
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

            {
                char tmpl[64];
                snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n",
                    ctx->static_link_spill_slot->offset);
                Register_t *defs_arr[] = {reloaded};
                *inst_list = add_inst_du(*inst_list, ctx, defs_arr, 1, NULL, 0, tmpl);
            }

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
        reg = get_reg_with_spill(get_reg_stack(), inst_list);
    if (reg == NULL)
    {
        free(offsets);
        return NULL;
    }

    {
        char tmpl[128];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%rbp), %%0\n", offsets[0]);
        Register_t *defs_arr[] = {reg};
        *inst_list = add_inst_du(*inst_list, ctx, defs_arr, 1, NULL, 0, tmpl);
    }

    for (int i = 1; i < levels_to_traverse; ++i)
    {
        char tmpl[128];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t-%d(%%0), %%0\n", offsets[i]);
        Register_t *du_arr[] = {reg};
        *inst_list = add_inst_du(*inst_list, ctx, du_arr, 1, du_arr, 1, tmpl);
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

void codegen_report_warning(const CodeGenContext *ctx, const char *fmt, ...)
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
    (void)ctx;
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
    if (ctx->except_frames != NULL)
    {
        for (int i = 0; i < ctx->except_depth; ++i)
        {
            free(ctx->except_frames[i].label);
            ctx->except_frames[i].label = NULL;
        }
        free(ctx->except_frames);
        ctx->except_frames = NULL;
    }
    ctx->except_depth = 0;
    ctx->except_capacity = 0;
}

static void codegen_reset_with_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->with_stack != NULL)
    {
        free(ctx->with_stack);
        ctx->with_stack = NULL;
    }
    ctx->with_depth = 0;
    ctx->with_capacity = 0;
}

static void codegen_reset_loop_stack(CodeGenContext *ctx)
{
    if (ctx == NULL)
        return;
    if (ctx->loop_frames != NULL)
    {
        for (int i = 0; i < ctx->loop_depth; ++i)
        {
            free(ctx->loop_frames[i].label);
            ctx->loop_frames[i].label = NULL;
            free(ctx->loop_frames[i].continue_label);
            ctx->loop_frames[i].continue_label = NULL;
        }
        free(ctx->loop_frames);
        ctx->loop_frames = NULL;
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
        return kgpc_type_to_string(node->type);

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
    codegen_emit_semantic_scope_comments(out, ctx->symtab->builtin_scope->table, "builtins", 0);

    ScopeNode *scope = ctx->symtab->current_scope;
    int depth = 0;
    while (scope != NULL)
    {
        HashTable_t *table = scope->table;
        char label[32];
        snprintf(label, sizeof(label), "scope %d", depth);
        codegen_emit_semantic_scope_comments(out, table, label, 0);
        scope = scope->parent;
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

void codegen_sanitize_identifier_for_label(const char *value, char *buffer, size_t size)
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
    snprintf(buffer, sizeof(buffer), "__kgpc_program_var_%s_%d",
        sanitized, ++ctx->global_data_counter);
    return strdup(buffer);
}

/* Build a unit-qualified Pascal storage key for a unit-defined global.
 * Returns a newly-allocated string of the form "<unit_name>_$_<bare_id>" when
 * the variable belongs to a named unit, or NULL when it should be registered
 * under its bare name (program-scope vars, classvars, parameters, etc.).
 *
 * The qualified key is used as the StackNode label so that two units which
 * export the same Pascal identifier (e.g. each FPC charmap unit's
 * `unicodemap` typed-const) end up with distinct stack entries even though
 * the assembler-level static_label remains unique (a counter-suffixed
 * __kgpc_program_var_*).  Without this, the FLAT stack manager would alias
 * every unit's `unicodemap` to the first one registered, making FPC's
 * `mappings` linked list self-referencing. */
char *codegen_make_unit_qualified_key(int source_unit_index, const char *bare_id)
{
    if (bare_id == NULL || source_unit_index <= 0)
        return NULL;
    const char *unit_name = unit_registry_get(source_unit_index);
    if (unit_name == NULL || unit_name[0] == '\0')
        return NULL;
    size_t len = strlen(unit_name) + 3 /* "_$_" */ + strlen(bare_id) + 1;
    char *buf = (char *)malloc(len);
    if (buf == NULL)
        return NULL;
    snprintf(buf, len, "%s_$_%s", unit_name, bare_id);
    return buf;
}

/* Return 1 if more than one typed-const named `bare_id` is declared
 * across any of the loaded unit scopes.  Walks `unit_scopes[]` directly
 * (not the scope-tree chain from current_scope) because the colliding
 * typed-consts may be in sibling unit scopes that aren't reachable via
 * the current scope's dep chain.  Used to decide whether storage for
 * that name needs unit qualification — singletons like
 * System.MemoryManager keep their bare-name alias for C-runtime linkage,
 * while colliding typed-consts get per-unit qualified storage. */
/* Scan a decl list for a typed-const named `bare_id`.  Used by the
 * collision detector below — operates directly on the AST so it doesn't
 * depend on HashNode flags being set consistently across semcheck and
 * codegen passes. */
static int codegen_decl_list_has_typed_const_named(ListNode_t *decls, const char *bare_id)
{
    for (ListNode_t *cur = decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL)
            continue;
        ListNode_t *ids = NULL;
        int is_typed_const = 0;
        if (decl->type == TREE_VAR_DECL)
        {
            ids = decl->tree_data.var_decl_data.ids;
            is_typed_const = decl->tree_data.var_decl_data.is_typed_const;
        }
        else if (decl->type == TREE_ARR_DECL)
        {
            ids = decl->tree_data.arr_decl_data.ids;
            is_typed_const = decl->tree_data.arr_decl_data.is_typed_const;
        }
        if (!is_typed_const)
            continue;
        for (ListNode_t *id = ids; id != NULL; id = id->next)
        {
            if (id->cur != NULL &&
                pascal_identifier_equals((const char *)id->cur, bare_id))
                return 1;
        }
    }
    return 0;
}

/* Return 1 if more than one loaded unit declares a typed-const named
 * `bare_id`.  Walks `comp_ctx->loaded_units[]` directly so it doesn't
 * depend on HashNode flag propagation (which can be inconsistent between
 * semcheck and codegen registration passes).  Used to decide whether
 * storage for that name needs unit qualification — singletons like
 * System.MemoryManager keep their bare-name alias for C-runtime linkage,
 * while colliding typed-consts (each FPC charmap unit's `unicodemap`)
 * get per-unit qualified storage. */
static int codegen_typed_const_name_collides_ctx(CodeGenContext *ctx, const char *bare_id)
{
    if (ctx == NULL || ctx->comp_ctx == NULL ||
        bare_id == NULL || bare_id[0] == '\0')
        return 0;
    int count = 0;
    for (int i = 0; i < ctx->comp_ctx->loaded_unit_count && count < 2; ++i)
    {
        Tree_t *unit = ctx->comp_ctx->loaded_units[i].unit_tree;
        if (unit == NULL || unit->type != TREE_UNIT)
            continue;
        if (codegen_decl_list_has_typed_const_named(
                unit->tree_data.unit_data.interface_var_decls, bare_id) ||
            codegen_decl_list_has_typed_const_named(
                unit->tree_data.unit_data.implementation_var_decls, bare_id))
        {
            count++;
        }
    }
    return count > 1;
}


/* Resolve the StackNode storage key for a var decl.  Typed-consts declared
 * in a unit get the "<unit>_$_<name>" qualification ONLY when there is an
 * actual cross-unit name collision (each FPC charmap unit's `unicodemap`,
 * etc.).  Singletons like System.MemoryManager keep their bare key so the
 * KGPC C runtime can link against the bare name.  Plain `var` declarations
 * always keep their bare key. */
static char *codegen_var_storage_key(CodeGenContext *ctx, SymTab_t *symtab,
    Tree_t *decl_tree, const char *bare_id)
{
    (void)symtab;
    if (bare_id == NULL)
        return NULL;
    int src_unit = 0;
    int defined_in_unit = 0;
    int is_typed_const = 0;
    if (decl_tree != NULL)
    {
        if (decl_tree->type == TREE_VAR_DECL)
        {
            src_unit = decl_tree->tree_data.var_decl_data.source_unit_index;
            defined_in_unit = decl_tree->tree_data.var_decl_data.defined_in_unit;
            is_typed_const = decl_tree->tree_data.var_decl_data.is_typed_const;
        }
        else if (decl_tree->type == TREE_ARR_DECL)
        {
            src_unit = decl_tree->tree_data.arr_decl_data.source_unit_index;
            defined_in_unit = decl_tree->tree_data.arr_decl_data.defined_in_unit;
            is_typed_const = decl_tree->tree_data.arr_decl_data.is_typed_const;
        }
    }
    if (is_typed_const && defined_in_unit && src_unit > 0 &&
        codegen_typed_const_name_collides_ctx(ctx, bare_id))
    {
        char *key = codegen_make_unit_qualified_key(src_unit, bare_id);
        if (key != NULL)
            return key;
    }
    return strdup(bare_id);
}

/* Emit either a .bss allocation with a bare-name alias (when the variable's
 * user-facing symbol differs from its internal label, e.g. unit variables)
 * or a simple .comm directive.  Using .bss instead of .comm is required
 * because .set cannot target .comm symbols on most assemblers.
 *
 *   sym   – the user-facing symbol name (e.g. "MyVar")
 *   label – the internal storage label (e.g. "__kgpc_program_var_MyVar_1")
 *   size  – allocation size in bytes
 *   alignment – required alignment
 *   defined_in_unit – non-zero when the var comes from a unit (needs alias)
 */
static void codegen_emit_bss_or_comm(FILE *out, const char *sym, const char *label,
                                     int size, int alignment, int defined_in_unit)
{
    int need_alias = (label != NULL && sym != NULL &&
                      defined_in_unit && strcmp(sym, label) != 0);

    if (need_alias) {
        if (codegen_target_is_windows()) {
            fprintf(out, "\t.section .bss\n");
        } else {
            fprintf(out, "\t.pushsection .bss\n");
        }
        if (alignment > 0)
            fprintf(out, "\t.align\t%d\n", alignment);
        fprintf(out, ".globl\t%s\n", label);
        fprintf(out, "%s:\n", label);
        fprintf(out, "\t.zero\t%d\n", size);
        fprintf(out, ".globl\t%s\n", sym);
        fprintf(out, "\t.set\t%s, %s\n", sym, label);
        if (codegen_target_is_windows()) {
            fprintf(out, "\t.section .text\n");
        } else {
            fprintf(out, "\t.popsection\n");
        }
    } else {
        const char *effective = label != NULL ? label : sym;
        if (effective == NULL)
            return;
        if (alignment > 0)
            fprintf(out, "\t.comm\t%s,%d,%d\n", effective, size, alignment);
        else
            fprintf(out, "\t.comm\t%s,%d\n", effective, size);
    }
}

void codegen_common_enum_typeinfo_label(const char *type_id, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    buffer[0] = '\0';
    if (type_id == NULL || type_id[0] == '\0')
        return;

    char sanitized[CODEGEN_MAX_INST_BUF];
    codegen_sanitize_identifier_for_label(type_id, sanitized, sizeof(sanitized));
    {
        const char *prefix = "__kgpc_enum_typeinfo_";
        snprintf(buffer, size, "%s%.*s", prefix,
            (int)((size > strlen(prefix) + 1) ? (size - strlen(prefix) - 1) : 0),
            sanitized);
    }
}

void codegen_common_record_typeinfo_label(const char *type_id, char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    buffer[0] = '\0';
    if (type_id == NULL || type_id[0] == '\0')
        return;

    /* Use type_id directly without sanitization — dots are valid in
     * assembly labels and the TYPEINFO label emission uses dots for
     * nested types (e.g. "tcgprocinfo.ttempinfo_flags_entry"). */
    snprintf(buffer, size, "%s_TYPEINFO", type_id);
}

void codegen_common_typeinfo_label_for_type_id(SymTab_t *symtab, const char *type_id,
    char *buffer, size_t size)
{
    if (buffer == NULL || size == 0)
        return;

    buffer[0] = '\0';
    if (type_id == NULL || type_id[0] == '\0')
        return;

    HashNode_t *type_node = NULL;
    if (symtab != NULL &&
        FindSymbol(&type_node, symtab, type_id) != 0 &&
        type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
    {
        struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_enum)
        {
            codegen_common_enum_typeinfo_label(type_id, buffer, size);
            return;
        }

        {
            struct RecordType *record = hashnode_get_record_type(type_node);
            if (record != NULL)
            {
                /* Use record->type_id when available — for nested types the
                 * type_id is the full qualified name (e.g. "tcgprocinfo.ttempinfo_flags_entry")
                 * which matches how the label is emitted.  Fall back to the
                 * caller's type_id if record->type_id is not set. */
                const char *label_name = (record->type_id != NULL) ?
                    record->type_id : type_id;
                codegen_common_record_typeinfo_label(label_name, buffer, size);
                return;
            }
        }
    }

    codegen_common_enum_typeinfo_label(type_id, buffer, size);
}

static void codegen_emit_enum_typeinfo_for_alias(CodeGenContext *ctx, const char *type_name,
    struct TypeAlias *alias)
{
    if (ctx == NULL || ctx->output_file == NULL || type_name == NULL || alias == NULL)
        return;
    if (!alias->is_enum || alias->enum_literals == NULL)
        return;

    int count = ListLength(alias->enum_literals);
    if (count <= 0)
        return;

    char type_label[CODEGEN_MAX_INST_BUF];
    codegen_sanitize_identifier_for_label(type_name, type_label, sizeof(type_label));

    size_t typeinfo_len = strlen(type_label) + strlen("__kgpc_enum_typeinfo_") + 1;
    char *typeinfo_label = (char *)malloc(typeinfo_len);
    if (typeinfo_label == NULL)
        return;
    snprintf(typeinfo_label, typeinfo_len, "__kgpc_enum_typeinfo_%s", type_label);

    fprintf(ctx->output_file, "\n# Enum RTTI for %s\n", type_name);
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s\n", typeinfo_label);
    fprintf(ctx->output_file, "%s:\n", typeinfo_label);

    /* Emit enum TypeInfo/TTypeData in the layout KGPC's current FPC RTL
     * TypInfo build expects:
     *   Kind: Byte = tkEnumeration (3)
     *   Name: ShortString
     *   AttributeTable: Pointer = nil
     *   OrdType: Byte = otULong (5) for default zero-based enums
     *   MinValue, MaxValue: LongInt
     *   BaseType: PPTypeInfo (nil)
     *   NameList: packed shortstrings for each literal, followed by unit/type owner
     * This keeps KGPC-emitted enum RTTI aligned with the TypInfo offsets
     * produced by KGPC's own FPC RTL build. */
    {
        char escaped_type_name[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_type_name, type_name, sizeof(escaped_type_name));
        fprintf(ctx->output_file, "\t.byte\t3,%zu\n", strlen(type_name));
        if (type_name[0] != '\0')
            fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_type_name);
        fprintf(ctx->output_file, "\t.quad\t0\n");
        fprintf(ctx->output_file, "\t.byte\t5\n");
        fprintf(ctx->output_file, "\t.long\t0,%d\n", count - 1);
        fprintf(ctx->output_file, "\t.quad\t0\n");
    }

    for (ListNode_t *lit = alias->enum_literals; lit != NULL; lit = lit->next)
    {
        const char *literal = (lit->cur != NULL) ? (const char *)lit->cur : "";
        char escaped_literal[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_literal, literal, sizeof(escaped_literal));
        fprintf(ctx->output_file, "\t.byte\t%zu\n", strlen(literal));
        if (literal[0] != '\0')
            fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_literal);
    }

    {
        char escaped_owner_name[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_owner_name, type_name, sizeof(escaped_owner_name));
        fprintf(ctx->output_file, "\t.byte\t%zu\n", strlen(type_name));
        if (type_name[0] != '\0')
            fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_owner_name);
        fprintf(ctx->output_file, "\t.byte\t0\n");
    }
    free(typeinfo_label);
}

static void codegen_emit_enum_typeinfo_from_table(CodeGenContext *ctx, HashTable_t *table,
    int emit_unit_types, const char **emitted_labels, int *emitted_count, int *emitted_any)
{
    if (ctx == NULL || table == NULL)
        return;

    for (int i = 0; i < TABLE_SIZE; ++i)
    {
        ListNode_t *entry = table->table[i];
        while (entry != NULL)
        {
            HashNode_t *node = (HashNode_t *)entry->cur;
            entry = entry->next;
            if (node == NULL || node->hash_type != HASHTYPE_TYPE)
                continue;

            if (emit_unit_types)
            {
                if (!node->defined_in_unit)
                    continue;
            }
            else
            {
                if (node->defined_in_unit)
                    continue;
            }

            if (node->type == NULL)
                continue;

            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias == NULL || !alias->is_enum || alias->enum_literals == NULL)
                continue;

            const char *type_name = (alias->alias_name != NULL) ? alias->alias_name : node->id;
            if (type_name == NULL || type_name[0] == '\0')
                continue;

            char type_label[CODEGEN_MAX_INST_BUF];
            codegen_sanitize_identifier_for_label(type_name, type_label, sizeof(type_label));
            size_t label_len = strlen(type_label) + strlen("__kgpc_enum_typeinfo_") + 1;
            char *label = (char *)malloc(label_len);
            if (label == NULL)
                continue;
            snprintf(label, label_len, "__kgpc_enum_typeinfo_%s", type_label);

            int already_emitted = 0;
            for (int idx = 0; idx < *emitted_count; ++idx)
            {
                if (emitted_labels[idx] != NULL && strcmp(emitted_labels[idx], label) == 0)
                {
                    already_emitted = 1;
                    break;
                }
            }
            if (already_emitted)
            {
                free(label);
                continue;
            }

            if (*emitted_count < 512)
            {
                emitted_labels[*emitted_count] = strdup(label);
                if (emitted_labels[*emitted_count] != NULL)
                    (*emitted_count)++;
            }
            free(label);

            if (emitted_any != NULL && !(*emitted_any))
            {
                fprintf(ctx->output_file, "\n# Enum RTTI metadata\n");
                fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
                *emitted_any = 1;
            }

            codegen_emit_enum_typeinfo_for_alias(ctx, type_name, alias);
        }
    }
}

static void codegen_emit_enum_typeinfo(CodeGenContext *ctx, SymTab_t *symtab, int emit_unit_types)
{
    if (ctx == NULL || symtab == NULL)
        return;

    const char *emitted_labels[512];
    int emitted_count = 0;
    int emitted_any = 0;
    for (int i = 0; i < 512; ++i)
        emitted_labels[i] = NULL;

    if (symtab->builtin_scope->table != NULL)
        codegen_emit_enum_typeinfo_from_table(ctx, symtab->builtin_scope->table, emit_unit_types,
            emitted_labels, &emitted_count, &emitted_any);

    ScopeNode *scope = symtab->current_scope;
    while (scope != NULL)
    {
        HashTable_t *table = scope->table;
        codegen_emit_enum_typeinfo_from_table(ctx, table, emit_unit_types,
            emitted_labels, &emitted_count, &emitted_any);
        scope = scope->parent;
    }

    if (emitted_any)
        fprintf(ctx->output_file, "%s\n", codegen_text_section_resume());

    for (int i = 0; i < emitted_count; ++i)
        free((void *)emitted_labels[i]);
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
/* Tail pointer for O(1) add_inst append.
 * Tracks the (head, tail) of the last inst_list built by add_inst.
 * When the same head is passed and the cached tail's ->next is still NULL,
 * we append in O(1) instead of walking the entire list (O(n)).
 * MUST be invalidated before free_inst_list and ConcatList. */
static ListNode_t *g_inst_tail = NULL;
static ListNode_t *g_inst_head = NULL;

void add_inst_invalidate_cache(void)
{
    g_inst_tail = NULL;
    g_inst_head = NULL;
}

ListNode_t *add_inst(ListNode_t *inst_list, const char *inst)
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
    else if (g_inst_head == inst_list && g_inst_tail != NULL && g_inst_tail->next == NULL)
    {
        /* Fast path: cached tail is valid, O(1) append */
        g_inst_tail->next = new_node;
    }
    else
    {
        /* Slow path: walk to end */
        PushListNodeBack(inst_list, new_node);
    }
    g_inst_head = inst_list;
    g_inst_tail = new_node;

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return inst_list;
}

/* add_inst_du — emit an instruction template with def/use metadata.
 *
 * fmt is a template string where %0, %1, ... are placeholders for
 * the physical register names of the def/use registers.  Placeholders
 * are substituted by ir_emit_function() before code emission.
 *
 * defs[0..n_defs-1] are written by this instruction.
 * uses[0..n_uses-1] are read by this instruction.
 * vreg_ids[] is filled in defs-first order: [defs[0], ..., uses[0], ...].
 * Each register without an assigned vreg_id (vreg_id == -1) receives a
 * fresh ID from ctx->next_vreg_id++.
 *
 * The trailing ... is accepted but ignored (kept for signature compatibility). */
ListNode_t *add_inst_du(ListNode_t *inst_list, CodeGenContext *ctx,
                        Register_t **defs, int n_defs,
                        Register_t **uses, int n_uses,
                        const char *fmt, ...)
{
    IrInst_t *inst = ir_inst_new(NULL, defs, n_defs, uses, n_uses);
    if (inst == NULL)
        return inst_list;

    /* Store template string */
    inst->tmpl = fmt ? strdup(fmt) : NULL;
    inst->text = NULL;

    /* Assign vreg_ids: defs first, then uses.
     * If ctx is available, assign fresh IDs to unassigned registers.
     * Also copy physical register names (bit_64/bit_32) into the instruction
     * so that ir_emit_function() can resolve placeholder names without
     * dereferencing the borrowed Register_t pointers (which may be freed by
     * reset_reg_stack() when nested subprograms are codegen'd before
     * ir_emit_function() is called on the outer function). */
    int placeholder = 0;
    for (int i = 0; i < n_defs && i < IR_MAX_DEFS && placeholder < (int)(sizeof(inst->vreg_ids)/sizeof(inst->vreg_ids[0])); ++i, ++placeholder)
    {
        if (defs[i] != NULL)
        {
            if (ctx != NULL && defs[i]->vreg_id == -1)
                defs[i]->vreg_id = ctx->next_vreg_id++;
            inst->vreg_ids[placeholder] = defs[i]->vreg_id;
            if (defs[i]->bit_64) snprintf(inst->reg_names_64[placeholder], IR_REG_NAME_BUF, "%s", defs[i]->bit_64);
            else inst->reg_names_64[placeholder][0] = '\0';
            if (defs[i]->bit_32) snprintf(inst->reg_names_32[placeholder], IR_REG_NAME_BUF, "%s", defs[i]->bit_32);
            else inst->reg_names_32[placeholder][0] = '\0';
        }
    }
    for (int i = 0; i < n_uses && i < IR_MAX_USES && placeholder < (int)(sizeof(inst->vreg_ids)/sizeof(inst->vreg_ids[0])); ++i, ++placeholder)
    {
        if (uses[i] != NULL)
        {
            if (ctx != NULL && uses[i]->vreg_id == -1)
                uses[i]->vreg_id = ctx->next_vreg_id++;
            inst->vreg_ids[placeholder] = uses[i]->vreg_id;
            if (uses[i]->bit_64) snprintf(inst->reg_names_64[placeholder], IR_REG_NAME_BUF, "%s", uses[i]->bit_64);
            else inst->reg_names_64[placeholder][0] = '\0';
            if (uses[i]->bit_32) snprintf(inst->reg_names_32[placeholder], IR_REG_NAME_BUF, "%s", uses[i]->bit_32);
            else inst->reg_names_32[placeholder][0] = '\0';
        }
    }
    inst->n_placeholders = placeholder;

    ListNode_t *new_node = CreateListNode(inst, LIST_IR_INST);
    if (new_node == NULL)
    {
        ir_inst_free(inst);
        return inst_list;
    }

    if (inst_list == NULL)
    {
        inst_list = new_node;
    }
    else if (g_inst_head == inst_list && g_inst_tail != NULL && g_inst_tail->next == NULL)
    {
        g_inst_tail->next = new_node;
    }
    else
    {
        PushListNodeBack(inst_list, new_node);
    }
    g_inst_head = inst_list;
    g_inst_tail = new_node;

    return inst_list;
}


ListNode_t *codegen_emit_interface_vtable_slot_init(ListNode_t *inst_list,
    CodeGenContext *ctx, const struct RecordType *class_record,
    const char *class_type_id, Register_t *instance_reg)
{
    if (ctx == NULL || class_record == NULL || class_type_id == NULL || instance_reg == NULL ||
        class_record->num_interfaces <= 0)
        return inst_list;

    long long base_size = 0;
    codegen_sizeof_record_type(ctx, (struct RecordType *)class_record, &base_size);

    int iface_count = 0;
    for (int ii = 0; ii < class_record->num_interfaces; ++ii)
    {
        if (class_record->interface_names[ii] != NULL)
            iface_count++;
    }
    if (iface_count <= 0)
        return inst_list;

    if (class_record->has_cached_size)
        base_size = class_record->cached_size - iface_count * 8;

    Register_t *ivtbl_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (ivtbl_reg == NULL)
        return inst_list;

    int slot_idx = 0;
    for (int ii = 0; ii < class_record->num_interfaces; ++ii)
    {
        if (class_record->interface_names[ii] == NULL)
            continue;
        long long offset = base_size + slot_idx * 8;
        {
            char tmpl[CODEGEN_MAX_INST_BUF];
            snprintf(tmpl, sizeof(tmpl), "\tleaq\t%s_INTF_%s_VTABLE(%%rip), %%0\n",
                class_type_id, class_record->interface_names[ii]);
            Register_t *defs_arr[] = {ivtbl_reg};
            inst_list = add_inst_du(inst_list, ctx, defs_arr, 1, NULL, 0, tmpl);
        }
        {
            char tmpl[CODEGEN_MAX_INST_BUF];
            snprintf(tmpl, sizeof(tmpl), "\tmovq\t%%0, %lld(%%1)\n", offset);
            Register_t *uses_arr[] = {ivtbl_reg, instance_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, uses_arr, 2, tmpl);
        }
        slot_idx++;
    }

    free_reg(get_reg_stack(), ivtbl_reg);
    return inst_list;
}

ListNode_t *codegen_emit_interface_dispatch(ListNode_t *inst_list,
    CodeGenContext *ctx, const char *self_reg, const char *iface_name,
    int vmt_index, const char *label_prefix, const char *target_slot_label,
    int preserve_indirect_call_regs, CodegenCallArgSpillFn spill_fn,
    CodegenCallArgRestoreFn restore_fn)
{
    if (ctx == NULL || self_reg == NULL || label_prefix == NULL || target_slot_label == NULL)
        return inst_list;

    char buffer[CODEGEN_MAX_INST_BUF];
    int label_id = ++ctx->label_counter;

    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r11\n", self_reg);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\tmovq\t(%r11), %r11\n");
    inst_list = add_inst(inst_list, "\tmovq\t(%r11), %rax\n");
    inst_list = add_inst(inst_list, "\taddq\t8(%r11), %rax\n");
    snprintf(buffer, sizeof(buffer), "\tjz\t.L%s_direct_%d\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n", vmt_index * 8);
    inst_list = add_inst(inst_list, buffer);
    if (preserve_indirect_call_regs)
    {
        CallerSaveState caller_state;
        regstack_caller_save(get_reg_stack(), &inst_list, &caller_state);
        inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
        regstack_caller_restore(get_reg_stack(), &inst_list, &caller_state);
    }
    else
    {
        inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
    }
    snprintf(buffer, sizeof(buffer), "\tjmp\t.L%s_done_%d\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);

    snprintf(buffer, sizeof(buffer), ".L%s_direct_%d:\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);

    StackNode_t *target_slot = add_l_t_bytes((char *)target_slot_label, 8);
    if (iface_name != NULL && iface_name[0] != '\0' && target_slot != NULL &&
        spill_fn != NULL && restore_fn != NULL)
    {
        char guid_label[640];
        int arg_spills[6] = {0};
        int xmm_spills[8] = {0};
        snprintf(guid_label, sizeof(guid_label), "__kgpc_guid_%s", iface_name);
        inst_list = spill_fn(inst_list, arg_spills, xmm_spills);
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", self_reg, current_arg_reg64(0));
        inst_list = add_inst(inst_list, buffer);
        {
            char guid_buffer[768];
            snprintf(guid_buffer, sizeof(guid_buffer), "\tleaq\t%s(%%rip), %s\n", guid_label, current_arg_reg64(1));
            inst_list = add_inst(inst_list, guid_buffer);
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", vmt_index, current_arg_reg32(2));
        inst_list = add_inst(inst_list, buffer);
        inst_list = add_inst(inst_list, "\tcall\t__kgpc_resolve_intf_method\n");
        snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n", target_slot->offset);
        inst_list = add_inst(inst_list, buffer);
        inst_list = restore_fn(inst_list, arg_spills, xmm_spills);
        if (preserve_indirect_call_regs)
        {
            CallerSaveState caller_state;
            regstack_caller_save(get_reg_stack(), &inst_list, &caller_state);
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n", target_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
            regstack_caller_restore(get_reg_stack(), &inst_list, &caller_state);
        }
        else
        {
            snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%r11\n", target_slot->offset);
            inst_list = add_inst(inst_list, buffer);
            inst_list = add_inst(inst_list, "\tcall\t*%r11\n");
        }
    }

    snprintf(buffer, sizeof(buffer), ".L%s_done_%d:\n", label_prefix, label_id);
    inst_list = add_inst(inst_list, buffer);
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

    /* Invalidate the tail cache — the nodes about to be freed
     * might include g_inst_tail. */
    add_inst_invalidate_cache();

    cur = inst_list;
    while(cur != NULL)
    {
        if (cur->type == LIST_IR_INST)
            ir_inst_free((IrInst_t *)cur->cur);
        else
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

        /* Unsigned variants: use jb/jbe/ja/jae instead of jl/jle/jg/jge */
        case LT_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "jae" : "jb");
            break;
        case LE_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "ja" : "jbe");
            break;
        case GT_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "jbe" : "ja");
            break;
        case GE_U:
            snprintf(jmp_buf, 6, inverse > 0 ? "jb" : "jae");
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

/* Forward declaration */
void codegen_function_header_ex_alias_vis(char *func_name, CodeGenContext *ctx, int nostackframe, const char *cname_override, int emit_weak);
void codegen_stack_space_for_inst_list(ListNode_t *inst_list, CodeGenContext *ctx);

/* Generates a function header.
 * If nostackframe is set, only emits the label without prologue (push %rbp / mov %rsp, %rbp).
 * If cname_override is set and differs from func_name, emits an additional .globl + label alias. */
void codegen_function_header_ex_alias(char *func_name, CodeGenContext *ctx, int nostackframe, const char *cname_override)
{
    codegen_function_header_ex_alias_vis(func_name, ctx, nostackframe, cname_override, 0);
}

/* Emit the function header.  Always uses .globl for symbol visibility. */
void codegen_function_header_ex_alias_vis(char *func_name, CodeGenContext *ctx, int nostackframe, const char *cname_override, int emit_weak)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_name != NULL);
    assert(ctx != NULL);
    codegen_emit_function_debug_comments(func_name, ctx);
    /* Emit per-function .text section when --function-sections is enabled,
     * allowing the linker's --gc-sections to strip unused functions. */
    if (function_sections_flag())
        fprintf(ctx->output_file, "\t.section\t.text.%s,\"ax\",@progbits\n", func_name);
    /* When using cached unit .o (skip-unit-codegen), program-level functions
     * (defined_in_unit == 0) must be local to avoid clashing with same-named
     * unit functions in the cache .o.  All program code is in the same .s,
     * so local binding still resolves correctly.  Unit functions in the cache
     * are .globl so the runtime can call them. */
    const char *vis = ".globl";
    if (skip_unit_codegen_flag() && emit_weak == 0)
        vis = NULL;
    /* Emit alias label from cname_override (e.g. [Public,Alias:'FPC_DO_EXIT']) */
    if (cname_override != NULL && strcmp(cname_override, func_name) != 0) {
        if (vis != NULL)
            fprintf(ctx->output_file, "%s\t%s\n", vis, cname_override);
        fprintf(ctx->output_file, "%s:\n", cname_override);
    }
    if (vis != NULL)
        fprintf(ctx->output_file, "%s\t%s\n", vis, func_name);
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_proc\t%s\n", func_name);
    if (nostackframe) {
        fprintf(ctx->output_file, "%s:\n", func_name);
    } else {
        fprintf(ctx->output_file, "%s:\n\tpushq\t%%rbp\n", func_name);
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_pushreg\t%%rbp\n");
        fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rbp\n");
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_setframe\t%%rbp, 0\n");
    }

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_function_header_ex(char *func_name, CodeGenContext *ctx, int nostackframe)
{
    codegen_function_header_ex_alias(func_name, ctx, nostackframe, NULL);
}

void codegen_function_header(char *func_name, CodeGenContext *ctx)
{
    codegen_function_header_ex(func_name, ctx, 0);
}

/* Generates a function footer.
 * If nostackframe is set, emits only ret without leave. */
void codegen_function_footer_ex(char *func_name, CodeGenContext *ctx, int nostackframe)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(func_name != NULL);
    assert(ctx != NULL);
    if (nostackframe) {
        fprintf(ctx->output_file, "\tret\n");
    } else {
        /* Restore callee-saved GP registers before leaving the frame */
        if (ctx->callee_save_rbx_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
        if (ctx->callee_save_r12_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
        if (ctx->callee_save_r13_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
        if (ctx->callee_save_r14_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
        if (ctx->callee_save_r15_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
        fprintf(ctx->output_file, "\tnop\n\tleave\n\tret\n");
    }
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_endproc\n");

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_function_footer(char *func_name, CodeGenContext *ctx)
{
    codegen_function_footer_ex(func_name, ctx, 0);
}


/* This is the entry function */
void codegen(Tree_t *tree, const char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab,
             CompilationContext *comp_ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    char *prgm_name;

    assert(tree != NULL);
    assert(input_file_name != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (ctx->target_abi != KGPC_TARGET_ABI_SYSTEM_V && ctx->target_abi != KGPC_TARGET_ABI_WINDOWS)
        ctx->target_abi = current_target_abi();

    g_current_codegen_abi = ctx->target_abi;
    g_stack_home_space_bytes = (ctx->target_abi == KGPC_TARGET_ABI_WINDOWS) ? 32 : 0;
    ctx->pending_stack_arg_bytes = 0;
    ctx->emitted_subprograms = NULL;
    ctx->comp_ctx = comp_ctx;
    g_codegen_available_subprograms = NULL;
    codegen_set_destroy(&g_available_subprograms_set);
    memset(&g_available_subprograms_set, 0, sizeof(g_available_subprograms_set));
    g_available_subprograms_tail = NULL;
    memset(&g_codegen_callable_exports, 0, sizeof(g_codegen_callable_exports));

    ctx->symtab = symtab;
    ctx->is_whole_program = 1;
    symtab->skip_unit_filter = 1;

    codegen_reset_finally_stack(ctx);
    codegen_reset_loop_stack(ctx);
    codegen_reset_except_stack(ctx);

    CODEGEN_DEBUG("DEBUG: ENTERING codegen\n");
    init_stackmng();

    codegen_program_header(input_file_name, ctx);

    /* Reset emitted-subprograms O(1) tracking for this compilation */
    codegen_reset_emitted_set();

    /* Detect and resolve cross-unit mangled name collisions (e.g.
     * comptty.IsATTY vs termio.IsATTY both producing "isatty_t").
     * Two passes: first collect all (mangled_id, unit) pairs, then
     * prefix only colliding names with "unit$$". */
    {
        CgCollisionMap collision_map;
        memset(&collision_map, 0, sizeof(collision_map));
        /* Pass 1: collect collisions from all units */
        if (comp_ctx != NULL) {
            for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
                Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
                if (unit != NULL && unit->type == TREE_UNIT)
                    codegen_collect_mangled_collisions(
                        unit->tree_data.unit_data.subprograms, &collision_map);
            }
        }
        /* Pass 2: apply prefixes to colliding names */
        if (comp_ctx != NULL) {
            for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
                Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
                if (unit != NULL && unit->type == TREE_UNIT)
                    codegen_apply_collision_prefixes(
                        unit->tree_data.unit_data.subprograms, symtab,
                        &collision_map);
            }
        }
        cg_collision_destroy(&collision_map);
    }

    /* Collect callable export names from loaded units, then program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT)
                codegen_collect_callable_export_names(unit->tree_data.unit_data.subprograms);
        }
    }
    codegen_collect_callable_export_names(tree->tree_data.program_data.subprograms);

    codegen_rodata(ctx, symtab);
    codegen_emit_enum_typeinfo(ctx, symtab, 0);

    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            codegen_mark_class_constructors_used(unit->tree_data.unit_data.interface_type_decls, symtab);
            codegen_mark_class_constructors_used(unit->tree_data.unit_data.implementation_type_decls, symtab);
        }
    }
    codegen_mark_class_constructors_used(tree->tree_data.program_data.type_declaration, symtab);

    /* Collect available subprogram labels from loaded units, then program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT)
                codegen_collect_available_subprogram_labels(unit->tree_data.unit_data.subprograms);
        }
    }
    codegen_collect_available_subprogram_labels(tree->tree_data.program_data.subprograms);

    codegen_vmt(ctx, symtab, tree, comp_ctx);

    prgm_name = codegen_program(tree, ctx, symtab, comp_ctx);

    codegen_main(prgm_name, ctx);

    codegen_program_footer(ctx);

    if (ctx->emitted_subprograms != NULL)
    {
        DestroyList(ctx->emitted_subprograms);
        ctx->emitted_subprograms = NULL;
    }
    if (g_codegen_available_subprograms != NULL)
    {
        DestroyList(g_codegen_available_subprograms);
        g_codegen_available_subprograms = NULL;
    codegen_set_destroy(&g_available_subprograms_set);
    memset(&g_available_subprograms_set, 0, sizeof(g_available_subprograms_set));
    g_available_subprograms_tail = NULL;
    }
    codegen_set_destroy(&g_codegen_callable_exports);

    free_stackmng();
    codegen_reset_loop_stack(ctx);
    codegen_reset_finally_stack(ctx);
    codegen_reset_except_stack(ctx);
    codegen_reset_with_stack(ctx);

    CODEGEN_DEBUG("DEBUG: LEAVING codegen\n");
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
    return;
}

void codegen_unit(Tree_t *tree, const char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(tree != NULL);
    assert(tree->type == TREE_UNIT);
    assert(input_file_name != NULL);
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (ctx->target_abi != KGPC_TARGET_ABI_SYSTEM_V && ctx->target_abi != KGPC_TARGET_ABI_WINDOWS)
        ctx->target_abi = current_target_abi();

    g_current_codegen_abi = ctx->target_abi;
    g_stack_home_space_bytes = (ctx->target_abi == KGPC_TARGET_ABI_WINDOWS) ? 32 : 0;
    ctx->pending_stack_arg_bytes = 0;
    ctx->emitted_subprograms = NULL;
    g_codegen_available_subprograms = NULL;
    codegen_set_destroy(&g_available_subprograms_set);
    memset(&g_available_subprograms_set, 0, sizeof(g_available_subprograms_set));
    g_available_subprograms_tail = NULL;
    memset(&g_codegen_callable_exports, 0, sizeof(g_codegen_callable_exports));

    ctx->symtab = symtab;
    ctx->is_whole_program = 0;
    symtab->skip_unit_filter = 1;

    codegen_reset_finally_stack(ctx);
    codegen_reset_loop_stack(ctx);
    codegen_reset_except_stack(ctx);

    init_stackmng();

    codegen_program_header(input_file_name, ctx);
    /* No collision detection needed for single-unit codegen — collisions
     * only occur when multiple units are merged in program codegen. */
    codegen_collect_callable_export_names(tree->tree_data.unit_data.subprograms);
    codegen_mark_class_constructors_used(tree->tree_data.unit_data.interface_type_decls, symtab);
    codegen_mark_class_constructors_used(tree->tree_data.unit_data.implementation_type_decls, symtab);
    codegen_rodata(ctx, symtab);
    codegen_emit_enum_typeinfo(ctx, symtab, 1);
    codegen_collect_available_subprogram_labels(tree->tree_data.unit_data.subprograms);
    codegen_vmt(ctx, symtab, tree, NULL);

    /* Generate code for unit subprograms */
    codegen_subprograms(tree->tree_data.unit_data.subprograms, ctx, symtab);

    /* Generate initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL)
    {
        char *unit_id = tree->tree_data.unit_data.unit_id;
        char init_label[CODEGEN_LABEL_BUFFER_SIZE];
        snprintf(init_label, sizeof(init_label), "_UNIT_%s_INIT", unit_id ? unit_id : "UNKNOWN");

        int prev_callee_rbx = ctx->callee_save_rbx_offset;
        int prev_callee_r12 = ctx->callee_save_r12_offset;
        int prev_callee_r13 = ctx->callee_save_r13_offset;
        int prev_callee_r14 = ctx->callee_save_r14_offset;
        int prev_callee_r15 = ctx->callee_save_r15_offset;
        push_stackscope();
        codegen_function_locals(tree->tree_data.unit_data.interface_var_decls, ctx, symtab);
        codegen_function_locals(tree->tree_data.unit_data.implementation_var_decls, ctx, symtab);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.interface_const_decls);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.implementation_const_decls);
        {
            StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
            StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
            StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
            StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
            StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
            ctx->callee_save_rbx_offset = rbx_slot->offset;
            ctx->callee_save_r12_offset = r12_slot->offset;
            ctx->callee_save_r13_offset = r13_slot->offset;
            ctx->callee_save_r14_offset = r14_slot->offset;
            ctx->callee_save_r15_offset = r15_slot->offset;
        }
        reset_reg_stack();
        codegen_invalidate_static_link_cache(ctx);
        ListNode_t *inst_list = NULL;
        ctx->next_vreg_id = 0;
        inst_list = codegen_stmt(tree->tree_data.unit_data.initialization, inst_list, ctx, symtab);

        fprintf(ctx->output_file, "\t.globl\t%s\n", init_label);
        fprintf(ctx->output_file, "%s:\n", init_label);
        fprintf(ctx->output_file, "\tpushq\t%%rbp\n");
        fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rbp\n");
        codegen_stack_space_for_inst_list(inst_list, ctx);
#if USE_GRAPH_COLORING_ALLOCATOR
        ir_liveness_allocate(inst_list);
#endif
        ir_emit_function(inst_list);
#if USE_GRAPH_COLORING_ALLOCATOR
        ir_peephole_remove_redundant_moves(&inst_list);
#endif
        codegen_inst_list(inst_list, ctx);
        if (ctx->callee_save_rbx_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
        if (ctx->callee_save_r12_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
        if (ctx->callee_save_r13_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
        if (ctx->callee_save_r14_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
        if (ctx->callee_save_r15_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
        fprintf(ctx->output_file, "\tleave\n");
        fprintf(ctx->output_file, "\tret\n");

        if (dump_ir_flag())
            ir_print_function(stderr, init_label, inst_list);
        if (dump_ir_cfg_flag())
        {
            Cfg_t *cfg = cfg_build(inst_list);
            cfg_print(stderr, cfg, init_label);
            cfg_free(cfg);
        }
        if (dump_ir_liveness_flag())
        {
            Cfg_t *cfg = cfg_build(inst_list);
            LivenessInfo_t *liveness = liveness_compute(cfg);
            liveness_print(stderr, cfg, liveness, init_label);
            liveness_free(liveness);
            cfg_free(cfg);
        }
        free_inst_list(inst_list);
        pop_stackscope();
        ctx->callee_save_rbx_offset = prev_callee_rbx;
        ctx->callee_save_r12_offset = prev_callee_r12;
        ctx->callee_save_r13_offset = prev_callee_r13;
        ctx->callee_save_r14_offset = prev_callee_r14;
        ctx->callee_save_r15_offset = prev_callee_r15;
    }

    /* Generate finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL)
    {
        char *unit_id = tree->tree_data.unit_data.unit_id;
        char final_label[CODEGEN_LABEL_BUFFER_SIZE];
        snprintf(final_label, sizeof(final_label), "_UNIT_%s_FINAL", unit_id ? unit_id : "UNKNOWN");

        int prev_callee_rbx = ctx->callee_save_rbx_offset;
        int prev_callee_r12 = ctx->callee_save_r12_offset;
        int prev_callee_r13 = ctx->callee_save_r13_offset;
        int prev_callee_r14 = ctx->callee_save_r14_offset;
        int prev_callee_r15 = ctx->callee_save_r15_offset;
        push_stackscope();
        codegen_function_locals(tree->tree_data.unit_data.interface_var_decls, ctx, symtab);
        codegen_function_locals(tree->tree_data.unit_data.implementation_var_decls, ctx, symtab);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.interface_const_decls);
        codegen_emit_const_decl_equivs_from_list(ctx,
            tree->tree_data.unit_data.implementation_const_decls);
        {
            StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
            StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
            StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
            StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
            StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
            ctx->callee_save_rbx_offset = rbx_slot->offset;
            ctx->callee_save_r12_offset = r12_slot->offset;
            ctx->callee_save_r13_offset = r13_slot->offset;
            ctx->callee_save_r14_offset = r14_slot->offset;
            ctx->callee_save_r15_offset = r15_slot->offset;
        }
        reset_reg_stack();
        codegen_invalidate_static_link_cache(ctx);
        ListNode_t *inst_list = NULL;
        ctx->next_vreg_id = 0;
        inst_list = codegen_stmt(tree->tree_data.unit_data.finalization, inst_list, ctx, symtab);

        fprintf(ctx->output_file, "\t.globl\t%s\n", final_label);
        fprintf(ctx->output_file, "%s:\n", final_label);
        fprintf(ctx->output_file, "\tpushq\t%%rbp\n");
        fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rbp\n");
        codegen_stack_space_for_inst_list(inst_list, ctx);
#if USE_GRAPH_COLORING_ALLOCATOR
        ir_liveness_allocate(inst_list);
#endif
        ir_emit_function(inst_list);
#if USE_GRAPH_COLORING_ALLOCATOR
        ir_peephole_remove_redundant_moves(&inst_list);
#endif
        codegen_inst_list(inst_list, ctx);
        if (ctx->callee_save_rbx_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%rbx\n", ctx->callee_save_rbx_offset);
        if (ctx->callee_save_r12_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r12\n", ctx->callee_save_r12_offset);
        if (ctx->callee_save_r13_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r13\n", ctx->callee_save_r13_offset);
        if (ctx->callee_save_r14_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r14\n", ctx->callee_save_r14_offset);
        if (ctx->callee_save_r15_offset > 0)
            fprintf(ctx->output_file, "\tmovq\t-%d(%%rbp), %%r15\n", ctx->callee_save_r15_offset);
        fprintf(ctx->output_file, "\tleave\n");
        fprintf(ctx->output_file, "\tret\n");

        if (dump_ir_flag())
            ir_print_function(stderr, final_label, inst_list);
        if (dump_ir_cfg_flag())
        {
            Cfg_t *cfg = cfg_build(inst_list);
            cfg_print(stderr, cfg, final_label);
            cfg_free(cfg);
        }
        if (dump_ir_liveness_flag())
        {
            Cfg_t *cfg = cfg_build(inst_list);
            LivenessInfo_t *liveness = liveness_compute(cfg);
            liveness_print(stderr, cfg, liveness, final_label);
            liveness_free(liveness);
            cfg_free(cfg);
        }
        free_inst_list(inst_list);
        pop_stackscope();
        ctx->callee_save_rbx_offset = prev_callee_rbx;
        ctx->callee_save_r12_offset = prev_callee_r12;
        ctx->callee_save_r13_offset = prev_callee_r13;
        ctx->callee_save_r14_offset = prev_callee_r14;
        ctx->callee_save_r15_offset = prev_callee_r15;
    }

    codegen_program_footer(ctx);

    if (ctx->emitted_subprograms != NULL)
    {
        DestroyList(ctx->emitted_subprograms);
        ctx->emitted_subprograms = NULL;
    }
    if (g_codegen_available_subprograms != NULL)
    {
        DestroyList(g_codegen_available_subprograms);
        g_codegen_available_subprograms = NULL;
    codegen_set_destroy(&g_available_subprograms_set);
    memset(&g_available_subprograms_set, 0, sizeof(g_available_subprograms_set));
    g_available_subprograms_tail = NULL;
    }
    codegen_set_destroy(&g_codegen_callable_exports);

    free_stackmng();
    codegen_reset_loop_stack(ctx);
    codegen_reset_finally_stack(ctx);
    codegen_reset_except_stack(ctx);
    codegen_reset_with_stack(ctx);
}

static int codegen_is_valid_asm_symbol_name(const char *id)
{
    if (id == NULL || id[0] == '\0')
        return 0;

    if (!(isalpha((unsigned char)id[0]) || id[0] == '_'))
        return 0;

    for (size_t i = 1; id[i] != '\0'; ++i)
    {
        if (!(isalnum((unsigned char)id[i]) || id[i] == '_'))
            return 0;
    }

    return 1;
}

static int codegen_const_symbol_emitted(ListNode_t *emitted_symbols, const char *id)
{
    ListNode_t *cur = emitted_symbols;
    while (cur != NULL)
    {
        if (cur->cur != NULL && strcmp((const char *)cur->cur, id) == 0)
            return 1;
        cur = cur->next;
    }
    return 0;
}

static int codegen_should_emit_const_equiv_symbol(CodeGenContext *ctx,
    HashTable_t *table, const HashNode_t *symbol, const char *id)
{
    (void)ctx;
    (void)table;
    (void)symbol;
    if (id == NULL || !codegen_is_valid_asm_symbol_name(id))
        return 0;

    if (codegen_set_contains_ci(&g_codegen_callable_exports, id))
        return 0;

    return 1;
}

static void codegen_emit_integer_const_equivs_from_table(CodeGenContext *ctx,
    HashTable_t *table, ListNode_t **emitted_symbols)
{
    assert(ctx != NULL);
    assert(emitted_symbols != NULL);

    if (table == NULL)
        return;

    for (int bucket = 0; bucket < TABLE_SIZE; ++bucket)
    {
        ListNode_t *bucket_node = table->table[bucket];
        while (bucket_node != NULL)
        {
            HashNode_t *symbol = (HashNode_t *)bucket_node->cur;
            if (symbol != NULL &&
                symbol->hash_type == HASHTYPE_CONST &&
                symbol->is_constant &&
                symbol->const_string_value == NULL &&
                symbol->const_set_value == NULL &&
                symbol->id != NULL &&
                codegen_should_emit_const_equiv_symbol(ctx, table, symbol, symbol->id) &&
                !codegen_const_symbol_emitted(*emitted_symbols, symbol->id))
            {
                int type_tag = codegen_tag_from_kgpc(symbol->type);
                if (is_ordinal_type(type_tag))
                {
                    fprintf(ctx->output_file, ".equ %s, %lld\n", symbol->id, symbol->const_int_value);
                    char *emitted_copy = strdup(symbol->id);
                    if (emitted_copy != NULL)
                    {
                        ListNode_t *new_node = CreateListNode(emitted_copy, LIST_STRING);
                        if (*emitted_symbols == NULL)
                            *emitted_symbols = new_node;
                        else
                            *emitted_symbols = PushListNodeBack(*emitted_symbols, new_node);
                    }
                }
            }
            bucket_node = bucket_node->next;
        }
    }
}

static void codegen_emit_integer_const_equivs(CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(ctx != NULL);
    assert(symtab != NULL);

    ListNode_t *emitted_symbols = NULL;

    /* Prefer user/global scopes first, then builtins. */
    ScopeNode *scope_node = symtab->current_scope;
    while (scope_node != NULL)
    {
        codegen_emit_integer_const_equivs_from_table(ctx, scope_node->table,
            &emitted_symbols);
        scope_node = scope_node->parent;
    }
    codegen_emit_integer_const_equivs_from_table(ctx, symtab->builtin_scope->table, &emitted_symbols);

    ListNode_t *cur = emitted_symbols;
    while (cur != NULL)
    {
        free(cur->cur);
        cur = cur->next;
    }
    DestroyList(emitted_symbols);
}

void codegen_emit_local_const_equivs(CodeGenContext *ctx, SymTab_t *symtab)
{
    assert(ctx != NULL);
    assert(symtab != NULL);

    if (symtab->current_scope == NULL || symtab->current_scope->table == NULL)
        return;

    ListNode_t *emitted_symbols = NULL;
    codegen_emit_integer_const_equivs_from_table(ctx, symtab->current_scope->table,
        &emitted_symbols);

    ListNode_t *cur = emitted_symbols;
    while (cur != NULL)
    {
        free(cur->cur);
        cur = cur->next;
    }
    DestroyList(emitted_symbols);
}

static int codegen_eval_const_expr(struct Expression *expr, SymTab_t *symtab, long long *out_value)
{
    if (expr == NULL || out_value == NULL)
        return 0;

    switch (expr->type)
    {
        case EXPR_INUM:
            *out_value = expr->expr_data.i_num;
            return 1;
        case EXPR_BOOL:
            *out_value = expr->expr_data.bool_value ? 1 : 0;
            return 1;
        case EXPR_CHAR_CODE:
            *out_value = (unsigned char)(expr->expr_data.char_code & 0xFF);
            return 1;
        case EXPR_VAR_ID:
            if (symtab != NULL && expr->expr_data.id != NULL)
            {
                HashNode_t *node = codegen_find_const_eval_symbol(symtab, expr->expr_data.id);
                if (node != NULL &&
                    node != NULL &&
                    (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
                {
                    int type_tag = codegen_tag_from_kgpc(node->type);
                    if (!is_ordinal_type(type_tag))
                        return 0;
                    *out_value = node->const_int_value;
                    return 1;
                }
            }
            return 0;
        case EXPR_SIGN_TERM:
            if (expr->expr_data.sign_term != NULL)
            {
                long long inner;
                if (codegen_eval_const_expr(expr->expr_data.sign_term, symtab, &inner))
                {
                    *out_value = -inner;
                    return 1;
                }
            }
            return 0;
        case EXPR_TYPECAST:
            if (expr->expr_data.typecast_data.expr != NULL)
                return codegen_eval_const_expr(expr->expr_data.typecast_data.expr, symtab, out_value);
            return 0;
        case EXPR_ADDOP:
        {
            long long left, right;
            if (codegen_eval_const_expr(expr->expr_data.addop_data.left_expr, symtab, &left) &&
                codegen_eval_const_expr(expr->expr_data.addop_data.right_term, symtab, &right))
            {
                switch (expr->expr_data.addop_data.addop_type)
                {
                    case PLUS:
                        *out_value = left + right;
                        return 1;
                    case MINUS:
                        *out_value = left - right;
                        return 1;
                    case OR:
                        *out_value = left | right;
                        return 1;
                    case XOR:
                        *out_value = left ^ right;
                        return 1;
                }
            }
            return 0;
        }
        case EXPR_MULOP:
        {
            long long left, right;
            if (codegen_eval_const_expr(expr->expr_data.mulop_data.left_term, symtab, &left) &&
                codegen_eval_const_expr(expr->expr_data.mulop_data.right_factor, symtab, &right))
            {
                switch (expr->expr_data.mulop_data.mulop_type)
                {
                    case STAR:
                        *out_value = left * right;
                        return 1;
                    case SLASH:
                        return 0;
                    case DIV:
                        if (right != 0)
                        {
                            *out_value = left / right;
                            return 1;
                        }
                        return 0;
                    case MOD:
                        if (right != 0)
                        {
                            *out_value = left % right;
                            return 1;
                        }
                        return 0;
                    case AND:
                        *out_value = left & right;
                        return 1;
                    case SHL:
                        *out_value = left << right;
                        return 1;
                    case SHR:
                        *out_value = (unsigned long long)left >> right;
                        return 1;
                }
            }
            return 0;
        }
        default:
            return 0;
    }
}

void codegen_emit_const_decl_equivs_from_list(CodeGenContext *ctx, ListNode_t *const_decls)
{
    assert(ctx != NULL);

    if (const_decls == NULL)
        return;

    for (ListNode_t *cur = const_decls; cur != NULL; cur = cur->next)
    {
        Tree_t *decl = (Tree_t *)cur->cur;
        if (decl == NULL || decl->type != TREE_CONST_DECL)
            continue;

        const char *id = decl->tree_data.const_decl_data.id;
        struct Expression *value = decl->tree_data.const_decl_data.value;

        if (id == NULL || value == NULL)
            continue;

        if (!codegen_should_emit_const_equiv_symbol(ctx, NULL, NULL, id))
            continue;

        if (ctx->symtab != NULL)
        {
            HashNode_t *node = NULL;
            if (FindSymbol(&node, ctx->symtab, (char *)id) != 0 &&
                node != NULL &&
                (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
            {
                int type_tag = codegen_tag_from_kgpc(node->type);
                if (!is_ordinal_type(type_tag))
                    continue;
            }
        }

        long long const_value = 0;
        if (codegen_eval_const_expr(value, ctx != NULL ? ctx->symtab : NULL, &const_value))
            fprintf(ctx->output_file, ".equ %s, %lld\n", id, const_value);
    }
}

void codegen_register_owner_unit_scope(CodeGenContext *ctx,
    SymTab_t *symtab, int source_unit_index)
{
    if (ctx == NULL || symtab == NULL || ctx->comp_ctx == NULL || source_unit_index <= 0)
        return;

    LoadedUnit *loaded_unit = compilation_context_find_unit(ctx->comp_ctx, source_unit_index);
    if (loaded_unit == NULL || loaded_unit->unit_tree == NULL ||
        loaded_unit->unit_tree->type != TREE_UNIT)
        return;

    Tree_t *unit = loaded_unit->unit_tree;
    ScopeNode *saved_scope = symtab->current_scope;
    int saved_unit_index = symtab->current_unit_index;
    ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, source_unit_index);
    if (unit_scope != NULL && unit_scope->codegen_unit_scope_registered)
        return;
    if (unit_scope != NULL)
        symtab->current_scope = unit_scope;
    symtab->current_unit_index = source_unit_index;

    codegen_register_type_enum_literals(unit->tree_data.unit_data.interface_type_decls, symtab);
    codegen_register_type_enum_literals(unit->tree_data.unit_data.implementation_type_decls, symtab);
    codegen_register_decl_list(ctx, unit->tree_data.unit_data.interface_var_decls, symtab, 0);
    codegen_register_decl_list(ctx, unit->tree_data.unit_data.implementation_var_decls, symtab, 0);
    codegen_register_const_decls(unit->tree_data.unit_data.interface_const_decls, symtab);
    codegen_register_const_decls(unit->tree_data.unit_data.implementation_const_decls, symtab);
    if (unit_scope != NULL)
        unit_scope->codegen_unit_scope_registered = 1;

    symtab->current_scope = saved_scope;
    symtab->current_unit_index = saved_unit_index;
}

void codegen_rodata(CodeGenContext *ctx, SymTab_t *symtab)
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
    fprintf(ctx->output_file, "%s\n", codegen_text_section_resume());
    codegen_emit_integer_const_equivs(ctx, symtab);
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Check whether a method uses the SRET calling convention (returns a
 * value >8 bytes via a hidden first pointer argument).  This shifts
 * Self from the first to the second argument register. */

/* Generates platform-compatible headers */
void codegen_program_header(const char *fname, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(fname != NULL);
    assert(ctx != NULL);
    {
        char escaped_fname[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_fname, fname, sizeof(escaped_fname));
        fprintf(ctx->output_file, "\t.file\t\"%s\"\n", escaped_fname);
    }
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

    fprintf(ctx->output_file, "\t.text\n");
    fprintf(ctx->output_file, "\t.set\tKGPC_TARGET_WINDOWS, %d\n", codegen_target_is_windows());
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
        fprintf(ctx->output_file, "\t.string\t\"KGPC: 0.0.0\"\n");
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
    {
        fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", call_space);
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_stackalloc\t%d\n", call_space);
    }
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_endprologue\n");
    fprintf(ctx->output_file, "\tcall\tkgpc_init_args\n");
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

static int codegen_max_rbp_stack_ref(ListNode_t *inst_list)
{
    int max_offset = 0;

    while (inst_list != NULL)
    {
        const char *text = NULL;
        if (inst_list->type == LIST_STRING && inst_list->cur != NULL)
        {
            text = (const char *)inst_list->cur;
        }
        else if (inst_list->type == LIST_IR_INST && inst_list->cur != NULL)
        {
            IrInst_t *ir = (IrInst_t *)inst_list->cur;
            /* Prefer the pre-formatted template (contains concrete %rbp offsets)
             * over the post-substitution text (not available yet at this point). */
            text = ir->tmpl ? ir->tmpl : ir->text;
        }

        if (text != NULL)
        {
            const char *cursor = text;
            while ((cursor = strchr(cursor, '-')) != NULL)
            {
                const char *digits = cursor + 1;
                char *endptr = NULL;
                long offset;

                if (!isdigit((unsigned char)*digits))
                {
                    ++cursor;
                    continue;
                }

                offset = strtol(digits, &endptr, 10);
                if (endptr != NULL &&
                    strncmp(endptr, "(%rbp)", 6) == 0 &&
                    offset > 0 && offset <= INT_MAX &&
                    (int)offset > max_offset)
                {
                    max_offset = (int)offset;
                }

                cursor = digits;
            }
        }

        inst_list = inst_list->next;
    }

    return max_offset;
}

static int codegen_compute_stack_space_from_insts(ListNode_t *inst_list, CodeGenContext *ctx)
{
    int needed_space = codegen_max_rbp_stack_ref(inst_list);
    int div;
    int rem;

    assert(ctx != NULL);

    if (ctx->callee_save_rbx_offset > needed_space)
        needed_space = ctx->callee_save_rbx_offset;
    if (ctx->callee_save_r12_offset > needed_space)
        needed_space = ctx->callee_save_r12_offset;
    if (ctx->callee_save_r13_offset > needed_space)
        needed_space = ctx->callee_save_r13_offset;
    if (ctx->callee_save_r14_offset > needed_space)
        needed_space = ctx->callee_save_r14_offset;
    if (ctx->callee_save_r15_offset > needed_space)
        needed_space = ctx->callee_save_r15_offset;

    needed_space += current_stack_home_space();
    if (needed_space <= 0)
        return 0;

    div = needed_space / REQUIRED_OFFSET;
    rem = needed_space % REQUIRED_OFFSET;
    if (rem > 0)
        ++div;

    return div * REQUIRED_OFFSET;
}

/* Generates code to allocate needed stack space */
void codegen_stack_space_for_inst_list(ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    int needed_space;
    const int stack_probe_page = 4096;

    assert(ctx != NULL);

    needed_space = codegen_compute_stack_space_from_insts(inst_list, ctx);
    assert(needed_space >= 0);

    int aligned_space = align_to_multiple(needed_space, REQUIRED_OFFSET);

    if(aligned_space != 0)
    {
        if (aligned_space > stack_probe_page)
        {
            int remaining = aligned_space;

            while (remaining > stack_probe_page)
            {
                fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", stack_probe_page);
                fprintf(ctx->output_file, "\tmovq\t$0, (%%rsp)\n");
                remaining -= stack_probe_page;
            }

            if (remaining > 0)
            {
                fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", remaining);
                fprintf(ctx->output_file, "\tmovq\t$0, (%%rsp)\n");
            }
        }
        else
        {
            fprintf(ctx->output_file, "\tsubq\t$%d, %%rsp\n", aligned_space);
        }
        if (codegen_target_is_windows())
            fprintf(ctx->output_file, "\t.seh_stackalloc\t%d\n", aligned_space);

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
             * rep stosq will clobber rcx and rdi.
             * Note: rdi is non-volatile (callee-saved) on Windows x64 ABI, so we must preserve it. */
            fprintf(ctx->output_file, "\tmovq\t%%rcx, %%r11\n");  /* Save rcx (1st param) to r11 */
            fprintf(ctx->output_file, "\tmovq\t%%rdi, %%r10\n");  /* Save rdi (callee-saved) to r10 */
            fprintf(ctx->output_file, "\tmovq\t%%rsp, %%rdi\n");   /* rdi = stack pointer */
            fprintf(ctx->output_file, "\txorq\t%%rax, %%rax\n");   /* rax = 0 */
            fprintf(ctx->output_file, "\tmovl\t$%d, %%ecx\n", quadwords);  /* ecx = count */
            fprintf(ctx->output_file, "\trep stosq\n");            /* Zero-fill */
            fprintf(ctx->output_file, "\tmovq\t%%r10, %%rdi\n");  /* Restore rdi */
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
    if (codegen_target_is_windows())
        fprintf(ctx->output_file, "\t.seh_endprologue\n");

    /* Save callee-saved registers after the stack frame is set up */
    if (ctx->callee_save_rbx_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%rbx, -%d(%%rbp)\n", ctx->callee_save_rbx_offset);
    if (ctx->callee_save_r12_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r12, -%d(%%rbp)\n", ctx->callee_save_r12_offset);
    if (ctx->callee_save_r13_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r13, -%d(%%rbp)\n", ctx->callee_save_r13_offset);
    if (ctx->callee_save_r14_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r14, -%d(%%rbp)\n", ctx->callee_save_r14_offset);
    if (ctx->callee_save_r15_offset > 0)
        fprintf(ctx->output_file, "\tmovq\t%%r15, -%d(%%rbp)\n", ctx->callee_save_r15_offset);

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

void codegen_stack_space(CodeGenContext *ctx)
{
    codegen_stack_space_for_inst_list(NULL, ctx);
}

/* Writes instruction list to file */
void codegen_inst_list(ListNode_t *inst_list, CodeGenContext *ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif

    assert(ctx != NULL);

    while(inst_list != NULL)
    {
        const char *text;
        if (inst_list->type == LIST_IR_INST)
        {
            IrInst_t *ir = (IrInst_t *)inst_list->cur;
            assert(ir != NULL);
            text = ir->text;
        }
        else
        {
            text = (const char *)inst_list->cur;
        }
        assert(text != NULL);

        fprintf(ctx->output_file, "%s", text);

        inst_list = inst_list->next;
    }
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}

/* Returns the program name for use with main */
char * codegen_program(Tree_t *prgm, CodeGenContext *ctx, SymTab_t *symtab,
                       CompilationContext *comp_ctx)
{
    if (prgm == NULL)
        return NULL;

    struct Program *prog_data = &prgm->tree_data.program_data;
    if (getenv("KGPC_DEBUG_CODEGEN") != NULL) {
        fprintf(stderr, "[CodeGen] codegen_program: starting\n");
        if (prog_data->body_statement != NULL) {
            fprintf(stderr, "[CodeGen]   body_statement is NOT NULL, type=%d\n", prog_data->body_statement->type);
        } else {
            fprintf(stderr, "[CodeGen]   body_statement is NULL\n");
        }
    }
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
    int prev_callee_rbx = ctx->callee_save_rbx_offset;
    int prev_callee_r12 = ctx->callee_save_r12_offset;
    int prev_callee_r13 = ctx->callee_save_r13_offset;
    int prev_callee_r14 = ctx->callee_save_r14_offset;
    int prev_callee_r15 = ctx->callee_save_r15_offset;
    ctx->current_subprogram_id = prgm_name;
    ctx->current_subprogram_mangled = prgm_name;
    ctx->current_subprogram_lexical_depth = 0;

    push_stackscope();

    /* Process var/const declarations from loaded units first, then program.
     * These are always needed — even with --skip-unit-codegen, the per-test
     * compilation must emit unit globals and run unit init code.
     *
     * Bind symtab->current_unit_index AND current_scope per unit so that
     * bare-name lookups inside codegen_function_locals (e.g. FindSymbol
     * for `var_info`, which feeds back the array element size used to
     * size the .comm allocation) resolve to the unit currently being
     * processed rather than the last-write-wins entry in the global
     * symtab.  Without this, two units declaring same-named typed-consts
     * of different element types end up with one unit's storage sized
     * using the OTHER unit's element size. */
    if (comp_ctx != NULL) {
        int saved_unit_index = symtab->current_unit_index;
        ScopeNode *saved_scope = symtab->current_scope;
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            int unit_idx = comp_ctx->loaded_units[i].unit_idx;
            if (unit_idx > 0) {
                symtab->current_unit_index = unit_idx;
                ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, unit_idx);
                if (unit_scope != NULL)
                    symtab->current_scope = unit_scope;
            }
            codegen_function_locals(unit->tree_data.unit_data.interface_var_decls, ctx, symtab);
            codegen_function_locals(unit->tree_data.unit_data.implementation_var_decls, ctx, symtab);
            codegen_emit_const_decl_equivs_from_list(ctx, unit->tree_data.unit_data.interface_const_decls);
            codegen_emit_const_decl_equivs_from_list(ctx, unit->tree_data.unit_data.implementation_const_decls);
        }
        symtab->current_unit_index = saved_unit_index;
        symtab->current_scope = saved_scope;
    }
    codegen_function_locals(data->var_declaration, ctx, symtab);
    codegen_emit_const_decl_equivs_from_list(ctx, data->const_declaration);

    /* Allocate callee-save slots AFTER locals so t-section offsets don't collide */
    {
        StackNode_t *rbx_slot = add_l_t_bytes("__callee_rbx", 8);
        StackNode_t *r12_slot = add_l_t_bytes("__callee_r12", 8);
        StackNode_t *r13_slot = add_l_t_bytes("__callee_r13", 8);
        StackNode_t *r14_slot = add_l_t_bytes("__callee_r14", 8);
        StackNode_t *r15_slot = add_l_t_bytes("__callee_r15", 8);
        ctx->callee_save_rbx_offset = rbx_slot->offset;
        ctx->callee_save_r12_offset = r12_slot->offset;
        ctx->callee_save_r13_offset = r13_slot->offset;
        ctx->callee_save_r14_offset = r14_slot->offset;
        ctx->callee_save_r15_offset = r15_slot->offset;
    }

    /* Pre-pass: propagate cname_override from forward declarations to their
     * matching implementations so that codegen_function_header emits both labels
     * directly (e.g., FPC_INTERLOCKEDCOMPAREEXCHANGE64: and
     * interlockedcompareexchange64_i64_i64_i64:).  This eliminates the need
     * for .set alias post-passes and ensures the cache .o has all needed symbols. */
    {
        /* Build a flat list of all subprograms (units + program) */
        ListNode_t *all_subs_head = NULL;
        if (comp_ctx != NULL) {
            for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
                Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
                if (unit == NULL || unit->type != TREE_UNIT) continue;
                ListNode_t *usubs = unit->tree_data.unit_data.subprograms;
                while (usubs != NULL) {
                    ListNode_t *copy = CreateListNode(usubs->cur, usubs->type);
                    if (copy != NULL) { copy->next = all_subs_head; all_subs_head = copy; }
                    usubs = usubs->next;
                }
            }
        }
        {
            ListNode_t *psubs = data->subprograms;
            while (psubs != NULL) {
                ListNode_t *copy = CreateListNode(psubs->cur, psubs->type);
                if (copy != NULL) { copy->next = all_subs_head; all_subs_head = copy; }
                psubs = psubs->next;
            }
        }

        /* Pass 1: propagate cname_override from forward decls to implementations.
         * Forward decls have [Alias:'FPC_XYZ'] but no body; the impl has a body
         * but no cname_override.  Copy the alias so the impl header emits both labels. */
        for (ListNode_t *fwd = all_subs_head; fwd != NULL; fwd = fwd->next) {
            if (fwd->type != LIST_TREE || fwd->cur == NULL) continue;
            Tree_t *fwd_sub = (Tree_t *)fwd->cur;
            if (fwd_sub->type != TREE_SUBPROGRAM) continue;
            const char *alias = fwd_sub->tree_data.subprogram_data.cname_override;
            if (alias == NULL) continue;
            if (fwd_sub->tree_data.subprogram_data.statement_list != NULL) continue; /* has body — not a forward decl */
            /* Only propagate FPC_/KGPC_ internal aliases */
            if (strncmp(alias, "FPC_", 4) != 0 && strncmp(alias, "KGPC_", 5) != 0) continue;
            const char *fwd_id = fwd_sub->tree_data.subprogram_data.id;
            if (fwd_id == NULL) continue;

            for (ListNode_t *impl = all_subs_head; impl != NULL; impl = impl->next) {
                if (impl->type != LIST_TREE || impl->cur == NULL) continue;
                Tree_t *impl_sub = (Tree_t *)impl->cur;
                if (impl_sub == fwd_sub || impl_sub->type != TREE_SUBPROGRAM) continue;
                if (impl_sub->tree_data.subprogram_data.statement_list == NULL) continue;
                if (impl_sub->tree_data.subprogram_data.cname_override != NULL) continue; /* already has alias */
                const char *impl_id = impl_sub->tree_data.subprogram_data.id;
                if (impl_id == NULL) continue;
                int matched = (strcasecmp(fwd_id, impl_id) == 0);
                if (!matched && impl_sub->tree_data.subprogram_data.cname_override != NULL &&
                    strcasecmp(impl_sub->tree_data.subprogram_data.cname_override, alias) == 0)
                    matched = 1;
                if (matched) {
                    impl_sub->tree_data.subprogram_data.cname_override = strdup(fwd_sub->tree_data.subprogram_data.cname_override);
                    break;
                }
            }
        }

        /* Free the temporary list (shallow copies only) */
        ListNode_t *tmp = all_subs_head;
        while (tmp != NULL) {
            ListNode_t *next = tmp->next;
            tmp->cur = NULL;
            free(tmp);
            tmp = next;
        }
    }

    /* Emit program subprograms first (they override unit versions with the
     * same mangled_id), then unit subprograms.
     * Units are iterated in REVERSE load order so that more fundamental units
     * (e.g. System) are processed last and their implementations win over
     * wrapper functions in higher-level units (e.g. objpas).  In the
     * loaded_units array, dependencies are added before their dependents
     * return from load_unit(), so the most fundamental unit has the highest
     * array index.  Iterating in reverse processes fundamentals first. */
    codegen_subprograms(data->subprograms, ctx, symtab);
    if (comp_ctx != NULL && !skip_unit_codegen_flag()) {
        for (int i = comp_ctx->loaded_unit_count - 1; i >= 0; --i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            codegen_subprograms(unit->tree_data.unit_data.subprograms, ctx, symtab);
        }
    }

    /* The cname_override pre-pass above propagated aliases from forward
     * declarations to their matching implementations, so codegen_function_header
     * now emits both the alias and mangled_id labels directly.  No .set
     * aliases needed — the cache .o is self-contained. */

    reset_reg_stack();
    codegen_invalidate_static_link_cache(ctx);
    inst_list = NULL;
    ctx->next_vreg_id = 0;
    /* Emit var initializers from loaded units first, then program.
     * Switch symtab->current_scope to each unit's ScopeNode so FindIdent_Tree
     * resolves identifiers in that unit's own scope first (e.g. cp1252's
     * typed-const `unicodemap` resolves to cp1252's HashNode, not cp1250's),
     * which is what makes same-named typed-consts in multiple units
     * disambiguate correctly without per-site lookup fallbacks. */
    if (comp_ctx != NULL) {
        int saved_unit_index = symtab->current_unit_index;
        ScopeNode *saved_scope = symtab->current_scope;
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            int unit_idx = comp_ctx->loaded_units[i].unit_idx;
            symtab->current_unit_index = unit_idx;
            ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, unit_idx);
            if (unit_scope != NULL)
                symtab->current_scope = unit_scope;
            inst_list = codegen_var_initializers(unit->tree_data.unit_data.interface_var_decls, inst_list, ctx, symtab);
            inst_list = codegen_var_initializers(unit->tree_data.unit_data.implementation_var_decls, inst_list, ctx, symtab);
        }
        symtab->current_unit_index = saved_unit_index;
        symtab->current_scope = saved_scope;
    }
    inst_list = codegen_var_initializers(data->var_declaration, inst_list, ctx, symtab);

    /* Class constructors initialize class-level storage before unit/program
     * initialization code can observe it. */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            inst_list = codegen_class_constructor_calls(inst_list,
                unit->tree_data.unit_data.interface_type_decls, symtab);
            inst_list = codegen_class_constructor_calls(inst_list,
                unit->tree_data.unit_data.implementation_type_decls, symtab);
        }
    }
    inst_list = codegen_class_constructor_calls(inst_list, data->type_declaration, symtab);

    /* Emit unit initialization blocks in dependency (load) order.
     * Switch current_scope to each unit's ScopeNode so identifier lookups
     * resolve in that unit's own scope first. */
    if (comp_ctx != NULL) {
        int saved_unit_index = symtab->current_unit_index;
        ScopeNode *saved_scope = symtab->current_scope;
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            struct Statement *init_stmt = unit->tree_data.unit_data.initialization;
            if (init_stmt == NULL)
                continue;
            int unit_idx = comp_ctx->loaded_units[i].unit_idx;
            symtab->current_unit_index = unit_idx;
            ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, unit_idx);
            if (unit_scope != NULL)
                symtab->current_scope = unit_scope;
            /* Only inline the inner statements from compound statements */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT) {
                ListNode_t *stnode = init_stmt->stmt_data.compound_statement;
                while (stnode != NULL) {
                    if (stnode->type == LIST_STMT && stnode->cur != NULL)
                        inst_list = codegen_stmt((struct Statement *)stnode->cur, inst_list, ctx, symtab);
                    stnode = stnode->next;
                }
            } else {
                inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
            }
        }
        symtab->current_unit_index = saved_unit_index;
        symtab->current_scope = saved_scope;
    }

    if (data->body_statement == NULL && getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] WARNING: program body is NULL during codegen\n");
    }
    inst_list = codegen_stmt(data->body_statement, inst_list, ctx, symtab);

    /* Emit unit finalization blocks in reverse dependency order (LIFO).
     * Switch current_scope per unit so lookups resolve in unit scope first. */
    if (comp_ctx != NULL) {
        int saved_unit_index = symtab->current_unit_index;
        ScopeNode *saved_scope = symtab->current_scope;
        for (int i = comp_ctx->loaded_unit_count - 1; i >= 0; --i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit == NULL || unit->type != TREE_UNIT)
                continue;
            struct Statement *final_stmt = unit->tree_data.unit_data.finalization;
            if (final_stmt == NULL)
                continue;
            int unit_idx = comp_ctx->loaded_units[i].unit_idx;
            symtab->current_unit_index = unit_idx;
            ScopeNode *unit_scope = GetOrCreateUnitScope(symtab, unit_idx);
            if (unit_scope != NULL)
                symtab->current_scope = unit_scope;
            inst_list = codegen_stmt(final_stmt, inst_list, ctx, symtab);
        }
        symtab->current_unit_index = saved_unit_index;
        symtab->current_scope = saved_scope;
    }

    codegen_function_header(prgm_name, ctx);
    codegen_stack_space_for_inst_list(inst_list, ctx);
#if USE_GRAPH_COLORING_ALLOCATOR
    ir_liveness_allocate(inst_list);
#endif
    ir_emit_function(inst_list);
#if USE_GRAPH_COLORING_ALLOCATOR
    ir_peephole_remove_redundant_moves(&inst_list);
#endif
    codegen_inst_list(inst_list, ctx);
    codegen_function_footer(prgm_name, ctx);
    if (dump_ir_flag())
        ir_print_function(stderr, prgm_name, inst_list);
    if (dump_ir_cfg_flag())
    {
        Cfg_t *cfg = cfg_build(inst_list);
        cfg_print(stderr, cfg, prgm_name);
        cfg_free(cfg);
    }
    if (dump_ir_liveness_flag())
    {
        Cfg_t *cfg = cfg_build(inst_list);
        LivenessInfo_t *liveness = liveness_compute(cfg);
        liveness_print(stderr, cfg, liveness, prgm_name);
        liveness_free(liveness);
        cfg_free(cfg);
    }
    free_inst_list(inst_list);

    /* Emit INITFINAL table — FPC system unit references this to run unit
       init/finalization.  KGPC inlines that code into main, so emit a
       minimal table with TableCount = 0.
       */
    if (ctx->output_file != NULL) {
        fprintf(ctx->output_file, "\n.data\n");
        fprintf(ctx->output_file, ".globl\tINITFINAL\n");
        fprintf(ctx->output_file, "INITFINAL:\n");
        fprintf(ctx->output_file, "\t.long\t0\n");  /* TableCount = 0 */
    }

    /* Emit FPC_RESOURCESTRINGTABLES as a zero-length table (no resource strings). */
    if (ctx->output_file != NULL) {
        fprintf(ctx->output_file, ".globl\tFPC_RESOURCESTRINGTABLES\n");
        fprintf(ctx->output_file, "FPC_RESOURCESTRINGTABLES:\n");
        fprintf(ctx->output_file, "\t.quad\t0\n");
    }

    pop_stackscope();

    ctx->current_subprogram_id = prev_id;
    ctx->current_subprogram_mangled = prev_mangled;
    ctx->current_subprogram_lexical_depth = prev_depth;
    ctx->callee_save_rbx_offset = prev_callee_rbx;
    ctx->callee_save_r12_offset = prev_callee_r12;
    ctx->callee_save_r13_offset = prev_callee_r13;
    ctx->callee_save_r14_offset = prev_callee_r14;
    ctx->callee_save_r15_offset = prev_callee_r15;

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
                FindSymbol(&type_node, symtab, tree->tree_data.var_decl_data.type_id);
            }
            int decl_type_owned = 0;
            KgpcType *decl_type = resolve_type_from_vardecl(tree, symtab, &decl_type_owned);
            KgpcType *cached_type = tree->tree_data.var_decl_data.cached_kgpc_type;

            while(id_list != NULL)
            {
                HashNode_t decl_type_node;
                HashNode_t cached_type_node;
                HashNode_t *fallback_type_node = NULL;
                HashNode_t *var_info = NULL;
                if (decl_type != NULL)
                {
                    memset(&decl_type_node, 0, sizeof(decl_type_node));
                    decl_type_node.type = decl_type;
                    fallback_type_node = &decl_type_node;
                }
                if (cached_type != NULL)
                {
                    memset(&cached_type_node, 0, sizeof(cached_type_node));
                    cached_type_node.type = cached_type;
                    if (fallback_type_node == NULL)
                        fallback_type_node = &cached_type_node;
                }
                if (symtab != NULL)
                    FindSymbol(&var_info, symtab, id_list->cur);

                HashNode_t *effective_type_node = type_node;
                if (decl_type != NULL)
                    effective_type_node = &decl_type_node;
                else if (effective_type_node == NULL)
                    effective_type_node = fallback_type_node;

                KgpcType *param_type = NULL;
                if (effective_type_node != NULL)
                    param_type = effective_type_node->type;
                if (param_type == NULL && var_info != NULL)
                    param_type = var_info->type;
                param_type = codegen_prefer_promoted_shortstring_type(param_type, var_info);
                KGPC_COMPILER_HARD_ASSERT(param_type != NULL,
                    "missing type metadata for local '%s' (declared type '%s')",
                    (const char *)id_list->cur,
                    tree->tree_data.var_decl_data.type_id != NULL ?
                        tree->tree_data.var_decl_data.type_id : "<null>");

                if (param_type != NULL && kgpc_type_is_array(param_type))
                {
                    KgpcArrayDimensionInfo array_info;
                    int dim_info_result = kgpc_type_get_array_dimension_info(param_type, symtab, &array_info);
                    
                    /* If dimension info lookup succeeded, use its values; otherwise fall back to simpler methods */
                    int element_size;
                    int array_start;
                    long long total_size;
                    
                    if (dim_info_result == 0 && array_info.dim_count > 0 &&
                        array_info.element_size > 0 &&
                        array_info.element_size <= INT_MAX &&
                        array_info.dim_lowers[0] >= INT_MIN &&
                        array_info.dim_lowers[0] <= INT_MAX)
                    {
                        /* Use computed values from kgpc_type_get_array_dimension_info which handles
                         * multi-dimensional arrays and constant-based bounds correctly */
                        element_size = (int)array_info.element_size;
                        array_start = (int)array_info.dim_lowers[0];
                        total_size = array_info.total_size;
                    }
                    else
                    {
                        /* Fallback: use simpler methods that work for basic array types */
                        element_size = (int)kgpc_type_get_array_element_size(param_type);
                        if (element_size <= 0 && param_type != NULL &&
                            param_type->info.array_info.element_type != NULL &&
                            param_type->info.array_info.element_type->kind == TYPE_KIND_RECORD)
                        {
                            long long record_size = 0;
                            if (codegen_sizeof_record_type(ctx,
                                    param_type->info.array_info.element_type->info.record_info,
                                    &record_size) == 0 && record_size > 0 && record_size <= INT_MAX)
                            {
                                element_size = (int)record_size;
                            }
                        }
                        KGPC_COMPILER_HARD_ASSERT(element_size > 0,
                            "unable to resolve array element size for local '%s'",
                            (const char *)id_list->cur);
                        array_start = param_type->info.array_info.start_index;
                        total_size = kgpc_type_sizeof(param_type);
                    }
                    
                    int is_open_array = kgpc_type_is_dynamic_array(param_type);

                    if (is_open_array)
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
                        {
                            char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                            add_dynamic_array(storage_key != NULL ? storage_key : (char *)id_list->cur,
                                element_size, array_start, is_program_scope, static_label);
                            if (storage_key != NULL)
                                free(storage_key);
                        }
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        if (total_size <= 0)
                            total_size = element_size;

                        if (is_program_scope)
                        {
                            char *static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, static_label,
                                    (int)total_size, alignment,
                                    tree->tree_data.var_decl_data.defined_in_unit
                                    && !(tree->tree_data.var_decl_data.is_typed_const &&
                                         codegen_typed_const_name_collides_ctx(ctx,
                                             (const char *)id_list->cur)));
                            }
                            {
                                char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                                add_static_array(storage_key != NULL ? storage_key : (char *)id_list->cur,
                                    (int)total_size, element_size, array_start, static_label);
                                if (storage_key != NULL)
                                    free(storage_key);
                            }
                            if (static_label != NULL)
                                free(static_label);
                        }
                        else
                        {
                            add_array((char *)id_list->cur, (int)total_size, element_size,
                                array_start);
                        }
                    }
                }
                else if (var_info != NULL && var_info->type != NULL && kgpc_type_is_array(var_info->type))
                {
                    KgpcType *array_type = var_info->type;
                    KgpcType *element_type = array_type->info.array_info.element_type;
                    long long element_size_ll = kgpc_type_get_array_element_size(array_type);
                    if (element_size_ll <= 0 && element_type != NULL)
                        element_size_ll = kgpc_type_sizeof(element_type);
                    if (element_size_ll <= 0 && element_type != NULL &&
                        element_type->kind == TYPE_KIND_RECORD)
                    {
                        long long record_size = 0;
                        if (codegen_sizeof_record_type(ctx, element_type->info.record_info,
                                &record_size) == 0 && record_size > 0)
                        {
                            element_size_ll = record_size;
                        }
                    }
                    if (element_size_ll <= 0)
                        element_size_ll = 4;

                    int start = 0;
                    int end = -1;
                    kgpc_type_get_array_bounds(array_type, &start, &end);
                    int is_open_array = kgpc_type_is_dynamic_array(array_type);

                    if (is_open_array)
                    {
                        char *static_label = NULL;
                        if (is_program_scope)
                        {
                            static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int descriptor_bytes = codegen_dynamic_array_descriptor_bytes((int)element_size_ll);
                                int alignment = descriptor_bytes >= 8 ? 8 : DOUBLEWORD;
                                fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                    static_label, descriptor_bytes, alignment);
                            }
                        }
                        {
                            char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                            add_dynamic_array(storage_key != NULL ? storage_key : (char *)id_list->cur,
                                (int)element_size_ll, start, is_program_scope, static_label);
                            if (storage_key != NULL)
                                free(storage_key);
                        }
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        int length = end - start + 1;
                        if (length < 0)
                            length = 0;
                        long long total_size = (long long)length * element_size_ll;
                        if (total_size <= 0)
                            total_size = element_size_ll;
                        if (is_program_scope)
                        {
                            char *static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                            if (ctx->output_file != NULL && static_label != NULL)
                            {
                                int alignment = total_size >= 8 ? 8 : DOUBLEWORD;
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, static_label,
                                    (int)total_size, alignment,
                                    tree->tree_data.var_decl_data.defined_in_unit
                                    && !(tree->tree_data.var_decl_data.is_typed_const &&
                                         codegen_typed_const_name_collides_ctx(ctx,
                                             (const char *)id_list->cur)));
                            }
                            {
                                char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                                add_static_array(storage_key != NULL ? storage_key : (char *)id_list->cur,
                                    (int)total_size, (int)element_size_ll, start, static_label);
                                if (storage_key != NULL)
                                    free(storage_key);
                            }
                            if (static_label != NULL)
                                free(static_label);
                        }
                        else
                        {
                            add_array((char *)id_list->cur, (int)total_size,
                                (int)element_size_ll, start);
                        }
                    }
                }
                else
                {
                    int alloc_size = DOUBLEWORD;
                    HashNode_t *size_node = NULL;  /* Node to get size from */
                    HashNode_t temp_size_node;
                    
                    if (symtab != NULL)
                    {
                        if (FindSymbol(&var_info, symtab, id_list->cur) != 0 && var_info != NULL)
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
                    int decl_storage_size = -1;
                    decl_storage_size = codegen_storage_size_from_type_alias(
                        tree->tree_data.var_decl_data.inline_type_alias);
                    if (decl_type != NULL)
                    {
                        int type_size = codegen_storage_size_from_type(decl_type);
                        if (decl_storage_size <= 0)
                            decl_storage_size = type_size;
                    }
                    if (decl_storage_size <= 0 && cached_type != NULL)
                        decl_storage_size = codegen_storage_size_from_type(cached_type);
                    if (decl_storage_size <= 0 && type_node != NULL)
                        decl_storage_size = get_var_storage_size(type_node);

                    if (decl_storage_size > 0)
                    {
                        alloc_size = decl_storage_size;
                    }
                    else if (size_node != NULL)
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
                                CODEGEN_DEBUG("DEBUG ALLOC: Detected class type for '%s', allocating 8 bytes\n",
                                    (char *)id_list->cur);
                                alloc_size = 8;  /* Classes are heap-allocated; variable holds pointer */
                            }
                            else
                            {
                                CODEGEN_DEBUG("DEBUG ALLOC: Detected record type for '%s', allocating full size\n",
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
                        const char *absolute_target = tree->tree_data.var_decl_data.absolute_target;
                        const char *absolute_base = tree->tree_data.var_decl_data.absolute_base_id;
                        const char *absolute_field = tree->tree_data.var_decl_data.absolute_field_id;
                        if (absolute_target != NULL && id_list != NULL && id_list->next == NULL)
                        {
                            if (absolute_base != NULL && absolute_field != NULL)
                            {
                                /* Record field alias: extract base var and field name */
                                const char *base_var = absolute_base;
                                const char *field_name = absolute_field;

                                if (base_var != NULL && field_name != NULL)
                                {
                                    /* Look up base variable in symbol table to get record type */
                                    int field_offset = -1;
                                    HashNode_t *base_node = NULL;
                                    if (ctx->symtab != NULL &&
                                        FindSymbol(&base_node, ctx->symtab, base_var) != 0 &&
                                        base_node != NULL)
                                    {
                                        struct RecordType *record = get_record_type_from_node(base_node);
                                        if (record != NULL)
                                        {
                                            field_offset = record_type_get_field_offset(ctx->symtab, record, field_name);
                                        }
                                    }
                                    
                                    if (field_offset >= 0)
                                    {
                                        if (add_absolute_var_alias_with_offset((char *)id_list->cur, 
                                            (char *)base_var, field_offset, alloc_size) == 0)
                                        {
                                            id_list = id_list->next;
                                            continue;
                                        }
                                    }
                                }
                                fprintf(stderr,
                                    "Warning: absolute variable alias to record field '%s' failed to resolve.\n",
                                    absolute_target);
                            }
                            else if (absolute_base != NULL &&
                                (add_absolute_var_alias((char *)id_list->cur, absolute_base) == 0 ||
                                 add_absolute_static_symbol_alias((char *)id_list->cur,
                                     absolute_base, alloc_size) == 0))
                            {
                                id_list = id_list->next;
                                continue;
                            }
                            else
                            {
                                fprintf(stderr,
                                    "Warning: failed to resolve absolute variable alias target '%s'.\n",
                                    absolute_target);
                            }
                        }

                        char *static_label = NULL;
                        int is_external_var = tree->tree_data.var_decl_data.is_external;
                        char *cname_override = tree->tree_data.var_decl_data.cname_override;
                        if (cname_override != NULL) {
                            /* Use the external/public name directly */
                            static_label = strdup(cname_override);
                        } else {
                            static_label = codegen_make_program_var_label(ctx, (char *)id_list->cur);
                        }
                        
                        if (ctx->output_file != NULL && static_label != NULL)
                        {
                            if (is_external_var) {
                                /* External variable: don't allocate storage, just reference the symbol */
                                /* No .comm directive needed - the symbol is defined elsewhere */
                            } else {
                                int alignment = alloc_size >= 16 ? 16 : (alloc_size >= 8 ? 8 : DOUBLEWORD);
                                /* Only emit a bare-name alias when there is no explicit cname override */
                                int defined_for_alias = (cname_override == NULL) ?
                                    (tree->tree_data.var_decl_data.defined_in_unit
                                     && !(tree->tree_data.var_decl_data.is_typed_const &&
                                          codegen_typed_const_name_collides_ctx(ctx,
                                              (const char *)id_list->cur))) : 0;
                                if (cname_override != NULL) {
                                    /* Public name: make it globally visible */
                                    fprintf(ctx->output_file, "\t.globl\t%s\n", static_label);
                                }
                                /* Typed-const record whose initialiser is wholly static:
                                 * emit a .data block instead of .bss so the loader fills
                                 * the storage at program start.  Any C constructor that
                                 * later runs (e.g. kgpc_init_memory_manager) is then free
                                 * to override individual fields without being clobbered by
                                 * runtime field-by-field stores from the program body. */
                                int statically_emitted = 0;
                                if (cname_override == NULL &&
                                    tree->tree_data.var_decl_data.is_typed_const &&
                                    codegen_try_emit_typed_const_record_static_alias(ctx, symtab,
                                        tree, (const char *)id_list->cur, static_label,
                                        alloc_size, defined_for_alias) == 0)
                                {
                                    statically_emitted = 1;
                                }
                                if (!statically_emitted) {
                                    codegen_emit_bss_or_comm(ctx->output_file,
                                        (const char *)id_list->cur, static_label,
                                        alloc_size, alignment, defined_for_alias);
                                }
                            }
                        }
                        char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                        add_static_var(storage_key != NULL ? storage_key : (char *)id_list->cur,
                            alloc_size, static_label);
                        if (storage_key != NULL)
                            free(storage_key);
                        if (static_label != NULL)
                            free(static_label);
                    }
                    else
                    {
                        const char *absolute_target = tree->tree_data.var_decl_data.absolute_target;
                        const char *absolute_base = tree->tree_data.var_decl_data.absolute_base_id;
                        const char *absolute_field = tree->tree_data.var_decl_data.absolute_field_id;
                        if (absolute_target != NULL && id_list != NULL && id_list->next == NULL)
                        {
                            if (absolute_base != NULL && absolute_field != NULL)
                            {
                                /* Record field alias: extract base var and field name */
                                const char *base_var = absolute_base;
                                const char *field_name = absolute_field;

                                if (base_var != NULL && field_name != NULL)
                                {
                                    /* Look up base variable in symbol table to get record type */
                                    int field_offset = -1;
                                    HashNode_t *base_node = NULL;
                                    if (ctx->symtab != NULL &&
                                        FindSymbol(&base_node, ctx->symtab, base_var) != 0 &&
                                        base_node != NULL)
                                    {
                                        struct RecordType *record = get_record_type_from_node(base_node);
                                        if (record != NULL)
                                        {
                                            field_offset = record_type_get_field_offset(ctx->symtab, record, field_name);
                                        }
                                    }
                                    
                                    if (field_offset >= 0)
                                    {
                                        if (add_absolute_var_alias_with_offset((char *)id_list->cur, 
                                            (char *)base_var, field_offset, alloc_size) == 0)
                                        {
                                            id_list = id_list->next;
                                            continue;
                                        }
                                    }
                                }
                                fprintf(stderr,
                                    "Warning: absolute variable alias to record field '%s' failed to resolve.\n",
                                    absolute_target);
                            }
                            else if (absolute_base != NULL &&
                                (add_absolute_var_alias((char *)id_list->cur, absolute_base) == 0 ||
                                 add_absolute_static_symbol_alias((char *)id_list->cur,
                                     absolute_base, alloc_size) == 0))
                            {
                                id_list = id_list->next;
                                continue;
                            }
                            else
                            {
                                fprintf(stderr,
                                    "Warning: failed to resolve absolute variable alias target '%s'.\n",
                                    absolute_target);
                            }
                        }

                        /* Local typed constants need static storage so their
                         * address remains valid after the function returns. */
                        if (tree->tree_data.var_decl_data.has_static_storage &&
                            tree->tree_data.var_decl_data.static_label != NULL)
                        {
                            char *static_label = tree->tree_data.var_decl_data.static_label;
                            if (!tree->tree_data.var_decl_data.static_storage_emitted)
                            {
                                if (ctx->output_file != NULL)
                                {
                                    int alignment = alloc_size >= 16 ? 16 : (alloc_size >= 8 ? 8 : DOUBLEWORD);
                                    fprintf(ctx->output_file, "\t.comm\t%s,%d,%d\n",
                                        static_label, alloc_size, alignment);
                                }
                                tree->tree_data.var_decl_data.static_storage_emitted = 1;
                            }
                            add_static_var((char *)id_list->cur, alloc_size, static_label);
                        }
                        else
                        {
                            add_l_x((char *)id_list->cur, alloc_size);
                        }
                    }
                }
                id_list = id_list->next;
            };

            if (decl_type_owned && decl_type != NULL)
                destroy_kgpc_type(decl_type);
        }
        else if (tree->type == TREE_ARR_DECL)
        {
            struct Array *arr = &tree->tree_data.arr_decl_data;
            id_list = arr->ids;

            int is_dynamic = (arr->e_range < arr->s_range);

            HashNode_t *type_node = NULL;
            if (arr->type_id != NULL && symtab != NULL)
                FindSymbol(&type_node, symtab, arr->type_id);

            struct RecordType *record_desc = NULL;
            if (type_node != NULL)
            {
                record_desc = get_record_type_from_node(type_node);
                struct TypeAlias *alias = get_type_alias_from_node(type_node);
                if (record_desc == NULL && alias != NULL && alias->target_type_id != NULL)
                {
                    HashNode_t *target_node = NULL;
                    if (FindSymbol(&target_node, symtab, alias->target_type_id) != 0 &&
                        target_node != NULL)
                        record_desc = get_record_type_from_node(target_node);
                }
            }
            if (record_desc == NULL && arr->inline_record_type != NULL)
            {
                record_desc = arr->inline_record_type;
            }

            long long computed_size = 0;
            int element_size = 0;
            if (arr->element_kgpc_type != NULL)
            {
                computed_size = kgpc_type_sizeof(arr->element_kgpc_type);
                if (computed_size > 0 && computed_size <= INT_MAX)
                    element_size = (int)computed_size;
            }
            if (element_size <= 0 &&
                codegen_sizeof_type_reference(ctx, arr->type, arr->type_id,
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
                            element_size = 4;  // Match FPC's 32-bit LongInt
                            break;
                        case REAL_TYPE:
                        case STRING_TYPE:
                        case FILE_TYPE:
                        case TEXT_TYPE:
                            element_size = 8;
                            break;
                        case SHORTSTRING_TYPE:
                            element_size = 256;
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
                    {
                        char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                        add_dynamic_array(storage_key != NULL ? storage_key : (char *)id_list->cur,
                            element_size, arr->s_range, is_program_scope, static_label);
                        if (storage_key != NULL)
                            free(storage_key);
                    }
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

                /* For multi-dimensional arrays, kgpc_type_sizeof only accounts
                 * for the outer dimension.  Use kgpc_type_get_array_dimension_info
                 * which correctly multiplies all dimension sizes together.
                 *
                 * IMPORTANT: don't let the symtab lookup override total_size for
                 * single-dim arrays whose bounds were already supplied by the
                 * AST.  Two same-named typed-const arrays in different units
                 * (e.g. cpX.reversemap and cpY.reversemap, both
                 * `array[0..N] of trec2` with different N) share a name in the
                 * cross-unit symtab — FindSymbol returns whichever HashNode is
                 * currently visible, which carries the OTHER declaration's
                 * bounds.  The AST's arr->e_range / arr->s_range are
                 * per-declaration and always correct, so trust them whenever
                 * dim_info reports a single dimension and the AST has an
                 * explicit positive length. */
                int kgpc_dim_count = 0;
                if (id_list != NULL && symtab != NULL) {
                    const char *var_name = (const char *)id_list->cur;
                    HashNode_t *var_node = NULL;
                    if (FindSymbol(&var_node, symtab, var_name) && var_node != NULL && var_node->type != NULL) {
                        KgpcArrayDimensionInfo dim_info;
                        if (kgpc_type_get_array_dimension_info(var_node->type, symtab, &dim_info) == 0) {
                            kgpc_dim_count = dim_info.dim_count;
                            if (dim_info.strides[0] > 0 &&
                                dim_info.strides[0] <= INT_MAX)
                            {
                                element_size = (int)dim_info.strides[0];
                            }
                            /* Only use dim_info.total_size when it actually
                             * encodes more dimensions than the AST's outer
                             * range expression alone, or when the AST didn't
                             * carry explicit bounds (alias-typed
                             * declarations).  Otherwise the AST's
                             * per-declaration bounds win — guarding against
                             * cross-unit symtab aliasing of same-named typed
                             * consts. */
                            if (dim_info.total_size > 0 &&
                                (dim_info.dim_count > 1 || length <= 0)) {
                                total_size = (int)dim_info.total_size;
                            } else if (dim_info.dim_count == 1 && length > 0) {
                                /* Recompute total_size with possibly-updated
                                 * element_size from strides[0]; keep bounds
                                 * from the AST. */
                                total_size = length * element_size;
                                if (total_size <= 0)
                                    total_size = element_size;
                            }
                        } else {
                            long long kgpc_size = kgpc_type_sizeof(var_node->type);
                            if (kgpc_size > total_size)
                                total_size = (int)kgpc_size;
                        }
                    }
                }

                /* For multi-dim typed-consts inside subprograms, FindSymbol on
                 * the local name returns a KgpcType that only models the outer
                 * dimension (kgpc_dim_count==1), so total_size ends up at
                 * outer_length * scalar_element_size — short by the product of
                 * the inner dimensions.  The parser-built arr->array_dimensions
                 * list has the full set of ranges; use it whenever the KgpcType
                 * recorded fewer dimensions than the source declared.  Without
                 * this, .comm under-allocates and the runtime init loop's
                 * inner-dim writes overwrite adjacent BSS — the cause of the
                 * cutils.pas internalerror 2014041302 during pp.pas bootstrap:
                 * defcmp.pas's 4x4 basedefconvertsexplicit typed-const ran off
                 * into cpubase_14's guard byte, so flags_to_cond's lookup table
                 * appeared pre-initialised with zeroes and C_None was returned
                 * for every integer comparison. */
                if (arr->array_dimensions != NULL) {
                    long long product = 1;
                    int parsed_all = 1;
                    int count = 0;
                    ListNode_t *dim_node = arr->array_dimensions;
                    while (dim_node != NULL && dim_node->type == LIST_STRING &&
                           dim_node->cur != NULL) {
                        const char *range_str = (const char *)dim_node->cur;
                        long long lo = 0, hi = 0;
                        long long dim_size = 0;
                        if (sscanf(range_str, "%lld..%lld", &lo, &hi) == 2 && hi >= lo) {
                            dim_size = hi - lo + 1;
                        } else if (pascal_identifier_equals(range_str, "Boolean")) {
                            dim_size = 2;
                        } else if (symtab != NULL) {
                            /* Named range/enum: resolve via type symbol's TypeAlias
                             * (same logic as kgpc_type_get_array_dimension_info's
                             * alias branch). Necessary for procedure-scope typed
                             * consts whose KgpcType only models the outer dim. */
                            HashNode_t *type_node = NULL;
                            if (FindSymbol(&type_node, symtab, range_str) != 0 &&
                                type_node != NULL &&
                                type_node->hash_type == HASHTYPE_TYPE) {
                                struct TypeAlias *range_alias =
                                    hashnode_get_type_alias(type_node);
                                if (range_alias != NULL) {
                                    if (range_alias->is_enum &&
                                        range_alias->enum_literals != NULL) {
                                        dim_size = (long long)ListLength(
                                            range_alias->enum_literals);
                                    } else if (range_alias->is_range &&
                                               range_alias->range_known) {
                                        long long lower = range_alias->range_start;
                                        long long upper = range_alias->range_end;
                                        if (upper >= lower)
                                            dim_size = upper - lower + 1;
                                    }
                                }
                            }
                            /* Explicit "<lo>..<hi>" with named-constant bounds
                             * (e.g. "OS_F32..OS_F128").  sscanf("%lld..%lld") above
                             * only matches numeric bounds, and the unit-scope
                             * "uses"-imported symtab carries enum literals as
                             * HASHTYPE_CONST entries.  Split on ".." and resolve
                             * each side via the symbol table — needed for the
                             * inner dim of FPC's cgx86.pas convertopsse, whose
                             * bounds OS_F32..OS_F128 come from cgbase.pas. */
                            if (dim_size == 0) {
                                const char *dotdot = strstr(range_str, "..");
                                if (dotdot != NULL) {
                                    char left_buf[128], right_buf[128];
                                    size_t left_len = (size_t)(dotdot - range_str);
                                    if (left_len > 0 && left_len < sizeof(left_buf)) {
                                        memcpy(left_buf, range_str, left_len);
                                        left_buf[left_len] = '\0';
                                        const char *right_str = dotdot + 2;
                                        size_t right_len = strlen(right_str);
                                        if (right_len > 0 && right_len < sizeof(right_buf)) {
                                            memcpy(right_buf, right_str, right_len + 1);
                                            char *lp = left_buf;
                                            while (*lp == ' ' || *lp == '\t') lp++;
                                            char *rp = right_buf;
                                            while (*rp == ' ' || *rp == '\t') rp++;
                                            long long lo_val = 0, hi_val = 0;
                                            int lo_ok = 0, hi_ok = 0;
                                            HashNode_t *ln = NULL;
                                            if (FindSymbol(&ln, symtab, lp) != 0 &&
                                                ln != NULL &&
                                                (ln->hash_type == HASHTYPE_CONST ||
                                                 ln->is_typed_const)) {
                                                lo_val = ln->const_int_value;
                                                lo_ok = 1;
                                            }
                                            HashNode_t *rn = NULL;
                                            if (FindSymbol(&rn, symtab, rp) != 0 &&
                                                rn != NULL &&
                                                (rn->hash_type == HASHTYPE_CONST ||
                                                 rn->is_typed_const)) {
                                                hi_val = rn->const_int_value;
                                                hi_ok = 1;
                                            }
                                            if (lo_ok && hi_ok && hi_val >= lo_val)
                                                dim_size = hi_val - lo_val + 1;
                                        }
                                    }
                                }
                            }
                        }
                        if (dim_size <= 0) {
                            parsed_all = 0;
                            break;
                        }
                        product *= dim_size;
                        count++;
                        dim_node = dim_node->next;
                    }
                    if (parsed_all && count > kgpc_dim_count && product > 0 &&
                        element_size > 0 && product <= INT_MAX / element_size) {
                        int full = (int)(product * element_size);
                        if (full > total_size)
                            total_size = full;
                    }
                }

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
                                codegen_emit_bss_or_comm(ctx->output_file,
                                    (const char *)id_list->cur, generated_label,
                                    total_size, alignment,
                                    arr->defined_in_unit
                                    && !(tree->tree_data.arr_decl_data.is_typed_const &&
                                         codegen_typed_const_name_collides_ctx(ctx,
                                             (const char *)id_list->cur)));
                            }
                            label_to_use = generated_label;
                        }
                        {
                            char *storage_key = codegen_var_storage_key(ctx, symtab, tree, (const char *)id_list->cur);
                            add_static_array(storage_key != NULL ? storage_key : (char *)id_list->cur,
                                total_size, element_size, arr->s_range, label_to_use);
                            if (storage_key != NULL)
                                free(storage_key);
                        }
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
