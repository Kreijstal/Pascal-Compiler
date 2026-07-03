/*
    Damon Gwinn
    Code generation
    This is the dragon slayer

    See codegen.h for stack and implementation details
*/

#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/NameMangling.h"
#include "../../Parser/SemanticCheck/SemCheck.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../flags.h"
#include "abi_constants.h"
#include "codegen.h"
#include "codegen_expression.h"
#include "codegen_statement.h"
#include "codegen_string_set.h"
#include "codegen_symbol_resolution.h"
#include "expr_tree/expr_tree.h"
#include "register_types.h"
#include "stackmng/stackmng.h"
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "ir/ir_cfg.h"
#include "ir/ir_inst.h"
#include "ir/ir_liveness.h"
#include "ir/ir_peephole.h"
#if USE_GRAPH_COLORING_ALLOCATOR
#include "graph_coloring_allocator.h"
#endif

#include "codegen_subprograms_internal.h"
#include "codegen_vmt_internal.h"

#define CODEGEN_POINTER_SIZE_BYTES 8

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

/* An "assembler" subprogram is one whose entire body is an inline asm block.
 * For such routines the asm body is solely responsible for the return value
 * (FPC semantics): codegen must not emit a result-load epilogue, which would
 * either clobber the register the asm set (framed assembler routine) or read
 * from an uninitialized %rbp slot.  FPC accepts both the explicit `assembler`
 * directive and the bare `asm ... end` body form, so detect the body shape
 * rather than relying on the directive keyword surviving into the AST. */
static int subprogram_body_is_pure_asm(struct Statement *body) {
  if (body == NULL)
    return 0;
  if (body->type == STMT_ASM_BLOCK)
    return 1;
  if (body->type == STMT_COMPOUND_STATEMENT) {
    ListNode_t *cur = body->stmt_data.compound_statement;
    /* Pure-asm bodies wrap exactly one asm block.  A compound containing
     * Pascal statements alongside an asm block is a regular routine that
     * still uses the normal result-load epilogue. */
    int seen_asm = 0;
    while (cur != NULL) {
      if (cur->type == LIST_STMT && cur->cur != NULL) {
        struct Statement *child = (struct Statement *)cur->cur;
        if (child->type == STMT_ASM_BLOCK)
          seen_asm = 1;
        else
          return 0;
      }
      cur = cur->next;
    }
    return seen_asm;
  }
  return 0;
}

static const char *alloc_integer_arg_reg(int use_64bit, int *next_index) {
  if (next_index == NULL)
    return NULL;

  const char *reg = use_64bit ? current_arg_reg64(*next_index)
                              : current_arg_reg32(*next_index);
  if (reg == NULL)
    return NULL;

  ++(*next_index);
  return reg;
}

static const char *alloc_sse_arg_reg(int *next_index) {
  if (next_index == NULL)
    return NULL;

  const char *reg = current_arg_reg_xmm(*next_index);
  if (reg == NULL) {
    fprintf(stderr,
            "ERROR: Max SSE argument register limit exceeded (index=%d)\n",
            *next_index);
    exit(1);
  }

  ++(*next_index);
  return reg;
}

static inline struct TypeAlias *get_type_alias_from_node(HashNode_t *node) {
  return hashnode_get_type_alias(node);
}

static const char *codegen_resolve_record_type_name(HashNode_t *node,
                                                    SymTab_t *symtab) {
  if (node == NULL)
    return NULL;
  if (hashnode_is_record(node) && node->id != NULL)
    return node->id;
  struct TypeAlias *alias = get_type_alias_from_node(node);
  if (alias != NULL && alias->target_type_id != NULL && symtab != NULL) {
    HashNode_t *target = NULL;
    if (FindSymbol(&target, symtab, alias->target_type_id) != 0 &&
        target != NULL)
      return codegen_resolve_record_type_name(target, symtab);
  }
  return NULL;
}

static int codegen_return_storage_size(KgpcType *return_type);
static int codegen_return_type_id_storage_size(const char *return_type_id);
static int codegen_dynamic_array_element_size_from_type(CodeGenContext *ctx,
                                                        KgpcType *array_type);
static void add_alias_for_return_var(StackNode_t *return_var,
                                     const char *alias_label);
static void add_result_alias_for_return_var(StackNode_t *return_var);
static ListNode_t *codegen_store_class_typeinfo(ListNode_t *inst_list,
                                                CodeGenContext *ctx,
                                                StackNode_t *var_node,
                                                const char *type_name);
static ListNode_t *codegen_emit_tfile_configure(ListNode_t *inst_list,
                                                StackNode_t *file_node,
                                                long long element_size,
                                                int element_hash_tag);
static int codegen_resolve_file_component(const struct TypeAlias *alias,
                                          SymTab_t *symtab,
                                          long long *element_size_out,
                                          int *element_hash_tag_out);
ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list,
                                     CodeGenContext *ctx, SymTab_t *symtab);

ListNode_t *codegen_vect_reg(ListNode_t *inst_list, int num_vec) {
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

/* Returns the distance between the total declared size of value (non-var) float
 * parameters and KGPC's native float size (8 bytes per param).  A distance of 0
 * means all value float params are Double/Real (8 bytes) — the KGPC-native
 * size. Used by has_later_override to prefer Double sincos over Single or
 * Extended. */
int codegen_float_native_distance(Tree_t *sub) {
  int n_value = 0;
  int total_declared = 0;
  ListNode_t *p = sub->tree_data.subprogram_data.args_var;
  while (p != NULL) {
    Tree_t *decl = (Tree_t *)p->cur;
    if (decl != NULL && decl->type == TREE_VAR_DECL) {
      int is_var = decl->tree_data.var_decl_data.is_var_param;
      if (!is_var) {
        n_value++;
        const char *tid = decl->tree_data.var_decl_data.type_id;
        int sz = 8; /* default: double/real/float → 8 bytes (KGPC native) */
        if (tid != NULL) {
          if (pascal_identifier_equals(tid, "single"))
            sz = 4;
          else if (pascal_identifier_equals(tid, "extended") ||
                   pascal_identifier_equals(tid, "extended80"))
            sz = 16;
          /* double, real, float, currency, longreal → sz stays 8 */
        }
        total_declared += sz;
      }
    }
    p = p->next;
  }
  if (n_value == 0)
    return 0; /* no value params: distance 0 (use original later-wins rule) */
  int native = n_value * 8;
  int dist = total_declared - native;
  return dist < 0 ? -dist : dist;
}

/* Codegen for a list of subprograms */
void codegen_subprograms(ListNode_t *sub_list, CodeGenContext *ctx,
                         SymTab_t *symtab) {
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
#endif
  Tree_t *sub;

  assert(ctx != NULL);
  assert(symtab != NULL);

  while (sub_list != NULL) {
    sub = (Tree_t *)sub_list->cur;
    assert(sub != NULL);
    assert(sub->type == TREE_SUBPROGRAM);

    const char *mangled_id = sub->tree_data.subprogram_data.mangled_id;
    int trace_tfplistenum =
        getenv("KGPC_TRACE_TFPLISTENUM") != NULL && mangled_id != NULL &&
        strncasecmp(mangled_id, "tfplistenumerator__", 19) == 0;
    int trace_missing_calls =
        getenv("KGPC_TRACE_MISSING_CALLS") != NULL && mangled_id != NULL &&
        (strcasecmp(mangled_id, "format_us_a") == 0 ||
         strcasecmp(mangled_id, "format_s_a") == 0 ||
         strcasecmp(mangled_id, "codepagenametocodepage_s") == 0 ||
         strcasecmp(mangled_id, "stringofchar_c_li") == 0 ||
         strcasecmp(mangled_id, "stringofchar_c_i64") == 0);

    if (trace_tfplistenum || trace_missing_calls) {
      fprintf(stderr,
              "[codegen] sub=%s id=%s owner=%s method=%s body=%d used=%d "
              "template=%d\n",
              mangled_id,
              sub->tree_data.subprogram_data.id != NULL
                  ? sub->tree_data.subprogram_data.id
                  : "(null)",
              sub->tree_data.subprogram_data.owner_class != NULL
                  ? sub->tree_data.subprogram_data.owner_class
                  : "(null)",
              sub->tree_data.subprogram_data.method_name != NULL
                  ? sub->tree_data.subprogram_data.method_name
                  : "(null)",
              sub->tree_data.subprogram_data.statement_list != NULL,
              sub->tree_data.subprogram_data.is_used,
              sub->tree_data.subprogram_data.is_generic_template);
    }

    /* When --skip-unit-codegen is active, skip subprograms that belong
     * to a unit (source_unit_index != 0).  Their code comes from the
     * pre-compiled warmup .o file. */
    if (skip_unit_codegen_flag() &&
        sub->tree_data.subprogram_data.source_unit_index != 0) {
      sub_list = sub_list->next;
      continue;
    }

    if (mangled_id != NULL &&
        codegen_set_contains(&g_emitted_set, mangled_id)) {
      if (trace_tfplistenum || trace_missing_calls)
        fprintf(stderr, "[codegen] skip already emitted %s\n", mangled_id);
      sub_list = sub_list->next;
      continue;
    }

    if (sub->tree_data.subprogram_data.statement_list == NULL) {
      if (trace_tfplistenum || trace_missing_calls)
        fprintf(stderr, "[codegen] skip no body %s\n",
                mangled_id != NULL ? mangled_id : "(null)");
      sub_list = sub_list->next;
      continue;
    }

    /* If a LATER subprogram from the SAME UNIT has the same mangled_id and
     * a body, skip this one.  This handles platform-specific overrides:
     * e.g. Unix sysutils.pp defines FileExists(RawByteString) after the
     * generic filutil.inc version.  The later definition wins.
     *
     * Cross-unit: if a LATER subprogram from a unit that this function's
     * unit depends on (i.e. a more fundamental unit) has the same mangled_id,
     * prefer the fundamental unit's version.  This handles the case where a
     * wrapper unit (e.g. objpas) defines a same-named function that wraps
     * the system unit's implementation — both compile to the same mangled
     * name, but the system unit's version is the real implementation.
     * We check the actual dependency graph via unit_registry_is_dep()
     * instead of assuming lower source_unit_index = more fundamental. */
    if (mangled_id != NULL) {
      int this_unit = sub->tree_data.subprogram_data.source_unit_index;
      int has_later_override = 0;
      /* For float overloads (e.g. sincos Single/Double/Extended), prefer the
       * one whose value param sizes are closest to KGPC's native 8-byte float.
       * Only skip the current if a later same-unit same-mangled body has a
       * distance to native that is ≤ the current one's distance. */
      int current_dist = codegen_float_native_distance(sub);
      ListNode_t *later = sub_list->next;
      while (later != NULL) {
        if (later->type == LIST_TREE && later->cur != NULL) {
          Tree_t *later_sub = (Tree_t *)later->cur;
          int later_unit =
              later_sub->tree_data.subprogram_data.source_unit_index;
          if (later_sub->type == TREE_SUBPROGRAM &&
              later_sub->tree_data.subprogram_data.statement_list != NULL &&
              later_sub->tree_data.subprogram_data.mangled_id != NULL &&
              strcmp(later_sub->tree_data.subprogram_data.mangled_id,
                     mangled_id) == 0 &&
              (later_unit == this_unit ||
               (later_unit > 0 && this_unit > 0 &&
                unit_registry_is_dep(this_unit, later_unit)))) {
            int later_dist = codegen_float_native_distance(later_sub);
            if (later_dist <= current_dist) {
              has_later_override = 1;
              break;
            }
            /* later_dist > current_dist: the later body is farther from
             * KGPC's native float size (e.g. Single at dist=4 vs Double
             * at dist=0).  Don't let it override the current better one;
             * continue scanning for a yet-better later body. */
          }
        }
        later = later->next;
      }
      if (has_later_override) {
        if (trace_tfplistenum || trace_missing_calls)
          fprintf(stderr, "[codegen] skip later override %s\n", mangled_id);
        sub_list = sub_list->next;
        continue;
      }
    }

    /* Skip unused functions (dead code elimination / reachability pass). */
    if (!disable_dce_flag() && !sub->tree_data.subprogram_data.is_used) {
      if (trace_tfplistenum || trace_missing_calls)
        fprintf(stderr, "[codegen] skip dce-unused %s\n",
                mangled_id != NULL ? mangled_id : "(null)");
      sub_list = sub_list->next;
      continue;
    }

    /* Skip unspecialized generic subprogram templates. */
    if (sub->tree_data.subprogram_data.is_generic_template) {
      if (trace_tfplistenum || trace_missing_calls)
        fprintf(stderr, "[codegen] skip generic template %s\n",
                mangled_id != NULL ? mangled_id : "(null)");
      sub_list = sub_list->next;
      continue;
    }

    if (mangled_id != NULL) {
      ListNode_t *node = CreateListNode((void *)mangled_id, LIST_STRING);
      if (ctx->emitted_subprograms == NULL) {
        ctx->emitted_subprograms = node;
        g_emitted_tail = node;
      } else {
        g_emitted_tail->next = node;
        g_emitted_tail = node;
      }
      codegen_set_insert(&g_emitted_set, mangled_id);
    }

    /* When populating the codegen cache, buffer each function's output
     * so we can discard broken functions (those with codegen errors like
     * unresolved non-locals).  The broken section gets a ud2 stub;
     * --gc-sections removes it at link time. Also write successful unit
     * functions to ctx->cache_output for the cache artifact. */
#ifndef _WIN32
    if (codegen_cache_miss_flag()) {
      int source_unit_index = sub->tree_data.subprogram_data.source_unit_index;
      FILE *real_output = ctx->output_file;
      char *membuf = NULL;
      size_t membuf_size = 0;
      FILE *mem_output = open_memstream(&membuf, &membuf_size);
      ctx->output_file = mem_output;
      int had_error_before = ctx->had_error;
      ctx->had_error = 0;

      switch (sub->tree_data.subprogram_data.sub_type) {
      case TREE_SUBPROGRAM_PROC:
        codegen_procedure(sub, ctx, symtab);
        break;
      case TREE_SUBPROGRAM_FUNC:
        codegen_function(sub, ctx, symtab);
        break;
      default:
        assert(0 && "Unrecognized subprogram type in codegen!");
      }

      fflush(mem_output);
      fclose(mem_output);
      ctx->output_file = real_output;

      if (ctx->had_error) {
        /* Write the (broken) output to the main .s normally —
         * it's a used function, codegen error is reported. */
        fwrite(membuf, 1, membuf_size, real_output);
        /* Emit a ud2 stub in cache so the symbol exists but
         * broken code isn't cached. */
        if (mangled_id != NULL && ctx->cache_output != NULL &&
            source_unit_index != 0) {
          /* COFF/PE gas rejects ELF "ax",@progbits section syntax; keep
           * Windows code in a single .text (see codegen_function_header). */
          if (codegen_target_is_windows())
            fprintf(ctx->cache_output, "\t.text\n");
          else
            fprintf(ctx->cache_output,
                    "\t.section\t.text.%s,\"ax\",@progbits\n", mangled_id);
          fprintf(ctx->cache_output,
                  "\t.globl\t%s\n"
                  "%s:\n"
                  "\tud2\n",
                  mangled_id, mangled_id);
        }
        /* Reset register allocator to recover from leaked registers */
        reset_reg_stack();
      } else {
        /* Good output — write to real file */
        fwrite(membuf, 1, membuf_size, real_output);
        /* Write unit functions to cache output with per-function
         * section headers so --gc-sections can strip unused ones. */
        if (ctx->cache_output != NULL && source_unit_index != 0 &&
            mangled_id != NULL) {
          if (codegen_target_is_windows())
            fprintf(ctx->cache_output, "\t.text\n");
          else
            fprintf(ctx->cache_output,
                    "\t.section\t.text.%s,\"ax\",@progbits\n", mangled_id);
          fwrite(membuf, 1, membuf_size, ctx->cache_output);
        }
      }
      ctx->had_error = had_error_before;
      free(membuf);
    } else
#endif /* _WIN32 */
    {
      /* Codegen errors are per-subprogram: a failure inside one body
       * does not invalidate later bodies (and must not be observed by
       * downstream codegen of the next subprogram as a stale "had we
       * already errored?" gate).  Save and restore ctx->had_error
       * around each subprogram so the OR of all subprograms' errors is
       * surfaced to the caller while the in-flight flag accurately
       * reflects ONLY the current subprogram.  The cache-miss branch
       * above does the same dance for its own reasons; mirror it here
       * to prevent error state from leaking forward when codegen runs
       * directly (cache hit, Windows). */
      int had_error_before = (ctx != NULL) ? ctx->had_error : 0;
      if (ctx != NULL)
        ctx->had_error = 0;
      switch (sub->tree_data.subprogram_data.sub_type) {
      case TREE_SUBPROGRAM_PROC:
        codegen_procedure(sub, ctx, symtab);
        break;
      case TREE_SUBPROGRAM_FUNC:
        codegen_function(sub, ctx, symtab);
        break;
      default:
        assert(0 && "Unrecognized subprogram type in codegen!");
      }
      if (ctx != NULL)
        ctx->had_error = (ctx->had_error || had_error_before);
    }
    sub_list = sub_list->next;
  }
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
}

/* Code generation for a procedure */
void codegen_procedure(Tree_t *proc_tree, CodeGenContext *ctx,
                       SymTab_t *symtab) {
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

  if (codegen_runtime_owns_exported_symbol(sub_id) ||
      codegen_runtime_owns_exported_symbol(proc->cname_override)) {
    return;
  }

  /* The register allocator is global state (single reg_stack per process).
   * Without this reset, any registers that the previous function's codegen
   * left "in use" (because that function's body didn't perfectly free them)
   * remain marked as in-use here, and the current function's allocator
   * sees fewer free registers than reality — producing wrong codegen.
   * Previously this leak only got recovered when had_error tripped (line
   * with reset_reg_stack() in the cache-miss loop); making it
   * unconditional removes that hidden coupling. */
  reset_reg_stack();
  /* reset_reg_stack just destroyed every Register_t the previous subprogram
   * cached in ctx->static_link_reg.  Drop the dangling pointer (and the
   * spill-slot pointer from the previous scope) before any expression in
   * this subprogram can read it back as a live register. */
  codegen_invalidate_static_link_cache(ctx);

  const char *prev_sub_id = ctx->current_subprogram_id;
  const char *prev_sub_mangled = ctx->current_subprogram_mangled;
  const char *prev_sub_method_name = ctx->current_subprogram_method_name;
  const char *prev_result_name = ctx->current_subprogram_result_name;
  const char *prev_sub_owner_class = ctx->current_subprogram_owner_class;
  const char *prev_sub_owner_class_full =
      ctx->current_subprogram_owner_class_full;
  int prev_is_nonstatic_class_method =
      ctx->current_subprogram_is_nonstatic_class_method;
  ListNode_t *prev_sub_args = ctx->current_subprogram_args;
  ListNode_t *prev_sub_declarations = ctx->current_subprogram_declarations;
  StackNode_t *prev_return_slot = ctx->current_return_slot;
  KgpcType *prev_return_type = ctx->current_return_type;
  StackNode_t *prev_record_return_slot = ctx->current_record_return_slot;
  long long prev_record_return_size = ctx->current_record_return_size;
  int prev_callee_rbx = ctx->callee_save_rbx_offset;
  int prev_callee_r12 = ctx->callee_save_r12_offset;
  int prev_callee_r13 = ctx->callee_save_r13_offset;
  int prev_callee_r14 = ctx->callee_save_r14_offset;
  int prev_callee_r15 = ctx->callee_save_r15_offset;
  /* Save the managed dynamic-array temp tracking and start fresh.  Each
   * subprogram has its own frame, so rbp offsets do not survive across
   * subprogram boundaries; tracking is per-frame. */
  int *prev_dynarray_temp_offsets = ctx->managed_dynarray_temp_offsets;
  int prev_dynarray_temp_count = ctx->managed_dynarray_temp_count;
  int prev_dynarray_temp_capacity = ctx->managed_dynarray_temp_capacity;
  ctx->managed_dynarray_temp_offsets = NULL;
  ctx->managed_dynarray_temp_count = 0;
  ctx->managed_dynarray_temp_capacity = 0;
  /* While emitting THIS subprogram's body, identifier resolution for
   * cross-unit references (typed-consts, file-level consts) must prefer
   * the subprogram's own owning unit before falling back to the
   * program-wide symtab (which is last-write-wins for colliding names
   * across units, e.g. `msg` declared identically in two units, or
   * FPC's `ait_const2str` declared in both aggas.pas and agx86nsm.pas).
   * Save the caller's unit_index so it is restored after the body. */
  int prev_unit_index = symtab->current_unit_index;

  push_stackscope();
  inst_list = NULL;
  ctx->next_vreg_id = 0;

  /* Callee-save slots are allocated AFTER arguments and locals (below)
   * so that the t-section offsets account for the z and x section sizes. */
  if (proc_tree->tree_data.subprogram_data.nostackframe) {
    ctx->callee_save_rbx_offset = 0;
    ctx->callee_save_r12_offset = 0;
    ctx->callee_save_r13_offset = 0;
    ctx->callee_save_r14_offset = 0;
    ctx->callee_save_r15_offset = 0;
  }

  /* Static links are supported for nested procedures/functions (depth >= 1),
   * but NOT for:
   * - Top-level procedures (depth 0)
   * - Class methods (which have owner_class set)
   *
   * Class methods receive 'self' in the first register and should not use
   * static links. When there are parameters, the static link is passed in %rdi
   * and all arguments are shifted by one register position. */
  int num_args = (proc->args_var == NULL) ? 0 : ListLength(proc->args_var);
  ctx->current_subprogram_id = proc->id;
  ctx->current_subprogram_mangled = sub_id;
  ctx->current_subprogram_method_name = proc->method_name;
  ctx->current_subprogram_result_name = proc->result_var_name;
  ctx->current_subprogram_owner_class = proc->owner_class;
  ctx->current_subprogram_owner_class_full = proc->owner_class_full;
  ctx->current_subprogram_is_nonstatic_class_method =
      (proc->owner_class != NULL && proc->method_name != NULL &&
       from_cparser_is_method_nonstatic_class_method(proc->owner_class,
                                                     proc->method_name));
  ctx->current_subprogram_args = proc->args_var;
  ctx->current_return_slot = NULL;
  ctx->current_return_type = NULL;
  ctx->current_record_return_slot = NULL;
  ctx->current_record_return_size = 0;
  EnterScope(symtab, 0);
  codegen_register_owner_unit_scope(ctx, symtab, proc->source_unit_index);
  /* Bind unit-of-origin so per-unit lookups (e.g.
   * codegen_find_var_decl_for_unit for same-named typed-const arrays declared
   * in multiple units) target the subprogram's OWN unit while its body is being
   * emitted, not whichever unit was active when the program reached this
   * subprogram. */
  if (proc->source_unit_index > 0)
    symtab->current_unit_index = proc->source_unit_index;
  codegen_register_local_types(proc->type_declarations, symtab);
  codegen_register_decl_list(ctx, proc->args_var, symtab, 1);
  codegen_register_decl_list(ctx, proc->declarations, symtab, 0);
  codegen_register_const_decls(proc->const_declarations, symtab);
  int lexical_depth = proc->nesting_level;
  if (lexical_depth < 0)
    lexical_depth = codegen_get_lexical_depth(ctx) + 1;
  else if (lexical_depth <= 0 && ctx->current_subprogram_lexical_depth >= 0 &&
           ctx->current_subprogram_id != NULL)
    lexical_depth = ctx->current_subprogram_lexical_depth + 1;
  int prev_depth = ctx->current_subprogram_lexical_depth;
  int is_nested_function = proc->is_nested;
  if (lexical_depth <= 0 && is_nested_function) {
    lexical_depth = codegen_get_lexical_depth(ctx) + 1;
  }
  ctx->current_subprogram_lexical_depth = lexical_depth;
  int is_class_method = (proc->owner_class != NULL && !is_nested_function);
  StackNode_t *static_link = NULL;

  /* For class methods, register class vars with the stack manager */
  if (is_class_method)
    codegen_add_class_vars_for_method(ctx->current_subprogram_owner_class,
                                      ctx->current_subprogram_method_name,
                                      symtab, ctx);

  /* Process arguments first to allocate their stack space. Nested procedures
   * receive a static link only when they access an outer frame themselves or
   * must forward an outer frame to a nested child. */
  int will_need_static_link =
      (!is_class_method && is_nested_function &&
       (proc_tree->tree_data.subprogram_data.requires_static_link ||
        proc_tree->tree_data.subprogram_data.has_nested_requiring_link));

  /* If there are arguments and we'll need a static link, shift argument
   * registers by 1 */
  int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;
  /* For nostackframe functions, skip parameter saves — there is no frame,
   * so stores relative to %rbp would corrupt the caller's stack. */
  if (!proc_tree->tree_data.subprogram_data.nostackframe)
    inst_list = codegen_subprogram_arguments(proc->args_var, inst_list, ctx,
                                             symtab, arg_start_index);

  /* Now add static link after arguments to avoid overlap */
  if (will_need_static_link) {
    /* Reserve space for static link (parent's frame pointer) after arguments
     * This ensures it doesn't overlap with argument storage */
    static_link = add_l_x("__static_link__", 8);
    codegen_register_static_link_proc(ctx, sub_id, lexical_depth);
  }

  codegen_function_locals(proc->declarations, ctx, symtab);

  /* Allocate callee-save slots AFTER args (z) and locals (x) so that
   * the t-section offset = z_offset + x_offset + t_offset doesn't collide. */
  if (!proc_tree->tree_data.subprogram_data.nostackframe) {
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

  /* Recursively generate nested subprograms.  Save and restore
   * next_vreg_id, and reset the reg_stack so that stale vreg_ids from
   * nested compilations do not contaminate the outer function's register
   * tracking.  The instructions emitted so far already have register names
   * copied into them (not borrowed pointers), so resetting the reg_stack
   * is safe. */
  {
    int saved_vreg_id = ctx->next_vreg_id;
    codegen_subprograms(proc->subprograms, ctx, symtab);
    reset_reg_stack();
    /* The nested subprograms ran reset_reg_stack themselves, so any
     * static_link cache entry was either freed cleanly or just dangles
     * — invalidate here so this outer body never reads a stale pointer. */
    codegen_invalidate_static_link_cache(ctx);
    ctx->next_vreg_id = saved_vreg_id;
  }

  /* Set up asm parameter mapping for nostackframe functions.
     These functions skip the frame prologue, so inline asm should use
     ABI registers directly instead of stack offsets. */
  int prev_is_nostackframe = ctx->is_nostackframe;
  int prev_asm_param_count = ctx->asm_param_count;
  ctx->is_nostackframe = proc->nostackframe;
  ctx->asm_param_count = 0;
  if (proc->nostackframe && proc->args_var != NULL) {
    int pi = arg_start_index;
    ListNode_t *a = proc->args_var;
    while (a != NULL && pi < 16) {
      if (a->type == LIST_TREE && a->cur != NULL) {
        Tree_t *param = (Tree_t *)a->cur;
        if (param->type == TREE_VAR_DECL &&
            param->tree_data.var_decl_data.ids != NULL) {
          /* Determine parameter size for register width selection.
           * var/const params are always pointer-sized (8 bytes). */
          int param_size = 8;
          if (!param->tree_data.var_decl_data.is_var_param &&
              !param->tree_data.var_decl_data.is_const_param) {
            KgpcType *kt = param->tree_data.var_decl_data.cached_kgpc_type;
            if (kt != NULL) {
              long long sz = kgpc_type_sizeof(kt);
              if (sz == 1 || sz == 2 || sz == 4 || sz == 8)
                param_size = (int)sz;
            }
          }
          ListNode_t *id_node = param->tree_data.var_decl_data.ids;
          while (id_node != NULL && pi < 16) {
            if (id_node->cur != NULL) {
              ctx->asm_params[ctx->asm_param_count].name =
                  (const char *)id_node->cur;
              ctx->asm_params[ctx->asm_param_count].reg_index = pi;
              ctx->asm_params[ctx->asm_param_count].size_bytes = param_size;
              ctx->asm_param_count++;
              pi++;
            }
            id_node = id_node->next;
          }
        }
      }
      a = a->next;
    }
  }

  inst_list =
      codegen_var_initializers(proc->declarations, inst_list, ctx, symtab);
  ctx->current_subprogram_declarations = proc->declarations;
  inst_list = codegen_stmt(proc->statement_list, inst_list, ctx, symtab);

  /* Release the element-data buffer behind every managed dynamic-array
   * local before the frame is torn down.  STMT_EXIT branches out earlier
   * also emit this cleanup so all return paths honor the same contract. */
  inst_list = codegen_emit_managed_local_cleanup(inst_list, proc->declarations,
                                                 ctx, symtab);
  inst_list = codegen_emit_managed_dynarray_temp_cleanup(ctx, inst_list);

  if (proc->owner_class != NULL && proc->method_name != NULL &&
      pascal_identifier_equals(proc->owner_class, "TObject") &&
      pascal_identifier_equals(proc->method_name, "Free")) {
    int self_depth = 0;
    StackNode_t *self_var = find_label_with_depth("self", &self_depth);
    char buffer[128];
    const char *arg_reg = current_arg_reg64(0);

    if (self_var != NULL)
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
               self_var->offset, arg_reg);
    else
      snprintf(buffer, sizeof(buffer), "\tmovq\t-8(%%rbp), %s\n", arg_reg);
    inst_list = add_inst(inst_list, buffer);

    inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");

    /* Dispatch through the VMT to TObject.FreeInstance instead of
     * unconditionally invoking the libc free path.  FPC's RTL relies on
     * subclasses (notably tsymtable) overriding FreeInstance to
     * implement refcount-shared semantics: the destructor runs every
     * time `Free` is called but the storage is only released when the
     * refcount reaches zero.  Going through the VMT also routes the
     * actual free through `MemoryManager.FreeMem`, which pairs with
     * the allocator that the constructor used.
     *
     * We look up the FreeInstance slot dynamically from TObject's
     * method list so that the offset travels with the RecordType and
     * stays in sync if the VMT layout shifts. */
    int freeinstance_slot = -1;
    if (symtab != NULL) {
      struct RecordType *tobject =
          codegen_lookup_record_type_by_name(symtab, "TObject", 0);
      if (tobject != NULL) {
        for (ListNode_t *cur = tobject->methods; cur != NULL; cur = cur->next) {
          if (cur->cur == NULL)
            continue;
          struct MethodInfo *mi = (struct MethodInfo *)cur->cur;
          if (mi->name != NULL && mi->vmt_index >= 0 &&
              pascal_identifier_equals(mi->name, "FreeInstance")) {
            freeinstance_slot = mi->vmt_index;
            break;
          }
        }
      }
    }

    if (freeinstance_slot >= 0) {
      /* arg_reg already holds Self (loaded above using the ABI-correct
       * first GPR arg register — %rdi on SysV, %rcx on Win64).  The
       * body emitted above implements `if Self <> nil then Destroy`,
       * and on the nil branch falls through to here.  Guard the
       * FreeInstance dispatch with a Self <> nil check so a nil
       * receiver simply returns. */
      char skip_label[64];
      gen_label(skip_label, sizeof(skip_label), ctx);
      snprintf(buffer, sizeof(buffer), "\ttestq\t%s, %s\n", arg_reg, arg_reg);
      inst_list = add_inst(inst_list, buffer);
      {
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        kgpc_backend_target()->emit_branch(&em, BE_EQ, skip_label);
        inst_list = em.list;
      }

      /* Load VMT from (Self), then the method pointer from
       * <slot*VMT_SLOT_SIZE_BYTES>(VMT), and dispatch. */
      snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %%r11\n", arg_reg);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%r11), %%r11\n",
               freeinstance_slot * VMT_SLOT_SIZE_BYTES);
      inst_list = add_inst(inst_list, buffer);
      inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
      inst_list = codegen_call_with_shadow_space(inst_list, "*%r11");

      snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
      inst_list = add_inst(inst_list, buffer);
    } else {
      /* No TObject definition reachable (e.g. early bootstrap or a
       * standalone program that doesn't import objpas).  Fall back to
       * the libc free path so test programs without an RTL still build.
       * This branch is never taken when the FPC RTL is in scope. */
      inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_freemem");
    }
  }

  /* For constructors, return Self in %rax.
   * Constructors receive Self in the first parameter and should return it
   * to allow constructor chaining and assignment. */
  int is_constructor = proc->is_constructor;
  assert(!is_constructor ||
         proc->owner_class != NULL /* constructors must have an owner class */);

  if (is_constructor) {
    /* Constructors must return Self. Prefer a materialized receiver label
     * in the rebuilt scope, then the first explicit argument label, and
     * finally the hidden receiver slot at -8(%rbp). */
    int self_depth = 0;
    StackNode_t *self_var = find_label_with_depth("self", &self_depth);
    if (self_var == NULL) {
      ListNode_t *first_arg = proc->args_var;
      if (first_arg != NULL && first_arg->cur != NULL) {
        Tree_t *first_param = (Tree_t *)first_arg->cur;
        if (first_param != NULL && first_param->type == TREE_VAR_DECL) {
          struct Var *param_var = &first_param->tree_data.var_decl_data;
          if (param_var->ids != NULL && param_var->ids->cur != NULL) {
            char *param_id = (char *)param_var->ids->cur;
            self_var = find_label_with_depth(param_id, &self_depth);
          }
        }
      }
    }

    {
      char buffer[128];
      if (self_var != NULL)
        snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n",
                 self_var->offset);
      else
        snprintf(buffer, sizeof(buffer), "\tmovq\t-8(%%rbp), %%rax\n");
      inst_list = add_inst(inst_list, buffer);
    }
  }

  if (static_link != NULL) {
    char buffer[64];
    const char *link_reg = current_arg_reg64(0);
    ListNode_t *static_link_inst = NULL;
    assert(link_reg != NULL && "current_arg_reg64(0) should never return NULL");
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, -%d(%%rbp)\n", link_reg,
             static_link->offset);
    add_inst_invalidate_cache();
    static_link_inst = add_inst(static_link_inst, buffer);
    add_inst_invalidate_cache();
    inst_list = ConcatList(static_link_inst, inst_list);
  }

  codegen_emit_local_const_equivs(ctx, symtab);
  codegen_emit_const_decl_equivs_from_list(ctx, proc->const_declarations);
  codegen_function_header_ex_alias_vis(sub_id, ctx, proc->nostackframe,
                                       proc->cname_override,
                                       proc->defined_in_unit);
  if (!proc->nostackframe)
    codegen_stack_space_for_inst_list(inst_list, ctx);
#if USE_GRAPH_COLORING_ALLOCATOR
  ir_liveness_allocate(inst_list);
#endif
  ir_emit_function(inst_list);
#if USE_GRAPH_COLORING_ALLOCATOR
  ir_peephole_remove_redundant_moves(&inst_list);
#endif
  codegen_inst_list(inst_list, ctx);
  codegen_function_footer_ex(sub_id, ctx, proc->nostackframe);
  if (dump_ir_flag())
    ir_print_function(stderr, sub_id, inst_list);
  if (dump_ir_cfg_flag()) {
    Cfg_t *cfg = cfg_build(inst_list);
    cfg_print(stderr, cfg, sub_id);
    cfg_free(cfg);
  }
  if (dump_ir_liveness_flag()) {
    Cfg_t *cfg = cfg_build(inst_list);
    LivenessInfo_t *liveness = liveness_compute(cfg);
    liveness_print(stderr, cfg, liveness, sub_id);
    liveness_free(liveness);
    cfg_free(cfg);
  }
  /* Release any static-link register the body left cached.  Without this
   * the callee-save reg (rbx/r12-r15) the nested-frame walker pinned in
   * codegen_acquire_static_link survives into the next subprogram's
   * reset_reg_stack — that not only fires "Not all registers freed" but
   * leaves a dangling Register_t* in ctx->static_link_reg that the next
   * acquire can return verbatim when the level happens to match. */
  codegen_reset_static_link_cache(ctx);
  free_inst_list(inst_list);
  pop_stackscope();
  LeaveScope(symtab);
  /* Restore caller's unit-of-origin context. */
  symtab->current_unit_index = prev_unit_index;

  ctx->is_nostackframe = prev_is_nostackframe;
  ctx->asm_param_count = prev_asm_param_count;
  ctx->current_subprogram_id = prev_sub_id;
  ctx->current_subprogram_mangled = prev_sub_mangled;
  ctx->current_subprogram_method_name = prev_sub_method_name;
  ctx->current_subprogram_result_name = prev_result_name;
  ctx->current_subprogram_owner_class = prev_sub_owner_class;
  ctx->current_subprogram_owner_class_full = prev_sub_owner_class_full;
  ctx->current_subprogram_is_nonstatic_class_method =
      prev_is_nonstatic_class_method;
  ctx->current_subprogram_args = prev_sub_args;
  ctx->current_subprogram_declarations = prev_sub_declarations;
  ctx->current_return_slot = prev_return_slot;
  ctx->current_return_type = prev_return_type;
  ctx->current_record_return_slot = prev_record_return_slot;
  ctx->current_record_return_size = prev_record_return_size;
  ctx->current_subprogram_lexical_depth = prev_depth;
  ctx->callee_save_rbx_offset = prev_callee_rbx;
  ctx->callee_save_r12_offset = prev_callee_r12;
  ctx->callee_save_r13_offset = prev_callee_r13;
  ctx->callee_save_r14_offset = prev_callee_r14;
  ctx->callee_save_r15_offset = prev_callee_r15;
  /* Restore the outer subprogram's managed dynarray temp tracking.
   * Free this frame's buffer since its tracked offsets reference a
   * stack frame that no longer exists. */
  free(ctx->managed_dynarray_temp_offsets);
  ctx->managed_dynarray_temp_offsets = prev_dynarray_temp_offsets;
  ctx->managed_dynarray_temp_count = prev_dynarray_temp_count;
  ctx->managed_dynarray_temp_capacity = prev_dynarray_temp_capacity;

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
}

/* Code generation for a function */
void codegen_function(Tree_t *func_tree, CodeGenContext *ctx,
                      SymTab_t *symtab) {
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
  int prev_returns_dynamic_array = ctx->returns_dynamic_array;
  int prev_dynamic_array_descriptor_size = ctx->dynamic_array_descriptor_size;
  long long record_return_size = 0;

  func = &func_tree->tree_data.subprogram_data;
  sub_id = (func->mangled_id != NULL) ? func->mangled_id : func->id;

  if (codegen_runtime_owns_exported_symbol(sub_id) ||
      codegen_runtime_owns_exported_symbol(func->cname_override)) {
    return;
  }

  /* See comment in codegen_procedure: reset the global register allocator
   * so leaks from prior functions don't poison this function's codegen. */
  reset_reg_stack();
  /* reset_reg_stack just destroyed every Register_t the previous subprogram
   * cached in ctx->static_link_reg.  Drop the dangling pointer (and the
   * spill-slot pointer from the previous scope) before any expression in
   * this subprogram can read it back as a live register. */
  codegen_invalidate_static_link_cache(ctx);

  const char *prev_sub_id = ctx->current_subprogram_id;
  const char *prev_sub_mangled = ctx->current_subprogram_mangled;
  const char *prev_sub_method_name = ctx->current_subprogram_method_name;
  const char *prev_result_name = ctx->current_subprogram_result_name;
  const char *prev_sub_owner_class = ctx->current_subprogram_owner_class;
  const char *prev_sub_owner_class_full =
      ctx->current_subprogram_owner_class_full;
  int prev_is_nonstatic_class_method =
      ctx->current_subprogram_is_nonstatic_class_method;
  ListNode_t *prev_sub_args = ctx->current_subprogram_args;
  ListNode_t *prev_sub_declarations = ctx->current_subprogram_declarations;
  StackNode_t *prev_return_slot = ctx->current_return_slot;
  KgpcType *prev_return_type = ctx->current_return_type;
  StackNode_t *prev_record_return_slot = ctx->current_record_return_slot;
  long long prev_record_return_size = ctx->current_record_return_size;
  int prev_callee_rbx = ctx->callee_save_rbx_offset;
  int prev_callee_r12 = ctx->callee_save_r12_offset;
  int prev_callee_r13 = ctx->callee_save_r13_offset;
  int prev_callee_r14 = ctx->callee_save_r14_offset;
  int prev_callee_r15 = ctx->callee_save_r15_offset;
  /* Save per-function managed dynamic-array temp tracking (see
   * codegen_procedure for rationale). */
  int *prev_dynarray_temp_offsets = ctx->managed_dynarray_temp_offsets;
  int prev_dynarray_temp_count = ctx->managed_dynarray_temp_count;
  int prev_dynarray_temp_capacity = ctx->managed_dynarray_temp_capacity;
  ctx->managed_dynarray_temp_offsets = NULL;
  ctx->managed_dynarray_temp_count = 0;
  ctx->managed_dynarray_temp_capacity = 0;
  /* Save the caller's unit-index; we'll bind it to the function's own
   * source_unit_index below.  See codegen_procedure for rationale. */
  int prev_unit_index = symtab->current_unit_index;

  push_stackscope();
  inst_list = NULL;
  ctx->next_vreg_id = 0;

  /* Callee-save slots are allocated AFTER arguments and locals (below)
   * so that the t-section offsets account for the z and x section sizes. */
  if (func_tree->tree_data.subprogram_data.nostackframe) {
    ctx->callee_save_rbx_offset = 0;
    ctx->callee_save_r12_offset = 0;
    ctx->callee_save_r13_offset = 0;
    ctx->callee_save_r14_offset = 0;
    ctx->callee_save_r15_offset = 0;
  }

  /* Static links are supported for nested functions (depth >= 1), but NOT for:
   * - Top-level functions (depth 0)
   * - Class methods (which have owner_class set)
   *
   * Class methods receive 'self' in the first register and should not use
   * static links. When there are parameters, the static link is passed in %rdi
   * (or second register if function returns a record) and all arguments are
   * shifted accordingly. */
  int num_args = (func->args_var == NULL) ? 0 : ListLength(func->args_var);
  ctx->current_subprogram_id = func->id;
  ctx->current_subprogram_mangled = sub_id;
  ctx->current_subprogram_method_name = func->method_name;
  ctx->current_subprogram_result_name = func->result_var_name;
  ctx->current_subprogram_owner_class = func->owner_class;
  ctx->current_subprogram_owner_class_full = func->owner_class_full;
  ctx->current_subprogram_is_nonstatic_class_method =
      (func->owner_class != NULL && func->method_name != NULL &&
       from_cparser_is_method_nonstatic_class_method(func->owner_class,
                                                     func->method_name));
  ctx->current_subprogram_args = func->args_var;
  ctx->current_return_slot = NULL;
  ctx->current_return_type = NULL;
  ctx->current_record_return_slot = NULL;
  ctx->current_record_return_size = 0;
  EnterScope(symtab, 0);
  codegen_register_owner_unit_scope(ctx, symtab, func->source_unit_index);
  /* Bind unit-of-origin so per-unit lookups (e.g. for same-named
   * typed-const arrays declared in multiple units) target the
   * function's OWN unit while its body is being emitted. */
  if (func->source_unit_index > 0)
    symtab->current_unit_index = func->source_unit_index;
  codegen_register_local_types(func->type_declarations, symtab);
  codegen_register_decl_list(ctx, func->args_var, symtab, 1);
  codegen_register_decl_list(ctx, func->declarations, symtab, 0);
  codegen_register_const_decls(func->const_declarations, symtab);
  int lexical_depth = func->nesting_level;
  if (lexical_depth < 0)
    lexical_depth = codegen_get_lexical_depth(ctx) + 1;
  else if (lexical_depth <= 0 && ctx->current_subprogram_lexical_depth >= 0 &&
           ctx->current_subprogram_id != NULL)
    lexical_depth = ctx->current_subprogram_lexical_depth + 1;
  int prev_depth = ctx->current_subprogram_lexical_depth;
  int is_nested_function = func->is_nested;
  if (lexical_depth <= 0 && is_nested_function) {
    lexical_depth = codegen_get_lexical_depth(ctx) + 1;
  }
  ctx->current_subprogram_lexical_depth = lexical_depth;
  int is_class_method = (func->owner_class != NULL && !is_nested_function);
  StackNode_t *static_link = NULL;

  /* For class methods, register class vars with the stack manager */
  if (is_class_method)
    codegen_add_class_vars_for_method(ctx->current_subprogram_owner_class,
                                      ctx->current_subprogram_method_name,
                                      symtab, ctx);

  HashNode_t *func_node = NULL;

  if (symtab != NULL) {
    /* For overloaded functions, we need to find the correct overload by
     * matching the mangled name. FindIdent alone is insufficient because it
     * returns the first match, which might be a different overload. */
    if (func->mangled_id != NULL) {
      /* Try to find all identifiers with this name */
      ListNode_t *all_matches = FindAllIdents(symtab, func->id);
      ListNode_t *cur = all_matches;
      HashNode_t *same_mangled_same_unit = NULL;

      /* Find the one with matching mangled name */
      while (cur != NULL && func_node == NULL) {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate != NULL && candidate->type != NULL &&
            candidate->type->kind == TYPE_KIND_PROCEDURE &&
            candidate->type->info.proc_info.definition == func_tree) {
          func_node = candidate;
          break;
        }
        if (candidate != NULL && candidate->mangled_id != NULL &&
            strcmp(candidate->mangled_id, func->mangled_id) == 0) {
          if (candidate->source_unit_index == func->source_unit_index) {
            same_mangled_same_unit = candidate;
          } else if (func_node == NULL) {
            func_node = candidate;
          }
        }
        cur = cur->next;
      }

      if (same_mangled_same_unit != NULL)
        func_node = same_mangled_same_unit;

      if (all_matches != NULL)
        DestroyList(all_matches);
    }

    if (func_node != NULL && !func->defined_in_unit &&
        func->source_unit_index == 0 && func_node->source_unit_index != 0 &&
        func_node->type != NULL &&
        func_node->type->kind == TYPE_KIND_PROCEDURE &&
        func_node->type->info.proc_info.definition != func_tree) {
      func_node = NULL;
    }

    /* Fallback to simple lookup if no mangled name or no match found */
    if (func_node == NULL) {
      FindSymbol(&func_node, symtab, func->id);
      if (func_node != NULL && func_node->type != NULL &&
          func_node->type->kind == TYPE_KIND_PROCEDURE &&
          func_node->type->info.proc_info.definition != NULL &&
          func_node->type->info.proc_info.definition != func_tree) {
        ListNode_t *all_matches = FindAllIdents(symtab, func->id);
        ListNode_t *cur = all_matches;
        while (cur != NULL) {
          HashNode_t *candidate = (HashNode_t *)cur->cur;
          if (candidate != NULL && candidate->type != NULL &&
              candidate->type->kind == TYPE_KIND_PROCEDURE &&
              candidate->type->info.proc_info.definition == func_tree) {
            func_node = candidate;
            break;
          }
          cur = cur->next;
        }
        if (all_matches != NULL)
          DestroyList(all_matches);
      }
    }
  }

  /* Check if function returns a record by examining KgpcType */
  if (func_node != NULL && func_node->type != NULL &&
      func_node->type->kind == TYPE_KIND_PROCEDURE) {
    KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
    if (return_type != NULL) {
      if (kgpc_type_is_record(return_type)) {
        struct RecordType *record_desc = kgpc_type_get_record(return_type);
        /* A fieldless record/object is legitimately zero bytes (e.g. the FPC
         * compiler's `TCondRegs = object` with only a constructor/destructor).
         * Accept a successful size of 0 rather than treating it as a sizing
         * failure; has_record_return then stays false (Self is returned in a
         * register, not via a hidden sret pointer). */
        if (record_desc != NULL &&
            codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record_desc,
                                          &record_return_size) == 0 &&
            record_return_size >= 0 && record_return_size <= INT_MAX) {
          has_record_return = (record_return_size > 8);
        } else {
          long long fallback_size = kgpc_type_sizeof(return_type);
          if (fallback_size > 0 && fallback_size <= INT_MAX) {
            record_return_size = fallback_size;
            has_record_return = (record_return_size > 8);
          } else {
            codegen_report_error(ctx,
                                 "ERROR: Unable to determine size for record "
                                 "return value of %s.",
                                 func->id);
            record_return_size = 0;
          }
        }
      } else if (return_type->kind == TYPE_KIND_ARRAY) {
        if (kgpc_type_is_dynamic_array(return_type)) {
          returns_dynamic_array = 1;
          dynamic_array_element_size =
              codegen_dynamic_array_element_size_from_type(ctx, return_type);
          dynamic_array_descriptor_size =
              codegen_dynamic_array_descriptor_bytes(
                  dynamic_array_element_size);
          dynamic_array_lower_bound = return_type->info.array_info.start_index;
        } else {
          long long array_size = kgpc_type_sizeof(return_type);
          if (array_size > 0 && array_size <= INT_MAX) {
            has_record_return = (array_size > 8);
            record_return_size = array_size;
          } else {
            codegen_report_error(
                ctx,
                "ERROR: Unable to determine size for array return value of %s.",
                func->id);
            record_return_size = 0;
          }
        }
      }
    }
  } else if (func_node != NULL && func_node->type != NULL &&
             kgpc_type_is_record(func_node->type)) {
    struct RecordType *record = hashnode_get_record_type(func_node);
    if (record != NULL) {
      /* Get size from record */
      if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record,
                                        &record_return_size) != 0 ||
          record_return_size < 0 || record_return_size > INT_MAX) {
        long long fallback_size = kgpc_type_sizeof(func_node->type);
        if (fallback_size > 0 && fallback_size <= INT_MAX) {
          record_return_size = fallback_size;
          has_record_return = (record_return_size > 8);
        } else {
          codegen_report_error(
              ctx,
              "ERROR: Unable to determine size for record return value of %s.",
              func->id);
          record_return_size = 0;
        }
      } else {
        has_record_return = (record_return_size > 8);
      }
    }
  }

  /* Also check return_type_id from the function tree for functions with record
   * returns that weren't looked up in symbol table correctly (e.g., class
   * operators) */
  if (!has_record_return && func->return_type_id != NULL && symtab != NULL) {
    CODEGEN_DEBUG("DEBUG: Checking return_type_id='%s' for function '%s'\n",
                  func->return_type_id, func->id);
    HashNode_t *return_type_node = NULL;
    FindSymbol(&return_type_node, symtab, func->return_type_id);
    if (return_type_node != NULL) {
      CODEGEN_DEBUG("DEBUG: Found return type node\n");
      struct RecordType *record_type =
          hashnode_get_record_type(return_type_node);
      if (record_type != NULL) {
        CODEGEN_DEBUG("DEBUG: It's a record type!\n");
        if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record_type,
                                          &record_return_size) == 0 &&
            record_return_size > 0 && record_return_size <= INT_MAX) {
          CODEGEN_DEBUG("DEBUG: Setting has_record_return=1, size=%lld\n",
                        record_return_size);
          has_record_return = (record_return_size > 8);
        }
      } else if (return_type_node->type != NULL &&
                 return_type_node->type->kind != TYPE_KIND_ARRAY) {
        long long value_size = kgpc_type_sizeof(return_type_node->type);
        if (value_size > 0 && value_size <= INT_MAX) {
          has_record_return = (value_size > 8);
          record_return_size = value_size;
        }
      } else if (return_type_node->type != NULL &&
                 return_type_node->type->kind == TYPE_KIND_ARRAY &&
                 !kgpc_type_is_dynamic_array(return_type_node->type)) {
        long long array_size = kgpc_type_sizeof(return_type_node->type);
        if (array_size > 0 && array_size <= INT_MAX) {
          has_record_return = 1;
          record_return_size = array_size;
        } else {
          codegen_report_error(
              ctx,
              "ERROR: Unable to determine size for array return value of %s.",
              func->id);
          record_return_size = 0;
        }
      }
    } else {
      CODEGEN_DEBUG("DEBUG: return_type_node is NULL\n");
    }
  } else {
    CODEGEN_DEBUG("DEBUG: Skipped return_type_id check: has_record_return=%d, "
                  "return_type_id=%s, symtab=%p\n",
                  has_record_return,
                  func->return_type_id ? func->return_type_id : "NULL",
                  (void *)symtab);
  }

  /* Also check inline_return_type from the function tree for functions with
   * inline record returns */
  if (!has_record_return && func->inline_return_type != NULL &&
      func->inline_return_type->base_type == RECORD_TYPE) {
    struct RecordType *inline_record = NULL;
    KgpcType *inline_kgpc = func->inline_return_type->kgpc_type;

    if (inline_kgpc != NULL && kgpc_type_is_record(inline_kgpc)) {
      inline_record = kgpc_type_get_record(inline_kgpc);
    }

    if (inline_record == NULL &&
        func->inline_return_type->target_type_id != NULL && symtab != NULL) {
      HashNode_t *inline_type_node = NULL;
      FindSymbol(&inline_type_node, symtab,
                 func->inline_return_type->target_type_id);
      if (inline_type_node != NULL)
        inline_record = hashnode_get_record_type(inline_type_node);
    }

    if (inline_record != NULL &&
        codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, inline_record,
                                      &record_return_size) == 0 &&
        record_return_size > 0 && record_return_size <= INT_MAX) {
      has_record_return = (record_return_size > 8);
    } else if (inline_kgpc != NULL) {
      long long inline_size = kgpc_type_sizeof(inline_kgpc);
      if (inline_size > 0 && inline_size <= INT_MAX) {
        record_return_size = inline_size;
        has_record_return = (record_return_size > 8);
      }
    }
  }

  if (!has_record_return && func->inline_return_type != NULL &&
      func->inline_return_type->kgpc_type != NULL &&
      func->inline_return_type->kgpc_type->kind == TYPE_KIND_ARRAY &&
      !kgpc_type_is_dynamic_array(func->inline_return_type->kgpc_type)) {
    long long array_size =
        kgpc_type_sizeof(func->inline_return_type->kgpc_type);
    if (array_size > 0 && array_size <= INT_MAX) {
      has_record_return = (array_size > 8);
      record_return_size = array_size;
    } else {
      codegen_report_error(
          ctx, "ERROR: Unable to determine size for array return value of %s.",
          func->id);
      record_return_size = 0;
    }
  }

  /* Resolve dynamic array return types that were not found via func_node,
   * especially for class methods returning aliased dynamic arrays
   * (e.g. TUnicodeCharArray). */
  if (!returns_dynamic_array && func->return_type_id != NULL &&
      symtab != NULL) {
    HashNode_t *return_type_node = NULL;
    FindSymbol(&return_type_node, symtab, func->return_type_id);
    if (return_type_node != NULL) {
      KgpcType *return_type = return_type_node->type;
      if (return_type == NULL) {
        struct TypeAlias *alias = hashnode_get_type_alias(return_type_node);
        if (alias != NULL)
          return_type = create_kgpc_type_from_type_alias(alias, symtab, 0);
      }
      if (return_type != NULL && return_type->kind == TYPE_KIND_ARRAY &&
          kgpc_type_is_dynamic_array(return_type)) {
        returns_dynamic_array = 1;
        dynamic_array_element_size =
            codegen_dynamic_array_element_size_from_type(ctx, return_type);
        dynamic_array_descriptor_size =
            codegen_dynamic_array_descriptor_bytes(dynamic_array_element_size);
        dynamic_array_lower_bound = return_type->info.array_info.start_index;
      }
    }
  }

  if (!has_record_return && func_node != NULL && func_node->type != NULL) {
    KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
    if (return_type != NULL && kgpc_type_is_shortstring(return_type)) {
      long long shortstring_size = kgpc_type_sizeof(return_type);
      has_record_return = 1;
      record_return_size = shortstring_size > 0 ? shortstring_size : 256;
    }
  }
  /* Fallback: if the AST node's return_type tag is SHORTSTRING_TYPE (set during
   * AST conversion under {$H-}), but the KgpcType on the symbol table didn't
   * reflect it (because semcheck runs with the flag reset), force sret and
   * patch the KgpcType so body codegen uses value-type access. */
  if (!has_record_return && func->return_type == SHORTSTRING_TYPE) {
    has_record_return = 1;
    record_return_size = 256;
  }
  if (func->return_type == SHORTSTRING_TYPE && func_node != NULL &&
      func_node->type != NULL && func_node->type->kind == TYPE_KIND_PROCEDURE) {
    KgpcType *ret = func_node->type->info.proc_info.return_type;
    if (ret != NULL && ret->kind == TYPE_KIND_PRIMITIVE &&
        kgpc_type_get_primitive_tag(ret) == STRING_TYPE) {
      /* Patch STRING -> SHORTSTRING so body codegen uses value semantics */
      KgpcType *shortstring_type = codegen_canonical_shortstring_type();
      kgpc_type_retain(shortstring_type);
      kgpc_type_release(ret);
      func_node->type->info.proc_info.return_type = shortstring_type;
    } else if (ret == NULL) {
      KgpcType *shortstring_type = codegen_canonical_shortstring_type();
      kgpc_type_retain(shortstring_type);
      func_node->type->info.proc_info.return_type = shortstring_type;
    }
  }

  /* Set ctx->current_return_type AFTER the SHORTSTRING_TYPE patch above,
   * so that body codegen (e.g. `FuncName := expr`) sees the corrected
   * return type and routes through ShortString assignment paths instead
   * of kgpc_string_assign_from_shortstring (which expects char** semantics). */
  if (func_node != NULL && func_node->type != NULL &&
      func_node->type->kind == TYPE_KIND_PROCEDURE)
    ctx->current_return_type = kgpc_type_get_return_type(func_node->type);

  /* For nested functions (or any function where func_node lookup failed),
   * the symbol table may not contain the function, leaving
   * ctx->current_return_type as NULL.  When the AST says the return type
   * is SHORTSTRING_TYPE, synthesize a KgpcType so that body codegen
   * (e.g. `FuncName := expr`) correctly identifies the function result
   * variable as ShortString and avoids kgpc_string_assign_from_shortstring
   * which expects char** (AnsiString variable) semantics. */
  if (ctx->current_return_type == NULL && func->return_type == SHORTSTRING_TYPE)
    ctx->current_return_type = codegen_canonical_shortstring_type();

  /* An advanced-record constructor returns Self (the constructed instance) in
   * %rax, exactly like a class constructor, and never through a hidden
   * by-value record sret pointer. For such a constructor whose record exceeds
   * 8 bytes the return-type checks above would otherwise set has_record_return,
   * inserting an sret parameter that shifts Self out of argument register 0 and
   * corrupts every call site (the call sites pass the destination directly as
   * Self). Suppress the record-return/sret treatment so the >8-byte case
   * behaves like the <=8-byte case, which already returns Self in %rax.
   *
   * TP `object` constructors are excluded: they keep the sret+Self convention
   * that New(p, Ctor(...)) and instance-receiver calls rely on. */
  if (func->is_constructor && has_record_return && func->owner_class != NULL &&
      symtab != NULL) {
    HashNode_t *owner_node = NULL;
    struct RecordType *owner_rec = NULL;
    if (FindSymbol(&owner_node, symtab, func->owner_class) != 0 &&
        owner_node != NULL)
      owner_rec = hashnode_get_record_type(owner_node);
    if (owner_rec == NULL || !owner_rec->is_object) {
      has_record_return = 0;
      record_return_size = 0;
    }
  }

  int will_need_static_link =
      (!is_class_method && is_nested_function &&
       (func_tree->tree_data.subprogram_data.requires_static_link ||
        func_tree->tree_data.subprogram_data.has_nested_requiring_link));

  /* Calculate argument start index:
   * - If function returns record: use index 1 (record pointer in first arg)
   * - If function will need static link: add 1 for static link
   * - Otherwise: use index 0 */
  int arg_start_index = has_record_return ? 1 : 0;
  if (will_need_static_link && num_args > 0)
    arg_start_index++;

  /* For nostackframe functions, skip parameter saves — there is no frame,
   * so stores relative to %rbp would corrupt the caller's stack. */
  if (!func_tree->tree_data.subprogram_data.nostackframe)
    inst_list = codegen_subprogram_arguments(func->args_var, inst_list, ctx,
                                             symtab, arg_start_index);

  /* Add static link after arguments to avoid stack overlap */
  if (will_need_static_link) {
    /* Reserve space for static link after arguments */
    static_link = add_l_x("__static_link__", 8);
    codegen_register_static_link_proc(ctx, sub_id, lexical_depth);
  }

  int return_size = DOUBLEWORD;
  if (returns_dynamic_array)
    return_size = dynamic_array_descriptor_size;
  else if (has_record_return)
    return_size = (int)record_return_size;
  else if (func->return_type_id != NULL) {
    int return_type_id_size =
        codegen_return_type_id_storage_size(func->return_type_id);
    if (return_type_id_size > 0)
      return_size = return_type_id_size;
  }
  if (return_size == DOUBLEWORD && func_node != NULL &&
      func_node->type != NULL && func_node->type->kind == TYPE_KIND_PROCEDURE) {
    /* Get return type from KgpcType */
    KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
    if (return_type != NULL) {
      struct TypeAlias *alias = kgpc_type_get_type_alias(return_type);
      if (alias != NULL && alias->target_type_id != NULL &&
          pascal_identifier_equals(alias->target_type_id, "Single")) {
        return_size = 4; /* Single is 4 bytes */
      } else {
        return_size = codegen_return_storage_size(return_type);
      }
    }
  }
  if (return_size == DOUBLEWORD && func_node != NULL &&
      func_node->type != NULL && func_node->type->kind == TYPE_KIND_PRIMITIVE) {
    int tag = kgpc_type_get_primitive_tag(func_node->type);
    struct TypeAlias *alias = kgpc_type_get_type_alias(func_node->type);
    if (alias != NULL && alias->storage_size > 0)
      return_size = (int)alias->storage_size;
    else if (tag == EXTENDED_TYPE)
      return_size = 10;
    else if (tag == REAL_TYPE || tag == STRING_TYPE || tag == POINTER_TYPE ||
             tag == INT64_TYPE)
      return_size = 8;
    else if (tag == LONGINT_TYPE)
      return_size = DOUBLEWORD; /* 32-bit FPC-compatible LongInt */
    else if (tag == BOOL)
      return_size = DOUBLEWORD;
  }
  if (return_size == DOUBLEWORD && func_node != NULL &&
      func_node->type != NULL && func_node->type->kind == TYPE_KIND_POINTER) {
    return_size = 8;
  }

  /* Fallback for nested functions: func_node is looked up by name in the
   * global symtab, but a nested function lives in its parent's local scope
   * and is not found there, so func_node stays NULL and every size
   * correction above is skipped — leaving an 8-byte class/pointer return
   * defaulted to DOUBLEWORD, which truncates the result pointer to 32 bits
   * in the return-value load (movl instead of movq).  The return *type*,
   * however, is globally visible regardless of function nesting, so resolve
   * it by name and take its storage size.  Only widens when the resolved
   * type is genuinely larger (class/pointer/Int64), so scalar returns are
   * unaffected. */
  if (return_size == DOUBLEWORD && !has_record_return &&
      !returns_dynamic_array && func->return_type_id != NULL &&
      symtab != NULL) {
    HashNode_t *rt_node = NULL;
    FindSymbol(&rt_node, symtab, func->return_type_id);
    if (rt_node != NULL && rt_node->type != NULL) {
      int rt_size = codegen_return_storage_size(rt_node->type);
      if (rt_size > return_size)
        return_size = rt_size;
    }
  }

  if (returns_dynamic_array)
    return_var = add_dynamic_array(func->id, dynamic_array_element_size,
                                   dynamic_array_lower_bound, 0, NULL);
  else
    return_var = add_l_x(func->id, return_size);

  /* Store dynamic array return info in context for exit statement handling */
  ctx->returns_dynamic_array = returns_dynamic_array;
  ctx->dynamic_array_descriptor_size = dynamic_array_descriptor_size;

  /* Allow Delphi-style Result alias in regular functions too. */
  add_result_alias_for_return_var(return_var);
  ctx->current_return_slot = return_var;

  if (func->result_var_name != NULL &&
      !pascal_identifier_equals(func->result_var_name, func->id) &&
      !pascal_identifier_equals(func->result_var_name, "Result")) {
    add_alias_for_return_var(return_var, func->result_var_name);
  }
  /* For class methods, also alias the unmangled method name to the return slot
   */
  if (func->method_name != NULL) {
    add_alias_for_return_var(return_var, func->method_name);
  }

  if (has_record_return)
    return_dest_slot = add_l_x("__record_return_dest__", (int)sizeof(void *));

  /* Make the SRET destination slot visible to EXIT statements so they
   * memcpy the local Result into the caller's buffer instead of loading
   * a single qword.  Without this, an early `Exit` from a ShortString- or
   * record-returning function leaves the caller's buffer untouched and
   * returns 8 stale bytes in %rax. */
  if (has_record_return && return_dest_slot != NULL) {
    ctx->current_record_return_slot = return_dest_slot;
    ctx->current_record_return_size = record_return_size;
  }

  if (has_record_return && return_dest_slot != NULL) {
    const char *ret_reg = current_arg_reg64(0);
    if (ret_reg != NULL) {
      char ptr_buffer[64];
      snprintf(ptr_buffer, sizeof(ptr_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
               ret_reg, return_dest_slot->offset);
      ListNode_t *record_return_inst = NULL;
      add_inst_invalidate_cache(); /* switching to different list */
      record_return_inst = add_inst(record_return_inst, ptr_buffer);
      add_inst_invalidate_cache(); /* ConcatList changes head */
      inst_list = ConcatList(record_return_inst, inst_list);
    }
  }

  /* Constructors routed through codegen_function must return Self.
   * Initialize the return variable with Self (-8(%rbp)) so that even if
   * the constructor body doesn't explicitly set Result, the return value
   * is the allocated instance pointer (not zero). */
  {
    int func_is_constructor = func->is_constructor;
    assert(!func_is_constructor ||
           func->owner_class !=
               NULL /* constructors must have an owner class */);
    if (func_is_constructor && return_var != NULL) {
      {
        /* Integrated: load from the frame slot into a physical register via the
         * vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64, {.mem_frame = {BE_BASE_FP, -8}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(return_var->offset)}}};
        BeOperand a = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
    }
  }

  codegen_function_locals(func->declarations, ctx, symtab);

  /* Allocate callee-save slots AFTER args (z) and locals (x) so that
   * the t-section offset = z_offset + x_offset + t_offset doesn't collide. */
  if (!func_tree->tree_data.subprogram_data.nostackframe) {
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

  /* Recursively generate nested subprograms.  Save and restore
   * next_vreg_id, and reset the reg_stack so that stale vreg_ids from
   * nested compilations do not contaminate the outer function's register
   * tracking.  The instructions emitted so far already have register names
   * copied into them (not borrowed pointers), so resetting the reg_stack
   * is safe. */
  {
    int saved_vreg_id = ctx->next_vreg_id;
    int saved_returns_dynamic_array = ctx->returns_dynamic_array;
    int saved_dynamic_array_descriptor_size =
        ctx->dynamic_array_descriptor_size;
    codegen_subprograms(func->subprograms, ctx, symtab);
    ctx->returns_dynamic_array = saved_returns_dynamic_array;
    ctx->dynamic_array_descriptor_size = saved_dynamic_array_descriptor_size;
    reset_reg_stack();
    codegen_invalidate_static_link_cache(ctx);
    ctx->next_vreg_id = saved_vreg_id;
  }

  /* Set up asm parameter mapping for nostackframe functions. */
  int prev_is_nostackframe = ctx->is_nostackframe;
  int prev_asm_param_count = ctx->asm_param_count;
  ctx->is_nostackframe = func->nostackframe;
  ctx->asm_param_count = 0;
  if (func->nostackframe && func->args_var != NULL) {
    int pi = arg_start_index;
    ListNode_t *a = func->args_var;
    while (a != NULL && pi < 16) {
      if (a->type == LIST_TREE && a->cur != NULL) {
        Tree_t *param = (Tree_t *)a->cur;
        if (param->type == TREE_VAR_DECL &&
            param->tree_data.var_decl_data.ids != NULL) {
          /* Determine parameter size for register width selection.
           * var/const params are always pointer-sized (8 bytes). */
          int param_size = 8;
          if (!param->tree_data.var_decl_data.is_var_param &&
              !param->tree_data.var_decl_data.is_const_param) {
            KgpcType *kt = param->tree_data.var_decl_data.cached_kgpc_type;
            if (kt != NULL) {
              long long sz = kgpc_type_sizeof(kt);
              if (sz == 1 || sz == 2 || sz == 4 || sz == 8)
                param_size = (int)sz;
            }
          }
          ListNode_t *id_node = param->tree_data.var_decl_data.ids;
          while (id_node != NULL && pi < 16) {
            if (id_node->cur != NULL) {
              ctx->asm_params[ctx->asm_param_count].name =
                  (const char *)id_node->cur;
              ctx->asm_params[ctx->asm_param_count].reg_index = pi;
              ctx->asm_params[ctx->asm_param_count].size_bytes = param_size;
              ctx->asm_param_count++;
              pi++;
            }
            id_node = id_node->next;
          }
        }
      }
      a = a->next;
    }
  }

  inst_list =
      codegen_var_initializers(func->declarations, inst_list, ctx, symtab);
  ctx->current_subprogram_declarations = func->declarations;
  inst_list = codegen_stmt(func->statement_list, inst_list, ctx, symtab);

  /* Release the element-data buffer behind every managed dynamic-array
   * local before the frame is torn down.  STMT_EXIT branches out earlier
   * also emit this cleanup so all return paths honor the same contract.
   *
   * Since commit 7c840d0a dynarray assignment is a deep copy, so even
   * `Result := localvar` gives Result its own independent buffer.
   * User-declared locals can therefore be finalized here regardless of
   * whether the function returns a dynamic array.  The Result slot itself
   * is NOT included in func->declarations (it is added to the stack
   * separately via add_dynamic_array), so codegen_emit_managed_local_cleanup
   * never touches it — the kgpc_dynarray_clone_descriptor call below
   * transfers its buffer to the caller safely. */
  inst_list = codegen_emit_managed_local_cleanup(inst_list, func->declarations,
                                                 ctx, symtab);
  inst_list = codegen_emit_managed_dynarray_temp_cleanup(ctx, inst_list);

  /* For assembler functions, the asm block handles the return value
   * entirely.  Skip the compiler-generated return-value epilogue: it would
   * overwrite the register the asm body set (for a framed assembler routine)
   * or read from an invalid %rbp offset (for a nostackframe one). */
  if (func->nostackframe || func->is_assembler ||
      subprogram_body_is_pure_asm(func->statement_list)) {
    /* Skip return value loading — asm is responsible */
  } else if (returns_dynamic_array) {
#if KGPC_ENABLE_REG_DEBUG
    const char *prev_reg_ctx = g_reg_debug_context;
    g_reg_debug_context = "dyn_array_return";
#endif
    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL)
      addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (addr_reg == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to allocate register for dynamic array return.");
    } else {
      {
        /* Integrated: address-of the frame slot into a pool register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(return_var->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }

      if (codegen_target_is_windows()) {
        {
          /* Integrated: emit the arg move through the target-neutral backend
           * vtable instead of a raw AT&T template (byte-identical output). */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
          BeOperand src = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
          kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%edx\n",
                 dynamic_array_descriptor_size);
        inst_list = add_inst(inst_list, buffer);
      } else {
        {
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
          BeOperand src = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
          kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %%esi\n",
                 dynamic_array_descriptor_size);
        inst_list = add_inst(inst_list, buffer);
      }

      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(
          inst_list, "kgpc_dynarray_clone_descriptor");
      free_arg_regs();
      free_reg(get_reg_stack(), addr_reg);
    }
#if KGPC_ENABLE_REG_DEBUG
    g_reg_debug_context = prev_reg_ctx;
#endif
  } else if (has_record_return && return_dest_slot != NULL &&
             record_return_size > 0) {
    Register_t *dest_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (dest_reg == NULL)
      dest_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    Register_t *src_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (src_reg == NULL)
      src_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (size_reg == NULL)
      size_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (dest_reg == NULL || src_reg == NULL || size_reg == NULL) {
      if (dest_reg != NULL)
        free_reg(get_reg_stack(), dest_reg);
      if (src_reg != NULL)
        free_reg(get_reg_stack(), src_reg);
      if (size_reg != NULL)
        free_reg(get_reg_stack(), size_reg);
      codegen_report_error(
          ctx, "ERROR: Unable to allocate registers for record return copy.");
    } else {
      {
        /* Integrated: load from the frame slot through the backend vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_VREG, BE_W64, {.vreg = dest_reg}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(return_dest_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }

      {
        /* Integrated: address-of the frame slot into a pool register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_VREG, BE_W64, {.vreg = src_reg}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(return_var->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }

      {
        char tmpl[64];
        snprintf(tmpl, sizeof(tmpl), "\tmovq\t$%lld, %%0\n",
                 record_return_size);
        Register_t *defs_arr[] = {size_reg};
        inst_list = add_inst_du(inst_list, ctx, defs_arr, 1, NULL, 0, tmpl);
      }

      if (codegen_target_is_windows()) {
        /* Move dest/src before size to avoid clobbering %r8. */
        {
          Register_t *u[] = {dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rcx\n");
        }
        {
          Register_t *u[] = {src_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
        }
        {
          Register_t *u[] = {size_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %r8\n");
        }
      } else {
        /* Move in reverse order to avoid register conflicts when temp regs
         * overlap with arg regs */
        {
          Register_t *u[] = {size_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdx\n");
        }
        {
          Register_t *u[] = {src_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rsi\n");
        }
        {
          Register_t *u[] = {dest_reg};
          inst_list =
              add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, %rdi\n");
        }
      }

      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
      free_arg_regs();

      {
        /* Integrated: load from the frame slot into a physical register via the
         * vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = RETURN_REG_64}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(return_dest_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }

      free_reg(get_reg_stack(), dest_reg);
      free_reg(get_reg_stack(), src_reg);
      free_reg(get_reg_stack(), size_reg);
    }
  } else {
    /* Determine if return type is Real (floating-point) */
    int is_real_return = 0;
    if (func_node != NULL && func_node->type != NULL &&
        func_node->type->kind == TYPE_KIND_PROCEDURE) {
      KgpcType *return_type = kgpc_type_get_return_type(func_node->type);
      if (return_type != NULL && return_type->kind == TYPE_KIND_PRIMITIVE) {
        int tag = kgpc_type_get_primitive_tag(return_type);
        if (is_real_family_type(tag))
          is_real_return = 1;
      }
    } else if (func_node != NULL && func_node->type != NULL &&
               func_node->type->kind == TYPE_KIND_PRIMITIVE) {
      int tag = kgpc_type_get_primitive_tag(func_node->type);
      if (is_real_family_type(tag))
        is_real_return = 1;
    }

    /* Use movss for Single (4-byte), movsd for Double/Real (8-byte), return in
     * xmm0. Check element_size which stores the unaligned size, not size which
     * may be padded. */
    long long unaligned_return_size = return_var->element_size > 0
                                          ? return_var->element_size
                                          : return_var->size;
    if (is_real_return && return_var->element_size == 10)
      snprintf(buffer, 50, "\tfldt\t-%d(%%rbp)\n", return_var->offset);
    else if (is_real_return && unaligned_return_size <= 4)
      snprintf(buffer, 50, "\tmovss\t-%d(%%rbp), %%xmm0\n", return_var->offset);
    else if (is_real_return)
      snprintf(buffer, 50, "\tmovsd\t-%d(%%rbp), %%xmm0\n", return_var->offset);
    else {
      /* Use actual return type size (not stack slot size which may be
       * padded) to choose movl vs movq.  A 4-byte record allocated in
       * an 8-byte slot would otherwise read 4 bytes of garbage. */
      long long actual_return_size = return_var->element_size > 0
                                         ? return_var->element_size
                                         : return_var->size;
      if (func->return_type_id != NULL) {
        int return_type_id_size =
            codegen_return_type_id_storage_size(func->return_type_id);
        if (return_type_id_size > 0)
          actual_return_size = return_type_id_size;
      }
      if (func_node != NULL && func_node->type != NULL) {
        KgpcType *ret_type = kgpc_type_get_return_type(func_node->type);
        if (ret_type != NULL) {
          actual_return_size = codegen_return_storage_size(ret_type);
        }
      }
      if (actual_return_size >= 8)
        snprintf(buffer, 50, "\tmovq\t-%d(%%rbp), %s\n", return_var->offset,
                 RETURN_REG_64);
      else
        snprintf(buffer, 50, "\tmovl\t-%d(%%rbp), %s\n", return_var->offset,
                 RETURN_REG_32);
    }
    inst_list = add_inst(inst_list, buffer);
  }

  if (static_link != NULL) {
    char link_buffer[64];
    const char *link_reg = current_arg_reg64(has_record_return ? 1 : 0);
    ListNode_t *static_link_inst = NULL;
    assert(link_reg != NULL &&
           "current_arg_reg64() should never return NULL for valid indices");
    snprintf(link_buffer, sizeof(link_buffer), "\tmovq\t%s, -%d(%%rbp)\n",
             link_reg, static_link->offset);
    add_inst_invalidate_cache();
    static_link_inst = add_inst(static_link_inst, link_buffer);
    add_inst_invalidate_cache();
    inst_list = ConcatList(static_link_inst, inst_list);
  }

  codegen_emit_local_const_equivs(ctx, symtab);
  codegen_emit_const_decl_equivs_from_list(ctx, func->const_declarations);
  codegen_function_header_ex_alias_vis(sub_id, ctx, func->nostackframe,
                                       func->cname_override,
                                       func->defined_in_unit);
  if (!func->nostackframe)
    codegen_stack_space_for_inst_list(inst_list, ctx);
#if USE_GRAPH_COLORING_ALLOCATOR
  ir_liveness_allocate(inst_list);
#endif
  ir_emit_function(inst_list);
#if USE_GRAPH_COLORING_ALLOCATOR
  ir_peephole_remove_redundant_moves(&inst_list);
#endif
  codegen_inst_list(inst_list, ctx);
  codegen_function_footer_ex(sub_id, ctx, func->nostackframe);
  if (dump_ir_flag())
    ir_print_function(stderr, sub_id, inst_list);
  if (dump_ir_cfg_flag()) {
    Cfg_t *cfg = cfg_build(inst_list);
    cfg_print(stderr, cfg, sub_id);
    cfg_free(cfg);
  }
  if (dump_ir_liveness_flag()) {
    Cfg_t *cfg = cfg_build(inst_list);
    LivenessInfo_t *liveness = liveness_compute(cfg);
    liveness_print(stderr, cfg, liveness, sub_id);
    liveness_free(liveness);
    cfg_free(cfg);
  }
  /* See matching note in codegen_procedure — release any cached
   * static-link register before the next subprogram's reset_reg_stack
   * destroys the underlying Register_t storage. */
  codegen_reset_static_link_cache(ctx);
  free_inst_list(inst_list);
  pop_stackscope();
  LeaveScope(symtab);
  /* Restore caller's unit-of-origin context. */
  symtab->current_unit_index = prev_unit_index;

  ctx->is_nostackframe = prev_is_nostackframe;
  ctx->asm_param_count = prev_asm_param_count;
  ctx->current_subprogram_id = prev_sub_id;
  ctx->current_subprogram_mangled = prev_sub_mangled;
  ctx->current_subprogram_method_name = prev_sub_method_name;
  ctx->current_subprogram_result_name = prev_result_name;
  ctx->current_subprogram_owner_class = prev_sub_owner_class;
  ctx->current_subprogram_owner_class_full = prev_sub_owner_class_full;
  ctx->current_subprogram_is_nonstatic_class_method =
      prev_is_nonstatic_class_method;
  ctx->current_subprogram_args = prev_sub_args;
  ctx->current_subprogram_declarations = prev_sub_declarations;
  ctx->current_return_slot = prev_return_slot;
  ctx->current_return_type = prev_return_type;
  ctx->current_record_return_slot = prev_record_return_slot;
  ctx->current_record_return_size = prev_record_return_size;
  ctx->current_subprogram_lexical_depth = prev_depth;
  ctx->callee_save_rbx_offset = prev_callee_rbx;
  ctx->callee_save_r12_offset = prev_callee_r12;
  ctx->callee_save_r13_offset = prev_callee_r13;
  ctx->callee_save_r14_offset = prev_callee_r14;
  ctx->callee_save_r15_offset = prev_callee_r15;
  ctx->returns_dynamic_array = prev_returns_dynamic_array;
  ctx->dynamic_array_descriptor_size = prev_dynamic_array_descriptor_size;
  /* Restore the outer subprogram's managed dynarray temp tracking. */
  free(ctx->managed_dynarray_temp_offsets);
  ctx->managed_dynarray_temp_offsets = prev_dynarray_temp_offsets;
  ctx->managed_dynarray_temp_count = prev_dynarray_temp_count;
  ctx->managed_dynarray_temp_capacity = prev_dynarray_temp_capacity;

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
}

/* Helper function to determine the size in bytes for a return type */
static int get_return_type_size(int return_type) {
  if (return_type == EXTENDED_TYPE)
    return 10;
  if (return_type == STRING_TYPE || return_type == POINTER_TYPE ||
      return_type == REAL_TYPE || return_type == INT64_TYPE)
    return 8;
  return 4; /* Default for INT_TYPE, LONGINT_TYPE, BOOL, CHAR_TYPE, etc. */
}

static int codegen_return_storage_size(KgpcType *return_type) {
  if (return_type == NULL)
    return DOUBLEWORD;

  long long type_size = kgpc_type_sizeof(return_type);
  if (type_size > 0 && type_size <= INT_MAX)
    return (int)type_size;

  if (return_type->kind == TYPE_KIND_POINTER)
    return 8;

  if (return_type->kind == TYPE_KIND_PRIMITIVE) {
    int tag = kgpc_type_get_primitive_tag(return_type);
    if (tag == EXTENDED_TYPE)
      return 10;
    if (tag == REAL_TYPE || tag == STRING_TYPE || tag == POINTER_TYPE ||
        tag == INT64_TYPE || tag == QWORD_TYPE)
      return 8;
  }

  return DOUBLEWORD;
}

static int codegen_return_type_id_storage_size(const char *return_type_id) {
  if (return_type_id == NULL)
    return 0;

  if (pascal_identifier_equals(return_type_id, "Single"))
    return 4;
  if (pascal_identifier_equals(return_type_id, "Extended"))
    return 10;
  if (pascal_identifier_equals(return_type_id, "Real") ||
      pascal_identifier_equals(return_type_id, "Double"))
    return 8;
  if (pascal_identifier_equals(return_type_id, "string") ||
      pascal_identifier_equals(return_type_id, "AnsiString") ||
      pascal_identifier_equals(return_type_id, "UnicodeString") ||
      pascal_identifier_equals(return_type_id, "WideString") ||
      pascal_identifier_equals(return_type_id, "Int64") ||
      pascal_identifier_equals(return_type_id, "QWord") ||
      pascal_identifier_equals(return_type_id, "UInt64") ||
      pascal_identifier_equals(return_type_id, "NativeInt") ||
      pascal_identifier_equals(return_type_id, "NativeUInt") ||
      pascal_identifier_equals(return_type_id, "SizeInt") ||
      pascal_identifier_equals(return_type_id, "SizeUInt") ||
      pascal_identifier_equals(return_type_id, "PtrInt") ||
      pascal_identifier_equals(return_type_id, "PtrUInt") ||
      pascal_identifier_equals(return_type_id, "IntPtr") ||
      pascal_identifier_equals(return_type_id, "UIntPtr") ||
      pascal_identifier_equals(return_type_id, "Pointer") ||
      pascal_identifier_equals(return_type_id, "PChar") ||
      pascal_identifier_equals(return_type_id, "PAnsiChar"))
    return 8;

  return 0;
}

/* Helper to add an alias label for a return variable so multiple identifiers
 * share storage. */
static void add_alias_for_return_var(StackNode_t *return_var,
                                     const char *alias_label) {
  if (return_var == NULL || alias_label == NULL || alias_label[0] == '\0')
    return;

  /* Create a stack node pointing to the same offset */
  StackNode_t *result_alias = init_stack_node(
      return_var->offset, (char *)alias_label, return_var->size);
  if (result_alias == NULL)
    return;

  result_alias->element_size = return_var->element_size;
  result_alias->is_alias = 1;
  result_alias->is_dynamic =
      return_var->is_dynamic; /* Copy dynamic array flag */
  if (return_var->is_static && return_var->static_label != NULL)
    result_alias->static_label = strdup(return_var->static_label);

  /* Add it to the x list in the current stack scope using the list API */
  StackScope_t *cur_scope = get_cur_scope();
  if (cur_scope != NULL) {
    ListNode_t *new_list_node = CreateListNode(result_alias, LIST_UNSPECIFIED);
    if (new_list_node != NULL) {
      if (cur_scope->x == NULL) {
        cur_scope->x = new_list_node;
        cur_scope->x_tail = new_list_node;
      } else {
        cur_scope->x_tail->next = new_list_node;
        cur_scope->x_tail = new_list_node;
      }
      /* Keep the hash index in sync with the list. */
      stackscope_index_x_insert(cur_scope, new_list_node);
    }
  }
}

int add_absolute_var_alias(const char *alias_label, const char *target_label) {
  if (alias_label == NULL || alias_label[0] == '\0' || target_label == NULL ||
      target_label[0] == '\0')
    return 1;

  StackNode_t *target = find_label((char *)target_label);
  if (target == NULL)
    return 1;

  StackNode_t *alias =
      init_stack_node(target->offset, (char *)alias_label, target->size);
  if (alias == NULL)
    return 1;

  alias->element_size = target->element_size;
  alias->is_alias = 1;
  alias->is_static = target->is_static;
  alias->is_reference = target->is_reference;
  if (target->static_label != NULL)
    alias->static_label = strdup(target->static_label);

  StackScope_t *cur_scope = get_cur_scope();
  if (cur_scope == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  ListNode_t *new_list_node = CreateListNode(alias, LIST_UNSPECIFIED);
  if (new_list_node == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  if (cur_scope->x == NULL) {
    cur_scope->x = new_list_node;
    cur_scope->x_tail = new_list_node;
  } else {
    cur_scope->x_tail->next = new_list_node;
    cur_scope->x_tail = new_list_node;
  }
  /* Keep the hash index in sync with the list. */
  stackscope_index_x_insert(cur_scope, new_list_node);

  return 0;
}

int add_absolute_static_symbol_alias(const char *alias_label,
                                     const char *target_symbol,
                                     int alias_size) {
  if (alias_label == NULL || alias_label[0] == '\0' || target_symbol == NULL ||
      target_symbol[0] == '\0' || alias_size <= 0)
    return 1;

  StackNode_t *alias = init_stack_node(0, (char *)alias_label, alias_size);
  if (alias == NULL)
    return 1;

  alias->element_size = alias_size;
  alias->is_alias = 1;
  alias->is_static = 1;
  alias->static_label = strdup(target_symbol);
  if (alias->static_label == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  StackScope_t *cur_scope = get_cur_scope();
  if (cur_scope == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  ListNode_t *new_list_node = CreateListNode(alias, LIST_UNSPECIFIED);
  if (new_list_node == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  if (cur_scope->x == NULL) {
    cur_scope->x = new_list_node;
    cur_scope->x_tail = new_list_node;
  } else {
    cur_scope->x_tail->next = new_list_node;
    cur_scope->x_tail = new_list_node;
  }
  /* Keep the hash index in sync with the list. */
  stackscope_index_x_insert(cur_scope, new_list_node);

  return 0;
}

/* Add absolute alias with offset for record field access.
 * Creates an alias variable that points to base_var + field_offset. */
int add_absolute_var_alias_with_offset(const char *alias_label,
                                       const char *target_label,
                                       int field_offset, int alias_size) {
  if (alias_label == NULL || alias_label[0] == '\0' || target_label == NULL ||
      target_label[0] == '\0')
    return 1;

  StackNode_t *target = find_label((char *)target_label);
  if (target == NULL)
    return 1;

  /* Create alias with adjusted offset: target offset + field offset */
  int adjusted_offset = target->offset + field_offset;
  StackNode_t *alias =
      init_stack_node(adjusted_offset, (char *)alias_label, alias_size);
  if (alias == NULL)
    return 1;

  alias->element_size = alias_size;
  alias->is_alias = 1;
  alias->is_static = target->is_static;
  alias->is_reference = target->is_reference;
  if (target->static_label != NULL) {
    /* For static variables, create a new label with offset suffix.
     * The assembly will reference base+offset. */
    size_t label_len = strlen(target->static_label) + 32;
    char *offset_label = (char *)malloc(label_len);
    if (offset_label != NULL) {
      snprintf(offset_label, label_len, "%s+%d", target->static_label,
               field_offset);
      alias->static_label = offset_label;
    } else {
      alias->static_label = strdup(target->static_label);
    }
  }

  StackScope_t *cur_scope = get_cur_scope();
  if (cur_scope == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  ListNode_t *new_list_node = CreateListNode(alias, LIST_UNSPECIFIED);
  if (new_list_node == NULL) {
    destroy_stack_node(alias);
    return 1;
  }

  if (cur_scope->x == NULL) {
    cur_scope->x = new_list_node;
    cur_scope->x_tail = new_list_node;
  } else {
    cur_scope->x_tail->next = new_list_node;
    cur_scope->x_tail = new_list_node;
  }
  /* Keep the hash index in sync with the list. */
  stackscope_index_x_insert(cur_scope, new_list_node);

  return 0;
}

/* Helper function to add a Result alias for anonymous function return variable
 */
static void add_result_alias_for_return_var(StackNode_t *return_var) {
  add_alias_for_return_var(return_var, "Result");
}

static int codegen_dynamic_array_element_size_from_type(CodeGenContext *ctx,
                                                        KgpcType *array_type) {
  if (array_type == NULL || array_type->kind != TYPE_KIND_ARRAY)
    return DOUBLEWORD;

  KgpcType *element_type = array_type->info.array_info.element_type;
  if (element_type == NULL)
    return DOUBLEWORD;

  switch (element_type->kind) {
  case TYPE_KIND_PRIMITIVE: {
    if (element_type->type_alias != NULL &&
        element_type->type_alias->storage_size > 0 &&
        element_type->type_alias->storage_size <= INT_MAX) {
      return (int)element_type->type_alias->storage_size;
    }
    int tag = kgpc_type_get_primitive_tag(element_type);
    switch (tag) {
    case LONGINT_TYPE:
      return DOUBLEWORD; // 4 bytes for FPC's 32-bit LongInt
    case REAL_TYPE:
    case STRING_TYPE:
    case POINTER_TYPE:
      return 8;
    case SHORTSTRING_TYPE: {
      int short_size = codegen_shortstring_storage_size(element_type);
      return short_size > 0 ? short_size : 256;
    }
    case CHAR_TYPE:
    case BOOL:
      return 1;
    default:
      return DOUBLEWORD;
    }
  }
  case TYPE_KIND_RECORD: {
    struct RecordType *record = kgpc_type_get_record(element_type);
    long long size = 0;
    if (record != NULL &&
        codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL, record, &size) ==
            0 &&
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

int codegen_dynamic_array_descriptor_bytes(int element_size) {
  int descriptor_size = 4 * DOUBLEWORD;
  int needed = element_size * 2;
  if (needed > descriptor_size)
    descriptor_size = needed;
  return descriptor_size;
}

/* Code generation for an anonymous function/procedure
 * This generates the function body and returns the function's label name.
 * The caller is responsible for generating code to load the address of this
 * function.
 */
void codegen_anonymous_method(struct Expression *expr, CodeGenContext *ctx,
                              SymTab_t *symtab) {
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
#endif

  assert(expr != NULL);
  assert(expr->type == EXPR_ANONYMOUS_FUNCTION ||
         expr->type == EXPR_ANONYMOUS_PROCEDURE);
  assert(ctx != NULL);
  assert(symtab != NULL);

  struct AnonymousMethod *anon = &expr->expr_data.anonymous_method_data;

  if (anon->generated_name == NULL) {
    codegen_report_error(
        ctx, "ERROR: Anonymous method missing generated name at line %d",
        expr->line_num);
    return;
  }

  if (anon->body == NULL) {
/* Empty body - generate a no-op function */
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: Anonymous method %s has no body, generating no-op\n",
                  anon->generated_name);
#endif
  }

  const char *prev_sub_id = ctx->current_subprogram_id;
  const char *prev_sub_mangled = ctx->current_subprogram_mangled;
  int prev_callee_rbx = ctx->callee_save_rbx_offset;
  int prev_callee_r12 = ctx->callee_save_r12_offset;
  int prev_callee_r13 = ctx->callee_save_r13_offset;
  int prev_callee_r14 = ctx->callee_save_r14_offset;
  int prev_callee_r15 = ctx->callee_save_r15_offset;

  push_stackscope();

  /* Allocate stack slots for callee-saved registers */
  reset_reg_stack();
  codegen_invalidate_static_link_cache(ctx);
  ListNode_t *inst_list = NULL;
  ctx->next_vreg_id = 0;
  int num_args = (anon->parameters == NULL) ? 0 : ListLength(anon->parameters);
  int lexical_depth = codegen_get_lexical_depth(ctx) + 1;
  int prev_depth = ctx->current_subprogram_lexical_depth;
  ctx->current_subprogram_lexical_depth = lexical_depth;
  int is_nested = (lexical_depth >= 1);

  ctx->current_subprogram_id = anon->generated_name;
  ctx->current_subprogram_mangled = anon->generated_name;

  /* Anonymous methods are always nested (they're defined inside some other
   * context). They always need a static link to access variables from their
   * parent scope (closure). The static link is passed in %rdi (first register)
   * and parameters are shifted by 1.
   */
  StackNode_t *static_link = NULL;
  int will_need_static_link = is_nested;
  int arg_start_index = (will_need_static_link && num_args > 0) ? 1 : 0;

  /* Process parameters (convert from TREE_VAR_DECL to stack allocations) */
  inst_list = codegen_subprogram_arguments(anon->parameters, inst_list, ctx,
                                           symtab, arg_start_index);

  /* Add static link after parameters */
  if (will_need_static_link) {
    static_link = add_l_x("__static_link__", 8);
    codegen_register_static_link_proc(ctx, anon->generated_name, lexical_depth);

    if (static_link != NULL) {
      /* Static link always comes in %rdi (first register) */
      /* Integrated: store a physical register to the frame slot via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(static_link->offset)}}};
      BeOperand a = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
  }

  /* For functions (not procedures), allocate space for the return value */
  StackNode_t *return_var = NULL;
  if (anon->is_function && anon->return_type != -1) {
    int return_size = get_return_type_size(anon->return_type);
    return_var = add_l_x(anon->generated_name, return_size);

    /* Also add "Result" as an alias at the same stack offset */
    add_result_alias_for_return_var(return_var);
  }

  /* No local variable declarations in anonymous methods (they're inline) */
  /* No nested subprograms in anonymous methods */

  /* Allocate callee-save slots AFTER args (z) and locals (x) so that
   * the t-section offset = z_offset + x_offset + t_offset doesn't collide. */
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

  /* Generate the body */
  if (anon->body != NULL) {
    inst_list = codegen_stmt(anon->body, inst_list, ctx, symtab);
  }

  /* For functions, move return value to correct return register */
  if (anon->is_function && return_var != NULL) {
    char buffer[64];
    int return_is_real =
        (anon->return_type == REAL_TYPE || anon->return_type == EXTENDED_TYPE);
    int return_size = get_return_type_size(anon->return_type);
    int use_qword = return_size >= 8;
    if (return_size == 0 && return_var->size >= 8)
      use_qword = 1;

    /* Check element_size for unaligned Single type (4 bytes) */
    long long unaligned_return_size = return_var->element_size > 0
                                          ? return_var->element_size
                                          : return_var->size;
    if (return_is_real && return_var->element_size == 10)
      snprintf(buffer, sizeof(buffer), "\tfldt\t-%d(%%rbp)\n",
               return_var->offset);
    else if (return_is_real && unaligned_return_size <= 4)
      snprintf(buffer, sizeof(buffer), "\tmovss\t-%d(%%rbp), %%xmm0\n",
               return_var->offset);
    else if (return_is_real)
      snprintf(buffer, sizeof(buffer), "\tmovsd\t-%d(%%rbp), %%xmm0\n",
               return_var->offset);
    else if (use_qword)
      snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %%rax\n",
               return_var->offset);
    else
      snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %%eax\n",
               return_var->offset);
    inst_list = add_inst(inst_list, buffer);
  }

  /* Generate the function header, stack allocation, body, and footer */
  codegen_function_header(anon->generated_name, ctx);
  codegen_stack_space_for_inst_list(inst_list, ctx);
#if USE_GRAPH_COLORING_ALLOCATOR
  ir_liveness_allocate(inst_list);
#endif
  ir_emit_function(inst_list);
#if USE_GRAPH_COLORING_ALLOCATOR
  ir_peephole_remove_redundant_moves(&inst_list);
#endif
  codegen_inst_list(inst_list, ctx);
  codegen_function_footer(anon->generated_name, ctx);
  if (dump_ir_flag())
    ir_print_function(stderr, anon->generated_name, inst_list);
  if (dump_ir_cfg_flag()) {
    Cfg_t *cfg = cfg_build(inst_list);
    cfg_print(stderr, cfg, anon->generated_name);
    cfg_free(cfg);
  }
  if (dump_ir_liveness_flag()) {
    Cfg_t *cfg = cfg_build(inst_list);
    LivenessInfo_t *liveness = liveness_compute(cfg);
    liveness_print(stderr, cfg, liveness, anon->generated_name);
    liveness_free(liveness);
    cfg_free(cfg);
  }

  free_inst_list(inst_list);
  pop_stackscope();

  ctx->current_subprogram_id = prev_sub_id;
  ctx->current_subprogram_mangled = prev_sub_mangled;
  ctx->current_subprogram_lexical_depth = prev_depth;
  ctx->callee_save_rbx_offset = prev_callee_rbx;
  ctx->callee_save_r12_offset = prev_callee_r12;
  ctx->callee_save_r13_offset = prev_callee_r13;
  ctx->callee_save_r14_offset = prev_callee_r14;
  ctx->callee_save_r15_offset = prev_callee_r15;

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
}

/* Code generation for subprogram arguments */
ListNode_t *codegen_subprogram_arguments(ListNode_t *args,
                                         ListNode_t *inst_list,
                                         CodeGenContext *ctx, SymTab_t *symtab,
                                         int arg_start_index) {
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
  /* Positive offsets from %rbp to reach stack-passed arguments.
   * System V: 16(%rbp) is the first stack arg (after saved rbp + return addr).
   * Windows x64: 48(%rbp) is the first stack arg (after saved rbp + return addr
   * + 32-byte shadow space). */
  int stack_arg_offset = codegen_target_is_windows() ? 48 : 16;
  ListNode_t *record_param_queue = NULL;
  int param_index = 0;

  assert(ctx != NULL);

  if (arg_start_index < 0)
    arg_start_index = 0;

  next_gpr_index = arg_start_index;

  /* Pre-pass phase 1: Check if there's any record/dynarray parameter that will
   * need kgpc_move. If so, we need to pre-allocate ALL parameter storage and
   * save registers to final locations before processing starts, to avoid
   * register clobbering issues. */
  ListNode_t *args_scan = args;
  int has_record_or_dynarray = 0;
  int param_count_for_alloc = 0;

  while (args_scan != NULL) {
    Tree_t *scan_decl = (Tree_t *)args_scan->cur;
    if (scan_decl->type == TREE_VAR_DECL) {
      ListNode_t *scan_ids = scan_decl->tree_data.var_decl_data.ids;
      int scan_type = scan_decl->tree_data.var_decl_data.type;
      KgpcType *scan_cached_type =
          scan_decl->tree_data.var_decl_data.cached_kgpc_type;
      int is_var = scan_decl->tree_data.var_decl_data.is_var_param;

      while (scan_ids != NULL) {
        param_count_for_alloc++;

        /* Check if this parameter is a record or dynarray that needs kgpc_move
         */
        if (!is_var) {
          HashNode_t *scan_type_node = NULL;
          if (scan_type == UNKNOWN_TYPE &&
              scan_decl->tree_data.var_decl_data.type_id != NULL &&
              symtab != NULL) {
            FindSymbol(&scan_type_node, symtab,
                       scan_decl->tree_data.var_decl_data.type_id);
          }

          struct RecordType *rec = NULL;
          if (scan_type_node != NULL)
            rec = hashnode_get_record_type(scan_type_node);
          if (rec == NULL && scan_cached_type != NULL) {
            HashNode_t cached_node;
            memset(&cached_node, 0, sizeof(cached_node));
            cached_node.type = scan_cached_type;
            rec = hashnode_get_record_type(&cached_node);
          }

          if (rec != NULL) {
            has_record_or_dynarray = 1;
          } else if (scan_cached_type != NULL &&
                     scan_cached_type->kind == TYPE_KIND_ARRAY &&
                     kgpc_type_is_dynamic_array(scan_cached_type)) {
            has_record_or_dynarray = 1;
          } else if (scan_type_node != NULL && scan_type_node->type != NULL &&
                     scan_type_node->type->kind == TYPE_KIND_ARRAY &&
                     kgpc_type_is_dynamic_array(scan_type_node->type)) {
            has_record_or_dynarray = 1;
          } else {
            KgpcType *param_type = NULL;
            if (scan_type_node != NULL)
              param_type = scan_type_node->type;
            else if (scan_cached_type != NULL)
              param_type = scan_cached_type;
            if (param_type != NULL && param_type->kind == TYPE_KIND_PRIMITIVE &&
                kgpc_type_get_primitive_tag(param_type) == SET_TYPE &&
                kgpc_type_sizeof(param_type) > 4) {
              has_record_or_dynarray = 1;
            }
            /* Method-pointer (TMethod) parameters are 16-byte
             * aggregates passed by reference; they take the same
             * inline-copy path. */
            if (kgpc_type_is_method_pointer(param_type))
              has_record_or_dynarray = 1;
          }
          /* Extended (10-byte / bestreal) value parameters that are
           * stack-passed call kgpc_move at entry to copy the 10-byte
           * value from caller's stack slot to the local storage.
           * kgpc_move forwards to memmove and does NOT preserve %rsi
           * (or other GPRs), so any subsequent register-passed
           * parameter would read a clobbered register.  Trigger the
           * presave mechanism so all GPR-passed params are spilled
           * to home slots before the first kgpc_move call. */
          if (!has_record_or_dynarray && !is_var &&
              (scan_type == REAL_TYPE || scan_type == EXTENDED_TYPE)) {
            int scan_real_storage_size = codegen_real_param_storage_size(
                scan_decl, NULL, scan_cached_type);
            if (scan_real_storage_size == 16)
              has_record_or_dynarray = 1;
          }
          /* Value ShortString parameters call kgpc_shortstring_to_shortstring
           * at entry, which clobbers subsequent parameter registers.
           * Trigger the presave mechanism so all registers are saved first. */
          if (!has_record_or_dynarray &&
              !scan_decl->tree_data.var_decl_data.is_const_param) {
            int scan_is_ss = 0;
            if (scan_type == SHORTSTRING_TYPE)
              scan_is_ss = 1;
            if (!scan_is_ss && scan_cached_type != NULL) {
              if (kgpc_type_is_shortstring(scan_cached_type))
                scan_is_ss = 1;
              else {
                struct TypeAlias *sa =
                    kgpc_type_get_type_alias(scan_cached_type);
                if (sa != NULL && sa->is_shortstring)
                  scan_is_ss = 1;
              }
            }
            if (!scan_is_ss && scan_type_node != NULL &&
                scan_type_node->type != NULL) {
              if (kgpc_type_is_shortstring(scan_type_node->type))
                scan_is_ss = 1;
              else {
                struct TypeAlias *sa =
                    kgpc_type_get_type_alias(scan_type_node->type);
                if (sa != NULL && sa->is_shortstring)
                  scan_is_ss = 1;
              }
            }
            if (scan_is_ss)
              has_record_or_dynarray = 1;
          }
        }
        scan_ids = scan_ids->next;
      }
    } else if (scan_decl->type == TREE_ARR_DECL) {
      ListNode_t *scan_ids = scan_decl->tree_data.arr_decl_data.ids;
      while (scan_ids != NULL) {
        param_count_for_alloc++;
        /* Value ShortString array params also need presave */
        if (!has_record_or_dynarray &&
            scan_decl->tree_data.arr_decl_data.is_shortstring) {
          HashNode_t *sym = NULL;
          if (symtab != NULL)
            FindSymbol(&sym, symtab, (char *)scan_ids->cur);
          if (sym == NULL || !sym->is_var_parameter)
            has_record_or_dynarray = 1;
        }
        scan_ids = scan_ids->next;
      }
    }
    args_scan = args_scan->next;
  }

  /* Pre-pass phase 2: If there are record/dynarray parameters, pre-allocate ALL
   * storage and save registers to their final locations before processing
   * starts. */
  if (has_record_or_dynarray && param_count_for_alloc > 0) {
    args_scan = args;
    int scan_gpr_index = arg_start_index;
    int scan_sse_index = 0;
    while (args_scan != NULL) {
      Tree_t *scan_decl = (Tree_t *)args_scan->cur;
      if (scan_decl->type == TREE_VAR_DECL) {
        int scan_type = scan_decl->tree_data.var_decl_data.type;
        int scan_is_var = scan_decl->tree_data.var_decl_data.is_var_param;
        KgpcType *scan_cached_type =
            scan_decl->tree_data.var_decl_data.cached_kgpc_type;
        ListNode_t *scan_ids = scan_decl->tree_data.var_decl_data.ids;
        while (scan_ids != NULL) {
          int scan_real_storage_size = 8;
          if (!scan_is_var &&
              (scan_type == REAL_TYPE || scan_type == EXTENDED_TYPE))
            scan_real_storage_size = codegen_real_param_storage_size(
                scan_decl, NULL, scan_cached_type);
          /* Float (REAL_TYPE) parameters that are not passed by reference
           * use SSE/XMM registers, NOT integer registers. Skip integer
           * register allocation for them so subsequent integer params
           * get the correct registers.
           * NOTE: SSE regs ARE clobbered by ASAN-intercepted memmove
           * (kgpc_move), so we must presave them to stack before any
           * kgpc_move call, just like GPR params. */
          if (!scan_is_var &&
              (scan_type == REAL_TYPE || scan_type == EXTENDED_TYPE) &&
              scan_real_storage_size < 16) {
            if (scan_sse_index < kgpc_max_sse_arg_regs()) {
              const char *xmm_reg = current_arg_reg_xmm(scan_sse_index);
              scan_sse_index++;
              if (xmm_reg != NULL) {
                char temp_name[64];
                snprintf(temp_name, sizeof(temp_name), "__presaved_%s__",
                         (char *)scan_ids->cur);
                StackNode_t *presaved_slot = add_q_z(temp_name);
                if (presaved_slot != NULL) {
                  if (scan_real_storage_size == 4)
                    snprintf(buffer, sizeof(buffer),
                             "\tmovss\t%s, -%d(%%rbp)\n", xmm_reg,
                             presaved_slot->offset);
                  else
                    snprintf(buffer, sizeof(buffer),
                             "\tmovsd\t%s, -%d(%%rbp)\n", xmm_reg,
                             presaved_slot->offset);
                  inst_list = add_inst(inst_list, buffer);
                }
              }
            }
            scan_ids = scan_ids->next;
            continue;
          }
          /* Extended (10-byte) value parameters are passed on the
           * stack — they don't consume an integer arg register, so
           * skip GPR allocation and no presave slot is needed for
           * the Extended value itself. */
          if (!scan_is_var &&
              (scan_type == REAL_TYPE || scan_type == EXTENDED_TYPE) &&
              scan_real_storage_size == 16) {
            scan_ids = scan_ids->next;
            continue;
          }
          const char *param_reg = alloc_integer_arg_reg(1, &scan_gpr_index);
          if (param_reg != NULL) {
            /* Allocate final storage slot and save register directly */
            char temp_name[64];
            snprintf(temp_name, sizeof(temp_name), "__presaved_%s__",
                     (char *)scan_ids->cur);
            StackNode_t *presaved_slot = add_q_z(temp_name);
            if (presaved_slot != NULL) {
              {
                /* Integrated: store a physical register to the frame slot via
                 * the vtable. */
                BeEmitter em = codegen_beemitter(inst_list, ctx);
                BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                                 {.mem_frame = {BE_BASE_FP, -(long long)(presaved_slot->offset)}}};
                BeOperand a = {OPK_PHYS, BE_W64, {.phys = param_reg}};
                kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
                inst_list = em.list;
              }
            }
          }
          scan_ids = scan_ids->next;
        }
      } else if (scan_decl->type == TREE_ARR_DECL) {
        ListNode_t *scan_ids = scan_decl->tree_data.arr_decl_data.ids;
        while (scan_ids != NULL) {
          const char *param_reg = alloc_integer_arg_reg(1, &scan_gpr_index);
          if (param_reg != NULL) {
            char temp_name[64];
            snprintf(temp_name, sizeof(temp_name), "__presaved_%s__",
                     (char *)scan_ids->cur);
            StackNode_t *presaved_slot = add_q_z(temp_name);
            if (presaved_slot != NULL) {
              {
                /* Integrated: store a physical register to the frame slot via
                 * the vtable. */
                BeEmitter em = codegen_beemitter(inst_list, ctx);
                BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                                 {.mem_frame = {BE_BASE_FP, -(long long)(presaved_slot->offset)}}};
                BeOperand a = {OPK_PHYS, BE_W64, {.phys = param_reg}};
                kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
                inst_list = em.list;
              }
            }
          }
          scan_ids = scan_ids->next;
        }
      }
      args_scan = args_scan->next;
    }
  }

  /* Reset for main processing pass */
  next_gpr_index = arg_start_index;

  while (args != NULL) {
    arg_decl = (Tree_t *)args->cur;
    switch (arg_decl->type) {
    case TREE_VAR_DECL:
      arg_ids = arg_decl->tree_data.var_decl_data.ids;
      type = arg_decl->tree_data.var_decl_data.type;
      HashNode_t *resolved_type_node = NULL;
      KgpcType *cached_arg_type =
          arg_decl->tree_data.var_decl_data.cached_kgpc_type;
      int inferred_type_tag = type;
      HashNode_t cached_arg_node;
      HashNode_t *cached_arg_node_ptr = NULL;
      if (cached_arg_type != NULL) {
        memset(&cached_arg_node, 0, sizeof(cached_arg_node));
        cached_arg_node.type = cached_arg_type;
        cached_arg_node_ptr = &cached_arg_node;
      }

      // Resolve type aliases if needed
      if (type == UNKNOWN_TYPE &&
          arg_decl->tree_data.var_decl_data.type_id != NULL && symtab != NULL) {
        HashNode_t *type_node = NULL;
        FindSymbol(&type_node, symtab,
                   arg_decl->tree_data.var_decl_data.type_id);
        if (type_node != NULL) {
          struct TypeAlias *alias = get_type_alias_from_node(type_node);
          if (alias != NULL) {
            type = alias->base_type;
          }
          resolved_type_node = type_node;
        }
      }

      /* If the legacy type tag was UNKNOWN, derive it from the resolved
       * KgpcType so that class/pointer parameters are treated as 64-bit values.
       */
      if (inferred_type_tag == UNKNOWN_TYPE) {
        if (resolved_type_node != NULL && resolved_type_node->type != NULL)
          inferred_type_tag = codegen_tag_from_kgpc(resolved_type_node->type);
        else if (cached_arg_type != NULL)
          inferred_type_tag = codegen_tag_from_kgpc(cached_arg_type);
      }

      while (arg_ids != NULL) {
        int tree_is_var_param = arg_decl->tree_data.var_decl_data.is_var_param;
        int is_untyped_param =
            arg_decl->tree_data.var_decl_data.is_untyped_param;
        if (is_untyped_param)
          tree_is_var_param = 1;
        int symbol_is_var_param = tree_is_var_param;
        int is_self_param = 0;
        if (arg_decl->tree_data.var_decl_data.ids != NULL) {
          const char *first_id =
              (const char *)arg_decl->tree_data.var_decl_data.ids->cur;
          if (first_id != NULL && pascal_identifier_equals(first_id, "Self"))
            is_self_param = 1;
        }
        if (is_self_param && codegen_self_param_is_class(arg_decl, symtab)) {
          tree_is_var_param = 0;
          symbol_is_var_param = 0;
        }
        struct RecordType *record_type_info = NULL;
        int is_dynarray_param = 0;
        int dynarray_elem_size = 0;

        if (!symbol_is_var_param) {
          if (resolved_type_node != NULL)
            record_type_info = hashnode_get_record_type(resolved_type_node);
          if (record_type_info == NULL && cached_arg_node_ptr != NULL)
            record_type_info = hashnode_get_record_type(cached_arg_node_ptr);
          if (record_type_info == NULL) {
            KgpcType *param_type = NULL;
            if (resolved_type_node != NULL)
              param_type = resolved_type_node->type;
            else if (cached_arg_type != NULL)
              param_type = cached_arg_type;
            if (param_type != NULL && kgpc_type_is_record(param_type)) {
              record_type_info = kgpc_type_get_record(param_type);
            } else if (param_type != NULL &&
                       param_type->kind == TYPE_KIND_ARRAY &&
                       kgpc_type_is_dynamic_array(param_type)) {
              is_dynarray_param = 1;
              dynarray_elem_size =
                  (int)kgpc_type_get_array_element_size(param_type);
              if (dynarray_elem_size <= 0)
                dynarray_elem_size = 1;
            }
          }
        }

        int is_char_set_param = 0;
        long long char_set_size = 0;
        if (!symbol_is_var_param) {
          KgpcType *param_type = NULL;
          if (resolved_type_node != NULL)
            param_type = resolved_type_node->type;
          else if (cached_arg_type != NULL)
            param_type = cached_arg_type;
          if (param_type != NULL && param_type->kind == TYPE_KIND_PRIMITIVE &&
              kgpc_type_get_primitive_tag(param_type) == SET_TYPE) {
            long long size = kgpc_type_sizeof(param_type);
            if (size > 4) {
              is_char_set_param = 1;
              char_set_size = size;
            }
          }
        }

        /* Method-pointer (TMethod) params are 16-byte aggregates,
         * passed by reference like records.  The callee copies the
         * 16 bytes from the source pointer into an inline local
         * slot, so subsequent uses of the param see the inline
         * descriptor exactly like a local variable. */
        int is_method_ptr_param = 0;
        long long method_ptr_size = 0;
        if (!symbol_is_var_param) {
          KgpcType *param_type = NULL;
          if (resolved_type_node != NULL)
            param_type = resolved_type_node->type;
          else if (cached_arg_type != NULL)
            param_type = cached_arg_type;
          if (kgpc_type_is_method_pointer(param_type)) {
            is_method_ptr_param = 1;
            method_ptr_size = 16;
          }
        }

        if (record_type_info != NULL || is_dynarray_param ||
            is_char_set_param || is_method_ptr_param) {
          long long record_size = 0;
          if (is_dynarray_param) {
            record_size =
                codegen_dynamic_array_descriptor_bytes(dynarray_elem_size);
          } else if (is_char_set_param) {
            record_size = char_set_size;
          } else if (is_method_ptr_param) {
            record_size = method_ptr_size;
          } else if (codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                                                   record_type_info,
                                                   &record_size) != 0 ||
                     record_size < 0) {
            codegen_report_error(
                ctx, "ERROR: Unable to determine size for record parameter %s.",
                (char *)arg_ids->cur);
            return inst_list;
          }

          if (record_size > INT_MAX) {
            codegen_report_error(
                ctx,
                "ERROR: Record parameter %s exceeds supported size limits.",
                (char *)arg_ids->cur);
            return inst_list;
          }

          RecordParamWork *work =
              (RecordParamWork *)malloc(sizeof(RecordParamWork));
          if (work == NULL) {
            codegen_report_error(
                ctx, "ERROR: Unable to allocate record param work.");
            return inst_list;
          }

          work->id = (const char *)arg_ids->cur;
          work->size = (int)record_size;
          work->stack_arg_offset = 0;
          work->has_stack_arg = 0;
          work->arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);
          work->is_dynarray = is_dynarray_param;
          work->dynarray_elem_size = dynarray_elem_size;
          work->dynarray_lower_bound = 0;
          work->arg_index = param_index;

          if (work->arg_reg == NULL) {
            work->stack_arg_offset = stack_arg_offset;
            work->has_stack_arg = 1;
            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
          }

          ListNode_t *work_node = CreateListNode(work, LIST_UNSPECIFIED);
          if (work_node == NULL) {
            free(work);
            codegen_report_error(ctx, "ERROR: Unable to enqueue record param.");
            return inst_list;
          }

          if (record_param_queue == NULL)
            record_param_queue = work_node;
          else
            record_param_queue =
                PushListNodeBack(record_param_queue, work_node);

          arg_ids = arg_ids->next;
          param_index++;
          continue;
        }

        // Var parameters are passed by reference (as pointers), so always use
        // 64-bit Also use 64-bit for strings, explicit pointers, arrays, or
        // aliases that explicitly require 8-byte storage (e.g.,
        // NativeUInt/Int64).
        int is_var_param = symbol_is_var_param;
        int is_array_type = 0;
        int type_requires_qword = 0;
        int real_storage_size = 8;
        int use_extended_stack_param = 0;
        int is_shortstring_param = 0;

        /* Determine if parameter is an array type via resolved type only */
        if (resolved_type_node != NULL && resolved_type_node->type != NULL &&
            kgpc_type_is_array(resolved_type_node->type)) {
          is_array_type = 1;
          type_requires_qword = kgpc_type_uses_qword(resolved_type_node->type);
          struct TypeAlias *alias =
              kgpc_type_get_type_alias(resolved_type_node->type);
          if (kgpc_type_is_shortstring(resolved_type_node->type) ||
              (alias != NULL && alias->is_shortstring))
            is_shortstring_param = 1;
        } else if (cached_arg_type != NULL &&
                   kgpc_type_is_array(cached_arg_type)) {
          is_array_type = 1;
          type_requires_qword = kgpc_type_uses_qword(cached_arg_type);
          struct TypeAlias *alias = kgpc_type_get_type_alias(cached_arg_type);
          if (kgpc_type_is_shortstring(cached_arg_type) ||
              (alias != NULL && alias->is_shortstring))
            is_shortstring_param = 1;
        } else if (resolved_type_node != NULL &&
                   resolved_type_node->type != NULL) {
          type_requires_qword = kgpc_type_uses_qword(resolved_type_node->type);
          struct TypeAlias *alias =
              kgpc_type_get_type_alias(resolved_type_node->type);
          if (kgpc_type_is_shortstring(resolved_type_node->type) ||
              (alias != NULL && alias->is_shortstring))
            is_shortstring_param = 1;
        } else if (cached_arg_type != NULL) {
          type_requires_qword = kgpc_type_uses_qword(cached_arg_type);
          struct TypeAlias *alias = kgpc_type_get_type_alias(cached_arg_type);
          if (kgpc_type_is_shortstring(cached_arg_type) ||
              (alias != NULL && alias->is_shortstring))
            is_shortstring_param = 1;
        }

        if (inferred_type_tag == REAL_TYPE ||
            inferred_type_tag == EXTENDED_TYPE)
          real_storage_size = codegen_real_param_storage_size(
              arg_decl, resolved_type_node, cached_arg_type);

        use_extended_stack_param =
            (!is_var_param && !is_array_type && !is_shortstring_param &&
             (inferred_type_tag == REAL_TYPE ||
              inferred_type_tag == EXTENDED_TYPE) &&
             real_storage_size == 16);

        int use_64bit =
            is_var_param || is_array_type || type_requires_qword ||
            (inferred_type_tag == STRING_TYPE ||
             inferred_type_tag == POINTER_TYPE || type == PROCEDURE ||
             ((inferred_type_tag == REAL_TYPE ||
               inferred_type_tag == EXTENDED_TYPE) &&
              real_storage_size > 4));
        int use_sse_reg = 0;
        if (!is_var_param && !is_array_type && !is_shortstring_param &&
            (inferred_type_tag == REAL_TYPE ||
             inferred_type_tag == EXTENDED_TYPE) &&
            real_storage_size < 16)
          use_sse_reg = 1;
        /* Also detect shortstring from inferred_type_tag when resolved/cached
         * types didn't flag it (e.g. bare 'string' under {$H-}). */
        if (!is_shortstring_param && inferred_type_tag == SHORTSTRING_TYPE)
          is_shortstring_param = 1;
        /* Under {$H-}, 'string' parameters may have STRING_TYPE in the
         * tree but the actual code treats them as ShortString.  Detect
         * this by checking the cached_arg_type's type_alias. */
        if (!is_shortstring_param && !is_var_param &&
            (inferred_type_tag == STRING_TYPE || type == STRING_TYPE) &&
            cached_arg_type != NULL) {
          struct TypeAlias *ca_alias =
              kgpc_type_get_type_alias(cached_arg_type);
          if (ca_alias != NULL && ca_alias->is_shortstring)
            is_shortstring_param = 1;
          /* Primitive STRING_TYPE that isn't a heap-allocated AnsiString
           * and has no type_alias is likely a {$H-} shortstring.
           * Check kgpc_type_sizeof: shortstrings have fixed sizes (1-256),
           * while AnsiString/heap has size 8 (pointer). */
          if (!is_shortstring_param &&
              cached_arg_type->kind == TYPE_KIND_PRIMITIVE &&
              kgpc_type_get_primitive_tag(cached_arg_type) == STRING_TYPE) {
            long long tsize = kgpc_type_sizeof(cached_arg_type);
            if (tsize > 8 && tsize <= 256)
              is_shortstring_param = 1;
          }
        }
        int is_const_param = arg_decl->tree_data.var_decl_data.is_const_param;
        int is_value_shortstring =
            is_shortstring_param && !is_var_param && !is_const_param;
        if (is_value_shortstring) {
          /* VALUE ShortString parameters: allocate full shortstring
           * buffer so the callee owns a local copy.  The incoming
           * register/stack slot carries a pointer to the caller's
           * ShortString; we will copy it below.
           * NOTE: const ShortString params are passed by reference
           * (like var) and must NOT get a local copy. */
          int ss_size = 256;
          if (resolved_type_node != NULL && resolved_type_node->type != NULL) {
            int resolved_size =
                codegen_shortstring_storage_size(resolved_type_node->type);
            if (resolved_size > 0)
              ss_size = resolved_size;
          } else if (cached_arg_type != NULL) {
            int cached_size = codegen_shortstring_storage_size(cached_arg_type);
            if (cached_size > 0)
              ss_size = cached_size;
          }
          arg_stack = add_l_z_bytes((char *)arg_ids->cur, ss_size);
          /* NOT setting is_reference — the slot holds the actual
           * ShortString data, not a pointer to it. */
        } else if (use_extended_stack_param)
          arg_stack = add_l_z_bytes((char *)arg_ids->cur, 10);
        else
          arg_stack = use_64bit ? add_q_z((char *)arg_ids->cur)
                                : add_l_z((char *)arg_ids->cur);
        if (arg_stack != NULL &&
            (symbol_is_var_param || is_array_type || is_shortstring_param) &&
            !is_value_shortstring)
          arg_stack->is_reference = 1;
        if (use_extended_stack_param) {
          Register_t *src_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
          Register_t *dst_addr_reg = NULL;
          if (src_addr_reg == NULL)
            src_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
          if (src_addr_reg == NULL) {
            codegen_report_error(
                ctx,
                "ERROR: Unable to allocate register for Extended parameter %s.",
                (char *)arg_ids->cur);
            return inst_list;
          }

          dst_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
          if (dst_addr_reg == NULL)
            dst_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
          if (dst_addr_reg == NULL) {
            free_reg(get_reg_stack(), src_addr_reg);
            codegen_report_error(ctx,
                                 "ERROR: Unable to allocate destination "
                                 "register for Extended parameter %s.",
                                 (char *)arg_ids->cur);
            return inst_list;
          }

          {
            char tmpl[64];
            snprintf(tmpl, sizeof(tmpl), "\tleaq\t%d(%%rbp), %%0\n",
                     stack_arg_offset);
            Register_t *defs_arr[] = {src_addr_reg};
            inst_list = add_inst_du(inst_list, ctx, defs_arr, 1, NULL, 0, tmpl);
          }
          {
            /* Integrated: address-of the frame slot into a pool register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_VREG, BE_W64, {.vreg = dst_addr_reg}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(arg_stack->offset)}}};
            kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }

          if (codegen_target_is_windows()) {
            {
              Register_t *u[] = {dst_addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rcx\n");
            }
            {
              Register_t *u[] = {src_addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rdx\n");
            }
            inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
          } else {
            {
              Register_t *u[] = {dst_addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rdi\n");
            }
            {
              Register_t *u[] = {src_addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rsi\n");
            }
            inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
          }
          inst_list = codegen_vect_reg(inst_list, 0);
          inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
          free_arg_regs();
          free_reg(get_reg_stack(), dst_addr_reg);
          free_reg(get_reg_stack(), src_addr_reg);
          stack_arg_offset += 16;
        } else if (is_value_shortstring) {
          /* VALUE ShortString parameter: copy from caller's buffer
           * into the local stack buffer allocated above.
           * The incoming register (or stack slot) holds a pointer
           * to the caller's ShortString. */
          arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);

          /* Obtain the source pointer: prefer presaved slot, then
           * register, then stack. */
          Register_t *src_ptr_reg = NULL;
          const char *source_ptr = NULL;

          StackNode_t *presaved_slot = NULL;
          if (has_record_or_dynarray) {
            char presaved_name[64];
            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__",
                     (char *)arg_ids->cur);
            presaved_slot = find_label(presaved_name);
          }

          if (presaved_slot != NULL) {
            src_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (src_ptr_reg == NULL)
              src_ptr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_ptr_reg != NULL) {
              /* Integrated: load from the frame slot through the backend vtable. */
              BeEmitter em = codegen_beemitter(inst_list, ctx);
              BeOperand dst = {OPK_VREG, BE_W64, {.vreg = src_ptr_reg}};
              BeOperand src = {OPK_MEM_FRAME, BE_W64,
                               {.mem_frame = {BE_BASE_FP,
                                              -(long long)(presaved_slot->offset)}}};
              kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
              inst_list = em.list;
              source_ptr = src_ptr_reg->bit_64;
            }
          }

          if (source_ptr == NULL && arg_reg != NULL) {
            src_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (src_ptr_reg == NULL)
              src_ptr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_ptr_reg != NULL) {
              char tmpl[64];
              snprintf(tmpl, sizeof(tmpl), "\tmovq\t%s, %%0\n", arg_reg);
              Register_t *defs_arr[] = {src_ptr_reg};
              inst_list =
                  add_inst_du(inst_list, ctx, defs_arr, 1, NULL, 0, tmpl);
              source_ptr = src_ptr_reg->bit_64;
            }
          }

          if (source_ptr == NULL) {
            src_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (src_ptr_reg == NULL)
              src_ptr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_ptr_reg != NULL) {
              /* Integrated: load from the frame slot through the backend vtable. */
              BeEmitter em = codegen_beemitter(inst_list, ctx);
              BeOperand dst = {OPK_VREG, BE_W64, {.vreg = src_ptr_reg}};
              BeOperand src = {OPK_MEM_FRAME, BE_W64,
                               {.mem_frame = {BE_BASE_FP,
                                              (long long)(stack_arg_offset)}}};
              kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
              inst_list = em.list;
              stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
              source_ptr = src_ptr_reg->bit_64;
            }
          }

          if (source_ptr == NULL) {
            codegen_report_error(ctx,
                                 "ERROR: Unable to allocate register for "
                                 "ShortString parameter %s.",
                                 (char *)arg_ids->cur);
            return inst_list;
          }

          Register_t *dst_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
          if (dst_addr_reg == NULL)
            dst_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
          if (dst_addr_reg == NULL) {
            if (src_ptr_reg != NULL)
              free_reg(get_reg_stack(), src_ptr_reg);
            codegen_report_error(ctx,
                                 "ERROR: Unable to allocate destination "
                                 "register for ShortString parameter %s.",
                                 (char *)arg_ids->cur);
            return inst_list;
          }

          int ss_size = arg_stack->element_size;
          if (ss_size <= 0)
            ss_size = 256;

          /* leaq dest buffer address */
          {
            /* Integrated: address-of the frame slot into a pool register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_VREG, BE_W64, {.vreg = dst_addr_reg}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(arg_stack->offset)}}};
            kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }

          /* kgpc_shortstring_to_shortstring(dest, dest_size, src) */
          if (codegen_target_is_windows()) {
            {
              Register_t *u[] = {dst_addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rcx\n");
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", ss_size);
            inst_list = add_inst(inst_list, buffer);
            {
              Register_t *u[] = {src_ptr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %r8\n");
            }
          } else {
            {
              Register_t *u[] = {dst_addr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rdi\n");
            }
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n", ss_size);
            inst_list = add_inst(inst_list, buffer);
            {
              Register_t *u[] = {src_ptr_reg};
              inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                      "\tmovq\t%0, %rdx\n");
            }
          }
          inst_list = codegen_vect_reg(inst_list, 0);
          inst_list = codegen_call_with_shadow_space(
              inst_list, "kgpc_shortstring_to_shortstring");
          free_arg_regs();
          free_reg(get_reg_stack(), dst_addr_reg);
          if (src_ptr_reg != NULL)
            free_reg(get_reg_stack(), src_ptr_reg);
        } else if (use_sse_reg) {
          /* Check for a presaved slot: the pre-pass saves XMM register
           * params before any kgpc_move call (which can clobber XMM
           * registers under ASAN-intercepted memmove). */
          StackNode_t *sse_presaved_slot = NULL;
          if (has_record_or_dynarray) {
            char presaved_name[64];
            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__",
                     (char *)arg_ids->cur);
            sse_presaved_slot = find_label(presaved_name);
          }

          if (sse_presaved_slot != NULL) {
            /* Use presaved value — XMM reg may be clobbered by now */
            alloc_sse_arg_reg(&next_sse_index); /* advance index only */
            if (real_storage_size == 4)
              snprintf(buffer, sizeof(buffer),
                       "\tmovss\t-%d(%%rbp), %%xmm0\n"
                       "\tmovss\t%%xmm0, -%d(%%rbp)\n",
                       sse_presaved_slot->offset, arg_stack->offset);
            else
              snprintf(buffer, sizeof(buffer),
                       "\tmovsd\t-%d(%%rbp), %%xmm0\n"
                       "\tmovsd\t%%xmm0, -%d(%%rbp)\n",
                       sse_presaved_slot->offset, arg_stack->offset);
            inst_list = add_inst(inst_list, buffer);
          } else if (next_sse_index < kgpc_max_sse_arg_regs()) {
            const char *xmm_reg = alloc_sse_arg_reg(&next_sse_index);
            if (real_storage_size == 4)
              snprintf(buffer, sizeof(buffer), "\tmovss\t%s, -%d(%%rbp)\n",
                       xmm_reg, arg_stack->offset);
            else
              snprintf(buffer, sizeof(buffer), "\tmovsd\t%s, -%d(%%rbp)\n",
                       xmm_reg, arg_stack->offset);
            inst_list = add_inst(inst_list, buffer);
          } else {
            if (real_storage_size == 4) {
              snprintf(buffer, sizeof(buffer), "\tmovss\t%d(%%rbp), %%xmm0\n",
                       stack_arg_offset);
              inst_list = add_inst(inst_list, buffer);
              snprintf(buffer, sizeof(buffer), "\tmovss\t%%xmm0, -%d(%%rbp)\n",
                       arg_stack->offset);
            } else {
              snprintf(buffer, sizeof(buffer), "\tmovsd\t%d(%%rbp), %%xmm0\n",
                       stack_arg_offset);
              inst_list = add_inst(inst_list, buffer);
              snprintf(buffer, sizeof(buffer), "\tmovsd\t%%xmm0, -%d(%%rbp)\n",
                       arg_stack->offset);
            }
            inst_list = add_inst(inst_list, buffer);
            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
          }
        } else {
          arg_reg = alloc_integer_arg_reg(use_64bit, &next_gpr_index);
          Register_t *stack_value_reg = NULL;
          const char *value_source = NULL;

          /* Check if we have a presaved slot from pre-pass. We must use it
           * because the argument registers may have been clobbered by kgpc_move
           * calls when processing earlier record/dynarray parameters. */
          StackNode_t *presaved_slot = NULL;
          if (has_record_or_dynarray) {
            char presaved_name[64];
            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__",
                     (char *)arg_ids->cur);
            presaved_slot = find_label(presaved_name);
          }

          if (presaved_slot != NULL && arg_reg != NULL) {
            /* Load from presaved slot since register may be clobbered */
            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (stack_value_reg == NULL)
              stack_value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (stack_value_reg != NULL) {
              if (use_64bit) {
                snprintf(buffer, sizeof(buffer), "\tmovq\t-%d(%%rbp), %s\n",
                         presaved_slot->offset, stack_value_reg->bit_64);
                value_source = stack_value_reg->bit_64;
              } else {
                snprintf(buffer, sizeof(buffer), "\tmovl\t-%d(%%rbp), %s\n",
                         presaved_slot->offset, stack_value_reg->bit_32);
                value_source = stack_value_reg->bit_32;
              }
              inst_list = add_inst(inst_list, buffer);
            }
          }

          if (value_source == NULL) {
            value_source = arg_reg;
          }

          if (value_source == NULL) {
            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (stack_value_reg == NULL)
              stack_value_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (stack_value_reg == NULL) {
              codegen_report_error(
                  ctx, "ERROR: Unable to allocate register for argument %s.",
                  (char *)arg_ids->cur);
              return inst_list;
            }
            if (use_64bit) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                       stack_arg_offset, stack_value_reg->bit_64);
            } else {
              snprintf(buffer, sizeof(buffer), "\tmovl\t%d(%%rbp), %s\n",
                       stack_arg_offset, stack_value_reg->bit_32);
            }
            inst_list = add_inst(inst_list, buffer);
            stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
            value_source =
                use_64bit ? stack_value_reg->bit_64 : stack_value_reg->bit_32;
          }

          if (use_64bit)
            snprintf(buffer, 50, "\tmovq\t%s, -%d(%%rbp)\n", value_source,
                     arg_stack->offset);
          else
            snprintf(buffer, 50, "\tmovl\t%s, -%d(%%rbp)\n", value_source,
                     arg_stack->offset);
          inst_list = add_inst(inst_list, buffer);
          if (stack_value_reg != NULL)
            free_reg(get_reg_stack(), stack_value_reg);
        }
        arg_ids = arg_ids->next;
      }
      break;
    case TREE_ARR_DECL:
      arg_ids = arg_decl->tree_data.arr_decl_data.ids;
      while (arg_ids != NULL) {
        int is_value_shortstring = 0;
        if (arg_decl->tree_data.arr_decl_data.is_shortstring) {
          /* Check symtab: if NOT a var parameter, this is a
           * value ShortString that needs a local copy. */
          HashNode_t *sym = NULL;
          if (symtab != NULL)
            FindSymbol(&sym, symtab, (char *)arg_ids->cur);
          if (sym == NULL || !sym->is_var_parameter)
            is_value_shortstring = 1;
        }

        arg_reg = alloc_integer_arg_reg(1, &next_gpr_index);

        if (is_value_shortstring) {
          /* Allocate full ShortString buffer for value copy */
          int ss_size = 256;
          KgpcType *arr_type =
              arg_decl->tree_data.arr_decl_data.element_kgpc_type;
          if (arr_type != NULL) {
            int resolved_size = codegen_shortstring_storage_size(arr_type);
            if (resolved_size > 0)
              ss_size = resolved_size;
          }
          if (ss_size <= 0) {
            /* Try to compute from range */
            int s = arg_decl->tree_data.arr_decl_data.s_range;
            int e = arg_decl->tree_data.arr_decl_data.e_range;
            if (e >= s) {
              long long diff = (long long)e - (long long)s;
              if (diff >= 1 && diff <= (long long)INT_MAX - 1)
                ss_size = (int)(diff + 1);
              else
                ss_size = 256;
            } else {
              ss_size = 256;
            }
          }
          arg_stack = add_l_z_bytes((char *)arg_ids->cur, ss_size);
          /* NOT setting is_reference — local value copy */

          /* Copy from caller's buffer into local stack buffer */
          Register_t *src_ptr_reg = NULL;
          const char *source_ptr = NULL;

          StackNode_t *presaved_slot = NULL;
          if (has_record_or_dynarray) {
            char presaved_name[64];
            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__",
                     (char *)arg_ids->cur);
            presaved_slot = find_label(presaved_name);
          }

          if (presaved_slot != NULL) {
            src_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (src_ptr_reg == NULL)
              src_ptr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_ptr_reg != NULL) {
              {
                /* Integrated: load from the frame slot into a physical register
                 * via the vtable. */
                BeEmitter em = codegen_beemitter(inst_list, ctx);
                BeOperand dst = {OPK_PHYS, BE_W64, {.phys = src_ptr_reg->bit_64}};
                BeOperand src = {OPK_MEM_FRAME, BE_W64,
                                 {.mem_frame = {BE_BASE_FP, -(long long)(presaved_slot->offset)}}};
                kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
                inst_list = em.list;
              }
              source_ptr = src_ptr_reg->bit_64;
            }
          }

          if (source_ptr == NULL && arg_reg != NULL) {
            src_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (src_ptr_reg == NULL)
              src_ptr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_ptr_reg != NULL) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", arg_reg,
                       src_ptr_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
              source_ptr = src_ptr_reg->bit_64;
            }
          }

          if (source_ptr == NULL) {
            src_ptr_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (src_ptr_reg == NULL)
              src_ptr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
            if (src_ptr_reg != NULL) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                       stack_arg_offset, src_ptr_reg->bit_64);
              inst_list = add_inst(inst_list, buffer);
              stack_arg_offset += CODEGEN_POINTER_SIZE_BYTES;
              source_ptr = src_ptr_reg->bit_64;
            }
          }

          if (source_ptr == NULL) {
            codegen_report_error(ctx,
                                 "ERROR: Unable to allocate register for "
                                 "ShortString array parameter %s.",
                                 (char *)arg_ids->cur);
            return inst_list;
          }

          Register_t *dst_addr_reg = get_free_reg(get_reg_stack(), &inst_list);
          if (dst_addr_reg == NULL)
            dst_addr_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
          if (dst_addr_reg == NULL) {
            if (src_ptr_reg != NULL)
              free_reg(get_reg_stack(), src_ptr_reg);
            codegen_report_error(ctx,
                                 "ERROR: Unable to allocate dest register for "
                                 "ShortString array parameter %s.",
                                 (char *)arg_ids->cur);
            return inst_list;
          }

          {
            /* Integrated: address-of the frame slot into a physical register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = dst_addr_reg->bit_64}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(arg_stack->offset)}}};
            kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }

          if (codegen_target_is_windows()) {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n",
                     dst_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rdx\n", ss_size);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n", source_ptr);
            inst_list = add_inst(inst_list, buffer);
          } else {
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n",
                     dst_addr_reg->bit_64);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %%rsi\n", ss_size);
            inst_list = add_inst(inst_list, buffer);
            snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", source_ptr);
            inst_list = add_inst(inst_list, buffer);
          }
          inst_list = codegen_vect_reg(inst_list, 0);
          inst_list = codegen_call_with_shadow_space(
              inst_list, "kgpc_shortstring_to_shortstring");
          free_arg_regs();
          free_reg(get_reg_stack(), dst_addr_reg);
          if (src_ptr_reg != NULL)
            free_reg(get_reg_stack(), src_ptr_reg);
        } else {
          arg_stack = add_q_z((char *)arg_ids->cur);
          if (arg_stack != NULL)
            arg_stack->is_reference = 1;
          Register_t *stack_value_reg = NULL;
          const char *value_source = NULL;

          /* Check if we have a presaved slot from pre-pass */
          StackNode_t *presaved_slot = NULL;
          if (has_record_or_dynarray) {
            char presaved_name[64];
            snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__",
                     (char *)arg_ids->cur);
            presaved_slot = find_label(presaved_name);
          }

          if (presaved_slot != NULL && arg_reg != NULL) {
            /* Load from presaved slot since register may be clobbered */
            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (stack_value_reg != NULL) {
              {
                /* Integrated: load from the frame slot into a physical register
                 * via the vtable. */
                BeEmitter em = codegen_beemitter(inst_list, ctx);
                BeOperand dst = {OPK_PHYS, BE_W64, {.phys = stack_value_reg->bit_64}};
                BeOperand src = {OPK_MEM_FRAME, BE_W64,
                                 {.mem_frame = {BE_BASE_FP, -(long long)(presaved_slot->offset)}}};
                kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
                inst_list = em.list;
              }
              value_source = stack_value_reg->bit_64;
            }
          }

          if (value_source == NULL)
            value_source = arg_reg;
          if (value_source == NULL) {
            stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
            if (stack_value_reg == NULL) {
              codegen_report_error(
                  ctx,
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
          {
            /* Integrated: store a physical register to the frame slot via the
             * vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(arg_stack->offset)}}};
            BeOperand a = {OPK_PHYS, BE_W64, {.phys = value_source}};
            kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
            inst_list = em.list;
          }
          if (stack_value_reg != NULL)
            free_reg(get_reg_stack(), stack_value_reg);
        }
        arg_ids = arg_ids->next;
        param_index++;
      }
      break;
    default:
      fprintf(stderr,
              "WARNING: Unknown argument type %d for procedure parameter.\n",
              arg_decl ? arg_decl->type : -1);
      break;
    }
    args = args->next;
  }

  if (record_param_queue != NULL) {
    ListNode_t *rec_node = record_param_queue;
    while (rec_node != NULL) {
      RecordParamWork *work = (RecordParamWork *)rec_node->cur;
      StackNode_t *record_slot = add_l_x((char *)work->id, work->size);
      if (record_slot == NULL) {
        codegen_report_error(
            ctx, "ERROR: Failed to allocate storage for record parameter %s.",
            work->id != NULL ? work->id : "(null)");
        free(work);
        rec_node = rec_node->next;
        continue;
      }
      if (work->is_dynarray) {
        record_slot->is_array = 1;
        record_slot->is_dynamic = 1;
        record_slot->element_size = (work->dynarray_elem_size > 0)
                                        ? work->dynarray_elem_size
                                        : DOUBLEWORD;
        record_slot->array_lower_bound = work->dynarray_lower_bound;
      }

      Register_t *stack_value_reg = NULL;
      char presaved_name[64];
      snprintf(presaved_name, sizeof(presaved_name), "__presaved_%s__",
               work->id);
      StackNode_t *presaved_slot = find_label(presaved_name);
      const char *record_src_reg = NULL;
      Register_t *loaded_param_reg = NULL;

      if (presaved_slot != NULL) {
        loaded_param_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (loaded_param_reg == NULL)
          loaded_param_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
        if (loaded_param_reg != NULL) {
          {
            /* Integrated: load from the frame slot into a physical register via
             * the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = loaded_param_reg->bit_64}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(presaved_slot->offset)}}};
            kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          record_src_reg = loaded_param_reg->bit_64;
        }
      }
      if (record_src_reg == NULL && work->arg_reg != NULL) {
        record_src_reg = work->arg_reg;
      }

      if (record_src_reg == NULL) {
        if (work->arg_reg == NULL && !work->has_stack_arg) {
          const char *fallback_reg =
              current_arg_reg64(arg_start_index + work->arg_index);
          if (fallback_reg != NULL)
            record_src_reg = fallback_reg;
        }
      }

      if (record_src_reg == NULL) {
        if (!work->has_stack_arg) {
          codegen_report_error(ctx,
                               "ERROR: Unable to locate record parameter %s.",
                               work->id != NULL ? work->id : "(null)");
          if (loaded_param_reg != NULL)
            free_reg(get_reg_stack(), loaded_param_reg);
          free(work);
          rec_node = rec_node->next;
          continue;
        }

        stack_value_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (stack_value_reg == NULL) {
          codegen_report_error(
              ctx,
              "ERROR: Unable to allocate register for record parameter %s.",
              work->id != NULL ? work->id : "(null)");
          if (loaded_param_reg != NULL)
            free_reg(get_reg_stack(), loaded_param_reg);
          free(work);
          rec_node = rec_node->next;
          continue;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%%rbp), %s\n",
                 work->stack_arg_offset, stack_value_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
        record_src_reg = stack_value_reg->bit_64;
      }

      Register_t *size_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (size_reg == NULL)
        size_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (size_reg == NULL) {
        codegen_report_error(
            ctx,
            "ERROR: Unable to allocate register for record parameter size.");
        if (stack_value_reg != NULL)
          free_reg(get_reg_stack(), stack_value_reg);
        if (loaded_param_reg != NULL)
          free_reg(get_reg_stack(), loaded_param_reg);
        free(work);
        rec_node = rec_node->next;
        continue;
      }

      snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", work->size,
               size_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);

      if (codegen_target_is_windows()) {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", record_src_reg);
        inst_list = add_inst(inst_list, buffer);
        {
          /* Integrated: address-of the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(record_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%r8\n",
                 size_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      } else {
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", record_src_reg);
        inst_list = add_inst(inst_list, buffer);
        {
          /* Integrated: address-of the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(record_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n",
                 size_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);
      }

      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
      free_arg_regs();
      free_reg(get_reg_stack(), size_reg);
      if (stack_value_reg != NULL)
        free_reg(get_reg_stack(), stack_value_reg);
      if (loaded_param_reg != NULL)
        free_reg(get_reg_stack(), loaded_param_reg);

      free(work);
      rec_node = rec_node->next;
    }
    DestroyList(record_param_queue);
  }

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
  return inst_list;
}

static ListNode_t *codegen_store_class_typeinfo(ListNode_t *inst_list,
                                                CodeGenContext *ctx,
                                                StackNode_t *var_node,
                                                const char *type_name) {
  if (var_node == NULL || type_name == NULL || type_name[0] == '\0' ||
      var_node->is_reference)
    return inst_list;

  char typeinfo_label[512];
  snprintf(typeinfo_label, sizeof(typeinfo_label), "%s_VMT", type_name);

  /* Class variables are pointers to instances. We need to:
   * 1. Allocate memory for the instance (size determined from type)
   * 2. Store the typeinfo pointer in the first field
   * 3. Store the instance pointer in the variable
   *
   * For now, we use a simplified approach: allocate a fixed size (64 bytes
   * should be enough for most classes) and zero-initialize with calloc. A
   * better approach would compute the actual size from the RecordType.
   */

  /* Allocate the instance through the runtime helper. This path is used in
   * early generic class materialization where the Pascal heap bootstrap may
   * not be ready yet. */
  char buffer[1024];
  const char *size_reg =
      current_arg_reg64(0); /* RDI on Linux, RCX on Windows */

  if (size_reg != NULL) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t$64, %s\n", size_reg);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_allocmem");

    /* RAX now contains the pointer to the allocated instance */
    /* Store the typeinfo pointer in the first field */
    inst_list =
        add_inst(inst_list, "\tpushq\t%rax\n"); /* Save instance pointer */
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %%r10\n",
             typeinfo_label);
    inst_list = add_inst(inst_list, buffer);
    inst_list =
        add_inst(inst_list,
                 "\tmovq\t%r10, (%rax)\n"); /* Store typeinfo in first field */
    inst_list =
        add_inst(inst_list, "\tpopq\t%rax\n"); /* Restore instance pointer */

    /* Store the instance pointer in the class variable */
    if (var_node->is_static) {
      const char *label = var_node->static_label != NULL
                              ? var_node->static_label
                              : var_node->label;
      snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, %s(%%rip)\n", label);
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%%rax, -%d(%%rbp)\n",
               var_node->offset);
    }
    inst_list = add_inst(inst_list, buffer);
  } else {
    /* Fallback if we can't determine arg registers - just store NULL for now */
    if (ctx != NULL) {
      codegen_report_error(ctx, "ERROR: Unable to allocate class instance - "
                                "register allocation failed");
    }
    if (var_node->is_static) {
      const char *label = var_node->static_label != NULL
                              ? var_node->static_label
                              : var_node->label;
      snprintf(buffer, sizeof(buffer), "\tmovq\t$0, %s(%%rip)\n", label);
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
               var_node->offset);
    }
    inst_list = add_inst(inst_list, buffer);
  }

  return inst_list;
}

static ListNode_t *codegen_emit_tfile_configure(ListNode_t *inst_list,
                                                StackNode_t *file_node,
                                                long long element_size,
                                                int element_hash_tag) {
  if (file_node == NULL || element_size <= 0)
    return inst_list;

  const char *slot_reg = current_arg_reg64(0);
  const char *size_reg = current_arg_reg64(1);
  const char *tag_reg = current_arg_reg32(2);
  if (slot_reg == NULL || size_reg == NULL || tag_reg == NULL)
    return inst_list;

  char buffer[256];
  if (file_node->is_static) {
    const char *label = (file_node->static_label != NULL)
                            ? file_node->static_label
                            : file_node->label;
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s(%%rip), %s\n", label,
             slot_reg);
  } else {
    snprintf(buffer, sizeof(buffer), "\tleaq\t-%d(%%rbp), %s\n",
             file_node->offset, slot_reg);
  }
  inst_list = add_inst(inst_list, buffer);

  snprintf(buffer, sizeof(buffer), "\tmovq\t$%lld, %s\n", element_size,
           size_reg);
  inst_list = add_inst(inst_list, buffer);

  snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, %s\n", element_hash_tag,
           tag_reg);
  inst_list = add_inst(inst_list, buffer);

  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_tfile_configure");
  return inst_list;
}

static int codegen_type_tag_to_hashvar(int parser_tag) {
  switch (parser_tag) {
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

static long long codegen_type_tag_size(int parser_tag) {
  switch (parser_tag) {
  case CHAR_TYPE:
  case BOOL:
    return 1;
  case LONGINT_TYPE:
    return 4; // Match FPC's 32-bit LongInt
  case REAL_TYPE:
    return 8;
  case INT_TYPE:
  default:
    return 4;
  }
}

static int codegen_resolve_file_component(const struct TypeAlias *alias,
                                          SymTab_t *symtab,
                                          long long *element_size_out,
                                          int *element_hash_tag_out) {
  if (alias == NULL || !alias->is_file || element_size_out == NULL ||
      element_hash_tag_out == NULL)
    return 0;

  int parser_tag = alias->file_type;
  HashNode_t *type_node = NULL;
  if (parser_tag == UNKNOWN_TYPE && alias->file_type_id != NULL &&
      symtab != NULL) {
    if (FindSymbol(&type_node, symtab, alias->file_type_id) != 0 &&
        type_node != NULL) {
      if (type_node->type != NULL)
        parser_tag = kgpc_type_get_primitive_tag(type_node->type);
    }
  }

  if (parser_tag == UNKNOWN_TYPE && type_node != NULL &&
      type_node->type != NULL)
    parser_tag = kgpc_type_get_primitive_tag(type_node->type);

  if (parser_tag == UNKNOWN_TYPE)
    parser_tag = INT_TYPE;

  long long elem_size = codegen_type_tag_size(parser_tag);
  int hash_tag = codegen_type_tag_to_hashvar(parser_tag);

  if (type_node != NULL && type_node->type != NULL) {
    long long resolved_size = kgpc_type_sizeof(type_node->type);
    if (resolved_size > 0)
      elem_size = resolved_size;

    int resolved_tag = kgpc_type_get_primitive_tag(type_node->type);
    if (resolved_tag != UNKNOWN_TYPE)
      hash_tag = codegen_type_tag_to_hashvar(resolved_tag);
  }

  *element_size_out = elem_size;
  *element_hash_tag_out = hash_tag;
  return 1;
}

/* ------------------------------------------------------------------------ */
/* Typed-const record static .data initialisation                           */
/* ------------------------------------------------------------------------ */
/* A typed-const record declaration like
 *
 *     MemoryManager: TMemoryManager = (
 *         NeedLock: false; GetMem: @SysGetMem; ... );
 *
 * has historically been lowered (in the parser) to a compound statement of
 * field-by-field assignments executed at runtime from the program body.  That
 * runtime init clobbers any prior .bss contents — in particular it overwrites
 * the values placed there by C ctors such as kgpc_init_memory_manager.
 *
 * For records whose initialiser values are all statically resolvable
 * constants (integer literals, NIL, address-of-procedure expressions, booleans,
 * etc.), we emit a .data block at compile time that bakes the field values
 * directly into the symbol's storage.  A C constructor that later runs (before
 * main) is then free to override any individual field, and the program body
 * no longer runs the field-by-field stores.  This unifies the allocator chain
 * end-to-end without overriding or special-casing any FPC RTL symbol by name.
 */

/* Try to evaluate an Expression as a 64-bit integer constant.
 * Returns 0 on success with *out set; non-zero on failure. */
static int codegen_typed_const_eval_int(struct Expression *expr,
                                        long long *out) {
  if (expr == NULL || out == NULL)
    return -1;
  switch (expr->type) {
  case EXPR_INUM:
    *out = expr->expr_data.i_num;
    return 0;
  case EXPR_BOOL:
    *out = expr->expr_data.bool_value ? 1 : 0;
    return 0;
  case EXPR_CHAR_CODE:
    *out = (long long)expr->expr_data.char_code;
    return 0;
  case EXPR_NIL:
    *out = 0;
    return 0;
  case EXPR_SIGN_TERM: {
    long long sub = 0;
    if (codegen_typed_const_eval_int(expr->expr_data.sign_term, &sub) != 0)
      return -1;
    *out = -sub;
    return 0;
  }
  case EXPR_TYPECAST: {
    /* TypeCast around an integer literal evaluates to the inner int. */
    struct Expression *inner = expr->expr_data.typecast_data.expr;
    return codegen_typed_const_eval_int(inner, out);
  }
  case EXPR_SET: {
    /* Small-set bitmask emitted by the parser as an unsigned word. */
    if (expr->expr_data.set_data.is_constant) {
      *out = (long long)(unsigned int)expr->expr_data.set_data.bitmask;
      return 0;
    }
    return -1;
  }
  default:
    return -1;
  }
}

/* Is `expr` statically resolvable for emission as a field initialiser? */
static int codegen_typed_const_is_static(struct Expression *expr) {
  if (expr == NULL)
    return 0;
  switch (expr->type) {
  case EXPR_INUM:
  case EXPR_BOOL:
  case EXPR_CHAR_CODE:
  case EXPR_NIL:
  case EXPR_ADDR_OF_PROC:
    return 1;
  case EXPR_SIGN_TERM:
    return codegen_typed_const_is_static(expr->expr_data.sign_term);
  case EXPR_TYPECAST:
    return codegen_typed_const_is_static(expr->expr_data.typecast_data.expr);
  case EXPR_SET:
    return expr->expr_data.set_data.is_constant ? 1 : 0;
  default:
    return 0;
  }
}

/* Determine the on-disk size for a field, given the resolved RecordField. */
static long long codegen_typed_const_field_size(struct RecordField *field) {
  if (field == NULL)
    return 0;
  if (field->has_cached_layout && field->cached_size > 0)
    return field->cached_size;
  /* Fall back to deriving from the primitive tag. */
  long long sz = sizeof_from_type_tag(field->type);
  if (sz > 0)
    return sz;
  /* Pointers and procedure-pointer fields default to 8 bytes. */
  if (field->is_pointer || field->proc_type != NULL)
    return 8;
  return 0;
}

/* Emit one initialiser entry covering `size` bytes starting at the current
 * cursor, taking the value from `value_expr`.  `static_label` is unused here
 * but kept for symmetry with future extensions.  Returns 0 on success. */
static int codegen_emit_typed_const_field_data(FILE *out,
                                               struct Expression *value_expr,
                                               long long size) {
  if (out == NULL || value_expr == NULL || size <= 0)
    return -1;

  /* Address-of-procedure: emit the function label as .quad. */
  if (value_expr->type == EXPR_ADDR_OF_PROC) {
    const char *label = value_expr->expr_data.addr_of_proc_data.proc_mangled_id;
    if (label == NULL || label[0] == '\0')
      return -1;
    if (size != 8)
      return -1; /* Function pointer must be 8 bytes. */
    fprintf(out, "\t.quad\t%s\n", label);
    return 0;
  }

  long long ival = 0;
  if (codegen_typed_const_eval_int(value_expr, &ival) == 0) {
    switch (size) {
    case 1:
      fprintf(out, "\t.byte\t%lld\n", ival & 0xff);
      break;
    case 2:
      fprintf(out, "\t.word\t%lld\n", ival & 0xffff);
      break;
    case 4:
      fprintf(out, "\t.long\t%lld\n", ival & 0xffffffffll);
      break;
    case 8:
      fprintf(out, "\t.quad\t%lld\n", ival);
      break;
    default:
      /* Wider widths: emit raw bytes (little-endian).  Caller pads
       * via the surrounding offset cursor. */
      {
        unsigned long long uval = (unsigned long long)ival;
        for (long long i = 0; i < size; ++i) {
          unsigned int b = (unsigned int)((uval >> (i * 8)) & 0xff);
          if (i == 0)
            fprintf(out, "\t.byte\t%u", b);
          else
            fprintf(out, ", %u", b);
        }
        fputc('\n', out);
      }
      break;
    }
    return 0;
  }

  return -1;
}

/* Walk `stmt` collecting field-name -> value-expr pairs into the supplied
 * arrays.  Returns 0 on success.  Each pair must originate from a
 * `<var>.<field> := <expr>` assignment where the base is the typed-const var
 * itself.  Failure to match any of these shapes returns non-zero so the
 * caller can fall back to runtime initialisation. */
typedef struct {
  const char *field_name;
  struct Expression *value;
} TypedConstFieldInit;

static int codegen_collect_typed_const_inits(struct Statement *stmt,
                                             const char *var_name,
                                             TypedConstFieldInit *out,
                                             int max_out, int *count_inout) {
  if (stmt == NULL || count_inout == NULL)
    return -1;
  if (stmt->type == STMT_COMPOUND_STATEMENT) {
    ListNode_t *cur = stmt->stmt_data.compound_statement;
    while (cur != NULL) {
      struct Statement *child = (struct Statement *)cur->cur;
      if (codegen_collect_typed_const_inits(child, var_name, out, max_out,
                                            count_inout) != 0)
        return -1;
      cur = cur->next;
    }
    return 0;
  }
  if (stmt->type != STMT_VAR_ASSIGN)
    return -1;
  struct Expression *lhs = stmt->stmt_data.var_assign_data.var;
  struct Expression *rhs = stmt->stmt_data.var_assign_data.expr;
  if (lhs == NULL || rhs == NULL)
    return -1;
  if (lhs->type != EXPR_RECORD_ACCESS)
    return -1;
  struct Expression *base = lhs->expr_data.record_access_data.record_expr;
  const char *field = lhs->expr_data.record_access_data.field_id;
  if (base == NULL || base->type != EXPR_VAR_ID || base->expr_data.id == NULL ||
      field == NULL)
    return -1;
  if (!pascal_identifier_equals(base->expr_data.id, var_name))
    return -1;
  if (*count_inout >= max_out)
    return -1;
  out[*count_inout].field_name = field;
  out[*count_inout].value = rhs;
  (*count_inout)++;
  return 0;
}

/* Try to emit a typed-const record's storage statically into .data, writing
 * each field at its offset and zero-filling the gaps.  Returns 0 if the full
 * record was emitted (and the caller must skip the runtime init).  Returns
 * non-zero (negative) otherwise.
 *
 * `static_label` is the assembler symbol that backs storage (e.g.
 * "__kgpc_program_var_MemoryManager_96").  `total_size` is the storage size.
 * `emit_bare_alias` is non-zero when the bare Pascal name (var_name) should
 * be globally exported as an alias to static_label, mirroring the policy
 * that codegen_emit_bss_or_comm uses for unit-defined typed-consts. */
int codegen_try_emit_typed_const_record_static_alias(
    CodeGenContext *ctx, SymTab_t *symtab, Tree_t *decl, const char *var_name,
    const char *static_label, long long total_size, int emit_bare_alias) {
  if (ctx == NULL || ctx->output_file == NULL || decl == NULL ||
      decl->type != TREE_VAR_DECL)
    return -1;
  if (!decl->tree_data.var_decl_data.is_typed_const)
    return -1;
  if (decl->tree_data.var_decl_data.static_init_emitted)
    return -1; /* Already done; don't double-emit. */
  if (var_name == NULL || static_label == NULL || total_size <= 0)
    return -1;
  struct Statement *init_stmt = decl->tree_data.var_decl_data.initializer;
  if (init_stmt == NULL)
    return -1;

  /* Resolve the variable's RecordType.  Prefer the inline record (records
   * declared inline have no named type), otherwise look up by type_id. */
  struct RecordType *record = decl->tree_data.var_decl_data.inline_record_type;
  HashNode_t *type_node = NULL;
  if (record == NULL && decl->tree_data.var_decl_data.type_id != NULL &&
      symtab != NULL) {
    if (FindSymbol(&type_node, symtab, decl->tree_data.var_decl_data.type_id) !=
            0 &&
        type_node != NULL) {
      record = hashnode_get_record_type(type_node);
    }
  }
  if (record == NULL)
    return -1;
  /* Don't statically initialise class instances — class typed-consts hold
   * a class reference pointer rather than the instance layout. */
  if (record_type_is_class(record))
    return -1;

  /* Collect (field, expr) pairs from the lowered initialiser. */
  TypedConstFieldInit inits[64];
  int init_count = 0;
  if (codegen_collect_typed_const_inits(init_stmt, var_name, inits, 64,
                                        &init_count) != 0)
    return -1;
  if (init_count == 0)
    return -1;

  /* Resolve each field to an offset + size + value.  All values must be
   * statically evaluable for us to commit to the .data path. */
  typedef struct {
    long long offset;
    long long size;
    struct Expression *value;
  } ResolvedInit;
  ResolvedInit resolved[64];
  int resolved_count = 0;
  for (int i = 0; i < init_count; ++i) {
    if (!codegen_typed_const_is_static(inits[i].value))
      return -1;
    struct RecordField *field_desc = NULL;
    long long offset = 0;
    if (resolve_record_field(symtab, record, inits[i].field_name, &field_desc,
                             &offset, decl->line_num, 1) != 0 ||
        field_desc == NULL)
      return -1;
    long long size = codegen_typed_const_field_size(field_desc);
    if (size <= 0)
      return -1;
    if (offset < 0 || offset + size > total_size)
      return -1;
    resolved[resolved_count].offset = offset;
    resolved[resolved_count].size = size;
    resolved[resolved_count].value = inits[i].value;
    resolved_count++;
  }
  /* Sort by offset (insertion sort — N<=64). */
  for (int i = 1; i < resolved_count; ++i) {
    ResolvedInit cur = resolved[i];
    int j = i - 1;
    while (j >= 0 && resolved[j].offset > cur.offset) {
      resolved[j + 1] = resolved[j];
      j--;
    }
    resolved[j + 1] = cur;
  }
  /* Detect overlaps. */
  for (int i = 1; i < resolved_count; ++i) {
    if (resolved[i].offset < resolved[i - 1].offset + resolved[i - 1].size)
      return -1;
  }

  /* Determine alignment and bare-name alias emission, mirroring
   * codegen_emit_bss_or_comm so the existing .bss path and this .data path
   * stay symmetric. */
  int alignment = (total_size >= 8) ? 8 : DOUBLEWORD;
  FILE *out = ctx->output_file;
  int need_alias = emit_bare_alias && strcmp(var_name, static_label) != 0;

  /* Emit into .data so the loader populates the storage at program start. */
  if (codegen_target_is_windows())
    fprintf(out, "\t.section .data\n");
  else
    fprintf(out, "\t.pushsection .data\n");
  if (alignment > 0)
    fprintf(out, "\t.align\t%d\n", alignment);
  fprintf(out, "\t.globl\t%s\n", static_label);
  fprintf(out, "%s:\n", static_label);

  long long cursor = 0;
  for (int i = 0; i < resolved_count; ++i) {
    if (resolved[i].offset > cursor)
      fprintf(out, "\t.zero\t%lld\n", resolved[i].offset - cursor);
    if (codegen_emit_typed_const_field_data(out, resolved[i].value,
                                            resolved[i].size) != 0) {
      /* Pipe a hard failure: rewind through a marker.  We've already
       * emitted partial data, so just zero the rest and let the caller
       * also schedule a runtime store.  But better: bail before this
       * partial state.  Since is_static was checked beforehand, this
       * shouldn't happen — but guard anyway. */
      if (codegen_target_is_windows())
        fprintf(out, "\t.section .text\n");
      else
        fprintf(out, "\t.popsection\n");
      return -1;
    }
    cursor = resolved[i].offset + resolved[i].size;
  }
  if (cursor < total_size)
    fprintf(out, "\t.zero\t%lld\n", total_size - cursor);

  if (need_alias) {
    fprintf(out, "\t.globl\t%s\n", var_name);
    fprintf(out, "\t.set\t%s, %s\n", var_name, static_label);
  }

  if (codegen_target_is_windows())
    fprintf(out, "\t.section .text\n");
  else
    fprintf(out, "\t.popsection\n");

  decl->tree_data.var_decl_data.static_init_emitted = 1;
  return 0;
}

ListNode_t *codegen_var_initializers(ListNode_t *decls, ListNode_t *inst_list,
                                     CodeGenContext *ctx, SymTab_t *symtab) {
  assert(ctx != NULL);
  assert(symtab != NULL);
  while (decls != NULL) {
    Tree_t *decl = (Tree_t *)decls->cur;
    if (decl == NULL) {
      decls = decls->next;
      continue;
    }

    if (decl->type == TREE_VAR_DECL) {
      HashNode_t *type_node = NULL;
      if (decl->tree_data.var_decl_data.type_id != NULL)
        FindSymbol(&type_node, symtab, decl->tree_data.var_decl_data.type_id);

      struct TypeAlias *alias = get_type_alias_from_node(type_node);
      if (alias != NULL && alias->is_array && alias->is_open_array) {
        ListNode_t *ids = decl->tree_data.var_decl_data.ids;
        while (ids != NULL) {
          char *var_name = (char *)ids->cur;
          StackNode_t *array_node = find_label(var_name);
          if (array_node != NULL && array_node->is_dynamic &&
              array_node->offset > 0) {
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                     array_node->offset);
            inst_list = add_inst(inst_list, buffer);
            int length_offset = array_node->offset - 2 * DOUBLEWORD;
            if (length_offset < array_node->offset) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                       length_offset);
              inst_list = add_inst(inst_list, buffer);
            }
          }
          ids = ids->next;
        }
      }

      /* Initialize FILE variables to NULL */
      if ((type_node != NULL && node_is_file_type(type_node)) ||
          (type_node == NULL &&
           decl->tree_data.var_decl_data.type == FILE_TYPE)) {
        struct TypeAlias *decl_inline_alias =
            decl->tree_data.var_decl_data.inline_type_alias;
        ListNode_t *ids = decl->tree_data.var_decl_data.ids;
        while (ids != NULL) {
          char *var_name = (char *)ids->cur;
          StackNode_t *file_node = find_label(var_name);
          if (file_node != NULL) {
            char buffer[128];
            if (!file_node->is_static) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                       file_node->offset);
              inst_list = add_inst(inst_list, buffer);
            }

            long long file_elem_size = 0;
            int file_elem_hash = HASHVAR_INTEGER;
            struct TypeAlias *file_alias = get_type_alias_from_node(type_node);
            if (file_alias == NULL && decl_inline_alias != NULL)
              file_alias = decl_inline_alias;
            if (file_alias == NULL || !file_alias->is_file) {
              HashNode_t *var_hash = NULL;
              if (FindSymbol(&var_hash, symtab, var_name) != 0 &&
                  var_hash != NULL)
                file_alias = hashnode_get_type_alias(var_hash);
            }

            int have_component = codegen_resolve_file_component(
                file_alias, symtab, &file_elem_size, &file_elem_hash);

            if (have_component) {
              inst_list = codegen_emit_tfile_configure(
                  inst_list, file_node, file_elem_size, file_elem_hash);
            }
          }
          ids = ids->next;
        }
      }

      struct Statement *init_stmt = decl->tree_data.var_decl_data.initializer;
      /* Suppress the runtime field-by-field initialiser when the
       * record's .data block has already been emitted statically
       * during codegen_function_locals (see
       * codegen_try_emit_typed_const_record_static_ex).  This keeps
       * any C constructor that fills the same record (e.g.
       * kgpc_init_memory_manager) from being clobbered. */
      if (init_stmt != NULL && decl->tree_data.var_decl_data.is_typed_const &&
          decl->tree_data.var_decl_data.static_init_emitted) {
        init_stmt = NULL;
      }
      if (type_node != NULL && node_is_class_type(type_node)) {
        struct RecordType *record_desc = hashnode_get_record_type(type_node);
        const char *class_type_name =
            (record_desc != NULL && record_desc->type_id != NULL)
                ? record_desc->type_id
                : codegen_resolve_record_type_name(type_node, symtab);
        ListNode_t *ids = decl->tree_data.var_decl_data.ids;
        while (ids != NULL) {
          char *var_name = (char *)ids->cur;
          StackNode_t *var_node = find_label(var_name);
          inst_list = codegen_store_class_typeinfo(inst_list, ctx, var_node,
                                                   class_type_name);
          ids = ids->next;
        }
      }

      if (init_stmt != NULL)
        inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
    } else if (decl->type == TREE_ARR_DECL) {
      struct Array *arr = &decl->tree_data.arr_decl_data;
      if (arr->e_range < arr->s_range) {
        ListNode_t *ids = arr->ids;
        while (ids != NULL) {
          char *var_name = (char *)ids->cur;
          StackNode_t *array_node = find_label(var_name);
          if (array_node != NULL && array_node->is_dynamic &&
              array_node->offset > 0) {
            char buffer[128];
            snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                     array_node->offset);
            inst_list = add_inst(inst_list, buffer);
            int length_offset = array_node->offset - 2 * DOUBLEWORD;
            if (length_offset < array_node->offset) {
              snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
                       length_offset);
              inst_list = add_inst(inst_list, buffer);
            }
          }
          ids = ids->next;
        }
      }
      struct Statement *init_stmt = arr->initializer;
      if (init_stmt != NULL) {
        if (arr->is_typed_const && arr->init_guard_label != NULL) {
          char done_label[64];
          gen_label(done_label, sizeof(done_label), ctx);

          char buffer[128];
          snprintf(buffer, sizeof(buffer), "\tmovb\t%s(%%rip), %%al\n",
                   arr->init_guard_label);
          inst_list = add_inst(inst_list, buffer);
          inst_list = add_inst(inst_list, "\ttestb\t%al, %al\n");
          {
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            kgpc_backend_target()->emit_branch(&em, BE_NE, done_label);
            inst_list = em.list;
          }

          inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);

          snprintf(buffer, sizeof(buffer), "\tmovb\t$1, %s(%%rip)\n",
                   arr->init_guard_label);
          inst_list = add_inst(inst_list, buffer);

          char label_decl[96];
          snprintf(label_decl, sizeof(label_decl), "%s:\n", done_label);
          inst_list = add_inst(inst_list, label_decl);
        } else {
          inst_list = codegen_stmt(init_stmt, inst_list, ctx, symtab);
        }
      }
    }
    decls = decls->next;
  }
  return inst_list;
}
#if KGPC_ENABLE_REG_DEBUG
extern const char *g_reg_debug_context;
#endif
