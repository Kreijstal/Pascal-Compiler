/*
    Damon Gwinn
    Code generation for array expressions
*/

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/SemCheck.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_Expr_Internal.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "codegen.h"
#include "codegen_expr_array.h"
#include "codegen_expression.h"
#include "expr_tree/expr_tree.h"
#include "register_types.h"
#include "stackmng/stackmng.h"

#define CODEGEN_POINTER_SIZE_BYTES (kgpc_target_pointer_size())

struct RecordType *codegen_expr_record_type(const struct Expression *expr,
                                            SymTab_t *symtab);
int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
                        struct RecordType *record_type, long long *size_out,
                        int depth);

/* Compute the per-element storage size of an AST-declared array's element
 * type when the element is a shortstring.  Returns the in-memory size
 * (length-byte + capacity) in bytes, or 0 if the element type is not a
 * shortstring or its size cannot be determined.
 *
 * Mirrors the logic of the in-line fallback below (the SHORTSTRING_TYPE
 * and TYPE_KIND_ARRAY-of-CHAR branches), but operates on the AST's
 * `arr_decl_data.element_kgpc_type` so it works for cross-unit lookups
 * where the symtab's HashNode may carry a different declaration's
 * generic ShortString element type. */
static long long codegen_arr_decl_shortstring_elem_storage(const Tree_t *decl) {
  if (decl == NULL || decl->type != TREE_ARR_DECL)
    return 0;
  KgpcType *etype = decl->tree_data.arr_decl_data.element_kgpc_type;
  if (etype == NULL)
    return 0;

  /* SHORTSTRING_TYPE primitive with alias info: storage is N+1 bytes,
   * where N is the declared capacity recorded on the alias. */
  if (etype->kind == TYPE_KIND_PRIMITIVE &&
      etype->info.primitive_type_tag == SHORTSTRING_TYPE &&
      etype->type_alias != NULL && etype->type_alias->is_shortstring &&
      etype->type_alias->array_end > 0) {
    return (long long)etype->type_alias->array_end + 1;
  }

  if (kgpc_type_string_storage_kind(etype) != KGPC_STRING_STORAGE_SHORTSTRING)
    return 0;

  /* TYPE_KIND_ARRAY of CHAR: shortstring is represented either as
   *   - array[0..N] of char  (start=0, end=N): includes the length-byte
   *     slot at index 0 — total storage is N+1 bytes and that's exactly
   *     what kgpc_type_sizeof reports.
   *   - array[1..N] of char  (start=1, end=N): data-only — kgpc_type_sizeof
   *     reports N bytes and the on-disk shortstring adds one more byte
   *     for the length prefix.
   * Distinguish by start_index so we don't double-count for the form
   * the AST actually emits for string[N] typed-const elements. */
  if (etype->kind == TYPE_KIND_ARRAY) {
    long long ds = kgpc_type_sizeof(etype);
    if (ds > 0)
      return (etype->info.array_info.start_index == 0) ? ds : ds + 1;
  }
  return 0;
}

static int codegen_type_is_inline_shortstring_storage(const KgpcType *type) {
  if (type == NULL)
    return 0;

  struct TypeAlias *alias = kgpc_type_get_type_alias((KgpcType *)type);
  if (alias != NULL && alias->is_shortstring)
    return 1;

  if (kgpc_type_string_storage_kind((KgpcType *)type) ==
      KGPC_STRING_STORAGE_SHORTSTRING)
    return 1;

  if (type->kind == TYPE_KIND_ARRAY &&
      type->info.array_info.element_type != NULL &&
      type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
      type->info.array_info.element_type->info.primitive_type_tag ==
          CHAR_TYPE &&
      type->info.array_info.start_index == 0 &&
      type->info.array_info.end_index >= 0 &&
      type->info.array_info.end_index <= 255)
    return 1;

  return 0;
}

static int codegen_type_is_shortstring_result_storage(const KgpcType *type) {
  if (type == NULL)
    return 0;
  if (codegen_type_is_inline_shortstring_storage(type))
    return 1;
  return kgpc_type_string_storage_kind((KgpcType *)type) ==
         KGPC_STRING_STORAGE_SHORTSTRING;
}

static int codegen_record_field_is_inline_shortstring_storage(
    const struct RecordField *field, const KgpcType *field_type) {
  if (field == NULL)
    return 0;

  if (field_type != NULL &&
      (codegen_type_is_inline_shortstring_storage(field_type) ||
       kgpc_type_string_storage_kind((KgpcType *)field_type) ==
           KGPC_STRING_STORAGE_SHORTSTRING))
    return 1;

  if (field->type == SHORTSTRING_TYPE)
    return 1;

  if (field->type == STRING_TYPE && field->has_cached_layout &&
      field->cached_size > CODEGEN_POINTER_SIZE_BYTES)
    return 1;

  return 0;
}

static int codegen_expr_is_current_shortstring_result_storage(
    const struct Expression *expr, CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || expr->type != EXPR_VAR_ID ||
      expr->expr_data.id == NULL)
    return 0;

  if (!codegen_type_is_shortstring_result_storage(ctx->current_return_type))
    return 0;

  if (pascal_identifier_equals(expr->expr_data.id, "Result")) {
    HashNode_t *shadow_node = NULL;
    if (ctx->symtab != NULL &&
        FindSymbol(&shadow_node, ctx->symtab, expr->expr_data.id) != 0 &&
        shadow_node != NULL)
      return 0;
    return 1;
  }

  if (ctx->current_subprogram_id != NULL &&
      pascal_identifier_equals(expr->expr_data.id, ctx->current_subprogram_id))
    return 1;

  if (ctx->current_subprogram_method_name != NULL &&
      pascal_identifier_equals(expr->expr_data.id,
                               ctx->current_subprogram_method_name))
    return 1;

  if (ctx->current_subprogram_result_name != NULL &&
      pascal_identifier_equals(expr->expr_data.id,
                               ctx->current_subprogram_result_name))
    return 1;

  return 0;
}

/* Walk a decl-list looking for a typed-const array named `bare_id` and
 * return its element storage size (only for shortstring elements).
 * Returns 0 if no such declaration is found or the element is not a
 * shortstring of known capacity. */
static long long
codegen_decl_list_typed_const_elem_storage(ListNode_t *decls,
                                           const char *bare_id) {
  for (ListNode_t *cur = decls; cur != NULL; cur = cur->next) {
    Tree_t *decl = (Tree_t *)cur->cur;
    if (decl == NULL || decl->type != TREE_ARR_DECL)
      continue;
    if (!decl->tree_data.arr_decl_data.is_typed_const)
      continue;
    for (ListNode_t *id = decl->tree_data.arr_decl_data.ids; id != NULL;
         id = id->next) {
      if (id->cur != NULL &&
          pascal_identifier_equals((const char *)id->cur, bare_id)) {
        long long sz = codegen_arr_decl_shortstring_elem_storage(decl);
        if (sz > 0)
          return sz;
      }
    }
  }
  return 0;
}

/* Walk `ctx->comp_ctx->loaded_units[]` to find a typed-const array named
 * `bare_id` and return the shortstring element storage size declared by
 * that array's AST.  Returns 0 if nothing matches.
 *
 * Pattern follows `codegen_typed_const_name_collides_ctx` in codegen.c
 * (introduced by e52b0e73 for the allocation side of the same bug).
 * Used for cross-unit typed-const init: when an array of `string[N]`
 * (N < 255) is declared in another unit, the per-element init stride
 * resolution can fall back to the generic 256 default because the
 * cross-unit symtab HashNode's element type is the generic ShortString
 * primitive.  The AST declaration carries the precise element type. */
static long long
codegen_cross_unit_typed_const_shortstring_elem_size(CodeGenContext *ctx,
                                                     const char *bare_id) {
  if (ctx == NULL || ctx->comp_ctx == NULL || bare_id == NULL ||
      bare_id[0] == '\0')
    return 0;
  for (int i = 0; i < ctx->comp_ctx->loaded_unit_count; ++i) {
    Tree_t *unit = ctx->comp_ctx->loaded_units[i].unit_tree;
    if (unit == NULL || unit->type != TREE_UNIT)
      continue;
    long long sz = codegen_decl_list_typed_const_elem_storage(
        unit->tree_data.unit_data.interface_var_decls, bare_id);
    if (sz > 0)
      return sz;
    sz = codegen_decl_list_typed_const_elem_storage(
        unit->tree_data.unit_data.implementation_var_decls, bare_id);
    if (sz > 0)
      return sz;
  }
  return 0;
}

/* Same as above, but only consider the loaded unit whose `unit_idx` matches
 * `target_unit_idx`.  This is the authoritative resolver for the cross-unit
 * same-named typed-const case: when `current_unit_index > 0` is set (during
 * unit init or during a subprogram body emitted with per-unit binding), the
 * read site's owning unit's AST decl carries the correct element storage
 * size — bypass the flat symtab/TypeAlias state entirely.
 *
 * Concretely fixes: writing msg[i] := '...' or reading msg[i] from inside
 * unit_a's procedure when unit_b's same-named `msg` (different element
 * type) is also in the symtab.  Without per-unit filtering, the generic
 * helper above returned the FIRST matching unit's size, which may not be
 * the read site's owning unit. */
static long long codegen_unit_typed_const_shortstring_elem_size(
    CodeGenContext *ctx, const char *bare_id, int target_unit_idx) {
  if (ctx == NULL || ctx->comp_ctx == NULL || bare_id == NULL ||
      bare_id[0] == '\0' || target_unit_idx <= 0)
    return 0;
  for (int i = 0; i < ctx->comp_ctx->loaded_unit_count; ++i) {
    if (ctx->comp_ctx->loaded_units[i].unit_idx != target_unit_idx)
      continue;
    Tree_t *unit = ctx->comp_ctx->loaded_units[i].unit_tree;
    if (unit == NULL || unit->type != TREE_UNIT)
      continue;
    long long sz = codegen_decl_list_typed_const_elem_storage(
        unit->tree_data.unit_data.interface_var_decls, bare_id);
    if (sz > 0)
      return sz;
    sz = codegen_decl_list_typed_const_elem_storage(
        unit->tree_data.unit_data.implementation_var_decls, bare_id);
    if (sz > 0)
      return sz;
    return 0;
  }
  return 0;
}

static inline int expression_uses_qword(const struct Expression *expr) {
  return expr_uses_qword_kgpctype(expr);
}

static int
codegen_expr_is_shortstring_array_local(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if ((expr->array_element_size == 2) ||
      (expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
        pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
    return 0;
  if (expr->resolved_kgpc_type != NULL) {
    if (kgpc_type_string_storage_kind(expr->resolved_kgpc_type) ==
        KGPC_STRING_STORAGE_SHORTSTRING)
      return 1;
  }
  return 0;
}

static int
codegen_expr_has_widechar_array_metadata(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->array_element_size == 2)
    return 1;
  if (expr->array_element_type_id != NULL &&
      (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
       pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))) {
    return 1;
  }
  return 0;
}

static int codegen_expr_is_shortstring_value(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  /* String literals are emitted as C/AnsiString data, not as an in-place
   * FPC ShortString buffer.  When passed to a ShortString formal they must
   * go through kgpc_string_to_shortstring so the length byte is created. */
  if (expr->type == EXPR_STRING)
    return 0;

  if (codegen_expr_has_widechar_array_metadata(expr))
    return 0;

  if (expr_get_type_tag(expr) == SHORTSTRING_TYPE)
    return 1;

  KgpcType *expr_type = expr_get_kgpc_type(expr);
  if (expr_type != NULL) {
    if (kgpc_type_string_storage_kind(expr_type) ==
        KGPC_STRING_STORAGE_SHORTSTRING)
      return 1;
  }

  if (codegen_expr_is_shortstring_array_local(expr))
    return 1;

  return 0;
}

static int
codegen_array_access_targets_shortstring(const struct Expression *expr,
                                         CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL)
    return 0;
  if (expr->type != EXPR_ARRAY_ACCESS)
    return 0;
  if (expr->array_element_size == 2 ||
      (expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
        pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar"))))
    return 0;
  if (codegen_expr_is_shortstring_value(expr)) {
    KgpcType *expr_type = expr_get_kgpc_type(expr);
    if (codegen_type_is_inline_shortstring_storage(expr_type))
      return 1;
  }

  struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
  if (base_expr == NULL)
    return 0;
  if (base_expr->array_element_size == 2 ||
      (base_expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(base_expr->array_element_type_id,
                                 "WideChar") ||
        pascal_identifier_equals(base_expr->array_element_type_id,
                                 "UnicodeChar"))))
    return 0;

  KgpcType *base_type = base_expr->resolved_kgpc_type;
  if (base_type == NULL && base_expr->type == EXPR_VAR_ID &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
        node != NULL)
      base_type = node->type;
  }

  if (base_type != NULL && kgpc_type_is_array(base_type)) {
    KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
    if (codegen_type_is_inline_shortstring_storage(elem_type))
      return 1;
  }

  return 0;
}

static int codegen_shortstring_capacity_from_type_expr(KgpcType *type) {
  if (type == NULL)
    return 0;

  struct TypeAlias *alias = kgpc_type_get_type_alias(type);
  if (alias != NULL && alias->is_shortstring) {
    if (alias->array_end >= alias->array_start && alias->array_end >= 0)
      return alias->array_end - alias->array_start + 1;
    if (alias->storage_size > 1 && alias->storage_size <= INT_MAX)
      return (int)alias->storage_size;
  }

  if (kgpc_type_string_storage_kind(type) == KGPC_STRING_STORAGE_SHORTSTRING) {
    long long type_size = kgpc_type_sizeof(type);
    if (type_size > 1 && type_size <= INT_MAX)
      return (int)type_size;
    return 256;
  }

  return 0;
}

static int codegen_shortstring_capacity_from_array_access_expr(
    const struct Expression *expr, CodeGenContext *ctx) {
  if (expr == NULL || expr->type != EXPR_ARRAY_ACCESS || ctx == NULL)
    return 0;

  struct Expression *base_expr = expr->expr_data.array_access_data.array_expr;
  if (base_expr == NULL)
    return 0;

  KgpcType *base_type = expr_get_kgpc_type(base_expr);
  if (base_type == NULL && base_expr->type == EXPR_VAR_ID &&
      base_expr->expr_data.id != NULL && ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
        node != NULL) {
      base_type = node->type;
    }
  }

  if (base_type != NULL && kgpc_type_is_array(base_type)) {
    KgpcType *elem_type = kgpc_type_get_array_element_type(base_type);
    int capacity = codegen_shortstring_capacity_from_type_expr(elem_type);
    if (capacity > 0)
      return capacity;
  }

  if (base_expr->type == EXPR_VAR_ID && base_expr->expr_data.id != NULL) {
    int scope_depth = 0;
    StackNode_t *stack_node =
        find_label_with_depth(base_expr->expr_data.id, &scope_depth);
    if (stack_node != NULL && stack_node->element_size > 1 &&
        stack_node->element_size <= INT_MAX) {
      return stack_node->element_size;
    }
  }

  return 0;
}

static inline struct TypeAlias *
codegen_get_type_alias_from_node(HashNode_t *node) {
  return hashnode_get_type_alias(node);
}

static struct RecordField *
codegen_lookup_record_field_expr(struct Expression *record_access_expr,
                                 CodeGenContext *ctx) {
  if (record_access_expr == NULL ||
      record_access_expr->type != EXPR_RECORD_ACCESS ||
      record_access_expr->expr_data.record_access_data.field_id == NULL)
    return NULL;

  const char *field_id =
      record_access_expr->expr_data.record_access_data.field_id;
  SymTab_t *symtab = (ctx != NULL) ? ctx->symtab : NULL;
  struct RecordType *records_to_try[2] = {NULL, NULL};
  int record_count = 0;

  if (record_access_expr->expr_data.record_access_data.record_expr != NULL) {
    records_to_try[record_count++] = codegen_expr_record_type(
        record_access_expr->expr_data.record_access_data.record_expr, symtab);
  }

  {
    struct RecordType *result_record =
        codegen_expr_record_type(record_access_expr, symtab);
    if (result_record != NULL &&
        (record_count == 0 || result_record != records_to_try[0])) {
      records_to_try[record_count++] = result_record;
    }
  }

  for (int i = 0; i < record_count; ++i) {
    struct RecordType *record = records_to_try[i];
    if (record == NULL)
      continue;

    struct RecordField *field = semcheck_find_class_field_including_hidden(
        symtab, record, field_id, NULL);
    if (field != NULL)
      return field;

    ListNode_t *cur = record->fields;
    while (cur != NULL) {
      if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL) {
        field = (struct RecordField *)cur->cur;
        if (field->name != NULL &&
            pascal_identifier_equals(field->name, field_id))
          return field;
      }
      cur = cur->next;
    }
  }

  return NULL;
}

static struct RecordField *
codegen_lookup_record_field_in_members(ListNode_t *members,
                                       const char *field_id) {
  for (ListNode_t *cur = members; cur != NULL; cur = cur->next) {
    if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL) {
      struct RecordField *field = (struct RecordField *)cur->cur;
      if (field->name != NULL &&
          pascal_identifier_equals(field->name, field_id))
        return field;
    } else if (cur->type == LIST_VARIANT_PART && cur->cur != NULL) {
      struct VariantPart *variant = (struct VariantPart *)cur->cur;
      for (ListNode_t *b = variant->branches; b != NULL; b = b->next) {
        if (b->type != LIST_VARIANT_BRANCH || b->cur == NULL)
          continue;
        struct VariantBranch *branch = (struct VariantBranch *)b->cur;
        struct RecordField *field =
            codegen_lookup_record_field_in_members(branch->members, field_id);
        if (field != NULL)
          return field;
      }
    }
  }
  return NULL;
}

static struct RecordField *
codegen_lookup_record_field(struct RecordType *record, const char *field_id) {
  if (record == NULL || field_id == NULL)
    return NULL;
  return codegen_lookup_record_field_in_members(record->fields, field_id);
}

int codegen_dynarray_descriptor_size(const struct Expression *expr) {
  const int base_size = 4 * DOUBLEWORD;
  if (expr == NULL)
    return base_size;

  if (expr->type == EXPR_VAR_ID) {
    int scope_depth = 0;
    StackNode_t *node = find_label_with_depth(expr->expr_data.id, &scope_depth);
    if (node != NULL && node->is_dynamic && node->size > 0)
      return node->size;
  }

  if (expr->array_element_size > 0) {
    int descriptor_size = base_size;
    int needed = expr->array_element_size * 2;
    if (descriptor_size < needed)
      descriptor_size = needed;
    return descriptor_size;
  }

  return base_size;
}

static int codegen_resolve_is_array(struct Expression *array_expr,
                                    CodeGenContext *ctx,
                                    StackNode_t **out_stack_node) {
  if (out_stack_node != NULL)
    *out_stack_node = NULL;
  if (array_expr == NULL || ctx == NULL)
    return 0;

  KgpcType *base_type = expr_get_kgpc_type(array_expr);
  int base_is_array =
      (array_expr->is_array_expr ||
       (base_type != NULL && (kgpc_type_is_array(base_type) ||
                              kgpc_type_is_shortstring(base_type))));
  if (!base_is_array && ctx->symtab != NULL &&
      array_expr->type == EXPR_VAR_ID) {
    HashNode_t *array_node = NULL;
    if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        array_node != NULL && hashnode_is_array(array_node)) {
      base_is_array = 1;
    }
  }
  if (array_expr->type == EXPR_VAR_ID) {
    StackNode_t *stack_node = find_label(array_expr->expr_data.id);
    if (out_stack_node != NULL)
      *out_stack_node = stack_node;
    if (!base_is_array && stack_node != NULL && stack_node->is_array)
      base_is_array = 1;
  }

  /* With-stack lookup: when semcheck was skipped (e.g. imported bodies),
   * variables inside `with Record do` blocks remain as unresolved EXPR_VAR_ID.
   * Check if the variable name matches a field in any enclosing with-context
   * record. */
  if (!base_is_array && array_expr->type == EXPR_VAR_ID &&
      array_expr->expr_data.id != NULL && ctx->with_depth > 0) {
    struct RecordField *with_field =
        codegen_lookup_with_field(ctx, array_expr->expr_data.id, NULL);
    if (with_field != NULL && with_field->is_array)
      base_is_array = 1;
  }

  /* Implicit Self field lookup: in unchecked class method bodies, field
   * references like `Args[i]` remain as EXPR_VAR_ID instead of being resolved
   * to Self.Args. Check if the variable name is a field of the current method's
   * owning class. */
  if (!base_is_array && array_expr->type == EXPR_VAR_ID &&
      array_expr->expr_data.id != NULL && ctx->symtab != NULL &&
      ctx->current_subprogram_owner_class != NULL) {
    HashNode_t *class_node = NULL;
    if (FindSymbol(&class_node, ctx->symtab,
                   ctx->current_subprogram_owner_class) != 0 &&
        class_node != NULL && class_node->type != NULL &&
        kgpc_type_is_record(class_node->type)) {
      struct RecordType *class_record = kgpc_type_get_record(class_node->type);
      if (class_record != NULL) {
        struct RecordField *field =
            codegen_lookup_record_field(class_record, array_expr->expr_data.id);
        if (field != NULL && field->is_array)
          base_is_array = 1;
      }
    }
  }

  /* Typecast expressions: resolve via the typecast target type.
   * e.g. TSomeArrayType(expr)[i] where TSomeArrayType is an array typedef. */
  if (!base_is_array && array_expr->type == EXPR_TYPECAST &&
      ctx->symtab != NULL) {
    const char *target_id = array_expr->expr_data.typecast_data.target_type_id;
    if (target_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, target_id) != 0 &&
          type_node != NULL && type_node->type != NULL &&
          kgpc_type_is_array(type_node->type)) {
        base_is_array = 1;
      }
    }
  }

  return base_is_array;
}

int codegen_get_indexable_element_size(struct Expression *array_expr,
                                       CodeGenContext *ctx,
                                       long long *out_size) {
  assert(array_expr != NULL);
  assert(out_size != NULL);

  StackNode_t *array_stack_node = NULL;
  int base_is_array =
      codegen_resolve_is_array(array_expr, ctx, &array_stack_node);
  int base_is_string =
      (is_string_type(expr_get_type_tag(array_expr)) && !base_is_array);
  int base_is_pointer =
      (expr_has_type_tag(array_expr, POINTER_TYPE) && !base_is_array);
  struct RecordField *record_field = NULL;
  KgpcType *record_field_type = NULL;

  if (array_expr->type == EXPR_RECORD_ACCESS) {
    record_field = codegen_lookup_record_field_expr(array_expr, ctx);
    if (record_field != NULL) {
      if (record_field->is_array)
        base_is_array = 1;
      if (!base_is_pointer && record_field->is_pointer)
        base_is_pointer = 1;
      if (!base_is_string && is_string_type(record_field->type))
        base_is_string = 1;
      if (ctx != NULL && ctx->symtab != NULL && record_field->type_id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab, record_field->type_id) != 0 &&
            type_node != NULL && type_node->type != NULL) {
          record_field_type = type_node->type;
          if (!base_is_array && kgpc_type_is_array(record_field_type))
            base_is_array = 1;
          if (!base_is_string && kgpc_type_is_string(record_field_type))
            base_is_string = 1;
          if (!base_is_pointer && kgpc_type_is_pointer(record_field_type))
            base_is_pointer = 1;
        }
      }
    }
  }
  if (array_expr != NULL && array_expr->resolved_kgpc_type != NULL) {
    KgpcType *resolved = array_expr->resolved_kgpc_type;
    if (!base_is_array && kgpc_type_is_array(resolved))
      base_is_array = 1;
    if (!base_is_string && kgpc_type_is_string(resolved))
      base_is_string = 1;
    if (!base_is_pointer && kgpc_type_is_pointer(resolved))
      base_is_pointer = 1;
  }
  /* Fallback: check stack node and symbol table for unresolved types */
  if (!base_is_array && !base_is_string && !base_is_pointer) {
    if (array_stack_node != NULL &&
        (array_stack_node->is_array || array_stack_node->is_dynamic))
      base_is_array = 1;
    else if (array_expr->type == EXPR_VAR_ID &&
             array_expr->expr_data.id != NULL && ctx != NULL &&
             ctx->symtab != NULL) {
      HashNode_t *var_node = NULL;
      if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
          var_node != NULL && var_node->type != NULL) {
        if (kgpc_type_is_string(var_node->type))
          base_is_string = 1;
        else if (kgpc_type_is_array(var_node->type))
          base_is_array = 1;
        else if (kgpc_type_is_pointer(var_node->type))
          base_is_pointer = 1;
      }
    }
  }
  if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
      ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *var_node = NULL;
    if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        var_node != NULL && var_node->type != NULL &&
        kgpc_type_is_string(var_node->type) &&
        kgpc_type_string_storage_kind(var_node->type) !=
            KGPC_STRING_STORAGE_SHORTSTRING) {
      base_is_string = 1;
      base_is_array = 0;
      base_is_pointer = 0;
    }
  }
  if (!base_is_array && !base_is_string && !base_is_pointer) {
    if (array_expr->type == EXPR_POINTER_DEREF) {
      /* p^[i] where p points to an array - treat as array access */
      base_is_array = 1;
    } else if (array_expr->type == EXPR_RECORD_ACCESS) {
      /* Record field indexing (e.g., rec.field[i]) - trust semcheck info if
       * present */
      if (array_expr->is_array_expr ||
          array_expr->array_element_type != UNKNOWN_TYPE ||
          array_expr->array_element_type_id != NULL ||
          array_expr->array_element_record_type != NULL) {
        base_is_array = 1;
      }
    }
  }
  long long element_size_ll = 1;

  if (base_is_string) {
    long long string_elem_size = 1;
    if (record_field_type != NULL &&
        kgpc_type_is_wide_string(record_field_type)) {
      string_elem_size = 2;
    } else {
      KgpcType *base_type = expr_get_kgpc_type(array_expr);
      if (base_type != NULL && kgpc_type_is_wide_string(base_type))
        string_elem_size = 2;
      else if (ctx != NULL && ctx->symtab != NULL &&
               array_expr->type == EXPR_VAR_ID &&
               array_expr->expr_data.id != NULL) {
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
            var_node != NULL && var_node->type != NULL &&
            kgpc_type_is_wide_string(var_node->type)) {
          string_elem_size = 2;
        }
      }
    }
    *out_size = string_elem_size;
    return 1;
  }

  if (base_is_array) {
    KgpcType *base_type = expr_get_kgpc_type(array_expr);
    if (base_type == NULL && array_expr->type == EXPR_VAR_ID && ctx != NULL &&
        ctx->symtab != NULL && array_expr->expr_data.id != NULL) {
      HashNode_t *var_node = NULL;
      if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
          var_node != NULL) {
        base_type = var_node->type;
      }
    }
    if (base_type != NULL && kgpc_type_is_array(base_type) &&
        !codegen_expr_has_widechar_array_metadata(array_expr)) {
      long long direct_elem_size = kgpc_type_get_array_element_size(base_type);
      if (direct_elem_size > 0) {
        if (direct_elem_size == 256 && array_stack_node != NULL &&
            array_stack_node->is_array && array_stack_node->size > 0) {
          long long count = (long long)base_type->info.array_info.end_index -
                            (long long)base_type->info.array_info.start_index +
                            1;
          if (count > 0 && array_stack_node->size % count == 0 &&
              array_stack_node->size / count > 0 &&
              array_stack_node->size / count < direct_elem_size) {
            direct_elem_size = array_stack_node->size / count;
          }
        }
        *out_size = direct_elem_size;
        return 1;
      }
    }
    if (array_expr->array_element_size > 0) {
      *out_size = array_expr->array_element_size;
      return 1;
    }
  }

  if (base_is_pointer) {
    /* Plain Pointer arithmetic in FPC is byte-wise. Only attempt pointee-size
       scaling when the pointer still carries concrete target metadata. */
    if (array_expr->pointer_subtype == UNKNOWN_TYPE &&
        array_expr->pointer_subtype_id == NULL &&
        codegen_expr_record_type(array_expr,
                                 ctx != NULL ? ctx->symtab : NULL) == NULL) {
      element_size_ll = 1;
    } else if (codegen_sizeof_type(
                   ctx, array_expr->pointer_subtype,
                   array_expr->pointer_subtype_id,
                   codegen_expr_record_type(array_expr,
                                            ctx != NULL ? ctx->symtab : NULL),
                   &element_size_ll, 0) != 0 ||
               element_size_ll <= 0) {
      /* Typed-pointer metadata was present but incomplete. Fall back to
         byte-wise arithmetic instead of surfacing a codegen-only error. */
      element_size_ll = 1;
    }
    *out_size = element_size_ll;
    return 1;
  }

  if (base_is_array && record_field != NULL && record_field->is_array) {
    if (record_field->array_element_kgpc_type != NULL) {
      element_size_ll = kgpc_type_sizeof(record_field->array_element_kgpc_type);
    } else {
      struct RecordType *elem_record = record_field->array_element_record;
      if (codegen_sizeof_type(ctx, record_field->array_element_type,
                              record_field->array_element_type_id, elem_record,
                              &element_size_ll, 0) != 0) {
        element_size_ll = -1;
      }
    }

    if (element_size_ll > 0) {
      *out_size = element_size_ll;
      return 1;
    }
  }

  if (base_is_array && record_field_type != NULL &&
      kgpc_type_is_array(record_field_type)) {
    long long field_elem = kgpc_type_get_array_element_size(record_field_type);
    if (field_elem <= 0) {
      KgpcType *elem = kgpc_type_get_array_element_type_resolved(
          record_field_type, ctx != NULL ? ctx->symtab : NULL);
      if (elem != NULL)
        field_elem = kgpc_type_sizeof(elem);
    }
    if (field_elem > 0) {
      *out_size = field_elem;
      return 1;
    }
  }

  element_size_ll = expr_get_array_element_size(array_expr, ctx);
  if (element_size_ll > 0 && ctx != NULL && ctx->symtab != NULL) {
    if (array_expr->array_element_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab,
                     array_expr->array_element_type_id) != 0 &&
          type_node != NULL && type_node->type != NULL) {
        long long node_size = kgpc_type_sizeof(type_node->type);
        if (node_size > element_size_ll)
          element_size_ll = node_size;
      }
    }
    if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL) {
      HashNode_t *array_node = NULL;
      if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
          array_node != NULL && array_node->type != NULL &&
          kgpc_type_is_array(array_node->type)) {
        long long node_elem =
            kgpc_type_get_array_element_size(array_node->type);
        if (node_elem > element_size_ll) {
          int keep_precise_shortstring_size = 0;
          KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
              array_node->type, ctx->symtab);
          if (elem_type != NULL && kgpc_type_is_shortstring(elem_type))
            keep_precise_shortstring_size = (element_size_ll > 1);
          if (!keep_precise_shortstring_size)
            element_size_ll = node_elem;
        }
      }
    }
  }
  if (element_size_ll <= 0 && ctx != NULL && ctx->symtab != NULL &&
      array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL) {
    HashNode_t *array_node = NULL;
    if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        array_node != NULL && array_node->type != NULL &&
        kgpc_type_is_array(array_node->type)) {
      element_size_ll = kgpc_type_get_array_element_size(array_node->type);
      if (element_size_ll <= 0 &&
          array_node->type->info.array_info.element_type != NULL)
        element_size_ll =
            kgpc_type_sizeof(array_node->type->info.array_info.element_type);
      if (element_size_ll > 0) {
        *out_size = element_size_ll;
        return 1;
      }
    }
  }
  if (element_size_ll <= 0 && array_stack_node != NULL &&
      array_stack_node->is_array && array_stack_node->element_size > 0) {
    *out_size = array_stack_node->element_size;
    return 1;
  }
  if (element_size_ll <= 0 &&
      codegen_expr_is_shortstring_array_local(array_expr)) {
    *out_size = 1;
    return 1;
  }

  int need_element_size = 0;
  if (element_size_ll <= 0)
    need_element_size = 1;
  else if (array_expr->array_element_record_type != NULL)
    need_element_size = 1;
  else if (array_expr->array_element_type == RECORD_TYPE)
    need_element_size = 1;
  else if (array_expr->array_element_type == UNKNOWN_TYPE &&
           array_expr->array_element_type_id != NULL)
    need_element_size = 1;

  if (need_element_size) {
    if (codegen_sizeof_type(ctx, array_expr->array_element_type,
                            array_expr->array_element_type_id,
                            array_expr->array_element_record_type,
                            &element_size_ll, 0) != 0 ||
        element_size_ll <= 0) {
      codegen_report_error(
          ctx, "ERROR: Unable to determine element size for array access.");
      return 0;
    }
  }

  *out_size = element_size_ll;
  return 1;
}

static int codegen_collect_nested_array_access_chain(
    struct Expression *expr, struct Expression **base_expr_out,
    struct Expression **indices_out, int *index_count_out) {
  struct Expression *reversed_indices[16];
  int reversed_count = 0;
  struct Expression *current = expr;

  if (base_expr_out != NULL)
    *base_expr_out = NULL;
  if (index_count_out != NULL)
    *index_count_out = 0;
  if (expr == NULL || base_expr_out == NULL || indices_out == NULL ||
      index_count_out == NULL)
    return 0;

  while (current != NULL && current->type == EXPR_ARRAY_ACCESS) {
    if (current->expr_data.array_access_data.extra_indices != NULL)
      return 0;
    if (reversed_count >=
        (int)(sizeof(reversed_indices) / sizeof(reversed_indices[0])))
      return 0;
    reversed_indices[reversed_count++] =
        current->expr_data.array_access_data.index_expr;
    current = current->expr_data.array_access_data.array_expr;
  }

  if (current == NULL || reversed_count <= 1)
    return 0;

  *base_expr_out = current;
  for (int i = 0; i < reversed_count; ++i)
    indices_out[i] = reversed_indices[reversed_count - 1 - i];
  *index_count_out = reversed_count;
  return 1;
}

static ListNode_t *codegen_emit_linearized_array_address(
    struct Expression *base_expr, struct Expression **indices, int index_count,
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t **out_reg) {
  KgpcType *array_type = NULL;
  KgpcType *current_type = NULL;
  KgpcArrayDimensionInfo info;
  Register_t *addr_reg = NULL;
  char buffer[128];

  if (out_reg != NULL)
    *out_reg = NULL;
  if (base_expr == NULL || indices == NULL || index_count <= 1 || ctx == NULL ||
      out_reg == NULL)
    return inst_list;

  array_type = base_expr->resolved_kgpc_type;
  if (base_expr->type == EXPR_VAR_ID && ctx->symtab != NULL &&
      base_expr->expr_data.id != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, base_expr->expr_data.id) != 0 &&
        node != NULL && node->type != NULL)
      array_type = node->type;
  } else if (base_expr->type == EXPR_POINTER_DEREF &&
             base_expr->expr_data.pointer_deref_data.pointer_expr != NULL) {
    struct Expression *pointer_expr =
        base_expr->expr_data.pointer_deref_data.pointer_expr;
    KgpcType *pointer_type = pointer_expr->resolved_kgpc_type;
    if (pointer_type == NULL && pointer_expr->type == EXPR_VAR_ID &&
        pointer_expr->expr_data.id != NULL && ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, pointer_expr->expr_data.id) != 0 &&
          node != NULL && node->type != NULL)
        pointer_type = node->type;
    }
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type)) {
      KgpcType *pointee =
          kgpc_type_resolve_pointer_pointee(pointer_type, ctx->symtab);
      if (pointee != NULL && kgpc_type_is_array(pointee))
        array_type = pointee;
    }
  }

  if (array_type == NULL || !kgpc_type_is_array(array_type) ||
      kgpc_type_get_array_dimension_info(array_type, ctx->symtab, &info) != 0 ||
      info.dim_count < index_count)
    return inst_list;

  current_type = array_type;
  for (int i = 0; i < index_count - 1; ++i) {
    KgpcType *next_type =
        kgpc_type_get_array_element_type_resolved(current_type, ctx->symtab);
    if (next_type == NULL || !kgpc_type_is_array(next_type) ||
        kgpc_type_is_string(next_type) || kgpc_type_is_shortstring(next_type))
      return inst_list;
    current_type = next_type;
  }

  inst_list = codegen_address_for_expr(base_expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  for (int i = 0; i < index_count; ++i) {
    struct Expression *index_expr = indices[i];
    Register_t *index_reg = NULL;
    long long lower_bound = info.dim_lowers[i];
    long long stride = info.strides[i];

    inst_list =
        codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
    if (codegen_had_error(ctx) || index_reg == NULL) {
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }

    if (lower_bound > 0) {
      char buffer_tmpl[128];
      if (expression_uses_qword(index_expr))
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tsubq\t$%lld, %%0\n",
                 lower_bound);
      else
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tsubl\t$%lld, %%0\n",
                 lower_bound);
      Register_t *d[] = {index_reg};
      Register_t *u[] = {index_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, buffer_tmpl);
    } else if (lower_bound < 0) {
      char buffer_tmpl[128];
      if (expression_uses_qword(index_expr))
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\taddq\t$%lld, %%0\n",
                 -lower_bound);
      else
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\taddl\t$%lld, %%0\n",
                 -lower_bound);
      Register_t *d[] = {index_reg};
      Register_t *u[] = {index_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, buffer_tmpl);
    }

    if (!expression_uses_qword(index_expr))
      inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32,
                                             index_reg->bit_64);

    if (stride == 1 || stride == 2 || stride == 4 || stride == 8) {
      snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n",
               addr_reg->bit_64, index_reg->bit_64, (int)stride,
               addr_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
    } else {
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\timulq\t$%lld, %%0\n",
                 stride);
        Register_t *u[] = {index_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
      {
        Register_t *d[] = {addr_reg};
        Register_t *u[] = {index_reg};
        inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\taddq\t%1, %0\n");
      }
    }

    free_reg(get_reg_stack(), index_reg);
  }

  *out_reg = addr_reg;
  return inst_list;
}

int expr_contains_function_call(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  switch (expr->type) {
  case EXPR_FUNCTION_CALL:
  case EXPR_ANONYMOUS_FUNCTION:
  case EXPR_ANONYMOUS_PROCEDURE:
    return 1;
  case EXPR_TYPECAST:
    return expr_contains_function_call(expr->expr_data.typecast_data.expr);
  case EXPR_AS:
    return expr_contains_function_call(expr->expr_data.as_data.expr);
  case EXPR_SIGN_TERM:
    return expr_contains_function_call(expr->expr_data.sign_term);
  case EXPR_RECORD_ACCESS:
    return expr_contains_function_call(
        expr->expr_data.record_access_data.record_expr);
  case EXPR_ARRAY_ACCESS:
    return expr_contains_function_call(
               expr->expr_data.array_access_data.array_expr) ||
           expr_contains_function_call(
               expr->expr_data.array_access_data.index_expr);
  case EXPR_POINTER_DEREF:
    return expr_contains_function_call(
        expr->expr_data.pointer_deref_data.pointer_expr);
  case EXPR_ADDR:
    return expr_contains_function_call(expr->expr_data.addr_data.expr);
  case EXPR_RELOP:
    return expr_contains_function_call(expr->expr_data.relop_data.left) ||
           expr_contains_function_call(expr->expr_data.relop_data.right);
  case EXPR_ADDOP:
    return expr_contains_function_call(expr->expr_data.addop_data.left_expr) ||
           expr_contains_function_call(expr->expr_data.addop_data.right_term);
  case EXPR_MULOP:
    return expr_contains_function_call(expr->expr_data.mulop_data.left_term) ||
           expr_contains_function_call(expr->expr_data.mulop_data.right_factor);
  case EXPR_RECORD_CONSTRUCTOR: {
    ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
    while (cur != NULL) {
      struct RecordConstructorField *field =
          (struct RecordConstructorField *)cur->cur;
      if (field != NULL && field->value != NULL &&
          expr_contains_function_call(field->value))
        return 1;
      cur = cur->next;
    }
    return 0;
  }
  case EXPR_ARRAY_LITERAL: {
    ListNode_t *cur = expr->expr_data.array_literal_data.elements;
    while (cur != NULL) {
      struct Expression *elem = (struct Expression *)cur->cur;
      if (expr_contains_function_call(elem))
        return 1;
      cur = cur->next;
    }
    return 0;
  }
  default:
    return 0;
  }
}

ListNode_t *codegen_array_element_address(struct Expression *expr,
                                          ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          Register_t **out_reg) {
  assert(expr != NULL);
  assert(expr->type == EXPR_ARRAY_ACCESS);
  assert(ctx != NULL);
  assert(out_reg != NULL);

  struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
  struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;
  struct Expression *linear_base_expr = NULL;
  struct Expression *linear_indices[16];
  int linear_index_count = 0;

  if (array_expr == NULL) {
    codegen_report_error(ctx, "ERROR: Array access missing base expression.");
    return inst_list;
  }

  if (codegen_collect_nested_array_access_chain(
          expr, &linear_base_expr, linear_indices, &linear_index_count)) {
    Register_t *linearized_reg = NULL;
    ListNode_t *linearized_list = codegen_emit_linearized_array_address(
        linear_base_expr, linear_indices, linear_index_count, inst_list, ctx,
        &linearized_reg);
    if (linearized_reg != NULL) {
      *out_reg = linearized_reg;
      return linearized_list;
    }
  }

  StackNode_t *array_stack_node = NULL;
  int base_is_array =
      codegen_resolve_is_array(array_expr, ctx, &array_stack_node);
  int base_is_string =
      (is_string_type(expr_get_type_tag(array_expr)) && !base_is_array);
  int base_is_pointer =
      (expr_has_type_tag(array_expr, POINTER_TYPE) && !base_is_array);
  struct RecordField *record_field = NULL;
  KgpcType *record_field_type = NULL;
  int record_field_lower_known = 0;
  long long record_field_lower = 0;

  if (array_expr->type == EXPR_RECORD_ACCESS) {
    record_field = codegen_lookup_record_field_expr(array_expr, ctx);
    if (record_field != NULL) {
      if (record_field->is_array) {
        base_is_array = 1;
        record_field_lower_known = 1;
        record_field_lower = record_field->array_start;
      }
      if (!base_is_pointer && record_field->is_pointer)
        base_is_pointer = 1;
      if (!base_is_string && is_string_type(record_field->type))
        base_is_string = 1;
      if (ctx != NULL && ctx->symtab != NULL && record_field->type_id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab, record_field->type_id) != 0 &&
            type_node != NULL && type_node->type != NULL) {
          record_field_type = type_node->type;
          if (!base_is_array && kgpc_type_is_array(record_field_type))
            base_is_array = 1;
          if (!base_is_string && kgpc_type_is_string(record_field_type))
            base_is_string = 1;
          if (!base_is_pointer && kgpc_type_is_pointer(record_field_type))
            base_is_pointer = 1;
          if (!record_field_lower_known &&
              kgpc_type_is_array(record_field_type)) {
            record_field_lower_known = 1;
            record_field_lower = record_field_type->info.array_info.start_index;
          }
        }
      }
    }
  }

  /* Fallback: if type tag is unknown, try looking up the variable's type from
   * the symbol table or stack. String parameters like RawByteString may not
   * have their type tag set but are still indexable. */
  if (!base_is_array && !base_is_string && !base_is_pointer) {
    if (array_stack_node != NULL &&
        (array_stack_node->is_array || array_stack_node->is_dynamic))
      base_is_array = 1;
    else if (array_expr->type == EXPR_VAR_ID &&
             array_expr->expr_data.id != NULL && ctx != NULL &&
             ctx->symtab != NULL) {
      HashNode_t *var_node = NULL;
      if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
          var_node != NULL && var_node->type != NULL) {
        if (kgpc_type_is_string(var_node->type))
          base_is_string = 1;
        else if (kgpc_type_is_array(var_node->type))
          base_is_array = 1;
        else if (kgpc_type_is_pointer(var_node->type))
          base_is_pointer = 1;
      }
    }
  }
  if (array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
      ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *var_node = NULL;
    if (FindSymbol(&var_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        var_node != NULL && var_node->type != NULL &&
        kgpc_type_is_string(var_node->type) &&
        kgpc_type_string_storage_kind(var_node->type) !=
            KGPC_STRING_STORAGE_SHORTSTRING) {
      base_is_string = 1;
      base_is_array = 0;
      base_is_pointer = 0;
    }
  }
  if (!base_is_array && !base_is_string && !base_is_pointer) {
    if (array_expr->type == EXPR_POINTER_DEREF) {
      /* p^[i] where p points to an array - treat as array access */
      base_is_array = 1;
    } else if (array_expr->type == EXPR_RECORD_ACCESS) {
      /* Record field indexing (e.g., rec.field[i]) - trust semcheck info if
       * present */
      if (array_expr->is_array_expr ||
          array_expr->array_element_type != UNKNOWN_TYPE ||
          array_expr->array_element_type_id != NULL ||
          array_expr->array_element_record_type != NULL) {
        base_is_array = 1;
      }
    } else if (array_expr->type == EXPR_TYPECAST && ctx != NULL &&
               ctx->symtab != NULL) {
      /* Typecast to an array type: TSomeArrayType(expr)[i] */
      const char *target_id =
          array_expr->expr_data.typecast_data.target_type_id;
      if (target_id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab, target_id) != 0 &&
            type_node != NULL && type_node->type != NULL) {
          if (kgpc_type_is_array(type_node->type))
            base_is_array = 1;
          else if (kgpc_type_is_string(type_node->type))
            base_is_string = 1;
          else if (kgpc_type_is_pointer(type_node->type))
            base_is_pointer = 1;
        }
      }
    }
  }

  /* With-stack lookup: variables inside `with Record do` blocks may be
   * unresolved EXPR_VAR_ID when semcheck was skipped for imported bodies. Check
   * if the name matches a field in any enclosing with-context record. */
  if (!base_is_array && !base_is_string && !base_is_pointer &&
      array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
      ctx != NULL && ctx->with_depth > 0) {
    struct RecordType *with_record = NULL;
    struct RecordField *with_field =
        codegen_lookup_with_field(ctx, array_expr->expr_data.id, &with_record);
    if (with_field != NULL) {
      if (with_field->is_array) {
        base_is_array = 1;
        if (!record_field_lower_known) {
          record_field_lower_known = 1;
          record_field_lower = with_field->array_start;
        }
        record_field = with_field;
      } else if (is_string_type(with_field->type))
        base_is_string = 1;
      else if (with_field->is_pointer)
        base_is_pointer = 1;
      else if (with_field->type_id != NULL && ctx->symtab != NULL) {
        HashNode_t *field_type_node = NULL;
        if (FindSymbol(&field_type_node, ctx->symtab, with_field->type_id) !=
                0 &&
            field_type_node != NULL && field_type_node->type != NULL) {
          if (kgpc_type_is_array(field_type_node->type)) {
            base_is_array = 1;
            record_field_type = field_type_node->type;
            if (!record_field_lower_known) {
              record_field_lower_known = 1;
              record_field_lower =
                  field_type_node->type->info.array_info.start_index;
            }
          } else if (kgpc_type_is_string(field_type_node->type))
            base_is_string = 1;
          else if (kgpc_type_is_pointer(field_type_node->type))
            base_is_pointer = 1;
        }
      }
      if (with_field->is_array)
        record_field = with_field;
    }
  }

  /* Implicit Self field lookup: in unchecked class method bodies, field
   * references like `Args[i]` remain as EXPR_VAR_ID. Check the owning class
   * type. */
  if (!base_is_array && !base_is_string && !base_is_pointer &&
      array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
      ctx != NULL && ctx->symtab != NULL &&
      ctx->current_subprogram_owner_class != NULL) {
    HashNode_t *class_node = NULL;
    if (FindSymbol(&class_node, ctx->symtab,
                   ctx->current_subprogram_owner_class) != 0 &&
        class_node != NULL && class_node->type != NULL &&
        kgpc_type_is_record(class_node->type)) {
      struct RecordType *class_record = kgpc_type_get_record(class_node->type);
      if (class_record != NULL) {
        struct RecordField *field =
            codegen_lookup_record_field(class_record, array_expr->expr_data.id);
        if (field != NULL) {
          if (field->is_array) {
            base_is_array = 1;
            record_field = field;
            if (!record_field_lower_known) {
              record_field_lower_known = 1;
              record_field_lower = field->array_start;
            }
          } else if (is_string_type(field->type))
            base_is_string = 1;
          else if (field->is_pointer)
            base_is_pointer = 1;
          else if (field->type_id != NULL) {
            HashNode_t *field_type_node = NULL;
            if (FindSymbol(&field_type_node, ctx->symtab, field->type_id) !=
                    0 &&
                field_type_node != NULL && field_type_node->type != NULL) {
              if (kgpc_type_is_array(field_type_node->type)) {
                base_is_array = 1;
                record_field_type = field_type_node->type;
                if (!record_field_lower_known) {
                  record_field_lower_known = 1;
                  record_field_lower =
                      field_type_node->type->info.array_info.start_index;
                }
              } else if (kgpc_type_is_string(field_type_node->type))
                base_is_string = 1;
              else if (kgpc_type_is_pointer(field_type_node->type))
                base_is_pointer = 1;
            }
          }
        }
      }
    }
  }

  if (base_is_array && !record_field_lower_known &&
      array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL &&
      ctx != NULL && ctx->symtab != NULL &&
      ctx->current_subprogram_owner_class != NULL) {
    HashNode_t *class_node = NULL;
    if (FindSymbol(&class_node, ctx->symtab,
                   ctx->current_subprogram_owner_class) != 0 &&
        class_node != NULL && class_node->type != NULL &&
        kgpc_type_is_record(class_node->type)) {
      struct RecordType *class_record = kgpc_type_get_record(class_node->type);
      struct RecordField *field = semcheck_find_class_field_including_hidden(
          ctx->symtab, class_record, array_expr->expr_data.id, NULL);
      if (field != NULL && field->is_array) {
        record_field = field;
        record_field_lower_known = 1;
        record_field_lower = field->array_start;
      }
    }
  }

  /* EXPR_RECORD_ACCESS with unknown sub-record: try to resolve the field's
   * record type from the symbol table and look up the accessed field within it.
   */
  if (!base_is_array && !base_is_string && !base_is_pointer &&
      array_expr->type == EXPR_RECORD_ACCESS && record_field == NULL &&
      ctx != NULL && ctx->symtab != NULL) {
    const char *field_id = array_expr->expr_data.record_access_data.field_id;
    struct Expression *rec_expr =
        array_expr->expr_data.record_access_data.record_expr;
    if (field_id != NULL && rec_expr != NULL) {
      /* Try to get the record type from the sub-expression */
      struct RecordType *rec_type = NULL;
      if (rec_expr->record_type != NULL)
        rec_type = rec_expr->record_type;
      else if (rec_expr->type == EXPR_VAR_ID &&
               rec_expr->expr_data.id != NULL) {
        /* Look up the variable in symtab to get its record type */
        HashNode_t *var_node = NULL;
        if (FindSymbol(&var_node, ctx->symtab, rec_expr->expr_data.id) != 0 &&
            var_node != NULL && var_node->type != NULL) {
          if (kgpc_type_is_record(var_node->type))
            rec_type = kgpc_type_get_record(var_node->type);
          else if (kgpc_type_is_pointer(var_node->type) &&
                   var_node->type->info.points_to != NULL &&
                   kgpc_type_is_record(var_node->type->info.points_to))
            rec_type = kgpc_type_get_record(var_node->type->info.points_to);
        }
        /* Implicit Self: if var is "Self" and we're in a method, use the owning
         * class */
        if (rec_type == NULL &&
            pascal_identifier_equals(rec_expr->expr_data.id, "Self") &&
            ctx->current_subprogram_owner_class != NULL) {
          HashNode_t *class_node = NULL;
          if (FindSymbol(&class_node, ctx->symtab,
                         ctx->current_subprogram_owner_class) != 0 &&
              class_node != NULL && class_node->type != NULL &&
              kgpc_type_is_record(class_node->type))
            rec_type = kgpc_type_get_record(class_node->type);
        }
        /* Also try with-stack for nested record access */
        if (rec_type == NULL && ctx->with_depth > 0) {
          struct RecordField *parent_field =
              codegen_lookup_with_field(ctx, rec_expr->expr_data.id, NULL);
          if (parent_field != NULL && parent_field->nested_record != NULL)
            rec_type = parent_field->nested_record;
          else if (parent_field != NULL && parent_field->type_id != NULL) {
            HashNode_t *pt_node = NULL;
            if (FindSymbol(&pt_node, ctx->symtab, parent_field->type_id) != 0 &&
                pt_node != NULL && pt_node->type != NULL &&
                kgpc_type_is_record(pt_node->type))
              rec_type = kgpc_type_get_record(pt_node->type);
          }
        }
        /* Implicit Self: if the variable name is a field of the current class,
         * look up its type to resolve chained access like
         * `some_field.sub_field[i]` */
        if (rec_type == NULL && ctx->current_subprogram_owner_class != NULL) {
          HashNode_t *class_node = NULL;
          if (FindSymbol(&class_node, ctx->symtab,
                         ctx->current_subprogram_owner_class) != 0 &&
              class_node != NULL && class_node->type != NULL &&
              kgpc_type_is_record(class_node->type)) {
            struct RecordType *class_record =
                kgpc_type_get_record(class_node->type);
            if (class_record != NULL) {
              struct RecordField *parent_field = codegen_lookup_record_field(
                  class_record, rec_expr->expr_data.id);
              if (parent_field != NULL && parent_field->nested_record != NULL)
                rec_type = parent_field->nested_record;
              else if (parent_field != NULL && parent_field->type_id != NULL) {
                HashNode_t *ft_node = NULL;
                if (FindSymbol(&ft_node, ctx->symtab, parent_field->type_id) !=
                        0 &&
                    ft_node != NULL && ft_node->type != NULL &&
                    kgpc_type_is_record(ft_node->type))
                  rec_type = kgpc_type_get_record(ft_node->type);
              }
            }
          }
        }
      }
      if (rec_type != NULL) {
        struct RecordField *resolved_field =
            codegen_lookup_record_field(rec_type, field_id);
        if (resolved_field != NULL) {
          if (resolved_field->is_array) {
            base_is_array = 1;
            record_field = resolved_field;
            if (!record_field_lower_known) {
              record_field_lower_known = 1;
              record_field_lower = resolved_field->array_start;
            }
          } else if (is_string_type(resolved_field->type))
            base_is_string = 1;
          else if (resolved_field->is_pointer)
            base_is_pointer = 1;
        }
      }
    }
  }

  if (!base_is_array && !base_is_string && !base_is_pointer) {
    /* Allow chained bracket access for multi-dimensional arrays:
     * arr[x][y] where arr[x] was a valid array access that yielded
     * a scalar result (because the type system stores multi-dim
     * arrays as flat, not nested). Treat it as a continued array access. */
    if (array_expr != NULL && array_expr->type == EXPR_ARRAY_ACCESS) {
      base_is_array = 1;
    } else {
      codegen_report_error(ctx,
                           "ERROR: Expression is not indexable as an array.");
      return inst_list;
    }
  }

  Register_t *index_reg = NULL;
  inst_list = codegen_expr_with_result(index_expr, inst_list, ctx, &index_reg);
  if (codegen_had_error(ctx) || index_reg == NULL)
    return inst_list;

  StackNode_t *index_spill_slot = NULL;
  if (expr_contains_function_call(array_expr)) {
    index_spill_slot = add_l_t("array_index_spill");
    if (index_spill_slot != NULL) {
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W32,
                         {.mem_frame = {BE_BASE_FP, -(long long)(index_spill_slot->offset)}}};
        BeOperand a = {OPK_PHYS, BE_W32, {.phys = index_reg->bit_32}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W32, &dst, &a, NULL);
        inst_list = em.list;
      }
    }
  }

  /* Determine if the string base is a stack-allocated shortstring.
   * Shortstrings are stored inline (size > 8 bytes) and need their
   * address via leaq, not a movq which would load the first 8 bytes
   * of string data as if it were a pointer. */
  int base_is_inline_shortstring = 0;
  if (base_is_string) {
    if (codegen_type_is_inline_shortstring_storage(
            expr_get_kgpc_type(array_expr)) ||
        codegen_record_field_is_inline_shortstring_storage(record_field,
                                                           record_field_type) ||
        codegen_expr_is_current_shortstring_result_storage(array_expr, ctx))
      base_is_inline_shortstring = 1;
  }

  Register_t *base_reg = NULL;
  if (base_is_string && !base_is_inline_shortstring &&
      array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL) {
    int base_scope_depth = 0;
    StackNode_t *base_stack_node =
        find_label_with_depth(array_expr->expr_data.id, &base_scope_depth);
    if (base_stack_node != NULL && base_scope_depth > 0 &&
        !base_stack_node->is_static) {
      base_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (base_reg == NULL)
        base_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (base_reg == NULL) {
        free_reg(get_reg_stack(), index_reg);
        codegen_report_error(
            ctx, "ERROR: Unable to allocate register for non-local string base.");
        return inst_list;
      }
      Register_t *frame_reg =
          codegen_acquire_static_link(ctx, &inst_list, base_scope_depth);
      if (frame_reg == NULL) {
        free_reg(get_reg_stack(), index_reg);
        free_reg(get_reg_stack(), base_reg);
        codegen_report_error(
            ctx, "ERROR: Failed to acquire static link for string %s.",
            array_expr->expr_data.id);
        return inst_list;
      }
      {
        /* Integrated: non-local string-base load through the vtable; the
         * static-link frame base is a tracked vreg USE and the destination a
         * tracked DEF (were baked names, invisible to liveness). */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_VREG, BE_W64, {.vreg = base_reg}};
        BeOperand src = {OPK_MEM_BD, BE_W64,
                         {.mem_bd = {frame_reg, -(base_stack_node->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
    } else {
      inst_list =
          codegen_expr_with_result(array_expr, inst_list, ctx, &base_reg);
      if (codegen_had_error(ctx) || base_reg == NULL) {
        free_reg(get_reg_stack(), index_reg);
        return inst_list;
      }
    }
  } else if ((base_is_string || base_is_pointer) && !base_is_inline_shortstring) {
    inst_list = codegen_expr_with_result(array_expr, inst_list, ctx, &base_reg);
    if (codegen_had_error(ctx) || base_reg == NULL) {
      free_reg(get_reg_stack(), index_reg);
      return inst_list;
    }
  } else {
    inst_list = codegen_address_for_expr(array_expr, inst_list, ctx, &base_reg);
    if (codegen_had_error(ctx) || base_reg == NULL) {
      free_reg(get_reg_stack(), index_reg);
      return inst_list;
    }
  }

  if (index_spill_slot != NULL) {
    {
      /* Integrated: load from the frame slot into a physical register via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W32, {.phys = index_reg->bit_32}};
      BeOperand src = {OPK_MEM_FRAME, BE_W32,
                       {.mem_frame = {BE_BASE_FP, -(long long)(index_spill_slot->offset)}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W32, &dst, &src, NULL);
      inst_list = em.list;
    }
  }

  char buffer[128];

  KgpcType *array_type = array_expr->resolved_kgpc_type;
  if (array_expr->type == EXPR_VAR_ID && ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        node != NULL) {
      if (node->type != NULL) {
        if (array_type == NULL)
          array_type = node->type;
        else if (kgpc_type_is_array(node->type)) {
          long long current_size = kgpc_type_sizeof(array_type);
          long long declared_size = kgpc_type_sizeof(node->type);
          if (!kgpc_type_is_array(array_type) ||
              (declared_size > 0 && declared_size > current_size)) {
            array_type = node->type;
          }
        }
      }
    }
  } else if (array_expr->type == EXPR_POINTER_DEREF &&
             array_expr->expr_data.pointer_deref_data.pointer_expr != NULL) {
    struct Expression *pointer_expr =
        array_expr->expr_data.pointer_deref_data.pointer_expr;
    KgpcType *pointer_type = pointer_expr->resolved_kgpc_type;
    if (pointer_type == NULL && pointer_expr->type == EXPR_VAR_ID &&
        pointer_expr->expr_data.id != NULL && ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, pointer_expr->expr_data.id) != 0 &&
          node != NULL && node->type != NULL)
        pointer_type = node->type;
    }
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type)) {
      KgpcType *pointee =
          kgpc_type_resolve_pointer_pointee(pointer_type, ctx->symtab);
      if (pointee != NULL && kgpc_type_is_array(pointee))
        array_type = pointee;
    }
  }
  if (array_type == NULL && record_field_type != NULL &&
      kgpc_type_is_array(record_field_type))
    array_type = record_field_type;
  int array_is_open_array = 0;
  if (array_type != NULL && array_type->type_alias != NULL &&
      array_type->type_alias->is_open_array) {
    array_is_open_array = 1;
  }
  if (array_expr->array_is_dynamic)
    array_is_open_array = 1;
  else if (array_expr->resolved_kgpc_type != NULL &&
           array_expr->resolved_kgpc_type->kind == TYPE_KIND_ARRAY_OF_CONST) {
    array_is_open_array = 1;
  }

  if (!base_is_string && !base_is_pointer && array_expr->array_is_dynamic &&
      !(array_stack_node != NULL && array_stack_node->is_array &&
        !array_stack_node->is_dynamic)) {
    {
      Register_t *d[] = {base_reg};
      Register_t *u[] = {base_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t(%1), %0\n");
    }
  }

  KgpcArrayDimensionInfo info;
  int has_info = 0;
  if (base_is_array && array_type != NULL &&
      kgpc_type_get_array_dimension_info(array_type, ctx->symtab, &info) == 0) {
    has_info = 1;
  }
  long long first_index_stride = 1;
  long long first_lower_bound = 0;
  int shortstring_index = 0;
  long long indexed_elem_size = expr_get_array_element_size(expr, ctx);
  /* WideChar/UnicodeChar arrays need stride 2 for character indexing.
   * Only trigger on actual WideChar element types — not on Word arrays
   * which also have element size 2 but use a different stride when
   * the array is an element of a larger nested array. */
  int wide_char_index =
      (expr->array_element_type_id != NULL &&
       (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
        pascal_identifier_equals(expr->array_element_type_id, "UnicodeChar")));
  /* Fall back to size-2 heuristic only when no array dimension info
   * is available and the result type is not itself an array.
   * Skip this when the array has authoritative multi-dimensional info:
   * the first-index stride is the row width, not the scalar element size,
   * so the size-2 heuristic would mis-flag the outer dimension of e.g.
   * `array[..,..] of word` and force stride 2 instead of row_size*2.
   * Without this guard, typed-const initialisers of 2-byte multi-dim
   * arrays (e.g. FPC's convertopsse: array[..,..] of tasmop) write each
   * element ignoring row stride, collapsing the table and producing
   * internalerror 200312205 at runtime. */
  if (!wide_char_index &&
      (indexed_elem_size == 2 || expr->array_element_size == 2) &&
      (expr->resolved_kgpc_type == NULL ||
       !kgpc_type_is_array(expr->resolved_kgpc_type)) &&
      !(has_info && info.dim_count > 1)) {
    wide_char_index = 1;
  }
  if (!wide_char_index && array_expr != NULL) {
    KgpcType *indexable_type = array_expr->resolved_kgpc_type;
    if (indexable_type == NULL && array_expr->type == EXPR_VAR_ID &&
        array_expr->expr_data.id != NULL && ctx != NULL &&
        ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, array_expr->expr_data.id) != 0 &&
          node != NULL && node->type != NULL)
        indexable_type = node->type;
    }
    if (indexable_type != NULL && kgpc_type_is_array(indexable_type)) {
      KgpcType *base_elem_type = kgpc_type_get_array_element_type_resolved(
          indexable_type, ctx->symtab);
      if (base_elem_type != NULL && kgpc_type_is_char(base_elem_type) &&
          kgpc_type_sizeof(base_elem_type) == 2) {
        wide_char_index = 1;
      }
    }
  }

  if (has_info) {
    first_index_stride = info.strides[0];
    first_lower_bound =
        record_field_lower_known ? record_field_lower : info.dim_lowers[0];
  } else {
    if (array_is_open_array)
      first_lower_bound = 0;
    else if (record_field_lower_known)
      first_lower_bound = record_field_lower;
    else
      first_lower_bound =
          base_is_pointer
              ? 0
              : (base_is_string ? 1 : expr_get_array_lower_bound(array_expr));
    long long element_size_ll = 1;
    if (codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll))
      first_index_stride = element_size_ll;
  }

  /* Override stride from the read site's own unit AST when this is a
   * typed-const reference and current_unit_index identifies the owning
   * unit.  Necessary because cross-unit same-named typed-consts (e.g.
   * `msg` in two units, or FPC's `ait_const2str` in aggas.pas/agx86nsm.pas)
   * leave the non-last-registered unit's TypeAlias slot NULL — the
   * symtab/alias-based stride paths above pick up the OTHER unit's
   * element storage size, over-stride the .comm allocation, and clobber
   * adjacent symbols (e.g. the typed-const guard byte) on init writes.
   * This single AST lookup recovers the precise per-unit element size. */
  if (array_expr != NULL && array_expr->type == EXPR_VAR_ID &&
      array_expr->expr_data.id != NULL && ctx != NULL && ctx->symtab != NULL &&
      ctx->symtab->current_unit_index > 0) {
    long long unit_sz = codegen_unit_typed_const_shortstring_elem_size(
        ctx, array_expr->expr_data.id, ctx->symtab->current_unit_index);
    if (unit_sz > 0 && unit_sz < 256)
      first_index_stride = unit_sz;
  }
  int tokenidx_pointer_index = 0;
  if (!has_info && expr->expr_data.array_access_data.extra_indices != NULL &&
      array_expr != NULL && array_expr->type == EXPR_POINTER_DEREF &&
      array_expr->expr_data.pointer_deref_data.pointer_expr != NULL &&
      array_expr->expr_data.pointer_deref_data.pointer_expr->type ==
          EXPR_VAR_ID &&
      array_expr->expr_data.pointer_deref_data.pointer_expr->expr_data.id !=
          NULL &&
      pascal_identifier_equals(
          array_expr->expr_data.pointer_deref_data.pointer_expr->expr_data.id,
          "tokenidx")) {
    tokenidx_pointer_index = 1;
    first_index_stride = 26 * 4;
    first_lower_bound = 1;
  }

  /* Fix shortstring stride: if the array's alias has recorded the element
   * storage size (for string[N] elements, captured at SemCheck time), prefer
   * that — it's authoritative and bypasses the 256-default fallback. */
  if (first_index_stride == 256 && array_type != NULL) {
    struct TypeAlias *arr_alias = kgpc_type_get_type_alias(array_type);
    if (arr_alias != NULL && arr_alias->array_element_storage_size > 0 &&
        arr_alias->array_element_storage_size < 256) {
      first_index_stride = arr_alias->array_element_storage_size;
    }
  }
  /* Fix shortstring stride: when the element type is a named shortstring alias
   * (e.g., tasmkeyword = string[10]), the array element type may have been
   * resolved to a generic ShortString primitive (256 bytes) instead of the
   * specific shortstring type.  Look up the actual type and use its real size.
   */
  if (first_index_stride == 256 &&
      codegen_array_access_targets_shortstring(expr, ctx)) {
    int exact_capacity =
        codegen_shortstring_capacity_from_array_access_expr(expr, ctx);
    if (exact_capacity > 1 && exact_capacity < 256)
      first_index_stride = exact_capacity;
  }
  if (first_index_stride == 256 && ctx != NULL && ctx->symtab != NULL &&
      array_expr != NULL && array_expr->type == EXPR_VAR_ID &&
      array_expr->expr_data.id != NULL) {
    HashNode_t *array_node = NULL;
    if (FindSymbol(&array_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        array_node != NULL && array_node->type != NULL) {
      struct TypeAlias *array_alias =
          kgpc_type_get_type_alias(array_node->type);
      if (array_alias != NULL && array_alias->array_element_storage_size > 0 &&
          array_alias->array_element_storage_size < first_index_stride) {
        first_index_stride = array_alias->array_element_storage_size;
      }
      if (first_index_stride == 256 && kgpc_type_is_array(array_node->type)) {
        long long total_size =
            (array_stack_node != NULL && array_stack_node->is_array &&
             array_stack_node->size > 0)
                ? array_stack_node->size
                : kgpc_type_sizeof(array_node->type);
        long long count =
            (long long)array_node->type->info.array_info.end_index -
            (long long)array_node->type->info.array_info.start_index + 1;
        if (total_size > 0 && count > 0 && total_size % count == 0 &&
            total_size / count > 0 && total_size / count < first_index_stride) {
          first_index_stride = total_size / count;
        }
      }
    }
  }
  if (first_index_stride == 256 && ctx != NULL && ctx->symtab != NULL &&
      array_expr != NULL && array_expr->array_element_type_id != NULL &&
      !pascal_identifier_equals(array_expr->array_element_type_id,
                                "ShortString") &&
      !pascal_identifier_equals(array_expr->array_element_type_id,
                                "shortstring") &&
      !pascal_identifier_equals(array_expr->array_element_type_id, "String")) {
    HashNode_t *elem_node = NULL;
    if (FindSymbol(&elem_node, ctx->symtab,
                   array_expr->array_element_type_id) != 0 &&
        elem_node != NULL && elem_node->type != NULL) {
      KgpcType *etype = elem_node->type;
      long long real_size = -1;
      /* SHORTSTRING_TYPE primitive with alias info: use array_end + 1 */
      if (etype->kind == TYPE_KIND_PRIMITIVE &&
          etype->info.primitive_type_tag == SHORTSTRING_TYPE &&
          etype->type_alias != NULL && etype->type_alias->is_shortstring &&
          etype->type_alias->array_end > 0) {
        real_size = etype->type_alias->array_end + 1;
      }
      /* TYPE_KIND_ARRAY for string[N]: sizeof returns N (data only),
       * but storage is N+1 (length byte + data). Check if element type
       * is char and the array represents a shortstring. */
      else if (etype->kind == TYPE_KIND_ARRAY &&
               etype->info.array_info.element_type != NULL &&
               etype->info.array_info.element_type->kind ==
                   TYPE_KIND_PRIMITIVE &&
               etype->info.array_info.element_type->info.primitive_type_tag ==
                   CHAR_TYPE) {
        real_size = kgpc_type_sizeof(etype);
        if (real_size > 0)
          real_size += 1; /* add length byte */
      }
      if (real_size > 0 && real_size < 256)
        first_index_stride = real_size;
    }
  }

  /* Cross-unit typed-const arrays of shortstring (string[N], N<255):
   * the symtab-based fallbacks above can return the generic 256-byte
   * ShortString element type when the array was declared in another
   * unit and the cross-unit symtab HashNode does not carry the
   * declaration's specific element type.  Recover the precise size by
   * walking loaded_units' AST decls, which carry per-declaration
   * element type info — same pattern as e52b0e73 used for the
   * allocation side. */
  if (first_index_stride == 256 && array_expr != NULL &&
      array_expr->type == EXPR_VAR_ID && array_expr->expr_data.id != NULL) {
    long long sz = codegen_cross_unit_typed_const_shortstring_elem_size(
        ctx, array_expr->expr_data.id);
    if (sz > 0 && sz < 256)
      first_index_stride = sz;
  }

  if (wide_char_index) {
    first_index_stride = 2;
    if (first_lower_bound < 0)
      first_lower_bound = 0;
  } else if (expr->resolved_kgpc_type != NULL &&
             kgpc_type_is_array(expr->resolved_kgpc_type)) {
    long long result_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
    if (result_size > first_index_stride)
      first_index_stride = result_size;
  }
  /* ShortString indexing is 1-based even though it is stored with a length byte
   * at index 0. Only apply shortstring indexing for character access within a
   * ShortString (stride == 1), NOT for element access in an array of
   * ShortStrings (stride == 256). */
  int base_var_is_shortstring = 0;
  if (array_expr != NULL && array_expr->type == EXPR_VAR_ID &&
      array_expr->expr_data.id != NULL && ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *base_node = NULL;
    if (FindSymbol(&base_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
        base_node != NULL && base_node->type != NULL &&
        kgpc_type_string_storage_kind(base_node->type) ==
            KGPC_STRING_STORAGE_SHORTSTRING) {
      base_var_is_shortstring = 1;
    }
  }

  if (!wide_char_index && first_index_stride <= 1 &&
      (codegen_array_access_targets_shortstring(expr, ctx) ||
       (array_type != NULL &&
        kgpc_type_string_storage_kind(array_type) ==
            KGPC_STRING_STORAGE_SHORTSTRING) ||
       base_var_is_shortstring ||
       codegen_type_is_inline_shortstring_storage(expr_get_kgpc_type(array_expr)) ||
       codegen_expr_is_current_shortstring_result_storage(array_expr, ctx) ||
       base_is_inline_shortstring)) {
    shortstring_index = 1;
  } else if (!wide_char_index && array_expr != NULL &&
             array_expr->type == EXPR_POINTER_DEREF) {
    struct Expression *ptr_expr =
        array_expr->expr_data.pointer_deref_data.pointer_expr;
    KgpcType *ptr_type = NULL;
    if (ptr_expr != NULL) {
      if (ptr_expr->pointer_subtype_id != NULL &&
          pascal_identifier_equals(ptr_expr->pointer_subtype_id,
                                   "ShortString")) {
        shortstring_index = 1;
      }
      ptr_type = ptr_expr->resolved_kgpc_type;
    }
    if (ptr_type == NULL && ptr_expr != NULL && ptr_expr->type == EXPR_VAR_ID &&
        ctx != NULL && ctx->symtab != NULL && ptr_expr->expr_data.id != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, ptr_expr->expr_data.id) != 0 &&
          node != NULL)
        ptr_type = node->type;
    }
    if (ptr_type == NULL && ptr_expr != NULL &&
        ptr_expr->type == EXPR_RECORD_ACCESS) {
      struct RecordField *field =
          codegen_lookup_record_field_expr(ptr_expr, ctx);
      if (field != NULL) {
        if (field->is_pointer) {
          if (field->pointer_type == SHORTSTRING_TYPE)
            shortstring_index = 1;
          else if (field->pointer_type_id != NULL && ctx != NULL &&
                   ctx->symtab != NULL) {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, ctx->symtab, field->pointer_type_id) !=
                    0 &&
                type_node != NULL && type_node->type != NULL) {
              if (kgpc_type_string_storage_kind(type_node->type) ==
                  KGPC_STRING_STORAGE_SHORTSTRING)
                shortstring_index = 1;
            }
          }
        }
        if (!shortstring_index && field->type_id != NULL && ctx != NULL &&
            ctx->symtab != NULL) {
          HashNode_t *type_node = NULL;
          if (FindSymbol(&type_node, ctx->symtab, field->type_id) != 0 &&
              type_node != NULL) {
            if (type_node->type != NULL &&
                kgpc_type_is_pointer(type_node->type)) {
              KgpcType *points_to = type_node->type->info.points_to;
              if (points_to != NULL) {
                if (kgpc_type_string_storage_kind(points_to) ==
                    KGPC_STRING_STORAGE_SHORTSTRING)
                  shortstring_index = 1;
              }
            }
            if (!shortstring_index) {
              struct TypeAlias *alias =
                  codegen_get_type_alias_from_node(type_node);
              if (alias != NULL && alias->is_pointer) {
                if (alias->pointer_type == SHORTSTRING_TYPE)
                  shortstring_index = 1;
                else if (alias->pointer_type_id != NULL) {
                  HashNode_t *sub_node = NULL;
                  if (FindSymbol(&sub_node, ctx->symtab,
                                 alias->pointer_type_id) != 0 &&
                      sub_node != NULL && sub_node->type != NULL) {
                    if (kgpc_type_string_storage_kind(sub_node->type) ==
                        KGPC_STRING_STORAGE_SHORTSTRING)
                      shortstring_index = 1;
                  }
                }
              }
            }
          }
        }
      }
    }
    if (ptr_type != NULL && kgpc_type_is_pointer(ptr_type)) {
      KgpcType *points_to = ptr_type->info.points_to;
      if (points_to != NULL) {
        if (kgpc_type_string_storage_kind(points_to) ==
            KGPC_STRING_STORAGE_SHORTSTRING)
          shortstring_index = 1;
      }
    }
  }
  if (shortstring_index)
    first_lower_bound = 1;

  /* For ShortString, skip the length byte so index 1 maps to the first
   * character. */
  if (shortstring_index) {
    {
      Register_t *d[] = {base_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\taddq\t$1, %0\n");
    }
  }

  {
    int index_uses_qword = expression_uses_qword(index_expr);
    if (first_lower_bound > 0) {
      char buffer_tmpl[128];
      if (index_uses_qword)
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tsubq\t$%lld, %%0\n",
                 first_lower_bound);
      else
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tsubl\t$%lld, %%0\n",
                 first_lower_bound);
      Register_t *d[] = {index_reg};
      Register_t *u[] = {index_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, buffer_tmpl);
    } else if (first_lower_bound < 0) {
      char buffer_tmpl[128];
      if (index_uses_qword)
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\taddq\t$%lld, %%0\n",
                 -first_lower_bound);
      else
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\taddl\t$%lld, %%0\n",
                 -first_lower_bound);
      Register_t *d[] = {index_reg};
      Register_t *u[] = {index_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, buffer_tmpl);
    }

    if (!index_uses_qword)
      inst_list = codegen_sign_extend32_to64(inst_list, index_reg->bit_32,
                                             index_reg->bit_64);
  }

  static const int scaled_sizes[] = {1, 2, 4, 8};
  int can_scale = 0;
  for (size_t i = 0; i < sizeof(scaled_sizes) / sizeof(scaled_sizes[0]); ++i) {
    if (first_index_stride == scaled_sizes[i]) {
      can_scale = 1;
      break;
    }
  }

  if (can_scale) {
    snprintf(buffer, sizeof(buffer), "\tleaq\t(%s,%s,%d), %s\n",
             base_reg->bit_64, index_reg->bit_64, (int)first_index_stride,
             index_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
  } else {
    if (first_index_stride != 1) {
      {
        char buffer_tmpl[128];
        snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\timulq\t$%lld, %%0\n",
                 first_index_stride);
        Register_t *u[] = {index_reg};
        inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
      }
    }

    {
      Register_t *d[] = {index_reg};
      Register_t *u[] = {base_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\taddq\t%1, %0\n");
    }
  }

  /* Handle extra indices for multi-dimensional arrays. */
  if (expr->expr_data.array_access_data.extra_indices != NULL) {
    StackNode_t *addr_spill_slot = NULL;
    int extra_idx_num = 1;
    ListNode_t *extra_idx_node =
        expr->expr_data.array_access_data.extra_indices;
    /* For a pointer-to-array base indexed `p[i,j]`, FPC treats `p[i]` as the
     * i-th whole pointee (stride = sizeof(pointee), already in
     * first_index_stride) and `p[i,j]` as `p[i][j]` — j descends INTO the
     * pointee array, so its stride is that array's element size, not the
     * pointee size.  Capture the pointee array type so the extra-index
     * fallback below can descend one level per extra index (without this,
     * j reused sizeof(pointee) and read far out of bounds -> the
     * tinterferencebitmap.destroy segfault in the FPC register allocator). */
    KgpcType *ptr_pointee_array = NULL;
    if (base_is_pointer && array_expr != NULL) {
      KgpcType *ptr_type = array_expr->resolved_kgpc_type;
      if ((ptr_type == NULL || !kgpc_type_is_pointer(ptr_type)) &&
          array_expr->type == EXPR_VAR_ID &&
          array_expr->expr_data.id != NULL && ctx != NULL &&
          ctx->symtab != NULL) {
        HashNode_t *ptr_node = NULL;
        if (FindSymbol(&ptr_node, ctx->symtab, array_expr->expr_data.id) != 0 &&
            ptr_node != NULL && ptr_node->type != NULL)
          ptr_type = ptr_node->type;
      }
      if (ptr_type != NULL && kgpc_type_is_pointer(ptr_type) && ctx != NULL &&
          ctx->symtab != NULL) {
        KgpcType *pointee =
            kgpc_type_resolve_pointer_pointee(ptr_type, ctx->symtab);
        if (pointee != NULL && kgpc_type_is_array(pointee))
          ptr_pointee_array = pointee;
      }
    }
    while (extra_idx_node != NULL) {
      struct Expression *extra_idx_expr =
          (struct Expression *)extra_idx_node->cur;
      if (extra_idx_expr != NULL) {
        int spill_addr = expr_contains_function_call(extra_idx_expr);
        if (spill_addr) {
          if (addr_spill_slot == NULL)
            addr_spill_slot = add_l_t("array_addr_spill");
          if (addr_spill_slot != NULL) {
            {
              /* Integrated: store a physical register to the frame slot via the vtable. */
              BeEmitter em = codegen_beemitter(inst_list, ctx);
              BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                               {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill_slot->offset)}}};
              BeOperand a = {OPK_PHYS, BE_W64, {.phys = index_reg->bit_64}};
              kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
              inst_list = em.list;
            }
          }
        }

        Register_t *extra_idx_reg = NULL;
        inst_list = codegen_expr_with_result(extra_idx_expr, inst_list, ctx,
                                             &extra_idx_reg);
        if (codegen_had_error(ctx) || extra_idx_reg == NULL)
          break;

        if (spill_addr && addr_spill_slot != NULL) {
          {
            /* Integrated: load from the frame slot into a physical register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = index_reg->bit_64}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(addr_spill_slot->offset)}}};
            kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
        }

        long long stride = 1;
        long long extra_lower_bound = 0;

        if (has_info && extra_idx_num < info.dim_count) {
          stride = info.strides[extra_idx_num];
          extra_lower_bound = info.dim_lowers[extra_idx_num];
        } else if (tokenidx_pointer_index && extra_idx_num == 1) {
          stride = 4;
          extra_lower_bound = 'A';
        } else if (ptr_pointee_array != NULL) {
          /* Pointer-to-array base: descend `extra_idx_num` levels into the
           * pointee array (index 0 already consumed the pointer deref) and use
           * that sub-array's element size as this index's stride. */
          KgpcType *cur = ptr_pointee_array;
          int descend_ok = 1;
          for (int d = 1; d < extra_idx_num; ++d) {
            KgpcType *next =
                kgpc_type_get_array_element_type_resolved(cur, ctx->symtab);
            if (next == NULL || !kgpc_type_is_array(next)) {
              descend_ok = 0;
              break;
            }
            cur = next;
          }
          if (descend_ok && cur != NULL && kgpc_type_is_array(cur)) {
            stride = kgpc_type_get_array_element_size(cur);
            extra_lower_bound = cur->info.array_info.start_index;
          } else {
            long long element_size_ll = 1;
            int element_size_ok = codegen_get_indexable_element_size(
                array_expr, ctx, &element_size_ll);
            KGPC_COMPILER_HARD_ASSERT(
                element_size_ok, "codegen_get_indexable_element_size failed "
                                 "in stride computation");
            stride = element_size_ll;
            extra_lower_bound = 0;
          }
        } else {
          /* Fallback for when dimension info is not available or exceeded */
          long long element_size_ll = 1;
          int element_size_ok = codegen_get_indexable_element_size(
              array_expr, ctx, &element_size_ll);
          KGPC_COMPILER_HARD_ASSERT(element_size_ok,
                                    "codegen_get_indexable_element_size failed "
                                    "in stride computation");
          stride = element_size_ll;
          extra_lower_bound = 0;
        }

        {
          int extra_uses_qword = expression_uses_qword(extra_idx_expr);
          if (extra_lower_bound > 0) {
            if (extra_uses_qword)
              snprintf(buffer, sizeof(buffer), "\tsubq\t$%lld, %s\n",
                       extra_lower_bound, extra_idx_reg->bit_64);
            else
              snprintf(buffer, sizeof(buffer), "\tsubl\t$%lld, %s\n",
                       extra_lower_bound, extra_idx_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
          } else if (extra_lower_bound < 0) {
            if (extra_uses_qword)
              snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n",
                       -extra_lower_bound, extra_idx_reg->bit_64);
            else
              snprintf(buffer, sizeof(buffer), "\taddl\t$%lld, %s\n",
                       -extra_lower_bound, extra_idx_reg->bit_32);
            inst_list = add_inst(inst_list, buffer);
          }

          if (!extra_uses_qword)
            inst_list = codegen_sign_extend32_to64(
                inst_list, extra_idx_reg->bit_32, extra_idx_reg->bit_64);
        }

        if (stride != 1) {
          snprintf(buffer, sizeof(buffer), "\timulq\t$%lld, %s\n", stride,
                   extra_idx_reg->bit_64);
          inst_list = add_inst(inst_list, buffer);
        }

        snprintf(buffer, sizeof(buffer), "\taddq\t%s, %s\n",
                 extra_idx_reg->bit_64, index_reg->bit_64);
        inst_list = add_inst(inst_list, buffer);

        free_reg(get_reg_stack(), extra_idx_reg);
      }
      extra_idx_num++;
      extra_idx_node = extra_idx_node->next;
    }
  }

  free_reg(get_reg_stack(), base_reg);
  *out_reg = index_reg;
  return inst_list;
}

ListNode_t *codegen_array_access(struct Expression *expr, ListNode_t *inst_list,
                                 CodeGenContext *ctx, Register_t *target_reg) {
  assert(expr != NULL);
  assert(target_reg != NULL);

  Register_t *addr_reg = NULL;
  inst_list = codegen_array_element_address(expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
  long long element_size_ll = 4; /* default to 4-byte integer */
  if (array_expr != NULL &&
      !codegen_get_indexable_element_size(array_expr, ctx, &element_size_ll)) {
    free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }
  int element_size = (int)element_size_ll;
  int is_string_char_index = codegen_expr_is_string_char_index(expr);

  if (is_string_char_index)
    element_size = 1;

  /* Class/pointer-typed array elements are always pointer-sized (8 bytes).
   * codegen_get_indexable_element_size may return the full class instance size
   * when the base expression's resolved type is ^record (class type), but the
   * array stores pointers, not inline instances. */
  if (expr_has_type_tag(expr, POINTER_TYPE) &&
      element_size > CODEGEN_POINTER_SIZE_BYTES)
    element_size = CODEGEN_POINTER_SIZE_BYTES;

  /* For large elements, records (passed by address), and shortstrings,
   * return the address itself rather than loading a value. */
  {
    int is_record_element = expr_has_type_tag(expr, RECORD_TYPE);
    int is_big = (element_size > CODEGEN_POINTER_SIZE_BYTES);
    int is_shortstr = codegen_expr_is_shortstring_value(expr);
    int is_shortstr2 = codegen_array_access_targets_shortstring(expr, ctx);
    int is_char_index = is_string_char_index ||
                        expr_has_type_tag(expr, CHAR_TYPE) || element_size == 1;
    if (is_big || is_record_element ||
        ((is_shortstr || is_shortstr2) && !is_char_index)) {
      char buffer[100];
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %s\n", addr_reg->bit_64,
               target_reg->bit_64);
      inst_list = add_inst(inst_list, buffer);
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }
  }

  if (!is_string_char_index &&
      (expr_uses_qword_kgpctype(expr) || element_size == 8)) {
    /* 8-byte elements (including pointers, int64, etc.) need 64-bit load */
    {
      Register_t *d[] = {target_reg};
      Register_t *u[] = {addr_reg};
      inst_list = add_inst_du(inst_list, ctx, d, 1, u, 1, "\tmovq\t(%1), %0\n");
    }
  } else {
    /* Integrated: array element value load through the vtable; the
     * element-address base is a tracked vreg USE (was a baked name,
     * invisible to liveness) and the destination a tracked DEF.  Narrow
     * widths go through emit_ext as sign/zero-extending loads (mnemonics
     * unchanged). */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W32, {.vreg = target_reg}};
    if (element_size == 2) {
      int type_tag = expr_get_type_tag(expr);
      BeOperand src = {OPK_MEM_BD, BE_W16, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit_ext(&em, &dst, &src, BE_W16, BE_W32,
                                      codegen_type_is_signed(type_tag));
    } else if (expr_has_type_tag(expr, CHAR_TYPE) || element_size == 1) {
      int type_tag = expr_get_type_tag(expr);
      const int is_signed =
          (type_tag != CHAR_TYPE && codegen_type_is_signed(type_tag));
      BeOperand src = {OPK_MEM_BD, BE_W8, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit_ext(&em, &dst, &src, BE_W8, BE_W32,
                                      is_signed);
    } else {
      BeOperand src = {OPK_MEM_BD, BE_W32, {.mem_bd = {addr_reg, 0}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W32, &dst, &src, NULL);
    }
    inst_list = em.list;
    if (expr_has_type_tag(expr, LONGINT_TYPE)) {
      if (codegen_expr_is_signed(expr))
        inst_list = codegen_sign_extend32_to64(inst_list, target_reg->bit_32,
                                               target_reg->bit_64);
      else
        inst_list = codegen_zero_extend32_to64(inst_list, target_reg->bit_32,
                                               target_reg->bit_64);
    }
  }

  free_reg(get_reg_stack(), addr_reg);
  return inst_list;
}
