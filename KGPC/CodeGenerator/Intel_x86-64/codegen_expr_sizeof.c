#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_Expr_Internal.h"
#include "../../identifier_utils.h"
#include "codegen.h"
#include "codegen_expr_sizeof.h"
#include "codegen_expression.h"

extern const char *kgpc_getenv(const char *name);

int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
                        struct RecordType *record_type, long long *size_out,
                        int depth);
int codegen_sizeof_array_node(CodeGenContext *ctx, HashNode_t *node,
                              long long *size_out, int depth);
int codegen_sizeof_array_type_kgpc(CodeGenContext *ctx, KgpcType *type,
                                   long long *size_out);
int codegen_sizeof_named_array_alias(CodeGenContext *ctx,
                                     const struct TypeAlias *alias,
                                     long long *size_out);
long long codegen_set_storage_size_from_alias(CodeGenContext *ctx,
                                              const struct TypeAlias *alias);
long long codegen_set_storage_size_for_set_alias(const struct TypeAlias *alias);
struct RecordType *codegen_expr_record_type(const struct Expression *expr,
                                            SymTab_t *symtab);

static int codegen_sizeof_variant_part(CodeGenContext *ctx,
                                       struct VariantPart *variant,
                                       long long *size_out, int depth);
static int codegen_get_variant_part_alignment(CodeGenContext *ctx,
                                              struct VariantPart *variant,
                                              int depth);
static int codegen_get_record_members_alignment(CodeGenContext *ctx,
                                                ListNode_t *members, int depth);
static int codegen_sizeof_record_members(CodeGenContext *ctx,
                                         ListNode_t *members,
                                         long long *size_out, int depth);

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

/* Keep these in sync with codegen_expression.c. */
#ifndef CODEGEN_POINTER_SIZE_BYTES
#define CODEGEN_POINTER_SIZE_BYTES (kgpc_target_pointer_size())
#endif
#ifndef CODEGEN_SIZEOF_RECURSION_LIMIT
#define CODEGEN_SIZEOF_RECURSION_LIMIT 32
#endif

/* Align an offset to a given alignment boundary. */
static long long codegen_align_offset_value(long long offset, int alignment) {
  if (alignment <= 1)
    return offset;
  long long rem = offset % alignment;
  if (rem == 0)
    return offset;
  return offset + (alignment - rem);
}

/* Compute the alignment requirement of a single record field.  Mirrors
 * SemCheck_sizeof.c::get_field_alignment without the cached_alignment
 * lookup (codegen does not maintain that cache field).  Returns 1 when the
 * type cannot be resolved to keep the existing behaviour safe. */
static int codegen_get_field_alignment(CodeGenContext *ctx,
                                       struct RecordField *field, int depth) {
  if (field == NULL)
    return 1;
  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    return 1;
  if (field->is_pointer)
    return CODEGEN_POINTER_SIZE_BYTES;
  if (field->is_array && field->array_is_open)
    return CODEGEN_POINTER_SIZE_BYTES;

  long long size = 0;
  int type_tag = field->type;
  const char *type_id = field->type_id;
  struct RecordType *record_type = field->nested_record;
  if (field->is_array) {
    type_tag = field->array_element_type;
    type_id = field->array_element_type_id;
    record_type = field->array_element_record;
  }

  if (record_type != NULL) {
    if (codegen_sizeof_record(ctx, record_type, &size, depth + 1) != 0)
      return 1;
  } else if (type_tag != UNKNOWN_TYPE || type_id != NULL) {
    if (codegen_sizeof_type(ctx, type_tag, type_id, NULL, &size, depth + 1) !=
        0)
      return 1;
  }

  if (size >= 8)
    return 8;
  if (size >= 4)
    return 4;
  if (size >= 2)
    return 2;
  return 1;
}

/* Compute alignment of any branch's worst-case field within a variant part. */
static int codegen_get_variant_part_alignment(CodeGenContext *ctx,
                                              struct VariantPart *variant,
                                              int depth) {
  if (variant == NULL)
    return 1;
  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    return 1;
  int max_align = 1;
  for (ListNode_t *cur = variant->branches; cur != NULL; cur = cur->next) {
    if (cur->type == LIST_VARIANT_BRANCH && cur->cur != NULL) {
      struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
      int branch_align =
          codegen_get_record_members_alignment(ctx, branch->members, depth + 1);
      if (branch_align > max_align)
        max_align = branch_align;
    }
  }
  return max_align;
}

/* Compute the maximum alignment across a list of record members. */
static int codegen_get_record_members_alignment(CodeGenContext *ctx,
                                                ListNode_t *members,
                                                int depth) {
  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT)
    return 1;
  int max_align = 1;
  for (ListNode_t *cur = members; cur != NULL; cur = cur->next) {
    if (cur->type == LIST_RECORD_FIELD && cur->cur != NULL) {
      struct RecordField *field = (struct RecordField *)cur->cur;
      if (field != NULL && !field->is_class_var) {
        int field_align = codegen_get_field_alignment(ctx, field, depth + 1);
        if (field_align > max_align)
          max_align = field_align;
      }
    } else if (cur->type == LIST_VARIANT_PART && cur->cur != NULL) {
      struct VariantPart *variant = (struct VariantPart *)cur->cur;
      int variant_align =
          codegen_get_variant_part_alignment(ctx, variant, depth + 1);
      if (variant_align > max_align)
        max_align = variant_align;
    }
  }
  return max_align;
}

static int codegen_sizeof_record_members(CodeGenContext *ctx,
                                         ListNode_t *members,
                                         long long *size_out, int depth) {
  if (size_out == NULL)
    return 1;

  long long total = 0;
  ListNode_t *cur = members;
  while (cur != NULL) {
    if (cur->cur == NULL) {
      cur = cur->next;
      continue;
    }

    if (cur->type == LIST_RECORD_FIELD) {
      struct RecordField *field = (struct RecordField *)cur->cur;
      long long field_size = 0;

      if (field->is_array) {
        if (field->array_is_open || field->array_end < field->array_start) {
          /* Dynamic arrays are 16-byte descriptors:
           * { void *data; int64_t length }. */
          field_size = 16;
        } else {
          long long element_size = 0;
          /* Anonymous element record (e.g. locHistory: array[0..24] of record
           * ... end) leaves array_element_type_id NULL.  Pass the in-memory
           * RecordType layout when we have one so codegen_sizeof_type
           * can size it directly instead of erroring on the missing
           * type-id. */
          struct RecordType *elem_rec = NULL;
          if (field->array_element_type == RECORD_TYPE)
            elem_rec = field->array_element_record;
          if (codegen_sizeof_type(ctx, field->array_element_type,
                                  field->array_element_type_id, elem_rec,
                                  &element_size, depth + 1) != 0)
            return 1;

          long long count =
              (long long)field->array_end - (long long)field->array_start + 1;
          if (count < 0) {
            codegen_report_error(ctx,
                                 "ERROR: Invalid bounds for array field %s.",
                                 field->name != NULL ? field->name : "");
            return 1;
          }

          field_size = element_size * count;
        }

        total += field_size;
        cur = cur->next;
        continue;
      }

      if (field->nested_record != NULL) {
        if (codegen_sizeof_record(ctx, field->nested_record, &field_size,
                                  depth + 1) != 0)
          return 1;
      } else {
        if (codegen_sizeof_type(ctx, field->type, field->type_id, NULL,
                                &field_size, depth + 1) != 0)
          return 1;
      }

      total += field_size;
    } else if (cur->type == LIST_VARIANT_PART) {
      struct VariantPart *variant = (struct VariantPart *)cur->cur;
      int variant_align =
          codegen_get_variant_part_alignment(ctx, variant, depth + 1);
      total = codegen_align_offset_value(total, variant_align);

      long long variant_size = 0;
      if (codegen_sizeof_variant_part(ctx, variant, &variant_size, depth + 1) !=
          0)
        return 1;
      total += variant_size;
    }

    cur = cur->next;
  }

  *size_out = total;
  return 0;
}

static int codegen_sizeof_variant_part(CodeGenContext *ctx,
                                       struct VariantPart *variant,
                                       long long *size_out, int depth) {
  if (size_out == NULL)
    return 1;

  if (variant == NULL) {
    *size_out = 0;
    return 0;
  }

  if (variant->has_cached_size) {
    *size_out = variant->cached_size;
    return 0;
  }

  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT) {
    codegen_report_error(
        ctx, "ERROR: Variant part nesting exceeds supported depth.");
    return 1;
  }

  long long max_size = 0;
  ListNode_t *cur = variant->branches;
  while (cur != NULL) {
    if (cur->type == LIST_VARIANT_BRANCH && cur->cur != NULL) {
      struct VariantBranch *branch = (struct VariantBranch *)cur->cur;
      long long branch_size = 0;
      if (codegen_sizeof_record_members(ctx, branch->members, &branch_size,
                                        depth + 1) != 0)
        return 1;
      if (branch_size > max_size)
        max_size = branch_size;
    }
    cur = cur->next;
  }

  variant->cached_size = max_size;
  variant->has_cached_size = 1;
  *size_out = max_size;
  return 0;
}

int codegen_sizeof_record(CodeGenContext *ctx, struct RecordType *record,
                          long long *size_out, int depth) {
  if (size_out == NULL)
    return 1;

  if (record == NULL) {
    *size_out = 0;
    return 0;
  }

  if (record->has_cached_size && record->cached_size > 0) {
    *size_out = record->cached_size;
    return 0;
  }

  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT) {
    codegen_report_error(ctx,
                         "ERROR: Record type nesting exceeds supported depth.");
    return 1;
  }

  long long members_size = 0;
  int result =
      codegen_sizeof_record_members(ctx, record->fields, &members_size, depth);
  if (result != 0)
    return result;

  /* For classes, add 8 bytes for the VMT pointer at the beginning */
  if (record_type_is_class(record))
    *size_out = 8 + members_size;
  else
    *size_out = members_size;
  record->cached_size = *size_out;
  record->has_cached_size = 1;

  return 0;
}

int codegen_sizeof_record_type(CodeGenContext *ctx, struct RecordType *record,
                               long long *size_out) {
  return codegen_sizeof_record(ctx, record, size_out, 0);
}

int codegen_sizeof_alias(CodeGenContext *ctx, struct TypeAlias *alias,
                         long long *size_out, int depth) {
  if (size_out == NULL)
    return 1;

  if (alias == NULL) {
    codegen_report_error(
        ctx,
        "ERROR: Incomplete type alias encountered during size computation.");
    return 1;
  }

  if (alias->storage_size > 0 && !alias->is_array && !alias->is_set &&
      !alias->is_enum && !alias->is_file) {
    *size_out = alias->storage_size;
    return 0;
  }

  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT) {
    codegen_report_error(ctx,
                         "ERROR: Type alias nesting exceeds supported depth.");
    return 1;
  }

  if (alias->is_array) {
    if (alias->is_open_array || alias->array_end < alias->array_start) {
      codegen_report_error(
          ctx, "ERROR: Unable to determine size of open array type.");
      return 1;
    }

    if (codegen_sizeof_named_array_alias(ctx, alias, size_out) == 0 &&
        *size_out > 0) {
      return 0;
    }

    long long element_size = 0;
    if (codegen_sizeof_type(ctx, alias->array_element_type,
                            alias->array_element_type_id, NULL, &element_size,
                            depth + 1) != 0)
      return 1;

    long long count =
        (long long)alias->array_end - (long long)alias->array_start + 1;
    if (count < 0) {
      codegen_report_error(
          ctx, "ERROR: Invalid bounds for array type during size computation.");
      return 1;
    }

    *size_out = element_size * count;
    return 0;
  }

  if (alias->is_set) {
    *size_out = codegen_set_storage_size_from_alias(ctx, alias);
    return 0;
  }

  if (alias->base_type != UNKNOWN_TYPE)
    return codegen_sizeof_type(ctx, alias->base_type, NULL, NULL, size_out,
                               depth + 1);

  if (alias->target_type_id != NULL)
    return codegen_sizeof_type(ctx, UNKNOWN_TYPE, alias->target_type_id, NULL,
                               size_out, depth + 1);

  codegen_report_error(ctx, "ERROR: Unable to resolve type alias size.");
  return 1;
}

int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
                            long long *size_out, int depth) {
  if (size_out == NULL || node == NULL)
    return 1;

  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT) {
    codegen_report_error(
        ctx, "ERROR: Type resolution exceeded supported recursion depth.");
    return 1;
  }

  /* PREFERRED PATH: Try using KgpcType directly if available */
  if (node->type != NULL) {
    if (kgpc_type_is_array(node->type) &&
        codegen_sizeof_array_type_kgpc(ctx, node->type, size_out) == 0) {
      return 0;
    }
    long long size = kgpc_type_sizeof(node->type);
    if (size > 0) {
      *size_out = size;
      return 0;
    } else if (size == 0) {
      /* Zero-sized type */
      *size_out = 0;
      return 0;
    }
    /* else size < 0: kgpc_type_sizeof couldn't determine size, fall through to
     * legacy path */
  }

  /* LEGACY PATH: KgpcType not available or couldn't determine size */

  /* Check if this is an array */
  int is_array = hashnode_is_array(node);

  if (is_array)
    return codegen_sizeof_array_node(ctx, node, size_out, depth);

  if (node->hash_type == HASHTYPE_TYPE) {
    struct RecordType *record = hashnode_get_record_type(node);
    if (record != NULL)
      return codegen_sizeof_record(ctx, record, size_out, depth + 1);
    struct TypeAlias *alias = hashnode_get_type_alias(node);
    if (alias != NULL)
      return codegen_sizeof_alias(ctx, alias, size_out, depth + 1);
  }

  if (hashnode_is_record(node)) {
    struct RecordType *record_type = hashnode_get_record_type(node);
    if (record_type != NULL)
      return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);
  }

  struct TypeAlias *alias = hashnode_get_type_alias(node);
  if (alias != NULL)
    return codegen_sizeof_alias(ctx, alias, size_out, depth + 1);

  if (node->type != NULL) {
    long long base = kgpc_type_sizeof(node->type);
    if (base >= 0) {
      *size_out = base;
      return 0;
    }
  } else {
    codegen_report_error(ctx, "ERROR: Symbol %s has no type information.",
                         node->id != NULL ? node->id : "");
    return 1;
  }

  codegen_report_error(ctx, "ERROR: Unable to determine size for symbol %s.",
                       node->id != NULL ? node->id : "");
  return 1;
}

int codegen_get_record_size(CodeGenContext *ctx, struct Expression *expr,
                            long long *size_out) {
  if (expr == NULL || size_out == NULL)
    return 1;

  /* ShortString uses fixed 256-byte storage (length byte + 255 chars). */
  if (expr_get_type_tag(expr) == SHORTSTRING_TYPE) {
    *size_out = 256;
    return 0;
  }

  /* Large set types (> 4 bytes) are routed through record-style assignment.
   * Use kgpc_type_sizeof to get the set storage size directly. */
  if (expr_get_type_tag(expr) == SET_TYPE) {
    KgpcType *set_type = expr_get_kgpc_type(expr);
    if (set_type != NULL) {
      long long set_size = kgpc_type_sizeof(set_type);
      if (set_size > 0) {
        *size_out = set_size;
        return 0;
      }
    }
  }

  /* For record field access to a large set field, use the field's cached size
   */
  if (expr->type == EXPR_RECORD_ACCESS && ctx != NULL) {
    struct RecordField *field =
        codegen_lookup_record_field_expr((struct Expression *)expr, ctx);
    if (field != NULL && field->type == SET_TYPE && field->has_cached_layout &&
        field->cached_size > 4) {
      *size_out = field->cached_size;
      return 0;
    }
  }

  if (kgpc_getenv("KGPC_DEBUG_RECORD_SIZE") != NULL) {
    KgpcType *dbg_type = expr_get_kgpc_type(expr);
    struct RecordType *dbg_record =
        codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
    fprintf(stderr,
            "[KGPC_DEBUG_RECORD_SIZE] expr_type=%d resolved_tag=%d "
            "record_type=%p kgpc=%s\n",
            expr->type, codegen_tag_from_kgpc(dbg_type), (void *)dbg_record,
            dbg_type != NULL ? kgpc_type_to_string(dbg_type) : "<null>");
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
      fprintf(stderr, "[KGPC_DEBUG_RECORD_SIZE]   id=%s\n", expr->expr_data.id);
    else if (expr->type == EXPR_RECORD_ACCESS &&
             expr->expr_data.record_access_data.field_id != NULL)
      fprintf(stderr, "[KGPC_DEBUG_RECORD_SIZE]   field=%s\n",
              expr->expr_data.record_access_data.field_id);
  }

  /* Check resolved_kgpc_type first — the semantic checker may have
   * rewritten the type (e.g. interface identifier → TGUID). */
  KgpcType *expr_type = expr_get_kgpc_type(expr);
  if (expr_type != NULL) {
    if (kgpc_type_is_shortstring(expr_type)) {
      long long short_size = kgpc_type_sizeof(expr_type);
      *size_out = (short_size > 1 && short_size <= INT_MAX) ? short_size : 256;
      return 0;
    }
    if (kgpc_type_is_string(expr_type)) {
      /* AnsiString/UnicodeString are pointer-sized */
      *size_out = CODEGEN_POINTER_SIZE_BYTES;
      return 0;
    }
    if (kgpc_type_is_record(expr_type))
      return codegen_sizeof_record(ctx, expr_type->info.record_info, size_out,
                                   0);
    if (kgpc_type_is_pointer(expr_type) && expr_type->info.points_to != NULL &&
        kgpc_type_is_record(expr_type->info.points_to))
      return codegen_sizeof_record(
          ctx, expr_type->info.points_to->info.record_info, size_out, 0);
  }

  struct RecordType *expr_record =
      codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
  if (expr_record != NULL)
    return codegen_sizeof_record(ctx, expr_record, size_out, 0);

  if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 && node != NULL)
      return codegen_sizeof_hashnode(ctx, node, size_out, 0);
  }

  if (expr->type == EXPR_POINTER_DEREF) {
    if (expr->pointer_subtype_id != NULL && ctx != NULL &&
        ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, expr->pointer_subtype_id) != 0 &&
          node != NULL)
        return codegen_sizeof_hashnode(ctx, node, size_out, 0);
    }

    struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    while (pointer_expr != NULL && pointer_expr->type == EXPR_TYPECAST &&
           pointer_expr->expr_data.typecast_data.expr != NULL) {
      pointer_expr = pointer_expr->expr_data.typecast_data.expr;
    }

    if (pointer_expr != NULL) {
      struct RecordType *pointer_record = codegen_expr_record_type(
          pointer_expr, ctx != NULL ? ctx->symtab : NULL);
      if (pointer_record != NULL)
        return codegen_sizeof_record(ctx, pointer_record, size_out, 0);

      if (pointer_expr->pointer_subtype_id != NULL && ctx != NULL &&
          ctx->symtab != NULL) {
        HashNode_t *node = NULL;
        if (FindSymbol(&node, ctx->symtab, pointer_expr->pointer_subtype_id) !=
                0 &&
            node != NULL)
          return codegen_sizeof_hashnode(ctx, node, size_out, 0);
      }
    }
  }

  if (expr->type == EXPR_ARRAY_ACCESS) {
    struct RecordType *elem_record = expr->array_element_record_type;
    if (elem_record == NULL && ctx != NULL && ctx->symtab != NULL &&
        expr->array_element_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, expr->array_element_type_id) !=
              0 &&
          type_node != NULL) {
        elem_record = hashnode_get_record_type(type_node);
        if (elem_record == NULL && type_node->type != NULL &&
            kgpc_type_is_record(type_node->type)) {
          elem_record = kgpc_type_get_record(type_node->type);
        }
        if (elem_record == NULL) {
          struct TypeAlias *alias = hashnode_get_type_alias(type_node);
          if (alias != NULL) {
            if (alias->inline_record_type != NULL)
              elem_record = alias->inline_record_type;
            else if (alias->target_type_id != NULL) {
              HashNode_t *target_node = NULL;
              if (FindSymbol(&target_node, ctx->symtab,
                             alias->target_type_id) != 0 &&
                  target_node != NULL) {
                elem_record = hashnode_get_record_type(target_node);
                if (elem_record == NULL && target_node->type != NULL &&
                    kgpc_type_is_record(target_node->type)) {
                  elem_record = kgpc_type_get_record(target_node->type);
                }
              }
            }
          }
        }
      }
    }
    if (elem_record == NULL && ctx != NULL) {
      struct Expression *array_expr =
          expr->expr_data.array_access_data.array_expr;
      struct RecordField *field =
          codegen_lookup_record_field_expr(array_expr, ctx);
      if (field != NULL) {
        if (field->array_element_record != NULL)
          elem_record = field->array_element_record;
        else if (field->array_element_type_id != NULL && ctx->symtab != NULL) {
          HashNode_t *field_node = NULL;
          if (FindSymbol(&field_node, ctx->symtab,
                         field->array_element_type_id) != 0 &&
              field_node != NULL) {
            elem_record = hashnode_get_record_type(field_node);
            if (elem_record == NULL && field_node->type != NULL &&
                kgpc_type_is_record(field_node->type)) {
              elem_record = kgpc_type_get_record(field_node->type);
            }
          }
        }
      }
    }
    if (elem_record != NULL)
      return codegen_sizeof_record(ctx, elem_record, size_out, 0);
  }

  if (expr->type == EXPR_TYPECAST && expr->expr_data.typecast_data.expr != NULL)
    return codegen_get_record_size(ctx, expr->expr_data.typecast_data.expr,
                                   size_out);

  codegen_report_error(
      ctx, "ERROR: Unable to determine size for record expression.");
  return 1;
}

int codegen_sizeof_pointer_target(CodeGenContext *ctx,
                                  struct Expression *pointer_expr,
                                  long long *size_out) {
  if (pointer_expr == NULL || size_out == NULL)
    return 1;

  int subtype = pointer_expr->pointer_subtype;
  const char *type_id = pointer_expr->pointer_subtype_id;
  struct RecordType *record_type =
      codegen_expr_record_type(pointer_expr, ctx != NULL ? ctx->symtab : NULL);

  if (record_type == NULL && type_id != NULL && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, type_id) != 0 && node != NULL)
      record_type = hashnode_get_record_type(node);
  }

  if (record_type == NULL && subtype == RECORD_TYPE && type_id == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to determine record size for pointer target.");
    return 1;
  }

  /* For untyped pointers (Pointer type with no target info), return failure
   * without reporting an error — the caller handles this by defaulting to
   * step=1 */
  if (subtype == UNKNOWN_TYPE && type_id == NULL && record_type == NULL) {
    KgpcType *pointer_type = expr_get_kgpc_type(pointer_expr);
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type)) {
      KgpcType *points_to = pointer_type->info.points_to;
      if (points_to != NULL) {
        if (kgpc_type_is_set(points_to) && points_to->type_alias != NULL &&
            points_to->type_alias->is_set) {
          long long set_size =
              codegen_set_storage_size_for_set_alias(points_to->type_alias);
          if (set_size > 0) {
            *size_out = set_size;
            return 0;
          }
        }
        long long pointee_size = kgpc_type_sizeof(points_to);
        if (pointee_size > 0) {
          *size_out = pointee_size;
          return 0;
        }
      }
    }
    return 1;
  }

  if (codegen_sizeof_type(ctx, subtype, type_id, record_type, size_out, 0) ==
          0 &&
      *size_out > 0) {
    return 0;
  }

  KgpcType *pointer_type = expr_get_kgpc_type(pointer_expr);
  if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type)) {
    struct TypeAlias *pointer_alias = kgpc_type_get_type_alias(pointer_type);
    if (pointer_alias != NULL && pointer_alias->is_pointer) {
      if (pointer_alias->pointer_type_id != NULL && ctx != NULL &&
          ctx->symtab != NULL) {
        HashNode_t *target_node = NULL;
        if (FindSymbol(&target_node, ctx->symtab,
                       pointer_alias->pointer_type_id) != 0 &&
            target_node != NULL &&
            codegen_sizeof_hashnode(ctx, target_node, size_out, 0) == 0 &&
            *size_out > 0) {
          return 0;
        }
      }

      if (pointer_alias->pointer_type != UNKNOWN_TYPE &&
          codegen_sizeof_type(ctx, pointer_alias->pointer_type,
                              pointer_alias->pointer_type_id, NULL, size_out,
                              0) == 0 &&
          *size_out > 0) {
        return 0;
      }
    }

    KgpcType *points_to = pointer_type->info.points_to;
    if (points_to != NULL) {
      if (kgpc_type_is_set(points_to) && points_to->type_alias != NULL) {
        long long set_size = 0;
        if (points_to->type_alias->is_set)
          set_size =
              codegen_set_storage_size_for_set_alias(points_to->type_alias);
        if (set_size > 0) {
          *size_out = set_size;
          return 0;
        }
      }
      if (kgpc_type_is_array(points_to)) {
        KgpcArrayDimensionInfo info;
        int dimension_status = kgpc_type_get_array_dimension_info(
            points_to, ctx != NULL ? ctx->symtab : NULL, &info);
        assert(dimension_status == 0 && info.dim_count > 0);

        long long element_size = info.element_size;
        KgpcType *element_type = kgpc_type_get_array_element_type(points_to);
        if (element_type != NULL && element_type->kind == TYPE_KIND_RECORD) {
          assert(element_type->info.record_info != NULL);
          int record_size_status = codegen_sizeof_record(
              ctx, element_type->info.record_info, &element_size, 0);
          assert(record_size_status == 0 && element_size > 0);
          (void)record_size_status;
        } else if (element_size <= 0 && element_type != NULL) {
          element_size = kgpc_type_sizeof(element_type);
        }
        assert(element_size > 0);

        long long total_size = element_size;
        for (int i = info.dim_count - 1; i >= 0; i--) {
          assert(info.dim_sizes[i] > 0);
          assert(total_size <= LLONG_MAX / info.dim_sizes[i]);
          total_size *= info.dim_sizes[i];
        }
        assert(total_size > 0);
        *size_out = total_size;
        return 0;
      }
      long long pointee_size = kgpc_type_sizeof(points_to);
      if (pointee_size > 0) {
        if (!(kgpc_type_is_set(points_to) && pointee_size <= 4 &&
              pointer_expr->pointer_subtype_id != NULL)) {
          *size_out = pointee_size;
          return 0;
        }
      }
    }
  }

  return 1;
}

/* Best-effort size for a record field, respecting packed/range aliases */
long long codegen_record_field_effective_size(struct Expression *expr,
                                              CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL)
    return expr_effective_size_bytes(expr);

  struct RecordField *field = codegen_lookup_record_field_expr(expr, ctx);
  if (field == NULL && expr->expr_data.record_access_data.field_id != NULL &&
      expr->expr_data.record_access_data.record_expr != NULL && ctx != NULL &&
      ctx->symtab != NULL) {
    struct RecordType *record = codegen_expr_record_type(
        expr->expr_data.record_access_data.record_expr, ctx->symtab);
    if (record != NULL)
      field = semcheck_find_class_field_including_hidden(
          ctx->symtab, record, expr->expr_data.record_access_data.field_id,
          NULL);
  }
  long long field_size = 0;
  if (field != NULL && !field->is_array) {
    const char *field_type_id = field->type_id;
    if (field_type_id == NULL && field->type_ref != NULL)
      field_type_id = type_ref_base_name(field->type_ref);

    if (expr->resolved_kgpc_type != NULL &&
        expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
        expr->resolved_kgpc_type->info.primitive_type_tag == REAL_TYPE) {
      long long resolved_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
      if (resolved_size > 0)
        return resolved_size;
    }

    if (field->type == REAL_TYPE && field_type_id != NULL) {
      if (pascal_identifier_equals(field_type_id, "Single"))
        return 4;
      if (pascal_identifier_equals(field_type_id, "Double") ||
          pascal_identifier_equals(field_type_id, "Real"))
        return 8;
    }

    if (ctx->symtab != NULL && field_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, field_type_id) != 0 &&
          type_node != NULL && type_node->type != NULL) {
        long long type_size = kgpc_type_sizeof(type_node->type);
        if (type_size > 0 && type_node->type->kind == TYPE_KIND_PRIMITIVE &&
            type_node->type->info.primitive_type_tag == ENUM_TYPE)
          return type_size;
        if (type_size > 0 &&
            !(field->has_cached_layout && field->cached_size > 0))
          return type_size;
      }
    }

    if (field->has_cached_layout && field->cached_size > 0)
      return field->cached_size;

    struct RecordType *nested = field->nested_record;
    if (codegen_sizeof_type_reference(ctx, field->type, field->type_id, nested,
                                      &field_size) == 0 &&
        field_size > 0)
      return field_size;
  }

  long long size = expr_effective_size_bytes(expr);
  if (size > 0)
    return size;
  return field_size;
}
