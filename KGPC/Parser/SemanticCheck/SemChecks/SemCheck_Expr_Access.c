/*
    SemCheck_Expr_Access.c - Array access and function call semantic checks

    This file contains semantic checking for:
    - Array element access (arr[i])
    - Function/procedure calls

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"
#include "SemCheck_stmt.h"
#include <ctype.h>
#include <limits.h>
#include <time.h>

static void semcheck_clear_array_linearization(struct Expression *expr) {
  if (expr == NULL)
    return;
  if (expr->expr_data.array_access_data.linear_strides != NULL) {
    free(expr->expr_data.array_access_data.linear_strides);
    expr->expr_data.array_access_data.linear_strides = NULL;
  }
  if (expr->expr_data.array_access_data.linear_lowers != NULL) {
    free(expr->expr_data.array_access_data.linear_lowers);
    expr->expr_data.array_access_data.linear_lowers = NULL;
  }
  expr->expr_data.array_access_data.linear_index_count = 0;
  expr->expr_data.array_access_data.linear_info_valid = 0;
}

static void
semcheck_compute_array_linearization(SymTab_t *symtab, struct Expression *expr,
                                     struct Expression *array_expr) {
  if (expr == NULL || array_expr == NULL)
    return;

  KgpcType *array_type = array_expr->resolved_kgpc_type;
  if (array_type == NULL && array_expr->type == EXPR_VAR_ID && symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, array_expr->expr_data.id) != 0 &&
        node != NULL)
      array_type = node->type;
  } else if (array_type == NULL && array_expr->type == EXPR_POINTER_DEREF &&
             array_expr->expr_data.pointer_deref_data.pointer_expr != NULL) {
    struct Expression *pointer_expr =
        array_expr->expr_data.pointer_deref_data.pointer_expr;
    KgpcType *pointer_type = pointer_expr->resolved_kgpc_type;
    if (pointer_type == NULL && pointer_expr->type == EXPR_VAR_ID &&
        pointer_expr->expr_data.id != NULL && symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, symtab, pointer_expr->expr_data.id) != 0 &&
          node != NULL)
        pointer_type = node->type;
    }
    if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type)) {
      KgpcType *pointee =
          kgpc_type_resolve_pointer_pointee(pointer_type, symtab);
      if (pointee != NULL && kgpc_type_is_array(pointee))
        array_type = pointee;
    }
  }

  if (array_type == NULL || !kgpc_type_is_array(array_type))
    return;

  KgpcArrayDimensionInfo info;
  if (kgpc_type_get_array_dimension_info(array_type, symtab, &info) != 0)
    return;

  int extra_count = 0;
  if (expr->expr_data.array_access_data.extra_indices != NULL)
    extra_count = ListLength(expr->expr_data.array_access_data.extra_indices);

  int index_count = 1 + extra_count;
  if (index_count <= 1)
    return;

  if (index_count > info.dim_count)
    index_count = info.dim_count;

  long long *lowers =
      (long long *)calloc((size_t)index_count, sizeof(long long));
  long long *strides =
      (long long *)calloc((size_t)index_count, sizeof(long long));
  if (lowers == NULL || strides == NULL) {
    if (lowers != NULL)
      free(lowers);
    if (strides != NULL)
      free(strides);
    return;
  }

  for (int i = 0; i < index_count; i++) {
    lowers[i] = info.dim_lowers[i];
    strides[i] = info.strides[i];
  }

  semcheck_clear_array_linearization(expr);
  expr->expr_data.array_access_data.linear_index_count = index_count;
  expr->expr_data.array_access_data.linear_strides = strides;
  expr->expr_data.array_access_data.linear_lowers = lowers;
  expr->expr_data.array_access_data.linear_info_valid = 1;

  if (kgpc_getenv("KGPC_DEBUG_ARRAY_LINEAR") != NULL) {
    fprintf(stderr,
            "[SemCheck] array linearization: indices=%d elem_size=%lld\n",
            index_count, info.element_size);
    for (int i = 0; i < index_count; ++i) {
      fprintf(stderr, "  dim%d: low=%lld stride=%lld\n", i, lowers[i],
              strides[i]);
    }
  }
}

/** ARRAY_ACCESS **/
int semcheck_arrayaccess(int *type_return, SymTab_t *symtab,
                         struct Expression *expr, int max_scope_lev,
                         int mutating) {
  int return_val = 0;
  int index_type = UNKNOWN_TYPE;
  int element_type = UNKNOWN_TYPE;
  struct Expression *array_expr;
  struct Expression *access_expr;

  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_ARRAY_ACCESS);

  semcheck_clear_pointer_info(expr);
  semcheck_clear_array_info(expr);
  semcheck_clear_array_linearization(expr);

  array_expr = expr->expr_data.array_access_data.array_expr;
  access_expr = expr->expr_data.array_access_data.index_expr;

  if (array_expr == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, array access requires a base expression.\n\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  /* Normalize parser shape for expressions like:
   *   not ACollation^.Backwards[i]
   * cparser can produce ARRAY_ACCESS(RECORD_ACCESS(NOT(base), field), index),
   * but semantic checking expects NOT to wrap the whole indexed expression.
   */
  if (array_expr->type == EXPR_RECORD_ACCESS &&
      array_expr->expr_data.record_access_data.record_expr != NULL) {
    struct Expression *record_base =
        array_expr->expr_data.record_access_data.record_expr;
    if (record_base->type == EXPR_RELOP &&
        record_base->expr_data.relop_data.type == NOT &&
        record_base->expr_data.relop_data.right == NULL &&
        record_base->expr_data.relop_data.left != NULL &&
        array_expr->expr_data.record_access_data.field_id != NULL) {
      struct Expression *inner_record = record_base->expr_data.relop_data.left;
      record_base->expr_data.relop_data.left = NULL;

      struct Expression *field_access = mk_recordaccess(
          array_expr->line_num, inner_record,
          strdup(array_expr->expr_data.record_access_data.field_id));
      if (field_access == NULL) {
        semcheck_error_with_context_at(expr->line_num, expr->col_num,
                                       expr->source_index,
                                       "Error on line %d: failed to normalize "
                                       "NOT over indexed field access.\n",
                                       expr->line_num);
        *type_return = UNKNOWN_TYPE;
        return 1;
      }

      struct Expression *indexed_expr =
          (struct Expression *)calloc(1, sizeof(struct Expression));
      if (indexed_expr == NULL) {
        semcheck_error_with_context_at(expr->line_num, expr->col_num,
                                       expr->source_index,
                                       "Error on line %d: failed to allocate "
                                       "normalized indexed expression.\n",
                                       expr->line_num);
        destroy_expr(field_access);
        *type_return = UNKNOWN_TYPE;
        return 1;
      }
      *indexed_expr = *expr;
      indexed_expr->expr_data.array_access_data.array_expr = field_access;

      destroy_expr(array_expr);

      expr->type = EXPR_RELOP;
      memset(&expr->expr_data.relop_data, 0,
             sizeof(expr->expr_data.relop_data));
      expr->expr_data.relop_data.type = NOT;
      expr->expr_data.relop_data.left = indexed_expr;
      expr->expr_data.relop_data.right = NULL;
      semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);

      return semcheck_relop(type_return, symtab, expr, max_scope_lev, mutating);
    }
  }

  if (array_expr->type == EXPR_VAR_ID) {
    HashNode_t *array_node = NULL;
    int found = FindSymbol(&array_node, symtab, array_expr->expr_data.id);
    int has_value_ident = (found && array_node != NULL &&
                           array_node->hash_type != HASHTYPE_TYPE &&
                           array_node->hash_type != HASHTYPE_FUNCTION &&
                           array_node->hash_type != HASHTYPE_PROCEDURE &&
                           array_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE);
    if (!has_value_ident) {
      int property_result = semcheck_try_indexed_property_getter(
          type_return, symtab, expr, max_scope_lev, mutating);
      if (property_result >= 0)
        return return_val + property_result;

      /* If the EXPR_VAR_ID was not found in scope AND indexed property
       * resolution from Self failed, try the WITH context.  This handles
       * patterns like:
       *   with SomeList do  WriteLn(Items[i]);
       * where Items is an indexed property of SomeList's class.
       * Transform the base from EXPR_VAR_ID to EXPR_RECORD_ACCESS so
       * that semcheck_try_indexed_property_getter's RECORD_ACCESS path
       * can detect the indexed property with its enclosing record. */
      struct Expression *with_expr = NULL;
      int with_status = semcheck_with_try_resolve(
          array_expr->expr_data.id, symtab, &with_expr, expr->line_num);
      if (with_status == 0 && with_expr != NULL) {
        char *field_id = array_expr->expr_data.id;
        array_expr->expr_data.id = NULL;
        array_expr->type = EXPR_RECORD_ACCESS;
        memset(&array_expr->expr_data.record_access_data, 0,
               sizeof(array_expr->expr_data.record_access_data));
        array_expr->expr_data.record_access_data.record_expr = with_expr;
        array_expr->expr_data.record_access_data.field_id = field_id;
        array_expr->expr_data.record_access_data.field_offset = 0;

        property_result = semcheck_try_indexed_property_getter(
            type_return, symtab, expr, max_scope_lev, mutating);
        if (property_result >= 0)
          return return_val + property_result;
      }
    }
  } else if (array_expr->type == EXPR_RECORD_ACCESS) {
    int property_result = semcheck_try_indexed_property_getter(
        type_return, symtab, expr, max_scope_lev, mutating);
    if (property_result >= 0)
      return return_val + property_result;
  }

  int base_type = UNKNOWN_TYPE;
  KgpcType *base_kgpc_type = NULL;
  int base_mutating = mutating;
  if (mutating != NO_MUTATE && (array_expr->type == EXPR_VAR_ID ||
                                array_expr->type == EXPR_RECORD_ACCESS)) {
    struct ClassProperty *prop = NULL;
    if (array_expr->type == EXPR_VAR_ID) {
      HashNode_t *self_node = NULL;
      if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL) {
        struct RecordType *self_record = get_record_type_from_node(self_node);
        if (self_record != NULL)
          prop = semcheck_find_class_property(symtab, self_record,
                                              array_expr->expr_data.id, NULL);
      }
    } else if (array_expr->type == EXPR_RECORD_ACCESS) {
      struct Expression *record_expr =
          array_expr->expr_data.record_access_data.record_expr;
      const char *field_id = array_expr->expr_data.record_access_data.field_id;
      if (record_expr != NULL && field_id != NULL) {
        KgpcType *record_kgpc_type = NULL;
        semcheck_expr_with_type(&record_kgpc_type, symtab, record_expr,
                                max_scope_lev, NO_MUTATE);
        struct RecordType *owner_record = semcheck_with_resolve_record_type(
            symtab, record_expr,
            semcheck_tag_from_kgpc(record_expr->resolved_kgpc_type),
            expr->line_num);
        if (owner_record != NULL)
          prop = semcheck_find_class_property(symtab, owner_record, field_id,
                                              NULL);
      }
    }

    if (prop != NULL && !(prop->is_indexed && prop->write_accessor != NULL))
      base_mutating = NO_MUTATE;
  }

  return_val += semcheck_expr_with_type(&base_kgpc_type, symtab, array_expr,
                                        max_scope_lev, base_mutating);
  base_type = semcheck_tag_from_kgpc(base_kgpc_type);

  /* Support default indexed property access by converting obj[idx] to
   * obj.field[idx]. This handles classes like TStringList where obj[i] maps to
   * obj.FItems[i]. */
  {
    KgpcType *rec_ptr_type = array_expr->resolved_kgpc_type;
    struct RecordType *rec = NULL;
    if (rec_ptr_type != NULL && kgpc_type_is_pointer(rec_ptr_type) &&
        rec_ptr_type->info.points_to != NULL &&
        kgpc_type_is_record(rec_ptr_type->info.points_to)) {
      rec = kgpc_type_get_record(rec_ptr_type->info.points_to);
    } else if (rec_ptr_type != NULL && kgpc_type_is_record(rec_ptr_type)) {
      rec = kgpc_type_get_record(rec_ptr_type);
    }

    /* Check if class has a default indexed property and we're not already
     * accessing it */
    if (rec != NULL && rec->default_indexed_property != NULL) {
      int is_already_default_field = 0;
      if (array_expr->type == EXPR_RECORD_ACCESS &&
          array_expr->expr_data.record_access_data.field_id != NULL &&
          pascal_identifier_equals(
              array_expr->expr_data.record_access_data.field_id,
              rec->default_indexed_property)) {
        is_already_default_field = 1;
      }

      if (!is_already_default_field) {
        /* Transform to obj.default_indexed_property[idx] */
        struct Expression *field_access =
            (struct Expression *)calloc(1, sizeof(struct Expression));
        assert(field_access != NULL);
        field_access->line_num = array_expr->line_num;
        field_access->type = EXPR_RECORD_ACCESS;
        field_access->expr_data.record_access_data.record_expr = array_expr;
        field_access->expr_data.record_access_data.field_id =
            strdup(rec->default_indexed_property);

        expr->expr_data.array_access_data.array_expr = field_access;
        array_expr = field_access;

        /* Try indexed property getter first.  For true indexed properties
         * (e.g. property Items[Index: Integer]: String read GetItem; default)
         * this rewrites the whole EXPR_ARRAY_ACCESS into a getter call and
         * returns the correct property result type.  We must do this BEFORE
         * calling semcheck_expr_with_type on the field_access, because that
         * would eagerly invoke the property getter without the index argument,
         * losing the indexed-property semantics. */
        int ipg_result = semcheck_try_indexed_property_getter(
            type_return, symtab, expr, max_scope_lev, mutating);
        if (ipg_result >= 0)
          return return_val + ipg_result;

        /* When mutating (LHS of assignment), the getter bails out because
         * it only handles read access.  Resolve the property element type
         * from the class property declaration without rewriting the AST —
         * the actual setter rewrite will happen later in
         * semcheck_try_indexed_property_assignment. */
        if (mutating != NO_MUTATE && rec != NULL) {
          struct RecordType *prop_owner = NULL;
          struct ClassProperty *prop = semcheck_find_class_property(
              symtab, rec, rec->default_indexed_property, &prop_owner);
          if (prop != NULL && prop->is_indexed &&
              prop->write_accessor != NULL) {
            *type_return = prop->type;
            return return_val;
          }
        }

        /* Not an indexed property — evaluate the field access so array info
         * is populated (e.g. for a default-indexed field like FItems). */
        int field_type = UNKNOWN_TYPE;
        KgpcType *field_kgpc_type = NULL;
        semcheck_expr_with_type(&field_kgpc_type, symtab, field_access,
                                max_scope_lev, mutating);
        field_type = semcheck_tag_from_kgpc(field_kgpc_type);
        base_type = field_type;
        /* Continue with the rest of semcheck using the new array_expr */
      }
    }
  }

  /* Handle pointer deref indexing: p^[i] where p is a pointer to element (e.g.,
   * PChar). Rewrite the array access to index the pointer directly instead of
   * the dereferenced value, but preserve array/string semantics when the
   * pointer targets an array or string. */
  if (!array_expr->is_array_expr && base_type != POINTER_TYPE &&
      array_expr->type == EXPR_POINTER_DEREF) {
    struct Expression *pointer_expr =
        array_expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr != NULL) {
      KgpcType *ptr_kgpc_type = pointer_expr->resolved_kgpc_type;
      if (ptr_kgpc_type != NULL && kgpc_type_is_pointer(ptr_kgpc_type)) {
        KgpcType *points_to = ptr_kgpc_type->info.points_to;
        int points_to_is_string = 0;
        if (points_to != NULL) {
          struct TypeAlias *alias = kgpc_type_get_type_alias(points_to);
          if (kgpc_type_is_string(points_to) ||
              kgpc_type_is_shortstring(points_to) ||
              (alias != NULL && alias->is_shortstring)) {
            points_to_is_string = 1;
          }
        }
        if (!points_to_is_string) {
          if (pointer_expr->pointer_subtype == SHORTSTRING_TYPE ||
              pointer_expr->pointer_subtype == STRING_TYPE)
            points_to_is_string = 1;
          else if (pointer_expr->pointer_subtype_id != NULL) {
            int subtype_tag = semcheck_map_builtin_type_name(
                symtab, pointer_expr->pointer_subtype_id);
            if (subtype_tag == SHORTSTRING_TYPE || subtype_tag == STRING_TYPE)
              points_to_is_string = 1;
            else if (pascal_identifier_equals(pointer_expr->pointer_subtype_id,
                                              "ShortString") ||
                     pascal_identifier_equals(pointer_expr->pointer_subtype_id,
                                              "AnsiString") ||
                     pascal_identifier_equals(pointer_expr->pointer_subtype_id,
                                              "UnicodeString") ||
                     pascal_identifier_equals(pointer_expr->pointer_subtype_id,
                                              "WideString"))
              points_to_is_string = 1;
          }
        }
        if (points_to == NULL ||
            (!kgpc_type_is_array(points_to) && !points_to_is_string)) {
          /* Replace array_expr with the pointer (skip the deref) so we
           * index the pointer. The original pointer_deref Expression
           * is now orphaned — its pointer_expr child has been moved
           * into the array_expr slot, so null the child first and
           * destroy the deref wrapper to avoid leaking it. */
          struct Expression *old_deref = array_expr;
          old_deref->expr_data.pointer_deref_data.pointer_expr = NULL;
          expr->expr_data.array_access_data.array_expr = pointer_expr;
          array_expr = pointer_expr;
          base_kgpc_type = ptr_kgpc_type;
          base_type = POINTER_TYPE;
          destroy_expr(old_deref);
        }
      }
    }
  }

  int base_is_string =
      (kgpc_type_is_string(base_kgpc_type) && !array_expr->is_array_expr);
  /* Only treat as pointer indexing if NOT an array expression - for arrays of
   * pointers, we want to go through the array path to properly handle element
   * type info */
  int base_is_pointer =
      (base_type == POINTER_TYPE && !array_expr->is_array_expr);

  if (!array_expr->is_array_expr && !base_is_string && !base_is_pointer) {
    if (base_type == UNKNOWN_TYPE) {
      *type_return = UNKNOWN_TYPE;
      return return_val;
    }
    int property_result = semcheck_try_indexed_property_getter(
        type_return, symtab, expr, max_scope_lev, mutating);
    if (property_result >= 0)
      return return_val + property_result;

    /* Allow chained bracket access for multi-dimensional arrays:
     * arr[x][y] where arr[x] is an EXPR_ARRAY_ACCESS whose result
     * is not marked as an array (because the type system stores
     * multi-dimensional arrays as flat, not nested).  Treat the
     * result of the inner access as indexable. */
    if (array_expr->type == EXPR_ARRAY_ACCESS) {
      /* Treat as array access — element type/size carries over
       * from the inner array access expression. */
      element_type = array_expr->array_element_type;
      if (element_type == UNKNOWN_TYPE)
        element_type = LONGINT_TYPE;
      *type_return = element_type;
      return return_val;
    }

    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, expression is not indexable as an array.\n\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return return_val + 1;
  }

  if (base_is_string) {
    /* String indexing yields a character. WideString/UnicodeString elements
     * are WideChar while regular strings use AnsiChar. Both resolve to
     * CHAR_TYPE at the semantic analysis level; character width differences
     * are handled later during code generation. */
    element_type = CHAR_TYPE;
    if (base_kgpc_type != NULL && kgpc_type_is_wide_string(base_kgpc_type)) {
      if (expr->array_element_type_id == NULL)
        expr->array_element_type_id = strdup("WideChar");
      expr->array_element_size = 2;
    } else {
      if (expr->array_element_type_id == NULL)
        expr->array_element_type_id = strdup("AnsiChar");
      expr->array_element_size = 1;
    }
  } else if (base_is_pointer) {
    /* Pointer indexing: p[i] is equivalent to (p+i)^ */
    /* Get the type that the pointer points to */
    element_type = array_expr->pointer_subtype;

    if (element_type == UNKNOWN_TYPE &&
        array_expr->pointer_subtype_id != NULL) {
      int resolved_type = UNKNOWN_TYPE;
      if (resolve_type_identifier(&resolved_type, symtab,
                                  array_expr->pointer_subtype_id,
                                  expr->line_num) == 0)
        element_type = resolved_type;
    }

    /* Fallback: resolve element type from KgpcType (e.g. PPAnsiChar^[i]) */
    if (element_type == UNKNOWN_TYPE && base_kgpc_type != NULL &&
        kgpc_type_is_pointer(base_kgpc_type) &&
        base_kgpc_type->info.points_to != NULL) {
      KgpcType *points_to = base_kgpc_type->info.points_to;
      if (kgpc_type_is_char(points_to)) {
        element_type = CHAR_TYPE;
        if (expr->array_element_type_id == NULL) {
          const char *char_type_id = "AnsiChar";
          struct TypeAlias *alias = kgpc_type_get_type_alias(points_to);
          if (alias != NULL) {
            if ((alias->alias_name != NULL &&
                 (pascal_identifier_equals(alias->alias_name, "WideChar") ||
                  pascal_identifier_equals(alias->alias_name,
                                           "UnicodeChar"))) ||
                (alias->target_type_id != NULL &&
                 (pascal_identifier_equals(alias->target_type_id, "WideChar") ||
                  pascal_identifier_equals(alias->target_type_id,
                                           "UnicodeChar"))))
              char_type_id = "WideChar";
          } else if (kgpc_type_sizeof(points_to) == 2) {
            char_type_id = "WideChar";
          }
          expr->array_element_type_id = strdup(char_type_id);
        }
        expr->array_element_size = (kgpc_type_sizeof(points_to) == 2) ? 2 : 1;
      } else if (kgpc_type_is_integer(points_to))
        element_type = kgpc_type_get_primitive_tag(points_to);
      else if (kgpc_type_is_pointer(points_to)) {
        element_type = POINTER_TYPE;
        /* Propagate pointer subtype info so subsequent dereferences
         * or indexing can resolve the correct element type
         * (e.g. for PPAnsiChar^[i], the sub_tag is CHAR_TYPE) */
        if (points_to->info.points_to != NULL) {
          int sub_tag = kgpc_type_get_primitive_tag(points_to->info.points_to);
          if (sub_tag != UNKNOWN_TYPE)
            expr->pointer_subtype = sub_tag;
        }
      } else if (kgpc_type_is_record(points_to)) {
        element_type = RECORD_TYPE;
        expr->array_element_record_type = kgpc_type_get_record(points_to);
        if (expr->array_element_record_type != NULL &&
            expr->array_element_record_type->type_id != NULL &&
            expr->array_element_type_id == NULL) {
          expr->array_element_type_id =
              strdup(expr->array_element_record_type->type_id);
        }
      }
    }

    if (element_type == RECORD_TYPE &&
        expr->array_element_record_type == NULL &&
        array_expr->pointer_subtype_id != NULL) {
      expr->array_element_record_type =
          semcheck_lookup_record_type(symtab, array_expr->pointer_subtype_id);
      if (expr->array_element_record_type != NULL &&
          expr->array_element_type_id == NULL)
        expr->array_element_type_id = strdup(array_expr->pointer_subtype_id);
    }

    /* Copy pointer target type info to result */
    if (element_type == POINTER_TYPE &&
        array_expr->pointer_subtype_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, symtab, array_expr->pointer_subtype_id) != 0 &&
          type_node != NULL) {
        struct TypeAlias *alias = get_type_alias_from_node(type_node);
        if (alias != NULL && alias->is_pointer) {
          expr->pointer_subtype = alias->pointer_type;
          if (alias->pointer_type_id != NULL)
            expr->pointer_subtype_id = strdup(alias->pointer_type_id);
        }
      }
    }
  } else {
    element_type = array_expr->array_element_type;
    if (element_type == UNKNOWN_TYPE &&
        array_expr->array_element_type_id != NULL) {
      int resolved_type = UNKNOWN_TYPE;
      if (resolve_type_identifier(&resolved_type, symtab,
                                  array_expr->array_element_type_id,
                                  expr->line_num) == 0)
        element_type = resolved_type;
    }
    if (element_type == UNKNOWN_TYPE &&
        array_expr->array_element_record_type != NULL)
      element_type = RECORD_TYPE;
    if (element_type == ARRAY_OF_CONST_TYPE) {
      element_type = RECORD_TYPE;
    }

    if (array_expr->array_element_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, symtab, array_expr->array_element_type_id) !=
              0 &&
          type_node != NULL) {
        struct TypeAlias *alias = get_type_alias_from_node(type_node);
        if (alias != NULL && alias->is_array) {
          semcheck_set_array_info_from_alias(expr, symtab, alias,
                                             expr->line_num);
        }
      }
    }

    if (element_type == POINTER_TYPE) {
      int pointer_subtype = UNKNOWN_TYPE;
      const char *pointer_subtype_id = NULL;

      if (array_expr->array_element_type_id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, array_expr->array_element_type_id) !=
                0 &&
            type_node != NULL) {
          struct TypeAlias *alias = get_type_alias_from_node(type_node);
          if (alias != NULL && alias->is_pointer) {
            pointer_subtype = alias->pointer_type;
            pointer_subtype_id = alias->pointer_type_id;
          }
        }
      }

      if (pointer_subtype == UNKNOWN_TYPE && pointer_subtype_id == NULL &&
          array_expr->array_element_type_id != NULL &&
          (array_expr->array_element_type_id[0] == 'P' ||
           array_expr->array_element_type_id[0] == 'p') &&
          array_expr->array_element_type_id[1] != '\0') {
        const char *candidate_type_id = array_expr->array_element_type_id + 1;
        HashNode_t *candidate_node = NULL;
        if (FindSymbol(&candidate_node, symtab, candidate_type_id) != 0 &&
            candidate_node != NULL) {
          pointer_subtype_id = candidate_type_id;
          set_type_from_hashtype(&pointer_subtype, candidate_node);
        }
        if (pointer_subtype == UNKNOWN_TYPE) {
          int mapped =
              semcheck_map_builtin_type_name(symtab, candidate_type_id);
          if (mapped != UNKNOWN_TYPE) {
            pointer_subtype_id = candidate_type_id;
            pointer_subtype = mapped;
          }
        }
      }

      if (pointer_subtype == UNKNOWN_TYPE &&
          array_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_array(array_expr->resolved_kgpc_type)) {
        KgpcType *elem_type =
            kgpc_type_get_array_element_type(array_expr->resolved_kgpc_type);
        if (elem_type != NULL && elem_type->kind == TYPE_KIND_POINTER) {
          int mapped = kgpc_type_get_pointer_subtype_tag(elem_type);
          if (mapped != UNKNOWN_TYPE)
            pointer_subtype = mapped;
        }
      }

      semcheck_set_pointer_info(expr, pointer_subtype, pointer_subtype_id);
    }
  }

  KgpcType *index_kgpc_type = NULL;
  return_val += semcheck_expr_with_type(&index_kgpc_type, symtab, access_expr,
                                        max_scope_lev, NO_MUTATE);
  index_type = semcheck_tag_from_kgpc(index_kgpc_type);
  if (!is_ordinal_type(index_type) && index_type != UNKNOWN_TYPE) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, expected ordinal type (integer, char, boolean, or "
        "enum) in array index expression!\n\n",
        expr->line_num);
    ++return_val;
  }

  /* Type-check extra indices for multi-dimensional arrays */
  if (expr->expr_data.array_access_data.extra_indices != NULL) {
    if (kgpc_getenv("KGPC_DEBUG_ARRAY_LINEAR") != NULL) {
      fprintf(stderr, "[SemCheck] array access has extra indices\n");
    }
    ListNode_t *extra_idx = expr->expr_data.array_access_data.extra_indices;
    while (extra_idx != NULL) {
      struct Expression *idx_expr = (struct Expression *)extra_idx->cur;
      if (idx_expr != NULL) {
        int extra_idx_type = UNKNOWN_TYPE;
        KgpcType *extra_kgpc_type = NULL;
        return_val += semcheck_expr_with_type(
            &extra_kgpc_type, symtab, idx_expr, max_scope_lev, NO_MUTATE);
        extra_idx_type = semcheck_tag_from_kgpc(extra_kgpc_type);
        if (!is_ordinal_type(extra_idx_type) &&
            extra_idx_type != UNKNOWN_TYPE) {
          semcheck_error_with_context_at(
              expr->line_num, expr->col_num, expr->source_index,
              "Error on line %d, expected ordinal type (integer, char, "
              "boolean, or enum) in array index expression!\n\n",
              expr->line_num);
          ++return_val;
        }
      }
      extra_idx = extra_idx->next;
    }
  }

  /* Propagate resolved KgpcType to the result of the indexing expression */
  KgpcType *res_type = NULL;
  KgpcType *base_array_type = array_expr->resolved_kgpc_type;
  /* Pointer-to-array base (`p[i]` == `p^[i]`): resolve through the pointer so
   * the element keeps its full KgpcType.  Without this the element of a
   * pointer-to-array-of-pointers (e.g. `fbitmap[x1]` : Pinterferencebitmap2)
   * falls back to a bare primitive(POINTER), dropping the pointee, which then
   * breaks a subsequent dereference `fbitmap[x1,y1]^[k]`. */
  if (base_array_type != NULL && base_array_type->kind == TYPE_KIND_POINTER) {
    KgpcType *base_pointee =
        kgpc_type_resolve_pointer_pointee(base_array_type, symtab);
    if (base_pointee != NULL && kgpc_type_is_array(base_pointee))
      base_array_type = base_pointee;
  }
  if (base_array_type != NULL && kgpc_type_is_array(base_array_type)) {
    res_type = kgpc_type_get_array_element_type(base_array_type);
    if (res_type != NULL)
      kgpc_type_retain(res_type);

    /* Apply extra indices for multi-dimensional arrays */
    if (expr->expr_data.array_access_data.extra_indices != NULL) {
      ListNode_t *extra_idx = expr->expr_data.array_access_data.extra_indices;
      while (extra_idx != NULL && res_type != NULL &&
             kgpc_type_is_array(res_type)) {
        KgpcType *next = kgpc_type_get_array_element_type(res_type);
        if (next != NULL)
          kgpc_type_retain(next);
        kgpc_type_release(res_type);
        res_type = next;
        extra_idx = extra_idx->next;
      }
    }
  }

  if (res_type != NULL)
    element_type = semcheck_tag_from_kgpc(res_type);

  /* If the element type is itself an array (nested dynamic arrays), propagate
   * array info */
  if (res_type != NULL && kgpc_type_is_array(res_type)) {
    expr->is_array_expr = 1;
    expr->array_lower_bound = res_type->info.array_info.start_index;
    expr->array_upper_bound = res_type->info.array_info.end_index;
    expr->array_is_dynamic = kgpc_type_is_dynamic_array(res_type);
    KgpcType *inner_elem =
        kgpc_type_get_array_element_type_resolved(res_type, symtab);
    if (inner_elem != NULL) {
      expr->array_element_type = semcheck_tag_from_kgpc(inner_elem);
      if (kgpc_type_is_record(inner_elem)) {
        expr->array_element_record_type = kgpc_type_get_record(inner_elem);
        /* Compute element size from the record */
        if (expr->array_element_record_type != NULL) {
          long long computed_size = 0;
          if (sizeof_from_record(symtab, expr->array_element_record_type,
                                 &computed_size, 0, expr->line_num) == 0 &&
              computed_size > 0 && computed_size <= INT_MAX) {
            expr->array_element_size = (int)computed_size;
          }
        }
      } else {
        /* Compute element size from KgpcType */
        long long sz = kgpc_type_sizeof(inner_elem);
        if (sz > 0 && sz <= INT_MAX)
          expr->array_element_size = (int)sz;
      }
    }
    element_type = POINTER_TYPE; /* Dynamic arrays are pointers at runtime */
  }

  if (element_type == UNKNOWN_TYPE)
    element_type = LONGINT_TYPE;

  *type_return = element_type;

  if (res_type == NULL) {
    if (element_type == RECORD_TYPE) {
      struct RecordType *fallback_record = NULL;
      if (expr->array_element_record_type != NULL)
        fallback_record = expr->array_element_record_type;
      if (array_expr->array_element_record_type != NULL)
        fallback_record = array_expr->array_element_record_type;
      else if (array_expr->array_element_type_id != NULL)
        fallback_record = semcheck_lookup_record_type(
            symtab, array_expr->array_element_type_id);
      else if (array_expr->array_element_type == ARRAY_OF_CONST_TYPE)
        fallback_record = semcheck_lookup_record_type(symtab, "TVarRec");
      if (fallback_record != NULL)
        res_type = create_record_type(fallback_record);
    }
    if (res_type == NULL) {
      int is_wide_char_elem =
          (element_type == CHAR_TYPE && expr->array_element_type_id != NULL &&
           (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
            pascal_identifier_equals(expr->array_element_type_id,
                                     "UnicodeChar")));
      if (is_wide_char_elem) {
        res_type = create_primitive_type_with_size(CHAR_TYPE, 2);
        if (res_type != NULL) {
          HashNode_t *wide_node = NULL;
          if (FindSymbol(&wide_node, symtab, "WideChar") != 0 &&
              wide_node != NULL) {
            struct TypeAlias *wide_alias = get_type_alias_from_node(wide_node);
            if (wide_alias != NULL)
              kgpc_type_set_type_alias(res_type, wide_alias);
          }
        }
      } else {
        res_type = create_primitive_type(element_type);
      }
    }
  }

  if (expr->resolved_kgpc_type != NULL)
    destroy_kgpc_type(expr->resolved_kgpc_type);
  expr->resolved_kgpc_type = res_type;

  semcheck_compute_array_linearization(symtab, expr, array_expr);

  return return_val;
}

/* Helper to resolve the actual type tag from a TREE_VAR_DECL parameter
 * declaration */
int semcheck_try_indexed_property_getter(int *type_return, SymTab_t *symtab,
                                         struct Expression *expr,
                                         int max_scope_lev, int mutating) {
  if (type_return == NULL || symtab == NULL || expr == NULL)
    return -1;
  if (mutating != NO_MUTATE)
    return -1;
  if (expr->type != EXPR_ARRAY_ACCESS)
    return -1;

  struct Expression *array_expr = expr->expr_data.array_access_data.array_expr;
  struct Expression *index_expr = expr->expr_data.array_access_data.index_expr;
  if (array_expr == NULL)
    return -1;

  const char *base_id = NULL;
  if (array_expr->type == EXPR_VAR_ID)
    base_id = array_expr->expr_data.id;
  else if (array_expr->type == EXPR_FUNCTION_CALL &&
           array_expr->expr_data.function_call_data.args_expr == NULL)
    base_id = array_expr->expr_data.function_call_data.id;

  if (base_id == NULL && array_expr->type == EXPR_RECORD_ACCESS) {
    struct Expression *record_expr =
        array_expr->expr_data.record_access_data.record_expr;
    const char *field_id = array_expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL || index_expr == NULL)
      return -1;

    KgpcType *record_type = NULL;
    semcheck_expr_with_type(&record_type, symtab, record_expr, max_scope_lev,
                            mutating);

    struct RecordType *record_info = NULL;
    if (record_type != NULL && kgpc_type_is_pointer(record_type) &&
        record_type->info.points_to != NULL &&
        kgpc_type_is_record(record_type->info.points_to)) {
      record_info = kgpc_type_get_record(record_type->info.points_to);
    } else if (record_type != NULL && kgpc_type_is_record(record_type)) {
      record_info = kgpc_type_get_record(record_type);
    }
    if (record_info == NULL)
      return -1;

    struct RecordType *property_owner = NULL;
    struct ClassProperty *property = semcheck_find_class_property(
        symtab, record_info, field_id, &property_owner);
    if (property == NULL || property->read_accessor == NULL ||
        !property->is_indexed)
      return -1;

    /* If property read accessor is a field, rewrite and let array access
     * proceed normally. */
    struct RecordField *read_field = semcheck_find_class_field_including_hidden(
        symtab, record_info, property->read_accessor, NULL);
    if (read_field != NULL) {
      if (!pascal_identifier_equals(field_id, property->read_accessor)) {
        free(array_expr->expr_data.record_access_data.field_id);
        array_expr->expr_data.record_access_data.field_id =
            strdup(property->read_accessor);
        if (array_expr->expr_data.record_access_data.field_id == NULL)
          return -1;
      }
      return -1;
    }

    HashNode_t *getter_node = semcheck_find_class_method(
        symtab, property_owner != NULL ? property_owner : record_info,
        property->read_accessor, NULL);
    if (getter_node == NULL)
      return -1;

    int is_static_getter = 0;
    if (property_owner != NULL && property_owner->type_id != NULL &&
        getter_node->id != NULL) {
      is_static_getter = from_cparser_is_method_static(property_owner->type_id,
                                                       getter_node->id);
    }
    if (!is_static_getter && getter_node->type != NULL &&
        getter_node->type->kind == TYPE_KIND_PROCEDURE) {
      ListNode_t *params = kgpc_type_get_procedure_params(getter_node->type);
      if (params == NULL)
        is_static_getter = 1;
    }

    /* Save extra_indices before transformation clears array_access_data */
    ListNode_t *extra_indices = expr->expr_data.array_access_data.extra_indices;
    expr->expr_data.array_access_data.extra_indices = NULL;

    /* Detach record_expr from array_expr before destroying it. */
    array_expr->expr_data.record_access_data.record_expr = NULL;
    destroy_expr(array_expr);
    expr->expr_data.array_access_data.array_expr = NULL;
    expr->expr_data.array_access_data.index_expr = NULL;

    ListNode_t *args_head = NULL;
    ListNode_t *args_tail = NULL;
    if (!is_static_getter) {
      args_head = CreateListNode(record_expr, LIST_EXPR);
      if (args_head == NULL)
        return -1;
      args_tail = args_head;
    } else {
      destroy_expr(record_expr);
    }

    ListNode_t *index_node = CreateListNode(index_expr, LIST_EXPR);
    if (index_node == NULL)
      return -1;
    if (args_tail != NULL)
      args_tail->next = index_node;
    else
      args_head = index_node;
    args_tail = index_node;

    /* Append extra indices for multi-index properties (e.g. bitmap[x,y]) */
    while (extra_indices != NULL) {
      ListNode_t *next = extra_indices->next;
      extra_indices->next = NULL;
      args_tail->next = extra_indices;
      args_tail = extra_indices;
      extra_indices = next;
    }

    char *id_copy = getter_node->id != NULL ? strdup(getter_node->id) : NULL;
    char *mangled_copy = NULL;
    if (getter_node->mangled_id != NULL)
      mangled_copy = strdup(getter_node->mangled_id);

    if ((getter_node->id != NULL && id_copy == NULL) ||
        (getter_node->mangled_id != NULL && mangled_copy == NULL)) {
      free(id_copy);
      free(mangled_copy);
      return -1;
    }

    expr->type = EXPR_FUNCTION_CALL;
    memset(&expr->expr_data.function_call_data, 0,
           sizeof(expr->expr_data.function_call_data));
    expr->expr_data.function_call_data.id = id_copy;
    expr->expr_data.function_call_data.mangled_id = mangled_copy;
    expr->expr_data.function_call_data.args_expr = args_head;
    /* Set resolved_func so semcheck_funccall goes via method_call_resolved
     * (which semchecks arguments) rather than the funccall_cleanup fast-path
     * (which skips argument semchecking, leaving e.g. FCount as EXPR_VAR_ID).
     */
    expr->expr_data.function_call_data.resolved_func = getter_node;
    expr->expr_data.function_call_data.call_hash_type = getter_node->hash_type;
    semcheck_expr_set_call_kgpc_type(expr, getter_node->type, 0);
    expr->expr_data.function_call_data.is_call_info_valid = 1;
    semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    expr->array_element_type_id = NULL;
    expr->array_element_record_type = NULL;
    expr->array_element_size = 0;

    return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev,
                                    mutating);
  }

  if (base_id != NULL && index_expr != NULL) {
    HashNode_t *self_node = NULL;
    if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL) {
      struct RecordType *self_record = get_record_type_from_node(self_node);
      if (self_record != NULL) {
        struct RecordType *property_owner = NULL;
        struct ClassProperty *property = semcheck_find_class_property(
            symtab, self_record, base_id, &property_owner);
        if (property != NULL && property->read_accessor != NULL &&
            property->is_indexed) {
          HashNode_t *getter_node = semcheck_find_class_method(
              symtab, property_owner != NULL ? property_owner : self_record,
              property->read_accessor, NULL);
          if (getter_node != NULL) {
            int is_static_getter = 0;
            if (property_owner != NULL && property_owner->type_id != NULL &&
                getter_node->id != NULL) {
              is_static_getter = from_cparser_is_method_static(
                  property_owner->type_id, getter_node->id);
            }
            if (!is_static_getter && getter_node->type != NULL &&
                getter_node->type->kind == TYPE_KIND_PROCEDURE) {
              ListNode_t *params =
                  kgpc_type_get_procedure_params(getter_node->type);
              if (params == NULL)
                is_static_getter = 1;
            }

            struct Expression *self_expr = NULL;
            if (!is_static_getter)
              self_expr = mk_varid(expr->line_num, strdup("Self"));

            destroy_expr(array_expr);
            expr->expr_data.array_access_data.array_expr = NULL;
            expr->expr_data.array_access_data.index_expr = NULL;

            ListNode_t *args_head = NULL;
            ListNode_t *args_tail = NULL;
            if (!is_static_getter) {
              if (self_expr == NULL)
                return -1;
              args_head = CreateListNode(self_expr, LIST_EXPR);
              if (args_head == NULL)
                return -1;
              args_tail = args_head;
            }

            ListNode_t *index_node = CreateListNode(index_expr, LIST_EXPR);
            if (index_node == NULL)
              return -1;
            if (args_tail != NULL)
              args_tail->next = index_node;
            else
              args_head = index_node;

            char *id_copy =
                getter_node->id != NULL ? strdup(getter_node->id) : NULL;
            char *mangled_copy = NULL;
            if (getter_node->mangled_id != NULL)
              mangled_copy = strdup(getter_node->mangled_id);

            if ((getter_node->id != NULL && id_copy == NULL) ||
                (getter_node->mangled_id != NULL && mangled_copy == NULL)) {
              free(id_copy);
              free(mangled_copy);
              return -1;
            }

            expr->type = EXPR_FUNCTION_CALL;
            memset(&expr->expr_data.function_call_data, 0,
                   sizeof(expr->expr_data.function_call_data));
            expr->expr_data.function_call_data.id = id_copy;
            expr->expr_data.function_call_data.mangled_id = mangled_copy;
            expr->expr_data.function_call_data.args_expr = args_head;
            /* Set resolved_func so semcheck_funccall goes via
             * method_call_resolved (which semchecks arguments) rather than the
             * funccall_cleanup fast-path (which skips argument semchecking,
             * leaving e.g. FCount as EXPR_VAR_ID). */
            expr->expr_data.function_call_data.resolved_func = getter_node;
            expr->expr_data.function_call_data.call_hash_type =
                getter_node->hash_type;
            semcheck_expr_set_call_kgpc_type(expr, getter_node->type, 0);
            expr->expr_data.function_call_data.is_call_info_valid = 1;
            semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
            expr->is_array_expr = 0;
            expr->array_element_type = UNKNOWN_TYPE;
            expr->array_element_type_id = NULL;
            expr->array_element_record_type = NULL;
            expr->array_element_size = 0;

            return semcheck_expr_legacy_tag(type_return, symtab, expr,
                                            max_scope_lev, mutating);
          }
        }
      }
    }
  }

  if (base_id == NULL || index_expr == NULL)
    return -1;

  size_t id_len = strlen(base_id);
  char *getter_id = (char *)malloc(id_len + 4);
  if (getter_id == NULL)
    return -1;
  snprintf(getter_id, id_len + 4, "Get%s", base_id);

  HashNode_t *getter_node = NULL;
  int getter_found = (FindSymbol(&getter_node, symtab, getter_id) != 0);
  if (!getter_found || getter_node == NULL ||
      getter_node->hash_type != HASHTYPE_FUNCTION) {
    free(getter_id);
    return -1;
  }

  ListNode_t *args = CreateListNode(index_expr, LIST_EXPR);
  if (args == NULL) {
    free(getter_id);
    return -1;
  }

  expr->type = EXPR_FUNCTION_CALL;
  memset(&expr->expr_data.function_call_data, 0,
         sizeof(expr->expr_data.function_call_data));
  expr->expr_data.function_call_data.id = getter_id;
  expr->expr_data.function_call_data.args_expr = args;
  expr->expr_data.function_call_data.mangled_id = NULL;
  semcheck_reset_function_call_cache(expr);

  destroy_expr(array_expr);

  return semcheck_funccall(type_return, symtab, expr, max_scope_lev, mutating);
}
