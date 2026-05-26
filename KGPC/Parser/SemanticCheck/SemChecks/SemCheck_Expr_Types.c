/*
    SemCheck_Expr_Types.c - Type cast and access expression semantic checks

    This file contains semantic checking for:
    - Type casts (typecast expression)
    - IS expression (type checking)
    - AS expression (type conversion)
    - Pointer dereference (^)
    - Record/class field access (record.field)
    - Property access and getter/setter transformation

    Part of the SemCheck module split from SemCheck_expr.c.
*/

#include "SemCheck_Expr_Internal.h"
#include "unit_registry.h"
#include "../../ErrVars.h"

int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab,
                                         const char *type_name,
                                         const char *literal_name,
                                         long long *out_value);
int semcheck_resolve_scoped_enum_literal_ref(SymTab_t *symtab,
                                             const QualifiedIdent *type_ref,
                                             const char *literal_name,
                                             long long *out_value);

static char *semcheck_join_qualified_prefix(const QualifiedIdent *name) {
  if (name == NULL || name->segments == NULL || name->count <= 1)
    return NULL;
  size_t total = 0;
  for (int i = 0; i < name->count - 1; ++i) {
    if (name->segments[i] != NULL)
      total += strlen(name->segments[i]);
    if (i + 1 < name->count - 1)
      total += 1;
  }
  char *out = (char *)malloc(total + 1);
  if (out == NULL)
    return NULL;
  out[0] = '\0';
  for (int i = 0; i < name->count - 1; ++i) {
    if (name->segments[i] != NULL)
      strcat(out, name->segments[i]);
    if (i + 1 < name->count - 1)
      strcat(out, ".");
  }
  return out;
}

char *build_qualified_identifier_from_expr_local(struct Expression *expr) {
  if (expr == NULL)
    return NULL;
  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
    return strdup(expr->expr_data.id);
  if (expr->type != EXPR_RECORD_ACCESS)
    return NULL;

  struct Expression *record_expr =
      expr->expr_data.record_access_data.record_expr;
  char *field_id = expr->expr_data.record_access_data.field_id;
  if (record_expr == NULL || field_id == NULL)
    return NULL;

  char *base = build_qualified_identifier_from_expr_local(record_expr);
  if (base == NULL)
    return NULL;
  size_t qualified_len = strlen(base) + 1 + strlen(field_id) + 1;
  char *qualified = (char *)malloc(qualified_len);
  if (qualified != NULL)
    snprintf(qualified, qualified_len, "%s.%s", base, field_id);
  free(base);
  return qualified;
}

int semcheck_has_value_ident(SymTab_t *symtab, const char *id) {
  if (symtab == NULL || id == NULL)
    return 0;
  const char *trace_nonlocal = kgpc_getenv("KGPC_TRACE_NONLOCAL");
  int trace_id = (trace_nonlocal != NULL && id != NULL &&
                  (strcmp(trace_nonlocal, "1") == 0 ||
                   pascal_identifier_equals(id, trace_nonlocal)));

  for (ScopeNode *cur_scope = symtab->current_scope; cur_scope != NULL;
       cur_scope = cur_scope->parent) {
    HashTable_t *table = cur_scope->table;
    ListNode_t *matches = FindAllIdentsInTable(table, id);
    if (matches != NULL) {
      for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type != HASHTYPE_TYPE &&
            node->hash_type != HASHTYPE_FUNCTION &&
            node->hash_type != HASHTYPE_PROCEDURE &&
            node->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
          if (trace_id) {
            fprintf(stderr,
                    "[KGPC_TRACE_NONLOCAL] semcheck_has_value_ident id=%s "
                    "hit=%s hash=%d unit=%d\n",
                    id, node->id != NULL ? node->id : "<null>", node->hash_type,
                    node->source_unit_index);
          }
          DestroyList(matches);
          return 1;
        }
      }
      DestroyList(matches);
    }
  }

  ListNode_t *builtin_matches =
      FindAllIdentsInTable(symtab->builtin_scope->table, id);
  if (builtin_matches != NULL) {
    for (ListNode_t *cur = builtin_matches; cur != NULL; cur = cur->next) {
      HashNode_t *node = (HashNode_t *)cur->cur;
      if (node != NULL && node->hash_type != HASHTYPE_TYPE &&
          node->hash_type != HASHTYPE_FUNCTION &&
          node->hash_type != HASHTYPE_PROCEDURE &&
          node->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
        if (trace_id) {
          fprintf(stderr,
                  "[KGPC_TRACE_NONLOCAL] semcheck_has_value_ident builtin "
                  "id=%s hit=%s hash=%d unit=%d\n",
                  id, node->id != NULL ? node->id : "<null>", node->hash_type,
                  node->source_unit_index);
        }
        DestroyList(builtin_matches);
        return 1;
      }
    }
    DestroyList(builtin_matches);
  }

  /* Check if id is a field of Self (implicit class field access inside
   * methods). This prevents unit-name resolution from shadowing class fields
   * when a unit name collides with a field name (e.g., field "symtable" vs unit
   * "symtable"). */
  {
    HashNode_t *self_node = NULL;
    if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL) {
      struct RecordType *self_record = get_record_type_from_node(self_node);
      if (self_record != NULL) {
        struct RecordField *field = NULL;
        long long offset = 0;
        if (resolve_record_field(symtab, self_record, id, &field, &offset, 0,
                                 1) == 0 &&
            field != NULL)
          return 1;
      }
    }
  }

  return 0;
}

KgpcType *semcheck_create_value_kgpc_type_for_record_local(
    struct RecordType *record_info) {
  if (record_info == NULL)
    return NULL;

  KgpcType *record_kgpc = create_record_type(record_info);
  if (record_kgpc == NULL)
    return NULL;

  if (!record_type_is_class(record_info) && !record_info->is_interface)
    return record_kgpc;

  KgpcType *ptr_type = create_pointer_type(record_kgpc);
  destroy_kgpc_type(record_kgpc);
  return ptr_type;
}

KgpcType *
semcheck_create_value_kgpc_type_from_node_local(HashNode_t *type_node) {
  if (type_node == NULL || type_node->type == NULL)
    return NULL;

  KgpcType *node_type = type_node->type;
  if (kgpc_type_is_record(node_type)) {
    struct RecordType *record_info = kgpc_type_get_record(node_type);
    if (record_info != NULL &&
        (record_type_is_class(record_info) || record_info->is_interface)) {
      return semcheck_create_value_kgpc_type_for_record_local(record_info);
    }
  }

  kgpc_type_retain(node_type);
  return node_type;
}

int semcheck_type_alias_has_enum_literal(const struct TypeAlias *alias,
                                         const char *field_id) {
  if (alias == NULL || field_id == NULL || !alias->is_enum ||
      alias->enum_literals == NULL)
    return 0;
  for (ListNode_t *literal_node = alias->enum_literals; literal_node != NULL;
       literal_node = literal_node->next) {
    const char *literal_name = (const char *)literal_node->cur;
    if (literal_name != NULL &&
        pascal_identifier_equals(literal_name, field_id))
      return 1;
  }
  return 0;
}

static int g_semcheck_alias_target_probe_depth = 0;

HashNode_t *
semcheck_find_exact_qualified_type_node(SymTab_t *symtab,
                                        const QualifiedIdent *type_ref) {
  if (symtab == NULL || type_ref == NULL || type_ref->count <= 0)
    return NULL;

  HashNode_t *type_node = NULL;
  char *qualified = qualified_ident_join(type_ref, ".");
  if (qualified != NULL) {
    if (FindSymbol(&type_node, symtab, qualified) != 0 && type_node != NULL &&
        type_node->hash_type == HASHTYPE_TYPE) {
      free(qualified);
      return type_node;
    }
    type_node = semcheck_find_preferred_type_node(symtab, qualified);
    if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
      free(qualified);
      return type_node;
    }
    free(qualified);
  }

  if (type_ref->count > 1 && type_ref->segments != NULL &&
      type_ref->segments[0] != NULL) {
    const char *unit_name = type_ref->segments[0];
    const char *base_name = qualified_ident_last(type_ref);
    if (base_name != NULL) {
      ListNode_t *matches = FindAllIdents(symtab, base_name);
      HashNode_t *best = NULL;
      for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
          continue;
        const char *candidate_unit =
            unit_registry_get(candidate->source_unit_index);
        if (candidate_unit != NULL &&
            pascal_identifier_equals(candidate_unit, unit_name)) {
          best = candidate;
          if (candidate->defined_in_unit)
            break;
        }
      }
      if (matches != NULL)
        DestroyList(matches);
      if (best != NULL)
        return best;
    }
  }

  TypeRef temp_ref = {0};
  temp_ref.name = (QualifiedIdent *)type_ref;
  return semcheck_find_preferred_type_node_with_ref(symtab, &temp_ref, NULL);
}

HashNode_t *semcheck_find_exact_type_node_for_ref(SymTab_t *symtab,
                                                  const TypeRef *type_ref,
                                                  const char *type_id,
                                                  const char *field_id) {
  if (symtab == NULL)
    return NULL;

  if (type_ref != NULL && type_ref->name != NULL && type_ref->name->count > 1 &&
      type_ref->name->segments != NULL && type_ref->name->segments[0] != NULL) {
    HashNode_t *qualified_node =
        semcheck_find_exact_qualified_type_node(symtab, type_ref->name);
    if (qualified_node != NULL)
      return qualified_node;

    const char *unit_name = type_ref->name->segments[0];
    const char *base_name = qualified_ident_last(type_ref->name);
    if (base_name != NULL) {
      ListNode_t *matches = FindAllIdents(symtab, base_name);
      HashNode_t *best = NULL;
      for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *candidate = (HashNode_t *)cur->cur;
        if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
          continue;
        const char *candidate_unit =
            unit_registry_get(candidate->source_unit_index);
        if (candidate_unit != NULL &&
            pascal_identifier_equals(candidate_unit, unit_name)) {
          best = candidate;
          if (candidate->defined_in_unit)
            break;
        } else if (candidate->defined_in_unit) {
          struct TypeAlias *candidate_alias =
              hashnode_get_type_alias(candidate);
          if (semcheck_type_alias_has_enum_literal(candidate_alias, field_id)) {
            best = candidate;
            break;
          }
          if (best == NULL) {
            /* Qualified refs should never prefer a local shadow alias
             * over a unit-defined type when source-unit metadata is
             * missing on the imported node. */
            best = candidate;
          }
        }
      }
      if (matches != NULL)
        DestroyList(matches);
      if (best != NULL)
        return best;
    }
  }

  return semcheck_find_preferred_type_node_with_ref(symtab, type_ref, type_id);
}

static int semcheck_try_resolve_enum_literal_from_type_alias_depth(
    SymTab_t *symtab, const struct TypeAlias *type_alias, const char *field_id,
    long long *out_value, int depth) {
  if (symtab == NULL || type_alias == NULL || field_id == NULL ||
      out_value == NULL)
    return 0;
  if (depth > 8)
    return 0;

  if (type_alias->is_enum && type_alias->enum_literals != NULL) {
    int ordinal = 0;
    for (ListNode_t *literal_node = type_alias->enum_literals;
         literal_node != NULL; literal_node = literal_node->next, ++ordinal) {
      const char *literal_name = (const char *)literal_node->cur;
      if (literal_name != NULL &&
          pascal_identifier_equals(literal_name, field_id)) {
        *out_value = ordinal;
        return 1;
      }
    }
  }

  HashNode_t *target_node = NULL;
  if (type_alias->target_type_ref != NULL &&
      type_alias->target_type_ref->name != NULL) {
    char *qualified_target =
        type_ref_render_source(type_alias->target_type_ref);
    if (qualified_target != NULL) {
      if (semcheck_resolve_scoped_enum_literal(symtab, qualified_target,
                                               field_id, out_value)) {
        free(qualified_target);
        return 1;
      }
      free(qualified_target);
    }
    target_node = semcheck_find_exact_type_node_for_ref(
        symtab, type_alias->target_type_ref, type_alias->target_type_id,
        field_id);
  } else if (type_alias->target_type_id != NULL) {
    target_node =
        semcheck_find_preferred_type_node(symtab, type_alias->target_type_id);
  }

  if (target_node != NULL && target_node->type != NULL) {
    struct TypeAlias *target_alias = hashnode_get_type_alias(target_node);
    if (target_alias != NULL &&
        semcheck_try_resolve_enum_literal_from_type_alias_depth(
            symtab, target_alias, field_id, out_value, depth + 1)) {
      return 1;
    }
  }

  if (type_alias->target_type_ref != NULL &&
      type_alias->target_type_ref->name != NULL &&
      semcheck_resolve_scoped_enum_literal_ref(
          symtab, type_alias->target_type_ref->name, field_id, out_value)) {
    return 1;
  }

  if (type_alias->target_type_id != NULL &&
      semcheck_resolve_scoped_enum_literal(symtab, type_alias->target_type_id,
                                           field_id, out_value)) {
    return 1;
  }

  return 0;
}

int semcheck_try_resolve_enum_literal_from_type_alias(
    SymTab_t *symtab, const struct TypeAlias *type_alias, const char *field_id,
    long long *out_value) {
  return semcheck_try_resolve_enum_literal_from_type_alias_depth(
      symtab, type_alias, field_id, out_value, 0);
}

/*
 * Preference tier for enum type candidates.
 * When the same enum type name exists in multiple units, we prefer:
 *   1. The definition from the current compilation unit (most local)
 *   2. A definition imported from any unit (defined_in_unit flag)
 *   3. A definition from program scope (source_unit_index == 0)
 *
 * Lower tier value = better candidate.
 */
typedef enum {
  ENUM_PREF_SAME_UNIT = 0, /* Best: defined in the current unit */
  ENUM_PREF_IMPORTED_UNIT, /* Good: imported from another unit */
  ENUM_PREF_PROGRAM_SCOPE, /* Acceptable: in program scope */
  ENUM_PREF_OTHER          /* Fallback */
} EnumCandidatePreference;

static EnumCandidatePreference
semcheck_enum_candidate_preference(SymTab_t *symtab, HashNode_t *candidate) {
  int cur_unit_idx =
      (symtab->current_scope != NULL) ? symtab->current_scope->unit_index : 0;

  /* Same unit as the current scope — highest priority. */
  if (cur_unit_idx > 0 && candidate->source_unit_index == cur_unit_idx)
    return ENUM_PREF_SAME_UNIT;

  /* Imported from a named unit — second priority. */
  if (candidate->defined_in_unit)
    return ENUM_PREF_IMPORTED_UNIT;

  /* Program-level scope (unit_index 0) — lowest useful priority. */
  if (candidate->source_unit_index == 0)
    return ENUM_PREF_PROGRAM_SCOPE;

  return ENUM_PREF_OTHER;
}

HashNode_t *semcheck_find_visible_enum_type_candidate_with_literal(
    SymTab_t *symtab, const char *type_name, const char *field_id,
    long long *out_value) {
  if (symtab == NULL || type_name == NULL || field_id == NULL ||
      out_value == NULL)
    return NULL;
  if (g_semcheck_alias_target_probe_depth > 0)
    return NULL;

  ListNode_t *matches = FindAllIdents(symtab, type_name);
  HashNode_t *best = NULL;
  long long best_value = 0;
  EnumCandidatePreference best_pref = ENUM_PREF_OTHER;

  for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
    HashNode_t *candidate = (HashNode_t *)cur->cur;
    long long candidate_value = 0;
    struct TypeAlias *candidate_alias = NULL;

    if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
      continue;
    candidate_alias = hashnode_get_type_alias(candidate);
    if (!semcheck_try_resolve_enum_literal_from_type_alias(
            symtab, candidate_alias, field_id, &candidate_value))
      continue;

    EnumCandidatePreference pref =
        semcheck_enum_candidate_preference(symtab, candidate);

    if (best == NULL || pref < best_pref) {
      best = candidate;
      best_value = candidate_value;
      best_pref = pref;
    }
  }

  if (matches != NULL)
    DestroyList(matches);

  if (best != NULL)
    *out_value = best_value;
  return best;
}

static HashNode_t *semcheck_find_any_proc_symbol(SymTab_t *symtab,
                                                 const char *id) {
  if (symtab == NULL || id == NULL)
    return NULL;

  for (ScopeNode *cur_scope = symtab->current_scope; cur_scope != NULL;
       cur_scope = cur_scope->parent) {
    HashTable_t *table = cur_scope->table;
    ListNode_t *matches = FindAllIdentsInTable(table, id);
    if (matches != NULL) {
      for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && (node->hash_type == HASHTYPE_FUNCTION ||
                             node->hash_type == HASHTYPE_PROCEDURE)) {
          DestroyList(matches);
          return node;
        }
      }
      DestroyList(matches);
    }
  }

  return NULL;
}

int semcheck_typecast(int *type_return, SymTab_t *symtab,
                      struct Expression *expr, int max_scope_lev,
                      int mutating) {
  (void)mutating;

  assert(type_return != NULL);
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_TYPECAST);

  int error_count = 0;
  int inner_type = UNKNOWN_TYPE;
  KgpcType *inner_kgpc_type = NULL;

  if (expr->expr_data.typecast_data.expr != NULL) {
    error_count += semcheck_expr_with_type(&inner_kgpc_type, symtab,
                                           expr->expr_data.typecast_data.expr,
                                           max_scope_lev, NO_MUTATE);
    inner_type = semcheck_tag_from_kgpc(inner_kgpc_type);
  }

  const char *target_id = expr->expr_data.typecast_data.target_type_id;
  if (target_id != NULL && pascal_identifier_equals(target_id, "unaligned")) {
    if (expr->expr_data.typecast_data.expr == NULL) {
      semcheck_error_with_context_at(
          expr->line_num, expr->col_num, expr->source_index,
          "Error on line %d, unaligned requires an argument.\n\n",
          expr->line_num);
      *type_return = UNKNOWN_TYPE;
      return 1;
    }
    expr->expr_data.typecast_data.target_type = inner_type;
    if (inner_kgpc_type != NULL) {
      semcheck_expr_set_resolved_kgpc_type_shared(expr, inner_kgpc_type);
      *type_return = semcheck_tag_from_kgpc(inner_kgpc_type);
    } else {
      semcheck_expr_set_resolved_type(expr, inner_type);
      *type_return = inner_type;
    }
    return error_count;
  }

  int target_type = expr->expr_data.typecast_data.target_type;
  const TypeRef *target_ref = expr->expr_data.typecast_data.target_type_ref;
  const char *qualifier = expr->expr_data.typecast_data.type_qualifier;
  const char *target_base_id = type_ref_base_name(target_ref);
  if (target_base_id == NULL)
    target_base_id = expr->expr_data.typecast_data.target_type_id;
  if (qualifier == NULL && target_ref != NULL && target_ref->name != NULL &&
      target_ref->name->count > 1) {
    char *prefix = semcheck_join_qualified_prefix(target_ref->name);
    if (prefix != NULL) {
      expr->expr_data.typecast_data.type_qualifier = prefix;
      qualifier = prefix;
    }
    if (target_base_id != NULL &&
        expr->expr_data.typecast_data.target_type_id != NULL &&
        !pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                  target_base_id)) {
      free(expr->expr_data.typecast_data.target_type_id);
      expr->expr_data.typecast_data.target_type_id = strdup(target_base_id);
    }
  }
  int builtin_mapped = semcheck_map_builtin_type_name(symtab, target_base_id);
  int target_is_builtin = (builtin_mapped != UNKNOWN_TYPE);
  if (target_type == UNKNOWN_TYPE && builtin_mapped != UNKNOWN_TYPE)
    target_type = builtin_mapped;

  if (qualifier != NULL && !semcheck_is_unit_name(qualifier)) {
    HashNode_t *qual_node = NULL;
    if (FindSymbol(&qual_node, symtab, qualifier) != 0 && qual_node != NULL &&
        qual_node->hash_type != HASHTYPE_TYPE) {
      int call_result = semcheck_reinterpret_typecast_as_call(
          type_return, symtab, expr, max_scope_lev);
      if (call_result == 0 || expr->type != EXPR_TYPECAST)
        return call_result;
    }
  }

  /* Resolve the target type unless we already mapped a builtin */
  if (target_type == UNKNOWN_TYPE || !target_is_builtin) {
    HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(
        symtab, target_ref, target_base_id);
    if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE) {
      int call_result = semcheck_reinterpret_typecast_as_call(
          type_return, symtab, expr, max_scope_lev);
      if (call_result == 0)
        return 0;
    }
    error_count += resolve_type_identifier_ref(&target_type, symtab, target_id,
                                               target_ref, expr->line_num);
  }

  HashNode_t *array_target_node = NULL;
  int target_is_array = 0;
  if (expr->expr_data.typecast_data.target_type_id != NULL) {
    array_target_node = semcheck_find_preferred_type_node_with_ref(
        symtab, target_ref, target_id);
    if (array_target_node != NULL && array_target_node->type != NULL &&
        array_target_node->type->kind == TYPE_KIND_ARRAY) {
      target_is_array = 1;
    }
  }

  if (target_type == UNKNOWN_TYPE &&
      expr->expr_data.typecast_data.target_type_id == NULL &&
      expr->expr_data.typecast_data.target_type_ref == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, typecast requires a target type.\n\n",
        expr->line_num);
    ++error_count;
  }

  *type_return = target_type;
  semcheck_expr_set_resolved_type(expr, target_type);

  if (expr->resolved_kgpc_type != NULL) {
    destroy_kgpc_type(expr->resolved_kgpc_type);
    expr->resolved_kgpc_type = NULL;
  }
  /* Set resolved_kgpc_type for the target type so that callers
   * (especially overload mangling) see the cast target type, not the inner
   * expression's type. Look up the target type node for a proper KgpcType
   * with alias info preserved (needed for AnsiString → _rbs mangling). */
  if (target_type != UNKNOWN_TYPE && target_type != PROCEDURE &&
      expr->resolved_kgpc_type == NULL) {
    const char *tid = expr->expr_data.typecast_data.target_type_id;
    HashNode_t *type_node = NULL;
    if (tid != NULL && FindSymbol(&type_node, symtab, tid) != 0 &&
        type_node != NULL && type_node->type != NULL) {
      /* Verify the looked-up type matches the expected target_type.
       * String types like AnsiString may be registered in the RTL as
       * pointer-to-record types, but for typecast semantics we need
       * the primitive STRING_TYPE representation so callers (including
       * overload resolution) see the correct type tag. */
      int node_tag = semcheck_tag_from_kgpc(type_node->type);
      if (target_is_builtin && node_tag != target_type &&
          node_tag != UNKNOWN_TYPE) {
        /* Symbol table type doesn't match builtin mapping; fall
         * through to create a fresh primitive from target_type. */
        type_node = NULL;
      } else {
        semcheck_expr_set_resolved_kgpc_type_shared(expr, type_node->type);
      }
    } else {
      KgpcType *target_kgpc = create_primitive_type(target_type);
      if (target_kgpc != NULL) {
        /* For string-type typecasts (AnsiString, RawByteString, UnicodeString),
         * attach a type_alias with alias_name so call-site mangling can
         * distinguish between string subtypes (e.g., _rbs vs _s). */
        if (target_type == STRING_TYPE && tid != NULL &&
            target_kgpc->type_alias == NULL) {
          struct TypeAlias *alias =
              (struct TypeAlias *)calloc(1, sizeof(struct TypeAlias));
          if (alias != NULL) {
            alias->alias_name = strdup(tid);
            alias->base_type = STRING_TYPE;
          }
          target_kgpc->type_alias = alias;
        }
        expr->resolved_kgpc_type = target_kgpc;
      }
    }
  }
  if (target_type == PROCEDURE) {
    HashNode_t *target_node = NULL;
    if (expr->expr_data.typecast_data.target_type_id != NULL) {
      target_node = semcheck_find_type_node_with_kgpc_type_ref(
          symtab, expr->expr_data.typecast_data.target_type_ref,
          expr->expr_data.typecast_data.target_type_id);
      if (target_node == NULL &&
          FindSymbol(&target_node, symtab,
                     expr->expr_data.typecast_data.target_type_id) != 0) {
        /* target_node assigned by FindIdent when present */
      }
    }

    if (target_node != NULL && target_node->type != NULL &&
        target_node->type->kind == TYPE_KIND_PROCEDURE) {
      kgpc_type_retain(target_node->type);
      expr->resolved_kgpc_type = target_node->type;
    }
  }
  if (target_type == POINTER_TYPE) {
    /* Resolve full pointer type info so deref preserves record/element types */
    KgpcType *resolved_ptr = NULL;
    HashNode_t *target_node = NULL;
    struct TypeAlias *alias = NULL;
    if (expr->expr_data.typecast_data.target_type_id != NULL ||
        expr->expr_data.typecast_data.target_type_ref != NULL) {
      target_node = semcheck_find_preferred_type_node_with_ref(
          symtab, expr->expr_data.typecast_data.target_type_ref,
          expr->expr_data.typecast_data.target_type_id);
      if (target_node == NULL &&
          expr->expr_data.typecast_data.target_type_id != NULL &&
          FindSymbol(&target_node, symtab,
                     expr->expr_data.typecast_data.target_type_id) != 0) {
        /* target_node assigned by FindIdent when present */
      }
      if (target_node == NULL &&
          expr->expr_data.typecast_data.target_type_id != NULL) {
        const char *owner_full =
            semcheck_get_current_subprogram_owner_class_full();
        const char *owner_outer =
            semcheck_get_current_subprogram_owner_class_outer();
        if (owner_full == NULL)
          owner_full = semcheck_get_current_method_owner();
        target_node = semcheck_find_type_node_in_owner_chain(
            symtab, expr->expr_data.typecast_data.target_type_id, owner_full,
            owner_outer);
      }
      if (target_node != NULL && target_node->type != NULL) {
        resolved_ptr = target_node->type;
        kgpc_type_retain(resolved_ptr);

        if (resolved_ptr->kind == TYPE_KIND_POINTER) {
          KgpcType *points_to =
              kgpc_type_resolve_pointer_pointee(resolved_ptr, symtab);
          if (points_to != NULL && points_to->kind == TYPE_KIND_RECORD &&
              points_to->info.record_info != NULL) {
            semcheck_set_pointer_info(expr, RECORD_TYPE,
                                      points_to->info.record_info->type_id);
          } else if (points_to != NULL &&
                     points_to->kind == TYPE_KIND_PRIMITIVE) {
            int subtype = kgpc_type_get_primitive_tag(points_to);
            semcheck_set_pointer_info(expr, subtype, NULL);
          } else if (points_to != NULL && points_to->kind == TYPE_KIND_ARRAY) {
            /* Pointer to array: propagate the array type id so that
             * dereference can set is_array_expr on the result.
             * Look up the pointer alias to get the pointee type name. */
            const char *arr_subtype_id = NULL;
            struct TypeAlias *ptr_alias = get_type_alias_from_node(target_node);
            if (ptr_alias != NULL && ptr_alias->pointer_type_id != NULL)
              arr_subtype_id = ptr_alias->pointer_type_id;
            semcheck_set_pointer_info(expr, UNKNOWN_TYPE, arr_subtype_id);
          }
        }
      }
      if (target_node != NULL)
        alias = get_type_alias_from_node(target_node);
      if (alias != NULL && alias->is_pointer) {
        KgpcType *alias_type =
            create_kgpc_type_from_type_alias(alias, symtab, 0);
        if (alias_type != NULL) {
          if (alias->kgpc_type == alias_type)
            kgpc_type_retain(alias_type);
          if (resolved_ptr != NULL)
            destroy_kgpc_type(resolved_ptr);
          resolved_ptr = alias_type;

          if (resolved_ptr->kind == TYPE_KIND_POINTER) {
            KgpcType *points_to =
                kgpc_type_resolve_pointer_pointee(resolved_ptr, symtab);
            if (points_to != NULL && points_to->kind == TYPE_KIND_RECORD &&
                points_to->info.record_info != NULL) {
              semcheck_set_pointer_info(expr, RECORD_TYPE,
                                        points_to->info.record_info->type_id);
            } else if (points_to != NULL &&
                       points_to->kind == TYPE_KIND_PRIMITIVE) {
              int subtype = kgpc_type_get_primitive_tag(points_to);
              semcheck_set_pointer_info(expr, subtype, NULL);
            } else if (points_to != NULL &&
                       points_to->kind == TYPE_KIND_ARRAY) {
              const char *arr_subtype_id = NULL;
              if (alias != NULL && alias->pointer_type_id != NULL)
                arr_subtype_id = alias->pointer_type_id;
              semcheck_set_pointer_info(expr, UNKNOWN_TYPE, arr_subtype_id);
            }
          }
        }
      }
    }

    if (resolved_ptr == NULL) {
      int inferred_subtype = UNKNOWN_TYPE;

      if (inner_kgpc_type != NULL) {
        if (kgpc_type_is_string(inner_kgpc_type) ||
            kgpc_type_is_shortstring(inner_kgpc_type) ||
            kgpc_type_is_char(inner_kgpc_type)) {
          inferred_subtype = CHAR_TYPE;
        } else if (kgpc_type_is_array(inner_kgpc_type)) {
          KgpcType *elem = inner_kgpc_type->info.array_info.element_type;
          if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
              elem->info.primitive_type_tag == CHAR_TYPE) {
            inferred_subtype = CHAR_TYPE;
          }
        }
      }

      if (inferred_subtype != UNKNOWN_TYPE) {
        KgpcType *points_to = create_primitive_type(inferred_subtype);
        if (points_to != NULL) {
          resolved_ptr = create_pointer_type(points_to);
          kgpc_type_release(points_to);
          semcheck_set_pointer_info(expr, inferred_subtype, NULL);
        } else {
          resolved_ptr = create_pointer_type(NULL);
          semcheck_set_pointer_info(
              expr, UNKNOWN_TYPE, expr->expr_data.typecast_data.target_type_id);
        }
      } else {
        resolved_ptr = create_pointer_type(NULL);
        semcheck_set_pointer_info(expr, UNKNOWN_TYPE,
                                  expr->expr_data.typecast_data.target_type_id);
      }
    } else if (resolved_ptr->kind == TYPE_KIND_PRIMITIVE &&
               resolved_ptr->info.primitive_type_tag == POINTER_TYPE) {
      if (expr->pointer_subtype == UNKNOWN_TYPE && inner_kgpc_type != NULL) {
        if (kgpc_type_is_string(inner_kgpc_type) ||
            kgpc_type_is_shortstring(inner_kgpc_type) ||
            kgpc_type_is_char(inner_kgpc_type)) {
          semcheck_set_pointer_info(expr, CHAR_TYPE, NULL);
        } else if (kgpc_type_is_array(inner_kgpc_type)) {
          KgpcType *elem = inner_kgpc_type->info.array_info.element_type;
          if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
              elem->info.primitive_type_tag == CHAR_TYPE) {
            semcheck_set_pointer_info(expr, CHAR_TYPE, NULL);
          }
        }
      }
    }

    if (resolved_ptr != NULL && resolved_ptr->kind == TYPE_KIND_POINTER &&
        resolved_ptr->info.points_to == NULL) {
      const char *subtype_id = NULL;
      const TypeRef *subtype_ref = NULL;
      if (alias != NULL) {
        if (alias->pointer_type_id != NULL)
          subtype_id = alias->pointer_type_id;
        else if (alias->target_type_id != NULL)
          subtype_id = alias->target_type_id;
        if (alias->pointer_type_ref != NULL)
          subtype_ref = alias->pointer_type_ref;
        else if (alias->target_type_ref != NULL)
          subtype_ref = alias->target_type_ref;
      }
      if (subtype_id == NULL)
        subtype_id = expr->pointer_subtype_id;
      if (subtype_ref == NULL)
        subtype_ref = expr->pointer_subtype_ref;

      HashNode_t *sub_node = semcheck_find_preferred_type_node_with_ref(
          symtab, subtype_ref, subtype_id);
      if (sub_node == NULL && subtype_id != NULL) {
        const char *owner_full =
            semcheck_get_current_subprogram_owner_class_full();
        const char *owner_outer =
            semcheck_get_current_subprogram_owner_class_outer();
        if (owner_full == NULL)
          owner_full = semcheck_get_current_method_owner();
        sub_node = semcheck_find_type_node_in_owner_chain(
            symtab, subtype_id, owner_full, owner_outer);
      }

      KgpcType *points_to = NULL;
      int points_to_owned = 0;
      if (sub_node != NULL) {
        struct RecordType *record_info = get_record_type_from_node(sub_node);
        if (sub_node->type != NULL) {
          if (sub_node->type->kind == TYPE_KIND_PRIMITIVE &&
              sub_node->type->info.primitive_type_tag == RECORD_TYPE &&
              record_info != NULL) {
            points_to = create_record_type(record_info);
            points_to_owned = 1;
            semcheck_set_pointer_info(expr, RECORD_TYPE, record_info->type_id);
          } else {
            kgpc_type_retain(sub_node->type);
            points_to = sub_node->type;
          }
        } else if (record_info != NULL) {
          points_to = create_record_type(record_info);
          points_to_owned = 1;
          semcheck_set_pointer_info(expr, RECORD_TYPE, record_info->type_id);
        }
      }
      if (points_to == NULL && subtype_id != NULL) {
        struct RecordType *record_info =
            semcheck_lookup_record_type(symtab, subtype_id);
        if (record_info != NULL) {
          points_to = create_record_type(record_info);
          points_to_owned = 1;
          semcheck_set_pointer_info(expr, RECORD_TYPE, record_info->type_id);
        }
      }

      if (points_to != NULL) {
        KgpcType *new_ptr = create_pointer_type(points_to);
        if (new_ptr != NULL) {
          if (resolved_ptr != NULL)
            destroy_kgpc_type(resolved_ptr);
          resolved_ptr = new_ptr;
          points_to = NULL;
        }
        if (points_to_owned && points_to != NULL)
          destroy_kgpc_type(points_to);
      }
    }

    if (alias != NULL && alias->pointer_type_id != NULL) {
      if (expr->pointer_subtype_id == NULL ||
          (expr->expr_data.typecast_data.target_type_id != NULL &&
           pascal_identifier_equals(
               expr->pointer_subtype_id,
               expr->expr_data.typecast_data.target_type_id))) {
        int subtype_tag = expr->pointer_subtype;
        semcheck_set_pointer_info(expr, subtype_tag, alias->pointer_type_id);
      }
    }

    expr->resolved_kgpc_type = resolved_ptr;
  } else if (target_type == RECORD_TYPE) {
    HashNode_t *target_node = NULL;
    struct RecordType *record_info = NULL;
    if (expr->expr_data.typecast_data.target_type_id != NULL ||
        expr->expr_data.typecast_data.target_type_ref != NULL) {
      target_node = semcheck_find_preferred_type_node_with_ref(
          symtab, expr->expr_data.typecast_data.target_type_ref,
          expr->expr_data.typecast_data.target_type_id);
      if (target_node == NULL &&
          expr->expr_data.typecast_data.target_type_id != NULL &&
          FindSymbol(&target_node, symtab,
                     expr->expr_data.typecast_data.target_type_id) != 0) {
        /* target_node assigned by FindIdent when present */
      }
      if (target_node == NULL &&
          expr->expr_data.typecast_data.target_type_id != NULL) {
        const char *owner_full =
            semcheck_get_current_subprogram_owner_class_full();
        const char *owner_outer =
            semcheck_get_current_subprogram_owner_class_outer();
        if (owner_full == NULL)
          owner_full = semcheck_get_current_method_owner();
        target_node = semcheck_find_type_node_in_owner_chain(
            symtab, expr->expr_data.typecast_data.target_type_id, owner_full,
            owner_outer);
      }
      if (target_node != NULL) {
        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
          fprintf(stderr,
                  "[SemCheck] typecast record target=%s node=%p kgpc_kind=%d\n",
                  expr->expr_data.typecast_data.target_type_id,
                  (void *)target_node,
                  target_node->type != NULL ? target_node->type->kind : -1);
        }
        record_info = get_record_type_from_node(target_node);
        if (record_info == NULL && target_node->type != NULL &&
            kgpc_type_is_record(target_node->type)) {
          record_info = kgpc_type_get_record(target_node->type);
        }
      }
      if (record_info == NULL)
        record_info = semcheck_lookup_record_type(
            symtab, expr->expr_data.typecast_data.target_type_id);
    }

    if (record_info != NULL) {
      KgpcType *target_type = NULL;
      if (target_node != NULL && target_node->type != NULL &&
          kgpc_type_is_record(target_node->type) &&
          kgpc_type_get_record(target_node->type) != NULL) {
        target_type = target_node->type;
      }
      if (target_type != NULL) {
        kgpc_type_retain(target_type);
        expr->resolved_kgpc_type = target_type;
      } else {
        expr->resolved_kgpc_type = create_record_type(record_info);
      }
    } else if (target_node != NULL && target_node->type != NULL) {
      kgpc_type_retain(target_node->type);
      expr->resolved_kgpc_type = target_node->type;
    } else {
      expr->resolved_kgpc_type = create_primitive_type(RECORD_TYPE);
    }
  }

  if (expr->resolved_kgpc_type == NULL) {
    HashNode_t *target_node = semcheck_find_preferred_type_node_with_ref(
        symtab, expr->expr_data.typecast_data.target_type_ref,
        expr->expr_data.typecast_data.target_type_id);
    if (target_node != NULL && target_node->type != NULL) {
      kgpc_type_retain(target_node->type);
      expr->resolved_kgpc_type = target_node->type;
    } else if (target_type != UNKNOWN_TYPE) {
      expr->resolved_kgpc_type = create_primitive_type(target_type);
    }
  } else if (target_type != UNKNOWN_TYPE && expr->resolved_kgpc_type != NULL &&
             expr->resolved_kgpc_type->kind == TYPE_KIND_PRIMITIVE) {
    int prim_tag = expr->resolved_kgpc_type->info.primitive_type_tag;
    if (prim_tag != target_type) {
      destroy_kgpc_type(expr->resolved_kgpc_type);
      expr->resolved_kgpc_type = create_primitive_type(target_type);
    }
    semcheck_clear_array_info(expr);
  }

  if (target_is_array && array_target_node != NULL &&
      array_target_node->type != NULL) {
    if (expr->resolved_kgpc_type != NULL) {
      destroy_kgpc_type(expr->resolved_kgpc_type);
      expr->resolved_kgpc_type = NULL;
    }
    kgpc_type_retain(array_target_node->type);
    expr->resolved_kgpc_type = array_target_node->type;
    semcheck_set_array_info_from_kgpctype(expr, symtab, array_target_node->type,
                                          expr->line_num);
    *type_return = UNKNOWN_TYPE;
  }

  /* Record-to-primitive typecast via operator overload.
   *
   * When the source has an `operator := (const c: TRec): <prim>` overload
   * (e.g. `Tconstexprint` → `int64`), an explicit cast like `int64(rec)` must
   * invoke that operator.  Without this rewrite, build_expr_tree strips the
   * EXPR_TYPECAST and emits the raw record address as the int value, which
   * causes wild garbage at the call site.  This was the root cause of the
   * "Data element too large" miscompile in pp_bootstrap (textrec.inc:57)
   * where pexpr.pas calls cstringdef.createshort(int64(p.value), true).
   *
   * We must NOT activate for raw bit-cast typecasts like
   * `word(packed_2byte_rec)` (used by FPC's crt unit on TCharAttr).  Those
   * records have no operator :=, and falling into
   * semcheck_try_record_conversion_expression's global operator search risks
   * matching unrelated overloads such as `olevariant__op_assign_word` and
   * producing a NULL-call crash.
   *
   * Gate: only invoke the rewrite when a SPECIFIC operator named
   * "<SourceTypeId>__op_assign_<TargetTypeId>" or "<SourceTypeId>__op_assign"
   * exists in scope.  This requires both the source and target to have
   * identifiable type names. */
  if (target_type != UNKNOWN_TYPE && target_type != PROCEDURE &&
      target_type != POINTER_TYPE && !target_is_array &&
      inner_kgpc_type != NULL && expr->resolved_kgpc_type != NULL &&
      expr->expr_data.typecast_data.expr != NULL &&
      (is_integer_type(target_type) || target_type == REAL_TYPE ||
       target_type == EXTENDED_TYPE || target_type == BOOL ||
       target_type == CHAR_TYPE) &&
      (inner_kgpc_type->kind == TYPE_KIND_RECORD ||
       (inner_kgpc_type->kind == TYPE_KIND_POINTER &&
        inner_kgpc_type->info.points_to != NULL &&
        inner_kgpc_type->info.points_to->kind == TYPE_KIND_RECORD) ||
       inner_type == RECORD_TYPE)) {
    const char *source_type_id = NULL;
    if (inner_kgpc_type->kind == TYPE_KIND_RECORD &&
        inner_kgpc_type->info.record_info != NULL) {
      source_type_id = inner_kgpc_type->info.record_info->type_id;
    } else if (inner_kgpc_type->kind == TYPE_KIND_POINTER &&
               inner_kgpc_type->info.points_to != NULL &&
               inner_kgpc_type->info.points_to->kind == TYPE_KIND_RECORD &&
               inner_kgpc_type->info.points_to->info.record_info != NULL) {
      source_type_id =
          inner_kgpc_type->info.points_to->info.record_info->type_id;
    }

    const char *target_id_for_op = expr->expr_data.typecast_data.target_type_id;

    int has_source_specific_op = 0;
    if (source_type_id != NULL) {
      char op_id[320];
      snprintf(op_id, sizeof(op_id), "%s__op_assign", source_type_id);
      ListNode_t *cands = FindAllIdents(symtab, op_id);
      if (cands != NULL) {
        has_source_specific_op = 1;
        DestroyList(cands);
      }
      if (!has_source_specific_op && target_id_for_op != NULL) {
        snprintf(op_id, sizeof(op_id), "%s__op_assign_%s", source_type_id,
                 target_id_for_op);
        cands = FindAllIdents(symtab, op_id);
        if (cands != NULL) {
          has_source_specific_op = 1;
          DestroyList(cands);
        }
      }
    }

    if (has_source_specific_op) {
      KgpcType *converted_source = inner_kgpc_type;
      int converted_owned = 0;
      if (semcheck_try_record_conversion_expression(
              symtab, &expr->expr_data.typecast_data.expr, NULL,
              expr->resolved_kgpc_type, &converted_source, &converted_owned)) {
        /* Inner slot now points to a fresh EXPR_FUNCTION_CALL whose
         * first arg is the original record expression.  The outer
         * EXPR_TYPECAST is preserved as a redundant int-to-int cast
         * over the call's primitive result. */
        if (converted_owned && converted_source != NULL)
          destroy_kgpc_type(converted_source);
      }
    }
  }

  (void)inner_type;
  return error_count;
}

int semcheck_is_expr(int *type_return, SymTab_t *symtab,
                     struct Expression *expr, int max_scope_lev, int mutating) {
  (void)mutating;
  assert(type_return != NULL);
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_IS);

  int error_count = 0;
  struct Expression *value_expr = expr->expr_data.is_data.expr;
  if (value_expr == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, \"is\" operator requires a value expression.\n\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  int value_type = UNKNOWN_TYPE;
  KgpcType *value_kgpc_type_is = NULL;
  error_count += semcheck_expr_with_type(&value_kgpc_type_is, symtab,
                                         value_expr, max_scope_lev, NO_MUTATE);
  value_type = semcheck_tag_from_kgpc(value_kgpc_type_is);

  struct RecordType *value_record = NULL;
  if (value_kgpc_type_is != NULL) {
    KgpcType *inner_is = value_kgpc_type_is;
    if (inner_is->kind == TYPE_KIND_POINTER && inner_is->info.points_to != NULL)
      inner_is = inner_is->info.points_to;
    if (inner_is->kind == TYPE_KIND_RECORD)
      value_record = inner_is->info.record_info;
  }

  /* Classes are pointers to records, so we need to handle POINTER_TYPE */
  int is_valid_class = 0;
  if (value_type == RECORD_TYPE && value_record != NULL &&
      record_type_is_class(value_record)) {
    is_valid_class = 1;
  } else if (value_type == POINTER_TYPE && value_record != NULL &&
             record_type_is_class(value_record)) {
    is_valid_class = 1;
  }
  /* Also check via KgpcType for cases where record_type isn't set on the
   * expression */
  if (!is_valid_class && value_kgpc_type_is != NULL) {
    KgpcType *inner_is = value_kgpc_type_is;
    if (inner_is->kind == TYPE_KIND_POINTER && inner_is->info.points_to != NULL)
      inner_is = inner_is->info.points_to;
    if (inner_is->kind == TYPE_KIND_RECORD &&
        inner_is->info.record_info != NULL &&
        record_type_is_class(inner_is->info.record_info)) {
      is_valid_class = 1;
      if (value_record == NULL)
        value_record = inner_is->info.record_info;
    }
  }
  if (!is_valid_class && value_kgpc_type_is != NULL &&
      value_kgpc_type_is->kind == TYPE_KIND_POINTER) {
    KgpcType *points_to = value_kgpc_type_is->info.points_to;
    if (points_to == NULL) {
      /* Legacy class-instance flows can lose pointee metadata and degrade
       * to generic pointer. Keep "is" usable in RTL exception paths. */
      is_valid_class = 1;
    } else if (points_to->kind == TYPE_KIND_PRIMITIVE &&
               points_to->info.primitive_type_tag == RECORD_TYPE) {
      is_valid_class = 1;
    } else if (points_to->kind == TYPE_KIND_POINTER &&
               points_to->info.points_to != NULL &&
               points_to->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
               points_to->info.points_to->info.primitive_type_tag ==
                   RECORD_TYPE) {
      is_valid_class = 1;
    }
  }

  if (!is_valid_class) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, \"is\" operator requires a class instance on the "
        "left-hand side.\n\n",
        expr->line_num);
    ++error_count;
  }

  int target_type = expr->expr_data.is_data.target_type;
  struct RecordType *target_record = NULL;
  if (expr->expr_data.is_data.target_type_id != NULL) {
    target_record = semcheck_lookup_record_type(
        symtab, expr->expr_data.is_data.target_type_id);

    if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
      fprintf(stderr, "[SemCheck] is_expr: lookup '%s' -> %p\n",
              expr->expr_data.is_data.target_type_id, target_record);
      if (target_record) {
        fprintf(stderr, "[SemCheck]   is_class=%d\n", target_record->is_class);
      }
    }
  }

  /* Check if target is a class (could be RECORD_TYPE or POINTER_TYPE to record)
   */
  int is_valid_target = 0;
  if (target_record != NULL && record_type_is_class(target_record)) {
    is_valid_target = 1;
  }
  /* Also check via symbol table KgpcType when record type lookup fails */
  if (!is_valid_target && expr->expr_data.is_data.target_type_id != NULL) {
    HashNode_t *is_target_node = NULL;
    if (FindSymbol(&is_target_node, symtab,
                   expr->expr_data.is_data.target_type_id) != 0 &&
        is_target_node != NULL && is_target_node->type != NULL) {
      KgpcType *is_tgt = is_target_node->type;
      if (is_tgt->kind == TYPE_KIND_POINTER && is_tgt->info.points_to != NULL)
        is_tgt = is_tgt->info.points_to;
      if (is_tgt->kind == TYPE_KIND_RECORD &&
          is_tgt->info.record_info != NULL &&
          record_type_is_class(is_tgt->info.record_info)) {
        is_valid_target = 1;
        target_record = is_tgt->info.record_info;
      }

      /* Accept class-reference variables on RHS (e.g. Obj is ObjType,
       * where ObjType: TClass). These are runtime class refs and do
       * not necessarily carry a concrete RecordType at semcheck time. */
      if (!is_valid_target && (is_target_node->hash_type == HASHTYPE_VAR ||
                               is_target_node->hash_type == HASHTYPE_ARRAY)) {
        KgpcType *target_var_type = is_target_node->type;
        if (target_var_type != NULL) {
          int target_is_pointer =
              (target_var_type->kind == TYPE_KIND_POINTER) ||
              (target_var_type->kind == TYPE_KIND_PRIMITIVE &&
               target_var_type->info.primitive_type_tag == POINTER_TYPE);
          if (target_is_pointer)
            is_valid_target = 1;
        }
      }
    }
  }

  /* When the RHS identifier is a class field like FItemClass: TClass,
   * FindIdent won't find it directly - try resolving via implicit Self. */
  if (!is_valid_target && expr->expr_data.is_data.target_type_id != NULL) {
    const char *rhs_id = expr->expr_data.is_data.target_type_id;
    const char *owner_id = semcheck_get_current_method_owner();
    if (owner_id != NULL) {
      HashNode_t *owner_node = NULL;
      if (FindSymbol(&owner_node, symtab, owner_id) != 0 &&
          owner_node != NULL && owner_node->type != NULL) {
        struct RecordType *owner_rec = NULL;
        if (kgpc_type_is_record(owner_node->type))
          owner_rec = kgpc_type_get_record(owner_node->type);
        else if (kgpc_type_is_pointer(owner_node->type) &&
                 owner_node->type->info.points_to != NULL &&
                 kgpc_type_is_record(owner_node->type->info.points_to))
          owner_rec = kgpc_type_get_record(owner_node->type->info.points_to);
        if (owner_rec != NULL) {
          struct RecordField *field =
              semcheck_find_class_field_including_hidden(symtab, owner_rec,
                                                         rhs_id, NULL);
          if (field != NULL) {
            /* Field found - check if its type is a class reference (pointer) */
            int is_ptr = field->is_pointer || field->type == POINTER_TYPE;
            /* Also check via the field's type_id lookup for class-of types */
            if (!is_ptr && field->type_id != NULL) {
              HashNode_t *ft_node = NULL;
              if (FindSymbol(&ft_node, symtab, field->type_id) != 0 &&
                  ft_node != NULL && ft_node->type != NULL) {
                KgpcType *ft = ft_node->type;
                is_ptr = (ft->kind == TYPE_KIND_POINTER) ||
                         (ft->kind == TYPE_KIND_PRIMITIVE &&
                          ft->info.primitive_type_tag == POINTER_TYPE);
              }
            }
            if (is_ptr)
              is_valid_target = 1;
          }
        }
      }
    }
  }

  if (!is_valid_target) {
    semcheck_error_with_context_at(expr->line_num, expr->col_num,
                                   expr->source_index,
                                   "Error on line %d, \"is\" operator requires "
                                   "a class type on the right-hand side.\n\n",
                                   expr->line_num);
    ++error_count;
  }
  target_type = RECORD_TYPE;

  expr->expr_data.is_data.target_type = target_type;
  expr->expr_data.is_data.target_record_type = target_record;
  semcheck_expr_set_resolved_type(expr, BOOL);
  *type_return = BOOL;
  return error_count;
}

int semcheck_as_expr(int *type_return, SymTab_t *symtab,
                     struct Expression *expr, int max_scope_lev, int mutating) {
  (void)mutating;
  assert(type_return != NULL);
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_AS);

  int error_count = 0;
  struct Expression *value_expr = expr->expr_data.as_data.expr;
  if (value_expr == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, \"as\" operator requires a value expression.\n\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  int value_type = UNKNOWN_TYPE;
  KgpcType *value_kgpc_type_as = NULL;
  error_count += semcheck_expr_with_type(&value_kgpc_type_as, symtab,
                                         value_expr, max_scope_lev, NO_MUTATE);
  value_type = semcheck_tag_from_kgpc(value_kgpc_type_as);

  struct RecordType *value_record = NULL;
  if (value_kgpc_type_as != NULL) {
    KgpcType *inner_as = value_kgpc_type_as;
    if (inner_as->kind == TYPE_KIND_POINTER && inner_as->info.points_to != NULL)
      inner_as = inner_as->info.points_to;
    if (inner_as->kind == TYPE_KIND_RECORD)
      value_record = inner_as->info.record_info;
  }

  /* Classes are pointers to records, so we need to handle POINTER_TYPE */
  int is_valid_class = 0;
  if (value_type == RECORD_TYPE && value_record != NULL &&
      record_type_is_class(value_record)) {
    is_valid_class = 1;
  } else if (value_type == POINTER_TYPE && value_record != NULL &&
             record_type_is_class(value_record)) {
    is_valid_class = 1;
  }

  if (!is_valid_class) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, \"as\" operator requires a class instance on the "
        "left-hand side.\n\n",
        expr->line_num);
    ++error_count;
  }

  int target_type = expr->expr_data.as_data.target_type;
  struct RecordType *target_record = NULL;
  if (expr->expr_data.as_data.target_type_id != NULL) {
    target_record = semcheck_lookup_record_type(
        symtab, expr->expr_data.as_data.target_type_id);
  }

  /* Check if target is a class (could be RECORD_TYPE or POINTER_TYPE to record)
   */
  int is_valid_target = 0;
  if (target_record != NULL && record_type_is_class(target_record)) {
    is_valid_target = 1;
  }

  if (!is_valid_target) {
    semcheck_error_with_context_at(expr->line_num, expr->col_num,
                                   expr->source_index,
                                   "Error on line %d, \"as\" operator requires "
                                   "a class type on the right-hand side.\n\n",
                                   expr->line_num);
    ++error_count;
  }
  /* Determine correct target type */
  target_type = RECORD_TYPE;
  KgpcType *result_kgpc_type = NULL;

  if (target_record != NULL && record_type_is_class(target_record)) {
    target_type = POINTER_TYPE;
    KgpcType *record_kgpc = create_record_type(target_record);
    if (record_kgpc != NULL) {
      result_kgpc_type = create_pointer_type(record_kgpc);
      kgpc_type_release(record_kgpc);
    }
  } else {
    result_kgpc_type = create_record_type(target_record);
  }

  expr->expr_data.as_data.target_type = target_type;
  expr->expr_data.as_data.target_record_type = target_record;
  semcheck_expr_set_resolved_type(expr, target_type);

  if (expr->resolved_kgpc_type != NULL) {
    destroy_kgpc_type(expr->resolved_kgpc_type);
    expr->resolved_kgpc_type = NULL;
  }
  expr->resolved_kgpc_type = result_kgpc_type;

  *type_return = target_type;
  return error_count;
}

int semcheck_pointer_deref(int *type_return, SymTab_t *symtab,
                           struct Expression *expr, int max_scope_lev,
                           int mutating) {
  (void)mutating;

  assert(type_return != NULL);
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_POINTER_DEREF);

  semcheck_clear_pointer_info(expr);
  semcheck_clear_array_info(expr);

  struct Expression *pointer_expr =
      expr->expr_data.pointer_deref_data.pointer_expr;
  if (pointer_expr == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, dereference operator requires an operand.\\n\\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  int error_count = 0;
  int pointer_type = UNKNOWN_TYPE;
  KgpcType *pointer_kgpc_type = NULL;
  error_count += semcheck_expr_with_type(
      &pointer_kgpc_type, symtab, pointer_expr, max_scope_lev, NO_MUTATE);
  pointer_type = semcheck_tag_from_kgpc(pointer_kgpc_type);

  /* Some pointer aliases lose their tag mapping while still carrying pointer
   * metadata. Recover pointer-ness from explicit pointer metadata so chained
   * dereferences (e.g. ^^) keep working. */
  if (pointer_type != POINTER_TYPE) {
    if ((pointer_expr->resolved_kgpc_type != NULL &&
         pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) ||
        pointer_expr->pointer_subtype != UNKNOWN_TYPE ||
        pointer_expr->pointer_subtype_id != NULL) {
      pointer_type = POINTER_TYPE;
    }
  }

  if (pointer_type != POINTER_TYPE) {
    if (pointer_type == UNKNOWN_TYPE) {
      *type_return = UNKNOWN_TYPE;
      return error_count;
    }
    /* Class/object types resolve as RECORD_TYPE but are pointer-like
     * and support dereference in FPC.  Accept them silently. */
    if (pointer_type == RECORD_TYPE) {
      *type_return = RECORD_TYPE;
      return error_count;
    }
    semcheck_error_with_context_at(expr->line_num, expr->col_num,
                                   expr->source_index,
                                   "Error on line %d, dereference operator "
                                   "requires a pointer expression.\\n\\n",
                                   expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return ++error_count;
  }

  int target_type = pointer_expr->pointer_subtype;
  if (target_type == UNKNOWN_TYPE && pointer_expr->pointer_subtype_id != NULL) {
    HashNode_t *target_node = NULL;
    if (FindSymbol(&target_node, symtab, pointer_expr->pointer_subtype_id) !=
            0 &&
        target_node != NULL) {
      set_type_from_hashtype(&target_type, target_node);
      struct TypeAlias *alias = get_type_alias_from_node(target_node);
      if (alias != NULL) {
        if (alias->base_type != UNKNOWN_TYPE)
          target_type = alias->base_type;
        else if (alias->is_pointer)
          target_type = POINTER_TYPE;
        else if (alias->is_set)
          target_type = SET_TYPE;
        else if (alias->is_enum)
          target_type = ENUM_TYPE;
        else if (alias->is_file)
          target_type = FILE_TYPE;
      }
    }

    /* If still unknown, try to infer size from the subtype id.
     * This helps pointer types like PAnsiChar inherit a 1-byte element size
     * instead of defaulting to LONGINT (4 bytes). */
    if (target_type == UNKNOWN_TYPE) {
      long long inferred_size = 0;
      if (sizeof_from_type_ref(symtab, UNKNOWN_TYPE,
                               pointer_expr->pointer_subtype_id, &inferred_size,
                               0, expr->line_num) == 0 &&
          inferred_size > 0) {
        if (inferred_size == 1)
          target_type = CHAR_TYPE;
        else if (inferred_size == 2)
          target_type = INT_TYPE; /* 16-bit */
        else if (inferred_size <= 4)
          target_type = INT_TYPE;
        else
          target_type = LONGINT_TYPE;
      }
    }
  }

  /* If we still don't know the subtype, inspect the pointer symbol's alias info
   */
  if (target_type == UNKNOWN_TYPE && pointer_expr->type == EXPR_VAR_ID) {
    HashNode_t *ptr_node = NULL;
    if (FindSymbol(&ptr_node, symtab, pointer_expr->expr_data.id) != 0 &&
        ptr_node != NULL) {
      struct TypeAlias *alias = get_type_alias_from_node(ptr_node);
      if (alias != NULL && alias->is_pointer) {
        target_type = alias->pointer_type;
        if (target_type == UNKNOWN_TYPE && alias->pointer_type_id != NULL) {
          long long inferred_size = 0;
          if (sizeof_from_type_ref(symtab, UNKNOWN_TYPE, alias->pointer_type_id,
                                   &inferred_size, 0, expr->line_num) == 0 &&
              inferred_size > 0) {
            if (inferred_size == 1)
              target_type = CHAR_TYPE;
            else if (inferred_size <= 4)
              target_type = INT_TYPE;
            else
              target_type = LONGINT_TYPE;
          }
        }

        if (alias->pointer_type_id != NULL && expr->pointer_subtype_id == NULL)
          semcheck_set_pointer_info(expr, target_type, alias->pointer_type_id);
      }
    }
  }

  /* If subtype id was absent, try to infer from the resolved KgpcType pointer
   * info */
  if (target_type == UNKNOWN_TYPE && pointer_expr->resolved_kgpc_type != NULL &&
      pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
    KgpcType *points_to = kgpc_type_resolve_pointer_pointee(
        pointer_expr->resolved_kgpc_type, symtab);
    if (points_to != NULL) {
      /* Check the actual kind of the pointed-to type first */
      if (kgpc_type_is_record(points_to)) {
        target_type = RECORD_TYPE;
      } else if (points_to->kind == TYPE_KIND_POINTER) {
        target_type = POINTER_TYPE;
      } else if (points_to->kind == TYPE_KIND_ARRAY) {
        /* Pointer to array: set up array info on the deref result
         * so it can be indexed with [i] */
        target_type = INT_TYPE; /* placeholder; actual element type set below */
        expr->is_array_expr = 1;
        expr->array_element_type = UNKNOWN_TYPE;
        expr->array_element_type_id = NULL;

        KgpcType *elem_type = points_to->info.array_info.element_type;
        if (elem_type != NULL) {
          target_type = semcheck_tag_from_kgpc(elem_type);
          if (target_type == UNKNOWN_TYPE)
            target_type = INT_TYPE;
          expr->array_element_type = semcheck_tag_from_kgpc(elem_type);
          if (elem_type->kind == TYPE_KIND_RECORD)
            expr->array_element_record_type = kgpc_type_get_record(elem_type);
        }
        if (points_to->info.array_info.element_type_id != NULL)
          expr->array_element_type_id =
              strdup(points_to->info.array_info.element_type_id);

        /* Set the resolved KgpcType to the array type */
        if (expr->resolved_kgpc_type != NULL)
          destroy_kgpc_type(expr->resolved_kgpc_type);
        kgpc_type_retain(points_to);
        expr->resolved_kgpc_type = points_to;
      } else {
        /* Fall back to tag-based then size-based inference for primitive types
         */
        target_type = semcheck_tag_from_kgpc(points_to);
        if (target_type == UNKNOWN_TYPE) {
          long long inferred_size = kgpc_type_sizeof(points_to);
          if (inferred_size == 1)
            target_type = CHAR_TYPE;
          else if (inferred_size == 2)
            target_type = INT_TYPE;
          else if (inferred_size > 0 && inferred_size <= 4)
            target_type = INT_TYPE;
          else if (inferred_size > 0)
            target_type = LONGINT_TYPE;
        }
      }
    }
  }

  /* Last resort before falling back to LONGINT_TYPE: if the pointer expression
   * carries a subtype id (e.g. "TCGParaLocation" from a typed pointer alias),
   * try to resolve it as a record type.  This handles cross-unit pointer types
   * where the earlier FindSymbol + set_type_from_hashtype (line ~1481) could
   * not resolve the pointed-to type because it is only registered in the
   * cross-unit record type tables searched by semcheck_lookup_record_type. */
  if (target_type == UNKNOWN_TYPE && pointer_expr->pointer_subtype_id != NULL) {
    struct RecordType *rec =
        semcheck_lookup_record_type(symtab, pointer_expr->pointer_subtype_id);
    if (rec != NULL)
      target_type = RECORD_TYPE;
  }

  /* Fallback for genuinely untyped pointers (e.g. bare `Pointer` type).
   * The function still returns LONGINT_TYPE for backward compatibility, but
   * when pointer_subtype_id is present we propagate it to the dereference
   * expression.  This allows semcheck_recordaccess to recover the actual
   * record type via the subtype id instead of being misled by LONGINT_TYPE. */
  if (target_type == UNKNOWN_TYPE) {
    if (pointer_expr->pointer_subtype_id != NULL) {
      /* Propagate the subtype id to the dereference expression so that
       * semcheck_recordaccess can attempt record type recovery. */
      if (expr->pointer_subtype_id == NULL)
        semcheck_set_pointer_info(expr, UNKNOWN_TYPE,
                                  pointer_expr->pointer_subtype_id);
    }
    target_type = LONGINT_TYPE;
  }

  if (target_type == POINTER_TYPE) {
    /* For double-pointer dereference (e.g. PPAnsiChar^), resolve what the
     * resulting pointer (PAnsiChar) points to, so that indexing with [i]
     * can determine the correct element type (Char). */
    int resolved_subtype = UNKNOWN_TYPE;
    const char *resolved_subtype_id = pointer_expr->pointer_subtype_id;

    /* Try to resolve from KgpcType chain first */
    if (pointer_expr->resolved_kgpc_type != NULL &&
        pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
      KgpcType *deref_type = kgpc_type_resolve_pointer_pointee(
          pointer_expr->resolved_kgpc_type, symtab);
      if (deref_type != NULL && deref_type->kind == TYPE_KIND_POINTER) {
        KgpcType *sub_points_to =
            kgpc_type_resolve_pointer_pointee(deref_type, symtab);
        resolved_subtype = semcheck_tag_from_kgpc(sub_points_to);
        if (resolved_subtype == UNKNOWN_TYPE) {
          long long sz = kgpc_type_sizeof(sub_points_to);
          if (sz == 1)
            resolved_subtype = CHAR_TYPE;
          else if (sz <= 4)
            resolved_subtype = INT_TYPE;
          else
            resolved_subtype = LONGINT_TYPE;
        }
      }
    }

    /* If KgpcType didn't help, try resolving from subtype_id */
    if (resolved_subtype == UNKNOWN_TYPE && resolved_subtype_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, symtab, resolved_subtype_id) != 0 &&
          type_node != NULL) {
        struct TypeAlias *alias = get_type_alias_from_node(type_node);
        if (alias != NULL && alias->is_pointer) {
          if (alias->pointer_type != UNKNOWN_TYPE)
            resolved_subtype = alias->pointer_type;
          if (alias->pointer_type_id != NULL) {
            resolved_subtype_id = alias->pointer_type_id;
            if (resolved_subtype == UNKNOWN_TYPE) {
              struct RecordType *sub_record =
                  semcheck_lookup_record_type(symtab, alias->pointer_type_id);
              if (sub_record != NULL)
                resolved_subtype = RECORD_TYPE;
            }
          }
        }
      }
    }

    semcheck_set_pointer_info(
        expr,
        resolved_subtype != UNKNOWN_TYPE ? resolved_subtype : POINTER_TYPE,
        resolved_subtype_id);
  }
  if (pointer_expr->pointer_subtype_id != NULL) {
    HashNode_t *type_node = NULL;
    if (FindSymbol(&type_node, symtab, pointer_expr->pointer_subtype_id) != 0 &&
        type_node != NULL) {
      struct TypeAlias *alias = get_type_alias_from_node(type_node);
      if (alias != NULL && alias->is_array) {
        semcheck_set_array_info_from_alias(expr, symtab, alias, expr->line_num);
      }
    }
  }

  /* Set the expression's resolved type to the pointed-to type.
   * This is critical for code generation to emit correct-sized loads. */
  semcheck_expr_set_resolved_type(expr, target_type);

  /* Propagate KgpcType from the pointer's target for downstream resolution.
   * This is required for chained dereferences and method dispatch on alias
   * pointers. */
  if (pointer_expr->resolved_kgpc_type != NULL &&
      pointer_expr->resolved_kgpc_type->kind == TYPE_KIND_POINTER) {
    KgpcType *points_to = kgpc_type_resolve_pointer_pointee(
        pointer_expr->resolved_kgpc_type, symtab);
    if (points_to != NULL) {
      semcheck_expr_set_resolved_kgpc_type_shared(expr, points_to);
    }
  }
  if (target_type == RECORD_TYPE &&
      (expr->resolved_kgpc_type == NULL ||
       !kgpc_type_is_record(expr->resolved_kgpc_type)) &&
      pointer_expr->pointer_subtype_id != NULL) {
    struct RecordType *record_info =
        semcheck_lookup_record_type(symtab, pointer_expr->pointer_subtype_id);
    if (record_info != NULL) {
      KgpcType *record_kgpc = create_record_type(record_info);
      if (record_kgpc != NULL) {
        semcheck_expr_set_resolved_kgpc_type_shared(expr, record_kgpc);
        destroy_kgpc_type(record_kgpc);
      }
    }
  }

  *type_return = target_type;
  return error_count;
}

int semcheck_property_type_info(SymTab_t *symtab,
                                struct ClassProperty *property, int line_num,
                                int *type_out, struct RecordType **record_out) {
  if (type_out == NULL || property == NULL)
    return 1;

  int resolved_type = property->type;
  if (property->type_id != NULL || property->type_ref != NULL) {
    if (resolve_type_identifier_ref(&resolved_type, symtab, property->type_id,
                                    property->type_ref, line_num) != 0) {
      semcheck_error_with_context(
          "Error on line %d, unable to resolve type for property %s.\n\n",
          line_num, property->name != NULL ? property->name : "<unnamed>");
      return 1;
    }
  }

  if (resolved_type == UNKNOWN_TYPE && property->type_id == NULL &&
      property->type_ref == NULL) {
    semcheck_error_with_context(
        "Error on line %d, property %s must specify a type.\n\n", line_num,
        property->name != NULL ? property->name : "<unnamed>");
    return 1;
  }

  *type_out = resolved_type;
  if (record_out != NULL) {
    if (resolved_type == RECORD_TYPE) {
      HashNode_t *type_node = semcheck_find_preferred_type_node_with_ref(
          symtab, property->type_ref, property->type_id);
      if (type_node == NULL)
        type_node = semcheck_find_type_node_with_kgpc_type_ref(
            symtab, property->type_ref, property->type_id);
      if (type_node != NULL)
        *record_out = get_record_type_from_node(type_node);
      else if (property->type_id != NULL)
        *record_out = semcheck_lookup_record_type(symtab, property->type_id);
      else
        *record_out = NULL;
    } else
      *record_out = NULL;
  }

  return 0;
}

int semcheck_transform_property_getter_call(int *type_return, SymTab_t *symtab,
                                            struct Expression *expr,
                                            int max_scope_lev, int mutating,
                                            HashNode_t *method_node,
                                            struct RecordType *owner_record) {
  if (expr == NULL || expr->type != EXPR_RECORD_ACCESS || method_node == NULL) {
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  struct Expression *object_expr =
      expr->expr_data.record_access_data.record_expr;
  if (object_expr == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, property getter requires an object instance.\n\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  /* Check if the getter is a static function (takes no Self parameter).
   * Static getters don't need the object instance as an argument. */
  int is_static_getter = 0;
  if (owner_record != NULL && owner_record->type_id != NULL &&
      method_node->id != NULL) {
    is_static_getter =
        from_cparser_is_method_static(owner_record->type_id, method_node->id);
  }
  if (!is_static_getter && method_node->type != NULL &&
      method_node->type->kind == TYPE_KIND_PROCEDURE) {
    ListNode_t *params = kgpc_type_get_procedure_params(method_node->type);
    if (params == NULL) {
      /* No parameters - this is a static getter */
      is_static_getter = 1;
    }
  }

  expr->expr_data.record_access_data.record_expr = NULL;
  if (expr->expr_data.record_access_data.field_id != NULL) {
    free(expr->expr_data.record_access_data.field_id);
    expr->expr_data.record_access_data.field_id = NULL;
  }

  ListNode_t *arg_node = NULL;
  if (!is_static_getter) {
    /* Non-static getter - pass object as first argument */
    arg_node = CreateListNode(object_expr, LIST_EXPR);
    if (arg_node == NULL) {
      semcheck_error_with_context_at(
          expr->line_num, expr->col_num, expr->source_index,
          "Error on line %d, unable to allocate getter argument list.\n\n",
          expr->line_num);
      expr->expr_data.record_access_data.record_expr = object_expr;
      *type_return = UNKNOWN_TYPE;
      return 1;
    }
  } else {
    /* Static getter - no object argument needed, destroy the object expression
     */
    destroy_expr(object_expr);
  }

  char *id_copy = method_node->id != NULL ? strdup(method_node->id) : NULL;
  char *mangled_copy = NULL;
  if (method_node->mangled_id != NULL)
    mangled_copy = strdup(method_node->mangled_id);

  if ((method_node->id != NULL && id_copy == NULL) ||
      (method_node->mangled_id != NULL && mangled_copy == NULL)) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, unable to prepare property getter call.\n\n",
        expr->line_num);
    free(id_copy);
    free(mangled_copy);
    if (arg_node != NULL) {
      /* Restore object_expr ownership before freeing arg_node */
      object_expr = (struct Expression *)arg_node->cur;
      arg_node->cur = NULL;
      free(arg_node);
      expr->expr_data.record_access_data.record_expr = object_expr;
    }
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  expr->type = EXPR_FUNCTION_CALL;
  memset(&expr->expr_data.function_call_data, 0,
         sizeof(expr->expr_data.function_call_data));
  expr->expr_data.function_call_data.id = id_copy;
  expr->expr_data.function_call_data.mangled_id = mangled_copy;
  expr->expr_data.function_call_data.args_expr = arg_node;
  /* Set resolved_func so semcheck_funccall goes via method_call_resolved (which
   * runs the VMT dispatch check at lines 5241-5283) rather than the
   * funccall_cleanup fast-path (which skips VMT dispatch, causing direct calls
   * to abstract methods). */
  expr->expr_data.function_call_data.resolved_func = method_node;
  expr->expr_data.function_call_data.call_hash_type = method_node->hash_type;
  semcheck_expr_set_call_kgpc_type(expr, method_node->type, 0);
  expr->expr_data.function_call_data.is_call_info_valid = 1;
  semcheck_expr_set_resolved_type(expr, UNKNOWN_TYPE);
  expr->is_array_expr = 0;
  expr->array_element_type = UNKNOWN_TYPE;
  expr->array_element_type_id = NULL;
  expr->array_element_record_type = NULL;
  expr->array_element_size = 0;

  /* Keep legacy_tag here - expression is rewritten and needs re-checking with
   * int type_return */
  return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev,
                                  mutating);
}

int semcheck_try_reinterpret_as_typecast(int *type_return, SymTab_t *symtab,
                                         struct Expression *expr,
                                         int max_scope_lev) {
  assert(type_return != NULL);
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_FUNCTION_CALL);

  const char *id = expr->expr_data.function_call_data.id;
  if (id == NULL)
    return 0;
  if (pascal_identifier_equals(id, "Create"))
    return 0;
  if (kgpc_getenv("KGPC_DEBUG_TYPECAST") != NULL &&
      pascal_identifier_equals(id, "TextRec")) {
    fprintf(stderr, "[SemCheck] try_typecast TextRec at line %d\n",
            expr->line_num);
  }

  /* Only reinterpret as a typecast when there is exactly one argument. */
  int arg_count = 0;
  for (ListNode_t *cur = expr->expr_data.function_call_data.args_expr;
       cur != NULL; cur = cur->next) {
    arg_count++;
    if (arg_count > 1)
      return 0;
  }

  /* If a method with this name exists on Self, don't reinterpret as a typecast.
   */
  HashNode_t *self_node = NULL;
  if (FindSymbol(&self_node, symtab, "Self") != 0 && self_node != NULL) {
    struct RecordType *self_record = get_record_type_from_node(self_node);
    if (self_record != NULL &&
        semcheck_record_may_have_method_name(symtab, self_record, id) &&
        semcheck_find_class_method(symtab, self_record, id, NULL) != NULL) {
      return 0;
    }
  }

  char *id_copy = strdup(id);
  if (id_copy == NULL)
    return 0;

  /* Only proceed if the callee resolves to a type identifier or a known builtin
   * type */
  HashNode_t *type_node = semcheck_find_type_node_with_kgpc_type(symtab, id);
  int target_type = UNKNOWN_TYPE;
  if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
    set_type_from_hashtype(&target_type, type_node);
  if (target_type == UNKNOWN_TYPE && type_node != NULL &&
      type_node->hash_type == HASHTYPE_TYPE && type_node->type != NULL &&
      kgpc_type_is_record(type_node->type)) {
    target_type = RECORD_TYPE;
  }
  if (type_node == NULL)
    FindSymbol(&type_node, symtab, id);
  if (target_type == UNKNOWN_TYPE && type_node != NULL &&
      type_node->hash_type == HASHTYPE_TYPE && type_node->type != NULL &&
      kgpc_type_is_record(type_node->type)) {
    target_type = RECORD_TYPE;
  }
  if (target_type == UNKNOWN_TYPE)
    target_type = semcheck_map_builtin_type_name(symtab, id);

  if (type_node == NULL) {
    const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
    const char *owner_outer =
        semcheck_get_current_subprogram_owner_class_outer();
    if (owner_full == NULL)
      owner_full = semcheck_get_current_method_owner();
    type_node = semcheck_find_type_node_in_owner_chain(symtab, id, owner_full,
                                                       owner_outer);
    if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
      set_type_from_hashtype(&target_type, type_node);
      if (target_type == UNKNOWN_TYPE && type_node->type != NULL &&
          kgpc_type_is_record(type_node->type)) {
        target_type = RECORD_TYPE;
      }
    }
  }

  int is_unaligned_cast = pascal_identifier_equals(id, "unaligned");
  int is_type_identifier =
      (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) ||
      (target_type != UNKNOWN_TYPE) || is_unaligned_cast;
  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
    fprintf(stderr,
            "[SemCheck] try_typecast id=%s type_node=%p hash_type=%d "
            "target_type=%d\n",
            id, (void *)type_node,
            type_node != NULL ? type_node->hash_type : -1, target_type);
  }
  if (!is_type_identifier) {
    if (kgpc_getenv("KGPC_DEBUG_TYPECAST") != NULL &&
        pascal_identifier_equals(id, "TextRec")) {
      fprintf(stderr,
              "[SemCheck] try_typecast TextRec: not a type identifier (node=%p "
              "hash=%d target_type=%d)\n",
              (void *)type_node, type_node != NULL ? type_node->hash_type : -1,
              target_type);
    }
    free(id_copy);
    return 0;
  }

  /* Require exactly one argument for a typecast */
  ListNode_t *args = expr->expr_data.function_call_data.args_expr;
  if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
    fprintf(stderr, "[SemCheck] try_typecast id=%s args=%d\n", id,
            args != NULL ? ListLength(args) : 0);
  }
  if (args == NULL || args->next != NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, typecast to %s expects exactly one argument.\n",
        expr->line_num, id);
    *type_return = UNKNOWN_TYPE;
    free(id_copy);
    return 1;
  }

  struct Expression *inner_expr = (struct Expression *)args->cur;

  /* Clean up function-call-specific fields without freeing the inner expression
   */
  if (expr->expr_data.function_call_data.id != NULL) {
    free(expr->expr_data.function_call_data.id);
    expr->expr_data.function_call_data.id = NULL;
  }
  if (expr->expr_data.function_call_data.mangled_id != NULL) {
    free(expr->expr_data.function_call_data.mangled_id);
    expr->expr_data.function_call_data.mangled_id = NULL;
  }
  if (expr->expr_data.function_call_data.call_kgpc_type != NULL) {
    destroy_kgpc_type(expr->expr_data.function_call_data.call_kgpc_type);
    expr->expr_data.function_call_data.call_kgpc_type = NULL;
  }

  /* Manually free the argument list nodes but keep the expression alive */
  ListNode_t *to_free = args;
  while (to_free != NULL) {
    ListNode_t *next = to_free->next;
    to_free->cur = NULL;
    free(to_free);
    to_free = next;
  }
  expr->expr_data.function_call_data.args_expr = NULL;

  if (expr->expr_data.function_call_data.placeholder_method_name != NULL) {
    free(expr->expr_data.function_call_data.placeholder_method_name);
    expr->expr_data.function_call_data.placeholder_method_name = NULL;
  }
  if (expr->expr_data.function_call_data.call_qualifier != NULL) {
    free(expr->expr_data.function_call_data.call_qualifier);
    expr->expr_data.function_call_data.call_qualifier = NULL;
  }
  if (expr->expr_data.function_call_data.self_class_name != NULL) {
    free(expr->expr_data.function_call_data.self_class_name);
    expr->expr_data.function_call_data.self_class_name = NULL;
  }
  if (expr->expr_data.function_call_data.cached_owner_class != NULL) {
    free(expr->expr_data.function_call_data.cached_owner_class);
    expr->expr_data.function_call_data.cached_owner_class = NULL;
  }
  if (expr->expr_data.function_call_data.cached_method_name != NULL) {
    free(expr->expr_data.function_call_data.cached_method_name);
    expr->expr_data.function_call_data.cached_method_name = NULL;
  }
  if (expr->expr_data.function_call_data.procedural_var_expr != NULL) {
    destroy_expr(expr->expr_data.function_call_data.procedural_var_expr);
    expr->expr_data.function_call_data.procedural_var_expr = NULL;
  }
  if (expr->expr_data.function_call_data.constructor_receiver_expr != NULL) {
    destroy_expr(expr->expr_data.function_call_data.constructor_receiver_expr);
    expr->expr_data.function_call_data.constructor_receiver_expr = NULL;
  }

  /* Reinterpret as a typecast expression */
  expr->type = EXPR_TYPECAST;
  expr->expr_data.typecast_data.target_type = target_type;
  expr->expr_data.typecast_data.target_type_id = id_copy;
  expr->expr_data.typecast_data.expr = inner_expr;

  return semcheck_typecast(type_return, symtab, expr, max_scope_lev, NO_MUTATE);
}

int semcheck_reinterpret_typecast_as_call(int *type_return, SymTab_t *symtab,
                                          struct Expression *expr,
                                          int max_scope_lev) {
  if (expr == NULL || expr->expr_data.typecast_data.target_type_id == NULL)
    return 1;

  HashNode_t *func_node = NULL;
  int found_func =
      (FindSymbol(&func_node, symtab,
                  expr->expr_data.typecast_data.target_type_id) != 0 &&
       func_node != NULL);

  /* For dotted identifiers like "widestringmanager.UpperProc", if the full name
   * is not found as a function, check if the prefix is a variable/record - the
   * function call handler will split and resolve it as a record field call. */
  if (!found_func) {
    const char *prefix = expr->expr_data.typecast_data.type_qualifier;
    if (prefix != NULL) {
      HashNode_t *prefix_node = NULL;
      int prefix_found = (FindSymbol(&prefix_node, symtab, prefix) != 0 &&
                          prefix_node != NULL);
      if (prefix_found)
        found_func =
            1; /* Let the function call handler resolve the dotted name */
    }
  }

  /* If the bare name wasn't found, check if we're inside a method body
   * and the identifier could be a method of the current class. */
  if (!found_func) {
    const char *method_owner = semcheck_get_current_method_owner();
    if (method_owner != NULL &&
        expr->expr_data.typecast_data.target_type_id != NULL) {
      char mangled[512];
      snprintf(mangled, sizeof(mangled), "%s__%s", method_owner,
               expr->expr_data.typecast_data.target_type_id);
      HashNode_t *method_check = NULL;
      if (FindSymbol(&method_check, symtab, mangled) != 0 &&
          method_check != NULL &&
          (method_check->hash_type == HASHTYPE_FUNCTION ||
           method_check->hash_type == HASHTYPE_PROCEDURE)) {
        found_func = 1;
        func_node = method_check;
      }
      /* Also walk parent classes */
      if (!found_func) {
        HashNode_t *owner_node = NULL;
        if (FindSymbol(&owner_node, symtab, method_owner) != 0 &&
            owner_node != NULL) {
          struct RecordType *rec = get_record_type_from_node(owner_node);
          const char *parent = rec ? rec->parent_class_name : NULL;
          while (parent != NULL && !found_func) {
            snprintf(mangled, sizeof(mangled), "%s__%s", parent,
                     expr->expr_data.typecast_data.target_type_id);
            HashNode_t *parent_method = NULL;
            if (FindSymbol(&parent_method, symtab, mangled) != 0 &&
                parent_method != NULL &&
                (parent_method->hash_type == HASHTYPE_FUNCTION ||
                 parent_method->hash_type == HASHTYPE_PROCEDURE)) {
              found_func = 1;
              func_node = parent_method;
            } else {
              HashNode_t *parent_node = NULL;
              if (FindSymbol(&parent_node, symtab, parent) != 0 &&
                  parent_node != NULL) {
                struct RecordType *parent_rec =
                    get_record_type_from_node(parent_node);
                parent = parent_rec ? parent_rec->parent_class_name : NULL;
              } else
                parent = NULL;
            }
          }
        }
      }
    }
  }

  if (!found_func)
    return 1;

  if (func_node != NULL && func_node->hash_type != HASHTYPE_FUNCTION &&
      func_node->hash_type != HASHTYPE_PROCEDURE &&
      func_node->hash_type != HASHTYPE_BUILTIN_PROCEDURE) {
    /* For dotted identifiers, the func_node may be a variable (record) -
     * that's OK, the function call handler will resolve the field call */
    if (expr->expr_data.typecast_data.type_qualifier == NULL)
      return 1;
  }

  struct Expression *arg_expr = expr->expr_data.typecast_data.expr;
  expr->expr_data.typecast_data.expr = NULL;

  char *call_id = expr->expr_data.typecast_data.target_type_id;
  expr->expr_data.typecast_data.target_type_id = NULL;
  char *call_qualifier = expr->expr_data.typecast_data.type_qualifier;
  expr->expr_data.typecast_data.type_qualifier = NULL;
  TypeRef *target_type_ref = expr->expr_data.typecast_data.target_type_ref;
  expr->expr_data.typecast_data.target_type_ref = NULL;
  if (target_type_ref != NULL)
    type_ref_free(target_type_ref);

  expr->type = EXPR_FUNCTION_CALL;
  memset(&expr->expr_data.function_call_data, 0,
         sizeof(expr->expr_data.function_call_data));
  expr->expr_data.function_call_data.id = call_id;
  expr->expr_data.function_call_data.call_qualifier = call_qualifier;
  if (arg_expr != NULL)
    expr->expr_data.function_call_data.args_expr =
        CreateListNode(arg_expr, LIST_EXPR);

  /* Keep legacy_tag here - typecast reinterpreted as call needs re-checking */
  return semcheck_expr_legacy_tag(type_return, symtab, expr, max_scope_lev,
                                  NO_MUTATE);
}

/* Try to find a bare method name in the current class for @MethodName patterns.
 * Returns the method HashNode_t if found, NULL otherwise. */
static HashNode_t *find_implicit_self_method(SymTab_t *symtab,
                                             const char *name) {
  if (name == NULL)
    return NULL;
  const char *owner_id = semcheck_get_current_method_owner();
  if (owner_id == NULL)
    return NULL;
  HashNode_t *owner_node = NULL;
  if (FindSymbol(&owner_node, symtab, owner_id) == 0 || owner_node == NULL)
    return NULL;
  struct RecordType *owner_rec = NULL;
  if (owner_node->type != NULL) {
    if (kgpc_type_is_record(owner_node->type))
      owner_rec = kgpc_type_get_record(owner_node->type);
    else if (kgpc_type_is_pointer(owner_node->type) &&
             owner_node->type->info.points_to != NULL &&
             kgpc_type_is_record(owner_node->type->info.points_to))
      owner_rec = kgpc_type_get_record(owner_node->type->info.points_to);
  }
  if (owner_rec == NULL)
    return NULL;
  HashNode_t *method_node =
      semcheck_find_class_method(symtab, owner_rec, name, NULL);
  if (method_node != NULL && (method_node->hash_type == HASHTYPE_FUNCTION ||
                              method_node->hash_type == HASHTYPE_PROCEDURE))
    return method_node;
  return NULL;
}

static char *semcheck_dup_proc_target_symbol(SymTab_t *symtab,
                                             HashNode_t *target) {
  if (target == NULL)
    return NULL;

  const char *target_name = NULL;

  if (target->internproc_id != NULL && target->internproc_id[0] != '\0')
    target_name = target->internproc_id;

  if ((target_name == NULL || target_name[0] == '\0') && target->type != NULL &&
      target->type->kind == TYPE_KIND_PROCEDURE) {
    Tree_t *proc_def = target->type->info.proc_info.definition;
    if (proc_def != NULL) {
      if (proc_def->tree_data.subprogram_data.cname_override != NULL &&
          proc_def->tree_data.subprogram_data.cname_override[0] != '\0')
        target_name = proc_def->tree_data.subprogram_data.cname_override;
      else if (proc_def->tree_data.subprogram_data.cname_flag &&
               proc_def->tree_data.subprogram_data.id != NULL &&
               proc_def->tree_data.subprogram_data.id[0] != '\0')
        target_name = proc_def->tree_data.subprogram_data.id;
      else if (proc_def->tree_data.subprogram_data.mangled_id != NULL &&
               proc_def->tree_data.subprogram_data.mangled_id[0] != '\0')
        target_name = proc_def->tree_data.subprogram_data.mangled_id;
    }
  }

  if ((target_name == NULL || target_name[0] == '\0') && symtab != NULL &&
      target->owner_class != NULL && target->method_name != NULL) {
    HashNode_t *prefix_match = NULL;
    size_t base_len =
        strlen(target->owner_class) + 2 + strlen(target->method_name) + 1;
    char *base_name = (char *)malloc(base_len);
    if (base_name != NULL) {
      snprintf(base_name, base_len, "%s__%s", target->owner_class,
               target->method_name);
      ListNode_t *matches = FindAllIdents(symtab, base_name);
      for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        if (cand == NULL || cand->type == NULL ||
            cand->type->kind != TYPE_KIND_PROCEDURE)
          continue;
        Tree_t *proc_def = cand->type->info.proc_info.definition;
        if (proc_def == NULL)
          continue;
        if (proc_def->tree_data.subprogram_data.cname_override != NULL &&
            proc_def->tree_data.subprogram_data.cname_override[0] != '\0') {
          target_name = proc_def->tree_data.subprogram_data.cname_override;
          break;
        }
        if (proc_def->tree_data.subprogram_data.mangled_id != NULL &&
            proc_def->tree_data.subprogram_data.mangled_id[0] != '\0') {
          target_name = proc_def->tree_data.subprogram_data.mangled_id;
          break;
        }
        if (cand->mangled_id != NULL && cand->mangled_id[0] != '\0') {
          target_name = cand->mangled_id;
          break;
        }
      }
      if (matches != NULL)
        DestroyList(matches);
      if ((target_name == NULL || target_name[0] == '\0') &&
          FindIdentByPrefix(&prefix_match, symtab, base_name) != 0 &&
          prefix_match != NULL && prefix_match->type != NULL &&
          prefix_match->type->kind == TYPE_KIND_PROCEDURE) {
        Tree_t *proc_def = prefix_match->type->info.proc_info.definition;
        if (proc_def != NULL &&
            proc_def->tree_data.subprogram_data.cname_override != NULL &&
            proc_def->tree_data.subprogram_data.cname_override[0] != '\0') {
          target_name = proc_def->tree_data.subprogram_data.cname_override;
        } else if (proc_def != NULL &&
                   proc_def->tree_data.subprogram_data.mangled_id != NULL &&
                   proc_def->tree_data.subprogram_data.mangled_id[0] != '\0') {
          target_name = proc_def->tree_data.subprogram_data.mangled_id;
        } else if (prefix_match->mangled_id != NULL &&
                   prefix_match->mangled_id[0] != '\0') {
          target_name = prefix_match->mangled_id;
        }
      }
      free(base_name);
    }
  }

  if ((target_name == NULL || target_name[0] == '\0') &&
      target->mangled_id != NULL && target->mangled_id[0] != '\0')
    target_name = target->mangled_id;

  if ((target_name == NULL || target_name[0] == '\0') && target->id != NULL &&
      target->id[0] != '\0')
    target_name = target->id;

  return target_name != NULL ? strdup(target_name) : NULL;
}

/* The specialize_bare parser rule (pascal_expression.c) now handles
 * @specialize Type<Args>.Method structurally via PASCAL_T_CONSTRUCTED_TYPE
 * AST nodes, eliminating the need for source-buffer re-parsing. */

int semcheck_addressof(int *type_return, SymTab_t *symtab,
                       struct Expression *expr, int max_scope_lev,
                       int mutating) {
  (void)mutating;

  assert(type_return != NULL);
  assert(symtab != NULL);
  assert(expr != NULL);
  assert(expr->type == EXPR_ADDR);

  semcheck_clear_pointer_info(expr);

  struct Expression *inner = expr->expr_data.addr_data.expr;
  if (inner == NULL) {
    semcheck_error_with_context_at(
        expr->line_num, expr->col_num, expr->source_index,
        "Error on line %d, address-of operator requires an operand.\\n\\n",
        expr->line_num);
    *type_return = UNKNOWN_TYPE;
    return 1;
  }

  int error_count = 0;
  int inner_type = UNKNOWN_TYPE;
  int treated_as_proc_ref = 0;
  HashNode_t *resolved_proc_symbol = NULL;
  int dbg_specialize_addr = kgpc_getenv("KGPC_DEBUG_ADDR_SPECIALIZE") != NULL;

  if (dbg_specialize_addr && expr->line_num == 256) {
    fprintf(
        stderr,
        "[ADDR-SPECIALIZE] line=%d source_index=%d inner_type=%d inner_id=%s\n",
        expr->line_num, expr->source_index, inner->type,
        (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL)
            ? inner->expr_data.id
            : "(n/a)");
  }

  /* If operand is a bare function/procedure identifier, don't auto-convert to a
   * call */
  if (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL) {
    HashNode_t *inner_symbol = NULL;
    if (FindSymbol(&inner_symbol, symtab, inner->expr_data.id) &&
        inner_symbol != NULL &&
        (inner_symbol->hash_type == HASHTYPE_FUNCTION ||
         inner_symbol->hash_type == HASHTYPE_PROCEDURE ||
         inner_symbol->hash_type == HASHTYPE_BUILTIN_PROCEDURE)) {
      inner_type = PROCEDURE;
      treated_as_proc_ref = 1;
      resolved_proc_symbol = inner_symbol;
    }
    /* Fallback: bare method name inside a method body (e.g. @ReadData
     * where ReadData is a method of the current class). */
    if (!treated_as_proc_ref &&
        find_implicit_self_method(symtab, inner->expr_data.id) != NULL) {
      inner_type = PROCEDURE;
      treated_as_proc_ref = 1;
    }
  }
  /* Also check if inner is already a FUNCTION_CALL with no args - this can
   * happen when the parser sees a function identifier and auto-converts it to a
   * call. In the @FunctionName case, we don't want to resolve overloads - we
   * want the address. */
  else if (inner->type == EXPR_FUNCTION_CALL &&
           inner->expr_data.function_call_data.args_expr == NULL) {
    const char *func_id = inner->expr_data.function_call_data.id;
    if (func_id != NULL) {
      HashNode_t *inner_symbol = NULL;
      if (FindSymbol(&inner_symbol, symtab, func_id) && inner_symbol != NULL &&
          (inner_symbol->hash_type == HASHTYPE_FUNCTION ||
           inner_symbol->hash_type == HASHTYPE_PROCEDURE ||
           inner_symbol->hash_type == HASHTYPE_BUILTIN_PROCEDURE)) {
        /* This is @FunctionName where FunctionName was auto-converted to a
         * call. Skip overload resolution - we just want the function's address.
         */
        inner_type = PROCEDURE;
        treated_as_proc_ref = 1;
        resolved_proc_symbol = inner_symbol;
      }
      /* Fallback: bare method name auto-converted to call inside a method body
       */
      if (!treated_as_proc_ref &&
          find_implicit_self_method(symtab, func_id) != NULL) {
        inner_type = PROCEDURE;
        treated_as_proc_ref = 1;
      }
    }
  } else if (inner->type == EXPR_FUNCTION_CALL &&
             inner->expr_data.function_call_data.args_expr != NULL &&
             (inner->expr_data.function_call_data.is_method_call_placeholder ||
              inner->expr_data.function_call_data.placeholder_method_name !=
                  NULL)) {
    const char *method_id =
        inner->expr_data.function_call_data.placeholder_method_name;
    if (method_id == NULL)
      method_id = inner->expr_data.function_call_data.id;

    ListNode_t *arg0 = inner->expr_data.function_call_data.args_expr;
    struct Expression *receiver_expr = (arg0 != NULL && arg0->type == LIST_EXPR)
                                           ? (struct Expression *)arg0->cur
                                           : NULL;
    struct RecordType *rec_info = NULL;

    if (receiver_expr != NULL) {
      if (receiver_expr->type == EXPR_VAR_ID &&
          receiver_expr->expr_data.id != NULL) {
        HashNode_t *owner_node = NULL;
        if (FindSymbol(&owner_node, symtab, receiver_expr->expr_data.id) != 0 &&
            owner_node != NULL && owner_node->hash_type == HASHTYPE_TYPE &&
            owner_node->type != NULL) {
          KgpcType *owner_type = owner_node->type;
          if (kgpc_type_is_record(owner_type))
            rec_info = kgpc_type_get_record(owner_type);
          else if (kgpc_type_is_pointer(owner_type) &&
                   owner_type->info.points_to != NULL &&
                   kgpc_type_is_record(owner_type->info.points_to))
            rec_info = kgpc_type_get_record(owner_type->info.points_to);
        }
      }

      if (rec_info == NULL) {
        KgpcType *receiver_type = NULL;
        semcheck_expr_with_type(&receiver_type, symtab, receiver_expr,
                                max_scope_lev, NO_MUTATE);
        if (receiver_type != NULL) {
          if (kgpc_type_is_record(receiver_type))
            rec_info = kgpc_type_get_record(receiver_type);
          else if (kgpc_type_is_pointer(receiver_type) &&
                   receiver_type->info.points_to != NULL &&
                   kgpc_type_is_record(receiver_type->info.points_to))
            rec_info = kgpc_type_get_record(receiver_type->info.points_to);
        }
      }
    }

    if (method_id != NULL) {
      HashNode_t *method_node = NULL;
      if (rec_info != NULL)
        method_node =
            semcheck_find_class_method(symtab, rec_info, method_id, NULL);

      if (method_node == NULL) {
        const char *owner = semcheck_get_current_method_owner();
        if (owner != NULL) {
          size_t mlen = strlen(owner) + 2 + strlen(method_id);
          char *mangled_base = (char *)malloc(mlen + 1);
          if (mangled_base != NULL) {
            snprintf(mangled_base, mlen + 1, "%s__%s", owner, method_id);
            ListNode_t *matches = FindAllIdents(symtab, mangled_base);
            if (matches != NULL) {
              for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
                HashNode_t *candidate = (HashNode_t *)cur->cur;
                if (candidate != NULL &&
                    (candidate->hash_type == HASHTYPE_FUNCTION ||
                     candidate->hash_type == HASHTYPE_PROCEDURE)) {
                  method_node = candidate;
                  break;
                }
              }
              DestroyList(matches);
            }
            free(mangled_base);
          }
        }
      }

      if (method_node == NULL) {
        method_node = semcheck_find_any_proc_symbol(symtab, method_id);
      }

      if (method_node == NULL) {
        size_t len = strlen(method_id);
        char *prefixed = (char *)malloc(len + 3);
        if (prefixed != NULL) {
          snprintf(prefixed, len + 3, "__%s", method_id);
          method_node = semcheck_find_any_proc_symbol(symtab, prefixed);
          free(prefixed);
        }
      }

      if (method_node != NULL &&
          (method_node->hash_type == HASHTYPE_FUNCTION ||
           method_node->hash_type == HASHTYPE_PROCEDURE)) {
        inner_type = PROCEDURE;
        treated_as_proc_ref = 1;
        resolved_proc_symbol = method_node;
      }
    }
  }
  /* Handle @obj.Method - address of a method through record access.
   * The inner expression is EXPR_RECORD_ACCESS where the field is a method
   * name. We need to detect this before semcheck converts it to a function
   * call. */
  else if (inner->type == EXPR_RECORD_ACCESS &&
           inner->expr_data.record_access_data.field_id != NULL) {
    const char *field_id = inner->expr_data.record_access_data.field_id;
    struct Expression *record_expr =
        inner->expr_data.record_access_data.record_expr;
    if (record_expr != NULL) {
      struct RecordType *rec_info = NULL;
      int skip_record_expr_semcheck = 0;

      if (record_expr->type == EXPR_VAR_ID &&
          record_expr->expr_data.id != NULL) {
        HashNode_t *owner_node = NULL;
        if (FindSymbol(&owner_node, symtab, record_expr->expr_data.id) != 0 &&
            owner_node != NULL && owner_node->hash_type == HASHTYPE_TYPE &&
            owner_node->type != NULL) {
          KgpcType *owner_type = owner_node->type;
          if (kgpc_type_is_record(owner_type))
            rec_info = kgpc_type_get_record(owner_type);
          else if (kgpc_type_is_pointer(owner_type) &&
                   owner_type->info.points_to != NULL &&
                   kgpc_type_is_record(owner_type->info.points_to))
            rec_info = kgpc_type_get_record(owner_type->info.points_to);
        } else if (dbg_specialize_addr) {
          fprintf(stderr, "[ADDR-SPECIALIZE] owner type not found for %s\n",
                  record_expr->expr_data.id);
        }
        if (inner->is_specialize_addr_target) {
          skip_record_expr_semcheck = 1;
        }
      }

      /* Resolve the base expression to get its record type */
      if (rec_info == NULL && !skip_record_expr_semcheck) {
        KgpcType *record_kgpc = NULL;
        semcheck_expr_with_type(&record_kgpc, symtab, record_expr,
                                max_scope_lev, NO_MUTATE);
        int record_tag = semcheck_tag_from_kgpc(record_kgpc);
        if (record_tag == RECORD_TYPE && record_kgpc != NULL &&
            kgpc_type_is_record(record_kgpc))
          rec_info = kgpc_type_get_record(record_kgpc);
        else if (record_tag == POINTER_TYPE && record_kgpc != NULL &&
                 kgpc_type_is_pointer(record_kgpc) &&
                 record_kgpc->info.points_to != NULL &&
                 kgpc_type_is_record(record_kgpc->info.points_to))
          rec_info = kgpc_type_get_record(record_kgpc->info.points_to);
      }

      if (rec_info != NULL) {
        HashNode_t *method_node =
            semcheck_find_class_method(symtab, rec_info, field_id, NULL);
        if (method_node != NULL &&
            (method_node->hash_type == HASHTYPE_FUNCTION ||
             method_node->hash_type == HASHTYPE_PROCEDURE)) {
          if (dbg_specialize_addr)
            fprintf(stderr,
                    "[ADDR-SPECIALIZE] method resolved on type receiver: %s\n",
                    field_id);
          inner_type = PROCEDURE;
          treated_as_proc_ref = 1;
          resolved_proc_symbol = method_node;
        } else if (dbg_specialize_addr) {
          fprintf(stderr,
                  "[ADDR-SPECIALIZE] method not found on type receiver: %s\n",
                  field_id);
        }
      } else if (field_id != NULL) {
        HashNode_t *fallback_symbol =
            semcheck_find_any_proc_symbol(symtab, field_id);
        if (fallback_symbol == NULL && record_expr != NULL &&
            record_expr->type == EXPR_VAR_ID &&
            record_expr->expr_data.id != NULL &&
            pascal_identifier_equals(record_expr->expr_data.id, "Self")) {
          const char *owner = semcheck_get_current_method_owner();
          if (owner != NULL) {
            size_t mlen = strlen(owner) + 2 + strlen(field_id);
            char *mangled_base = (char *)malloc(mlen + 1);
            if (mangled_base != NULL) {
              snprintf(mangled_base, mlen + 1, "%s__%s", owner, field_id);
              ListNode_t *matches = FindAllIdents(symtab, mangled_base);
              if (matches != NULL) {
                HashNode_t *owner_match = NULL;
                HashNode_t *first_proc = NULL;
                for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
                  HashNode_t *candidate = (HashNode_t *)cur->cur;
                  if (candidate == NULL ||
                      (candidate->hash_type != HASHTYPE_FUNCTION &&
                       candidate->hash_type != HASHTYPE_PROCEDURE) ||
                      candidate->type == NULL)
                    continue;
                  if (first_proc == NULL)
                    first_proc = candidate;

                  ListNode_t *params =
                      kgpc_type_get_procedure_params(candidate->type);
                  if (params == NULL || params->cur == NULL)
                    continue;
                  Tree_t *first_decl = (Tree_t *)params->cur;
                  const char *first_type_id = NULL;
                  if (first_decl->type == TREE_VAR_DECL)
                    first_type_id = first_decl->tree_data.var_decl_data.type_id;
                  else if (first_decl->type == TREE_ARR_DECL)
                    first_type_id = first_decl->tree_data.arr_decl_data.type_id;
                  if (first_type_id != NULL &&
                      pascal_identifier_equals(first_type_id, owner)) {
                    owner_match = candidate;
                    break;
                  }
                }
                fallback_symbol =
                    (owner_match != NULL) ? owner_match : first_proc;
                DestroyList(matches);
              }
              free(mangled_base);
            }
          }
        }
        if (fallback_symbol == NULL) {
          size_t len = strlen(field_id);
          char *prefixed = (char *)malloc(len + 3);
          if (prefixed != NULL) {
            snprintf(prefixed, len + 3, "__%s", field_id);
            fallback_symbol = semcheck_find_any_proc_symbol(symtab, prefixed);
            free(prefixed);
          }
        }
        if (fallback_symbol != NULL) {
          if (dbg_specialize_addr)
            fprintf(
                stderr,
                "[ADDR-SPECIALIZE] fallback proc symbol resolved by name: %s\n",
                field_id);
          inner_type = PROCEDURE;
          treated_as_proc_ref = 1;
          resolved_proc_symbol = fallback_symbol;
        } else if (dbg_specialize_addr) {
          fprintf(stderr,
                  "[ADDR-SPECIALIZE] fallback proc symbol not found: %s\n",
                  field_id);
        }

        /* Keep @TypeLike.Method on the procedure-reference path even if
         * symbol lookup is deferred/unavailable in this pass. */
        if (fallback_symbol == NULL && record_expr->type == EXPR_VAR_ID &&
            record_expr->expr_data.id != NULL &&
            inner->is_specialize_addr_target) {
          inner_type = PROCEDURE;
          treated_as_proc_ref = 1;
        }
      }
    }
  }

  if (!treated_as_proc_ref) {
    KgpcType *inner_kgpc_type = NULL;
    semcheck_expr_with_type(&inner_kgpc_type, symtab, inner, max_scope_lev,
                            NO_MUTATE);
    inner_type = semcheck_tag_from_kgpc(inner_kgpc_type);
  }

  /* Special case: address-of array expressions (array variables/fields). */
  if (inner != NULL && inner->resolved_kgpc_type != NULL &&
      kgpc_type_is_array(inner->resolved_kgpc_type)) {
    if (kgpc_getenv("KGPC_ASSERT_ADDROF_ARRAY") != NULL)
      assert(kgpc_type_is_array(inner->resolved_kgpc_type));
    KgpcType *array_type = inner->resolved_kgpc_type;
    KgpcType *element_type =
        kgpc_type_get_array_element_type_resolved(array_type, symtab);
    int element_tag = UNKNOWN_TYPE;
    const char *element_type_id = NULL;
    if (element_type != NULL) {
      element_tag = semcheck_tag_from_kgpc(element_type);
      if (element_type->type_alias != NULL) {
        element_type_id = element_type->type_alias->alias_name != NULL
                              ? element_type->type_alias->alias_name
                              : element_type->type_alias->target_type_id;
      }
    }

    semcheck_set_pointer_info(expr, element_tag, element_type_id);
    *type_return = POINTER_TYPE;

    /* Preserve pointer-to-array KgpcType for overloads like pSigSet. */
    KgpcType *ptr_type = create_pointer_type(array_type);
    if (ptr_type != NULL) {
      semcheck_expr_set_resolved_kgpc_type_shared(expr, ptr_type);
      destroy_kgpc_type(ptr_type);
    }
    return error_count;
  }

  /* Special case: If the inner expression was auto-converted from a function
   * identifier to a function call (because we're in NO_MUTATE mode), we need to
   * reverse that since we're taking the address of the function, not calling
   * it. */
  int converted_to_proc_addr =
      treated_as_proc_ref; /* Already converted if we treated it as proc ref */
  if (!converted_to_proc_addr && inner->type == EXPR_FUNCTION_CALL &&
      inner->expr_data.function_call_data.args_expr == NULL) {
    const char *func_id = inner->expr_data.function_call_data.id;
    if (func_id != NULL) {
      HashNode_t *func_symbol = NULL;
      if (FindSymbol(&func_symbol, symtab, func_id) != 0 &&
          func_symbol != NULL &&
          (func_symbol->hash_type == HASHTYPE_FUNCTION ||
           func_symbol->hash_type == HASHTYPE_PROCEDURE ||
           func_symbol->hash_type == HASHTYPE_BUILTIN_PROCEDURE)) {
        /* This was auto-converted - treat it as a procedure reference instead
         */
        inner_type = PROCEDURE;
        converted_to_proc_addr = 1;
        /* We'll handle this below in the PROCEDURE case */
      }
    }
  }

  if (inner_type == UNKNOWN_TYPE) {
    if (inner->type == EXPR_VAR_ID && inner->expr_data.id != NULL) {
      HashNode_t *inner_symbol = NULL;
      if (FindSymbol(&inner_symbol, symtab, inner->expr_data.id) != 0 &&
          inner_symbol != NULL && inner_symbol->type == NULL &&
          inner_symbol->is_var_parameter) {
        *type_return = POINTER_TYPE;
        if (expr->resolved_kgpc_type != NULL)
          destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = create_pointer_type(NULL);
        return error_count;
      }
    }
    *type_return = UNKNOWN_TYPE;
    return error_count;
  }

  const char *type_id = NULL;
  if (inner_type == POINTER_TYPE && inner->pointer_subtype_id != NULL)
    type_id = inner->pointer_subtype_id;
  else if (inner_type == CHAR_TYPE) {
    /* Preserve character subtype identity for @string[index] cases,
     * especially WideString/UnicodeString element addresses. */
    if (inner->array_element_type_id != NULL)
      type_id = inner->array_element_type_id;
    else if (inner->resolved_kgpc_type != NULL) {
      struct TypeAlias *inner_alias =
          kgpc_type_get_type_alias(inner->resolved_kgpc_type);
      if (inner_alias != NULL) {
        if (inner_alias->alias_name != NULL)
          type_id = inner_alias->alias_name;
        else if (inner_alias->target_type_id != NULL)
          type_id = inner_alias->target_type_id;
      }
    }
  }

  struct RecordType *record_info = NULL;
  if (inner->resolved_kgpc_type != NULL) {
    if (inner_type == RECORD_TYPE &&
        kgpc_type_is_record(inner->resolved_kgpc_type))
      record_info = kgpc_type_get_record(inner->resolved_kgpc_type);
    else if (inner_type == POINTER_TYPE &&
             kgpc_type_is_pointer(inner->resolved_kgpc_type) &&
             inner->resolved_kgpc_type->info.points_to != NULL &&
             kgpc_type_is_record(inner->resolved_kgpc_type->info.points_to))
      record_info =
          kgpc_type_get_record(inner->resolved_kgpc_type->info.points_to);
  }

  semcheck_set_pointer_info(expr, inner_type, type_id);
  *type_return = POINTER_TYPE;

  /* Create a proper KgpcType for the address-of expression */
  KgpcType *pointed_to_type = NULL;

  /* Convert inner_type to KgpcType */
  if (inner_type == INT_TYPE) {
    pointed_to_type = create_primitive_type(INT_TYPE);
  } else if (inner_type == LONGINT_TYPE) {
    pointed_to_type = create_primitive_type(LONGINT_TYPE);
  } else if (inner_type == REAL_TYPE) {
    pointed_to_type = create_primitive_type(REAL_TYPE);
  } else if (inner_type == CHAR_TYPE) {
    /* Keep resolved char subtype (e.g. WideChar) rather than collapsing to
     * plain Char; overload resolution depends on this metadata. */
    if (inner->resolved_kgpc_type != NULL &&
        kgpc_type_is_char(inner->resolved_kgpc_type)) {
      pointed_to_type = inner->resolved_kgpc_type;
      kgpc_type_retain(pointed_to_type);
    } else {
      pointed_to_type = create_primitive_type(CHAR_TYPE);
    }
  } else if (inner_type == STRING_TYPE) {
    pointed_to_type = create_primitive_type(STRING_TYPE);
  } else if (inner_type == RECORD_TYPE && record_info != NULL) {
    pointed_to_type = create_record_type(record_info);
  } else if (inner_type == RECORD_TYPE && record_info == NULL) {
    /* Record type without record_info — use inner's resolved KgpcType if
     * available */
    if (inner->resolved_kgpc_type != NULL) {
      pointed_to_type = inner->resolved_kgpc_type;
      kgpc_type_retain(pointed_to_type);
    } else {
      pointed_to_type = create_primitive_type(RECORD_TYPE);
    }
  } else if (inner_type == POINTER_TYPE) {
    /* For pointer types, get the resolved KgpcType of the inner expression */
    if (inner->resolved_kgpc_type != NULL) {
      pointed_to_type = inner->resolved_kgpc_type;
      kgpc_type_retain(pointed_to_type); /* We're taking a reference */
    } else {
      /* Fallback: create untyped pointer */
      pointed_to_type = NULL;
    }
  } else if (inner_type == PROCEDURE) {
    int proc_type_owned = 0;
    KgpcType *proc_type = NULL;

    /* For procedures/functions, we need the actual procedural type, not the
     * return type. semcheck_resolve_expression_kgpc_type returns the return
     * type for functions, so we look up the symbol directly instead. */
    const char *proc_id = NULL;
    if (inner->type == EXPR_VAR_ID) {
      proc_id = inner->expr_data.id;
    } else if (inner->type == EXPR_FUNCTION_CALL &&
               inner->expr_data.function_call_data.args_expr == NULL) {
      proc_id = inner->expr_data.function_call_data.id;
    }

    if (proc_id != NULL) {
      HashNode_t *proc_symbol = NULL;
      if (FindSymbol(&proc_symbol, symtab, proc_id) != 0 &&
          proc_symbol != NULL &&
          (proc_symbol->hash_type == HASHTYPE_PROCEDURE ||
           proc_symbol->hash_type == HASHTYPE_FUNCTION) &&
          proc_symbol->type != NULL &&
          proc_symbol->type->kind == TYPE_KIND_PROCEDURE) {
        /* Use the procedure type from the symbol */
        proc_type = proc_symbol->type;
        proc_type_owned = 0; /* Shared reference */
      }
      /* Fallback: implicit Self method */
      if (proc_type == NULL) {
        HashNode_t *method_node = find_implicit_self_method(symtab, proc_id);
        if (method_node != NULL && method_node->type != NULL &&
            method_node->type->kind == TYPE_KIND_PROCEDURE) {
          proc_type = method_node->type;
          proc_type_owned = 0;
        }
      }
    } else if (resolved_proc_symbol != NULL &&
               resolved_proc_symbol->type != NULL &&
               resolved_proc_symbol->type->kind == TYPE_KIND_PROCEDURE) {
      proc_type = resolved_proc_symbol->type;
      proc_type_owned = 0;
    }

    if (proc_type != NULL) {
      if (!proc_type_owned)
        kgpc_type_retain(proc_type);
      pointed_to_type = proc_type;
    }
    /* Handle both EXPR_VAR_ID (for procedures) and EXPR_FUNCTION_CALL (for
     * functions that were auto-converted) */
    if (inner->type == EXPR_VAR_ID) {
      HashNode_t *proc_symbol = NULL;
      HashNode_t *implicit_method = NULL;
      if (FindSymbol(&proc_symbol, symtab, inner->expr_data.id) != 0 &&
          proc_symbol != NULL &&
          (proc_symbol->hash_type == HASHTYPE_PROCEDURE ||
           proc_symbol->hash_type == HASHTYPE_FUNCTION)) {
        /* Found directly */
      }
      /* Also try implicit Self method lookup; prefer it when the direct symbol
       * does not carry a stable emitted target for a class method reference. */
      implicit_method = find_implicit_self_method(symtab, inner->expr_data.id);
      if (implicit_method != NULL &&
          (proc_symbol == NULL || proc_symbol->owner_class == NULL ||
           proc_symbol->method_name == NULL ||
           proc_symbol->mangled_id == NULL ||
           proc_symbol->mangled_id[0] == '\0' ||
           (proc_symbol->id != NULL &&
            strcmp(proc_symbol->mangled_id, proc_symbol->id) == 0))) {
        proc_symbol = implicit_method;
      }
      if (proc_symbol != NULL &&
          (proc_symbol->hash_type == HASHTYPE_PROCEDURE ||
           proc_symbol->hash_type == HASHTYPE_FUNCTION)) {
        /* If this resolves to a class method via implicit Self,
         * synthesise a `Self` reference as the receiver so codegen
         * can build a TMethod descriptor.  Static class methods
         * have no Self and thus no receiver.  */
        int is_implicit_method = (proc_symbol == implicit_method) ||
                                 (proc_symbol->owner_class != NULL &&
                                  proc_symbol->method_name != NULL);
        int is_static_method = 0;
        if (proc_symbol->owner_class != NULL &&
            proc_symbol->method_name != NULL) {
          is_static_method = from_cparser_is_method_static(
              proc_symbol->owner_class, proc_symbol->method_name);
        }
        struct Expression *receiver_expr_local = NULL;
        if (is_implicit_method && !is_static_method) {
          HashNode_t *self_node = NULL;
          if (FindSymbol(&self_node, symtab, "Self") != 0 &&
              self_node != NULL) {
            receiver_expr_local = mk_varid(0, strdup("Self"));
          }
        }

        expr->expr_data.addr_data.expr = NULL;
        destroy_expr(inner);
        expr->type = EXPR_ADDR_OF_PROC;
        expr->expr_data.addr_of_proc_data.proc_mangled_id =
            semcheck_dup_proc_target_symbol(symtab, proc_symbol);
        expr->expr_data.addr_of_proc_data.proc_id =
            proc_symbol->id ? strdup(proc_symbol->id) : NULL;
        expr->expr_data.addr_of_proc_data.source_unit_index =
            proc_symbol->source_unit_index;
        expr->expr_data.addr_of_proc_data.receiver_expr = receiver_expr_local;
        /* Resolve the type NOW while the symbol is still alive,
         * instead of relying on procedure_symbol later. */
        if (proc_symbol->type != NULL &&
            proc_symbol->type->kind == TYPE_KIND_PROCEDURE) {
          kgpc_type_retain(proc_symbol->type);
          expr->resolved_kgpc_type = create_pointer_type(proc_symbol->type);
          kgpc_type_release(proc_symbol->type);
        } else {
          KgpcType *generic_proc = create_procedure_type(NULL, NULL);
          expr->resolved_kgpc_type = create_pointer_type(generic_proc);
          if (generic_proc != NULL)
            destroy_kgpc_type(generic_proc);
        }
        converted_to_proc_addr = 1;
      }
    } else if (inner->type == EXPR_FUNCTION_CALL &&
               inner->expr_data.function_call_data.args_expr == NULL) {
      /* This was auto-converted from a function identifier - get the original
       * symbol */
      const char *func_id = inner->expr_data.function_call_data.id;
      if (func_id != NULL) {
        HashNode_t *proc_symbol = NULL;
        HashNode_t *implicit_method = NULL;
        if (FindSymbol(&proc_symbol, symtab, func_id) != 0 &&
            proc_symbol != NULL &&
            (proc_symbol->hash_type == HASHTYPE_FUNCTION ||
             proc_symbol->hash_type == HASHTYPE_PROCEDURE)) {
          /* Found directly */
        }
        implicit_method = find_implicit_self_method(symtab, func_id);
        if (implicit_method != NULL &&
            (proc_symbol == NULL || proc_symbol->owner_class == NULL ||
             proc_symbol->method_name == NULL ||
             proc_symbol->mangled_id == NULL ||
             proc_symbol->mangled_id[0] == '\0' ||
             (proc_symbol->id != NULL &&
              strcmp(proc_symbol->mangled_id, proc_symbol->id) == 0))) {
          proc_symbol = implicit_method;
        }
        if (proc_symbol != NULL &&
            (proc_symbol->hash_type == HASHTYPE_FUNCTION ||
             proc_symbol->hash_type == HASHTYPE_PROCEDURE)) {
          expr->expr_data.addr_data.expr = NULL;
          destroy_expr(inner);
          expr->type = EXPR_ADDR_OF_PROC;
          expr->expr_data.addr_of_proc_data.proc_mangled_id =
              semcheck_dup_proc_target_symbol(symtab, proc_symbol);
          expr->expr_data.addr_of_proc_data.proc_id =
              proc_symbol->id ? strdup(proc_symbol->id) : NULL;
          expr->expr_data.addr_of_proc_data.source_unit_index =
              proc_symbol->source_unit_index;
          /* Resolve the type NOW while the symbol is still alive. */
          if (proc_symbol->type != NULL &&
              proc_symbol->type->kind == TYPE_KIND_PROCEDURE) {
            kgpc_type_retain(proc_symbol->type);
            expr->resolved_kgpc_type = create_pointer_type(proc_symbol->type);
            kgpc_type_release(proc_symbol->type);
          } else {
            KgpcType *generic_proc = create_procedure_type(NULL, NULL);
            expr->resolved_kgpc_type = create_pointer_type(generic_proc);
            if (generic_proc != NULL)
              destroy_kgpc_type(generic_proc);
          }
          converted_to_proc_addr = 1;
        }
      }
    } else if (inner->type == EXPR_RECORD_ACCESS &&
               inner->expr_data.record_access_data.field_id != NULL) {
      /* @obj.Method - get the method symbol from the record type */
      const char *field_id = inner->expr_data.record_access_data.field_id;
      struct Expression *record_expr =
          inner->expr_data.record_access_data.record_expr;
      struct RecordType *rec_info = NULL;

      if (record_expr != NULL && record_expr->resolved_kgpc_type != NULL) {
        KgpcType *rt = record_expr->resolved_kgpc_type;
        if (kgpc_type_is_record(rt))
          rec_info = kgpc_type_get_record(rt);
        else if (kgpc_type_is_pointer(rt) && rt->info.points_to != NULL &&
                 kgpc_type_is_record(rt->info.points_to))
          rec_info = kgpc_type_get_record(rt->info.points_to);
      }
      if (rec_info == NULL && record_expr != NULL &&
          record_expr->type == EXPR_VAR_ID &&
          record_expr->expr_data.id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, symtab, record_expr->expr_data.id) != 0 &&
            type_node != NULL && type_node->hash_type == HASHTYPE_TYPE) {
          rec_info = get_record_type_from_node(type_node);
          if (rec_info == NULL && type_node->type != NULL) {
            if (kgpc_type_is_record(type_node->type))
              rec_info = kgpc_type_get_record(type_node->type);
            else if (kgpc_type_is_pointer(type_node->type) &&
                     type_node->type->info.points_to != NULL &&
                     kgpc_type_is_record(type_node->type->info.points_to))
              rec_info = kgpc_type_get_record(type_node->type->info.points_to);
          }
        }
      }

      if (rec_info != NULL) {
        struct RecordType *actual_owner = NULL;
        HashNode_t *method_node = semcheck_find_class_method(
            symtab, rec_info, field_id, &actual_owner);
        if (method_node != NULL &&
            (method_node->hash_type == HASHTYPE_FUNCTION ||
             method_node->hash_type == HASHTYPE_PROCEDURE)) {
          /* Save the receiver expression before destroying the
           * inner record-access node — it carries the Self
           * pointer needed for TMethod construction. */
          struct Expression *saved_receiver =
              inner->expr_data.record_access_data.record_expr;
          inner->expr_data.record_access_data.record_expr = NULL;
          if (rec_info != NULL && rec_info->type_id != NULL &&
              from_cparser_is_method_static(rec_info->type_id, field_id)) {
            destroy_expr(saved_receiver);
            saved_receiver = NULL;
          }

          expr->expr_data.addr_data.expr = NULL;
          destroy_expr(inner);
          expr->type = EXPR_ADDR_OF_PROC;
          expr->expr_data.addr_of_proc_data.proc_mangled_id =
              semcheck_dup_proc_target_symbol(symtab, method_node);
          expr->expr_data.addr_of_proc_data.proc_id =
              method_node->id ? strdup(method_node->id) : NULL;
          expr->expr_data.addr_of_proc_data.source_unit_index =
              method_node->source_unit_index;
          expr->expr_data.addr_of_proc_data.receiver_expr = saved_receiver;
          if (method_node->type != NULL &&
              method_node->type->kind == TYPE_KIND_PROCEDURE) {
            kgpc_type_retain(method_node->type);
            expr->resolved_kgpc_type = create_pointer_type(method_node->type);
            kgpc_type_release(method_node->type);
          } else {
            KgpcType *generic_proc = create_procedure_type(NULL, NULL);
            expr->resolved_kgpc_type = create_pointer_type(generic_proc);
            if (generic_proc != NULL)
              destroy_kgpc_type(generic_proc);
          }
          converted_to_proc_addr = 1;
        }
      }
    } else if (resolved_proc_symbol != NULL &&
               (resolved_proc_symbol->hash_type == HASHTYPE_FUNCTION ||
                resolved_proc_symbol->hash_type == HASHTYPE_PROCEDURE)) {
      expr->expr_data.addr_data.expr = NULL;
      destroy_expr(inner);
      expr->type = EXPR_ADDR_OF_PROC;
      expr->expr_data.addr_of_proc_data.proc_mangled_id =
          semcheck_dup_proc_target_symbol(symtab, resolved_proc_symbol);
      expr->expr_data.addr_of_proc_data.proc_id =
          resolved_proc_symbol->id ? strdup(resolved_proc_symbol->id) : NULL;
      expr->expr_data.addr_of_proc_data.source_unit_index =
          resolved_proc_symbol->source_unit_index;
      if (resolved_proc_symbol->type != NULL &&
          resolved_proc_symbol->type->kind == TYPE_KIND_PROCEDURE) {
        kgpc_type_retain(resolved_proc_symbol->type);
        expr->resolved_kgpc_type =
            create_pointer_type(resolved_proc_symbol->type);
        kgpc_type_release(resolved_proc_symbol->type);
      } else {
        KgpcType *generic_proc = create_procedure_type(NULL, NULL);
        expr->resolved_kgpc_type = create_pointer_type(generic_proc);
        if (generic_proc != NULL)
          destroy_kgpc_type(generic_proc);
      }
      converted_to_proc_addr = 1;
    } else if (inner->type == EXPR_RECORD_ACCESS &&
               inner->expr_data.record_access_data.field_id != NULL) {
      const char *field_id = inner->expr_data.record_access_data.field_id;
      struct Expression *record_expr =
          inner->expr_data.record_access_data.record_expr;
      if (record_expr == NULL || record_expr->type != EXPR_VAR_ID ||
          record_expr->expr_data.id == NULL ||
          !inner->is_specialize_addr_target) {
        converted_to_proc_addr = 0;
      } else {
        size_t nlen = strlen(field_id) + 2;
        char *synth_name = (char *)malloc(nlen + 1);
        if (synth_name != NULL)
          snprintf(synth_name, nlen + 1, "__%s", field_id);

        if (synth_name != NULL) {
          HashNode_t *existing = NULL;
          if (FindSymbol(&existing, symtab, synth_name) == 0) {
            KgpcType *generic_proc = create_procedure_type(NULL, NULL);
            if (generic_proc != NULL) {
              (void)PushProcedureOntoScope_Typed(symtab, synth_name, synth_name,
                                                 generic_proc);
              destroy_kgpc_type(generic_proc);
            }
          }

          expr->expr_data.addr_data.expr = NULL;
          destroy_expr(inner);
          expr->type = EXPR_ADDR_OF_PROC;
          expr->expr_data.addr_of_proc_data.proc_id = strdup(synth_name);
          expr->expr_data.addr_of_proc_data.proc_mangled_id =
              strdup(synth_name);
          expr->expr_data.addr_of_proc_data.source_unit_index = 0;
          free(synth_name);
        }
        if (expr->resolved_kgpc_type != NULL) {
          destroy_kgpc_type(expr->resolved_kgpc_type);
          expr->resolved_kgpc_type = NULL;
        }
        {
          KgpcType *generic_proc = create_procedure_type(NULL, NULL);
          expr->resolved_kgpc_type = create_pointer_type(generic_proc);
          if (generic_proc != NULL)
            destroy_kgpc_type(generic_proc);
        }
        converted_to_proc_addr = 1;
      }
    }
  }
  /* For other types, we could add more conversions here */

  /* Create the pointer type */
  if (pointed_to_type != NULL && expr->type != EXPR_ADDR_OF_PROC) {
    if (expr->resolved_kgpc_type != NULL) {
      destroy_kgpc_type(expr->resolved_kgpc_type);
    }
    expr->resolved_kgpc_type = create_pointer_type(pointed_to_type);
    destroy_kgpc_type(pointed_to_type); /* create_pointer_type retained it */
  }

  /* If we successfully converted to a procedure address, don't count inner
   * expression errors. Those errors were from trying to call the function with
   * no arguments, which is not what we want. */
  if (converted_to_proc_addr && expr->type == EXPR_ADDR_OF_PROC) {
    return 0; /* Success - ignore inner errors */
  }

  return error_count;
}
