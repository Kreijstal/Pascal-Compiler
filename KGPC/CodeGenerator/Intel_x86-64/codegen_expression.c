/*
    Damon Gwinn
    Code generation for expressions
*/

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__GLIBC__) || (defined(__APPLE__) && defined(__MACH__)) ||         \
    defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define HAVE_EXECINFO 1
#include <execinfo.h>
#endif

/* Forward declarations for unresolved method stubs — implementation after
 * includes. */

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
#include "../../Parser/pascal_frontend.h"
#include "../../flags.h"
#include "../../format_arg.h"
#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "abi_constants.h"
#include "codegen.h"
#include "codegen_expr_arguments.h"
#include "codegen_expr_array.h"
#include "codegen_expr_relop.h"
#include "codegen_expr_sizeof.h"
#include "codegen_expression.h"
#include "codegen_expression_internal.h"
#include "expr_tree/expr_tree.h"
#include "register_types.h"
#include "stackmng/stackmng.h"

/* Cached getenv() — defined in SemCheck.c */
extern const char *kgpc_getenv(const char *name);
/* Defined in codegen_stmt_assignment.c, declared in codegen_stmt_internal.h
 * which we don't include here to avoid pulling the rest of the stmt module
 * surface. */
int codegen_expr_is_wide_string_value(const struct Expression *expr);
static int
codegen_array_access_targets_shortstring(const struct Expression *expr,
                                         CodeGenContext *ctx);
int codegen_get_char_array_length(const struct Expression *expr,
                                  CodeGenContext *ctx, long long *out_len);
#define CODEGEN_POINTER_SIZE_BYTES 8
#define CODEGEN_SIZEOF_RECURSION_LIMIT 32

/* Helper functions for transitioning from legacy type fields to KgpcType */

/* Helper function to check if a node is a record type */
static inline int codegen_node_is_record_type(HashNode_t *node) {
  return hashnode_is_record(node);
}

/* Helper function to get RecordType from HashNode */
static inline struct RecordType *
codegen_get_record_type_from_node(HashNode_t *node) {
  return hashnode_get_record_type(node);
}

const struct RecordType *codegen_record_class_var_owner_named(
    SymTab_t *symtab, struct RecordType *record, const char *field_id) {
  if (record == NULL || field_id == NULL)
    return NULL;

  struct RecordType *field_owner = NULL;
  struct RecordField *field = semcheck_find_class_field_including_hidden(
      symtab, record, field_id, &field_owner);
  if (field != NULL && field->is_class_var == 1)
    return field_owner != NULL ? field_owner : record;

  for (ListNode_t *node = record->fields; node != NULL; node = node->next) {
    if (node->type != LIST_RECORD_FIELD || node->cur == NULL)
      continue;
    field = (struct RecordField *)node->cur;
    if (field->is_class_var == 1 && field->name != NULL &&
        pascal_identifier_equals(field->name, field_id))
      return record;
  }

  return NULL;
}

ListNode_t *codegen_emit_classvar_base_address_named(
    ListNode_t *inst_list, const char *addr_reg64,
    const struct RecordType *record, long long field_offset) {
  if (addr_reg64 == NULL || record == NULL || record->type_id == NULL)
    return inst_list;

  char buffer[160];
  snprintf(buffer, sizeof(buffer), "\tleaq\t%s_CLASSVAR(%%rip), %s\n",
           record->type_id, addr_reg64);
  inst_list = add_inst(inst_list, buffer);

  if (field_offset != 0) {
    snprintf(buffer, sizeof(buffer), "\taddq\t$%lld, %s\n", field_offset,
             addr_reg64);
    inst_list = add_inst(inst_list, buffer);
  }

  return inst_list;
}

int codegen_record_matches_owner_class(CodeGenContext *ctx,
                                       const struct RecordType *record) {
  if (ctx == NULL || record == NULL || record->type_id == NULL ||
      ctx->current_subprogram_owner_class == NULL)
    return 0;

  if (pascal_identifier_equals(record->type_id,
                               ctx->current_subprogram_owner_class))
    return 1;

  if (ctx->current_subprogram_owner_class_full != NULL &&
      pascal_identifier_equals(record->type_id,
                               ctx->current_subprogram_owner_class_full))
    return 1;

  return 0;
}

int codegen_nonstatic_class_method_owner_field_uses_classvar(
    CodeGenContext *ctx, const struct RecordType *record,
    const struct Expression *record_expr) {
  int is_nonstatic_class_method = 0;
  if (ctx == NULL || record == NULL || record_expr == NULL ||
      !codegen_record_matches_owner_class(ctx, record))
    return 0;

  is_nonstatic_class_method = ctx->current_subprogram_is_nonstatic_class_method;
  if (!is_nonstatic_class_method &&
      ctx->current_subprogram_owner_class != NULL &&
      ctx->current_subprogram_method_name != NULL) {
    is_nonstatic_class_method = from_cparser_is_method_nonstatic_class_method(
        (char *)ctx->current_subprogram_owner_class,
        (char *)ctx->current_subprogram_method_name);
  }
  if (!is_nonstatic_class_method)
    return 0;

  if (record_expr->type == EXPR_VAR_ID && record_expr->expr_data.id != NULL &&
      pascal_identifier_equals(record_expr->expr_data.id, "Self"))
    return 1;

  return 0;
}

HashNode_t *codegen_find_owner_unit_symbol(CodeGenContext *ctx,
                                           const char *id) {
  if (ctx == NULL || ctx->symtab == NULL || id == NULL)
    return NULL;

  int unit_index = ctx->symtab->current_unit_index;
  if (unit_index <= 0 || unit_index >= SYMTAB_MAX_UNITS)
    return NULL;

  ScopeNode *unit_scope = ctx->symtab->unit_scopes[unit_index];
  if (unit_scope == NULL || unit_scope->table == NULL)
    return NULL;

  HashNode_t *node =
      FindIdentInTableForUnit(unit_scope->table, (char *)id, unit_index);
  if (node != NULL)
    return node;

  return FindIdentInTable(unit_scope->table, (char *)id);
}

HashNode_t *codegen_prefer_visible_var_over_const(CodeGenContext *ctx,
                                                  const char *id,
                                                  HashNode_t *node) {
  if (ctx == NULL || ctx->symtab == NULL || id == NULL || node == NULL ||
      !(node->hash_type == HASHTYPE_CONST || node->is_constant))
    return node;

  int caller_unit = ctx->symtab->current_unit_index;
  if (caller_unit <= 0)
    return node;

  if (!(node->defined_in_unit && !node->unit_is_public &&
        node->source_unit_index != caller_unit))
    return node;

  ListNode_t *matches = FindAllIdents(ctx->symtab, id);
  HashNode_t *best = node;
  int best_priority = 0;
  for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
    HashNode_t *cand = (HashNode_t *)cur->cur;
    if (cand == NULL ||
        !(cand->hash_type == HASHTYPE_VAR || cand->hash_type == HASHTYPE_ARRAY))
      continue;
    if (cand->defined_in_unit && !cand->unit_is_public &&
        cand->source_unit_index != caller_unit)
      continue;

    int priority = 0;
    if (cand->source_unit_index == caller_unit)
      priority = 4;
    else if (cand->source_unit_index > 0 &&
             unit_registry_is_dep(caller_unit, cand->source_unit_index))
      priority = 3;
    else if (cand->source_unit_index == 0)
      priority = 2;
    else if (cand->source_unit_index > 0 &&
             cand->source_unit_index != caller_unit)
      priority = 1;

    if (priority > best_priority) {
      best = cand;
      best_priority = priority;
    }
  }
  if (matches != NULL)
    DestroyList(matches);

  return best;
}

static KgpcType *
codegen_function_call_return_type_from_expr(const struct Expression *expr) {
  KgpcType *call_type = NULL;
  KgpcType *ret_type = NULL;
  const char *ret_id = NULL;
  static KgpcType *cached_shortstring = NULL;
  static KgpcType *cached_ansistring = NULL;

  if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return NULL;

  if (expr->expr_data.function_call_data.mangled_id != NULL &&
      strncmp(expr->expr_data.function_call_data.mangled_id, "kgpc_", 5) == 0 &&
      expr->resolved_kgpc_type != NULL) {
    return expr->resolved_kgpc_type;
  }

  call_type = expr->expr_data.function_call_data.call_kgpc_type;
  if (call_type == NULL &&
      expr->expr_data.function_call_data.resolved_func != NULL) {
    call_type = expr->expr_data.function_call_data.resolved_func->type;
  }

  /* Builtin lowering sometimes rewrites a call directly to a runtime helper
   * and clears the semantic call cache. In that case, prefer the semchecked
   * expression result type over falling back to unrelated source-level
   * declarations that happen to share the original identifier. */
  if (call_type == NULL && expr->resolved_kgpc_type != NULL)
    return expr->resolved_kgpc_type;

  if (call_type == NULL || call_type->kind != TYPE_KIND_PROCEDURE)
    return NULL;

  /* In {$H-} / compiler bootstrap code, the callee declaration may still
   * carry return_type_id = "String", while the semchecked call expression has
   * already resolved that to fixed ShortString storage.  Prefer the resolved
   * expression type so callers emit the hidden result buffer before Self. */
  if (expr->resolved_kgpc_type != NULL &&
      (kgpc_type_is_shortstring(expr->resolved_kgpc_type) ||
       (expr->resolved_kgpc_type->type_alias != NULL &&
        expr->resolved_kgpc_type->type_alias->is_shortstring))) {
    return expr->resolved_kgpc_type;
  }

  /* If the callee's Tree_t has return_type == SHORTSTRING_TYPE (set during AST
   * conversion under {$H-}), honour that even though the KgpcType from
   * semantic analysis may say STRING_TYPE.  Must check before ret_type
   * early-return since semcheck creates STRING_TYPE KgpcType when the
   * global flag is reset. */
  if (call_type->info.proc_info.definition != NULL &&
      call_type->info.proc_info.definition->tree_data.subprogram_data
              .return_type == SHORTSTRING_TYPE) {
    if (cached_shortstring == NULL)
      cached_shortstring = create_primitive_type(SHORTSTRING_TYPE);
    return cached_shortstring;
  }

  ret_type = kgpc_type_get_return_type(call_type);
  if (ret_type != NULL)
    return ret_type;

  ret_id = call_type->info.proc_info.return_type_id;
  if (ret_id == NULL && call_type->info.proc_info.definition != NULL)
    ret_id = call_type->info.proc_info.definition->tree_data.subprogram_data
                 .return_type_id;
  if (ret_id == NULL)
    return NULL;

  if (pascal_identifier_equals(ret_id, "ShortString")) {
    if (cached_shortstring == NULL)
      cached_shortstring = create_primitive_type(SHORTSTRING_TYPE);
    return cached_shortstring;
  }
  if (pascal_identifier_equals(ret_id, "String") &&
      pascal_frontend_default_shortstring()) {
    if (cached_shortstring == NULL)
      cached_shortstring = create_primitive_type(SHORTSTRING_TYPE);
    return cached_shortstring;
  }
  if (pascal_identifier_equals(ret_id, "AnsiString") ||
      pascal_identifier_equals(ret_id, "String")) {
    if (cached_ansistring == NULL)
      cached_ansistring = create_primitive_type(STRING_TYPE);
    return cached_ansistring;
  }
  return NULL;
}

static const char *
codegen_function_call_return_type_id_from_expr(const struct Expression *expr) {
  KgpcType *call_type = NULL;

  if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return NULL;

  call_type = expr->expr_data.function_call_data.call_kgpc_type;
  if (call_type == NULL &&
      expr->expr_data.function_call_data.resolved_func != NULL) {
    call_type = expr->expr_data.function_call_data.resolved_func->type;
  }
  if (call_type == NULL || call_type->kind != TYPE_KIND_PROCEDURE)
    return NULL;

  if (call_type->info.proc_info.return_type_id != NULL)
    return call_type->info.proc_info.return_type_id;
  if (call_type->info.proc_info.definition != NULL)
    return call_type->info.proc_info.definition->tree_data.subprogram_data
        .return_type_id;
  return NULL;
}

static int codegen_return_type_is_shortstring_value(KgpcType *ret_type,
                                                    const char *ret_id) {
  if (ret_type != NULL) {
    struct TypeAlias *alias = kgpc_type_get_type_alias(ret_type);
    if (kgpc_type_is_shortstring(ret_type) ||
        (alias != NULL && alias->is_shortstring))
      return 1;
    if (kgpc_type_equals_tag(ret_type, STRING_TYPE) &&
        !kgpc_type_is_shortstring(ret_type)) {
      if (alias != NULL)
        return alias->is_shortstring;
      if (ret_id != NULL &&
          (pascal_identifier_equals(ret_id, "AnsiString") ||
           pascal_identifier_equals(ret_id, "UnicodeString") ||
           pascal_identifier_equals(ret_id, "WideString")))
        return 0;
      if (ret_id != NULL && !pascal_identifier_equals(ret_id, "String"))
        return 0;
      if (pascal_frontend_default_shortstring())
        return 1;
    }
  }
  if (ret_id != NULL && pascal_identifier_equals(ret_id, "String") &&
      pascal_frontend_default_shortstring())
    return 1;
  if (ret_id != NULL && pascal_identifier_equals(ret_id, "ShortString"))
    return 1;
  return 0;
}

static int
codegen_method_template_returns_shortstring(CodeGenContext *ctx,
                                            const struct MethodTemplate *tmpl) {
  if (tmpl == NULL || tmpl->kind != METHOD_TEMPLATE_FUNCTION)
    return 0;
  if (tmpl->method_tree != NULL &&
      tmpl->method_tree->tree_data.subprogram_data.return_type ==
          SHORTSTRING_TYPE)
    return 1;
  if (ctx != NULL && tmpl->return_type_ast != NULL) {
    KgpcType *ret_type =
        convert_type_spec_to_kgpctype(tmpl->return_type_ast, ctx->symtab);
    if (codegen_return_type_is_shortstring_value(ret_type, NULL))
      return 1;
  }
  return 0;
}

static int
codegen_virtual_call_returns_shortstring(CodeGenContext *ctx,
                                         const struct Expression *expr) {
  if (ctx == NULL || expr == NULL || expr->type != EXPR_FUNCTION_CALL ||
      !expr->expr_data.function_call_data.is_virtual_call)
    return 0;

  const char *owner_name = expr->expr_data.function_call_data.self_class_name;
  if (owner_name == NULL)
    owner_name = expr->expr_data.function_call_data.cached_owner_class;
  const char *method_name =
      expr->expr_data.function_call_data.cached_method_name;
  if (method_name == NULL)
    method_name = expr->expr_data.function_call_data.id;
  if (owner_name == NULL || method_name == NULL)
    return 0;

  struct RecordType *record =
      semcheck_lookup_record_type(ctx->symtab, owner_name);
  if (record == NULL)
    return 0;

  for (ListNode_t *cur = record->method_templates; cur != NULL;
       cur = cur->next) {
    struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
    if (tmpl != NULL && tmpl->name != NULL &&
        pascal_identifier_equals(tmpl->name, method_name) &&
        codegen_method_template_returns_shortstring(ctx, tmpl))
      return 1;
  }
  return 0;
}

static int codegen_bare_method_returns_shortstring(CodeGenContext *ctx,
                                                   const char *method_name) {
  if (ctx == NULL || method_name == NULL)
    return 0;
  const char *owner_name = ctx->current_subprogram_owner_class;
  if (owner_name == NULL)
    owner_name = ctx->current_subprogram_owner_class_full;
  if (owner_name == NULL)
    return 0;
  struct RecordType *record =
      semcheck_lookup_record_type(ctx->symtab, owner_name);
  if (record == NULL)
    return 0;
  for (ListNode_t *cur = record->method_templates; cur != NULL;
       cur = cur->next) {
    struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
    if (tmpl != NULL && tmpl->name != NULL &&
        pascal_identifier_equals(tmpl->name, method_name) &&
        codegen_method_template_returns_shortstring(ctx, tmpl))
      return 1;
  }
  return 0;
}

static struct RecordField *
codegen_find_unique_record_field(SymTab_t *symtab, const char *field_id,
                                 struct RecordType **out_record);
struct RecordField *codegen_lookup_with_field(CodeGenContext *ctx,
                                              const char *field_id,
                                              struct RecordType **out_record);
long long codegen_array_elem_size_from_field(struct RecordField *field,
                                             CodeGenContext *ctx);
struct RecordType *codegen_expr_record_type(const struct Expression *expr,
                                            SymTab_t *symtab) {
  if (expr == NULL)
    return NULL;
  if (expr->record_type != NULL)
    return expr->record_type;
  if (expr->type == EXPR_VAR_ID && symtab != NULL &&
      expr->expr_data.id != NULL) {
    HashNode_t *var_node = NULL;
    if (FindSymbol(&var_node, symtab, expr->expr_data.id) != 0 &&
        var_node != NULL) {
      struct RecordType *rec = codegen_get_record_type_from_node(var_node);
      if (rec != NULL)
        return rec;
      if (var_node->type != NULL) {
        if (kgpc_type_is_record(var_node->type))
          return kgpc_type_get_record(var_node->type);
        if (kgpc_type_is_pointer(var_node->type) &&
            var_node->type->info.points_to != NULL &&
            kgpc_type_is_record(var_node->type->info.points_to))
          return kgpc_type_get_record(var_node->type->info.points_to);
      }
    }
  }
  if (expr->type == EXPR_TYPECAST && symtab != NULL) {
    const char *target_id = expr->expr_data.typecast_data.target_type_id;
    if (target_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, symtab, target_id) != 0 && type_node != NULL) {
        struct RecordType *rec = codegen_get_record_type_from_node(type_node);
        if (rec != NULL)
          return rec;
        if (type_node->type != NULL) {
          if (kgpc_type_is_record(type_node->type))
            return kgpc_type_get_record(type_node->type);
          if (kgpc_type_is_pointer(type_node->type) &&
              type_node->type->info.points_to != NULL &&
              kgpc_type_is_record(type_node->type->info.points_to))
            return kgpc_type_get_record(type_node->type->info.points_to);
        }
      }
    }
  }
  if (expr->type == EXPR_FUNCTION_CALL && symtab != NULL) {
    const char *call_id = expr->expr_data.function_call_data.id;
    if (call_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, symtab, call_id) != 0 && type_node != NULL) {
        struct RecordType *rec = codegen_get_record_type_from_node(type_node);
        if (rec != NULL)
          return rec;
        if (type_node->type != NULL) {
          if (kgpc_type_is_record(type_node->type))
            return kgpc_type_get_record(type_node->type);
          if (kgpc_type_is_pointer(type_node->type) &&
              type_node->type->info.points_to != NULL &&
              kgpc_type_is_record(type_node->type->info.points_to))
            return kgpc_type_get_record(type_node->type->info.points_to);
        }
      }
    }
  }
  if (expr->type == EXPR_RECORD_ACCESS && symtab != NULL) {
    if (expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_record(expr->resolved_kgpc_type))
      return kgpc_type_get_record(expr->resolved_kgpc_type);

    struct Expression *base_expr =
        expr->expr_data.record_access_data.record_expr;
    const char *field_id = expr->expr_data.record_access_data.field_id;
    if (base_expr != NULL && field_id != NULL) {
      struct RecordType *base_record =
          codegen_expr_record_type(base_expr, symtab);
      if (base_record != NULL) {
        struct RecordField *field = semcheck_find_class_field_including_hidden(
            symtab, base_record, field_id, NULL);
        if (field != NULL) {
          if (field->nested_record != NULL)
            return field->nested_record;
          if (field->type_id != NULL) {
            HashNode_t *type_node = NULL;
            if (FindSymbol(&type_node, symtab, field->type_id) != 0 &&
                type_node != NULL) {
              struct RecordType *rec =
                  codegen_get_record_type_from_node(type_node);
              if (rec != NULL)
                return rec;
              if (type_node->type != NULL &&
                  kgpc_type_is_record(type_node->type))
                return kgpc_type_get_record(type_node->type);
            }
          }
        }
      }
    }
  }

  KgpcType *expr_type = expr_get_kgpc_type((struct Expression *)expr);
  if (expr_type != NULL) {
    if (kgpc_type_is_record(expr_type))
      return kgpc_type_get_record(expr_type);
    if (kgpc_type_is_pointer(expr_type) && expr_type->info.points_to != NULL &&
        kgpc_type_is_record(expr_type->info.points_to))
      return kgpc_type_get_record(expr_type->info.points_to);
  }

  if (expr->pointer_subtype_id != NULL && symtab != NULL) {
    HashNode_t *target_node = NULL;
    if (FindSymbol(&target_node, symtab, expr->pointer_subtype_id) != 0 &&
        target_node != NULL)
      return codegen_get_record_type_from_node(target_node);
  }

  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL &&
      symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, expr->expr_data.id) != 0 && node != NULL) {
      struct RecordType *record = codegen_get_record_type_from_node(node);
      if (record != NULL)
        return record;
      if (node->type != NULL && kgpc_type_is_pointer(node->type) &&
          node->type->info.points_to != NULL &&
          kgpc_type_is_record(node->type->info.points_to)) {
        return kgpc_type_get_record(node->type->info.points_to);
      }
    }
  }

  return NULL;
}

static int codegen_expr_is_type_identifier(const struct Expression *expr,
                                           CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL ||
      expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
    return 0;

  HashNode_t *node = NULL;
  if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) == 0 || node == NULL)
    return 0;
  return node->hash_type == HASHTYPE_TYPE;
}

static int codegen_expr_is_class_reference_value(const struct Expression *expr,
                                                 CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  if (expr->resolved_kgpc_type != NULL) {
    struct TypeAlias *alias =
        kgpc_type_get_type_alias(expr->resolved_kgpc_type);
    if (alias != NULL && alias->is_class_reference)
      return 1;
  }

  if (ctx != NULL && ctx->symtab != NULL && expr->type == EXPR_VAR_ID &&
      expr->expr_data.id != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL) {
      struct TypeAlias *alias = hashnode_get_type_alias(node);
      if (alias != NULL && alias->is_class_reference)
        return 1;
      if (node->type != NULL) {
        alias = kgpc_type_get_type_alias(node->type);
        if (alias != NULL && alias->is_class_reference)
          return 1;
      }
    }
  }

  if (ctx != NULL && ctx->symtab != NULL && expr->type == EXPR_TYPECAST &&
      expr->expr_data.typecast_data.target_type_id != NULL) {
    HashNode_t *type_node = NULL;
    if (FindSymbol(&type_node, ctx->symtab,
                   expr->expr_data.typecast_data.target_type_id) != 0 &&
        type_node != NULL) {
      struct TypeAlias *alias = hashnode_get_type_alias(type_node);
      if (alias != NULL && alias->is_class_reference)
        return 1;
      if (type_node->type != NULL) {
        alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_class_reference)
          return 1;
      }
    }
  }

  return 0;
}

static int codegen_type_is_class_vmt_value(KgpcType *type) {
  if (type == NULL)
    return 0;

  if (type->type_alias != NULL && type->type_alias->is_class_reference)
    return 1;

  if (type->kind == TYPE_KIND_POINTER && type->info.points_to != NULL) {
    if (type->info.points_to->kind == TYPE_KIND_POINTER &&
        type->info.points_to->type_alias != NULL &&
        type->info.points_to->type_alias->is_class_reference) {
      return 1;
    }
  }

  return 0;
}

int codegen_expr_is_class_vmt_value(const struct Expression *expr,
                                    CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL)
    return 0;

  if (codegen_expr_is_type_identifier(expr, ctx))
    return 1;

  if (codegen_type_is_class_vmt_value(expr->resolved_kgpc_type))
    return 1;

  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL) {
    if (ctx->current_subprogram_is_nonstatic_class_method &&
        pascal_identifier_equals(expr->expr_data.id, "Self")) {
      return 1;
    }

    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL) {
      if (node->hash_type == HASHTYPE_TYPE &&
          codegen_type_is_class_vmt_value(node->type)) {
        return 1;
      }

      if (codegen_type_is_class_vmt_value(node->type))
        return 1;
    }
  }

  return 0;
}

int codegen_expr_needs_class_method_vmt_self(const struct Expression *expr,
                                             CodeGenContext *ctx) {
  struct RecordType *record = NULL;

  if (expr == NULL || ctx == NULL)
    return 0;
  if (codegen_expr_is_type_identifier(expr, ctx))
    return 0;
  if (codegen_expr_is_class_reference_value(expr, ctx))
    return 0;

  record = codegen_expr_record_type(expr, ctx->symtab);
  return (record != NULL && record_type_is_class(record));
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

static int codegen_expr_is_char_array_like(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE)
    return 1;
  KgpcType *expr_type = expr_get_kgpc_type(expr);
  if (kgpc_type_string_storage_kind(expr_type) ==
      KGPC_STRING_STORAGE_CHAR_ARRAY)
    return 1;
  return 0;
}

int codegen_expr_is_shortstring_value_ctx(const struct Expression *expr,
                                          CodeGenContext *ctx) {
  int expr_is_current_result = 0;

  if (expr != NULL && expr->type == EXPR_FUNCTION_CALL && ctx != NULL) {
    KgpcType *ret_type = NULL;
    const char *ret_id = NULL;

    ret_id = codegen_function_call_return_type_id_from_expr(expr);
    ret_type = codegen_function_call_return_type_from_expr(expr);
    if (codegen_return_type_is_shortstring_value(ret_type, ret_id))
      return 1;
    if (codegen_virtual_call_returns_shortstring(ctx, expr))
      return 1;

    if (ret_type == NULL) {
      HashNode_t *call_node = NULL;
      KgpcType *call_type =
          codegen_resolve_function_call_type(ctx, expr, &call_node);
      if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE) {
        ret_type = kgpc_type_get_return_type(call_type);
        ret_id = call_type->info.proc_info.return_type_id;
        if (ret_id == NULL && call_type->info.proc_info.definition != NULL)
          ret_id = call_type->info.proc_info.definition->tree_data
                       .subprogram_data.return_type_id;
        if (codegen_return_type_is_shortstring_value(ret_type, ret_id))
          return 1;
      }
      if (call_node != NULL && call_node->type != NULL &&
          call_node->type->kind == TYPE_KIND_PROCEDURE) {
        ret_type = kgpc_type_get_return_type(call_node->type);
        ret_id = call_node->type->info.proc_info.return_type_id;
        if (ret_id == NULL &&
            call_node->type->info.proc_info.definition != NULL)
          ret_id = call_node->type->info.proc_info.definition->tree_data
                       .subprogram_data.return_type_id;
        if (codegen_return_type_is_shortstring_value(ret_type, ret_id))
          return 1;
      }
    }
  }

  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL) {
    const char *expr_id = expr->expr_data.id;
    const char *current_id = ctx->current_subprogram_id;
    if (expr_id != NULL) {
      if (pascal_identifier_equals(expr_id, "Result")) {
        HashNode_t *shadow_node = NULL;
        if (!(ctx->symtab != NULL &&
              FindSymbol(&shadow_node, ctx->symtab, expr_id) != 0 &&
              shadow_node != NULL))
          expr_is_current_result = 1;
      } else if (current_id != NULL &&
                 pascal_identifier_equals(expr_id, current_id))
        expr_is_current_result = 1;
      else if (ctx->current_subprogram_method_name != NULL &&
               pascal_identifier_equals(expr_id,
                                        ctx->current_subprogram_method_name))
        expr_is_current_result = 1;
      else if (ctx->current_subprogram_result_name != NULL &&
               pascal_identifier_equals(expr_id,
                                        ctx->current_subprogram_result_name))
        expr_is_current_result = 1;
    }
  }

  if (codegen_expr_is_shortstring_value(expr))
    return 1;
  if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS) {
    long long char_len = 0;
    if (codegen_get_char_array_length(expr, ctx, &char_len) && char_len > 1 &&
        char_len <= 256)
      return 1;
  }
  if (expr != NULL && expr->type == EXPR_ARRAY_ACCESS &&
      codegen_array_access_targets_shortstring(expr, ctx))
    return 1;
  if (expr_is_current_result && ctx != NULL) {
    KgpcType *ret_type = ctx->current_return_type;
    if (ret_type != NULL && (kgpc_type_is_shortstring(ret_type) ||
                             (ret_type->type_alias != NULL &&
                              ret_type->type_alias->is_shortstring)))
      return 1;
  }
  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
      ctx->symtab != NULL) {
    if (codegen_bare_method_returns_shortstring(ctx, expr->expr_data.id))
      return 1;
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL && node->type != NULL) {
      if (node->type->kind == TYPE_KIND_PROCEDURE) {
        KgpcType *ret_type = kgpc_type_get_return_type(node->type);
        const char *ret_id = node->type->info.proc_info.return_type_id;
        if (ret_id == NULL && node->type->info.proc_info.definition != NULL)
          ret_id = node->type->info.proc_info.definition->tree_data
                       .subprogram_data.return_type_id;
        if (codegen_return_type_is_shortstring_value(ret_type, ret_id))
          return 1;
      }
      if (kgpc_type_equals_tag(node->type, STRING_TYPE) &&
          kgpc_type_string_storage_kind(node->type) !=
              KGPC_STRING_STORAGE_SHORTSTRING) {
        struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
        if (alias != NULL && !alias->is_shortstring)
          return 0;
        if (alias == NULL && !pascal_frontend_default_shortstring())
          return 0;
        if (alias == NULL)
          return 1;
      }
      if (kgpc_type_string_storage_kind(node->type) ==
          KGPC_STRING_STORAGE_SHORTSTRING)
        return 1;
    }
  }

  return 0;
}

int codegen_expr_function_call_returns_ansistring(
    CodeGenContext *ctx, const struct Expression *expr) {
  if (ctx == NULL || expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return 0;

  KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
  if (call_type == NULL)
    call_type = codegen_resolve_function_call_type(
        ctx, (struct Expression *)expr, NULL);
  if (call_type == NULL || call_type->kind != TYPE_KIND_PROCEDURE)
    return 0;

  const char *ret_id = call_type->info.proc_info.return_type_id;
  if (ret_id != NULL && pascal_identifier_equals(ret_id, "AnsiString"))
    return 1;

  KgpcType *ret_type = kgpc_type_get_return_type(call_type);
  if (ret_type != NULL && ret_type->kind == TYPE_KIND_PRIMITIVE &&
      kgpc_type_get_primitive_tag(ret_type) == STRING_TYPE &&
      !kgpc_type_is_shortstring(ret_type)) {
    return 1;
  }

  return 0;
}

int codegen_expr_is_char_array_like_ctx(const struct Expression *expr,
                                        CodeGenContext *ctx) {
  if (codegen_expr_is_char_array_like(expr))
    return 1;
  if (expr != NULL && expr->type == EXPR_VAR_ID && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
        node != NULL && node->type != NULL) {
      if (kgpc_type_is_array(node->type)) {
        KgpcType *elem = kgpc_type_get_array_element_type(node->type);
        if (elem != NULL && elem->kind == TYPE_KIND_PRIMITIVE &&
            elem->info.primitive_type_tag == CHAR_TYPE)
          return 1;
      }
    }
  }
  return 0;
}

ListNode_t *codegen_promote_shortstring_reg(ListNode_t *inst_list,
                                            CodeGenContext *ctx,
                                            Register_t *value_reg) {
  if (inst_list == NULL || ctx == NULL || value_reg == NULL)
    return inst_list;
  const char *arg_reg64 = current_arg_reg64(0);
  if (arg_reg64 == NULL)
    return inst_list;

  {
    /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_PHYS, BE_W64, {.phys = arg_reg64}};
    BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
    kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list =
      codegen_call_with_shadow_space(inst_list, "kgpc_shortstring_to_string");
  {
    /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = value_reg}};
    BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
    kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  free_arg_regs();
  return inst_list;
}

ListNode_t *codegen_spill_reg64_temp(ListNode_t *inst_list, CodeGenContext *ctx,
                                     const Register_t *reg,
                                     const char *temp_name,
                                     StackNode_t **spill_slot) {
  if (spill_slot != NULL)
    *spill_slot = NULL;
  if (reg == NULL || spill_slot == NULL || temp_name == NULL)
    return inst_list;

  StackNode_t *slot = add_l_t((char *)temp_name);
  if (slot == NULL)
    return inst_list;

  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(slot->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = (Register_t *)reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }
  *spill_slot = slot;
  return inst_list;
}

ListNode_t *codegen_restore_spilled_reg64(ListNode_t *inst_list,
                                          CodeGenContext *ctx,
                                          const Register_t *reg,
                                          StackNode_t *spill_slot) {
  if (reg == NULL || spill_slot == NULL)
    return inst_list;

  {
    /* Integrated: load from the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = (Register_t *)reg}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(spill_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
    return em.list;
  }
}

int codegen_get_char_array_length(const struct Expression *expr,
                                  CodeGenContext *ctx, long long *out_len) {
  if (out_len != NULL)
    *out_len = 0;
  if (expr == NULL)
    return 0;

  long long lower = 0;
  long long upper = -1;
  int found = 0;

  if (expr->is_array_expr && expr->array_element_type == CHAR_TYPE) {
    lower = expr_get_array_lower_bound(expr);
    upper = expr_get_array_upper_bound(expr);
    found = 1;
  } else {
    KgpcType *kgpc = expr_get_kgpc_type(expr);
    if (kgpc != NULL && kgpc_type_is_array(kgpc) &&
        kgpc->info.array_info.element_type != NULL &&
        kgpc->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
        kgpc->info.array_info.element_type->info.primitive_type_tag ==
            CHAR_TYPE) {
      lower = kgpc->info.array_info.start_index;
      upper = kgpc->info.array_info.end_index;
      found = 1;
    } else if (expr->type == EXPR_VAR_ID && ctx != NULL &&
               ctx->symtab != NULL) {
      HashNode_t *node = NULL;
      if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) != 0 &&
          node != NULL && node->type != NULL &&
          kgpc_type_is_array(node->type) &&
          node->type->info.array_info.element_type != NULL &&
          node->type->info.array_info.element_type->kind ==
              TYPE_KIND_PRIMITIVE &&
          node->type->info.array_info.element_type->info.primitive_type_tag ==
              CHAR_TYPE) {
        lower = node->type->info.array_info.start_index;
        upper = node->type->info.array_info.end_index;
        found = 1;
      }
    }
  }

  if (!found)
    return 0;
  if (upper < lower)
    return 0;
  if (out_len != NULL)
    *out_len = (upper - lower + 1);
  return 1;
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
  if (codegen_expr_is_shortstring_value(expr))
    return 1;
  if (codegen_expr_is_char_array_like(expr)) {
    long long size = expr_effective_size_bytes(expr);
    if (size > 1 && size <= 256)
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
    if (kgpc_type_string_storage_kind(elem_type) ==
        KGPC_STRING_STORAGE_SHORTSTRING)
      return 1;
  }

  return 0;
}

StackNode_t *codegen_alloc_temp_bytes(const char *prefix, int size);
static const char *codegen_register_name8(const Register_t *reg);
const char *codegen_register_name16(const Register_t *reg);
static ListNode_t *codegen_store_value_to_stack(ListNode_t *inst_list,
                                                CodeGenContext *ctx,
                                                Register_t *value_reg,
                                                int offset, int element_size);
ListNode_t *codegen_expr_maybe_convert_int_like_to_real(
    int target_type, struct Expression *arg_expr, Register_t *top_reg,
    ListNode_t *inst_list, CodeGenContext *ctx);
ListNode_t *codegen_expr_with_result(struct Expression *expr,
                                     ListNode_t *inst_list, CodeGenContext *ctx,
                                     Register_t **out_reg);
ListNode_t *codegen_materialize_array_literal(struct Expression *expr,
                                              ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              Register_t **out_reg);
static ListNode_t *codegen_materialize_array_of_const(struct Expression *expr,
                                                      ListNode_t *inst_list,
                                                      CodeGenContext *ctx,
                                                      Register_t **out_reg);

static const char *codegen_class_typeinfo_label(struct RecordType *record,
                                                const char *fallback_id) {
  if (record != NULL && record->type_id != NULL)
    return record->type_id;
  return fallback_id;
}

static ListNode_t *codegen_load_typeinfo_from_instance_ptr(
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *instance_ptr_reg,
    Register_t **out_reg) {
  if (out_reg != NULL)
    *out_reg = NULL;

  if (ctx == NULL || instance_ptr_reg == NULL)
    return inst_list;

  Register_t *typeinfo_reg = codegen_try_get_reg(&inst_list, ctx, "class RTTI");
  if (typeinfo_reg == NULL)
    return inst_list;

  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n",
           instance_ptr_reg->bit_64, typeinfo_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  /* Load vTypeInfo from VMT (VMT_VTYPEINFO_OFFSET = slot 7 = byte 56) */
  snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%s), %s\n", VMT_VTYPEINFO_OFFSET,
           typeinfo_reg->bit_64, typeinfo_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);

  if (out_reg != NULL)
    *out_reg = typeinfo_reg;
  else
    free_reg(get_reg_stack(), typeinfo_reg);

  return inst_list;
}

static ListNode_t *codegen_load_typeinfo_from_class_vmt_ptr(
    ListNode_t *inst_list, CodeGenContext *ctx, Register_t *class_vmt_reg,
    Register_t **out_reg) {
  if (out_reg != NULL)
    *out_reg = NULL;

  if (ctx == NULL || class_vmt_reg == NULL)
    return inst_list;

  Register_t *typeinfo_reg = codegen_try_get_reg(&inst_list, ctx, "class RTTI");
  if (typeinfo_reg == NULL)
    return inst_list;

  char buffer[128];
  /* Load vTypeInfo from VMT (VMT_VTYPEINFO_OFFSET = slot 7 = byte 56) */
  snprintf(buffer, sizeof(buffer), "\tmovq\t%d(%s), %s\n", VMT_VTYPEINFO_OFFSET,
           class_vmt_reg->bit_64, typeinfo_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);

  if (out_reg != NULL)
    *out_reg = typeinfo_reg;
  else
    free_reg(get_reg_stack(), typeinfo_reg);

  return inst_list;
}

static ListNode_t *codegen_load_class_typeinfo(struct Expression *expr,
                                               ListNode_t *inst_list,
                                               CodeGenContext *ctx,
                                               Register_t **out_reg) {
  if (out_reg != NULL)
    *out_reg = NULL;

  if (expr == NULL || ctx == NULL)
    return inst_list;

  if (!codegen_expr_is_addressable(expr)) {
    if (codegen_expr_is_class_vmt_value(expr, ctx)) {
      Register_t *class_vmt_reg = NULL;
      inst_list =
          codegen_expr_with_result(expr, inst_list, ctx, &class_vmt_reg);
      if (codegen_had_error(ctx) || class_vmt_reg == NULL)
        return inst_list;
      inst_list = codegen_load_typeinfo_from_class_vmt_ptr(
          inst_list, ctx, class_vmt_reg, out_reg);
      free_reg(get_reg_stack(), class_vmt_reg);
      return inst_list;
    }
    if (codegen_expr_needs_class_method_vmt_self(expr, ctx)) {
      Register_t *instance_ptr_reg = NULL;
      inst_list =
          codegen_expr_with_result(expr, inst_list, ctx, &instance_ptr_reg);
      if (codegen_had_error(ctx) || instance_ptr_reg == NULL)
        return inst_list;
      inst_list = codegen_load_typeinfo_from_instance_ptr(
          inst_list, ctx, instance_ptr_reg, out_reg);
      free_reg(get_reg_stack(), instance_ptr_reg);
      return inst_list;
    }
    codegen_report_error(ctx, "ERROR: RTTI operations currently require "
                              "addressable class expressions.");
    return inst_list;
  }

  /* For class variables (which are pointers), we need to:
   * 1. Load the pointer value from the variable
   * 2. Dereference the pointer to get the typeinfo (first field of the
   * instance)
   *
   * For non-class types, we only need:
   * 1. Get address and dereference to get typeinfo
   */
  struct RecordType *expr_record =
      codegen_expr_record_type(expr, ctx != NULL ? ctx->symtab : NULL);
  int is_class_var = (expr_record != NULL && record_type_is_class(expr_record));

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
  if (codegen_had_error(ctx) || addr_reg == NULL)
    return inst_list;

  Register_t *typeinfo_reg = NULL;
  char buffer[128];
  if (is_class_var) {
    Register_t *instance_ptr_reg =
        codegen_try_get_reg(&inst_list, ctx, "class instance");
    if (instance_ptr_reg == NULL) {
      free_reg(get_reg_stack(), addr_reg);
      return inst_list;
    }
    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64,
             instance_ptr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
    inst_list = codegen_load_typeinfo_from_instance_ptr(
        inst_list, ctx, instance_ptr_reg, &typeinfo_reg);
    free_reg(get_reg_stack(), instance_ptr_reg);
  } else {
    inst_list = codegen_load_typeinfo_from_instance_ptr(
        inst_list, ctx, addr_reg, &typeinfo_reg);
  }
  free_reg(get_reg_stack(), addr_reg);

  if (typeinfo_reg == NULL || codegen_had_error(ctx))
    return inst_list;

  if (out_reg != NULL)
    *out_reg = typeinfo_reg;
  else
    free_reg(get_reg_stack(), typeinfo_reg);
  return inst_list;
}

static void codegen_move_rtti_args(ListNode_t **inst_list, CodeGenContext *ctx,
                                   Register_t *value_reg,
                                   const char *target_label) {
  char buffer[128];
  if (codegen_target_is_windows()) {
    {
      /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
      BeEmitter em = codegen_beemitter(*inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
      BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
      kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
      *inst_list = em.list;
    }
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rdx\n",
             target_label);
    *inst_list = add_inst(*inst_list, buffer);
  } else {
    {
      /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
      BeEmitter em = codegen_beemitter(*inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
      BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
      kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
      *inst_list = em.list;
    }
    snprintf(buffer, sizeof(buffer), "\tleaq\t%s_TYPEINFO(%%rip), %%rsi\n",
             target_label);
    *inst_list = add_inst(*inst_list, buffer);
  }
}
/* Helper to check if a formal parameter declaration expects a string type. */

/* Return 1 if the mangled call target already expects a char (not a string)
 * for the given argument position.  This prevents the codegen from inserting
 * a spurious kgpc_char_to_string promotion when a user-defined wrapper
 * (e.g. SysUtils.Pos) declares the formal parameter as AnsiString but the
 * semantic checker has already rewritten the call to a char-specific runtime
 * overload.
 *
 * The naming convention for Pos overloads encodes argument types as a
 * two-letter suffix after "kgpc_string_pos_":
 *   first  letter = substr type  (c = char, a = ansistring, s = shortstring)
 *   second letter = value  type  (c = char, a = ansistring, s = shortstring)
 * arg_index 0 corresponds to the first letter, arg_index 1 to the second. */

static ListNode_t *codegen_expr_convert_int_like_to_real(ListNode_t *inst_list,
                                                         CodeGenContext *ctx,
                                                         Register_t *value_reg,
                                                         int source_type) {
  if (inst_list == NULL || value_reg == NULL)
    return inst_list;

  {
    char buffer_tmpl[128];
    if (source_type == LONGINT_TYPE || source_type == INT64_TYPE ||
        source_type == QWORD_TYPE)
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tcvtsi2sdq\t%%0, %%xmm0\n");
    else
      snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tcvtsi2sdl\t%%0, %%xmm0\n");
    Register_t *u[] = {value_reg};
    inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1, buffer_tmpl);
  }

  {
    Register_t *d[] = {value_reg};
    return add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%xmm0, %0\n");
  }
}

ListNode_t *codegen_expr_maybe_convert_int_like_to_real(
    int target_type, struct Expression *source_expr, Register_t *value_reg,
    ListNode_t *inst_list, CodeGenContext *ctx) {
  if (inst_list == NULL || source_expr == NULL || value_reg == NULL)
    return inst_list;

  int source_type = expr_get_type_tag(source_expr);
  int conversion_type = source_type;
  if (target_type == REAL_TYPE && source_type == REAL_TYPE &&
      source_expr->type == EXPR_TYPECAST &&
      source_expr->expr_data.typecast_data.expr != NULL) {
    int inner_type =
        expr_get_type_tag(source_expr->expr_data.typecast_data.expr);
    if (inner_type == INT_TYPE || inner_type == LONGINT_TYPE ||
        inner_type == INT64_TYPE || inner_type == QWORD_TYPE)
      conversion_type = inner_type;
  }
  if (target_type == REAL_TYPE &&
      (conversion_type == INT_TYPE || conversion_type == LONGINT_TYPE ||
       conversion_type == INT64_TYPE || conversion_type == QWORD_TYPE)) {
    inst_list = codegen_expr_convert_int_like_to_real(inst_list, ctx, value_reg,
                                                      conversion_type);
  }

  return inst_list;
}

static unsigned long codegen_expr_next_temp_suffix(void) {
  static unsigned long counter = 0;
  return ++counter;
}

int codegen_expr_align_to(int value, int alignment) {
  if (alignment <= 0)
    return value;
  int remainder = value % alignment;
  if (remainder == 0)
    return value;
  return value + (alignment - remainder);
}

StackNode_t *codegen_alloc_temp_bytes(const char *prefix, int size) {
  if (size <= 0)
    size = DOUBLEWORD;
  char label[32];
  snprintf(label, sizeof(label), "%s_%lu", prefix != NULL ? prefix : "temp",
           codegen_expr_next_temp_suffix());
  return add_l_t_bytes(label, size);
}

int codegen_expr_involves_extended(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  if (expr_get_kgpc_type(expr) != NULL &&
      kgpc_type_is_extended(expr_get_kgpc_type(expr)))
    return 1;

  switch (expr->type) {
  case EXPR_SIGN_TERM:
    return codegen_expr_involves_extended(expr->expr_data.sign_term);
  case EXPR_ADDOP:
    return codegen_expr_involves_extended(
               expr->expr_data.addop_data.left_expr) ||
           codegen_expr_involves_extended(
               expr->expr_data.addop_data.right_term);
  case EXPR_MULOP:
    return codegen_expr_involves_extended(
               expr->expr_data.mulop_data.left_term) ||
           codegen_expr_involves_extended(
               expr->expr_data.mulop_data.right_factor);
  case EXPR_TYPECAST:
    return codegen_expr_involves_extended(expr->expr_data.typecast_data.expr);
  default:
    return 0;
  }
}

static ListNode_t *codegen_extended_copy(ListNode_t *inst_list,
                                         CodeGenContext *ctx,
                                         const char *dest_addr,
                                         const char *src_addr) {
  if (dest_addr == NULL || src_addr == NULL)
    return inst_list;

  if (codegen_target_is_windows()) {
    char buffer[CODEGEN_MAX_INST_BUF];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_addr);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\tmovl\t$10, %r8d\n");
  } else {
    char buffer[CODEGEN_MAX_INST_BUF];
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_addr);
    inst_list = add_inst(inst_list, buffer);
    inst_list = add_inst(inst_list, "\tmovl\t$10, %edx\n");
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_move");
  free_arg_regs();
  return inst_list;
}

static ListNode_t *codegen_extended_store_from_reg(ListNode_t *inst_list,
                                                   CodeGenContext *ctx,
                                                   struct Expression *expr,
                                                   Register_t *value_reg,
                                                   const char *dest_addr) {
  if (value_reg == NULL || dest_addr == NULL)
    return inst_list;

  int source_type = expr_get_type_tag(expr);
  char buffer[CODEGEN_MAX_INST_BUF];
  if (source_type == INT_TYPE || source_type == LONGINT_TYPE ||
      source_type == INT64_TYPE || source_type == QWORD_TYPE) {
    const char *src_reg64 = value_reg->bit_64;
    if (source_type == INT_TYPE) {
      inst_list = codegen_sign_extend32_to64(inst_list, value_reg->bit_32,
                                             value_reg->bit_64);
    }
    if (codegen_target_is_windows()) {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdx\n", src_reg64);
      inst_list = add_inst(inst_list, buffer);
    } else {
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr);
      inst_list = add_inst(inst_list, buffer);
      snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rsi\n", src_reg64);
      inst_list = add_inst(inst_list, buffer);
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(
        inst_list, "kgpc_store_extended_from_int64");
    free_arg_regs();
    return inst_list;
  }

  inst_list = codegen_expr_maybe_convert_int_like_to_real(
      REAL_TYPE, expr, value_reg, inst_list, ctx);
  if (codegen_target_is_windows()) {
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rcx\n", dest_addr);
    inst_list = add_inst(inst_list, buffer);
    {
      /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
      BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
      kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }
  } else {
    snprintf(buffer, sizeof(buffer), "\tmovq\t%s, %%rdi\n", dest_addr);
    inst_list = add_inst(inst_list, buffer);
    {
      /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rsi"}};
      BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
      kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }
  }
  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list,
                                             "kgpc_store_extended_from_bits");
  free_arg_regs();
  return inst_list;
}

static ListNode_t *codegen_materialize_extended_expr_internal(
    struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *dest_addr_reg) {
  if (expr == NULL || ctx == NULL || dest_addr_reg == NULL)
    return inst_list;

  StackNode_t *dest_slot = add_l_t("ext_dest_ptr");
  if (dest_slot == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to allocate spill slot for Extended destination.");
    return inst_list;
  }
  {
    /* Integrated: store a physical register to the frame slot via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
    BeOperand a = {OPK_PHYS, BE_W64, {.phys = dest_addr_reg->bit_64}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }

  switch (expr->type) {
  case EXPR_SIGN_TERM: {
    StackNode_t *src_slot = codegen_alloc_temp_bytes("ext_neg", 10);
    StackNode_t *src_ptr_slot = add_l_t("ext_neg_ptr");
    Register_t *src_addr = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (src_slot == NULL || src_ptr_slot == NULL || src_addr == NULL)
      return inst_list;
    {
      /* Integrated: address-of the frame slot into a physical register via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = src_addr->bit_64}};
      BeOperand src = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(src_slot->offset)}}};
      kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }
    {
      /* Integrated: store a physical register to the frame slot via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(src_ptr_slot->offset)}}};
      BeOperand a = {OPK_PHYS, BE_W64, {.phys = src_addr->bit_64}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
      inst_list = em.list;
    }
    inst_list = codegen_materialize_extended_expr_internal(
        expr->expr_data.sign_term, inst_list, ctx, src_addr);
    if (codegen_target_is_windows()) {
      {
        /* Integrated: load from the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: load from the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(src_ptr_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
    } else {
      {
        /* Integrated: load from the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: load from the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rsi"}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(src_ptr_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
    }
    inst_list = codegen_vect_reg(inst_list, 0);
    inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_extended_neg");
    free_arg_regs();
    free_reg(get_reg_stack(), src_addr);
    return inst_list;
  }
  case EXPR_ADDOP:
  case EXPR_MULOP: {
    int op_type = (expr->type == EXPR_ADDOP)
                      ? expr->expr_data.addop_data.addop_type
                      : expr->expr_data.mulop_data.mulop_type;
    const char *helper = NULL;
    switch (op_type) {
    case PLUS:
      helper = "kgpc_extended_add";
      break;
    case MINUS:
      helper = "kgpc_extended_sub";
      break;
    case STAR:
      helper = "kgpc_extended_mul";
      break;
    case SLASH:
      helper = "kgpc_extended_div";
      break;
    default:
      break;
    }
    if (helper != NULL) {
      struct Expression *left_expr = (expr->type == EXPR_ADDOP)
                                         ? expr->expr_data.addop_data.left_expr
                                         : expr->expr_data.mulop_data.left_term;
      struct Expression *right_expr =
          (expr->type == EXPR_ADDOP) ? expr->expr_data.addop_data.right_term
                                     : expr->expr_data.mulop_data.right_factor;
      StackNode_t *lhs_slot = codegen_alloc_temp_bytes("ext_lhs", 10);
      StackNode_t *rhs_slot = codegen_alloc_temp_bytes("ext_rhs", 10);
      StackNode_t *lhs_ptr_slot = add_l_t("ext_lhs_ptr");
      StackNode_t *rhs_ptr_slot = add_l_t("ext_rhs_ptr");
      Register_t *lhs_addr = get_reg_with_spill(get_reg_stack(), &inst_list);
      Register_t *rhs_addr = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (lhs_slot == NULL || rhs_slot == NULL || lhs_ptr_slot == NULL ||
          rhs_ptr_slot == NULL || lhs_addr == NULL || rhs_addr == NULL)
        return inst_list;
      {
        /* Integrated: address-of the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = lhs_addr->bit_64}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(lhs_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: address-of the frame slot into a physical register via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = rhs_addr->bit_64}};
        BeOperand src = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(rhs_slot->offset)}}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(lhs_ptr_slot->offset)}}};
        BeOperand a = {OPK_PHYS, BE_W64, {.phys = lhs_addr->bit_64}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(rhs_ptr_slot->offset)}}};
        BeOperand a = {OPK_PHYS, BE_W64, {.phys = rhs_addr->bit_64}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
      inst_list = codegen_materialize_extended_expr_internal(
          left_expr, inst_list, ctx, lhs_addr);
      inst_list = codegen_materialize_extended_expr_internal(
          right_expr, inst_list, ctx, rhs_addr);
      if (codegen_target_is_windows()) {
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(lhs_ptr_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%r8"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(rhs_ptr_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
      } else {
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rsi"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(lhs_ptr_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        {
          /* Integrated: load from the frame slot into a physical register via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
          BeOperand src = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(rhs_ptr_slot->offset)}}};
          kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
      }
      inst_list = codegen_vect_reg(inst_list, 0);
      inst_list = codegen_call_with_shadow_space(inst_list, helper);
      free_arg_regs();
      free_reg(get_reg_stack(), rhs_addr);
      free_reg(get_reg_stack(), lhs_addr);
      return inst_list;
    }
    break;
  }
  case EXPR_TYPECAST:
    if (expr->expr_data.typecast_data.expr != NULL) {
      struct Expression *inner = expr->expr_data.typecast_data.expr;
      if (codegen_expr_involves_extended(inner))
        return codegen_materialize_extended_expr_internal(inner, inst_list, ctx,
                                                          dest_addr_reg);
    }
    break;
  default:
    break;
  }

  if (codegen_expr_involves_extended(expr) &&
      codegen_expr_is_addressable(expr)) {
    Register_t *src_addr = NULL;
    Register_t *dest_reload = get_reg_with_spill(get_reg_stack(), &inst_list);
    if (dest_reload == NULL)
      return inst_list;
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &src_addr);
    if (codegen_had_error(ctx) || src_addr == NULL)
      return inst_list;
    {
      /* Integrated: load from the frame slot into a physical register via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_PHYS, BE_W64, {.phys = dest_reload->bit_64}};
      BeOperand src = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
      kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }
    inst_list = codegen_extended_copy(inst_list, ctx, dest_reload->bit_64,
                                      src_addr->bit_64);
    free_reg(get_reg_stack(), dest_reload);
    free_reg(get_reg_stack(), src_addr);
    return inst_list;
  }

  Register_t *value_reg = NULL;
  Register_t *dest_reload = get_reg_with_spill(get_reg_stack(), &inst_list);
  if (dest_reload == NULL)
    return inst_list;
  inst_list = codegen_expr_with_result(expr, inst_list, ctx, &value_reg);
  if (codegen_had_error(ctx) || value_reg == NULL)
    return inst_list;
  {
    /* Integrated: load from the frame slot into a physical register via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_PHYS, BE_W64, {.phys = dest_reload->bit_64}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(dest_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LOAD, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  inst_list = codegen_extended_store_from_reg(inst_list, ctx, expr, value_reg,
                                              dest_reload->bit_64);
  free_reg(get_reg_stack(), dest_reload);
  free_reg(get_reg_stack(), value_reg);
  return inst_list;
}

ListNode_t *codegen_materialize_extended_expr(struct Expression *expr,
                                              ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              Register_t *dest_addr_reg) {
  return codegen_materialize_extended_expr_internal(expr, inst_list, ctx,
                                                    dest_addr_reg);
}

ListNode_t *codegen_emit_is_expr(struct Expression *expr, ListNode_t *inst_list,
                                 CodeGenContext *ctx, Register_t **out_reg) {
  if (out_reg != NULL)
    *out_reg = NULL;

  if (expr == NULL)
    return inst_list;

  const char *target_label = NULL;
  Register_t *target_typeinfo_reg = NULL;

  /* Support dynamic class-reference variables on RHS (Obj is ObjType).
   * This also handles bare field names in FPC RTL method bodies that
   * bypass semcheck — codegen_get_nonlocal resolves them as Self.field. */
  if (ctx != NULL && ctx->symtab != NULL &&
      expr->expr_data.is_data.target_record_type == NULL &&
      expr->expr_data.is_data.target_type_id != NULL) {
    HashNode_t *target_node = NULL;
    int found = FindSymbol(&target_node, ctx->symtab,
                           expr->expr_data.is_data.target_type_id);
    int is_dynamic_ref = (found != 0 && target_node != NULL &&
                          target_node->hash_type == HASHTYPE_VAR);
    /* Also treat as dynamic if the name is not in the symtab at all
     * but we're inside a class method (bare field name). */
    if (!is_dynamic_ref && (found == 0 || target_node == NULL) &&
        ctx->current_subprogram_owner_class != NULL &&
        find_label("Self") != NULL)
      is_dynamic_ref = 1;
    if (is_dynamic_ref) {
      Register_t *class_ref_reg = NULL;
      struct Expression target_expr;
      memset(&target_expr, 0, sizeof(target_expr));
      target_expr.line_num = expr->line_num;
      target_expr.col_num = expr->col_num;
      target_expr.type = EXPR_VAR_ID;
      target_expr.expr_data.id = expr->expr_data.is_data.target_type_id;
      inst_list = codegen_expr_with_result(&target_expr, inst_list, ctx,
                                           &class_ref_reg);
      if (class_ref_reg == NULL)
        return inst_list;
      /* The field value is a class reference (VMT pointer).
       * Extract TYPEINFO from VMT slot 7 (VMT_VTYPEINFO_OFFSET). */
      char ti_buf[128];
      snprintf(ti_buf, sizeof(ti_buf), "\tmovq\t%d(%s), %s\n",
               VMT_VTYPEINFO_OFFSET, class_ref_reg->bit_64,
               class_ref_reg->bit_64);
      inst_list = add_inst(inst_list, ti_buf);
      target_typeinfo_reg = class_ref_reg;
    }
  }

  if (target_typeinfo_reg == NULL) {
    target_label =
        codegen_class_typeinfo_label(expr->expr_data.is_data.target_record_type,
                                     expr->expr_data.is_data.target_type_id);
    if (target_label == NULL) {
      codegen_report_error(
          ctx, "ERROR: Unable to resolve class type for \"is\" operator.");
      return inst_list;
    }
  }

  Register_t *value_reg = NULL;
  inst_list = codegen_load_class_typeinfo(expr->expr_data.is_data.expr,
                                          inst_list, ctx, &value_reg);
  if (value_reg == NULL)
    return inst_list;

  if (target_typeinfo_reg != NULL) {
    if (codegen_target_is_windows()) {
      {
        /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
        BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
        kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
        BeOperand src = {OPK_VREG, BE_W64, {.vreg = target_typeinfo_reg}};
        kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
    } else {
      {
        /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
        BeOperand src = {OPK_VREG, BE_W64, {.vreg = value_reg}};
        kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
      {
        /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rsi"}};
        BeOperand src = {OPK_VREG, BE_W64, {.vreg = target_typeinfo_reg}};
        kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
        inst_list = em.list;
      }
    }
  } else {
    codegen_move_rtti_args(&inst_list, ctx, value_reg, target_label);
  }
  free_reg(get_reg_stack(), value_reg);
  if (target_typeinfo_reg != NULL)
    free_reg(get_reg_stack(), target_typeinfo_reg);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_is");
  free_arg_regs();

  if (out_reg != NULL) {
    Register_t *result_reg = codegen_try_get_reg(&inst_list, ctx, "is result");
    if (result_reg == NULL)
      return inst_list;

    {
      Register_t *d[] = {result_reg};
      inst_list =
          add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovl\t%eax, %0\n");
    }
    *out_reg = result_reg;
  }

  return inst_list;
}

static ListNode_t *codegen_emit_class_cast_check_from_instance_ptr(
    struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *instance_ptr_reg) {
  if (expr == NULL || instance_ptr_reg == NULL)
    return inst_list;

  const char *target_label =
      codegen_class_typeinfo_label(expr->expr_data.as_data.target_record_type,
                                   expr->expr_data.as_data.target_type_id);
  if (target_label == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to resolve class type for \"as\" operator.");
    return inst_list;
  }

  /* FPC semantics: (nil as T) yields nil without performing the type check.
   * Skip the VMT/RTTI dereference when the source pointer is NULL. */
  char skip_label[64];
  gen_label(skip_label, sizeof(skip_label), ctx);
  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\ttestq\t%s, %s\n",
           instance_ptr_reg->bit_64, instance_ptr_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_EQ, skip_label);
    inst_list = em.list;
  }

  Register_t *typeinfo_reg = NULL;
  inst_list = codegen_load_typeinfo_from_instance_ptr(
      inst_list, ctx, instance_ptr_reg, &typeinfo_reg);
  if (typeinfo_reg == NULL || codegen_had_error(ctx))
    return inst_list;

  /* Preserve the instance pointer across the runtime call (caller-saved
   * registers may be clobbered). Reserve the 32-byte Windows shadow space as
   * well so the saved pointer is not overwritten. */
  inst_list = add_inst(inst_list, "\tsubq\t$48, %rsp\n");
  {
    Register_t *u[] = {instance_ptr_reg};
    inst_list =
        add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, 32(%rsp)\n");
  }

  codegen_move_rtti_args(&inst_list, ctx, typeinfo_reg, target_label);
  free_reg(get_reg_stack(), typeinfo_reg);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_check_cast");
  free_arg_regs();

  {
    Register_t *d[] = {instance_ptr_reg};
    inst_list =
        add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t32(%rsp), %0\n");
  }
  inst_list = add_inst(inst_list, "\taddq\t$48, %rsp\n");

  snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
  inst_list = add_inst(inst_list, buffer);
  return inst_list;
}

static ListNode_t *codegen_emit_class_cast_check_from_class_vmt_ptr(
    struct Expression *expr, ListNode_t *inst_list, CodeGenContext *ctx,
    Register_t *class_vmt_reg) {
  if (expr == NULL || class_vmt_reg == NULL)
    return inst_list;

  const char *target_label =
      codegen_class_typeinfo_label(expr->expr_data.as_data.target_record_type,
                                   expr->expr_data.as_data.target_type_id);
  if (target_label == NULL) {
    codegen_report_error(
        ctx, "ERROR: Unable to resolve class type for \"as\" operator.");
    return inst_list;
  }

  /* FPC semantics: (nil as T) yields nil without performing the type check. */
  char skip_label[64];
  gen_label(skip_label, sizeof(skip_label), ctx);
  char buffer[128];
  snprintf(buffer, sizeof(buffer), "\ttestq\t%s, %s\n", class_vmt_reg->bit_64,
           class_vmt_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  {
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    kgpc_backend_target()->emit_branch(&em, BE_EQ, skip_label);
    inst_list = em.list;
  }

  Register_t *typeinfo_reg = NULL;
  inst_list = codegen_load_typeinfo_from_class_vmt_ptr(
      inst_list, ctx, class_vmt_reg, &typeinfo_reg);
  if (typeinfo_reg == NULL || codegen_had_error(ctx))
    return inst_list;

  inst_list = add_inst(inst_list, "\tsubq\t$48, %rsp\n");
  {
    Register_t *u[] = {class_vmt_reg};
    inst_list =
        add_inst_du(inst_list, ctx, NULL, 0, u, 1, "\tmovq\t%0, 32(%rsp)\n");
  }

  codegen_move_rtti_args(&inst_list, ctx, typeinfo_reg, target_label);
  free_reg(get_reg_stack(), typeinfo_reg);

  inst_list = codegen_vect_reg(inst_list, 0);
  inst_list = codegen_call_with_shadow_space(inst_list, "kgpc_rtti_check_cast");
  free_arg_regs();

  {
    Register_t *d[] = {class_vmt_reg};
    inst_list =
        add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t32(%rsp), %0\n");
  }
  inst_list = add_inst(inst_list, "\taddq\t$48, %rsp\n");

  snprintf(buffer, sizeof(buffer), "%s:\n", skip_label);
  inst_list = add_inst(inst_list, buffer);
  return inst_list;
}

ListNode_t *codegen_emit_class_cast_check_from_address(struct Expression *expr,
                                                       ListNode_t *inst_list,
                                                       CodeGenContext *ctx,
                                                       Register_t *addr_reg) {
  if (expr == NULL || addr_reg == NULL)
    return inst_list;

  struct Expression *source_expr = expr->expr_data.as_data.expr;
  struct RecordType *source_record =
      codegen_expr_record_type(source_expr, ctx != NULL ? ctx->symtab : NULL);
  int is_class_var =
      (source_record != NULL && record_type_is_class(source_record));

  Register_t *instance_ptr_reg = addr_reg;
  if (is_class_var) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "\tmovq\t(%s), %s\n", addr_reg->bit_64,
             addr_reg->bit_64);
    inst_list = add_inst(inst_list, buffer);
  }

  return codegen_emit_class_cast_check_from_instance_ptr(expr, inst_list, ctx,
                                                         instance_ptr_reg);
}

ListNode_t *codegen_emit_class_cast_check(struct Expression *expr,
                                          ListNode_t *inst_list,
                                          CodeGenContext *ctx) {
  if (expr == NULL || expr->expr_data.as_data.expr == NULL)
    return inst_list;

  Register_t *addr_reg = NULL;
  inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr, inst_list,
                                       ctx, &addr_reg);
  if (addr_reg == NULL)
    return inst_list;

  inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list, ctx,
                                                         addr_reg);
  free_reg(get_reg_stack(), addr_reg);
  return inst_list;
}

static inline const char *codegen_register_id_to_8bit(RegisterId_t reg_id) {
  switch (reg_id) {
  case REG_RAX:
    return "%al";
  case REG_RBX:
    return "%bl";
  case REG_RCX:
    return "%cl";
  case REG_RDX:
    return "%dl";
  case REG_RSI:
    return "%sil";
  case REG_RDI:
    return "%dil";
  case REG_RBP:
    return "%bpl";
  case REG_RSP:
    return "%spl";
  case REG_R8:
    return "%r8b";
  case REG_R9:
    return "%r9b";
  case REG_R10:
    return "%r10b";
  case REG_R11:
    return "%r11b";
  case REG_R12:
    return "%r12b";
  case REG_R13:
    return "%r13b";
  case REG_R14:
    return "%r14b";
  case REG_R15:
    return "%r15b";
  default:
    return NULL;
  }
}

static inline const char *codegen_register_id_to_16bit(RegisterId_t reg_id) {
  switch (reg_id) {
  case REG_RAX:
    return "%ax";
  case REG_RBX:
    return "%bx";
  case REG_RCX:
    return "%cx";
  case REG_RDX:
    return "%dx";
  case REG_RSI:
    return "%si";
  case REG_RDI:
    return "%di";
  case REG_RBP:
    return "%bp";
  case REG_RSP:
    return "%sp";
  case REG_R8:
    return "%r8w";
  case REG_R9:
    return "%r9w";
  case REG_R10:
    return "%r10w";
  case REG_R11:
    return "%r11w";
  case REG_R12:
    return "%r12w";
  case REG_R13:
    return "%r13w";
  case REG_R14:
    return "%r14w";
  case REG_R15:
    return "%r15w";
  default:
    return NULL;
  }
}

static const char *codegen_register_name8(const Register_t *reg) {
  if (reg == NULL || reg->bit_64 == NULL)
    return NULL;
  return codegen_register_id_to_8bit(reg->reg_id);
}

const char *codegen_register_name16(const Register_t *reg) {
  if (reg == NULL || reg->bit_64 == NULL)
    return NULL;
  return codegen_register_id_to_16bit(reg->reg_id);
}

static ListNode_t *codegen_store_value_to_stack(ListNode_t *inst_list,
                                                CodeGenContext *ctx,
                                                Register_t *value_reg,
                                                int offset, int element_size) {
  if (value_reg == NULL)
    return inst_list;

  if (element_size == 1) {
    const char *reg8 = codegen_register_name8(value_reg);
    assert(reg8 != NULL && "8-bit register name not found for store operation");
    {
      /* Integrated: store a physical register to the frame slot via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W8,
                       {.mem_frame = {BE_BASE_FP, -(long long)(offset)}}};
      BeOperand a = {OPK_PHYS, BE_W8, {.phys = reg8}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W8, &dst, &a, NULL);
      return em.list;
    }
  } else if (element_size == 2) {
    const char *reg16 = codegen_register_name16(value_reg);
    assert(reg16 != NULL &&
           "16-bit register name not found for store operation");
    {
      /* Integrated: store a physical register to the frame slot via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W16,
                       {.mem_frame = {BE_BASE_FP, -(long long)(offset)}}};
      BeOperand a = {OPK_PHYS, BE_W16, {.phys = reg16}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W16, &dst, &a, NULL);
      return em.list;
    }
  } else if (element_size == 4) {
    {
      /* Integrated: store to the frame slot through the backend vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_MEM_FRAME, BE_W32,
                       {.mem_frame = {BE_BASE_FP, -(long long)(offset)}}};
      BeOperand a = {OPK_VREG, BE_W32, {.vreg = value_reg}};
      kgpc_backend_target()->emit(&em, BE_STORE, BE_W32, &dst, &a, NULL);
      return em.list;
    }
  }

  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = value_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    return em.list;
  }
}

ListNode_t *codegen_materialize_array_literal(struct Expression *expr,
                                              ListNode_t *inst_list,
                                              CodeGenContext *ctx,
                                              Register_t **out_reg) {
  if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL || out_reg == NULL)
    return inst_list;

  if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
    return codegen_materialize_array_of_const(expr, inst_list, ctx, out_reg);

  int element_size = expr_get_array_element_size(expr, ctx);
  if (element_size <= 0)
    element_size = DOUBLEWORD;

  int element_count = expr->expr_data.array_literal_data.element_count;
  if (element_count == 0) {
    const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
    int descriptor_size =
        codegen_expr_align_to(2 * pointer_bytes, pointer_bytes);
    if (expr->array_element_size > 0) {
      int candidate = expr->array_element_size * 2;
      if (descriptor_size < candidate)
        descriptor_size = codegen_expr_align_to(candidate, pointer_bytes);
    }
    StackNode_t *desc_slot =
        codegen_alloc_temp_bytes("arr_lit_desc", descriptor_size);
    if (desc_slot == NULL)
      return inst_list;

    Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
    if (addr_reg == NULL) {
      codegen_report_error(
          ctx,
          "ERROR: Unable to allocate register for array literal descriptor.");
      return inst_list;
    }

    char buffer[128];
    snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
             desc_slot->offset);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
             desc_slot->offset - pointer_bytes);
    inst_list = add_inst(inst_list, buffer);

    int field_count = descriptor_size / pointer_bytes;
    for (int field = 2; field < field_count; ++field) {
      int field_offset = desc_slot->offset - field * pointer_bytes;
      snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n",
               field_offset);
      inst_list = add_inst(inst_list, buffer);
    }

    {
      /* Integrated: address-of the frame slot into a pool register via the vtable. */
      BeEmitter em = codegen_beemitter(inst_list, ctx);
      BeOperand dst = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
      BeOperand src = {OPK_MEM_FRAME, BE_W64,
                       {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset)}}};
      kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
      inst_list = em.list;
    }

    *out_reg = addr_reg;
    return inst_list;
  }
  int data_size =
      codegen_expr_align_to(element_count * element_size, DOUBLEWORD);
  StackNode_t *data_slot = codegen_alloc_temp_bytes("arr_lit_data", data_size);
  if (data_slot == NULL)
    return inst_list;

  /* When the formal array element is an inline ShortString slot (size N+1 bytes
   * containing {length_byte, chars[N]}), each element source value -- which may
   * be a string literal (AnsiString pointer), an AnsiString variable, a CHAR,
   * or another ShortString -- must be converted into that slot via the runtime
   * helper.  Without this, codegen_store_value_to_stack would just write an
   * 8-byte movq of the pointer/ordinal into the slot, leaving the length byte
   * holding a low pointer byte and the rest of the slot uninitialized.  This
   * matters in particular for FPC's RTTI emission path
   * (queue_subscriptn_multiple_by_name in aasmcnst.pas) which passes
   * `array of TIDString = string[127]` literals; without the conversion the
   * callee sees garbage length bytes and trips an internalerror. */
  int element_is_shortstring_slot =
      (expr->array_element_type == SHORTSTRING_TYPE && element_size >= 2);

  ListNode_t *cur = expr->expr_data.array_literal_data.elements;
  int index = 0;
  while (cur != NULL) {
    struct Expression *element_expr = (struct Expression *)cur->cur;
    int element_offset = data_slot->offset - index * element_size;

    if (element_is_shortstring_slot) {
      /* Determine the conversion path based on the source expression's
       * type: shortstring source -> kgpc_shortstring_to_shortstring,
       * char source -> kgpc_char_to_string + kgpc_string_to_shortstring,
       * everything else (AnsiString / string literal) ->
       * kgpc_string_to_shortstring.  Anything that produces something
       * other than a string-like value falls back to the legacy store. */
      int src_is_shortstring = codegen_expr_is_shortstring_value(element_expr);
      int src_is_char =
          (!src_is_shortstring && expr_get_type_tag(element_expr) == CHAR_TYPE);
      int src_is_stringlike =
          (!src_is_shortstring && !src_is_char &&
           (element_expr->type == EXPR_STRING ||
            expr_get_type_tag(element_expr) == STRING_TYPE));

      if (src_is_shortstring || src_is_char || src_is_stringlike) {
        Register_t *src_reg = NULL;
        if (src_is_shortstring && codegen_expr_is_addressable(element_expr))
          inst_list =
              codegen_address_for_expr(element_expr, inst_list, ctx, &src_reg);
        else
          inst_list =
              codegen_expr_with_result(element_expr, inst_list, ctx, &src_reg);
        if (codegen_had_error(ctx) || src_reg == NULL) {
          if (src_reg != NULL)
            free_reg(get_reg_stack(), src_reg);
          return inst_list;
        }

        /* For a CHAR ordinal, promote to a heap AnsiString first so
         * kgpc_string_to_shortstring can dereference it as a pointer. */
        if (src_is_char) {
          const char *char_arg32 = current_arg_reg32(0);
          if (char_arg32 == NULL)
            char_arg32 = "%edi";
          {
            /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W32, {.phys = char_arg32}};
            BeOperand src = {OPK_VREG, BE_W32, {.vreg = src_reg}};
            kgpc_backend_target()->emit(&em, BE_MOV, BE_W32, &dst, &src, NULL);
            inst_list = em.list;
          }
          inst_list = codegen_vect_reg(inst_list, 0);
          inst_list =
              codegen_call_with_shadow_space(inst_list, "kgpc_char_to_string");
          {
            /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_VREG, BE_W64, {.vreg = src_reg}};
            BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
            kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          free_arg_regs();
        }

        /* Compute dest address (leaq -element_offset(%rbp), arg0).
         * src_reg holds the source pointer.  Call signature:
         *   void kgpc_string_to_shortstring(char *dest,
         *                                   const char *src,
         *                                   size_t dest_size);
         * kgpc_string_to_shortstring also handles ShortString sources
         * (unmanaged buffers with a length byte at offset 0). */
        char buf[128];
        if (codegen_target_is_windows()) {
          {
            /* Integrated: address-of the frame slot into a physical register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rcx"}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(element_offset)}}};
            kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          {
            /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdx"}};
            BeOperand src = {OPK_VREG, BE_W64, {.vreg = src_reg}};
            kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          snprintf(buf, sizeof(buf), "\tmovl\t$%d, %%r8d\n", element_size);
          inst_list = add_inst(inst_list, buf);
        } else {
          {
            /* Integrated: address-of the frame slot into a physical register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rdi"}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(element_offset)}}};
            kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          {
            /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = "%rsi"}};
            BeOperand src = {OPK_VREG, BE_W64, {.vreg = src_reg}};
            kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          snprintf(buf, sizeof(buf), "\tmovl\t$%d, %%edx\n", element_size);
          inst_list = add_inst(inst_list, buf);
        }
        inst_list = add_inst(inst_list, "\tmovl\t$0, %eax\n");
        inst_list = codegen_call_with_shadow_space(
            inst_list, "kgpc_string_to_shortstring");
        free_arg_regs();
        free_reg(get_reg_stack(), src_reg);

        cur = cur->next;
        ++index;
        continue;
      }
      /* fall through to legacy path for unrecognized source kinds */
    }

    Register_t *value_reg = NULL;
    inst_list =
        codegen_expr_with_result(element_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL) {
      if (value_reg != NULL)
        free_reg(get_reg_stack(), value_reg);
      return inst_list;
    }

    inst_list = codegen_store_value_to_stack(inst_list, ctx, value_reg,
                                             element_offset, element_size);
    free_reg(get_reg_stack(), value_reg);

    cur = cur->next;
    ++index;
  }

  const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
  int descriptor_size = codegen_expr_align_to(2 * pointer_bytes, pointer_bytes);
  if (expr->array_element_size > 0) {
    int candidate = expr->array_element_size * 2;
    if (descriptor_size < candidate)
      descriptor_size = codegen_expr_align_to(candidate, pointer_bytes);
  }
  StackNode_t *desc_slot =
      codegen_alloc_temp_bytes("arr_lit_desc", descriptor_size);
  if (desc_slot == NULL)
    return inst_list;

  Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (addr_reg == NULL) {
    codegen_report_error(
        ctx,
        "ERROR: Unable to allocate register for array literal descriptor.");
    return inst_list;
  }

  char buffer[128];
  {
    /* Integrated: address-of the frame slot into a pool register via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(data_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }

  {
    char buffer_tmpl[128];
    snprintf(buffer_tmpl, sizeof(buffer_tmpl), "\tmovq\t$%d, %%0\n",
             element_count);
    Register_t *d[] = {addr_reg};
    inst_list = add_inst_du(inst_list, ctx, d, 1, NULL, 0, buffer_tmpl);
  }
  {
    /* Integrated: store to the frame slot through the backend vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset - pointer_bytes)}}};
    BeOperand a = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }

  int field_count = descriptor_size / pointer_bytes;
  for (int field = 2; field < field_count; ++field) {
    int field_offset = desc_slot->offset - field * pointer_bytes;
    snprintf(buffer, sizeof(buffer), "\tmovq\t$0, -%d(%%rbp)\n", field_offset);
    inst_list = add_inst(inst_list, buffer);
  }

  {
    /* Integrated: address-of the frame slot into a pool register via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  *out_reg = addr_reg;
  return inst_list;
}

typedef struct {
  int type_tag;
  int tvar_kind;
} TypeTagToTvarKind;

static const TypeTagToTvarKind tvar_kind_table[] = {
    {INT_TYPE, KGPC_TVAR_KIND_INT},
    {LONGINT_TYPE, KGPC_TVAR_KIND_INT},
    {BYTE_TYPE, KGPC_TVAR_KIND_INT},
    {WORD_TYPE, KGPC_TVAR_KIND_INT},
    {LONGWORD_TYPE, KGPC_TVAR_KIND_INT},
    {INT64_TYPE, KGPC_TVAR_KIND_INT},
    {QWORD_TYPE, KGPC_TVAR_KIND_INT},
    {ENUM_TYPE, KGPC_TVAR_KIND_INT},
    {BOOL, KGPC_TVAR_KIND_BOOL},
    {CHAR_TYPE, KGPC_TVAR_KIND_CHAR},
    {REAL_TYPE, KGPC_TVAR_KIND_REAL},
    {STRING_TYPE, KGPC_TVAR_KIND_ANSISTRING},
    {SHORTSTRING_TYPE, KGPC_TVAR_KIND_STRING},
    {POINTER_TYPE, KGPC_TVAR_KIND_POINTER},
};

static int codegen_format_arg_kind_for_expr(struct Expression *expr) {
  int type_tag = expr_get_type_tag(expr);
  for (size_t i = 0; i < sizeof(tvar_kind_table) / sizeof(tvar_kind_table[0]);
       ++i) {
    if (tvar_kind_table[i].type_tag == type_tag)
      return tvar_kind_table[i].tvar_kind;
  }
  return -1;
}

static ListNode_t *codegen_materialize_array_of_const(struct Expression *expr,
                                                      ListNode_t *inst_list,
                                                      CodeGenContext *ctx,
                                                      Register_t **out_reg) {
  if (expr == NULL || expr->type != EXPR_ARRAY_LITERAL || out_reg == NULL)
    return inst_list;

  const int element_size = (int)sizeof(kgpc_tvarrec);
  int element_count = expr->expr_data.array_literal_data.element_count;
  int data_size =
      codegen_expr_align_to(element_count * element_size, DOUBLEWORD);
  StackNode_t *data_slot =
      codegen_alloc_temp_bytes("arr_const_data", data_size);
  if (data_slot == NULL)
    return inst_list;

  ListNode_t *cur = expr->expr_data.array_literal_data.elements;
  int index = 0;
  char buffer[128];

  while (cur != NULL) {
    struct Expression *element_expr = (struct Expression *)cur->cur;
    Register_t *value_reg = NULL;
    inst_list =
        codegen_expr_with_result(element_expr, inst_list, ctx, &value_reg);
    if (codegen_had_error(ctx) || value_reg == NULL) {
      if (value_reg != NULL)
        free_reg(get_reg_stack(), value_reg);
      return inst_list;
    }

    int kind = codegen_format_arg_kind_for_expr(element_expr);
    if (kind < 0) {
      codegen_report_error(
          ctx, "ERROR: Unsupported argument type %d in array of const literal.",
          expr_get_type_tag(element_expr));
      free_reg(get_reg_stack(), value_reg);
      return inst_list;
    }

    int element_offset = data_slot->offset - index * element_size;
    snprintf(buffer, sizeof(buffer), "\tmovl\t$%d, -%d(%%rbp)\n", kind,
             element_offset);
    inst_list = add_inst(inst_list, buffer);
    snprintf(buffer, sizeof(buffer), "\tmovl\t$0, -%d(%%rbp)\n",
             element_offset - 4);
    inst_list = add_inst(inst_list, buffer);

    int data_offset = element_offset - 8;
    if (kind == KGPC_TVAR_KIND_REAL) {
      /* FPC's vtExtended expects a pointer to the Extended/Double value,
       * not the raw value.  Allocate a temp slot, store the double there,
       * and put the address in the TVarRec data field. */
      StackNode_t *real_slot = codegen_alloc_temp_bytes("varrec_real", 8);
      if (real_slot != NULL) {
        {
          /* Integrated: store a physical register to the frame slot via the vtable. */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                           {.mem_frame = {BE_BASE_FP, -(long long)(real_slot->offset)}}};
          BeOperand a = {OPK_PHYS, BE_W64, {.phys = value_reg->bit_64}};
          kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
          inst_list = em.list;
        }
        Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
        if (addr_reg != NULL) {
          {
            /* Integrated: address-of the frame slot into a physical register via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
            BeOperand src = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(real_slot->offset)}}};
            kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
            inst_list = em.list;
          }
          {
            /* Integrated: store a physical register to the frame slot via the vtable. */
            BeEmitter em = codegen_beemitter(inst_list, ctx);
            BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                             {.mem_frame = {BE_BASE_FP, -(long long)(data_offset)}}};
            BeOperand a = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
            kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
            inst_list = em.list;
          }
          free_reg(get_reg_stack(), addr_reg);
        }
      }
    } else {
      {
        /* Integrated: store a physical register to the frame slot via the vtable. */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                         {.mem_frame = {BE_BASE_FP, -(long long)(data_offset)}}};
        BeOperand a = {OPK_PHYS, BE_W64, {.phys = value_reg->bit_64}};
        kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }
    }

    free_reg(get_reg_stack(), value_reg);
    cur = cur->next;
    ++index;
  }

  const int pointer_bytes = CODEGEN_POINTER_SIZE_BYTES;
  StackNode_t *desc_slot = codegen_alloc_temp_bytes(
      "arr_const_desc",
      codegen_expr_align_to(2 * pointer_bytes, pointer_bytes));
  if (desc_slot == NULL)
    return inst_list;

  Register_t *addr_reg = get_free_reg(get_reg_stack(), &inst_list);
  if (addr_reg == NULL) {
    codegen_report_error(
        ctx,
        "ERROR: Unable to allocate register for array of const descriptor.");
    return inst_list;
  }

  {
    /* Integrated: address-of the frame slot into a physical register via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(data_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }
  {
    /* Integrated: store a physical register to the frame slot via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset)}}};
    BeOperand a = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }

  snprintf(buffer, sizeof(buffer), "\tmovq\t$%d, %s\n", element_count,
           addr_reg->bit_64);
  inst_list = add_inst(inst_list, buffer);
  {
    /* Integrated: store a physical register to the frame slot via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset - pointer_bytes)}}};
    BeOperand a = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
    kgpc_backend_target()->emit(&em, BE_STORE, BE_W64, &dst, &a, NULL);
    inst_list = em.list;
  }

  {
    /* Integrated: address-of the frame slot into a physical register via the vtable. */
    BeEmitter em = codegen_beemitter(inst_list, ctx);
    BeOperand dst = {OPK_PHYS, BE_W64, {.phys = addr_reg->bit_64}};
    BeOperand src = {OPK_MEM_FRAME, BE_W64,
                     {.mem_frame = {BE_BASE_FP, -(long long)(desc_slot->offset)}}};
    kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &src, NULL);
    inst_list = em.list;
  }

  *out_reg = addr_reg;
  return inst_list;
}

/* Helper function to get TypeAlias from HashNode */
static inline struct TypeAlias *
codegen_get_type_alias_from_node(HashNode_t *node) {
  return hashnode_get_type_alias(node);
}

static unsigned long codegen_next_record_temp_id(void) {
  static unsigned long counter = 0;
  return ++counter;
}

StackNode_t *codegen_alloc_record_temp(long long size) {
  if (size <= 0 || size > INT_MAX)
    return NULL;

  char label[32];
  snprintf(label, sizeof(label), "record_arg_%lu",
           codegen_next_record_temp_id());
  return add_l_t_bytes(label, (int)size);
}

static inline int type_is_file_like(int type_tag) {
  return type_tag == FILE_TYPE || type_tag == TEXT_TYPE;
}

int codegen_type_uses_qword(int type_tag) {
  return (type_tag == REAL_TYPE || type_tag == INT64_TYPE ||
          type_tag == QWORD_TYPE || type_tag == POINTER_TYPE ||
          type_tag == STRING_TYPE || type_tag == SHORTSTRING_TYPE ||
          type_is_file_like(type_tag) || type_tag == PROCEDURE);
}

int codegen_type_is_signed(int type_tag) {
  switch (type_tag) {
  case INT_TYPE:
  case LONGINT_TYPE:
  case INT64_TYPE:
    return 1;
  default:
    return 0;
  }
}

static void codegen_typeinfo_label_for_type_id(SymTab_t *symtab,
                                               const char *type_id,
                                               char *buffer, size_t size) {
  codegen_common_typeinfo_label_for_type_id(symtab, type_id, buffer, size);
}

/* Helper to get KgpcType from expression, preferring resolved_kgpc_type.
 * Returns the KgpcType if available, or creates a temporary one from legacy
 * fields. Returns NULL if type cannot be determined. Note: The returned
 * KgpcType should NOT be freed - it's either owned by the expression or is a
 * static/temporary type. */
KgpcType *expr_get_kgpc_type(const struct Expression *expr) {
  if (expr == NULL)
    return NULL;

  if (expr->resolved_kgpc_type != NULL)
    return expr->resolved_kgpc_type;

  static KgpcType *primitive_cache[256];
  int tag = UNKNOWN_TYPE;

  switch (expr->type) {
  case EXPR_INUM:
    tag = INT_TYPE;
    break;
  case EXPR_RNUM:
    tag = REAL_TYPE;
    break;
  case EXPR_STRING:
    tag = STRING_TYPE;
    break;
  case EXPR_CHAR_CODE:
    tag = CHAR_TYPE;
    break;
  case EXPR_BOOL:
    tag = BOOL;
    break;
  case EXPR_TYPECAST:
    /* A char-valued typecast (char(65), char(byte(x))) yields CHAR_TYPE so
     * downstream char->string promotion fires correctly. Other typecasts are
     * left to resolved_kgpc_type (checked above) / fall through to NULL,
     * preserving prior behaviour. */
    if (expr->expr_data.typecast_data.target_type == CHAR_TYPE)
      tag = CHAR_TYPE;
    break;
  case EXPR_NIL:
    return create_pointer_type(NULL);
  case EXPR_TYPEINFO:
    return create_pointer_type(NULL);
  case EXPR_RECORD_CONSTRUCTOR: {
    struct RecordType *record = codegen_expr_record_type(expr, NULL);
    if (record != NULL)
      return create_record_type(record);
    return NULL;
  }
  case EXPR_POINTER_DEREF: {
    struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr == NULL)
      return NULL;
    KgpcType *ptr_type = expr_get_kgpc_type(pointer_expr);
    if (ptr_type != NULL && kgpc_type_is_pointer(ptr_type))
      return ptr_type->info.points_to;
    return NULL;
  }
  case EXPR_ARRAY_LITERAL: {
    if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
      return create_array_of_const_type();
    if (expr->array_element_type != UNKNOWN_TYPE) {
      KgpcType *elem_type = NULL;
      if (expr->array_element_type == RECORD_TYPE &&
          expr->array_element_record_type != NULL)
        elem_type = create_record_type(expr->array_element_record_type);
      else
        elem_type = create_primitive_type(expr->array_element_type);

      if (elem_type != NULL) {
        int end_index = expr->array_upper_bound;
        if (end_index < expr->array_lower_bound)
          end_index = expr->array_lower_bound - 1;
        return create_array_type(elem_type, expr->array_lower_bound, end_index);
      }
    }
    return NULL;
  }
  case EXPR_FUNCTION_CALL: {
    /* First check if resolved_kgpc_type was set during semcheck */
    if (expr->resolved_kgpc_type != NULL)
      return expr->resolved_kgpc_type;

    /* For function calls, try to get the return type from call_kgpc_type */
    KgpcType *call_type = expr->expr_data.function_call_data.call_kgpc_type;
    if (call_type == NULL &&
        expr->expr_data.function_call_data.resolved_func != NULL) {
      call_type = expr->expr_data.function_call_data.resolved_func->type;
    }
    if (call_type != NULL && call_type->kind == TYPE_KIND_PROCEDURE) {
      KgpcType *ret_type = codegen_function_call_return_type_from_expr(expr);
      if (ret_type != NULL)
        return ret_type;

      /* If return_type is NULL, check return_type_id using type lookup */
      const char *ret_id = call_type->info.proc_info.return_type_id;
      if (kgpc_getenv("KGPC_DEBUG_CODEGEN") != NULL && ret_id != NULL) {
        fprintf(stderr,
                "[CodeGen] expr_get_kgpc_type: EXPR_FUNCTION_CALL "
                "return_type_id='%s'\n",
                ret_id);
      }
      if (ret_id != NULL && kgpc_type_id_uses_qword(ret_id, NULL)) {
        /* Return a pointer type to indicate 64-bit return */
        static KgpcType *cached_pointer = NULL;
        if (cached_pointer == NULL)
          cached_pointer = create_pointer_type(NULL);
        return cached_pointer;
      }
    }
    return NULL;
  }
  default:
    break;
  }

  if (tag != UNKNOWN_TYPE) {
    if (tag >= 0 &&
        tag < (int)(sizeof(primitive_cache) / sizeof(primitive_cache[0]))) {
      if (primitive_cache[tag] == NULL)
        primitive_cache[tag] = create_primitive_type(tag);
      return primitive_cache[tag];
    }
    return create_primitive_type(tag);
  }

  return NULL;
}

long long expr_effective_size_bytes(const struct Expression *expr) {
  /* For pointer dereference, try to get size from the pointer's subtype info.
   * This handles cases like PByte^ where Byte is a subrange type that maps to
   * INT_TYPE but should have size 1. */
  if (expr != NULL && expr->type == EXPR_POINTER_DEREF) {
    struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type)) {
      KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
      if (points_to != NULL) {
        long long size = kgpc_type_sizeof(points_to);
        if (size > 0)
          return size;
      }
    }
  }

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL) {
    long long size = kgpc_type_sizeof(type);
    if (size > 0)
      return size;
  }

  int tag = expr_get_type_tag(expr);
  switch (tag) {
  case CHAR_TYPE:
    return 1;
  case INT_TYPE:
  case BOOL:
  case SET_TYPE:
  case ENUM_TYPE:
    return 4;
  case FILE_TYPE:
    return kgpc_target_filerec_size();
  case TEXT_TYPE:
    return kgpc_target_textrec_size();
  case STRING_TYPE:
  case POINTER_TYPE:
  case REAL_TYPE:
    return 8;
  case LONGINT_TYPE:
    return 4; // Match FPC's 32-bit LongInt
  default:
    return 0;
  }
}

/* Helper to get type tag from expression, preferring resolved_kgpc_type */
int expr_get_type_tag(const struct Expression *expr) {
  if (expr == NULL)
    return UNKNOWN_TYPE;

  if (expr->type == EXPR_MULOP &&
      expr->expr_data.mulop_data.mulop_type == SLASH)
    return REAL_TYPE;

  if (expr->is_array_expr && expr->array_element_type != UNKNOWN_TYPE) {
    if (expr->array_element_type == CHAR_TYPE &&
        expr_get_array_lower_bound(expr) == 0 &&
        expr_get_array_upper_bound(expr) == 255)
      return SHORTSTRING_TYPE;
    return expr->array_element_type;
  }

  /* Prefer KgpcType if available */
  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL) {
    int tag = codegen_tag_from_kgpc(type);
    if (tag != UNKNOWN_TYPE)
      return tag;
    switch (type->kind) {
    case TYPE_KIND_POINTER:
      return POINTER_TYPE;
    case TYPE_KIND_RECORD:
      return RECORD_TYPE;
    case TYPE_KIND_PROCEDURE:
      return PROCEDURE;
    case TYPE_KIND_ARRAY_OF_CONST:
      return ARRAY_OF_CONST_TYPE;
    default:
      break;
    }
  }

  return UNKNOWN_TYPE;
}

/* Helper to get array lower bound from expression, preferring
 * resolved_kgpc_type */
int expr_get_array_lower_bound(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  /* Prefer KgpcType if available */
  if (expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_array(expr->resolved_kgpc_type)) {
    int start = 0;
    if (kgpc_type_get_array_bounds(expr->resolved_kgpc_type, &start, NULL) == 0)
      return start;
  }

  /* Fall back to legacy field */
  return expr->array_lower_bound;
}

/* Helper to get array upper bound from expression, preferring
 * resolved_kgpc_type */
int expr_get_array_upper_bound(const struct Expression *expr) {
  if (expr == NULL)
    return -1;

  if (expr->resolved_kgpc_type != NULL &&
      kgpc_type_is_array(expr->resolved_kgpc_type)) {
    int end = -1;
    if (kgpc_type_get_array_bounds(expr->resolved_kgpc_type, NULL, &end) == 0)
      return end;
  }

  return expr->array_upper_bound;
}

static long long codegen_set_storage_size_for_high(long long high) {
  if (high < 32)
    return 4;
  if (high < 256)
    return 32;
  return (high + 7) / 8;
}

long long
codegen_set_storage_size_for_set_alias(const struct TypeAlias *alias) {
  if (alias == NULL || !alias->is_set)
    return 4;

  if (alias->storage_size > 0)
    return alias->storage_size;

  if (alias->set_element_type == CHAR_TYPE ||
      (alias->set_element_type_id != NULL &&
       (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
        pascal_identifier_equals(alias->set_element_type_id, "AnsiChar"))))
    return 32;

  if (alias->is_enum_set && alias->inline_enum_values != NULL) {
    int count = ListLength(alias->inline_enum_values);
    if (count > 0)
      return codegen_set_storage_size_for_high((long long)count - 1);
  }

  if (alias->range_known && alias->range_end >= alias->range_start) {
    long long count =
        (long long)alias->range_end - (long long)alias->range_start + 1;
    if (count > 0)
      return codegen_set_storage_size_for_high(count - 1);
  }

  return 4;
}

static int expr_is_large_set_type(KgpcType *type) {
  if (type == NULL)
    return 0;

  struct TypeAlias *alias = type->type_alias;
  if (alias != NULL && alias->is_set) {
    if (codegen_set_storage_size_for_set_alias(alias) > 4)
      return 1;
  }

  if (kgpc_type_is_set(type) && kgpc_type_sizeof(type) > 4)
    return 1;

  return 0;
}

/* Check if an expression represents a "large" set that requires memory-based
 * operations (> 4 bytes).  This includes char sets (32 bytes) and enum sets
 * whose element type has more than 32 members. */
int expr_is_char_set_ctx(const struct Expression *expr, CodeGenContext *ctx) {
  if (expr == NULL)
    return 0;

  KgpcType *expr_type = expr_get_kgpc_type(expr);
  if (expr_type != NULL) {
    if (expr_is_large_set_type(expr_type))
      return 1;
  }

  if (expr->type == EXPR_POINTER_DEREF && ctx != NULL && ctx->symtab != NULL) {
    const struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr != NULL) {
      KgpcType *deref_type = NULL;
      if (pointer_expr->resolved_kgpc_type != NULL &&
          kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type)) {
        deref_type = pointer_expr->resolved_kgpc_type->info.points_to;
      } else if (pointer_expr->type == EXPR_VAR_ID &&
                 pointer_expr->expr_data.id != NULL) {
        HashNode_t *ptr_node = NULL;
        if (FindSymbol(&ptr_node, ctx->symtab, pointer_expr->expr_data.id) !=
                0 &&
            ptr_node != NULL && ptr_node->type != NULL &&
            kgpc_type_is_pointer(ptr_node->type)) {
          deref_type = ptr_node->type->info.points_to;
        }
      } else {
        KgpcType *pointer_type = expr_get_kgpc_type(pointer_expr);
        if (pointer_type != NULL && kgpc_type_is_pointer(pointer_type))
          deref_type = pointer_type->info.points_to;
      }

      if (deref_type != NULL) {
        if (expr_is_large_set_type(deref_type))
          return 1;
      }
    }
  }

  /* Check if expression has a KgpcType with type_alias */
  if (expr->resolved_kgpc_type != NULL) {
    struct TypeAlias *alias = expr->resolved_kgpc_type->type_alias;
    if (alias != NULL && alias->is_set) {
      if (expr_is_large_set_type(expr->resolved_kgpc_type))
        return 1;
    }
    if (expr_is_large_set_type(expr->resolved_kgpc_type))
      return 1;
  }

  /* For variable references, look up the type in the symbol table */
  if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, expr->expr_data.id) && node != NULL) {
      if (node->type != NULL) {
        struct TypeAlias *alias = node->type->type_alias;
        if (alias != NULL && alias->is_set) {
          if (expr_is_large_set_type(node->type))
            return 1;
        }
        if (expr_is_large_set_type(node->type))
          return 1;
      }
      if (node->hash_type == HASHTYPE_CONST && node->const_set_value != NULL &&
          node->const_set_size > 4) {
        return 1;
      }
    }
  }

  /* For record field access, check if the field is a large set (> 4 bytes) */
  if (expr->type == EXPR_RECORD_ACCESS && ctx != NULL) {
    struct RecordField *field = codegen_expr_lookup_record_field(
        (struct Expression *)expr, (CodeGenContext *)ctx);
    if (field != NULL && field->type == SET_TYPE) {
      if (field->has_cached_layout && field->cached_size > 4)
        return 1;
      /* Fallback: check set_element_type_id in the symtab */
      if (field->set_element_type_id != NULL && ctx->symtab != NULL) {
        HashNode_t *elem_node = NULL;
        if (FindSymbol(&elem_node, ctx->symtab, field->set_element_type_id) !=
                0 &&
            elem_node != NULL && elem_node->type != NULL) {
          struct TypeAlias *elem_alias = elem_node->type->type_alias;
          if (elem_alias != NULL && elem_alias->is_enum &&
              elem_alias->enum_literals != NULL &&
              ListLength(elem_alias->enum_literals) > 32)
            return 1;
        }
      }
    }
  }

  /* For set literals, check if elements are characters or single-char strings
   */
  if (expr->type == EXPR_SET && expr->expr_data.set_data.elements != NULL) {
    ListNode_t *node = expr->expr_data.set_data.elements;
    while (node != NULL) {
      struct SetElement *element = (struct SetElement *)node->cur;
      if (element->lower != NULL) {
        int elem_type = expr_get_type_tag(element->lower);
        /* Character sets can have CHAR_TYPE or STRING_TYPE (single char)
         * elements */
        if (elem_type == CHAR_TYPE)
          return 1;
        if (elem_type == STRING_TYPE && element->lower->type == EXPR_STRING) {
          /* Single-character string literal */
          if (element->lower->expr_data.string != NULL &&
              strlen(element->lower->expr_data.string) == 1)
            return 1;
        }
        if (element->lower->type == EXPR_CHAR_CODE)
          return 1;
        if (element->lower->type == EXPR_STRING &&
            element->lower->expr_data.string != NULL &&
            strlen(element->lower->expr_data.string) == 1)
          return 1;
        /* Integer elements > 31 can't fit in a 32-bit register set;
           route to memory-based 32-byte set (e.g. [97..122] for 'a'..'z') */
        if (element->lower->type == EXPR_INUM &&
            element->lower->expr_data.i_num > 31)
          return 1;
        /* Enum-literal identifiers with ordinal > 31 also need the
           memory-based path. Resolve via symtab. */
        if (element->lower->type == EXPR_VAR_ID &&
            element->lower->expr_data.id != NULL && ctx != NULL &&
            ctx->symtab != NULL) {
          HashNode_t *lit_node = NULL;
          if (FindSymbol(&lit_node, ctx->symtab,
                         element->lower->expr_data.id) != 0 &&
              lit_node != NULL && lit_node->hash_type == HASHTYPE_CONST &&
              lit_node->is_constant && lit_node->const_int_value > 31)
            return 1;
        }
      }
      if (element->upper != NULL) {
        if (element->upper->type == EXPR_INUM &&
            element->upper->expr_data.i_num > 31)
          return 1;
        if (element->upper->type == EXPR_VAR_ID &&
            element->upper->expr_data.id != NULL && ctx != NULL &&
            ctx->symtab != NULL) {
          HashNode_t *lit_node = NULL;
          if (FindSymbol(&lit_node, ctx->symtab,
                         element->upper->expr_data.id) != 0 &&
              lit_node != NULL && lit_node->hash_type == HASHTYPE_CONST &&
              lit_node->is_constant && lit_node->const_int_value > 31)
            return 1;
        }
      }
      node = node->next;
    }
  }

  return 0;
}

/* Wrapper that doesn't need context - for backward compatibility */
int expr_is_char_set(const struct Expression *expr) {
  return expr_is_char_set_ctx(expr, NULL);
}

/* Helper to get array element size from expression, preferring
 * resolved_kgpc_type ctx parameter reserved for future use in computing complex
 * type sizes */
long long expr_get_array_element_size(const struct Expression *expr,
                                      CodeGenContext *ctx) {
  if (expr == NULL)
    return -1;

  int expects_array_metadata = 0;
  if (expr->is_array_expr || expr->type == EXPR_ARRAY_ACCESS ||
      expr->type == EXPR_ARRAY_LITERAL ||
      expr->array_element_type != UNKNOWN_TYPE ||
      expr->array_element_type_id != NULL) {
    expects_array_metadata = 1;
  }

  if (expr->array_element_type == ARRAY_OF_CONST_TYPE)
    return (long long)sizeof(kgpc_tvarrec);

  if (expr->type == EXPR_ARRAY_ACCESS && expr->resolved_kgpc_type != NULL) {
    /* If the result of this array access is itself an array, return that
     * array's element size (not the total sizeof the result array).
     * This ensures nested array stores use the correct write width. */
    if (kgpc_type_is_array(expr->resolved_kgpc_type)) {
      long long elem_size =
          kgpc_type_get_array_element_size(expr->resolved_kgpc_type);
      if (elem_size <= 0 && ctx != NULL && ctx->symtab != NULL) {
        KgpcType *et = kgpc_type_get_array_element_type_resolved(
            expr->resolved_kgpc_type, ctx->symtab);
        if (et != NULL)
          elem_size = kgpc_type_sizeof(et);
      }
      if (elem_size > 0)
        return elem_size;
    }
    long long result_size = kgpc_type_sizeof(expr->resolved_kgpc_type);
    if (result_size > 0)
      return result_size;
  }

  if (expr->array_element_size > 0) {
    long long tag_size = -1;
    if (expr->array_element_type != UNKNOWN_TYPE)
      tag_size = get_type_tag_size(expr->array_element_type);
    if (tag_size <= 0 || expr->array_element_size != tag_size ||
        (expr->array_element_type_id != NULL &&
         (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
          pascal_identifier_equals(expr->array_element_type_id,
                                   "UnicodeChar")))) {
      return expr->array_element_size;
    }
  }

  /* Prefer KgpcType if available */
  if (expr->resolved_kgpc_type != NULL &&
      (kgpc_type_is_array(expr->resolved_kgpc_type) ||
       kgpc_type_is_shortstring(expr->resolved_kgpc_type))) {
    expects_array_metadata = 1;
    if (kgpc_type_is_shortstring(expr->resolved_kgpc_type))
      return 1;
    long long size = kgpc_type_get_array_element_size(expr->resolved_kgpc_type);
    if (size > 0)
      return size;
    if (ctx != NULL && ctx->symtab != NULL) {
      KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
          expr->resolved_kgpc_type, ctx->symtab);
      if (elem_type != NULL) {
        size = kgpc_type_sizeof(elem_type);
        if (size > 0)
          return size;
      }
    }
  }

  if (expr->array_element_type != UNKNOWN_TYPE) {
    if (expr->array_element_size > 0) {
      long long tag_size = get_type_tag_size(expr->array_element_type);
      if (expr->array_element_size != tag_size ||
          (expr->array_element_type_id != NULL &&
           (pascal_identifier_equals(expr->array_element_type_id, "WideChar") ||
            pascal_identifier_equals(expr->array_element_type_id,
                                     "UnicodeChar")))) {
        return expr->array_element_size;
      }
    }
    if (expr->array_element_type == CHAR_TYPE && ctx != NULL &&
        ctx->symtab != NULL && expr->array_element_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, expr->array_element_type_id) !=
              0 &&
          type_node != NULL && type_node->type != NULL) {
        long long node_size = kgpc_type_sizeof(type_node->type);
        if (node_size > 0 &&
            node_size != get_type_tag_size(expr->array_element_type)) {
          return node_size;
        }
      }
    }
    long long tag_size = get_type_tag_size(expr->array_element_type);
    if (tag_size > 0)
      return tag_size;
  }

  if (ctx != NULL && ctx->symtab != NULL &&
      expr->array_element_type_id != NULL) {
    HashNode_t *type_node = NULL;
    if (FindSymbol(&type_node, ctx->symtab, expr->array_element_type_id) != 0 &&
        type_node != NULL && type_node->type != NULL) {
      long long node_size = kgpc_type_sizeof(type_node->type);
      if (node_size > 0)
        return node_size;
    }
  }

  if (expr->type == EXPR_VAR_ID && ctx != NULL && ctx->symtab != NULL &&
      expr->expr_data.id != NULL) {
    HashNode_t *var_node = NULL;
    if (FindSymbol(&var_node, ctx->symtab, expr->expr_data.id) != 0 &&
        var_node != NULL && var_node->type != NULL &&
        kgpc_type_is_array(var_node->type)) {
      expects_array_metadata = 1;
      KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
          var_node->type, ctx->symtab);
      if (elem_type != NULL) {
        long long elem_size = kgpc_type_sizeof(elem_type);
        if (elem_size > 0)
          return elem_size;
      }
    }
  }

  if (expr->type == EXPR_POINTER_DEREF) {
    const struct Expression *pointer_expr =
        expr->expr_data.pointer_deref_data.pointer_expr;
    if (pointer_expr != NULL && pointer_expr->resolved_kgpc_type != NULL &&
        kgpc_type_is_pointer(pointer_expr->resolved_kgpc_type)) {
      KgpcType *points_to = pointer_expr->resolved_kgpc_type->info.points_to;
      if (points_to != NULL) {
        if (kgpc_type_is_array(points_to)) {
          long long elem_size = kgpc_type_get_array_element_size(points_to);
          if (elem_size <= 0) {
            KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
                points_to, ctx != NULL ? ctx->symtab : NULL);
            if (elem_type != NULL)
              elem_size = kgpc_type_sizeof(elem_type);
          }
          if (elem_size > 0)
            return elem_size;
        } else {
          long long elem_size = kgpc_type_sizeof(points_to);
          if (elem_size > 0)
            return elem_size;
        }
      }
    }

    if (pointer_expr != NULL && pointer_expr->type == EXPR_VAR_ID &&
        ctx != NULL && ctx->symtab != NULL &&
        pointer_expr->expr_data.id != NULL) {
      HashNode_t *var_node = NULL;
      if (FindSymbol(&var_node, ctx->symtab, pointer_expr->expr_data.id) != 0 &&
          var_node != NULL && var_node->type != NULL) {
        if (kgpc_type_is_pointer(var_node->type)) {
          KgpcType *points_to = var_node->type->info.points_to;
          if (points_to != NULL) {
            if (kgpc_type_is_array(points_to)) {
              long long elem_size = kgpc_type_get_array_element_size(points_to);
              if (elem_size <= 0) {
                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
                    points_to, ctx->symtab);
                if (elem_type != NULL)
                  elem_size = kgpc_type_sizeof(elem_type);
              }
              if (elem_size > 0)
                return elem_size;
            } else {
              long long elem_size = kgpc_type_sizeof(points_to);
              if (elem_size > 0)
                return elem_size;
            }
          }
        }
        if (kgpc_type_is_array(var_node->type)) {
          long long elem_size =
              kgpc_type_get_array_element_size(var_node->type);
          if (elem_size <= 0) {
            KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
                var_node->type, ctx->symtab);
            if (elem_type != NULL)
              elem_size = kgpc_type_sizeof(elem_type);
          }
          if (elem_size > 0)
            return elem_size;
        }
      }
    }

    if (pointer_expr != NULL && pointer_expr->type == EXPR_VAR_ID &&
        ctx != NULL && pointer_expr->expr_data.id != NULL) {
      struct RecordField *with_field =
          codegen_lookup_with_field(ctx, pointer_expr->expr_data.id, NULL);
      if (with_field != NULL) {
        long long elem_size =
            codegen_array_elem_size_from_field(with_field, ctx);
        if (elem_size > 0)
          return elem_size;
      }
      if (with_field == NULL && ctx->symtab != NULL) {
        struct RecordField *unique_field = codegen_find_unique_record_field(
            ctx->symtab, pointer_expr->expr_data.id, NULL);
        if (unique_field != NULL) {
          long long elem_size =
              codegen_array_elem_size_from_field(unique_field, ctx);
          if (elem_size > 0)
            return elem_size;
        }
      }
    }

    const struct Expression *lookup_expr = pointer_expr;
    if (lookup_expr != NULL && lookup_expr->type == EXPR_TYPECAST &&
        lookup_expr->expr_data.typecast_data.expr != NULL) {
      lookup_expr = lookup_expr->expr_data.typecast_data.expr;
    }

    if (lookup_expr != NULL && lookup_expr->type == EXPR_RECORD_ACCESS &&
        ctx != NULL) {
      struct RecordField *field = codegen_expr_lookup_record_field(
          (struct Expression *)lookup_expr, ctx);
      if (field != NULL) {
        {
          long long elem_size = codegen_array_elem_size_from_field(field, ctx);
          if (elem_size > 0)
            return elem_size;
        }
      }
    }

    if (pointer_expr != NULL && pointer_expr->type == EXPR_TYPECAST &&
        ctx != NULL && ctx->symtab != NULL) {
      const char *target_id =
          pointer_expr->expr_data.typecast_data.target_type_id;
      if (target_id != NULL) {
        HashNode_t *type_node = NULL;
        if (FindSymbol(&type_node, ctx->symtab, target_id) != 0 &&
            type_node != NULL && type_node->type != NULL &&
            kgpc_type_is_pointer(type_node->type)) {
          KgpcType *points_to = type_node->type->info.points_to;
          if (points_to != NULL) {
            long long elem_size = kgpc_type_sizeof(points_to);
            if (elem_size > 0)
              return elem_size;
          }
        }
      }
    }

    if (expr->pointer_subtype != UNKNOWN_TYPE) {
      long long tag_size = get_type_tag_size(expr->pointer_subtype);
      if (tag_size > 0)
        return tag_size;
    }
    if (ctx != NULL && ctx->symtab != NULL &&
        expr->pointer_subtype_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, expr->pointer_subtype_id) != 0 &&
          type_node != NULL && type_node->type != NULL) {
        long long node_size = kgpc_type_sizeof(type_node->type);
        if (node_size > 0)
          return node_size;
      }
    }
  }

  if (expr->type == EXPR_ARRAY_ACCESS) {
    const struct Expression *base =
        expr->expr_data.array_access_data.array_expr;
    if (base != NULL) {
      KgpcType *base_type = expr_get_kgpc_type(base);
      int base_is_pointer_like =
          expr_has_type_tag(base, POINTER_TYPE) ||
          (base_type != NULL && kgpc_type_is_pointer(base_type));
      int base_is_string_like =
          is_string_type(expr_get_type_tag(base)) ||
          (base_type != NULL && kgpc_type_is_string(base_type));
      long long indexed_size = -1;
      if ((base_is_pointer_like || base_is_string_like) &&
          codegen_get_indexable_element_size((struct Expression *)base, ctx,
                                             &indexed_size) &&
          indexed_size > 0) {
        return indexed_size;
      }
      long long base_elem_size = expr_get_array_element_size(base, ctx);
      if (base_elem_size > 0)
        return base_elem_size;
    }
  }

  if (!expects_array_metadata)
    return -1;

  /* With-stack lookup: the base expression might be a variable from
   * an enclosing `with` block that semcheck didn't resolve. */
  if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL && ctx != NULL &&
      ctx->with_depth > 0) {
    struct RecordField *with_field =
        codegen_lookup_with_field(ctx, expr->expr_data.id, NULL);
    if (with_field != NULL) {
      long long elem_size = codegen_array_elem_size_from_field(with_field, ctx);
      if (elem_size > 0)
        return elem_size;
    }
  }

  /* For EXPR_ARRAY_ACCESS base in EXPR_VAR_ID, try with-stack on the inner base
   */
  if (expr->type == EXPR_ARRAY_ACCESS) {
    const struct Expression *inner_base =
        expr->expr_data.array_access_data.array_expr;
    if (inner_base != NULL && inner_base->type == EXPR_VAR_ID &&
        inner_base->expr_data.id != NULL && ctx != NULL &&
        ctx->with_depth > 0) {
      struct RecordField *with_field =
          codegen_lookup_with_field(ctx, inner_base->expr_data.id, NULL);
      if (with_field != NULL) {
        long long elem_size =
            codegen_array_elem_size_from_field(with_field, ctx);
        if (elem_size > 0)
          return elem_size;
      }
    }
  }

  /* Hard invariant: metadata gaps must be fixed at source, not silently
   * defaulted. */
  KGPC_COMPILER_HARD_ASSERT(expr->array_element_size > 0,
                            "unable to determine array element size "
                            "(expr_type=%d elem_tag=%d elem_type_id=%s)",
                            expr->type, expr->array_element_type,
                            expr->array_element_type_id != NULL
                                ? expr->array_element_type_id
                                : "<null>");
  return expr->array_element_size;
}

/* Check if expression is signed, working with KgpcType */
int expr_is_signed_kgpctype(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL)
    return kgpc_type_is_signed(type);

  int tag = expr_get_type_tag(expr);
  if (tag != UNKNOWN_TYPE)
    return codegen_type_is_signed(tag);

  return 0;
}

/* Check if expression uses qword, working with KgpcType */
int expr_uses_qword_kgpctype(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  if (expr_has_type_tag(expr, REAL_TYPE)) {
    long long eff_size = expr_effective_size_bytes(expr);
    if (eff_size == 4)
      return 0;
  }

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL)
    return kgpc_type_uses_qword(type);

  return 0;
}

/* Check if expression has a specific type tag, working with KgpcType */
int expr_has_type_tag(const struct Expression *expr, int type_tag) {
  if (expr == NULL)
    return (type_tag == UNKNOWN_TYPE);

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type != NULL)
    return kgpc_type_equals_tag(type, type_tag);

  return 0;
}

/* Detect expressions that evaluate to a char value at runtime even though
 * the semantic checker may have promoted their type tag to STRING_TYPE.
 * This covers string indexing (Result[L]) which loads a single byte. */
int codegen_expr_is_string_char_index(const struct Expression *expr) {
  if (expr == NULL)
    return 0;
  /* EXPR_ARRAY_ACCESS into a string yields a char */
  if (expr->type == EXPR_ARRAY_ACCESS &&
      expr->expr_data.array_access_data.array_expr != NULL) {
    struct Expression *base = expr->expr_data.array_access_data.array_expr;
    if (expr_has_type_tag(base, STRING_TYPE) ||
        expr_has_type_tag(base, SHORTSTRING_TYPE) ||
        (base->resolved_kgpc_type != NULL &&
         (kgpc_type_is_string(base->resolved_kgpc_type) ||
          kgpc_type_is_shortstring(base->resolved_kgpc_type))))
      return 1;
  }
  return 0;
}

int expr_is_char_pointer(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  KgpcType *type = expr_get_kgpc_type(expr);
  if (type == NULL || !kgpc_type_is_pointer(type))
    return 0;

  if (expr->pointer_subtype == CHAR_TYPE)
    return 1;
  if (expr->pointer_subtype_id != NULL) {
    if (pascal_identifier_equals(expr->pointer_subtype_id, "AnsiChar") ||
        pascal_identifier_equals(expr->pointer_subtype_id, "WideChar") ||
        pascal_identifier_equals(expr->pointer_subtype_id, "Char"))
      return 1;
  }

  if (type != NULL && kgpc_type_is_pointer(type)) {
    KgpcType *pointee = type->info.points_to;
    if (pointee != NULL && pointee->kind == TYPE_KIND_PRIMITIVE &&
        pointee->info.primitive_type_tag == CHAR_TYPE)
      return 1;
  }

  return 0;
}

long long codegen_expr_sret_size(const struct Expression *expr) {
  KgpcType *ret_type = NULL;
  KgpcType *type = NULL;
  long long size = 0;

  if (expr == NULL)
    return 0;

  if (expr->type == EXPR_FUNCTION_CALL) {
    ret_type = codegen_function_call_return_type_from_expr(expr);
    if (ret_type != NULL) {
      if (kgpc_type_is_shortstring(ret_type) ||
          (ret_type->type_alias != NULL &&
           ret_type->type_alias->is_shortstring)) {
        long long ret_size = kgpc_type_sizeof(ret_type);
        return ret_size > 0 ? ret_size : 256;
      }

      if (kgpc_type_is_record(ret_type) ||
          (ret_type->kind == TYPE_KIND_ARRAY &&
           !kgpc_type_is_dynamic_array(ret_type)) ||
          (ret_type->type_alias != NULL &&
           ret_type->type_alias->is_shortstring)) {
        long long ret_size = kgpc_type_sizeof(ret_type);
        if (ret_size > 0)
          return ret_size;
        /* ret_type says record/array but the cached size is absent
         * (has_cached_size == 0 on the RecordType).  For records,
         * fall through to the expr-tag path below which always has
         * a safe fallback of 16.  For static arrays / shortstring
         * aliases the size should always be known after semcheck,
         * so return 0 conservatively for those. */
        if (!kgpc_type_is_record(ret_type))
          return 0;
        /* Record with uncached size: drop through to outer checks. */
      } else {
        if (kgpc_type_is_extended(ret_type))
          return 10;
        return 0;
      }
    }
  }

  if (expr_has_type_tag(expr, RECORD_TYPE)) {
    KgpcType *record_type = expr_get_kgpc_type(expr);
    if (record_type != NULL) {
      long long size = kgpc_type_sizeof(record_type);
      if (size > 0)
        return size;
    }
    return 16;
  }

  /* Secondary fallback: if we dropped through from EXPR_FUNCTION_CALL with a
   * record ret_type but expr_has_type_tag returned false (e.g.
   * resolved_kgpc_type was set to the accessed field's type such as String,
   * masking the record return type), trust ret_type directly.  The
   * two-pointer-size estimate covers all records that use SRET on Windows x64;
   * codegen_sizeof_record_type will compute the exact size when it allocates
   * the sret slot. */
  if (ret_type != NULL && kgpc_type_is_record(ret_type))
    return 2 * CODEGEN_POINTER_SIZE_BYTES;

  /* ShortStrings are passed via SRET because they're small fixed-size arrays.
   * Use the actual sized-shortstring storage when type metadata is available.
   */
  if (expr_has_type_tag(expr, SHORTSTRING_TYPE)) {
    type = expr_get_kgpc_type(expr);
    if (type != NULL) {
      size = kgpc_type_sizeof(type);
      if (size > 0)
        return size;
    }
    return 256;
  }

  type = expr_get_kgpc_type(expr);
  if (type != NULL && type->kind == TYPE_KIND_ARRAY &&
      !kgpc_type_is_dynamic_array(type)) {
    long long size = kgpc_type_sizeof(type);
    return size > 0 ? size : 16;
  }

  /* Also check for shortstring type aliases */
  if (type != NULL && type->type_alias != NULL &&
      type->type_alias->is_shortstring) {
    long long size = kgpc_type_sizeof(type);
    return size > 0 ? size : 256;
  }

  /* Extended (10 bytes) is returned via hidden sret pointer, matching
   * the callee convention which copies the result through kgpc_move.
   * Only applies to function calls — variables are not sret. */
  if (type != NULL && kgpc_type_is_extended(type) &&
      expr->type == EXPR_FUNCTION_CALL)
    return 10;

  return 0;
}

int expr_returns_sret(const struct Expression *expr) {
  if (expr != NULL && expr->type == EXPR_FUNCTION_CALL) {
    /* Procvar calls take priority: a procvar returning a record keeps its
     * sret ABI even if builtin_call_lowering was set on the same expression
     * node (e.g. BUILTIN_CALL_STRPAS placed by WriteLn string handling). */
    if (expr->expr_data.function_call_data.is_procedural_var_call &&
        expr->expr_data.function_call_data.cached_procvar_sret_size > 8)
      return 1;
    if (expr->expr_data.function_call_data.builtin_call_lowering ==
        BUILTIN_CALL_STRPAS)
      return 0;
  }

  long long sret_size = codegen_expr_sret_size(expr);
  if (sret_size <= 0)
    return 0;
  if (expr != NULL && expr->type == EXPR_FUNCTION_CALL) {
    KgpcType *ret_type = codegen_function_call_return_type_from_expr(expr);
    if (ret_type != NULL && kgpc_type_is_shortstring(ret_type))
      return 1;
    if (ret_type != NULL && kgpc_type_is_extended(ret_type))
      return 1;
  }
  if (expr != NULL && expr_has_type_tag(expr, SHORTSTRING_TYPE))
    return 1;
  if (expr != NULL) {
    KgpcType *type = expr_get_kgpc_type(expr);
    if (type != NULL && type->type_alias != NULL &&
        type->type_alias->is_shortstring)
      return 1;
    if (type != NULL && kgpc_type_is_extended(type) &&
        expr->type == EXPR_FUNCTION_CALL)
      return 1;
  }
  return sret_size > 8;
}

void codegen_release_function_call_mangled_id(struct Expression *expr) {
  if (expr == NULL || expr->type != EXPR_FUNCTION_CALL)
    return;

  if (expr->expr_data.function_call_data.mangled_id != NULL) {
    free(expr->expr_data.function_call_data.mangled_id);
    expr->expr_data.function_call_data.mangled_id = NULL;
  }
}

int codegen_expr_is_signed(const struct Expression *expr) {
  return expr_is_signed_kgpctype(expr);
}

static inline const char *register_name_for_type(const Register_t *reg,
                                                 int type_tag) {
  if (reg == NULL)
    return NULL;
  return codegen_type_uses_qword(type_tag) ? reg->bit_64 : reg->bit_32;
}

static inline const char *
register_name_for_expr(const Register_t *reg, const struct Expression *expr) {
  if (expr == NULL)
    return register_name_for_type(reg, UNKNOWN_TYPE);
  /* Use KgpcType-based helper instead of converting to tag */
  return expr_uses_qword_kgpctype(expr) ? reg->bit_64 : reg->bit_32;
}

static inline int expression_uses_qword(const struct Expression *expr) {
  return expr_uses_qword_kgpctype(expr);
}

int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
                        struct RecordType *record_type, long long *size_out,
                        int depth);

int codegen_sizeof_hashnode(CodeGenContext *ctx, HashNode_t *node,
                            long long *size_out, int depth);

int codegen_expr_is_addressable(const struct Expression *expr) {
  if (expr == NULL)
    return 0;

  switch (expr->type) {
  case EXPR_VAR_ID:
  case EXPR_ARRAY_ACCESS:
  case EXPR_RECORD_ACCESS:
  case EXPR_POINTER_DEREF:
  case EXPR_ADDR:
  case EXPR_RECORD_CONSTRUCTOR:
    return 1;
  case EXPR_FUNCTION_CALL:
    /* Function-call expressions are addressable only when they are lowered
     * through a hidden sret return buffer. */
    return expr_returns_sret(expr);
  case EXPR_TYPECAST:
    if (expr->expr_data.typecast_data.expr != NULL) {
      /* Managed-string encoding conversions are NOT addressable: the cast
       * produces a freshly-allocated buffer in a different encoding, so the
       * caller has to materialise the value and stash it in a temp.  If we
       * report addressable here, the function-call argument codegen takes
       * the inner expression's address and skips the conversion entirely —
       * which is how `FileExists(UnicodeString(FileName))` ended up passing
       * raw ANSI bytes to GetFileAttributesW. */
      struct Expression *tc_inner = expr->expr_data.typecast_data.expr;
      const char *target_id = expr->expr_data.typecast_data.target_type_id;
      int target_is_wide =
          (target_id != NULL &&
           (pascal_identifier_equals(target_id, "UnicodeString") ||
            pascal_identifier_equals(target_id, "WideString")));
      int target_is_ansi =
          (target_id != NULL &&
           (pascal_identifier_equals(target_id, "AnsiString") ||
            pascal_identifier_equals(target_id, "RawByteString") ||
            pascal_identifier_equals(target_id, "string")));
      int inner_is_wide = codegen_expr_is_wide_string_value(tc_inner);
      int inner_is_ansi = !inner_is_wide &&
                          (expr_has_type_tag(tc_inner, STRING_TYPE) ||
                           tc_inner->type == EXPR_STRING);
      if ((target_is_wide && inner_is_ansi) ||
          (target_is_ansi && inner_is_wide))
        return 0;
      return codegen_expr_is_addressable(tc_inner);
    }
    return 0;
  case EXPR_AS:
    if (expr->expr_data.as_data.expr != NULL)
      return codegen_expr_is_addressable(expr->expr_data.as_data.expr);
    return 0;
  default:
    return 0;
  }
}

int codegen_sizeof_array_node(CodeGenContext *ctx, HashNode_t *node,
                              long long *size_out, int depth) {
  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT) {
    codegen_report_error(
        ctx, "ERROR: Type resolution exceeded supported recursion depth.");
    return 1;
  }

  /* Check if array is dynamic */
  int is_dynamic = hashnode_is_dynamic_array(node);

  if (is_dynamic) {
    codegen_report_error(ctx,
                         "ERROR: Unable to determine size of dynamic array %s.",
                         node->id != NULL ? node->id : "");
    return 1;
  }

  /* Get element size from KgpcType */
  long long element_size = hashnode_get_element_size(node);

  if (element_size <= 0) {
    struct TypeAlias *alias = codegen_get_type_alias_from_node(node);
    if (alias != NULL && alias->is_array) {
      if (codegen_sizeof_type(ctx, alias->array_element_type,
                              alias->array_element_type_id, NULL, &element_size,
                              depth + 1) != 0)
        return 1;
    } else if (codegen_node_is_record_type(node)) {
      struct RecordType *record_type = codegen_get_record_type_from_node(node);
      if (record_type != NULL &&
          codegen_sizeof_record(ctx, record_type, &element_size, depth + 1) !=
              0)
        return 1;
    } else {
      if (node->type == NULL) {
        codegen_report_error(ctx,
                             "ERROR: Unable to determine element size for "
                             "array %s (missing type info).",
                             node->id != NULL ? node->id : "");
        return 1;
      }

      long long base = kgpc_type_sizeof(node->type);
      if (base < 0) {
        codegen_report_error(
            ctx, "ERROR: Unable to determine element size for array %s.",
            node->id != NULL ? node->id : "");
        return 1;
      }
      element_size = base;
    }
  }

  /* Get array bounds from KgpcType if available */
  int array_start, array_end;
  hashnode_get_array_bounds(node, &array_start, &array_end);

  long long count = (long long)array_end - (long long)array_start + 1;
  if (count < 0) {
    codegen_report_error(
        ctx, "ERROR: Invalid bounds for array %s during size computation.",
        node->id != NULL ? node->id : "");
    return 1;
  }

  *size_out = element_size * count;
  return 0;
}

int codegen_sizeof_array_type_kgpc(CodeGenContext *ctx, KgpcType *type,
                                   long long *size_out) {
  if (size_out == NULL || type == NULL || !kgpc_type_is_array(type))
    return 1;

  if (ctx != NULL && ctx->symtab != NULL) {
    KgpcArrayDimensionInfo info;
    if (kgpc_type_get_array_dimension_info(type, ctx->symtab, &info) == 0 &&
        info.total_size >= 0) {
      *size_out = info.total_size;
      return 0;
    }
  }

  long long size = kgpc_type_sizeof(type);
  if (size > 0) {
    *size_out = size;
    return 0;
  }

  return 1;
}

int codegen_sizeof_named_array_alias(CodeGenContext *ctx,
                                     const struct TypeAlias *alias,
                                     long long *size_out) {
  if (ctx == NULL || ctx->symtab == NULL || alias == NULL ||
      alias->alias_name == NULL || size_out == NULL) {
    return 1;
  }

  HashNode_t *node = NULL;
  if (FindSymbol(&node, ctx->symtab, alias->alias_name) == 0 || node == NULL ||
      node->type == NULL || !kgpc_type_is_array(node->type)) {
    return 1;
  }

  return codegen_sizeof_array_type_kgpc(ctx, node->type, size_out);
}

static long long codegen_default_set_storage_size_for_high(long long high) {
  if (high < 32)
    return 4;
  if (high < 256)
    return 32;
  return (high + 7) / 8;
}

long long codegen_set_storage_size_from_alias(CodeGenContext *ctx,
                                              const struct TypeAlias *alias) {
  if (alias == NULL || !alias->is_set)
    return 4;

  if (alias->storage_size > 0)
    return alias->storage_size;

  if (alias->set_element_type == CHAR_TYPE ||
      alias->set_element_type == BYTE_TYPE ||
      (alias->set_element_type_id != NULL &&
       (pascal_identifier_equals(alias->set_element_type_id, "Char") ||
        pascal_identifier_equals(alias->set_element_type_id, "AnsiChar") ||
        pascal_identifier_equals(alias->set_element_type_id, "Byte"))))
    return 32;

  if (alias->is_enum_set && alias->inline_enum_values != NULL) {
    int count = ListLength(alias->inline_enum_values);
    if (count > 0)
      return codegen_default_set_storage_size_for_high((long long)count - 1);
  }

  if (alias->range_known)
    return codegen_default_set_storage_size_for_high(alias->range_end);

  if (alias->set_element_type_id != NULL && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *elem_node = NULL;
    if (FindSymbol(&elem_node, ctx->symtab, alias->set_element_type_id) != 0 &&
        elem_node != NULL) {
      struct TypeAlias *elem_alias =
          codegen_get_type_alias_from_node(elem_node);
      if (elem_alias != NULL) {
        if (elem_alias->is_enum && elem_alias->enum_literals != NULL) {
          int count = ListLength(elem_alias->enum_literals);
          if (count > 0)
            return codegen_default_set_storage_size_for_high((long long)count -
                                                             1);
        }
        if (elem_alias->range_known)
          return codegen_default_set_storage_size_for_high(
              elem_alias->range_end);
      }
    }
  }

  return 4;
}

int codegen_sizeof_type(CodeGenContext *ctx, int type_tag, const char *type_id,
                        struct RecordType *record_type, long long *size_out,
                        int depth) {
  if (size_out == NULL)
    return 1;

  if (depth > CODEGEN_SIZEOF_RECURSION_LIMIT) {
    codegen_report_error(
        ctx, "ERROR: Unable to determine type size due to excessive nesting.");
    return 1;
  }

  if (record_type != NULL)
    return codegen_sizeof_record(ctx, record_type, size_out, depth + 1);

  if (type_tag == RECORD_TYPE && type_id == NULL) {
    codegen_report_error(
        ctx,
        "ERROR: Unable to resolve anonymous record type for size computation.");
    return 1;
  }

  /* For procedure types, we must check if the named type alias is
   * "procedure of object" (TMethod) — those are 16-byte aggregates,
   * while plain procedure pointers are 8 bytes.  Look up the type_id
   * before falling back to the generic 8-byte sizing. */
  if (type_tag == PROCEDURE && type_id != NULL && ctx != NULL &&
      ctx->symtab != NULL) {
    HashNode_t *type_node = NULL;
    if (FindSymbol(&type_node, ctx->symtab, type_id) != 0 &&
        type_node != NULL && type_node->type != NULL &&
        kgpc_type_is_method_pointer(type_node->type)) {
      *size_out = 16;
      return 0;
    }
  }

  int can_resolve_type_id =
      (type_id != NULL && ctx != NULL && ctx->symtab != NULL);
  if (can_resolve_type_id) {
    HashNode_t *node = NULL;
    if (FindSymbol(&node, ctx->symtab, type_id) != 0 && node != NULL)
      return codegen_sizeof_hashnode(ctx, node, size_out, depth + 1);
  }

  if (type_tag != UNKNOWN_TYPE) {
    long long base = get_type_tag_size(type_tag);
    if (base >= 0) {
      /* Fixed-width scalar builtins (SmallInt, ShortInt, ...) carry a generic
       * INT_TYPE tag whose natural size (4) is wider than their true width.
       * The authoritative width lives on the named alias's storage_size.  When
       * the type_id names such a narrowing alias, honor it so packed records
       * (FPC's coffsymbol.section:smallint) get the correct 18-byte layout
       * here, matching SemCheck_sizeof.c.  This keeps the codegen and semantic
       * record-size caches consistent regardless of which path runs first.
       * Only narrowing is honored, so width-varying aliases like Integer or
       * LongInt (storage_size == tag size) are unaffected. */
      if (type_id != NULL && ctx != NULL && ctx->symtab != NULL) {
        HashNode_t *alias_node =
            semcheck_find_preferred_type_node(ctx->symtab, type_id);
        struct TypeAlias *alias =
            (alias_node != NULL) ? hashnode_get_type_alias(alias_node) : NULL;
        if (alias != NULL && alias->storage_size > 0 &&
            alias->storage_size < base && !alias->is_array && !alias->is_set &&
            !alias->is_enum && !alias->is_file && !alias->is_pointer)
          base = alias->storage_size;
      }
      *size_out = base;
      return 0;
    }
  }

  if (can_resolve_type_id) {
    codegen_report_error(
        ctx, "ERROR: Unable to resolve type %s for size computation.", type_id);
    return 1;
  }

  codegen_report_error(
      ctx, "ERROR: Unable to determine size for expression type %d.", type_tag);
  return 1;
}

/* Lookup the RecordField metadata for a record access expression */
struct RecordField *
codegen_expr_lookup_record_field(struct Expression *record_access_expr,
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

static struct RecordField *
codegen_find_unique_record_field(SymTab_t *symtab, const char *field_id,
                                 struct RecordType **out_record) {
  if (symtab == NULL || field_id == NULL)
    return NULL;

  struct RecordField *found_field = NULL;
  struct RecordType *found_record = NULL;

  HashTable_t *tables[2];
  tables[0] = symtab->builtin_scope->table;
  tables[1] = NULL;

  ScopeNode *scope = symtab->current_scope;
  while (scope != NULL) {
    tables[1] = scope->table;
    for (int t = 0; t < 2; ++t) {
      HashTable_t *table = tables[t];
      if (table == NULL)
        continue;
      for (int i = 0; i < TABLE_SIZE; ++i) {
        ListNode_t *node_list = table->table[i];
        while (node_list != NULL) {
          HashNode_t *node = (HashNode_t *)node_list->cur;
          if (node != NULL && node->hash_type == HASHTYPE_TYPE) {
            struct RecordType *record = codegen_get_record_type_from_node(node);
            if (record != NULL) {
              struct RecordField *field =
                  codegen_lookup_record_field(record, field_id);
              if (field != NULL) {
                if (found_field != NULL && found_record != record) {
                  if (field->is_pointer == found_field->is_pointer &&
                      field->is_array == found_field->is_array &&
                      field->pointer_type == found_field->pointer_type &&
                      ((field->pointer_type_id == NULL &&
                        found_field->pointer_type_id == NULL) ||
                       (field->pointer_type_id != NULL &&
                        found_field->pointer_type_id != NULL &&
                        pascal_identifier_equals(
                            field->pointer_type_id,
                            found_field->pointer_type_id))) &&
                      field->array_element_type ==
                          found_field->array_element_type &&
                      ((field->array_element_type_id == NULL &&
                        found_field->array_element_type_id == NULL) ||
                       (field->array_element_type_id != NULL &&
                        found_field->array_element_type_id != NULL &&
                        pascal_identifier_equals(
                            field->array_element_type_id,
                            found_field->array_element_type_id))) &&
                      field->array_element_record ==
                          found_field->array_element_record) {
                    found_field = field;
                    found_record = record;
                    break;
                  }
                  return NULL;
                }
                found_field = field;
                found_record = record;
              }
            }
          }
          node_list = node_list->next;
        }
      }
    }
    scope = scope->parent;
  }

  if (found_field != NULL && out_record != NULL)
    *out_record = found_record;
  return found_field;
}

struct RecordField *codegen_lookup_with_field(CodeGenContext *ctx,
                                              const char *field_id,
                                              struct RecordType **out_record) {
  if (ctx == NULL || field_id == NULL || ctx->with_depth <= 0)
    return NULL;
  for (int i = ctx->with_depth; i > 0; --i) {
    struct RecordType *record = ctx->with_stack[i - 1].record_type;
    struct RecordField *field = codegen_lookup_record_field(record, field_id);
    if (field != NULL) {
      if (out_record != NULL)
        *out_record = record;
      return field;
    }
  }
  return NULL;
}

long long codegen_array_elem_size_from_field(struct RecordField *field,
                                             CodeGenContext *ctx) {
  if (field == NULL)
    return -1;
  if (field->is_pointer) {
    if (field->pointer_type != UNKNOWN_TYPE) {
      long long tag_size = get_type_tag_size(field->pointer_type);
      if (tag_size > 0)
        return tag_size;
    }
    if (ctx != NULL && ctx->symtab != NULL && field->pointer_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, field->pointer_type_id) != 0 &&
          type_node != NULL && type_node->type != NULL) {
        KgpcType *points_to = NULL;
        if (kgpc_type_is_pointer(type_node->type))
          points_to = type_node->type->info.points_to;
        if (points_to != NULL) {
          long long elem_size = kgpc_type_sizeof(points_to);
          if (elem_size > 0)
            return elem_size;
        }
        if (kgpc_type_is_array(type_node->type)) {
          long long elem_size =
              kgpc_type_get_array_element_size(type_node->type);
          if (elem_size <= 0) {
            KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(
                type_node->type, ctx->symtab);
            if (elem_type != NULL)
              elem_size = kgpc_type_sizeof(elem_type);
          }
          if (elem_size > 0)
            return elem_size;
        }
        {
          long long node_size = kgpc_type_sizeof(type_node->type);
          if (node_size > 0)
            return node_size;
        }
      }
    }
  } else if (field->is_array) {
    if (field->array_element_type != UNKNOWN_TYPE) {
      long long tag_size = get_type_tag_size(field->array_element_type);
      if (tag_size > 0)
        return tag_size;
    }
    if (ctx != NULL && ctx->symtab != NULL &&
        field->array_element_type_id != NULL) {
      HashNode_t *type_node = NULL;
      if (FindSymbol(&type_node, ctx->symtab, field->array_element_type_id) !=
              0 &&
          type_node != NULL && type_node->type != NULL) {
        long long elem_size = kgpc_type_sizeof(type_node->type);
        if (elem_size > 0)
          return elem_size;
      }
    }
    if (field->array_element_record != NULL) {
      long long elem_size = 0;
      if (codegen_sizeof_record(ctx, field->array_element_record, &elem_size,
                                0) == 0 &&
          elem_size > 0)
        return elem_size;
    }
  }
  return -1;
}

/* Code generation for expressions */
const char *describe_expression_kind(const struct Expression *expr) {
  if (expr == NULL)
    return "unknown";

  switch (expr->type) {
  case EXPR_VAR_ID:
    return "variable reference";
  case EXPR_ARRAY_ACCESS:
    return "array access";
  case EXPR_FUNCTION_CALL:
    return "function call";
  case EXPR_ADDOP:
    return "additive expression";
  case EXPR_MULOP:
    return "multiplicative expression";
  case EXPR_SIGN_TERM:
    return "signed term";
  case EXPR_RELOP:
    return "relational expression";
  case EXPR_INUM:
    return "integer literal";
  case EXPR_RNUM:
    return "real literal";
  default:
    return "expression";
  }
}

Register_t *codegen_try_get_reg(ListNode_t **inst_list, CodeGenContext *ctx,
                                const char *usage) {
  Register_t *reg = get_free_reg(get_reg_stack(), inst_list);
  if (reg == NULL)
    reg = get_reg_with_spill(get_reg_stack(), inst_list);
  if (reg == NULL)
    codegen_report_error(ctx, "ERROR: Unable to allocate register for %s.",
                         usage);
  return reg;
}

ListNode_t *codegen_expr_tree_value(struct Expression *expr,
                                    ListNode_t *inst_list, CodeGenContext *ctx,
                                    Register_t **out_reg) {

  if (expr != NULL) {
    if (expr->type == EXPR_IS)
      return codegen_emit_is_expr(expr, inst_list, ctx, out_reg);
    if (expr->type == EXPR_TYPEINFO) {
      const char *type_id = expr->expr_data.typeinfo_data.type_id;
      if (type_id == NULL || type_id[0] == '\0') {
        codegen_report_error(ctx, "ERROR: TypeInfo missing type identifier.");
        if (out_reg != NULL)
          *out_reg = NULL;
        return inst_list;
      }

      Register_t *tmp_reg = codegen_try_get_reg(&inst_list, ctx, "typeinfo");
      if (tmp_reg == NULL) {
        if (out_reg != NULL)
          *out_reg = NULL;
        return inst_list;
      }

      char label[CODEGEN_MAX_INST_BUF];
      codegen_typeinfo_label_for_type_id(ctx->symtab, type_id, label,
                                         sizeof(label));
      {
        /* Integrated: emit through the target-neutral backend vtable
         * (byte-identical). */
        BeEmitter em = codegen_beemitter(inst_list, ctx);
        BeOperand dst = {OPK_VREG, BE_W64, {.vreg = tmp_reg}};
        BeOperand a = {OPK_RIP_SYM, BE_W64, {.sym = label}};
        kgpc_backend_target()->emit(&em, BE_LEA, BE_W64, &dst, &a, NULL);
        inst_list = em.list;
      }

      if (out_reg != NULL)
        *out_reg = tmp_reg;
      else
        free_reg(get_reg_stack(), tmp_reg);
      return inst_list;
    }
    if (expr->type == EXPR_ARRAY_LITERAL) {
      Register_t *tmp_reg = NULL;
      inst_list =
          codegen_materialize_array_literal(expr, inst_list, ctx, &tmp_reg);
      if (out_reg != NULL)
        *out_reg = tmp_reg;
      else if (tmp_reg != NULL)
        free_reg(get_reg_stack(), tmp_reg);
      return inst_list;
    }
    if (expr->type == EXPR_AS) {
      if (expr->expr_data.as_data.expr == NULL)
        return inst_list;

      if (codegen_expr_is_addressable(expr->expr_data.as_data.expr)) {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr,
                                             inst_list, ctx, &addr_reg);
        if (addr_reg == NULL)
          return inst_list;

        inst_list = codegen_emit_class_cast_check_from_address(expr, inst_list,
                                                               ctx, addr_reg);

        if (out_reg != NULL)
          *out_reg = addr_reg;
        else
          free_reg(get_reg_stack(), addr_reg);
        return inst_list;
      }

      if (codegen_expr_is_class_vmt_value(expr->expr_data.as_data.expr, ctx)) {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(expr->expr_data.as_data.expr,
                                             inst_list, ctx, &value_reg);
        if (value_reg == NULL || codegen_had_error(ctx))
          return inst_list;

        inst_list = codegen_emit_class_cast_check_from_class_vmt_ptr(
            expr, inst_list, ctx, value_reg);

        if (out_reg != NULL)
          *out_reg = value_reg;
        else
          free_reg(get_reg_stack(), value_reg);
        return inst_list;
      }

      if (codegen_expr_needs_class_method_vmt_self(expr->expr_data.as_data.expr,
                                                   ctx)) {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(expr->expr_data.as_data.expr,
                                             inst_list, ctx, &value_reg);
        if (value_reg == NULL || codegen_had_error(ctx))
          return inst_list;

        inst_list = codegen_emit_class_cast_check_from_instance_ptr(
            expr, inst_list, ctx, value_reg);

        if (out_reg != NULL)
          *out_reg = value_reg;
        else
          free_reg(get_reg_stack(), value_reg);
        return inst_list;
      }

      codegen_report_error(ctx, "ERROR: RTTI operations currently require "
                                "addressable class expressions.");
      return inst_list;
    }
  }

  /* ShortString → AnsiString/RawByteString typecast: build_expr_tree strips
   * EXPR_TYPECAST, so the conversion would be lost.  Detect it here and emit
   * a call to kgpc_shortstring_to_string before the generic tree path. */
  if (expr != NULL && expr->type == EXPR_TYPECAST &&
      expr->expr_data.typecast_data.expr != NULL) {
    int tc_target = expr->expr_data.typecast_data.target_type;
    struct Expression *tc_inner = expr->expr_data.typecast_data.expr;
    int inner_is_ss = codegen_expr_is_shortstring_value_ctx(tc_inner, ctx);
    if (inner_is_ss && tc_target == STRING_TYPE &&
        (!expr_has_type_tag(tc_inner, STRING_TYPE) ||
         expr_has_type_tag(tc_inner, SHORTSTRING_TYPE))) {
      /* Evaluate the inner ShortString expression to get a pointer to
       * the length-prefixed data, then convert it to a heap AnsiString. */
      Register_t *ss_reg = NULL;
      inst_list = codegen_expr_tree_value(tc_inner, inst_list, ctx, &ss_reg);
      if (ss_reg != NULL) {
        inst_list = codegen_promote_shortstring_reg(inst_list, ctx, ss_reg);
        if (out_reg != NULL)
          *out_reg = ss_reg;
        else
          free_reg(get_reg_stack(), ss_reg);
      } else if (out_reg != NULL) {
        *out_reg = NULL;
      }
      return inst_list;
    }

    /* Managed-string encoding typecasts: build_expr_tree strips EXPR_TYPECAST
     * so without this peek the conversion is lost and the call site receives
     * the inner expression's raw pointer with the wrong encoding.  This is
     * exactly what breaks FPC's Win RTL `FileExists(UnicodeString(FileName))`
     * — without conversion, GetFileAttributesW reads ANSI bytes as UTF-16. */
    int inner_is_wide = codegen_expr_is_wide_string_value(tc_inner);
    int target_is_wide =
        (expr->expr_data.typecast_data.target_type_id != NULL &&
         (pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "UnicodeString") ||
          pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "WideString")));
    int target_is_ansi_string =
        (tc_target == STRING_TYPE && !target_is_wide &&
         (expr->expr_data.typecast_data.target_type_id == NULL ||
          pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "AnsiString") ||
          pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "RawByteString") ||
          pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "string")));
    int inner_is_ansi_string =
        !inner_is_wide && !inner_is_ss &&
        (expr_has_type_tag(tc_inner, STRING_TYPE) ||
         tc_inner->type == EXPR_STRING);

    /* `string(p)` / `AnsiString(p)` where p is a PChar (^Char).  Without a
     * real conversion the typecast is a no-op and the raw C-string pointer
     * flows downstream as if it were a managed string.  Any later
     * ShortString consumer then reaches kgpc_string_to_shortstring, whose
     * C-string heuristic guesses a length from src[0] and silently drops the
     * first character whenever (unsigned char)src[0] == strlen-1 (e.g. a
     * 47-char name beginning with '.' = 46).  This is FPC's ogcoff.pas
     * Read_str (`secname := string(PChar(@FCoffStrs[..]))`); the dropped
     * leading '.' made a COFF section name miss the Win64 link-script glob
     * and raised Internal error 202102001 in the KGPC->FPC self-host.
     * Materialise a genuine managed AnsiString via StrPas so the value is
     * correctly length-tracked everywhere. */
    int inner_is_pchar = 0;
    {
      KgpcType *inner_kt = expr_get_kgpc_type(tc_inner);
      if (inner_kt != NULL && kgpc_type_is_pointer(inner_kt) &&
          inner_kt->info.points_to != NULL &&
          inner_kt->info.points_to->kind == TYPE_KIND_PRIMITIVE &&
          inner_kt->info.points_to->info.primitive_type_tag == CHAR_TYPE)
        inner_is_pchar = 1;
    }

    /* In the compiler's default $H- world `string` is a ShortString, so a
     * `string(p)` cast records SHORTSTRING_TYPE as its target.  Treat that the
     * same as the AnsiString case: convert the PChar to a genuine managed
     * string here.  The managed result then flows into the surrounding
     * ShortString assignment through kgpc_string_to_shortstring, which copies
     * managed sources verbatim (no C-string length heuristic), preserving the
     * leading character. */
    int target_is_shortstring_cast =
        (tc_target == SHORTSTRING_TYPE &&
         (expr->expr_data.typecast_data.target_type_id == NULL ||
          pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "string") ||
          pascal_identifier_equals(expr->expr_data.typecast_data.target_type_id,
                                   "ShortString")));

    const char *enc_helper = NULL;
    if (target_is_wide && inner_is_ansi_string)
      enc_helper = "kgpc_unicodestring_from_string";
    else if (target_is_ansi_string && inner_is_wide)
      enc_helper = "kgpc_string_from_unicodestring";
    else if ((target_is_ansi_string || target_is_shortstring_cast) &&
             inner_is_pchar)
      enc_helper = "kgpc_strpas_string";

    if (enc_helper != NULL) {
      Register_t *inner_reg = NULL;
      inst_list = codegen_expr_tree_value(tc_inner, inst_list, ctx, &inner_reg);
      if (inner_reg != NULL) {
        {
          /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64,
                           {.phys = codegen_target_is_windows() ? "%rcx"
                                                                : "%rdi"}};
          BeOperand src = {OPK_VREG, BE_W64, {.vreg = inner_reg}};
          kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(inst_list, enc_helper);
        free_arg_regs();
        {
          /* Integrated: emit through the target-neutral backend vtable (byte-identical). */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_VREG, BE_W64, {.vreg = inner_reg}};
          BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
          kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        if (out_reg != NULL)
          *out_reg = inner_reg;
        else
          free_reg(get_reg_stack(), inner_reg);
      } else if (out_reg != NULL) {
        *out_reg = NULL;
      }
      return inst_list;
    }
  }

  codegen_begin_expression(ctx);

  expr_node_t *expr_tree = build_expr_tree(expr);

  Register_t *target_reg =
      codegen_try_get_reg(&inst_list, ctx, describe_expression_kind(expr));
  if (target_reg == NULL) {

    free_expr_tree(expr_tree);

    if (out_reg != NULL)
      *out_reg = NULL;
    codegen_end_expression(ctx);
    return inst_list;
  }

  inst_list = gencode_expr_tree(expr_tree, inst_list, ctx, target_reg);

  free_expr_tree(expr_tree);

  if (out_reg != NULL) {
    *out_reg = target_reg;
    codegen_end_expression(ctx);
  } else {
    codegen_end_expression(ctx);
    free_reg(get_reg_stack(), target_reg);
  }

  return inst_list;
}

static ListNode_t *codegen_expr_via_tree(struct Expression *expr,
                                         ListNode_t *inst_list,
                                         CodeGenContext *ctx) {
  return codegen_expr_tree_value(expr, inst_list, ctx, NULL);
}

ListNode_t *codegen_sign_extend32_to64(ListNode_t *inst_list,
                                       const char *src_reg32,
                                       const char *dst_reg64) {
  assert(src_reg32 != NULL);
  assert(dst_reg64 != NULL);

  char buffer[CODEGEN_MAX_INST_BUF];
  snprintf(buffer, sizeof(buffer), "\tmovslq\t%s, %s\n", src_reg32, dst_reg64);
  return add_inst(inst_list, buffer);
}

ListNode_t *codegen_zero_extend32_to64(ListNode_t *inst_list,
                                       const char *src_reg32,
                                       const char *dst_reg32) {
  assert(src_reg32 != NULL);
  assert(dst_reg32 != NULL);

  char buffer[CODEGEN_MAX_INST_BUF];
  snprintf(buffer, sizeof(buffer), "\tmovl\t%s, %s\n", src_reg32, dst_reg32);
  return add_inst(inst_list, buffer);
}

int codegen_sizeof_type_reference(CodeGenContext *ctx, int type_tag,
                                  const char *type_id,
                                  struct RecordType *record_type,
                                  long long *size_out) {
  return codegen_sizeof_type(ctx, type_tag, type_id, record_type, size_out, 0);
}

ListNode_t *codegen_expr(struct Expression *expr, ListNode_t *inst_list,
                         CodeGenContext *ctx) {
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
#endif
  assert(expr != NULL);
  assert(ctx != NULL);
  CODEGEN_DEBUG("DEBUG: Generating code for expression type %d\n", expr->type);

  if (expr_has_type_tag(expr, SET_TYPE)) {
    Register_t *set_reg = NULL;
    inst_list = codegen_set_expr(expr, inst_list, ctx, &set_reg);
    if (codegen_had_error(ctx))
      return inst_list;
    if (set_reg != NULL)
      free_reg(get_reg_stack(), set_reg);
    return inst_list;
  }

  switch (expr->type) {
  case EXPR_VAR_ID:
    CODEGEN_DEBUG("DEBUG: Processing variable ID expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_RECORD_ACCESS:
    CODEGEN_DEBUG("DEBUG: Processing record access expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_ARRAY_ACCESS:
    CODEGEN_DEBUG("DEBUG: Processing array access expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_MULOP:
    CODEGEN_DEBUG("DEBUG: Processing mulop expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_INUM:
    CODEGEN_DEBUG("DEBUG: Processing integer constant expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_RNUM:
    CODEGEN_DEBUG("DEBUG: Processing real constant expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_BOOL:
    CODEGEN_DEBUG("DEBUG: Processing boolean constant expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_NIL:
    CODEGEN_DEBUG("DEBUG: Processing nil literal expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_SET:
    CODEGEN_DEBUG("DEBUG: Processing set literal expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_STRING:
    CODEGEN_DEBUG("DEBUG: Processing string literal expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_CHAR_CODE:
    CODEGEN_DEBUG("DEBUG: Processing character code literal expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_POINTER_DEREF:
    CODEGEN_DEBUG("DEBUG: Processing pointer dereference expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_RECORD_CONSTRUCTOR: {
    Register_t *addr_reg = NULL;
    inst_list = codegen_address_for_expr(expr, inst_list, ctx, &addr_reg);
    if (addr_reg != NULL)
      free_reg(get_reg_stack(), addr_reg);
    return inst_list;
  }
  case EXPR_ADDR:
    CODEGEN_DEBUG("DEBUG: Processing address-of expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_ADDR_OF_PROC:
    CODEGEN_DEBUG("DEBUG: Processing address-of-procedure expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_TYPEINFO:
    CODEGEN_DEBUG("DEBUG: Processing typeinfo expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_ARRAY_LITERAL:
    CODEGEN_DEBUG("DEBUG: Processing array literal expression\n");
    {
      Register_t *tmp_reg = NULL;
      inst_list =
          codegen_materialize_array_literal(expr, inst_list, ctx, &tmp_reg);
      if (tmp_reg != NULL)
        free_reg(get_reg_stack(), tmp_reg);
    }
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_RELOP:
    CODEGEN_DEBUG("DEBUG: Processing relational operator expression\n");
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return codegen_simple_relop(expr, inst_list, ctx, NULL);
  case EXPR_ADDOP:
    CODEGEN_DEBUG("DEBUG: Processing addop expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_SIGN_TERM:
    CODEGEN_DEBUG("DEBUG: Processing sign term expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_FUNCTION_CALL:
    CODEGEN_DEBUG("DEBUG: Processing function call expression\n");
    inst_list = codegen_expr_via_tree(expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_TYPECAST:
    CODEGEN_DEBUG("DEBUG: Processing typecast expression\n");
    if (expr->expr_data.typecast_data.expr != NULL)
      inst_list =
          codegen_expr(expr->expr_data.typecast_data.expr, inst_list, ctx);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_IS:
    CODEGEN_DEBUG("DEBUG: Processing RTTI is expression\n");
    inst_list = codegen_emit_is_expr(expr, inst_list, ctx, NULL);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  case EXPR_AS:
    CODEGEN_DEBUG("DEBUG: Processing RTTI as expression\n");
    if (expr->expr_data.as_data.expr != NULL) {
      if (codegen_expr_is_addressable(expr->expr_data.as_data.expr)) {
        Register_t *addr_reg = NULL;
        inst_list = codegen_address_for_expr(expr->expr_data.as_data.expr,
                                             inst_list, ctx, &addr_reg);
        if (addr_reg != NULL) {
          inst_list = codegen_emit_class_cast_check_from_address(
              expr, inst_list, ctx, addr_reg);
          free_reg(get_reg_stack(), addr_reg);
        }
      } else if (codegen_expr_is_class_vmt_value(expr->expr_data.as_data.expr,
                                                 ctx)) {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(expr->expr_data.as_data.expr,
                                             inst_list, ctx, &value_reg);
        if (value_reg != NULL) {
          inst_list = codegen_emit_class_cast_check_from_class_vmt_ptr(
              expr, inst_list, ctx, value_reg);
          free_reg(get_reg_stack(), value_reg);
        }
      } else if (codegen_expr_needs_class_method_vmt_self(
                     expr->expr_data.as_data.expr, ctx)) {
        Register_t *value_reg = NULL;
        inst_list = codegen_expr_with_result(expr->expr_data.as_data.expr,
                                             inst_list, ctx, &value_reg);
        if (value_reg != NULL) {
          inst_list = codegen_emit_class_cast_check_from_instance_ptr(
              expr, inst_list, ctx, value_reg);
          free_reg(get_reg_stack(), value_reg);
        }
      } else {
        codegen_report_error(ctx, "ERROR: RTTI operations currently require "
                                  "addressable class expressions.");
      }
    }
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
    return inst_list;
  default:
    assert(0 && "Unsupported expression type");
    return inst_list;
  }
}

static int codegen_expr_is_byref_var_id(const struct Expression *expr,
                                        CodeGenContext *ctx) {
  if (expr == NULL || ctx == NULL || ctx->symtab == NULL ||
      expr->type != EXPR_VAR_ID || expr->expr_data.id == NULL)
    return 0;

  HashNode_t *symbol = NULL;
  if (FindSymbol(&symbol, ctx->symtab, expr->expr_data.id) == 0 ||
      symbol == NULL)
    return 0;

  return symbol->is_var_parameter;
}

static const struct Expression *
codegen_unwrap_typecast_chain(const struct Expression *expr,
                              int *saw_extended_cast) {
  const struct Expression *cur = expr;
  if (saw_extended_cast != NULL)
    *saw_extended_cast = 0;

  while (cur != NULL && cur->type == EXPR_TYPECAST &&
         cur->expr_data.typecast_data.expr != NULL) {
    if (saw_extended_cast != NULL &&
        cur->expr_data.typecast_data.target_type == EXTENDED_TYPE) {
      *saw_extended_cast = 1;
    }
    cur = cur->expr_data.typecast_data.expr;
  }

  return cur;
}

ListNode_t *codegen_expr_with_result(struct Expression *expr,
                                     ListNode_t *inst_list, CodeGenContext *ctx,
                                     Register_t **out_reg) {
#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
#endif
  assert(out_reg != NULL);

  /* Handle set expressions specially - they need codegen_set_expr for proper
   * bitmask generation */
  if (expr != NULL && expr_has_type_tag(expr, SET_TYPE)) {
    inst_list = codegen_set_expr(expr, inst_list, ctx, out_reg);
#ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s (SET_TYPE path)\n", __func__);
#endif
    return inst_list;
  }

  if (expr != NULL && expr->type == EXPR_TYPECAST &&
      expr->expr_data.typecast_data.expr != NULL) {
    int target_tag = expr->expr_data.typecast_data.target_type;
    int saw_extended_cast = 0;
    const struct Expression *source_expr = codegen_unwrap_typecast_chain(
        expr->expr_data.typecast_data.expr, &saw_extended_cast);

    if (!codegen_expr_is_byref_var_id(source_expr, ctx))
      goto skip_byref_typecast_fastpath;

    if (target_tag == REAL_TYPE || target_tag == EXTENDED_TYPE) {
      Register_t *addr_reg = NULL;
      Register_t *result_reg = NULL;
      inst_list = codegen_address_for_expr((struct Expression *)source_expr,
                                           inst_list, ctx, &addr_reg);
      if (codegen_had_error(ctx) || addr_reg == NULL)
        return inst_list;

      result_reg = get_free_reg(get_reg_stack(), &inst_list);
      if (result_reg == NULL)
        result_reg = get_reg_with_spill(get_reg_stack(), &inst_list);
      if (result_reg == NULL) {
        free_reg(get_reg_stack(), addr_reg);
        return inst_list;
      }

      if (target_tag == EXTENDED_TYPE || saw_extended_cast) {
        {
          /* Integrated: emit through the target-neutral backend vtable
           * (byte-identical). */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_PHYS, BE_W64,
                           {.phys = codegen_target_is_windows() ? "%rcx"
                                                                : "%rdi"}};
          BeOperand src = {OPK_VREG, BE_W64, {.vreg = addr_reg}};
          kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
        inst_list = codegen_vect_reg(inst_list, 0);
        inst_list = codegen_call_with_shadow_space(
            inst_list, "kgpc_load_extended_to_bits");
        free_arg_regs();
        {
          /* Integrated: emit through the target-neutral backend vtable
           * (byte-identical). */
          BeEmitter em = codegen_beemitter(inst_list, ctx);
          BeOperand dst = {OPK_VREG, BE_W64, {.vreg = result_reg}};
          BeOperand src = {OPK_PHYS, BE_W64, {.phys = "%rax"}};
          kgpc_backend_target()->emit(&em, BE_MOV, BE_W64, &dst, &src, NULL);
          inst_list = em.list;
        }
      } else {
        long long real_size =
            expr_effective_size_bytes((struct Expression *)source_expr);
        if (real_size <= 0)
          real_size = expr_effective_size_bytes(expr);
        if (real_size <= 4) {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovss\t(%0), %xmm0\n");
          }
          inst_list = add_inst(inst_list, "\tcvtss2sd\t%xmm0, %xmm0\n");
        } else {
          {
            Register_t *u[] = {addr_reg};
            inst_list = add_inst_du(inst_list, ctx, NULL, 0, u, 1,
                                    "\tmovsd\t(%0), %xmm0\n");
          }
        }
        {
          Register_t *d[] = {result_reg};
          inst_list =
              add_inst_du(inst_list, ctx, d, 1, NULL, 0, "\tmovq\t%xmm0, %0\n");
        }
      }

      free_reg(get_reg_stack(), addr_reg);
      *out_reg = result_reg;
      return inst_list;
    }
  }
skip_byref_typecast_fastpath:

  inst_list = codegen_expr_tree_value(expr, inst_list, ctx, out_reg);

#ifdef DEBUG_CODEGEN
  CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
#endif
  return inst_list;
}

/* Code generation for non-local variable access */

/* Code generation for passing arguments */

/* Helper for codegen_get_nonlocal */
