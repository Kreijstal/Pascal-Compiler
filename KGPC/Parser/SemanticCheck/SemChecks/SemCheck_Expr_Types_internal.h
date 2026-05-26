#ifndef SEMCHECK_EXPR_TYPES_INTERNAL_H
#define SEMCHECK_EXPR_TYPES_INTERNAL_H

#include "SemCheck_Expr_Internal.h"

char *build_qualified_identifier_from_expr_local(struct Expression *expr);
int semcheck_has_value_ident(SymTab_t *symtab, const char *id);
KgpcType *semcheck_create_value_kgpc_type_for_record_local(
    struct RecordType *record_info);
KgpcType *
semcheck_create_value_kgpc_type_from_node_local(HashNode_t *type_node);
int semcheck_type_alias_has_enum_literal(const struct TypeAlias *alias,
                                         const char *field_id);
HashNode_t *
semcheck_find_exact_qualified_type_node(SymTab_t *symtab,
                                        const QualifiedIdent *type_ref);
HashNode_t *semcheck_find_exact_type_node_for_ref(SymTab_t *symtab,
                                                  const TypeRef *type_ref,
                                                  const char *type_id,
                                                  const char *field_id);
int semcheck_try_resolve_enum_literal_from_type_alias(
    SymTab_t *symtab, const struct TypeAlias *type_alias, const char *field_id,
    long long *out_value);
HashNode_t *semcheck_find_visible_enum_type_candidate_with_literal(
    SymTab_t *symtab, const char *type_name, const char *field_id,
    long long *out_value);

#endif
