#ifndef KGPC_IDENT_REF_H
#define KGPC_IDENT_REF_H

#include <stddef.h>

typedef struct QualifiedIdent {
  char **segments;
  int count;
} QualifiedIdent;

typedef struct TypeRef {
  QualifiedIdent *name;
  struct TypeRef **generic_args;
  int num_generic_args;
  int is_class_reference; /* For "class of T" */
  /* Intrusive links into the live-allocation list consumed by
   * type_ref_cleanup_remaining().  Embedding the linkage in the node makes
   * untracking on free O(1) rather than an O(n) scan of a side list, which
   * otherwise turned destroying a large parse tree into O(n^2) work.  These
   * are runtime-only bookkeeping and are never serialized. */
  struct TypeRef *live_prev;
  struct TypeRef *live_next;
} TypeRef;

QualifiedIdent *qualified_ident_from_single(const char *segment);
QualifiedIdent *qualified_ident_from_dotted(const char *name);
QualifiedIdent *qualified_ident_from_segments(char **segments, int count,
                                              int take_ownership);
QualifiedIdent *qualified_ident_clone(const QualifiedIdent *src);
void qualified_ident_free(QualifiedIdent *id);
const char *qualified_ident_last(const QualifiedIdent *id);
int qualified_ident_equals_ci(const QualifiedIdent *lhs,
                              const QualifiedIdent *rhs);
char *qualified_ident_join(const QualifiedIdent *id, const char *sep);

TypeRef *type_ref_create(QualifiedIdent *name, TypeRef **args, int num_args);
TypeRef *type_ref_clone(const TypeRef *src);
void type_ref_free(TypeRef *ref);
void type_ref_cleanup_remaining(void);
char *type_ref_render_mangled(const TypeRef *ref);
char *type_ref_render_source(const TypeRef *ref);
const char *type_ref_base_name(const TypeRef *ref);

/* Pascal case-insensitive structural equality on TypeRef.  Both NULL match.
 * Compares qualified name (case-insensitive), is_class_reference flag, and
 * recursively compares generic argument lists. */
int type_ref_equal_ci(const TypeRef *lhs, const TypeRef *rhs);

/* Same comparison applied element-wise to two parallel TypeRef arrays.
 * NULL arrays with count 0 are treated as equal. */
int type_ref_array_equal_ci(TypeRef *const *lhs, int lhs_count,
                            TypeRef *const *rhs, int rhs_count);

#endif /* KGPC_IDENT_REF_H */
