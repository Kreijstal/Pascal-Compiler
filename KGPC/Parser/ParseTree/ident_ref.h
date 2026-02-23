#ifndef KGPC_IDENT_REF_H
#define KGPC_IDENT_REF_H

#include <stddef.h>

typedef struct QualifiedIdent
{
    char **segments;
    int count;
} QualifiedIdent;

typedef struct TypeRef
{
    QualifiedIdent *name;
    struct TypeRef **generic_args;
    int num_generic_args;
    int is_class_reference; /* For "class of T" */
} TypeRef;

QualifiedIdent *qualified_ident_from_single(const char *segment);
QualifiedIdent *qualified_ident_from_segments(char **segments, int count, int take_ownership);
QualifiedIdent *qualified_ident_clone(const QualifiedIdent *src);
void qualified_ident_free(QualifiedIdent *id);
const char *qualified_ident_last(const QualifiedIdent *id);
int qualified_ident_equals_ci(const QualifiedIdent *lhs, const QualifiedIdent *rhs);
char *qualified_ident_join(const QualifiedIdent *id, const char *sep);

TypeRef *type_ref_create(QualifiedIdent *name, TypeRef **args, int num_args);
TypeRef *type_ref_clone(const TypeRef *src);
void type_ref_free(TypeRef *ref);
char *type_ref_render_mangled(const TypeRef *ref);
char *type_ref_render_source(const TypeRef *ref);
const char *type_ref_base_name(const TypeRef *ref);

#endif /* KGPC_IDENT_REF_H */
