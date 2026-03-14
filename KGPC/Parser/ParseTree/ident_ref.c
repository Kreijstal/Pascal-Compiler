#include "ident_ref.h"
#include "../../identifier_utils.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static char *dup_trimmed_ident_segment(const char *segment)
{
    if (segment == NULL)
        return NULL;

    const unsigned char *start = (const unsigned char *)segment;
    while (*start != '\0' && isspace(*start))
        ++start;

    const unsigned char *end = start + strlen((const char *)start);
    while (end > start && isspace(*(end - 1)))
        --end;

    size_t len = (size_t)(end - start);
    if (len == 0)
        return NULL;

    char *out = (char *)malloc(len + 1);
    if (out == NULL)
        return NULL;
    memcpy(out, start, len);
    out[len] = '\0';
    return out;
}

QualifiedIdent *qualified_ident_from_single(const char *segment)
{
    if (segment == NULL)
        return NULL;
    char **segments = (char **)calloc(1, sizeof(char *));
    if (segments == NULL)
        return NULL;
    segments[0] = strdup(segment);
    if (segments[0] == NULL)
    {
        free(segments);
        return NULL;
    }
    QualifiedIdent *id = (QualifiedIdent *)calloc(1, sizeof(QualifiedIdent));
    if (id == NULL)
    {
        free(segments[0]);
        free(segments);
        return NULL;
    }
    id->segments = segments;
    id->count = 1;
    return id;
}

QualifiedIdent *qualified_ident_from_dotted(const char *name)
{
    if (name == NULL || name[0] == '\0')
        return NULL;

    int count = 1;
    for (const char *p = name; *p != '\0'; ++p)
    {
        if (*p == '.')
            ++count;
    }

    char **segments = (char **)calloc((size_t)count, sizeof(char *));
    if (segments == NULL)
        return NULL;

    int idx = 0;
    const char *seg_start = name;
    for (const char *p = name; ; ++p)
    {
        if (*p == '.' || *p == '\0')
        {
            size_t len = (size_t)(p - seg_start);
            if (len == 0)
                goto fail;
            char *raw = (char *)malloc(len + 1);
            if (raw == NULL)
                goto fail;
            memcpy(raw, seg_start, len);
            raw[len] = '\0';
            segments[idx] = dup_trimmed_ident_segment(raw);
            free(raw);
            if (segments[idx] == NULL)
                goto fail;
            ++idx;
            if (*p == '\0')
                break;
            seg_start = p + 1;
        }
    }

    return qualified_ident_from_segments(segments, count, 1);

fail:
    if (segments != NULL)
    {
        for (int i = 0; i < count; ++i)
            free(segments[i]);
        free(segments);
    }
    return NULL;
}

QualifiedIdent *qualified_ident_from_segments(char **segments, int count, int take_ownership)
{
    if (segments == NULL || count <= 0)
        return NULL;
    QualifiedIdent *id = (QualifiedIdent *)calloc(1, sizeof(QualifiedIdent));
    if (id == NULL)
        return NULL;
    if (take_ownership)
    {
        id->segments = (char **)calloc((size_t)count, sizeof(char *));
        if (id->segments == NULL)
        {
            free(id);
            return NULL;
        }
        id->count = count;
        for (int i = 0; i < count; ++i)
        {
            id->segments[i] = dup_trimmed_ident_segment(segments[i]);
            if (segments[i] != NULL)
                free(segments[i]);
            if (segments[i] != NULL && id->segments[i] == NULL)
            {
                qualified_ident_free(id);
                free(segments);
                return NULL;
            }
        }
        free(segments);
        return id;
    }
    id->segments = (char **)calloc((size_t)count, sizeof(char *));
    if (id->segments == NULL)
    {
        free(id);
        return NULL;
    }
    id->count = count;
    for (int i = 0; i < count; ++i)
    {
        id->segments[i] = dup_trimmed_ident_segment(segments[i]);
        if (segments[i] != NULL && id->segments[i] == NULL)
        {
            qualified_ident_free(id);
            return NULL;
        }
    }
    return id;
}

QualifiedIdent *qualified_ident_clone(const QualifiedIdent *src)
{
    if (src == NULL || src->count <= 0 || src->segments == NULL)
        return NULL;
    return qualified_ident_from_segments(src->segments, src->count, 0);
}

void qualified_ident_free(QualifiedIdent *id)
{
    if (id == NULL)
        return;
    if (id->segments != NULL)
    {
        for (int i = 0; i < id->count; ++i)
            free(id->segments[i]);
        free(id->segments);
    }
    free(id);
}

const char *qualified_ident_last(const QualifiedIdent *id)
{
    if (id == NULL || id->segments == NULL || id->count <= 0)
        return NULL;
    return id->segments[id->count - 1];
}

int qualified_ident_equals_ci(const QualifiedIdent *lhs, const QualifiedIdent *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return lhs == rhs;
    if (lhs->count != rhs->count)
        return 0;
    for (int i = 0; i < lhs->count; ++i)
    {
        const char *l = lhs->segments[i];
        const char *r = rhs->segments[i];
        if (!pascal_identifier_equals(l, r))
            return 0;
    }
    return 1;
}

char *qualified_ident_join(const QualifiedIdent *id, const char *sep)
{
    if (id == NULL || id->segments == NULL || id->count <= 0)
        return NULL;
    if (sep == NULL)
        sep = ".";
    size_t sep_len = strlen(sep);
    size_t total = 1;
    for (int i = 0; i < id->count; ++i)
    {
        if (id->segments[i] != NULL)
            total += strlen(id->segments[i]);
        if (i + 1 < id->count)
            total += sep_len;
    }
    char *out = (char *)malloc(total);
    if (out == NULL)
        return NULL;
    out[0] = '\0';
    for (int i = 0; i < id->count; ++i)
    {
        if (id->segments[i] != NULL)
            strcat(out, id->segments[i]);
        if (i + 1 < id->count)
            strcat(out, sep);
    }
    return out;
}

TypeRef *type_ref_create(QualifiedIdent *name, TypeRef **args, int num_args)
{
    if (name == NULL)
        return NULL;
    TypeRef *ref = (TypeRef *)calloc(1, sizeof(TypeRef));
    if (ref == NULL)
        return NULL;
    ref->name = name;
    ref->generic_args = args;
    ref->num_generic_args = num_args;
    ref->is_class_reference = 0;
    return ref;
}

TypeRef *type_ref_clone(const TypeRef *src)
{
    if (src == NULL)
        return NULL;
    QualifiedIdent *name = qualified_ident_clone(src->name);
    TypeRef **args = NULL;
    int num_args = src->num_generic_args;
    if (num_args > 0)
    {
        args = (TypeRef **)calloc((size_t)num_args, sizeof(TypeRef *));
        if (args == NULL)
        {
            qualified_ident_free(name);
            return NULL;
        }
        for (int i = 0; i < num_args; ++i)
            args[i] = type_ref_clone(src->generic_args[i]);
    }
    TypeRef *out = type_ref_create(name, args, num_args);
    if (out != NULL)
        out->is_class_reference = src->is_class_reference;
    return out;
}

void type_ref_free(TypeRef *ref)
{
    if (ref == NULL)
        return;
    qualified_ident_free(ref->name);
    if (ref->generic_args != NULL)
    {
        for (int i = 0; i < ref->num_generic_args; ++i)
            type_ref_free(ref->generic_args[i]);
        free(ref->generic_args);
    }
    free(ref);
}

const char *type_ref_base_name(const TypeRef *ref)
{
    if (ref == NULL)
        return NULL;
    return qualified_ident_last(ref->name);
}

static char *type_ref_render_joined(const TypeRef *ref, const char *sep, const char *arg_sep)
{
    if (ref == NULL || ref->name == NULL)
        return NULL;
    char *base = qualified_ident_join(ref->name, sep);
    if (base == NULL)
        return NULL;
    if (ref->num_generic_args <= 0)
        return base;
    size_t total = strlen(base) + 1;
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_joined(ref->generic_args[i], sep, arg_sep);
        if (arg != NULL)
        {
            total += strlen(arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            total += strlen(arg_sep);
    }
    char *out = (char *)malloc(total + 1);
    if (out == NULL)
    {
        free(base);
        return NULL;
    }
    out[0] = '\0';
    strcat(out, base);
    free(base);
    strcat(out, "$");
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_joined(ref->generic_args[i], sep, arg_sep);
        if (arg != NULL)
        {
            strcat(out, arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            strcat(out, arg_sep);
    }
    return out;
}

char *type_ref_render_mangled(const TypeRef *ref)
{
    return type_ref_render_joined(ref, ".", "$");
}

char *type_ref_render_source(const TypeRef *ref)
{
    if (ref == NULL || ref->name == NULL)
        return NULL;
    char *base = qualified_ident_join(ref->name, ".");
    if (base == NULL)
        return NULL;
    if (ref->num_generic_args <= 0)
        return base;
    size_t total = strlen(base) + 2;
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_source(ref->generic_args[i]);
        if (arg != NULL)
        {
            total += strlen(arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            total += 2;
    }
    char *out = (char *)malloc(total + 1);
    if (out == NULL)
    {
        free(base);
        return NULL;
    }
    out[0] = '\0';
    strcat(out, base);
    free(base);
    strcat(out, "<");
    for (int i = 0; i < ref->num_generic_args; ++i)
    {
        char *arg = type_ref_render_source(ref->generic_args[i]);
        if (arg != NULL)
        {
            strcat(out, arg);
            free(arg);
        }
        if (i + 1 < ref->num_generic_args)
            strcat(out, ", ");
    }
    strcat(out, ">");
    return out;
}
