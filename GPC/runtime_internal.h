#ifndef GPC_RUNTIME_INTERNAL_H
#define GPC_RUNTIME_INTERNAL_H

#include <stddef.h>

#if !defined(_WIN32)
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__CYGWIN__)
#define GPC_HAVE_GETDOMAINNAME 1
#endif
#endif

typedef struct gpc_class_typeinfo
{
    const struct gpc_class_typeinfo *parent;
    const char *class_name;
    const void *vmt;
} gpc_class_typeinfo;

char *gpc_alloc_empty_string(void);
char *gpc_string_duplicate(const char *value);
int gpc_rtti_is(const gpc_class_typeinfo *value_type,
    const gpc_class_typeinfo *target_type);
void gpc_rtti_check_cast(const gpc_class_typeinfo *value_type,
    const gpc_class_typeinfo *target_type);

#endif /* GPC_RUNTIME_INTERNAL_H */
