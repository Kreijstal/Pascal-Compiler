#ifndef GPC_RUNTIME_INTERNAL_H
#define GPC_RUNTIME_INTERNAL_H

#include <stddef.h>

#if !defined(_WIN32)
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__CYGWIN__)
#define GPC_HAVE_GETDOMAINNAME 1
#endif
#endif

char *gpc_alloc_empty_string(void);
char *gpc_string_duplicate(const char *value);

#endif /* GPC_RUNTIME_INTERNAL_H */
