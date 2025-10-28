#ifndef GPC_IDENTIFIER_UTILS_H
#define GPC_IDENTIFIER_UTILS_H

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static inline char *pascal_identifier_lower_dup(const char *src)
{
    if (src == NULL)
        return NULL;

    size_t len = strlen(src);
    char *dst = (char *)malloc(len + 1);
    if (dst == NULL)
        return NULL;

    for (size_t i = 0; i < len; ++i)
        dst[i] = (char)tolower((unsigned char)src[i]);

    dst[len] = '\0';
    return dst;
}

static inline int pascal_identifier_equals(const char *lhs, const char *rhs)
{
    if (lhs == NULL || rhs == NULL)
        return lhs == rhs;

    while (*lhs != '\0' && *rhs != '\0')
    {
        unsigned char cl = (unsigned char)*lhs;
        unsigned char cr = (unsigned char)*rhs;
        if (tolower(cl) != tolower(cr))
            return 0;
        ++lhs;
        ++rhs;
    }

    return *lhs == '\0' && *rhs == '\0';
}

#endif /* GPC_IDENTIFIER_UTILS_H */
