#ifndef KGPC_IDENTIFIER_UTILS_H
#define KGPC_IDENTIFIER_UTILS_H

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

/* Case-insensitive strstr - finds needle in haystack ignoring case */
static inline const char *pascal_strcasestr(const char *haystack, const char *needle)
{
    if (haystack == NULL || needle == NULL)
        return NULL;
    
    if (needle[0] == '\0')
        return haystack;
    
    for (; *haystack != '\0'; ++haystack)
    {
        if (tolower((unsigned char)*haystack) == tolower((unsigned char)*needle))
        {
            /* Potential match, check the rest */
            const char *h = haystack;
            const char *n = needle;
            while (*n != '\0' && *h != '\0')
            {
                if (tolower((unsigned char)*h) != tolower((unsigned char)*n))
                    break;
                ++h;
                ++n;
            }
            if (*n == '\0')
                return haystack;
        }
    }
    
    return NULL;
}

#endif /* KGPC_IDENTIFIER_UTILS_H */
