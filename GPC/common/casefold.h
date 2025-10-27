#ifndef GPC_COMMON_CASEFOLD_H
#define GPC_COMMON_CASEFOLD_H

#include <ctype.h>

static inline int gpc_casefold_char(int ch)
{
    return tolower((unsigned char)ch);
}

static inline int gpc_identifier_cmp(const char *lhs, const char *rhs)
{
    if (lhs == NULL && rhs == NULL)
        return 0;
    if (lhs == NULL)
        return -1;
    if (rhs == NULL)
        return 1;

    while (*lhs != '\0' && *rhs != '\0')
    {
        int diff = gpc_casefold_char(*lhs) - gpc_casefold_char(*rhs);
        if (diff != 0)
            return diff;
        ++lhs;
        ++rhs;
    }

    return gpc_casefold_char(*lhs) - gpc_casefold_char(*rhs);
}

static inline int gpc_identifier_equals(const char *lhs, const char *rhs)
{
    return gpc_identifier_cmp(lhs, rhs) == 0;
}

#endif /* GPC_COMMON_CASEFOLD_H */
