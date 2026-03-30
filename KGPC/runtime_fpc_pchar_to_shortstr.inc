#include <stddef.h>
#include <string.h>

static inline void fpc_pchar_to_shortstr_internal(char *res, const char *p)
{
    if (res == NULL)
        return;
    if (p == NULL)
    {
        res[0] = 0;
        return;
    }
    size_t len = strlen(p);
    if (len > 255)
        len = 255;
    res[0] = (char)len;
    if (len > 0)
        memcpy(res + 1, p, len);
}
