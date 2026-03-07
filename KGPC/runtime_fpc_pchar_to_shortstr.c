#include <stddef.h>
#include <string.h>

/* Convert null-terminated C string (PAnsiChar) to FPC ShortString.
 * KGPC name-mangles this as fpc_pchar_to_shortstr (lowercase). */
void fpc_pchar_to_shortstr(char *res, const char *p)
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
