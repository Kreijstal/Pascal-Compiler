#include <stddef.h>
#include <string.h>

/* Convert null-terminated C string (PAnsiChar) to FPC ShortString.
 * This provides the lowercase-mangled name used by KGPC's system.p.
 * The uppercase FPC_PCHAR_TO_SHORTSTR is in a separate .o file
 * (runtime_fpc_pchar_to_shortstr_upper.c) so the linker only pulls
 * it when not already defined by compiler-emitted FPC Pascal code. */
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
