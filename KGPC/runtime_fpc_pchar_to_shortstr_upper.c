#include <stddef.h>
#include <string.h>

/* FPC_PCHAR_TO_SHORTSTR — uppercase alias used by FPC's compilerproc convention.
 * In a separate .o from the lowercase fpc_pchar_to_shortstr so the linker
 * only pulls this when the symbol is otherwise undefined.  When the FPC RTL
 * is compiled, the compiler emits its own FPC_PCHAR_TO_SHORTSTR and this
 * archive member is never linked. */
void FPC_PCHAR_TO_SHORTSTR(char *res, const char *p)
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
