#include "runtime_fpc_pchar_to_shortstr.inc"

/* FPC_PCHAR_TO_SHORTSTR — uppercase alias used by FPC's compilerproc convention.
 * In a separate .o from the lowercase fpc_pchar_to_shortstr so the linker
 * only pulls this when the symbol is otherwise undefined.  When the FPC RTL
 * is compiled, the compiler emits its own FPC_PCHAR_TO_SHORTSTR and this
 * archive member is never linked. */
void FPC_PCHAR_TO_SHORTSTR(char *res, const char *p)
{
    fpc_pchar_to_shortstr_internal(res, p);
}
