#include "runtime_fpc_pchar_to_shortstr.inc"

/* Convert null-terminated C string (PAnsiChar) to FPC ShortString.
 * This provides the lowercase-mangled name used by KGPC's system.p.
 * The uppercase FPC_PCHAR_TO_SHORTSTR is in a separate .o file
 * (runtime_fpc_pchar_to_shortstr_upper.c) so the linker only pulls
 * it when not already defined by compiler-emitted FPC Pascal code. */
void fpc_pchar_to_shortstr(char *res, const char *p)
{
    fpc_pchar_to_shortstr_internal(res, p);
}
