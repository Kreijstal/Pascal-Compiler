/* ------------------------------------------------------------------ */
/* Default definitions of the FPC entry-information / threading /      */
/* widestring / resourcestring globals that the Win64 entry shim in    */
/* runtime_fpc_init.c populates from kgpc_init_args.                    */
/*                                                                      */
/* These globals are normally provided by the linked Pascal RTL:       */
/*   EntryInformation              -> FPC system.pp (system.inc:76)     */
/*   _FPC_SysInstance, _FPC_TlsKey -> system.pp / systhrd.inc           */
/*   WStrInitTablesTable           -> system.pp widestring manager      */
/*   _FPC_ResourceStringTables     -> objpas.pp (objpas.pp:426)         */
/* A native KGPC --target-windows program links KGPC's own System unit  */
/* instead of FPC's RTL, so none of these symbols get a definition and  */
/* the link fails with "undefined reference to EntryInformation" (and   */
/* the four siblings).                                                  */
/*                                                                      */
/* This translation unit supplies them.  It is deliberately a SEPARATE  */
/* archive member: when an FPC-RTL program (the pp.pas bootstrap) is    */
/* linked, system.pp/objpas already define every one of these symbols,  */
/* so the linker never pulls this member from libkgpc_runtime.a and     */
/* there is no double definition; when a native program is linked, the  */
/* member is pulled to resolve the otherwise-undefined references.      */
/* Standard static-archive resolution — not a weak symbol, not a        */
/* fallback branch.                                                     */
/*                                                                      */
/* Only the Win64 entry shim references these, so the definitions are   */
/* compiled only for Windows targets (matching runtime_fpc_init.c).     */
/* ------------------------------------------------------------------ */
#if defined(_WIN32) || defined(_WIN64) || defined(__MINGW32__) ||              \
    defined(__MINGW64__)

#include <stdint.h>

#include "runtime_fpc_win_entryinfo.h"

KgpcFPCEntryInformation EntryInformation;

void *WStrInitTablesTable = 0;

/* `_FPC_*` is a reserved C identifier prefix; bind to the linker symbol */
/* via an asm name, matching the extern decls in runtime_fpc_init.c.     */
uint64_t *kgpc_def_fpc_sys_instance __asm__("_FPC_SysInstance") = 0;
uint32_t *kgpc_def_fpc_tlskey __asm__("_FPC_TlsKey") = 0;
void *kgpc_def_fpc_resstr_tables __asm__("_FPC_ResourceStringTables") = 0;

#endif
