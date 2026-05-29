#ifndef KGPC_RUNTIME_FPC_WIN_ENTRYINFO_H
#define KGPC_RUNTIME_FPC_WIN_ENTRYINFO_H

#include <stdint.h>

/* TEntryInformation: the FPC system.pp global (rtl/inc/system.inc:76).
 * Layout per rtl/inc/systemh.inc:722 with HAS_ENTRYINFORMATION_OS on Win64
 * (rtl/win/sysosh.inc:54):
 *   InitFinalTable (Pointer @ 0)
 *   ThreadvarTablesTable (Pointer @ 8)
 *   ResourceStringTables (Pointer @ 16)
 *   ResStrInitTables (Pointer @ 24)
 *   ResLocation (Pointer @ 32)
 *   PascalMain (Procedure @ 40)
 *   valgrind_used (Boolean @ 48) + 7 bytes padding
 *   OS.TlsKeyAddr (PDWord @ 56)
 *   OS.SysInstance (PQWord @ 64)
 *   OS.WideInitTables (Pointer @ 72)
 * Total 80 bytes.  Shared between the entry shim (runtime_fpc_init.c, which
 * populates it) and the native-program default definition
 * (runtime_fpc_win_entry_globals.c) so both agree on the layout. */
typedef struct {
  void *InitFinalTable;
  void *ThreadvarTablesTable;
  void *ResourceStringTables;
  void *ResStrInitTables;
  void *ResLocation;
  void *PascalMain;
  uint8_t valgrind_used;
  uint8_t _pad_os[7];
  void *OS_TlsKeyAddr;
  void *OS_SysInstance;
  void *OS_WideInitTables;
} KgpcFPCEntryInformation;

#endif /* KGPC_RUNTIME_FPC_WIN_ENTRYINFO_H */
