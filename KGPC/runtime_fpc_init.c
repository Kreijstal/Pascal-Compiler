/*
 * runtime_fpc_init.c — FPC RTL initialization code for KGPC.
 *
 * Contains functions called from kgpc_init_args (in runtime.c) that must
 * always be linked in, regardless of FPC RTL mode.  Separated from
 * runtime_fpc_assign.c so that the archive linker does NOT pull in the
 * FPC file-I/O strong overrides in non-FPC mode (which would conflict
 * with the compiler-generated symbols from system.p).
 */

#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ------------------------------------------------------------------ */
/* MODE_OPEN: FPC constant S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH */
/* = 0666 octal = 438.  Referenced as a global variable by FPC RTL code.     */
/* ------------------------------------------------------------------ */
int32_t MODE_OPEN = 0x1B6; /* 438 decimal, 0o666 octal */

/* ------------------------------------------------------------------ */
/* operatingsystem_parameter_* : FPC system unit global variables      */
/* Set by kgpc_fpc_init_os_params() called from kgpc_init_args.        */
/* ------------------------------------------------------------------ */
int32_t operatingsystem_parameter_argc = 0;
void *operatingsystem_parameter_argv = NULL;
void *operatingsystem_parameter_envp = NULL;

/* FPC system startup globals normally provided by the target startup code. */
uintptr_t __stklen = 8u * 1024u * 1024u;
void *__stkptr = NULL;

/* Called from kgpc_init_args to populate FPC globals */
void kgpc_fpc_init_os_params(int argc, char **argv, char **envp) {
  operatingsystem_parameter_argc = (int32_t)argc;
  operatingsystem_parameter_argv = (void *)argv;
  operatingsystem_parameter_envp = (void *)envp;
}

void kgpc_fpc_init_stack_params(void *stack_probe) {
  if (__stkptr == NULL)
    __stkptr = stack_probe;
  if (__stklen == 0)
    __stklen = 8u * 1024u * 1024u;
}

/* ------------------------------------------------------------------ */
/* FPC FPU initialization.                                             */
/* Default8087CW and DefaultMXCSR are declared in the system unit as   */
/* zero-initialized globals.  sysresetfpu loads them into the FPU/SSE  */
/* control registers, but zero means all exceptions are unmasked.      */
/* FPC's normal values: Default8087CW=$1332, DefaultMXCSR=$1F80.       */
/* Must be set before initthread_u64 calls sysresetfpu.                */
/*                                                                     */
/* Common definitions (.comm) are overridden by the FPC system unit's  */
/* strong .globl definitions when linked together.  For non-FPC        */
/* programs the .comm provides the storage so the init function        */
/* doesn't fault.  Portable — no weak symbols needed.                  */
/* ------------------------------------------------------------------ */
#ifdef __GNUC__
int16_t Default8087CW __attribute__((common));
int32_t DefaultMXCSR __attribute__((common));
#else
int16_t Default8087CW;
int32_t DefaultMXCSR;
#endif

void kgpc_fpc_init_fpu(void) {
  Default8087CW = 0x1332; /* FPC default: mask all x87 exceptions */
  DefaultMXCSR =
      0x1F80; /* SSE default: mask all exceptions, round-to-nearest */
}

/* Note: FPC heap init (initthread_u64 / InitThread) is handled by the
 * FPC system unit's own initialization section — not by the C runtime.
 * This avoids needing a weak symbol or function pointer for a function
 * that only exists in FPC programs. */

/* ------------------------------------------------------------------ */
/* FPC_THREADVARTABLES: Thread variable tables (TltvInitTablesTable)   */
/* count=0 means no thread variables — init/copy loops do nothing.     */
/* ------------------------------------------------------------------ */
struct {
  int64_t count;
  void *tables[1]; /* placeholder, never accessed when count=0 */
} FPC_THREADVARTABLES = {0, {NULL}};

/* ------------------------------------------------------------------ */
/* Win64 RTL indirect-entry-information shim.                          */
/*                                                                     */
/* When KGPC targets Windows, the program's main() calls Pascal `pp`   */
/* directly without going through FPC's _FPC_EXE_Entry/                */
/* SetupEntryInformation/OsSetupEntryInformation chain (which is the   */
/* responsibility of sysinit.pp's _mainCRTStartup in a normal FPC      */
/* build).  As a result, the indirection-pointer globals the win64     */
/* system.pp expects to be already wired up — _FPC_SysInstance         */
/* (PQWord), _FPC_TlsKey (PDword), and WStrInitTablesTable             */
/* (PWStrInitTablesTable) — remain NULL when system.pp's initial-      */
/* ization section runs.  The very first dereference (`FPCSysInstance^ */
/* := getmodulehandle(nil)` in FPCSource/rtl/win64/system.pp:436) then */
/* faults writing to address 0.                                        */
/*                                                                     */
/* This shim provides backing storage for SysInstance, TlsKeyVar, and  */
/* a count=0 WStrInitTables, and installs pointers into the three      */
/* indirection globals from kgpc_init_args (before `pp` runs).         */
/*                                                                     */
/* Only compiled on Windows targets; the corresponding symbols do not  */
/* exist on Linux RTL and would be unresolved externs there.           */
/* ------------------------------------------------------------------ */
#if defined(_WIN32) || defined(_WIN64) || defined(__MINGW32__) ||              \
    defined(__MINGW64__)

/* count=0 wide-string init table.  System.pp's InitWin32Widestrings    */
/* iterates `for i:=1 to WStrInitTablesTable^.count`; count=0 skips     */
/* the loop entirely.                                                   */
static struct {
  int64_t count;
  void *tables[1];
} kgpc_fpc_wstr_init_tables = {0, {NULL}};

/* count=0 init/final and resource-string tables for EntryInformation.
 * TInitFinalTable layout: TableCount, InitCount, then Procs[].  FPC's
 * FPC_INITIALIZEUNITS / FPC_FINALIZEUNITS loop `for i := 1 to TableCount`
 * and `while InitCount > 0`; both exit immediately when zero. */
static struct {
  uint64_t TableCount;
  uint64_t InitCount;
} kgpc_fpc_init_final_table = {0, 0};

static struct {
  int64_t count;
  void *tables[1];
} kgpc_fpc_resourcestring_tables = {0, {NULL}};

/* Backing storage for the indirection pointers.                       */
static uint64_t kgpc_fpc_sys_instance_storage;
static uint32_t kgpc_fpc_tlskey_storage = 0xFFFFFFFFu; /* matches FPC's
                                                          TlsKeyVar default */

/* The Pascal globals are declared in FPCSource/rtl/win64/system.pp:58 */
/* and FPCSource/rtl/win/systhrd.inc:97 as:                             */
/*   FPCSysInstance : PQWord; public name '_FPC_SysInstance';           */
/*   TLSKey         : PDword; public name '_FPC_TlsKey';                */
/* `_FPC_*` is a reserved C identifier prefix; use asm-name binding to */
/* refer to the linker symbol without forming a reserved C name.       */
extern uint64_t *kgpc_fpc_sys_instance_ptr __asm__("_FPC_SysInstance");
extern uint32_t *kgpc_fpc_tlskey_ptr __asm__("_FPC_TlsKey");
extern void *WStrInitTablesTable;

/* EntryInformation: TEntryInformation global in FPC system.pp (declared
 * in rtl/inc/system.inc:76 as `EntryInformation: TEntryInformation`).
 * Normally populated by sysinit.pp's SetupEntryInformation chain, which
 * KGPC bypasses.  Layout per rtl/inc/systemh.inc:722 with
 * HAS_ENTRYINFORMATION_OS on Win64 (rtl/win/sysosh.inc:54):
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
 * Total 80 bytes. */
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

extern KgpcFPCEntryInformation EntryInformation;

void kgpc_fpc_init_win_entry_info(void) {
  kgpc_fpc_sys_instance_ptr = &kgpc_fpc_sys_instance_storage;
  kgpc_fpc_tlskey_ptr = &kgpc_fpc_tlskey_storage;
  WStrInitTablesTable = &kgpc_fpc_wstr_init_tables;

  EntryInformation.InitFinalTable = &kgpc_fpc_init_final_table;
  EntryInformation.ThreadvarTablesTable = &FPC_THREADVARTABLES;
  EntryInformation.ResourceStringTables = &kgpc_fpc_resourcestring_tables;
  EntryInformation.ResStrInitTables = NULL;
  EntryInformation.ResLocation = NULL;
  EntryInformation.PascalMain = NULL;
  EntryInformation.valgrind_used = 0;
  EntryInformation.OS_TlsKeyAddr = &kgpc_fpc_tlskey_storage;
  EntryInformation.OS_SysInstance = &kgpc_fpc_sys_instance_storage;
  EntryInformation.OS_WideInitTables = &kgpc_fpc_wstr_init_tables;
}

#else /* !Windows */

void kgpc_fpc_init_win_entry_info(void) { /* no-op on non-Windows */ }

#endif

/* ------------------------------------------------------------------ */
/* dest / Dest: operator result parameters leaked as global references */
/* These appear as dead code in .weak functions overridden by the      */
/* runtime's strong widechar__op_assign_olevariant_wc.  Providing      */
/* them as zero globals satisfies the linker.                          */
/* ------------------------------------------------------------------ */
void *dest = NULL;
void *Dest = NULL;

/* ------------------------------------------------------------------ */
/* FPC TThreadManager implementation (single-threaded).                */
/* FPC RTL's commoninit calls through CurrentTM function pointers.     */
/* ------------------------------------------------------------------ */

/* RTL events: malloc'd flag + mutex + condvar for proper wait semantics */
typedef struct {
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  int signaled;
} KgpcRTLEvent;

static void *kgpc_rtlevent_create(void) {
  KgpcRTLEvent *ev = (KgpcRTLEvent *)calloc(1, sizeof(KgpcRTLEvent));
  if (ev == NULL)
    return NULL;
  pthread_mutex_init(&ev->mutex, NULL);
  pthread_cond_init(&ev->cond, NULL);
  return ev;
}

static void kgpc_rtlevent_destroy(void *event) {
  KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
  if (ev == NULL)
    return;
  pthread_cond_destroy(&ev->cond);
  pthread_mutex_destroy(&ev->mutex);
  free(ev);
}

static void kgpc_rtlevent_set(void *event) {
  KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
  if (ev == NULL)
    return;
  pthread_mutex_lock(&ev->mutex);
  ev->signaled = 1;
  pthread_cond_signal(&ev->cond);
  pthread_mutex_unlock(&ev->mutex);
}

static void kgpc_rtlevent_reset(void *event) {
  KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
  if (ev == NULL)
    return;
  pthread_mutex_lock(&ev->mutex);
  ev->signaled = 0;
  pthread_mutex_unlock(&ev->mutex);
}

static void kgpc_rtlevent_wait(void *event) {
  KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
  if (ev == NULL)
    return;
  pthread_mutex_lock(&ev->mutex);
  while (!ev->signaled)
    pthread_cond_wait(&ev->cond, &ev->mutex);
  ev->signaled = 0;
  pthread_mutex_unlock(&ev->mutex);
}

static void kgpc_rtlevent_wait_timeout(void *event, int timeout) {
  KgpcRTLEvent *ev = (KgpcRTLEvent *)event;
  if (ev == NULL)
    return;
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  ts.tv_sec += timeout / 1000;
  ts.tv_nsec += (timeout % 1000) * 1000000L;
  if (ts.tv_nsec >= 1000000000L) {
    ts.tv_sec++;
    ts.tv_nsec -= 1000000000L;
  }
  pthread_mutex_lock(&ev->mutex);
  while (!ev->signaled)
    if (pthread_cond_timedwait(&ev->cond, &ev->mutex, &ts) != 0)
      break;
  ev->signaled = 0;
  pthread_mutex_unlock(&ev->mutex);
}

/* Critical sections backed by pthread_mutex */
static void kgpc_critsection_init(void *cs) {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init((pthread_mutex_t *)cs, &attr);
  pthread_mutexattr_destroy(&attr);
}
static void kgpc_critsection_done(void *cs) {
  pthread_mutex_destroy((pthread_mutex_t *)cs);
}
static void kgpc_critsection_enter(void *cs) {
  pthread_mutex_lock((pthread_mutex_t *)cs);
}
static int64_t kgpc_critsection_tryenter(void *cs) {
  return pthread_mutex_trylock((pthread_mutex_t *)cs) == 0 ? 1 : 0;
}
static void kgpc_critsection_leave(void *cs) {
  pthread_mutex_unlock((pthread_mutex_t *)cs);
}

static int64_t kgpc_tm_init(void) { return 1; }
static int64_t kgpc_get_current_thread_id(void) {
  return (int64_t)pthread_self();
}

/* CurrentTM: FPC's TThreadManager record (35 function pointers).
 * Defined in Pascal (system.p / system.pp) — the C runtime references them
 * as extern so the Pascal-generated symbols are the canonical definitions. */
extern void *CurrentTM[34 + 1];
extern void *NoThreadManager[34 + 1];
extern void *LazyInitThreadingProcList;

/* Called from kgpc_init_args to populate CurrentTM */
void kgpc_fpc_init_thread_manager(void) {
  if (CurrentTM[29] != NULL) /* already initialized */
    return;

  CurrentTM[0] = (void *)kgpc_tm_init; /* InitManager */
  CurrentTM[1] = (void *)kgpc_tm_init; /* DoneManager */
  CurrentTM[12] = (void *)kgpc_get_current_thread_id;
  CurrentTM[15] = (void *)kgpc_critsection_init;
  CurrentTM[16] = (void *)kgpc_critsection_done;
  CurrentTM[17] = (void *)kgpc_critsection_enter;
  CurrentTM[18] = (void *)kgpc_critsection_tryenter;
  CurrentTM[19] = (void *)kgpc_critsection_leave;
  CurrentTM[24] = (void *)kgpc_rtlevent_create; /* BasicEventCreate */
  CurrentTM[25] = (void *)kgpc_rtlevent_destroy;
  CurrentTM[26] = (void *)kgpc_rtlevent_reset;
  CurrentTM[27] = (void *)kgpc_rtlevent_set;
  CurrentTM[28] = (void *)kgpc_rtlevent_wait;
  CurrentTM[29] = (void *)kgpc_rtlevent_create; /* RTLEventCreate */
  CurrentTM[30] = (void *)kgpc_rtlevent_destroy;
  CurrentTM[31] = (void *)kgpc_rtlevent_set;
  CurrentTM[32] = (void *)kgpc_rtlevent_reset;
  CurrentTM[33] = (void *)kgpc_rtlevent_wait;
  CurrentTM[34] = (void *)kgpc_rtlevent_wait_timeout;
}

/* ------------------------------------------------------------------ */
/* format_s_a: Strong override for FPC's Format(AnsiString, array of  */
/* const) function.  Must live here (not runtime_fpc_assign.c) so     */
/* that the linker always sees the strong symbol — runtime_fpc_assign  */
/* is only pulled from the archive when another symbol references it. */
/* ------------------------------------------------------------------ */
typedef struct {
  int32_t kind;
  int32_t reserved;
  union {
    int64_t v_int;
    double v_real;
    void *v_ptr;
  } data;
} kgpc_tvarrec_t;
extern char *kgpc_format(const char *fmt, const kgpc_tvarrec_t *args,
                         size_t arg_count);

__attribute__((unused)) static char *format_s_a(const char *fmt,
                                                const void *args_desc) {
  const void *const *desc = (const void *const *)args_desc;
  const kgpc_tvarrec_t *args_array = (const kgpc_tvarrec_t *)desc[0];
  size_t count = (size_t)(uintptr_t)desc[1];
  return kgpc_format(fmt, args_array, count);
}
