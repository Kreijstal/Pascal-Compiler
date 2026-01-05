/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#ifndef FLAGS_H
#define FLAGS_H

typedef enum {
    KGPC_TARGET_ABI_SYSTEM_V = 0,
    KGPC_TARGET_ABI_WINDOWS = 1,
} kgpc_target_abi_t;

/* Debug flags - can be enabled at compile time or runtime */
/* To enable at compile time: -DKGPC_DEBUG_SEMCHECK etc. */
/* To enable at runtime: set KGPC_DEBUG_SEMCHECK=1 etc. */

#ifndef KGPC_DEBUG_SEMCHECK
#define KGPC_DEBUG_SEMCHECK 0
#endif

#ifndef KGPC_DEBUG_TFPG
#define KGPC_DEBUG_TFPG 0
#endif

#ifndef KGPC_DEBUG_DEFAULT_PARAMS
#define KGPC_DEBUG_DEFAULT_PARAMS 0
#endif

#ifndef KGPC_DEBUG_GENERIC_CLONES
#define KGPC_DEBUG_GENERIC_CLONES 0
#endif

#ifndef KGPC_DEBUG_RESOLVE_TYPE
#define KGPC_DEBUG_RESOLVE_TYPE 0
#endif

#ifndef KGPC_DEBUG_PREDECLARE
#define KGPC_DEBUG_PREDECLARE 0
#endif

#ifndef KGPC_DEBUG_SEMSTEPS
#define KGPC_DEBUG_SEMSTEPS 0
#endif

#ifndef KGPC_DEBUG_BODY
#define KGPC_DEBUG_BODY 0
#endif

#ifndef KGPC_DEBUG_EXTERNAL
#define KGPC_DEBUG_EXTERNAL 0
#endif

#ifndef KGPC_DEBUG_LENGTH_ARGS
#define KGPC_DEBUG_LENGTH_ARGS 0
#endif

#ifndef KGPC_DEBUG_CONST_CAST
#define KGPC_DEBUG_CONST_CAST 0
#endif

#ifndef KGPC_DEBUG_PREDECLARE_POINTERS
#define KGPC_DEBUG_PREDECLARE_POINTERS 0
#endif

#ifndef KGPC_DEBUG_AMBIGUOUS
#define KGPC_DEBUG_AMBIGUOUS 0
#endif

#ifndef KGPC_DEBUG_INHERITED
#define KGPC_DEBUG_INHERITED 0
#endif

/* Macro to check debug flag (compile-time or runtime) */
#define DEBUG_FLAG(name) (KGPC_DEBUG_##name || getenv("KGPC_DEBUG_" #name) != NULL)

void set_nonlocal_flag(void);
void set_o1_flag(void);
void set_o2_flag(void);
void set_parse_only_flag(void);
void set_time_passes_flag(void);
void set_target_windows_flag(void);
void set_target_sysv_flag(void);
void set_dump_ast_path(const char *path);
void set_asm_debug_flag(void);
void set_stdlib_loaded_flag(int loaded);
void set_compile_system_unit_flag(void);
void set_goto_enabled_flag(void);

int nonlocal_flag(void);
int optimize_flag(void);
int parse_only_flag(void);
int time_passes_flag(void);
int target_windows_flag(void);
kgpc_target_abi_t current_target_abi(void);
const char *dump_ast_path(void);
void clear_dump_ast_path(void);
int asm_debug_flag(void);
int stdlib_loaded_flag(void);
int compile_system_unit_flag(void);
int goto_enabled_flag(void);

#endif
