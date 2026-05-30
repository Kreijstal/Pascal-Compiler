/**
 * @file flags.h
 * @brief Compiler-wide flag state (setters, getters, compile-time
 * `DEBUG_FLAG` helpers).
 *
 * Holds the global on/off knobs (optimisation, codegen-cache, parse-only,
 * target-ABI, debug-output channels, ...).  Setters change process-global
 * state; getters return the current value.  Debug flags can be turned on
 * at build time (`-DKGPC_DEBUG_FOO`) or at run time
 * (`KGPC_DEBUG_FOO=1` env var) — the `DEBUG_FLAG(FOO)` macro yields true
 * for either path.
 */

#ifndef FLAGS_H
#define FLAGS_H

#include <stdbool.h>

/** @brief Target ABI selection for the codegen and runtime. */
typedef enum {
  KGPC_TARGET_ABI_SYSTEM_V = 0,  /**< Linux / macOS / BSD: SysV AMD64 calling convention. */
  KGPC_TARGET_ABI_WINDOWS = 1,   /**< Win64 calling convention. */
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

/**
 * @brief Test whether a debug channel is on (compile-time or runtime).
 *
 * `DEBUG_FLAG(FOO)` is true iff either `KGPC_DEBUG_FOO` is defined
 * to non-zero, or the environment variable `KGPC_DEBUG_FOO` is set.
 */
#define DEBUG_FLAG(name)                                                       \
  (KGPC_DEBUG_##name || getenv("KGPC_DEBUG_" #name) != NULL)

/** @brief Enable emission of non-local variable accesses (display register). */
void set_nonlocal_flag(void);
/** @brief Enable `-O1` optimisation level. */
void set_o1_flag(void);
/** @brief Enable `-O2` optimisation level. */
void set_o2_flag(void);
/** @brief Enable parse-only mode (semantic check without codegen). */
void set_parse_only_flag(void);
/** @brief Enable per-pass timing diagnostics. */
void set_time_passes_flag(void);
/** @brief Set the target ABI to Win64. */
void set_target_windows_flag(void);
/** @brief Set the target ABI to SysV AMD64 (Linux/macOS/BSD). */
void set_target_sysv_flag(void);

/**
 * @brief Set the dump-AST output path.
 *
 * Returns true on success; false if memory allocation failed
 * (in which case the previous path is cleared).
 */
bool set_dump_ast_path(const char *path);

/** @brief Enable annotated assembly output (`--asm-debug`). */
void set_asm_debug_flag(void);
/** @brief Disable dead-code elimination. */
void set_disable_dce_flag(void);
/** @brief Mark the bundled stdlib as loaded (or not loaded). */
void set_stdlib_loaded_flag(int loaded);
/** @brief Record whether the bundled stdlib is skipped (`--no-stdlib`/`-Us`),
 *  i.e. the program supplies its own RTL standard files. */
void set_no_stdlib_flag(int no_stdlib);
/** @brief Enter "compile system unit" mode (the `system.pp` self-build). */
void set_compile_system_unit_flag(void);
/** @brief Allow `goto` (off by default). */
void set_goto_enabled_flag(void);
/** @brief Enable per-function code sections (linker garbage collection). */
void set_function_sections_flag(void);
/** @brief Disable per-function code sections. */
void clear_function_sections_flag(void);
/** @brief Skip code generation for the current unit (cache hit). */
void set_skip_unit_codegen_flag(void);
/** @brief Clear the skip-unit-codegen flag (return to normal compile). */
void clear_skip_unit_codegen_flag(void);
/** @brief Record that the codegen cache lookup missed. */
void set_codegen_cache_miss_flag(void);
/** @brief Clear the codegen-cache-miss flag. */
void clear_codegen_cache_miss_flag(void);
/** @brief Enable IR dump (`--dump-ir`). */
void set_dump_ir_flag(void);
/** @brief Enable IR CFG dump (`--dump-ir-cfg`). */
void set_dump_ir_cfg_flag(void);
/** @brief Enable IR liveness dump (`--dump-ir-liveness`). */
void set_dump_ir_liveness_flag(void);

/** @brief Get the non-local-access flag. */
int nonlocal_flag(void);
/** @brief Get the current optimisation level (0/1/2). */
int optimize_flag(void);
/** @brief Get the parse-only flag. */
int parse_only_flag(void);
/** @brief Get the time-passes flag. */
int time_passes_flag(void);
/** @brief Get whether the target is Win64. */
int target_windows_flag(void);
/** @brief Storage size (bytes) of FPC's FileRec for the active target
 *  (376 on Linux x86_64, 640 on Win64). */
int kgpc_target_filerec_size(void);
/** @brief Storage size (bytes) of FPC's TextRec for the active target
 *  (640 on Linux x86_64, 904 on Win64). */
int kgpc_target_textrec_size(void);
/** @brief Get the active target ABI (`SYSTEM_V` or `WINDOWS`). */
kgpc_target_abi_t current_target_abi(void);
/** @brief Get the dump-AST output path, or NULL if unset. */
const char *dump_ast_path(void);
/** @brief Free and clear the dump-AST path. */
void clear_dump_ast_path(void);
/** @brief Get the annotated-assembly flag. */
int asm_debug_flag(void);
/** @brief Get the disable-DCE flag. */
int disable_dce_flag(void);
/** @brief Get the stdlib-loaded flag. */
int stdlib_loaded_flag(void);
/** @brief Get the no-stdlib flag (program supplies its own RTL std files). */
int no_stdlib_flag(void);
/** @brief Get the compile-system-unit flag. */
int compile_system_unit_flag(void);
/** @brief Get the goto-enabled flag. */
int goto_enabled_flag(void);
/** @brief Get the function-sections flag. */
int function_sections_flag(void);
/** @brief Get the skip-unit-codegen flag. */
int skip_unit_codegen_flag(void);
/** @brief Get the codegen-cache-miss flag. */
int codegen_cache_miss_flag(void);
/** @brief Get the dump-IR flag. */
int dump_ir_flag(void);
/** @brief Get the dump-IR-CFG flag. */
int dump_ir_cfg_flag(void);
/** @brief Get the dump-IR-liveness flag. */
int dump_ir_liveness_flag(void);

#endif
