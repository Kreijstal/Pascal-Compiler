/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#include "flags.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Flag for turning on non-local variable chasing */
/* Set with '-non-local' */
/* Non-local access is required by RTL/bootstrap units; keep it enabled by
 * default. */
int FLAG_NON_LOCAL_CHASING = 1;

/* Flag for turning on optimizations */
/* Set with -O1 and -O2 */
int FLAG_OPTIMIZE = 0;
int FLAG_PARSE_ONLY = 0;
static char *FLAG_DUMP_AST_PATH = NULL;
static int FLAG_TIME_PASSES = 0;
static int FLAG_ASM_DEBUG_COMMENTS = 0;
static int FLAG_DISABLE_DCE = 0;
static int FLAG_STDLIB_LOADED = 0;

/* Flag set when the bundled KGPC stdlib is skipped (--no-stdlib / -Us): the
 * program supplies its own RTL (e.g. the FPC RTL from source).  In that mode
 * KGPC must NOT substitute its C-runtime stdio for the RTL's own standard text
 * files (Input/Output/StdErr/...): the RTL initialises them itself (on Win64
 * via OpenStdIO binding StdErr to StdErrorHandle), and hijacking them to the C
 * runtime sends WriteLn(stderr,...) to the wrong OS handle. */
static int FLAG_NO_STDLIB = 0;

/* Flag for compiling the System unit (FPC -Us flag) */
static int FLAG_COMPILE_SYSTEM_UNIT = 0;

/* Flag for enabling goto statements (FPC -Sg flag) */
static int FLAG_GOTO_ENABLED = 0;

/* Flag for emitting each function in its own .text section (for linker
 * --gc-sections) */
static int FLAG_FUNCTION_SECTIONS = 0;

/* Flag for skipping unit codegen (only emit program code; units come from
 * cached .o) */
static int FLAG_SKIP_UNIT_CODEGEN = 0;

/* Flag indicating we're populating the codegen cache (cache miss).
 * Gates per-function error isolation, non-fatal codegen, and mark-all-used. */
static int FLAG_CODEGEN_CACHE_MISS = 0;

static kgpc_target_abi_t FLAG_TARGET_ABI =
#if defined(_WIN32) || defined(__CYGWIN__)
    KGPC_TARGET_ABI_WINDOWS;
#else
    KGPC_TARGET_ABI_SYSTEM_V;
#endif

void set_nonlocal_flag(void) { FLAG_NON_LOCAL_CHASING = 1; }
void set_o1_flag(void) {
  if (FLAG_OPTIMIZE < 1)
    FLAG_OPTIMIZE = 1;
}

void set_o2_flag(void) {
  if (FLAG_OPTIMIZE < 2)
    FLAG_OPTIMIZE = 2;
}

void set_parse_only_flag(void) { FLAG_PARSE_ONLY = 1; }

void set_time_passes_flag(void) { FLAG_TIME_PASSES = 1; }

void set_asm_debug_flag(void) { FLAG_ASM_DEBUG_COMMENTS = 1; }

void set_disable_dce_flag(void) { FLAG_DISABLE_DCE = 1; }

void set_stdlib_loaded_flag(int loaded) { FLAG_STDLIB_LOADED = loaded ? 1 : 0; }
void set_no_stdlib_flag(int no_stdlib) { FLAG_NO_STDLIB = no_stdlib ? 1 : 0; }
bool set_dump_ast_path(const char *path) {
  if (FLAG_DUMP_AST_PATH != NULL) {
    free(FLAG_DUMP_AST_PATH);
    FLAG_DUMP_AST_PATH = NULL;
  }

  if (path != NULL) {
    FLAG_DUMP_AST_PATH = strdup(path);
    if (FLAG_DUMP_AST_PATH == NULL) {
      fprintf(stderr, "ERROR: Unable to allocate memory for dump-ast path.\n");
      return false;
    }
  }
  return true;
}

void set_target_windows_flag(void) {
  FLAG_TARGET_ABI = KGPC_TARGET_ABI_WINDOWS;
}

void set_target_sysv_flag(void) { FLAG_TARGET_ABI = KGPC_TARGET_ABI_SYSTEM_V; }

int nonlocal_flag(void) { return FLAG_NON_LOCAL_CHASING; }
int optimize_flag(void) { return FLAG_OPTIMIZE; }

int parse_only_flag(void) { return FLAG_PARSE_ONLY; }

int time_passes_flag(void) { return FLAG_TIME_PASSES; }

int asm_debug_flag(void) { return FLAG_ASM_DEBUG_COMMENTS; }

int disable_dce_flag(void) { return FLAG_DISABLE_DCE; }

int stdlib_loaded_flag(void) { return FLAG_STDLIB_LOADED; }

int no_stdlib_flag(void) { return FLAG_NO_STDLIB; }

const char *dump_ast_path(void) { return FLAG_DUMP_AST_PATH; }

void clear_dump_ast_path(void) {
  if (FLAG_DUMP_AST_PATH != NULL) {
    free(FLAG_DUMP_AST_PATH);
    FLAG_DUMP_AST_PATH = NULL;
  }
}

int target_windows_flag(void) {
  return FLAG_TARGET_ABI == KGPC_TARGET_ABI_WINDOWS;
}

kgpc_target_abi_t current_target_abi(void) { return FLAG_TARGET_ABI; }

int kgpc_target_filerec_size(void) {
  /* Storage size of FPC's FileRec (rtl/inc/filerec.inc), which a typed/untyped
   * file variable must reserve and which the RTL's InitFile zeroes with
   * FillChar(f, SizeOf(FileRec), 0).  The layout is target-dependent:
   *
   *   field      | Linux x86_64        | Win64
   *   -----------+---------------------+--------------------------
   *   Handle     | THandle = Longint 4 | THandle = QWord 8 (+4 pad)
   *   Mode       | longint 4           | longint 4
   *   RecSize    | SizeInt 8           | SizeInt 8
   *   _private   | 64                  | 64
   *   UserData   | 32                  | 32
   *   name       | AnsiChar[0..255]256 | UnicodeChar[0..255] 512
   *   FullName   | Pointer 8           | Pointer 8
   *   -----------+---------------------+--------------------------
   *   total      | 376                 | 640
   *
   * `name`'s element type is TFileTextRecChar (systemh.inc): AnsiChar on
   * Unix (FPC_ANSI_TEXTFILEREC) and UnicodeChar on Windows.  FullName is
   * present on both targets (USE_FILEREC_FULLNAME follows
   * FPC_HAS_FEATURE_UNICODESTRINGS).  A wrong size here under-allocates the
   * file variable so InitFile's zero-fill overruns into the adjacent symbol. */
  return target_windows_flag() ? 640 : 376;
}

int kgpc_target_textrec_size(void) {
  /* Storage size of FPC's TextRec (rtl/inc/textrec.inc), which a Text
   * variable must reserve and which the RTL's text init zeroes.  Like
   * FileRec the layout is target-dependent:
   *
   *   field      | Linux x86_64        | Win64
   *   -----------+---------------------+--------------------------
   *   Handle     | THandle = Longint 4 | THandle = QWord 8 (+4 pad)
   *   Mode       | longint 4           | longint 4
   *   bufsize..  | 6 * SizeInt/ptr 48  | 6 * SizeInt/ptr 48
   *   4 funcptrs | 32                  | 32
   *   UserData   | 32                  | 32
   *   name       | AnsiChar[0..255]256 | UnicodeChar[0..255] 512
   *   LineEnd    | string[3] 4         | string[3] 4
   *   buffer     | AnsiChar[0..255]256 | AnsiChar[0..255] 256
   *   CodePage   | Word 2 (+pad)       | Word 2 (+pad)
   *   FullName   | Pointer 8           | Pointer 8
   *   -----------+---------------------+--------------------------
   *   total      | 640                 | 904
   *
   * As with FileRec, `name`'s element is TFileTextRecChar — AnsiChar on
   * Unix, UnicodeChar (2 bytes) on Windows — so on Win64 it grows by 256
   * bytes; together with the wider THandle (+4) and its alignment pad (+4)
   * that lifts the record from 640 to 904.  The `buffer` field (256 bytes)
   * makes a wrong size here especially dangerous: writing a Text var's
   * buffer overruns far past the allocation into adjacent symbols. */
  return target_windows_flag() ? 904 : 640;
}

void set_compile_system_unit_flag(void) { FLAG_COMPILE_SYSTEM_UNIT = 1; }

void set_goto_enabled_flag(void) { FLAG_GOTO_ENABLED = 1; }

int compile_system_unit_flag(void) { return FLAG_COMPILE_SYSTEM_UNIT; }

int goto_enabled_flag(void) { return FLAG_GOTO_ENABLED; }

void set_function_sections_flag(void) { FLAG_FUNCTION_SECTIONS = 1; }

void clear_function_sections_flag(void) { FLAG_FUNCTION_SECTIONS = 0; }

int function_sections_flag(void) { return FLAG_FUNCTION_SECTIONS; }

void set_skip_unit_codegen_flag(void) { FLAG_SKIP_UNIT_CODEGEN = 1; }

void clear_skip_unit_codegen_flag(void) { FLAG_SKIP_UNIT_CODEGEN = 0; }

int skip_unit_codegen_flag(void) { return FLAG_SKIP_UNIT_CODEGEN; }

/* Flag for --dump-ir-after=def-use: dump IR with def/use annotations to stderr
 * after each function's code generation. */
static int FLAG_DUMP_IR = 0;

/* Flag for --dump-ir-after=cfg: dump the control-flow graph to stderr
 * after each function's code generation. */
static int FLAG_DUMP_IR_CFG = 0;

/* Flag for --dump-ir-after=liveness: dump live-in/live-out sets to stderr
 * after each function's code generation. */
static int FLAG_DUMP_IR_LIVENESS = 0;

void set_codegen_cache_miss_flag(void) { FLAG_CODEGEN_CACHE_MISS = 1; }

void clear_codegen_cache_miss_flag(void) { FLAG_CODEGEN_CACHE_MISS = 0; }

int codegen_cache_miss_flag(void) { return FLAG_CODEGEN_CACHE_MISS; }

void set_dump_ir_flag(void) { FLAG_DUMP_IR = 1; }

int dump_ir_flag(void) { return FLAG_DUMP_IR; }

void set_dump_ir_cfg_flag(void) { FLAG_DUMP_IR_CFG = 1; }

int dump_ir_cfg_flag(void) { return FLAG_DUMP_IR_CFG; }

void set_dump_ir_liveness_flag(void) { FLAG_DUMP_IR_LIVENESS = 1; }

int dump_ir_liveness_flag(void) { return FLAG_DUMP_IR_LIVENESS; }
