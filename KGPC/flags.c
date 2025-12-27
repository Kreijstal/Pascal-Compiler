/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#include "flags.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Flag for turning on non-local variable chasing */
/* Set with '-non-local' */
/* WARNING: Currently buggy */
int FLAG_NON_LOCAL_CHASING = 0;

/* Flag for turning on optimizations */
/* Set with -O1 and -O2 */
int FLAG_OPTIMIZE = 0;
int FLAG_PARSE_ONLY = 0;
static char *FLAG_DUMP_AST_PATH = NULL;
static int FLAG_TIME_PASSES = 0;
static int FLAG_ASM_DEBUG_COMMENTS = 0;
static int FLAG_STDLIB_LOADED = 0;

static kgpc_target_abi_t FLAG_TARGET_ABI =
#if defined(_WIN32) || defined(__CYGWIN__)
    KGPC_TARGET_ABI_WINDOWS;
#else
    KGPC_TARGET_ABI_SYSTEM_V;
#endif

void set_nonlocal_flag(void)
{
    FLAG_NON_LOCAL_CHASING = 1;
}
void set_o1_flag(void)
{
    if(FLAG_OPTIMIZE < 1)
        FLAG_OPTIMIZE = 1;
}

void set_o2_flag(void)
{
    if(FLAG_OPTIMIZE < 2)
        FLAG_OPTIMIZE = 2;
}

void set_parse_only_flag(void)
{
    FLAG_PARSE_ONLY = 1;
}

void set_time_passes_flag(void)
{
    FLAG_TIME_PASSES = 1;
}

void set_asm_debug_flag(void)
{
    FLAG_ASM_DEBUG_COMMENTS = 1;
}

void set_stdlib_loaded_flag(int loaded)
{
    FLAG_STDLIB_LOADED = loaded ? 1 : 0;
}
void set_dump_ast_path(const char *path)
{
    if (FLAG_DUMP_AST_PATH != NULL)
    {
        free(FLAG_DUMP_AST_PATH);
        FLAG_DUMP_AST_PATH = NULL;
    }

    if (path != NULL)
    {
        FLAG_DUMP_AST_PATH = strdup(path);
        if (FLAG_DUMP_AST_PATH == NULL)
        {
            fprintf(stderr, "ERROR: Unable to allocate memory for dump-ast path.\n");
            exit(1);
        }
    }
}

void set_target_windows_flag(void)
{
    FLAG_TARGET_ABI = KGPC_TARGET_ABI_WINDOWS;
}

void set_target_sysv_flag(void)
{
    FLAG_TARGET_ABI = KGPC_TARGET_ABI_SYSTEM_V;
}

int nonlocal_flag(void)
{
    return FLAG_NON_LOCAL_CHASING;
}
int optimize_flag(void)
{
    return FLAG_OPTIMIZE;
}

int parse_only_flag(void)
{
    return FLAG_PARSE_ONLY;
}

int time_passes_flag(void)
{
    return FLAG_TIME_PASSES;
}

int asm_debug_flag(void)
{
    return FLAG_ASM_DEBUG_COMMENTS;
}

int stdlib_loaded_flag(void)
{
    return FLAG_STDLIB_LOADED;
}

const char *dump_ast_path(void)
{
    return FLAG_DUMP_AST_PATH;
}

void clear_dump_ast_path(void)
{
    if (FLAG_DUMP_AST_PATH != NULL)
    {
        free(FLAG_DUMP_AST_PATH);
        FLAG_DUMP_AST_PATH = NULL;
    }
}

int target_windows_flag(void)
{
    return FLAG_TARGET_ABI == KGPC_TARGET_ABI_WINDOWS;
}

kgpc_target_abi_t current_target_abi(void)
{
    return FLAG_TARGET_ABI;
}
