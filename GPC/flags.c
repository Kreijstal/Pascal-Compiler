/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#include "flags.h"

/* Flag for turning on non-local variable chasing */
/* Set with '-non-local' */
/* WARNING: Currently buggy */
int FLAG_NON_LOCAL_CHASING = 0;

/* Flag for turning on optimizations */
/* Set with -O1 and -O2 */
int FLAG_OPTIMIZE = 0;

static gpc_target_abi_t FLAG_TARGET_ABI =
#ifdef _WIN32
    GPC_TARGET_ABI_WINDOWS;
#else
    GPC_TARGET_ABI_SYSTEM_V;
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

void set_target_windows_flag(void)
{
    FLAG_TARGET_ABI = GPC_TARGET_ABI_WINDOWS;
}

void set_target_sysv_flag(void)
{
    FLAG_TARGET_ABI = GPC_TARGET_ABI_SYSTEM_V;
}

int nonlocal_flag(void)
{
    return FLAG_NON_LOCAL_CHASING;
}
int optimize_flag(void)
{
    return FLAG_OPTIMIZE;
}

int target_windows_flag(void)
{
    return FLAG_TARGET_ABI == GPC_TARGET_ABI_WINDOWS;
}

gpc_target_abi_t current_target_abi(void)
{
    return FLAG_TARGET_ABI;
}
