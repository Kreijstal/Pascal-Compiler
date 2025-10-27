/*
    Damon Gwinn
    Defines for special registers with their purpose
*/

#ifndef REGISTER_TYPES_H
#define REGISTER_TYPES_H

    /* TODO: Add division and stack chasing registers */

    /* Return register */
    #define RETURN_REG_64 "%rax"
    #define RETURN_REG_32 "%eax"

    /* Argument registers */
    /* TODO: Add remaining registers */
    #define NUM_ARG_REG 4

#include "../../flags.h"

extern gpc_target_abi_t g_current_codegen_abi;

static inline const char *current_arg_reg64(int num)
{
    static const char *const windows_regs[] = { "%rcx", "%rdx", "%r8", "%r9" };
    static const char *const sysv_regs[] = { "%rdi", "%rsi", "%rdx", "%rcx" };
    const char *const *regs = (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    if (num < 0 || num >= NUM_ARG_REG)
        return NULL;
    return regs[num];
}

static inline const char *current_arg_reg32(int num)
{
    static const char *const windows_regs[] = { "%ecx", "%edx", "%r8d", "%r9d" };
    static const char *const sysv_regs[] = { "%edi", "%esi", "%edx", "%ecx" };
    const char *const *regs = (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    if (num < 0 || num >= NUM_ARG_REG)
        return NULL;
    return regs[num];
}

static inline const char *current_non_local_reg64(void)
{
    return g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS ? "%r11" : "%rcx";
}

static inline const char *current_non_local_reg32(void)
{
    return g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS ? "%r11d" : "%ecx";
}

#endif
