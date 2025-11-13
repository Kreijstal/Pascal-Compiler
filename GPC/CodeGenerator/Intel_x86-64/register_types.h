/*
    Damon Gwinn
    Defines for special registers with their purpose
*/

#ifndef REGISTER_TYPES_H
#define REGISTER_TYPES_H

#include "../../flags.h"

    /* TODO: Add division and stack chasing registers */

    /* Return register */
    #define RETURN_REG_64 "%rax"
    #define RETURN_REG_32 "%eax"

extern gpc_target_abi_t g_current_codegen_abi;

static inline int gpc_max_int_arg_regs(void)
{
    return (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? 4 : 6;
}

static inline int gpc_max_sse_arg_regs(void)
{
    return (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? 4 : 8;
}

static inline const char *current_arg_reg64(int num)
{
    static const char *const windows_regs[] = { "%rcx", "%rdx", "%r8", "%r9" };
    static const char *const sysv_regs[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
    const char *const *regs = (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    int limit = gpc_max_int_arg_regs();
    if (num < 0 || num >= limit)
        return NULL;
    return regs[num];
}

static inline const char *current_arg_reg32(int num)
{
    static const char *const windows_regs[] = { "%ecx", "%edx", "%r8d", "%r9d" };
    static const char *const sysv_regs[] = { "%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d" };
    const char *const *regs = (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    int limit = gpc_max_int_arg_regs();
    if (num < 0 || num >= limit)
        return NULL;
    return regs[num];
}

static inline const char *current_arg_reg_xmm(int num)
{
    static const char *const windows_regs[] = { "%xmm0", "%xmm1", "%xmm2", "%xmm3" };
    static const char *const sysv_regs[] = { "%xmm0", "%xmm1", "%xmm2", "%xmm3",
                                             "%xmm4", "%xmm5", "%xmm6", "%xmm7" };
    const char *const *regs = (g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS) ? windows_regs : sysv_regs;
    int limit = gpc_max_sse_arg_regs();
    if (num < 0 || num >= limit)
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
