#ifndef ASM_EMIT_H
#define ASM_EMIT_H

/**
 * Assembly instruction emission helpers for Intel x86-64.
 *
 * These inline functions consolidate common snprintf patterns used throughout
 * the code generator, reducing duplication and improving maintainability.
 */

#include <stdio.h>
#include <string.h>

/**
 * Emit a movq instruction: movq src, dst
 */
static inline void asm_emit_movq(char *buffer, size_t bufsize, const char *src, const char *dst)
{
    snprintf(buffer, bufsize, "\tmovq\t%s, %s\n", src, dst);
}

/**
 * Emit a movq instruction with stack offset: movq src, -offset(%rbp)
 */
static inline void asm_emit_movq_to_stack(char *buffer, size_t bufsize, const char *src_reg, int offset)
{
    snprintf(buffer, bufsize, "\tmovq\t%s, -%d(%%rbp)\n", src_reg, offset);
}

/**
 * Emit a movq instruction from stack offset: movq -offset(%rbp), dst
 */
static inline void asm_emit_movq_from_stack(char *buffer, size_t bufsize, int offset, const char *dst_reg)
{
    snprintf(buffer, bufsize, "\tmovq\t-%d(%%rbp), %s\n", offset, dst_reg);
}

/**
 * Emit a movl instruction: movl src, dst
 */
static inline void asm_emit_movl(char *buffer, size_t bufsize, const char *src, const char *dst)
{
    snprintf(buffer, bufsize, "\tmovl\t%s, %s\n", src, dst);
}

/**
 * Emit a movl with immediate value: movl $imm, dst
 */
static inline void asm_emit_movl_imm(char *buffer, size_t bufsize, int imm, const char *dst_reg)
{
    snprintf(buffer, bufsize, "\tmovl\t$%d, %s\n", imm, dst_reg);
}

/**
 * Emit a movdqu instruction (SIMD move): movdqu src, -offset(%rbp)
 */
static inline void asm_emit_movdqu_to_stack(char *buffer, size_t bufsize, const char *src_reg, int offset)
{
    snprintf(buffer, bufsize, "\tmovdqu\t%s, -%d(%%rbp)\n", src_reg, offset);
}

/**
 * Emit a movdqu instruction from stack: movdqu -offset(%rbp), dst
 */
static inline void asm_emit_movdqu_from_stack(char *buffer, size_t bufsize, int offset, const char *dst_reg)
{
    snprintf(buffer, bufsize, "\tmovdqu\t-%d(%%rbp), %s\n", offset, dst_reg);
}

/**
 * Emit a movabsq instruction with immediate: movabsq $imm, dst
 */
static inline void asm_emit_movabsq(char *buffer, size_t bufsize, long long imm, const char *dst_reg)
{
    snprintf(buffer, bufsize, "\tmovabsq\t$%lld, %s\n", imm, dst_reg);
}

/**
 * Emit a cmpq instruction: cmpq op1, op2
 */
static inline void asm_emit_cmpq(char *buffer, size_t bufsize, const char *op1, const char *op2)
{
    snprintf(buffer, bufsize, "\tcmpq\t%s, %s\n", op1, op2);
}

/**
 * Emit a cmpq with register and stack offset: cmpq reg, -offset(%rbp)
 */
static inline void asm_emit_cmpq_stack(char *buffer, size_t bufsize, const char *reg, int offset)
{
    snprintf(buffer, bufsize, "\tcmpq\t%s, -%d(%%rbp)\n", reg, offset);
}

/**
 * Emit a cmpq with immediate and stack offset: cmpq $imm, -offset(%rbp)
 */
static inline void asm_emit_cmpq_imm_stack(char *buffer, size_t bufsize, long long imm, int offset)
{
    snprintf(buffer, bufsize, "\tcmpq\t$%lld, -%d(%%rbp)\n", imm, offset);
}

/**
 * Emit a cmpl with immediate and stack offset: cmpl $imm, -offset(%rbp)
 */
static inline void asm_emit_cmpl_imm_stack(char *buffer, size_t bufsize, long long imm, int offset)
{
    snprintf(buffer, bufsize, "\tcmpl\t$%lld, -%d(%%rbp)\n", imm, offset);
}

/**
 * Emit cvtsi2sdq (convert signed int64 to double): cvtsi2sdq src, %xmm0
 */
static inline void asm_emit_cvtsi2sdq(char *buffer, size_t bufsize, const char *src_reg)
{
    snprintf(buffer, bufsize, "\tcvtsi2sdq\t%s, %%xmm0\n", src_reg);
}

/**
 * Emit cvtsi2sdl (convert signed int32 to double): cvtsi2sdl src, %xmm0
 */
static inline void asm_emit_cvtsi2sdl(char *buffer, size_t bufsize, const char *src_reg)
{
    snprintf(buffer, bufsize, "\tcvtsi2sdl\t%s, %%xmm0\n", src_reg);
}

/**
 * Emit leaq instruction with RIP-relative addressing: leaq label(%rip), dst
 */
static inline void asm_emit_leaq_rip(char *buffer, size_t bufsize, const char *label, const char *dst_reg)
{
    snprintf(buffer, bufsize, "\tleaq\t%s(%%rip), %s\n", label, dst_reg);
}

/**
 * Emit leaq instruction with stack offset: leaq -offset(%rbp), dst
 */
static inline void asm_emit_leaq_stack(char *buffer, size_t bufsize, int offset, const char *dst_reg)
{
    snprintf(buffer, bufsize, "\tleaq\t-%d(%%rbp), %s\n", offset, dst_reg);
}

#endif /* ASM_EMIT_H */
