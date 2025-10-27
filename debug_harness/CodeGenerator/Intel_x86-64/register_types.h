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

#if defined(_WIN32) || defined(_WIN64)
    /* Microsoft x64 ABI */
    #define ARG_REG_1_64 "%rcx"
    #define ARG_REG_1_32 "%ecx"

    #define ARG_REG_2_64 "%rdx"
    #define ARG_REG_2_32 "%edx"

    #define ARG_REG_3_64 "%r8"
    #define ARG_REG_3_32 "%r8d"

    #define ARG_REG_4_64 "%r9"
    #define ARG_REG_4_32 "%r9d"

    #define NON_LOCAL_REG_64 "%r11"
    #define NON_LOCAL_REG_32 "%r11d"
#else
    /* System V AMD64 ABI */
    #define ARG_REG_1_64 "%rdi"
    #define ARG_REG_1_32 "%edi"

    #define ARG_REG_2_64 "%rsi"
    #define ARG_REG_2_32 "%esi"

    #define ARG_REG_3_64 "%rdx"
    #define ARG_REG_3_32 "%edx"

    #define ARG_REG_4_64 "%rcx"
    #define ARG_REG_4_32 "%ecx"

    #define NON_LOCAL_REG_64 "%rcx"
    #define NON_LOCAL_REG_32 "%ecx"
#endif

#endif
