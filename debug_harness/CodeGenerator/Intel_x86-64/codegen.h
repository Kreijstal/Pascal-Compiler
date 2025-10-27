/*
    Damon Gwinn
    Code generation
    This is the dragon slayer

    Stack structure (64-bit):
        This is the current stack structure, subject to changes as needed
        Last modified 5-6-19

        d_word refers to a doubleword or 4 bytes
        q_word refers to a quadword or 8 bytes

        TODO:
            - Support non-local variables
            - Support generalized reading and writing
            - Support real numbers
            - Support more general purpose registers
            - Support registers r8-15 as general purpose registers

        LOW ADDRESSES
        ============
        t   d_words (temporaries)
        ============
        x   d_words (local variables)
        ============
        z   d_words (argument variables)
        ============
        1   q_word (static parent pointer)
        ============
        1   q_word (subprogram return addr)
        ============
        HIGH ADDRESSES

    Above aligned to 16 bytes as standard requires

    NOTES:
        - Add arguments, then locals, then temps
        - Function return "variable" pushed to x stack portion (SemCheck makes sure there's no issues)

    GENERAL PURPOSE REGISTERS:
        - RAX
        - R10

    SPECIAL REGISTERS:
        - RSP (Stack pointer)
        - RBP (Base pointer)

    REGISTER CONVENTIONS
        - RAX for returns
        - RAX and RDX for division (div in RAX, mod in RDX)
            - Dividend in RAX, divisor in RDX
        - RCX used for base pointer chasing (var in different scope)

    ARGUMENT CONVENTIONS:
        The first is placed in rdi, the second in rsi, the third in rdx, and then rcx, r8 and r9.
        All future arguments placed on the stack

    STACK MANAGEMENT:
        A stack management structure will be utilized (see stackmng.h)

        The stack manager shall consist of:
            - StackScope structure of the current scope which contains:
                - The current z offset (in bytes)
                - The current x offset (in bytes)
                - The current t offset (in bytes)
                - z blocks in use with labels and offsets
                - x blocks in use with labels and offsets
                - t blocks in use with labels and offsets
                - StackScope pointer to previous scope
            - Register stack of free general-purpose registers

    INSTRUCTION LAYOUT:
        The first section of instructions for a function is known as the "prologue."
        This section performs gcc standard stack management first, then allocates needed stack
        space.

        The next section is the body which consists of the desired user functionality of the
        function.

        The final section is the epilogue which performs required gcc function exiting standards

    OUTPUT:
        Output is written as a gcc assembly file (.s). Assembly is then assembled using gcc.

        The write builtin currently only takes integer types and has the
            label LC0 with %d\n. The function call is "call gpc_printf"

        The read builtin currently only reads integer types and has the
            label LC1 with "%d". The function call is "call gpc_scanf"

    TEMPORARIES:
        Temporaries are allocated as needed and reused once freed for efficiency
*/

#ifndef CODE_GEN_H
#define CODE_GEN_H

#include "../../flags.h"

extern gpc_target_abi_t g_current_codegen_abi;
extern int g_stack_home_space_bytes;

#ifdef GPC_DEBUG_CODEGEN
#define DEBUG_CODEGEN
#endif
#ifdef DEBUG_CODEGEN
#define CODEGEN_DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define CODEGEN_DEBUG(...) ((void)0)
#endif
#define MAX_ARGS 3
#define REQUIRED_OFFSET 16

static inline int codegen_target_is_windows(void)
{
    return g_current_codegen_abi == GPC_TARGET_ABI_WINDOWS;
}

#define NORMAL_JMP -1

#include <stdlib.h>
#include <stdio.h>
#include "stackmng/stackmng.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"

/*
    The context for the code generator.
    This struct holds all the state that was previously global,
    allowing for a more modular and re-entrant design.
*/
typedef struct {
    int label_counter;
    int write_label_counter;
    FILE *output_file;
    SymTab_t *symtab;
    gpc_target_abi_t target_abi;
} CodeGenContext;

/* Generates a label */
void gen_label(char *buf, int buf_len, CodeGenContext *ctx);

/* Escapes string for assembly output */
void escape_string(char *dest, const char *src, size_t dest_size);

/* This is the entry function */
void codegen(Tree_t *, const char *input_file_name, CodeGenContext *ctx, SymTab_t *symtab);

ListNode_t *add_inst(ListNode_t *, char *);
ListNode_t *gencode_jmp(int type, int inverse, char *label, ListNode_t *inst_list);

void codegen_program_header(const char *, CodeGenContext *ctx);
void codegen_rodata(CodeGenContext *ctx);
void codegen_program_footer(CodeGenContext *ctx);
void codegen_main(char *prgm_name, CodeGenContext *ctx);
void codegen_stack_space(CodeGenContext *ctx);
void codegen_inst_list(ListNode_t *, CodeGenContext *ctx);

char * codegen_program(Tree_t *, CodeGenContext *ctx, SymTab_t *symtab);
void codegen_function_locals(ListNode_t *, CodeGenContext *ctx);
ListNode_t *codegen_vect_reg(ListNode_t *, int);

void codegen_subprograms(ListNode_t *, CodeGenContext *ctx, SymTab_t *symtab);
void codegen_procedure(Tree_t *, CodeGenContext *ctx, SymTab_t *symtab);
void codegen_function(Tree_t *, CodeGenContext *ctx, SymTab_t *symtab);
ListNode_t *codegen_subprogram_arguments(ListNode_t *, ListNode_t *, CodeGenContext *ctx);


#endif
