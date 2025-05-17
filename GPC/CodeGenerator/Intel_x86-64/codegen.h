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
        - RBX
        - RDI

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

        The write bultin currently only takes integer types and has the
            label LC0 with %d\n. The function call is "call printf"

        The read bultin currently only reads integer types and has the
            label LC1 with "%d". The function call is "call __isoc99_scanf"
            - NOTE: Using c99 gcc standard here for simplicity

    TEMPORARIES:
        Temporaries are allocated as needed and reused once freed for efficiency
*/

#ifndef CODE_GEN_H
#define CODE_GEN_H

/* Platform detection */
#ifdef __linux__
#define PLATFORM_LINUX 1
#define PLATFORM_WINDOWS 0
#elif defined(_WIN32) || defined(_WIN64)
#define PLATFORM_LINUX 0
#define PLATFORM_WINDOWS 1
#else
#error "Unsupported platform"
#endif

#define DEBUG_CODEGEN
#define MAX_ARGS 3
#define REQUIRED_OFFSET 16

/* Linux-specific defines */
#if PLATFORM_LINUX
#define WRITE_SYSCALL_NUM 1
#define STDOUT_FD 1
#define EXIT_SYSCALL_NUM 60
#define SCANF_CALL "__isoc99_scanf"
#define SCANF_REGISTER ".LC1(%rip)"
#elif defined(_WIN32) || defined(_WIN64)
#define PLATFORM_WINDOWS 1
#define SCANF_CALL "__isoc99_scanf@PLT"
#define SCANF_REGISTER ".LC1"
#else
#error "Unsupported platform"
#endif

#define NORMAL_JMP -1

#include <stdlib.h>
#include <stdio.h>
#include "stackmng/stackmng.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"

/* For creating labels to jump to */
/* Please initialize to 1 */
extern int label_counter;
extern int write_label_counter;

/* Generates a label */
void gen_label(char *buf, int buf_len);

/* Escapes string for assembly output */
void escape_string(char *dest, const char *src, size_t dest_size);

/* This is the entry function */
void codegen(Tree_t *, char *input_file_name, char *output_file_name);

ListNode_t *add_inst(ListNode_t *, char *);

void codegen_program_header(char *, FILE *);;
void codegen_program_footer(FILE *);
void codegen_main(char *prgm_name, FILE *o_file);
void codegen_stack_space(FILE *);
void codegen_inst_list(ListNode_t *, FILE *);

char * codegen_program(Tree_t *, FILE *);
void codegen_function_locals(ListNode_t *, FILE *);
ListNode_t *codegen_vect_reg(ListNode_t *, int);

void codegen_subprograms(ListNode_t *, FILE *);
void codegen_procedure(Tree_t *, FILE *);
void codegen_function(Tree_t *, FILE *);
ListNode_t *codegen_subprogram_arguments(ListNode_t *, ListNode_t *, FILE *);

ListNode_t *codegen_stmt(struct Statement *, ListNode_t *,FILE *);
ListNode_t *codegen_compound_stmt(struct Statement *, ListNode_t *, FILE *);
ListNode_t *codegen_var_assignment(struct Statement *, ListNode_t *, FILE *);
ListNode_t *codegen_proc_call(struct Statement *, ListNode_t *, FILE *);
ListNode_t *codegen_if_then(struct Statement *, ListNode_t *, FILE *);
ListNode_t *codegen_while(struct Statement *, ListNode_t *, FILE *);
ListNode_t *codegen_for(struct Statement *, ListNode_t *, FILE *);

ListNode_t *codegen_pass_arguments(ListNode_t *, ListNode_t *, FILE *);
ListNode_t *codegen_get_nonlocal(ListNode_t *, char *, int *);

ListNode_t *codegen_simple_relop(struct Expression *, ListNode_t *,
    FILE *, int *);

ListNode_t *codegen_expr(struct Expression *, ListNode_t *, FILE *);
ListNode_t *codegen_builtin_write(ListNode_t *, ListNode_t *, FILE *);
ListNode_t *codegen_builtin_writeln(ListNode_t *, ListNode_t *, FILE *);
ListNode_t *codegen_builtin_read(ListNode_t *, ListNode_t *, FILE *);
ListNode_t *codegen_args(ListNode_t*, ListNode_t *, FILE *);

/* (DEPRECATED) */
ListNode_t *codegen_expr_varid(struct Expression *, ListNode_t *, FILE *);
ListNode_t *codegen_expr_inum(struct Expression *, ListNode_t *, FILE *);

#endif
