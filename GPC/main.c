#include <stdio.h>
#include "Parser/ParsePascal.h"
#include "flags.h"
#include "CodeGenerator/new_codegen/flat_ir_generator.h"
#include "CodeGenerator/new_codegen/x86_64_codegen.h"
#include "Parser/List/List.h"
#include "Parser/flat_ast.h"
#include "Parser/SemanticCheck/flat_sem_check.h"

// This is a temporary path for the standard library.
// In the future, this should be located based on the compiler's install location.
#define STDLIB_PATH "tests/cases/stdlib.p"

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Usage: gpc <input file> [output file]\n");
        return 1;
    }

    init_flags();

    fprintf(stderr, "[MAIN] Parsing stdlib...\n"); fflush(stderr);
    FlatNode *stdlib_ast = ParsePascalOnly(STDLIB_PATH);
    if (stdlib_ast == NULL) {
        fprintf(stderr, "FATAL: Could not parse standard library at %s\n", STDLIB_PATH);
        return 1;
    }

    fprintf(stderr, "[MAIN] Parsing user program...\n"); fflush(stderr);
    FlatNode *user_ast = ParsePascal(argv[1]);
    if (user_ast == NULL) {
        fprintf(stderr, "Parsing failed for %s.\n", argv[1]);
        return 1;
    }

    fprintf(stderr, "[MAIN] Merging ASTs...\n"); fflush(stderr);
    if (stdlib_ast->node_type == FL_PROGRAM && user_ast->node_type == FL_PROGRAM) {
        ListNode_t *stdlib_subprograms = stdlib_ast->data.program.subprograms;
        ListNode_t *user_subprograms = user_ast->data.program.subprograms;

        ListNode_t *combined_subprograms = Chain(stdlib_subprograms, user_subprograms);
        user_ast->data.program.subprograms = combined_subprograms;
    } else {
        fprintf(stderr, "FATAL: Root of AST is not a program node. Cannot merge stdlib.\n");
        return 1;
    }
    fprintf(stderr, "[MAIN] Merged ASTs.\n"); fflush(stderr);


    fprintf(stderr, "[MAIN] Running semantic check...\n"); fflush(stderr);
    int sem_errors = sem_check(user_ast);
    if (sem_errors > 0) {
        fprintf(stderr, "[MAIN] Compilation aborted due to %d semantic errors.\n", sem_errors);
        return 1;
    }
    fprintf(stderr, "[MAIN] Semantic check passed.\n"); fflush(stderr);

    FILE *out;
    if (argc > 2) {
        out = fopen(argv[2], "w");
        if (out == NULL) {
            perror("Error opening output file");
            return 1;
        }
    } else {
        out = stdout;
    }

    fprintf(stderr, "[MAIN] Generating IR...\n"); fflush(stderr);
    ListNode_t *ir_list = generate_ir_from_flat_ast(user_ast, NULL); // TODO: Pass symtab
    fprintf(stderr, "[MAIN] Generated IR. Generating code...\n"); fflush(stderr);
    codegen_x86_64(ir_list, out, user_ast->data.program.id);
    fprintf(stderr, "[MAIN] Generated code.\n"); fflush(stderr);

    fclose(out);

    printf("Compiler finished.\n");

    return 0;
}
