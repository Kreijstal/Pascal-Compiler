#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "debug_deserializer.h"
#include "flags.h"
#include "CodeGenerator/Intel_x86-64/codegen.h"
#include "CodeGenerator/Intel_x86-64/codegen_expression.h"
#include "CodeGenerator/Intel_x86-64/expr_tree/expr_tree.h"
#include "Parser/List/List.h"
#include "Parser/SemanticCheck/SymTab/SymTab.h"
#include "CodeGenerator/Intel_x86-64/stackmng/stackmng.h"

// Global variables that are defined in main.c
Tree_t *parse_tree = NULL;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
int num_args_alloced = 0;

int main() {
    FILE *fp = fopen("serializer_log.txt", "r");
    if (fp == NULL) {
        perror("Error opening serializer_log.txt");
        return 1;
    }

    // Find the call that crashes
    char buffer[256];
    while (fgets(buffer, sizeof(buffer), fp) != NULL) {
        if (strstr(buffer, "4 i") != NULL) {
            break;
        }
    }

    // The file pointer is now at the beginning of the line "4 i"
    // We need to rewind to the beginning of that line
    fseek(fp, -strlen(buffer), SEEK_CUR);


    struct Expression *expr = deserialize_expression(fp);
    if (expr == NULL) {
        fprintf(stderr, "Failed to deserialize expression\n");
        return 1;
    }

    printf("Deserialized expression type: %d\n", expr->type);

    // Create dummy context and symtab
    CodeGenContext *ctx = (CodeGenContext *)calloc(1, sizeof(CodeGenContext));
   ctx->target_abi = current_target_abi();
    init_stackmng();
    push_stackscope();
    add_l_x("i", DOUBLEWORD);

    printf("Finding label 'i'...\n");
    StackNode_t *var_node = find_label("i");
    if (var_node == NULL) {
        fprintf(stderr, "Could not find label 'i'\n");
    } else {
        printf("Found label 'i' at offset %d\n", var_node->offset);
    }

    printf("Calling codegen_pass_arguments...\n");

    ListNode_t *args = CreateListNode(expr, LIST_EXPR);
    codegen_pass_arguments(args, NULL, ctx, NULL);

    printf("codegen_pass_arguments returned\n");

    printf("Test harness finished.\n");

    return 0;
}
