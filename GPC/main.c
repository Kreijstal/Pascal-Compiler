/* Damon Gwinn */
/* Where it all begins */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flags.h"
#include "Parser/ParseTree/tree.h"

/* Global variable definitions */
Tree_t *parse_tree = NULL;
int num_args_alloced = 0;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
#include "Parser/ParseTree/tree.h"
#include "Parser/ParsePascal.h"

#ifdef NEW_CODEGEN
#include "CodeGenerator/new_codegen/ir_generator.h"
#include "CodeGenerator/new_codegen/x86_64_codegen.h"
#else
#include "CodeGenerator/Intel_x86-64/codegen.h"
#endif

void set_flags(char **, int);

#include "Parser/SemanticCheck/SemCheck.h"
#include "stacktrace.h"

#include <assert.h>
int main(int argc, char **argv)
{
    assert(argv != NULL);
    install_stack_trace_handler();
    Tree_t *prelude_tree, *user_tree;
    int required_args, args_left;

    required_args = 3;

    if(argc < required_args)
    {
        fprintf(stderr, "USAGE: [exec] [INPUT_FILE] [OUTPUT_FILE] [OPTIONAL_FLAG_1] ...\n");
        exit(1);
    }

    /* Setting flags */
    args_left = argc - required_args;
    if(args_left > 0)
    {
        set_flags(argv + required_args, args_left);
    }

    file_to_parse = "GPC/stdlib.p";
    prelude_tree = ParsePascalOnly("GPC/stdlib.p");
    file_to_parse = argv[1];
    user_tree = ParsePascalOnly(argv[1]);

    if(prelude_tree != NULL && user_tree != NULL)
    {
        ListNode_t *prelude_subs = prelude_tree->tree_data.program_data.subprograms;
        ListNode_t *user_subs = user_tree->tree_data.program_data.subprograms;

        if (prelude_subs == NULL)
        {
            // user_tree is already correct
        }
        else
        {
            ListNode_t *last_node = prelude_subs;
            while(last_node->next != NULL)
            {
                last_node = last_node->next;
            }
            last_node->next = user_subs;
            user_tree->tree_data.program_data.subprograms = prelude_subs;
        }
        // Since we moved the user_subs list, we need to avoid a double free
        if(prelude_tree != NULL)
            prelude_tree->tree_data.program_data.subprograms = NULL;

        int sem_result;
        SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
        if(sem_result == 0)
        {
            fprintf(stderr, "Generating code to file: %s\n", argv[2]);

#ifdef NEW_CODEGEN
            FILE *out = fopen(argv[2], "w");
            if (out == NULL) {
                fprintf(stderr, "ERROR: Failed to open output file: %s\n", argv[2]);
                exit(1);
            }
            ListNode_t *ir_list = generate_ir(user_tree, symtab);
            codegen_x86_64(ir_list, out);
            fclose(out);
#else
            CodeGenContext ctx;
            ctx.output_file = fopen(argv[2], "w");
            if (ctx.output_file == NULL)
            {
                fprintf(stderr, "ERROR: Failed to open output file: %s\n", argv[2]);
                exit(1);
            }
            ctx.label_counter = 1;
            ctx.write_label_counter = 1;
            codegen(user_tree, argv[1], &ctx, symtab);
            fclose(ctx.output_file);
#endif
        }
        DestroySymTab(symtab);

        destroy_tree(prelude_tree);
        destroy_tree(user_tree);
        prelude_tree = NULL;
        user_tree = NULL;
    }
    else
    {
        if(prelude_tree != NULL)
        {
            destroy_tree(prelude_tree);
            prelude_tree = NULL;
        }
        if(user_tree != NULL)
        {
            destroy_tree(user_tree);
            user_tree = NULL;
        }
    }
}

void set_flags(char **optional_args, int count)
{
    int i;

    assert(optional_args != NULL);

    i = 0;
    while(count > 0)
    {
        assert(optional_args[i] != NULL);
        if(strcmp(optional_args[i], "-non-local") == 0)
        {
            fprintf(stderr, "Non-local codegen support enabled\n");
            fprintf(stderr, "WARNING: Non-local is still in development and is very buggy!\n\n");
            set_nonlocal_flag();
        }
        else if(strcmp(optional_args[i], "-O1") == 0)
        {
            fprintf(stderr, "O1 optimizations enabled!\n\n");
            set_o1_flag();
        }
        else if(strcmp(optional_args[i], "-O2") == 0)
        {
            fprintf(stderr, "O2 optimizations enabled!\n\n");
            set_o2_flag();
        }
        else
        {
            fprintf(stderr, "ERROR: Unrecognized flag: %s\n", optional_args[i]);
            exit(1);
        }

        --count;
        ++i;
    }
}
