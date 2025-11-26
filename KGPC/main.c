/* Damon Gwinn */
/* Where it all begins */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#ifndef _WIN32
#include <strings.h>
#include <unistd.h>
#else
#define strcasecmp _stricmp
#include <io.h>
#ifndef R_OK
#define R_OK 4
#endif
#define access _access
#endif
#include <ctype.h>
#include <stdbool.h>
#include "flags.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/ParsePascal.h"
#include "unit_paths.h"

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

static UnitSearchPaths g_unit_paths;

typedef struct
{
    char **names;
    size_t count;
    size_t capacity;
} UnitSet;

static void unit_set_init(UnitSet *set)
{
    set->names = NULL;
    set->count = 0;
    set->capacity = 0;
}

static void unit_set_destroy(UnitSet *set)
{
    if (set->names != NULL)
    {
        for (size_t i = 0; i < set->count; ++i)
        {
            free(set->names[i]);
        }
        free(set->names);
    }
    set->names = NULL;
    set->count = 0;
    set->capacity = 0;
}

static bool unit_set_contains(const UnitSet *set, const char *name)
{
    if (name == NULL)
        return true;

    for (size_t i = 0; i < set->count; ++i)
    {
        if (strcmp(set->names[i], name) == 0)
            return true;
    }
    return false;
}

static bool unit_set_add(UnitSet *set, char *name)
{
    if (name == NULL)
        return false;

    if (unit_set_contains(set, name))
    {
        free(name);
        return false;
    }

    if (set->count == set->capacity)
    {
        size_t new_capacity = set->capacity == 0 ? 4 : set->capacity * 2;
        char **new_names = (char **)realloc(set->names, new_capacity * sizeof(char *));
        if (new_names == NULL)
        {
            free(name);
            return false;
        }
        set->names = new_names;
        set->capacity = new_capacity;
    }

    set->names[set->count++] = name;
    return true;
}

static char *lowercase_copy(const char *name)
{
    return unit_search_paths_normalize_name(name);
}

static char *build_unit_path(const char *unit_name)
{
    return unit_search_paths_resolve(&g_unit_paths, unit_name);
}

static void append_initialization_statement(Tree_t *program, struct Statement *init_stmt)
{
    if (program == NULL || init_stmt == NULL)
        return;

    struct Statement *body = program->tree_data.program_data.body_statement;
    if (body == NULL || body->type != STMT_COMPOUND_STATEMENT)
    {
        destroy_stmt(init_stmt);
        return;
    }

    if (init_stmt->type != STMT_COMPOUND_STATEMENT)
    {
        destroy_stmt(init_stmt);
        return;
    }

    ListNode_t *init_list = init_stmt->stmt_data.compound_statement;
    if (init_list == NULL)
    {
        destroy_stmt(init_stmt);
        return;
    }

    if (body->stmt_data.compound_statement == NULL)
        body->stmt_data.compound_statement = init_list;
    else
        ConcatList(body->stmt_data.compound_statement, init_list);

    init_stmt->stmt_data.compound_statement = NULL;
    destroy_stmt(init_stmt);
}

static void mark_unit_subprograms(ListNode_t *sub_list)
{
    ListNode_t *node = sub_list;
    while (node != NULL)
    {
        if (node->type == LIST_TREE && node->cur != NULL)
        {
            Tree_t *sub = (Tree_t *)node->cur;
            if (sub->type == TREE_SUBPROGRAM)
                sub->tree_data.subprogram_data.defined_in_unit = 1;
        }
        node = node->next;
    }
}

static void merge_unit_into_program(Tree_t *program, Tree_t *unit_tree)
{
    if (program == NULL || unit_tree == NULL)
        return;

    program->tree_data.program_data.type_declaration =
        ConcatList(program->tree_data.program_data.type_declaration,
                   unit_tree->tree_data.unit_data.interface_type_decls);
    unit_tree->tree_data.unit_data.interface_type_decls = NULL;

    program->tree_data.program_data.const_declaration =
        ConcatList(program->tree_data.program_data.const_declaration,
                   unit_tree->tree_data.unit_data.interface_const_decls);
    unit_tree->tree_data.unit_data.interface_const_decls = NULL;

    program->tree_data.program_data.var_declaration =
        ConcatList(program->tree_data.program_data.var_declaration,
                   unit_tree->tree_data.unit_data.interface_var_decls);
    unit_tree->tree_data.unit_data.interface_var_decls = NULL;

    program->tree_data.program_data.type_declaration =
        ConcatList(program->tree_data.program_data.type_declaration,
                   unit_tree->tree_data.unit_data.implementation_type_decls);
    unit_tree->tree_data.unit_data.implementation_type_decls = NULL;

    program->tree_data.program_data.const_declaration =
        ConcatList(program->tree_data.program_data.const_declaration,
                   unit_tree->tree_data.unit_data.implementation_const_decls);
    unit_tree->tree_data.unit_data.implementation_const_decls = NULL;

    program->tree_data.program_data.var_declaration =
        ConcatList(program->tree_data.program_data.var_declaration,
                   unit_tree->tree_data.unit_data.implementation_var_decls);
    unit_tree->tree_data.unit_data.implementation_var_decls = NULL;

    mark_unit_subprograms(unit_tree->tree_data.unit_data.subprograms);
    program->tree_data.program_data.subprograms =
        ConcatList(program->tree_data.program_data.subprograms,
                   unit_tree->tree_data.unit_data.subprograms);
    unit_tree->tree_data.unit_data.subprograms = NULL;

    append_initialization_statement(program, unit_tree->tree_data.unit_data.initialization);
    unit_tree->tree_data.unit_data.initialization = NULL;
}

static void load_units_from_list(Tree_t *program, ListNode_t *uses, UnitSet *visited);

static void load_unit(Tree_t *program, const char *unit_name, UnitSet *visited)
{
    if (unit_name == NULL || visited == NULL)
        return;

    char *normalized = lowercase_copy(unit_name);
    if (normalized == NULL)
        return;

    if (!unit_set_add(visited, normalized))
        return;

    char *path = build_unit_path(normalized);
    if (path == NULL)
        return;

    Tree_t *unit_tree = ParsePascalOnly(path);
    free(path);
    if (unit_tree == NULL)
    {
        fprintf(stderr, "ERROR: Failed to load unit '%s'.\n", unit_name);
        exit(1);
    }

    if (unit_tree->type != TREE_UNIT)
    {
        fprintf(stderr, "ERROR: %s is not a Pascal unit.\n", unit_name);
        destroy_tree(unit_tree);
        exit(1);
    }

    load_units_from_list(program, unit_tree->tree_data.unit_data.interface_uses, visited);
    load_units_from_list(program, unit_tree->tree_data.unit_data.implementation_uses, visited);

    merge_unit_into_program(program, unit_tree);
    destroy_tree(unit_tree);
}

static void load_units_from_list(Tree_t *program, ListNode_t *uses, UnitSet *visited)
{
    ListNode_t *cur = uses;
    while (cur != NULL)
    {
        if (cur->type == LIST_STRING && cur->cur != NULL)
            load_unit(program, (const char *)cur->cur, visited);
        cur = cur->next;
    }
}

/* Global variable definitions */
Tree_t *parse_tree = NULL;
int num_args_alloced = 0;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
#include "Parser/ParsePascal.h"
#include "CodeGenerator/Intel_x86-64/codegen.h"

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
    int exit_status = 0;

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

    unit_search_paths_init(&g_unit_paths);
    unit_search_paths_set_user(&g_unit_paths, argv[1]);

    file_to_parse = "KGPC/stdlib.p";
    unit_search_paths_set_vendor(&g_unit_paths, file_to_parse);
    prelude_tree = ParsePascalOnly("KGPC/stdlib.p");
    file_to_parse = argv[1];
    user_tree = ParsePascalOnly(argv[1]);

    if(prelude_tree != NULL && user_tree != NULL)
    {
        UnitSet visited_units;
        unit_set_init(&visited_units);

        ListNode_t *prelude_subs = prelude_tree->tree_data.program_data.subprograms;
        ListNode_t *user_subs = user_tree->tree_data.program_data.subprograms;

        if (prelude_subs == NULL)
        {
            // user_tree is already correct
        }
        else
        {
            /* Mark prelude (stdlib.p) subprograms as library procedures so they don't
             * incorrectly get static links when merged into user programs */
            mark_unit_subprograms(prelude_subs);
            
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

        load_units_from_list(user_tree, prelude_tree->tree_data.program_data.uses_units, &visited_units);
        load_units_from_list(user_tree, user_tree->tree_data.program_data.uses_units, &visited_units);

        unit_set_destroy(&visited_units);

        if (parse_only_flag())
        {
            FILE *out = fopen(argv[2], "w");
            if (out == NULL)
            {
                fprintf(stderr, "ERROR: Failed to open output file: %s\n", argv[2]);
                exit(1);
            }
            fprintf(stderr, "Parse-only mode: skipping semantic analysis and code generation.\n");
            fprintf(out, "; parse-only mode: no code generated\n");
            fclose(out);

            destroy_tree(prelude_tree);
            destroy_tree(user_tree);
            unit_search_paths_destroy(&g_unit_paths);
            return 0;
        }

        int sem_result;
        SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
        if(sem_result == 0)
        {
            fprintf(stderr, "Generating code to file: %s\n", argv[2]);

            CodeGenContext ctx;
            memset(&ctx, 0, sizeof(ctx));
            ctx.output_file = fopen(argv[2], "w");
            if (ctx.output_file == NULL)
            {
                fprintf(stderr, "ERROR: Failed to open output file: %s\n", argv[2]);
                exit(1);
            }
            ctx.label_counter = 1;
            ctx.write_label_counter = 1;
            ctx.symtab = symtab;
            ctx.target_abi = current_target_abi();
            ctx.had_error = 0;
            ctx.loop_exit_labels = NULL;
            ctx.loop_depth = 0;
            ctx.loop_capacity = 0;

            /* Mark which functions are actually used */
            /* Temporarily disabled due to segfaults in some test cases */
            /*
            extern void mark_used_functions(Tree_t *program, SymTab_t *symtab);
            fprintf(stderr, "DEBUG main.c: About to call mark_used_functions\n");
            mark_used_functions(user_tree, symtab);
            fprintf(stderr, "DEBUG main.c: Returned from mark_used_functions\n");
            */

            codegen(user_tree, argv[1], &ctx, symtab);

            int codegen_failed = codegen_had_error(&ctx);
            fclose(ctx.output_file);
            if (codegen_failed)
            {
                fprintf(stderr, "Code generation failed; removing incomplete output file.\n");
                remove(argv[2]);
                exit_status = 1;
            }
        }
        else if (sem_result > 0)
        {
            exit_status = sem_result;
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
    unit_search_paths_destroy(&g_unit_paths);
    return exit_status;
}

void set_flags(char **optional_args, int count)
{
    int i;

    assert(optional_args != NULL);

    i = 0;
    while(count > 0)
    {
        assert(optional_args[i] != NULL);
        const char *arg = optional_args[i];
        if(strcmp(arg, "-non-local") == 0)
        {
            fprintf(stderr, "Non-local codegen support enabled\n");
            fprintf(stderr, "WARNING: Non-local is still in development and is very buggy!\n\n");
            set_nonlocal_flag();
        }
        else if(strcmp(arg, "-O1") == 0)
        {
            fprintf(stderr, "O1 optimizations enabled!\n\n");
            set_o1_flag();
        }
        else if(strcmp(arg, "-O2") == 0)
        {
            fprintf(stderr, "O2 optimizations enabled!\n\n");
            set_o2_flag();
        }
        else if(strcmp(arg, "-parse-only") == 0 || strcmp(arg, "--parse-only") == 0)
        {
            fprintf(stderr, "Parse-only mode enabled.\n\n");
            set_parse_only_flag();
        }
        else if(strcmp(arg, "--target-windows") == 0 || strcmp(arg, "-target-windows") == 0 || strcmp(arg, "--windows-abi") == 0)
        {
            fprintf(stderr, "Target ABI: Windows x64\n\n");
            set_target_windows_flag();
        }
        else if(strcmp(arg, "--target-sysv") == 0 || strcmp(arg, "-target-sysv") == 0 || strcmp(arg, "--sysv-abi") == 0)
        {
            fprintf(stderr, "Target ABI: System V AMD64\n\n");
            set_target_sysv_flag();
        }
        else if((strcmp(arg, "--target") == 0 || strcmp(arg, "-target") == 0) && count > 1)
        {
            const char *value = optional_args[i + 1];
            if(strcasecmp(value, "windows") == 0 || strcasecmp(value, "win64") == 0)
            {
                fprintf(stderr, "Target ABI: Windows x64\n\n");
                set_target_windows_flag();
            }
            else if(strcasecmp(value, "sysv") == 0 || strcasecmp(value, "systemv") == 0 || strcasecmp(value, "linux") == 0)
            {
                fprintf(stderr, "Target ABI: System V AMD64\n\n");
                set_target_sysv_flag();
            }
            else
            {
                fprintf(stderr, "ERROR: Unknown target ABI '%s'\n", value);
                exit(1);
            }
            --count;
            ++i;
        }
        else if(strncmp(arg, "--target=", 9) == 0)
        {
            const char *value = arg + 9;
            if(strcasecmp(value, "windows") == 0 || strcasecmp(value, "win64") == 0)
            {
                fprintf(stderr, "Target ABI: Windows x64\n\n");
                set_target_windows_flag();
            }
            else if(strcasecmp(value, "sysv") == 0 || strcasecmp(value, "systemv") == 0 || strcasecmp(value, "linux") == 0)
            {
                fprintf(stderr, "Target ABI: System V AMD64\n\n");
                set_target_sysv_flag();
            }
            else
            {
                fprintf(stderr, "ERROR: Unknown target ABI '%s'\n", value);
                exit(1);
            }
        }
        else if(strcmp(arg, "--asm-debug") == 0 || strcmp(arg, "--asm-debug-comments") == 0)
        {
            fprintf(stderr, "Assembly debug comments enabled.\n\n");
            set_asm_debug_flag();
        }
        else
        {
            fprintf(stderr, "ERROR: Unrecognized flag: %s\n", arg);
            exit(1);
        }

        --count;
        ++i;
    }
}
