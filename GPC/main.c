/* Damon Gwinn */
/* Where it all begins */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include "flags.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/ParsePascal.h"

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
    if (name == NULL)
        return NULL;

    size_t len = strlen(name);
    char *copy = (char *)malloc(len + 1);
    if (copy == NULL)
        return NULL;

    for (size_t i = 0; i < len; ++i)
        copy[i] = (char)tolower((unsigned char)name[i]);
    copy[len] = '\0';
    return copy;
}

static char *build_unit_path(const char *unit_name)
{
    const char *prefix = "GPC/Units/";
    const char *suffix = ".p";
    size_t len = strlen(prefix) + strlen(unit_name) + strlen(suffix) + 1;
    char *path = (char *)malloc(len);
    if (path == NULL)
        return NULL;

    snprintf(path, len, "%s%s%s", prefix, unit_name, suffix);
    return path;
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

static void merge_unit_into_program(Tree_t *program, Tree_t *unit_tree)
{
    if (program == NULL || unit_tree == NULL)
        return;

    program->tree_data.program_data.type_declaration =
        ConcatList(program->tree_data.program_data.type_declaration,
                   unit_tree->tree_data.unit_data.interface_type_decls);
    unit_tree->tree_data.unit_data.interface_type_decls = NULL;

    program->tree_data.program_data.var_declaration =
        ConcatList(program->tree_data.program_data.var_declaration,
                   unit_tree->tree_data.unit_data.interface_var_decls);
    unit_tree->tree_data.unit_data.interface_var_decls = NULL;

    program->tree_data.program_data.type_declaration =
        ConcatList(program->tree_data.program_data.type_declaration,
                   unit_tree->tree_data.unit_data.implementation_type_decls);
    unit_tree->tree_data.unit_data.implementation_type_decls = NULL;

    program->tree_data.program_data.var_declaration =
        ConcatList(program->tree_data.program_data.var_declaration,
                   unit_tree->tree_data.unit_data.implementation_var_decls);
    unit_tree->tree_data.unit_data.implementation_var_decls = NULL;

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
            return 0;
        }

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

        int sem_result;
        SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
        if(sem_result == 0)
        {
            fprintf(stderr, "Generating code to file: %s\n", argv[2]);

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
        else if(strcmp(optional_args[i], "-parse-only") == 0 || strcmp(optional_args[i], "--parse-only") == 0)
        {
            fprintf(stderr, "Parse-only mode enabled.\n\n");
            set_parse_only_flag();
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
