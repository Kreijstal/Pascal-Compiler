/*
 * GPC - Gwinn Pascal Compiler (cparser integration)
 */

#include <assert.h>
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "pascal_declaration.h"

#include "flags.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/ParseTree/from_cparser.h"
#include "Parser/SemanticCheck/SemCheck.h"
#include "CodeGenerator/Intel_x86-64/codegen.h"
#include "stacktrace.h"

extern ast_t *ast_nil;

/* Legacy globals referenced in other components */
Tree_t *parse_tree = NULL;
int num_args_alloced = 0;
int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;

static void print_usage(const char *prog_name)
{
    fprintf(stderr, "Usage: %s <input.p> <output.s> [flags]\n", prog_name);
    fprintf(stderr, "  Compiles Pascal source to x86-64 assembly\n");
}

static int file_is_readable(const char *path)
{
    return path != NULL && access(path, R_OK) == 0;
}

static char *duplicate_path(const char *path)
{
    if (path == NULL)
        return NULL;

    char *dup = strdup(path);
    if (dup == NULL)
        fprintf(stderr, "Error: Memory allocation failed while duplicating path '%s'\n", path);

    return dup;
}

static ssize_t get_executable_path(char *buffer, size_t size, const char *argv0)
{
#if defined(__linux__) || defined(__unix__) || defined(__APPLE__)
    ssize_t len = readlink("/proc/self/exe", buffer, size - 1);
    if (len >= 0)
    {
        buffer[len] = '\0';
        return len;
    }
#endif

    if (argv0 != NULL)
    {
        char resolved[PATH_MAX];
        if (realpath(argv0, resolved) != NULL)
        {
            strncpy(buffer, resolved, size);
            buffer[size - 1] = '\0';
            return (ssize_t)strlen(buffer);
        }
    }

    return -1;
}

static char *resolve_stdlib_path(const char *argv0)
{
    const char *env = getenv("GPC_STDLIB");
    if (file_is_readable(env))
        return duplicate_path(env);

    if (file_is_readable("GPC/stdlib.p"))
        return duplicate_path("GPC/stdlib.p");

    const char *source_root = getenv("MESON_SOURCE_ROOT");
    if (source_root != NULL)
    {
        char candidate[PATH_MAX];
        snprintf(candidate, sizeof(candidate), "%s/GPC/stdlib.p", source_root);
        if (file_is_readable(candidate))
            return duplicate_path(candidate);
    }

    char exe_path[PATH_MAX];
    ssize_t len = get_executable_path(exe_path, sizeof(exe_path), argv0);
    if (len > 0)
    {
        char exe_dir[PATH_MAX];
        strncpy(exe_dir, exe_path, sizeof(exe_dir));
        exe_dir[sizeof(exe_dir) - 1] = '\0';

        char *dir = dirname(exe_dir);
        if (dir != NULL)
        {
            const char *relative_candidates[] = {
                "../../GPC/stdlib.p",
                "../GPC/stdlib.p",
                "../stdlib.p",
            };

            for (size_t i = 0; i < sizeof(relative_candidates) / sizeof(relative_candidates[0]); ++i)
            {
                char candidate[PATH_MAX];
                snprintf(candidate, sizeof(candidate), "%s/%s", dir, relative_candidates[i]);
                if (file_is_readable(candidate))
                    return duplicate_path(candidate);
            }
        }
    }

    return NULL;
}

static void set_flags(char **optional_args, int count)
{
    int i = 0;
    while (count > 0)
    {
        if (strcmp(optional_args[i], "-non-local") == 0)
        {
            fprintf(stderr, "Non-local codegen support enabled\n");
            fprintf(stderr, "WARNING: Non-local is still in development and is very buggy!\n\n");
            set_nonlocal_flag();
        }
        else if (strcmp(optional_args[i], "-O1") == 0)
        {
            fprintf(stderr, "O1 optimizations enabled!\n\n");
            set_o1_flag();
        }
        else if (strcmp(optional_args[i], "-O2") == 0)
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

static char *read_file(const char *path, size_t *out_len)
{
    FILE *f = fopen(path, "rb");
    if (f == NULL)
    {
        fprintf(stderr, "Error: Cannot open file '%s'\n", path);
        return NULL;
    }

    if (fseek(f, 0, SEEK_END) != 0)
    {
        fprintf(stderr, "Error: Failed to seek file '%s'\n", path);
        fclose(f);
        return NULL;
    }

    long size = ftell(f);
    if (size < 0)
    {
        fprintf(stderr, "Error: Failed to determine file size for '%s'\n", path);
        fclose(f);
        return NULL;
    }

    if (fseek(f, 0, SEEK_SET) != 0)
    {
        fprintf(stderr, "Error: Failed to rewind file '%s'\n", path);
        fclose(f);
        return NULL;
    }

    char *buffer = malloc((size_t)size + 1);
    if (buffer == NULL)
    {
        fprintf(stderr, "Error: Memory allocation failed while reading '%s'\n", path);
        fclose(f);
        return NULL;
    }

    size_t read = fread(buffer, 1, (size_t)size, f);
    fclose(f);

    buffer[read] = '\0';
    if (out_len != NULL)
        *out_len = read;
    return buffer;
}

static void report_parse_error(const char *path, ParseError *err)
{
    if (err == NULL)
        return;

    fprintf(stderr, "Parse error in %s:\n", path);
    fprintf(stderr, "  Line %d, Column %d: %s\n",
            err->line, err->col,
            err->message ? err->message : "unknown error");
    if (err->unexpected)
        fprintf(stderr, "  Unexpected: %s\n", err->unexpected);
}

static Tree_t *parse_pascal_file(const char *path)
{
    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return NULL;

    combinator_t *parser = new_combinator();
    init_pascal_complete_program_parser(&parser);

    input_t *input = new_input();
    input->buffer = buffer;
    input->length = (int)length;

    if (ast_nil == NULL)
    {
        ast_nil = new_ast();
        ast_nil->typ = PASCAL_T_NONE;
    }

    file_to_parse = (char *)path;

    ParseResult result = parse(input, parser);
    Tree_t *tree = NULL;
    if (!result.is_success)
    {
        report_parse_error(path, result.value.error);
        if (result.value.error != NULL)
            free_error(result.value.error);
    }
    else
    {
        if (input->start < input->length)
        {
            fprintf(stderr,
                    "Warning: Parser did not consume entire input for %s (at position %d of %d)\n",
                    path, input->start, input->length);
        }

        tree = tree_from_pascal_ast(result.value.ast);
        if (tree == NULL)
        {
            fprintf(stderr, "Error: Failed to convert AST for '%s' to legacy parse tree.\n", path);
        }
        free_ast(result.value.ast);
    }

    free(buffer);
    free(input);
    file_to_parse = NULL;
    free_combinator(parser);
    return tree;
}

int main(int argc, char **argv)
{
    install_stack_trace_handler();

    if (argc < 3)
    {
        print_usage(argv[0]);
        return 1;
    }

    const char *input_file = argv[1];
    const char *output_file = argv[2];

    int optional_count = argc - 3;
    if (optional_count > 0)
        set_flags(argv + 3, optional_count);

    char *stdlib_path = resolve_stdlib_path(argv[0]);
    if (stdlib_path == NULL)
    {
        fprintf(stderr, "Error: Unable to locate stdlib.p. Set GPC_STDLIB or run from the project root.\n");
        return 1;
    }

    Tree_t *prelude_tree = parse_pascal_file(stdlib_path);
    if (prelude_tree == NULL)
    {
        free(stdlib_path);
        return 1;
    }

    Tree_t *user_tree = parse_pascal_file(input_file);
    if (user_tree == NULL)
    {
        destroy_tree(prelude_tree);
        free(stdlib_path);
        return 1;
    }

    ListNode_t *prelude_subs = prelude_tree->tree_data.program_data.subprograms;
    ListNode_t *user_subs = user_tree->tree_data.program_data.subprograms;
    if (prelude_subs != NULL)
    {
        ListNode_t *last = prelude_subs;
        while (last->next != NULL)
            last = last->next;
        last->next = user_subs;
        user_tree->tree_data.program_data.subprograms = prelude_subs;
        prelude_tree->tree_data.program_data.subprograms = NULL;
    }

    int sem_result = 0;
    SymTab_t *symtab = start_semcheck(user_tree, &sem_result);
    int exit_code = 0;

    if (sem_result <= 0)
    {
        fprintf(stderr, "Generating code to file: %s\n", output_file);

        CodeGenContext ctx;
        ctx.output_file = fopen(output_file, "w");
        if (ctx.output_file == NULL)
        {
            fprintf(stderr, "ERROR: Failed to open output file: %s\n", output_file);
            DestroySymTab(symtab);
            destroy_tree(prelude_tree);
            destroy_tree(user_tree);
            return 1;
        }
        ctx.label_counter = 1;
        ctx.write_label_counter = 1;

        codegen(user_tree, input_file, &ctx, symtab);
        fclose(ctx.output_file);
    }
    else
    {
        fprintf(stderr, "Semantic analysis failed with %d error(s).\n", sem_result);
        exit_code = sem_result;
    }

    DestroySymTab(symtab);
    destroy_tree(prelude_tree);
    destroy_tree(user_tree);
    free(stdlib_path);

    if (ast_nil != NULL)
    {
        free(ast_nil);
        ast_nil = NULL;
    }

    if (sem_result > 0)
        return exit_code > 0 ? exit_code : 1;

    return exit_code;
}
