#define _GNU_SOURCE
/*
 * GPC - Gwinn Pascal Compiler (cparser integration)
 */

#include <assert.h>
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif
#ifndef _WIN32
#include <unistd.h>
#endif
#include <ctype.h>
#include <stdbool.h>

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

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#ifndef R_OK
#define R_OK 4
#endif
#define access _access
#endif

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

#ifdef _WIN32
static char* realpath(const char* path, char* resolved_path)
{
    DWORD len = GetFullPathNameA(path, PATH_MAX, resolved_path, NULL);
    if (len == 0 || len >= PATH_MAX)
        return NULL;
    return resolved_path;
}
#endif

static ssize_t get_executable_path(char *buffer, size_t size, const char *argv0)
{
#if defined(__linux__) || defined(__unix__) || defined(__APPLE__)
    ssize_t len = readlink("/proc/self/exe", buffer, size - 1);
    if (len >= 0)
    {
        buffer[len] = '\0';
        return len;
    }
#elif defined(_WIN32)
    DWORD len = GetModuleFileNameA(NULL, buffer, (DWORD)size);
    if (len > 0 && len < size)
        return (ssize_t)len;
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

static void mark_stdlib_var_params(ListNode_t *subprograms)
{
    for (ListNode_t *node = subprograms; node != NULL; node = node->next)
    {
        if (node->type != LIST_TREE || node->cur == NULL)
            continue;

        Tree_t *sub = (Tree_t *)node->cur;
        if (sub->type != TREE_SUBPROGRAM)
            continue;

        if (sub->tree_data.subprogram_data.sub_type != TREE_SUBPROGRAM_PROC ||
            sub->tree_data.subprogram_data.id == NULL)
            continue;

        if (strcasecmp(sub->tree_data.subprogram_data.id, "read") != 0)
            continue;

        ListNode_t *params = sub->tree_data.subprogram_data.args_var;
        while (params != NULL)
        {
            if (params->type == LIST_TREE && params->cur != NULL)
            {
                Tree_t *param_decl = (Tree_t *)params->cur;
                if (param_decl->type == TREE_VAR_DECL)
                    param_decl->tree_data.var_decl_data.is_var_param = 1;
            }
            params = params->next;
        }
    }
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

static const char *skip_utf8_bom(const char *cursor, const char *end)
{
    if (end - cursor >= 3 &&
        (unsigned char)cursor[0] == 0xEF &&
        (unsigned char)cursor[1] == 0xBB &&
        (unsigned char)cursor[2] == 0xBF)
    {
        return cursor + 3;
    }
    return cursor;
}

static const char *skip_whitespace_and_comments(const char *cursor, const char *end)
{
    while (cursor < end)
    {
        unsigned char ch = (unsigned char)*cursor;
        if (isspace(ch))
        {
            ++cursor;
            continue;
        }

        if (ch == '{')
        {
            ++cursor;
            while (cursor < end && *cursor != '}')
                ++cursor;
            if (cursor < end)
                ++cursor;
            continue;
        }

        if (ch == '(' && (cursor + 1) < end && cursor[1] == '*')
        {
            cursor += 2;
            while ((cursor + 1) < end && !(cursor[0] == '*' && cursor[1] == ')'))
                ++cursor;
            if ((cursor + 1) < end)
                cursor += 2;
            else
                cursor = end;
            continue;
        }

        if (ch == '/' && (cursor + 1) < end && cursor[1] == '/')
        {
            cursor += 2;
            while (cursor < end && *cursor != '\n')
                ++cursor;
            continue;
        }

        break;
    }

    return cursor;
}

static bool buffer_starts_with_unit(const char *buffer, size_t length)
{
    const char *cursor = buffer;
    const char *end = buffer + length;
    cursor = skip_utf8_bom(cursor, end);
    cursor = skip_whitespace_and_comments(cursor, end);

    const char *keyword = "unit";
    size_t keyword_len = strlen(keyword);
    if ((size_t)(end - cursor) < keyword_len)
        return false;

    if (strncasecmp(cursor, keyword, keyword_len) != 0)
        return false;

    const char *after = cursor + keyword_len;
    if (after < end && (isalnum((unsigned char)*after) || *after == '_'))
        return false;

    return true;
}

static Tree_t *parse_pascal_file(const char *path)
{
    size_t length = 0;
    char *buffer = read_file(path, &length);
    if (buffer == NULL)
        return NULL;

    combinator_t *parser = new_combinator();
    bool is_unit = buffer_starts_with_unit(buffer, length);
    if (is_unit)
        init_pascal_unit_parser(&parser);
    else
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
            free(set->names[i]);
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
        if (strcmp(set->names[i], name) == 0)
            return true;

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

    Tree_t *unit_tree = parse_pascal_file(path);
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
    UnitSet visited_units;
    unit_set_init(&visited_units);
    if (prelude_subs != NULL)
        mark_stdlib_var_params(prelude_subs);
    if (prelude_subs != NULL)
    {
        ListNode_t *last = prelude_subs;
        while (last->next != NULL)
            last = last->next;
        last->next = user_subs;
        user_tree->tree_data.program_data.subprograms = prelude_subs;
        prelude_tree->tree_data.program_data.subprograms = NULL;
    }

    load_units_from_list(user_tree, prelude_tree->tree_data.program_data.uses_units, &visited_units);
    load_units_from_list(user_tree, user_tree->tree_data.program_data.uses_units, &visited_units);

    unit_set_destroy(&visited_units);

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
