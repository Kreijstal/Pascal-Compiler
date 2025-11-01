#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <strings.h>
#else
#ifndef strncasecmp
#define strncasecmp _strnicmp
#endif
#endif
#include "pascal_parser.h"
#include "pascal_preprocessor.h"

// Forward declaration
static void print_ast_indented(ast_t* ast, int depth);
static void print_error_with_partial_ast(ParseError* error);

static const char* skip_utf8_bom(const char* cursor, const char* end) {
    if ((size_t)(end - cursor) >= 3 &&
        (unsigned char)cursor[0] == 0xEF &&
        (unsigned char)cursor[1] == 0xBB &&
        (unsigned char)cursor[2] == 0xBF) {
        return cursor + 3;
    }
    return cursor;
}

static const char* skip_whitespace_and_comments(const char* cursor, const char* end) {
    while (cursor < end) {
        unsigned char ch = (unsigned char)*cursor;

        if (isspace(ch)) {
            ++cursor;
            continue;
        }

        if (ch == '{') {
            ++cursor;
            while (cursor < end && *cursor != '}') {
                ++cursor;
            }
            if (cursor < end)
                ++cursor;
            continue;
        }

        if (ch == '(' && (cursor + 1) < end && cursor[1] == '*') {
            cursor += 2;
            while ((cursor + 1) < end && !(cursor[0] == '*' && cursor[1] == ')')) {
                ++cursor;
            }
            if ((cursor + 1) < end)
                cursor += 2;
            else
                cursor = end;
            continue;
        }

        if (ch == '/' && (cursor + 1) < end && cursor[1] == '/') {
            cursor += 2;
            while (cursor < end && *cursor != '\n') {
                ++cursor;
            }
            continue;
        }

        break;
    }

    return cursor;
}

static bool buffer_starts_with_keyword(const char* buffer, size_t length, const char* keyword) {
    const char* cursor = buffer;
    const char* end = buffer + length;
    cursor = skip_utf8_bom(cursor, end);
    cursor = skip_whitespace_and_comments(cursor, end);

    size_t keyword_len = strlen(keyword);
    if ((size_t)(end - cursor) < keyword_len)
        return false;

    if (strncasecmp(cursor, keyword, keyword_len) != 0)
        return false;

    const char* after = cursor + keyword_len;
    if (after < end && (isalnum((unsigned char)*after) || *after == '_'))
        return false;

    return true;
}

// Helper function to print ParseError with partial AST
static void print_error_chain(ParseError* error, int depth) {
    if (error == NULL) {
        return;
    }

    for (int i = 0; i < depth; i++) {
        printf("  ");
    }

    printf("Error at line %d, col %d: ", error->line, error->col);
    if (error->parser_name) {
        printf("In parser '%s': ", error->parser_name);
    }
    printf("%s\n", error->message ? error->message : "<no message>");

    if (error->unexpected) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Unexpected input: \"%s\"\n", error->unexpected);
    }

    if (error->context) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Context:\n");
        const char* ctx = error->context;
        while (*ctx) {
            const char* newline = strchr(ctx, '\n');
            for (int i = 0; i < depth; i++) {
                printf("  ");
            }
            printf("  ");
            if (newline) {
                printf("%.*s\n", (int)(newline - ctx), ctx);
                ctx = newline + 1;
            } else {
                printf("%s\n", ctx);
                break;
            }
        }
    }

    if (error->partial_ast != NULL) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Partial AST:\n");
        print_ast_indented(error->partial_ast, depth + 1);
    }

    if (error->cause != NULL) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Caused by:\n");
        print_error_chain(error->cause, depth + 1);
    }
}

static void print_error_with_partial_ast(ParseError* error) {
    print_error_chain(error, 0);
}

// Helper function to print AST with indentation
static void print_ast_indented(ast_t* ast, int depth) {
    if (ast == NULL || ast == ast_nil) return;
    for (int i = 0; i < depth; i++) printf("  ");
    printf("(%s", pascal_tag_to_string(ast->typ));
    if (ast->sym) printf(" %s", ast->sym->name);

    ast_t* child = ast->child;
    if (child) {
        printf("\n");
        print_ast_indented(child, depth + 1);
    }
    printf(")");

    if (ast->next) {
        printf("\n");
        print_ast_indented(ast->next, depth);
    }
}


int main(int argc, char *argv[]) {
    bool print_ast = false;
    bool parse_procedure = false;
    char *filename = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--print-ast") == 0) {
            print_ast = true;
        } else if (strcmp(argv[i], "--parse-procedure") == 0) {
            parse_procedure = true;
        } else {
            filename = argv[i];
        }
    }

    if (filename == NULL) {
        fprintf(stderr, "Usage: %s [--print-ast] [--parse-procedure] <filename>\n", argv[0]);
        return 1;
    }

    // Read file content
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return 1;
    }
    
    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    if (file_size == -1L) {
        fprintf(stderr, "Error: could not determine file size for '%s'\n", filename);
        fclose(file);
        return 1;
    }
    fseek(file, 0, SEEK_SET);
    
    // Allocate buffer and read file
    char *file_content = malloc((size_t)file_size + 1);
    if (file_content == NULL) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        fclose(file);
        return 1;
    }
    
    size_t bytes_read = fread(file_content, 1, (size_t)file_size, file);
    if (bytes_read != (size_t)file_size && ferror(file)) {
        fprintf(stderr, "Error: Failed to read complete file '%s'\n", filename);
        free(file_content);
        fclose(file);
        return 1;
    }
    file_content[bytes_read] = '\0';
    fclose(file);

    printf("Parsing file: %s\n", filename);
    printf("File size: %zu bytes\n", bytes_read);
    printf("First 100 characters: '%.100s'\n", file_content);

    PascalPreprocessor *preprocessor = pascal_preprocessor_create();
    if (preprocessor == NULL) {
        fprintf(stderr, "Error: failed to initialise preprocessor\n");
        free(file_content);
        return 1;
    }
    if (!pascal_preprocessor_define(preprocessor, "FPC") ||
        !pascal_preprocessor_define(preprocessor, "OBJFPC")) {
        fprintf(stderr, "Error: failed to set default defines\n");
        pascal_preprocessor_free(preprocessor);
        free(file_content);
        return 1;
    }

    char *preprocess_error = NULL;
    size_t preprocessed_length = 0;
    char *preprocessed_content = pascal_preprocess_buffer(
        preprocessor,
        filename,
        file_content,
        bytes_read,
        &preprocessed_length,
        &preprocess_error);
    pascal_preprocessor_free(preprocessor);

    if (preprocessed_content == NULL) {
        fprintf(stderr, "Preprocessing failed: %s\n", preprocess_error ? preprocess_error : "unknown error");
        free(preprocess_error);
        free(file_content);
        return 1;
    }
    free(preprocess_error);

    printf("Preprocessed size: %zu bytes\n", preprocessed_length);

    bool parse_as_unit = !parse_procedure &&
        buffer_starts_with_keyword(preprocessed_content, preprocessed_length, "unit");
    if (parse_procedure) {
        printf("Detected top-level form: procedure (forced by flag)\n");
    } else {
        printf("Detected top-level form: %s\n", parse_as_unit ? "unit" : "program");
    }

    combinator_t *parser = new_combinator();
    if (parse_procedure) {
        init_pascal_procedure_parser(&parser);
    } else if (parse_as_unit) {
        init_pascal_unit_parser(&parser);
    } else {
        init_pascal_complete_program_parser(&parser);
    }

    input_t *in = new_input();
    in->buffer = preprocessed_content;
    in->length = preprocessed_length;
    ast_nil = new_ast();
    ast_nil->typ = PASCAL_T_NONE;

    ParseResult result = parse(in, parser);
    
    printf("Parse completed. Success: %s\n", result.is_success ? "YES" : "NO");
    if (!result.is_success && result.value.error) {
        printf("Input position when failed: %d of %d\n", in->start, in->length);
        if (in->start < in->length) {
            printf("Context around failure: '%.50s'\n", in->buffer + in->start);
        }
    }

    if (result.is_success) {
        if (in->start < in->length) {
            int trailing_index = in->start;
            int trailing_line = 1;
            int trailing_col = 1;
            parser_calculate_line_col(in, trailing_index, &trailing_line, &trailing_col);

            fprintf(stderr,
                    "Error: Parser did not consume entire input. Trailing input begins at line %d, column %d.\n",
                    trailing_line, trailing_col);

            char* context = parser_format_context(in, trailing_line, trailing_col, trailing_index);
            if (context != NULL) {
                fprintf(stderr, "%s", context);
                free(context);
            }

            const char* remaining = in->buffer + in->start;
            fprintf(stderr, "Remaining characters: '%.*s'%s\n",
                    80, remaining, strlen(remaining) > 80 ? "..." : "");

            free_ast(result.value.ast);
            free(preprocessed_content);
            free(file_content);
            free_combinator(parser);
            free(in);
            free(ast_nil);
            return 1;
        }
        if (print_ast) {
            print_pascal_ast(result.value.ast);
        }
        free_ast(result.value.ast);
    } else {
        print_error_with_partial_ast(result.value.error);
        free_error(result.value.error);
        free(preprocessed_content);
        free(file_content);
        free_combinator(parser);
        free(in);
        free(ast_nil);
        return 1;
    }

    free_combinator(parser);
    free(in);
    free(ast_nil);
    free(preprocessed_content);
    free(file_content);

    return 0;
}
