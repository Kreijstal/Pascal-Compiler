#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pascal_parser.h"

// Forward declaration
static void print_ast_indented(ast_t* ast, int depth);
static void print_error_with_partial_ast(ParseError* error);

// Helper function to print ParseError with partial AST
static void print_error_with_partial_ast(ParseError* error) {
    if (error == NULL) return;

    printf("Error at line %d, col %d: ", error->line, error->col);
    if (error->parser_name) {
        printf("In parser '%s': ", error->parser_name);
    }
    printf("%s\n", error->message);

    if (error->unexpected) {
        printf("Unexpected input: \"%s\"\n", error->unexpected);
    }

    if (error->partial_ast != NULL) {
        printf("Partial AST:\n");
        print_ast_indented(error->partial_ast, 1);
    }
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
    char *filename = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--print-ast") == 0) {
            print_ast = true;
        } else {
            filename = argv[i];
        }
    }

    if (filename == NULL) {
        fprintf(stderr, "Usage: %s [--print-ast] <filename>\n", argv[0]);
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

    combinator_t *parser = new_combinator();
    // Use unit parser instead of expression parser for full Pascal units
    init_pascal_unit_parser(&parser);
    
    printf("Parsing file: %s\n", filename);
    printf("File size: %zu bytes\n", bytes_read);
    printf("First 100 characters: '%.100s'\n", file_content);

    input_t *in = new_input();
    in->buffer = file_content;
    in->length = bytes_read;
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
            fprintf(stderr, "Error: Parser did not consume entire input. Trailing characters: '%s'\n", in->buffer + in->start);
            free_ast(result.value.ast);
            return 1;
        }
        if (print_ast) {
            print_pascal_ast(result.value.ast);
        }
        free_ast(result.value.ast);
    } else {
        print_error_with_partial_ast(result.value.error);
        free_error(result.value.error);
        free(file_content);
        return 1;
    }

    free_combinator(parser);
    free(in);
    free(ast_nil);
    free(file_content);

    return 0;
}
