/*
 * cparser_frontend.c
 * New parser frontend using cparser library
 */

#include "cparser_frontend.h"
#include "cparser_adapter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External from cparser
extern ast_t* ast_nil;

// Parse a Pascal file using cparser
Tree_t* parse_pascal_file_with_cparser(const char* filename) {
    // Read file content
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return NULL;
    }
    
    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    if (file_size == -1L) {
        fprintf(stderr, "Error: could not determine file size for '%s'\n", filename);
        fclose(file);
        return NULL;
    }
    fseek(file, 0, SEEK_SET);
    
    // Allocate buffer and read file
    char *file_content = malloc((size_t)file_size + 1);
    if (file_content == NULL) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        fclose(file);
        return NULL;
    }
    
    size_t bytes_read = fread(file_content, 1, (size_t)file_size, file);
    if (bytes_read != (size_t)file_size && ferror(file)) {
        fprintf(stderr, "Error: Failed to read complete file '%s'\n", filename);
        free(file_content);
        fclose(file);
        return NULL;
    }
    file_content[bytes_read] = '\0';
    fclose(file);

    // Initialize cparser
    combinator_t *parser = new_combinator();
    init_pascal_complete_program_parser(&parser);
    
    input_t *in = new_input();
    in->buffer = file_content;
    in->length = bytes_read;
    
    // Initialize ast_nil if not already done
    if (ast_nil == NULL) {
        ast_nil = new_ast();
        ast_nil->typ = PASCAL_T_NONE;
    }

    // Parse the input
    ParseResult result = parse(in, parser);
    
    Tree_t* parse_tree = NULL;
    
    if (!result.is_success) {
        fprintf(stderr, "Parse error in '%s'\n", filename);
        if (result.value.error) {
            fprintf(stderr, "Error at line %d, col %d: %s\n",
                    result.value.error->line,
                    result.value.error->col,
                    result.value.error->message);
            if (result.value.error->unexpected) {
                fprintf(stderr, "Unexpected: %s\n", result.value.error->unexpected);
            }
            free_error(result.value.error);
        }
    } else {
        // Check if entire input was consumed
        if (in->start < in->length) {
            fprintf(stderr, "Warning: Parser did not consume entire input. Position: %d/%d\n",
                    in->start, in->length);
        }
        
        // Convert cparser AST to GPC parse tree
        parse_tree = cparser_ast_to_tree(result.value.ast);
        
        // Free the cparser AST
        free_ast(result.value.ast);
    }

    // Cleanup
    free_combinator(parser);
    free(in);
    free(file_content);
    
    return parse_tree;
}
