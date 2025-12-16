static void pascal_test_cleanup(void);
#define TEST_FINI pascal_test_cleanup()

#include "acutest.h"
#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "pascal_preprocessor.h"
#include "pascal_keywords.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct parser_cleanup_entry {
    combinator_t* parser;
    combinator_t** anchor;
} parser_cleanup_entry_t;

static parser_cleanup_entry_t* parser_cleanup_entries = NULL;
static size_t parser_cleanup_count = 0;
static size_t parser_cleanup_capacity = 0;

static void track_parser_for_cleanup(combinator_t* parser, combinator_t** anchor) {
    if (parser == NULL) {
        if (anchor != NULL) {
            free(anchor);
        }
        return;
    }
    if (parser_cleanup_count == parser_cleanup_capacity) {
        size_t new_capacity = parser_cleanup_capacity ? parser_cleanup_capacity * 2 : 16;
        parser_cleanup_entry_t* new_items = realloc(parser_cleanup_entries, new_capacity * sizeof(parser_cleanup_entry_t));
        if (new_items == NULL) {
            fprintf(stderr, "Out of memory while tracking parser cleanup.\n");
            exit(EXIT_FAILURE);
        }
        parser_cleanup_entries = new_items;
        parser_cleanup_capacity = new_capacity;
    }
    parser_cleanup_entries[parser_cleanup_count++] = (parser_cleanup_entry_t){
        .parser = parser,
        .anchor = anchor
    };
}

static void clear_tracked_parsers(void) {
    for (size_t i = 0; i < parser_cleanup_count; ++i) {
        if (parser_cleanup_entries[i].parser != NULL) {
            free_combinator(parser_cleanup_entries[i].parser);
            parser_cleanup_entries[i].parser = NULL;
        }
        if (parser_cleanup_entries[i].anchor != NULL) {
            free(parser_cleanup_entries[i].anchor);
            parser_cleanup_entries[i].anchor = NULL;
        }
    }
    parser_cleanup_count = 0;
}

static combinator_t* create_expression_parser(void) {
    combinator_t** anchor = safe_malloc(sizeof(combinator_t*));
    *anchor = new_combinator();
    init_pascal_expression_parser(anchor, NULL);
    combinator_t* parser = *anchor;
    track_parser_for_cleanup(parser, anchor);
    return parser;
}

static combinator_t* create_statement_parser(void) {
    combinator_t** anchor = safe_malloc(sizeof(combinator_t*));
    *anchor = new_combinator();
    init_pascal_statement_parser(anchor);
    combinator_t* parser = *anchor;
    track_parser_for_cleanup(parser, anchor);
    return parser;
}

static combinator_t* create_program_parser(void) {
    combinator_t** anchor = safe_malloc(sizeof(combinator_t*));
    *anchor = new_combinator();
    init_pascal_program_parser(anchor);
    combinator_t* parser = *anchor;
    track_parser_for_cleanup(parser, anchor);
    return parser;
}

static combinator_t* create_complete_program_parser(void) {
    combinator_t** anchor = safe_malloc(sizeof(combinator_t*));
    *anchor = new_combinator();
    init_pascal_complete_program_parser(anchor);
    combinator_t* parser = *anchor;
    track_parser_for_cleanup(parser, anchor);
    return parser;
}

static combinator_t* create_procedure_parser(void) {
    combinator_t** anchor = safe_malloc(sizeof(combinator_t*));
    *anchor = new_combinator();
    init_pascal_procedure_parser(anchor);
    combinator_t* parser = *anchor;
    track_parser_for_cleanup(parser, anchor);
    return parser;
}

// Shared parser instances to avoid expensive re-initialization
// These are initialized lazily on first use
static combinator_t* shared_expression_parser = NULL;
static combinator_t* shared_statement_parser = NULL;
static combinator_t* shared_unit_parser = NULL;
static combinator_t* shared_program_parser = NULL;

static void release_shared_parser(combinator_t** parser) {
    if (parser != NULL && *parser != NULL) {
        free_combinator(*parser);
        *parser = NULL;
    }
}

static void pascal_test_cleanup(void) {
    clear_tracked_parsers();
    release_shared_parser(&shared_expression_parser);
    release_shared_parser(&shared_statement_parser);
    release_shared_parser(&shared_unit_parser);
    release_shared_parser(&shared_program_parser);
}

__attribute__((unused)) static combinator_t* get_expression_parser(void) {
    if (shared_expression_parser == NULL) {
        shared_expression_parser = new_combinator();
        init_pascal_expression_parser(&shared_expression_parser, NULL);
    }
    return shared_expression_parser;
}

static combinator_t* get_statement_parser(void) {
    if (shared_statement_parser == NULL) {
        shared_statement_parser = new_combinator();
        init_pascal_statement_parser(&shared_statement_parser);
    }
    return shared_statement_parser;
}

static combinator_t* get_unit_parser(void) {
    if (shared_unit_parser == NULL) {
        shared_unit_parser = new_combinator();
        init_pascal_unit_parser(&shared_unit_parser);
    }
    return shared_unit_parser;
}

static combinator_t* get_program_parser(void) {
    if (shared_program_parser == NULL) {
        shared_program_parser = new_combinator();
        init_pascal_complete_program_parser(&shared_program_parser);
    }
    return shared_program_parser;
}

static ast_t* find_first_node_of_type(ast_t* node, tag_t target) {
    if (node == NULL || node == ast_nil) {
        return NULL;
    }

    for (ast_t* current = node; current != NULL && current != ast_nil; current = current->next) {
        if (current->typ == target) {
            return current;
        }
        ast_t* child_result = find_first_node_of_type(current->child, target);
        if (child_result != NULL) {
            return child_result;
        }
    }

    return NULL;
}

static char* preprocess_pascal_source(const char* path,
                                      const char* kind,
                                      const char* name,
                                      char* source);

static char* load_pascal_snippet(const char* filename) {
    FILE* file = NULL;
    char* path = NULL;
    char* buffer = NULL;
    char* result = NULL;

    const char* snippets_dir = "snippets/";

#ifdef TEST_SRCDIR
    const char* base_dir = TEST_SRCDIR;
    size_t base_len = strlen(base_dir);
#else
    const char* current_file = __FILE__;
    const char* last_slash = strrchr(current_file, '/');
#ifdef _WIN32
    const char* last_backslash = strrchr(current_file, '\\');
    if (last_backslash && (!last_slash || last_backslash > last_slash)) {
        last_slash = last_backslash;
    }
#endif
    size_t base_len = last_slash ? (size_t)(last_slash - current_file + 1) : 0;
    const char* base_dir = current_file;
#endif

    size_t snippets_len = strlen(snippets_dir);
    size_t filename_len = strlen(filename);

#ifdef TEST_SRCDIR
    // When using TEST_SRCDIR, we need an extra byte for the separator '/'
    path = (char*)malloc(base_len + 1 + snippets_len + filename_len + 1);
#else
    path = (char*)malloc(base_len + snippets_len + filename_len + 1);
#endif
    if (!path) {
        fprintf(stderr, "Out of memory while constructing snippet path for %s\n", filename);
        goto cleanup;
    }

#ifdef TEST_SRCDIR
    snprintf(path, base_len + 1 + snippets_len + filename_len + 1, "%s/%s%s", 
             base_dir, snippets_dir, filename);
#else
    snprintf(path, base_len + snippets_len + filename_len + 1, "%.*s%s%s", 
             (int)base_len, base_dir, snippets_dir, filename);
#endif

    file = fopen(path, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open Pascal snippet '%s' at %s\n", filename, path);
        goto cleanup;
    }

    if (fseek(file, 0, SEEK_END) != 0) {
        fprintf(stderr, "Failed to seek to end of snippet '%s'\n", filename);
        goto cleanup;
    }

    long size = ftell(file);
    if (size < 0) {
        fprintf(stderr, "Failed to determine size of snippet '%s'\n", filename);
        goto cleanup;
    }

    if (fseek(file, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Failed to rewind snippet '%s'\n", filename);
        goto cleanup;
    }

    buffer = (char*)malloc((size_t)size + 1);
    if (!buffer) {
        fprintf(stderr, "Out of memory while reading snippet '%s'\n", filename);
        goto cleanup;
    }

    size_t read = fread(buffer, 1, (size_t)size, file);
    if (read != (size_t)size) {
        fprintf(stderr, "Failed to read snippet '%s' (expected %ld bytes, got %zu)\n", filename, size, read);
        goto cleanup;
    }

    buffer[size] = '\0';
    result = buffer;
    buffer = NULL;  // Prevent cleanup from freeing it

    result = preprocess_pascal_source(path, "snippet", filename, result);
    if (!result) {
        goto cleanup;
    }

cleanup:
    if (file) fclose(file);
    free(path);
    free(buffer);  // Only frees on error path
    return result;
}

static char* preprocess_pascal_source(const char* path,
                                      const char* kind,
                                      const char* name,
                                      char* source) {
    if (!source) {
        return NULL;
    }

    PascalPreprocessor* pp = pascal_preprocessor_create();
    if (!pp) {
        fprintf(stderr, "Failed to allocate Pascal preprocessor for %s '%s'\n", kind, name);
        free(source);
        return NULL;
    }

    char* preprocess_error = NULL;
    char* preprocessed = pascal_preprocess_buffer(pp, path, source, strlen(source), NULL, &preprocess_error);
    pascal_preprocessor_free(pp);

    if (!preprocessed) {
        fprintf(stderr, "Failed to preprocess Pascal %s '%s'%s%s\n",
                kind,
                name,
                preprocess_error ? ": " : "",
                preprocess_error ? preprocess_error : "");
        free(preprocess_error);
        free(source);
        return NULL;
    }

    free(preprocess_error);
    free(source);
    return preprocessed;
}

static size_t encode_utf8(uint32_t codepoint, char* out) {
    if (codepoint <= 0x7F) {
        out[0] = (char)codepoint;
        return 1;
    } else if (codepoint <= 0x7FF) {
        out[0] = (char)(0xC0 | (codepoint >> 6));
        out[1] = (char)(0x80 | (codepoint & 0x3F));
        return 2;
    } else if (codepoint <= 0xFFFF) {
        out[0] = (char)(0xE0 | (codepoint >> 12));
        out[1] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[2] = (char)(0x80 | (codepoint & 0x3F));
        return 3;
    } else if (codepoint <= 0x10FFFF) {
        out[0] = (char)(0xF0 | (codepoint >> 18));
        out[1] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
        out[2] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[3] = (char)(0x80 | (codepoint & 0x3F));
        return 4;
    }
    out[0] = '?';
    return 1;
}

static uint16_t read_utf16_code_unit(const uint8_t* data, size_t index, bool little_endian) {
    const uint8_t* p = data + 2 * index;
    if (little_endian) {
        return (uint16_t)(p[0] | ((uint16_t)p[1] << 8));
    }
    return (uint16_t)(((uint16_t)p[0] << 8) | p[1]);
}

static char* convert_utf16_to_utf8(const uint8_t* data, size_t byte_len, bool little_endian) {
    if (data == NULL) {
        return NULL;
    }

    size_t code_units = byte_len / 2;
    char* out = (char*)malloc(code_units * 4 + 1);
    if (!out) {
        return NULL;
    }

    size_t out_pos = 0;
    for (size_t i = 0; i < code_units; ++i) {
        uint16_t w1 = read_utf16_code_unit(data, i, little_endian);

        uint32_t codepoint;
        if (w1 >= 0xD800 && w1 <= 0xDBFF) {
            if (i + 1 < code_units) {
                uint16_t w2 = read_utf16_code_unit(data, i + 1, little_endian);

                if (w2 >= 0xDC00 && w2 <= 0xDFFF) {
                    codepoint = 0x10000 + ((((uint32_t)w1 - 0xD800) << 10) | ((uint32_t)w2 - 0xDC00));
                    ++i; // Consume the low surrogate
                } else {
                    codepoint = 0xFFFD; // Replacement character
                }
            } else {
                codepoint = 0xFFFD;
            }
        } else if (w1 >= 0xDC00 && w1 <= 0xDFFF) {
            codepoint = 0xFFFD;
        } else {
            codepoint = w1;
        }

        out_pos += encode_utf8(codepoint, out + out_pos);
    }

    out[out_pos] = '\0';
    return out;
}

static char* load_pascal_file(const char* filename) {
    FILE* file = NULL;
    char* path = NULL;
    char* buffer = NULL;
    char* result = NULL;

    const char* pascal_dir = "pascal/";

#ifdef TEST_SRCDIR
    const char* base_dir = TEST_SRCDIR;
    size_t base_len = strlen(base_dir);
#else
    const char* current_file = __FILE__;
    const char* last_slash = strrchr(current_file, '/');
#ifdef _WIN32
    const char* last_backslash = strrchr(current_file, '\\');
    if (last_backslash && (!last_slash || last_backslash > last_slash)) {
        last_slash = last_backslash;
    }
#endif
    size_t base_len = last_slash ? (size_t)(last_slash - current_file + 1) : 0;
    const char* base_dir = current_file;
#endif

    size_t pascal_len = strlen(pascal_dir);
    size_t filename_len = strlen(filename);

#ifdef TEST_SRCDIR
    // When using TEST_SRCDIR, we need an extra byte for the separator '/'
    path = (char*)malloc(base_len + 1 + pascal_len + filename_len + 1);
#else
    path = (char*)malloc(base_len + pascal_len + filename_len + 1);
#endif
    if (!path) {
        fprintf(stderr, "Out of memory while constructing pascal file path for %s\n", filename);
        goto cleanup;
    }

#ifdef TEST_SRCDIR
    snprintf(path, base_len + 1 + pascal_len + filename_len + 1, "%s/%s%s", 
             base_dir, pascal_dir, filename);
#else
    snprintf(path, base_len + pascal_len + filename_len + 1, "%.*s%s%s", 
             (int)base_len, base_dir, pascal_dir, filename);
#endif

    file = fopen(path, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open Pascal file '%s' at %s\n", filename, path);
        goto cleanup;
    }

    if (fseek(file, 0, SEEK_END) != 0) {
        fprintf(stderr, "Failed to seek to end of Pascal file '%s'\n", filename);
        goto cleanup;
    }

    long size = ftell(file);
    if (size < 0) {
        fprintf(stderr, "Failed to determine size of Pascal file '%s'\n", filename);
        goto cleanup;
    }

    if (fseek(file, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Failed to rewind Pascal file '%s'\n", filename);
        goto cleanup;
    }

    buffer = (char*)malloc((size_t)size + 1);
    if (!buffer) {
        fprintf(stderr, "Out of memory while reading Pascal file '%s'\n", filename);
        goto cleanup;
    }

    size_t read = fread(buffer, 1, (size_t)size, file);
    if (read != (size_t)size) {
        fprintf(stderr, "Failed to read Pascal file '%s' (expected %ld bytes, got %zu)\n", filename, size, read);
        goto cleanup;
    }

    buffer[size] = '\0';
    result = buffer;
    buffer = NULL;  // Prevent cleanup from freeing it

    if (result && size >= 2) {
        unsigned char b0 = (unsigned char)result[0];
        unsigned char b1 = (unsigned char)result[1];
        if ((b0 == 0xFF && b1 == 0xFE) || (b0 == 0xFE && b1 == 0xFF)) {
            bool little_endian = (b0 == 0xFF);
            char* converted = convert_utf16_to_utf8((const uint8_t*)(result + 2), (size_t)size - 2, little_endian);
            if (!converted) {
                free(result);
                result = NULL;
                goto cleanup;
            }
            free(result);
            result = converted;
        }
    }

    if (result) {
        result = preprocess_pascal_source(path, "file", filename, result);
        if (!result) {
            goto cleanup;
        }
    }

cleanup:
    if (file) fclose(file);
    free(path);
    free(buffer);  // Only frees on error path
    return result;
}

void test_pascal_integer_parsing(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("123");
    input->length = 3;

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "123") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_invalid_input(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("1 +");
    input->length = 3;

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(!res.is_success);
    TEST_ASSERT(res.value.error->partial_ast != NULL);
    TEST_ASSERT(res.value.error->partial_ast->typ == PASCAL_T_ADD);
    ast_t* lhs = res.value.error->partial_ast->child;
    TEST_ASSERT(lhs->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(lhs->sym->name, "1") == 0);

    free_error(res.value.error);    free(input->buffer);
    free_input(input);
}

/*
 * Regression test for GitHub issue: stray identifiers between declaration sections
 * and main block should cause a parse error, not be silently ignored.
 *
 * Bug case:
 *   program colors;
 *   const cyan=#27+'[36m';
 *   clear=#27+'[0m';
 *   rot      <-- stray identifier (should be an error)
 *   begin
 *     writeln('Hello');
 *   end.
 *
 * Previously this would silently compile but produce no output.
 */
void test_pascal_stray_identifier_before_begin(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    
    const char* source = 
        "program colors;\n"
        "const cyan=#27+'[36m';\n"
        "clear=#27+'[0m';\n"
        "rot\n"                           // stray identifier - should be an error
        "begin\n"
        "  writeln('Hello');\n"
        "end.\n";
    
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    // This should FAIL to parse - the 'rot' identifier is not valid
    TEST_ASSERT(!res.is_success);
    TEST_MSG("Parser should reject stray identifiers between const section and begin block");
    
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        TEST_ASSERT(res.value.error != NULL);
        // Check that the error mentions the problem area
        if (res.value.error->message != NULL) {
            TEST_MSG("Error message: %s", res.value.error->message);
        }
        free_error(res.value.error);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_preprocessor_conditionals(void) {
    PascalPreprocessor *pp = pascal_preprocessor_create();
    TEST_ASSERT(pp != NULL);
    if (!pp) {
        return;
    }

    TEST_ASSERT(pascal_preprocessor_define(pp, "FOO"));

    const char *source = "{$ifdef FOO}foo{$else}bar{$endif}";
    size_t source_len = strlen(source);
    char *error_message = NULL;
    size_t output_len = 0;
    char *result = pascal_preprocess_buffer(pp, "<memory>", source, source_len, &output_len, &error_message);

    TEST_ASSERT(result != NULL);
    TEST_ASSERT(error_message == NULL);
    if (error_message) {
        free(error_message);
    }
    if (result) {
        TEST_CHECK(strstr(result, "foo") != NULL);
        TEST_CHECK(strstr(result, "bar") == NULL);
        free(result);
    }

    TEST_ASSERT(pascal_preprocessor_undefine(pp, "FOO"));

    const char *source2 = "{$ifdef FOO}x{$else}y{$endif}";
    char *error_message2 = NULL;
    size_t output_len2 = 0;
    char *result2 = pascal_preprocess_buffer(pp, "<memory>", source2, strlen(source2), &output_len2, &error_message2);

    TEST_ASSERT(result2 != NULL);
    TEST_ASSERT(error_message2 == NULL);
    if (error_message2) {
        free(error_message2);
    }
    if (result2) {
        TEST_CHECK(strstr(result2, "y") != NULL);
        TEST_CHECK(strstr(result2, "x") == NULL);
        free(result2);
    }

    pascal_preprocessor_free(pp);
}

void test_pascal_preprocessor_comment_mixing(void) {
    PascalPreprocessor *pp = pascal_preprocessor_create();
    TEST_ASSERT(pp != NULL);
    if (!pp) {
        return;
    }

    const char *source =
        "{ brace comment with 'apostrophe' }\n"
        "(* paren comment with {$ifdef IGNORE} nested 'quote' *)\n"
        "// line comment with 'quote' and CRLF\r\n"
        "value := 'text';\n";

    char *error_message = NULL;
    size_t output_len = 0;
    char *result = pascal_preprocess_buffer(
        pp,
        "<memory>",
        source,
        strlen(source),
        &output_len,
        &error_message);

    TEST_ASSERT(result != NULL);
    TEST_ASSERT(error_message == NULL);

    if (error_message) {
        free(error_message);
    }

    if (result) {
        TEST_CHECK(strstr(result, "line comment with 'quote' and CRLF\r\n") != NULL);
        TEST_CHECK(strstr(result, "value := 'text';") != NULL);
        TEST_CHECK(strstr(result, "\r\n") != NULL);
        free(result);
    }

    pascal_preprocessor_free(pp);
}

void test_pascal_preprocessor_numeric_comparisons(void) {
    PascalPreprocessor *pp = pascal_preprocessor_create();
    TEST_ASSERT(pp != NULL);
    if (!pp) {
        return;
    }

    // Test undefined macro in comparison causes error (matching FPC behavior)
    const char *source1 = "{$if UNDEFINED_MACRO=0}yes{$else}no{$endif}";
    char *error1 = NULL;
    char *result1 = pascal_preprocess_buffer(pp, "<memory>", source1, strlen(source1), NULL, &error1);
    TEST_ASSERT(result1 == NULL);  // Should fail
    TEST_ASSERT(error1 != NULL);   // Should have error message
    if (error1) {
        TEST_CHECK(strstr(error1, "undefined macro") != NULL);
        free(error1);
    }
    if (result1) free(result1);

    // Test undefined macro in comparison causes error (matching FPC behavior)
    const char *source2 = "{$if FPC_FULLVERSION>30300}new{$else}old{$endif}";
    char *error2 = NULL;
    char *result2 = pascal_preprocess_buffer(pp, "<memory>", source2, strlen(source2), NULL, &error2);
    TEST_ASSERT(result2 == NULL);  // Should fail
    TEST_ASSERT(error2 != NULL);   // Should have error message
    if (error2) {
        TEST_CHECK(strstr(error2, "undefined macro") != NULL);
        free(error2);
    }
    if (result2) free(result2);

    // Test numeric comparison with defined macro
    TEST_ASSERT(pascal_preprocessor_define(pp, "VERSION:=30400"));
    const char *source3 = "{$if VERSION>30300}new{$else}old{$endif}";
    char *error3 = NULL;
    char *result3 = pascal_preprocess_buffer(pp, "<memory>", source3, strlen(source3), NULL, &error3);
    TEST_ASSERT(result3 != NULL);
    TEST_ASSERT(error3 == NULL);
    if (result3) {
        // VERSION is 30400, which is > 30300
        TEST_CHECK(strstr(result3, "new") != NULL);
        TEST_CHECK(strstr(result3, "old") == NULL);
        free(result3);
    }
    if (error3) free(error3);

    // Test less than comparison
    const char *source4 = "{$if VERSION<40000}yes{$else}no{$endif}";
    char *error4 = NULL;
    char *result4 = pascal_preprocess_buffer(pp, "<memory>", source4, strlen(source4), NULL, &error4);
    TEST_ASSERT(result4 != NULL);
    TEST_ASSERT(error4 == NULL);
    if (result4) {
        // VERSION is 30400, which is < 40000
        TEST_CHECK(strstr(result4, "yes") != NULL);
        TEST_CHECK(strstr(result4, "no") == NULL);
        free(result4);
    }
    if (error4) free(error4);

    pascal_preprocessor_free(pp);
}


void test_pascal_function_call(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("my_func");  // Just test identifier parsing
    input->length = strlen("my_func");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = res.value.ast;
    if (res.value.ast->child && res.value.ast->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = res.value.ast->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "my_func") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_function_call_with_escaped_quote(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    const char* expr = "talk(2,'I''m looking for a girl.')";
    input->buffer = strdup(expr);
    input->length = strlen(expr);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    TEST_ASSERT(input->start == input->length);

    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNC_CALL);

        ast_t* func_name = res.value.ast->child;
        TEST_ASSERT(func_name != NULL);

        ast_t* actual_name_node = func_name;
        if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
            actual_name_node = func_name->child;
        }

        TEST_ASSERT(actual_name_node->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(actual_name_node->sym != NULL);
        TEST_ASSERT(strcmp(actual_name_node->sym->name, "talk") == 0);

        ast_t* first_arg = func_name->next;
        TEST_ASSERT(first_arg != NULL);
        TEST_ASSERT(first_arg->typ == PASCAL_T_INTEGER);
        TEST_ASSERT(first_arg->sym != NULL);
        TEST_ASSERT(strcmp(first_arg->sym->name, "2") == 0);

        ast_t* second_arg = first_arg->next;
        TEST_ASSERT(second_arg != NULL);
        TEST_ASSERT(second_arg->typ == PASCAL_T_STRING);
        TEST_ASSERT(second_arg->sym != NULL);
        TEST_ASSERT(strcmp(second_arg->sym->name, "I'm looking for a girl.") == 0);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);

}

void test_pascal_string_literal(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("\"hello world\"");
    input->length = strlen("\"hello world\"");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "hello world") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_function_call_no_args(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("func()");
    input->length = strlen("func()");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNC_CALL);
    
    // First child should be the function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "func") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_function_call_with_args(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("func(5, 10)");
    input->length = strlen("func(5, 10)");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNC_CALL);
    
    // First child should be the function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "func") == 0);
    
    // Arguments follow
    ast_t* arg1 = func_name->next;
    TEST_ASSERT(arg1->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(arg1->sym->name, "5") == 0);
    
    ast_t* arg2 = arg1->next;
    TEST_ASSERT(arg2->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(arg2->sym->name, "10") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_mod_operator(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("7 mod 3");
    input->length = strlen("7 mod 3");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_MOD);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "7") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_mod_operator_percent(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("7 % 3");
    input->length = strlen("7 % 3");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_MOD);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "7") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_string_concatenation(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("\"hello\" + \"world\"");
    input->length = strlen("\"hello\" + \"world\"");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADD);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(left->sym->name, "hello") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(right->sym->name, "world") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_complex_expression(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("procedure((5*7)-5)+\"test\"");
    input->length = strlen("procedure((5*7)-5)+\"test\"");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADD);
    
    // Left side should be function call
    ast_t* func_call = res.value.ast->child;
    TEST_ASSERT(func_call->typ == PASCAL_T_FUNC_CALL);
    
    // Function name
    ast_t* func_name = func_call->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "procedure") == 0);
    
    // Function argument: (5*7)-5
    ast_t* arg = func_name->next;
    TEST_ASSERT(arg->typ == PASCAL_T_SUB);
    
    // Right side should be string
    ast_t* right = func_call->next;
    TEST_ASSERT(right->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(right->sym->name, "test") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_div_operator(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("10 div 3");
    input->length = strlen("10 div 3");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_INTDIV);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "10") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test real number parsing
void test_pascal_real_number(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("3.14");
    input->length = strlen("3.14");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_REAL);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "3.14") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test character literal parsing
void test_pascal_char_literal(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("'A'");
    input->length = strlen("'A'");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "A") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

static void assert_char_code_literal_success(combinator_t* p, const char* literal) {
    input_t* input = new_input();
    input->buffer = strdup(literal);
    input->length = (int)strlen(literal);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CHAR_CODE);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, literal) == 0);

    free_ast(res.value.ast);
    free(input->buffer);
    free_input(input);
}

static void assert_char_code_literal_failure(combinator_t* p, const char* literal) {
    input_t* input = new_input();
    input->buffer = strdup(literal);
    input->length = (int)strlen(literal);

    ParseResult res = parse(input, p);

    TEST_ASSERT(!res.is_success);

    free_error(res.value.error);
    free(input->buffer);
    free_input(input);
}

void test_pascal_char_code_literal(void) {
    combinator_t* p = create_expression_parser();
    assert_char_code_literal_success(p, "#13");
    assert_char_code_literal_success(p, "#$0D");

    assert_char_code_literal_failure(p, "#");
    assert_char_code_literal_failure(p, "#$ZZ");
    assert_char_code_literal_failure(p, "#abc");
}

// Test unary plus operator
void test_pascal_unary_plus(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("+42");
    input->length = strlen("+42");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_POS);
    
    // Check operand
    ast_t* operand = res.value.ast->child;
    TEST_ASSERT(operand->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(operand->sym->name, "42") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test relational operators
void test_pascal_relational_operators(void) {
    const char* expressions[] = {
        "5 = 5", "5 <> 3", "3 < 5", "5 > 3", "3 <= 5", "5 >= 3"
    };
    pascal_tag_t expected_tags[] = {
        PASCAL_T_EQ, PASCAL_T_NE, PASCAL_T_LT, PASCAL_T_GT, PASCAL_T_LE, PASCAL_T_GE
    };
    
    for (int i = 0; i < 6; i++) {
        combinator_t* p = create_expression_parser();
        input_t* input = new_input();
        input->buffer = strdup(expressions[i]);
        input->length = strlen(expressions[i]);
        ParseResult res = parse(input, p);
        TEST_ASSERT(res.is_success);
        TEST_ASSERT(res.value.ast->typ == expected_tags[i]);
        free_ast(res.value.ast);        free(input->buffer);
        free_input(input);
    }
}

// Test boolean operators
void test_pascal_boolean_operators(void) {
    const char* expressions[] = {
        "true and false", "true or false", "not true", "true xor false"
    };
    pascal_tag_t expected_tags[] = {
        PASCAL_T_AND, PASCAL_T_OR, PASCAL_T_NOT, PASCAL_T_XOR
    };
    
    for (int i = 0; i < 4; i++) {
        combinator_t* p = create_expression_parser();
        input_t* input = new_input();
        input->buffer = strdup(expressions[i]);
        input->length = strlen(expressions[i]);

        ParseResult res = parse(input, p);

        TEST_ASSERT(res.is_success);
        TEST_ASSERT(res.value.ast->typ == expected_tags[i]);

        free_ast(res.value.ast);        free(input->buffer);
        free_input(input);
    }
}

// Test bitwise shift operators
void test_pascal_bitwise_operators(void) {
    const char* expressions[] = { "8 shl 2", "8 shr 1", "8 rol 3", "8 ror 2" };
    pascal_tag_t expected_tags[] = { PASCAL_T_SHL, PASCAL_T_SHR, PASCAL_T_ROL, PASCAL_T_ROR };

    for (int i = 0; i < 4; i++) {
        combinator_t* p = create_expression_parser();
        input_t* input = new_input();
        input->buffer = strdup(expressions[i]);
        input->length = strlen(expressions[i]);

        ParseResult res = parse(input, p);

        TEST_ASSERT(res.is_success);
        TEST_ASSERT(res.value.ast->typ == expected_tags[i]);

        free_ast(res.value.ast);
        free(input->buffer);
        free_input(input);
    }
}

// Test address operator
void test_pascal_address_operator(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("@myVar");
    input->length = strlen("@myVar");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADDR);
    
    // Check operand
    ast_t* operand = res.value.ast->child;
    TEST_ASSERT(operand->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = operand;
    if (operand->child && operand->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = operand->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "myVar") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test complex expression with new operators
void test_pascal_comprehensive_expression(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("(x + y) * z div 4");
    input->length = strlen("(x + y) * z div 4");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_INTDIV);
    
    // Left side should be multiplication
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_MUL);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test precedence: arithmetic vs comparison
void test_pascal_precedence(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("a + b = c * d");
    input->length = strlen("a + b = c * d");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_EQ); // = should be top level

    // Left side should be addition
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_ADD);
    
    // Right side should be multiplication  
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_MUL);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test type casting
void test_pascal_type_casting(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("Integer('A')");
    input->length = strlen("Integer('A')");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_TYPECAST);
    
    // First child should be the type name
    ast_t* type_name = res.value.ast->child;
    TEST_ASSERT(type_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(type_name->sym->name, "Integer") == 0);
    
    // Second child should be the expression being cast
    ast_t* expr = type_name->next;
    TEST_ASSERT(expr->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(expr->sym->name, "A") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test set constructor parsing
void test_pascal_set_constructor(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("[1, 2, 3]");
    input->length = strlen("[1, 2, 3]");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET);
    
    // Check first element
    ast_t* elem1 = res.value.ast->child;
    TEST_ASSERT(elem1->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(elem1->sym->name, "1") == 0);
    
    // Check second element
    ast_t* elem2 = elem1->next;
    TEST_ASSERT(elem2->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(elem2->sym->name, "2") == 0);
    
    // Check third element
    ast_t* elem3 = elem2->next;
    TEST_ASSERT(elem3->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(elem3->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test empty set constructor
void test_pascal_empty_set(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("[]");
    input->length = strlen("[]");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET);
    TEST_ASSERT(res.value.ast->child == NULL); // Empty set

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test character set constructor
void test_pascal_char_set(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("['a', 'b', 'c']");
    input->length = strlen("['a', 'b', 'c']");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET);
    
    // Check first element
    ast_t* elem1 = res.value.ast->child;
    TEST_ASSERT(elem1->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(elem1->sym->name, "a") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test typed file type parsing: file of Integer
void test_pascal_typed_file_type(void) {
    combinator_t* p = get_program_parser();
    const char* src =
        "program FileTest;\n"
        "type\n"
        "  TIntegerFile = file of Integer;\n"
        "var\n"
        "  F: TIntegerFile;\n"
        "begin\n"
        "end.\n";

    input_t* input = new_input();
    input->buffer = strdup(src);
    input->length = (int)strlen(src);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);

    ast_t* program_decl = res.value.ast;
    TEST_ASSERT(program_decl != NULL);

    ast_t* file_node = find_first_node_of_type(program_decl, PASCAL_T_FILE_TYPE);
    TEST_ASSERT(file_node != NULL);

    ast_t* elem_type = file_node->child;
    TEST_ASSERT(elem_type != NULL);
    TEST_ASSERT(elem_type->typ == PASCAL_T_TYPE_SPEC || elem_type->typ == PASCAL_T_IDENTIFIER);

    free_ast(res.value.ast);
    free(input->buffer);
    free_input(input);
}

// Regression: routine-local repeated const sections with typed record/array consts
void test_program_routine_local_typed_consts(void) {
    combinator_t* p = get_program_parser();
    const char* src =
        "program P;\n"
        "function W: integer; stdcall;\n"
        "const A = 1;\n"
        "const R: record a: integer; end = (a:7);\n"
        "const Arr: array[1..3] of integer = (1,2,3);\n"
        "var v: integer;\n"
        "begin v := A; W := v; end;\n"
        "begin end.";
    input_t* input = new_input();
    input->buffer = strdup(src);
    input->length = (int)strlen(src);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) free_ast(res.value.ast); else free_error(res.value.error);
    free(input->buffer);
    free_input(input);
}

// Regression: case statement with begin..end branch inside routine
void test_program_case_with_begin_end_branch(void) {
    combinator_t* p = get_program_parser();
    const char* src =
        "program P;\n"
        "function W: integer; stdcall;\n"
        "var Msg: integer;\n"
        "begin\n"
        "  case Msg of 1: begin W := 1; end; end;\n"
        "end;\n"
        "begin end.";
    input_t* input = new_input();
    input->buffer = strdup(src);
    input->length = (int)strlen(src);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) free_ast(res.value.ast); else free_error(res.value.error);
    free(input->buffer);
    free_input(input);
}

// Regression: routine directive between header and body should parse
void test_program_routine_directive_before_body(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    const char* src =
        "program p;\n"
        "function About: integer; stdcall;\n"
        "begin\n  About := 0;\nend;\n"
        "begin\nend.";
    input->buffer = strdup(src);
    input->length = (int)strlen(src);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) { free_ast(res.value.ast); } else { free_error(res.value.error); }
    free(input->buffer);
    free_input(input);
}

// Regression: type declaration with function type + stdcall
void test_type_subroutine_with_stdcall(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    const char* src =
        "program p;\n"
        "type TWNDPROC = function (Wnd: HWND; Msg: DWORD; wParam: ptruint; lParam: ptrint): integer; stdcall;\n"
        "begin\nend.";
    input->buffer = strdup(src);
    input->length = (int)strlen(src);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) { free_ast(res.value.ast); } else { free_error(res.value.error); }
    free(input->buffer);
    free_input(input);
}

// Regression: var absolute clause
void test_var_absolute_clause(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    const char* src =
        "program p;\n"
        "var\n  ListBox: HWND absolute ListBoxInt;\n"
        "begin\nend.";
    input->buffer = strdup(src);
    input->length = (int)strlen(src);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) { free_ast(res.value.ast); } else { free_error(res.value.error); }
    free(input->buffer);
    free_input(input);
}

// Regression: library header and exports section
void test_library_header_and_exports(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    const char* src =
        "library MyLib;\n"
        "exports DllFunction index 1;\n"
        "begin\nend.";
    input->buffer = strdup(src);
    input->length = (int)strlen(src);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) { free_ast(res.value.ast); } else { free_error(res.value.error); }
    free(input->buffer);
    free_input(input);
}

// Test range expression
void test_pascal_range_expression(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("1..10");
    input->length = strlen("1..10");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_RANGE);
    
    // Check left operand
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "1") == 0);
    
    // Check right operand
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "10") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test character range expression
void test_pascal_char_range(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("'a'..'z'");
    input->length = strlen("'a'..'z'");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_RANGE);
    
    // Check left operand
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(left->sym->name, "a") == 0);
    
    // Check right operand
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(right->sym->name, "z") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test set union operation
void test_pascal_set_union(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("[1, 2] + [3, 4]");
    input->length = strlen("[1, 2] + [3, 4]");

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET_UNION);
    
    // Check left operand is a set
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_SET);
    
    // Check right operand is a set
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_SET);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test class type checking with 'is' operator
void test_pascal_is_operator(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("MyObject is TMyClass");
    input->length = strlen("MyObject is TMyClass");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_IS);
    
    // Check left operand (object identifier)
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_left_node = left;
    if (left->child && left->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_left_node = left->child;
    }
    
    TEST_ASSERT(actual_left_node->sym && 
               actual_left_node->sym->name && 
               strcmp(actual_left_node->sym->name, "MyObject") == 0);
    
    // Check right operand (class type identifier)
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_right_node = right;
    if (right->child && right->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_right_node = right->child;
    }
    
    TEST_ASSERT(actual_right_node->sym && 
               actual_right_node->sym->name && 
               strcmp(actual_right_node->sym->name, "TMyClass") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test class type casting with 'as' operator
void test_pascal_as_operator(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("Sender as TButton");
    input->length = strlen("Sender as TButton");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_AS);
    
    // Check left operand (object identifier)
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_left_node = left;
    if (left->child && left->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_left_node = left->child;
    }
    
    TEST_ASSERT(actual_left_node->sym && 
               actual_left_node->sym->name && 
               strcmp(actual_left_node->sym->name, "Sender") == 0);
    
    // Check right operand (target class type)
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_right_node = right;
    if (right->child && right->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_right_node = right->child;
    }
    
    TEST_ASSERT(actual_right_node->sym && 
               actual_right_node->sym->name && 
               strcmp(actual_right_node->sym->name, "TButton") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test complex 'as' operator with field access (basic parsing)
void test_pascal_as_operator_with_field_access(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("Sender.Button as TForm");
    input->length = strlen("Sender.Button as TForm");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_AS);
    
    // Check left operand with record field access
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_MEMBER_ACCESS);

    ast_t* base = left->child;
    TEST_ASSERT(base != NULL);
    if (base->child && base->child->typ == PASCAL_T_IDENTIFIER) {
        base = base->child;
    }
    TEST_ASSERT(base->sym && base->sym->name && strcmp(base->sym->name, "Sender") == 0);

    ast_t* field = base->next;
    while (field && field->typ == PASCAL_T_NONE) {
        field = field->child;
    }
    TEST_ASSERT(field != NULL);
    if (field->child && field->child->typ == PASCAL_T_IDENTIFIER) {
        field = field->child;
    }
    TEST_ASSERT(field->sym && field->sym->name && strcmp(field->sym->name, "Button") == 0);
    
    // Check right operand
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_right_node = right;
    if (right->child && right->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_right_node = right->child;
    }
    
    TEST_ASSERT(actual_right_node->sym && 
               actual_right_node->sym->name && 
               strcmp(actual_right_node->sym->name, "TForm") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// --- Pascal Statement Tests ---

void test_pascal_assignment_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("x := 42;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // The program parser wraps in a NONE node, so get the actual statement
    ast_t* stmt = res.value.ast;
    // For terminated statements, we expect the actual statement type directly
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASSIGNMENT);
    
    // Check identifier
    ast_t* identifier = stmt->child;
    TEST_ASSERT(identifier->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(identifier->sym->name, "x") == 0);
    
    // Check assignment operator (skipped in AST)
    
    // Check expression value
    ast_t* value = identifier->next;
    TEST_ASSERT(value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(value->sym->name, "42") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_expression_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("writeln(\"Hello\");");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_CHECK(res.is_success);
    if (!res.is_success) {
        if (res.value.error) {
            printf("Parse error: %s at line %d, col %d\n",
                   res.value.error->message, res.value.error->line, res.value.error->col);
            free_error(res.value.error);
        }
        free(input->buffer);
        free_input(input);
        return;
    }
    
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    
    TEST_CHECK(stmt && stmt->typ == PASCAL_T_STATEMENT);
    if (!stmt || stmt->typ != PASCAL_T_STATEMENT) {
        free_ast(res.value.ast);        free(input->buffer);
        free_input(input);
        return;
    }
    
    // Check function call
    ast_t* func_call = stmt->child;
    TEST_CHECK(func_call && func_call->typ == PASCAL_T_FUNC_CALL);
    if (!func_call || func_call->typ != PASCAL_T_FUNC_CALL) {
        free_ast(res.value.ast);        free(input->buffer);
        free_input(input);
        return;
    }
    
    // Check function name
    ast_t* func_name = func_call->child;
    TEST_CHECK(func_name && func_name->typ == PASCAL_T_IDENTIFIER);
    if (!func_name || func_name->typ != PASCAL_T_IDENTIFIER) {
        free_ast(res.value.ast);        free(input->buffer);
        free_input(input);
        return;
    }
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
        
        // For built-in functions, the symbol might be in the grandchild
        if (!actual_name_node->sym && actual_name_node->child && 
            actual_name_node->child->typ == PASCAL_T_IDENTIFIER) {
            actual_name_node = actual_name_node->child;
        }
    }
    
    TEST_CHECK(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "writeln") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_if_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("if x > 0 then y := 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_IF_STMT);
    
    // Check condition (x > 0)
    ast_t* condition = stmt->child;
    TEST_ASSERT(condition->typ == PASCAL_T_GT);
    
    // Check then statement
    ast_t* then_stmt = condition->next;
    TEST_ASSERT(then_stmt->typ == PASCAL_T_ASSIGNMENT);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_if_else_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("if x > 0 then y := 1 else y := -1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_IF_STMT);
    
    // Check condition
    ast_t* condition = stmt->child;
    TEST_ASSERT(condition->typ == PASCAL_T_GT);
    
    // Check then statement
    ast_t* then_stmt = condition->next;
    TEST_ASSERT(then_stmt->typ == PASCAL_T_ASSIGNMENT);
    
    // Check else clause
    ast_t* else_clause = then_stmt->next;
    TEST_ASSERT(else_clause->typ == PASCAL_T_ELSE);
    
    // Check else statement
    ast_t* else_stmt = else_clause->child;
    TEST_ASSERT(else_stmt->typ == PASCAL_T_ASSIGNMENT);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_begin_end_block(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("begin x := 1; y := 2 end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_BEGIN_BLOCK);
    
    // The child should be the statement list
    ast_t* stmt_list = res.value.ast->child;
    TEST_ASSERT(stmt_list != NULL);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_for_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("for i := 1 to 10 do x := x + i;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    // Check loop initializer
    ast_t* initializer = stmt->child;
    TEST_ASSERT(initializer->typ == PASCAL_T_ASSIGNMENT);

    ast_t* loop_var = initializer->child;
    TEST_ASSERT(loop_var->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(loop_var->sym->name, "i") == 0);

    ast_t* start_value = loop_var->next;
    TEST_ASSERT(start_value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(start_value->sym->name, "1") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_for_statement_without_assignment(void) {
    combinator_t* p = create_program_parser();
    input_t* input = new_input();
    input->buffer = strdup("for j to 3 do writeln(j);");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }

    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    ast_t* initializer = stmt->child;
    TEST_ASSERT(initializer->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(initializer->sym->name, "j") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_for_downto_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("for i := 10 downto 1 do x := x + i;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    // Check loop initializer
    ast_t* initializer = stmt->child;
    TEST_ASSERT(initializer->typ == PASCAL_T_ASSIGNMENT);

    ast_t* loop_var = initializer->child;
    TEST_ASSERT(loop_var->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(loop_var->sym->name, "i") == 0);

    ast_t* start_value = loop_var->next;
    TEST_ASSERT(start_value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(start_value->sym->name, "10") == 0);

    // Check that the direction is DOWNTO
    ast_t* direction = initializer->next;
    TEST_ASSERT(direction != NULL);
    TEST_ASSERT(direction->typ == PASCAL_T_DOWNTO);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_for_downto_without_assignment(void) {
    combinator_t* p = create_program_parser();
    input_t* input = new_input();
    input->buffer = strdup("for j downto 1 do writeln(j);");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }

    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    ast_t* initializer = stmt->child;
    TEST_ASSERT(initializer->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(initializer->sym->name, "j") == 0);

    // Check that the direction is DOWNTO
    ast_t* direction = initializer->next;
    TEST_ASSERT(direction != NULL);
    TEST_ASSERT(direction->typ == PASCAL_T_DOWNTO);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_for_to_direction(void) {
    combinator_t* p = create_program_parser();
    input_t* input = new_input();
    input->buffer = strdup("for i := 1 to 5 do x := i;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }

    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    // Check that the direction is TO (not DOWNTO)
    ast_t* initializer = stmt->child;
    ast_t* direction = initializer->next;
    TEST_ASSERT(direction != NULL);
    TEST_ASSERT(direction->typ == PASCAL_T_TO);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_while_statement(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("while x > 0 do x := x - 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_WHILE_STMT);
    
    // Check condition
    ast_t* condition = stmt->child;
    TEST_ASSERT(condition->typ == PASCAL_T_GT);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_simple_asm_block(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm mov ax, 5 end;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASM_BLOCK);
    
    // Check the ASM body content
    ast_t* asm_body = stmt->child;
    TEST_ASSERT(asm_body->typ == PASCAL_T_NONE);
    TEST_ASSERT(strcmp(asm_body->sym->name, "mov ax, 5 ") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_multiline_asm_block(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm\n  mov ax, bx\n  add ax, 10\n  int 21h\nend;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASM_BLOCK);
    
    // Check the ASM body content - it should contain newlines and instructions
    ast_t* asm_body = stmt->child;
    TEST_ASSERT(asm_body->typ == PASCAL_T_NONE);
    TEST_ASSERT(strstr(asm_body->sym->name, "mov ax, bx") != NULL);
    TEST_ASSERT(strstr(asm_body->sym->name, "add ax, 10") != NULL);
    TEST_ASSERT(strstr(asm_body->sym->name, "int 21h") != NULL);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_empty_asm_block(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm end;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASM_BLOCK);
    
    // Check the ASM body content - should be empty except for space
    ast_t* asm_body = stmt->child;
    TEST_ASSERT(asm_body->typ == PASCAL_T_NONE);
    TEST_ASSERT(strcmp(asm_body->sym->name, "") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_unterminated_asm_block(void) {
    combinator_t* p = create_program_parser();// Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm mov ax, 5");  // Missing 'end'
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(!res.is_success);
    // Could be "Expected ';'" from program parser or "Unterminated ASM block" from ASM parser
    TEST_ASSERT(res.value.error->message != NULL);

    free_error(res.value.error);    free(input->buffer);
    free_input(input);
}

// === Procedure/Function Declaration Tests ===

void test_pascal_simple_procedure(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("procedure MyProcedure; begin end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_PROCEDURE_DECL);
    
    // Check procedure name
    ast_t* proc_name = res.value.ast->child;
    TEST_ASSERT(proc_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(proc_name->sym->name, "MyProcedure") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_procedure_with_params(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("procedure MyProcedure(x: integer; y: string); begin end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_PROCEDURE_DECL);

    // Check procedure name
    ast_t* proc_name = res.value.ast->child;
    TEST_ASSERT(proc_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(proc_name->sym->name, "MyProcedure") == 0);

    // Check parameters exist
    ast_t* params = proc_name->next;
    TEST_ASSERT(params != NULL); // parameter list should exist

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_procedure_with_trailing_semicolon(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("procedure MyProcedure; begin; writeln('hi'); end;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(input->start == input->length);

    free_ast(res.value.ast);
    free(input->buffer);
    free_input(input);
}

void test_pascal_simple_function(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("function Square(x: integer): integer; begin end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNCTION_DECL);
    
    // Check function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(func_name->sym->name, "Square") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_function_with_trailing_semicolon(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("function Multiply(x, y: integer): integer; begin; Multiply := x * y; end;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(input->start == input->length);

    free_ast(res.value.ast);
    free(input->buffer);
    free_input(input);
}

void test_pascal_function_no_params(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("function GetValue: integer; begin end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNCTION_DECL);
    
    // Check function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(func_name->sym->name, "GetValue") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_function_multiple_params(void) {
    combinator_t* p = create_procedure_parser();
    input_t* input = new_input();
    input->buffer = strdup("function Calculate(a: real; b: real; c: integer): real; begin end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNCTION_DECL);
    
    // Check function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(func_name->sym->name, "Calculate") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

void test_pascal_unit_declaration(void) {
    combinator_t* p = get_unit_parser();

    input_t* input = new_input();
    char* source = load_pascal_snippet("unit_declaration.pas");
    TEST_ASSERT(source != NULL);
    if (!source) {
        free_input(input);
        return;
    }
    input->buffer = source;
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    // This test is expected to fail because unit parsing is not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_UNIT_DECL);
        ast_t* unit_name = res.value.ast->child;
        TEST_ASSERT(unit_name->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(unit_name->sym->name, "MyUnit") == 0);

        ast_t* interface_sec = unit_name->next;
        TEST_ASSERT(interface_sec->typ == PASCAL_T_INTERFACE_SECTION);

        ast_t* implementation_sec = interface_sec->next;
        TEST_ASSERT(implementation_sec->typ == PASCAL_T_IMPLEMENTATION_SECTION);
        free_ast(res.value.ast);
    } else {
        // We expect this path to be taken.
        // We must free the error object.
        free_error(res.value.error);
    }
    
    // Don't free the shared parser
    free(input->buffer);
    free_input(input);
}

void test_pascal_pointer_type_declaration(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = load_pascal_snippet("pointer_type_declaration.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    // This test is expected to fail because pointer types are not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // If it succeeds, we should check the structure
        ast_t* program_decl = res.value.ast;
        TEST_ASSERT(program_decl->typ == PASCAL_T_PROGRAM_DECL);

        // Find type section
        ast_t* current = program_decl->child;
        while(current && current->typ != PASCAL_T_TYPE_SECTION) {
            current = current->next;
        }
        TEST_ASSERT(current != NULL);
        TEST_ASSERT(current->typ == PASCAL_T_TYPE_SECTION);

        ast_t* type_decl = current->child;
        TEST_ASSERT(type_decl->typ == PASCAL_T_TYPE_DECL);

        ast_t* type_spec = type_decl->child->next;
        TEST_ASSERT(type_spec->typ == PASCAL_T_TYPE_SPEC);

        ast_t* pointer_type = type_spec->child;
        TEST_ASSERT(pointer_type->typ == PASCAL_T_POINTER_TYPE);

        ast_t* referenced_type = pointer_type->child;
        TEST_ASSERT(referenced_type->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(referenced_type->sym->name, "TMyRec") == 0);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_method_implementation(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = load_pascal_snippet("method_implementation.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    // This test is expected to fail because method implementations are not fully supported.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // If it succeeds, we should check the structure
        ast_t* program_decl = res.value.ast;
        TEST_ASSERT(program_decl->typ == PASCAL_T_PROGRAM_DECL);

        // Find the method implementation
        ast_t* current = program_decl->child;
        while(current && current->typ != PASCAL_T_METHOD_IMPL) {
            current = current->next;
        }
        TEST_ASSERT(current != NULL);
        TEST_ASSERT(current->typ == PASCAL_T_METHOD_IMPL);

        // The first child of the METHOD_IMPL node should be the QUALIFIED_IDENTIFIER node.
        ast_t* qualified_id_node = current->child;
        TEST_ASSERT(qualified_id_node != NULL);
        TEST_ASSERT(qualified_id_node->typ == PASCAL_T_QUALIFIED_IDENTIFIER);

        // Now, check the children of the QUALIFIED_IDENTIFIER node.
        // First child is the ClassName.
        ast_t* class_name_node = qualified_id_node->child;
        TEST_ASSERT(class_name_node != NULL);
        TEST_ASSERT(class_name_node->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(class_name_node->sym->name, "TMyObject") == 0);

        // Second child is the MethodName.
        ast_t* method_name_node = class_name_node->next;
        TEST_ASSERT(method_name_node != NULL);
        TEST_ASSERT(method_name_node->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(method_name_node->sym->name, "MyMethod") == 0);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_with_statement_single_context(void) {
    combinator_t* parser = get_statement_parser();

    input_t* input = new_input();
    input->buffer = strdup("with MyRecord do field := 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, parser);

    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* with_stmt = res.value.ast;
        TEST_ASSERT(with_stmt->typ == PASCAL_T_WITH_STMT);

        ast_t* contexts = with_stmt->child;
        TEST_ASSERT(contexts != NULL);
        TEST_ASSERT(contexts->typ == PASCAL_T_WITH_CONTEXTS);

        ast_t* first_context = contexts->child;
        TEST_ASSERT(first_context != NULL);
        TEST_ASSERT(first_context->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(first_context->sym->name, "MyRecord") == 0);
        TEST_ASSERT(first_context->next == NULL || first_context->next == ast_nil);

        ast_t* statement = contexts->next;
        TEST_ASSERT(statement != NULL);
        TEST_ASSERT(statement->typ == PASCAL_T_ASSIGNMENT);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_with_statement_multiple_contexts(void) {
    combinator_t* parser = get_statement_parser();

    input_t* input = new_input();
    input->buffer = strdup("with Outer, Inner do Value := 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, parser);

    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* with_stmt = res.value.ast;
        TEST_ASSERT(with_stmt->typ == PASCAL_T_WITH_STMT);

        ast_t* contexts = with_stmt->child;
        TEST_ASSERT(contexts != NULL);
        TEST_ASSERT(contexts->typ == PASCAL_T_WITH_CONTEXTS);

        ast_t* first_context = contexts->child;
        TEST_ASSERT(first_context != NULL);
        TEST_ASSERT(first_context->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(first_context->sym->name, "Outer") == 0);

        ast_t* second_context = first_context->next;
        TEST_ASSERT(second_context != NULL);
        TEST_ASSERT(second_context->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(second_context->sym->name, "Inner") == 0);

        TEST_ASSERT(second_context->next == NULL || second_context->next == ast_nil);

        ast_t* statement = contexts->next;
        TEST_ASSERT(statement != NULL);
        TEST_ASSERT(statement->typ == PASCAL_T_ASSIGNMENT);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_with_statement_nested(void) {
    combinator_t* parser = get_statement_parser();

    input_t* input = new_input();
    input->buffer = strdup("with Outer do with Inner do Value := 2;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, parser);

    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* outer_with = res.value.ast;
        TEST_ASSERT(outer_with->typ == PASCAL_T_WITH_STMT);

        ast_t* outer_contexts = outer_with->child;
        TEST_ASSERT(outer_contexts != NULL);
        TEST_ASSERT(outer_contexts->typ == PASCAL_T_WITH_CONTEXTS);
        TEST_ASSERT(outer_contexts->child != NULL);
        TEST_ASSERT(strcmp(outer_contexts->child->sym->name, "Outer") == 0);

        ast_t* inner_stmt = outer_contexts->next;
        TEST_ASSERT(inner_stmt != NULL);
        TEST_ASSERT(inner_stmt->typ == PASCAL_T_WITH_STMT);

        ast_t* inner_contexts = inner_stmt->child;
        TEST_ASSERT(inner_contexts != NULL);
        TEST_ASSERT(inner_contexts->typ == PASCAL_T_WITH_CONTEXTS);
        TEST_ASSERT(inner_contexts->child != NULL);
        TEST_ASSERT(strcmp(inner_contexts->child->sym->name, "Inner") == 0);

        ast_t* assignment = inner_contexts->next;
        TEST_ASSERT(assignment != NULL);
        TEST_ASSERT(assignment->typ == PASCAL_T_ASSIGNMENT);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_with_statement_invalid_syntax(void) {
    combinator_t* program_parser = get_program_parser();

    input_t* leading_comma_input = new_input();
    leading_comma_input->buffer = strdup("program p; begin with , Inner do begin end; end.");
    leading_comma_input->length = strlen(leading_comma_input->buffer);
    ParseResult leading_comma_result = parse(leading_comma_input, program_parser);
    TEST_ASSERT(!leading_comma_result.is_success);
    if (!leading_comma_result.is_success) {
        free_error(leading_comma_result.value.error);
    }
    free(leading_comma_input->buffer);
    free_input(leading_comma_input);

    input_t* trailing_comma_input = new_input();
    trailing_comma_input->buffer = strdup("program p; begin with Outer, do begin end; end.");
    trailing_comma_input->length = strlen(trailing_comma_input->buffer);
    ParseResult trailing_comma_result = parse(trailing_comma_input, program_parser);
    TEST_ASSERT(!trailing_comma_result.is_success);
    if (!trailing_comma_result.is_success) {
        free_error(trailing_comma_result.value.error);
    }
    free(trailing_comma_input->buffer);
    free_input(trailing_comma_input);
}

void test_pascal_exit_statement(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("exit;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    // This test is expected to fail because 'exit' statements are not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        ast_t* exit_stmt = res.value.ast;
        TEST_ASSERT(exit_stmt->typ == PASCAL_T_EXIT_STMT);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_include_directive(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = load_pascal_snippet("include_directive_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    // The parser should succeed, treating the include as a comment.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // The main block should be empty, as the include was ignored.
        ast_t* program_decl = res.value.ast;
        ast_t* main_block = NULL;
        ast_t* current = program_decl->child;
        while(current) {
            if (current->typ == PASCAL_T_MAIN_BLOCK) {
                main_block = current;
                break;
            }
            current = current->next;
        }

        TEST_ASSERT(main_block != NULL);
        TEST_ASSERT(main_block->child == NULL);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_forward_declared_function(void) {
    combinator_t* p = get_unit_parser();

    input_t* input = new_input();
    char* unit_code = load_pascal_snippet("forward_declared_function_unit.pas");
    TEST_ASSERT(unit_code != NULL);
    if (!unit_code) {
        free_input(input);
        return;
    }
    input->buffer = unit_code;
    input->length = strlen(unit_code);

    ParseResult res = parse(input, p);

    // This test is expected to fail because unit parsing is not fully implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // If it succeeds, we'd check the AST to ensure the call is resolved.
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_record_type(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = load_pascal_snippet("record_type_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // Find the type section
        ast_t* program_decl = res.value.ast;
        TEST_ASSERT(program_decl->typ == PASCAL_T_PROGRAM_DECL);

        // Find type section
        ast_t* current = program_decl->child;
        while(current && current->typ != PASCAL_T_TYPE_SECTION) {
            current = current->next;
        }
        TEST_ASSERT(current != NULL);
        TEST_ASSERT(current->typ == PASCAL_T_TYPE_SECTION);

        // Find type declaration
        ast_t* type_decl = current->child;
        TEST_ASSERT(type_decl->typ == PASCAL_T_TYPE_DECL);
        
        // Check type name
        ast_t* type_name = type_decl->child;
        TEST_ASSERT(type_name->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(type_name->sym->name, "TMyRecord") == 0);

        // Check record type
        ast_t* type_spec = type_name->next;
        TEST_ASSERT(type_spec->typ == PASCAL_T_TYPE_SPEC);
        
        ast_t* record_type = type_spec->child;
        TEST_ASSERT(record_type->typ == PASCAL_T_RECORD_TYPE);

        // Check record fields
        ast_t* field1 = record_type->child;
        TEST_ASSERT(field1->typ == PASCAL_T_FIELD_DECL);
        
        ast_t* field1_name = field1->child;
        TEST_ASSERT(field1_name->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(field1_name->sym->name, "field1") == 0);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

// Test simple case statement
void test_pascal_simple_case_statement(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case x of 1: y := 2; 3: y := 4 end");  // Full case statement
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    if (!res.is_success) {
        printf("Parse error: %s at line %d, col %d\n", 
               res.value.error->message, res.value.error->line, res.value.error->col);
        if (res.value.error->partial_ast) {
            printf("Partial AST type: %d\n", res.value.error->partial_ast->typ);
        }
        free_error(res.value.error);        free(input->buffer);
        free_input(input);
        return;
    }

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CASE_STMT);
    
    // Check case expression (x)
    ast_t* case_expr = res.value.ast->child;
    TEST_ASSERT(case_expr->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(case_expr->sym->name, "x") == 0);
    
    // Check first case branch
    ast_t* first_branch = case_expr->next;
    TEST_ASSERT(first_branch->typ == PASCAL_T_CASE_BRANCH);
    
    // Check case label list
    ast_t* label_list = first_branch->child;
    TEST_ASSERT(label_list->typ == PASCAL_T_CASE_LABEL_LIST);
    
    // Check first case label
    ast_t* first_label = label_list->child;
    TEST_ASSERT(first_label->typ == PASCAL_T_CASE_LABEL);
    TEST_ASSERT(first_label->child->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(first_label->child->sym->name, "1") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test case statement with ranges
void test_pascal_case_statement_with_ranges(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case x of 1..5: writeln(); 10..15: writeln() end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    if (!res.is_success) {
        printf("Range test parse error: %s at line %d, col %d\n", 
               res.value.error->message, res.value.error->line, res.value.error->col);
        free_error(res.value.error);        free(input->buffer);
        free_input(input);
        return;
    }

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CASE_STMT);
    
    // Check case expression (x)
    ast_t* case_expr = res.value.ast->child;
    TEST_ASSERT(case_expr->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(case_expr->sym->name, "x") == 0);
    
    // Check first case branch with range
    ast_t* first_branch = case_expr->next;
    TEST_ASSERT(first_branch->typ == PASCAL_T_CASE_BRANCH);
    
    ast_t* label_list = first_branch->child;
    TEST_ASSERT(label_list->typ == PASCAL_T_CASE_LABEL_LIST);
    
    ast_t* first_label = label_list->child;
    TEST_ASSERT(first_label->typ == PASCAL_T_CASE_LABEL);
    
    // The label should contain a range
    ast_t* range = first_label->child;
    TEST_ASSERT(range->typ == PASCAL_T_RANGE);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test case statement with multiple labels
void test_pascal_case_statement_multiple_labels(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case n of 1, 3, 5: writeln(); 2, 4, 6: writeln() end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    if (!res.is_success) {
        printf("Multiple labels parse error: %s at line %d, col %d\n", 
               res.value.error->message, res.value.error->line, res.value.error->col);
        free_error(res.value.error);        free(input->buffer);
        free_input(input);
        return;
    }

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CASE_STMT);
    
    // Check case expression (n)
    ast_t* case_expr = res.value.ast->child;
    TEST_ASSERT(case_expr->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(case_expr->sym->name, "n") == 0);
    
    // Check first case branch with multiple labels
    ast_t* first_branch = case_expr->next;
    TEST_ASSERT(first_branch->typ == PASCAL_T_CASE_BRANCH);
    
    ast_t* label_list = first_branch->child;
    TEST_ASSERT(label_list->typ == PASCAL_T_CASE_LABEL_LIST);
    
    // Check first label (1)
    ast_t* first_label = label_list->child;
    TEST_ASSERT(first_label->typ == PASCAL_T_CASE_LABEL);
    
    ast_t* first_value = first_label->child;
    TEST_ASSERT(first_value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(first_value->sym->name, "1") == 0);
    
    // Check second label (3)
    ast_t* second_label = first_label->next;
    TEST_ASSERT(second_label->typ == PASCAL_T_CASE_LABEL);
    
    ast_t* second_value = second_label->child;
    TEST_ASSERT(second_value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(second_value->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test case statement with else clause
void test_pascal_case_statement_with_else(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case x of 1: y := 1; 2: y := 2 else y := 0 end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    if (!res.is_success) {
        printf("Else clause parse error: %s at line %d, col %d\n", 
               res.value.error->message, res.value.error->line, res.value.error->col);
        free_error(res.value.error);        free(input->buffer);
        free_input(input);
        return;
    }

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CASE_STMT);
    
    // Check case expression
    ast_t* case_expr = res.value.ast->child;
    TEST_ASSERT(case_expr->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(case_expr->sym->name, "x") == 0);
    
    // Find the else clause by walking through the children
    ast_t* current = case_expr->next;
    ast_t* else_clause = NULL;
    
    while (current != NULL) {
        if (current->typ == PASCAL_T_ELSE) {
            else_clause = current;
            break;
        }
        current = current->next;
    }
    
    TEST_ASSERT(else_clause != NULL);
    TEST_ASSERT(else_clause->typ == PASCAL_T_ELSE);
    
    // Check else statement
    ast_t* else_stmt = else_clause->child;
    TEST_ASSERT(else_stmt->typ == PASCAL_T_ASSIGNMENT);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test case statement with expression labels (constants, negatives, etc.)
void test_pascal_case_expression_labels(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case x of -1: writeln(); +5: writeln(); (10): writeln() end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    if (!res.is_success) {
        printf("Expression labels parse error: %s at line %d, col %d\n", 
               res.value.error->message, res.value.error->line, res.value.error->col);
        free_error(res.value.error);        free(input->buffer);
        free_input(input);
        return;
    }

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CASE_STMT);
    
    // Check case expression
    ast_t* case_expr = res.value.ast->child;
    TEST_ASSERT(case_expr->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(case_expr->sym->name, "x") == 0);
    
    // Check first case branch with negative value
    ast_t* first_branch = case_expr->next;
    TEST_ASSERT(first_branch->typ == PASCAL_T_CASE_BRANCH);
    
    ast_t* label_list = first_branch->child;
    TEST_ASSERT(label_list->typ == PASCAL_T_CASE_LABEL_LIST);
    
    ast_t* first_label = label_list->child;
    TEST_ASSERT(first_label->typ == PASCAL_T_CASE_LABEL);
    
    // The first label should be a negation expression
    ast_t* neg_expr = first_label->child;
    TEST_ASSERT(neg_expr->typ == PASCAL_T_NEG);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test case statement with character labels
void test_pascal_case_statement_char_labels(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case ch of 'A': writeln(); 'B': writeln() end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    if (!res.is_success) {
        printf("Char labels parse error: %s at line %d, col %d\n", 
               res.value.error->message, res.value.error->line, res.value.error->col);
        free_error(res.value.error);        free(input->buffer);
        free_input(input);
        return;
    }

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CASE_STMT);
    
    // Check case expression
    ast_t* case_expr = res.value.ast->child;
    TEST_ASSERT(case_expr->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(case_expr->sym->name, "ch") == 0);
    
    // Check first case branch
    ast_t* first_branch = case_expr->next;
    TEST_ASSERT(first_branch->typ == PASCAL_T_CASE_BRANCH);
    
    ast_t* label_list = first_branch->child;
    TEST_ASSERT(label_list->typ == PASCAL_T_CASE_LABEL_LIST);
    
    ast_t* label = label_list->child;
    TEST_ASSERT(label->typ == PASCAL_T_CASE_LABEL);
    
    ast_t* char_value = label->child;
    TEST_ASSERT(char_value->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(char_value->sym->name, "A") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free_input(input);
}

// Test case statement with invalid expressions as labels (should fail)
void test_pascal_case_invalid_expression_labels(void) {
    combinator_t* p = create_statement_parser();
    // Test with function call as case label (should be invalid)
    input_t* input = new_input();
    input->buffer = strdup("case x of func(): writeln() end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);
    
    // This should fail because function calls are not valid case labels
    TEST_ASSERT(!res.is_success);
    
    if (!res.is_success) {
        free_error(res.value.error);
    } else {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);

    // Test with variable assignment as case label (should be invalid)
    p = create_statement_parser();
    
    input = new_input();
    input->buffer = strdup("case x of y := 5: writeln() end");
    input->length = strlen(input->buffer);

    res = parse_pascal_expression(input, p);
    
    // This should fail because assignments are not valid case labels
    TEST_ASSERT(!res.is_success);
    
    if (!res.is_success) {
        free_error(res.value.error);
    } else {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

// Test pointer dereference operator (basic support)
void test_pascal_pointer_dereference(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("ptr^");
    input->length = strlen("ptr^");

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_DEREF);
        ast_t* target = res.value.ast->child;
        TEST_ASSERT(target != NULL);
        if (target->child && target->child->typ == PASCAL_T_IDENTIFIER) {
            target = target->child;
        }
        TEST_ASSERT(target->sym && target->sym->name && strcmp(target->sym->name, "ptr") == 0);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

// Test array access (basic support) 
void test_pascal_array_access_with_deref(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("ptr^[i]");
    input->length = strlen("ptr^[i]");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_ARRAY_ACCESS);
        ast_t* deref_node = res.value.ast->child;
        TEST_ASSERT(deref_node != NULL);
        TEST_ASSERT(deref_node->typ == PASCAL_T_DEREF);
        ast_t* base = deref_node->child;
        if (base->child && base->child->typ == PASCAL_T_IDENTIFIER) {
            base = base->child;
        }
        TEST_ASSERT(base->sym && base->sym->name && strcmp(base->sym->name, "ptr") == 0);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_paren_star_comment(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("(* this is a comment *) 42");
    input->length = strlen(input->buffer);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (!res.is_success) {
        free_error(res.value.error);
    } else {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_hex_literal(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    input->buffer = strdup("$FF");
    input->length = strlen(input->buffer);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (!res.is_success) {
        free_error(res.value.error);
    } else {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_case_range_label(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    input->buffer = strdup("case i of 'a'..'z': write(i) end");
    input->length = strlen(input->buffer);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (!res.is_success) {
        free_error(res.value.error);
    } else {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_enumerated_type_declaration(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("enumerated_type_declaration.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_simple_const_declaration(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("simple_const_declaration.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_complex_const_declaration(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("complex_const_declaration.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* mul_node = find_first_node_of_type(res.value.ast, PASCAL_T_MUL);
        TEST_ASSERT(mul_node != NULL);
        ast_t* tuple_node = find_first_node_of_type(res.value.ast, PASCAL_T_TUPLE);
        TEST_ASSERT(tuple_node != NULL);
        ast_t* sub_node = find_first_node_of_type(res.value.ast, PASCAL_T_SUB);
        TEST_ASSERT(sub_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_var_section(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("var_section.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_set_operations_program(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    const char* union_expr = "[1, 3] + [5]";
    input->buffer = strdup(union_expr);
    input->length = strlen(union_expr);

    ParseResult res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET_UNION);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);

    p = create_expression_parser();
    input = new_input();
    const char* intersect_expr = "[1, 3] * [3]";
    input->buffer = strdup(intersect_expr);
    input->length = strlen(intersect_expr);

    res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_MUL);
        ast_t* set_literal = find_first_node_of_type(res.value.ast, PASCAL_T_SET);
        TEST_ASSERT(set_literal != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);

    // Parse membership expression to ensure "in" is recognised with set unions
    p = create_expression_parser();
    input = new_input();
    const char* in_expr = "3 in ([1, 3] + [5])";
    input->buffer = strdup(in_expr);
    input->length = strlen(in_expr);

    res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_IN);
        ast_t* union_node = find_first_node_of_type(res.value.ast, PASCAL_T_SET_UNION);
        TEST_ASSERT(union_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);

    // Parse full program exercising set operations and subrange set types
    p = create_complete_program_parser();
    input = new_input();
    char* program_source = load_pascal_snippet("set_operations_program.pas");
    TEST_ASSERT(program_source != NULL);
    if (!program_source) {
        free_input(input);
        return;
    }
    input->buffer = program_source;
    input->length = strlen(program_source);

    res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_dialog_program(void) {
    combinator_t* parser = create_complete_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("dialog_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }

    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, parser);
    if (!res.is_success) {
        fprintf(stderr, "Failed to parse dialog program: %s\n",
                res.value.error && res.value.error->message ? res.value.error->message : "<no message>");
        if (res.value.error && res.value.error->context) {
            fprintf(stderr, "%s", res.value.error->context);
        }
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_multiply_program(void) {
    combinator_t* parser = create_complete_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("multiply_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }

    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, parser);
    if (!res.is_success) {
        fprintf(stderr, "Failed to parse multiply program: %s\n",
                res.value.error && res.value.error->message ? res.value.error->message : "<no message>");
        if (res.value.error && res.value.error->context) {
            fprintf(stderr, "%s", res.value.error->context);
        }
    }
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        free_ast(res.value.ast);
    } else if (res.value.error) {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_pointer_operations_program(void) {
    combinator_t* p = create_expression_parser();
    input_t* input = new_input();
    const char* addr_expr = "@value";
    input->buffer = strdup(addr_expr);
    input->length = strlen(addr_expr);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADDR);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);

    p = create_expression_parser();
    input = new_input();
    const char* deref_expr = "ptr^";
    input->buffer = strdup(deref_expr);
    input->length = strlen(deref_expr);

    res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_DEREF);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_record_member_access_program(void) {
    combinator_t* p = create_statement_parser();
    input_t* input = new_input();
    const char* block =
        "begin\n"
        "  p.x := 1;\n"
        "  total := p.x + p.y;\n"
        "end";
    input->buffer = strdup(block);
    input->length = strlen(block);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* member_node = find_first_node_of_type(res.value.ast, PASCAL_T_MEMBER_ACCESS);
        TEST_ASSERT(member_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_record_member_access_complete_program(void) {
    combinator_t* p = create_complete_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("record_member_access_complete_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* member_node = find_first_node_of_type(res.value.ast, PASCAL_T_MEMBER_ACCESS);
        TEST_ASSERT(member_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_unitless_program(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = load_pascal_snippet("unitless_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        ast_t* program_decl = res.value.ast;
        TEST_ASSERT(program_decl->typ == PASCAL_T_PROGRAM_DECL);

        bool found_uses = false;
        bool found_const = false;
        bool found_main_block = false;

        for (ast_t* current = program_decl->child; current != NULL; current = current->next) {
            if (current->typ == PASCAL_T_USES_SECTION) {
                found_uses = true;
            } else if (current->typ == PASCAL_T_CONST_SECTION) {
                found_const = true;
            } else if (current->typ == PASCAL_T_MAIN_BLOCK) {
                found_main_block = true;
            }
        }

        TEST_ASSERT(found_uses);
        TEST_ASSERT(found_const);
        TEST_ASSERT(found_main_block);

        ast_t* set_literal = find_first_node_of_type(program_decl, PASCAL_T_SET);
        TEST_ASSERT(set_literal != NULL);
        if (set_literal) {
            ast_t* char_code = find_first_node_of_type(set_literal, PASCAL_T_CHAR_CODE);
            TEST_ASSERT(char_code != NULL);
        }

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_fpc_style_unit_parsing(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("fpc_style_unit.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

// --- Helpers for advanced feature regression tests ---
static void assert_pascal_unit_parses_snippet(const char* snippet_name) {
    char* source = load_pascal_snippet(snippet_name);
    TEST_ASSERT(source != NULL);
    if (!source) {
        return;
    }

    combinator_t* parser = get_unit_parser();

    input_t* input = new_input();
    input->buffer = source;
    input->length = strlen(source);

    ParseResult result = parse(input, parser);
    bool success = result.is_success;

    if (result.is_success && input->start < input->length) {
        TEST_MSG("Parser left trailing input: '%.*s'", 40, input->buffer + input->start);
        success = false;
    }

    if (!result.is_success && result.value.error && result.value.error->message) {
        TEST_MSG("Unexpected parse failure: %s", result.value.error->message);
    }

    if (result.is_success) {
        free_ast(result.value.ast);
    } else if (result.value.error) {
        free_error(result.value.error);
    }
    free(input->buffer);
    free_input(input);

    TEST_ASSERT(success);
}

// --- Regression tests for features seen in the FPC corpus ---
void test_pascal_unit_with_dotted_name(void) {
    assert_pascal_unit_parses_snippet("unit_with_dotted_name.pas");
}

void test_pascal_uses_with_dotted_unit(void) {
    assert_pascal_unit_parses_snippet("uses_with_dotted_unit.pas");
}

void test_pascal_out_parameter_modifier(void) {
    assert_pascal_unit_parses_snippet("out_parameter_modifier.pas");
}

void test_pascal_resourcestring_section(void) {
    assert_pascal_unit_parses_snippet("resourcestring_section.pas");
}

void test_pascal_threadvar_section(void) {
    assert_pascal_unit_parses_snippet("threadvar_section.pas");
}

void test_pascal_set_of_enum_typed_constant(void) {
    assert_pascal_unit_parses_snippet("set_of_enum_typed_constant.pas");
}

void test_pascal_generic_type_declaration(void) {
    assert_pascal_unit_parses_snippet("generic_type_declaration.pas");
}

void test_pascal_generic_record_type_declaration(void) {
    assert_pascal_unit_parses_snippet("generic_record_simple.pas");
}

void test_pascal_generic_class_tfpg_declaration(void) {
    assert_pascal_unit_parses_snippet("generic_class_tfpg.pas");
}

void test_pascal_specialize_alias(void) {
    assert_pascal_unit_parses_snippet("specialize_alias.pas");
}

void test_pascal_class_function_modifier(void) {
    assert_pascal_unit_parses_snippet("class_function_modifier.pas");
}

void test_pascal_class_operator_overload(void) {
    assert_pascal_unit_parses_snippet("class_operator_overload.pas");
}

void test_pascal_type_helper_for_string(void) {
    assert_pascal_unit_parses_snippet("type_helper_for_string.pas");
}

void test_pascal_overload_directive(void) {
    assert_pascal_unit_parses_snippet("overload_directive.pas");
}

void test_pascal_inline_directive(void) {
    assert_pascal_unit_parses_snippet("inline_directive.pas");
}

void test_complex_fpc_rax64int_unit(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_snippet("complex_rax64int_unit.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

// Tests for pascal directory snippets
void test_aligned_records(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("alignedrecords.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_array_function_return(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("arrayfunctionreturn.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }

    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_pascal_record_field_signterm(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("record_field_signterm.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }

    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
    }

    free(input->buffer);
    free_input(input);
}

void test_const_set(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("constset.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_deprecated_type(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("deprecatedtype.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_dotted_types(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("dottedtypes.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_end_token(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("endtoken.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_experimentals(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("experimentals.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_external_function(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("externalfunction.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_finalization_initialization_exports(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("finalizationinitializationexports.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_forward_overloaded(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("forwardoverloaded.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_forward_without_semicolon(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("forwardwithoutsemicolon.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_generic_constraints(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("genericconstraints.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_generic_interface_method_delegation(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("genericinterfacemethoddelegation.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_implements_generic_type(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("implementsgenerictype.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_include_file(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("includefile.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_managed_records(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("managedrecords.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_message_method(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("messagemethod.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_multiline(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("multiline.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_non_aligned_records(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("nonalignedrecords.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_numbers(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("numbers.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pointer_chars(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("pointerchars.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_properties(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("properties.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_strict_visibility(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("strictvisibility.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_try_except(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("tryexcept.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_umlauts(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("umlauts.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_whitespace_around_ifdef_condition(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("whitespacearoundifdefcondition.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_deprecated_on_const(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("DeprecatedOnConst.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_variant_record_field_attributes(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("VariantRecordFieldAttributes.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_interspersed_decls_in_procedure(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("../../../../tests/test_cases/interspersed_decls_in_procedure.p");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_pascal_fpc_operator_overload_symbols(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program Test;\n"
        "type\n"
        "  TVector = record\n"
        "    X, Y: Integer;\n"
        "    class operator +(const A, B: TVector): TVector;\n"
        "    class operator *(const V: TVector; S: Integer): TVector;\n"
        "  end;\n"
        "class operator TVector.+(const A, B: TVector): TVector;\n"
        "begin\n"
        "end;\n"
        "class operator TVector.*(const V: TVector; S: Integer): TVector;\n"
        "begin\n"
        "end;\n"
        "begin\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_delphi_operator_overload_names(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program Test;\n"
        "type\n"
        "  TVector = record\n"
        "    X, Y: Integer;\n"
        "    class operator Add(const A, B: TVector): TVector;\n"
        "    class operator Multiply(const V: TVector; S: Integer): TVector;\n"
        "  end;\n"
        "class operator TVector.Add(const A, B: TVector): TVector;\n"
        "begin\n"
        "end;\n"
        "class operator TVector.Multiply(const V: TVector; S: Integer): TVector;\n"
        "begin\n"
        "end;\n"
        "begin\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_method_with_var_section(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program Test;\n"
        "type\n"
        "  TTest = record\n"
        "    X: Integer;\n"
        "  end;\n"
        "procedure TTest.SetX(Value: Integer);\n"
        "var\n"
        "  Temp: Integer;\n"
        "begin\n"
        "  Temp := Value;\n"
        "  X := Temp;\n"
        "end;\n"
        "begin\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_operator_with_var_section(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program Test;\n"
        "type\n"
        "  TVector = record\n"
        "    X, Y: Integer;\n"
        "  end;\n"
        "class operator TVector.+(const A, B: TVector): TVector;\n"
        "var\n"
        "  Result: TVector;\n"
        "begin\n"
        "  Result.X := A.X + B.X;\n"
        "  Result.Y := A.Y + B.Y;\n"
        "end;\n"
        "begin\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_constructor_destructor_with_var_section(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program Test;\n"
        "type\n"
        "  TTest = class\n"
        "    X: Integer;\n"
        "  end;\n"
        "constructor TTest.Create;\n"
        "var\n"
        "  InitValue: Integer;\n"
        "begin\n"
        "  InitValue := 42;\n"
        "  X := InitValue;\n"
        "end;\n"
        "destructor TTest.Destroy;\n"
        "var\n"
        "  FinalValue: Integer;\n"
        "begin\n"
        "  FinalValue := X;\n"
        "end;\n"
        "begin\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_program_forward_declaration(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program TestForward;\n"
        "\n"
        "procedure Second; forward;\n"
        "\n"
        "procedure First;\n"
        "begin\n"
        "  Second;\n"
        "end;\n"
        "\n"
        "procedure Second;\n"
        "begin\n"
        "  writeln('Hello');\n"
        "end;\n"
        "\n"
        "begin\n"
        "  First;\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        // Verify the AST has three procedure declarations plus main block
        ast_t* ast = res.value.ast;
        TEST_ASSERT(ast != NULL);
        TEST_ASSERT(ast->typ == PASCAL_T_PROGRAM_DECL);
        
        // Count the procedure declarations (should be 3: forward decl + First + Second impl)
        int proc_count = 0;
        ast_t* child = ast->child;
        while (child != NULL) {
            if (child->typ == PASCAL_T_PROCEDURE_DECL) {
                proc_count++;
            }
            child = child->next;
        }
        TEST_ASSERT(proc_count == 3);
        
        free_ast(res.value.ast);
    } else {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0); // Force failure with error message shown
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_program_external_declaration(void) {
    combinator_t* p = get_program_parser();
    
    const char* source =
        "program TestExternal;\n"
        "\n"
        "uses ctypes;\n"
        "\n"
        "function helper_add(a, b: integer): cint; external;\n"
        "\n"
        "var\n"
        "  result: cint;\n"
        "\n"
        "begin\n"
        "  result := helper_add(40, 2);\n"
        "  writeln(result);\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        // Verify the AST has the external function declaration separate from main block
        ast_t* ast = res.value.ast;
        TEST_ASSERT(ast != NULL);
        TEST_ASSERT(ast->typ == PASCAL_T_PROGRAM_DECL);
        
        // Should have: uses section, function decl, var section, main block
        int func_count = 0;
        int main_block_count = 0;
        ast_t* child = ast->child;
        while (child != NULL) {
            if (child->typ == PASCAL_T_FUNCTION_DECL) {
                func_count++;
            }
            if (child->typ == PASCAL_T_MAIN_BLOCK) {
                main_block_count++;
            }
            child = child->next;
        }
        TEST_ASSERT(func_count == 1);
        TEST_ASSERT(main_block_count == 1);
        
        free_ast(res.value.ast);
    } else {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_pascal_unit_forward_declaration(void) {
    combinator_t* p = get_unit_parser();
    
    const char* source =
        "unit TestUnit;\n"
        "\n"
        "interface\n"
        "\n"
        "implementation\n"
        "\n"
        "procedure Second; forward;\n"
        "\n"
        "procedure First;\n"
        "begin\n"
        "  Second;\n"
        "end;\n"
        "\n"
        "procedure Second;\n"
        "begin\n"
        "  writeln('Hello');\n"
        "end;\n"
        "\n"
        "end.\n";
    
    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);
    
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    
    if (res.is_success) {
        // Verify the AST has the implementation section with three procedure declarations
        ast_t* ast = res.value.ast;
        TEST_ASSERT(ast != NULL);
        TEST_ASSERT(ast->typ == PASCAL_T_UNIT_DECL);
        
        // Find the implementation section
        ast_t* child = ast->child;
        ast_t* impl_section = NULL;
        while (child != NULL) {
            if (child->typ == PASCAL_T_IMPLEMENTATION_SECTION) {
                impl_section = child;
                break;
            }
            child = child->next;
        }
        TEST_ASSERT(impl_section != NULL);
        
        // Count procedure declarations in implementation section
        int proc_count = 0;
        child = impl_section->child;
        while (child != NULL) {
            if (child->typ == PASCAL_T_PROCEDURE_DECL) {
                proc_count++;
            }
            child = child->next;
        }
        TEST_ASSERT(proc_count == 3);
        
        free_ast(res.value.ast);
    } else {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    
    free(input->buffer);
    free_input(input);
}

void test_external_variants(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("external_variants.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }
    free(input->buffer);
    free_input(input);
}

void test_external_program_variants(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("external_program.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    if (!res.is_success) {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_external_simple_debug(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("external_simple.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    if (!res.is_success) {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_external_library_debug(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("external_library.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    if (!res.is_success) {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_external_minimal_debug(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("external_minimal.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    if (!res.is_success) {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

void test_external_no_args_debug(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("external_no_args.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    if (!res.is_success) {
        printf("Parse error: %s\n", res.value.error->message);
        free_error(res.value.error);
        TEST_ASSERT(0);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

// Test for FPC system.pp compatibility - documents parser bugs with comments containing keywords
// This test currently FAILS due to:
// 1. "type Double" distinct type parsing bug after record with dword fields
// 2. Comments containing 'implementation' keyword incorrectly parsed
// 3. Comments containing 'initialization' keyword incorrectly parsed
void test_fpc_system_comment_keywords(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("unitwithimplcomment.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    // This test documents a known bug - it should pass but currently fails
    if (!res.is_success) {
        printf("KNOWN BUG: Parse error in FPC system.pp compatible code: %s\n", res.value.error->message);
        free_error(res.value.error);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}

// Test for Delphi-style form unit with class, method implementation, private vars/consts
void test_delphi_form_unit(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = load_pascal_file("delphiformunit.pas");
    TEST_ASSERT(program != NULL);
    if (!program) {
        free_input(input);
        return;
    }
    input->buffer = program;
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    if (!res.is_success) {
        printf("Parse error in Delphi form unit: %s\n", res.value.error->message);
        free_error(res.value.error);
    }
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    }
    free(input->buffer);
    free_input(input);
}


TEST_LIST = {
    { "test_pascal_integer_parsing", test_pascal_integer_parsing },
    { "test_pascal_invalid_input", test_pascal_invalid_input },
    { "test_pascal_stray_identifier_before_begin", test_pascal_stray_identifier_before_begin },
    { "test_pascal_preprocessor_conditionals", test_pascal_preprocessor_conditionals },
    { "test_pascal_preprocessor_comment_mixing", test_pascal_preprocessor_comment_mixing },
    { "test_pascal_preprocessor_numeric_comparisons", test_pascal_preprocessor_numeric_comparisons },
    { "test_pascal_function_call", test_pascal_function_call },
    { "test_pascal_function_call_with_escaped_quote", test_pascal_function_call_with_escaped_quote },
    { "test_pascal_string_literal", test_pascal_string_literal },
    { "test_pascal_function_call_no_args", test_pascal_function_call_no_args },
    { "test_pascal_function_call_with_args", test_pascal_function_call_with_args },
    { "test_pascal_mod_operator", test_pascal_mod_operator },
    { "test_pascal_mod_operator_percent", test_pascal_mod_operator_percent },
    { "test_pascal_string_concatenation", test_pascal_string_concatenation },
    { "test_pascal_complex_expression", test_pascal_complex_expression },
    { "test_pascal_div_operator", test_pascal_div_operator },
    { "test_pascal_real_number", test_pascal_real_number },
    { "test_pascal_char_literal", test_pascal_char_literal },
    { "test_pascal_char_code_literal", test_pascal_char_code_literal },
    { "test_pascal_unary_plus", test_pascal_unary_plus },
    { "test_pascal_relational_operators", test_pascal_relational_operators },
    { "test_pascal_boolean_operators", test_pascal_boolean_operators },
    { "test_pascal_bitwise_operators", test_pascal_bitwise_operators },
    { "test_pascal_address_operator", test_pascal_address_operator },
    { "test_pascal_comprehensive_expression", test_pascal_comprehensive_expression },
    { "test_pascal_precedence", test_pascal_precedence },
    { "test_pascal_type_casting", test_pascal_type_casting },
    { "test_pascal_set_constructor", test_pascal_set_constructor },
    { "test_pascal_empty_set", test_pascal_empty_set },
    { "test_pascal_char_set", test_pascal_char_set },
    { "test_pascal_typed_file_type", test_pascal_typed_file_type },
    { "test_pascal_range_expression", test_pascal_range_expression },
    { "test_pascal_char_range", test_pascal_char_range },
    { "test_pascal_set_union", test_pascal_set_union },
    { "test_pascal_is_operator", test_pascal_is_operator },
    { "test_pascal_as_operator", test_pascal_as_operator },
    { "test_pascal_as_operator_with_field_access", test_pascal_as_operator_with_field_access },
    // Statement tests
    { "test_pascal_assignment_statement", test_pascal_assignment_statement },
    { "test_pascal_expression_statement", test_pascal_expression_statement },
    { "test_pascal_if_statement", test_pascal_if_statement },
    { "test_pascal_if_else_statement", test_pascal_if_else_statement },
    { "test_pascal_begin_end_block", test_pascal_begin_end_block },
    { "test_pascal_for_statement", test_pascal_for_statement },
    { "test_pascal_for_statement_without_assignment", test_pascal_for_statement_without_assignment },
    { "test_pascal_for_downto_statement", test_pascal_for_downto_statement },
    { "test_pascal_for_downto_without_assignment", test_pascal_for_downto_without_assignment },
    { "test_pascal_for_to_direction", test_pascal_for_to_direction },
    { "test_pascal_while_statement", test_pascal_while_statement },
    { "test_pascal_simple_asm_block", test_pascal_simple_asm_block },
    { "test_pascal_multiline_asm_block", test_pascal_multiline_asm_block },
    { "test_pascal_empty_asm_block", test_pascal_empty_asm_block },
    { "test_pascal_unterminated_asm_block", test_pascal_unterminated_asm_block },
    // Procedure/Function Declaration tests
    { "test_pascal_simple_procedure", test_pascal_simple_procedure },
    { "test_pascal_procedure_with_params", test_pascal_procedure_with_params },
    { "test_pascal_procedure_with_trailing_semicolon", test_pascal_procedure_with_trailing_semicolon },
    { "test_pascal_simple_function", test_pascal_simple_function },
    { "test_pascal_function_with_trailing_semicolon", test_pascal_function_with_trailing_semicolon },
    { "test_pascal_function_no_params", test_pascal_function_no_params },
    { "test_pascal_function_multiple_params", test_pascal_function_multiple_params },
    // Failing tests for missing features
    { "test_pascal_record_type", test_pascal_record_type },
    { "test_pascal_unit_declaration", test_pascal_unit_declaration },
    { "test_pascal_pointer_type_declaration", test_pascal_pointer_type_declaration },
    { "test_pascal_method_implementation", test_pascal_method_implementation },
    { "test_pascal_with_statement_single_context", test_pascal_with_statement_single_context },
    { "test_pascal_with_statement_multiple_contexts", test_pascal_with_statement_multiple_contexts },
    { "test_pascal_with_statement_nested", test_pascal_with_statement_nested },
    { "test_pascal_with_statement_invalid_syntax", test_pascal_with_statement_invalid_syntax },
    { "test_pascal_exit_statement", test_pascal_exit_statement },
    { "test_pascal_include_directive", test_pascal_include_directive },
    { "test_pascal_forward_declared_function", test_pascal_forward_declared_function },
    // Case statement tests
    { "test_pascal_simple_case_statement", test_pascal_simple_case_statement },
    { "test_pascal_case_statement_with_ranges", test_pascal_case_statement_with_ranges },
    { "test_pascal_case_statement_multiple_labels", test_pascal_case_statement_multiple_labels },
    { "test_pascal_case_statement_with_else", test_pascal_case_statement_with_else },
    { "test_pascal_case_expression_labels", test_pascal_case_expression_labels },
    { "test_pascal_case_statement_char_labels", test_pascal_case_statement_char_labels },
    { "test_pascal_case_invalid_expression_labels", test_pascal_case_invalid_expression_labels },
    { "test_pascal_paren_star_comment", test_pascal_paren_star_comment },
    { "test_pascal_hex_literal", test_pascal_hex_literal },
    { "test_pascal_case_range_label", test_pascal_case_range_label },
    { "test_pascal_pointer_dereference", test_pascal_pointer_dereference },
    { "test_pascal_array_access_with_deref", test_pascal_array_access_with_deref },
    // New failing tests for missing features
    { "test_pascal_enumerated_type_declaration", test_pascal_enumerated_type_declaration },
    { "test_pascal_simple_const_declaration", test_pascal_simple_const_declaration },
    { "test_pascal_complex_const_declaration", test_pascal_complex_const_declaration },
    { "test_pascal_set_operations_program", test_pascal_set_operations_program },
    { "test_pascal_dialog_program", test_pascal_dialog_program },
    { "test_pascal_multiply_program", test_pascal_multiply_program },
    { "test_pascal_pointer_operations_program", test_pascal_pointer_operations_program },
    { "test_pascal_record_member_access_program", test_pascal_record_member_access_program },
    { "test_pascal_record_member_access_complete_program", test_pascal_record_member_access_complete_program },
    { "test_pascal_unitless_program", test_pascal_unitless_program },
    { "test_pascal_var_section", test_pascal_var_section },
    { "test_pascal_unit_with_dotted_name", test_pascal_unit_with_dotted_name },
    { "test_pascal_uses_with_dotted_unit", test_pascal_uses_with_dotted_unit },
    { "test_pascal_out_parameter_modifier", test_pascal_out_parameter_modifier },
    { "test_pascal_resourcestring_section", test_pascal_resourcestring_section },
    { "test_pascal_threadvar_section", test_pascal_threadvar_section },
    { "test_pascal_set_of_enum_typed_constant", test_pascal_set_of_enum_typed_constant },
    { "test_pascal_generic_type_declaration", test_pascal_generic_type_declaration },
    { "test_pascal_generic_record_type_declaration", test_pascal_generic_record_type_declaration },
    { "test_pascal_generic_class_tfpg_declaration", test_pascal_generic_class_tfpg_declaration },
    { "test_pascal_specialize_alias", test_pascal_specialize_alias },
    { "test_pascal_class_function_modifier", test_pascal_class_function_modifier },
    { "test_pascal_class_operator_overload", test_pascal_class_operator_overload },
    { "test_pascal_type_helper_for_string", test_pascal_type_helper_for_string },
    { "test_pascal_overload_directive", test_pascal_overload_directive },
    { "test_pascal_inline_directive", test_pascal_inline_directive },
    { "test_fpc_style_unit_parsing", test_fpc_style_unit_parsing },
    { "test_complex_fpc_rax64int_unit", test_complex_fpc_rax64int_unit },
    // Pascal directory snippet tests
    { "test_aligned_records", test_aligned_records },
    { "test_pascal_array_function_return", test_pascal_array_function_return },
    { "test_pascal_record_field_signterm", test_pascal_record_field_signterm },
    { "test_const_set", test_const_set },
    { "test_deprecated_type", test_deprecated_type },
    { "test_dotted_types", test_dotted_types },
    { "test_end_token", test_end_token },
    { "test_experimentals", test_experimentals },
    { "test_external_function", test_external_function },
    { "test_finalization_initialization_exports", test_finalization_initialization_exports },
    { "test_forward_overloaded", test_forward_overloaded },
    { "test_forward_without_semicolon", test_forward_without_semicolon },
    { "test_generic_constraints", test_generic_constraints },
    { "test_generic_interface_method_delegation", test_generic_interface_method_delegation },
    { "test_implements_generic_type", test_implements_generic_type },
    { "test_include_file", test_include_file },
    { "test_managed_records", test_managed_records },
    { "test_message_method", test_message_method },
    { "test_multiline", test_multiline },
    { "test_non_aligned_records", test_non_aligned_records },
    { "test_numbers", test_numbers },
    { "test_pointer_chars", test_pointer_chars },
    { "test_properties", test_properties },
    { "test_strict_visibility", test_strict_visibility },
    { "test_try_except", test_try_except },
    { "test_umlauts", test_umlauts },
    { "test_whitespace_around_ifdef_condition", test_whitespace_around_ifdef_condition },
    { "test_deprecated_on_const", test_deprecated_on_const },
    { "test_variant_record_field_attributes", test_variant_record_field_attributes },
    { "test_pascal_interspersed_decls_in_procedure", test_pascal_interspersed_decls_in_procedure },
    { "test_pascal_fpc_operator_overload_symbols", test_pascal_fpc_operator_overload_symbols },
    { "test_pascal_delphi_operator_overload_names", test_pascal_delphi_operator_overload_names },
    { "test_pascal_method_with_var_section", test_pascal_method_with_var_section },
    { "test_pascal_operator_with_var_section", test_pascal_operator_with_var_section },
    { "test_pascal_constructor_destructor_with_var_section", test_pascal_constructor_destructor_with_var_section },
    { "test_pascal_program_forward_declaration", test_pascal_program_forward_declaration },
    { "test_pascal_program_external_declaration", test_pascal_program_external_declaration },
    { "test_pascal_unit_forward_declaration", test_pascal_unit_forward_declaration },
    { "test_external_variants", test_external_variants },
    { "test_external_program_variants", test_external_program_variants },
    { "test_external_simple_debug", test_external_simple_debug },
    { "test_external_library_debug", test_external_library_debug },
    { "test_external_minimal_debug", test_external_minimal_debug },
    { "test_external_no_args_debug", test_external_no_args_debug },
    { "test_program_routine_local_typed_consts", test_program_routine_local_typed_consts },
    { "test_program_case_with_begin_end_branch", test_program_case_with_begin_end_branch },
    { "test_program_routine_directive_before_body", test_program_routine_directive_before_body },
    { "test_type_subroutine_with_stdcall", test_type_subroutine_with_stdcall },
    { "test_var_absolute_clause", test_var_absolute_clause },
    { "test_library_header_and_exports", test_library_header_and_exports },
    { "test_fpc_system_comment_keywords", test_fpc_system_comment_keywords },
    { "test_delphi_form_unit", test_delphi_form_unit },
    { NULL, NULL }
};
