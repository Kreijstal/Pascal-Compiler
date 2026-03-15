#define _GNU_SOURCE
#include "pascal_keywords.h"
#include "pascal_parser.h"
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#ifdef _WIN32
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

const char* pascal_reserved_keywords[] = {
    "begin", "end", "if", "then", "else", "while", "do", "for", "to", "downto",
    "repeat", "until", "case", "of", "var", "const", "type",
    "and", "or", "not", "xor", "div", "mod", "in", "nil", "true", "false",
    "array", "record", "set", "packed",
    // Built-in type keywords
    "integer", "real", "boolean", "char", "string", "byte", "word", "longint",
    // Exception handling keywords
    "try", "finally", "except", "raise", "on", "break",
    // Class and object-oriented keywords
    "class", "object", "private", "public", "protected", "published",
    "property", "inherited", "self", "constructor", "destructor",
    // Additional Pascal keywords
    "function", "procedure", "program", "unit", "uses", "interface", "implementation",
    "label", "goto",
    // Unit initialization/finalization keywords
    "initialization", "finalization",
    NULL
};

static const char* expression_identifier_keywords[] = {
    "program", "unit",
    "record", "array", "set", "packed",
    "object", "class",
    "integer", "real", "boolean", "char", "string",
    "byte", "word", "longint",
    "procedure",
    // Object-oriented helpers that must be usable as identifiers in expressions
    "self",
    // Boolean literals - needed for scoped enums like TUseBoolStrs.False
    "false", "true",
    NULL
};

bool is_pascal_keyword(const char* str) {
    for (int i = 0; pascal_reserved_keywords[i] != NULL; i++) {
        if (strcasecmp(str, pascal_reserved_keywords[i]) == 0) {
            return true;
        }
    }
    return false;
}

bool pascal_keyword_allowed_in_expression(const char* str) {
    if (str == NULL)
        return false;
    for (int i = 0; expression_identifier_keywords[i] != NULL; i++) {
        if (strcasecmp(str, expression_identifier_keywords[i]) == 0) {
            return true;
        }
    }
    return false;
}

// Word-boundary aware case-insensitive keyword matching
static ParseResult keyword_ci_fn(input_t* in, void* args, char* parser_name) {
    char* str = ((match_args*)args)->str;
    int len = (int)strlen(str);
    int pos = in->start;

    /* Fast path: check bounds, case-insensitive match, and word boundary without read1() */
    if (pos + len > in->length || strncasecmp(in->buffer + pos, str, len) != 0) {
        return make_failure(in, strdup("Expected keyword"));
    }

    // Check for word boundary: next character should not be alphanumeric or underscore
    if (pos + len < in->length) {
        char next_char = in->buffer[pos + len];
        if (isalnum((unsigned char)next_char) || next_char == '_') {
            return make_failure(in, strdup("Expected keyword, not part of identifier"));
        }
    }

    /* Advance position — keywords are always on one line, just advance col */
    in->start = pos + len;
    in->col += len;
    return make_success(ast_nil);
}

// Create word-boundary aware case-insensitive keyword parser
combinator_t* keyword_ci(const char* str) {
    match_args* args = (match_args*)safe_malloc(sizeof(match_args));
    args->str = (char*)str;  // Safe cast - str is never modified, only read
    combinator_t* comb = new_combinator();
    comb->type = P_CI_KEYWORD;
    comb->fn = keyword_ci_fn;
    comb->args = args;
    return comb;
}

// New argument struct for the custom parser function
typedef struct {
    const char* keyword;
    tag_t tag;
} keyword_parser_args;

// Custom parser function
static ParseResult match_keyword_fn(input_t* in, void* args, char* parser_name) {
    keyword_parser_args* k_args = (keyword_parser_args*)args;
    const char* keyword = k_args->keyword;
    int len = strlen(keyword);

    if (in->start + len > in->length || strncasecmp(in->buffer + in->start, keyword, len) != 0) {
        return make_failure(in, strdup("Expected keyword"));
    }

    if (in->start + len < in->length) {
        char next_char = in->buffer[in->start + len];
        if (isalnum((unsigned char)next_char) || next_char == '_') {
            return make_failure(in, strdup("Expected keyword, not part of identifier"));
        }
    }

    char* matched_text = (char*)safe_malloc(len + 1);
    memcpy(matched_text, in->buffer + in->start, len);
    matched_text[len] = '\0';

    /* Keywords are always on one line — just advance col */
    in->start += len;
    in->col += len;

    ast_t* ast = new_ast();
    ast->typ = k_args->tag;
    ast->sym = sym_lookup(matched_text);
    free(matched_text);
    set_ast_position(ast, in);
    return make_success(ast);
}

// Combinator constructor
combinator_t* create_keyword_parser(const char* keyword_str, tag_t tag) {
    combinator_t* comb = new_combinator();
    keyword_parser_args* args = (keyword_parser_args*)safe_malloc(sizeof(keyword_parser_args));
    args->keyword = keyword_str;
    args->tag = tag;

    comb->fn = match_keyword_fn;
    comb->args = args;
    // No specific type, it's a custom function
    return comb;
}
