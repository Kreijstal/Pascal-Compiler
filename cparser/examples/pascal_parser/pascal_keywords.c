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
    "array", "record", "set", "packed", "break",
    // Exception handling keywords
    "try", "finally", "except", "raise", "on",
    // Class and object-oriented keywords
    "class", "object", "private", "public", "protected", "published",
    "property", "inherited", "self", "constructor", "destructor",
    // Additional Pascal keywords
    "function", "procedure", "program", "unit", "uses", "interface", "implementation",
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

// Word-boundary aware case-insensitive keyword matching
static ParseResult keyword_ci_fn(input_t* in, void* args, char* parser_name) {
    char* str = ((match_args*)args)->str;
    InputState state;
    save_input_state(in, &state);

    int len = strlen(str);

    // Match the keyword case-insensitively
    for (int i = 0; i < len; i++) {
        char c = read1(in);
        if (tolower(c) != tolower(str[i])) {
            restore_input_state(in, &state);
            char* err_msg;
            asprintf(&err_msg, "Expected keyword '%s' (case-insensitive)", str);
            return make_failure_v2(in, parser_name, err_msg, NULL);
        }
    }

    // Check for word boundary: next character should not be alphanumeric or underscore
    if (in->start < in->length) {
        char next_char = in->buffer[in->start];
        if (isalnum((unsigned char)next_char) || next_char == '_') {
            restore_input_state(in, &state);
            char* err_msg;
            asprintf(&err_msg, "Expected keyword '%s', not part of identifier", str);
            return make_failure_v2(in, parser_name, err_msg, NULL);
        }
    }

    return make_success(ast_nil);
}

// Create word-boundary aware case-insensitive keyword parser
combinator_t* keyword_ci(char* str) {
    match_args* args = (match_args*)safe_malloc(sizeof(match_args));
    args->str = str;
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
        char* err_msg;
        asprintf(&err_msg, "Expected keyword '%s'", keyword);
        return make_failure_v2(in, parser_name, err_msg, NULL);
    }

    if (in->start + len < in->length) {
        char next_char = in->buffer[in->start + len];
        if (isalnum(next_char) || next_char == '_') {
            char* err_msg;
            asprintf(&err_msg, "Expected keyword '%s', not part of identifier", keyword);
            return make_failure_v2(in, parser_name, err_msg, NULL);
        }
    }

    char* matched_text = (char*)safe_malloc(len + 1);
    strncpy(matched_text, in->buffer + in->start, len);
    matched_text[len] = '\0';

    for (int i = 0; i < len; i++) {
        read1(in); // Advance input to update line/col info
    }

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
