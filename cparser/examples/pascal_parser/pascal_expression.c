#include "pascal_expression.h"
#include "pascal_parser.h"
#include "pascal_keywords.h"
#include "pascal_type.h"
#include "pascal_declaration.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#ifdef _WIN32
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

static ast_t* wrap_pointer_suffix(ast_t* parsed);
static ast_t* wrap_array_suffix(ast_t* parsed);
static ast_t* build_array_or_pointer_chain(ast_t* parsed);
static ast_t* wrap_nil_literal(ast_t* parsed);
static ast_t* wrap_true_literal(ast_t* parsed);
static ast_t* wrap_false_literal(ast_t* parsed);
static combinator_t* create_suffix_choice(combinator_t** expr_parser_ref);

// Helper to discard parse failures
static inline void discard_failure(ParseResult result) {
    if (!result.is_success) {
        free_error(result.value.error);
    }
}

// Pascal identifier parser that excludes reserved keywords
static ParseResult pascal_identifier_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    int start_pos = in->start;
    char c = read1(in);

    // Must start with letter or underscore
    if (c != '_' && !isalpha((unsigned char)c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected identifier"), NULL);
    }

    // Continue with alphanumeric or underscore
    while (isalnum((unsigned char)(c = read1(in))) || c == '_');
    if (c != EOF) in->start--;

    // Extract the identifier text
    int len = in->start - start_pos;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_pos, len);
    text[len] = '\0';

    // Check if it's a reserved keyword
    if (is_pascal_keyword(text)) {
        free(text);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Identifier cannot be a reserved keyword"), NULL);
    }

    // Create AST node for valid identifier (following original cident_fn pattern)
    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    free(text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    return make_success(ast);
}

// Create Pascal identifier combinator that excludes keywords
combinator_t* pascal_identifier(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_CIDENT; // Reuse the same type for compatibility
    comb->fn = pascal_identifier_fn;
    comb->args = args;
    return comb;
}

// Keywords that can be used as function names in expressions
static const char* expression_allowed_keywords[] = {
    "procedure", "function", "program", "unit",
    "record", "array", "set", "packed",  // type keywords that can be variable names
    "object", "class",                   // OOP keywords that can be variable names
    NULL
};

// Check if a keyword is allowed as an identifier in expressions
static bool is_expression_allowed_keyword(const char* str) {
    for (int i = 0; expression_allowed_keywords[i] != NULL; i++) {
        if (strcasecmp(str, expression_allowed_keywords[i]) == 0) {
            return true;
        }
    }
    return false;
}

// Pascal identifier parser for expressions - allows certain keywords as function names
static ParseResult pascal_expression_identifier_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    int start_pos = in->start;
    char c = read1(in);

    // Must start with letter or underscore
    if (c != '_' && !isalpha((unsigned char)c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected identifier"), NULL);
    }

    // Continue with alphanumeric or underscore
    while (isalnum((unsigned char)(c = read1(in))) || c == '_');
    if (c != EOF) in->start--;

    // Extract the identifier text
    int len = in->start - start_pos;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_pos, len);
    text[len] = '\0';

    // Check if it's a reserved keyword that's NOT allowed in expressions
    if (is_pascal_keyword(text) && !is_expression_allowed_keyword(text)) {
        free(text);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Identifier cannot be a reserved keyword"), NULL);
    }

    // Create AST node for valid identifier
    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    free(text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    return make_success(ast);
}

// Create Pascal expression identifier combinator that allows certain keywords
combinator_t* pascal_expression_identifier(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_CIDENT;
    comb->fn = pascal_expression_identifier_fn;
    comb->args = args;
    return comb;
}

static ParseResult real_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    int start_pos = in->start;

    // Parse integer part
    char c = read1(in);
    if (!isdigit(c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected digit"), NULL);
    }

    while (isdigit(c = read1(in)));
    if (c != EOF) in->start--; // Back up one if not EOF

    // Must have decimal point
    if (read1(in) != '.') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected decimal point"), NULL);
    }

    // Parse fractional part (at least one digit required)
    c = read1(in);
    if (!isdigit(c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected digit after decimal point"), NULL);
    }

    while (isdigit(c = read1(in)));
    if (c != EOF) in->start--; // Back up one if not EOF

    // Optional exponent part (e.g., E+10, e-5, E3)
    c = read1(in);
    if (c == 'e' || c == 'E') {
        // Parse optional sign
        c = read1(in);
        if (c == '+' || c == '-') {
            c = read1(in);
        }

        // Must have at least one digit after E/e
        if (!isdigit(c)) {
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected digit after exponent"), NULL);
        }

        // Parse remaining exponent digits
        while (isdigit(c = read1(in)));
        if (c != EOF) in->start--; // Back up one if not EOF
    } else if (c != EOF) {
        in->start--; // Back up if we didn't find exponent
    }

    // Create AST node with the real number value
    int len = in->start - start_pos;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_pos, len);
    text[len] = '\0';

    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    free(text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    return make_success(ast);
}

combinator_t* real_number(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY; // Reuse existing type for custom parser
    comb->fn = real_fn;
    comb->args = args;
    return comb;
}

// Custom parser for hexadecimal integers (e.g., $FF, $1A2B)
static ParseResult hex_integer_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    int start_pos = in->start;
    int c = read1(in);

    // Must start with $
    if (c != '$') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '$' for hex literal"), NULL);
    }

    // Must have at least one hex digit after $
    c = read1(in);
    if (c == EOF || !isxdigit(c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected hex digit after '$'"), NULL);
    }

    // Continue reading hex digits
    while ((c = read1(in)) != EOF && isxdigit(c));
    if (c != EOF) in->start--;

    // Extract the hex text (including the $)
    int len = in->start - start_pos;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_pos, len);
    text[len] = '\0';

    // Create AST node with the hex literal value
    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    free(text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    return make_success(ast);
}

combinator_t* hex_integer(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY; // Reuse existing type for custom parser
    comb->fn = hex_integer_fn;
    comb->args = args;
    return comb;
}

// Custom parser for character literals (e.g., 'A', 'x')
static ParseResult char_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Must start with single quote
    if (read1(in) != '\'') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected single quote"), NULL);
    }

    // Must have at least one character
    char char_value = read1(in);
    if (char_value == EOF) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Unterminated character literal"), NULL);
    }

    // Must end with single quote
    if (read1(in) != '\'') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected closing single quote"), NULL);
    }

    // A doubled quote means we're looking at the start of a Pascal string
    // literal rather than a standalone character literal (e.g. 'I''m ...').
    // In that situation we should fail here so that the string parser gets a
    // chance to consume the whole token instead of incorrectly succeeding on
    // the first character only.
    char lookahead = read1(in);
    if (lookahead == '\'') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected single character literal"), NULL);
    }
    if (lookahead != EOF) {
        in->start--;
    }

    // Create AST node with the character value
    char text[2];
    text[0] = char_value;
    text[1] = '\0';

    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    return make_success(ast);
}

combinator_t* char_literal(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY; // Reuse existing type for custom parser
    comb->fn = char_fn;
    comb->args = args;
    return comb;
}

// Custom parser for character code literals (e.g., #13, #$0D)
static ParseResult char_code_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    if (read1(in) != '#') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '#' for character code"), NULL);
    }

    int literal_start = state.start;
    
    // Buffer to accumulate multiple consecutive character codes
    char* result = NULL;
    int result_len = 0;
    int result_capacity = 0;
    
    // Parse first char code (we already consumed the '#')
    char c = read1(in);
    int value = 0;
    
    if (c == '$') {
        c = read1(in);
        if (c == EOF || !isxdigit((unsigned char)c)) {
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected hex digits after '#$'"), NULL);
        }
        value = (isdigit(c) ? c - '0' : tolower(c) - 'a' + 10);
        while ((c = read1(in)) != EOF && isxdigit((unsigned char)c)) {
            value = value * 16 + (isdigit(c) ? c - '0' : tolower(c) - 'a' + 10);
        }
    } else if (c != EOF && isdigit((unsigned char)c)) {
        value = c - '0';
        do {
            c = read1(in);
            if (c != EOF && isdigit((unsigned char)c)) {
                value = value * 10 + (c - '0');
            }
        } while (c != EOF && isdigit((unsigned char)c));
    } else {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected digits after '#'"), NULL);
    }
    
    if (c != EOF && in->start > 0) {
        in->start--;
    }
    
    // Store first character value
    if (result_capacity == 0) {
        result_capacity = 256;  // Start with reasonable capacity
        result = (char*)safe_malloc(result_capacity);
    }
    if (result_len >= result_capacity) {
        result_capacity *= 2;
        result = (char*)realloc(result, result_capacity);
    }
    result[result_len++] = (char)(value & 0xFF);
    
    // Try to consume additional consecutive character codes
    while (1) {
        InputState char_state;
        save_input_state(in, &char_state);
        
        // Skip whitespace (Pascal allows it between char codes)
        while ((c = read1(in)) != EOF && isspace((unsigned char)c));
        
        if (c != '#') {
            // Not another char code, restore and break
            restore_input_state(in, &char_state);
            break;
        }
        
        // Parse the next char code value
        c = read1(in);
        value = 0;
        
        if (c == '$') {
            c = read1(in);
            if (c == EOF || !isxdigit((unsigned char)c)) {
                // Invalid char code, restore and break
                restore_input_state(in, &char_state);
                break;
            }
            value = (isdigit(c) ? c - '0' : tolower(c) - 'a' + 10);
            while ((c = read1(in)) != EOF && isxdigit((unsigned char)c)) {
                value = value * 16 + (isdigit(c) ? c - '0' : tolower(c) - 'a' + 10);
            }
        } else if (c != EOF && isdigit((unsigned char)c)) {
            value = c - '0';
            do {
                c = read1(in);
                if (c != EOF && isdigit((unsigned char)c)) {
                    value = value * 10 + (c - '0');
                }
            } while (c != EOF && isdigit((unsigned char)c));
        } else {
            // Invalid char code, restore and break
            restore_input_state(in, &char_state);
            break;
        }
        
        if (c != EOF && in->start > 0) {
            in->start--;
        }
        
        // Add this character to result
        if (result_len >= result_capacity) {
            result_capacity *= 2;
            result = (char*)realloc(result, result_capacity);
        }
        result[result_len++] = (char)(value & 0xFF);
    }

    // Create AST node with the combined string
    // For a string of char codes, we store it as a STRING type rather than CHAR_CODE
    ast_t* ast = new_ast();
    if (result_len == 1) {
        // Single character - use original CHAR_CODE type
        ast->typ = pargs->tag;
        char* text = (char*)safe_malloc(in->start - literal_start + 1);
        memcpy(text, in->buffer + literal_start, in->start - literal_start);
        text[in->start - literal_start] = '\0';
        ast->sym = sym_lookup(text);
        free(text);
    } else {
        // Multiple characters - store as a STRING
        // Need to create a null-terminated copy
        char* str_copy = (char*)safe_malloc(result_len + 1);
        memcpy(str_copy, result, result_len);
        str_copy[result_len] = '\0';
        ast->typ = PASCAL_T_STRING;
        ast->sym = sym_lookup(str_copy);
        free(str_copy);
    }
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    free(result);
    return make_success(ast);
}

combinator_t* char_code_literal(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY;
    comb->fn = char_code_fn;
    comb->args = args;
    return comb;
}

// Custom parser for range expressions (e.g., 'a'..'z', 1..10)
static ParseResult range_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // This will be called as part of an infix expression parser
    // We just need to consume the ".." token
    if (read1(in) != '.' || read1(in) != '.') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '..'"), NULL);
    }

    // Create a placeholder AST node - the actual range will be built by the expression parser
    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup("..");
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    return make_success(ast);
}

combinator_t* range_operator(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY; // Reuse existing type for custom parser
    comb->fn = range_fn;
    comb->args = args;
    return comb;
}

// Simplified set constructor parser using existing parse utilities
static ParseResult set_fn(input_t* in, void* args, char* parser_name) {
    set_args* sargs = (set_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Must start with '['
    if (read1(in) != '[') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '['"), NULL);
    }

    ast_t* set_node = new_ast();
    set_node->typ = sargs->tag;
    set_node->sym = NULL;
    set_node->child = NULL;
    set_node->next = NULL;
    set_ast_position(set_node, in);

    // Skip whitespace manually
    char c;
    while (isspace((unsigned char)(c = read1(in))));
    if (c != EOF) in->start--;

    // Check for empty set
    c = read1(in);
    if (c == ']') {
        return make_success(set_node);
    }
    if (c != EOF) in->start--; // Back up

    // Parse set elements using the provided expression parser
    combinator_t* expr_parser = lazy(sargs->expr_parser);

    // Parse comma-separated expressions
    ast_t* first_element = NULL;
    ast_t* current_element = NULL;

    while (true) {
        // Skip whitespace
        while (isspace((unsigned char)(c = read1(in))));
        if (c != EOF) in->start--;

        ParseResult elem_result = parse(in, expr_parser);
        if (!elem_result.is_success) {
            free_ast(set_node);
            free_combinator(expr_parser);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected set element"), NULL);
        }

        // Add element to set
        if (!first_element) {
            first_element = elem_result.value.ast;
            current_element = elem_result.value.ast;
            set_node->child = elem_result.value.ast;
        } else {
            current_element->next = elem_result.value.ast;
            current_element = elem_result.value.ast;
        }

        // Skip whitespace
        while (isspace((unsigned char)(c = read1(in))));
        if (c != EOF) in->start--;

        // Check for comma or closing bracket
        c = read1(in);
        if (c == ']') {
            break;
        } else if (c == ',') {
            continue; // Parse next element
        } else {
            free_ast(set_node);
            free_combinator(expr_parser);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected ',' or ']'"), NULL);
        }
    }

    free_combinator(expr_parser);
    return make_success(set_node);
}

combinator_t* set_constructor(tag_t tag, combinator_t** expr_parser) {
    set_args* args = (set_args*)safe_malloc(sizeof(set_args));
    args->tag = tag;
    args->expr_parser = expr_parser;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY;
    comb->fn = set_fn;
    comb->args = args;
    return comb;
}

// Record constructor parser for (field: value; field: value; ...)
static ParseResult record_constructor_fn(input_t* in, void* args, char* parser_name) {
    set_args* rargs = (set_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Must start with '('
    if (read1(in) != '(') {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '('"), NULL);
    }

    ast_t* record_node = new_ast();
    record_node->typ = rargs->tag;
    record_node->sym = NULL;
    record_node->child = NULL;
    record_node->next = NULL;
    set_ast_position(record_node, in);

    // Skip whitespace manually
    char c;
    while (isspace((unsigned char)(c = read1(in))));
    if (c != EOF) in->start--;

    // Check for empty record constructor
    c = read1(in);
    if (c == ')') {
        return make_success(record_node);
    }
    if (c != EOF) in->start--; // Back up

    // Parse field assignments using the provided expression parser
    combinator_t* expr_parser = lazy(rargs->expr_parser);

    // Parse semicolon-separated field assignments
    ast_t* first_field = NULL;
    ast_t* current_field = NULL;

    while (true) {
        // Skip whitespace
        while (isspace((unsigned char)(c = read1(in))));
        if (c != EOF) in->start--;

        // Parse field name (identifier)
        combinator_t* field_name_parser = token(pascal_identifier(PASCAL_T_IDENTIFIER));
        ParseResult field_result = parse(in, field_name_parser);
        if (!field_result.is_success) {
            free_ast(record_node);
            free_combinator(expr_parser);
            free_combinator(field_name_parser);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected field name"), NULL);
        }

        // Skip whitespace
        while (isspace((unsigned char)(c = read1(in))));
        if (c != EOF) in->start--;

        // Expect colon after field name
        if (read1(in) != ':') {
            free_ast(record_node);
            free_combinator(expr_parser);
            free_combinator(field_name_parser);
            free_ast(field_result.value.ast);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected ':' after field name"), NULL);
        }

        // Skip whitespace
        while (isspace((unsigned char)(c = read1(in))));
        if (c != EOF) in->start--;

        // Parse field value expression
        ParseResult value_result = parse(in, expr_parser);
        if (!value_result.is_success) {
            free_ast(record_node);
            free_combinator(expr_parser);
            free_combinator(field_name_parser);
            free_ast(field_result.value.ast);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected field value expression"), NULL);
        }

        // Create field assignment node
        ast_t* field_assignment = new_ast();
        field_assignment->typ = PASCAL_T_ASSIGNMENT;
        field_assignment->sym = NULL;
        field_assignment->child = field_result.value.ast; // field name
        field_assignment->child->next = value_result.value.ast; // field value
        field_assignment->next = NULL;
        set_ast_position(field_assignment, in);

        // Add field assignment to record
        if (!first_field) {
            first_field = field_assignment;
            current_field = field_assignment;
            record_node->child = field_assignment;
        } else {
            current_field->next = field_assignment;
            current_field = field_assignment;
        }

        free_combinator(field_name_parser);

        // Skip whitespace
        while (isspace((unsigned char)(c = read1(in))));
        if (c != EOF) in->start--;

        // Check for semicolon or closing parenthesis
        c = read1(in);
        if (c == ')') {
            break;
        } else if (c == ';') {
            continue; // Parse next field
        } else {
            free_ast(record_node);
            free_combinator(expr_parser);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected ';' or ')'"), NULL);
        }
    }

    free_combinator(expr_parser);
    return make_success(record_node);
}

combinator_t* record_constructor(tag_t tag, combinator_t** expr_parser) {
    set_args* args = (set_args*)safe_malloc(sizeof(set_args));
    args->tag = tag;
    args->expr_parser = expr_parser;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY;
    comb->fn = record_constructor_fn;
    comb->args = args;
    return comb;
}

// Removed unused relational_ops() function that had non-boundary-aware match("in")

// Pascal single-quoted string content parser using combinators - handles '' escaping
static ParseResult pascal_single_quoted_content_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    int start_offset = in->start;

    // Build content by parsing until we hit the closing quote (not doubled)
    while(1) {
        InputState current_state;
        save_input_state(in, &current_state);

        char c = read1(in);
        if (c == EOF) {
            break; // End of content
        }

        if (c == '\'') {
            // Check if this is an escaped quote (doubled quote)
            char next_c = read1(in);
            if (next_c == '\'') {
                // This is an escaped single quote, continue parsing
                continue;
            } else {
                // This is the end of the string, put back the character and break
                if (next_c != EOF) in->start--;
                restore_input_state(in, &current_state);
                break;
            }
        }
    }

    int len = in->start - start_offset;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_offset, len);
    text[len] = '\0';

    // Process Pascal-style escape sequences (doubled quotes)
    int processed_len = 0;
    char* processed_text = (char*)safe_malloc(len + 1);

    for (int i = 0; i < len; i++) {
        if (text[i] == '\'' && i + 1 < len && text[i + 1] == '\'') {
            // This is an escaped single quote - add one quote and skip the next
            processed_text[processed_len++] = '\'';
            i++; // Skip the next quote
        } else {
            processed_text[processed_len++] = text[i];
        }
    }
    processed_text[processed_len] = '\0';

    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(processed_text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    free(text);
    free(processed_text);
    return make_success(ast);
}

combinator_t* pascal_single_quoted_content(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY; // Reuse existing type for custom parser
    comb->fn = pascal_single_quoted_content_fn;
    comb->args = args;
    return comb;
}

// Pascal double-quoted string content parser - handles \ escaping
static ParseResult pascal_double_quoted_content_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    int start_offset = in->start;

    while(1) {
        char c = read1(in);
        if (c == EOF) {
            break; // End of content
        }

        if (c == '"') {
            // End of string - put back the quote and break
            in->start--;
            break;
        }

        if (c == '\\') {
            // Skip the escaped character
            read1(in);
        }
    }

    int len = in->start - start_offset;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_offset, len);
    text[len] = '\0';

    // Process C-style escape sequences
    int processed_len = 0;
    char* processed_text = (char*)safe_malloc(len + 1);

    for (int i = 0; i < len; i++) {
        if (text[i] == '\\' && i + 1 < len) {
            // Process escape sequence
            char next = text[i + 1];
            switch (next) {
                case 'n': processed_text[processed_len++] = '\n'; break;
                case 't': processed_text[processed_len++] = '\t'; break;
                case '"': processed_text[processed_len++] = '"'; break;
                case '\\': processed_text[processed_len++] = '\\'; break;
                default:
                    // Unknown escape - keep both characters
                    processed_text[processed_len++] = text[i];
                    processed_text[processed_len++] = text[i + 1];
                    break;
            }
            i++; // Skip the next character
        } else {
            processed_text[processed_len++] = text[i];
        }
    }
    processed_text[processed_len] = '\0';

    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(processed_text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    free(text);
    free(processed_text);
    return make_success(ast);
}

// Create combinator for Pascal double-quoted string content
combinator_t* pascal_double_quoted_content(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY; // Reuse existing type for custom parser
    comb->fn = pascal_double_quoted_content_fn;
    comb->args = args;
    return comb;
}

// Pascal string parser using the between combinator for safety

combinator_t* pascal_string(tag_t tag) {
    combinator_t* single_quoted = between(
        match("'"),
        match("'"),
        pascal_single_quoted_content(tag)
    );

    combinator_t* double_quoted = between(
        match("\""),
        match("\""),
        pascal_double_quoted_content(tag)
    );

    return multi(new_combinator(), PASCAL_T_NONE,
        single_quoted,
        double_quoted,
        NULL
    );
}

// Parser for implicit string concatenation: 'hello'#13'world'
// In Pascal, adjacent string literals and character codes are implicitly concatenated
// Also handles single char literals vs strings properly
static ParseResult implicit_string_concat_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);
    
    // Parse first item: try char literal first, then string, then char code
    combinator_t* first_item = token(multi(new_combinator(), PASCAL_T_NONE,
        char_literal(PASCAL_T_CHAR),
        pascal_string(PASCAL_T_STRING),
        char_code_literal(PASCAL_T_CHAR_CODE),
        NULL
    ));
    
    ParseResult first = parse(in, first_item);
    free_combinator(first_item);
    
    if (!first.is_success) {
        discard_failure(first);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected string or char"), NULL);
    }
    
    ast_t* result = first.value.ast;
    int item_count = 1;
    
    // Try to parse additional strings/char codes without operators (with whitespace skipping)
    while (1) {
        InputState lookahead_state;
        save_input_state(in, &lookahead_state);
        
        combinator_t* next_item = token(multi(new_combinator(), PASCAL_T_NONE,
            pascal_string(PASCAL_T_STRING),
            char_code_literal(PASCAL_T_CHAR_CODE),
            NULL
        ));
        
        ParseResult next = parse(in, next_item);
        free_combinator(next_item);
        
        if (!next.is_success) {
            discard_failure(next);
            restore_input_state(in, &lookahead_state);
            break; // No more adjacent strings/char codes
        }
        
        item_count++;
        
        // Concatenate using ADD node
        ast_t* concat = new_ast();
        concat->typ = PASCAL_T_ADD;
        concat->child = result;
        result->next = next.value.ast;
        concat->next = NULL;
        set_ast_position(concat, in);
        result = concat;
    }
    
    // If we only got one item and it was a CHAR, return it as-is
    if (item_count == 1 && result->typ == PASCAL_T_CHAR) {
        return make_success(result);
    }
    
    // Wrap result in the requested tag
    if (result->typ != pargs->tag && pargs->tag != PASCAL_T_NONE) {
        ast_t* wrapper = new_ast();
        wrapper->typ = pargs->tag;
        wrapper->child = result;
        wrapper->next = NULL;
        set_ast_position(wrapper, in);
        result = wrapper;
    }
    
    return make_success(result);
}

combinator_t* implicit_string_concat(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_SATISFY;
    comb->fn = implicit_string_concat_fn;
    comb->args = args;
    return comb;
}


// Post-process AST to handle semantic operations like set union
static void post_process_set_operations(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) return;

    // Process children first (depth-first)
    post_process_set_operations(ast->child);
    post_process_set_operations(ast->next);

    // Check if this is an ADD operation with two SET operands
    if (ast->typ == PASCAL_T_ADD && ast->child && ast->child->next) {
        ast_t* left = ast->child;
        ast_t* right = ast->child->next;

        if (left->typ == PASCAL_T_SET && right->typ == PASCAL_T_SET) {
            // Convert ADD to SET_UNION
            ast->typ = PASCAL_T_SET_UNION;
        }
    }
}

// --- Parser Definition ---
void init_pascal_expression_parser(combinator_t** p, combinator_t** stmt_parser) {
    // Pascal identifier parser - use expression identifier that allows some keywords in expression contexts
    combinator_t* identifier = token(pascal_expression_identifier(PASCAL_T_IDENTIFIER));

    // Function name: use expression identifier parser that allows certain keywords as function names
    combinator_t* func_name = token(pascal_expression_identifier(PASCAL_T_IDENTIFIER));

    // Function call parser: function name followed by optional argument list
    combinator_t* arg_list = between(
        token(match("(")),
        token(match(")")),
        optional(sep_by(lazy(p), token(match(","))))
    );

    combinator_t* func_call = seq(new_combinator(), PASCAL_T_FUNC_CALL,
        func_name,                        // function name (built-in or custom)
        arg_list,
        NULL
    );

    // Pointer/array suffix parsing for identifiers like ptr^[i] or table[i]
    combinator_t* first_suffix = create_suffix_choice(p);
    combinator_t* more_suffixes = many(create_suffix_choice(p));
    combinator_t* suffixes = seq(new_combinator(), PASCAL_T_NONE,
        first_suffix,
        more_suffixes,
        NULL
    );

    combinator_t* array_access = map(seq(new_combinator(), PASCAL_T_NONE,
            func_name,
            suffixes,
            NULL),
        build_array_or_pointer_chain
    );

    // Type cast parser: TypeName(expression) - only for built-in types
    combinator_t* typecast = seq(new_combinator(), PASCAL_T_TYPECAST,
        token(type_name(PASCAL_T_IDENTIFIER)), // type name - only built-in types
        between(token(match("(")), token(match(")")), lazy(p)), // expression
        NULL
    );

    // Boolean literal parsers
    combinator_t* boolean_true = map(token(keyword_ci("true")), wrap_true_literal);
    combinator_t* boolean_false = map(token(keyword_ci("false")), wrap_false_literal);

    // Tuple constructor: (expr, expr, ...) - for nested array constants like ((1,2),(3,4))
    combinator_t* tuple = seq(new_combinator(), PASCAL_T_TUPLE,
        token(match("(")),
        sep_by(lazy(p), token(match(","))),  // comma-separated list of expressions
        token(match(")")),
        NULL
    );

    // Use standard factor parser - defer complex pointer dereference for now
    combinator_t* nil_literal = map(token(keyword_ci("nil")), wrap_nil_literal);

    // Constructed type parser for expressions (e.g., TFoo<Integer>.Create)
    // Use cident for type arguments to allow type names like Integer, String, etc.
    combinator_t* type_arg = token(cident(PASCAL_T_TYPE_ARG));
    combinator_t* type_arg_list = seq(new_combinator(), PASCAL_T_TYPE_ARG_LIST,
        token(match("<")),
        sep_by(type_arg, token(match(","))),
        token(match(">")),
        NULL
    );
    combinator_t* constructed_type = seq(new_combinator(), PASCAL_T_CONSTRUCTED_TYPE,
        token(pascal_expression_identifier(PASCAL_T_IDENTIFIER)),
        type_arg_list,
        NULL
    );

    combinator_t *factor = multi(new_combinator(), PASCAL_T_NONE,
        token(anonymous_function(PASCAL_T_ANONYMOUS_FUNCTION, p, stmt_parser)),  // Anonymous functions
        token(anonymous_procedure(PASCAL_T_ANONYMOUS_PROCEDURE, p, stmt_parser)), // Anonymous procedures
        token(real_number(PASCAL_T_REAL)),        // Real numbers (3.14) - try first
        token(hex_integer(PASCAL_T_INTEGER)),     // Hex integers ($FF) - try before decimal
        token(integer(PASCAL_T_INTEGER)),         // Integers (123)
        implicit_string_concat(PASCAL_T_NONE),    // Strings and char codes with implicit concatenation (handles char/string detection)
        token(set_constructor(PASCAL_T_SET, p)),  // Set constructors [1, 2, 3]
        token(boolean_true),                      // Boolean true
        token(boolean_false),                     // Boolean false
        nil_literal,                              // Nil literal
        typecast,                                 // Type casts Integer(x) - try before func_call
        array_access,                             // Array access (supports pointer dereference)
        func_call,                                // Function calls func(x)
        between(token(match("(")), token(match(")")), lazy(p)), // Parenthesized expressions - try before tuple
        tuple,                                    // Tuple constants (a,b,c) - try after parenthesized expressions
        token(record_constructor(PASCAL_T_RECORD_CONSTRUCTOR, p)), // Record constructors (field: value; field: value)
        constructed_type,                         // Constructed types like TFoo<Integer> - try before identifier
        identifier,                               // Identifiers (variables, built-ins)
        NULL
    );

    expr(*p, factor);

    // Precedence levels (lower number = lower precedence, must be consecutive starting from 0)
    // Precedence 0: Boolean OR (lowest precedence)
    expr_insert(*p, 0, PASCAL_T_OR, EXPR_INFIX, ASSOC_LEFT, token(match("or")));

    // Precedence 1: Boolean XOR
    expr_insert(*p, 1, PASCAL_T_XOR, EXPR_INFIX, ASSOC_LEFT, token(match("xor")));

    // Precedence 2: Boolean AND
    expr_insert(*p, 2, PASCAL_T_AND, EXPR_INFIX, ASSOC_LEFT, token(match("and")));

    // Precedence 3: All relational operators - multi-char operators added last (tried first)
    // Single character operators
    expr_insert(*p, 3, PASCAL_T_EQ, EXPR_INFIX, ASSOC_LEFT, token(match("=")));
    expr_altern(*p, 3, PASCAL_T_LT, token(match("<")));
    expr_altern(*p, 3, PASCAL_T_GT, token(match(">")));
    expr_altern(*p, 3, PASCAL_T_IN, token(keyword_ci("in")));
    expr_altern(*p, 3, PASCAL_T_IS, token(keyword_ci("is")));
    expr_altern(*p, 3, PASCAL_T_AS, token(keyword_ci("as")));
    // Multi-character operators (added last = tried first in expr parser)
    expr_altern(*p, 3, PASCAL_T_NE, token(match("<>")));
    expr_altern(*p, 3, PASCAL_T_GE, token(match(">=")));
    expr_altern(*p, 3, PASCAL_T_LE, token(match("<=")));

    // Precedence 4: Range operator (..)
    expr_insert(*p, 4, PASCAL_T_RANGE, EXPR_INFIX, ASSOC_LEFT, token(match("..")));

    // Precedence 5: Addition and Subtraction (includes string concatenation and set operations)
    expr_insert(*p, 5, PASCAL_T_ADD, EXPR_INFIX, ASSOC_LEFT, token(match("+")));
    expr_altern(*p, 5, PASCAL_T_SUB, token(match("-")));
    expr_altern(*p, 5, PASCAL_T_SET_SYM_DIFF, token(match("><")));

    // Precedence 6: Multiplication, Division, Modulo, and Bitwise shifts
    expr_insert(*p, 6, PASCAL_T_MUL, EXPR_INFIX, ASSOC_LEFT, token(match("*")));
    expr_altern(*p, 6, PASCAL_T_DIV, token(match("/")));
    expr_altern(*p, 6, PASCAL_T_INTDIV, token(keyword_ci("div")));
    expr_altern(*p, 6, PASCAL_T_MOD, token(keyword_ci("mod")));
    expr_altern(*p, 6, PASCAL_T_MOD, token(match("%")));
    expr_altern(*p, 6, PASCAL_T_SHL, token(keyword_ci("shl")));
    expr_altern(*p, 6, PASCAL_T_SHR, token(keyword_ci("shr")));
    expr_altern(*p, 6, PASCAL_T_ROL, token(keyword_ci("rol")));
    expr_altern(*p, 6, PASCAL_T_ROR, token(keyword_ci("ror")));

    // Precedence 7: Unary operators
    expr_insert(*p, 7, PASCAL_T_NEG, EXPR_PREFIX, ASSOC_NONE, token(match("-")));
    expr_insert(*p, 7, PASCAL_T_POS, EXPR_PREFIX, ASSOC_NONE, token(match("+")));
    expr_insert(*p, 7, PASCAL_T_NOT, EXPR_PREFIX, ASSOC_NONE, token(match("not")));
    expr_insert(*p, 7, PASCAL_T_ADDR, EXPR_PREFIX, ASSOC_NONE, token(match("@")));

    // Field width operator for formatted output: expression:width (same precedence as unary)
    expr_insert(*p, 7, PASCAL_T_FIELD_WIDTH, EXPR_INFIX, ASSOC_LEFT, token(match(":")));

    // Precedence 8: Member access (highest precedence)
    combinator_t* member_access_op = seq(new_combinator(), PASCAL_T_NONE,
        match("."),
        pnot(match(".")),  // not followed by another dot
        NULL
    );
    expr_insert(*p, 8, PASCAL_T_MEMBER_ACCESS, EXPR_INFIX, ASSOC_LEFT, token(member_access_op));
    
    // Precedence 9: Pointer dereference operator (postfix): expression^ (higher than member access)
    expr_insert(*p, 9, PASCAL_T_DEREF, EXPR_POSTFIX, ASSOC_LEFT, token(match("^")));
}

// --- Utility Functions ---
ParseResult parse_pascal_expression(input_t* input, combinator_t* parser) {
    ParseResult result = parse(input, parser);
    if (result.is_success) {
        post_process_set_operations(result.value.ast);
    }
    return result;
}

static combinator_t* create_suffix_choice(combinator_t** expr_parser_ref) {
    combinator_t* pointer_suffix = map(token(match("^")), wrap_pointer_suffix);

    combinator_t* index_list = between(
        token(match("[")),
        token(match("]")),
        sep_by(lazy(expr_parser_ref), token(match(",")))
    );
    combinator_t* array_suffix = map(index_list, wrap_array_suffix);

    combinator_t* choice = multi(new_combinator(), PASCAL_T_NONE,
        array_suffix,
        pointer_suffix,
        NULL
    );

    return choice;
}

static ast_t* wrap_pointer_suffix(ast_t* parsed) {
    if (parsed != NULL && parsed != ast_nil) {
        free_ast(parsed);
    }
    ast_t* node = new_ast();
    node->typ = PASCAL_T_DEREF;
    node->child = NULL;
    node->next = NULL;
    return node;
}

static ast_t* wrap_array_suffix(ast_t* parsed) {
    ast_t* node = new_ast();
    node->typ = PASCAL_T_ARRAY_ACCESS;
    node->child = (parsed == ast_nil) ? NULL : parsed;
    node->next = NULL;
    return node;
}

static ast_t* build_array_or_pointer_chain(ast_t* parsed) {
    if (parsed == NULL || parsed == ast_nil) {
        return parsed;
    }

    ast_t* base = parsed;
    ast_t* suffix = base->next;
    base->next = NULL;

    if (suffix == ast_nil) {
        suffix = NULL;
    }

    ast_t* current = base;
    while (suffix != NULL) {
        ast_t* next_suffix = suffix->next;
        if (next_suffix == ast_nil) {
            next_suffix = NULL;
        }
        suffix->next = NULL;

        switch (suffix->typ) {
            case PASCAL_T_DEREF: {
                suffix->child = current;
                current = suffix;
                break;
            }
            case PASCAL_T_ARRAY_ACCESS: {
                ast_t* indices = suffix->child;
                suffix->child = current;

                if (indices == ast_nil) {
                    indices = NULL;
                }

                if (indices != NULL) {
                    ast_t* tail = current;
                    while (tail->next != NULL) {
                        tail = tail->next;
                    }
                    tail->next = indices;
                }

                current = suffix;
                break;
            }
            default: {
                suffix->child = current;
                current = suffix;
                break;
            }
        }

        suffix = next_suffix;
    }

    return current;
}

static ast_t* wrap_nil_literal(ast_t* parsed) {
    if (parsed != NULL && parsed != ast_nil) {
        free_ast(parsed);
    }
    ast_t* node = new_ast();
    node->typ = PASCAL_T_NIL;
    node->child = NULL;
    node->next = NULL;
    node->sym = sym_lookup("nil");
    return node;
}

static ast_t* wrap_true_literal(ast_t* parsed) {
    if (parsed != NULL && parsed != ast_nil) {
        free_ast(parsed);
    }
    ast_t* node = new_ast();
    node->typ = PASCAL_T_BOOLEAN;
    node->child = NULL;
    node->next = NULL;
    node->sym = sym_lookup("true");
    return node;
}

static ast_t* wrap_false_literal(ast_t* parsed) {
    if (parsed != NULL && parsed != ast_nil) {
        free_ast(parsed);
    }
    ast_t* node = new_ast();
    node->typ = PASCAL_T_BOOLEAN;
    node->child = NULL;
    node->next = NULL;
    node->sym = sym_lookup("false");
    return node;
}

// Helper function to skip over anonymous function/procedure body
// Returns 0 on success, -1 on failure
static int skip_anonymous_body(input_t* in) {
    // Skip everything from begin to matching end
    int begin_count = 1;  // We already parsed one "begin"
    while (begin_count > 0 && in->start < in->length) {
        // Skip whitespace and comments
        while (in->start < in->length && isspace((unsigned char)in->buffer[in->start]))
            in->start++;
        
        // Check for begin/end keywords
        InputState check_state;
        save_input_state(in, &check_state);
        
        combinator_t* begin_check = token(keyword_ci("begin"));
        ParseResult begin_check_res = parse(in, begin_check);
        free_combinator(begin_check);
        
        if (begin_check_res.is_success) {
            free_ast(begin_check_res.value.ast);
            begin_count++;
            continue;
        } else {
            discard_failure(begin_check_res);
            restore_input_state(in, &check_state);
        }
        
        combinator_t* end_check = token(keyword_ci("end"));
        ParseResult end_check_res = parse(in, end_check);
        free_combinator(end_check);
        
        if (end_check_res.is_success) {
            free_ast(end_check_res.value.ast);
            begin_count--;
            if (begin_count == 0) {
                return 0;  // Success - found matching end
            }
            continue;
        } else {
            discard_failure(end_check_res);
            restore_input_state(in, &check_state);
        }
        
        // Skip one character if we didn't find begin or end
        if (in->start < in->length)
            in->start++;
    }
    
    return (begin_count == 0) ? 0 : -1;  // -1 means unmatched begin
}

// Anonymous function parser: function(params): ReturnType begin ... end
typedef struct {
    tag_t tag;
    combinator_t** expr_parser;
    combinator_t** stmt_parser;
} anon_func_args;

static ParseResult anonymous_function_fn(input_t* in, void* args, char* parser_name) {
    anon_func_args* afargs = (anon_func_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Parse "function" keyword
    combinator_t* function_keyword = token(keyword_ci("function"));
    ParseResult func_res = parse(in, function_keyword);
    free_combinator(function_keyword);
    
    if (!func_res.is_success) {
        discard_failure(func_res);
        return make_failure_v2(in, parser_name, strdup("Expected 'function'"), NULL);
    }
    free_ast(func_res.value.ast);

    // Parse optional parameter list
    combinator_t* params_parser = create_pascal_param_parser();
    ParseResult params_res = parse(in, params_parser);
    free_combinator(params_parser);
    
    ast_t* params_ast = NULL;
    if (params_res.is_success) {
        params_ast = params_res.value.ast;
    } else {
        discard_failure(params_res);
    }

    // Parse return type ": Type"
    combinator_t* colon_parser = token(match(":"));
    ParseResult colon_res = parse(in, colon_parser);
    free_combinator(colon_parser);
    
    if (!colon_res.is_success) {
        if (params_ast != NULL) free_ast(params_ast);
        discard_failure(colon_res);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected ':' for function return type"), NULL);
    }
    free_ast(colon_res.value.ast);

    // Parse return type
    combinator_t* type_spec = token(cident(PASCAL_T_IDENTIFIER));
    ParseResult type_res = parse(in, type_spec);
    free_combinator(type_spec);
    
    if (!type_res.is_success) {
        if (params_ast != NULL) free_ast(params_ast);
        discard_failure(type_res);
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected return type"), NULL);
    }
    
    ast_t* return_type_ast = new_ast();
    return_type_ast->typ = PASCAL_T_RETURN_TYPE;
    return_type_ast->child = type_res.value.ast;

    // Parse body: begin ... end  (parse as a compound statement using statement parser)
    ast_t* body_ast = NULL;
    
    if (afargs->stmt_parser != NULL && *afargs->stmt_parser != NULL) {
        // Use the statement parser to parse the body
        combinator_t* stmt_lazy = lazy(afargs->stmt_parser);
        ParseResult body_res = parse(in, stmt_lazy);
        free_combinator(stmt_lazy);
        
        if (!body_res.is_success) {
            if (params_ast != NULL) free_ast(params_ast);
            free_ast(return_type_ast);
            discard_failure(body_res);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Failed to parse anonymous function body"), NULL);
        }
        
        body_ast = body_res.value.ast;
    } else {
        // Fallback: skip the body if no statement parser available (for backward compatibility)
        combinator_t* begin_keyword = token(keyword_ci("begin"));
        ParseResult begin_res = parse(in, begin_keyword);
        free_combinator(begin_keyword);
        
        if (!begin_res.is_success) {
            if (params_ast != NULL) free_ast(params_ast);
            free_ast(return_type_ast);
            discard_failure(begin_res);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected 'begin' for function body"), NULL);
        }
        free_ast(begin_res.value.ast);

        // Skip the body parsing to avoid circular dependency issues
        if (skip_anonymous_body(in) != 0) {
            if (params_ast != NULL) free_ast(params_ast);
            free_ast(return_type_ast);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Unmatched 'begin' in anonymous function"), NULL);
        }
    }

    // Build AST: params -> return_type -> body
    ast_t* anon_func_ast = new_ast();
    anon_func_ast->typ = afargs->tag;
    anon_func_ast->child = params_ast;
    
    if (params_ast != NULL) {
        params_ast->next = return_type_ast;
    } else {
        anon_func_ast->child = return_type_ast;
    }
    return_type_ast->next = body_ast;
    
    set_ast_position(anon_func_ast, in);
    return make_success(anon_func_ast);
}

combinator_t* anonymous_function(tag_t tag, combinator_t** expr_parser, combinator_t** stmt_parser) {
    combinator_t* comb = new_combinator();
    anon_func_args* args = safe_malloc(sizeof(anon_func_args));
    args->tag = tag;
    args->expr_parser = expr_parser;
    args->stmt_parser = stmt_parser;
    comb->args = args;
    comb->fn = anonymous_function_fn;
    return comb;
}

// Anonymous procedure parser: procedure(params) begin ... end
static ParseResult anonymous_procedure_fn(input_t* in, void* args, char* parser_name) {
    anon_func_args* afargs = (anon_func_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Parse "procedure" keyword
    combinator_t* procedure_keyword = token(keyword_ci("procedure"));
    ParseResult proc_res = parse(in, procedure_keyword);
    free_combinator(procedure_keyword);
    
    if (!proc_res.is_success) {
        discard_failure(proc_res);
        return make_failure_v2(in, parser_name, strdup("Expected 'procedure'"), NULL);
    }
    free_ast(proc_res.value.ast);

    // Parse optional parameter list
    combinator_t* params_parser = create_pascal_param_parser();
    ParseResult params_res = parse(in, params_parser);
    free_combinator(params_parser);
    
    ast_t* params_ast = NULL;
    if (params_res.is_success) {
        params_ast = params_res.value.ast;
    } else {
        discard_failure(params_res);
    }

    // Parse body: begin ... end (parse as a compound statement using statement parser)
    ast_t* body_ast = NULL;
    
    if (afargs->stmt_parser != NULL && *afargs->stmt_parser != NULL) {
        // Use the statement parser to parse the body
        fprintf(stderr, "DEBUG: Anonymous procedure - using statement parser\n");
        combinator_t* stmt_lazy = lazy(afargs->stmt_parser);
        ParseResult body_res = parse(in, stmt_lazy);
        free_combinator(stmt_lazy);
        
        if (!body_res.is_success) {
            if (params_ast != NULL) free_ast(params_ast);
            discard_failure(body_res);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Failed to parse anonymous procedure body"), NULL);
        }
        
        body_ast = body_res.value.ast;
        if (body_ast != NULL) {
            fprintf(stderr, "DEBUG: Anonymous procedure - body parsed successfully, typ=%d\n", body_ast->typ);
        } else {
            fprintf(stderr, "DEBUG: Anonymous procedure - body_ast is NULL\n");
        }
    } else {
        fprintf(stderr, "DEBUG: Anonymous procedure - NO statement parser available, using fallback\n");
        // Fallback: skip the body if no statement parser available (for backward compatibility)
        combinator_t* begin_keyword = token(keyword_ci("begin"));
        ParseResult begin_res = parse(in, begin_keyword);
        free_combinator(begin_keyword);
        
        if (!begin_res.is_success) {
            if (params_ast != NULL) free_ast(params_ast);
            discard_failure(begin_res);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected 'begin' for procedure body"), NULL);
        }
        free_ast(begin_res.value.ast);

        // Skip the body parsing to avoid circular dependency issues
        if (skip_anonymous_body(in) != 0) {
            if (params_ast != NULL) free_ast(params_ast);
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Unmatched 'begin' in anonymous procedure"), NULL);
        }
    }

    // Build AST: params -> body
    ast_t* anon_proc_ast = new_ast();
    anon_proc_ast->typ = afargs->tag;
    anon_proc_ast->child = params_ast;
    
    if (params_ast != NULL) {
        params_ast->next = body_ast;
    } else {
        anon_proc_ast->child = body_ast;
    }
    
    set_ast_position(anon_proc_ast, in);
    return make_success(anon_proc_ast);
}

combinator_t* anonymous_procedure(tag_t tag, combinator_t** expr_parser, combinator_t** stmt_parser) {
    combinator_t* comb = new_combinator();
    anon_func_args* args = safe_malloc(sizeof(anon_func_args));
    args->tag = tag;
    args->expr_parser = expr_parser;
    args->stmt_parser = stmt_parser;
    comb->args = args;
    comb->fn = anonymous_procedure_fn;
    return comb;
}
