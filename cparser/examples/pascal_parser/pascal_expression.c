#include "pascal_expression.h"
#include "pascal_parser.h"
#include "pascal_keywords.h"
#include "pascal_type.h"
#include "combinators.h"
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
static combinator_t* create_suffix_choice(combinator_t** expr_parser_ref);

// Pascal identifier parser that excludes reserved keywords
static ParseResult pascal_identifier_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    int start_pos = in->start;
    char c = read1(in);

    // Must start with letter or underscore
    if (c != '_' && !isalpha(c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected identifier"), NULL);
    }

    // Continue with alphanumeric or underscore
    while (isalnum(c = read1(in)) || c == '_');
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
    if (c != '_' && !isalpha(c)) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected identifier"), NULL);
    }

    // Continue with alphanumeric or underscore
    while (isalnum(c = read1(in)) || c == '_');
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

    char c = read1(in);
    if (c == '$') {
        c = read1(in);
        if (c == EOF || !isxdigit((unsigned char)c)) {
            restore_input_state(in, &state);
            return make_failure_v2(in, parser_name, strdup("Expected hex digits after '#$'"), NULL);
        }
        while ((c = read1(in)) != EOF && isxdigit((unsigned char)c));
    } else if (c != EOF && isdigit((unsigned char)c)) {
        do {
            c = read1(in);
        } while (c != EOF && isdigit((unsigned char)c));
    } else {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected digits after '#'"), NULL);
    }

    if (c != EOF && in->start > 0) {
        in->start--;
    }

    int len = in->start - literal_start;
    char* text = (char*)safe_malloc(len + 1);
    memcpy(text, in->buffer + literal_start, len);
    text[len] = '\0';

    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);

    free(text);
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
    while (isspace(c = read1(in)));
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
        while (isspace(c = read1(in)));
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
        while (isspace(c = read1(in)));
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

// Helper to convert a list of char_code_literal AST nodes to a single string
static ast_t* string_from_char_codes(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) {
        return ast_nil;
    }

    size_t total_len = 0;
    for (ast_t* current = ast; current != NULL && current != ast_nil; current = current->next) {
        total_len++;
    }

    char* result_str = (char*)safe_malloc(total_len + 1);
    size_t current_pos = 0;

    for (ast_t* current = ast; current != NULL && current != ast_nil; current = current->next) {
        const char* code_str = current->sym->name;
        long val = 0;
        if (code_str[0] == '#') {
            if (code_str[1] == '$') {
                val = strtol(code_str + 2, NULL, 16);
            } else {
                val = strtol(code_str + 1, NULL, 10);
            }
        }
        result_str[current_pos++] = (char)(val & 0xFF);
    }
    result_str[current_pos] = '\0';

    ast_t* string_node = new_ast();
    string_node->typ = PASCAL_T_STRING;
    string_node->sym = sym_lookup(result_str);
    string_node->child = NULL;
    string_node->next = NULL;

    free(result_str);
    free_ast(ast); // Free the intermediate list of char codes
    return string_node;
}

// Helper to decide whether to convert char codes to a string or keep them as-is
static ast_t* smart_char_code_converter(ast_t* ast) {
    if (ast == NULL || ast == ast_nil) {
        return ast_nil;
    }

    // If there's only one char code in the sequence, return it as a CHAR_CODE node
    if (ast->next == NULL || ast->next == ast_nil) {
        return ast;
    }

    // If there are multiple char codes, convert them to a single STRING node
    return string_from_char_codes(ast);
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
void init_pascal_expression_parser(combinator_t** p) {
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
    combinator_t* boolean_true = seq(new_combinator(), PASCAL_T_BOOLEAN,
        match_ci("true"),
        NULL
    );
    combinator_t* boolean_false = seq(new_combinator(), PASCAL_T_BOOLEAN,
        match_ci("false"),
        NULL
    );

    // Tuple constructor: (expr, expr, ...) - for nested array constants like ((1,2),(3,4))
    combinator_t* tuple = seq(new_combinator(), PASCAL_T_TUPLE,
        token(match("(")),
        sep_by(lazy(p), token(match(","))),  // comma-separated list of expressions
        token(match(")")),
        NULL
    );

    // Use standard factor parser - defer complex pointer dereference for now
    combinator_t* nil_literal = map(token(keyword_ci("nil")), wrap_nil_literal);

    combinator_t* char_codes = map(many1(token(char_code_literal(PASCAL_T_CHAR_CODE))), smart_char_code_converter);

    combinator_t *factor = multi(new_combinator(), PASCAL_T_NONE,
        token(real_number(PASCAL_T_REAL)),        // Real numbers (3.14) - try first
        token(hex_integer(PASCAL_T_INTEGER)),     // Hex integers ($FF) - try before decimal
        token(integer(PASCAL_T_INTEGER)),         // Integers (123)
        token(char_literal(PASCAL_T_CHAR)),       // Characters ('A')
        char_codes,                               // Character code strings (#77#90)
        token(pascal_string(PASCAL_T_STRING)),    // Strings ("hello" or 'hello')
        token(set_constructor(PASCAL_T_SET, p)),  // Set constructors [1, 2, 3]
        token(boolean_true),                      // Boolean true
        token(boolean_false),                     // Boolean false
        nil_literal,                              // Nil literal
        typecast,                                 // Type casts Integer(x) - try before func_call
        array_access,                             // Array access (supports pointer dereference)
        func_call,                                // Function calls func(x)
        between(token(match("(")), token(match(")")), lazy(p)), // Parenthesized expressions - try before tuple
        tuple,                                    // Tuple constants (a,b,c) - try after parenthesized expressions
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
