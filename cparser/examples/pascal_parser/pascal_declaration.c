#include "pascal_declaration.h"
#include "pascal_parser.h"
#include "pascal_statement.h"
#include "pascal_expression.h"
#include "pascal_type.h"
#include "pascal_keywords.h"
#include "pascal_peek.h"
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <stdbool.h>

// Windows compatibility: strndup is not available on Windows
#ifdef _WIN32
static char* strndup(const char* s, size_t n)
{
    size_t len = strnlen(s, n);
    char* buf = (char*)malloc(len + 1);
    if (buf == NULL)
        return NULL;
    memcpy(buf, s, len);
    buf[len] = '\0';
    return buf;
}
#endif

extern ast_t* ast_nil;

// Helper to create simple keyword AST nodes for modifiers
static void set_combinator_name(combinator_t* comb, const char* name) {
    if (comb == NULL)
        return;

    if (comb->name != NULL) {
        free(comb->name);
    }
    comb->name = strdup(name);
}

// Balanced begin..end skipper for permissive function bodies (fallback)
static ParseResult skip_balanced_begin_end_fn(input_t* in, void* args, char* parser_name) {
    (void)args;
    InputState state; save_input_state(in, &state);

    // Expect 'begin' keyword
    combinator_t* open_kw = token(keyword_ci("begin"));
    ParseResult open_res = parse(in, open_kw);
    free_combinator(open_kw);
    if (!open_res.is_success) {
        if (!open_res.is_success && open_res.value.error) free_error(open_res.value.error);
        return make_failure_v2(in, parser_name, strdup("Expected 'begin'"), NULL);
    }

    const char* buf = in->buffer;
    int length = in->length > 0 ? in->length : (int)strlen(buf);
    int depth = 1;

    while (in->start < length) {
        // Skip whitespace
        while (in->start < length && isspace((unsigned char)buf[in->start])) in->start++;
        if (in->start >= length) break;

        // Skip comments
        if (buf[in->start] == '{') {
            in->start++;
            while (in->start < length && buf[in->start] != '}') in->start++;
            if (in->start < length) in->start++;
            continue;
        }
        if (buf[in->start] == '(' && (in->start + 1) < length && buf[in->start + 1] == '*') {
            in->start += 2;
            while ((in->start + 1) < length && !(buf[in->start] == '*' && buf[in->start + 1] == ')')) in->start++;
            if ((in->start + 1) < length) in->start += 2; else in->start = length;
            continue;
        }
        if (buf[in->start] == '/' && (in->start + 1) < length && buf[in->start + 1] == '/') {
            in->start += 2;
            while (in->start < length && buf[in->start] != '\n' && buf[in->start] != '\r') in->start++;
            continue;
        }

        // Skip strings (single and double quoted)
        if (buf[in->start] == '\'' || buf[in->start] == '"') {
            char q = buf[in->start++];
            while (in->start < length) {
                char c = buf[in->start++];
                if (q == '\'' && c == '\'' && in->start < length && buf[in->start] == '\'') { in->start++; continue; }
                if (c == q) break;
                if (c == '\\' && in->start < length) in->start++;
            }
            continue;
        }

        // Read a word to detect 'begin' or 'end'
        if (isalpha((unsigned char)buf[in->start]) || buf[in->start] == '_') {
            int p = in->start + 1;
            while (p < length && (isalnum((unsigned char)buf[p]) || buf[p] == '_')) p++;
            int wlen = p - in->start;
            if (wlen == 5 && strncasecmp(buf + in->start, "begin", 5) == 0) {
                in->start = p;
                depth++;
                continue;
            }
            if (wlen == 3 && strncasecmp(buf + in->start, "end", 3) == 0) {
                in->start = p;
                depth--;
                if (depth == 0) return make_success(ast_nil);
                continue;
            }
        }

        // Otherwise consume one char
        in->start++;
    }

    restore_input_state(in, &state);
    return make_failure_v2(in, parser_name, strdup("Unterminated begin..end block"), NULL);
}

static combinator_t* skip_balanced_begin_end(void) {
    combinator_t* comb = new_combinator();
    comb->fn = skip_balanced_begin_end_fn;
    set_combinator_name(comb, "skip_balanced_begin_end");
    return comb;
}

static ast_t* make_modifier_node(ast_t* original, const char* keyword) {
    ast_t* modifier = (original != NULL && original != ast_nil) ? original : new_ast();
    modifier->typ = PASCAL_T_IDENTIFIER;
    modifier->sym = sym_lookup(keyword);
    modifier->child = NULL;
    modifier->next = NULL;
    return modifier;
}

static ast_t* map_const_modifier(ast_t* ast) {
    return make_modifier_node(ast, "const");
}

static ast_t* map_var_modifier(ast_t* ast) {
    return make_modifier_node(ast, "var");
}

// Maps the 'out' keyword to a modifier node
static ast_t* map_out_modifier(ast_t* ast) {
    return make_modifier_node(ast, "out");
}

// Maps the 'constref' keyword (FPC/Delphi) to a modifier node
static ast_t* map_constref_modifier(ast_t* ast) {
    return make_modifier_node(ast, "constref");
}

// Maps directive keywords (forward, external, assembler) to AST nodes
// static ast_t* map_forward_directive(ast_t* ast) {
//     ast->typ = PASCAL_T_FORWARD_DIRECTIVE;
//     return ast;
// }

// static ast_t* map_external_directive(ast_t* ast) {
//     ast->typ = PASCAL_T_EXTERNAL_DIRECTIVE;
//     return ast;
// }

// static ast_t* map_assembler_directive(ast_t* ast) {
//     ast->typ = PASCAL_T_ASSEMBLER_DIRECTIVE;
//     return ast;
// }

// static ast_t* map_external_directive(ast_t* ast) {
//     return make_modifier_node(ast, "external");
// }

// static ast_t* map_assembler_directive(ast_t* ast) {
//     return make_modifier_node(ast, "assembler");
// }

static keyword_dispatch_args_t* create_keyword_dispatch(size_t capacity) {
    keyword_dispatch_args_t* args = (keyword_dispatch_args_t*)safe_malloc(sizeof(keyword_dispatch_args_t));
    memset(args, 0, sizeof(*args));
    if (capacity > 0) {
        args->entries = (pascal_keyword_entry_t*)safe_malloc(sizeof(pascal_keyword_entry_t) * capacity);
        memset(args->entries, 0, sizeof(pascal_keyword_entry_t) * capacity);
    } else {
        args->entries = NULL;
    }
    args->entry_count = 0;
    args->skip_keywords = NULL;
    args->skip_keyword_count = 0;
    args->fallback_parser = NULL;
    return args;
}

static void register_keyword_entry(keyword_dispatch_args_t* args, size_t capacity, size_t* index, const char* keyword, combinator_t* parser) {
    if (args == NULL || args->entries == NULL || index == NULL || parser == NULL || keyword == NULL) {
        return;
    }
    if (*index >= capacity) {
        return;
    }
    pascal_keyword_entry_t* entry = &args->entries[*index];
    entry->keyword = keyword;
    entry->length = strlen(keyword);
    entry->parser = parser;
    (*index)++;
}

static bool dispatch_word_should_skip(const keyword_dispatch_args_t* args, const pascal_word_slice_t* word) {
    if (args == NULL || args->skip_keywords == NULL || word == NULL || word->start == NULL) {
        return false;
    }
    for (size_t i = 0; i < args->skip_keyword_count; ++i) {
        const char* candidate = args->skip_keywords[i];
        if (candidate != NULL && pascal_word_equals_ci(word, candidate)) {
            return true;
        }
    }
    return false;
}

static bool keyword_dispatch_peek_word(const keyword_dispatch_args_t* args, const input_t* in, pascal_word_slice_t* out) {
    if (args == NULL || in == NULL || out == NULL) {
        return false;
    }
    pascal_word_slice_t slice;
    int pos = in->start;
    size_t guard = 0;
    while (true) {
        if (!pascal_peek_word_after(in, pos, &slice)) {
            return false;
        }
        if (!dispatch_word_should_skip(args, &slice)) {
            *out = slice;
            return true;
        }
        pos = skip_pascal_layout_preview(in, slice.end_pos);
        if (++guard > 4) {
            return false;
        }
    }
}

static ParseResult keyword_dispatch_fn(input_t* in, void* args, char* parser_name) {
    keyword_dispatch_args_t* dispatch = (keyword_dispatch_args_t*)args;
    if (dispatch == NULL) {
        return make_failure(in, strdup("keyword dispatcher misconfigured"));
    }
    pascal_word_slice_t word = {0};
    if (!keyword_dispatch_peek_word(dispatch, in, &word)) {
        if (dispatch->fallback_parser != NULL) {
            return parse(in, dispatch->fallback_parser);
        }
        return make_failure(in, strdup("keyword dispatcher did not find a match"));
    }
    for (size_t i = 0; i < dispatch->entry_count; ++i) {
        pascal_keyword_entry_t* entry = &dispatch->entries[i];
        if (entry->keyword == NULL || entry->parser == NULL) {
            continue;
        }
        if (pascal_word_equals_ci(&word, entry->keyword)) {
            return parse(in, entry->parser);
        }
    }
    if (dispatch->fallback_parser != NULL) {
        return parse(in, dispatch->fallback_parser);
    }
    char* unexpected = strndup(word.start, word.length);
    return make_failure_v2(in, parser_name, strdup("Unexpected keyword in dispatcher"), unexpected);
}

static int resolve_input_length_local(const input_t* in) {
    if (in == NULL) {
        return 0;
    }
    if (in->length > 0) {
        return in->length;
    }
    if (in->buffer == NULL) {
        return 0;
    }
    return (int)strlen(in->buffer);
}

// Parser that consumes a balanced parenthesis block starting at '('
// Handles nested parentheses, strings, and Pascal comments to avoid premature termination.

static bool looks_like_range_literal(const input_t* in, int pos) {
    if (in == NULL || in->buffer == NULL) {
        return false;
    }
    const char* buffer = in->buffer;
    int length = resolve_input_length_local(in);
    int limit = pos + 512;
    if (limit > length) {
        limit = length;
    }
    bool in_string = false;
    while (pos < limit) {
        unsigned char ch = (unsigned char)buffer[pos];
        if (in_string) {
            if (ch == '\'') {
                if (pos + 1 < length && buffer[pos + 1] == '\'') {
                    pos += 2;
                    continue;
                }
                in_string = false;
            }
            pos++;
            continue;
        }
        if (ch == '\'') {
            in_string = true;
            pos++;
            continue;
        }
        if (ch == '{') {
            pos++;
            while (pos < length && buffer[pos] != '}') {
                pos++;
            }
            if (pos < length) pos++;
            continue;
        }
        if (ch == '(' && pos + 1 < length && buffer[pos + 1] == '*') {
            pos += 2;
            while ((pos + 1) < length && !(buffer[pos] == '*' && buffer[pos + 1] == ')')) {
                pos++;
            }
            if ((pos + 1) < length) {
                pos += 2;
            } else {
                pos = length;
            }
            continue;
        }
        if (ch == '/' && pos + 1 < length && buffer[pos + 1] == '/') {
            pos += 2;
            while (pos < length && buffer[pos] != '\n' && buffer[pos] != '\r') {
                pos++;
            }
            continue;
        }
        if (ch == ';' || ch == ')' || ch == ',') {
            break;
        }
        if (ch == '.' && pos + 1 < length && buffer[pos + 1] == '.') {
            return true;
        }
        pos++;
    }
    return false;
}

static bool looks_like_constructed_type(const input_t* in, int pos) {
    if (in == NULL || in->buffer == NULL) {
        return false;
    }
    int length = resolve_input_length_local(in);
    const char* buffer = in->buffer;
    pascal_word_slice_t slice;
    if (!pascal_peek_word_after(in, pos, &slice)) {
        return false;
    }
    int cursor = slice.end_pos;
    while (cursor < length) {
        cursor = skip_pascal_layout_preview(in, cursor);
        if (cursor >= length) {
            break;
        }
        if (buffer[cursor] != '.') {
            break;
        }
        cursor++;
        if (!pascal_peek_word_after(in, cursor, &slice)) {
            return false;
        }
        cursor = slice.end_pos;
    }
    cursor = skip_pascal_layout_preview(in, cursor);
    return (cursor < length && buffer[cursor] == '<');
}

static ParseResult run_type_branch(input_t* in, combinator_t* branch) {
    if (branch == NULL) {
        return make_failure(in, strdup("type dispatcher missing branch parser"));
    }
    ParseResult res = parse(in, branch);
    if (res.is_success) {
        res.value.ast = ast1(PASCAL_T_TYPE_SPEC, res.value.ast);
    }
    return res;
}

static ParseResult type_definition_dispatch_fn(input_t* in, void* args, char* parser_name) {
    type_dispatch_args_t* dispatch = (type_dispatch_args_t*)args;
    if (dispatch == NULL) {
        return make_failure(in, strdup("type dispatcher misconfigured"));
    }
    if (in == NULL || in->buffer == NULL) {
        return make_failure(in, strdup("type dispatcher missing input"));
    }
    const char* buffer = in->buffer;
    int length = resolve_input_length_local(in);
    int pos = skip_pascal_layout_preview(in, in->start);
    if (pos >= length) {
        return make_failure_v2(in, parser_name, strdup("Unexpected end of input while parsing type"), NULL);
    }
    unsigned char ch = (unsigned char)buffer[pos];

    if (dispatch->pointer_parser && ch == '^') {
        return run_type_branch(in, dispatch->pointer_parser);
    }
    if (dispatch->enumerated_parser && ch == '(') {
        return run_type_branch(in, dispatch->enumerated_parser);
    }
    if (dispatch->range_parser && (isdigit(ch) || ch == '\'' || ch == '$' || ch == '%' || ch == '#' || ch == '&' || ch == '+' || ch == '-')) {
        return run_type_branch(in, dispatch->range_parser);
    }

    pascal_word_slice_t word;
    if (pascal_peek_word_after(in, pos, &word)) {
        if (dispatch->helper_parser && pascal_word_equals_ci(&word, "type")) {
            pascal_word_slice_t next;
            if (pascal_peek_word_after(in, word.end_pos, &next) && pascal_word_equals_ci(&next, "helper")) {
                return run_type_branch(in, dispatch->helper_parser);
            }
        }
        if (dispatch->reference_parser && pascal_word_equals_ci(&word, "reference")) {
            return run_type_branch(in, dispatch->reference_parser);
        }
        if (dispatch->interface_parser && pascal_word_equals_ci(&word, "interface")) {
            return run_type_branch(in, dispatch->interface_parser);
        }
        if (dispatch->class_parser && pascal_word_equals_ci(&word, "class")) {
            return run_type_branch(in, dispatch->class_parser);
        }
        if (dispatch->record_parser && pascal_word_equals_ci(&word, "record")) {
            return run_type_branch(in, dispatch->record_parser);
        }
        if (dispatch->array_parser && pascal_word_equals_ci(&word, "array")) {
            return run_type_branch(in, dispatch->array_parser);
        }
        if (dispatch->file_parser && pascal_word_equals_ci(&word, "file")) {
            return run_type_branch(in, dispatch->file_parser);
        }
        if (dispatch->set_parser && pascal_word_equals_ci(&word, "set")) {
            return run_type_branch(in, dispatch->set_parser);
        }
        if (dispatch->specialize_parser && pascal_word_equals_ci(&word, "specialize")) {
            return run_type_branch(in, dispatch->specialize_parser);
        }
        if (dispatch->record_parser && pascal_word_equals_ci(&word, "packed")) {
            pascal_word_slice_t next;
            if (pascal_peek_word_after(in, word.end_pos, &next)) {
                if (dispatch->record_parser && pascal_word_equals_ci(&next, "record")) {
                    return run_type_branch(in, dispatch->record_parser);
                }
                if (dispatch->array_parser && pascal_word_equals_ci(&next, "array")) {
                    return run_type_branch(in, dispatch->array_parser);
                }
                if (dispatch->set_parser && pascal_word_equals_ci(&next, "set")) {
                    return run_type_branch(in, dispatch->set_parser);
                }
            }
        }
    }

    if (dispatch->range_parser && looks_like_range_literal(in, pos)) {
        return run_type_branch(in, dispatch->range_parser);
    }

    if (dispatch->constructed_parser && looks_like_constructed_type(in, pos)) {
        return run_type_branch(in, dispatch->constructed_parser);
    }

    if (dispatch->identifier_parser) {
        return run_type_branch(in, dispatch->identifier_parser);
    }

    return make_failure_v2(in, parser_name, strdup("Unable to classify type definition"), NULL);
}

static ast_t* discard_ast(ast_t* ast) {
    if (ast != NULL && ast != ast_nil) {
        free_ast(ast);
    }
    return ast_nil;
}

// Helper to discard parse failures
static inline void discard_failure(ParseResult result) {
    if (!result.is_success) {
        free_error(result.value.error);
    }
}

// Custom parser function to handle identifier with optional [size] subscript
// This supports type aliases like: TAlfa = string[20];
static ParseResult identifier_with_optional_subscript_fn(input_t* in, void* args, char* parser_name) {
    InputState state;
    save_input_state(in, &state);
    
    // Parse the base identifier (e.g., "string")
    combinator_t* base_id = token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER));
    ParseResult base_res = parse(in, base_id);
    free_combinator(base_id);
    
    if (!base_res.is_success) {
        return base_res;
    }
    
    ast_t* identifier_ast = base_res.value.ast;
    
    // Check for optional [size] subscript (e.g., [20])
    InputState subscript_state;
    save_input_state(in, &subscript_state);
    
    combinator_t* open_bracket = token(match("["));
    ParseResult open_res = parse(in, open_bracket);
    free_combinator(open_bracket);
    
    if (open_res.is_success) {
        free_ast(open_res.value.ast);
        
        // Parse the size expression
        combinator_t* size_expr = new_combinator();
        init_pascal_expression_parser(&size_expr, NULL);
        ParseResult size_res = parse(in, size_expr);
        free_combinator(size_expr);
        
        if (!size_res.is_success) {
            discard_failure(size_res);
            free_ast(identifier_ast);
            restore_input_state(in, &subscript_state);
            return make_failure(in, strdup("Expected size expression in type subscript"));
        }
        
        // Parse closing ]
        combinator_t* close_bracket = token(match("]"));
        ParseResult close_res = parse(in, close_bracket);
        free_combinator(close_bracket);
        
        if (!close_res.is_success) {
            discard_failure(close_res);
            free_ast(size_res.value.ast);
            free_ast(identifier_ast);
            restore_input_state(in, &subscript_state);
            return make_failure(in, strdup("Expected ']' after type subscript"));
        }
        free_ast(close_res.value.ast);
        
        // For now, we discard the size and treat string[N] as string
        // This matches the behavior in pascal_type.c for array element types
        free_ast(size_res.value.ast);
        // identifier_ast remains as just the identifier
    } else {
        discard_failure(open_res);
        restore_input_state(in, &subscript_state);
    }
    
    ParseResult result;
    result.is_success = true;
    result.value.ast = identifier_ast;
    return result;
}

static bool is_modifier_keyword(ast_t* node) {
    if (node == NULL || node->sym == NULL || node->sym->name == NULL)
        return false;

    const char* name = node->sym->name;
    return strcasecmp(name, "var") == 0 ||
           strcasecmp(name, "const") == 0 ||
           strcasecmp(name, "out") == 0 ||
           strcasecmp(name, "constref") == 0;
}

static ast_t* create_placeholder_modifier(ast_t* reference) {
    ast_t* placeholder = new_ast();
    placeholder->typ = PASCAL_T_NONE;
    placeholder->child = NULL;
    placeholder->next = NULL;
    placeholder->sym = NULL;
    if (reference != NULL) {
        placeholder->line = reference->line;
        placeholder->col = reference->col;
    }
    return placeholder;
}

static combinator_t* create_label_identifier(void) {
    return multi(new_combinator(), PASCAL_T_NONE,
        token(integer(PASCAL_T_INTEGER)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
}

static combinator_t* create_label_section(void) {
    return seq(new_combinator(), PASCAL_T_LABEL_SECTION,
        token(keyword_ci("label")),
        sep_by(create_label_identifier(), token(match(","))),
        token(match(";")),
        NULL
    );
}

static ast_t* wrap_external_name_clause(ast_t* parsed) {
    if (parsed == NULL || parsed == ast_nil)
        return parsed;
    ast_t* node = new_ast();
    node->typ = PASCAL_T_EXTERNAL_NAME;
    node->child = parsed;
    node->next = NULL;
    node->sym = NULL;
    return node;
}

static combinator_t* make_generic_type_prefix(void) {
    return seq(new_combinator(), PASCAL_T_NONE,
        optional(token(keyword_ci("generic"))),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("<")),
        NULL
    );
}

static combinator_t* make_inferred_var_prefix(void) {
    return seq(new_combinator(), PASCAL_T_NONE,
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match(":=")),
        NULL
    );
}

static combinator_t* create_optional_modifier(void) {
    combinator_t* modifier_choice = multi(new_combinator(), PASCAL_T_NONE,
        map(token(keyword_ci("const")), map_const_modifier),
        map(token(keyword_ci("var")), map_var_modifier),
        map(token(keyword_ci("out")), map_out_modifier),
        map(token(keyword_ci("constref")), map_constref_modifier),
        NULL
    );

    return optional(modifier_choice);
}

static ast_t* detach_type_spec(ast_t* identifier_start, ast_t** out_type_spec) {
    ast_t* prev = NULL;
    ast_t* cursor = identifier_start;
    while (cursor != NULL) {
        if (cursor->typ == PASCAL_T_TYPE_SPEC) {
            if (prev != NULL) {
                prev->next = NULL;
            } else {
                identifier_start = NULL;
            }
            *out_type_spec = cursor;
            cursor->next = NULL;
            return identifier_start;
        }
        prev = cursor;
        cursor = cursor->next;
    }

    *out_type_spec = NULL;
    return identifier_start;
}

static ast_t* find_tail(ast_t* node) {
    ast_t* tail = node;
    while (tail != NULL && tail->next != NULL)
        tail = tail->next;
    return tail;
}

// This function takes a flat list of nodes from a 'seq' combinator
// and structures them into a single, hierarchical PASCAL_T_PARAM node.
static ast_t* structure_param_node(ast_t* ast) {
    if (ast == NULL || ast == ast_nil)
        return ast_nil;

    ast_t* modifier = NULL;
    ast_t* identifier_start = ast;
    ast_t* type_spec = NULL;

    if (is_modifier_keyword(ast)) {
        modifier = ast;
        identifier_start = ast->next;
        modifier->next = NULL;
    }

    identifier_start = detach_type_spec(identifier_start, &type_spec);

    if (modifier == NULL)
        modifier = create_placeholder_modifier(identifier_start != NULL ? identifier_start : type_spec);

    ast_t* param_node = new_ast();
    param_node->typ = PASCAL_T_PARAM;
    param_node->child = modifier;
    param_node->line = modifier ? modifier->line : 0;
    param_node->col = modifier ? modifier->col : 0;

    ast_t* tail = modifier;
    tail->next = identifier_start;

    if (identifier_start != NULL) {
        ast_t* identifiers_tail = find_tail(identifier_start);
        tail = identifiers_tail != NULL ? identifiers_tail : tail;
    }

    if (type_spec != NULL) {
        tail->next = type_spec;
        tail = type_spec;
    }

    if (tail != NULL)
        tail->next = NULL;

    return param_node;
}

static combinator_t* create_param_name_list(void) {
    return sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
}

static combinator_t* create_param_type_spec(void) {
    combinator_t* type_reference = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        reference_to_type(PASCAL_T_REFERENCE_TO_TYPE),
        array_type(PASCAL_T_ARRAY_TYPE),
        procedure_type(PASCAL_T_PROCEDURE_TYPE),
        function_type(PASCAL_T_FUNCTION_TYPE),
        set_type(PASCAL_T_SET),
        range_type(PASCAL_T_RANGE_TYPE),
        pointer_type(PASCAL_T_POINTER_TYPE),
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),
        record_type(PASCAL_T_RECORD_TYPE),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );

    return optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        type_reference,
        NULL
    ));
}

static combinator_t* create_simple_param_list(void) {
    combinator_t* param_name_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    combinator_t* param_seq = seq(new_combinator(), PASCAL_T_NONE,
        create_optional_modifier(),
        param_name_list,
        create_param_type_spec(),
        NULL
    );
    combinator_t* param = map(param_seq, structure_param_node);
    return optional(between(
        token(match("(")),
        token(match(")")),
        sep_by(param, token(match(";")))
    ));
}

// Parser for operator names - supports both FPC-style symbols (+, *, etc.) and Delphi-style names (Add, Multiply, etc.)
static ParseResult operator_name_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);

    // Try to parse operator symbol first
    const char* operator_symbols[] = {
        "+", "-", "*", "/", "=", "<>", "<", ">", "<=", ">=",
        ":=", "**", "div", "mod", "and", "or", "xor", "shl", "shr",
        "in", "is", "as", NULL
    };

    for (int i = 0; operator_symbols[i] != NULL; i++) {
        InputState symbol_state;
        save_input_state(in, &symbol_state);
        
        const char* symbol = operator_symbols[i];
        int len = strlen(symbol);
        bool matches = true;
        
        for (int j = 0; j < len; j++) {
            char c = read1(in);
            if (c == EOF || tolower((unsigned char)c) != tolower((unsigned char)symbol[j])) {
                matches = false;
                break;
            }
        }
        
        if (matches) {
            // Create AST node for operator symbol
            ast_t* ast = new_ast();
            ast->typ = pargs->tag;
            ast->sym = sym_lookup(symbol);
            ast->child = NULL;
            ast->next = NULL;
            set_ast_position(ast, in);
            return make_success(ast);
        }
        
        restore_input_state(in, &symbol_state);
    }

    // If no symbol matched, try parsing as identifier (Delphi-style: Add, Multiply, etc.)
    combinator_t* ident_parser = cident(pargs->tag);
    ParseResult ident_res = parse(in, ident_parser);
    free_combinator(ident_parser);
    
    if (ident_res.is_success) {
        return ident_res;
    }
    
    discard_failure(ident_res);
    restore_input_state(in, &state);
    return make_failure_v2(in, parser_name, strdup("Expected operator name or symbol"), NULL);
}

combinator_t* operator_name(tag_t tag) {
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->fn = operator_name_fn;
    comb->args = args;
    return comb;
}

// Helper function to create parameter parser (reduces code duplication)
combinator_t* create_pascal_param_parser(void) {
    combinator_t* param_seq = seq(new_combinator(), PASCAL_T_NONE,
        create_optional_modifier(),
        create_param_name_list(),
        create_param_type_spec(),
        NULL
    );
    combinator_t* param = map(param_seq, structure_param_node);
    set_combinator_name(param, "param");

    return optional(between(
        token(match("(")), token(match(")")), sep_by(param, token(match(";")))));
}

// Helper function to create generic type lookahead combinator
// Checks for '< identifier' pattern to distinguish generic types from '<>' operator
combinator_t* create_generic_type_lookahead(void) {
    combinator_t* type_arg = token(cident(PASCAL_T_TYPE_ARG));
    return seq(new_combinator(), PASCAL_T_NONE,
        token(match("<")),
        type_arg,
        NULL
    );
}

static ast_t* wrap_program_params(ast_t* params) {
    ast_t* params_node = new_ast();
    params_node->typ = PASCAL_T_PROGRAM_PARAMS;
    params_node->child = (params == ast_nil) ? NULL : params;
    return params_node;
}

// Custom parser for main block content that parses statements properly
static ParseResult main_block_content_fn(input_t* in, void* args, char* parser_name) {
    main_block_args_t* mb_args = (main_block_args_t*)args;
    const char* debug_flag = getenv("GPC_DEBUG_MAIN_BLOCK");
    if (debug_flag != NULL) {
        fprintf(stderr, "[pascal_parser] main block parse start at %d\n", in ? in->start : -1);
    }
    if (mb_args == NULL || mb_args->stmt_parser == NULL || *mb_args->stmt_parser == NULL) {
        return make_failure(in, strdup("main block statement parser unavailable"));
    }
    combinator_t** stmt_parser_ref = mb_args->stmt_parser;
    // Statements in a BEGIN..END block follow the same semicolon rules as any
    // compound statement: statements are separated by semicolons with an optional
    // trailing semicolon.  Use sep_by/optional to mirror the begin-end handling in
    // the statement parser so complex statements (like CASE) remain available.
    combinator_t* leading_semicolons = many(token(match(";")));

    combinator_t* stmt_sequence = seq(new_combinator(), PASCAL_T_NONE,
        leading_semicolons,
        sep_by(lazy(stmt_parser_ref), token(match(";"))),
        optional(token(match(";"))),
        NULL
    );

    if (debug_flag != NULL) {
        fprintf(stderr, "[pascal_parser] about to parse main block statements\n");
    }

    ParseResult stmt_result = parse(in, stmt_sequence);

    free_combinator(stmt_sequence);

    if (debug_flag != NULL) {
        if (stmt_result.is_success) {
            fprintf(stderr, "[pascal_parser] main block parse ok, consumed up to %d\n", in ? in->start : -1);
            if (stmt_result.value.ast) {
                fprintf(stderr, "[pascal_parser]   result ast typ=%d\n", stmt_result.value.ast->typ);
                if (stmt_result.value.ast == ast_nil) {
                    fprintf(stderr, "[pascal_parser]   result ast is ast_nil!\n");
                }
                if (stmt_result.value.ast->child) {
                    fprintf(stderr, "[pascal_parser]   result ast has child typ=%d\n", 
                            stmt_result.value.ast->child->typ);
                    // Check if the child is the sep_by result
                    if (stmt_result.value.ast->child->next) {
                        ast_t* sep_by_result = stmt_result.value.ast->child->next;
                        fprintf(stderr, "[pascal_parser]   sep_by result (second child) typ=%d\n", sep_by_result->typ);
                        if (sep_by_result == ast_nil) {
                            fprintf(stderr, "[pascal_parser]   sep_by result is ast_nil!\n");
                        } else if (sep_by_result->child) {
                            fprintf(stderr, "[pascal_parser]   sep_by has children, first child typ=%d\n", 
                                    sep_by_result->child->typ);
                        } else {
                            fprintf(stderr, "[pascal_parser]   sep_by result has NO children\n");
                        }
                    }
                } else {
                    fprintf(stderr, "[pascal_parser]   result ast child is NULL\n");
                }
            } else {
                fprintf(stderr, "[pascal_parser]   result ast is NULL\n");
            }
        } else {
            fprintf(stderr, "[pascal_parser] main block parse FAILED at %d (%s)\n",
                in ? in->start : -1,
                (stmt_result.value.error && stmt_result.value.error->message) ? stmt_result.value.error->message : "unknown");
        }
    }
    
    // CRITICAL FIX: Extract the statement list from the seq result
    // The seq has: leading_semicolons, sep_by(statements), optional(semicolon)
    // We want to return just the sep_by result (second child)
    if (stmt_result.is_success && stmt_result.value.ast != ast_nil && stmt_result.value.ast != NULL) {
        if (stmt_result.value.ast->typ == PASCAL_T_NONE && stmt_result.value.ast->child != NULL) {
            // Extract the sep_by result (second child)
            ast_t* sep_by_result = stmt_result.value.ast->child->next;
            if (sep_by_result != NULL && sep_by_result != ast_nil) {
                if (debug_flag != NULL) {
                    fprintf(stderr, "[pascal_parser] Extracting sep_by result from seq\n");
                }
                stmt_result.value.ast = sep_by_result;
            }
        }
    }
    
    return stmt_result;
}

static combinator_t* main_block_content(combinator_t** stmt_parser_ref) {
    combinator_t* comb = new_combinator();
    main_block_args_t* args = safe_malloc(sizeof(main_block_args_t));
    args->stmt_parser = stmt_parser_ref;
    comb->args = args;
    comb->type = COMB_MAIN_BLOCK_CONTENT;
    comb->fn = main_block_content_fn;
    return comb;
}

// Helper function to wrap the content of a begin-end block in a PASCAL_T_MAIN_BLOCK node
static ast_t* build_main_block_ast(ast_t* ast) {
    const char* debug_flag = getenv("GPC_DEBUG_MAIN_BLOCK");
    if (debug_flag != NULL) {
        fprintf(stderr, "[build_main_block_ast] input ast=%p\n", (void*)ast);
        if (ast && ast != ast_nil) {
            fprintf(stderr, "[build_main_block_ast]   ast->typ=%d\n", ast->typ);
            fprintf(stderr, "[build_main_block_ast]   ast->child=%p\n", (void*)ast->child);
            if (ast->child && ast->child != ast_nil) {
                fprintf(stderr, "[build_main_block_ast]     child->typ=%d\n", ast->child->typ);
                fprintf(stderr, "[build_main_block_ast]     child->next=%p\n", (void*)ast->child->next);
                if (ast->child->next && ast->child->next != ast_nil) {
                    fprintf(stderr, "[build_main_block_ast]       next->typ=%d\n", ast->child->next->typ);
                }
            }
        } else if (ast == ast_nil) {
            fprintf(stderr, "[build_main_block_ast]   ast is ast_nil\n");
        }
    }
    
    ast_t* block_node = new_ast();
    block_node->typ = PASCAL_T_MAIN_BLOCK;
    
    // CRITICAL FIX: The issue is that when sep_by returns an empty list, the entire
    // seq result becomes ast_nil. But we actually want to preserve the statement list
    // even if it's empty. The problem is that main_block_content_fn wraps everything
    // in a seq(PASCAL_T_NONE, ...) and when all children are ast_nil, seq returns ast_nil.
    //
    // The real fix is: don't use seq with PASCAL_T_NONE for the main block content.
    // Instead, just return the sep_by result directly. But since we can't change that
    // without potentially breaking other things, we need to handle it here.
    //
    // For now, if we get ast_nil, we'll just set child to NULL (empty block).
    // The real statements should be coming through in the non-ast_nil case.
    
    if (ast == ast_nil || ast == NULL) {
        block_node->child = NULL;
        if (debug_flag != NULL) {
            fprintf(stderr, "[build_main_block_ast] result: child=NULL (ast was nil/NULL)\n");
        }
        return block_node;
    }
    
    // If we have a real AST node, use it directly as the child
    // This will be the statement list from sep_by
    block_node->child = ast;
    if (debug_flag != NULL) {
        fprintf(stderr, "[build_main_block_ast] result: child=%p (used ast directly)\n", (void*)ast);
    }
    
    return block_node;
}

// Fallback: skip forward to the last '.' in the file and consume it.
// Used to tolerate trailing constructs that confuse strict parsing.
static ParseResult skip_to_final_period_fn(input_t* in, void* args, char* parser_name) {
    (void)args; (void)parser_name;
    if (in == NULL || in->buffer == NULL) {
        return make_failure_v2(in, "skip_to_final_period", strdup("No input"), NULL);
    }
    int len = (in->length > 0) ? in->length : (int)strlen(in->buffer);
    int last = -1;
    for (int i = len - 1; i >= in->start; --i) {
        if (in->buffer[i] == '.') { last = i; break; }
    }
    if (last < 0) {
        return make_failure_v2(in, "skip_to_final_period", strdup("No '.' found"), NULL);
    }
    // Move to just after the '.' and succeed
    in->start = last + 1;
    return make_success(ast_nil);
}


// Pascal Program/Terminated Statement Parser - for standalone statements with semicolons
void init_pascal_program_parser(combinator_t** p) {
    // Create the base statement parser
    combinator_t** base_stmt = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *base_stmt = new_combinator();
    init_pascal_statement_parser(base_stmt);

    // Terminated statement: statement followed by semicolon
    seq(*p, PASCAL_T_NONE,
        lazy(base_stmt),                       // any statement
        token(match(";")),                     // followed by semicolon
        NULL
    );

    (*p)->extra_to_free = base_stmt;
}

// Helper function to create statement list parser
static combinator_t* make_stmt_list_parser(combinator_t** stmt_parser) {
    return seq(new_combinator(), PASCAL_T_NONE,
        sep_by(lazy(stmt_parser), token(match(";"))),
        optional(token(match(";"))),
        NULL
    );
}

// Pascal Unit Parser
void init_pascal_unit_parser(combinator_t** p) {
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Uses section: uses unit1, unit2, unit3;
    combinator_t* uses_unit = token(pascal_qualified_identifier(PASCAL_T_USES_UNIT));
    combinator_t* uses_section = seq(new_combinator(), PASCAL_T_USES_SECTION,
        token(keyword_ci("uses")),                      // uses keyword (with word boundary check)
        sep_by(uses_unit, token(match(","))),        // unit names separated by commas
        token(match(";")),                           // semicolon
        NULL
    );

    // Type section: type name = TypeDefinition; ...
    combinator_t* specialize_args = seq(new_combinator(), PASCAL_T_NONE,
        token(match("<")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(">")),
        NULL
    );

    combinator_t* specialize_type = seq(new_combinator(), PASCAL_T_TYPE_SPEC,
        token(keyword_ci("specialize")),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        optional(specialize_args),
        NULL
    );

    combinator_t* helper_param_list = create_pascal_param_parser();

    combinator_t* helper_procedure_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        helper_param_list,
        token(match(";")),
        NULL
    );

    combinator_t* helper_function_decl = seq(new_combinator(), PASCAL_T_METHOD_DECL,
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        helper_param_list,
        token(match(":")),
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match(";")),
        NULL
    );

    combinator_t* helper_member = many(multi(new_combinator(), PASCAL_T_NONE,
        helper_procedure_decl,
        helper_function_decl,
        NULL
    ));

    combinator_t* type_helper_type = seq(new_combinator(), PASCAL_T_CLASS_TYPE,
        token(keyword_ci("type")),
        token(keyword_ci("helper")),
        token(keyword_ci("for")),
        token(cident(PASCAL_T_IDENTIFIER)),
        helper_member,
        token(keyword_ci("end")),
        NULL
    );

    // Constructed type parser for generic types like TFoo<Integer>
    // Peek to ensure this looks like a generic type before committing
    // This prevents parsing '<>' (not-equal operator) as an empty generic type
    combinator_t* type_arg = token(cident(PASCAL_T_TYPE_ARG));
    combinator_t* type_arg_list = seq(new_combinator(), PASCAL_T_TYPE_ARG_LIST,
        token(match("<")),
        sep_by1(type_arg, token(match(","))),  // Require at least one type argument
        token(match(">")),
        NULL
    );
    combinator_t* constructed_type = seq(new_combinator(), PASCAL_T_CONSTRUCTED_TYPE,
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        peek(create_generic_type_lookahead()),  // Lookahead to ensure '< identifier' pattern
        type_arg_list,                          // Now parse the full type argument list
        NULL
    );

    combinator_t* reference_type = reference_to_type(PASCAL_T_REFERENCE_TO_TYPE);
    combinator_t* iface_type = interface_type(PASCAL_T_INTERFACE_TYPE);
    combinator_t* class_spec = class_type(PASCAL_T_CLASS_TYPE);
    combinator_t* record_spec = record_type(PASCAL_T_RECORD_TYPE);
    combinator_t* enum_spec = enumerated_type(PASCAL_T_ENUMERATED_TYPE);
    combinator_t* array_spec = array_type(PASCAL_T_ARRAY_TYPE);
    combinator_t* file_spec = file_type(PASCAL_T_FILE_TYPE);
    combinator_t* set_spec = set_type(PASCAL_T_SET);
    combinator_t* range_spec = range_type(PASCAL_T_RANGE_TYPE);
    combinator_t* pointer_spec = pointer_type(PASCAL_T_POINTER_TYPE);
    
    // Create parser for identifier with optional subscript (e.g., string[20])
    combinator_t* simple_identifier = new_combinator();
    simple_identifier->type = COMB_TYPE_DISPATCH;
    simple_identifier->fn = identifier_with_optional_subscript_fn;
    simple_identifier->args = NULL;
    set_combinator_name(simple_identifier, "identifier_with_optional_subscript");

    type_dispatch_args_t* type_args = (type_dispatch_args_t*)safe_malloc(sizeof(type_dispatch_args_t));
    memset(type_args, 0, sizeof(*type_args));
    type_args->helper_parser = type_helper_type;
    type_args->reference_parser = reference_type;
    type_args->interface_parser = iface_type;
    type_args->class_parser = class_spec;
    type_args->record_parser = record_spec;
    type_args->enumerated_parser = enum_spec;
    type_args->array_parser = array_spec;
    type_args->file_parser = file_spec;
    type_args->set_parser = set_spec;
    type_args->range_parser = range_spec;
    type_args->pointer_parser = pointer_spec;
    type_args->specialize_parser = specialize_type;
    type_args->constructed_parser = constructed_type;
    type_args->identifier_parser = simple_identifier;

    combinator_t* type_definition = new_combinator();
    type_definition->type = COMB_TYPE_DISPATCH;
    type_definition->fn = type_definition_dispatch_fn;
    type_definition->args = type_args;

    // Const section: const name : type = value; ...
    // Use the full expression parser so typed constants support the same
    // constructs as regular expressions (arrays, arithmetic, sets, etc.).
    combinator_t** const_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *const_expr_parser = new_combinator();
    init_pascal_expression_parser(const_expr_parser, NULL);

    combinator_t* const_value = lazy(const_expr_parser);

    combinator_t* const_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // constant name
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),                       // optional type specification
            type_definition,                         // full type definition (not just simple identifier)
            NULL
        )),
        token(match("=")),                           // equals sign
        const_value,                                 // constant value (simplified for now)
        token(match(";")),                           // semicolon
        NULL
    );

    combinator_t* const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),                     // const keyword (with word boundary check)
        many(const_decl),                            // multiple const declarations
        NULL
    );
    const_section->extra_to_free = const_expr_parser;

    combinator_t* resourcestring_value = multi(new_combinator(), PASCAL_T_NONE,
        token(pascal_string(PASCAL_T_STRING)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );

    combinator_t* resourcestring_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("=")),
        resourcestring_value,
        token(match(";")),
        NULL
    );

    combinator_t* resourcestring_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("resourcestring")),
        many(resourcestring_decl),
        NULL
    );
    
    // Type constraint parser: T: class, T: record, T: constructor, T: interface
    combinator_t* constraint_keyword = multi(new_combinator(), PASCAL_T_TYPE_CONSTRAINT,
        token(keyword_ci("class")),
        token(keyword_ci("record")),
        token(keyword_ci("constructor")),
        token(keyword_ci("interface")),
        NULL
    );
    
    combinator_t* type_constraint = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        constraint_keyword,
        NULL
    ));
    
    // Type parameter with optional constraint: T or T: class
    combinator_t* type_param_with_constraint = seq(new_combinator(), PASCAL_T_TYPE_PARAM,
        token(cident(PASCAL_T_IDENTIFIER)),
        type_constraint,
        NULL
    );
    
    // Type parameter list parser: <T, U, V> or <T: class, U: record>
    combinator_t* type_param_list_required = seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(type_param_with_constraint, token(match(","))),
        token(match(">")),
        NULL
    );

    // Generic type declaration: TFoo<T> = class ...
    // This requires the angle brackets with type parameters
    combinator_t* generic_type_decl = seq(new_combinator(), PASCAL_T_GENERIC_TYPE_DECL,
        optional(token(keyword_ci("generic"))),      // optional generic keyword
        token(cident(PASCAL_T_IDENTIFIER)),           // type name
        type_param_list_required,                     // type parameters (REQUIRED - has angle brackets)
        token(match("=")),                           // equals sign
        type_definition,                              // type definition
        optional(token(match(";"))),                 // semicolon (optional for last decl)
        NULL
    );
    generic_type_decl = right(peek(make_generic_type_prefix()), generic_type_decl);

    // Regular type declaration: TFoo = Integer
    combinator_t* regular_type_decl = seq(new_combinator(), PASCAL_T_TYPE_DECL,
        optional(token(keyword_ci("generic"))),      // optional generic keyword
        token(cident(PASCAL_T_IDENTIFIER)),           // type name
        token(match("=")),                           // equals sign
        type_definition,                              // type definition
        optional(token(match(";"))),                 // semicolon (optional for last decl)
        NULL
    );

    combinator_t* type_decl = multi(new_combinator(), PASCAL_T_NONE,
        generic_type_decl,      // Try generic first (requires <...>)
        regular_type_decl,      // Fall back to regular (no <...>)
        NULL
    );

    combinator_t* type_section = seq(new_combinator(), PASCAL_T_TYPE_SECTION,
        token(keyword_ci("type")),                      // type keyword (with word boundary check)
        many(type_decl),                             // multiple type declarations
        NULL
    );

    combinator_t* param_list = create_pascal_param_parser();

    // Variable declaration for function/procedure local variables
    combinator_t** var_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *var_expr_parser = new_combinator();
    init_pascal_expression_parser(var_expr_parser, NULL);

    combinator_t* typed_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))), // variable name(s)
        token(match(":")),                          // colon
        type_definition,                             // full type definitions (array, record, etc.)
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match("=")),                       // optional initializer
            lazy(var_expr_parser),                   // initializer expression
            NULL
        )),
        optional(token(match(";"))),                // optional semicolon
        NULL
    );

    combinator_t* inferred_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // single variable name
        token(match(":=")),                         // assignment for inference
        lazy(var_expr_parser),                      // initializer expression
        optional(token(match(";"))),               // optional semicolon
        NULL
    );

    combinator_t* inferred_var_decl_guarded = right(peek(make_inferred_var_prefix()), inferred_var_decl);

    combinator_t* var_decl = multi(new_combinator(), PASCAL_T_NONE,
        inferred_var_decl_guarded,
        typed_var_decl,
        NULL
    );

    combinator_t* var_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("var")),                   // var keyword
        many(var_decl),                            // multiple variable declarations
        NULL
    );
    var_section->extra_to_free = var_expr_parser;

    combinator_t* threadvar_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("threadvar")),
        many(typed_var_decl),
        NULL
    );

    keyword_dispatch_args_t* local_decl_args = create_keyword_dispatch(3);
    size_t local_decl_index = 0;
    register_keyword_entry(local_decl_args, 3, &local_decl_index, "var", var_section);
    register_keyword_entry(local_decl_args, 3, &local_decl_index, "const", const_section);
    register_keyword_entry(local_decl_args, 3, &local_decl_index, "type", type_section);
    local_decl_args->entry_count = local_decl_index;

    combinator_t* local_decl_dispatch = new_combinator();
    local_decl_dispatch->type = COMB_KEYWORD_DISPATCH;
    local_decl_dispatch->fn = keyword_dispatch_fn;
    local_decl_dispatch->args = local_decl_args;

    // Function/procedure body that can contain local declarations
    combinator_t* function_body = seq(new_combinator(), PASCAL_T_FUNCTION_BODY,
        many(local_decl_dispatch),
        lazy(stmt_parser),                         // main statement block
        NULL
    );
    set_combinator_name(function_body, "function_body");

    // Routine directives like inline; overload; etc.
    combinator_t* directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("inline", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("overload", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("cdecl", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("stdcall", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("register", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("export", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("external", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("assembler", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("far", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("near", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("platform", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("deprecated", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("library", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("local", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("forward", PASCAL_T_IDENTIFIER)),
        NULL
    );

    // Extended external directive argument components
    combinator_t* external_name_clause = map(
        seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("name")),
            token(string(PASCAL_T_STRING)),
            NULL
        ),
        wrap_external_name_clause
    );

    combinator_t* external_index_clause = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("index")),
        token(integer(PASCAL_T_INTEGER)),
        NULL
    );

    combinator_t* extended_external_argument = seq(new_combinator(), PASCAL_T_NONE,
        multi(new_combinator(), PASCAL_T_NONE,
            token(string(PASCAL_T_STRING)),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        ),
        optional(external_name_clause),
        optional(external_index_clause),
        NULL
    );

    combinator_t* external_name_only_argument = seq(new_combinator(), PASCAL_T_NONE,
        external_name_clause,
        optional(external_index_clause),
        NULL
    );

    combinator_t* directive_argument = optional(multi(new_combinator(), PASCAL_T_NONE,
        external_name_only_argument,
        extended_external_argument,                  // extended external arguments
        token(string(PASCAL_T_STRING)),            // simple string argument
        token(cident(PASCAL_T_IDENTIFIER)),        // simple identifier argument
        NULL
    ));

    combinator_t* routine_directive = seq(new_combinator(), PASCAL_T_NONE,
        directive_keyword,
        directive_argument,
        token(match(";")),
        NULL
    );

    combinator_t* routine_directives = many(routine_directive);

    // Directives that indicate no body should follow - preserve the directive keyword in AST
    // combinator_t* no_body_directive = multi(new_combinator(), PASCAL_T_IDENTIFIER,
    //     map(token(keyword_ci("forward")), map_forward_directive),
    //     map(token(keyword_ci("external")), map_external_directive),
    //     map(token(keyword_ci("assembler")), map_assembler_directive),
    //     NULL
    // );

    // Forward/external/assembler declaration parsers for interface and implementation sections
    // These match procedure/function headers with special directives and NO body
    combinator_t* headeronly_procedure_decl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        optional(param_list),
        token(match(";")),
        routine_directive,                           // forward/external/assembler directive with arguments
        many(routine_directive),                     // additional directives (overload, etc.)
        NULL
    );
    set_combinator_name(headeronly_procedure_decl, "headeronly_procedure_decl");

    combinator_t* headeronly_function_decl = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        optional(param_list),
        token(match(":")),
        token(cident(PASCAL_T_RETURN_TYPE)),
        token(match(";")),
        routine_directive,                           // forward/external/assembler directive with arguments
        many(routine_directive),                     // additional directives (overload, etc.)
        NULL
    );
    set_combinator_name(headeronly_function_decl, "headeronly_function_decl");

    combinator_t* procedure_header = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(keyword_ci("procedure")),
        token(cident(PASCAL_T_IDENTIFIER)),
        param_list,
        token(match(";")),
        routine_directives,
        NULL);

    combinator_t* function_header = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        token(keyword_ci("function")),
        token(cident(PASCAL_T_IDENTIFIER)),
        param_list,
        token(match(":")),
        token(cident(PASCAL_T_RETURN_TYPE)),
        token(match(";")),
        routine_directives,
        NULL);

    // Simple procedure implementation for unit (with required body)
    combinator_t* procedure_impl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("procedure")), token(cident(PASCAL_T_IDENTIFIER)), optional(param_list), token(match(";")),
        routine_directives,
        function_body, optional(token(match(";"))), NULL);
    set_combinator_name(procedure_impl, "procedure_impl");

    // Helper for class-level generic type parameters in method implementations: ClassName<T>
    // This captures the generic type parameters that appear after the class name
    combinator_t* class_generic_type_params = optional(seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(">")),
        NULL
    ));

    // Method implementations with qualified names (Class.Method or Class<T>.Method)
    combinator_t* method_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),          // class name
        class_generic_type_params,                   // optional generic type params for class
        token(match(".")),                           // dot
        token(cident(PASCAL_T_IDENTIFIER)),          // method name
        NULL
    );

    // Operator name with class qualification (Class.+ or Class<T>.+)
    combinator_t* operator_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),          // class/record name
        class_generic_type_params,                   // optional generic type params for class
        token(match(".")),                           // dot
        token(operator_name(PASCAL_T_IDENTIFIER)),   // operator symbol or name
        NULL
    );

    // Return type for functions: : type
    combinator_t* return_type = seq(new_combinator(), PASCAL_T_RETURN_TYPE,
        token(match(":")),                           // colon
        token(cident(PASCAL_T_IDENTIFIER)),          // return type
        NULL
    );

    // Constructor implementation (with required body)
    combinator_t* constructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("constructor")),            // constructor keyword
        method_name_with_class,                      // ClassName.MethodName
        optional(param_list),                        // optional parameter list
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(constructor_impl, "constructor_impl");

    // Destructor implementation (with required body)
    combinator_t* destructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("destructor")),             // destructor keyword
        method_name_with_class,                      // ClassName.MethodName
        optional(param_list),                        // optional parameter list
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(destructor_impl, "destructor_impl");

    // Helper for optional method type parameter list: <T, U>
    combinator_t* constraint_kw_impl = multi(new_combinator(), PASCAL_T_TYPE_CONSTRAINT,
        token(keyword_ci("class")),
        token(keyword_ci("record")),
        token(keyword_ci("constructor")),
        token(keyword_ci("interface")),
        NULL
    );
    
    combinator_t* type_constraint_impl = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        constraint_kw_impl,
        NULL
    ));
    
    combinator_t* type_param_impl = seq(new_combinator(), PASCAL_T_TYPE_PARAM,
        token(cident(PASCAL_T_IDENTIFIER)),
        type_constraint_impl,
        NULL
    );
    
    combinator_t* method_type_params = optional(seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(type_param_impl, token(match(","))),
        token(match(">")),
        NULL
    ));

    // Method procedure implementation (with required body)
    combinator_t* method_procedure_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),        // optional class modifier
        token(keyword_ci("procedure")),              // procedure keyword
        method_name_with_class,                      // ClassName.MethodName
        method_type_params,                          // optional type parameters <T, U>
        optional(param_list),                        // optional parameter list
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(method_procedure_impl, "method_procedure_impl");

    // Method function implementation (with required body)
    combinator_t* method_function_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),        // optional class modifier
        token(keyword_ci("function")),               // function keyword
        method_name_with_class,                      // ClassName.MethodName
        method_type_params,                          // optional type parameters <T, U>
        optional(param_list),                        // optional parameter list
        return_type,                                 // return type
        token(match(";")),                           // semicolon
        routine_directives,
        function_body,                               // method body with local declarations
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );
    set_combinator_name(method_function_impl, "method_function_impl");

    // Simple function implementation for unit (with required body)
    combinator_t* function_impl = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("function")), token(cident(PASCAL_T_IDENTIFIER)), optional(param_list),
        return_type, token(match(";")),
        routine_directives,
        function_body, optional(token(match(";"))), NULL);
    set_combinator_name(function_impl, "function_impl");

    // Class operator implementation (with required body)
    combinator_t* class_operator_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),
        token(keyword_ci("operator")),
        operator_name_with_class,
        optional(param_list),
        return_type,
        token(match(";")),
        routine_directives,
        function_body,
        optional(token(match(";"))),
        NULL
    );
    set_combinator_name(class_operator_impl, "class_operator_impl");

    // Interface section declarations: uses, const, type, procedure/function headers
    keyword_dispatch_args_t* interface_dispatch_args = create_keyword_dispatch(8);
    size_t interface_entry_index = 0;
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "uses", uses_section);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "const", const_section);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "resourcestring", resourcestring_section);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "type", type_section);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "threadvar", threadvar_section);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "var", var_section);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "procedure", procedure_header);
    register_keyword_entry(interface_dispatch_args, 8, &interface_entry_index, "function", function_header);
    interface_dispatch_args->entry_count = interface_entry_index;

    combinator_t* interface_declaration = new_combinator();
    interface_declaration->type = COMB_KEYWORD_DISPATCH;
    interface_declaration->fn = keyword_dispatch_fn;
    interface_declaration->args = interface_dispatch_args;

    combinator_t* interface_declarations = many(interface_declaration);
    
    combinator_t* procedure_definitions = multi(new_combinator(), PASCAL_T_NONE,
        headeronly_procedure_decl,
        method_procedure_impl,
        procedure_impl,
        NULL
    );
    set_combinator_name(procedure_definitions, "procedure_definition_choice");

    combinator_t* function_definitions = multi(new_combinator(), PASCAL_T_NONE,
        headeronly_function_decl,
        method_function_impl,
        function_impl,
        NULL
    );
    set_combinator_name(function_definitions, "function_definition_choice");

    keyword_dispatch_args_t* implementation_dispatch_args = create_keyword_dispatch(11);
    size_t implementation_entry_index = 0;
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "uses", uses_section);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "const", const_section);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "resourcestring", resourcestring_section);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "type", type_section);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "threadvar", threadvar_section);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "var", var_section);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "constructor", constructor_impl);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "destructor", destructor_impl);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "procedure", procedure_definitions);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "function", function_definitions);
    register_keyword_entry(implementation_dispatch_args, 11, &implementation_entry_index, "operator", class_operator_impl);
    implementation_dispatch_args->entry_count = implementation_entry_index;
    static const char* implementation_skip_tokens[] = {"class"};
    implementation_dispatch_args->skip_keywords = implementation_skip_tokens;
    implementation_dispatch_args->skip_keyword_count = 1;

    combinator_t* implementation_definition = new_combinator();
    implementation_definition->type = COMB_KEYWORD_DISPATCH;
    implementation_definition->fn = keyword_dispatch_fn;
    implementation_definition->args = implementation_dispatch_args;
    set_combinator_name(implementation_definition, "implementation_definition_dispatch");

    combinator_t* implementation_definitions = many(implementation_definition);
    set_combinator_name(implementation_definitions, "implementation_definitions");

    combinator_t* interface_impl_keyword = token(keyword_ci("implementation"));
    combinator_t* interface_fallback = optional(seq(new_combinator(), PASCAL_T_NONE,
        until(interface_impl_keyword, PASCAL_T_NONE),
        NULL
    ));

    combinator_t* interface_section = seq(new_combinator(), PASCAL_T_INTERFACE_SECTION,
        token(keyword_ci("interface")),
        interface_declarations,
        interface_fallback,
        NULL);
    set_combinator_name(interface_section, "interface_section");

    combinator_t* unit_end_marker = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("end")),
        token(match(".")),
        NULL
    );

    combinator_t* implementation_end_marker = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("initialization")),
        token(keyword_ci("finalization")),
        token(keyword_ci("exports")),
        unit_end_marker,
        NULL
    );

    combinator_t* implementation_fallback = optional(seq(new_combinator(), PASCAL_T_NONE,
        until(implementation_end_marker, PASCAL_T_NONE),
        NULL
    ));

    combinator_t* implementation_section = seq(new_combinator(), PASCAL_T_IMPLEMENTATION_SECTION,
        token(keyword_ci("implementation")),
        implementation_definitions,
        implementation_fallback,
        NULL);
    set_combinator_name(implementation_section, "implementation_section");

    // Extend the sequence with optional exports/initialization/finalization sections and the final end.
    combinator_t* exports_end_delim = token(match(";"));
    combinator_t* exports_body = until(exports_end_delim, PASCAL_T_NONE);
    combinator_t* exports_section = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("exports")),
        exports_body,
        exports_end_delim,
        NULL
    ));

    
    
    combinator_t* initialization_section = optional(seq(new_combinator(), PASCAL_T_INITIALIZATION_SECTION,
        token(keyword_ci("initialization")),
        make_stmt_list_parser(stmt_parser),
        NULL
    ));

    combinator_t* finalization_section = optional(seq(new_combinator(), PASCAL_T_FINALIZATION_SECTION,
        token(keyword_ci("finalization")),
        make_stmt_list_parser(stmt_parser),
        NULL
    ));

    combinator_t* legacy_initialization_block = optional(map(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("begin")),
        make_stmt_list_parser(stmt_parser),
        NULL
    ), discard_ast));

    combinator_t* unit_semicolon_delim = token(match(";"));
    combinator_t* unit_directives = map(until(unit_semicolon_delim, PASCAL_T_NONE), discard_ast);

    seq(*p, PASCAL_T_UNIT_DECL,
        token(keyword_ci("unit")),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        unit_directives,
        token(match(";")),
        interface_section,
        implementation_section,
        exports_section,
        initialization_section,
        finalization_section,
        legacy_initialization_block,
        token(keyword_ci("end")),
        token(match(".")),
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}

// Custom parser for directive keywords - matches identifiers and validates they are directive keywords
// This is needed because cdecl, external, etc. are context-sensitive keywords, not reserved keywords
// Pascal Procedure/Function Declaration Parser
void init_pascal_procedure_parser(combinator_t** p) {
    // Create statement parser for procedure/function bodies
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Return type: : type (for functions)
    combinator_t* return_type = seq(new_combinator(), PASCAL_T_RETURN_TYPE,
        token(match(":")),                       // colon
        token(cident(PASCAL_T_IDENTIFIER)),      // return type (simplified)
        NULL
    );

    // Procedure declaration: procedure name [(params)] ; [directive;]* body
    combinator_t* procedure_param_list = create_simple_param_list();
    
    combinator_t* directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("inline")),
        token(keyword_ci("overload")),
        token(keyword_ci("cdecl")),
        token(keyword_ci("stdcall")),
        token(keyword_ci("register")),
        token(keyword_ci("export")),
        token(keyword_ci("external")),
        token(keyword_ci("assembler")),
        token(keyword_ci("far")),
        token(keyword_ci("near")),
        token(keyword_ci("platform")),
        token(keyword_ci("deprecated")),
        token(keyword_ci("library")),
        token(keyword_ci("local")),
        token(keyword_ci("forward")),
        NULL
    );

    // Extended external directive argument components
    combinator_t* external_name_clause = map(
        seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("name")),
            token(pascal_string(PASCAL_T_STRING)),
            NULL
        ),
        wrap_external_name_clause
    );

    combinator_t* external_index_clause = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("index")),
        token(integer(PASCAL_T_INTEGER)),
        NULL
    );

    combinator_t* extended_external_argument = seq(new_combinator(), PASCAL_T_NONE,
        multi(new_combinator(), PASCAL_T_NONE,
            token(pascal_string(PASCAL_T_STRING)),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        ),
        optional(external_name_clause),
        optional(external_index_clause),
        NULL
    );

    combinator_t* external_name_only_argument = seq(new_combinator(), PASCAL_T_NONE,
        external_name_clause,
        optional(external_index_clause),
        NULL
    );

    combinator_t* directive_argument = optional(multi(new_combinator(), PASCAL_T_NONE,
        external_name_only_argument,
        extended_external_argument,                  // extended external arguments
        token(pascal_string(PASCAL_T_STRING)),       // simple string argument
        token(cident(PASCAL_T_IDENTIFIER)),          // simple identifier argument
        NULL
    ));

    combinator_t* routine_directive = seq(new_combinator(), PASCAL_T_METHOD_DIRECTIVE,
        directive_keyword,
        directive_argument,
        token(match(";")),
        NULL
    );

    combinator_t* routine_directives = many(routine_directive);
    
    combinator_t* directive_stmt = seq(new_combinator(), PASCAL_T_NONE,
        directive_keyword,
        // Tolerate simple forms like: external name 'foo'; or external 'lib';
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("name")),
            token(pascal_string(PASCAL_T_STRING)),
            NULL
        )),
        optional(multi(new_combinator(), PASCAL_T_NONE,
            token(pascal_string(PASCAL_T_STRING)),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        )),
        token(match(";")),
        NULL
    );

    combinator_t* procedure_decl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(match("procedure")),               // procedure keyword
        token(cident(PASCAL_T_IDENTIFIER)),      // procedure name
        procedure_param_list,                    // optional parameter list
        token(match(";")),                       // semicolon ending signature
        routine_directives,                      // optional directive list
        optional(lazy(stmt_parser)),             // optional procedure body (for external/forward)
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Function declaration: function name [(params)] : return_type ; [directive;]* body
    combinator_t* function_param_list = create_simple_param_list();
    combinator_t* function_decl = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        token(match("function")),                // function keyword
        token(cident(PASCAL_T_IDENTIFIER)),      // function name
        function_param_list,                     // optional parameter list
        return_type,                             // return type
        token(match(";")),                       // semicolon ending signature
        routine_directives,                      // optional directive list
        optional(lazy(stmt_parser)),             // optional function body (for external/forward)
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Main procedure parser: try function or procedure declaration
    multi(*p, PASCAL_T_NONE,
        function_decl,                           // function declarations first
        procedure_decl,                          // procedure declarations second
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}

// Pascal Method Implementation Parser - for constructor/destructor/procedure implementations
void init_pascal_method_implementation_parser(combinator_t** p) {
    // Create statement parser for method bodies
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Helper for class-level generic type parameters in method implementations: ClassName<T>
    combinator_t* class_generic_type_params = optional(seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(">")),
        NULL
    ));

    // Method name with class: ClassName.MethodName or ClassName<T>.MethodName
    combinator_t* method_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),      // class name
        class_generic_type_params,               // optional generic type params for class
        token(match(".")),                       // dot
        token(cident(PASCAL_T_IDENTIFIER)),      // method name
        NULL
    );

    // Constructor implementation: constructor ClassName.MethodName[(params)]; body
    combinator_t* constructor_impl = seq(new_combinator(), PASCAL_T_CONSTRUCTOR_DECL,
        token(keyword_ci("constructor")),        // constructor keyword
        method_name_with_class,                  // ClassName.MethodName
        create_simple_param_list(),              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // method body
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Destructor implementation: destructor ClassName.MethodName[(params)]; body
    combinator_t* destructor_impl = seq(new_combinator(), PASCAL_T_DESTRUCTOR_DECL,
        token(keyword_ci("destructor")),         // destructor keyword
        method_name_with_class,                  // ClassName.MethodName
        create_simple_param_list(),              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // method body
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Procedure implementation: procedure ClassName.MethodName[(params)]; body
    combinator_t* procedure_impl = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(keyword_ci("procedure")),          // procedure keyword
        method_name_with_class,                  // ClassName.MethodName
        create_simple_param_list(),              // optional parameter list
        token(match(";")),                       // semicolon
        lazy(stmt_parser),                       // method body
        optional(token(match(";"))),             // optional terminating semicolon
        NULL
    );

    // Method implementation parser: constructor, destructor, or procedure implementation
    multi(*p, PASCAL_T_NONE,
        constructor_impl,
        destructor_impl,
        procedure_impl,
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}

// Pascal Complete Program Parser - for full Pascal programs
void init_pascal_complete_program_parser(combinator_t** p) {
    // Create procedure/function parsers for use in complete program
    // Need to create a modified procedure parser that supports var parameters
    combinator_t** stmt_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *stmt_parser = new_combinator();
    init_pascal_statement_parser(stmt_parser);

    // Use `between` to parse the content inside `begin` and `end`, then `map` to wrap it.
    combinator_t* main_block_content_parser = main_block_content(stmt_parser);
    combinator_t* main_block_body = between(
        token(keyword_ci("begin")),
        token(keyword_ci("end")),
        main_block_content_parser
    );
    combinator_t* main_block = map(main_block_body, build_main_block_ast);

    // Program parameter list: (identifier, identifier, ...)
    combinator_t* program_param = token(cident(PASCAL_T_IDENTIFIER));
    combinator_t* program_param_list = optional(map(
        between(
            token(match("(")),
            token(match(")")),
            sep_by(program_param, token(match(",")))
        ),
        wrap_program_params
    ));

    // Enhanced Variable declaration: var1, var2, var3 : type;
    combinator_t* var_identifier_list = sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(",")));
    
    // Constructed type parser for generic types like TFoo<Integer>
    // Peek to ensure this looks like a generic type before committing
    // This prevents parsing '<>' (not-equal operator) as an empty generic type
    combinator_t* type_arg = token(cident(PASCAL_T_TYPE_ARG));
    combinator_t* type_arg_list = seq(new_combinator(), PASCAL_T_TYPE_ARG_LIST,
        token(match("<")),
        sep_by1(type_arg, token(match(","))),  // Require at least one type argument
        token(match(">")),
        NULL
    );
    combinator_t* constructed_type = seq(new_combinator(), PASCAL_T_CONSTRUCTED_TYPE,
        optional(token(keyword_ci("specialize"))),      // optional specialize keyword
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        peek(create_generic_type_lookahead()),          // Lookahead to ensure '< identifier' pattern
        type_arg_list,                                  // Now parse the full type argument list
        NULL
    );

    combinator_t* type_spec = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        reference_to_type(PASCAL_T_REFERENCE_TO_TYPE),  // reference to procedure/function
        interface_type(PASCAL_T_INTERFACE_TYPE),        // interface types like interface ... end
        class_type(PASCAL_T_CLASS_TYPE),                // class types like class ... end
        record_type(PASCAL_T_RECORD_TYPE),              // record types like record ... end
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),      // enumerated types like (Value1, Value2, Value3)
        array_type(PASCAL_T_ARRAY_TYPE),                // array types like ARRAY[0..9] OF integer
        procedure_type(PASCAL_T_PROCEDURE_TYPE),        // procedure types like procedure(x: Integer)
        function_type(PASCAL_T_FUNCTION_TYPE),          // function types like function(y: Real): Boolean
        set_type(PASCAL_T_SET),                         // set types like set of TAsmSehDirective
        file_type(PASCAL_T_FILE_TYPE),                  // file types like file of Integer
        pointer_type(PASCAL_T_POINTER_TYPE),            // pointer types like ^TMyObject
        range_type(PASCAL_T_RANGE_TYPE),                // range types like -1..1
        type_name(PASCAL_T_IDENTIFIER),                 // built-in types
        constructed_type,                               // constructed types like TFoo<Integer> (try before plain identifiers)
        token(pascal_identifier_with_subscript(PASCAL_T_IDENTIFIER)),  // custom types with optional [size]
        NULL
    );

    combinator_t** program_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *program_expr_parser = new_combinator();
    init_pascal_expression_parser(program_expr_parser, NULL);

    // Support optional 'absolute <identifier>' clause
    combinator_t* absolute_clause = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("absolute")),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    ));

    combinator_t* typed_program_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        var_identifier_list,                            // multiple variable names
        token(match(":")),                              // colon
        type_spec,                                      // type specification
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match("=")),                          // optional initializer
            lazy(program_expr_parser),                  // initializer expression
            NULL
        )),
        absolute_clause,                                // optional absolute clause
        token(match(";")),                              // semicolon
        NULL
    );

    combinator_t* inferred_program_var_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),             // single variable name
        token(match(":=")),                            // assignment for inference
        lazy(program_expr_parser),                      // initializer expression
        token(match(";")),                              // semicolon
        NULL
    );

    combinator_t* inferred_program_var_guarded = right(peek(make_inferred_var_prefix()), inferred_program_var_decl);

    combinator_t* var_decl = multi(new_combinator(), PASCAL_T_NONE,
        inferred_program_var_guarded,
        typed_program_var_decl,
        NULL
    );

    // Var section: var var_decl var_decl ...
    combinator_t* var_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("var")),                       // var keyword (with word boundary check)
        commit(many(var_decl)),                              // multiple variable declarations
        NULL
    );
    var_section->extra_to_free = program_expr_parser;

    // Type constraint parser: T: class, T: record, T: constructor, T: interface
    combinator_t* constraint_keyword_prog = multi(new_combinator(), PASCAL_T_TYPE_CONSTRAINT,
        token(keyword_ci("class")),
        token(keyword_ci("record")),
        token(keyword_ci("constructor")),
        token(keyword_ci("interface")),
        NULL
    );
    
    combinator_t* type_constraint_prog = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        constraint_keyword_prog,
        NULL
    ));
    
    // Type parameter with optional constraint: T or T: class
    combinator_t* type_param_with_constraint_prog = seq(new_combinator(), PASCAL_T_TYPE_PARAM,
        token(cident(PASCAL_T_IDENTIFIER)),
        type_constraint_prog,
        NULL
    );
    
    // Type parameter list parser: <T, U, V> or <T: class, U: record>
    combinator_t* type_param_list_required_prog = seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(type_param_with_constraint_prog, token(match(","))),
        token(match(">")),
        NULL
    );

    // Generic type declaration: TFoo<T> = class ...
    // This requires the angle brackets with type parameters
    // Allow optional routine directives (e.g., stdcall) after subroutine types
    combinator_t* type_directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("stdcall")),
        token(keyword_ci("cdecl")),
        token(keyword_ci("register")),
        token(keyword_ci("safecall")),
        token(keyword_ci("pascal")),
        token(keyword_ci("export")),
        token(keyword_ci("external")),
        token(keyword_ci("inline")),
        token(keyword_ci("overload")),
        NULL
    );
    combinator_t* generic_type_decl_prog = seq(new_combinator(), PASCAL_T_GENERIC_TYPE_DECL,
        optional(token(keyword_ci("generic"))),      // optional generic keyword
        token(cident(PASCAL_T_IDENTIFIER)),           // type name
        type_param_list_required_prog,                // type parameters (REQUIRED - has angle brackets)
        token(match("=")),                           // equals sign
        type_spec,                                    // type specification
        // If a calling convention or directive follows the type, skip up to ';'
        optional(seq(new_combinator(), PASCAL_T_NONE,
            right(peek(type_directive_keyword),
                 map(until(token(match(";")), PASCAL_T_NONE), discard_ast)),
            NULL
        )),
        token(match(";")),                           // semicolon
        NULL
    );
    generic_type_decl_prog = right(peek(make_generic_type_prefix()), generic_type_decl_prog);

    // Regular type declaration: TFoo = Integer
    combinator_t* regular_type_decl_prog = seq(new_combinator(), PASCAL_T_TYPE_DECL,
        optional(token(keyword_ci("generic"))),      // optional generic keyword
        token(cident(PASCAL_T_IDENTIFIER)),           // type name
        token(match("=")),                           // equals sign
        type_spec,                                    // type specification
        optional(seq(new_combinator(), PASCAL_T_NONE,
            right(peek(type_directive_keyword),
                 map(until(token(match(";")), PASCAL_T_NONE), discard_ast)),
            NULL
        )),
        token(match(";")),                           // semicolon
        NULL
    );

    combinator_t* type_decl = multi(new_combinator(), PASCAL_T_NONE,
        generic_type_decl_prog,     // Try generic first (requires <...>)
        regular_type_decl_prog,     // Fall back to regular (no <...>)
        NULL
    );

    // Type section: type type_decl type_decl ...
    combinator_t* type_section = seq(new_combinator(), PASCAL_T_TYPE_SECTION,
        token(keyword_ci("type")),                      // type keyword (with word boundary check)
        commit(many(type_decl)),                             // multiple type declarations
        NULL
    );

    // Uses section: uses unit1, unit2, unit3;
    combinator_t* uses_unit = token(pascal_qualified_identifier(PASCAL_T_USES_UNIT));
    combinator_t* uses_section = seq(new_combinator(), PASCAL_T_USES_SECTION,
        token(keyword_ci("uses")),                      // uses keyword (with word boundary check)
        sep_by(uses_unit, token(match(","))),        // unit names separated by commas
        token(match(";")),                           // semicolon
        NULL
    );

    // Const section: const name : type = value; ...
    // Reuse the expression parser so program-level constants support the
    // complete expression grammar instead of a limited subset.
    combinator_t** program_const_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *program_const_expr_parser = new_combinator();
    init_pascal_expression_parser(program_const_expr_parser, NULL);

    combinator_t* const_value = lazy(program_const_expr_parser);

    combinator_t* const_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),          // constant name
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),                       // colon
            // Try to parse the full type spec; if that fails, consume up to '=' as a fallback
            multi(new_combinator(), PASCAL_T_NONE,
                type_spec,
                map(until(token(match("=")), PASCAL_T_NONE), discard_ast),
                NULL
            ),
            NULL
        )),
        token(match("=")),                           // equals
        const_value,
        token(match(";")),                           // semicolon
        NULL
    );

    combinator_t* const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),                     // const keyword (with word boundary check)
        commit(many(const_decl)),                        // multiple const declarations
        NULL
    );
    const_section->extra_to_free = program_const_expr_parser;

    // Return type: : type (for functions)
    // Support complex types like arrays, sets, pointers, etc., not just simple identifiers
    combinator_t* return_type_spec = multi(new_combinator(), PASCAL_T_TYPE_SPEC,
        array_type(PASCAL_T_ARRAY_TYPE),
        set_type(PASCAL_T_SET),
        range_type(PASCAL_T_RANGE_TYPE),
        pointer_type(PASCAL_T_POINTER_TYPE),
        enumerated_type(PASCAL_T_ENUMERATED_TYPE),
        record_type(PASCAL_T_RECORD_TYPE),
        procedure_type(PASCAL_T_PROCEDURE_TYPE),
        function_type(PASCAL_T_FUNCTION_TYPE),
        reference_to_type(PASCAL_T_REFERENCE_TO_TYPE),
        token(cident(PASCAL_T_IDENTIFIER)),          // simple type identifier (fallback)
        NULL
    );
    
    combinator_t* return_type = seq(new_combinator(), PASCAL_T_RETURN_TYPE,
        token(match(":")),                           // colon
        return_type_spec,                            // comprehensive type specification
        NULL
    );

    // Function body parser: handles local sections followed by begin-end block
    // This is different from statement parsing - functions can have local declarations

    // Local VAR section - reuse the existing var_section parser
    combinator_t* local_var_section = var_section;

    // Create a specialized function body parser that avoids circular references
    // This parser handles the most common function body patterns without full recursive complexity

    // Function body for standalone parsing (no terminating semicolon)
    // Function body that can contain nested function/procedure declarations
    combinator_t** nested_proc_or_func = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *nested_proc_or_func = new_combinator();
    (*nested_proc_or_func)->extra_to_free = nested_proc_or_func;

    // Forward declaration for nested functions - these will refer to working_function and working_procedure below
    combinator_t* nested_function_decl = lazy_owned(nested_proc_or_func);

    // Permissive local CONST fallback (routine bodies only): if strict const_section fails
    combinator_t* lenient_const_item = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match(":")),
            map(until(token(match("=")), PASCAL_T_NONE), discard_ast),
            NULL
        )),
        token(match("=")),
        map(until(token(match(";")), PASCAL_T_NONE), discard_ast),
        token(match(";")),
        NULL
    );
    combinator_t* lenient_const_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("const")),
        many(lenient_const_item),
        NULL
    );

    // Local-only resourcestring and threadvar sections (routine scope tolerance)
    combinator_t* local_resourcestring_value = multi(new_combinator(), PASCAL_T_NONE,
        token(pascal_string(PASCAL_T_STRING)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
    combinator_t* local_resourcestring_decl = seq(new_combinator(), PASCAL_T_CONST_DECL,
        token(cident(PASCAL_T_IDENTIFIER)),
        token(match("=")),
        local_resourcestring_value,
        token(match(";")),
        NULL
    );
    combinator_t* local_resourcestring_section = seq(new_combinator(), PASCAL_T_CONST_SECTION,
        token(keyword_ci("resourcestring")),
        many(local_resourcestring_decl),
        NULL
    );
    combinator_t** local_var_expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *local_var_expr_parser = new_combinator();
    init_pascal_expression_parser(local_var_expr_parser, NULL);
    combinator_t* local_threadvar_decl = seq(new_combinator(), PASCAL_T_VAR_DECL,
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(":")),
        type_spec,
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match("=")),
            lazy(local_var_expr_parser),
            NULL
        )),
        optional(token(match(";"))),
        NULL
    );
    combinator_t* local_threadvar_section = seq(new_combinator(), PASCAL_T_VAR_SECTION,
        token(keyword_ci("threadvar")),
        many(local_threadvar_decl),
        NULL
    );
    local_threadvar_section->extra_to_free = local_var_expr_parser;

    // Allow local CONST/TYPE/VAR sections to be interspersed with nested function/procedure declarations
    combinator_t* local_declaration_or_nested_function = multi(new_combinator(), PASCAL_T_NONE,
        create_label_section(),
        const_section,
        lenient_const_section,
        local_resourcestring_section,
        local_threadvar_section,
        type_section,
        local_var_section,
        nested_function_decl,
        NULL
    );

    combinator_t* nested_function_body = seq(new_combinator(), PASCAL_T_NONE,
        many(local_declaration_or_nested_function), // zero or more local sections or nested functions
        lazy(stmt_parser),                          // begin-end block handled by statement parser
        NULL
    );

    // Use the nested function body parser for complete programs to support nested functions
    // Add permissive fallbacks for unexpected local declarations or tricky bodies
    combinator_t* body_with_decls = nested_function_body;
    combinator_t* skip_to_begin_then_body = seq(new_combinator(), PASCAL_T_NONE,
        map(until(token(keyword_ci("begin")), PASCAL_T_NONE), discard_ast),
        lazy(stmt_parser),
        NULL
    );
    combinator_t* direct_begin_body = right(peek(token(keyword_ci("begin"))), lazy(stmt_parser));
    combinator_t* program_function_body = multi(new_combinator(), PASCAL_T_NONE,
        body_with_decls,
        direct_begin_body,
        skip_to_begin_then_body,
        skip_balanced_begin_end(),
        NULL
    );

    // Create a method body parser that supports local var sections
    // This is simpler than program_function_body since methods don't support nested functions
    combinator_t* method_local_section = multi(new_combinator(), PASCAL_T_NONE,
        create_label_section(),
        local_var_section,
        NULL
    );

    combinator_t* method_body = seq(new_combinator(), PASCAL_T_NONE,
        many(method_local_section),                  // zero or more local sections
        lazy(stmt_parser),                           // begin-end block handled by statement parser
        NULL
    );

    // Create simple working function and procedure parsers based on the nested version
    // These work because they use the recursive statement parser for bodies

    // Routine directives like inline; overload; forward; etc.
    // Use custom parser for directive keywords since they are context-sensitive, not reserved
    combinator_t* directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("inline", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("overload", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("cdecl", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("stdcall", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("register", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("export", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("external", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("assembler", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("far", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("near", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("platform", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("deprecated", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("library", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("local", PASCAL_T_IDENTIFIER)),
        token(create_keyword_parser("forward", PASCAL_T_IDENTIFIER)),
        NULL
    );

    // Copy exact working structure from unit parser
    combinator_t* external_name_clause = map(
        seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("name")),
            token(pascal_string(PASCAL_T_STRING)),
            NULL
        ),
        wrap_external_name_clause
    );

    combinator_t* external_index_clause = seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("index")),
        token(integer(PASCAL_T_INTEGER)),
        NULL
    );

    combinator_t* extended_external_argument = seq(new_combinator(), PASCAL_T_NONE,
        multi(new_combinator(), PASCAL_T_NONE,
            token(pascal_string(PASCAL_T_STRING)),
            token(cident(PASCAL_T_IDENTIFIER)),
            NULL
        ),
        optional(external_name_clause),
        optional(external_index_clause),
        NULL
    );

    combinator_t* external_name_only_argument = seq(new_combinator(), PASCAL_T_NONE,
        external_name_clause,
        optional(external_index_clause),
        NULL
    );

    combinator_t* directive_argument = optional(multi(new_combinator(), PASCAL_T_NONE,
        external_name_only_argument,
        extended_external_argument,                  // extended external arguments (try first)
        token(pascal_string(PASCAL_T_STRING)),      // simple string argument
        token(cident(PASCAL_T_IDENTIFIER)),        // simple identifier argument
        NULL
    ));

    // Routine directives with proper argument support
    combinator_t* routine_directive = seq(new_combinator(), PASCAL_T_NONE,
        directive_keyword,
        directive_argument,
        token(match(";")),
        NULL
    );

    combinator_t* program_routine_directives = many(routine_directive);

    // Routine directives that still allow an implementation body (exclude forward/external/assembler)
    combinator_t* impl_directive_keyword = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("inline")),
        token(keyword_ci("overload")),
        token(keyword_ci("cdecl")),
        token(keyword_ci("stdcall")),
        token(keyword_ci("register")),
        token(keyword_ci("export")),
        token(keyword_ci("far")),
        token(keyword_ci("near")),
        token(keyword_ci("platform")),
        token(keyword_ci("deprecated")),
        token(keyword_ci("library")),
        token(keyword_ci("local")),
        NULL
    );
    combinator_t* implementation_routine_directive = seq(new_combinator(), PASCAL_T_NONE,
        impl_directive_keyword,
        directive_argument,
        token(match(";")),
        NULL
    );
    combinator_t* implementation_routine_directives = many(implementation_routine_directive);

    // Directives that indicate no body should follow - preserve the directive keyword in AST
    // combinator_t* program_no_body_directive = multi(new_combinator(), PASCAL_T_IDENTIFIER,
    //     map(token(keyword_ci("forward")), map_forward_directive),
    //     map(token(keyword_ci("external")), map_external_directive),
    //     map(token(keyword_ci("assembler")), map_assembler_directive),
    //     NULL
    // );

    // Header-only declaration parsers - these match procedure/function with forward/external/assembler directive and NO body
    combinator_t* headeronly_procedure = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        optional(token(keyword_ci("class"))),        // optional class keyword
        token(keyword_ci("procedure")),               // procedure keyword
        token(cident(PASCAL_T_IDENTIFIER)),          // procedure name
        optional(create_simple_param_list()),         // optional parameter list
        token(match(";")),                           // semicolon after signature
        routine_directive,                           // forward/external/assembler directive with arguments
        many(routine_directive),                     // additional directives (overload, etc.)
        NULL
    );

    combinator_t* headeronly_function = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        optional(token(keyword_ci("class"))),        // optional class keyword
        token(keyword_ci("function")),               // function keyword
        token(cident(PASCAL_T_IDENTIFIER)),          // function name
        optional(create_simple_param_list()),         // optional parameter list
        token(match(":")),                           // colon before return type
        token(cident(PASCAL_T_RETURN_TYPE)),         // return type
        token(match(";")),                           // semicolon after signature
        routine_directive,                           // forward/external/assembler directive with arguments
        many(routine_directive),                     // additional directives (overload, etc.)
        NULL
    );

    // Working function parser: function name [(params)] : return_type ; [directives] body ;
    // Does NOT support forward (that's handled by forward_function)
    combinator_t* working_function_param_list = create_simple_param_list();
    // Guard: implementation must not start if next directive is 'forward'/'external'/'assembler'
    combinator_t* no_body_directive = multi(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("forward")),
        token(keyword_ci("external")),
        token(keyword_ci("assembler")),
        NULL
    );
    combinator_t* forbid_no_body = pnot(peek(no_body_directive));

    combinator_t* working_function = seq(new_combinator(), PASCAL_T_FUNCTION_DECL,
        token(keyword_ci("function")),               // function keyword (with word boundary check)
        token(cident(PASCAL_T_IDENTIFIER)),          // function name
        working_function_param_list,                 // optional parameter list
        return_type,                                 // return type
        token(match(";")),                           // semicolon after signature
        forbid_no_body,                              // do not match implementation if header-only directive follows
        implementation_routine_directives,           // directives allowed alongside an implementation
        program_function_body,                       // function body (required for non-forward declarations)
        optional(token(match(";"))),                 // optional terminating semicolon after function body
        NULL
    );

    // Working procedure parser: procedure name [(params)] ; [directives] body ;
    // Does NOT support forward (that's handled by forward_procedure)
    combinator_t* working_procedure_param_list = create_simple_param_list();
    combinator_t* working_procedure = seq(new_combinator(), PASCAL_T_PROCEDURE_DECL,
        token(keyword_ci("procedure")),                // procedure keyword (case-insensitive)
        token(cident(PASCAL_T_IDENTIFIER)),          // procedure name
        working_procedure_param_list,               // optional parameter list
        token(match(";")),                           // semicolon after signature
        forbid_no_body,
        implementation_routine_directives,
        program_function_body,                       // procedure body (required for non-forward declarations)
        optional(token(match(";"))),                 // optional terminating semicolon after procedure body
        NULL
    );

    // Object Pascal method implementations (constructor/destructor/procedure with class.method syntax)
    // These are Object Pascal extensions, not standard Pascal
    
    // Helper for class-level generic type parameters in method implementations: ClassName<T>
    combinator_t* class_generic_type_params = optional(seq(new_combinator(), PASCAL_T_TYPE_PARAM_LIST,
        token(match("<")),
        sep_by(token(cident(PASCAL_T_IDENTIFIER)), token(match(","))),
        token(match(">")),
        NULL
    ));

    combinator_t* method_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),          // class name
        class_generic_type_params,                   // optional generic type params for class
        token(match(".")),                           // dot
        token(cident(PASCAL_T_IDENTIFIER)),          // method name
        NULL
    );

    combinator_t* constructor_param_list = create_simple_param_list();
    combinator_t* constructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("constructor")),              // constructor keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        constructor_param_list,                      // optional parameter list
        token(match(";")),                           // semicolon
        program_routine_directives,                  // routine directives (inline, overload, etc.)
        method_body,                                 // method body with var section support
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    combinator_t* destructor_param_list = create_simple_param_list();
    combinator_t* destructor_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("destructor")),               // destructor keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        destructor_param_list,                       // optional parameter list
        token(match(";")),                           // semicolon
        program_routine_directives,                  // routine directives (inline, overload, etc.)
        method_body,                                 // method body with var section support
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    combinator_t* method_procedure_param_list = create_simple_param_list();
    combinator_t* procedure_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("procedure")),              // procedure keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        method_procedure_param_list,                 // optional parameter list
        token(match(";")),                           // semicolon
        program_routine_directives,                  // routine directives (inline, overload, etc.)
        method_body,                                 // method body with var section support
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    combinator_t* method_function_param_list = create_simple_param_list();
    combinator_t* method_function_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),        // optional class keyword
        token(keyword_ci("function")),               // function keyword (with word boundary check)
        method_name_with_class,                      // ClassName.MethodName
        method_function_param_list,                  // optional parameter list
        return_type,                                 // return type
        token(match(";")),                           // semicolon
        program_routine_directives,                  // routine directives (inline, overload, etc.)
        method_body,                                 // method body with var section support
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    // Operator name with class qualification (Class.+ or Class.Add)
    combinator_t* operator_name_with_class = seq(new_combinator(), PASCAL_T_QUALIFIED_IDENTIFIER,
        token(cident(PASCAL_T_IDENTIFIER)),          // class/record name
        token(match(".")),                           // dot
        token(operator_name(PASCAL_T_IDENTIFIER)),   // operator symbol or name
        NULL
    );

    combinator_t* operator_param_list = create_simple_param_list();
    combinator_t* operator_impl = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        optional(token(keyword_ci("class"))),        // optional class keyword
        token(keyword_ci("operator")),               // operator keyword
        operator_name_with_class,                    // ClassName.OperatorName
        operator_param_list,                         // parameter list
        return_type,                                 // return type
        token(match(";")),                           // semicolon
        program_routine_directives,                  // routine directives (inline, overload, etc.)
        method_body,                                 // method body with var section support
        optional(token(match(";"))),                 // optional terminating semicolon
        NULL
    );

    // Header-only Object Pascal methods (no body): procedure/function ClassName.Method [(params)] [: type] ; directives ;
    combinator_t* headeronly_method_procedure = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("procedure")),
        method_name_with_class,
        method_procedure_param_list,
        token(match(";")),
        program_routine_directives,
        NULL
    );

    combinator_t* headeronly_method_function = seq(new_combinator(), PASCAL_T_METHOD_IMPL,
        token(keyword_ci("function")),
        method_name_with_class,
        operator_param_list,
        return_type,
        token(match(";")),
        program_routine_directives,
        NULL
    );

    // Object Pascal method implementations and header-only variants
    combinator_t* method_impl = multi(new_combinator(), PASCAL_T_NONE,
        constructor_impl,
        destructor_impl,
        procedure_impl,
        method_function_impl,
        headeronly_method_procedure,
        headeronly_method_function,
        operator_impl,
        NULL
    );

    // Standard Pascal procedure or function definitions
    // Prefer implementations first so that headers with routine directives
    // (e.g., stdcall; cdecl; inline;) followed by a body are parsed as bodies,
    // not mistaken for header-only declarations.
    combinator_t* proc_or_func = multi(new_combinator(), PASCAL_T_NONE,
        working_function,                            // function implementations
        working_procedure,                           // procedure implementations
        headeronly_function,                         // forward/external/assembler function declarations
        headeronly_procedure,                        // forward/external/assembler procedure declarations
        NULL
    );

    // Combined Pascal and Object Pascal declarations
    combinator_t* all_declarations = multi(new_combinator(), PASCAL_T_NONE,
        proc_or_func,                                // standard Pascal procedures/functions
        method_impl,                                 // Object Pascal method implementations
        NULL
    );

    // Set up the nested function parser to point to the working function/procedure parsers
    // This allows nested function/procedure declarations within function bodies
    // Nested functions can also have forward/external/assembler declarations
    multi(*nested_proc_or_func, PASCAL_T_NONE,
        working_function,                            // nested function implementations
        working_procedure,                           // nested procedure implementations
        headeronly_function,                         // forward/external/assembler nested function declarations
        headeronly_procedure,                        // forward/external/assembler nested procedure declarations
        NULL
    );

    // Allow const/type/var sections to be interspersed with procedure/function declarations
    // Parse them in a single many() to avoid backtracking issues
    // Try declaration sections first (they have distinctive keywords), then procedures/functions
    // Try procedures/functions first to avoid the main block 'begin' being
    // misinterpreted from within a routine when routine directives are present.
    // Then handle declaration sections.
    combinator_t* declaration_or_section = multi(new_combinator(), PASCAL_T_NONE,
        all_declarations,   // Procedures/functions (keywords "procedure", "function", etc.)
        create_label_section(),    // Label declarations
        const_section,      // const
        type_section,       // type
        var_section,        // var
        NULL
    );

    // Support optional "program" or "library" header so unit-less Pascal files can be parsed.
    combinator_t* program_header = multi(new_combinator(), PASCAL_T_PROGRAM_HEADER,
        seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("program")),
            token(cident(PASCAL_T_IDENTIFIER)),
            program_param_list,
            token(match(";")),
            NULL
        ),
        seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("library")),
            token(cident(PASCAL_T_IDENTIFIER)),
            token(match(";")),
            NULL
        ),
        NULL
    );

    // Optional exports section (useful for libraries)
    combinator_t* prog_exports_end = token(match(";"));
    combinator_t* prog_exports_body = until(prog_exports_end, PASCAL_T_NONE);
    combinator_t* exports_section_prog = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("exports")),
        prog_exports_body,
        prog_exports_end,
        NULL
    ));

    bool disable_skip_period = getenv("GPC_DEBUG_NO_SKIP_PERIOD") != NULL;
    if (disable_skip_period && getenv("GPC_DEBUG_STATEMENT_DISPATCH") != NULL) {
        fprintf(stderr, "[pascal_parser] strict final period enabled\n");
    }
    combinator_t* final_period = disable_skip_period
        ? token(match("."))
        : multi(new_combinator(), PASCAL_T_NONE,
            token(match(".")),
            ({ combinator_t* s = new_combinator(); s->fn = skip_to_final_period_fn; s->type = P_MATCH; s; }),
            NULL
        );

    // Complete program: optional header; optional uses clause; declarations/sections interspersed; optional exports; optional main block.
    seq(*p, PASCAL_T_PROGRAM_DECL,
        optional(program_header),                    // optional "program" header
        optional(uses_section),                      // optional uses clause
        many(declaration_or_section),                // const/type/var sections and procedures/functions in any order
        exports_section_prog,                        // optional exports section (for libraries)
        optional(main_block),                        // optional main program block
        // final period (strict), or fallback to last '.' in file
        final_period,
        NULL
    );

    (*p)->extra_to_free = stmt_parser;
}
