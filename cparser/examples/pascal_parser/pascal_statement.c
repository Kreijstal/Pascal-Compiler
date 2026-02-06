#include "pascal_statement.h"
#include "pascal_parser.h"
#include "pascal_expression.h"
#include "pascal_keywords.h"
#include "pascal_peek.h"
#include "pascal_statement_keywords.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifndef _WIN32
#include <strings.h>
#else
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

// Forward declaration
static ast_t* build_pointer_lvalue_chain(ast_t* parsed);

static bool peek_label_statement(input_t* in) {
    if (in == NULL || in->buffer == NULL) {
        return false;
    }
    int length = in->length > 0 ? in->length : (int)strlen(in->buffer);
    int pos = skip_pascal_layout_preview(in, in->start);
    if (pos >= length) {
        return false;
    }
    const char* buffer = in->buffer;
    unsigned char ch = (unsigned char)buffer[pos];

    // Character literal case labels: 'X': or 'X'..'Y':
    if (ch == '\'') {
        pos++;
        if (pos >= length) return false;
        pos++;  // skip the character
        if (pos >= length || buffer[pos] != '\'') return false;
        pos++;  // skip closing quote
        int after = skip_pascal_layout_preview(in, pos);
        if (after < length && buffer[after] == ':' &&
            !(after + 1 < length && buffer[after + 1] == '=')) {
            return true;
        }
        // Check for range: 'X'..'Y':
        if (after + 1 < length && buffer[after] == '.' && buffer[after + 1] == '.') {
            return true;
        }
        return false;
    }

    // Control char literals: #nn:
    if (ch == '#') {
        pos++;
        while (pos < length && isdigit((unsigned char)buffer[pos])) {
            pos++;
        }
        int after = skip_pascal_layout_preview(in, pos);
        if (after < length && buffer[after] == ':' &&
            !(after + 1 < length && buffer[after + 1] == '=')) {
            return true;
        }
        if (after + 1 < length && buffer[after] == '.' && buffer[after + 1] == '.') {
            return true;
        }
        return false;
    }

    if (!(isalpha(ch) || ch == '_' || isdigit(ch))) {
        return false;
    }
    if (isdigit(ch)) {
        while (pos < length && isdigit((unsigned char)buffer[pos])) {
            pos++;
        }
    } else {
        while (pos < length && (isalnum((unsigned char)buffer[pos]) || buffer[pos] == '_')) {
            pos++;
        }
    }
    int after = skip_pascal_layout_preview(in, pos);
    if (after < length && buffer[after] == ':' &&
        !(after + 1 < length && buffer[after + 1] == '=')) {
        return true;
    }
    return false;
}

static bool peek_assignment_operator(input_t* in) {
    if (in == NULL || in->buffer == NULL) {
        return false;
    }
    const char* buffer = in->buffer;
    int length = in->length > 0 ? in->length : (int)strlen(buffer);
    int pos = skip_pascal_layout_preview(in, in->start);
    const int scan_limit = pos + 512 < length ? pos + 512 : length;
    bool in_string = false;
    
    // Trace peek
    if (getenv("KGPC_DEBUG_TRACE") != NULL) {
         FILE* f = fopen("/tmp/parser_trace.log", "a");
         if (f) { fprintf(f, "TRACE: peek_assign at %d starting at '%.10s'\n", in ? in->line : 0, buffer + pos); fclose(f); }
    }

    while (pos < scan_limit) {
        unsigned char ch = (unsigned char)buffer[pos];
        if (in_string) {
            if (ch == '\'') {
                if (pos + 1 < length && buffer[pos + 1] == '\'') {
                    pos += 2;
                    continue;
                }
                in_string = false;
                pos++;
                continue;
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
        if (isspace(ch)) {
            pos = skip_pascal_layout_preview(in, pos);
            continue;
        }
        if (ch == ':' && pos + 1 < length && buffer[pos + 1] == '=') {
            return true;
        }
        if (ch == '+' && pos + 1 < length && buffer[pos + 1] == '=') {
            return true;
        }
        if (ch == ';' || ch == '\n' || ch == '\r') {
            return false;
        }
        /* Check if we're at a statement-terminating keyword like 'else', 'then', 'do', 'end', 'until'.
         * These keywords mark the boundary of a statement, so we should not scan past them
         * looking for an assignment operator. */
        if (isalpha(ch)) {
            int kw_start = pos;
            int kw_end = pos;
            while (kw_end < length && (isalnum((unsigned char)buffer[kw_end]) || buffer[kw_end] == '_')) {
                kw_end++;
            }
            size_t kw_len = (size_t)(kw_end - kw_start);
            if (kw_len >= 2 && kw_len <= 7) {
                /* Check for statement-terminating keywords (case-insensitive) */
                if ((kw_len == 4 && strncasecmp(buffer + kw_start, "else", 4) == 0) ||
                    (kw_len == 4 && strncasecmp(buffer + kw_start, "then", 4) == 0) ||
                    (kw_len == 2 && strncasecmp(buffer + kw_start, "do", 2) == 0) ||
                    (kw_len == 3 && strncasecmp(buffer + kw_start, "end", 3) == 0) ||
                    (kw_len == 5 && strncasecmp(buffer + kw_start, "until", 5) == 0) ||
                    (kw_len == 6 && strncasecmp(buffer + kw_start, "except", 6) == 0) ||
                    (kw_len == 7 && strncasecmp(buffer + kw_start, "finally", 7) == 0)) {
                    return false;
                }
            }
            pos = kw_end;
            continue;
        }
        pos++;
    }
    return false;
}

typedef struct {
    combinator_t **stmt_parser;
    combinator_t *case_label_list;
} case_stmt_list_args;

static bool peek_colon_not_assign(input_t* in) {
    if (in == NULL || in->buffer == NULL) {
        return false;
    }
    int length = in->length > 0 ? in->length : (int)strlen(in->buffer);
    int pos = skip_pascal_layout_preview(in, in->start);
    if (pos >= length) {
        return false;
    }
    if (in->buffer[pos] != ':') {
        return false;
    }
    if ((pos + 1) < length && in->buffer[pos + 1] == '=') {
        return false;
    }
    return true;
}

static bool case_branch_should_stop(input_t* in, combinator_t* case_label_list) {
    pascal_word_slice_t slice;
    if (pascal_peek_word(in, &slice)) {
        if (pascal_word_equals_ci(&slice, "else") ||
            pascal_word_equals_ci(&slice, "end")) {
            return true;
        }
    }
    if (peek_label_statement(in)) {
        return true;
    }
    if (case_label_list != NULL) {
        InputState state;
        save_input_state(in, &state);
        ParseResult res = parse(in, case_label_list);
        if (res.is_success) {
            if (res.value.ast != ast_nil) {
                free_ast(res.value.ast);
            }
            if (peek_colon_not_assign(in)) {
                restore_input_state(in, &state);
                return true;
            }
        } else if (res.value.error != NULL) {
            free_error(res.value.error);
        }
        restore_input_state(in, &state);
    }
    return false;
}

static ParseResult case_branch_stmt_list_fn(input_t* in, void* args, char* parser_name) {
    case_stmt_list_args* clargs = (case_stmt_list_args*)args;
    if (clargs == NULL || clargs->stmt_parser == NULL || *clargs->stmt_parser == NULL) {
        return make_failure_v2(in, parser_name, strdup("Invalid case branch parser state"), NULL);
    }

    InputState state;
    save_input_state(in, &state);

    ast_t* first = NULL;
    ast_t* last = NULL;
    int stmt_count = 0;

    while (!case_branch_should_stop(in, clargs->case_label_list)) {
        ParseResult stmt_res = parse(in, *clargs->stmt_parser);
        if (!stmt_res.is_success) {
            if (stmt_res.value.error != NULL) {
                free_error(stmt_res.value.error);
            }
            // Allow empty case branch bodies (e.g., "1: ;" where body is empty).
            // In Pascal, empty statements are valid in case branches.
            break;
        }

        ast_t* stmt_ast = stmt_res.value.ast;
        if (stmt_ast == ast_nil) {
            stmt_ast = NULL;
        }
        if (stmt_ast != NULL) {
            if (first == NULL) {
                first = stmt_ast;
            } else if (last != NULL) {
                last->next = stmt_ast;
            }
            last = stmt_ast;
            stmt_count++;
        }

        InputState semi_state;
        save_input_state(in, &semi_state);
        combinator_t* semi = token(match(";"));
        ParseResult semi_res = parse(in, semi);
        free_combinator(semi);
        if (!semi_res.is_success) {
            if (semi_res.value.error != NULL) {
                free_error(semi_res.value.error);
            }
            break;
        }
        free_ast(semi_res.value.ast);

        if (case_branch_should_stop(in, clargs->case_label_list)) {
            restore_input_state(in, &semi_state);
            break;
        }
    }

    if (stmt_count == 0) {
        return make_success(ast_nil);
    }

    if (stmt_count == 1) {
        return make_success(first);
    }

    ast_t* block = new_ast();
    block->typ = PASCAL_T_BEGIN_BLOCK;
    block->child = first;
    block->next = NULL;
    block->line = first != NULL ? first->line : in->line;
    block->col = first != NULL ? first->col : in->col;
    return make_success(block);
}

static combinator_t* case_branch_stmt_list(combinator_t** stmt_parser, combinator_t* case_label_list) {
    case_stmt_list_args* args = (case_stmt_list_args*)safe_malloc(sizeof(case_stmt_list_args));
    args->stmt_parser = stmt_parser;
    args->case_label_list = case_label_list;
    combinator_t* comb = new_combinator();
    comb->fn = case_branch_stmt_list_fn;
    comb->args = args;
    comb->name = strdup("case_branch_stmt_list");
    return comb;
}

static bool next_non_layout_is_comma(input_t* in) {
    if (in == NULL || in->buffer == NULL)
        return false;
    int length = in->length > 0 ? in->length : (int)strlen(in->buffer);
    int pos = skip_pascal_layout_preview(in, in->start);
    return (pos < length && in->buffer[pos] == ',');
}

static ParseResult with_context_comma_guard_fn(input_t* in, void* args, char* parser_name) {
    (void)args;
    if (next_non_layout_is_comma(in)) {
        return make_failure_v2(in, parser_name,
            strdup("Expected expression in WITH context list"), NULL);
    }
    return make_success(ast_nil);
}

static combinator_t* with_context_comma_guard(void) {
    combinator_t* comb = new_combinator();
    comb->type = COMB_EXPECT;
    comb->fn = with_context_comma_guard_fn;
    comb->args = NULL;
    comb->name = strdup("with_context_comma_guard");
    return comb;
}

static bool case_call_name_allowed(const char* name) {
    if (name == NULL) {
        return false;
    }
    return strcasecmp(name, "low") == 0 ||
           strcasecmp(name, "high") == 0 ||
           strcasecmp(name, "ord") == 0 ||
           strcasecmp(name, "chr") == 0 ||
           strcasecmp(name, "succ") == 0 ||
           strcasecmp(name, "pred") == 0 ||
           strcasecmp(name, "length") == 0;
}

static const char* case_call_name(ast_t* call_node) {
    if (call_node == NULL || call_node == ast_nil || call_node->typ != PASCAL_T_FUNC_CALL) {
        return NULL;
    }
    ast_t* base = call_node->child;
    if (base == NULL || base == ast_nil) {
        return NULL;
    }
    if (base->typ == PASCAL_T_IDENTIFIER && base->sym != NULL) {
        return base->sym->name;
    }
    return NULL;
}

static bool case_label_has_disallowed_call(ast_t* node) {
    if (node == NULL || node == ast_nil) {
        return false;
    }
    if (node->typ == PASCAL_T_FUNC_CALL) {
        const char* name = case_call_name(node);
        if (!case_call_name_allowed(name)) {
            return true;
        }
        // Allowed function call: still validate argument expressions.
        ast_t* base = node->child;
        ast_t* arg = base ? base->next : NULL;
        while (arg != NULL && arg != ast_nil) {
            if (case_label_has_disallowed_call(arg)) {
                return true;
            }
            arg = arg->next;
        }
        return false;
    }
    if (case_label_has_disallowed_call(node->child)) {
        return true;
    }
    return case_label_has_disallowed_call(node->next);
}

static ParseResult case_label_guard_fn(input_t* in, void* args, char* parser_name) {
    combinator_t* inner = (combinator_t*)args;
    if (inner == NULL) {
        return make_failure_v2(in, parser_name, strdup("Internal case label guard error"), NULL);
    }
    int start = in->start;
    ParseResult res = parse(in, inner);
    if (!res.is_success) {
        return res;
    }
    if (case_label_has_disallowed_call(res.value.ast)) {
        free_ast(res.value.ast);
        in->start = start;
        return make_failure_v2(in, parser_name,
            strdup("Function calls are not valid case labels"), NULL);
    }
    return res;
}

static combinator_t* case_label_guard(combinator_t* inner) {
    combinator_t* comb = new_combinator();
    comb->type = COMB_LABEL_GUARD;
    comb->fn = case_label_guard_fn;
    comb->args = inner;
    comb->name = strdup("case_label_guard");
    return comb;
}

static ast_t* wrap_with_contexts(ast_t* contexts) {
    if (contexts == NULL || contexts == ast_nil) {
        return ast_nil;
    }
    return ast1(PASCAL_T_WITH_CONTEXTS, contexts);
}

// Utility to drop an AST node entirely.
static ast_t* discard_ast_stmt(ast_t* ast) {
    if (ast != NULL && ast != ast_nil) {
        free_ast(ast);
    }
    return ast_nil;
}

static combinator_t* make_case_expression(combinator_t** expr_parser) {
    // Note: boolean literals (true/false) must come BEFORE cident to avoid being
    // parsed as identifiers.
    combinator_t* case_func_call = seq(new_combinator(), PASCAL_T_FUNC_CALL,
        pascal_qualified_identifier(PASCAL_T_IDENTIFIER),
        between(token(match("(")), token(match(")")),
            optional(sep_by(lazy(expr_parser), token(match(","))))
        ),
        NULL
    );
    combinator_t* const_expr_factor = multi(new_combinator(), PASCAL_T_NONE,
        hex_integer(PASCAL_T_INTEGER),
        binary_integer(PASCAL_T_INTEGER),
        octal_integer(PASCAL_T_INTEGER),
        integer(PASCAL_T_INTEGER),
        char_literal(PASCAL_T_CHAR),
        control_char_literal(PASCAL_T_CHAR),
        char_code_literal(PASCAL_T_CHAR_CODE),
        token(create_keyword_parser("true", PASCAL_T_BOOLEAN)),   // Boolean true
        token(create_keyword_parser("false", PASCAL_T_BOOLEAN)),  // Boolean false
        case_func_call,
        pascal_qualified_identifier(PASCAL_T_IDENTIFIER),  // supports dotted identifiers like THorzRectAlign.Left
        between(token(match("(")), token(match(")")), lazy(expr_parser)), // parenthesized expressions
        NULL);

    // Allow simple arithmetic in case labels like (CONST + 1) or -5
    return multi(new_combinator(), PASCAL_T_NONE,
        seq(new_combinator(), PASCAL_T_NEG,
            token(match("-")),
            const_expr_factor,
            NULL),
        seq(new_combinator(), PASCAL_T_POS,
            token(match("+")),
            const_expr_factor,
            NULL),
        const_expr_factor,
        NULL);
}

static bool slice_matches_keyword_ci(const char* slice, size_t len, const char* keyword) {
    if (slice == NULL || keyword == NULL) {
        return false;
    }
    size_t key_len = strlen(keyword);
    if (key_len != len) {
        return false;
    }
    return strncasecmp(slice, keyword, len) == 0;
}

static bool is_reserved_keyword_slice(const char* slice, size_t len) {
    if (slice == NULL || len == 0) {
        return false;
    }
    for (int i = 0; pascal_reserved_keywords[i] != NULL; ++i) {
        const char* keyword = pascal_reserved_keywords[i];
        if (slice_matches_keyword_ci(slice, len, keyword)) {
            return true;
        }
    }
    return false;
}

static bool is_statement_boundary_token(input_t* in) {
    if (in == NULL || in->buffer == NULL) {
        return false;
    }

    const char* buffer = in->buffer;
    int length = in->length > 0 ? in->length : (int)strlen(buffer);
    int pos = skip_pascal_layout_preview(in, in->start);
    if (pos >= length) {
        return true; // EOF naturally terminates a statement
    }

    unsigned char ch = (unsigned char)buffer[pos];
    if (ch == ';' || ch == '.') {
        return true;
    }
    if (!isalpha(ch)) {
        return false;
    }

    int cursor = pos;
    while (cursor < length && (isalnum((unsigned char)buffer[cursor]) || buffer[cursor] == '_')) {
        cursor++;
    }

    size_t word_len = (size_t)(cursor - pos);
    if (word_len == 0) {
        return false;
    }

    return (word_len == 3 && strncasecmp(buffer + pos, "end", 3) == 0) ||
           (word_len == 4 && strncasecmp(buffer + pos, "else", 4) == 0) ||
           (word_len == 5 && strncasecmp(buffer + pos, "until", 5) == 0) ||
           (word_len == 6 && strncasecmp(buffer + pos, "except", 6) == 0) ||
           (word_len == 7 && strncasecmp(buffer + pos, "finally", 7) == 0);
}

static ast_t* make_empty_statement_node(input_t* in) {
    ast_t* node = new_ast();
    if (node == NULL) {
        return ast_nil;
    }
    node->typ = PASCAL_T_STATEMENT;
    node->child = NULL;
    node->next = NULL;
    set_ast_position(node, in);
    return node;
}

static ParseResult empty_statement_fn(input_t* in, void* args, char* parser_name) {
    (void)args;
    if (in == NULL) {
        return make_failure_v2(NULL, parser_name, strdup("Empty statement parser requires input"), NULL);
    }
    if (!is_statement_boundary_token(in)) {
        return make_failure_v2(in, parser_name, strdup("Empty statement can only appear before boundary tokens (end, else, until, except, finally)"), NULL);
    }
    return make_success(make_empty_statement_node(in));
}

static ParseResult statement_dispatch_fn(input_t* in, void* args, char* parser_name) {
    (void)parser_name;
    statement_dispatch_args_t* dispatch = (statement_dispatch_args_t*)args;
    if (dispatch == NULL) {
        return make_failure(in, strdup("statement dispatcher misconfigured"));
    }
    if (in == NULL || in->buffer == NULL) {
        return make_failure(in, strdup("statement dispatcher missing input"));
    }

    const char* buffer = in->buffer;
    int length = in->length > 0 ? in->length : (int)strlen(buffer);
    if (length <= 0) {
        return make_failure_v2(in, parser_name, strdup("Empty input while parsing statement"), NULL);
    }

    int pos = skip_pascal_layout_preview(in, in->start);
    if (pos >= length) {
        return make_failure_v2(in, parser_name, strdup("Unexpected end of input while parsing statement"), NULL);
    }

    const char* slice = buffer + pos;
    unsigned char ch = (unsigned char)*slice;
    bool starts_identifier = (ch == '_' || isalpha(ch));
    bool starts_digit = isdigit(ch);

    if (starts_identifier) {
        int cursor = pos + 1;
        while (cursor < length) {
            unsigned char next = (unsigned char)buffer[cursor];
            if (!(isalnum(next) || next == '_')) {
                break;
            }
            cursor++;
        }
        size_t ident_len = (size_t)(cursor - pos);
        size_t buffer_len = ident_len + 1;
        char stack_buf[32];
        char* keyword_buf = stack_buf;
        bool heap_keyword = false;
        if (buffer_len > sizeof(stack_buf)) {
            keyword_buf = (char*)safe_malloc(buffer_len);
            heap_keyword = true;
        }
        memcpy(keyword_buf, slice, ident_len);
        keyword_buf[ident_len] = '\0';
        
        // TRACE dispatch
        if (getenv("KGPC_DEBUG_TRACE") != NULL) {
             FILE* f = fopen("/tmp/parser_trace.log", "a");
             if (f) { fprintf(f, "TRACE: stmt_dispatch seeing identifier '%s' at %d\n", keyword_buf, in ? in->line : 0); fclose(f); }
        }

        if (getenv("KGPC_DEBUG_STATEMENT_DISPATCH") != NULL) {
            // fprintf(stderr, "[statement_dispatch] leading identifier '%s'\n", keyword_buf);
        }

        if (getenv("KGPC_DEBUG_TRACE") != NULL) {
             FILE* f = fopen("/tmp/parser_trace.log", "a");
             if (f) { fprintf(f, "TRACE: stmt_dispatch seeing identifier '%s' at %d\n", keyword_buf, in ? in->line : 0); fclose(f); }
        }

        if (getenv("KGPC_DEBUG_STATEMENT_DISPATCH") != NULL) {
            fprintf(stderr, "[statement_dispatch] leading identifier '%s'\n", keyword_buf);
        }
        for (size_t i = 0; i < ident_len; ++i) {
            keyword_buf[i] = (char)tolower((unsigned char)keyword_buf[i]);
        }
        const struct statement_keyword_record* keyword_record = statement_keyword_lookup(keyword_buf, ident_len);
        
        // Manual fallback for 'continue' which collides with 'asm' in the perfect hash
        static const struct statement_keyword_record continue_record = {"continue", STMT_KW_CONTINUE};
        if (keyword_record == NULL && ident_len == 8 && strcmp(keyword_buf, "continue") == 0) {
            keyword_record = &continue_record;
        }
        bool reserved_keyword = is_reserved_keyword_slice(slice, ident_len);
        bool keyword_allowed_as_expr = pascal_keyword_allowed_in_expression(keyword_buf);
        if (heap_keyword) free(keyword_buf);
        if (keyword_record != NULL &&
            dispatch->keyword_parsers != NULL &&
            (size_t)keyword_record->id < dispatch->keyword_count) {
            combinator_t* keyword_parser = dispatch->keyword_parsers[keyword_record->id];
            if (keyword_parser != NULL) {
                return parse(in, keyword_parser);
            }
        }

        if (slice_matches_keyword_ci(slice, ident_len, "on") && dispatch->on_handler_parser != NULL) {
            return parse(in, dispatch->on_handler_parser);
        }

        if (dispatch->label_parser != NULL && peek_label_statement(in)) {
            return parse(in, dispatch->label_parser);
        }

        if (reserved_keyword && !keyword_allowed_as_expr) {
            return make_failure_v2(in, parser_name, strdup("Reserved keyword cannot start a statement here"), NULL);
        }

        // NOTE: Do NOT use speculative assignment parsing here (trying assignment even without `:=`).
        // This was attempted in commit fe74623 but caused FPC RTL regressions where constructs like
        // strlen(@array[0]) in bunxsysc.inc would be parsed incorrectly.
        if (dispatch->assignment_parser != NULL && peek_assignment_operator(in)) {
            return parse(in, dispatch->assignment_parser);
        }

        if (dispatch->expr_parser != NULL) {
            return parse(in, dispatch->expr_parser);
        }

        return make_failure_v2(in, parser_name, strdup("Unable to dispatch identifier-led statement"), NULL);
    }

    if (starts_digit) {
        if (dispatch->label_parser != NULL && peek_label_statement(in)) {
            return parse(in, dispatch->label_parser);
        }
        if (dispatch->expr_parser != NULL) {
            return parse(in, dispatch->expr_parser);
        }
        return make_failure_v2(in, parser_name, strdup("Unable to dispatch numeric-led statement"), NULL);
    }

    if (dispatch->expr_parser != NULL) {
        return parse(in, dispatch->expr_parser);
    }

    return make_failure_v2(in, parser_name, strdup("No matching statement parser"), NULL);
}

static ParseResult for_init_dispatch_fn(input_t* in, void* args, char* parser_name) {
    (void)parser_name;
    for_init_dispatch_args_t* dispatch = (for_init_dispatch_args_t*)args;
    if (dispatch == NULL) {
        return make_failure(in, strdup("for-initializer dispatcher misconfigured"));
    }
    if (peek_assignment_operator(in)) {
        return parse(in, dispatch->assignment_parser);
    }
    return parse(in, dispatch->identifier_parser);
}

// ASM block body parser - uses proper until() combinator instead of manual scanning
static combinator_t* asm_body(tag_t tag) {
    return until(match("end"), tag);  // Use raw match instead of token to preserve whitespace
}

// Wrap a parsed "^" token into a dedicated dereference AST node so we can
// reuse the existing array/member lvalue parsers while supporting pointer
// suffixes.  This mirrors the helper used by the expression parser.
static ast_t* wrap_pointer_lvalue_suffix(ast_t* parsed) {
    if (parsed != NULL && parsed != ast_nil) {
        free_ast(parsed);
    }

    ast_t* node = new_ast();
    node->typ = PASCAL_T_DEREF;
    node->child = NULL;
    node->next = NULL;
    return node;
}

// Wrap a typecast followed by "^" into a dereference node with the typecast as child.
// This handles lvalues like PCardinal(P)^ := value;
static ast_t* wrap_typecast_deref_lvalue(ast_t* parsed) {
    if (parsed == NULL || parsed == ast_nil)
        return parsed;

    // parsed is a flat list: typecast_node -> deref_node (from ^) -> optional suffixes
    ast_t* typecast_node = parsed;
    ast_t* deref_node = typecast_node->next;

    if (deref_node == NULL || deref_node == ast_nil)
        return parsed;  // No deref, shouldn't happen but be safe

    // Detach typecast from the chain
    typecast_node->next = NULL;

    // Get any additional suffixes after the deref
    ast_t* additional_suffixes = deref_node->next;
    deref_node->next = NULL;

    // Set the typecast as child of the deref
    deref_node->child = typecast_node;

    // Now chain any additional suffixes by passing through build_pointer_lvalue_chain
    if (additional_suffixes != NULL && additional_suffixes != ast_nil) {
        // Reconnect: deref_node -> additional_suffixes
        deref_node->next = additional_suffixes;
        return build_pointer_lvalue_chain(deref_node);
    }

    return deref_node;
}

static ast_t* wrap_array_lvalue_suffix(ast_t* parsed) {
    ast_t* node = new_ast();
    node->typ = PASCAL_T_ARRAY_ACCESS;
    node->child = (parsed == ast_nil) ? NULL : parsed;
    node->next = NULL;
    return node;
}

static combinator_t* pascal_label_identifier(void) {
    return multi(new_combinator(), PASCAL_T_NONE,
        token(integer(PASCAL_T_INTEGER)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
}

static ast_t* build_label_statement_ast(ast_t* parsed) {
    if (parsed == NULL || parsed == ast_nil)
        return ast_nil;

    ast_t* label_node = parsed;
    ast_t* stmt_node = NULL;

    if (label_node == ast_nil)
        label_node = NULL;

    if (label_node != NULL) {
        stmt_node = label_node->next;
        label_node->next = NULL;
    }

    if (stmt_node == ast_nil)
        stmt_node = NULL;
    if (stmt_node != NULL)
        stmt_node->next = NULL;

    ast_t* node = new_ast();
    node->typ = PASCAL_T_LABEL_STMT;
    node->child = label_node;
    node->next = NULL;
    node->line = (label_node != NULL) ? label_node->line : (stmt_node != NULL ? stmt_node->line : 0);
    node->col = (label_node != NULL) ? label_node->col : (stmt_node != NULL ? stmt_node->col : 0);

    if (label_node != NULL)
        label_node->next = stmt_node;
    else
        node->child = stmt_node;

    return node;
}

static ast_t* build_goto_ast(ast_t* parsed) {
    if (parsed == NULL || parsed == ast_nil)
        return ast_nil;

    ast_t* label_node = parsed;
    if (label_node == ast_nil)
        label_node = NULL;

    ast_t* trailing = NULL;
    if (label_node != NULL) {
        trailing = label_node->next;
        label_node->next = NULL;
    }

    if (trailing != NULL)
        free_ast(trailing);

    ast_t* node = new_ast();
    node->typ = PASCAL_T_GOTO_STMT;
    node->child = label_node;
    node->next = NULL;
    node->line = (label_node != NULL) ? label_node->line : 0;
    node->col = (label_node != NULL) ? label_node->col : 0;

    return node;
}

static ParseResult member_suffix_fn(input_t* in, void* args, char* parser_name) {
    InputState state;
    save_input_state(in, &state);

    combinator_t* dot = token(match("."));
    ParseResult dot_res = parse(in, dot);
    free_combinator(dot);
    if (!dot_res.is_success) {
        if (dot_res.value.error != NULL) {
            free_error(dot_res.value.error);
        }
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected '.' in member access"), NULL);
    }
    free_ast(dot_res.value.ast);

    combinator_t* identifier = token(pascal_expression_identifier(PASCAL_T_IDENTIFIER));
    ParseResult ident_res = parse(in, identifier);
    free_combinator(identifier);
    if (!ident_res.is_success) {
        if (ident_res.value.error != NULL) {
            free_error(ident_res.value.error);
        }
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected identifier after '.'"), NULL);
    }

    ast_t* ident_ast = ident_res.value.ast;
    ast_t* node = new_ast();
    node->typ = PASCAL_T_MEMBER_ACCESS;
    node->child = (ident_ast == ast_nil) ? NULL : ident_ast;
    node->next = NULL;
    set_ast_position(node, in);

    return make_success(node);
}

// Build a nested chain by attaching each parsed suffix to the base lvalue
// expression.  If there are no suffixes the original base node is returned
// unchanged.
static ast_t* build_pointer_lvalue_chain(ast_t* parsed) {
    if (parsed == NULL || parsed == ast_nil)
        return parsed;

    ast_t* base = parsed;
    ast_t* suffix = base->next;
    base->next = NULL;

    if (suffix == ast_nil)
        suffix = NULL;

    ast_t* current = base;
    while (suffix != NULL) {
        ast_t* next_suffix = suffix->next;
        if (next_suffix == ast_nil)
            next_suffix = NULL;

        suffix->next = NULL;

        switch (suffix->typ) {
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
            case PASCAL_T_MEMBER_ACCESS: {
                ast_t* field = suffix->child;
                suffix->child = current;

                if (field == ast_nil) {
                    field = NULL;
                }

                if (field != NULL) {
                    ast_t* tail = suffix->child;
                    while (tail->next != NULL) {
                        tail = tail->next;
                    }
                    tail->next = field;
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

// Transform "x += y" style compound assignments into a regular assignment
// with the addition expression on the right-hand side.  This keeps the rest
// of the compiler unaware of the compound assignment syntax while still
// allowing us to accept programs that use it.
static ast_t* transform_plus_assignment(ast_t* assignment_ast) {
    if (assignment_ast == NULL || assignment_ast == ast_nil)
        return assignment_ast;

    ast_t* lhs = assignment_ast->child;
    if (lhs == NULL)
        return assignment_ast;

    ast_t* rhs = lhs->next;
    if (rhs == NULL)
        return assignment_ast;

    // Detach the right-hand side so copying the lhs does not duplicate it.
    lhs->next = NULL;

    ast_t* lhs_copy = copy_ast(lhs);
    if (lhs_copy == NULL || lhs_copy == ast_nil) {
        lhs->next = rhs;
        return assignment_ast;
    }

    lhs_copy->line = lhs->line;
    lhs_copy->col = lhs->col;

    ast_t* add_node = new_ast();
    add_node->typ = PASCAL_T_ADD;
    add_node->child = lhs_copy;
    add_node->next = NULL;
    add_node->line = assignment_ast->line;
    add_node->col = assignment_ast->col;

    lhs_copy->next = rhs;
    lhs->next = add_node;

    return assignment_ast;
}

// --- Pascal Statement Parser Implementation ---
void init_pascal_statement_parser(combinator_t** p) {
    // Create the main statement parser pointer for recursive references FIRST
    // This allows the expression parser to reference it via lazy()
    combinator_t** stmt_parser = p;
    
    // Create the expression parser and pass the statement parser to it
    combinator_t** expr_parser = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *expr_parser = new_combinator();
    init_pascal_expression_parser(expr_parser, stmt_parser);

    combinator_t* empty_statement = new_combinator();
    empty_statement->fn = empty_statement_fn;
    empty_statement->name = strdup("empty_statement");

    combinator_t* stmt_or_empty = multi(new_combinator(), PASCAL_T_NONE,
        lazy(stmt_parser),
        empty_statement,
        NULL
    );

    // Left-value parser: base identifier with optional pointer, array, or member suffixes.
    combinator_t* simple_identifier = token(pascal_expression_identifier(PASCAL_T_IDENTIFIER));

    combinator_t* pointer_suffix = map(token(match("^")), wrap_pointer_lvalue_suffix);
    combinator_t* array_indices = between(
        token(match("[")),
        token(match("]")),
        sep_by(lazy(expr_parser), token(match(",")))
    );
    combinator_t* array_suffix = map(array_indices, wrap_array_lvalue_suffix);
    combinator_t* member_suffix = new_combinator();
    member_suffix->fn = member_suffix_fn;

    combinator_t* suffix_choice = multi(new_combinator(), PASCAL_T_NONE,
        array_suffix,
        pointer_suffix,
        member_suffix,
        NULL
    );
    combinator_t* suffixes = many(suffix_choice);
    combinator_t* required_suffixes = seq(new_combinator(), PASCAL_T_NONE,
        suffix_choice,
        suffixes,
        NULL
    );

    // Simple typecast lvalue without suffixes: Integer(x) := 42
    // Uses type_name for built-in types only (safer, avoids matching function calls)
    combinator_t* typecast_lvalue_simple = seq(new_combinator(), PASCAL_T_TYPECAST,
        token(type_name(PASCAL_T_IDENTIFIER)),
        between(token(match("(")), token(match(")")), lazy(expr_parser)),
        NULL
    );

    // Typecast lvalue with required pointer suffix: PCardinal(@x)^ := 42
    // This requires at least one suffix starting with '^' to be a valid lvalue.
    // Uses cident to allow any type name (including user-defined pointer types).
    // The required '^' suffix ensures function calls like strlen(@x) don't match.
    combinator_t* typecast_base = seq(new_combinator(), PASCAL_T_TYPECAST,
        token(cident(PASCAL_T_IDENTIFIER)),
        between(token(match("(")), token(match(")")), lazy(expr_parser)),
        NULL
    );

    // specialize TypeName<T>(expr) typecast lvalues
    combinator_t* lvalue_type_arg = token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER));
    combinator_t* lvalue_type_arg_list = seq(new_combinator(), PASCAL_T_TYPE_ARG_LIST,
        token(match("<")),
        sep_by1(lvalue_type_arg, token(match(","))),
        token(match(">")),
        NULL
    );
    combinator_t* specialize_type_base = seq(new_combinator(), PASCAL_T_CONSTRUCTED_TYPE,
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        lvalue_type_arg_list,
        NULL
    );
    combinator_t* specialize_typecast_base = seq(new_combinator(), PASCAL_T_TYPECAST,
        token(keyword_ci("specialize")),
        specialize_type_base,
        between(token(match("(")), token(match(")")), lazy(expr_parser)),
        NULL
    );
    // Require pointer suffix followed by optional additional suffixes
    combinator_t* required_pointer_suffix = seq(new_combinator(), PASCAL_T_NONE,
        pointer_suffix,
        suffixes,
        NULL
    );
    combinator_t* typecast_lvalue_with_deref = map(seq(new_combinator(), PASCAL_T_NONE,
        typecast_base,
        required_pointer_suffix,
        NULL
    ), wrap_typecast_deref_lvalue);

    // Typecast lvalue with member/array/pointer suffix: TFoo(obj).Field := 1
    combinator_t* typecast_lvalue_with_suffixes = map(seq(new_combinator(), PASCAL_T_NONE,
        typecast_base,
        required_suffixes,
        NULL
    ), build_pointer_lvalue_chain);

    combinator_t* specialize_lvalue_simple = seq(new_combinator(), PASCAL_T_TYPECAST,
        token(keyword_ci("specialize")),
        specialize_type_base,
        between(token(match("(")), token(match(")")), lazy(expr_parser)),
        NULL
    );

    combinator_t* specialize_lvalue_with_suffixes = map(seq(new_combinator(), PASCAL_T_NONE,
        specialize_typecast_base,
        required_suffixes,
        NULL
    ), build_pointer_lvalue_chain);

    combinator_t* simple_lvalue = map(seq(new_combinator(), PASCAL_T_NONE,
        simple_identifier,
        suffixes,
        NULL
    ), build_pointer_lvalue_chain);

    // Function call lvalue: unaligned(PUint16(Dest)^) := 0
    // Some FPC intrinsics (unaligned, etc.) return references that can be assigned to.
    // Parse as identifier(expr) with optional suffixes.
    combinator_t* funcall_lvalue = map(seq(new_combinator(), PASCAL_T_NONE,
        typecast_base,               // identifier(expr) - reuses typecast_base
        suffixes,                    // optional suffixes after the call
        NULL
    ), build_pointer_lvalue_chain);

    combinator_t* lvalue = multi(new_combinator(), PASCAL_T_NONE,
        typecast_lvalue_with_deref,  // Try typecast with deref first (PCardinal(@x)^)
        typecast_lvalue_with_suffixes, // Then typecast with field/array/pointer access
        specialize_lvalue_with_suffixes, // specialize T<T>(x).Field := 1
        typecast_lvalue_simple,      // Then simple typecast (Integer(x))
        specialize_lvalue_simple,    // specialize T<T>(x) := value
        funcall_lvalue,              // Function call returning reference: unaligned(expr) := 0
        simple_lvalue,               // Finally simple identifier with optional suffixes
        NULL
    );

    // Assignment statement: support both ":=" and "+=" compound assignments
    combinator_t* simple_assignment = seq(new_combinator(), PASCAL_T_ASSIGNMENT,
        trace("Enter simple_assignment"),
        lvalue,                                // left-hand side (identifier or member access)
        trace("Matched lvalue"),
        token(match(":=")),                    // assignment operator
        trace("Matched :="),
        lazy(expr_parser),                     // expression
        trace("Matched expression"),
        NULL
    );

    combinator_t* plus_assignment_seq = seq(new_combinator(), PASCAL_T_ASSIGNMENT,
        lvalue,
        token(match("+=")),
        lazy(expr_parser),
        NULL
    );
    combinator_t* plus_assignment = map(plus_assignment_seq, transform_plus_assignment);

    combinator_t* assignment = multi(new_combinator(), PASCAL_T_NONE,
        plus_assignment,
        simple_assignment,
        NULL
    );

    // Goto statement: goto <label>
    combinator_t* goto_stmt = map(seq(new_combinator(), PASCAL_T_NONE,
        token(keyword_ci("goto")),
        pascal_label_identifier(),
        NULL
    ), build_goto_ast);

    // Labeled statement: <label>: statement
    combinator_t* labeled_stmt = map(seq(new_combinator(), PASCAL_T_NONE,
        pascal_label_identifier(),
        token(match(":")),
        stmt_or_empty,
        NULL
    ), build_label_statement_ast);

    // Simple expression statement: expression (no semicolon here)
    combinator_t* expr_stmt = seq(new_combinator(), PASCAL_T_STATEMENT,
        lazy(expr_parser),
        NULL
    );

    // Begin-end block: begin [statement_list] end
    // Simplified statement list parser - just use sep_by with optional trailing semicolon
    combinator_t* stmt_list = seq(new_combinator(), PASCAL_T_NONE,
        sep_by(lazy(stmt_parser), token(match(";"))),     // statements separated by semicolons
        optional(token(match(";"))),                      // optional trailing semicolon
        NULL
    );

    // Pascal allows empty statements represented by standalone semicolons.
    combinator_t* leading_semicolons = many(token(match(";")));

    // Try empty block first (begin end), then non-empty
    combinator_t* begin_end_block = multi(new_combinator(), PASCAL_T_NONE,
        seq(new_combinator(), PASCAL_T_BEGIN_BLOCK,
            token(keyword_ci("begin")),
            trace("Enter empty begin block"),
            token(keyword_ci("end")),
            trace("Exit empty begin block"),
            NULL
        ),
        seq(new_combinator(), PASCAL_T_BEGIN_BLOCK,
            token(keyword_ci("begin")),
            trace("Enter begin block"),
            leading_semicolons,
            stmt_list,
            trace("About to match end of begin block"),
            token(keyword_ci("end")),
            trace("Exit begin block"),
            NULL
        ),
        NULL
    );

    // If statement: if expression then statement [else statement]
    // Once we see "if", commit to parsing an if statement
    combinator_t* if_stmt = seq(new_combinator(), PASCAL_T_IF_STMT,
        token(keyword_ci("if")),                     // if keyword (case-insensitive)
        trace("Enter if_stmt"),
        commit(seq(new_combinator(), PASCAL_T_NONE,
            lazy(expr_parser),                         // condition
            token(keyword_ci("then")),                   // then keyword (case-insensitive)
            trace("Parsing if_then statement"),
            stmt_or_empty,                         // then statement
            optional(seq(new_combinator(), PASCAL_T_ELSE,    // optional else part
                token(keyword_ci("else")),               // else keyword (case-insensitive)
                stmt_or_empty,
                NULL
            )),
            trace("Exit if_stmt"),
            NULL
        )),
        NULL
    );

    // For-in statement: for identifier in expression do statement
    // Use nested seq() pattern like for_stmt for consistent AST structure
    combinator_t* for_in_stmt = seq(new_combinator(), PASCAL_T_FOR_IN_STMT,
        token(keyword_ci("for")),                // for keyword (case-insensitive)
        seq(new_combinator(), PASCAL_T_NONE,
            simple_identifier,                       // loop variable (identifier)
            token(keyword_ci("in")),                 // in keyword (case-insensitive)
            lazy(expr_parser),                       // collection expression
            token(keyword_ci("do")),                 // do keyword (case-insensitive)
            stmt_or_empty,                       // loop body statement
            NULL
        ),
        NULL
    );

    // For statement: for [identifier := expression | identifier] (to|downto) expression do statement
    combinator_t* for_direction = multi(new_combinator(), PASCAL_T_NONE,
        token(create_keyword_parser("to", PASCAL_T_TO)),                 // to keyword (case-insensitive)
        token(create_keyword_parser("downto", PASCAL_T_DOWNTO)),         // downto keyword (case-insensitive)
        NULL
    );
    for_init_dispatch_args_t* for_init_args = (for_init_dispatch_args_t*)safe_malloc(sizeof(for_init_dispatch_args_t));
    for_init_args->assignment_parser = assignment;
    for_init_args->identifier_parser = simple_identifier;
    combinator_t* for_initializer = new_combinator();
    for_initializer->type = COMB_FOR_INIT_DISPATCH;
    for_initializer->fn = for_init_dispatch_fn;
    for_initializer->args = for_init_args;
    combinator_t* for_stmt = seq(new_combinator(), PASCAL_T_FOR_STMT,
        token(keyword_ci("for")),                // for keyword (case-insensitive)
        commit(seq(new_combinator(), PASCAL_T_NONE,
            for_initializer,                        // loop initializer (assignment or bare identifier)
            for_direction,                          // to or downto
            lazy(expr_parser),                      // end expression
            token(keyword_ci("do")),                 // do keyword (case-insensitive)
            stmt_or_empty,                      // loop body statement
            NULL
        )),
        NULL
    );

    // Combined for statement parser (try for-in first, then regular for)
    combinator_t* any_for_stmt = multi(new_combinator(), PASCAL_T_NONE,
        for_in_stmt,
        for_stmt,
        NULL
    );

    // While statement: while expression do statement
    combinator_t* while_stmt = seq(new_combinator(), PASCAL_T_WHILE_STMT,
        token(keyword_ci("while")),              // while keyword (case-insensitive)
        trace("Enter while_stmt"),
        commit(seq(new_combinator(), PASCAL_T_NONE,
            lazy(expr_parser),                     // condition
            token(keyword_ci("do")),                 // do keyword (case-insensitive)
            stmt_or_empty,                     // body statement
            trace("Exit while_stmt"),
            NULL
        )),
        NULL
    );

    // Repeat statement: repeat statement_list until expression
    combinator_t* repeat_stmt_list = seq(new_combinator(), PASCAL_T_STATEMENT_LIST,
        sep_by(lazy(stmt_parser), token(match(";"))),    // statements separated by semicolons
        optional(token(match(";"))),                     // optional trailing semicolon
        NULL
    );

    combinator_t* repeat_stmt = seq(new_combinator(), PASCAL_T_REPEAT_STMT,
        token(keyword_ci("repeat")),           // repeat keyword (case-insensitive)
        trace("Enter repeat_stmt"),
        commit(seq(new_combinator(), PASCAL_T_NONE,
            repeat_stmt_list,                      // repeated statements
            trace("About to match until"),
            token(keyword_ci("until")),           // until keyword (case-insensitive)
            lazy(expr_parser),                     // termination expression
            trace("Exit repeat_stmt"),
            NULL
        )),
        NULL
    );

    // With statement: with expression[, expression...] do statement
    combinator_t* with_additional_context = seq(new_combinator(), PASCAL_T_NONE,
        token(match(",")),
        commit(lazy(expr_parser)),
        NULL
    );
    combinator_t* with_context_sequence = seq(new_combinator(), PASCAL_T_NONE,
        lazy(expr_parser),
        many(with_additional_context),
        NULL
    );
    combinator_t* with_contexts = map(with_context_sequence, wrap_with_contexts);

    combinator_t* with_stmt = seq(new_combinator(), PASCAL_T_WITH_STMT,
        token(keyword_ci("with")),               // with keyword (case-insensitive)
        commit(seq(new_combinator(), PASCAL_T_NONE,
            commit(with_context_comma_guard()),
            with_contexts,                        // one or more context expressions
            commit(with_context_comma_guard()),
            token(keyword_ci("do")),                 // do keyword (case-insensitive)
            stmt_or_empty,                     // body statement
            NULL
        )),
        NULL
    );

    // ASM block: asm ... end [reglist]
    combinator_t* asm_reg = multi(new_combinator(), PASCAL_T_NONE,
        token(pascal_string(PASCAL_T_STRING)),
        token(cident(PASCAL_T_IDENTIFIER)),
        NULL
    );
    combinator_t* asm_reglist = optional(between(
        token(match("[")),
        token(match("]")),
        sep_by(asm_reg, token(match(",")))
    ));
    combinator_t* asm_stmt = seq(new_combinator(), PASCAL_T_ASM_BLOCK,
        token(match("asm")),                   // asm keyword
        commit(seq(new_combinator(), PASCAL_T_NONE,
            asm_body(PASCAL_T_NONE),               // asm body content
            token(match("end")),                   // end keyword
            asm_reglist,                           // optional register list
            NULL
        )),
        NULL
    );

    // Shared helper: list of statements allowing optional semicolons between entries
    combinator_t* try_statement_list = many(seq(new_combinator(), PASCAL_T_NONE,
        pnot(peek(keyword_ci("except"))),
        pnot(peek(keyword_ci("finally"))),
        lazy(stmt_parser),
        optional(token(match(";"))),
        NULL
    ));

    // Try-finally block: try statements finally statements end
    // Wrap finally portion in a dedicated node so the converter can distinguish it.
    combinator_t* try_finally = seq(new_combinator(), PASCAL_T_TRY_BLOCK,
        token(keyword_ci("try")),
        try_statement_list,
        seq(new_combinator(), PASCAL_T_FINALLY_BLOCK,
            token(keyword_ci("finally")),
            many(seq(new_combinator(), PASCAL_T_NONE,
                stmt_or_empty,
                optional(token(match(";"))),
                NULL
            )),
            NULL
        ),
        token(keyword_ci("end")),
        NULL
    );

    // On-exception handler: "on <id>[:<type>] do <statement>"
    // Parse the variable name and optional type specification
    combinator_t* exception_var = token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER));
    combinator_t* exception_type_spec = optional(seq(new_combinator(), PASCAL_T_NONE,
        token(match(":")),
        token(pascal_qualified_identifier(PASCAL_T_IDENTIFIER)),
        NULL
    ));
    
    combinator_t* optional_handler_semicolon = map(optional(token(match(";"))), discard_ast_stmt);
    combinator_t* on_exception_handler = seq(new_combinator(), PASCAL_T_ON_CLAUSE,
        token(keyword_ci("on")),
        exception_var,
        exception_type_spec,
        token(keyword_ci("do")),
        stmt_or_empty,
        optional_handler_semicolon,
        NULL
    );

    // Try-except block: try statements except statements end
    combinator_t* except_item = seq(new_combinator(), PASCAL_T_NONE,
        pnot(peek(keyword_ci("else"))),
        multi(new_combinator(), PASCAL_T_NONE,
            on_exception_handler,
            seq(new_combinator(), PASCAL_T_NONE,
                stmt_or_empty,
                optional(token(match(";"))),
                NULL
            ),
            NULL
        ),
        NULL
    );

    combinator_t* try_except = seq(new_combinator(), PASCAL_T_TRY_BLOCK,
        token(keyword_ci("try")),
        try_statement_list,
        seq(new_combinator(), PASCAL_T_EXCEPT_BLOCK,
            token(keyword_ci("except")),
            many(except_item),
            optional(seq(new_combinator(), PASCAL_T_NONE,
                token(keyword_ci("else")),
                stmt_or_empty,
                optional(token(match(";"))),
                NULL
            )),
            NULL
        ),
        token(keyword_ci("end")),
        NULL
    );

    // Raise statement: raise [expression] [at addr[, frame]]
    combinator_t* raise_stmt = seq(new_combinator(), PASCAL_T_RAISE_STMT,
        token(keyword_ci("raise")),            // raise keyword (case-insensitive)
        optional(lazy(expr_parser)),           // optional exception expression
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(keyword_ci("at")),
            lazy(expr_parser),
            optional(seq(new_combinator(), PASCAL_T_NONE,
                token(match(",")),
                lazy(expr_parser),
                NULL
            )),
            NULL
        )),
        NULL
    );

    // Inherited statement: inherited [method_call]
    combinator_t* inherited_stmt = seq(new_combinator(), PASCAL_T_INHERITED_STMT,
        token(keyword_ci("inherited")),        // inherited keyword (case-insensitive)
        optional(lazy(expr_parser)),           // optional method call expression (e.g., inherited Destroy)
        NULL
    );

    // Exit statement: exit; or exit(expression);
    combinator_t* exit_stmt = seq(new_combinator(), PASCAL_T_EXIT_STMT,
        token(keyword_ci("exit")),
        optional(seq(new_combinator(), PASCAL_T_NONE,
            token(match("(")),
            lazy(expr_parser),
            token(match(")")),
            NULL
        )),
        NULL
    );

    // Break statement: break
    combinator_t* break_stmt = token(create_keyword_parser("break", PASCAL_T_BREAK_STMT));

    // Continue statement: continue
    combinator_t* continue_stmt = token(create_keyword_parser("continue", PASCAL_T_CONTINUE_STMT));

    // Case statement: case expression of label1: stmt1; label2: stmt2; [else stmt;] end
    // Case labels should handle constant expressions, not just simple values
    
    combinator_t* case_expression_start = make_case_expression(expr_parser);
    combinator_t* case_expression_end = make_case_expression(expr_parser);
    combinator_t* case_expression_single = make_case_expression(expr_parser);

    // Range case label: expression..expression
    combinator_t* range_case_label = seq(new_combinator(), PASCAL_T_RANGE,
        case_label_guard(case_expression_start),
        token(match("..")),
        case_label_guard(case_expression_end),
        NULL);
    
    combinator_t* case_label = multi(new_combinator(), PASCAL_T_CASE_LABEL,
        token(range_case_label),    // Try range first
        token(case_label_guard(case_expression_single)),     // Then single expressions
        NULL
    );
    
    combinator_t* case_label_list = seq(new_combinator(), PASCAL_T_CASE_LABEL_LIST,
        sep_by(case_label, token(match(","))), // labels separated by commas
        NULL
    );
    
    combinator_t* case_branch_body = case_branch_stmt_list(stmt_parser, case_label_list);
    combinator_t* case_branch = seq(new_combinator(), PASCAL_T_CASE_BRANCH,
        case_label_list,                       // case labels
        token(match(":")),                     // colon
        case_branch_body,                      // statement or statement list
        NULL
    );
    
    combinator_t* case_stmt = seq(new_combinator(), PASCAL_T_CASE_STMT,
        token(keyword_ci("case")),             // case keyword
        commit(seq(new_combinator(), PASCAL_T_NONE,
            lazy(expr_parser),                     // case expression
            token(keyword_ci("of")),               // of keyword
            sep_end_by(case_branch, token(match(";"))), // case branches with optional trailing semicolon
            optional(seq(new_combinator(), PASCAL_T_ELSE, // optional else clause
                token(keyword_ci("else")),         // else keyword
                case_branch_body,                  // else statement or list
                optional(token(match(";"))),      // optional semicolon after else block
                NULL
            )),
            token(keyword_ci("end")),              // end keyword
            NULL
        )),
        NULL
    );

    combinator_t* try_stmt = multi(new_combinator(), PASCAL_T_TRY_BLOCK,
        try_finally,
        try_except,
        NULL
    );

    statement_dispatch_args_t* dispatch_args = (statement_dispatch_args_t*)safe_malloc(sizeof(statement_dispatch_args_t));
    memset(dispatch_args, 0, sizeof(*dispatch_args));
    dispatch_args->keyword_count = STMT_KW_COUNT;
    dispatch_args->keyword_parsers = (combinator_t**)safe_malloc(sizeof(combinator_t*) * STMT_KW_COUNT);
    for (size_t i = 0; i < STMT_KW_COUNT; ++i) {
        dispatch_args->keyword_parsers[i] = NULL;
    }
    dispatch_args->keyword_parsers[STMT_KW_BEGIN] = begin_end_block;
    dispatch_args->keyword_parsers[STMT_KW_GOTO] = goto_stmt;
    dispatch_args->keyword_parsers[STMT_KW_TRY] = try_stmt;
    dispatch_args->keyword_parsers[STMT_KW_CASE] = case_stmt;
    dispatch_args->keyword_parsers[STMT_KW_RAISE] = raise_stmt;
    dispatch_args->keyword_parsers[STMT_KW_INHERITED] = inherited_stmt;
    dispatch_args->keyword_parsers[STMT_KW_BREAK] = break_stmt;
    dispatch_args->keyword_parsers[STMT_KW_CONTINUE] = continue_stmt;
    dispatch_args->keyword_parsers[STMT_KW_EXIT] = exit_stmt;
    dispatch_args->keyword_parsers[STMT_KW_ASM] = asm_stmt;
    dispatch_args->keyword_parsers[STMT_KW_IF] = if_stmt;
    dispatch_args->keyword_parsers[STMT_KW_FOR] = any_for_stmt;
    dispatch_args->keyword_parsers[STMT_KW_REPEAT] = repeat_stmt;
    dispatch_args->keyword_parsers[STMT_KW_WHILE] = while_stmt;
    dispatch_args->keyword_parsers[STMT_KW_WITH] = with_stmt;
    dispatch_args->on_handler_parser = on_exception_handler;
    dispatch_args->label_parser = labeled_stmt;
    dispatch_args->assignment_parser = assignment;
    dispatch_args->expr_parser = expr_stmt;

    combinator_t* stmt_dispatch = new_combinator();
    stmt_dispatch->type = COMB_STATEMENT_DISPATCH;
    stmt_dispatch->fn = statement_dispatch_fn;
    stmt_dispatch->args = dispatch_args;

    if (*stmt_parser != NULL && *stmt_parser != stmt_dispatch) {
        free_combinator(*stmt_parser);
    }
    *stmt_parser = stmt_dispatch;
    (*p)->extra_to_free = expr_parser;
}
