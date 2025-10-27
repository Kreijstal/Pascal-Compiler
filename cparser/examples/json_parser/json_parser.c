#include "json_parser.h"
#include "combinators.h"
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

// Predicate for whitespace
static bool is_whitespace(char c) {
    return isspace((unsigned char)c);
}

//=============================================================================
// Custom JSON-Specific Parser Implementations
//=============================================================================

// Forward declarations
static ParseResult number_fn(input_t* in, void* args, char* parser_name);
static ParseResult null_core_fn(input_t* in, void* args, char* parser_name);
static ParseResult bool_core_fn(input_t* in, void* args, char* parser_name);

combinator_t* number(tag_t tag);
combinator_t* json_null(tag_t tag);
combinator_t* json_bool(tag_t tag);
combinator_t* json_string(tag_t tag);


static ParseResult number_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);
    int start_pos = in->start;
    char c = read1(in);
    if (!isdigit(c) && c != '-') { restore_input_state(in, &state); return make_failure_v2(in, parser_name, strdup("Expected a number."), NULL); }
    if (c == '-') {
        c = read1(in);
        if (!isdigit(c)) { restore_input_state(in, &state); return make_failure_v2(in, parser_name, strdup("Expected a digit after minus."), NULL); }
    }
    // consume digits
    while (in->start < in->length && isdigit(in->buffer[in->start])) in->start++;
    // check for .
    if (in->start < in->length && in->buffer[in->start] == '.') {
        in->start++; // consume .
        if (in->start >= in->length || !isdigit(in->buffer[in->start])) { restore_input_state(in, &state); return make_failure_v2(in, parser_name, strdup("Invalid fractional part."), NULL); }
        while (in->start < in->length && isdigit(in->buffer[in->start])) in->start++;
    }
    // check for e or E
    if (in->start < in->length && (in->buffer[in->start] == 'e' || in->buffer[in->start] == 'E')) {
        in->start++; // consume e/E
        if (in->start < in->length && (in->buffer[in->start] == '+' || in->buffer[in->start] == '-')) in->start++; // consume +/-
        if (in->start >= in->length || !isdigit(in->buffer[in->start])) { restore_input_state(in, &state); return make_failure_v2(in, parser_name, strdup("Invalid exponent part."), NULL); }
        while (in->start < in->length && isdigit(in->buffer[in->start])) in->start++;
    }
    int len = in->start - start_pos;
    if (len == 0 || (len == 1 && in->buffer[start_pos] == '-')) { restore_input_state(in, &state); return make_failure_v2(in, parser_name, strdup("Invalid number."), NULL); }
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_pos, len);
    text[len] = '\0';
    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(text);
    free(text);
    return make_success(ast);
}

combinator_t* number(tag_t tag) {
    combinator_t* ws = many(satisfy(is_whitespace, JSON_T_NONE));
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* num = new_combinator();
    num->type = P_INTEGER;
    num->fn = number_fn;
    num->args = args;
    return right(ws, num);
}

static ParseResult null_core_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    ParseResult result = parse(in, match("null"));
    if (result.is_success) {
        free_ast(result.value.ast);
        ast_t* ast = new_ast();
        ast->typ = pargs->tag;
        return make_success(ast);
    }
    return result;
}

combinator_t* json_null(tag_t tag) {
    combinator_t* ws = many(satisfy(is_whitespace, JSON_T_NONE));
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* null_core = new_combinator();
    null_core->type = P_MATCH;
    null_core->fn = null_core_fn;
    null_core->args = args;
    return right(ws, null_core);
}

static ParseResult bool_core_fn(input_t* in, void* args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state;
    save_input_state(in, &state);
    ParseResult res_true = parse(in, match("true"));
    if (res_true.is_success) {
        free_ast(res_true.value.ast);
        ast_t* ast = new_ast();
        ast->typ = pargs->tag;
        ast->sym = sym_lookup("1");
        return make_success(ast);
    }
    free_error(res_true.value.error);
    restore_input_state(in, &state);
    ParseResult res_false = parse(in, match("false"));
    if (res_false.is_success) {
        free_ast(res_false.value.ast);
        ast_t* ast = new_ast();
        ast->typ = pargs->tag;
        ast->sym = sym_lookup("0");
        return make_success(ast);
    }
    return res_false;
}

combinator_t* json_bool(tag_t tag) {
    combinator_t* ws = many(satisfy(is_whitespace, JSON_T_NONE));
    prim_args* args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t* bool_core = new_combinator();
    bool_core->type = P_MATCH;
    bool_core->fn = bool_core_fn;
    bool_core->args = args;
    return right(ws, bool_core);
}

combinator_t* json_string(tag_t tag) {
    combinator_t* ws = many(satisfy(is_whitespace, JSON_T_NONE));
    return right(ws, string(tag));
}

//=============================================================================
// The Complete JSON Grammar
//=============================================================================

combinator_t* json_parser() {
    // The core recursive parser for any single JSON value.
    combinator_t** p_json_value = (combinator_t**)safe_malloc(sizeof(combinator_t*));
    *p_json_value = new_combinator();
    (*p_json_value)->extra_to_free = p_json_value;

    // The various types of JSON values
    combinator_t* j_string = json_string(JSON_T_STRING);
    combinator_t* j_number = number(JSON_T_INT);
    combinator_t* j_null = json_null(JSON_T_NONE);
    combinator_t* j_bool = json_bool(JSON_T_INT); // Using INT for bool

    // Recursive definitions for array and object, using new lazy proxies each time
    combinator_t* kv_pair = seq(new_combinator(), JSON_T_ASSIGN, json_string(JSON_T_STRING), expect(match(":"), "Expected ':'"), lazy(p_json_value), NULL);
    combinator_t* j_array = seq(new_combinator(), JSON_T_SEQ, match("["), sep_by(lazy(p_json_value), match(",")), expect(match("]"), "Expected ']'"), NULL);
    combinator_t* j_object = seq(new_combinator(), JSON_T_SEQ, match("{"), sep_by(kv_pair, match(",")), expect(match("}"), "Expected '}'"), NULL);

    // The main json_value parser is a `multi` choice between all possible types.
    multi(*p_json_value, JSON_T_NONE, j_string, j_number, j_null, j_bool, j_array, j_object, NULL);

    return *p_json_value;
}
