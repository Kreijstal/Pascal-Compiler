#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include "parser.h"
#include "combinator_internals.h"

//=============================================================================
// Internal Structs & Forward Declarations
//=============================================================================

// --- Argument Structs ---
typedef struct { char * str; } match_args;
typedef struct { combinator_t* delimiter; tag_t tag; } until_args;
typedef struct op_t { tag_t tag; combinator_t * comb; struct op_t * next; } op_t;
typedef struct expr_list { op_t * op; expr_fix fix; expr_assoc assoc; combinator_t * comb; struct expr_list * next; } expr_list;

// --- Static Function Forward Declarations ---
static ParseResult lazy_fn(input_t * in, void * args, char* parser_name);
static ParseResult match_fn(input_t * in, void * args, char* parser_name);
static ParseResult integer_fn(input_t * in, void * args, char* parser_name);
static ParseResult cident_fn(input_t * in, void * args, char* parser_name);
static ParseResult string_fn(input_t * in, void * args, char* parser_name);
static ParseResult until_fn(input_t * in, void * args, char* parser_name);
static ParseResult any_char_fn(input_t * in, void * args, char* parser_name);
static ParseResult satisfy_fn(input_t * in, void * args, char* parser_name);
static ParseResult expr_fn(input_t * in, void * args, char* parser_name);
static ast_t* ensure_ast_nil_initialized();


//=============================================================================
// GLOBAL STATE & HELPER FUNCTIONS
//=============================================================================

ast_t * ast_nil = NULL;

// --- Result & Error Helpers ---
ParseResult make_success(ast_t* ast) {
    return (ParseResult){ .is_success = true, .value.ast = ast };
}

ParseResult make_failure_v2(input_t* in, char* parser_name, char* message, char* unexpected) {
    ParseError* err = (ParseError*)safe_malloc(sizeof(ParseError));
    err->line = in->line;
    err->col = in->col;
    err->message = message;
    err->parser_name = parser_name ? strdup(parser_name) : NULL;
    err->unexpected = unexpected;
    err->cause = NULL;
    err->partial_ast = NULL;
    return (ParseResult){ .is_success = false, .value.error = err };
}

ParseResult make_failure(input_t* in, char* message) {
    return make_failure_v2(in, NULL, message, NULL);
}

ParseResult make_failure_with_ast(input_t* in, char* message, ast_t* partial_ast) {
    ParseError* err = (ParseError*)safe_malloc(sizeof(ParseError));
    err->line = in->line;
    err->col = in->col;
    err->message = message;
    err->cause = NULL;
    err->partial_ast = partial_ast;
    return (ParseResult){ .is_success = false, .value.error = err };
}

ParseResult wrap_failure_with_ast(input_t* in, char* message, ParseResult original_result, ast_t* partial_ast) {
    if (original_result.is_success) {
        return original_result;
    }
    
    // Validate input parameters
    if (original_result.value.error == NULL) {
        return make_failure(in, "Cannot wrap NULL error");
    }
    
    if (message == NULL) {
        return make_failure(in, "Cannot wrap with NULL message");
    }
    
    ParseError* original_error = original_result.value.error;
    ParseError* new_err = (ParseError*)safe_malloc(sizeof(ParseError));
    if (new_err == NULL) {
        return make_failure(in, "Memory allocation failed for error wrapper");
    }
    
    new_err->line = in->line;
    new_err->col = in->col;
    new_err->message = strdup(message);
    if (new_err->message == NULL) {
        free(new_err);
        return make_failure(in, "Memory allocation failed for error message");
    }
    new_err->cause = original_error;
    new_err->partial_ast = partial_ast;
    new_err->parser_name = NULL;
    new_err->unexpected = NULL;
    
    return (ParseResult){ .is_success = false, .value.error = new_err };
}

ParseResult wrap_failure(input_t* in, char* message, char* parser_name, ParseResult cause) {
    ParseError* err = (ParseError*)safe_malloc(sizeof(ParseError));
    err->line = in->line;
    err->col = in->col;
    err->message = message;
    err->cause = cause.value.error;
    err->partial_ast = NULL;
    err->parser_name = parser_name ? strdup(parser_name) : NULL;
    err->unexpected = NULL; // The unexpected token is now part of the message in expect_fn
    return (ParseResult){ .is_success = false, .value.error = err };
}

// --- Input State Management ---
void save_input_state(input_t* in, InputState* state) {
    state->start = in->start; state->line = in->line; state->col = in->col;
}

void restore_input_state(input_t* in, InputState* state) {
    in->start = state->start; in->line = state->line; in->col = state->col;
}

// --- Public Helpers ---
/* HARDENED: Changed exit(1) to abort() for immediate crash. */
void* safe_malloc(size_t size) {
    void* ptr = malloc(size);
    if (!ptr) {
        fprintf(stderr, "FATAL: safe_malloc failed to allocate %zu bytes at %s:%d\n", size, __FILE__, __LINE__);
        abort();
    }
    return ptr;
}

/* HARDENED: Changed exit(1) to abort() for immediate crash. */
void exception(const char * err) {
   fprintf(stderr, "FATAL: %s at %s:%d\n", err, __FILE__, __LINE__);
   abort();
}

ast_t * new_ast() {
    ast_t* ast = (ast_t *) safe_malloc(sizeof(ast_t));
    ast->typ = 0; // Default tag
    ast->child = NULL;
    ast->next = NULL;
    ast->sym = NULL;
    ast->line = 0;
    ast->col = 0;
    return ast;
}

// Set AST node position from current input state
void set_ast_position(ast_t* ast, input_t* in) {
    if (ast != NULL && in != NULL) {
        ast->line = in->line;
        ast->col = in->col;
    }
}

ast_t* ast1(tag_t typ, ast_t* a1) {
    ast_t* ast = new_ast();
    ast->typ = typ; ast->child = a1; ast->next = NULL;
    return ast;
}

ast_t* copy_ast(ast_t* orig) {
    if (orig == NULL) return NULL;
    if (orig == ensure_ast_nil_initialized()) return ensure_ast_nil_initialized();
    ast_t* new = new_ast();
    new->typ = orig->typ;
    new->sym = orig->sym ? sym_lookup(orig->sym->name) : NULL;
    new->child = copy_ast(orig->child);
    new->next = copy_ast(orig->next);
    return new;
}

ast_t* ast2(tag_t typ, ast_t* a1, ast_t* a2) {
    ast_t* ast = new_ast();
    ast->typ = typ; ast->child = a1; a1->next = a2; ast->next = NULL;
    return ast;
}

sym_t * sym_lookup(const char * name) {
   sym_t * sym = (sym_t *) safe_malloc(sizeof(sym_t));
   sym->name = (char *) safe_malloc(strlen(name) + 1);
   strcpy(sym->name, name);
   return sym;
}

input_t * new_input() {
    input_t * in = (input_t *) safe_malloc(sizeof(input_t));
    in->buffer = NULL; in->alloc = 0; in->length = 0; in->start = 0; in->line = 1; in->col = 1;
    return in;
}

// Initialize input buffer with proper line/column tracking
void init_input_buffer(input_t *in, char *buffer, int length) {
    in->buffer = buffer;
    in->length = length;
    in->start = 0;
    // Reset to beginning for parsing
    in->line = 1;
    in->col = 1;
}

char read1(input_t * in) {
    if (in->buffer == NULL) {
        char linebuf[2048];
        if (fgets(linebuf, sizeof(linebuf), stdin) == NULL) {
            in->length = 0; in->start = 0; return EOF;
        }
        in->length = strlen(linebuf);
        in->alloc = in->length + 1;
        in->buffer = (char*)safe_malloc(in->alloc);
        strcpy(in->buffer, linebuf);
        in->start = 0; in->line = 1; in->col = 1;
    }
    if (in->start < in->length) {
        char c = in->buffer[in->start++];
        if (c == '\n') { in->line++; in->col = 1; } else { in->col++; }
        return c;
    }
    return EOF;
}

/*void skip_whitespace(input_t * in) {
   char c;
   while ((c = read1(in)) == ' ' || c == '\n' || c == '\t') ;
   if (c != EOF) { in->start--; if (c == '\n') { in->line--; } else { in->col--;} }
}
*/
//=============================================================================
// PRIMITIVE PARSING FUNCTIONS (THE `_fn` IMPLEMENTATIONS)
//=============================================================================

combinator_t * new_combinator() {
    combinator_t *comb = (combinator_t *) safe_malloc(sizeof(combinator_t));
    // Explicitly zero out the entire struct to avoid uninitialised value warnings
    memset(comb, 0, sizeof(combinator_t));
    comb->type = P_MATCH; // Default value, will be overridden
    comb->extra_to_free = NULL;
    return comb;
}

static ParseResult match_ci_fn(input_t * in, void * args, char* parser_name) {
    char * str = ((match_args *) args)->str;
    InputState state; save_input_state(in, &state);
    for (int i = 0, len = strlen(str); i < len; i++) {
        char c = read1(in);
        if (tolower(c) != tolower(str[i])) {
            restore_input_state(in, &state);
            char* unexpected = strndup(in->buffer + state.start, 10);
            char* err_msg;
            if (asprintf(&err_msg, "Parser '%s' Expected '%s' (case-insensitive) but found '%.10s...'", parser_name ? parser_name : "N/A", str, unexpected) < 0) {
                err_msg = strdup("Expected token (case-insensitive)");
            }
            return make_failure_v2(in, parser_name, err_msg, unexpected);
        }
    }
    return make_success(ensure_ast_nil_initialized());
}

static ParseResult match_fn(input_t * in, void * args, char* parser_name) {
    char * str = ((match_args *) args)->str;
    InputState state; save_input_state(in, &state);
    for (int i = 0, len = strlen(str); i < len; i++) {
        char c = read1(in);
        if (c != str[i]) {
            restore_input_state(in, &state);
            char* unexpected = strndup(in->buffer + state.start, 10);
            char* err_msg;
            if (asprintf(&err_msg, "Parser '%s' Expected '%s' but found '%.10s...'", parser_name ? parser_name : "N/A", str, unexpected) < 0) {
                err_msg = strdup("Expected token");
            }
            return make_failure_v2(in, parser_name, err_msg, unexpected);
        }
    }
    return make_success(ensure_ast_nil_initialized());
}

static ParseResult integer_fn(input_t * in, void * args, char* parser_name) {
   prim_args* pargs = (prim_args*)args;
   InputState state; save_input_state(in, &state);
   int start_pos_ws = in->start;
   char c = read1(in);
   if (!isdigit(c)) {
       restore_input_state(in, &state);
       char* unexpected = strndup(in->buffer + state.start, 10);
       return make_failure_v2(in, parser_name, strdup("Expected a digit."), unexpected);
   }
   while (isdigit(c = read1(in))) ;
   if (c != EOF) in->start--;
   int len = in->start - start_pos_ws;
   char * text = (char*)safe_malloc(len + 1);
   strncpy(text, in->buffer + start_pos_ws, len);
   text[len] = '\0';
   ast_t * ast = new_ast();
   ast->typ = pargs->tag; ast->sym = sym_lookup(text); free(text);
   ast->child = NULL; ast->next = NULL;
   set_ast_position(ast, in);
   return make_success(ast);
}

static ParseResult cident_fn(input_t * in, void * args, char* parser_name) {
   prim_args* pargs = (prim_args*)args;
   InputState state; save_input_state(in, &state);
   int start_pos_ws = in->start;
   char c = read1(in);
   if (c != '_' && !isalpha(c)) {
       restore_input_state(in, &state);
       char* unexpected = strndup(in->buffer + state.start, 10);
       return make_failure_v2(in, parser_name, strdup("Expected identifier."), unexpected);
   }
   while (isalnum(c = read1(in)) || c == '_') ;
   if (c != EOF) in->start--;
   int len = in->start - start_pos_ws;
   char * text = (char*)safe_malloc(len + 1);
   strncpy(text, in->buffer + start_pos_ws, len);
   text[len] = '\0';
   ast_t * ast = new_ast();
   ast->typ = pargs->tag; ast->sym = sym_lookup(text); free(text);
   ast->child = NULL; ast->next = NULL;
   set_ast_position(ast, in);
   return make_success(ast);
}

static ParseResult string_fn(input_t * in, void * args, char* parser_name) {
   prim_args* pargs = (prim_args*)args;
   InputState state; save_input_state(in, &state);
   if (read1(in) != '"') {
       restore_input_state(in, &state);
       char* unexpected = strndup(in->buffer + state.start, 10);
       return make_failure_v2(in, parser_name, strdup("Expected '\"'."), unexpected);
   }
   int capacity = 64;
   char * str_val = (char *) safe_malloc(capacity);
   int len = 0; char c;
   while ((c = read1(in)) != '"') {
      if (c == EOF) {
          free(str_val);
          return make_failure_v2(in, parser_name, strdup("Unterminated string."), NULL);
      }
      if (c == '\\') {
         c = read1(in);
         if (c == EOF) {
             free(str_val);
             return make_failure_v2(in, parser_name, strdup("Unterminated string."), NULL);
         }
         switch (c) {
            case 'n': c = '\n'; break; case 't': c = '\t'; break;
            case '"': c = '"'; break; case '\\': c = '\\'; break;
         }
      }
      if (len + 1 >= capacity) {
         capacity *= 2;
         char* new_str_val = realloc(str_val, capacity);
         if (!new_str_val) { free(str_val); exception("realloc failed"); }
         str_val = new_str_val;
      }
      str_val[len++] = c;
   }
   str_val[len] = '\0';
   ast_t * ast = new_ast();
   ast->typ = pargs->tag; ast->sym = sym_lookup(str_val); free(str_val);
   ast->child = NULL; ast->next = NULL;
   set_ast_position(ast, in);
   return make_success(ast);
}

static ParseResult any_char_fn(input_t * in, void * args, char* parser_name) {
    prim_args* pargs = (prim_args*)args;
    InputState state; save_input_state(in, &state);
    char c = read1(in);
    if (c == EOF) {
        restore_input_state(in, &state);
        return make_failure_v2(in, parser_name, strdup("Expected any character, but found EOF."), NULL);
    }
    char str[2] = {c, '\0'};
    ast_t* ast = new_ast();
    ast->typ = pargs->tag;
    ast->sym = sym_lookup(str);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);
    return make_success(ast);
}

static ParseResult satisfy_fn(input_t * in, void * args, char* parser_name) {
    satisfy_args* sargs = (satisfy_args*)args;
    InputState state; save_input_state(in, &state);
    char c = read1(in);
    if (c == EOF || !sargs->pred(c)) {
        restore_input_state(in, &state);
        char* unexpected = strndup(in->buffer + state.start, 10);
        return make_failure_v2(in, parser_name, strdup("Predicate not satisfied."), unexpected);
    }
    char str[2] = {c, '\0'};
    ast_t* ast = new_ast();
    ast->typ = sargs->tag;
    ast->sym = sym_lookup(str);
    ast->child = NULL;
    ast->next = NULL;
    set_ast_position(ast, in);
    return make_success(ast);
}

static ParseResult until_fn(input_t* in, void* args, char* parser_name) {
    until_args* uargs = (until_args*)args;
    int start_offset = in->start;
    while(1) {
        InputState current_state; save_input_state(in, &current_state);
        ParseResult res = parse(in, uargs->delimiter);
        if (res.is_success) {
            if (res.value.ast != ensure_ast_nil_initialized()) free_ast(res.value.ast);
            restore_input_state(in, &current_state); break;
        }
        free_error(res.value.error);
        restore_input_state(in, &current_state);
        if (read1(in) == EOF) break;
    }
    int len = in->start - start_offset;
    char* text = (char*)safe_malloc(len + 1);
    strncpy(text, in->buffer + start_offset, len);
    text[len] = '\0';
    ast_t* ast = new_ast();
    ast->typ = uargs->tag; ast->sym = sym_lookup(text); free(text);
    set_ast_position(ast, in);
    return make_success(ast);
}

static ParseResult expr_fn(input_t * in, void * args, char* parser_name) {
   expr_list * list = (expr_list *) args;
   if (list == NULL) return make_failure_v2(in, parser_name, strdup("Invalid expression grammar."), NULL);
   if (list->fix == EXPR_BASE) return parse(in, list->comb);
   if (list->fix == EXPR_PREFIX) {
       op_t* op = list->op;
       if (op) {
           InputState state; save_input_state(in, &state);
           ParseResult op_res = parse(in, op->comb);
           if (op_res.is_success) {
               free_ast(op_res.value.ast);
               ParseResult rhs_res = expr_fn(in, args, parser_name);
               if (!rhs_res.is_success) return rhs_res;
               return make_success(ast1(op->tag, rhs_res.value.ast));
           }
               free_error(op_res.value.error);
           restore_input_state(in, &state);
       }
   }
   ParseResult res = expr_fn(in, (void *) list->next, parser_name);
   if (!res.is_success) return res;
   ast_t* lhs = res.value.ast;
   if (list->fix == EXPR_INFIX) {
       while (1) {
           InputState loop_state; save_input_state(in, &loop_state);
           op_t *op = list->op;
           bool found_op = false;
           while (op) {
               ParseResult op_res = parse(in, op->comb);
               if (op_res.is_success) {
                   tag_t op_tag = op->tag;
                   free_ast(op_res.value.ast);
                   ParseResult rhs_res = expr_fn(in, (void *) list->next, parser_name);
                   if (!rhs_res.is_success) {
                       ast_t* rhs_partial_ast = rhs_res.value.error ? rhs_res.value.error->partial_ast : NULL;
                       if (rhs_res.value.error) {
                           rhs_res.value.error->partial_ast = NULL;
                       }
                       ast_t* new_partial_ast = ast2(op_tag, lhs, rhs_partial_ast);
                       return wrap_failure_with_ast(in, "Failed to parse right-hand side of infix operator", rhs_res, new_partial_ast);
                   }
                   lhs = ast2(op_tag, lhs, rhs_res.value.ast);
                   found_op = true;
                   break;
               }
               free_error(op_res.value.error);
               op = op->next;
           }
           if (!found_op) { restore_input_state(in, &loop_state); break; }
       }
   }
   if (list->fix == EXPR_POSTFIX) {
       while (1) {
           InputState loop_state; save_input_state(in, &loop_state);
           op_t *op = list->op;
           bool found_op = false;
           while (op) {
               ParseResult op_res = parse(in, op->comb);
               if (op_res.is_success) {
                   tag_t op_tag = op->tag;
                   free_ast(op_res.value.ast);
                   lhs = ast1(op_tag, lhs);
                   found_op = true;
                   break;
               }
               free_error(op_res.value.error);
               op = op->next;
           }
           if (!found_op) { restore_input_state(in, &loop_state); break; }
       }
   }
   return make_success(lhs);
}

static ParseResult lazy_fn(input_t * in, void * args, char* parser_name) {
    lazy_args* largs = (lazy_args*)args;
    if (largs == NULL || largs->parser_ptr == NULL || *largs->parser_ptr == NULL) {
        fprintf(stderr, "Lazy parser not initialized or pointer is NULL.\n");
        exception("Lazy parser not initialized.");
    }
    combinator_t* lazy_parser = *largs->parser_ptr;
    if (lazy_parser->fn == NULL) {
        fprintf(stderr, "Lazy parser's fn is NULL for parser at %p\n", lazy_parser);
        exception("Lazy parser's fn is NULL.");
    }
    // If the lazy parser has no name, give it the name of the lazy combinator
    if (lazy_parser->name == NULL && parser_name != NULL) {
        lazy_parser->name = strdup(parser_name);
    }
    return parse(in, lazy_parser);
}

static ParseResult eoi_fn(input_t * in, void * args, char* parser_name) {
    if (in->start == in->length) {
        return make_success(ast_nil);
    }
    return make_failure_v2(in, parser_name, strdup("Expected end of input."), NULL);
}

//=============================================================================
// PRIMITIVE PARSER CREATION FUNCTIONS (THE PUBLIC API)
//=============================================================================

combinator_t * match(char * str) {
    match_args * args = (match_args*)safe_malloc(sizeof(match_args));
    args->str = str;
    combinator_t * comb = new_combinator();
    comb->name = strdup("match");
    comb->type = P_MATCH; comb->fn = match_fn; comb->args = args; return comb;
}
combinator_t * match_ci(char * str) {
    match_args * args = (match_args*)safe_malloc(sizeof(match_args));
    args->str = str;
    combinator_t * comb = new_combinator();
    comb->name = strdup("match_ci");
    comb->type = P_CI_KEYWORD; comb->fn = match_ci_fn; comb->args = args; return comb;
}
combinator_t * integer(tag_t tag) {
    prim_args * args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t * comb = new_combinator();
    comb->name = strdup("integer");
    comb->type = P_INTEGER; comb->fn = integer_fn; comb->args = args; return comb;
}
combinator_t * cident(tag_t tag) {
    prim_args * args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t * comb = new_combinator();
    comb->name = strdup("cident");
    comb->type = P_CIDENT; comb->fn = cident_fn; comb->args = args; return comb;
}
combinator_t * string(tag_t tag) {
    prim_args * args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t * comb = new_combinator();
    comb->name = strdup("string");
    comb->type = P_STRING;
    comb->fn = string_fn;
    comb->args = args;
    return comb;
}
combinator_t * eoi() {
    combinator_t * comb = new_combinator();
    comb->name = strdup("eoi");
    comb->type = P_EOI;
    comb->fn = eoi_fn;
    comb->args = NULL;
    return comb;
}

combinator_t * satisfy(char_predicate pred, tag_t tag) {
    satisfy_args* args = (satisfy_args*)safe_malloc(sizeof(satisfy_args));
    args->pred = pred;
    args->tag = tag;
    combinator_t * comb = new_combinator();
    comb->name = strdup("satisfy");
    comb->type = P_SATISFY;
    comb->fn = satisfy_fn;
    comb->args = (void*)args;
    return comb;
}
combinator_t* until(combinator_t* p, tag_t tag) {
    until_args* args = (until_args*)safe_malloc(sizeof(until_args));
    args->delimiter = p;
    args->tag = tag;
    combinator_t* comb = new_combinator();
    comb->type = P_UNTIL; comb->fn = until_fn; comb->args = args; return comb;
}

combinator_t * any_char(tag_t tag) {
    prim_args * args = (prim_args*)safe_malloc(sizeof(prim_args));
    args->tag = tag;
    combinator_t * comb = new_combinator();
    comb->name = strdup("any_char");
    comb->type = P_ANY_CHAR;
    comb->fn = any_char_fn;
    comb->args = args;
    return comb;
}
combinator_t * expr(combinator_t * exp, combinator_t * base) {
   expr_list * args = (expr_list*)safe_malloc(sizeof(expr_list));
   args->next = NULL; args->fix = EXPR_BASE; args->comb = base; args->op = NULL;
   exp->type = COMB_EXPR; exp->fn = expr_fn; exp->args = args; return exp;
}
void expr_insert(combinator_t * exp, int prec, tag_t tag, expr_fix fix, expr_assoc assoc, combinator_t * comb) {
    expr_list *node = (expr_list*)safe_malloc(sizeof(expr_list));
    op_t *op = (op_t*)safe_malloc(sizeof(op_t));
    op->tag = tag; op->comb = comb; op->next = NULL;
    node->op = op; node->fix = fix; node->assoc = assoc; node->comb = NULL;
    expr_list **p_list = (expr_list**)&exp->args;
    for (int i = 0; i < prec; i++) {
        if (*p_list == NULL || (*p_list)->fix == EXPR_BASE) exception("Invalid precedence for expression");
        p_list = &(*p_list)->next;
    }
    node->next = *p_list; *p_list = node;
}
void expr_altern(combinator_t * exp, int prec, tag_t tag, combinator_t * comb) {
    expr_list* list = (expr_list*)exp->args;
    for (int i = 0; i < prec; i++) {
        if (list == NULL) exception("Invalid precedence for expression alternative");
        list = list->next;
    }
    if (list->fix == EXPR_BASE || list == NULL) exception("Invalid precedence");
    op_t* op = (op_t*)safe_malloc(sizeof(op_t));
    op->tag = tag; op->comb = comb; op->next = list->op;
    list->op = op;
}

//=============================================================================
// THE UNIVERSAL PARSE FUNCTION
//=============================================================================
ParseResult parse(input_t * in, combinator_t * comb) {
    if (!comb || !comb->fn) exception("Attempted to parse with a NULL or uninitialized combinator.");
    return comb->fn(in, (void *)comb->args, comb->name);
}

static combinator_t* create_lazy(combinator_t** parser_ptr, bool owns_parser) {
    if (parser_ptr == NULL) {
        exception("create_lazy called with NULL parser_ptr");
    }
    if (owns_parser && *parser_ptr == NULL) {
        exception("create_lazy called with NULL owned parser");
    }
    lazy_args* args = (lazy_args*)safe_malloc(sizeof(lazy_args));
    args->parser_ptr = parser_ptr;
    args->owns_parser = owns_parser;
    combinator_t* comb = new_combinator();
    comb->type = COMB_LAZY;
    comb->fn = lazy_fn;
    comb->args = args;
    return comb;
}

combinator_t * lazy(combinator_t** parser_ptr) {
    return create_lazy(parser_ptr, false);
}

combinator_t * lazy_owned(combinator_t** parser_ptr) {
    return create_lazy(parser_ptr, true);
}

//=============================================================================
// MEMORY MANAGEMENT
//=============================================================================

void free_error(ParseError* err) {
    if (err == NULL) return;
    if (err->parser_name) free(err->parser_name);
    if (err->unexpected) free(err->unexpected);
    free(err->message);
    free_error(err->cause);
    if (err->partial_ast != NULL) {
        free_ast(err->partial_ast);
    }
    free(err);
}

void free_ast(ast_t* ast) {
    if (ast == NULL || ast == ensure_ast_nil_initialized()) return;
    free_ast(ast->child);
    free_ast(ast->next);
    if (ast->sym) { free(ast->sym->name); free(ast->sym); }
    free(ast);
}

// Initialize ast_nil if not already initialized
static ast_t* ensure_ast_nil_initialized() {
    if (ast_nil == NULL) {
        ast_nil = new_ast();
        ast_nil->typ = 0;
    }
    return ast_nil;
}


void parser_walk_ast(ast_t* ast, ast_visitor_fn visitor, void* context) {
    if (ast == NULL || ast == ensure_ast_nil_initialized()) {
        return;
    }

    visitor(ast, context);

    if (ast->child) {
        parser_walk_ast(ast->child, visitor, context);
    }

    if (ast->next) {
        parser_walk_ast(ast->next, visitor, context);
    }
}

typedef struct visited_node { const void* ptr; struct visited_node* next; } visited_node;
typedef struct extra_node {
    void* ptr;
    combinator_t* comb;
    struct extra_node* next;
} extra_node;

void free_combinator_recursive(combinator_t* comb, visited_node** visited, extra_node** extras);

bool is_visited(const void* ptr, visited_node* list) {
    for (visited_node* current = list; current != NULL; current = current->next) {
        if (current->ptr == ptr) return true;
    }
    return false;
}

static void release_extra_nodes(extra_node** extras, visited_node** visited);

void free_combinator(combinator_t* comb) {
    visited_node* visited = NULL;
    extra_node* extras = NULL;
    free_combinator_recursive(comb, &visited, &extras);
    // Drain any heap-allocated pointer wrappers that were deferred during the
    // recursive walk. These nodes own both the wrapper pointer itself and, when
    // present, the combinator the pointer referenced at creation time.
    release_extra_nodes(&extras, &visited);
    visited_node* current = visited;
    while (current != NULL) {
        visited_node* temp = current;
        current = current->next;
        free(temp);
    }
}

void free_combinator_recursive(combinator_t* comb, visited_node** visited, extra_node** extras) {
    if (comb == NULL || is_visited(comb, *visited)) return;
    visited_node* new_visited = (visited_node*)safe_malloc(sizeof(visited_node));
    new_visited->ptr = comb;
    new_visited->next = *visited;
    *visited = new_visited;

    // Ensure type is valid to avoid uninitialised value warnings
    if (comb->type >= P_MATCH && comb->type <= P_EOI) {
        // Type is valid, proceed with normal logic
    } else {
        // Type is invalid/uninitialised, set to default and free args if present
        comb->type = P_MATCH;
        if (comb->args != NULL) {
            free(comb->args);
            comb->args = NULL;
        }
        free(comb);
        return;
    }

    if (comb->name) {
        free(comb->name);
        comb->name = NULL;
    }
    if (comb->args != NULL) {
        switch (comb->type) {
            case P_CI_KEYWORD:
            case P_MATCH:
                free((match_args*)comb->args);
                break;
            case COMB_EXPECT: {
                expect_args* args = (expect_args*)comb->args;
                free_combinator_recursive(args->comb, visited, extras);
                free(args);
                break;
            }
            case COMB_OPTIONAL: {
                optional_args* args = (optional_args*)comb->args;
                free_combinator_recursive(args->p, visited, extras);
                free(args);
                break;
            }
            case COMB_ERRMAP: {
                errmap_args* args = (errmap_args*)comb->args;
                free_combinator_recursive(args->parser, visited, extras);
                free(args);
                break;
            }
            case COMB_MAP: {
                map_args* args = (map_args*)comb->args;
                free_combinator_recursive(args->parser, visited, extras);
                free(args);
                break;
            }
            case P_SUCCEED: {
                succeed_args* args = (succeed_args*)comb->args;
                free_ast(args->ast);
                free(args);
                break;
            }
            case COMB_CHAINL1: {
                chainl1_args* args = (chainl1_args*)comb->args;
                free_combinator_recursive(args->p, visited, extras);
                free_combinator_recursive(args->op, visited, extras);
                free(args);
                break;
            }
            case COMB_SEP_END_BY: {
                sep_end_by_args* args = (sep_end_by_args*)comb->args;
                free_combinator_recursive(args->p, visited, extras);
                free_combinator_recursive(args->sep, visited, extras);
                free(args);
                break;
            }
            case COMB_SEP_BY: {
                sep_by_args* args = (sep_by_args*)comb->args;
                free_combinator_recursive(args->p, visited, extras);
                free_combinator_recursive(args->sep, visited, extras);
                free(args);
                break;
            }
            case COMB_NOT: {
                not_args* args = (not_args*)comb->args;
                free_combinator_recursive(args->p, visited, extras);
                free(args);
                break;
            }
            case COMB_PEEK: {
                peek_args* args = (peek_args*)comb->args;
                free_combinator_recursive(args->p, visited, extras);
                free(args);
                break;
            }
            case COMB_BETWEEN: {
                between_args* args = (between_args*)comb->args;
                free_combinator_recursive(args->open, visited, extras);
                free_combinator_recursive(args->close, visited, extras);
                free_combinator_recursive(args->p, visited, extras);
                free(args);
                break;
            }
            case COMB_GSEQ:
            case COMB_SEQ:
            case COMB_MULTI: {
                seq_args* args = (seq_args*)comb->args;
                seq_list* current = args->list;
                while (current != NULL) {
                    free_combinator_recursive(current->comb, visited, extras);
                    seq_list* temp = current;
                    current = current->next;
                    free(temp);
                }
                free(args);
                break;
            }
            case COMB_FLATMAP: {
                flatMap_args* args = (flatMap_args*)comb->args;
                free_combinator_recursive(args->parser, visited, extras);
                free(args);
                break;
            }
            case P_UNTIL: {
                until_args* args = (until_args*)comb->args;
                if (args != NULL) {
                    if (args->delimiter != NULL) {
                        free_combinator_recursive(args->delimiter, visited, extras);
                    }
                    free(args);
                }
                break;
            }
            case COMB_LAZY: {
                lazy_args* args = (lazy_args*)comb->args;
                if (args != NULL) {
                    if (args->owns_parser && args->parser_ptr != NULL && *args->parser_ptr != NULL) {
                        free_combinator_recursive(*args->parser_ptr, visited, extras);
                    }
                    free(args);
                }
                break;
            }
            case COMB_EXPR: {
                expr_list* list = (expr_list*)comb->args;
                while (list != NULL) {
                    if (list->fix == EXPR_BASE) {
                        free_combinator_recursive(list->comb, visited, extras);
                    }
                    op_t* op = list->op;
                    while (op != NULL) {
                        free_combinator_recursive(op->comb, visited, extras);
                        op_t* temp_op = op;
                        op = op->next;
                        free(temp_op);
                    }
                    expr_list* temp_list = list;
                    list = list->next;
                    free(temp_list);
                }
                break;
            }
            case P_SATISFY: {
                free((satisfy_args*)comb->args);
                break;
            }
            case COMB_LEFT:
            case COMB_RIGHT: {
                pair_args* args = (pair_args*)comb->args;
                free_combinator_recursive(args->p1, visited, extras);
                free_combinator_recursive(args->p2, visited, extras);
                free(args);
                break;
            }
            case COMB_MANY: {
                free_combinator_recursive((combinator_t*)comb->args, visited, extras);
                break;
            }
            case P_INTEGER:
            case P_CIDENT:
            case P_STRING:
            case P_ANY_CHAR: {
                prim_args* args = (prim_args*)comb->args;
                if (args != NULL) {
                    free(args);
                }
                break;
            }
            default:
                break;
        }
    }
    if (comb->extra_to_free) {
        combinator_t **to_clear = (combinator_t **)comb->extra_to_free;
        combinator_t* owned_comb = NULL;
        if (to_clear != NULL) {
            owned_comb = *to_clear;
            *to_clear = NULL;
        }
        extra_node* node = (extra_node*)safe_malloc(sizeof(extra_node));
        node->ptr = comb->extra_to_free;
        node->comb = owned_comb;
        node->next = *extras;
        *extras = node;
        comb->extra_to_free = NULL;
    }
    free(comb);
}

static void release_extra_nodes(extra_node** extras, visited_node** visited) {
    while (*extras != NULL) {
        extra_node* node = *extras;
        *extras = node->next;
        if (node->comb != NULL) {
            // Recursively release the combinator captured when the wrapper was
            // enqueued. Any additional extra_to_free entries discovered during
            // this call are appended to the shared list referenced by
            // `extras` so they can be drained in-order.
            free_combinator_recursive(node->comb, visited, extras);
        }
        free(node->ptr);
        free(node);
    }
}
