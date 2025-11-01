#define _GNU_SOURCE
#include "parser.h"
#include "combinators.h"
#include "combinator_internals.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

// Initialize ast_nil if not already initialized
static ast_t* ensure_ast_nil_initialized() {
    if (ast_nil == NULL) {
        ast_nil = new_ast();
        ast_nil->typ = 0;
    }
    return ast_nil;
}

// --- Static Function Forward Declarations ---
static ParseResult expect_fn(input_t * in, void * args, char* parser_name);
static ParseResult seq_fn(input_t * in, void * args, char* parser_name);
static ParseResult multi_fn(input_t * in, void * args, char* parser_name);
static ParseResult flatMap_fn(input_t * in, void * args, char* parser_name);
static ParseResult left_fn(input_t * in, void * args, char* parser_name);
static ParseResult right_fn(input_t * in, void * args, char* parser_name);
static ParseResult pnot_fn(input_t * in, void * args, char* parser_name);
static ParseResult peek_fn(input_t * in, void * args, char* parser_name);
static ParseResult gseq_fn(input_t * in, void * args, char* parser_name);
static ParseResult between_fn(input_t * in, void * args, char* parser_name);
static ParseResult sep_by_fn(input_t * in, void * args, char* parser_name);
static ParseResult sep_by1_fn(input_t * in, void * args, char* parser_name);
static ParseResult sep_end_by_fn(input_t * in, void * args, char* parser_name);
static ParseResult chainl1_fn(input_t * in, void * args, char* parser_name);
static ParseResult succeed_fn(input_t * in, void * args, char* parser_name);
static ParseResult map_fn(input_t * in, void * args, char* parser_name);
static ParseResult errmap_fn(input_t * in, void * args, char* parser_name);
static ParseResult many_fn(input_t * in, void * args, char* parser_name);
static ParseResult optional_fn(input_t * in, void * args, char* parser_name);

// --- _fn Implementations ---

static ParseResult optional_fn(input_t * in, void * args, char* parser_name) {
    optional_args* oargs = (optional_args*)args;
    InputState state;
    save_input_state(in, &state);
    ParseResult res = parse(in, oargs->p);
    if (res.is_success) {
        return res;
    }
    // If it fails, we restore the input and return success with a nil AST.
    restore_input_state(in, &state);
    free_error(res.value.error);
    return make_success(ensure_ast_nil_initialized());
}

static ParseResult pnot_fn(input_t * in, void * args, char* parser_name) {
    not_args* nargs = (not_args*)args;
    InputState state; save_input_state(in, &state);
    ParseResult res = parse(in, nargs->p);
    restore_input_state(in, &state);
    if (res.is_success) {
        free_ast(res.value.ast);
        return make_failure_v2(in, parser_name, strdup("not combinator failed."), NULL);
    }
    // The error from the inner parse is consumed and we return success.
    free_error(res.value.error);
    return make_success(ast_nil);
}

static ParseResult peek_fn(input_t * in, void * args, char* parser_name) {
    peek_args* pargs = (peek_args*)args;
    InputState state; save_input_state(in, &state);
    ParseResult res = parse(in, pargs->p);
    restore_input_state(in, &state);
    return res;
}

static ParseResult expect_fn(input_t * in, void * args, char* parser_name) {
    expect_args * eargs = (expect_args *) args;
    ParseResult res = parse(in, eargs->comb);
    if (res.is_success) return res;

    char* final_message;
    if (res.value.error && res.value.error->unexpected) {
        if (asprintf(&final_message, "%s but found '%s'", eargs->msg, res.value.error->unexpected) < 0) {
            final_message = strdup(eargs->msg);
        }
    } else {
        final_message = strdup(eargs->msg);
    }
    return wrap_failure(in, final_message, parser_name, res);
}

static ParseResult between_fn(input_t * in, void * args, char* parser_name) {
    between_args* bargs = (between_args*)args;
    InputState state; save_input_state(in, &state);

    ParseResult r_open = parse(in, bargs->open);
    if (!r_open.is_success) return r_open;
    free_ast(r_open.value.ast);

    ParseResult r_p = parse(in, bargs->p);
    if (!r_p.is_success) {
        restore_input_state(in, &state);
        return r_p;
    }

    ParseResult r_close = parse(in, bargs->close);
    if (!r_close.is_success) {
        restore_input_state(in, &state);
        free_ast(r_p.value.ast);
        return r_close;
    }
    free_ast(r_close.value.ast);

    return r_p;
}

static ast_t *append_remaining(input_t *in, sep_by_args *sargs, ast_t *tail) {
    while (1) {
        InputState state;
        save_input_state(in, &state);

        ParseResult sep_res = parse(in, sargs->sep);
        if (!sep_res.is_success) {
            restore_input_state(in, &state);
            free_error(sep_res.value.error);
            break;
        }
        free_ast(sep_res.value.ast);

        ParseResult next_res = parse(in, sargs->p);
        if (!next_res.is_success) {
            restore_input_state(in, &state);
            free_error(next_res.value.error);
            break;
        }

        tail->next = next_res.value.ast;
        tail = tail->next;
    }

    return tail;
}

static ParseResult sep_by_common(input_t *in, sep_by_args *sargs, bool require_first) {
    ParseResult first = parse(in, sargs->p);
    if (!first.is_success) {
        if (!require_first) {
            free_error(first.value.error);
            return make_success(ast_nil);
        }
        return first;
    }

    ast_t *head = first.value.ast;
    ast_t *tail = head;

    tail = append_remaining(in, sargs, tail);

    return make_success(head);
}

static ParseResult sep_by_fn(input_t * in, void * args, char* parser_name) {
    return sep_by_common(in, (sep_by_args*)args, false);
}

static ParseResult sep_by1_fn(input_t * in, void * args, char* parser_name) {
    return sep_by_common(in, (sep_by_args*)args, true);
}

static ParseResult sep_end_by_fn(input_t * in, void * args, char* parser_name) {
    sep_end_by_args* sargs = (sep_end_by_args*)args;
    ast_t* head = NULL;
    ast_t* tail = NULL;

    ParseResult res = parse(in, sargs->p);
    if (!res.is_success) {
        free_error(res.value.error);
        return make_success(ast_nil);
    }
    head = tail = res.value.ast;

    while (1) {
        InputState state; save_input_state(in, &state);
        ParseResult sep_res = parse(in, sargs->sep);
        if (!sep_res.is_success) {
            restore_input_state(in, &state);
            free_error(sep_res.value.error);
            break;
        }
        free_ast(sep_res.value.ast);

        ParseResult p_res = parse(in, sargs->p);
        if (!p_res.is_success) {
            restore_input_state(in, &state);
            free_error(p_res.value.error);
            break;
        }
        tail->next = p_res.value.ast;
        tail = tail->next;
    }

    // Try to parse a final separator
    InputState final_sep_state; save_input_state(in, &final_sep_state);
    ParseResult final_sep_res = parse(in, sargs->sep);
    if (!final_sep_res.is_success) {
        restore_input_state(in, &final_sep_state);
        free_error(final_sep_res.value.error);
    } else {
        free_ast(final_sep_res.value.ast);
    }

    return make_success(head);
}

static ParseResult chainl1_fn(input_t * in, void * args, char* parser_name) {
    chainl1_args* cargs = (chainl1_args*)args;

    ParseResult res = parse(in, cargs->p);
    if (!res.is_success) {
        return res;
    }
    ast_t* left = res.value.ast;

    while (1) {
        InputState state; save_input_state(in, &state);
        ParseResult op_res = parse(in, cargs->op);
        if (!op_res.is_success) {
            restore_input_state(in, &state);
            free_error(op_res.value.error);
            break;
        }
        tag_t op_tag = op_res.value.ast->typ;
        free_ast(op_res.value.ast);

        ParseResult right_res = parse(in, cargs->p);
        if (!right_res.is_success) {
            restore_input_state(in, &state);
            free_ast(left);
            // The op succeeded, so there is no error to free in op_res
            return wrap_failure(in, strdup("Expected operand after operator in chainl1"), parser_name, right_res);
        }
        ast_t* right = right_res.value.ast;
        left = ast2(op_tag, left, right);
    }

    return make_success(left);
}

static ParseResult succeed_fn(input_t * in, void * args, char* parser_name) {
    succeed_args* sargs = (succeed_args*)args;
    return make_success(copy_ast(sargs->ast));
}

static ParseResult errmap_fn(input_t * in, void * args, char* parser_name) {
    errmap_args* eargs = (errmap_args*)args;
    ParseResult res = parse(in, eargs->parser);
    if (res.is_success) {
        return res;
    }
    res.value.error = eargs->func(res.value.error);
    return res;
}

static ParseResult many_fn(input_t * in, void * args, char* parser_name) {
    combinator_t* p = (combinator_t*)args;
    ast_t* head = NULL;
    ast_t* tail = NULL;
    while (1) {
        InputState state;
        save_input_state(in, &state);
        ParseResult res = parse(in, p);
        if (!res.is_success) {
            restore_input_state(in, &state);
            free_error(res.value.error);
            break;
        }
        if (head == NULL) {
            head = tail = res.value.ast;
        } else {
            tail->next = res.value.ast;
            tail = tail->next;
        }
    }
    return make_success(head ? head : ast_nil);
}

static ParseResult map_fn(input_t * in, void * args, char* parser_name) {
    map_args* margs = (map_args*)args;
    ParseResult res = parse(in, margs->parser);
    if (!res.is_success) {
        return res;
    }
    ast_t* new_ast = margs->func ? margs->func(res.value.ast) : res.value.ast;
    return make_success(new_ast);
}

static ast_t* find_tail_node(ast_t* node) {
    if (node == NULL || node == ast_nil)
        return node;

    ast_t* cur = node;
    while (cur->next != NULL && cur->next != ast_nil)
        cur = cur->next;
    return cur;
}

static ParseResult gseq_fn(input_t * in, void * args, char* parser_name) {
    seq_args * sa = (seq_args *) args;
    seq_list * seq = sa->list;
    ast_t * head = NULL, * tail = NULL;
    while (seq != NULL) {
        ParseResult res = parse(in, seq->comb);
        if (!res.is_success) return res;
        if (res.value.ast != ast_nil) {
            if (head == NULL) {
                head = res.value.ast;
                tail = find_tail_node(head);
            } else {
                tail->next = res.value.ast;
                tail = find_tail_node(tail->next);
            }
        }
        seq = seq->next;
    }
    ast_t* result_child = head ? head : ast_nil;
    if (sa->typ == 0) {
        return make_success(result_child);
    } else {
        return make_success(ast1(sa->typ, result_child));
    }
}

static ParseResult seq_fn(input_t * in, void * args, char* parser_name) {
    InputState state; save_input_state(in, &state);
    seq_args * sa = (seq_args *) args;
    seq_list * seq = sa->list;
    ast_t * head = NULL, * tail = NULL;
    while (seq != NULL) {
        ParseResult res = parse(in, seq->comb);
        if (!res.is_success) {
            restore_input_state(in, &state);
            if (res.value.error->message == NULL) {
                return wrap_failure_with_ast(in, "Failed to parse sequence.", res, head);
            }
            return wrap_failure_with_ast(in, res.value.error->message, res, head);
        }
        if (res.value.ast != ast_nil) {
            if (head == NULL) {
                head = res.value.ast;
                tail = find_tail_node(head);
            } else {
                tail->next = res.value.ast;
                tail = find_tail_node(tail->next);
            }
        }
        seq = seq->next;
    }
    ast_t* result_child = head ? head : ast_nil;
    if (sa->typ == 0) {
        return make_success(result_child);
    } else {
        return make_success(ast1(sa->typ, result_child));
    }
}

static ParseResult multi_fn(input_t * in, void * args, char* parser_name) {
    seq_args * sa = (seq_args *) args;
    seq_list * seq = sa->list;
    /* HARDENED: A multi-parser should always have alternatives by design. */
    if (seq == NULL) {
        fprintf(stderr,
            "FATAL: %s (%s:%d) multi-parser called with no alternatives\n",
            __func__, __FILE__, __LINE__);
        abort();
    }

    InputState state;
    save_input_state(in, &state);

    ParseResult last_res;
    for (seq_list *current = seq; current != NULL; current = current->next) {
        restore_input_state(in, &state);
        ParseResult res = parse(in, current->comb);
        if (res.is_success) {
            if (sa->typ != 0) {
                res.value.ast = ast1(sa->typ, res.value.ast);
            }
            return res;
        }
        if (current->next == NULL) {
            last_res = res;
        } else {
            free_error(res.value.error);
        }
    }

    return last_res;
}

static ParseResult flatMap_fn(input_t * in, void * args, char* parser_name) {
    flatMap_args * fm_args = (flatMap_args *)args;
    InputState state; save_input_state(in, &state);
    ParseResult res = parse(in, fm_args->parser);
    if (!res.is_success) return res;
    combinator_t * next_parser = fm_args->func(res.value.ast);
    /* HARDENED: A flatMap function must not return a NULL parser. */
    if (next_parser == NULL) {
        fprintf(stderr,
            "FATAL: %s (%s:%d) flatMap function returned a NULL parser\n",
            __func__, __FILE__, __LINE__);
        abort();
    }
    ParseResult final_res = parse(in, next_parser);
    free_combinator(next_parser);
    if (!final_res.is_success) restore_input_state(in, &state);
    return final_res;
}

static ParseResult left_fn(input_t * in, void * args, char* parser_name) {
    pair_args* pargs = (pair_args*)args;
    InputState state; save_input_state(in, &state);
    ParseResult r1 = parse(in, pargs->p1);
    if (!r1.is_success) return r1;
    ParseResult r2 = parse(in, pargs->p2);
    if (!r2.is_success) {
        restore_input_state(in, &state);
        return wrap_failure_with_ast(in, "left combinator failed on second parser", r2, r1.value.ast);
    }
    free_ast(r2.value.ast);
    return r1;
}

static ParseResult right_fn(input_t * in, void * args, char* parser_name) {
    pair_args* pargs = (pair_args*)args;
    InputState state; save_input_state(in, &state);
    ParseResult r1 = parse(in, pargs->p1);
    if (!r1.is_success) return r1;
    ParseResult r2 = parse(in, pargs->p2);
    if (!r2.is_success) {
        restore_input_state(in, &state);
        free_ast(r1.value.ast);
        return r2;
    }
    free_ast(r1.value.ast);
    return r2;
}


// --- Constructor Implementations ---

combinator_t * expect(combinator_t * c, char * msg) {
    expect_args * args = (expect_args*)safe_malloc(sizeof(expect_args));
    args->msg = msg; args->comb = c;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "expect %s", c->name ? c->name : "unnamed_parser");
    (void)ret; // Suppress unused variable warning
    comb->type = COMB_EXPECT; comb->fn = expect_fn; comb->args = (void *) args; return comb;
}

combinator_t * seq(combinator_t * ret, tag_t typ, combinator_t * c1, ...) {
    va_list ap;
    va_start(ap, c1);

    // Create the list of combinators
    seq_list* head = (seq_list*)safe_malloc(sizeof(seq_list));
    head->comb = c1;
    seq_list* current = head;
    int count = 1;
    size_t total_name_len = strlen("sequence of ") + (c1->name ? strlen(c1->name) : 0);

    combinator_t* c;
    while ((c = va_arg(ap, combinator_t*)) != NULL) {
        current->next = (seq_list*)safe_malloc(sizeof(seq_list));
        current = current->next;
        current->comb = c;
        count++;
        total_name_len += (c->name ? strlen(c->name) : 0) + 2; // for ", "
    }
    current->next = NULL;
    va_end(ap);

    // Allocate and generate the name
    char* name_buffer = (char*)safe_malloc(total_name_len + 1);
    char* p = name_buffer;
    p += sprintf(p, "sequence of ");
    current = head;
    for (int i = 0; i < count; i++) {
        if (current->comb->name) {
            p += sprintf(p, "%s", current->comb->name);
        }
        if (i < count - 1) {
            p += sprintf(p, ", ");
        }
        current = current->next;
    }
    ret->name = name_buffer;

    // Set up the combinator
    seq_args* args = (seq_args*)safe_malloc(sizeof(seq_args));
    args->typ = typ;
    args->list = head;
    ret->type = COMB_SEQ;
    ret->args = (void*)args;
    ret->fn = seq_fn;
    return ret;
}

combinator_t * multi(combinator_t * ret, tag_t typ, combinator_t * c1, ...) {
    va_list ap;
    va_start(ap, c1);

    // Create the list of combinators
    seq_list* head = (seq_list*)safe_malloc(sizeof(seq_list));
    head->comb = c1;
    seq_list* current = head;
    int count = 1;
    size_t total_name_len = strlen("any of ") + (c1->name ? strlen(c1->name) : 0);

    combinator_t* c;
    while ((c = va_arg(ap, combinator_t*)) != NULL) {
        current->next = (seq_list*)safe_malloc(sizeof(seq_list));
        current = current->next;
        current->comb = c;
        count++;
        total_name_len += (c->name ? strlen(c->name) : 0) + 2; // for ", "
    }
    current->next = NULL;
    va_end(ap);

    // Allocate and generate the name
    char* name_buffer = (char*)safe_malloc(total_name_len + 1);
    char* p = name_buffer;
    p += sprintf(p, "any of ");
    current = head;
    for (int i = 0; i < count; i++) {
        if (current->comb->name) {
            p += sprintf(p, "%s", current->comb->name);
        }
        if (i < count - 1) {
            p += sprintf(p, ", ");
        }
        current = current->next;
    }
    ret->name = name_buffer;

    // Set up the combinator
    seq_args* args = (seq_args*)safe_malloc(sizeof(seq_args));
    args->typ = typ;
    args->list = head;
    ret->type = COMB_MULTI;
    ret->args = (void*)args;
    ret->fn = multi_fn;
    return ret;
}

combinator_t * flatMap(combinator_t * p, flatMap_func func) {
    flatMap_args * args = (flatMap_args*)safe_malloc(sizeof(flatMap_args));
    args->parser = p; args->func = func;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "flatMap over %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_FLATMAP; comb->fn = flatMap_fn; comb->args = args; return comb;
}

combinator_t * left(combinator_t* p1, combinator_t* p2) {
    pair_args* args = (pair_args*)safe_malloc(sizeof(pair_args));
    args->p1 = p1; args->p2 = p2;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "left of %s and %s", p1->name ? p1->name : "unnamed_parser", p2->name ? p2->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_LEFT; comb->fn = left_fn; comb->args = (void *) args; return comb;
}

combinator_t * right(combinator_t* p1, combinator_t* p2) {
    pair_args* args = (pair_args*)safe_malloc(sizeof(pair_args));
    args->p1 = p1; args->p2 = p2;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "right of %s and %s", p1->name ? p1->name : "unnamed_parser", p2->name ? p2->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_RIGHT; comb->fn = right_fn; comb->args = (void *) args; return comb;
}

combinator_t * pnot(combinator_t* p) {
    not_args* args = (not_args*)safe_malloc(sizeof(not_args));
    args->p = p;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "not %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_NOT; comb->fn = pnot_fn; comb->args = (void *) args; return comb;
}

combinator_t * peek(combinator_t* p) {
    peek_args* args = (peek_args*)safe_malloc(sizeof(peek_args));
    args->p = p;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "peek %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_PEEK; comb->fn = peek_fn; comb->args = (void *) args; return comb;
}

combinator_t * gseq(combinator_t * ret, tag_t typ, combinator_t * c1, ...) {
    va_list ap;
    va_start(ap, c1);

    // Create the list of combinators
    seq_list* head = (seq_list*)safe_malloc(sizeof(seq_list));
    head->comb = c1;
    seq_list* current = head;
    int count = 1;
    size_t total_name_len = strlen("gseq of ") + (c1->name ? strlen(c1->name) : 0);

    combinator_t* c;
    while ((c = va_arg(ap, combinator_t*)) != NULL) {
        current->next = (seq_list*)safe_malloc(sizeof(seq_list));
        current = current->next;
        current->comb = c;
        count++;
        total_name_len += (c->name ? strlen(c->name) : 0) + 2; // for ", "
    }
    current->next = NULL;
    va_end(ap);

    // Allocate and generate the name
    char* name_buffer = (char*)safe_malloc(total_name_len + 1);
    char* p = name_buffer;
    p += sprintf(p, "gseq of ");
    current = head;
    for (int i = 0; i < count; i++) {
        if (current->comb->name) {
            p += sprintf(p, "%s", current->comb->name);
        }
        if (i < count - 1) {
            p += sprintf(p, ", ");
        }
        current = current->next;
    }
    ret->name = name_buffer;

    // Set up the combinator
    seq_args* args = (seq_args*)safe_malloc(sizeof(seq_args));
    args->typ = typ;
    args->list = head;
    ret->type = COMB_GSEQ;
    ret->args = (void*)args;
    ret->fn = gseq_fn;
    return ret;
}

combinator_t * between(combinator_t* open, combinator_t* close, combinator_t* p) {
    between_args* args = (between_args*)safe_malloc(sizeof(between_args));
    args->open = open;
    args->close = close;
    args->p = p;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "between %s and %s", open->name ? open->name : "unnamed_parser", close->name ? close->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_BETWEEN;
    comb->fn = between_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * errmap(combinator_t* p, err_map_func func) {
    errmap_args* args = (errmap_args*)safe_malloc(sizeof(errmap_args));
    args->parser = p;
    args->func = func;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "errmap over %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_ERRMAP;
    comb->fn = errmap_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * map(combinator_t* p, map_func func) {
    map_args* args = (map_args*)safe_malloc(sizeof(map_args));
    args->parser = p;
    args->func = func;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "map over %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_MAP;
    comb->fn = map_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * sep_by(combinator_t* p, combinator_t* sep) {
    sep_by_args* args = (sep_by_args*)safe_malloc(sizeof(sep_by_args));
    args->p = p;
    args->sep = sep;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "%s separated by %s", p->name ? p->name : "unnamed_parser", sep->name ? sep->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_SEP_BY;
    comb->fn = sep_by_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * sep_by1(combinator_t* p, combinator_t* sep) {
    sep_by_args* args = (sep_by_args*)safe_malloc(sizeof(sep_by_args));
    args->p = p;
    args->sep = sep;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "%s separated by1 %s", p->name ? p->name : "unnamed_parser", sep->name ? sep->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_SEP_BY1;
    comb->fn = sep_by1_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * sep_end_by(combinator_t* p, combinator_t* sep) {
    sep_end_by_args* args = (sep_end_by_args*)safe_malloc(sizeof(sep_end_by_args));
    args->p = p;
    args->sep = sep;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "%s separated and ended by %s", p->name ? p->name : "unnamed_parser", sep->name ? sep->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_SEP_END_BY;
    comb->fn = sep_end_by_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * chainl1(combinator_t* p, combinator_t* op) {
    chainl1_args* args = (chainl1_args*)safe_malloc(sizeof(chainl1_args));
    args->p = p;
    args->op = op;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "chainl1 of %s with %s", p->name ? p->name : "unnamed_parser", op->name ? op->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_CHAINL1;
    comb->fn = chainl1_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * succeed(ast_t* ast) {
    succeed_args* args = (succeed_args*)safe_malloc(sizeof(succeed_args));
    args->ast = ast;
    combinator_t * comb = new_combinator();
    comb->type = P_SUCCEED;
    comb->fn = succeed_fn;
    comb->args = (void *) args;
    return comb;
}

combinator_t * many(combinator_t* p) {
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "many %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_MANY;
    comb->fn = many_fn;
    comb->args = (void *) p;
    return comb;
}

combinator_t * optional(combinator_t* p) {
    optional_args* args = (optional_args*)safe_malloc(sizeof(optional_args));
    args->p = p;
    combinator_t * comb = new_combinator();
    int ret = asprintf(&comb->name, "optional %s", p->name ? p->name : "unnamed_parser");
    (void)ret;
    comb->type = COMB_OPTIONAL;
    comb->fn = optional_fn;
    comb->args = (void *) args;
    return comb;
}
