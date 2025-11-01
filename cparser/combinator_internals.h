#ifndef COMBINATOR_INTERNALS_H
#define COMBINATOR_INTERNALS_H

#include "parser.h"

// --- Argument Structs for Combinators ---

typedef struct {
    combinator_t * comb;
    char * msg;
} expect_args;

typedef struct seq_list {
    combinator_t * comb;
    struct seq_list * next;
} seq_list;

typedef struct {
    tag_t typ;
    seq_list * list;
} seq_args;

typedef struct {
    combinator_t * parser;
    flatMap_func func;
} flatMap_args;

typedef struct {
    combinator_t* p1;
    combinator_t* p2;
} pair_args;

typedef struct {
    combinator_t* p;
} not_args;

typedef struct {
    combinator_t* p;
} peek_args;

typedef struct {
    combinator_t* p;
} optional_args;

typedef struct {
    combinator_t* open;
    combinator_t* close;
    combinator_t* p;
} between_args;

typedef struct {
    combinator_t* p;
    combinator_t* sep;
} sep_by_args;

typedef struct {
    combinator_t* p;
    combinator_t* sep;
} sep_end_by_args;

typedef struct {
    combinator_t* p;
    combinator_t* op;
} chainl1_args;

typedef struct {
    ast_t* ast;
} succeed_args;

typedef struct {
    combinator_t* parser;
    map_func func;
} map_args;

typedef struct {
    combinator_t* parser;
    err_map_func func;
} errmap_args;

typedef struct {
    combinator_t* parser;
} anchor_args;

typedef struct {
    char_predicate pred;
    tag_t tag;
} satisfy_args;

typedef struct {
    combinator_t** parser_ptr;
    bool owns_parser;
} lazy_args;

#endif // COMBINATOR_INTERNALS_H
