#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdbool.h>

//=============================================================================
// Public-Facing Structs and Enums
//=============================================================================

// Forward declarations
typedef struct ast_t ast_t;
typedef struct combinator_t combinator_t;
typedef struct input_t input_t;
typedef struct ParseResult ParseResult;

// AST node types
typedef unsigned int tag_t;

// --- Argument Structs ---
typedef struct { tag_t tag; } prim_args;

// Symbol
typedef struct sym_t {
   char * name;
} sym_t;

// AST node
struct ast_t {
   tag_t typ;
   ast_t * child;
   ast_t * next;
   sym_t * sym;
   int line;
   int col;
};

// Input stream
struct input_t {
   char * buffer;
   int alloc;
   int length;
   int start;
   int line;
   int col;
};

// --- Parse Result & Error Structs ---
typedef struct ParseError {
    int line;
    int col;
    char* message;
    char* parser_name;
    char* unexpected;
    struct ParseError* cause;
    ast_t* partial_ast;
} ParseError;

struct ParseResult {
    bool is_success;
    union {
        ast_t* ast;
        ParseError* error;
    } value;
};

// Main parser struct
typedef enum {
    P_MATCH, P_MATCH_RAW, P_INTEGER, P_CIDENT, P_STRING, P_UNTIL, P_SUCCEED, P_ANY_CHAR, P_SATISFY, P_CI_KEYWORD,
    COMB_EXPECT, COMB_SEQ, COMB_MULTI, COMB_FLATMAP, COMB_MANY, COMB_EXPR,
    COMB_OPTIONAL, COMB_SEP_BY, COMB_LEFT, COMB_RIGHT, COMB_NOT, COMB_PEEK,
    COMB_GSEQ, COMB_BETWEEN, COMB_SEP_END_BY, COMB_CHAINL1, COMB_MAP, COMB_ERRMAP,
    COMB_LAZY,
    P_EOI
} parser_type_t;

typedef ParseResult (*comb_fn)(input_t *in, void *args, char* parser_name);

struct combinator_t {
    parser_type_t type;
    comb_fn fn;
    void * args;
    void * extra_to_free;
    char* name;
};

// For flatMap
typedef combinator_t * (*flatMap_func)(ast_t *ast);

// For map
typedef ast_t * (*map_func)(ast_t *ast);

// For errmap
typedef ParseError * (*err_map_func)(ParseError *err);

// For satisfy
typedef bool (*char_predicate)(char);

// Partial AST error wrapping
ParseResult wrap_failure_with_ast(input_t* in, char* message, ParseResult original_result, ast_t* partial_ast);

//=============================================================================
// Global Variables
//=============================================================================

extern ast_t * ast_nil;


//=============================================================================
// Public Function Prototypes
//=============================================================================

// --- Core Parser Function ---
ParseResult parse(input_t * in, combinator_t * comb);

// --- Primitive Parser Constructors ---
combinator_t * match(char * str);
combinator_t * match_ci(char * str);
combinator_t * match_raw(char * str);
combinator_t * integer(tag_t tag);
combinator_t * cident(tag_t tag);
combinator_t * string(tag_t tag);
combinator_t * until(combinator_t* p, tag_t tag);
combinator_t * any_char(tag_t tag);
combinator_t * satisfy(char_predicate pred, tag_t tag);
combinator_t * eoi();

// --- Combinator Constructors ---
combinator_t * lazy(combinator_t** parser_ptr);
combinator_t * lazy_owned(combinator_t** parser_ptr);

// --- Expression Parser Constructors ---
typedef enum { EXPR_BASE, EXPR_INFIX, EXPR_PREFIX, EXPR_POSTFIX } expr_fix;
typedef enum { ASSOC_LEFT, ASSOC_RIGHT, ASSOC_NONE } expr_assoc;

combinator_t * expr(combinator_t * exp, combinator_t * base);
void expr_insert(combinator_t * exp, int prec, tag_t tag, expr_fix fix, expr_assoc assoc, combinator_t * comb);
void expr_altern(combinator_t * exp, int prec, tag_t tag, combinator_t * comb);

// --- Input Stream Helpers ---
input_t * new_input();
char read1(input_t * in);
void set_ast_position(ast_t* ast, input_t* in);
void init_input_buffer(input_t *in, char *buffer, int length);

// --- AST Helpers ---
typedef void (*ast_visitor_fn)(ast_t* node, void* context);
void parser_walk_ast(ast_t* ast, ast_visitor_fn visitor, void* context);
ast_t* new_ast();
void free_ast(ast_t* ast);
void free_error(ParseError* err);
ast_t* ast1(tag_t typ, ast_t* a1);
ast_t* ast2(tag_t typ, ast_t* a1, ast_t* a2);
ast_t* copy_ast(ast_t* orig);

// --- Combinator Helpers ---
combinator_t* new_combinator();

// --- Extensibility Helpers ---
typedef struct { int start; int line; int col; } InputState;
void save_input_state(input_t* in, InputState* state);
void restore_input_state(input_t* in, InputState* state);
ParseResult make_success(ast_t* ast);
ParseResult make_failure(input_t* in, char* message);
ParseResult make_failure_v2(input_t* in, char* parser_name, char* message, char* unexpected);
ParseResult wrap_failure(input_t* in, char* message, char* parser_name, ParseResult cause);

// --- Helper Function Prototypes ---
void* safe_malloc(size_t size);
sym_t * sym_lookup(const char * name);

// --- Memory Management ---
void free_combinator(combinator_t* comb);
void exception(const char * err);


#endif // PARSER_H
