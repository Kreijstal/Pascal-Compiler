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
typedef struct memo_table memo_table_t;

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
   int index;  /* Byte offset in preprocessed buffer for accurate error context */
};

// Input stream
struct input_t {
   char * buffer;
   int alloc;
   int length;
   int start;
   int line;
   int col;
   int source_line;  // Original source line (survives backtracking, updated by #line directives)
   int source_line_base;  // Line number from last #line directive
   int source_line_base_pos;  // Buffer position after last #line directive
   memo_table_t* memo;
};

// --- Parse Result & Error Structs ---
typedef struct ParseError {
    int line;
    int col;
    int index;
    char* message;
    char* parser_name;
    char* unexpected;
    char* context;
    struct ParseError* cause;
    ast_t* partial_ast;
    bool committed;  // If true, prevents backtracking in multi combinator
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
    P_LAYOUT,
    COMB_EXPECT, COMB_SEQ, COMB_MULTI, COMB_FLATMAP, COMB_MANY, COMB_EXPR,
    COMB_OPTIONAL, COMB_SEP_BY, COMB_SEP_BY1, COMB_LEFT, COMB_RIGHT, COMB_NOT, COMB_PEEK,
    COMB_GSEQ, COMB_BETWEEN, COMB_SEP_END_BY, COMB_CHAINL1, COMB_MAP, COMB_ERRMAP,
    COMB_COMMIT,
    COMB_FOR_INIT_DISPATCH,
    COMB_ASSIGNMENT_GUARD,
    COMB_LABEL_GUARD,
    COMB_STATEMENT_DISPATCH,
    COMB_CLASS_MEMBER_DISPATCH,
    COMB_KEYWORD_DISPATCH,
    COMB_TYPE_DISPATCH,
    COMB_LAZY,
    COMB_VARIANT_TAG,
    COMB_VARIANT_PART,
    COMB_MAIN_BLOCK_CONTENT,
    COMB_EXPR_LVALUE,
    P_EOI
} parser_type_t;

typedef ParseResult (*comb_fn)(input_t *in, void *args, char* parser_name);

struct combinator_t {
    parser_type_t type;
    comb_fn fn;
    void * args;
    void * extra_to_free;
    char* name;
    size_t memo_id;
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

// --- Profiling & Diagnostics ---
typedef struct parser_stats {
    size_t parse_calls;
    size_t parse_successes;
    size_t parse_failures;
    size_t memo_hits;
    size_t memo_misses;
    size_t memo_recursions;
    size_t memo_entries_created;
    size_t memo_replays;
    size_t memo_result_clones;
    size_t ast_nodes_created;
    size_t ast_nodes_copied;
} parser_stats_t;

void parser_stats_reset(void);
parser_stats_t parser_stats_snapshot(void);

typedef enum {
    PARSER_MEMO_FULL,
    PARSER_MEMO_FAILURES_ONLY,
    PARSER_MEMO_DISABLED
} parser_memo_mode_t;

void parser_set_memo_mode(parser_memo_mode_t mode);

typedef struct parser_comb_stat {
    size_t memo_id;
    char* name;
    parser_type_t type;
    size_t calls;
    size_t successes;
    size_t failures;
    size_t failure_with_consumption;
    size_t total_failure_consumed;
    size_t max_failure_consumed;
    size_t total_success_consumed;
} parser_comb_stat_t;

void parser_comb_stats_set_enabled(bool enabled);
void parser_comb_stats_reset(void);
const parser_comb_stat_t* parser_comb_stats_snapshot(size_t* count);

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
void free_input(input_t *in);
char read1(input_t * in);
void set_ast_position(ast_t* ast, input_t* in);
void init_input_buffer(input_t *in, char *buffer, int length);

// --- AST Helpers ---
typedef void (*ast_visitor_fn)(ast_t* node, void* context);
void parser_walk_ast(ast_t* ast, ast_visitor_fn visitor, void* context);
ast_t* new_ast();
void free_ast(ast_t* ast);
void free_error(ParseError* err);
void ensure_parse_error_contexts(ParseError* err, input_t* in);
ast_t* ast1(tag_t typ, ast_t* a1);
ast_t* ast2(tag_t typ, ast_t* a1, ast_t* a2);
ast_t* copy_ast(ast_t* orig);
void parser_calculate_line_col(input_t* in, int index, int* out_line, int* out_col);
char* parser_format_context(input_t* in, int line, int col, int index);

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

typedef struct for_init_dispatch_args {
    combinator_t* assignment_parser;
    combinator_t* identifier_parser;
} for_init_dispatch_args_t;

typedef struct pascal_keyword_entry {
    const char* keyword;
    size_t length;
    combinator_t* parser;
} pascal_keyword_entry_t;

typedef struct statement_dispatch_args {
    combinator_t** keyword_parsers;
    size_t keyword_count;
    combinator_t* label_parser;
    combinator_t* assignment_parser;
    combinator_t* expr_parser;
    combinator_t* on_handler_parser;
} statement_dispatch_args_t;

typedef struct expr_lvalue_args {
    combinator_t* expr_parser;
} expr_lvalue_args_t;

typedef struct class_member_dispatch_args {
    combinator_t* constructor_parser;
    combinator_t* destructor_parser;
    combinator_t* procedure_parser;
    combinator_t* function_parser;
    combinator_t* operator_parser;
    combinator_t* property_parser;
    combinator_t* field_parser;
} class_member_dispatch_args_t;

typedef struct keyword_dispatch_args {
    pascal_keyword_entry_t* entries;
    size_t entry_count;
    const char** skip_keywords;
    size_t skip_keyword_count;
    combinator_t* fallback_parser;
} keyword_dispatch_args_t;

typedef struct type_dispatch_args {
    combinator_t* helper_parser;
    combinator_t* reference_parser;
    combinator_t* interface_parser;
    combinator_t* class_parser;
    combinator_t* class_of_parser;       /* For "class of <typename>" class reference type */
    combinator_t* record_parser;
    combinator_t* object_parser;         /* For legacy "object" type (similar to record with methods) */
    combinator_t* enumerated_parser;
    combinator_t* array_parser;
    combinator_t* file_parser;
    combinator_t* set_parser;
    combinator_t* range_parser;
    combinator_t* pointer_parser;
    combinator_t* specialize_parser;
    combinator_t* constructed_parser;
    combinator_t* identifier_parser;
    combinator_t* distinct_type_parser;  /* For "type <typename>" distinct type syntax */
    combinator_t* distinct_type_range_parser;  /* For "type <range>" distinct type from subrange */
    combinator_t* procedure_parser;      /* For traditional procedure type (without "reference to") */
    combinator_t* function_parser;       /* For traditional function type (without "reference to") */
} type_dispatch_args_t;

typedef struct main_block_args {
    combinator_t** stmt_parser;
} main_block_args_t;

// --- Helper Function Prototypes ---
void* safe_malloc(size_t size);
sym_t * sym_lookup(const char * name);

// --- Memory Management ---
void free_combinator(combinator_t* comb);
void exception(const char * err);


#endif // PARSER_H
