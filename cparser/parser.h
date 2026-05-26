/**
 * @file parser.h
 * @brief Public API of the cparser parser-combinator library.
 *
 * cparser builds backtracking PEG-style parsers out of small composable
 * @ref combinator_t values.  This header declares the AST and result types,
 * the primitive constructors (@ref match, @ref integer, @ref cident, …),
 * the input stream, profiling/memoisation knobs, and the helpers used by
 * extensions to build new combinator kinds.  See `cparser/ARCHITECTURE.md`
 * in the repository root for the design overview.
 */
#ifndef PARSER_H
#define PARSER_H

#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//=============================================================================
// Public-Facing Structs and Enums
//=============================================================================

// Forward declarations
typedef struct ast_t ast_t;
typedef struct combinator_t combinator_t;
typedef struct input_t input_t;
typedef struct ParseResult ParseResult;
typedef struct memo_table memo_table_t;

/** @brief AST node tag (unsigned int identifying the node kind). */
typedef unsigned int tag_t;

// --- Argument Structs ---
/** @brief Argument bundle for primitive parsers that only need a tag. */
typedef struct {
  tag_t tag;
} prim_args;

/** @brief Interned identifier symbol; pointer-equality is identity. */
typedef struct sym_t {
  char *name; /**< Interned name; never NULL once published. */
} sym_t;

/**
 * @brief AST node.
 *
 * Nodes form a sibling-linked list (`next`) with a child pointer; the
 * top-level AST is the root node.  @c typ is a @ref tag_t identifying
 * the node kind.  @c line / @c col / @c index point at the matching
 * input span.
 */
struct ast_t {
  tag_t typ;
  ast_t *child;
  ast_t *next;
  sym_t *sym;
  int line;
  int col;
  int index; /**< Byte offset in preprocessed buffer for accurate error context. */
};

/**
 * @brief Input stream backing a parse.
 *
 * Wraps a flat buffer and tracks position, line/column, and the optional
 * memoisation table.  Mutated as parsers consume input; backtracking
 * restores @c start, @c line, @c col.
 */
struct input_t {
  char *buffer;
  int alloc;
  int length;
  int start;
  int line;
  int col;
  int source_line;          /**< Source line surviving backtracking; updated by `#line`. */
  int source_line_base;     /**< Line number from the last `#line` directive. */
  int source_line_base_pos; /**< Buffer position after the last `#line` directive. */
  char *source_filename;    /**< Current filename from `{#line N "file"}` or NULL. */
  memo_table_t *memo;
};

// --- Parse Result & Error Structs ---
/**
 * @brief Detailed failure description produced when a parse rejects input.
 *
 * Carries enough context for human-readable diagnostics.  When a
 * combinator wraps a sub-failure it stores the inner one in @c cause.
 */
typedef struct ParseError {
  int line;
  int col;
  int index;
  char *message;
  char *parser_name;
  char *unexpected;
  char *context;
  char *source_filename; /**< Source filename from `{#line}` or NULL. */
  struct ParseError *cause;
  ast_t *partial_ast;
  bool committed;        /**< If true, prevents backtracking in `multi` combinator. */
  bool static_strings;   /**< If true, @c message / @c parser_name are static (not freed). */
  const char *format_arg; /**< Optional: when non-NULL, @c message is a `%s` template. */
} ParseError;

/** @brief Tagged-union result of @ref parse: success-with-AST or detailed error. */
struct ParseResult {
  bool is_success;
  union {
    ast_t *ast;
    ParseError *error;
  } value;
};

/** @brief Discriminator for @ref combinator_t (selects the @c fn semantics). */
typedef enum {
  P_MATCH,
  P_MATCH_RAW,
  P_INTEGER,
  P_CIDENT,
  P_STRING,
  P_UNTIL,
  P_SUCCEED,
  P_ANY_CHAR,
  P_SATISFY,
  P_CI_KEYWORD,
  P_LAYOUT,
  COMB_EXPECT,
  COMB_SEQ,
  COMB_MULTI,
  COMB_FLATMAP,
  COMB_MANY,
  COMB_EXPR,
  COMB_OPTIONAL,
  COMB_SEP_BY,
  COMB_SEP_BY1,
  COMB_LEFT,
  COMB_RIGHT,
  COMB_NOT,
  COMB_PEEK,
  COMB_GSEQ,
  COMB_BETWEEN,
  COMB_SEP_END_BY,
  COMB_CHAINL1,
  COMB_MAP,
  COMB_ERRMAP,
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

/** @brief Function signature implementing a combinator kind. */
typedef ParseResult (*comb_fn)(input_t *in, void *args, char *parser_name);

/**
 * @brief Parser-combinator handle.
 *
 * Combinators are first-class values: build with `seq`, `multi`,
 * `many`, … and invoke with @ref parse.  Lifetimes are managed by the
 * library; call @ref free_combinator on roots when finished.
 */
struct combinator_t {
  parser_type_t type;
  comb_fn fn;
  void *args;
  void *extra_to_free;
  char *name;
  size_t memo_id;
  bool cached; /**< If true, @ref free_combinator skips this combinator. */
};

/** @brief Callback signature for `flatMap` — returns the next combinator. */
typedef combinator_t *(*flatMap_func)(ast_t *ast);

/** @brief Callback signature for `map` — rewrites the AST on success. */
typedef ast_t *(*map_func)(ast_t *ast);

/** @brief Callback signature for `errmap` — rewrites the error on failure. */
typedef ParseError *(*err_map_func)(ParseError *err);

/** @brief Predicate used by `satisfy(predicate, tag)`. */
typedef bool (*char_predicate)(char);

/**
 * @brief Attach @p partial_ast to @p original_result if it was a failure.
 *
 * Lets error handlers surface what was successfully parsed before the
 * failure point.
 */
ParseResult wrap_failure_with_ast(input_t *in, char *message,
                                  ParseResult original_result,
                                  ast_t *partial_ast);

//=============================================================================
// Global Variables
//=============================================================================

/** @brief Singleton AST node used to represent "nothing" / empty productions. */
extern ast_t *ast_nil;

// --- Profiling & Diagnostics ---
/** @brief Aggregate parser-runtime counters; snapshot via @ref parser_stats_snapshot. */
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

/** @brief Zero the global parser statistics counters. */
void parser_stats_reset(void);

/** @brief Return a copy of the current parser statistics. */
parser_stats_t parser_stats_snapshot(void);

/** @brief Number of combinators currently allocated by the library. */
size_t parser_combinator_count(void);

/** @brief Mark @p comb as cached so @ref free_combinator leaves it alone. */
void combinator_mark_cached(combinator_t *comb);

/**
 * @brief Bias memoisation toward shorter-lived parsers.
 *
 * Lowers the call-count threshold that triggers memoisation, useful when
 * compiling small one-shot inputs.
 */
void parser_set_ephemeral_threshold(void);

/** @brief Dump a per-`parser_type_t` call-count profile labelled @p label. */
void parser_print_type_profile(const char *label);

/** @brief Clear the per-`parser_type_t` profile counters. */
void parser_reset_type_profile(void);

/** @brief Memoisation policy; see @ref parser_set_memo_mode. */
typedef enum {
  PARSER_MEMO_FULL,           /**< Cache successes and failures. */
  PARSER_MEMO_FAILURES_ONLY,  /**< Cache only failures (default for big grammars). */
  PARSER_MEMO_DISABLED        /**< Disable memoisation entirely (debugging). */
} parser_memo_mode_t;

/** @brief Change the memoisation policy for subsequent parses. */
void parser_set_memo_mode(parser_memo_mode_t mode);

/** @brief Per-combinator profile sample (one entry per named combinator). */
typedef struct parser_comb_stat {
  size_t memo_id;
  char *name;
  parser_type_t type;
  size_t calls;
  size_t successes;
  size_t failures;
  size_t failure_with_consumption;
  size_t total_failure_consumed;
  size_t max_failure_consumed;
  size_t total_success_consumed;
} parser_comb_stat_t;

/** @brief Toggle per-combinator profiling (off by default). */
void parser_comb_stats_set_enabled(bool enabled);

/** @brief Reset the per-combinator profile table. */
void parser_comb_stats_reset(void);

/**
 * @brief Snapshot the per-combinator profile.
 * @param[out] count  Number of entries returned.
 * @returns Library-owned array (do not free).
 */
const parser_comb_stat_t *parser_comb_stats_snapshot(size_t *count);

//=============================================================================
// Public Function Prototypes
//=============================================================================

// --- Core Parser Function ---
/**
 * @brief Run combinator @p comb over input @p in.
 *
 * Returns a @ref ParseResult that is either @c is_success with
 * @c value.ast or a failure with @c value.error.  The caller owns the
 * returned AST (free with @ref free_ast or the detached counterpart).
 */
ParseResult parse(input_t *in, combinator_t *comb);

// --- Primitive Parser Constructors ---
/** @brief Match the literal string @p str, case-sensitively. */
combinator_t *match(char *str);
/** @brief Match @p str case-insensitively (single-byte ASCII). */
combinator_t *match_ci(char *str);
/** @brief Match @p str verbatim with no whitespace skipping. */
combinator_t *match_raw(char *str);
/** @brief Parse a decimal integer literal, tagging the AST as @p tag. */
combinator_t *integer(tag_t tag);
/** @brief Parse a C-style identifier (alpha/underscore start), tag @p tag. */
combinator_t *cident(tag_t tag);
/** @brief Parse a double-quoted string literal, tag @p tag. */
combinator_t *string(tag_t tag);
/** @brief Consume until @p p matches (exclusive); AST contains the consumed run. */
combinator_t *until(combinator_t *p, tag_t tag);
/** @brief Match a single character, tag @p tag. */
combinator_t *any_char(tag_t tag);
/** @brief Match a single character satisfying @p pred. */
combinator_t *satisfy(char_predicate pred, tag_t tag);
/** @brief Match end-of-input. */
combinator_t *eoi();

// --- Combinator Constructors ---
/**
 * @brief Forward-reference combinator: dereferences @p parser_ptr at parse
 * time, enabling recursive grammars without circular ownership.
 */
combinator_t *lazy(combinator_t **parser_ptr);

/** @brief Like @ref lazy but takes ownership of the pointed-to combinator. */
combinator_t *lazy_owned(combinator_t **parser_ptr);

// --- Expression Parser Constructors ---
/** @brief Associativity / fixity of an expression operator. */
typedef enum { EXPR_BASE, EXPR_INFIX, EXPR_PREFIX, EXPR_POSTFIX } expr_fix;

/** @brief Operator associativity for ambiguous-precedence resolution. */
typedef enum { ASSOC_LEFT, ASSOC_RIGHT, ASSOC_NONE } expr_assoc;

/**
 * @brief Build an operator-precedence expression parser around @p base.
 *
 * @p exp is a stub combinator from @ref new_combinator that will be
 * populated by @ref expr_insert / @ref expr_altern calls.  @p base
 * matches atomic operands (literals, identifiers, …).
 */
combinator_t *expr(combinator_t *exp, combinator_t *base);

/**
 * @brief Register an operator at precedence @p prec.
 *
 * @p exp is the expression combinator built via @ref expr.  @p prec is
 * the precedence band (higher binds tighter).  @p tag is the AST tag
 * to emit when this operator fires.  @p fix selects the operator's
 * position (prefix / infix / postfix).  @p assoc is the associativity
 * for resolving same-precedence chains.  @p comb matches the operator
 * token itself.
 */
void expr_insert(combinator_t *exp, int prec, tag_t tag, expr_fix fix,
                 expr_assoc assoc, combinator_t *comb);

/** @brief Add an extra operator alternative at the existing band @p prec. */
void expr_altern(combinator_t *exp, int prec, tag_t tag, combinator_t *comb);

// --- Input Stream Helpers ---
/** @brief Allocate a fresh @ref input_t.  Never returns NULL. */
#if defined(__GNUC__) || defined(__clang__)
__attribute__((returns_nonnull))
#endif
input_t *new_input();

/** @brief Free @p in; the @c buffer field is *not* freed by this call. */
void free_input(input_t *in);

/** @brief Peek-and-consume one byte from @p in, updating line/col tracking. */
char read1(input_t *in);

/** @brief Copy the current input position into @p ast (line / col / index). */
void set_ast_position(ast_t *ast, input_t *in);

/** @brief Bind @p buffer (@p length bytes) into @p in and reset position to 0. */
void init_input_buffer(input_t *in, char *buffer, int length);

// --- AST Helpers ---
/** @brief Visitor callback passed to @ref parser_walk_ast. */
typedef void (*ast_visitor_fn)(ast_t *node, void *context);

/** @brief Pre-order walk over @p ast invoking @p visitor on each node. */
void parser_walk_ast(ast_t *ast, ast_visitor_fn visitor, void *context);

/** @brief Allocate a zeroed AST node from the parser's recycler / pool. */
ast_t *new_ast();

/** @brief Return @p ast and its subtree to the parser pool. */
void free_ast(ast_t *ast);

/** @brief Release @p err and its @c cause chain. */
void free_error(ParseError *err);

/**
 * @brief Lazily format @p err->message if @c format_arg is set.
 * @returns Pointer owned by @p err; valid until the next call.
 */
const char *parse_error_get_message(ParseError *err);

/**
 * @brief Fill in @c context / @c source_filename on @p err and its causes.
 *
 * Expensive; deferred until the caller decides to actually report the
 * error so cheap probing in `multi`/`optional` doesn't pay the cost.
 */
void ensure_parse_error_contexts(ParseError *err, input_t *in);

/** @brief Build a node with tag @p typ and single child @p a1. */
ast_t *ast1(tag_t typ, ast_t *a1);

/** @brief Build a node with tag @p typ; @p a1 is child, @p a2 is its sibling. */
ast_t *ast2(tag_t typ, ast_t *a1, ast_t *a2);

/** @brief Shallow-copy @p orig into a fresh pool node (children share pointers). */
ast_t *copy_ast(ast_t *orig);

/**
 * @brief Deep-copy an AST tree into standalone (malloc'd) memory,
 * independent of the parser's AST node pool.  Must be freed with
 * @ref free_ast_detached.
 */
ast_t *copy_ast_detached(ast_t *orig);

/** @brief Free a tree allocated by @ref copy_ast_detached (uses free(), not pool). */
void free_ast_detached(ast_t *ast);

/** @brief Convert a buffer @p index back to 1-based line/column. */
void parser_calculate_line_col(input_t *in, int index, int *out_line,
                               int *out_col);

/**
 * @brief Build a one-line context snippet (caret pointing at column).
 * @returns Malloc'd string owned by the caller.
 */
char *parser_format_context(input_t *in, int line, int col, int index);

// --- Combinator Helpers ---
/**
 * @brief Allocate a zeroed combinator stub for later configuration.
 *
 * Used as the first arg to @c seq/@c multi/@c expr so the call returns
 * the same combinator address as the stub (enables forward references).
 */
combinator_t *new_combinator();

// --- Extensibility Helpers ---
/** @brief Snapshot of input position for save / restore / backtrack. */
typedef struct {
  int start;
  int line;
  int col;
} InputState;

/** @brief Capture current position of @p in into @p state. */
void save_input_state(input_t *in, InputState *state);

/** @brief Restore @p in to a previously captured @p state (backtrack). */
void restore_input_state(input_t *in, InputState *state);

/** @brief Construct a successful @ref ParseResult holding @p ast. */
ParseResult make_success(ast_t *ast);

/** @brief Construct a failure with a malloc'd @p message. */
ParseResult make_failure(input_t *in, char *message);

/** @brief Like @ref make_failure but carries parser-name and unexpected-token. */
ParseResult make_failure_v2(input_t *in, char *parser_name, char *message,
                            char *unexpected);

/** @brief Failure variant where @p message is a static string (not freed). */
ParseResult make_failure_static(input_t *in, const char *message);

/** @brief Re-fail with a higher-level message, chaining @p cause via @c cause. */
ParseResult wrap_failure(input_t *in, char *message, char *parser_name,
                         ParseResult cause);

/** @brief Dispatch arguments for the for-init combinator (assignment | bare-id). */
typedef struct for_init_dispatch_args {
  combinator_t *assignment_parser;
  combinator_t *identifier_parser;
} for_init_dispatch_args_t;

/** @brief One row of the keyword-dispatch table; pre-hashed by keyword. */
typedef struct pascal_keyword_entry {
  const char *keyword;
  size_t length;
  combinator_t *parser;
} pascal_keyword_entry_t;

/** @brief Dispatch arguments for the statement combinator. */
typedef struct statement_dispatch_args {
  combinator_t **keyword_parsers;
  size_t keyword_count;
  combinator_t *label_parser;
  combinator_t *assignment_parser;
  combinator_t *expr_parser;
  combinator_t *on_handler_parser;
} statement_dispatch_args_t;

/** @brief Argument bundle for the lvalue combinator. */
typedef struct expr_lvalue_args {
  combinator_t *expr_parser;
} expr_lvalue_args;

/** @brief Dispatch arguments for class-body member parsing. */
typedef struct class_member_dispatch_args {
  combinator_t *constructor_parser;
  combinator_t *destructor_parser;
  combinator_t *procedure_parser;
  combinator_t *function_parser;
  combinator_t *operator_parser;
  combinator_t *property_parser;
  combinator_t *field_parser;
} class_member_dispatch_args_t;

/** @brief Generic keyword dispatcher: pick a parser by leading keyword. */
typedef struct keyword_dispatch_args {
  pascal_keyword_entry_t *entries;
  size_t entry_count;
  const char **skip_keywords;
  size_t skip_keyword_count;
  combinator_t *fallback_parser;
} keyword_dispatch_args_t;

/** @brief Dispatch arguments for type-expression parsing. */
typedef struct type_dispatch_args {
  combinator_t *helper_parser;
  combinator_t *reference_parser;
  combinator_t *interface_parser;
  combinator_t *class_parser;
  combinator_t *class_of_parser;       /**< For `class of <typename>` class reference type. */
  combinator_t *record_parser;
  combinator_t *object_parser;         /**< Legacy `object` (record-with-methods). */
  combinator_t *enumerated_parser;
  combinator_t *array_parser;
  combinator_t *file_parser;
  combinator_t *set_parser;
  combinator_t *range_parser;
  combinator_t *pointer_parser;
  combinator_t *specialize_parser;
  combinator_t *constructed_parser;
  combinator_t *identifier_parser;
  combinator_t *distinct_type_parser;       /**< `type <typename>` distinct-type syntax. */
  combinator_t *distinct_type_range_parser; /**< `type <range>` distinct from subrange. */
  combinator_t *procedure_parser;           /**< Traditional procedure type (no `reference to`). */
  combinator_t *function_parser;            /**< Traditional function type. */
} type_dispatch_args_t;

/** @brief Argument bundle for the top-level program/unit `main begin..end` parser. */
typedef struct main_block_args {
  combinator_t **stmt_parser;
} main_block_args_t;

// --- Helper Function Prototypes ---
/**
 * @brief malloc() that aborts on out-of-memory.  Never returns NULL.
 *
 * Used pervasively in cparser: failure to allocate is treated as a
 * fatal compiler error, not a recoverable condition.
 */
#if defined(__GNUC__) || defined(__clang__)
__attribute__((returns_nonnull, malloc))
#endif
void *safe_malloc(size_t size);

/**
 * @brief Intern @p name into the global symbol table.
 * @returns Library-owned @ref sym_t* such that two equal names share
 * one pointer (enables pointer-equality identity checks).
 */
sym_t *sym_lookup(const char *name);

// --- Memory Management ---
/** @brief Free a single combinator (not its referenced children). */
void free_combinator(combinator_t *comb);

/**
 * @brief Free a whole combinator graph reachable from @p roots.
 *
 * Walks the graph collecting every reachable combinator exactly once,
 * then frees them; safe for cyclic graphs (e.g. via @ref lazy).
 */
void free_combinator_graph(combinator_t **roots, size_t count);

/** @brief Drain the combinator free-list (called at shutdown). */
void parser_drain_free_list(void);

/** @brief Drain the AST node pool's free-list (called at shutdown). */
void parser_drain_ast_free_list(void);

/** @brief Drain the @ref ParseError pool's free-list (called at shutdown). */
void parser_drain_error_free_list(void);

/** @brief Print @p err to stderr and `abort()`.  Used for unrecoverable errors. */
void exception(const char *err);

#endif // PARSER_H
