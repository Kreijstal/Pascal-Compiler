/**
 * @file combinators.h
 * @brief Higher-order parser combinator constructors.
 *
 * Each function here returns a freshly allocated @ref combinator_t that
 * composes its arguments.  Sentinel-terminated variadics
 * (@ref seq, @ref multi, @ref gseq) take a `(combinator_t *)NULL` to mark
 * the end of the list.  See `cparser/ARCHITECTURE.md` in the repo root
 * for the combinator catalog and design notes.
 */
#ifndef COMBINATORS_H
#define COMBINATORS_H

#include "parser.h"

//=============================================================================
// Combinator Constructors
//=============================================================================

/**
 * @brief Replace @p c's failure message with @p msg.
 * @returns Combinator that runs @p c; on failure substitutes @p msg.
 */
combinator_t *expect(combinator_t *c, char *msg);

/**
 * @brief Zero-or-more repetition of @p p (`p*`).
 *
 * Always succeeds.  Successive matches become AST siblings.
 */
combinator_t *many(combinator_t *p);

/** @brief Run @p p once; succeed with @ref ast_nil if @p p fails. */
combinator_t *optional(combinator_t *p);

/**
 * @brief One-or-more @p p separated by @p sep (matches `p (sep p)*`).
 * Trailing @p sep is NOT consumed.
 */
combinator_t *sep_by(combinator_t *p, combinator_t *sep);

/** @brief Like @ref sep_by but requires at least one @p p. */
combinator_t *sep_by1(combinator_t *p, combinator_t *sep);

/**
 * @brief Sequence: run each combinator in order; emit AST with tag @p typ
 *        and one child per matched element.
 *
 * Variadic arguments are @ref combinator_t pointers, terminated with
 * `(combinator_t *)NULL`.
 *
 * @param ret  Stub combinator from @ref new_combinator (return value is @p ret).
 * @param typ  Tag for the emitted AST node.
 * @param c1   First parser in the sequence.
 */
combinator_t *seq(combinator_t *ret, tag_t typ, combinator_t *c1, ...);

/**
 * @brief Ordered alternation: try each combinator until one succeeds.
 *
 * Backtracks on failure unless an inner @ref commit fires.  Variadics
 * end with `(combinator_t *)NULL`.
 */
combinator_t *multi(combinator_t *ret, tag_t typ, combinator_t *c1, ...);

/** @brief Run @p p, then run the combinator returned by @p func over its AST. */
combinator_t *flatMap(combinator_t *p, flatMap_func func);

/** @brief Run @p p1 then @p p2; the AST of the whole is @p p1's AST. */
combinator_t *left(combinator_t *p1, combinator_t *p2);

/** @brief Run @p p1 then @p p2; the AST of the whole is @p p2's AST. */
combinator_t *right(combinator_t *p1, combinator_t *p2);

/** @brief Negative lookahead: succeed iff @p p fails; no input consumed. */
combinator_t *pnot(combinator_t *p);

/** @brief Positive lookahead: succeed iff @p p succeeds; no input consumed. */
combinator_t *peek(combinator_t *p);

/**
 * @brief Sequence variant whose AST inlines each child's children (`gseq` = "graft seq").
 *
 * Useful for productions where the child node is a structural wrapper
 * the consumer wants to skip past.  Variadics end with `(combinator_t *)NULL`.
 */
combinator_t *gseq(combinator_t *ret, tag_t typ, combinator_t *c1, ...);

/**
 * @brief Match `open p close`; emit just @p p's AST.
 *
 * @p open and @p close are typically delimiter literals (`(`, `)` …).
 */
combinator_t *between(combinator_t *open, combinator_t *close, combinator_t *p);

/** @brief Like @ref sep_by but allows (and silently consumes) a trailing @p sep. */
combinator_t *sep_end_by(combinator_t *p, combinator_t *sep);

/**
 * @brief Left-associative binary operator chain: `p (op p)*` folded left.
 *
 * @p op should yield an AST node whose two children are the operands.
 */
combinator_t *chainl1(combinator_t *p, combinator_t *op);

/** @brief Combinator that always succeeds, emitting @p ast.  Consumes no input. */
combinator_t *succeed(ast_t *ast);

/** @brief Apply @p func to @p p's AST on success (post-process). */
combinator_t *map(combinator_t *p, map_func func);

/** @brief Apply @p func to @p p's error on failure (post-process diagnostics). */
combinator_t *errmap(combinator_t *p, err_map_func func);

/**
 * @brief Mark @p p as a commit point: once @p p starts consuming, an
 * enclosing @ref multi will not try alternatives even if @p p fails.
 */
combinator_t *commit(combinator_t *p);

#endif // COMBINATORS_H
