#ifndef COMBINATORS_H
#define COMBINATORS_H

#include "parser.h"

//=============================================================================
// Combinator Constructors
//=============================================================================

combinator_t * expect(combinator_t * c, char * msg);
combinator_t * many(combinator_t* p);
combinator_t * optional(combinator_t* p);
combinator_t * sep_by(combinator_t* p, combinator_t* sep);
combinator_t * sep_by1(combinator_t* p, combinator_t* sep);
combinator_t * seq(combinator_t * ret, tag_t typ, combinator_t * c1, ...);
combinator_t * multi(combinator_t * ret, tag_t typ, combinator_t * c1, ...);
combinator_t * flatMap(combinator_t * p, flatMap_func func);
combinator_t * left(combinator_t* p1, combinator_t* p2);
combinator_t * right(combinator_t* p1, combinator_t* p2);
combinator_t * pnot(combinator_t* p);
combinator_t * peek(combinator_t* p);
combinator_t * gseq(combinator_t * ret, tag_t typ, combinator_t * c1, ...);
combinator_t * between(combinator_t* open, combinator_t* close, combinator_t* p);
combinator_t * sep_end_by(combinator_t* p, combinator_t* sep);
combinator_t * chainl1(combinator_t* p, combinator_t* op);
combinator_t * succeed(ast_t* ast);
combinator_t * map(combinator_t* p, map_func func);
combinator_t * errmap(combinator_t* p, err_map_func func);
combinator_t * commit(combinator_t* p);

#endif // COMBINATORS_H
