#ifndef TYPE_TAGS_H
#define TYPE_TAGS_H

/*
 * Token constants required by the legacy semantic analyser and code generator.
 * These were previously provided by the bison-generated Grammar.tab.h but the
 * cparser integration no longer emits that header.  The numeric values only
 * need to be unique and stable within the compiler so we define them manually
 * here.
 */

#define UNKNOWN_TYPE        0
#define INT_TYPE            1
#define REAL_TYPE           2
#define LONGINT_TYPE        3
#define STRING_TYPE         4
#define BUILTIN_ANY_TYPE    5

/* Legacy token constants reused by the semantic analyser and code generator */
#define BOOL                6
#define PROCEDURE           7

#define EQ                  8
#define NE                  9
#define LT                  10
#define LE                  11
#define GT                  12
#define GE                  13

#define AND                 14
#define OR                  15
#define NOT                 16

#define PLUS                17
#define MINUS               18
#define STAR                19
#define SLASH               20
#define DIV                 21
#define MOD                 22

/* Bitwise operators */
#define XOR                 23
#define SHL                 24
#define SHR                 25
#define ROL                 26
#define ROR                 27

#endif /* TYPE_TAGS_H */
