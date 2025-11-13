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
#define RECORD_TYPE         34
#define ARRAY_OF_CONST_TYPE 35
#define CHAR_TYPE           28
#define POINTER_TYPE        29
#define SET_TYPE            30
#define ENUM_TYPE           31
#define FILE_TYPE           32

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

/* Set membership */
#define IN                  33

/*
 * Type classification utilities
 */

/**
 * Check if a type tag represents an ordinal type in Pascal.
 * Ordinal types are types with a finite set of ordered values and include:
 * - Integer types (INT_TYPE, LONGINT_TYPE)
 * - Enumerated types (ENUM_TYPE)
 * - Character type (CHAR_TYPE)
 * - Boolean type (BOOL)
 * 
 * Ordinal types can be used as:
 * - Array index types
 * - Array bound types
 * - Loop control variables
 * - Case statement selectors
 * - Set base types
 */
static inline int is_ordinal_type(int type_tag)
{
    return (type_tag == INT_TYPE || type_tag == LONGINT_TYPE ||
            type_tag == ENUM_TYPE || type_tag == CHAR_TYPE || type_tag == BOOL);
}

#endif /* TYPE_TAGS_H */
