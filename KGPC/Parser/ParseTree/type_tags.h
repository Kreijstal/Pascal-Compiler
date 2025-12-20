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
#define LONGINT_TYPE        3   /* 32-bit signed integer (FPC-compatible LongInt) */
#define STRING_TYPE         4
#define BUILTIN_ANY_TYPE    5
#define INT64_TYPE          38  /* 64-bit signed integer (Int64, TDateTime) */
#define RECORD_TYPE         34
#define ARRAY_OF_CONST_TYPE 35
#define TEXT_TYPE           36
#define CHAR_TYPE           28
#define POINTER_TYPE        29
#define SET_TYPE            30
#define ENUM_TYPE           31
#define FILE_TYPE           32
#define SHORTSTRING_TYPE    37

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
    return (type_tag == INT_TYPE || type_tag == LONGINT_TYPE || type_tag == INT64_TYPE ||
            type_tag == ENUM_TYPE || type_tag == CHAR_TYPE || type_tag == BOOL);
}

/**
 * Check if a type tag represents an integer type.
 * Integer types include:
 * - INT_TYPE (32-bit Integer)
 * - LONGINT_TYPE (32-bit LongInt for FPC compatibility)
 * - INT64_TYPE (64-bit Int64)
 */
static inline int is_integer_type(int type_tag)
{
    return (type_tag == INT_TYPE || type_tag == LONGINT_TYPE || type_tag == INT64_TYPE);
}

/**
 * Check if a type tag represents a string type.
 * String types include:
 * - STRING_TYPE (dynamic string/AnsiString)
 * - SHORTSTRING_TYPE (fixed-length short strings)
 */
static inline int is_string_type(int type_tag)
{
    return (type_tag == STRING_TYPE || type_tag == SHORTSTRING_TYPE);
}

/**
 * Check if an expression represents a shortstring (array of char).
 * ShortString variables are stored as array[0..255] of char internally,
 * and resolve to CHAR_TYPE as the element type.
 * 
 * @param type_tag The resolved type tag of the expression
 * @param is_array_expr Whether the expression is marked as an array expression
 * @return 1 if this represents a shortstring/char array, 0 otherwise
 */
static inline int is_shortstring_array(int type_tag, int is_array_expr)
{
    return is_array_expr && type_tag == CHAR_TYPE;
}

#endif /* TYPE_TAGS_H */
