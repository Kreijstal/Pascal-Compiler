#ifndef JSON_PARSER_H
#define JSON_PARSER_H

#include "parser.h"

// --- Custom Tags for JSON ---
typedef enum {
    JSON_T_NONE, JSON_T_STRING, JSON_T_INT, JSON_T_ASSIGN, JSON_T_SEQ
} json_tag_t;

// Returns a combinator that can parse a complete JSON value.
combinator_t* json_parser();

#endif // JSON_PARSER_H
