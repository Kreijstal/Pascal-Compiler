#ifndef PASCAL_STATEMENT_KEYWORD_TYPES_H
#define PASCAL_STATEMENT_KEYWORD_TYPES_H

typedef enum statement_keyword_id {
    STMT_KW_BEGIN = 0,
    STMT_KW_GOTO,
    STMT_KW_TRY,
    STMT_KW_CASE,
    STMT_KW_RAISE,
    STMT_KW_INHERITED,
    STMT_KW_BREAK,
    STMT_KW_EXIT,
    STMT_KW_ASM,
    STMT_KW_IF,
    STMT_KW_FOR,
    STMT_KW_REPEAT,
    STMT_KW_WHILE,
    STMT_KW_WITH,
    STMT_KW_COUNT
} statement_keyword_id;

struct statement_keyword_record {
    const char* name;
    statement_keyword_id id;
};

#endif
