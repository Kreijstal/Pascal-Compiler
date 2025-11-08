#ifndef PASCAL_PEEK_H
#define PASCAL_PEEK_H

#include "parser.h"

typedef struct pascal_word_slice {
    const char* start;
    size_t length;
    int start_pos;
    int end_pos;
} pascal_word_slice_t;

int skip_pascal_layout_preview(const input_t* in, int pos);
bool pascal_peek_word(const input_t* in, pascal_word_slice_t* slice);
bool pascal_peek_word_after(const input_t* in, int pos, pascal_word_slice_t* slice);
bool pascal_word_equals_ci(const pascal_word_slice_t* slice, const char* keyword);

#endif
