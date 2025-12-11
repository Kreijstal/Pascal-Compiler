#include "pascal_peek.h"
#include <ctype.h>
#include <string.h>
#include <strings.h>

int skip_pascal_layout_preview(const input_t* in, int pos) {
    if (in == NULL || in->buffer == NULL) {
        return pos;
    }
    const char* buffer = in->buffer;
    int length = in->length;
    if (length <= 0) {
        length = (int)strlen(buffer);
    }
    while (pos < length) {
        unsigned char ch = (unsigned char)buffer[pos];
        if (isspace(ch)) {
            pos++;
            continue;
        }
        if (ch == '{') {
            int depth = 1;
            pos++;
            while (pos < length && depth > 0) {
                if (buffer[pos] == '{') {
                    depth++;
                    pos++;
                    continue;
                }
                if (buffer[pos] == '}') {
                    depth--;
                    pos++;
                    continue;
                }
                pos++;
            }
            continue;
        }
        if (ch == '(' && (pos + 1) < length && buffer[pos + 1] == '*') {
            int depth = 1;
            pos += 2;
            while (pos < length && depth > 0) {
                if (buffer[pos] == '(' && (pos + 1) < length && buffer[pos + 1] == '*') {
                    depth++;
                    pos += 2;
                    continue;
                }
                if (buffer[pos] == '*' && (pos + 1) < length && buffer[pos + 1] == ')') {
                    depth--;
                    pos += 2;
                    continue;
                }
                pos++;
            }
            continue;
        }
        if (ch == '/' && (pos + 1) < length && buffer[pos + 1] == '/') {
            pos += 2;
            while (pos < length && buffer[pos] != '\n' && buffer[pos] != '\r') {
                pos++;
            }
            continue;
        }
        break;
    }
    return pos;
}

static int resolve_input_length(const input_t* in) {
    if (in == NULL) {
        return 0;
    }
    if (in->length > 0) {
        return in->length;
    }
    if (in->buffer == NULL) {
        return 0;
    }
    return (int)strlen(in->buffer);
}

static bool read_word_at(const input_t* in, int pos, pascal_word_slice_t* slice) {
    if (in == NULL || in->buffer == NULL || slice == NULL) {
        return false;
    }
    int length = resolve_input_length(in);
    if (pos >= length) {
        return false;
    }
    const char* buffer = in->buffer;
    unsigned char ch = (unsigned char)buffer[pos];
    if (!(isalpha(ch) || ch == '_')) {
        return false;
    }
    int start = pos;
    pos++;
    while (pos < length) {
        unsigned char next = (unsigned char)buffer[pos];
        if (!(isalnum(next) || next == '_')) {
            break;
        }
        pos++;
    }
    slice->start = buffer + start;
    slice->length = (size_t)(pos - start);
    slice->start_pos = start;
    slice->end_pos = pos;
    return true;
}

bool pascal_peek_word(const input_t* in, pascal_word_slice_t* slice) {
    if (in == NULL) {
        return false;
    }
    return pascal_peek_word_after(in, in->start, slice);
}

bool pascal_peek_word_after(const input_t* in, int pos, pascal_word_slice_t* slice) {
    if (in == NULL || slice == NULL) {
        return false;
    }
    pos = skip_pascal_layout_preview(in, pos);
    return read_word_at(in, pos, slice);
}

bool pascal_word_equals_ci(const pascal_word_slice_t* slice, const char* keyword) {
    if (slice == NULL || slice->start == NULL || keyword == NULL) {
        return false;
    }
    size_t keyword_len = strlen(keyword);
    if (keyword_len != slice->length) {
        return false;
    }
    return strncasecmp(slice->start, keyword, keyword_len) == 0;
}
