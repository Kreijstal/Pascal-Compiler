#include "pascal_peek.h"
#include <ctype.h>
#include <string.h>

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
            pos++;
            while (pos < length && buffer[pos] != '}') {
                pos++;
            }
            if (pos < length) pos++;
            continue;
        }
        if (ch == '(' && (pos + 1) < length && buffer[pos + 1] == '*') {
            pos += 2;
            while ((pos + 1) < length && !(buffer[pos] == '*' && buffer[pos + 1] == ')')) {
                pos++;
            }
            if ((pos + 1) < length) {
                pos += 2;
            } else {
                pos = length;
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
