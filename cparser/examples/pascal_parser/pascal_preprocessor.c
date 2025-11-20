#include "pascal_preprocessor.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INCLUDE_DEPTH 32

typedef struct {
    char *name;
    char *value;
} DefineEntry;

struct PascalPreprocessor {
    DefineEntry *defines;
    size_t define_count;
    size_t define_capacity;
};

typedef struct {
    bool parent_allows;
    bool branch_taken;
    bool active;
    bool saw_else;
} ConditionalFrame;

typedef struct {
    ConditionalFrame *items;
    size_t size;
    size_t capacity;
} ConditionalStack;

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} StringBuilder;

static int ascii_tolower(int c);
static int ascii_strncasecmp(const char *a, const char *b, size_t n);
static bool set_error(char **error_message, const char *fmt, ...);
static void string_builder_init(StringBuilder *sb);
static bool string_builder_append_char(StringBuilder *sb, char c);
static void string_builder_free(StringBuilder *sb);
static bool ensure_capacity(void **buffer, size_t element_size, size_t *capacity, size_t needed);
static bool push_conditional(ConditionalStack *stack, bool parent_active, bool condition);
static bool pop_conditional(ConditionalStack *stack);
static ConditionalFrame *peek_conditional(ConditionalStack *stack);
static bool current_branch_active(ConditionalStack *stack);
static void activate_fallback_branch(ConditionalFrame *frame);
static bool handle_directive(PascalPreprocessor *pp,
                             const char *filename,
                             const char *input,
                             size_t length,
                             size_t *index,
                             bool paren_style,
                             ConditionalStack *conditions,
                             StringBuilder *output,
                             int depth,
                             char **error_message);
static bool preprocess_buffer_internal(PascalPreprocessor *pp,
                                       const char *filename,
                                       const char *input,
                                       size_t length,
                                       ConditionalStack *conditions,
                                       StringBuilder *output,
                                       int depth,
                                       char **error_message);
static bool read_file_contents(const char *filename, char **buffer, size_t *length, char **error_message);
static bool define_symbol(PascalPreprocessor *pp, const char *symbol);
static bool undefine_symbol(PascalPreprocessor *pp, const char *symbol);
static bool symbol_is_defined(const PascalPreprocessor *pp, const char *symbol);
static void trim(char **begin, char **end);
static char *duplicate_range(const char *start, const char *end);
static void extract_directory(const char *filename, char *buffer, size_t buffer_size);
static bool resolve_include_path(const char *current_file, const char *directive_path, char **result_path);
static bool parse_identifier(const char *start, const char *end, char **out_identifier);
static void uppercase(char *str);
static bool evaluate_if_directive(PascalPreprocessor *pp,
                                  const char *expression,
                                  bool *result,
                                  char **error_message);

// Expression parser functions returning int64_t
static bool parse_expression(const char **cursor,
                             int64_t *value,
                             PascalPreprocessor *pp,
                             char **error_message);
static bool parse_simple_expression(const char **cursor,
                                    int64_t *value,
                                    PascalPreprocessor *pp,
                                    char **error_message);
static bool parse_term(const char **cursor,
                       int64_t *value,
                       PascalPreprocessor *pp,
                       char **error_message);
static bool parse_factor(const char **cursor,
                         int64_t *value,
                         PascalPreprocessor *pp,
                         char **error_message);
static const char *get_symbol_value(const PascalPreprocessor *pp, const char *symbol);

PascalPreprocessor *pascal_preprocessor_create(void) {
    PascalPreprocessor *pp = calloc(1, sizeof(*pp));
    if (!pp) {
        return NULL;
    }
    return pp;
}

void pascal_preprocessor_free(PascalPreprocessor *pp) {
    if (!pp) {
        return;
    }
    for (size_t i = 0; i < pp->define_count; ++i) {
        free(pp->defines[i].name);
        free(pp->defines[i].value);
    }
    free(pp->defines);
    free(pp);
}

bool pascal_preprocessor_define(PascalPreprocessor *pp, const char *symbol) {
    return define_symbol(pp, symbol);
}

bool pascal_preprocessor_undefine(PascalPreprocessor *pp, const char *symbol) {
    return undefine_symbol(pp, symbol);
}

bool pascal_preprocessor_is_defined(const PascalPreprocessor *pp, const char *symbol) {
    return symbol_is_defined(pp, symbol);
}

char *pascal_preprocess_buffer(PascalPreprocessor *pp,
                               const char *filename,
                               const char *input,
                               size_t length,
                               size_t *out_length,
                               char **error_message) {
    if (!pp || !input) {
        set_error(error_message, "invalid input to preprocessor");
        return NULL;
    }

    ConditionalStack conditions = {0};
    StringBuilder output;
    string_builder_init(&output);

    if (!preprocess_buffer_internal(pp, filename, input, length, &conditions, &output, 0, error_message)) {
        string_builder_free(&output);
        free(conditions.items);
        return NULL;
    }

    if (conditions.size != 0) {
        set_error(error_message, "unterminated conditional in '%s'", filename ? filename : "<buffer>");
        string_builder_free(&output);
        free(conditions.items);
        return NULL;
    }

    if (!output.data) {
        output.data = malloc(1);
        if (!output.data) {
            string_builder_free(&output);
            free(conditions.items);
            set_error(error_message, "out of memory");
            return NULL;
        }
        output.data[0] = '\0';
    }
    if (out_length) {
        *out_length = output.length;
    }
    char *result = output.data;
    free(conditions.items);
    return result;
}

char *pascal_preprocess_file(PascalPreprocessor *pp,
                             const char *filename,
                             size_t *out_length,
                             char **error_message) {
    if (!filename) {
        set_error(error_message, "no filename provided");
        return NULL;
    }
    char *buffer = NULL;
    size_t length = 0;
    if (!read_file_contents(filename, &buffer, &length, error_message)) {
        free(buffer);
        return NULL;
    }
    char *result = pascal_preprocess_buffer(pp, filename, buffer, length, out_length, error_message);
    free(buffer);
    return result;
}

static bool preprocess_buffer_internal(PascalPreprocessor *pp,
                                       const char *filename,
                                       const char *input,
                                       size_t length,
                                       ConditionalStack *conditions,
                                       StringBuilder *output,
                                       int depth,
                                       char **error_message) {
    if (depth > MAX_INCLUDE_DEPTH) {
        return set_error(error_message, "maximum include depth exceeded while processing '%s'", filename ? filename : "<buffer>");
    }

    bool in_string = false;
    char string_delim = '\0';
    bool in_brace_comment = false;
    bool in_paren_comment = false;
    bool in_line_comment = false;

    for (size_t i = 0; i < length; ++i) {
        char c = input[i];

        bool in_comment = in_brace_comment || in_paren_comment || in_line_comment;

        if (!in_string && !in_comment) {
            if (c == '{' && i + 1 < length && input[i + 1] == '$') {
                if (!handle_directive(pp, filename, input, length, &i, false, conditions, output, depth, error_message)) {
                    return false;
                }
                continue;
            }
            if (c == '(' && i + 2 < length && input[i + 1] == '*' && input[i + 2] == '$') {
                if (!handle_directive(pp, filename, input, length, &i, true, conditions, output, depth, error_message)) {
                    return false;
                }
                continue;
            }
        }

        if (in_line_comment) {
            if (c == '\n') {
                in_line_comment = false;
            } else if (c == '\r' && i + 1 < length && input[i + 1] == '\n') {
                in_line_comment = false;
                if (current_branch_active(conditions)) {
                    if (!string_builder_append_char(output, c)) {
                        return set_error(error_message, "out of memory");
                    }
                    if (!string_builder_append_char(output, '\n')) {
                        return set_error(error_message, "out of memory");
                    }
                } else {
                    if (!string_builder_append_char(output, '\n')) {
                        return set_error(error_message, "out of memory");
                    }
                }
                ++i;
                continue;
            }
        } else if (in_brace_comment) {
            if (c == '}') {
                in_brace_comment = false;
            }
        } else if (in_paren_comment) {
            if (c == '*' && i + 1 < length && input[i + 1] == ')') {
                in_paren_comment = false;
            }
        } else if (in_string) {
            if (c == string_delim) {
                if (string_delim == '\'' && i + 1 < length && input[i + 1] == '\'') {
                    // Escaped single quote in Pascal string literal
                    if (current_branch_active(conditions)) {
                        if (!string_builder_append_char(output, c)) {
                            return set_error(error_message, "out of memory");
                        }
                    }
                    ++i;
                    c = input[i];
                } else {
                    in_string = false;
                    string_delim = '\0';
                }
            }
        } else {
            if (c == '{') {
                in_brace_comment = true;
            } else if (c == '(' && i + 1 < length && input[i + 1] == '*') {
                in_paren_comment = true;
            } else if (c == '/' && i + 1 < length && input[i + 1] == '/') {
                in_line_comment = true;
            } else if (c == '\'' || c == '"') {
                in_string = true;
                string_delim = c;
            }
        }

        if (current_branch_active(conditions)) {
            if (!string_builder_append_char(output, c)) {
                return set_error(error_message, "out of memory");
            }
        } else if (c == '\n') {
            if (!string_builder_append_char(output, '\n')) {
                return set_error(error_message, "out of memory");
            }
        }
    }

    return true;
}

static bool handle_directive(PascalPreprocessor *pp,
                             const char *filename,
                             const char *input,
                             size_t length,
                             size_t *index,
                             bool paren_style,
                             ConditionalStack *conditions,
                             StringBuilder *output,
                             int depth,
                             char **error_message) {
    size_t start = *index;
    size_t cursor = start + (paren_style ? 3 : 2);
    size_t end = cursor;
    bool closed = false;

    while (end < length) {
        if (!paren_style && input[end] == '}') {
            closed = true;
            break;
        }
        if (paren_style && end + 1 < length && input[end] == '*' && input[end + 1] == ')') {
            closed = true;
            break;
        }
        ++end;
    }

    if (!closed) {
        return set_error(error_message, "unterminated directive in '%s'", filename ? filename : "<buffer>");
    }

    char *content = duplicate_range(&input[cursor], &input[end]);
    if (!content) {
        return set_error(error_message, "out of memory");
    }

    char *content_begin = content;
    char *content_end = content + strlen(content);
    trim(&content_begin, &content_end);

    char *keyword = NULL;
    if (!parse_identifier(content_begin, content_end, &keyword)) {
        free(content);
        return set_error(error_message, "malformed compiler directive in '%s'", filename ? filename : "<buffer>");
    }
    uppercase(keyword);

    const char *rest = content_begin + strlen(keyword);
    while (*rest && isspace((unsigned char)*rest)) {
        ++rest;
    }

    bool branch_active = current_branch_active(conditions);
    bool handled = false;
    bool is_io_check_directive = false;

    if (strcmp(keyword, "I") == 0) {
        const char *cursor = rest;
        while (*cursor && isspace((unsigned char)*cursor)) {
            ++cursor;
        }
        if (*cursor == '+' || *cursor == '-') {
            const char *after = cursor + 1;
            while (*after && isspace((unsigned char)*after)) {
                ++after;
            }
            if (*after == '\0') {
                is_io_check_directive = true;
            }
        }
    }

    if (is_io_check_directive) {
        handled = true;
        // {$I+} / {$I-} toggles I/O-checking; the compiler currently does not
        // distinguish the modes, so treat it as a no-op directive.
    } else if (strcmp(keyword, "I") == 0 || strcmp(keyword, "INCLUDE") == 0) {
        handled = true;
        if (!branch_active) {
            // Skip include in inactive branch
        } else {
            char *path_token = NULL;
            const char *rest_cursor = rest;
            if (*rest_cursor == '\'' || *rest_cursor == '"') {
                char quote = *rest_cursor++;
                const char *path_start = rest_cursor;
                while (*rest_cursor && *rest_cursor != quote) {
                    ++rest_cursor;
                }
                path_token = duplicate_range(path_start, rest_cursor);
            } else {
                const char *path_start = rest_cursor;
                while (*rest_cursor && !isspace((unsigned char)*rest_cursor)) {
                    ++rest_cursor;
                }
                path_token = duplicate_range(path_start, rest_cursor);
            }
            if (!path_token || path_token[0] == '\0') {
                bool err = set_error(error_message, "empty include directive in '%s'", filename ? filename : "<buffer>");
                free(keyword);
                free(content);
                free(path_token);
                return err;
            }

            char *resolved_path = NULL;
            if (!resolve_include_path(filename, path_token, &resolved_path)) {
                bool err = set_error(error_message, "unable to resolve include '%s' in '%s'", path_token, filename ? filename : "<buffer>");
                free(keyword);
                free(content);
                free(path_token);
                return err;
            }

            char *include_buffer = NULL;
            size_t include_length = 0;
            if (!read_file_contents(resolved_path, &include_buffer, &include_length, error_message)) {
                // Fallback: if directive used bare identifier without extension, try appending .inc
                bool retried = false;
                if (strchr(path_token, '.') == NULL) {
                    size_t base_len = strlen(path_token);
                    char *with_inc = malloc(base_len + 5);
                    if (with_inc) {
                        memcpy(with_inc, path_token, base_len);
                        memcpy(with_inc + base_len, ".inc", 5);
                        char *resolved2 = NULL;
                        if (resolve_include_path(filename, with_inc, &resolved2)) {
                            // Clear previous error message (if any) and retry
                            if (error_message && *error_message) {
                                free(*error_message);
                                *error_message = NULL;
                            }
                            retried = read_file_contents(resolved2, &include_buffer, &include_length, error_message);
                            free(resolved2);
                        }
                        free(with_inc);
                    }
                }
                if (!retried) {
                    free(keyword);
                    free(content);
                    free(path_token);
                    free(resolved_path);
                    free(include_buffer);
                    return false;
                }
            }

            bool ok = preprocess_buffer_internal(pp, resolved_path, include_buffer, include_length, conditions, output, depth + 1, error_message);
            free(include_buffer);
            free(resolved_path);
            if (!ok) {
                free(keyword);
                free(content);
                free(path_token);
                return false;
            }
            free(path_token);
        }
    } else if (strcmp(keyword, "DEFINE") == 0) {
        handled = true;
        if (branch_active) {
            // The rest of the line is the definition, which might include :=
            // We don't parse just an identifier, we take the whole rest
            char *def_content = duplicate_range(rest, content_end);
            if (!def_content) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
            
            // trim
            char *def_begin = def_content;
            char *def_end = def_content + strlen(def_content);
            trim(&def_begin, &def_end);

            if (def_begin[0] == '\0') {
                free(def_content);
                free(keyword);
                free(content);
                return set_error(error_message, "malformed define directive in '%s'", filename ? filename : "<buffer>");
            }

            define_symbol(pp, def_begin);
            free(def_content);
        }
    } else if (strcmp(keyword, "UNDEF") == 0 || strcmp(keyword, "UNDEFINE") == 0) {
        handled = true;
        if (branch_active) {
            char *symbol = NULL;
            if (!parse_identifier(rest, content_end, &symbol) || symbol[0] == '\0') {
                free(symbol);
                free(keyword);
                free(content);
                return set_error(error_message, "malformed undef directive in '%s'", filename ? filename : "<buffer>");
            }
            undefine_symbol(pp, symbol);
            free(symbol);
        }
    } else if (strcmp(keyword, "IFDEF") == 0 || strcmp(keyword, "IFNDEF") == 0) {
        handled = true;
        char *symbol = NULL;
        if (!parse_identifier(rest, content_end, &symbol) || symbol[0] == '\0') {
            free(symbol);
            free(keyword);
            free(content);
            return set_error(error_message, "malformed conditional directive in '%s'", filename ? filename : "<buffer>");
        }
        bool defined = symbol_is_defined(pp, symbol);
        if (strcmp(keyword, "IFNDEF") == 0) {
            defined = !defined;
        }
        bool parent_active = current_branch_active(conditions);
        if (!push_conditional(conditions, parent_active, defined)) {
            free(symbol);
            free(keyword);
            free(content);
            return set_error(error_message, "out of memory");
        }
        free(symbol);
    } else if (strcmp(keyword, "IF") == 0) {
        handled = true;
        bool cond_value = false;
        if (!evaluate_if_directive(pp, rest, &cond_value, error_message)) {
            free(keyword);
            free(content);
            // Wrap any specific error with the expected prefix
            if (error_message && *error_message) {
                char *specific_error = *error_message;
                char *wrapped_error = NULL;
                int len = snprintf(NULL, 0, "unsupported {$IF} expression: %s", specific_error);
                if (len > 0) {
                    wrapped_error = malloc(len + 1);
                    if (wrapped_error) {
                        snprintf(wrapped_error, len + 1, "unsupported {$IF} expression: %s", specific_error);
                        free(specific_error);
                        *error_message = wrapped_error;
                    }
                }
            } else {
                return set_error(
                    error_message,
                    "unsupported {$IF} expression in '%s'",
                    filename ? filename : "<buffer>"
                );
            }
            return false;
        }
        bool parent_active = current_branch_active(conditions);
        if (!push_conditional(conditions, parent_active, cond_value)) {
            free(keyword);
            free(content);
            return set_error(error_message, "out of memory");
        }
    } else if (strcmp(keyword, "ELSE") == 0) {
        handled = true;
        ConditionalFrame *frame = peek_conditional(conditions);
        if (!frame) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ELSE} without matching {$IF}");
        }
        if (frame->saw_else) {
            free(keyword);
            free(content);
            return set_error(error_message, "duplicate {$ELSE} in conditional block");
        }
        activate_fallback_branch(frame);
    } else if (strcmp(keyword, "ELSEIFDEF") == 0 || strcmp(keyword, "ELSEIFNDEF") == 0) {
        handled = true;
        ConditionalFrame *frame = peek_conditional(conditions);
        if (!frame) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ELSEIF} without matching {$IF}");
        }
        if (frame->saw_else) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ELSEIF} after {$ELSE}");
        }
        char *symbol = NULL;
        if (!parse_identifier(rest, content_end, &symbol) || symbol[0] == '\0') {
            free(symbol);
            free(keyword);
            free(content);
            return set_error(error_message, "malformed {$ELSEIF} directive");
        }
        bool defined = symbol_is_defined(pp, symbol);
        if (strcmp(keyword, "ELSEIFNDEF") == 0) {
            defined = !defined;
        }
        if (!frame->branch_taken && frame->parent_allows && defined) {
            frame->active = true;
            frame->branch_taken = true;
        } else {
            frame->active = false;
        }
        free(symbol);
    } else if (strcmp(keyword, "ELSEIF") == 0) {
        handled = true;
        ConditionalFrame *frame = peek_conditional(conditions);
        if (!frame) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ELSEIF} without matching {$IF}");
        }
        if (frame->saw_else) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ELSEIF} after {$ELSE}");
        }
        if (*rest == '\0') {
            activate_fallback_branch(frame);
        } else {
            bool cond_value = false;
            if (!evaluate_if_directive(pp, rest, &cond_value, error_message)) {
                free(keyword);
                free(content);
                bool has_specific_error = error_message != NULL && *error_message != NULL;
                if (!has_specific_error) {
                    return set_error(error_message, "unsupported {$ELSEIF} expression");
                }
                return false;
            }
            if (!frame->branch_taken && frame->parent_allows && cond_value) {
                frame->active = true;
                frame->branch_taken = true;
            } else {
                frame->active = false;
            }
        }
    } else if (strcmp(keyword, "ENDIF") == 0) {
        handled = true;
        if (!pop_conditional(conditions)) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ENDIF} without matching {$IF}");
        }
    } else {
        handled = true; // Treat unknown directives as whitespace
    }

    if (branch_active && handled && strcmp(keyword, "INCLUDE") != 0 && strcmp(keyword, "I") != 0) {
        // To avoid token merging when directives appear in-line, insert a single space
        if (!string_builder_append_char(output, ' ')) {
            free(keyword);
            free(content);
            return set_error(error_message, "out of memory");
        }
    }

    free(keyword);
    free(content);

    if (paren_style) {
        *index = end + 1; // Skip the closing '*'
    } else {
        *index = end;
    }

    return true;
}

static bool read_file_contents(const char *filename, char **buffer, size_t *length, char **error_message) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        return set_error(error_message, "failed to open '%s'", filename);
    }
    if (fseek(file, 0, SEEK_END) != 0) {
        fclose(file);
        return set_error(error_message, "failed to seek '%s'", filename);
    }
    long size = ftell(file);
    if (size < 0) {
        fclose(file);
        return set_error(error_message, "failed to determine size of '%s'", filename);
    }
    if (fseek(file, 0, SEEK_SET) != 0) {
        fclose(file);
        return set_error(error_message, "failed to rewind '%s'", filename);
    }
    char *data = malloc((size_t)size + 1);
    if (!data) {
        fclose(file);
        return set_error(error_message, "out of memory reading '%s'", filename);
    }
    size_t read = fread(data, 1, (size_t)size, file);
    fclose(file);
    if (read != (size_t)size) {
        free(data);
        return set_error(error_message, "failed to read '%s'", filename);
    }
    data[read] = '\0';
    if (buffer) {
        *buffer = data;
    }
    if (length) {
        *length = read;
    }
    return true;
}

static bool set_error(char **error_message, const char *fmt, ...) {
    if (!error_message) {
        return false;
    }
    va_list args;
    va_start(args, fmt);
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, fmt, args_copy);
    va_end(args_copy);
    if (needed < 0) {
        va_end(args);
        return false;
    }
    size_t size = (size_t)needed + 1;
    char *buffer = malloc(size);
    if (!buffer) {
        va_end(args);
        return false;
    }
    vsnprintf(buffer, size, fmt, args);
    va_end(args);
    *error_message = buffer;
    return false;
}

static void string_builder_init(StringBuilder *sb) {
    sb->data = NULL;
    sb->length = 0;
    sb->capacity = 0;
}

static bool string_builder_append_char(StringBuilder *sb, char c) {
    if (!ensure_capacity((void **)&sb->data, sizeof(char), &sb->capacity, sb->length + 2)) {
        return false;
    }
    sb->data[sb->length++] = c;
    sb->data[sb->length] = '\0';
    return true;
}

static void string_builder_free(StringBuilder *sb) {
    free(sb->data);
    sb->data = NULL;
    sb->length = 0;
    sb->capacity = 0;
}

static bool ensure_capacity(void **buffer, size_t element_size, size_t *capacity, size_t needed) {
    if (*capacity >= needed) {
        return true;
    }
    size_t new_capacity = (*capacity == 0) ? 64 : *capacity;
    while (new_capacity < needed) {
        new_capacity *= 2;
    }
    void *new_buffer = realloc(*buffer, new_capacity * element_size);
    if (!new_buffer) {
        return false;
    }
    *buffer = new_buffer;
    *capacity = new_capacity;
    return true;
}

static int ascii_tolower(int c) {
    if (c >= 'A' && c <= 'Z') {
        return c - 'A' + 'a';
    }
    return c;
}

static int ascii_strncasecmp(const char *a, const char *b, size_t n) {
    for (size_t i = 0; i < n; ++i) {
        unsigned char ca = (unsigned char)a[i];
        unsigned char cb = (unsigned char)b[i];
        if (ca == '\0' || cb == '\0') {
            return ascii_tolower(ca) - ascii_tolower(cb);
        }
        int diff = ascii_tolower(ca) - ascii_tolower(cb);
        if (diff != 0) {
            return diff;
        }
    }
    return 0;
}

static bool push_conditional(ConditionalStack *stack, bool parent_active, bool condition) {
    if (!ensure_capacity((void **)&stack->items, sizeof(ConditionalFrame), &stack->capacity, stack->size + 1)) {
        return false;
    }
    ConditionalFrame frame;
    frame.parent_allows = parent_active;
    frame.branch_taken = condition && parent_active;
    frame.active = frame.branch_taken;
    frame.saw_else = false;
    stack->items[stack->size++] = frame;
    return true;
}

static bool pop_conditional(ConditionalStack *stack) {
    if (stack->size == 0) {
        return false;
    }
    --stack->size;
    return true;
}

static ConditionalFrame *peek_conditional(ConditionalStack *stack) {
    if (stack->size == 0) {
        return NULL;
    }
    return &stack->items[stack->size - 1];
}

static bool current_branch_active(ConditionalStack *stack) {
    if (stack->size == 0) {
        return true;
    }
    return stack->items[stack->size - 1].active;
}

static void activate_fallback_branch(ConditionalFrame *frame) {
    frame->saw_else = true;
    if (!frame->branch_taken && frame->parent_allows) {
        frame->active = true;
        frame->branch_taken = true;
    } else {
        frame->active = false;
    }
}

static bool define_symbol(PascalPreprocessor *pp, const char *symbol) {
    if (!pp || !symbol || symbol[0] == '\0') {
        return false;
    }

    // Parse symbol for optional value assignment :=
    // Format: NAME [:= VALUE]
    char *name_part = NULL;
    char *value_part = NULL;

    const char *assign_pos = strstr(symbol, ":=");
    if (assign_pos) {
        size_t name_len = (size_t)(assign_pos - symbol);
        char *temp_name = malloc(name_len + 1);
        if (!temp_name) return false;
        memcpy(temp_name, symbol, name_len);
        temp_name[name_len] = '\0';
        
        char *trimmed_name_begin = temp_name;
        char *trimmed_name_end = temp_name + name_len;
        trim(&trimmed_name_begin, &trimmed_name_end);
        name_part = strdup(trimmed_name_begin);
        free(temp_name);

        const char *val_start = assign_pos + 2;
        char *temp_val = strdup(val_start);
        if (!temp_val) {
            free(name_part);
            return false;
        }
        char *trimmed_val_begin = temp_val;
        char *trimmed_val_end = temp_val + strlen(temp_val);
        trim(&trimmed_val_begin, &trimmed_val_end);
        value_part = strdup(trimmed_val_begin);
        free(temp_val);
    } else {
        name_part = strdup(symbol);
        // Default value for simple defines is "1"
        value_part = strdup("1"); 
    }

    if (!name_part || !value_part) {
        free(name_part);
        free(value_part);
        return false;
    }

    uppercase(name_part);

    // Check if already defined, if so update value
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, name_part) == 0) {
            free(pp->defines[i].value);
            pp->defines[i].value = value_part;
            free(name_part);
            return true;
        }
    }

    if (!ensure_capacity((void **)&pp->defines, sizeof(DefineEntry), &pp->define_capacity, pp->define_count + 1)) {
        free(name_part);
        free(value_part);
        return false;
    }

    pp->defines[pp->define_count].name = name_part;
    pp->defines[pp->define_count].value = value_part;
    pp->define_count++;
    return true;
}

static bool undefine_symbol(PascalPreprocessor *pp, const char *symbol) {
    if (!pp || !symbol) {
        return false;
    }
    char *upper = strdup(symbol);
    if (!upper) {
        return false;
    }
    uppercase(upper);
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, upper) == 0) {
            free(pp->defines[i].name);
            free(pp->defines[i].value);
            pp->defines[i] = pp->defines[pp->define_count - 1];
            // Zero out the moved-from slot to be safe, though count decrement handles it
            pp->defines[pp->define_count - 1].name = NULL;
            pp->defines[pp->define_count - 1].value = NULL;
            --pp->define_count;
            free(upper);
            return true;
        }
    }
    free(upper);
    return true;
}

static bool symbol_is_defined(const PascalPreprocessor *pp, const char *symbol) {
    if (!pp || !symbol) {
        return false;
    }
    char *upper = strdup(symbol);
    if (!upper) {
        return false;
    }
    uppercase(upper);
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, upper) == 0) {
            free(upper);
            return true;
        }
    }
    free(upper);
    return false;
}

static void trim(char **begin, char **end) {
    while (*begin < *end && isspace((unsigned char)**begin)) {
        (*begin)++;
    }
    while (*end > *begin && isspace((unsigned char)*(*end - 1))) {
        (*end)--;
    }
    **end = '\0';
}

static char *duplicate_range(const char *start, const char *end) {
    if (end < start) {
        return NULL;
    }
    size_t len = (size_t)(end - start);
    char *result = malloc(len + 1);
    if (!result) {
        return NULL;
    }
    memcpy(result, start, len);
    result[len] = '\0';
    return result;
}

static bool parse_identifier(const char *start, const char *end, char **out_identifier) {
    const char *cursor = start;
    while (cursor < end && isspace((unsigned char)*cursor)) {
        ++cursor;
    }
    const char *begin = cursor;
    if (cursor < end && (isalpha((unsigned char)*cursor) || *cursor == '_')) {
        ++cursor;
        while (cursor < end && (isalnum((unsigned char)*cursor) || *cursor == '_')) {
            ++cursor;
        }
    }
    size_t len = (size_t)(cursor - begin);
    if (len == 0) {
        return false;
    }
    char *result = malloc(len + 1);
    if (!result) {
        return false;
    }
    memcpy(result, begin, len);
    result[len] = '\0';
    if (out_identifier) {
        *out_identifier = result;
    } else {
        free(result);
    }
    return true;
}

static void uppercase(char *str) {
    if (!str) {
        return;
    }
    while (*str) {
        *str = (char)toupper((unsigned char)*str);
        ++str;
    }
}

static void extract_directory(const char *filename, char *buffer, size_t buffer_size) {
    if (!buffer || buffer_size == 0) {
        return;
    }
    if (!filename || filename[0] == '\0') {
        strncpy(buffer, ".", buffer_size - 1);
        buffer[buffer_size - 1] = '\0';
        return;
    }
    const char *last_slash = strrchr(filename, '/');
#ifdef _WIN32
    const char *last_backslash = strrchr(filename, '\\');
    if (!last_slash || (last_backslash && last_backslash > last_slash)) {
        last_slash = last_backslash;
    }
#endif
    if (!last_slash) {
        strncpy(buffer, ".", buffer_size - 1);
        buffer[buffer_size - 1] = '\0';
        return;
    }
    size_t len = (size_t)(last_slash - filename);
    if (len >= buffer_size) {
        len = buffer_size - 1;
    }
    memcpy(buffer, filename, len);
    buffer[len] = '\0';
}

static bool resolve_include_path(const char *current_file, const char *directive_path, char **result_path) {
    if (!directive_path || !result_path) {
        return false;
    }
    if (directive_path[0] == '\0') {
        return false;
    }
    if (directive_path[0] == '/'
#ifdef _WIN32
        || (strlen(directive_path) > 1 && directive_path[1] == ':')
#endif
    ) {
        *result_path = strdup(directive_path);
        return *result_path != NULL;
    }

    char directory[1024];
    extract_directory(current_file, directory, sizeof(directory));

    size_t dir_len = strlen(directory);
    size_t path_len = strlen(directive_path);
    size_t total_len = dir_len + 1 + path_len + 1;
    char *combined = malloc(total_len);
    if (!combined) {
        return false;
    }
    snprintf(combined, total_len, "%s/%s", directory, directive_path);
    *result_path = combined;
    return true;
}

static const char *get_symbol_value(const PascalPreprocessor *pp, const char *symbol) {
    if (!pp || !symbol) return NULL;
    char *upper = strdup(symbol);
    if (!upper) return NULL;
    uppercase(upper);
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, upper) == 0) {
            free(upper);
            return pp->defines[i].value;
        }
    }
    free(upper);
    return NULL;
}

static bool evaluate_if_directive(PascalPreprocessor *pp,
                                  const char *expression,
                                  bool *result,
                                  char **error_message) {
    const char *cursor = expression;
    int64_t value = 0;
    if (!parse_expression(&cursor, &value, pp, error_message)) {
        return false;
    }
    while (*cursor && isspace((unsigned char)*cursor)) {
        ++cursor;
    }
    if (*cursor != '\0') {
        return set_error(error_message, "unexpected characters after expression");
    }
    *result = (value != 0);
    return true;
}

// Expression = SimpleExpression [ RelOp SimpleExpression ]
static bool parse_expression(const char **cursor,
                             int64_t *value,
                             PascalPreprocessor *pp,
                             char **error_message) {
    if (!parse_simple_expression(cursor, value, pp, error_message)) {
        return false;
    }

    while (**cursor && isspace((unsigned char)**cursor)) {
        ++(*cursor);
    }

    // Check for relational operators
    // =, <>, <, <=, >, >=
    enum { OP_NONE, OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE } op = OP_NONE;

    if (**cursor == '=') {
        op = OP_EQ;
        ++(*cursor);
    } else if (**cursor == '<') {
        ++(*cursor);
        if (**cursor == '>') {
            op = OP_NE;
            ++(*cursor);
        } else if (**cursor == '=') {
            op = OP_LE;
            ++(*cursor);
        } else {
            op = OP_LT;
        }
    } else if (**cursor == '>') {
        ++(*cursor);
        if (**cursor == '=') {
            op = OP_GE;
            ++(*cursor);
        } else {
            op = OP_GT;
        }
    }

    if (op != OP_NONE) {
        int64_t rhs = 0;
        if (!parse_simple_expression(cursor, &rhs, pp, error_message)) {
            return false;
        }
        switch (op) {
            case OP_EQ: *value = (*value == rhs); break;
            case OP_NE: *value = (*value != rhs); break;
            case OP_LT: *value = (*value < rhs); break;
            case OP_LE: *value = (*value <= rhs); break;
            case OP_GT: *value = (*value > rhs); break;
            case OP_GE: *value = (*value >= rhs); break;
            default: break;
        }
    }

    return true;
}

// SimpleExpression = Term { AddOp Term }
// AddOp = +, -, OR, XOR
static bool parse_simple_expression(const char **cursor,
                                    int64_t *value,
                                    PascalPreprocessor *pp,
                                    char **error_message) {
    if (!parse_term(cursor, value, pp, error_message)) {
        return false;
    }

    while (1) {
        const char *start = *cursor;
        while (**cursor && isspace((unsigned char)**cursor)) {
            ++(*cursor);
        }

        enum { OP_NONE, OP_ADD, OP_SUB, OP_OR, OP_XOR } op = OP_NONE;

        if (**cursor == '+') {
            op = OP_ADD;
            ++(*cursor);
        } else if (**cursor == '-') {
            op = OP_SUB;
            ++(*cursor);
        } else if (ascii_strncasecmp(*cursor, "OR", 2) == 0 && !isalnum((unsigned char)(*cursor)[2]) && (*cursor)[2] != '_') {
            op = OP_OR;
            *cursor += 2;
        } else if (ascii_strncasecmp(*cursor, "XOR", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_XOR;
            *cursor += 3;
        }

        if (op == OP_NONE) {
            *cursor = start;
            break;
        }

        int64_t rhs = 0;
        if (!parse_term(cursor, &rhs, pp, error_message)) {
            return false;
        }

        switch (op) {
            case OP_ADD: *value += rhs; break;
            case OP_SUB: *value -= rhs; break;
            case OP_OR:  *value = (*value | rhs); break;
            case OP_XOR: *value = (*value ^ rhs); break;
            default: break;
        }
    }
    return true;
}

// Term = Factor { MulOp Factor }
// MulOp = *, /, DIV, MOD, AND
static bool parse_term(const char **cursor,
                       int64_t *value,
                       PascalPreprocessor *pp,
                       char **error_message) {
    if (!parse_factor(cursor, value, pp, error_message)) {
        return false;
    }

    while (1) {
        const char *start = *cursor;
        while (**cursor && isspace((unsigned char)**cursor)) {
            ++(*cursor);
        }

        enum { OP_NONE, OP_MUL, OP_DIV, OP_INTDIV, OP_MOD, OP_AND } op = OP_NONE;

        if (**cursor == '*') {
            op = OP_MUL;
            ++(*cursor);
        } else if (**cursor == '/') {
            op = OP_DIV; // Treat / as integer division for preprocessor? Or error? Pascal / is float.
            // For preprocessor, usually integer math. Let's assume integer div.
            ++(*cursor);
        } else if (ascii_strncasecmp(*cursor, "DIV", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_INTDIV;
            *cursor += 3;
        } else if (ascii_strncasecmp(*cursor, "MOD", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_MOD;
            *cursor += 3;
        } else if (ascii_strncasecmp(*cursor, "AND", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_AND;
            *cursor += 3;
        }

        if (op == OP_NONE) {
            *cursor = start;
            break;
        }

        int64_t rhs = 0;
        if (!parse_factor(cursor, &rhs, pp, error_message)) {
            return false;
        }

        switch (op) {
            case OP_MUL: *value *= rhs; break;
            case OP_DIV: 
            case OP_INTDIV:
                if (rhs == 0) return set_error(error_message, "division by zero");
                *value /= rhs; 
                break;
            case OP_MOD:
                if (rhs == 0) return set_error(error_message, "division by zero");
                *value %= rhs;
                break;
            case OP_AND: *value = (*value & rhs); break;
            default: break;
        }
    }
    return true;
}

// Factor = Atom | NOT Factor | ( Expression ) | + Factor | - Factor
// Atom = Identifier | Number | DEFINED(...) | DECLARED(...)
static bool parse_factor(const char **cursor,
                         int64_t *value,
                         PascalPreprocessor *pp,
                         char **error_message) {
    while (**cursor && isspace((unsigned char)**cursor)) {
        ++(*cursor);
    }

    // Unary operators
    if (ascii_strncasecmp(*cursor, "NOT", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
        *cursor += 3;
        int64_t inner = 0;
        if (!parse_factor(cursor, &inner, pp, error_message)) return false;
        *value = !inner;
        return true;
    }
    if (**cursor == '+') {
        ++(*cursor);
        return parse_factor(cursor, value, pp, error_message);
    }
    if (**cursor == '-') {
        ++(*cursor);
        int64_t inner = 0;
        if (!parse_factor(cursor, &inner, pp, error_message)) return false;
        *value = -inner;
        return true;
    }

    // Parentheses
    if (**cursor == '(') {
        ++(*cursor);
        if (!parse_expression(cursor, value, pp, error_message)) return false;
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        if (**cursor != ')') return set_error(error_message, "missing ')'");
        ++(*cursor);
        return true;
    }

    // DEFINED / DECLARED
    if (ascii_strncasecmp(*cursor, "DEFINED", 7) == 0 || ascii_strncasecmp(*cursor, "DECLARED", 8) == 0) {
        bool is_declared = (ascii_strncasecmp(*cursor, "DECLARED", 8) == 0);
        *cursor += (is_declared ? 8 : 7);
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        
        bool paren = false;
        if (**cursor == '(') {
            paren = true;
            ++(*cursor);
        }
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        
        const char *start = *cursor;
        if (!isalpha((unsigned char)**cursor) && **cursor != '_') return set_error(error_message, "expected identifier");
        while (**cursor && (isalnum((unsigned char)**cursor) || **cursor == '_' || **cursor == '.')) ++(*cursor);
        
        char *sym = duplicate_range(start, *cursor);
        if (!sym) return set_error(error_message, "out of memory");
        
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        if (paren) {
            if (**cursor != ')') {
                free(sym);
                return set_error(error_message, "missing ')'");
            }
            ++(*cursor);
        }
        
        *value = symbol_is_defined(pp, sym) ? 1 : 0;
        free(sym);
        return true;
    }

    // Number
    if (isdigit((unsigned char)**cursor)) {
        char *end;
        *value = strtoll(*cursor, &end, 0);
        *cursor = end;
        return true;
    }

    // Identifier (resolve to value)
    const char *start = *cursor;
    if (isalpha((unsigned char)**cursor) || **cursor == '_') {
        while (**cursor && (isalnum((unsigned char)**cursor) || **cursor == '_' || **cursor == '.')) {
            ++(*cursor);
        }
        char *sym = duplicate_range(start, *cursor);
        if (!sym) return set_error(error_message, "out of memory");
        
        const char *val_str = get_symbol_value(pp, sym);
        if (val_str) {
            // Try to parse value as integer
            char *end;
            int64_t parsed_val = strtoll(val_str, &end, 0);
            if (*end == '\0') {
                *value = parsed_val;
            } else {
                // If not a pure number, treat as 1 (true) if defined? 
                // Or maybe 0?
                // For now, if it has a value but not number, let's say 0 unless it's "TRUE"
                if (ascii_strncasecmp(val_str, "TRUE", 4) == 0) *value = 1;
                else if (ascii_strncasecmp(val_str, "FALSE", 5) == 0) *value = 0;
                else *value = parsed_val; // Best effort
            }
        } else {
            // Undefined symbol -> error (backward compatibility)
            bool err = set_error(
                error_message,
                "undefined macro '%s' in conditional expression",
                sym
            );
            free(sym);
            return err;
        }
        free(sym);
        return true;
    }

    return set_error(error_message, "syntax error in expression");
}
