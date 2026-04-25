#include "pascal_preprocessor.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_INCLUDE_DEPTH 32

typedef struct {
    char *name;
    char *value;
    bool is_macro;  // true if explicitly defined with := (should expand), false if just defined
} DefineEntry;

struct PascalPreprocessor {
    DefineEntry *defines;
    size_t define_count;
    size_t define_capacity;
    bool macro_enabled;  // Track {$macro on/off} state
    bool flatten_only;   // Expand includes only; keep all directives and branches
    char **include_paths;
    size_t include_path_count;
    size_t include_path_capacity;
    /* Track resolved include files for cache invalidation */
    char **included_files;
    size_t included_file_count;
    size_t included_file_capacity;
    /* For declared() support - points to current output buffer during preprocessing */
    const char *current_output;
    size_t current_output_len;
    /* Track {$ASMMODE INTEL/ATT} state */
    bool asmmode_intel;
};

typedef struct {
    bool parent_allows;
    bool branch_taken;
    bool active;
    bool saw_else;
    char *filename;
    int line;
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
static const char *try_expand_macro(PascalPreprocessor *pp, const char *input, size_t length, size_t pos, size_t *out_identifier_len);
static char *expand_macro_value_once(PascalPreprocessor *pp, const char *value);
static void string_builder_init(StringBuilder *sb);
static bool string_builder_append_char(StringBuilder *sb, char c);
static bool string_builder_append_string(StringBuilder *sb, const char *str);
static void string_builder_free(StringBuilder *sb);
static char *string_builder_finalize(StringBuilder *sb);
static bool ensure_capacity(void **buffer, size_t element_size, size_t *capacity, size_t needed);
static bool emit_line_directive(StringBuilder *sb, int line, const char *filename);
static char *my_strdup(const char *s) {
    if (!s) return NULL;
    size_t len = strlen(s);
    char *d = malloc(len + 1);
    if (d) memcpy(d, s, len + 1);
    return d;
}
static bool push_conditional(ConditionalStack *stack, bool parent_active, bool condition, const char *filename, int line);
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
                             int current_line,
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
static bool define_symbol_const(PascalPreprocessor *pp, const char *name, const char *value);
static bool undefine_symbol(PascalPreprocessor *pp, const char *symbol);
static bool symbol_is_defined(const PascalPreprocessor *pp, const char *symbol);
static bool find_const_integer_in_source(PascalPreprocessor *pp, const char *symbol, int64_t *out_value);
static void trim(char **begin, char **end);
static char *duplicate_range(const char *start, const char *end);
static void extract_directory(const char *filename, char *buffer, size_t buffer_size);
static bool resolve_include_path(const PascalPreprocessor *pp, const char *current_file, const char *directive_path, char **result_path);
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
static bool is_keyword_operator(const char *cursor, const char *keyword, size_t keyword_len);
static const char *get_symbol_value(const PascalPreprocessor *pp, const char *symbol);
static const char *get_macro_value(const PascalPreprocessor *pp, const char *symbol);

PascalPreprocessor *pascal_preprocessor_create(void) {
    PascalPreprocessor *pp = malloc(sizeof(PascalPreprocessor));
    if (!pp) {
        return NULL;
    }
    pp->defines = NULL;
    pp->define_count = 0;
    pp->define_capacity = 0;
    pp->macro_enabled = true;  // FPC-style: macro expansion is on by default
    pp->flatten_only = false;
    pp->include_paths = NULL;
    pp->include_path_count = 0;
    pp->include_path_capacity = 0;
    pp->included_files = NULL;
    pp->included_file_count = 0;
    pp->included_file_capacity = 0;
    pp->current_output = NULL;
    pp->current_output_len = 0;
    pp->asmmode_intel = false;
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
    for (size_t i = 0; i < pp->include_path_count; ++i) {
        free(pp->include_paths[i]);
    }
    free(pp->include_paths);
    for (size_t i = 0; i < pp->included_file_count; ++i) {
        free(pp->included_files[i]);
    }
    free(pp->included_files);
    free(pp);
}

bool pascal_preprocessor_define(PascalPreprocessor *pp, const char *symbol) {
    return define_symbol(pp, symbol);
}

bool pascal_preprocessor_define_macro(PascalPreprocessor *pp, const char *symbol, const char *value) {
    if (!symbol || !value) return false;
    size_t len = strlen(symbol) + strlen(value) + 3;
    char *combined = malloc(len);
    if (!combined) return false;
    snprintf(combined, len, "%s:=%s", symbol, value);
    bool result = define_symbol(pp, combined);
    free(combined);
    return result;
}

bool pascal_preprocessor_define_const(PascalPreprocessor *pp, const char *symbol, const char *value) {
    return define_symbol_const(pp, symbol, value);
}

bool pascal_preprocessor_undefine(PascalPreprocessor *pp, const char *symbol) {
    return undefine_symbol(pp, symbol);
}

bool pascal_preprocessor_is_defined(const PascalPreprocessor *pp, const char *symbol) {
    return symbol_is_defined(pp, symbol);
}

bool pascal_preprocessor_add_include_path(PascalPreprocessor *pp, const char *path) {
    if (!pp || !path) return false;
    
    if (pp->include_path_count >= pp->include_path_capacity) {
        size_t new_capacity = pp->include_path_capacity == 0 ? 8 : pp->include_path_capacity * 2;
        char **new_paths = realloc(pp->include_paths, new_capacity * sizeof(char*));
        if (!new_paths) return false;
        pp->include_paths = new_paths;
        pp->include_path_capacity = new_capacity;
    }
    
    char *path_copy = my_strdup(path);
    if (!path_copy) return false;
    
    pp->include_paths[pp->include_path_count++] = path_copy;
    return true;
}

static void record_included_file(PascalPreprocessor *pp, const char *path) {
    if (!pp || !path) return;
    if (pp->included_file_count >= pp->included_file_capacity) {
        size_t new_cap = pp->included_file_capacity == 0 ? 16 : pp->included_file_capacity * 2;
        char **new_arr = realloc(pp->included_files, new_cap * sizeof(char *));
        if (!new_arr) return;
        pp->included_files = new_arr;
        pp->included_file_capacity = new_cap;
    }
    char *copy = my_strdup(path);
    if (copy)
        pp->included_files[pp->included_file_count++] = copy;
}

size_t pascal_preprocessor_get_included_files(const PascalPreprocessor *pp,
                                               const char *const **out_files) {
    if (!pp || !out_files) return 0;
    *out_files = (const char *const *)pp->included_files;
    return pp->included_file_count;
}

bool pascal_preprocessor_is_intel_asm(const PascalPreprocessor *pp) {
    return pp != NULL && pp->asmmode_intel;
}

void pascal_preprocessor_set_flatten_only(PascalPreprocessor *pp, bool flatten_only) {
    if (!pp) {
        return;
    }
    pp->flatten_only = flatten_only;
    if (flatten_only) {
        pp->macro_enabled = false;
    }
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
        // Construct detailed error message with stack trace
        size_t total_len = 256; // Base message length
        for (size_t i = 0; i < conditions.size; ++i) {
            total_len += strlen(conditions.items[i].filename) + 32;
        }
        
        char *msg = malloc(total_len);
        if (msg) {
            int offset = snprintf(msg, total_len, "unterminated conditional in '%s':\n", filename ? filename : "<buffer>");
            for (size_t i = 0; i < conditions.size; ++i) {
                ConditionalFrame *frame = &conditions.items[i];
                offset += snprintf(msg + offset, total_len - offset, "  Open conditional at %s:%d\n", 
                                 frame->filename ? frame->filename : "<unknown>", frame->line);
            }
            if (*error_message) free(*error_message);
            *error_message = msg;
        } else {
            set_error(error_message, "unterminated conditional in '%s'", filename ? filename : "<buffer>");
        }
        
        string_builder_free(&output);
        for (size_t i = 0; i < conditions.size; ++i) {
            free(conditions.items[i].filename);
        }
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
    for (size_t i = 0; i < conditions.size; ++i) {
        free(conditions.items[i].filename);
    }
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

// Helper to detect lines to skip in systemh.inc and x86_64.inc
// Returns: 0 = keep, 1 = skip line, 2 = skip block (start)
static int should_skip_line(const char *fname, const char *line_start) {
    if (!fname) return 0;
    size_t len = strlen(fname);
    
    while (*line_start && isspace((unsigned char)*line_start) && *line_start != '\n') line_start++;

    if (len >= 11 && strcmp(fname + len - 11, "systemh.inc") == 0) {
        if (strncmp(line_start, "function get_frame:pointer;", 27) == 0) return 1;
        if (strncmp(line_start, "Function Get_pc_addr : CodePointer;", 35) == 0) return 1;
    }
    
    if (len >= 10 && strcmp(fname + len - 10, "x86_64.inc") == 0) {
        if (strncmp(line_start, "function get_frame:pointer;assembler;nostackframe;", 50) == 0) return 2;
    }

    return 0;
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
    int brace_comment_depth = 0;
    int paren_comment_depth = 0;
    bool in_line_comment = false;

    int current_line = 1;
    int last_emitted_line = 0;  /* Track last line we emitted content for */
    bool need_line_directive = true;  /* Need to emit line directive for first content */

    bool skip_block_mode = false;

    for (size_t i = 0; i < length; ++i) {
        // Check for line skipping at start of line
        if ((i == 0 || input[i-1] == '\n') && !pp->flatten_only && current_branch_active(conditions)) {
             if (skip_block_mode) {
                 // Check if line is "end;"
                 size_t j = i;
                 while (j < length && isspace((unsigned char)input[j]) && input[j] != '\n') j++;
                 if (strncmp(input + j, "end;", 4) == 0) {
                     skip_block_mode = false;
                     // Skip this line too (the end;)
                 }
                 // Skip the line including the newline
                 while (i < length && input[i] != '\n') i++;
                 // Skip the newline too (don't leave empty lines)
                 if (i < length && input[i] == '\n') i++;
                 i--; 
                 continue;
             }

             int skip_action = should_skip_line(filename, input + i);
             if (skip_action > 0) {
                 if (skip_action == 2) {
                     skip_block_mode = true;
                 }
                 // Skip until newline, then skip the newline too
                 while (i < length && input[i] != '\n') i++;
                 // Skip the newline too (don't leave empty lines)
                 if (i < length && input[i] == '\n') i++;
                 i--; 
                 continue;
             }
        }
        char c = input[i];

        bool in_comment = (brace_comment_depth > 0) || (paren_comment_depth > 0) || in_line_comment;

        if (!in_string && !in_comment) {
            if (c == '{' && i + 1 < length && input[i + 1] == '$') {
                if (!handle_directive(pp, filename, input, length, &i, false, conditions, output, depth, current_line, error_message)) {
                    return false;
                }
                continue;
            }
            if (c == '(' && i + 2 < length && input[i + 1] == '*' && input[i + 2] == '$') {
                if (!handle_directive(pp, filename, input, length, &i, true, conditions, output, depth, current_line, error_message)) {
                    return false;
                }
                continue;
            }
            if (c == '{') {
                size_t close = i + 1;
                bool has_newline = false;
                bool has_nested = false;
                for (; close < length; ++close) {
                    if (input[close] == '\n' || input[close] == '\r') {
                        has_newline = true;
                        break;
                    }
                    if (input[close] == '{') {
                        has_nested = true;
                        break;
                    }
                    if (input[close] == '}') {
                        break;
                    }
                }
                if (!has_newline && !has_nested && close < length && input[close] == '}') {
                    const char *p = input + i + 1;
                    const char *end = input + close;
                    while (p < end && isspace((unsigned char)*p))
                        ++p;
                    const char *id1_start = p;
                    if (p < end && (isalpha((unsigned char)*p) || *p == '_')) {
                        ++p;
                        while (p < end && (isalnum((unsigned char)*p) || *p == '_'))
                            ++p;
                        const char *id1_end = p;
                        while (p < end && isspace((unsigned char)*p))
                            ++p;
                        if (p + 1 < end && p[0] == ':' && p[1] == '=') {
                            p += 2;
                            while (p < end && isspace((unsigned char)*p))
                                ++p;
                            const char *id2_start = p;
                            if (p < end && (isalpha((unsigned char)*p) || *p == '_')) {
                                ++p;
                                while (p < end && (isalnum((unsigned char)*p) || *p == '_'))
                                    ++p;
                                const char *id2_end = p;
                                while (p < end && isspace((unsigned char)*p))
                                    ++p;
                                if (p == end) {
                                    size_t next = close + 1;
                                    while (next < length && (input[next] == ' ' || input[next] == '\t'))
                                        ++next;
                                    if (next < length && input[next] == '(') {
                                        if (pp->flatten_only || current_branch_active(conditions)) {
                                            bool should_emit_directive = false;
                                            if (need_line_directive && filename != NULL && depth > 0) {
                                                if (i == 0 || input[i - 1] == '\n' || last_emitted_line == 0) {
                                                    should_emit_directive = true;
                                                }
                                            }
                                            if (should_emit_directive) {
                                                if (!emit_line_directive(output, current_line, filename)) {
                                                    return set_error(error_message, "out of memory");
                                                }
                                                need_line_directive = false;
                                            }
                                            for (const char *cur = id1_start; cur < id1_end; ++cur) {
                                                if (!string_builder_append_char(output, *cur)) {
                                                    return set_error(error_message, "out of memory");
                                                }
                                            }
                                            if (!string_builder_append_char(output, ':') ||
                                                !string_builder_append_char(output, '=')) {
                                                return set_error(error_message, "out of memory");
                                            }
                                            for (const char *cur = id2_start; cur < id2_end; ++cur) {
                                                if (!string_builder_append_char(output, *cur)) {
                                                    return set_error(error_message, "out of memory");
                                                }
                                            }
                                            last_emitted_line = current_line;
                                        } else {
                                            need_line_directive = true;
                                        }
                                        i = close;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if (in_line_comment) {
            if (c == '\n') {
                current_line++;
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
        } else if (brace_comment_depth > 0) {
            if (c == '{') {
                ++brace_comment_depth;
            } else if (c == '}') {
                if (brace_comment_depth > 0)
                    --brace_comment_depth;
            }
        } else if (paren_comment_depth > 0) {
            if (c == '(' && i + 1 < length && input[i + 1] == '*') {
                ++paren_comment_depth;
            } else if (c == '*' && i + 1 < length && input[i + 1] == ')') {
                if (paren_comment_depth > 0)
                    --paren_comment_depth;
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
                brace_comment_depth = 1;
            } else if (c == '(' && i + 1 < length && input[i + 1] == '*') {
                paren_comment_depth = 1;
            } else if (c == '/' && i + 1 < length && input[i + 1] == '/') {
                in_line_comment = true;
            } else if (c == '\'' || c == '"') {
                in_string = true;
                string_delim = c;
            }
        }

        if (pp->flatten_only || current_branch_active(conditions)) {
            /* Emit line directive at start of included file, or when there's a gap in line numbers */
            bool should_emit_directive = false;
            if (need_line_directive && filename != NULL && depth > 0) {
                /* At start of line (or first character), emit directive */
                if (i == 0 || input[i-1] == '\n' || last_emitted_line == 0) {
                    should_emit_directive = true;
                }
            }
            if (should_emit_directive) {
                if (!emit_line_directive(output, current_line, filename)) {
                    return set_error(error_message, "out of memory");
                }
                need_line_directive = false;
            }
            
            // Try macro expansion if we're not in a comment or string
            if (!pp->flatten_only && !in_comment && !in_string && pp->macro_enabled) {
                size_t identifier_len = 0;
                const char *macro_value = try_expand_macro(pp, input, length, i, &identifier_len);
                
                if (macro_value) {
                    // Output the macro value instead of the identifier (single-pass expansion)
                    char *expanded_value = expand_macro_value_once(pp, macro_value);
                    if (expanded_value) {
                        if (!string_builder_append_string(output, expanded_value)) {
                            free(expanded_value);
                            return set_error(error_message, "out of memory");
                        }
                        free(expanded_value);
                    } else {
                        return set_error(error_message, "out of memory");
                    }
                    // Skip the identifier in the input
                    i += identifier_len - 1;  // -1 because loop will increment i
                    last_emitted_line = current_line;
                    continue;
                }
            }
            
            if (!string_builder_append_char(output, c)) {
                return set_error(error_message, "out of memory");
            }
            last_emitted_line = current_line;
        } else {
            /* Content is being skipped - mark that we need a line directive when we resume */
            need_line_directive = true;
            /* Emit newlines even for skipped content to keep line numbers in sync.
             * Without this, ifdef'd-out lines shrink the preprocessed output and
             * cause line number drift in the main file (depth 0) where {#line}
             * directives are not emitted. */
            if (c == '\n') {
                if (!string_builder_append_char(output, '\n')) {
                    return set_error(error_message, "out of memory");
                }
            }
        }
        
        // Track line numbers for ALL newlines
        if (c == '\n') {
            current_line++;
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
                             int current_line,
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
    bool skip_trailing_newline = false;

    if (strcmp(keyword, "I") == 0) {
        const char *cursor = rest;
        while (*cursor && isspace((unsigned char)*cursor)) {
            ++cursor;
        }
        // If the first character is + or -, treat it as a switch directive (e.g. {$I-,Q-})
        // This takes precedence over treating it as a filename.
        if (*cursor == '+' || *cursor == '-') {
            is_io_check_directive = true;
        }
    }

    if (is_io_check_directive) {
        handled = true;
        // {$I+} / {$I-} toggles I/O-checking; the compiler currently does not
        // distinguish the modes, so treat it as a no-op directive.
    } else if (pp->flatten_only) {
        size_t directive_end = paren_style ? end + 1 : end;
        bool is_include = (strcmp(keyword, "I") == 0 || strcmp(keyword, "INCLUDE") == 0);
        if (is_include) {
            handled = true;

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

            // Handle special FPC include macros like %DATE%, %TIME%, %FPCVERSION%, etc.
            if (path_token[0] == '%' && path_token[strlen(path_token) - 1] == '%') {
                char special_value[64] = "";
                bool is_special = false;

                if (strcmp(path_token, "%DATE%") == 0) {
                    time_t now = time(NULL);
                    struct tm *tm_info = localtime(&now);
                    snprintf(special_value, sizeof(special_value), "'%04d/%02d/%02d'",
                             tm_info->tm_year + 1900, tm_info->tm_mon + 1, tm_info->tm_mday);
                    is_special = true;
                } else if (strcmp(path_token, "%TIME%") == 0) {
                    time_t now = time(NULL);
                    struct tm *tm_info = localtime(&now);
                    snprintf(special_value, sizeof(special_value), "'%02d:%02d:%02d'",
                             tm_info->tm_hour, tm_info->tm_min, tm_info->tm_sec);
                    is_special = true;
                } else if (strcmp(path_token, "%FPCVERSION%") == 0) {
                    snprintf(special_value, sizeof(special_value), "'3.2.2'");
                    is_special = true;
                } else if (strcmp(path_token, "%FPCTARGET%") == 0) {
                    snprintf(special_value, sizeof(special_value), "'x86_64-linux'");
                    is_special = true;
                } else if (strcmp(path_token, "%FPCTARGETOS%") == 0) {
                    snprintf(special_value, sizeof(special_value), "'linux'");
                    is_special = true;
                } else if (strcmp(path_token, "%FPCTARGETCPU%") == 0) {
                    snprintf(special_value, sizeof(special_value), "'x86_64'");
                    is_special = true;
                } else if (strcmp(path_token, "%FILE%") == 0) {
                    const char *base = filename ? filename : "<unknown>";
                    snprintf(special_value, sizeof(special_value), "'%s'", base);
                    is_special = true;
                } else if (strcmp(path_token, "%LINE%") == 0) {
                    snprintf(special_value, sizeof(special_value), "0");
                    is_special = true;
                } else if (strcmp(path_token, "%LINENUM%") == 0) {
                    snprintf(special_value, sizeof(special_value), "0");
                    is_special = true;
                }

                if (is_special) {
                    size_t val_len = strlen(special_value);
                    for (size_t i = 0; i < val_len; ++i) {
                        if (!string_builder_append_char(output, special_value[i])) {
                            free(path_token);
                            free(keyword);
                            free(content);
                            return set_error(error_message, "out of memory");
                        }
                    }
                    free(path_token);
                    free(keyword);
                    free(content);
                    *index = directive_end;
                    return true;
                }
            }

            char *resolved_path = NULL;
            if (!resolve_include_path(pp, filename, path_token, &resolved_path)) {
                bool err = set_error(error_message, "unable to resolve include '%s' in '%s'", path_token, filename ? filename : "<buffer>");
                free(keyword);
                free(content);
                free(path_token);
                return err;
            }

            char *include_buffer = NULL;
            size_t include_length = 0;
            if (!read_file_contents(resolved_path, &include_buffer, &include_length, error_message)) {
                bool retried = false;
                if (strchr(path_token, '.') == NULL) {
                    size_t base_len = strlen(path_token);
                    char *with_inc = malloc(base_len + 5);
                    if (with_inc) {
                        memcpy(with_inc, path_token, base_len);
                        memcpy(with_inc + base_len, ".inc", 5);
                        char *resolved2 = NULL;
                        if (resolve_include_path(pp, filename, with_inc, &resolved2)) {
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

            /* Line directives are emitted dynamically when content is output,
             * so we don't emit {#line 1} here - the first emitted content will
             * trigger a line directive with the correct line number */

            record_included_file(pp, resolved_path);
            bool ok = preprocess_buffer_internal(pp, resolved_path, include_buffer, include_length, conditions, output, depth + 1, error_message);

            /* Emit line directive returning to parent file */
            if (ok) {
                emit_line_directive(output, current_line + 1, filename);
            }

            free(include_buffer);
            free(resolved_path);
            free(path_token);
            free(keyword);
            free(content);
            skip_trailing_newline = true;
            *index = directive_end;
            if (skip_trailing_newline && *index + 1 < length && input[*index + 1] == '\n')
                (*index)++;
            else if (skip_trailing_newline && *index + 1 < length && input[*index + 1] == '\r') {
                (*index)++;
                if (*index + 1 < length && input[*index + 1] == '\n')
                    (*index)++;
            }
            return ok;
        }

        handled = true;
        for (size_t i = start; i <= directive_end; ++i) {
            if (!string_builder_append_char(output, input[i])) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
        }
        free(keyword);
        free(content);
        *index = directive_end;
        return true;
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

            // Handle special FPC include macros like %DATE%, %TIME%, %FPCVERSION%, etc.
            // These return string literals instead of including files.
            if (path_token[0] == '%' && path_token[strlen(path_token) - 1] == '%') {
                char special_value[64] = "";
                bool is_special = false;

                if (strcmp(path_token, "%DATE%") == 0) {
                    // Return current date in 'YYYY/MM/DD' format
                    time_t now = time(NULL);
                    struct tm *tm_info = localtime(&now);
                    snprintf(special_value, sizeof(special_value), "'%04d/%02d/%02d'",
                             tm_info->tm_year + 1900, tm_info->tm_mon + 1, tm_info->tm_mday);
                    is_special = true;
                } else if (strcmp(path_token, "%TIME%") == 0) {
                    // Return current time in 'HH:MM:SS' format
                    time_t now = time(NULL);
                    struct tm *tm_info = localtime(&now);
                    snprintf(special_value, sizeof(special_value), "'%02d:%02d:%02d'",
                             tm_info->tm_hour, tm_info->tm_min, tm_info->tm_sec);
                    is_special = true;
                } else if (strcmp(path_token, "%FPCVERSION%") == 0) {
                    // Return FPC version string (we'll use a placeholder)
                    snprintf(special_value, sizeof(special_value), "'3.2.2'");
                    is_special = true;
                } else if (strcmp(path_token, "%FPCTARGET%") == 0) {
                    // Return target platform
                    snprintf(special_value, sizeof(special_value), "'x86_64-linux'");
                    is_special = true;
                } else if (strcmp(path_token, "%FPCTARGETOS%") == 0) {
                    snprintf(special_value, sizeof(special_value), "'linux'");
                    is_special = true;
                } else if (strcmp(path_token, "%FPCTARGETCPU%") == 0) {
                    snprintf(special_value, sizeof(special_value), "'x86_64'");
                    is_special = true;
                } else if (strcmp(path_token, "%FILE%") == 0) {
                    // Return current file name
                    const char *base = filename ? filename : "<unknown>";
                    snprintf(special_value, sizeof(special_value), "'%s'", base);
                    is_special = true;
                } else if (strcmp(path_token, "%LINE%") == 0) {
                    // Return line number (approximate - we don't track exact line)
                    snprintf(special_value, sizeof(special_value), "0");
                    is_special = true;
                } else if (strcmp(path_token, "%LINENUM%") == 0) {
                    snprintf(special_value, sizeof(special_value), "0");
                    is_special = true;
                }

                if (is_special) {
                    // Append the special value directly to output
                    size_t val_len = strlen(special_value);
                    for (size_t i = 0; i < val_len; ++i) {
                        string_builder_append_char(output, special_value[i]);
                    }
                    free(path_token);
                    free(keyword);
                    free(content);
                    *index = paren_style ? end + 1 : end;
                    return true;
                }
                // If not recognized, fall through to try as a file
            }

            char *resolved_path = NULL;
            if (!resolve_include_path(pp, filename, path_token, &resolved_path)) {
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
                        if (resolve_include_path(pp, filename, with_inc, &resolved2)) {
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

            /* Line directives are emitted dynamically when content is output,
             * so we don't emit {#line 1} here - the first emitted content will
             * trigger a line directive with the correct line number */

            record_included_file(pp, resolved_path);
            bool ok = preprocess_buffer_internal(pp, resolved_path, include_buffer, include_length, conditions, output, depth + 1, error_message);

            /* Emit line directive returning to parent file */
            if (ok) {
                emit_line_directive(output, current_line + 1, filename);
                skip_trailing_newline = true;
            }

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
        if (!push_conditional(conditions, parent_active, defined, filename, current_line)) {
            free(symbol);
            free(keyword);
            free(content);
            return set_error(error_message, "out of memory");
        }
        free(symbol);
    } else if (strcmp(keyword, "IFOPT") == 0) {
        // {$IFOPT X+} or {$IFOPT X-} - compiler option check
        // Treat all compiler options as disabled by default
        handled = true;
        bool parent_active = current_branch_active(conditions);
        // The condition is false - options are not enabled
        if (!push_conditional(conditions, parent_active, false, filename, current_line)) {
            free(keyword);
            free(content);
            return set_error(error_message, "out of memory");
        }
    } else if (strcmp(keyword, "IF") == 0) {
        handled = true;
        bool cond_value = false;
        /* Update output context for declared() support */
        pp->current_output = output->data;
        pp->current_output_len = output->length;
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
        if (!push_conditional(conditions, parent_active, cond_value, filename, current_line)) {
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
            /* Update output context for declared() support */
            pp->current_output = output->data;
            pp->current_output_len = output->length;
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
    } else if (strcmp(keyword, "ENDIF") == 0 || strcmp(keyword, "IFEND") == 0) {
        handled = true;
        ConditionalFrame *frame = peek_conditional(conditions);
        char *filename_to_free = NULL;
        if (frame) {
            filename_to_free = frame->filename;
        }
        if (!pop_conditional(conditions)) {
            free(keyword);
            free(content);
            return set_error(error_message, "{$ENDIF} without matching {$IF}");
        }
        free(filename_to_free);
    } else if (strcmp(keyword, "MACRO") == 0) {
        handled = true;
        // Handle {$macro on} and {$macro off}
        if (branch_active) {
            const char *arg = rest;
            while (*arg && isspace((unsigned char)*arg)) ++arg;
            
            if (ascii_strncasecmp(arg, "ON", 2) == 0 && 
                !isalnum((unsigned char)arg[2]) && arg[2] != '_') {
                pp->macro_enabled = true;
            } else if (ascii_strncasecmp(arg, "OFF", 3) == 0 && 
                       !isalnum((unsigned char)arg[3]) && arg[3] != '_') {
                pp->macro_enabled = false;
            }
            // Silently ignore invalid arguments
        }
    } else {
        handled = true; // Treat unknown directives as whitespace
        
        // Check if this is a compiler directive that should be preserved in output
        // These directives affect compilation but don't affect preprocessing
        bool should_preserve = false;
        if (strcmp(keyword, "MODE") == 0 ||
            strcmp(keyword, "MODESWITCH") == 0 ||
            strcmp(keyword, "CALLING") == 0 ||
            strcmp(keyword, "CODEPAGE") == 0 ||
            strcmp(keyword, "ALIGN") == 0 ||
            strcmp(keyword, "PACKRECORDS") == 0 ||
            strcmp(keyword, "ASMMODE") == 0 ||
            strcmp(keyword, "PACKENUM") == 0 ||
            strcmp(keyword, "PACKSET") == 0 ||
            strcmp(keyword, "BITPACKING") == 0 ||
            strcmp(keyword, "ASSERTIONS") == 0 ||
            strcmp(keyword, "OPTIMIZATION") == 0 ||
            strcmp(keyword, "INLINE") == 0 ||
            strcmp(keyword, "GOTO") == 0 ||
            strcmp(keyword, "HINTS") == 0 ||
            strcmp(keyword, "WARNINGS") == 0 ||
            strcmp(keyword, "NOTES") == 0 ||
            strcmp(keyword, "WARN") == 0 ||
            strcmp(keyword, "FATAL") == 0 ||
            strcmp(keyword, "ERROR") == 0 ||
            strcmp(keyword, "MESSAGE") == 0 ||
            strcmp(keyword, "STOP") == 0) {
            should_preserve = true;
        }

        // Track {$ASMMODE INTEL/ATT} state for codegen
        if (branch_active && strcmp(keyword, "ASMMODE") == 0) {
            // rest contains the asmmode value (e.g. "INTEL" or "ATT")
            char asmmode_val[32];
            size_t vi = 0;
            for (size_t ri = 0; rest[ri] && vi < sizeof(asmmode_val) - 1; ri++) {
                if (rest[ri] != ' ' && rest[ri] != '\t')
                    asmmode_val[vi++] = rest[ri];
            }
            asmmode_val[vi] = '\0';
            if (strcasecmp(asmmode_val, "INTEL") == 0)
                pp->asmmode_intel = true;
            else if (strcasecmp(asmmode_val, "ATT") == 0 ||
                     strcasecmp(asmmode_val, "DEFAULT") == 0)
                pp->asmmode_intel = false;
        }

        // If we should preserve this directive, output it verbatim
        if (branch_active && should_preserve) {
            // Reconstruct the original directive
            if (!string_builder_append_char(output, '{')) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
            if (!string_builder_append_char(output, '$')) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
            if (!string_builder_append_string(output, keyword)) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
            if (*rest) {
                if (!string_builder_append_char(output, ' ')) {
                    free(keyword);
                    free(content);
                    return set_error(error_message, "out of memory");
                }
                if (!string_builder_append_string(output, rest)) {
                    free(keyword);
                    free(content);
                    return set_error(error_message, "out of memory");
                }
            }
            if (!string_builder_append_char(output, '}')) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
        }
    }

    if (branch_active && handled && strcmp(keyword, "INCLUDE") != 0 && strcmp(keyword, "I") != 0) {
        // Check if we already preserved this directive
        bool already_preserved = false;
        if (strcmp(keyword, "MODE") == 0 ||
            strcmp(keyword, "MODESWITCH") == 0 ||
            strcmp(keyword, "CALLING") == 0 ||
            strcmp(keyword, "CODEPAGE") == 0 ||
            strcmp(keyword, "ALIGN") == 0 ||
            strcmp(keyword, "PACKRECORDS") == 0 ||
            strcmp(keyword, "PACKENUM") == 0 ||
            strcmp(keyword, "PACKSET") == 0 ||
            strcmp(keyword, "BITPACKING") == 0 ||
            strcmp(keyword, "ASSERTIONS") == 0 ||
            strcmp(keyword, "OPTIMIZATION") == 0 ||
            strcmp(keyword, "INLINE") == 0 ||
            strcmp(keyword, "GOTO") == 0 ||
            strcmp(keyword, "HINTS") == 0 ||
            strcmp(keyword, "WARNINGS") == 0 ||
            strcmp(keyword, "NOTES") == 0 ||
            strcmp(keyword, "WARN") == 0 ||
            strcmp(keyword, "FATAL") == 0 ||
            strcmp(keyword, "ERROR") == 0 ||
            strcmp(keyword, "MESSAGE") == 0 ||
            strcmp(keyword, "STOP") == 0) {
            already_preserved = true;
        }
        
        // Only add space if we didn't preserve the directive
        if (!already_preserved) {
            // To avoid token merging when directives appear in-line, insert a single space
            if (!string_builder_append_char(output, ' ')) {
                free(keyword);
                free(content);
                return set_error(error_message, "out of memory");
            }
        }
    }

    free(keyword);
    free(content);

    if (paren_style) {
        *index = end + 1; // Skip the closing '*'
    } else {
        *index = end;
    }

    /* After an include with a {#line} directive, skip the trailing newline
     * of the include directive line. The {#line} directive already accounts
     * for the line transition, so emitting this newline would cause an
     * off-by-one in line tracking. */
    if (skip_trailing_newline) {
        if (*index + 1 < length && input[*index + 1] == '\n')
            (*index)++;
        else if (*index + 1 < length && input[*index + 1] == '\r') {
            (*index)++;
            if (*index + 1 < length && input[*index + 1] == '\n')
                (*index)++;
        }
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

    {
        int line = 1;
        int in_brace_comment = 0;
        int in_paren_comment = 0;
        const char *cursor = data;
        while (*cursor != '\0')
        {
            const char *line_start = cursor;
            const char *line_end = cursor;
            while (*line_end != '\0' && *line_end != '\n' && *line_end != '\r')
                ++line_end;

            const char *p = line_start;
            while (p < line_end)
            {
                if (in_brace_comment)
                {
                    if (*p == '}')
                        in_brace_comment = 0;
                    ++p;
                    continue;
                }
                if (in_paren_comment)
                {
                    if ((p + 1) < line_end && p[0] == '*' && p[1] == ')')
                    {
                        in_paren_comment = 0;
                        p += 2;
                        continue;
                    }
                    ++p;
                    continue;
                }

                if (*p == ' ' || *p == '\t')
                {
                    ++p;
                    continue;
                }
                if ((p + 1) < line_end && p[0] == '/' && p[1] == '/')
                    break;
                if (*p == '{')
                {
                    in_brace_comment = 1;
                    ++p;
                    continue;
                }
                if ((p + 1) < line_end && p[0] == '(' && p[1] == '*')
                {
                    in_paren_comment = 1;
                    p += 2;
                    continue;
                }

                {
                    size_t remaining = (size_t)(line_end - p);
                    int is_conflict_marker =
                        (remaining == 7 && strncmp(p, "=======", 7) == 0) ||
                        (remaining >= 7 &&
                         ((strncmp(p, "<<<<<<<", 7) == 0) ||
                          (strncmp(p, ">>>>>>>", 7) == 0)) &&
                         (remaining == 7 || p[7] == ' ' || p[7] == '\t'));
                    if (is_conflict_marker)
                    {
                        free(data);
                        return set_error(error_message,
                            "merge conflict marker found in '%s' at line %d",
                            filename, line);
                    }
                }

                break;
            }

            cursor = line_end;
            if (*cursor == '\r' && cursor[1] == '\n')
                ++cursor;
            if (*cursor != '\0')
                ++cursor;
            ++line;
        }
    }

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

static bool string_builder_append_string(StringBuilder *sb, const char *str) {
    if (!str) {
        return true;
    }
    while (*str) {
        if (!string_builder_append_char(sb, *str)) {
            return false;
        }
        ++str;
    }
    return true;
}

/* Emit a #line directive to track source file positions across include boundaries.
 * Format: {#line <line_number> "<filename>"}
 * The parser will recognize this and reset its line counter accordingly. */
static bool emit_line_directive(StringBuilder *sb, int line, const char *filename) {
    if (filename == NULL) {
        return true;
    }
    char directive[512];
    /* Escape backslashes in filename for Windows paths */
    char escaped_filename[256];
    size_t j = 0;
    for (size_t i = 0; filename[i] && j < sizeof(escaped_filename) - 1; i++) {
        if (filename[i] == '\\') {
            if (j < sizeof(escaped_filename) - 2) {
                escaped_filename[j++] = '\\';
                escaped_filename[j++] = '\\';
            }
        } else {
            escaped_filename[j++] = filename[i];
        }
    }
    escaped_filename[j] = '\0';

    snprintf(directive, sizeof(directive), "\n{#line %d \"%s\"}\n", line, escaped_filename);
    return string_builder_append_string(sb, directive);
}

static void string_builder_free(StringBuilder *sb) {
    free(sb->data);
    sb->data = NULL;
    sb->length = 0;
    sb->capacity = 0;
}

static char *string_builder_finalize(StringBuilder *sb) {
    char *data = sb->data;
    sb->data = NULL;
    sb->length = 0;
    sb->capacity = 0;
    return data;
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

static bool push_conditional(ConditionalStack *stack, bool parent_active, bool condition, const char *filename, int line) {
    if (!ensure_capacity((void **)&stack->items, sizeof(ConditionalFrame), &stack->capacity, stack->size + 1)) {
        return false;
    }
    ConditionalFrame *frame = &stack->items[stack->size++];
    frame->parent_allows = parent_active;
    frame->branch_taken = parent_active && condition;
    frame->active = frame->branch_taken;
    frame->saw_else = false;
    frame->filename = filename ? my_strdup(filename) : NULL;
    frame->line = line;
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

// Try to expand a macro at the current position
// Returns the macro value if expansion should happen, NULL otherwise
// Sets *out_identifier_len to the length of the identifier to skip
static const char *try_expand_macro(PascalPreprocessor *pp, const char *input, size_t length, size_t pos, size_t *out_identifier_len) {
    *out_identifier_len = 0;
    
    if (!pp->macro_enabled) {
        return NULL;
    }
    
    // Check if we're at the start of an identifier
    if (pos >= length) {
        return NULL;
    }
    
    char c = input[pos];
    if (!isalpha((unsigned char)c) && c != '_') {
        return NULL;
    }
    
    // Check if we're in the middle of an identifier (not at the start)
    // If the previous character is alphanumeric or underscore, we're not at the start
    if (pos > 0) {
        char prev = input[pos - 1];
        if (isalnum((unsigned char)prev) || prev == '_') {
            return NULL;
        }
    }
    
    // Extract only the first identifier segment (stop at dot for qualified names)
    // FPC macros expand just the macro name, e.g., UT in UT.ARG_MAX expands to UnixType.ARG_MAX
    size_t start = pos;
    size_t end = pos;
    while (end < length && (isalnum((unsigned char)input[end]) || input[end] == '_')) {
        ++end;
    }
    
    // Create identifier string (without any trailing dot or qualified parts)
    size_t id_len = end - start;
    char *identifier = malloc(id_len + 1);
    if (!identifier) {
        return NULL;
    }
    memcpy(identifier, &input[start], id_len);
    identifier[id_len] = '\0';
    
    // Look up the macro - only expand if it's an actual macro (defined with :=)
    // Simple conditional defines (like UNIX, LINUX) should NOT be expanded
    const char *value = get_macro_value(pp, identifier);
    free(identifier);
    
    if (value) {
        *out_identifier_len = id_len;
        return value;
    }
    
    return NULL;
}

static char *expand_macro_value_once(PascalPreprocessor *pp, const char *value) {
    if (!pp || !value) {
        return NULL;
    }
    if (!pp->macro_enabled) {
        return strdup(value);
    }

    StringBuilder output;
    string_builder_init(&output);

    bool in_string = false;
    char string_delim = '\0';
    size_t length = strlen(value);

    for (size_t i = 0; i < length; ++i) {
        char c = value[i];
        if (in_string) {
            if (c == string_delim) {
                if (string_delim == '\'' && i + 1 < length && value[i + 1] == '\'') {
                    if (!string_builder_append_char(&output, c)) {
                        string_builder_free(&output);
                        return NULL;
                    }
                    ++i;
                    c = value[i];
                } else {
                    in_string = false;
                    string_delim = '\0';
                }
            }
        } else if (c == '\'' || c == '"') {
            in_string = true;
            string_delim = c;
        }

        if (!in_string) {
            size_t identifier_len = 0;
            const char *macro_value = try_expand_macro(pp, value, length, i, &identifier_len);
            if (macro_value) {
                if (!string_builder_append_string(&output, macro_value)) {
                    string_builder_free(&output);
                    return NULL;
                }
                i += identifier_len - 1;
                continue;
            }
        }

        if (!string_builder_append_char(&output, c)) {
            string_builder_free(&output);
            return NULL;
        }
    }

    return string_builder_finalize(&output);
}

static bool define_symbol(PascalPreprocessor *pp, const char *symbol) {
    if (!pp || !symbol || symbol[0] == '\0') {
        return false;
    }

    // Parse symbol for optional value assignment :=
    // Format: NAME [:= VALUE]
    char *name_part = NULL;
    char *value_part = NULL;
    bool is_macro = false;  // Track if this is an explicit macro (with := assignment)

    const char *assign_pos = strstr(symbol, ":=");
    if (assign_pos) {
        is_macro = true;  // Explicit assignment makes this a macro
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
        // Default value for simple defines is "1" (for conditional evaluation)
        // But this is NOT a macro - should not be expanded in text
        value_part = strdup("1");
        is_macro = false;
    }

    if (!name_part || !value_part) {
        free(name_part);
        free(value_part);
        return false;
    }

    uppercase(name_part);

    // Check if already defined, if so update value and macro status
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, name_part) == 0) {
            free(pp->defines[i].value);
            pp->defines[i].value = value_part;
            pp->defines[i].is_macro = is_macro;
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
    pp->defines[pp->define_count].is_macro = is_macro;
    pp->define_count++;
    return true;
}

/* Define a symbol with a value but is_macro = false, so it's available for {$if}
 * evaluation via get_symbol_value() but NOT for text replacement via get_macro_value(). */
static bool define_symbol_const(PascalPreprocessor *pp, const char *name, const char *value) {
    if (!pp || !name || !value) return false;

    char *name_part = strdup(name);
    char *value_part = strdup(value);
    if (!name_part || !value_part) {
        free(name_part);
        free(value_part);
        return false;
    }
    uppercase(name_part);

    /* Update existing entry if present */
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, name_part) == 0) {
            free(pp->defines[i].value);
            pp->defines[i].value = value_part;
            pp->defines[i].is_macro = false;
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
    pp->defines[pp->define_count].is_macro = false;
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

/* Evaluate a simple const expression from preprocessed Pascal source.
 * Handles: integers, identifier references (recursive), +, -, *, div, mod, parentheses.
 * This is a mini expression evaluator for const initializer expressions found in source. */
static bool eval_const_expr(PascalPreprocessor *pp, const char **cursor, const char *end, int64_t *value, int depth);

static void skip_spaces_in_range(const char **cursor, const char *end) {
    while (*cursor < end && isspace((unsigned char)**cursor))
        ++(*cursor);
}

/* Resolve the size of a built-in Pascal type by name (case-insensitive).
 * Returns true and sets *size if the type is recognized. */
static bool resolve_builtin_type_size(PascalPreprocessor *pp, const char *type_name_input, int64_t *size) {
    char *type_name = strdup(type_name_input);
    if (!type_name) return false;
    uppercase(type_name);

    bool cpu64 = pascal_preprocessor_is_defined(pp, "CPU64");
    bool found = false;

    if (strcmp(type_name, "NATIVEINT") == 0 || strcmp(type_name, "NATIVEUINT") == 0 ||
        strcmp(type_name, "SIZEINT") == 0 || strcmp(type_name, "SIZEUINT") == 0 ||
        strcmp(type_name, "PTRINT") == 0 || strcmp(type_name, "PTRUINT") == 0 ||
        strcmp(type_name, "INTPTR") == 0 || strcmp(type_name, "UINTPTR") == 0 ||
        strcmp(type_name, "TCONSTPTRUINT") == 0 || strcmp(type_name, "TCONSTPTRINT") == 0 ||
        strcmp(type_name, "PUINT") == 0 || strcmp(type_name, "PINT") == 0 ||
        strcmp(type_name, "AINT") == 0 || strcmp(type_name, "ASIZEINT") == 0 ||
        strcmp(type_name, "POINTER") == 0) {
        *size = cpu64 ? 8 : 4;
        found = true;
    }
    else if (strcmp(type_name, "INT64") == 0 || strcmp(type_name, "QWORD") == 0 || strcmp(type_name, "UINT64") == 0 ||
        strcmp(type_name, "TSYSPARAM") == 0 || strcmp(type_name, "ALUUINT") == 0 ||
        strcmp(type_name, "ALUSINT") == 0 || strcmp(type_name, "VALSINT") == 0 || strcmp(type_name, "VALUINT") == 0 ||
        strcmp(type_name, "AINTMAX") == 0 || strcmp(type_name, "TBITSBASE") == 0 ||
        strcmp(type_name, "TUNSIGNEDINTTYPE") == 0 || strcmp(type_name, "TSIGNEDINTTYPE") == 0 ||
        strcmp(type_name, "HANDLE") == 0 || strcmp(type_name, "THANDLE") == 0 ||
        strcmp(type_name, "TLARGEINTEGER") == 0 || strcmp(type_name, "TTHREADID") == 0) {
        *size = 8;
        found = true;
    }
    else if (strcmp(type_name, "INTEGER") == 0 || strcmp(type_name, "LONGINT") == 0 ||
             strcmp(type_name, "INT32") == 0 || strcmp(type_name, "CARDINAL") == 0 ||
             strcmp(type_name, "DWORD") == 0 || strcmp(type_name, "UINT32") == 0 ||
             strcmp(type_name, "LONGWORD") == 0 || strcmp(type_name, "LONGBOOL") == 0) {
        *size = 4;
        found = true;
    }
    else if (strcmp(type_name, "SMALLINT") == 0 || strcmp(type_name, "INT16") == 0 ||
             strcmp(type_name, "WORD") == 0 || strcmp(type_name, "UINT16") == 0 ||
             strcmp(type_name, "WIDECHAR") == 0 || strcmp(type_name, "WORDBOOL") == 0 ||
             strcmp(type_name, "TCOMPILERWIDECHAR") == 0) {
        *size = 2;
        found = true;
    }
    else if (strcmp(type_name, "SHORTINT") == 0 || strcmp(type_name, "INT8") == 0 ||
             strcmp(type_name, "BYTE") == 0 || strcmp(type_name, "UINT8") == 0 ||
             strcmp(type_name, "CHAR") == 0 || strcmp(type_name, "BOOLEAN") == 0 ||
             strcmp(type_name, "ANSICHAR") == 0 || strcmp(type_name, "BYTEBOOL") == 0) {
        *size = 1;
        found = true;
    }
    else if (strcmp(type_name, "VALREAL") == 0 || strcmp(type_name, "EXTENDED") == 0 ||
             strcmp(type_name, "LONGDOUBLE") == 0 || strcmp(type_name, "BESTREALREC") == 0 ||
             strcmp(type_name, "TEXTENDED80REC") == 0 || strcmp(type_name, "BESTREAL") == 0) {
        *size = 10;
        found = true;
    }
    else if (strcmp(type_name, "DOUBLE") == 0 || strcmp(type_name, "REAL") == 0) {
        *size = 8;
        found = true;
    }
    else if (strcmp(type_name, "SINGLE") == 0 || strcmp(type_name, "FLOAT") == 0) {
        *size = 4;
        found = true;
    }
    else if (strcmp(type_name, "COMP") == 0 || strcmp(type_name, "CURRENCY") == 0) {
        *size = 8;
        found = true;
    }
    else if (strcmp(type_name, "TIME_T") == 0 || strcmp(type_name, "CLONG") == 0 ||
             strcmp(type_name, "CULONG") == 0 || strcmp(type_name, "CLONGLONG") == 0 ||
             strcmp(type_name, "CULONGLONG") == 0 || strcmp(type_name, "CSIZE_T") == 0 ||
             strcmp(type_name, "CSSIZE_T") == 0 || strcmp(type_name, "COFF_T") == 0 ||
             strcmp(type_name, "OFF_T") == 0) {
        *size = 8;
        found = true;
    }
    else if (strcmp(type_name, "CINT") == 0 || strcmp(type_name, "CUINT") == 0 ||
             strcmp(type_name, "CINT32") == 0 || strcmp(type_name, "CUINT32") == 0) {
        *size = 4;
        found = true;
    }
    else if (strcmp(type_name, "CSHORT") == 0 || strcmp(type_name, "CUSHORT") == 0 ||
             strcmp(type_name, "CINT16") == 0 || strcmp(type_name, "CUINT16") == 0) {
        *size = 2;
        found = true;
    }
    else if (strcmp(type_name, "CCHAR") == 0 || strcmp(type_name, "CSCHAR") == 0 ||
             strcmp(type_name, "CUCHAR") == 0 || strcmp(type_name, "CINT8") == 0 ||
             strcmp(type_name, "CUINT8") == 0) {
        *size = 1;
        found = true;
    }
    else if (strcmp(type_name, "CODEPTRUINT") == 0 || strcmp(type_name, "CODEPTRINT") == 0 ||
             strcmp(type_name, "CODEPOINTER") == 0) {
        *size = cpu64 ? 8 : 4;
        found = true;
    }
    else if (strstr(type_name, "POINTER") != NULL || type_name[0] == 'P') {
        *size = cpu64 ? 8 : 4;
        found = true;
    }

    free(type_name);
    return found;
}

/* Compute the size of a packed record type by scanning the preprocessed source.
 * Looks for: TYPE_NAME = packed record FIELD1: TYPE1; FIELD2: TYPE2; ... end;
 * Returns the sum of field sizes (no alignment padding for packed records). */
static bool compute_packed_record_size_from_source(PascalPreprocessor *pp, const char *type_name, int64_t *size) {
    if (!pp || !pp->current_output || pp->current_output_len == 0) return false;

    const char *src = pp->current_output;
    size_t src_len = pp->current_output_len;
    const char *src_end = src + src_len;
    size_t name_len = strlen(type_name);

    const char *pos = src;
    while (pos + name_len < src_end) {
        /* Find the type name (case-insensitive whole word) */
        bool match = true;
        for (size_t i = 0; i < name_len; i++) {
            if (ascii_tolower(pos[i]) != ascii_tolower(type_name[i])) {
                match = false;
                break;
            }
        }
        if (!match || (pos > src && (isalnum((unsigned char)pos[-1]) || pos[-1] == '_')) ||
            (pos + name_len < src_end && (isalnum((unsigned char)pos[name_len]) || pos[name_len] == '_'))) {
            pos++;
            continue;
        }

        /* Check for: <whitespace> = <whitespace> packed <whitespace> record */
        const char *p = pos + name_len;
        while (p < src_end && isspace((unsigned char)*p)) p++;
        if (p >= src_end || *p != '=') { pos++; continue; }
        p++; /* skip '=' */
        while (p < src_end && isspace((unsigned char)*p)) p++;
        /* Check for "packed" keyword */
        bool is_packed = false;
        if (p + 6 <= src_end && ascii_strncasecmp(p, "packed", 6) == 0 &&
            (p + 6 >= src_end || !isalnum((unsigned char)p[6]))) {
            is_packed = true;
            p += 6;
            while (p < src_end && isspace((unsigned char)*p)) p++;
        }

        /* Check for "record" keyword */
        if (p + 6 > src_end || ascii_strncasecmp(p, "record", 6) != 0 ||
            (p + 6 < src_end && isalnum((unsigned char)p[6]))) {
            pos++;
            continue;
        }
        p += 6; /* skip "record" */

        /* Now parse fields until "end" keyword.
         * Each field is: field_name [, field_name ...] : type_name ; */
        int64_t total_size = 0;
        bool success = true;

        while (p < src_end) {
            while (p < src_end && isspace((unsigned char)*p)) p++;
            if (p >= src_end) { success = false; break; }

            /* Skip // comments */
            while (p + 1 < src_end && p[0] == '/' && p[1] == '/') {
                while (p < src_end && *p != '\n') p++;
                while (p < src_end && isspace((unsigned char)*p)) p++;
            }
            /* Skip { } comments */
            while (p < src_end && *p == '{') {
                while (p < src_end && *p != '}') p++;
                if (p < src_end) p++; /* skip '}' */
                while (p < src_end && isspace((unsigned char)*p)) p++;
            }
            /* Skip (* *) comments */
            while (p + 1 < src_end && p[0] == '(' && p[1] == '*') {
                p += 2;
                while (p + 1 < src_end && !(p[0] == '*' && p[1] == ')')) p++;
                if (p + 1 < src_end) p += 2;
                while (p < src_end && isspace((unsigned char)*p)) p++;
            }
            if (p >= src_end) { success = false; break; }

            /* Check for "end" keyword */
            if (p + 3 <= src_end && ascii_strncasecmp(p, "end", 3) == 0 &&
                (p + 3 >= src_end || (!isalnum((unsigned char)p[3]) && p[3] != '_'))) {
                break;
            }

            /* Parse field name(s) - skip to ':' */
            const char *colon = p;
            while (colon < src_end && *colon != ':' && *colon != ';') colon++;
            if (colon >= src_end || *colon != ':') { success = false; break; }

            /* Count field names (separated by commas) */
            int field_count = 1;
            for (const char *c = p; c < colon; c++) {
                if (*c == ',') field_count++;
            }

            colon++; /* skip ':' */
            while (colon < src_end && isspace((unsigned char)*colon)) colon++;

            /* Parse field type name */
            const char *ftype_start = colon;
            while (colon < src_end && (isalnum((unsigned char)*colon) || *colon == '_')) colon++;
            if (colon == ftype_start) { success = false; break; }

            char *field_type = malloc((size_t)(colon - ftype_start) + 1);
            if (!field_type) { success = false; break; }
            memcpy(field_type, ftype_start, (size_t)(colon - ftype_start));
            field_type[colon - ftype_start] = '\0';

            /* Resolve field type size */
            int64_t field_size;
            if (!resolve_builtin_type_size(pp, field_type, &field_size)) {
                /* Try resolving as a type alias from const/type declarations */
                /* For type aliases like "StringDataHeaderHashType = uint16", try const lookup */
                bool resolved = false;
                /* Scan source for TYPE_ALIAS = <builtin_type> pattern */
                if (pp->current_output) {
                    const char *s = pp->current_output;
                    size_t ft_len = strlen(field_type);
                    while (s + ft_len < src_end) {
                        bool m = true;
                        for (size_t i = 0; i < ft_len; i++) {
                            if (ascii_tolower(s[i]) != ascii_tolower(field_type[i])) { m = false; break; }
                        }
                        if (m && (s == pp->current_output || (!isalnum((unsigned char)s[-1]) && s[-1] != '_')) &&
                            (s + ft_len >= src_end || (!isalnum((unsigned char)s[ft_len]) && s[ft_len] != '_'))) {
                            const char *after = s + ft_len;
                            while (after < src_end && isspace((unsigned char)*after)) after++;
                            if (after < src_end && *after == '=' && (after <= s || after[-1] != ':')) {
                                after++;
                                while (after < src_end && isspace((unsigned char)*after)) after++;
                                const char *tstart = after;
                                while (after < src_end && (isalnum((unsigned char)*after) || *after == '_')) after++;
                                if (after > tstart) {
                                    char *resolved_type = malloc((size_t)(after - tstart) + 1);
                                    if (resolved_type) {
                                        memcpy(resolved_type, tstart, (size_t)(after - tstart));
                                        resolved_type[after - tstart] = '\0';
                                        if (resolve_builtin_type_size(pp, resolved_type, &field_size)) {
                                            resolved = true;
                                        }
                                        free(resolved_type);
                                    }
                                }
                            }
                        }
                        if (resolved) break;
                        s++;
                    }
                }
                if (!resolved) {
                    free(field_type);
                    success = false;
                    break;
                }
            }
            free(field_type);

            if (is_packed) {
                total_size += field_size * field_count;
            } else {
                /* For non-packed records, use natural alignment (simplified) */
                for (int i = 0; i < field_count; i++) {
                    int64_t align = field_size;
                    if (align > 8) align = 8;
                    if (align > 1) {
                        int64_t rem = total_size % align;
                        if (rem != 0) total_size += align - rem;
                    }
                    total_size += field_size;
                }
            }

            /* Skip to semicolon */
            while (colon < src_end && *colon != ';') colon++;
            if (colon < src_end) colon++; /* skip ';' */
            p = colon;
        }

        if (success) {
            *size = total_size;
            return true;
        }
        pos++;
    }
    return false;
}

static bool eval_const_factor(PascalPreprocessor *pp, const char **cursor, const char *end, int64_t *value, int depth) {
    if (depth > 32) return false;
    skip_spaces_in_range(cursor, end);
    if (*cursor >= end) return false;

    /* Unary NOT */
    if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "NOT", 3) == 0 &&
        (*cursor + 3 >= end || (!isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_'))) {
        *cursor += 3;
        int64_t inner;
        if (!eval_const_factor(pp, cursor, end, &inner, depth + 1)) return false;
        *value = !inner;
        return true;
    }
    /* Unary minus */
    if (**cursor == '-') {
        ++(*cursor);
        int64_t inner;
        if (!eval_const_factor(pp, cursor, end, &inner, depth + 1)) return false;
        *value = -inner;
        return true;
    }
    /* Unary plus */
    if (**cursor == '+') {
        ++(*cursor);
        return eval_const_factor(pp, cursor, end, value, depth + 1);
    }
    /* Parenthesized expression */
    if (**cursor == '(') {
        ++(*cursor);
        if (!eval_const_expr(pp, cursor, end, value, depth + 1)) return false;
        skip_spaces_in_range(cursor, end);
        if (*cursor < end && **cursor == ')') ++(*cursor);
        return true;
    }
    /* Hex literal */
    if (**cursor == '$') {
        ++(*cursor);
        if (*cursor >= end || !isxdigit((unsigned char)**cursor)) return false;
        int64_t v = 0;
        while (*cursor < end && isxdigit((unsigned char)**cursor)) {
            int d;
            char c = **cursor;
            if (c >= '0' && c <= '9') d = c - '0';
            else if (c >= 'a' && c <= 'f') d = c - 'a' + 10;
            else d = c - 'A' + 10;
            v = v * 16 + d;
            ++(*cursor);
        }
        *value = v;
        return true;
    }
    /* Number */
    if (isdigit((unsigned char)**cursor)) {
        int64_t v = 0;
        while (*cursor < end && isdigit((unsigned char)**cursor)) {
            v = v * 10 + (**cursor - '0');
            ++(*cursor);
        }
        *value = v;
        return true;
    }
    /* Identifier - recursive const lookup */
    if (isalpha((unsigned char)**cursor) || **cursor == '_') {
        const char *id_start = *cursor;
        while (*cursor < end && (isalnum((unsigned char)**cursor) || **cursor == '_'))
            ++(*cursor);
        size_t id_len = (size_t)(*cursor - id_start);
        char *id = malloc(id_len + 1);
        if (!id) return false;
        memcpy(id, id_start, id_len);
        id[id_len] = '\0';

        /* Check if it's TRUE/FALSE */
        if (ascii_strncasecmp(id, "TRUE", 5) == 0 && id_len == 4) { *value = 1; free(id); return true; }
        if (ascii_strncasecmp(id, "FALSE", 6) == 0 && id_len == 5) { *value = 0; free(id); return true; }

        /* Handle sizeof(TYPE) calls */
        if (id_len == 6 && ascii_strncasecmp(id, "SIZEOF", 6) == 0) {
            free(id);
            skip_spaces_in_range(cursor, end);
            if (*cursor >= end || **cursor != '(') return false;
            ++(*cursor); /* skip '(' */
            skip_spaces_in_range(cursor, end);

            const char *type_start = *cursor;
            if (*cursor >= end || (!isalpha((unsigned char)**cursor) && **cursor != '_')) return false;
            while (*cursor < end && (isalnum((unsigned char)**cursor) || **cursor == '_'))
                ++(*cursor);
            size_t type_len = (size_t)(*cursor - type_start);
            char *type_name = malloc(type_len + 1);
            if (!type_name) return false;
            memcpy(type_name, type_start, type_len);
            type_name[type_len] = '\0';

            skip_spaces_in_range(cursor, end);
            if (*cursor < end && **cursor == ')') ++(*cursor);

            /* Try built-in type table */
            int64_t sz;
            if (resolve_builtin_type_size(pp, type_name, &sz)) {
                *value = sz;
                free(type_name);
                return true;
            }
            /* Try computing packed record size from source */
            if (compute_packed_record_size_from_source(pp, type_name, &sz)) {
                *value = sz;
                free(type_name);
                return true;
            }
            free(type_name);
            return false;
        }

        /* Try preprocessor defines first */
        const char *val_str = get_symbol_value(pp, id);
        if (val_str) {
            char *endp;
            int64_t v = strtoll(val_str, &endp, 0);
            if (*endp == '\0') {
                *value = v;
                free(id);
                return true;
            }
        }
        /* Try const lookup from source */
        int64_t v;
        if (find_const_integer_in_source(pp, id, &v)) {
            *value = v;
            free(id);
            return true;
        }
        free(id);
        return false;
    }
    return false;
}

static bool eval_const_term(PascalPreprocessor *pp, const char **cursor, const char *end, int64_t *value, int depth) {
    if (!eval_const_factor(pp, cursor, end, value, depth)) return false;
    for (;;) {
        skip_spaces_in_range(cursor, end);
        if (*cursor >= end) break;
        if (**cursor == '*') {
            ++(*cursor);
            int64_t right;
            if (!eval_const_factor(pp, cursor, end, &right, depth)) return false;
            *value *= right;
        } else if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "DIV", 3) == 0 &&
                   (*cursor + 3 >= end || !isalnum((unsigned char)(*cursor)[3]))) {
            *cursor += 3;
            int64_t right;
            if (!eval_const_factor(pp, cursor, end, &right, depth)) return false;
            if (right == 0) return false;
            *value /= right;
        } else if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "MOD", 3) == 0 &&
                   (*cursor + 3 >= end || !isalnum((unsigned char)(*cursor)[3]))) {
            *cursor += 3;
            int64_t right;
            if (!eval_const_factor(pp, cursor, end, &right, depth)) return false;
            if (right == 0) return false;
            *value %= right;
        } else if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "SHL", 3) == 0 &&
                   (*cursor + 3 >= end || !isalnum((unsigned char)(*cursor)[3]))) {
            *cursor += 3;
            int64_t right;
            if (!eval_const_factor(pp, cursor, end, &right, depth)) return false;
            if (right < 0 || right >= 64) return false;
            *value <<= right;
        } else if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "SHR", 3) == 0 &&
                   (*cursor + 3 >= end || !isalnum((unsigned char)(*cursor)[3]))) {
            *cursor += 3;
            int64_t right;
            if (!eval_const_factor(pp, cursor, end, &right, depth)) return false;
            if (right < 0 || right >= 64) return false;
            *value >>= right;
        } else {
            break;
        }
    }
    return true;
}

/* Parse additive: term (('+' | '-') term)* */
static bool eval_const_additive(PascalPreprocessor *pp, const char **cursor, const char *end, int64_t *value, int depth) {
    if (!eval_const_term(pp, cursor, end, value, depth)) return false;
    for (;;) {
        skip_spaces_in_range(cursor, end);
        if (*cursor >= end) break;
        if (**cursor == '+') {
            ++(*cursor);
            int64_t right;
            if (!eval_const_term(pp, cursor, end, &right, depth)) return false;
            *value += right;
        } else if (**cursor == '-') {
            ++(*cursor);
            int64_t right;
            if (!eval_const_term(pp, cursor, end, &right, depth)) return false;
            *value -= right;
        } else {
            break;
        }
    }
    return true;
}

/* Parse comparison: additive (('=' | '<>' | '<=' | '>=' | '<' | '>') additive)? */
static bool eval_const_comparison(PascalPreprocessor *pp, const char **cursor, const char *end, int64_t *value, int depth) {
    if (!eval_const_additive(pp, cursor, end, value, depth)) return false;
    for (;;) {
        skip_spaces_in_range(cursor, end);
        if (*cursor >= end) break;
        if (*cursor + 1 < end && **cursor == '<' && (*cursor)[1] == '=') {
            *cursor += 2;
            int64_t right;
            if (!eval_const_additive(pp, cursor, end, &right, depth)) return false;
            *value = (*value <= right) ? 1 : 0;
        } else if (*cursor + 1 < end && **cursor == '>' && (*cursor)[1] == '=') {
            *cursor += 2;
            int64_t right;
            if (!eval_const_additive(pp, cursor, end, &right, depth)) return false;
            *value = (*value >= right) ? 1 : 0;
        } else if (*cursor + 1 < end && **cursor == '<' && (*cursor)[1] == '>') {
            *cursor += 2;
            int64_t right;
            if (!eval_const_additive(pp, cursor, end, &right, depth)) return false;
            *value = (*value != right) ? 1 : 0;
        } else if (**cursor == '<') {
            ++(*cursor);
            int64_t right;
            if (!eval_const_additive(pp, cursor, end, &right, depth)) return false;
            *value = (*value < right) ? 1 : 0;
        } else if (**cursor == '>') {
            ++(*cursor);
            int64_t right;
            if (!eval_const_additive(pp, cursor, end, &right, depth)) return false;
            *value = (*value > right) ? 1 : 0;
        } else if (**cursor == '=') {
            ++(*cursor);
            int64_t right;
            if (!eval_const_additive(pp, cursor, end, &right, depth)) return false;
            *value = (*value == right) ? 1 : 0;
        } else {
            break;
        }
    }
    return true;
}

/* Parse expression: comparison (('and' | 'or') comparison)* */
static bool eval_const_expr(PascalPreprocessor *pp, const char **cursor, const char *end, int64_t *value, int depth) {
    if (!eval_const_comparison(pp, cursor, end, value, depth)) return false;
    for (;;) {
        skip_spaces_in_range(cursor, end);
        if (*cursor >= end) break;
        if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "AND", 3) == 0 &&
            (*cursor + 3 >= end || !isalnum((unsigned char)(*cursor)[3]))) {
            *cursor += 3;
            int64_t right;
            if (!eval_const_comparison(pp, cursor, end, &right, depth)) return false;
            *value = (*value && right) ? 1 : 0;
        } else if (*cursor + 2 <= end && ascii_strncasecmp(*cursor, "OR", 2) == 0 &&
                   (*cursor + 2 >= end || !isalnum((unsigned char)(*cursor)[2]))) {
            *cursor += 2;
            int64_t right;
            if (!eval_const_comparison(pp, cursor, end, &right, depth)) return false;
            *value = (*value || right) ? 1 : 0;
        } else if (*cursor + 3 <= end && ascii_strncasecmp(*cursor, "NOT", 3) == 0 &&
                   (*cursor + 3 >= end || !isalnum((unsigned char)(*cursor)[3]))) {
            /* NOT as unary prefix - shouldn't appear here but handle for robustness */
            break;
        } else {
            break;
        }
    }
    return true;
}

/* Search the already-preprocessed output for a Pascal const declaration like:
 *   IDENTIFIER = EXPRESSION ;
 * Evaluate the expression and cache the result in the defines table.
 * Returns true if the const was found and evaluated successfully. */
static bool find_const_integer_in_source(PascalPreprocessor *pp, const char *symbol, int64_t *out_value) {
    if (!pp || !symbol || !pp->current_output || pp->current_output_len == 0)
        return false;

    size_t sym_len = strlen(symbol);
    if (sym_len == 0) return false;

    const char *src = pp->current_output;
    size_t src_len = pp->current_output_len;

    /* Scan for pattern: IDENTIFIER = EXPRESSION ;
     * We look for the identifier followed by = (but not :=) and ending with ; */
    const char *pos = src;
    const char *src_end = src + src_len;

    while (pos < src_end) {
        /* Find the symbol name (case-insensitive) */
        const char *found = pos;
        while (found + sym_len <= src_end) {
            /* Simple case-insensitive substring search */
            bool match = true;
            for (size_t i = 0; i < sym_len; i++) {
                if (ascii_tolower(found[i]) != ascii_tolower(symbol[i])) {
                    match = false;
                    break;
                }
            }
            if (match) {
                /* Check that it's a whole word: char before must not be alnum/_, char after must not be alnum/_ */
                bool word_start = (found == src || (!isalnum((unsigned char)found[-1]) && found[-1] != '_'));
                bool word_end = (found + sym_len >= src_end || (!isalnum((unsigned char)found[sym_len]) && found[sym_len] != '_'));
                if (word_start && word_end) break;
            }
            found++;
        }
        if (found + sym_len > src_end) break;

        /* Now check that what follows is whitespace then '=' (but not ':=') */
        const char *after = found + sym_len;
        while (after < src_end && isspace((unsigned char)*after)) after++;

        if (after < src_end && *after == '=') {
            /* Make sure it's not ':=' by checking the char before '=' */
            if (after > src && after[-1] == ':') {
                pos = after + 1;
                continue;
            }
            after++; /* skip '=' */

            /* Find the semicolon that ends this const value */
            const char *semi = after;
            int paren_depth = 0;
            while (semi < src_end) {
                if (*semi == '(') paren_depth++;
                else if (*semi == ')') paren_depth--;
                else if (*semi == ';' && paren_depth <= 0) break;
                semi++;
            }
            if (semi >= src_end) {
                pos = after;
                continue;
            }

            /* Try to evaluate the expression between after and semi */
            const char *expr_cursor = after;
            int64_t val;
            if (eval_const_expr(pp, &expr_cursor, semi, &val, 0)) {
                /* Verify we consumed all meaningful content */
                skip_spaces_in_range(&expr_cursor, semi);
                if (expr_cursor >= semi) {
                    /* Cache the value */
                    char val_str[32];
                    snprintf(val_str, sizeof(val_str), "%lld", (long long)val);
                    define_symbol_const(pp, symbol, val_str);
                    *out_value = val;
                    return true;
                }
            }
            /* Found the const declaration but couldn't evaluate the expression
             * (e.g. complex initializers with function calls, pointer arithmetic,
             * address-of like PtrUint(@Node(nil^).field)).  Return 0 as a
             * best-effort fallback so {$if} assertions using this const don't
             * cause a preprocessor error. */
            *out_value = 0;
            return true;
        }
        pos = found + 1;
    }
    return false;
}

/* Scan source text for Pascal declarations of the given identifier.
 * This is used by declared() to check if an identifier has been declared
 * in the Pascal source (var, const, type, function, procedure).
 * Returns true if a declaration pattern is found. */
static bool symbol_is_declared_in_source(const char *source, size_t source_len, const char *symbol) {
    if (!source || !symbol || source_len == 0) {
        return false;
    }
    
    size_t sym_len = strlen(symbol);
    if (sym_len == 0) {
        return false;
    }
    
    /* Declaration keywords to look for */
    static const char *decl_keywords[] = { "var", "const", "type", "function", "procedure", NULL };
    
    /* Scan the source for declaration patterns */
    const char *pos = source;
    const char *end = source + source_len;
    
    while (pos < end) {
        /* Skip whitespace */
        while (pos < end && isspace((unsigned char)*pos)) {
            pos++;
        }
        if (pos >= end) break;
        
        /* Skip comments: { }, (* *), and // */
        if (*pos == '{') {
            /* Brace comment - skip until closing brace */
            pos++;
            while (pos < end && *pos != '}') {
                pos++;
            }
            if (pos < end) pos++;  /* Skip closing brace */
            continue;
        }
        if (pos + 1 < end && *pos == '(' && *(pos + 1) == '*') {
            /* (* *) comment - skip until closing *) */
            pos += 2;
            while (pos + 1 < end && !(*pos == '*' && *(pos + 1) == ')')) {
                pos++;
            }
            if (pos + 1 < end) pos += 2;  /* Skip closing *) */
            continue;
        }
        if (pos + 1 < end && *pos == '/' && *(pos + 1) == '/') {
            /* // comment - skip until end of line */
            pos += 2;
            while (pos < end && *pos != '\n') {
                pos++;
            }
            if (pos < end) pos++;  /* Skip newline */
            continue;
        }
        
        /* Skip string literals: 'string' */
        if (*pos == '\'') {
            pos++;
            while (pos < end) {
                if (*pos == '\'') {
                    pos++;
                    /* Check for escaped quote ('') */
                    if (pos < end && *pos == '\'') {
                        pos++;  /* Skip second quote of escaped pair */
                        continue;
                    }
                    break;  /* End of string */
                }
                pos++;
            }
            continue;
        }
        
        /* Check for declaration keywords */
        bool found_keyword = false;
        for (int i = 0; decl_keywords[i] != NULL; i++) {
            size_t kw_len = strlen(decl_keywords[i]);
            if ((size_t)(end - pos) >= kw_len && 
                ascii_strncasecmp(pos, decl_keywords[i], kw_len) == 0 &&
                (pos + kw_len >= end || !isalnum((unsigned char)pos[kw_len]))) {
                pos += kw_len;
                found_keyword = true;
                break;
            }
        }
        
        if (found_keyword) {
            /* After a declaration keyword, look for the identifier */
            /* Skip whitespace */
            while (pos < end && isspace((unsigned char)*pos)) {
                pos++;
            }
            
            /* Now check identifiers after this keyword until we hit another keyword or section */
            while (pos < end) {
                /* Skip whitespace */
                while (pos < end && isspace((unsigned char)*pos)) {
                    pos++;
                }
                if (pos >= end) break;

                /* Skip comments: { }, (* *), and // */
                if (*pos == '{') {
                    pos++;
                    while (pos < end && *pos != '}') pos++;
                    if (pos < end) pos++;
                    continue;
                }
                if (pos + 1 < end && *pos == '(' && *(pos + 1) == '*') {
                    pos += 2;
                    while (pos + 1 < end && !(*pos == '*' && *(pos + 1) == ')')) pos++;
                    if (pos + 1 < end) pos += 2;
                    continue;
                }
                if (pos + 1 < end && *pos == '/' && *(pos + 1) == '/') {
                    pos += 2;
                    while (pos < end && *pos != '\n') pos++;
                    if (pos < end) pos++;
                    continue;
                }

                /* Check if we hit a new section keyword */
                bool is_section = false;
                const char *section_keywords[] = { "var", "const", "type", "begin", "end",
                    "implementation", "interface", "uses", "unit", "program", NULL };
                for (int i = 0; section_keywords[i] != NULL; i++) {
                    size_t kw_len = strlen(section_keywords[i]);
                    if ((size_t)(end - pos) >= kw_len &&
                        ascii_strncasecmp(pos, section_keywords[i], kw_len) == 0 &&
                        (pos + kw_len >= end || !isalnum((unsigned char)pos[kw_len]))) {
                        is_section = true;
                        break;
                    }
                }
                if (is_section) break;

                /* Skip string literals */
                if (*pos == '\'') {
                    pos++;
                    while (pos < end) {
                        if (*pos == '\'') {
                            pos++;
                            if (pos < end && *pos == '\'') { pos++; continue; }
                            break;
                        }
                        pos++;
                    }
                    continue;
                }

                /* Extract identifier */
                if (isalpha((unsigned char)*pos) || *pos == '_') {
                    const char *ident_start = pos;
                    while (pos < end && (isalnum((unsigned char)*pos) || *pos == '_')) {
                        pos++;
                    }
                    size_t ident_len = (size_t)(pos - ident_start);

                    /* Check if this identifier matches */
                    if (ident_len == sym_len && ascii_strncasecmp(ident_start, symbol, sym_len) == 0) {
                        return true;
                    }

                    /* Skip until semicolon or next identifier (comma-separated list) */
                    while (pos < end && *pos != ';' && *pos != ',') {
                        if (*pos == '(') {
                            /* Skip parenthesized content (e.g., function parameters) */
                            int depth = 1;
                            pos++;
                            while (pos < end && depth > 0) {
                                if (*pos == '(') depth++;
                                else if (*pos == ')') depth--;
                                pos++;
                            }
                        } else if (*pos == '{') {
                            /* Skip brace comments inside declarations */
                            pos++;
                            while (pos < end && *pos != '}') pos++;
                            if (pos < end) pos++;
                        } else {
                            pos++;
                        }
                    }
                    if (pos < end && *pos == ',') {
                        pos++;  /* Continue with next identifier in list */
                        continue;
                    }
                    if (pos < end && *pos == ';') {
                        pos++;  /* Continue to next declaration */
                        continue;
                    }
                } else {
                    /* Skip non-identifier character */
                    pos++;
                }
            }
        } else {
            /* Not a declaration keyword, skip to next word */
            while (pos < end && (isalnum((unsigned char)*pos) || *pos == '_')) {
                pos++;
            }
            if (pos < end) pos++;
        }
    }
    
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
    // FPC allows macro names to start with digits (e.g., 64BitFS)
    // So we accept any alphanumeric or underscore character
    if (cursor < end && (isalnum((unsigned char)*cursor) || *cursor == '_')) {
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

static bool resolve_include_path(const PascalPreprocessor *pp, const char *current_file, const char *directive_path, char **result_path) {
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

    // First try relative to current file
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
    
    // Check if file exists
    FILE *f = fopen(combined, "r");
    if (f) {
        fclose(f);
        *result_path = combined;
        return true;
    }
    free(combined);
    
    // Try each include path
    if (pp) {
        for (size_t i = 0; i < pp->include_path_count; ++i) {
            size_t inc_dir_len = strlen(pp->include_paths[i]);
            total_len = inc_dir_len + 1 + path_len + 1;
            combined = malloc(total_len);
            if (!combined) {
                return false;
            }
            snprintf(combined, total_len, "%s/%s", pp->include_paths[i], directive_path);
            
            f = fopen(combined, "r");
            if (f) {
                fclose(f);
                *result_path = combined;
                return true;
            }
            free(combined);
        }
    }
    
    /* File not found in any include path.
     * We return the relative path anyway so the caller can report a meaningful error.
     * The caller (read_file_contents) will fail to open this path and report the error. */
    total_len = dir_len + 1 + path_len + 1;
    combined = malloc(total_len);
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

/* Get macro value only if it's an actual text-replacement macro (defined with :=) */
static const char *get_macro_value(const PascalPreprocessor *pp, const char *symbol) {
    if (!pp || !symbol) return NULL;
    char *upper = strdup(symbol);
    if (!upper) return NULL;
    uppercase(upper);
    for (size_t i = 0; i < pp->define_count; ++i) {
        if (strcmp(pp->defines[i].name, upper) == 0) {
            free(upper);
            /* Only return value if this is an actual macro (defined with :=) */
            if (pp->defines[i].is_macro) {
                return pp->defines[i].value;
            }
            return NULL;
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
    // FPC allows trailing closing parentheses in {$IF} expressions (syntax quirk)
    // Skip them to be compatible
    while (*cursor == ')') {
        ++cursor;
        while (*cursor && isspace((unsigned char)*cursor)) {
            ++cursor;
        }
    }
    if (*cursor != '\0') {
        char preview[250];
        snprintf(preview, sizeof(preview), "%.200s", expression);
        return set_error(error_message, "unexpected characters after expression in: %s", preview);
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
    // =, <>, <, <=, >, >=, IN
    enum { OP_NONE, OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE, OP_IN } op = OP_NONE;

    // Check for 'in' keyword (set membership test)
    if (is_keyword_operator(*cursor, "IN", 2)) {
        op = OP_IN;
        *cursor += 2;
    } else if (**cursor == '=') {
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
            case OP_IN:
                // Set membership test: element IN set
                // element is ordinal value (0-63), set is bitmask
                if (*value >= 0 && *value < 64) {
                    *value = ((1LL << *value) & rhs) != 0;
                } else {
                    *value = 0;  // out of range = not in set
                }
                break;
            default: break;
        }
    }

    return true;
}

/* Helper function to check if a string matches an operator keyword */
static bool is_keyword_operator(const char *cursor, const char *keyword, size_t keyword_len)
{
    if (ascii_strncasecmp(cursor, keyword, keyword_len) != 0)
        return false;
    char next = cursor[keyword_len];
    return !isalnum((unsigned char)next) && next != '_';
}

/* Skip an expression term during short-circuit evaluation.
 * Tracks parentheses and stops at operators that would end the term.
 */
static void skip_shortcircuit_term(const char **cursor)
{
    int paren_depth = 0;
    while (**cursor) {
        char c = **cursor;
        if (c == '(') {
            paren_depth++;
        } else if (c == ')') {
            if (paren_depth == 0) break;
            paren_depth--;
        } else if (paren_depth == 0) {
            /* Check for operators that would end this term */
            if (c == '+' || c == '-' || c == '*' || c == '/' ||
                c == '=' || c == '<' || c == '>') break;
            if (is_keyword_operator(*cursor, "DIV", 3) ||
                is_keyword_operator(*cursor, "MOD", 3) ||
                is_keyword_operator(*cursor, "AND", 3) ||
                is_keyword_operator(*cursor, "OR", 2) ||
                is_keyword_operator(*cursor, "XOR", 3)) break;
        }
        (*cursor)++;
    }
}

// SimpleExpression = Term { AddOp Term }
// AddOp = +, -, OR, XOR
// Note: OR and XOR use short-circuit evaluation for boolean contexts
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

        // Short-circuit evaluation for OR: if LHS is true (non-zero), skip RHS
        if (op == OP_OR && *value != 0) {
            skip_shortcircuit_term(cursor);
            // Result stays true (non-zero)
            continue;
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
// MulOp = *, /, DIV, MOD, SHL, SHR, AND
// Note: AND uses short-circuit evaluation for boolean contexts
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

        enum { OP_NONE, OP_MUL, OP_DIV, OP_INTDIV, OP_MOD, OP_AND, OP_SHL, OP_SHR } op = OP_NONE;

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
        } else if (ascii_strncasecmp(*cursor, "SHL", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_SHL;
            *cursor += 3;
        } else if (ascii_strncasecmp(*cursor, "SHR", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_SHR;
            *cursor += 3;
        } else if (ascii_strncasecmp(*cursor, "AND", 3) == 0 && !isalnum((unsigned char)(*cursor)[3]) && (*cursor)[3] != '_') {
            op = OP_AND;
            *cursor += 3;
        }

        if (op == OP_NONE) {
            *cursor = start;
            break;
        }

        // Short-circuit evaluation for AND: if LHS is false (zero), skip RHS
        if (op == OP_AND && *value == 0) {
            skip_shortcircuit_term(cursor);
            // Result stays false (zero)
            continue;
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
            case OP_SHL:
                if (rhs < 0 || rhs >= 64)
                    return set_error(error_message, "invalid shift count");
                *value <<= rhs;
                break;
            case OP_SHR:
                if (rhs < 0 || rhs >= 64)
                    return set_error(error_message, "invalid shift count");
                *value = (int64_t)((uint64_t)*value >> rhs);
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

    // SIZEOF function
    if (ascii_strncasecmp(*cursor, "SIZEOF", 6) == 0) {
        *cursor += 6;
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        
        if (**cursor != '(') return set_error(error_message, "expected '(' after SIZEOF");
        ++(*cursor);
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        
        const char *start = *cursor;
        if (!isalpha((unsigned char)**cursor) && **cursor != '_') return set_error(error_message, "expected type identifier");
        while (**cursor && (isalnum((unsigned char)**cursor) || **cursor == '_' || **cursor == '.')) ++(*cursor);
        
        char *type_name = duplicate_range(start, *cursor);
        if (!type_name) return set_error(error_message, "out of memory");
        
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        if (**cursor != ')') {
            free(type_name);
            return set_error(error_message, "missing ')'");
        }
        ++(*cursor);
        
        // Map type names to their sizes (in bytes)
        // For x86_64 (CPU64)
        uppercase(type_name);
        
        int64_t size = 0;
        bool found = false;
        bool cpu64 = pascal_preprocessor_is_defined(pp, "CPU64");

        if (strcmp(type_name, "NATIVEINT") == 0 || strcmp(type_name, "NATIVEUINT") == 0 ||
            strcmp(type_name, "SIZEINT") == 0 || strcmp(type_name, "SIZEUINT") == 0 ||
            strcmp(type_name, "PTRINT") == 0 || strcmp(type_name, "PTRUINT") == 0 ||
            strcmp(type_name, "INTPTR") == 0 || strcmp(type_name, "UINTPTR") == 0 ||
            strcmp(type_name, "TCONSTPTRUINT") == 0 || strcmp(type_name, "TCONSTPTRINT") == 0 ||
            strcmp(type_name, "PUINT") == 0 || strcmp(type_name, "PINT") == 0 ||
            strcmp(type_name, "AINT") == 0 || strcmp(type_name, "ASIZEINT") == 0) {
            size = cpu64 ? 8 : 4;
            found = true;
        }

        // Integer types (64-bit on x86_64)
        if (!found && (strcmp(type_name, "INT64") == 0 || strcmp(type_name, "QWORD") == 0 || strcmp(type_name, "UINT64") == 0 ||
            strcmp(type_name, "TSYSPARAM") == 0 || strcmp(type_name, "V") == 0 || strcmp(type_name, "ALUUINT") == 0 ||
            strcmp(type_name, "ALUSINT") == 0 || strcmp(type_name, "VALSINT") == 0 || strcmp(type_name, "VALUINT") == 0 ||
            strcmp(type_name, "FREECHUNK") == 0 || strcmp(type_name, "AINTMAX") == 0 || strcmp(type_name, "ASIZEINT") == 0 ||
            strcmp(type_name, "TBITSBASE") == 0 || strcmp(type_name, "TUNSIGNEDINTTYPE") == 0 ||
            strcmp(type_name, "TSIGNEDINTTYPE") == 0 || strcmp(type_name, "INTPTR") == 0 ||
            strcmp(type_name, "UINTPTR") == 0 || strcmp(type_name, "HANDLE") == 0 ||
            strcmp(type_name, "THANDLE") == 0 || strcmp(type_name, "TLARGEINTEGER") == 0 ||
            strcmp(type_name, "TTHREADID") == 0)) {
            size = 8;
            found = true;
        } else if (strcmp(type_name, "INTEGER") == 0 || strcmp(type_name, "LONGINT") == 0 ||
                   strcmp(type_name, "INT32") == 0 || strcmp(type_name, "CARDINAL") == 0 ||
                   strcmp(type_name, "DWORD") == 0 || strcmp(type_name, "UINT32") == 0 ||
                   strcmp(type_name, "LONGWORD") == 0 || strcmp(type_name, "LONGBOOL") == 0) {
            size = 4;
            found = true;
        } else if (strcmp(type_name, "SMALLINT") == 0 || strcmp(type_name, "INT16") == 0 ||
                   strcmp(type_name, "WORD") == 0 || strcmp(type_name, "UINT16") == 0 ||
                   strcmp(type_name, "WIDECHAR") == 0 || strcmp(type_name, "WORDBOOL") == 0 ||
                   strcmp(type_name, "TCOMPILERWIDECHAR") == 0) {
            size = 2;
            found = true;
        } else if (strcmp(type_name, "SHORTINT") == 0 || strcmp(type_name, "INT8") == 0 || 
                   strcmp(type_name, "BYTE") == 0 || strcmp(type_name, "UINT8") == 0 || 
                   strcmp(type_name, "CHAR") == 0 || strcmp(type_name, "BOOLEAN") == 0 ||
                   strcmp(type_name, "ANSICHAR") == 0 || strcmp(type_name, "BYTEBOOL") == 0) {
            size = 1;
            found = true;
        }
        // Floating point types
        else if (strcmp(type_name, "VALREAL") == 0 || strcmp(type_name, "EXTENDED") == 0 ||
                   strcmp(type_name, "LONGDOUBLE") == 0 || strcmp(type_name, "BESTREALREC") == 0 ||
                   strcmp(type_name, "TEXTENDED80REC") == 0 || strcmp(type_name, "BESTREAL") == 0) {
            // FPC on x86_64 maps ValReal/Extended/bestreal to the 80-bit extended type (10 bytes)
            // bestrealrec is TExtended80Rec on x86_64 with extended float support
            size = 10;
            found = true;
        } else if (strcmp(type_name, "DOUBLE") == 0 || strcmp(type_name, "REAL") == 0) {
            size = 8;
            found = true;
        } else if (strcmp(type_name, "SINGLE") == 0 || strcmp(type_name, "FLOAT") == 0) {
            size = 4;
            found = true;
        } else if (strcmp(type_name, "COMP") == 0 || strcmp(type_name, "CURRENCY") == 0) {
            size = 8;
            found = true;
        }
        // C types (for x86_64 Linux)
        else if (strcmp(type_name, "TIME_T") == 0 || strcmp(type_name, "CLONG") == 0 || 
                 strcmp(type_name, "CULONG") == 0 || strcmp(type_name, "CLONGLONG") == 0 || 
                 strcmp(type_name, "CULONGLONG") == 0 || strcmp(type_name, "CSIZE_T") == 0 ||
                 strcmp(type_name, "CSSIZE_T") == 0 || strcmp(type_name, "COFF_T") == 0 ||
                 strcmp(type_name, "OFF_T") == 0) {
            size = 8;  // 64-bit on x86_64
            found = true;
        } else if (strcmp(type_name, "CINT") == 0 || strcmp(type_name, "CUINT") == 0 || 
                   strcmp(type_name, "CINT32") == 0 || strcmp(type_name, "CUINT32") == 0) {
            size = 4;
            found = true;
        } else if (strcmp(type_name, "CSHORT") == 0 || strcmp(type_name, "CUSHORT") == 0 || 
                   strcmp(type_name, "CINT16") == 0 || strcmp(type_name, "CUINT16") == 0) {
            size = 2;
            found = true;
        } else if (strcmp(type_name, "CCHAR") == 0 || strcmp(type_name, "CSCHAR") == 0 || 
                   strcmp(type_name, "CUCHAR") == 0 || strcmp(type_name, "CINT8") == 0 || 
                   strcmp(type_name, "CUINT8") == 0) {
            size = 1;
            found = true;
        }
        // Code pointer types (FPC builtins)
        else if (strcmp(type_name, "CODEPTRUINT") == 0 || strcmp(type_name, "CODEPTRINT") == 0 ||
                 strcmp(type_name, "CODEPOINTER") == 0) {
            size = cpu64 ? 8 : 4;
            found = true;
        }
        // Pointer types
        else if (strstr(type_name, "POINTER") != NULL || type_name[0] == 'P') {
            size = 8;  // 64-bit pointers on x86_64
            found = true;
        }

        if (!found) {
            // Unknown type — assume pointer size as fallback.
            // FPC's preprocessor can resolve variable types in sizeof(),
            // which ours cannot. Most such variables are platform-sized
            // (SizeInt, SizeUint, etc.) so pointer size is a safe default.
            size = cpu64 ? 8 : 4;
        }
        
        free(type_name);
        *value = size;
        return true;
    }

    // HIGH / LOW functions
    if (ascii_strncasecmp(*cursor, "HIGH", 4) == 0 || ascii_strncasecmp(*cursor, "LOW", 3) == 0) {
        bool is_high = (ascii_strncasecmp(*cursor, "HIGH", 4) == 0);
        *cursor += (is_high ? 4 : 3);
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        
        if (**cursor != '(') return set_error(error_message, "expected '(' after HIGH/LOW");
        ++(*cursor);
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        
        const char *start = *cursor;
        if (!isalpha((unsigned char)**cursor) && **cursor != '_') return set_error(error_message, "expected type identifier");
        while (**cursor && (isalnum((unsigned char)**cursor) || **cursor == '_' || **cursor == '.')) ++(*cursor);
        
        char *type_name = duplicate_range(start, *cursor);
        if (!type_name) return set_error(error_message, "out of memory");
        
        while (**cursor && isspace((unsigned char)**cursor)) ++(*cursor);
        if (**cursor != ')') {
            free(type_name);
            return set_error(error_message, "missing ')'");
        }
        ++(*cursor);
        
        // Map type names to their High/Low values
        // For CPU64 (x86_64): ValSInt=int64, ValUInt=qword
        // For CPU32: ValSInt=Longint, ValUInt=Cardinal
        uppercase(type_name);
        
        int64_t result = 0;
        bool found = false;
        
        if (strcmp(type_name, "VALSINT") == 0 || strcmp(type_name, "INT64") == 0) {
            result = is_high ? INT64_MAX : INT64_MIN;
            found = true;
        } else if (strcmp(type_name, "VALUINT") == 0 || strcmp(type_name, "QWORD") == 0 || strcmp(type_name, "UINT64") == 0) {
            // For unsigned, Low is 0, High is max
            result = is_high ? (int64_t)UINT64_MAX : 0;
            found = true;
        } else if (strcmp(type_name, "INTEGER") == 0 || strcmp(type_name, "LONGINT") == 0 ||
                   strcmp(type_name, "INT32") == 0) {
            result = is_high ? INT32_MAX : INT32_MIN;
            found = true;
        } else if (strcmp(type_name, "CARDINAL") == 0 || strcmp(type_name, "DWORD") == 0 ||
                   strcmp(type_name, "UINT32") == 0 || strcmp(type_name, "LONGWORD") == 0) {
            result = is_high ? (int64_t)UINT32_MAX : 0;
            found = true;
        } else if (strcmp(type_name, "SMALLINT") == 0 || strcmp(type_name, "INT16") == 0) {
            result = is_high ? INT16_MAX : INT16_MIN;
            found = true;
        } else if (strcmp(type_name, "WORD") == 0 || strcmp(type_name, "UINT16") == 0) {
            result = is_high ? (int64_t)UINT16_MAX : 0;
            found = true;
        } else if (strcmp(type_name, "SHORTINT") == 0 || strcmp(type_name, "INT8") == 0) {
            result = is_high ? INT8_MAX : INT8_MIN;
            found = true;
        } else if (strcmp(type_name, "BYTE") == 0 || strcmp(type_name, "UINT8") == 0) {
            result = is_high ? (int64_t)UINT8_MAX : 0;
            found = true;
        } else if (strcmp(type_name, "ERRORCODE") == 0) {
            // Special case for FPC system unit: ErrorCode is a variable of type Word
            // High(ErrorCode) -> High(Word)
            result = is_high ? (int64_t)UINT16_MAX : 0;
            found = true;
        }
        
        if (!found) {
            bool err = set_error(error_message, "unsupported type '%s' for HIGH/LOW", type_name);
            free(type_name);
            return err;
        }
        
        free(type_name);
        *value = result;
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
        
        /* For declared(), also check if identifier appears in Pascal declarations */
        if (is_declared) {
            /* Check preprocessor defines first */
            if (symbol_is_defined(pp, sym)) {
                *value = 1;
            } else if (pp->current_output != NULL && pp->current_output_len > 0) {
                /* Then check Pascal source declarations */
                *value = symbol_is_declared_in_source(pp->current_output, pp->current_output_len, sym) ? 1 : 0;
            } else {
                *value = 0;
            }
        } else {
            /* For defined(), only check preprocessor defines */
            *value = symbol_is_defined(pp, sym) ? 1 : 0;
        }
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

        // Boolean literals: true = 1, false = 0
        if (ascii_strncasecmp(sym, "TRUE", 5) == 0) {
            *value = 1;
            free(sym);
            return true;
        }
        if (ascii_strncasecmp(sym, "FALSE", 6) == 0) {
            *value = 0;
            free(sym);
            return true;
        }

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
            // Try to find a Pascal const integer in the preprocessed source
            int64_t const_val;
            if (find_const_integer_in_source(pp, sym, &const_val)) {
                *value = const_val;
            } else if (pp->current_output != NULL && pp->current_output_len > 0 &&
                       symbol_is_declared_in_source(pp->current_output, pp->current_output_len, sym)) {
                // Symbol is declared in source but its value can't be computed
                // (e.g. complex const initializers with function calls, pointer
                // arithmetic, address-of like PtrUint(@Node(nil^).field)).
                // Return 0 as best-effort so {$if} assertions don't error.
                *value = 0;
            } else {
                // Truly undefined symbol -> error (matching FPC behavior)
                bool err = set_error(
                    error_message,
                    "undefined macro '%s'",
                    sym
                );
                free(sym);
                return err;
            }
        }
        free(sym);
        return true;
    }

    return set_error(error_message, "syntax error in expression");
}
