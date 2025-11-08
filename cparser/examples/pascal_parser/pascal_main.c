#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#ifndef _WIN32
#include <strings.h>
#include <time.h>
#else
#ifndef strncasecmp
#define strncasecmp _strnicmp
#endif
#ifndef strcasecmp
#define strcasecmp _stricmp
#endif
#include <windows.h>
#endif
#include "pascal_parser.h"
#include "pascal_preprocessor.h"

#ifdef _WIN32
typedef struct {
    LARGE_INTEGER value;
} monotonic_time_t;

static void monotonic_now(monotonic_time_t* t) {
    QueryPerformanceCounter(&t->value);
}

static double seconds_between(monotonic_time_t start, monotonic_time_t end) {
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(&frequency);
    double elapsed = (double)(end.value.QuadPart - start.value.QuadPart);
    return elapsed / (double)frequency.QuadPart;
}
#else
typedef struct timespec monotonic_time_t;

static void monotonic_now(monotonic_time_t* t) {
    clock_gettime(CLOCK_MONOTONIC, t);
}

static double seconds_between(monotonic_time_t start, monotonic_time_t end) {
    double seconds = (double)(end.tv_sec - start.tv_sec);
    double nanoseconds = (double)(end.tv_nsec - start.tv_nsec) / 1e9;
    return seconds + nanoseconds;
}
#endif

// Forward declaration
static void print_ast_indented(ast_t* ast, int depth);
static void print_error_with_partial_ast(ParseError* error);

static const char* skip_utf8_bom(const char* cursor, const char* end) {
    if ((size_t)(end - cursor) >= 3 &&
        (unsigned char)cursor[0] == 0xEF &&
        (unsigned char)cursor[1] == 0xBB &&
        (unsigned char)cursor[2] == 0xBF) {
        return cursor + 3;
    }
    return cursor;
}

static const char* skip_whitespace_and_comments(const char* cursor, const char* end) {
    while (cursor < end) {
        unsigned char ch = (unsigned char)*cursor;

        if (isspace(ch)) {
            ++cursor;
            continue;
        }

        if (ch == '{') {
            ++cursor;
            while (cursor < end && *cursor != '}') {
                ++cursor;
            }
            if (cursor < end)
                ++cursor;
            continue;
        }

        if (ch == '(' && (cursor + 1) < end && cursor[1] == '*') {
            cursor += 2;
            while ((cursor + 1) < end && !(cursor[0] == '*' && cursor[1] == ')')) {
                ++cursor;
            }
            if ((cursor + 1) < end)
                cursor += 2;
            else
                cursor = end;
            continue;
        }

        if (ch == '/' && (cursor + 1) < end && cursor[1] == '/') {
            cursor += 2;
            while (cursor < end && *cursor != '\n') {
                ++cursor;
            }
            continue;
        }

        break;
    }

    return cursor;
}

static bool buffer_starts_with_keyword(const char* buffer, size_t length, const char* keyword) {
    const char* cursor = buffer;
    const char* end = buffer + length;
    cursor = skip_utf8_bom(cursor, end);
    cursor = skip_whitespace_and_comments(cursor, end);

    size_t keyword_len = strlen(keyword);
    if ((size_t)(end - cursor) < keyword_len)
        return false;

    if (strncasecmp(cursor, keyword, keyword_len) != 0)
        return false;

    const char* after = cursor + keyword_len;
    if (after < end && (isalnum((unsigned char)*after) || *after == '_'))
        return false;

    return true;
}

static void print_profile_report(
    const char* filename,
    double file_read_seconds,
    double preprocess_seconds,
    double parser_init_seconds,
    double parse_seconds,
    parser_stats_t stats) {
    double total = file_read_seconds + preprocess_seconds + parser_init_seconds + parse_seconds;
    double hit_pct = stats.parse_calls ? (100.0 * (double)stats.memo_hits / (double)stats.parse_calls) : 0.0;
    double miss_pct = stats.parse_calls ? (100.0 * (double)stats.memo_misses / (double)stats.parse_calls) : 0.0;
    double copy_pct = stats.ast_nodes_created ? (100.0 * (double)stats.ast_nodes_copied / (double)stats.ast_nodes_created) : 0.0;
    double success_pct = stats.parse_calls ? (100.0 * (double)stats.parse_successes / (double)stats.parse_calls) : 0.0;

    printf("\n== Profiling Report (%s) ==\n", filename ? filename : "<buffer>");
    printf("  File read            : %.3f s\n", file_read_seconds);
    printf("  Preprocess           : %.3f s\n", preprocess_seconds);
    printf("  Parser init          : %.3f s\n", parser_init_seconds);
    printf("  Core parse           : %.3f s\n", parse_seconds);
    printf("  Accounted total      : %.3f s\n", total);
    printf("  parse() calls        : %zu\n", stats.parse_calls);
    printf("    successes          : %zu (%.1f%%)\n", stats.parse_successes, success_pct);
    printf("    failures           : %zu\n", stats.parse_failures);
    printf("    memo hits          : %zu (%.1f%%)\n", stats.memo_hits, hit_pct);
    printf("    memo misses        : %zu (%.1f%%)\n", stats.memo_misses, miss_pct);
    printf("    left recursion     : %zu\n", stats.memo_recursions);
    printf("    memo replays       : %zu\n", stats.memo_replays);
    printf("    memo entries       : %zu\n", stats.memo_entries_created);
    printf("  AST nodes allocated  : %zu\n", stats.ast_nodes_created);
    printf("    copies via memo    : %zu (%.1f%% of total)\n", stats.ast_nodes_copied, copy_pct);
    printf("  Memo result clones   : %zu\n", stats.memo_result_clones);
}

static const char* parser_type_to_string(parser_type_t type) {
    switch (type) {
        case P_MATCH: return "match";
        case P_MATCH_RAW: return "match_raw";
        case P_INTEGER: return "integer";
        case P_CIDENT: return "cident";
        case P_STRING: return "string";
        case P_UNTIL: return "until";
        case P_SUCCEED: return "succeed";
        case P_ANY_CHAR: return "any_char";
        case P_SATISFY: return "satisfy";
        case P_CI_KEYWORD: return "ci_keyword";
        case P_LAYOUT: return "layout";
        case COMB_EXPECT: return "expect";
        case COMB_SEQ: return "seq";
        case COMB_MULTI: return "multi";
        case COMB_FLATMAP: return "flatMap";
        case COMB_MANY: return "many";
        case COMB_EXPR: return "expr";
        case COMB_OPTIONAL: return "optional";
        case COMB_SEP_BY: return "sep_by";
        case COMB_SEP_BY1: return "sep_by1";
        case COMB_LEFT: return "left";
        case COMB_RIGHT: return "right";
        case COMB_NOT: return "not";
        case COMB_PEEK: return "peek";
        case COMB_GSEQ: return "gseq";
        case COMB_BETWEEN: return "between";
        case COMB_SEP_END_BY: return "sep_end_by";
        case COMB_CHAINL1: return "chainl1";
        case COMB_MAP: return "map";
        case COMB_ERRMAP: return "errmap";
        case COMB_COMMIT: return "commit";
        case COMB_LAZY: return "lazy";
        case COMB_VARIANT_TAG: return "variant_tag";
        case COMB_VARIANT_PART: return "variant_part";
        case COMB_FOR_INIT_DISPATCH: return "for_init_dispatch";
        case COMB_ASSIGNMENT_GUARD: return "assignment_guard";
        case COMB_LABEL_GUARD: return "label_guard";
        case COMB_STATEMENT_DISPATCH: return "statement_dispatch";
        case COMB_CLASS_MEMBER_DISPATCH: return "class_member_dispatch";
        case P_EOI: return "eoi";
        default: return "unknown";
    }
}

typedef struct comb_entry {
    const parser_comb_stat_t* stat;
} comb_entry_t;

static int compare_comb_entries(const void* a, const void* b) {
    const comb_entry_t* ea = (const comb_entry_t*)a;
    const comb_entry_t* eb = (const comb_entry_t*)b;
    const parser_comb_stat_t* sa = ea->stat;
    const parser_comb_stat_t* sb = eb->stat;
    uint64_t aval = sa->total_failure_consumed;
    uint64_t bval = sb->total_failure_consumed;
    if (aval < bval) return 1;
    if (aval > bval) return -1;
    if (sa->failure_with_consumption < sb->failure_with_consumption) return 1;
    if (sa->failure_with_consumption > sb->failure_with_consumption) return -1;
    if (sa->failures < sb->failures) return 1;
    if (sa->failures > sb->failures) return -1;
    return 0;
}

static void print_comb_profile(size_t top_n) {
    size_t count = 0;
    const parser_comb_stat_t* stats = parser_comb_stats_snapshot(&count);
    if (stats == NULL || count == 0) {
        printf("\n== Combinator Hotspots ==\n  <no stats available>\n");
        return;
    }
    comb_entry_t* entries = (comb_entry_t*)malloc(count * sizeof(comb_entry_t));
    if (!entries) {
        printf("\n== Combinator Hotspots ==\n  <allocation failed>\n");
        return;
    }
    size_t used = 0;
    for (size_t i = 1; i < count; ++i) {
        if (stats[i].calls == 0) continue;
        entries[used++].stat = &stats[i];
    }
    if (used == 0) {
        free(entries);
        printf("\n== Combinator Hotspots ==\n  <no parser activity recorded>\n");
        return;
    }
    qsort(entries, used, sizeof(comb_entry_t), compare_comb_entries);
    if (top_n == 0 || top_n > used) {
        top_n = used;
    }
    printf("\n== Combinator Hotspots (top %zu by failure consumption) ==\n", top_n);
    printf("%-4s %-40s %-12s %8s %8s %8s %10s %10s %12s\n",
           "#", "Parser", "Type", "Calls", "Fails", "Fail>0", "AvgFail", "MaxFail", "TotalFail");
    for (size_t i = 0; i < top_n; ++i) {
        const parser_comb_stat_t* st = entries[i].stat;
        double avg_fail = st->failure_with_consumption ?
            (double)st->total_failure_consumed / (double)st->failure_with_consumption : 0.0;
        const char* name = st->name ? st->name : "<unnamed>";
        printf("%-4zu %-40.40s %-12s %8zu %8zu %8zu %10.1f %10zu %12zu\n",
               i + 1,
               name,
               parser_type_to_string(st->type),
               st->calls,
               st->failures,
               st->failure_with_consumption,
               avg_fail,
               st->max_failure_consumed,
               st->total_failure_consumed);
    }
    free(entries);
}

// Helper function to print ParseError with partial AST
static void print_error_chain(ParseError* error, int depth) {
    if (error == NULL) {
        return;
    }

    for (int i = 0; i < depth; i++) {
        printf("  ");
    }

    printf("Error at line %d, col %d: ", error->line, error->col);
    if (error->parser_name) {
        printf("In parser '%s': ", error->parser_name);
    }
    printf("%s\n", error->message ? error->message : "<no message>");

    if (error->unexpected) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Unexpected input: \"%s\"\n", error->unexpected);
    }

    if (error->context) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Context:\n");
        const char* ctx = error->context;
        while (*ctx) {
            const char* newline = strchr(ctx, '\n');
            for (int i = 0; i < depth; i++) {
                printf("  ");
            }
            printf("  ");
            if (newline) {
                printf("%.*s\n", (int)(newline - ctx), ctx);
                ctx = newline + 1;
            } else {
                printf("%s\n", ctx);
                break;
            }
        }
    }

    if (error->partial_ast != NULL) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Partial AST:\n");
        print_ast_indented(error->partial_ast, depth + 1);
    }

    if (error->cause != NULL) {
        for (int i = 0; i < depth; i++) {
            printf("  ");
        }
        printf("Caused by:\n");
        print_error_chain(error->cause, depth + 1);
    }
}

static void print_error_with_partial_ast(ParseError* error) {
    print_error_chain(error, 0);
}

// Helper function to print AST with indentation
static void print_ast_indented(ast_t* ast, int depth) {
    if (ast == NULL || ast == ast_nil) return;
    for (int i = 0; i < depth; i++) printf("  ");
    printf("(%s", pascal_tag_to_string(ast->typ));
    if (ast->sym) printf(" %s", ast->sym->name);

    ast_t* child = ast->child;
    if (child) {
        printf("\n");
        print_ast_indented(child, depth + 1);
    }
    printf(")");

    if (ast->next) {
        printf("\n");
        print_ast_indented(ast->next, depth);
    }
}


int main(int argc, char *argv[]) {
    bool print_ast = false;
    bool parse_procedure = false;
    char *filename = NULL;
    bool profile = false;
    bool profile_combinators = false;
    size_t profile_combinators_top = 15;
    double file_read_seconds = 0.0;
    double preprocess_seconds = 0.0;
    double parser_init_seconds = 0.0;
    double parse_seconds = 0.0;
    parser_stats_t stats = {0};
    bool stats_ready = false;
    monotonic_time_t read_start, read_end;
    monotonic_time_t preprocess_start, preprocess_end;
    monotonic_time_t parser_init_start, parser_init_end;
    monotonic_time_t parse_start, parse_end;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--print-ast") == 0) {
            print_ast = true;
        } else if (strcmp(argv[i], "--parse-procedure") == 0) {
            parse_procedure = true;
        } else if (strcmp(argv[i], "--profile") == 0 || strcmp(argv[i], "--benchmark") == 0) {
            profile = true;
        } else if (strncmp(argv[i], "--profile-combinators", 21) == 0) {
            profile = true;
            profile_combinators = true;
            const char* value = argv[i] + 21;
            if (*value == '=' || *value == ':') {
                value++;
                if (*value == '\0') {
                    fprintf(stderr, "Invalid value for --profile-combinators\n");
                    return 1;
                }
                char* endptr = NULL;
                unsigned long parsed = strtoul(value, &endptr, 10);
                if (endptr == value || *endptr != '\0') {
                    fprintf(stderr, "Invalid value for --profile-combinators: %s\n", value);
                    return 1;
                }
                profile_combinators_top = parsed;
            } else if (*value != '\0') {
                fprintf(stderr, "Unknown option '%s'\n", argv[i]);
                return 1;
            }
        } else if (strncmp(argv[i], "--memo-mode=", 12) == 0) {
            const char* mode = argv[i] + 12;
            if (strcasecmp(mode, "full") == 0) {
                parser_set_memo_mode(PARSER_MEMO_FULL);
            } else if (strcasecmp(mode, "failures") == 0 || strcasecmp(mode, "failure") == 0 ||
                       strcasecmp(mode, "fail") == 0) {
                parser_set_memo_mode(PARSER_MEMO_FAILURES_ONLY);
            } else if (strcasecmp(mode, "off") == 0 || strcasecmp(mode, "none") == 0 ||
                       strcasecmp(mode, "disable") == 0 || strcasecmp(mode, "disabled") == 0) {
                parser_set_memo_mode(PARSER_MEMO_DISABLED);
            } else {
                fprintf(stderr, "Unknown memo mode '%s'. Expected full|failures|off\n", mode);
                return 1;
            }
        } else if (strcmp(argv[i], "--memo-mode") == 0 && i + 1 < argc) {
            const char* mode = argv[++i];
            if (strcasecmp(mode, "full") == 0) {
                parser_set_memo_mode(PARSER_MEMO_FULL);
            } else if (strcasecmp(mode, "failures") == 0 || strcasecmp(mode, "failure") == 0 ||
                       strcasecmp(mode, "fail") == 0) {
                parser_set_memo_mode(PARSER_MEMO_FAILURES_ONLY);
            } else if (strcasecmp(mode, "off") == 0 || strcasecmp(mode, "none") == 0 ||
                       strcasecmp(mode, "disable") == 0 || strcasecmp(mode, "disabled") == 0) {
                parser_set_memo_mode(PARSER_MEMO_DISABLED);
            } else {
                fprintf(stderr, "Unknown memo mode '%s'. Expected full|failures|off\n", mode);
                return 1;
            }
        } else if (strcmp(argv[i], "--memo-mode") == 0) {
            parser_set_memo_mode(PARSER_MEMO_DISABLED);
            continue;
        } else {
            filename = argv[i];
        }
    }

    if (filename == NULL) {
        fprintf(stderr, "Usage: %s [--print-ast] [--parse-procedure] [--profile] [--profile-combinators[=N]] [--memo-mode=<full|failures|off>] <filename>\n", argv[0]);
        return 1;
    }

    parser_set_memo_mode(PARSER_MEMO_DISABLED);

    if (profile_combinators) {
        parser_comb_stats_set_enabled(true);
        parser_comb_stats_reset();
    } else {
        parser_comb_stats_set_enabled(false);
    }

    // Read file content
    if (profile) {
        monotonic_now(&read_start);
    }
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return 1;
    }
    
    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    if (file_size == -1L) {
        fprintf(stderr, "Error: could not determine file size for '%s'\n", filename);
        fclose(file);
        return 1;
    }
    fseek(file, 0, SEEK_SET);
    
    // Allocate buffer and read file
    char *file_content = malloc((size_t)file_size + 1);
    if (file_content == NULL) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        fclose(file);
        return 1;
    }
    
    size_t bytes_read = fread(file_content, 1, (size_t)file_size, file);
    if (bytes_read != (size_t)file_size && ferror(file)) {
        fprintf(stderr, "Error: Failed to read complete file '%s'\n", filename);
        free(file_content);
        fclose(file);
        return 1;
    }
    file_content[bytes_read] = '\0';
    fclose(file);
    if (profile) {
        monotonic_now(&read_end);
        file_read_seconds = seconds_between(read_start, read_end);
    }

    printf("Parsing file: %s\n", filename);
    printf("File size: %zu bytes\n", bytes_read);
    printf("First 100 characters: '%.100s'\n", file_content);

    PascalPreprocessor *preprocessor = pascal_preprocessor_create();
    if (preprocessor == NULL) {
        fprintf(stderr, "Error: failed to initialise preprocessor\n");
        free(file_content);
        return 1;
    }
    if (!pascal_preprocessor_define(preprocessor, "FPC") ||
        !pascal_preprocessor_define(preprocessor, "OBJFPC")) {
        fprintf(stderr, "Error: failed to set default defines\n");
        pascal_preprocessor_free(preprocessor);
        free(file_content);
        return 1;
    }

    char *preprocess_error = NULL;
    size_t preprocessed_length = 0;
    if (profile) {
        monotonic_now(&preprocess_start);
    }
    char *preprocessed_content = pascal_preprocess_buffer(
        preprocessor,
        filename,
        file_content,
        bytes_read,
        &preprocessed_length,
        &preprocess_error);
    pascal_preprocessor_free(preprocessor);
    if (profile) {
        monotonic_now(&preprocess_end);
        preprocess_seconds = seconds_between(preprocess_start, preprocess_end);
    }

    if (preprocessed_content == NULL) {
        fprintf(stderr, "Preprocessing failed: %s\n", preprocess_error ? preprocess_error : "unknown error");
        free(preprocess_error);
        free(file_content);
        return 1;
    }
    free(preprocess_error);

    printf("Preprocessed size: %zu bytes\n", preprocessed_length);

    bool parse_as_unit = !parse_procedure &&
        buffer_starts_with_keyword(preprocessed_content, preprocessed_length, "unit");
    if (parse_procedure) {
        printf("Detected top-level form: procedure (forced by flag)\n");
    } else {
        printf("Detected top-level form: %s\n", parse_as_unit ? "unit" : "program");
    }

    if (profile) {
        monotonic_now(&parser_init_start);
    }
    combinator_t *parser = new_combinator();
    if (parse_procedure) {
        init_pascal_procedure_parser(&parser);
    } else if (parse_as_unit) {
        init_pascal_unit_parser(&parser);
    } else {
        init_pascal_complete_program_parser(&parser);
    }
    if (profile) {
        monotonic_now(&parser_init_end);
        parser_init_seconds = seconds_between(parser_init_start, parser_init_end);
    }

    input_t *in = new_input();
    in->buffer = preprocessed_content;
    in->length = preprocessed_length;
    ast_nil = new_ast();
    ast_nil->typ = PASCAL_T_NONE;

    if (profile) {
        parser_stats_reset();
        monotonic_now(&parse_start);
    }
    ParseResult result = parse(in, parser);
    if (profile) {
        monotonic_now(&parse_end);
        parse_seconds = seconds_between(parse_start, parse_end);
        stats = parser_stats_snapshot();
        stats_ready = true;
    }
    
    printf("Parse completed. Success: %s\n", result.is_success ? "YES" : "NO");
    if (!result.is_success && result.value.error) {
        printf("Input position when failed: %d of %d\n", in->start, in->length);
        if (in->start < in->length) {
            printf("Context around failure: '%.50s'\n", in->buffer + in->start);
        }
    }

    if (result.is_success) {
        if (in->start < in->length) {
            int trailing_index = in->start;
            int trailing_line = 1;
            int trailing_col = 1;
            parser_calculate_line_col(in, trailing_index, &trailing_line, &trailing_col);

            fprintf(stderr,
                    "Error: Parser did not consume entire input. Trailing input begins at line %d, column %d.\n",
                    trailing_line, trailing_col);

            char* context = parser_format_context(in, trailing_line, trailing_col, trailing_index);
            if (context != NULL) {
                fprintf(stderr, "%s", context);
                free(context);
            }

            const char* remaining = in->buffer + in->start;
            fprintf(stderr, "Remaining characters: '%.*s'%s\n",
                    80, remaining, strlen(remaining) > 80 ? "..." : "");

            free_ast(result.value.ast);
            free(preprocessed_content);
            free(file_content);
            free_combinator(parser);
            free_input(in);
            free(ast_nil);
            if (profile && stats_ready) {
                print_profile_report(filename, file_read_seconds, preprocess_seconds, parser_init_seconds, parse_seconds, stats);
            }
            if (profile_combinators) {
                print_comb_profile(profile_combinators_top);
            }
            return 1;
        }
        if (print_ast) {
            print_pascal_ast(result.value.ast);
        }
        free_ast(result.value.ast);
    } else {
        print_error_with_partial_ast(result.value.error);
        free_error(result.value.error);
        free(preprocessed_content);
        free(file_content);
        free_combinator(parser);
        free_input(in);
        free(ast_nil);
        if (profile && stats_ready) {
            print_profile_report(filename, file_read_seconds, preprocess_seconds, parser_init_seconds, parse_seconds, stats);
        }
        if (profile_combinators) {
            print_comb_profile(profile_combinators_top);
        }
        return 1;
    }

    free_combinator(parser);
    free_input(in);
    free(ast_nil);
    free(preprocessed_content);
    free(file_content);
    if (profile && stats_ready) {
        print_profile_report(filename, file_read_seconds, preprocess_seconds, parser_init_seconds, parse_seconds, stats);
    }
    if (profile_combinators) {
        print_comb_profile(profile_combinators_top);
    }

    return 0;
}
