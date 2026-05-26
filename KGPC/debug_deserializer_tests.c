#include "acutest.h"
#include "debug_deserializer.h"
#include "Parser/ParseTree/type_tags.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

enum { SPACE_INSERTION_INTERVAL = 41 };

static char *make_repeated_text(size_t length, int with_spaces) {
  char *buffer = (char *)malloc(length + 1);
  size_t i;

  TEST_ASSERT(buffer != NULL);
  if (buffer == NULL) {
    return NULL;
  }

  for (i = 0; i < length; ++i) {
    if (with_spaces && i != 0 && (i % SPACE_INSERTION_INTERVAL) == 0) {
      buffer[i] = ' ';
    } else {
      buffer[i] = (char)('a' + (i % 26));
    }
  }
  buffer[length] = '\0';
  return buffer;
}

static struct Expression *deserialize_from_text(const char *text) {
  FILE *fp = tmpfile();
  struct Expression *expr;

  TEST_ASSERT(fp != NULL);
  if (fp == NULL) {
    return NULL;
  }

  TEST_ASSERT(fputs(text, fp) != EOF);
  rewind(fp);
  expr = deserialize_expression(fp);
  fclose(fp);
  return expr;
}

static char *format_text(const char *fmt, ...) {
  va_list args;
  va_list args_copy;
  int length;
  char *buffer;

  va_start(args, fmt);
  va_copy(args_copy, args);
  length = vsnprintf(NULL, 0, fmt, args);
  va_end(args);
  TEST_ASSERT(length >= 0);
  if (length < 0) {
    va_end(args_copy);
    return NULL;
  }

  buffer = (char *)malloc((size_t)length + 1);
  TEST_ASSERT(buffer != NULL);
  if (buffer == NULL) {
    va_end(args_copy);
    return NULL;
  }

  TEST_ASSERT(vsnprintf(buffer, (size_t)length + 1, fmt, args_copy) == length);
  va_end(args_copy);
  return buffer;
}

static void destroy_test_expr(struct Expression *expr) {
  if (expr == NULL) {
    return;
  }

  switch (expr->type) {
  case EXPR_VAR_ID:
    free(expr->expr_data.id);
    break;
  case EXPR_STRING:
    free(expr->expr_data.string);
    break;
  case EXPR_FUNCTION_CALL:
    free(expr->expr_data.function_call_data.id);
    break;
  case EXPR_TYPECAST:
    free(expr->expr_data.typecast_data.target_type_id);
    destroy_test_expr(expr->expr_data.typecast_data.expr);
    break;
  default:
    break;
  }

  free(expr);
}

static void test_deserialize_long_identifier_tokens(void) {
  char *identifier = make_repeated_text(400, 0);
  char *serialized = NULL;
  struct Expression *expr;

  TEST_ASSERT(identifier != NULL);
  if (identifier == NULL) {
    return;
  }

  serialized = format_text("%d %s\n", EXPR_VAR_ID, identifier);
  if (serialized == NULL) {
    free(identifier);
    return;
  }

  expr = deserialize_from_text(serialized);
  TEST_ASSERT(expr != NULL);
  if (expr != NULL) {
    TEST_ASSERT(expr->type == EXPR_VAR_ID);
    TEST_ASSERT(strcmp(expr->expr_data.id, identifier) == 0);
    destroy_test_expr(expr);
  }

  free(serialized);

  serialized = format_text("%d %s\n0\n", EXPR_FUNCTION_CALL, identifier);
  if (serialized == NULL) {
    free(identifier);
    return;
  }

  expr = deserialize_from_text(serialized);
  TEST_ASSERT(expr != NULL);
  if (expr != NULL) {
    TEST_ASSERT(expr->type == EXPR_FUNCTION_CALL);
    TEST_ASSERT(strcmp(expr->expr_data.function_call_data.id, identifier) == 0);
    destroy_test_expr(expr);
  }

  free(serialized);

  serialized = format_text("%d %d %s\n%d 7\n", EXPR_TYPECAST, UNKNOWN_TYPE,
                           identifier, EXPR_INUM);
  if (serialized == NULL) {
    free(identifier);
    return;
  }

  expr = deserialize_from_text(serialized);
  TEST_ASSERT(expr != NULL);
  if (expr != NULL) {
    TEST_ASSERT(expr->type == EXPR_TYPECAST);
    TEST_ASSERT(
        strcmp(expr->expr_data.typecast_data.target_type_id, identifier) == 0);
    TEST_ASSERT(expr->expr_data.typecast_data.expr != NULL);
    if (expr->expr_data.typecast_data.expr != NULL) {
      TEST_ASSERT(expr->expr_data.typecast_data.expr->type == EXPR_INUM);
      TEST_ASSERT(expr->expr_data.typecast_data.expr->expr_data.i_num == 7);
    }
    destroy_test_expr(expr);
  }

  free(serialized);
  free(identifier);
}

static void test_deserialize_long_quoted_string(void) {
  char *str = make_repeated_text(1800, 1);
  char *serialized = NULL;
  struct Expression *expr;

  TEST_ASSERT(str != NULL);
  if (str == NULL) {
    return;
  }

  serialized = format_text("%d \"%s\"\n", EXPR_STRING, str);
  if (serialized == NULL) {
    free(str);
    return;
  }

  expr = deserialize_from_text(serialized);
  TEST_ASSERT(expr != NULL);
  if (expr != NULL) {
    TEST_ASSERT(expr->type == EXPR_STRING);
    TEST_ASSERT(strcmp(expr->expr_data.string, str) == 0);
    destroy_test_expr(expr);
  }

  free(serialized);
  free(str);
}

TEST_LIST = {{"test_deserialize_long_identifier_tokens",
              test_deserialize_long_identifier_tokens},
             {"test_deserialize_long_quoted_string",
              test_deserialize_long_quoted_string},
             {NULL, NULL}};
