#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "debug_deserializer.h"
#include "Parser/ParseTree/tree_types.h"
#include "Parser/ParseTree/type_tags.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/SemanticCheck/HashTable/HashTable.h"

static int append_char(char **buffer, size_t *length, size_t *capacity,
                       int ch) {
  if (*length + 1 >= *capacity) {
    /* Start with a small stack-friendly chunk and grow geometrically so
     * arbitrarily long serialized tokens/strings stay linear-time. */
    size_t new_capacity = (*capacity == 0) ? 64 : (*capacity * 2);
    char *new_buffer = (char *)realloc(*buffer, new_capacity);
    if (new_buffer == NULL) {
      return 0;
    }
    *buffer = new_buffer;
    *capacity = new_capacity;
  }

  (*buffer)[(*length)++] = (char)ch;
  return 1;
}

static char *read_token(FILE *fp) {
  int ch;
  char *buffer = NULL;
  size_t length = 0;
  size_t capacity = 0;

  do {
    ch = fgetc(fp);
  } while (ch != EOF && isspace((unsigned char)ch));

  if (ch == EOF) {
    return NULL;
  }

  while (ch != EOF && !isspace((unsigned char)ch)) {
    if (!append_char(&buffer, &length, &capacity, ch)) {
      free(buffer);
      return NULL;
    }
    ch = fgetc(fp);
  }

  if (ch != EOF && isspace((unsigned char)ch)) {
    ungetc(ch, fp);
  }

  if (!append_char(&buffer, &length, &capacity, '\0')) {
    free(buffer);
    return NULL;
  }

  return buffer;
}

static char *read_quoted_string(FILE *fp) {
  int ch;
  char *buffer = NULL;
  size_t length = 0;
  size_t capacity = 0;

  do {
    ch = fgetc(fp);
  } while (ch != EOF && isspace((unsigned char)ch));

  if (ch != '"') {
    if (ch != EOF) {
      ungetc(ch, fp);
    }
    return NULL;
  }

  while ((ch = fgetc(fp)) != EOF) {
    if (ch == '"') {
      if (!append_char(&buffer, &length, &capacity, '\0')) {
        free(buffer);
        return NULL;
      }
      return buffer;
    }
    if (!append_char(&buffer, &length, &capacity, ch)) {
      free(buffer);
      return NULL;
    }
  }

  free(buffer);
  return NULL;
}

struct Expression *deserialize_expression(FILE *fp) {
  int type;
  if (fscanf(fp, "%d", &type) != 1) {
    return NULL;
  }

  struct Expression *expr =
      (struct Expression *)malloc(sizeof(struct Expression));
  expr->type = type;
  expr->line_num = -1; // Not serialized
  expr->field_width = NULL;
  expr->field_precision = NULL;

  switch (type) {
  case EXPR_VAR_ID: {
    char *id = read_token(fp);
    if (id == NULL) {
      free(expr);
      return NULL;
    }
    expr->expr_data.id = id;
    break;
  }
  case EXPR_INUM: {
    long long i_num;
    if (fscanf(fp, "%lld", &i_num) != 1) {
      free(expr);
      return NULL;
    }
    expr->expr_data.i_num = i_num;
    break;
  }
  case EXPR_STRING: {
    char *str = read_quoted_string(fp);
    if (str == NULL) {
      free(expr);
      return NULL;
    }
    expr->expr_data.string = str;
    break;
  }
  case EXPR_ADDOP: {
    int addop_type;
    if (fscanf(fp, "%d", &addop_type) != 1) {
      free(expr);
      return NULL;
    }
    expr->expr_data.addop_data.addop_type = addop_type;
    expr->expr_data.addop_data.left_expr = deserialize_expression(fp);
    expr->expr_data.addop_data.right_term = deserialize_expression(fp);
    break;
  }
  case EXPR_MULOP: {
    int mulop_type;
    if (fscanf(fp, "%d", &mulop_type) != 1) {
      free(expr);
      return NULL;
    }
    expr->expr_data.mulop_data.mulop_type = mulop_type;
    expr->expr_data.mulop_data.left_term = deserialize_expression(fp);
    expr->expr_data.mulop_data.right_factor = deserialize_expression(fp);
    break;
  }
  case EXPR_RELOP: {
    int relop_type;
    if (fscanf(fp, "%d", &relop_type) != 1) {
      free(expr);
      return NULL;
    }
    expr->expr_data.relop_data.type = relop_type;
    expr->expr_data.relop_data.left = deserialize_expression(fp);
    expr->expr_data.relop_data.right = deserialize_expression(fp);
    break;
  }
  case EXPR_SIGN_TERM: {
    expr->expr_data.sign_term = deserialize_expression(fp);
    break;
  }
  case EXPR_FUNCTION_CALL: {
    char *id = read_token(fp);
    if (id == NULL) {
      free(expr);
      return NULL;
    }
    int arg_count = 0;
    if (fscanf(fp, "%d", &arg_count) != 1 || arg_count < 0) {
      free(id);
      free(expr);
      return NULL;
    }
    ListNode_t *args = NULL;
    ListNode_t *tail = NULL;
    for (int i = 0; i < arg_count; i++) {
      struct Expression *arg_expr = deserialize_expression(fp);
      if (arg_expr == NULL) {
        free(id);
        free(expr);
        return NULL;
      }
      ListNode_t *arg_node = (ListNode_t *)malloc(sizeof(ListNode_t));
      if (arg_node == NULL) {
        free(id);
        free(expr);
        return NULL;
      }
      arg_node->type = LIST_EXPR;
      arg_node->cur = arg_expr;
      arg_node->next = NULL;
      if (args == NULL)
        args = arg_node;
      else
        tail->next = arg_node;
      tail = arg_node;
    }
    expr->expr_data.function_call_data.id = id;
    expr->expr_data.function_call_data.args_expr = args;
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.mangled_id = NULL;
    expr->expr_data.function_call_data.is_call_info_valid = 0;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
    expr->expr_data.function_call_data.call_kgpc_type = NULL;
    break;
  }
  case EXPR_TYPECAST: {
    int target_type;
    if (fscanf(fp, "%d", &target_type) != 1) {
      free(expr);
      return NULL;
    }

    char *target_type_id = read_token(fp);
    if (target_type_id == NULL) {
      free(expr);
      return NULL;
    }

    if (strcmp(target_type_id, "NULL") == 0) {
      free(target_type_id);
      expr->expr_data.typecast_data.target_type_id = NULL;
    } else {
      expr->expr_data.typecast_data.target_type_id = target_type_id;
    }

    expr->expr_data.typecast_data.target_type = target_type;
    expr->expr_data.typecast_data.expr = deserialize_expression(fp);
    break;
  }
  default:
    free(expr);
    return NULL;
  }

  return expr;
}
