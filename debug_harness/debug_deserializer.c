#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "debug_deserializer.h"
#include "Parser/ParseTree/tree_types.h"
#include "Grammar.tab.h"
#include "Parser/ParseTree/tree.h"

struct Expression *deserialize_expression(FILE *fp) {
    int type;
    if (fscanf(fp, "%d", &type) != 1) {
        return NULL;
    }

    struct Expression *expr = (struct Expression *)malloc(sizeof(struct Expression));
    expr->type = type;
    expr->line_num = -1; // Not serialized

    switch (type) {
        case EXPR_VAR_ID: {
            char *id = (char *)malloc(100); // Assume max length
            fscanf(fp, "%s", id);
            expr->expr_data.id = id;
            break;
        }
        case EXPR_INUM: {
            int i_num;
            fscanf(fp, "%d", &i_num);
            expr->expr_data.i_num = i_num;
            break;
        }
        case EXPR_STRING: {
            char *str = (char *)malloc(1024); // Assume max length
            fscanf(fp, " \"%[^\"]\"", str);
            expr->expr_data.string = str;
            break;
        }
        case EXPR_ADDOP: {
            int addop_type;
            fscanf(fp, "%d", &addop_type);
            expr->expr_data.addop_data.addop_type = addop_type;
            expr->expr_data.addop_data.left_expr = deserialize_expression(fp);
            expr->expr_data.addop_data.right_term = deserialize_expression(fp);
            break;
        }
        case EXPR_MULOP: {
            int mulop_type;
            fscanf(fp, "%d", &mulop_type);
            expr->expr_data.mulop_data.mulop_type = mulop_type;
            expr->expr_data.mulop_data.left_term = deserialize_expression(fp);
            expr->expr_data.mulop_data.right_factor = deserialize_expression(fp);
            break;
        }
        case EXPR_RELOP: {
            int relop_type;
            fscanf(fp, "%d", &relop_type);
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
            char *id = (char *)malloc(100); // Assume max length
            fscanf(fp, "%s", id);
            expr->expr_data.function_call_data.id = id;
            expr->expr_data.function_call_data.args_expr = NULL; // Not deserializing args
            expr->expr_data.function_call_data.resolved_func = NULL;
            expr->expr_data.function_call_data.mangled_id = NULL;
            break;
        }
        default:
            free(expr);
            return NULL;
    }

    return expr;
}
