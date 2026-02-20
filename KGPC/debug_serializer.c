#include <stdio.h>
#include <stdlib.h>
#include "debug_serializer.h"
#include "Parser/ParseTree/tree_types.h"
#include "Parser/ParseTree/type_tags.h"

void serialize_expression_recursive(FILE *fp, struct Expression *expr) {
    if (expr == NULL) {
        fprintf(fp, "NULL\n");
        return;
    }

    fprintf(fp, "%d ", expr->type);

    switch (expr->type) {
        case EXPR_VAR_ID:
            fprintf(fp, "%s\n", expr->expr_data.id);
            break;
        case EXPR_INUM:
            fprintf(fp, "%lld\n", expr->expr_data.i_num);
            break;
        case EXPR_STRING:
            fprintf(fp, "\"%s\"\n", expr->expr_data.string);
            break;
        case EXPR_ADDOP:
            fprintf(fp, "%d\n", expr->expr_data.addop_data.addop_type);
            serialize_expression_recursive(fp, expr->expr_data.addop_data.left_expr);
            serialize_expression_recursive(fp, expr->expr_data.addop_data.right_term);
            break;
        case EXPR_MULOP:
            fprintf(fp, "%d\n", expr->expr_data.mulop_data.mulop_type);
            serialize_expression_recursive(fp, expr->expr_data.mulop_data.left_term);
            serialize_expression_recursive(fp, expr->expr_data.mulop_data.right_factor);
            break;
        case EXPR_RELOP:
            fprintf(fp, "%d\n", expr->expr_data.relop_data.type);
            serialize_expression_recursive(fp, expr->expr_data.relop_data.left);
            serialize_expression_recursive(fp, expr->expr_data.relop_data.right);
            break;
        case EXPR_SIGN_TERM:
            serialize_expression_recursive(fp, expr->expr_data.sign_term);
            break;
        case EXPR_FUNCTION_CALL:
            fprintf(fp, "%s\n", expr->expr_data.function_call_data.id);
            // Not serializing args for now to keep it simple
            break;
        case EXPR_TYPECAST:
            fprintf(fp, "%d ", expr->expr_data.typecast_data.target_type);
            if (expr->expr_data.typecast_data.target_type_id != NULL)
                fprintf(fp, "%s\n", expr->expr_data.typecast_data.target_type_id);
            else
                fprintf(fp, "NULL\n");
            serialize_expression_recursive(fp, expr->expr_data.typecast_data.expr);
            break;
        case EXPR_TYPEINFO:
            if (expr->expr_data.typeinfo_data.type_id != NULL)
                fprintf(fp, "%s\n", expr->expr_data.typeinfo_data.type_id);
            else
                fprintf(fp, "NULL\n");
            break;
        default:
            fprintf(fp, "UNKNOWN\n");
    }
}

void serialize_expression(FILE *fp, struct Expression *expr) {
    serialize_expression_recursive(fp, expr);
}
