#include "debug_serializer.h"
#include <stdio.h>

void serialize_expression(FILE *log_file, struct Expression *expr) {
    if (expr == NULL) {
        fprintf(log_file, "NULL\n");
        return;
    }

    fprintf(log_file, "%d ", expr->type);

    switch (expr->type) {
        case EXPR_VAR_ID:
            fprintf(log_file, "%s\n", expr->expr_data.id);
            break;
        case EXPR_INUM:
            fprintf(log_file, "%d\n", expr->expr_data.i_num);
            break;
        case EXPR_RNUM:
            fprintf(log_file, "%f\n", expr->expr_data.r_num);
            break;
        case EXPR_STRING:
            fprintf(log_file, "%s\n", expr->expr_data.string);
            break;
        case EXPR_FUNCTION_CALL:
            fprintf(log_file, "%s\n", expr->expr_data.function_call_data.id);
            break;
        case EXPR_ARRAY_ACCESS:
            fprintf(log_file, "%s\n", expr->expr_data.array_access_data.id);
            break;
        default:
            fprintf(log_file, "\n");
            break;
    }
}
