#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "debug_deserializer.h"
#include "Parser/ParseTree/tree_types.h"
#include "Parser/ParseTree/type_tags.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/SemanticCheck/HashTable/HashTable.h"

struct Expression *deserialize_expression(FILE *fp) {
    int type;
    if (fscanf(fp, "%d", &type) != 1) {
        return NULL;
    }

    struct Expression *expr = (struct Expression *)malloc(sizeof(struct Expression));
    expr->type = type;
    expr->line_num = -1; // Not serialized
    expr->field_width = NULL;
    expr->field_precision = NULL;

    switch (type) {
        case EXPR_VAR_ID: {
            char *id = (char *)malloc(100); // Assume max length
            fscanf(fp, "%s", id);
            expr->expr_data.id = id;
            break;
        }
        case EXPR_INUM: {
            long long i_num;
            fscanf(fp, "%lld", &i_num);
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
            if (id == NULL) {
                free(expr);
                return NULL;
            }
            if (fscanf(fp, "%99s", id) != 1) {
                free(id);
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
                    destroy_list(args);
                    free(expr);
                    return NULL;
                }

                ListNode_t *arg_node = CreateListNode(arg_expr, LIST_EXPR);
                if (arg_node == NULL) {
                    free(id);
                    destroy_expr(arg_expr);
                    destroy_list(args);
                    free(expr);
                    return NULL;
                }

                if (args == NULL)
                    args = arg_node;
                else
                    tail->next = arg_node;
                tail = arg_node;
            }

            expr->expr_data.function_call_data.id = id;
            expr->expr_data.function_call_data.mangled_id = NULL;
            expr->expr_data.function_call_data.args_expr = args;
            expr->expr_data.function_call_data.resolved_func = NULL;
            expr->expr_data.function_call_data.is_call_info_valid = 0;
            expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
            expr->expr_data.function_call_data.call_kgpc_type = NULL;
            expr->expr_data.function_call_data.is_procedural_var_call = 0;
            expr->expr_data.function_call_data.procedural_var_symbol = NULL;
            expr->expr_data.function_call_data.procedural_var_expr = NULL;
            expr->expr_data.function_call_data.is_method_call_placeholder = 0;
            expr->expr_data.function_call_data.placeholder_method_name = NULL;
            expr->expr_data.function_call_data.is_virtual_call = 0;
            expr->expr_data.function_call_data.is_interface_call = 0;
            expr->expr_data.function_call_data.vmt_index = -1;
            expr->expr_data.function_call_data.self_class_name = NULL;
            expr->expr_data.function_call_data.constructor_receiver_expr = NULL;
            expr->expr_data.function_call_data.arg0_is_dynarray_descriptor = 0;
            expr->expr_data.function_call_data.call_qualifier = NULL;
            expr->expr_data.function_call_data.cached_owner_class = NULL;
            expr->expr_data.function_call_data.cached_method_name = NULL;
            expr->expr_data.function_call_data.cached_procvar_sret_size = 0;
            break;
        }
        case EXPR_TYPECAST: {
            int target_type;
            if (fscanf(fp, "%d", &target_type) != 1) {
                free(expr);
                return NULL;
            }

            char buffer[256];
            if (fscanf(fp, "%255s", buffer) != 1) {
                free(expr);
                return NULL;
            }

            if (strcmp(buffer, "NULL") == 0)
                expr->expr_data.typecast_data.target_type_id = NULL;
            else
                expr->expr_data.typecast_data.target_type_id = strdup(buffer);

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
