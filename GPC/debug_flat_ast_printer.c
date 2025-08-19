#include "debug_flat_ast_printer.h"
#include <stdio.h>
#include <assert.h>

static void print_indent(FILE *f, int indent) {
    for (int i = 0; i < indent; i++) {
        fprintf(f, "  ");
    }
}

static void print_flat_ast_recursive(FlatNode *node, FILE *f, int indent) {
    if (node == NULL) {
        print_indent(f, indent);
        fprintf(f, "(null)\n");
        return;
    }

    print_indent(f, indent);
    fprintf(f, "(L%d) ", node->line_num);

    switch (node->node_type) {
        case FL_PROGRAM:
            fprintf(f, "PROGRAM %s\n", node->data.program.id);
            print_indent(f, indent + 1);
            fprintf(f, "DECLARATIONS:\n");
            // TODO: Print declarations
            print_indent(f, indent + 1);
            fprintf(f, "SUBPROGRAMS:\n");
            // TODO: Print subprograms
            print_indent(f, indent + 1);
            fprintf(f, "BODY:\n");
            print_flat_ast_recursive(node->data.program.compound_statement, f, indent + 2);
            break;
        case FL_COMPOUND_STATEMENT:
            fprintf(f, "COMPOUND_STATEMENT\n");
            if (node->data.compound_statement.stmt_list != NULL) {
                ListNode_t *current = node->data.compound_statement.stmt_list;
                while (current != NULL) {
                    print_flat_ast_recursive((FlatNode *)current->cur, f, indent + 1);
                    current = current->next;
                }
            }
            break;
        case FL_PROCEDURE_CALL:
            fprintf(f, "PROCEDURE_CALL %s\n", node->data.procedure_call.id);
            if (node->data.procedure_call.args != NULL) {
                print_indent(f, indent + 1);
                fprintf(f, "ARGS:\n");
                ListNode_t *current = node->data.procedure_call.args;
                while (current != NULL) {
                    print_flat_ast_recursive((FlatNode *)current->cur, f, indent + 2);
                    current = current->next;
                }
            }
            break;
        case FL_STRING:
            fprintf(f, "STRING: \"%s\"\n", node->data.string);
            break;
        case FL_IF_THEN:
            fprintf(f, "IF\n");
            print_flat_ast_recursive(node->data.if_then.condition, f, indent + 1);
            print_indent(f, indent);
            fprintf(f, "THEN\n");
            print_flat_ast_recursive(node->data.if_then.then_stmt, f, indent + 1);
            if (node->data.if_then.else_stmt) {
                print_indent(f, indent);
                fprintf(f, "ELSE\n");
                print_flat_ast_recursive(node->data.if_then.else_stmt, f, indent + 1);
            }
            break;
        case FL_VAR_ASSIGN:
            fprintf(f, "ASSIGN\n");
            print_flat_ast_recursive(node->data.var_assign.var, f, indent + 1);
            print_flat_ast_recursive(node->data.var_assign.expr, f, indent + 1);
            break;
        case FL_VAR_ID:
            fprintf(f, "VAR_ID: %s\n", node->data.var_id.id);
            break;
        case FL_INUM:
            fprintf(f, "INUM: %d\n", node->data.inum);
            break;
        case FL_RELOP:
            fprintf(f, "RELOP: %d\n", node->data.bin_op.op);
            print_flat_ast_recursive(node->data.bin_op.left, f, indent + 1);
            print_flat_ast_recursive(node->data.bin_op.right, f, indent + 1);
            break;
        case FL_ASM_BLOCK:
            fprintf(f, "ASM_BLOCK:\n");
            print_indent(f, indent + 1);
            fprintf(f, "\"%s\"\n", node->data.asm_block.code);
            break;
        default:
            fprintf(f, "UNKNOWN_NODE_TYPE: %d\n", node->node_type);
            break;
    }
}

void print_flat_ast(FlatNode *node, FILE *f) {
    print_flat_ast_recursive(node, f, 0);
}
