#include "flat_ir_generator.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "../../ir.h"
#include "../../Parser/flat_ast.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree_types.h" // For Param_t
#include "../../Parser/SemanticCheck/mangle.h"

// Forward declarations
static void generate_ir_for_node(FlatNode *node, SymTab_t *symtab, ListNode_t **ir_list, FlatNode* current_subprogram);

ListNode_t *generate_ir_from_flat_ast(FlatNode *ast, SymTab_t *table) {
    ListNode_t *ir_list = NULL;
    generate_ir_for_node(ast, table, &ir_list, NULL);
    // The list is built in reverse, so we need to reverse it back
    return ReverseList(ir_list);
}

static IRInstruction* create_ir_instruction(IROpcode opcode) {
    IRInstruction *inst = malloc(sizeof(IRInstruction));
    inst->opcode = opcode;
    inst->dest = NULL;
    inst->src1 = NULL;
    inst->src2 = NULL;
    inst->label = NULL;
    inst->proc_name = NULL;
    inst->args = NULL;
    inst->asm_string = NULL;
    return inst;
}

static IRValue* create_ir_value(char *name, int is_global) {
    IRValue *val = malloc(sizeof(IRValue));
    val->name = name;
    val->is_global = is_global;
    val->reg = NULL;
    return val;
}

static int temp_reg_count = 0;
static IRValue* create_temp_reg() {
    char *name = malloc(10);
    sprintf(name, "t%d", temp_reg_count++);
    return create_ir_value(name, 0);
}


static void generate_ir_for_node(FlatNode *node, SymTab_t *symtab, ListNode_t **ir_list, FlatNode* current_subprogram) {
    if (node == NULL) {
        return;
    }

    switch (node->node_type) {
        case FL_PROGRAM:
            {
                IRInstruction *main_label_inst = create_ir_instruction(IR_LABEL);
                main_label_inst->label = node->data.program.id;
                *ir_list = Cons(main_label_inst, *ir_list);

                generate_ir_for_node(node->data.program.compound_statement, symtab, ir_list, NULL);

                *ir_list = Cons(create_ir_instruction(IR_RETURN), *ir_list);

                ListNode_t *current_sub = node->data.program.subprograms;
                while (current_sub != NULL) {
                    generate_ir_for_node((FlatNode *)current_sub->cur, symtab, ir_list, (FlatNode *)current_sub->cur);
                    current_sub = current_sub->next;
                }
            }
            break;
        case FL_PROCEDURE:
            {
                // Similar to function, but no return value handling needed yet
                generate_ir_for_node(node->data.procedure.compound_statement, symtab, ir_list, node);

                IRInstruction *ret_inst = create_ir_instruction(IR_RETURN);
                *ir_list = Cons(ret_inst, *ir_list);

                IRInstruction *label_inst = create_ir_instruction(IR_LABEL);
                label_inst->label = get_mangled_name(node->data.procedure.id, node->data.procedure.params);
                *ir_list = Cons(label_inst, *ir_list);
            }
            break;
        case FL_FUNCTION:
            {
                // Generate IR for the function body first
                generate_ir_for_node(node->data.function.compound_statement, symtab, ir_list, node);

                // Add the function return instruction
                IRInstruction *ret_inst = create_ir_instruction(IR_RETURN);
                *ir_list = Cons(ret_inst, *ir_list);

                // Add the function label (entry point)
                IRInstruction *label_inst = create_ir_instruction(IR_LABEL);
                label_inst->label = get_mangled_name(node->data.function.id, node->data.function.params);
                *ir_list = Cons(label_inst, *ir_list);
            }
            break;
        case FL_COMPOUND_STATEMENT:
            {
                // The statement list from the parser is in reverse order.
                // We need to reverse it back to process statements in the correct order.
                ListNode_t *reversed_list = ReverseList(node->data.compound_statement.stmt_list);

                ListNode_t *current = reversed_list;
                while (current != NULL) {
                    generate_ir_for_node((FlatNode *)current->cur, symtab, ir_list, current_subprogram);
                    current = current->next;
                }

                // Free the reversed list structure (not the content)
                DestroyList(reversed_list);
            }
            break;
        case FL_WHILE_LOOP:
            // TODO
            break;
        case FL_FOR_LOOP:
            // TODO
            break;
        case FL_IF_THEN:
            // TODO
            break;
        case FL_VAR_ASSIGN:
            {
                // Generate IR for the expression on the right side of the assignment
                generate_ir_for_node(node->data.var_assign.expr, symtab, ir_list, current_subprogram);
                IRInstruction *expr_inst = (IRInstruction *)(*ir_list)->cur;

                char *assign_target_id = node->data.var_assign.var->data.var_id.id;

                // Check if this is an assignment to the function name (i.e., a return statement)
                if (current_subprogram != NULL &&
                    current_subprogram->node_type == FL_FUNCTION &&
                    strcmp(current_subprogram->data.function.id, assign_target_id) == 0)
                {
                    IRInstruction *inst = create_ir_instruction(IR_STORE_RETURN_VAR);
                    inst->src1 = expr_inst->dest;
                    *ir_list = Cons(inst, *ir_list);
                } else {
                    // This is a normal variable assignment
                    IRInstruction *inst = create_ir_instruction(IR_STORE_VAR);
                    inst->dest = create_ir_value(assign_target_id, 0); // Assuming local for now
                    inst->src1 = expr_inst->dest; // The value to store (from the RHS expression)
                    *ir_list = Cons(inst, *ir_list);
                }
            }
            break;
        case FL_PROCEDURE_CALL:
            {
                // Mangle call site
                char* proc_name = node->data.procedure_call.id;
                if (strcmp(proc_name, "writeln") == 0 || strcmp(proc_name, "write") == 0 || strcmp(proc_name, "read") == 0) {
                    if (node->data.procedure_call.args != NULL) {
                        FlatNode *arg_node = (FlatNode *)node->data.procedure_call.args->cur;
                        if (arg_node->node_type == FL_STRING) {
                            proc_name = malloc(strlen(node->data.procedure_call.id) + 3);
                            sprintf(proc_name, "%s_s", node->data.procedure_call.id);
                        } else {
                            // Assume integer for now. This is not robust.
                            proc_name = malloc(strlen(node->data.procedure_call.id) + 3);
                            sprintf(proc_name, "%s_i", node->data.procedure_call.id);
                        }
                    }
                }

                IRInstruction *call_inst = create_ir_instruction(IR_CALL);
                call_inst->proc_name = proc_name;

                // Handle arguments (safely)
                if (node->data.procedure_call.args != NULL) {
                    // For now, still assume one argument, but do it safely
                    FlatNode *arg_node = (FlatNode *)node->data.procedure_call.args->cur;
                    generate_ir_for_node(arg_node, symtab, ir_list, current_subprogram);

                    if (*ir_list != NULL) {
                        IRInstruction *arg_inst = (IRInstruction *)(*ir_list)->cur;
                        call_inst->src1 = arg_inst->dest;

                        if (arg_node->node_type == FL_STRING) {
                            call_inst->arg_type = 1;
                        } else {
                            call_inst->arg_type = 0;
                        }
                    }
                }

                *ir_list = Cons(call_inst, *ir_list);
            }
            break;
        case FL_VAR_ID:
            {
                // NOTE: This doesn't check the symbol table yet to see if it's global.
                // This will need to be improved later.
                IRInstruction *inst = create_ir_instruction(IR_LOAD_VAR);
                inst->src1 = create_ir_value(node->data.var_id.id, 0); // Assuming local for now
                inst->dest = create_temp_reg();
                *ir_list = Cons(inst, *ir_list);
            }
            break;
        case FL_ARRAY_ACCESS:
            // TODO
            break;
        case FL_FUNCTION_CALL:
            {
                // TODO: Handle arguments
                IRInstruction *call_inst = create_ir_instruction(IR_CALL);
                call_inst->proc_name = node->data.function_call.id;
                *ir_list = Cons(call_inst, *ir_list);

                IRInstruction *retrieve_inst = create_ir_instruction(IR_RETRIEVE_RETURN_VAL);
                retrieve_inst->dest = create_temp_reg();
                *ir_list = Cons(retrieve_inst, *ir_list);
            }
            break;
        case FL_RELOP:
        case FL_ADDOP:
        case FL_MULOP:
            {
                // Generate IR for the left and right operands
                generate_ir_for_node(node->data.bin_op.left, symtab, ir_list, current_subprogram);
                IRInstruction *left_inst = (IRInstruction *)(*ir_list)->cur;

                generate_ir_for_node(node->data.bin_op.right, symtab, ir_list, current_subprogram);
                IRInstruction *right_inst = (IRInstruction *)(*ir_list)->cur;

                IRInstruction *inst = NULL;
                switch (node->data.bin_op.op) {
                    case OP_ADD:
                        inst = create_ir_instruction(IR_ADD);
                        break;
                    case OP_SUB:
                        inst = create_ir_instruction(IR_SUB);
                        break;
                    case OP_MUL:
                        inst = create_ir_instruction(IR_MUL);
                        break;
                    case OP_DIV:
                        inst = create_ir_instruction(IR_DIV);
                        break;
                    case OP_MOD:
                        inst = create_ir_instruction(IR_MOD);
                        break;
                    default:
                        // For now, do nothing for other ops
                        break;
                }

                if (inst) {
                    inst->src1 = left_inst->dest;
                    inst->src2 = right_inst->dest;
                    inst->dest = create_temp_reg();
                    *ir_list = Cons(inst, *ir_list);
                }
            }
            break;
        case FL_INUM:
            {
                IRInstruction *inst = create_ir_instruction(IR_LOAD_CONST);
                char *s_inum = malloc(12); // Enough for a 32-bit integer string
                sprintf(s_inum, "%d", node->data.inum);
                inst->src1 = create_ir_value(s_inum, 0); // Not a global
                inst->dest = create_temp_reg();
                *ir_list = Cons(inst, *ir_list);
            }
            break;
        case FL_RNUM:
            // TODO
            break;
        case FL_STRING:
            {
                IRInstruction *inst = create_ir_instruction(IR_LOAD_STRING);
                inst->src1 = create_ir_value(node->data.string, 0);
                inst->dest = create_temp_reg();
                *ir_list = Cons(inst, *ir_list);
            }
            break;
        case FL_UNOP:
            // TODO
            break;
        case FL_ASM_BLOCK:
            {
                IRInstruction *inst = create_ir_instruction(IR_ASM);
                inst->asm_string = node->data.asm_block.code;
                *ir_list = Cons(inst, *ir_list);
            }
            break;
        default:
            assert(0 && "Unknown node type in IR generator");
            break;
    }
}
