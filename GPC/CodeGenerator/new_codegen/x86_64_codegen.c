#include "x86_64_codegen.h"
#include "register_allocator.h"
#include "symbol_table.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "Grammar.tab.h"

void codegen_x86_64(ListNode_t *ir_list, FILE *out) {
    SymbolTable *table = symbol_table_new();
    int stack_offset = 0;

    fprintf(out, "\t.file\t\"new_codegen.p\"\n");
    fprintf(out, ".section .rodata\n");
    fprintf(out, ".format_str_d:\n");
    fprintf(out, ".string \"%%d\\n\"\n");
    fprintf(out, ".text\n");

    fprintf(out, ".globl new_codegen_simple\n");
    fprintf(out, "new_codegen_simple:\n");
    fprintf(out, "\tpushq\t%%rbp\n");
    fprintf(out, "\tmovq\t%%rsp, %%rbp\n");

    new_init_register_allocator();
    while (ir_list) {
        IRInstruction *inst = ir_list->cur;
        if (inst->opcode == IR_LOAD_CONST) {
            Register *reg = new_alloc_reg();
            fprintf(out, "\tmovl\t$%s, %s\n", inst->src1->name, reg->name);
            inst->dest->name = reg->name;
        }
        else if (inst->opcode == IR_LOAD_VAR) {
            Register *reg = new_alloc_reg();
            int offset = symbol_table_get(table, inst->src1->name);
            fprintf(out, "\tmovl\t-%d(%%rbp), %s\n", offset, reg->name);
            inst->dest->name = reg->name;
        }
        else if (inst->opcode == IR_STORE_VAR) {
            int offset = symbol_table_get(table, inst->dest->name);
            if (offset == -1) {
                stack_offset += 4;
                offset = stack_offset;
                symbol_table_put(table, inst->dest->name, offset);
            }
            fprintf(out, "\tmovl\t%s, -%d(%%rbp)\n", inst->src1->name, offset);
        }
        else if (inst->opcode == IR_ADD) {
            Register *reg = new_alloc_reg();
            fprintf(out, "\tmovl\t%s, %s\n", inst->src1->name, reg->name);
            fprintf(out, "\taddl\t%s, %s\n", inst->src2->name, reg->name);
            inst->dest->name = reg->name;
        }
        else if (inst->opcode == IR_SUB) {
            Register *reg = new_alloc_reg();
            fprintf(out, "\tmovl\t%s, %s\n", inst->src1->name, reg->name);
            fprintf(out, "\tsubl\t%s, %s\n", inst->src2->name, reg->name);
            inst->dest->name = reg->name;
        }
        else if (inst->opcode == IR_CMP) {
            fprintf(out, "\tcmpl\t%s, %s\n", inst->src2->name, inst->src1->name);
        }
        else if (inst->opcode == IR_JUMP_IF_ZERO) {
            char jmp_buf[4];
            switch(inst->relop_type)
            {
                case EQ: strncpy(jmp_buf, "jne", 4); break;
                case NE: strncpy(jmp_buf, "je", 4); break;
                case LT: strncpy(jmp_buf, "jge", 4); break;
                case LE: strncpy(jmp_buf, "jg", 4); break;
                case GT: strncpy(jmp_buf, "jle", 4); break;
                case GE: strncpy(jmp_buf, "jl", 4); break;
                default: assert(0 && "Unknown relop type");
            }
            fprintf(out, "\t%s\t%s\n", jmp_buf, inst->label);
        }
        else if (inst->opcode == IR_JUMP) {
            fprintf(out, "\tjmp\t%s\n", inst->label);
        }
        else if (inst->opcode == IR_LABEL) {
            fprintf(out, "%s:\n", inst->label);
        }
        else if (inst->opcode == IR_MUL) {
            Register *reg = new_alloc_reg();
            fprintf(out, "\tmovl\t%s, %s\n", inst->src1->name, reg->name);
            fprintf(out, "\timull\t%s, %s\n", inst->src2->name, reg->name);
            inst->dest->name = reg->name;
        }
        else if (inst->opcode == IR_DIV) {
            fprintf(out, "\tmovl\t%s, %%eax\n", inst->src1->name);
            fprintf(out, "\tcltd\n");
            fprintf(out, "\tidivl\t%s\n", inst->src2->name);
            inst->dest->name = "%eax";
        }
        else if (inst->opcode == IR_MOD) {
            fprintf(out, "\tmovl\t%s, %%eax\n", inst->src1->name);
            fprintf(out, "\tcltd\n");
            fprintf(out, "\tidivl\t%s\n", inst->src2->name);
            inst->dest->name = "%edx";
        }
        ir_list = ir_list->next;
    }

    fprintf(out, "\tnop\n");
    fprintf(out, "\tleave\n");
    fprintf(out, "\tret\n");

    fprintf(out, ".globl main\n");
    fprintf(out, "main:\n");
    fprintf(out, "\tpushq\t%%rbp\n");
    fprintf(out, "\tmovq\t%%rsp, %%rbp\n");
    fprintf(out, "\tcall\tnew_codegen_simple\n");
    fprintf(out, "\tmovl\t$0, %%eax\n");
    fprintf(out, "\tleave\n");
    fprintf(out, "\tret\n");

    symbol_table_free(table);
}
