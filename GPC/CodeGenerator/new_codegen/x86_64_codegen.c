#include "x86_64_codegen.h"
#include "register_allocator.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "Grammar.tab.h"
#include "../../Parser/List/List.h"

typedef struct {
    char *name;
    int offset;
} LocalSymbol;

// A simple list to keep track of global variables
static ListNode_t *globals = NULL;
static ListNode_t *locals = NULL;
static int local_stack_offset = 0;

static int get_local_offset(char *name) {
    fprintf(stderr, "Getting offset for %s\n", name);
    ListNode_t *current = locals;
    while (current) {
        LocalSymbol *sym = current->cur;
        if (strcmp(sym->name, name) == 0) {
            return sym->offset;
        }
        current = current->next;
    }
    return -1;
}

static int add_local(char *name) {
    int offset = get_local_offset(name);
    if (offset != -1) {
        return offset;
    }

    local_stack_offset += 4;
    LocalSymbol *sym = malloc(sizeof(LocalSymbol));
    sym->name = strdup(name);
    sym->offset = local_stack_offset;

    if (locals == NULL) {
        locals = CreateListNode(sym, LIST_UNSPECIFIED);
    } else {
        locals = PushListNodeBack(locals, CreateListNode(sym, LIST_UNSPECIFIED));
    }
    return sym->offset;
}

static void add_global(char *name) {
    if (globals == NULL) {
        globals = CreateListNode(strdup(name), LIST_UNSPECIFIED);
        return;
    }

    ListNode_t *current = globals;
    while (current) {
        if (strcmp(current->cur, name) == 0) {
            return; // Already exists
        }
        if (current->next == NULL) {
            break;
        }
        current = current->next;
    }
    current->next = CreateListNode(strdup(name), LIST_UNSPECIFIED);
}


void codegen_x86_64(ListNode_t *ir_list, FILE *out) {
    // First pass: find all global variables
    ListNode_t *current_ir = ir_list;
    while (current_ir) {
        IRInstruction *inst = current_ir->cur;
        if (inst->opcode == IR_STORE_VAR && inst->dest->is_global) {
            add_global(inst->dest->name);
        }
        if (inst->opcode == IR_LOAD_VAR && inst->src1->is_global) {
            add_global(inst->src1->name);
        }
        current_ir = current_ir->next;
    }

    fprintf(out, "\t.file\t\"new_codegen.p\"\n");
    fprintf(out, ".section .rodata\n");
    fprintf(out, ".format_str_d:\n");
    fprintf(out, ".string \"%%d\\n\"\n");

    // Emit global variables
    if (globals) {
        fprintf(out, ".data\n");
        ListNode_t *current_global = globals;
        while (current_global) {
            fprintf(out, ".globl %s\n", (char*)current_global->cur);
            fprintf(out, ".size %s, 4\n", (char*)current_global->cur);
            fprintf(out, "%s:\n", (char*)current_global->cur);
            fprintf(out, "\t.long 0\n");
            current_global = current_global->next;
        }
    }

    fprintf(out, ".text\n");

    fprintf(out, ".globl new_codegen_simple\n");
    fprintf(out, "new_codegen_simple:\n");
    fprintf(out, "\tpushq\t%%rbp\n");
    fprintf(out, "\tmovq\t%%rsp, %%rbp\n");

    new_init_register_allocator();

    current_ir = ir_list;
    while (current_ir) {
        IRInstruction *inst = current_ir->cur;
        if (inst->opcode == IR_LOAD_CONST) {
            Register *reg = new_alloc_reg();
            fprintf(out, "\tmovl\t$%s, %s\n", inst->src1->name, reg->name);
            inst->dest->reg = reg;
        }
        else if (inst->opcode == IR_LOAD_VAR) {
            Register *reg = new_alloc_reg();
            if (inst->src1->is_global) {
                fprintf(out, "\tmovl\t%s(%%rip), %s\n", inst->src1->name, reg->name);
            } else {
                int offset = get_local_offset(inst->src1->name);
                fprintf(out, "\tmovl\t-%d(%%rbp), %s\n", offset, reg->name);
            }
            inst->dest->reg = reg;
        }
        else if (inst->opcode == IR_STORE_VAR) {
            if (inst->dest->is_global) {
                fprintf(out, "\tmovl\t%s, %s(%%rip)\n", inst->src1->reg->name, inst->dest->name);
            } else {
                int offset = add_local(inst->dest->name);
                fprintf(out, "\tmovl\t%s, -%d(%%rbp)\n", inst->src1->reg->name, offset);
            }
        }
        else if (inst->opcode == IR_ADD) {
            Register *reg = inst->src1->reg;
            fprintf(out, "\taddl\t%s, %s\n", inst->src2->reg->name, reg->name);
            inst->dest->reg = reg;
        }
        else if (inst->opcode == IR_SUB) {
            Register *reg = inst->src1->reg;
            fprintf(out, "\tsubl\t%s, %s\n", inst->src2->reg->name, reg->name);
            inst->dest->reg = reg;
        }
        else if (inst->opcode == IR_CMP) {
            fprintf(out, "\tcmpl\t%s, %s\n", inst->src2->reg->name, inst->src1->reg->name);
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
        else if (inst->opcode == IR_CALL) {
            fprintf(out, "\tcall\t%s\n", inst->proc_name);
        }
        else if (inst->opcode == IR_RETURN) {
            fprintf(out, "\tleave\n");
            fprintf(out, "\tret\n");
        }
        else if (inst->opcode == IR_MUL) {
            fprintf(stderr, "imull: src1: %s, src2: %s\n", inst->src1->reg->name, inst->src2->reg->name);
            Register *reg = inst->src1->reg;
            fprintf(stderr, "imull: reg: %s\n", reg->name);
            fprintf(out, "\timull\t%s, %s\n", inst->src2->reg->name, reg->name);
            inst->dest->reg = reg;
            fprintf(stderr, "imull: done\n");
        }
        else if (inst->opcode == IR_DIV) {
            fprintf(stderr, "Generating idivl for %s and %s\n", inst->src1->reg->name, inst->src2->reg->name);
            fprintf(out, "\tmovl\t%s, %%eax\n", inst->src1->reg->name);
            fprintf(out, "\tcltd\n");
            fprintf(out, "\tidivl\t%s\n", inst->src2->reg->name);
            inst->dest->reg = new_alloc_reg();
            inst->dest->reg->name = "%eax";
        }
        else if (inst->opcode == IR_MOD) {
            fprintf(out, "\tmovl\t%s, %%eax\n", inst->src1->reg->name);
            fprintf(out, "\tcltd\n");
            fprintf(out, "\tidivl\t%s\n", inst->src2->reg->name);
            inst->dest->reg = new_alloc_reg();
            inst->dest->reg->name = "%edx";
        }
        current_ir = current_ir->next;
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

    // Free globals list
    ListNode_t *current = globals;
    while (current) {
        free(current->cur);
        current = current->next;
    }
    DestroyList(globals);
    globals = NULL;

    // Free locals list
    current = locals;
    while (current) {
        LocalSymbol *sym = current->cur;
        free(sym->name);
        free(sym);
        current = current->next;
    }
    DestroyList(locals);
    locals = NULL;
    local_stack_offset = 0;
}
