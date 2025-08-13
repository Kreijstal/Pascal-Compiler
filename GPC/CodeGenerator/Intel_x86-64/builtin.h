#ifndef BUILTIN_H
#define BUILTIN_H

#include <stdio.h>
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"

// Function prototypes for builtin handling
ListNode_t *codegen_builtin_write(ListNode_t *args, ListNode_t *inst_list, FILE *o_file, int *write_label_counter);
ListNode_t *codegen_builtin_writeln(ListNode_t *args, ListNode_t *inst_list, FILE *o_file, int *write_label_counter);
ListNode_t *codegen_builtin_read(ListNode_t *args, ListNode_t *inst_list, FILE *o_file, int *write_label_counter);

#endif // BUILTIN_H
