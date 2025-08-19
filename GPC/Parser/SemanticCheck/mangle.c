#include "mangle.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../Parser/ParseTree/tree_types.h"

char* get_mangled_name(char* base_name, ListNode_t* params) {
    if (params == NULL) {
        return strdup(base_name);
    }

    size_t len = strlen(base_name) + 1;
    char *mangled_name = malloc(len);
    if (mangled_name == NULL) { perror("malloc failed"); exit(1); }
    strcpy(mangled_name, base_name);

    ListNode_t* current_param_group = params;
    while(current_param_group != NULL) {
        Param_t* param_group = (Param_t*)current_param_group->cur;

        ListNode_t* current_id = param_group->id_list;
        while(current_id != NULL) {
            len += 2; // for "_" + type char
            mangled_name = realloc(mangled_name, len);
            if (mangled_name == NULL) { perror("realloc failed"); exit(1); }

            strcat(mangled_name, "_");
            switch(param_group->type) {
                case TYPE_INTEGER: strcat(mangled_name, "i"); break;
                case TYPE_LONGINT: strcat(mangled_name, "l"); break;
                case TYPE_REAL: strcat(mangled_name, "r"); break;
                case TYPE_STRING: strcat(mangled_name, "s"); break;
                case TYPE_BOOLEAN: strcat(mangled_name, "b"); break;
                case TYPE_PCHAR: strcat(mangled_name, "c"); break;
                default: strcat(mangled_name, "x"); break; // Unknown type
            }
            current_id = current_id->next;
        }
        current_param_group = current_param_group->next;
    }
    return mangled_name;
}
