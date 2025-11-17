#include <stdio.h>
#include "cparser/parser.h"
#include "pascal_parser.h"

int main() {
    printf("PASCAL_T_BEGIN_BLOCK = %d\n", PASCAL_T_BEGIN_BLOCK);
    printf("PASCAL_T_MAIN_BLOCK = %d\n", PASCAL_T_MAIN_BLOCK);
    printf("PASCAL_T_VAR_SECTION = %d\n", PASCAL_T_VAR_SECTION);
    printf("PASCAL_T_LABEL_SECTION = %d\n", PASCAL_T_LABEL_SECTION);
    printf("PASCAL_T_TYPE_SECTION = %d\n", PASCAL_T_TYPE_SECTION);
    return 0;
}
