#include <stdio.h>
#include "cparser/parser.h"
#include "pascal_parser.h"

int main() {
    printf("PASCAL_T_PROGRAM_DECL = %d\n", PASCAL_T_PROGRAM_DECL);
    printf("PASCAL_T_UNIT_DECL = %d\n", PASCAL_T_UNIT_DECL);
    printf("PASCAL_T_USES_SECTION = %d\n", PASCAL_T_USES_SECTION);
    return 0;
}
