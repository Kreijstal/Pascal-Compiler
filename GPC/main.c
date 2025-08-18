#include <stdio.h>
#include "Parser/ParseTree/tree.h"

extern int yyparse();
extern FILE *yyin;

int line_num = 1;
int col_num = 1;
char *file_to_parse = NULL;
Tree_t *parse_tree = NULL;

int main(int argc, char **argv)
{
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if(yyin == NULL)
        {
            fprintf(stderr, "Error opening file: %s\n", argv[1]);
            return 1;
        }
        file_to_parse = argv[1];
        yyparse();
    }
    return 0;
}
