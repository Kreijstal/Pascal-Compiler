#include <stdio.h>
#include "Parser/ErrVars.h"
#include "Parser/List/List.h"
#include "Parser/ParseTree/tree.h"
#include "Parser/LexAndYacc/Grammar.tab.h"

/* Local definitions override extern declarations */
int line_num = 1;
int col_num = 1;
Tree_t *parse_tree = NULL;
char *file_to_parse = NULL;
extern int yylex(void);

extern FILE* yyin;

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("Failed to open input file");
        return 1;
    }

    int token;
    while ((token = yylex()) != END_OF_FILE) {
        if (token == STRING) {
            printf("Token: %d (\"%s\")\n", token, yylval.str);
        } else {
            printf("Token: %d\n", token);
        }
    }

    fclose(yyin);
    return 0;
}