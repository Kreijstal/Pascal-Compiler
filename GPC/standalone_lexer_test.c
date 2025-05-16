#include <stdio.h>

/* Minimal token definitions */
enum {
    TOKEN_EOF = 0,
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_STRING,
    TOKEN_KEYWORD
};

extern FILE* yyin;
extern int yylex(void);

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
    while ((token = yylex()) != 0) {
        printf("Token: %d\n", token);
    }

    fclose(yyin);
    return 0;
}