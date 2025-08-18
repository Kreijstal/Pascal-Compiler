#include <stdio.h>

void print_integer(int n) {
    printf("%d\n", n);
}

void print_string(char *s) {
    printf("%s\n", s);
}

void print_integer_no_newline(int n) {
    printf("%d", n);
}

int read_integer() {
    int n;
    scanf("%d", &n);
    return n;
}
