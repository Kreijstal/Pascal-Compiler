#include <stdio.h>

int main() {
    FILE* f = fopen("GPC/stdlib.p", "r");
    if (f == NULL) {
        perror("Error opening file");
        return 1;
    }
    printf("File opened successfully.\n");
    fclose(f);
    return 0;
}
