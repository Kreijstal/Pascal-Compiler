#include <stdlib.h>
#include <string.h>
#include "symbol_table.h"
#include "../../Parser/List/List.h"

typedef struct {
    char *name;
    int offset;
} Symbol;

struct SymbolTable {
    ListNode_t *symbols;
};

SymbolTable *symbol_table_new() {
    SymbolTable *table = malloc(sizeof(SymbolTable));
    table->symbols = NULL;
    return table;
}

void symbol_table_free(SymbolTable *table) {
    ListNode_t *current = table->symbols;
    while (current) {
        Symbol *sym = current->cur;
        free(sym->name);
        free(sym);
        current = current->next;
    }
    DestroyList(table->symbols);
    free(table);
}

void symbol_table_put(SymbolTable *table, char *name, int offset) {
    Symbol *sym = malloc(sizeof(Symbol));
    sym->name = strdup(name);
    sym->offset = offset;
    ListNode_t *node = CreateListNode(sym, LIST_UNSPECIFIED);
    if (table->symbols == NULL) {
        table->symbols = node;
    } else {
        PushListNodeBack(table->symbols, node);
    }
}

int symbol_table_get(SymbolTable *table, char *name) {
    ListNode_t *current = table->symbols;
    while (current) {
        Symbol *sym = current->cur;
        if (strcmp(sym->name, name) == 0) {
            return sym->offset;
        }
        current = current->next;
    }
    return -1; // Not found
}
