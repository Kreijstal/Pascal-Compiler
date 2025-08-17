#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

typedef struct SymbolTable SymbolTable;

SymbolTable *symbol_table_new();
void symbol_table_free(SymbolTable *table);
void symbol_table_put(SymbolTable *table, char *name, int offset);
int symbol_table_get(SymbolTable *table, char *name);

#endif // SYMBOL_TABLE_H
