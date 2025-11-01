#ifndef FROM_CPARSER_H
#define FROM_CPARSER_H

#include "parser.h"
#include "pascal_parser.h"
#include "tree.h"
#include "GpcType.h"

Tree_t *tree_from_pascal_ast(ast_t *program_ast);

/* Forward declaration for symbol table - avoid circular dependency */
struct SymTab;

/* Convert an AST type specification to a GpcType object.
 * This is used by the semantic checker to build type information.
 * Returns NULL if the type cannot be converted.
 * The caller owns the returned GpcType and must free it with destroy_gpc_type().
 */
GpcType *convert_type_spec_to_gpctype(ast_t *type_spec, struct SymTab *symtab);

#endif /* FROM_CPARSER_H */
