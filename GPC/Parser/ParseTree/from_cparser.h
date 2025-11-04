#ifndef FROM_CPARSER_H
#define FROM_CPARSER_H

#include "parser.h"
#include "pascal_parser.h"
#include "tree.h"
#include "GpcType.h"

Tree_t *tree_from_pascal_ast(ast_t *program_ast);

/* Forward declaration for symbol table - avoid circular dependency */
struct SymTab;

/* Method binding information */
typedef struct {
    char *class_name;
    char *method_name;
    int is_virtual;
    int is_override;
} ClassMethodBinding;

/* Convert an AST type specification to a GpcType object.
 * This is used by the semantic checker to build type information.
 * Returns NULL if the type cannot be converted.
 * The caller owns the returned GpcType and must free it with destroy_gpc_type().
 */
GpcType *convert_type_spec_to_gpctype(ast_t *type_spec, struct SymTab *symtab);

/* Get method information for a class.
 * Returns a list of ClassMethodBinding for the given class.
 * Caller should NOT free the returned list - it's owned by the parser.
 */
void get_class_methods(const char *class_name, ListNode_t **methods_out, int *count_out);

#endif /* FROM_CPARSER_H */
