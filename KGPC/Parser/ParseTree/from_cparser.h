#ifndef FROM_CPARSER_H
#define FROM_CPARSER_H

#include "parser.h"
#include "pascal_parser.h"
#include "tree.h"
#include "KgpcType.h"

Tree_t *tree_from_pascal_ast(ast_t *program_ast);

/* Frontend error tracking */
void from_cparser_reset_error_count(void);
int from_cparser_get_error_count(void);

/* Forward declaration for symbol table - avoid circular dependency */
struct SymTab;

/* Method binding information */
typedef struct {
    char *class_name;
    char *method_name;
    int is_virtual;
    int is_override;
    int is_static;   /* 1 if method is static (no Self parameter) */
} ClassMethodBinding;

/* Convert an AST type specification to a KgpcType object.
 * This is used by the semantic checker to build type information.
 * Returns NULL if the type cannot be converted.
 * The caller owns the returned KgpcType and must free it with destroy_kgpc_type().
 */
KgpcType *convert_type_spec_to_kgpctype(ast_t *type_spec, struct SymTab *symtab);

/* Build a procedure type from a method template (used for interface method declarations). */
KgpcType *from_cparser_method_template_to_proctype(struct MethodTemplate *method_template,
    struct RecordType *record, struct SymTab *symtab);

/* Get method information for a class.
 * Returns a list of ClassMethodBinding for the given class.
 * Caller should NOT free the returned list - it's owned by the parser.
 */
void get_class_methods(const char *class_name, ListNode_t **methods_out, int *count_out);

/* Check if a method is declared as static (no Self parameter).
 * Returns 1 if static, 0 otherwise.
 */
int from_cparser_is_method_static(const char *class_name, const char *method_name);
int from_cparser_is_type_helper(const char *helper_id);

/* Check if a method is virtual (needs VMT dispatch).
 * Returns 1 if virtual or override, 0 otherwise.
 */
int from_cparser_is_method_virtual(const char *class_name, const char *method_name);

void from_cparser_enable_pending_specializations(void);
void from_cparser_disable_pending_specializations(void);
void resolve_pending_generic_aliases(Tree_t *program_tree);
void append_generic_method_clones(Tree_t *program_tree);
void resolve_pending_generic_subprograms(Tree_t *program_tree);

/* Get the method template AST for a class method declaration.
 * Returns the params_ast from the method template, or NULL if not found.
 * This is used to copy default parameter values from class declarations to implementations.
 */
struct MethodTemplate *from_cparser_get_method_template(struct RecordType *record, const char *method_name);

/* Convert a method template params AST into a parameter list (Tree_t var decls).
 * Returns a list of Tree_t* (LIST_TREE) or NULL if no params. */
ListNode_t *from_cparser_convert_params_ast(ast_t *params_ast);

/* Register a method template with the class method binding system.
 * This is used by the semantic checker to ensure method templates are findable.
 */
void from_cparser_register_method_template(const char *class_name, const char *method_name,
    int is_virtual, int is_override, int is_static);

/* Find all class names that have a method with the given name.
 * Returns a list of class names (caller must free each string and the list).
 * Sets *count_out to the number of classes found.
 */
ListNode_t *from_cparser_find_classes_with_method(const char *method_name, int *count_out);

/* Convert an AST expression node to an Expression structure.
 * This is used internally and also needed for copying default parameter values.
 */
struct Expression *from_cparser_convert_expression(ast_t *expr_node);

#endif /* FROM_CPARSER_H */
