/*
    Damon Gwinn
    Parse tree for the Pascal Grammar
*/

#include "tree.h"
#include "tree_types.h"
#include "type_tags.h"
#include "KgpcType.h"
#include "../SemanticCheck/HashTable/HashTable.h"  /* For HashType enum */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

extern void free_ast(struct ast_t *ast);
extern struct ast_t *copy_ast(struct ast_t *orig);

static void print_record_field(struct RecordField *field, FILE *f, int num_indent);
static void print_class_property(struct ClassProperty *property, FILE *f, int num_indent);
static void destroy_record_field(struct RecordField *field);
static void print_variant_part(struct VariantPart *variant, FILE *f, int num_indent);
static void print_variant_branch(struct VariantBranch *branch, FILE *f, int num_indent);
static void destroy_class_property(struct ClassProperty *property);
static void destroy_method_template(struct MethodTemplate *method);
static struct MethodTemplate *clone_method_template(const struct MethodTemplate *method);
static ListNode_t *clone_method_template_list(const ListNode_t *methods);
static struct ClassProperty *clone_class_property(const struct ClassProperty *property);
static ListNode_t *clone_property_list(const ListNode_t *properties);
static void destroy_variant_part(struct VariantPart *variant);
static void destroy_variant_branch(struct VariantBranch *branch);
static ListNode_t *clone_member_list(const ListNode_t *members);
static struct RecordField *clone_record_field(const struct RecordField *field);
static struct VariantBranch *clone_variant_branch_internal(const struct VariantBranch *branch);
static struct VariantPart *clone_variant_part_internal(const struct VariantPart *variant);
static void clear_type_alias_fields(struct TypeAlias *alias);

/* NOTE: tree_print and destroy_tree implicitely call stmt and expr functions */
/* Tree printing */
void print_indent(FILE *f, int num_indent)
{
    int i;
    for(i=0; i<num_indent; ++i)
        fprintf(f, "  ");
}

void list_print(ListNode_t *list, FILE *f, int num_indent)
{
    ListNode_t *cur;

    cur = list;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        switch(cur->type)
        {
            case LIST_TREE:
                tree_print((Tree_t *)cur->cur, f, num_indent);
                break;
            case LIST_EXPR:
                expr_print((struct Expression *)cur->cur, f, num_indent);
                break;
            case LIST_STMT:
                stmt_print((struct Statement *)cur->cur, f, num_indent);
                break;
            case LIST_STRING:
                print_indent(f, num_indent);
                fprintf(f, "%s\n", (char *)cur->cur);
                break;
            case LIST_RECORD_FIELD:
                print_record_field((struct RecordField *)cur->cur, f, num_indent);
                break;
            case LIST_CLASS_PROPERTY:
                print_class_property((struct ClassProperty *)cur->cur, f, num_indent);
                break;
            case LIST_SET_ELEMENT: {
                struct SetElement *element = (struct SetElement *)cur->cur;
                print_indent(f, num_indent);
                fprintf(f, "[SET_ELEMENT]:\n");
                if (element != NULL) {
                    if (element->lower != NULL) {
                        print_indent(f, num_indent + 1);
                        fprintf(f, "[LOWER]:\n");
                        expr_print(element->lower, f, num_indent + 2);
                    }
                    if (element->upper != NULL) {
                        print_indent(f, num_indent + 1);
                        fprintf(f, "[UPPER]:\n");
                        expr_print(element->upper, f, num_indent + 2);
                    }
                }
                break;
            }
            case LIST_VARIANT_PART:
                print_variant_part((struct VariantPart *)cur->cur, f, num_indent);
                break;
            case LIST_VARIANT_BRANCH:
                print_variant_branch((struct VariantBranch *)cur->cur, f, num_indent);
                break;
            case LIST_CASE_BRANCH: {
                struct CaseBranch *branch = (struct CaseBranch *)cur->cur;
                print_indent(f, num_indent);
                fprintf(f, "[CASE_BRANCH]\n");
                if (branch != NULL) {
                    if (branch->labels != NULL) {
                        print_indent(f, num_indent + 1);
                        fprintf(f, "[LABELS]:\n");
                        list_print(branch->labels, f, num_indent + 2);
                    }
                    if (branch->stmt != NULL) {
                        print_indent(f, num_indent + 1);
                        fprintf(f, "[STMT]:\n");
                        stmt_print(branch->stmt, f, num_indent + 2);
                    }
                }
                break;
            }
            case LIST_METHOD_TEMPLATE: {
                struct MethodTemplate *method = (struct MethodTemplate *)cur->cur;
                print_indent(f, num_indent);
                fprintf(f, "[METHOD_TEMPLATE:%s]\n",
                    method != NULL && method->name != NULL ? method->name : "<unnamed>");
                break;
            }
            case LIST_UNSPECIFIED:
                print_indent(f, num_indent);
                fprintf(f, "[UNSPECIFIED_LIST_ENTRY]\n");
                break;
            default:
                fprintf(stderr, "BAD TYPE IN list_print!\n");
                exit(1);
        }
        cur = cur->next;
    }
}

static void print_record_field(struct RecordField *field, FILE *f, int num_indent)
{
    if (field == NULL)
        return;

    print_indent(f, num_indent);
    fprintf(f, "[FIELD:%s", field->name != NULL ? field->name : "<unnamed>");
    if (field->type_id != NULL)
        fprintf(f, " type=%s", field->type_id);
    else
        fprintf(f, " type=%d", field->type);
    fprintf(f, "]\n");

    if (field->is_array)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[ARRAY_FIELD start=%d end=%d open=%d]", field->array_start,
            field->array_end, field->array_is_open);
        if (field->array_element_type_id != NULL)
            fprintf(f, " element_type=%s", field->array_element_type_id);
        else
            fprintf(f, " element_type=%d", field->array_element_type);
        fprintf(f, "\n");
    }

    if (field->nested_record != NULL)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[NESTED_RECORD]:\n");
        list_print(field->nested_record->fields, f, num_indent + 2);
    }
}

static void print_class_property(struct ClassProperty *property, FILE *f, int num_indent)
{
    if (property == NULL)
        return;

    print_indent(f, num_indent);
    fprintf(f, "[PROPERTY:%s", property->name != NULL ? property->name : "<unnamed>");
    if (property->type_id != NULL)
        fprintf(f, " type=%s", property->type_id);
    else
        fprintf(f, " type=%d", property->type);
    if (property->read_accessor != NULL)
        fprintf(f, " read=%s", property->read_accessor);
    if (property->write_accessor != NULL)
    fprintf(f, " write=%s", property->write_accessor);
    if (property->is_indexed)
        fprintf(f, " indexed");
    fprintf(f, "]\n");
}

static void destroy_class_property(struct ClassProperty *property)
{
    if (property == NULL)
        return;
    free(property->name);
    free(property->type_id);
    free(property->read_accessor);
    free(property->write_accessor);
    free(property);
}

static void destroy_method_template(struct MethodTemplate *method)
{
    if (method == NULL)
        return;
    free(method->name);
    if (method->method_ast != NULL)
        free_ast(method->method_ast);
    if (method->method_impl_ast != NULL)
        free_ast(method->method_impl_ast);
    if (method->method_tree != NULL)
        destroy_tree(method->method_tree);
    free(method);
}

static struct MethodTemplate *clone_method_template(const struct MethodTemplate *method)
{
    if (method == NULL)
        return NULL;

    struct MethodTemplate *clone = (struct MethodTemplate *)calloc(1, sizeof(struct MethodTemplate));
    if (clone == NULL)
        return NULL;

    clone->name = method->name != NULL ? strdup(method->name) : NULL;
    clone->method_ast = method->method_ast != NULL ? copy_ast(method->method_ast) : NULL;
    clone->method_tree = NULL; /* Method trees are rebuilt on demand */
    clone->kind = method->kind;
    clone->is_class_method = method->is_class_method;
    clone->is_static = method->is_static;
    clone->is_virtual = method->is_virtual;
    clone->is_override = method->is_override;
    clone->has_return_type = method->has_return_type;
    clone->params_ast = NULL;
    clone->return_type_ast = NULL;
    clone->directives_ast = NULL;
    clone->method_impl_ast = method->method_impl_ast != NULL ? copy_ast(method->method_impl_ast) : NULL;

    return clone;
}

static ListNode_t *clone_method_template_list(const ListNode_t *methods)
{
    if (methods == NULL)
        return NULL;

    ListNode_t *head = NULL;
    ListNode_t **tail = &head;
    const ListNode_t *cur = methods;
    while (cur != NULL)
    {
        if (cur->type == LIST_METHOD_TEMPLATE)
        {
            struct MethodTemplate *method_clone = clone_method_template((struct MethodTemplate *)cur->cur);
            if (method_clone != NULL)
            {
                ListNode_t *node = CreateListNode(method_clone, LIST_METHOD_TEMPLATE);
                *tail = node;
                tail = &node->next;
            }
        }
        cur = cur->next;
    }
    return head;
}

static struct ClassProperty *clone_class_property(const struct ClassProperty *property)
{
    if (property == NULL)
        return NULL;

    struct ClassProperty *clone = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    assert(clone != NULL);
    clone->name = property->name != NULL ? strdup(property->name) : NULL;
    clone->type = property->type;
    clone->type_id = property->type_id != NULL ? strdup(property->type_id) : NULL;
    clone->read_accessor = property->read_accessor != NULL ? strdup(property->read_accessor) : NULL;
    clone->write_accessor = property->write_accessor != NULL ? strdup(property->write_accessor) : NULL;
    clone->is_indexed = property->is_indexed;
    return clone;
}

static ListNode_t *clone_property_list(const ListNode_t *properties)
{
    if (properties == NULL)
        return NULL;

    ListNode_t *head = NULL;
    ListNode_t **tail = &head;
    const ListNode_t *cur = properties;
    while (cur != NULL)
    {
        if (cur->type == LIST_CLASS_PROPERTY)
        {
            struct ClassProperty *property_clone = clone_class_property((struct ClassProperty *)cur->cur);
            ListNode_t *node = CreateListNode(property_clone, LIST_CLASS_PROPERTY);
            *tail = node;
            tail = &node->next;
        }
        cur = cur->next;
    }
    return head;
}

static void print_variant_branch(struct VariantBranch *branch, FILE *f, int num_indent)
{
    if (branch == NULL)
        return;

    print_indent(f, num_indent);
    fprintf(f, "[VARIANT_BRANCH]\n");

    if (branch->labels != NULL)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[LABELS]:\n");
        list_print(branch->labels, f, num_indent + 2);
    }

    if (branch->members != NULL)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[MEMBERS]:\n");
        list_print(branch->members, f, num_indent + 2);
    }
}

static void print_variant_part(struct VariantPart *variant, FILE *f, int num_indent)
{
    if (variant == NULL)
        return;

    print_indent(f, num_indent);
    fprintf(f, "[VARIANT_PART]\n");

    if (variant->tag_field != NULL)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[TAG]:\n");
        print_record_field(variant->tag_field, f, num_indent + 2);
    }

    if (variant->branches != NULL)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[BRANCHES]:\n");
        list_print(variant->branches, f, num_indent + 2);
    }
}

static void destroy_record_field(struct RecordField *field)
{
    if (field == NULL)
        return;

    if (field->name != NULL)
        free(field->name);
    if (field->type_id != NULL)
        free(field->type_id);
    if (field->array_element_type_id != NULL)
        free(field->array_element_type_id);
    if (field->pointer_type_id != NULL)
        free(field->pointer_type_id);
    if (field->enum_literals != NULL)
        destroy_list(field->enum_literals);
    if (field->proc_type != NULL)
        kgpc_type_release(field->proc_type);
    destroy_record_type(field->nested_record);
    destroy_record_type(field->array_element_record);
    free(field);
}

static void destroy_variant_branch(struct VariantBranch *branch)
{
    if (branch == NULL)
        return;

    destroy_list(branch->labels);
    destroy_list(branch->members);
    free(branch);
}

static void destroy_variant_part(struct VariantPart *variant)
{
    if (variant == NULL)
        return;

    if (variant->tag_type_id != NULL)
        free(variant->tag_type_id);
    if (variant->tag_record != NULL)
        destroy_record_type(variant->tag_record);
    destroy_list(variant->branches);
    /* tag_field is owned by the surrounding field list */
    free(variant);
}

void tree_print(Tree_t *tree, FILE *f, int num_indent)
{
    print_indent(f, num_indent);

    switch(tree->type)
    {
        case TREE_PROGRAM_TYPE:
          assert(tree->tree_data.program_data.program_id != NULL);
          fprintf(f, "[PROGRAM:%s]\n", tree->tree_data.program_data.program_id);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[ARGS]:\n");
          list_print(tree->tree_data.program_data.args_char, f, num_indent+1);

        print_indent(f, num_indent);
        fprintf(f, "[USES]:\n");
        list_print(tree->tree_data.program_data.uses_units, f, num_indent+1);

        print_indent(f, num_indent);
        fprintf(f, "[LABELS]:\n");
        list_print(tree->tree_data.program_data.label_declaration, f, num_indent+1);

        print_indent(f, num_indent);
        fprintf(f, "[CONST_DECLS]:\n");
        list_print(tree->tree_data.program_data.const_declaration, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[TYPE_DECLS]:\n");
          list_print(tree->tree_data.program_data.type_declaration, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[VAR_DECLS]:\n");
          list_print(tree->tree_data.program_data.var_declaration, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[SUBPROGRAMS]:\n");
          list_print(tree->tree_data.program_data.subprograms, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[BODY]:\n");
          stmt_print(tree->tree_data.program_data.body_statement, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[FINALIZATION_STATEMENTS]:\n");
          list_print(tree->tree_data.program_data.finalization_statements, f, num_indent+1);
          break;

        case TREE_UNIT:
          assert(tree->tree_data.unit_data.unit_id != NULL);
          fprintf(f, "[UNIT:%s]\n", tree->tree_data.unit_data.unit_id);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[INTERFACE_USES]:\n");
          list_print(tree->tree_data.unit_data.interface_uses, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[INTERFACE_CONST_DECLS]:\n");
          list_print(tree->tree_data.unit_data.interface_const_decls, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[INTERFACE_TYPE_DECLS]:\n");
          list_print(tree->tree_data.unit_data.interface_type_decls, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[INTERFACE_VAR_DECLS]:\n");
          list_print(tree->tree_data.unit_data.interface_var_decls, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[IMPLEMENTATION_USES]:\n");
          list_print(tree->tree_data.unit_data.implementation_uses, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[IMPLEMENTATION_CONST_DECLS]:\n");
          list_print(tree->tree_data.unit_data.implementation_const_decls, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[IMPLEMENTATION_TYPE_DECLS]:\n");
          list_print(tree->tree_data.unit_data.implementation_type_decls, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[IMPLEMENTATION_VAR_DECLS]:\n");
          list_print(tree->tree_data.unit_data.implementation_var_decls, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[SUBPROGRAMS]:\n");
          list_print(tree->tree_data.unit_data.subprograms, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[INITIALIZATION]:\n");
          stmt_print(tree->tree_data.unit_data.initialization, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[FINALIZATION]:\n");
          stmt_print(tree->tree_data.unit_data.finalization, f, num_indent+1);
          break;

        case TREE_SUBPROGRAM:
          switch(tree->tree_data.subprogram_data.sub_type)
          {
              case TREE_SUBPROGRAM_PROC:
                fprintf(f, "[PROCEDURE:%s]:\n", tree->tree_data.subprogram_data.id);
                print_indent(f, num_indent);
                break;
              case TREE_SUBPROGRAM_FUNC:
                fprintf(f, "[FUNCTION:%s]:\n", tree->tree_data.subprogram_data.id);
                print_indent(f, num_indent);
                if (tree->tree_data.subprogram_data.return_type_id != NULL)
                    fprintf(f, "[RETURNS:%s]\n", tree->tree_data.subprogram_data.return_type_id);
                else
                    fprintf(f, "[RETURNS:%d]\n", tree->tree_data.subprogram_data.return_type);
                break;

              default:
                  fprintf(stderr, "BAD TYPE IN TREE_SUBPROGRAM!\n");
                  exit(1);
          }
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[ARGS]:\n");
          list_print(tree->tree_data.subprogram_data.args_var, f, num_indent+1);

        print_indent(f, num_indent);
        fprintf(f, "[CONST_DECLS]:\n");
        list_print(tree->tree_data.subprogram_data.const_declarations, f, num_indent+1);

        print_indent(f, num_indent);
        fprintf(f, "[LABELS]:\n");
        list_print(tree->tree_data.subprogram_data.label_declarations, f, num_indent+1);
       
        print_indent(f, num_indent);
        fprintf(f, "[TYPE_DECLS]:\n");
        list_print(tree->tree_data.subprogram_data.type_declarations, f, num_indent+1);

        print_indent(f, num_indent);
        fprintf(f, "[VAR_DECLS]:\n");
        list_print(tree->tree_data.subprogram_data.declarations, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[SUBPROGRAMS]:\n");
          list_print(tree->tree_data.subprogram_data.subprograms, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[BODY]:\n");
          stmt_print(tree->tree_data.subprogram_data.statement_list, f, num_indent+1);
          break;

        case TREE_VAR_DECL:
          if (tree->tree_data.var_decl_data.type_id != NULL)
            fprintf(f, "[VARDECL of type %s]\n", tree->tree_data.var_decl_data.type_id);
          else
            fprintf(f, "[VARDECL of type %d]\n", tree->tree_data.var_decl_data.type);
          list_print(tree->tree_data.var_decl_data.ids, f, num_indent+1);
          break;

        case TREE_CONST_DECL:
          if (tree->tree_data.const_decl_data.type_id != NULL)
            fprintf(f, "[CONSTDECL %s of type %s]\n",
                tree->tree_data.const_decl_data.id,
                tree->tree_data.const_decl_data.type_id);
          else
            fprintf(f, "[CONSTDECL %s]\n", tree->tree_data.const_decl_data.id);

          if (tree->tree_data.const_decl_data.value != NULL)
          {
              print_indent(f, num_indent+1);
              fprintf(f, "[VALUE]:\n");
              expr_print(tree->tree_data.const_decl_data.value, f, num_indent+2);
          }
          break;

        case TREE_ARR_DECL:
          if (tree->tree_data.arr_decl_data.type_id != NULL)
            fprintf(f, "[ARRDECL of type %s in range(%d, %d)]\n",
                tree->tree_data.arr_decl_data.type_id,
                tree->tree_data.arr_decl_data.s_range,
                tree->tree_data.arr_decl_data.e_range);
          else
            fprintf(f, "[ARRDECL of type %d in range(%d, %d)]\n",
                tree->tree_data.arr_decl_data.type,
                tree->tree_data.arr_decl_data.s_range,
                tree->tree_data.arr_decl_data.e_range);

          list_print(tree->tree_data.arr_decl_data.ids, f, num_indent+1);
          if (tree->tree_data.arr_decl_data.initializer != NULL)
          {
              print_indent(f, num_indent + 1);
              fprintf(f, "[INITIALIZER]:\n");
              stmt_print(tree->tree_data.arr_decl_data.initializer, f, num_indent + 2);
          }
          break;

        case TREE_TYPE_DECL:
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
            {
                fprintf(f, "[TYPEDECL:%s RECORD]\n", tree->tree_data.type_decl_data.id);
                if (tree->tree_data.type_decl_data.info.record != NULL)
                {
                    print_indent(f, num_indent + 1);
                    fprintf(f, "[FIELDS]:\n");
                    list_print(tree->tree_data.type_decl_data.info.record->fields, f, num_indent + 2);
                    if (tree->tree_data.type_decl_data.info.record->properties != NULL)
                    {
                        print_indent(f, num_indent + 1);
                        fprintf(f, "[PROPERTIES]:\n");
                        list_print(tree->tree_data.type_decl_data.info.record->properties, f, num_indent + 2);
                    }
                }
            }
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                const struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                fprintf(f, "[TYPEDECL:%s ALIAS]\n", tree->tree_data.type_decl_data.id);
                print_indent(f, num_indent + 1);
                if (alias->is_array)
                {
                    fprintf(f, "[ARRAY ALIAS start=%d end=%d open=%d]\n",
                        alias->array_start, alias->array_end, alias->is_open_array);
                    print_indent(f, num_indent + 1);
                    if (alias->array_element_type_id != NULL)
                        fprintf(f, "[ELEMENT_TYPE:%s]\n", alias->array_element_type_id);
                    else
                        fprintf(f, "[ELEMENT_TYPE:%d]\n", alias->array_element_type);
                }
                else if (alias->target_type_id != NULL)
                {
                    fprintf(f, "[ALIASES:%s]\n", alias->target_type_id);
                }
                else
                {
                    fprintf(f, "[ALIASES_TYPE:%d]\n", alias->base_type);
                }
            }
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_GENERIC)
            {
                const struct GenericDecl *generic = &tree->tree_data.type_decl_data.info.generic;
                fprintf(f, "[TYPEDECL:%s GENERIC]\n", tree->tree_data.type_decl_data.id);
                print_indent(f, num_indent + 1);
                fprintf(f, "[TYPE_PARAMETERS:%d]\n", generic->num_type_params);
                for (int i = 0; i < generic->num_type_params; ++i)
                {
                    print_indent(f, num_indent + 2);
                    fprintf(f, "%s\n", generic->type_parameters[i]);
                }
            }
            else
            {
                fprintf(f, "[TYPEDECL:%s = %d..%d]\n", tree->tree_data.type_decl_data.id,
                    tree->tree_data.type_decl_data.info.range.start, tree->tree_data.type_decl_data.info.range.end);
            }
            break;

        default:
        fprintf(stderr, "BAD TYPE IN tree_print!\n");
        exit(1);
    }
}

void stmt_print(struct Statement *stmt, FILE *f, int num_indent)
{
    if (stmt == NULL) {
        print_indent(f, num_indent);
        fprintf(f, "[NULL_STMT]\\n");
        return;
    }
    print_indent(f, num_indent);
    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
          fprintf(f, "[VARASSIGN]:\n");
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[VAR]:\n");
          expr_print(stmt->stmt_data.var_assign_data.var, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[EXPR]:\n");
          expr_print(stmt->stmt_data.var_assign_data.expr, f, num_indent+1);
          break;

        case STMT_PROCEDURE_CALL:
          fprintf(f, "[PROCEDURE_CALL:%s]:\n", stmt->stmt_data.procedure_call_data.id);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[ARGS]:\n");
          list_print(stmt->stmt_data.procedure_call_data.expr_args, f, num_indent+1);
          break;

        case STMT_EXPR:
          fprintf(f, "[EXPR_STMT]:\n");
          expr_print(stmt->stmt_data.expr_stmt_data.expr, f, num_indent+1);
          break;

        case STMT_COMPOUND_STATEMENT:
          fprintf(f, "[COMPOUND_STMT]:\n");
          list_print(stmt->stmt_data.compound_statement, f, num_indent+1);
          break;

        case STMT_LABEL:
          fprintf(f, "[LABEL:%s]:\n", stmt->stmt_data.label_data.label != NULL ?
              stmt->stmt_data.label_data.label : "<unnamed>");
          if (stmt->stmt_data.label_data.stmt != NULL)
              stmt_print(stmt->stmt_data.label_data.stmt, f, num_indent+1);
          break;

        case STMT_GOTO:
          fprintf(f, "[GOTO:%s]\n", stmt->stmt_data.goto_data.label != NULL ?
              stmt->stmt_data.goto_data.label : "<unnamed>");
          break;

        case STMT_IF_THEN:
          fprintf(f, "[IF]:\n");
          expr_print(stmt->stmt_data.if_then_data.relop_expr, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[THEN]:\n");
          stmt_print(stmt->stmt_data.if_then_data.if_stmt, f, num_indent+1);

          if(stmt->stmt_data.if_then_data.else_stmt != NULL)
          {
              print_indent(f, num_indent);
              fprintf(f, "[ELSE]:\n");
              stmt_print(stmt->stmt_data.if_then_data.else_stmt, f, num_indent+1);
          }
          break;

        case STMT_WHILE:
          fprintf(f, "[WHILE]:\n");
          expr_print(stmt->stmt_data.while_data.relop_expr, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[DO]:\n");
          stmt_print(stmt->stmt_data.while_data.while_stmt, f, num_indent+1);
          break;

        case STMT_REPEAT:
          fprintf(f, "[REPEAT]:\n");
          list_print(stmt->stmt_data.repeat_data.body_list, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[UNTIL]:\n");
          expr_print(stmt->stmt_data.repeat_data.until_expr, f, num_indent+1);
          break;

        case STMT_FOR:
          fprintf(f, "[FOR]:\n");
          switch(stmt->stmt_data.for_data.for_assign_type)
          {
              case STMT_FOR_VAR:
                expr_print(stmt->stmt_data.for_data.for_assign_data.var, f, num_indent+1);
                break;
              case STMT_FOR_ASSIGN_VAR:
                stmt_print(stmt->stmt_data.for_data.for_assign_data.var_assign, f, num_indent+1);
                break;
              default:
                fprintf(stderr, "BAD TYPE IN STMT_FOR!\n");
                exit(1);
          }

          print_indent(f, num_indent);
          fprintf(f, "[TO]:\n");
          expr_print(stmt->stmt_data.for_data.to, f, num_indent+1);

          print_indent(f, num_indent);
              fprintf(f, "[DO]:\n");
          stmt_print(stmt->stmt_data.for_data.do_for, f, num_indent+1);
          break;

        case STMT_BREAK:
          fprintf(f, "[BREAK]\n");
          break;
        case STMT_CONTINUE:
            fprintf(f, "[CONTINUE]\n");
            break;

        case STMT_ASM_BLOCK:
          fprintf(f, "[ASM_BLOCK]:\n");
          print_indent(f, num_indent+1);
          fprintf(f, "%s\n", stmt->stmt_data.asm_block_data.code);
          break;

        case STMT_EXIT:
          fprintf(f, "[EXIT]");
          if (stmt->stmt_data.exit_data.return_expr != NULL)
          {
            fprintf(f, ":\n");
            ++num_indent;
            print_indent(f, num_indent);
            fprintf(f, "[RETURN_VALUE]:\n");
            expr_print(stmt->stmt_data.exit_data.return_expr, f, num_indent + 1);
            --num_indent;
          }
          else
          {
            fprintf(f, "\n");
          }
          break;

        case STMT_CASE:
            fprintf(f, "[CASE]:\n");
            ++num_indent;
            print_indent(f, num_indent);
            fprintf(f, "[SELECTOR]:\n");
            expr_print(stmt->stmt_data.case_data.selector_expr, f, num_indent + 1);
            print_indent(f, num_indent);
            fprintf(f, "[BRANCHES]:\n");
            {
                ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
                while (branch_node != NULL)
                {
                    struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
                    print_indent(f, num_indent + 1);
                    fprintf(f, "[BRANCH]:\n");
                    print_indent(f, num_indent + 2);
                    fprintf(f, "[LABELS]:\n");
                    list_print(branch->labels, f, num_indent + 3);
                    if (branch->stmt != NULL)
                    {
                        print_indent(f, num_indent + 2);
                        fprintf(f, "[STMT]:\n");
                        stmt_print(branch->stmt, f, num_indent + 3);
                    }
                    branch_node = branch_node->next;
                }
            }
            if (stmt->stmt_data.case_data.else_stmt != NULL)
            {
                print_indent(f, num_indent);
                fprintf(f, "[ELSE]:\n");
                stmt_print(stmt->stmt_data.case_data.else_stmt, f, num_indent + 1);
            }
            break;

        case STMT_WITH:
            fprintf(f, "[WITH]:\n");
            ++num_indent;
            print_indent(f, num_indent);
            fprintf(f, "[CONTEXT]:\n");
            expr_print(stmt->stmt_data.with_data.context_expr, f, num_indent + 1);
            if (stmt->stmt_data.with_data.body_stmt != NULL)
            {
                print_indent(f, num_indent);
                fprintf(f, "[BODY]:\n");
                stmt_print(stmt->stmt_data.with_data.body_stmt, f, num_indent + 1);
            }
            break;

        case STMT_TRY_FINALLY:
            fprintf(f, "[TRY_FINALLY]:\n");
            ++num_indent;
            print_indent(f, num_indent);
            fprintf(f, "[TRY]:\n");
            list_print(stmt->stmt_data.try_finally_data.try_statements, f, num_indent + 1);
            print_indent(f, num_indent);
            fprintf(f, "[FINALLY]:\n");
            list_print(stmt->stmt_data.try_finally_data.finally_statements, f, num_indent + 1);
            break;

        case STMT_TRY_EXCEPT:
            fprintf(f, "[TRY_EXCEPT]:\n");
            ++num_indent;
            print_indent(f, num_indent);
            fprintf(f, "[TRY]:\n");
            list_print(stmt->stmt_data.try_except_data.try_statements, f, num_indent + 1);
            print_indent(f, num_indent);
            fprintf(f, "[EXCEPT]:\n");
            list_print(stmt->stmt_data.try_except_data.except_statements, f, num_indent + 1);
            break;

        case STMT_RAISE:
            fprintf(f, "[RAISE]:\n");
            if (stmt->stmt_data.raise_data.exception_expr != NULL)
                expr_print(stmt->stmt_data.raise_data.exception_expr, f, num_indent + 1);
            break;

        case STMT_INHERITED:
            fprintf(f, "[INHERITED]:\n");
            if (stmt->stmt_data.inherited_data.call_expr != NULL)
                expr_print(stmt->stmt_data.inherited_data.call_expr, f, num_indent + 1);
            break;

        default:
          fprintf(stderr, "BAD TYPE IN stmt_print! type=%d\n", stmt->type);
          print_indent(f, num_indent);
          fprintf(f, "[STMT:unknown type=%d]\n", stmt->type);
          return;
    }
}

void expr_print(struct Expression *expr, FILE *f, int num_indent)
{
    if (expr == NULL)
    {
        print_indent(f, num_indent);
        fprintf(f, "[EXPR:null]\n");
        return;
    }
    print_indent(f, num_indent);
    switch(expr->type)
    {
        case EXPR_RELOP:
          fprintf(f, "[RELOP:%d]:\n", expr->expr_data.relop_data.type);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[LEFT]:\n");
          expr_print(expr->expr_data.relop_data.left, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[RIGHT]:\n");
          expr_print(expr->expr_data.relop_data.right, f, num_indent+1);
          break;

        case EXPR_SIGN_TERM:
          fprintf(f, "[SIGN]:\n");
          expr_print(expr->expr_data.sign_term, f, num_indent+1);
          break;

        case EXPR_ADDOP:
          fprintf(f, "[ADDOP:%d]:\n", expr->expr_data.addop_data.addop_type);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[LEFT]:\n");
          expr_print(expr->expr_data.addop_data.left_expr, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[RIGHT]:\n");
          expr_print(expr->expr_data.addop_data.right_term, f, num_indent+1);
          break;

        case EXPR_MULOP:
          fprintf(f, "[MULOP:%d]:\n", expr->expr_data.mulop_data.mulop_type);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[LEFT]:\n");
          expr_print(expr->expr_data.mulop_data.left_term, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[RIGHT]:\n");
          expr_print(expr->expr_data.mulop_data.right_factor, f, num_indent+1);
          break;

        case EXPR_VAR_ID:
          fprintf(f, "[VAR_ID:%s]\n", expr->expr_data.id);
          break;

        case EXPR_ARRAY_ACCESS:
          fprintf(f, "[ARRAY_ACC]\n");
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[ARRAY]:\n");
          expr_print(expr->expr_data.array_access_data.array_expr, f, num_indent+1);

          print_indent(f, num_indent);
          fprintf(f, "[INDEX]:\n");
          expr_print(expr->expr_data.array_access_data.index_expr, f, num_indent+1);

          /* Print extra indices for multi-dimensional arrays */
          if (expr->expr_data.array_access_data.extra_indices != NULL)
          {
              int idx_num = 2;
              ListNode_t *idx = expr->expr_data.array_access_data.extra_indices;
              while (idx != NULL)
              {
                  print_indent(f, num_indent);
                  fprintf(f, "[INDEX_%d]:\n", idx_num++);
                  if (idx->cur != NULL)
                      expr_print((struct Expression *)idx->cur, f, num_indent+1);
                  idx = idx->next;
              }
          }
          break;

        case EXPR_RECORD_ACCESS:
          fprintf(f, "[RECORD_ACC:%s]\n",
              expr->expr_data.record_access_data.field_id != NULL ?
              expr->expr_data.record_access_data.field_id : "<unknown>");
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[RECORD]:\n");
          expr_print(expr->expr_data.record_access_data.record_expr, f, num_indent+1);
          break;

        case EXPR_FUNCTION_CALL:
          fprintf(f, "[FUNC_CALL:%s]:\n", expr->expr_data.function_call_data.id);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[ARGS]:\n");
          list_print(expr->expr_data.function_call_data.args_expr, f, num_indent+1);
          break;

        case EXPR_INUM:
          fprintf(f, "[I_NUM:%lld]\n", expr->expr_data.i_num);
          break;

        case EXPR_RNUM:
          fprintf(f, "[R_NUM:%f]\n", expr->expr_data.r_num);
          break;

        case EXPR_STRING:
          fprintf(f, "[STRING:%s]\n", expr->expr_data.string);
          break;
        case EXPR_CHAR_CODE:
          fprintf(f, "[CHAR_CODE:%u]\n", expr->expr_data.char_code);
          break;

        case EXPR_BOOL:
          fprintf(f, "[BOOL:%s]\n", expr->expr_data.bool_value ? "TRUE" : "FALSE");
          break;
        case EXPR_NIL:
          fprintf(f, "[NIL]\n");
          break;
        case EXPR_SET:
          fprintf(f, "[SET:0x%X]\n", expr->expr_data.set_data.bitmask);
          break;

        case EXPR_ARRAY_LITERAL:
        {
          fprintf(f, "[ARRAY_LITERAL count=%d]\n",
              expr->expr_data.array_literal_data.element_count);
          ListNode_t *cur = expr->expr_data.array_literal_data.elements;
          while (cur != NULL)
          {
              print_indent(f, num_indent + 1);
              fprintf(f, "[ELEMENT]:\n");
              expr_print((struct Expression *)cur->cur, f, num_indent + 2);
              cur = cur->next;
          }
          break;
        }

        case EXPR_RECORD_CONSTRUCTOR:
        {
          fprintf(f, "[RECORD_CONSTRUCTOR count=%d]\n",
              expr->expr_data.record_constructor_data.field_count);
          ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
          while (cur != NULL)
          {
              struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
              print_indent(f, num_indent + 1);
              fprintf(f, "[FIELD:%s]:\n",
                  field != NULL && field->field_id != NULL ? field->field_id : "<unknown>");
              if (field != NULL && field->value != NULL)
                  expr_print(field->value, f, num_indent + 2);
              cur = cur->next;
          }
          break;
        }

        case EXPR_POINTER_DEREF:
          fprintf(f, "[POINTER_DEREF]\n");
          ++num_indent;
          print_indent(f, num_indent);
          fprintf(f, "[POINTER]:\n");
          expr_print(expr->expr_data.pointer_deref_data.pointer_expr, f, num_indent+1);
          --num_indent;
          break;

        case EXPR_ADDR:
          fprintf(f, "[ADDR]\n");
          ++num_indent;
          print_indent(f, num_indent);
          fprintf(f, "[EXPR]:\n");
          expr_print(expr->expr_data.addr_data.expr, f, num_indent+1);
          --num_indent;
          break;

        case EXPR_TYPECAST:
          fprintf(f, "[TYPECAST]\n");
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[TARGET_TYPE:%d]\n", expr->expr_data.typecast_data.target_type);

          if (expr->expr_data.typecast_data.target_type_id != NULL)
          {
              print_indent(f, num_indent);
              fprintf(f, "[TARGET_TYPE_ID:%s]\n", expr->expr_data.typecast_data.target_type_id);
          }

          print_indent(f, num_indent);
          fprintf(f, "[EXPR]:\n");
          expr_print(expr->expr_data.typecast_data.expr, f, num_indent+1);
          --num_indent;
          break;
        case EXPR_IS:
          fprintf(f, "[IS]\n");
          ++num_indent;
          print_indent(f, num_indent);
          fprintf(f, "[TARGET_TYPE:%d]\n", expr->expr_data.is_data.target_type);
          if (expr->expr_data.is_data.target_type_id != NULL)
          {
              print_indent(f, num_indent);
              fprintf(f, "[TARGET_TYPE_ID:%s]\n", expr->expr_data.is_data.target_type_id);
          }
          print_indent(f, num_indent);
          fprintf(f, "[EXPR]:\n");
          expr_print(expr->expr_data.is_data.expr, f, num_indent + 1);
          --num_indent;
          break;
        case EXPR_AS:
          fprintf(f, "[AS]\n");
          ++num_indent;
          print_indent(f, num_indent);
          fprintf(f, "[TARGET_TYPE:%d]\n", expr->expr_data.as_data.target_type);
          if (expr->expr_data.as_data.target_type_id != NULL)
          {
              print_indent(f, num_indent);
              fprintf(f, "[TARGET_TYPE_ID:%s]\n", expr->expr_data.as_data.target_type_id);
          }
          print_indent(f, num_indent);
          fprintf(f, "[EXPR]:\n");
          expr_print(expr->expr_data.as_data.expr, f, num_indent + 1);
          --num_indent;
          break;

        default:
          fprintf(stderr, "BAD TYPE IN expr_print! type=%d\n", expr->type);
          print_indent(f, num_indent);
          fprintf(f, "[EXPR:unknown type=%d]\n", expr->type);
          return;
    }
}

/* Tree freeing */
/* WARNING: Also frees all c strings and other such types */
void destroy_list(ListNode_t *list)
{
    ListNode_t *cur, *prev;
    if(list != NULL)
    {
        cur = list;
        while(cur != NULL)
        {
            assert(cur->cur != NULL);
            switch(cur->type)
            {
                case LIST_TREE:
                    destroy_tree((Tree_t *)cur->cur);
                    break;
                case LIST_STMT:
                    destroy_stmt((struct Statement *)cur->cur);
                    break;
                case LIST_EXPR:
                    destroy_expr((struct Expression *)cur->cur);
                    break;
            case LIST_STRING:
                free((char *)cur->cur);
                break;
            case LIST_RECORD_FIELD:
                destroy_record_field((struct RecordField *)cur->cur);
                break;
            case LIST_CLASS_PROPERTY:
                destroy_class_property((struct ClassProperty *)cur->cur);
                break;
            case LIST_SET_ELEMENT:
                destroy_set_element((struct SetElement *)cur->cur);
                break;
            case LIST_CASE_BRANCH:
                /* Case branches are handled specially in destroy_stmt for STMT_CASE */
                /* Don't free the branch here as it's already freed there */
                break;
            case LIST_VARIANT_PART:
                destroy_variant_part((struct VariantPart *)cur->cur);
                break;
            case LIST_VARIANT_BRANCH:
                destroy_variant_branch((struct VariantBranch *)cur->cur);
                break;
            case LIST_METHOD_TEMPLATE:
                destroy_method_template((struct MethodTemplate *)cur->cur);
                break;
            case LIST_UNSPECIFIED:
                /* LIST_UNSPECIFIED nodes have unknown content type - cannot safely free.
                 * These are used for temporary storage where the content ownership
                 * is managed elsewhere. Do not free cur->cur here. */
                break;
            default:
                fprintf(stderr, "BAD TYPE IN destroy_list [%d]!\n", cur->type);
                exit(1);
            }
            prev = cur;
            cur = cur->next;
            free(prev);
        }
    }
}

void destroy_tree(Tree_t *tree)
{
    assert(tree != NULL);
    switch(tree->type)
    {
        case TREE_PROGRAM_TYPE:
          free(tree->tree_data.program_data.program_id);
          destroy_list(tree->tree_data.program_data.args_char);
          destroy_list(tree->tree_data.program_data.uses_units);

          destroy_list(tree->tree_data.program_data.label_declaration);

          destroy_list(tree->tree_data.program_data.const_declaration);
          destroy_list(tree->tree_data.program_data.type_declaration);

          destroy_list(tree->tree_data.program_data.var_declaration);

          destroy_list(tree->tree_data.program_data.subprograms);

          destroy_stmt(tree->tree_data.program_data.body_statement);
          
          destroy_list(tree->tree_data.program_data.finalization_statements);
          break;

        case TREE_UNIT:
          free(tree->tree_data.unit_data.unit_id);
          destroy_list(tree->tree_data.unit_data.interface_uses);
          destroy_list(tree->tree_data.unit_data.interface_const_decls);
          destroy_list(tree->tree_data.unit_data.interface_type_decls);
          destroy_list(tree->tree_data.unit_data.interface_var_decls);
          destroy_list(tree->tree_data.unit_data.implementation_uses);
          destroy_list(tree->tree_data.unit_data.implementation_const_decls);
          destroy_list(tree->tree_data.unit_data.implementation_type_decls);
          destroy_list(tree->tree_data.unit_data.implementation_var_decls);
          destroy_list(tree->tree_data.unit_data.subprograms);
          if (tree->tree_data.unit_data.initialization != NULL)
              destroy_stmt(tree->tree_data.unit_data.initialization);
          if (tree->tree_data.unit_data.finalization != NULL)
              destroy_stmt(tree->tree_data.unit_data.finalization);
          break;

        case TREE_SUBPROGRAM:
          free(tree->tree_data.subprogram_data.id);
          if (tree->tree_data.subprogram_data.mangled_id != NULL)
            free(tree->tree_data.subprogram_data.mangled_id);
          if (tree->tree_data.subprogram_data.cname_override != NULL)
            free(tree->tree_data.subprogram_data.cname_override);
          if (tree->tree_data.subprogram_data.return_type_id != NULL)
            free(tree->tree_data.subprogram_data.return_type_id);

          destroy_list(tree->tree_data.subprogram_data.args_var);

          destroy_list(tree->tree_data.subprogram_data.const_declarations);
          destroy_list(tree->tree_data.subprogram_data.label_declarations);
          destroy_list(tree->tree_data.subprogram_data.type_declarations);
          destroy_list(tree->tree_data.subprogram_data.declarations);

          destroy_list(tree->tree_data.subprogram_data.subprograms);

          destroy_stmt(tree->tree_data.subprogram_data.statement_list);
          if (tree->tree_data.subprogram_data.generic_type_params != NULL) {
              for (int i = 0; i < tree->tree_data.subprogram_data.num_generic_type_params; i++)
                  free(tree->tree_data.subprogram_data.generic_type_params[i]);
              free(tree->tree_data.subprogram_data.generic_type_params);
          }
          if (tree->tree_data.subprogram_data.generic_template_ast != NULL)
              free_ast(tree->tree_data.subprogram_data.generic_template_ast);
          if (tree->tree_data.subprogram_data.result_var_name != NULL)
              free(tree->tree_data.subprogram_data.result_var_name);
          break;

        case TREE_VAR_DECL:
          destroy_list(tree->tree_data.var_decl_data.ids);
          if (tree->tree_data.var_decl_data.type_id != NULL)
              free(tree->tree_data.var_decl_data.type_id);
          if (tree->tree_data.var_decl_data.initializer != NULL)
              destroy_stmt(tree->tree_data.var_decl_data.initializer);
          if (tree->tree_data.var_decl_data.inline_record_type != NULL)
              destroy_record_type(tree->tree_data.var_decl_data.inline_record_type);
          if (tree->tree_data.var_decl_data.inline_type_alias != NULL)
          {
              clear_type_alias_fields(tree->tree_data.var_decl_data.inline_type_alias);
              free(tree->tree_data.var_decl_data.inline_type_alias);
          }
          if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
          {
              destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
              tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
          }
          if (tree->tree_data.var_decl_data.absolute_target != NULL)
              free(tree->tree_data.var_decl_data.absolute_target);
          break;

        case TREE_ARR_DECL:
          destroy_list(tree->tree_data.arr_decl_data.ids);
          if (tree->tree_data.arr_decl_data.type_id != NULL)
            free(tree->tree_data.arr_decl_data.type_id);
          if (tree->tree_data.arr_decl_data.range_str != NULL)
            free(tree->tree_data.arr_decl_data.range_str);
          if (tree->tree_data.arr_decl_data.initializer != NULL)
              destroy_stmt(tree->tree_data.arr_decl_data.initializer);
          if (tree->tree_data.arr_decl_data.static_label != NULL)
              free(tree->tree_data.arr_decl_data.static_label);
          if (tree->tree_data.arr_decl_data.init_guard_label != NULL)
              free(tree->tree_data.arr_decl_data.init_guard_label);
          break;

        case TREE_CONST_DECL:
          free(tree->tree_data.const_decl_data.id);
          if (tree->tree_data.const_decl_data.type_id != NULL)
            free(tree->tree_data.const_decl_data.type_id);
          destroy_expr(tree->tree_data.const_decl_data.value);
          break;

        case TREE_TYPE_DECL:
            free(tree->tree_data.type_decl_data.id);
            if (tree->tree_data.type_decl_data.kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
            }
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                destroy_record_type(tree->tree_data.type_decl_data.info.record);
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                clear_type_alias_fields(alias);
            }
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_GENERIC)
            {
                struct GenericDecl *generic = &tree->tree_data.type_decl_data.info.generic;
                if (generic->type_parameters != NULL)
                {
                    for (int i = 0; i < generic->num_type_params; ++i)
                        free(generic->type_parameters[i]);
                    free(generic->type_parameters);
                    generic->type_parameters = NULL;
                }
                generic->num_type_params = 0;
                if (generic->original_ast != NULL)
                {
                    free_ast(generic->original_ast);
                    generic->original_ast = NULL;
                }
                if (generic->record_template != NULL)
                {
                    destroy_record_type(generic->record_template);
                    generic->record_template = NULL;
                }
            }
            break;

        default:
          fprintf(stderr, "BAD TYPE IN destroy_tree!\n");
          exit(1);
    }
    free(tree);
}

void destroy_stmt(struct Statement *stmt)
{
    if (stmt == NULL)
        return;

    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
          if (stmt->stmt_data.var_assign_data.var != NULL)
              destroy_expr(stmt->stmt_data.var_assign_data.var);
          if (stmt->stmt_data.var_assign_data.expr != NULL)
              destroy_expr(stmt->stmt_data.var_assign_data.expr);
          break;

        case STMT_PROCEDURE_CALL:
          free(stmt->stmt_data.procedure_call_data.id);
          if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
            free(stmt->stmt_data.procedure_call_data.mangled_id);
          destroy_list(stmt->stmt_data.procedure_call_data.expr_args);
          if (stmt->stmt_data.procedure_call_data.procedural_var_expr != NULL)
          {
              destroy_expr(stmt->stmt_data.procedure_call_data.procedural_var_expr);
              stmt->stmt_data.procedure_call_data.procedural_var_expr = NULL;
          }
          if (stmt->stmt_data.procedure_call_data.call_kgpc_type != NULL)
          {
              destroy_kgpc_type(stmt->stmt_data.procedure_call_data.call_kgpc_type);
              stmt->stmt_data.procedure_call_data.call_kgpc_type = NULL;
          }
          break;

        case STMT_EXPR:
          if (stmt->stmt_data.expr_stmt_data.expr != NULL)
              destroy_expr(stmt->stmt_data.expr_stmt_data.expr);
          break;

        case STMT_COMPOUND_STATEMENT:
          destroy_list(stmt->stmt_data.compound_statement);
          break;

        case STMT_LABEL:
          free(stmt->stmt_data.label_data.label);
          destroy_stmt(stmt->stmt_data.label_data.stmt);
          break;

        case STMT_GOTO:
          free(stmt->stmt_data.goto_data.label);
          break;

        case STMT_IF_THEN:
          destroy_expr(stmt->stmt_data.if_then_data.relop_expr);
          destroy_stmt(stmt->stmt_data.if_then_data.if_stmt);

          if(stmt->stmt_data.if_then_data.else_stmt != NULL)
          {
              destroy_stmt(stmt->stmt_data.if_then_data.else_stmt);
          }
          break;

        case STMT_WHILE:
          destroy_expr(stmt->stmt_data.while_data.relop_expr);
          destroy_stmt(stmt->stmt_data.while_data.while_stmt);
          break;

        case STMT_REPEAT:
          destroy_list(stmt->stmt_data.repeat_data.body_list);
          destroy_expr(stmt->stmt_data.repeat_data.until_expr);
          break;

        case STMT_FOR:
          switch(stmt->stmt_data.for_data.for_assign_type)
          {
              case STMT_FOR_VAR:
                destroy_expr(stmt->stmt_data.for_data.for_assign_data.var);
                break;
              case STMT_FOR_ASSIGN_VAR:
                destroy_stmt(stmt->stmt_data.for_data.for_assign_data.var_assign);
                break;
              default:
                fprintf(stderr, "BAD TYPE IN STMT_FOR (destroy_stmt)!\n");
                exit(1);
          }

          destroy_expr(stmt->stmt_data.for_data.to);
          destroy_stmt(stmt->stmt_data.for_data.do_for);
          break;

        case STMT_FOR_IN:
          destroy_expr(stmt->stmt_data.for_in_data.loop_var);
          destroy_expr(stmt->stmt_data.for_in_data.collection);
          destroy_stmt(stmt->stmt_data.for_in_data.do_stmt);
          break;

        case STMT_ASM_BLOCK:
          free(stmt->stmt_data.asm_block_data.code);
          break;

        case STMT_EXIT:
          /* Free the optional return expression */
          if (stmt->stmt_data.exit_data.return_expr != NULL)
            destroy_expr(stmt->stmt_data.exit_data.return_expr);
          break;
        case STMT_BREAK:
        case STMT_CONTINUE:
          /* No data to free for simple control flow statements */
          break;

        case STMT_CASE:
          destroy_expr(stmt->stmt_data.case_data.selector_expr);
          /* Destroy case branches */
          {
              ListNode_t *branch_node = stmt->stmt_data.case_data.branches;
              while (branch_node != NULL) {
                  struct CaseBranch *branch = (struct CaseBranch *)branch_node->cur;
                  if (branch != NULL) {
                      destroy_list(branch->labels);
                      destroy_stmt(branch->stmt);
                      free(branch);
                  }
                  branch_node = branch_node->next;
              }
              destroy_list(stmt->stmt_data.case_data.branches);
          }
          if (stmt->stmt_data.case_data.else_stmt != NULL)
              destroy_stmt(stmt->stmt_data.case_data.else_stmt);
          break;

        case STMT_WITH:
          destroy_expr(stmt->stmt_data.with_data.context_expr);
          destroy_stmt(stmt->stmt_data.with_data.body_stmt);
          break;

        case STMT_TRY_FINALLY:
          destroy_list(stmt->stmt_data.try_finally_data.try_statements);
          destroy_list(stmt->stmt_data.try_finally_data.finally_statements);
          break;

        case STMT_TRY_EXCEPT:
          destroy_list(stmt->stmt_data.try_except_data.try_statements);
          destroy_list(stmt->stmt_data.try_except_data.except_statements);
          if (stmt->stmt_data.try_except_data.exception_var_name != NULL)
              free(stmt->stmt_data.try_except_data.exception_var_name);
          if (stmt->stmt_data.try_except_data.exception_type_name != NULL)
              free(stmt->stmt_data.try_except_data.exception_type_name);
          break;

        case STMT_RAISE:
          if (stmt->stmt_data.raise_data.exception_expr != NULL)
              destroy_expr(stmt->stmt_data.raise_data.exception_expr);
          break;

        case STMT_INHERITED:
          if (stmt->stmt_data.inherited_data.call_expr != NULL)
              destroy_expr(stmt->stmt_data.inherited_data.call_expr);
          break;

          default:
            fprintf(stderr, "BAD TYPE IN stmt_print!\n");
            exit(1);
    }
    free(stmt);
}

void destroy_expr(struct Expression *expr)
{
    assert(expr != NULL);
    if (expr->field_width != NULL)
    {
        destroy_expr(expr->field_width);
        expr->field_width = NULL;
    }
    if (expr->field_precision != NULL)
    {
        destroy_expr(expr->field_precision);
        expr->field_precision = NULL;
    }
    switch(expr->type)
    {
        case EXPR_RELOP:
          if (expr->expr_data.relop_data.left != NULL)
              destroy_expr(expr->expr_data.relop_data.left);
          if (expr->expr_data.relop_data.right != NULL)
              destroy_expr(expr->expr_data.relop_data.right);
          break;

        case EXPR_SIGN_TERM:
          destroy_expr(expr->expr_data.sign_term);
          break;

        case EXPR_ADDOP:
          destroy_expr(expr->expr_data.addop_data.left_expr);
          destroy_expr(expr->expr_data.addop_data.right_term);
          break;

        case EXPR_MULOP:
          destroy_expr(expr->expr_data.mulop_data.left_term);
          destroy_expr(expr->expr_data.mulop_data.right_factor);
          break;

        case EXPR_VAR_ID:
          free(expr->expr_data.id);
          break;

        case EXPR_ARRAY_ACCESS:
          if (expr->expr_data.array_access_data.array_expr != NULL)
              destroy_expr(expr->expr_data.array_access_data.array_expr);
          if (expr->expr_data.array_access_data.index_expr != NULL)
              destroy_expr(expr->expr_data.array_access_data.index_expr);
          /* Free extra indices for multi-dimensional arrays */
          if (expr->expr_data.array_access_data.extra_indices != NULL)
          {
              ListNode_t *idx = expr->expr_data.array_access_data.extra_indices;
              while (idx != NULL)
              {
                  ListNode_t *next = idx->next;
                  if (idx->cur != NULL)
                      destroy_expr((struct Expression *)idx->cur);
                  free(idx);
                  idx = next;
              }
          }
          if (expr->expr_data.array_access_data.linear_strides != NULL)
          {
              free(expr->expr_data.array_access_data.linear_strides);
              expr->expr_data.array_access_data.linear_strides = NULL;
          }
          if (expr->expr_data.array_access_data.linear_lowers != NULL)
          {
              free(expr->expr_data.array_access_data.linear_lowers);
              expr->expr_data.array_access_data.linear_lowers = NULL;
          }
          expr->expr_data.array_access_data.linear_index_count = 0;
          expr->expr_data.array_access_data.linear_info_valid = 0;
          break;

        case EXPR_RECORD_ACCESS:
          if (expr->expr_data.record_access_data.record_expr != NULL)
              destroy_expr(expr->expr_data.record_access_data.record_expr);
          free(expr->expr_data.record_access_data.field_id);
          break;

        case EXPR_FUNCTION_CALL:
          free(expr->expr_data.function_call_data.id);
          if (expr->expr_data.function_call_data.mangled_id != NULL)
              free(expr->expr_data.function_call_data.mangled_id);
          destroy_list(expr->expr_data.function_call_data.args_expr);
          if (expr->expr_data.function_call_data.procedural_var_expr != NULL)
          {
              destroy_expr(expr->expr_data.function_call_data.procedural_var_expr);
              expr->expr_data.function_call_data.procedural_var_expr = NULL;
          }
          if (expr->expr_data.function_call_data.call_kgpc_type != NULL)
          {
              destroy_kgpc_type(expr->expr_data.function_call_data.call_kgpc_type);
              expr->expr_data.function_call_data.call_kgpc_type = NULL;
          }
          break;

        case EXPR_INUM:
          break;

        case EXPR_RNUM:
          break;

        case EXPR_STRING:
          free(expr->expr_data.string);
          break;
        case EXPR_CHAR_CODE:
          break;

        case EXPR_BOOL:
          break;

        case EXPR_NIL:
          break;

        case EXPR_SET:
          destroy_list(expr->expr_data.set_data.elements);
          expr->expr_data.set_data.elements = NULL;
          break;
        case EXPR_ARRAY_LITERAL:
        {
          ListNode_t *cur = expr->expr_data.array_literal_data.elements;
          while (cur != NULL)
          {
            if (cur->cur != NULL)
              destroy_expr((struct Expression *)cur->cur);
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
          }
          expr->expr_data.array_literal_data.elements = NULL;
          break;
        }

        case EXPR_RECORD_CONSTRUCTOR:
        {
          ListNode_t *cur = expr->expr_data.record_constructor_data.fields;
          while (cur != NULL)
          {
            struct RecordConstructorField *field = (struct RecordConstructorField *)cur->cur;
            if (field != NULL)
            {
                if (field->value != NULL)
                    destroy_expr(field->value);
                free(field->field_id);
                free(field->field_type_id);
                free(field->array_element_type_id);
                free(field);
            }
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
          }
          expr->expr_data.record_constructor_data.fields = NULL;
          break;
        }

        case EXPR_POINTER_DEREF:
          if (expr->expr_data.pointer_deref_data.pointer_expr != NULL)
          {
              destroy_expr(expr->expr_data.pointer_deref_data.pointer_expr);
              expr->expr_data.pointer_deref_data.pointer_expr = NULL;
          }
          break;

        case EXPR_ADDR:
          if (expr->expr_data.addr_data.expr != NULL)
          {
              destroy_expr(expr->expr_data.addr_data.expr);
              expr->expr_data.addr_data.expr = NULL;
          }
          break;

        case EXPR_TYPECAST:
          if (expr->expr_data.typecast_data.target_type_id != NULL)
          {
              free(expr->expr_data.typecast_data.target_type_id);
              expr->expr_data.typecast_data.target_type_id = NULL;
          }
          if (expr->expr_data.typecast_data.expr != NULL)
          {
              destroy_expr(expr->expr_data.typecast_data.expr);
              expr->expr_data.typecast_data.expr = NULL;
          }
          break;
        case EXPR_IS:
          if (expr->expr_data.is_data.expr != NULL)
          {
              destroy_expr(expr->expr_data.is_data.expr);
              expr->expr_data.is_data.expr = NULL;
          }
          if (expr->expr_data.is_data.target_type_id != NULL)
          {
              free(expr->expr_data.is_data.target_type_id);
              expr->expr_data.is_data.target_type_id = NULL;
          }
          break;
        case EXPR_AS:
          if (expr->expr_data.as_data.expr != NULL)
          {
              destroy_expr(expr->expr_data.as_data.expr);
              expr->expr_data.as_data.expr = NULL;
          }
          if (expr->expr_data.as_data.target_type_id != NULL)
          {
              free(expr->expr_data.as_data.target_type_id);
              expr->expr_data.as_data.target_type_id = NULL;
          }
          break;

        case EXPR_ADDR_OF_PROC:
          /* Nothing to free - procedure_symbol is a reference, not owned */
          break;

        case EXPR_ANONYMOUS_FUNCTION:
        case EXPR_ANONYMOUS_PROCEDURE:
          if (expr->expr_data.anonymous_method_data.generated_name != NULL)
          {
              free(expr->expr_data.anonymous_method_data.generated_name);
              expr->expr_data.anonymous_method_data.generated_name = NULL;
          }
          if (expr->expr_data.anonymous_method_data.parameters != NULL)
          {
              destroy_list(expr->expr_data.anonymous_method_data.parameters);
              expr->expr_data.anonymous_method_data.parameters = NULL;
          }
          if (expr->expr_data.anonymous_method_data.return_type_id != NULL)
          {
              free(expr->expr_data.anonymous_method_data.return_type_id);
              expr->expr_data.anonymous_method_data.return_type_id = NULL;
          }
          if (expr->expr_data.anonymous_method_data.body != NULL)
          {
              destroy_stmt(expr->expr_data.anonymous_method_data.body);
              expr->expr_data.anonymous_method_data.body = NULL;
          }
          break;

        default:
          fprintf(stderr, "BAD TYPE IN destroy_expr!\n");
          exit(1);
    }
    if (expr->pointer_subtype_id != NULL)
    {
        free(expr->pointer_subtype_id);
        expr->pointer_subtype_id = NULL;
    }
    if (expr->array_element_type_id != NULL)
    {
        free(expr->array_element_type_id);
        expr->array_element_type_id = NULL;
    }
    if (expr->resolved_kgpc_type != NULL)
    {
        destroy_kgpc_type(expr->resolved_kgpc_type);
        expr->resolved_kgpc_type = NULL;
    }
    free(expr);
}

void destroy_record_type(struct RecordType *record_type)
{
    if (record_type == NULL)
        return;

    destroy_list(record_type->fields);
    if (record_type->properties != NULL)
    {
        ListNode_t *cur = record_type->properties;
        while (cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)cur->cur;
            destroy_class_property(property);
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
        }
    }
    free(record_type->parent_class_name);
    free(record_type->helper_base_type_id);
    free(record_type->helper_parent_id);
    free(record_type->type_id);
    
    /* Free methods list */
    if (record_type->methods != NULL) {
        ListNode_t *cur = record_type->methods;
        while (cur != NULL) {
            struct MethodInfo *method = (struct MethodInfo *)cur->cur;
            if (method != NULL) {
                free(method->name);
                free(method->mangled_name);
                free(method);
            }
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
        }
    }
    destroy_list(record_type->method_templates);
    if (record_type->generic_args != NULL)
    {
        for (int i = 0; i < record_type->num_generic_args; ++i)
            free(record_type->generic_args[i]);
        free(record_type->generic_args);
    }
    record_type->generic_args = NULL;
    record_type->num_generic_args = 0;
    record_type->generic_decl = NULL;
    free(record_type->default_indexed_property);
    free(record_type->default_indexed_element_type_id);
    if (record_type->record_properties != NULL)
    {
        ListNode_t *cur = record_type->record_properties;
        while (cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)cur->cur;
            destroy_class_property(property);
            ListNode_t *next = cur->next;
            free(cur);
            cur = next;
        }
    }

    free(record_type);
}

struct RecordType *clone_record_type(const struct RecordType *record_type)
{
    if (record_type == NULL)
        return NULL;

    struct RecordType *clone = (struct RecordType *)malloc(sizeof(struct RecordType));
    assert(clone != NULL);
    clone->fields = NULL;
    clone->properties = NULL;
    clone->parent_class_name = record_type->parent_class_name ? strdup(record_type->parent_class_name) : NULL;
    clone->methods = NULL;  /* Methods list copied during semantic checking if needed */
    clone->method_templates = clone_method_template_list(record_type->method_templates);
    clone->is_class = record_type->is_class;
    clone->is_interface = record_type->is_interface;
    clone->is_type_helper = record_type->is_type_helper;
    clone->helper_base_type_id = record_type->helper_base_type_id ?
        strdup(record_type->helper_base_type_id) : NULL;
    clone->helper_parent_id = record_type->helper_parent_id ?
        strdup(record_type->helper_parent_id) : NULL;
    clone->type_id = record_type->type_id ? strdup(record_type->type_id) : NULL;
    clone->has_cached_size = record_type->has_cached_size;
    clone->cached_size = record_type->cached_size;
    clone->generic_decl = record_type->generic_decl;
    clone->num_generic_args = record_type->num_generic_args;
    clone->method_clones_emitted = 0;
    clone->generic_args = NULL;
    if (record_type->generic_args != NULL && record_type->num_generic_args > 0)
    {
        clone->generic_args = (char **)calloc((size_t)record_type->num_generic_args, sizeof(char *));
        if (clone->generic_args != NULL)
        {
            for (int i = 0; i < record_type->num_generic_args; ++i)
                clone->generic_args[i] = record_type->generic_args[i] != NULL ?
                    strdup(record_type->generic_args[i]) : NULL;
        }
    }

    clone->default_indexed_property = record_type->default_indexed_property ?
        strdup(record_type->default_indexed_property) : NULL;
    clone->default_indexed_element_type = record_type->default_indexed_element_type;
    clone->default_indexed_element_type_id = record_type->default_indexed_element_type_id ?
        strdup(record_type->default_indexed_element_type_id) : NULL;

    clone->fields = clone_member_list(record_type->fields);
    clone->properties = clone_property_list(record_type->properties);
    clone->record_properties = clone_property_list(record_type->record_properties);

    clone->guid_string = record_type->guid_string ? strdup(record_type->guid_string) : NULL;
    clone->num_interfaces = record_type->num_interfaces;
    clone->interface_names = NULL;
    if (record_type->interface_names != NULL && record_type->num_interfaces > 0) {
        clone->interface_names = (char **)calloc((size_t)record_type->num_interfaces, sizeof(char *));
        if (clone->interface_names != NULL) {
            for (int i = 0; i < record_type->num_interfaces; ++i)
                clone->interface_names[i] = record_type->interface_names[i] != NULL ?
                    strdup(record_type->interface_names[i]) : NULL;
        }
    }

    return clone;
}

static struct RecordField *clone_record_field(const struct RecordField *field)
{
    if (field == NULL)
        return NULL;

    struct RecordField *clone = (struct RecordField *)calloc(1, sizeof(struct RecordField));
    assert(clone != NULL);
    clone->name = field->name != NULL ? strdup(field->name) : NULL;
    clone->type = field->type;
    clone->type_id = field->type_id != NULL ? strdup(field->type_id) : NULL;
    clone->nested_record = clone_record_type(field->nested_record);
    clone->proc_type = field->proc_type;
    if (clone->proc_type != NULL)
        kgpc_type_retain(clone->proc_type);
    clone->is_array = field->is_array;
    clone->array_start = field->array_start;
    clone->array_end = field->array_end;
    clone->array_element_type = field->array_element_type;
    clone->array_element_type_id = field->array_element_type_id != NULL ?
        strdup(field->array_element_type_id) : NULL;
    clone->array_element_record = clone_record_type(field->array_element_record);
    clone->array_is_open = field->array_is_open;
    clone->is_hidden = field->is_hidden;
    clone->is_class_var = field->is_class_var;
    clone->is_pointer = field->is_pointer;
    clone->pointer_type = field->pointer_type;
    clone->pointer_type_id = field->pointer_type_id != NULL ?
        strdup(field->pointer_type_id) : NULL;
    clone->enum_literals = NULL; /* enum_literals are not cloned - they're registered at declaration */
    return clone;
}

static ListNode_t *clone_member_list(const ListNode_t *members)
{
    if (members == NULL)
        return NULL;

    ListNode_t *head = NULL;
    ListNode_t **tail = &head;
    const ListNode_t *cur = members;
    struct RecordField *last_orig_field = NULL;
    struct RecordField *last_clone_field = NULL;

    while (cur != NULL)
    {
        switch (cur->type)
        {
            case LIST_RECORD_FIELD:
            {
                struct RecordField *field = (struct RecordField *)cur->cur;
                struct RecordField *field_clone = clone_record_field(field);
                last_orig_field = field;
                last_clone_field = field_clone;
                ListNode_t *node = CreateListNode(field_clone, LIST_RECORD_FIELD);
                *tail = node;
                tail = &node->next;
                break;
            }
            case LIST_VARIANT_PART:
            {
                struct VariantPart *variant = (struct VariantPart *)cur->cur;
                struct VariantPart *variant_clone = clone_variant_part_internal(variant);
                if (variant_clone != NULL && variant->tag_field != NULL)
                {
                    if (variant->tag_field == last_orig_field)
                        variant_clone->tag_field = last_clone_field;
                    else
                        variant_clone->tag_field = NULL;
                }
                ListNode_t *node = CreateListNode(variant_clone, LIST_VARIANT_PART);
                *tail = node;
                tail = &node->next;
                last_orig_field = NULL;
                last_clone_field = NULL;
                break;
            }
            default:
                /* Unsupported type in member list */
                break;
        }
        cur = cur->next;
    }

    return head;
}

static struct VariantBranch *clone_variant_branch_internal(const struct VariantBranch *branch)
{
    if (branch == NULL)
        return NULL;

    struct VariantBranch *clone = (struct VariantBranch *)calloc(1, sizeof(struct VariantBranch));
    assert(clone != NULL);

    clone->labels = NULL; /* Labels are not deep-cloned to avoid duplication */
    clone->members = clone_member_list(branch->members);
    return clone;
}

static struct VariantPart *clone_variant_part_internal(const struct VariantPart *variant)
{
    if (variant == NULL)
        return NULL;

    struct VariantPart *clone = (struct VariantPart *)calloc(1, sizeof(struct VariantPart));
    assert(clone != NULL);

    clone->tag_field = NULL;
    clone->tag_type = variant->tag_type;
    clone->tag_type_id = variant->tag_type_id != NULL ? strdup(variant->tag_type_id) : NULL;
    clone->tag_record = clone_record_type(variant->tag_record);
    clone->has_cached_size = variant->has_cached_size;
    clone->cached_size = variant->cached_size;

    ListNode_t *head = NULL;
    ListNode_t **tail = &head;
    ListNode_t *cur = variant->branches;
    while (cur != NULL)
    {
        if (cur->type == LIST_VARIANT_BRANCH)
        {
            struct VariantBranch *branch_clone = clone_variant_branch_internal((struct VariantBranch *)cur->cur);
            ListNode_t *node = CreateListNode(branch_clone, LIST_VARIANT_BRANCH);
            *tail = node;
            tail = &node->next;
        }
        cur = cur->next;
    }
    clone->branches = head;
    return clone;
}

Tree_t *mk_program(int line_num, char *id, ListNode_t *args, ListNode_t *uses,
    ListNode_t *labels, ListNode_t *const_decl, ListNode_t *var_decl, ListNode_t *type_decl,
    ListNode_t *subprograms, struct Statement *compound_statement)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_PROGRAM_TYPE;
    new_tree->tree_data.program_data.program_id = id;
    new_tree->tree_data.program_data.args_char = args;
    new_tree->tree_data.program_data.uses_units = uses;
    new_tree->tree_data.program_data.label_declaration = labels;
    new_tree->tree_data.program_data.const_declaration = const_decl;
    new_tree->tree_data.program_data.var_declaration = var_decl;
    new_tree->tree_data.program_data.type_declaration = type_decl;
    new_tree->tree_data.program_data.subprograms = subprograms;
    new_tree->tree_data.program_data.subprograms = subprograms;
    new_tree->tree_data.program_data.body_statement = compound_statement;
    if (getenv("KGPC_DEBUG_BODY") != NULL) {
        fprintf(stderr, "[KGPC] mk_program: body_statement=%p\n", compound_statement);
    }
    new_tree->tree_data.program_data.finalization_statements = NULL;

    return new_tree;
}

Tree_t *mk_unit(int line_num, char *id, ListNode_t *interface_uses,
    ListNode_t *interface_const_decls, ListNode_t *interface_type_decls,
    ListNode_t *interface_var_decls, ListNode_t *implementation_uses,
    ListNode_t *implementation_const_decls,
    ListNode_t *implementation_type_decls,
    ListNode_t *implementation_var_decls, ListNode_t *subprograms,
    struct Statement *initialization, struct Statement *finalization)
{
    Tree_t *new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_UNIT;
    new_tree->tree_data.unit_data.unit_id = id;
    new_tree->tree_data.unit_data.interface_uses = interface_uses;
    new_tree->tree_data.unit_data.interface_const_decls = interface_const_decls;
    new_tree->tree_data.unit_data.interface_type_decls = interface_type_decls;
    new_tree->tree_data.unit_data.interface_var_decls = interface_var_decls;
    new_tree->tree_data.unit_data.implementation_uses = implementation_uses;
    new_tree->tree_data.unit_data.implementation_const_decls = implementation_const_decls;
    new_tree->tree_data.unit_data.implementation_type_decls = implementation_type_decls;
    new_tree->tree_data.unit_data.implementation_var_decls = implementation_var_decls;
    new_tree->tree_data.unit_data.subprograms = subprograms;
    new_tree->tree_data.unit_data.initialization = initialization;
    new_tree->tree_data.unit_data.finalization = finalization;

    return new_tree;
}

Tree_t *mk_typedecl(int line_num, char *id, int start, int end)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_TYPE_DECL;
    new_tree->tree_data.type_decl_data.id = id;
    new_tree->tree_data.type_decl_data.kind = TYPE_DECL_RANGE;
    new_tree->tree_data.type_decl_data.kgpc_type = NULL;
    new_tree->tree_data.type_decl_data.info.range.start = start;
    new_tree->tree_data.type_decl_data.info.range.end = end;
    new_tree->tree_data.type_decl_data.defined_in_unit = 0;
    new_tree->tree_data.type_decl_data.unit_is_public = 0;

    return new_tree;
}


Tree_t *mk_record_type(int line_num, char *id, struct RecordType *record_type)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_TYPE_DECL;
    new_tree->tree_data.type_decl_data.id = id;
    new_tree->tree_data.type_decl_data.kind = TYPE_DECL_RECORD;
    new_tree->tree_data.type_decl_data.kgpc_type = NULL;
    new_tree->tree_data.type_decl_data.info.record = record_type;
    new_tree->tree_data.type_decl_data.defined_in_unit = 0;
    new_tree->tree_data.type_decl_data.unit_is_public = 0;

    return new_tree;
}


Tree_t *mk_procedure(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *label_decl, ListNode_t *type_decl, ListNode_t *var_decl,
    ListNode_t *subprograms, struct Statement *compound_statement,
    int cname_flag, int overload_flag)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_SUBPROGRAM;
    new_tree->tree_data.subprogram_data.sub_type = TREE_SUBPROGRAM_PROC;
    new_tree->tree_data.subprogram_data.id = id;
    new_tree->tree_data.subprogram_data.mangled_id = NULL;
    new_tree->tree_data.subprogram_data.args_var = args;
    new_tree->tree_data.subprogram_data.const_declarations = const_decl;
    new_tree->tree_data.subprogram_data.label_declarations = label_decl;
    new_tree->tree_data.subprogram_data.type_declarations = type_decl;
    new_tree->tree_data.subprogram_data.return_type = -1;
    new_tree->tree_data.subprogram_data.return_type_id = NULL;
    new_tree->tree_data.subprogram_data.inline_return_type = NULL;
    new_tree->tree_data.subprogram_data.cname_flag = cname_flag;
    new_tree->tree_data.subprogram_data.cname_override = NULL;
    new_tree->tree_data.subprogram_data.overload_flag = overload_flag;
    new_tree->tree_data.subprogram_data.nesting_level = 0;
    new_tree->tree_data.subprogram_data.requires_static_link = 0;
    new_tree->tree_data.subprogram_data.defined_in_unit = 0;
    new_tree->tree_data.subprogram_data.has_nested_requiring_link = 0;
    new_tree->tree_data.subprogram_data.unit_is_public = 0;
    new_tree->tree_data.subprogram_data.declarations = var_decl;
    new_tree->tree_data.subprogram_data.subprograms = subprograms;
    new_tree->tree_data.subprogram_data.statement_list = compound_statement;
    new_tree->tree_data.subprogram_data.is_used = 0; /* Mark_used will set reachable routines */

    return new_tree;
}

Tree_t *mk_function(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *label_decl, ListNode_t *type_decl, ListNode_t *var_decl,
    ListNode_t *subprograms, struct Statement *compound_statement,
    int return_type, char *return_type_id, struct TypeAlias *inline_return_type, int cname_flag, int overload_flag)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_SUBPROGRAM;
    new_tree->tree_data.subprogram_data.sub_type = TREE_SUBPROGRAM_FUNC;
    new_tree->tree_data.subprogram_data.id = id;
    new_tree->tree_data.subprogram_data.mangled_id = NULL;
    new_tree->tree_data.subprogram_data.args_var = args;
    new_tree->tree_data.subprogram_data.const_declarations = const_decl;
    new_tree->tree_data.subprogram_data.label_declarations = label_decl;
    new_tree->tree_data.subprogram_data.type_declarations = type_decl;
    new_tree->tree_data.subprogram_data.return_type = return_type;
    new_tree->tree_data.subprogram_data.return_type_id = return_type_id;
    new_tree->tree_data.subprogram_data.inline_return_type = inline_return_type;
    new_tree->tree_data.subprogram_data.cname_flag = cname_flag;
    new_tree->tree_data.subprogram_data.cname_override = NULL;
    new_tree->tree_data.subprogram_data.overload_flag = overload_flag;
    new_tree->tree_data.subprogram_data.nesting_level = 0;
    new_tree->tree_data.subprogram_data.requires_static_link = 0;
    new_tree->tree_data.subprogram_data.has_nested_requiring_link = 0;
    new_tree->tree_data.subprogram_data.defined_in_unit = 0;
    new_tree->tree_data.subprogram_data.unit_is_public = 0;
    new_tree->tree_data.subprogram_data.declarations = var_decl;
    new_tree->tree_data.subprogram_data.subprograms = subprograms;
    new_tree->tree_data.subprogram_data.statement_list = compound_statement;
    new_tree->tree_data.subprogram_data.is_used = 0; /* Mark_used will set reachable routines */

    return new_tree;
}

/*enum TreeType{TREE_PROGRAM_TYPE, TREE_SUBPROGRAM, TREE_VAR_DECL, TREE_STATEMENT_TYPE};*/

Tree_t *mk_vardecl(int line_num, ListNode_t *ids, int type, char *type_id,
    int is_var_param, int inferred_type, struct Statement *initializer,
    struct RecordType *inline_record_type, struct TypeAlias *inline_type_alias,
    char *absolute_target)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_VAR_DECL;
    new_tree->tree_data.var_decl_data.ids = ids;
    new_tree->tree_data.var_decl_data.type = type;
    new_tree->tree_data.var_decl_data.type_id = type_id;
    new_tree->tree_data.var_decl_data.is_var_param = is_var_param;
    new_tree->tree_data.var_decl_data.is_untyped_param = 0;
    new_tree->tree_data.var_decl_data.inferred_type = inferred_type;
    new_tree->tree_data.var_decl_data.initializer = initializer;
    new_tree->tree_data.var_decl_data.is_typed_const = 0;
    new_tree->tree_data.var_decl_data.currency_scaled = 0;
    new_tree->tree_data.var_decl_data.inline_record_type = inline_record_type;
    new_tree->tree_data.var_decl_data.inline_type_alias = inline_type_alias;
    new_tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
    new_tree->tree_data.var_decl_data.defined_in_unit = 0;
    new_tree->tree_data.var_decl_data.unit_is_public = 0;
    new_tree->tree_data.var_decl_data.cname_override = NULL;
    new_tree->tree_data.var_decl_data.is_external = 0;
    new_tree->tree_data.var_decl_data.absolute_target = absolute_target;

    return new_tree;
}

Tree_t *mk_typealiasdecl(int line_num, char *id, int is_array, int actual_type, char *type_id, int start, int end)
{
    Tree_t *new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_TYPE_DECL;
    new_tree->tree_data.type_decl_data.id = id;
    new_tree->tree_data.type_decl_data.kind = TYPE_DECL_ALIAS;
    new_tree->tree_data.type_decl_data.kgpc_type = NULL;

    struct TypeAlias *alias = &new_tree->tree_data.type_decl_data.info.alias;
    alias->base_type = is_array ? UNKNOWN_TYPE : actual_type;
    alias->is_char_alias = 0;
    alias->target_type_id = NULL;
    alias->inline_record_type = NULL;
    alias->is_array = is_array;
    alias->array_start = start;
    alias->array_end = end;
    alias->array_element_type = UNKNOWN_TYPE;
    alias->array_element_type_id = NULL;
    alias->is_shortstring = 0;
    alias->is_open_array = (alias->is_array && end < start);
    alias->array_dimensions = NULL;
    alias->is_pointer = 0;
    alias->pointer_type = UNKNOWN_TYPE;
    alias->pointer_type_id = NULL;
    alias->is_set = 0;
    alias->set_element_type = UNKNOWN_TYPE;
    alias->set_element_type_id = NULL;
    alias->is_enum_set = 0;
    alias->inline_enum_values = NULL;
    alias->is_enum = 0;
    alias->enum_literals = NULL;
    alias->is_file = 0;
    alias->file_type = UNKNOWN_TYPE;
    alias->kgpc_type = NULL;  /* Initialize shared KgpcType for enums/sets */
    alias->file_type_id = NULL;
    alias->is_range = 0;
    alias->range_known = 0;
    alias->range_start = 0;
    alias->range_end = 0;
    alias->storage_size = 0;
    new_tree->tree_data.type_decl_data.defined_in_unit = 0;
    new_tree->tree_data.type_decl_data.unit_is_public = 0;

    if (alias->is_array)
    {
        alias->array_element_type = actual_type;
        if (actual_type == UNKNOWN_TYPE && type_id != NULL)
            alias->array_element_type_id = type_id;
        else if (type_id != NULL)
            free(type_id);
        else
            alias->array_element_type_id = NULL;
    }
    else
    {
        alias->base_type = actual_type;
        if (type_id != NULL)
            alias->target_type_id = type_id;
        else
            alias->target_type_id = NULL;
    }

    return new_tree;
}

Tree_t *mk_arraydecl(int line_num, ListNode_t *ids, int type, char *type_id, int start, int end,
    char *range_str, struct Statement *initializer)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_ARR_DECL;
    new_tree->tree_data.arr_decl_data.ids = ids;
    new_tree->tree_data.arr_decl_data.type = type;
    new_tree->tree_data.arr_decl_data.type_id = type_id;
    new_tree->tree_data.arr_decl_data.s_range = start;
    new_tree->tree_data.arr_decl_data.e_range = end;
    new_tree->tree_data.arr_decl_data.range_str = range_str;
    new_tree->tree_data.arr_decl_data.initializer = initializer;
    new_tree->tree_data.arr_decl_data.is_typed_const = 0;
    new_tree->tree_data.arr_decl_data.is_shortstring = 0;
    new_tree->tree_data.arr_decl_data.has_static_storage = 0;
    new_tree->tree_data.arr_decl_data.static_storage_emitted = 0;
    new_tree->tree_data.arr_decl_data.static_label = NULL;
    new_tree->tree_data.arr_decl_data.init_guard_label = NULL;
    new_tree->tree_data.arr_decl_data.defined_in_unit = 0;
    new_tree->tree_data.arr_decl_data.unit_is_public = 0;

    return new_tree;
}

Tree_t *mk_constdecl(int line_num, char *id, char *type_id, struct Expression *value)
{
    Tree_t *new_tree = (Tree_t *)calloc(1, sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_CONST_DECL;
    new_tree->tree_data.const_decl_data.id = id;
    new_tree->tree_data.const_decl_data.type_id = type_id;
    new_tree->tree_data.const_decl_data.value = value;
    new_tree->tree_data.const_decl_data.defined_in_unit = 0;
    new_tree->tree_data.const_decl_data.unit_is_public = 0;

    return new_tree;
}


/************** Statement routines **************/
struct Statement *mk_varassign(int line_num, int col_num, struct Expression *var, struct Expression *expr)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = col_num;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_VAR_ASSIGN;
    new_stmt->stmt_data.var_assign_data.var = var;
    new_stmt->stmt_data.var_assign_data.expr = expr;

    return new_stmt;
}

struct Statement *mk_label(int line_num, char *label, struct Statement *stmt)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_LABEL;
    new_stmt->stmt_data.label_data.label = label;
    new_stmt->stmt_data.label_data.stmt = stmt;

    return new_stmt;
}

struct Statement *mk_goto(int line_num, char *label)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_GOTO;
    new_stmt->stmt_data.goto_data.label = label;

    return new_stmt;
}

struct Statement *mk_break(int line_num)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_BREAK;
    memset(&new_stmt->stmt_data, 0, sizeof(new_stmt->stmt_data));

    return new_stmt;
}

struct Statement *mk_continue(int line_num)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_CONTINUE;
    memset(&new_stmt->stmt_data, 0, sizeof(new_stmt->stmt_data));

    return new_stmt;
}


struct Statement *mk_exit(int line_num)
{
    return mk_exit_with_value(line_num, NULL);
}

struct Statement *mk_exit_with_value(int line_num, struct Expression *return_expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_EXIT;
    memset(&new_stmt->stmt_data, 0, sizeof(new_stmt->stmt_data));
    new_stmt->stmt_data.exit_data.return_expr = return_expr;

    return new_stmt;
}

struct Statement *mk_procedurecall(int line_num, char *id, ListNode_t *expr_args)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_PROCEDURE_CALL;
    new_stmt->stmt_data.procedure_call_data.id = id;
    new_stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    new_stmt->stmt_data.procedure_call_data.expr_args = expr_args;
    new_stmt->stmt_data.procedure_call_data.resolved_proc = NULL;
    new_stmt->stmt_data.procedure_call_data.is_call_info_valid = 0;
    new_stmt->stmt_data.procedure_call_data.call_hash_type = HASHTYPE_VAR;  /* Default, will be overwritten */
    new_stmt->stmt_data.procedure_call_data.call_kgpc_type = NULL;
    new_stmt->stmt_data.procedure_call_data.is_procedural_var_call = 0;
    new_stmt->stmt_data.procedure_call_data.procedural_var_symbol = NULL;
    new_stmt->stmt_data.procedure_call_data.procedural_var_expr = NULL;
    new_stmt->stmt_data.procedure_call_data.is_method_call_placeholder = 0;

    return new_stmt;
}

struct Statement *mk_exprstmt(int line_num, int col_num, struct Expression *expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = col_num;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_EXPR;
    new_stmt->stmt_data.expr_stmt_data.expr = expr;

    return new_stmt;
}

struct Statement *mk_compoundstatement(int line_num, ListNode_t *compound_statement)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_COMPOUND_STATEMENT;
    new_stmt->stmt_data.compound_statement = compound_statement;

    return new_stmt;
}

struct Statement *mk_ifthen(int line_num, struct Expression *eval_relop, struct Statement *if_stmt,
                            struct Statement *else_stmt)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_IF_THEN;
    new_stmt->stmt_data.if_then_data.relop_expr = eval_relop;
    new_stmt->stmt_data.if_then_data.if_stmt = if_stmt;
    new_stmt->stmt_data.if_then_data.else_stmt = else_stmt;

    return new_stmt;
}

struct Statement *mk_while(int line_num, struct Expression *eval_relop,
                            struct Statement *while_stmt)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_WHILE;
    new_stmt->stmt_data.while_data.relop_expr = eval_relop;
    new_stmt->stmt_data.while_data.while_stmt = while_stmt;

    return new_stmt;
}

struct Statement *mk_repeat(int line_num, ListNode_t *body_list,
                            struct Expression *until_expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_REPEAT;
    new_stmt->stmt_data.repeat_data.body_list = body_list;
    new_stmt->stmt_data.repeat_data.until_expr = until_expr;

    return new_stmt;
}

struct Statement *mk_forassign(int line_num, struct Statement *for_assign, struct Expression *to,
                               struct Statement *do_for, int is_downto)
{
   struct Statement *new_stmt;
   new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

   new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
   new_stmt->source_index = -1;
   new_stmt->type = STMT_FOR;
   new_stmt->stmt_data.for_data.for_assign_type = STMT_FOR_ASSIGN_VAR;


   new_stmt->stmt_data.for_data.to = to;
   new_stmt->stmt_data.for_data.do_for = do_for;
   new_stmt->stmt_data.for_data.for_assign_data.var_assign = for_assign;
   new_stmt->stmt_data.for_data.is_downto = is_downto;

   return new_stmt;
}

struct Statement *mk_forvar(int line_num, struct Expression *for_var, struct Expression *to,
                              struct Statement *do_for, int is_downto)
{
  struct Statement *new_stmt;
  new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

  new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
  new_stmt->source_index = -1;
  new_stmt->type = STMT_FOR;
  new_stmt->stmt_data.for_data.for_assign_type = STMT_FOR_VAR;


  new_stmt->stmt_data.for_data.to = to;
  new_stmt->stmt_data.for_data.do_for = do_for;
  new_stmt->stmt_data.for_data.for_assign_data.var = for_var;
  new_stmt->stmt_data.for_data.is_downto = is_downto;

  return new_stmt;
}

struct Statement *mk_for_in(int line_num, struct Expression *loop_var, struct Expression *collection,
                             struct Statement *do_stmt)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_FOR_IN;
    new_stmt->stmt_data.for_in_data.loop_var = loop_var;
    new_stmt->stmt_data.for_in_data.collection = collection;
    new_stmt->stmt_data.for_in_data.do_stmt = do_stmt;

    return new_stmt;
}

struct Statement *mk_asmblock(int line_num, char *code)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_ASM_BLOCK;
    new_stmt->stmt_data.asm_block_data.code = code;

    return new_stmt;
}

struct Statement *mk_case(int line_num, struct Expression *selector, ListNode_t *branches, struct Statement *else_stmt)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_CASE;
    new_stmt->stmt_data.case_data.selector_expr = selector;
    new_stmt->stmt_data.case_data.branches = branches;
    new_stmt->stmt_data.case_data.else_stmt = else_stmt;

    return new_stmt;
}

struct Statement *mk_with(int line_num, struct Expression *context, struct Statement *body)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_WITH;
    new_stmt->stmt_data.with_data.context_expr = context;
    new_stmt->stmt_data.with_data.body_stmt = body;

    return new_stmt;
}

struct Statement *mk_tryfinally(int line_num, ListNode_t *try_stmts, ListNode_t *finally_stmts)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_TRY_FINALLY;
    new_stmt->stmt_data.try_finally_data.try_statements = try_stmts;
    new_stmt->stmt_data.try_finally_data.finally_statements = finally_stmts;

    return new_stmt;
}

struct Statement *mk_tryexcept(int line_num, ListNode_t *try_stmts, ListNode_t *except_stmts,
                               char *exception_var_name, char *exception_type_name)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_TRY_EXCEPT;
    new_stmt->stmt_data.try_except_data.try_statements = try_stmts;
    new_stmt->stmt_data.try_except_data.except_statements = except_stmts;
    new_stmt->stmt_data.try_except_data.exception_var_name = exception_var_name;
    new_stmt->stmt_data.try_except_data.exception_type_name = exception_type_name;
    new_stmt->stmt_data.try_except_data.has_on_clause = (exception_var_name != NULL || exception_type_name != NULL) ? 1 : 0;

    return new_stmt;
}

struct Statement *mk_raise(int line_num, struct Expression *expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_RAISE;
    new_stmt->stmt_data.raise_data.exception_expr = expr;

    return new_stmt;
}

struct Statement *mk_inherited(int line_num, struct Expression *expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->col_num = 0;
    new_stmt->source_index = -1;
    new_stmt->type = STMT_INHERITED;
    new_stmt->stmt_data.inherited_data.call_expr = expr;

    return new_stmt;
}

/*********** Expression routines ***************/
static void init_expression(struct Expression *expr, int line_num, enum ExprType type)
{
    assert(expr != NULL);
    expr->line_num = line_num;
    expr->col_num = 0;  /* Initialize to 0 - not tracked for expressions yet */
    expr->source_index = -1;  /* -1 indicates unknown byte offset */
    expr->type = type;
    expr->field_width = NULL;
    expr->field_precision = NULL;
    expr->resolved_kgpc_type = NULL;
    expr->is_default_initializer = 0;
    expr->pointer_subtype = UNKNOWN_TYPE;
    expr->pointer_subtype_id = NULL;
    expr->record_type = NULL;
    expr->is_pointer_diff = 0;
    expr->is_array_expr = 0;
    expr->array_element_type = UNKNOWN_TYPE;
    expr->array_element_type_id = NULL;
    expr->array_lower_bound = 0;
    expr->array_upper_bound = -1;
    expr->array_element_size = 0;
    expr->array_is_dynamic = 0;
    expr->array_element_record_type = NULL;
    expr->expr_data.array_literal_data.elements = NULL;
    expr->expr_data.array_literal_data.element_count = 0;
    expr->expr_data.array_literal_data.elements_semchecked = 0;
    expr->expr_data.function_call_data.id = NULL;
    expr->expr_data.function_call_data.mangled_id = NULL;
    expr->expr_data.function_call_data.args_expr = NULL;
    expr->expr_data.function_call_data.resolved_func = NULL;
    expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
    expr->expr_data.function_call_data.call_kgpc_type = NULL;
    expr->expr_data.function_call_data.is_call_info_valid = 0;
    expr->expr_data.function_call_data.is_procedural_var_call = 0;
    expr->expr_data.function_call_data.procedural_var_symbol = NULL;
    expr->expr_data.function_call_data.procedural_var_expr = NULL;
    expr->expr_data.function_call_data.is_method_call_placeholder = 0;
    expr->expr_data.function_call_data.is_virtual_call = 0;
    expr->expr_data.function_call_data.vmt_index = -1;
    expr->expr_data.function_call_data.self_class_name = NULL;
    expr->expr_data.function_call_data.arg0_is_dynarray_descriptor = 0;
    expr->expr_data.array_access_data.linear_index_count = 0;
    expr->expr_data.array_access_data.linear_strides = NULL;
    expr->expr_data.array_access_data.linear_lowers = NULL;
    expr->expr_data.array_access_data.linear_info_valid = 0;
}

struct Expression *mk_relop(int line_num, int type, struct Expression *left,
                                struct Expression *right)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_RELOP);
    new_expr->expr_data.relop_data.type = type;
    new_expr->expr_data.relop_data.left = left;
    new_expr->expr_data.relop_data.right = right;

    return new_expr;
}

struct Expression *mk_signterm(int line_num, struct Expression *sign_term)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_SIGN_TERM);
    new_expr->expr_data.sign_term = sign_term;

    return new_expr;
}

struct Expression *mk_addop(int line_num, int type, struct Expression *left,
                                struct Expression *right)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ADDOP);
    new_expr->expr_data.addop_data.addop_type = type;
    new_expr->expr_data.addop_data.left_expr = left;
    new_expr->expr_data.addop_data.right_term = right;

    return new_expr;
}

struct Expression *mk_mulop(int line_num, int type, struct Expression *left,
                                struct Expression *right)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_MULOP);
    new_expr->expr_data.mulop_data.mulop_type = type;
    new_expr->expr_data.mulop_data.left_term = left;
    new_expr->expr_data.mulop_data.right_factor = right;

    return new_expr;
}

struct Expression *mk_varid(int line_num, char *id)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_VAR_ID);
    new_expr->expr_data.id = id;

    return new_expr;
}

struct Expression *mk_arrayaccess(int line_num, struct Expression *array_expr, struct Expression *index_expr)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ARRAY_ACCESS);
    new_expr->expr_data.array_access_data.array_expr = array_expr;
    new_expr->expr_data.array_access_data.index_expr = index_expr;
    new_expr->expr_data.array_access_data.extra_indices = NULL;
    new_expr->expr_data.array_access_data.linear_index_count = 0;
    new_expr->expr_data.array_access_data.linear_strides = NULL;
    new_expr->expr_data.array_access_data.linear_lowers = NULL;
    new_expr->expr_data.array_access_data.linear_info_valid = 0;

    return new_expr;
}

struct Expression *mk_recordaccess(int line_num, struct Expression *record_expr, char *field_id)
{
    if (record_expr != NULL && record_expr->type == EXPR_ADDR)
    {
        struct Expression *inner = record_expr->expr_data.addr_data.expr;
        free(record_expr);
        return mk_addressof(line_num, mk_recordaccess(line_num, inner, field_id));
    }

    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_RECORD_ACCESS);
    new_expr->expr_data.record_access_data.record_expr = record_expr;
    new_expr->expr_data.record_access_data.field_id = field_id;
    new_expr->expr_data.record_access_data.field_offset = 0;

    return new_expr;
}

struct Expression *mk_pointer_deref(int line_num, struct Expression *pointer_expr)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_POINTER_DEREF);
    new_expr->expr_data.pointer_deref_data.pointer_expr = pointer_expr;

    return new_expr;
}

struct Expression *mk_array_literal(int line_num, ListNode_t *elements, int element_count)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ARRAY_LITERAL);
    new_expr->expr_data.array_literal_data.elements = elements;
    new_expr->expr_data.array_literal_data.element_count = element_count;
    new_expr->expr_data.array_literal_data.elements_semchecked = 0;
    new_expr->is_array_expr = 1;
    new_expr->array_is_dynamic = 1;
    new_expr->array_lower_bound = 0;
    new_expr->array_upper_bound = element_count - 1;

    return new_expr;
}

struct Expression *mk_addressof(int line_num, struct Expression *expr)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ADDR);
    new_expr->expr_data.addr_data.expr = expr;

    return new_expr;
}

struct Expression *mk_functioncall(int line_num, char *id, ListNode_t *args)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_FUNCTION_CALL);
    new_expr->expr_data.function_call_data.id = id;
    new_expr->expr_data.function_call_data.mangled_id = NULL;
    new_expr->expr_data.function_call_data.args_expr = args;
    new_expr->expr_data.function_call_data.resolved_func = NULL;
    new_expr->expr_data.function_call_data.is_call_info_valid = 0;
    new_expr->expr_data.function_call_data.call_hash_type = HASHTYPE_VAR;
    new_expr->expr_data.function_call_data.call_kgpc_type = NULL;
    new_expr->expr_data.function_call_data.is_procedural_var_call = 0;
    new_expr->expr_data.function_call_data.procedural_var_symbol = NULL;
    new_expr->expr_data.function_call_data.procedural_var_expr = NULL;
    new_expr->expr_data.function_call_data.is_method_call_placeholder = 0;

    return new_expr;
}

struct Expression *mk_inum(int line_num, long long i_num)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_INUM);
    new_expr->expr_data.i_num = i_num;

    return new_expr;
}

struct Expression *mk_charcode(int line_num, unsigned int char_code)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_CHAR_CODE);
    new_expr->expr_data.char_code = char_code & 0xFFu;

    return new_expr;
}

struct Expression *mk_string(int line_num, char *string)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_STRING);
    new_expr->expr_data.string = string;

    return new_expr;
}

struct Expression *mk_rnum(int line_num, double r_num)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_RNUM);
    new_expr->expr_data.r_num = r_num;

    return new_expr;
}

struct Expression *mk_bool(int line_num, int value)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_BOOL);
    new_expr->expr_data.bool_value = (value != 0);

    return new_expr;
}

struct Expression *mk_nil(int line_num)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_NIL);
    return new_expr;
}

struct SetElement *mk_set_element(struct Expression *lower, struct Expression *upper)
{
    struct SetElement *element = (struct SetElement *)malloc(sizeof(struct SetElement));
    assert(element != NULL);

    element->lower = lower;
    element->upper = upper;
    return element;
}

void destroy_set_element(struct SetElement *element)
{
    if (element == NULL)
        return;

    if (element->lower != NULL)
        destroy_expr(element->lower);
    if (element->upper != NULL)
        destroy_expr(element->upper);
    free(element);
}

struct Expression *mk_set(int line_num, unsigned int bitmask, ListNode_t *elements, int is_constant)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_SET);
    new_expr->expr_data.set_data.bitmask = bitmask;
    new_expr->expr_data.set_data.elements = elements;
    new_expr->expr_data.set_data.is_constant = is_constant;

    return new_expr;
}

struct Expression *mk_record_constructor(int line_num, ListNode_t *fields, int field_count)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_RECORD_CONSTRUCTOR);
    new_expr->expr_data.record_constructor_data.fields = fields;
    new_expr->expr_data.record_constructor_data.field_count = field_count;
    new_expr->expr_data.record_constructor_data.fields_semchecked = 0;

    return new_expr;
}

struct Expression *mk_typecast(int line_num, int target_type, char *target_type_id,
    struct Expression *expr)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_TYPECAST);
    new_expr->expr_data.typecast_data.target_type = target_type;
    new_expr->expr_data.typecast_data.target_type_id = target_type_id;
    new_expr->expr_data.typecast_data.expr = expr;

    return new_expr;
}

struct Expression *mk_is(int line_num, struct Expression *expr,
    int target_type, char *target_type_id)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_IS);
    new_expr->expr_data.is_data.expr = expr;
    new_expr->expr_data.is_data.target_type = target_type;
    new_expr->expr_data.is_data.target_type_id = target_type_id;
    new_expr->expr_data.is_data.target_record_type = NULL;
    return new_expr;
}

struct Expression *mk_as(int line_num, struct Expression *expr,
    int target_type, char *target_type_id)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_AS);
    new_expr->expr_data.as_data.expr = expr;
    new_expr->expr_data.as_data.target_type = target_type;
    new_expr->expr_data.as_data.target_type_id = target_type_id;
    new_expr->expr_data.as_data.target_record_type = NULL;
    return new_expr;
}

struct Expression *mk_anonymous_function(int line_num, char *generated_name, 
    ListNode_t *parameters, int return_type, char *return_type_id, struct Statement *body)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ANONYMOUS_FUNCTION);
    new_expr->expr_data.anonymous_method_data.generated_name = generated_name;
    new_expr->expr_data.anonymous_method_data.parameters = parameters;
    new_expr->expr_data.anonymous_method_data.return_type = return_type;
    new_expr->expr_data.anonymous_method_data.return_type_id = return_type_id;
    new_expr->expr_data.anonymous_method_data.body = body;
    new_expr->expr_data.anonymous_method_data.is_function = 1;

    return new_expr;
}

struct Expression *mk_anonymous_procedure(int line_num, char *generated_name,
    ListNode_t *parameters, struct Statement *body)
{
    struct Expression *new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ANONYMOUS_PROCEDURE);
    new_expr->expr_data.anonymous_method_data.generated_name = generated_name;
    new_expr->expr_data.anonymous_method_data.parameters = parameters;
    new_expr->expr_data.anonymous_method_data.return_type = -1;  // No return type for procedures
    new_expr->expr_data.anonymous_method_data.return_type_id = NULL;
    new_expr->expr_data.anonymous_method_data.body = body;
    new_expr->expr_data.anonymous_method_data.is_function = 0;

    return new_expr;
}
static void clear_type_alias_fields(struct TypeAlias *alias)
{
    if (alias == NULL)
        return;

    if (alias->alias_name != NULL)
    {
        free(alias->alias_name);
        alias->alias_name = NULL;
    }
    if (alias->target_type_id != NULL)
    {
        free(alias->target_type_id);
        alias->target_type_id = NULL;
    }
    if (alias->array_element_type_id != NULL)
    {
        free(alias->array_element_type_id);
        alias->array_element_type_id = NULL;
    }
    if (alias->array_dimensions != NULL)
    {
        destroy_list(alias->array_dimensions);
        alias->array_dimensions = NULL;
    }
    if (alias->pointer_type_id != NULL)
    {
        free(alias->pointer_type_id);
        alias->pointer_type_id = NULL;
    }
    if (alias->set_element_type_id != NULL)
    {
        free(alias->set_element_type_id);
        alias->set_element_type_id = NULL;
    }
    if (alias->inline_enum_values != NULL)
    {
        destroy_list(alias->inline_enum_values);
        alias->inline_enum_values = NULL;
    }
    if (alias->enum_literals != NULL)
    {
        destroy_list(alias->enum_literals);
        alias->enum_literals = NULL;
    }
    if (alias->file_type_id != NULL)
    {
        free(alias->file_type_id);
        alias->file_type_id = NULL;
    }
    if (alias->kgpc_type != NULL)
    {
        kgpc_type_release(alias->kgpc_type);
        alias->kgpc_type = NULL;
    }
    if (alias->inline_record_type != NULL)
    {
        destroy_record_type(alias->inline_record_type);
        alias->inline_record_type = NULL;
    }
    alias->is_char_alias = 0;
    alias->is_shortstring = 0;
    alias->is_range = 0;
    alias->range_known = 0;
    alias->range_start = 0;
    alias->range_end = 0;
    alias->storage_size = 0;
}
