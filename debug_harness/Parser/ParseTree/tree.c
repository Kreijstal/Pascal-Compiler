/*
    Damon Gwinn
    Parse tree for the Pascal Grammar
*/

#include "tree.h"
#include "tree_types.h"
#include "type_tags.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

static void print_record_field(struct RecordField *field, FILE *f, int num_indent);
static void destroy_record_field(struct RecordField *field);

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

    if (field->nested_record != NULL)
    {
        print_indent(f, num_indent + 1);
        fprintf(f, "[NESTED_RECORD]:\n");
        list_print(field->nested_record->fields, f, num_indent + 2);
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
    destroy_record_type(field->nested_record);
    free(field);
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
          break;

        case TREE_UNIT:
          assert(tree->tree_data.unit_data.unit_id != NULL);
          fprintf(f, "[UNIT:%s]\n", tree->tree_data.unit_data.unit_id);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[INTERFACE_USES]:\n");
          list_print(tree->tree_data.unit_data.interface_uses, f, num_indent+1);

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

        case STMT_COMPOUND_STATEMENT:
          fprintf(f, "[COMPOUND_STMT]:\n");
          list_print(stmt->stmt_data.compound_statement, f, num_indent+1);
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

          case STMT_ASM_BLOCK:
            fprintf(f, "[ASM_BLOCK]:\n");
            print_indent(f, num_indent+1);
            fprintf(f, "%s\n", stmt->stmt_data.asm_block_data.code);
            break;

          default:
            fprintf(stderr, "BAD TYPE IN stmt_print!\n");
            exit(1);
    }
}

void expr_print(struct Expression *expr, FILE *f, int num_indent)
{
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
          fprintf(f, "[ARRAY_ACC:%s]\n", expr->expr_data.array_access_data.id);
          ++num_indent;

          print_indent(f, num_indent);
          fprintf(f, "[INDEX]:\n");
          expr_print(expr->expr_data.array_access_data.array_expr, f, num_indent+1);
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

        default:
          fprintf(stderr, "BAD TYPE IN expr_print!\n");
          exit(1);
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

          destroy_list(tree->tree_data.program_data.const_declaration);
          destroy_list(tree->tree_data.program_data.type_declaration);

          destroy_list(tree->tree_data.program_data.var_declaration);

          destroy_list(tree->tree_data.program_data.subprograms);

          destroy_stmt(tree->tree_data.program_data.body_statement);
          break;

        case TREE_UNIT:
          free(tree->tree_data.unit_data.unit_id);
          destroy_list(tree->tree_data.unit_data.interface_uses);
          destroy_list(tree->tree_data.unit_data.interface_type_decls);
          destroy_list(tree->tree_data.unit_data.interface_var_decls);
          destroy_list(tree->tree_data.unit_data.implementation_uses);
          destroy_list(tree->tree_data.unit_data.implementation_type_decls);
          destroy_list(tree->tree_data.unit_data.implementation_var_decls);
          destroy_list(tree->tree_data.unit_data.subprograms);
          if (tree->tree_data.unit_data.initialization != NULL)
              destroy_stmt(tree->tree_data.unit_data.initialization);
          break;

        case TREE_SUBPROGRAM:
          free(tree->tree_data.subprogram_data.id);
          if (tree->tree_data.subprogram_data.mangled_id != NULL)
            free(tree->tree_data.subprogram_data.mangled_id); // <-- ADD THIS
          if (tree->tree_data.subprogram_data.return_type_id != NULL)
            free(tree->tree_data.subprogram_data.return_type_id);

          destroy_list(tree->tree_data.subprogram_data.args_var);

          destroy_list(tree->tree_data.subprogram_data.const_declarations);
          destroy_list(tree->tree_data.subprogram_data.declarations);

          destroy_list(tree->tree_data.subprogram_data.subprograms);

          destroy_stmt(tree->tree_data.subprogram_data.statement_list);
          break;

        case TREE_VAR_DECL:
          destroy_list(tree->tree_data.var_decl_data.ids);
          break;

        case TREE_ARR_DECL:
          destroy_list(tree->tree_data.arr_decl_data.ids);
          if (tree->tree_data.arr_decl_data.type_id != NULL)
            free(tree->tree_data.arr_decl_data.type_id);
          break;

        case TREE_CONST_DECL:
          free(tree->tree_data.const_decl_data.id);
          if (tree->tree_data.const_decl_data.type_id != NULL)
            free(tree->tree_data.const_decl_data.type_id);
          destroy_expr(tree->tree_data.const_decl_data.value);
          break;

        case TREE_TYPE_DECL:
            free(tree->tree_data.type_decl_data.id);
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                destroy_record_type(tree->tree_data.type_decl_data.info.record);
            break;

        default:
          fprintf(stderr, "BAD TYPE IN destroy_tree!\n");
          exit(1);
    }
    free(tree);
}

void destroy_stmt(struct Statement *stmt)
{
    assert(stmt != NULL);
    switch(stmt->type)
    {
        case STMT_VAR_ASSIGN:
          destroy_expr(stmt->stmt_data.var_assign_data.var);
          destroy_expr(stmt->stmt_data.var_assign_data.expr);
          break;

        case STMT_PROCEDURE_CALL:
          free(stmt->stmt_data.procedure_call_data.id);
          if (stmt->stmt_data.procedure_call_data.mangled_id != NULL)
            free(stmt->stmt_data.procedure_call_data.mangled_id);
          destroy_list(stmt->stmt_data.procedure_call_data.expr_args);
          break;

        case STMT_COMPOUND_STATEMENT:
          destroy_list(stmt->stmt_data.compound_statement);
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

        case STMT_ASM_BLOCK:
          free(stmt->stmt_data.asm_block_data.code);
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
    switch(expr->type)
    {
        case EXPR_RELOP:
          destroy_expr(expr->expr_data.relop_data.left);
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
          free(expr->expr_data.array_access_data.id);
          destroy_expr(expr->expr_data.array_access_data.array_expr);
          break;

        case EXPR_FUNCTION_CALL:
          free(expr->expr_data.function_call_data.id);
          destroy_list(expr->expr_data.function_call_data.args_expr);
          break;

        case EXPR_INUM:
          break;

        case EXPR_RNUM:
          break;

        case EXPR_STRING:
          free(expr->expr_data.string);
          break;

        default:
          fprintf(stderr, "BAD TYPE IN expr_print!\n");
          exit(1);
    }
    free(expr);
}

void destroy_record_type(struct RecordType *record_type)
{
    if (record_type == NULL)
        return;

    destroy_list(record_type->fields);
    free(record_type);
}

struct RecordType *clone_record_type(const struct RecordType *record_type)
{
    if (record_type == NULL)
        return NULL;

    struct RecordType *clone = (struct RecordType *)malloc(sizeof(struct RecordType));
    assert(clone != NULL);
    clone->fields = NULL;

    ListNode_t *cur = record_type->fields;
    while (cur != NULL)
    {
        struct RecordField *field = (struct RecordField *)cur->cur;
        assert(field != NULL);

        struct RecordField *field_clone = (struct RecordField *)malloc(sizeof(struct RecordField));
        assert(field_clone != NULL);
        field_clone->name = field->name != NULL ? strdup(field->name) : NULL;
        field_clone->type = field->type;
        field_clone->type_id = field->type_id != NULL ? strdup(field->type_id) : NULL;
        field_clone->nested_record = clone_record_type(field->nested_record);

        ListNode_t *node = CreateListNode(field_clone, LIST_RECORD_FIELD);
        if (clone->fields == NULL)
            clone->fields = node;
        else
            PushListNodeBack(clone->fields, node);

        cur = cur->next;
    }

    return clone;
}

Tree_t *mk_program(int line_num, char *id, ListNode_t *args, ListNode_t *uses,
    ListNode_t *const_decl, ListNode_t *var_decl, ListNode_t *type_decl,
    ListNode_t *subprograms, struct Statement *compound_statement)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_PROGRAM_TYPE;
    new_tree->tree_data.program_data.program_id = id;
    new_tree->tree_data.program_data.args_char = args;
    new_tree->tree_data.program_data.uses_units = uses;
    new_tree->tree_data.program_data.const_declaration = const_decl;
    new_tree->tree_data.program_data.var_declaration = var_decl;
    new_tree->tree_data.program_data.type_declaration = type_decl;
    new_tree->tree_data.program_data.subprograms = subprograms;
    new_tree->tree_data.program_data.body_statement = compound_statement;

    return new_tree;
}

Tree_t *mk_unit(int line_num, char *id, ListNode_t *interface_uses,
    ListNode_t *interface_type_decls, ListNode_t *interface_var_decls,
    ListNode_t *implementation_uses, ListNode_t *implementation_type_decls,
    ListNode_t *implementation_var_decls, ListNode_t *subprograms,
    struct Statement *initialization)
{
    Tree_t *new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_UNIT;
    new_tree->tree_data.unit_data.unit_id = id;
    new_tree->tree_data.unit_data.interface_uses = interface_uses;
    new_tree->tree_data.unit_data.interface_type_decls = interface_type_decls;
    new_tree->tree_data.unit_data.interface_var_decls = interface_var_decls;
    new_tree->tree_data.unit_data.implementation_uses = implementation_uses;
    new_tree->tree_data.unit_data.implementation_type_decls = implementation_type_decls;
    new_tree->tree_data.unit_data.implementation_var_decls = implementation_var_decls;
    new_tree->tree_data.unit_data.subprograms = subprograms;
    new_tree->tree_data.unit_data.initialization = initialization;

    return new_tree;
}

Tree_t *mk_typedecl(int line_num, char *id, int start, int end)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_TYPE_DECL;
    new_tree->tree_data.type_decl_data.id = id;
    new_tree->tree_data.type_decl_data.kind = TYPE_DECL_RANGE;
    new_tree->tree_data.type_decl_data.info.range.start = start;
    new_tree->tree_data.type_decl_data.info.range.end = end;

    return new_tree;
}


Tree_t *mk_record_type(int line_num, char *id, struct RecordType *record_type)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_TYPE_DECL;
    new_tree->tree_data.type_decl_data.id = id;
    new_tree->tree_data.type_decl_data.kind = TYPE_DECL_RECORD;
    new_tree->tree_data.type_decl_data.info.record = record_type;

    return new_tree;
}


Tree_t *mk_procedure(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *var_decl, ListNode_t *subprograms, struct Statement *compound_statement,
    int cname_flag, int overload_flag)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_SUBPROGRAM;
    new_tree->tree_data.subprogram_data.sub_type = TREE_SUBPROGRAM_PROC;
    new_tree->tree_data.subprogram_data.id = id;
    new_tree->tree_data.subprogram_data.mangled_id = NULL;
    new_tree->tree_data.subprogram_data.args_var = args;
    new_tree->tree_data.subprogram_data.const_declarations = const_decl;
    new_tree->tree_data.subprogram_data.return_type = -1;
    new_tree->tree_data.subprogram_data.return_type_id = NULL;
    new_tree->tree_data.subprogram_data.cname_flag = cname_flag;
    new_tree->tree_data.subprogram_data.overload_flag = overload_flag;
    new_tree->tree_data.subprogram_data.declarations = var_decl;
    new_tree->tree_data.subprogram_data.subprograms = subprograms;
    new_tree->tree_data.subprogram_data.statement_list = compound_statement;

    return new_tree;
}

Tree_t *mk_function(int line_num, char *id, ListNode_t *args, ListNode_t *const_decl,
    ListNode_t *var_decl, ListNode_t *subprograms, struct Statement *compound_statement,
    int return_type, char *return_type_id, int cname_flag, int overload_flag)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_SUBPROGRAM;
    new_tree->tree_data.subprogram_data.sub_type = TREE_SUBPROGRAM_FUNC;
    new_tree->tree_data.subprogram_data.id = id;
    new_tree->tree_data.subprogram_data.mangled_id = NULL;
    new_tree->tree_data.subprogram_data.args_var = args;
    new_tree->tree_data.subprogram_data.const_declarations = const_decl;
    new_tree->tree_data.subprogram_data.return_type = return_type;
    new_tree->tree_data.subprogram_data.return_type_id = return_type_id;
    new_tree->tree_data.subprogram_data.cname_flag = cname_flag;
    new_tree->tree_data.subprogram_data.overload_flag = overload_flag;
    new_tree->tree_data.subprogram_data.declarations = var_decl;
    new_tree->tree_data.subprogram_data.subprograms = subprograms;
    new_tree->tree_data.subprogram_data.statement_list = compound_statement;

    return new_tree;
}

/*enum TreeType{TREE_PROGRAM_TYPE, TREE_SUBPROGRAM, TREE_VAR_DECL, TREE_STATEMENT_TYPE};*/

Tree_t *mk_vardecl(int line_num, ListNode_t *ids, int type, char *type_id, int is_var_param)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_VAR_DECL;
    new_tree->tree_data.var_decl_data.ids = ids;
    new_tree->tree_data.var_decl_data.type = type;
    new_tree->tree_data.var_decl_data.type_id = type_id;
    new_tree->tree_data.var_decl_data.is_var_param = is_var_param;

    return new_tree;
}

Tree_t *mk_arraydecl(int line_num, ListNode_t *ids, int type, char *type_id, int start, int end)
{
    Tree_t *new_tree;
    new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_ARR_DECL;
    new_tree->tree_data.arr_decl_data.ids = ids;
    new_tree->tree_data.arr_decl_data.type = type;
    new_tree->tree_data.arr_decl_data.type_id = type_id;
    new_tree->tree_data.arr_decl_data.s_range = start;
    new_tree->tree_data.arr_decl_data.e_range = end;

    return new_tree;
}

Tree_t *mk_constdecl(int line_num, char *id, char *type_id, struct Expression *value)
{
    Tree_t *new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_CONST_DECL;
    new_tree->tree_data.const_decl_data.id = id;
    new_tree->tree_data.const_decl_data.type_id = type_id;
    new_tree->tree_data.const_decl_data.value = value;

    return new_tree;
}


/************** Statement routines **************/
struct Statement *mk_varassign(int line_num, struct Expression *var, struct Expression *expr)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_VAR_ASSIGN;
    new_stmt->stmt_data.var_assign_data.var = var;
    new_stmt->stmt_data.var_assign_data.expr = expr;

    return new_stmt;
}

struct Statement *mk_procedurecall(int line_num, char *id, ListNode_t *expr_args)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_PROCEDURE_CALL;
    new_stmt->stmt_data.procedure_call_data.id = id;
    new_stmt->stmt_data.procedure_call_data.mangled_id = NULL;
    new_stmt->stmt_data.procedure_call_data.expr_args = expr_args;
    new_stmt->stmt_data.procedure_call_data.resolved_proc = NULL;

    return new_stmt;
}

struct Statement *mk_compoundstatement(int line_num, ListNode_t *compound_statement)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
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
    new_stmt->type = STMT_WHILE;
    new_stmt->stmt_data.while_data.relop_expr = eval_relop;
    new_stmt->stmt_data.while_data.while_stmt = while_stmt;

    return new_stmt;
}

struct Statement *mk_forassign(int line_num, struct Statement *for_assign, struct Expression *to,
                               struct Statement *do_for)
{
   struct Statement *new_stmt;
   new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

   new_stmt->line_num = line_num;
   new_stmt->type = STMT_FOR;
   new_stmt->stmt_data.for_data.for_assign_type = STMT_FOR_ASSIGN_VAR;


   new_stmt->stmt_data.for_data.to = to;
   new_stmt->stmt_data.for_data.do_for = do_for;
   new_stmt->stmt_data.for_data.for_assign_data.var_assign = for_assign;

   return new_stmt;
}

struct Statement *mk_forvar(int line_num, struct Expression *for_var, struct Expression *to,
                              struct Statement *do_for)
{
  struct Statement *new_stmt;
  new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

  new_stmt->line_num = line_num;
  new_stmt->type = STMT_FOR;
  new_stmt->stmt_data.for_data.for_assign_type = STMT_FOR_VAR;


  new_stmt->stmt_data.for_data.to = to;
  new_stmt->stmt_data.for_data.do_for = do_for;
  new_stmt->stmt_data.for_data.for_assign_data.var = for_var;

  return new_stmt;
}

struct Statement *mk_asmblock(int line_num, char *code)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_ASM_BLOCK;
    new_stmt->stmt_data.asm_block_data.code = code;

    return new_stmt;
}

/*********** Expression routines ***************/
struct Expression *mk_relop(int line_num, int type, struct Expression *left,
                                struct Expression *right)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_RELOP;
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

    new_expr->line_num = line_num;
    new_expr->type = EXPR_SIGN_TERM;
    new_expr->expr_data.sign_term = sign_term;

    return new_expr;
}

struct Expression *mk_addop(int line_num, int type, struct Expression *left,
                                struct Expression *right)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_ADDOP;
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

    new_expr->line_num = line_num;
    new_expr->type = EXPR_MULOP;
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

    new_expr->line_num = line_num;
    new_expr->type = EXPR_VAR_ID;
    new_expr->expr_data.id = id;

    return new_expr;
}

struct Expression *mk_arrayaccess(int line_num, char *id, struct Expression *index_expr)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_ARRAY_ACCESS;
    new_expr->expr_data.array_access_data.id = id;
    new_expr->expr_data.array_access_data.array_expr = index_expr;

    return new_expr;
}

struct Expression *mk_functioncall(int line_num, char *id, ListNode_t *args)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_FUNCTION_CALL;
    new_expr->expr_data.function_call_data.id = id;
    new_expr->expr_data.function_call_data.mangled_id = NULL;
    new_expr->expr_data.function_call_data.args_expr = args;
    new_expr->expr_data.function_call_data.resolved_func = NULL;

    return new_expr;
}

struct Expression *mk_inum(int line_num, long long i_num)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_INUM;
    new_expr->expr_data.i_num = i_num;

    return new_expr;
}

struct Expression *mk_string(int line_num, char *string)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_STRING;
    new_expr->expr_data.string = string;

    return new_expr;
}

struct Expression *mk_rnum(int line_num, float r_num)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    new_expr->line_num = line_num;
    new_expr->type = EXPR_RNUM;
    new_expr->expr_data.r_num = r_num;

    return new_expr;
}
