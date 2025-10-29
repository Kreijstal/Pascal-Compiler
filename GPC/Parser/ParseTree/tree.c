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

        case STMT_ASM_BLOCK:
          fprintf(f, "[ASM_BLOCK]:\n");
          print_indent(f, num_indent+1);
          fprintf(f, "%s\n", stmt->stmt_data.asm_block_data.code);
          break;

        case STMT_EXIT:
          fprintf(f, "[EXIT]\n");
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

        case EXPR_BOOL:
          fprintf(f, "[BOOL:%s]\n", expr->expr_data.bool_value ? "TRUE" : "FALSE");
          break;
        case EXPR_SET:
          fprintf(f, "[SET:0x%X]\n", expr->expr_data.set_data.bitmask);
          break;

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
            case LIST_SET_ELEMENT:
                destroy_set_element((struct SetElement *)cur->cur);
                break;
            case LIST_CASE_BRANCH:
                /* Case branches are handled specially in destroy_stmt for STMT_CASE */
                /* Don't free the branch here as it's already freed there */
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
          if (tree->tree_data.var_decl_data.type_id != NULL)
              free(tree->tree_data.var_decl_data.type_id);
          if (tree->tree_data.var_decl_data.initializer != NULL)
              destroy_stmt(tree->tree_data.var_decl_data.initializer);
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
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
            {
                struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                if (alias->target_type_id != NULL)
                    free(alias->target_type_id);
                if (alias->array_element_type_id != NULL)
                    free(alias->array_element_type_id);
                if (alias->array_dimensions != NULL)
                    destroy_list(alias->array_dimensions);
                if (alias->pointer_type_id != NULL)
                    free(alias->pointer_type_id);
                if (alias->set_element_type_id != NULL)
                    free(alias->set_element_type_id);
                if (alias->enum_literals != NULL)
                    destroy_list(alias->enum_literals);
                if (alias->file_type_id != NULL)
                    free(alias->file_type_id);
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

        case STMT_ASM_BLOCK:
          free(stmt->stmt_data.asm_block_data.code);
          break;

        case STMT_EXIT:
        case STMT_BREAK:
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

        case EXPR_RECORD_ACCESS:
          if (expr->expr_data.record_access_data.record_expr != NULL)
              destroy_expr(expr->expr_data.record_access_data.record_expr);
          free(expr->expr_data.record_access_data.field_id);
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

        case EXPR_BOOL:
          break;

        case EXPR_SET:
          destroy_list(expr->expr_data.set_data.elements);
          expr->expr_data.set_data.elements = NULL;
          break;

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

        default:
          fprintf(stderr, "BAD TYPE IN destroy_expr!\n");
          exit(1);
    }
    if (expr->pointer_subtype_id != NULL)
    {
        free(expr->pointer_subtype_id);
        expr->pointer_subtype_id = NULL;
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

Tree_t *mk_vardecl(int line_num, ListNode_t *ids, int type, char *type_id, int is_var_param, int inferred_type, struct Statement *initializer)
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
    new_tree->tree_data.var_decl_data.inferred_type = inferred_type;
    new_tree->tree_data.var_decl_data.initializer = initializer;

    return new_tree;
}

Tree_t *mk_typealiasdecl(int line_num, char *id, int is_array, int actual_type, char *type_id, int start, int end)
{
    Tree_t *new_tree = (Tree_t *)malloc(sizeof(Tree_t));
    assert(new_tree != NULL);

    new_tree->line_num = line_num;
    new_tree->type = TREE_TYPE_DECL;
    new_tree->tree_data.type_decl_data.id = id;
    new_tree->tree_data.type_decl_data.kind = TYPE_DECL_ALIAS;

    struct TypeAlias *alias = &new_tree->tree_data.type_decl_data.info.alias;
    alias->base_type = is_array ? UNKNOWN_TYPE : actual_type;
    alias->target_type_id = NULL;
    alias->is_array = is_array;
    alias->array_start = start;
    alias->array_end = end;
    alias->array_element_type = UNKNOWN_TYPE;
    alias->array_element_type_id = NULL;
    alias->is_open_array = (alias->is_array && end < start);
    alias->array_dimensions = NULL;
    alias->is_pointer = 0;
    alias->pointer_type = UNKNOWN_TYPE;
    alias->pointer_type_id = NULL;
    alias->is_set = 0;
    alias->set_element_type = UNKNOWN_TYPE;
    alias->set_element_type_id = NULL;
    alias->is_enum = 0;
    alias->enum_literals = NULL;
    alias->is_file = 0;
    alias->file_type = UNKNOWN_TYPE;
    alias->file_type_id = NULL;

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

struct Statement *mk_break(int line_num)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_BREAK;
    memset(&new_stmt->stmt_data, 0, sizeof(new_stmt->stmt_data));

    return new_stmt;
}

struct Statement *mk_exit(int line_num)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_EXIT;
    memset(&new_stmt->stmt_data, 0, sizeof(new_stmt->stmt_data));

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

struct Statement *mk_repeat(int line_num, ListNode_t *body_list,
                            struct Expression *until_expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_REPEAT;
    new_stmt->stmt_data.repeat_data.body_list = body_list;
    new_stmt->stmt_data.repeat_data.until_expr = until_expr;

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

struct Statement *mk_case(int line_num, struct Expression *selector, ListNode_t *branches, struct Statement *else_stmt)
{
    struct Statement *new_stmt;
    new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
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
    new_stmt->type = STMT_TRY_FINALLY;
    new_stmt->stmt_data.try_finally_data.try_statements = try_stmts;
    new_stmt->stmt_data.try_finally_data.finally_statements = finally_stmts;

    return new_stmt;
}

struct Statement *mk_tryexcept(int line_num, ListNode_t *try_stmts, ListNode_t *except_stmts)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_TRY_EXCEPT;
    new_stmt->stmt_data.try_except_data.try_statements = try_stmts;
    new_stmt->stmt_data.try_except_data.except_statements = except_stmts;

    return new_stmt;
}

struct Statement *mk_raise(int line_num, struct Expression *expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_RAISE;
    new_stmt->stmt_data.raise_data.exception_expr = expr;

    return new_stmt;
}

struct Statement *mk_inherited(int line_num, struct Expression *expr)
{
    struct Statement *new_stmt = (struct Statement *)malloc(sizeof(struct Statement));
    assert(new_stmt != NULL);

    new_stmt->line_num = line_num;
    new_stmt->type = STMT_INHERITED;
    new_stmt->stmt_data.inherited_data.call_expr = expr;

    return new_stmt;
}

/*********** Expression routines ***************/
static void init_expression(struct Expression *expr, int line_num, enum ExprType type)
{
    assert(expr != NULL);
    expr->line_num = line_num;
    expr->type = type;
    expr->field_width = NULL;
    expr->field_precision = NULL;
    expr->resolved_type = UNKNOWN_TYPE;
    expr->pointer_subtype = UNKNOWN_TYPE;
    expr->pointer_subtype_id = NULL;
    expr->record_type = NULL;
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

struct Expression *mk_arrayaccess(int line_num, char *id, struct Expression *index_expr)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_ARRAY_ACCESS);
    new_expr->expr_data.array_access_data.id = id;
    new_expr->expr_data.array_access_data.array_expr = index_expr;

    return new_expr;
}

struct Expression *mk_recordaccess(int line_num, struct Expression *record_expr, char *field_id)
{
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

struct Expression *mk_string(int line_num, char *string)
{
    struct Expression *new_expr;
    new_expr = (struct Expression *)malloc(sizeof(struct Expression));
    assert(new_expr != NULL);

    init_expression(new_expr, line_num, EXPR_STRING);
    new_expr->expr_data.string = string;

    return new_expr;
}

struct Expression *mk_rnum(int line_num, float r_num)
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
