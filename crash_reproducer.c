#include <stdio.h>
#include "GPC/Parser/ParseTree/tree.h"
#include "GPC/Parser/ParseTree/tree_types.h"
#include "GPC/Parser/List/List.h"
#include "GPC/Parser/SemanticCheck/HashTable/HashTable.h"
#include "GPC/CodeGenerator/Intel_x86-64/codegen.h"
#include "GPC/CodeGenerator/Intel_x86-64/codegen_statement.h"
#include "GPC/CodeGenerator/Intel_x86-64/codegen_expression.h"

int main() {
    CodeGenContext ctx;
    ctx.output_file = fopen("dummy.s", "w");
    ctx.label_counter = 1;
    ctx.write_label_counter = 1;

    struct Statement stmt;
    stmt.type = STMT_PROCEDURE_CALL;
    stmt.line_num = 1;

    struct ProcedureCallData proc_data;
    proc_data.id = "mod3";
    proc_data.mangled_id = "mod3";

    struct Expression expr;
    expr.type = EXPR_VAR_ID;
    expr.line_num = 1;
    expr.expr_data.id = "i";

    proc_data.expr_args = CreateListNode(&expr, LIST_EXPR);

    HashNode_t proc_node;
    proc_node.id = "mod3";
    proc_node.mangled_id = "mod3";
    proc_node.hash_type = HASHTYPE_PROCEDURE;

    Tree_t arg_tree;
    arg_tree.type = TREE_VAR_DECL;
    struct VarDeclData var_decl_data;
    var_decl_data.ids = CreateListNode("s", LIST_STRING);
    var_decl_data.type = INT_TYPE;
    arg_tree.tree_data.var_decl_data = var_decl_data;

    proc_node.args = CreateListNode(&arg_tree, LIST_TREE);

    proc_data.resolved_proc = &proc_node;

    stmt.stmt_data.procedure_call_data = proc_data;

    codegen_proc_call(&stmt, NULL, &ctx);

    fclose(ctx.output_file);

    return 0;
}
