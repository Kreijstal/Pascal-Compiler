#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "register_types.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "asm_emit.h"  /* Assembly instruction emission helpers */
#include "expr_tree/expr_tree.h"

#include "codegen_statement_parts/codegen_stmt_infrastructure.inc"
#include "codegen_statement_parts/codegen_stmt_assignment.inc"
#include "codegen_statement_parts/codegen_stmt_dispatch.inc"
#include "codegen_statement_parts/codegen_stmt_builtins.inc"
#include "codegen_statement_parts/codegen_stmt_calls_and_control.inc"
