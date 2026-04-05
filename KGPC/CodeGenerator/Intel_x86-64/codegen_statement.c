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


#include "codegen_statement_parts/codegen_statement_part1.inc"
#include "codegen_statement_parts/codegen_statement_part2.inc"
#include "codegen_statement_parts/codegen_statement_part3.inc"
#include "codegen_statement_parts/codegen_statement_part4.inc"
#include "codegen_statement_parts/codegen_statement_part5.inc"
#include "codegen_statement_parts/codegen_statement_part6.inc"

