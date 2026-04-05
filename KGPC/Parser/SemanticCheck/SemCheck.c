/*
    Damon Gwinn
    Performs semantic checking on a given parse tree

    NOTE: Max scope level refers to the highest level scope we can reference a variable at
        - 0 is the current scope, 1 is the first above and so on
        - Functions can't have side effects, but they can contain procedures so this is a
            general way to define the maximum scope level

    TODO: CHECK FOR RETURN IN FUNCTIONS (Add a "referenced" flag to symbol table elements. Need it for optimizations anyway...)
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <time.h>
#ifndef _WIN32
#include <strings.h>
#include <unistd.h>
#include <fcntl.h>
#else
#define strcasecmp _stricmp
#include <io.h>
#include <fcntl.h>
#endif
#include <math.h>
#include "SemCheck.h"
#include "../ParseTree/ident_ref.h"
#include "SemChecks/SemCheck_sizeof.h"
#include "../../flags.h"
#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "../../string_intern.h"
#include "../../Optimizer/optimizer.h"
#include "../../compilation_context.h"
#include "../pascal_frontend.h"
#include "../ParseTree/tree.h"
#include "../ParseTree/tree_types.h"

/* From SemCheck_Expr_Constructors.c */
int semcheck_typecheck_record_constructor(struct Expression *expr, SymTab_t *symtab,
    int max_scope_lev, struct RecordType *record_type, int line_num);
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/type_tags.h"

HashNode_t *semcheck_find_type_node_in_owner_chain(SymTab_t *symtab,
    const char *type_id, const char *owner_full, const char *owner_outer);
#include "../ParseTree/KgpcType.h"
#include "../ParseTree/from_cparser.h"
#include "../ParseTree/operator_registry.h"
#include "../ParseTree/generic_types.h"
#include "../parser_error.h"
#include "../ErrVars.h"
#include "./SymTab/SymTab.h"
#include "./HashTable/HashTable.h"
#include "SemChecks/SemCheck_stmt.h"
#include "SemChecks/SemCheck_expr.h"
#include "NameMangling.h"
#include <stdarg.h>

#include "SemCheck_parts/SemCheck_env_types_errors.inc"
#include "SemCheck_parts/SemCheck_symbols_and_class.inc"
#include "SemCheck_parts/SemCheck_const_eval.inc"
#include "SemCheck_parts/SemCheck_init_and_inheritance.inc"
#include "SemCheck_parts/SemCheck_vmt_and_type_decls.inc"
#include "SemCheck_parts/SemCheck_const_decls_and_builtins.inc"
#include "SemCheck_parts/SemCheck_program_and_vars.inc"
#include "SemCheck_parts/SemCheck_subprograms.inc"
