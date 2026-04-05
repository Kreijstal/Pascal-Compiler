/*
 * codegen_statement.c — Statement code generation (public API wrapper).
 *
 * The implementation is split across separate compilation units under
 * codegen_statement_parts/ for maintainability:
 *   - codegen_stmt_infrastructure.c  — register management, type helpers, expr evaluation
 *   - codegen_stmt_assignment.c      — string/shortstring/array/record assignment
 *   - codegen_stmt_dispatch.c        — control flow stacks, statement dispatch
 *   - codegen_stmt_builtins.c        — builtin procedure codegen
 *   - codegen_stmt_calls_and_control.c — procedure calls, loops, exceptions
 *
 * This file is intentionally empty; the public API declared in
 * codegen_statement.h is implemented in codegen_stmt_dispatch.c
 * (codegen_stmt, codegen_compound_stmt, etc.) and codegen_stmt_calls_and_control.c
 * (codegen_proc_call, codegen_var_assignment, etc.).
 */
