# AST Ownership Rule

## Motivation

The semantic check and codegen passes routinely *rewrite* AST nodes in
place: a syntactic shape that the parser produced is replaced by a
different shape that better reflects the resolved semantics
(constant-folded literals, implicit `Self.` qualifiers, scoped-enum
literals, `proccall -> EXIT` lowerings, and so on). The struct types
`struct Expression` and `struct Statement` are *tagged unions*: every
node carries `type` plus an `expr_data` / `stmt_data` union whose
storage is reinterpreted according to `type`.

In-place rewriting is therefore three logically distinct operations:

1. **Reclaim** every heap-owned field reachable through the *old*
   tag's payload (sub-Expressions, strdup'd identifiers, lists, KgpcType
   references, etc).
2. **Switch** `node->type` to the new tag.
3. **Initialize** the *new* tag's payload.

If step 1 is skipped, every heap allocation reachable from the old
payload leaks. If step 1 frees something that step 3 still needs
(typically a KgpcType pointer that the new node will re-share), it
becomes a use-after-free instead.

This file documents the invariant the codebase relies on and lists
where it has been violated, so a future audit can keep finding them.

## The rule

> **Before assigning to `node->type` (or to any union field of a tagged
> union node), every heap-owned field currently reachable through the
> old tag's union payload must either be (a) destroyed, or (b) retained
> separately and explicitly transferred to its new owner.**

Helpers that encapsulate step 1 for a given tag already exist; use them
instead of open-coding `destroy_expr` + `free`:

| Old tag | Helper |
| --- | --- |
| `EXPR_RECORD_ACCESS` | `record_access_clear_payload(expr, destroy_record_expr)` in `SemCheck_Expr_Types_recordaccess.c` |
| `EXPR_FUNCTION_CALL` | `semcheck_reset_function_call_cache(expr)` (cache fields only) — the heavy fields must still be handled by `destroy_expr` semantics or hand-rolled |

For tags without a helper today (`EXPR_VAR_ID`, `EXPR_ARRAY_ACCESS`,
`STMT_PROCEDURE_CALL`, ...) the rewrite site is expected to free fields
inline before the type switch. When the same pattern recurs in two
places, factor a helper rather than copying the inline frees.

### Borrowed KgpcType pointers

`record_kgpc_type` and similar locals returned by
`semcheck_expr_with_type` are *borrowed* references owned by the
checked subexpression's `resolved_kgpc_type` slot. If that
subexpression is destroyed as part of the rewrite, the borrowed
reference dangles. When the new payload still needs that type, the
rewrite must `kgpc_type_retain` it before the destroy, then transfer
it to the new owner (e.g. via
`semcheck_expr_set_resolved_kgpc_type_shared`), then release.

### Stmt rewriting

For `struct Statement` the union is `stmt_data`. Exactly the same rule
applies: see `5c4f5fb9 proccall -> EXIT transform: reclaim union fields
before switching stmt->type` for an example where a `STMT_PROCEDURE_CALL`
was switched to `STMT_EXIT` and the old call's heap fields had to be
freed first.

## Past violations (regression catalogue)

Every one of these commits fixed an instance of the rule above. They
are listed so future readers can see the shape of the bug and confirm
they understand it before extending the rewrite logic:

- `bcb33582` &mdash; semcheck_arrayaccess pointer-deref shortcut:
  destroy bypassed deref wrapper (rewrite did not free the discarded
  `EXPR_POINTER_DEREF` wrapper after hoisting its child up).
- `5c4f5fb9` &mdash; proccall -> EXIT transform: reclaim union fields
  before switching `stmt->type` (heap fields in `procedure_call_data`
  were freed *after* the type switch, when the union no longer
  represented them).
- `242c3c5a` &mdash; funccall_method implicit-Self rewrite: destroy
  original arg before overwriting (the argument-list slot was
  overwritten with a synthesized Self-qualified expression while the
  old `EXPR_VAR_ID` was still its owner).
- `ae992752` &mdash; codegen enum RTTI: free label on already-emitted
  continue path (codegen-side reuse where the continue path skipped a
  label-free already done by the fall-through path).
- `c85716e3` &mdash; generic-spec teardown: own freshly-cloned record +
  reclaim nested_type_decls (a freshly cloned record was retained by
  one slot and orphaned by another).
- `76317c99` &mdash; convert_method_impl: destroy the orphaned tree
  when bailing on generic templates (an early return left an allocated
  subtree dangling).
- `86242495` / `0af8c69a` &mdash; same-shape "free prior value before
  reassignment" fixes inside function-call data.

## Current fix

`semcheck_recordaccess` resolves chained scoped-enum literals such as
`UnitName.TypeName.Literal` by re-tagging the outer `EXPR_RECORD_ACCESS`
node as an `EXPR_INUM` holding the ordinal value. The pre-fix
sequence at the end of `semcheck_recordaccess` was:

```c
if (record_type == ENUM_TYPE)
{
    ...
    if (semcheck_resolve_scoped_enum_literal(symtab, enum_type_name, field_id, &enum_value))
    {
        expr->type = EXPR_INUM;          /* switch */
        expr->expr_data.i_num = enum_value; /* overwrite union with int */
        ...
    }
}
```

This left the previous `record_access_data.record_expr` (the inner
`EXPR_RECORD_ACCESS` for `UnitName.TypeName`) and its strdup'd
`field_id` ("Literal") allocated but unreachable. Under
`-Db_sanitize=address` on `tdd_scoped_enum_qualified_import_conflict.p`
the leak surfaced as a 344-byte `Expression` direct leak plus a 7-byte
field-id direct leak plus a 15-byte indirect leak on the inner's
strdup'd qualified id, repeated twice for the two scoped-enum
references in that program.

The fix calls `record_access_clear_payload(expr, /*destroy_record_expr=*/1)`
before the `expr->type = EXPR_INUM` assignment, and retains
`record_kgpc_type` across the destroy so the subsequent
`semcheck_expr_set_resolved_kgpc_type_shared` does not see a dangling
borrowed reference.

Regression test: `tests/test_cases/tdd_scoped_enum_record_access_no_leak.p`
(must be run under the ASan build,
`meson setup build-asan -Db_sanitize=address && meson test -C build-asan`,
to detect a future re-introduction of the leak).
