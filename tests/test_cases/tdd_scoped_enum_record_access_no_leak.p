{ Regression test: semcheck_recordaccess used to overwrite the union
  storage of an EXPR_RECORD_ACCESS when rewriting it to EXPR_INUM for a
  resolved scoped-enum literal (e.g. UnitName.TypeName.Literal) without
  first reclaiming the record_expr subtree and field_id strdup.  The
  inner Expression and its strdup'd qualified id leaked.

  The bug was reachable through the qualified-record-access -> enum
  literal resolution path at the bottom of semcheck_recordaccess
  (record_type == ENUM_TYPE branch), reproducible with any two-level
  qualified scoped-enum reference like UnitName.TypeName.Literal where
  TypeName is itself a record-access chain.

  Under AddressSanitizer (b_sanitize=address) the original bug
  manifested as direct leaks of the inner Expression struct and the
  field_id strdup; the fix calls record_access_clear_payload before
  switching expr->type. }
program tdd_scoped_enum_record_access_no_leak;

{$mode objfpc}

uses tdd_alias_scoped_enum_shadow_import_unit;

const
  CLocalEndian = ObjPas.TEndian.Little;

begin
  if CLocalEndian = ObjPas.TEndian.Little then
    writeln('little')
  else
    writeln('big')
end.
