{ Regression: `Exit(scalar)` from a record-returning function where the
  return type has an `operator := (scalar) : Record` overload.

  Bug: SemCheck's STMT_EXIT path used to rewrite `exit(0)` into
  `exit(scalar__op_assign(0))` correctly, but passed
  `&return_expr->resolved_kgpc_type` as the rewriter's source-type
  tracker.  The rewriter writes the call's record return type back
  through that pointer, clobbering the integer literal's
  resolved_kgpc_type with the record type.  Codegen then saw an
  EXPR_INUM whose `resolved_kgpc_type` was a 16-byte record and tried
  to pass it as a by-value record argument, failing with
  "Unsupported record argument expression". }

{$mode objfpc}

program record_op_result_passed_by_value;

type
  TBits = record
    a, b : qword;
  end;

operator := (const u : qword) : TBits;
begin
  result.a := u;
  result.b := u + 1;
end;

function GetBits : TBits;
begin
  exit(qword(42));   { Implicit qword -> TBits via op_assign }
end;

var
  r : TBits;
begin
  r := GetBits;
  WriteLn('a=', r.a, ' b=', r.b);
end.
