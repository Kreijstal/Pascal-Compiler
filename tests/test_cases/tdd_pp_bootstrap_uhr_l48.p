{ Regression test: assigning a CHAR via typecast to a SHORTSTRING function
  result must promote the char to a string first.  Previously the codegen
  for `result := char(int_expr)` (an EXPR_TYPECAST whose inner type is
  CHAR_TYPE) routed through codegen_call_string_to_shortstring with
  value_reg holding the char's ordinal, which kgpc_string_to_shortstring
  then dereferenced as a pointer (SIGSEGV at runtime).  This pattern is
  used by FPC's pexpr.get_stringconst, so the resulting pp_bootstrap
  crashed with "Compilation raised exception internally" whenever it
  parsed `external <string>` for `tests/test_cases/unix_http_request.p`. }
{$mode objfpc}
{$H-}
program tdd_pp_bootstrap_uhr_l48;

type
  TVal = record
    svalue : longint;
  end;

  TNode = class
    value : TVal;
  end;

function get_stringconst : string;
var
  p : TNode;
begin
  get_stringconst := '';
  p := TNode.Create;
  p.value.svalue := 99; { 'c' }
  get_stringconst := char(p.value.svalue);
  p.Free;
end;

begin
  writeln('[', get_stringconst, ']');
end.
