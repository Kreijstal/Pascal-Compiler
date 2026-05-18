program regr_inline_operator_cross_unit;
{$mode objfpc}

uses regr_inline_operator_cross_unit_helper;

var
  A, B: TConstExprInt;
begin
  A.V := 1;
  B.V := 2;
  A.Opts := [poInline];

  if (A <= B) and not (A >= B) and HasInlineOpt(A) then
    WriteLn('OK')
  else
    WriteLn('BUG');
end.
