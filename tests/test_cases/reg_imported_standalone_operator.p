{$mode objfpc}
program reg_imported_standalone_operator;

uses reg_imported_standalone_operator_unit;

var
  a, b, outValue: TCounter;
begin
  a.Value := 9;
  b.Value := 4;
  outValue := a - b;
  WriteLn(outValue.Value);
end.
