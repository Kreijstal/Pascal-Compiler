program test_val_shortstring;
{$mode objfpc}
var
  ss: ShortString;
  val1: Integer;
  val2: Real;
  code: Integer;
begin
  ss := '42';
  Val(ss, val1, code);
  WriteLn(val1);
  WriteLn(code);
  ss := '3.14';
  Val(ss, val2, code);
  WriteLn(code);
  ss := 'abc';
  Val(ss, val1, code);
  WriteLn(code > 0);
end.
