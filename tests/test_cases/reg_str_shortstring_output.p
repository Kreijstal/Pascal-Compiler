{$mode objfpc}
program reg_str_shortstring_output;
var
  i: Int64;
  s: ShortString;
begin
  i := 42;
  Str(i, s);
  WriteLn(s);
end.
