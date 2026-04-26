program shortstring_indexed_assignment_repro;
{$mode objfpc}
var
  s: string;
begin
  s[1] := 'a';
  s[2] := 'b';
  s[3] := 'c';
  s[0] := Chr(3);
  WriteLn(s);
end.
