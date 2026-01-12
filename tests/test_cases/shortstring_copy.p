{ Test: Copy with ShortString }
{ BUG: Copy may not work correctly with ShortString type }
{$mode objfpc}
program shortstring_copy;

var
  S: ShortString;
  R: ShortString;
begin
  S := 'Hello World';
  R := Copy(S, 1, 5);
  WriteLn(R);
end.
