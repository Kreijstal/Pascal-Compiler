{$mode objfpc}
program for_in_string;

var
  S: string;
  C: char;
  Sum: Integer;

begin
  S := 'abc';
  Sum := 0;
  for C in S do
    Sum := Sum + Ord(C);
  writeln(Sum);
end.
