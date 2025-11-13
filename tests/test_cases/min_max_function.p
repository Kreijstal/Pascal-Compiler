program min_max_function;

uses Math;

var
  i: integer;
  l: longint;
begin
  i := Max(3, -4);
  writeln('MaxInt=', i);
  i := Min(3, -4);
  writeln('MinInt=', i);
  l := Max(10000000000, -5);
  writeln('MaxLong=', l);
  l := Min(-10000000000, 5);
  writeln('MinLong=', l);
end.
