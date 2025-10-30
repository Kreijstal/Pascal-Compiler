program RealArithmetic;
var
  a, b, c: Real;
begin
  a := 1.5;
  b := 2.25;
  c := a + b;
  writeln(c);
  c := a * b;
  writeln(c);
  if a < b then
    writeln('less')
  else
    writeln('not less');
  if b > a then
    writeln('more')
  else
    writeln('not more');
  c := a - b;
  writeln(c);
  c := b / a;
  writeln(c);
end.
