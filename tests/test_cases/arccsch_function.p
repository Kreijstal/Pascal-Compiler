program arccsch_function;
var
  r: real;
begin
  r := ArcCsch(1.0);
  writeln('ArcCschOne=', r:0:4);
  r := ArcCsch(0.5);
  writeln('ArcCschHalf=', r:0:4);
  r := ArcCsch(2.0);
  writeln('ArcCschTwo=', r:0:4);
end.
