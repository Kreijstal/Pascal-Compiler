program cot_function;
var
  r: real;
begin
  r := Cot(Pi / 4);
  writeln('CotPi4=', r:0:4);
  r := Cot(Pi / 6);
  writeln('CotPi6=', r:0:4);
  r := Cot(Pi / 3);
  writeln('CotPi3=', r:0:4);
end.
