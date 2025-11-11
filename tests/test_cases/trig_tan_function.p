program trig_tan_function;
var
  x: real;
begin
  x := Tan(0.0);
  writeln('TanZero=', x:0:2);
  x := Tan(DegToRad(45.0));
  writeln('Tan45=', x:0:2);
end.
