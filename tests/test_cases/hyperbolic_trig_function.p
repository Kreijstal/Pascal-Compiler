program hyperbolic_trig_function;
var
  r: real;
begin
  r := Sinh(0.0);
  writeln('SinhZero=', r:0:1);
  r := Sinh(1.0);
  writeln('SinhOne=', r:0:4);

  r := Cosh(0.0);
  writeln('CoshZero=', r:0:1);
  r := Cosh(1.0);
  writeln('CoshOne=', r:0:4);

  r := Tanh(0.0);
  writeln('TanhZero=', r:0:1);
  r := Tanh(1.0);
  writeln('TanhOne=', r:0:4);
end.
