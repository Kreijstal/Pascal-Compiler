program power_function;
var
  r: real;
begin
  r := Power(2.5, 3.0);
  writeln('PowerReal=', r:0:4);
  r := Power(4.0, 0.5);
  writeln('PowerRoot=', r:0:4);
  r := Power(5, 3);
  writeln('PowerInt=', r:0:4);
end.
