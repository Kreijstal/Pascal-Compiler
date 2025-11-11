program sech_function;
var
  r: real;
begin
  r := Sech(0.0);
  writeln('SechZero=', r:0:4);
  r := Sech(0.5);
  writeln('SechHalf=', r:0:4);
  r := Sech(1.0);
  writeln('SechOne=', r:0:4);
end.
