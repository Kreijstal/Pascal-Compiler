program sincos_procedure;

uses Math;

var
  s, c: real;
begin
  SinCos(Pi / 6, s, c);
  writeln('SinPi6=', s:0:4);
  writeln('CosPi6=', c:0:4);
  SinCos(Pi / 4, s, c);
  writeln('SinPi4=', s:0:4);
  writeln('CosPi4=', c:0:4);
end.
