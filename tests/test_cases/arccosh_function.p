program arccosh_function;
var
  r: real;
begin
  r := ArcCosh(1.0);
  writeln('ArcCoshOne=', r:0:1);
  r := ArcCosh(2.0);
  writeln('ArcCoshTwo=', r:0:4);
  r := ArcCosh(10.0);
  writeln('ArcCoshTen=', r:0:4);
end.
