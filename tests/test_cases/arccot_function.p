program arccot_function;
var
  r: real;
begin
  r := ArcCot(1.0);
  writeln('ArcCotOne=', r:0:4);
  r := ArcCot(0.0);
  writeln('ArcCotZero=', r:0:4);
  r := ArcCot(-1.0);
  writeln('ArcCotMinusOne=', r:0:4);
end.
