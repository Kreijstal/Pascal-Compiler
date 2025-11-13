program arcsin_arccos_function;
var
  r: real;
begin
  r := ArcSin(0.0);
  writeln('ArcSinZero=', r:0:2);
  r := ArcSin(1.0);
  writeln('ArcSinOne=', r:0:4);
  r := ArcCos(1.0);
  writeln('ArcCosOne=', r:0:2);
  r := ArcCos(0.0);
  writeln('ArcCosZero=', r:0:4);
end.
