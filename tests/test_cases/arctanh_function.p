program arctanh_function;
var
  r: real;
begin
  r := ArcTanh(0.0);
  writeln('ArcTanhZero=', r:0:1);
  r := ArcTanh(0.5);
  writeln('ArcTanhHalf=', r:0:4);
  r := ArcTanh(-0.5);
  writeln('ArcTanhMinusHalf=', r:0:4);
end.
