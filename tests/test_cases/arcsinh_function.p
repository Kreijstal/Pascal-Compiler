program arcsinh_function;
var
  r: real;
begin
  r := ArcSinh(0.0);
  writeln('ArcSinhZero=', r:0:1);
  r := ArcSinh(1.0);
  writeln('ArcSinhOne=', r:0:4);
  r := ArcSinh(-1.0);
  writeln('ArcSinhMinusOne=', r:0:4);
  r := ArcSinh(10.0);
  writeln('ArcSinhTen=', r:0:4);
end.
