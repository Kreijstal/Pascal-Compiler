program arctan2_function;
var
  r: real;
begin
  r := ArcTan2(0.0, 1.0);
  writeln('ArcTan2_0_1=', r:0:4);
  r := ArcTan2(1.0, 0.0);
  writeln('ArcTan2_1_0=', r:0:4);
  r := ArcTan2(-1.0, -1.0);
  writeln('ArcTan2_-1_-1=', r:0:4);
end.
