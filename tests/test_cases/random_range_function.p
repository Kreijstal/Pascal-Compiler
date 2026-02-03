program random_range_function;
var
  val: longint;
begin
  RandSeed := 0;
  val := RandomRange(-5, 5);
  writeln('Range1=', val);
  val := RandomRange(10, 20);
  writeln('Range2=', val);
end.
