program random_real_function;
begin
  SetRandSeed(123456789);
  writeln('RandomNoParam=', Random:0:6);
  writeln('RandomReal=', Random(3.5):0:6);
  writeln('RandomInt=', Random(10));
end.
