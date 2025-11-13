program random_seed_function;
var
  prevSeed: longint;
  r1, r2: longint;
begin
  SetRandSeed(12345);
  prevSeed := RandSeed;
  writeln('Seed1=', prevSeed);
  r1 := Random(1000);
  r2 := Random(1000);
  writeln('Values1=', r1, ',', r2);
  SetRandSeed(prevSeed);
  writeln('Seed2=', RandSeed);
  writeln('Values2=', Random(1000), ',', Random(1000));
end.
