program random_function;
var
  r: real;
  i: longint;
begin
  RandSeed := 0;
  r := Random;
  writeln('RandReal=', r:0:8);
  i := Random(10);
  writeln('RandInt10=', i);
  i := Random(1);
  writeln('RandInt1=', i);
  Randomize;
  writeln('Randomized=1');
end.
