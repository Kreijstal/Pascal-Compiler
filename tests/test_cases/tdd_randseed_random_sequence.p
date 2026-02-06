program tdd_randseed_random_sequence;
var
  i: integer;
begin
  RandSeed := 42;
  writeln('seed0=', RandSeed);
  for i := 1 to 5 do
    writeln(Random(36));
  writeln('seed_after=', RandSeed);
end.
