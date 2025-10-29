program SetOperations;
var
  odds: set of 1..10;
  evens: set of 1..10;
  mix: set of 1..10;
  result: set of 1..10;
  hasFour: boolean;
begin
  odds := [1, 3, 5, 7, 9];
  evens := [2, 4, 6, 8, 10];
  mix := [3, 4, 5];

  result := odds + mix;
  hasFour := 4 in result;
  if hasFour then
    writeln('flag-has-4')
  else
    writeln('flag-missing-4');

  if 4 in result then
    writeln('union-has-4')
  else
    writeln('union-missing-4');

  result := mix * evens;
  if 4 in result then
    writeln('intersection-has-4')
  else
    writeln('intersection-missing-4');

  if 2 in [1, 2, 3] then
    writeln('constructor-has-2')
  else
    writeln('constructor-missing-2');
end.
