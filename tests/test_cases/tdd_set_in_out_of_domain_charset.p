program t;
var
  e: Longint;
begin
  { Set with elements up to 85 forces a char-set (>31) representation.
    Testing a large out-of-domain element (10000) must yield FALSE,
    not read past the set storage. }
  e := 10000;
  if e in [32, 33, 84, 85] then
    writeln('WRONG: 10000 in set')
  else
    writeln('OK: 10000 not in set');
  e := 33;
  if e in [32, 33, 84, 85] then
    writeln('OK: 33 in set')
  else
    writeln('WRONG: 33 not in set');
  e := 50;
  if e in [32, 33, 84, 85] then
    writeln('WRONG: 50 in set')
  else
    writeln('OK: 50 not in set');
end.
