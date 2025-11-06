program Repro;
var k, s: integer;
begin
  write('Enter two integers (k s): ');
  read(k);
  read(s);
  if k <> s then writeln('k (', k, ') <> s (', s, ') → copying s to k') else writeln('k = s (', k, ')');
  if k <> s then k := s;
  if k <> s then writeln('Still different: k=', k, ' s=', s) else writeln('Now equal: k=', k, ' s=', s);
end.
