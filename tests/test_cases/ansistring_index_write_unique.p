program ansistring_index_write_unique;

{$H+}

var
  s, t: string;

begin
  s := 'abc';
  t := s;
  t[1] := 'A';
  writeln('s=', s);
  writeln('t=', t);
end.
