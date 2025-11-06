program string_implicit_concat;
const
  s1 = 'hello'#13#10'world';
  s2 = 'a'#66'c';
begin
  writeln(s1);
  writeln(s2);
end.
