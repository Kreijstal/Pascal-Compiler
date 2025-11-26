program missing_string_ops;
var
  s1, s2, s3: AnsiString;
begin
  s1 := 'hello';
  s2 := 'world';
  s3 := s1 + ' ' + s2;
  writeln(s3);
  if s3 = 'hello world' then
    writeln('Equal')
  else
    writeln('Not Equal');
end.
