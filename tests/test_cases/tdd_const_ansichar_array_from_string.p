program tdd_const_ansichar_array_from_string;

const
  s = 'abc';
  a: array[1..length(s)] of AnsiChar = s;

begin
  if a[1] = 'a' then
    writeln('ok');
end.
