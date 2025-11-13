program upcase_function;

var
  c: char;
  s: string;

begin
  c := UpCase('a');
  writeln('Case1=', c);

  c := 'z';
  writeln('Case2=', UpCase(c));

  s := 'abc';
  s[2] := UpCase(s[2]);
  writeln('Case3=', s);
end.
