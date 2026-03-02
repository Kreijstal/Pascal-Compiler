program tdd_case_char_percent;

var
  c: char;
  s: string;

begin
  c := '%';
  case c of
    '%': s := '%';
  else
    s := 'x';
  end;
  writeln(s);
end.
