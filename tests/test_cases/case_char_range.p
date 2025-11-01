program CaseCharRange;
var
  ch: char;
begin
  ch := '5';
  case ch of
    '-', '0'..'9': writeln('numeric');
  else
    writeln('other');
  end;

  ch := 'A';
  case ch of
    '-', '0'..'9': writeln('numeric');
  else
    writeln('other');
  end;
end.
