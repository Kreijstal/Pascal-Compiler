program case_char_code_range;
var
  c: char;
begin
  c := #130;
  case c of
    #128..#255: writeln('match');
  else
    writeln('nomatch');
  end;
end.
