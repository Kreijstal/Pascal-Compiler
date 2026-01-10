program setstring_pchar;

var
  S: string;

begin
  SetString(S, PChar('abcd'), 3);
  writeln(S);
end.
