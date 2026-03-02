program pansistring_deref_addr;

type
  PMyString = ^AnsiString;

procedure Accept(p: PAnsiChar);
begin
  if p = nil then
    writeln('nil')
  else
    writeln(p^);
end;

var
  s: AnsiString;
  p: PMyString;
begin
  s := 'abc';
  p := @s;
  Accept(@p^[1]);
end.
