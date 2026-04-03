program tdd_nested_shortstring_return;
{$mode objfpc}
{$H-}

procedure Outer;
  function PadEnd(s: string; i: longint): string;
  begin
    if length(s) >= i then
      s := s + ' '
    else
      while length(s) < i do
        s := s + ' ';
    PadEnd := s;
  end;
var
  x: string;
begin
  x := PadEnd('', 0);
  Writeln('[', x, ']');
  Writeln(Length(x));
end;

begin
  Outer;
end.
