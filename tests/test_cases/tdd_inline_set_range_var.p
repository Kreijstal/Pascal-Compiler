program tdd_inline_set_range_var;

{$mode objfpc}

var
  AdjColours: set of 0..255;
  Colour: Byte;

begin
  AdjColours := [];
  Include(AdjColours, 3);
  Colour := 3;
  if Colour in AdjColours then
    Writeln('ok')
  else
    Writeln('missing');
end.
