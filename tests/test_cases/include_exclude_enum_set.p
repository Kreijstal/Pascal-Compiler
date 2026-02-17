program test_include_enum_set;
{$mode objfpc}

type
  TColor = (Red, Green, Blue, Yellow, Cyan, Magenta);
  TColorSet = set of TColor;

var
  colors: TColorSet;
begin
  colors := [];
  Include(colors, Red);
  Include(colors, Blue);
  Include(colors, Yellow);
  
  if Red in colors then Write('R');
  if Green in colors then Write('G');
  if Blue in colors then Write('B');
  if Yellow in colors then Write('Y');
  if Cyan in colors then Write('C');
  if Magenta in colors then Write('M');
  WriteLn;
  
  Exclude(colors, Blue);
  
  if Red in colors then Write('R');
  if Green in colors then Write('G');
  if Blue in colors then Write('B');
  if Yellow in colors then Write('Y');
  if Cyan in colors then Write('C');
  if Magenta in colors then Write('M');
  WriteLn;
end.
