{ Test scoped enums - {$SCOPEDENUMS ON} directive }
{ When enabled, enum values must be qualified with type name }
program scoped_enums;

{$SCOPEDENUMS ON}

type
  TColor = (Red, Green, Blue);

var
  c: TColor;
begin
  c := TColor.Red;
  WriteLn('Scoped enum: TColor.Red');
  
  c := TColor.Green;
  WriteLn('Scoped enum: TColor.Green');
  
  c := TColor.Blue;
  WriteLn('Scoped enum: TColor.Blue');
end.
