program tdd_array_of_shortstring;
type
  TStringType = ShortString;
var
  SValues: array of TStringType;
begin
  SetLength(SValues, 2);
  SValues[0] := 'hello';
  SValues[1] := 'world';
  writeln(SValues[0]);
  writeln(SValues[1]);
end.
