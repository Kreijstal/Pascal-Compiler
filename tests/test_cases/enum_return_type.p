{ Test enum type as function return type }
{ This tests that enum types are properly predeclared for function return types }
program enum_return_type;

type
    TColor = (Red, Green, Blue);

function GetDefaultColor: TColor;
begin
  Result := Green;
end;

var
  c: TColor;
begin
  c := GetDefaultColor;
  
  { Compare using Ord() for reliable comparison }
  if Ord(c) = Ord(Green) then
    WriteLn('Color is Green (correct)')
  else
    WriteLn('Color test FAILED');
end.
