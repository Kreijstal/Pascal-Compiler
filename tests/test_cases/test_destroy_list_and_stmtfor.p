program TestDestroyPaths;
uses SysUtils;

type
  TDummyRec = record A,B: Integer; end;
var
  i: Integer;
  R: TDummyRec;
begin
  for i := 1 to 3 do begin end;
  R.A := 1; R.B := 2;
  Writeln(R.A + R.B);
end.
