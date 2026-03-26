{$mode objfpc}
program test_copy_dynarray;
type
  TIntArray = array of Integer;

procedure DoCopy;
var
  a, b: TIntArray;
begin
  SetLength(a, 3);
  a[0] := 10; a[1] := 20; a[2] := 30;
  b := Copy(a);
end;

begin
  { DoCopy is not called — we only verify Copy(a) type-checks }
  WriteLn('OK');
end.
