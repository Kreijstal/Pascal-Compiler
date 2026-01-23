{ Test: WriteStr intrinsic function }
{ BUG: WriteStr(S, values...) intrinsic not implemented }
{$mode objfpc}
program writestr_intrinsic;

var
  S: string;
  I: Integer;
begin
  I := 42;
  WriteStr(S, 'Value: ', I);
  WriteLn(S);
end.
