{ Test: Assign nil to dynamic array Result }
{ BUG: Result := nil for dynamic array return type may fail }
{$mode objfpc}
program dynarray_nil_assign;

type
  TIntArray = array of Integer;

function GetEmpty: TIntArray;
begin
  Result := nil;
end;

var
  A: TIntArray;
begin
  A := GetEmpty;
  if A = nil then
    WriteLn('OK');
end.
