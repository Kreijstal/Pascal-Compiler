{ Test: SetLength on Result that is a dynamic array }
{ BUG: SetLength(Result, N) fails when function returns dynamic array }
{$mode objfpc}
program setlength_result_dynarray;

type
  TIntArray = array of Integer;

function MakeArray(N: Integer): TIntArray;
var
  I: Integer;
begin
  SetLength(Result, N);
  for I := 0 to N - 1 do
    Result[I] := I;
end;

var
  Arr: TIntArray;
begin
  Arr := MakeArray(3);
  WriteLn(Arr[2]);
end.
