{ Regression: nested subprogram should not clobber dynarray return flags }
{$mode objfpc}
program reg_dynarray_exit_nested;

type
  TIntArray = array of Integer;

function Outer(N: Integer): TIntArray;
  function Inner: Integer;
  begin
    Inner := 1;
  end;
var
  I: Integer;
begin
  SetLength(Result, N);
  for I := 0 to N - 1 do
    Result[I] := I + 10;
  if Inner = 1 then
    Exit;
  SetLength(Result, 1);
  Result[0] := 99;
end;

var
  A: TIntArray;
begin
  A := Outer(3);
  WriteLn(A[0], ' ', A[2]);
end.
