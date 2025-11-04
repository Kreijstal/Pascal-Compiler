program ProcedureTypesTest;

type
  TIntFunc = function(x: Integer): Integer;

function AddOne(x: Integer): Integer;
begin
  AddOne := x + 1
end;

var
  F: TIntFunc;
  Result: Integer;

begin
  F := AddOne;
  Result := F(5);
  WriteLn('F(5) = ', Result);
  WriteLn('Test passed');
end.
