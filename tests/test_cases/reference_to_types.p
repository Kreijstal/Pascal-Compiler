program ReferenceToTypesTest;

type
  TIntFunc = reference to function(x: Integer): Integer;
  TVoidProc = reference to procedure(x: Integer);

function AddOne(x: Integer): Integer;
begin
  AddOne := x + 1
end;

procedure PrintValue(x: Integer);
begin
  WriteLn('Value: ', x)
end;

var
  F: TIntFunc;
  P: TVoidProc;
  Result: Integer;

begin
  { Reference to types work with named functions }
  F := AddOne;
  Result := F(5);
  WriteLn('F(5) = ', Result);
  
  P := PrintValue;
  P(42);
  
  WriteLn('Reference to types test passed');
end.
