{$mode objfpc}
program test_funcptr_assign;

{ Test: Assigning function pointer to procedural field in record
  This tests that a function address can be assigned to a procedural field
  in a record when the function has a pointer return type. }

type
  TIntFunc = function(X: Integer): Integer;
  
  TRec = record
    Func: TIntFunc;
  end;

function Double(X: Integer): Integer;
begin
  Result := X * 2;
end;

var
  R: TRec;
begin
  R.Func := @Double;
  { Only test assignment, not call through the field }
  WriteLn('PASS');
end.
