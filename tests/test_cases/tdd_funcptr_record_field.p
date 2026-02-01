{ TDD test for function pointer field in record
  This test verifies that:
  1. Record fields with function pointer types are properly recognized as functions (not procedures)
  2. Assignment to function pointer field with @FuncName works correctly
  3. Calling the function through the field works correctly
  
  Verified: Passes with FPC and KGPC
}
{$mode objfpc}
program tdd_funcptr_record_field;

type
  TFunc = function: Integer;

  TRec = record
    F: TFunc;
  end;

var
  R: TRec;

function MyFunc: Integer;
begin
  Result := 42;
end;

begin
  R.F := @MyFunc;
  WriteLn(R.F());
end.
