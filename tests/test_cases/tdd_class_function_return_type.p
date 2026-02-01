{$mode objfpc}
program tdd_class_function_return_type;

{ Test that class function return types are correctly parsed and resolved.
  This tests the fix for class methods in class declarations not having
  their return types wrapped in PASCAL_T_RETURN_TYPE nodes. }

type
  TMyClass = class
  public
    class function GetName: ShortString;
    class function GetValue: Integer;
  end;

class function TMyClass.GetName: ShortString;
begin
  Result := 'MyClass';
end;

class function TMyClass.GetValue: Integer;
begin
  Result := 42;
end;

var
  S: ShortString;
  N: Integer;
begin
  S := TMyClass.GetName;
  N := TMyClass.GetValue;
  WriteLn(S);
  WriteLn(N);
end.
