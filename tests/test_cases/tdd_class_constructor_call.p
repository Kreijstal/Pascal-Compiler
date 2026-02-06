{$mode objfpc}
{ TDD test: Class constructor call from class reference should work }
program tdd_class_constructor_call;

type
  TMyClass = class
  public
    Value: Integer;
    constructor Create(V: Integer);
  end;

constructor TMyClass.Create(V: Integer);
begin
  Value := V;
end;

var
  Obj: TMyClass;
begin
  Obj := TMyClass.Create(42);
  WriteLn(Obj.Value);
  Obj.Free;
end.
