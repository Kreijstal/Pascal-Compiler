program class_features;

type
  TMyClass = class
    Value: Integer;
    procedure Init(NewValue: Integer);
    procedure AddAndPrint(Delta: Integer);
  end;

var
  Obj: TMyClass;

procedure TMyClass.Init(NewValue: Integer);
begin
  Value := NewValue;
end;

procedure TMyClass.AddAndPrint(Delta: Integer);
begin
  Value := Value + Delta;
  WriteLn(Value);
end;

begin
  Obj.Init(5);
  Obj.AddAndPrint(10);
  Obj.AddAndPrint(-3);
end.
