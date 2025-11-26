program ClassDemo;

type
  TMyClass = class
    MyInt: Integer;
    procedure MyMethod;
  end;

procedure TMyClass.MyMethod;
begin
  WriteLn(MyInt + 10);
end;

var
  Obj: TMyClass;
begin
  Obj := TMyClass.Create;
  Obj.MyInt := 5;
  Obj.MyMethod;
end.
