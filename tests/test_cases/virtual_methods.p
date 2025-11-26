program VirtualMethodsSimple;

type
  TMyClass = class
    MyInt: Integer;
    procedure MyVirtualMethod; virtual;
  end;

procedure TMyClass.MyVirtualMethod;
begin
  WriteLn('TMyClass shows MyInt + 10: ', MyInt + 10);
end;

var
  C: TMyClass;
begin
  C := TMyClass.Create;
  C.MyInt := 0;
  C.MyVirtualMethod;
  
  C.MyInt := 10;
  C.MyVirtualMethod;
end.