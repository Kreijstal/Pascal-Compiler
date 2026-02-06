program test_metaclass;
{$MODE OBJFPC}
{$MODESWITCH class}

type
  TMyClass = class
    class function ClassName: String;
  end;

  TMyClassRef = class of TMyClass;

class function TMyClass.ClassName: String;
begin
  Result := 'TMyClass';
end;

var
  c: TMyClassRef;
begin
  c := TMyClass;
  WriteLn(c.ClassName);
end.
