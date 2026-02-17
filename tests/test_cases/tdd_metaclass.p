program test_metaclass6;
{$MODE OBJFPC}
{$MODESWITCH class}

type
  TMyClass = class
    class function GetInfo(const x: Integer): Integer;
  end;

  TMyClassRef = class of TMyClass;

class function TMyClass.GetInfo(const x: Integer): Integer;
begin
  Result := x * 2;
end;

var
  c: TMyClassRef;
begin
  c := TMyClass;
  WriteLn(c.GetInfo(21));
end.
