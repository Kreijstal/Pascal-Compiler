program tdd_class_method_unqualified_virtual_self;
{$mode objfpc}

type
  TUtil = class
    class procedure Outer(Value: Integer); virtual;
    class procedure Inner(Value: Integer); virtual;
  end;

class procedure TUtil.Outer(Value: Integer);
begin
  Inner(Value + 5);
end;

class procedure TUtil.Inner(Value: Integer);
begin
  Writeln(Value);
end;

begin
  TUtil.Outer(37);
end.
