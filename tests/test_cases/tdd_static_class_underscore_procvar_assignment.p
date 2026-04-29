program tdd_static_class_underscore_procvar_assignment;
{$mode objfpc}

type
  TCallback = procedure(Value: LongInt);

  TState = class
    class procedure _Run(Value: LongInt); static;
  end;

var
  Callback: TCallback = nil;

class procedure TState._Run(Value: LongInt);
begin
  WriteLn(Value);
end;

begin
  Callback := @TState._Run;
  if Assigned(Callback) then
    Callback(7);
end.
