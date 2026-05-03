program tdd_static_class_procvar_assignment;
{$mode objfpc}

type
  TCallback = procedure(Value: LongInt);

  TState = class
    class procedure Run(Value: LongInt); static;
  end;

var
  Callback: TCallback = nil;

class procedure TState.Run(Value: LongInt);
begin
  WriteLn(Value);
end;

begin
  Callback := @TState.Run;
  if Assigned(Callback) then
    Callback(5);
end.
