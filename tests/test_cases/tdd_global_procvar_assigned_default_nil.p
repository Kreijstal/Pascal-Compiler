program tdd_global_procvar_assigned_default_nil;
{$mode objfpc}

type
  TCallback = procedure(Value: LongInt);

var
  Callback: TCallback;

begin
  if Assigned(Callback) then
    Callback(1)
  else
    WriteLn('nil');
end.
