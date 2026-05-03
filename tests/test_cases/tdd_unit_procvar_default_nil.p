unit tdd_unit_procvar_default_nil;
{$mode objfpc}

interface

type
  TCallback = procedure(Value: LongInt);

var
  Callback: TCallback;

procedure Run;

implementation

procedure Run;
begin
  if Assigned(Callback) then
    Callback(1)
  else
    WriteLn('nil');
end;

end.
