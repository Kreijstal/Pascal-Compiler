program tdd_private_underscore_method_procvar;
{$mode objfpc}

type
  TState = class
  private
    function _Value: LongInt;
  public
    function Run: LongInt;
  end;

function TState._Value: LongInt;
begin
  Result := 3;
end;

function TState.Run: LongInt;
const
  Callbacks: array[0..0] of Pointer = (@TState._Value);
begin
  if Assigned(Callbacks[0]) then
    Result := 3
  else
    Result := 0;
end;

var
  State: TState;

begin
  State := TState.Create;
  WriteLn(State.Run);
  State.Free;
end.
