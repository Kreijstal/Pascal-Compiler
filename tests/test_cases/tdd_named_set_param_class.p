{$mode objfpc}
program tdd_named_set_param_class;

type
  TVarState = (vsRead, vsWritten);
  TVarStateFlag = (vsfMustBeValid);
  TVarStateFlags = set of TVarStateFlag;

  TNode = class
  end;

procedure SetVarState(P: TNode; NewState: TVarState; Flags: TVarStateFlags);
begin
  if (P <> nil) and (NewState = vsRead) and (vsfMustBeValid in Flags) then
    WriteLn('ok');
end;

var
  N: TNode;
begin
  N := TNode.Create;
  SetVarState(N, vsRead, [vsfMustBeValid]);
end.
