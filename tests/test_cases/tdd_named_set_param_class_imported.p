{$mode objfpc}
program tdd_named_set_param_class_imported;

uses
  tdd_named_set_param_class_unit;

var
  N: TNode;
begin
  N := TNode.Create;
  SetVarState(N, vsRead, [vsfMustBeValid]);
end.
