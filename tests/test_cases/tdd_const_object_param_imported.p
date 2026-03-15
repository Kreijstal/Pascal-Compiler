{$mode objfpc}
program tdd_const_object_param_imported;

uses
  tdd_const_object_param_unit;

var
  Thing: TThing;
begin
  Thing.Init;
  UseThing(Thing);
end.
