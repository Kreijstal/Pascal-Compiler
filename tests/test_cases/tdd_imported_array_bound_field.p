program tdd_imported_array_bound_field;

{$mode objfpc}

uses tdd_imported_array_bound_field_layout;

var
  State: TRegisterState;

begin
  State := TRegisterState.Create;
  State.Registers[0] := 0;
  State.Registers[1] := 1;
  if 1 in State.Seen then
    Writeln('overlap')
  else
    Writeln('ok');
end.
