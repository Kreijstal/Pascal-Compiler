program module_property_program;
{$mode objfpc}

{ Test: Module-level property in program context }
{ This is used in FPC system.pp: property cmdline:PAnsiChar read get_cmdline; }

var
  _internal_value: Integer;

function get_value: Integer;
begin
  Result := _internal_value;
end;

property my_value: Integer read get_value;

begin
  _internal_value := 42;
  writeln('Value via property: ', my_value);
end.
