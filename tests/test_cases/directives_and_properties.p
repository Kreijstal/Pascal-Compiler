program directives_and_properties;

uses
  directives_and_properties_unit;

begin
  writeln(UnitValue);
  AddUnitValue(5);
  writeln(UnitValue);
  UnitValue := 3;
  writeln(UnitValue);
end.
