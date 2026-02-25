{$mode objfpc}
program tdd_qualified_range_unit;

uses tdd_qualified_range_unit_types;

const
  L = Low(tdd_qualified_range_unit_types.TValueRelationship);
  H = High(tdd_qualified_range_unit_types.TValueRelationship);

begin
  writeln(L);
  writeln(H);
end.
