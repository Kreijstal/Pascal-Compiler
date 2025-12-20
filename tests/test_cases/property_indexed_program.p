program property_indexed_program;

uses property_indexed_unit;

begin
  G[true] := 11;
  writeln(IndexVal[true]);
end.
