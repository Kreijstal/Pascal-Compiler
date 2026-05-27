unit tdd_local_enum_lit_vs_foreign_type_alias_unit;
{$mode objfpc}
interface

{ This unit publishes a TYPE alias whose name will collide
  case-insensitively with an enum literal declared LOCALLY inside a
  procedure in the importing unit.  Pascal keeps types and value-
  constants in distinct namespaces; the local enum literal must still
  resolve to the CONST regardless of the foreign-unit TYPE alias being
  visible. }
type
  BCHAR = byte;

implementation

end.
