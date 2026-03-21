program reg_imported_qualified_const_lowhigh;

{$mode objfpc}

uses reg_imported_qualified_const_lowhigh_unit;

const
  AliasLow = reg_imported_qualified_const_lowhigh_unit.LowAlias;
  AliasEq = reg_imported_qualified_const_lowhigh_unit.EqAlias;
  AliasHigh = reg_imported_qualified_const_lowhigh_unit.HighAlias;

begin
  WriteLn(Ord(AliasLow));
  WriteLn(Ord(AliasEq));
  WriteLn(Ord(AliasHigh));
end.
