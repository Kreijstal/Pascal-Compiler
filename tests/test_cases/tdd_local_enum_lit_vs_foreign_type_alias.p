{ Regression: defcmp.pas's compare_defs_ext declares a function-local
  enum type whose literal `bchar` collides case-insensitively with the
  Windows unit's `BCHAR = word` type alias.  Without the codegen
  enum-literal registration distinguishing the type-namespace TYPE from
  the value-namespace CONST, codegen_register_local_types' FindSymbol
  call returns the foreign TYPE and skips pushing the local CONST,
  leaving `bchar` unbound and ending in
  `ERROR: Unresolved non-local symbol bchar reached codegen fallback.`

  Pascal puts types and value-constants in separate namespaces; the
  function-local enum literal must register as a CONST alongside the
  imported TYPE alias. }
program tdd_local_enum_lit_vs_foreign_type_alias;
{$mode objfpc}

uses tdd_local_enum_lit_vs_foreign_type_alias_unit; { brings BCHAR (type) into scope }

procedure show;
type
  tordtype3 = (oa3, ob3, oc3, od3);
  tbasedef3 = (bvoid3, bchar, bint3, bbool3); { local enum literal `bchar` }
const
  basedeftbl3 : array[tordtype3] of tbasedef3 = (bvoid3, bchar, bchar, bint3);
var
  t : tordtype3;
begin
  for t := oa3 to od3 do
    writeln('val=', Ord(basedeftbl3[t]));
end;

begin
  show;
end.
