{
  Regression test: set literal of enum with explicit values must use
  the declared values, not the enum-literal positions.

  Bug: KGPC's contextual-enum and scoped-enum resolvers walked
  alias->enum_literals and returned the list index, ignoring the
  parallel alias->enum_values list of explicit ordinals. As a result,
  a set literal like [in_typeinfo_x] (= 43) was encoded with bit 2
  (the position of in_typeinfo_x in the enum) and `l in [...]` failed.

  Manifested in pp_bootstrap as `Identifier not found "TypeInfo"`
  because pexpr.pas's in_typeinfo_x case in statement_syssym is gated
  by `if (l in [in_typeinfo_x,in_gettypekind_x,in_ismanagedtype_x])`.
  The miscompiled set literal caused that test to always be false,
  so KGPC-built pp_bootstrap reported TypeInfo unknown.
}
program reg_set_of_explicit_enum;

type
  tinlinenumber=(
     in_none              = -1,
     in_lo_word           = 1,
     in_typeinfo_x        = 43,
     in_gettypekind_x     = 96,
     in_ismanagedtype_x   = 99
  );
var
  l: tinlinenumber;
begin
  l := in_typeinfo_x;
  if l in [in_typeinfo_x, in_gettypekind_x, in_ismanagedtype_x] then
    writeln('ok-43')
  else
    writeln('FAIL-43');
  l := in_gettypekind_x;
  if l in [in_typeinfo_x, in_gettypekind_x, in_ismanagedtype_x] then
    writeln('ok-96')
  else
    writeln('FAIL-96');
  l := in_lo_word;
  if l in [in_typeinfo_x, in_gettypekind_x, in_ismanagedtype_x] then
    writeln('FAIL-1')
  else
    writeln('ok-not-in');
end.
