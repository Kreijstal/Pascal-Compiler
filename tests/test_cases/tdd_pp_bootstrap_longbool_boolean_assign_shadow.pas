unit tdd_pp_bootstrap_longbool_boolean_assign_shadow;

{ Mirrors FPC's rtl/win/sysos.inc which declares an
  implementation-section MoveFileW returning longint inside the System
  unit.  Importers must NOT see this private overload — interface-public
  overloads with the same name must shadow it. }

interface

procedure ShadowDummy;

implementation

function TwinSym(x: LongInt): LongInt;
begin
    TwinSym := x;
end;

procedure ShadowDummy;
begin
end;

end.
