program tdd_pp_bootstrap_longbool_boolean_assign;

{ Regression test for sysutils.pp:869 RenameFile.

  Two units both declare TwinSym:
    - helper unit's public interface: TwinSym(x: LongInt): WINBOOL (= LongBool)
    - shadow unit's implementation-only: TwinSym(x: LongInt): LongInt

  When the program uses both, overload resolution must pick the public
  WINBOOL version so `Result: Boolean := TwinSym(x)` typechecks the way
  FPC's `Result := MoveFileW(...)` does inside sysutils.pp.  Before the
  fix the private LongInt shadow leaks across units and the assignment
  fails with "incompatible types in assignment for Result (lhs: Boolean,
  rhs: LongInt)". }

uses tdd_pp_bootstrap_longbool_boolean_assign_shadow,
     tdd_pp_bootstrap_longbool_boolean_assign_helper;

function CheckFlag(x: LongInt): Boolean;
begin
    Result := TwinSym(x);
end;

begin
    ShadowDummy;
    if CheckFlag(1) and (not CheckFlag(0)) then
        WriteLn('OK')
    else
        WriteLn('FAIL');
end.
