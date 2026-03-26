{ Regression test: scoped enum IN set expressions
  Verifies that scoped enum values (EnumType.Value) work correctly
  in IN expressions (set membership test).

  This pattern is used in heaptrc.pp (CheckFlag.InsideLock in cf).
}
{$mode objfpc}
{$SCOPEDENUMS ON}
program regression_scoped_enum_in_expr;

type
  TFlag = (None, Active, Locked);

var
  flags: set of TFlag;
begin
  flags := [];
  Include(flags, TFlag.Active);
  Include(flags, TFlag.Locked);

  if TFlag.Active in flags then
    WriteLn('Active is set')
  else
    WriteLn('Active is NOT set');

  if TFlag.None in flags then
    WriteLn('None is set')
  else
    WriteLn('None is NOT set');

  if TFlag.Locked in flags then
    WriteLn('Locked is set')
  else
    WriteLn('Locked is NOT set');
end.
