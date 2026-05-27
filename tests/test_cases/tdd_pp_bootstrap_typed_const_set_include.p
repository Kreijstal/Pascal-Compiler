{ Regression: FPC declares writeable set typed-constants like

    LeadBytes: set of AnsiChar = [];

  in rtl/objpas/sysutils/sysstrh.inc, then mutates them at unit init
  time via Include() (rtl/win/sysutils.pp:1357).  This is FPC's default
  {$WRITEABLECONST ON} behaviour: typed consts (`const X: T = init;`)
  are initialised once but freely writable thereafter.

  KGPC's var-id check used to require HASHTYPE_VAR for any mutating
  position and rejected HASHTYPE_CONST with
  "cannot assign \"X\", is not a scalar variable!" — even when the
  symbol was actually a typed set const.  Now `had_type_annotation` is
  propagated from the parser through const-decl lowering, and the
  Include/Exclude/assignment check accepts HASHTYPE_CONST as long as
  the typed-const flag is set. }
{$mode objfpc}
program TypedConstSetInclude;

const
  Flags: set of AnsiChar = [];

var
  C: AnsiChar;

begin
  C := 'X';
  Include(Flags, C);
  C := 'Y';
  Include(Flags, C);
  if ('X' in Flags) and ('Y' in Flags) and not ('Z' in Flags) then
    Writeln('ok')
  else
    Writeln('bad');

  Exclude(Flags, 'X');
  if not ('X' in Flags) and ('Y' in Flags) then
    Writeln('exclude-ok')
  else
    Writeln('exclude-bad');
end.
