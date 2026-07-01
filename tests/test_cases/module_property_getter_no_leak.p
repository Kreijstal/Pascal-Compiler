{ Regression: an unqualified identifier that resolves via the FPC-style
  "module property getter" heuristic (bare `Foo` rewritten to `GetFoo()`
  when a GetFoo function is in scope) is converted in place from EXPR_VAR_ID
  to EXPR_FUNCTION_CALL in semcheck_varid_ex.  The conversion memset()s the
  expr_data union to function_call form, which previously clobbered the
  strdup'd VAR_ID id without freeing it, leaking it (e.g. `Tzseconds` in the
  FPC unix RTL, the last remaining pp.pas self-compile leak).

  This exercises the getter rewrite on an expression RHS so the store must
  still compute correctly (confirming no use-after-free from the added free). }
program module_property_getter_no_leak;

function GetSekunden: Longint;
begin
  GetSekunden := 77;
end;

function Compute: Longint;
begin
  Compute := -Sekunden div 7 + Sekunden;
end;

begin
  writeln(Compute);
end.
