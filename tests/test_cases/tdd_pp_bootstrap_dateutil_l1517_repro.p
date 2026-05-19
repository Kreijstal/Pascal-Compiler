program tdd_pp_bootstrap_dateutil_l1517_repro;
{$mode objfpc}

{ Regression test for: pp_bootstrap (FPC compiled by KGPC) hits
  "Compilation raised exception internally" / SIGSEGV in
  tasmsymbol.increfs (via trgobj.do_spill_read -> spilling_create_load
  -> tai_cpu_abstract.loadref) when compiling an expression that has
  three or more double-returning function calls combined with '+'.

  Original site: FPCSource/packages/rtl-objpas/src/inc/dateutil.inc:1517
    Result:=(DateOf(AStartDate)<=DateOf(ADate)) and (DateOf(ADate)<=DateOf(AEndDate))

  KGPC itself compiles this construct cleanly; the bug is in the
  FPC compiler (pp.pas) that KGPC builds.  pp_bootstrap's spill code
  reads a stale tasmsymbol pointer out of a treference that should
  have been zero-cleared by reference_reset/FillChar.

  This test guards KGPC against re-introducing the miscompile when the
  FPC bootstrap chain is rebuilt. }

function f(v: double): double;
begin
  Result := v;
end;

var
  a, b, c: double;
  r: double;
begin
  a := 1.0;
  b := 2.0;
  c := 3.0;
  r := f(a) + f(b) + f(c);
  writeln(r:0:1);
end.
