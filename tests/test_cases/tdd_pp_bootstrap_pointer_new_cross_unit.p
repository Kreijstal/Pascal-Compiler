{ Regression: under FPC's pp.pas Windows-target compile, the pointer
  alias `psysteminfo = ^tsysteminfo` declared in compiler/systems.pas
  collides (case-insensitively) with the RTL's `TSystemInfo` from
  rtl/win/sysos.inc.  At the `new(targetinfos[t]); targetinfos[t]^ := r`
  site inside registertarget, KGPC's codegen_sizeof_pointer_target
  performs a FindSymbol(symtab, "tsysteminfo") that returns the
  Windows record (48 bytes) instead of the compiler record (440
  bytes).  Both the kgpc_new size and the kgpc_move size are wrong;
  the under-allocated block is then over-written by the record copy
  and pp_win.exe later page-faults inside set_target.

  This test exercises the structural fix: when a pointer alias is
  declared in unit U and U declares its own record T, the pointer's
  target size must be U's T, not a transitively-imported same-named
  record. }
{$mode objfpc}
program TddPpBootstrapPointerNewCrossUnit;

uses tdd_pp_bootstrap_pointer_new_cross_unit_systems;

var
  src : tinfo;
begin
  src.sentinel_lo := 1111;
  src.payload[40] := 4040;
  src.sentinel_hi := 9999;
  RegisterIt(src);
  DumpIt(r_b);
end.
