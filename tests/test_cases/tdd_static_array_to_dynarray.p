program tdd_static_array_to_dynarray;
{ Regression: assigning a fixed-size (typed-constant / static) array to a
  dynamic array must deep-copy the element data into a freshly allocated heap
  buffer.  KGPC instead loaded the static array's first bytes as a scalar and
  passed that value to kgpc_dynarray_assign_from_temp as if it were a heap
  descriptor pointer -> a wild-pointer dereference / heap corruption on free.

  This is exactly FPC's get_saved_registers_int (compiler/x86_64/cpupara.pas):
      result := win64_saved_std_regs;   { static array -> array of tsuperregister }
  whose corrupted free crashed the KGPC-bootstrapped FPC 3.2.2 compiler during
  Win64 proc-entry codegen (g_proc_entry). }
{$mode objfpc}{$H-}
type
  tsr = word;
  tarr = array of tsr;
const
  win64regs: array[0..7] of tsr = (3, 7, 6, 12, 13, 14, 15, 5);
  others:    array[0..4] of tsr = (3, 12, 13, 14, 15);
function getregs(win: boolean): tarr;
begin
  if win then
    getregs := win64regs        { static array -> dynamic array result }
  else
    getregs := others;
end;
var
  a: tarr;
  i: integer;
begin
  a := getregs(true);
  write('len=', length(a), ':');
  for i := 0 to high(a) do write(' ', a[i]);
  writeln;
  a := nil;                     { decr_ref -> free (must not corrupt heap) }
  a := getregs(false);
  write('len=', length(a), ':');
  for i := 0 to high(a) do write(' ', a[i]);
  writeln;
  a := nil;
  writeln('freed ok');
end.
