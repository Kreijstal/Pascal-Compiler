{ Regression: `for <var> in <set>` must yield the ELEMENT ORDINAL, not the
  bit-mask within the byte.

  KGPC's set for-in codegen holds six registers live simultaneously in the loop
  body (index, set base, byte-index, bit-index, byte-value, and the 1<<bit
  mask), but the allocatable register pool only has five (rbx, r12-r15).  The
  mask register therefore reused the index register's physical register and
  clobbered the running index before it was stored into the loop variable, so
  the loop variable received `1 shl (index and 7)` instead of `index`.

  This miscompiled FPC's optimiser: GetIntRegisterBetween in aoptx86.pas does
  `for CurrentSuperReg in RegSet do CurrentReg := newreg(R_INTREGISTER,
  TSuperRegister(CurrentSuperReg), RegSize)`, so a garbage superregister index
  produced undecodable cmov/mov/xor operands when bootstrapping system.pp at
  -O2.

  The fix reloads the loop index from its canonical memory slot before storing
  it into the loop variable.

  Expected: the iterated elements are the set members in ascending order. }
program for_in_set_index_clobber;

type
  TByteSet = set of byte;
  TSmall = (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13);

var
  bs: TByteSet;
  es: set of TSmall;
  b: byte;
  e: TSmall;
  sum: longint;

begin
  { Elements within one byte and spanning into the next byte. }
  bs := [3, 5, 7, 12];
  sum := 0;
  write('byteset:');
  for b in bs do
  begin
    write(' ', b);
    sum := sum + b;
  end;
  writeln(' sum=', sum);

  { Set of a small enum type. }
  es := [E1, E4, E9, E13];
  write('enumset:');
  for e in es do
    write(' ', ord(e));
  writeln;
end.
