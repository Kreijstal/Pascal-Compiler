{ Regression: an unsigned narrowing typecast Word()/Byte() must truncate to its
  width whenever its value is materialised -- as a writeln/call argument or an
  array index -- not only inside arithmetic or an assignment.

  KGPC stripped the cast in non-arithmetic value contexts, so the full operand
  survived: Word($1050002) yielded 0x1050002 (17104898) instead of 2.  In FPC's
  x86 CMOV optimiser that surfaced as `ConstWriteSizes[Word(ConstRegs[Count])]`
  and `[getsupreg(...)]` (= tregisterrec(r).supreg, the low 16 bits) indexing a
  small byte array by the full 32-bit register encoding -> a wildly out-of-bounds
  pointer -> SIGSEGV in TCMovTracking.Process inside the KGPC-built FPC compiler
  (pp_bootstrap) at -O2.  Expected: every line prints 2 / arr[2]=20. }
program word_byte_cast_truncates_as_value;
var arr: array[0..9] of byte; r: longint; i: byte; w: word; b: byte;
begin
  for i := 0 to 9 do arr[i] := i * 10;
  r := $1050002;
  writeln('argW=', Word(r));        { 2 }
  writeln('argB=', Byte(r));        { 2 }
  writeln('idx=', arr[Word(r)]);    { arr[2] = 20 }
  w := Word(r); b := Byte(r);
  writeln('asg=', w, ',', b);       { 2,2 }
end.
