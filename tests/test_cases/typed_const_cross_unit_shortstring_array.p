{ Regression test for cross-unit typed-const array-of-shortstring init.

  KGPC used to emit per-element address computation with stride 256 for
  `array[T] of string[N<255]` when the array was declared in another
  unit, because the cross-unit symtab HashNode did not carry the
  declaration's specific element type (it appeared as a generic
  ShortString primitive, 256 bytes).  The init code wrote element 34 of
  FPC's `token2str : array[tasmtoken] of string[10]` at offset 8704 —
  far past the 847-byte allocation — clobbering adjacent BSS and
  producing a downstream `Asm: [mov mem16,???]` error during pp_bootstrap
  startup.

  The companion Python test inspects the emitted asm and asserts that
  the per-element shift uses stride 11 (N+1 for string[10]), never 256. }
program typed_const_cross_unit_shortstring_array;

uses
  typed_const_cross_unit_shortstring_array_recunit;

begin
end.
