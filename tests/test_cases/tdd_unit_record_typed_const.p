{ Regression test: cross-unit-implementation typed-const record initializer.

  Reproduces the bug where KGPC was filtering out typed-const declarations
  whose record type lives in a unit reachable only via the declaring unit's
  IMPLEMENTATION uses.  Symptom in the wild: cpuelf.pas's
  `elf_target_x86_64: TElfTarget = (...)` initializer compiled with every
  field assignment hitting offset 0, leaving `ElfTarget.encodereloc` NULL
  and crashing pp_bootstrap when it tried to invoke the indirect call. }

program tdd_unit_record_typed_const;
uses tdd_unit_record_typed_const_holder;
begin
  dump;
end.
