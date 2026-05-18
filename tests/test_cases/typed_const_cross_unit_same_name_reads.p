{ Regression test for #731: cross-unit reads of same-named typed-const arrays
  must resolve to the read-site's own unit's storage, not whichever unit's
  registration happens last during codegen.

  Two units (twounit_same_name_reads_a, twounit_same_name_reads_b) each
  declare a private (implementation-section) typed-const named `msg` with
  different content.  Each unit also exports a procedure that reads its own
  `msg` and writes it.  Before the fix, codegen's find_label("msg") inside
  write_a returned unit_b's StackNode (because semcheck's mangled_id slot
  for the bare name "msg" was last-write-wins across both units), so the
  call read unit_b's storage instead of unit_a's.

  This is the same bug shape as the FPC pp.pas compile of `msgtxt`:
  ait_const2str is declared identically in aggas.pas and agx86nsm.pas
  (two different file-level consts named ait_const2str).  When KGPC
  compiles pp.pas into pp_bootstrap, every read of ait_const2str inside
  TGNUAssembler.WriteTree resolves to agx86nsm.pas's NASM table, so the
  emitted directives are DB/DQ/FIXME_ULEB128BIT instead of .byte/.quad.
  When pp_bootstrap later compiles pp.pas itself, the same bug produces
  pp_stage2 with garbage bytes in .rodata for msgtxt, and pp_stage2 -h
  SIGSEGVs in tmessage.createidx. }
program typed_const_cross_unit_same_name_reads;

uses
  typed_const_cross_unit_same_name_reads_a,
  typed_const_cross_unit_same_name_reads_b;

begin
  write_a;
  write_b;
end.
