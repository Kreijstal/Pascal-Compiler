{ Regression test: DCE must scan inline-asm operands even when the
  instruction line begins with a local label, e.g.
    .LOops: jmp TargetFunc
  Previously the asm scanner read the first whitespace-delimited token
  as the mnemonic, so `.LOops:` (the label) was treated as the
  mnemonic, the `jmp` was ignored, and the body of `TargetFunc` was
  eliminated by dead-code analysis.  This reproduces the
  `trulyrelocatethreadvar_li` link failure observed when cross-
  compiling pp.pas to win64, where FPCSource/rtl/win/systhrd.inc
  contains `.LOops: jmp TrulyRelocateThreadvar`. }
program tdd_inline_asm_label_jmp_dce;
uses tdd_inline_asm_label_jmp_dce_unit;
begin
  writeln(CallerAsm(5));
end.
