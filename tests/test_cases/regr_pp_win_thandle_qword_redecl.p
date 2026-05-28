program regr_pp_win_thandle_qword_redecl;

{ Regression: prelude.p previously pre-declared THandle = LongInt (4 bytes).
  That predeclaration leaked a 32-bit primitive_type_tag into the symbol
  table BEFORE any later RTL declaration (e.g. FPC's
  FPCSource/rtl/win/sysosh.inc THandle = QWord under CPU64) could replace
  it.  KGPC's type-redecl path then dropped the new alias info because the
  existing primitive target (LongInt) did not match the new (QWord), and
  every subsequent expression typed as THandle was treated as 4-byte.

  On Windows-x86_64 this produced a truncating spill:
    movq %rax, %r12        ; HMODULE return value from GetModuleHandleA
    movl %r12d, -64(%rbp)  ; 32-bit STORE of a 64-bit pointer
    ...
    movl -64(%rbp), %r12d  ; reload TRUNCATED 32-bit value
    movslq %r12d, %r12     ; sign-extends back to a bogus 32-bit pointer
    movq (%r12), %rbx      ; faults at 0x400000e0-class addresses
  while the correct sequence is `movq %r12, -64(%rbp)` /
  `movq -64(%rbp), %r12`.

  This test exercises the same shape: user-redeclares THandle as QWord,
  stores a pointer-class value (image-base-style), and verifies both the
  full 64-bit value survives a round-trip and SizeOf is 8.  Before the
  fix, the stored value's high 32 bits would be lost. }

type
  THandle = QWord;

var
  h: THandle;
  reload_ok: Boolean;
begin
  h := QWord($140000000);
  WriteLn('value=', h);
  reload_ok := (h = QWord($140000000));
  if reload_ok then
    WriteLn('reload_ok=TRUE')
  else
    WriteLn('reload_ok=FALSE');
  if SizeOf(h) = 8 then
    WriteLn('sizeof_thandle=8')
  else
    WriteLn('sizeof_thandle=', SizeOf(h));
end.
