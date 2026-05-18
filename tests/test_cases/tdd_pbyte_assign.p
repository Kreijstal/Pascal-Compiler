{ Regression test: pb^ := byte_var, where pb is a pbyte pointer,
  must emit a 1-byte store (movb), not a 4-byte store (movl).

  Before the fix, the scalar pointer-deref assignment path used movl
  for any non-CHAR_TYPE non-use_word destination, including BYTE_TYPE.
  The 4-byte store overflowed the destination by 3 bytes and clobbered
  adjacent stack slots. In pp_bootstrap (FPC compiled by KGPC), this
  manifested as taicpu.gencode's `pb^ := ea_data.sib` overwriting the
  low byte of the adjacent local `codes : pchar`, causing the encoder
  loop to re-read the same code byte and emit relocations endlessly
  until TFPList hit MaxListSize.

  This test catches the regression by writing through a pbyte at the
  end of a 4-byte buffer, then reading the adjacent local variable
  to confirm it was not clobbered. }
program tdd_pbyte_assign;
var
  buf: array[0..3] of byte;
  guard: longword;
  pb: ^byte;
  src: byte;
begin
  guard := $DEADBEEF;
  src := $77;
  pb := @buf[3];
  pb^ := src;
  writeln(buf[0], ' ', buf[1], ' ', buf[2], ' ', buf[3]);
  writeln('guard=', guard);
end.
