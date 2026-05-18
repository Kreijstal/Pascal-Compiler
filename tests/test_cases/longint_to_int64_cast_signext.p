program longint_to_int64_cast_signext;
{$mode objfpc}{$h+}

{ Explicit widening typecast Int64(longint_var) / QWord(longint_var) must
  sign-extend the 32-bit signed value to 64 bits.  Previously KGPC stripped
  the typecast and emitted a 32-bit movl that zero-extended, turning
  longint(-1) into 0x00000000FFFFFFFF (4294967295) inside the wider
  destination.  This broke FPC's adaptrange/getrange paths because
  cordconstnode.create(int64(v.value),..) on an enum literal whose
  tenumsym.value = -1 produced 4294967295, which then failed the enum
  range check (-1..N). }

var
  l: longint;
  q: int64;
  u: qword;
begin
  l := -1;
  q := int64(l);
  u := qword(int64(l));
  writeln('q=', q);
  writeln('u=', u);
end.
