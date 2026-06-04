{ Regression: a signed sub-32-bit reinterpret cast (SmallInt(x)/ShortInt(x))
  must sign-extend from its own width when the result is widened.  Casting an
  unsigned Word/Byte to SmallInt/ShortInt used to zero-extend, so SmallInt of
  $ffff stayed 65535 instead of -1.  This is the root cause of FPC's str()
  default-format sentinels (-1, -32767) losing their sign in the KGPC-built
  FPC, which pushed the Windows self-host one stage past stage-3 convergence. }
program tdd_smallint_word_signext;
var
  w: word;
  b: byte;
  z: longint;
begin
  w := $ffff;
  z := smallint(w);
  writeln(z);                      { -1 }
  w := $8001;
  writeln(longint(smallint(w)));   { -32767 }
  b := $ff;
  z := shortint(b);
  writeln(z);                      { -1 }
  b := $80;
  writeln(longint(shortint(b)));   { -128 }
end.
