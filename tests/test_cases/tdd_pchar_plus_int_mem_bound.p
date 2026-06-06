program tdd_pchar_plus_int_mem_bound;
{ Regression for pointer + integer arithmetic where the integer offset lives
  in a memory slot (a parameter/local), not a register or immediate.

  KGPC's pointer-arithmetic path computed `p + len` by emitting
      addq  -12(%rbp), %rbx        { len is a 32-bit slot at -12 }
  which reads 8 bytes from the 4-byte `len` slot, pulling the adjacent 4 bytes
  (the low half of `p` itself, stored next to it) into the high 32 bits of the
  offset.  The result was `p + len + (low32(p) << 32)` — a wild pointer.

  This is exactly FPC's FPHash / DJBHash loop:
      pmax := p + len;  while p < pmax do begin h := h*33 + p^; inc(p) end;
  With the bug, pmax was astronomically large, so the loop walked off the end
  of the buffer: it segfaulted on Win64 (high stack addresses) when reading p^
  and looped effectively forever on Linux/Wine.  That crash killed the FPC
  3.2.2 RTL build (system.pp's def_system_macro -> fphash) under the
  KGPC-bootstrapped compiler. }

function djbhash(p: pchar; len: integer): longword;
var
  pmax: pchar;
begin
  djbhash := 5381;
  pmax := p + len;
  while p < pmax do
  begin
    djbhash := longword(longint(djbhash shl 5) + longint(djbhash)) + longword(p^);
    inc(p);
  end;
end;

function countbytes(p: pchar; len: integer): integer;
var
  pmax: pchar;
  cnt: integer;
begin
  pmax := p + len;
  cnt := 0;
  while p < pmax do
  begin
    inc(cnt);
    inc(p);
  end;
  countbytes := cnt;
end;

var
  buf: array[0..15] of char;
  i: integer;
begin
  for i := 0 to 15 do
    buf[i] := chr(65 + i);          { 'A'..'P' }
  writeln(countbytes(@buf[0], 6));  { 6 }
  writeln(countbytes(@buf[0], 0));  { 0 }
  writeln(countbytes(@buf[0], 16)); { 16 }
  writeln(djbhash(@buf[0], 6));     { hash of 'ABCDEF' }
  writeln(djbhash(@buf[0], 0));     { 5381 (empty) }
end.
