program tdd_ptr_plus_narrow_mem;
{ Regression for pointer + narrow-integer arithmetic where the offset lives in a
  memory slot (a parameter), not a register or immediate.

  The pointer-arithmetic path loaded a non-qword memory offset with a blanket
  movl/movslq (4 bytes).  For a Byte (1) or Word (2) parameter slot that reads
  past the operand, pulling adjacent stack bytes into the high bits of the
  offset and corrupting the pointer.  The load width must match the operand:
  movzbl for Byte, movzwl for Word.

  Mirrors tdd_pchar_plus_int_mem_bound but with narrow offsets: with the bug the
  `p < pmax` walk uses a wild pmax and miscounts / runs away. }

function countb(p: pchar; len: byte): integer;
var
  pmax: pchar;
  cnt: integer;
begin
  pmax := p + len;          { len is a 1-byte parameter slot }
  cnt := 0;
  while p < pmax do
  begin
    inc(cnt);
    inc(p);
  end;
  countb := cnt;
end;

function countw(p: pchar; len: word): integer;
var
  pmax: pchar;
  cnt: integer;
begin
  pmax := p + len;          { len is a 2-byte parameter slot }
  cnt := 0;
  while p < pmax do
  begin
    inc(cnt);
    inc(p);
  end;
  countw := cnt;
end;

var
  buf: array[0..15] of char;
  i: integer;
begin
  for i := 0 to 15 do
    buf[i] := chr(65 + i);          { 'A'..'P' }
  writeln(countb(@buf[0], 5));      { 5 }
  writeln(countb(@buf[0], 0));      { 0 }
  writeln(countb(@buf[0], 16));     { 16 }
  writeln(countw(@buf[0], 7));      { 7 }
  writeln(countw(@buf[0], 12));     { 12 }
end.
