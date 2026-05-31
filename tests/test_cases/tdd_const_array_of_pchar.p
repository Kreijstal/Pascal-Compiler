{ Regression: a `const array of PAnsiChar/PChar` initialized from string
  literals must lower each element to a POINTER to a static NUL-terminated
  buffer.  A single-quote literal such as '.' is parsed as a Char
  (EXPR_CHAR_CODE), so a single-character element was stored as the raw
  character ordinal (e.g. 46) directly into the pointer slot.  Reading
  FixDriveStr[0] then dereferenced the integer 46 and segfaulted.  The same
  bug affected scalar `p := '.'` where p is PChar.  Multi-character literals
  already lowered to a `.string` literal address, so only single-char
  elements were wrong.  Mirrors sysutils' FixDriveStr const. }
program tdd_const_array_of_pchar;
{$mode objfpc}
const
  FixDriveStr : array[0..3] of PAnsiChar = ('.', '/fd0/.', '/fd1/.', '/.');
  Names       : array[0..2] of PChar     = ('a', 'bb', 'ccc');
var
  i: integer;
  p: PAnsiChar;
begin
  for i := 0 to 3 do
    writeln(FixDriveStr[i]);
  for i := 0 to 2 do
    writeln(Names[i]);
  p := '!';
  writeln(p);
end.
