{ Regression: comparing a dynamic `array of char` (FPC's TAnsiCharDynArray) to
  nil inside a short-circuit boolean must be a pointer test, not a string
  comparison.  expr_is_char_array_expr() is true for char arrays, so
  `(a = nil)` used to fall into the char-array compare path, which has no static
  length for a dynamic array and bailed out WITHOUT emitting a comparison --
  leaving the array's (always-nonzero) slot address as the boolean result, so
  `(a = nil)` was always treated as false-but-nonzero.  This mirrors FPC
  ogcoff.pas TCoffObjInput.Read_str:
      if (FCoffStrs=nil) or (strpos>=FCoffStrSize) or (FCoffStrs[strpos]=#0) ...
  whose mis-compilation broke the win64 internal linker's COFF string reader. }
program dynchararray_nil_in_or;
{$mode objfpc}
type
  tcharbuf = array of ansichar;
var
  a: tcharbuf;

{ The exact short-circuit shape from Read_str: nil-test first, then a bounds
  check, then an element read. }
function read_at(strpos: longword): longint;
begin
  if (a = nil) or (strpos >= 8) or (a[strpos] = #0) then
    read_at := 999
  else
    read_at := ord(a[strpos]);
end;

begin
  SetLength(a, 8);
  a[0] := 'X';
  a[1] := #0;

  { a is allocated, so (a=nil) is false; element 0 is 'X' (not #0) -> ord('X') }
  writeln(read_at(0));               { 88 }
  { element 1 is #0 -> third OR term fires -> 999 }
  writeln(read_at(1));               { 999 }

  { standalone <> nil and = nil }
  if a <> nil then writeln('NOTNIL') else writeln('NIL');   { NOTNIL }
  if a = nil then writeln('ISNIL') else writeln('LIVE');    { LIVE }

  { reversed operand order }
  if nil = a then writeln('EQ') else writeln('NE');         { NE }

  { after release, the same OR must now see nil }
  a := nil;
  if (a = nil) or (1 > 2) then writeln('NILNOW') else writeln('STILL');  { NILNOW }
end.
