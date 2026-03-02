program tdd_pointer_alias_nil_index;

type
  PMyByte = ^Byte;

var
  p: PMyByte = nil;
  buf: array[0..2] of Byte;

begin
  if p = nil then writeln('nil');
  buf[0] := 7;
  p := @buf[0];
  p[1] := 9;
  writeln(p[0]);
  writeln(p[1]);
end.
