program byte_writeln_unsigned;
{$mode objfpc}

{ Test: Byte type should print as unsigned (0..255) }

type
  TBuffer = array[0..3] of Byte;

var
  buf: TBuffer;
  i: Integer;
begin
  FillChar(buf, SizeOf(buf), 255);
  for i := 0 to 3 do
    write(buf[i], ' ');
  writeln;
end.
