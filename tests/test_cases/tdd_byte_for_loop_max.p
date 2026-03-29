program tdd_byte_for_loop_max;
var
  i: Byte;
  count: Integer;
begin
  count := 0;
  for i := 0 to 255 do
    Inc(count);
  WriteLn(count);
end.
