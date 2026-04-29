program tdd_open_array_out_untyped_forward;

type
  TByteArray = array[0..7] of Byte;
  PByte = ^Byte;

procedure FillRaw(out b; len: Integer);
var
  p: PByte;
  i: Integer;
begin
  p := @b;
  for i := 0 to len - 1 do
    p[i] := Byte(i + 10);
end;

procedure FillOpen(out arr: array of Byte);
begin
  FillRaw(arr, SizeOf(arr));
end;

var
  data: TByteArray;
begin
  FillOpen(data);
  WriteLn(data[0]);
  WriteLn(data[7]);
end.
