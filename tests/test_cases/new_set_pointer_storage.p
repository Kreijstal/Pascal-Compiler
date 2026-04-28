program new_set_pointer_storage;

type
  TConstSet = set of 0..255;
  PConstSet = ^TConstSet;
  TByteArray32 = array[0..31] of byte;

procedure FillBytes(out arr: array of byte);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    arr[i] := i + 1;
end;

var
  ps: PConstSet;
begin
  New(ps);
  FillBytes(TByteArray32(ps^));
  if (0 in ps^) and (255 in ps^) then
    writeln('edges set')
  else
    writeln('missing edge');
  Dispose(ps);
end.
