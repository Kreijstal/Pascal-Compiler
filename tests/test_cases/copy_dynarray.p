{$mode objfpc}
program test_copy_dynarray;
type
  TIntArray = array of Integer;
var
  a, b: TIntArray;
begin
  setlength(a, 10);
  a[0] := 1;
  a[1] := 2;
  b := copy(a, 0, 2);
  if length(b) <> 2 then
    halt(1);
  if b[0] <> 1 then
    halt(2);
  if b[1] <> 2 then
    halt(3);

  b := copy(a);
  if length(b) <> 10 then
    halt(4);
  if b[0] <> 1 then
    halt(5);

  writeln('OK');
end.
