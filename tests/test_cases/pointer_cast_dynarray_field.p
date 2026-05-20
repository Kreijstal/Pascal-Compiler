{$mode objfpc}
{ Regression: casting a record's dynamic-array field to a raw pointer must
  yield the descriptor's `data` field (the heap buffer pointer), not the
  address of the descriptor itself. Trunk FPC's
  ngtcon.parse_arraydef uses `ca:=pointer(stringnode.valueas)` followed by
  byte-loop dereference, so emitting the descriptor's own address would
  read struct internals as char bytes (e.g. into pp_stage2 .rodata for
  the msgtxt typed-const).
}
program pointer_cast_dynarray_field;
type
  TByteDynArray = array of byte;
  TMyRec = record
    pad : longint;
    arr : TByteDynArray;
  end;
var
  r  : TMyRec;
  p  : pointer;
  pb : pbyte;
  i  : longint;
begin
  setlength(r.arr, 5);
  for i := 0 to 4 do
    r.arr[i] := byte(65 + i);
  p := pointer(r.arr);
  pb := p;
  for i := 0 to 4 do
    begin
      writeln(byte(pb^));
      inc(pb);
    end;
end.
