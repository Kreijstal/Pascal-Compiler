program RecordFieldArrayAssign;
{ Regression test: assigning to a record field that is a fixed-size char array
  must copy every element, not just the first byte. }

type
  TAlfa = packed array[1..4] of char;

  TEntry = record
    Name: TAlfa;
    Link: integer;
  end;

var
  src: TAlfa;
  dest: TEntry;
  i: integer;
  ok: boolean;

begin
  { Initialize the source array with a distinctive pattern }
  for i := 1 to 4 do
    src[i] := chr(ord('A') + i - 1);

  dest.Link := 42;
  dest.Name := src;

  ok := true;
  for i := 1 to 4 do
    if dest.Name[i] <> src[i] then
      ok := false;

  if ok and (dest.Link = 42) then
    writeln('record field copy ok')
  else
    writeln('record field copy failed');
end.
