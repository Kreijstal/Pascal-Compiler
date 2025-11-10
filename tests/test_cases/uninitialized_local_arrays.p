program TestUninitializedNested;

type
  TRecord = record
    value: integer;
    next: integer;
  end;

procedure Pollute;
var
  garbage: array[0..100] of integer;
  i: integer;
begin
  { Fill stack with non-zero values }
  for i := 0 to 100 do
    garbage[i] := i + 1000;
  { Use the array to prevent optimization }
  writeln('Polluted stack with value: ', garbage[50]);
end;

procedure TestUninitialized;
var
  list: array[0..10] of TRecord;
  i: integer;
begin
  { This should work if array is zero-initialized }
  { but might hang if list[0].next contains leftover garbage from Pollute }
  i := 0;
  while list[i].next <> 0 do
  begin
    writeln('Following link from ', i, ' to ', list[i].next);
    if i > 10 then
    begin
      writeln('ERROR: Infinite loop detected!');
      halt(1);
    end;
    i := list[i].next;
  end;
  writeln('Success! Array was properly zero-initialized.');
end;

begin
  Pollute;
  TestUninitialized;
end.
