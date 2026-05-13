program regr_sizeof_open_array;

type
  TPPUSet1 = array[0..0] of byte;

procedure putdata(const arr: array of byte; size: LongInt);
begin
  WriteLn('size=', size, ' len=', Length(arr));
end;

procedure putset(const arr: array of byte);
begin
  putdata(arr, SizeOf(arr));
end;

var
  tableoptions: TPPUSet1;
begin
  tableoptions[0] := 1;
  putset(TPPUSet1(tableoptions));
end.
