program TDDNotPtrFieldIndex;

type
  TRec = record
    Backwards: array[0..3] of Boolean;
  end;
  PRec = ^TRec;

var
  r: TRec;
  p: PRec;
  i: Integer;
begin
  r.Backwards[0] := True;
  p := @r;
  i := 0;

  if not p^.Backwards[i] then
    writeln(0)
  else
    writeln(1);
end.
