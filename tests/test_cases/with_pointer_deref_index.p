program with_pointer_deref_index;

type
  PChar = ^Char;
  TRec = record
    BufPtr: PChar;
    BufPos: Integer;
  end;

var
  r: TRec;
  buf: array[0..3] of Char;

begin
  buf[0] := 'a';
  buf[1] := 'b';
  buf[2] := 'c';
  buf[3] := #0;

  r.BufPtr := @buf[0];
  r.BufPos := 1;

  with TRec(r) do
  begin
    writeln(BufPtr^[BufPos]);
    BufPos := 2;
    writeln(BufPtr^[BufPos]);
  end;
end.
