program tdd_inc_pansichar_sizeof;

type
  TRec = record
    x: Integer;
  end;

var
  p: ^TRec;

begin
  new(p);
  inc(PAnsiChar(p), SizeOf(p^));
  writeln('ok');
end.
