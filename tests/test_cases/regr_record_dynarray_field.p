program regr_record_dynarray_field;

{$mode objfpc}

type
  TRec = record
    Data: array of byte;
  end;

var
  r: TRec;
  i: integer;
begin
  SetLength(r.Data, 3);
  r.Data[0] := 10;
  r.Data[1] := 20;
  r.Data[2] := 30;
  writeln(r.Data[0], ' ', r.Data[1], ' ', r.Data[2]);
  writeln('len=', Length(r.Data));
end.
