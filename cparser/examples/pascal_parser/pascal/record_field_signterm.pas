program record_field_signterm;

type
  TRec = record
    StartIndex: integer;
  end;

var
  Items: array[1..2] of TRec;
  idx, value: integer;

begin
  idx := 1;
  value := (-Items[idx]).StartIndex;
end.
