program TestChainedAccess;
type
  TElementRecord = record
    Name: array[1..10] of integer;
  end;
  TRecordArray = array[1..5] of TElementRecord;
var
  RecordCollection: TRecordArray;
  value: integer;
begin
  RecordCollection[2].Name[1] := 65;
  value := RecordCollection[2].Name[1];
  writeln(value);
end.
