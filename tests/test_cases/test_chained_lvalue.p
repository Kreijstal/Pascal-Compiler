program TestChainedAccess;
type
  TElementRecord = record
    Name: array[1..10] of char;
  end;
  TRecordArray = array[1..5] of TElementRecord;
var
  RecordCollection: TRecordArray;
  ch: char;
begin
  RecordCollection[2].Name[1] := 'A';
  ch := RecordCollection[2].Name[1];
  writeln(ch);
end.
