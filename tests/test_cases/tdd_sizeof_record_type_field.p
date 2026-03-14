program tdd_sizeof_record_type_field;
{$mode delphi}

type
  TRec = record
  case Integer of
    0: (Bytes: array[0..3] of Byte);
    1: (AsInt: Integer);
  end;

begin
  Writeln(SizeOf(TRec.Bytes));
end.
