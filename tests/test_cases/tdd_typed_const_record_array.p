program tdd_typed_const_record_array;

type
  TRec = record
    A: LongInt;
    B: LongInt;
  end;

const
  Recs: array[0..1] of TRec = (
    (A: 1; B: 2),
    (A: 3; B: 4)
  );

begin
  Writeln(Recs[0].A, ' ', Recs[0].B);
  Writeln(Recs[1].A, ' ', Recs[1].B);
end.
