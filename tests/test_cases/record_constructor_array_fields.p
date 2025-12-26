program RecordConstructorArrayFields;

type
  TIntArray = array[1..3] of Integer;
  TCharPair = array[0..1] of Char;

  TRec = record
    A: TIntArray;
    B: TCharPair;
  end;

var
  R: TRec;
begin
  R := (A: (1, 2, 3); B: ('x', 'y'));
  WriteLn(R.A[1], ',', R.A[2], ',', R.A[3], ';', R.B[0], R.B[1]);
end.
