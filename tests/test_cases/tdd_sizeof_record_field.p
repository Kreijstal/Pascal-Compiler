{$mode objfpc}

program tdd_sizeof_record_field;

type
  TRec = record
    Digits: array[0..18] of char;
  end;

var
  R: TRec;
  L: LongInt;

begin
  L := SizeOf(R.Digits);
  writeln(L);
end.
