program tdd_record_subrange_field;

type
  TStamp = packed record
    Month: 1..12;
    Day: 1..31;
  end;

var
  Stamp: TStamp;

begin
  Stamp.Month := 12;
  Stamp.Day := 31;
  Writeln(Stamp.Month);
end.
