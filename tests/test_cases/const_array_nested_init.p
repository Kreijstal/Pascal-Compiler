program const_array_nested_init;

type
  TDayTable = array[1..12] of Word;

const
  MonthDays: array[Boolean] of TDayTable = (
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  );

begin
  Writeln(MonthDays[False][2]);
  Writeln(MonthDays[True][2]);
end.
