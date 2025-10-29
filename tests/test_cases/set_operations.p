program set_operations;

type
  TDay = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
  TDaySet = set of TDay;

var
  weekdays, weekend, all_days, overlap: TDaySet;
  day: TDay;
  union_count, intersection_count: integer;
begin
  weekdays := [Monday, Tuesday, Wednesday, Thursday, Friday];
  weekend := [Saturday, Sunday];
  all_days := weekdays + weekend;
  overlap := weekdays * weekend;

  union_count := 0;
  intersection_count := 0;

  for day := Monday to Sunday do
  begin
    if day in all_days then
      union_count := union_count + 1;
    if day in overlap then
      intersection_count := intersection_count + 1;
  end;

  writeln(union_count);
  writeln(intersection_count);

  if Friday in all_days then
    writeln('weekday');

  if Saturday in all_days then
    writeln('weekend');
end.
