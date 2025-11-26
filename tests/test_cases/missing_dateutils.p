program missing_dateutils;
uses DateUtils, SysUtils;
var
  d: TDateTime;
  s: string;
begin
  d := Now;
  s := DateTimeToStr(d);
  writeln('Current time: ', s);
end.
