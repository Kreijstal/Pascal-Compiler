program tdatetime_basics;

{$mode objfpc}

uses
  sysutils, dateutils;

const
  JulianEpoch = TDateTime(-2415018.5);
  UnixEpoch = JulianEpoch + TDateTime(2440587.5);
  UnixDateDelta = Trunc(UnixEpoch);

begin
  writeln(SizeOf(TDateTime));
  writeln(UnixDateDelta);
  writeln(MillisecondsBetween(UnixToDateTime(0), UnixToDateTime(1)));
end.
