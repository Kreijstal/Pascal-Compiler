program tdatetime_basics;

{$mode objfpc}

uses
{$ifdef FPC}
  sysutils, dateutils;
{$else}
  sysutils;
{$endif}

const
  JulianEpoch = TDateTime(-2415018.5);
  UnixEpoch = JulianEpoch + TDateTime(2440587.5);
  UnixDateDelta = Trunc(UnixEpoch);

begin
  writeln(SizeOf(TDateTime));
  writeln(UnixDateDelta);
  writeln(MillisecondsBetween(UnixToDateTime(0), UnixToDateTime(1)));
end.
