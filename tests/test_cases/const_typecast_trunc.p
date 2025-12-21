program const_typecast_trunc;

{$mode objfpc}

type
  TDateTime = type Double;

const
  JulianEpoch = TDateTime(-2415018.5);
  UnixEpoch = JulianEpoch + TDateTime(2440587.5);
  UnixDateDelta = Trunc(UnixEpoch);

begin
  writeln(UnixDateDelta);
end.
