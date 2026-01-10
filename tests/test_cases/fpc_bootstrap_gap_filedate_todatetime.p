program fpc_bootstrap_gap_filedate_todatetime;

{$mode objfpc}

uses SysUtils;

var
  D: LongInt;
  DT: TDateTime;

begin
  D := 0;
  DT := FileDateToDateTime(D);
  writeln('dt=', DT:0:0);
end.
