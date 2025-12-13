{ Test TRTLCriticalSection type - required for FPC bootstrap heaptrc.pp }
program fpc_bootstrap_trtlcriticalsection;

{$mode objfpc}

var
  cs: TRTLCriticalSection;
begin
  WriteLn('TRTLCriticalSection supported');
end.
