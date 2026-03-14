{ Test GetFPCHeapStatus function }
program fpc_heap_status;
{$mode objfpc}
var
  hs: TFPCHeapStatus;
begin
  hs := GetFPCHeapStatus;
  { Just verify we can call it and access fields }
  WriteLn(hs.CurrHeapUsed >= 0);
end.
