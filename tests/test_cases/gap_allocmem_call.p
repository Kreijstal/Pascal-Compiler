program gap_allocmem_call;

var
  P: Pointer;

begin
  P := AllocMem(8);
  if P <> nil then
    WriteLn('ok')
  else
    WriteLn('fail');
  FreeMem(P);
end.
