program fpc_bootstrap_interlockedexchangeadd_pointer;
{$mode objfpc}
{ Bootstrap blocker: pointer overload of InterlockedExchangeAdd used in RTL. }

var
  p: Pointer;
  r: Pointer;

begin
  p := Pointer(16);
  r := InterlockedExchangeAdd(p, Pointer(4));
  writeln(PtrUInt(r));
  writeln(PtrUInt(p));
end.
