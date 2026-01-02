program fpc_bootstrap_const_default_param;
{$mode objfpc}
{$J-}
{ Test: Untyped constant as default parameter value }
{ FPC: Should compile and output Little,Big }
{ KGPC: Fails or warns with "Could not copy default value" }
{ This pattern is used in FPC's TGUIDHelper.Create with CPUEndian }

type
  TEndian = (Little, Big);

const
  CPUEndian = TEndian.Little;  { Untyped constant }

procedure ShowEndian(E: TEndian = CPUEndian);
begin
  if E = TEndian.Little then
    writeln('Little')
  else
    writeln('Big');
end;

begin
  ShowEndian;  { Use default CPUEndian }
  ShowEndian(TEndian.Big);  { Explicit Big }
end.
