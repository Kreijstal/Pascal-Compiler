program fpc_bootstrap_scoped_enum_comparison;
{$mode objfpc}
{$scopedenums on}
{ Test: Comparing scoped enum values }
{ FPC: Should compile and output Big,Small }
{ KGPC: Fails with type comparison errors }
{ This pattern is used in FPC's TGUIDHelper with TEndian }

type
  TEndian = (Little, Big);

function IsLittleEndian(E: TEndian): Boolean;
begin
  Result := E = TEndian.Little;
end;

var
  e: TEndian;
begin
  e := TEndian.Big;
  if e = TEndian.Big then
    writeln('Big')
  else
    writeln('Little');
    
  if IsLittleEndian(TEndian.Little) then
    writeln('Small')
  else
    writeln('Large');
end.
