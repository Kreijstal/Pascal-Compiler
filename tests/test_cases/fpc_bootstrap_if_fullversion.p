{ Test $if FPC_FULLVERSION comparison - required for FPC bootstrap objpas.pp }
{ The FPC_FULLVERSION predefined constant is used in objpas.pp to }
{ conditionally compile generic interface declarations }
program fpc_bootstrap_if_fullversion;

{$mode objfpc}

{$if FPC_FULLVERSION >= 20000}
const 
  VERSION_CHECK = 'Version OK';
{$else}
const
  VERSION_CHECK = 'Version too old';
{$endif}

begin
  WriteLn(VERSION_CHECK);
end.
