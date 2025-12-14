{ Test: unit initialization section execution }
{ Pattern from FPC unix.pp - initialization sections call procedures }
program fpc_bootstrap_unit_init;
{$mode objfpc}

uses fpc_bootstrap_unit_init_unit;

begin
  WriteLn('InitCount: ', InitCount);
end.
