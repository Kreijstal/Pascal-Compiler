{ Test System.MaxInt qualified constant access in const expression }
{ Required for FPC bootstrap - unix.pp uses similar patterns }
program fpc_bootstrap_system_qualified_const;

{$mode objfpc}

const
  MY_MAX = System.MaxInt;

begin
  WriteLn('System.MaxInt = ', MY_MAX);
end.
