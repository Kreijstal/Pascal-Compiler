{ Test resourcestring actual output - required for FPC bootstrap sysconst.pp }
{ resourcestring declarations should be usable just like regular string constants }
program fpc_bootstrap_resourcestring_output;

{$mode objfpc}

resourcestring
  SHello = 'Hello from resourcestring';

begin
  WriteLn(SHello);
end.
