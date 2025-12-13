{ Test PText type - pointer to Text - required for FPC bootstrap heaptrc.pp }
program fpc_bootstrap_ptext;

{$mode objfpc}

var
  pf: PText;
begin
  pf := nil;
  WriteLn('PText type works');
end.
