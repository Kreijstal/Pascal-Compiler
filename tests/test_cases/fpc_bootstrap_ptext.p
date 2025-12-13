{ Test PText type - pointer to Text - required for FPC bootstrap heaptrc.pp }
program fpc_bootstrap_ptext;

{$mode objfpc}

var
  f: Text;
  pf: PText;
begin
  pf := @f;
  WriteLn('PText works');
end.
