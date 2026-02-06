{ Repro for KGPC: Pos target must be a string (syssbh.inc:110) }
program fpc_bootstrap_syssbh_pos_alias;

{$mode objfpc}
{$H+}

var
  s: string[10];
  p: SizeInt;

begin
  s := 'abc';
  p := Pos('b', s);
  if p > 0 then
    writeln('OK')
  else
    writeln('FAIL');
end.
