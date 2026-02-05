{ Repro for KGPC: Pos substring must be a string (syshelps.inc:432) }
program fpc_bootstrap_syshelps_pos_lowercase;

{$mode objfpc}
{$H+}

var
  sub: string[10];
  s: AnsiString;
  p: SizeInt;

begin
  sub := 'he';
  s := 'hello';
  p := Pos(sub, s);
  if p > 0 then
    writeln('OK')
  else
    writeln('FAIL');
end.
