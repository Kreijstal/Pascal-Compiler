{ Repro for KGPC: "expression is not indexable as an array" (sysutils/fina.inc:366) }
program fpc_bootstrap_fina_shortstring_move;

{$mode objfpc}
{$H+}

const
  OneLevelBack = '..' + '/';

var
  ResultStr: AnsiString;
  Len, J: SizeInt;

begin
  ResultStr := '';
  Len := Length(ResultStr);
  SetLength(ResultStr, Len + 3 * Length(OneLevelBack));
  for J := 0 to 2 do
    Move(ShortString(OneLevelBack)[1], ResultStr[Len + 1 + J * Length(OneLevelBack)], Length(OneLevelBack));
  writeln('OK');
end.
