program fpc_bootstrap_shortstring_result_lenbyte;

{$H-}

function CopyViaFunctionName(const S: string): string;
var
  I: LongInt;
begin
  for I := 1 to Length(S) do
    CopyViaFunctionName[I] := S[I];
  CopyViaFunctionName[0] := S[0];
end;

begin
  Writeln(CopyViaFunctionName('linux'));
end.
