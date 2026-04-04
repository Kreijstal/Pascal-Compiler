{$H+}
program ReproParamStrV6;

type
  ShortString = array[0..255] of Char;

function ParamStr(index: LongInt): ShortString; external name 'paramstr_li';

procedure Prior;
begin
end;

procedure GetPara;
var
  para: string;
begin
  para := ParamStr(0);
end;

begin
  GetPara;
end.
