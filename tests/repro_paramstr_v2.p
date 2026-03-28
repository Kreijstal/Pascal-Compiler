program ReproParamStrV2;

type
  ShortString = array[0..255] of Char;

function ParamStr(index: LongInt): ShortString; external name 'paramstr_li';
function ParamCount: LongInt; external name 'paramcount_void';

procedure Prior;
begin
end;

procedure GetPara;
var
  para: string;
  i: Word;
begin
  for i := 1 to ParamCount do
  begin
    para := ParamStr(i);
    if para[1] = '-' then
      Writeln(para);
  end;
end;

begin
  GetPara;
end.
