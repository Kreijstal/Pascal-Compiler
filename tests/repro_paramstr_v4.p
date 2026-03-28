program ReproParamStrV4;

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
