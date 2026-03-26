{$H+}
program TDDParamStrShortStringSretOrder;

procedure Prior;
begin
end;

procedure GetPara;
var
  ParaValue: string;
  I: Word;
begin
  for I := 1 to ParamCount do
  begin
    ParaValue := ParamStr(I);
    if ParaValue[1] = '-' then
      Writeln(ParaValue);
  end;
  Writeln('');
end;

begin
  GetPara;
end.
