program TestSystemParamStr;

procedure Prior;
begin
end;

procedure GetPara;
var
  s: string;
begin
  s := ParamStr(0);
end;

begin
  GetPara;
end.
