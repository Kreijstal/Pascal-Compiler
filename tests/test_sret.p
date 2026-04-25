program TestSRET;

function MyParamStr(index: LongInt): ShortString;
begin
  MyParamStr := 'abc';
end;

procedure Prior;
begin
end;

procedure GetPara;
var
  s: ShortString;
begin
  s := MyParamStr(1);
end;

begin
  GetPara;
end.
