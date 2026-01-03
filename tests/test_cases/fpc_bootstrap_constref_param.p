{ Test: constref parameters are used by FPC RTL entry points (SysEntry). }
program fpc_bootstrap_constref_param;
{$mode objfpc}

type
  TEntryInfo = record
    Code: LongInt;
    Data: LongInt;
  end;

var
  GlobalInfo: TEntryInfo;

procedure Consume(constref info: TEntryInfo);
begin
  if PtrUInt(@info) = PtrUInt(@GlobalInfo) then
    WriteLn('SAME')
  else
    WriteLn('COPY');
end;

begin
  GlobalInfo.Code := 7;
  GlobalInfo.Data := 11;
  Consume(GlobalInfo);
end.
