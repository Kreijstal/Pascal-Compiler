{$mode objfpc}
{$modeswitch typehelpers}
{ TDD: helper overload resolution with named parameter }
program tdd_guidhelper_named_param;

type
  TEndian = (Little, Big);
  TBytes = array of Byte;
  TGUID = record
    D1: LongWord;
  end;

const
  CPUEndian = TEndian.Little;

type
  TGUIDHelper = type helper for TGUID
    class function Create(const Data; DataEndian: TEndian = CPUEndian): TGUID; overload; static;
    class function Create(const B: TBytes; DataEndian: TEndian = CPUEndian): TGUID; overload; static;
    class function Create(const B: TBytes; AStartIndex: Cardinal; DataEndian: TEndian = CPUEndian): TGUID; overload; static;
  end;

class function TGUIDHelper.Create(const Data; DataEndian: TEndian): TGUID;
begin
  Result := Create(Data, DataEndian=TEndian.Big);
end;

class function TGUIDHelper.Create(const B: TBytes; DataEndian: TEndian): TGUID;
begin
  Result := Create(B, 0, DataEndian);
end;

class function TGUIDHelper.Create(const B: TBytes; AStartIndex: Cardinal; DataEndian: TEndian): TGUID;
begin
  Result.D1 := AStartIndex;
end;

var
  G: TGUID;
  B: TBytes;
begin
  SetLength(B, 16);
  G := TGUID.Create(B);
  WriteLn(G.D1);
end.
