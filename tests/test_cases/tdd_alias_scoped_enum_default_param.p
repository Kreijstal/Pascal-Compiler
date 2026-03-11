{$mode objfpc}
{$modeswitch typehelpers}
program tdd_alias_scoped_enum_default_param;

type
  TEndianAlias = ObjPas.TEndian;
  TBytes = array of Byte;
  TGuidLike = record
    D1: LongWord;
  end;

const
  CPUEndian = TEndianAlias.Little;

type
  TGuidLikeHelper = type helper for TGuidLike
    class function Create(const B: TBytes; DataEndian: TEndianAlias = CPUEndian): TGuidLike; static;
  end;

class function TGuidLikeHelper.Create(const B: TBytes; DataEndian: TEndianAlias): TGuidLike;
begin
  if DataEndian = TEndianAlias.Big then
    Result.D1 := 1
  else
    Result.D1 := 0;
end;

var
  B: TBytes;
  G: TGuidLike;
begin
  SetLength(B, 1);
  G := TGuidLike.Create(B);
  Writeln(G.D1);
end.
