unit TypInfo;

interface

function GetEnumName(TypeInfo: Pointer; Value: LongInt): ShortString;

implementation

type
  PEnumTypeInfo = ^TEnumTypeInfo;
  TEnumTypeInfo = record
    Count: LongInt;
    Reserved: LongInt;
    Names: array[0..0] of PAnsiChar;
  end;

function GetEnumName(TypeInfo: Pointer; Value: LongInt): ShortString;
var
  EnumInfo: PEnumTypeInfo;
  NamePtr: PAnsiChar;
begin
  GetEnumName := '';
  if TypeInfo = nil then
    exit;
  EnumInfo := PEnumTypeInfo(TypeInfo);
  if (Value < 0) or (Value >= EnumInfo^.Count) then
    exit;
  NamePtr := EnumInfo^.Names[Value];
  if NamePtr = nil then
    exit;
  kgpc_string_to_shortstring(GetEnumName, NamePtr, 256);
end;

end.
