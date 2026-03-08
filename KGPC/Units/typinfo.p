unit TypInfo;

interface

function GetEnumName(TypeInfo: Pointer; Value: LongInt): String;

implementation

type
  PEnumTypeInfo = ^TEnumTypeInfo;
  TEnumTypeInfo = record
    Count: LongInt;
    Reserved: LongInt;
    Names: array[0..0] of PAnsiChar;
  end;

  PByte = ^Byte;

function GetEnumName(TypeInfo: Pointer; Value: LongInt): String;
var
  EnumInfo: PEnumTypeInfo;
  NamePtr: PAnsiChar;
  DataPtr: PByte;
  NameLen: Byte;
  MinValue: LongInt;
  CurValue: LongInt;
  TempName: ShortString;
begin
  GetEnumName := '';
  if TypeInfo = nil then
    exit;

  { FPC-compatible enum TypeInfo:
    Kind byte = 3 (tkEnumeration), followed by shortstring name and TTypeData. }
  DataPtr := PByte(TypeInfo);
  if DataPtr^ = 3 then
  begin
    Inc(DataPtr);
    NameLen := DataPtr^;
    Inc(DataPtr, NameLen + 1);
    if (PPointer(DataPtr)^ = nil) and (PByte(DataPtr + SizeOf(Pointer))^ <= 7) then
      Inc(DataPtr, SizeOf(Pointer)); { Optional leading AttributeTable pointer }
    Inc(DataPtr); { OrdType }
    MinValue := PLongInt(DataPtr)^;
    Inc(DataPtr, SizeOf(LongInt));
    CurValue := PLongInt(DataPtr)^;
    Inc(DataPtr, SizeOf(LongInt));
    Inc(DataPtr, SizeOf(Pointer)); { BaseType }

    Dec(Value, MinValue);
    if Value < 0 then
      exit;
    while Value > 0 do
    begin
      Inc(DataPtr, DataPtr^ + 1);
      Dec(Value);
    end;
    FillChar(TempName, SizeOf(TempName), 0);
    Move(DataPtr^, TempName, DataPtr^ + 1);
    GetEnumName := TempName;
    exit;
  end;

  { Legacy KGPC enum TypeInfo blob. }
  EnumInfo := PEnumTypeInfo(TypeInfo);
  if (Value < 0) or (Value >= EnumInfo^.Count) then
    exit;
  NamePtr := EnumInfo^.Names[Value];
  if NamePtr = nil then
    exit;
  GetEnumName := NamePtr;
end;

end.
