program tdd_typecast_deref_record_and;

{$mode objfpc}

type
  TCommonHeader = record
    h: QWord;
  end;
  pCommonHeader = ^TCommonHeader;

const
  CommonHeaderSize = SizeOf(TCommonHeader);
  SizeIndexMask = QWord($FF);

function IndexToSize(sizeIndex: QWord): QWord; inline;
begin
  Result := sizeIndex;
end;

var
  rec: TCommonHeader;
  p: Pointer;

begin
  rec.h := 123;
  p := Pointer(PtrUInt(@rec) + CommonHeaderSize);
  writeln(IndexToSize(pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask));
end.
