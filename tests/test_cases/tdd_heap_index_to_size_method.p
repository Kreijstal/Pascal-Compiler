program tdd_heap_index_to_size_method;

{$mode objfpc}

type
  TCommonHeader = record
    h: QWord;
  end;
  pCommonHeader = ^TCommonHeader;

  THeapInc = object
    class function IndexToSize(sizeIndex: QWord): QWord; static; inline;
    function FreeFixed(p: Pointer): QWord;
  end;

const
  CommonHeaderSize = SizeOf(TCommonHeader);
  SizeIndexMask = QWord($FF);

class function THeapInc.IndexToSize(sizeIndex: QWord): QWord;
begin
  Result := sizeIndex;
end;

function THeapInc.FreeFixed(p: Pointer): QWord;
begin
  Result := IndexToSize(pCommonHeader(p - CommonHeaderSize)^.h and SizeIndexMask);
end;

var
  rec: TCommonHeader;
  p: Pointer;
  h: THeapInc;

begin
  rec.h := 123;
  p := Pointer(PtrUInt(@rec) + CommonHeaderSize);
  writeln(h.FreeFixed(p));
end.
