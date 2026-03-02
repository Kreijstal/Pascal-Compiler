program tdd_nested_typecast_object;

{$mode objfpc}

type
  THeapInc = object
  type
    TCommonHeader = record
      h: QWord;
    end;
    pCommonHeader = ^TCommonHeader;
    TThreadState = object
      procedure Touch(p: Pointer);
    end;
  const
    CommonHeaderSize = SizeOf(TCommonHeader);
    SizeIndexMask = QWord($FF);
  end;

procedure THeapInc.TThreadState.Touch(p: Pointer);
begin
  THeapInc.pCommonHeader(p)^.h := THeapInc.pCommonHeader(p)^.h and THeapInc.SizeIndexMask;
end;

begin
  writeln(1);
end.
