program tdd_forward_pointer_record;

{$mode objfpc}

type
  THeapInc = object
  type
    pCommonHeader = ^CommonHeader;
    CommonHeader = record
      h: LongWord;
    end;
    TThreadState = object
      procedure Touch(p: Pointer);
    end;
  const
    SizeIndexMask = LongWord($FF);
  end;

procedure THeapInc.TThreadState.Touch(p: Pointer);
begin
  THeapInc.pCommonHeader(p)^.h := THeapInc.pCommonHeader(p)^.h and THeapInc.SizeIndexMask;
end;

begin
  writeln(1);
end.
