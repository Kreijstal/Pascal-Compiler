program BinaryFileOps;

uses SysUtils;

const
  FileName = 'binary_file_ops.tmp';

type
  TLongArray = array[0..2] of LongInt;

var
  Values: TLongArray;
  ReadBack: TLongArray;

function BoolStr(Value: boolean): string;
begin
  if Value then
    BoolStr := 'TRUE'
  else
    BoolStr := 'FALSE';
end;

procedure Cleanup;
begin
  if FileExists(FileName) then
    if not DeleteFile(FileName) then
      WriteLn('Warning: Failed to delete file');
end;

var
  Bin: file of LongInt;
  Written, ReadCount: LongInt;
  SeekOK, PosOK, TruncOK: boolean;
  CloseAfterWrite, CloseAfterRead: boolean;
  IORes: Integer;
begin
  Cleanup;
  Values[0] := 10;
  Values[1] := 20;
  Values[2] := 30;

  Assign(Bin, FileName);
  {$I-}
  Rewrite(Bin);
  IORes := IOResult;
  {$I+}
  Written := 0;
  CloseAfterWrite := False;
  if IORes = 0 then
  begin
    {$I-}
    BlockWrite(Bin, Values, Length(Values), Written);
    IORes := IOResult;
    {$I+}
    if IORes <> 0 then
      Written := 0;

    {$I-}
    Close(Bin);
    IORes := IOResult;
    {$I+}
    CloseAfterWrite := IORes = 0;
  end;

  Assign(Bin, FileName);
  {$I-}
  Reset(Bin);
  IORes := IOResult;
  {$I+}
  ReadCount := 0;
  SeekOK := False;
  PosOK := False;
  TruncOK := False;
  CloseAfterRead := False;
  if IORes = 0 then
  begin
    {$I-}
    BlockRead(Bin, ReadBack, Length(ReadBack), ReadCount);
    IORes := IOResult;
    {$I+}
    if IORes <> 0 then
      ReadCount := 0;

    {$I-}
    Seek(Bin, 0);
    IORes := IOResult;
    {$I+}
    SeekOK := IORes = 0;

    {$I-}
    PosOK := FilePos(Bin) = 0;
    IORes := IOResult;
    {$I+}
    if IORes <> 0 then
      PosOK := False;

    {$I-}
    Seek(Bin, 2);
    IORes := IOResult;
    {$I+}
    if IORes = 0 then
    begin
      {$I-}
      Truncate(Bin);
      IORes := IOResult;
      {$I+}
      TruncOK := IORes = 0;
    end;

    {$I-}
    Close(Bin);
    IORes := IOResult;
    {$I+}
    CloseAfterRead := IORes = 0;
  end;

  WriteLn('WriteOK=', BoolStr(Written = Length(Values)));
  WriteLn('ReadOK=', BoolStr(ReadCount = Length(ReadBack)));
  WriteLn('DataOK=', BoolStr((ReadBack[0] = 10) and (ReadBack[1] = 20) and (ReadBack[2] = 30)));
  WriteLn('SeekOK=', BoolStr(SeekOK));
  WriteLn('PosOK=', BoolStr(PosOK));
  WriteLn('TruncOK=', BoolStr(TruncOK));
  WriteLn('CloseAfterWrite=', BoolStr(CloseAfterWrite));
  WriteLn('CloseAfterRead=', BoolStr(CloseAfterRead));

  Cleanup;
end.
