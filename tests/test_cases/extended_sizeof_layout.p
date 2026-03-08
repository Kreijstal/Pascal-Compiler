program ExtendedSizeOfLayout;

type
  TOnlyExtended = record
    E: Extended;
  end;

  TByteThenExtended = record
    B: Byte;
    E: Extended;
  end;

  TExtendedArray = array[0..1] of Extended;

begin
  WriteLn(SizeOf(Extended));
  WriteLn(SizeOf(TOnlyExtended));
  WriteLn(SizeOf(TByteThenExtended));
  WriteLn(SizeOf(TExtendedArray));
end.
