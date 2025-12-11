program FPCPackedRecord;
{
  Test packed record types.
  This is used in FPC RTL's charset.pp:
    tunicodecharmapping = packed record
      unicode : tunicodechar;
      flag : tunicodecharmappingflag;
      reserved : byte;
    end;
}

type
  TMyEnum = (flagA, flagB, flagC);
  
  { Regular record - should be aligned }
  TRegularRecord = record
    a: byte;
    b: integer;  { Will be aligned to 4-byte boundary }
    c: byte;
  end;
  
  { Packed record - no padding }
  TPackedRecord = packed record
    a: byte;
    b: integer;  { No alignment, immediately after a }
    c: byte;
  end;

var
  r1: TRegularRecord;
  r2: TPackedRecord;

begin
  r1.a := 1;
  r1.b := 100;
  r1.c := 2;
  WriteLn('Regular record: a=', r1.a, ' b=', r1.b, ' c=', r1.c);
  WriteLn('Regular record size: ', SizeOf(TRegularRecord));
  
  r2.a := 3;
  r2.b := 200;
  r2.c := 4;
  WriteLn('Packed record: a=', r2.a, ' b=', r2.b, ' c=', r2.c);
  WriteLn('Packed record size: ', SizeOf(TPackedRecord));
  
  WriteLn('Test completed');
end.
