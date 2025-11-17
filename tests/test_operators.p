type
  TMyRecord = record
    MyInt: Integer;
    class operator+ (const C1, C2: TMyRecord): TMyRecord;
  end;

class operator TMyRecord.+ (const C1, C2: TMyRecord): TMyRecord;
begin
  Result.MyInt := C1.MyInt + C2.MyInt;
end;

var
  R1, R2, R3: TMyRecord;
begin
  R1.MyInt := 1;
  R2.MyInt := 10;
  R3 := R1 + R2;
  WriteLn('Result = ', R3.MyInt);
end.
