uses SysUtils;

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
  R, R2: TMyRecord;
begin
  R.MyInt := 1;
  R2.MyInt := 10;
  R := R + R2;
  WriteLn('1 + 10 = ', R.MyInt);
end.
