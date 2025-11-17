uses SysUtils;

type
  TMyRecord = record
    MyInt: Integer;
  end;

var
  R: TMyRecord;
begin
  R.MyInt := 111;
  WriteLn('Test = ', R.MyInt);
end.
