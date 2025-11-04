program ArrayOfRecordsFieldAccess;

type
  TMyRecord = record
    MyField: integer;
    AnotherField: integer;
  end;

var
  MyArray: array[0..2] of TMyRecord;
  x, y, z: integer;

begin
  MyArray[0].MyField := 10;
  MyArray[0].AnotherField := 20;
  MyArray[1].MyField := 30;
  MyArray[2].AnotherField := 40;
  
  x := MyArray[0].MyField;
  y := MyArray[1].MyField;
  z := MyArray[2].AnotherField;
  
  writeln('x = ', x);
  writeln('y = ', y);
  writeln('z = ', z);
end.
