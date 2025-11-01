program minimal_record;

type
  TTestType = (ttOne, ttTwo);

  TTestRecord = record
    case ValueType: TTestType of
      ttOne: (IntValue: Integer);
      ttTwo: (StrValue: string);
  end;

var
  rec: TTestRecord;

function TestFunction: TTestRecord;
begin
  TestFunction.ValueType := ttOne;
  TestFunction.IntValue := 42;
end;

begin
  rec.ValueType := ttOne;
  rec.IntValue := 10;
  WriteLn('Record test');
  rec := TestFunction;
  WriteLn('Function result: ', rec.IntValue);
end.
