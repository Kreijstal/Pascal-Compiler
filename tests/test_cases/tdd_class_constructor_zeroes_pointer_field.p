program tdd_class_constructor_zeroes_pointer_field;

type
  PLong = ^LongInt;

  TBox = class
    Value: LongInt;
    Padding1: LongInt;
    Padding2: LongInt;
    Padding3: LongInt;
    Padding4: LongInt;
    Padding5: LongInt;
    Padding6: LongInt;
    Padding7: LongInt;
    Ptr: PLong;
    constructor Create(AValue: LongInt);
    function ReadValue: LongInt;
  end;

constructor TBox.Create(AValue: LongInt);
begin
  Value := AValue;
end;

function TBox.ReadValue: LongInt;
begin
  if Ptr = nil then
    ReadValue := Value
  else
    ReadValue := Ptr^;
end;

var
  Box: TBox;

begin
  Box := TBox.Create(37);
  writeln(Box.ReadValue);
end.
